/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2016 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team.

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/
#![allow(
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::dpx_sfnt::{
    dfont_open, sfnt_close, sfnt_create_FontFile_stream, sfnt_open, sfnt_read_table_directory,
    sfnt_require_table, sfnt_set_table,
};
use crate::bridge::DisplayExt;
use crate::streq_ptr;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_agl::{
    agl_chop_suffix, agl_lookup_list, agl_name_convert_unicode, agl_name_is_unicode,
    agl_suffix_to_otltag,
};
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_truetype_file};
use super::dpx_mem::new;
use super::dpx_pdfencoding::{pdf_encoding_get_encoding, pdf_encoding_is_predefined};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_descriptor, pdf_font_get_encoding, pdf_font_get_index,
    pdf_font_get_resource, pdf_font_get_usedchars, pdf_font_get_verbose, pdf_font_is_in_use,
};
use super::dpx_tfm::{tfm_get_width, tfm_open};
use super::dpx_tt_aux::tt_get_fontdesc;
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_cmap::{tt_cmap_lookup, tt_cmap_read, tt_cmap_release};
use super::dpx_tt_glyf::{
    tt_add_glyph, tt_build_finish, tt_build_init, tt_build_tables, tt_find_glyph, tt_get_index,
};
use super::dpx_tt_gsub::{
    otl_gsub, otl_gsub_add_feat, otl_gsub_apply, otl_gsub_apply_alt, otl_gsub_apply_lig,
    otl_gsub_new, otl_gsub_release, otl_gsub_select,
};
use super::dpx_tt_post::{tt_lookup_post_table, tt_read_post_table, tt_release_post_table};
use super::dpx_tt_table::tt_get_ps_fontname;
use crate::dpx_pdfobj::{pdf_obj, pdf_ref_obj, pdf_release_obj, IntoObj, PushObj};
use crate::shims::sprintf;
use libc::{atoi, free, memcpy, memset, strchr, strcpy, strlen};

use super::dpx_sfnt::{put_big_endian, sfnt};

use super::dpx_tt_post::tt_post_table;

use super::dpx_tt_cmap::tt_cmap;

pub(crate) use sfnt_table_info::SfntTableInfo;

/// Tag consts for SfntTableInfo.
pub(crate) mod sfnt_table_info {
    pub(crate) type Tag = &'static [u8; 4];

    /*
     * The 'name' table should be preserved since it contains copyright
     * information, but it might cause problem when there are invalid
     * table entries (wrongly encoded text which is often the case in
     * CJK fonts). Acrobat does not use 'name' table. Unicode TrueType
     * fonts may have 10K bytes 'name' table...
     *
     * We preserve the 'OS/2' table too, since it contains the license
     * information. PDF applications should use this table to decide
     * whether the font is embedded only for the purpose of preview &
     * printing. Otherwise, we must encrypt the document. Acrobat does
     * not use 'OS/2' table, though...
     */
    #[derive(Copy, Clone)]
    pub(crate) struct SfntTableInfo {
        name: Tag,
        must_exist: bool,
    }

    impl SfntTableInfo {
        pub(crate) const fn new(name: Tag, must_exist: bool) -> Self {
            SfntTableInfo { name, must_exist }
        }

        pub(crate) const fn name(&self) -> Tag {
            self.name
        }

        /// # Safety
        /// This function assumes the name is valid utf8.
        pub(crate) unsafe fn name_str(&self) -> &str {
            &std::str::from_utf8_unchecked(self.name)
        }

        pub(crate) const fn must_exist(&self) -> bool {
            self.must_exist
        }
    }

    pub(crate) const OS_2: Tag = b"OS/2";
    pub(crate) const HEAD: Tag = b"head";
    pub(crate) const HHEA: Tag = b"hhea";
    pub(crate) const LOCA: Tag = b"loca";
    pub(crate) const MAXP: Tag = b"maxp";
    pub(crate) const NAME: Tag = b"name";
    pub(crate) const GLYF: Tag = b"glyf";
    pub(crate) const HMTX: Tag = b"hmtx";
    pub(crate) const FPGM: Tag = b"fpgm";
    pub(crate) const CVT: Tag = b"cvt ";
    pub(crate) const PREP: Tag = b"prep";
    pub(crate) const CMAP: Tag = b"cmap";
}

/* Acoid conflict with CHAR ... from <winnt.h>.  */
/* Data Types as described in Apple's TTRefMan */
/* Order of lookup should be
 *  post, unicode+otl
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct glyph_mapper {
    pub(crate) codetogid: *mut tt_cmap,
    pub(crate) gsub: *mut otl_gsub,
    pub(crate) sfont: *mut sfnt,
    pub(crate) nametogid: *mut tt_post_table,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

/* TrueType */
/* Modifying this has no effect :P */

pub(crate) unsafe fn pdf_font_open_truetype(font: &mut pdf_font) -> i32 {
    let mut embedding: i32 = 1i32;
    let ident = font.ident.clone(); /* Must be embedded. */
    let index = pdf_font_get_index(font);
    assert!(!ident.is_empty());
    let sfont = if let Some(handle) = dpx_open_truetype_file(&ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&ident) {
        dfont_open(handle, index)
    } else {
        return -1i32;
    };
    if sfont.is_null() {
        warn!("Could not open TrueType font: {}", ident,);
        return -1i32;
    }
    let error = if (*sfont).type_0 == 1i32 << 4i32 {
        let offset = ttc_read_offset(sfont, index);
        if offset == 0_u32 {
            panic!("Invalid TTC index in {}.", ident);
        }
        sfnt_read_table_directory(sfont, offset)
    } else {
        sfnt_read_table_directory(sfont, (*sfont).offset)
    };
    if error != 0 {
        sfnt_close(sfont);
        return -1i32;
        /* Silently */
    }
    /* Reading fontdict before checking fonttype conflicts with PKFONT
     * because pdf_font_get_resource() always makes a dictionary.
     */
    let encoding_id = pdf_font_get_encoding(font);
    pdf_font_get_resource(font);

    let fontname = tt_get_ps_fontname(sfont).unwrap_or_else(|| ident.clone());
    if fontname.is_empty() {
        panic!("Can\'t find valid fontname for \"{}\".", ident);
    }
    font.fontname = fontname.clone();
    let tmp = tt_get_fontdesc(sfont, &mut embedding, -1i32, 1i32, &fontname)
        .expect("Could not optain necessary font info.");

    {
        let descriptor = pdf_font_get_descriptor(font);
        assert!(!descriptor.is_null());
        (*descriptor).as_dict_mut().merge(&tmp);
    }
    if embedding == 0 {
        if encoding_id >= 0i32 && pdf_encoding_is_predefined(encoding_id) == 0 {
            sfnt_close(sfont);
            panic!("Custom encoding not allowed for non-embedded TrueType font.");
        } else {
            /* There are basically no guarantee for font substitution
             * can work with "symblic" fonts. At least all glyphs
             * contained in the font must be identified; glyphs covers
             * by this instance of font should contain glyphs only from
             * Adobe Standard Latin Set. We allow non-embedded font
             * only to predefined encodings for this reason. Note that
             * "builtin" encoding means "MacRoman" here.
             */
            panic!(
                "Font file=\"{}\" can\'t be embedded due to license restrictions.",
                ident
            );
            /* ENABLE_NOEMBED */
        }
    }
    sfnt_close(sfont);
    let fontdict = pdf_font_get_resource(font);
    fontdict.as_dict_mut().set("Type", "Font");
    fontdict.as_dict_mut().set("Subtype", "TrueType");
    0i32
}
const required_table: [SfntTableInfo; 12] = {
    use sfnt_table_info::*;
    [
        SfntTableInfo::new(OS_2, false),
        SfntTableInfo::new(HEAD, false),
        SfntTableInfo::new(HHEA, true),
        SfntTableInfo::new(LOCA, true),
        SfntTableInfo::new(MAXP, true),
        SfntTableInfo::new(NAME, true),
        SfntTableInfo::new(GLYF, true),
        SfntTableInfo::new(HMTX, true),
        SfntTableInfo::new(FPGM, false),
        SfntTableInfo::new(CVT, false),
        SfntTableInfo::new(PREP, false),
        SfntTableInfo::new(CMAP, true),
    ]
};

unsafe fn do_widths(font: &mut pdf_font, widths: *mut f64) {
    let usedchars = pdf_font_get_usedchars(font);
    let mut tmparray = vec![];
    let mut firstchar = 255i32;
    let mut lastchar = 0i32;
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            if code < firstchar {
                firstchar = code
            }
            if code > lastchar {
                lastchar = code
            }
        }
    }
    if firstchar > lastchar {
        warn!("No glyphs actually used???");
        return;
    }
    let tfm_id = tfm_open(&*font.map_name, 0i32);
    for code in firstchar..=lastchar {
        if *usedchars.offset(code as isize) != 0 {
            let width = if tfm_id < 0i32 {
                /* tfm is not found */
                *widths.offset(code as isize)
            } else {
                1000. * tfm_get_width(tfm_id, code)
            };
            tmparray.push_obj((width / 0.1 + 0.5).floor() * 0.1);
        } else {
            tmparray.push_obj(0f64);
        }
    }
    let fontdict = pdf_font_get_resource(font);
    let empty = tmparray.is_empty();
    let tmparray = tmparray.into_obj();
    if !empty {
        fontdict.as_dict_mut().set("Widths", pdf_ref_obj(tmparray));
    }
    pdf_release_obj(tmparray);
    fontdict.as_dict_mut().set("FirstChar", firstchar as f64);
    fontdict.as_dict_mut().set("LastChar", lastchar as f64);
}
static mut verbose: i32 = 0i32;
/*
 * There are several issues in TrueType font support in PDF.
 * How PDF viewers select TrueType cmap table is not so clear.
 * Most reliable way seem to reencode font and sort glyphs as
 * charcode == gid and to use Mac-Roman format 0 subtable.
 * It does not work with encodings that uses full 256 range since
 * GID = 0 is reserved for .notdef, so GID = 256 is not accessible.
 */
unsafe fn do_builtin_encoding(font: &mut pdf_font, usedchars: *const i8, sfont: *mut sfnt) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    let ttcm = tt_cmap_read(sfont, 1_u16, 0_u16);
    if ttcm.is_null() {
        warn!("Could not read Mac-Roman TrueType cmap table...");
        return -1i32;
    }
    let cmap_table =
        new((274_u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    memset(cmap_table as *mut libc::c_void, 0i32, 274);
    put_big_endian(cmap_table as *mut libc::c_void, 0i32, 2i32);
    /* Version  */
    put_big_endian(cmap_table.offset(2) as *mut libc::c_void, 1i32, 2i32);
    /* Number of subtables */
    put_big_endian(cmap_table.offset(4) as *mut libc::c_void, 1u32 as i32, 2i32);
    /* Platform ID */
    put_big_endian(cmap_table.offset(6) as *mut libc::c_void, 0u32 as i32, 2i32);
    /* Encoding ID */
    put_big_endian(cmap_table.offset(8) as *mut libc::c_void, 12i32, 4i32);
    /* Offset   */
    put_big_endian(cmap_table.offset(12) as *mut libc::c_void, 0i32, 2i32);
    /* Format   */
    put_big_endian(cmap_table.offset(14) as *mut libc::c_void, 262i32, 2i32);
    /* Length   */
    put_big_endian(cmap_table.offset(16) as *mut libc::c_void, 0i32, 2i32);
    /* Language */
    let glyphs = tt_build_init(); /* .notdef */
    if verbose > 2i32 {
        info!("[glyphs:/.notdef");
    }
    let mut count = 1;
    for code in 0..256 {
        if !(*usedchars.offset(code as isize) == 0) {
            if verbose > 2i32 {
                info!("/.c0x{:02x}", code);
            }
            let mut idx;
            let gid = tt_cmap_lookup(ttcm, code as u32);
            if gid as i32 == 0i32 {
                warn!(
                    "Glyph for character code=0x{:02x} missing in font font-file=\"{}\".",
                    code,
                    (&*font).ident,
                );
                idx = 0_u16
            } else {
                idx = tt_find_glyph(glyphs, gid);
                if idx as i32 == 0i32 {
                    idx = tt_add_glyph(glyphs, gid, count as u16)
                }
                /* count returned. */
            } /* bug here */
            *cmap_table.offset((18i32 + code) as isize) = (idx as i32 & 0xffi32) as i8;
            count += 1
        }
    }
    tt_cmap_release(ttcm);
    if verbose > 2i32 {
        info!("]");
    }
    if tt_build_tables(sfont, glyphs) < 0i32 {
        warn!("Packing TrueType font into SFNT failed!");
        tt_build_finish(glyphs);
        free(cmap_table as *mut libc::c_void);
        return -1i32;
    }
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            let idx = tt_get_index(glyphs, *cmap_table.offset((18i32 + code) as isize) as u16);
            widths[code as usize] = (1000.0f64
                * (*(*glyphs).gd.offset(idx as isize)).advw as i32 as f64
                / (*glyphs).emsize as i32 as f64
                / 1i32 as f64
                + 0.5f64)
                .floor()
                * 1i32 as f64
        } else {
            widths[code as usize] = 0.0f64
        }
    }
    do_widths(font, widths.as_mut_ptr());
    if verbose > 1i32 {
        info!("[{} glyphs]", (*glyphs).num_glyphs as i32);
    }
    tt_build_finish(glyphs);
    sfnt_set_table(
        sfont,
        sfnt_table_info::CMAP,
        cmap_table as *mut libc::c_void,
        274_u32,
    );
    0i32
}
/* WARNING: This modifies glyphname itself */
unsafe fn agl_decompose_glyphname(
    glyphname: *mut i8,
    nptrs: *mut *mut i8,
    size: i32,
    suffix: *mut *mut i8,
) -> i32 {
    let mut p: *mut i8 = glyphname; /* _FIXME_ */
    let mut q = strchr(p, '.' as i32); /* chop every thing after *first* dot */
    if q.is_null() {
        *suffix = ptr::null_mut()
    } else {
        *q = '\u{0}' as i32 as i8;
        q = q.offset(1);
        *suffix = q
    }
    let ref mut fresh0 = *nptrs.offset(0);
    *fresh0 = p;
    let mut n = 1;
    while !p.is_null() && *p as i32 != 0 {
        p = strchr(p, '_' as i32);
        if p.is_null() || *p.offset(1) as i32 == '\u{0}' as i32 {
            break;
        }
        if n >= size {
            panic!("Uh ah...");
        }
        *p = '\u{0}' as i32 as i8;
        p = p.offset(1);
        let ref mut fresh1 = *nptrs.offset(n as isize);
        *fresh1 = p;
        n += 1
    }
    n
}
unsafe fn select_gsub(feat: &[u8], gm: *mut glyph_mapper) -> i32 {
    if feat.is_empty() || gm.is_null() || (*gm).gsub.is_null() {
        return -1i32;
    }
    /* First treat as is */
    let idx = otl_gsub_select((*gm).gsub, b"*", b"*", feat);
    if idx >= 0i32 {
        return 0i32;
    }
    if verbose > 1i32 {
        info!(
            "\ntrutype>> Try loading OTL GSUB for \"*.*.{}\"...",
            feat.display()
        );
    }
    let error = otl_gsub_add_feat((*gm).gsub, b"*", b"*", feat, (*gm).sfont);
    if error == 0 {
        let idx = otl_gsub_select((*gm).gsub, b"*", b"*", feat);
        return if idx >= 0i32 { 0i32 } else { -1i32 };
    }
    -1i32
}
/* Apply GSUB. This is a bit tricky... */
unsafe fn selectglyph(
    mut in_0: u16,
    suffix: *const i8,
    gm: *mut glyph_mapper,
    out: *mut u16,
) -> i32 {
    let mut t: [i8; 5] = [0; 5];
    let mut error;
    assert!(!suffix.is_null() && !gm.is_null() && !out.is_null());
    assert!(!suffix.is_null() && *suffix as i32 != 0i32);
    let s = new((strlen(suffix).wrapping_add(1) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    strcpy(s, suffix);
    /* First try converting suffix to feature tag.
     * agl.c currently only knows less ambiguos cases;
     * e.g., 'sc', 'superior', etc.
     */
    if let Some(r) = agl_suffix_to_otltag(CStr::from_ptr(s).to_bytes())
    /* 'suffix' may represent feature tag. */
    {
        /* We found feature tag for 'suffix'. */
        error = select_gsub(r, gm); /* no fallback for this */
        if error == 0 {
            error = otl_gsub_apply((*gm).gsub, &mut in_0)
        }
    } else {
        /* Try loading GSUB only when length of 'suffix' is less
         * than or equal to 4. tt_gsub give a warning otherwise.
         */
        if strlen(s) > 4 {
            error = -1i32
        } else if strlen(s) == 4 {
            error = select_gsub(CStr::from_ptr(s).to_bytes(), gm)
        } else {
            /* Uh */
            /* less than 4. pad ' '. */
            memset(t.as_mut_ptr() as *mut libc::c_void, ' ' as i32, 4);
            t[4] = '\u{0}' as i32 as i8;
            memcpy(
                t.as_mut_ptr() as *mut libc::c_void,
                s as *const libc::c_void,
                strlen(s),
            );
            error = select_gsub(CStr::from_ptr(t.as_mut_ptr()).to_bytes(), gm)
        }
        if error == 0 {
            /* 'suffix' represents feature tag. */
            error = otl_gsub_apply((*gm).gsub, &mut in_0)
        } else {
            /* other case: alt1, nalt10... (alternates) */
            let mut q = s.offset(strlen(s) as isize).offset(-1);
            while q > s && *q as i32 >= '0' as i32 && *q as i32 <= '9' as i32 {
                q = q.offset(-1)
            }
            if q == s {
                error = -1i32
            } else {
                /* starting at 1 */
                let n = atoi(q.offset(1)) - 1i32;
                *q.offset(1) = '\u{0}' as i32 as i8;
                if strlen(s) > 4 {
                    error = -1i32
                } else {
                    /* This may be alternate substitution. */
                    memset(t.as_mut_ptr() as *mut libc::c_void, ' ' as i32, 4);
                    t[4] = '\u{0}' as i32 as i8;
                    memcpy(
                        t.as_mut_ptr() as *mut libc::c_void,
                        s as *const libc::c_void,
                        strlen(s),
                    );
                    error = select_gsub(CStr::from_ptr(s).to_bytes(), gm);
                    if error == 0 {
                        error = otl_gsub_apply_alt((*gm).gsub, n as u16, &mut in_0 as *mut u16)
                    }
                }
            }
        }
    }
    free(s as *mut libc::c_void);
    *out = in_0;
    error
}
/* Compose glyphs via ligature substitution. */
unsafe fn composeglyph(
    glyphs: *mut u16,
    n_glyphs: i32,
    feat: *const i8,
    gm: *mut glyph_mapper,
    gid: *mut u16,
) -> i32 {
    let mut t: [i8; 5] = [
        ' ' as i32 as i8,
        ' ' as i32 as i8,
        ' ' as i32 as i8,
        ' ' as i32 as i8,
        0_i8,
    ];
    assert!(!glyphs.is_null() && n_glyphs > 0i32 && !gm.is_null() && !gid.is_null());
    let mut error = if feat.is_null() || *feat.offset(0) as i32 == '\u{0}' as i32 {
        /* meaning "Unknown" */
        select_gsub(b"(?lig|lig?|?cmp|cmp?|frac|afrc)", gm)
    } else if strlen(feat) > 4 {
        -1
    } else {
        memcpy(
            t.as_mut_ptr() as *mut libc::c_void,
            feat as *const libc::c_void,
            strlen(feat),
        );
        select_gsub(CStr::from_ptr(t.as_mut_ptr()).to_bytes(), gm)
    };
    if error == 0 {
        error = otl_gsub_apply_lig((*gm).gsub, glyphs, n_glyphs as u16, gid)
    }
    error
}
/* This may be called by findparanoiac(). */
unsafe fn composeuchar(
    unicodes: *mut i32,
    n_unicodes: i32,
    feat: *const i8,
    gm: *mut glyph_mapper,
    gid: *mut u16,
) -> i32 {
    let mut error: i32 = 0i32;
    if (*gm).codetogid.is_null() {
        return -1i32;
    }
    let gids =
        new((n_unicodes as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    let mut i = 0;
    while error == 0 && i < n_unicodes {
        *gids.offset(i as isize) =
            tt_cmap_lookup((*gm).codetogid, *unicodes.offset(i as isize) as u32);
        error = if *gids.offset(i as isize) as i32 == 0i32 {
            -1i32
        } else {
            0i32
        };
        i += 1
    }
    if error == 0 {
        error = composeglyph(gids, n_unicodes, feat, gm, gid)
    }
    free(gids as *mut libc::c_void);
    error
}
/* Search 'post' table. */
unsafe fn findposttable(glyph_name: *const i8, gid: *mut u16, gm: *mut glyph_mapper) -> i32 {
    if (*gm).nametogid.is_null() {
        return -1i32;
    }
    *gid = tt_lookup_post_table((*gm).nametogid, glyph_name);
    if *gid as i32 == 0i32 {
        -1i32
    } else {
        0i32
    }
}
/* This is wrong. We must care about '.'. */
/* Glyph names are concatinated with '_'. */
unsafe fn findcomposite(glyphname: *const i8, gid: *mut u16, gm: *mut glyph_mapper) -> i32 {
    let mut suffix: *mut i8 = ptr::null_mut();
    let mut gids: [u16; 32] = [0; 32];
    let mut nptrs: [*mut i8; 32] = [ptr::null_mut(); 32];
    let error = findposttable(glyphname, gid, gm);
    if error == 0 {
        return 0i32;
    }
    let gname =
        new((strlen(glyphname).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8; /* first try composing glyph */
    strcpy(gname, glyphname);
    memset(
        gids.as_mut_ptr() as *mut libc::c_void,
        0i32,
        (32usize).wrapping_mul(::std::mem::size_of::<u16>()),
    );
    let n_comp = agl_decompose_glyphname(gname, nptrs.as_mut_ptr(), 32i32, &mut suffix);
    let mut error = 0i32;
    let mut i = 0;
    while error == 0 && i < n_comp as usize {
        error = resolve_glyph(nptrs[i], &mut *gids.as_mut_ptr().offset(i as isize), gm);
        if error != 0 {
            warn!(
                "Could not resolve glyph \"{}\" ({}th component of glyph \"{}\").",
                CStr::from_ptr(nptrs[i]).display(),
                i,
                CStr::from_ptr(glyphname).display(),
            );
        }
        i += 1
    }
    if error == 0 {
        if !suffix.is_null()
            && (streq_ptr(suffix, b"liga\x00" as *const u8 as *const i8) as i32 != 0
                || streq_ptr(suffix, b"dlig\x00" as *const u8 as *const i8) as i32 != 0
                || streq_ptr(suffix, b"hlig\x00" as *const u8 as *const i8) as i32 != 0
                || streq_ptr(suffix, b"frac\x00" as *const u8 as *const i8) as i32 != 0
                || streq_ptr(suffix, b"ccmp\x00" as *const u8 as *const i8) as i32 != 0
                || streq_ptr(suffix, b"afrc\x00" as *const u8 as *const i8) as i32 != 0)
        {
            error = composeglyph(gids.as_mut_ptr(), n_comp, suffix, gm, gid)
        } else {
            error = composeglyph(gids.as_mut_ptr(), n_comp, ptr::null(), gm, gid);
            if error == 0 && !suffix.is_null() {
                /* a_b_c.vert */
                error = selectglyph(*gid, suffix, gm, gid)
            }
        }
    }
    free(gname as *mut libc::c_void);
    error
}
/* glyphname should not have suffix here */
unsafe fn findparanoiac(glyphname: *const i8, gid: *mut u16, gm: *mut glyph_mapper) -> i32 {
    let mut idx: u16 = 0_u16;
    let mut error;
    let mut agln = agl_lookup_list(glyphname);
    while !agln.is_null() && idx as i32 == 0i32 {
        if !(*agln).suffix.is_null() {
            error = findparanoiac((*agln).name, &mut idx, gm);
            if error != 0 {
                return error;
            }
            error = selectglyph(idx, (*agln).suffix, gm, &mut idx);
            if error != 0 {
                warn!(
                    "Variant \"{}\" for glyph \"{}\" might not be found.",
                    CStr::from_ptr((*agln).suffix).display(),
                    CStr::from_ptr((*agln).name).display(),
                );
                warn!("Using glyph name without suffix instead...");
                //error = 0i32
                /* ignore */
            }
        } else if (*agln).n_components == 1i32 {
            idx = tt_cmap_lookup((*gm).codetogid, (*agln).unicodes[0] as u32)
        } else if (*agln).n_components > 1i32 {
            if verbose >= 0i32 {
                /* give warning */
                warn!(
                    "Glyph \"{}\" looks like a composite glyph...",
                    CStr::from_ptr((*agln).name).display(),
                );
            }
            error = composeuchar(
                (*agln).unicodes.as_mut_ptr(),
                (*agln).n_components,
                ptr::null(),
                gm,
                &mut idx,
            );
            if verbose >= 0i32 {
                if error != 0 {
                    warn!("Not found...");
                } else {
                    let mut _i: i32 = 0;
                    let mut _n: i32 = 0i32;
                    let mut _p: *mut i8 = ptr::null_mut();
                    let mut _buf: [i8; 256] = [0; 256];
                    warn!(
                        ">> Composite glyph glyph-name=\"{}\" found at glyph-id=\"{}\".",
                        CStr::from_ptr((*agln).name).display(),
                        idx,
                    );
                    _p = _buf.as_mut_ptr();
                    _i = 0i32;
                    while _i < (*agln).n_components && _n < 245i32 {
                        let fresh2 = _n;
                        _n = _n + 1;
                        *_p.offset(fresh2 as isize) =
                            (if _i == 0i32 { '<' as i32 } else { ' ' as i32 }) as i8;
                        if (*agln).unicodes[_i as usize] >= 0x10000i32 {
                            _n += sprintf(
                                _p.offset(_n as isize),
                                b"U+%06X\x00" as *const u8 as *const i8,
                                (*agln).unicodes[_i as usize],
                            )
                        } else {
                            _n += sprintf(
                                _p.offset(_n as isize),
                                b"U+%04X\x00" as *const u8 as *const i8,
                                (*agln).unicodes[_i as usize],
                            )
                        }
                        let fresh3 = _n;
                        _n = _n + 1;
                        *_p.offset(fresh3 as isize) = (if _i == (*agln).n_components - 1i32 {
                            '>' as i32
                        } else {
                            ',' as i32
                        }) as i8;
                        _i += 1;
                    }
                    let fresh4 = _n;
                    _n = _n + 1;
                    *_p.offset(fresh4 as isize) = '\u{0}' as i32 as i8;
                    warn!(">> Input Unicode seq.=\"{}\" ==> glyph-id=\"{}\" in font-file=\"_please_try_-v_\".",
                                CStr::from_ptr(_buf.as_mut_ptr()).display(), idx);
                }
            }
        } else {
            unreachable!();
        }
        agln = (*agln).alternate
    }
    *gid = idx;
    if idx as i32 == 0i32 {
        -1i32
    } else {
        0i32
    }
}
unsafe fn resolve_glyph(glyphname: *const i8, gid: *mut u16, gm: *mut glyph_mapper) -> i32 {
    assert!(!glyphname.is_null());
    /* Boooo */
    /*
     * First we try glyph name to GID mapping using post table if post table
     * is available. If post table is not available or glyph is not listed
     * in the post table, then we try Unicode if Windows-Unicode TrueType
     * cmap is available.
     */
    let error = findposttable(glyphname, gid, gm);
    if error == 0 {
        return 0i32;
    }
    if (*gm).codetogid.is_null() {
        return -1i32;
    }
    let (name, suffix) = agl_chop_suffix(CStr::from_ptr(glyphname).to_bytes());
    if let Some(name) = name {
        let mut error = if agl_name_is_unicode(name.to_bytes()) {
            let ucv = agl_name_convert_unicode(name.as_ptr());
            *gid = tt_cmap_lookup((*gm).codetogid, ucv as u32);
            if *gid as i32 == 0i32 {
                -1
            } else {
                0
            }
        } else {
            findparanoiac(name.as_ptr(), gid, gm)
        };
        if error == 0 {
            if let Some(suffix) = suffix {
                error = selectglyph(*gid, suffix.as_ptr(), gm, gid);
                if error != 0 {
                    warn!(
                        "Variant \"{}\" for glyph \"{}\" might not be found.",
                        suffix.display(),
                        name.display(),
                    );
                    warn!("Using glyph name without suffix instead...");
                    error = 0i32
                    /* ignore */
                }
            }
        }
        error
    } else {
        /* .notdef, .foo */
        -1
    }
}
/* Things are complicated. We still need to use PostScript
 * glyph names. But OpenType fonts may not have PS name to
 * glyph mapping. We use Unicode plus OTL GSUB for finding
 * glyphs in this case.
 */
unsafe fn setup_glyph_mapper(mut gm: *mut glyph_mapper, sfont: *mut sfnt) -> i32 {
    (*gm).sfont = sfont;
    (*gm).nametogid = tt_read_post_table(sfont);
    (*gm).codetogid = tt_cmap_read(sfont, 3_u16, 10_u16);
    if (*gm).codetogid.is_null() {
        (*gm).codetogid = tt_cmap_read(sfont, 3_u16, 1_u16)
    }
    if (*gm).nametogid.is_null() && (*gm).codetogid.is_null() {
        return -1i32;
    }
    (*gm).gsub = otl_gsub_new();
    0i32
}
unsafe fn clean_glyph_mapper(mut gm: *mut glyph_mapper) {
    if !(*gm).gsub.is_null() {
        otl_gsub_release((*gm).gsub);
    }
    if !(*gm).codetogid.is_null() {
        tt_cmap_release((*gm).codetogid);
    }
    if !(*gm).nametogid.is_null() {
        tt_release_post_table((*gm).nametogid);
    }
    (*gm).gsub = ptr::null_mut();
    (*gm).codetogid = ptr::null_mut();
    (*gm).nametogid = ptr::null_mut();
    (*gm).sfont = ptr::null_mut();
}
unsafe fn do_custom_encoding(
    font: &mut pdf_font,
    encoding: *mut *mut i8,
    usedchars: *const i8,
    sfont: *mut sfnt,
) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    let mut gm: glyph_mapper = glyph_mapper {
        codetogid: ptr::null_mut(),
        gsub: ptr::null_mut(),
        sfont: ptr::null_mut(),
        nametogid: ptr::null_mut(),
    };
    assert!(!encoding.is_null() && !usedchars.is_null() && !sfont.is_null());
    let error = setup_glyph_mapper(&mut gm, sfont);
    if error != 0 {
        warn!(
            "No post table nor Unicode cmap found in font: {}",
            (&*font).ident,
        );
        warn!(">> I can\'t find glyphs without this!");
        return -1i32;
    }
    let cmap_table =
        new((274_u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    memset(cmap_table as *mut libc::c_void, 0i32, 274);
    put_big_endian(cmap_table as *mut libc::c_void, 0i32, 2i32);
    /* Version  */
    put_big_endian(cmap_table.offset(2) as *mut libc::c_void, 1i32, 2i32);
    /* Number of subtables */
    put_big_endian(cmap_table.offset(4) as *mut libc::c_void, 1u32 as i32, 2i32);
    /* Platform ID */
    put_big_endian(cmap_table.offset(6) as *mut libc::c_void, 0u32 as i32, 2i32);
    /* Encoding ID */
    put_big_endian(cmap_table.offset(8) as *mut libc::c_void, 12i32, 4i32);
    /* Offset   */
    put_big_endian(cmap_table.offset(12) as *mut libc::c_void, 0i32, 2i32);
    /* Format   */
    put_big_endian(cmap_table.offset(14) as *mut libc::c_void, 262i32, 2i32);
    /* Length   */
    put_big_endian(cmap_table.offset(16) as *mut libc::c_void, 0i32, 2i32);
    /* Language */
    let glyphs = tt_build_init(); /* +1 for .notdef */
    let mut count = 1;
    for code in 0..256 {
        if !(*usedchars.offset(code as isize) == 0) {
            let mut gid: u16 = 0;
            let mut idx;
            if (*encoding.offset(code as isize)).is_null()
                || streq_ptr(
                    *encoding.offset(code as isize),
                    b".notdef\x00" as *const u8 as *const i8,
                ) as i32
                    != 0
            {
                warn!("Character code=\"0x{:02X}\" mapped to \".notdef\" glyph used in font font-file=\"{}\"", code,
                            (&*font).ident);
                warn!(">> Maybe incorrect encoding specified?");
                idx = 0_u16
            } else {
                let error = if !strchr(*encoding.offset(code as isize), '_' as i32).is_null() {
                    findcomposite(*encoding.offset(code as isize), &mut gid, &mut gm)
                } else {
                    resolve_glyph(*encoding.offset(code as isize), &mut gid, &mut gm)
                };
                /*
                 * Older versions of gs had problem with glyphs (other than .notdef)
                 * mapped to gid = 0.
                 */
                if error != 0 {
                    warn!(
                        "Glyph \"{}\" not available in font \"{}\".",
                        CStr::from_ptr(*encoding.offset(code as isize)).display(),
                        (&*font).ident,
                    ); /* count returned. */
                } else if verbose > 1i32 {
                    info!(
                        "truetype>> Glyph glyph-name=\"{}\" found at glyph-id=\"{}\".\n",
                        CStr::from_ptr(*encoding.offset(code as isize)).display(),
                        gid,
                    );
                }
                idx = tt_find_glyph(glyphs, gid);
                if idx as i32 == 0i32 {
                    idx = tt_add_glyph(glyphs, gid, count as u16);
                    count += 1
                }
            }
            *cmap_table.offset((18i32 + code) as isize) = (idx as i32 & 0xffi32) as i8
        }
        /* bug here */
    } /* _FIXME_: wrong message */
    clean_glyph_mapper(&mut gm);
    if tt_build_tables(sfont, glyphs) < 0i32 {
        warn!("Packing TrueType font into SFNT file faild...");
        tt_build_finish(glyphs);
        free(cmap_table as *mut libc::c_void);
        return -1i32;
    }
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            let idx = tt_get_index(glyphs, *cmap_table.offset((18i32 + code) as isize) as u16);
            widths[code as usize] = (1000.0f64
                * (*(*glyphs).gd.offset(idx as isize)).advw as i32 as f64
                / (*glyphs).emsize as i32 as f64
                / 1i32 as f64
                + 0.5f64)
                .floor()
                * 1i32 as f64
        } else {
            widths[code as usize] = 0.0f64
        }
    }
    do_widths(font, widths.as_mut_ptr());
    if verbose > 1i32 {
        info!("[{} glyphs]", (*glyphs).num_glyphs as i32);
    }
    tt_build_finish(glyphs);
    sfnt_set_table(
        sfont,
        sfnt_table_info::CMAP,
        cmap_table as *mut libc::c_void,
        274_u32,
    );
    0i32
}

pub(crate) unsafe fn pdf_font_load_truetype(font: &mut pdf_font) -> i32 {
    let descriptor: *mut pdf_obj = pdf_font_get_descriptor(font);
    let encoding_id: i32 = pdf_font_get_encoding(font);
    /* ENABLE_NOEMBED */
    let index: i32 = pdf_font_get_index(font); /* Should find *truetype* here */
    if !pdf_font_is_in_use(font) {
        return 0i32;
    }
    verbose = pdf_font_get_verbose();
    let sfont = if let Some(handle) = dpx_open_truetype_file(&font.ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&font.ident) {
        dfont_open(handle, index)
    } else {
        panic!("Unable to open TrueType/dfont font file: {}", font.ident);
    };
    if sfont.is_null() {
        panic!("Unable to open TrueType/dfont file: {}", font.ident);
    } else {
        if (*sfont).type_0 != 1i32 << 0i32
            && (*sfont).type_0 != 1i32 << 4i32
            && (*sfont).type_0 != 1i32 << 8i32
        {
            sfnt_close(sfont);
            panic!("Font \"{}\" not a TrueType/dfont font?", font.ident);
        }
    }
    let error = if (*sfont).type_0 == 1i32 << 4i32 {
        let offset = ttc_read_offset(sfont, index);
        if offset == 0_u32 {
            panic!("Invalid TTC index in {}.", font.ident);
        }
        sfnt_read_table_directory(sfont, offset)
    } else {
        sfnt_read_table_directory(sfont, (*sfont).offset)
    };
    if error != 0 {
        sfnt_close(sfont);
        panic!(
            "Reading SFND table dir failed for font-file=\"{}\"... Not a TrueType font?",
            font.ident
        );
    }
    /*
     * Create new TrueType cmap table with MacRoman encoding.
     */
    let usedchars: *mut i8 = pdf_font_get_usedchars(font);
    let error = if encoding_id < 0i32 {
        do_builtin_encoding(font, usedchars, sfont)
    } else {
        let enc_vec = pdf_encoding_get_encoding(encoding_id);
        do_custom_encoding(font, enc_vec, usedchars, sfont)
    };
    if error != 0 {
        sfnt_close(sfont);
        panic!(
            "Error occured while creating font subfont for \"{}\"",
            font.ident
        );
    }
    /* ENABLE_NOEMBED */
    /*
     * TODO: post table?
     */

    for table in &required_table {
        if sfnt_require_table(sfont.as_mut().unwrap(), table).is_err() {
            sfnt_close(sfont);
            panic!(
                "Required TrueType table \"{}\" does not exist in font: {}",
                table.name_str(),
                font.ident
            );
        }
    }
    /*
     * FontFile2
     */
    let fontfile = sfnt_create_FontFile_stream(sfont); /* XXX */
    sfnt_close(sfont);
    if verbose > 1i32 {
        info!("[{} bytes]", fontfile.len());
    }
    let fontfile = fontfile.into_obj();
    (*descriptor)
        .as_dict_mut()
        .set("FontFile2", pdf_ref_obj(fontfile));
    pdf_release_obj(fontfile);
    0i32
}
