/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2018 by Jin-Hwan Cho and Shunsaku Hirata,
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
    dfont_open, sfnt_create_FontFile_stream, sfnt_open, sfnt_read_table_directory,
    sfnt_require_table, sfnt_set_table,
};
use crate::bridge::DisplayExt;
use crate::{info, warn};
use std::ffi::{CStr, CString};
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
use super::dpx_tt_glyf::{tt_add_glyph, tt_build_tables, tt_find_glyph, tt_get_index, tt_glyphs};
use super::dpx_tt_gsub::{
    otl_gsub, otl_gsub_add_feat, otl_gsub_apply, otl_gsub_apply_alt, otl_gsub_apply_lig,
    otl_gsub_new, otl_gsub_release, otl_gsub_select,
};
use super::dpx_tt_post::{tt_lookup_post_table, tt_read_post_table, tt_release_post_table};
use super::dpx_tt_table::tt_get_ps_fontname;
use crate::dpx_pdfobj::{pdf_obj, IntoRef, PushObj};
use crate::shims::sprintf;
use libc::{atoi, free, memcpy, memset, strcpy, strlen};

use super::dpx_sfnt::{sfnt, PutBE};

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
#[repr(C)]
pub(crate) struct glyph_mapper<'a> {
    pub(crate) codetogid: *mut tt_cmap,
    pub(crate) gsub: *mut otl_gsub,
    pub(crate) sfont: &'a sfnt,
    pub(crate) nametogid: *mut tt_post_table,
}

/* TrueType */
/* Modifying this has no effect :P */

pub(crate) unsafe fn pdf_font_open_truetype(font: &mut pdf_font) -> i32 {
    let mut embedding: i32 = 1;
    let ident = font.ident.clone(); /* Must be embedded. */
    let index = pdf_font_get_index(font);
    assert!(!ident.is_empty());
    let mut sfont = if let Some(handle) = dpx_open_truetype_file(&ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&ident) {
        if let Some(sfont) = dfont_open(handle, index) {
            sfont
        } else {
            warn!("Could not open TrueType font: {}", ident);
            return -1;
        }
    } else {
        return -1;
    };
    let error = if sfont.type_0 == 1 << 4 {
        let offset = ttc_read_offset(&sfont, index);
        if offset == 0_u32 {
            panic!("Invalid TTC index in {}.", ident);
        }
        sfnt_read_table_directory(&mut sfont, offset)
    } else {
        let offset = sfont.offset;
        sfnt_read_table_directory(&mut sfont, offset)
    };
    if error != 0 {
        return -1;
        /* Silently */
    }
    /* Reading fontdict before checking fonttype conflicts with PKFONT
     * because pdf_font_get_resource() always makes a dictionary.
     */
    let encoding_id = pdf_font_get_encoding(font);
    pdf_font_get_resource(font);

    let fontname = tt_get_ps_fontname(&mut sfont).unwrap_or_else(|| ident.clone());
    if fontname.is_empty() {
        panic!("Can\'t find valid fontname for \"{}\".", ident);
    }
    font.fontname = fontname.clone();
    let tmp = tt_get_fontdesc(&sfont, &mut embedding, -1, 1, &fontname)
        .expect("Could not optain necessary font info.");

    {
        let descriptor = pdf_font_get_descriptor(font);
        assert!(!descriptor.is_null());
        (*descriptor).as_dict_mut().merge(&tmp);
    }
    if embedding == 0 {
        if encoding_id >= 0 && pdf_encoding_is_predefined(encoding_id) == 0 {
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
    let fontdict = pdf_font_get_resource(font);
    fontdict.as_dict_mut().set("Type", "Font");
    fontdict.as_dict_mut().set("Subtype", "TrueType");
    0
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
    let mut firstchar = 255;
    let mut lastchar = 0;
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
    let tfm_id = tfm_open(&*font.map_name, 0);
    for code in firstchar..=lastchar {
        if *usedchars.offset(code as isize) != 0 {
            let width = if tfm_id < 0 {
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
    if !tmparray.is_empty() {
        fontdict.as_dict_mut().set("Widths", tmparray.into_ref());
    }
    fontdict.as_dict_mut().set("FirstChar", firstchar as f64);
    fontdict.as_dict_mut().set("LastChar", lastchar as f64);
}
static mut verbose: i32 = 0;
/*
 * There are several issues in TrueType font support in PDF.
 * How PDF viewers select TrueType cmap table is not so clear.
 * Most reliable way seem to reencode font and sort glyphs as
 * charcode == gid and to use Mac-Roman format 0 subtable.
 * It does not work with encodings that uses full 256 range since
 * GID = 0 is reserved for .notdef, so GID = 256 is not accessible.
 */
unsafe fn do_builtin_encoding(font: &mut pdf_font, usedchars: *const i8, sfont: &mut sfnt) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    let ttcm = tt_cmap_read(sfont, 1_u16, 0_u16);
    if ttcm.is_null() {
        warn!("Could not read Mac-Roman TrueType cmap table...");
        return -1;
    }
    let mut cmap_table = Vec::with_capacity(274);
    cmap_table.put_be(0_u16); //  Version
    cmap_table.put_be(1_u16); // Number of subtables
    cmap_table.put_be(1_u16); // Platform ID
    cmap_table.put_be(0_u16); // Encoding ID
    cmap_table.put_be(12_u32); // Offset
    cmap_table.put_be(0_u16); // Format
    cmap_table.put_be(262_u16); // Length
    cmap_table.put_be(0_u16); // Language

    let mut glyphs = tt_glyphs::init(); /* .notdef */
    if verbose > 2 {
        info!("[glyphs:/.notdef");
    }
    let mut count = 1;
    for code in 0..256 {
        if !(*usedchars.offset(code as isize) == 0) {
            if verbose > 2 {
                info!("/.c0x{:02x}", code);
            }
            let mut idx;
            let gid = tt_cmap_lookup(ttcm, code as u32);
            if gid as i32 == 0 {
                warn!(
                    "Glyph for character code=0x{:02x} missing in font font-file=\"{}\".",
                    code,
                    (&*font).ident,
                );
                idx = 0_u16
            } else {
                idx = tt_find_glyph(&glyphs, gid);
                if idx as i32 == 0 {
                    idx = tt_add_glyph(&mut glyphs, gid, count as u16)
                }
                /* count returned. */
            } /* bug here */
            cmap_table.push((idx as i32 & 0xff) as u8);
            count += 1;
        } else {
            cmap_table.push(0);
        }
    }

    tt_cmap_release(ttcm);
    if verbose > 2 {
        info!("]");
    }
    if tt_build_tables(sfont, &mut glyphs) < 0 {
        warn!("Packing TrueType font into SFNT failed!");
        return -1;
    }
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            let idx = tt_get_index(&glyphs, cmap_table[(18 + code) as usize] as u16);
            widths[code as usize] = (1000.0f64 * glyphs.gd[idx as usize].advw as i32 as f64
                / glyphs.emsize as i32 as f64
                / 1 as f64
                + 0.5f64)
                .floor()
                * 1 as f64
        } else {
            widths[code as usize] = 0.;
        }
    }
    do_widths(font, widths.as_mut_ptr());
    if verbose > 1 {
        info!("[{} glyphs]", glyphs.gd.len());
    }
    sfnt_set_table(sfont, sfnt_table_info::CMAP, cmap_table);
    0
}
/* WARNING: This modifies glyphname itself */
fn agl_decompose_glyphname(glyphname: &mut String) -> (Vec<String>, Option<String>) {
    let mut suffix = None;
    let mut it = glyphname.split('.');
    let it1 = it.next().unwrap();
    if let Some(s) = it.next() {
        suffix = Some(s.to_string());
        *glyphname = it1.to_string();
    }
    let mut nptrs = Vec::new();
    for part in glyphname.split('_') {
        if !part.is_empty() {
            nptrs.push(part.to_string());
        } else {
            break;
        }
    }
    if nptrs.len() > 1 {
        *glyphname = nptrs[0].clone();
    }
    (nptrs, suffix)
}
unsafe fn select_gsub(feat: &[u8], gm: &mut glyph_mapper) -> i32 {
    if feat.is_empty() || gm.gsub.is_null() {
        return -1;
    }
    /* First treat as is */
    let idx = otl_gsub_select(gm.gsub, b"*", b"*", feat);
    if idx >= 0 {
        return 0;
    }
    if verbose > 1 {
        info!(
            "\ntrutype>> Try loading OTL GSUB for \"*.*.{}\"...",
            feat.display()
        );
    }
    let error = otl_gsub_add_feat(&mut *gm.gsub, b"*", b"*", feat, gm.sfont);
    if error == 0 {
        let idx = otl_gsub_select(gm.gsub, b"*", b"*", feat);
        return if idx >= 0 { 0 } else { -1 };
    }
    -1
}
/* Apply GSUB. This is a bit tricky... */
unsafe fn selectglyph(
    mut in_0: u16,
    suffix: *const i8,
    gm: &mut glyph_mapper,
    out: *mut u16,
) -> i32 {
    let mut t: [i8; 5] = [0; 5];
    let mut error;
    assert!(!suffix.is_null() && !out.is_null());
    assert!(!suffix.is_null() && *suffix as i32 != 0);
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
            error = otl_gsub_apply(gm.gsub, &mut in_0)
        }
    } else {
        /* Try loading GSUB only when length of 'suffix' is less
         * than or equal to 4. tt_gsub give a warning otherwise.
         */
        if strlen(s) > 4 {
            error = -1
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
            error = otl_gsub_apply(gm.gsub, &mut in_0)
        } else {
            /* other case: alt1, nalt10... (alternates) */
            let mut q = s.offset(strlen(s) as isize).offset(-1);
            while q > s && *q as i32 >= '0' as i32 && *q as i32 <= '9' as i32 {
                q = q.offset(-1)
            }
            if q == s {
                error = -1
            } else {
                /* starting at 1 */
                let n = atoi(q.offset(1)) - 1;
                *q.offset(1) = '\u{0}' as i32 as i8;
                if strlen(s) > 4 {
                    error = -1
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
                        error = otl_gsub_apply_alt(gm.gsub, n as u16, &mut in_0 as *mut u16)
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
    n_glyphs: usize,
    feat: Option<&str>,
    gm: &mut glyph_mapper,
    gid: *mut u16,
) -> i32 {
    assert!(!glyphs.is_null() && n_glyphs > 0 && !gid.is_null());
    let mut error = match feat {
        None | Some("") => {
            /* meaning "Unknown" */
            select_gsub(b"(?lig|lig?|?cmp|cmp?|frac|afrc)", gm)
        }
        Some(s) if s.len() > 4 => -1,
        Some(s) => {
            let t = s.to_string();
            select_gsub(t.as_bytes(), gm)
        }
    };
    if error == 0 {
        error = otl_gsub_apply_lig(gm.gsub, glyphs, n_glyphs as u16, gid)
    }
    error
}
/* This may be called by findparanoiac(). */
unsafe fn composeuchar(
    unicodes: *mut i32,
    n_unicodes: i32,
    feat: Option<&str>,
    gm: &mut glyph_mapper,
    gid: *mut u16,
) -> i32 {
    let mut error: i32 = 0;
    if gm.codetogid.is_null() {
        return -1;
    }
    let gids =
        new((n_unicodes as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    let mut i = 0;
    while error == 0 && i < n_unicodes {
        *gids.offset(i as isize) =
            tt_cmap_lookup(gm.codetogid, *unicodes.offset(i as isize) as u32);
        error = if *gids.offset(i as isize) as i32 == 0 {
            -1
        } else {
            0
        };
        i += 1
    }
    if error == 0 {
        error = composeglyph(gids, n_unicodes as usize, feat, gm, gid)
    }
    free(gids as *mut libc::c_void);
    error
}
/* Search 'post' table. */
unsafe fn findposttable(glyph_name: &str, gid: *mut u16, gm: *mut glyph_mapper) -> i32 {
    if (*gm).nametogid.is_null() {
        return -1;
    }
    *gid = tt_lookup_post_table((*gm).nametogid, glyph_name);
    if *gid as i32 == 0 {
        -1
    } else {
        0
    }
}
/* This is wrong. We must care about '.'. */
/* Glyph names are concatinated with '_'. */
unsafe fn findcomposite(glyphname: &mut String, gid: *mut u16, gm: &mut glyph_mapper) -> i32 {
    let mut gids: [u16; 32] = [0; 32];
    let error = findposttable(glyphname, gid, gm);
    if error == 0 {
        return 0;
    }
    let (nptrs, suffix) = agl_decompose_glyphname(glyphname);
    let mut error = 0;
    let mut i = 0;
    while error == 0 && i < nptrs.len() as usize {
        error = resolve_glyph(&nptrs[i], &mut *gids.as_mut_ptr().offset(i as isize), gm);
        if error != 0 {
            warn!(
                "Could not resolve glyph \"{}\" ({}th component of glyph \"{}\").",
                nptrs[i], i, glyphname,
            );
        }
        i += 1
    }
    if error == 0 {
        match &suffix {
            Some(suffix)
                if (suffix == "liga"
                    || suffix == "dlig"
                    || suffix == "hlig"
                    || suffix == "frac"
                    || suffix == "ccmp"
                    || suffix == "afrc") =>
            {
                error = composeglyph(gids.as_mut_ptr(), nptrs.len(), Some(suffix), gm, gid)
            }
            Some(suffix) => {
                error = composeglyph(gids.as_mut_ptr(), nptrs.len(), None, gm, gid);
                if error == 0 {
                    /* a_b_c.vert */
                    let suffix = CString::new(suffix.as_bytes()).unwrap();
                    error = selectglyph(*gid, suffix.as_ptr(), gm, gid)
                }
            }
            None => {
                error = composeglyph(gids.as_mut_ptr(), nptrs.len(), None, gm, gid);
            }
        }
    }
    error
}
/* glyphname should not have suffix here */
unsafe fn findparanoiac(glyphname: &[u8], gid: *mut u16, gm: &mut glyph_mapper) -> i32 {
    let mut idx: u16 = 0_u16;
    let mut error;
    let mut agln = agl_lookup_list(glyphname);
    while !agln.is_null() && idx as i32 == 0 {
        if !(*agln).suffix.is_null() {
            error = findparanoiac(CStr::from_ptr((*agln).name).to_bytes(), &mut idx, gm);
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
                //error = 0
                /* ignore */
            }
        } else if (*agln).n_components == 1 {
            idx = tt_cmap_lookup(gm.codetogid, (*agln).unicodes[0] as u32)
        } else if (*agln).n_components > 1 {
            if verbose >= 0 {
                /* give warning */
                warn!(
                    "Glyph \"{}\" looks like a composite glyph...",
                    CStr::from_ptr((*agln).name).display(),
                );
            }
            error = composeuchar(
                (*agln).unicodes.as_mut_ptr(),
                (*agln).n_components,
                None,
                gm,
                &mut idx,
            );
            if verbose >= 0 {
                if error != 0 {
                    warn!("Not found...");
                } else {
                    let mut _i: i32 = 0;
                    let mut _n: i32 = 0;
                    let mut _p: *mut i8 = ptr::null_mut();
                    let mut _buf: [i8; 256] = [0; 256];
                    warn!(
                        ">> Composite glyph glyph-name=\"{}\" found at glyph-id=\"{}\".",
                        CStr::from_ptr((*agln).name).display(),
                        idx,
                    );
                    _p = _buf.as_mut_ptr();
                    _i = 0;
                    while _i < (*agln).n_components && _n < 245 {
                        *_p.offset(_n as isize) =
                            (if _i == 0 { '<' as i32 } else { ' ' as i32 }) as i8;
                        _n = _n + 1;
                        if (*agln).unicodes[_i as usize] >= 0x10000 {
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
                        *_p.offset(_n as isize) = (if _i == (*agln).n_components - 1 {
                            '>' as i32
                        } else {
                            ',' as i32
                        }) as i8;
                        _n = _n + 1;
                        _i += 1;
                    }
                    *_p.offset(_n as isize) = '\u{0}' as i32 as i8;
                    _n = _n + 1;
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
    if idx as i32 == 0 {
        -1
    } else {
        0
    }
}
unsafe fn resolve_glyph(glyphname: &str, gid: *mut u16, gm: &mut glyph_mapper) -> i32 {
    assert!(!glyphname.is_empty());
    /* Boooo */
    /*
     * First we try glyph name to GID mapping using post table if post table
     * is available. If post table is not available or glyph is not listed
     * in the post table, then we try Unicode if Windows-Unicode TrueType
     * cmap is available.
     */
    let error = findposttable(glyphname, gid, gm);
    if error == 0 {
        return 0;
    }
    if gm.codetogid.is_null() {
        return -1;
    }
    let (name, suffix) = agl_chop_suffix(glyphname.as_bytes());
    if let Some(name) = name {
        let mut error = if agl_name_is_unicode(name.to_bytes()) {
            let ucv = agl_name_convert_unicode(name.as_ptr());
            *gid = tt_cmap_lookup(gm.codetogid, ucv as u32);
            if *gid as i32 == 0 {
                -1
            } else {
                0
            }
        } else {
            findparanoiac(name.to_bytes(), gid, gm)
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
                    error = 0
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

impl<'a> Drop for glyph_mapper<'a> {
    fn drop(&mut self) {
        unsafe {
            if !self.gsub.is_null() {
                otl_gsub_release(self.gsub);
            }
            if !self.codetogid.is_null() {
                tt_cmap_release(self.codetogid);
            }
            if !self.nametogid.is_null() {
                tt_release_post_table(self.nametogid);
            }
            self.gsub = ptr::null_mut();
            self.codetogid = ptr::null_mut();
            self.nametogid = ptr::null_mut();
        }
    }
}

unsafe fn do_custom_encoding(
    font: &mut pdf_font,
    encoding: &mut [String],
    usedchars: *const i8,
    sfont: &mut sfnt,
) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    assert!(!encoding[0].is_empty() && !usedchars.is_null());
    /* Things are complicated. We still need to use PostScript
     * glyph names. But OpenType fonts may not have PS name to
     * glyph mapping. We use Unicode plus OTL GSUB for finding
     * glyphs in this case.
     */
    // setup_glyph_mapper
    let nametogid = tt_read_post_table(sfont);
    let mut codetogid = tt_cmap_read(sfont, 3_u16, 10_u16);
    if codetogid.is_null() {
        codetogid = tt_cmap_read(sfont, 3_u16, 1_u16)
    }
    if nametogid.is_null() && codetogid.is_null() {
        warn!(
            "No post table nor Unicode cmap found in font: {}",
            (&*font).ident,
        );
        warn!(">> I can\'t find glyphs without this!");
        return -1;
    } else {
        let mut glyphs;
        let mut cmap_table;
        {
            let mut gm = glyph_mapper {
                sfont,
                nametogid,
                codetogid,
                gsub: otl_gsub_new(),
            };
            cmap_table = Vec::with_capacity(274);
            cmap_table.put_be(0_u16); // Version
            cmap_table.put_be(1_u16); // Number of subtables
            cmap_table.put_be(1_u16); // Platform ID
            cmap_table.put_be(0_u16); // Encoding ID
            cmap_table.put_be(12_u32); // Offset
            cmap_table.put_be(0_u16); // Format
            cmap_table.put_be(262_u16); // Length
            cmap_table.put_be(0_u16); // Language

            glyphs = tt_glyphs::init(); /* +1 for .notdef */
            let mut count = 1;
            for code in 0..256 {
                if !(*usedchars.offset(code as isize) == 0) {
                    let mut gid: u16 = 0;
                    let mut idx;
                    if (encoding[code as usize]).is_empty() || encoding[code as usize] == ".notdef"
                    {
                        warn!("Character code=\"0x{:02X}\" mapped to \".notdef\" glyph used in font font-file=\"{}\"", code,
                                    (&*font).ident);
                        warn!(">> Maybe incorrect encoding specified?");
                        idx = 0_u16
                    } else {
                        let error = if encoding[code as usize].contains('_') {
                            findcomposite(&mut encoding[code as usize], &mut gid, &mut gm)
                        } else {
                            resolve_glyph(&encoding[code as usize], &mut gid, &mut gm)
                        };
                        /*
                         * Older versions of gs had problem with glyphs (other than .notdef)
                         * mapped to gid = 0.
                         */
                        if error != 0 {
                            warn!(
                                "Glyph \"{}\" not available in font \"{}\".",
                                encoding[code as usize],
                                (&*font).ident,
                            ); /* count returned. */
                        } else if verbose > 1 {
                            info!(
                                "truetype>> Glyph glyph-name=\"{}\" found at glyph-id=\"{}\".\n",
                                encoding[code as usize], gid,
                            );
                        }
                        idx = tt_find_glyph(&glyphs, gid);
                        if idx == 0 {
                            idx = tt_add_glyph(&mut glyphs, gid, count as u16);
                            count += 1
                        }
                    }
                    cmap_table.push((idx as i32 & 0xff) as u8);
                } else {
                    cmap_table.push(0);
                }
                /* bug here */
            } /* _FIXME_: wrong message */
        }
        if tt_build_tables(sfont, &mut glyphs) < 0 {
            warn!("Packing TrueType font into SFNT file faild...");
            return -1;
        }
        for code in 0..256 {
            if *usedchars.offset(code as isize) != 0 {
                let idx = tt_get_index(&glyphs, cmap_table[(18 + code) as usize] as u16);
                widths[code as usize] = (1000.0f64 * glyphs.gd[idx as usize].advw as i32 as f64
                    / glyphs.emsize as i32 as f64
                    / 1.
                    + 0.5f64)
                    .floor()
                    * 1.
            } else {
                widths[code as usize] = 0.0f64
            }
        }
        do_widths(font, widths.as_mut_ptr());
        if verbose > 1 {
            info!("[{} glyphs]", glyphs.gd.len());
        }
        sfnt_set_table(sfont, sfnt_table_info::CMAP, cmap_table);
        0
    }
}

pub(crate) unsafe fn pdf_font_load_truetype(font: &mut pdf_font) -> i32 {
    let descriptor: *mut pdf_obj = pdf_font_get_descriptor(font);
    let encoding_id: i32 = pdf_font_get_encoding(font);
    /* ENABLE_NOEMBED */
    let index: i32 = pdf_font_get_index(font); /* Should find *truetype* here */
    if !pdf_font_is_in_use(font) {
        return 0;
    }
    verbose = pdf_font_get_verbose();
    let mut sfont = if let Some(handle) = dpx_open_truetype_file(&font.ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&font.ident) {
        dfont_open(handle, index).expect(&format!(
            "Unable to open TrueType/dfont file: {}",
            font.ident
        ))
    } else {
        panic!("Unable to open TrueType/dfont font file: {}", font.ident);
    };
    if sfont.type_0 != 1 << 0 && sfont.type_0 != 1 << 4 && sfont.type_0 != 1 << 8 {
        panic!("Font \"{}\" not a TrueType/dfont font?", font.ident);
    }
    let error = if sfont.type_0 == 1 << 4 {
        let offset = ttc_read_offset(&mut sfont, index);
        if offset == 0_u32 {
            panic!("Invalid TTC index in {}.", font.ident);
        }
        sfnt_read_table_directory(&mut sfont, offset)
    } else {
        let offset = sfont.offset;
        sfnt_read_table_directory(&mut sfont, offset)
    };
    if error != 0 {
        panic!(
            "Reading SFND table dir failed for font-file=\"{}\"... Not a TrueType font?",
            font.ident
        );
    }
    /*
     * Create new TrueType cmap table with MacRoman encoding.
     */
    let usedchars: *mut i8 = pdf_font_get_usedchars(font);
    let error = if encoding_id < 0 {
        do_builtin_encoding(font, usedchars, &mut sfont)
    } else {
        let enc_vec = pdf_encoding_get_encoding(encoding_id);
        do_custom_encoding(font, enc_vec, usedchars, &mut sfont)
    };
    if error != 0 {
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
        if sfnt_require_table(&mut sfont, table).is_err() {
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
    let fontfile = sfnt_create_FontFile_stream(&mut sfont); /* XXX */
    if verbose > 1 {
        info!("[{} bytes]", fontfile.len());
    }
    (*descriptor)
        .as_dict_mut()
        .set("FontFile2", fontfile.into_ref());
    0
}
