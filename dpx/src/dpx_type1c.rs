/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2007-2016 by Jin-Hwan Cho and Shunsaku Hirata,
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
    non_upper_case_globals,
    unused_mut
)]

use super::dpx_sfnt::{sfnt_close, sfnt_find_table_pos, sfnt_open, sfnt_read_table_directory};
use crate::streq_ptr;
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_cff::{
    cff_add_string, cff_charsets_lookup, cff_charsets_lookup_inverse, cff_close,
    cff_encoding_lookup, cff_get_index_header, cff_get_name, cff_get_sid, cff_get_string,
    cff_index_size, cff_new_index, cff_open, cff_pack_charsets, cff_pack_encoding, cff_pack_index,
    cff_put_header, cff_read_charsets, cff_read_encoding, cff_read_private, cff_read_subrs,
    cff_release_charsets, cff_release_encoding, cff_release_index, cff_set_name, cff_update_string,
    CffIndex, Pack,
};
use super::dpx_cff_dict::{
    cff_dict_add, cff_dict_get, cff_dict_known, cff_dict_pack, cff_dict_remove, cff_dict_set,
    cff_dict_update,
};
use super::dpx_cs_type2::cs_copy_charstring;
use super::dpx_dpxfile::dpx_open_opentype_file;
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer_u8 as work_buffer;
use super::dpx_pdfencoding::{pdf_create_ToUnicode_CMap, pdf_encoding_get_encoding};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_descriptor, pdf_font_get_encoding, pdf_font_get_flag,
    pdf_font_get_fontname, pdf_font_get_ident, pdf_font_get_mapname, pdf_font_get_resource,
    pdf_font_get_uniqueTag, pdf_font_get_usedchars, pdf_font_get_verbose, pdf_font_is_in_use,
    pdf_font_set_flags, pdf_font_set_fontname, pdf_font_set_subtype,
};
use super::dpx_tfm::{tfm_get_width, tfm_open};
use super::dpx_tt_aux::tt_get_fontdesc;
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_array_length, pdf_merge_dict,
    pdf_new_array, pdf_new_name, pdf_new_number, pdf_new_stream, pdf_new_string, pdf_ref_obj,
    pdf_release_obj, pdf_stream_dataptr, pdf_stream_length, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use crate::{ttstub_input_read};
use libc::{free, strcmp, strlen};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;

use super::dpx_cff::cff_index;
pub type l_offset = u32;
use super::dpx_cff::cff_encoding;
use super::dpx_cff::cff_map;
pub type s_SID = u16;
use super::dpx_cff::cff_range1;
/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
/* 1-byte unsigned number */
/* 2-byte unsigned number */
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
/* 1, 2, 3, or 4-byte offset */
/* 2-byte string identifier  */
/* number of objects stored in INDEX */
/* Offset array element size, 1-4    */
/* Offset array, count + 1 offsets   */
/* Object data                       */
/* format major version (starting at 1) */
/* format minor version (starting at 0) */
/* Header size (bytes)                  */
/* Absolute offset (0) size             */
/* Dictionary */
/* encoded data value (as u8 or u16) */
/* opname                                 */
/* number of values                        */
/* values                                  */
/* Encoding, Charset and FDSelect */
/* SID or CID, or u8 for Encoding  */
/* no. of remaining gids/codes in this range */
/* SID or CID (u16)      */
/* u16-version of range1 */
/* if (format & 0x80) then have supplement */
/* number of entries */
/* format 0 */
/* format 1 */
/* number of supplementary data */
/* supplement */
use super::dpx_cff::cff_charsets;

use super::dpx_cff::cff_font;

use super::dpx_cs_type2::cs_ginfo;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/*
 * CFF/OpenType Font support:
 *
 *  Adobe Technical Note #5176, "The Compact Font Format Specfication"
 *
 * NOTE:
 *
 *  Many CFF/OpenType does not have meaningful/correct CFF encoding.
 *  Encoding should be expilicitly supplied in the fontmap.
 *
 */
/* Font info. from OpenType tables */
#[no_mangle]
pub unsafe extern "C" fn pdf_font_open_type1c(mut font: *mut pdf_font) -> i32 {
    assert!(!font.is_null());
    let ident = pdf_font_get_ident(font);
    let encoding_id = pdf_font_get_encoding(font);
    let handle = dpx_open_opentype_file(ident);
    if handle.is_none() {
        return -1i32;
    }
    let handle = handle.unwrap();
    let sfont = sfnt_open(handle);
    if sfont.is_null()
        || (*sfont).type_0 != 1i32 << 2i32
        || sfnt_read_table_directory(sfont, 0_u32) < 0i32
    {
        panic!("Not a CFF/OpenType font (9)?");
    }
    let offset = sfnt_find_table_pos(sfont, b"CFF ");
    if offset < 1_u32 {
        panic!("No \"CFF \" table found; not a CFF/OpenType font (10)?");
    }
    let cffont = cff_open(&mut (*sfont).handle, offset as i32, 0i32); // TODO: use link
    if cffont.is_null() {
        panic!("Could not read CFF font data");
    }
    if (*cffont).flag & 1i32 << 0i32 != 0 {
        cff_close(cffont);
        sfnt_close(sfont);
        return -1i32;
    }
    let fontname = cff_get_name(&*cffont);
    if fontname.is_null() {
        panic!("No valid FontName found in CFF/OpenType font.");
    }
    pdf_font_set_fontname(font, fontname);
    free(fontname as *mut libc::c_void);
    cff_close(cffont);
    /*
     * Font like AdobePiStd does not have meaningful built-in encoding.
     * Some software generate CFF/OpenType font with incorrect encoding.
     */
    if encoding_id < 0i32 {
        warn!("Built-in encoding used for CFF/OpenType font.");
        warn!("CFF font in OpenType font sometimes have strange built-in encoding.");
        warn!("If you find text is not encoded properly in the generated PDF file,");
        warn!("please specify appropriate \".enc\" file in your fontmap.");
    }
    pdf_font_set_subtype(font, 1i32);
    let mut embedding = if pdf_font_get_flag(font, 1i32 << 0i32) != 0 {
        0i32
    } else {
        1i32
    };
    let descriptor = pdf_font_get_descriptor(font);
    /*
     * Create font descriptor from OpenType tables.
     * We can also use CFF TOP DICT/Private DICT for this.
     */
    let tmp = tt_get_fontdesc(sfont, &mut embedding, -1i32, 1i32, fontname); /* copy */
    if tmp.is_null() {
        panic!("Could not obtain neccesary font info from OpenType table.");
    }
    pdf_merge_dict(&mut *descriptor, &*tmp);
    pdf_release_obj(tmp);
    if embedding == 0 {
        /* tt_get_fontdesc may have changed this */
        pdf_font_set_flags(font, 1i32 << 0i32);
    }
    sfnt_close(sfont);
    0i32
}
unsafe extern "C" fn add_SimpleMetrics(
    mut font: *mut pdf_font,
    cffont: &cff_font,
    mut widths: *mut f64,
    mut num_glyphs: u16,
) {
    let mut firstchar;
    let mut lastchar;
    let fontdict = pdf_font_get_resource(&mut *font);
    let usedchars = pdf_font_get_usedchars(font);
    /* The widhts array in the font dictionary must be given relative
     * to the default scaling of 1000:1, not relative to the scaling
     * given by the font matrix.
     */
    let scaling =
        if cff_dict_known(cffont.topdict, b"FontMatrix\x00" as *const u8 as *const i8) != 0 {
            1000i32 as f64
                * cff_dict_get(
                    cffont.topdict,
                    b"FontMatrix\x00" as *const u8 as *const i8,
                    0i32,
                )
        } else {
            1.
        };
    let tmp_array = pdf_new_array();
    if num_glyphs as i32 <= 1i32 {
        /* This should be error. */
        lastchar = 0i32;
        firstchar = lastchar;
        pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
    } else {
        firstchar = 255i32;
        lastchar = 0i32;
        for code in 0..256i32 {
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
            pdf_release_obj(tmp_array);
            panic!("No glyphs used at all!");
        }
        let tfm_id = tfm_open(pdf_font_get_mapname(font), 0i32);
        for code in firstchar..=lastchar {
            if *usedchars.offset(code as isize) != 0 {
                let width;
                if tfm_id < 0i32 {
                    /* tfm is not found */
                    // width = scaling * *widths.offset(code as isize) TODO: check
                } else {
                    width = 1000.0f64 * tfm_get_width(tfm_id, code);
                    let diff = width - scaling * *widths.offset(code as isize);
                    if diff.abs() > 1.0f64 {
                        warn!(
                            "Glyph width mismatch for TFM and font ({})",
                            CStr::from_ptr(pdf_font_get_mapname(font)).display(),
                        );
                        warn!(
                            "TFM: {} vs. CFF font: {}",
                            width,
                            *widths.offset(code as isize),
                        );
                    }
                    pdf_add_array(
                        &mut *tmp_array,
                        pdf_new_number((width / 0.1f64 + 0.5f64).floor() * 0.1f64),
                    );
                }
            } else {
                pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
            }
        }
    }
    if pdf_array_length(&*tmp_array) > 0_u32 {
        pdf_add_dict(fontdict, "Widths", pdf_ref_obj(tmp_array));
    }
    pdf_release_obj(tmp_array);
    pdf_add_dict(fontdict, "FirstChar", pdf_new_number(firstchar as f64));
    pdf_add_dict(fontdict, "LastChar", pdf_new_number(lastchar as f64));
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_load_type1c(mut font: *mut pdf_font) -> i32 {
    let mut offset: i32 = 0i32;
    let mut ginfo = cs_ginfo::new();
    let mut widths: [f64; 256] = [0.; 256];
    assert!(!font.is_null());
    let verbose = pdf_font_get_verbose();
    if !pdf_font_is_in_use(font) {
        return 0i32;
    }
    if pdf_font_get_flag(font, 1i32 << 0i32) != 0 {
        panic!("Only embedded font supported for CFF/OpenType font.");
    }
    let usedchars = pdf_font_get_usedchars(font);
    let fontname = pdf_font_get_fontname(font);
    let ident = pdf_font_get_ident(font);
    let uniqueTag = pdf_font_get_uniqueTag(font);
    if usedchars.is_null() || fontname.is_null() || ident.is_null() {
        panic!("Unexpected error....");
    }
    let fontdict = pdf_font_get_resource(&mut *font); /* Actually string object */
    let descriptor = pdf_font_get_descriptor(font);
    let encoding_id = pdf_font_get_encoding(font);
    let handle = dpx_open_opentype_file(ident);
    if handle.is_none() {
        panic!(
            "Could not open OpenType font: {}",
            CStr::from_ptr(ident).display(),
        );
    }
    let mut handle = handle.unwrap();
    let sfont = sfnt_open(handle);
    if sfont.is_null() {
        panic!(
            "Could not open OpenType font: {}",
            CStr::from_ptr(ident).display(),
        );
    }
    if sfnt_read_table_directory(sfont, 0_u32) < 0i32 {
        panic!(
            "Could not read OpenType table directory: {}",
            CStr::from_ptr(ident).display(),
        );
    }
    if (*sfont).type_0 != 1i32 << 2i32 || {
        offset = sfnt_find_table_pos(sfont, b"CFF ") as i32;
        offset == 0i32
    } {
        panic!("Not a CFF/OpenType font (11)?");
    }
    let cffont = cff_open(&mut (*sfont).handle, offset, 0i32); // TODO: use link
    if cffont.is_null() {
        panic!("Could not open CFF font.");
    }
    let cffont = &mut *cffont;
    if cffont.flag & 1i32 << 0i32 != 0 {
        panic!("This is CIDFont...");
    }
    let fullname =
        new((strlen(fontname).wrapping_add(8)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    sprintf(
        fullname,
        b"%6s+%s\x00" as *const u8 as *const i8,
        uniqueTag,
        fontname,
    );
    /* Offsets from DICTs */
    cff_read_charsets(cffont);
    if encoding_id < 0i32 {
        cff_read_encoding(cffont);
    }
    cff_read_private(cffont);
    cff_read_subrs(cffont);
    /* FIXME */
    cffont._string = cff_new_index(0i32 as u16);
    /* New Charsets data */
    let mut charset =
        &mut *(new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
            as *mut cff_charsets);
    charset.format = 0i32 as u8;
    charset.num_entries = 0i32 as u16;
    charset.data.glyphs =
        new((256_u64).wrapping_mul(::std::mem::size_of::<s_SID>() as u64) as u32) as *mut s_SID;
    /*
     * Encoding related things.
     */
    let enc_vec;
    if encoding_id >= 0i32 {
        enc_vec = pdf_encoding_get_encoding(encoding_id)
    } else {
        /*
         * Create enc_vec and ToUnicode CMap for built-in encoding.
         */
        enc_vec = new((256_u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as u32)
            as *mut *mut i8;
        for code in 0..256 {
            if *usedchars.offset(code as isize) != 0 {
                let gid = cff_encoding_lookup(cffont, code as u8);
                let ref mut fresh0 = *enc_vec.offset(code as isize);
                *fresh0 = cff_get_string(cffont, cff_charsets_lookup_inverse(cffont, gid))
            } else {
                let ref mut fresh1 = *enc_vec.offset(code as isize);
                *fresh1 = ptr::null_mut()
            }
        }
        if !(*fontdict).as_dict().has("ToUnicode") {
            let tounicode = pdf_create_ToUnicode_CMap(fullname, enc_vec, usedchars);
            if !tounicode.is_null() {
                pdf_add_dict(fontdict, "ToUnicode", pdf_ref_obj(tounicode));
                pdf_release_obj(tounicode);
            }
        }
    }
    /*
     * New Encoding data:
     *
     *  We should not use format 0 here.
     *  The number of encoded glyphs (num_entries) is limited to 255 in format 0,
     *  and hence it causes problem for encodings that uses full 256 code-points.
     *  As we always sort glyphs by encoding, we can avoid this problem simply
     *  by using format 1; Using full range result in a single range, 0 255.
     *
     *  Creating actual encoding date is delayed to eliminate character codes to
     *  be mapped to .notdef and to handle multiply-encoded glyphs.
     */
    let encoding = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_encoding>() as u64) as u32)
        as *mut cff_encoding;
    (*encoding).format = 1i32 as u8;
    (*encoding).num_entries = 0i32 as u8;
    (*encoding).data.range1 =
        new((255_u64).wrapping_mul(::std::mem::size_of::<cff_range1>() as u64) as u32)
            as *mut cff_range1;
    (*encoding).num_supps = 0i32 as u8;
    (*encoding).supp =
        new((255_u64).wrapping_mul(::std::mem::size_of::<cff_map>() as u64) as u32) as *mut cff_map;
    /*
     * Charastrings.
     */
    let offset = cff_dict_get(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
    ) as u64;
    cffont.handle.as_mut().unwrap().seek(SeekFrom::Start(cffont.offset as u64 + offset)).unwrap();
    let cs_idx = cff_get_index_header(cffont);
    /* Offset is now absolute offset ... fixme */
    let mut offset = cffont.handle.as_mut().unwrap().seek(SeekFrom::Current(0)).unwrap() as i32;
    let cs_count = (*cs_idx).count;
    if (cs_count as i32) < 2i32 {
        panic!("No valid charstring data found.");
    }
    /* New CharStrings INDEX */
    let charstrings = cff_new_index(257i32 as u16); /* 256 + 1 for ".notdef" glyph */
    let mut max_len = 2 * 65536;
    (*charstrings).data =
        new((max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
    let mut charstring_len = 0;
    /*
     * Information from OpenType table is rough estimate. Replace with accurate value.
     */
    if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"StdVW\x00" as *const u8 as *const i8,
        ) != 0
    {
        let stemv = cff_dict_get(
            *cffont.private.offset(0),
            b"StdVW\x00" as *const u8 as *const i8,
            0i32,
        );
        pdf_add_dict(&mut *descriptor, "StemV", pdf_new_number(stemv));
    }
    /*
     * Widths
     */
    let default_width = if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
        ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let nominal_width = if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
        ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let data = new((65536_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    /* First we add .notdef glyph.
     * All Type 1 font requires .notdef glyph to be present.
     */
    if verbose > 2i32 {
        info!("[glyphs:/.notdef");
    }
    let mut size = (*(*cs_idx).offset.offset(1)).wrapping_sub(*(*cs_idx).offset.offset(0)) as i32;
    if size > 65536i32 {
        panic!("Charstring too long: gid={}, {} bytes", 0, size);
    }
    *(*charstrings).offset.offset(0) = (charstring_len + 1i32) as l_offset;
    let handle = cffont.handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(offset as u64 + *(*cs_idx).offset.offset(0) as u64 - 1)).unwrap();
    ttstub_input_read(handle.0.as_ptr(), data as *mut i8, size as size_t);
    charstring_len += cs_copy_charstring(
        (*charstrings).data.offset(charstring_len as isize),
        max_len - charstring_len,
        data,
        size,
        cffont.gsubr,
        *cffont.subrs.offset(0),
        default_width,
        nominal_width,
        &mut ginfo,
    );
    let notdef_width = ginfo.wx;
    /*
     * Subset font
     */
    let mut num_glyphs = 1_u16;
    let pdfcharset = pdf_new_stream(0i32);
    for code in 0..256 {
        widths[code as usize] = notdef_width;
        if !(*usedchars.offset(code as isize) == 0
            || (*enc_vec.offset(code as isize)).is_null()
            || streq_ptr(
                *enc_vec.offset(code as isize),
                b".notdef\x00" as *const u8 as *const i8,
            ) as i32
                != 0)
        {
            /*
             * FIXME:
             *  cff_get_sid() obtain SID from original String INDEX.
             *  It should be cff_string_get_sid(string, ...).
             *  cff_add_string(cff, ...) -> cff_string_add(string, ...).
             */
            let sid_orig = cff_get_sid(cffont, *enc_vec.offset(code as isize)) as s_SID;
            let sid = (if (sid_orig as i32) < 391i32 {
                sid_orig as i32
            } else {
                cff_add_string(cffont, *enc_vec.offset(code as isize), 0i32) as i32
            }) as s_SID;
            /*
             * We use "unique = 0" because duplicate strings are impossible
             * at this stage unless the original font already had duplicates.
             */
            /*
             * Check if multiply-encoded glyph.
             */
            let mut j = 0;
            while (j as i32) < (*charset).num_entries as i32 {
                if sid as i32 == *(*charset).data.glyphs.offset(j as isize) as i32 {
                    /* Already have this glyph. */
                    (*(*encoding).supp.offset((*encoding).num_supps as isize)).code = code as u8; /* Used but multiply-encoded. */
                    (*(*encoding).supp.offset((*encoding).num_supps as isize)).glyph = sid;
                    *usedchars.offset(code as isize) = 0_i8;
                    (*encoding).num_supps = ((*encoding).num_supps as i32 + 1i32) as u8;
                    break;
                } else {
                    j += 1;
                }
            }
            if !((j as i32) < (*charset).num_entries as i32) {
                /* This is new encoding entry. */
                let gid_0 = cff_charsets_lookup(cffont, sid_orig); /* FIXME */
                if gid_0 as i32 == 0i32 {
                    warn!(
                        "Glyph \"{}\" missing in font \"{}\".",
                        CStr::from_ptr(*enc_vec.offset(code as isize)).display(),
                        CStr::from_ptr(fontname).display(),
                    ); /* Set unused for writing correct encoding */
                    warn!("Maybe incorrect encoding specified.");
                    *usedchars.offset(code as isize) = 0_i8
                } else {
                    pdf_add_stream(
                        &mut *pdfcharset,
                        b"/\x00" as *const u8 as *const i8 as *const libc::c_void,
                        1i32,
                    );
                    pdf_add_stream(
                        &mut *pdfcharset,
                        *enc_vec.offset(code as isize) as *const libc::c_void,
                        strlen(*enc_vec.offset(code as isize)) as i32,
                    );
                    if verbose > 2i32 {
                        info!(
                            "/{}",
                            CStr::from_ptr(*enc_vec.offset(code as isize)).display(),
                        );
                    }
                    size = (*(*cs_idx).offset.offset((gid_0 as i32 + 1i32) as isize))
                        .wrapping_sub(*(*cs_idx).offset.offset(gid_0 as isize))
                        as i32;
                    if size > 65536i32 {
                        panic!("Charstring too long: gid={}, {} bytes", gid_0, size);
                    }
                    if charstring_len + 65536i32 >= max_len {
                        max_len = charstring_len + 2i32 * 65536i32;
                        (*charstrings).data = renew(
                            (*charstrings).data as *mut libc::c_void,
                            (max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                                as u32,
                        ) as *mut u8
                    }
                    *(*charstrings).offset.offset(num_glyphs as isize) =
                        (charstring_len + 1i32) as l_offset;
                    let handle = cffont.handle.as_mut().unwrap();
                    handle.seek(SeekFrom::Start(offset as u64 + *(*cs_idx).offset.offset(gid_0 as isize) as u64 - 1)).unwrap();
                    ttstub_input_read(handle.0.as_ptr(), data as *mut i8, size as size_t);
                    charstring_len += cs_copy_charstring(
                        (*charstrings).data.offset(charstring_len as isize),
                        max_len - charstring_len,
                        data,
                        size,
                        cffont.gsubr,
                        *cffont.subrs.offset(0),
                        default_width,
                        nominal_width,
                        &mut ginfo,
                    );
                    widths[code as usize] = ginfo.wx;
                    *(*charset)
                        .data
                        .glyphs
                        .offset((*charset).num_entries as isize) = sid;
                    (*charset).num_entries = ((*charset).num_entries as i32 + 1i32) as u16;
                    num_glyphs = num_glyphs.wrapping_add(1)
                }
            }
        }
        /* Prevent duplication. */
    }
    if verbose > 2i32 {
        info!("]");
    }
    free(data as *mut libc::c_void);
    /*
     * Now we create encoding data.
     */
    if (*encoding).num_supps as i32 > 0i32 {
        (*encoding).format = ((*encoding).format as i32 | 0x80i32) as u8
    } else {
        free((*encoding).supp as *mut libc::c_void); /* Have supplemantary data. */
        /* FIXME */
    }
    let mut code = 0_u16;
    while code < 256 {
        if !(*usedchars.offset(code as isize) == 0
            || (*enc_vec.offset(code as isize)).is_null()
            || streq_ptr(
                *enc_vec.offset(code as isize),
                b".notdef\x00" as *const u8 as *const i8,
            ) as i32
                != 0)
        {
            (*(*encoding)
                .data
                .range1
                .offset((*encoding).num_entries as isize))
            .first = code;
            (*(*encoding)
                .data
                .range1
                .offset((*encoding).num_entries as isize))
            .n_left = 0i32 as u8;
            code = code.wrapping_add(1);
            while (code as i32) < 256i32
                && *usedchars.offset(code as isize) as i32 != 0
                && !(*enc_vec.offset(code as isize)).is_null()
                && strcmp(
                    *enc_vec.offset(code as isize),
                    b".notdef\x00" as *const u8 as *const i8,
                ) != 0
            {
                let ref mut fresh2 = (*(*encoding)
                    .data
                    .range1
                    .offset((*encoding).num_entries as isize))
                .n_left;
                *fresh2 = (*fresh2 as i32 + 1i32) as u8;
                code = code.wrapping_add(1)
            }
            (*encoding).num_entries = ((*encoding).num_entries as i32 + 1i32) as u8
        }
        code = code.wrapping_add(1)
        /* The above while() loop stopped at unused char or code == 256. */
    }
    /* cleanup */
    if encoding_id < 0i32 && !enc_vec.is_null() {
        for code in 0..256 {
            if !(*enc_vec.offset(code as isize)).is_null() {
                free(*enc_vec.offset(code as isize) as *mut libc::c_void);
            }
        }
        free(enc_vec as *mut libc::c_void);
    }
    cff_release_index(cs_idx);
    *(*charstrings).offset.offset(num_glyphs as isize) = (charstring_len + 1i32) as l_offset;
    (*charstrings).count = num_glyphs;
    charstring_len = cff_index_size(charstrings) as i32;
    cffont.num_glyphs = num_glyphs;
    /*
     * Discard old one, set new data.
     */
    if !cffont.charsets.is_null() {
        cff_release_charsets(cffont.charsets);
    }
    cffont.charsets = charset;
    if !cffont.encoding.is_null() {
        cff_release_encoding(cffont.encoding);
    }
    cffont.encoding = encoding;
    /*
     * We don't use subroutines at all.
     */
    if !cffont.gsubr.is_null() {
        cff_release_index(cffont.gsubr);
    }
    cffont.gsubr = cff_new_index(0i32 as u16);
    if !(*cffont.subrs.offset(0)).is_null() {
        cff_release_index(*cffont.subrs.offset(0));
    }
    let ref mut fresh3 = *cffont.subrs.offset(0);
    *fresh3 = ptr::null_mut();
    /*
     * Flag must be reset since cff_pack_encoding(charset) does not write
     * encoding(charset) if HAVE_STANDARD_ENCODING(CHARSET) is set. We are
     * re-encoding font.
     */
    cffont.flag = 1i32 << 1i32;
    /*
     * FIXME:
     *  Update String INDEX to delete unused strings.
     */
    cff_dict_update(cffont.topdict, cffont);
    if !(*cffont.private.offset(0)).is_null() {
        cff_dict_update(*cffont.private.offset(0), cffont);
    }
    cff_update_string(cffont);
    /*
     * Calculate sizes of Top DICT and Private DICT.
     * All offset values in DICT are set to long (32-bit) integer
     * in cff_dict_pack(), those values are updated later.
     */
    let mut topdict = CffIndex::new(1);

    cff_dict_remove(cffont.topdict, b"UniqueID\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"XUID\x00" as *const u8 as *const i8);
    /*
     * Force existence of Encoding.
     */
    if cff_dict_known(cffont.topdict, b"Encoding\x00" as *const u8 as *const i8) == 0 {
        cff_dict_add(
            cffont.topdict,
            b"Encoding\x00" as *const u8 as *const i8,
            1i32,
        ); /* no Subrs */
    }
    topdict.offset[1] = (cff_dict_pack(cffont.topdict, &mut work_buffer[..]) + 1) as l_offset;
    let mut private_size = 0;
    if !(*cffont.private.offset(0)).is_null() {
        cff_dict_remove(
            *cffont.private.offset(0),
            b"Subrs\x00" as *const u8 as *const i8,
        );
        private_size = cff_dict_pack(*cffont.private.offset(0), &mut work_buffer[..])
    }
    /*
     * Estimate total size of fontfile.
     */
    let mut stream_data_len = 4_usize; /* header size */
    stream_data_len += cff_set_name(cffont, fullname) as usize;
    free(fullname as *mut libc::c_void);
    stream_data_len += topdict.size();
    stream_data_len += cff_index_size(cffont.string);
    stream_data_len += cff_index_size(cffont.gsubr);
    /* We are using format 1 for Encoding and format 0 for charset.
     * TODO: Should implement cff_xxx_size().
     */
    stream_data_len +=
        2 + (*encoding).num_entries as usize * 2 + 1 + (*encoding).num_supps as usize * 3;
    stream_data_len += 1 + (*charset).num_entries as usize * 2;
    stream_data_len += charstring_len as usize;
    stream_data_len += private_size as usize;
    /*
     * Now we create FontFile data.
     */
    let mut stream_data = vec![0_u8; stream_data_len];
    /*
     * Data Layout order as described in CFF spec., sec 2 "Data Layout".
     */
    let mut offset = 0_usize;
    /* Header */
    offset += cff_put_header(cffont, &mut stream_data[offset..]);
    /* Name */
    offset += cff_pack_index(cffont.name, &mut stream_data[offset..]);
    /* Top DICT */
    let topdict_offset = offset;
    offset += topdict.size();
    /* Strings */
    offset += cff_pack_index(cffont.string, &mut stream_data[offset..]);
    /* Global Subrs */
    offset += cff_pack_index(cffont.gsubr, &mut stream_data[offset..]);
    /* Encoding */
    cff_dict_set(
        cffont.topdict,
        b"Encoding\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    offset += cff_pack_encoding(cffont, &mut stream_data[offset..]);
    /* charset */
    cff_dict_set(
        cffont.topdict,
        b"charset\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    offset += cff_pack_charsets(cffont, &mut stream_data[offset..]);
    /* CharStrings */
    cff_dict_set(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    offset += cff_pack_index(
        charstrings,
        &mut stream_data[offset..offset + charstring_len as usize],
    );
    cff_release_index(charstrings);
    /* Private */
    cff_dict_set(
        cffont.topdict,
        b"Private\x00" as *const u8 as *const i8,
        1i32,
        offset as f64,
    );
    if !(*cffont.private.offset(0)).is_null() && private_size > 0 {
        private_size = cff_dict_pack(
            *cffont.private.offset(0),
            &mut stream_data[offset..offset + private_size],
        )
    }
    cff_dict_set(
        cffont.topdict,
        b"Private\x00" as *const u8 as *const i8,
        0i32,
        private_size as f64,
    );
    offset += private_size as usize;
    /* Finally Top DICT */
    topdict.data = vec![0; (topdict.offset[topdict.count as usize]) as usize - 1];
    cff_dict_pack(cffont.topdict, &mut topdict.data[..]);
    let len = topdict.size();
    topdict.pack(&mut stream_data[topdict_offset..topdict_offset + len]);

    /* Copyright and Trademark Notice ommited. */
    /* Handle Widths in fontdict. */
    add_SimpleMetrics(font, cffont, widths.as_mut_ptr(), num_glyphs);
    /* Close font */
    cff_close(cffont);
    sfnt_close(sfont);
    if verbose > 1i32 {
        info!("[{}/{} glyphs][{} bytes]", num_glyphs, cs_count, offset,);
    }
    /*
     * CharSet
     */
    pdf_add_dict(
        &mut *descriptor,
        "CharSet",
        pdf_new_string(
            pdf_stream_dataptr(&*pdfcharset),
            pdf_stream_length(&*pdfcharset) as size_t,
        ),
    );
    pdf_release_obj(pdfcharset);
    /*
     * Write PDF FontFile data.
     */
    let fontfile = pdf_new_stream(STREAM_COMPRESS);
    let stream_dict = (*fontfile).as_stream_mut().get_dict_mut();
    pdf_add_dict(&mut *descriptor, "FontFile3", pdf_ref_obj(fontfile));
    pdf_add_dict(stream_dict, "Subtype", pdf_new_name("Type1C"));
    pdf_add_stream(
        &mut *fontfile,
        stream_data.as_mut_ptr() as *mut libc::c_void,
        offset as i32,
    );
    pdf_release_obj(fontfile);
    0i32
}
