/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2007-2018 by Jin-Hwan Cho and Shunsaku Hirata,
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

use super::dpx_sfnt::{sfnt_find_table_pos, sfnt_open, sfnt_read_table_directory};
use crate::{info, warn};

use super::dpx_cff::{
    cff_add_string, cff_charsets_lookup, cff_charsets_lookup_inverse, cff_encoding_lookup,
    cff_get_index_header, cff_get_sid, cff_get_string, cff_index_size, cff_new_index, cff_open,
    cff_pack_charsets, cff_pack_encoding, cff_pack_index, cff_put_header, cff_read_charsets,
    cff_read_encoding, cff_read_private, cff_read_subrs, cff_release_charsets,
    cff_release_encoding, cff_release_index, cff_set_name, cff_update_string, CffIndex, Pack,
};
use super::dpx_cs_type2::cs_copy_charstring;
use super::dpx_dpxfile::dpx_open_opentype_file;
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer_u8 as work_buffer;
use super::dpx_pdfencoding::{pdf_create_ToUnicode_CMap, pdf_encoding_get_encoding};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_descriptor, pdf_font_get_encoding, pdf_font_get_flag,
    pdf_font_get_resource, pdf_font_get_uniqueTag, pdf_font_get_usedchars, pdf_font_get_verbose,
    pdf_font_is_in_use, pdf_font_set_flags, pdf_font_set_subtype, FontType,
};
use super::dpx_tfm::{tfm_get_width, tfm_open};
use super::dpx_tt_aux::tt_get_fontdesc;
use crate::dpx_pdfobj::{pdf_stream, pdf_string, IntoRef, PushObj, STREAM_COMPRESS};
use libc::free;

use std::io::{Read, Seek, SeekFrom};

pub(crate) type l_offset = u32;
use super::dpx_cff::cff_encoding;
use super::dpx_cff::cff_map;
pub(crate) type s_SID = u16;
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

pub(crate) unsafe fn pdf_font_open_type1c(font: &mut pdf_font) -> i32 {
    let ident = &*font.ident;
    let encoding_id = pdf_font_get_encoding(font);
    let handle = dpx_open_opentype_file(ident);
    if handle.is_none() {
        return -1;
    }
    let handle = handle.unwrap();
    let mut sfont = sfnt_open(handle);
    if sfont.type_0 != 1 << 2 || sfnt_read_table_directory(&mut sfont, 0) < 0 {
        panic!("Not a CFF/OpenType font (9)?");
    }
    let offset = sfnt_find_table_pos(&sfont, b"CFF ");
    if offset < 1_u32 {
        panic!("No \"CFF \" table found; not a CFF/OpenType font (10)?");
    }
    let cffont =
        cff_open(sfont.handle.clone(), offset as i32, 0).expect("Could not read CFF font data");
    if cffont.flag & 1 << 0 != 0 {
        return -1;
    }
    let fontname = cffont.name.get_name(cffont.index);
    font.fontname = fontname.clone();
    /*
     * Font like AdobePiStd does not have meaningful built-in encoding.
     * Some software generate CFF/OpenType font with incorrect encoding.
     */
    if encoding_id < 0 {
        warn!("Built-in encoding used for CFF/OpenType font.");
        warn!("CFF font in OpenType font sometimes have strange built-in encoding.");
        warn!("If you find text is not encoded properly in the generated PDF file,");
        warn!("please specify appropriate \".enc\" file in your fontmap.");
    }
    pdf_font_set_subtype(font, FontType::Type1c);
    let mut embedding = if pdf_font_get_flag(font, 1 << 0) != 0 {
        0
    } else {
        1
    };
    let descriptor = pdf_font_get_descriptor(font);
    /*
     * Create font descriptor from OpenType tables.
     * We can also use CFF TOP DICT/Private DICT for this.
     */
    if let Some(tmp) = tt_get_fontdesc(&sfont, &mut embedding, -1, 1, &fontname) {
        /* copy */
        (*descriptor).as_dict_mut().merge(&tmp);
        if embedding == 0 {
            /* tt_get_fontdesc may have changed this */
            pdf_font_set_flags(font, 1 << 0);
        }
        0
    } else {
        panic!("Could not obtain neccesary font info from OpenType table.");
    }
}
unsafe fn add_SimpleMetrics(
    font: &mut pdf_font,
    cffont: &cff_font,
    widths: *mut f64,
    num_glyphs: u16,
) {
    let mut firstchar;
    let mut lastchar;
    let usedchars = pdf_font_get_usedchars(font);
    /* The widhts array in the font dictionary must be given relative
     * to the default scaling of 1000:1, not relative to the scaling
     * given by the font matrix.
     */
    let scaling = if cffont.topdict.contains_key("FontMatrix") {
        1000 as f64 * cffont.topdict.get("FontMatrix", 0)
    } else {
        1.
    };
    let mut tmp_array = vec![];
    if num_glyphs as i32 <= 1 {
        /* This should be error. */
        lastchar = 0;
        firstchar = lastchar;
        tmp_array.push_obj(0f64);
    } else {
        firstchar = 255;
        lastchar = 0;
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
            panic!("No glyphs used at all!");
        }
        let tfm_id = tfm_open(&(&*font).map_name, 0);
        for code in firstchar..=lastchar {
            if *usedchars.offset(code as isize) != 0 {
                let width;
                if tfm_id < 0 {
                    /* tfm is not found */
                    // width = scaling * *widths.offset(code as isize) TODO: check
                } else {
                    width = 1000.0f64 * tfm_get_width(tfm_id, code);
                    let diff = width - scaling * *widths.offset(code as isize);
                    if diff.abs() > 1.0f64 {
                        warn!(
                            "Glyph width mismatch for TFM and font ({})",
                            (&*font).map_name,
                        );
                        warn!(
                            "TFM: {} vs. CFF font: {}",
                            width,
                            *widths.offset(code as isize),
                        );
                    }
                    tmp_array.push_obj((width / 0.1 + 0.5).floor() * 0.1);
                }
            } else {
                tmp_array.push_obj(0f64);
            }
        }
    }

    let fontdict = pdf_font_get_resource(font).as_dict_mut();
    if !tmp_array.is_empty() {
        fontdict.set("Widths", tmp_array.into_ref());
    }
    fontdict.set("FirstChar", firstchar as f64);
    fontdict.set("LastChar", lastchar as f64);
}

pub(crate) unsafe fn pdf_font_load_type1c(font: &mut pdf_font) -> i32 {
    let mut offset: i32 = 0;
    let mut ginfo = cs_ginfo::new();
    let mut widths: [f64; 256] = [0.; 256];
    let verbose = pdf_font_get_verbose();
    if !pdf_font_is_in_use(font) {
        return 0;
    }
    if pdf_font_get_flag(font, 1 << 0) != 0 {
        panic!("Only embedded font supported for CFF/OpenType font.");
    }
    let usedchars = pdf_font_get_usedchars(font);
    let uniqueTag = pdf_font_get_uniqueTag(font);
    if usedchars.is_null() || font.fontname.is_empty() || font.ident.is_empty() {
        panic!("Unexpected error....");
    }
    let encoding_id = pdf_font_get_encoding(font);

    let handle = dpx_open_opentype_file(&font.ident);
    if handle.is_none() {
        panic!("Could not open OpenType font: {}", font.ident);
    }
    let handle = handle.unwrap();
    let mut sfont = sfnt_open(handle);
    if sfnt_read_table_directory(&mut sfont, 0_u32) < 0 {
        panic!("Could not read OpenType table directory: {}", font.ident);
    }
    if sfont.type_0 != 1 << 2 || {
        offset = sfnt_find_table_pos(&sfont, b"CFF ") as i32;
        offset == 0
    } {
        panic!("Not a CFF/OpenType font (11)?");
    }
    let mut cffont = cff_open(sfont.handle.clone(), offset, 0).expect("Could not open CFF font.");
    if cffont.flag & 1 << 0 != 0 {
        panic!("This is CIDFont...");
    }
    let fullname = format!("{}+{}", uniqueTag, font.fontname);
    /* Offsets from DICTs */
    cff_read_charsets(&mut cffont);
    if encoding_id < 0 {
        cff_read_encoding(&mut cffont);
    }
    cff_read_private(&mut cffont);
    cff_read_subrs(&mut cffont);
    /* FIXME */
    cffont._string = Some(CffIndex::new(0));
    /* New Charsets data */
    let mut charset =
        &mut *(new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
            as *mut cff_charsets);
    charset.format = 0 as u8;
    charset.num_entries = 0 as u16;
    charset.data.glyphs =
        new((256_u64).wrapping_mul(::std::mem::size_of::<s_SID>() as u64) as u32) as *mut s_SID;
    /*
     * Encoding related things.
     */
    let mut enc_vec: Vec<String> = Vec::new();
    let enc_slice = if encoding_id >= 0 {
        pdf_encoding_get_encoding(encoding_id)
    } else {
        /*
         * Create enc_vec and ToUnicode CMap for built-in encoding.
         */
        for code in 0..256 {
            if *usedchars.offset(code as isize) != 0 {
                let gid = cff_encoding_lookup(&cffont, code as u8);
                enc_vec.push(cff_get_string(
                    &cffont,
                    cff_charsets_lookup_inverse(&cffont, gid),
                ));
            } else {
                enc_vec.push(String::new());
            }
        }
        let fontdict = pdf_font_get_resource(font).as_dict_mut();
        if !fontdict.has("ToUnicode") {
            if let Some(tounicode) =
                pdf_create_ToUnicode_CMap(&fullname, enc_vec.as_mut_slice(), usedchars)
            {
                fontdict.set("ToUnicode", tounicode.into_ref());
            }
        }
        enc_vec.as_mut_slice()
    };
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
    (*encoding).format = 1 as u8;
    (*encoding).num_entries = 0 as u8;
    (*encoding).data.range1 =
        new((255_u64).wrapping_mul(::std::mem::size_of::<cff_range1>() as u64) as u32)
            as *mut cff_range1;
    (*encoding).num_supps = 0 as u8;
    (*encoding).supp =
        new((255_u64).wrapping_mul(::std::mem::size_of::<cff_map>() as u64) as u32) as *mut cff_map;
    /*
     * Charastrings.
     */
    let offset = cffont.topdict.get("CharStrings", 0) as u64;
    cffont
        .handle
        .as_ref()
        .unwrap()
        .as_ref()
        .seek(SeekFrom::Start(cffont.offset as u64 + offset))
        .unwrap();
    let cs_idx = cff_get_index_header(&cffont);
    /* Offset is now absolute offset ... fixme */
    let offset = cffont
        .handle
        .as_ref()
        .unwrap()
        .as_ref()
        .seek(SeekFrom::Current(0))
        .unwrap() as i32;
    let cs_count = (*cs_idx).count;
    if (cs_count as i32) < 2 {
        panic!("No valid charstring data found.");
    }
    /* New CharStrings INDEX */
    let charstrings = cff_new_index(257 as u16); /* 256 + 1 for ".notdef" glyph */
    let mut max_len = 2 * 65536;
    (*charstrings).data =
        new((max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
    let mut charstring_len = 0;
    /*
     * Information from OpenType table is rough estimate. Replace with accurate value.
     */
    match cffont.private[0].as_ref() {
        Some(dict) if dict.contains_key("StdVW") => {
            let stemv = dict.get("StdVW", 0);
            let descriptor = (*pdf_font_get_descriptor(font)).as_dict_mut();
            descriptor.set("StemV", stemv);
        }
        _ => {}
    }
    /*
     * Widths
     */
    let default_width = match cffont.private[0].as_ref() {
        Some(dict) if dict.contains_key("defaultWidthX") => dict.get("defaultWidthX", 0),
        _ => 0.,
    };
    let nominal_width = match cffont.private[0].as_ref() {
        Some(dict) if dict.contains_key("nominalWidthX") => dict.get("nominalWidthX", 0),
        _ => 0.,
    };
    let data = new((65536_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    /* First we add .notdef glyph.
     * All Type 1 font requires .notdef glyph to be present.
     */
    if verbose > 2 {
        info!("[glyphs:/.notdef");
    }
    let mut size = (*(*cs_idx).offset.offset(1)).wrapping_sub(*(*cs_idx).offset.offset(0)) as i32;
    if size > 65536 {
        panic!("Charstring too long: gid={}, {} bytes", 0, size);
    }
    *(*charstrings).offset.offset(0) = (charstring_len + 1) as l_offset;
    let handle = &mut cffont.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(
            offset as u64 + *(*cs_idx).offset.offset(0) as u64 - 1,
        ))
        .unwrap();
    let slice = std::slice::from_raw_parts_mut(data, size as usize);
    handle.read(slice).unwrap();
    charstring_len += cs_copy_charstring(
        (*charstrings).data.offset(charstring_len as isize),
        max_len - charstring_len,
        data,
        size,
        &cffont.gsubr,
        &cffont.subrs[0],
        default_width,
        nominal_width,
        &mut ginfo,
    );
    let notdef_width = ginfo.wx;
    /*
     * Subset font
     */
    let mut num_glyphs = 1_u16;
    let mut pdfcharset = pdf_stream::new(0);
    for code in 0..256 {
        widths[code as usize] = notdef_width;
        if !(*usedchars.offset(code as isize) == 0
            || enc_slice[code as usize].is_empty()
            || enc_slice[code as usize] == ".notdef")
        {
            /*
             * FIXME:
             *  cff_get_sid() obtain SID from original String INDEX.
             *  It should be cff_string_get_sid(string, ...).
             *  cff_add_string(cff, ...) -> cff_string_add(string, ...).
             */
            let sid_orig = cff_get_sid(&cffont, &enc_slice[code as usize]) as s_SID;
            let sid = (if (sid_orig as i32) < 391 {
                sid_orig as i32
            } else {
                cff_add_string(&mut cffont, &enc_slice[code as usize], 0) as i32
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
                    (*encoding).num_supps = ((*encoding).num_supps as i32 + 1) as u8;
                    break;
                } else {
                    j += 1;
                }
            }
            if !((j as i32) < (*charset).num_entries as i32) {
                /* This is new encoding entry. */
                let gid_0 = cff_charsets_lookup(&cffont, sid_orig); /* FIXME */
                if gid_0 as i32 == 0 {
                    warn!(
                        "Glyph \"{}\" missing in font \"{}\".",
                        enc_slice[code as usize], font.fontname,
                    ); /* Set unused for writing correct encoding */
                    warn!("Maybe incorrect encoding specified.");
                    *usedchars.offset(code as isize) = 0_i8
                } else {
                    pdfcharset.add_str("/");
                    pdfcharset.add_slice(enc_slice[code as usize].as_bytes());
                    if verbose > 2 {
                        info!("/{}", enc_slice[code as usize]);
                    }
                    size = (*(*cs_idx).offset.offset((gid_0 as i32 + 1) as isize))
                        .wrapping_sub(*(*cs_idx).offset.offset(gid_0 as isize))
                        as i32;
                    if size > 65536 {
                        panic!("Charstring too long: gid={}, {} bytes", gid_0, size);
                    }
                    if charstring_len + 65536 >= max_len {
                        max_len = charstring_len + 2 * 65536;
                        (*charstrings).data = renew(
                            (*charstrings).data as *mut libc::c_void,
                            (max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                                as u32,
                        ) as *mut u8
                    }
                    *(*charstrings).offset.offset(num_glyphs as isize) =
                        (charstring_len + 1) as l_offset;
                    let handle = &mut cffont.handle.as_ref().unwrap().as_ref();
                    handle
                        .seek(SeekFrom::Start(
                            offset as u64 + *(*cs_idx).offset.offset(gid_0 as isize) as u64 - 1,
                        ))
                        .unwrap();
                    let slice = std::slice::from_raw_parts_mut(data, size as usize);
                    handle.read(slice).unwrap();
                    charstring_len += cs_copy_charstring(
                        (*charstrings).data.offset(charstring_len as isize),
                        max_len - charstring_len,
                        data,
                        size,
                        &cffont.gsubr,
                        &cffont.subrs[0],
                        default_width,
                        nominal_width,
                        &mut ginfo,
                    );
                    widths[code as usize] = ginfo.wx;
                    *(*charset)
                        .data
                        .glyphs
                        .offset((*charset).num_entries as isize) = sid;
                    (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16;
                    num_glyphs = num_glyphs.wrapping_add(1)
                }
            }
        }
        /* Prevent duplication. */
    }
    if verbose > 2 {
        info!("]");
    }
    free(data as *mut libc::c_void);
    /*
     * Now we create encoding data.
     */
    if (*encoding).num_supps as i32 > 0 {
        (*encoding).format = ((*encoding).format as i32 | 0x80) as u8
    } else {
        free((*encoding).supp as *mut libc::c_void); /* Have supplemantary data. */
        /* FIXME */
    }
    let mut code = 0_u16;
    while code < 256 {
        if !(*usedchars.offset(code as isize) == 0
            || enc_slice[code as usize].is_empty()
            || enc_slice[code as usize] == ".notdef")
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
            .n_left = 0 as u8;
            code = code.wrapping_add(1);
            while (code as i32) < 256
                && *usedchars.offset(code as isize) as i32 != 0
                && !enc_slice[code as usize].is_empty()
                && enc_slice[code as usize] != ".notdef"
            {
                (*(*encoding)
                    .data
                    .range1
                    .offset((*encoding).num_entries as isize))
                .n_left += 1;
                code += 1;
            }
            (*encoding).num_entries = ((*encoding).num_entries as i32 + 1) as u8
        }
        code = code.wrapping_add(1)
        /* The above while() loop stopped at unused char or code == 256. */
    }
    cff_release_index(cs_idx);
    *(*charstrings).offset.offset(num_glyphs as isize) = (charstring_len + 1) as l_offset;
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
    cffont.gsubr = Some(CffIndex::new(0));
    cffont.subrs[0] = None;
    /*
     * Flag must be reset since cff_pack_encoding(charset) does not write
     * encoding(charset) if HAVE_STANDARD_ENCODING(CHARSET) is set. We are
     * re-encoding font.
     */
    cffont.flag = 1 << 1;
    /*
     * FIXME:
     *  Update String INDEX to delete unused strings.
     */
    let mut topdict = std::mem::take(&mut cffont.topdict);
    topdict.update(&mut cffont);
    let _ = std::mem::replace(&mut cffont.topdict, topdict);
    let mut private = cffont.private[0].take();
    if let Some(dict) = private.as_mut() {
        dict.update(&mut cffont);
    }
    cffont.private[0] = private;
    cff_update_string(&mut cffont);
    /*
     * Calculate sizes of Top DICT and Private DICT.
     * All offset values in DICT are set to long (32-bit) integer
     * in cff_dict_pack(), those values are updated later.
     */
    let mut topdict = CffIndex::new(1);

    cffont.topdict.remove("UniqueID");
    cffont.topdict.remove("XUID");
    /*
     * Force existence of Encoding.
     */
    if !cffont.topdict.contains_key("Encoding") {
        cffont.topdict.add("Encoding", 1); /* no Subrs */
    }
    topdict.offset[1] = (cffont.topdict.pack(&mut work_buffer[..]) + 1) as l_offset;
    let mut private_size = 0;
    if let Some(dict) = cffont.private[0].as_mut() {
        dict.remove("Subrs");
        private_size = dict.pack(&mut work_buffer[..])
    }
    /*
     * Estimate total size of fontfile.
     */
    let mut stream_data_len = 4_usize; /* header size */
    stream_data_len += cff_set_name(&mut cffont, &fullname) as usize;
    stream_data_len += topdict.size();
    stream_data_len += cffont.string.as_mut().unwrap().size();
    stream_data_len += cffont.gsubr.as_mut().unwrap().size();
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
    offset += cff_put_header(&cffont, &mut stream_data[offset..]);
    /* Name */
    offset += cffont.name.pack(&mut stream_data[offset..]);
    /* Top DICT */
    let topdict_offset = offset;
    offset += topdict.size();
    /* Strings */
    offset += cffont
        .string
        .as_deref_mut()
        .unwrap()
        .pack(&mut stream_data[offset..]);
    /* Global Subrs */
    offset += cffont
        .gsubr
        .as_mut()
        .unwrap()
        .pack(&mut stream_data[offset..]);
    /* Encoding */
    cffont.topdict.set("Encoding", 0, offset as f64);
    offset += cff_pack_encoding(&cffont, &mut stream_data[offset..]);
    /* charset */
    cffont.topdict.set("charset", 0, offset as f64);
    offset += cff_pack_charsets(&cffont, &mut stream_data[offset..]);
    /* CharStrings */
    cffont.topdict.set("CharStrings", 0, offset as f64);
    offset += cff_pack_index(
        &mut *charstrings,
        &mut stream_data[offset..offset + charstring_len as usize],
    );
    cff_release_index(charstrings);
    /* Private */
    cffont.topdict.set("Private", 1, offset as f64);
    if let Some(dict) = cffont.private[0].as_mut() {
        if private_size > 0 {
            private_size = dict.pack(&mut stream_data[offset..offset + private_size]);
        }
    }
    cffont.topdict.set("Private", 0, private_size as f64);
    offset += private_size as usize;
    /* Finally Top DICT */
    topdict.data = vec![0; (topdict.offset[topdict.count as usize]) as usize - 1];
    cffont.topdict.pack(&mut topdict.data[..]);
    let len = topdict.size();
    topdict.pack(&mut stream_data[topdict_offset..topdict_offset + len]);

    /* Copyright and Trademark Notice ommited. */
    /* Handle Widths in fontdict. */
    add_SimpleMetrics(font, &cffont, widths.as_mut_ptr(), num_glyphs);
    /* Close font */
    if verbose > 1 {
        info!("[{}/{} glyphs][{} bytes]", num_glyphs, cs_count, offset);
    }
    let descriptor = (*pdf_font_get_descriptor(font)).as_dict_mut();
    /*
     * CharSet
     */
    descriptor.set("CharSet", pdf_string::new(&pdfcharset.content));
    /*
     * Write PDF FontFile data.
     */
    let mut fontfile = pdf_stream::new(STREAM_COMPRESS);
    fontfile.get_dict_mut().set("Subtype", "Type1C");
    fontfile.add_slice(&stream_data[..offset]);
    descriptor.set("FontFile3", fontfile.into_ref());
    0
}
