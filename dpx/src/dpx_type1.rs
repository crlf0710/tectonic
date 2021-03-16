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

use crate::mfree;
use crate::{info, warn};
use std::ptr;

use super::dpx_cff::{
    cff_add_string, cff_get_seac_sid, cff_glyph_lookup_str, cff_index_size, cff_new_index,
    cff_pack_charsets, cff_pack_encoding, cff_pack_index, cff_put_header, cff_release_charsets,
    cff_release_index, cff_set_name, cff_update_string, CffIndex, Pack,
};
use super::dpx_mem::{new, renew};
use super::dpx_pdfencoding::{pdf_create_ToUnicode_CMap, pdf_encoding_get_encoding};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_descriptor, pdf_font_get_encoding, pdf_font_get_resource,
    pdf_font_get_uniqueTag, pdf_font_get_usedchars, pdf_font_get_verbose, pdf_font_is_in_use,
    pdf_font_set_flags, pdf_font_set_subtype, FontType,
};
use super::dpx_t1_char::{t1char_convert_charstring, t1char_get_metrics};
use super::dpx_t1_load::{is_pfb, t1_get_fontname, t1_get_standard_glyph, t1_load_font};
use super::dpx_tfm::{tfm_get_width, tfm_open};
use crate::dpx_pdfobj::{pdf_stream, pdf_string, IntoRef, PushObj, STREAM_COMPRESS};
use bridge::{InFile, TTInputFormat};
use libc::free;

/* quasi-hack to get the primary input */
/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
pub(crate) type l_offset = u32;

use super::dpx_cff::cff_font;
/* format major version (starting at 1) */
/* format minor version (starting at 0) */
/* Header size (bytes)                  */
/* Absolute offset (0) size             */
/* Dictionary */
/* encoded data value (as u8 or u16) */
/* opname                                 */
/* number of values                        */
/* values                                  */

use super::dpx_cff::cff_charsets;
/* 1, 2, 3, or 4-byte offset */
pub(crate) type s_SID = u16;
use super::dpx_cff::cff_encoding;
use super::dpx_cff::cff_map;
use super::dpx_cff::cff_range1;

use super::dpx_t1_char::t1_ginfo;

/* Force bold at small text sizes */
unsafe fn is_basefont(name: &str) -> bool {
    const BASEFONTS: [&str; 14] = [
        "Courier",
        "Courier-Bold",
        "Courier-Oblique",
        "Courier-BoldOblique",
        "Helvetica",
        "Helvetica-Bold",
        "Helvetica-Oblique",
        "Helvetica-BoldOblique",
        "Symbol",
        "Times-Roman",
        "Times-Bold",
        "Times-Italic",
        "Times-BoldItalic",
        "ZapfDingbats",
    ];
    for f in &BASEFONTS {
        if name == *f {
            return true;
        }
    }
    false
}

pub(crate) unsafe fn pdf_font_open_type1(font: &mut pdf_font) -> i32 {
    let ident = font.ident.as_str();
    if is_basefont(ident) {
        font.fontname = ident.to_owned();
        pdf_font_set_subtype(font, FontType::Type1);
        pdf_font_set_flags(font, 1 << 0 | 1 << 2);
    } else {
        /* NOTE: skipping qcheck_filetype() call in dpx_find_type1_file but we
         * call is_pfb() in just a second anyway.
         */
        if let Some(mut handle) = InFile::open(ident, TTInputFormat::TYPE1, 0) {
            let mut fontname = String::new();
            if !is_pfb(&mut handle) || t1_get_fontname(&mut handle, &mut fontname) < 0 {
                panic!("Failed to read Type 1 font \"{}\".", ident);
            }
            font.fontname = fontname;
            pdf_font_set_subtype(font, FontType::Type1);
        } else {
            return -1;
        }
    }
    0
}
unsafe fn get_font_attr(font: &mut pdf_font, cffont: &mut cff_font) {
    let italicangle;
    let mut flags: i32 = 0;
    const L_c: [&str; 4] = ["H", "P", "Pi", "Rho"];
    const L_d: [&str; 4] = ["p", "q", "mu", "eta"];
    const L_a: [&str; 3] = ["b", "h", "lambda"];
    let mut gm = t1_ginfo::new();
    let mut defaultwidth = 500_f64;
    let nominalwidth = 0_f64;
    /*
     * CapHeight, Ascent, and Descent is meaningfull only for Latin/Greek/Cyrillic.
     * The BlueValues and OtherBlues also have those information.
     */
    let mut capheight;
    let mut ascent;
    let mut descent;
    if cffont.topdict.contains_key("FontBBox") {
        /* Default values */
        ascent = cffont.topdict.get("FontBBox", 3);
        capheight = ascent;
        descent = cffont.topdict.get("FontBBox", 1)
    } else {
        capheight = 680.0f64;
        ascent = 690.0f64;
        descent = -190.0f64
    }
    let private = cffont.private[0].as_ref().unwrap();
    let stemv = if private.contains_key("StdVW") {
        private.get("StdVW", 0)
    } else {
        /*
         * We may use the following values for StemV:
         *  Thin - ExtraLight: <= 50
         *  Light: 71
         *  Regular(Normal): 88
         *  Medium: 109
         *  SemiBold(DemiBold): 135
         *  Bold - Heavy: >= 166
         */
        88.
    };
    if cffont.topdict.contains_key("ItalicAngle") {
        italicangle = cffont.topdict.get("ItalicAngle", 0);
        if italicangle != 0. {
            flags |= 1 << 6
        }
    } else {
        italicangle = 0.;
    }
    /*
     * Use "space", "H", "p", and "b" for various values.
     * Those characters should not "seac". (no accent)
     */
    let mut gid = cff_glyph_lookup_str(cffont, "space") as i32; /* FIXME */
    if gid >= 0 && gid < (*cffont.cstrings).count as i32 {
        t1char_get_metrics(
            (*cffont.cstrings)
                .data
                .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                .offset(-1),
            (*(*cffont.cstrings).offset.offset((gid + 1) as isize))
                .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize)) as i32,
            &cffont.subrs[0],
            &mut gm,
        );
        defaultwidth = gm.wx
    }
    for i in &L_c {
        gid = cff_glyph_lookup_str(cffont, *i) as i32;
        if gid >= 0 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                &cffont.subrs[0],
                &mut gm,
            );
            capheight = gm.bbox.ury;
            break;
        }
    }
    for i in &L_d {
        gid = cff_glyph_lookup_str(cffont, *i) as i32;
        if gid >= 0 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                &cffont.subrs[0],
                &mut gm,
            );
            descent = gm.bbox.lly;
            break;
        }
    }
    for i in &L_a {
        gid = cff_glyph_lookup_str(cffont, *i) as i32;
        if gid >= 0 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                &cffont.subrs[0],
                &mut gm,
            );
            ascent = gm.bbox.ury;
            break;
        }
    }
    let private = cffont.private[0].as_mut().unwrap();
    if defaultwidth != 0.0f64 {
        private.add("defaultWidthX", 1);
        private.set("defaultWidthX", 0, defaultwidth);
    }
    if nominalwidth != 0.0f64 {
        private.add("nominalWidthX", 1);
        private.set("nominalWidthX", 0, nominalwidth);
    }
    if private.contains_key("ForceBold") && private.get("ForceBold", 0) != 0. {
        flags |= 1 << 18
    }
    if private.contains_key("IsFixedPitch") && private.get("IsFixedPitch", 0) != 0. {
        flags |= 1 << 0
    }
    let fontname = &*font.fontname;
    if !fontname.contains("Sans") {
        flags |= 1 << 1
    }
    if fontname.contains("Caps") {
        flags |= 1 << 17
    }
    flags |= 1 << 2;
    let descriptor = (*pdf_font_get_descriptor(font)).as_dict_mut();
    descriptor.set("CapHeight", capheight);
    descriptor.set("Ascent", ascent);
    descriptor.set("Descent", descent);
    descriptor.set("ItalicAngle", italicangle);
    descriptor.set("StemV", stemv);
    descriptor.set("Flags", flags as f64);
}
unsafe fn add_metrics(
    font: &mut pdf_font,
    cffont: &cff_font,
    enc_vec: &[String],
    widths: *mut f64,
    num_glyphs: i32,
) {
    let mut firstchar;
    let mut lastchar;
    let descriptor = pdf_font_get_descriptor(font);
    let usedchars = pdf_font_get_usedchars(font);
    /*
     * The original FontBBox of the font is preserved, instead
     * of replacing it with tight bounding box calculated from
     * charstrings, to prevent Acrobat 4 from greeking text as
     * much as possible.
     */
    if !cffont.topdict.contains_key("FontBBox") {
        panic!("No FontBBox?");
    }
    /* The widhts array in the font dictionary must be given relative
     * to the default scaling of 1000:1, not relative to the scaling
     * given by the font matrix.
     */
    let scaling = if cffont.topdict.contains_key("FontMatrix") {
        1000. * cffont.topdict.get("FontMatrix", 0)
    } else {
        1.
    };
    let mut tmp_array = vec![];
    for i in 0..4 {
        let val = cffont.topdict.get("FontBBox", i);
        tmp_array.push_obj((val / 1. + 0.5).floor() * 1.);
    }
    (*descriptor).as_dict_mut().set("FontBBox", tmp_array);
    let mut tmp_array = vec![];
    if num_glyphs <= 1 {
        /* This must be an error. */
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
            warn!("No glyphs actually used???");
            return;
        }
        /* PLEASE FIX THIS
         * It's wrong to use TFM width here... We should warn if TFM width
         * and actual glyph width are different.
         */
        let tfm_id = tfm_open(&(&*font).map_name, 0);
        for code in firstchar..=lastchar {
            if *usedchars.offset(code as isize) != 0 {
                let width;
                if tfm_id < 0 {
                    /* tfm is not found */
                    width = scaling
                        * *widths
                            .offset(cff_glyph_lookup_str(cffont, &enc_vec[code as usize]) as isize)
                } else {
                    width = 1000.0f64 * tfm_get_width(tfm_id, code);
                    let diff = width
                        - scaling
                            * *widths.offset(
                                cff_glyph_lookup_str(cffont, &enc_vec[code as usize]) as isize
                            );
                    if diff.abs() > 1.0f64 {
                        warn!(
                            "Glyph width mismatch for TFM and font ({})",
                            (&*font).map_name
                        );
                        warn!(
                            "TFM: {} vs. Type1 font: {}",
                            width,
                            *widths.offset(
                                cff_glyph_lookup_str(cffont, &enc_vec[code as usize]) as isize
                            ),
                        );
                    }
                }
                tmp_array.push_obj((width / 0.1 + 0.5).floor() * 0.1);
            } else {
                tmp_array.push_obj(0f64);
            }
        }
    }
    let empty = tmp_array.is_empty();
    let fontdict = pdf_font_get_resource(font).as_dict_mut(); /* Actually string object */
    if !empty {
        fontdict.set("Widths", tmp_array.into_ref());
    }
    fontdict.set("FirstChar", firstchar as f64);
    fontdict.set("LastChar", lastchar as f64);
}
unsafe fn write_fontfile(
    font: &mut pdf_font,
    cffont: &mut cff_font,
    pdfcharset: &pdf_stream,
) -> i32 {
    let mut wbuf: [u8; 1024] = [0; 1024];
    let descriptor = (*pdf_font_get_descriptor(font)).as_dict_mut();
    let mut topdict = CffIndex::new(1);

    /*
     * Force existence of Encoding.
     */
    if !cffont.topdict.contains_key("CharStrings") {
        cffont.topdict.add("CharStrings", 1);
    }
    if !cffont.topdict.contains_key("charset") {
        cffont.topdict.add("charset", 1);
    }
    if !cffont.topdict.contains_key("Encoding") {
        cffont.topdict.add("Encoding", 1);
    }
    let mut private_size = cffont.private[0].as_mut().unwrap().pack(&mut wbuf[..]);
    /* Private dict is required (but may have size 0) */
    if !cffont.topdict.contains_key("Private") {
        cffont.topdict.add("Private", 2);
    }
    topdict.offset[1] = (cffont.topdict.pack(&mut wbuf[..]) + 1) as l_offset;
    /*
     * Estimate total size of fontfile.
     */
    let charstring_len = cff_index_size(cffont.cstrings); /* header size */
    let mut stream_data_len = 4_usize;
    stream_data_len += cffont.name.size();
    stream_data_len += topdict.size();
    stream_data_len += cffont.string.as_mut().unwrap().size();
    stream_data_len += cffont.gsubr.as_mut().unwrap().size();
    /* We are using format 1 for Encoding and format 0 for charset.
     * TODO: Should implement cff_xxx_size().
     */
    stream_data_len += 2
        + (*cffont.encoding).num_entries as usize * 2
        + 1
        + (*cffont.encoding).num_supps as usize * 3;
    stream_data_len += 1 + (*cffont.charsets).num_entries as usize * 2;
    stream_data_len += charstring_len;
    stream_data_len += private_size;
    /*
     * Now we create FontFile data.
     */
    let mut stream_data = vec![0u8; stream_data_len];
    /*
     * Data Layout order as described in CFF spec., sec 2 "Data Layout".
     */
    let mut offset = 0_usize;
    /* Header */
    offset += cff_put_header(cffont, &mut stream_data[offset..]);
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
    /* TODO: don't write Encoding entry if the font is always used
     * with PDF Encoding information. Applies to type1c.c as well.
     */
    cffont.topdict.set("Encoding", 0, offset as f64);
    offset += cff_pack_encoding(cffont, &mut stream_data[offset..]);
    /* charset */
    cffont.topdict.set("charset", 0, offset as f64);
    offset += cff_pack_charsets(cffont, &mut stream_data[offset..]);
    /* CharStrings */
    cffont.topdict.set("CharStrings", 0, offset as f64);
    offset += cff_pack_index(
        &mut *cffont.cstrings,
        &mut stream_data[offset..offset + charstring_len],
    );
    /* Private */
    if let Some(dict) = cffont.private[0].as_mut() {
        if private_size > 0 {
            private_size = dict.pack(&mut stream_data[offset..offset + private_size]);
            cffont.topdict.set("Private", 1, offset as f64);
            cffont.topdict.set("Private", 0, private_size as f64);
        }
    }
    offset += private_size;
    /* Finally Top DICT */
    topdict.data = vec![0; (topdict.offset[topdict.count as usize]) as usize - 1];
    cffont.topdict.pack(&mut topdict.data[..]);
    let len = topdict.size();
    topdict.pack(&mut stream_data[topdict_offset..topdict_offset + len]);
    /* Copyright and Trademark Notice ommited. */
    /* Flush Font File */
    let mut fontfile = pdf_stream::new(STREAM_COMPRESS);
    fontfile.get_dict_mut().set("Subtype", "Type1C");
    fontfile.add_slice(&stream_data[..offset]);
    descriptor.set("FontFile3", fontfile.into_ref());
    descriptor.set("CharSet", pdf_string::new(&pdfcharset.content));
    offset as i32
}

pub(crate) unsafe fn pdf_font_load_type1(font: &mut pdf_font) -> i32 {
    if !pdf_font_is_in_use(font) {
        return 0;
    }
    let verbose = pdf_font_get_verbose();
    let encoding_id = pdf_font_get_encoding(font);
    pdf_font_get_descriptor(font);
    let usedchars = pdf_font_get_usedchars(font);
    let uniqueTag = pdf_font_get_uniqueTag(font);
    let ident = font.ident.as_str();
    if usedchars.is_null() || ident.is_empty() || font.fontname.is_empty() {
        panic!("Type1: Unexpected error.");
    }
    let handle = InFile::open(ident, TTInputFormat::TYPE1, 0);
    if handle.is_none() {
        panic!("Type1: Could not open Type1 font: {}", ident);
    }
    let handle = handle.unwrap();
    let mut enc_vec: Vec<String> = Vec::new();
    if encoding_id < 0 {
        for _ in 0..=0xff {
            enc_vec.push(String::new());
        }
    };
    let mut cffont = t1_load_font(enc_vec.as_mut_slice(), 0, handle);
    let fullname = format!("{}+{}", uniqueTag, font.fontname);
    /* Encoding related things. */
    let enc_slice = if encoding_id < 0 {
        /* Create enc_vec and ToUnicode CMap for built-in encoding. */
        let fontdict = pdf_font_get_resource(font).as_dict_mut(); /* Actually string object */
        if !fontdict.has("ToUnicode") {
            if let Some(tounicode) =
                pdf_create_ToUnicode_CMap(&fullname, enc_vec.as_mut_slice(), usedchars)
            {
                fontdict.set("ToUnicode", tounicode.into_ref());
            }
        }
        enc_vec.as_mut_slice()
    } else {
        pdf_encoding_get_encoding(encoding_id)
    };
    cff_set_name(&mut cffont, &fullname);
    /* defaultWidthX, CapHeight, etc. */
    get_font_attr(font, &mut cffont);
    let private = cffont.private[0].as_ref().unwrap();
    let defaultwidth = if private.contains_key("defaultWidthX") {
        private.get("defaultWidthX", 0)
    } else {
        0.
    };
    let nominalwidth = if private.contains_key("nominalWidthX") {
        private.get("nominalWidthX", 0)
    } else {
        0.
    };
    /* Create CFF encoding, charset, sort glyphs */
    let GIDMap =
        new((1024_u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32) as *mut u16; /* FIXME */
    let mut pdfcharset = pdf_stream::new(0); /* With pseudo unique tag */
    cffont.encoding = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_encoding>() as u64) as u32)
        as *mut cff_encoding;
    (*cffont.encoding).format = 1 as u8;
    (*cffont.encoding).num_entries = 0 as u8;
    (*cffont.encoding).data.range1 = new((256_u64)
        .wrapping_mul(::std::mem::size_of::<cff_range1>() as u64)
        as u32) as *mut cff_range1;
    (*cffont.encoding).num_supps = 0 as u8;
    (*cffont.encoding).supp =
        new((256_u64).wrapping_mul(::std::mem::size_of::<cff_map>() as u64) as u32) as *mut cff_map;
    let charset = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    (*charset).format = 0 as u8;
    (*charset).num_entries = 0 as u16;
    (*charset).data.glyphs =
        new((1024_u64).wrapping_mul(::std::mem::size_of::<s_SID>() as u64) as u32) as *mut s_SID;
    let gid = cff_glyph_lookup_str(&cffont, ".notdef") as i32;
    if gid < 0 {
        panic!("Type 1 font with no \".notdef\" glyph???");
    }
    *GIDMap.offset(0) = gid as u16;
    if verbose > 2 {
        info!("[glyphs:/.notdef");
    }
    let mut num_glyphs = 1 as u16;
    let mut prev = -2;
    for code in 0..=0xff {
        let glyph = &enc_slice[code as usize];
        if !(*usedchars.offset(code as isize) == 0) {
            if glyph == ".notdef" {
                warn!(
                    "Character mapped to .notdef used in font: {}",
                    font.fontname
                );
                *usedchars.offset(code as isize) = 0_i8
            } else {
                let gid = cff_glyph_lookup_str(&cffont, glyph) as i32;
                if gid < 1 || gid >= (*cffont.cstrings).count as i32 {
                    warn!("Glyph \"{}\" missing in font \"{}\".", glyph, font.fontname);
                    *usedchars.offset(code as isize) = 0_i8
                } else {
                    let mut duplicate = 0;
                    while duplicate < code {
                        if *usedchars.offset(duplicate as isize) as i32 != 0
                            && !(enc_slice[duplicate as usize]).is_empty()
                            && &enc_slice[duplicate as usize] == glyph
                        {
                            break;
                        }
                        duplicate += 1
                    }
                    let sid = cff_add_string(&mut cffont, glyph, 1);
                    if duplicate < code {
                        /* found duplicates */
                        (*(*cffont.encoding)
                            .supp
                            .offset((*cffont.encoding).num_supps as isize))
                        .code = duplicate as u8;
                        (*(*cffont.encoding)
                            .supp
                            .offset((*cffont.encoding).num_supps as isize))
                        .glyph = sid;
                        (*cffont.encoding).num_supps =
                            ((*cffont.encoding).num_supps as i32 + 1) as u8
                    } else {
                        *GIDMap.offset(num_glyphs as isize) = gid as u16;
                        *(*charset)
                            .data
                            .glyphs
                            .offset((*charset).num_entries as isize) = sid;
                        (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16;
                        if code != prev + 1 {
                            (*cffont.encoding).num_entries =
                                ((*cffont.encoding).num_entries as i32 + 1) as u8;
                            (*(*cffont.encoding)
                                .data
                                .range1
                                .offset(((*cffont.encoding).num_entries as i32 - 1) as isize))
                            .first = code as s_SID;
                            (*(*cffont.encoding)
                                .data
                                .range1
                                .offset(((*cffont.encoding).num_entries as i32 - 1) as isize))
                            .n_left = 0;
                        } else {
                            (*(*cffont.encoding)
                                .data
                                .range1
                                .offset(((*cffont.encoding).num_entries as i32 - 1) as isize))
                            .n_left += 1;
                        }
                        prev = code;
                        num_glyphs = num_glyphs.wrapping_add(1);
                        if verbose > 2 {
                            info!("/{}", glyph);
                        }
                        /* CharSet is actually string object. */
                        pdfcharset.add_str("/");
                        pdfcharset.add_slice(glyph.as_bytes());
                    }
                }
            }
        }
    }
    if (*cffont.encoding).num_supps as i32 > 0 {
        (*cffont.encoding).format = ((*cffont.encoding).format as i32 | 0x80) as u8
    } else {
        (*cffont.encoding).supp =
            mfree((*cffont.encoding).supp as *mut libc::c_void) as *mut cff_map
    }
    let widths = new(((*cffont.cstrings).count as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32) as *mut f64;
    /* No more strings will be added. The Type 1 seac operator may add another
     * glyph but the glyph name of those glyphs are contained in standard
     * string. The String Index will not be modified after here. BUT: We
     * cannot update the String Index yet because then we wouldn't be able to
     * find the GIDs of the base and accent characters (unless they have been
     * used already).
     */
    let mut gm = t1_ginfo::new();
    let mut dstlen_max = 0;
    let mut offset = dstlen_max;
    let cstring = cff_new_index((*cffont.cstrings).count);
    (*cstring).data = ptr::null_mut();
    *(*cstring).offset.offset(0) = 1 as l_offset;
    /* The num_glyphs increases if "seac" operators are used. */
    let mut gid_0 = 0_u16;
    while (gid_0 as i32) < num_glyphs as i32 {
        if offset + 65536 >= dstlen_max {
            dstlen_max += 65536 * 2;
            (*cstring).data = renew(
                (*cstring).data as *mut libc::c_void,
                (dstlen_max as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
            ) as *mut u8
        }
        let gid_orig = *GIDMap.offset(gid_0 as isize);
        let dstptr = (*cstring)
            .data
            .offset(*(*cstring).offset.offset(gid_0 as isize) as isize)
            .offset(-1);
        let srcptr = (*cffont.cstrings)
            .data
            .offset(*(*cffont.cstrings).offset.offset(gid_orig as isize) as isize)
            .offset(-1);
        let srclen = (*(*cffont.cstrings)
            .offset
            .offset((gid_orig as i32 + 1) as isize))
        .wrapping_sub(*(*cffont.cstrings).offset.offset(gid_orig as isize))
            as i32;
        offset += t1char_convert_charstring(
            dstptr,
            65536,
            srcptr,
            srclen,
            &cffont.subrs[0],
            defaultwidth,
            nominalwidth,
            &mut gm,
        );
        *(*cstring).offset.offset((gid_0 as i32 + 1) as isize) = (offset + 1) as l_offset;
        if gm.use_seac != 0 {
            /*
             * NOTE:
             *  1. seac.achar and seac.bchar must be contained in the CFF standard string.
             *  2. Those characters need not to be encoded.
             *  3. num_glyphs == charsets->num_entries + 1.
             */
            let achar_name = t1_get_standard_glyph(gm.seac.achar as i32);
            let achar_gid = cff_glyph_lookup_str(&cffont, achar_name) as i32;
            let bchar_name = t1_get_standard_glyph(gm.seac.bchar as i32);
            let bchar_gid = cff_glyph_lookup_str(&cffont, bchar_name) as i32;
            if achar_gid < 0 {
                warn!(
                    "Accent char \"{}\" not found. Invalid use of \"seac\" operator.",
                    achar_name,
                );
                continue;
            } else if bchar_gid < 0 {
                warn!(
                    "Base char \"{}\" not found. Invalid use of \"seac\" operator.",
                    bchar_name,
                );
                continue;
            } else {
                let mut i = 0;
                while i < num_glyphs as i32 {
                    if *GIDMap.offset(i as isize) as i32 == achar_gid {
                        break;
                    }
                    i += 1
                }
                if i == num_glyphs as i32 {
                    if verbose > 2 {
                        info!("/{}", achar_name);
                    }
                    *GIDMap.offset(num_glyphs as isize) = achar_gid as u16;
                    num_glyphs += 1;
                    *(*charset)
                        .data
                        .glyphs
                        .offset((*charset).num_entries as isize) =
                        cff_get_seac_sid(&cffont, achar_name) as s_SID;
                    (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16
                }
                let mut i = 0;
                while i < num_glyphs as i32 {
                    if *GIDMap.offset(i as isize) as i32 == bchar_gid {
                        break;
                    }
                    i += 1;
                }
                if i == num_glyphs as i32 {
                    if verbose > 2 {
                        info!("/{}", bchar_name);
                    }
                    *GIDMap.offset(num_glyphs as isize) = bchar_gid as u16;
                    num_glyphs += 1;
                    *(*charset)
                        .data
                        .glyphs
                        .offset((*charset).num_entries as isize) =
                        cff_get_seac_sid(&cffont, bchar_name) as s_SID;
                    (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16
                }
            }
        }
        *widths.offset(gid_0 as isize) = gm.wx;
        gid_0 += 1;
    }
    (*cstring).count = num_glyphs;
    cffont.subrs = Vec::new();
    cff_release_index(cffont.cstrings);
    cffont.cstrings = cstring;
    cff_release_charsets(cffont.charsets);
    cffont.charsets = charset;
    if verbose > 2 {
        info!("]");
    }
    /* Now we can update the String Index */
    let mut topdict = std::mem::take(&mut cffont.topdict);
    topdict.update(&mut cffont);
    let _ = std::mem::replace(&mut cffont.topdict, topdict);
    let mut private = cffont.private[0].take();
    private.as_mut().unwrap().update(&mut cffont);
    cffont.private[0] = private;
    cff_update_string(&mut cffont);
    add_metrics(font, &cffont, enc_slice, widths, num_glyphs as i32);
    offset = write_fontfile(font, &mut cffont, &pdfcharset);
    if verbose > 1 {
        info!("[{} glyphs][{} bytes]", num_glyphs, offset);
    }
    free(widths as *mut libc::c_void);
    free(GIDMap as *mut libc::c_void);
    /* Maybe writing Charset is recommended for subsetted font. */
    0
}
