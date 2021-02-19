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

use crate::bridge::DisplayExt;
use crate::bridge::{size_t, ttstub_input_get_size, InFile, TTInputFormat};
use crate::info;
use std::io::Read;
use std::ptr;

use super::dpx_agl::{agl_lookup_list_str, agl_sput_UTF16BE};
use super::dpx_cid::CSI_UNICODE;
use super::dpx_cmap::{
    CMap_add_bfchar, CMap_add_codespacerange, CMap_new, CMap_release, CMap_set_CIDSysInfo,
    CMap_set_name, CMap_set_type, CMap_set_wmode,
};
use super::dpx_cmap_read::{CMap_parse, CMap_parse_check_sig};
use super::dpx_cmap_write::CMap_create_stream;
use super::dpx_dpxfile::dpx_tt_open;
use crate::dpx_pdfobj::{
    pdf_dict, pdf_get_version, pdf_link_obj, pdf_name, pdf_obj, pdf_release_obj, pdf_stream,
    IntoObj, PushObj,
};
use crate::dpx_pdfparse::{ParsePdfObj, SkipWhite};

#[derive(Clone)]
pub struct pdf_encoding {
    pub ident: String,
    pub enc_name: String,
    pub flags: i32,
    pub glyphs: [String; 256],
    pub is_used: [i8; 256],
    pub baseenc: *mut pdf_encoding,
    pub tounicode: *mut pdf_obj,
    pub resource: *mut pdf_obj,
}
use super::dpx_agl::agl_name;
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
static mut verbose: u8 = 0_u8;

pub(crate) unsafe fn pdf_encoding_set_verbose(level: i32) {
    verbose = level as u8;
}

const NEW_STRING: String = String::new();

unsafe fn pdf_init_encoding_struct() -> pdf_encoding {
    pdf_encoding {
        ident: String::new(),
        enc_name: String::new(),
        tounicode: ptr::null_mut(),
        baseenc: ptr::null_mut(),
        resource: ptr::null_mut(),
        flags: 0,
        is_used: [0; 256],
        glyphs: [NEW_STRING; 256],
    }
}
/* Creates the PDF Encoding entry for the encoding.
 * If baseenc is non-null, it is used as BaseEncoding entry.
 */
unsafe fn create_encoding_resource(
    encoding: *mut pdf_encoding,
    baseenc: *mut pdf_encoding,
) -> *mut pdf_obj {
    assert!(!encoding.is_null());
    assert!((*encoding).resource.is_null());
    if let Some(differences) = make_encoding_differences(
        &mut (*encoding).glyphs,
        if !baseenc.is_null() {
            Some(&(*baseenc).glyphs)
        } else {
            None
        },
        &mut (*encoding).is_used,
    ) {
        let resource = pdf_dict::new().into_obj();
        if !baseenc.is_null() {
            (*resource)
                .as_dict_mut()
                .set("BaseEncoding", pdf_link_obj((*baseenc).resource));
        }
        (*resource).as_dict_mut().set("Differences", differences);
        return resource;
    } else {
        /* Fix a bug with the MinionPro package using MnSymbol fonts
         * in its virtual fonts:
         *
         * Some font may have font_id even if no character is used.
         * For example, suppose that a virtual file A.vf uses two
         * other fonts, B and C. Even if only characters of B are used
         * in a DVI document, C will have font_id too.
         * In this case, both baseenc and differences can be NULL.
         *
         * Actually these fonts will be ignored in pdffont.c.
         */
        return if !baseenc.is_null() {
            pdf_link_obj((*baseenc).resource)
        } else {
            ptr::null_mut()
        };
    };
}
unsafe fn pdf_flush_encoding(encoding: &mut pdf_encoding) {
    if !encoding.resource.is_null() {
        pdf_release_obj(encoding.resource);
        encoding.resource = ptr::null_mut()
    }
    if !encoding.tounicode.is_null() {
        pdf_release_obj(encoding.tounicode);
        encoding.tounicode = ptr::null_mut()
    };
}
unsafe fn pdf_clean_encoding_struct(encoding: &mut pdf_encoding) {
    if !encoding.resource.is_null() {
        panic!("Object not flushed.");
    }
    pdf_release_obj(encoding.tounicode);
    for code in 0..256 {
        encoding.glyphs[code as usize] = String::new();
    }
}
unsafe fn is_similar_charset(enc_vec: &[String], enc_vec2: &[&str]) -> bool {
    let mut same: i32 = 0;
    for code in 0..256 {
        if !(!(enc_vec[code as usize]).is_empty()
            && enc_vec[code as usize] != enc_vec2[code as usize])
            && {
                same += 1;
                same >= 64
            }
        {
            /* is 64 a good level? */
            return true;
        }
    }
    false
}
/* Creates a PDF Differences array for the encoding, based on the
 * base encoding baseenc (if not NULL). Only character codes which
 * are actually used in the document are considered.
 */
unsafe fn make_encoding_differences(
    enc_vec: &mut [String; 256],
    baseenc: Option<&[String; 256]>,
    is_used: &mut [i8],
) -> Option<Vec<*mut pdf_obj>> {
    let mut count: i32 = 0;
    let mut skipping = true;
    /*
     *  Write all entries (except .notdef) if baseenc is unknown.
     *  If is_used is given, write only used entries.
     */
    let mut differences = Vec::new();
    for code in 0..256 {
        /* We skip NULL (= ".notdef"). Any character code mapped to ".notdef"
         * glyph should not be used in the document.
         */
        if is_used[code] == 0 || enc_vec[code].is_empty() {
            skipping = true
        } else if baseenc.is_none()
            || baseenc.unwrap()[code].is_empty()
            || baseenc.unwrap()[code] != enc_vec[code]
        {
            /*
             * Difference found.
             */
            if skipping {
                differences.push_obj(code as f64);
            }
            differences.push(pdf_name::new(enc_vec[code].as_bytes()).into_obj());
            skipping = false;
            count += 1
        } else {
            skipping = true
        }
    }
    /*
     * No difference found. Some PDF viewers can't handle differences without
     * any differences. We return NULL.
     */
    if count == 0 {
        return None;
    }
    Some(differences)
}
unsafe fn load_encoding_file(filename: &str) -> i32 {
    let mut enc_vec: [&str; 256] = [""; 256];
    if verbose != 0 {
        info!("(Encoding:{}", filename);
    }
    let handle = dpx_tt_open(filename, ".enc", TTInputFormat::ENC);
    if handle.is_none() {
        return -1;
    }
    let mut handle = handle.unwrap();
    let fsize = ttstub_input_get_size(&mut handle) as usize;
    let mut wbuf_0 = vec![0_u8; fsize];
    handle
        .read(&mut wbuf_0[..])
        .unwrap_or_else(|_| panic!("error reading {}", filename));
    let mut p = &wbuf_0[..fsize];
    p.skip_white();
    /*
     * Skip comment lines.
     */
    while !p.is_empty() && p[0] == b'%' {
        p.skip_line();
        p.skip_white();
    }
    let enc_name = if p[0] == b'/' {
        p.parse_pdf_name()
    } else {
        None
    };
    p.skip_white();
    if let Some(encoding_array) = p.parse_pdf_array(ptr::null_mut()) {
        for code in 0..256 {
            enc_vec[code] = (*encoding_array[code]).as_name().to_str().unwrap();
        }
        let enc_id = pdf_encoding_new_encoding(
            if let Some(enc_name) = &enc_name {
                enc_name.name.to_str().unwrap()
            } else {
                ""
            },
            filename,
            &enc_vec,
            None,
            0,
        );
        if let Some(enc_name) = enc_name {
            if verbose as i32 > 1 {
                info!("[{:?}]", enc_name.name.display());
            }
        }
        if verbose != 0 {
            info!(")");
        }
        enc_id
    } else {
        return -1;
    }
}

// Note: The elements are boxed to be able
// to get stable pointers to the cached data.
static mut enc_cache: Vec<Box<pdf_encoding>> = Vec::new();

pub(crate) unsafe fn pdf_init_encodings() {
    enc_cache = Vec::new();
    /*
     * PDF Predefined Encodings
     */
    pdf_encoding_new_encoding(
        "WinAnsiEncoding",
        "WinAnsiEncoding",
        &WinAnsiEncoding[..],
        None,
        1 << 0,
    );
    pdf_encoding_new_encoding(
        "MacRomanEncoding",
        "MacRomanEncoding",
        &MacRomanEncoding[..],
        None,
        1 << 0,
    );
    pdf_encoding_new_encoding(
        "MacExpertEncoding",
        "MacExpertEncoding",
        &MacExpertEncoding[..],
        None,
        1 << 0,
    );
}
/*
 * The original dvipdfm describes as:
 *
 *  Some software doesn't like BaseEncoding key (e.g., FastLane)
 *  so this code is commented out for the moment.  It may reemerge in the
 *  future
 *
 * and the line for BaseEncoding is commented out.
 *
 * I'm not sure why this happens. But maybe BaseEncoding key causes problems
 * when the font is Symbol font or TrueType font.
 */
unsafe fn pdf_encoding_new_encoding(
    enc_name: &str,
    ident: &str,
    encoding_vec: &[&str],
    mut baseenc_name: Option<&str>,
    flags: i32,
) -> i32 {
    let enc_id = enc_cache.len();
    enc_cache.push(Box::new(pdf_init_encoding_struct()));
    let mut encoding = &mut *enc_cache[enc_id];

    encoding.ident = ident.to_owned();
    encoding.enc_name = enc_name.to_owned();

    encoding.flags = flags;
    for code in 0..256 {
        if !encoding_vec[code].is_empty() && encoding_vec[code] != ".notdef" {
            encoding.glyphs[code] = encoding_vec[code].to_string();
        }
    }

    if baseenc_name.is_none()
        && flags & 1 << 0 == 0
        && is_similar_charset(&encoding.glyphs[..], &WinAnsiEncoding[..]) as i32 != 0
    {
        /* Dvipdfmx default setting. */
        baseenc_name = Some("WinAnsiEncoding")
    }
    /* TODO: make base encoding configurable */
    if let Some(baseenc_name) = baseenc_name {
        let baseenc_id: i32 = pdf_encoding_findresource(baseenc_name);
        if baseenc_id < 0 || pdf_encoding_is_predefined(baseenc_id) == 0 {
            panic!(
                "Illegal base encoding {} for encoding {}\n",
                baseenc_name, encoding.enc_name
            );
        }
        encoding.baseenc = &mut *enc_cache[baseenc_id as usize] as *mut pdf_encoding
    }
    if flags & 1 << 0 != 0 {
        encoding.resource = pdf_name::new(encoding.enc_name.as_bytes()).into_obj()
    }
    enc_id as i32
}
/* Creates Encoding resource and ToUnicode CMap
 * for all non-predefined encodings.
 */

pub(crate) unsafe fn pdf_encoding_complete() {
    for encoding in &mut enc_cache {
        const FLAG_IS_PREDEFINED: i32 = 1;
        const FLAG_USED_BY_TYPE3: i32 = 2;
        let encoding_is_predefined = (encoding.flags & FLAG_IS_PREDEFINED) != 0;
        if !encoding_is_predefined {
            /* Section 5.5.4 of the PDF 1.5 reference says that the encoding
             * of a Type 3 font must be completely described by a Differences
             * array, but implementation note 56 explains that this is rather
             * an incorrect implementation in Acrobat 4 and earlier. Hence,
             * we do use a base encodings for PDF versions >= 1.3.
             */
            let with_base = !(encoding.flags & FLAG_USED_BY_TYPE3 != 0) || pdf_get_version() >= 4;
            assert!(encoding.resource.is_null());
            encoding.resource = create_encoding_resource(
                encoding.as_mut(),
                if with_base {
                    encoding.baseenc
                } else {
                    ptr::null_mut()
                },
            );
            assert!(encoding.tounicode.is_null());
            encoding.tounicode = pdf_create_ToUnicode_CMap(
                &encoding.enc_name,
                encoding.glyphs.as_mut(),
                encoding.is_used.as_mut_ptr(),
            )
            .map(IntoObj::into_obj)
            .unwrap_or(ptr::null_mut());
        }
    }
}

pub(crate) unsafe fn pdf_close_encodings() {
    for encoding in &mut enc_cache {
        pdf_flush_encoding(encoding.as_mut());
        pdf_clean_encoding_struct(encoding.as_mut());
    }
    enc_cache.clear();
}

pub(crate) unsafe fn pdf_encoding_findresource(enc_name: &str) -> i32 {
    for (enc_id, encoding) in enc_cache.iter().enumerate() {
        if enc_name == encoding.ident {
            return enc_id as i32;
        } else if enc_name == encoding.enc_name {
            return enc_id as i32;
        }
    }
    load_encoding_file(enc_name)
}
/*
 * Pointer will change if other encoding is loaded...
 */

pub(crate) unsafe fn pdf_encoding_get_encoding<'a>(enc_id: i32) -> &'a mut [String] {
    if enc_id < 0 || enc_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut enc_cache[enc_id as usize];
    &mut encoding.glyphs[..]
}

pub(crate) unsafe fn pdf_get_encoding_obj(enc_id: i32) -> *mut pdf_obj {
    if enc_id < 0 || enc_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache[enc_id as usize];
    encoding.resource
}

pub(crate) unsafe fn pdf_encoding_is_predefined(enc_id: i32) -> i32 {
    if enc_id < 0 || enc_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut enc_cache[enc_id as usize];
    return if (*encoding).flags & 1 << 0 != 0 {
        1
    } else {
        0
    };
}

pub(crate) unsafe fn pdf_encoding_used_by_type3(enc_id: i32) {
    if enc_id < 0 || enc_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut enc_cache[enc_id as usize];
    encoding.flags |= 1 << 1;
}

pub(crate) unsafe fn pdf_encoding_get_name(enc_id: i32) -> String {
    if enc_id < 0 || enc_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut enc_cache[enc_id as usize];
    encoding.enc_name.clone()
}
static mut wbuf: [u8; 1024] = [0; 1024];
static mut range_min: [u8; 1] = [0u32 as u8];
static mut range_max: [u8; 1] = [0xffu32 as u8];

pub(crate) unsafe fn pdf_encoding_add_usedchars(encoding_id: i32, is_used: *const i8) {
    if encoding_id < 0 || encoding_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", encoding_id);
    }
    if is_used.is_null() || pdf_encoding_is_predefined(encoding_id) != 0 {
        return;
    }
    let encoding = &mut enc_cache[encoding_id as usize];
    for code in 0..=0xff {
        (*encoding).is_used[code as usize] = ((*encoding).is_used[code as usize] as i32
            | *is_used.offset(code as isize) as i32)
            as i8;
    }
}

pub(crate) unsafe fn pdf_encoding_get_tounicode(encoding_id: i32) -> *mut pdf_obj {
    if encoding_id < 0 || encoding_id >= enc_cache.len() as i32 {
        panic!("Invalid encoding id: {}", encoding_id);
    }
    enc_cache[encoding_id as usize].tounicode
}
/* Creates a ToUnicode CMap. An empty CMap is replaced by NULL.
 *
 * For PDF <= 1.4 a complete CMap is created unless all character codes
 * are predefined in PDF. For PDF >= 1.5 only those character codes which
 * are not predefined appear in the CMap.
 *
 * Note: The PDF 1.4 reference is not consistent: Section 5.9 describes
 * the Unicode mapping of PDF 1.3 and Section 9.7.2 (in the context of
 * Tagged PDF) the one of PDF 1.5.
 */

pub(crate) unsafe fn pdf_create_ToUnicode_CMap(
    enc_name: &str,
    enc_vec: &mut [String],
    is_used: *const i8,
) -> Option<pdf_stream> {
    assert!(!enc_name.is_empty());

    let mut cmap = CMap_new();
    CMap_set_name(&mut cmap, &format!("{}-UTF16", enc_name));
    CMap_set_type(&mut cmap, 2);
    CMap_set_wmode(&mut cmap, 0);
    CMap_set_CIDSysInfo(&mut cmap, &mut CSI_UNICODE);
    CMap_add_codespacerange(&mut cmap, range_min.as_ptr(), range_max.as_ptr(), 1);
    let mut all_predef = 1;
    for code in 0..=0xff {
        if !(!is_used.is_null() && *is_used.offset(code as isize) == 0) {
            if !(enc_vec[code as usize]).is_empty() {
                let mut fail_count: i32 = 0;
                let agln: *mut agl_name = agl_lookup_list_str(&enc_vec[code as usize]);
                /* Adobe glyph naming conventions are not used by viewers,
                 * hence even ligatures (e.g, "f_i") must be explicitly defined
                 */
                if pdf_get_version() < 5_u32 || agln.is_null() || (*agln).is_predef == 0 {
                    let c8 = [(code & 0xff) as u8];
                    let mut p = wbuf.as_mut_ptr().offset(1);
                    let endptr = wbuf.as_mut_ptr().offset(1024);
                    let len =
                        agl_sput_UTF16BE(&enc_vec[code as usize], &mut p, endptr, &mut fail_count);
                    if len >= 1 && fail_count == 0 {
                        CMap_add_bfchar(
                            &mut cmap,
                            c8.as_ptr(),
                            1,
                            wbuf.as_mut_ptr().offset(1),
                            len as size_t,
                        );
                        all_predef &= (!agln.is_null() && (*agln).is_predef != 0) as i32
                    }
                }
            }
        }
    }
    let stream = if all_predef != 0 {
        None
    } else {
        CMap_create_stream(&mut cmap)
    };
    CMap_release(&mut cmap);
    stream
}
/* Creates Encoding resource and ToUnicode CMap
 * for all non-predefined encodings.
 */
/* enc_name here is .enc file name or the name of predefined
 * encodings.
 */
/* Returns the Encoding resource object.
 */
/* WARNING:
 * Pointer(s) may change after another encoding is loaded.
 */
/*
 * pdf_create_ToUnicode_CMap() returns stream object but not
 * reference. This need to be renamed to other name like
 * pdf_create_ToUnicode_stream().
 */
/* pdf_encoding_copy_usedchars adds the given vector of used characters
 * to the corresponding vector of the encoding.
 */
/* Just load CMap identified with 'ident'. (parsed)
 * PDF stream object (not reference) returned.
 */

pub(crate) unsafe fn pdf_load_ToUnicode_stream(ident: &str) -> Option<pdf_stream> {
    let mut stream = None;
    if ident.is_empty() {
        return None;
    }
    if let Some(handle) = InFile::open(ident, TTInputFormat::CMAP, 0) {
        if CMap_parse_check_sig(&mut &handle) < 0 {
            return None;
        }
        let mut cmap = CMap_new();
        if CMap_parse(&mut cmap, handle).is_err() {
            warn!("Reading CMap file \"{}\" failed.", ident)
        } else {
            if verbose != 0 {
                info!("(CMap:{})", ident);
            }
            stream = CMap_create_stream(&mut cmap);
            if stream.is_none() {
                warn!("Failed to creat ToUnicode CMap stream for \"{}\".", ident)
            }
        }
        CMap_release(&mut cmap);
        stream
    } else {
        None
    }
}
static mut MacRomanEncoding: [&str; 256] = [
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quotesingle",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "grave",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    ".notdef",
    "Adieresis",
    "Aring",
    "Ccedilla",
    "Eacute",
    "Ntilde",
    "Odieresis",
    "Udieresis",
    "aacute",
    "agrave",
    "acircumflex",
    "adieresis",
    "atilde",
    "aring",
    "ccedilla",
    "eacute",
    "egrave",
    "ecircumflex",
    "edieresis",
    "iacute",
    "igrave",
    "icircumflex",
    "idieresis",
    "ntilde",
    "oacute",
    "ograve",
    "ocircumflex",
    "odieresis",
    "otilde",
    "uacute",
    "ugrave",
    "ucircumflex",
    "udieresis",
    "dagger",
    "degree",
    "cent",
    "sterling",
    "section",
    "bullet",
    "paragraph",
    "germandbls",
    "registered",
    "copyright",
    "trademark",
    "acute",
    "dieresis",
    "notequal",
    "AE",
    "Oslash",
    "infinity",
    "plusminus",
    "lessequal",
    "greaterequal",
    "yen",
    "mu",
    "partialdiff",
    "summation",
    "product",
    "pi",
    "integral",
    "ordfeminine",
    "ordmasculine",
    "Omega",
    "ae",
    "oslash",
    "questiondown",
    "exclamdown",
    "logicalnot",
    "radical",
    "florin",
    "approxequal",
    "Delta",
    "guillemotleft",
    "guillemotright",
    "ellipsis",
    "space",
    "Agrave",
    "Atilde",
    "Otilde",
    "OE",
    "oe",
    "endash",
    "emdash",
    "quotedblleft",
    "quotedblright",
    "quoteleft",
    "quoteright",
    "divide",
    "lozenge",
    "ydieresis",
    "Ydieresis",
    "fraction",
    "currency",
    "guilsinglleft",
    "guilsinglright",
    "fi",
    "fl",
    "daggerdbl",
    "periodcentered",
    "quotesinglbase",
    "quotedblbase",
    "perthousand",
    "Acircumflex",
    "Ecircumflex",
    "Aacute",
    "Edieresis",
    "Egrave",
    "Iacute",
    "Icircumflex",
    "Idieresis",
    "Igrave",
    "Oacute",
    "Ocircumflex",
    "apple",
    "Ograve",
    "Uacute",
    "Ucircumflex",
    "Ugrave",
    "dotlessi",
    "circumflex",
    "tilde",
    "macron",
    "breve",
    "dotaccent",
    "ring",
    "cedilla",
    "hungarumlaut",
    "ogonek",
    "caron",
];
static mut MacExpertEncoding: [&str; 256] = [
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "space",
    "exclamsmall",
    "Hungarumlautsmall",
    "centoldstyle",
    "dollaroldstyle",
    "dollarsuperior",
    "ampersandsmall",
    "Acutesmall",
    "parenleftsuperior",
    "parenrightsuperior",
    "twodotenleader",
    "onedotenleader",
    "comma",
    "hyphen",
    "period",
    "fraction",
    "zerooldstyle",
    "oneoldstyle",
    "twooldstyle",
    "threeoldstyle",
    "fouroldstyle",
    "fiveoldstyle",
    "sixoldstyle",
    "sevenoldstyle",
    "eightoldstyle",
    "nineoldstyle",
    "colon",
    "semicolon",
    ".notdef",
    "threequartersemdash",
    ".notdef",
    "questionsmall",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "Ethsmall",
    ".notdef",
    ".notdef",
    "onequarter",
    "onehalf",
    "threequarters",
    "oneeighth",
    "threeeighths",
    "fiveeighths",
    "seveneighths",
    "onethird",
    "twothirds",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "ff",
    "fi",
    "fl",
    "ffi",
    "ffl",
    "parenleftinferior",
    ".notdef",
    "parenrightinferior",
    "Circumflexsmall",
    "hypheninferior",
    "Gravesmall",
    "Asmall",
    "Bsmall",
    "Csmall",
    "Dsmall",
    "Esmall",
    "Fsmall",
    "Gsmall",
    "Hsmall",
    "Ismall",
    "Jsmall",
    "Ksmall",
    "Lsmall",
    "Msmall",
    "Nsmall",
    "Osmall",
    "Psmall",
    "Qsmall",
    "Rsmall",
    "Ssmall",
    "Tsmall",
    "Usmall",
    "Vsmall",
    "Wsmall",
    "Xsmall",
    "Ysmall",
    "Zsmall",
    "colonmonetary",
    "onefitted",
    "rupiah",
    "Tildesmall",
    ".notdef",
    ".notdef",
    "asuperior",
    "centsuperior",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "Aacutesmall",
    "Agravesmall",
    "Acircumflexsmall",
    "Adieresissmall",
    "Atildesmall",
    "Aringsmall",
    "Ccedillasmall",
    "Eacutesmall",
    "Egravesmall",
    "Ecircumflexsmall",
    "Edieresissmall",
    "Iacutesmall",
    "Igravesmall",
    "Icircumflexsmall",
    "Idieresissmall",
    "Ntildesmall",
    "Oacutesmall",
    "Ogravesmall",
    "Ocircumflexsmall",
    "Odieresissmall",
    "Otildesmall",
    "Uacutesmall",
    "Ugravesmall",
    "Ucircumflexsmall",
    "Udieresissmall",
    ".notdef",
    "eightsuperior",
    "fourinferior",
    "threeinferior",
    "sixinferior",
    "eightinferior",
    "seveninferior",
    "Scaronsmall",
    ".notdef",
    "centinferior",
    "twoinferior",
    ".notdef",
    "Dieresissmall",
    ".notdef",
    "Caronsmall",
    "osuperior",
    "fiveinferior",
    ".notdef",
    "commainferior",
    "periodinferior",
    "Yacutesmall",
    ".notdef",
    "dollarinferior",
    ".notdef",
    ".notdef",
    "Thornsmall",
    ".notdef",
    "nineinferior",
    "zeroinferior",
    "Zcaronsmall",
    "AEsmall",
    "Oslashsmall",
    "questiondownsmall",
    "oneinferior",
    "Lslashsmall",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "Cedillasmall",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "OEsmall",
    "figuredash",
    "hyphensuperior",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "exclamdownsmall",
    ".notdef",
    "Ydieresissmall",
    ".notdef",
    "onesuperior",
    "twosuperior",
    "threesuperior",
    "foursuperior",
    "fivesuperior",
    "sixsuperior",
    "sevensuperior",
    "ninesuperior",
    "zerosuperior",
    ".notdef",
    "esuperior",
    "rsuperior",
    "tsuperior",
    ".notdef",
    ".notdef",
    "isuperior",
    "ssuperior",
    "dsuperior",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "lsuperior",
    "Ogoneksmall",
    "Brevesmall",
    "Macronsmall",
    "bsuperior",
    "nsuperior",
    "msuperior",
    "commasuperior",
    "periodsuperior",
    "Dotaccentsmall",
    "Ringsmall",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
];
static mut WinAnsiEncoding: [&str; 256] = [
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quotesingle",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "grave",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    "bullet",
    "Euro",
    "bullet",
    "quotesinglbase",
    "florin",
    "quotedblbase",
    "ellipsis",
    "dagger",
    "daggerdbl",
    "circumflex",
    "perthousand",
    "Scaron",
    "guilsinglleft",
    "OE",
    "bullet",
    "Zcaron",
    "bullet",
    "bullet",
    "quoteleft",
    "quoteright",
    "quotedblleft",
    "quotedblright",
    "bullet",
    "endash",
    "emdash",
    "tilde",
    "trademark",
    "scaron",
    "guilsinglright",
    "oe",
    "bullet",
    "zcaron",
    "Ydieresis",
    "space",
    "exclamdown",
    "cent",
    "sterling",
    "currency",
    "yen",
    "brokenbar",
    "section",
    "dieresis",
    "copyright",
    "ordfeminine",
    "guillemotleft",
    "logicalnot",
    "hyphen",
    "registered",
    "macron",
    "degree",
    "plusminus",
    "twosuperior",
    "threesuperior",
    "acute",
    "mu",
    "paragraph",
    "periodcentered",
    "cedilla",
    "onesuperior",
    "ordmasculine",
    "guillemotright",
    "onequarter",
    "onehalf",
    "threequarters",
    "questiondown",
    "Agrave",
    "Aacute",
    "Acircumflex",
    "Atilde",
    "Adieresis",
    "Aring",
    "AE",
    "Ccedilla",
    "Egrave",
    "Eacute",
    "Ecircumflex",
    "Edieresis",
    "Igrave",
    "Iacute",
    "Icircumflex",
    "Idieresis",
    "Eth",
    "Ntilde",
    "Ograve",
    "Oacute",
    "Ocircumflex",
    "Otilde",
    "Odieresis",
    "multiply",
    "Oslash",
    "Ugrave",
    "Uacute",
    "Ucircumflex",
    "Udieresis",
    "Yacute",
    "Thorn",
    "germandbls",
    "agrave",
    "aacute",
    "acircumflex",
    "atilde",
    "adieresis",
    "aring",
    "ae",
    "ccedilla",
    "egrave",
    "eacute",
    "ecircumflex",
    "edieresis",
    "igrave",
    "iacute",
    "icircumflex",
    "idieresis",
    "eth",
    "ntilde",
    "ograve",
    "oacute",
    "ocircumflex",
    "otilde",
    "odieresis",
    "divide",
    "oslash",
    "ugrave",
    "uacute",
    "ucircumflex",
    "udieresis",
    "yacute",
    "thorn",
    "ydieresis",
];
