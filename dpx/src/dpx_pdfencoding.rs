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
    non_upper_case_globals,
    unused_mut
)]

use std::io::Read;
use crate::info;
use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_agl::{agl_lookup_list, agl_sput_UTF16BE};
use super::dpx_cid::CSI_UNICODE;
use super::dpx_cmap::{
    CMap_add_bfchar, CMap_add_codespacerange, CMap_new, CMap_release, CMap_set_CIDSysInfo,
    CMap_set_name, CMap_set_type, CMap_set_wmode,
};
use super::dpx_cmap_read::{CMap_parse, CMap_parse_check_sig};
use super::dpx_cmap_write::CMap_create_stream;
use super::dpx_dpxfile::dpx_tt_open;
use super::dpx_mem::{new, renew};
use crate::dpx_pdfobj::{
    pdf_copy_name, pdf_get_version,
    pdf_link_obj, pdf_name_value, pdf_new_dict, pdf_new_number, pdf_obj,
    pdf_release_obj, IntoObj,
};
use crate::dpx_pdfparse::{ParsePdfObj, SkipWhite};
use crate::mfree;
use crate::streq_ptr;
use crate::{ttstub_input_close, ttstub_input_get_size, ttstub_input_open};
use libc::{free, memset, strcmp, strcpy, strlen};

pub type __ssize_t = i64;
pub type size_t = u64;

use crate::TTInputFormat;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_encoding {
    pub ident: *mut i8,
    pub enc_name: *mut i8,
    pub flags: i32,
    pub glyphs: [*mut i8; 256],
    pub is_used: [i8; 256],
    pub baseenc: *mut pdf_encoding,
    pub tounicode: *mut pdf_obj,
    pub resource: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub count: i32,
    pub capacity: i32,
    pub encodings: *mut pdf_encoding,
}
use super::dpx_agl::agl_name;
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
static mut verbose: u8 = 0_u8;

pub unsafe fn pdf_encoding_set_verbose(mut level: i32) {
    verbose = level as u8;
}
unsafe fn pdf_init_encoding_struct(mut encoding: *mut pdf_encoding) {
    assert!(!encoding.is_null());
    (*encoding).ident = ptr::null_mut();
    (*encoding).enc_name = ptr::null_mut();
    memset(
        (*encoding).glyphs.as_mut_ptr() as *mut libc::c_void,
        0i32,
        (256usize).wrapping_mul(::std::mem::size_of::<*mut i8>()),
    );
    memset(
        (*encoding).is_used.as_mut_ptr() as *mut libc::c_void,
        0i32,
        256,
    );
    (*encoding).tounicode = ptr::null_mut();
    (*encoding).baseenc = ptr::null_mut();
    (*encoding).resource = ptr::null_mut();
    (*encoding).flags = 0i32;
}
/* Creates the PDF Encoding entry for the encoding.
 * If baseenc is non-null, it is used as BaseEncoding entry.
 */
unsafe fn create_encoding_resource(
    mut encoding: *mut pdf_encoding,
    mut baseenc: *mut pdf_encoding,
) -> *mut pdf_obj {
    assert!(!encoding.is_null());
    assert!((*encoding).resource.is_null());
    let differences = make_encoding_differences(
        (*encoding).glyphs.as_mut_ptr(),
        if !baseenc.is_null() {
            (*baseenc).glyphs.as_mut_ptr()
        } else {
            0 as *mut *mut i8
        },
        (*encoding).is_used.as_mut_ptr(),
    );
    if !differences.is_null() {
        let mut resource = pdf_new_dict();
        if !baseenc.is_null() {
            (*resource).as_dict_mut().set("BaseEncoding", pdf_link_obj((*baseenc).resource));
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
unsafe fn pdf_flush_encoding(mut encoding: *mut pdf_encoding) {
    assert!(!encoding.is_null());
    if !(*encoding).resource.is_null() {
        pdf_release_obj((*encoding).resource);
        (*encoding).resource = ptr::null_mut()
    }
    if !(*encoding).tounicode.is_null() {
        pdf_release_obj((*encoding).tounicode);
        (*encoding).tounicode = ptr::null_mut()
    };
}
unsafe fn pdf_clean_encoding_struct(mut encoding: *mut pdf_encoding) {
    assert!(!encoding.is_null());
    if !(*encoding).resource.is_null() {
        panic!("Object not flushed.");
    }
    pdf_release_obj((*encoding).tounicode);
    free((*encoding).ident as *mut libc::c_void);
    free((*encoding).enc_name as *mut libc::c_void);
    (*encoding).ident = ptr::null_mut();
    (*encoding).enc_name = ptr::null_mut();
    for code in 0..256 {
        (*encoding).glyphs[code as usize] =
            mfree((*encoding).glyphs[code as usize] as *mut libc::c_void) as *mut i8;
    }
    (*encoding).ident = ptr::null_mut();
    (*encoding).enc_name = ptr::null_mut();
}
unsafe fn is_similar_charset(mut enc_vec: *mut *mut i8, mut enc_vec2: *mut *const i8) -> bool {
    let mut same: i32 = 0i32;
    for code in 0..256 {
        if !(!(*enc_vec.offset(code as isize)).is_null()
            && strcmp(
                *enc_vec.offset(code as isize),
                *enc_vec2.offset(code as isize),
            ) != 0)
            && {
                same += 1;
                same >= 64i32
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
    mut enc_vec: *mut *mut i8,
    mut baseenc: *mut *mut i8,
    mut is_used: *const i8,
) -> *mut pdf_obj {
    let mut count: i32 = 0i32;
    let mut skipping: i32 = 1i32;
    assert!(!enc_vec.is_null());
    /*
     *  Write all entries (except .notdef) if baseenc is unknown.
     *  If is_used is given, write only used entries.
     */
    let mut differences = vec![];
    for code in 0..256 {
        /* We skip NULL (= ".notdef"). Any character code mapped to ".notdef"
         * glyph should not be used in the document.
         */
        if !is_used.is_null() && *is_used.offset(code as isize) == 0
            || (*enc_vec.offset(code as isize)).is_null()
        {
            skipping = 1i32
        } else if baseenc.is_null()
            || (*baseenc.offset(code as isize)).is_null()
            || strcmp(
                *baseenc.offset(code as isize),
                *enc_vec.offset(code as isize),
            ) != 0i32
        {
            /*
             * Difference found.
             */
            if skipping != 0 {
                differences.push(pdf_new_number(code as f64));
            }
            differences.push(pdf_copy_name(*enc_vec.offset(code as isize)));
            skipping = 0i32;
            count += 1
        } else {
            skipping = 1i32
        }
    }
    /*
     * No difference found. Some PDF viewers can't handle differences without
     * any differences. We return NULL.
     */
    if count == 0i32 {
        return ptr::null_mut();
    }
    differences.into_obj()
}
unsafe fn load_encoding_file(mut filename: *const i8) -> i32 {
    let mut enc_vec: [*const i8; 256] = [ptr::null(); 256];
    if filename.is_null() {
        return -1i32;
    }
    if verbose != 0 {
        info!("(Encoding:{}", CStr::from_ptr(filename).display());
    }
    let handle = dpx_tt_open(
        filename,
        b".enc\x00" as *const u8 as *const i8,
        TTInputFormat::ENC,
    );
    if handle.is_none() {
        return -1i32;
    }
    let mut handle = handle.unwrap();
    let fsize = ttstub_input_get_size(&mut handle) as usize;
    let mut wbuf_0 = vec![0_u8; fsize];
    handle.read(&mut wbuf_0[..])
        .expect(&format!("error reading {}", CStr::from_ptr(filename).display()));
    ttstub_input_close(handle);
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
    let encoding_array = p.parse_pdf_array(ptr::null_mut());
    if encoding_array.is_none() {
        pdf_release_obj(enc_name.unwrap_or(ptr::null_mut()));
        return -1i32;
    }
    let encoding_array = encoding_array.unwrap();
    for code in 0..256 {
        enc_vec[code as usize] = pdf_name_value((*encoding_array).as_array().get(code).unwrap()).as_ptr();
    }
    let enc_id = pdf_encoding_new_encoding(
        if let Some(enc_name) = enc_name {
            pdf_name_value(&*enc_name).as_ptr()
        } else {
            ptr::null_mut()
        },
        filename,
        enc_vec.as_mut_ptr(),
        ptr::null(),
        0i32,
    );
    if let Some(enc_name) = enc_name {
        if verbose as i32 > 1i32 {
            info!("[{:?}]", pdf_name_value(&*enc_name).display());
        }
        pdf_release_obj(enc_name);
    }
    pdf_release_obj(encoding_array);
    if verbose != 0 {
        info!(")");
    }
    enc_id
}
static mut enc_cache: C2RustUnnamed = C2RustUnnamed {
        count: 0i32,
        capacity: 0i32,
        encodings: ptr::null_mut(),
    };

pub unsafe fn pdf_init_encodings() {
    enc_cache.count = 0i32;
    enc_cache.capacity = 3i32;
    enc_cache.encodings = new((enc_cache.capacity as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<pdf_encoding>() as u64)
        as u32) as *mut pdf_encoding;
    /*
     * PDF Predefined Encodings
     */
    pdf_encoding_new_encoding(
        b"WinAnsiEncoding\x00" as *const u8 as *const i8,
        b"WinAnsiEncoding\x00" as *const u8 as *const i8,
        WinAnsiEncoding.as_mut_ptr(),
        ptr::null(),
        1i32 << 0i32,
    );
    pdf_encoding_new_encoding(
        b"MacRomanEncoding\x00" as *const u8 as *const i8,
        b"MacRomanEncoding\x00" as *const u8 as *const i8,
        MacRomanEncoding.as_mut_ptr(),
        ptr::null(),
        1i32 << 0i32,
    );
    pdf_encoding_new_encoding(
        b"MacExpertEncoding\x00" as *const u8 as *const i8,
        b"MacExpertEncoding\x00" as *const u8 as *const i8,
        MacExpertEncoding.as_mut_ptr(),
        ptr::null(),
        1i32 << 0i32,
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
    mut enc_name: *const i8,
    mut ident: *const i8,
    mut encoding_vec: *mut *const i8,
    mut baseenc_name: *const i8,
    mut flags: i32,
) -> i32 {
    let enc_id = enc_cache.count;
    let fresh0 = enc_cache.count;
    enc_cache.count = enc_cache.count + 1;
    if fresh0 >= enc_cache.capacity {
        enc_cache.capacity += 16i32;
        enc_cache.encodings = renew(
            enc_cache.encodings as *mut libc::c_void,
            (enc_cache.capacity as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<pdf_encoding>() as u64) as u32,
        ) as *mut pdf_encoding
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    pdf_init_encoding_struct(encoding);
    (*encoding).ident =
        new((strlen(ident).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*encoding).ident, ident);
    (*encoding).enc_name =
        new((strlen(enc_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*encoding).enc_name, enc_name);
    (*encoding).flags = flags;
    for code in 0..256 {
        if !(*encoding_vec.offset(code as isize)).is_null()
            && strcmp(
                *encoding_vec.offset(code as isize),
                b".notdef\x00" as *const u8 as *const i8,
            ) != 0
        {
            (*encoding).glyphs[code as usize] = new((strlen(*encoding_vec.offset(code as isize))
                .wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>())
                as _) as *mut i8;
            strcpy(
                (*encoding).glyphs[code as usize],
                *encoding_vec.offset(code as isize),
            );
        }
    }
    if baseenc_name.is_null()
        && flags & 1i32 << 0i32 == 0
        && is_similar_charset(
            (*encoding).glyphs.as_mut_ptr(),
            WinAnsiEncoding.as_mut_ptr(),
        ) as i32
            != 0
    {
        /* Dvipdfmx default setting. */
        baseenc_name = b"WinAnsiEncoding\x00" as *const u8 as *const i8
    }
    /* TODO: make base encoding configurable */
    if !baseenc_name.is_null() {
        let mut baseenc_id: i32 = pdf_encoding_findresource(baseenc_name);
        if baseenc_id < 0i32 || pdf_encoding_is_predefined(baseenc_id) == 0 {
            panic!(
                "Illegal base encoding {} for encoding {}\n",
                CStr::from_ptr(baseenc_name).display(),
                CStr::from_ptr((*encoding).enc_name).display()
            );
        }
        (*encoding).baseenc =
            &mut *enc_cache.encodings.offset(baseenc_id as isize) as *mut pdf_encoding
    }
    if flags & 1i32 << 0i32 != 0 {
        (*encoding).resource = pdf_copy_name((*encoding).enc_name)
    }
    enc_id
}
/* Creates Encoding resource and ToUnicode CMap
 * for all non-predefined encodings.
 */

pub unsafe fn pdf_encoding_complete() {
    for enc_id in 0..enc_cache.count {
        if pdf_encoding_is_predefined(enc_id) == 0 {
            let mut encoding: *mut pdf_encoding =
                &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
            /* Section 5.5.4 of the PDF 1.5 reference says that the encoding
             * of a Type 3 font must be completely described by a Differences
             * array, but implementation note 56 explains that this is rather
             * an incorrect implementation in Acrobat 4 and earlier. Hence,
             * we do use a base encodings for PDF versions >= 1.3.
             */
            let mut with_base: i32 =
                ((*encoding).flags & 1i32 << 1i32 == 0 || pdf_get_version() >= 4_u32) as i32;
            assert!((*encoding).resource.is_null());
            (*encoding).resource = create_encoding_resource(
                encoding,
                if with_base != 0 {
                    (*encoding).baseenc
                } else {
                    ptr::null_mut()
                },
            );
            assert!((*encoding).tounicode.is_null());
            (*encoding).tounicode = pdf_create_ToUnicode_CMap(
                (*encoding).enc_name,
                (*encoding).glyphs.as_mut_ptr(),
                (*encoding).is_used.as_mut_ptr(),
            )
        }
    }
}

pub unsafe fn pdf_close_encodings() {
    if !enc_cache.encodings.is_null() {
        for enc_id in 0..enc_cache.count {
            let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
            if !encoding.is_null() {
                pdf_flush_encoding(encoding);
                pdf_clean_encoding_struct(encoding);
            }
        }
        free(enc_cache.encodings as *mut libc::c_void);
    }
    enc_cache.encodings = ptr::null_mut();
    enc_cache.count = 0i32;
    enc_cache.capacity = 0i32;
}

pub unsafe fn pdf_encoding_findresource(mut enc_name: *const i8) -> i32 {
    assert!(!enc_name.is_null());
    for enc_id in 0..enc_cache.count {
        let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
        if !(*encoding).ident.is_null() && streq_ptr(enc_name, (*encoding).ident) as i32 != 0 {
            return enc_id;
        } else {
            if !(*encoding).enc_name.is_null()
                && streq_ptr(enc_name, (*encoding).enc_name) as i32 != 0
            {
                return enc_id;
            }
        }
    }
    load_encoding_file(enc_name)
}
/*
 * Pointer will change if other encoding is loaded...
 */

pub unsafe fn pdf_encoding_get_encoding(mut enc_id: i32) -> *mut *mut i8 {
    if enc_id < 0i32 || enc_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    (*encoding).glyphs.as_mut_ptr()
}

pub unsafe fn pdf_get_encoding_obj(mut enc_id: i32) -> *mut pdf_obj {
    if enc_id < 0i32 || enc_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    (*encoding).resource
}

pub unsafe fn pdf_encoding_is_predefined(mut enc_id: i32) -> i32 {
    if enc_id < 0i32 || enc_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    return if (*encoding).flags & 1i32 << 0i32 != 0 {
        1i32
    } else {
        0i32
    };
}

pub unsafe fn pdf_encoding_used_by_type3(mut enc_id: i32) {
    if enc_id < 0i32 || enc_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    (*encoding).flags |= 1i32 << 1i32;
}

pub unsafe fn pdf_encoding_get_name(mut enc_id: i32) -> *mut i8 {
    if enc_id < 0i32 || enc_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", enc_id);
    }
    let encoding = &mut *enc_cache.encodings.offset(enc_id as isize) as *mut pdf_encoding;
    (*encoding).enc_name
}
static mut wbuf: [u8; 1024] = [0; 1024];
static mut range_min: [u8; 1] = [0u32 as u8];
static mut range_max: [u8; 1] = [0xffu32 as u8];

pub unsafe fn pdf_encoding_add_usedchars(mut encoding_id: i32, mut is_used: *const i8) {
    if encoding_id < 0i32 || encoding_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", encoding_id);
    }
    if is_used.is_null() || pdf_encoding_is_predefined(encoding_id) != 0 {
        return;
    }
    let encoding = &mut *enc_cache.encodings.offset(encoding_id as isize) as *mut pdf_encoding;
    for code in 0..=0xff {
        (*encoding).is_used[code as usize] = ((*encoding).is_used[code as usize] as i32
            | *is_used.offset(code as isize) as i32)
            as i8;
    }
}

pub unsafe fn pdf_encoding_get_tounicode(mut encoding_id: i32) -> *mut pdf_obj {
    if encoding_id < 0i32 || encoding_id >= enc_cache.count {
        panic!("Invalid encoding id: {}", encoding_id);
    }
    (*enc_cache.encodings.offset(encoding_id as isize)).tounicode
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

pub unsafe fn pdf_create_ToUnicode_CMap(
    mut enc_name: *const i8,
    mut enc_vec: *mut *mut i8,
    mut is_used: *const i8,
) -> *mut pdf_obj {
    assert!(!enc_name.is_null() && !enc_vec.is_null());

    let cmap = CMap_new();
    CMap_set_name(cmap, &format!("{}-UTF16", CStr::from_ptr(enc_name).display()));
    CMap_set_type(cmap, 2i32);
    CMap_set_wmode(cmap, 0i32);
    CMap_set_CIDSysInfo(cmap, &mut CSI_UNICODE);
    CMap_add_codespacerange(
        cmap,
        range_min.as_mut_ptr(),
        range_max.as_mut_ptr(),
        1i32 as size_t,
    );
    let mut all_predef = 1;
    for code in 0..=0xff {
        if !(!is_used.is_null() && *is_used.offset(code as isize) == 0) {
            if !(*enc_vec.offset(code as isize)).is_null() {
                let mut fail_count: i32 = 0i32;
                let mut agln: *mut agl_name = agl_lookup_list(*enc_vec.offset(code as isize));
                /* Adobe glyph naming conventions are not used by viewers,
                 * hence even ligatures (e.g, "f_i") must be explicitly defined
                 */
                if pdf_get_version() < 5_u32 || agln.is_null() || (*agln).is_predef == 0 {
                    wbuf[0] = (code & 0xffi32) as u8;
                    let mut p = wbuf.as_mut_ptr().offset(1);
                    let endptr = wbuf.as_mut_ptr().offset(1024);
                    let len = agl_sput_UTF16BE(
                        *enc_vec.offset(code as isize),
                        &mut p,
                        endptr,
                        &mut fail_count,
                    );
                    if len >= 1i32 && fail_count == 0 {
                        CMap_add_bfchar(
                            cmap,
                            wbuf.as_mut_ptr(),
                            1i32 as size_t,
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
        ptr::null_mut()
    } else {
        CMap_create_stream(cmap)
    };
    CMap_release(cmap);
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

pub unsafe fn pdf_load_ToUnicode_stream(mut ident: *const i8) -> *mut pdf_obj {
    let mut stream: *mut pdf_obj = ptr::null_mut();
    if ident.is_null() {
        return ptr::null_mut();
    }
    let mut handle = ttstub_input_open(ident, TTInputFormat::CMAP, 0i32);
    if handle.is_none() {
        return ptr::null_mut();
    }
    if CMap_parse_check_sig(handle.as_mut()) < 0i32 {
        ttstub_input_close(handle.unwrap());
        return ptr::null_mut();
    }
    let mut handle = handle.unwrap();
    let cmap = CMap_new();
    if CMap_parse(cmap, handle) < 0i32 {
        warn!(
            "Reading CMap file \"{}\" failed.",
            CStr::from_ptr(ident).display()
        )
    } else {
        if verbose != 0 {
            info!("(CMap:{})", CStr::from_ptr(ident).display());
        }
        stream = CMap_create_stream(cmap);
        if stream.is_null() {
            warn!(
                "Failed to creat ToUnicode CMap stream for \"{}\".",
                CStr::from_ptr(ident).display()
            )
        }
    }
    CMap_release(cmap);
    stream
}
static mut MacRomanEncoding: [*const i8; 256] = [
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclam\x00" as *const u8 as *const i8,
    b"quotedbl\x00" as *const u8 as *const i8,
    b"numbersign\x00" as *const u8 as *const i8,
    b"dollar\x00" as *const u8 as *const i8,
    b"percent\x00" as *const u8 as *const i8,
    b"ampersand\x00" as *const u8 as *const i8,
    b"quotesingle\x00" as *const u8 as *const i8,
    b"parenleft\x00" as *const u8 as *const i8,
    b"parenright\x00" as *const u8 as *const i8,
    b"asterisk\x00" as *const u8 as *const i8,
    b"plus\x00" as *const u8 as *const i8,
    b"comma\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"period\x00" as *const u8 as *const i8,
    b"slash\x00" as *const u8 as *const i8,
    b"zero\x00" as *const u8 as *const i8,
    b"one\x00" as *const u8 as *const i8,
    b"two\x00" as *const u8 as *const i8,
    b"three\x00" as *const u8 as *const i8,
    b"four\x00" as *const u8 as *const i8,
    b"five\x00" as *const u8 as *const i8,
    b"six\x00" as *const u8 as *const i8,
    b"seven\x00" as *const u8 as *const i8,
    b"eight\x00" as *const u8 as *const i8,
    b"nine\x00" as *const u8 as *const i8,
    b"colon\x00" as *const u8 as *const i8,
    b"semicolon\x00" as *const u8 as *const i8,
    b"less\x00" as *const u8 as *const i8,
    b"equal\x00" as *const u8 as *const i8,
    b"greater\x00" as *const u8 as *const i8,
    b"question\x00" as *const u8 as *const i8,
    b"at\x00" as *const u8 as *const i8,
    b"A\x00" as *const u8 as *const i8,
    b"B\x00" as *const u8 as *const i8,
    b"C\x00" as *const u8 as *const i8,
    b"D\x00" as *const u8 as *const i8,
    b"E\x00" as *const u8 as *const i8,
    b"F\x00" as *const u8 as *const i8,
    b"G\x00" as *const u8 as *const i8,
    b"H\x00" as *const u8 as *const i8,
    b"I\x00" as *const u8 as *const i8,
    b"J\x00" as *const u8 as *const i8,
    b"K\x00" as *const u8 as *const i8,
    b"L\x00" as *const u8 as *const i8,
    b"M\x00" as *const u8 as *const i8,
    b"N\x00" as *const u8 as *const i8,
    b"O\x00" as *const u8 as *const i8,
    b"P\x00" as *const u8 as *const i8,
    b"Q\x00" as *const u8 as *const i8,
    b"R\x00" as *const u8 as *const i8,
    b"S\x00" as *const u8 as *const i8,
    b"T\x00" as *const u8 as *const i8,
    b"U\x00" as *const u8 as *const i8,
    b"V\x00" as *const u8 as *const i8,
    b"W\x00" as *const u8 as *const i8,
    b"X\x00" as *const u8 as *const i8,
    b"Y\x00" as *const u8 as *const i8,
    b"Z\x00" as *const u8 as *const i8,
    b"bracketleft\x00" as *const u8 as *const i8,
    b"backslash\x00" as *const u8 as *const i8,
    b"bracketright\x00" as *const u8 as *const i8,
    b"asciicircum\x00" as *const u8 as *const i8,
    b"underscore\x00" as *const u8 as *const i8,
    b"grave\x00" as *const u8 as *const i8,
    b"a\x00" as *const u8 as *const i8,
    b"b\x00" as *const u8 as *const i8,
    b"c\x00" as *const u8 as *const i8,
    b"d\x00" as *const u8 as *const i8,
    b"e\x00" as *const u8 as *const i8,
    b"f\x00" as *const u8 as *const i8,
    b"g\x00" as *const u8 as *const i8,
    b"h\x00" as *const u8 as *const i8,
    b"i\x00" as *const u8 as *const i8,
    b"j\x00" as *const u8 as *const i8,
    b"k\x00" as *const u8 as *const i8,
    b"l\x00" as *const u8 as *const i8,
    b"m\x00" as *const u8 as *const i8,
    b"n\x00" as *const u8 as *const i8,
    b"o\x00" as *const u8 as *const i8,
    b"p\x00" as *const u8 as *const i8,
    b"q\x00" as *const u8 as *const i8,
    b"r\x00" as *const u8 as *const i8,
    b"s\x00" as *const u8 as *const i8,
    b"t\x00" as *const u8 as *const i8,
    b"u\x00" as *const u8 as *const i8,
    b"v\x00" as *const u8 as *const i8,
    b"w\x00" as *const u8 as *const i8,
    b"x\x00" as *const u8 as *const i8,
    b"y\x00" as *const u8 as *const i8,
    b"z\x00" as *const u8 as *const i8,
    b"braceleft\x00" as *const u8 as *const i8,
    b"bar\x00" as *const u8 as *const i8,
    b"braceright\x00" as *const u8 as *const i8,
    b"asciitilde\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Adieresis\x00" as *const u8 as *const i8,
    b"Aring\x00" as *const u8 as *const i8,
    b"Ccedilla\x00" as *const u8 as *const i8,
    b"Eacute\x00" as *const u8 as *const i8,
    b"Ntilde\x00" as *const u8 as *const i8,
    b"Odieresis\x00" as *const u8 as *const i8,
    b"Udieresis\x00" as *const u8 as *const i8,
    b"aacute\x00" as *const u8 as *const i8,
    b"agrave\x00" as *const u8 as *const i8,
    b"acircumflex\x00" as *const u8 as *const i8,
    b"adieresis\x00" as *const u8 as *const i8,
    b"atilde\x00" as *const u8 as *const i8,
    b"aring\x00" as *const u8 as *const i8,
    b"ccedilla\x00" as *const u8 as *const i8,
    b"eacute\x00" as *const u8 as *const i8,
    b"egrave\x00" as *const u8 as *const i8,
    b"ecircumflex\x00" as *const u8 as *const i8,
    b"edieresis\x00" as *const u8 as *const i8,
    b"iacute\x00" as *const u8 as *const i8,
    b"igrave\x00" as *const u8 as *const i8,
    b"icircumflex\x00" as *const u8 as *const i8,
    b"idieresis\x00" as *const u8 as *const i8,
    b"ntilde\x00" as *const u8 as *const i8,
    b"oacute\x00" as *const u8 as *const i8,
    b"ograve\x00" as *const u8 as *const i8,
    b"ocircumflex\x00" as *const u8 as *const i8,
    b"odieresis\x00" as *const u8 as *const i8,
    b"otilde\x00" as *const u8 as *const i8,
    b"uacute\x00" as *const u8 as *const i8,
    b"ugrave\x00" as *const u8 as *const i8,
    b"ucircumflex\x00" as *const u8 as *const i8,
    b"udieresis\x00" as *const u8 as *const i8,
    b"dagger\x00" as *const u8 as *const i8,
    b"degree\x00" as *const u8 as *const i8,
    b"cent\x00" as *const u8 as *const i8,
    b"sterling\x00" as *const u8 as *const i8,
    b"section\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"paragraph\x00" as *const u8 as *const i8,
    b"germandbls\x00" as *const u8 as *const i8,
    b"registered\x00" as *const u8 as *const i8,
    b"copyright\x00" as *const u8 as *const i8,
    b"trademark\x00" as *const u8 as *const i8,
    b"acute\x00" as *const u8 as *const i8,
    b"dieresis\x00" as *const u8 as *const i8,
    b"notequal\x00" as *const u8 as *const i8,
    b"AE\x00" as *const u8 as *const i8,
    b"Oslash\x00" as *const u8 as *const i8,
    b"infinity\x00" as *const u8 as *const i8,
    b"plusminus\x00" as *const u8 as *const i8,
    b"lessequal\x00" as *const u8 as *const i8,
    b"greaterequal\x00" as *const u8 as *const i8,
    b"yen\x00" as *const u8 as *const i8,
    b"mu\x00" as *const u8 as *const i8,
    b"partialdiff\x00" as *const u8 as *const i8,
    b"summation\x00" as *const u8 as *const i8,
    b"product\x00" as *const u8 as *const i8,
    b"pi\x00" as *const u8 as *const i8,
    b"integral\x00" as *const u8 as *const i8,
    b"ordfeminine\x00" as *const u8 as *const i8,
    b"ordmasculine\x00" as *const u8 as *const i8,
    b"Omega\x00" as *const u8 as *const i8,
    b"ae\x00" as *const u8 as *const i8,
    b"oslash\x00" as *const u8 as *const i8,
    b"questiondown\x00" as *const u8 as *const i8,
    b"exclamdown\x00" as *const u8 as *const i8,
    b"logicalnot\x00" as *const u8 as *const i8,
    b"radical\x00" as *const u8 as *const i8,
    b"florin\x00" as *const u8 as *const i8,
    b"approxequal\x00" as *const u8 as *const i8,
    b"Delta\x00" as *const u8 as *const i8,
    b"guillemotleft\x00" as *const u8 as *const i8,
    b"guillemotright\x00" as *const u8 as *const i8,
    b"ellipsis\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"Agrave\x00" as *const u8 as *const i8,
    b"Atilde\x00" as *const u8 as *const i8,
    b"Otilde\x00" as *const u8 as *const i8,
    b"OE\x00" as *const u8 as *const i8,
    b"oe\x00" as *const u8 as *const i8,
    b"endash\x00" as *const u8 as *const i8,
    b"emdash\x00" as *const u8 as *const i8,
    b"quotedblleft\x00" as *const u8 as *const i8,
    b"quotedblright\x00" as *const u8 as *const i8,
    b"quoteleft\x00" as *const u8 as *const i8,
    b"quoteright\x00" as *const u8 as *const i8,
    b"divide\x00" as *const u8 as *const i8,
    b"lozenge\x00" as *const u8 as *const i8,
    b"ydieresis\x00" as *const u8 as *const i8,
    b"Ydieresis\x00" as *const u8 as *const i8,
    b"fraction\x00" as *const u8 as *const i8,
    b"currency\x00" as *const u8 as *const i8,
    b"guilsinglleft\x00" as *const u8 as *const i8,
    b"guilsinglright\x00" as *const u8 as *const i8,
    b"fi\x00" as *const u8 as *const i8,
    b"fl\x00" as *const u8 as *const i8,
    b"daggerdbl\x00" as *const u8 as *const i8,
    b"periodcentered\x00" as *const u8 as *const i8,
    b"quotesinglbase\x00" as *const u8 as *const i8,
    b"quotedblbase\x00" as *const u8 as *const i8,
    b"perthousand\x00" as *const u8 as *const i8,
    b"Acircumflex\x00" as *const u8 as *const i8,
    b"Ecircumflex\x00" as *const u8 as *const i8,
    b"Aacute\x00" as *const u8 as *const i8,
    b"Edieresis\x00" as *const u8 as *const i8,
    b"Egrave\x00" as *const u8 as *const i8,
    b"Iacute\x00" as *const u8 as *const i8,
    b"Icircumflex\x00" as *const u8 as *const i8,
    b"Idieresis\x00" as *const u8 as *const i8,
    b"Igrave\x00" as *const u8 as *const i8,
    b"Oacute\x00" as *const u8 as *const i8,
    b"Ocircumflex\x00" as *const u8 as *const i8,
    b"apple\x00" as *const u8 as *const i8,
    b"Ograve\x00" as *const u8 as *const i8,
    b"Uacute\x00" as *const u8 as *const i8,
    b"Ucircumflex\x00" as *const u8 as *const i8,
    b"Ugrave\x00" as *const u8 as *const i8,
    b"dotlessi\x00" as *const u8 as *const i8,
    b"circumflex\x00" as *const u8 as *const i8,
    b"tilde\x00" as *const u8 as *const i8,
    b"macron\x00" as *const u8 as *const i8,
    b"breve\x00" as *const u8 as *const i8,
    b"dotaccent\x00" as *const u8 as *const i8,
    b"ring\x00" as *const u8 as *const i8,
    b"cedilla\x00" as *const u8 as *const i8,
    b"hungarumlaut\x00" as *const u8 as *const i8,
    b"ogonek\x00" as *const u8 as *const i8,
    b"caron\x00" as *const u8 as *const i8,
];
static mut MacExpertEncoding: [*const i8; 256] = [
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclamsmall\x00" as *const u8 as *const i8,
    b"Hungarumlautsmall\x00" as *const u8 as *const i8,
    b"centoldstyle\x00" as *const u8 as *const i8,
    b"dollaroldstyle\x00" as *const u8 as *const i8,
    b"dollarsuperior\x00" as *const u8 as *const i8,
    b"ampersandsmall\x00" as *const u8 as *const i8,
    b"Acutesmall\x00" as *const u8 as *const i8,
    b"parenleftsuperior\x00" as *const u8 as *const i8,
    b"parenrightsuperior\x00" as *const u8 as *const i8,
    b"twodotenleader\x00" as *const u8 as *const i8,
    b"onedotenleader\x00" as *const u8 as *const i8,
    b"comma\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"period\x00" as *const u8 as *const i8,
    b"fraction\x00" as *const u8 as *const i8,
    b"zerooldstyle\x00" as *const u8 as *const i8,
    b"oneoldstyle\x00" as *const u8 as *const i8,
    b"twooldstyle\x00" as *const u8 as *const i8,
    b"threeoldstyle\x00" as *const u8 as *const i8,
    b"fouroldstyle\x00" as *const u8 as *const i8,
    b"fiveoldstyle\x00" as *const u8 as *const i8,
    b"sixoldstyle\x00" as *const u8 as *const i8,
    b"sevenoldstyle\x00" as *const u8 as *const i8,
    b"eightoldstyle\x00" as *const u8 as *const i8,
    b"nineoldstyle\x00" as *const u8 as *const i8,
    b"colon\x00" as *const u8 as *const i8,
    b"semicolon\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"threequartersemdash\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"questionsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Ethsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"onequarter\x00" as *const u8 as *const i8,
    b"onehalf\x00" as *const u8 as *const i8,
    b"threequarters\x00" as *const u8 as *const i8,
    b"oneeighth\x00" as *const u8 as *const i8,
    b"threeeighths\x00" as *const u8 as *const i8,
    b"fiveeighths\x00" as *const u8 as *const i8,
    b"seveneighths\x00" as *const u8 as *const i8,
    b"onethird\x00" as *const u8 as *const i8,
    b"twothirds\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"ff\x00" as *const u8 as *const i8,
    b"fi\x00" as *const u8 as *const i8,
    b"fl\x00" as *const u8 as *const i8,
    b"ffi\x00" as *const u8 as *const i8,
    b"ffl\x00" as *const u8 as *const i8,
    b"parenleftinferior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"parenrightinferior\x00" as *const u8 as *const i8,
    b"Circumflexsmall\x00" as *const u8 as *const i8,
    b"hypheninferior\x00" as *const u8 as *const i8,
    b"Gravesmall\x00" as *const u8 as *const i8,
    b"Asmall\x00" as *const u8 as *const i8,
    b"Bsmall\x00" as *const u8 as *const i8,
    b"Csmall\x00" as *const u8 as *const i8,
    b"Dsmall\x00" as *const u8 as *const i8,
    b"Esmall\x00" as *const u8 as *const i8,
    b"Fsmall\x00" as *const u8 as *const i8,
    b"Gsmall\x00" as *const u8 as *const i8,
    b"Hsmall\x00" as *const u8 as *const i8,
    b"Ismall\x00" as *const u8 as *const i8,
    b"Jsmall\x00" as *const u8 as *const i8,
    b"Ksmall\x00" as *const u8 as *const i8,
    b"Lsmall\x00" as *const u8 as *const i8,
    b"Msmall\x00" as *const u8 as *const i8,
    b"Nsmall\x00" as *const u8 as *const i8,
    b"Osmall\x00" as *const u8 as *const i8,
    b"Psmall\x00" as *const u8 as *const i8,
    b"Qsmall\x00" as *const u8 as *const i8,
    b"Rsmall\x00" as *const u8 as *const i8,
    b"Ssmall\x00" as *const u8 as *const i8,
    b"Tsmall\x00" as *const u8 as *const i8,
    b"Usmall\x00" as *const u8 as *const i8,
    b"Vsmall\x00" as *const u8 as *const i8,
    b"Wsmall\x00" as *const u8 as *const i8,
    b"Xsmall\x00" as *const u8 as *const i8,
    b"Ysmall\x00" as *const u8 as *const i8,
    b"Zsmall\x00" as *const u8 as *const i8,
    b"colonmonetary\x00" as *const u8 as *const i8,
    b"onefitted\x00" as *const u8 as *const i8,
    b"rupiah\x00" as *const u8 as *const i8,
    b"Tildesmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"asuperior\x00" as *const u8 as *const i8,
    b"centsuperior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Aacutesmall\x00" as *const u8 as *const i8,
    b"Agravesmall\x00" as *const u8 as *const i8,
    b"Acircumflexsmall\x00" as *const u8 as *const i8,
    b"Adieresissmall\x00" as *const u8 as *const i8,
    b"Atildesmall\x00" as *const u8 as *const i8,
    b"Aringsmall\x00" as *const u8 as *const i8,
    b"Ccedillasmall\x00" as *const u8 as *const i8,
    b"Eacutesmall\x00" as *const u8 as *const i8,
    b"Egravesmall\x00" as *const u8 as *const i8,
    b"Ecircumflexsmall\x00" as *const u8 as *const i8,
    b"Edieresissmall\x00" as *const u8 as *const i8,
    b"Iacutesmall\x00" as *const u8 as *const i8,
    b"Igravesmall\x00" as *const u8 as *const i8,
    b"Icircumflexsmall\x00" as *const u8 as *const i8,
    b"Idieresissmall\x00" as *const u8 as *const i8,
    b"Ntildesmall\x00" as *const u8 as *const i8,
    b"Oacutesmall\x00" as *const u8 as *const i8,
    b"Ogravesmall\x00" as *const u8 as *const i8,
    b"Ocircumflexsmall\x00" as *const u8 as *const i8,
    b"Odieresissmall\x00" as *const u8 as *const i8,
    b"Otildesmall\x00" as *const u8 as *const i8,
    b"Uacutesmall\x00" as *const u8 as *const i8,
    b"Ugravesmall\x00" as *const u8 as *const i8,
    b"Ucircumflexsmall\x00" as *const u8 as *const i8,
    b"Udieresissmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"eightsuperior\x00" as *const u8 as *const i8,
    b"fourinferior\x00" as *const u8 as *const i8,
    b"threeinferior\x00" as *const u8 as *const i8,
    b"sixinferior\x00" as *const u8 as *const i8,
    b"eightinferior\x00" as *const u8 as *const i8,
    b"seveninferior\x00" as *const u8 as *const i8,
    b"Scaronsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"centinferior\x00" as *const u8 as *const i8,
    b"twoinferior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Dieresissmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Caronsmall\x00" as *const u8 as *const i8,
    b"osuperior\x00" as *const u8 as *const i8,
    b"fiveinferior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"commainferior\x00" as *const u8 as *const i8,
    b"periodinferior\x00" as *const u8 as *const i8,
    b"Yacutesmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"dollarinferior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Thornsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"nineinferior\x00" as *const u8 as *const i8,
    b"zeroinferior\x00" as *const u8 as *const i8,
    b"Zcaronsmall\x00" as *const u8 as *const i8,
    b"AEsmall\x00" as *const u8 as *const i8,
    b"Oslashsmall\x00" as *const u8 as *const i8,
    b"questiondownsmall\x00" as *const u8 as *const i8,
    b"oneinferior\x00" as *const u8 as *const i8,
    b"Lslashsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Cedillasmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"OEsmall\x00" as *const u8 as *const i8,
    b"figuredash\x00" as *const u8 as *const i8,
    b"hyphensuperior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"exclamdownsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Ydieresissmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"onesuperior\x00" as *const u8 as *const i8,
    b"twosuperior\x00" as *const u8 as *const i8,
    b"threesuperior\x00" as *const u8 as *const i8,
    b"foursuperior\x00" as *const u8 as *const i8,
    b"fivesuperior\x00" as *const u8 as *const i8,
    b"sixsuperior\x00" as *const u8 as *const i8,
    b"sevensuperior\x00" as *const u8 as *const i8,
    b"ninesuperior\x00" as *const u8 as *const i8,
    b"zerosuperior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"esuperior\x00" as *const u8 as *const i8,
    b"rsuperior\x00" as *const u8 as *const i8,
    b"tsuperior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"isuperior\x00" as *const u8 as *const i8,
    b"ssuperior\x00" as *const u8 as *const i8,
    b"dsuperior\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"lsuperior\x00" as *const u8 as *const i8,
    b"Ogoneksmall\x00" as *const u8 as *const i8,
    b"Brevesmall\x00" as *const u8 as *const i8,
    b"Macronsmall\x00" as *const u8 as *const i8,
    b"bsuperior\x00" as *const u8 as *const i8,
    b"nsuperior\x00" as *const u8 as *const i8,
    b"msuperior\x00" as *const u8 as *const i8,
    b"commasuperior\x00" as *const u8 as *const i8,
    b"periodsuperior\x00" as *const u8 as *const i8,
    b"Dotaccentsmall\x00" as *const u8 as *const i8,
    b"Ringsmall\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
];
static mut WinAnsiEncoding: [*const i8; 256] = [
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclam\x00" as *const u8 as *const i8,
    b"quotedbl\x00" as *const u8 as *const i8,
    b"numbersign\x00" as *const u8 as *const i8,
    b"dollar\x00" as *const u8 as *const i8,
    b"percent\x00" as *const u8 as *const i8,
    b"ampersand\x00" as *const u8 as *const i8,
    b"quotesingle\x00" as *const u8 as *const i8,
    b"parenleft\x00" as *const u8 as *const i8,
    b"parenright\x00" as *const u8 as *const i8,
    b"asterisk\x00" as *const u8 as *const i8,
    b"plus\x00" as *const u8 as *const i8,
    b"comma\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"period\x00" as *const u8 as *const i8,
    b"slash\x00" as *const u8 as *const i8,
    b"zero\x00" as *const u8 as *const i8,
    b"one\x00" as *const u8 as *const i8,
    b"two\x00" as *const u8 as *const i8,
    b"three\x00" as *const u8 as *const i8,
    b"four\x00" as *const u8 as *const i8,
    b"five\x00" as *const u8 as *const i8,
    b"six\x00" as *const u8 as *const i8,
    b"seven\x00" as *const u8 as *const i8,
    b"eight\x00" as *const u8 as *const i8,
    b"nine\x00" as *const u8 as *const i8,
    b"colon\x00" as *const u8 as *const i8,
    b"semicolon\x00" as *const u8 as *const i8,
    b"less\x00" as *const u8 as *const i8,
    b"equal\x00" as *const u8 as *const i8,
    b"greater\x00" as *const u8 as *const i8,
    b"question\x00" as *const u8 as *const i8,
    b"at\x00" as *const u8 as *const i8,
    b"A\x00" as *const u8 as *const i8,
    b"B\x00" as *const u8 as *const i8,
    b"C\x00" as *const u8 as *const i8,
    b"D\x00" as *const u8 as *const i8,
    b"E\x00" as *const u8 as *const i8,
    b"F\x00" as *const u8 as *const i8,
    b"G\x00" as *const u8 as *const i8,
    b"H\x00" as *const u8 as *const i8,
    b"I\x00" as *const u8 as *const i8,
    b"J\x00" as *const u8 as *const i8,
    b"K\x00" as *const u8 as *const i8,
    b"L\x00" as *const u8 as *const i8,
    b"M\x00" as *const u8 as *const i8,
    b"N\x00" as *const u8 as *const i8,
    b"O\x00" as *const u8 as *const i8,
    b"P\x00" as *const u8 as *const i8,
    b"Q\x00" as *const u8 as *const i8,
    b"R\x00" as *const u8 as *const i8,
    b"S\x00" as *const u8 as *const i8,
    b"T\x00" as *const u8 as *const i8,
    b"U\x00" as *const u8 as *const i8,
    b"V\x00" as *const u8 as *const i8,
    b"W\x00" as *const u8 as *const i8,
    b"X\x00" as *const u8 as *const i8,
    b"Y\x00" as *const u8 as *const i8,
    b"Z\x00" as *const u8 as *const i8,
    b"bracketleft\x00" as *const u8 as *const i8,
    b"backslash\x00" as *const u8 as *const i8,
    b"bracketright\x00" as *const u8 as *const i8,
    b"asciicircum\x00" as *const u8 as *const i8,
    b"underscore\x00" as *const u8 as *const i8,
    b"grave\x00" as *const u8 as *const i8,
    b"a\x00" as *const u8 as *const i8,
    b"b\x00" as *const u8 as *const i8,
    b"c\x00" as *const u8 as *const i8,
    b"d\x00" as *const u8 as *const i8,
    b"e\x00" as *const u8 as *const i8,
    b"f\x00" as *const u8 as *const i8,
    b"g\x00" as *const u8 as *const i8,
    b"h\x00" as *const u8 as *const i8,
    b"i\x00" as *const u8 as *const i8,
    b"j\x00" as *const u8 as *const i8,
    b"k\x00" as *const u8 as *const i8,
    b"l\x00" as *const u8 as *const i8,
    b"m\x00" as *const u8 as *const i8,
    b"n\x00" as *const u8 as *const i8,
    b"o\x00" as *const u8 as *const i8,
    b"p\x00" as *const u8 as *const i8,
    b"q\x00" as *const u8 as *const i8,
    b"r\x00" as *const u8 as *const i8,
    b"s\x00" as *const u8 as *const i8,
    b"t\x00" as *const u8 as *const i8,
    b"u\x00" as *const u8 as *const i8,
    b"v\x00" as *const u8 as *const i8,
    b"w\x00" as *const u8 as *const i8,
    b"x\x00" as *const u8 as *const i8,
    b"y\x00" as *const u8 as *const i8,
    b"z\x00" as *const u8 as *const i8,
    b"braceleft\x00" as *const u8 as *const i8,
    b"bar\x00" as *const u8 as *const i8,
    b"braceright\x00" as *const u8 as *const i8,
    b"asciitilde\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"Euro\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"quotesinglbase\x00" as *const u8 as *const i8,
    b"florin\x00" as *const u8 as *const i8,
    b"quotedblbase\x00" as *const u8 as *const i8,
    b"ellipsis\x00" as *const u8 as *const i8,
    b"dagger\x00" as *const u8 as *const i8,
    b"daggerdbl\x00" as *const u8 as *const i8,
    b"circumflex\x00" as *const u8 as *const i8,
    b"perthousand\x00" as *const u8 as *const i8,
    b"Scaron\x00" as *const u8 as *const i8,
    b"guilsinglleft\x00" as *const u8 as *const i8,
    b"OE\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"Zcaron\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"quoteleft\x00" as *const u8 as *const i8,
    b"quoteright\x00" as *const u8 as *const i8,
    b"quotedblleft\x00" as *const u8 as *const i8,
    b"quotedblright\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"endash\x00" as *const u8 as *const i8,
    b"emdash\x00" as *const u8 as *const i8,
    b"tilde\x00" as *const u8 as *const i8,
    b"trademark\x00" as *const u8 as *const i8,
    b"scaron\x00" as *const u8 as *const i8,
    b"guilsinglright\x00" as *const u8 as *const i8,
    b"oe\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"zcaron\x00" as *const u8 as *const i8,
    b"Ydieresis\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclamdown\x00" as *const u8 as *const i8,
    b"cent\x00" as *const u8 as *const i8,
    b"sterling\x00" as *const u8 as *const i8,
    b"currency\x00" as *const u8 as *const i8,
    b"yen\x00" as *const u8 as *const i8,
    b"brokenbar\x00" as *const u8 as *const i8,
    b"section\x00" as *const u8 as *const i8,
    b"dieresis\x00" as *const u8 as *const i8,
    b"copyright\x00" as *const u8 as *const i8,
    b"ordfeminine\x00" as *const u8 as *const i8,
    b"guillemotleft\x00" as *const u8 as *const i8,
    b"logicalnot\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"registered\x00" as *const u8 as *const i8,
    b"macron\x00" as *const u8 as *const i8,
    b"degree\x00" as *const u8 as *const i8,
    b"plusminus\x00" as *const u8 as *const i8,
    b"twosuperior\x00" as *const u8 as *const i8,
    b"threesuperior\x00" as *const u8 as *const i8,
    b"acute\x00" as *const u8 as *const i8,
    b"mu\x00" as *const u8 as *const i8,
    b"paragraph\x00" as *const u8 as *const i8,
    b"periodcentered\x00" as *const u8 as *const i8,
    b"cedilla\x00" as *const u8 as *const i8,
    b"onesuperior\x00" as *const u8 as *const i8,
    b"ordmasculine\x00" as *const u8 as *const i8,
    b"guillemotright\x00" as *const u8 as *const i8,
    b"onequarter\x00" as *const u8 as *const i8,
    b"onehalf\x00" as *const u8 as *const i8,
    b"threequarters\x00" as *const u8 as *const i8,
    b"questiondown\x00" as *const u8 as *const i8,
    b"Agrave\x00" as *const u8 as *const i8,
    b"Aacute\x00" as *const u8 as *const i8,
    b"Acircumflex\x00" as *const u8 as *const i8,
    b"Atilde\x00" as *const u8 as *const i8,
    b"Adieresis\x00" as *const u8 as *const i8,
    b"Aring\x00" as *const u8 as *const i8,
    b"AE\x00" as *const u8 as *const i8,
    b"Ccedilla\x00" as *const u8 as *const i8,
    b"Egrave\x00" as *const u8 as *const i8,
    b"Eacute\x00" as *const u8 as *const i8,
    b"Ecircumflex\x00" as *const u8 as *const i8,
    b"Edieresis\x00" as *const u8 as *const i8,
    b"Igrave\x00" as *const u8 as *const i8,
    b"Iacute\x00" as *const u8 as *const i8,
    b"Icircumflex\x00" as *const u8 as *const i8,
    b"Idieresis\x00" as *const u8 as *const i8,
    b"Eth\x00" as *const u8 as *const i8,
    b"Ntilde\x00" as *const u8 as *const i8,
    b"Ograve\x00" as *const u8 as *const i8,
    b"Oacute\x00" as *const u8 as *const i8,
    b"Ocircumflex\x00" as *const u8 as *const i8,
    b"Otilde\x00" as *const u8 as *const i8,
    b"Odieresis\x00" as *const u8 as *const i8,
    b"multiply\x00" as *const u8 as *const i8,
    b"Oslash\x00" as *const u8 as *const i8,
    b"Ugrave\x00" as *const u8 as *const i8,
    b"Uacute\x00" as *const u8 as *const i8,
    b"Ucircumflex\x00" as *const u8 as *const i8,
    b"Udieresis\x00" as *const u8 as *const i8,
    b"Yacute\x00" as *const u8 as *const i8,
    b"Thorn\x00" as *const u8 as *const i8,
    b"germandbls\x00" as *const u8 as *const i8,
    b"agrave\x00" as *const u8 as *const i8,
    b"aacute\x00" as *const u8 as *const i8,
    b"acircumflex\x00" as *const u8 as *const i8,
    b"atilde\x00" as *const u8 as *const i8,
    b"adieresis\x00" as *const u8 as *const i8,
    b"aring\x00" as *const u8 as *const i8,
    b"ae\x00" as *const u8 as *const i8,
    b"ccedilla\x00" as *const u8 as *const i8,
    b"egrave\x00" as *const u8 as *const i8,
    b"eacute\x00" as *const u8 as *const i8,
    b"ecircumflex\x00" as *const u8 as *const i8,
    b"edieresis\x00" as *const u8 as *const i8,
    b"igrave\x00" as *const u8 as *const i8,
    b"iacute\x00" as *const u8 as *const i8,
    b"icircumflex\x00" as *const u8 as *const i8,
    b"idieresis\x00" as *const u8 as *const i8,
    b"eth\x00" as *const u8 as *const i8,
    b"ntilde\x00" as *const u8 as *const i8,
    b"ograve\x00" as *const u8 as *const i8,
    b"oacute\x00" as *const u8 as *const i8,
    b"ocircumflex\x00" as *const u8 as *const i8,
    b"otilde\x00" as *const u8 as *const i8,
    b"odieresis\x00" as *const u8 as *const i8,
    b"divide\x00" as *const u8 as *const i8,
    b"oslash\x00" as *const u8 as *const i8,
    b"ugrave\x00" as *const u8 as *const i8,
    b"uacute\x00" as *const u8 as *const i8,
    b"ucircumflex\x00" as *const u8 as *const i8,
    b"udieresis\x00" as *const u8 as *const i8,
    b"yacute\x00" as *const u8 as *const i8,
    b"thorn\x00" as *const u8 as *const i8,
    b"ydieresis\x00" as *const u8 as *const i8,
];
