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
    non_camel_case_types,
    non_snake_case,
)]

use euclid::point2;

use std::ffi::{CStr, CString};
use std::io::Read;
use std::ptr;

use crate::DisplayExt;
use crate::TTInputFormat;
use crate::{spc_warn, warn};

use super::util::{spc_util_read_blahblah, spc_util_read_dimtrns, spc_util_read_pdfcolor};
use super::{
    spc_begin_annot, spc_clear_objects, spc_end_annot, spc_flush_object, spc_lookup_object,
    spc_push_object, spc_resume_annot, spc_suspend_annot,
};
use crate::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode};
use crate::dpx_dpxutil::{
    ht_append_table, ht_clear_table, ht_init_table, ht_lookup_table, ParseCIdent,
};
use crate::dpx_dvipdfmx::is_xdv;
use crate::dpx_fontmap::{
    is_pdfm_mapline, pdf_append_fontmap_record, pdf_clear_fontmap_record, pdf_init_fontmap_record,
    pdf_insert_fontmap_record, pdf_load_fontmap_file, pdf_read_fontmap_line,
    pdf_remove_fontmap_record,
};
use crate::dpx_mem::new;
use crate::dpx_mfileio::work_buffer_u8 as WORK_BUFFER;
use crate::dpx_pdfcolor::{pdf_color_get_current, pdf_color_pop, pdf_color_push, pdf_color_set};
use crate::dpx_pdfdev::pdf_sprint_matrix;
use crate::dpx_pdfdev::{
    pdf_dev_get_coord, pdf_dev_pop_coord, pdf_dev_push_coord, pdf_dev_reset_color,
};
use crate::dpx_pdfdev::{pdf_dev_put_image, transform_info, transform_info_clear, Rect, TMatrix};
use crate::dpx_pdfdoc::{
    pdf_doc_add_annot, pdf_doc_add_bead, pdf_doc_add_names, pdf_doc_add_page_content,
    pdf_doc_begin_article, pdf_doc_begin_grabbing, pdf_doc_bookmarks_add, pdf_doc_bookmarks_depth,
    pdf_doc_bookmarks_down, pdf_doc_bookmarks_up, pdf_doc_current_page_number,
    pdf_doc_end_grabbing, pdf_doc_get_dictionary, pdf_doc_set_bgcolor, pdf_doc_set_bop_content,
    pdf_doc_set_eop_content,
};
use crate::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_grestore, pdf_dev_gsave, pdf_dev_transform};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_link_obj, pdf_name, pdf_obj, pdf_obj_typeof, pdf_release_obj, pdf_remove_dict,
    pdf_set_string, pdf_stream, pdf_string_length, pdf_string_value, IntoObj, PdfObjType,
    STREAM_COMPRESS,
};
use crate::dpx_pdfparse::{ParseIdent, ParsePdfObj, SkipWhite};
use crate::dpx_pdfximage::{pdf_ximage_findresource, pdf_ximage_get_reference};
use crate::dpx_unicode::{
    UC_UTF16BE_encode_char, UC_UTF16BE_is_valid_string, UC_UTF8_decode_char,
    UC_UTF8_is_valid_string, UC_is_valid,
};
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{free, memcmp, strlen, strstr};

pub type __ssize_t = i64;
pub type size_t = u64;

use super::{spc_arg, spc_env};

use super::SpcHandler;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct spc_pdf_ {
    pub annot_dict: *mut pdf_obj,
    pub lowest_level: i32,
    pub resourcemap: *mut ht_table,
    pub cd: tounicode,
    /* quasi-hack to get the primary input */
    /* For to-UTF16-BE conversion :( */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tounicode {
    pub cmap_id: i32,
    pub unescape_backslash: i32,
    pub taintkeys: *mut pdf_obj,
    /* An array of PDF names. */
}

use crate::dpx_dpxutil::ht_table;

use crate::dpx_fontmap::fontmap_rec;

use crate::dpx_pdfximage::load_options;

/* PLEASE REMOVE THIS */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct resource_map {
    pub type_0: i32,
    pub res_id: i32,
}
use crate::dpx_cmap::CMap;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut _PDF_STAT: spc_pdf_ = spc_pdf_ {
    annot_dict: ptr::null_mut(),
    lowest_level: 255i32,
    resourcemap: ptr::null_mut(),
    cd: tounicode {
        cmap_id: -1i32,
        unescape_backslash: 0i32,
        taintkeys: ptr::null_mut(),
    },
};
/* PLEASE REMOVE THIS */
unsafe fn hval_free(vp: *mut libc::c_void) {
    free(vp); /* unused */
}
unsafe fn addresource(sd: *mut spc_pdf_, ident: *const i8, res_id: i32) -> i32 {
    if ident.is_null() || res_id < 0i32 {
        return -1i32;
    }
    let r = new((1_u64).wrapping_mul(::std::mem::size_of::<resource_map>() as u64) as u32)
        as *mut resource_map;
    (*r).type_0 = 0i32;
    (*r).res_id = res_id;
    ht_append_table(
        (*sd).resourcemap,
        ident as *const libc::c_void,
        strlen(ident) as i32,
        r as *mut libc::c_void,
    );
    spc_push_object(ident, pdf_ximage_get_reference(res_id));
    0i32
}
unsafe fn findresource(sd: *mut spc_pdf_, ident: Option<&CString>) -> i32 {
    if let Some(ident) = ident {
        let r = ht_lookup_table(
            (*sd).resourcemap,
            ident.as_ptr() as *const libc::c_void,
            ident.to_bytes().len() as i32,
        ) as *mut resource_map;
        if !r.is_null() {
            (*r).res_id
        } else {
            -1
        }
    } else {
        -1
    }
}
unsafe fn spc_handler_pdfm__init(dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_pdf_ = dp as *mut spc_pdf_;
    /* The folllowing dictionary entry keys are considered as keys for
     * text strings. Be sure that string object is NOT always a text string.
     */
    const DEFAULT_TAINTKEYS: [&str; 11] = [
        "Title", "Author", "Subject", "Keywords", "Creator", "Producer", "Contents", "Subj", "TU",
        "T", "TM",
    ];
    (*sd).annot_dict = ptr::null_mut();
    (*sd).lowest_level = 255i32;
    (*sd).resourcemap =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        (*sd).resourcemap,
        Some(hval_free as unsafe fn(_: *mut libc::c_void) -> ()),
    );
    let array: Vec<*mut pdf_obj> = DEFAULT_TAINTKEYS
        .iter()
        .map(|&key| key.into_obj())
        .collect();
    (*sd).cd.taintkeys = array.into_obj();
    0i32
}
unsafe fn spc_handler_pdfm__clean(dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_pdf_ = dp as *mut spc_pdf_;
    if !(*sd).annot_dict.is_null() {
        warn!("Unbalanced bann and eann found.");
        pdf_release_obj((*sd).annot_dict);
    }
    (*sd).lowest_level = 255i32;
    (*sd).annot_dict = ptr::null_mut();
    if !(*sd).resourcemap.is_null() {
        ht_clear_table((*sd).resourcemap);
        free((*sd).resourcemap as *mut libc::c_void);
    }
    (*sd).resourcemap = ptr::null_mut();
    pdf_release_obj((*sd).cd.taintkeys);
    (*sd).cd.taintkeys = ptr::null_mut();
    0i32
}

pub unsafe fn spc_pdfm_at_begin_document() -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    spc_handler_pdfm__init(sd as *mut libc::c_void)
}

pub unsafe fn spc_pdfm_at_end_document() -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    spc_handler_pdfm__clean(sd as *mut libc::c_void)
}
/* Dvipdfm specials */
unsafe fn spc_handler_pdfm_bop(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if !(*args).cur.is_empty() {
        pdf_doc_set_bop_content((*args).cur.as_ptr() as *const i8, (*args).cur.len() as u32);
    }
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_eop(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if !(*args).cur.is_empty() {
        pdf_doc_set_eop_content((*args).cur.as_ptr() as *const i8, (*args).cur.len() as u32);
    }
    (*args).cur = &[];
    0i32
}
/* Why should we have this kind of things? */
unsafe fn safeputresdent(kp: &pdf_name, vp: *mut pdf_obj, dp: *mut libc::c_void) -> i32 {
    assert!(!vp.is_null() && !dp.is_null());
    let key = kp.to_bytes();
    let dict_ref = (*(dp as *mut pdf_obj)).as_dict_mut();
    if dict_ref.has(key) {
        warn!(
            "Object \"{}\" already defined in dict! (ignored)",
            key.display()
        );
    } else {
        dict_ref.set(key, pdf_link_obj(vp));
    }
    0i32
}
unsafe fn safeputresdict(kp: &pdf_name, vp: *mut pdf_obj, dp: *mut libc::c_void) -> i32 {
    assert!(!vp.is_null() && !dp.is_null());
    let key = kp.to_bytes();
    let dict_ref = (*(dp as *mut pdf_obj)).as_dict_mut();
    let dict = dict_ref.get_mut(key);
    if (*vp).is_indirect() {
        dict_ref.set(key, pdf_link_obj(vp));
    } else if (*vp).is_dict() {
        if let Some(dict) = dict {
            (*vp).as_dict_mut().foreach(
                Some(
                    safeputresdent
                        as unsafe fn(_: &pdf_name, _: *mut pdf_obj, _: *mut libc::c_void) -> i32,
                ),
                dict as *mut pdf_obj as *mut libc::c_void,
            );
        } else {
            dict_ref.set(key, pdf_link_obj(vp));
        }
    } else {
        warn!(
            "Invalid type (not DICT) for page/form resource dict entry: key=\"{}\"",
            key.display(),
        );
        return -1i32;
    }
    0i32
}
/* Think what happens if you do
 *
 *  pdf:put @resources << /Font << >> >>
 *
 */
unsafe fn spc_handler_pdfm_put(spe: *mut spc_env, ap: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    (*ap).cur.skip_white();
    let ident = (*ap).cur.parse_opt_ident();
    if ident.is_none() {
        spc_warn!(spe, "Missing object identifier.");
        return -1i32;
    }
    let ident = ident.unwrap();
    let obj1 = spc_lookup_object(ident.as_ptr()); /* put obj2 into obj1 */
    if obj1.is_null() {
        spc_warn!(spe, "Specified object not exist: {}", ident.display(),);
        return -1i32;
    }
    (*ap).cur.skip_white();
    let obj2 = (*ap).cur.parse_pdf_object(ptr::null_mut());
    if obj2.is_none() {
        spc_warn!(
            spe,
            "Missing (an) object(s) to put into \"{}\"!",
            ident.display(),
        );
        return -1i32;
    }
    let obj2 = obj2.unwrap();
    match pdf_obj_typeof(obj1) {
        PdfObjType::DICT => {
            if !(*obj2).is_dict() {
                spc_warn!(
                    spe,
                    "Inconsistent object type for \"put\" (expecting DICT): {}",
                    ident.display(),
                );
                error = -1i32
            } else if ident.to_bytes() == b"resources" {
                error = (*obj2).as_dict_mut().foreach(
                    Some(
                        safeputresdict
                            as unsafe fn(
                                _: &pdf_name,
                                _: *mut pdf_obj,
                                _: *mut libc::c_void,
                            ) -> i32,
                    ),
                    obj1 as *mut libc::c_void,
                )
            } else {
                (*obj1).as_dict_mut().merge((*obj2).as_dict());
            }
        }
        PdfObjType::STREAM => {
            if (*obj2).is_dict() {
                (*obj1)
                    .as_stream_mut()
                    .get_dict_mut()
                    .merge((*obj2).as_dict());
            } else if (*obj2).is_stream() {
                spc_warn!(
                    spe,
                    "\"put\" operation not supported for STREAM <- STREAM: {}",
                    ident.display(),
                );
                error = -1i32
            } else {
                spc_warn!(
                    spe,
                    "Invalid type: expecting a DICT or STREAM: {}",
                    ident.display(),
                );
                error = -1i32
            }
        }
        PdfObjType::ARRAY => {
            /* dvipdfm */
            (*obj1).as_array_mut().push(pdf_link_obj(obj2));
            while !(*ap).cur.is_empty() {
                if let Some(obj3) = (*ap).cur.parse_pdf_object(ptr::null_mut()) {
                    (*obj1).as_array_mut().push(obj3);
                    (*ap).cur.skip_white();
                } else {
                    break;
                }
            }
        }
        _ => {
            spc_warn!(
                spe,
                "Can\'t \"put\" object into non-DICT/STREAM/ARRAY type object: {}",
                ident.display(),
            );
            error = -1i32
        }
    }
    pdf_release_obj(obj2);
    error
}
/* For pdf:tounicode support
 * This feature is provided for convenience. TeX can't do
 * input encoding conversion.
 */
unsafe fn reencodestring(cmap: *mut CMap, instring: *mut pdf_obj) -> i32 {
    let mut wbuf: [u8; 4096] = [0; 4096];
    if cmap.is_null() || instring.is_null() {
        return 0i32;
    }
    let mut inbufleft = pdf_string_length(&*instring) as size_t;
    let mut inbufcur = pdf_string_value(&*instring) as *const u8;
    wbuf[0] = 0xfe_u8;
    wbuf[1] = 0xff_u8;
    let mut obufcur = wbuf.as_mut_ptr().offset(2);
    let mut obufleft = (4096i32 - 2i32) as size_t;
    CMap_decode(
        cmap,
        &mut inbufcur,
        &mut inbufleft,
        &mut obufcur,
        &mut obufleft,
    );
    if inbufleft > 0i32 as u64 {
        return -1i32;
    }
    pdf_set_string(
        &mut *instring,
        wbuf.as_mut_ptr(),
        (4096i32 as u64).wrapping_sub(obufleft),
    );
    0i32
}
unsafe fn maybe_reencode_utf8(instring: *mut pdf_obj) -> i32 {
    let mut non_ascii: i32 = 0i32;
    let mut cp: *const u8;
    let mut wbuf: [u8; 4096] = [0; 4096];
    if instring.is_null() {
        return 0i32;
    }
    let inlen = pdf_string_length(&*instring) as i32;
    let inbuf = pdf_string_value(&*instring) as *mut u8;
    /* check if the input string is strictly ASCII */
    cp = inbuf; /* no need to reencode ASCII strings */
    while cp < inbuf.offset(inlen as isize) as *const u8 {
        if *cp as i32 > 127i32 {
            non_ascii = 1i32
        }
        cp = cp.offset(1)
    }
    if non_ascii == 0i32 {
        return 0i32;
    }
    /* Check if the input string is valid UTF8 string
     * This routine may be called against non-text strings.
     * We need to re-encode string only when string is a text string
     * endcoded in UTF8.
     */
    if !UC_UTF8_is_valid_string(inbuf, inbuf.offset(inlen as isize)) {
        return 0i32;
    } else {
        if *inbuf.offset(0) as i32 == 0xfei32
            && *inbuf.offset(1) as i32 == 0xffi32
            && UC_UTF16BE_is_valid_string(inbuf.offset(2), inbuf.offset(inlen as isize)) as i32 != 0
        {
            return 0i32;
        }
    } /* no need to reencode UTF16BE with BOM */
    cp = inbuf; /* out of valid Unicode range, give up (redundant) */
    let mut op = wbuf.as_mut_ptr();
    let fresh0 = op;
    op = op.offset(1);
    *fresh0 = 0xfe_u8;
    let fresh1 = op;
    op = op.offset(1);
    *fresh1 = 0xff_u8;
    while cp < inbuf.offset(inlen as isize) as *const u8 {
        let usv = UC_UTF8_decode_char(&mut cp, inbuf.offset(inlen as isize));
        if !UC_is_valid(usv) {
            return -1i32;
        }
        let len = UC_UTF16BE_encode_char(usv, &mut op, wbuf.as_mut_ptr().offset(4096)) as i32;
        if len == 0i32 {
            return -1i32;
        }
    }
    pdf_set_string(
        &mut *instring,
        wbuf.as_mut_ptr(),
        op.wrapping_offset_from(wbuf.as_mut_ptr()) as i64 as size_t,
    );
    0i32
}
/* The purpose of this routine is to check if given string object is
 * surely an object for *text* strings. It does not do a complete check
 * but does a quick check. Please add entries for taintkeys if you have found
 * additional dictionary entries which is considered as a text string.
 */
unsafe fn needreencode(kp: &pdf_name, vp: *mut pdf_obj, cd: *mut tounicode) -> i32 {
    let mut r: i32 = 0i32;
    assert!(!cd.is_null() && !(*cd).taintkeys.is_null());
    assert!((*vp).is_string());
    for i in 0..(*(*cd).taintkeys).as_array().len() {
        let tk = (*(*cd).taintkeys).as_array()[i];
        assert!((*tk).is_name());
        if kp.to_bytes() == (*tk).as_name().to_bytes() {
            r = 1i32;
            break;
        }
    }
    if r != 0 {
        /* Check UTF-16BE BOM. */
        if pdf_string_length(&*vp) >= 2_u32
            && memcmp(
                pdf_string_value(&*vp),
                b"\xfe\xff\x00" as *const u8 as *const i8 as *const libc::c_void,
                2,
            ) == 0
        {
            r = 0i32
        }
    } /* continue */
    r
}
unsafe fn modstrings(kp: &pdf_name, vp: *mut pdf_obj, dp: *mut libc::c_void) -> i32 {
    let mut r: i32 = 0i32;
    let cd: *mut tounicode = dp as *mut tounicode;
    match pdf_obj_typeof(vp) {
        PdfObjType::STRING => {
            if !cd.is_null() && (*cd).cmap_id >= 0i32 && !(*cd).taintkeys.is_null() {
                let cmap: *mut CMap = CMap_cache_get((*cd).cmap_id);
                if needreencode(kp, vp, cd) != 0 {
                    r = reencodestring(cmap, vp)
                }
            } else if is_xdv != 0 && !cd.is_null() && !(*cd).taintkeys.is_null() {
                /* Please fix this... PDF string object is not always a text string.
                 * needreencode() is assumed to do a simple check if given string
                 * object is actually a text string.
                 */
                if needreencode(kp, vp, cd) != 0 {
                    r = maybe_reencode_utf8(vp)
                }
            }
            if r < 0i32 {
                /* error occured... */
                warn!("Failed to convert input string to UTF16...");
            }
        }
        PdfObjType::DICT => {
            r = (*vp).as_dict_mut().foreach(
                Some(
                    modstrings
                        as unsafe fn(_: &pdf_name, _: *mut pdf_obj, _: *mut libc::c_void) -> i32,
                ),
                dp,
            )
        }
        PdfObjType::STREAM => {
            r = (*vp).as_stream_mut().get_dict_mut().foreach(
                Some(
                    modstrings
                        as unsafe fn(_: &pdf_name, _: *mut pdf_obj, _: *mut libc::c_void) -> i32,
                ),
                dp,
            )
        }
        _ => {}
    }
    r
}

pub trait ParsePdfDictU {
    fn parse_pdf_dict_with_tounicode(&mut self, cd: *mut tounicode) -> Option<*mut pdf_obj>;
}

impl ParsePdfDictU for &[u8] {
    fn parse_pdf_dict_with_tounicode(&mut self, cd: *mut tounicode) -> Option<*mut pdf_obj> {
        /* disable this test for XDV files, as we do UTF8 reencoding with no cmap */
        if unsafe { is_xdv == 0 && (*cd).cmap_id < 0i32 } {
            return self.parse_pdf_dict(ptr::null_mut());
        }
        /* :( */
        let dict = if unsafe { !cd.is_null() && (*cd).unescape_backslash != 0 } {
            self.parse_pdf_tainted_dict()
        } else {
            self.parse_pdf_dict(ptr::null_mut())
        };
        if let Some(d) = dict {
            unsafe {
                (*d).as_dict_mut().foreach(
                    Some(
                        modstrings
                            as unsafe fn(
                                _: &pdf_name,
                                _: *mut pdf_obj,
                                _: *mut libc::c_void,
                            ) -> i32,
                    ),
                    cd as *mut libc::c_void,
                );
            }
        }
        dict
    }
}

unsafe fn spc_handler_pdfm_annot(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut rect = Rect::zero();
    let mut ident = None;
    let mut ti = transform_info::new();
    (*args).cur.skip_white();
    if (*args).cur[0] == b'@' {
        ident = (*args).cur.parse_opt_ident();
        (*args).cur.skip_white();
    }
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        return -1i32;
    }
    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return -1i32;
    }
    let annot_dict = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd);
    if annot_dict.is_none() {
        spc_warn!(spe, "Could not find dictionary object.");
        return -1i32;
    }
    let annot_dict = annot_dict.unwrap();
    if !(*annot_dict).is_dict() {
        spc_warn!(spe, "Invalid type: not dictionary object.");
        pdf_release_obj(annot_dict);
        return -1i32;
    }
    let mut cp = point2((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut cp, None);
    if ti.flags & 1i32 << 0i32 != 0 {
        rect = ti.bbox.translate(cp.to_vector());
    } else {
        rect.min.x = cp.x;
        rect.min.y = cp.y - (*spe).mag * ti.depth;
        rect.max.x = cp.x + (*spe).mag * ti.width;
        rect.max.y = cp.y + (*spe).mag * ti.height
    }
    /* Order is important... */
    if let Some(i) = ident.as_ref() {
        spc_push_object(i.as_ptr(), pdf_link_obj(annot_dict));
    }
    /* Add this reference. */
    pdf_doc_add_annot(
        pdf_doc_current_page_number() as u32,
        &mut rect,
        annot_dict,
        1i32,
    );
    if let Some(i) = ident {
        spc_flush_object(i.as_ptr());
    }
    pdf_release_obj(annot_dict);
    0i32
}
/* NOTE: This can't have ident. See "Dvipdfm User's Manual". */
unsafe fn spc_handler_pdfm_bann(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if !(*sd).annot_dict.is_null() {
        spc_warn!(spe, "Can\'t begin an annotation when one is pending.");
        return -1i32;
    }
    (*args).cur.skip_white();
    if let Some(annot_dict) = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd) {
        (*sd).annot_dict = annot_dict;
        if !(*(*sd).annot_dict).is_dict() {
            spc_warn!(spe, "Invalid type: not a dictionary object.");
            pdf_release_obj((*sd).annot_dict);
            (*sd).annot_dict = ptr::null_mut();
            return -1i32;
        }
    } else {
        (*sd).annot_dict = ptr::null_mut();
        spc_warn!(spe, "Ignoring annotation with invalid dictionary.");
        return -1i32;
    }
    spc_begin_annot(spe, (*sd).annot_dict)
}
unsafe fn spc_handler_pdfm_eann(spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if (*sd).annot_dict.is_null() {
        spc_warn!(spe, "Tried to end an annotation without starting one!");
        return -1i32;
    }
    let error = spc_end_annot(spe);
    pdf_release_obj((*sd).annot_dict);
    (*sd).annot_dict = ptr::null_mut();
    error
}
/* Color:.... */
unsafe fn spc_handler_pdfm_bcolor(spe: *mut spc_env, ap: *mut spc_arg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if !(*ap).cur.is_empty() {
            spc_util_read_pdfcolor(spe, ap, Some(psc))
        } else {
            Ok(fc.clone())
        };
    }
    if let (Ok(mut sc), Ok(fc)) = (sc, fc) {
        pdf_color_push(&mut sc, &fc);
        0
    } else {
        spc_warn!(spe, "Invalid color specification?");
        -1
    }
}
/*
 * This special changes the current color without clearing the color stack.
 * It therefore differs from "color rgb 1 0 0".
 */
unsafe fn spc_handler_pdfm_scolor(spe: *mut spc_env, ap: *mut spc_arg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if !(*ap).cur.is_empty() {
            spc_util_read_pdfcolor(spe, ap, Some(psc))
        } else {
            Ok(fc.clone())
        };
    }
    if let (Ok(mut fc), Ok(mut sc)) = (fc, sc) {
        pdf_color_set(&mut sc, &mut fc);
        0
    } else {
        spc_warn!(spe, "Invalid color specification?");
        -1
    }
}
unsafe fn spc_handler_pdfm_ecolor(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_color_pop();
    0i32
}
unsafe fn spc_handler_pdfm_btrans(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        return -1i32;
    }
    /* Create transformation matrix */
    let mut M = ti.matrix.clone();
    M.m31 += (1. - M.m11) * (*spe).x_user - M.m21 * (*spe).y_user;
    M.m32 += (1. - M.m22) * (*spe).y_user - M.m12 * (*spe).x_user;
    pdf_dev_gsave();
    pdf_dev_concat(&mut M);
    0i32
}
unsafe fn spc_handler_pdfm_etrans(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_dev_grestore();
    /*
     * Unfortunately, the following line is necessary in case
     * of a color change inside of the save/restore pair.
     * (Font changes are automatically corrected by pdf_dev_grestore().)
     * Anything that was done there must be redone, so in effect,
     * we make no assumptions about what fonts. We act like we are
     * starting a new page.
     */
    pdf_dev_reset_color(0i32);
    0i32
}
unsafe fn spc_handler_pdfm_outline(spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut is_open: i32 = -1i32;
    (*args).cur.skip_white();
    /*
     * pdf:outline is extended to support open/close feature
     *
     * pdf:outline 1 ... (as DVIPDFM)
     * pdf:outline [] 1 ... (open bookmark)
     * pdf:outline [-] 1 ... (closed bookmark)
     */
    if (*args).cur.len() > 3 && (*args).cur[0] == b'[' {
        (*args).cur = &(*args).cur[1..];
        if (*args).cur[0] == b'-' {
            (*args).cur = &(*args).cur[1..];
        } else {
            is_open = 1i32
        }
        (*args).cur = &(*args).cur[1..];
    }
    (*args).cur.skip_white();
    let mut level = if let Some(tmp) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
        if !(!tmp.is_null() && (*tmp).is_number()) {
            pdf_release_obj(tmp);
            spc_warn!(spe, "Expecting number for outline item depth.");
            return -1i32;
        }
        let level = (*tmp).as_f64() as i32;
        pdf_release_obj(tmp);
        level
    } else {
        spc_warn!(spe, "Missing number for outline item depth.");
        return -1i32;
    };
    /* What is this? Starting at level 3 and can go down to level 1?
     *
     * Here is the original comment:
     *  Make sure we know where the starting level is
     *
     * NOTE: added
     *  We need this for converting pages from 3rd to... :(
     */
    (*sd).lowest_level = if (*sd).lowest_level < level {
        (*sd).lowest_level
    } else {
        level
    };
    level += 1i32 - (*sd).lowest_level;
    let item_dict = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd);
    if item_dict.is_none() {
        spc_warn!(spe, "Ignoring invalid dictionary.");
        return -1i32;
    }
    let item_dict = item_dict.unwrap();
    let mut current_depth = pdf_doc_bookmarks_depth();
    if current_depth > level {
        loop {
            let fresh2 = current_depth;
            current_depth = current_depth - 1;
            if !(fresh2 > level) {
                break;
            }
            pdf_doc_bookmarks_up();
        }
    } else if current_depth < level {
        loop {
            let fresh3 = current_depth;
            current_depth = current_depth + 1;
            if !(fresh3 < level) {
                break;
            }
            pdf_doc_bookmarks_down();
        }
    }
    pdf_doc_bookmarks_add(item_dict, is_open);
    0i32
}
unsafe fn spc_handler_pdfm_article(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    (*args).cur.skip_white();
    if let Some(ident) = (*args).cur.parse_opt_ident() {
        if let Some(info_dict) = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd) {
            pdf_doc_begin_article(ident.as_ptr(), pdf_link_obj(info_dict));
            spc_push_object(ident.as_ptr(), info_dict);
            0
        } else {
            spc_warn!(spe, "Ignoring article with invalid info dictionary.");
            return -1i32;
        }
    } else {
        spc_warn!(spe, "Article name expected but not found.");
        -1
    }
}
unsafe fn spc_handler_pdfm_bead(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let article_info;
    let mut ti = transform_info::new();
    (*args).cur.skip_white();
    if (*args).cur[0] != b'@' {
        spc_warn!(spe, "Article identifier expected but not found.");
        return -1i32;
    }
    let article_name = (*args).cur.parse_opt_ident();
    if article_name.is_none() {
        spc_warn!(spe, "Article reference expected but not found.");
        return -1i32;
    }
    let article_name = article_name.unwrap();
    /* If okay so far, try to get a bounding box */
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        return -1i32;
    }
    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return -1i32;
    }
    let mut cp = point2((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut cp, None);
    let mut rect = if ti.flags & 1i32 << 0i32 != 0 {
        ti.bbox.translate(cp.to_vector())
    } else {
        Rect::new(
            point2(cp.x, cp.y - (*spe).mag * ti.depth),
            point2(cp.x + (*spe).mag * ti.width, cp.y + (*spe).mag * ti.height),
        )
    };
    (*args).cur.skip_white();
    if (*args).cur[0] != b'<' {
        article_info = pdf_dict::new().into_obj();
    } else {
        if let Some(ai) = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd) {
            article_info = ai;
        } else {
            spc_warn!(spe, "Error in reading dictionary.");
            return -1i32;
        }
    }
    /* Does this article exist yet */
    let article = spc_lookup_object(article_name.as_ptr());
    if !article.is_null() {
        (*article).as_dict_mut().merge((*article_info).as_dict());
        pdf_release_obj(article_info);
    } else {
        pdf_doc_begin_article(article_name.as_ptr(), pdf_link_obj(article_info));
        spc_push_object(article_name.as_ptr(), article_info);
    }
    let page_no = pdf_doc_current_page_number();
    pdf_doc_add_bead(article_name.as_ptr(), &[], page_no, &mut rect);
    0i32
}
unsafe fn spc_handler_pdfm_image(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut ident = None;
    let mut ti = transform_info::new();
    let mut options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    (*args).cur.skip_white();
    if (*args).cur[0] == b'@' {
        ident = (*args).cur.parse_opt_ident();
        let xobj_id = findresource(sd, ident.as_ref());
        if xobj_id >= 0 {
            if let Some(i) = ident {
                spc_warn!(
                    spe,
                    "Object reference name for image \"{}\" already used.",
                    i.display(),
                );
            }
            return -1i32;
        }
    }
    /* 2015/12/29
     * There should not be "page" and "pagebox" in read_dimtrns().
     * It is for reading "dimensions" and "transformations" and "page" is
     * completely unrelated.
     */
    transform_info_clear(&mut ti);
    if spc_util_read_blahblah(
        spe,
        &mut ti,
        &mut options.page_no,
        &mut options.bbox_type,
        args,
    ) < 0i32
    {
        spc_warn!(spe, "Reading option field in pdf:image failed.");
        return -1i32;
    }
    (*args).cur.skip_white();
    let fspec = (*args).cur.parse_pdf_object(ptr::null_mut());
    if fspec.is_none() {
        spc_warn!(spe, "Missing filename string for pdf:image.");
        return -1i32;
    }
    let fspec = fspec.unwrap();
    if !(*fspec).is_string() {
        spc_warn!(spe, "Missing filename string for pdf:image.");
        pdf_release_obj(fspec);
        return -1i32;
    }
    (*args).cur.skip_white();
    if !(*args).cur.is_empty() {
        options.dict = if let Some(obj) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
            obj
        } else {
            ptr::null_mut()
        };
    }
    let xobj_id = pdf_ximage_findresource(pdf_string_value(&*fspec) as *const i8, options);
    if xobj_id < 0i32 {
        spc_warn!(spe, "Could not find image resource...");
        pdf_release_obj(fspec);
        return -1i32;
    }
    if ti.flags & 1i32 << 4i32 == 0 {
        pdf_dev_put_image(xobj_id, &mut ti, (*spe).x_user, (*spe).y_user);
    }
    if let Some(i) = ident {
        addresource(sd, i.as_ptr(), xobj_id);
    }
    pdf_release_obj(fspec);
    0i32
}
/* Use do_names instead. */
unsafe fn spc_handler_pdfm_dest(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    let name = (*args).cur.parse_pdf_object(ptr::null_mut());
    if name.is_none() {
        spc_warn!(
            spe,
            "PDF string expected for destination name but not found."
        );
        return -1i32;
    }
    let name = name.unwrap();
    if !(*name).is_string() {
        spc_warn!(
            spe,
            "PDF string expected for destination name but invalid type."
        );
        pdf_release_obj(name);
        return -1i32;
    }
    if let Some(array) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
        if !(*array).is_array() {
            spc_warn!(spe, "Destination not specified as an array object!");
            pdf_release_obj(name);
            pdf_release_obj(array);
            return -1i32;
        }
        pdf_doc_add_names(
            b"Dests\x00" as *const u8 as *const i8,
            pdf_string_value(&*name),
            pdf_string_length(&*name) as i32,
            array,
        );
    } else {
        spc_warn!(spe, "No destination specified for pdf:dest.");
        pdf_release_obj(name);
        return -1i32;
    }
    pdf_release_obj(name);
    0i32
}
unsafe fn spc_handler_pdfm_names(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let category = (*args).cur.parse_pdf_object(ptr::null_mut());
    if category.is_none() {
        spc_warn!(spe, "PDF name expected but not found.");
        return -1i32;
    }
    let category = category.unwrap();
    if !(*category).is_name() {
        spc_warn!(spe, "PDF name expected but not found.");
        pdf_release_obj(category);
        return -1i32;
    }
    if let Some(tmp) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
        if (*tmp).is_array() {
            let size = (*tmp).as_array().len() as i32;
            if size % 2i32 != 0i32 {
                spc_warn!(spe, "Array size not multiple of 2 for pdf:names.");
                pdf_release_obj(category);
                pdf_release_obj(tmp);
                return -1i32;
            }
            for i in 0..(size / 2) as usize {
                let key = (*tmp).as_array()[2 * i];
                let value = (*tmp).as_array_mut()[2 * i + 1];
                if !(*key).is_string() {
                    spc_warn!(spe, "Name tree key must be string.");
                    pdf_release_obj(category);
                    pdf_release_obj(tmp);
                    return -1i32;
                } else {
                    if pdf_doc_add_names(
                        (*category).as_name().as_ptr() as *mut i8,
                        pdf_string_value(&*key),
                        pdf_string_length(&*key) as i32,
                        pdf_link_obj(value),
                    ) < 0i32
                    {
                        spc_warn!(spe, "Failed to add Name tree entry...");
                        pdf_release_obj(category);
                        pdf_release_obj(tmp);
                        return -1i32;
                    }
                }
            }
            pdf_release_obj(tmp);
        } else if (*tmp).is_string() {
            let key = tmp;
            if let Some(value) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
                if pdf_doc_add_names(
                    (*category).as_name().as_ptr() as *mut i8,
                    pdf_string_value(&*key),
                    pdf_string_length(&*key) as i32,
                    value,
                ) < 0i32
                {
                    spc_warn!(spe, "Failed to add Name tree entry...");
                    pdf_release_obj(category);
                    pdf_release_obj(key);
                    return -1i32;
                }
                pdf_release_obj(key);
            } else {
                pdf_release_obj(category);
                pdf_release_obj(key);
                spc_warn!(spe, "PDF object expected but not found.");
                return -1i32;
            }
        } else {
            pdf_release_obj(tmp);
            pdf_release_obj(category);
            spc_warn!(spe, "Invalid object type for pdf:names.");
            return -1i32;
        }
    } else {
        spc_warn!(spe, "PDF object expected but not found.");
        pdf_release_obj(category);
        return -1i32;
    }
    pdf_release_obj(category);
    0i32
}
unsafe fn spc_handler_pdfm_docinfo(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if let Some(dict) = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd) {
        let docinfo = pdf_doc_get_dictionary("Info");
        (*docinfo).as_dict_mut().merge((*dict).as_dict());
        pdf_release_obj(dict);
        0
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        -1
    }
}
unsafe fn spc_handler_pdfm_docview(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if let Some(dict) = (*args).cur.parse_pdf_dict_with_tounicode(&mut (*sd).cd) {
        let catalog = pdf_doc_get_dictionary("Catalog");
        /* Avoid overriding whole ViewerPreferences */
        let pref_old = (*catalog).as_dict_mut().get_mut("ViewerPreferences"); /* Close all? */
        let pref_add = (*dict).as_dict().get("ViewerPreferences");
        if let (Some(pref_old), Some(pref_add)) = (pref_old, pref_add) {
            (*pref_old).as_dict_mut().merge((*pref_add).as_dict());
            pdf_remove_dict(&mut *dict, "ViewerPreferences");
        }
        (*catalog).as_dict_mut().merge((*dict).as_dict());
        pdf_release_obj(dict);
        0
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        return -1i32;
    }
}
unsafe fn spc_handler_pdfm_close(mut _spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if let Some(ident) = (*args).cur.parse_opt_ident() {
        spc_flush_object(ident.as_ptr());
    } else {
        spc_clear_objects();
    }
    0i32
}
unsafe fn spc_handler_pdfm_object(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if let Some(ident) = (*args).cur.parse_opt_ident() {
        if let Some(object) = (*args).cur.parse_pdf_object(ptr::null_mut()) {
            spc_push_object(ident.as_ptr(), object)
        } else {
            spc_warn!(
                spe,
                "Could not find an object definition for \"{}\".",
                ident.display(),
            );
            return -1i32;
        }
        0
    } else {
        spc_warn!(spe, "Could not find a object identifier.");
        return -1i32;
    }
}
unsafe fn spc_handler_pdfm_content(spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut len = 0;
    (*args).cur.skip_white();
    if !(*args).cur.is_empty() {
        let mut M = TMatrix::create_translation((*spe).x_user, (*spe).y_user);
        WORK_BUFFER[len] = b' ';
        len += 1;
        WORK_BUFFER[len] = b'q';
        len += 1;
        WORK_BUFFER[len] = b' ';
        len += 1;
        len += pdf_sprint_matrix(&mut WORK_BUFFER[len..], &mut M) as usize;
        WORK_BUFFER[len] = b' ';
        len += 1;
        WORK_BUFFER[len] = b'c';
        len += 1;
        WORK_BUFFER[len] = b'm';
        len += 1;
        WORK_BUFFER[len] = b' ';
        len += 1;
        /* op: Q */
        pdf_doc_add_page_content(&WORK_BUFFER[..len]); /* op: q cm */
        pdf_doc_add_page_content((*args).cur); /* op: ANY */
        pdf_doc_add_page_content(b" Q");
        /* op: ANY */
    } /* op: */
    (*args).cur = &[]; /* op: ANY */
    return 0i32; /*kpse_find_pict(instring);*/
}
unsafe fn spc_handler_pdfm_literal(spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut direct: i32 = 0i32;
    (*args).cur.skip_white();
    while !(*args).cur.is_empty() {
        if (*args).cur.len() >= 7 && (*args).cur.starts_with(b"reverse") {
            (*args).cur = &(*args).cur[7..];
            warn!("The special \"pdf:literal reverse ...\" is no longer supported.\nIgnore the \"reverse\" option.");
        } else {
            if !((*args).cur.len() >= 6 && (*args).cur.starts_with(b"direct")) {
                break;
            }
            direct = 1i32;
            (*args).cur = &(*args).cur[6..];
        }
        (*args).cur.skip_white();
    }
    if !(*args).cur.is_empty() {
        if direct == 0 {
            let mut M = TMatrix::create_translation((*spe).x_user, (*spe).y_user);
            pdf_dev_concat(&mut M);
        }
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content((*args).cur);
        if direct == 0 {
            let mut M = TMatrix::create_translation(-(*spe).x_user, -(*spe).y_user);
            pdf_dev_concat(&mut M);
        }
    }
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_bcontent(spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_dev_gsave();
    let pos = pdf_dev_get_coord();
    let mut M = TMatrix::create_translation((*spe).x_user - pos.x, (*spe).y_user - pos.y);
    pdf_dev_concat(&mut M);
    pdf_dev_push_coord((*spe).x_user, (*spe).y_user);
    0i32
}
unsafe fn spc_handler_pdfm_econtent(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_dev_pop_coord();
    pdf_dev_grestore();
    pdf_dev_reset_color(0i32);
    0i32
}
unsafe fn spc_handler_pdfm_code(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if !(*args).cur.is_empty() {
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content((*args).cur);
        (*args).cur = &[];
    }
    0i32
}
unsafe fn spc_handler_pdfm_do_nothing(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_stream_with_type(
    spe: *mut spc_env,
    args: *mut spc_arg,
    type_0: i32,
) -> i32 {
    (*args).cur.skip_white();
    let ident = (*args).cur.parse_opt_ident();
    if ident.is_none() {
        spc_warn!(spe, "Missing objname for pdf:(f)stream.");
        return -1i32;
    }
    (*args).cur.skip_white();
    let tmp = (*args).cur.parse_pdf_object(ptr::null_mut());
    if tmp.is_none() {
        spc_warn!(spe, "Missing input string for pdf:(f)stream.");
        return -1i32;
    }
    let tmp = tmp.unwrap();
    if !(*tmp).is_string() {
        spc_warn!(spe, "Invalid type of input string for pdf:(f)stream.");
        pdf_release_obj(tmp);
        return -1i32;
    }
    let instring = pdf_string_value(&*tmp) as *mut i8;
    let mut fstream = match type_0 {
        1 => {
            if instring.is_null() {
                spc_warn!(spe, "Missing filename for pdf:fstream.");
                pdf_release_obj(tmp);
                return -1i32;
            }
            let fullname = ptr::null_mut::<i8>();
            if fullname.is_null() {
                spc_warn!(
                    spe,
                    "File \"{}\" not found.",
                    CStr::from_ptr(instring).display(),
                );
                pdf_release_obj(tmp);
                return -1i32;
            }
            let handle = ttstub_input_open(fullname, TTInputFormat::PICT, 0i32);
            if handle.is_none() {
                spc_warn!(
                    spe,
                    "Could not open file: {}",
                    CStr::from_ptr(instring).display(),
                );
                pdf_release_obj(tmp);
                free(fullname as *mut libc::c_void);
                return -1i32;
            }
            let mut handle = handle.unwrap();
            let mut fstream = pdf_stream::new(STREAM_COMPRESS);
            loop {
                let nb_read = handle.read(&mut WORK_BUFFER[..]).unwrap();
                if !(nb_read > 0) {
                    // TODO: check
                    break;
                }
                fstream.add_slice(&WORK_BUFFER[..nb_read]);
            }
            ttstub_input_close(handle);
            free(fullname as *mut libc::c_void);
            fstream
        }
        0 => {
            let mut fstream = pdf_stream::new(STREAM_COMPRESS);
            if !instring.is_null() {
                fstream.add(instring as *const libc::c_void, strlen(instring) as i32);
            }
            fstream
        }
        _ => {
            pdf_release_obj(tmp);
            return -1i32;
        }
    };
    pdf_release_obj(tmp);
    /*
     * Optional dict.
     *
     *  TODO: check Length, Filter...
     */
    (*args).cur.skip_white();
    if (*args).cur[0] == b'<' {
        let stream_dict = fstream.get_dict_mut();
        if let Some(tmp) = (*args).cur.parse_pdf_dict(ptr::null_mut()) {
            if (*tmp).as_dict().has("Length") {
                pdf_remove_dict(&mut *tmp, "Length");
            } else if (*tmp).as_dict().has("Filter") {
                pdf_remove_dict(&mut *tmp, "Filter");
            }
            stream_dict.merge((*tmp).as_dict());
            pdf_release_obj(tmp);
        } else {
            spc_warn!(spe, "Parsing dictionary failed.");
            return -1i32;
        }
    }
    /* Users should explicitly close this. */
    spc_push_object(ident.unwrap().as_ptr(), fstream.into_obj());
    0i32
}
/*
 * STREAM: Create a PDF stream object from an input string.
 *
 *  pdf: stream @objname (input_string) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_stream(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    spc_handler_pdfm_stream_with_type(spe, args, 0i32)
}
/*
 * FSTREAM: Create a PDF stream object from an existing file.
 *
 *  pdf: fstream @objname (filename) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_fstream(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    spc_handler_pdfm_stream_with_type(spe, args, 1i32)
}
/* Grab page content as follows:
 *
 * Reference point = (x_user, y_user)
 *
 * Case 1. \special{pdf:bxobj @obj width WD height HT depth DP}
 *
 *     Grab the box with the lower-left corner (x_user, y_user-DP)
 *     and the upper right corner (x_user+WD, y_user+HT).
 *
 * Case 2. \special{pdf:bxobj @obj bbox LLX LLY URX, URY}
 *
 *     Grab the box with the lower-left corner (x_user+LLX, y_user+LLY)
 *     and the upper right corner (x_user+URX, y_user+URY).
 *
 * Note that scale, xscale, yscale, xoffset, yoffset options are ignored.
 */
unsafe fn spc_handler_pdfm_bform(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    (*args).cur.skip_white();
    if let Some(ident) = (*args).cur.parse_opt_ident() {
        transform_info_clear(&mut ti);
        if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
            return -1i32;
        }
        /* A XForm with zero dimension results in a non-invertible transformation
         * matrix. And it may result in unpredictable behaviour. It might be an
         * error in Acrobat. Bounding box with zero dimension may cause division
         * by zero.
         */
        let mut cropbox = if ti.flags & 1i32 << 0i32 != 0 {
            if ti.bbox.size().width == 0. || ti.bbox.size().height == 0. {
                spc_warn!(spe, "Bounding box has a zero dimension.");
                return -1i32;
            }
            ti.bbox
        } else {
            if ti.width == 0.0f64 || ti.depth + ti.height == 0.0f64 {
                spc_warn!(spe, "Bounding box has a zero dimension.");
                return -1i32;
            }
            Rect::new(point2(0., -ti.depth), point2(ti.width, ti.height))
        };
        let xobj_id =
            pdf_doc_begin_grabbing(ident.as_ptr(), (*spe).x_user, (*spe).y_user, &mut cropbox);
        if xobj_id < 0i32 {
            spc_warn!(spe, "Couldn\'t start form object.");
            return -1i32;
        }
        spc_push_object(ident.as_ptr(), pdf_ximage_get_reference(xobj_id));
        0
    } else {
        spc_warn!(spe, "A form XObject must have name.");
        -1
    }
}
/* An extra dictionary after exobj must be merged to the form dictionary,
 * not resource dictionary.
 * Please use pdf:put @resources (before pdf:exobj) instead.
 */
unsafe fn spc_handler_pdfm_eform(mut _spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let attrib: *mut pdf_obj = ptr::null_mut();
    (*args).cur.skip_white();
    let attrib = if !(*args).cur.is_empty() {
        if let Some(attrib) = (*args).cur.parse_pdf_dict(ptr::null_mut()) {
            if !(*attrib).is_dict() {
                pdf_release_obj(attrib);
                ptr::null_mut()
            } else {
                attrib
            }
        } else {
            pdf_release_obj(attrib);
            ptr::null_mut()
        }
    } else {
        ptr::null_mut()
    };
    pdf_doc_end_grabbing(attrib);
    0i32
}
/* Saved XObjects can be used as follows:
 *
 * Reference point = (x_user, y_user)
 *
 * Case 1. \special{pdf:uxobj @obj width WD height HT depth DP}
 *
 *     Scale the XObject to fit in the box
 *     [x_user, y_user-DP, x_user+WD, y_user+HT].
 *
 * Case 2. \special{pdf:uxobj @obj xscale XS yscale YS}
 *
 *     Scale the XObject with XS and YS. Note that width and xscale
 *     or height and yscale cannot be used together.
 *
 * Case 3. \special{pdf:bxobj @obj bbox LLX LLY URX, URY}
 *
 *     Scale the XObject to fit in the box
 *     [x_user+LLX, y_user+LLY, x_user+URX, y_user+URY].
 *
 * Note that xoffset and yoffset moves the reference point where the
 * lower-left corner of the XObject will be put.
 */
unsafe fn spc_handler_pdfm_uxobj(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut ti = transform_info::new();
    let options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    (*args).cur.skip_white();
    if let Some(ident) = (*args).cur.parse_opt_ident() {
        transform_info_clear(&mut ti);
        if !(*args).cur.is_empty() {
            if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
                return -1i32;
            }
        }
        /* Dvipdfmx was suddenly changed to use file name to identify
         * external images. We can't use ident to find image resource
         * here.
         */
        let mut xobj_id = findresource(sd, Some(&ident));
        if xobj_id < 0i32 {
            xobj_id = pdf_ximage_findresource(ident.as_ptr(), options);
            if xobj_id < 0i32 {
                spc_warn!(
                    spe,
                    "Specified (image) object doesn\'t exist: {}",
                    ident.display(),
                );
                return -1;
            }
        }
        pdf_dev_put_image(xobj_id, &mut ti, (*spe).x_user, (*spe).y_user);
        0
    } else {
        spc_warn!(spe, "No object identifier given.");
        -1
    }
}
unsafe fn spc_handler_pdfm_link(spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    spc_resume_annot(spe)
}
unsafe fn spc_handler_pdfm_nolink(spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    spc_suspend_annot(spe)
}
/* Handled at BOP */
unsafe fn spc_handler_pdfm_pagesize(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur = &[];
    0i32
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_pdfm_bgcolor(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    match spc_util_read_pdfcolor(spe, args, None) {
        Ok(colorspec) => {
            pdf_doc_set_bgcolor(Some(&colorspec));
            0
        }
        Err(_) => {
            spc_warn!(spe, "No valid color specified?");
            -1
        }
    }
}
unsafe fn spc_handler_pdfm_mapline(spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    static mut BUFFER: [u8; 1024] = [0; 1024];
    (*ap).cur.skip_white();
    if (*ap).cur.is_empty() {
        spc_warn!(spe, "Empty mapline special?");
        return -1i32;
    }
    let opchr = (*ap).cur[0];
    if opchr == b'-' || opchr == b'+' {
        (*ap).cur = &(*ap).cur[1..];
    }
    (*ap).cur.skip_white();
    match opchr {
        45 => {
            if let Some(map_name) = (*ap).cur.parse_ident() {
                pdf_remove_fontmap_record(map_name.as_ptr());
            } else {
                spc_warn!(spe, "Invalid fontmap line: Missing TFM name.");
                error = -1i32
            }
        }
        _ => {
            BUFFER.copy_from_slice((*ap).cur);
            BUFFER[(*ap).cur.len()] = 0;
            let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                as *mut fontmap_rec;
            pdf_init_fontmap_record(mrec);
            error = pdf_read_fontmap_line(
                mrec,
                BUFFER.as_mut_ptr() as *mut i8,
                (*ap).cur.len() as i32,
                is_pdfm_mapline(BUFFER.as_mut_ptr() as *mut i8),
            );
            if error != 0 {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr == b'+' {
                pdf_append_fontmap_record((*mrec).map_name, mrec);
            } else {
                pdf_insert_fontmap_record((*mrec).map_name, mrec);
            }
            pdf_clear_fontmap_record(mrec);
            free(mrec as *mut libc::c_void);
        }
    }
    if error == 0 {
        (*ap).cur = &[];
    }
    0i32
}
unsafe fn spc_handler_pdfm_mapfile(spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let error;
    (*args).cur.skip_white();
    if (*args).cur.is_empty() {
        return 0i32;
    }
    let mode = match (*args).cur[0] {
        45 => {
            (*args).cur = &(*args).cur[1..];
            '-' as i32
        }
        43 => {
            (*args).cur = &(*args).cur[1..];
            '+' as i32
        }
        _ => 0,
    };
    if let Some(mapfile) = (*args).cur.parse_val_ident() {
        error = pdf_load_fontmap_file(mapfile.as_c_str(), mode)
    } else {
        spc_warn!(spe, "No fontmap file specified.");
        return -1i32;
    }
    error
}
unsafe fn spc_handler_pdfm_tounicode(spe: *mut spc_env, args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    /* First clear */
    (*sd).cd.cmap_id = -1i32;
    (*sd).cd.unescape_backslash = 0i32;
    (*args).cur.skip_white();
    if (*args).cur.is_empty() {
        spc_warn!(spe, "Missing CMap name for pdf:tounicode.");
        return -1i32;
    }
    /* _FIXME_
     * Any valid char allowed for PDF name object should be allowed here.
     * The argument to this special should be a PDF name obejct.
     * But it's too late to change this special.
     */
    if let Some(cmap_name) = (*args).cur.parse_ident() {
        (*sd).cd.cmap_id = CMap_cache_find(&cmap_name.to_string_lossy());
        if (*sd).cd.cmap_id < 0i32 {
            spc_warn!(
                spe,
                "Failed to load ToUnicode mapping: {}",
                cmap_name.display(),
            );
            return -1i32;
        }
        /* Shift-JIS like encoding may contain backslash in 2nd byte.
         * WARNING: This will add nasty extension to PDF parser.
         */
        if (*sd).cd.cmap_id >= 0i32 {
            if !strstr(cmap_name.as_ptr(), b"RKSJ\x00" as *const u8 as *const i8).is_null()
                || !strstr(cmap_name.as_ptr(), b"B5\x00" as *const u8 as *const i8).is_null()
                || !strstr(cmap_name.as_ptr(), b"GBK\x00" as *const u8 as *const i8).is_null()
                || !strstr(cmap_name.as_ptr(), b"KSC\x00" as *const u8 as *const i8).is_null()
            {
                (*sd).cd.unescape_backslash = 1i32
            }
        }
        0
    } else {
        spc_warn!(spe, "Missing ToUnicode mapping name...");
        -1
    }
}
const PDFM_HANDLERS: [SpcHandler; 80] = [
    SpcHandler {
        key: b"annotation",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: b"annotate",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: b"annot",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: b"ann",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: b"outline",
        exec: Some(spc_handler_pdfm_outline),
    },
    SpcHandler {
        key: b"out",
        exec: Some(spc_handler_pdfm_outline),
    },
    SpcHandler {
        key: b"article",
        exec: Some(spc_handler_pdfm_article),
    },
    SpcHandler {
        key: b"art",
        exec: Some(spc_handler_pdfm_article),
    },
    SpcHandler {
        key: b"bead",
        exec: Some(spc_handler_pdfm_bead),
    },
    SpcHandler {
        key: b"thread",
        exec: Some(spc_handler_pdfm_bead),
    },
    SpcHandler {
        key: b"destination",
        exec: Some(spc_handler_pdfm_dest),
    },
    SpcHandler {
        key: b"dest",
        exec: Some(spc_handler_pdfm_dest),
    },
    SpcHandler {
        key: b"object",
        exec: Some(spc_handler_pdfm_object),
    },
    SpcHandler {
        key: b"obj",
        exec: Some(spc_handler_pdfm_object),
    },
    SpcHandler {
        key: b"docinfo",
        exec: Some(spc_handler_pdfm_docinfo),
    },
    SpcHandler {
        key: b"docview",
        exec: Some(spc_handler_pdfm_docview),
    },
    SpcHandler {
        key: b"content",
        exec: Some(spc_handler_pdfm_content),
    },
    SpcHandler {
        key: b"put",
        exec: Some(spc_handler_pdfm_put),
    },
    SpcHandler {
        key: b"close",
        exec: Some(spc_handler_pdfm_close),
    },
    SpcHandler {
        key: b"bop",
        exec: Some(spc_handler_pdfm_bop),
    },
    SpcHandler {
        key: b"eop",
        exec: Some(spc_handler_pdfm_eop),
    },
    SpcHandler {
        key: b"image",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: b"img",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: b"epdf",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: b"link",
        exec: Some(spc_handler_pdfm_link),
    },
    SpcHandler {
        key: b"nolink",
        exec: Some(spc_handler_pdfm_nolink),
    },
    SpcHandler {
        key: b"begincolor",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"bcolor",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"bc",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"setcolor",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: b"scolor",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: b"sc",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: b"endcolor",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"ecolor",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"ec",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"begingray",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"bgray",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"bg",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: b"endgray",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"egray",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"eg",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: b"bgcolor",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: b"bgc",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: b"bbc",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: b"bbg",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: b"pagesize",
        exec: Some(spc_handler_pdfm_pagesize),
    },
    SpcHandler {
        key: b"bannot",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: b"beginann",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: b"bann",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: b"eannot",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: b"endann",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: b"eann",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: b"btrans",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: b"begintransform",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: b"begintrans",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: b"bt",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: b"etrans",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: b"endtransform",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: b"endtrans",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: b"et",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: b"bform",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: b"beginxobj",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: b"bxobj",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: b"eform",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: b"endxobj",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: b"exobj",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: b"usexobj",
        exec: Some(spc_handler_pdfm_uxobj),
    },
    SpcHandler {
        key: b"uxobj",
        exec: Some(spc_handler_pdfm_uxobj),
    },
    SpcHandler {
        key: b"tounicode",
        exec: Some(spc_handler_pdfm_tounicode),
    },
    SpcHandler {
        key: b"literal",
        exec: Some(spc_handler_pdfm_literal),
    },
    SpcHandler {
        key: b"stream",
        exec: Some(spc_handler_pdfm_stream),
    },
    SpcHandler {
        key: b"fstream",
        exec: Some(spc_handler_pdfm_fstream),
    },
    SpcHandler {
        key: b"names",
        exec: Some(spc_handler_pdfm_names),
    },
    SpcHandler {
        key: b"mapline",
        exec: Some(spc_handler_pdfm_mapline),
    },
    SpcHandler {
        key: b"mapfile",
        exec: Some(spc_handler_pdfm_mapfile),
    },
    SpcHandler {
        key: b"bcontent",
        exec: Some(spc_handler_pdfm_bcontent),
    },
    SpcHandler {
        key: b"econtent",
        exec: Some(spc_handler_pdfm_econtent),
    },
    SpcHandler {
        key: b"code",
        exec: Some(spc_handler_pdfm_code),
    },
    SpcHandler {
        key: b"minorversion",
        exec: Some(spc_handler_pdfm_do_nothing),
    },
    SpcHandler {
        key: b"encrypt",
        exec: Some(spc_handler_pdfm_do_nothing),
    },
];
pub fn spc_pdfm_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"pdf:")
}

pub unsafe fn spc_pdfm_setup_handler(
    mut sph: *mut SpcHandler,
    spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    (*ap).cur.skip_white();
    if !(*ap).cur.starts_with(b"pdf:") {
        spc_warn!(spe, "Not pdf: special???");
        return -1i32;
    }
    (*ap).cur = &(*ap).cur[b"pdf:".len()..];
    (*ap).cur.skip_white();
    if let Some(q) = (*ap).cur.parse_c_ident() {
        for handler in PDFM_HANDLERS.iter() {
            if q.to_bytes() == handler.key {
                (*ap).command = Some(handler.key);
                (*sph).key = b"pdf:";
                (*sph).exec = handler.exec;
                (*ap).cur.skip_white();
                error = 0i32;
                break;
            }
        }
    }
    error
}
