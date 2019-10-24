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
    unused_mut
)]

use std::io::Read;
use std::ffi::CStr;

use crate::DisplayExt;
use crate::TTInputFormat;
use crate::{spc_warn, warn};
use crate::{streq_ptr, strstartswith};

use super::util::{spc_util_read_blahblah, spc_util_read_dimtrns, spc_util_read_pdfcolor};
use super::{
    spc_begin_annot, spc_clear_objects, spc_end_annot, spc_flush_object, spc_lookup_object,
    spc_push_object, spc_resume_annot, spc_suspend_annot,
};
use crate::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode};
use crate::dpx_dpxutil::parse_c_ident;
use crate::dpx_dpxutil::{ht_append_table, ht_clear_table, ht_init_table, ht_lookup_table};
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
use crate::dpx_pdfdev::{
    pdf_dev_put_image, pdf_rect, pdf_tmatrix, transform_info, transform_info_clear,
};
use crate::dpx_pdfdoc::{
    pdf_doc_add_annot, pdf_doc_add_bead, pdf_doc_add_names, pdf_doc_add_page_content,
    pdf_doc_add_page_content_ptr, pdf_doc_begin_article, pdf_doc_begin_grabbing,
    pdf_doc_bookmarks_add, pdf_doc_bookmarks_depth, pdf_doc_bookmarks_down, pdf_doc_bookmarks_up,
    pdf_doc_current_page_number, pdf_doc_end_grabbing, pdf_doc_get_dictionary, pdf_doc_set_bgcolor,
    pdf_doc_set_bop_content, pdf_doc_set_eop_content,
};
use crate::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_grestore, pdf_dev_gsave, pdf_dev_transform};
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_array_length, pdf_copy_name, pdf_file,
    pdf_foreach_dict, pdf_get_array, pdf_link_obj, pdf_lookup_dict, pdf_merge_dict, pdf_name_value,
    pdf_new_array, pdf_new_dict, pdf_new_stream, pdf_number_value, pdf_obj, pdf_obj_typeof,
    pdf_release_obj, pdf_remove_dict, pdf_set_string, pdf_stream_dict, pdf_string_length,
    pdf_string_value, PdfObjType,
};
use crate::dpx_pdfparse::{
    parse_ident, parse_opt_ident, parse_pdf_dict, parse_pdf_object, parse_pdf_tainted_dict,
    parse_val_ident, skip_white,
};
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

use crate::dpx_pdfdev::pdf_coord;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut _PDF_STAT: spc_pdf_ = spc_pdf_ {
    annot_dict: 0 as *const pdf_obj as *mut pdf_obj,
    lowest_level: 255i32,
    resourcemap: 0 as *const ht_table as *mut ht_table,
    cd: {
        let mut init = tounicode {
            cmap_id: -1i32,
            unescape_backslash: 0i32,
            taintkeys: 0 as *const pdf_obj as *mut pdf_obj,
        };
        init
    },
};
/* PLEASE REMOVE THIS */
unsafe extern "C" fn hval_free(mut vp: *mut libc::c_void) {
    free(vp); /* unused */
}
unsafe extern "C" fn addresource(
    mut sd: *mut spc_pdf_,
    mut ident: *const i8,
    mut res_id: i32,
) -> i32 {
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
unsafe extern "C" fn findresource(mut sd: *mut spc_pdf_, mut ident: *const i8) -> i32 {
    if ident.is_null() {
        return -1i32;
    }
    let r = ht_lookup_table(
        (*sd).resourcemap,
        ident as *const libc::c_void,
        strlen(ident) as i32,
    ) as *mut resource_map;
    if !r.is_null() {
        (*r).res_id
    } else {
        -1i32
    }
}
unsafe fn spc_handler_pdfm__init(mut dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_pdf_ = dp as *mut spc_pdf_;
    /* The folllowing dictionary entry keys are considered as keys for
     * text strings. Be sure that string object is NOT always a text string.
     */
    static mut DEFAULT_TAINTKEYS: [*const i8; 12] = [
        b"Title\x00" as *const u8 as *const i8,
        b"Author\x00" as *const u8 as *const i8,
        b"Subject\x00" as *const u8 as *const i8,
        b"Keywords\x00" as *const u8 as *const i8,
        b"Creator\x00" as *const u8 as *const i8,
        b"Producer\x00" as *const u8 as *const i8,
        b"Contents\x00" as *const u8 as *const i8,
        b"Subj\x00" as *const u8 as *const i8,
        b"TU\x00" as *const u8 as *const i8,
        b"T\x00" as *const u8 as *const i8,
        b"TM\x00" as *const u8 as *const i8,
        0 as *const i8,
    ];
    (*sd).annot_dict = 0 as *mut pdf_obj;
    (*sd).lowest_level = 255i32;
    (*sd).resourcemap =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        (*sd).resourcemap,
        Some(hval_free as unsafe extern "C" fn(_: *mut libc::c_void) -> ()),
    );
    (*sd).cd.taintkeys = pdf_new_array();
    let mut i = 0;
    while !DEFAULT_TAINTKEYS[i].is_null() {
        pdf_add_array((*sd).cd.taintkeys, pdf_copy_name(DEFAULT_TAINTKEYS[i]));
        i += 1
    }
    0i32
}
unsafe fn spc_handler_pdfm__clean(mut dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_pdf_ = dp as *mut spc_pdf_;
    if !(*sd).annot_dict.is_null() {
        warn!("Unbalanced bann and eann found.");
        pdf_release_obj((*sd).annot_dict);
    }
    (*sd).lowest_level = 255i32;
    (*sd).annot_dict = 0 as *mut pdf_obj;
    if !(*sd).resourcemap.is_null() {
        ht_clear_table((*sd).resourcemap);
        free((*sd).resourcemap as *mut libc::c_void);
    }
    (*sd).resourcemap = 0 as *mut ht_table;
    pdf_release_obj((*sd).cd.taintkeys);
    (*sd).cd.taintkeys = 0 as *mut pdf_obj;
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn spc_pdfm_at_begin_document() -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    spc_handler_pdfm__init(sd as *mut libc::c_void)
}
#[no_mangle]
pub unsafe extern "C" fn spc_pdfm_at_end_document() -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    spc_handler_pdfm__clean(sd as *mut libc::c_void)
}
/* Dvipdfm specials */
unsafe fn spc_handler_pdfm_bop(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if (*args).curptr < (*args).endptr {
        pdf_doc_set_bop_content(
            (*args).curptr,
            (*args).endptr.wrapping_offset_from((*args).curptr) as i64 as i32 as u32,
        );
    }
    (*args).curptr = (*args).endptr;
    0i32
}
unsafe fn spc_handler_pdfm_eop(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if (*args).curptr < (*args).endptr {
        pdf_doc_set_eop_content(
            (*args).curptr,
            (*args).endptr.wrapping_offset_from((*args).curptr) as i64 as i32 as u32,
        );
    }
    (*args).curptr = (*args).endptr;
    0i32
}
/* Why should we have this kind of things? */
unsafe extern "C" fn safeputresdent(
    mut kp: *mut pdf_obj,
    mut vp: *mut pdf_obj,
    mut dp: *mut libc::c_void,
) -> i32 {
    assert!(!kp.is_null() && !vp.is_null() && !dp.is_null());
    let key = pdf_name_value(&*kp);
    if pdf_lookup_dict(dp as *mut pdf_obj, key.to_bytes()).is_some() {
        warn!(
            "Object \"{}\" already defined in dict! (ignored)",
            key.display()
        );
    } else {
        pdf_add_dict(dp as *mut pdf_obj, key.to_bytes(), pdf_link_obj(vp));
    }
    0i32
}
unsafe extern "C" fn safeputresdict(
    mut kp: *mut pdf_obj,
    mut vp: *mut pdf_obj,
    mut dp: *mut libc::c_void,
) -> i32 {
    assert!(!kp.is_null() && !vp.is_null() && !dp.is_null());
    let key = pdf_name_value(&*kp);
    let dict = pdf_lookup_dict(dp as *mut pdf_obj, key.to_bytes());
    if (*vp).is_indirect() {
        pdf_add_dict(dp as *mut pdf_obj, key.to_bytes(), pdf_link_obj(vp));
    } else if (*vp).is_dict() {
        if let Some(dict) = dict {
            pdf_foreach_dict(
                vp,
                Some(
                    safeputresdent
                        as unsafe extern "C" fn(
                            _: *mut pdf_obj,
                            _: *mut pdf_obj,
                            _: *mut libc::c_void,
                        ) -> i32,
                ),
                dict as *mut libc::c_void,
            );
        } else {
            pdf_add_dict(dp as *mut pdf_obj, key.to_bytes(), pdf_link_obj(vp));
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
unsafe fn spc_handler_pdfm_put(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    let ident = parse_opt_ident(&mut (*ap).curptr, (*ap).endptr);
    if ident.is_null() {
        spc_warn!(spe, "Missing object identifier.");
        return -1i32;
    }
    let obj1 = spc_lookup_object(ident); /* put obj2 into obj1 */
    if obj1.is_null() {
        spc_warn!(
            spe,
            "Specified object not exist: {}",
            CStr::from_ptr(ident).display(),
        );
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    let obj2 = parse_pdf_object(&mut (*ap).curptr, (*ap).endptr, 0 as *mut pdf_file);
    if obj2.is_null() {
        spc_warn!(
            spe,
            "Missing (an) object(s) to put into \"{}\"!",
            CStr::from_ptr(ident).display(),
        );
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    match pdf_obj_typeof(obj1) {
        PdfObjType::DICT => {
            if !(*obj2).is_dict() {
                spc_warn!(
                    spe,
                    "Inconsistent object type for \"put\" (expecting DICT): {}",
                    CStr::from_ptr(ident).display(),
                );
                error = -1i32
            } else if streq_ptr(ident, b"resources\x00" as *const u8 as *const i8) {
                error = pdf_foreach_dict(
                    obj2,
                    Some(
                        safeputresdict
                            as unsafe extern "C" fn(
                                _: *mut pdf_obj,
                                _: *mut pdf_obj,
                                _: *mut libc::c_void,
                            ) -> i32,
                    ),
                    obj1 as *mut libc::c_void,
                )
            } else {
                pdf_merge_dict(obj1, obj2);
            }
        }
        PdfObjType::STREAM => {
            if (*obj2).is_dict() {
                pdf_merge_dict(pdf_stream_dict(obj1), obj2);
            } else if (*obj2).is_stream() {
                spc_warn!(
                    spe,
                    "\"put\" operation not supported for STREAM <- STREAM: {}",
                    CStr::from_ptr(ident).display(),
                );
                error = -1i32
            } else {
                spc_warn!(
                    spe,
                    "Invalid type: expecting a DICT or STREAM: {}",
                    CStr::from_ptr(ident).display(),
                );
                error = -1i32
            }
        }
        PdfObjType::ARRAY => {
            /* dvipdfm */
            pdf_add_array(obj1, pdf_link_obj(obj2));
            while (*ap).curptr < (*ap).endptr {
                let mut obj3: *mut pdf_obj =
                    parse_pdf_object(&mut (*ap).curptr, (*ap).endptr, 0 as *mut pdf_file);
                if obj3.is_null() {
                    break;
                }
                pdf_add_array(obj1, obj3);
                skip_white(&mut (*ap).curptr, (*ap).endptr);
            }
        }
        _ => {
            spc_warn!(
                spe,
                "Can\'t \"put\" object into non-DICT/STREAM/ARRAY type object: {}",
                CStr::from_ptr(ident).display(),
            );
            error = -1i32
        }
    }
    pdf_release_obj(obj2);
    free(ident as *mut libc::c_void);
    error
}
/* For pdf:tounicode support
 * This feature is provided for convenience. TeX can't do
 * input encoding conversion.
 */
unsafe extern "C" fn reencodestring(mut cmap: *mut CMap, mut instring: *mut pdf_obj) -> i32 {
    let mut wbuf: [u8; 4096] = [0; 4096];
    if cmap.is_null() || instring.is_null() {
        return 0i32;
    }
    let mut inbufleft = pdf_string_length(instring) as size_t;
    let mut inbufcur = pdf_string_value(instring) as *const u8;
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
        instring,
        wbuf.as_mut_ptr(),
        (4096i32 as u64).wrapping_sub(obufleft),
    );
    0i32
}
unsafe extern "C" fn maybe_reencode_utf8(mut instring: *mut pdf_obj) -> i32 {
    let mut non_ascii: i32 = 0i32;
    let mut cp: *const u8;
    let mut wbuf: [u8; 4096] = [0; 4096];
    if instring.is_null() {
        return 0i32;
    }
    let inlen = pdf_string_length(instring) as i32;
    let inbuf = pdf_string_value(instring) as *mut u8;
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
        instring,
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
unsafe extern "C" fn needreencode(
    mut kp: *mut pdf_obj,
    mut vp: *mut pdf_obj,
    mut cd: *mut tounicode,
) -> i32 {
    let mut r: i32 = 0i32;
    assert!(!cd.is_null() && !(*cd).taintkeys.is_null());
    assert!((*kp).is_name());
    assert!((*vp).is_string());
    for i in 0..pdf_array_length((*cd).taintkeys) {
        let tk = pdf_get_array((*cd).taintkeys, i as i32);
        assert!(!tk.is_null() && (*tk).is_name());
        if pdf_name_value(&*kp) == pdf_name_value(&*tk) {
            r = 1i32;
            break;
        }
    }
    if r != 0 {
        /* Check UTF-16BE BOM. */
        if pdf_string_length(vp) >= 2_u32
            && memcmp(
                pdf_string_value(vp),
                b"\xfe\xff\x00" as *const u8 as *const i8 as *const libc::c_void,
                2,
            ) == 0
        {
            r = 0i32
        }
    } /* continue */
    r
}
unsafe extern "C" fn modstrings(
    mut kp: *mut pdf_obj,
    mut vp: *mut pdf_obj,
    mut dp: *mut libc::c_void,
) -> i32 {
    let mut r: i32 = 0i32;
    let mut cd: *mut tounicode = dp as *mut tounicode;
    assert!((*kp).is_name());
    match pdf_obj_typeof(vp) {
        PdfObjType::STRING => {
            if !cd.is_null() && (*cd).cmap_id >= 0i32 && !(*cd).taintkeys.is_null() {
                let mut cmap: *mut CMap = CMap_cache_get((*cd).cmap_id);
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
            r = pdf_foreach_dict(
                vp,
                Some(
                    modstrings
                        as unsafe extern "C" fn(
                            _: *mut pdf_obj,
                            _: *mut pdf_obj,
                            _: *mut libc::c_void,
                        ) -> i32,
                ),
                dp,
            )
        }
        PdfObjType::STREAM => {
            r = pdf_foreach_dict(
                pdf_stream_dict(vp),
                Some(
                    modstrings
                        as unsafe extern "C" fn(
                            _: *mut pdf_obj,
                            _: *mut pdf_obj,
                            _: *mut libc::c_void,
                        ) -> i32,
                ),
                dp,
            )
        }
        _ => {}
    }
    r
}
unsafe extern "C" fn parse_pdf_dict_with_tounicode(
    mut pp: *mut *const i8,
    mut endptr: *const i8,
    mut cd: *mut tounicode,
) -> *mut pdf_obj {
    /* disable this test for XDV files, as we do UTF8 reencoding with no cmap */
    if is_xdv == 0 && (*cd).cmap_id < 0i32 {
        return parse_pdf_dict(pp, endptr, 0 as *mut pdf_file);
    }
    /* :( */
    let dict = if !cd.is_null() && (*cd).unescape_backslash != 0 {
        parse_pdf_tainted_dict(pp, endptr)
    } else {
        parse_pdf_dict(pp, endptr, 0 as *mut pdf_file)
    };
    if !dict.is_null() {
        pdf_foreach_dict(
            dict,
            Some(
                modstrings
                    as unsafe extern "C" fn(
                        _: *mut pdf_obj,
                        _: *mut pdf_obj,
                        _: *mut libc::c_void,
                    ) -> i32,
            ),
            cd as *mut libc::c_void,
        );
    }
    dict
}
unsafe fn spc_handler_pdfm_annot(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut rect = pdf_rect::new();
    let mut ident: *mut i8 = 0 as *mut i8;
    let mut ti = transform_info::new();
    skip_white(&mut (*args).curptr, (*args).endptr);
    if *(*args).curptr.offset(0) as i32 == '@' as i32 {
        ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
        skip_white(&mut (*args).curptr, (*args).endptr);
    }
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    let mut annot_dict =
        parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if annot_dict.is_null() {
        spc_warn!(spe, "Could not find dictionary object.");
        free(ident as *mut libc::c_void);
        return -1i32;
    } else {
        if !(!annot_dict.is_null() && (*annot_dict).is_dict()) {
            spc_warn!(spe, "Invalid type: not dictionary object.");
            free(ident as *mut libc::c_void);
            pdf_release_obj(annot_dict);
            return -1i32;
        }
    }
    let mut cp = pdf_coord::new((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut cp, None);
    if ti.flags & 1i32 << 0i32 != 0 {
        rect.llx = ti.bbox.llx + cp.x;
        rect.lly = ti.bbox.lly + cp.y;
        rect.urx = ti.bbox.urx + cp.x;
        rect.ury = ti.bbox.ury + cp.y
    } else {
        rect.llx = cp.x;
        rect.lly = cp.y - (*spe).mag * ti.depth;
        rect.urx = cp.x + (*spe).mag * ti.width;
        rect.ury = cp.y + (*spe).mag * ti.height
    }
    /* Order is important... */
    if !ident.is_null() {
        spc_push_object(ident, pdf_link_obj(annot_dict));
    }
    /* Add this reference. */
    pdf_doc_add_annot(
        pdf_doc_current_page_number() as u32,
        &mut rect,
        annot_dict,
        1i32,
    );
    if !ident.is_null() {
        spc_flush_object(ident);
        free(ident as *mut libc::c_void);
    }
    pdf_release_obj(annot_dict);
    0i32
}
/* NOTE: This can't have ident. See "Dvipdfm User's Manual". */
unsafe fn spc_handler_pdfm_bann(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if !(*sd).annot_dict.is_null() {
        spc_warn!(spe, "Can\'t begin an annotation when one is pending.");
        return -1i32;
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    (*sd).annot_dict =
        parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if (*sd).annot_dict.is_null() {
        spc_warn!(spe, "Ignoring annotation with invalid dictionary.");
        return -1i32;
    } else {
        if !(!(*sd).annot_dict.is_null() && (*(*sd).annot_dict).is_dict()) {
            spc_warn!(spe, "Invalid type: not a dictionary object.");
            pdf_release_obj((*sd).annot_dict);
            (*sd).annot_dict = 0 as *mut pdf_obj;
            return -1i32;
        }
    }
    spc_begin_annot(spe, (*sd).annot_dict)
}
unsafe fn spc_handler_pdfm_eann(mut spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    if (*sd).annot_dict.is_null() {
        spc_warn!(spe, "Tried to end an annotation without starting one!");
        return -1i32;
    }
    let error = spc_end_annot(spe);
    pdf_release_obj((*sd).annot_dict);
    (*sd).annot_dict = 0 as *mut pdf_obj;
    error
}
/* Color:.... */
unsafe fn spc_handler_pdfm_bcolor(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if (*ap).curptr < (*ap).endptr {
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
unsafe fn spc_handler_pdfm_scolor(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if (*ap).curptr < (*ap).endptr {
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
unsafe fn spc_handler_pdfm_btrans(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        return -1i32;
    }
    /* Create transformation matrix */
    let mut M = ti.matrix.clone();
    M.e += (1.0f64 - M.a) * (*spe).x_user - M.c * (*spe).y_user;
    M.f += (1.0f64 - M.d) * (*spe).y_user - M.b * (*spe).x_user;
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
unsafe fn spc_handler_pdfm_outline(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut is_open: i32 = -1i32;
    skip_white(&mut (*args).curptr, (*args).endptr);
    /*
     * pdf:outline is extended to support open/close feature
     *
     * pdf:outline 1 ... (as DVIPDFM)
     * pdf:outline [] 1 ... (open bookmark)
     * pdf:outline [-] 1 ... (closed bookmark)
     */
    if (*args).curptr.offset(3) < (*args).endptr && *(*args).curptr as i32 == '[' as i32 {
        (*args).curptr = (*args).curptr.offset(1);
        if *(*args).curptr as i32 == '-' as i32 {
            (*args).curptr = (*args).curptr.offset(1)
        } else {
            is_open = 1i32
        }
        (*args).curptr = (*args).curptr.offset(1)
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    let tmp = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if tmp.is_null() {
        spc_warn!(spe, "Missing number for outline item depth.");
        return -1i32;
    } else {
        if !(!tmp.is_null() && (*tmp).is_number()) {
            pdf_release_obj(tmp);
            spc_warn!(spe, "Expecting number for outline item depth.");
            return -1i32;
        }
    }
    let mut level = pdf_number_value(tmp) as i32;
    pdf_release_obj(tmp);
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
    let item_dict =
        parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if item_dict.is_null() {
        spc_warn!(spe, "Ignoring invalid dictionary.");
        return -1i32;
    }
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
unsafe fn spc_handler_pdfm_article(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if ident.is_null() {
        spc_warn!(spe, "Article name expected but not found.");
        return -1i32;
    }
    let info_dict =
        parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if info_dict.is_null() {
        spc_warn!(spe, "Ignoring article with invalid info dictionary.");
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    pdf_doc_begin_article(ident, pdf_link_obj(info_dict));
    spc_push_object(ident, info_dict);
    free(ident as *mut libc::c_void);
    0i32
}
unsafe fn spc_handler_pdfm_bead(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let article_info;
    let mut rect = pdf_rect::new();
    let mut ti = transform_info::new();
    skip_white(&mut (*args).curptr, (*args).endptr);
    if *(*args).curptr.offset(0) as i32 != '@' as i32 {
        spc_warn!(spe, "Article identifier expected but not found.");
        return -1i32;
    }
    let article_name = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if article_name.is_null() {
        spc_warn!(spe, "Article reference expected but not found.");
        return -1i32;
    }
    /* If okay so far, try to get a bounding box */
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        free(article_name as *mut libc::c_void);
        return -1i32;
    }
    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        free(article_name as *mut libc::c_void);
        return -1i32;
    }
    let mut cp = pdf_coord::new((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut cp, None);
    if ti.flags & 1i32 << 0i32 != 0 {
        rect.llx = ti.bbox.llx + cp.x;
        rect.lly = ti.bbox.lly + cp.y;
        rect.urx = ti.bbox.urx + cp.x;
        rect.ury = ti.bbox.ury + cp.y
    } else {
        rect.llx = cp.x;
        rect.lly = cp.y - (*spe).mag * ti.depth;
        rect.urx = cp.x + (*spe).mag * ti.width;
        rect.ury = cp.y + (*spe).mag * ti.height
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    if *(*args).curptr.offset(0) as i32 != '<' as i32 {
        article_info = pdf_new_dict()
    } else {
        article_info =
            parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
        if article_info.is_null() {
            spc_warn!(spe, "Error in reading dictionary.");
            free(article_name as *mut libc::c_void);
            return -1i32;
        }
    }
    /* Does this article exist yet */
    let article = spc_lookup_object(article_name);
    if !article.is_null() {
        pdf_merge_dict(article, article_info);
        pdf_release_obj(article_info);
    } else {
        pdf_doc_begin_article(article_name, pdf_link_obj(article_info));
        spc_push_object(article_name, article_info);
    }
    let page_no = pdf_doc_current_page_number();
    pdf_doc_add_bead(article_name, &[], page_no, &mut rect);
    free(article_name as *mut libc::c_void);
    0i32
}
unsafe fn spc_handler_pdfm_image(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut ident: *mut i8 = 0 as *mut i8;
    let mut ti = transform_info::new();
    let mut options: load_options = {
        let mut init = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: 0 as *mut pdf_obj,
        };
        init
    };
    skip_white(&mut (*args).curptr, (*args).endptr);
    if *(*args).curptr.offset(0) as i32 == '@' as i32 {
        ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
        let xobj_id = findresource(sd, ident);
        if xobj_id >= 0i32 {
            spc_warn!(
                spe,
                "Object reference name for image \"{}\" already used.",
                CStr::from_ptr(ident).display(),
            );
            free(ident as *mut libc::c_void);
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
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    let fspec = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if fspec.is_null() {
        spc_warn!(spe, "Missing filename string for pdf:image.");
        free(ident as *mut libc::c_void);
        return -1i32;
    } else {
        if !(!fspec.is_null() && (*fspec).is_string()) {
            spc_warn!(spe, "Missing filename string for pdf:image.");
            pdf_release_obj(fspec);
            free(ident as *mut libc::c_void);
            return -1i32;
        }
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr < (*args).endptr {
        options.dict = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file)
    }
    let xobj_id = pdf_ximage_findresource(pdf_string_value(fspec) as *const i8, options);
    if xobj_id < 0i32 {
        spc_warn!(spe, "Could not find image resource...");
        pdf_release_obj(fspec);
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    if ti.flags & 1i32 << 4i32 == 0 {
        pdf_dev_put_image(xobj_id, &mut ti, (*spe).x_user, (*spe).y_user);
    }
    if !ident.is_null() {
        addresource(sd, ident, xobj_id);
        free(ident as *mut libc::c_void);
    }
    pdf_release_obj(fspec);
    0i32
}
/* Use do_names instead. */
unsafe fn spc_handler_pdfm_dest(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    skip_white(&mut (*args).curptr, (*args).endptr);
    let name = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if name.is_null() {
        spc_warn!(
            spe,
            "PDF string expected for destination name but not found."
        );
        return -1i32;
    } else {
        if !(!name.is_null() && (*name).is_string()) {
            spc_warn!(
                spe,
                "PDF string expected for destination name but invalid type."
            );
            pdf_release_obj(name);
            return -1i32;
        }
    }
    let array = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if array.is_null() {
        spc_warn!(spe, "No destination specified for pdf:dest.");
        pdf_release_obj(name);
        return -1i32;
    } else {
        if !(!array.is_null() && (*array).is_array()) {
            spc_warn!(spe, "Destination not specified as an array object!");
            pdf_release_obj(name);
            pdf_release_obj(array);
            return -1i32;
        }
    }
    pdf_doc_add_names(
        b"Dests\x00" as *const u8 as *const i8,
        pdf_string_value(name),
        pdf_string_length(name) as i32,
        array,
    );
    pdf_release_obj(name);
    0i32
}
unsafe fn spc_handler_pdfm_names(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let category = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if category.is_null() {
        spc_warn!(spe, "PDF name expected but not found.");
        return -1i32;
    } else {
        if !(!category.is_null() && (*category).is_name()) {
            spc_warn!(spe, "PDF name expected but not found.");
            pdf_release_obj(category);
            return -1i32;
        }
    }
    let tmp = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if tmp.is_null() {
        spc_warn!(spe, "PDF object expected but not found.");
        pdf_release_obj(category);
        return -1i32;
    } else {
        if !tmp.is_null() && (*tmp).is_array() {
            let size = pdf_array_length(tmp) as i32;
            if size % 2i32 != 0i32 {
                spc_warn!(spe, "Array size not multiple of 2 for pdf:names.");
                pdf_release_obj(category);
                pdf_release_obj(tmp);
                return -1i32;
            }
            for i in 0..(size / 2) {
                let key = pdf_get_array(tmp, 2i32 * i);
                let value = pdf_get_array(tmp, 2i32 * i + 1i32);
                if !(!key.is_null() && (*key).is_string()) {
                    spc_warn!(spe, "Name tree key must be string.");
                    pdf_release_obj(category);
                    pdf_release_obj(tmp);
                    return -1i32;
                } else {
                    if pdf_doc_add_names(
                        pdf_name_value(&*category).as_ptr() as *mut i8,
                        pdf_string_value(key),
                        pdf_string_length(key) as i32,
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
        } else if !tmp.is_null() && (*tmp).is_string() {
            let key = tmp;
            let value = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
            if value.is_null() {
                pdf_release_obj(category);
                pdf_release_obj(key);
                spc_warn!(spe, "PDF object expected but not found.");
                return -1i32;
            }
            if pdf_doc_add_names(
                pdf_name_value(&*category).as_ptr() as *mut i8,
                pdf_string_value(key),
                pdf_string_length(key) as i32,
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
            pdf_release_obj(tmp);
            pdf_release_obj(category);
            spc_warn!(spe, "Invalid object type for pdf:names.");
            return -1i32;
        }
    }
    pdf_release_obj(category);
    0i32
}
unsafe fn spc_handler_pdfm_docinfo(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let dict = parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if dict.is_null() {
        spc_warn!(spe, "Dictionary object expected but not found.");
        return -1i32;
    }
    let docinfo = pdf_doc_get_dictionary("Info");
    pdf_merge_dict(docinfo, dict);
    pdf_release_obj(dict);
    0i32
}
unsafe fn spc_handler_pdfm_docview(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let dict = parse_pdf_dict_with_tounicode(&mut (*args).curptr, (*args).endptr, &mut (*sd).cd);
    if dict.is_null() {
        spc_warn!(spe, "Dictionary object expected but not found.");
        return -1i32;
    }
    let catalog = pdf_doc_get_dictionary("Catalog");
    /* Avoid overriding whole ViewerPreferences */
    let pref_old = pdf_lookup_dict(catalog, "ViewerPreferences"); /* Close all? */
    let pref_add = pdf_lookup_dict(dict, "ViewerPreferences");
    if let (Some(pref_old), Some(pref_add)) = (pref_old, pref_add) {
        pdf_merge_dict(pref_old, pref_add);
        pdf_remove_dict(dict, "ViewerPreferences");
    }
    pdf_merge_dict(catalog, dict);
    pdf_release_obj(dict);
    0i32
}
unsafe fn spc_handler_pdfm_close(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if !ident.is_null() {
        spc_flush_object(ident);
        free(ident as *mut libc::c_void);
    } else {
        spc_clear_objects();
    }
    0i32
}
unsafe fn spc_handler_pdfm_object(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if ident.is_null() {
        spc_warn!(spe, "Could not find a object identifier.");
        return -1i32;
    }
    let object = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if object.is_null() {
        spc_warn!(
            spe,
            "Could not find an object definition for \"{}\".",
            CStr::from_ptr(ident).display(),
        );
        free(ident as *mut libc::c_void);
        return -1i32;
    } else {
        spc_push_object(ident, object);
    }
    free(ident as *mut libc::c_void);
    0i32
}
unsafe fn spc_handler_pdfm_content(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut len = 0;
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr < (*args).endptr {
        let mut M = pdf_tmatrix {
            a: 1.,
            b: 0.,
            c: 0.,
            d: 1.,
            e: (*spe).x_user,
            f: (*spe).y_user,
        };
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
        len = (*args).endptr.wrapping_offset_from((*args).curptr) as usize; /* op: ANY */
        pdf_doc_add_page_content_ptr((*args).curptr, len as u32); /* op: */
        pdf_doc_add_page_content(b" Q");
        /* op: ANY */
    } /* op: */
    (*args).curptr = (*args).endptr; /* op: ANY */
    return 0i32; /*kpse_find_pict(instring);*/
}
unsafe fn spc_handler_pdfm_literal(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut direct: i32 = 0i32;
    skip_white(&mut (*args).curptr, (*args).endptr);
    while (*args).curptr < (*args).endptr {
        if (*args).curptr.offset(7) <= (*args).endptr
            && !strstartswith((*args).curptr, b"reverse\x00" as *const u8 as *const i8).is_null()
        {
            (*args).curptr = (*args).curptr.offset(7);
            warn!("The special \"pdf:literal reverse ...\" is no longer supported.\nIgnore the \"reverse\" option.");
        } else {
            if !((*args).curptr.offset(6) <= (*args).endptr
                && !strstartswith((*args).curptr, b"direct\x00" as *const u8 as *const i8)
                    .is_null())
            {
                break;
            }
            direct = 1i32;
            (*args).curptr = (*args).curptr.offset(6)
        }
        skip_white(&mut (*args).curptr, (*args).endptr);
    }
    if (*args).curptr < (*args).endptr {
        let mut M = pdf_tmatrix::new();
        if direct == 0 {
            M.d = 1.0f64;
            M.a = M.d;
            M.c = 0.0f64;
            M.b = M.c;
            M.e = (*spe).x_user;
            M.f = (*spe).y_user;
            pdf_dev_concat(&mut M);
        }
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content_ptr(
            (*args).curptr,
            (*args).endptr.wrapping_offset_from((*args).curptr) as i64 as i32 as u32,
        );
        if direct == 0 {
            M.e = -(*spe).x_user;
            M.f = -(*spe).y_user;
            pdf_dev_concat(&mut M);
        }
    }
    (*args).curptr = (*args).endptr;
    0i32
}
unsafe fn spc_handler_pdfm_bcontent(mut spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    let mut xpos: f64 = 0.;
    let mut ypos: f64 = 0.;
    pdf_dev_gsave();
    pdf_dev_get_coord(&mut xpos, &mut ypos);
    let mut M = pdf_tmatrix {
        a: 1.,
        b: 0.,
        c: 0.,
        d: 1.,
        e: (*spe).x_user - xpos,
        f: (*spe).y_user - ypos,
    };
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
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr < (*args).endptr {
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content_ptr(
            (*args).curptr,
            (*args).endptr.wrapping_offset_from((*args).curptr) as i64 as i32 as u32,
        );
        (*args).curptr = (*args).endptr
    }
    0i32
}
unsafe fn spc_handler_pdfm_do_nothing(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).curptr = (*args).endptr;
    0i32
}
unsafe fn spc_handler_pdfm_stream_with_type(
    mut spe: *mut spc_env,
    mut args: *mut spc_arg,
    mut type_0: i32,
) -> i32 {
    let fstream;
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if ident.is_null() {
        spc_warn!(spe, "Missing objname for pdf:(f)stream.");
        return -1i32;
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    let tmp = parse_pdf_object(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
    if tmp.is_null() {
        spc_warn!(spe, "Missing input string for pdf:(f)stream.");
        free(ident as *mut libc::c_void);
        return -1i32;
    } else {
        if !(!tmp.is_null() && (*tmp).is_string()) {
            spc_warn!(spe, "Invalid type of input string for pdf:(f)stream.");
            pdf_release_obj(tmp);
            free(ident as *mut libc::c_void);
            return -1i32;
        }
    }
    let instring = pdf_string_value(tmp) as *mut i8;
    match type_0 {
        1 => {
            if instring.is_null() {
                spc_warn!(spe, "Missing filename for pdf:fstream.");
                pdf_release_obj(tmp);
                free(ident as *mut libc::c_void);
                return -1i32;
            }
            let fullname = 0 as *mut i8;
            if fullname.is_null() {
                spc_warn!(
                    spe,
                    "File \"{}\" not found.",
                    CStr::from_ptr(instring).display(),
                );
                pdf_release_obj(tmp);
                free(ident as *mut libc::c_void);
                return -1i32;
            }
            let handle =
                ttstub_input_open(fullname, TTInputFormat::PICT, 0i32);
            if handle.is_none() {
                spc_warn!(
                    spe,
                    "Could not open file: {}",
                    CStr::from_ptr(instring).display(),
                );
                pdf_release_obj(tmp);
                free(ident as *mut libc::c_void);
                free(fullname as *mut libc::c_void);
                return -1i32;
            }
            let mut handle = handle.unwrap();
            fstream = pdf_new_stream(1i32 << 0i32);
            loop {
                let nb_read = handle.read(&mut WORK_BUFFER[..]).unwrap();
                if !(nb_read > 0) { // TODO: check
                    break;
                }
                pdf_add_stream(
                    fstream,
                    WORK_BUFFER.as_mut_ptr() as *const libc::c_void,
                    nb_read as i32,
                );
            }
            ttstub_input_close(handle);
            free(fullname as *mut libc::c_void);
        }
        0 => {
            fstream = pdf_new_stream(1i32 << 0i32);
            if !instring.is_null() {
                pdf_add_stream(
                    fstream,
                    instring as *const libc::c_void,
                    strlen(instring) as i32,
                );
            }
        }
        _ => {
            pdf_release_obj(tmp);
            free(ident as *mut libc::c_void);
            return -1i32;
        }
    }
    pdf_release_obj(tmp);
    /*
     * Optional dict.
     *
     *  TODO: check Length, Filter...
     */
    skip_white(&mut (*args).curptr, (*args).endptr);
    if *(*args).curptr.offset(0) as i32 == '<' as i32 {
        let stream_dict = pdf_stream_dict(fstream);
        let tmp = parse_pdf_dict(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
        if tmp.is_null() {
            spc_warn!(spe, "Parsing dictionary failed.");
            pdf_release_obj(fstream);
            free(ident as *mut libc::c_void);
            return -1i32;
        }
        if pdf_lookup_dict(tmp, "Length").is_some() {
            pdf_remove_dict(tmp, "Length");
        } else if pdf_lookup_dict(tmp, "Filter").is_some() {
            pdf_remove_dict(tmp, "Filter");
        }
        pdf_merge_dict(stream_dict, tmp);
        pdf_release_obj(tmp);
    }
    /* Users should explicitly close this. */
    spc_push_object(ident, fstream);
    free(ident as *mut libc::c_void);
    0i32
}
/*
 * STREAM: Create a PDF stream object from an input string.
 *
 *  pdf: stream @objname (input_string) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_stream(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    spc_handler_pdfm_stream_with_type(spe, args, 0i32)
}
/*
 * FSTREAM: Create a PDF stream object from an existing file.
 *
 *  pdf: fstream @objname (filename) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_fstream(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
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
unsafe fn spc_handler_pdfm_bform(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut cropbox = pdf_rect::new();
    let mut ti = transform_info::new();
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if ident.is_null() {
        spc_warn!(spe, "A form XObject must have name.");
        return -1i32;
    }
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
        free(ident as *mut libc::c_void);
        return -1i32;
    }
    /* A XForm with zero dimension results in a non-invertible transformation
     * matrix. And it may result in unpredictable behaviour. It might be an
     * error in Acrobat. Bounding box with zero dimension may cause division
     * by zero.
     */
    if ti.flags & 1i32 << 0i32 != 0 {
        if ti.bbox.urx - ti.bbox.llx == 0.0f64 || ti.bbox.ury - ti.bbox.lly == 0.0f64 {
            spc_warn!(spe, "Bounding box has a zero dimension.");
            free(ident as *mut libc::c_void);
            return -1i32;
        }
        cropbox.llx = ti.bbox.llx;
        cropbox.lly = ti.bbox.lly;
        cropbox.urx = ti.bbox.urx;
        cropbox.ury = ti.bbox.ury
    } else {
        if ti.width == 0.0f64 || ti.depth + ti.height == 0.0f64 {
            spc_warn!(spe, "Bounding box has a zero dimension.");
            free(ident as *mut libc::c_void);
            return -1i32;
        }
        cropbox.llx = 0.0f64;
        cropbox.lly = -ti.depth;
        cropbox.urx = ti.width;
        cropbox.ury = ti.height
    }
    let xobj_id = pdf_doc_begin_grabbing(ident, (*spe).x_user, (*spe).y_user, &mut cropbox);
    if xobj_id < 0i32 {
        free(ident as *mut libc::c_void);
        spc_warn!(spe, "Couldn\'t start form object.");
        return -1i32;
    }
    spc_push_object(ident, pdf_ximage_get_reference(xobj_id));
    free(ident as *mut libc::c_void);
    0i32
}
/* An extra dictionary after exobj must be merged to the form dictionary,
 * not resource dictionary.
 * Please use pdf:put @resources (before pdf:exobj) instead.
 */
unsafe fn spc_handler_pdfm_eform(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut attrib: *mut pdf_obj = 0 as *mut pdf_obj;
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr < (*args).endptr {
        attrib = parse_pdf_dict(&mut (*args).curptr, (*args).endptr, 0 as *mut pdf_file);
        if !attrib.is_null() && !(!attrib.is_null() && (*attrib).is_dict()) {
            pdf_release_obj(attrib);
            attrib = 0 as *mut pdf_obj
        }
    }
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
unsafe fn spc_handler_pdfm_uxobj(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    let mut ti = transform_info::new();
    let mut options: load_options = {
        let mut init = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: 0 as *mut pdf_obj,
        };
        init
    };
    skip_white(&mut (*args).curptr, (*args).endptr);
    let ident = parse_opt_ident(&mut (*args).curptr, (*args).endptr);
    if ident.is_null() {
        spc_warn!(spe, "No object identifier given.");
        return -1i32;
    }
    transform_info_clear(&mut ti);
    if (*args).curptr < (*args).endptr {
        if spc_util_read_dimtrns(spe, &mut ti, args, 0i32) < 0i32 {
            free(ident as *mut libc::c_void);
            return -1i32;
        }
    }
    /* Dvipdfmx was suddenly changed to use file name to identify
     * external images. We can't use ident to find image resource
     * here.
     */
    let mut xobj_id = findresource(sd, ident);
    if xobj_id < 0i32 {
        xobj_id = pdf_ximage_findresource(ident, options);
        if xobj_id < 0i32 {
            spc_warn!(
                spe,
                "Specified (image) object doesn\'t exist: {}",
                CStr::from_ptr(ident).display(),
            );
            free(ident as *mut libc::c_void);
            return -1i32;
        }
    }
    pdf_dev_put_image(xobj_id, &mut ti, (*spe).x_user, (*spe).y_user);
    free(ident as *mut libc::c_void);
    0i32
}
unsafe fn spc_handler_pdfm_link(mut spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    spc_resume_annot(spe)
}
unsafe fn spc_handler_pdfm_nolink(mut spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    spc_suspend_annot(spe)
}
/* Handled at BOP */
unsafe fn spc_handler_pdfm_pagesize(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).curptr = (*args).endptr;
    0i32
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_pdfm_bgcolor(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
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
unsafe fn spc_handler_pdfm_mapline(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    static mut BUFFER: [i8; 1024] = [0; 1024];
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    if (*ap).curptr >= (*ap).endptr {
        spc_warn!(spe, "Empty mapline special?");
        return -1i32;
    }
    let opchr = *(*ap).curptr.offset(0);
    if opchr as i32 == '-' as i32 || opchr as i32 == '+' as i32 {
        (*ap).curptr = (*ap).curptr.offset(1)
    }
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    match opchr as i32 {
        45 => {
            let map_name = parse_ident(&mut (*ap).curptr, (*ap).endptr);
            if !map_name.is_null() {
                pdf_remove_fontmap_record(map_name);
                free(map_name as *mut libc::c_void);
            } else {
                spc_warn!(spe, "Invalid fontmap line: Missing TFM name.");
                error = -1i32
            }
        }
        _ => {
            let mut p = (*ap).curptr;
            let mut q = BUFFER.as_mut_ptr();
            while p < (*ap).endptr {
                let fresh11 = p;
                p = p.offset(1);
                let fresh12 = q;
                q = q.offset(1);
                *fresh12 = *fresh11
            }
            *q = '\u{0}' as i32 as i8;
            let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                as *mut fontmap_rec;
            pdf_init_fontmap_record(mrec);
            error = pdf_read_fontmap_line(
                mrec,
                BUFFER.as_mut_ptr(),
                (*ap).endptr.wrapping_offset_from((*ap).curptr) as i64 as i32,
                is_pdfm_mapline(BUFFER.as_mut_ptr()),
            );
            if error != 0 {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr as i32 == '+' as i32 {
                pdf_append_fontmap_record((*mrec).map_name, mrec);
            } else {
                pdf_insert_fontmap_record((*mrec).map_name, mrec);
            }
            pdf_clear_fontmap_record(mrec);
            free(mrec as *mut libc::c_void);
        }
    }
    if error == 0 {
        (*ap).curptr = (*ap).endptr
    }
    0i32
}
unsafe fn spc_handler_pdfm_mapfile(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut error;
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr >= (*args).endptr {
        return 0i32;
    }
    let mode = match *(*args).curptr.offset(0) as i32 {
        45 => {
            (*args).curptr = (*args).curptr.offset(1);
            '-' as i32
        }
        43 => {
            (*args).curptr = (*args).curptr.offset(1);
            '+' as i32
        }
        _ => 0,
    };
    let mapfile = parse_val_ident(&mut (*args).curptr, (*args).endptr);
    if mapfile.is_null() {
        spc_warn!(spe, "No fontmap file specified.");
        return -1i32;
    } else {
        error = pdf_load_fontmap_file(mapfile, mode)
    }
    free(mapfile as *mut libc::c_void);
    error
}
unsafe fn spc_handler_pdfm_tounicode(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut sd: *mut spc_pdf_ = &mut _PDF_STAT;
    /* First clear */
    (*sd).cd.cmap_id = -1i32;
    (*sd).cd.unescape_backslash = 0i32;
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr >= (*args).endptr {
        spc_warn!(spe, "Missing CMap name for pdf:tounicode.");
        return -1i32;
    }
    /* _FIXME_
     * Any valid char allowed for PDF name object should be allowed here.
     * The argument to this special should be a PDF name obejct.
     * But it's too late to change this special.
     */
    let cmap_name = parse_ident(&mut (*args).curptr, (*args).endptr);
    if cmap_name.is_null() {
        spc_warn!(spe, "Missing ToUnicode mapping name...");
        return -1i32;
    }
    (*sd).cd.cmap_id = CMap_cache_find(cmap_name);
    if (*sd).cd.cmap_id < 0i32 {
        spc_warn!(
            spe,
            "Failed to load ToUnicode mapping: {}",
            CStr::from_ptr(cmap_name).display(),
        );
        free(cmap_name as *mut libc::c_void);
        return -1i32;
    }
    /* Shift-JIS like encoding may contain backslash in 2nd byte.
     * WARNING: This will add nasty extension to PDF parser.
     */
    if (*sd).cd.cmap_id >= 0i32 {
        if !strstr(cmap_name, b"RKSJ\x00" as *const u8 as *const i8).is_null()
            || !strstr(cmap_name, b"B5\x00" as *const u8 as *const i8).is_null()
            || !strstr(cmap_name, b"GBK\x00" as *const u8 as *const i8).is_null()
            || !strstr(cmap_name, b"KSC\x00" as *const u8 as *const i8).is_null()
        {
            (*sd).cd.unescape_backslash = 1i32
        }
    }
    free(cmap_name as *mut libc::c_void);
    0i32
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
#[no_mangle]
pub unsafe extern "C" fn spc_pdfm_check_special(mut buf: *const i8, mut len: i32) -> bool {
    let mut p = buf;
    let endptr = p.offset(len as isize);
    skip_white(&mut p, endptr);
    if p.offset(strlen(b"pdf:\x00" as *const u8 as *const i8) as isize) <= endptr
        && memcmp(
            p as *const libc::c_void,
            b"pdf:\x00" as *const u8 as *const i8 as *const libc::c_void,
            strlen(b"pdf:\x00" as *const u8 as *const i8),
        ) == 0
    {
        return true;
    }
    false
}
#[no_mangle]
pub unsafe extern "C" fn spc_pdfm_setup_handler(
    mut sph: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    if (*ap)
        .curptr
        .offset(strlen(b"pdf:\x00" as *const u8 as *const i8) as isize)
        >= (*ap).endptr
        || memcmp(
            (*ap).curptr as *const libc::c_void,
            b"pdf:\x00" as *const u8 as *const i8 as *const libc::c_void,
            strlen(b"pdf:\x00" as *const u8 as *const i8),
        ) != 0
    {
        spc_warn!(spe, "Not pdf: special???");
        return -1i32;
    }
    (*ap).curptr = (*ap)
        .curptr
        .offset(strlen(b"pdf:\x00" as *const u8 as *const i8) as isize);
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        for handler in PDFM_HANDLERS.iter() {
            if CStr::from_ptr(q).to_bytes() == handler.key {
                (*ap).command = Some(handler.key);
                (*sph).key = b"pdf:";
                (*sph).exec = handler.exec;
                skip_white(&mut (*ap).curptr, (*ap).endptr);
                error = 0i32;
                break;
            }
        }
        free(q as *mut libc::c_void);
    }
    error
}
