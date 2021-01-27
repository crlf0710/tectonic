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
#![allow(non_camel_case_types, non_snake_case)]

use euclid::point2;
use once_cell::sync::Lazy;
use std::collections::HashMap;

use std::io::Read;
use std::ptr;

use crate::bridge::DisplayExt;
use crate::bridge::TTInputFormat;
use crate::{spc_warn, warn};

use super::util::{spc_util_read_blahblah, spc_util_read_dimtrns, spc_util_read_pdfcolor};
use super::{
    spc_begin_annot, spc_clear_objects, spc_end_annot, spc_flush_object, spc_lookup_object,
    spc_push_object, spc_resume_annot, spc_suspend_annot,
};

use crate::bridge::{size_t, InFile};
use crate::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_dvipdfmx::is_xdv;
use crate::dpx_fontmap::{
    is_pdfm_mapline, pdf_append_fontmap_record, pdf_init_fontmap_record, pdf_insert_fontmap_record,
    pdf_load_fontmap_file, pdf_read_fontmap_line, pdf_remove_fontmap_record,
};
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
    pdf_dict, pdf_link_obj, pdf_name, pdf_obj, pdf_release_obj, pdf_remove_dict, pdf_stream,
    pdf_string, IntoObj, Object, STREAM_COMPRESS,
};
use crate::dpx_pdfparse::{ParseIdent, ParsePdfObj, SkipWhite};
use crate::dpx_pdfximage::{pdf_ximage_findresource, pdf_ximage_get_reference};
use crate::dpx_unicode::{
    UC_UTF16BE_encode_char, UC_UTF16BE_is_valid_string, UC_UTF8_decode_char,
    UC_UTF8_is_valid_string, UC_is_valid,
};

use super::{SpcArg, SpcEnv};

use super::SpcHandler;
#[derive(Clone)]
pub(crate) struct spc_pdf_ {
    pub(crate) annot_dict: *mut pdf_obj,
    pub(crate) lowest_level: i32,
    pub(crate) resourcemap: HashMap<String, resource_map>,
    pub(crate) cd: tounicode,
    /* quasi-hack to get the primary input */
    /* For to-UTF16-BE conversion :( */
}
impl spc_pdf_ {
    pub(crate) fn new() -> Self {
        Self {
            annot_dict: ptr::null_mut(),
            lowest_level: 255,
            resourcemap: HashMap::new(),
            cd: tounicode {
                cmap_id: -1,
                unescape_backslash: 0,
                taintkeys: ptr::null_mut(),
            },
        }
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tounicode {
    pub(crate) cmap_id: i32,
    pub(crate) unescape_backslash: i32,
    pub(crate) taintkeys: *mut pdf_obj,
    /* An array of PDF names. */
}

use crate::dpx_pdfximage::load_options;

/* PLEASE REMOVE THIS */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct resource_map {
    pub(crate) type_0: i32,
    pub(crate) res_id: i32,
}
use crate::dpx_cmap::CMap;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

pub(crate) static mut _PDF_STAT: Lazy<spc_pdf_> = Lazy::new(|| spc_pdf_::new());

unsafe fn addresource(sd: &mut spc_pdf_, ident: &str, res_id: i32) -> i32 {
    if ident.is_empty() || res_id < 0i32 {
        return -1i32;
    }
    let r = resource_map { type_0: 0, res_id };
    sd.resourcemap.insert(ident.to_string(), r);
    spc_push_object(ident, pdf_ximage_get_reference(res_id));
    0i32
}
unsafe fn findresource(sd: &mut spc_pdf_, ident: Option<&String>) -> i32 {
    if let Some(ident) = ident {
        if let Some(r) = sd.resourcemap.get(ident) {
            r.res_id
        } else {
            -1
        }
    } else {
        -1
    }
}
unsafe fn spc_handler_pdfm__init(sd: &mut spc_pdf_) -> i32 {
    /* The folllowing dictionary entry keys are considered as keys for
     * text strings. Be sure that string object is NOT always a text string.
     */
    const DEFAULT_TAINTKEYS: [&str; 11] = [
        "Title", "Author", "Subject", "Keywords", "Creator", "Producer", "Contents", "Subj", "TU",
        "T", "TM",
    ];
    sd.annot_dict = ptr::null_mut();
    sd.lowest_level = 255;
    sd.resourcemap.clear();
    let array: Vec<*mut pdf_obj> = DEFAULT_TAINTKEYS
        .iter()
        .map(|&key| key.into_obj())
        .collect();
    sd.cd.taintkeys = array.into_obj();
    0i32
}
unsafe fn spc_handler_pdfm__clean(sd: &mut spc_pdf_) -> i32 {
    if !sd.annot_dict.is_null() {
        warn!("Unbalanced bann and eann found.");
        pdf_release_obj(sd.annot_dict);
    }
    sd.lowest_level = 255;
    sd.annot_dict = ptr::null_mut();
    sd.resourcemap.clear();
    pdf_release_obj(sd.cd.taintkeys);
    sd.cd.taintkeys = ptr::null_mut();
    0i32
}

pub(crate) unsafe fn spc_pdfm_at_begin_document() -> i32 {
    let sd = &mut _PDF_STAT;
    spc_handler_pdfm__init(sd)
}

pub(crate) unsafe fn spc_pdfm_at_end_document() -> i32 {
    let sd = &mut _PDF_STAT;
    spc_handler_pdfm__clean(sd)
}
/* Dvipdfm specials */
unsafe fn spc_handler_pdfm_bop(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    if !args.cur.is_empty() {
        pdf_doc_set_bop_content(&args.cur);
    }
    args.cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_eop(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    if !args.cur.is_empty() {
        pdf_doc_set_eop_content(&args.cur);
    }
    args.cur = &[];
    0i32
}
/* Why should we have this kind of things? */
unsafe fn safeputresdent(kp: &pdf_name, vp: &mut pdf_obj, dp: &mut pdf_dict) -> i32 {
    let key = kp.to_bytes();
    if dp.has(key) {
        warn!(
            "Object \"{}\" already defined in dict! (ignored)",
            key.display()
        );
    } else {
        dp.set(key, pdf_link_obj(vp));
    }
    0i32
}
unsafe fn safeputresdict(kp: &pdf_name, vp: &mut pdf_obj, dp: &mut pdf_dict) -> i32 {
    let key = kp.to_bytes();
    let dict = dp.get_mut(key);
    match &mut vp.data {
        Object::Indirect(_) => {
            dp.set(key, pdf_link_obj(vp));
        }
        Object::Dict(vpd) => {
            if let Some(dict) = dict {
                vpd.foreach(safeputresdent, dict.as_dict_mut());
            } else {
                dp.set(key, pdf_link_obj(vp));
            }
        }
        _ => {
            warn!(
                "Invalid type (not DICT) for page/form resource dict entry: key=\"{}\"",
                key.display(),
            );
            return -1i32;
        }
    }
    0i32
}
/* Think what happens if you do
 *
 *  pdf:put @resources << /Font << >> >>
 *
 */
unsafe fn spc_handler_pdfm_put(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let mut error: i32 = 0i32;
    ap.cur.skip_white();
    let ident = ap.cur.parse_opt_ident();
    if ident.is_none() {
        spc_warn!(spe, "Missing object identifier.");
        return -1i32;
    }
    let ident = ident.unwrap();
    let obj1 = spc_lookup_object(&ident); /* put obj2 into obj1 */
    if obj1.is_null() {
        spc_warn!(spe, "Specified object not exist: {}", ident);
        return -1i32;
    }
    ap.cur.skip_white();
    let obj2 = ap.cur.parse_pdf_object(ptr::null_mut());
    if obj2.is_none() {
        spc_warn!(spe, "Missing (an) object(s) to put into \"{}\"!", ident);
        return -1i32;
    }
    let obj2 = obj2.unwrap();
    match &mut (*obj1).data {
        Object::Dict(d1) => {
            if let Object::Dict(d2) = &mut (*obj2).data {
                if ident == "resources" {
                    error = d2.foreach(safeputresdict, d1);
                } else {
                    d1.merge(d2);
                }
            } else {
                spc_warn!(
                    spe,
                    "Inconsistent object type for \"put\" (expecting DICT): {}",
                    ident,
                );
                error = -1i32
            }
        }
        Object::Stream(obj1) => match &(*obj2).data {
            Object::Dict(d) => {
                obj1.get_dict_mut().merge(d);
            }
            Object::Stream(_) => {
                spc_warn!(
                    spe,
                    "\"put\" operation not supported for STREAM <- STREAM: {}",
                    ident,
                );
                error = -1;
            }
            _ => {
                spc_warn!(spe, "Invalid type: expecting a DICT or STREAM: {}", ident);
                error = -1;
            }
        },
        Object::Array(obj1) => {
            /* dvipdfm */
            obj1.push(pdf_link_obj(obj2));
            while !ap.cur.is_empty() {
                if let Some(obj3) = ap.cur.parse_pdf_object(ptr::null_mut()) {
                    obj1.push(obj3);
                    ap.cur.skip_white();
                } else {
                    break;
                }
            }
        }
        _ => {
            spc_warn!(
                spe,
                "Can\'t \"put\" object into non-DICT/STREAM/ARRAY type object: {}",
                ident,
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
unsafe fn reencodestring(cmap: *mut CMap, instring: *mut pdf_string) -> i32 {
    let mut wbuf: [u8; 4096] = [0; 4096];
    if cmap.is_null() || instring.is_null() {
        return 0i32;
    }
    let slice = (*instring).to_bytes();
    let mut inbufleft = slice.len() as size_t;
    let mut inbufcur = slice.as_ptr() as *const u8;
    wbuf[0] = 0xfe_u8;
    wbuf[1] = 0xff_u8;
    let mut obufcur = wbuf.as_mut_ptr().offset(2);
    let mut obufleft = (4096i32 - 2i32) as size_t;
    CMap_decode(
        &*cmap,
        &mut inbufcur,
        &mut inbufleft,
        &mut obufcur,
        &mut obufleft,
    );
    if inbufleft > 0 {
        return -1i32;
    }
    (*instring).set(&wbuf[..(4096_usize - obufleft as usize)]);
    0i32
}
unsafe fn maybe_reencode_utf8(instring: *mut pdf_string) -> i32 {
    let mut non_ascii: i32 = 0i32;
    let mut cp: *const u8;
    let mut wbuf: [u8; 4096] = [0; 4096];
    if instring.is_null() {
        return 0i32;
    }
    let slice = (*instring).as_mut_slice();
    let inlen = slice.len() as i32;
    let inbuf = slice.as_mut_ptr() as *mut u8;
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
    *op = 0xfe_u8;
    op = op.offset(1);
    *op = 0xff_u8;
    op = op.offset(1);
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
    (*instring).set(&wbuf[..(op.offset_from(wbuf.as_ptr()) as usize)]);
    0i32
}
/* The purpose of this routine is to check if given string object is
 * surely an object for *text* strings. It does not do a complete check
 * but does a quick check. Please add entries for taintkeys if you have found
 * additional dictionary entries which is considered as a text string.
 */
unsafe fn needreencode(kp: &pdf_name, vp: &pdf_string, cd: &tounicode) -> i32 {
    let mut r = 0;
    assert!(!cd.taintkeys.is_null());
    for i in 0..(*cd.taintkeys).as_array().len() {
        let tk = (*cd.taintkeys).as_array()[i];
        if let Object::Name(tk) = &(*tk).data {
            if kp.to_bytes() == tk.to_bytes() {
                r = 1;
                break;
            }
        } else {
            panic!();
        }
    }
    if r != 0 {
        /* Check UTF-16BE BOM. */
        if vp.to_bytes().starts_with(b"\xfe\xff") {
            r = 0
        }
    } /* continue */
    r
}
unsafe fn modstrings(kp: &pdf_name, vp: &mut pdf_obj, cd: &mut tounicode) -> i32 {
    let mut r: i32 = 0i32;
    match &mut vp.data {
        Object::String(vp) => {
            if cd.cmap_id >= 0i32 && !cd.taintkeys.is_null() {
                let cmap: *mut CMap = CMap_cache_get(cd.cmap_id);
                if needreencode(kp, vp, cd) != 0 {
                    r = reencodestring(cmap, vp)
                }
            } else if is_xdv != 0 && !cd.taintkeys.is_null() {
                /* Please fix this... PDF string object is not always a text string.
                 * needreencode() is assumed to do a simple check if given string
                 * object is actually a text string.
                 */
                if needreencode(kp, &*vp, cd) != 0 {
                    r = maybe_reencode_utf8(vp)
                }
            }
            if r < 0i32 {
                /* error occured... */
                warn!("Failed to convert input string to UTF16...");
            }
        }
        Object::Dict(vp) => r = vp.foreach(modstrings, cd),
        Object::Stream(vp) => r = vp.get_dict_mut().foreach(modstrings, cd),
        _ => {}
    }
    r
}

pub(crate) trait ParsePdfDictU {
    fn parse_pdf_dict_with_tounicode(&mut self, cd: &mut tounicode) -> Option<pdf_dict>;
}

impl ParsePdfDictU for &[u8] {
    fn parse_pdf_dict_with_tounicode(&mut self, cd: &mut tounicode) -> Option<pdf_dict> {
        /* disable this test for XDV files, as we do UTF8 reencoding with no cmap */
        if unsafe { is_xdv == 0 && cd.cmap_id < 0 } {
            return self.parse_pdf_dict(ptr::null_mut());
        }
        /* :( */
        let mut dict = if unsafe { cd.unescape_backslash != 0 } {
            self.parse_pdf_tainted_dict()
        } else {
            self.parse_pdf_dict(ptr::null_mut())
        };
        if let Some(d) = &mut dict {
            unsafe {
                d.foreach(modstrings, cd);
            }
        }
        dict
    }
}

unsafe fn spc_handler_pdfm_annot(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    let mut rect = Rect::zero();
    let mut ident = None;
    args.cur.skip_white();
    if args.cur[0] == b'@' {
        ident = args.cur.parse_opt_ident();
        args.cur.skip_white();
    }

    let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
        ti
    } else {
        return -1;
    };

    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return -1i32;
    }
    let annot_dict = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd);
    if annot_dict.is_none() {
        spc_warn!(spe, "Could not find dictionary object.");
        return -1i32;
    }
    let annot_dict = annot_dict.unwrap().into_obj();
    let mut cp = point2(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut cp, None);
    if ti.flags & 1i32 << 0i32 != 0 {
        rect = ti.bbox.translate(cp.to_vector());
    } else {
        rect.min.x = cp.x;
        rect.min.y = cp.y - spe.mag * ti.depth;
        rect.max.x = cp.x + spe.mag * ti.width;
        rect.max.y = cp.y + spe.mag * ti.height
    }
    /* Order is important... */
    if let Some(i) = ident.as_ref() {
        spc_push_object(i, pdf_link_obj(annot_dict));
    }
    /* Add this reference. */
    pdf_doc_add_annot(
        pdf_doc_current_page_number() as usize,
        &mut rect,
        annot_dict,
        1i32,
    );
    if let Some(i) = ident {
        spc_flush_object(&i);
    }
    pdf_release_obj(annot_dict);
    0
}
/* NOTE: This can't have ident. See "Dvipdfm User's Manual". */
unsafe fn spc_handler_pdfm_bann(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    if !sd.annot_dict.is_null() {
        spc_warn!(spe, "Can\'t begin an annotation when one is pending.");
        return -1i32;
    }
    args.cur.skip_white();
    if let Some(annot_dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        sd.annot_dict = annot_dict.into_obj();
    } else {
        sd.annot_dict = ptr::null_mut();
        spc_warn!(spe, "Ignoring annotation with invalid dictionary.");
        return -1i32;
    }
    spc_begin_annot(spe, sd.annot_dict)
}
unsafe fn spc_handler_pdfm_eann(spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    if sd.annot_dict.is_null() {
        spc_warn!(spe, "Tried to end an annotation without starting one!");
        return -1i32;
    }
    let error = spc_end_annot(spe);
    pdf_release_obj(sd.annot_dict);
    sd.annot_dict = ptr::null_mut();
    error
}
/* Color:.... */
unsafe fn spc_handler_pdfm_bcolor(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if !ap.cur.is_empty() {
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
unsafe fn spc_handler_pdfm_scolor(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let (psc, pfc) = pdf_color_get_current();
    let fc = spc_util_read_pdfcolor(spe, ap, Some(pfc));
    let mut sc = Err(());
    if let Ok(ref fc) = fc {
        sc = if !ap.cur.is_empty() {
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
unsafe fn spc_handler_pdfm_ecolor(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    pdf_color_pop();
    0i32
}
unsafe fn spc_handler_pdfm_btrans(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
        ti
    } else {
        return -1;
    };
    /* Create transformation matrix */
    let mut M = ti.matrix.clone();
    M.m31 += (1. - M.m11) * spe.x_user - M.m21 * spe.y_user;
    M.m32 += (1. - M.m22) * spe.y_user - M.m12 * spe.x_user;
    pdf_dev_gsave();
    pdf_dev_concat(&mut M);
    0i32
}
unsafe fn spc_handler_pdfm_etrans(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
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
unsafe fn spc_handler_pdfm_outline(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    let mut is_open: i32 = -1i32;
    args.cur.skip_white();
    /*
     * pdf:outline is extended to support open/close feature
     *
     * pdf:outline 1 ... (as DVIPDFM)
     * pdf:outline [] 1 ... (open bookmark)
     * pdf:outline [-] 1 ... (closed bookmark)
     */
    if args.cur.len() > 3 && args.cur[0] == b'[' {
        args.cur = &args.cur[1..];
        if args.cur[0] == b'-' {
            args.cur = &args.cur[1..];
        } else {
            is_open = 1i32
        }
        args.cur = &args.cur[1..];
    }
    args.cur.skip_white();
    let mut level = if let Some(tmp) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::Number(level) = (*tmp).data {
            pdf_release_obj(tmp);
            level as i32
        } else {
            pdf_release_obj(tmp);
            spc_warn!(spe, "Expecting number for outline item depth.");
            return -1i32;
        }
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
    sd.lowest_level = if sd.lowest_level < level {
        sd.lowest_level
    } else {
        level
    };
    level += 1i32 - sd.lowest_level;
    let item_dict = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd);
    if item_dict.is_none() {
        spc_warn!(spe, "Ignoring invalid dictionary.");
        return -1i32;
    }
    let item_dict = item_dict.unwrap();
    let mut current_depth = pdf_doc_bookmarks_depth();
    if current_depth > level {
        while current_depth > level {
            current_depth -= 1;
            pdf_doc_bookmarks_up();
        }
    } else if current_depth < level {
        while current_depth < level {
            current_depth += 1;
            pdf_doc_bookmarks_down();
        }
    }
    pdf_doc_bookmarks_add(&mut *item_dict.into_obj(), is_open);
    0i32
}
unsafe fn spc_handler_pdfm_article(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        if let Some(info_dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
            let info_dict = info_dict.into_obj();
            pdf_doc_begin_article(&ident, pdf_link_obj(info_dict));
            spc_push_object(&ident, info_dict);
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
unsafe fn spc_handler_pdfm_bead(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    args.cur.skip_white();
    if args.cur[0] != b'@' {
        spc_warn!(spe, "Article identifier expected but not found.");
        return -1i32;
    }
    let article_name = args.cur.parse_opt_ident();
    if article_name.is_none() {
        spc_warn!(spe, "Article reference expected but not found.");
        return -1i32;
    }
    let article_name = article_name.unwrap();
    /* If okay so far, try to get a bounding box */
    let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
        ti
    } else {
        return -1;
    };
    if ti.flags & 1i32 << 0i32 != 0
        && (ti.flags & 1i32 << 1i32 != 0 || ti.flags & 1i32 << 2i32 != 0)
    {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return -1i32;
    }
    let mut cp = point2(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut cp, None);
    let mut rect = if ti.flags & 1i32 << 0i32 != 0 {
        ti.bbox.translate(cp.to_vector())
    } else {
        Rect::new(
            point2(cp.x, cp.y - spe.mag * ti.depth),
            point2(cp.x + spe.mag * ti.width, cp.y + spe.mag * ti.height),
        )
    };
    args.cur.skip_white();
    let article_info;
    if args.cur[0] != b'<' {
        article_info = pdf_dict::new();
    } else {
        if let Some(ai) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
            article_info = ai;
        } else {
            spc_warn!(spe, "Error in reading dictionary.");
            return -1i32;
        }
    }
    /* Does this article exist yet */
    let article = spc_lookup_object(&article_name);
    if !article.is_null() {
        (*article).as_dict_mut().merge(&article_info);
    } else {
        let article_info = article_info.into_obj();
        pdf_doc_begin_article(&article_name, pdf_link_obj(article_info));
        spc_push_object(&article_name, article_info);
    }
    let page_no = pdf_doc_current_page_number();
    pdf_doc_add_bead(&article_name, "", page_no, &mut rect);
    0
}
unsafe fn spc_handler_pdfm_image(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    let mut ident = None;
    let mut ti = transform_info::new();
    let mut options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    args.cur.skip_white();
    if args.cur[0] == b'@' {
        ident = args.cur.parse_opt_ident();
        let xobj_id = findresource(sd, ident.as_ref());
        if xobj_id >= 0 {
            if let Some(i) = ident {
                spc_warn!(
                    spe,
                    "Object reference name for image \"{}\" already used.",
                    i,
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
    let mut page_no = Some(options.page_no);
    let mut bbox_type = Some(options.bbox_type);
    transform_info_clear(&mut ti);
    if spc_util_read_blahblah(spe, &mut ti, &mut page_no, &mut bbox_type, args) < 0i32 {
        spc_warn!(spe, "Reading option field in pdf:image failed.");
        return -1i32;
    }
    options.page_no = page_no.unwrap();
    options.bbox_type = bbox_type.unwrap();
    args.cur.skip_white();
    if let Some(fspec) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::String(string) = &mut (*fspec).data {
            args.cur.skip_white();
            if !args.cur.is_empty() {
                options.dict = if let Some(obj) = args.cur.parse_pdf_object(ptr::null_mut()) {
                    obj
                } else {
                    ptr::null_mut()
                };
            }
            let xobj_id =
                pdf_ximage_findresource(std::str::from_utf8(string.to_bytes()).unwrap(), options);
            if xobj_id < 0i32 {
                spc_warn!(spe, "Could not find image resource...");
                pdf_release_obj(fspec);
                return -1i32;
            }
            if ti.flags & 1i32 << 4i32 == 0 {
                pdf_dev_put_image(xobj_id, &mut ti, spe.x_user, spe.y_user);
            }
            if let Some(i) = ident {
                addresource(sd, &i, xobj_id);
            }
            pdf_release_obj(fspec);
            0
        } else {
            spc_warn!(spe, "Missing filename string for pdf:image.");
            pdf_release_obj(fspec);
            -1
        }
    } else {
        spc_warn!(spe, "Missing filename string for pdf:image.");
        -1
    }
}
/* Use do_names instead. */
unsafe fn spc_handler_pdfm_dest(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if let Some(name) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::String(name_str) = &(*name).data {
            if let Some(array) = args.cur.parse_pdf_object(ptr::null_mut()) {
                if let Object::Array(_) = (*array).data {
                    pdf_doc_add_names(b"Dests", name_str.to_bytes(), array);
                } else {
                    spc_warn!(spe, "Destination not specified as an array object!");
                    pdf_release_obj(name);
                    pdf_release_obj(array);
                    return -1;
                }
            } else {
                spc_warn!(spe, "No destination specified for pdf:dest.");
                pdf_release_obj(name);
                return -1;
            }
            pdf_release_obj(name);
            0
        } else {
            spc_warn!(
                spe,
                "PDF string expected for destination name but invalid type."
            );
            pdf_release_obj(name);
            -1
        }
    } else {
        spc_warn!(
            spe,
            "PDF string expected for destination name but not found."
        );
        -1
    }
}
unsafe fn spc_handler_pdfm_names(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    if let Some(category) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::Name(cat_name) = &(*category).data {
            if let Some(tmp) = args.cur.parse_pdf_object(ptr::null_mut()) {
                match &mut (*tmp).data {
                    Object::Array(array) => {
                        let size = array.len() as i32;
                        if size % 2 != 0 {
                            spc_warn!(spe, "Array size not multiple of 2 for pdf:names.");
                            pdf_release_obj(category);
                            pdf_release_obj(tmp);
                            return -1;
                        }
                        for i in 0..(size / 2) as usize {
                            let key = array[2 * i];
                            let value = array[2 * i + 1];
                            if let Object::String(key) = &(*key).data {
                                if pdf_doc_add_names(
                                    cat_name.to_bytes(),
                                    key.to_bytes(),
                                    pdf_link_obj(value),
                                ) < 0
                                {
                                    spc_warn!(spe, "Failed to add Name tree entry...");
                                    pdf_release_obj(category);
                                    pdf_release_obj(tmp);
                                    return -1;
                                }
                            } else {
                                spc_warn!(spe, "Name tree key must be string.");
                                pdf_release_obj(category);
                                pdf_release_obj(tmp);
                                return -1;
                            }
                        }
                        pdf_release_obj(tmp);
                    }
                    Object::String(string) => {
                        if let Some(value) = args.cur.parse_pdf_object(ptr::null_mut()) {
                            if pdf_doc_add_names(cat_name.to_bytes(), string.to_bytes(), value) < 0
                            {
                                spc_warn!(spe, "Failed to add Name tree entry...");
                                pdf_release_obj(category);
                                pdf_release_obj(tmp);
                                return -1;
                            }
                            pdf_release_obj(tmp);
                        } else {
                            pdf_release_obj(category);
                            pdf_release_obj(tmp);
                            spc_warn!(spe, "PDF object expected but not found.");
                            return -1;
                        }
                    }
                    _ => {
                        pdf_release_obj(tmp);
                        pdf_release_obj(category);
                        spc_warn!(spe, "Invalid object type for pdf:names.");
                        return -1;
                    }
                }
            } else {
                spc_warn!(spe, "PDF object expected but not found.");
                pdf_release_obj(category);
                return -1i32;
            }
            pdf_release_obj(category);
            0
        } else {
            spc_warn!(spe, "PDF name expected but not found.");
            pdf_release_obj(category);
            -1
        }
    } else {
        spc_warn!(spe, "PDF name expected but not found.");
        -1
    }
}
unsafe fn spc_handler_pdfm_docinfo(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    if let Some(dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        let docinfo = pdf_doc_get_dictionary("Info");
        (*docinfo).as_dict_mut().merge(&dict);
        0
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        -1
    }
}
unsafe fn spc_handler_pdfm_docview(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    if let Some(mut dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        let catalog = pdf_doc_get_dictionary("Catalog");
        /* Avoid overriding whole ViewerPreferences */
        let pref_old = (*catalog).as_dict_mut().get_mut("ViewerPreferences"); /* Close all? */
        let pref_add = dict.get("ViewerPreferences");
        if let (Some(pref_old), Some(pref_add)) = (pref_old, pref_add) {
            (*pref_old).as_dict_mut().merge((*pref_add).as_dict());
            pdf_remove_dict(&mut dict, "ViewerPreferences");
        }
        (*catalog).as_dict_mut().merge(&dict);
        0
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        return -1i32;
    }
}
unsafe fn spc_handler_pdfm_close(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        spc_flush_object(&ident);
    } else {
        spc_clear_objects();
    }
    0i32
}
unsafe fn spc_handler_pdfm_object(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        if let Some(object) = args.cur.parse_pdf_object(ptr::null_mut()) {
            spc_push_object(&ident, object)
        } else {
            spc_warn!(
                spe,
                "Could not find an object definition for \"{}\".",
                ident,
            );
            return -1i32;
        }
        0
    } else {
        spc_warn!(spe, "Could not find a object identifier.");
        return -1i32;
    }
}
unsafe fn spc_handler_pdfm_content(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if !args.cur.is_empty() {
        let mut M = TMatrix::create_translation(spe.x_user, spe.y_user);
        let mut buf = Vec::new();
        buf.push(b' ');
        buf.push(b'q');
        buf.push(b' ');
        pdf_sprint_matrix(&mut buf, &mut M);
        buf.push(b' ');
        buf.push(b'c');
        buf.push(b'm');
        buf.push(b' ');
        /* op: Q */
        pdf_doc_add_page_content(&buf); /* op: q cm */
        pdf_doc_add_page_content(args.cur); /* op: ANY */
        pdf_doc_add_page_content(b" Q");
        /* op: ANY */
    } /* op: */
    args.cur = &[]; /* op: ANY */
    return 0i32; /*kpse_find_pict(instring);*/
}
unsafe fn spc_handler_pdfm_literal(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let mut direct: i32 = 0i32;
    args.cur.skip_white();
    while !args.cur.is_empty() {
        if args.cur.len() >= 7 && args.cur.starts_with(b"reverse") {
            args.cur = &args.cur[7..];
            warn!("The special \"pdf:literal reverse ...\" is no longer supported.\nIgnore the \"reverse\" option.");
        } else {
            if !(args.cur.len() >= 6 && args.cur.starts_with(b"direct")) {
                break;
            }
            direct = 1i32;
            args.cur = &args.cur[6..];
        }
        args.cur.skip_white();
    }
    if !args.cur.is_empty() {
        if direct == 0 {
            let mut M = TMatrix::create_translation(spe.x_user, spe.y_user);
            pdf_dev_concat(&mut M);
        }
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content(args.cur);
        if direct == 0 {
            let mut M = TMatrix::create_translation(-spe.x_user, -spe.y_user);
            pdf_dev_concat(&mut M);
        }
    }
    args.cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_bcontent(spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    pdf_dev_gsave();
    let pos = pdf_dev_get_coord();
    let mut M = TMatrix::create_translation(spe.x_user - pos.x, spe.y_user - pos.y);
    pdf_dev_concat(&mut M);
    pdf_dev_push_coord(spe.x_user, spe.y_user);
    0i32
}
unsafe fn spc_handler_pdfm_econtent(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    pdf_dev_pop_coord();
    pdf_dev_grestore();
    pdf_dev_reset_color(0i32);
    0i32
}
unsafe fn spc_handler_pdfm_code(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if !args.cur.is_empty() {
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content(args.cur);
        args.cur = &[];
    }
    0i32
}
unsafe fn spc_handler_pdfm_do_nothing(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur = &[];
    0i32
}
unsafe fn spc_handler_pdfm_stream_with_type(
    spe: &mut SpcEnv,
    args: &mut SpcArg,
    type_0: i32,
) -> i32 {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        args.cur.skip_white();
        if let Some(tmp) = args.cur.parse_pdf_object(ptr::null_mut()) {
            if let Object::String(instring) = &(*tmp).data {
                let instring = instring.to_bytes();
                let mut fstream = match type_0 {
                    1 => {
                        if instring.is_empty() {
                            spc_warn!(spe, "Missing filename for pdf:fstream.");
                            pdf_release_obj(tmp);
                            return -1i32;
                        }
                        let fullname: Option<String> = None; // TODO: check dead code
                        if let Some(fullname) = &fullname {
                            if let Some(mut handle) = InFile::open(fullname, TTInputFormat::PICT, 0)
                            {
                                let mut fstream = pdf_stream::new(STREAM_COMPRESS);
                                loop {
                                    let nb_read = handle.read(&mut WORK_BUFFER[..]).unwrap();
                                    if !(nb_read > 0) {
                                        // TODO: check
                                        break;
                                    }
                                    fstream.add_slice(&WORK_BUFFER[..nb_read]);
                                }
                                fstream
                            } else {
                                spc_warn!(spe, "Could not open file: {}", instring.display());
                                pdf_release_obj(tmp);
                                return -1i32;
                            }
                        } else {
                            spc_warn!(spe, "File \"{}\" not found.", instring.display());
                            pdf_release_obj(tmp);
                            return -1i32;
                        }
                    }
                    0 => {
                        let mut fstream = pdf_stream::new(STREAM_COMPRESS);
                        if !instring.is_empty() {
                            fstream.add(
                                instring.as_ptr() as *const libc::c_void,
                                instring.len() as i32,
                            );
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
                args.cur.skip_white();
                if args.cur[0] == b'<' {
                    let stream_dict = fstream.get_dict_mut();
                    if let Some(mut tmp) = args.cur.parse_pdf_dict(ptr::null_mut()) {
                        if tmp.has("Length") {
                            pdf_remove_dict(&mut tmp, "Length");
                        } else if tmp.has("Filter") {
                            pdf_remove_dict(&mut tmp, "Filter");
                        }
                        stream_dict.merge(&tmp);
                    } else {
                        spc_warn!(spe, "Parsing dictionary failed.");
                        return -1i32;
                    }
                }
                /* Users should explicitly close this. */
                spc_push_object(&ident, fstream.into_obj());
                0
            } else {
                spc_warn!(spe, "Invalid type of input string for pdf:(f)stream.");
                pdf_release_obj(tmp);
                -1
            }
        } else {
            spc_warn!(spe, "Missing input string for pdf:(f)stream.");
            -1
        }
    } else {
        spc_warn!(spe, "Missing objname for pdf:(f)stream.");
        -1
    }
}
/*
 * STREAM: Create a PDF stream object from an input string.
 *
 *  pdf: stream @objname (input_string) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_stream(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    spc_handler_pdfm_stream_with_type(spe, args, 0i32)
}
/*
 * FSTREAM: Create a PDF stream object from an existing file.
 *
 *  pdf: fstream @objname (filename) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_fstream(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
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
unsafe fn spc_handler_pdfm_bform(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
            ti
        } else {
            return -1;
        };
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
        let xobj_id = pdf_doc_begin_grabbing(&ident, spe.x_user, spe.y_user, &mut cropbox);
        if xobj_id < 0i32 {
            spc_warn!(spe, "Couldn\'t start form object.");
            return -1i32;
        }
        spc_push_object(&ident, pdf_ximage_get_reference(xobj_id));
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
unsafe fn spc_handler_pdfm_eform(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    let attrib = if !args.cur.is_empty() {
        args.cur
            .parse_pdf_dict(ptr::null_mut())
            .map(IntoObj::into_obj)
            .unwrap_or(ptr::null_mut())
    } else {
        ptr::null_mut()
    };
    pdf_doc_end_grabbing(attrib);
    0
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
unsafe fn spc_handler_pdfm_uxobj(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    let options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        let mut ti = if !args.cur.is_empty() {
            if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
                ti
            } else {
                return -1;
            }
        } else {
            transform_info::new()
        };
        /* Dvipdfmx was suddenly changed to use file name to identify
         * external images. We can't use ident to find image resource
         * here.
         */
        let mut xobj_id = findresource(sd, Some(&ident));
        if xobj_id < 0i32 {
            xobj_id = pdf_ximage_findresource(&ident, options);
            if xobj_id < 0i32 {
                spc_warn!(spe, "Specified (image) object doesn\'t exist: {}", ident);
                return -1;
            }
        }
        pdf_dev_put_image(xobj_id, &mut ti, spe.x_user, spe.y_user);
        0
    } else {
        spc_warn!(spe, "No object identifier given.");
        -1
    }
}
unsafe fn spc_handler_pdfm_link(spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    spc_resume_annot(spe)
}
unsafe fn spc_handler_pdfm_nolink(spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    spc_suspend_annot(spe)
}
/* Handled at BOP */
unsafe fn spc_handler_pdfm_pagesize(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur = &[];
    0i32
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_pdfm_bgcolor(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
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
unsafe fn spc_handler_pdfm_mapline(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let mut error: i32 = 0i32;
    ap.cur.skip_white();
    if ap.cur.is_empty() {
        spc_warn!(spe, "Empty mapline special?");
        return -1i32;
    }
    let opchr = ap.cur[0];
    if opchr == b'-' || opchr == b'+' {
        ap.cur = &ap.cur[1..];
    }
    ap.cur.skip_white();
    match opchr {
        45 => {
            if let Some(map_name) = ap.cur.parse_ident() {
                pdf_remove_fontmap_record(&map_name);
            } else {
                spc_warn!(spe, "Invalid fontmap line: Missing TFM name.");
                error = -1i32
            }
        }
        _ => {
            let s = String::from_utf8(ap.cur.into()).unwrap();
            let mut mrec = pdf_init_fontmap_record();
            error = pdf_read_fontmap_line(&mut mrec, &s, is_pdfm_mapline(&s));
            if error != 0 {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr == b'+' {
                pdf_append_fontmap_record(&mrec.map_name, &mrec);
            } else {
                pdf_insert_fontmap_record(&mrec.map_name, &mrec).ok();
            }
        }
    }
    if error == 0 {
        ap.cur = &[];
    }
    0i32
}
unsafe fn spc_handler_pdfm_mapfile(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let error;
    args.cur.skip_white();
    if args.cur.is_empty() {
        return 0i32;
    }
    let mode = match args.cur[0] {
        45 => {
            args.cur = &args.cur[1..];
            '-' as i32
        }
        43 => {
            args.cur = &args.cur[1..];
            '+' as i32
        }
        _ => 0,
    };
    if let Some(mapfile) = args.cur.parse_val_ident() {
        error = pdf_load_fontmap_file(&mapfile, mode)
    } else {
        spc_warn!(spe, "No fontmap file specified.");
        return -1i32;
    }
    error
}
unsafe fn spc_handler_pdfm_tounicode(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let sd = &mut _PDF_STAT;
    /* First clear */
    sd.cd.cmap_id = -1i32;
    sd.cd.unescape_backslash = 0i32;
    args.cur.skip_white();
    if args.cur.is_empty() {
        spc_warn!(spe, "Missing CMap name for pdf:tounicode.");
        return -1i32;
    }
    /* _FIXME_
     * Any valid char allowed for PDF name object should be allowed here.
     * The argument to this special should be a PDF name obejct.
     * But it's too late to change this special.
     */
    if let Some(cmap_name) = args.cur.parse_ident() {
        sd.cd.cmap_id = CMap_cache_find(&cmap_name);
        if sd.cd.cmap_id < 0i32 {
            spc_warn!(spe, "Failed to load ToUnicode mapping: {}", cmap_name);
            return -1i32;
        }
        /* Shift-JIS like encoding may contain backslash in 2nd byte.
         * WARNING: This will add nasty extension to PDF parser.
         */
        if sd.cd.cmap_id >= 0 {
            if cmap_name.contains("RKSJ")
                || cmap_name.contains("B5")
                || cmap_name.contains("GBK")
                || cmap_name.contains("KSC")
            {
                sd.cd.unescape_backslash = 1i32
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
        key: "annotation",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: "annotate",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: "annot",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: "ann",
        exec: Some(spc_handler_pdfm_annot),
    },
    SpcHandler {
        key: "outline",
        exec: Some(spc_handler_pdfm_outline),
    },
    SpcHandler {
        key: "out",
        exec: Some(spc_handler_pdfm_outline),
    },
    SpcHandler {
        key: "article",
        exec: Some(spc_handler_pdfm_article),
    },
    SpcHandler {
        key: "art",
        exec: Some(spc_handler_pdfm_article),
    },
    SpcHandler {
        key: "bead",
        exec: Some(spc_handler_pdfm_bead),
    },
    SpcHandler {
        key: "thread",
        exec: Some(spc_handler_pdfm_bead),
    },
    SpcHandler {
        key: "destination",
        exec: Some(spc_handler_pdfm_dest),
    },
    SpcHandler {
        key: "dest",
        exec: Some(spc_handler_pdfm_dest),
    },
    SpcHandler {
        key: "object",
        exec: Some(spc_handler_pdfm_object),
    },
    SpcHandler {
        key: "obj",
        exec: Some(spc_handler_pdfm_object),
    },
    SpcHandler {
        key: "docinfo",
        exec: Some(spc_handler_pdfm_docinfo),
    },
    SpcHandler {
        key: "docview",
        exec: Some(spc_handler_pdfm_docview),
    },
    SpcHandler {
        key: "content",
        exec: Some(spc_handler_pdfm_content),
    },
    SpcHandler {
        key: "put",
        exec: Some(spc_handler_pdfm_put),
    },
    SpcHandler {
        key: "close",
        exec: Some(spc_handler_pdfm_close),
    },
    SpcHandler {
        key: "bop",
        exec: Some(spc_handler_pdfm_bop),
    },
    SpcHandler {
        key: "eop",
        exec: Some(spc_handler_pdfm_eop),
    },
    SpcHandler {
        key: "image",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: "img",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: "epdf",
        exec: Some(spc_handler_pdfm_image),
    },
    SpcHandler {
        key: "link",
        exec: Some(spc_handler_pdfm_link),
    },
    SpcHandler {
        key: "nolink",
        exec: Some(spc_handler_pdfm_nolink),
    },
    SpcHandler {
        key: "begincolor",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "bcolor",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "bc",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "setcolor",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: "scolor",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: "sc",
        exec: Some(spc_handler_pdfm_scolor),
    },
    SpcHandler {
        key: "endcolor",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "ecolor",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "ec",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "begingray",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "bgray",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "bg",
        exec: Some(spc_handler_pdfm_bcolor),
    },
    SpcHandler {
        key: "endgray",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "egray",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "eg",
        exec: Some(spc_handler_pdfm_ecolor),
    },
    SpcHandler {
        key: "bgcolor",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: "bgc",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: "bbc",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: "bbg",
        exec: Some(spc_handler_pdfm_bgcolor),
    },
    SpcHandler {
        key: "pagesize",
        exec: Some(spc_handler_pdfm_pagesize),
    },
    SpcHandler {
        key: "bannot",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: "beginann",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: "bann",
        exec: Some(spc_handler_pdfm_bann),
    },
    SpcHandler {
        key: "eannot",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: "endann",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: "eann",
        exec: Some(spc_handler_pdfm_eann),
    },
    SpcHandler {
        key: "btrans",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: "begintransform",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: "begintrans",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: "bt",
        exec: Some(spc_handler_pdfm_btrans),
    },
    SpcHandler {
        key: "etrans",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: "endtransform",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: "endtrans",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: "et",
        exec: Some(spc_handler_pdfm_etrans),
    },
    SpcHandler {
        key: "bform",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: "beginxobj",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: "bxobj",
        exec: Some(spc_handler_pdfm_bform),
    },
    SpcHandler {
        key: "eform",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: "endxobj",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: "exobj",
        exec: Some(spc_handler_pdfm_eform),
    },
    SpcHandler {
        key: "usexobj",
        exec: Some(spc_handler_pdfm_uxobj),
    },
    SpcHandler {
        key: "uxobj",
        exec: Some(spc_handler_pdfm_uxobj),
    },
    SpcHandler {
        key: "tounicode",
        exec: Some(spc_handler_pdfm_tounicode),
    },
    SpcHandler {
        key: "literal",
        exec: Some(spc_handler_pdfm_literal),
    },
    SpcHandler {
        key: "stream",
        exec: Some(spc_handler_pdfm_stream),
    },
    SpcHandler {
        key: "fstream",
        exec: Some(spc_handler_pdfm_fstream),
    },
    SpcHandler {
        key: "names",
        exec: Some(spc_handler_pdfm_names),
    },
    SpcHandler {
        key: "mapline",
        exec: Some(spc_handler_pdfm_mapline),
    },
    SpcHandler {
        key: "mapfile",
        exec: Some(spc_handler_pdfm_mapfile),
    },
    SpcHandler {
        key: "bcontent",
        exec: Some(spc_handler_pdfm_bcontent),
    },
    SpcHandler {
        key: "econtent",
        exec: Some(spc_handler_pdfm_econtent),
    },
    SpcHandler {
        key: "code",
        exec: Some(spc_handler_pdfm_code),
    },
    SpcHandler {
        key: "minorversion",
        exec: Some(spc_handler_pdfm_do_nothing),
    },
    SpcHandler {
        key: "encrypt",
        exec: Some(spc_handler_pdfm_do_nothing),
    },
];
pub(crate) fn spc_pdfm_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"pdf:")
}

pub(crate) unsafe fn spc_pdfm_setup_handler(
    sph: &mut SpcHandler,
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
) -> i32 {
    let mut error: i32 = -1i32;
    ap.cur.skip_white();
    if !ap.cur.starts_with(b"pdf:") {
        spc_warn!(spe, "Not pdf: special???");
        return -1i32;
    }
    ap.cur = &ap.cur[b"pdf:".len()..];
    ap.cur.skip_white();
    if let Some(q) = ap.cur.parse_c_ident() {
        for handler in PDFM_HANDLERS.iter() {
            if q == handler.key {
                ap.command = Some(handler.key);
                *sph = SpcHandler {
                    key: "pdf:",
                    exec: handler.exec,
                };
                ap.cur.skip_white();
                error = 0i32;
                break;
            }
        }
    }
    error
}
