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
#![allow(non_camel_case_types, non_snake_case)]

use super::{Result, ERR, ERROR};

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

use crate::bridge::InFile;
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
use crate::dpx_pdfdoc::{pdf_doc_mut, pdf_doc_set_bgcolor, PdfPageBoundary};
use crate::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_grestore, pdf_dev_gsave, pdf_dev_transform};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_link_obj, pdf_name, pdf_obj, pdf_stream, pdf_string, IntoObj, Object,
    STREAM_COMPRESS,
};
use crate::dpx_pdfparse::{ParseIdent, ParsePdfObj, SkipWhite};
use crate::dpx_pdfximage::{pdf_ximage_findresource, pdf_ximage_get_reference};
use crate::dpx_unicode::{UC_UTF16BE_is_valid_string, UC_UTF8_is_valid_string};

use super::{Handler, SpcArg, SpcEnv};

#[derive(Clone)]
pub(crate) struct spc_pdf_ {
    pub(crate) annotation_started: bool,
    pub(crate) lowest_level: i32,
    pub(crate) resourcemap: HashMap<String, resource_map>,
    pub(crate) cd: tounicode,
    /* quasi-hack to get the primary input */
    /* For to-UTF16-BE conversion :( */
}
impl spc_pdf_ {
    pub(crate) fn new() -> Self {
        Self {
            annotation_started: false,
            lowest_level: 255,
            resourcemap: HashMap::new(),
            cd: tounicode {
                cmap_id: None,
                unescape_backslash: 0,
                taintkeys: ptr::null_mut(),
            },
        }
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tounicode {
    pub(crate) cmap_id: Option<usize>,
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

pub(crate) static mut _PDF_STAT: Lazy<spc_pdf_> = Lazy::new(|| spc_pdf_::new());

unsafe fn addresource(sd: &mut spc_pdf_, ident: &str, res_id: i32) -> i32 {
    if ident.is_empty() || res_id < 0 {
        return -1;
    }
    let r = resource_map { type_0: 0, res_id };
    sd.resourcemap.insert(ident.to_string(), r);
    spc_push_object(ident, pdf_ximage_get_reference(res_id));
    0
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
unsafe fn spc_handler_pdfm__init(sd: &mut spc_pdf_) -> Result<()> {
    /* The folllowing dictionary entry keys are considered as keys for
     * text strings. Be sure that string object is NOT always a text string.
     */
    const DEFAULT_TAINTKEYS: [&str; 11] = [
        "Title", "Author", "Subject", "Keywords", "Creator", "Producer", "Contents", "Subj", "TU",
        "T", "TM",
    ];
    sd.annotation_started = false;
    sd.lowest_level = 255;
    sd.resourcemap.clear();
    let array: Vec<*mut pdf_obj> = DEFAULT_TAINTKEYS
        .iter()
        .map(|&key| key.into_obj())
        .collect();
    sd.cd.taintkeys = array.into_obj();
    Ok(())
}
unsafe fn spc_handler_pdfm__clean(sd: &mut spc_pdf_) -> Result<()> {
    if sd.annotation_started {
        warn!("Unbalanced bann and eann found.");
    }
    sd.lowest_level = 255;
    sd.annotation_started = false;
    sd.resourcemap.clear();
    crate::release!(sd.cd.taintkeys);
    sd.cd.taintkeys = ptr::null_mut();
    Ok(())
}

pub(crate) unsafe fn spc_pdfm_at_begin_document() -> Result<()> {
    let sd = &mut _PDF_STAT;
    spc_handler_pdfm__init(sd)
}

pub(crate) unsafe fn spc_pdfm_at_end_document() -> Result<()> {
    let sd = &mut _PDF_STAT;
    spc_handler_pdfm__clean(sd)
}
/* Dvipdfm specials */
unsafe fn spc_handler_pdfm_bop(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if !args.cur.is_empty() {
        pdf_doc_mut().set_bop_content(&args.cur);
    }
    args.cur = &[];
    Ok(())
}
unsafe fn spc_handler_pdfm_eop(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if !args.cur.is_empty() {
        pdf_doc_mut().set_eop_content(&args.cur);
    }
    args.cur = &[];
    Ok(())
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
    0
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
            return -1;
        }
    }
    0
}
/* Think what happens if you do
 *
 *  pdf:put @resources << /Font << >> >>
 *
 */
unsafe fn spc_handler_pdfm_put(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
    let mut error = Ok(());
    ap.cur.skip_white();
    let ident = ap.cur.parse_opt_ident();
    if ident.is_none() {
        spc_warn!(spe, "Missing object identifier.");
        return ERR;
    }
    let ident = ident.unwrap();
    let obj1 = spc_lookup_object(&ident); /* put obj2 into obj1 */
    if obj1.is_null() {
        spc_warn!(spe, "Specified object not exist: {}", ident);
        return ERR;
    }
    ap.cur.skip_white();
    let obj2 = ap.cur.parse_pdf_object(ptr::null_mut());
    if obj2.is_none() {
        spc_warn!(spe, "Missing (an) object(s) to put into \"{}\"!", ident);
        return ERR;
    }
    let obj2 = obj2.unwrap();
    match &mut (*obj1).data {
        Object::Dict(d1) => {
            if let Object::Dict(d2) = &mut (*obj2).data {
                if ident == "resources" {
                    error = {
                        let res = d2.foreach(safeputresdict, d1);
                        if res == 0 {
                            Ok(())
                        } else {
                            Err(std::num::NonZeroI32::new(res).unwrap())
                        }
                    };
                } else {
                    d1.merge(d2);
                }
            } else {
                spc_warn!(
                    spe,
                    "Inconsistent object type for \"put\" (expecting DICT): {}",
                    ident,
                );
                error = ERR;
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
                error = ERR;
            }
            _ => {
                spc_warn!(spe, "Invalid type: expecting a DICT or STREAM: {}", ident);
                error = ERR;
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
            error = ERR;
        }
    }
    crate::release!(obj2);
    error
}
/* For pdf:tounicode support
 * This feature is provided for convenience. TeX can't do
 * input encoding conversion.
 */
unsafe fn reencodestring(cmap: *mut CMap, instring: *mut pdf_string) -> i32 {
    let mut wbuf: [u8; 4096] = [0; 4096];
    if cmap.is_null() || instring.is_null() {
        return 0;
    }
    let mut inbuf = (*instring).to_bytes();
    wbuf[0] = 0xfe_u8;
    wbuf[1] = 0xff_u8;
    let (_, obuf) = CMap_decode(&*cmap, &mut inbuf, &mut wbuf[2..]);
    if inbuf.len() > 0 {
        return -1;
    }
    let len = 4096 - obuf.len();
    (*instring).set(&wbuf[..len]);
    0
}
unsafe fn maybe_reencode_utf8(instring: *mut pdf_string) -> std::result::Result<(), i32> {
    let mut wbuf: [u8; 4096] = [0; 4096];
    if instring.is_null() {
        return Ok(());
    }
    let inbuf = (*instring).to_bytes();
    /* check if the input string is strictly ASCII */
    /* no need to reencode ASCII strings */
    if inbuf.is_ascii() {
        return Ok(());
    }
    /* Check if the input string is valid UTF8 string
     * This routine may be called against non-text strings.
     * We need to re-encode string only when string is a text string
     * endcoded in UTF8.
     */
    if !UC_UTF8_is_valid_string(inbuf) {
        return Ok(());
    } else {
        if inbuf[0] == 0xfe && inbuf[1] == 0xff && UC_UTF16BE_is_valid_string(inbuf) as i32 != 0 {
            return Ok(());
        }
    } /* no need to reencode UTF16BE with BOM */
    /* out of valid Unicode range, give up (redundant) */
    let mut op = wbuf.as_mut_ptr();
    *op = 0xfe_u8;
    op = op.offset(1);
    *op = 0xff_u8;
    op = op.offset(1);
    for c16 in std::str::from_utf8(inbuf).map_err(|_| -1)?.encode_utf16() {
        let c8 = c16.to_be_bytes();
        *op.offset(0) = c8[0];
        *op.offset(1) = c8[1];
        op = op.offset(2);
    }
    (*instring).set(&wbuf[..(op.offset_from(wbuf.as_ptr()) as usize)]);
    Ok(())
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
    let mut r: i32 = 0;
    match &mut vp.data {
        Object::String(vp) => {
            if cd.cmap_id.is_some() && !cd.taintkeys.is_null() {
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
                    r = if maybe_reencode_utf8(vp).is_ok() {
                        0
                    } else {
                        -1
                    };
                }
            }
            if r < 0 {
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
        if unsafe { is_xdv == 0 && cd.cmap_id.is_none() } {
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

unsafe fn spc_handler_pdfm_annot(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
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
        return ERR;
    };

    if ti.flags & 1 << 0 != 0 && (ti.flags & 1 << 1 != 0 || ti.flags & 1 << 2 != 0) {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return ERR;
    }
    let annot_dict = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd);
    if annot_dict.is_none() {
        spc_warn!(spe, "Could not find dictionary object.");
        return ERR;
    }
    let annot_dict = annot_dict.unwrap().into_obj();
    let mut cp = point2(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut cp, None);
    if ti.flags & 1 << 0 != 0 {
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
    let p = pdf_doc_mut();
    p.add_annot(p.current_page_number(), &mut rect, annot_dict, 1);
    if let Some(i) = ident {
        spc_flush_object(&i);
    }
    crate::release!(annot_dict);
    Ok(())
}
/* NOTE: This can't have ident. See "Dvipdfm User's Manual". */
unsafe fn spc_handler_pdfm_bann(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    if sd.annotation_started {
        spc_warn!(spe, "Can\'t begin an annotation when one is pending.");
        return ERR;
    }
    args.cur.skip_white();
    if let Some(annot_dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        sd.annotation_started = true;
        spc_begin_annot(spe, annot_dict)
    } else {
        sd.annotation_started = false;
        spc_warn!(spe, "Ignoring annotation with invalid dictionary.");
        return ERR;
    }
}
unsafe fn spc_handler_pdfm_eann(spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    if !sd.annotation_started {
        spc_warn!(spe, "Tried to end an annotation without starting one!");
        return ERR;
    }
    let error = spc_end_annot(spe);
    sd.annotation_started = false;
    error
}
/* Color:.... */
unsafe fn spc_handler_pdfm_bcolor(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
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
        Ok(())
    } else {
        spc_warn!(spe, "Invalid color specification?");
        ERR
    }
}
/*
 * This special changes the current color without clearing the color stack.
 * It therefore differs from "color rgb 1 0 0".
 */
unsafe fn spc_handler_pdfm_scolor(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
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
        Ok(())
    } else {
        spc_warn!(spe, "Invalid color specification?");
        ERR
    }
}
unsafe fn spc_handler_pdfm_ecolor(_spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    pdf_color_pop();
    Ok(())
}
unsafe fn spc_handler_pdfm_btrans(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
        ti
    } else {
        return ERR;
    };
    /* Create transformation matrix */
    let mut M = ti.matrix.clone();
    M.m31 += (1. - M.m11) * spe.x_user - M.m21 * spe.y_user;
    M.m32 += (1. - M.m22) * spe.y_user - M.m12 * spe.x_user;
    pdf_dev_gsave();
    pdf_dev_concat(&mut M);
    Ok(())
}
unsafe fn spc_handler_pdfm_etrans(_spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    pdf_dev_grestore();
    /*
     * Unfortunately, the following line is necessary in case
     * of a color change inside of the save/restore pair.
     * (Font changes are automatically corrected by pdf_dev_grestore().)
     * Anything that was done there must be redone, so in effect,
     * we make no assumptions about what fonts. We act like we are
     * starting a new page.
     */
    pdf_dev_reset_color(0);
    Ok(())
}
unsafe fn spc_handler_pdfm_outline(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    let mut is_open: i32 = -1;
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
            is_open = 1
        }
        args.cur = &args.cur[1..];
    }
    args.cur.skip_white();
    let mut level = if let Some(tmp) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::Number(level) = (*tmp).data {
            crate::release!(tmp);
            level as i32
        } else {
            crate::release!(tmp);
            spc_warn!(spe, "Expecting number for outline item depth.");
            return ERR;
        }
    } else {
        spc_warn!(spe, "Missing number for outline item depth.");
        return ERR;
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
    level += 1 - sd.lowest_level;
    let item_dict = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd);
    if item_dict.is_none() {
        spc_warn!(spe, "Ignoring invalid dictionary.");
        return ERR;
    }
    let item_dict = item_dict.unwrap();
    let p = pdf_doc_mut();
    let mut current_depth = p.bookmarks_depth();
    if current_depth > level {
        while current_depth > level {
            current_depth -= 1;
            p.bookmarks_up();
        }
    } else if current_depth < level {
        while current_depth < level {
            current_depth += 1;
            p.bookmarks_down();
        }
    }
    p.bookmarks_add(&mut *item_dict.into_obj(), is_open);
    Ok(())
}
unsafe fn spc_handler_pdfm_article(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        if let Some(info_dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
            let info_dict = info_dict.into_obj();
            pdf_doc_mut().begin_article(&ident, pdf_link_obj(info_dict));
            spc_push_object(&ident, info_dict);
            Ok(())
        } else {
            spc_warn!(spe, "Ignoring article with invalid info dictionary.");
            ERR
        }
    } else {
        spc_warn!(spe, "Article name expected but not found.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_bead(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    args.cur.skip_white();
    if args.cur[0] != b'@' {
        spc_warn!(spe, "Article identifier expected but not found.");
        return ERR;
    }
    let article_name = args.cur.parse_opt_ident();
    if article_name.is_none() {
        spc_warn!(spe, "Article reference expected but not found.");
        return ERR;
    }
    let article_name = article_name.unwrap();
    /* If okay so far, try to get a bounding box */
    let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
        ti
    } else {
        return ERR;
    };
    if ti.flags & 1 << 0 != 0 && (ti.flags & 1 << 1 != 0 || ti.flags & 1 << 2 != 0) {
        spc_warn!(spe, "You can\'t specify both bbox and width/height.");
        return ERR;
    }
    let mut cp = point2(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut cp, None);
    let mut rect = if ti.flags & 1 << 0 != 0 {
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
            return ERR;
        }
    }
    /* Does this article exist yet */
    let article = spc_lookup_object(&article_name);
    let p = pdf_doc_mut();
    if !article.is_null() {
        (*article).as_dict_mut().merge(&article_info);
    } else {
        let article_info = article_info.into_obj();
        p.begin_article(&article_name, pdf_link_obj(article_info));
        spc_push_object(&article_name, article_info);
    }
    let page_no = p.current_page_number();
    p.add_bead(&article_name, "", page_no, &mut rect);
    Ok(())
}
unsafe fn spc_handler_pdfm_image(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    let mut ident = None;
    let mut ti = transform_info::new();
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
            return ERR;
        }
    }
    /* 2015/12/29
     * There should not be "page" and "pagebox" in read_dimtrns().
     * It is for reading "dimensions" and "transformations" and "page" is
     * completely unrelated.
     */
    let mut options: load_options = load_options {
        page_no: 1,
        bbox_type: PdfPageBoundary::Auto,
        dict: ptr::null_mut(),
    };
    let mut page_no = Some(options.page_no);
    let mut bbox_type = Some(options.bbox_type);
    transform_info_clear(&mut ti);
    if spc_util_read_blahblah(spe, &mut ti, &mut page_no, &mut bbox_type, args) < 0 {
        spc_warn!(spe, "Reading option field in pdf:image failed.");
        return ERR;
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
            if xobj_id < 0 {
                spc_warn!(spe, "Could not find image resource...");
                crate::release!(fspec);
                return ERR;
            }
            if ti.flags & 1 << 4 == 0 {
                pdf_dev_put_image(xobj_id, &mut ti, spe.x_user, spe.y_user);
            }
            if let Some(i) = ident {
                addresource(sd, &i, xobj_id);
            }
            crate::release!(fspec);
            Ok(())
        } else {
            spc_warn!(spe, "Missing filename string for pdf:image.");
            crate::release!(fspec);
            ERR
        }
    } else {
        spc_warn!(spe, "Missing filename string for pdf:image.");
        ERR
    }
}
/* Use do_names instead. */
unsafe fn spc_handler_pdfm_dest(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if let Some(name) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::String(name_str) = &(*name).data {
            if let Some(array) = args.cur.parse_pdf_object(ptr::null_mut()) {
                if let Object::Array(_) = (*array).data {
                    pdf_doc_mut()
                        .add_names(b"Dests", name_str.to_bytes(), &mut *array)
                        .ok();
                } else {
                    spc_warn!(spe, "Destination not specified as an array object!");
                    crate::release!(name);
                    crate::release!(array);
                    return ERR;
                }
            } else {
                spc_warn!(spe, "No destination specified for pdf:dest.");
                crate::release!(name);
                return ERR;
            }
            crate::release!(name);
            Ok(())
        } else {
            spc_warn!(
                spe,
                "PDF string expected for destination name but invalid type."
            );
            crate::release!(name);
            ERR
        }
    } else {
        spc_warn!(
            spe,
            "PDF string expected for destination name but not found."
        );
        ERR
    }
}
unsafe fn spc_handler_pdfm_names(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if let Some(category) = args.cur.parse_pdf_object(ptr::null_mut()) {
        if let Object::Name(cat_name) = &(*category).data {
            if let Some(tmp) = args.cur.parse_pdf_object(ptr::null_mut()) {
                match &mut (*tmp).data {
                    Object::Array(array) => {
                        let size = array.len() as i32;
                        if size % 2 != 0 {
                            spc_warn!(spe, "Array size not multiple of 2 for pdf:names.");
                            crate::release!(category);
                            crate::release!(tmp);
                            return ERR;
                        }
                        for i in 0..(size / 2) as usize {
                            let key = array[2 * i];
                            let value = array[2 * i + 1];
                            if let Object::String(key) = &(*key).data {
                                if pdf_doc_mut()
                                    .add_names(
                                        cat_name.to_bytes(),
                                        key.to_bytes(),
                                        &mut *pdf_link_obj(value),
                                    )
                                    .is_err()
                                {
                                    spc_warn!(spe, "Failed to add Name tree entry...");
                                    crate::release!(category);
                                    crate::release!(tmp);
                                    return ERR;
                                }
                            } else {
                                spc_warn!(spe, "Name tree key must be string.");
                                crate::release!(category);
                                crate::release!(tmp);
                                return ERR;
                            }
                        }
                        crate::release!(tmp);
                    }
                    Object::String(string) => {
                        if let Some(value) = args.cur.parse_pdf_object(ptr::null_mut()) {
                            if pdf_doc_mut()
                                .add_names(cat_name.to_bytes(), string.to_bytes(), &mut *value)
                                .is_err()
                            {
                                spc_warn!(spe, "Failed to add Name tree entry...");
                                crate::release!(category);
                                crate::release!(tmp);
                                return ERR;
                            }
                            crate::release!(tmp);
                        } else {
                            crate::release!(category);
                            crate::release!(tmp);
                            spc_warn!(spe, "PDF object expected but not found.");
                            return ERR;
                        }
                    }
                    _ => {
                        crate::release!(tmp);
                        crate::release!(category);
                        spc_warn!(spe, "Invalid object type for pdf:names.");
                        return ERR;
                    }
                }
            } else {
                spc_warn!(spe, "PDF object expected but not found.");
                crate::release!(category);
                return ERR;
            }
            crate::release!(category);
            Ok(())
        } else {
            spc_warn!(spe, "PDF name expected but not found.");
            crate::release!(category);
            ERR
        }
    } else {
        spc_warn!(spe, "PDF name expected but not found.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_docinfo(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    if let Some(dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        let docinfo = pdf_doc_mut().get_dictionary("Info");
        (*docinfo).as_dict_mut().merge(&dict);
        Ok(())
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_docview(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    if let Some(mut dict) = args.cur.parse_pdf_dict_with_tounicode(&mut sd.cd) {
        let catalog = pdf_doc_mut().get_dictionary("Catalog");
        /* Avoid overriding whole ViewerPreferences */
        let pref_old = (*catalog).as_dict_mut().get_mut("ViewerPreferences"); /* Close all? */
        let pref_add = dict.get("ViewerPreferences");
        if let (Some(pref_old), Some(pref_add)) = (pref_old, pref_add) {
            (*pref_old).as_dict_mut().merge((*pref_add).as_dict());
            dict.remove("ViewerPreferences");
        }
        (*catalog).as_dict_mut().merge(&dict);
        Ok(())
    } else {
        spc_warn!(spe, "Dictionary object expected but not found.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_close(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        spc_flush_object(&ident);
    } else {
        spc_clear_objects();
    }
    Ok(())
}
unsafe fn spc_handler_pdfm_object(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        if let Some(object) = args.cur.parse_pdf_object(ptr::null_mut()) {
            spc_push_object(&ident, object);
            Ok(())
        } else {
            spc_warn!(
                spe,
                "Could not find an object definition for \"{}\".",
                ident,
            );
            ERR
        }
    } else {
        spc_warn!(spe, "Could not find a object identifier.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_content(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
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
        let p = pdf_doc_mut();
        p.add_page_content(&buf); /* op: q cm */
        p.add_page_content(args.cur); /* op: ANY */
        p.add_page_content(b" Q");
        /* op: ANY */
    } /* op: */
    args.cur = &[]; /* op: ANY */
    Ok(()) /*kpse_find_pict(instring);*/
}
unsafe fn spc_handler_pdfm_literal(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let mut direct: i32 = 0;
    args.cur.skip_white();
    while !args.cur.is_empty() {
        if let Some(cur) = args.cur.strip_prefix(b"reverse") {
            args.cur = cur;
            warn!("The special \"pdf:literal reverse ...\" is no longer supported.\nIgnore the \"reverse\" option.");
        } else {
            if let Some(cur) = args.cur.strip_prefix(b"direct") {
                direct = 1;
                args.cur = cur;
            } else {
                break;
            }
        }
        args.cur.skip_white();
    }
    if !args.cur.is_empty() {
        if direct == 0 {
            let mut M = TMatrix::create_translation(spe.x_user, spe.y_user);
            pdf_dev_concat(&mut M);
        }
        let p = pdf_doc_mut();
        p.add_page_content(b" ");
        p.add_page_content(args.cur);
        if direct == 0 {
            let mut M = TMatrix::create_translation(-spe.x_user, -spe.y_user);
            pdf_dev_concat(&mut M);
        }
    }
    args.cur = &[];
    Ok(())
}
unsafe fn spc_handler_pdfm_bcontent(spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    pdf_dev_gsave();
    let pos = pdf_dev_get_coord();
    let mut M = TMatrix::create_translation(spe.x_user - pos.x, spe.y_user - pos.y);
    pdf_dev_concat(&mut M);
    pdf_dev_push_coord(spe.x_user, spe.y_user);
    Ok(())
}
unsafe fn spc_handler_pdfm_econtent(_spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    pdf_dev_pop_coord();
    pdf_dev_grestore();
    pdf_dev_reset_color(0);
    Ok(())
}
unsafe fn spc_handler_pdfm_code(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if !args.cur.is_empty() {
        let p = pdf_doc_mut();
        p.add_page_content(b" ");
        p.add_page_content(args.cur);
        args.cur = &[];
    }
    Ok(())
}
unsafe fn spc_handler_pdfm_do_nothing(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur = &[];
    Ok(())
}
unsafe fn spc_handler_pdfm_stream_with_type(
    spe: &mut SpcEnv,
    args: &mut SpcArg,
    type_0: i32,
) -> Result<()> {
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
                            crate::release!(tmp);
                            return ERR;
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
                                crate::release!(tmp);
                                return ERR;
                            }
                        } else {
                            spc_warn!(spe, "File \"{}\" not found.", instring.display());
                            crate::release!(tmp);
                            return ERR;
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
                        crate::release!(tmp);
                        return ERR;
                    }
                };
                crate::release!(tmp);
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
                            tmp.remove("Length");
                        } else if tmp.has("Filter") {
                            tmp.remove("Filter");
                        }
                        stream_dict.merge(&tmp);
                    } else {
                        spc_warn!(spe, "Parsing dictionary failed.");
                        return ERR;
                    }
                }
                /* Users should explicitly close this. */
                spc_push_object(&ident, fstream.into_obj());
                Ok(())
            } else {
                spc_warn!(spe, "Invalid type of input string for pdf:(f)stream.");
                crate::release!(tmp);
                ERR
            }
        } else {
            spc_warn!(spe, "Missing input string for pdf:(f)stream.");
            ERR
        }
    } else {
        spc_warn!(spe, "Missing objname for pdf:(f)stream.");
        ERR
    }
}
/*
 * STREAM: Create a PDF stream object from an input string.
 *
 *  pdf: stream @objname (input_string) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_stream(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    spc_handler_pdfm_stream_with_type(spe, args, 0)
}
/*
 * FSTREAM: Create a PDF stream object from an existing file.
 *
 *  pdf: fstream @objname (filename) [PDF_DICT]
 */
unsafe fn spc_handler_pdfm_fstream(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    spc_handler_pdfm_stream_with_type(spe, args, 1)
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
unsafe fn spc_handler_pdfm_bform(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        let ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
            ti
        } else {
            return ERR;
        };
        /* A XForm with zero dimension results in a non-invertible transformation
         * matrix. And it may result in unpredictable behaviour. It might be an
         * error in Acrobat. Bounding box with zero dimension may cause division
         * by zero.
         */
        let mut cropbox = if ti.flags & 1 << 0 != 0 {
            if ti.bbox.size().width == 0. || ti.bbox.size().height == 0. {
                spc_warn!(spe, "Bounding box has a zero dimension.");
                return ERR;
            }
            ti.bbox
        } else {
            if ti.width == 0.0f64 || ti.depth + ti.height == 0.0f64 {
                spc_warn!(spe, "Bounding box has a zero dimension.");
                return ERR;
            }
            Rect::new(point2(0., -ti.depth), point2(ti.width, ti.height))
        };
        let xobj_id = pdf_doc_mut().begin_grabbing(&ident, spe.x_user, spe.y_user, &mut cropbox);
        if xobj_id < 0 {
            spc_warn!(spe, "Couldn\'t start form object.");
            return ERR;
        }
        spc_push_object(&ident, pdf_ximage_get_reference(xobj_id));
        Ok(())
    } else {
        spc_warn!(spe, "A form XObject must have name.");
        ERR
    }
}
/* An extra dictionary after exobj must be merged to the form dictionary,
 * not resource dictionary.
 * Please use pdf:put @resources (before pdf:exobj) instead.
 */
unsafe fn spc_handler_pdfm_eform(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    let attrib = if !args.cur.is_empty() {
        args.cur
            .parse_pdf_dict(ptr::null_mut())
            .map(IntoObj::into_obj)
            .unwrap_or(ptr::null_mut())
    } else {
        ptr::null_mut()
    };
    pdf_doc_mut().end_grabbing(attrib);
    Ok(())
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
unsafe fn spc_handler_pdfm_uxobj(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    args.cur.skip_white();
    if let Some(ident) = args.cur.parse_opt_ident() {
        let mut ti = if !args.cur.is_empty() {
            if let Ok(ti) = spc_util_read_dimtrns(spe, args, 0) {
                ti
            } else {
                return ERR;
            }
        } else {
            transform_info::new()
        };
        /* Dvipdfmx was suddenly changed to use file name to identify
         * external images. We can't use ident to find image resource
         * here.
         */
        let mut xobj_id = findresource(sd, Some(&ident));
        if xobj_id < 0 {
            let options: load_options = load_options {
                page_no: 1,
                bbox_type: PdfPageBoundary::Auto,
                dict: ptr::null_mut(),
            };
            xobj_id = pdf_ximage_findresource(&ident, options);
            if xobj_id < 0 {
                spc_warn!(spe, "Specified (image) object doesn\'t exist: {}", ident);
                return ERR;
            }
        }
        pdf_dev_put_image(xobj_id, &mut ti, spe.x_user, spe.y_user);
        Ok(())
    } else {
        spc_warn!(spe, "No object identifier given.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_link(spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    spc_resume_annot(spe)
}
unsafe fn spc_handler_pdfm_nolink(spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    spc_suspend_annot(spe)
}
/* Handled at BOP */
unsafe fn spc_handler_pdfm_pagesize(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur = &[];
    Ok(())
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_pdfm_bgcolor(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    match spc_util_read_pdfcolor(spe, args, None) {
        Ok(colorspec) => {
            pdf_doc_set_bgcolor(Some(&colorspec));
            Ok(())
        }
        Err(_) => {
            spc_warn!(spe, "No valid color specified?");
            ERR
        }
    }
}
unsafe fn spc_handler_pdfm_mapline(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
    let mut error = Ok(());
    ap.cur.skip_white();
    if ap.cur.is_empty() {
        spc_warn!(spe, "Empty mapline special?");
        return ERR;
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
                error = ERR;
            }
        }
        _ => {
            let s = String::from_utf8(ap.cur.into()).unwrap();
            let mut mrec = pdf_init_fontmap_record();
            error = pdf_read_fontmap_line(&mut mrec, &s, is_pdfm_mapline(&s));
            if error.is_err() {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr == b'+' {
                pdf_append_fontmap_record(&mrec.map_name, &mrec);
            } else {
                pdf_insert_fontmap_record(&mrec.map_name, &mrec).ok();
            }
        }
    }
    if error.is_ok() {
        ap.cur = &[];
    }
    Ok(())
}
unsafe fn spc_handler_pdfm_mapfile(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if args.cur.is_empty() {
        return Ok(());
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
        pdf_load_fontmap_file(&mapfile, mode)
    } else {
        spc_warn!(spe, "No fontmap file specified.");
        ERR
    }
}
unsafe fn spc_handler_pdfm_tounicode(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let sd = &mut _PDF_STAT;
    /* First clear */
    sd.cd.cmap_id = None;
    sd.cd.unescape_backslash = 0;
    args.cur.skip_white();
    if args.cur.is_empty() {
        spc_warn!(spe, "Missing CMap name for pdf:tounicode.");
        return ERR;
    }
    /* _FIXME_
     * Any valid char allowed for PDF name object should be allowed here.
     * The argument to this special should be a PDF name obejct.
     * But it's too late to change this special.
     */
    if let Some(cmap_name) = args.cur.parse_ident() {
        sd.cd.cmap_id = CMap_cache_find(&cmap_name);
        if sd.cd.cmap_id.is_none() {
            spc_warn!(spe, "Failed to load ToUnicode mapping: {}", cmap_name);
            return ERR;
        }
        /* Shift-JIS like encoding may contain backslash in 2nd byte.
         * WARNING: This will add nasty extension to PDF parser.
         */
        if sd.cd.cmap_id.is_some() {
            if cmap_name.contains("RKSJ")
                || cmap_name.contains("B5")
                || cmap_name.contains("GBK")
                || cmap_name.contains("KSC")
            {
                sd.cd.unescape_backslash = 1
            }
        }
        Ok(())
    } else {
        spc_warn!(spe, "Missing ToUnicode mapping name...");
        ERR
    }
}

static PDFM_HANDLERS: phf::Map<&'static str, Handler> = phf::phf_map! {
    "annotation" => spc_handler_pdfm_annot,
    "annotate" => spc_handler_pdfm_annot,
    "annot" => spc_handler_pdfm_annot,
    "ann" => spc_handler_pdfm_annot,
    "outline" => spc_handler_pdfm_outline,
    "out" => spc_handler_pdfm_outline,
    "article" => spc_handler_pdfm_article,
    "art" => spc_handler_pdfm_article,
    "bead" => spc_handler_pdfm_bead,
    "thread" => spc_handler_pdfm_bead,
    "destination" => spc_handler_pdfm_dest,
    "dest" => spc_handler_pdfm_dest,
    "object" => spc_handler_pdfm_object,
    "obj" => spc_handler_pdfm_object,
    "docinfo" => spc_handler_pdfm_docinfo,
    "docview" => spc_handler_pdfm_docview,
    "content" => spc_handler_pdfm_content,
    "put" => spc_handler_pdfm_put,
    "close" => spc_handler_pdfm_close,
    "bop" => spc_handler_pdfm_bop,
    "eop" => spc_handler_pdfm_eop,
    "image" => spc_handler_pdfm_image,
    "img" => spc_handler_pdfm_image,
    "epdf" => spc_handler_pdfm_image,
    "link" => spc_handler_pdfm_link,
    "nolink" => spc_handler_pdfm_nolink,
    "begincolor" => spc_handler_pdfm_bcolor,
    "bcolor" => spc_handler_pdfm_bcolor,
    "bc" => spc_handler_pdfm_bcolor,
    "setcolor" => spc_handler_pdfm_scolor,
    "scolor" => spc_handler_pdfm_scolor,
    "sc" => spc_handler_pdfm_scolor,
    "endcolor" => spc_handler_pdfm_ecolor,
    "ecolor" => spc_handler_pdfm_ecolor,
    "ec" => spc_handler_pdfm_ecolor,
    "begingray" => spc_handler_pdfm_bcolor,
    "bgray" => spc_handler_pdfm_bcolor,
    "bg" => spc_handler_pdfm_bcolor,
    "endgray" => spc_handler_pdfm_ecolor,
    "egray" => spc_handler_pdfm_ecolor,
    "eg" => spc_handler_pdfm_ecolor,
    "bgcolor" => spc_handler_pdfm_bgcolor,
    "bgc" => spc_handler_pdfm_bgcolor,
    "bbc" => spc_handler_pdfm_bgcolor,
    "bbg" => spc_handler_pdfm_bgcolor,
    "pagesize" => spc_handler_pdfm_pagesize,
    "bannot" => spc_handler_pdfm_bann,
    "beginann" => spc_handler_pdfm_bann,
    "bann" => spc_handler_pdfm_bann,
    "eannot" => spc_handler_pdfm_eann,
    "endann" => spc_handler_pdfm_eann,
    "eann" => spc_handler_pdfm_eann,
    "btrans" => spc_handler_pdfm_btrans,
    "begintransform" => spc_handler_pdfm_btrans,
    "begintrans" => spc_handler_pdfm_btrans,
    "bt" => spc_handler_pdfm_btrans,
    "etrans" => spc_handler_pdfm_etrans,
    "endtransform" => spc_handler_pdfm_etrans,
    "endtrans" => spc_handler_pdfm_etrans,
    "et" => spc_handler_pdfm_etrans,
    "bform" => spc_handler_pdfm_bform,
    "beginxobj" => spc_handler_pdfm_bform,
    "bxobj" => spc_handler_pdfm_bform,
    "eform" => spc_handler_pdfm_eform,
    "endxobj" => spc_handler_pdfm_eform,
    "exobj" => spc_handler_pdfm_eform,
    "usexobj" => spc_handler_pdfm_uxobj,
    "uxobj" => spc_handler_pdfm_uxobj,
    "tounicode" => spc_handler_pdfm_tounicode,
    "literal" => spc_handler_pdfm_literal,
    "stream" => spc_handler_pdfm_stream,
    "fstream" => spc_handler_pdfm_fstream,
    "names" => spc_handler_pdfm_names,
    "mapline" => spc_handler_pdfm_mapline,
    "mapfile" => spc_handler_pdfm_mapfile,
    "bcontent" => spc_handler_pdfm_bcontent,
    "econtent" => spc_handler_pdfm_econtent,
    "code" => spc_handler_pdfm_code,
    "minorversion" => spc_handler_pdfm_do_nothing,
    "encrypt" => spc_handler_pdfm_do_nothing,
};
pub(crate) fn spc_pdfm_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"pdf:")
}

pub(crate) unsafe fn spc_pdfm_setup_handler(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<Handler> {
    ap.cur.skip_white();
    if let Some(cur) = ap.cur.strip_prefix(b"pdf:") {
        ap.cur = cur;
        ap.cur.skip_white();
        if let Some(q) = ap.cur.parse_c_ident() {
            if let Some((key, &exec)) = PDFM_HANDLERS.get_entry(q.as_str()) {
                ap.command = Some(key);
                ap.cur.skip_white();
                return Ok(exec);
            }
        }
        ERROR()
    } else {
        spc_warn!(spe, "Not pdf: special???");
        ERROR()
    }
}
