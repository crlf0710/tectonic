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
    unused_assignments
)]

use euclid::point2;

use crate::bridge::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use crate::strstartswith;
use crate::warn;

use super::dpx_dvipdfmx::translate_origin;
use super::dpx_fontmap::fontmap;
use super::dpx_pdfcolor::PdfColor;
use super::dpx_pdfdev::{
    dev_unit_dviunit, graphics_mode, pdf_dev_get_dirmode, pdf_dev_get_font_wmode,
    pdf_dev_get_param, pdf_dev_locate_font, pdf_dev_put_image, pdf_dev_set_dirmode,
    pdf_dev_set_param, pdf_dev_set_string, transform_info, transform_info_clear, Point, Rect,
    TMatrix,
};
use super::dpx_pdfdoc::{pdf_doc_begin_grabbing, pdf_doc_end_grabbing};
use super::dpx_pdfdraw::{
    pdf_dev_arc, pdf_dev_arcn, pdf_dev_clip, pdf_dev_closepath, pdf_dev_concat,
    pdf_dev_currentmatrix, pdf_dev_currentpoint, pdf_dev_curveto, pdf_dev_dtransform,
    pdf_dev_eoclip, pdf_dev_flushpath, pdf_dev_grestore, pdf_dev_gsave, pdf_dev_idtransform,
    pdf_dev_lineto, pdf_dev_moveto, pdf_dev_newpath, pdf_dev_rcurveto, pdf_dev_rlineto,
    pdf_dev_rmoveto, pdf_dev_set_color, pdf_dev_setdash, pdf_dev_setlinecap, pdf_dev_setlinejoin,
    pdf_dev_setlinewidth, pdf_dev_setmiterlimit,
};
use super::dpx_pdfparse::dump;
use super::dpx_subfont::{lookup_sfd_record, sfd_load_record};
use super::dpx_tfm::{tfm_exists, tfm_get_width, tfm_open, tfm_string_width};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_name, pdf_release_obj, pdf_set_number, IntoObj, PdfObjVariant, PushObj,
};
use crate::dpx_pdfparse::{
    parse_number, pdfparse_skip_line, skip_white, ParseIdent, ParsePdfObj, SkipWhite,
};
use libc::{atof, free, strtod};

pub(crate) type __off_t = i64;
pub(crate) type __off64_t = i64;
use crate::bridge::size_t;

pub(crate) type spt_t = i32;

#[derive(Clone, Copy)]
pub(crate) enum Opcode {
    Add = 1,
    Sub = 2,
    Mul = 3,
    Div = 4,
    Neg = 5,
    Truncate = 6,

    Clear = 10,
    ExCh = 11,
    Pop = 12,

    NewPath = 31,
    ClosePath = 32,
    MoveTo = 33,
    RMoveTo = 34,
    CurveTo = 35,
    RCurveTo = 36,
    LineTo = 37,
    RLineTo = 38,
    Arc = 39,
    ArcN = 40,

    Fill = 41,
    Stroke = 42,
    Show = 43,

    Clip = 44,
    EoClip = 45,

    ShowPage = 49,

    GSave = 50,
    GRestore = 51,

    Concat = 52,
    Scale = 53,
    Translate = 54,
    Rotate = 55,

    SetLineWidth = 60,
    SetDash = 61,
    SetLineCap = 62,
    SetLineJoin = 63,
    SetMIterLimit = 64,

    SetGray = 70,
    SetRgbColor = 71,
    SetCmykColor = 72,

    CurrentPoint = 80,
    IDTransform = 81,
    DTransform = 82,

    FindFont = 201,
    ScaleFont = 202,
    SetFont = 203,
    CurrentFont = 204,

    StringWidth = 210,

    Def = 999,

    FShow = 1001,
    STexFig = 1002,
    ETexFig = 1003,
    Hlw = 1004,
    Vlw = 1005,
    Rd = 1006,
    B = 1007,
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct mp_font {
    pub(crate) font_name: String,
    pub(crate) font_id: i32,
    pub(crate) tfm_id: i32,
    pub(crate) subfont_id: i32,
    pub(crate) pt_size: f64,
}
struct operators {
    pub(crate) token: &'static [u8],
    pub(crate) opcode: Opcode,
}
impl operators {
    const fn new(token: &'static [u8], opcode: Opcode) -> Self {
        Self { token, opcode }
    }
}

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/*
 * Define the origin as (llx, lly) in order to
 * match the new xetex.def and dvipdfmx.def
 */
static mut Xorigin: f64 = 0.;
static mut Yorigin: f64 = 0.;

static mut font_stack: Vec<mp_font> = Vec::new();

static mut currentfont: i32 = -1i32;
static mut mp_cmode: i32 = 0i32;
unsafe fn mp_setfont(font_name: &str, pt_size: f64) -> i32 {
    let mut subfont_id: i32 = -1i32;
    if let Some(font) = font_stack.last() {
        if font.font_name == font_name && (font.pt_size == pt_size) {
            return 0;
        }
    }
    let mrec = fontmap.get(font_name);
    if let Some(mrec) = mrec {
        if !mrec.charmap.sfd_name.is_empty() && !mrec.charmap.subfont_id.is_empty() {
            subfont_id = sfd_load_record(&mrec.charmap.sfd_name, &mrec.charmap.subfont_id)
        }
    }
    /* See comments in dvi_locate_font() in dvi.c. */
    let name = match mrec {
        Some(mrec) if !mrec.map_name.is_empty() => &mrec.map_name,
        _ => font_name,
    };
    let font_id = pdf_dev_locate_font(name, (pt_size * dev_unit_dviunit()) as spt_t);
    let new_font = mp_font {
        font_name: font_name.to_string(),
        font_id,
        tfm_id: tfm_open(font_name, 0),
        subfont_id,
        pt_size,
    };
    if let Some(font) = font_stack.last_mut() {
        *font = new_font;
    } else {
        /* ***TODO*** Here some problem exists! */
        font_stack.push(new_font);
    }
    if font_id < 0 {
        panic!("MPOST: No physical font assigned for \"{}\".", font_name);
    }
    0
}
unsafe fn save_font() {
    match font_stack.last() {
        Some(current) => font_stack.push(current.clone()),
        None => font_stack.push(mp_font {
            font_name: "Courier".to_string(),
            font_id: -1,
            tfm_id: 0,
            subfont_id: 0,
            pt_size: 1.,
        }),
    }
}
unsafe fn restore_font() {
    font_stack.pop().expect("No currentfont...");
}
unsafe fn clear_fonts() {
    font_stack = vec![];
}
unsafe fn is_fontname(token: &[u8]) -> bool {
    if let Ok(token) = std::str::from_utf8(token) {
        if fontmap.contains_key(token) {
            return true;
        }
        tfm_exists(token)
    } else {
        false
    }
}

pub(crate) unsafe fn mps_scan_bbox(pp: *mut *const i8, endptr: *const i8, bbox: &mut Rect) -> i32 {
    let mut values: [f64; 4] = [0.; 4];
    /* skip_white() skips lines starting '%'... */
    while *pp < endptr && libc::isspace(**pp as _) != 0 {
        *pp = (*pp).offset(1)
    }
    /* Scan for bounding box record */
    while *pp < endptr && **pp as i32 == '%' as i32 {
        if (*pp).offset(14) < endptr
            && !strstartswith(*pp, b"%%BoundingBox:\x00" as *const u8 as *const i8).is_null()
        {
            *pp = (*pp).offset(14);
            let mut i = 0;
            while i < 4i32 {
                skip_white(pp, endptr);
                let number = parse_number(pp, endptr);
                if number.is_null() {
                    break;
                }
                values[i as usize] = atof(number);
                free(number as *mut libc::c_void);
                i += 1
            }
            if i < 4i32 {
                return -1i32;
            } else {
                /* The new xetex.def and dvipdfmx.def require bbox->llx = bbox->lly = 0.  */
                if translate_origin != 0 {
                    *bbox = Rect::new(
                        Point::zero(),
                        point2(values[2] - values[0], values[3] - values[1]),
                    );
                    Xorigin = values[0];
                    Yorigin = values[1];
                } else {
                    *bbox = Rect::new(point2(values[0], values[1]), point2(values[2], values[3]));
                    Xorigin = 0.;
                    Yorigin = 0.
                }
                return 0i32;
            }
        }
        pdfparse_skip_line(pp, endptr);
        while *pp < endptr && libc::isspace(**pp as _) != 0 {
            *pp = (*pp).offset(1)
        }
    }
    -1i32
}
static mut ps_operators: [operators; 48] = {
    use Opcode::*;
    [
        operators::new(b"add", Add),
        operators::new(b"mul", Mul),
        operators::new(b"div", Div),
        operators::new(b"neg", Neg),
        operators::new(b"sub", Sub),
        operators::new(b"truncate", Truncate),
        operators::new(b"clear", Clear),
        operators::new(b"exch", ExCh),
        operators::new(b"pop", Pop),
        operators::new(b"clip", Clip),
        operators::new(b"eoclip", EoClip),
        operators::new(b"closepath", ClosePath),
        operators::new(b"concat", Concat),
        operators::new(b"newpath", NewPath),
        operators::new(b"moveto", MoveTo),
        operators::new(b"rmoveto", RMoveTo),
        operators::new(b"lineto", LineTo),
        operators::new(b"rlineto", RLineTo),
        operators::new(b"curveto", CurveTo),
        operators::new(b"rcurveto", RCurveTo),
        operators::new(b"arc", Arc),
        operators::new(b"arcn", ArcN),
        operators::new(b"stroke", Stroke),
        operators::new(b"fill", Fill),
        operators::new(b"show", Show),
        operators::new(b"showpage", ShowPage),
        operators::new(b"gsave", GSave),
        operators::new(b"grestore", GRestore),
        operators::new(b"translate", Translate),
        operators::new(b"rotate", Rotate),
        operators::new(b"scale", Scale),
        operators::new(b"setlinecap", SetLineCap),
        operators::new(b"setlinejoin", SetLineJoin),
        operators::new(b"setlinewidth", SetLineWidth),
        operators::new(b"setmiterlimit", SetMIterLimit),
        operators::new(b"setdash", SetDash),
        operators::new(b"setgray", SetGray),
        operators::new(b"setrgbcolor", SetRgbColor),
        operators::new(b"setcmykcolor", SetCmykColor),
        operators::new(b"currentpoint", CurrentPoint),
        operators::new(b"dtransform", DTransform),
        operators::new(b"idtransform", IDTransform),
        operators::new(b"findfont", FindFont),
        operators::new(b"scalefont", ScaleFont),
        operators::new(b"setfont", SetFont),
        operators::new(b"currentfont", CurrentFont),
        operators::new(b"stringwidth", StringWidth),
        operators::new(b"def", Def),
    ]
};
static mut mps_operators: [operators; 28] = {
    use Opcode::*;
    [
        operators::new(b"fshow", FShow),
        operators::new(b"startTexFig", STexFig),
        operators::new(b"endTexFig", ETexFig),
        operators::new(b"hlw", Hlw),
        operators::new(b"vlw", Vlw),
        operators::new(b"l", LineTo),
        operators::new(b"r", RLineTo),
        operators::new(b"c", CurveTo),
        operators::new(b"m", MoveTo),
        operators::new(b"p", ClosePath),
        operators::new(b"n", NewPath),
        operators::new(b"C", SetCmykColor),
        operators::new(b"G", SetGray),
        operators::new(b"R", SetRgbColor),
        operators::new(b"lj", SetLineJoin),
        operators::new(b"ml", SetMIterLimit),
        operators::new(b"lc", SetLineCap),
        operators::new(b"S", Stroke),
        operators::new(b"F", Fill),
        operators::new(b"q", GSave),
        operators::new(b"Q", GRestore),
        operators::new(b"s", Scale),
        operators::new(b"t", Concat),
        operators::new(b"sd", SetDash),
        operators::new(b"rd", Rd),
        operators::new(b"P", ShowPage),
        operators::new(b"B", B),
        operators::new(b"W", Clip),
    ]
};

unsafe fn get_opcode(token: &[u8]) -> Result<Opcode, ()> {
    for op in ps_operators.iter() {
        if token == op.token {
            return Ok(op.opcode);
        }
    }
    for op in mps_operators.iter() {
        if token == op.token {
            return Ok(op.opcode);
        }
    }
    Err(())
}
static mut STACK: Vec<PdfObjVariant> = Vec::new();
trait PushChecked {
    fn push_checked<T>(&mut self, val: T) -> Result<(), ()>
    where
        T: IntoObj;
}
impl PushChecked for Vec<PdfObjVariant> {
    fn push_checked<T>(&mut self, val: T) -> Result<(), ()>
    where
        T: IntoObj,
    {
        if self.len() < 1024 {
            self.push(val.into_obj_variant());
            Ok(())
        } else {
            warn!("PS stack overflow including MetaPost file or inline PS code");
            Err(())
        }
    }
}

unsafe fn do_exch() -> i32 {
    let len = STACK.len();
    if len < 2 {
        return -1;
    }
    STACK.swap(len - 1, len - 2);
    0
}
unsafe fn do_clear() -> i32 {
    STACK.clear();
    0
}
unsafe fn pop_get_numbers(values: &mut [f64]) -> i32 {
    let mut count = values.len();
    loop {
        if count == 0 {
            break;
        }
        count -= 1;
        if let Some(tmp) = STACK.pop() {
            if !tmp.is_number() {
                warn!("mpost: Not a number!");
                count += 1;
                break;
            } else {
                values[count] = tmp.as_f64();
            }
        } else {
            warn!("mpost: Stack underflow.");
            count += 1;
            break;
        }
    }
    count as i32
}
unsafe fn cvr_array(array: PdfObjVariant, values: &mut [f64]) -> i32 {
    let mut count = values.len();
    if let PdfObjVariant::ARRAY(array) = array {
        loop {
            let fresh2 = count;
            count -= 1;
            if !(fresh2 > 0) {
                break;
            }
            let tmp = array[count];
            if !(*tmp).is_number() {
                warn!("mpost: Not a number!");
                break;
            } else {
                values[count] = (*tmp).as_f64()
            }
        }
    } else {
        warn!("mpost: Not an array!");
    }
    (count + 1) as i32
}
unsafe fn is_fontdict(dict: &PdfObjVariant) -> bool {
    if let PdfObjVariant::DICT(d) = &dict {
        if let Some(typ) = d.get("Type") {
            if matches!(&typ.data, PdfObjVariant::NAME(typ) if typ.to_bytes() == b"Font") {
                if let Some(name) = d.get("FontName") {
                    if let PdfObjVariant::NAME(_) = name.data {
                        if let Some(scale) = d.get("FontScale") {
                            return matches!(scale.data, PdfObjVariant::NUMBER(_));
                        }
                    }
                }
            }
        }
        false
    } else {
        false
    }
}
unsafe fn do_findfont() -> i32 {
    let mut error = 0;
    if let Some(font_name) = STACK.pop() {
        match &font_name {
            PdfObjVariant::STRING(_) | PdfObjVariant::NAME(_) => {
                /* Do not check the existence...
                 * The reason for this is that we cannot locate PK font without
                 * font scale.
                 */
                let mut font_dict = pdf_dict::new();
                font_dict.set("Type", "Font");
                match &font_name {
                    PdfObjVariant::STRING(font_name) => {
                        font_dict.set("FontName", pdf_name::new(font_name.to_bytes_without_nul()));
                    }
                    PdfObjVariant::NAME(_) => {
                        font_dict.set("FontName", font_name);
                    }
                    _ => unreachable!(),
                }
                font_dict.set("FontScale", 1_f64);
                let font_dict = font_dict.into_obj();
                if STACK.push_checked(font_dict).is_err() {
                    pdf_release_obj(font_dict);
                    error = 1
                }
            }
            _ => error = 1,
        }
    } else {
        return 1;
    }
    error
}
unsafe fn do_scalefont() -> i32 {
    let mut scale = [0.; 1];
    let mut error = pop_get_numbers(scale.as_mut());
    if error != 0 {
        return error;
    }
    if let Some(mut font_dict) = STACK.pop() {
        if is_fontdict(&font_dict) {
            let font_scale = font_dict.as_dict_mut().get_mut("FontScale").unwrap();
            let val = (*font_scale).as_f64() * scale[0];
            pdf_set_number(&mut *font_scale, val);
            if STACK.push_checked(font_dict).is_err() {
                error = 1i32
            }
        } else {
            error = 1i32
        }
        error
    } else {
        1
    }
}
unsafe fn do_setfont() -> i32 {
    if let Some(font_dict) = STACK.pop() {
        let error = if !is_fontdict(&font_dict) {
            1
        } else {
            /* Subfont support prevent us from managing
             * font in a single place...
             */
            let font_name = (font_dict.as_dict().get("FontName").unwrap()).as_name();
            let font_scale = font_dict.as_dict().get("FontScale").unwrap().as_f64();
            mp_setfont(font_name.to_str().unwrap(), font_scale)
        };
        error
    } else {
        1
    }
}
/* Push dummy font dict onto PS STACK */
unsafe fn do_currentfont() -> i32 {
    let mut error: i32 = 0i32; /* Should not be error... */
    /* Should not be error... */
    let font = if currentfont < 0i32 {
        ptr::null_mut()
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if font.is_null() {
        warn!("Currentfont undefined...");
        return 1i32;
    } else {
        let mut font_dict = pdf_dict::new();
        font_dict.set("Type", "Font");
        font_dict.set("FontName", pdf_name::new((*font).font_name.as_bytes()));
        font_dict.set("FontScale", (*font).pt_size);
        if STACK.len() < 1024 {
            STACK.push_obj(font_dict)
        } else {
            warn!("PS stack overflow...");
            error = 1i32
        }
    }
    error
}
unsafe fn do_show() -> i32 {
    let mut cp = Point::zero();
    let font = if currentfont < 0i32 {
        ptr::null_mut()
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if font.is_null() {
        warn!("Currentfont not set.");
        return 1i32;
    }
    pdf_dev_currentpoint(&mut cp);
    if let Some(text) = STACK.pop() {
        if let PdfObjVariant::STRING(text) = &text {
            if (*font).font_id < 0 {
                warn!("mpost: not set.");
                return 1i32;
            }
            let text = text.to_bytes();
            if (*font).tfm_id < 0 {
                warn!("mpost: TFM not found for \"{}\".", (*font).font_name);
                warn!("mpost: Text width not calculated...");
            }
            let mut text_width = 0_f64;
            if (*font).subfont_id >= 0 {
                let mut ustr = Vec::with_capacity(text.len() * 2);
                for i in 0..text.len() {
                    let uch = lookup_sfd_record((*font).subfont_id, text[i]);
                    ustr.push((uch as i32 >> 8) as u8);
                    ustr.push((uch as i32 & 0xff) as u8);
                    if (*font).tfm_id >= 0 {
                        text_width += tfm_get_width((*font).tfm_id, text[i] as i32)
                    }
                }
                text_width *= (*font).pt_size;
                pdf_dev_set_string(
                    (cp.x * dev_unit_dviunit()) as spt_t,
                    (cp.y * dev_unit_dviunit()) as spt_t,
                    ustr.as_ptr() as *const libc::c_void,
                    ustr.len() as size_t,
                    (text_width * dev_unit_dviunit()) as spt_t,
                    (*font).font_id,
                    0i32,
                );
            } else {
                if (*font).tfm_id >= 0i32 {
                    text_width =
                        tfm_string_width((*font).tfm_id, text) as f64 / (1i32 << 20i32) as f64;
                    text_width *= (*font).pt_size
                }
                pdf_dev_set_string(
                    (cp.x * dev_unit_dviunit()) as spt_t,
                    (cp.y * dev_unit_dviunit()) as spt_t,
                    text.as_ptr() as *const libc::c_void,
                    text.len() as size_t,
                    (text_width * dev_unit_dviunit()) as spt_t,
                    (*font).font_id,
                    0i32,
                );
            }
            if pdf_dev_get_font_wmode((*font).font_id) != 0 {
                pdf_dev_rmoveto(0.0f64, -text_width);
            } else {
                pdf_dev_rmoveto(text_width, 0.0f64);
            }
            graphics_mode();
            0
        } else {
            -1
        }
    } else {
        -1
    }
}
unsafe fn do_mpost_bind_def(ps_code: *const i8, x_user: f64, y_user: f64) -> i32 {
    let mut start = CStr::from_ptr(ps_code).to_bytes();
    mp_parse_body(&mut start, x_user, y_user)
}
unsafe fn do_texfig_operator(opcode: Opcode, x_user: f64, y_user: f64) -> i32 {
    static mut fig_p: transform_info = transform_info::new();
    static mut in_tfig: i32 = 0i32;
    static mut xobj_id: i32 = -1i32;
    static mut count: i32 = 0i32;
    let mut values: [f64; 6] = [0.; 6];
    let mut error: i32 = 0i32;
    match opcode {
        Opcode::STexFig => {
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                transform_info_clear(&mut fig_p);
                let dvi2pts = 1.0f64 / dev_unit_dviunit();
                fig_p.width = values[0] * dvi2pts;
                fig_p.height = values[1] * dvi2pts;
                fig_p.bbox.min.x = values[2] * dvi2pts;
                fig_p.bbox.min.y = -values[3] * dvi2pts;
                fig_p.bbox.max.x = values[4] * dvi2pts;
                fig_p.bbox.max.y = -values[5] * dvi2pts;
                fig_p.flags |= 1i32 << 0i32;
                let resname = format!("__tf{}__", count);
                xobj_id = pdf_doc_begin_grabbing(
                    &resname,
                    fig_p.bbox.min.x,
                    fig_p.bbox.max.y,
                    &mut fig_p.bbox,
                );
                in_tfig = 1i32;
                count += 1
            }
        }
        Opcode::ETexFig => {
            if in_tfig == 0 {
                panic!("endTexFig without valid startTexFig!.");
            }
            pdf_doc_end_grabbing(ptr::null_mut());
            pdf_dev_put_image(xobj_id, &mut fig_p, x_user, y_user);
            in_tfig = 0i32
        }
        _ => error = 1i32,
    }
    error
}
unsafe fn ps_dev_CTM() -> TMatrix {
    let mut M = pdf_dev_currentmatrix();
    M.m11 *= 1000.;
    M.m12 *= 1000.;
    M.m21 *= 1000.;
    M.m22 *= 1000.;
    M.m31 *= 1000.;
    M.m32 *= 1000.;
    M
}
/*
 * Again, the only piece that needs x_user and y_user is
 * that piece dealing with texfig.
 */
unsafe fn do_operator(token: &[u8], x_user: f64, y_user: f64) -> i32 {
    let mut error: i32 = 0i32;
    let mut tmp = None;
    let mut cp = Point::zero();
    let opcode = get_opcode(token);
    if opcode.is_err() {
        if is_fontname(token) {
            if STACK.push_checked(pdf_name::new(token)).is_err() {
                return 1;
            }
        } else {
            warn!("Unknown token \"{}\"", token.display());
            return 1;
        }
        return -1;
    }
    let opcode = opcode.unwrap();
    match opcode {
        Opcode::Add => {
            /*
             * Arithmetic operators
             */
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK.push_checked(values[0] + values[1]).is_err() {
                    error = 1i32
                }
            }
        }
        Opcode::Mul => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK.push_checked(values[0] * values[1]).is_err() {
                    error = 1i32
                }
            }
        }
        Opcode::Neg => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK.push_checked(-values[0]).is_err() {
                    error = 1i32
                }
            }
        }
        Opcode::Sub => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK.push_checked(values[0] - values[1]).is_err() {
                    error = 1i32
                }
            }
        }
        Opcode::Div => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK.push_checked(values[0] / values[1]).is_err() {
                    error = 1i32
                }
            }
        }
        Opcode::Truncate => {
            /* Round toward zero. */
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                if STACK
                    .push_checked(if values[0] > 0. {
                        values[0].floor()
                    } else {
                        values[0].ceil()
                    })
                    .is_err()
                {
                    error = 1;
                }
            }
        }
        Opcode::Clear => {
            /* STACK operation */
            error = do_clear()
        }
        Opcode::Pop => {
            STACK.pop();
        }
        Opcode::ExCh => error = do_exch(),
        Opcode::MoveTo => {
            /* Path construction */
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_moveto(values[0], values[1])
            }
        }
        Opcode::RMoveTo => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_rmoveto(values[0], values[1])
            }
        }
        Opcode::LineTo => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_lineto(values[0], values[1])
            }
        }
        Opcode::RLineTo => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_rlineto(values[0], values[1])
            }
        }
        Opcode::CurveTo => {
            let mut values = [0.; 6];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_curveto(
                    values[0], values[1], values[2], values[3], values[4], values[5],
                )
            }
        }
        Opcode::RCurveTo => {
            let mut values = [0.; 6];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_rcurveto(
                    values[0], values[1], values[2], values[3], values[4], values[5],
                )
            }
        }
        Opcode::ClosePath => error = pdf_dev_closepath(),
        Opcode::Arc => {
            let mut values = [0.; 5];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_arc(values[0], values[1], values[2], values[3], values[4])
            }
        }
        Opcode::ArcN => {
            let mut values = [0.; 5];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_arcn(values[0], values[1], values[2], values[3], values[4])
            }
        }
        Opcode::NewPath => {
            pdf_dev_newpath();
        }
        Opcode::Stroke => {
            /* fill rule not supported yet */
            pdf_dev_flushpath(b'S', 0);
        }
        Opcode::Fill => {
            pdf_dev_flushpath(b'f', 0);
        }
        Opcode::Clip => error = pdf_dev_clip(),
        Opcode::EoClip => error = pdf_dev_eoclip(),
        Opcode::GSave => {
            /* Graphics state operators: */
            error = pdf_dev_gsave(); /* This does pdf_release_obj() */
            save_font();
        }
        Opcode::GRestore => {
            error = pdf_dev_grestore();
            restore_font();
        }
        Opcode::Concat => {
            tmp = STACK.pop();
            let mut values = [0.; 6];
            error = cvr_array(tmp.unwrap(), values.as_mut()); // TODO: check
            tmp = None;
            if error != 0 {
                warn!("Missing array before \"concat\".");
            } else {
                let mut matrix = TMatrix::from_row_major_array(values);
                error = pdf_dev_concat(&mut matrix)
            }
        }
        Opcode::Scale => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                match mp_cmode {
                    _ => {}
                }
                let mut matrix = TMatrix::create_scale(values[0], values[1]);
                error = pdf_dev_concat(&mut matrix)
            }
        }
        Opcode::Rotate => {
            /* Positive angle means clock-wise direction in graphicx-dvips??? */
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                let mut matrix = match mp_cmode {
                    1 | 0 => {
                        /* Really? */
                        TMatrix::create_rotation(euclid::Angle::degrees(values[0]))
                    }
                    _ => TMatrix::create_rotation(euclid::Angle::degrees(-values[0])),
                };
                error = pdf_dev_concat(&mut matrix)
            }
        }
        Opcode::Translate => {
            let mut values = [0.; 2];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                let mut matrix = TMatrix::create_translation(values[0], values[1]);
                error = pdf_dev_concat(&mut matrix)
            }
        }
        Opcode::SetDash => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                let mut num_dashes = 0_usize;
                let mut dash_values: [f64; 16] = [0.; 16];
                let offset = values[0];
                if let Some(pattern) = STACK.pop() {
                    if let PdfObjVariant::ARRAY(pattern) = pattern {
                        num_dashes = pattern.len();
                        if num_dashes > 16 {
                            warn!("Too many dashes...");
                            error = 1i32
                        } else {
                            let mut i = 0;
                            while i < num_dashes && error == 0 {
                                let dash = pattern[i];
                                if !(*dash).is_number() {
                                    error = 1i32
                                } else {
                                    dash_values[i] = (*dash).as_f64()
                                }
                                i += 1
                            }
                            if error == 0 {
                                error = pdf_dev_setdash(&dash_values[..num_dashes], offset)
                            }
                        }
                    } else {
                        error = 1
                    }
                } else {
                    error = 1;
                }
            }
        }
        Opcode::SetLineCap => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_setlinecap(values[0] as i32)
            }
        }
        Opcode::SetLineJoin => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_setlinejoin(values[0] as i32)
            }
        }
        Opcode::SetLineWidth => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_setlinewidth(values[0])
            }
        }
        Opcode::SetMIterLimit => {
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                error = pdf_dev_setmiterlimit(values[0])
            }
        }
        Opcode::SetCmykColor => {
            let mut values = [0.; 4];
            error = pop_get_numbers(values.as_mut());
            /* Not handled properly */
            if error == 0 {
                let color =
                    PdfColor::from_cmyk(values[0], values[1], values[2], values[3]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        Opcode::SetGray => {
            /* Not handled properly */
            let mut values = [0.; 1];
            error = pop_get_numbers(values.as_mut()); /* This does pdf_release_obj() */
            if error == 0 {
                let color = PdfColor::from_gray(values[0]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        Opcode::SetRgbColor => {
            let mut values = [0.; 3];
            error = pop_get_numbers(values.as_mut());
            if error == 0 {
                let color = PdfColor::from_rgb(values[0], values[1], values[2]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        Opcode::ShowPage => {}
        Opcode::CurrentPoint => {
            error = pdf_dev_currentpoint(&mut cp);
            if error == 0 {
                if STACK.push_checked(cp.x).is_ok() {
                    if STACK.push_checked(cp.y).is_err() {
                        error = 1i32
                    }
                } else {
                    error = 1i32
                }
            }
        }
        Opcode::DTransform => {
            let mut values = [0.; 6];
            let mut matrix = None;
            if let Some(tmp2) = STACK.pop() {
                if let PdfObjVariant::ARRAY(_) = &tmp2 {
                    error = cvr_array(tmp2, values.as_mut());
                    tmp = None;
                    if error == 0 {
                        matrix = Some(TMatrix::from_row_major_array(values));
                        tmp = STACK.pop();
                    }
                } else {
                    tmp = Some(tmp2);
                }
            }
            if error == 0 {
                if let Some(tmp) = tmp.filter(|o| o.is_number()) {
                    cp.y = tmp.as_f64();
                    if let Some(tmp) = STACK.pop().filter(|o| o.is_number()) {
                        cp.x = tmp.as_f64();
                        /* Here, we need real PostScript CTM */
                        let mut matrix = matrix.unwrap_or_else(|| ps_dev_CTM());
                        /* This does pdf_release_obj() */
                        pdf_dev_dtransform(&mut cp, Some(&mut matrix));
                        if STACK.push_checked(cp.x).is_ok() {
                            if STACK.push_checked(cp.y).is_err() {
                                error = 1i32
                            }
                        } else {
                            error = 1i32
                        }
                    } else {
                        error = 1i32
                    }
                } else {
                    error = 1i32
                }
            }
        }
        Opcode::IDTransform => {
            let mut matrix = None;
            let mut values = [0.; 6];
            if let Some(tmp2) = STACK.pop() {
                if let PdfObjVariant::ARRAY(_) = &tmp2 {
                    error = cvr_array(tmp2, values.as_mut());
                    tmp = None;
                    if error == 0 {
                        matrix = Some(TMatrix::from_row_major_array(values));
                        tmp = STACK.pop();
                    }
                } else {
                    tmp = Some(tmp2);
                }
            }
            if error == 0 {
                if let Some(tmp) = tmp.filter(|o| o.is_number()) {
                    cp.y = tmp.as_f64();
                    if let Some(tmp) = STACK.pop().filter(|o| o.is_number()) {
                        cp.x = tmp.as_f64();
                        /* Here, we need real PostScript CTM */
                        let matrix = matrix.unwrap_or_else(|| ps_dev_CTM());
                        pdf_dev_idtransform(&mut cp, Some(&matrix));
                        if STACK.push_checked(cp.x).is_ok() {
                            if STACK.push_checked(cp.y).is_err() {
                                error = 1i32
                            }
                        } else {
                            error = 1i32
                        }
                    } else {
                        error = 1;
                    }
                } else {
                    error = 1;
                }
            }
        }
        Opcode::FindFont => error = do_findfont(),
        Opcode::ScaleFont => error = do_scalefont(),
        Opcode::SetFont => error = do_setfont(),
        Opcode::CurrentFont => error = do_currentfont(),
        Opcode::Show => error = do_show(),
        Opcode::StringWidth => error = 1i32,
        /* Extensions */
        Opcode::FShow => {
            error = do_mpost_bind_def(
                b"exch findfont exch scalefont setfont show\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        Opcode::STexFig | Opcode::ETexFig => error = do_texfig_operator(opcode, x_user, y_user),
        Opcode::Hlw => {
            error = do_mpost_bind_def(
                b"0 dtransform exch truncate exch idtransform pop setlinewidth\x00" as *const u8
                    as *const i8,
                x_user,
                y_user,
            )
        }
        Opcode::Vlw => {
            error = do_mpost_bind_def(
                b"0 exch dtransform truncate idtransform setlinewidth pop\x00" as *const u8
                    as *const i8,
                x_user,
                y_user,
            )
        }
        Opcode::Rd => {
            error = do_mpost_bind_def(
                b"[] 0 setdash\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        Opcode::B => {
            error = do_mpost_bind_def(
                b"gsave fill grestore\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        Opcode::Def => {
            STACK.pop();
            STACK.pop();
        }
    }
    error
}
/*
 * In PDF, current path is not a part of graphics state parameter.
 * Hence, current path is not saved by the "q" operator  and is not
 * recovered by the "Q" operator. This means that the following PS
 * code
 *
 *   <path construction> gsave <path painting> grestore ...
 *
 * can't be translated to PDF code
 *
 *   <path construction> q <path painting> Q ...
 *
 * . Only clipping path (which is graphics state parameter in PDF
 * too) is treated in the same way. So, we write clipping path
 * immediately and forget about it but remember current path.
 */
/*
 * The only sections that need to know x_user and y _user are those
 * dealing with texfig.
 */
unsafe fn mp_parse_body(start: &mut &[u8], x_user: f64, y_user: f64) -> i32 {
    let mut error = 0;
    start.skip_white();
    while !start.is_empty() && error == 0 {
        if start[0].is_ascii_digit()
            || start.len() > 1 && (start[0] == b'+' || start[0] == b'-' || start[0] == b'.')
        {
            let mut next: *mut i8 = ptr::null_mut();
            let value = strtod(start.as_ptr() as *const i8, &mut next);
            let pos = next.offset_from(start.as_ptr() as *const i8) as usize;
            if pos < start.len()
                && !b"<([{/%".contains(&(*next as u8))
                && libc::isspace(start[pos] as _) == 0
            {
                warn!("Unkown PostScript operator.");
                dump(&start[..pos]);
                error = 1
            } else if STACK.push_checked(value).is_ok() {
                *start = &start[pos..];
            } else {
                error = 1;
                break;
            }
        /*
         * PDF parser can't handle PS operator inside arrays.
         * This shouldn't use parse_pdf_array().
         */
        } else {
            let mut parsed = false;
            if start[0] == b'[' {
                if let Some(array) = start.parse_pdf_array(ptr::null_mut()) {
                    parsed = true;
                    if STACK.push_checked(array).is_err() {
                        error = 1;
                        break;
                    }
                }
            } else if start.len() > 1 && (start[0] == b'<' && start[1] == b'<') {
                /* This cannot handle ASCII85 string. */
                if let Some(dict) = start.parse_pdf_dict(ptr::null_mut()) {
                    parsed = true;
                    if STACK.push_checked(dict).is_err() {
                        error = 1;
                        break;
                    }
                }
            } else if start[0] == b'(' || start[0] == b'<' {
                if let Some(string) = start.parse_pdf_string() {
                    parsed = true;
                    if STACK.push_checked(string).is_err() {
                        error = 1;
                        break;
                    }
                }
            } else if start[0] == b'/' {
                if let Some(name) = start.parse_pdf_name() {
                    parsed = true;
                    if STACK.push_checked(name).is_err() {
                        error = 1;
                        break;
                    }
                }
            }
            if !parsed {
                error = if let Some(token) = start.parse_ident() {
                    do_operator(token.as_bytes(), x_user, y_user)
                } else {
                    1
                };
            }
        }
        start.skip_white();
    }
    error
}

pub(crate) unsafe fn mps_eop_cleanup() {
    clear_fonts();
    do_clear();
}

pub(crate) unsafe fn mps_stack_depth() -> i32 {
    STACK.len() as i32
}

pub(crate) unsafe fn mps_exec_inline(pp: &mut &[u8], x_user: f64, y_user: f64) -> i32 {
    /* Compatibility for dvipsk. */
    let dirmode = pdf_dev_get_dirmode();
    if dirmode != 0 {
        mp_cmode = 2i32
    } else {
        mp_cmode = 1i32
    }
    let autorotate = pdf_dev_get_param(1i32);
    pdf_dev_set_param(1i32, 0i32);
    //pdf_color_push(); /* ... */
    /* Comment in dvipdfm:
     * Remember that x_user and y_user are off by 0.02 %
     */
    pdf_dev_moveto(x_user, y_user);

    let error = mp_parse_body(pp, x_user, y_user);

    //pdf_color_pop(); /* ... */
    pdf_dev_set_param(1i32, autorotate);
    pdf_dev_set_dirmode(dirmode);
    error
}
