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
    unused_assignments,
    unused_mut
)]

use crate::DisplayExt;
use std::ffi::{CStr, CString};

use crate::warn;
use crate::strstartswith;

use super::dpx_dvipdfmx::translate_origin;
use super::dpx_fontmap::pdf_lookup_fontmap_record;
use super::dpx_mem::new;
//use super::dpx_mfileio::file_size;
use super::dpx_pdfcolor::PdfColor;
use super::dpx_pdfdev::{
    dev_unit_dviunit, graphics_mode, Coord, pdf_dev_get_dirmode, pdf_dev_get_font_wmode,
    pdf_dev_get_param, pdf_dev_locate_font, pdf_dev_put_image, pdf_dev_set_dirmode,
    pdf_dev_set_param, pdf_dev_set_string, Rect, TMatrix, transform_info,
    transform_info_clear,
};
use super::dpx_pdfdoc::{
    pdf_doc_begin_grabbing, /*pdf_doc_begin_page, pdf_doc_current_page_number,*/ pdf_doc_end_grabbing,
    /*pdf_doc_end_page, pdf_doc_set_mediabox,*/
};
use super::dpx_pdfdraw::{
    pdf_dev_arc, pdf_dev_arcn, pdf_dev_clip, pdf_dev_closepath, pdf_dev_concat,
    pdf_dev_currentmatrix, pdf_dev_currentpoint, pdf_dev_curveto, pdf_dev_dtransform,
    pdf_dev_eoclip, pdf_dev_flushpath, pdf_dev_grestore, pdf_dev_gsave, pdf_dev_idtransform,
    pdf_dev_lineto, pdf_dev_moveto, pdf_dev_newpath, pdf_dev_rcurveto, pdf_dev_rlineto,
    pdf_dev_rmoveto, pdf_dev_set_color, pdf_dev_setdash, pdf_dev_setlinecap, pdf_dev_setlinejoin,
    pdf_dev_setlinewidth, pdf_dev_setmiterlimit,
};
use super::dpx_pdfparse::dump_slice;
use super::dpx_subfont::{lookup_sfd_record, sfd_load_record};
use super::dpx_tfm::{tfm_exists, tfm_get_width, tfm_open, tfm_string_width};
use crate::dpx_pdfobj::{
    pdf_add_dict, pdf_array_length, pdf_copy_name, pdf_file, pdf_get_array, pdf_lookup_dict,
    pdf_name_value, pdf_new_dict, pdf_new_name, pdf_new_number, pdf_number_value, pdf_obj,
    pdf_release_obj, pdf_set_number, pdf_string_length, pdf_string_value,
};
use crate::dpx_pdfparse::{
    parse_number,
    pdfparse_skip_line, skip_white, SkipWhite, ParseIdent, ParsePdfObj,
};
use crate::shims::sprintf;
use libc::{atof, /*fread, */free, /*rewind, */ strtod};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type size_t = u64;
//use libc::FILE;

pub type spt_t = i32;

#[derive(Clone)]
#[repr(C)]
pub struct mp_font {
    pub font_name: CString,
    pub font_id: i32,
    pub tfm_id: i32,
    pub subfont_id: i32,
    pub pt_size: f64,
}
struct operators {
    pub token: &'static [u8],
    pub opcode: i32,
}
impl operators {
    const fn new(token: &'static [u8], opcode: i32) -> Self {
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
unsafe fn mp_setfont(mut font_name: &CStr, mut pt_size: f64) -> i32 {
    let mut subfont_id: i32 = -1i32;
    if let Some(font) = font_stack.last() {
        if (font.font_name.as_c_str() == font_name) && (font.pt_size == pt_size) {
            return 0;
        }
    }
    let mrec = pdf_lookup_fontmap_record(font_name.to_bytes());
    if !mrec.is_null()
        && !(*mrec).charmap.sfd_name.is_null()
        && !(*mrec).charmap.subfont_id.is_null()
    {
        subfont_id = sfd_load_record((*mrec).charmap.sfd_name, (*mrec).charmap.subfont_id)
    }
    /* See comments in dvi_locate_font() in dvi.c. */
    let name = if !mrec.is_null() && !(*mrec).map_name.is_null() {
        CStr::from_ptr((*mrec).map_name)
    } else {
        font_name
    };
    let font_id = pdf_dev_locate_font(name.as_ptr(), (pt_size * dev_unit_dviunit()) as spt_t);
    let new_font = mp_font {
        font_name: font_name.to_owned(),
        font_id,
        tfm_id: tfm_open(font_name.as_ptr(), 0),
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
        panic!(
            "MPOST: No physical font assigned for \"{}\".",
            font_name.display()
        );
    }
    0
}
unsafe fn save_font() {
    match font_stack.last() {
        Some(current) => font_stack.push(current.clone()),
        None => font_stack.push(
            mp_font {
                font_name: CString::new("Courier").unwrap(),
                font_id: -1,
                tfm_id: 0,
                subfont_id: 0,
                pt_size: 1.,
            }
        ),
    }
}
unsafe fn restore_font() {
    font_stack.pop().expect("No currentfont...");
}
unsafe fn clear_fonts() {
    font_stack = vec![];
}
unsafe fn is_fontname(token: &[u8]) -> bool {
    let mrec = pdf_lookup_fontmap_record(token);
    if !mrec.is_null() {
        return true;
    }
    tfm_exists(token)
}
#[no_mangle]
pub unsafe extern "C" fn mps_scan_bbox(
    mut pp: *mut *const i8,
    mut endptr: *const i8,
    bbox: &mut Rect,
) -> i32 {
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
                    bbox.ll = Coord::zero();
                    bbox.ur.x = values[2] - values[0];
                    bbox.ur.y = values[3] - values[1];
                    Xorigin = values[0];
                    Yorigin = values[1]
                } else {
                    bbox.ll.x = values[0];
                    bbox.ll.y = values[1];
                    bbox.ur.x = values[2];
                    bbox.ur.y = values[3];
                    Xorigin = 0.0f64;
                    Yorigin = 0.0f64
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
/*unsafe fn skip_prolog(mut start: *mut *const i8, mut end: *const i8) {
    let mut found_prolog: i32 = 0i32;
    let save = *start;
    while *start < end {
        if **start as i32 != '%' as i32 {
            skip_white(start, end);
        }
        if *start >= end {
            break;
        }
        if !strstartswith(*start, b"%%EndProlog\x00" as *const u8 as *const i8).is_null() {
            found_prolog = 1i32;
            pdfparse_skip_line(start, end);
            break;
        } else if !strstartswith(*start, b"%%Page:\x00" as *const u8 as *const i8).is_null() {
            pdfparse_skip_line(start, end);
            break;
        } else {
            pdfparse_skip_line(start, end);
        }
    }
    if found_prolog == 0 {
        *start = save
    };
}*/
static mut ps_operators: [operators; 48] = [
    operators::new(b"add", 1),
    operators::new(b"mul", 3),
    operators::new(b"div", 4),
    operators::new(b"neg", 5),
    operators::new(b"sub", 2),
    operators::new(b"truncate", 6),
    operators::new(b"clear", 10),
    operators::new(b"exch", 11),
    operators::new(b"pop", 12),
    operators::new(b"clip", 44),
    operators::new(b"eoclip", 45),
    operators::new(b"closepath", 32),
    operators::new(b"concat", 52),
    operators::new(b"newpath", 31),
    operators::new(b"moveto", 33),
    operators::new(b"rmoveto", 34),
    operators::new(b"lineto", 37),
    operators::new(b"rlineto", 38),
    operators::new(b"curveto", 35),
    operators::new(b"rcurveto", 36),
    operators::new(b"arc", 39),
    operators::new(b"arcn", 40),
    operators::new(b"stroke", 42),
    operators::new(b"fill", 41),
    operators::new(b"show", 43),
    operators::new(b"showpage", 49),
    operators::new(b"gsave", 50),
    operators::new(b"grestore", 51),
    operators::new(b"translate", 54),
    operators::new(b"rotate", 55),
    operators::new(b"scale", 53),
    operators::new(b"setlinecap", 62),
    operators::new(b"setlinejoin", 63),
    operators::new(b"setlinewidth", 60),
    operators::new(b"setmiterlimit", 64),
    operators::new(b"setdash", 61),
    operators::new(b"setgray", 70),
    operators::new(b"setrgbcolor", 71),
    operators::new(b"setcmykcolor", 72),
    operators::new(b"currentpoint", 80),
    operators::new(b"dtransform", 82),
    operators::new(b"idtransform", 81),
    operators::new(b"findfont", 201),
    operators::new(b"scalefont", 202),
    operators::new(b"setfont", 203),
    operators::new(b"currentfont", 204),
    operators::new(b"stringwidth", 210),
    operators::new(b"def", 999),
];
static mut mps_operators: [operators; 28] = [
    operators::new(b"fshow", 1001),
    operators::new(b"startTexFig", 1002),
    operators::new(b"endTexFig", 1003),
    operators::new(b"hlw", 1004),
    operators::new(b"vlw", 1005),
    operators::new(b"l", 37),
    operators::new(b"r", 38),
    operators::new(b"c", 35),
    operators::new(b"m", 33),
    operators::new(b"p", 32),
    operators::new(b"n", 31),
    operators::new(b"C", 72),
    operators::new(b"G", 70),
    operators::new(b"R", 71),
    operators::new(b"lj", 63),
    operators::new(b"ml", 64),
    operators::new(b"lc", 62),
    operators::new(b"S", 42),
    operators::new(b"F", 41),
    operators::new(b"q", 50),
    operators::new(b"Q", 51),
    operators::new(b"s", 53),
    operators::new(b"t", 52),
    operators::new(b"sd", 61),
    operators::new(b"rd", 1006),
    operators::new(b"P", 49),
    operators::new(b"B", 1007),
    operators::new(b"W", 44),
];
unsafe fn get_opcode(token: &[u8]) -> i32 {
    for op in ps_operators.iter() {
        if token == op.token {
            return op.opcode;
        }
    }
    for op in mps_operators.iter() {
        if token == op.token {
            return op.opcode;
        }
    }
    -1i32
}
static mut STACK: Vec<*mut pdf_obj> = Vec::new();
trait PushChecked {
    type Val;
    fn push_checked(&mut self, val: Self::Val) -> Result<(), ()>;
}
impl PushChecked for Vec<*mut pdf_obj> {
    type Val = *mut pdf_obj;
    fn push_checked(&mut self, val: Self::Val) -> Result<(), ()> {
        if self.len() < 1024 {
            self.push(val);
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
        return -1i32;
    }
    let tmp = STACK[len - 1];
    STACK[len - 1] = STACK[len - 2];
    STACK[len - 2] = tmp;
    0i32
}
unsafe fn do_clear() -> i32 {
    while !STACK.is_empty() {
        if let Some(tmp) = STACK.pop() {
            pdf_release_obj(tmp);
        }
    }
    0i32
}
unsafe fn pop_get_numbers(mut values: *mut f64, mut count: i32) -> i32 {
    loop {
        let fresh1 = count;
        count -= 1;
        if !(fresh1 > 0i32) {
            break;
        }
        if let Some(tmp) = STACK.pop() {
            if !(*tmp).is_number() {
                warn!("mpost: Not a number!");
                pdf_release_obj(tmp);
                break;
            } else {
                *values.offset(count as isize) = pdf_number_value(&*tmp);
                pdf_release_obj(tmp);
            }
        } else {
            warn!("mpost: Stack underflow.");
            break;
        }
    }
    count + 1i32
}
unsafe fn cvr_array(mut array: *mut pdf_obj, mut values: *mut f64, mut count: i32) -> i32 {
    if !(!array.is_null() && (*array).is_array()) {
        warn!("mpost: Not an array!");
    } else {
        loop {
            let fresh2 = count;
            count = count - 1;
            if !(fresh2 > 0i32) {
                break;
            }
            let tmp = pdf_get_array(&mut *array, count);
            if !(!tmp.is_null() && (*tmp).is_number()) {
                warn!("mpost: Not a number!");
                break;
            } else {
                *values.offset(count as isize) = pdf_number_value(&*tmp)
            }
        }
    }
    pdf_release_obj(array);
    count + 1i32
}
unsafe fn is_fontdict(mut dict: *mut pdf_obj) -> bool {
    if !(!dict.is_null() && (*dict).is_dict()) {
        return false;
    }
    let tmp = pdf_lookup_dict(&mut *dict, "Type").filter(|&tmp| {
        (*tmp).is_name()
            && pdf_name_value(&*tmp).to_string_lossy() == "Font"
    });
    if tmp.is_none() {
        return false;
    }
    let tmp =
        pdf_lookup_dict(&mut *dict, "FontName").filter(|&tmp| (*tmp).is_name());
    if tmp.is_none() {
        return false;
    }
    let tmp =
        pdf_lookup_dict(&mut *dict, "FontScale").filter(|&tmp| (*tmp).is_number());
    tmp.is_some()
}
unsafe fn do_findfont() -> i32 {
    let mut error: i32 = 0i32;
    if let Some(font_name) = STACK.pop() {
        if (*font_name).is_string() || (*font_name).is_name() {
            /* Do not check the existence...
             * The reason for this is that we cannot locate PK font without
             * font scale.
             */
            let font_dict = pdf_new_dict();
            pdf_add_dict(&mut *font_dict, "Type", pdf_new_name("Font"));
            if (*font_name).is_string() {
                pdf_add_dict(
                    &mut *font_dict,
                    "FontName",
                    pdf_copy_name(pdf_string_value(&*font_name) as *const i8),
                );
                pdf_release_obj(font_name);
            } else {
                pdf_add_dict(&mut *font_dict, "FontName", font_name);
            }
            pdf_add_dict(&mut *font_dict, "FontScale", pdf_new_number(1.0f64));
            if STACK.push_checked(font_dict).is_err() {
                pdf_release_obj(font_dict);
                error = 1i32
            }
        } else {
            error = 1i32
        }
    } else {
        return 1i32;
    }
    error
}
unsafe fn do_scalefont() -> i32 {
    let mut scale: f64 = 0.;
    let mut error = pop_get_numbers(&mut scale, 1i32);
    if error != 0 {
        return error;
    }
    if let Some(font_dict) = STACK.pop() {
        if is_fontdict(font_dict) {
            let font_scale = pdf_lookup_dict(&mut *font_dict, "FontScale").unwrap();
            pdf_set_number(&mut *font_scale, pdf_number_value(&*font_scale) * scale);
            if STACK.push_checked(font_dict).is_err() {
                pdf_release_obj(font_dict);
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
        let error = if !is_fontdict(font_dict) {
            1
        } else {
            /* Subfont support prevent us from managing
             * font in a single place...
             */
            let font_name = pdf_name_value(&*pdf_lookup_dict(&mut *font_dict, "FontName").unwrap());
            let font_scale = pdf_number_value(&*pdf_lookup_dict(&mut *font_dict, "FontScale").unwrap());
            mp_setfont(font_name, font_scale)
        };
        pdf_release_obj(font_dict);
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
        0 as *mut mp_font
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if font.is_null() {
        warn!("Currentfont undefined...");
        return 1i32;
    } else {
        let font_dict = pdf_new_dict();
        pdf_add_dict(&mut *font_dict, "Type", pdf_new_name("Font"));
        pdf_add_dict(&mut *font_dict, "FontName", pdf_new_name((*font).font_name.to_bytes()));
        pdf_add_dict(&mut *font_dict, "FontScale", pdf_new_number((*font).pt_size));
        if STACK.len() < 1024 {
            STACK.push(font_dict)
        } else {
            warn!("PS stack overflow...");
            pdf_release_obj(font_dict);
            error = 1i32
        }
    }
    error
}
unsafe fn do_show() -> i32 {
    let mut cp = Coord::zero();
    let font = if currentfont < 0i32 {
        0 as *mut mp_font
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if font.is_null() {
        warn!("Currentfont not set.");
        return 1i32;
    }
    pdf_dev_currentpoint(&mut cp);
    let text_str = STACK.pop();
    if text_str.is_none() {
        return 1i32;
    }
    let text_str = text_str.unwrap();
    if !(*text_str).is_string() {
        pdf_release_obj(text_str);
        return 1i32;
    }
    if (*font).font_id < 0i32 {
        warn!("mpost: not set.");
        pdf_release_obj(text_str);
        return 1i32;
    }
    let strptr = pdf_string_value(&*text_str) as *mut u8;
    let length = pdf_string_length(&*text_str) as i32;
    if (*font).tfm_id < 0i32 {
        warn!(
            "mpost: TFM not found for \"{}\".",
            (*font).font_name.display()
        );
        warn!("mpost: Text width not calculated...");
    }
    let mut text_width = 0_f64;
    if (*font).subfont_id >= 0i32 {
        let ustr = new(
            ((length * 2i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
        ) as *mut u8;
        for i in 0..length {
            let uch = lookup_sfd_record((*font).subfont_id, *strptr.offset(i as isize));
            *ustr.offset((2i32 * i) as isize) = (uch as i32 >> 8i32) as u8;
            *ustr.offset((2i32 * i + 1i32) as isize) = (uch as i32 & 0xffi32) as u8;
            if (*font).tfm_id >= 0i32 {
                text_width += tfm_get_width((*font).tfm_id, *strptr.offset(i as isize) as i32)
            }
        }
        text_width *= (*font).pt_size;
        pdf_dev_set_string(
            (cp.x * dev_unit_dviunit()) as spt_t,
            (cp.y * dev_unit_dviunit()) as spt_t,
            ustr as *const libc::c_void,
            (length * 2i32) as size_t,
            (text_width * dev_unit_dviunit()) as spt_t,
            (*font).font_id,
            0i32,
        );
        free(ustr as *mut libc::c_void);
    } else {
        if (*font).tfm_id >= 0i32 {
            text_width = tfm_string_width((*font).tfm_id, strptr, length as u32) as f64
                / (1i32 << 20i32) as f64;
            text_width *= (*font).pt_size
        }
        pdf_dev_set_string(
            (cp.x * dev_unit_dviunit()) as spt_t,
            (cp.y * dev_unit_dviunit()) as spt_t,
            strptr as *const libc::c_void,
            length as size_t,
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
    pdf_release_obj(text_str);
    0i32
}
unsafe fn do_mpost_bind_def(mut ps_code: *const i8, mut x_user: f64, mut y_user: f64) -> i32 {
    let mut start = CStr::from_ptr(ps_code).to_bytes();
    mp_parse_body(&mut start, x_user, y_user)
}
unsafe fn do_texfig_operator(mut opcode: i32, mut x_user: f64, mut y_user: f64) -> i32 {
    static mut fig_p: transform_info = transform_info::new();
    static mut in_tfig: i32 = 0i32;
    static mut xobj_id: i32 = -1i32;
    static mut count: i32 = 0i32;
    let mut values: [f64; 6] = [0.; 6];
    let mut error: i32 = 0i32;
    match opcode {
        1002 => {
            error = pop_get_numbers(values.as_mut_ptr(), 6i32);
            if error == 0 {
                let mut resname: [i8; 256] = [0; 256];
                transform_info_clear(&mut fig_p);
                let dvi2pts = 1.0f64 / dev_unit_dviunit();
                fig_p.width = values[0] * dvi2pts;
                fig_p.height = values[1] * dvi2pts;
                fig_p.bbox.ll.x = values[2] * dvi2pts;
                fig_p.bbox.ll.y = -values[3] * dvi2pts;
                fig_p.bbox.ur.x = values[4] * dvi2pts;
                fig_p.bbox.ur.y = -values[5] * dvi2pts;
                fig_p.flags |= 1i32 << 0i32;
                sprintf(
                    resname.as_mut_ptr(),
                    b"__tf%d__\x00" as *const u8 as *const i8,
                    count,
                );
                xobj_id = pdf_doc_begin_grabbing(
                    resname.as_mut_ptr(),
                    fig_p.bbox.ll.x,
                    fig_p.bbox.ur.y,
                    &mut fig_p.bbox,
                );
                in_tfig = 1i32;
                count += 1
            }
        }
        1003 => {
            if in_tfig == 0 {
                panic!("endTexFig without valid startTexFig!.");
            }
            pdf_doc_end_grabbing(0 as *mut pdf_obj);
            pdf_dev_put_image(xobj_id, &mut fig_p, x_user, y_user);
            in_tfig = 0i32
        }
        _ => error = 1i32,
    }
    error
}
unsafe fn ps_dev_CTM(M: &mut TMatrix) -> i32 {
    pdf_dev_currentmatrix(M);
    M.a *= 1000.;
    M.b *= 1000.;
    M.c *= 1000.;
    M.d *= 1000.;
    M.e *= 1000.;
    M.f *= 1000.;
    0i32
}
/*
 * Again, the only piece that needs x_user and y_user is
 * that piece dealing with texfig.
 */
unsafe fn do_operator(token: &[u8], mut x_user: f64, mut y_user: f64) -> i32 {
    let mut error: i32 = 0i32;
    let mut values: [f64; 12] = [0.; 12];
    let mut tmp = None;
    let mut matrix = TMatrix::new();
    let mut cp = Coord::zero();
    let opcode = get_opcode(token);
    match opcode {
        1 => {
            /*
             * Arithmetic operators
             */
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(values[0] + values[1])).is_err() {
                    error = 1i32
                }
            }
        }
        3 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(values[0] * values[1])).is_err() {
                    error = 1i32
                }
            }
        }
        5 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(-values[0])).is_err() {
                    error = 1i32
                }
            }
        }
        2 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(values[0] - values[1])).is_err() {
                    error = 1i32
                }
            }
        }
        4 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(values[0] / values[1])).is_err() {
                    error = 1i32
                }
            }
        }
        6 => {
            /* Round toward zero. */
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(
                    if values[0] > 0. {
                        values[0].floor()
                    } else {
                        values[0].ceil()
                    }
                )).is_err() {
                    error = 1;
                }
            }
        }
        10 => {
            /* STACK operation */
            error = do_clear()
        }
        12 => {
            if let Some(tmp) = STACK.pop() {
                pdf_release_obj(tmp);
            }
        }
        11 => error = do_exch(),
        33 => {
            /* Path construction */
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                error = pdf_dev_moveto(values[0], values[1])
            }
        }
        34 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                error = pdf_dev_rmoveto(values[0], values[1])
            }
        }
        37 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                error = pdf_dev_lineto(values[0], values[1])
            }
        }
        38 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                error = pdf_dev_rlineto(values[0], values[1])
            }
        }
        35 => {
            error = pop_get_numbers(values.as_mut_ptr(), 6i32);
            if error == 0 {
                error = pdf_dev_curveto(
                    values[0], values[1], values[2], values[3], values[4], values[5],
                )
            }
        }
        36 => {
            error = pop_get_numbers(values.as_mut_ptr(), 6i32);
            if error == 0 {
                error = pdf_dev_rcurveto(
                    values[0], values[1], values[2], values[3], values[4], values[5],
                )
            }
        }
        32 => error = pdf_dev_closepath(),
        39 => {
            error = pop_get_numbers(values.as_mut_ptr(), 5i32);
            if error == 0 {
                error = pdf_dev_arc(values[0], values[1], values[2], values[3], values[4])
            }
        }
        40 => {
            error = pop_get_numbers(values.as_mut_ptr(), 5i32);
            if error == 0 {
                error = pdf_dev_arcn(values[0], values[1], values[2], values[3], values[4])
            }
        }
        31 => {
            pdf_dev_newpath();
        }
        42 => {
            /* fill rule not supported yet */
            pdf_dev_flushpath(b'S', 0);
        }
        41 => {
            pdf_dev_flushpath(b'f', 0);
        }
        44 => error = pdf_dev_clip(),
        45 => error = pdf_dev_eoclip(),
        50 => {
            /* Graphics state operators: */
            error = pdf_dev_gsave(); /* This does pdf_release_obj() */
            save_font();
        }
        51 => {
            error = pdf_dev_grestore();
            restore_font();
        }
        52 => {
            tmp = STACK.pop();
            error = cvr_array(tmp.unwrap(), values.as_mut_ptr(), 6i32); // TODO: check
            tmp = None;
            if error != 0 {
                warn!("Missing array before \"concat\".");
            } else {
                matrix.a = values[0];
                matrix.b = values[1];
                matrix.c = values[2];
                matrix.d = values[3];
                matrix.e = values[4];
                matrix.f = values[5];
                error = pdf_dev_concat(&mut matrix)
            }
        }
        53 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                match mp_cmode {
                    _ => {}
                }
                matrix.a = values[0];
                matrix.b = 0.0f64;
                matrix.c = 0.0f64;
                matrix.d = values[1];
                matrix.e = 0.0f64;
                matrix.f = 0.0f64;
                error = pdf_dev_concat(&mut matrix)
            }
        }
        55 => {
            /* Positive angle means clock-wise direction in graphicx-dvips??? */
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                values[0] = values[0] * 3.14159265358979323846f64 / 180i32 as f64;
                match mp_cmode {
                    1 | 0 => {
                        /* Really? */
                        let (s, c) = values[0].sin_cos();
                        matrix.a = c;
                        matrix.b = -s;
                        matrix.c = s;
                        matrix.d = c;
                        matrix.e = 0.;
                        matrix.f = 0.
                    }
                    _ => {
                        let (s, c) = values[0].sin_cos();
                        matrix.a = c;
                        matrix.b = s;
                        matrix.c = -s;
                        matrix.d = c;
                        matrix.e = 0.;
                        matrix.f = 0.
                    }
                }
                error = pdf_dev_concat(&mut matrix)
            }
        }
        54 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                matrix.a = 1.;
                matrix.b = 0.;
                matrix.c = 0.;
                matrix.d = 1.;
                matrix.e = values[0];
                matrix.f = values[1];
                error = pdf_dev_concat(&mut matrix)
            }
        }
        61 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                let mut num_dashes = 0_usize;
                let mut dash_values: [f64; 16] = [0.; 16];
                let offset = values[0];
                if let Some(pattern) = STACK.pop() {
                    if !(*pattern).is_array() {
                        pdf_release_obj(pattern);
                        error = 1
                    } else {
                        num_dashes = pdf_array_length(&*pattern) as usize;
                        if num_dashes > 16 {
                            warn!("Too many dashes...");
                            pdf_release_obj(pattern);
                            error = 1i32
                        } else {
                            let mut i = 0;
                            while i < num_dashes && error == 0 {
                                let dash = pdf_get_array(&mut *pattern, i as i32);
                                if !(!dash.is_null() && (*dash).is_number()) {
                                    error = 1i32
                                } else {
                                    dash_values[i as usize] = pdf_number_value(&*dash)
                                }
                                i += 1
                            }
                            pdf_release_obj(pattern);
                            if error == 0 {
                                error = pdf_dev_setdash(&dash_values[..num_dashes], offset)
                            }
                        }
                    }
                } else {
                    error = 1;
                }
            }
        }
        62 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                error = pdf_dev_setlinecap(values[0] as i32)
            }
        }
        63 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                error = pdf_dev_setlinejoin(values[0] as i32)
            }
        }
        60 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                error = pdf_dev_setlinewidth(values[0])
            }
        }
        64 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                error = pdf_dev_setmiterlimit(values[0])
            }
        }
        72 => {
            error = pop_get_numbers(values.as_mut_ptr(), 4i32);
            /* Not handled properly */
            if error == 0 {
                let color =
                    PdfColor::from_cmyk(values[0], values[1], values[2], values[3]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        70 => {
            /* Not handled properly */
            error = pop_get_numbers(values.as_mut_ptr(), 1i32); /* This does pdf_release_obj() */
            if error == 0 {
                let color = PdfColor::from_gray(values[0]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        71 => {
            error = pop_get_numbers(values.as_mut_ptr(), 3i32);
            if error == 0 {
                let color = PdfColor::from_rgb(values[0], values[1], values[2]).unwrap();
                pdf_dev_set_color(&color, 0, 0);
                pdf_dev_set_color(&color, 0x20, 0);
            }
        }
        49 => {}
        80 => {
            error = pdf_dev_currentpoint(&mut cp);
            if error == 0 {
                if STACK.push_checked(pdf_new_number(cp.x)).is_ok() {
                    if STACK.push_checked(pdf_new_number(cp.y)).is_err() {
                        error = 1i32
                    }
                } else {
                    error = 1i32
                }
            }
        }
        82 => {
            let mut has_matrix: i32 = 0i32;
            if let Some(tmp2) = STACK.pop() {
                if (*tmp2).is_array() {
                    error = cvr_array(tmp2, values.as_mut_ptr(), 6i32);
                    tmp = None;
                    if error == 0 {
                        matrix.a = values[0];
                        matrix.b = values[1];
                        matrix.c = values[2];
                        matrix.d = values[3];
                        matrix.e = values[4];
                        matrix.f = values[5];
                        tmp = STACK.pop();
                        has_matrix = 1i32;
                    }
                } else {
                    tmp = Some(tmp2);
                }
            }
            if error == 0 {
                if let Some(tmp) = tmp.filter(|&o| (*o).is_number()) {
                    cp.y = pdf_number_value(&*tmp);
                    pdf_release_obj(tmp);
                    if let Some(tmp) = STACK.pop().filter(|&o| (*o).is_number()) {
                        cp.x = pdf_number_value(&*tmp);
                        pdf_release_obj(tmp);
                        if has_matrix == 0 {
                            ps_dev_CTM(&mut matrix);
                            /* Here, we need real PostScript CTM */
                        } /* This does pdf_release_obj() */
                        pdf_dev_dtransform(&mut cp, Some(&mut matrix));
                        if STACK.push_checked(pdf_new_number(cp.x)).is_ok() {
                            if STACK.push_checked(pdf_new_number(cp.y)).is_err() {
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
        81 => {
            let mut has_matrix_0: i32 = 0i32;
            if let Some(tmp2) = STACK.pop() {
                if (*tmp2).is_array() {
                    error = cvr_array(tmp2, values.as_mut_ptr(), 6i32);
                    tmp = None;
                    if error == 0 {
                        matrix.a = values[0];
                        matrix.b = values[1];
                        matrix.c = values[2];
                        matrix.d = values[3];
                        matrix.e = values[4];
                        matrix.f = values[5];
                        tmp = STACK.pop();
                        has_matrix_0 = 1i32;
                    }
                } else {
                    tmp = Some(tmp2);
                }
            } 
            if error == 0 {
                if let Some(tmp) = tmp.filter(|&o| (*o).is_number()) {
                    cp.y = pdf_number_value(&*tmp);
                    pdf_release_obj(tmp);
                    if let Some(tmp) = STACK.pop().filter(|&o| (*o).is_number()) {
                        cp.x = pdf_number_value(&*tmp);
                        pdf_release_obj(tmp);
                        if has_matrix_0 == 0 {
                            ps_dev_CTM(&mut matrix);
                            /* Here, we need real PostScript CTM */
                        }
                        pdf_dev_idtransform(&mut cp, Some(&matrix));
                        if STACK.push_checked(pdf_new_number(cp.x)).is_ok() {
                            if STACK.push_checked(pdf_new_number(cp.y)).is_err() {
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
        201 => error = do_findfont(),
        202 => error = do_scalefont(),
        203 => error = do_setfont(),
        204 => error = do_currentfont(),
        43 => error = do_show(),
        210 => error = 1i32,
        1001 => {
            /* Extensions */
            error = do_mpost_bind_def(
                b"exch findfont exch scalefont setfont show\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        1002 | 1003 => error = do_texfig_operator(opcode, x_user, y_user),
        1004 => {
            error = do_mpost_bind_def(
                b"0 dtransform exch truncate exch idtransform pop setlinewidth\x00" as *const u8
                    as *const i8,
                x_user,
                y_user,
            )
        }
        1005 => {
            error = do_mpost_bind_def(
                b"0 exch dtransform truncate idtransform setlinewidth pop\x00" as *const u8
                    as *const i8,
                x_user,
                y_user,
            )
        }
        1006 => {
            error = do_mpost_bind_def(
                b"[] 0 setdash\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        1007 => {
            error = do_mpost_bind_def(
                b"gsave fill grestore\x00" as *const u8 as *const i8,
                x_user,
                y_user,
            )
        }
        999 => {
            STACK.pop();
            STACK.pop();
        }
        _ => {
            if is_fontname(token) {
                if STACK.push_checked(pdf_new_name(token)).is_err() {
                    error = 1i32
                }
            } else {
                warn!("Unknown token \"{}\"", token.display());
                error = 1i32
            }
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
unsafe fn mp_parse_body(
    mut start: &mut &[u8],
    mut x_user: f64,
    mut y_user: f64,
) -> i32 {
    let mut obj = 0 as *mut pdf_obj;
    let mut error: i32 = 0i32;
    start.skip_white();
    while !start.is_empty() && error == 0 {
        if start[0].is_ascii_digit()
            || start.len() > 1
                && (start[0] == b'+'
                    || start[0] == b'-'
                    || start[0] == b'.')
        {
            let mut next: *mut i8 = 0 as *mut i8;
            let value = strtod(start.as_ptr() as *const i8, &mut next);
            let pos = next.wrapping_offset_from(start.as_ptr() as *const i8) as usize;
            if pos < start.len()
                && !b"<([{/%".contains(&(*next as u8))
                && libc::isspace(start[pos] as _) == 0
            {
                warn!("Unkown PostScript operator.");
                dump_slice(&start[..pos]);
                error = 1i32
            } else if STACK.push_checked(pdf_new_number(value)).is_ok() {
                *start = &start[pos..];
            } else {
                error = 1i32;
                break;
            }
        /*
         * PDF parser can't handle PS operator inside arrays.
         * This shouldn't use parse_pdf_array().
         */
        } else if start[0] == b'[' &&
            start.parse_pdf_array(0 as *mut pdf_file).map(|o| {obj = o; ()}).is_some()
        {
            if STACK.push_checked(obj).is_err() {
                error = 1i32;
                break;
            }
        /* This cannot handle ASCII85 string. */
        } else if start.len() > 1
            && (start[0] == b'<' && start[1] == b'<')
            && start.parse_pdf_dict(0 as *mut pdf_file).map(|o| {obj = o; ()}).is_some()
        {
            if STACK.push_checked(obj).is_err() {
                error = 1i32;
                break;
            }
        } else if (start[0] == b'(' || start[0] == b'<') && 
            start.parse_pdf_string().map(|o| {obj = o; ()}).is_some() {
            if STACK.push_checked(obj).is_err() {
                error = 1i32;
                break;
            }
        } else if start[0] == b'/' && 
            start.parse_pdf_name().map(|o| {obj = o; ()}).is_some() {
            if STACK.push_checked(obj).is_err() {
                error = 1i32;
                break;
            }
        } else {
            if let Some(token) = start.parse_ident() {
                error = do_operator(token.to_bytes(), x_user, y_user);
            } else {
                error = 1i32
            }
        }
        start.skip_white();
    }
    error
}
#[no_mangle]
pub unsafe extern "C" fn mps_eop_cleanup() {
    clear_fonts();
    do_clear();
}
#[no_mangle]
pub unsafe extern "C" fn mps_stack_depth() -> i32 {
    STACK.len() as i32
}
#[no_mangle]
pub unsafe extern "C" fn mps_exec_inline(
    pp: &mut &[u8],
    mut x_user: f64,
    mut y_user: f64,
) -> i32 {
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
/*#[no_mangle]
pub unsafe extern "C" fn mps_do_page(mut image_file: *mut FILE) -> i32 {
    /* scale, xorig, yorig */
    let mut bbox = Rect::zero();
    rewind(image_file);
    let size = file_size(image_file);
    if size == 0i32 {
        warn!("Can\'t read any byte in the MPS file.");
        return -1i32;
    }
    let mut buffer =
        new(((size + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    fread(
        buffer as *mut libc::c_void,
        ::std::mem::size_of::<i8>(),
        size as _,
        image_file,
    );
    *buffer.offset(size as isize) = 0_i8;
    let mut start = buffer as *const i8;
    let end = buffer.offset(size as isize);
    let mut error = mps_scan_bbox(&mut start, end, &mut bbox);
    if error != 0 {
        warn!("Error occured while scanning MetaPost file headers: Could not find BoundingBox.");
        free(buffer as *mut libc::c_void);
        return -1i32;
    }
    mp_cmode = 0i32;
    pdf_doc_begin_page(1.0f64, -Xorigin, -Yorigin);
    pdf_doc_set_mediabox(pdf_doc_current_page_number() as u32, &bbox);
    let dir_mode = pdf_dev_get_dirmode();
    pdf_dev_set_param(1i32, 0i32);
    skip_prolog(&mut start, end);
    let mut error = mp_parse_body(&mut start, end, 0.0f64, 0.0f64);
    if error != 0 {
        warn!("Errors occured while interpreting MetaPost file.");
    }
    pdf_dev_set_param(1i32, 1i32);
    pdf_dev_set_dirmode(dir_mode);
    pdf_doc_end_page();
    free(buffer as *mut libc::c_void);
    /*
     * The reason why we don't return XObject itself is
     * PDF inclusion may not be made so.
     */
    if error != 0 {
        -1i32
    } else {
        0i32
    }
}*/
