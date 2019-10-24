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
use std::ffi::CStr;

use crate::mfree;
use crate::warn;
use crate::{streq_ptr, strstartswith};

use super::dpx_dvipdfmx::translate_origin;
use super::dpx_fontmap::pdf_lookup_fontmap_record;
use super::dpx_mem::new;
use super::dpx_mfileio::file_size;
use super::dpx_pdfcolor::PdfColor;
use super::dpx_pdfdev::{
    dev_unit_dviunit, graphics_mode, pdf_coord, pdf_dev_get_dirmode, pdf_dev_get_font_wmode,
    pdf_dev_get_param, pdf_dev_locate_font, pdf_dev_put_image, pdf_dev_set_dirmode,
    pdf_dev_set_param, pdf_dev_set_string, pdf_rect, pdf_tmatrix, transform_info,
    transform_info_clear,
};
use super::dpx_pdfdoc::{
    pdf_doc_begin_grabbing, pdf_doc_begin_page, pdf_doc_current_page_number, pdf_doc_end_grabbing,
    pdf_doc_end_page, pdf_doc_set_mediabox,
};
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
    pdf_add_dict, pdf_array_length, pdf_copy_name, pdf_file, pdf_get_array, pdf_lookup_dict,
    pdf_name_value, pdf_new_dict, pdf_new_name, pdf_new_number, pdf_number_value, pdf_obj,
    pdf_release_obj, pdf_set_number, pdf_string_length, pdf_string_value,
};
use crate::dpx_pdfparse::{
    parse_ident, parse_number, parse_pdf_array, parse_pdf_dict, parse_pdf_name, parse_pdf_string,
    pdfparse_skip_line, skip_white,
};
use crate::shims::sprintf;
use libc::{atof, fread, free, rewind, strchr, strcpy, strlen, strtod};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type size_t = u64;
use libc::FILE;

pub type spt_t = i32;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct mp_font {
    pub font_name: *mut i8,
    pub font_id: i32,
    pub tfm_id: i32,
    pub subfont_id: i32,
    pub pt_size: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct operators {
    pub token: *const i8,
    pub opcode: i32,
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
static mut font_stack: [mp_font; 256] = {
    let mut init: [mp_font; 256] = [
        mp_font {
            font_name: std::ptr::null_mut(),
            font_id: 0,
            tfm_id: 0,
            subfont_id: 0,
            pt_size: 0.0,
        }; 
        256
    ];
    init[0] = mp_font {
        font_name: std::ptr::null_mut(),
        font_id: -1,
        tfm_id: -1,
        subfont_id: -1,
        pt_size: 0.0,
    }; /* No currentfont */
    init
};
static mut currentfont: i32 = -1i32;
static mut mp_cmode: i32 = 0i32;
unsafe fn mp_setfont(mut font_name: *const i8, mut pt_size: f64) -> i32 {
    let mut subfont_id: i32 = -1i32;
    let mut font = if currentfont < 0i32 {
        0 as *mut mp_font
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if !font.is_null() {
        if streq_ptr((*font).font_name, font_name) as i32 != 0 && (*font).pt_size == pt_size {
            return 0i32;
        }
    } else {
        /* ***TODO*** Here some problem exists! */
        font = &mut *font_stack.as_mut_ptr().offset(0) as *mut mp_font;
        (*font).font_name = 0 as *mut i8;
        currentfont = 0i32
    }
    let mrec = pdf_lookup_fontmap_record(font_name);
    if !mrec.is_null()
        && !(*mrec).charmap.sfd_name.is_null()
        && !(*mrec).charmap.subfont_id.is_null()
    {
        subfont_id = sfd_load_record((*mrec).charmap.sfd_name, (*mrec).charmap.subfont_id)
    }
    /* See comments in dvi_locate_font() in dvi.c. */
    let name = if !mrec.is_null() && !(*mrec).map_name.is_null() {
        (*mrec).map_name
    } else {
        font_name
    }; /* Need not exist in MP mode */
    free((*font).font_name as *mut libc::c_void);
    (*font).font_name =
        new((strlen(font_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*font).font_name, font_name);
    (*font).subfont_id = subfont_id;
    (*font).pt_size = pt_size;
    (*font).tfm_id = tfm_open(font_name, 0i32);
    (*font).font_id = pdf_dev_locate_font(name, (pt_size * dev_unit_dviunit()) as spt_t);
    if (*font).font_id < 0i32 {
        panic!(
            "MPOST: No physical font assigned for \"{}\".",
            CStr::from_ptr(font_name).display()
        );
    }
    0i32
}
unsafe fn save_font() {
    if currentfont < 0i32 {
        font_stack[0].font_name = new((strlen(b"Courier\x00" as *const u8 as *const i8)
            .wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy(
            font_stack[0].font_name,
            b"Courier\x00" as *const u8 as *const i8,
        );
        font_stack[0].pt_size = 1i32 as f64;
        font_stack[0].tfm_id = 0i32;
        font_stack[0].subfont_id = 0i32;
        currentfont = 0i32
    }
    let fresh0 = currentfont;
    currentfont = currentfont + 1;
    let current = &mut *font_stack.as_mut_ptr().offset(fresh0 as isize) as *mut mp_font;
    let next = &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font;
    (*next).font_name = new((strlen((*current).font_name).wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    strcpy((*next).font_name, (*current).font_name);
    (*next).pt_size = (*current).pt_size;
    (*next).subfont_id = (*current).subfont_id;
    (*next).tfm_id = (*current).tfm_id;
}
unsafe fn restore_font() {
    let current = if currentfont < 0i32 {
        0 as *mut mp_font
    } else {
        &mut *font_stack.as_mut_ptr().offset(currentfont as isize) as *mut mp_font
    };
    if !current.is_null() {
        (*current).font_name = mfree((*current).font_name as *mut libc::c_void) as *mut i8
    } else {
        panic!("No currentfont...");
    }
    currentfont -= 1;
}
unsafe fn clear_fonts() {
    while currentfont >= 0i32 {
        free(font_stack[currentfont as usize].font_name as *mut libc::c_void);
        currentfont -= 1
    }
}
unsafe fn is_fontname(mut token: *const i8) -> bool {
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
    bbox: &mut pdf_rect,
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
                    bbox.llx = 0i32 as f64;
                    bbox.lly = 0i32 as f64;
                    bbox.urx = values[2] - values[0];
                    bbox.ury = values[3] - values[1];
                    Xorigin = values[0];
                    Yorigin = values[1]
                } else {
                    bbox.llx = values[0];
                    bbox.lly = values[1];
                    bbox.urx = values[2];
                    bbox.ury = values[3];
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
unsafe fn skip_prolog(mut start: *mut *const i8, mut end: *const i8) {
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
}
static mut ps_operators: [operators; 48] = [
    operators {
        token: b"add\x00" as *const u8 as *const i8,
        opcode: 1,
    },
    operators {
        token: b"mul\x00" as *const u8 as *const i8,
        opcode: 3,
    },
    operators {
        token: b"div\x00" as *const u8 as *const i8,
        opcode: 4,
    },
    operators {
        token: b"neg\x00" as *const u8 as *const i8,
        opcode: 5,
    },
    operators {
        token: b"sub\x00" as *const u8 as *const i8,
        opcode: 2,
    },
    operators {
        token: b"truncate\x00" as *const u8 as *const i8,
        opcode: 6,
    },
    operators {
        token: b"clear\x00" as *const u8 as *const i8,
        opcode: 10,
    },
    operators {
        token: b"exch\x00" as *const u8 as *const i8,
        opcode: 11,
    },
    operators {
        token: b"pop\x00" as *const u8 as *const i8,
        opcode: 12,
    },
    operators {
        token: b"clip\x00" as *const u8 as *const i8,
        opcode: 44,
    },
    operators {
        token: b"eoclip\x00" as *const u8 as *const i8,
        opcode: 45,
    },
    operators {
        token: b"closepath\x00" as *const u8 as *const i8,
        opcode: 32,
    },
    operators {
        token: b"concat\x00" as *const u8 as *const i8,
        opcode: 52,
    },
    operators {
        token: b"newpath\x00" as *const u8 as *const i8,
        opcode: 31,
    },
    operators {
        token: b"moveto\x00" as *const u8 as *const i8,
        opcode: 33,
    },
    operators {
        token: b"rmoveto\x00" as *const u8 as *const i8,
        opcode: 34,
    },
    operators {
        token: b"lineto\x00" as *const u8 as *const i8,
        opcode: 37,
    },
    operators {
        token: b"rlineto\x00" as *const u8 as *const i8,
        opcode: 38,
    },
    operators {
        token: b"curveto\x00" as *const u8 as *const i8,
        opcode: 35,
    },
    operators {
        token: b"rcurveto\x00" as *const u8 as *const i8,
        opcode: 36,
    },
    operators {
        token: b"arc\x00" as *const u8 as *const i8,
        opcode: 39,
    },
    operators {
        token: b"arcn\x00" as *const u8 as *const i8,
        opcode: 40,
    },
    operators {
        token: b"stroke\x00" as *const u8 as *const i8,
        opcode: 42,
    },
    operators {
        token: b"fill\x00" as *const u8 as *const i8,
        opcode: 41,
    },
    operators {
        token: b"show\x00" as *const u8 as *const i8,
        opcode: 43,
    },
    operators {
        token: b"showpage\x00" as *const u8 as *const i8,
        opcode: 49,
    },
    operators {
        token: b"gsave\x00" as *const u8 as *const i8,
        opcode: 50,
    },
    operators {
        token: b"grestore\x00" as *const u8 as *const i8,
        opcode: 51,
    },
    operators {
        token: b"translate\x00" as *const u8 as *const i8,
        opcode: 54,
    },
    operators {
        token: b"rotate\x00" as *const u8 as *const i8,
        opcode: 55,
    },
    operators {
        token: b"scale\x00" as *const u8 as *const i8,
        opcode: 53,
    },
    operators {
        token: b"setlinecap\x00" as *const u8 as *const i8,
        opcode: 62,
    },
    operators {
        token: b"setlinejoin\x00" as *const u8 as *const i8,
        opcode: 63,
    },
    operators {
        token: b"setlinewidth\x00" as *const u8 as *const i8,
        opcode: 60,
    },
    operators {
        token: b"setmiterlimit\x00" as *const u8 as *const i8,
        opcode: 64,
    },
    operators {
        token: b"setdash\x00" as *const u8 as *const i8,
        opcode: 61,
    },
    operators {
        token: b"setgray\x00" as *const u8 as *const i8,
        opcode: 70,
    },
    operators {
        token: b"setrgbcolor\x00" as *const u8 as *const i8,
        opcode: 71,
    },
    operators {
        token: b"setcmykcolor\x00" as *const u8 as *const i8,
        opcode: 72,
    },
    operators {
        token: b"currentpoint\x00" as *const u8 as *const i8,
        opcode: 80,
    },
    operators {
        token: b"dtransform\x00" as *const u8 as *const i8,
        opcode: 82,
    },
    operators {
        token: b"idtransform\x00" as *const u8 as *const i8,
        opcode: 81,
    },
    operators {
        token: b"findfont\x00" as *const u8 as *const i8,
        opcode: 201,
    },
    operators {
        token: b"scalefont\x00" as *const u8 as *const i8,
        opcode: 202,
    },
    operators {
        token: b"setfont\x00" as *const u8 as *const i8,
        opcode: 203,
    },
    operators {
        token: b"currentfont\x00" as *const u8 as *const i8,
        opcode: 204,
    },
    operators {
        token: b"stringwidth\x00" as *const u8 as *const i8,
        opcode: 210,
    },
    operators {
        token: b"def\x00" as *const u8 as *const i8,
        opcode: 999,
    },
];
static mut mps_operators: [operators; 28] = [
    operators {
        token: b"fshow\x00" as *const u8 as *const i8,
        opcode: 1001,
    },
    operators {
        token: b"startTexFig\x00" as *const u8 as *const i8,
        opcode: 1002,
    },
    operators {
        token: b"endTexFig\x00" as *const u8 as *const i8,
        opcode: 1003,
    },
    operators {
        token: b"hlw\x00" as *const u8 as *const i8,
        opcode: 1004,
    },
    operators {
        token: b"vlw\x00" as *const u8 as *const i8,
        opcode: 1005,
    },
    operators {
        token: b"l\x00" as *const u8 as *const i8,
        opcode: 37,
    },
    operators {
        token: b"r\x00" as *const u8 as *const i8,
        opcode: 38,
    },
    operators {
        token: b"c\x00" as *const u8 as *const i8,
        opcode: 35,
    },
    operators {
        token: b"m\x00" as *const u8 as *const i8,
        opcode: 33,
    },
    operators {
        token: b"p\x00" as *const u8 as *const i8,
        opcode: 32,
    },
    operators {
        token: b"n\x00" as *const u8 as *const i8,
        opcode: 31,
    },
    operators {
        token: b"C\x00" as *const u8 as *const i8,
        opcode: 72,
    },
    operators {
        token: b"G\x00" as *const u8 as *const i8,
        opcode: 70,
    },
    operators {
        token: b"R\x00" as *const u8 as *const i8,
        opcode: 71,
    },
    operators {
        token: b"lj\x00" as *const u8 as *const i8,
        opcode: 63,
    },
    operators {
        token: b"ml\x00" as *const u8 as *const i8,
        opcode: 64,
    },
    operators {
        token: b"lc\x00" as *const u8 as *const i8,
        opcode: 62,
    },
    operators {
        token: b"S\x00" as *const u8 as *const i8,
        opcode: 42,
    },
    operators {
        token: b"F\x00" as *const u8 as *const i8,
        opcode: 41,
    },
    operators {
        token: b"q\x00" as *const u8 as *const i8,
        opcode: 50,
    },
    operators {
        token: b"Q\x00" as *const u8 as *const i8,
        opcode: 51,
    },
    operators {
        token: b"s\x00" as *const u8 as *const i8,
        opcode: 53,
    },
    operators {
        token: b"t\x00" as *const u8 as *const i8,
        opcode: 52,
    },
    operators {
        token: b"sd\x00" as *const u8 as *const i8,
        opcode: 61,
    },
    operators {
        token: b"rd\x00" as *const u8 as *const i8,
        opcode: 1006,
    },
    operators {
        token: b"P\x00" as *const u8 as *const i8,
        opcode: 49,
    },
    operators {
        token: b"B\x00" as *const u8 as *const i8,
        opcode: 1007,
    },
    operators {
        token: b"W\x00" as *const u8 as *const i8,
        opcode: 44,
    },
];
unsafe fn get_opcode(mut token: *const i8) -> i32 {
    for i in 0..(::std::mem::size_of::<[operators; 48]>() as u64)
        .wrapping_div(::std::mem::size_of::<operators>() as u64) as usize
    {
        if streq_ptr(token, ps_operators[i].token) {
            return ps_operators[i].opcode;
        }
    }
    for i in 0..(::std::mem::size_of::<[operators; 28]>() as u64)
        .wrapping_div(::std::mem::size_of::<operators>() as u64) as usize
    {
        if streq_ptr(token, mps_operators[i].token) {
            return mps_operators[i].opcode;
        }
    }
    -1i32
}
static mut stack: [*mut pdf_obj; 1024] = [0 as *const pdf_obj as *mut pdf_obj; 1024];
static mut top_stack: u32 = 0_u32;
unsafe fn do_exch() -> i32 {
    if top_stack < 2_u32 {
        return -1i32;
    }
    let tmp = stack[top_stack.wrapping_sub(1_u32) as usize];
    stack[top_stack.wrapping_sub(1_u32) as usize] = stack[top_stack.wrapping_sub(2_u32) as usize];
    stack[top_stack.wrapping_sub(2_u32) as usize] = tmp;
    0i32
}
unsafe fn do_clear() -> i32 {
    while top_stack > 0_u32 {
        let mut tmp = if top_stack > 0_u32 {
            top_stack = top_stack.wrapping_sub(1);
            stack[top_stack as usize]
        } else {
            0 as *mut pdf_obj
        };
        pdf_release_obj(tmp);
    }
    0i32
}
unsafe fn pop_get_numbers(mut values: *mut f64, mut count: i32) -> i32 {
    loop {
        let fresh1 = count;
        count = count - 1;
        if !(fresh1 > 0i32) {
            break;
        }
        let tmp = if top_stack > 0_u32 {
            top_stack = top_stack.wrapping_sub(1);
            stack[top_stack as usize]
        } else {
            0 as *mut pdf_obj
        };
        if tmp.is_null() {
            warn!("mpost: Stack underflow.");
            break;
        } else if !(!tmp.is_null() && (*tmp).is_number()) {
            warn!("mpost: Not a number!");
            pdf_release_obj(tmp);
            break;
        } else {
            *values.offset(count as isize) = pdf_number_value(tmp);
            pdf_release_obj(tmp);
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
            let tmp = pdf_get_array(array, count);
            if !(!tmp.is_null() && (*tmp).is_number()) {
                warn!("mpost: Not a number!");
                break;
            } else {
                *values.offset(count as isize) = pdf_number_value(tmp)
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
    let tmp = pdf_lookup_dict(dict, "Type").filter(|&tmp| {
        (*tmp).is_name()
            && pdf_name_value(&*tmp).to_string_lossy() == "Font"
    });
    if tmp.is_none() {
        return false;
    }
    let tmp =
        pdf_lookup_dict(dict, "FontName").filter(|&tmp| (*tmp).is_name());
    if tmp.is_none() {
        return false;
    }
    let tmp =
        pdf_lookup_dict(dict, "FontScale").filter(|&tmp| (*tmp).is_number());
    tmp.is_some()
}
unsafe fn do_findfont() -> i32 {
    let mut error: i32 = 0i32;
    let font_name = if top_stack > 0_u32 {
        top_stack = top_stack.wrapping_sub(1);
        stack[top_stack as usize]
    } else {
        0 as *mut pdf_obj
    };
    if font_name.is_null() {
        return 1i32;
    } else {
        if !font_name.is_null() && (*font_name).is_string()
            || !font_name.is_null() && (*font_name).is_name()
        {
            /* Do not check the existence...
             * The reason for this is that we cannot locate PK font without
             * font scale.
             */
            let font_dict = pdf_new_dict();
            pdf_add_dict(font_dict, "Type", pdf_new_name("Font"));
            if !font_name.is_null() && (*font_name).is_string() {
                pdf_add_dict(
                    font_dict,
                    "FontName",
                    pdf_copy_name(pdf_string_value(font_name) as *const i8),
                );
                pdf_release_obj(font_name);
            } else {
                pdf_add_dict(font_dict, "FontName", font_name);
            }
            pdf_add_dict(font_dict, "FontScale", pdf_new_number(1.0f64));
            if top_stack < 1024_u32 {
                let fresh3 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh3 as usize] = font_dict
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                pdf_release_obj(font_dict);
                error = 1i32
            }
        } else {
            error = 1i32
        }
    }
    error
}
unsafe fn do_scalefont() -> i32 {
    let mut scale: f64 = 0.;
    let mut error = pop_get_numbers(&mut scale, 1i32);
    if error != 0 {
        return error;
    }
    let font_dict = if top_stack > 0_u32 {
        top_stack = top_stack.wrapping_sub(1);
        stack[top_stack as usize]
    } else {
        0 as *mut pdf_obj
    };
    if font_dict.is_null() {
        error = 1i32
    } else if is_fontdict(font_dict) {
        let font_scale = pdf_lookup_dict(font_dict, "FontScale").unwrap();
        pdf_set_number(font_scale, pdf_number_value(font_scale) * scale);
        if top_stack < 1024_u32 {
            let fresh4 = top_stack;
            top_stack = top_stack.wrapping_add(1);
            stack[fresh4 as usize] = font_dict
        } else {
            warn!("PS stack overflow including MetaPost file or inline PS code");
            pdf_release_obj(font_dict);
            error = 1i32
        }
    } else {
        error = 1i32
    }
    error
}
unsafe fn do_setfont() -> i32 {
    let font_dict = if top_stack > 0_u32 {
        top_stack = top_stack.wrapping_sub(1);
        stack[top_stack as usize]
    } else {
        0 as *mut pdf_obj
    };
    let error = if !is_fontdict(font_dict) {
        1
    } else {
        /* Subfont support prevent us from managing
         * font in a single place...
         */
        let font_name = pdf_name_value(&*pdf_lookup_dict(font_dict, "FontName").unwrap());
        let font_scale = pdf_number_value(pdf_lookup_dict(font_dict, "FontScale").unwrap());
        mp_setfont(font_name.as_ptr(), font_scale)
    };
    pdf_release_obj(font_dict);
    error
}
/* Push dummy font dict onto PS stack */
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
        pdf_add_dict(font_dict, "Type", pdf_new_name("Font"));
        pdf_add_dict(font_dict, "FontName", pdf_copy_name((*font).font_name));
        pdf_add_dict(font_dict, "FontScale", pdf_new_number((*font).pt_size));
        if top_stack < 1024_u32 {
            let fresh5 = top_stack;
            top_stack = top_stack.wrapping_add(1);
            stack[fresh5 as usize] = font_dict
        } else {
            warn!("PS stack overflow...");
            pdf_release_obj(font_dict);
            error = 1i32
        }
    }
    error
}
unsafe fn do_show() -> i32 {
    let mut cp = pdf_coord::zero();
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
    let text_str = if top_stack > 0_u32 {
        top_stack = top_stack.wrapping_sub(1);
        stack[top_stack as usize]
    } else {
        0 as *mut pdf_obj
    };
    if !(!text_str.is_null() && (*text_str).is_string()) {
        pdf_release_obj(text_str);
        return 1i32;
    }
    if (*font).font_id < 0i32 {
        warn!("mpost: not set.");
        pdf_release_obj(text_str);
        return 1i32;
    }
    let strptr = pdf_string_value(text_str) as *mut u8;
    let length = pdf_string_length(text_str) as i32;
    if (*font).tfm_id < 0i32 {
        warn!(
            "mpost: TFM not found for \"{}\".",
            CStr::from_ptr((*font).font_name).display()
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
    let mut start = ps_code;
    let end = start.offset(strlen(start) as isize);
    mp_parse_body(&mut start, end, x_user, y_user)
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
                fig_p.bbox.llx = values[2] * dvi2pts;
                fig_p.bbox.lly = -values[3] * dvi2pts;
                fig_p.bbox.urx = values[4] * dvi2pts;
                fig_p.bbox.ury = -values[5] * dvi2pts;
                fig_p.flags |= 1i32 << 0i32;
                sprintf(
                    resname.as_mut_ptr(),
                    b"__tf%d__\x00" as *const u8 as *const i8,
                    count,
                );
                xobj_id = pdf_doc_begin_grabbing(
                    resname.as_mut_ptr(),
                    fig_p.bbox.llx,
                    fig_p.bbox.ury,
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
unsafe fn ps_dev_CTM(M: &mut pdf_tmatrix) -> i32 {
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
unsafe fn do_operator(mut token: *const i8, mut x_user: f64, mut y_user: f64) -> i32 {
    let mut error: i32 = 0i32;
    let mut values: [f64; 12] = [0.; 12];
    let mut tmp: *mut pdf_obj = 0 as *mut pdf_obj;
    let mut matrix = pdf_tmatrix::new();
    let mut cp = pdf_coord::zero();
    let opcode = get_opcode(token);
    let mut current_block_294: u64;
    match opcode {
        1 => {
            /*
             * Arithmetic operators
             */
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh6 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh6 as usize] = pdf_new_number(values[0] + values[1])
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        3 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh7 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh7 as usize] = pdf_new_number(values[0] * values[1])
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        5 => {
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh8 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh8 as usize] = pdf_new_number(-values[0])
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        2 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh9 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh9 as usize] = pdf_new_number(values[0] - values[1])
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        4 => {
            error = pop_get_numbers(values.as_mut_ptr(), 2i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh10 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh10 as usize] = pdf_new_number(values[0] / values[1])
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        6 => {
            /* Round toward zero. */
            error = pop_get_numbers(values.as_mut_ptr(), 1i32);
            if error == 0 {
                if top_stack < 1024_u32 {
                    let fresh11 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh11 as usize] = pdf_new_number(if values[0] > 0i32 as f64 {
                        values[0].floor()
                    } else {
                        values[0].ceil()
                    })
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        10 => {
            /* Stack operation */
            error = do_clear()
        }
        12 => {
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            };
            pdf_release_obj(tmp);
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
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            };
            error = cvr_array(tmp, values.as_mut_ptr(), 6i32);
            tmp = 0 as *mut pdf_obj;
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
                let pattern = if top_stack > 0_u32 {
                    top_stack = top_stack.wrapping_sub(1);
                    stack[top_stack as usize]
                } else {
                    0 as *mut pdf_obj
                };
                if !(!pattern.is_null() && (*pattern).is_array()) {
                    pdf_release_obj(pattern);
                    error = 1i32
                } else {
                    num_dashes = pdf_array_length(pattern) as usize;
                    if num_dashes > 16 {
                        warn!("Too many dashes...");
                        pdf_release_obj(pattern);
                        error = 1i32
                    } else {
                        let mut i = 0;
                        while i < num_dashes && error == 0 {
                            let dash = pdf_get_array(pattern, i as i32);
                            if !(!dash.is_null() && (*dash).is_number()) {
                                error = 1i32
                            } else {
                                dash_values[i as usize] = pdf_number_value(dash)
                            }
                            i += 1
                        }
                        pdf_release_obj(pattern);
                        if error == 0 {
                            error = pdf_dev_setdash(&dash_values[..num_dashes], offset)
                        }
                    }
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
                if top_stack < 1024_u32 {
                    let fresh12 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh12 as usize] = pdf_new_number(cp.x);
                    if top_stack < 1024_u32 {
                        let fresh13 = top_stack;
                        top_stack = top_stack.wrapping_add(1);
                        stack[fresh13 as usize] = pdf_new_number(cp.y)
                    } else {
                        warn!("PS stack overflow including MetaPost file or inline PS code");
                        error = 1i32
                    }
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            }
        }
        82 => {
            let mut has_matrix: i32 = 0i32;
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            };
            if !tmp.is_null() && (*tmp).is_array() {
                error = cvr_array(tmp, values.as_mut_ptr(), 6i32);
                tmp = 0 as *mut pdf_obj;
                if error != 0 {
                    current_block_294 = 9125367800366194000;
                } else {
                    matrix.a = values[0];
                    matrix.b = values[1];
                    matrix.c = values[2];
                    matrix.d = values[3];
                    matrix.e = values[4];
                    matrix.f = values[5];
                    tmp = if top_stack > 0_u32 {
                        top_stack = top_stack.wrapping_sub(1);
                        stack[top_stack as usize]
                    } else {
                        0 as *mut pdf_obj
                    };
                    has_matrix = 1i32;
                    current_block_294 = 15375688482130298215;
                }
            } else {
                current_block_294 = 15375688482130298215;
            }
            match current_block_294 {
                9125367800366194000 => {}
                _ => {
                    if !(!tmp.is_null() && (*tmp).is_number()) {
                        error = 1i32
                    } else {
                        cp.y = pdf_number_value(tmp);
                        pdf_release_obj(tmp);
                        tmp = if top_stack > 0_u32 {
                            top_stack = top_stack.wrapping_sub(1);
                            stack[top_stack as usize]
                        } else {
                            0 as *mut pdf_obj
                        };
                        if !(!tmp.is_null() && (*tmp).is_number()) {
                            error = 1i32
                        } else {
                            cp.x = pdf_number_value(tmp);
                            pdf_release_obj(tmp);
                            if has_matrix == 0 {
                                ps_dev_CTM(&mut matrix);
                                /* Here, we need real PostScript CTM */
                            } /* This does pdf_release_obj() */
                            pdf_dev_dtransform(&mut cp, Some(&mut matrix));
                            if top_stack < 1024_u32 {
                                let fresh14 = top_stack;
                                top_stack = top_stack.wrapping_add(1);
                                stack[fresh14 as usize] = pdf_new_number(cp.x);
                                if top_stack < 1024_u32 {
                                    let fresh15 = top_stack;
                                    top_stack = top_stack.wrapping_add(1);
                                    stack[fresh15 as usize] = pdf_new_number(cp.y)
                                } else {
                                    warn!("PS stack overflow including MetaPost file or inline PS code");
                                    error = 1i32
                                }
                            } else {
                                warn!(
                                    "PS stack overflow including MetaPost file or inline PS code"
                                );
                                error = 1i32
                            }
                        }
                    }
                }
            }
        }
        81 => {
            let mut has_matrix_0: i32 = 0i32;
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            };
            if !tmp.is_null() && (*tmp).is_array() {
                error = cvr_array(tmp, values.as_mut_ptr(), 6i32);
                tmp = 0 as *mut pdf_obj;
                if error != 0 {
                    current_block_294 = 9125367800366194000;
                } else {
                    matrix.a = values[0];
                    matrix.b = values[1];
                    matrix.c = values[2];
                    matrix.d = values[3];
                    matrix.e = values[4];
                    matrix.f = values[5];
                    tmp = if top_stack > 0_u32 {
                        top_stack = top_stack.wrapping_sub(1);
                        stack[top_stack as usize]
                    } else {
                        0 as *mut pdf_obj
                    };
                    has_matrix_0 = 1i32;
                    current_block_294 = 9910899284672532069;
                }
            } else {
                current_block_294 = 9910899284672532069;
            }
            match current_block_294 {
                9125367800366194000 => {}
                _ => {
                    if !(!tmp.is_null() && (*tmp).is_number()) {
                        error = 1i32
                    } else {
                        cp.y = pdf_number_value(tmp);
                        pdf_release_obj(tmp);
                        tmp = if top_stack > 0_u32 {
                            top_stack = top_stack.wrapping_sub(1);
                            stack[top_stack as usize]
                        } else {
                            0 as *mut pdf_obj
                        };
                        if !(!tmp.is_null() && (*tmp).is_number()) {
                            error = 1i32
                        } else {
                            cp.x = pdf_number_value(tmp);
                            pdf_release_obj(tmp);
                            if has_matrix_0 == 0 {
                                ps_dev_CTM(&mut matrix);
                                /* Here, we need real PostScript CTM */
                            }
                            pdf_dev_idtransform(&mut cp, Some(&matrix));
                            if top_stack < 1024_u32 {
                                let fresh16 = top_stack;
                                top_stack = top_stack.wrapping_add(1);
                                stack[fresh16 as usize] = pdf_new_number(cp.x);
                                if top_stack < 1024_u32 {
                                    let fresh17 = top_stack;
                                    top_stack = top_stack.wrapping_add(1);
                                    stack[fresh17 as usize] = pdf_new_number(cp.y)
                                } else {
                                    warn!("PS stack overflow including MetaPost file or inline PS code");
                                    error = 1i32
                                }
                            } else {
                                warn!(
                                    "PS stack overflow including MetaPost file or inline PS code"
                                );
                                error = 1i32
                            }
                        }
                    }
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
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            };
            tmp = if top_stack > 0_u32 {
                top_stack = top_stack.wrapping_sub(1);
                stack[top_stack as usize]
            } else {
                0 as *mut pdf_obj
            }
        }
        _ => {
            if is_fontname(token) {
                if top_stack < 1024_u32 {
                    let fresh18 = top_stack;
                    top_stack = top_stack.wrapping_add(1);
                    stack[fresh18 as usize] = pdf_copy_name(token)
                } else {
                    warn!("PS stack overflow including MetaPost file or inline PS code");
                    error = 1i32
                }
            } else {
                warn!("Unknown token \"{}\"", CStr::from_ptr(token).display());
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
    mut start: *mut *const i8,
    mut end: *const i8,
    mut x_user: f64,
    mut y_user: f64,
) -> i32 {
    let mut obj: *mut pdf_obj = 0 as *mut pdf_obj;
    let mut error: i32 = 0i32;
    skip_white(start, end);
    while *start < end && error == 0 {
        if libc::isdigit(**start as _) != 0
            || *start < end.offset(-1)
                && (**start as i32 == '+' as i32
                    || **start as i32 == '-' as i32
                    || **start as i32 == '.' as i32)
        {
            let mut next: *mut i8 = 0 as *mut i8;
            let value = strtod(*start, &mut next);
            if next < end as *mut i8
                && strchr(b"<([{/%\x00" as *const u8 as *const i8, *next as i32).is_null()
                && libc::isspace(*next as _) == 0
            {
                warn!("Unkown PostScript operator.");
                dump(*start, next);
                error = 1i32
            } else if top_stack < 1024_u32 {
                let fresh19 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh19 as usize] = pdf_new_number(value);
                *start = next
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                error = 1i32;
                break;
            }
        /*
         * PDF parser can't handle PS operator inside arrays.
         * This shouldn't use parse_pdf_array().
         */
        } else if **start as i32 == '[' as i32 && {
            obj = parse_pdf_array(start, end, 0 as *mut pdf_file);
            !obj.is_null()
        } {
            if top_stack < 1024_u32 {
                let fresh20 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh20 as usize] = obj
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                error = 1i32;
                break;
            }
        /* This cannot handle ASCII85 string. */
        } else if *start < end.offset(-1)
            && (**start as i32 == '<' as i32 && *(*start).offset(1) as i32 == '<' as i32)
            && {
                obj = parse_pdf_dict(start, end, 0 as *mut pdf_file);
                !obj.is_null()
            }
        {
            if top_stack < 1024_u32 {
                let fresh21 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh21 as usize] = obj
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                error = 1i32;
                break;
            }
        } else if (**start as i32 == '(' as i32 || **start as i32 == '<' as i32) && {
            obj = parse_pdf_string(start, end);
            !obj.is_null()
        } {
            if top_stack < 1024_u32 {
                let fresh22 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh22 as usize] = obj
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                error = 1i32;
                break;
            }
        } else if **start as i32 == '/' as i32 && {
            obj = parse_pdf_name(start, end);
            !obj.is_null()
        } {
            if top_stack < 1024_u32 {
                let fresh23 = top_stack;
                top_stack = top_stack.wrapping_add(1);
                stack[fresh23 as usize] = obj
            } else {
                warn!("PS stack overflow including MetaPost file or inline PS code");
                error = 1i32;
                break;
            }
        } else {
            let token = parse_ident(start, end);
            if token.is_null() {
                error = 1i32
            } else {
                error = do_operator(token, x_user, y_user);
                free(token as *mut libc::c_void);
            }
        }
        skip_white(start, end);
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
    top_stack as i32
}
#[no_mangle]
pub unsafe extern "C" fn mps_exec_inline(
    mut p: *mut *const i8,
    mut endptr: *const i8,
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
    let error = mp_parse_body(p, endptr, x_user, y_user);
    //pdf_color_pop(); /* ... */
    pdf_dev_set_param(1i32, autorotate);
    pdf_dev_set_dirmode(dirmode);
    error
}
#[no_mangle]
pub unsafe extern "C" fn mps_do_page(mut image_file: *mut FILE) -> i32 {
    /* scale, xorig, yorig */
    let mut bbox = pdf_rect::new();
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
}
