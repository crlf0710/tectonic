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

use euclid::point2;

use crate::bridge::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_cff::cff_charsets_lookup_cid;
use super::dpx_cmap::{CMap_cache_get, CMap_decode};
use super::dpx_dvi::dvi_is_tracking_boxes;
use super::dpx_fontmap::fontmap;
use super::dpx_pdfcolor::{pdf_color_clear_stack, pdf_color_get_current};
use super::dpx_pdfdoc::pdf_doc_expand_box;
use super::dpx_pdfdoc::{pdf_doc_add_page_content, pdf_doc_add_page_resource};
use super::dpx_pdfdraw::{
    pdf_dev_clear_gstates, pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to,
    pdf_dev_gsave, pdf_dev_init_gstates, pdf_dev_rectclip,
};
use super::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_set_color, pdf_dev_transform};
use super::dpx_pdffont::{
    pdf_font_findresource, pdf_get_font_encoding, pdf_get_font_reference, pdf_get_font_subtype,
    pdf_get_font_usedchars, pdf_get_font_wmode,
};
use super::dpx_pdfximage::{
    pdf_ximage_get_reference, pdf_ximage_get_resname, pdf_ximage_scale_image,
};
use crate::dpx_pdfobj::{pdf_link_obj, pdf_obj, pdf_release_obj, pdfobj_escape_str};

use crate::bridge::size_t;

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum MotionState {
    GRAPHICS_MODE = 1,
    TEXT_MODE = 2,
    STRING_MODE = 3,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum TextWMode {
    HH = 0,
    HV = 1,
    VH = 4,
    VV = 5,
    HD = 3,
    VD = 7,
}

impl From<i32> for TextWMode {
    fn from(r: i32) -> Self {
        use TextWMode::*;
        match r {
            0 => HH,
            1 => HV,
            4 => VH,
            5 => VV,
            3 => HD,
            7 => VD,
            _ => unreachable!(),
        }
    }
}

fn ANGLE_CHANGES(m1: TextWMode, m2: TextWMode) -> bool {
    !(((m1 as i32) - (m2 as i32)).abs() % 5 == 0)
}
fn ROTATE_TEXT(m: TextWMode) -> bool {
    (m != TextWMode::HH) && (m != TextWMode::VV)
}

pub(crate) type spt_t = i32;

pub(crate) type TMatrix = euclid::Transform2D<f64, (), ()>;

pub(crate) type Rect = euclid::Box2D<f64, ()>;

pub trait Corner {
    fn lower_left(&self) -> Point;
    fn upper_right(&self) -> Point;
    fn lower_right(&self) -> Point;
    fn upper_left(&self) -> Point;
}
impl Corner for Rect {
    fn lower_left(&self) -> Point {
        self.min
    }
    fn upper_right(&self) -> Point {
        self.max
    }
    fn lower_right(&self) -> Point {
        point2(self.max.x, self.min.y)
    }
    fn upper_left(&self) -> Point {
        point2(self.min.x, self.max.y)
    }
}

/// Pointinate (point) in TeX document
pub(crate) type Point = euclid::Point2D<f64, ()>;

pub(crate) trait Equal {
    fn equal(&self, other: &Self) -> bool;
}

impl Equal for Point {
    fn equal(&self, other: &Self) -> bool {
        ((self.x - other.x).abs() < 1e-7) && ((self.y - other.y).abs() < 1e-7)
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct transform_info {
    pub(crate) width: f64,
    pub(crate) height: f64,
    pub(crate) depth: f64,
    pub(crate) matrix: TMatrix,
    pub(crate) bbox: Rect,
    pub(crate) flags: i32,
}
impl transform_info {
    pub(crate) const fn new() -> Self {
        Self {
            width: 0.,
            height: 0.,
            depth: 0.,
            matrix: TMatrix {
                m11: 1.,
                m12: 0.,
                m21: 0.,
                m22: 1.,
                m31: 0.,
                m32: 0.,
                _unit: core::marker::PhantomData,
            },
            bbox: Rect::new(point2(0., 0.), point2(0., 0.)),
            flags: 0,
        }
    }
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct dev_font {
    pub(crate) short_name: Vec<u8>,
    pub(crate) used_on_this_page: i32,
    pub(crate) tex_name: String,
    pub(crate) sptsize: spt_t,
    pub(crate) font_id: i32,
    pub(crate) enc_id: i32,
    pub(crate) real_font_index: i32,
    pub(crate) resource: *mut pdf_obj,
    pub(crate) used_chars: *mut i8,
    pub(crate) format: i32,
    pub(crate) wmode: i32,
    pub(crate) extend: f64,
    pub(crate) slant: f64,
    pub(crate) bold: f64,
    pub(crate) mapc: i32,
    pub(crate) ucs_group: i32,
    pub(crate) ucs_plane: i32,
    pub(crate) is_unicode: i32,
    pub(crate) cff_charsets: *mut cff_charsets,
}

impl Drop for dev_font {
    fn drop(&mut self) {
        unsafe {
            self.tex_name = String::new();
            pdf_release_obj(self.resource);
            self.resource = ptr::null_mut();
            self.cff_charsets = ptr::null_mut();
        }
    }
}

use super::dpx_cff::cff_charsets;
/*
 * Unit conversion, formatting and others.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct DevUnit {
    pub(crate) dvi2pts: f64,
    pub(crate) min_bp_val: i32,
    pub(crate) precision: i32,
    /* Number of decimal digits (in fractional part) kept. */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct TextState {
    pub(crate) font_id: i32,
    pub(crate) offset: spt_t,
    pub(crate) ref_x: spt_t,
    pub(crate) ref_y: spt_t,
    pub(crate) raise: spt_t,
    pub(crate) leading: spt_t,
    pub(crate) matrix: FontMatrix,
    pub(crate) bold_param: f64,
    pub(crate) dir_mode: i32,
    pub(crate) force_reset: i32,
    pub(crate) is_mb: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct FontMatrix {
    pub(crate) slant: f64,
    pub(crate) extend: f64,
    pub(crate) rotate: TextWMode,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct DevParam {
    pub(crate) autorotate: i32,
    pub(crate) colormode: i32,
}
/* Mapping types, MAP_IS_NAME is not supported. */
/* Lookup flags */
/* DEBUG */
/* Codespacerange */

use super::dpx_fontmap::fontmap_rec;
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0i32;

pub(crate) unsafe fn pdf_dev_set_verbose(level: i32) {
    verbose = level;
}
/* Not working yet... */

pub(crate) unsafe fn pdf_dev_scale() -> f64 {
    1.0f64
}
static mut dev_unit: DevUnit = DevUnit {
    dvi2pts: 0.0f64,
    min_bp_val: 658i32,
    precision: 2i32,
};

pub(crate) unsafe fn dev_unit_dviunit() -> f64 {
    1.0f64 / dev_unit.dvi2pts
}
static mut ten_pow: [u32; 10] = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
];
static mut ten_pow_inv: [f64; 10] = [
    1.0f64,
    0.1f64,
    0.01f64,
    0.001f64,
    0.0001f64,
    0.00001f64,
    0.000001f64,
    0.0000001f64,
    0.00000001f64,
    0.000000001f64,
];
/* NOTE: Acrobat 5 and prior uses 16.16 fixed point representation for
 * real numbers.
 */
fn p_dtoa(mut value: f64, prec: i32, b: &mut Vec<u8>) {
    static mut format_buffer: [u8; 512] = [0; 512];
    let buf = unsafe { &mut format_buffer };
    let p: [i32; 10] = [
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    ];
    let mut n = if value < 0. {
        value = -value;
        buf[0] = b'-';
        1
    } else {
        0
    };
    let f = value.fract();
    let mut i = value.trunc();
    let mut g = (f * p[prec as usize] as f64 + 0.5) as i32;
    if g == p[prec as usize] {
        g = 0;
        i += 1.
    }
    if i != 0. {
        let fv = format!("{:.0}", i);
        let m = fv.as_bytes().len();
        buf[n..n + m].copy_from_slice(&fv.as_bytes());
        n += m
    } else if g == 0 {
        buf[0] = b'0';
        n = 1;
    }
    if g != 0 {
        buf[n] = b'.';
        for j in (0..prec).rev() {
            buf[n + 1 + j as usize] = ((g % 10) as u8).wrapping_add(b'0'); // TODO: check
            g /= 10;
        }
        n += 1 + prec as usize;
        while buf[n - 1] == b'0' {
            n -= 1
        }
    }
    buf[n] = 0;

    b.extend_from_slice(&buf[..n as usize]);
}
unsafe fn dev_sprint_bp(buf: &mut Vec<u8>, value: spt_t, error: *mut spt_t) {
    let prec: i32 = dev_unit.precision;
    let value_in_bp = value as f64 * dev_unit.dvi2pts;
    if !error.is_null() {
        let error_in_bp = value_in_bp
            - (value_in_bp / ten_pow_inv[prec as usize] + 0.5f64).floor()
                * ten_pow_inv[prec as usize];
        *error = (error_in_bp / dev_unit.dvi2pts).round() as spt_t
    }
    p_dtoa(value_in_bp, prec, buf)
}
/* They are affected by precision (set at device initialization). */
pub(crate) fn pdf_sprint_matrix(buf: &mut Vec<u8>, M: &TMatrix) {
    let precision = unsafe { dev_unit.precision };
    let prec2: i32 = if precision + 2 < 8i32 {
        precision + 2
    } else {
        8i32
    }; /* xxx_sprint_xxx NULL terminates strings. */
    let prec0: i32 = if precision > 2i32 { precision } else { 2i32 }; /* xxx_sprint_xxx NULL terminates strings. */
    p_dtoa(M.m11, prec2, buf); /* xxx_sprint_xxx NULL terminates strings. */
    buf.push(b' '); /* xxx_sprint_xxx NULL terminates strings. */
    p_dtoa(M.m12, prec2, buf);
    buf.push(b' ');
    p_dtoa(M.m21, prec2, buf);
    buf.push(b' ');
    p_dtoa(M.m22, prec2, buf);
    buf.push(b' ');
    p_dtoa(M.m31, prec0, buf);
    buf.push(b' ');
    p_dtoa(M.m32, prec0, buf);
}
pub(crate) fn pdf_sprint_rect(buf: &mut Vec<u8>, rect: &Rect) {
    let precision = unsafe { dev_unit.precision };
    p_dtoa(rect.min.x, precision, buf);
    buf.push(b' ');
    p_dtoa(rect.min.y, precision, buf);
    buf.push(b' ');
    p_dtoa(rect.max.x, precision, buf);
    buf.push(b' ');
    p_dtoa(rect.max.y, precision, buf);
}
pub(crate) fn pdf_sprint_coord(buf: &mut Vec<u8>, p: &Point) {
    let precision = unsafe { dev_unit.precision };
    p_dtoa(p.x, precision, buf);
    buf.push(b' ');
    p_dtoa(p.y, precision, buf);
}
pub(crate) fn pdf_sprint_length(buf: &mut Vec<u8>, value: f64) {
    p_dtoa(value, unsafe { dev_unit.precision }, buf);
}
pub(crate) fn pdf_sprint_number(buf: &mut Vec<u8>, value: f64) {
    p_dtoa(value, 8, buf);
}
static mut dev_param: DevParam = DevParam {
    autorotate: 1i32,
    colormode: 1i32,
};
static mut motion_state: MotionState = MotionState::GRAPHICS_MODE;
static mut text_state: TextState = TextState {
    font_id: -1,
    offset: 0,
    ref_x: 0,
    ref_y: 0,
    raise: 0,
    leading: 0,
    matrix: FontMatrix {
        slant: 0.,
        extend: 1.,
        rotate: TextWMode::HH,
    },
    bold_param: 0.,
    dir_mode: 0,
    force_reset: 0,
    is_mb: 0,
};
static mut dev_fonts: Vec<dev_font> = Vec::new();
static mut num_phys_fonts: i32 = 0i32;
unsafe fn dev_set_text_matrix(
    xpos: spt_t,
    ypos: spt_t,
    slant: f64,
    extend: f64,
    rotate: TextWMode,
) {
    /* slant is negated for vertical font so that right-side
     * is always lower. */
    let tma;
    let tmb;
    let tmc;
    let tmd;
    match rotate {
        TextWMode::VH => {
            /* Vertical font */
            tma = slant;
            tmb = 1.;
            tmc = -extend;
            tmd = 0.;
        }
        TextWMode::HV => {
            /* Horizontal font */
            tma = 0.;
            tmb = -extend;
            tmc = 1.;
            tmd = -slant;
        }
        TextWMode::HH => {
            /* Horizontal font */
            tma = extend;
            tmb = 0.;
            tmc = slant;
            tmd = 1.;
        }
        TextWMode::VV => {
            /* Vertical font */
            tma = 1.;
            tmb = -slant;
            tmc = 0.;
            tmd = extend;
        }
        TextWMode::HD => {
            /* Horizontal font */
            tma = 0.;
            tmb = extend;
            tmc = -1.;
            tmd = slant;
        }
        TextWMode::VD => {
            /* Vertical font */
            tma = -1.; /* op: Tm */
            tmb = slant;
            tmc = 0.;
            tmd = -extend;
        }
    }
    let mut tm = TMatrix::row_major(
        tma,
        tmb,
        tmc,
        tmd,
        xpos as f64 * dev_unit.dvi2pts,
        ypos as f64 * dev_unit.dvi2pts,
    );
    let mut buf = Vec::new();
    buf.push(b' ');
    pdf_sprint_matrix(&mut buf, &mut tm);
    buf.push(b' ');
    buf.push(b'T');
    buf.push(b'm');
    pdf_doc_add_page_content(&buf);
    text_state.ref_x = xpos;
    text_state.ref_y = ypos;
    text_state.matrix.slant = slant;
    text_state.matrix.extend = extend;
    text_state.matrix.rotate = rotate;
}
/*
 * reset_text_state() outputs a BT and does any necessary coordinate
 * transformations to get ready to ship out text.
 */
unsafe fn reset_text_state() {
    /*
     * We need to reset the line matrix to handle slanted fonts.
     */
    pdf_doc_add_page_content(b" BT"); /* op: BT */
    /*
     * text_state.matrix is identity at top of page.
     * This sometimes write unnecessary "Tm"s when transition from
     * GRAPHICS_MODE to TEXT_MODE occurs.
     */
    if text_state.force_reset != 0
        || text_state.matrix.slant != 0.0f64
        || text_state.matrix.extend != 1.0f64
        || ROTATE_TEXT(text_state.matrix.rotate)
    {
        dev_set_text_matrix(
            0i32,
            0i32,
            text_state.matrix.slant,
            text_state.matrix.extend,
            text_state.matrix.rotate,
        ); /* op: TJ */
    } /* op: TJ */
    text_state.ref_x = 0i32;
    text_state.ref_y = 0i32;
    text_state.offset = 0i32;
    text_state.force_reset = 0i32;
}
unsafe fn text_mode() {
    match motion_state {
        MotionState::STRING_MODE => {
            pdf_doc_add_page_content(if text_state.is_mb != 0 {
                b">]TJ"
            } else {
                b")]TJ"
            });
        }
        MotionState::GRAPHICS_MODE => {
            reset_text_state();
        }
        MotionState::TEXT_MODE => {}
    }
    motion_state = MotionState::TEXT_MODE;
    text_state.offset = 0i32;
}

pub(crate) unsafe fn graphics_mode() {
    match motion_state {
        MotionState::GRAPHICS_MODE => {}
        MotionState::STRING_MODE | MotionState::TEXT_MODE => {
            if let MotionState::STRING_MODE = motion_state {
                pdf_doc_add_page_content(if text_state.is_mb != 0 {
                    b">]TJ"
                } else {
                    b")]TJ"
                });
            }

            pdf_doc_add_page_content(b" ET"); /* op: ET */
            text_state.force_reset = 0i32;
            text_state.font_id = -1i32
        }
    }
    motion_state = MotionState::GRAPHICS_MODE;
}
unsafe fn start_string(xpos: spt_t, ypos: spt_t, slant: f64, extend: f64, rotate: TextWMode) {
    let mut error_delx: spt_t = 0i32;
    let mut error_dely: spt_t = 0i32;
    let delx = xpos - text_state.ref_x;
    let dely = ypos - text_state.ref_y;
    /*
     * Precompensating for line transformation matrix.
     *
     * Line transformation matrix L for horizontal font in horizontal
     * mode and it's inverse I is
     *
     *          | e  0|          | 1/e  0|
     *   L_hh = |     | , I_hh = |       |
     *          | s  1|          |-s/e  1|
     *
     * For vertical font in vertical mode,
     *
     *          | 1  -s|          | 1  s/e|
     *   L_vv = |      | , I_vv = |       |
     *          | 0   e|          | 0  1/e|
     *
     * For vertical font in horizontal mode,
     *
     *          | s   1|          | 0  1|
     *   L_vh = |      | = L_vv x |     |
     *          |-e   0|          |-1  0|
     *
     *          | 0  -1|
     *   I_vh = |      | x I_vv
     *          | 1   0|
     *
     * For horizontal font in vertical mode,
     *
     *          | 0  -e|          | 0  -1|
     *   L_hv = |      | = L_hh x |      |
     *          | 1  -s|          | 1   0|
     *
     *          | 0   1|
     *   I_hv = |      | x I_hh
     *          |-1   0|
     *
     */
    let mut buf = Vec::new();
    match rotate {
        TextWMode::VH => {
            /* Vertical font in horizontal mode: rot = +90
             *                           | 0  -1/e|
             * d_user =  d x I_vh = d x  |        |
             *                           | 1   s/e|
             */
            let desired_delx = dely;
            let desired_dely = (-(delx as f64 - dely as f64 * slant) / extend) as spt_t;
            /* error_del is in device space
             *
             *               | 0  1|
             *  e = e_user x |     | = (-e_user_y, e_user_x)
             *               |-1  0|
             *
             * We must care about rotation here but not extend/slant...
             * The extend and slant actually is font matrix.
             */
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_dely);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_delx);
            error_delx = -error_delx
        }
        TextWMode::HV => {
            /* Horizontal font in vertical mode: rot = -90
             *
             *                         |-s/e  1|
             * d_user = d x I_hv = d x |       |
             *                         |-1/e  0|
             */
            let desired_delx = (-(dely as f64 + delx as f64 * slant) / extend) as spt_t;
            let desired_dely = delx;
            /*
             * e = (e_user_y, -e_user_x)
             */
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_dely);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_delx);
            error_dely = -error_dely
        }
        TextWMode::HH => {
            /* Horizontal font in horizontal mode:
             *                         | 1/e    0|
             * d_user = d x I_hh = d x |         |
             *                         |-s/e    1|
             */
            let desired_delx = ((delx as f64 - dely as f64 * slant) / extend) as spt_t;
            let desired_dely = dely;
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_delx);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_dely)
        }
        TextWMode::VV => {
            /* Vertical font in vertical mode:
             *                         | 1  s/e|
             * d_user = d x I_vv = d x |       |
             *                         | 0  1/e|
             */
            let desired_delx = delx;
            let desired_dely = ((dely as f64 + delx as f64 * slant) / extend) as spt_t;
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_delx);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_dely);
        }
        TextWMode::HD => {
            /* Horizontal font in down-to-up mode: rot = +90
             *
             *                          | s/e  -1|
             * d_user = d x -I_hv = d x |        |
             *                          | 1/e   0|
             */
            let desired_delx = -((-(dely as f64 + delx as f64 * slant) / extend) as spt_t);
            let desired_dely = -delx;
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_dely);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_delx);
            error_delx = -error_delx;
            error_dely = -error_dely
        }
        TextWMode::VD => {
            /* Vertical font in down-to-up mode: rot = 180
             *                          |-1 -s/e|
             * d_user = d x -I_vv = d x |       |
             *                          | 0 -1/e|
             */
            let desired_delx = -delx; /* op: */
            let desired_dely = -(((dely as f64 + delx as f64 * slant) / extend) as spt_t);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_delx, &mut error_delx);
            buf.push(b' ');
            dev_sprint_bp(&mut buf, desired_dely, &mut error_dely);
            error_delx = -error_delx;
            error_dely = -error_dely;
        }
    }
    pdf_doc_add_page_content(&buf);
    /*
     * dvipdfm wrongly using "TD" in place of "Td".
     * The TD operator set leading, but we are not using T* etc.
     */
    pdf_doc_add_page_content(if text_state.is_mb != 0 {
        b" Td[<"
    } else {
        b" Td[("
    }); /* op: Td */
    /* Error correction */
    text_state.ref_x = xpos - error_delx;
    text_state.ref_y = ypos - error_dely;
    text_state.offset = 0i32;
}
unsafe fn string_mode(xpos: spt_t, ypos: spt_t, slant: f64, extend: f64, rotate: TextWMode) {
    match motion_state {
        MotionState::GRAPHICS_MODE | MotionState::TEXT_MODE => {
            if let MotionState::GRAPHICS_MODE = motion_state {
                reset_text_state();
            }

            if text_state.force_reset != 0 {
                dev_set_text_matrix(xpos, ypos, slant, extend, rotate); /* op: */
                pdf_doc_add_page_content(if text_state.is_mb != 0 { b"[<" } else { b"[(" });
                text_state.force_reset = 0i32
            } else {
                start_string(xpos, ypos, slant, extend, rotate);
            }
        }
        MotionState::STRING_MODE => {}
    }
    motion_state = MotionState::STRING_MODE;
}
/*
 * The purpose of the following routine is to force a Tf only
 * when it's actually necessary.  This became a problem when the
 * VF code was added.  The VF spec says to instantiate the
 * first font contained in the VF file before drawing a virtual
 * character.  However, that font may not be used for
 * many characters (e.g. small caps fonts).  For this reason,
 * dev_select_font() should not force a "physical" font selection.
 * This routine prevents a PDF Tf font selection until there's
 * really a character in that font.
 */
unsafe fn dev_set_font(font_id: i32) -> i32 {
    /* text_mode() must come before text_state.is_mb is changed. */
    text_mode(); /* Caller should check font_id. */
    let font = &mut dev_fonts[font_id as usize]; /* space not necessary. */
    text_state.is_mb = if font.format == 3i32 { 1i32 } else { 0i32 };
    let vert_font = if font.wmode != 0 { 1 } else { 0 };
    let vert_dir = if dev_param.autorotate != 0 {
        text_state.dir_mode
    } else {
        vert_font
    };
    let text_rotate = TextWMode::from(vert_font << 2 | vert_dir);
    if font.slant != text_state.matrix.slant
        || font.extend != text_state.matrix.extend
        || ANGLE_CHANGES(text_rotate, text_state.matrix.rotate)
    {
        text_state.force_reset = 1i32
    }
    text_state.matrix.slant = font.slant;
    text_state.matrix.extend = font.extend;
    text_state.matrix.rotate = text_rotate;
    let bold = font.bold;
    let sptsize = font.sptsize;
    let real_font = if font.real_font_index >= 0 {
        &mut dev_fonts[font.real_font_index as usize]
    } else {
        font
    };
    if real_font.resource.is_null() {
        real_font.resource = pdf_get_font_reference(real_font.font_id);
        real_font.used_chars = pdf_get_font_usedchars(real_font.font_id)
    }
    if real_font.used_on_this_page == 0 {
        pdf_doc_add_page_resource(
            "Font",
            &real_font.short_name,
            pdf_link_obj(real_font.resource),
        );
        real_font.used_on_this_page = 1i32
    }
    let font_scale = sptsize as f64 * dev_unit.dvi2pts;
    let mut buf = vec![b' ', b'/'];
    buf.extend_from_slice(real_font.short_name.as_slice());
    buf.push(b' ');
    p_dtoa(
        font_scale,
        if dev_unit.precision + 1 < 8 {
            dev_unit.precision + 1
        } else {
            8
        },
        &mut buf,
    );
    buf.push(b' ');
    buf.push(b'T');
    buf.push(b'f');
    pdf_doc_add_page_content(buf.as_slice());
    if bold > 0. || bold != text_state.bold_param {
        let content = if bold <= 0. {
            " 0 Tr".to_string()
        } else {
            format!(" 2 Tr {:.6} w", bold)
        };
        pdf_doc_add_page_content(content.as_bytes());
        /* op: Tr w */
    }
    text_state.bold_param = bold;
    text_state.font_id = font_id;
    0i32
}
/* Access text state parameters.
 */

pub(crate) unsafe fn pdf_dev_get_font_wmode(font_id: i32) -> i32 {
    let font = &mut dev_fonts[font_id as usize] as *mut dev_font;
    if !font.is_null() {
        return (*font).wmode;
    }
    0i32
}
static mut sbuf0: [u8; 4096] = [0; 4096];
static mut sbuf1: [u8; 4096] = [0; 4096];
unsafe fn handle_multibyte_string(
    font: &dev_font,
    str_ptr: *mut *const u8,
    str_len: *mut size_t,
    ctype: i32,
) -> i32 {
    let mut p = *str_ptr;
    let mut length = *str_len;
    if ctype == -1i32 && !font.cff_charsets.is_null() {
        /* freetype glyph indexes */
        /* Convert freetype glyph indexes to CID. */
        let mut inbuf: *const u8 = p;
        let mut outbuf: *mut u8 = sbuf0.as_mut_ptr();
        for _ in (0..length).step_by(2) {
            let mut gid = ((*inbuf as i32) << 8) as u32;
            inbuf = inbuf.offset(1);
            gid = gid.wrapping_add(*inbuf as u32);
            inbuf = inbuf.offset(1);
            gid = cff_charsets_lookup_cid(&*font.cff_charsets, gid as u16) as u32;
            *outbuf = (gid >> 8) as u8;
            outbuf = outbuf.offset(1);
            *outbuf = (gid & 0xff) as u8;
            outbuf = outbuf.offset(1);
        }
        p = sbuf0.as_mut_ptr();
        length = outbuf.offset_from(sbuf0.as_mut_ptr()) as i64 as size_t
    } else if font.is_unicode != 0 {
        /* _FIXME_ */
        /* UCS-4 */
        if ctype == 1i32 {
            if length.wrapping_mul(4) >= 4096 {
                warn!("Too long string...");
                return -1i32;
            }
            for i in 0..length {
                sbuf1[i.wrapping_mul(4) as usize] = font.ucs_group as u8;
                sbuf1[i.wrapping_mul(4).wrapping_add(1) as usize] = font.ucs_plane as u8;
                sbuf1[i.wrapping_mul(4).wrapping_add(2) as usize] = '\u{0}' as i32 as u8;
                sbuf1[i.wrapping_mul(4).wrapping_add(3) as usize] = *p.offset(i as isize);
            }
            length = (length as u64).wrapping_mul(4) as size_t as size_t
        } else if ctype == 2i32 {
            let mut len: size_t = 0i32 as size_t;
            if length.wrapping_mul(2) >= 4096 {
                warn!("Too long string...");
                return -1i32;
            }
            let mut i = 0i32 as size_t;
            while i < length {
                sbuf1[len as usize] = font.ucs_group as u8;
                if *p.offset(i as isize) as i32 & 0xf8 == 0xd8 {
                    /* Check for valid surrogate pair.  */
                    if *p.offset(i as isize) as i32 & 0xfc != 0xd8
                        || i.wrapping_add(2) >= length
                        || *p.offset(i.wrapping_add(2) as isize) as i32 & 0xfc != 0xdc
                    {
                        warn!(
                            "Invalid surrogate p[{}]={:02X}...",
                            i,
                            *p.offset(i as isize) as i32,
                        );
                        return -1i32;
                    }
                    let c = ((*p.offset(i as isize) as i32 & 0x3) << 10
                        | (*p.offset(i.wrapping_add(1) as isize) as i32) << 2
                        | *p.offset(i.wrapping_add(2) as isize) as i32 & 0x3)
                        + 0x100;
                    sbuf1[len.wrapping_add(1) as usize] = (c >> 8 & 0xff) as u8;
                    sbuf1[len.wrapping_add(2) as usize] = (c & 0xff) as u8;
                    i = (i as u64).wrapping_add(2) as size_t as size_t
                } else {
                    sbuf1[len.wrapping_add(1) as usize] = font.ucs_plane as u8;
                    sbuf1[len.wrapping_add(2) as usize] = *p.offset(i as isize)
                }
                sbuf1[len.wrapping_add(3) as usize] = *p.offset(i.wrapping_add(1) as isize);
                i = (i as u64).wrapping_add(2) as size_t as size_t;
                len = (len as u64).wrapping_add(4) as size_t as size_t
            }
            length = len
        }
        p = sbuf1.as_mut_ptr()
    } else if ctype == 1 && font.mapc >= 0 {
        /* Omega workaround...
         * Translate single-byte chars to double byte code space.
         */
        if length.wrapping_mul(2) >= 4096 {
            warn!("Too long string...");
            return -1;
        }
        for i in 0..length {
            sbuf1[i.wrapping_mul(2) as usize] = (font.mapc & 0xff) as u8;
            sbuf1[i.wrapping_mul(2).wrapping_add(1) as usize] = *p.offset(i as isize);
        }
        length = (length as u64).wrapping_mul(2) as size_t as size_t;
        p = sbuf1.as_mut_ptr()
    }
    /*
     * Font is double-byte font. Output is assumed to be 16-bit fixed length
     * encoding.
     * TODO: A character decomposed to multiple characters.
     */
    if ctype != -1i32 && font.enc_id >= 0 {
        let cmap = CMap_cache_get(font.enc_id);
        let mut inbuf_0 = p;
        let mut outbuf_0 = sbuf0.as_mut_ptr();
        let mut inbytesleft = length;
        let mut outbytesleft = 4096;
        CMap_decode(
            &*cmap,
            &mut inbuf_0,
            &mut inbytesleft,
            &mut outbuf_0,
            &mut outbytesleft,
        );
        if inbytesleft != 0 {
            warn!("CMap conversion failed. ({} bytes remains)", inbytesleft);
            return -1i32;
        }
        length = (4096 as size_t).wrapping_sub(outbytesleft);
        p = sbuf0.as_mut_ptr()
    }
    *str_ptr = p;
    *str_len = length;
    0i32
}
static mut dev_coords: Vec<Point> = Vec::new();

pub(crate) unsafe fn pdf_dev_get_coord() -> Point {
    dev_coords.last().copied().unwrap_or(Point::zero())
}

pub(crate) unsafe fn pdf_dev_push_coord(xpos: f64, ypos: f64) {
    dev_coords.push(Point::new(xpos, ypos));
}

pub(crate) unsafe fn pdf_dev_pop_coord() {
    let _ = dev_coords.pop();
}
/*
 * ctype:
 *  -1 input string contains 2-byte Freetype glyph index values
 *     (XeTeX only)
 *  0  byte-width of char can be variable and input string
 *     is properly encoded.
 *  n  Single character cosumes n bytes in input string.
 *
 * _FIXME_
 * -->
 * selectfont(font_name, point_size) and show_string(pos, string)
 */

pub(crate) unsafe fn pdf_dev_set_string(
    mut xpos: spt_t,
    mut ypos: spt_t,
    instr_ptr: *const libc::c_void,
    instr_len: size_t,
    width: spt_t,
    font_id: i32,
    ctype: i32,
) {
    /* Pointer to the reencoded string. */
    if font_id < 0 || font_id as usize >= dev_fonts.len() {
        panic!("Invalid font: {} ({})", font_id, dev_fonts.len());
    }
    if font_id != text_state.font_id {
        dev_set_font(font_id);
    }
    let font = if text_state.font_id < 0 {
        ptr::null_mut()
    } else {
        &mut dev_fonts[text_state.font_id as usize] as *mut dev_font
    };
    if font.is_null() {
        panic!("Currentfont not set.");
    }
    let real_font = if (*font).real_font_index >= 0 {
        &mut dev_fonts[(*font).real_font_index as usize] as *mut dev_font
    } else {
        font
    };
    let text_xorigin = text_state.ref_x;
    let text_yorigin = text_state.ref_y;
    let mut str_ptr = instr_ptr as *const u8;
    let mut length = instr_len;
    if (*font).format == 3i32 {
        if handle_multibyte_string(&*font, &mut str_ptr, &mut length, ctype) < 0 {
            panic!("Error in converting input string...");
        }
        if !(*real_font).used_chars.is_null() {
            for i in (0..length).step_by(2) {
                let cid: u16 = ((*str_ptr.offset(i as isize) as i32) << 8
                    | *str_ptr.offset((i + 1) as isize) as i32)
                    as u16;
                *(*real_font).used_chars.offset((cid as i32 / 8) as isize) |=
                    (1i32 << 7 - cid as i32 % 8) as i8;
            }
        }
    } else {
        if !(*real_font).used_chars.is_null() {
            for i in 0..length {
                *(*real_font)
                    .used_chars
                    .offset(*str_ptr.offset(i as isize) as isize) = 1_i8;
            }
        }
    }
    if let Some(last) = dev_coords.last() {
        xpos -= (last.x / dev_unit.dvi2pts).round() as spt_t;
        ypos -= (last.y / dev_unit.dvi2pts).round() as spt_t
    }
    /*
     * Kern is in units of character units, i.e., 1000 = 1 em.
     *
     * Positive kern means kerning (reduce excess white space).
     *
     * The following formula is of the form a*x/b where a, x, and b are signed long
     * integers.  Since in integer arithmetic (a*x) could overflow and a*(x/b) would
     * not be accurate, we use floating point arithmetic rather than trying to do
     * this all with integer arithmetic.
     *
     * 1000.0 / (font->extend * font->sptsize) is caluculated each times...
     * Is accuracy really a matter? Character widths are always rounded to integer
     * (in 1000 units per em) but dvipdfmx does not take into account of this...
     */
    let (delh, delv) = if text_state.dir_mode == 0i32 {
        /* Left-to-right */
        (text_xorigin + text_state.offset - xpos, ypos - text_yorigin)
    } else if text_state.dir_mode == 1i32 {
        /* Top-to-bottom */
        (ypos - text_yorigin + text_state.offset, xpos - text_xorigin)
    } else {
        /* Bottom-to-top */
        (ypos + text_yorigin + text_state.offset, xpos + text_xorigin)
    };
    /* White-space more than 3em is not considered as a part of single text.
     * So we will break string mode in that case.
     * Dvipdfmx spend most of time processing strings with kern = 0 (but far
     * more times in font handling).
     * You may want to use pre-calculated value for WORD_SPACE_MAX.
     * More text compression may be possible by replacing kern with space char
     * when -kern is equal to space char width.
     */
    let kern = if text_state.force_reset != 0
        || (delv as i64).abs() > dev_unit.min_bp_val as i64
        || (delh as i64).abs() > (3.0f64 * (*font).extend * (*font).sptsize as f64) as spt_t as i64
    {
        text_mode();
        0
    } else {
        (1000.0f64 / (*font).extend * delh as f64 / (*font).sptsize as f64) as spt_t
    };
    /* Inaccucary introduced by rounding of character width appears within
     * single text block. There are point_size/1000 rounding error per character.
     * If you really care about accuracy, you should compensate this here too.
     */
    let mut buf = Vec::new();
    if motion_state != MotionState::STRING_MODE {
        string_mode(
            xpos,
            ypos,
            (*font).slant,
            (*font).extend,
            text_state.matrix.rotate,
        );
    } else if kern != 0 {
        /*
         * Same issues as earlier. Use floating point for simplicity.
         * This routine needs to be fast, so we don't call sprintf() or strcpy().
         */
        text_state.offset -=
            (kern as f64 * (*font).extend * ((*font).sptsize as f64 / 1000.0f64)) as spt_t; /* op: */
        buf.push(if text_state.is_mb != 0 { b'>' } else { b')' });
        if (*font).wmode != 0 {
            itoa::write(&mut buf, -kern).unwrap();
        } else {
            itoa::write(&mut buf, kern).unwrap();
        }
        buf.push(if text_state.is_mb != 0 { b'<' } else { b'(' });
        pdf_doc_add_page_content(buf.as_slice());
        buf.clear();
    }
    if text_state.is_mb != 0 {
        for i in 0..length {
            let first = *str_ptr.offset(i as isize) >> 4 & 0xf;
            let second = *str_ptr.offset(i as isize) & 0xf;
            buf.push(if first >= 10 {
                first + b'W'
            } else {
                first + b'0'
            });
            buf.push(if second >= 10 {
                second + b'W'
            } else {
                second + b'0'
            });
        }
    } else {
        pdfobj_escape_str(&mut buf, str_ptr, length);
    }
    /* I think if you really care about speed, you should avoid memcopy here. */
    pdf_doc_add_page_content(buf.as_slice()); /* op: */
    text_state.offset += width;
}

pub(crate) unsafe fn pdf_init_device(dvi2pts: f64, precision: i32, black_and_white: i32) {
    if precision < 0i32 || precision > 8i32 {
        warn!("Number of decimal digits out of range [0-{}].", 8i32);
    }
    if precision < 0i32 {
        dev_unit.precision = 0i32
    } else if precision > 8i32 {
        dev_unit.precision = 8i32
    } else {
        dev_unit.precision = precision
    }
    dev_unit.dvi2pts = dvi2pts;
    dev_unit.min_bp_val =
        ((1.0f64 / (ten_pow[dev_unit.precision as usize] as f64 * dvi2pts) / 1i32 as f64 + 0.5f64)
            .floor()
            * 1i32 as f64) as i32;
    if dev_unit.min_bp_val < 0i32 {
        dev_unit.min_bp_val = -dev_unit.min_bp_val
    }
    dev_param.colormode = if black_and_white != 0 { 0i32 } else { 1i32 };
    graphics_mode();
    pdf_color_clear_stack();
    pdf_dev_init_gstates();
    dev_fonts = Vec::new();
    dev_coords = Vec::new();
}

pub(crate) unsafe fn pdf_close_device() {
    dev_fonts = Vec::new();
    dev_coords = Vec::new();
    pdf_dev_clear_gstates();
}
/*
 * BOP, EOP, and FONT section.
 * BOP and EOP manipulate some of the same data structures
 * as the font stuff.
 */

pub(crate) unsafe fn pdf_dev_reset_fonts(newpage: i32) {
    for i in 0..dev_fonts.len() {
        dev_fonts[i as usize].used_on_this_page = 0i32;
    }
    text_state.font_id = -1i32;
    text_state.matrix.slant = 0.0f64;
    text_state.matrix.extend = 1.0f64;
    text_state.matrix.rotate = TextWMode::HH;
    if newpage != 0 {
        text_state.bold_param = 0.0f64
    }
    text_state.is_mb = 0i32;
}

pub(crate) unsafe fn pdf_dev_reset_color(force: i32) {
    let (sc, fc) = pdf_color_get_current();
    pdf_dev_set_color(sc, 0, force);
    pdf_dev_set_color(fc, 0x20, force);
}

pub(crate) unsafe fn pdf_dev_bop(M: &TMatrix) {
    graphics_mode();
    text_state.force_reset = 0i32;
    pdf_dev_gsave();
    pdf_dev_concat(M);
    pdf_dev_reset_fonts(1i32);
    pdf_dev_reset_color(0i32);
}

pub(crate) unsafe fn pdf_dev_eop() {
    graphics_mode();
    let depth = pdf_dev_current_depth();
    if depth != 1 {
        warn!("Unbalenced q/Q nesting...: {}", depth);
        pdf_dev_grestore_to(0);
    } else {
        pdf_dev_grestore();
    };
}
unsafe fn print_fontmap(font_name: &str, mrec: &fontmap_rec) {
    info!("\n");
    info!("fontmap: {} -> {}", font_name, mrec.font_name,);
    if !mrec.enc_name.is_empty() {
        info!("({})", mrec.enc_name);
    }
    if mrec.opt.extend != 1.0f64 {
        info!("[extend:{}]", mrec.opt.extend);
    }
    if mrec.opt.slant != 0.0f64 {
        info!("[slant:{}]", mrec.opt.slant);
    }
    if mrec.opt.bold != 0.0f64 {
        info!("[bold:{}]", mrec.opt.bold);
    }
    if mrec.opt.flags & 1i32 << 1i32 != 0 {
        info!("[noemb]");
    }
    if mrec.opt.mapc >= 0i32 {
        info!("[map:<{:02x}>]", mrec.opt.mapc);
    }
    if !mrec.opt.charcoll.is_empty() {
        info!("[csi:{}]", mrec.opt.charcoll);
    }
    if mrec.opt.index != 0 {
        info!("[index:{}]", mrec.opt.index);
    }
    match mrec.opt.style {
        1 => {
            info!("[style:bold]");
        }
        2 => {
            info!("[style:italic]");
        }
        3 => {
            info!("[style:bolditalic]");
        }
        _ => {}
    }
    info!("\n");
}
/* _FIXME_
 * Font is identified with font_name and point_size as in DVI here.
 * However, except for PDF_FONTTYPE_BITMAP, we can share the
 * short_name, resource and used_chars between multiple instances
 * of the same font at different sizes.
 */

pub(crate) unsafe fn pdf_dev_locate_font(font_name: &str, ptsize: spt_t) -> i32 {
    /* found a dev_font that matches the request */
    if ptsize == 0i32 {
        panic!("pdf_dev_locate_font() called with the zero ptsize.");
    }
    let mut i = 0;
    while i < dev_fonts.len() {
        if font_name == dev_fonts[i].tex_name {
            if ptsize == dev_fonts[i].sptsize {
                return i as i32;
            }
            if dev_fonts[i].format != 2 {
                break;
            }
            /* new dev_font will share pdf resource with /i/ */
        }
        i += 1;
    }

    /* New font */
    let mrec = fontmap.get_mut(font_name);
    if verbose > 1i32 {
        if let Some(ref mrec) = mrec {
            print_fontmap(font_name, mrec);
        }
    }
    let font_id = pdf_font_findresource(font_name, ptsize as f64 * dev_unit.dvi2pts, mrec);
    let mrec = fontmap.get(font_name);
    if font_id < 0 {
        return -1;
    }

    let mut font = dev_font {
        short_name: Vec::new(),
        used_on_this_page: 0,
        tex_name: font_name.to_string(),
        sptsize: ptsize,
        font_id,
        enc_id: pdf_get_font_encoding(font_id),
        real_font_index: if i < dev_fonts.len() { i as i32 } else { -1 },
        resource: ptr::null_mut(),
        used_chars: ptr::null_mut(),
        format: match pdf_get_font_subtype(font_id) {
            2 => 2,
            4 => 3,
            _ => 1,
        },
        wmode: pdf_get_font_wmode(font_id),
        extend: if let Some(mrec) = mrec {
            mrec.opt.extend
        } else {
            1.
        },
        slant: if let Some(mrec) = mrec {
            mrec.opt.slant
        } else {
            0.
        },
        bold: if let Some(mrec) = mrec {
            mrec.opt.bold
        } else {
            0.
        },
        mapc: match mrec {
            Some(mrec) if mrec.opt.mapc >= 0 => mrec.opt.mapc >> 8 & 0xff,
            _ => -1,
        },
        ucs_group: match mrec {
            Some(mrec) if mrec.enc_name == "unicode" && mrec.opt.mapc >= 0 => {
                mrec.opt.mapc >> 24 & 0xff
            }
            _ => 0,
        },
        ucs_plane: match mrec {
            Some(mrec) if mrec.enc_name == "unicode" && mrec.opt.mapc >= 0 => {
                mrec.opt.mapc >> 16 & 0xff
            }
            _ => 0,
        },
        is_unicode: match mrec {
            Some(mrec) if mrec.enc_name == "unicode" => 1,
            _ => 0,
        },
        cff_charsets: 0 as *mut cff_charsets,
    };

    if let Some(mrec) = mrec {
        font.cff_charsets = mrec.opt.cff_charsets as *mut cff_charsets
    }
    /* We found device font here. */
    if i < dev_fonts.len() {
        font.short_name = dev_fonts[i as usize].short_name.clone();
    /* Don't ref obj until font is actually used. */
    } else {
        font.short_name.push(b'F');
        itoa::write(&mut font.short_name, num_phys_fonts + 1).unwrap();
        num_phys_fonts += 1;
    }

    dev_fonts.push(font);

    (dev_fonts.len() - 1) as i32
}
/* This does not remember current stroking width. */
unsafe fn dev_sprint_line(
    buf: &mut Vec<u8>,
    width: spt_t,
    p0_x: spt_t,
    p0_y: spt_t,
    p1_x: spt_t,
    p1_y: spt_t,
) {
    let w = width as f64 * dev_unit.dvi2pts;
    p_dtoa(
        w,
        if dev_unit.precision + 1i32 < 8i32 {
            dev_unit.precision + 1i32
        } else {
            8i32
        },
        buf,
    );
    buf.push(b' ');
    buf.push(b'w');
    buf.push(b' ');
    dev_sprint_bp(buf, p0_x, ptr::null_mut());
    buf.push(b' ');
    dev_sprint_bp(buf, p0_y, ptr::null_mut());
    buf.push(b' ');
    buf.push(b'm');
    buf.push(b' ');
    dev_sprint_bp(buf, p1_x, ptr::null_mut());
    buf.push(b' ');
    dev_sprint_bp(buf, p1_y, ptr::null_mut());
    buf.push(b' ');
    buf.push(b'l');
    buf.push(b' ');
    buf.push(b'S');
}

pub(crate) unsafe fn pdf_dev_set_rule(
    mut xpos: spt_t,
    mut ypos: spt_t,
    width: spt_t,
    height: spt_t,
) {
    if let Some(last) = dev_coords.last() {
        xpos -= (last.x / dev_unit.dvi2pts).round() as spt_t;
        ypos -= (last.y / dev_unit.dvi2pts).round() as spt_t
    }
    graphics_mode();
    let mut buf = vec![b' ', b'q', b' '];
    /* Don't use too thick line. */
    let width_in_bp = (if width < height { width } else { height }) as f64 * dev_unit.dvi2pts;
    if width_in_bp < 0.0f64 || width_in_bp > 5.0f64 {
        let mut rect = Rect::zero();
        rect.min.x = dev_unit.dvi2pts * xpos as f64;
        rect.min.y = dev_unit.dvi2pts * ypos as f64;
        rect.max.x = dev_unit.dvi2pts * width as f64;
        rect.max.y = dev_unit.dvi2pts * height as f64;
        pdf_sprint_rect(&mut buf, &rect);
        buf.push(b' ');
        buf.push(b'r');
        buf.push(b'e');
        buf.push(b' ');
        buf.push(b'f');
    } else if width > height {
        /* NOTE:
         *  A line width of 0 denotes the thinnest line that can be rendered at
         *  device resolution. See, PDF Reference Manual 4th ed., sec. 4.3.2,
         *  "Details of Graphics State Parameters", p. 185.
         */
        if height < dev_unit.min_bp_val {
            warn!("Too thin line: height={} ({} bp)", height, width_in_bp);
            warn!("Please consider using \"-d\" option.");
        }
        dev_sprint_line(
            &mut buf,
            height,
            xpos,
            ypos + height / 2i32,
            xpos + width,
            ypos + height / 2i32,
        );
    } else {
        if width < dev_unit.min_bp_val {
            warn!("Too thin line: width={} ({} bp)", width, width_in_bp);
            warn!("Please consider using \"-d\" option.");
        }
        dev_sprint_line(
            &mut buf,
            width,
            xpos + width / 2i32,
            ypos,
            xpos + width / 2i32,
            ypos + height,
        );
    }
    buf.push(b' ');
    buf.push(b'Q');
    pdf_doc_add_page_content(&buf);
    /* op: q re f Q */
}
/* Rectangle in device space coordinate. */

pub(crate) unsafe fn pdf_dev_set_rect(
    rect: &mut Rect,
    x_user: spt_t,
    y_user: spt_t,
    width: spt_t,
    height: spt_t,
    depth: spt_t,
) {
    let mut p0 = Point::zero();
    let mut p1 = Point::zero();
    let mut p2 = Point::zero();
    let mut p3 = Point::zero();
    let dev_x = x_user as f64 * dev_unit.dvi2pts; /* currentmatrix */
    let dev_y = y_user as f64 * dev_unit.dvi2pts; /* 0 for B&W */
    if text_state.dir_mode != 0 {
        p0.x = dev_x - dev_unit.dvi2pts * depth as f64;
        p0.y = dev_y - dev_unit.dvi2pts * width as f64;
        p1.x = dev_x + dev_unit.dvi2pts * height as f64;
        p1.y = p0.y;
        p2.x = p1.x;
        p2.y = dev_y;
        p3.x = p0.x;
        p3.y = p2.y
    } else {
        p0.x = dev_x;
        p0.y = dev_y - dev_unit.dvi2pts * depth as f64;
        p1.x = dev_x + dev_unit.dvi2pts * width as f64;
        p1.y = p0.y;
        p2.x = p1.x;
        p2.y = dev_y + dev_unit.dvi2pts * height as f64;
        p3.x = p0.x;
        p3.y = p2.y
    }
    pdf_dev_transform(&mut p0, None);
    pdf_dev_transform(&mut p1, None);
    pdf_dev_transform(&mut p2, None);
    pdf_dev_transform(&mut p3, None);
    let min_x = p0.x.min(p1.x).min(p2.x).min(p3.x);
    let max_x = p0.x.max(p1.x).max(p2.x).max(p3.x);
    let min_y = p0.y.min(p1.y).min(p2.y).min(p3.y);
    let max_y = p0.y.max(p1.y).max(p2.y).max(p3.y);
    *rect = Rect::new(point2(min_x, min_y), point2(max_x, max_y));
}

pub(crate) unsafe fn pdf_dev_get_dirmode() -> i32 {
    text_state.dir_mode
}

pub(crate) unsafe fn pdf_dev_set_dirmode(text_dir: i32) {
    let font = if text_state.font_id < 0i32 {
        None
    } else {
        Some(&dev_fonts[text_state.font_id as usize])
    };
    let vert_font = match &font {
        Some(f) if f.wmode != 0 => 1,
        _ => 0,
    };
    let vert_dir = if dev_param.autorotate != 0 {
        text_dir
    } else {
        vert_font
    };
    let text_rotate = TextWMode::from(vert_font << 2 | vert_dir);
    if font.is_some() && ANGLE_CHANGES(text_rotate, text_state.matrix.rotate) {
        text_state.force_reset = 1i32
    }
    text_state.matrix.rotate = text_rotate;
    text_state.dir_mode = text_dir;
}
unsafe fn dev_set_param_autorotate(auto_rotate: i32) {
    let font = if text_state.font_id < 0i32 {
        None
    } else {
        Some(&dev_fonts[text_state.font_id as usize])
    };
    let vert_font = match &font {
        Some(f) if f.wmode != 0 => 1,
        _ => 0,
    };
    let vert_dir = if auto_rotate != 0 {
        text_state.dir_mode
    } else {
        vert_font
    };
    let text_rotate = TextWMode::from(vert_font << 2 | vert_dir);
    if ANGLE_CHANGES(text_rotate, text_state.matrix.rotate) {
        text_state.force_reset = 1i32
    }
    text_state.matrix.rotate = text_rotate;
    dev_param.autorotate = auto_rotate;
}

pub(crate) unsafe fn pdf_dev_get_param(param_type: i32) -> i32 {
    match param_type {
        1 => dev_param.autorotate,
        2 => dev_param.colormode,
        _ => {
            panic!("Unknown device parameter: {}", param_type);
        }
    }
}

pub(crate) unsafe fn pdf_dev_set_param(param_type: i32, value: i32) {
    match param_type {
        1 => {
            dev_set_param_autorotate(value);
        }
        2 => dev_param.colormode = value,
        _ => {
            panic!("Unknown device parameter: {}", param_type);
        }
    };
}

pub(crate) unsafe fn pdf_dev_put_image(
    id: i32,
    p: &mut transform_info,
    mut ref_x: f64,
    mut ref_y: f64,
) -> i32 {
    let mut r = Rect::zero();
    if let Some(last) = dev_coords.last() {
        ref_x -= last.x;
        ref_y -= last.y;
    }
    let mut M = p.matrix;
    M.m31 += ref_x;
    M.m32 += ref_y;
    /* Just rotate by -90, but not tested yet. Any problem if M has scaling? */
    if dev_param.autorotate != 0 && text_state.dir_mode != 0 {
        let tmp = -M.m11;
        M.m11 = M.m12;
        M.m12 = tmp;
        let tmp = -M.m21;
        M.m21 = M.m22;
        M.m22 = tmp
    }
    graphics_mode();
    pdf_dev_gsave();
    let M1 = pdf_ximage_scale_image(id, &mut r, p);
    M = M1.post_transform(&M);
    pdf_dev_concat(&mut M);
    /* Clip */
    if p.flags & 1i32 << 3i32 != 0 {
        pdf_dev_rectclip(&r); /* op: Do */
    }
    let res_name = CStr::from_ptr(pdf_ximage_get_resname(id));
    let content = format!(" /{} Do", res_name.display());
    pdf_doc_add_page_content(content.as_bytes());
    pdf_dev_grestore();
    pdf_doc_add_page_resource("XObject", res_name.to_bytes(), pdf_ximage_get_reference(id));
    if dvi_is_tracking_boxes() {
        let mut rect = Rect::zero();
        let mut corner: [Point; 4] = [Point::zero(); 4];
        pdf_dev_set_rect(
            &mut rect,
            (65536. * ref_x) as spt_t,
            (65536. * ref_y) as spt_t,
            (65536. * r.size().width) as spt_t,
            (65536. * r.size().height) as spt_t,
            0i32,
        );
        corner[0] = rect.lower_left();
        corner[1] = rect.upper_left();
        corner[2] = rect.upper_right();
        corner[3] = rect.lower_right();
        let P = p.matrix;
        for c in corner.iter_mut() {
            *c -= rect.min.to_vector();
            pdf_dev_transform(c, Some(&P));
            *c += rect.min.to_vector();
        }
        rect.min = corner[0];
        rect.max = corner[0];
        for c in corner.iter() {
            if c.x < rect.min.x {
                rect.min.x = c.x
            }
            if c.x > rect.max.x {
                rect.max.x = c.x
            }
            if c.y < rect.min.y {
                rect.min.y = c.y
            }
            if c.y > rect.max.y {
                rect.max.y = c.y
            }
        }
        pdf_doc_expand_box(&mut rect);
    }
    0i32
}

pub(crate) unsafe fn transform_info_clear(info: &mut transform_info) {
    /* Physical dimensions */
    info.width = 0.;
    info.height = 0.;
    info.depth = 0.;
    info.bbox = Rect::zero();
    /* Transformation matrix */
    info.matrix = TMatrix::identity();
    info.flags = 0;
}

pub(crate) unsafe fn pdf_dev_begin_actualtext(mut unicodes: *mut u16, mut count: i32) {
    let mut pdf_doc_enc = 1_usize;
    /* check whether we can use PDFDocEncoding for this string
    (we punt on the 0x80..0xA0 range that does not directly correspond to unicode)  */
    /* if using PDFDocEncoding, we only care about the low 8 bits,
    so start with the second byte of our pair */
    for i in 0..count {
        if *unicodes.offset(i as isize) as i32 > 0xffi32
            || *unicodes.offset(i as isize) as i32 > 0x7fi32
                && (*unicodes.offset(i as isize) as i32) < 0xa1i32
        {
            pdf_doc_enc = 0;
            break;
        }
    }
    graphics_mode();
    let mut content = Vec::from(b"\n/Span<</ActualText(".as_ref());
    if pdf_doc_enc == 0 {
        content.extend(b"\xfe\xff");
    }
    pdf_doc_add_page_content(&content);
    loop {
        if !(count > 0i32) {
            break;
        }
        count -= 1;
        let s: [u8; 2] = (*unicodes).to_be_bytes();
        let mut content = String::new();
        for i in pdf_doc_enc..2 {
            let c: u8 = s[i];
            if c == b'(' || c == b')' || c == b'\\' {
                content += &format!("\\{}", char::from(c));
            } else if (c as i32) < ' ' as i32 {
                content += &format!("\\{:03o}", c);
            } else {
                content += &format!("{}", char::from(c));
            }
        }
        pdf_doc_add_page_content(content.as_bytes());
        unicodes = unicodes.offset(1)
    }
    pdf_doc_add_page_content(b")>>BDC");
}
/* Not in spt_t. */
/* unit_conv: multiplier for input unit (spt_t) to bp conversion.
 * precision: How many fractional digits preserved in output (not real
 *            accuracy control).
 * is_bw:     Ignore color related special instructions.
 */
/* returns 1.0/unit_conv */
/* Draw texts and rules:
 *
 * xpos, ypos, width, and height are all fixed-point numbers
 * converted to big-points by multiplying unit_conv (dvi2pts).
 * They must be position in the user space.
 *
 * ctype:
 *   0 - input string is in multi-byte encoding.
 *   1 - input string is in 8-bit encoding.
 *   2 - input string is in 16-bit encoding.
 */
/* Place XObject */
/* The design_size and ptsize required by PK font support...
 */
/* The following two routines are NOT WORKING.
 * Dvipdfmx doesn't manage gstate well..
 */
/* Always returns 1.0, please rename this. */
/* Access text state parameters. */
/* ps: special support want this (pTeX). */
/* Text composition (direction) mode
 * This affects only when auto_rotate is enabled.
 */
/* Set rect to rectangle in device space.
 * Unit conversion spt_t to bp and transformation applied within it.
 */
/* Accessor to various device parameters.
 */
/* Text composition mode is ignored (always same as font's
 * writing mode) and glyph rotation is not enabled if
 * auto_rotate is unset.
 */
/*
 * For pdf_doc, pdf_draw and others.
 */
/* Force reselecting font and color:
 * XFrom (content grabbing) and Metapost support want them.
 */
/* Initialization of transformation matrix with M and others.
 * They are called within pdf_doc_begin_page() and pdf_doc_end_page().
 */
/* Text is normal and line art is not normal in dvipdfmx. So we don't have
 * begin_text (BT in PDF) and end_text (ET), but instead we have graphics_mode()
 * to terminate text section. pdf_dev_flushpath() and others call this.
 */

pub(crate) unsafe fn pdf_dev_end_actualtext() {
    graphics_mode();
    pdf_doc_add_page_content(b" EMC");
}
/* The name transform_info is misleading.
 * I'll put this here for a moment...
 */
/* Physical dimensions
 *
 * If those values are given, images will be scaled
 * and/or shifted to fit within a box described by
 * those values.
 */
/* transform matrix */
/* user_bbox */

pub(crate) unsafe fn pdf_dev_reset_global_state() {
    dev_fonts = Vec::new();
    num_phys_fonts = 0i32;
}
