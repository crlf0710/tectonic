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

use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_cff::cff_charsets_lookup_cid;
use super::dpx_cmap::{CMap_cache_get, CMap_decode};
use super::dpx_dvi::dvi_is_tracking_boxes;
use super::dpx_fontmap::pdf_lookup_fontmap_record;
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer_u8 as work_buffer;
use super::dpx_pdfcolor::{pdf_color_clear_stack, pdf_color_get_current};
use super::dpx_pdfdoc::pdf_doc_expand_box;
use super::dpx_pdfdoc::{pdf_doc_add_page_content, pdf_doc_add_page_resource};
use super::dpx_pdfdraw::{
    pdf_dev_clear_gstates, pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to,
    pdf_dev_gsave, pdf_dev_init_gstates,
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
use crate::shims::sprintf;
use crate::streq_ptr;
use libc::{free, strcpy};

pub type size_t = u64;

#[derive(Clone, Copy, PartialEq)]
pub enum MotionState {
    GRAPHICS_MODE = 1,
    TEXT_MODE = 2,
    STRING_MODE = 3,
}

#[derive(Clone, Copy, PartialEq)]
pub enum TextWMode {
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

pub type spt_t = i32;

pub type TMatrix = euclid::Transform2D<f64, (), ()>;

/*#[derive(Copy, Clone, Default)]
#[repr(C)]
/// Transform coordinate matrix
pub struct TMatrix {
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub d: f64,
    pub e: f64,
    pub f: f64,
}
impl TMatrix {
    /// Zero initialized transform matrix
    pub const fn new() -> Self {
        Self {
            a: 0.,
            b: 0.,
            c: 0.,
            d: 0.,
            e: 0.,
            f: 0.,
        }
    }
    /// Identity transform matrix
    pub const fn identity() -> Self {
        Self {
            a: 1.,
            b: 0.,
            c: 0.,
            d: 1.,
            e: 0.,
            f: 0.,
        }
    }
}*/
#[derive(Copy, Clone, Default)]
#[repr(C)]
/// Represents rectangle and TeX bbox in document
pub struct Rect {
    /// Lower left coorditate of rectangle
    pub ll: Coord,
    /// Upper right coorditate of rectangle
    pub ur: Coord,
}
impl Rect {
    /// Zero initialized rectangle
    pub const fn zero() -> Self {
        Self {
            ll: Coord::new(0., 0.),
            ur: Coord::new(0., 0.),
        }
    }
    /// Create new rectangle from lower left and upper right coorditate
    pub const fn new(ll: (f64, f64), ur: (f64, f64)) -> Self {
        Self {
            ll: Coord::new(ll.0, ll.1),
            ur: Coord::new(ur.0, ur.1),
        }
    }
    pub fn width(&self) -> f64 {
        self.ur.x - self.ll.x
    }
    pub fn height(&self) -> f64 {
        self.ur.y - self.ll.y
    }
    pub fn lower_left(&self) -> Coord {
        self.ll
    }
    pub fn upper_right(&self) -> Coord {
        self.ur
    }
    pub fn lower_right(&self) -> Coord {
        Coord::new(self.ur.x, self.ll.y)
    }
    pub fn upper_left(&self) -> Coord {
        Coord::new(self.ll.x, self.ur.y)
    }
}
impl From<(Coord, Coord)> for Rect {
    fn from(c: (Coord, Coord)) -> Self {
        Self {
            ll: c.0,
            ur: c.1,
        }
    }
}

impl std::fmt::Display for Rect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}, {}, {}]", self.ll.x, self.ll.y, self.ur.x, self.ur.y)
    }
}

/// Coordinate (point) in TeX document
pub type Coord = euclid::Point2D<f64, ()>;

pub trait Equal {
    fn equal(&self, other: &Self) -> bool;
}

impl Equal for Coord {
    fn equal(&self, other: &Self) -> bool {
        ((self.x - other.x).abs() < 1e-7) && ((self.y - other.y).abs() < 1e-7)
    }
}

#[derive(Copy, Clone, Default)]
#[repr(C)]
pub struct transform_info {
    pub width: f64,
    pub height: f64,
    pub depth: f64,
    pub matrix: TMatrix,
    pub bbox: Rect,
    pub flags: i32,
}
impl transform_info {
    pub const fn new() -> Self {
        Self {
            width: 0.,
            height: 0.,
            depth: 0.,
            matrix: TMatrix {
                m11: 1., m12: 0.,
                m21: 0., m22: 1.,
                m31: 0., m32: 0.,
                _unit: core::marker::PhantomData,
            },
            bbox: Rect::zero(),
            flags: 0,
        }
    }
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct dev_font {
    pub short_name: [i8; 7],
    pub used_on_this_page: i32,
    pub tex_name: *mut i8,
    pub sptsize: spt_t,
    pub font_id: i32,
    pub enc_id: i32,
    pub real_font_index: i32,
    pub resource: *mut pdf_obj,
    pub used_chars: *mut i8,
    pub format: i32,
    pub wmode: i32,
    pub extend: f64,
    pub slant: f64,
    pub bold: f64,
    pub mapc: i32,
    pub ucs_group: i32,
    pub ucs_plane: i32,
    pub is_unicode: i32,
    pub cff_charsets: *mut cff_charsets,
}
use super::dpx_cff::cff_charsets;
/*
 * Unit conversion, formatting and others.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct DevUnit {
    pub dvi2pts: f64,
    pub min_bp_val: i32,
    pub precision: i32,
    /* Number of decimal digits (in fractional part) kept. */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct TextState {
    pub font_id: i32,
    pub offset: spt_t,
    pub ref_x: spt_t,
    pub ref_y: spt_t,
    pub raise: spt_t,
    pub leading: spt_t,
    pub matrix: FontMatrix,
    pub bold_param: f64,
    pub dir_mode: i32,
    pub force_reset: i32,
    pub is_mb: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FontMatrix {
    pub slant: f64,
    pub extend: f64,
    pub rotate: TextWMode,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct DevParam {
    pub autorotate: i32,
    pub colormode: i32,
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

pub unsafe fn pdf_dev_set_verbose(mut level: i32) {
    verbose = level;
}
/* Not working yet... */

pub unsafe fn pdf_dev_scale() -> f64 {
    1.0f64
}
static mut dev_unit: DevUnit = DevUnit {
        dvi2pts: 0.0f64,
        min_bp_val: 658i32,
        precision: 2i32,
    };

pub unsafe fn dev_unit_dviunit() -> f64 {
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
unsafe fn p_itoa(mut value: i32, mut buf: *mut i8) -> u32 {
    let mut p: *mut i8 = buf;
    let mut sign = if value < 0i32 {
        let fresh0 = p;
        p = p.offset(1);
        *fresh0 = '-' as i32 as i8;
        value = -value;
        1
    } else {
        0
    };
    let mut ndigits = 0_u32;
    loop
    /* Generate at least one digit in reverse order */
    {
        let fresh1 = ndigits;
        ndigits = ndigits.wrapping_add(1);
        *p.offset(fresh1 as isize) = (value % 10i32 + '0' as i32) as i8;
        value /= 10i32;
        if !(value != 0i32) {
            break;
        }
    }
    /* Reverse the digits */
    for i in 0..ndigits.wrapping_div(2_u32) {
        let mut tmp: i8 = *p.offset(i as isize);
        *p.offset(i as isize) = *p.offset(ndigits.wrapping_sub(i).wrapping_sub(1_u32) as isize);
        *p.offset(ndigits.wrapping_sub(i).wrapping_sub(1_u32) as isize) = tmp;
    }
    *p.offset(ndigits as isize) = '\u{0}' as i32 as i8;
    return if sign != 0 {
        ndigits.wrapping_add(1_u32)
    } else {
        ndigits
    };
}
/* NOTE: Acrobat 5 and prior uses 16.16 fixed point representation for
 * real numbers.
 */
fn p_dtoa(mut value: f64, mut prec: i32, buf: &mut [u8]) -> usize {
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
        buf[n..n+m].copy_from_slice(&fv.as_bytes());
        n += m
    } else if g == 0i32 {
        buf[0] = b'0';
        n = 1;
    }
    if g != 0 {
        let mut j: i32 = prec;
        buf[n] = b'.';
        loop {
            let fresh4 = j;
            j = j - 1;
            if !(fresh4 != 0) {
                break;
            }
            buf[n+1+j as usize] = (g % 10) as u8 + b'0';
            g /= 10
        }
        n += 1 + prec as usize;
        while buf[n-1] == b'0' {
            n -= 1
        }
    }
    buf[n] = 0;
    n as usize
}
unsafe fn dev_sprint_bp(buf: &mut [u8], mut value: spt_t, mut error: *mut spt_t) -> usize {
    let mut prec: i32 = dev_unit.precision;
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
pub fn pdf_sprint_matrix(buf: &mut [u8], M: &TMatrix) -> usize {
    let precision = unsafe { dev_unit.precision };
    let mut prec2: i32 = if precision + 2 < 8i32 {
        precision + 2
    } else {
        8i32
    }; /* xxx_sprint_xxx NULL terminates strings. */
    let mut prec0: i32 = if precision > 2i32 {
        precision
    } else {
        2i32
    }; /* xxx_sprint_xxx NULL terminates strings. */
    let mut len = p_dtoa(M.m11, prec2, buf); /* xxx_sprint_xxx NULL terminates strings. */
    buf[len] = b' '; /* xxx_sprint_xxx NULL terminates strings. */
    len += 1;
    len += p_dtoa(M.m12, prec2, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(M.m21, prec2, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(M.m22, prec2, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(M.m31, prec0, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(M.m32, prec0, &mut buf[len..]);
    buf[len] = b'\x00';
    len
}
pub fn pdf_sprint_rect(buf: &mut [u8], rect: &Rect) -> usize {
    let precision = unsafe { dev_unit.precision };
    let mut len = p_dtoa(rect.ll.x, precision, buf);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(rect.ll.y, precision, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(rect.ur.x, precision, &mut buf[len..]);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(rect.ur.y, precision, &mut buf[len..]);
    buf[len] = 0;
    len
}
pub fn pdf_sprint_coord(buf: &mut [u8], p: &Coord) -> usize {
    let precision = unsafe { dev_unit.precision };
    let mut len = p_dtoa(p.x, precision, buf);
    buf[len] = b' ';
    len += 1;
    len += p_dtoa(p.y, precision, &mut buf[len..]);
    buf[len] = 0;
    len
}
pub fn pdf_sprint_length(buf: &mut [u8], value: f64) -> usize {
    let len = p_dtoa(value, unsafe{ dev_unit.precision }, buf);
    buf[len] = 0;
    len
}
pub fn pdf_sprint_number(buf: &mut [u8], mut value: f64) -> usize {
    let len = p_dtoa(value, 8, buf);
    buf[len] = 0;
    len
}
static mut dev_param: DevParam = DevParam {
        autorotate: 1i32,
        colormode: 1i32,
    };
static mut motion_state: MotionState = MotionState::GRAPHICS_MODE;
static mut format_buffer: [u8; 4096] = [0; 4096];
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
static mut dev_fonts: *mut dev_font = std::ptr::null_mut();
static mut num_dev_fonts: i32 = 0i32;
static mut max_dev_fonts: i32 = 0i32;
static mut num_phys_fonts: i32 = 0i32;
unsafe fn dev_set_text_matrix(
    mut xpos: spt_t,
    mut ypos: spt_t,
    mut slant: f64,
    mut extend: f64,
    mut rotate: TextWMode,
) {
    let mut len = 0;
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
    format_buffer[len] = b' ';
    len += 1;
    len += pdf_sprint_matrix(&mut format_buffer[len..], &mut tm) as usize;
    format_buffer[len] = b' ';
    len += 1;
    format_buffer[len] = b'T';
    len += 1;
    format_buffer[len] = b'm';
    len += 1;
    pdf_doc_add_page_content(&format_buffer[..len]);
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

pub unsafe fn graphics_mode() {
    match motion_state {
        MotionState::GRAPHICS_MODE => {}
        MotionState::STRING_MODE|MotionState::TEXT_MODE => {
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
unsafe fn start_string(
    mut xpos: spt_t,
    mut ypos: spt_t,
    mut slant: f64,
    mut extend: f64,
    mut rotate: TextWMode,
) {
    let mut error_delx: spt_t = 0i32;
    let mut error_dely: spt_t = 0i32;
    let mut len = 0_usize;
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
            let fresh18 = len;
            len = len + 1;
            format_buffer[fresh18 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_dely);
            let fresh19 = len;
            len = len + 1;
            format_buffer[fresh19 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_delx);
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
            let fresh20 = len;
            len = len + 1;
            format_buffer[fresh20 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_dely);
            let fresh21 = len;
            len = len + 1;
            format_buffer[fresh21 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_delx);
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
            let fresh22 = len;
            len = len + 1;
            format_buffer[fresh22 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_delx);
            let fresh23 = len;
            len = len + 1;
            format_buffer[fresh23 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_dely)
        }
        TextWMode::VV => {
            /* Vertical font in vertical mode:
             *                         | 1  s/e|
             * d_user = d x I_vv = d x |       |
             *                         | 0  1/e|
             */
            let desired_delx = delx;
            let desired_dely = ((dely as f64 + delx as f64 * slant) / extend) as spt_t;
            let fresh24 = len;
            len = len + 1;
            format_buffer[fresh24 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_delx);
            let fresh25 = len;
            len = len + 1;
            format_buffer[fresh25 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_dely)
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
            let fresh26 = len;
            len = len + 1;
            format_buffer[fresh26 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_dely);
            let fresh27 = len;
            len = len + 1;
            format_buffer[fresh27 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_delx);
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
            let fresh28 = len;
            len = len + 1;
            format_buffer[fresh28 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_delx, &mut error_delx);
            let fresh29 = len;
            len = len + 1;
            format_buffer[fresh29 as usize] = b' ';
            len += dev_sprint_bp(&mut format_buffer[len..], desired_dely, &mut error_dely);
            error_delx = -error_delx;
            error_dely = -error_dely
        }
    }
    pdf_doc_add_page_content(&format_buffer[..len as usize]);
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
unsafe fn string_mode(
    mut xpos: spt_t,
    mut ypos: spt_t,
    mut slant: f64,
    mut extend: f64,
    mut rotate: TextWMode,
) {
    match motion_state {
        MotionState::GRAPHICS_MODE|MotionState::TEXT_MODE => {
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
unsafe fn dev_set_font(mut font_id: i32) -> i32 {
    /* text_mode() must come before text_state.is_mb is changed. */
    text_mode(); /* Caller should check font_id. */
    let font = &mut *dev_fonts.offset(font_id as isize) as *mut dev_font; /* space not necessary. */
    assert!(!font.is_null());
    let real_font = if (*font).real_font_index >= 0i32 {
        &mut *dev_fonts.offset((*font).real_font_index as isize) as *mut dev_font
    } else {
        font
    };
    text_state.is_mb = if (*font).format == 3i32 { 1i32 } else { 0i32 };
    let vert_font = if (*font).wmode != 0 { 1 } else { 0 };
    let vert_dir = if dev_param.autorotate != 0 {
        text_state.dir_mode
    } else {
        vert_font
    };
    let text_rotate = TextWMode::from(vert_font << 2 | vert_dir);
    if (*font).slant != text_state.matrix.slant
        || (*font).extend != text_state.matrix.extend
        || ANGLE_CHANGES(text_rotate, text_state.matrix.rotate)
    {
        text_state.force_reset = 1i32
    }
    text_state.matrix.slant = (*font).slant;
    text_state.matrix.extend = (*font).extend;
    text_state.matrix.rotate = text_rotate;
    if (*real_font).resource.is_null() {
        (*real_font).resource = pdf_get_font_reference((*real_font).font_id);
        (*real_font).used_chars = pdf_get_font_usedchars((*real_font).font_id)
    }
    if (*real_font).used_on_this_page == 0 {
        pdf_doc_add_page_resource(
            "Font",
            (*real_font).short_name.as_mut_ptr(),
            pdf_link_obj((*real_font).resource),
        );
        (*real_font).used_on_this_page = 1i32
    }
    let font_scale = (*font).sptsize as f64 * dev_unit.dvi2pts;
    let mut len = sprintf(
        format_buffer.as_mut_ptr() as *mut i8,
        b" /%s\x00" as *const u8 as *const i8,
        (*real_font).short_name.as_mut_ptr(),
    ) as usize;
    let fresh30 = len;
    len = len + 1;
    format_buffer[fresh30 as usize] = b' ';
    len += p_dtoa(
        font_scale,
        if dev_unit.precision + 1i32 < 8i32 {
            dev_unit.precision + 1i32
        } else {
            8i32
        },
        &mut format_buffer[len..],
    ) as usize;
    let fresh31 = len;
    len = len + 1;
    format_buffer[fresh31 as usize] = b' ';
    let fresh32 = len;
    len = len + 1;
    format_buffer[fresh32 as usize] = b'T';
    let fresh33 = len;
    len = len + 1;
    format_buffer[fresh33 as usize] = b'f';
    pdf_doc_add_page_content(&format_buffer[..len]);
    if (*font).bold > 0.0f64 || (*font).bold != text_state.bold_param {
        if (*font).bold <= 0.0f64 {
            len = sprintf(
                format_buffer.as_mut_ptr() as *mut i8,
                b" 0 Tr\x00" as *const u8 as *const i8,
            ) as usize
        } else {
            len = sprintf(
                format_buffer.as_mut_ptr() as *mut i8,
                b" 2 Tr %.6f w\x00" as *const u8 as *const i8,
                (*font).bold,
            ) as usize
        }
        pdf_doc_add_page_content(&format_buffer[..len]);
        /* op: Tr w */
    }
    text_state.bold_param = (*font).bold;
    text_state.font_id = font_id;
    0i32
}
/* Access text state parameters.
 */

pub unsafe fn pdf_dev_get_font_wmode(mut font_id: i32) -> i32 {
    let font = &mut *dev_fonts.offset(font_id as isize) as *mut dev_font;
    if !font.is_null() {
        return (*font).wmode;
    }
    0i32
}
static mut sbuf0: [u8; 4096] = [0; 4096];
static mut sbuf1: [u8; 4096] = [0; 4096];
unsafe fn handle_multibyte_string(
    mut font: *mut dev_font,
    mut str_ptr: *mut *const u8,
    mut str_len: *mut size_t,
    mut ctype: i32,
) -> i32 {
    let mut p = *str_ptr;
    let mut length = *str_len;
    if ctype == -1i32 && !(*font).cff_charsets.is_null() {
        /* freetype glyph indexes */
        /* Convert freetype glyph indexes to CID. */
        let mut inbuf: *const u8 = p;
        let mut outbuf: *mut u8 = sbuf0.as_mut_ptr();
        for _ in (0..length).step_by(2) {
            let fresh34 = inbuf;
            inbuf = inbuf.offset(1);
            let mut gid = ((*fresh34 as i32) << 8i32) as u32;
            let fresh35 = inbuf;
            inbuf = inbuf.offset(1);
            gid = gid.wrapping_add(*fresh35 as u32);
            gid = cff_charsets_lookup_cid((*font).cff_charsets, gid as u16) as u32;
            let fresh36 = outbuf;
            outbuf = outbuf.offset(1);
            *fresh36 = (gid >> 8i32) as u8;
            let fresh37 = outbuf;
            outbuf = outbuf.offset(1);
            *fresh37 = (gid & 0xff_u32) as u8;
        }
        p = sbuf0.as_mut_ptr();
        length = outbuf.wrapping_offset_from(sbuf0.as_mut_ptr()) as i64 as size_t
    } else if (*font).is_unicode != 0 {
        /* _FIXME_ */
        /* UCS-4 */
        if ctype == 1i32 {
            if length.wrapping_mul(4i32 as u64) >= 4096i32 as u64 {
                warn!("Too long string...");
                return -1i32;
            }
            for i in 0..length {
                sbuf1[i.wrapping_mul(4i32 as u64) as usize] = (*font).ucs_group as u8;
                sbuf1[i.wrapping_mul(4i32 as u64).wrapping_add(1i32 as u64) as usize] =
                    (*font).ucs_plane as u8;
                sbuf1[i.wrapping_mul(4i32 as u64).wrapping_add(2i32 as u64) as usize] =
                    '\u{0}' as i32 as u8;
                sbuf1[i.wrapping_mul(4i32 as u64).wrapping_add(3i32 as u64) as usize] =
                    *p.offset(i as isize);
            }
            length = (length as u64).wrapping_mul(4i32 as u64) as size_t as size_t
        } else if ctype == 2i32 {
            let mut len: size_t = 0i32 as size_t;
            if length.wrapping_mul(2i32 as u64) >= 4096i32 as u64 {
                warn!("Too long string...");
                return -1i32;
            }
            let mut i = 0i32 as size_t;
            while i < length {
                sbuf1[len as usize] = (*font).ucs_group as u8;
                if *p.offset(i as isize) as i32 & 0xf8i32 == 0xd8i32 {
                    /* Check for valid surrogate pair.  */
                    if *p.offset(i as isize) as i32 & 0xfci32 != 0xd8i32
                        || i.wrapping_add(2i32 as u64) >= length
                        || *p.offset(i.wrapping_add(2i32 as u64) as isize) as i32 & 0xfci32
                            != 0xdci32
                    {
                        warn!(
                            "Invalid surrogate p[{}]={:02X}...",
                            i,
                            *p.offset(i as isize) as i32,
                        );
                        return -1i32;
                    }
                    let c = ((*p.offset(i as isize) as i32 & 0x3i32) << 10i32
                        | (*p.offset(i.wrapping_add(1i32 as u64) as isize) as i32) << 2i32
                        | *p.offset(i.wrapping_add(2i32 as u64) as isize) as i32 & 0x3i32)
                        + 0x100i32;
                    sbuf1[len.wrapping_add(1i32 as u64) as usize] = (c >> 8i32 & 0xffi32) as u8;
                    sbuf1[len.wrapping_add(2i32 as u64) as usize] = (c & 0xffi32) as u8;
                    i = (i as u64).wrapping_add(2i32 as u64) as size_t as size_t
                } else {
                    sbuf1[len.wrapping_add(1i32 as u64) as usize] = (*font).ucs_plane as u8;
                    sbuf1[len.wrapping_add(2i32 as u64) as usize] = *p.offset(i as isize)
                }
                sbuf1[len.wrapping_add(3i32 as u64) as usize] =
                    *p.offset(i.wrapping_add(1i32 as u64) as isize);
                i = (i as u64).wrapping_add(2i32 as u64) as size_t as size_t;
                len = (len as u64).wrapping_add(4i32 as u64) as size_t as size_t
            }
            length = len
        }
        p = sbuf1.as_mut_ptr()
    } else if ctype == 1i32 && (*font).mapc >= 0i32 {
        /* Omega workaround...
         * Translate single-byte chars to double byte code space.
         */
        if length.wrapping_mul(2i32 as u64) >= 4096i32 as u64 {
            warn!("Too long string...");
            return -1i32;
        }
        for i in 0..length {
            sbuf1[i.wrapping_mul(2i32 as u64) as usize] = ((*font).mapc & 0xffi32) as u8;
            sbuf1[i.wrapping_mul(2i32 as u64).wrapping_add(1i32 as u64) as usize] =
                *p.offset(i as isize);
        }
        length = (length as u64).wrapping_mul(2i32 as u64) as size_t as size_t;
        p = sbuf1.as_mut_ptr()
    }
    /*
     * Font is double-byte font. Output is assumed to be 16-bit fixed length
     * encoding.
     * TODO: A character decomposed to multiple characters.
     */
    if ctype != -1i32 && (*font).enc_id >= 0i32 {
        let cmap = CMap_cache_get((*font).enc_id);
        let mut inbuf_0 = p;
        let mut outbuf_0 = sbuf0.as_mut_ptr();
        let mut inbytesleft = length;
        let mut outbytesleft = 4096i32 as size_t;
        CMap_decode(
            cmap,
            &mut inbuf_0,
            &mut inbytesleft,
            &mut outbuf_0,
            &mut outbytesleft,
        );
        if inbytesleft != 0i32 as u64 {
            warn!("CMap conversion failed. ({} bytes remains)", inbytesleft,);
            return -1i32;
        }
        length = (4096i32 as u64).wrapping_sub(outbytesleft);
        p = sbuf0.as_mut_ptr()
    }
    *str_ptr = p;
    *str_len = length;
    0i32
}
static mut dev_coords: *mut Coord = std::ptr::null_mut();
static mut num_dev_coords: i32 = 0i32;
static mut max_dev_coords: i32 = 0i32;

pub unsafe fn pdf_dev_get_coord() -> Coord {
    if num_dev_coords > 0i32 {
        (*dev_coords.offset((num_dev_coords - 1i32) as isize))
    } else {
        Coord::zero()
    }
}

pub unsafe fn pdf_dev_push_coord(mut xpos: f64, mut ypos: f64) {
    if num_dev_coords >= max_dev_coords {
        max_dev_coords += 4i32;
        dev_coords = renew(
            dev_coords as *mut libc::c_void,
            (max_dev_coords as u32 as u64).wrapping_mul(::std::mem::size_of::<Coord>() as u64)
                as u32,
        ) as *mut Coord
    }
    (*dev_coords.offset(num_dev_coords as isize)).x = xpos;
    (*dev_coords.offset(num_dev_coords as isize)).y = ypos;
    num_dev_coords += 1;
}

pub unsafe fn pdf_dev_pop_coord() {
    if num_dev_coords > 0i32 {
        num_dev_coords -= 1
    };
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

pub unsafe fn pdf_dev_set_string(
    mut xpos: spt_t,
    mut ypos: spt_t,
    mut instr_ptr: *const libc::c_void,
    mut instr_len: size_t,
    mut width: spt_t,
    mut font_id: i32,
    mut ctype: i32,
) {
    /* Pointer to the reencoded string. */
    let mut len: size_t = 0i32 as size_t;
    if font_id < 0i32 || font_id >= num_dev_fonts {
        panic!("Invalid font: {} ({})", font_id, num_dev_fonts);
    }
    if font_id != text_state.font_id {
        dev_set_font(font_id);
    }
    let mut font = if text_state.font_id < 0i32 {
        ptr::null_mut()
    } else {
        &mut *dev_fonts.offset(text_state.font_id as isize) as *mut dev_font
    };
    if font.is_null() {
        panic!("Currentfont not set.");
    }
    let real_font = if (*font).real_font_index >= 0i32 {
        &mut *dev_fonts.offset((*font).real_font_index as isize) as *mut dev_font
    } else {
        font
    };
    let text_xorigin = text_state.ref_x;
    let text_yorigin = text_state.ref_y;
    let mut str_ptr = instr_ptr as *const u8;
    let mut length = instr_len;
    if (*font).format == 3i32 {
        if handle_multibyte_string(font, &mut str_ptr, &mut length, ctype) < 0i32 {
            panic!("Error in converting input string...");
        }
        if !(*real_font).used_chars.is_null() {
            for i in (0..length).step_by(2) {
                let mut cid: u16 = ((*str_ptr.offset(i as isize) as i32) << 8i32
                    | *str_ptr.offset(i.wrapping_add(1i32 as u64) as isize) as i32)
                    as u16;
                let ref mut fresh38 = *(*real_font).used_chars.offset((cid as i32 / 8i32) as isize);
                *fresh38 = (*fresh38 as i32 | 1i32 << 7i32 - cid as i32 % 8i32) as i8;
            }
        }
    } else if !(*real_font).used_chars.is_null() {
        for i in 0..length {
            *(*real_font)
                .used_chars
                .offset(*str_ptr.offset(i as isize) as isize) = 1_i8;
        }
    }
    if num_dev_coords > 0i32 {
        xpos -= ((*dev_coords.offset((num_dev_coords - 1i32) as isize)).x / dev_unit.dvi2pts)
            .round() as spt_t;
        ypos -= ((*dev_coords.offset((num_dev_coords - 1i32) as isize)).y / dev_unit.dvi2pts)
            .round() as spt_t
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
        (text_xorigin + text_state.offset - xpos,
        ypos - text_yorigin)
    } else if text_state.dir_mode == 1i32 {
        /* Top-to-bottom */
        (ypos - text_yorigin + text_state.offset,
        xpos - text_xorigin)
    } else {
        /* Bottom-to-top */
        (ypos + text_yorigin + text_state.offset,
        xpos + text_xorigin)
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
    if motion_state != MotionState::STRING_MODE {
        string_mode(
            xpos,
            ypos,
            (*font).slant,
            (*font).extend,
            text_state.matrix.rotate,
        );
    } else if kern != 0i32 {
        /*
         * Same issues as earlier. Use floating point for simplicity.
         * This routine needs to be fast, so we don't call sprintf() or strcpy().
         */
        text_state.offset -=
            (kern as f64 * (*font).extend * ((*font).sptsize as f64 / 1000.0f64)) as spt_t; /* op: */
        let fresh39 = len;
        len = len.wrapping_add(1);
        format_buffer[fresh39 as usize] = (if text_state.is_mb != 0 {
            '>' as i32
        } else {
            ')' as i32
        }) as u8;
        if (*font).wmode != 0 {
            len = (len as u64).wrapping_add(p_itoa(
                -kern,
                (format_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
            ) as u64) as size_t as size_t
        } else {
            len = (len as u64).wrapping_add(p_itoa(
                kern,
                (format_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
            ) as u64) as size_t as size_t
        }
        let fresh40 = len;
        len = len.wrapping_add(1);
        format_buffer[fresh40 as usize] = (if text_state.is_mb != 0 {
            '<' as i32
        } else {
            '(' as i32
        }) as u8;
        pdf_doc_add_page_content(&format_buffer[..len as usize]);
        len = 0i32 as size_t
    }
    if text_state.is_mb != 0 {
        if (4096i32 as u64).wrapping_sub(len) < (2i32 as u64).wrapping_mul(length) {
            panic!("Buffer overflow...");
        }
        for i in 0..length {
            let mut first = *str_ptr.offset(i as isize) as i32 >> 4i32 & 0xfi32;
            let mut second = *str_ptr.offset(i as isize) as i32 & 0xfi32;
            let fresh41 = len;
            len = len.wrapping_add(1);
            format_buffer[fresh41 as usize] = (if first >= 10i32 {
                first + 'W' as i32
            } else {
                first + '0' as i32
            }) as u8;
            let fresh42 = len;
            len = len.wrapping_add(1);
            format_buffer[fresh42 as usize] = (if second >= 10i32 {
                second + 'W' as i32
            } else {
                second + '0' as i32
            }) as u8;
        }
    } else {
        len = (len as u64).wrapping_add(pdfobj_escape_str(
            (format_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
            (4096i32 as u64).wrapping_sub(len),
            str_ptr,
            length,
        )) as size_t as size_t
    }
    /* I think if you really care about speed, you should avoid memcopy here. */
    pdf_doc_add_page_content(&format_buffer[..len as usize]); /* op: */
    text_state.offset += width;
}

pub unsafe fn pdf_init_device(
    mut dvi2pts: f64,
    mut precision: i32,
    mut black_and_white: i32,
) {
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
    max_dev_fonts = 0i32;
    num_dev_fonts = max_dev_fonts;
    dev_fonts = ptr::null_mut();
    max_dev_coords = 0i32;
    num_dev_coords = max_dev_coords;
    dev_coords = ptr::null_mut();
}

pub unsafe fn pdf_close_device() {
    if !dev_fonts.is_null() {
        for i in 0..num_dev_fonts {
            free((*dev_fonts.offset(i as isize)).tex_name as *mut libc::c_void);
            pdf_release_obj((*dev_fonts.offset(i as isize)).resource);
            let ref mut fresh43 = (*dev_fonts.offset(i as isize)).tex_name;
            *fresh43 = ptr::null_mut();
            let ref mut fresh44 = (*dev_fonts.offset(i as isize)).resource;
            *fresh44 = ptr::null_mut();
            let ref mut fresh45 = (*dev_fonts.offset(i as isize)).cff_charsets;
            *fresh45 = ptr::null_mut();
        }
        free(dev_fonts as *mut libc::c_void);
    }
    free(dev_coords as *mut libc::c_void);
    pdf_dev_clear_gstates();
}
/*
 * BOP, EOP, and FONT section.
 * BOP and EOP manipulate some of the same data structures
 * as the font stuff.
 */

pub unsafe fn pdf_dev_reset_fonts(mut newpage: i32) {
    for i in 0..num_dev_fonts {
        (*dev_fonts.offset(i as isize)).used_on_this_page = 0i32;
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

pub unsafe fn pdf_dev_reset_color(mut force: i32) {
    let (sc, fc) = pdf_color_get_current();
    pdf_dev_set_color(sc, 0, force);
    pdf_dev_set_color(fc, 0x20, force);
}

pub unsafe fn pdf_dev_bop(M: &TMatrix) {
    graphics_mode();
    text_state.force_reset = 0i32;
    pdf_dev_gsave();
    pdf_dev_concat(M);
    pdf_dev_reset_fonts(1i32);
    pdf_dev_reset_color(0i32);
}

pub unsafe fn pdf_dev_eop() {
    graphics_mode();
    let depth = pdf_dev_current_depth();
    if depth != 1 {
        warn!("Unbalenced q/Q nesting...: {}", depth);
        pdf_dev_grestore_to(0);
    } else {
        pdf_dev_grestore();
    };
}
unsafe fn print_fontmap(mut font_name: *const i8, mut mrec: *mut fontmap_rec) {
    if mrec.is_null() {
        return;
    }
    info!("\n");
    info!(
        "fontmap: {} -> {}",
        CStr::from_ptr(font_name).display(),
        CStr::from_ptr((*mrec).font_name).display(),
    );
    if !(*mrec).enc_name.is_null() {
        info!("({})", CStr::from_ptr((*mrec).enc_name).display());
    }
    if (*mrec).opt.extend != 1.0f64 {
        info!("[extend:{}]", (*mrec).opt.extend);
    }
    if (*mrec).opt.slant != 0.0f64 {
        info!("[slant:{}]", (*mrec).opt.slant);
    }
    if (*mrec).opt.bold != 0.0f64 {
        info!("[bold:{}]", (*mrec).opt.bold);
    }
    if (*mrec).opt.flags & 1i32 << 1i32 != 0 {
        info!("[noemb]");
    }
    if (*mrec).opt.mapc >= 0i32 {
        info!("[map:<{:02x}>]", (*mrec).opt.mapc);
    }
    if !(*mrec).opt.charcoll.is_null() {
        info!("[csi:{}]", CStr::from_ptr((*mrec).opt.charcoll).display());
    }
    if (*mrec).opt.index != 0 {
        info!("[index:{}]", (*mrec).opt.index);
    }
    match (*mrec).opt.style {
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

pub unsafe fn pdf_dev_locate_font(font_name: &CStr, mut ptsize: spt_t) -> i32 {
    /* found a dev_font that matches the request */
    if ptsize == 0i32 {
        panic!("pdf_dev_locate_font() called with the zero ptsize.");
    }
    let mut i = 0;
    while i < num_dev_fonts {
        if streq_ptr(font_name.as_ptr(), (*dev_fonts.offset(i as isize)).tex_name) {
            if ptsize == (*dev_fonts.offset(i as isize)).sptsize {
                return i;
            }
            if (*dev_fonts.offset(i as isize)).format != 2i32 {
                break;
            }
            /* new dev_font will share pdf resource with /i/ */
        }
        i += 1
    }
    /*
     * Make sure we have room for a new one, even though we may not
     * actually create one.
     */
    if num_dev_fonts >= max_dev_fonts {
        max_dev_fonts += 16i32;
        dev_fonts = renew(
            dev_fonts as *mut libc::c_void,
            (max_dev_fonts as u32 as u64).wrapping_mul(::std::mem::size_of::<dev_font>() as u64)
                as u32,
        ) as *mut dev_font
    }
    let font = &mut *dev_fonts.offset(num_dev_fonts as isize) as *mut dev_font;
    /* New font */
    let mrec = pdf_lookup_fontmap_record(font_name.to_bytes());
    if verbose > 1i32 {
        print_fontmap(font_name.as_ptr(), mrec);
    }
    (*font).font_id = pdf_font_findresource(font_name.as_ptr(), ptsize as f64 * dev_unit.dvi2pts, mrec);
    if (*font).font_id < 0i32 {
        return -1i32;
    }
    if !mrec.is_null() {
        (*font).cff_charsets = (*mrec).opt.cff_charsets as *mut cff_charsets
    }
    /* We found device font here. */
    if i < num_dev_fonts {
        (*font).real_font_index = i; /* NULL terminated here */
        strcpy(
            (*font).short_name.as_mut_ptr(),
            (*dev_fonts.offset(i as isize)).short_name.as_mut_ptr(),
        ); /* Don't ref obj until font is actually used. */
    } else {
        (*font).real_font_index = -1i32;
        (*font).short_name[0] = 'F' as i32 as i8;
        p_itoa(
            num_phys_fonts + 1i32,
            &mut *(*font).short_name.as_mut_ptr().offset(1),
        );
        num_phys_fonts += 1
    }
    (*font).used_on_this_page = 0i32;
    (*font).tex_name = new(font_name.to_bytes().len() as u32 + 1) as *mut i8;
    strcpy((*font).tex_name, font_name.as_ptr());
    (*font).sptsize = ptsize;
    match pdf_get_font_subtype((*font).font_id) {
        2 => (*font).format = 2i32,
        4 => (*font).format = 3i32,
        _ => (*font).format = 1i32,
    }
    (*font).wmode = pdf_get_font_wmode((*font).font_id);
    (*font).enc_id = pdf_get_font_encoding((*font).font_id);
    (*font).resource = ptr::null_mut();
    (*font).used_chars = ptr::null_mut();
    (*font).extend = 1.0f64;
    (*font).slant = 0.0f64;
    (*font).bold = 0.0f64;
    (*font).mapc = -1i32;
    (*font).is_unicode = 0i32;
    (*font).ucs_group = 0i32;
    (*font).ucs_plane = 0i32;
    if !mrec.is_null() {
        (*font).extend = (*mrec).opt.extend;
        (*font).slant = (*mrec).opt.slant;
        (*font).bold = (*mrec).opt.bold;
        if (*mrec).opt.mapc >= 0i32 {
            (*font).mapc = (*mrec).opt.mapc >> 8i32 & 0xffi32
        } else {
            (*font).mapc = -1i32
        }
        if streq_ptr((*mrec).enc_name, b"unicode\x00" as *const u8 as *const i8) {
            (*font).is_unicode = 1i32;
            if (*mrec).opt.mapc >= 0i32 {
                (*font).ucs_group = (*mrec).opt.mapc >> 24i32 & 0xffi32;
                (*font).ucs_plane = (*mrec).opt.mapc >> 16i32 & 0xffi32
            } else {
                (*font).ucs_group = 0i32;
                (*font).ucs_plane = 0i32
            }
        } else {
            (*font).is_unicode = 0i32
        }
    }
    let fresh46 = num_dev_fonts;
    num_dev_fonts = num_dev_fonts + 1;
    fresh46
}
/* This does not remember current stroking width. */
unsafe fn dev_sprint_line(
    buf: &mut [u8],
    mut width: spt_t,
    mut p0_x: spt_t,
    mut p0_y: spt_t,
    mut p1_x: spt_t,
    mut p1_y: spt_t,
) -> usize {
    let mut len = 0_usize;
    let w = width as f64 * dev_unit.dvi2pts;
    len += p_dtoa(
        w,
        if dev_unit.precision + 1i32 < 8i32 {
            dev_unit.precision + 1i32
        } else {
            8i32
        },
        &mut buf[len..],
    );
    buf[len] = b' ';
    len += 1;
    buf[len] = b'w';
    len += 1;
    buf[len] = b' ';
    len += 1;
    len += dev_sprint_bp(&mut buf[len..], p0_x, ptr::null_mut());
    buf[len] = b' ';
    len += 1;
    len += dev_sprint_bp(&mut buf[len..], p0_y, ptr::null_mut());
    buf[len] = b' ';
    len += 1;
    buf[len] = b'm';
    len += 1;
    buf[len] = b' ';
    len += 1;
    len += dev_sprint_bp(&mut buf[len..], p1_x, ptr::null_mut());
    buf[len] = b' ';
    len += 1;
    len += dev_sprint_bp(&mut buf[len..], p1_y, ptr::null_mut());
    buf[len] = b' ';
    len += 1;
    buf[len] = b'l';
    len += 1;
    buf[len] = b' ';
    len += 1;
    buf[len] = b'S';
    len += 1;
    len
}

pub unsafe fn pdf_dev_set_rule(
    mut xpos: spt_t,
    mut ypos: spt_t,
    mut width: spt_t,
    mut height: spt_t,
) {
    let mut len = 0_usize;
    if num_dev_coords > 0i32 {
        xpos -= ((*dev_coords.offset((num_dev_coords - 1i32) as isize)).x / dev_unit.dvi2pts)
            .round() as spt_t;
        ypos -= ((*dev_coords.offset((num_dev_coords - 1i32) as isize)).y / dev_unit.dvi2pts)
            .round() as spt_t
    }
    graphics_mode();
    let fresh59 = len;
    len = len + 1;
    format_buffer[fresh59 as usize] = b' ';
    let fresh60 = len;
    len = len + 1;
    format_buffer[fresh60 as usize] = b'q';
    let fresh61 = len;
    len = len + 1;
    format_buffer[fresh61 as usize] = b' ';
    /* Don't use too thick line. */
    let width_in_bp = (if width < height { width } else { height }) as f64 * dev_unit.dvi2pts;
    if width_in_bp < 0.0f64 || width_in_bp > 5.0f64 {
        let mut rect = Rect::zero();
        rect.ll.x = dev_unit.dvi2pts * xpos as f64;
        rect.ll.y = dev_unit.dvi2pts * ypos as f64;
        rect.ur.x = dev_unit.dvi2pts * width as f64;
        rect.ur.y = dev_unit.dvi2pts * height as f64;
        len += pdf_sprint_rect(&mut format_buffer[len..], &rect);
        let fresh62 = len;
        len = len + 1;
        format_buffer[fresh62 as usize] = b' ';
        let fresh63 = len;
        len = len + 1;
        format_buffer[fresh63 as usize] = b'r';
        let fresh64 = len;
        len = len + 1;
        format_buffer[fresh64 as usize] = b'e';
        let fresh65 = len;
        len = len + 1;
        format_buffer[fresh65 as usize] = b' ';
        let fresh66 = len;
        len = len + 1;
        format_buffer[fresh66 as usize] = b'f'
    } else if width > height {
        /* NOTE:
         *  A line width of 0 denotes the thinnest line that can be rendered at
         *  device resolution. See, PDF Reference Manual 4th ed., sec. 4.3.2,
         *  "Details of Graphics State Parameters", p. 185.
         */
        if height < dev_unit.min_bp_val {
            warn!("Too thin line: height={} ({} bp)", height, width_in_bp,);
            warn!("Please consider using \"-d\" option.");
        }
        len += dev_sprint_line(
            &mut format_buffer[len..],
            height,
            xpos,
            ypos + height / 2i32,
            xpos + width,
            ypos + height / 2i32,
        ) as usize
    } else {
        if width < dev_unit.min_bp_val {
            warn!("Too thin line: width={} ({} bp)", width, width_in_bp,);
            warn!("Please consider using \"-d\" option.");
        }
        len += dev_sprint_line(
            &mut format_buffer[len..],
            width,
            xpos + width / 2i32,
            ypos,
            xpos + width / 2i32,
            ypos + height,
        ) as usize
    }
    let fresh67 = len;
    len = len + 1;
    format_buffer[fresh67 as usize] = b' ';
    let fresh68 = len;
    len = len + 1;
    format_buffer[fresh68 as usize] = b'Q';
    pdf_doc_add_page_content(&format_buffer[..len]);
    /* op: q re f Q */
}
/* Rectangle in device space coordinate. */

pub unsafe fn pdf_dev_set_rect(
    rect: &mut Rect,
    mut x_user: spt_t,
    mut y_user: spt_t,
    mut width: spt_t,
    mut height: spt_t,
    mut depth: spt_t,
) {
    let mut p0 = Coord::zero();
    let mut p1 = Coord::zero();
    let mut p2 = Coord::zero();
    let mut p3 = Coord::zero();
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
    rect.ll.x = min_x;
    rect.ll.y = min_y;
    rect.ur.x = max_x;
    rect.ur.y = max_y;
}

pub unsafe fn pdf_dev_get_dirmode() -> i32 {
    text_state.dir_mode
}

pub unsafe fn pdf_dev_set_dirmode(mut text_dir: i32) {
    let font = if text_state.font_id < 0i32 {
        ptr::null_mut()
    } else {
        &mut *dev_fonts.offset(text_state.font_id as isize) as *mut dev_font
    };
    let vert_font = if !font.is_null() && (*font).wmode != 0 {
        1
    } else {
        0
    };
    let vert_dir = if dev_param.autorotate != 0 {
        text_dir
    } else {
        vert_font
    };
    let text_rotate = TextWMode::from(vert_font << 2 | vert_dir);
    if !font.is_null() && ANGLE_CHANGES(text_rotate, text_state.matrix.rotate) {
        text_state.force_reset = 1i32
    }
    text_state.matrix.rotate = text_rotate;
    text_state.dir_mode = text_dir;
}
unsafe fn dev_set_param_autorotate(mut auto_rotate: i32) {
    let font = if text_state.font_id < 0i32 {
        ptr::null_mut()
    } else {
        &mut *dev_fonts.offset(text_state.font_id as isize) as *mut dev_font
    };
    let vert_font = if !font.is_null() && (*font).wmode != 0 {
        1
    } else {
        0
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

pub unsafe fn pdf_dev_get_param(mut param_type: i32) -> i32 {
    match param_type {
        1 => dev_param.autorotate,
        2 => dev_param.colormode,
        _ => {
            panic!("Unknown device parameter: {}", param_type);
        }
    }
}

pub unsafe fn pdf_dev_set_param(mut param_type: i32, mut value: i32) {
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

pub unsafe fn pdf_dev_put_image(
    mut id: i32,
    p: &mut transform_info,
    mut ref_x: f64,
    mut ref_y: f64,
) -> i32 {
    let mut r = Rect::zero();
    if num_dev_coords > 0i32 {
        ref_x -= (*dev_coords.offset((num_dev_coords - 1i32) as isize)).x;
        ref_y -= (*dev_coords.offset((num_dev_coords - 1i32) as isize)).y
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
        r.clip(); /* op: Do */
    }
    let res_name = pdf_ximage_get_resname(id);
    let len = sprintf(
        work_buffer.as_mut_ptr() as *mut i8,
        b" /%s Do\x00" as *const u8 as *const i8,
        res_name,
    ) as usize;
    pdf_doc_add_page_content(&work_buffer[..len]);
    pdf_dev_grestore();
    pdf_doc_add_page_resource("XObject", res_name, pdf_ximage_get_reference(id));
    if dvi_is_tracking_boxes() {
        let mut rect = Rect::zero();
        let mut corner: [Coord; 4] = [Coord::zero(); 4];
        pdf_dev_set_rect(
            &mut rect,
            (65536. * ref_x) as spt_t,
            (65536. * ref_y) as spt_t,
            (65536. * r.width()) as spt_t,
            (65536. * r.height()) as spt_t,
            0i32,
        );
        corner[0] = rect.lower_left();
        corner[1] = rect.upper_left();
        corner[2] = rect.upper_right();
        corner[3] = rect.lower_right();
        let P = p.matrix;
        for c in corner.iter_mut() {
            *c -= rect.ll.to_vector();
            pdf_dev_transform(c, Some(&P));
            *c += rect.ll.to_vector();
        }
        rect.ll = corner[0];
        rect.ur = corner[0];
        for c in corner.iter() {
            if c.x < rect.ll.x {
                rect.ll.x = c.x
            }
            if c.x > rect.ur.x {
                rect.ur.x = c.x
            }
            if c.y < rect.ll.y {
                rect.ll.y = c.y
            }
            if c.y > rect.ur.y {
                rect.ur.y = c.y
            }
        }
        pdf_doc_expand_box(&mut rect);
    }
    0i32
}

pub unsafe fn transform_info_clear(info: &mut transform_info) {
    /* Physical dimensions */
    info.width = 0.;
    info.height = 0.;
    info.depth = 0.;
    info.bbox = Rect::zero();
    /* Transformation matrix */
    info.matrix = TMatrix::identity();
    info.flags = 0;
}

pub unsafe fn pdf_dev_begin_actualtext(mut unicodes: *mut u16, mut count: i32) {
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
    let mut len = sprintf(
        work_buffer.as_mut_ptr() as *mut i8,
        b"\n/Span<</ActualText(\x00" as *const u8 as *const i8,
    );
    if pdf_doc_enc == 0 {
        len += sprintf(
            (work_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
            b"\xfe\xff\x00" as *const u8 as *const i8,
        )
    }
    pdf_doc_add_page_content(&work_buffer[..len as usize]);
    loop {
        let fresh69 = count;
        count = count - 1;
        if !(fresh69 > 0i32) {
            break;
        }
        let mut s: [u8; 2] = (*unicodes).to_be_bytes();
        len = 0i32;
        for i in pdf_doc_enc..2 {
            let mut c: u8 = s[i];
            if c as i32 == '(' as i32 || c as i32 == ')' as i32 || c as i32 == '\\' as i32 {
                len += sprintf(
                    (work_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
                    b"\\%c\x00" as *const u8 as *const i8,
                    c as i32,
                )
            } else if (c as i32) < ' ' as i32 {
                len += sprintf(
                    (work_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
                    b"\\%03o\x00" as *const u8 as *const i8,
                    c as i32,
                )
            } else {
                len += sprintf(
                    (work_buffer.as_mut_ptr() as *mut i8).offset(len as isize),
                    b"%c\x00" as *const u8 as *const i8,
                    c as i32,
                )
            }
        }
        pdf_doc_add_page_content(&work_buffer[..len as usize]);
        unicodes = unicodes.offset(1)
    }
    len = sprintf(
        work_buffer.as_mut_ptr() as *mut i8,
        b")>>BDC\x00" as *const u8 as *const i8,
    );
    pdf_doc_add_page_content(&work_buffer[..len as usize]);
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

pub unsafe fn pdf_dev_end_actualtext() {
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

pub unsafe fn pdf_dev_reset_global_state() {
    dev_fonts = ptr::null_mut();
    num_dev_fonts = 0i32;
    max_dev_fonts = 0i32;
    num_phys_fonts = 0i32;
}
