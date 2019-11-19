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

use crate::warn;

use super::dpx_pdfcolor::{PdfColor, BLACK};
use super::dpx_pdfdev::{
    graphics_mode, pdf_dev_get_param, pdf_dev_reset_fonts, pdf_sprint_coord, pdf_sprint_length,
    pdf_sprint_matrix, pdf_sprint_rect,
};
use super::dpx_pdfdoc::pdf_doc_add_page_content;

// TODO move to context structure
static mut gs_stack: Vec<pdf_gstate> = Vec::new();

use crate::shims::sprintf;

use super::dpx_pdfdev::{Coord, Rect, TMatrix, Equal};

/* Graphics State */
#[derive(Clone)]
#[repr(C)]
pub struct pdf_gstate {
    pub cp: Coord,
    pub matrix: TMatrix,
    pub strokecolor: PdfColor,
    pub fillcolor: PdfColor,
    pub linedash: LineDash,
    pub linewidth: f64,
    pub linecap: i32,
    pub linejoin: i32,
    pub miterlimit: f64,
    pub flatness: i32,
    pub path: pdf_path,
    pub flags: i32,
    pub pt_fixee: Coord,
}
#[derive(Clone)]
pub struct pdf_path {
    pub path: Vec<pa_elem>, /* cm,  - */
                            /* colorspace here */
                            /* d,  D  */
                            /* w,  LW */
                            /* J,  LC */
                            /* j,  LJ */
                            /* M,  ML */
                            /* i,  FL, 0 to 100 (0 for use device-default) */
                            /* internal */
                            /* bookkeeping the origin of the last transform applied */
                            /* _PDF_DRAW_H_ */
}

impl pdf_path {
    pub fn new() -> Self {
        Self { path: vec![] }
    }
    pub fn len(&self) -> usize {
        self.path.len()
    }
    pub fn transform(&mut self, M: &TMatrix) -> i32 {
        for pe in self.path.iter_mut() {
            if pe.typ != PeType::TERMINATE {
                for n in (0..pe.typ.n_pts()).rev() {
                    pe.p[n] = M.transform_point(pe.p[n]);
                }
            }
        }
        0
    }
    /* start new subpath */
    pub fn moveto(&mut self, cp: &mut Coord, p0: Coord) -> i32 {
        if !self.path.is_empty() {
            let len = self.len();
            let pe = &mut self.path[len - 1];
            if pe.typ == PeType::MOVETO {
                *cp = p0;
                pe.p[0] = *cp;
                return 0;
            }
        }
        *cp = p0;
        let pe = pa_elem {
            typ: PeType::MOVETO,
            p: [*cp, Coord::zero(), Coord::zero()],
        };
        self.path.push(pe);
        0
    }
    
    /* Do 'compression' of path while adding new path elements.
     * Sequantial moveto command will be replaced with a
     * single moveto. If cp is not equal to the last point in pa,
     * then moveto is inserted (starting new subpath).
     * FIXME:
     * 'moveto' must be used to enforce starting new path.
     * This affects how 'closepath' is treated.
     */
    fn next_pe(&mut self, cp: &Coord) -> &mut pa_elem {
        if self.path.is_empty() {
            let mut pe = pa_elem::default();
            pe.p[0] = *cp;
            self.path.push(pe);
            self.path.push(pa_elem::default());
            let len = self.len();
            return &mut self.path[len - 1];
        }
        let len = self.len();
        let mut pe = &mut self.path[len - 1];
        match pe.typ {
            PeType::MOVETO => {
                pe.p[0] = *cp;
            }
            PeType::LINETO => {
                if !pe.p[0].equal(cp) {
                    let mut pe = pa_elem::default();
                    pe.p[0] = *cp;
                    self.path.push(pe);
                }
            }
            PeType::CURVETO => {
                if !pe.p[2].equal(cp) {
                    let mut pe = pa_elem::default();
                    pe.p[0] = *cp;
                    self.path.push(pe);
                }
            }
            PeType::CURVETO_Y | PeType::CURVETO_V => {
                if !pe.p[1].equal(cp) {
                    let mut pe = pa_elem::default();
                    pe.p[0] = *cp;
                    self.path.push(pe);
                }
            }
            PeType::CLOSEPATH => {
                let mut pe = pa_elem::default();
                pe.p[0] = *cp;
                self.path.push(pe);
            }
            _ => {}
        }
        self.path.push(pa_elem::default());
        let len = self.len();
        return &mut self.path[len - 1];
    }
    /* Path Construction */
    pub fn lineto(&mut self, cp: &mut Coord, p0: Coord) -> i32 {
        let pe = self.next_pe(cp);
        pe.typ = PeType::LINETO;
        *cp = p0;
        pe.p[0] = p0;
        0i32
    }
    pub fn curveto(
        &mut self,
        cp: &mut Coord,
        p0: Coord,
        p1: Coord,
        p2: Coord,
    ) -> i32 {
        let pe = self.next_pe(cp);
        if cp.equal(&p0) {
            pe.typ = PeType::CURVETO_V;
            pe.p[0] = p1;
            *cp = p2;
            pe.p[1] = *cp;
        } else if p1.equal(&p2) {
            pe.typ = PeType::CURVETO_Y;
            pe.p[0] = p0;
            *cp = p1;
            pe.p[1] = *cp;
        } else {
            pe.typ = PeType::CURVETO;
            pe.p[0] = p0;
            pe.p[1] = p1;
            *cp = p2;
            pe.p[2] = *cp;
        }
        0i32
    }
    /* This isn't specified as cp to somewhere. */
    fn elliptarc(
        &mut self,
        cp: &mut Coord,
        ca: Coord,
        mut r_x: f64,
        mut r_y: f64,
        mut xar: f64,
        mut a_0: f64,
        mut a_1: f64,
        mut a_d: i32,
    ) -> i32
    /* arc orientation        */ {
        let mut error: i32 = 0i32;
        if r_x.abs() < 2.5e-16f64 || r_y.abs() < 2.5e-16f64 {
            return -1i32;
        }
        if a_d < 0i32 {
            while a_1 > a_0 {
                a_1 -= 360.0f64
            }
        } else {
            while a_1 < a_0 {
                a_0 -= 360.0f64
            }
        }
        let mut d_a = a_1 - a_0;
        let mut n_c = 1;
        while d_a.abs() > 90.0f64 * n_c as f64 {
            n_c += 1
        }
        d_a /= n_c as f64;
        if d_a.abs() < 2.5e-16f64 {
            return -1i32;
        }
        a_0 *= core::f64::consts::PI / 180.;
        //a_1 *= core::f64::consts::PI / 180.; TODO: check
        d_a *= core::f64::consts::PI / 180.;

        let T = TMatrix::create_rotation(euclid::Angle::degrees(-xar));
        /* A parameter that controls cb-curve (off-curve) points */
        let b = 4.0f64 * (1.0f64 - (0.5f64 * d_a).cos()) / (3.0f64 * (0.5f64 * d_a).sin()); /* number of segments */
        let b_x = r_x * b;
        let b_y = r_y * b;
        let (s, c) = a_0.sin_cos();
        let p0 = T.transform_point(Coord::new(r_x * c, r_y * s)) + ca.to_vector();
        if self.path.is_empty() {
            self.moveto(cp, p0);
        } else if !cp.equal(&p0) {
            self.lineto(cp, p0);
            /* add line seg */
        }
        let mut i = 0;
        while error == 0 && i < n_c {
            let q = a_0 + i as f64 * d_a;
            let (s, c) = q.sin_cos();
            let e0 = Coord::new(c, s);
            let (s, c) = (q + d_a).sin_cos();
            let e1 = Coord::new(c, s);
            /* Condition for tangent vector requirs
             *  d1 = p1 - p0 = f ( sin a, -cos a)
             *  d2 = p2 - p3 = g ( sin b, -cos b)
             * and from symmetry
             *  g^2 = f^2
             */
            /* s.p. *//* e.p. */
            let mut p0 = T.transform_point(Coord::new(r_x * e0.x, r_y * e0.y)) + ca.to_vector();
            let p3 = T.transform_point(Coord::new(r_x * e1.x, r_y * e1.y)) + ca.to_vector();
            let mut p1 = T.transform_point(Coord::new(-b_x * e0.y, b_y * e0.x));
            let mut p2 = T.transform_point(Coord::new(b_x * e1.y, -b_y * e1.x));
            p1 += p0.to_vector();
            p2 += p3.to_vector();
            error = self.curveto(&mut p0, p1, p2, p3);
            *cp = p3;
            i += 1
        }
        error
    }
}

#[derive(Copy, Clone, Default)]
#[repr(C)]
pub struct pa_elem {
    pub typ: PeType,
    pub p: [Coord; 3],
}
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub struct LineDash {
    pub num_dash: i32,
    pub pattern: [f64; 16],
    pub offset: f64,
}

fn idtransform(M: &TMatrix, vec: Coord) -> Option<Coord> {
    let W = M.inverse()?;
    Some(Coord::new(
        vec.x * W.m11 + vec.y * W.m21,
        vec.x * W.m12 + vec.y * W.m22
    ))
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PeType {
    MOVETO = 0,
    LINETO = 1,
    CURVETO = 2,
    CURVETO_V = 3,
    CURVETO_Y = 4,
    CLOSEPATH = 5,
    TERMINATE = 6,
}

impl Default for PeType {
    fn default() -> Self {
        PeType::MOVETO
    }
}

impl PeType {
    pub fn opchr(&self) -> i8 {
        use PeType::*;
        (match *self {
            MOVETO => b'm',
            LINETO => b'l',
            CURVETO => b'c',
            CURVETO_V => b'v',
            CURVETO_Y => b'y',
            CLOSEPATH => b'h',
            TERMINATE => b' ',
        }) as i8
    }
    pub fn n_pts(&self) -> usize {
        use PeType::*;
        match *self {
            MOVETO => 1,
            LINETO => 1,
            CURVETO => 3,
            CURVETO_V => 2,
            CURVETO_Y => 2,
            CLOSEPATH => 0,
            TERMINATE => 0,
        }
    }
}

static mut fmt_buf: [u8; 1024] = [0; 1024];


unsafe fn pdf_path__closepath(pa: &mut pdf_path, cp: &mut Coord) -> i32
/* no arg */ {
    /* search for start point of the last subpath */
    let pe = pa.path.iter().rev().find(|pe| pe.typ == PeType::MOVETO);

    if let Some(pe) = pe {
        *cp = pe.p[0].clone();
        /* NOTE:
         *  Manually closed path without closepath is not
         *  affected by linejoin. A path with coincidental
         *  starting and ending point is not the same as
         *  'closed' path.
         */
        let mut pe = pa_elem::default();
        pe.typ = PeType::CLOSEPATH;
        pa.path.push(pe);
        0i32
    } else {
        -1i32
    }
}
/*
 *  x y width height re
 *
 * is equivalent to
 *
 *  x y m
 *  (x + width) y l
 *  (x + width) (y + height) l
 *  x (y + height) l
 *  h
 */
/* Just for quick test */
unsafe fn pdf_path__isarect(pa: &pdf_path, mut f_ir: i32) -> i32
/* fill-rule is ignorable */ {
    if pa.len() == 5 {
        let pe0 = &pa.path[0];
        let pe1 = &pa.path[1];
        let pe2 = &pa.path[2];
        let pe3 = &pa.path[3];
        let pe4 = &pa.path[4];
        if pe0.typ == PeType::MOVETO
            && pe1.typ == PeType::LINETO
            && pe2.typ == PeType::LINETO
            && pe3.typ == PeType::LINETO
            && pe4.typ == PeType::CLOSEPATH
        {
            if pe1.p[0].y - pe0.p[0].y == 0.
                && pe2.p[0].x - pe1.p[0].x == 0.
                && pe3.p[0].y - pe2.p[0].y == 0.
            {
                if pe1.p[0].x - pe0.p[0].x == pe2.p[0].x - pe3.p[0].x {
                    return 1i32;
                }
            /* Winding number is different but ignore it here. */
            } else if f_ir != 0
                && pe1.p[0].x - pe0.p[0].x == 0.
                && pe2.p[0].y - pe1.p[0].y == 0.
                && pe3.p[0].x - pe2.p[0].x == 0.
            {
                if pe1.p[0].y - pe0.p[0].y == pe2.p[0].y - pe3.p[0].y {
                    return 1i32;
                }
            }
        }
    }
    0i32
}
/* Path Painting */
/* F is obsoleted */
unsafe fn INVERTIBLE_MATRIX(M: &TMatrix) -> i32 {
    if M.determinant().abs() < 2.5e-16f64 {
        warn!("Transformation matrix not invertible.");
        warn!("--- M = [{} {} {} {} {} {}]", M.m11, M.m12, M.m21, M.m22, M.m31, M.m32);
        return -1i32;
    }
    0i32
}
/* rectfill, rectstroke, rectclip, recteoclip
 *
 * Draw isolated rectangle without actually doing
 * gsave/grestore operation.
 *
 * TODO:
 *  linestyle, fill-opacity, stroke-opacity,....
 *  As this routine draw a single graphics object
 *  each time, there should be options for specifying
 *  various drawing styles, which might inherite
 *  current graphcs state parameter.
 */
unsafe fn pdf_dev__rectshape(r: &Rect, M: Option<&TMatrix>, opchr: u8) -> i32 {
    let buf = &mut fmt_buf;
    let mut len = 0;
    assert!(b"fFsSbBW ".contains(&(opchr as u8)));
    let isclip = if opchr == b'W' || opchr == b' ' {
        1i32
    } else {
        0i32
    };
    /* disallow matrix for clipping.
     * q ... clip Q does nothing and
     * n M cm ... clip n alter CTM.
     */
    if M.is_some() && (isclip != 0 || INVERTIBLE_MATRIX(M.unwrap()) == 0) {
        return -1i32;
    } /* op: q cm n re Q */
    graphics_mode();
    buf[len] = b' ';
    len += 1;
    if isclip == 0 {
        buf[len] = b'q';
        len += 1;
        if let Some(m) = M {
            buf[len] = b' ';
            len += 1;
            len += pdf_sprint_matrix(&mut buf[len..], m);
            buf[len] = b' ';
            len += 1;
            buf[len] = b'c';
            len += 1;
            buf[len] = b'm';
            len += 1;
        }
        buf[len] = b' ';
        len += 1;
    }
    buf[len] = b'n';
    len += 1;
    let p = r.ll;
    let wd = r.width();
    let ht = r.height();
    buf[len] = b' ';
    len += 1;
    len += pdf_sprint_coord(&mut buf[len..], &p);
    buf[len] = b' ';
    len += 1;
    len += pdf_sprint_length(&mut buf[len..], wd);
    buf[len] = b' ';
    len += 1;
    len += pdf_sprint_length(&mut buf[len..], ht);
    buf[len] = b' ';
    len += 1;
    buf[len] = b'r';
    len += 1;
    buf[len] = b'e';
    len += 1;
    if opchr != b' ' {
        buf[len] = b' ';
        len += 1;
        buf[len] = opchr;
        len += 1;
        buf[len] = b' ';
        len += 1;
        buf[len] = if isclip != 0 { b'n' } else { b'Q' };
        len += 1;
    }
    pdf_doc_add_page_content(&buf[..len]);
    0i32
}
static mut path_added: i32 = 0i32;
/* FIXME */
unsafe fn pdf_dev__flushpath(
    pa: &mut pdf_path,
    mut opchr: u8,
    mut rule: i32,
    mut ignore_rule: i32,
) -> i32 {
    let mut b = &mut fmt_buf; /* height... */
    let mut b_len = 1024; /* op: re */
    let mut r = Rect::zero(); /* op: m l c v y h */
    let mut len = 0_usize;
    assert!(b"fFsSbBW ".contains(&opchr));
    let isclip = if opchr == b'W' { true } else { false };
    if
    /*pa.num_paths <= 0_u32 &&*/
    path_added == 0i32 {
        return 0i32;
    }
    path_added = 0i32;
    graphics_mode();
    let isrect = pdf_path__isarect(pa, ignore_rule);
    if isrect != 0 {
        let pe = &pa.path[0];
        let pe1 = &pa.path[2];
        r.ll = pe.p[0];
        r.ur = pe1.p[0] - pe.p[0].to_vector();
        b[len] = b' ';
        len += 1;
        len += pdf_sprint_rect(&mut b[len..], &r);
        b[len] = b' ';
        len += 1;
        b[len] = b'r';
        len += 1;
        b[len] = b'e';
        len += 1;
        pdf_doc_add_page_content(&b[..len]);
        len = 0;
    } else {
        for pe in pa.path.iter_mut() {
            /* op: f F s S b B W f* F* s* S* b* B* W* */
            let n_pts = if pe.typ != PeType::TERMINATE {
                pe.typ.n_pts() as i32
            } else {
                0i32
            };
            for (_j, pt) in (0..n_pts).zip(pe.p.iter_mut()) {
                /* op: m l c v y h */
                b[len] = b' ';
                len += 1;
                len += pdf_sprint_coord(&mut b[len..], &mut *pt);
            }
            b[len] = b' ';
            len += 1;
            b[len] = if
            /* !pe.is_null() &&*/
            pe.typ != PeType::TERMINATE {
                pe.typ.opchr() as u8
            } else {
                b' '
            };
            len += 1;
            if len + 128 > b_len {
                pdf_doc_add_page_content(&b[..len]);
                len = 0
            }
        }
        if len > 0 {
            pdf_doc_add_page_content(&b[..len]);
            len = 0;
        }
    }
    b[len] = b' ';
    len += 1;
    b[len] = opchr;
    len += 1;
    if rule == 1i32 {
        b[len] = b'*';
        len += 1;
    }
    if isclip {
        b[len] = b' ';
        len += 1;
        b[len] = b'n';
        len += 1;
    }
    pdf_doc_add_page_content(&b[..len]);
    0i32
}

impl pdf_gstate {
    pub fn init() -> Self {
        Self {
            cp: Coord::zero(),
            matrix: TMatrix::identity(),
            strokecolor: BLACK,
            fillcolor: BLACK,
            linedash: LineDash::default(),
            linecap: 0,  // TODO make enum
            linejoin: 0, // TODO make enum
            linewidth: 1.,
            miterlimit: 10.,
            flatness: 1,
            /* Internal variables */
            flags: 0,
            path: pdf_path::new(),
            pt_fixee: Coord::zero(),
        }
    }
}

unsafe fn copy_a_gstate(gs1: &mut pdf_gstate, gs2: &pdf_gstate) {
    gs1.cp = gs2.cp;
    gs1.matrix = gs2.matrix;
    /* TODO:
     * Path should be linked list and gsave only
     * record starting point within path rather than
     * copying whole path.
     */
    gs1.path = gs2.path.clone(); /* Initial state */
    gs1.linedash.num_dash = gs2.linedash.num_dash;
    for i in 0..gs2.linedash.num_dash as usize {
        gs1.linedash.pattern[i] = gs2.linedash.pattern[i];
    }
    gs1.linedash.offset = gs2.linedash.offset;
    gs1.linecap = gs2.linecap;
    gs1.linejoin = gs2.linejoin;
    gs1.linewidth = gs2.linewidth;
    gs1.miterlimit = gs2.miterlimit;
    gs1.flatness = gs2.flatness;
    gs1.fillcolor = gs2.fillcolor.clone();
    gs1.strokecolor = gs2.strokecolor.clone();
    gs1.pt_fixee.x = gs2.pt_fixee.x;
    gs1.pt_fixee.y = gs2.pt_fixee.y;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_init_gstates() {
    let mut stack = unsafe { &mut gs_stack };
    *stack = vec![];
    let gs = pdf_gstate::init();
    stack.push(gs);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_clear_gstates() {
    let mut stack = unsafe { &mut gs_stack };

    if stack.len() > 1 {
        /* at least 1 elem. */
        warn!("GS stack depth is not zero at the end of the document."); /* op: q */
    }
    *stack = vec![];
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_gsave() -> i32 {
    let mut stack = unsafe { &mut gs_stack };
    let gs0 = stack.last().unwrap();

    let mut gs1 = pdf_gstate::init();
    copy_a_gstate(&mut gs1, gs0);
    stack.push(gs1);

    pdf_doc_add_page_content(b" q");
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_grestore() -> i32 {
    let mut stack = unsafe { &mut gs_stack };
    if stack.len() <= 1 {
        /* Initial state at bottom */
        warn!("Too many grestores."); /* op: Q */
        return -1i32;
    }
    let _gs = stack.pop();
    pdf_doc_add_page_content(b" Q");
    pdf_dev_reset_fonts(0i32);
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_push_gstate() -> i32 {
    let mut stack = unsafe { &mut gs_stack };

    let gs0 = pdf_gstate::init();
    stack.push(gs0);

    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_pop_gstate() -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    if gss.len() <= 1 {
        /* Initial state at bottom */
        warn!("Too many grestores.");
        return -1i32;
    }
    let _gs = gss.pop();
    0i32
}
#[no_mangle]
pub extern "C" fn pdf_dev_current_depth() -> usize {
    let stack = unsafe { &gs_stack };
    stack.len() - 1
    /* 0 means initial state */
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_grestore_to(mut depth: usize) {
    let mut gss = unsafe { &mut gs_stack }; /* op: Q */
    if gss.len() > depth + 1 {
        warn!("Closing pending transformations at end of page/XObject.");
    }
    while gss.len() > depth + 1 {
        pdf_doc_add_page_content(b" Q");
        let _gs = gss.pop();
    }
    pdf_dev_reset_fonts(0i32);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_currentpoint(p: &mut Coord) -> i32 {
    let mut gss = unsafe { &gs_stack };
    let gs = gss.last().unwrap();
    *p = gs.cp.clone();
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_currentmatrix() -> TMatrix {
    let mut gss = unsafe { &gs_stack };
    let gs = gss.last().unwrap();
    gs.matrix.clone()
}
/*
 * mask == 0 means stroking color, mask == 0x20 nonstroking color
 *
 * force == 1 means that operators will be generated even if
 *   the color is the same as the current graphics state color
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_set_color(color: &PdfColor, mut mask: i8, mut force: i32) {
    let mut stack = unsafe { &mut gs_stack };
    let mut gs = stack.last_mut().unwrap();
    let current = if mask as i32 != 0 {
        &mut gs.fillcolor
    } else {
        &mut gs.strokecolor
    };
    if pdf_dev_get_param(2) == 0 || (force == 0 && color == current) {
        /* If "color" is already the current color, then do nothing
         * unless a color operator is forced
         */
        return;
    } /* op: RG K G rg k g etc. */
    graphics_mode(); /* Init to avoid compiler warning */
    let mut len = color.to_string(fmt_buf.as_mut_ptr(), mask);
    fmt_buf[len] = b' ';
    len += 1;
    match color {
        PdfColor::Rgb(..) => {
            fmt_buf[len] = b'R' | mask as u8;
            len += 1;
            fmt_buf[len] = b'G' | mask as u8;
            len += 1;
        }
        PdfColor::Cmyk(..) => {
            fmt_buf[len] = b'K' | mask as u8;
            len += 1;
        }
        PdfColor::Gray(..) => {
            fmt_buf[len] = b'G' | mask as u8;
            len += 1;
        }
        _ => {}
    }
    pdf_doc_add_page_content(&fmt_buf[..len]);
    *current = color.clone();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_concat(M: &TMatrix) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let CTM = &mut gs.matrix;
    let mut buf = &mut fmt_buf;
    let mut len = 0;
    /* Adobe Reader erases page content if there are
     * non invertible transformation.
     */
    if M.determinant().abs() < 2.5e-16 {
        warn!("Transformation matrix not invertible."); /* op: cm */
        warn!("--- M = [{} {} {} {} {} {}]", M.m11, M.m12, M.m21, M.m22, M.m31, M.m32);
        return -1i32;
    }
    if (M.m11 - 1.).abs() > 2.5e-16
        || M.m12.abs() > 2.5e-16
        || M.m21.abs() > 2.5e-16
        || (M.m22 - 1.).abs() > 2.5e-16
        || M.m31.abs() > 2.5e-16
        || M.m32.abs() > 2.5e-16
    {
        buf[len] = b' ';
        len += 1;
        len += pdf_sprint_matrix(&mut buf[len..], M);
        buf[len] = b' ';
        len += 1;
        buf[len] = b'c';
        len += 1;
        buf[len] = b'm';
        len += 1;
        pdf_doc_add_page_content(&buf[..len]);
        *CTM = M.post_transform(CTM);
    }
    let W = M.inverse().unwrap();
    cpa.transform(&W);
    *cpt = W.transform_point(*cpt);
    0i32
}
/*
 * num w        LW  linewidth (g.t. 0)
 * int J        LC  linecap
 * int j        LJ  linejoin
 * num M        ML  miter limit (g.t. 0)
 * array num d  D   line dash
 * int ri       RI  renderint intnet
 * int i        FL  flatness tolerance (0-100)
 * name gs      --  name: res. name of ExtGState dict.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_setmiterlimit(mut mlimit: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack }; /* op: M */
    let gs = gss.last_mut().unwrap(); /* op: J */
    let mut len = 0_usize; /* op: j */
    let mut buf = &mut fmt_buf; /* op: w */
    if gs.miterlimit != mlimit {
        buf[len] = b' '; /* op: */
        len += 1;
        len += pdf_sprint_length(&mut buf[len..], mlimit); /* op: */
        /* op: d */
        buf[len] = b' ';
        len += 1;
        buf[len] = b'M';
        len += 1;
        pdf_doc_add_page_content(&buf[..len]);
        gs.miterlimit = mlimit
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_setlinecap(mut capstyle: i32) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let mut buf = &mut fmt_buf;
    if gs.linecap != capstyle {
        let len = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b" %d J\x00" as *const u8 as *const i8,
            capstyle,
        ) as usize;
        pdf_doc_add_page_content(&buf[..len]);
        gs.linecap = capstyle
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_setlinejoin(mut joinstyle: i32) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let mut buf = &mut fmt_buf;
    if gs.linejoin != joinstyle {
        let len = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b" %d j\x00" as *const u8 as *const i8,
            joinstyle,
        ) as usize;
        pdf_doc_add_page_content(&buf[..len]);
        gs.linejoin = joinstyle
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_setlinewidth(mut width: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let mut len = 0_usize;
    let mut buf = &mut fmt_buf;
    if gs.linewidth != width {
        buf[len] = b' ';
        len += 1;
        len += pdf_sprint_length(&mut buf[len..], width);
        buf[len] = b' ';
        len += 1;
        buf[len] = b'w';
        len += 1;
        pdf_doc_add_page_content(&buf[..len]);
        gs.linewidth = width
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_setdash(pattern: &[f64], mut offset: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let mut buf = &mut fmt_buf;
    let count = pattern.len();
    gs.linedash.num_dash = count as i32;
    gs.linedash.offset = offset;
    pdf_doc_add_page_content(b" [");
    for i in 0..count {
        buf[0] = b' ';
        let len = pdf_sprint_length(&mut buf[1..], pattern[i]);
        pdf_doc_add_page_content(&buf[..len + 1]);
        gs.linedash.pattern[i] = pattern[i];
    }
    pdf_doc_add_page_content(b"] ");
    let len = pdf_sprint_length(&mut buf[..], offset);
    pdf_doc_add_page_content(&buf[..len]);
    pdf_doc_add_page_content(b" d");
    0i32
}
/* ZSYUEDVEDEOF */
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_clip() -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    pdf_dev__flushpath(cpa, b'W', 0, 0)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_eoclip() -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    pdf_dev__flushpath(cpa, b'W', 1, 0)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_flushpath(mut p_op: u8, mut fill_rule: i32) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    /* last arg 'ignore_rule' is only for single object
     * that can be converted to a rect where fill rule
     * is inessential.
     */
    let error = pdf_dev__flushpath(cpa, p_op, fill_rule, 1i32);
    cpa.path.clear();
    gs.flags &= !(1i32 << 0i32);
    error
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_newpath() -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let p = &mut gs.path;
    if !p.path.is_empty() {
        p.path.clear();
    }
    /* The following is required for "newpath" operator in mpost.c. */
    pdf_doc_add_page_content(b" n"); /* op: n */
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_moveto(mut x: f64, mut y: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.moveto(cpt, Coord::new(x, y))
    /* cpt updated */
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_rmoveto(mut x: f64, mut y: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.moveto(cpt, Coord::new(cpt.x + x, cpt.y + y))
    /* cpt updated */
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_lineto(mut x: f64, mut y: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.lineto(cpt, Coord::new(x, y))
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_rlineto(mut x: f64, mut y: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.lineto(cpt, Coord::new(x + cpt.x, y + cpt.y))
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_curveto(
    mut x0: f64,
    mut y0: f64,
    mut x1: f64,
    mut y1: f64,
    mut x2: f64,
    mut y2: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let p0 = Coord::new(x0, y0);
    let p1 = Coord::new(x1, y1);
    let p2 = Coord::new(x2, y2);
    cpa.curveto(cpt, p0, p1, p2)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_vcurveto(
    mut x0: f64,
    mut y0: f64,
    mut x1: f64,
    mut y1: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let cpt_copy = *cpt;
    let p0 = Coord::new(x0, y0);
    let p1 = Coord::new(x1, y1);
    cpa.curveto(cpt, cpt_copy, p0, p1)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_ycurveto(
    mut x0: f64,
    mut y0: f64,
    mut x1: f64,
    mut y1: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let p0 = Coord::new(x0, y0);
    let p1 = Coord::new(x1, y1);
    cpa.curveto(cpt, p0, p1, p1)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_rcurveto(
    mut x0: f64,
    mut y0: f64,
    mut x1: f64,
    mut y1: f64,
    mut x2: f64,
    mut y2: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let p0 = Coord::new(x0 + cpt.x, y0 + cpt.y);
    let p1 = Coord::new(x1 + cpt.x, y1 + cpt.y);
    let p2 = Coord::new(x2 + cpt.x, y2 + cpt.y);
    cpa.curveto(cpt, p0, p1, p2)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_closepath() -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpt = &mut gs.cp;
    let cpa = &mut gs.path;
    pdf_path__closepath(cpa, cpt)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_dtransform(p: &mut Coord, mut M: Option<&TMatrix>) {
    if let Some(m) = M {
        *p = m.transform_vector(p.to_vector()).to_point();
    } else {
        let gss = unsafe { &gs_stack };
        let gs = gss.last().unwrap();
        *p = gs.matrix.transform_vector(p.to_vector()).to_point();
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_idtransform(p: &mut Coord, M: Option<&TMatrix>) {
    if let Some(m) = M {
        *p = idtransform(m, *p).unwrap();
    } else {
        let gss = unsafe { &gs_stack };
        let gs = gss.last().unwrap();
        *p = idtransform(&gs.matrix, *p).unwrap();
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_transform(p: &mut Coord, M: Option<&TMatrix>) {
    if let Some(m) = M {
        *p = m.transform_point(*p);
    } else {
        let gss = unsafe { &gs_stack };
        let gs = gss.last().unwrap();
        *p = gs.matrix.transform_point(*p);
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_arc(c_x: f64, c_y: f64, r: f64, a_0: f64, a_1: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.elliptarc(cpt, Coord::new(c_x, c_y), r, r, 0.0f64, a_0, a_1, 1i32)
}
/* *negative* arc */
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_arcn(c_x: f64, c_y: f64, r: f64, a_0: f64, a_1: f64) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.elliptarc(cpt, Coord::new(c_x, c_y), r, r, 0.0f64, a_0, a_1, -1i32)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_arcx(
    c_x: f64,
    c_y: f64,
    r_x: f64,
    r_y: f64,
    a_0: f64,
    a_1: f64,
    a_d: i32,
    xar: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    cpa.elliptarc(cpt, Coord::new(c_x, c_y), r_x, r_y, xar, a_0, a_1, a_d)
}
/* Required by Tpic */
#[no_mangle]
pub unsafe extern "C" fn pdf_dev_bspline(
    x0: f64,
    y0: f64,
    x1: f64,
    y1: f64,
    x2: f64,
    y2: f64,
) -> i32 {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    let cpa = &mut gs.path;
    let cpt = &mut gs.cp;
    let p1 = Coord::new(x0 + 2. * (x1 - x0) / 3., y0 + 2. * (y1 - y0) / 3.);
    let p2 = Coord::new(x1 + (x2 - x1) / 3., y1 + (y2 - y1) / 3.);
    let p3 = Coord::new(x2, y2);
    cpa.curveto(cpt, p1, p2, p3)
}

impl Rect {
    pub fn fill(&self) {
        unsafe { pdf_dev__rectshape(self, None, b'f'); }
    }
    pub fn clip(&self) {
        unsafe { pdf_dev__rectshape(self, None, b'W'); }
    }
    /*pub fn add(&self) {
        path_added = 1;
        pdf_dev__rectshape(&self, None, b' ');
    }*/
}

#[no_mangle]
pub extern "C" fn pdf_dev_set_fixed_point(mut x: f64, mut y: f64) {
    let mut gss = unsafe { &mut gs_stack };
    let gs = gss.last_mut().unwrap();
    gs.pt_fixee = Coord::new(x, y);
}
/* m -> n x m */
/* Path Construction */
/* Path Painting */
/* NULL pointer of M mean apply current transformation */
/* Requires from mpost.c because new MetaPost graphics must initialize
 * the current gstate. */
/* extension */
/* arc direction   */
/* x-axis-rotation */
/* The depth here is the depth of q/Q nesting.
 * We must remember current depth of nesting when starting a page or xform,
 * and must recover until that depth at the end of page/xform.
 */
pub fn pdf_dev_get_fixed_point() -> Coord {
    let mut gss = unsafe { &gs_stack };
    let gs = gss.last().unwrap();
    gs.pt_fixee.clone()
}
