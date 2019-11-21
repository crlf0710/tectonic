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

use crate::streq_ptr;
use crate::warn;
use crate::DisplayExt;
use crate::SkipBlank;
use std::ffi::{CStr, CString};
use std::ptr;

use super::{spc_arg, spc_env};
use crate::spc_warn;

use crate::dpx_dpxutil::{ParseCIdent, ParseCString, ParseFloatDecimal};
use crate::dpx_pdfcolor::pdf_color_get_current;
use crate::dpx_pdfdev::pdf_dev_scale;
use crate::dpx_pdfdoc::{
    pdf_doc_add_page_content, pdf_doc_add_page_resource, pdf_doc_current_page_resources,
};
use crate::dpx_pdfdraw::{
    pdf_dev_arcx, pdf_dev_bspline, pdf_dev_concat, pdf_dev_flushpath, pdf_dev_grestore,
    pdf_dev_gsave, pdf_dev_lineto, pdf_dev_moveto, pdf_dev_newpath, pdf_dev_set_color,
    pdf_dev_setdash, pdf_dev_setlinecap, pdf_dev_setlinejoin, pdf_dev_setlinewidth,
    pdf_dev_setmiterlimit,
};
use crate::dpx_pdfobj::{
    pdf_foreach_dict, pdf_get_version, pdf_name_value, pdf_new_dict, pdf_new_name, pdf_new_string,
    pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_string_value,
};
use crate::dpx_pdfparse::ParseIdent;
use libc::atof;

use super::SpcHandler;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub fill: i32,
}
#[derive(Clone)]
pub struct spc_tpic_ {
    pub mode: C2RustUnnamed_0,
    pub pen_size: f64,
    pub fill_shape: bool,
    pub fill_color: f64,
    pub points: Vec<Point>,
}

use crate::dpx_pdfdev::Point;

use crate::dpx_pdfdev::TMatrix;

static mut _TPIC_STATE: spc_tpic_ = spc_tpic_ {
    mode: C2RustUnnamed_0 { fill: 0 },
    pen_size: 0.,
    fill_shape: false,
    fill_color: 0.,
    points: Vec::new(),
};
/* We use pdf_doc_add_page_content() here
 * since we always draw isolated graphics.
 */
unsafe fn tpic__clear(tp: *mut spc_tpic_) {
    (*tp).points.clear();
    (*tp).fill_shape = false;
    (*tp).fill_color = 0.0f64;
}
unsafe fn create_xgstate(a: f64, f_ais: i32) -> *mut pdf_obj
/* alpha is shape */ {
    let dict = pdf_new_dict(); /* dash pattern */
    let dict_ref = (*dict).as_dict_mut();
    dict_ref.set("Type", pdf_new_name("ExtGState"));
    if f_ais != 0 {
        dict_ref.set("AIS", true);
    }
    dict_ref.set("ca", a);
    dict
}
unsafe fn check_resourcestatus(category: &str, resname: &str) -> i32 {
    let dict1 = pdf_doc_current_page_resources();
    if dict1.is_null() {
        return 0i32;
    }
    if let Some(dict2) = (*dict1).as_dict().get(category) {
        if dict2.is_dict() && dict2.as_dict().has(resname) {
            return 1i32;
        }
    }
    0i32
}
unsafe fn set_linestyle(pn: f64, da: f64) -> i32 {
    let mut dp: [f64; 2] = [0.; 2];
    pdf_dev_setlinejoin(1i32);
    pdf_dev_setmiterlimit(1.4f64);
    pdf_dev_setlinewidth(pn);
    if da > 0.0f64 {
        dp[0] = da * 72.0f64;
        pdf_dev_setdash(&dp[..1], 0i32 as f64);
        pdf_dev_setlinecap(0i32);
    } else if da < 0.0f64 {
        dp[0] = pn;
        dp[1] = -da * 72.0f64;
        pdf_dev_setdash(&dp[..], 0i32 as f64);
        pdf_dev_setlinecap(1i32);
    } else {
        pdf_dev_setlinecap(0i32);
    }
    0i32
}
unsafe fn set_fillstyle(g: f64, a: f64, f_ais: i32) -> i32 {
    if a > 0.0f64 {
        let alp = (100. * a).round() as i32;
        let resname = format!("_Tps_a{:03}_", alp);
        if check_resourcestatus("ExtGState", &resname) == 0 {
            let dict = create_xgstate(
                (0.01f64 * alp as f64 / 0.01f64 + 0.5f64).floor() * 0.01f64,
                f_ais,
            );
            let s = CString::new(resname.as_bytes()).unwrap();
            pdf_doc_add_page_resource("ExtGState", s.as_ptr() as *const i8, pdf_ref_obj(dict));
            pdf_release_obj(dict);
        }
        let buf = format!(" /{} gs", resname);
        pdf_doc_add_page_content(buf.as_bytes());
        /* op: gs */
    } /* get stroking and fill colors */
    let (_sc, fc) = pdf_color_get_current();
    let new_fc = fc.clone().brightened(g);
    pdf_dev_set_color(&new_fc, 0x20, 0);
    0i32
}
unsafe fn set_styles(
    tp: *mut spc_tpic_,
    c: *const Point,
    f_fs: bool,
    f_vp: bool,
    pn: f64,
    da: f64,
) {
    let mut M = TMatrix::row_major(1., 0., 0., -1., (*c).x, (*c).y);
    pdf_dev_concat(&mut M);
    if f_vp {
        set_linestyle(pn, da);
    }
    if f_fs {
        let (g, a) = if (*tp).mode.fill == 0i32 || (*tp).fill_color == 0. {
            (1. - (*tp).fill_color, 0.)
        } else {
            (0., (*tp).fill_color)
        };
        let f_ais = if (*tp).mode.fill == 2 { 1 } else { 0 };
        set_fillstyle(g, a, f_ais);
    };
}
unsafe fn showpath(f_vp: bool, f_fs: bool)
/* visible_path, fill_shape */
{
    if f_vp {
        if f_fs {
            pdf_dev_flushpath(b'b', 0);
        } else {
            pdf_dev_flushpath(b'S', 0);
        }
    } else if f_fs {
        pdf_dev_flushpath(b'f', 0);
    } else {
        pdf_dev_newpath();
    };
}
unsafe fn tpic__polyline(tp: *mut spc_tpic_, c: *const Point, mut f_vp: bool, da: f64) -> i32 {
    let pn: f64 = (*tp).pen_size;
    let mut f_fs: bool = (*tp).fill_shape;
    let error: i32 = 0i32;
    /*
     * Acrobat claims 'Q' as illegal operation when there are unfinished
     * path (a path without path-painting operator applied)?
     */
    /* Shading is applied only to closed path. */
    f_fs = if (*tp).points[0].x == (*tp).points[(*tp).points.len() - 1].x
        && (*tp).points[0].y == (*tp).points[(*tp).points.len() - 1].y
    {
        f_fs as i32
    } else {
        0i32
    } != 0;
    f_vp = if pn > 0.0f64 { f_vp as i32 } else { 0i32 } != 0;
    if f_vp as i32 != 0 || f_fs as i32 != 0 {
        pdf_dev_gsave();
        set_styles(tp, c, f_fs, f_vp, pn, da);
        pdf_dev_moveto((*tp).points[0].x, (*tp).points[0].y);
        for pt in &(*tp).points {
            pdf_dev_lineto(pt.x, pt.y);
        }
        showpath(f_vp, f_fs);
        pdf_dev_grestore();
    }
    tpic__clear(tp);
    error
}
/*
 * Accroding to
 * "Tpic: Pic for TEX", Tim Morgan, Original by Brian Kernighan, p.20:
 *
 *  A spline is a smooth curve guided by a set of straight lines just
 *  like the line above. It begins at the same place, ends at the same
 *  place, and in between is tangent to the mid-point of each guiding
 *  line. The syntax for a spline is identical to a (path) line except
 *  for using spline instead of line.
 *
 * Spline is not a curve drawn by spline-fitting points p0, p1, ..., pn,
 * defined by tpic special "pa" command. Instead, a path defined by set
 * of points p0, p1, ... is guiding line mentioned above.
 *
 * Dvipsk draws them as a straight line from p0 to q1 = (p0 + p1)/2,
 * followed by a quadratic B-spline curve with starting point q1, (off-
 * curve) control point p1, end point q2 = (p1 + p2)/2, ..., and a
 * straight line from qn to pn.
 */
unsafe fn tpic__spline(tp: *mut spc_tpic_, c: *const Point, mut f_vp: bool, da: f64) -> i32 {
    let mut v: [f64; 6] = [0.; 6];
    let pn: f64 = (*tp).pen_size;
    let mut f_fs: bool = (*tp).fill_shape;
    let error: i32 = 0i32;
    f_fs = if (*tp).points[0].x == (*tp).points[(*tp).points.len() - 1].x
        && (*tp).points[0].y == (*tp).points[(*tp).points.len() - 1].y
    {
        f_fs as i32
    } else {
        0i32
    } != 0;
    f_vp = if pn > 0.0f64 { f_vp as i32 } else { 0i32 } != 0;
    if f_vp as i32 != 0 || f_fs as i32 != 0 {
        pdf_dev_gsave();
        set_styles(tp, c, f_fs, f_vp, pn, da);
        pdf_dev_moveto((*tp).points[0].x, (*tp).points[0].y);
        v[0] = 0.5f64 * ((*tp).points[0].x + (*tp).points[1].x);
        v[1] = 0.5f64 * ((*tp).points[0].y + (*tp).points[1].y);
        pdf_dev_lineto(v[0], v[1]);
        let mut i = 1;
        while i < (*tp).points.len() - 1 {
            /* B-spline control points */
            v[0] = 0.5f64 * ((*tp).points[i - 1].x + (*tp).points[i].x);
            v[1] = 0.5f64 * ((*tp).points[i - 1].y + (*tp).points[i].y);
            v[2] = (*tp).points[i].x;
            v[3] = (*tp).points[i].y;
            v[4] = 0.5f64 * ((*tp).points[i].x + (*tp).points[i + 1].x);
            v[5] = 0.5f64 * ((*tp).points[i].y + (*tp).points[i + 1].y);
            pdf_dev_bspline(v[0], v[1], v[2], v[3], v[4], v[5]);
            i += 1
        }
        pdf_dev_lineto((*tp).points[i].x, (*tp).points[i].y);
        showpath(f_vp, f_fs);
        pdf_dev_grestore();
    }
    tpic__clear(tp);
    error
}
unsafe fn tpic__arc(
    tp: *mut spc_tpic_,
    c: *const Point,
    mut f_vp: bool,
    da: f64,
    v: *mut f64,
) -> i32
/* 6 numbers */ {
    let pn: f64 = (*tp).pen_size;
    let mut f_fs: bool = (*tp).fill_shape;
    f_fs = if ((*v.offset(4) - *v.offset(5)).abs() + 0.5f64).round() >= 360i32 as f64 {
        f_fs as i32
    } else {
        0i32
    } != 0;
    f_vp = if pn > 0.0f64 { f_vp as i32 } else { 0i32 } != 0;
    if f_vp as i32 != 0 || f_fs as i32 != 0 {
        pdf_dev_gsave();
        set_styles(tp, c, f_fs, f_vp, pn, da);
        /* The arcx operator here draws an excess straight line from current
         * point to the starting point of the arc if they are different, as in
         * PostScript language. It may cuase an unexpected behavior when DVIPS
         * transformation command is inserted before TPIC ar command: it invokes
         * moveto and sets currentpoint which may be different from the starting
         * point of arc to be drawn. We use newpath here to avoid drawing an
         * excess line. I'm not sure if it is proper TPIC implementation but this
         * seems to be DVIPS compatible behavior.
         */
        pdf_dev_newpath();
        pdf_dev_arcx(
            *v.offset(0),
            *v.offset(1),
            *v.offset(2),
            *v.offset(3),
            *v.offset(4),
            *v.offset(5),
            1i32,
            0.0f64,
        );
        showpath(f_vp, f_fs);
        pdf_dev_grestore();
    }
    tpic__clear(tp);
    0i32
}
unsafe fn spc_currentpoint(spe: *mut spc_env, pg: *mut i32) -> Point {
    *pg = 0;
    Point::new((*spe).x_user, (*spe).y_user)
}
unsafe fn spc_handler_tpic_pn(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    if let Some(q) = (*ap).cur.parse_float_decimal() {
        (*tp).pen_size = atof(q.as_ptr()) * (0.072f64 / pdf_dev_scale());
        0
    } else {
        spc_warn!(spe, "Invalid pen size specified?");
        -1
    }
}
unsafe fn spc_handler_tpic_pa(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 2] = [0.; 2];
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    let mut i = 0;
    while i < 2 && !(*ap).cur.is_empty() {
        if let Some(q) = (*ap).cur.parse_float_decimal() {
            v[i] = atof(q.as_ptr());
            (*ap).cur.skip_blank();
            i += 1;
        } else {
            spc_warn!(spe, "Missing numbers for TPIC \"pa\" command.");
            return -1;
        }
    }
    if i != 2 {
        spc_warn!(spe, "Invalid arg for TPIC \"pa\" command.");
        return -1i32;
    }
    (*tp).points.push(Point::new(
        v[0] * (0.072f64 / pdf_dev_scale()),
        v[1] * (0.072f64 / pdf_dev_scale()),
    ));
    0i32
}
unsafe fn spc_handler_tpic_fp(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    if (*tp).points.len() < 2 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, 0.0f64)
}
unsafe fn spc_handler_tpic_ip(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    if (*tp).points.len() < 2 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, false, 0.0f64)
}
unsafe fn spc_handler_tpic_da(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    if let Some(q) = (*ap).cur.parse_float_decimal() {
        da = atof(q.as_ptr());
    }
    if (*tp).points.len() < 2 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_dt(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.0f64;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    if let Some(q) = (*ap).cur.parse_float_decimal() {
        da = -atof(q.as_ptr());
    }
    if (*tp).points.len() < 2 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_sp(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.0f64;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    if let Some(q) = (*ap).cur.parse_float_decimal() {
        da = atof(q.as_ptr());
    }
    if (*tp).points.len() < 3 {
        spc_warn!(spe, "Too few points (< 3) for spline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__spline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_ar(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 6] = [0.; 6];
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    let mut i = 0;
    while i < 6 && !(*ap).cur.is_empty() {
        if let Some(q) = (*ap).cur.parse_float_decimal() {
            v[i] = atof(q.as_ptr());
            (*ap).cur.skip_blank();
            i += 1;
        } else {
            spc_warn!(spe, "Invalid args. in TPIC \"ar\" command.");
            return -1i32;
        }
    }
    if i != 6 {
        spc_warn!(spe, "Invalid arg for TPIC \"ar\" command.");
        return -1i32;
    }
    v[0] *= 0.072f64 / pdf_dev_scale();
    v[1] *= 0.072f64 / pdf_dev_scale();
    v[2] *= 0.072f64 / pdf_dev_scale();
    v[3] *= 0.072f64 / pdf_dev_scale();
    v[4] *= 180.0f64 / std::f64::consts::PI;
    v[5] *= 180.0f64 / std::f64::consts::PI;
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__arc(tp, &mut cp, true, 0.0f64, v.as_mut_ptr())
}
unsafe fn spc_handler_tpic_ia(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 6] = [0.; 6];
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*ap).cur.skip_blank();
    let mut i = 0;
    while i < 6 && !(*ap).cur.is_empty() {
        if let Some(q) = (*ap).cur.parse_float_decimal() {
            v[i] = atof(q.as_ptr());
            (*ap).cur.skip_blank();
            i += 1;
        } else {
            spc_warn!(spe, "Invalid args. in TPIC \"ia\" command.");
            return -1i32;
        }
    }
    if i != 6 {
        spc_warn!(spe, "Invalid arg for TPIC \"ia\" command.");
        return -1i32;
    }
    v[0] *= 0.072f64 / pdf_dev_scale();
    v[1] *= 0.072f64 / pdf_dev_scale();
    v[2] *= 0.072f64 / pdf_dev_scale();
    v[3] *= 0.072f64 / pdf_dev_scale();
    v[4] *= 180.0f64 / std::f64::consts::PI;
    v[5] *= 180.0f64 / std::f64::consts::PI;
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__arc(tp, &mut cp, false, 0.0f64, v.as_mut_ptr())
}
unsafe fn spc_handler_tpic_sh(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 0.5f64;
    (*ap).cur.skip_blank();
    if let Some(q) = (*ap).cur.parse_float_decimal() {
        let g: f64 = atof(q.as_ptr());
        if g >= 0. && g <= 1. {
            (*tp).fill_color = g
        } else {
            warn!("Invalid fill color specified: {}\n", g);
            return -1;
        }
    }
    0i32
}
unsafe fn spc_handler_tpic_wh(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 0.0f64;
    0i32
}
unsafe fn spc_handler_tpic_bk(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 1.0f64;
    0i32
}
unsafe fn spc_handler_tpic_tx(spe: *mut spc_env, ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE; /* NULL terminate */
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    spc_warn!(spe, "TPIC command \"tx\" not supported.");
    -1i32
}
unsafe fn spc_handler_tpic__init(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    (*tp).pen_size = 1.0f64;
    (*tp).fill_shape = false;
    (*tp).fill_color = 0.0f64;
    (*tp).points = Vec::new();
    if (*tp).mode.fill != 0i32 && pdf_get_version() < 4_u32 {
        spc_warn!(spe, "Tpic shading support requires PDF version 1.4.");
        (*tp).mode.fill = 0i32
    }
    0i32
}
unsafe fn spc_handler_tpic__bophook(dp: *mut libc::c_void) -> i32 {
    let tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    tpic__clear(tp);
    0i32
}
unsafe fn spc_handler_tpic__eophook(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    if !(*tp).points.is_empty() {
        spc_warn!(spe, "Unflushed tpic path at end of the page.");
    }
    tpic__clear(tp);
    0i32
}
unsafe fn spc_handler_tpic__clean(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    if !(*tp).points.is_empty() {
        spc_warn!(spe, "Unflushed tpic path at end of the document.");
    }
    tpic__clear(tp);
    0i32
}

pub unsafe fn tpic_set_fill_mode(mode: i32) {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    (*tp).mode.fill = mode;
}

pub unsafe fn spc_tpic_at_begin_page() -> i32 {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__bophook(tp as *mut libc::c_void)
}

pub unsafe fn spc_tpic_at_end_page() -> i32 {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__eophook(ptr::null_mut(), tp as *mut libc::c_void)
}

pub unsafe fn spc_tpic_at_begin_document() -> i32 {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__init(ptr::null_mut(), tp as *mut libc::c_void)
}

pub unsafe fn spc_tpic_at_end_document() -> i32 {
    let tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__clean(ptr::null_mut(), tp as *mut libc::c_void)
}
unsafe fn spc_parse_kvpairs(mut ap: *mut spc_arg) -> *mut pdf_obj {
    let mut error: i32 = 0i32;
    let mut dict = pdf_new_dict();
    let dict_ref = (*dict).as_dict_mut();
    (*ap).cur.skip_blank();
    while error == 0 && !(*ap).cur.is_empty() {
        if let Some(kp) = (*ap).cur.parse_val_ident() {
            (*ap).cur.skip_blank();
            if !(*ap).cur.is_empty() && (*ap).cur[0] == b'=' {
                (*ap).cur = &(*ap).cur[1..];
                (*ap).cur.skip_blank();
                if (*ap).cur.is_empty() {
                    error = -1i32;
                    break;
                } else {
                    if let Some(vp) = (*ap).cur.parse_c_string() {
                        dict_ref.set(
                            kp.to_bytes(),
                            pdf_new_string(
                                vp.as_ptr() as *const libc::c_void,
                                (vp.to_bytes().len() + 1) as _,
                            ),
                        );
                    } else {
                        error = -1;
                    }
                }
            } else {
                /* Treated as 'flag' */
                dict_ref.set(kp.to_bytes(), true);
            }
            if error == 0 {
                (*ap).cur.skip_blank();
            }
        } else {
            break;
        }
    }
    if error != 0 {
        pdf_release_obj(dict);
        dict = ptr::null_mut()
    }
    dict
}
unsafe fn tpic_filter_getopts(kp: *mut pdf_obj, vp: *mut pdf_obj, dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    let mut error: i32 = 0i32;
    assert!(!kp.is_null() && !vp.is_null() && !tp.is_null());
    let k = pdf_name_value(&*kp).to_string_lossy();
    if k == "fill-mode" {
        if !(*vp).is_string() {
            warn!("Invalid value for TPIC option fill-mode...");
            error = -1i32
        } else {
            let v = pdf_string_value(&*vp) as *mut i8;
            if streq_ptr(v, b"shape\x00" as *const u8 as *const i8) {
                (*tp).mode.fill = 2i32
            } else if streq_ptr(v, b"opacity\x00" as *const u8 as *const i8) {
                (*tp).mode.fill = 1i32
            } else if streq_ptr(v, b"solid\x00" as *const u8 as *const i8) {
                (*tp).mode.fill = 0i32
            } else {
                warn!(
                    "Invalid value for TPIC option fill-mode: {}",
                    CStr::from_ptr(v).display(),
                );
                error = -1i32
            }
        }
    } else {
        warn!("Unrecognized option for TPIC special handler: {}", k,);
        error = -1i32
    }
    error
}
unsafe fn spc_handler_tpic__setopts(spe: *mut spc_env, ap: *mut spc_arg) -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let dict = spc_parse_kvpairs(ap);
    if dict.is_null() {
        return -1i32;
    }
    let error = pdf_foreach_dict(
        &mut *dict,
        Some(
            tpic_filter_getopts
                as unsafe fn(_: *mut pdf_obj, _: *mut pdf_obj, _: *mut libc::c_void) -> i32,
        ),
        tp as *mut libc::c_void,
    );
    if error == 0 {
        if (*tp).mode.fill != 0i32 && pdf_get_version() < 4_u32 {
            spc_warn!(spe, "Transparent fill mode requires PDF version 1.4.");
            (*tp).mode.fill = 0i32
        }
    }
    error
}
/* DEBUG */
const TPIC_HANDLERS: [SpcHandler; 13] = [
    SpcHandler {
        key: b"pn",
        exec: Some(spc_handler_tpic_pn),
    },
    SpcHandler {
        key: b"pa",
        exec: Some(spc_handler_tpic_pa),
    },
    SpcHandler {
        key: b"fp",
        exec: Some(spc_handler_tpic_fp),
    },
    SpcHandler {
        key: b"ip",
        exec: Some(spc_handler_tpic_ip),
    },
    SpcHandler {
        key: b"da",
        exec: Some(spc_handler_tpic_da),
    },
    SpcHandler {
        key: b"dt",
        exec: Some(spc_handler_tpic_dt),
    },
    SpcHandler {
        key: b"sp",
        exec: Some(spc_handler_tpic_sp),
    },
    SpcHandler {
        key: b"ar",
        exec: Some(spc_handler_tpic_ar),
    },
    SpcHandler {
        key: b"ia",
        exec: Some(spc_handler_tpic_ia),
    },
    SpcHandler {
        key: b"sh",
        exec: Some(spc_handler_tpic_sh),
    },
    SpcHandler {
        key: b"wh",
        exec: Some(spc_handler_tpic_wh),
    },
    SpcHandler {
        key: b"bk",
        exec: Some(spc_handler_tpic_bk),
    },
    SpcHandler {
        key: b"tx",
        exec: Some(spc_handler_tpic_tx),
    },
];
pub fn spc_tpic_check_special(mut buf: &[u8]) -> bool {
    buf.skip_blank();
    let hasnsp = buf.starts_with(b"tpic:");
    if hasnsp {
        buf = &buf[b"tpic:".len()..];
    }
    let mut istpic = false;
    if let Some(q) = buf.parse_c_ident() {
        if hasnsp && q.to_bytes() == b"__setopt__" {
            istpic = true;
        } else {
            for handler in TPIC_HANDLERS.iter() {
                if q.to_bytes() == handler.key {
                    istpic = true;
                    break;
                }
            }
        }
    }
    istpic
}

pub unsafe fn spc_tpic_setup_handler(
    mut sph: *mut SpcHandler,
    spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut hasnsp: i32 = 0i32;
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    (*ap).cur.skip_blank();
    if (*ap).cur.starts_with(b"tpic:") {
        (*ap).cur = &(*ap).cur[b"tpic:".len()..];
        hasnsp = 1;
    }
    if let Some(q) = (*ap).cur.parse_c_ident() {
        if hasnsp != 0 && q.to_bytes() == b"__setopt__" {
            (*ap).command = Some(b"__setopt__");
            (*sph).key = b"tpic:";
            (*sph).exec = Some(spc_handler_tpic__setopts);
            (*ap).cur.skip_blank();
            error = 0i32;
        } else {
            for handler in TPIC_HANDLERS.iter() {
                if q.to_bytes() == handler.key {
                    (*ap).command = Some(handler.key);
                    (*sph).key = b"tpic:";
                    (*sph).exec = handler.exec;
                    (*ap).cur.skip_blank();
                    error = 0i32;
                    break;
                }
            }
        }
    } else {
        error = -1;
    }
    error
}
