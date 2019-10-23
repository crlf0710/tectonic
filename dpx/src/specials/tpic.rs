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

use crate::mfree;
use crate::streq_ptr;
use crate::warn;
use crate::DisplayExt;
use std::ffi::CStr;

use super::{spc_arg, spc_env};
use crate::spc_warn;

use crate::dpx_dpxutil::{parse_c_ident, parse_c_ident_rust, parse_c_string, parse_float_decimal};
use crate::dpx_mem::renew;
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
    pdf_add_dict, pdf_foreach_dict, pdf_get_version, pdf_lookup_dict, pdf_name_value,
    pdf_new_boolean, pdf_new_dict, pdf_new_name, pdf_new_number, pdf_new_string, pdf_obj,
    pdf_ref_obj, pdf_release_obj, pdf_string_value,
};
use crate::dpx_pdfparse::parse_val_ident;
use crate::shims::sprintf;
use libc::{atof, free, memcmp, strlen};

use super::SpcHandler;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub fill: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct spc_tpic_ {
    pub mode: C2RustUnnamed_0,
    pub pen_size: f64,
    pub fill_shape: bool,
    pub fill_color: f64,
    pub points: *mut pdf_coord,
    pub num_points: i32,
    pub max_points: i32,
}

use crate::dpx_pdfdev::pdf_coord;

use crate::dpx_pdfdev::pdf_tmatrix;

/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
unsafe fn skip_blank(mut pp: *mut *const i8, mut endptr: *const i8) {
    let mut p: *const i8 = *pp;
    while p < endptr && (*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _) != 0) {
        p = p.offset(1)
    }
    *pp = p;
}
static mut _TPIC_STATE: spc_tpic_ = spc_tpic_ {
    mode: C2RustUnnamed_0 { fill: 0 },
    pen_size: 0.,
    fill_shape: false,
    fill_color: 0.,
    points: 0 as *const pdf_coord as *mut pdf_coord,
    num_points: 0,
    max_points: 0,
};
/* We use pdf_doc_add_page_content() here
 * since we always draw isolated graphics.
 */
unsafe fn tpic__clear(mut tp: *mut spc_tpic_) {
    (*tp).points = mfree((*tp).points as *mut libc::c_void) as *mut pdf_coord;
    (*tp).num_points = 0i32;
    (*tp).max_points = 0i32;
    (*tp).fill_shape = false;
    (*tp).fill_color = 0.0f64;
}
unsafe fn create_xgstate(mut a: f64, mut f_ais: i32) -> *mut pdf_obj
/* alpha is shape */ {
    let dict = pdf_new_dict(); /* dash pattern */
    pdf_add_dict(dict, "Type", pdf_new_name("ExtGState"));
    if f_ais != 0 {
        pdf_add_dict(dict, "AIS", pdf_new_boolean(1_i8));
    }
    pdf_add_dict(dict, "ca", pdf_new_number(a));
    dict
}
unsafe fn check_resourcestatus(category: &str, mut resname: &str) -> i32 {
    let dict1 = pdf_doc_current_page_resources();
    if dict1.is_null() {
        return 0i32;
    }
    if let Some(dict2) = pdf_lookup_dict(dict1, category) {
        if (*dict2).is_dict() && pdf_lookup_dict(dict2, resname).is_some() {
            return 1i32;
        }
    }
    0i32
}
unsafe fn set_linestyle(mut pn: f64, mut da: f64) -> i32 {
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
unsafe fn set_fillstyle(mut g: f64, mut a: f64, mut f_ais: i32) -> i32 {
    let mut resname: [u8; 32] = [0; 32];
    if a > 0.0f64 {
        let alp = (100. * a).round() as i32;
        sprintf(
            resname.as_mut_ptr() as *mut i8,
            b"_Tps_a%03d_\x00" as *const u8 as *const i8,
            alp,
        );
        if check_resourcestatus(
            "ExtGState",
            CStr::from_bytes_with_nul(&resname)
                .unwrap()
                .to_str()
                .unwrap(),
        ) == 0
        {
            let dict = create_xgstate(
                (0.01f64 * alp as f64 / 0.01f64 + 0.5f64).floor() * 0.01f64,
                f_ais,
            );
            pdf_doc_add_page_resource(
                "ExtGState",
                resname.as_ptr() as *const i8,
                pdf_ref_obj(dict),
            );
            pdf_release_obj(dict);
        }
        let mut buf: [u8; 38] = [0; 38];
        let len = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b" /%s gs\x00" as *const u8 as *const i8,
            resname.as_ptr() as *const i8,
        ) as usize;
        pdf_doc_add_page_content(&buf[..len]);
        /* op: gs */
    } /* get stroking and fill colors */
    let (_sc, fc) = pdf_color_get_current();
    let new_fc = fc.clone().brightened(g);
    pdf_dev_set_color(&new_fc, 0x20, 0);
    0i32
}
unsafe fn set_styles(
    mut tp: *mut spc_tpic_,
    mut c: *const pdf_coord,
    mut f_fs: bool,
    mut f_vp: bool,
    mut pn: f64,
    mut da: f64,
) {
    let mut M = pdf_tmatrix {
        a: 1.,
        b: 0.,
        c: 0.,
        d: -1.,
        e: (*c).x,
        f: (*c).y,
    };
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
        let mut f_ais = if (*tp).mode.fill == 2 { 1 } else { 0 };
        set_fillstyle(g, a, f_ais);
    };
}
unsafe fn showpath(mut f_vp: bool, mut f_fs: bool)
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
unsafe fn tpic__polyline(
    mut tp: *mut spc_tpic_,
    mut c: *const pdf_coord,
    mut f_vp: bool,
    mut da: f64,
) -> i32 {
    let mut pn: f64 = (*tp).pen_size;
    let mut f_fs: bool = (*tp).fill_shape;
    let mut error: i32 = 0i32;
    /*
     * Acrobat claims 'Q' as illegal operation when there are unfinished
     * path (a path without path-painting operator applied)?
     */
    /* Shading is applied only to closed path. */
    f_fs = if (*(*tp).points.offset(0)).x
        == (*(*tp).points.offset(((*tp).num_points - 1i32) as isize)).x
        && (*(*tp).points.offset(0)).y
            == (*(*tp).points.offset(((*tp).num_points - 1i32) as isize)).y
    {
        f_fs as i32
    } else {
        0i32
    } != 0;
    f_vp = if pn > 0.0f64 { f_vp as i32 } else { 0i32 } != 0;
    if f_vp as i32 != 0 || f_fs as i32 != 0 {
        pdf_dev_gsave();
        set_styles(tp, c, f_fs, f_vp, pn, da);
        pdf_dev_moveto((*(*tp).points.offset(0)).x, (*(*tp).points.offset(0)).y);
        for i in 0..(*tp).num_points {
            pdf_dev_lineto(
                (*(*tp).points.offset(i as isize)).x,
                (*(*tp).points.offset(i as isize)).y,
            );
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
unsafe fn tpic__spline(
    mut tp: *mut spc_tpic_,
    mut c: *const pdf_coord,
    mut f_vp: bool,
    mut da: f64,
) -> i32 {
    let mut v: [f64; 6] = [0.; 6];
    let mut pn: f64 = (*tp).pen_size;
    let mut f_fs: bool = (*tp).fill_shape;
    let mut error: i32 = 0i32;
    f_fs = if (*(*tp).points.offset(0)).x
        == (*(*tp).points.offset(((*tp).num_points - 1i32) as isize)).x
        && (*(*tp).points.offset(0)).y
            == (*(*tp).points.offset(((*tp).num_points - 1i32) as isize)).y
    {
        f_fs as i32
    } else {
        0i32
    } != 0;
    f_vp = if pn > 0.0f64 { f_vp as i32 } else { 0i32 } != 0;
    if f_vp as i32 != 0 || f_fs as i32 != 0 {
        pdf_dev_gsave();
        set_styles(tp, c, f_fs, f_vp, pn, da);
        pdf_dev_moveto((*(*tp).points.offset(0)).x, (*(*tp).points.offset(0)).y);
        v[0] = 0.5f64 * ((*(*tp).points.offset(0)).x + (*(*tp).points.offset(1)).x);
        v[1] = 0.5f64 * ((*(*tp).points.offset(0)).y + (*(*tp).points.offset(1)).y);
        pdf_dev_lineto(v[0], v[1]);
        let mut i = 1;
        while i < (*tp).num_points - 1i32 {
            /* B-spline control points */
            v[0] = 0.5f64
                * ((*(*tp).points.offset((i - 1i32) as isize)).x
                    + (*(*tp).points.offset(i as isize)).x);
            v[1] = 0.5f64
                * ((*(*tp).points.offset((i - 1i32) as isize)).y
                    + (*(*tp).points.offset(i as isize)).y);
            v[2] = (*(*tp).points.offset(i as isize)).x;
            v[3] = (*(*tp).points.offset(i as isize)).y;
            v[4] = 0.5f64
                * ((*(*tp).points.offset(i as isize)).x
                    + (*(*tp).points.offset((i + 1i32) as isize)).x);
            v[5] = 0.5f64
                * ((*(*tp).points.offset(i as isize)).y
                    + (*(*tp).points.offset((i + 1i32) as isize)).y);
            pdf_dev_bspline(v[0], v[1], v[2], v[3], v[4], v[5]);
            i += 1
        }
        pdf_dev_lineto(
            (*(*tp).points.offset(i as isize)).x,
            (*(*tp).points.offset(i as isize)).y,
        );
        showpath(f_vp, f_fs);
        pdf_dev_grestore();
    }
    tpic__clear(tp);
    error
}
unsafe fn tpic__arc(
    mut tp: *mut spc_tpic_,
    mut c: *const pdf_coord,
    mut f_vp: bool,
    mut da: f64,
    mut v: *mut f64,
) -> i32
/* 6 numbers */ {
    let mut pn: f64 = (*tp).pen_size;
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
unsafe fn spc_currentpoint(mut spe: *mut spc_env, mut pg: *mut i32) -> pdf_coord {
    *pg = 0;
    pdf_coord::new((*spe).x_user, (*spe).y_user)
}
unsafe fn spc_handler_tpic_pn(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
    if q.is_null() {
        spc_warn!(spe, "Invalid pen size specified?");
        return -1i32;
    }
    (*tp).pen_size = atof(q) * (0.072f64 / pdf_dev_scale());
    free(q as *mut libc::c_void);
    0i32
}
unsafe fn spc_handler_tpic_pa(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 2] = [0.; 2];
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let mut i = 0;
    while i < 2 && (*ap).curptr < (*ap).endptr {
        let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
        if q.is_null() {
            spc_warn!(spe, "Missing numbers for TPIC \"pa\" command.");
            return -1i32;
        }
        v[i] = atof(q);
        free(q as *mut libc::c_void);
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        i += 1
    }
    if i != 2 {
        spc_warn!(spe, "Invalid arg for TPIC \"pa\" command.");
        return -1i32;
    }
    if (*tp).num_points >= (*tp).max_points {
        (*tp).max_points += 256i32;
        (*tp).points = renew(
            (*tp).points as *mut libc::c_void,
            ((*tp).max_points as u32 as u64).wrapping_mul(::std::mem::size_of::<pdf_coord>() as u64)
                as u32,
        ) as *mut pdf_coord
    }
    (*(*tp).points.offset((*tp).num_points as isize)).x = v[0] * (0.072f64 / pdf_dev_scale());
    (*(*tp).points.offset((*tp).num_points as isize)).y = v[1] * (0.072f64 / pdf_dev_scale());
    (*tp).num_points += 1i32;
    0i32
}
unsafe fn spc_handler_tpic_fp(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    if (*tp).num_points <= 1i32 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, 0.0f64)
}
unsafe fn spc_handler_tpic_ip(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    if (*tp).num_points <= 1i32 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, false, 0.0f64)
}
unsafe fn spc_handler_tpic_da(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        da = atof(q);
        free(q as *mut libc::c_void);
    }
    if (*tp).num_points <= 1i32 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_dt(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.0f64;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        da = -atof(q);
        free(q as *mut libc::c_void);
    }
    if (*tp).num_points <= 1i32 {
        spc_warn!(spe, "Too few points (< 2) for polyline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__polyline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_sp(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut da: f64 = 0.0f64;
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        da = atof(q);
        free(q as *mut libc::c_void);
    }
    if (*tp).num_points <= 2i32 {
        spc_warn!(spe, "Too few points (< 3) for spline path.");
        return -1i32;
    }
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__spline(tp, &mut cp, true, da)
}
unsafe fn spc_handler_tpic_ar(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 6] = [0.; 6];
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let mut i = 0;
    while i < 6 && (*ap).curptr < (*ap).endptr {
        let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
        if q.is_null() {
            spc_warn!(spe, "Invalid args. in TPIC \"ar\" command.");
            return -1i32;
        }
        v[i] = atof(q);
        free(q as *mut libc::c_void);
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        i += 1
    }
    if i != 6 {
        spc_warn!(spe, "Invalid arg for TPIC \"ar\" command.");
        return -1i32;
    }
    v[0] *= 0.072f64 / pdf_dev_scale();
    v[1] *= 0.072f64 / pdf_dev_scale();
    v[2] *= 0.072f64 / pdf_dev_scale();
    v[3] *= 0.072f64 / pdf_dev_scale();
    v[4] *= 180.0f64 / 3.14159265358979323846f64;
    v[5] *= 180.0f64 / 3.14159265358979323846f64;
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__arc(tp, &mut cp, true, 0.0f64, v.as_mut_ptr())
}
unsafe fn spc_handler_tpic_ia(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let mut v: [f64; 6] = [0.; 6];
    let mut pg: i32 = 0;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let mut i = 0;
    while i < 6 && (*ap).curptr < (*ap).endptr {
        let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
        if q.is_null() {
            spc_warn!(spe, "Invalid args. in TPIC \"ia\" command.");
            return -1i32;
        }
        v[i] = atof(q);
        free(q as *mut libc::c_void);
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        i += 1
    }
    if i != 6 {
        spc_warn!(spe, "Invalid arg for TPIC \"ia\" command.");
        return -1i32;
    }
    v[0] *= 0.072f64 / pdf_dev_scale();
    v[1] *= 0.072f64 / pdf_dev_scale();
    v[2] *= 0.072f64 / pdf_dev_scale();
    v[3] *= 0.072f64 / pdf_dev_scale();
    v[4] *= 180.0f64 / 3.14159265358979323846f64;
    v[5] *= 180.0f64 / 3.14159265358979323846f64;
    let mut cp = spc_currentpoint(spe, &mut pg);
    tpic__arc(tp, &mut cp, false, 0.0f64, v.as_mut_ptr())
}
unsafe fn spc_handler_tpic_sh(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 0.5f64;
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        let mut g: f64 = atof(q);
        free(q as *mut libc::c_void);
        if g >= 0.0f64 && g <= 1.0f64 {
            (*tp).fill_color = g
        } else {
            warn!("Invalid fill color specified: {}\n", g);
            return -1i32;
        }
    }
    0i32
}
unsafe fn spc_handler_tpic_wh(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 0.0f64;
    0i32
}
unsafe fn spc_handler_tpic_bk(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    (*tp).fill_shape = true;
    (*tp).fill_color = 1.0f64;
    0i32
}
unsafe fn spc_handler_tpic_tx(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32
/* , void *dp) */ {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE; /* NULL terminate */
    assert!(!spe.is_null() && !ap.is_null() && !tp.is_null());
    spc_warn!(spe, "TPIC command \"tx\" not supported.");
    -1i32
}
unsafe fn spc_handler_tpic__init(mut spe: *mut spc_env, mut dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    (*tp).pen_size = 1.0f64;
    (*tp).fill_shape = false;
    (*tp).fill_color = 0.0f64;
    (*tp).points = 0 as *mut pdf_coord;
    (*tp).num_points = 0i32;
    (*tp).max_points = 0i32;
    if (*tp).mode.fill != 0i32 && pdf_get_version() < 4_u32 {
        spc_warn!(spe, "Tpic shading support requires PDF version 1.4.");
        (*tp).mode.fill = 0i32
    }
    0i32
}
unsafe fn spc_handler_tpic__bophook(mut dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    tpic__clear(tp);
    0i32
}
unsafe fn spc_handler_tpic__eophook(mut spe: *mut spc_env, mut dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    if (*tp).num_points > 0i32 {
        spc_warn!(spe, "Unflushed tpic path at end of the page.");
    }
    tpic__clear(tp);
    0i32
}
unsafe fn spc_handler_tpic__clean(mut spe: *mut spc_env, mut dp: *mut libc::c_void) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    assert!(!tp.is_null());
    if (*tp).num_points > 0i32 {
        spc_warn!(spe, "Unflushed tpic path at end of the document.");
    }
    tpic__clear(tp);
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn tpic_set_fill_mode(mut mode: i32) {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    (*tp).mode.fill = mode;
}
#[no_mangle]
pub unsafe extern "C" fn spc_tpic_at_begin_page() -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__bophook(tp as *mut libc::c_void)
}
#[no_mangle]
pub unsafe extern "C" fn spc_tpic_at_end_page() -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__eophook(0 as *mut spc_env, tp as *mut libc::c_void)
}
#[no_mangle]
pub unsafe extern "C" fn spc_tpic_at_begin_document() -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__init(0 as *mut spc_env, tp as *mut libc::c_void)
}
#[no_mangle]
pub unsafe extern "C" fn spc_tpic_at_end_document() -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    spc_handler_tpic__clean(0 as *mut spc_env, tp as *mut libc::c_void)
}
unsafe fn spc_parse_kvpairs(mut ap: *mut spc_arg) -> *mut pdf_obj {
    let mut error: i32 = 0i32;
    let mut dict = pdf_new_dict();
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    while error == 0 && (*ap).curptr < (*ap).endptr {
        let kp = parse_val_ident(&mut (*ap).curptr, (*ap).endptr);
        if kp.is_null() {
            break;
        }
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        if (*ap).curptr < (*ap).endptr && *(*ap).curptr.offset(0) as i32 == '=' as i32 {
            (*ap).curptr = (*ap).curptr.offset(1);
            skip_blank(&mut (*ap).curptr, (*ap).endptr);
            if (*ap).curptr == (*ap).endptr {
                free(kp as *mut libc::c_void);
                error = -1i32;
                break;
            } else {
                let vp = parse_c_string(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    pdf_add_dict(
                        dict,
                        CStr::from_ptr(kp).to_bytes(),
                        pdf_new_string(vp as *const libc::c_void, strlen(vp).wrapping_add(1) as _),
                    );
                    free(vp as *mut libc::c_void);
                }
            }
        } else {
            /* Treated as 'flag' */
            pdf_add_dict(dict, CStr::from_ptr(kp).to_bytes(), pdf_new_boolean(1_i8));
        }
        free(kp as *mut libc::c_void);
        if error == 0 {
            skip_blank(&mut (*ap).curptr, (*ap).endptr);
        }
    }
    if error != 0 {
        pdf_release_obj(dict);
        dict = 0 as *mut pdf_obj
    }
    dict
}
unsafe extern "C" fn tpic_filter_getopts(
    mut kp: *mut pdf_obj,
    mut vp: *mut pdf_obj,
    mut dp: *mut libc::c_void,
) -> i32 {
    let mut tp: *mut spc_tpic_ = dp as *mut spc_tpic_;
    let mut error: i32 = 0i32;
    assert!(!kp.is_null() && !vp.is_null() && !tp.is_null());
    let k = pdf_name_value(&*kp).to_string_lossy();
    if k == "fill-mode" {
        if !(*vp).is_string() {
            warn!("Invalid value for TPIC option fill-mode...");
            error = -1i32
        } else {
            let v = pdf_string_value(vp) as *mut i8;
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
unsafe fn spc_handler_tpic__setopts(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut tp: *mut spc_tpic_ = &mut _TPIC_STATE;
    let dict = spc_parse_kvpairs(ap);
    if dict.is_null() {
        return -1i32;
    }
    let error = pdf_foreach_dict(
        dict,
        Some(
            tpic_filter_getopts
                as unsafe extern "C" fn(
                    _: *mut pdf_obj,
                    _: *mut pdf_obj,
                    _: *mut libc::c_void,
                ) -> i32,
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
pub fn spc_tpic_check_special(buf: &[u8]) -> bool {
    let mut buf = crate::skip_blank(buf);
    let hasnsp = buf.starts_with(b"tpic:");
    if hasnsp {
        buf = &buf[b"tpic:".len()..];
    }
    let mut istpic = false;
    if let Some(q) = parse_c_ident_rust(buf) {
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
#[no_mangle]
pub unsafe extern "C" fn spc_tpic_setup_handler(
    mut sph: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut hasnsp: i32 = 0i32;
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    if (*ap)
        .curptr
        .offset(strlen(b"tpic:\x00" as *const u8 as *const i8) as isize)
        < (*ap).endptr
        && memcmp(
            (*ap).curptr as *const libc::c_void,
            b"tpic:\x00" as *const u8 as *const i8 as *const libc::c_void,
            strlen(b"tpic:\x00" as *const u8 as *const i8),
        ) == 0
    {
        (*ap).curptr = (*ap)
            .curptr
            .offset(strlen(b"tpic:\x00" as *const u8 as *const i8) as isize);
        hasnsp = 1i32
    }
    let q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
    if q.is_null() {
        error = -1i32
    } else if !q.is_null()
        && hasnsp != 0
        && streq_ptr(q, b"__setopt__\x00" as *const u8 as *const i8) as i32 != 0
    {
        (*ap).command = Some(b"__setopt__");
        (*sph).key = b"tpic:";
        (*sph).exec = Some(spc_handler_tpic__setopts);
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        error = 0i32;
        free(q as *mut libc::c_void);
    } else {
        for handler in TPIC_HANDLERS.iter() {
            if CStr::from_ptr(q).to_bytes() == handler.key {
                (*ap).command = Some(handler.key);
                (*sph).key = b"tpic:";
                (*sph).exec = handler.exec;
                skip_blank(&mut (*ap).curptr, (*ap).endptr);
                error = 0i32;
                break;
            }
        }
        free(q as *mut libc::c_void);
    }
    error
}
