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

use super::{spc_arg, spc_env};
use crate::dpx_dpxutil::{parse_c_ident, parse_float_decimal};
use crate::dpx_pdfcolor::PdfColor;
use crate::dpx_pdfdev::{Rect, TMatrix, transform_info};
use crate::dpx_pdfparse::skip_white;
use crate::mfree;
use crate::shims::strcasecmp;
use crate::spc_warn;
use crate::streq_ptr;
use crate::DisplayExt;
use libc::{atof, free, memcmp, strcmp, strlen};
use std::ffi::CStr;

/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
unsafe fn skip_blank(mut pp: *mut *const i8, mut endptr: *const i8) {
    let mut p: *const i8 = *pp; /* 360 / 60 */
    while p < endptr && (*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _) != 0) {
        p = p.offset(1)
    }
    *pp = p;
}
#[no_mangle]
pub unsafe extern "C" fn spc_util_read_numbers(
    mut values: *mut f64,
    mut num_values: i32,
    mut args: *mut spc_arg,
) -> i32 {
    skip_blank(&mut (*args).curptr, (*args).endptr);
    let mut count = 0;
    while count < num_values && (*args).curptr < (*args).endptr {
        let q = parse_float_decimal(&mut (*args).curptr, (*args).endptr);
        if q.is_null() {
            break;
        }
        *values.offset(count as isize) = atof(q);
        free(q as *mut libc::c_void);
        skip_blank(&mut (*args).curptr, (*args).endptr);
        count += 1
    }
    count
}
unsafe fn rgb_color_from_hsv(mut h: f64, mut s: f64, mut v: f64) -> PdfColor {
    let mut b = v;
    let mut g = b;
    let mut r = g;
    if s != 0.0f64 {
        let h6 = h * 6i32 as f64;
        let i = h6 as i32;
        let f = h6 - i as f64;
        let v1 = v * (1i32 as f64 - s);
        let v2 = v * (1i32 as f64 - s * f);
        let v3 = v * (1i32 as f64 - s * (1i32 as f64 - f));
        match i {
            0 => {
                r = v;
                g = v3;
                b = v1
            }
            1 => {
                r = v2;
                g = v;
                b = v1
            }
            2 => {
                r = v1;
                g = v;
                b = v3
            }
            3 => {
                r = v1;
                g = v2;
                b = v
            }
            4 => {
                r = v3;
                g = v1;
                b = v
            }
            5 => {
                r = v;
                g = v1;
                b = v2
            }
            6 => {
                r = v;
                g = v1;
                b = v2
            }
            _ => {}
        }
    }
    PdfColor::from_rgb(r, g, b).unwrap()
}
unsafe fn spc_read_color_color(
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> Result<PdfColor, ()> {
    let mut cv: [f64; 4] = [0.; 4];
    let mut result: Result<PdfColor, ()>;
    let mut q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
    if q.is_null() {
        spc_warn!(spe, "No valid color specified?");
        return Err(());
    }
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    if streq_ptr(q, b"rgb\x00" as *const u8 as *const i8) {
        /* Handle rgb color */
        let nc = spc_util_read_numbers(cv.as_mut_ptr(), 3i32, ap);
        if nc != 3i32 {
            spc_warn!(spe, "Invalid value for RGB color specification.");
            result = Err(())
        } else {
            result = PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn())
        }
    } else if streq_ptr(q, b"cmyk\x00" as *const u8 as *const i8) {
        /* Handle cmyk color */
        let nc = spc_util_read_numbers(cv.as_mut_ptr(), 4i32, ap);
        if nc != 4i32 {
            spc_warn!(spe, "Invalid value for CMYK color specification.");
            result = Err(())
        } else {
            result = PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn())
        }
    } else if streq_ptr(q, b"gray\x00" as *const u8 as *const i8) {
        /* Handle gray */
        let nc = spc_util_read_numbers(cv.as_mut_ptr(), 1i32, ap);
        if nc != 1i32 {
            spc_warn!(spe, "Invalid value for gray color specification.");
            result = Err(())
        } else {
            result = PdfColor::from_gray(cv[0]).map_err(|err| err.warn())
        }
    } else if streq_ptr(q, b"spot\x00" as *const u8 as *const i8) {
        /* Handle spot colors */
        let mut color_name: *mut i8 = parse_c_ident(&mut (*ap).curptr, (*ap).endptr); /* Must be a "named" color */
        if color_name.is_null() {
            spc_warn!(spe, "No valid spot color name specified?");
            return Err(());
        }
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        let nc = spc_util_read_numbers(cv.as_mut_ptr(), 1i32, ap);
        if nc != 1i32 {
            spc_warn!(spe, "Invalid value for spot color specification.");
            result = Err(());
            free(color_name as *mut libc::c_void);
        } else {
            result = PdfColor::from_spot(CStr::from_ptr(color_name).to_owned(), cv[0])
                .map_err(|err| err.warn())
        }
    } else if streq_ptr(q, b"hsb\x00" as *const u8 as *const i8) {
        let nc = spc_util_read_numbers(cv.as_mut_ptr(), 3i32, ap);
        if nc != 3i32 {
            spc_warn!(spe, "Invalid value for HSB color specification.");
            result = Err(());
        } else {
            let color = rgb_color_from_hsv(cv[0], cv[1], cv[2]);
            if let &PdfColor::Rgb(r, g, b) = &color {
                spc_warn!(
                    spe,
                    "HSB color converted to RGB: hsb: <{}, {}, {}> ==> rgb: <{}, {}, {}>",
                    cv[0],
                    cv[1],
                    cv[2],
                    r,
                    g,
                    b
                );
            } else {
                unreachable!();
            }
            result = Ok(color);
        }
    } else {
        result = if let Ok(name) = CStr::from_ptr(q).to_str() {
            if let Some(color) = pdf_color_namedcolor(name) {
                Ok(color)
            } else {
                Err(())
            }
        } else {
            Err(())
        };
        if result.is_err() {
            spc_warn!(
                spe,
                "Unrecognized color name: {}",
                CStr::from_ptr(q).display(),
            );
        }
    }
    free(q as *mut libc::c_void);
    result
}
/* Argument for this is PDF_Number or PDF_Array.
 * But we ignore that since we don't want to add
 * dependency to pdfxxx and @foo can not be
 * allowed for color specification. "pdf" here
 * means pdf: special syntax.
 */
unsafe fn spc_read_color_pdf(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> Result<PdfColor, ()> {
    let mut cv: [f64; 4] = [0.; 4]; /* at most four */
    let mut isarry: bool = false;
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    if *(*ap).curptr.offset(0) as i32 == '[' as i32 {
        (*ap).curptr = (*ap).curptr.offset(1);
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        isarry = true
    }
    let nc = spc_util_read_numbers(cv.as_mut_ptr(), 4i32, ap);
    let mut result = match nc {
        1 => PdfColor::from_gray(cv[0]).map_err(|err| err.warn()),
        3 => PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn()),
        4 => PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn()),
        _ => {
            /* Try to read the color names defined in dvipsname.def */
            let q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
            if q.is_null() {
                spc_warn!(spe, "No valid color specified?");
                return Err(());
            }
            let mut result = CStr::from_ptr(q)
                .to_str()
                .ok()
                .and_then(|name| pdf_color_namedcolor(name))
                .ok_or(());
            if result.is_err() {
                spc_warn!(
                    spe,
                    "Unrecognized color name: {}, keep the current color",
                    CStr::from_ptr(q).display(),
                );
            }
            free(q as *mut libc::c_void);
            result
        }
    };
    if isarry {
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        if (*ap).curptr >= (*ap).endptr || *(*ap).curptr.offset(0) as i32 != ']' as i32 {
            spc_warn!(spe, "Unbalanced \'[\' and \']\' in color specification.");
            result = Err(())
        } else {
            (*ap).curptr = (*ap).curptr.offset(1)
        }
    }
    result
}
/* This is for reading *single* color specification. */
#[no_mangle]
pub unsafe extern "C" fn spc_util_read_colorspec(
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
    mut syntax: bool,
) -> Result<PdfColor, ()> {
    assert!(!spe.is_null() && !ap.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    if (*ap).curptr >= (*ap).endptr {
        Err(())
    } else if syntax {
        spc_read_color_color(spe, ap)
    } else {
        spc_read_color_pdf(spe, ap)
    }
}
#[no_mangle]
pub unsafe extern "C" fn spc_util_read_pdfcolor(
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
    defaultcolor: Option<&PdfColor>,
) -> Result<PdfColor, ()> {
    assert!(!spe.is_null() && !ap.is_null());
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    if (*ap).curptr >= (*ap).endptr {
        Err(())
    } else if let Some(c) = spc_read_color_pdf(spe, ap)
        .ok()
        .or_else(|| defaultcolor.cloned())
    {
        Ok(c)
    } else {
        Err(())
    }
}
/* This need to allow 'true' prefix for unit and
 * length value must be divided by current magnification.
 */
/* XXX: there are four quasi-redundant versions of this; grp for K_UNIT__PT */
unsafe fn spc_util_read_length(
    mut spe: *mut spc_env,
    mut vp: *mut f64,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut u: f64 = 1.0f64;
    let mut ukeys: [*const i8; 10] = [
        b"pt\x00" as *const u8 as *const i8,
        b"in\x00" as *const u8 as *const i8,
        b"cm\x00" as *const u8 as *const i8,
        b"mm\x00" as *const u8 as *const i8,
        b"bp\x00" as *const u8 as *const i8,
        b"pc\x00" as *const u8 as *const i8,
        b"dd\x00" as *const u8 as *const i8,
        b"cc\x00" as *const u8 as *const i8,
        b"sp\x00" as *const u8 as *const i8,
        0 as *const i8,
    ];
    let mut error: i32 = 0i32;
    let q = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr); /* inverse magnify */
    if q.is_null() {
        return -1i32;
    }
    let v = atof(q);
    free(q as *mut libc::c_void);
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    let mut q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        let mut qq: *mut i8 = q;
        if strlen(q) >= strlen(b"true\x00" as *const u8 as *const i8)
            && memcmp(
                q as *const libc::c_void,
                b"true\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"true\x00" as *const u8 as *const i8),
            ) == 0
        {
            u /= if (*spe).mag != 0.0f64 {
                (*spe).mag
            } else {
                1.0f64
            };
            q = q.offset(strlen(b"true\x00" as *const u8 as *const i8) as isize);
            if *q == 0 {
                free(qq as *mut libc::c_void);
                skip_white(&mut (*ap).curptr, (*ap).endptr);
                q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
                qq = q
            }
        }
        if !q.is_null() {
            let mut k = 0;
            while !ukeys[k].is_null() && strcmp(ukeys[k], q) != 0 {
                k += 1
            }
            match k {
                0 => u *= 72.0f64 / 72.27f64,
                1 => u *= 72.0f64,
                2 => u *= 72.0f64 / 2.54f64,
                3 => u *= 72.0f64 / 25.4f64,
                4 => u *= 1.0f64,
                5 => u *= 12.0f64 * 72.0f64 / 72.27f64,
                6 => u *= 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
                7 => u *= 12.0f64 * 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
                8 => u *= 72.0f64 / (72.27f64 * 65536i32 as f64),
                _ => {
                    spc_warn!(
                        spe,
                        "Unknown unit of measure: {}",
                        CStr::from_ptr(q).display(),
                    );
                    error = -1i32
                }
            }
            free(qq as *mut libc::c_void);
        } else {
            spc_warn!(spe, "Missing unit of measure after \"true\"");
            error = -1i32
        }
    }
    *vp = v * u;
    error
}
/*
 * Compute a transformation matrix
 * transformations are applied in the following
 * order: scaling, rotate, displacement.
 */
extern "C" fn make_transmatrix(
    M: &mut TMatrix,
    mut xoffset: f64,
    mut yoffset: f64,
    mut xscale: f64,
    mut yscale: f64,
    mut rotate: f64,
) {
    let (s, c) = rotate.sin_cos();
    M.a = xscale * c;
    M.b = xscale * s;
    M.c = -yscale * s;
    M.d = yscale * c;
    M.e = xoffset;
    M.f = yoffset;
}
unsafe fn spc_read_dimtrns_dvips(
    mut spe: *mut spc_env,
    t: &mut transform_info,
    mut ap: *mut spc_arg,
) -> i32 {
    const _DTKEYS: [*const i8; 15] = [
        b"hoffset\x00" as *const u8 as *const i8,
        b"voffset\x00" as *const u8 as *const i8,
        b"hsize\x00" as *const u8 as *const i8,
        b"vsize\x00" as *const u8 as *const i8,
        b"hscale\x00" as *const u8 as *const i8,
        b"vscale\x00" as *const u8 as *const i8,
        b"angle\x00" as *const u8 as *const i8,
        b"clip\x00" as *const u8 as *const i8,
        b"llx\x00" as *const u8 as *const i8,
        b"lly\x00" as *const u8 as *const i8,
        b"urx\x00" as *const u8 as *const i8,
        b"ury\x00" as *const u8 as *const i8,
        b"rwi\x00" as *const u8 as *const i8,
        b"rhi\x00" as *const u8 as *const i8,
        0 as *const i8,
    ];
    let mut error: i32 = 0i32;
    let mut rotate = 0.0f64;
    let mut yoffset = rotate;
    let mut xoffset = yoffset;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    while error == 0 && (*ap).curptr < (*ap).endptr {
        let kp = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
        if kp.is_null() {
            break;
        }
        let mut k = 0;
        while !_DTKEYS[k].is_null() && strcmp(kp, _DTKEYS[k]) != 0 {
            k += 1
        }
        if _DTKEYS[k as usize].is_null() {
            spc_warn!(
                spe,
                "Unrecognized dimension/transformation key: {}",
                CStr::from_ptr(kp).display(),
            );
            error = -1i32;
            free(kp as *mut libc::c_void);
            break;
        } else {
            skip_blank(&mut (*ap).curptr, (*ap).endptr);
            if k == 7 {
                t.flags |= 1i32 << 3i32;
                free(kp as *mut libc::c_void);
            /* not key-value */
            } else {
                if (*ap).curptr < (*ap).endptr && *(*ap).curptr.offset(0) as i32 == '=' as i32 {
                    (*ap).curptr = (*ap).curptr.offset(1);
                    skip_blank(&mut (*ap).curptr, (*ap).endptr);
                }
                let mut vp;
                if *(*ap).curptr.offset(0) as i32 == '\'' as i32
                    || *(*ap).curptr.offset(0) as i32 == '\"' as i32
                {
                    let mut qchr: i8 = *(*ap).curptr.offset(0);
                    (*ap).curptr = (*ap).curptr.offset(1);
                    skip_blank(&mut (*ap).curptr, (*ap).endptr);
                    vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                    skip_blank(&mut (*ap).curptr, (*ap).endptr);
                    if !vp.is_null() && qchr as i32 != *(*ap).curptr.offset(0) as i32 {
                        spc_warn!(
                            spe,
                            "Syntax error in dimension/transformation specification."
                        );
                        error = -1i32;
                        vp = mfree(vp as *mut libc::c_void) as *mut i8
                    }
                    (*ap).curptr = (*ap).curptr.offset(1)
                } else {
                    vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr)
                }
                if error == 0 && vp.is_null() {
                    spc_warn!(
                        spe,
                        "Missing value for dimension/transformation: {}",
                        CStr::from_ptr(kp).display(),
                    );
                    error = -1i32
                }
                free(kp as *mut libc::c_void);
                if vp.is_null() || error != 0 {
                    break;
                }
                match k {
                    0 => xoffset = atof(vp),
                    1 => yoffset = atof(vp),
                    2 => {
                        t.width = atof(vp);
                        t.flags |= 1i32 << 1i32
                    }
                    3 => {
                        t.height = atof(vp);
                        t.flags |= 1i32 << 2i32
                    }
                    4 => xscale = atof(vp) / 100.0f64,
                    5 => yscale = atof(vp) / 100.0f64,
                    6 => rotate = 3.14159265358979323846f64 * atof(vp) / 180.0f64,
                    8 => {
                        t.bbox.ll.x = atof(vp);
                        t.flags |= 1i32 << 0i32
                    }
                    9 => {
                        t.bbox.ll.y = atof(vp);
                        t.flags |= 1i32 << 0i32
                    }
                    10 => {
                        t.bbox.ur.x = atof(vp);
                        t.flags |= 1i32 << 0i32
                    }
                    11 => {
                        t.bbox.ur.y = atof(vp);
                        t.flags |= 1i32 << 0i32
                    }
                    12 => {
                        t.width = atof(vp) / 10.0f64;
                        t.flags |= 1i32 << 1i32
                    }
                    13 => {
                        t.height = atof(vp) / 10.0f64;
                        t.flags |= 1i32 << 2i32
                    }
                    _ => {}
                }
                skip_blank(&mut (*ap).curptr, (*ap).endptr);
                free(vp as *mut libc::c_void);
            }
        }
    }
    make_transmatrix(&mut t.matrix, xoffset, yoffset, xscale, yscale, rotate);
    error
}
/* "page" and "pagebox" are not dimension nor transformation nor
 * something acceptable to put into here.
 * PLEASE DONT ADD HERE!
 */
unsafe fn spc_read_dimtrns_pdfm(
    mut spe: *mut spc_env,
    p: &mut transform_info,
    mut ap: *mut spc_arg,
) -> i32 {
    const _DTKEYS: [*const i8; 12] = [
        b"width\x00" as *const u8 as *const i8,
        b"height\x00" as *const u8 as *const i8,
        b"depth\x00" as *const u8 as *const i8,
        b"scale\x00" as *const u8 as *const i8,
        b"xscale\x00" as *const u8 as *const i8,
        b"yscale\x00" as *const u8 as *const i8,
        b"rotate\x00" as *const u8 as *const i8,
        b"bbox\x00" as *const u8 as *const i8,
        b"matrix\x00" as *const u8 as *const i8,
        b"clip\x00" as *const u8 as *const i8,
        b"hide\x00" as *const u8 as *const i8,
        0 as *const i8,
    ];
    let mut error: i32 = 0i32;
    let mut has_matrix = 0i32;
    let mut has_rotate = has_matrix;
    let mut has_scale = has_rotate; /* default: do clipping */
    let mut has_yscale = has_scale;
    let mut has_xscale = has_yscale;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    let mut rotate = 0.0f64;
    p.flags |= 1i32 << 3i32;
    p.flags &= !(1i32 << 4i32);
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    while error == 0 && (*ap).curptr < (*ap).endptr {
        let kp = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
        if kp.is_null() {
            break;
        }
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        let mut k = 0;
        while !_DTKEYS[k].is_null() && strcmp(_DTKEYS[k], kp) != 0 {
            k += 1
        }
        match k {
            0 => {
                error = spc_util_read_length(spe, &mut p.width, ap);
                p.flags |= 1i32 << 1i32
            }
            1 => {
                error = spc_util_read_length(spe, &mut p.height, ap);
                p.flags |= 1i32 << 2i32
            }
            2 => {
                error = spc_util_read_length(spe, &mut p.depth, ap);
                p.flags |= 1i32 << 2i32
            }
            3 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    yscale = atof(vp);
                    xscale = yscale;
                    has_scale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            4 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    xscale = atof(vp);
                    has_xscale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            5 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    yscale = atof(vp);
                    has_yscale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            6 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    rotate = 3.14159265358979323846f64 * atof(vp) / 180.0f64;
                    has_rotate = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            7 => {
                let mut v: [f64; 4] = [0.; 4];
                if spc_util_read_numbers(v.as_mut_ptr(), 4i32, ap) != 4i32 {
                    error = -1i32
                } else {
                    p.bbox = Rect::new((v[0], v[1]), (v[2], v[3]));
                    p.flags |= 1i32 << 0i32
                }
            }
            8 => {
                let mut v_0: [f64; 6] = [0.; 6];
                if spc_util_read_numbers(v_0.as_mut_ptr(), 6i32, ap) != 6i32 {
                    error = -1i32
                } else {
                    p.matrix.a = v_0[0];
                    p.matrix.b = v_0[1];
                    p.matrix.c = v_0[2];
                    p.matrix.d = v_0[3];
                    p.matrix.e = v_0[4];
                    p.matrix.f = v_0[5];
                    has_matrix = 1i32
                }
            }
            9 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    if atof(vp) != 0. {
                        p.flags |= 1i32 << 3i32
                    } else {
                        p.flags &= !(1i32 << 3i32)
                    }
                    free(vp as *mut libc::c_void);
                }
            }
            10 => p.flags |= 1i32 << 4i32,
            _ => error = -1i32,
        }
        if error != 0 {
            spc_warn!(
                spe,
                "Unrecognized key or invalid value for dimension/transformation: {}",
                CStr::from_ptr(kp).display(),
            );
        } else {
            skip_blank(&mut (*ap).curptr, (*ap).endptr);
        }
        free(kp as *mut libc::c_void);
    }
    if error == 0 {
        /* Check consistency */
        if has_xscale != 0 && p.flags & 1i32 << 1i32 != 0 {
            spc_warn!(spe, "Can\'t supply both width and xscale. Ignore xscale.");
            xscale = 1.0f64
        } else if has_yscale != 0 && p.flags & 1i32 << 2i32 != 0 {
            spc_warn!(
                spe,
                "Can\'t supply both height/depth and yscale. Ignore yscale."
            );
            yscale = 1.0f64
        } else if has_scale != 0 && (has_xscale != 0 || has_yscale != 0) {
            spc_warn!(spe, "Can\'t supply overall scale along with axis scales.");
            error = -1i32
        } else if has_matrix != 0
            && (has_scale != 0 || has_xscale != 0 || has_yscale != 0 || has_rotate != 0)
        {
            spc_warn!(spe, "Can\'t supply transform matrix along with scales or rotate. Ignore scales and rotate.");
        }
    }
    if has_matrix == 0 {
        make_transmatrix(&mut p.matrix, 0.0f64, 0.0f64, xscale, yscale, rotate);
    }
    if p.flags & 1i32 << 0i32 == 0 {
        p.flags &= !(1i32 << 3i32)
        /* no clipping needed */
    }
    error
}
#[no_mangle]
pub unsafe extern "C" fn spc_util_read_dimtrns(
    mut spe: *mut spc_env,
    ti: &mut transform_info,
    mut args: *mut spc_arg,
    mut syntax: i32,
) -> i32 {
    if spe.is_null() || args.is_null() {
        return -1i32;
    }
    if syntax != 0 {
        return spc_read_dimtrns_dvips(spe, ti, args);
    } else {
        return spc_read_dimtrns_pdfm(spe, ti, args);
    };
}
/* syntax 1: ((rgb|cmyk|hsb|gray) colorvalues)|colorname
 * syntax 0: pdf_number|pdf_array
 *
 * This is for reading *single* color specification.
 */
#[no_mangle]
pub unsafe extern "C" fn spc_util_read_blahblah(
    mut spe: *mut spc_env,
    p: &mut transform_info,
    mut page_no: *mut i32,
    mut bbox_type: *mut i32,
    mut ap: *mut spc_arg,
) -> i32 {
    const _DTKEYS: [*const i8; 14] = [
        b"width\x00" as *const u8 as *const i8,
        b"height\x00" as *const u8 as *const i8,
        b"depth\x00" as *const u8 as *const i8,
        b"scale\x00" as *const u8 as *const i8,
        b"xscale\x00" as *const u8 as *const i8,
        b"yscale\x00" as *const u8 as *const i8,
        b"rotate\x00" as *const u8 as *const i8,
        b"bbox\x00" as *const u8 as *const i8,
        b"matrix\x00" as *const u8 as *const i8,
        b"clip\x00" as *const u8 as *const i8,
        b"hide\x00" as *const u8 as *const i8,
        b"page\x00" as *const u8 as *const i8,
        b"pagebox\x00" as *const u8 as *const i8,
        0 as *const i8,
    ];
    let mut error: i32 = 0i32;
    let mut has_matrix = 0i32; /* default: do clipping */
    let mut has_rotate = has_matrix;
    let mut has_scale = has_rotate;
    let mut has_yscale = has_scale;
    let mut has_xscale = has_yscale;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    let mut rotate = 0.0f64;
    p.flags |= 1i32 << 3i32;
    p.flags &= !(1i32 << 4i32);
    skip_blank(&mut (*ap).curptr, (*ap).endptr);
    while error == 0 && (*ap).curptr < (*ap).endptr {
        let kp = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
        if kp.is_null() {
            break;
        }
        skip_blank(&mut (*ap).curptr, (*ap).endptr);
        let mut k = 0;
        while !_DTKEYS[k].is_null() && strcmp(_DTKEYS[k], kp) != 0 {
            k += 1
        }
        match k {
            0 => {
                error = spc_util_read_length(spe, &mut p.width, ap);
                p.flags |= 1i32 << 1i32
            }
            1 => {
                error = spc_util_read_length(spe, &mut p.height, ap);
                p.flags |= 1i32 << 2i32
            }
            2 => {
                error = spc_util_read_length(spe, &mut p.depth, ap);
                p.flags |= 1i32 << 2i32
            }
            3 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    yscale = atof(vp);
                    xscale = yscale;
                    has_scale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            4 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    xscale = atof(vp);
                    has_xscale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            5 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    yscale = atof(vp);
                    has_yscale = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            6 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    rotate = 3.14159265358979323846f64 * atof(vp) / 180.0f64;
                    has_rotate = 1i32;
                    free(vp as *mut libc::c_void);
                }
            }
            7 => {
                let mut v: [f64; 4] = [0.; 4];
                if spc_util_read_numbers(v.as_mut_ptr(), 4i32, ap) != 4i32 {
                    error = -1i32
                } else {
                    p.bbox = Rect::new((v[0], v[1]), (v[2], v[3]));
                    p.flags |= 1i32 << 0i32
                }
            }
            8 => {
                let mut v_0: [f64; 6] = [0.; 6];
                if spc_util_read_numbers(v_0.as_mut_ptr(), 6i32, ap) != 6i32 {
                    error = -1i32
                } else {
                    p.matrix.a = v_0[0];
                    p.matrix.b = v_0[1];
                    p.matrix.c = v_0[2];
                    p.matrix.d = v_0[3];
                    p.matrix.e = v_0[4];
                    p.matrix.f = v_0[5];
                    has_matrix = 1i32
                }
            }
            9 => {
                let vp = parse_float_decimal(&mut (*ap).curptr, (*ap).endptr);
                if vp.is_null() {
                    error = -1i32
                } else {
                    if atof(vp) != 0. {
                        p.flags |= 1i32 << 3i32
                    } else {
                        p.flags &= !(1i32 << 3i32)
                    }
                    free(vp as *mut libc::c_void);
                }
            }
            11 => {
                let mut page: f64 = 0.;
                if !page_no.is_null() && spc_util_read_numbers(&mut page, 1i32, ap) == 1i32 {
                    *page_no = page as i32
                } else {
                    error = -1i32
                }
            }
            10 => p.flags |= 1i32 << 4i32,
            12 => {
                let q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
                if !q.is_null() {
                    if !bbox_type.is_null() {
                        if strcasecmp(q, b"cropbox\x00" as *const u8 as *const i8) == 0i32 {
                            *bbox_type = 1i32
                        } else if strcasecmp(q, b"mediabox\x00" as *const u8 as *const i8) == 0i32 {
                            *bbox_type = 2i32
                        } else if strcasecmp(q, b"artbox\x00" as *const u8 as *const i8) == 0i32 {
                            *bbox_type = 3i32
                        } else if strcasecmp(q, b"trimbox\x00" as *const u8 as *const i8) == 0i32 {
                            *bbox_type = 4i32
                        } else if strcasecmp(q, b"bleedbox\x00" as *const u8 as *const i8) == 0i32 {
                            *bbox_type = 5i32
                        }
                    }
                    free(q as *mut libc::c_void);
                } else if !bbox_type.is_null() {
                    *bbox_type = 0i32
                }
            }
            _ => error = -1i32,
        }
        if error != 0 {
            spc_warn!(
                spe,
                "Unrecognized key or invalid value for dimension/transformation: {}",
                CStr::from_ptr(kp).display(),
            );
        } else {
            skip_blank(&mut (*ap).curptr, (*ap).endptr);
        }
        free(kp as *mut libc::c_void);
    }
    if error == 0 {
        /* Check consistency */
        if has_xscale != 0 && p.flags & 1i32 << 1i32 != 0 {
            spc_warn!(spe, "Can\'t supply both width and xscale. Ignore xscale.");
            xscale = 1.0f64
        } else if has_yscale != 0 && p.flags & 1i32 << 2i32 != 0 {
            spc_warn!(
                spe,
                "Can\'t supply both height/depth and yscale. Ignore yscale."
            );
            yscale = 1.0f64
        } else if has_scale != 0 && (has_xscale != 0 || has_yscale != 0) {
            spc_warn!(spe, "Can\'t supply overall scale along with axis scales.");
            error = -1i32
        } else if has_matrix != 0
            && (has_scale != 0 || has_xscale != 0 || has_yscale != 0 || has_rotate != 0)
        {
            spc_warn!(spe, "Can\'t supply transform matrix along with scales or rotate. Ignore scales and rotate.");
        }
    }
    if has_matrix == 0 {
        make_transmatrix(&mut p.matrix, 0.0f64, 0.0f64, xscale, yscale, rotate);
    }
    if p.flags & 1i32 << 0i32 == 0 {
        p.flags &= !(1i32 << 3i32)
        /* no clipping needed */
    }
    error
}

/* Color names */
struct Colordef {
    key: &'static str,
    color: PdfColor,
}

impl Colordef {
    const fn new(key: &'static str, color: PdfColor) -> Self {
        Colordef { key, color }
    }
}

const COLORDEFS: [Colordef; 68] = [
    Colordef::new("GreenYellow", PdfColor::Cmyk(0.15, 0.0, 0.69, 0.0)),
    Colordef::new("Yellow", PdfColor::Cmyk(0.0, 0.0, 1.0, 0.0)),
    Colordef::new("Goldenrod", PdfColor::Cmyk(0.0, 0.1, 0.84, 0.0)),
    Colordef::new("Dandelion", PdfColor::Cmyk(0.0, 0.29, 0.84, 0.0)),
    Colordef::new("Apricot", PdfColor::Cmyk(0.0, 0.32, 0.52, 0.0)),
    Colordef::new("Peach", PdfColor::Cmyk(0.0, 0.5, 0.7, 0.0)),
    Colordef::new("Melon", PdfColor::Cmyk(0.0, 0.46, 0.5, 0.0)),
    Colordef::new("YellowOrange", PdfColor::Cmyk(0.0, 0.42, 1.0, 0.0)),
    Colordef::new("Orange", PdfColor::Cmyk(0.0, 0.61, 0.87, 0.0)),
    Colordef::new("BurntOrange", PdfColor::Cmyk(0.0, 0.51, 1.0, 0.0)),
    Colordef::new("Bittersweet", PdfColor::Cmyk(0.0, 0.75, 1.0, 0.24)),
    Colordef::new("RedOrange", PdfColor::Cmyk(0.0, 0.77, 0.87, 0.0)),
    Colordef::new("Mahogany", PdfColor::Cmyk(0.0, 0.85, 0.87, 0.35)),
    Colordef::new("Maroon", PdfColor::Cmyk(0.0, 0.87, 0.68, 0.32)),
    Colordef::new("BrickRed", PdfColor::Cmyk(0.0, 0.89, 0.94, 0.28)),
    Colordef::new("Red", PdfColor::Cmyk(0.0, 1.0, 1.0, 0.0)),
    Colordef::new("OrangeRed", PdfColor::Cmyk(0.0, 1.0, 0.5, 0.0)),
    Colordef::new("RubineRed", PdfColor::Cmyk(0.0, 1.0, 0.13, 0.0)),
    Colordef::new("WildStrawberry", PdfColor::Cmyk(0.0, 0.96, 0.39, 0.0)),
    Colordef::new("Salmon", PdfColor::Cmyk(0.0, 0.53, 0.38, 0.0)),
    Colordef::new("CarnationPink", PdfColor::Cmyk(0.0, 0.63, 0.0, 0.0)),
    Colordef::new("Magenta", PdfColor::Cmyk(0.0, 1.0, 0.0, 0.0)),
    Colordef::new("VioletRed", PdfColor::Cmyk(0.0, 0.81, 0.0, 0.0)),
    Colordef::new("Rhodamine", PdfColor::Cmyk(0.0, 0.82, 0.0, 0.0)),
    Colordef::new("Mulberry", PdfColor::Cmyk(0.34, 0.90, 0.0, 0.02)),
    Colordef::new("RedViolet", PdfColor::Cmyk(0.07, 0.9, 0.0, 0.34)),
    Colordef::new("Fuchsia", PdfColor::Cmyk(0.47, 0.91, 0.0, 0.08)),
    Colordef::new("Lavender", PdfColor::Cmyk(0.0, 0.48, 0.0, 0.0)),
    Colordef::new("Thistle", PdfColor::Cmyk(0.12, 0.59, 0.0, 0.0)),
    Colordef::new("Orchid", PdfColor::Cmyk(0.32, 0.64, 0.0, 0.0)),
    Colordef::new("DarkOrchid", PdfColor::Cmyk(0.4, 0.8, 0.2, 0.0)),
    Colordef::new("Purple", PdfColor::Cmyk(0.45, 0.86, 0.0, 0.0)),
    Colordef::new("Plum", PdfColor::Cmyk(0.50, 1.0, 0.0, 0.0)),
    Colordef::new("Violet", PdfColor::Cmyk(0.79, 0.88, 0.0, 0.0)),
    Colordef::new("RoyalPurple", PdfColor::Cmyk(0.75, 0.9, 0.0, 0.0)),
    Colordef::new("BlueViolet", PdfColor::Cmyk(0.86, 0.91, 0.0, 0.04)),
    Colordef::new("Periwinkle", PdfColor::Cmyk(0.57, 0.55, 0.0, 0.0)),
    Colordef::new("CadetBlue", PdfColor::Cmyk(0.62, 0.57, 0.23, 0.0)),
    Colordef::new("CornflowerBlue", PdfColor::Cmyk(0.65, 0.13, 0.0, 0.0)),
    Colordef::new("MidnightBlue", PdfColor::Cmyk(0.98, 0.13, 0.0, 0.43)),
    Colordef::new("NavyBlue", PdfColor::Cmyk(0.94, 0.54, 0.0, 0.0)),
    Colordef::new("RoyalBlue", PdfColor::Cmyk(1.0, 0.5, 0.0, 0.0)),
    Colordef::new("Blue", PdfColor::Cmyk(1.0, 1.0, 0.0, 0.0)),
    Colordef::new("Cerulean", PdfColor::Cmyk(0.94, 0.11, 0.0, 0.0)),
    Colordef::new("Cyan", PdfColor::Cmyk(1.0, 0.0, 0.0, 0.0)),
    Colordef::new("ProcessBlue", PdfColor::Cmyk(0.96, 0.0, 0.0, 0.0)),
    Colordef::new("SkyBlue", PdfColor::Cmyk(0.62, 0.0, 0.12, 0.0)),
    Colordef::new("Turquoise", PdfColor::Cmyk(0.85, 0.0, 0.20, 0.0)),
    Colordef::new("TealBlue", PdfColor::Cmyk(0.86, 0.0, 0.34, 0.02)),
    Colordef::new("Aquamarine", PdfColor::Cmyk(0.82, 0.0, 0.3, 0.0)),
    Colordef::new("BlueGreen", PdfColor::Cmyk(0.85, 0.0, 0.33, 0.0)),
    Colordef::new("Emerald", PdfColor::Cmyk(1.0, 0.0, 0.5, 0.0)),
    Colordef::new("JungleGreen", PdfColor::Cmyk(0.99, 0.0, 0.52, 0.0)),
    Colordef::new("SeaGreen", PdfColor::Cmyk(0.69, 0.0, 0.5, 0.0)),
    Colordef::new("Green", PdfColor::Cmyk(1.0, 0.0, 1.0, 0.00f64)),
    Colordef::new("ForestGreen", PdfColor::Cmyk(0.91, 0.0, 0.88, 0.12)),
    Colordef::new("PineGreen", PdfColor::Cmyk(0.92, 0.0, 0.59, 0.25)),
    Colordef::new("LimeGreen", PdfColor::Cmyk(0.5, 0.0, 1.0, 0.0)),
    Colordef::new("YellowGreen", PdfColor::Cmyk(0.44, 0.0, 0.74, 0.0)),
    Colordef::new("SpringGreen", PdfColor::Cmyk(0.26, 0.0, 0.76, 0.0)),
    Colordef::new("OliveGreen", PdfColor::Cmyk(0.64, 0.0, 0.95, 0.40)),
    Colordef::new("RawSienna", PdfColor::Cmyk(0.0, 0.72, 1.0, 0.45)),
    Colordef::new("Sepia", PdfColor::Cmyk(0.0, 0.83, 1.0, 0.7)),
    Colordef::new("Brown", PdfColor::Cmyk(0.0, 0.81, 1.0, 0.6)),
    Colordef::new("Tan", PdfColor::Cmyk(0.14, 0.42, 0.56, 0.0)),
    Colordef::new("Gray", PdfColor::Gray(0.5)),
    Colordef::new("Black", PdfColor::Gray(0.0)),
    Colordef::new("White", PdfColor::Gray(1.0)),
];

/* From pdfcolor.c */
unsafe fn pdf_color_namedcolor(name: &str) -> Option<PdfColor> {
    COLORDEFS
        .as_ref()
        .iter()
        .find(|&colordef| colordef.key == name)
        .map(|colordef| colordef.color.clone())
}
