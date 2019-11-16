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

use crate::SkipBlank;
use super::{spc_arg, spc_env};
use crate::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use crate::dpx_pdfcolor::PdfColor;
use crate::dpx_pdfdev::{Rect, TMatrix, transform_info};
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;
use crate::DisplayExt;
use libc::{atof};
use std::ffi::CString;

/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

#[no_mangle]
pub unsafe extern "C" fn spc_util_read_numbers(
    mut values: *mut f64,
    mut num_values: i32,
    mut args: *mut spc_arg,
) -> i32 {
    (*args).cur.skip_blank();
    let mut count = 0;
    while count < num_values && !(*args).cur.is_empty() {
        if let Some(q) = (*args).cur.parse_float_decimal() {
            *values.offset(count as isize) = atof(q.as_ptr());
            (*args).cur.skip_blank();
            count += 1
        } else {
            break;
        }
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
    if let Some(q) = (*ap).cur.parse_c_ident() {
        (*ap).cur.skip_blank();
        match q.to_bytes() {
            b"rgb" => {
                /* Handle rgb color */
                let nc = spc_util_read_numbers(cv.as_mut_ptr(), 3i32, ap);
                if nc != 3i32 {
                    spc_warn!(spe, "Invalid value for RGB color specification.");
                    result = Err(())
                } else {
                    result = PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn())
                }
            },
            b"cmyk" => {
                /* Handle cmyk color */
                let nc = spc_util_read_numbers(cv.as_mut_ptr(), 4i32, ap);
                if nc != 4i32 {
                    spc_warn!(spe, "Invalid value for CMYK color specification.");
                    result = Err(())
                } else {
                    result = PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn())
                }
            },
            b"gray" => {
                /* Handle gray */
                let nc = spc_util_read_numbers(cv.as_mut_ptr(), 1i32, ap);
                if nc != 1i32 {
                    spc_warn!(spe, "Invalid value for gray color specification.");
                    result = Err(())
                } else {
                    result = PdfColor::from_gray(cv[0]).map_err(|err| err.warn())
                }
            },
            b"spot" => {
                /* Handle spot colors */
                if let Some(color_name) = (*ap).cur.parse_c_ident() { /* Must be a "named" color */
                    (*ap).cur.skip_blank();
                    let nc = spc_util_read_numbers(cv.as_mut_ptr(), 1, ap);
                    if nc != 1 {
                        spc_warn!(spe, "Invalid value for spot color specification.");
                        result = Err(());
                    } else {
                        result = PdfColor::from_spot(color_name, cv[0])
                            .map_err(|err| err.warn())
                    }
                } else {
                    spc_warn!(spe, "No valid spot color name specified?");
                    return Err(());
                }
            },
            b"hsb" => {
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
            },
            _ => {
                result = if let Ok(name) = q.to_str() {
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
                        q.display(),
                    );
                }
            }
        }
    } else {
        spc_warn!(spe, "No valid color specified?");
        return Err(());
    }
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
    (*ap).cur.skip_blank();
    if (*ap).cur[0] == b'[' {
        (*ap).cur = &(*ap).cur[1..];
        (*ap).cur.skip_blank();
        isarry = true
    }
    let nc = spc_util_read_numbers(cv.as_mut_ptr(), 4i32, ap);
    let mut result = match nc {
        1 => PdfColor::from_gray(cv[0]).map_err(|err| err.warn()),
        3 => PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn()),
        4 => PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn()),
        _ => {
            /* Try to read the color names defined in dvipsname.def */
            if let Some(q) = (*ap).cur.parse_c_ident() {
                let mut result = q
                    .to_str()
                    .ok()
                    .and_then(|name| pdf_color_namedcolor(name))
                    .ok_or(());
                if result.is_err() {
                    spc_warn!(
                        spe,
                        "Unrecognized color name: {}, keep the current color",
                        q.display(),
                    );
                }
                result
            } else {
                spc_warn!(spe, "No valid color specified?");
                return Err(());
            }
        }
    };
    if isarry {
        (*ap).cur.skip_blank();
        if (*ap).cur.is_empty() || (*ap).cur[0] != b']' {
            spc_warn!(spe, "Unbalanced \'[\' and \']\' in color specification.");
            result = Err(())
        } else {
            (*ap).cur = &(*ap).cur[1..];
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
    (*ap).cur.skip_blank();
    if (*ap).cur.is_empty() {
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
    (*ap).cur.skip_blank();
    if (*ap).cur.is_empty() {
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

pub trait ReadLengthSpc {
    fn read_length(&mut self, spe: &spc_env) -> Result<f64, ()>;
}
impl ReadLengthSpc for &[u8] {
    fn read_length(&mut self, spe: &spc_env) -> Result<f64, ()> {
        let mut p = *self; /* inverse magnify */
        let mut u: f64 = 1.0f64;
        let mut error: i32 = 0i32;
        let q = p.parse_float_decimal();
        if q.is_none() {
            *self = p;
            return Err(());
        }
        let v = unsafe { atof(q.unwrap().as_ptr()) };
        p.skip_white();
        if let Some(q) = p.parse_c_ident() {
            let mut bytes = q.to_bytes();
            if bytes.starts_with(b"true") {
                u /= if spe.mag != 0.0f64 { spe.mag } else { 1.0f64 };
                bytes = &bytes[b"true".len()..];
            }
            let q = if bytes.is_empty() { // TODO: check
                /* "true" was a separate word from the units */
                p.skip_white();
                p.parse_c_ident()
            } else {
                Some(CString::new(bytes).unwrap())
            };
            if let Some(ident) = q {
                match ident.to_bytes() {
                    b"pt" => u *= 72. / 72.27,
                    b"in" => u *= 72.,
                    b"cm" => u *= 72. / 2.54,
                    b"mm" => u *= 72. / 25.4,
                    b"bp" => u *= 1.,
                    b"pc" => u *= 12. * 72. / 72.27,
                    b"dd" => u *= 1238. / 1157. * 72. / 72.27,
                    b"cc" => u *= 12. * 1238. / 1157. * 72. / 72.27,
                    b"sp" => u *= 72. / (72.27 * 65536.),
                    _ => {
                        spc_warn!(spe, "Unknown unit of measure: {}", ident.display(),);
                        error = -1i32
                    }
                }
            } else {
                spc_warn!(spe, "Missing unit of measure after \"true\"");
                error = -1i32
            }
        }
        *self = p;
        if error == 0 {
            Ok( v * u )
        } else {
            Err(())
        }
    }
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
    const _DTKEYS: [&[u8]; 14] = [
        b"hoffset",
        b"voffset",
        b"hsize",
        b"vsize",
        b"hscale",
        b"vscale",
        b"angle",
        b"clip",
        b"llx",
        b"lly",
        b"urx",
        b"ury",
        b"rwi",
        b"rhi",
    ];
    let mut error: i32 = 0i32;
    let mut rotate = 0.0f64;
    let mut yoffset = rotate;
    let mut xoffset = yoffset;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    (*ap).cur.skip_blank();
    while error == 0 && !(*ap).cur.is_empty() {
        if let Some(kp) = (*ap).cur.parse_c_ident() {
            let mut k = 0;
            for &key in &_DTKEYS {
                if kp.to_bytes() == key {
                    break;
                }
                k += 1;
            }
            if k == 14 {
                spc_warn!(
                    spe,
                    "Unrecognized dimension/transformation key: {}",
                    kp.display(),
                );
                error = -1i32;
                break;
            } else {
                (*ap).cur.skip_blank();
                if k == 7 {
                    t.flags |= 1i32 << 3i32;
                /* not key-value */
                } else {
                    if !(*ap).cur.is_empty() && (*ap).cur[0] == b'=' {
                        (*ap).cur = &(*ap).cur[1..];
                        (*ap).cur.skip_blank();
                    }
                    let vp = if (*ap).cur[0] == b'\''
                        || (*ap).cur[0] == b'\"' {
                        let mut qchr = (*ap).cur[0];
                        (*ap).cur = &(*ap).cur[1..];
                        (*ap).cur.skip_blank();
                        let mut vp = (*ap).cur.parse_float_decimal();
                        (*ap).cur.skip_blank();
                        if vp.is_some() && qchr != (*ap).cur[0] {
                            spc_warn!(
                                spe,
                                "Syntax error in dimension/transformation specification."
                            );
                            error = -1i32;
                            vp = None;
                        }
                        (*ap).cur = &(*ap).cur[1..];
                        vp
                    } else {
                        (*ap).cur.parse_float_decimal()
                    };
                    if error == 0 && vp.is_none() {
                        spc_warn!(
                            spe,
                            "Missing value for dimension/transformation: {}",
                            kp.display(),
                        );
                        error = -1i32
                    }
                    if error != 0 {
                        break;
                    }
                    if let Some(vp) = vp {
                        let vp = vp.as_ptr();
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
                            6 => rotate = std::f64::consts::PI * atof(vp) / 180.0f64,
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
                        (*ap).cur.skip_blank();
                    } else {
                        break;
                    }
                }
            }
        } else {
            break;
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
    (*ap).cur.skip_blank();
    while error == 0 && !(*ap).cur.is_empty() {
        if let Some(kp) = (*ap).cur.parse_c_ident() {
            (*ap).cur.skip_blank();
            match kp.to_bytes() {
                b"width" => {
                    if let Ok(width) = (*ap).cur.read_length(&*spe) {
                        p.width = width;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 1i32
                }
                b"height" => {
                    if let Ok(height) = (*ap).cur.read_length(&*spe) {
                        p.height = height;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 2i32
                }
                b"depth" => {
                    if let Ok(depth) = (*ap).cur.read_length(&*spe) {
                        p.depth = depth;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 2i32
                }
                b"scale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        yscale = atof(vp.as_ptr());
                        xscale = yscale;
                        has_scale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"xscale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        xscale = atof(vp.as_ptr());
                        has_xscale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"yscale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        yscale = atof(vp.as_ptr());
                        has_yscale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"rotate" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        rotate = 3.14159265358979323846f64 * atof(vp.as_ptr()) / 180.0f64;
                        has_rotate = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"bbox" => {
                    let mut v: [f64; 4] = [0.; 4];
                    if spc_util_read_numbers(v.as_mut_ptr(), 4i32, ap) != 4i32 {
                        error = -1i32
                    } else {
                        p.bbox = Rect::new((v[0], v[1]), (v[2], v[3]));
                        p.flags |= 1i32 << 0i32
                    }
                }
                b"matrix" => {
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
                b"clip" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        if atof(vp.as_ptr()) != 0. {
                            p.flags |= 1i32 << 3i32
                        } else {
                            p.flags &= !(1i32 << 3i32)
                        }
                    } else {
                        error = -1i32
                    }
                }
                b"hide" => p.flags |= 1i32 << 4i32,
                _ => error = -1i32,
            }
            if error != 0 {
                spc_warn!(
                    spe,
                    "Unrecognized key or invalid value for dimension/transformation: {}",
                    kp.display(),
                );
            } else {
                (*ap).cur.skip_blank();
            }
        } else {
            break;
        }
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
    (*ap).cur.skip_blank();
    while error == 0 && !(*ap).cur.is_empty() {
        if let Some(kp) = (*ap).cur.parse_c_ident() {
            (*ap).cur.skip_blank();
            match kp.to_bytes() {
                b"width" => {
                    if let Ok(width) = (*ap).cur.read_length(&*spe) {
                        p.width = width;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 1i32
                }
                b"height" => {
                    if let Ok(height) = (*ap).cur.read_length(&*spe) {
                        p.height = height;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 2i32
                }
                b"depth" => {
                    if let Ok(depth) = (*ap).cur.read_length(&*spe) {
                        p.depth = depth;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1i32 << 2i32
                }
                b"scale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        yscale = atof(vp.as_ptr());
                        xscale = yscale;
                        has_scale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"xscale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        xscale = atof(vp.as_ptr());
                        has_xscale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"yscale" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        yscale = atof(vp.as_ptr());
                        has_yscale = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"rotate" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        rotate = 3.14159265358979323846f64 * atof(vp.as_ptr()) / 180.0f64;
                        has_rotate = 1i32;
                    } else {
                        error = -1i32
                    }
                }
                b"bbox" => {
                    let mut v: [f64; 4] = [0.; 4];
                    if spc_util_read_numbers(v.as_mut_ptr(), 4i32, ap) != 4i32 {
                        error = -1i32
                    } else {
                        p.bbox = Rect::new((v[0], v[1]), (v[2], v[3]));
                        p.flags |= 1i32 << 0i32
                    }
                }
                b"matrix" => {
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
                b"clip" => {
                    if let Some(vp) = (*ap).cur.parse_float_decimal() {
                        if atof(vp.as_ptr()) != 0. {
                            p.flags |= 1 << 3
                        } else {
                            p.flags &= !(1 << 3)
                        }
                    } else {
                        error = -1i32
                    }
                }
                b"page" => {
                    let mut page: f64 = 0.;
                    if !page_no.is_null() && spc_util_read_numbers(&mut page, 1i32, ap) == 1i32 {
                        *page_no = page as i32
                    } else {
                        error = -1i32
                    }
                }
                b"hide" => p.flags |= 1i32 << 4i32,
                b"pagebox" => {
                    if let Some(q) = (*ap).cur.parse_c_ident() {
                        if !bbox_type.is_null() {
                            match q.to_bytes().to_ascii_lowercase().as_slice() {
                                b"cropbox" => *bbox_type = 1,
                                b"mediabox" => *bbox_type = 2,
                                b"artbox" => *bbox_type = 3,
                                b"trimbox" => *bbox_type = 4,
                                b"bleedbox" => *bbox_type = 5,
                                _ => {},
                            }
                        }
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
                    kp.display(),
                );
            } else {
                (*ap).cur.skip_blank();
            }
        } else {
            break;
        }
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
