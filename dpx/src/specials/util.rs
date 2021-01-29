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

use euclid::point2;

use super::{SpcArg, SpcEnv};
use crate::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use crate::dpx_pdfcolor::{PdfColor, RgbPdfColor};
use crate::dpx_pdfdev::{transform_info, Rect, TMatrix};
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;
use crate::SkipBlank;
use arrayvec::ArrayVec;
use std::convert::TryInto;
use std::f64::consts::PI;

/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

/// Read numbers from [`SpcArg`]. The maximum amount read depends on the
/// capacity of the provided [`Array`] type.
pub(crate) fn read_numbers<T: arrayvec::Array<Item = f64>>(args: &mut SpcArg) -> ArrayVec<T> {
    args.cur.skip_blank();
    let mut vec = ArrayVec::new();
    let max_values = vec.capacity();
    for _ in 0..max_values {
        if args.cur.is_empty() {
            break;
        } else if let Some(q) = args.cur.parse_float_decimal_to_f64() {
            vec.push(q);
            args.cur.skip_blank();
        } else {
            break;
        }
    }
    vec
}

fn spc_read_color_color(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<PdfColor, ()> {
    let result: Result<PdfColor, ()>;
    if let Some(q) = ap.cur.parse_c_ident() {
        ap.cur.skip_blank();
        match q.as_str() {
            "rgb" => {
                /* Handle rgb color */
                let cv = read_numbers::<[_; 3]>(ap);
                if cv.len() != 3 {
                    spc_warn!(spe, "Invalid value for RGB color specification.");
                    result = Err(())
                } else {
                    result = PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn())
                }
            }
            "cmyk" => {
                /* Handle cmyk color */
                let cv = read_numbers::<[_; 4]>(ap);
                if cv.len() != 4 {
                    spc_warn!(spe, "Invalid value for CMYK color specification.");
                    result = Err(())
                } else {
                    result =
                        PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn())
                }
            }
            "gray" => {
                /* Handle gray */
                let cv = read_numbers::<[_; 1]>(ap);
                if cv.len() != 1 {
                    spc_warn!(spe, "Invalid value for gray color specification.");
                    result = Err(())
                } else {
                    result = PdfColor::from_gray(cv[0]).map_err(|err| err.warn())
                }
            }
            "spot" => {
                /* Handle spot colors */
                if let Some(color_name) = ap.cur.parse_c_ident() {
                    /* Must be a "named" color */
                    ap.cur.skip_blank();
                    let cv = read_numbers::<[_; 1]>(ap);
                    if cv.len() != 1 {
                        spc_warn!(spe, "Invalid value for spot color specification.");
                        result = Err(());
                    } else {
                        result = PdfColor::from_spot(&color_name, cv[0]).map_err(|err| err.warn())
                    }
                } else {
                    spc_warn!(spe, "No valid spot color name specified?");
                    return Err(());
                }
            }
            "hsb" => {
                let cv = read_numbers::<[_; 3]>(ap);
                if cv.len() != 3 {
                    spc_warn!(spe, "Invalid value for HSB color specification.");
                    result = Err(());
                } else {
                    let color = RgbPdfColor::from_hsv(cv[0], cv[1], cv[2]);
                    spc_warn!(
                        spe,
                        "HSB color converted to RGB: hsb: <{}, {}, {}> ==> rgb: <{}, {}, {}>",
                        cv[0],
                        cv[1],
                        cv[2],
                        color.r(),
                        color.g(),
                        color.b()
                    );
                    result = Ok(color.into());
                }
            }
            _ => {
                result = PdfColor::named(&q).ok_or(());
                if result.is_err() {
                    spc_warn!(spe, "Unrecognized color name: {}", q);
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
fn spc_read_color_pdf(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<PdfColor, ()> {
    let mut isarry: bool = false;
    ap.cur.skip_blank();
    if ap.cur[0] == b'[' {
        ap.cur = &ap.cur[1..];
        ap.cur.skip_blank();
        isarry = true
    }
    let cv = read_numbers::<[_; 4]>(ap);
    let mut result = match cv.len() {
        1 => PdfColor::from_gray(cv[0]).map_err(|err| err.warn()),
        3 => PdfColor::from_rgb(cv[0], cv[1], cv[2]).map_err(|err| err.warn()),
        4 => PdfColor::from_cmyk(cv[0], cv[1], cv[2], cv[3]).map_err(|err| err.warn()),
        _ => {
            /* Try to read the color names defined in dvipsname.def */
            if let Some(q) = ap.cur.parse_c_ident() {
                let result = PdfColor::named(&q).ok_or(());
                if result.is_err() {
                    spc_warn!(
                        spe,
                        "Unrecognized color name: {}, keep the current color",
                        q,
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
        ap.cur.skip_blank();
        if ap.cur.is_empty() || ap.cur[0] != b']' {
            spc_warn!(spe, "Unbalanced \'[\' and \']\' in color specification.");
            result = Err(())
        } else {
            ap.cur = &ap.cur[1..];
        }
    }
    result
}
/* This is for reading *single* color specification. */

pub(crate) fn spc_util_read_colorspec(
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
    syntax: bool,
) -> Result<PdfColor, ()> {
    ap.cur.skip_blank();
    if ap.cur.is_empty() {
        Err(())
    } else if syntax {
        spc_read_color_color(spe, ap)
    } else {
        spc_read_color_pdf(spe, ap)
    }
}

pub(crate) fn spc_util_read_pdfcolor(
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
    defaultcolor: Option<&PdfColor>,
) -> Result<PdfColor, ()> {
    ap.cur.skip_blank();
    if ap.cur.is_empty() {
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

pub(crate) trait ReadLengthSpc {
    fn read_length(&mut self, spe: &SpcEnv) -> Result<f64, ()>;
}

impl ReadLengthSpc for &[u8] {
    fn read_length(&mut self, spe: &SpcEnv) -> Result<f64, ()> {
        let mut p = *self; /* inverse magnify */
        let mut u: f64 = 1.0f64;
        let mut error: i32 = 0;
        let v = p.parse_float_decimal_to_f64();
        if v.is_none() {
            *self = p;
            return Err(());
        }
        let v = v.unwrap();
        p.skip_white();
        if let Some(q) = p.parse_c_ident() {
            let mut bytes = q.as_str();
            if bytes.starts_with("true") {
                u /= if spe.mag != 0. { spe.mag } else { 1. };
                bytes = &bytes["true".len()..];
            }
            let q = if bytes.is_empty() {
                // TODO: check
                /* "true" was a separate word from the units */
                p.skip_white();
                p.parse_c_ident()
            } else {
                Some(String::from(bytes))
            };
            if let Some(ident) = q {
                match ident.as_str() {
                    "pt" => u *= 72. / 72.27,
                    "in" => u *= 72.,
                    "cm" => u *= 72. / 2.54,
                    "mm" => u *= 72. / 25.4,
                    "bp" => u *= 1.,
                    "pc" => u *= 12. * 72. / 72.27,
                    "dd" => u *= 1238. / 1157. * 72. / 72.27,
                    "cc" => u *= 12. * 1238. / 1157. * 72. / 72.27,
                    "sp" => u *= 72. / (72.27 * 65536.),
                    _ => {
                        spc_warn!(spe, "Unknown unit of measure: {}", ident);
                        error = -1;
                    }
                }
            } else {
                spc_warn!(spe, "Missing unit of measure after \"true\"");
                error = -1
            }
        }
        *self = p;
        if error == 0 {
            Ok(v * u)
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
fn make_transmatrix(
    m: &mut TMatrix,
    xoffset: f64,
    yoffset: f64,
    xscale: f64,
    yscale: f64,
    rotate: f64,
) {
    let (s, c) = rotate.sin_cos();
    *m = TMatrix::row_major(
        xscale * c,
        xscale * s,
        -yscale * s,
        yscale * c,
        xoffset,
        yoffset,
    );
}

fn spc_read_dimtrns_dvips(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<transform_info, ()> {
    let mut t = transform_info::new();
    const _DTKEYS: [&str; 14] = [
        "hoffset", "voffset", "hsize", "vsize", "hscale", "vscale", "angle", "clip", "llx", "lly",
        "urx", "ury", "rwi", "rhi",
    ];
    let mut error: i32 = 0;
    let mut rotate = 0.0f64;
    let mut yoffset = rotate;
    let mut xoffset = yoffset;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    ap.cur.skip_blank();
    while error == 0 && !ap.cur.is_empty() {
        if let Some(kp) = ap.cur.parse_c_ident() {
            let mut k = 0;
            for &key in &_DTKEYS {
                if kp == key {
                    break;
                }
                k += 1;
            }
            if k == 14 {
                spc_warn!(spe, "Unrecognized dimension/transformation key: {}", kp);
                error = -1;
                break;
            } else {
                ap.cur.skip_blank();
                if k == 7 {
                    t.flags |= 1 << 3;
                /* not key-value */
                } else {
                    if !ap.cur.is_empty() && ap.cur[0] == b'=' {
                        ap.cur = &ap.cur[1..];
                        ap.cur.skip_blank();
                    }
                    let vp = if ap.cur[0] == b'\'' || ap.cur[0] == b'\"' {
                        let qchr = ap.cur[0];
                        ap.cur = &ap.cur[1..];
                        ap.cur.skip_blank();
                        let mut vp = ap.cur.parse_float_decimal();
                        ap.cur.skip_blank();
                        if vp.is_some() && qchr != ap.cur[0] {
                            spc_warn!(
                                spe,
                                "Syntax error in dimension/transformation specification."
                            );
                            error = -1;
                            vp = None;
                        }
                        ap.cur = &ap.cur[1..];
                        vp
                    } else {
                        ap.cur.parse_float_decimal()
                    };
                    if error == 0 && vp.is_none() {
                        spc_warn!(spe, "Missing value for dimension/transformation: {}", kp);
                        error = -1;
                    }
                    if error != 0 {
                        break;
                    }
                    if let Some(vp) = vp.map(|vp| vp.to_str().unwrap().parse().unwrap()) {
                        match k {
                            0 => xoffset = vp,
                            1 => yoffset = vp,
                            2 => {
                                t.width = vp;
                                t.flags |= 1 << 1;
                            }
                            3 => {
                                t.height = vp;
                                t.flags |= 1 << 2;
                            }
                            4 => xscale = vp / 100.,
                            5 => yscale = vp / 100.,
                            6 => rotate = std::f64::consts::PI * vp / 180.,
                            8 => {
                                t.bbox.min.x = vp;
                                t.flags |= 1 << 0;
                            }
                            9 => {
                                t.bbox.min.y = vp;
                                t.flags |= 1 << 0;
                            }
                            10 => {
                                t.bbox.max.x = vp;
                                t.flags |= 1 << 0;
                            }
                            11 => {
                                t.bbox.max.y = vp;
                                t.flags |= 1 << 0;
                            }
                            12 => {
                                t.width = vp / 10.;
                                t.flags |= 1 << 1;
                            }
                            13 => {
                                t.height = vp / 10.;
                                t.flags |= 1 << 2;
                            }
                            _ => {}
                        }
                        ap.cur.skip_blank();
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
    if error == 0 {
        Ok(t)
    } else {
        Err(())
    }
}
/* "page" and "pagebox" are not dimension nor transformation nor
 * something acceptable to put into here.
 * PLEASE DONT ADD HERE!
 */
fn spc_read_dimtrns_pdfm(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<transform_info, ()> {
    let mut p = transform_info::new();
    let mut error: i32 = 0;
    let mut has_matrix = false;
    let mut has_rotate = has_matrix;
    let mut has_scale = has_rotate; /* default: do clipping */
    let mut has_yscale = has_scale;
    let mut has_xscale = has_yscale;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    let mut rotate = 0.0f64;
    p.flags |= 1 << 3;
    p.flags &= !(1 << 4);
    ap.cur.skip_blank();
    while error == 0 && !ap.cur.is_empty() {
        if let Some(kp) = ap.cur.parse_c_ident() {
            ap.cur.skip_blank();
            match kp.as_str() {
                "width" => {
                    if let Ok(width) = ap.cur.read_length(&*spe) {
                        p.width = width;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 1;
                }
                "height" => {
                    if let Ok(height) = ap.cur.read_length(&*spe) {
                        p.height = height;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 2;
                }
                "depth" => {
                    if let Ok(depth) = ap.cur.read_length(&*spe) {
                        p.depth = depth;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 2;
                }
                "scale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        yscale = vp;
                        xscale = yscale;
                        has_scale = true;
                    } else {
                        error = -1;
                    }
                }
                "xscale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        xscale = vp;
                        has_xscale = true;
                    } else {
                        error = -1;
                    }
                }
                "yscale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        yscale = vp;
                        has_yscale = true;
                    } else {
                        error = -1;
                    }
                }
                "rotate" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        rotate = PI * vp / 180.0f64;
                        has_rotate = true;
                    } else {
                        error = -1;
                    }
                }
                "bbox" => {
                    let v: ArrayVec<[_; 4]> = read_numbers(ap);
                    if v.len() != 4 {
                        error = -1;
                    } else {
                        p.bbox = Rect::new(point2(v[0], v[1]), point2(v[2], v[3]));
                        p.flags |= 1 << 0;
                    }
                }
                "matrix" => {
                    let v_0: ArrayVec<[_; 6]> = read_numbers(ap);
                    if v_0.len() != 6 {
                        error = -1;
                    } else {
                        let v_0 = v_0.as_slice().try_into().unwrap();
                        p.matrix = TMatrix::from_row_major_array(v_0);
                        has_matrix = true;
                    }
                }
                "clip" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        if vp != 0. {
                            p.flags |= 1 << 3;
                        } else {
                            p.flags &= !(1 << 3)
                        }
                    } else {
                        error = -1;
                    }
                }
                "hide" => p.flags |= 1 << 4,
                _ => error = -1,
            }
            if error != 0 {
                spc_warn!(
                    spe,
                    "Unrecognized key or invalid value for dimension/transformation: {}",
                    kp,
                );
            } else {
                ap.cur.skip_blank();
            }
        } else {
            break;
        }
    }
    if error == 0 {
        /* Check consistency */
        if has_xscale && p.flags & 1 << 1 != 0 {
            spc_warn!(spe, "Can\'t supply both width and xscale. Ignore xscale.");
            xscale = 1.0f64
        } else if has_yscale && p.flags & 1 << 2 != 0 {
            spc_warn!(
                spe,
                "Can\'t supply both height/depth and yscale. Ignore yscale."
            );
            yscale = 1.0f64
        } else if has_scale && (has_xscale || has_yscale) {
            spc_warn!(spe, "Can\'t supply overall scale along with axis scales.");
            error = -1
        } else if has_matrix && (has_scale || has_xscale || has_yscale || has_rotate) {
            spc_warn!(spe, "Can\'t supply transform matrix along with scales or rotate. Ignore scales and rotate.");
        }
    }
    if !has_matrix {
        make_transmatrix(&mut p.matrix, 0.0, 0.0, xscale, yscale, rotate);
    }
    if p.flags & 1 << 0 == 0 {
        p.flags &= !(1 << 3)
        /* no clipping needed */
    }
    if error == 0 {
        Ok(p)
    } else {
        Err(())
    }
}

pub(crate) fn spc_util_read_dimtrns(
    spe: &mut SpcEnv,
    args: &mut SpcArg,
    syntax: i32,
) -> Result<transform_info, ()> {
    if syntax != 0 {
        spc_read_dimtrns_dvips(spe, args)
    } else {
        spc_read_dimtrns_pdfm(spe, args)
    }
}
/* syntax 1: ((rgb|cmyk|hsb|gray) colorvalues)|colorname
 * syntax 0: pdf_number|pdf_array
 *
 * This is for reading *single* color specification.
 */

pub(crate) fn spc_util_read_blahblah(
    spe: &mut SpcEnv,
    p: &mut transform_info,
    page_no: &mut Option<i32>,
    bbox_type: &mut Option<i32>,
    ap: &mut SpcArg,
) -> i32 {
    let mut error: i32 = 0;
    let mut has_matrix = false; /* default: do clipping */
    let mut has_rotate = has_matrix;
    let mut has_scale = has_rotate;
    let mut has_yscale = has_scale;
    let mut has_xscale = has_yscale;
    let mut yscale = 1.0f64;
    let mut xscale = yscale;
    let mut rotate = 0.0f64;
    p.flags |= 1 << 3;
    p.flags &= !(1 << 4);
    ap.cur.skip_blank();
    while error == 0 && !ap.cur.is_empty() {
        if let Some(kp) = ap.cur.parse_c_ident() {
            ap.cur.skip_blank();
            match kp.as_str() {
                "width" => {
                    if let Ok(width) = ap.cur.read_length(spe) {
                        p.width = width;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 1
                }
                "height" => {
                    if let Ok(height) = ap.cur.read_length(spe) {
                        p.height = height;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 2
                }
                "depth" => {
                    if let Ok(depth) = ap.cur.read_length(spe) {
                        p.depth = depth;
                    } else {
                        error = -1;
                    }
                    p.flags |= 1 << 2
                }
                "scale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        yscale = vp;
                        xscale = yscale;
                        has_scale = true;
                    } else {
                        error = -1
                    }
                }
                "xscale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        xscale = vp;
                        has_xscale = true;
                    } else {
                        error = -1
                    }
                }
                "yscale" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        yscale = vp;
                        has_yscale = true;
                    } else {
                        error = -1
                    }
                }
                "rotate" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        rotate = PI * vp / 180.0f64;
                        has_rotate = true;
                    } else {
                        error = -1
                    }
                }
                "bbox" => {
                    let v: ArrayVec<[_; 4]> = read_numbers(ap);
                    if v.len() != 4 {
                        error = -1
                    } else {
                        p.bbox = Rect::new(point2(v[0], v[1]), point2(v[2], v[3]));
                        p.flags |= 1 << 0
                    }
                }
                "matrix" => {
                    let v_0: ArrayVec<[_; 6]> = read_numbers(ap);
                    if v_0.len() != 6 {
                        error = -1
                    } else {
                        let v_0 = v_0.as_slice().try_into().unwrap();
                        p.matrix = TMatrix::from_row_major_array(v_0);
                        has_matrix = true
                    }
                }
                "clip" => {
                    if let Some(vp) = ap.cur.parse_float_decimal_to_f64() {
                        if vp != 0. {
                            p.flags |= 1 << 3
                        } else {
                            p.flags &= !(1 << 3)
                        }
                    } else {
                        error = -1
                    }
                }
                "page" => {
                    if let Some(page_no) = page_no.as_mut() {
                        let page: ArrayVec<[_; 1]> = read_numbers(ap);
                        if page.len() == 1 {
                            *page_no = page[0] as i32;
                        } else {
                            error = -1
                        }
                    } else {
                        error = -1;
                    }
                }
                "hide" => p.flags |= 1 << 4,
                "pagebox" => {
                    if let Some(q) = ap.cur.parse_c_ident() {
                        // TODO: Maybe remove is_some check
                        if bbox_type.is_some() {
                            // TODO: bbox_type can probably be an enum, use try_from
                            match q.to_ascii_lowercase().as_str() {
                                "cropbox" => *bbox_type = Some(1),
                                "mediabox" => *bbox_type = Some(2),
                                "artbox" => *bbox_type = Some(3),
                                "trimbox" => *bbox_type = Some(4),
                                "bleedbox" => *bbox_type = Some(5),
                                _ => {}
                            }
                        }
                    // TODO: Maybe remove is_some check
                    } else if bbox_type.is_some() {
                        *bbox_type = Some(0);
                    }
                }
                _ => error = -1,
            }
            if error != 0 {
                spc_warn!(
                    spe,
                    "Unrecognized key or invalid value for dimension/transformation: {}",
                    kp,
                );
            } else {
                ap.cur.skip_blank();
            }
        } else {
            break;
        }
    }
    if error == 0 {
        /* Check consistency */
        if has_xscale && p.flags & 1 << 1 != 0 {
            spc_warn!(spe, "Can\'t supply both width and xscale. Ignore xscale.");
            xscale = 1.0f64
        } else if has_yscale && p.flags & 1 << 2 != 0 {
            spc_warn!(
                spe,
                "Can\'t supply both height/depth and yscale. Ignore yscale."
            );
            yscale = 1.0f64
        } else if has_scale && (has_xscale || has_yscale) {
            spc_warn!(spe, "Can\'t supply overall scale along with axis scales.");
            error = -1
        } else if has_matrix && (has_scale || has_xscale || has_yscale || has_rotate) {
            spc_warn!(spe, "Can\'t supply transform matrix along with scales or rotate. Ignore scales and rotate.");
        }
    }
    if !has_matrix {
        make_transmatrix(&mut p.matrix, 0.0, 0.0, xscale, yscale, rotate);
    }
    if p.flags & 1 << 0 == 0 {
        p.flags &= !(1 << 3)
        /* no clipping needed */
    }
    error
}
