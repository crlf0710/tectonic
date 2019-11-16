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

use crate::DisplayExt;

use super::util::{spc_util_read_colorspec, spc_util_read_numbers};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_fontmap::{
    is_pdfm_mapline, pdf_append_fontmap_record, pdf_clear_fontmap_record, pdf_init_fontmap_record,
    pdf_insert_fontmap_record, pdf_load_fontmap_file, pdf_read_fontmap_line,
    pdf_remove_fontmap_record,
};
use crate::dpx_mem::{new, xrealloc};
use crate::dpx_mfileio::work_buffer_u8 as WORK_BUFFER;
use crate::dpx_pdfdev::{pdf_dev_reset_color, pdf_dev_reset_fonts};
use crate::dpx_pdfdoc::{
    pdf_doc_add_page_content, pdf_doc_set_bgcolor,
};
use crate::dpx_pdfdraw::{
    pdf_dev_concat, pdf_dev_get_fixed_point, pdf_dev_grestore, pdf_dev_gsave,
    pdf_dev_set_fixed_point,
};
use crate::dpx_pdfparse::{ParseIdent, SkipWhite};
use crate::shims::sprintf;
use crate::spc_warn;
use libc::{free};

use super::{spc_arg, spc_env};

use super::SpcHandler;
use crate::dpx_fontmap::fontmap_rec;

use crate::dpx_pdfdev::Coord;

use crate::dpx_pdfdev::TMatrix;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
#[no_mangle]
pub unsafe fn spc_handler_xtx_do_transform(
    mut x_user: f64,
    mut y_user: f64,
    mut a: f64,
    mut b: f64,
    mut c: f64,
    mut d: f64,
    mut e: f64,
    mut f: f64,
) -> i32 {
    let mut M = TMatrix::new();
    /* Create transformation matrix */
    M.a = a;
    M.b = b;
    M.c = c;
    M.d = d;
    M.e = (1.0f64 - M.a) * x_user - M.c * y_user + e;
    M.f = (1.0f64 - M.d) * y_user - M.b * x_user + f;
    pdf_dev_concat(&mut M);
    let pt = pdf_dev_get_fixed_point();
    pdf_dev_set_fixed_point(x_user - pt.x, y_user - pt.y);
    0i32
}
unsafe fn spc_handler_xtx_scale(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut values: [f64; 2] = [0.; 2];
    if spc_util_read_numbers(&mut *values.as_mut_ptr().offset(0), 2i32, args) < 2i32 {
        return -1i32;
    }
    (*args).cur = &[];
    return spc_handler_xtx_do_transform(
        (*spe).x_user,
        (*spe).y_user,
        values[0],
        0i32 as f64,
        0i32 as f64,
        values[1],
        0i32 as f64,
        0i32 as f64,
    );
}
/* Scaling without gsave/grestore. */
static mut SCALE_FACTORS: *mut Coord = std::ptr::null_mut();
static mut SCALE_FACTOR_COUNT: i32 = -1i32;
unsafe fn spc_handler_xtx_bscale(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut values: [f64; 2] = [0.; 2];
    SCALE_FACTOR_COUNT += 1;
    if SCALE_FACTOR_COUNT & 0xfi32 == 0 {
        SCALE_FACTORS = xrealloc(
            SCALE_FACTORS as *mut libc::c_void,
            ((SCALE_FACTOR_COUNT + 16i32) as u64)
                .wrapping_mul(::std::mem::size_of::<Coord>() as u64),
        ) as *mut Coord
    }
    if spc_util_read_numbers(&mut *values.as_mut_ptr().offset(0), 2i32, args) < 2i32 {
        return -1i32;
    }
    if values[0].abs() < 1.0e-7f64 || values[1].abs() < 1.0e-7f64 {
        return -1i32;
    }
    (*SCALE_FACTORS.offset(SCALE_FACTOR_COUNT as isize)).x = 1i32 as f64 / values[0];
    (*SCALE_FACTORS.offset(SCALE_FACTOR_COUNT as isize)).y = 1i32 as f64 / values[1];
    (*args).cur = &[];
    return spc_handler_xtx_do_transform(
        (*spe).x_user,
        (*spe).y_user,
        values[0],
        0i32 as f64,
        0i32 as f64,
        values[1],
        0i32 as f64,
        0i32 as f64,
    );
}
unsafe fn spc_handler_xtx_escale(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let fresh0 = SCALE_FACTOR_COUNT;
    SCALE_FACTOR_COUNT = SCALE_FACTOR_COUNT - 1;
    let mut factor: Coord = *SCALE_FACTORS.offset(fresh0 as isize);
    (*args).cur = &[];
    return spc_handler_xtx_do_transform(
        (*spe).x_user,
        (*spe).y_user,
        factor.x,
        0i32 as f64,
        0i32 as f64,
        factor.y,
        0i32 as f64,
        0i32 as f64,
    );
}
unsafe fn spc_handler_xtx_rotate(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut value: f64 = 0.;
    if spc_util_read_numbers(&mut value, 1i32, args) < 1i32 {
        return -1i32;
    }
    (*args).cur = &[];
    let (s, c) = (value * core::f64::consts::PI / 180.).sin_cos();
    spc_handler_xtx_do_transform(
        (*spe).x_user,
        (*spe).y_user,
        c,
        s,
        -s,
        c,
        0i32 as f64,
        0i32 as f64,
    )
}
#[no_mangle]
pub unsafe fn spc_handler_xtx_gsave(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_dev_gsave();
    0i32
}
#[no_mangle]
pub unsafe fn spc_handler_xtx_grestore(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_dev_grestore();
    /*
     * Unfortunately, the following line is necessary in case
     * of a font or color change inside of the save/restore pair.
     * Anything that was done there must be redone, so in effect,
     * we make no assumptions about what fonts. We act like we are
     * starting a new page.
     */
    pdf_dev_reset_fonts(0i32);
    pdf_dev_reset_color(0i32);
    0i32
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_xtx_papersize(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    0i32
}
unsafe fn spc_handler_xtx_backgroundcolor(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, false) {
        pdf_doc_set_bgcolor(Some(&colorspec));
        1
    } else {
        spc_warn!(spe, "No valid color specified?");
        -1
    }
}

/* FIXME: xdv2pdf's x:fontmapline and x:fontmapfile may have slightly different syntax/semantics */
unsafe fn spc_handler_xtx_fontmapline(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    static mut BUFFER: [u8; 1024] = [0; 1024];
    (*ap).cur.skip_white();
    if (*ap).cur.is_empty() {
        spc_warn!(spe, "Empty fontmapline special?");
        return -1i32;
    }
    let opchr = (*ap).cur[0];
    if opchr == b'-' || opchr == b'+' {
        (*ap).cur = &(*ap).cur[1..];
    }
    (*ap).cur.skip_white();
    match opchr as i32 {
        45 => {
            if let Some(map_name) = (*ap).cur.parse_ident() {
                pdf_remove_fontmap_record(map_name.as_ptr());
            } else {
                spc_warn!(spe, "Invalid fontmap line: Missing TFM name.");
                error = -1i32
            }
        }
        _ => {
            BUFFER[..(*ap).cur.len()].copy_from_slice((*ap).cur);
            BUFFER[(*ap).cur.len()] = 0;
            let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                as *mut fontmap_rec;
            pdf_init_fontmap_record(mrec);
            error = pdf_read_fontmap_line(
                mrec,
                BUFFER.as_mut_ptr() as *mut i8,
                (*ap).cur.len() as i32,
                is_pdfm_mapline(BUFFER.as_mut_ptr() as *mut i8),
            );
            if error != 0 {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr as i32 == '+' as i32 {
                pdf_append_fontmap_record((*mrec).map_name, mrec);
            } else {
                pdf_insert_fontmap_record((*mrec).map_name, mrec);
            }
            pdf_clear_fontmap_record(mrec);
            free(mrec as *mut libc::c_void);
        }
    }
    if error == 0 {
        (*ap).cur = &[];
    }
    0i32
}
unsafe fn spc_handler_xtx_fontmapfile(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if (*args).cur.is_empty() {
        return 0i32;
    }
    let mode = match (*args).cur[0] as i32 {
        45 => {
            (*args).cur = &(*args).cur[1..];
            '-' as i32
        }
        43 => {
            (*args).cur = &(*args).cur[1..];
            '+' as i32
        }
        _ => 0,
    };
    if let Some(mapfile) = (*args).cur.parse_val_ident() {
        pdf_load_fontmap_file(mapfile.as_c_str(), mode)
    } else {
        spc_warn!(spe, "No fontmap file specified.");
        -1
    }
}
static mut OVERLAY_NAME: [u8; 256] = [0; 256];
unsafe fn spc_handler_xtx_initoverlay(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if (*args).cur.is_empty() {
        return -1i32;
    }
    OVERLAY_NAME[..(*args).cur.len()].copy_from_slice((*args).cur);
    OVERLAY_NAME[(*args).cur.len()] = 0;
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_xtx_clipoverlay(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if (*args).cur.is_empty() {
        return -1i32;
    }
    pdf_dev_grestore();
    pdf_dev_gsave();
    let pos = OVERLAY_NAME.iter().position(|&x| x == 0).unwrap();
    if (*args).cur != &OVERLAY_NAME[..pos]
        && (*args).cur != b"all" {
        pdf_doc_add_page_content(b" 0 0 m W n");
    }
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_xtx_renderingmode(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut value: f64 = 0.;
    if spc_util_read_numbers(&mut value, 1i32, args) < 1i32 {
        return -1i32;
    }
    if (value as i32) < 0i32 || value as i32 > 7i32 {
        spc_warn!(spe, "Invalid text rendering mode {}.\n", value as i32,);
        return -1i32;
    }
    sprintf(
        WORK_BUFFER.as_mut_ptr() as *mut i8,
        b" %d Tr\x00" as *const u8 as *const i8,
        value as i32,
    );
    let pos = WORK_BUFFER.iter().position(|&x| x == 0).unwrap();
    pdf_doc_add_page_content(
        &WORK_BUFFER[..pos]
    );
    (*args).cur.skip_white();
    if !(*args).cur.is_empty() {
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content((*args).cur);
    }
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_xtx_unsupportedcolor(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    spc_warn!(
        spe,
        "xetex-style \\special{{x:{}}} is not supported by this driver;\nupdate document or driver to use \\special{{color}} instead.",
        (*args).command.unwrap().display(),
    );
    (*args).cur = &[];
    0i32
}
unsafe fn spc_handler_xtx_unsupported(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    spc_warn!(
        spe,
        "xetex-style \\special{{x:{}}} is not supported by this driver.",
        (*args).command.unwrap().display(),
    );
    (*args).cur = &[];
    0i32
}
const XTX_HANDLERS: [SpcHandler; 21] = [
    SpcHandler {
        key: b"textcolor",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"textcolorpush",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"textcolorpop",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"rulecolor",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"rulecolorpush",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"rulecolorpop",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: b"papersize",
        exec: Some(spc_handler_xtx_papersize),
    },
    SpcHandler {
        key: b"backgroundcolor",
        exec: Some(spc_handler_xtx_backgroundcolor),
    },
    SpcHandler {
        key: b"gsave",
        exec: Some(spc_handler_xtx_gsave),
    },
    SpcHandler {
        key: b"grestore",
        exec: Some(spc_handler_xtx_grestore),
    },
    SpcHandler {
        key: b"scale",
        exec: Some(spc_handler_xtx_scale),
    },
    SpcHandler {
        key: b"bscale",
        exec: Some(spc_handler_xtx_bscale),
    },
    SpcHandler {
        key: b"escale",
        exec: Some(spc_handler_xtx_escale),
    },
    SpcHandler {
        key: b"rotate",
        exec: Some(spc_handler_xtx_rotate),
    },
    SpcHandler {
        key: b"fontmapline",
        exec: Some(spc_handler_xtx_fontmapline),
    },
    SpcHandler {
        key: b"fontmapfile",
        exec: Some(spc_handler_xtx_fontmapfile),
    },
    SpcHandler {
        key: b"shadow",
        exec: Some(spc_handler_xtx_unsupported),
    },
    SpcHandler {
        key: b"colorshadow",
        exec: Some(spc_handler_xtx_unsupported),
    },
    SpcHandler {
        key: b"renderingmode",
        exec: Some(spc_handler_xtx_renderingmode),
    },
    SpcHandler {
        key: b"initoverlay",
        exec: Some(spc_handler_xtx_initoverlay),
    },
    SpcHandler {
        key: b"clipoverlay",
        exec: Some(spc_handler_xtx_clipoverlay),
    },
];
pub fn spc_xtx_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"x:")
}
#[no_mangle]
pub unsafe extern "C" fn spc_xtx_setup_handler(
    mut sph: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    (*ap).cur.skip_white();
    if !(*ap).cur.starts_with(b"x:") {
        spc_warn!(spe, "Not x: special???");
        return -1i32;
    }
    (*ap).cur = &(*ap).cur[b"x:".len()..];
    (*ap).cur.skip_white();
    if let Some(q) = (*ap).cur.parse_c_ident() {
        for handler in XTX_HANDLERS.iter() {
            if q.to_bytes() == handler.key {
                (*ap).command = Some(handler.key);
                (*sph).key = b"x:";
                (*sph).exec = handler.exec;
                (*ap).cur.skip_white();
                error = 0i32;
                break;
            }
        }
    }
    error
}
