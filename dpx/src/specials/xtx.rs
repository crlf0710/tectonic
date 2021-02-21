/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2018 by Jin-Hwan Cho and Shunsaku Hirata,
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
#![allow(non_camel_case_types, non_snake_case)]

use super::util::spc_util_read_colorspec;
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_fontmap::{
    is_pdfm_mapline, pdf_append_fontmap_record, pdf_init_fontmap_record, pdf_insert_fontmap_record,
    pdf_load_fontmap_file, pdf_read_fontmap_line, pdf_remove_fontmap_record,
};
use crate::dpx_pdfdev::{pdf_dev_reset_color, pdf_dev_reset_fonts};
use crate::dpx_pdfdoc::{pdf_doc_add_page_content, pdf_doc_set_bgcolor};
use crate::dpx_pdfdraw::{
    pdf_dev_concat, pdf_dev_get_fixed_point, pdf_dev_grestore, pdf_dev_gsave,
    pdf_dev_set_fixed_point,
};
use crate::dpx_pdfparse::{ParseIdent, SkipWhite};
use crate::spc_warn;

use super::{SpcArg, SpcEnv};

use super::SpcHandler;

use crate::dpx_pdfdev::Point;

use crate::dpx_pdfdev::TMatrix;

use arrayvec::ArrayVec;

pub(crate) unsafe fn spc_handler_xtx_do_transform(
    x_user: f64,
    y_user: f64,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
    e: f64,
    f: f64,
) -> i32 {
    let mut M = TMatrix::row_major(
        /* Create transformation matrix */
        a,
        b,
        c,
        d,
        (1. - a) * x_user - c * y_user + e,
        (1. - d) * y_user - b * x_user + f,
    );
    pdf_dev_concat(&mut M);
    let pt = pdf_dev_get_fixed_point();
    pdf_dev_set_fixed_point(x_user - pt.x, y_user - pt.y);
    0
}
unsafe fn spc_handler_xtx_scale(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let values = super::util::read_numbers::<[_; 2]>(args);
    if values.len() < 2 {
        return -1;
    }
    args.cur = &[];
    return spc_handler_xtx_do_transform(
        spe.x_user, spe.y_user, values[0], 0 as f64, 0 as f64, values[1], 0 as f64, 0 as f64,
    );
}
/* Scaling without gsave/grestore. */
static mut SCALE_FACTORS: Vec<Point> = Vec::new();

unsafe fn spc_handler_xtx_bscale(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let values: ArrayVec<[_; 2]> = super::util::read_numbers(args);
    if values.len() < 2 {
        return -1;
    }
    if values[0].abs() < 1.0e-7f64 || values[1].abs() < 1.0e-7f64 {
        return -1;
    }
    SCALE_FACTORS.push(Point::new(1 as f64 / values[0], 1 as f64 / values[1]));
    args.cur = &[];
    return spc_handler_xtx_do_transform(
        spe.x_user, spe.y_user, values[0], 0 as f64, 0 as f64, values[1], 0 as f64, 0 as f64,
    );
}

unsafe fn spc_handler_xtx_escale(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let factor = SCALE_FACTORS.pop().unwrap();
    args.cur = &[];
    return spc_handler_xtx_do_transform(
        spe.x_user, spe.y_user, factor.x, 0 as f64, 0 as f64, factor.y, 0 as f64, 0 as f64,
    );
}
unsafe fn spc_handler_xtx_rotate(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let values = super::util::read_numbers::<[f64; 1]>(args);
    if values.len() < 1 {
        return -1;
    }
    let value = values[0];
    args.cur = &[];
    let (s, c) = (value * core::f64::consts::PI / 180.).sin_cos();
    spc_handler_xtx_do_transform(spe.x_user, spe.y_user, c, s, -s, c, 0 as f64, 0 as f64)
}

pub(crate) unsafe fn spc_handler_xtx_gsave(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    pdf_dev_gsave();
    0
}

pub(crate) unsafe fn spc_handler_xtx_grestore(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    pdf_dev_grestore();
    /*
     * Unfortunately, the following line is necessary in case
     * of a font or color change inside of the save/restore pair.
     * Anything that was done there must be redone, so in effect,
     * we make no assumptions about what fonts. We act like we are
     * starting a new page.
     */
    pdf_dev_reset_fonts(0);
    pdf_dev_reset_color(0);
    0
}
/* Please remove this.
 * This should be handled before processing pages!
 */
unsafe fn spc_handler_xtx_papersize(_spe: &mut SpcEnv, _args: &mut SpcArg) -> i32 {
    0
}
unsafe fn spc_handler_xtx_backgroundcolor(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, false) {
        pdf_doc_set_bgcolor(Some(&colorspec));
        1
    } else {
        spc_warn!(spe, "No valid color specified?");
        -1
    }
}

/* FIXME: xdv2pdf's x:fontmapline and x:fontmapfile may have slightly different syntax/semantics */
unsafe fn spc_handler_xtx_fontmapline(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let mut error: i32 = 0;
    ap.cur.skip_white();
    if ap.cur.is_empty() {
        spc_warn!(spe, "Empty fontmapline special?");
        return -1;
    }
    let opchr = ap.cur[0];
    if opchr == b'-' || opchr == b'+' {
        ap.cur = &ap.cur[1..];
    }
    ap.cur.skip_white();
    match opchr as i32 {
        45 => {
            if let Some(map_name) = ap.cur.parse_ident() {
                pdf_remove_fontmap_record(&map_name);
            } else {
                spc_warn!(spe, "Invalid fontmap line: Missing TFM name.");
                error = -1
            }
        }
        _ => {
            let s = String::from_utf8(ap.cur.into()).unwrap();
            let mut mrec = pdf_init_fontmap_record();
            error = pdf_read_fontmap_line(&mut mrec, &s, is_pdfm_mapline(&s));
            if error != 0 {
                spc_warn!(spe, "Invalid fontmap line.");
            } else if opchr == b'+' {
                pdf_append_fontmap_record(&mrec.map_name, &mrec);
            } else {
                pdf_insert_fontmap_record(&mrec.map_name, &mrec).ok();
            }
        }
    }
    if error == 0 {
        ap.cur = &[];
    }
    0
}
unsafe fn spc_handler_xtx_fontmapfile(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if args.cur.is_empty() {
        return 0;
    }
    let mode = match args.cur[0] as i32 {
        45 => {
            args.cur = &args.cur[1..];
            '-' as i32
        }
        43 => {
            args.cur = &args.cur[1..];
            '+' as i32
        }
        _ => 0,
    };
    if let Some(mapfile) = args.cur.parse_val_ident() {
        pdf_load_fontmap_file(&mapfile, mode)
    } else {
        spc_warn!(spe, "No fontmap file specified.");
        -1
    }
}
static mut OVERLAY_NAME: [u8; 256] = [0; 256];
unsafe fn spc_handler_xtx_initoverlay(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if args.cur.is_empty() {
        return -1;
    }
    OVERLAY_NAME[..args.cur.len()].copy_from_slice(args.cur);
    OVERLAY_NAME[args.cur.len()] = 0;
    args.cur = &[];
    0
}
unsafe fn spc_handler_xtx_clipoverlay(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur.skip_white();
    if args.cur.is_empty() {
        return -1;
    }
    pdf_dev_grestore();
    pdf_dev_gsave();
    let pos = OVERLAY_NAME.iter().position(|&x| x == 0).unwrap();
    if args.cur != &OVERLAY_NAME[..pos] && args.cur != b"all" {
        pdf_doc_add_page_content(b" 0 0 m W n");
    }
    args.cur = &[];
    0
}
unsafe fn spc_handler_xtx_renderingmode(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    let values: ArrayVec<[_; 1]> = super::util::read_numbers(args);
    if values.is_empty() {
        return -1;
    }
    let value = values[0];
    if (value as i32) < 0 || value as i32 > 7 {
        spc_warn!(spe, "Invalid text rendering mode {}.\n", value as i32);
        return -1;
    }
    let content = format!(" {} Tr", value as i32);
    pdf_doc_add_page_content(content.as_bytes());
    args.cur.skip_white();
    if !args.cur.is_empty() {
        pdf_doc_add_page_content(b" ");
        pdf_doc_add_page_content(args.cur);
    }
    args.cur = &[];
    0
}
unsafe fn spc_handler_xtx_unsupportedcolor(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    spc_warn!(
        spe,
        "xetex-style \\special{{x:{}}} is not supported by this driver;\nupdate document or driver to use \\special{{color}} instead.",
        args.command.unwrap(),
    );
    args.cur = &[];
    0
}
unsafe fn spc_handler_xtx_unsupported(spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    spc_warn!(
        spe,
        "xetex-style \\special{{x:{}}} is not supported by this driver.",
        args.command.unwrap(),
    );
    args.cur = &[];
    0
}
const XTX_HANDLERS: [SpcHandler; 21] = [
    SpcHandler {
        key: "textcolor",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "textcolorpush",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "textcolorpop",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "rulecolor",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "rulecolorpush",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "rulecolorpop",
        exec: Some(spc_handler_xtx_unsupportedcolor),
    },
    SpcHandler {
        key: "papersize",
        exec: Some(spc_handler_xtx_papersize),
    },
    SpcHandler {
        key: "backgroundcolor",
        exec: Some(spc_handler_xtx_backgroundcolor),
    },
    SpcHandler {
        key: "gsave",
        exec: Some(spc_handler_xtx_gsave),
    },
    SpcHandler {
        key: "grestore",
        exec: Some(spc_handler_xtx_grestore),
    },
    SpcHandler {
        key: "scale",
        exec: Some(spc_handler_xtx_scale),
    },
    SpcHandler {
        key: "bscale",
        exec: Some(spc_handler_xtx_bscale),
    },
    SpcHandler {
        key: "escale",
        exec: Some(spc_handler_xtx_escale),
    },
    SpcHandler {
        key: "rotate",
        exec: Some(spc_handler_xtx_rotate),
    },
    SpcHandler {
        key: "fontmapline",
        exec: Some(spc_handler_xtx_fontmapline),
    },
    SpcHandler {
        key: "fontmapfile",
        exec: Some(spc_handler_xtx_fontmapfile),
    },
    SpcHandler {
        key: "shadow",
        exec: Some(spc_handler_xtx_unsupported),
    },
    SpcHandler {
        key: "colorshadow",
        exec: Some(spc_handler_xtx_unsupported),
    },
    SpcHandler {
        key: "renderingmode",
        exec: Some(spc_handler_xtx_renderingmode),
    },
    SpcHandler {
        key: "initoverlay",
        exec: Some(spc_handler_xtx_initoverlay),
    },
    SpcHandler {
        key: "clipoverlay",
        exec: Some(spc_handler_xtx_clipoverlay),
    },
];
pub(crate) fn spc_xtx_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"x:")
}

pub(crate) unsafe fn spc_xtx_setup_handler(
    sph: &mut SpcHandler,
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
) -> i32 {
    let mut error: i32 = -1;
    ap.cur.skip_white();
    if !ap.cur.starts_with(b"x:") {
        spc_warn!(spe, "Not x: special???");
        return -1;
    }
    ap.cur = &ap.cur[b"x:".len()..];
    ap.cur.skip_white();
    if let Some(q) = ap.cur.parse_c_ident() {
        for handler in XTX_HANDLERS.iter() {
            if q == handler.key {
                ap.command = Some(handler.key);
                *sph = SpcHandler {
                    key: "x:",
                    exec: handler.exec,
                };
                ap.cur.skip_white();
                error = 0;
                break;
            }
        }
    }
    error
}
