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

use crate::dpx_mfileio::tt_mfgets;
use crate::dpx_mpost::mps_scan_bbox;
use crate::dpx_pdfdev::{pdf_dev_put_image, transform_info, transform_info_clear};
use crate::dpx_pdfdoc::PdfPageBoundary;
use crate::dpx_pdfparse::SkipWhite;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use crate::spc_warn;
use bridge::{InFile, TTInputFormat};
use libc::strlen;
use std::ptr;

use super::{SpcArg, SpcEnv};

use super::SpcHandler;

use crate::dpx_pdfximage::load_options;

fn parse_postscriptbox_special(buf: &str) -> Result<(f64, f64, String), ()> {
    // TODO: port this to nom?
    let mut parts = Vec::new();
    for elem in buf.split("}") {
        if elem == "" {
            // "{a}{b}".split("}") is ["{a", "{b", ""]
            continue;
        }
        if !elem.starts_with("{") {
            return Err(());
        }
        parts.push(&elem[1..]);
    }

    if parts.len() != 3 {
        return Err(());
    }

    let width = parts[0].parse::<f64>().map_err(|_| ())?;
    let height = parts[1].parse::<f64>().map_err(|_| ())?;
    let filename = parts[2];
    Ok((width, height, filename.to_string()))
}

/* quasi-hack to get the primary input */
unsafe fn spc_handler_postscriptbox(spe: &mut SpcEnv, ap: &mut SpcArg) -> i32 {
    let mut ti = transform_info::new();
    let options: load_options = load_options {
        page_no: 1,
        bbox_type: PdfPageBoundary::Auto,
        dict: ptr::null_mut(),
    };
    let mut buf: [u8; 512] = [0; 512];
    if ap.cur.is_empty() {
        spc_warn!(
            spe,
            "No width/height/filename given for postscriptbox special."
        );
        return -1;
    }
    /* input is not NULL terminated */
    let len = ap.cur.len();
    let len = if 511 < len { 511 } else { len };
    buf[..len].copy_from_slice(&ap.cur[..len]);
    buf[len] = 0;
    transform_info_clear(&mut ti);

    let command = std::str::from_utf8(&buf).expect("non-utf8 postscriptbox special");

    spc_warn!(spe, "{}", command);

    let filename = if let Ok((width, height, filename)) = parse_postscriptbox_special(command) {
        ti.width = width;
        ti.height = height;
        filename
    } else {
        spc_warn!(spe, "Syntax error in postscriptbox special?");
        return -1;
    };

    ap.cur = &[];
    ti.width *= 72.0f64 / 72.27f64;
    ti.height *= 72.0f64 / 72.27f64;
    if let Some(mut handle) = InFile::open(&filename, TTInputFormat::PICT, 0) {
        ti.flags |= 1 << 1 | 1 << 2;
        loop {
            let mut p: *const i8 = tt_mfgets(buf.as_ptr() as *mut i8, 512, &mut handle);
            if p.is_null() {
                break;
            }
            if !(mps_scan_bbox(&mut p, p.offset(strlen(p) as isize), &mut ti.bbox) >= 0) {
                continue;
            }
            ti.flags |= 1 << 0;
            break;
        }
        let form_id = pdf_ximage_findresource(&filename, options);
        if form_id < 0 {
            spc_warn!(spe, "Failed to load image file: {}", filename);
            return -1;
        }
        pdf_dev_put_image(form_id, &mut ti, spe.x_user, spe.y_user);
        0
    } else {
        spc_warn!(spe, "Could not open image file: {}", filename);
        return -1;
    }
}
unsafe fn spc_handler_null(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur = &[];
    0
}
const MISC_HANDLERS: [SpcHandler; 6] = [
    SpcHandler {
        key: "postscriptbox",
        exec: Some(spc_handler_postscriptbox),
    },
    SpcHandler {
        key: "landscape",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: "papersize",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: "src:",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: "pos:",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: "om:",
        exec: Some(spc_handler_null),
    },
];

pub(crate) fn spc_misc_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    for handler in MISC_HANDLERS.iter() {
        if buf.starts_with(handler.key.as_bytes()) {
            return true;
        }
    }
    false
}

pub(crate) unsafe fn spc_misc_setup_handler(
    handle: &mut SpcHandler,
    _spe: &mut SpcEnv,
    args: &mut SpcArg,
) -> i32 {
    args.cur.skip_white();
    let key = args.cur;
    let mut keylen = 0;
    for &c in args.cur {
        if !(c as u8).is_ascii_alphabetic() {
            break;
        }
        keylen += 1;
    }
    args.cur = &args.cur[keylen..];
    if !args.cur.is_empty() && args.cur[0] == b':' {
        args.cur = &args.cur[1..];
        keylen += 1;
    }
    if keylen < 1 {
        return -1;
    }
    for handler in MISC_HANDLERS.iter() {
        if &key[..keylen] == handler.key.as_bytes() {
            args.cur.skip_white();
            args.command = Some(handler.key);
            *handle = SpcHandler {
                key: "???:",
                exec: handler.exec,
            };
            return 0;
        }
    }
    -1
}
