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
    unused_mut
)]

use crate::dpx_mfileio::tt_mfgets;
use crate::dpx_mpost::mps_scan_bbox;
use crate::dpx_pdfdev::{pdf_dev_put_image, transform_info, transform_info_clear};
use crate::dpx_pdfparse::SkipWhite;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use crate::shims::sscanf;
use crate::spc_warn;
use crate::DisplayExt;
use crate::TTInputFormat;
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{strlen};
use std::ffi::CStr;
use std::ptr;

use super::{spc_arg, spc_env};

use super::SpcHandler;

use crate::dpx_pdfximage::load_options;

/* quasi-hack to get the primary input */
unsafe fn spc_handler_postscriptbox(mut spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    let mut options: load_options = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: ptr::null_mut(),
        };
    let mut filename: [i8; 256] = [0; 256];
    let mut buf: [u8; 512] = [0; 512];
    assert!(!spe.is_null() && !ap.is_null());
    if (*ap).cur.is_empty() {
        spc_warn!(
            spe,
            "No width/height/filename given for postscriptbox special."
        );
        return -1i32;
    }
    /* input is not NULL terminated */
    let len = (*ap).cur.len();
    let len = if 511 < len { 511 } else { len };
    buf[..len].copy_from_slice(&(*ap).cur[..len]);
    buf[len] = 0;
    transform_info_clear(&mut ti);
    spc_warn!(spe, "{}", &buf[..len].display());
    if sscanf(
        buf.as_mut_ptr() as *mut i8,
        b"{%lfpt}{%lfpt}{%255[^}]}\x00" as *const u8 as *const i8,
        &mut ti.width as *mut f64,
        &mut ti.height as *mut f64,
        filename.as_mut_ptr(),
    ) != 3i32
    {
        spc_warn!(spe, "Syntax error in postscriptbox special?");
        return -1i32;
    }
    (*ap).cur = &[];
    ti.width *= 72.0f64 / 72.27f64;
    ti.height *= 72.0f64 / 72.27f64;
    let handle = ttstub_input_open(filename.as_mut_ptr(), TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        spc_warn!(
            spe,
            "Could not open image file: {}",
            CStr::from_ptr(filename.as_mut_ptr()).display(),
        );
        return -1i32;
    }
    let mut handle = handle.unwrap();
    ti.flags |= 1i32 << 1i32 | 1i32 << 2i32;
    loop {
        let mut p: *const i8 = tt_mfgets(buf.as_mut_ptr() as *mut i8, 512, &mut handle);
        if p.is_null() {
            break;
        }
        if !(mps_scan_bbox(&mut p, p.offset(strlen(p) as isize), &mut ti.bbox) >= 0i32) {
            continue;
        }
        ti.flags |= 1i32 << 0i32;
        break;
    }
    ttstub_input_close(handle);
    let form_id = pdf_ximage_findresource(filename.as_mut_ptr(), options);
    if form_id < 0i32 {
        spc_warn!(
            spe,
            "Failed to load image file: {}",
            CStr::from_ptr(filename.as_mut_ptr()).display(),
        );
        return -1i32;
    }
    pdf_dev_put_image(form_id, &mut ti, (*spe).x_user, (*spe).y_user);
    0i32
}
unsafe fn spc_handler_null(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur = &[];
    0i32
}
const MISC_HANDLERS: [SpcHandler; 6] = [
    SpcHandler {
        key: b"postscriptbox",
        exec: Some(spc_handler_postscriptbox),
    },
    SpcHandler {
        key: b"landscape",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: b"papersize",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: b"src:",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: b"pos:",
        exec: Some(spc_handler_null),
    },
    SpcHandler {
        key: b"om:",
        exec: Some(spc_handler_null),
    },
];

pub fn spc_misc_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    for handler in MISC_HANDLERS.iter() {
        if buf.starts_with(handler.key) {
            return true;
        }
    }
    false
}

pub unsafe fn spc_misc_setup_handler(
    mut handle: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut args: *mut spc_arg,
) -> i32 {
    assert!(!handle.is_null() && !spe.is_null() && !args.is_null());
    (*args).cur.skip_white();
    let key = (*args).cur;
    let mut keylen = 0;
    for &c in (*args).cur {
        if (c as u8).is_ascii_alphabetic() {
            break;
        }
        keylen += 1;
    }
    (*args).cur = &(*args).cur[keylen..];
    if !(*args).cur.is_empty() && (*args).cur[0] == b':' {
        (*args).cur = &(*args).cur[1..];
        keylen += 1;
    }
    if keylen < 1 {
        return -1i32;
    }
    for handler in MISC_HANDLERS.iter() {
        if keylen == handler.key.len() && &key[..keylen] == handler.key {
            (*args).cur.skip_white();
            (*args).command = Some(handler.key);
            (*handle).key = b"???:";
            (*handle).exec = handler.exec;
            return 0i32;
        }
    }
    -1i32
}
