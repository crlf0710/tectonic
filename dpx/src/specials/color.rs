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

use super::util::spc_util_read_colorspec;
use super::{spc_arg, spc_env, SpcHandler};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_pdfcolor::{pdf_color_clear_stack, pdf_color_pop, pdf_color_push, pdf_color_set};
use crate::dpx_pdfdoc::pdf_doc_set_bgcolor;
use crate::spc_warn;
use crate::SkipBlank;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/* Color stack is actually placed into pdfcolor.c.
 * The reason why we need to place stack there is
 * that we must reinstall color after grestore and
 * other operations that can change current color
 * implicitely.
 */
unsafe fn spc_handler_color_push(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if let Ok(mut colorspec) = spc_util_read_colorspec(spe, args, true) {
        let color_clone = colorspec.clone();
        pdf_color_push(&mut colorspec, &color_clone);
        0
    } else {
        -1
    }
}

unsafe fn spc_handler_color_pop(mut _spe: *mut spc_env, mut _args: *mut spc_arg) -> i32 {
    pdf_color_pop();
    0i32
}
/* Invoked by the special command "color rgb .625 0 0".
 * DVIPS clears the color stack, and then saves and sets the given color.
 */
unsafe fn spc_handler_color_default(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, true) {
        pdf_color_clear_stack();
        pdf_color_set(&colorspec, &colorspec);
        0
    } else {
        -1
    }
}
/* This is from color special? */
unsafe fn spc_handler_background(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, true) {
        pdf_doc_set_bgcolor(Some(&colorspec));
        0
    } else {
        -1
    }
}

pub fn spc_color_check_special(mut buf: &[u8]) -> bool {
    buf.skip_blank();
    if let Some(q) = buf.parse_c_ident() {
        q.to_bytes() == b"color" || q.to_bytes() == b"background"
    } else {
        false
    }
}

pub unsafe fn spc_color_setup_handler(
    mut sph: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    (*ap).cur.skip_blank();
    let q = (*ap).cur.parse_c_ident();
    if q.is_none() {
        return -1i32;
    }
    (*ap).cur.skip_blank();
    match q.unwrap().to_bytes() {
        b"background" => {
            (*ap).command = Some(b"background");
            (*sph).exec = Some(spc_handler_background);
        }
        b"color" => {
            /* color */
 /* cmyk, rgb, ... */
            let mut p = &(*ap).cur[..];
            if let Some(q) = p.parse_c_ident() {
                match q.to_bytes() {
                    b"push" => {
                        (*ap).command = Some(b"push");
                        (*sph).exec = Some(spc_handler_color_push);
                        (*ap).cur = p
                    }
                    b"pop" => {
                        (*ap).command = Some(b"pop");
                        (*sph).exec = Some(spc_handler_color_pop);
                        (*ap).cur = p
                    }
                    _ => {
                        (*ap).command = Some(b"");
                        (*sph).exec = Some(spc_handler_color_default)
                    }
                }
            } else {
                return -1i32;
            }
        }
        _ => {
            spc_warn!(spe, "Not color/background special?");
            return -1i32;
        }
    }
    (*ap).cur.skip_blank();
    0i32
}
