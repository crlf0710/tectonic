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

use std::ffi::CStr;

use super::{spc_arg, spc_env, SpcHandler};
use crate::dpx_dpxutil::parse_c_ident;
use crate::dpx_pdfparse::{skip_white, SkipWhite};
use crate::spc_warn;
use libc::{free, memcmp, strlen};

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

unsafe fn spc_handler_null(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).curptr = (*args).endptr;
    0i32
}
const DVIPDFMX_HANDLERS: [SpcHandler; 1] = [SpcHandler {
    key: b"config",
    exec: Some(spc_handler_null),
}];

pub fn spc_dvipdfmx_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"dvipdfmx:")
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvipdfmx_setup_handler(
    mut sph: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    let mut error: i32 = -1i32;
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    if (*ap)
        .curptr
        .offset(strlen(b"dvipdfmx:\x00" as *const u8 as *const i8) as isize)
        >= (*ap).endptr
        || memcmp(
            (*ap).curptr as *const libc::c_void,
            b"dvipdfmx:\x00" as *const u8 as *const i8 as *const libc::c_void,
            strlen(b"dvipdfmx:\x00" as *const u8 as *const i8),
        ) != 0
    {
        spc_warn!(spe, "Not dvipdfmx: special???");
        return -1i32;
    }
    (*ap).curptr = (*ap)
        .curptr
        .offset(strlen(b"dvipdfmx:\x00" as *const u8 as *const i8) as isize);
    skip_white(&mut (*ap).curptr, (*ap).endptr);
    let q = parse_c_ident(&mut (*ap).curptr, (*ap).endptr);
    if !q.is_null() {
        for handler in DVIPDFMX_HANDLERS.iter() {
            if CStr::from_ptr(q).to_bytes() == handler.key {
                (*ap).command = Some(handler.key);
                (*sph).key = b"dvipdfmx:";
                (*sph).exec = handler.exec;
                skip_white(&mut (*ap).curptr, (*ap).endptr);
                error = 0i32;
                break;
            }
        }
        free(q as *mut libc::c_void);
    }
    error
}
