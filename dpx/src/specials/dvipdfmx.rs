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
#![allow()]

use super::{SpcArg, SpcEnv, SpcHandler};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

unsafe fn spc_handler_null(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur = &[];
    0i32
}
const DVIPDFMX_HANDLERS: [SpcHandler; 1] = [SpcHandler {
    key: "config",
    exec: Some(spc_handler_null),
}];

pub(crate) fn spc_dvipdfmx_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"dvipdfmx:")
}

pub(crate) unsafe fn spc_dvipdfmx_setup_handler(
    sph: &mut SpcHandler,
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
) -> i32 {
    let mut error: i32 = -1i32;
    ap.cur.skip_white();
    if !ap.cur.starts_with(b"dvipdfmx:") {
        spc_warn!(spe, "Not dvipdfmx: special???");
        return -1i32;
    }
    ap.cur = &ap.cur[b"dvipdfmx:".len()..];
    ap.cur.skip_white();
    if let Some(q) = ap.cur.parse_c_ident() {
        for handler in DVIPDFMX_HANDLERS.iter() {
            if q == handler.key {
                ap.command = Some(handler.key);
                *sph = SpcHandler {
                    key: "dvipdfmx:",
                    exec: handler.exec,
                };
                ap.cur.skip_white();
                error = 0i32;
                break;
            }
        }
    }
    error
}
