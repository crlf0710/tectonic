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
#![allow()]

use super::{Handler, SpcArg, SpcEnv};
use super::{Result, ERROR};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;

unsafe fn spc_handler_null(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur = &[];
    Ok(())
}
static DVIPDFMX_HANDLERS: phf::Map<&'static str, Handler> = phf::phf_map! {
    "config" => spc_handler_null,
};

pub(crate) fn spc_dvipdfmx_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    buf.starts_with(b"dvipdfmx:")
}

pub(crate) unsafe fn spc_dvipdfmx_setup_handler(
    spe: &mut SpcEnv,
    ap: &mut SpcArg,
) -> Result<Handler> {
    ap.cur.skip_white();
    if let Some(cur) = ap.cur.strip_prefix(b"dvipdfmx:") {
        ap.cur = cur;
        ap.cur.skip_white();
        if let Some(q) = ap.cur.parse_c_ident() {
            if let Some((key, &exec)) = DVIPDFMX_HANDLERS.get_entry(q.as_str()) {
                ap.command = Some(key);
                ap.cur.skip_white();
                return Ok(exec);
            }
        }
        ERROR()
    } else {
        spc_warn!(spe, "Not dvipdfmx: special???");
        ERROR()
    }
}
