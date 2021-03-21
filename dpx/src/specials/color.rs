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

use super::util::spc_util_read_colorspec;
use super::{Handler, SpcArg, SpcEnv};
use super::{Result, ERR, ERROR};
use crate::dpx_dpxutil::ParseCIdent;
use crate::dpx_pdfcolor::{pdf_color_clear_stack, pdf_color_pop, pdf_color_push, pdf_color_set};
use crate::dpx_pdfdoc::pdf_doc_set_bgcolor;
use crate::spc_warn;
use crate::SkipBlank;

/* Color stack is actually placed into pdfcolor.c.
 * The reason why we need to place stack there is
 * that we must reinstall color after grestore and
 * other operations that can change current color
 * implicitely.
 */
unsafe fn spc_handler_color_push(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if let Ok(mut colorspec) = spc_util_read_colorspec(spe, args, true) {
        let color_clone = colorspec.clone();
        pdf_color_push(&mut colorspec, &color_clone);
        Ok(())
    } else {
        ERR
    }
}

unsafe fn spc_handler_color_pop(mut _spe: &mut SpcEnv, _args: &mut SpcArg) -> Result<()> {
    pdf_color_pop();
    Ok(())
}
/* Invoked by the special command "color rgb .625 0 0".
 * DVIPS clears the color stack, and then saves and sets the given color.
 */
unsafe fn spc_handler_color_default(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, true) {
        pdf_color_clear_stack();
        pdf_color_set(&colorspec, &colorspec);
        Ok(())
    } else {
        ERR
    }
}
/* This is from color special? */
unsafe fn spc_handler_background(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    if let Ok(colorspec) = spc_util_read_colorspec(spe, args, true) {
        pdf_doc_set_bgcolor(Some(&colorspec));
        Ok(())
    } else {
        ERR
    }
}

pub(crate) fn spc_color_check_special(mut buf: &[u8]) -> bool {
    buf.skip_blank();
    if let Some(q) = buf.parse_c_ident() {
        q == "color" || q == "background"
    } else {
        false
    }
}

pub(crate) unsafe fn spc_color_setup_handler(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<Handler> {
    ap.cur.skip_blank();
    let q = ap.cur.parse_c_ident();
    if q.is_none() {
        return ERROR();
    }
    ap.cur.skip_blank();
    let exec = match q.unwrap().as_ref() {
        "background" => {
            ap.command = Some("background");
            spc_handler_background
        }
        "color" => {
            /* color */
            /* cmyk, rgb, ... */
            let mut p = &ap.cur[..];
            if let Some(q) = p.parse_c_ident() {
                match q.as_ref() {
                    "push" => {
                        ap.command = Some("push");
                        ap.cur = p;
                        spc_handler_color_push
                    }
                    "pop" => {
                        ap.command = Some("pop");
                        ap.cur = p;
                        spc_handler_color_pop
                    }
                    _ => {
                        ap.command = Some("");
                        spc_handler_color_default
                    }
                }
            } else {
                return ERROR();
            }
        }
        _ => {
            spc_warn!(spe, "Not color/background special?");
            return ERROR();
        }
    };
    ap.cur.skip_blank();
    Ok(exec)
}
