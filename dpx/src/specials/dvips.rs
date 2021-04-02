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

use super::{Result, ERR, ERROR};

use std::ptr;

use crate::warn;

use super::{SpcArg, SpcEnv};
use crate::dpx_pdfdoc::PdfPageBoundary;
use crate::dpx_pdfdraw::pdf_dev_concat;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use bridge::{InFile, TTInputFormat};

use super::util::spc_util_read_dimtrns;
use crate::dpx_mpost::{mps_eop_cleanup, mps_exec_inline, mps_stack_depth};
use crate::dpx_pdfdev::{pdf_dev_put_image, transform_info, transform_info_clear, TMatrix};
use crate::dpx_pdfdraw::{
    pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to, pdf_dev_gsave,
};
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;

/* quasi-hack to get the primary input */

use super::Handler;

use crate::dpx_pdfximage::load_options;
static mut BLOCK_PENDING: i32 = 0;
static mut PENDING_X: f64 = 0.0f64;
static mut PENDING_Y: f64 = 0.0f64;
static mut POSITION_SET: i32 = 0;
static mut PS_HEADERS: Vec<String> = Vec::new();

unsafe fn spc_handler_ps_header(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if args.cur.len() <= 1 || args.cur[0] != b'=' {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return ERR;
    }
    args.cur = &args.cur[1..];
    let pro = String::from_utf8_lossy(args.cur).to_string();
    if InFile::open(&pro, TTInputFormat::TEX_PS_HEADER, 0).is_none() {
        spc_warn!(spe, "PS header {} not found.", pro);
        return ERR;
    }
    PS_HEADERS.push(pro);
    args.cur = &[];
    Ok(())
}
unsafe fn parse_filename<'a>(pp: &mut &'a [u8]) -> Option<&'a str> {
    let mut p = *pp;
    let qchar;
    if p.is_empty() {
        return None;
    } else {
        if p[0] == b'\"' || p[0] == b'\'' {
            qchar = p[0];
            p = &p[1..];
        } else {
            qchar = b' ';
        }
    }
    let mut n = 0;
    let q = p;
    while !p.is_empty() && p[0] != qchar {
        /* nothing */
        n += 1;
        p = &p[1..];
    }
    if qchar != b' ' {
        if p[0] != qchar {
            return None;
        }
        p = &p[1..];
    }
    if q.is_empty() || n == 0 {
        return None;
    }
    let r = Some(std::str::from_utf8(&q[..n]).unwrap());
    *pp = p;
    r
}
/* =filename ... */
unsafe fn spc_handler_ps_file(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur.skip_white();
    if args.cur.len() <= 1 || args.cur[0] != b'=' {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return ERR;
    }
    args.cur = &args.cur[1..];
    if let Some(filename) = parse_filename(&mut args.cur) {
        let mut ti = if let Ok(ti) = spc_util_read_dimtrns(spe, args, 1) {
            ti
        } else {
            return ERR;
        };
        let options: load_options = load_options {
            page_no: 1,
            bbox_type: PdfPageBoundary::Auto,
            dict: ptr::null_mut(),
        };
        let form_id = pdf_ximage_findresource(filename, options);
        if form_id < 0 {
            spc_warn!(spe, "Failed to read image file: {}", filename);
            return ERR;
        }
        pdf_dev_put_image(form_id, &mut ti, spe.x_user, spe.y_user);
        Ok(())
    } else {
        spc_warn!(spe, "No filename specified for PSfile special.");
        ERR
    }
}
/* This isn't correct implementation but dvipdfm supports... */
unsafe fn spc_handler_ps_plotfile(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let mut error = Ok(()); /* xscale = 1.0, yscale = -1.0 */
    let mut p = transform_info::new();
    spc_warn!(spe, "\"ps: plotfile\" found (not properly implemented)");
    args.cur.skip_white();
    if let Some(filename) = parse_filename(&mut args.cur) {
        let options: load_options = load_options {
            page_no: 1,
            bbox_type: PdfPageBoundary::Auto,
            dict: ptr::null_mut(),
        };
        let form_id = pdf_ximage_findresource(filename, options);
        if form_id < 0 {
            spc_warn!(spe, "Could not open PS file: {}", filename);
            error = ERR;
        } else {
            transform_info_clear(&mut p);
            p.matrix.m22 = -1.;
            pdf_dev_put_image(form_id, &mut p, 0 as f64, 0 as f64);
        }
        error
    } else {
        spc_warn!(spe, "Expecting filename but not found...");
        return ERR;
    }
}
unsafe fn spc_handler_ps_literal(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    let mut error = Ok(());
    let x_user;
    let y_user;
    assert!(!args.cur.is_empty());
    if let Some(cur) = args.cur.strip_prefix(b":[begin]") {
        BLOCK_PENDING += 1;
        POSITION_SET = 1;
        PENDING_X = spe.x_user;
        x_user = PENDING_X;
        PENDING_Y = spe.y_user;
        y_user = PENDING_Y;
        args.cur = cur;
    } else if let Some(cur) = args.cur.strip_prefix(b":[end]") {
        if BLOCK_PENDING <= 0 {
            spc_warn!(spe, "No corresponding ::[begin] found.");
            return ERR;
        }
        BLOCK_PENDING -= 1;
        POSITION_SET = 0;
        x_user = PENDING_X;
        y_user = PENDING_Y;
        args.cur = cur;
    } else if !args.cur.is_empty() && args.cur[0] == b':' {
        x_user = if POSITION_SET != 0 {
            PENDING_X
        } else {
            spe.x_user
        };
        y_user = if POSITION_SET != 0 {
            PENDING_Y
        } else {
            spe.y_user
        };
        args.cur = &args.cur[1..];
    } else {
        POSITION_SET = 1;
        PENDING_X = spe.x_user;
        x_user = PENDING_X;
        PENDING_Y = spe.y_user;
        y_user = PENDING_Y
    }
    args.cur.skip_white();
    if !args.cur.is_empty() {
        let st_depth = mps_stack_depth();
        let gs_depth = pdf_dev_current_depth();
        error = mps_exec_inline(&mut args.cur, x_user, y_user);
        if error.is_err() {
            spc_warn!(
                spe,
                "Interpreting PS code failed!!! Output might be broken!!!"
            );
            pdf_dev_grestore_to(gs_depth);
        } else if st_depth != mps_stack_depth() {
            spc_warn!(
                spe,
                "Stack not empty after execution of inline PostScript code."
            );
            spc_warn!(
                spe,
                ">> Your macro package makes some assumption on internal behaviour of DVI drivers."
            );
            spc_warn!(spe, ">> It may not compatible with dvipdfmx.");
        }
    }
    error
}
unsafe fn spc_handler_ps_trickscmd(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    warn!("PSTricks commands are disallowed in Tectonic");
    args.cur = &[];
    ERR
}
unsafe fn spc_handler_ps_tricksobj(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    warn!("PSTricks commands are disallowed in Tectonic");
    args.cur = &[];
    ERR
}
unsafe fn spc_handler_ps_default(spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    pdf_dev_gsave();
    let st_depth = mps_stack_depth();
    let gs_depth = pdf_dev_current_depth();
    let mut M = TMatrix::create_translation(spe.x_user, spe.y_user);
    pdf_dev_concat(&mut M);
    let error = mps_exec_inline(&mut args.cur, spe.x_user, spe.y_user);
    M.m31 = -spe.x_user;
    M.m32 = -spe.y_user;
    pdf_dev_concat(&mut M);
    if error.is_err() {
        spc_warn!(
            spe,
            "Interpreting PS code failed!!! Output might be broken!!!"
        );
    } else if st_depth != mps_stack_depth() {
        spc_warn!(
            spe,
            "Stack not empty after execution of inline PostScript code."
        );
        spc_warn!(
            spe,
            ">> Your macro package makes some assumption on internal behaviour of DVI drivers."
        );
        spc_warn!(spe, ">> It may not compatible with dvipdfmx.");
    }
    pdf_dev_grestore_to(gs_depth);
    pdf_dev_grestore();
    error
}

static DVIPS_HANDLERS: phf::Map<&'static str, Handler> = phf::phf_map! {
    "header" => spc_handler_ps_header,
    "PSfile" => spc_handler_ps_file,
    "psfile" => spc_handler_ps_file,
    "ps: plotfile " => spc_handler_ps_plotfile,
    "PS: plotfile " => spc_handler_ps_plotfile,
    "PS:" => spc_handler_ps_literal,
    "ps:" => spc_handler_ps_literal,
    "PST:" => spc_handler_ps_trickscmd,
    "pst:" => spc_handler_ps_tricksobj,
    "\" " => spc_handler_ps_default,
};

pub(crate) unsafe fn spc_dvips_at_begin_document() -> Result<()> {
    /* This function used to start the global_defs temp file. */
    Ok(())
}

pub(crate) unsafe fn spc_dvips_at_end_document() -> Result<()> {
    PS_HEADERS.clear();
    Ok(())
}

pub(crate) unsafe fn spc_dvips_at_begin_page() -> Result<()> {
    /* This function used do some things related to now-removed PSTricks functionality. */
    Ok(())
}

pub(crate) unsafe fn spc_dvips_at_end_page() -> Result<()> {
    mps_eop_cleanup();
    Ok(())
}
pub(crate) fn spc_dvips_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    if buf.is_empty() {
        return false;
    }
    for key in DVIPS_HANDLERS.keys() {
        if buf.starts_with(key.as_bytes()) {
            return true;
        }
    }
    false
}

pub(crate) unsafe fn spc_dvips_setup_handler(
    spe: &mut SpcEnv,
    args: &mut SpcArg,
) -> Result<Handler> {
    args.cur.skip_white();
    let key = args.cur;
    while !args.cur.is_empty() && (args.cur[0] as u8).is_ascii_alphabetic() {
        args.cur = &args.cur[1..];
    }
    /* Test for "ps:". The "ps::" special is subsumed under this case.  */
    if !args.cur.is_empty() && args.cur[0] == b':' {
        args.cur = &args.cur[1..];
        if let Some(cur) = args.cur.strip_prefix(b" plotfile ") {
            args.cur = cur;
        }
    } else if args.cur.len() > 1 && args.cur[0] == b'\"' && args.cur[1] == b' ' {
        args.cur = &args.cur[2..];
    }
    let keylen = key.len() - args.cur.len();
    if keylen < 1 {
        spc_warn!(spe, "Not ps: special???");
        return ERROR();
    }
    for (hkey, &exec) in DVIPS_HANDLERS.entries() {
        if &key[..keylen] == hkey.as_bytes() {
            args.cur.skip_white();
            args.command = Some(hkey);
            return Ok(exec);
        }
    }
    ERROR()
}
