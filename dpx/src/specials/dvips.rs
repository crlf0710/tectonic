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
use std::ffi::{CStr, CString};
use std::ptr;

use crate::mfree;
use crate::warn;

use super::{spc_arg, spc_env};
use crate::TTInputFormat;

use crate::dpx_pdfdraw::pdf_dev_concat;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use crate::{ttstub_input_close, ttstub_input_open};

use super::util::spc_util_read_dimtrns;
use crate::dpx_mem::{xmalloc, xrealloc};
use crate::dpx_mpost::{mps_eop_cleanup, mps_exec_inline, mps_stack_depth};
use crate::dpx_pdfdev::{pdf_dev_put_image, TMatrix, transform_info, transform_info_clear};
use crate::dpx_pdfdraw::{
    pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to, pdf_dev_gsave,
};
use crate::dpx_pdfparse::SkipWhite;
use crate::spc_warn;
use libc::{free, strncpy};

pub type size_t = u64;
/* quasi-hack to get the primary input */

use super::SpcHandler;

use crate::dpx_pdfximage::load_options;
static mut BLOCK_PENDING: i32 = 0i32;
static mut PENDING_X: f64 = 0.0f64;
static mut PENDING_Y: f64 = 0.0f64;
static mut POSITION_SET: i32 = 0i32;
static mut PS_HEADERS: *mut *mut i8 = 0 as *const *mut i8 as *mut *mut i8;
static mut NUM_PS_HEADERS: i32 = 0i32;
unsafe fn spc_handler_ps_header(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    (*args).cur.skip_white();
    if (*args).cur.len() <= 1 || (*args).cur[0] != b'=' {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return -1i32;
    }
    (*args).cur = &(*args).cur[1..];
    let pro = xmalloc(
        ((*args).cur.len() as i64 + 1i32 as i64) as size_t,
    ) as *mut i8;
    strncpy(
        pro,
        (*args).cur.as_ptr() as *mut i8,
        (*args).cur.len() as _,
    );
    *pro.offset((*args).cur.len() as isize) = 0_i8;
    let ps_header =
        ttstub_input_open(pro, TTInputFormat::TEX_PS_HEADER, 0i32);
    if ps_header.is_none() {
        spc_warn!(
            spe,
            "PS header {} not found.",
            CStr::from_ptr(pro).display(),
        );
        free(pro as *mut libc::c_void);
        return -1i32;
    }
    let ps_header = ps_header.unwrap();
    ttstub_input_close(ps_header);
    if NUM_PS_HEADERS & 0xfi32 == 0 {
        PS_HEADERS = xrealloc(
            PS_HEADERS as *mut libc::c_void,
            (::std::mem::size_of::<*mut i8>() as u64).wrapping_mul((NUM_PS_HEADERS + 16i32) as u64),
        ) as *mut *mut i8
    }
    let fresh0 = NUM_PS_HEADERS;
    NUM_PS_HEADERS = NUM_PS_HEADERS + 1;
    let ref mut fresh1 = *PS_HEADERS.offset(fresh0 as isize);
    *fresh1 = pro;
    (*args).cur = &[];
    0i32
}
unsafe fn parse_filename(pp: &mut &[u8]) -> Option<CString> {
    let mut p = *pp;
    let mut qchar;
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
    let r = Some(CString::new(&q[..n]).unwrap());
    *pp = p;
    r
}
/* =filename ... */
unsafe fn spc_handler_ps_file(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    let mut options: load_options = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: ptr::null_mut(),
        };
    assert!(!spe.is_null() && !args.is_null());
    (*args).cur.skip_white();
    if (*args).cur.len() <= 1 || (*args).cur[0] != b'=' {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return -1;
    }
    (*args).cur = &(*args).cur[1..];
    if let Some(filename) = parse_filename(&mut (*args).cur) {
        transform_info_clear(&mut ti);
        if spc_util_read_dimtrns(spe, &mut ti, args, 1i32) < 0i32 {
            return -1;
        }
        let form_id = pdf_ximage_findresource(filename.as_ptr(), options);
        if form_id < 0i32 {
            spc_warn!(
                spe,
                "Failed to read image file: {}",
                filename.display(),
            );
            return -1i32;
        }
        pdf_dev_put_image(form_id, &mut ti, (*spe).x_user, (*spe).y_user);
        0i32
    } else {
        spc_warn!(spe, "No filename specified for PSfile special.");
        -1
    }
}
/* This isn't correct implementation but dvipdfm supports... */
unsafe fn spc_handler_ps_plotfile(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32; /* xscale = 1.0, yscale = -1.0 */
    let mut p = transform_info::new();
    let mut options: load_options = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: ptr::null_mut(),
        };
    assert!(!spe.is_null() && !args.is_null());
    spc_warn!(spe, "\"ps: plotfile\" found (not properly implemented)");
    (*args).cur.skip_white();
    if let Some(filename) = parse_filename(&mut (*args).cur) {
        let form_id = pdf_ximage_findresource(filename.as_ptr(), options);
        if form_id < 0i32 {
            spc_warn!(
                spe,
                "Could not open PS file: {}",
                filename.display(),
            );
            error = -1i32
        } else {
            transform_info_clear(&mut p);
            p.matrix.d = -1.0f64;
            pdf_dev_put_image(form_id, &mut p, 0i32 as f64, 0i32 as f64);
        }
        error
    } else {
        spc_warn!(spe, "Expecting filename but not found...");
        return -1i32;
    }
}
unsafe fn spc_handler_ps_literal(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    let x_user;
    let y_user;
    assert!(!spe.is_null() && !args.is_null() && !(*args).cur.is_empty());
    if (*args).cur.starts_with(b":[begin]") {
        BLOCK_PENDING += 1;
        POSITION_SET = 1i32;
        PENDING_X = (*spe).x_user;
        x_user = PENDING_X;
        PENDING_Y = (*spe).y_user;
        y_user = PENDING_Y;
        (*args).cur = &(*args).cur[b":[begin]".len()..];
    } else if (*args).cur.starts_with(b":[end]") {
        if BLOCK_PENDING <= 0 {
            spc_warn!(spe, "No corresponding ::[begin] found.");
            return -1;
        }
        BLOCK_PENDING -= 1;
        POSITION_SET = 0;
        x_user = PENDING_X;
        y_user = PENDING_Y;
        (*args).cur = &(*args).cur[b":[end]".len()..];
    } else if !(*args).cur.is_empty() && (*args).cur[0] == b':' {
        x_user = if POSITION_SET != 0 {
            PENDING_X
        } else {
            (*spe).x_user
        };
        y_user = if POSITION_SET != 0 {
            PENDING_Y
        } else {
            (*spe).y_user
        };
        (*args).cur = &(*args).cur[1..];
    } else {
        POSITION_SET = 1;
        PENDING_X = (*spe).x_user;
        x_user = PENDING_X;
        PENDING_Y = (*spe).y_user;
        y_user = PENDING_Y
    }
    (*args).cur.skip_white();
    if !(*args).cur.is_empty() {
        let st_depth = mps_stack_depth();
        let gs_depth = pdf_dev_current_depth();
        error = mps_exec_inline(&mut (*args).cur, x_user, y_user);
        if error != 0 {
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
unsafe fn spc_handler_ps_trickscmd(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    warn!("PSTricks commands are disallowed in Tectonic");
    (*args).cur = &[];
    -1i32
}
unsafe fn spc_handler_ps_tricksobj(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    warn!("PSTricks commands are disallowed in Tectonic");
    (*args).cur = &[];
    -1i32
}
unsafe fn spc_handler_ps_default(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    assert!(!spe.is_null() && !args.is_null());
    pdf_dev_gsave();
    let st_depth = mps_stack_depth();
    let gs_depth = pdf_dev_current_depth();
    let mut M = TMatrix::new();
    M.d = 1.0f64;
    M.a = M.d;
    M.c = 0.0f64;
    M.b = M.c;
    M.e = (*spe).x_user;
    M.f = (*spe).y_user;
    pdf_dev_concat(&mut M);
    let error = mps_exec_inline(
        &mut (*args).cur,
        (*spe).x_user,
        (*spe).y_user,
    );
    M.e = -(*spe).x_user;
    M.f = -(*spe).y_user;
    pdf_dev_concat(&mut M);
    if error != 0 {
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
const DVIPS_HANDLERS: [SpcHandler; 10] = [
    SpcHandler {
        key: b"header",
        exec: Some(spc_handler_ps_header),
    },
    SpcHandler {
        key: b"PSfile",
        exec: Some(spc_handler_ps_file),
    },
    SpcHandler {
        key: b"psfile",
        exec: Some(spc_handler_ps_file),
    },
    SpcHandler {
        key: b"ps: plotfile ",
        exec: Some(spc_handler_ps_plotfile),
    },
    SpcHandler {
        key: b"PS: plotfile ",
        exec: Some(spc_handler_ps_plotfile),
    },
    SpcHandler {
        key: b"PS:",
        exec: Some(spc_handler_ps_literal),
    },
    SpcHandler {
        key: b"ps:",
        exec: Some(spc_handler_ps_literal),
    },
    SpcHandler {
        key: b"PST:",
        exec: Some(spc_handler_ps_trickscmd),
    },
    SpcHandler {
        key: b"pst:",
        exec: Some(spc_handler_ps_tricksobj),
    },
    SpcHandler {
        key: b"\" ",
        exec: Some(spc_handler_ps_default),
    },
];


pub unsafe fn spc_dvips_at_begin_document() -> i32 {
    /* This function used to start the global_defs temp file. */
    0i32
}

pub unsafe fn spc_dvips_at_end_document() -> i32 {
    if !PS_HEADERS.is_null() {
        while NUM_PS_HEADERS > 0i32 {
            NUM_PS_HEADERS -= 1;
            free(*PS_HEADERS.offset(NUM_PS_HEADERS as isize) as *mut libc::c_void);
        }
        PS_HEADERS = mfree(PS_HEADERS as *mut libc::c_void) as *mut *mut i8
    }
    0i32
}

pub unsafe fn spc_dvips_at_begin_page() -> i32 {
    /* This function used do some things related to now-removed PSTricks functionality. */
    0i32
}

pub unsafe fn spc_dvips_at_end_page() -> i32 {
    mps_eop_cleanup();
    0i32
}
pub fn spc_dvips_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    if buf.is_empty() {
        return false;
    }
    for handler in DVIPS_HANDLERS.iter() {
        if buf.starts_with(handler.key) {
            return true;
        }
    }
    false
}

pub unsafe fn spc_dvips_setup_handler(
    mut handle: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut args: *mut spc_arg,
) -> i32 {
    assert!(!handle.is_null() && !spe.is_null() && !args.is_null());
    (*args).cur.skip_white();
    let key = (*args).cur;
    while !(*args).cur.is_empty() && ((*args).cur[0] as u8).is_ascii_alphabetic() {
        (*args).cur = &(*args).cur[1..];
    }
    /* Test for "ps:". The "ps::" special is subsumed under this case.  */
    if !(*args).cur.is_empty() && (*args).cur[0] == b':' {
        (*args).cur = &(*args).cur[1..];
        if (*args).cur.starts_with(b" plotfile ") {
            (*args).cur = &(*args).cur[b" plotfile ".len()..];
        }
    } else if (*args).cur.len() > 1 && (*args).cur[0] == b'\"' && (*args).cur[1] == b' ' {
        (*args).cur = &(*args).cur[2..];
    }
    let keylen = key.len() - (*args).cur.len();
    if keylen < 1 {
        spc_warn!(spe, "Not ps: special???");
        return -1i32;
    }
    for handler in DVIPS_HANDLERS.iter() {
        if keylen == handler.key.len() && &key[..keylen] == handler.key {
            (*args).cur.skip_white();
            (*args).command = Some(handler.key);
            (*handle).key = b"ps:";
            (*handle).exec = handler.exec;
            return 0i32;
        }
    }
    -1i32
}
