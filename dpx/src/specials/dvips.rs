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
use std::ffi::CStr;

use crate::mfree;
use crate::strstartswith;
use crate::warn;

use super::{spc_arg, spc_env};
use crate::TTInputFormat;

use crate::dpx_pdfdraw::pdf_dev_concat;
use crate::dpx_pdfobj::pdf_obj;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use crate::{ttstub_input_close, ttstub_input_open};

use super::util::spc_util_read_dimtrns;
use crate::dpx_mem::{new, xmalloc, xrealloc};
use crate::dpx_mpost::{mps_eop_cleanup, mps_exec_inline, mps_stack_depth};
use crate::dpx_pdfdev::{pdf_dev_put_image, pdf_tmatrix, transform_info, transform_info_clear};
use crate::dpx_pdfdraw::{
    pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to, pdf_dev_gsave,
};
use crate::dpx_pdfparse::skip_white;
use crate::spc_warn;
use libc::{free, memcpy, strlen, strncpy};

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
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr.offset(1) >= (*args).endptr || *(*args).curptr.offset(0) as i32 != '=' as i32
    {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return -1i32;
    }
    (*args).curptr = (*args).curptr.offset(1);
    let pro = xmalloc(
        ((*args).endptr.wrapping_offset_from((*args).curptr) as i64 + 1i32 as i64) as size_t,
    ) as *mut i8;
    strncpy(
        pro,
        (*args).curptr,
        (*args).endptr.wrapping_offset_from((*args).curptr) as _,
    );
    *pro.offset((*args).endptr.wrapping_offset_from((*args).curptr) as i64 as isize) = 0_i8;
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
    (*args).curptr = (*args).endptr;
    0i32
}
unsafe fn parse_filename(mut pp: *mut *const i8, mut endptr: *const i8) -> *mut i8 {
    let mut p: *const i8 = *pp;
    let mut qchar;
    if p.is_null() || p >= endptr {
        return 0 as *mut i8;
    } else {
        if *p as i32 == '\"' as i32 || *p as i32 == '\'' as i32 {
            let fresh2 = p;
            p = p.offset(1);
            qchar = *fresh2
        } else {
            qchar = ' ' as i32 as i8
        }
    }
    let mut n = 0i32;
    let q = p;
    while p < endptr && *p as i32 != qchar as i32 {
        /* nothing */
        n += 1;
        p = p.offset(1)
    }
    if qchar as i32 != ' ' as i32 {
        if *p as i32 != qchar as i32 {
            return 0 as *mut i8;
        }
        p = p.offset(1)
    }
    if q.is_null() || n == 0i32 {
        return 0 as *mut i8;
    }
    let r = new(((n + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    memcpy(r as *mut libc::c_void, q as *const libc::c_void, n as _);
    *r.offset(n as isize) = '\u{0}' as i32 as i8;
    *pp = p;
    r
}
/* =filename ... */
unsafe fn spc_handler_ps_file(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut ti = transform_info::new();
    let mut options: load_options = {
        let mut init = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: 0 as *mut pdf_obj,
        };
        init
    };
    assert!(!spe.is_null() && !args.is_null());
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr.offset(1) >= (*args).endptr || *(*args).curptr.offset(0) as i32 != '=' as i32
    {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return -1i32;
    }
    (*args).curptr = (*args).curptr.offset(1);
    let filename = parse_filename(&mut (*args).curptr, (*args).endptr);
    if filename.is_null() {
        spc_warn!(spe, "No filename specified for PSfile special.");
        return -1i32;
    }
    transform_info_clear(&mut ti);
    if spc_util_read_dimtrns(spe, &mut ti, args, 1i32) < 0i32 {
        free(filename as *mut libc::c_void);
        return -1i32;
    }
    let form_id = pdf_ximage_findresource(filename, options);
    if form_id < 0i32 {
        spc_warn!(
            spe,
            "Failed to read image file: {}",
            CStr::from_ptr(filename).display(),
        );
        free(filename as *mut libc::c_void);
        return -1i32;
    }
    free(filename as *mut libc::c_void);
    pdf_dev_put_image(form_id, &mut ti, (*spe).x_user, (*spe).y_user);
    0i32
}
/* This isn't correct implementation but dvipdfm supports... */
unsafe fn spc_handler_ps_plotfile(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32; /* xscale = 1.0, yscale = -1.0 */
    let mut p = transform_info::new();
    let mut options: load_options = {
        let mut init = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: 0 as *mut pdf_obj,
        };
        init
    };
    assert!(!spe.is_null() && !args.is_null());
    spc_warn!(spe, "\"ps: plotfile\" found (not properly implemented)");
    skip_white(&mut (*args).curptr, (*args).endptr);
    let filename = parse_filename(&mut (*args).curptr, (*args).endptr);
    if filename.is_null() {
        spc_warn!(spe, "Expecting filename but not found...");
        return -1i32;
    }
    let form_id = pdf_ximage_findresource(filename, options);
    if form_id < 0i32 {
        spc_warn!(
            spe,
            "Could not open PS file: {}",
            CStr::from_ptr(filename).display(),
        );
        error = -1i32
    } else {
        transform_info_clear(&mut p);
        p.matrix.d = -1.0f64;
        pdf_dev_put_image(form_id, &mut p, 0i32 as f64, 0i32 as f64);
    }
    free(filename as *mut libc::c_void);
    error
}
unsafe fn spc_handler_ps_literal(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    let mut error: i32 = 0i32;
    let x_user;
    let y_user;
    assert!(!spe.is_null() && !args.is_null() && (*args).curptr <= (*args).endptr);
    if (*args)
        .curptr
        .offset(strlen(b":[begin]\x00" as *const u8 as *const i8) as isize)
        <= (*args).endptr
        && !strstartswith((*args).curptr, b":[begin]\x00" as *const u8 as *const i8).is_null()
    {
        BLOCK_PENDING += 1;
        POSITION_SET = 1i32;
        PENDING_X = (*spe).x_user;
        x_user = PENDING_X;
        PENDING_Y = (*spe).y_user;
        y_user = PENDING_Y;
        (*args).curptr = (*args)
            .curptr
            .offset(strlen(b":[begin]\x00" as *const u8 as *const i8) as isize)
    } else if (*args)
        .curptr
        .offset(strlen(b":[end]\x00" as *const u8 as *const i8) as isize)
        <= (*args).endptr
        && !strstartswith((*args).curptr, b":[end]\x00" as *const u8 as *const i8).is_null()
    {
        if BLOCK_PENDING <= 0i32 {
            spc_warn!(spe, "No corresponding ::[begin] found.");
            return -1i32;
        }
        BLOCK_PENDING -= 1;
        POSITION_SET = 0i32;
        x_user = PENDING_X;
        y_user = PENDING_Y;
        (*args).curptr = (*args)
            .curptr
            .offset(strlen(b":[end]\x00" as *const u8 as *const i8) as isize)
    } else if (*args).curptr < (*args).endptr && *(*args).curptr.offset(0) as i32 == ':' as i32 {
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
        (*args).curptr = (*args).curptr.offset(1)
    } else {
        POSITION_SET = 1i32;
        PENDING_X = (*spe).x_user;
        x_user = PENDING_X;
        PENDING_Y = (*spe).y_user;
        y_user = PENDING_Y
    }
    skip_white(&mut (*args).curptr, (*args).endptr);
    if (*args).curptr < (*args).endptr {
        let st_depth = mps_stack_depth();
        let gs_depth = pdf_dev_current_depth();
        error = mps_exec_inline(&mut (*args).curptr, (*args).endptr, x_user, y_user);
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
    (*args).curptr = (*args).endptr;
    -1i32
}
unsafe fn spc_handler_ps_tricksobj(mut _spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    warn!("PSTricks commands are disallowed in Tectonic");
    (*args).curptr = (*args).endptr;
    -1i32
}
unsafe fn spc_handler_ps_default(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    assert!(!spe.is_null() && !args.is_null());
    pdf_dev_gsave();
    let st_depth = mps_stack_depth();
    let gs_depth = pdf_dev_current_depth();
    let mut M = pdf_tmatrix::new();
    M.d = 1.0f64;
    M.a = M.d;
    M.c = 0.0f64;
    M.b = M.c;
    M.e = (*spe).x_user;
    M.f = (*spe).y_user;
    pdf_dev_concat(&mut M);
    let error = mps_exec_inline(
        &mut (*args).curptr,
        (*args).endptr,
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

#[no_mangle]
pub unsafe extern "C" fn spc_dvips_at_begin_document() -> i32 {
    /* This function used to start the global_defs temp file. */
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvips_at_end_document() -> i32 {
    if !PS_HEADERS.is_null() {
        while NUM_PS_HEADERS > 0i32 {
            NUM_PS_HEADERS -= 1;
            free(*PS_HEADERS.offset(NUM_PS_HEADERS as isize) as *mut libc::c_void);
        }
        PS_HEADERS = mfree(PS_HEADERS as *mut libc::c_void) as *mut *mut i8
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvips_at_begin_page() -> i32 {
    /* This function used do some things related to now-removed PSTricks functionality. */
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvips_at_end_page() -> i32 {
    mps_eop_cleanup();
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvips_check_special(mut buf: *const i8, mut len: i32) -> bool {
    let mut p = buf;
    let endptr = p.offset(len as isize);
    skip_white(&mut p, endptr);
    if p >= endptr {
        return false;
    }
    len = endptr.wrapping_offset_from(p) as i64 as i32;
    for handler in DVIPS_HANDLERS.iter() {
        if len as usize >= handler.key.len()
            && CStr::from_ptr(p).to_bytes().starts_with(handler.key)
        {
            return true;
        }
    }
    false
}
#[no_mangle]
pub unsafe extern "C" fn spc_dvips_setup_handler(
    mut handle: *mut SpcHandler,
    mut spe: *mut spc_env,
    mut args: *mut spc_arg,
) -> i32 {
    assert!(!handle.is_null() && !spe.is_null() && !args.is_null());
    skip_white(&mut (*args).curptr, (*args).endptr);
    let key = (*args).curptr;
    while (*args).curptr < (*args).endptr && libc::isalpha(*(*args).curptr.offset(0) as _) != 0 {
        (*args).curptr = (*args).curptr.offset(1)
    }
    /* Test for "ps:". The "ps::" special is subsumed under this case.  */
    if (*args).curptr < (*args).endptr && *(*args).curptr.offset(0) as i32 == ':' as i32 {
        (*args).curptr = (*args).curptr.offset(1);
        if (*args)
            .curptr
            .offset(strlen(b" plotfile \x00" as *const u8 as *const i8) as isize)
            <= (*args).endptr
            && !strstartswith((*args).curptr, b" plotfile \x00" as *const u8 as *const i8).is_null()
        {
            (*args).curptr = (*args)
                .curptr
                .offset(strlen(b" plotfile \x00" as *const u8 as *const i8) as isize)
        }
    } else if (*args).curptr.offset(1) < (*args).endptr
        && *(*args).curptr.offset(0) as i32 == '\"' as i32
        && *(*args).curptr.offset(1) as i32 == ' ' as i32
    {
        (*args).curptr = (*args).curptr.offset(2)
    }
    let keylen = (*args).curptr.wrapping_offset_from(key) as usize;
    if keylen < 1 {
        spc_warn!(spe, "Not ps: special???");
        return -1i32;
    }
    for handler in DVIPS_HANDLERS.iter() {
        if keylen == handler.key.len() && &CStr::from_ptr(key).to_bytes()[..keylen] == handler.key {
            skip_white(&mut (*args).curptr, (*args).endptr);
            (*args).command = Some(handler.key);
            (*handle).key = b"ps:";
            (*handle).exec = handler.exec;
            return 0i32;
        }
    }
    -1i32
}
