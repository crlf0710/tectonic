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
#![allow(non_camel_case_types, unused_mut)]

pub mod color;
pub mod dvipdfmx;
pub mod dvips;
pub mod html;
pub mod misc;
pub mod pdfm;
pub mod tpic;
pub mod util;
pub mod xtx;

use euclid::point2;

use crate::warn;
use crate::DisplayExt;
use std::ffi::{CStr, CString};
use std::ptr;

use self::color::{spc_color_check_special, spc_color_setup_handler};
use self::dvipdfmx::{spc_dvipdfmx_check_special, spc_dvipdfmx_setup_handler};
use self::html::{
    spc_html_at_begin_document, spc_html_at_begin_page, spc_html_at_end_document,
    spc_html_at_end_page, spc_html_check_special, spc_html_setup_handler,
};
use self::misc::{spc_misc_check_special, spc_misc_setup_handler};
use self::pdfm::{
    spc_pdfm_at_begin_document, spc_pdfm_at_end_document, spc_pdfm_check_special,
    spc_pdfm_setup_handler,
};
use self::tpic::{
    spc_tpic_at_begin_document, spc_tpic_at_begin_page, spc_tpic_at_end_document,
    spc_tpic_at_end_page, spc_tpic_check_special, spc_tpic_setup_handler,
};
use self::xtx::{spc_xtx_check_special, spc_xtx_setup_handler};
use super::dpx_dvi::{dvi_dev_xpos, dvi_dev_ypos, dvi_link_annot, dvi_tag_depth, dvi_untag_depth};
use super::dpx_pdfdoc::{
    pdf_doc_begin_annot, pdf_doc_current_page_number, pdf_doc_current_page_resources,
    pdf_doc_end_annot, pdf_doc_get_dictionary, pdf_doc_get_reference, pdf_doc_ref_page,
};
use super::dpx_pdfdraw::pdf_dev_transform;
use super::dpx_pdfnames::{
    pdf_delete_name_tree, pdf_names_add_object, pdf_names_close_object, pdf_names_lookup_object,
    pdf_names_lookup_reference, pdf_new_name_tree,
};
use super::dpx_pdfparse::{dump_slice, SkipWhite};
use super::specials::dvips::{
    spc_dvips_at_begin_document, spc_dvips_at_begin_page, spc_dvips_at_end_document,
    spc_dvips_at_end_page, spc_dvips_check_special, spc_dvips_setup_handler,
};
use crate::dpx_pdfobj::{pdf_new_number, pdf_obj, pdf_ref_obj};
use crate::shims::sprintf;
use libc::{atoi, memcmp, strcmp, strlen};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct spc_env {
    pub x_user: f64,
    pub y_user: f64,
    pub mag: f64,
    pub pg: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct spc_arg<'a> {
    pub cur: &'a [u8],
    pub base: &'a [u8],
    pub command: Option<&'static [u8]>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct SpcHandler {
    pub key: &'static [u8],
    pub exec: Option<unsafe fn(_: *mut spc_env, _: *mut spc_arg) -> i32>,
}

use super::dpx_dpxutil::ht_table;

use super::dpx_pdfdev::Point;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Special {
    pub key: *const i8,
    pub bodhk_func: Option<unsafe fn() -> i32>,
    pub eodhk_func: Option<unsafe fn() -> i32>,
    pub bophk_func: Option<unsafe fn() -> i32>,
    pub eophk_func: Option<unsafe fn() -> i32>,
    pub check_func: fn(_: &[u8]) -> bool,
    pub setup_func:
        unsafe fn(_: *mut SpcHandler, _: *mut spc_env, _: *mut spc_arg) -> i32,
}
static mut VERBOSE: i32 = 0i32;
pub unsafe fn spc_set_verbose(mut level: i32) {
    VERBOSE = level;
}
/* This is currently just to make other spc_xxx to not directly
 * call dvi_xxx.
 */
pub unsafe fn spc_begin_annot(mut _spe: *mut spc_env, mut dict: *mut pdf_obj) -> i32 {
    pdf_doc_begin_annot(dict); /* Tell dvi interpreter to handle line-break. */
    dvi_tag_depth();
    0i32
}
pub unsafe fn spc_end_annot(mut _spe: *mut spc_env) -> i32 {
    dvi_untag_depth();
    pdf_doc_end_annot();
    0i32
}
pub unsafe fn spc_resume_annot(mut _spe: *mut spc_env) -> i32 {
    dvi_link_annot(1i32);
    0i32
}

pub unsafe fn spc_suspend_annot(mut _spe: *mut spc_env) -> i32 {
    dvi_link_annot(0i32);
    0i32
}
static mut NAMED_OBJECTS: *mut ht_table = ptr::null_mut();
/* reserved keys */
static mut _RKEYS: [*const i8; 11] = [
    b"xpos\x00" as *const u8 as *const i8,
    b"ypos\x00" as *const u8 as *const i8,
    b"thispage\x00" as *const u8 as *const i8,
    b"prevpage\x00" as *const u8 as *const i8,
    b"nextpage\x00" as *const u8 as *const i8,
    b"resources\x00" as *const u8 as *const i8,
    b"pages\x00" as *const u8 as *const i8,
    b"names\x00" as *const u8 as *const i8,
    b"catalog\x00" as *const u8 as *const i8,
    b"docinfo\x00" as *const u8 as *const i8,
    ptr::null(),
];
/* pageN where N is a positive integer.
 * Note that page need not exist at this time.
 */
unsafe fn ispageref(mut key: *const i8) -> i32 {
    if strlen(key) <= strlen(b"page\x00" as *const u8 as *const i8)
        || memcmp(
            key as *const libc::c_void,
            b"page\x00" as *const u8 as *const i8 as *const libc::c_void,
            strlen(b"page\x00" as *const u8 as *const i8),
        ) != 0
    {
        return 0i32;
    } else {
        let mut p = key.offset(4);
        while *p as i32 != 0 && *p as i32 >= '0' as i32 && *p as i32 <= '9' as i32 {
            p = p.offset(1)
        }
        if *p as i32 != '\u{0}' as i32 {
            return 0i32;
        }
    }
    1i32
}

pub unsafe fn spc_lookup_reference(mut key: &CString) -> Option<*mut pdf_obj> {
    assert!(!NAMED_OBJECTS.is_null());
    let value = match key.to_bytes() {
        b"xpos" => {
            /* xpos and ypos must be position in device space here. */
            let mut cp = point2(dvi_dev_xpos(), 0.);
            pdf_dev_transform(&mut cp, None);
            pdf_new_number((cp.x / 0.01 + 0.5).floor() * 0.01)
        }
        b"ypos" => {
            let mut cp = point2(0., dvi_dev_ypos());
            pdf_dev_transform(&mut cp, None);
            pdf_new_number((cp.y / 0.01 + 0.5).floor() * 0.01)
        }
        b"thispage" => pdf_doc_get_reference("@THISPAGE"),
        b"prevpage" => pdf_doc_get_reference("@PREVPAGE"),
        b"nextpage" => pdf_doc_get_reference("@NEXTPAGE"),
        b"pages" => pdf_ref_obj(pdf_doc_get_dictionary("Pages")),
        b"names" => pdf_ref_obj(pdf_doc_get_dictionary("Names")),
        b"resources" => pdf_ref_obj(pdf_doc_current_page_resources()),
        b"catalog" => pdf_ref_obj(pdf_doc_get_dictionary("Catalog")),
        b"docinfo" => pdf_ref_obj(pdf_doc_get_dictionary("Info")),
        _ => {
            let key = key.as_ptr();
            if ispageref(key) != 0 {
                pdf_doc_ref_page(atoi(key.offset(4)) as u32)
            } else {
                pdf_names_lookup_reference(
                    NAMED_OBJECTS,
                    key as *const libc::c_void,
                    strlen(key) as i32,
                )
            }
        }
    };
    if value.is_null() {
        panic!(
            "Object reference {} not exist.",
            key.display(),
        );
    }
    if value.is_null() {
        None
    } else {
        Some(value)
    }
}
pub unsafe fn spc_lookup_object(mut key: *const i8) -> *mut pdf_obj {
    assert!(!NAMED_OBJECTS.is_null());
    if key.is_null() {
        return ptr::null_mut();
    }
    let mut k = 0i32;
    while !_RKEYS[k as usize].is_null() && strcmp(key, _RKEYS[k as usize]) != 0 {
        k += 1
    }
    let value;
    match k {
        0 => {
            let mut cp = point2(dvi_dev_xpos(), 0.);
            pdf_dev_transform(&mut cp, None);
            value = pdf_new_number((cp.x / 0.01f64 + 0.5f64).floor() * 0.01f64)
        }
        1 => {
            let mut cp = point2(0., dvi_dev_ypos());
            pdf_dev_transform(&mut cp, None);
            value = pdf_new_number((cp.y / 0.01f64 + 0.5f64).floor() * 0.01f64)
        }
        2 => value = pdf_doc_get_dictionary("@THISPAGE"),
        6 => value = pdf_doc_get_dictionary("Pages"),
        7 => value = pdf_doc_get_dictionary("Names"),
        5 => value = pdf_doc_current_page_resources(),
        8 => value = pdf_doc_get_dictionary("Catalog"),
        9 => value = pdf_doc_get_dictionary("Info"),
        _ => {
            value = pdf_names_lookup_object(
                NAMED_OBJECTS,
                key as *const libc::c_void,
                strlen(key) as i32,
            )
        }
    }
    /* spc_handler_pdfm_bead() in spc_pdfm.c controls NULL too.
      if (!value) {
        panic!("Object reference %s not exist.", key);
      }
    */
    return value; /* _FIXME_ */
}
pub unsafe fn spc_push_object(mut key: *const i8, mut value: *mut pdf_obj) {
    assert!(!NAMED_OBJECTS.is_null());
    if key.is_null() || value.is_null() {
        return;
    }
    pdf_names_add_object(
        NAMED_OBJECTS,
        key as *const libc::c_void,
        strlen(key) as i32,
        value,
    );
}
pub unsafe fn spc_flush_object(mut key: *const i8) {
    pdf_names_close_object(
        NAMED_OBJECTS,
        key as *const libc::c_void,
        strlen(key) as i32,
    );
}
pub unsafe fn spc_clear_objects() {
    pdf_delete_name_tree(&mut NAMED_OBJECTS);
    NAMED_OBJECTS = pdf_new_name_tree();
}
unsafe fn spc_handler_unknown(mut spe: *mut spc_env, mut args: *mut spc_arg) -> i32 {
    assert!(!spe.is_null() && !args.is_null());
    (*args).cur = &[];
    -1i32
}
unsafe fn init_special<'a, 'b>(
    mut special: &mut SpcHandler,
    mut spe: &mut spc_env,
    mut args: &'a mut spc_arg<'b>,
    mut buf: &'b [u8],
    mut x_user: f64,
    mut y_user: f64,
    mut mag: f64,
) where 'b: 'a {
    special.key = &[];
    special.exec = Some(spc_handler_unknown);
    spe.x_user = x_user;
    spe.y_user = y_user;
    spe.mag = mag;
    spe.pg = pdf_doc_current_page_number();
    args.cur = buf;
    args.base = buf;
    args.command = None;
}
unsafe fn check_garbage(mut args: &mut spc_arg) {
    if args.cur.is_empty() {
        return;
    }
    args.cur.skip_white();
    if !args.cur.is_empty() {
        warn!("Unparsed material at end of special ignored.");
        dump_slice((*args).cur);
    };
}
const KNOWN_SPECIALS: [Special; 8] = [
    Special {
        key: b"pdf:\x00" as *const u8 as *const i8,
        bodhk_func: Some(spc_pdfm_at_begin_document),
        eodhk_func: Some(spc_pdfm_at_end_document),
        bophk_func: None,
        eophk_func: None,
        check_func: spc_pdfm_check_special,
        setup_func: spc_pdfm_setup_handler,
    },
    Special {
        key: b"x:\x00" as *const u8 as *const i8,
        bodhk_func: None,
        eodhk_func: None,
        bophk_func: None,
        eophk_func: None,
        check_func: spc_xtx_check_special,
        setup_func: spc_xtx_setup_handler,
    },
    Special {
        key: b"dvipdfmx:\x00" as *const u8 as *const i8,
        bodhk_func: None,
        eodhk_func: None,
        bophk_func: None,
        eophk_func: None,
        check_func: spc_dvipdfmx_check_special,
        setup_func: spc_dvipdfmx_setup_handler,
    },
    Special {
        key: b"ps:\x00" as *const u8 as *const i8,
        bodhk_func: Some(spc_dvips_at_begin_document),
        eodhk_func: Some(spc_dvips_at_end_document),
        bophk_func: Some(spc_dvips_at_begin_page),
        eophk_func: Some(spc_dvips_at_end_page),
        check_func: spc_dvips_check_special,
        setup_func: spc_dvips_setup_handler,
    },
    Special {
        key: b"color\x00" as *const u8 as *const i8,
        bodhk_func: None,
        eodhk_func: None,
        bophk_func: None,
        eophk_func: None,
        check_func: spc_color_check_special,
        setup_func: spc_color_setup_handler,
    },
    Special {
        key: b"tpic\x00" as *const u8 as *const i8,
        bodhk_func: Some(spc_tpic_at_begin_document),
        eodhk_func: Some(spc_tpic_at_end_document),
        bophk_func: Some(spc_tpic_at_begin_page),
        eophk_func: Some(spc_tpic_at_end_page),
        check_func: spc_tpic_check_special,
        setup_func: spc_tpic_setup_handler,
    },
    Special {
        key: b"html:\x00" as *const u8 as *const i8,
        bodhk_func: Some(spc_html_at_begin_document),
        eodhk_func: Some(spc_html_at_end_document),
        bophk_func: Some(spc_html_at_begin_page),
        eophk_func: Some(spc_html_at_end_page),
        check_func: spc_html_check_special,
        setup_func: spc_html_setup_handler,
    },
    Special {
        key: b"unknown\x00" as *const u8 as *const i8,
        bodhk_func: None,
        eodhk_func: None,
        bophk_func: None,
        eophk_func: None,
        check_func: spc_misc_check_special,
        setup_func: spc_misc_setup_handler,
    },
];
pub unsafe fn spc_exec_at_begin_page() -> i32 {
    let mut error: i32 = 0i32;
    for spc in &KNOWN_SPECIALS {
        if let Some(bophk) = spc.bophk_func {
            error = bophk();
        }
    }
    error
}
pub unsafe fn spc_exec_at_end_page() -> i32 {
    let mut error: i32 = 0i32;
    for spc in &KNOWN_SPECIALS {
        if let Some(eophk) = spc.eophk_func {
            error = eophk();
        }
    }
    error
}
pub unsafe fn spc_exec_at_begin_document() -> i32 {
    let mut error: i32 = 0i32;
    assert!(NAMED_OBJECTS.is_null());
    NAMED_OBJECTS = pdf_new_name_tree();
    for spc in &KNOWN_SPECIALS {
        if let Some(bodhk) = spc.bodhk_func {
            error = bodhk();
        }
    }
    error
}
pub unsafe fn spc_exec_at_end_document() -> i32 {
    let mut error: i32 = 0i32;

    for spc in &KNOWN_SPECIALS {
        if let Some(eodhk) = spc.eodhk_func {
            error = eodhk();
        }
    }
    if !NAMED_OBJECTS.is_null() {
        pdf_delete_name_tree(&mut NAMED_OBJECTS);
    }
    error
}
unsafe fn print_error(mut name: *const i8, mut spe: *mut spc_env, mut ap: *mut spc_arg) {
    let mut ebuf: [u8; 64] = [0; 64];
    let mut pg: i32 = (*spe).pg;
    let mut c = point2((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut c, None);
    if (*ap).command.is_some() && !name.is_null() {
        warn!(
            "Interpreting special command {} ({}) failed.",
            (*ap).command.unwrap().display(),
            CStr::from_ptr(name).display(),
        );
        warn!(
            ">> at page=\"{}\" position=\"({}, {})\" (in PDF)",
            pg, c.x, c.y,
        );
    }
    let mut i = 0;
    for &b in (*ap).base {
        if i >= 63 {
            break;
        }
        if libc::isprint(b as _) != 0 {
            ebuf[i] = b;
            i += 1;
        } else {
            if !(i + 4 < 63) {
                break;
            }
            i += sprintf(
                ebuf.as_mut_ptr().offset(i as isize) as *mut i8,
                b"\\x%02x\x00" as *const u8 as *const i8,
                b as i32,
            ) as usize;
        }
    }
    ebuf[i] = 0;
    if !(*ap).cur.is_empty() {
        loop {
            let fresh1 = i;
            i = i - 1;
            if !(fresh1 > 60) {
                break;
            }
            ebuf[i] = b'.';
        }
    }
    warn!(">> xxx \"{}\"", CStr::from_ptr(ebuf.as_ptr() as *const i8).display());
    if !(*ap).cur.is_empty() {
        i = 0;
        for &b in (*ap).cur {
            if i >= 63 {
                break;
            }
            if libc::isprint(b as _) != 0 {
                ebuf[i] = b;
                i += 1;
            } else {
                if !(i + 4 < 63) {
                    break;
                }
                i += sprintf(
                    ebuf.as_mut_ptr().offset(i as isize) as *mut i8,
                    b"\\x%02x\x00" as *const u8 as *const i8,
                    b as i32,
                ) as usize;
            }
        }
        ebuf[i] = 0;
        if !(*ap).cur.is_empty() {
            loop {
                let fresh3 = i;
                i = i - 1;
                if !(fresh3 > 60) {
                    break;
                }
                ebuf[i] = b'.' as u8
            }
        }
        warn!(
            ">> Reading special command stopped around >>{}<<",
            CStr::from_ptr(ebuf.as_ptr() as *const i8).display()
        );
        (*ap).cur = &[];
    };
}
/* current page in PDF */
/* This should not use pdf_. */
/* PDF parser shouldn't depend on this...
 */
pub unsafe fn spc_exec_special(
    buffer: &[u8],
    mut x_user: f64,
    mut y_user: f64,
    mut mag: f64,
) -> i32 {
    let mut error: i32 = -1i32;
    let mut spe: spc_env = spc_env {
        x_user: 0.,
        y_user: 0.,
        mag: 0.,
        pg: 0,
    };
    let mut args: spc_arg = spc_arg {
        cur: &[],
        base: &[],
        command: None,
    };
    let mut special = SpcHandler {
        key: &[],
        exec: None,
    };
    if VERBOSE > 3 {
        dump_slice(buffer);
    }
    init_special(
        &mut special,
        &mut spe,
        &mut args,
        buffer,
        x_user,
        y_user,
        mag,
    );

    for spc in &KNOWN_SPECIALS {
        let found = (spc.check_func)(buffer);
        if found {
            error = (spc.setup_func)(&mut special, &mut spe, &mut args);
            if error == 0 {
                error = special.exec.expect("non-null function pointer")(&mut spe, &mut args)
            }
            if error != 0 {
                print_error(spc.key, &mut spe, &mut args);
            }
            break;
        }
    }
    check_garbage(&mut args);
    error
}
