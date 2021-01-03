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

pub(crate) mod color;
pub(crate) mod dvipdfmx;
pub(crate) mod dvips;
pub(crate) mod html;
pub(crate) mod misc;
pub(crate) mod pdfm;
pub(crate) mod tpic;
pub(crate) mod util;
pub(crate) mod xtx;

use euclid::point2;

use crate::bridge::DisplayExt;
use crate::warn;
use std::ffi::CStr;
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
use super::dpx_pdfparse::{dump, SkipWhite};
use super::specials::dvips::{
    spc_dvips_at_begin_document, spc_dvips_at_begin_page, spc_dvips_at_end_document,
    spc_dvips_at_end_page, spc_dvips_check_special, spc_dvips_setup_handler,
};
use crate::dpx_pdfobj::{pdf_obj, pdf_ref_obj, IntoObj};
use crate::shims::sprintf;

#[derive(Copy, Clone, Default)]
pub(crate) struct SpcEnv {
    pub(crate) x_user: f64,
    pub(crate) y_user: f64,
    pub(crate) mag: f64,
    pub(crate) pg: i32,
}
#[derive(Copy, Clone, Default)]
pub(crate) struct SpcArg<'a> {
    pub(crate) cur: &'a [u8],
    pub(crate) base: &'a [u8],
    pub(crate) command: Option<&'static str>,
}
#[derive(Copy, Clone, Default)]
pub(crate) struct SpcHandler {
    pub(crate) key: &'static str,
    pub(crate) exec: Option<unsafe fn(_: &mut SpcEnv, _: &mut SpcArg) -> i32>,
}

use super::dpx_dpxutil::ht_table;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct Special {
    pub(crate) key: *const i8,
    pub(crate) bodhk_func: Option<unsafe fn() -> i32>,
    pub(crate) eodhk_func: Option<unsafe fn() -> i32>,
    pub(crate) bophk_func: Option<unsafe fn() -> i32>,
    pub(crate) eophk_func: Option<unsafe fn() -> i32>,
    pub(crate) check_func: fn(_: &[u8]) -> bool,
    pub(crate) setup_func: unsafe fn(_: &mut SpcHandler, _: &mut SpcEnv, _: &mut SpcArg) -> i32,
}
static mut VERBOSE: i32 = 0i32;
pub(crate) unsafe fn spc_set_verbose(level: i32) {
    VERBOSE = level;
}
/* This is currently just to make other spc_xxx to not directly
 * call dvi_xxx.
 */
pub(crate) unsafe fn spc_begin_annot(mut _spe: &mut SpcEnv, dict: *mut pdf_obj) -> i32 {
    pdf_doc_begin_annot(dict); /* Tell dvi interpreter to handle line-break. */
    dvi_tag_depth();
    0i32
}
pub(crate) unsafe fn spc_end_annot(mut _spe: &mut SpcEnv) -> i32 {
    dvi_untag_depth();
    pdf_doc_end_annot();
    0i32
}
pub(crate) unsafe fn spc_resume_annot(mut _spe: &mut SpcEnv) -> i32 {
    dvi_link_annot(1i32);
    0i32
}

pub(crate) unsafe fn spc_suspend_annot(mut _spe: &mut SpcEnv) -> i32 {
    dvi_link_annot(0i32);
    0i32
}
static mut NAMED_OBJECTS: *mut ht_table = ptr::null_mut();

/* pageN where N is a positive integer.
 * Note that page need not exist at this time.
 */
unsafe fn ispageref(key: &str) -> bool {
    if !key.starts_with("page") {
        return false;
    } else {
        let mut p = &key[4..];
        while !p.is_empty() && (b'0'..=b'9').contains(&p.as_bytes()[0]) {
            p = &p[1..];
        }
        if !p.is_empty() {
            return false;
        }
    }
    true
}

pub(crate) unsafe fn spc_lookup_reference(key: &str) -> Option<*mut pdf_obj> {
    assert!(!NAMED_OBJECTS.is_null());
    let value = match key {
        "xpos" => {
            /* xpos and ypos must be position in device space here. */
            let mut cp = point2(dvi_dev_xpos(), 0.);
            pdf_dev_transform(&mut cp, None);
            ((cp.x / 0.01 + 0.5).floor() * 0.01).into_obj()
        }
        "ypos" => {
            let mut cp = point2(0., dvi_dev_ypos());
            pdf_dev_transform(&mut cp, None);
            ((cp.y / 0.01 + 0.5).floor() * 0.01).into_obj()
        }
        "thispage" => pdf_doc_get_reference("@THISPAGE"),
        "prevpage" => pdf_doc_get_reference("@PREVPAGE"),
        "nextpage" => pdf_doc_get_reference("@NEXTPAGE"),
        "pages" => pdf_ref_obj(pdf_doc_get_dictionary("Pages")),
        "names" => pdf_ref_obj(pdf_doc_get_dictionary("Names")),
        "resources" => pdf_ref_obj(pdf_doc_current_page_resources()),
        "catalog" => pdf_ref_obj(pdf_doc_get_dictionary("Catalog")),
        "docinfo" => pdf_ref_obj(pdf_doc_get_dictionary("Info")),
        _ => {
            if ispageref(key) {
                pdf_doc_ref_page((key[4..]).parse::<i32>().unwrap() as usize)
            } else {
                pdf_names_lookup_reference(
                    NAMED_OBJECTS,
                    key.as_ptr() as *const libc::c_void,
                    key.len() as i32,
                )
            }
        }
    };
    if value.is_null() {
        panic!("Object reference {} not exist.", key);
    }
    if value.is_null() {
        None
    } else {
        Some(value)
    }
}
pub(crate) unsafe fn spc_lookup_object(key: &str) -> *mut pdf_obj {
    assert!(!NAMED_OBJECTS.is_null());
    if key.is_empty() {
        return ptr::null_mut();
    }
    let value;
    match key {
        "xpos" => {
            let mut cp = point2(dvi_dev_xpos(), 0.);
            pdf_dev_transform(&mut cp, None);
            value = ((cp.x / 0.01 + 0.5).floor() * 0.01).into_obj()
        }
        "ypos" => {
            let mut cp = point2(0., dvi_dev_ypos());
            pdf_dev_transform(&mut cp, None);
            value = ((cp.y / 0.01 + 0.5).floor() * 0.01).into_obj()
        }
        "thispage" => value = pdf_doc_get_dictionary("@THISPAGE"),
        "pages" => value = pdf_doc_get_dictionary("Pages"),
        "names" => value = pdf_doc_get_dictionary("Names"),
        "resources" => value = pdf_doc_current_page_resources(),
        "catalog" => value = pdf_doc_get_dictionary("Catalog"),
        "docinfo" => value = pdf_doc_get_dictionary("Info"),
        _ => {
            value = pdf_names_lookup_object(
                NAMED_OBJECTS,
                key.as_ptr() as *const libc::c_void,
                key.len() as i32,
            )
        }
    }
    /* spc_handler_pdfm_bead() in spc_pdfm.c controls NULL too.
      if (!value) {
        panic!("Object reference %s not exist.", key);
      }
    */
    value /* _FIXME_ */
}
pub(crate) unsafe fn spc_push_object(key: &str, value: *mut pdf_obj) {
    assert!(!NAMED_OBJECTS.is_null());
    if key.is_empty() || value.is_null() {
        return;
    }
    pdf_names_add_object(
        NAMED_OBJECTS,
        key.as_ptr() as *const libc::c_void,
        key.len() as i32,
        value,
    );
}
pub(crate) unsafe fn spc_flush_object(key: &str) {
    pdf_names_close_object(
        NAMED_OBJECTS,
        key.as_ptr() as *const libc::c_void,
        key.len() as i32,
    );
}
pub(crate) unsafe fn spc_clear_objects() {
    pdf_delete_name_tree(&mut NAMED_OBJECTS);
    NAMED_OBJECTS = pdf_new_name_tree();
}
unsafe fn spc_handler_unknown(_spe: &mut SpcEnv, args: &mut SpcArg) -> i32 {
    args.cur = &[];
    -1i32
}
unsafe fn init_special<'a, 'b>(
    special: &mut SpcHandler,
    mut spe: &mut SpcEnv,
    mut args: &'a mut SpcArg<'b>,
    buf: &'b [u8],
    x_user: f64,
    y_user: f64,
    mag: f64,
) where
    'b: 'a,
{
    *special = SpcHandler {
        key: "",
        exec: Some(spc_handler_unknown),
    };
    spe.x_user = x_user;
    spe.y_user = y_user;
    spe.mag = mag;
    spe.pg = pdf_doc_current_page_number() as i32;
    args.cur = buf;
    args.base = buf;
    args.command = None;
}
unsafe fn check_garbage(args: &mut SpcArg) {
    if args.cur.is_empty() {
        return;
    }
    args.cur.skip_white();
    if !args.cur.is_empty() {
        warn!("Unparsed material at end of special ignored.");
        dump(args.cur);
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
pub(crate) unsafe fn spc_exec_at_begin_page() -> i32 {
    let mut error: i32 = 0i32;
    for spc in &KNOWN_SPECIALS {
        if let Some(bophk) = spc.bophk_func {
            error = bophk();
        }
    }
    error
}
pub(crate) unsafe fn spc_exec_at_end_page() -> i32 {
    let mut error: i32 = 0i32;
    for spc in &KNOWN_SPECIALS {
        if let Some(eophk) = spc.eophk_func {
            error = eophk();
        }
    }
    error
}
pub(crate) unsafe fn spc_exec_at_begin_document() -> i32 {
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
pub(crate) unsafe fn spc_exec_at_end_document() -> i32 {
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
unsafe fn print_error(name: *const i8, spe: &mut SpcEnv, ap: &mut SpcArg) {
    let mut ebuf: [u8; 64] = [0; 64];
    let pg: i32 = spe.pg;
    let mut c = point2(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut c, None);
    if ap.command.is_some() && !name.is_null() {
        warn!(
            "Interpreting special command {} ({}) failed.",
            ap.command.unwrap(),
            CStr::from_ptr(name).display(),
        );
        warn!(
            ">> at page=\"{}\" position=\"({}, {})\" (in PDF)",
            pg, c.x, c.y,
        );
    }
    let mut i = 0;
    for &b in ap.base {
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
    if !ap.cur.is_empty() {
        for j in 60..i {
            ebuf[j] = b'.';
        }
    }
    warn!(
        ">> xxx \"{}\"",
        CStr::from_ptr(ebuf.as_ptr() as *const i8).display()
    );
    if !ap.cur.is_empty() {
        let mut i = 0;
        for &b in ap.cur {
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
        if !ap.cur.is_empty() {
            for j in 60..i {
                ebuf[j] = b'.';
            }
        }
        warn!(
            ">> Reading special command stopped around >>{}<<",
            CStr::from_ptr(ebuf.as_ptr() as *const i8).display()
        );
        ap.cur = &[];
    };
}
/* current page in PDF */
/* This should not use pdf_. */
/* PDF parser shouldn't depend on this...
 */
pub(crate) unsafe fn spc_exec_special(buffer: &[u8], x_user: f64, y_user: f64, mag: f64) -> i32 {
    let mut error: i32 = -1i32;
    let mut spe = SpcEnv::default();
    let mut args = SpcArg::default();
    let mut special = SpcHandler::default();
    if VERBOSE > 3 {
        dump(buffer);
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
