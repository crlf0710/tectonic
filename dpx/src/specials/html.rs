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
)]

use crate::DisplayExt;
use std::ffi::{CStr, CString};
use std::ptr;

use crate::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_transform};
use crate::dpx_pdfximage::{
    pdf_ximage_findresource, pdf_ximage_get_reference, pdf_ximage_get_resname,
    pdf_ximage_scale_image,
};

use super::{spc_begin_annot, spc_end_annot};
use crate::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use crate::dpx_mem::new;
use crate::dpx_pdfdev::{graphics_mode, transform_info, transform_info_clear, Rect, TMatrix};
use crate::dpx_pdfdoc::{
    pdf_doc_add_names, pdf_doc_add_page_content, pdf_doc_add_page_resource,
    pdf_doc_current_page_resources, pdf_doc_get_reference,
};
use crate::dpx_pdfdraw::{pdf_dev_grestore, pdf_dev_gsave, pdf_dev_rectclip};
use crate::dpx_pdfobj::{
    pdf_link_obj, pdf_new_dict, pdf_new_null, pdf_new_string, pdf_obj, pdf_ref_obj,
    pdf_release_obj, pdf_string_value, IntoObj, PushObj,
};
use crate::spc_warn;
use libc::{atof, free, strcat, strcpy, strlen};

use super::{spc_arg, spc_env};

use super::SpcHandler;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct spc_html_ {
    pub opts: C2RustUnnamed_0,
    pub link_dict: *mut pdf_obj,
    pub baseurl: *mut i8,
    pub pending_type: i32,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub extensions: i32,
}

use crate::dpx_pdfximage::load_options;

use crate::dpx_pdfdev::Point;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut _HTML_STATE: spc_html_ = spc_html_ {
    opts: C2RustUnnamed_0 { extensions: 0i32 },
    link_dict: ptr::null_mut(),
    baseurl: ptr::null_mut(),
    pending_type: -1i32,
};
/* ENABLE_HTML_SVG_TRANSFORM */
unsafe fn parse_key_val(pp: &mut &[u8]) -> Result<(CString, CString), ()> {
    let mut error: i32 = 0i32;
    let mut p = *pp; /* include trailing NULL here!!! */
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    let mut v = None; /* Should be checked somewhere else */
    let mut q = p; /* skip '="' */
                   
    let mut n = 0; /* Assume this is URL */
    while !p.is_empty() && (p[0].is_ascii_alphanumeric() || p[0] == b'-' || p[0] == b':') {
        n += 1;
        p = &p[1..];
    }
    if n == 0 {
        return Err(());
    }
    let mut k = Some(CString::new(&q[..n]).unwrap());
    if p.len() <= 2 || p[0] != b'=' || p[1] != b'\"' && p[1] != b'\'' {
        k = None;
        *pp = p;
        error = -1
    } else {
        let qchr = p[1];
        p = &p[2..];
        q = p;
        let mut n = 0;
        while !p.is_empty() && p[0] != qchr {
            p = &p[1..];
            n += 1
        }
        if p.is_empty() || p[0] != qchr {
            error = -1
        } else {
            v = Some(CString::new(&q[..n]).unwrap());
            p = &p[1..];
        }
    }
    *pp = p;
    if error == -1 {
        Err(())
    } else {
        Ok((k.unwrap(), v.unwrap()))
    }
}

unsafe fn read_html_tag(
    attr: &mut pdf_obj,
    type_0: &mut i32,
    pp: &mut &[u8],
) -> Result<Vec<u8>, ()> {
    let mut p = *pp;
    let mut error: i32 = 0i32;
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    if p.is_empty() || p[0] != b'<' {
        return Err(());
    }
    *type_0 = 1;
    p = &p[1..];
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    if !p.is_empty() && p[0] == b'/' {
        *type_0 = 2;
        p = &p[1..];
        while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
            p = &p[1..];
        }
    }
    let mut name = Vec::with_capacity(p.len().min(127));
    let mut n = 0;
    while !p.is_empty()
        && n < 127
        && !(p[0] == b'>' || p[0] == b'/' || libc::isspace(p[0] as _) != 0)
    {
        name.push(p[0]);
        n += 1;
        p = &p[1..];
    }
    if n == 0 || p.is_empty() || !(p[0] == b'>' || p[0] == b'/' || libc::isspace(p[0] as _) != 0) {
        *pp = p;
        return Err(());
    }
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    while !p.is_empty() && error == 0 && p[0] != b'/' && p[0] != b'>' {
        if let Ok((kp, vp)) = parse_key_val(&mut p) {
            attr.as_dict_mut().set(
                kp.to_bytes().to_ascii_lowercase(),
                pdf_new_string(
                    vp.as_ptr() as *const libc::c_void,
                    (vp.to_bytes().len() + 1) as _,
                ),
            );
        } else {
            error = -1;
        }
        while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
            p = &p[1..];
        }
    }
    if error != 0 {
        *pp = p;
        return Err(());
    }
    if !p.is_empty() && p[0] == b'/' {
        *type_0 = 1;
        p = &p[1..];
        while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
            p = &p[1..];
        }
    }
    if p.is_empty() || p[0] != b'>' {
        *pp = p;
        return Err(());
    }
    p = &p[1..];
    name.make_ascii_lowercase();
    *pp = p;
    Ok(name)
}

unsafe fn spc_handler_html__init(dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_html_ = dp as *mut spc_html_;
    (*sd).link_dict = ptr::null_mut();
    (*sd).baseurl = ptr::null_mut();
    (*sd).pending_type = -1i32;
    0i32
}

unsafe fn spc_handler_html__clean(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let mut sd: *mut spc_html_ = dp as *mut spc_html_;
    free((*sd).baseurl as *mut libc::c_void);
    if (*sd).pending_type >= 0i32 || !(*sd).link_dict.is_null() {
        spc_warn!(spe, "Unclosed html anchor found.");
    }
    pdf_release_obj((*sd).link_dict);
    (*sd).pending_type = -1i32;
    (*sd).baseurl = ptr::null_mut();
    (*sd).link_dict = ptr::null_mut();
    0i32
}

unsafe fn spc_handler_html__bophook(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let sd: *mut spc_html_ = dp as *mut spc_html_;
    if (*sd).pending_type >= 0i32 {
        spc_warn!(
            spe,
            "...html anchor continues from previous page processed..."
        );
    }
    0i32
}

unsafe fn spc_handler_html__eophook(spe: *mut spc_env, dp: *mut libc::c_void) -> i32 {
    let sd: *mut spc_html_ = dp as *mut spc_html_;
    if (*sd).pending_type >= 0i32 {
        spc_warn!(spe, "Unclosed html anchor at end-of-page!");
    }
    0i32
}

unsafe fn fqurl(baseurl: *const i8, name: *const i8) -> *mut i8 {
    let mut len = strlen(name) as i32;
    if !baseurl.is_null() {
        len = (len as usize).wrapping_add(strlen(baseurl).wrapping_add(1)) as _
    }
    let q =
        new(((len + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    *q = '\u{0}' as i32 as i8;
    if !baseurl.is_null() && *baseurl.offset(0) as i32 != 0 {
        strcpy(q, baseurl);
        let p = q.offset(strlen(q) as isize).offset(-1);
        if *p as i32 == '/' as i32 {
            *p = '\u{0}' as i32 as i8
        }
        if *name.offset(0) as i32 != 0 && *name.offset(0) as i32 != '/' as i32 {
            strcat(q, b"/\x00" as *const u8 as *const i8);
        }
    }
    strcat(q, name);
    q
}

unsafe fn html_open_link(spe: *mut spc_env, name: *const i8, mut sd: *mut spc_html_) -> i32 {
    assert!(!name.is_null());
    assert!((*sd).link_dict.is_null());
    (*sd).link_dict = pdf_new_dict();
    (*(*sd).link_dict).as_dict_mut().set("Type", "Annot");
    (*(*sd).link_dict).as_dict_mut().set("Subtype", "Link");
    let mut color = vec![];
    color.push_obj(0f64);
    color.push_obj(0f64);
    color.push_obj(1f64);
    (*(*sd).link_dict).as_dict_mut().set("C", color.into_obj());
    let url = fqurl((*sd).baseurl, name);
    if *url.offset(0) as i32 == '#' as i32 {
        /* url++; causes memory leak in free(url) */
        (*(*sd).link_dict).as_dict_mut().set(
            "Dest",
            pdf_new_string(
                url.offset(1) as *const libc::c_void,
                strlen(url.offset(1)) as _,
            ),
        ); /* Otherwise must be bug */
    } else {
        let action: *mut pdf_obj = pdf_new_dict();
        (*action).as_dict_mut().set("Type", "Action");
        (*action).as_dict_mut().set("S", "URI");
        (*action).as_dict_mut().set(
            "URI",
            pdf_new_string(url as *const libc::c_void, strlen(url) as _),
        );
        (*(*sd).link_dict)
            .as_dict_mut()
            .set("A", pdf_link_obj(action));
        pdf_release_obj(action);
    }
    free(url as *mut libc::c_void);
    spc_begin_annot(spe, (*sd).link_dict);
    (*sd).pending_type = 0i32;
    0i32
}

unsafe fn html_open_dest(spe: *mut spc_env, name: *const i8, mut sd: *mut spc_html_) -> i32 {
    let mut cp = Point::new((*spe).x_user, (*spe).y_user);
    pdf_dev_transform(&mut cp, None);
    let page_ref = pdf_doc_get_reference("@THISPAGE");
    assert!(!page_ref.is_null());
    let mut array = vec![];
    array.push(page_ref);
    array.push_obj("XYZ");
    array.push(pdf_new_null());
    array.push_obj(cp.y + 24.);
    array.push(pdf_new_null());
    let error = pdf_doc_add_names(
        b"Dests\x00" as *const u8 as *const i8,
        name as *const libc::c_void,
        strlen(name) as i32,
        array.into_obj(),
    );
    if error != 0 {
        spc_warn!(
            spe,
            "Failed to add named destination: {}",
            CStr::from_ptr(name).display(),
        );
    }
    (*sd).pending_type = 1i32;
    error
}

unsafe fn spc_html__anchor_open(spe: *mut spc_env, attr: &pdf_obj, sd: *mut spc_html_) -> i32 {
    if (*sd).pending_type >= 0i32 || !(*sd).link_dict.is_null() {
        spc_warn!(spe, "Nested html anchors found!");
        return -1i32;
    }
    let href = attr.as_dict().get("href");
    let name = attr.as_dict().get("name");
    match (href, name) {
        (Some(_), Some(_)) => {
            spc_warn!(
                spe,
                "Sorry, you can\'t have both \"href\" and \"name\" in anchor tag..."
            );
            -1i32
        }
        (Some(href), None) => html_open_link(spe, pdf_string_value(href) as *const i8, sd),
        (None, Some(name)) => {
            /* name */
            html_open_dest(spe, pdf_string_value(name) as *const i8, sd)
        }
        _ => {
            spc_warn!(spe, "You should have \"href\" or \"name\" in anchor tag!");
            -1i32
        }
    }
}

unsafe fn spc_html__anchor_close(spe: *mut spc_env, mut sd: *mut spc_html_) -> i32 {
    let mut error: i32 = 0i32;
    match (*sd).pending_type {
        0 => {
            if !(*sd).link_dict.is_null() {
                spc_end_annot(spe);
                pdf_release_obj((*sd).link_dict);
                (*sd).link_dict = ptr::null_mut();
                (*sd).pending_type = -1i32
            } else {
                spc_warn!(spe, "Closing html anchor (link) without starting!");
                error = -1i32
            }
        }
        1 => (*sd).pending_type = -1i32,
        _ => {
            spc_warn!(spe, "No corresponding opening tag for html anchor.");
            error = -1i32
        }
    }
    error
}

unsafe fn spc_html__base_empty(spe: *mut spc_env, attr: &pdf_obj, mut sd: *mut spc_html_) -> i32 {
    let href = attr.as_dict().get("href");
    if href.is_none() {
        spc_warn!(spe, "\"href\" not found for \"base\" tag!");
        return -1i32;
    }
    let href = href.unwrap();
    let vp = pdf_string_value(&*href) as *mut i8;
    if !(*sd).baseurl.is_null() {
        spc_warn!(
            spe,
            "\"baseurl\" changed: \"{}\" --> \"{}\"",
            CStr::from_ptr((*sd).baseurl).display(),
            CStr::from_ptr(vp).display(),
        );
        free((*sd).baseurl as *mut libc::c_void);
    }
    (*sd).baseurl =
        new((strlen(vp).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    strcpy((*sd).baseurl, vp);
    0i32
}
/* This isn't completed.
 * Please think about placement of images.
 */
/* XXX: there are four quasi-redundant versions of this; grp for K_UNIT__PT */
unsafe fn atopt(a: &[u8]) -> f64 {
    let mut p = a;
    let mut u = 1.0f64;
    let q = p.parse_float_decimal();
    if q.is_none() {
        warn!(
            "Invalid length value: {} ({})",
            a.display(),
            char::from(p[0]),
        );
        return 0.0f64;
    }
    let v = atof(q.unwrap().as_ptr());
    if let Some(q) = p.parse_c_ident() {
        match q.to_bytes() {
            b"pt" => u *= 72.0f64 / 72.27f64,
            b"in" => u *= 72.0f64,
            b"cm" => u *= 72.0f64 / 2.54f64,
            b"mm" => u *= 72.0f64 / 25.4f64,
            b"bp" => u *= 1.0f64,
            b"pc" => u *= 12.0f64 * 72.0f64 / 72.27f64,
            b"dd" => u *= 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
            b"cc" => u *= 12.0f64 * 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
            b"sp" => u *= 72.0f64 / (72.27f64 * 65536i32 as f64),
            b"px" => u *= 1.0f64,
            _ => {
                warn!("Unknown unit of measure: {}", q.display());
            }
        }
    }
    v * u
}
/* Replicated from spc_tpic */
unsafe fn create_xgstate(a: f64, f_ais: i32) -> *mut pdf_obj
/* alpha is shape */ {
    let dict = pdf_new_dict();
    (*dict).as_dict_mut().set("Type", "ExtGState");
    if f_ais != 0 {
        (*dict).as_dict_mut().set("AIS", true);
    }
    (*dict).as_dict_mut().set("ca", a);
    dict
}
unsafe fn check_resourcestatus(category: &str, resname: &str) -> i32 {
    let dict1 = pdf_doc_current_page_resources();
    if dict1.is_null() {
        return 0i32;
    }
    if let Some(dict2) = (*dict1).as_dict().get(category) {
        if dict2.is_dict() && dict2.as_dict().has(resname) {
            return 1i32;
        }
    }
    0i32
}
/* ENABLE_HTML_SVG_OPACITY */
unsafe fn spc_html__img_empty(spe: *mut spc_env, attr: &pdf_obj) -> i32 {
    let mut ti = transform_info::new();
    let options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    let mut error: i32 = 0i32;
    let mut alpha: f64 = 1.0f64;
    /* ENABLE_HTML_SVG_OPACITY */
    let mut M: TMatrix = TMatrix::create_translation((*spe).x_user, (*spe).y_user);
    /* ENABLE_HTML_SVG_TRANSFORM */
    spc_warn!(
        spe,
        "html \"img\" tag found (not completed, plese don\'t use!)."
    );
    let src = attr.as_dict().get("src");
    if src.is_none() {
        spc_warn!(spe, "\"src\" attribute not found for \"img\" tag!");
        return -1i32;
    }
    let src = src.unwrap();
    transform_info_clear(&mut ti);
    if let Some(obj) = attr.as_dict().get("width") {
        ti.width = atopt(CStr::from_ptr(pdf_string_value(obj) as *const i8).to_bytes());
        ti.flags |= 1i32 << 1i32
    }
    if let Some(obj) = attr.as_dict().get("height") {
        ti.height = atopt(CStr::from_ptr(pdf_string_value(obj) as *const i8).to_bytes());
        ti.flags |= 1i32 << 2i32
    }
    if let Some(obj) = attr.as_dict().get("svg:opacity") {
        alpha = atof(pdf_string_value(obj) as *const i8);
        if alpha < 0.0f64 || alpha > 1.0f64 {
            spc_warn!(
                spe,
                "Invalid opacity value: {}",
                CStr::from_ptr(pdf_string_value(&*obj) as *mut i8).display(),
            );
            alpha = 1.0f64
        }
    }
    /* ENABLE_HTML_SVG_OPCAITY */
    if let Some(obj) = attr.as_dict().get("svg:transform") {
        let mut p: *const i8 = pdf_string_value(&*obj) as *const i8;
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        while *p as i32 != 0 && error == 0 {
            let mut N = TMatrix::identity();
            if let Ok(nextptr) = cvt_a_to_tmatrix(&mut N, CStr::from_ptr(p).to_bytes()) {
                p = nextptr.as_ptr() as *const i8;
            } else {
                error = -1;
            }
            if error == 0 {
                N.m32 = -N.m32;
                M = N.post_transform(&M);
                while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
                    p = p.offset(1)
                }
                if *p as i32 == ',' as i32 {
                    p = p.offset(1);
                    while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
                        p = p.offset(1)
                    }
                }
            }
        }
    }
    /* ENABLE_HTML_SVG_TRANSFORM */
    if error != 0 {
        spc_warn!(spe, "Error in html \"img\" tag attribute."); /* Not Tps prefix but... */
        return error;
    } /* op: */
    let id = pdf_ximage_findresource(pdf_string_value(src) as *const i8, options); /* op: */
    if id < 0i32 {
        spc_warn!(
            spe,
            "Could not find/load image: {}",
            CStr::from_ptr(pdf_string_value(src) as *const i8).display(),
        ); /* op: gs */
        error = -1i32
    } else {
        let mut r = Rect::zero();
        graphics_mode();
        pdf_dev_gsave();
        let a: i32 = (100.0f64 * alpha).round() as i32;
        if a != 0i32 {
            let res_name = format!("_Tps_a{:03}_", a);
            let res_name_c = CString::new(res_name.as_str()).unwrap();
            if check_resourcestatus("ExtGState", &res_name) == 0 {
                let dict = create_xgstate((0.01f64 * a as f64 / 0.01f64).round() * 0.01f64, 0i32);
                pdf_doc_add_page_resource("ExtGState", res_name_c.as_ptr(), pdf_ref_obj(dict));
                pdf_release_obj(dict);
            }
            pdf_doc_add_page_content(b" /");
            pdf_doc_add_page_content(res_name_c.to_bytes());
            pdf_doc_add_page_content(b" gs");
        }
        /* ENABLE_HTML_SVG_OPACITY */
        let M1 = pdf_ximage_scale_image(id, &mut r, &mut ti); /* op: */
        M = M1.post_transform(&M);
        pdf_dev_concat(&mut M);
        pdf_dev_rectclip(&r);
        let res_name = pdf_ximage_get_resname(id);
        pdf_doc_add_page_content(b" /");
        pdf_doc_add_page_content(CStr::from_ptr(res_name).to_bytes());
        pdf_doc_add_page_content(b" Do");
        pdf_dev_grestore();
        pdf_doc_add_page_resource("XObject", res_name, pdf_ximage_get_reference(id));
        /* ENABLE_HTML_SVG_XXX */
    }
    error
}
/* ENABLE_HTML_IMG_SUPPORT */
unsafe fn spc_handler_html_default(spe: *mut spc_env, mut ap: *mut spc_arg) -> i32 {
    let sd: *mut spc_html_ = &mut _HTML_STATE; /* treat "open" same as "empty" */
    /* treat "open" same as "empty" */
    let mut type_0: i32 = 1i32;
    if (*ap).cur.is_empty() {
        return 0i32;
    }
    let attr = pdf_new_dict();
    let name = read_html_tag(&mut *attr, &mut type_0, &mut (*ap).cur);
    if name.is_err() {
        pdf_release_obj(attr);
        return -1;
    }
    let error = match name.unwrap().as_slice() {
        b"a" => match type_0 {
            1 => spc_html__anchor_open(spe, &*attr, sd),
            2 => spc_html__anchor_close(spe, sd),
            _ => {
                spc_warn!(spe, "Empty html anchor tag???");
                -1
            }
        },
        b"base" => {
            if type_0 == 2 {
                spc_warn!(spe, "Close tag for \"base\"???");
                -1
            } else {
                spc_html__base_empty(spe, &*attr, sd)
            }
        }
        b"img" => {
            if type_0 == 2 {
                spc_warn!(spe, "Close tag for \"img\"???");
                -1
            } else {
                spc_html__img_empty(spe, &*attr)
            }
        }
        _ => 0,
    };
    pdf_release_obj(attr);
    while !(*ap).cur.is_empty() && libc::isspace((*ap).cur[0] as _) != 0 {
        (*ap).cur = &(*ap).cur[1..];
    }
    error
}
/* translate wsp* '(' wsp* number (comma-wsp number)? wsp* ')' */
unsafe fn cvt_a_to_tmatrix<'a>(M: &mut TMatrix, buf: &'a [u8]) -> Result<&'a [u8], ()> {
    let mut p = buf;
    let mut v: [f64; 6] = [0.; 6];
    while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    let q = p.parse_c_ident();
    if q.is_none() {
        return Err(());
    }
    /* parsed transformation key */
    /* handle args */
    while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    if p[0] != b'(' || p[1] == 0 {
        return Err(());
    }
    p = &p[1..];
    while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    let mut n = 0;
    while n < 6 && p[0] != 0 && p[0] != b')' {
        if let Some(q2) = p.parse_float_decimal() {
            v[n] = atof(q2.as_ptr());
            if p[0] == b',' {
                p = &p[1..];
            }
            while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
                p = &p[1..];
            }
            if p[0] == b',' {
                p = &p[1..];
                while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
                    p = &p[1..];
                }
            }
            n += 1;
        } else {
            break;
        }
    }
    if p[0] != b')' {
        return Err(());
    }
    p = &p[1..];
    match q.unwrap().to_bytes() {
        b"matrix" => {
            if n != 6 {
                return Err(());
            }
            *M = TMatrix::from_row_major_array(v);
        }
        b"translate" => {
            if n != 1 && n != 2 {
                return Err(());
            }
            *M = TMatrix::create_translation(v[0], if n == 2 { v[1] } else { 0. });
        }
        b"scale" => {
            if n != 1 && n != 2 {
                return Err(());
            }
            *M = TMatrix::create_scale(v[0], if n == 2 { v[1] } else { v[0] });
        }
        b"rotate" => {
            if n != 1 && n != 3 {
                return Err(());
            }
            let (s, c) = (v[0] * core::f64::consts::PI / 180.).sin_cos();
            M.m11 = c;
            M.m12 = -s;
            M.m21 = s;
            M.m22 = c;
            M.m31 = if n == 3 { v[1] } else { 0. };
            M.m32 = if n == 3 { v[2] } else { 0. };
        }
        b"skewX" => {
            if n != 1 {
                return Err(());
            }
            M.m11 = 1.;
            M.m12 = (v[0] * core::f64::consts::PI / 180.).tan();
            M.m21 = 0.;
            M.m22 = 1.;
        }
        b"skewY" => {
            if n != 1 {
                return Err(());
            }
            M.m11 = 1.;
            M.m12 = 0.;
            M.m21 = (v[0] * core::f64::consts::PI / 180.).tan();
            M.m22 = 1.;
        }
        _ => {}
    }
    Ok(p)
}
/* ENABLE_HTML_SVG_TRANSFORM */

pub unsafe fn spc_html_at_begin_document() -> i32 {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__init(sd as *mut libc::c_void)
}

pub unsafe fn spc_html_at_begin_page() -> i32 {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__bophook(ptr::null_mut(), sd as *mut libc::c_void)
}

pub unsafe fn spc_html_at_end_page() -> i32 {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__eophook(ptr::null_mut(), sd as *mut libc::c_void)
}

pub unsafe fn spc_html_at_end_document() -> i32 {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__clean(ptr::null_mut(), sd as *mut libc::c_void)
}

pub fn spc_html_check_special(buf: &[u8]) -> bool {
    let mut i = 0;
    for &p in buf {
        if unsafe { libc::isspace(p as _) == 0 } {
            break;
        }
        i += 1;
    }
    let buf = &buf[i..];
    buf.starts_with(b"html:")
}

pub unsafe fn spc_html_setup_handler(
    mut sph: *mut SpcHandler,
    spe: *mut spc_env,
    mut ap: *mut spc_arg,
) -> i32 {
    assert!(!sph.is_null() && !spe.is_null() && !ap.is_null());
    while !(*ap).cur.is_empty() && libc::isspace((*ap).cur[0] as _) != 0 {
        (*ap).cur = &(*ap).cur[1..];
    }
    if !(*ap).cur.starts_with(b"html:") {
        return -1i32;
    }
    (*ap).command = Some(b"");
    (*sph).key = b"html:";
    (*sph).exec = Some(spc_handler_html_default);
    (*ap).cur = &(*ap).cur[b"html:".len()..];
    while !(*ap).cur.is_empty() && libc::isspace((*ap).cur[0] as _) != 0 {
        (*ap).cur = &(*ap).cur[1..];
    }
    0i32
}
