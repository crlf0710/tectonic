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

use crate::bridge::DisplayExt;
use std::ffi::{CStr, CString};
use std::ptr;

use crate::dpx_pdfdoc::PdfPageBoundary;
use crate::dpx_pdfdraw::{pdf_dev_concat, pdf_dev_transform};
use crate::dpx_pdfximage::{
    pdf_ximage_findresource, pdf_ximage_get_reference, pdf_ximage_get_resname,
    pdf_ximage_scale_image,
};

use super::{spc_begin_annot, spc_end_annot};
use crate::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use crate::dpx_mem::new;
use crate::dpx_pdfdev::{graphics_mode, transform_info, transform_info_clear, TMatrix};
use crate::dpx_pdfdoc::pdf_doc_mut;
use crate::dpx_pdfdraw::{pdf_dev_grestore, pdf_dev_gsave, pdf_dev_rectclip};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_link_obj, pdf_obj, pdf_string, IntoObj, IntoRef, Object, PushObj,
};
use crate::spc_warn;
use libc::{atof, free, strcpy};

use super::{SpcArg, SpcEnv};

use super::SpcHandler;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct spc_html_ {
    pub(crate) opts: C2RustUnnamed_0,
    pub(crate) annotation_started: bool,
    pub(crate) baseurl: *mut i8,
    pub(crate) pending_type: i32,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) extensions: i32,
}

use crate::dpx_pdfximage::load_options;

use crate::dpx_pdfdev::Point;

static mut _HTML_STATE: spc_html_ = spc_html_ {
    opts: C2RustUnnamed_0 { extensions: 0 },
    annotation_started: false,
    baseurl: ptr::null_mut(),
    pending_type: -1,
};
/* ENABLE_HTML_SVG_TRANSFORM */
unsafe fn parse_key_val(pp: &mut &[u8]) -> Result<(CString, CString)> {
    let mut error: i32 = 0;
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
        return ERROR();
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
        ERROR()
    } else {
        Ok((k.unwrap(), v.unwrap()))
    }
}

unsafe fn read_html_tag(attr: &mut pdf_obj, type_0: &mut i32, pp: &mut &[u8]) -> Result<Vec<u8>> {
    let mut p = *pp;
    let mut error: i32 = 0;
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    if p.is_empty() || p[0] != b'<' {
        return ERROR();
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
        return ERROR();
    }
    while !p.is_empty() && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    while !p.is_empty() && error == 0 && p[0] != b'/' && p[0] != b'>' {
        if let Ok((kp, vp)) = parse_key_val(&mut p) {
            attr.as_dict_mut().set(
                kp.to_bytes().to_ascii_lowercase(),
                pdf_string::new_from_ptr(
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
        return ERROR();
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
        return ERROR();
    }
    p = &p[1..];
    name.make_ascii_lowercase();
    *pp = p;
    Ok(name)
}

unsafe fn spc_handler_html__init(dp: *mut libc::c_void) -> Result<()> {
    let mut sd: *mut spc_html_ = dp as *mut spc_html_;
    (*sd).annotation_started = false;
    (*sd).baseurl = ptr::null_mut();
    (*sd).pending_type = -1;
    Ok(())
}

unsafe fn spc_handler_html__clean(spe: *mut SpcEnv, dp: *mut libc::c_void) -> Result<()> {
    let mut sd: *mut spc_html_ = dp as *mut spc_html_;
    free((*sd).baseurl as *mut libc::c_void);
    if (*sd).pending_type >= 0 || (*sd).annotation_started {
        let spe = &*spe;
        spc_warn!(spe, "Unclosed html anchor found.");
    }
    (*sd).pending_type = -1;
    (*sd).baseurl = ptr::null_mut();
    (*sd).annotation_started = false;
    Ok(())
}

unsafe fn spc_handler_html__bophook(spe: *mut SpcEnv, dp: *mut libc::c_void) -> Result<()> {
    let sd: *mut spc_html_ = dp as *mut spc_html_;
    if (*sd).pending_type >= 0 {
        let spe = &*spe;
        spc_warn!(
            spe,
            "...html anchor continues from previous page processed..."
        );
    }
    Ok(())
}

unsafe fn spc_handler_html__eophook(spe: *mut SpcEnv, dp: *mut libc::c_void) -> Result<()> {
    let sd: *mut spc_html_ = dp as *mut spc_html_;
    if (*sd).pending_type >= 0 {
        let spe = &*spe;
        spc_warn!(spe, "Unclosed html anchor at end-of-page!");
    }
    Ok(())
}

unsafe fn fqurl(baseurl: &[u8], name: &[u8]) -> Vec<u8> {
    let mut q = Vec::with_capacity(if baseurl.is_empty() {
        name.len()
    } else {
        name.len() + baseurl.len() + 1
    });
    if !baseurl.is_empty() {
        let len = baseurl.len();
        q.extend(if baseurl[len - 1] == b'/' {
            &baseurl[..len - 1]
        } else {
            baseurl
        });
        if !name.is_empty() && name[0] != b'/' {
            q.push(b'/');
        }
    }
    q.extend(name);
    q
}

unsafe fn html_open_link(spe: &mut SpcEnv, name: &[u8], mut sd: *mut spc_html_) -> Result<()> {
    assert!(!name.is_empty());
    assert!(!(*sd).annotation_started);
    (*sd).annotation_started = true;
    let mut link_dict = pdf_dict::new();
    link_dict.set("Type", "Annot");
    link_dict.set("Subtype", "Link");
    let mut color = vec![];
    color.push_obj(0f64);
    color.push_obj(0f64);
    color.push_obj(1f64);
    link_dict.set("C", color);
    let url = fqurl(
        if (*sd).baseurl.is_null() {
            &[]
        } else {
            CStr::from_ptr((*sd).baseurl).to_bytes()
        },
        name,
    );
    if url[0] == b'#' {
        /* url++; causes memory leak in free(url) */
        link_dict.set("Dest", pdf_string::new(&url[1..])); /* Otherwise must be bug */
    } else {
        let mut action = pdf_dict::new();
        action.set("Type", "Action");
        action.set("S", "URI");
        action.set("URI", pdf_string::new(url));
        let action = action.into_obj();
        link_dict.set("A", pdf_link_obj(action));
        crate::release!(action);
    }
    spc_begin_annot(spe, link_dict).ok();
    (*sd).pending_type = 0;
    Ok(())
}

unsafe fn html_open_dest(spe: &mut SpcEnv, name: &[u8], mut sd: *mut spc_html_) -> Result<()> {
    let mut cp = Point::new(spe.x_user, spe.y_user);
    pdf_dev_transform(&mut cp, None);
    let p = pdf_doc_mut();
    let page_ref = p.get_reference("@THISPAGE");
    assert!(!page_ref.is_null());
    let mut array = vec![];
    array.push(page_ref);
    array.push_obj("XYZ");
    array.push_obj(Object::Null);
    array.push_obj(cp.y + 24.);
    array.push_obj(Object::Null);
    let error = p.add_names(b"Dests", name, &mut *array.into_obj());
    if error.is_err() {
        spc_warn!(spe, "Failed to add named destination: {}", name.display());
    }
    (*sd).pending_type = 1;
    error
}

unsafe fn spc_html__anchor_open(
    spe: &mut SpcEnv,
    attr: &pdf_obj,
    sd: *mut spc_html_,
) -> Result<()> {
    if (*sd).pending_type >= 0 || (*sd).annotation_started {
        spc_warn!(spe, "Nested html anchors found!");
        return ERR;
    }
    let href = attr.as_dict().get("href");
    let name = attr.as_dict().get("name");
    match (href, name) {
        (Some(_), Some(_)) => {
            spc_warn!(
                spe,
                "Sorry, you can\'t have both \"href\" and \"name\" in anchor tag..."
            );
            ERR
        }
        (Some(href), None) => html_open_link(spe, href.as_string().to_bytes_without_nul(), sd),
        (None, Some(name)) => {
            /* name */
            html_open_dest(spe, name.as_string().to_bytes_without_nul(), sd)
        }
        _ => {
            spc_warn!(spe, "You should have \"href\" or \"name\" in anchor tag!");
            ERR
        }
    }
}

unsafe fn spc_html__anchor_close(spe: &mut SpcEnv, mut sd: *mut spc_html_) -> Result<()> {
    let mut error = Ok(());
    match (*sd).pending_type {
        0 => {
            if (*sd).annotation_started {
                spc_end_annot(spe).ok();
                (*sd).annotation_started = false;
                (*sd).pending_type = -1
            } else {
                spc_warn!(spe, "Closing html anchor (link) without starting!");
                error = ERR;
            }
        }
        1 => (*sd).pending_type = -1,
        _ => {
            spc_warn!(spe, "No corresponding opening tag for html anchor.");
            error = ERR;
        }
    }
    error
}

unsafe fn spc_html__base_empty(
    spe: &mut SpcEnv,
    attr: &pdf_obj,
    mut sd: *mut spc_html_,
) -> Result<()> {
    let href = attr.as_dict().get("href");
    if href.is_none() {
        spc_warn!(spe, "\"href\" not found for \"base\" tag!");
        return ERR;
    }
    let href = href.unwrap();
    let vp = (*href).as_string().to_bytes();
    if !(*sd).baseurl.is_null() {
        spc_warn!(
            spe,
            "\"baseurl\" changed: \"{}\" --> \"{}\"",
            CStr::from_ptr((*sd).baseurl).display(),
            vp.display(),
        );
        free((*sd).baseurl as *mut libc::c_void);
    }
    (*sd).baseurl =
        new((vp.len().wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    let cstr = CString::new(vp).unwrap();
    strcpy((*sd).baseurl, cstr.as_ptr());
    Ok(())
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
        match q.as_ref() {
            "pt" => u *= 72.0f64 / 72.27f64,
            "in" => u *= 72.0f64,
            "cm" => u *= 72.0f64 / 2.54f64,
            "mm" => u *= 72.0f64 / 25.4f64,
            "bp" => u *= 1.0f64,
            "pc" => u *= 12.0f64 * 72.0f64 / 72.27f64,
            "dd" => u *= 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
            "cc" => u *= 12.0f64 * 1238.0f64 / 1157.0f64 * 72.0f64 / 72.27f64,
            "sp" => u *= 72.0f64 / (72.27f64 * 65536 as f64),
            "px" => u *= 1.0f64,
            _ => {
                warn!("Unknown unit of measure: {}", q);
            }
        }
    }
    v * u
}
/* Replicated from spc_tpic */
unsafe fn create_xgstate(a: f64, f_ais: i32) -> pdf_dict
/* alpha is shape */ {
    let mut dict = pdf_dict::new();
    dict.set("Type", "ExtGState");
    if f_ais != 0 {
        dict.set("AIS", true);
    }
    dict.set("ca", a);
    dict
}
unsafe fn check_resourcestatus(category: &str, resname: &str) -> i32 {
    let dict1 = pdf_doc_mut().current_page_resources();
    if dict1.is_null() {
        return 0;
    }
    if let Some(dict2) = (*dict1).as_dict().get(category) {
        match &dict2.data {
            Object::Dict(d2) if d2.has(resname) => {
                return 1;
            }
            _ => {}
        }
    }
    0
}
/* ENABLE_HTML_SVG_OPACITY */
unsafe fn spc_html__img_empty(spe: &mut SpcEnv, attr: &pdf_obj) -> Result<()> {
    let mut ti = transform_info::new();
    let options: load_options = load_options {
        page_no: 1,
        bbox_type: PdfPageBoundary::Auto,
        dict: ptr::null_mut(),
    };
    let mut alpha: f64 = 1.0f64;
    /* ENABLE_HTML_SVG_OPACITY */
    let mut M: TMatrix = TMatrix::create_translation(spe.x_user, spe.y_user);
    /* ENABLE_HTML_SVG_TRANSFORM */
    spc_warn!(
        spe,
        "html \"img\" tag found (not completed, plese don\'t use!)."
    );
    let src = attr.as_dict().get("src");
    if src.is_none() {
        spc_warn!(spe, "\"src\" attribute not found for \"img\" tag!");
        return ERR;
    }
    let src = src.unwrap();
    transform_info_clear(&mut ti);
    if let Some(obj) = attr.as_dict().get("width") {
        ti.width = atopt(obj.as_string().to_bytes_without_nul());
        ti.flags |= 1 << 1
    }
    if let Some(obj) = attr.as_dict().get("height") {
        ti.height = atopt(obj.as_string().to_bytes_without_nul());
        ti.flags |= 1 << 2;
    }
    if let Some(obj) = attr.as_dict().get("svg:opacity") {
        let cstr = CString::new(obj.as_string().to_bytes()).unwrap();
        alpha = atof(cstr.as_ptr());
        if alpha < 0. || alpha > 1. {
            spc_warn!(
                spe,
                "Invalid opacity value: {}",
                (*obj).as_string().to_bytes_without_nul().display(),
            );
            alpha = 1.;
        }
    }
    /* ENABLE_HTML_SVG_OPCAITY */
    if let Some(obj) = attr.as_dict().get("svg:transform") {
        let mut p = (&*obj).as_string().to_bytes();
        while !p.is_empty() && p[0].is_ascii_whitespace() {
            p = &p[1..];
        }
        while !p.is_empty() {
            let mut N = TMatrix::identity();
            p = cvt_a_to_tmatrix(&mut N, p).map_err(|e| {
                spc_warn!(spe, "Error in html \"img\" tag attribute."); /* Not Tps prefix but... */
                e
            })?;
            N.m32 = -N.m32;
            M = N.post_transform(&M);
            while !p.is_empty() && p[0].is_ascii_whitespace() {
                p = &p[1..];
            }
            if p[0] == b',' {
                p = &p[1..];
                while !p.is_empty() && p[0].is_ascii_whitespace() {
                    p = &p[1..];
                }
            }
        }
    }
    /* ENABLE_HTML_SVG_TRANSFORM */
    let id = pdf_ximage_findresource(
        &std::str::from_utf8(src.as_string().to_bytes()).unwrap(),
        options,
    ); /* op: */
    if id < 0 {
        spc_warn!(
            spe,
            "Could not find/load image: {}",
            src.as_string().to_bytes_without_nul().display(),
        ); /* op: gs */
        ERR
    } else {
        graphics_mode();
        pdf_dev_gsave();
        let p = pdf_doc_mut();
        let a: i32 = (100.0f64 * alpha).round() as i32;
        if a != 0 {
            let res_name = format!("_Tps_a{:03}_", a);
            if check_resourcestatus("ExtGState", &res_name) == 0 {
                let dict = create_xgstate((0.01f64 * a as f64 / 0.01f64).round() * 0.01f64, 0);
                p.add_page_resource("ExtGState", res_name.as_bytes(), dict.into_ref().into_obj());
            }
            p.add_page_content(b" /");
            p.add_page_content(res_name.as_bytes());
            p.add_page_content(b" gs");
        }
        /* ENABLE_HTML_SVG_OPACITY */
        let (r, M1) = pdf_ximage_scale_image(id, &mut ti); /* op: */
        M = M1.post_transform(&M);
        pdf_dev_concat(&mut M);
        pdf_dev_rectclip(&r);
        let res_name = CStr::from_ptr(pdf_ximage_get_resname(id));
        p.add_page_content(b" /");
        p.add_page_content(res_name.to_bytes());
        p.add_page_content(b" Do");
        pdf_dev_grestore();
        p.add_page_resource("XObject", res_name.to_bytes(), pdf_ximage_get_reference(id));
        /* ENABLE_HTML_SVG_XXX */
        Ok(())
    }
}
/* ENABLE_HTML_IMG_SUPPORT */
unsafe fn spc_handler_html_default(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
    let sd: *mut spc_html_ = &mut _HTML_STATE; /* treat "open" same as "empty" */
    /* treat "open" same as "empty" */
    let mut type_0: i32 = 1;
    if ap.cur.is_empty() {
        return Ok(());
    }
    let attr = pdf_dict::new().into_obj();
    let name = read_html_tag(&mut *attr, &mut type_0, &mut ap.cur);
    if name.is_err() {
        crate::release!(attr);
        return ERR;
    }
    let error = match name.unwrap().as_slice() {
        b"a" => match type_0 {
            1 => spc_html__anchor_open(spe, &*attr, sd),
            2 => spc_html__anchor_close(spe, sd),
            _ => {
                spc_warn!(spe, "Empty html anchor tag???");
                ERR
            }
        },
        b"base" => {
            if type_0 == 2 {
                spc_warn!(spe, "Close tag for \"base\"???");
                ERR
            } else {
                spc_html__base_empty(spe, &*attr, sd)
            }
        }
        b"img" => {
            if type_0 == 2 {
                spc_warn!(spe, "Close tag for \"img\"???");
                ERR
            } else {
                spc_html__img_empty(spe, &*attr)
            }
        }
        _ => Ok(()),
    };
    crate::release!(attr);
    while !ap.cur.is_empty() && libc::isspace(ap.cur[0] as _) != 0 {
        ap.cur = &ap.cur[1..];
    }
    error
}
/* translate wsp* '(' wsp* number (comma-wsp number)? wsp* ')' */
unsafe fn cvt_a_to_tmatrix<'a>(M: &mut TMatrix, buf: &'a [u8]) -> Result<&'a [u8]> {
    let mut p = buf;
    let mut v: [f64; 6] = [0.; 6];
    while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    let q = p.parse_c_ident();
    if q.is_none() {
        return ERROR();
    }
    /* parsed transformation key */
    /* handle args */
    while p[0] != 0 && libc::isspace(p[0] as _) != 0 {
        p = &p[1..];
    }
    if p[0] != b'(' || p[1] == 0 {
        return ERROR();
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
        return ERROR();
    }
    p = &p[1..];
    match q.unwrap().as_ref() {
        "matrix" => {
            if n != 6 {
                return ERROR();
            }
            *M = TMatrix::from_row_major_array(v);
        }
        "translate" => {
            if n != 1 && n != 2 {
                return ERROR();
            }
            *M = TMatrix::create_translation(v[0], if n == 2 { v[1] } else { 0. });
        }
        "scale" => {
            if n != 1 && n != 2 {
                return ERROR();
            }
            *M = TMatrix::create_scale(v[0], if n == 2 { v[1] } else { v[0] });
        }
        "rotate" => {
            if n != 1 && n != 3 {
                return ERROR();
            }
            let (s, c) = (v[0] * core::f64::consts::PI / 180.).sin_cos();
            M.m11 = c;
            M.m12 = -s;
            M.m21 = s;
            M.m22 = c;
            M.m31 = if n == 3 { v[1] } else { 0. };
            M.m32 = if n == 3 { v[2] } else { 0. };
        }
        "skewX" => {
            if n != 1 {
                return ERROR();
            }
            M.m11 = 1.;
            M.m12 = (v[0] * core::f64::consts::PI / 180.).tan();
            M.m21 = 0.;
            M.m22 = 1.;
        }
        "skewY" => {
            if n != 1 {
                return ERROR();
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

pub(crate) unsafe fn spc_html_at_begin_document() -> Result<()> {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__init(sd as *mut libc::c_void)
}

pub(crate) unsafe fn spc_html_at_begin_page() -> Result<()> {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__bophook(ptr::null_mut(), sd as *mut libc::c_void)
}

pub(crate) unsafe fn spc_html_at_end_page() -> Result<()> {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__eophook(ptr::null_mut(), sd as *mut libc::c_void)
}

pub(crate) unsafe fn spc_html_at_end_document() -> Result<()> {
    let sd: *mut spc_html_ = &mut _HTML_STATE;
    spc_handler_html__clean(ptr::null_mut(), sd as *mut libc::c_void)
}

pub(crate) fn spc_html_check_special(buf: &[u8]) -> bool {
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

pub(crate) unsafe fn spc_html_setup_handler(
    sph: &mut SpcHandler,
    _spe: &mut SpcEnv,
    ap: &mut SpcArg,
) -> Result<()> {
    while !ap.cur.is_empty() && libc::isspace(ap.cur[0] as _) != 0 {
        ap.cur = &ap.cur[1..];
    }
    if !ap.cur.starts_with(b"html:") {
        return ERR;
    }
    ap.command = Some("");
    *sph = SpcHandler {
        key: "html:",
        exec: Some(spc_handler_html_default),
    };
    ap.cur = &ap.cur[b"html:".len()..];
    while !ap.cur.is_empty() && libc::isspace(ap.cur[0] as _) != 0 {
        ap.cur = &ap.cur[1..];
    }
    Ok(())
}
