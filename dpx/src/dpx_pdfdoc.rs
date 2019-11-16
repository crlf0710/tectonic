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
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_mut
)]

use crate::mfree;
use crate::streq_ptr;
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::{CStr, CString};
use std::ptr;

use super::dpx_dpxutil::{
    ht_append_table, ht_clear_iter, ht_clear_table, ht_init_table, ht_iter_getkey, ht_iter_next,
    ht_lookup_table, ht_set_iter, ht_table_size,
};
use super::dpx_dvipdfmx::is_xdv;
use super::dpx_jpegimage::check_for_jpeg;
use super::dpx_mem::{new, renew};
use super::dpx_pdfcolor::{pdf_close_colors, pdf_color_set_verbose, pdf_init_colors, WHITE};
use super::dpx_pdfdev::{
    Coord, pdf_dev_bop, pdf_dev_eop, pdf_dev_get_coord, pdf_dev_get_param, pdf_dev_reset_color,
    pdf_dev_reset_fonts,
};
use super::dpx_pdfdraw::{
    pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to, pdf_dev_gsave,
    pdf_dev_pop_gstate, pdf_dev_push_gstate, pdf_dev_set_color,
};
use super::dpx_pdfencrypt::{pdf_enc_id_array, pdf_encrypt_obj};
use super::dpx_pdffont::{
    get_unique_time_if_given, pdf_close_fonts, pdf_font_set_verbose, pdf_init_fonts,
};
use super::dpx_pdfnames::{
    pdf_delete_name_tree, pdf_names_add_object, pdf_names_create_tree, pdf_new_name_tree,
};
use super::dpx_pdfresource::{pdf_close_resources, pdf_init_resources};
use super::dpx_pdfximage::{
    pdf_close_images, pdf_init_images, pdf_ximage_defineresource, pdf_ximage_findresource,
    pdf_ximage_get_reference, pdf_ximage_init_form_info, pdf_ximage_set_verbose, XInfo,
};
use super::dpx_pngimage::check_for_png;
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_array_length, pdf_compare_reference,
    pdf_deref_obj, pdf_file, pdf_file_get_catalog, pdf_link_obj,
    pdf_merge_dict, pdf_name_value, pdf_new_array, pdf_new_dict, pdf_new_name, pdf_new_number,
    pdf_new_stream, pdf_new_string, pdf_number_value, pdf_obj, pdf_obj_typeof, pdf_out_flush,
    pdf_out_init, pdf_ref_obj, pdf_release_obj, pdf_remove_dict, pdf_set_encrypt, pdf_set_id,
    pdf_set_info, pdf_set_root, pdf_stream_length, pdf_string_length,
    pdf_string_value, PdfObjType, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{free, memcpy, strcmp, strcpy, strlen, strncmp, strncpy};

pub type size_t = u64;

use crate::TTInputFormat;

pub use super::dpx_pdfcolor::PdfColor;

use super::dpx_pdfdev::{Rect, TMatrix};
#[derive(Copy, Clone)]
#[repr(C)]
pub struct form_list_node {
    pub q_depth: i32,
    pub form: pdf_form,
    pub prev: *mut form_list_node,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_form {
    pub ident: *mut i8,
    pub matrix: TMatrix,
    pub cropbox: Rect,
    pub resources: *mut pdf_obj,
    pub contents: *mut pdf_obj,
}

use super::dpx_dpxutil::ht_entry;
use super::dpx_dpxutil::ht_table;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_article {
    pub id: *mut i8,
    pub info: *mut pdf_obj,
    pub num_beads: u32,
    pub max_beads: u32,
    pub beads: *mut pdf_bead,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_bead {
    pub id: *mut i8,
    pub page_no: i32,
    pub rect: Rect,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_olitem {
    pub dict: *mut pdf_obj,
    pub is_open: i32,
    pub first: *mut pdf_olitem,
    pub parent: *mut pdf_olitem,
    pub next: *mut pdf_olitem,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_page {
    pub page_obj: *mut pdf_obj,
    pub page_ref: *mut pdf_obj,
    pub flags: i32,
    pub ref_x: f64,
    pub ref_y: f64,
    pub cropbox: Rect,
    pub resources: *mut pdf_obj,
    pub background: *mut pdf_obj,
    pub contents: *mut pdf_obj,
    pub content_refs: [*mut pdf_obj; 4],
    pub annots: *mut pdf_obj,
    pub beads: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_doc {
    pub root: C2RustUnnamed_3,
    pub info: *mut pdf_obj,
    pub pages: C2RustUnnamed_2,
    pub outlines: C2RustUnnamed_1,
    pub articles: C2RustUnnamed_0,
    pub names: *mut name_dict,
    pub check_gotos: i32,
    pub gotos: ht_table,
    pub opt: C2RustUnnamed,
    pub pending_forms: *mut form_list_node,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub outline_open_depth: i32,
    pub annot_grow: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct name_dict {
    pub category: *const i8,
    pub data: *mut ht_table,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub num_entries: u32,
    pub max_entries: u32,
    pub entries: *mut pdf_article,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_1 {
    pub first: *mut pdf_olitem,
    pub current: *mut pdf_olitem,
    pub current_depth: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_2 {
    pub mediabox: Rect,
    pub bop: *mut pdf_obj,
    pub eop: *mut pdf_obj,
    pub num_entries: u32,
    pub max_entries: u32,
    pub entries: *mut pdf_page,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_3 {
    pub dict: *mut pdf_obj,
    pub viewerpref: *mut pdf_obj,
    pub pagelabels: *mut pdf_obj,
    pub pages: *mut pdf_obj,
    pub names: *mut pdf_obj,
    pub threads: *mut pdf_obj,
}
use super::dpx_dpxutil::ht_iter;

use crate::dpx_pdfximage::{load_options, xform_info};
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_4 {
    pub dirty: i32,
    pub broken: i32,
    pub annot_dict: *mut pdf_obj,
    pub rect: Rect,
}
/* quasi-hack to get the primary input */
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0i32;
static mut manual_thumb_enabled: i8 = 0_i8;
static mut thumb_basename: *mut i8 = ptr::null_mut();
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_enable_manual_thumbnails() {
    manual_thumb_enabled = 1_i8;
    // without HAVE_LIBPNG:
    // warn!("Manual thumbnail is not supported without the libpng library.");
}
unsafe fn read_thumbnail(mut thumb_filename: *const i8) -> *mut pdf_obj {
    let mut options: load_options = load_options {
            page_no: 1i32,
            bbox_type: 0i32,
            dict: ptr::null_mut(),
        };
    let handle =
        ttstub_input_open(thumb_filename, TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        warn!(
            "Could not open thumbnail file \"{}\"",
            CStr::from_ptr(thumb_filename).display()
        );
        return ptr::null_mut();
    }
    let mut handle = handle.unwrap();
    if check_for_png(&mut handle) == 0
        && check_for_jpeg(&mut handle) == 0
    {
        warn!(
            "Thumbnail \"{}\" not a png/jpeg file!",
            CStr::from_ptr(thumb_filename).display()
        );
        ttstub_input_close(handle);
        return ptr::null_mut();
    }
    ttstub_input_close(handle);
    let xobj_id = pdf_ximage_findresource(thumb_filename, options);
    if xobj_id < 0i32 {
        warn!(
            "Could not read thumbnail file \"{}\".",
            CStr::from_ptr(thumb_filename).display()
        );
        ptr::null_mut()
    } else {
        pdf_ximage_get_reference(xobj_id)
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_verbose(mut level: i32) {
    verbose = level;
    pdf_font_set_verbose(level);
    pdf_color_set_verbose(level);
    pdf_ximage_set_verbose(level);
}
static mut pdoc: pdf_doc = pdf_doc {
    root: C2RustUnnamed_3 {
        dict: ptr::null_mut(),
        viewerpref: ptr::null_mut(),
        pagelabels: ptr::null_mut(),
        pages: ptr::null_mut(),
        names: ptr::null_mut(),
        threads: ptr::null_mut(),
    },
    info: ptr::null_mut(),
    pages: C2RustUnnamed_2 {
        mediabox: Rect::zero(),
        bop: ptr::null_mut(),
        eop: ptr::null_mut(),
        num_entries: 0,
        max_entries: 0,
        entries: ptr::null_mut(),
    },
    outlines: C2RustUnnamed_1 {
        first: ptr::null_mut(),
        current: ptr::null_mut(),
        current_depth: 0,
    },
    articles: C2RustUnnamed_0 {
        num_entries: 0,
        max_entries: 0,
        entries: ptr::null_mut(),
    },
    names: ptr::null_mut(),
    check_gotos: 0,
    gotos: ht_table {
        count: 0,
        hval_free_fn: None,
        table: [ptr::null_mut(); 503],
    },
    opt: C2RustUnnamed {
        outline_open_depth: 0,
        annot_grow: 0.,
    },
    pending_forms: ptr::null_mut(),
};
unsafe fn pdf_doc_init_catalog(mut p: *mut pdf_doc) {
    (*p).root.viewerpref = ptr::null_mut();
    (*p).root.pagelabels = ptr::null_mut();
    (*p).root.pages = ptr::null_mut();
    (*p).root.names = ptr::null_mut();
    (*p).root.threads = ptr::null_mut();
    (*p).root.dict = pdf_new_dict();
    pdf_set_root((*p).root.dict);
}
unsafe fn pdf_doc_close_catalog(mut p: *mut pdf_doc) {
    if !(*p).root.viewerpref.is_null() {
        let tmp = (*(*p).root.dict).as_dict().get("ViewerPreferences");
        if tmp.is_none() {
            pdf_add_dict(
                &mut *(*p).root.dict,
                "ViewerPreferences",
                pdf_ref_obj((*p).root.viewerpref),
            );
        } else if let Some(tmp) = tmp.filter(|&tmp| (*tmp).is_dict()) {
            pdf_merge_dict(&mut *(*p).root.viewerpref, &*tmp);
            pdf_add_dict(
                &mut *(*p).root.dict,
                "ViewerPreferences",
                pdf_ref_obj((*p).root.viewerpref),
            );
        } else {
            /* What should I do? */
            warn!("Could not modify ViewerPreferences.");
            /* Maybe reference */
        }
        pdf_release_obj((*p).root.viewerpref);
        (*p).root.viewerpref = ptr::null_mut()
    }
    if !(*p).root.pagelabels.is_null() {
        let tmp = (*(*p).root.dict).as_dict_mut().get_mut("PageLabels");
        if tmp.is_none() {
            let tmp = pdf_new_dict();
            pdf_add_dict(&mut *tmp, "Nums", pdf_link_obj((*p).root.pagelabels));
            pdf_add_dict(&mut *(*p).root.dict, "PageLabels", pdf_ref_obj(tmp));
            pdf_release_obj(tmp);
        } else {
            /* What should I do? */
            warn!("Could not modify PageLabels.");
        }
        pdf_release_obj((*p).root.pagelabels);
        (*p).root.pagelabels = ptr::null_mut()
    }
    pdf_add_dict(&mut *(*p).root.dict, "Type", pdf_new_name("Catalog"));
    pdf_release_obj((*p).root.dict);
    (*p).root.dict = ptr::null_mut();
}
/*
 * Pages are starting at 1.
 * The page count does not increase until the page is finished.
 */
unsafe fn doc_resize_page_entries(mut p: *mut pdf_doc, mut size: u32) {
    if size > (*p).pages.max_entries {
        /* global bop */
        (*p).pages.entries = renew(
            (*p).pages.entries as *mut libc::c_void,
            (size as u64).wrapping_mul(::std::mem::size_of::<pdf_page>() as u64) as u32,
        ) as *mut pdf_page; /* background */
        /* page body  */
        for i in (*p).pages.max_entries..size {
            let ref mut fresh0 = (*(*p).pages.entries.offset(i as isize)).page_obj; /* global eop */
            *fresh0 = ptr::null_mut();
            let ref mut fresh1 = (*(*p).pages.entries.offset(i as isize)).page_ref;
            *fresh1 = ptr::null_mut();
            (*(*p).pages.entries.offset(i as isize)).flags = 0i32;
            let ref mut fresh2 = (*(*p).pages.entries.offset(i as isize)).resources;
            *fresh2 = ptr::null_mut();
            let ref mut fresh3 = (*(*p).pages.entries.offset(i as isize)).background;
            *fresh3 = ptr::null_mut();
            let ref mut fresh4 = (*(*p).pages.entries.offset(i as isize)).contents;
            *fresh4 = ptr::null_mut();
            let ref mut fresh5 = (*(*p).pages.entries.offset(i as isize)).content_refs[0];
            *fresh5 = ptr::null_mut();
            let ref mut fresh6 = (*(*p).pages.entries.offset(i as isize)).content_refs[1];
            *fresh6 = ptr::null_mut();
            let ref mut fresh7 = (*(*p).pages.entries.offset(i as isize)).content_refs[2];
            *fresh7 = ptr::null_mut();
            let ref mut fresh8 = (*(*p).pages.entries.offset(i as isize)).content_refs[3];
            *fresh8 = ptr::null_mut();
            let ref mut fresh9 = (*(*p).pages.entries.offset(i as isize)).annots;
            *fresh9 = ptr::null_mut();
            let ref mut fresh10 = (*(*p).pages.entries.offset(i as isize)).beads;
            *fresh10 = ptr::null_mut();
        }
        (*p).pages.max_entries = size
    };
}
unsafe fn doc_get_page_entry(mut p: *mut pdf_doc, mut page_no: u32) -> *mut pdf_page {
    if page_no as u64 > 65535 {
        panic!("Page number {} too large!", page_no,);
    } else {
        if page_no == 0_u32 {
            panic!("Invalid Page number {}.", page_no,);
        }
    }
    if page_no > (*p).pages.max_entries {
        doc_resize_page_entries(p, page_no.wrapping_add(128u32));
    }
    &mut *(*p)
        .pages
        .entries
        .offset(page_no.wrapping_sub(1_u32) as isize) as *mut pdf_page
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_bop_content(mut content: *const i8, mut length: u32) {
    let mut p: *mut pdf_doc = &mut pdoc;
    assert!(!p.is_null());
    if !(*p).pages.bop.is_null() {
        pdf_release_obj((*p).pages.bop);
        (*p).pages.bop = ptr::null_mut()
    }
    if length > 0_u32 {
        (*p).pages.bop = pdf_new_stream(STREAM_COMPRESS);
        pdf_add_stream(
            &mut *(*p).pages.bop,
            content as *const libc::c_void,
            length as i32,
        );
    } else {
        (*p).pages.bop = ptr::null_mut()
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_eop_content(mut content: *const i8, mut length: u32) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if !(*p).pages.eop.is_null() {
        pdf_release_obj((*p).pages.eop);
        (*p).pages.eop = ptr::null_mut()
    }
    if length > 0_u32 {
        (*p).pages.eop = pdf_new_stream(STREAM_COMPRESS);
        pdf_add_stream(
            &mut *(*p).pages.eop,
            content as *const libc::c_void,
            length as i32,
        );
    } else {
        (*p).pages.eop = ptr::null_mut()
    };
}

fn asn_date() -> String {
    use chrono::prelude::*;

    let timeformat = "D:%Y%m%d%H%M%S";
    match get_unique_time_if_given() {
        Some(x) => {
            let x = DateTime::<Utc>::from(x);
            format!("{}-00'00'", x.format(timeformat))
        }
        None => {
            let x = Local::now();
            let tz = format!("{}", x.format("%z"));
            format!("{}{}'{}'", x.format(timeformat), &tz[0..3], &tz[3..])
        }
    }
}

unsafe fn pdf_doc_init_docinfo(mut p: *mut pdf_doc) {
    (*p).info = pdf_new_dict();
    pdf_set_info((*p).info);
}
unsafe fn pdf_doc_close_docinfo(mut p: *mut pdf_doc) {
    let mut docinfo: *mut pdf_obj = (*p).info;
    /*
     * Excerpt from PDF Reference 4th ed., sec. 10.2.1.
     *
     * Any entry whose value is not known should be omitted from the dictionary,
     * rather than included with an empty string as its value.
     *
     * ....
     *
     * Note: Although viewer applications can store custom metadata in the document
     * information dictionary, it is inappropriate to store private content or
     * structural information there; such information should be stored in the
     * document catalog instead (see Section 3.6.1,  Document Catalog ).
     */
    const KEYS: [&str; 8] = [
        "Title",
        "Author",
        "Subject",
        "Keywords",
        "Creator",
        "Producer",
        "CreationDate",
        "ModDate",
    ];
    for key in KEYS.iter() {
        if let Some(value) = (*docinfo).as_dict().get(*key) {
            if !(*value).is_string() {
                warn!("\"{}\" in DocInfo dictionary not string type.", key,);
                pdf_remove_dict(&mut *docinfo, key);
                warn!("\"{}\" removed from DocInfo.", key,);
            } else if pdf_string_length(&*value) == 0_u32 {
                /* The hyperref package often uses emtpy strings. */
                pdf_remove_dict(&mut *docinfo, key);
            }
        }
    }
    if !(*docinfo).as_dict().has("Producer") {
        let mut banner: [i8; 16] =
            *::std::mem::transmute::<&[u8; 16], &mut [i8; 16]>(b"xdvipdfmx (0.1)\x00");
        pdf_add_dict(
            &mut *docinfo,
            "Producer",
            pdf_new_string(
                banner.as_mut_ptr() as *const libc::c_void,
                strlen(banner.as_mut_ptr()) as _,
            ),
        );
    }
    if !(*docinfo).as_dict().has("CreationDate") {
        let now = asn_date();
        let l = now.len();

        pdf_add_dict(
            &mut *docinfo,
            "CreationDate",
            pdf_new_string(now.as_ptr() as *const libc::c_void, l as _),
        );
    }
    pdf_release_obj(docinfo);
    (*p).info = ptr::null_mut();
}
unsafe fn pdf_doc_get_page_resources(mut p: *mut pdf_doc, category: &str) -> *mut pdf_obj {
    if p.is_null() {
        return ptr::null_mut();
    }
    let res_dict = if !(*p).pending_forms.is_null() {
        if !(*(*p).pending_forms).form.resources.is_null() {
            (*(*p).pending_forms).form.resources
        } else {
            (*(*p).pending_forms).form.resources = pdf_new_dict();
            (*(*p).pending_forms).form.resources
        }
    } else {
        let currentpage =
            &mut *(*p).pages.entries.offset((*p).pages.num_entries as isize) as *mut pdf_page;
        if !(*currentpage).resources.is_null() {
            (*currentpage).resources
        } else {
            (*currentpage).resources = pdf_new_dict();
            (*currentpage).resources
        }
    };
    (*res_dict).as_dict_mut().get_mut(category).unwrap_or_else(|| {
        let mut resources = pdf_new_dict();
        pdf_add_dict(&mut *res_dict, category, resources);
        &mut *resources
    })
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_add_page_resource(
    category: &str,
    mut resource_name: *const i8,
    mut resource_ref: *mut pdf_obj,
) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if !(!resource_ref.is_null() && (*resource_ref).is_indirect()) {
        warn!("Passed non indirect reference...");
        resource_ref = pdf_ref_obj(resource_ref)
        /* leak */
    }
    let resource_name = CStr::from_ptr(resource_name);
    let resources = pdf_doc_get_page_resources(p, category);
    if let Some(duplicate) = (*resources).as_dict_mut().get_mut(resource_name.to_bytes()) {
        if pdf_compare_reference(duplicate, resource_ref) != 0 {
            warn!(
                "Conflicting page resource found (page: {}, category: {}, name: {}).",
                pdf_doc_current_page_number(),
                category,
                resource_name.display(),
            );
            warn!("Ignoring...");
            pdf_release_obj(resource_ref);
        } else {
            pdf_add_dict(&mut *resources, resource_name.to_bytes(), resource_ref);
        }
    } else {
        pdf_add_dict(&mut *resources, resource_name.to_bytes(), resource_ref);
    }
}
unsafe fn doc_flush_page(
    mut p: *mut pdf_doc,
    mut page: *mut pdf_page,
    mut parent_ref: *mut pdf_obj,
) {
    pdf_add_dict(&mut *(*page).page_obj, "Type", pdf_new_name("Page"));
    pdf_add_dict(&mut *(*page).page_obj, "Parent", parent_ref);
    /*
     * Clipping area specified by CropBox is affected by MediaBox which
     * might be inherit from parent node. If MediaBox of the root node
     * does not have enough size to cover all page's imaging area, using
     * CropBox here gives incorrect result.
     */
    if (*page).flags & 1i32 << 0i32 != 0 {
        let mediabox = pdf_new_array();
        pdf_add_array(
            &mut *mediabox,
            pdf_new_number(((*page).cropbox.ll.x / 0.01 + 0.5).floor() * 0.01),
        );
        pdf_add_array(
            &mut *mediabox,
            pdf_new_number(((*page).cropbox.ll.y / 0.01 + 0.5).floor() * 0.01),
        );
        pdf_add_array(
            &mut *mediabox,
            pdf_new_number(((*page).cropbox.ur.x / 0.01 + 0.5).floor() * 0.01),
        );
        pdf_add_array(
            &mut *mediabox,
            pdf_new_number(((*page).cropbox.ur.y / 0.01 + 0.5).floor() * 0.01),
        );
        pdf_add_dict(&mut *(*page).page_obj, "MediaBox", mediabox);
    }
    let mut count = 0_u32;
    let contents_array = pdf_new_array();
    if !(*page).content_refs[0].is_null() {
        /* global bop */
        pdf_add_array(&mut *contents_array, (*page).content_refs[0]);
        count = count.wrapping_add(1)
    } else if !(*p).pages.bop.is_null() && pdf_stream_length(&*(*p).pages.bop) > 0i32 {
        pdf_add_array(&mut *contents_array, pdf_ref_obj((*p).pages.bop));
        count = count.wrapping_add(1)
    }
    if !(*page).content_refs[1].is_null() {
        /* background */
        pdf_add_array(&mut *contents_array, (*page).content_refs[1]);
        count = count.wrapping_add(1)
    }
    if !(*page).content_refs[2].is_null() {
        /* page body */
        pdf_add_array(&mut *contents_array, (*page).content_refs[2]);
        count = count.wrapping_add(1)
    }
    if !(*page).content_refs[3].is_null() {
        /* global eop */
        pdf_add_array(&mut *contents_array, (*page).content_refs[3]);
        count = count.wrapping_add(1)
    } else if !(*p).pages.eop.is_null() && pdf_stream_length(&*(*p).pages.eop) > 0i32 {
        pdf_add_array(&mut *contents_array, pdf_ref_obj((*p).pages.eop));
        count = count.wrapping_add(1)
    }
    if count == 0_u32 {
        warn!("Page with empty content found!!!");
    }
    (*page).content_refs[0] = ptr::null_mut();
    (*page).content_refs[1] = ptr::null_mut();
    (*page).content_refs[2] = ptr::null_mut();
    (*page).content_refs[3] = ptr::null_mut();
    pdf_add_dict(&mut *(*page).page_obj, "Contents", contents_array);
    if !(*page).annots.is_null() {
        pdf_add_dict(&mut *(*page).page_obj, "Annots", pdf_ref_obj((*page).annots));
        pdf_release_obj((*page).annots);
    }
    if !(*page).beads.is_null() {
        pdf_add_dict(&mut *(*page).page_obj, "B", pdf_ref_obj((*page).beads));
        pdf_release_obj((*page).beads);
    }
    pdf_release_obj((*page).page_obj);
    pdf_release_obj((*page).page_ref);
    (*page).page_obj = ptr::null_mut();
    (*page).page_ref = ptr::null_mut();
    (*page).annots = ptr::null_mut();
    (*page).beads = ptr::null_mut();
}
unsafe fn build_page_tree(
    mut p: *mut pdf_doc,
    mut firstpage: *mut pdf_page,
    mut num_pages: i32,
    mut parent_ref: *mut pdf_obj,
) -> *mut pdf_obj {
    let self_0 = pdf_new_dict();
    /*
     * This is a slight kludge which allow the subtree dictionary
     * generated by this routine to be merged with the real
     * page_tree dictionary, while keeping the indirect object
     * references right.
     */
    let self_ref = if !parent_ref.is_null() {
        pdf_ref_obj(self_0)
    } else {
        pdf_ref_obj((*p).root.pages)
    };
    pdf_add_dict(&mut *self_0, "Type", pdf_new_name("Pages"));
    pdf_add_dict(&mut *self_0, "Count", pdf_new_number(num_pages as f64));
    if !parent_ref.is_null() {
        pdf_add_dict(&mut *self_0, "Parent", parent_ref);
    }
    let kids = pdf_new_array();
    if num_pages > 0i32 && num_pages <= 4i32 {
        for i in 0..num_pages {
            let page = firstpage.offset(i as isize);
            if (*page).page_ref.is_null() {
                (*page).page_ref = pdf_ref_obj((*page).page_obj)
            }
            pdf_add_array(&mut *kids, pdf_link_obj((*page).page_ref));
            doc_flush_page(p, page, pdf_link_obj(self_ref));
        }
    } else if num_pages > 0i32 {
        for i in 0..4 {
            let start = i * num_pages / 4i32;
            let end = (i + 1i32) * num_pages / 4i32;
            if end - start > 1i32 {
                let subtree = build_page_tree(
                    p,
                    firstpage.offset(start as isize),
                    end - start,
                    pdf_link_obj(self_ref),
                );
                pdf_add_array(&mut *kids, pdf_ref_obj(subtree));
                pdf_release_obj(subtree);
            } else {
                let page_0 = firstpage.offset(start as isize);
                if (*page_0).page_ref.is_null() {
                    (*page_0).page_ref = pdf_ref_obj((*page_0).page_obj)
                }
                pdf_add_array(&mut *kids, pdf_link_obj((*page_0).page_ref));
                doc_flush_page(p, page_0, pdf_link_obj(self_ref));
            }
        }
    }
    pdf_add_dict(&mut *self_0, "Kids", kids);
    pdf_release_obj(self_ref);
    self_0
}
unsafe fn pdf_doc_init_page_tree(mut p: *mut pdf_doc, mut media_width: f64, mut media_height: f64) {
    /*
     * Create empty page tree.
     * The docroot.pages is kept open until the document is closed.
     * This allows the user to write to pages if he so choses.
     */
    (*p).root.pages = pdf_new_dict();
    (*p).pages.num_entries = 0_u32;
    (*p).pages.max_entries = 0_u32;
    (*p).pages.entries = ptr::null_mut();
    (*p).pages.bop = ptr::null_mut();
    (*p).pages.eop = ptr::null_mut();
    (*p).pages.mediabox = Rect::new((0., 0.), (media_width, media_height));
}
unsafe fn pdf_doc_close_page_tree(mut p: *mut pdf_doc) {
    /*
     * Do consistency check on forward references to pages.
     */
    for page_no in (*p).pages.num_entries.wrapping_add(1_u32)..=(*p).pages.max_entries {
        let page = doc_get_page_entry(p, page_no);
        if !(*page).page_obj.is_null() {
            warn!("Nonexistent page #{} refered.", page_no);
            pdf_release_obj((*page).page_ref);
            (*page).page_ref = ptr::null_mut()
        }
        if !(*page).page_obj.is_null() {
            warn!("Entry for a nonexistent page #{} created.", page_no);
            pdf_release_obj((*page).page_obj);
            (*page).page_obj = ptr::null_mut()
        }
        if !(*page).annots.is_null() {
            warn!("Annotation attached to a nonexistent page #{}.", page_no);
            pdf_release_obj((*page).annots);
            (*page).annots = ptr::null_mut()
        }
        if !(*page).beads.is_null() {
            warn!("Article beads attached to a nonexistent page #{}.", page_no);
            pdf_release_obj((*page).beads);
            (*page).beads = ptr::null_mut()
        }
        if !(*page).resources.is_null() {
            pdf_release_obj((*page).resources);
            (*page).resources = ptr::null_mut()
        }
    }
    /*
     * Connect page tree to root node.
     */
    let page_tree_root = build_page_tree(
        p,
        &mut *(*p).pages.entries.offset(0),
        (*p).pages.num_entries as i32,
        ptr::null_mut(),
    );
    pdf_merge_dict(&mut *(*p).root.pages, &*page_tree_root);
    pdf_release_obj(page_tree_root);
    /* They must be after build_page_tree() */
    if !(*p).pages.bop.is_null() {
        pdf_add_stream(
            &mut *(*p).pages.bop,
            b"\n\x00" as *const u8 as *const i8 as *const libc::c_void,
            1i32,
        );
        pdf_release_obj((*p).pages.bop);
        (*p).pages.bop = ptr::null_mut()
    }
    if !(*p).pages.eop.is_null() {
        pdf_add_stream(
            &mut *(*p).pages.eop,
            b"\n\x00" as *const u8 as *const i8 as *const libc::c_void,
            1i32,
        );
        pdf_release_obj((*p).pages.eop);
        (*p).pages.eop = ptr::null_mut()
    }
    /* Create media box at root node and let the other pages inherit it. */
    let mediabox = pdf_new_array();
    pdf_add_array(
        &mut *mediabox,
        pdf_new_number(((*p).pages.mediabox.ll.x / 0.01 + 0.5).floor() * 0.01),
    );
    pdf_add_array(
        &mut *mediabox,
        pdf_new_number(((*p).pages.mediabox.ll.y / 0.01 + 0.5).floor() * 0.01),
    );
    pdf_add_array(
        &mut *mediabox,
        pdf_new_number(((*p).pages.mediabox.ur.x / 0.01 + 0.5).floor() * 0.01),
    );
    pdf_add_array(
        &mut *mediabox,
        pdf_new_number(((*p).pages.mediabox.ur.y / 0.01 + 0.5).floor() * 0.01),
    );
    pdf_add_dict(&mut *(*p).root.pages, "MediaBox", mediabox);
    pdf_add_dict(&mut *(*p).root.dict, "Pages", pdf_ref_obj((*p).root.pages));
    pdf_release_obj((*p).root.pages);
    (*p).root.pages = ptr::null_mut();
    (*p).pages.entries = mfree((*p).pages.entries as *mut libc::c_void) as *mut pdf_page;
    (*p).pages.num_entries = 0_u32;
    (*p).pages.max_entries = 0_u32;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_get_page_count(mut pf: *mut pdf_file) -> i32 {
    let catalog = pdf_file_get_catalog(pf);
    let page_tree = pdf_deref_obj((*catalog).as_dict_mut().get_mut("Pages"));
    if !(!page_tree.is_null() && (*page_tree).is_dict()) {
        return 0i32;
    }
    let mut tmp: *mut pdf_obj = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Count"));
    if !(!tmp.is_null() && (*tmp).is_number()) {
        pdf_release_obj(tmp);
        return 0i32;
    }
    let count = pdf_number_value(&*tmp) as i32;
    pdf_release_obj(tmp);
    count
}
/*
 * From PDFReference15_v6.pdf (p.119 and p.834)
 *
 * MediaBox rectangle (Required; inheritable)
 *
 * The media box defines the boundaries of the physical medium on which the
 * page is to be printed. It may include any extended area surrounding the
 * finished page for bleed, printing marks, or other such purposes. It may
 * also include areas close to the edges of the medium that cannot be marked
 * because of physical limitations of the output device. Content falling
 * outside this boundary can safely be discarded without affecting the
 * meaning of the PDF file.
 *
 * CropBox rectangle (Optional; inheritable)
 *
 * The crop box defines the region to which the contents of the page are to be
 * clipped (cropped) when displayed or printed. Unlike the other boxes, the
 * crop box has no defined meaning in terms of physical page geometry or
 * intended use; it merely imposes clipping on the page contents. However,
 * in the absence of additional information (such as imposition instructions
 * specified in a JDF or PJTF job ticket), the crop box will determine how
 * the page's contents are to be positioned on the output medium. The default
 * value is the page's media box.
 *
 * BleedBox rectangle (Optional; PDF 1.3)
 *
 * The bleed box (PDF 1.3) defines the region to which the contents of the
 * page should be clipped when output in a production environment. This may
 * include any extra "bleed area" needed to accommodate the physical
 * limitations of cutting, folding, and trimming equipment. The actual printed
 * page may include printing marks that fall outside the bleed box.
 * The default value is the page's crop box.
 *
 * TrimBox rectangle (Optional; PDF 1.3)
 *
 * The trim box (PDF 1.3) defines the intended dimensions of the finished page
 * after trimming. It may be smaller than the media box, to allow for
 * production-related content such as printing instructions, cut marks, or
 * color bars. The default value is the page's crop box.
 *
 * ArtBox rectangle (Optional; PDF 1.3)
 *
 * The art box (PDF 1.3) defines the extent of the page's meaningful content
 * (including potential white space) as intended by the page's creator.
 * The default value is the page's crop box.
 *
 * Rotate integer (Optional; inheritable)
 *
 * The number of degrees by which the page should be rotated clockwise when
 * displayed or printed. The value must be a multiple of 90. Default value: 0.
 */
/* count_p removed: Please use different interface if you want to get total page
 * number. pdf_doc_get_page() is obviously not an interface to do such.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_get_page(
    mut pf: *mut pdf_file,
    mut page_no: i32,
    mut options: i32,
    bbox: &mut Rect,
    matrix: &mut TMatrix,
    mut resources_p: *mut *mut pdf_obj,
) -> *mut pdf_obj
/* returned values */ {
    let mut current_block: u64;
    let mut resources: *mut pdf_obj = ptr::null_mut();
    let mut box_0: *mut pdf_obj = ptr::null_mut();
    let mut rotate: *mut pdf_obj = ptr::null_mut();
    let catalog = pdf_file_get_catalog(pf);
    let mut page_tree = pdf_deref_obj((*catalog).as_dict_mut().get_mut("Pages"));
    if !(!page_tree.is_null() && (*page_tree).is_dict()) {
        current_block = 7715203803291643663;
    } else {
        let mut tmp: *mut pdf_obj = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Count"));
        if !(!tmp.is_null() && (*tmp).is_number()) {
            pdf_release_obj(tmp);
            current_block = 7715203803291643663;
        } else {
            let count = pdf_number_value(&*tmp) as i32;
            pdf_release_obj(tmp);
            if page_no <= 0i32 || page_no > count {
                warn!("Page {} does not exist.", page_no);
                current_block = 5059794928954228255;
            } else {
                /*
                 * Seek correct page. Get MediaBox, CropBox and Resources.
                 * (Note that these entries can be inherited.)
                 */
                let mut art_box: *mut pdf_obj = ptr::null_mut();
                let mut trim_box: *mut pdf_obj = ptr::null_mut();
                let mut bleed_box: *mut pdf_obj = ptr::null_mut();
                let mut media_box: *mut pdf_obj = ptr::null_mut();
                let mut crop_box: *mut pdf_obj = ptr::null_mut();
                let mut depth: i32 = 30i32;
                let mut page_idx: i32 = page_no - 1i32;
                let mut kids_length: i32 = 1i32;
                let mut i: i32 = 0i32;
                's_83: loop {
                    depth -= 1;
                    if !(depth != 0 && i != kids_length) {
                        current_block = 13707613154239713890;
                        break;
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("MediaBox"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(media_box);
                        media_box = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("CropBox"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(crop_box);
                        crop_box = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("ArtBox"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(art_box);
                        art_box = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("TrimBox"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(trim_box);
                        trim_box = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("BleedBox"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(bleed_box);
                        bleed_box = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Rotate"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(rotate);
                        rotate = tmp_0
                    }
                    let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Resources"));
                    if !tmp_0.is_null() {
                        pdf_release_obj(resources);
                        resources = tmp_0
                    }
                    let kids = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Kids"));
                    if kids.is_null() {
                        current_block = 13707613154239713890;
                        break;
                    }
                    if !(!kids.is_null() && (*kids).is_array()) {
                        pdf_release_obj(kids);
                        current_block = 7715203803291643663;
                        break;
                    } else {
                        kids_length = pdf_array_length(&*kids) as i32;
                        i = 0i32;
                        while i < kids_length {
                            let count_0;
                            pdf_release_obj(page_tree);
                            page_tree = pdf_deref_obj((*kids).as_array_mut().get_mut(i));
                            if !(!page_tree.is_null()
                                && (*page_tree).is_dict())
                            {
                                current_block = 7715203803291643663;
                                break 's_83;
                            }
                            let tmp_0 = pdf_deref_obj((*page_tree).as_dict_mut().get_mut("Count"));
                            if !tmp_0.is_null() && (*tmp_0).is_number() {
                                /* Pages object */
                                count_0 = pdf_number_value(&*tmp_0) as i32;
                                pdf_release_obj(tmp_0);
                            } else if tmp_0.is_null() {
                                /* Page object */
                                count_0 = 1i32
                            } else {
                                pdf_release_obj(tmp_0);
                                current_block = 7715203803291643663;
                                break 's_83;
                            }
                            if page_idx < count_0 {
                                break;
                            }
                            page_idx -= count_0;
                            i += 1
                        }
                        pdf_release_obj(kids);
                    }
                }
                match current_block {
                    7715203803291643663 => {}
                    _ => {
                        if depth == 0 || kids_length == i {
                            pdf_release_obj(media_box);
                            pdf_release_obj(crop_box);
                        } else {
                            /* Nasty BBox selection... */
                            if options == 0i32 || options == 1i32 {
                                if !crop_box.is_null() {
                                    box_0 = crop_box
                                } else {
                                    box_0 = media_box;
                                    if box_0.is_null()
                                        && {
                                            box_0 = bleed_box;
                                            box_0.is_null()
                                        }
                                        && {
                                            box_0 = trim_box;
                                            box_0.is_null()
                                        }
                                        && !art_box.is_null()
                                    {
                                        box_0 = art_box
                                    }
                                }
                            } else if options == 2i32 {
                                if !media_box.is_null() {
                                    box_0 = media_box
                                } else {
                                    box_0 = crop_box;
                                    if box_0.is_null()
                                        && {
                                            box_0 = bleed_box;
                                            box_0.is_null()
                                        }
                                        && {
                                            box_0 = trim_box;
                                            box_0.is_null()
                                        }
                                        && !art_box.is_null()
                                    {
                                        box_0 = art_box
                                    }
                                }
                            } else if options == 3i32 {
                                if !art_box.is_null() {
                                    box_0 = art_box
                                } else {
                                    box_0 = crop_box;
                                    if box_0.is_null()
                                        && {
                                            box_0 = media_box;
                                            box_0.is_null()
                                        }
                                        && {
                                            box_0 = bleed_box;
                                            box_0.is_null()
                                        }
                                        && !trim_box.is_null()
                                    {
                                        box_0 = trim_box
                                    }
                                }
                            } else if options == 4i32 {
                                if !trim_box.is_null() {
                                    box_0 = trim_box
                                } else {
                                    box_0 = crop_box;
                                    if box_0.is_null()
                                        && {
                                            box_0 = media_box;
                                            box_0.is_null()
                                        }
                                        && {
                                            box_0 = bleed_box;
                                            box_0.is_null()
                                        }
                                        && !art_box.is_null()
                                    {
                                        box_0 = art_box
                                    }
                                }
                            } else if options == 5i32 {
                                if !bleed_box.is_null() {
                                    box_0 = bleed_box
                                } else {
                                    box_0 = crop_box;
                                    if box_0.is_null()
                                        && {
                                            box_0 = media_box;
                                            box_0.is_null()
                                        }
                                        && {
                                            box_0 = trim_box;
                                            box_0.is_null()
                                        }
                                        && !art_box.is_null()
                                    {
                                        box_0 = art_box
                                    }
                                }
                            }
                            let medbox = media_box;
                            if !(!(!box_0.is_null() && (*box_0).is_array())
                                || pdf_array_length(&*box_0) != 4
                                || !(!resources.is_null()
                                    && (*resources).is_dict()))
                            {
                                let mut i_0 = 4;
                                loop {
                                    let fresh11 = i_0;
                                    i_0 = i_0 - 1;
                                    if !(fresh11 != 0) {
                                        current_block = 13014351284863956202;
                                        break;
                                    }
                                    let mut tmp_1: *mut pdf_obj =
                                        pdf_deref_obj((*box_0).as_array_mut().get_mut(i_0));
                                    if !(!tmp_1.is_null()
                                        && (*tmp_1).is_number())
                                    {
                                        pdf_release_obj(tmp_1);
                                        current_block = 7715203803291643663;
                                        break;
                                    } else {
                                        let x = pdf_number_value(&*tmp_1);
                                        match i_0 {
                                            0 => bbox.ll.x = x,
                                            1 => bbox.ll.y = x,
                                            2 => bbox.ur.x = x,
                                            3 => bbox.ur.y = x,
                                            _ => {}
                                        }
                                        pdf_release_obj(tmp_1);
                                    }
                                }
                                match current_block {
                                    7715203803291643663 => {}
                                    _ =>
                                    /* New scheme only for XDV files */
                                    {
                                        if !medbox.is_null() && (is_xdv != 0 || options != 0) {
                                            i_0 = 4i32;
                                            loop {
                                                let fresh12 = i_0;
                                                i_0 = i_0 - 1;
                                                if !(fresh12 != 0) {
                                                    current_block = 10570719081292997246;
                                                    break;
                                                }
                                                let mut tmp_2: *mut pdf_obj =
                                                    pdf_deref_obj((*medbox).as_array_mut().get_mut(i_0));
                                                if !(!tmp_2.is_null()
                                                    && (*tmp_2).is_number())
                                                {
                                                    pdf_release_obj(tmp_2);
                                                    current_block = 7715203803291643663;
                                                    break;
                                                } else {
                                                    let x_0 = pdf_number_value(&*tmp_2);
                                                    match i_0 {
                                                        0 => {
                                                            if bbox.ll.x < x_0 {
                                                                bbox.ll.x = x_0
                                                            }
                                                        }
                                                        1 => {
                                                            if bbox.ll.y < x_0 {
                                                                bbox.ll.y = x_0
                                                            }
                                                        }
                                                        2 => {
                                                            if bbox.ur.x > x_0 {
                                                                bbox.ur.x = x_0
                                                            }
                                                        }
                                                        3 => {
                                                            if bbox.ur.y > x_0 {
                                                                bbox.ur.y = x_0
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                    pdf_release_obj(tmp_2);
                                                }
                                            }
                                        } else {
                                            current_block = 10570719081292997246;
                                        }
                                        match current_block {
                                            7715203803291643663 => {}
                                            _ => {
                                                pdf_release_obj(box_0);
                                                matrix.d = 1.0f64;
                                                matrix.a = matrix.d;
                                                matrix.c = 0.0f64;
                                                matrix.b = matrix.c;
                                                matrix.f = 0.0f64;
                                                matrix.e = matrix.f;
                                                if !rotate.is_null()
                                                    && (*rotate).is_number()
                                                {
                                                    let mut deg: f64 = pdf_number_value(&*rotate);
                                                    if deg - deg as i32 as f64 != 0.0f64 {
                                                        warn!("Invalid value specified for /Rotate: {}",
                                                                    deg);
                                                    } else if deg != 0.0f64 {
                                                        let mut rot: i32 = deg as i32;
                                                        if (rot % 90i32) as f64 != 0.0f64 {
                                                            warn!("Invalid value specified for /Rotate: {}",
                                                                        deg);
                                                        } else {
                                                            rot = rot % 360i32;
                                                            if rot < 0i32 {
                                                                rot += 360i32
                                                            }
                                                            match rot {
                                                                90 => {
                                                                    matrix.d = 0i32 as f64;
                                                                    matrix.a = matrix.d;
                                                                    matrix.b = -1i32 as f64;
                                                                    matrix.c = 1i32 as f64;
                                                                    matrix.e = bbox.ll.x - bbox.ll.y;
                                                                    matrix.f = bbox.ll.y + bbox.ur.x
                                                                }
                                                                180 => {
                                                                    matrix.d = -1i32 as f64;
                                                                    matrix.a = matrix.d;
                                                                    matrix.c = 0i32 as f64;
                                                                    matrix.b = matrix.c;
                                                                    matrix.e = bbox.ll.x + bbox.ur.x;
                                                                    matrix.f = bbox.ll.y + bbox.ur.y
                                                                }
                                                                270 => {
                                                                    matrix.d = 0i32 as f64;
                                                                    matrix.a = matrix.d;
                                                                    matrix.b = 1i32 as f64;
                                                                    matrix.c = -1i32 as f64;
                                                                    matrix.e = bbox.ll.x + bbox.ur.y;
                                                                    matrix.f = bbox.ll.y - bbox.ll.x
                                                                }
                                                                _ => {}
                                                            }
                                                        }
                                                    }
                                                    pdf_release_obj(rotate);
                                                    rotate = ptr::null_mut();
                                                    current_block = 3151994457458062110;
                                                } else if !rotate.is_null() {
                                                    current_block = 7715203803291643663;
                                                } else {
                                                    current_block = 3151994457458062110;
                                                }
                                                match current_block {
                                                    7715203803291643663 => {}
                                                    _ => {
                                                        if !resources_p.is_null() {
                                                            *resources_p = resources
                                                        } else {
                                                            pdf_release_obj(resources);
                                                        }
                                                        return page_tree;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        current_block = 7715203803291643663;
                    }
                }
            }
        }
    }
    match current_block {
        7715203803291643663 => {
            warn!("Cannot parse document. Broken PDF file?");
        }
        _ => {}
    }
    pdf_release_obj(box_0);
    pdf_release_obj(rotate);
    pdf_release_obj(resources);
    pdf_release_obj(page_tree);
    ptr::null_mut()
}
unsafe fn pdf_doc_init_bookmarks(mut p: *mut pdf_doc, mut bm_open_depth: i32) {
    (*p).opt.outline_open_depth = (if bm_open_depth >= 0i32 {
        bm_open_depth as u32
    } else {
        256u32.wrapping_sub(bm_open_depth as u32)
    }) as i32;
    (*p).outlines.current_depth = 1i32;
    let item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
        as *mut pdf_olitem;
    (*item).dict = ptr::null_mut();
    (*item).next = ptr::null_mut();
    (*item).first = ptr::null_mut();
    (*item).parent = ptr::null_mut();
    (*item).is_open = 1i32;
    (*p).outlines.current = item;
    (*p).outlines.first = item;
}
unsafe fn clean_bookmarks(mut item: *mut pdf_olitem) -> i32 {
    while !item.is_null() {
        let next = (*item).next;
        pdf_release_obj((*item).dict);
        if !(*item).first.is_null() {
            clean_bookmarks((*item).first);
        }
        free(item as *mut libc::c_void);
        item = next
    }
    0i32
}
unsafe fn flush_bookmarks(
    mut node: *mut pdf_olitem,
    mut parent_ref: *mut pdf_obj,
    parent_dict: &mut pdf_obj,
) -> i32 {
    assert!(!(*node).dict.is_null());
    let mut this_ref = pdf_ref_obj((*node).dict);
    pdf_add_dict(parent_dict, "First", pdf_link_obj(this_ref));
    let mut retval = 0;
    let mut item = node;
    let mut prev_ref = ptr::null_mut::<pdf_obj>();
    while !item.is_null() && !(*item).dict.is_null() {
        if !(*item).first.is_null() && !(*(*item).first).dict.is_null() {
            let count = flush_bookmarks((*item).first, this_ref, &mut *(*item).dict);
            if (*item).is_open != 0 {
                pdf_add_dict(&mut *(*item).dict, "Count", pdf_new_number(count as f64));
                retval += count
            } else {
                pdf_add_dict(&mut *(*item).dict, "Count", pdf_new_number(-count as f64));
            }
        }
        pdf_add_dict(&mut *(*item).dict, "Parent", pdf_link_obj(parent_ref));
        if !prev_ref.is_null() {
            pdf_add_dict(&mut *(*item).dict, "Prev", prev_ref);
        }
        let next_ref;
        if !(*item).next.is_null() && !(*(*item).next).dict.is_null() {
            next_ref = pdf_ref_obj((*(*item).next).dict);
            pdf_add_dict(&mut *(*item).dict, "Next", pdf_link_obj(next_ref));
        } else {
            next_ref = ptr::null_mut()
        }
        pdf_release_obj((*item).dict);
        (*item).dict = ptr::null_mut();
        prev_ref = this_ref;
        this_ref = next_ref;
        retval += 1;
        item = (*item).next
    }
    pdf_add_dict(parent_dict, "Last", pdf_link_obj(prev_ref));
    pdf_release_obj(prev_ref);
    pdf_release_obj((*node).dict);
    (*node).dict = ptr::null_mut();
    retval
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_bookmarks_up() -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    let item = (*p).outlines.current;
    if item.is_null() || (*item).parent.is_null() {
        warn!("Can\'t go up above the bookmark root node!");
        return -1i32;
    }
    let parent = (*item).parent;
    let mut item = (*parent).next;
    if (*parent).next.is_null() {
        item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
            as *mut pdf_olitem;
        (*parent).next = item;
        (*item).dict = ptr::null_mut();
        (*item).first = ptr::null_mut();
        (*item).next = ptr::null_mut();
        (*item).is_open = 0i32;
        (*item).parent = (*parent).parent
    }
    (*p).outlines.current = item;
    (*p).outlines.current_depth -= 1;
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_bookmarks_down() -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    let item = (*p).outlines.current;
    if (*item).dict.is_null() {
        warn!("Empty bookmark node!");
        warn!("You have tried to jump more than 1 level.");
        (*item).dict = pdf_new_dict();
        pdf_add_dict(
            &mut *(*item).dict,
            "Title",
            pdf_new_string(
                b"<No Title>\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"<No Title>\x00" as *const u8 as *const i8) as _,
            ),
        );
        let tcolor = pdf_new_array();
        pdf_add_array(&mut *tcolor, pdf_new_number(1.0f64));
        pdf_add_array(&mut *tcolor, pdf_new_number(0.0f64));
        pdf_add_array(&mut *tcolor, pdf_new_number(0.0f64));
        pdf_add_dict(&mut *(*item).dict, "C", pdf_link_obj(tcolor));
        pdf_release_obj(tcolor);
        pdf_add_dict(&mut *(*item).dict, "F", pdf_new_number(1.0f64));
        let action = pdf_new_dict();
        pdf_add_dict(&mut *action, "S", pdf_new_name("JavaScript"));
        pdf_add_dict(&mut *action,
                     "JS",
                     pdf_new_string(b"app.alert(\"The author of this document made this bookmark item empty!\", 3, 0)\x00"
                                        as *const u8 as *const i8 as
                                        *const libc::c_void,
                                    strlen(b"app.alert(\"The author of this document made this bookmark item empty!\", 3, 0)\x00"
                                               as *const u8 as
                                               *const i8) as _));
        pdf_add_dict(&mut *(*item).dict, "A", pdf_link_obj(action));
        pdf_release_obj(action);
    }
    let first = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
        as *mut pdf_olitem;
    (*item).first = first;
    (*first).dict = ptr::null_mut();
    (*first).is_open = 0i32;
    (*first).parent = item;
    (*first).next = ptr::null_mut();
    (*first).first = ptr::null_mut();
    (*p).outlines.current = first;
    (*p).outlines.current_depth += 1;
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_bookmarks_depth() -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    (*p).outlines.current_depth
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_bookmarks_add(mut dict: *mut pdf_obj, mut is_open: i32) {
    let mut p: *mut pdf_doc = &mut pdoc;
    assert!(!p.is_null() && !dict.is_null());
    let mut item = (*p).outlines.current;
    if item.is_null() {
        item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
            as *mut pdf_olitem;
        (*item).parent = ptr::null_mut();
        (*p).outlines.first = item
    } else if !(*item).dict.is_null() {
        /* go to next item */
        item = (*item).next
    }
    (*item).dict = dict;
    (*item).first = ptr::null_mut();
    (*item).is_open = if is_open < 0i32 {
        if (*p).outlines.current_depth > (*p).opt.outline_open_depth {
            0i32
        } else {
            1i32
        }
    } else {
        is_open
    };
    let next = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
        as *mut pdf_olitem;
    (*item).next = next;
    (*next).dict = ptr::null_mut();
    (*next).parent = (*item).parent;
    (*next).first = ptr::null_mut();
    (*next).is_open = -1i32;
    (*next).next = ptr::null_mut();
    (*p).outlines.current = item;
    pdf_doc_add_goto(dict);
}
unsafe fn pdf_doc_close_bookmarks(mut p: *mut pdf_doc) {
    let mut catalog: *mut pdf_obj = (*p).root.dict;
    let item = (*p).outlines.first;
    if !(*item).dict.is_null() {
        let bm_root = pdf_new_dict();
        let bm_root_ref = pdf_ref_obj(bm_root);
        let count = flush_bookmarks(item, bm_root_ref, &mut *bm_root);
        pdf_add_dict(&mut *bm_root, "Count", pdf_new_number(count as f64));
        pdf_add_dict(&mut *catalog, "Outlines", bm_root_ref);
        pdf_release_obj(bm_root);
    }
    clean_bookmarks(item);
    (*p).outlines.first = ptr::null_mut();
    (*p).outlines.current = ptr::null_mut();
    (*p).outlines.current_depth = 0i32;
}
static mut name_dict_categories: [*const i8; 10] = [
    b"Dests\x00" as *const u8 as *const i8,
    b"AP\x00" as *const u8 as *const i8,
    b"JavaScript\x00" as *const u8 as *const i8,
    b"Pages\x00" as *const u8 as *const i8,
    b"Templates\x00" as *const u8 as *const i8,
    b"IDS\x00" as *const u8 as *const i8,
    b"URLS\x00" as *const u8 as *const i8,
    b"EmbeddedFiles\x00" as *const u8 as *const i8,
    b"AlternatePresentations\x00" as *const u8 as *const i8,
    b"Renditions\x00" as *const u8 as *const i8,
];
unsafe fn pdf_doc_init_names(mut p: *mut pdf_doc, mut check_gotos: i32) {
    (*p).root.names = ptr::null_mut();
    (*p).names = new(((::std::mem::size_of::<[*const i8; 10]>() as u64)
        .wrapping_div(::std::mem::size_of::<*const i8>() as u64)
        .wrapping_add(1i32 as u64) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<name_dict>() as u64) as u32)
        as *mut name_dict;
    for i in 0..(::std::mem::size_of::<[*const i8; 10]>() as u64)
        .wrapping_div(::std::mem::size_of::<*const i8>() as u64)
    {
        let ref mut fresh13 = (*(*p).names.offset(i as isize)).category;
        *fresh13 = name_dict_categories[i as usize];
        let ref mut fresh14 = (*(*p).names.offset(i as isize)).data;
        *fresh14 = if strcmp(
            name_dict_categories[i as usize],
            b"Dests\x00" as *const u8 as *const i8,
        ) != 0
        {
            ptr::null_mut()
        } else {
            pdf_new_name_tree()
        };
        /*
         * We need a non-null entry for PDF destinations in order to find
         * broken links even if no destination is defined in the DVI file.
         */
    }
    let ref mut fresh15 = (*(*p).names.offset(
        (::std::mem::size_of::<[*const i8; 10]>() as u64)
            .wrapping_div(::std::mem::size_of::<*const i8>() as u64) as isize,
    ))
    .category;
    *fresh15 = ptr::null();
    let ref mut fresh16 = (*(*p).names.offset(
        (::std::mem::size_of::<[*const i8; 10]>() as u64)
            .wrapping_div(::std::mem::size_of::<*const i8>() as u64) as isize,
    ))
    .data;
    *fresh16 = ptr::null_mut();
    (*p).check_gotos = check_gotos;
    ht_init_table(
        &mut (*p).gotos,
        ::std::mem::transmute::<
            Option<unsafe extern "C" fn(_: *mut pdf_obj) -> ()>,
            Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
        >(Some(
            pdf_release_obj as unsafe extern "C" fn(_: *mut pdf_obj) -> (),
        )),
    );
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_add_names(
    mut category: *const i8,
    mut key: *const libc::c_void,
    mut keylen: i32,
    mut value: *mut pdf_obj,
) -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    let mut i = 0;
    while !(*(*p).names.offset(i as isize)).category.is_null() {
        if streq_ptr((*(*p).names.offset(i as isize)).category, category) {
            break;
        }
        i += 1;
    }
    if (*(*p).names.offset(i as isize)).category.is_null() {
        warn!(
            "Unknown name dictionary category \"{}\".",
            CStr::from_ptr(category).display()
        );
        return -1i32;
    }
    if (*(*p).names.offset(i as isize)).data.is_null() {
        let ref mut fresh17 = (*(*p).names.offset(i as isize)).data;
        *fresh17 = pdf_new_name_tree()
    }
    pdf_names_add_object((*(*p).names.offset(i as isize)).data, key, keylen, value)
}
unsafe fn pdf_doc_add_goto(mut annot_dict: *mut pdf_obj) {
    let mut current_block: u64;
    let mut A: *mut pdf_obj = ptr::null_mut();
    let mut S: *mut pdf_obj = ptr::null_mut();
    let mut D: *mut pdf_obj = ptr::null_mut();
    if pdoc.check_gotos == 0 {
        return;
    }
    /*
     * An annotation dictionary coming from an annotation special
     * must have a "Subtype". An annotation dictionary coming from
     * an outline special has none.
     */
    let subtype = pdf_deref_obj((*annot_dict).as_dict_mut().get_mut("Subtype"));
    if !subtype.is_null() {
        if !subtype.is_null() && pdf_obj_typeof(subtype) == PdfObjType::UNDEFINED {
            current_block = 14825911176647684745;
        } else if !(!subtype.is_null() && (*subtype).is_name()) {
            current_block = 10743935136377679094;
        } else if pdf_name_value(&*subtype).to_string_lossy() != "Link" {
            current_block = 6401626691277551363;
        } else {
            current_block = 3276175668257526147;
        }
    } else {
        current_block = 3276175668257526147;
    }
    match current_block {
        3276175668257526147 => {
            let mut dict = annot_dict;
            let mut key = "Dest";
            D = pdf_deref_obj((*annot_dict).as_dict_mut().get_mut(key));
            if !D.is_null() && pdf_obj_typeof(D) == PdfObjType::UNDEFINED {
                current_block = 14825911176647684745;
            } else {
                A = pdf_deref_obj((*annot_dict).as_dict_mut().get_mut("A"));
                if !A.is_null() {
                    if !A.is_null() && pdf_obj_typeof(A) == PdfObjType::UNDEFINED {
                        current_block = 14825911176647684745;
                    } else if !D.is_null()
                        || !(!A.is_null() && (*A).is_dict())
                    {
                        current_block = 10743935136377679094;
                    } else {
                        S = pdf_deref_obj((*A).as_dict_mut().get_mut("S"));
                        if !S.is_null() && pdf_obj_typeof(S) == PdfObjType::UNDEFINED {
                            current_block = 14825911176647684745;
                        } else if !(!S.is_null() && (*S).is_name()) {
                            current_block = 10743935136377679094;
                        } else if pdf_name_value(&*S).to_string_lossy() != "GoTo" {
                            current_block = 6401626691277551363;
                        } else {
                            dict = A;
                            key = "D";
                            D = pdf_deref_obj((*A).as_dict_mut().get_mut(key));
                            current_block = 9828876828309294594;
                        }
                    }
                } else {
                    current_block = 9828876828309294594;
                }
                match current_block {
                    14825911176647684745 => {}
                    10743935136377679094 => {}
                    6401626691277551363 => {}
                    _ => {
                        if !D.is_null() && (*D).is_string() {
                            let dest = pdf_string_value(&*D) as *mut i8;
                            let destlen = pdf_string_length(&*D) as i32;
                            let mut D_new = ht_lookup_table(
                                &mut pdoc.gotos,
                                dest as *const libc::c_void,
                                destlen,
                            ) as *mut pdf_obj;
                            if D_new.is_null() {
                                let mut buf: [i8; 10] = [0; 10];
                                /* We use hexadecimal notation for our numeric destinations.
                                 * Other bases (e.g., 10+26 or 10+2*26) would be more efficient.
                                 */
                                sprintf(
                                    buf.as_mut_ptr(),
                                    b"%x\x00" as *const u8 as *const i8,
                                    ht_table_size(&mut pdoc.gotos),
                                ); /* Maybe reference */
                                D_new = pdf_new_string(
                                    buf.as_mut_ptr() as *const libc::c_void,
                                    strlen(buf.as_mut_ptr()) as _,
                                );
                                ht_append_table(
                                    &mut pdoc.gotos,
                                    dest as *const libc::c_void,
                                    destlen,
                                    D_new as *mut libc::c_void,
                                );
                            }
                            pdf_add_dict(&mut *dict, key, pdf_link_obj(D_new));
                            current_block = 6401626691277551363;
                        } else if !D.is_null() && (*D).is_array() {
                            current_block = 6401626691277551363;
                        } else if !D.is_null() && pdf_obj_typeof(D) == PdfObjType::UNDEFINED {
                            current_block = 14825911176647684745;
                        } else {
                            current_block = 10743935136377679094;
                        }
                    }
                }
            }
        }
        _ => {}
    }
    match current_block {
        14825911176647684745 => {
            warn!("Cannot optimize PDF annotations. Output file may be broken. Please restart with option \"-C 0x10\"\n");
        }
        10743935136377679094 => {
            warn!("Unknown PDF annotation format. Output file may be broken.");
        }
        _ => {}
    }
    pdf_release_obj(subtype);
    pdf_release_obj(A);
    pdf_release_obj(S);
    pdf_release_obj(D);
}
unsafe fn warn_undef_dests(mut dests: *mut ht_table, mut gotos: *mut ht_table) {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: ptr::null_mut(),
        hash: ptr::null_mut(),
    };
    if ht_set_iter(gotos, &mut iter) < 0i32 {
        return;
    }
    loop {
        let mut keylen: i32 = 0;
        let mut key: *mut i8 = ht_iter_getkey(&mut iter, &mut keylen);
        if ht_lookup_table(dests, key as *const libc::c_void, keylen).is_null() {
            let mut dest: *mut i8 = new(((keylen + 1i32) as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<i8>() as u64)
                as u32) as *mut i8;
            memcpy(
                dest as *mut libc::c_void,
                key as *const libc::c_void,
                keylen as _,
            );
            *dest.offset(keylen as isize) = 0_i8;
            warn!(
                "PDF destination \"{}\" not defined.",
                CStr::from_ptr(dest).display()
            );
            free(dest as *mut libc::c_void);
        }
        if !(ht_iter_next(&mut iter) >= 0i32) {
            break;
        }
    }
    ht_clear_iter(&mut iter);
}
unsafe fn pdf_doc_close_names(mut p: *mut pdf_doc) {
    let mut i = 0;
    while !(*(*p).names.offset(i as isize)).category.is_null() {
        if !(*(*p).names.offset(i as isize)).data.is_null() {
            let mut data: *mut ht_table = (*(*p).names.offset(i as isize)).data;
            let mut count: i32 = 0;
            let name_tree;
            if pdoc.check_gotos == 0
                || strcmp(
                    (*(*p).names.offset(i as isize)).category,
                    b"Dests\x00" as *const u8 as *const i8,
                ) != 0
            {
                name_tree = pdf_names_create_tree(data, &mut count, ptr::null_mut());
            } else {
                name_tree = pdf_names_create_tree(data, &mut count, &mut pdoc.gotos);
                if verbose != 0 && count < (*data).count {
                    info!(
                        "\nRemoved {} unused PDF destinations\n",
                        (*data).count - count
                    );
                }
                if count < pdoc.gotos.count {
                    warn_undef_dests(data, &mut pdoc.gotos);
                }
            }
            if !name_tree.is_null() {
                if (*p).root.names.is_null() {
                    (*p).root.names = pdf_new_dict()
                }
                pdf_add_dict(
                    &mut *(*p).root.names,
                    CStr::from_ptr((*(*p).names.offset(i as isize)).category)
                        .to_str()
                        .unwrap(),
                    pdf_ref_obj(name_tree),
                );
                pdf_release_obj(name_tree);
            }
            pdf_delete_name_tree(&mut (*(*p).names.offset(i as isize)).data);
        }
        i += 1;
    }
    if !(*p).root.names.is_null() {
        let tmp = (*(*p).root.dict).as_dict().get("Names");
        if tmp.is_none() {
            pdf_add_dict(&mut *(*p).root.dict, "Names", pdf_ref_obj((*p).root.names));
        } else if let Some(tmp) = tmp.filter(|&tmp| (*tmp).is_dict()) {
            pdf_merge_dict(&mut *(*p).root.names, &*tmp);
            pdf_add_dict(&mut *(*p).root.dict, "Names", pdf_ref_obj((*p).root.names));
        } else {
            /* What should I do? */
            warn!("Could not modify Names dictionary.");
        }
        pdf_release_obj((*p).root.names);
        (*p).root.names = ptr::null_mut()
    }
    (*p).names = mfree((*p).names as *mut libc::c_void) as *mut name_dict;
    ht_clear_table(&mut (*p).gotos);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_add_annot(
    mut page_no: u32,
    rect: &Rect,
    mut annot_dict: *mut pdf_obj,
    mut new_annot: i32,
) {
    let mut p: *mut pdf_doc = &mut pdoc;
    let mut annot_grow: f64 = (*p).opt.annot_grow;
    let mut annbox = Rect::zero();
    let page = doc_get_page_entry(p, page_no);
    if (*page).annots.is_null() {
        (*page).annots = pdf_new_array()
    }
    let mut mediabox = Rect::zero();
    pdf_doc_get_mediabox(page_no, &mut mediabox);
    let pos = pdf_dev_get_coord();
    annbox.ll = rect.ll - pos;
    annbox.ur = rect.ur - pos;
    if annbox.ll.x < mediabox.ll.x
        || annbox.ur.x > mediabox.ur.x
        || annbox.ll.y < mediabox.ll.y
        || annbox.ur.y > mediabox.ur.y
    {
        warn!("Annotation out of page boundary.");
        warn!(
            "Current page\'s MediaBox: {}",
            mediabox,
        );
        warn!(
            "Annotation: {}",
            annbox,
        );
        warn!("Maybe incorrect paper size specified.");
    }
    if annbox.ll.x > annbox.ur.x || annbox.ll.y > annbox.ur.y {
        warn!(
            "Rectangle with negative width/height: {}",
            annbox,
        );
    }
    let rect_array = pdf_new_array();
    pdf_add_array(
        &mut *rect_array,
        pdf_new_number(((annbox.ll.x - annot_grow) / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *rect_array,
        pdf_new_number(((annbox.ll.y - annot_grow) / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *rect_array,
        pdf_new_number(((annbox.ur.x + annot_grow) / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *rect_array,
        pdf_new_number(((annbox.ur.y + annot_grow) / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_dict(&mut *annot_dict, "Rect", rect_array);
    pdf_add_array(&mut *(*page).annots, pdf_ref_obj(annot_dict));
    if new_annot != 0 {
        pdf_doc_add_goto(annot_dict);
    };
}
/*
 * PDF Article Thread
 */
unsafe fn pdf_doc_init_articles(mut p: *mut pdf_doc) {
    (*p).root.threads = ptr::null_mut();
    (*p).articles.num_entries = 0_u32;
    (*p).articles.max_entries = 0_u32;
    (*p).articles.entries = ptr::null_mut();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_begin_article(
    mut article_id: *const i8,
    mut article_info: *mut pdf_obj,
) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if article_id.is_null() || strlen(article_id) == 0 {
        panic!("Article thread without internal identifier.");
    }
    if (*p).articles.num_entries >= (*p).articles.max_entries {
        (*p).articles.max_entries = (*p).articles.max_entries.wrapping_add(16_u32);
        (*p).articles.entries = renew(
            (*p).articles.entries as *mut libc::c_void,
            ((*p).articles.max_entries as u64)
                .wrapping_mul(::std::mem::size_of::<pdf_article>() as u64) as u32,
        ) as *mut pdf_article
    }
    let article = &mut *(*p)
        .articles
        .entries
        .offset((*p).articles.num_entries as isize) as *mut pdf_article;
    (*article).id =
        new((strlen(article_id).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*article).id, article_id);
    (*article).info = article_info;
    (*article).num_beads = 0_u32;
    (*article).max_beads = 0_u32;
    (*article).beads = ptr::null_mut();
    (*p).articles.num_entries = (*p).articles.num_entries.wrapping_add(1);
}
unsafe fn find_bead(mut article: *mut pdf_article, mut bead_id: &[u8]) -> *mut pdf_bead {
    let mut bead = ptr::null_mut();
    for i in 0..(*article).num_beads {
        if CStr::from_ptr((*(*article).beads.offset(i as isize)).id).to_bytes() == bead_id {
            bead = &mut *(*article).beads.offset(i as isize) as *mut pdf_bead;
            break;
        }
    }
    bead
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_add_bead(
    mut article_id: *const i8,
    mut bead_id: &[u8],
    mut page_no: i32,
    rect: &Rect,
) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if article_id.is_null() {
        panic!("No article identifier specified.");
    }
    let mut article = ptr::null_mut();
    for i in 0..(*p).articles.num_entries {
        if streq_ptr((*(*p).articles.entries.offset(i as isize)).id, article_id) {
            article = &mut *(*p).articles.entries.offset(i as isize) as *mut pdf_article;
            break;
        }
    }
    if article.is_null() {
        panic!("Specified article thread that doesn\'t exist.");
    }
    let mut bead = if !bead_id.is_empty() {
        find_bead(article, bead_id)
    } else {
        ptr::null_mut()
    };
    if bead.is_null() {
        if (*article).num_beads >= (*article).max_beads {
            (*article).max_beads = (*article).max_beads.wrapping_add(16_u32);
            (*article).beads = renew(
                (*article).beads as *mut libc::c_void,
                ((*article).max_beads as u64).wrapping_mul(::std::mem::size_of::<pdf_bead>() as u64)
                    as u32,
            ) as *mut pdf_bead;
            for i in (*article).num_beads..(*article).max_beads {
                let ref mut fresh18 = (*(*article).beads.offset(i as isize)).id;
                *fresh18 = ptr::null_mut();
                (*(*article).beads.offset(i as isize)).page_no = -1i32;
            }
        }
        bead = &mut *(*article).beads.offset((*article).num_beads as isize) as *mut pdf_bead;
        if !bead_id.is_empty() {
            (*bead).id = CString::new(bead_id).unwrap().into_raw();
        } else {
            (*bead).id = ptr::null_mut()
        }
        (*article).num_beads = (*article).num_beads.wrapping_add(1)
    }
    (*bead).rect = *rect;
    (*bead).page_no = page_no;
}
unsafe fn make_article(
    mut p: *mut pdf_doc,
    mut article: *mut pdf_article,
    mut bead_ids: *mut *const i8,
    mut num_beads: u32,
    mut article_info: *mut pdf_obj,
) -> *mut pdf_obj {
    if article.is_null() {
        return ptr::null_mut();
    }
    let mut art_dict = pdf_new_dict();
    let mut last = ptr::null_mut();
    let mut prev = last;
    let mut first = prev;
    /*
     * The bead_ids represents logical order of beads in an article thread.
     * If bead_ids is not given, we create an article thread in the order of
     * beads appeared.
     */
    let n = (if !bead_ids.is_null() {
        num_beads
    } else {
        (*article).num_beads
    }) as i32;
    for i in 0..n {
        let bead = if !bead_ids.is_null() {
            find_bead(
                article,
                CStr::from_ptr(*bead_ids.offset(i as isize)).to_bytes(),
            )
        } else {
            &mut *(*article).beads.offset(i as isize) as *mut pdf_bead
        };
        if !(bead.is_null() || (*bead).page_no < 0i32) {
            last = pdf_new_dict();
            if prev.is_null() {
                first = last;
                pdf_add_dict(&mut *first, "T", pdf_ref_obj(art_dict));
            } else {
                pdf_add_dict(&mut *prev, "N", pdf_ref_obj(last));
                pdf_add_dict(&mut *last, "V", pdf_ref_obj(prev));
                /* We must link first to last. */
                if prev != first {
                    pdf_release_obj(prev);
                }
            }
            /* Realize bead now. */
            let page = doc_get_page_entry(p, (*bead).page_no as u32);
            if (*page).beads.is_null() {
                (*page).beads = pdf_new_array()
            }
            pdf_add_dict(&mut *last, "P", pdf_link_obj((*page).page_ref));
            let rect = pdf_new_array();
            pdf_add_array(
                &mut *rect,
                pdf_new_number(((*bead).rect.ll.x / 0.01 + 0.5).floor() * 0.01),
            );
            pdf_add_array(
                &mut *rect,
                pdf_new_number(((*bead).rect.ll.y / 0.01 + 0.5).floor() * 0.01),
            );
            pdf_add_array(
                &mut *rect,
                pdf_new_number(((*bead).rect.ur.x / 0.01 + 0.5).floor() * 0.01),
            );
            pdf_add_array(
                &mut *rect,
                pdf_new_number(((*bead).rect.ur.y / 0.01 + 0.5).floor() * 0.01),
            );
            pdf_add_dict(&mut *last, "R", rect);
            pdf_add_array(&mut *(*page).beads, pdf_ref_obj(last));
            prev = last
        }
    }
    if !first.is_null() && !last.is_null() {
        pdf_add_dict(&mut *last, "N", pdf_ref_obj(first));
        pdf_add_dict(&mut *first, "V", pdf_ref_obj(last));
        if first != last {
            pdf_release_obj(last);
        }
        pdf_add_dict(&mut *art_dict, "F", pdf_ref_obj(first));
        /* If article_info is supplied, we override article->info. */
        if !article_info.is_null() {
            pdf_add_dict(&mut *art_dict, "I", article_info);
        } else if !(*article).info.is_null() {
            pdf_add_dict(&mut *art_dict, "I", pdf_ref_obj((*article).info));
            pdf_release_obj((*article).info);
            (*article).info = ptr::null_mut()
            /* We do not write as object reference. */
        }
        pdf_release_obj(first);
    } else {
        pdf_release_obj(art_dict);
        art_dict = ptr::null_mut()
    }
    art_dict
}
unsafe fn clean_article(mut article: *mut pdf_article) {
    if article.is_null() {
        return;
    }
    if !(*article).beads.is_null() {
        for i in 0..(*article).num_beads {
            let id = (*(*article).beads.offset(i as isize)).id;
            if !id.is_null() {
                let _ = CString::from_raw(id);
            }
        }
        (*article).beads = mfree((*article).beads as *mut libc::c_void) as *mut pdf_bead
    }
    (*article).id = mfree((*article).id as *mut libc::c_void) as *mut i8;
    (*article).num_beads = 0_u32;
    (*article).max_beads = 0_u32;
}
unsafe fn pdf_doc_close_articles(mut p: *mut pdf_doc) {
    for i in 0..(*p).articles.num_entries {
        let article = &mut *(*p).articles.entries.offset(i as isize) as *mut pdf_article;
        if !(*article).beads.is_null() {
            let art_dict = make_article(p, article, 0 as *mut *const i8, 0_u32, ptr::null_mut());
            if (*p).root.threads.is_null() {
                (*p).root.threads = pdf_new_array()
            }
            pdf_add_array(&mut *(*p).root.threads, pdf_ref_obj(art_dict));
            pdf_release_obj(art_dict);
        }
        clean_article(article);
    }
    (*p).articles.entries = mfree((*p).articles.entries as *mut libc::c_void) as *mut pdf_article;
    (*p).articles.num_entries = 0_u32;
    (*p).articles.max_entries = 0_u32;
    if !(*p).root.threads.is_null() {
        pdf_add_dict(&mut *(*p).root.dict, "Threads", pdf_ref_obj((*p).root.threads));
        pdf_release_obj((*p).root.threads);
        (*p).root.threads = ptr::null_mut()
    };
}
/* page_no = 0 for root page tree node. */
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_mediabox(mut page_no: u32, mediabox: &Rect) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if page_no == 0_u32 {
        (*p).pages.mediabox = *mediabox;
    } else {
        let page = doc_get_page_entry(p, page_no);
        (*page).cropbox = *mediabox;
        (*page).flags |= 1i32 << 0i32
    };
}
unsafe fn pdf_doc_get_mediabox(mut page_no: u32, mediabox: &mut Rect) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if page_no == 0_u32 {
        *mediabox = (*p).pages.mediabox;
    } else {
        let page = doc_get_page_entry(p, page_no);
        if (*page).flags & 1i32 << 0i32 != 0 {
            *mediabox = (*page).cropbox;
        } else {
            *mediabox = (*p).pages.mediabox;
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_current_page_resources() -> *mut pdf_obj {
    let mut p: *mut pdf_doc = &mut pdoc;
    if !(*p).pending_forms.is_null() {
        if !(*(*p).pending_forms).form.resources.is_null() {
            (*(*p).pending_forms).form.resources
        } else {
            (*(*p).pending_forms).form.resources = pdf_new_dict();
            (*(*p).pending_forms).form.resources
        }
    } else {
        let currentpage =
            &mut *(*p).pages.entries.offset((*p).pages.num_entries as isize) as *mut pdf_page;
        if !(*currentpage).resources.is_null() {
            (*currentpage).resources
        } else {
            (*currentpage).resources = pdf_new_dict();
            (*currentpage).resources
        }
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_get_dictionary(category: &str) -> *mut pdf_obj {
    let mut p: *mut pdf_doc = &mut pdoc;
    let dict = match category {
        "Names" => {
            if (*p).root.names.is_null() {
                (*p).root.names = pdf_new_dict()
            }
            (*p).root.names
        },
        "Pages" => {
            if (*p).root.pages.is_null() {
                (*p).root.pages = pdf_new_dict()
            }
            (*p).root.pages
        },
        "Catalog" => {
            if (*p).root.dict.is_null() {
                (*p).root.dict = pdf_new_dict()
            }
            (*p).root.dict
        },
        "Info" => {
            if (*p).info.is_null() {
                (*p).info = pdf_new_dict()
            }
            (*p).info
        },
        "@THISPAGE" => {
            /* Sorry for this... */
            let currentpage =
                &mut *(*p).pages.entries.offset((*p).pages.num_entries as isize) as *mut pdf_page;
            (*currentpage).page_obj
        },
        _ => { ptr::null_mut() }
    };
    if dict.is_null() {
        panic!("Document dict. \"{}\" not exist. ", category);
    }
    dict
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_current_page_number() -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    (*p).pages.num_entries.wrapping_add(1_u32) as i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_ref_page(mut page_no: u32) -> *mut pdf_obj {
    let mut p: *mut pdf_doc = &mut pdoc;
    let page = doc_get_page_entry(p, page_no);
    if (*page).page_obj.is_null() {
        (*page).page_obj = pdf_new_dict();
        (*page).page_ref = pdf_ref_obj((*page).page_obj)
    }
    pdf_link_obj((*page).page_ref)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_get_reference(category: &str) -> *mut pdf_obj {
    let page_no = pdf_doc_current_page_number();
    let ref_0 = match category {
        "@THISPAGE" => pdf_doc_ref_page(page_no as u32),
        "@PREVPAGE" => {
            if page_no <= 1i32 {
                panic!("Reference to previous page, but no pages have been completed yet.");
            }
            pdf_doc_ref_page((page_no - 1i32) as u32)
        },
        "@NEXTPAGE" => pdf_doc_ref_page((page_no + 1i32) as u32),
        _ => { ptr::null_mut() },
    };
    if ref_0.is_null() {
        panic!("Reference to \"{}\" not exist. ", category);
    }
    ref_0
}
unsafe fn pdf_doc_new_page(mut p: *mut pdf_doc) {
    if (*p).pages.num_entries >= (*p).pages.max_entries {
        doc_resize_page_entries(p, (*p).pages.max_entries.wrapping_add(128u32));
    }
    /*
     * This is confusing. pdf_doc_finish_page() have increased page count!
     */
    let currentpage = &mut *(*p).pages.entries.offset((*p).pages.num_entries as isize) as *mut pdf_page;
    /* Was this page already instantiated by a forward reference to it? */
    if (*currentpage).page_ref.is_null() {
        (*currentpage).page_obj = pdf_new_dict();
        (*currentpage).page_ref = pdf_ref_obj((*currentpage).page_obj)
    }
    (*currentpage).background = ptr::null_mut();
    (*currentpage).contents = pdf_new_stream(STREAM_COMPRESS);
    (*currentpage).resources = pdf_new_dict();
    (*currentpage).annots = ptr::null_mut();
    (*currentpage).beads = ptr::null_mut();
}
/* This only closes contents and resources. */
unsafe fn pdf_doc_finish_page(mut p: *mut pdf_doc) {
    if !(*p).pending_forms.is_null() {
        panic!("A pending form XObject at the end of page.");
    }
    let currentpage = &mut *(*p).pages.entries.offset((*p).pages.num_entries as isize) as *mut pdf_page;
    if (*currentpage).page_obj.is_null() {
        (*currentpage).page_obj = pdf_new_dict()
    }
    /*
     * Make Contents array.
     */
    /*
     * Global BOP content stream.
     * pdf_ref_obj() returns reference itself when the object is
     * indirect reference, not reference to the indirect reference.
     * We keep bop itself but not reference to it since it is
     * expected to be small.
     */
    if !(*p).pages.bop.is_null() && pdf_stream_length(&*(*p).pages.bop) > 0i32 {
        (*currentpage).content_refs[0] = pdf_ref_obj((*p).pages.bop)
    } else {
        (*currentpage).content_refs[0] = ptr::null_mut()
    }
    /*
     * Current page background content stream.
     */
    if !(*currentpage).background.is_null() {
        if pdf_stream_length(&*(*currentpage).background) > 0i32 {
            (*currentpage).content_refs[1] = pdf_ref_obj((*currentpage).background);
            pdf_add_stream(
                &mut *(*currentpage).background,
                b"\n\x00" as *const u8 as *const i8 as *const libc::c_void,
                1i32,
            );
        }
        pdf_release_obj((*currentpage).background);
        (*currentpage).background = ptr::null_mut()
    } else {
        (*currentpage).content_refs[1] = ptr::null_mut()
    }
    /* Content body of current page */
    (*currentpage).content_refs[2] = pdf_ref_obj((*currentpage).contents);
    pdf_add_stream(
        &mut *(*currentpage).contents,
        b"\n\x00" as *const u8 as *const i8 as *const libc::c_void,
        1i32,
    );
    pdf_release_obj((*currentpage).contents);
    (*currentpage).contents = ptr::null_mut();
    /*
     * Global EOP content stream.
     */
    if !(*p).pages.eop.is_null() && pdf_stream_length(&*(*p).pages.eop) > 0i32 {
        (*currentpage).content_refs[3] = pdf_ref_obj((*p).pages.eop)
    } else {
        (*currentpage).content_refs[3] = ptr::null_mut()
    }
    /*
     * Page resources.
     */
    if !(*currentpage).resources.is_null() {
        /*
         * ProcSet is obsolete in PDF-1.4 but recommended for compatibility.
         */
        let procset = pdf_new_array();
        pdf_add_array(&mut *procset, &mut *pdf_new_name("PDF"));
        pdf_add_array(&mut *procset, &mut *pdf_new_name("Text"));
        pdf_add_array(&mut *procset, &mut *pdf_new_name("ImageC"));
        pdf_add_array(&mut *procset, &mut *pdf_new_name("ImageB"));
        pdf_add_array(&mut *procset, &mut *pdf_new_name("ImageI"));
        pdf_add_dict(&mut *(*currentpage).resources, "ProcSet", procset);
        pdf_add_dict(
            &mut *(*currentpage).page_obj,
            "Resources",
            pdf_ref_obj((*currentpage).resources),
        );
        pdf_release_obj((*currentpage).resources);
        (*currentpage).resources = ptr::null_mut()
    }
    if manual_thumb_enabled != 0 {
        let thumb_filename = new(
            (strlen(thumb_basename).wrapping_add(7)).wrapping_mul(::std::mem::size_of::<i8>()) as _
        ) as *mut i8;
        sprintf(
            thumb_filename,
            b"%s.%ld\x00" as *const u8 as *const i8,
            thumb_basename,
            (*p).pages.num_entries.wrapping_rem(99999_u32) as i64 + 1,
        );
        let thumb_ref = read_thumbnail(thumb_filename);
        free(thumb_filename as *mut libc::c_void);
        if !thumb_ref.is_null() {
            pdf_add_dict(&mut *(*currentpage).page_obj, "Thumb", thumb_ref);
        }
    }
    (*p).pages.num_entries = (*p).pages.num_entries.wrapping_add(1);
}

static mut bgcolor: PdfColor = WHITE;

/* Manual thumbnail */
/* Similar to bop_content */
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_bgcolor(color: Option<&PdfColor>) {
    bgcolor = if let Some(c) = color {
        c.clone()
    } else {
        /* as clear... */
        WHITE
    };
}
unsafe fn doc_fill_page_background(p: &mut pdf_doc) {
    let mut r = Rect::zero();
    let cm = pdf_dev_get_param(2i32);
    if cm == 0 || bgcolor.is_white() {
        return;
    }
    pdf_doc_get_mediabox(pdf_doc_current_page_number() as u32, &mut r);
    let currentpage = &mut *p.pages.entries.offset(p.pages.num_entries as isize);
    if currentpage.background.is_null() {
        currentpage.background = pdf_new_stream(STREAM_COMPRESS)
    }
    let saved_content = currentpage.contents;
    currentpage.contents = currentpage.background;
    pdf_dev_gsave();
    pdf_dev_set_color(&bgcolor, 0x20, 0);
    r.fill();
    pdf_dev_grestore();
    currentpage.contents = saved_content;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_begin_page(mut scale: f64, mut x_origin: f64, mut y_origin: f64) {
    let p = &mut pdoc;
    let mut M = TMatrix {
        a: scale,
        b: 0.,
        c: 0.,
        d: scale,
        e: x_origin,
        f: y_origin,
    };
    /* pdf_doc_new_page() allocates page content stream. */
    pdf_doc_new_page(p);
    pdf_dev_bop(&mut M);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_end_page() {
    let p = &mut pdoc;
    pdf_dev_eop();
    doc_fill_page_background(p);
    pdf_doc_finish_page(p);
}

pub unsafe fn pdf_doc_add_page_content(buffer: &[u8]) {
    let p = &mut pdoc;
    if !p.pending_forms.is_null() {
        pdf_add_stream(
            &mut *(*p.pending_forms).form.contents,
            buffer.as_ptr() as *const libc::c_void,
            buffer.len() as i32,
        );
    } else {
        let currentpage =
            &mut *p.pages.entries.offset(p.pages.num_entries as isize) as *mut pdf_page;
        pdf_add_stream(
            &mut *(*currentpage).contents,
            buffer.as_ptr() as *const libc::c_void,
            buffer.len() as i32,
        );
    };
}

static mut doccreator: *mut i8 = ptr::null_mut();
/* Ugh */
#[no_mangle]
pub unsafe extern "C" fn pdf_open_document(
    mut filename: *const i8,
    mut enable_encrypt: bool,
    mut enable_object_stream: bool,
    mut media_width: f64,
    mut media_height: f64,
    mut annot_grow_amount: f64,
    mut bookmark_open_depth: i32,
    mut check_gotos: i32,
) {
    let p = &mut pdoc;
    pdf_out_init(filename, enable_encrypt, enable_object_stream);
    pdf_doc_init_catalog(p);
    p.opt.annot_grow = annot_grow_amount;
    p.opt.outline_open_depth = bookmark_open_depth;
    pdf_init_resources();
    pdf_init_colors();
    pdf_init_fonts();
    /* Thumbnail want this to be initialized... */
    pdf_init_images();
    pdf_doc_init_docinfo(p);
    if !doccreator.is_null() {
        pdf_add_dict(
            &mut *p.info,
            "Creator",
            pdf_new_string(doccreator as *const libc::c_void, strlen(doccreator) as _),
        );
        doccreator = mfree(doccreator as *mut libc::c_void) as *mut i8
    }
    pdf_doc_init_bookmarks(p, bookmark_open_depth);
    pdf_doc_init_articles(p);
    pdf_doc_init_names(p, check_gotos);
    pdf_doc_init_page_tree(p, media_width, media_height);
    pdf_doc_set_bgcolor(None);
    if enable_encrypt {
        let mut encrypt: *mut pdf_obj = pdf_encrypt_obj();
        pdf_set_encrypt(encrypt);
        pdf_release_obj(encrypt);
    }
    pdf_set_id(pdf_enc_id_array());
    /* Create a default name for thumbnail image files */
    if manual_thumb_enabled != 0 {
        let mut fn_len: size_t = strlen(filename) as _;
        if fn_len > 4i32 as u64
            && strncmp(
                b".pdf\x00" as *const u8 as *const i8,
                filename.offset(fn_len as isize).offset(-4),
                4,
            ) == 0
        {
            thumb_basename = new(
                (fn_len.wrapping_sub(4i32 as u64).wrapping_add(1i32 as u64) as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32,
            ) as *mut i8;
            strncpy(thumb_basename, filename, fn_len.wrapping_sub(4) as _);
            *thumb_basename.offset(fn_len.wrapping_sub(4i32 as u64) as isize) = 0_i8
        } else {
            thumb_basename = new((fn_len.wrapping_add(1i32 as u64) as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<i8>() as u64)
                as u32) as *mut i8;
            strcpy(thumb_basename, filename);
        }
    }
    p.pending_forms = ptr::null_mut();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_set_creator(mut creator: *const i8) {
    if creator.is_null() || *creator.offset(0) as i32 == '\u{0}' as i32 {
        return;
    }
    doccreator =
        new((strlen(creator).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy(doccreator, creator);
    /* Ugh */
}
#[no_mangle]
pub unsafe extern "C" fn pdf_close_document() {
    let mut p: *mut pdf_doc = &mut pdoc;
    /*
     * Following things were kept around so user can add dictionary items.
     */
    pdf_doc_close_articles(p); /* Should be at last. */
    pdf_doc_close_names(p);
    pdf_doc_close_bookmarks(p);
    pdf_doc_close_page_tree(p);
    pdf_doc_close_docinfo(p);
    pdf_doc_close_catalog(p);
    pdf_close_images();
    pdf_close_fonts();
    pdf_close_colors();
    pdf_close_resources();
    pdf_out_flush();
    free(thumb_basename as *mut libc::c_void);
}
/*
 * All this routine does is give the form a name and add a unity scaling matrix.
 * It fills in required fields.  The caller must initialize the stream.
 */
unsafe fn pdf_doc_make_xform(
    mut xform: *mut pdf_obj,
    bbox: &mut Rect,
    matrix: Option<&TMatrix>,
    mut resources: *mut pdf_obj,
    mut attrib: *mut pdf_obj,
) {
    let xform_dict = (*xform).as_stream_mut().get_dict_mut();
    pdf_add_dict(xform_dict, "Type", pdf_new_name("XObject"));
    pdf_add_dict(xform_dict, "Subtype", pdf_new_name("Form"));
    pdf_add_dict(xform_dict, "FormType", pdf_new_number(1.0f64));
    let tmp = pdf_new_array();
    pdf_add_array(
        &mut *tmp,
        pdf_new_number((bbox.ll.x / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *tmp,
        pdf_new_number((bbox.ll.y / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *tmp,
        pdf_new_number((bbox.ur.x / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_array(
        &mut *tmp,
        pdf_new_number((bbox.ur.y / 0.001 + 0.5).floor() * 0.001),
    );
    pdf_add_dict(xform_dict, "BBox", tmp);
    if let Some(matrix) = matrix {
        let tmp = pdf_new_array();
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.a / 0.00001f64 + 0.5f64).floor() * 0.00001f64),
        );
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.b / 0.00001f64 + 0.5f64).floor() * 0.00001f64),
        );
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.c / 0.00001f64 + 0.5f64).floor() * 0.00001f64),
        );
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.d / 0.00001f64 + 0.5f64).floor() * 0.00001f64),
        );
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.e / 0.001f64 + 0.5f64).floor() * 0.001f64),
        );
        pdf_add_array(
            &mut *tmp,
            pdf_new_number((matrix.f / 0.001f64 + 0.5f64).floor() * 0.001f64),
        );
        pdf_add_dict(xform_dict, "Matrix", tmp);
    }
    if !attrib.is_null() {
        pdf_merge_dict(xform_dict, &*attrib);
    }
    pdf_add_dict(xform_dict, "Resources", resources);
}
/*
 * begin_form_xobj creates an xobject with its "origin" at
 * xpos and ypos that is clipped to the specified bbox. Note
 * that the origin is not the lower left corner of the bbox.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_begin_grabbing(
    mut ident: *const i8,
    mut ref_x: f64,
    mut ref_y: f64,
    cropbox: &Rect,
) -> i32 {
    let mut p: *mut pdf_doc = &mut pdoc;
    let mut info = Box::new(xform_info::default());
    pdf_dev_push_gstate();
    let mut fnode = new((1_u64).wrapping_mul(::std::mem::size_of::<form_list_node>() as u64) as u32)
        as *mut form_list_node;
    (*fnode).prev = (*p).pending_forms;
    (*fnode).q_depth = pdf_dev_current_depth() as i32;
    let form = &mut (*fnode).form;
    /*
     * The reference point of an Xobject is at the lower left corner
     * of the bounding box.  Since we would like to have an arbitrary
     * reference point, we use a transformation matrix, translating
     * the reference point to (0,0).
     */
    form.matrix.a = 1.;
    form.matrix.b = 0.;
    form.matrix.c = 0.;
    form.matrix.d = 1.;
    form.matrix.e = -ref_x;
    form.matrix.f = -ref_y;
    let ref_xy = Coord::new(ref_x, ref_y);
    form.cropbox.ll = ref_xy + cropbox.ll;
    form.cropbox.ur = ref_xy + cropbox.ur;
    form.contents = pdf_new_stream(STREAM_COMPRESS);
    form.resources = pdf_new_dict();
    pdf_ximage_init_form_info(&mut info);
    info.matrix.a = 1.0f64;
    info.matrix.b = 0.0f64;
    info.matrix.c = 0.0f64;
    info.matrix.d = 1.0f64;
    info.matrix.e = -ref_x;
    info.matrix.f = -ref_y;
    info.bbox = *cropbox;
    /* Use reference since content itself isn't available yet. */
    let xobj_id = pdf_ximage_defineresource(ident, XInfo::Form(info), pdf_ref_obj((*form).contents));
    (*p).pending_forms = fnode;
    /*
     * Make sure the object is self-contained by adding the
     * current font and color to the object stream.
     */
    pdf_dev_reset_fonts(1i32); /* force color operators to be added to stream */
    pdf_dev_reset_color(1i32);
    xobj_id
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_end_grabbing(mut attrib: *mut pdf_obj) {
    let mut p: *mut pdf_doc = &mut pdoc;
    if (*p).pending_forms.is_null() {
        warn!("Tried to close a nonexistent form XOject.");
        return;
    }
    let mut fnode = (*p).pending_forms;
    let mut form = &mut (*fnode).form;
    pdf_dev_grestore_to((*fnode).q_depth as usize);
    /*
     * ProcSet is obsolete in PDF-1.4 but recommended for compatibility.
     */
    let mut procset = pdf_new_array();
    pdf_add_array(&mut *procset, pdf_new_name("PDF"));
    pdf_add_array(&mut *procset, pdf_new_name("Text"));
    pdf_add_array(&mut *procset, pdf_new_name("ImageC"));
    pdf_add_array(&mut *procset, pdf_new_name("ImageB"));
    pdf_add_array(&mut *procset, pdf_new_name("ImageI"));
    pdf_add_dict(&mut *(*form).resources, "ProcSet", procset);
    let matrix = (*form).matrix.clone();
    pdf_doc_make_xform(
        (*form).contents,
        &mut (*form).cropbox,
        Some(&matrix),
        pdf_ref_obj((*form).resources),
        attrib,
    );
    pdf_release_obj((*form).resources);
    pdf_release_obj((*form).contents);
    pdf_release_obj(attrib);
    (*p).pending_forms = (*fnode).prev;
    pdf_dev_pop_gstate();
    pdf_dev_reset_fonts(1i32);
    pdf_dev_reset_color(0i32);
    free(fnode as *mut libc::c_void);
}
static mut breaking_state: C2RustUnnamed_4 = C2RustUnnamed_4 {
        dirty: 0i32,
        broken: 0i32,
        annot_dict: ptr::null_mut(),
        rect: Rect::zero(),
    };
unsafe fn reset_box() {
    breaking_state.rect = Rect::new(
        (core::f64::INFINITY, core::f64::INFINITY),
        (core::f64::NEG_INFINITY, core::f64::NEG_INFINITY)
    );
    breaking_state.dirty = 0i32;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_begin_annot(mut dict: *mut pdf_obj) {
    breaking_state.annot_dict = dict;
    breaking_state.broken = 0i32;
    reset_box();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_end_annot() {
    pdf_doc_break_annot();
    breaking_state.annot_dict = ptr::null_mut();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_break_annot() {
    if breaking_state.dirty != 0 {
        /* Copy dict */
        let mut annot_dict = pdf_new_dict();
        pdf_merge_dict(&mut *annot_dict, &*breaking_state.annot_dict);
        pdf_doc_add_annot(
            pdf_doc_current_page_number() as u32,
            &mut breaking_state.rect,
            annot_dict,
            (breaking_state.broken == 0) as i32,
        );
        pdf_release_obj(annot_dict);
        breaking_state.broken = 1i32
    }
    reset_box();
}
/* PDF document metadata */
/* They just return PDF dictionary object.
 * Callers are completely responsible for doing right thing...
 */
/* Not really managing tree...
 * There should be something for number tree.
 */
/* Page */
/* Article thread */
/* Bookmarks */
/* Returns xobj_id of started xform. */
/* Annotation */
/* Annotation with auto- clip and line (or page) break */
#[no_mangle]
pub unsafe extern "C" fn pdf_doc_expand_box(rect: &Rect) {
    breaking_state.rect.ll.x = breaking_state.rect.ll.x.min(rect.ll.x);
    breaking_state.rect.ll.y = breaking_state.rect.ll.y.min(rect.ll.y);
    breaking_state.rect.ur.x = breaking_state.rect.ur.x.max(rect.ur.x);
    breaking_state.rect.ur.y = breaking_state.rect.ur.y.max(rect.ur.y);
    breaking_state.dirty = 1;
}
