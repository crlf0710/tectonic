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
    non_upper_case_globals
)]

use euclid::point2;

use crate::bridge::DisplayExt;
use crate::mfree;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_dpxutil::{
    ht_append_table, ht_clear_iter, ht_clear_table, ht_init_table, ht_iter_getkey, ht_iter_next,
    ht_lookup_table, ht_set_iter, ht_table_size,
};
use super::dpx_dvipdfmx::is_xdv;
use super::dpx_jpegimage::check_for_jpeg;
use super::dpx_mem::new;
use super::dpx_pdfcolor::{pdf_close_colors, pdf_color_set_verbose, pdf_init_colors, WHITE};
use super::dpx_pdfdev::{
    pdf_dev_bop, pdf_dev_eop, pdf_dev_get_coord, pdf_dev_get_param, pdf_dev_reset_color,
    pdf_dev_reset_fonts,
};
use super::dpx_pdfdraw::{
    pdf_dev_current_depth, pdf_dev_grestore, pdf_dev_grestore_to, pdf_dev_gsave,
    pdf_dev_pop_gstate, pdf_dev_push_gstate, pdf_dev_rectfill, pdf_dev_set_color,
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
use crate::bridge::{InFile, TTInputFormat};
use crate::dpx_pdfobj::{
    pdf_deref_obj, pdf_dict, pdf_file, pdf_file_get_catalog, pdf_link_obj, pdf_obj, pdf_out_flush,
    pdf_out_init, pdf_ref_obj, pdf_release_obj, pdf_remove_dict, pdf_set_encrypt, pdf_set_id,
    pdf_set_info, pdf_set_root, pdf_stream, pdf_string, DerefObj, IntoObj, PdfObjVariant, PushObj,
    STREAM_COMPRESS,
};
use libc::{free, strcmp, strcpy, strlen};

pub(crate) use super::dpx_pdfcolor::PdfColor;

use super::dpx_pdfdev::{Rect, TMatrix};
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct form_list_node {
    pub(crate) q_depth: i32,
    pub(crate) form: pdf_form,
    pub(crate) prev: *mut form_list_node,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_form {
    pub(crate) ident: *mut i8,
    pub(crate) matrix: TMatrix,
    pub(crate) cropbox: Rect,
    pub(crate) resources: *mut pdf_obj,
    pub(crate) contents: *mut pdf_obj,
}

use super::dpx_dpxutil::ht_table;
#[derive(Clone)]
pub(crate) struct pdf_article {
    pub(crate) id: String,
    pub(crate) info: *mut pdf_obj,
    pub(crate) beads: Vec<pdf_bead>,
}
#[derive(Clone)]
pub(crate) struct pdf_bead {
    pub(crate) id: String,
    pub(crate) page_no: i32,
    pub(crate) rect: Rect,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_olitem {
    pub(crate) dict: *mut pdf_obj,
    pub(crate) is_open: i32,
    pub(crate) first: *mut pdf_olitem,
    pub(crate) parent: *mut pdf_olitem,
    pub(crate) next: *mut pdf_olitem,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_page {
    pub(crate) page_obj: *mut pdf_obj,
    pub(crate) page_ref: *mut pdf_obj,
    pub(crate) flags: i32,
    pub(crate) ref_x: f64,
    pub(crate) ref_y: f64,
    pub(crate) cropbox: Rect,
    pub(crate) resources: *mut pdf_obj,
    pub(crate) background: *mut pdf_obj,
    pub(crate) contents: *mut pdf_obj,
    pub(crate) content_refs: [*mut pdf_obj; 4],
    pub(crate) annots: *mut pdf_obj,
    pub(crate) beads: *mut pdf_obj,
}
#[derive(Clone)]
pub(crate) struct pdf_doc {
    pub(crate) root: DocRoot,
    pub(crate) info: *mut pdf_obj,
    pub(crate) pages: DocPages,
    pub(crate) outlines: DocOutlines,
    pub(crate) articles: Vec<pdf_article>,
    pub(crate) names: *mut name_dict,
    pub(crate) check_gotos: i32,
    pub(crate) gotos: ht_table,
    pub(crate) opt: DocOpt,
    pub(crate) pending_forms: *mut form_list_node,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct DocOpt {
    pub(crate) outline_open_depth: i32,
    pub(crate) annot_grow: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct name_dict {
    pub(crate) category: *const i8,
    pub(crate) data: *mut ht_table,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct DocOutlines {
    pub(crate) first: *mut pdf_olitem,
    pub(crate) current: *mut pdf_olitem,
    pub(crate) current_depth: i32,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct DocPages {
    pub(crate) mediabox: Rect,
    pub(crate) bop: *mut pdf_obj,
    pub(crate) eop: *mut pdf_obj,
    pub(crate) num_entries: usize,
    pub(crate) entries: Vec<pdf_page>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct DocRoot {
    pub(crate) dict: *mut pdf_obj,
    pub(crate) viewerpref: *mut pdf_obj,
    pub(crate) pagelabels: *mut pdf_obj,
    pub(crate) pages: *mut pdf_obj,
    pub(crate) names: *mut pdf_obj,
    pub(crate) threads: *mut pdf_obj,
}
use super::dpx_dpxutil::ht_iter;

use crate::dpx_pdfximage::{load_options, xform_info};
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct BreakingState {
    pub(crate) dirty: i32,
    pub(crate) broken: i32,
    pub(crate) annot_dict: *mut pdf_obj,
    pub(crate) rect: Rect,
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
static mut thumb_basename: String = String::new();

/*pub(crate) unsafe fn pdf_doc_enable_manual_thumbnails() {
    manual_thumb_enabled = 1_i8;
    // without HAVE_LIBPNG:
    // warn!("Manual thumbnail is not supported without the libpng library.");
}*/
unsafe fn read_thumbnail(thumb_filename: &str) -> *mut pdf_obj {
    let options: load_options = load_options {
        page_no: 1i32,
        bbox_type: 0i32,
        dict: ptr::null_mut(),
    };
    let handle = InFile::open(thumb_filename, TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        warn!("Could not open thumbnail file \"{}\"", thumb_filename);
        return ptr::null_mut();
    }
    let mut handle = handle.unwrap();
    if check_for_png(&mut handle) == 0 && check_for_jpeg(&mut handle) == 0 {
        warn!("Thumbnail \"{}\" not a png/jpeg file!", thumb_filename);
        return ptr::null_mut();
    }
    let xobj_id = pdf_ximage_findresource(thumb_filename, options);
    if xobj_id < 0 {
        warn!("Could not read thumbnail file \"{}\".", thumb_filename);
        ptr::null_mut()
    } else {
        pdf_ximage_get_reference(xobj_id)
    }
}

pub(crate) unsafe fn pdf_doc_set_verbose(level: i32) {
    verbose = level;
    pdf_font_set_verbose(level);
    pdf_color_set_verbose(level);
    pdf_ximage_set_verbose(level);
}
static mut pdoc: pdf_doc = pdf_doc {
    root: DocRoot {
        dict: ptr::null_mut(),
        viewerpref: ptr::null_mut(),
        pagelabels: ptr::null_mut(),
        pages: ptr::null_mut(),
        names: ptr::null_mut(),
        threads: ptr::null_mut(),
    },
    info: ptr::null_mut(),
    pages: DocPages {
        mediabox: Rect::new(point2(0., 0.), point2(0., 0.)),
        bop: ptr::null_mut(),
        eop: ptr::null_mut(),
        num_entries: 0,
        entries: Vec::new(),
    },
    outlines: DocOutlines {
        first: ptr::null_mut(),
        current: ptr::null_mut(),
        current_depth: 0,
    },
    articles: Vec::new(),
    names: ptr::null_mut(),
    check_gotos: 0,
    gotos: ht_table {
        count: 0,
        hval_free_fn: None,
        table: [ptr::null_mut(); 503],
    },
    opt: DocOpt {
        outline_open_depth: 0,
        annot_grow: 0.,
    },
    pending_forms: ptr::null_mut(),
};
unsafe fn pdf_doc_init_catalog(p: &mut pdf_doc) {
    p.root.viewerpref = ptr::null_mut();
    p.root.pagelabels = ptr::null_mut();
    p.root.pages = ptr::null_mut();
    p.root.names = ptr::null_mut();
    p.root.threads = ptr::null_mut();
    p.root.dict = pdf_dict::new().into_obj();
    pdf_set_root(p.root.dict);
}
unsafe fn pdf_doc_close_catalog(p: &mut pdf_doc) {
    if !p.root.viewerpref.is_null() {
        if let Some(tmp) = (*p.root.dict).as_dict().get("ViewerPreferences") {
            if let PdfObjVariant::DICT(tmp) = &tmp.data {
                (*p.root.viewerpref).as_dict_mut().merge(&tmp);
                (*p.root.dict)
                    .as_dict_mut()
                    .set("ViewerPreferences", pdf_ref_obj(p.root.viewerpref));
            } else {
                /* What should I do? */
                warn!("Could not modify ViewerPreferences.");
                /* Maybe reference */
            }
        } else {
            (*p.root.dict)
                .as_dict_mut()
                .set("ViewerPreferences", pdf_ref_obj(p.root.viewerpref));
        }
        pdf_release_obj(p.root.viewerpref);
        p.root.viewerpref = ptr::null_mut()
    }
    if !p.root.pagelabels.is_null() {
        let tmp = (*p.root.dict).as_dict_mut().get_mut("PageLabels");
        if tmp.is_none() {
            let mut tmp = pdf_dict::new();
            tmp.set("Nums", pdf_link_obj(p.root.pagelabels));
            let tmp = tmp.into_obj();
            (*p.root.dict)
                .as_dict_mut()
                .set("PageLabels", pdf_ref_obj(tmp));
            pdf_release_obj(tmp);
        } else {
            /* What should I do? */
            warn!("Could not modify PageLabels.");
        }
        pdf_release_obj(p.root.pagelabels);
        p.root.pagelabels = ptr::null_mut()
    }
    (*p.root.dict).as_dict_mut().set("Type", "Catalog");
    pdf_release_obj(p.root.dict);
    p.root.dict = ptr::null_mut();
}
/*
 * Pages are starting at 1.
 * The page count does not increase until the page is finished.
 */
unsafe fn doc_resize_page_entries(p: &mut pdf_doc, size: usize) {
    p.pages.entries.resize_with(size, || pdf_page {
        ref_x: 0.,
        ref_y: 0.,
        cropbox: Rect::zero(),
        page_obj: ptr::null_mut(),
        page_ref: ptr::null_mut(),
        flags: 0,
        resources: ptr::null_mut(),
        background: ptr::null_mut(),
        contents: ptr::null_mut(),
        content_refs: [ptr::null_mut(); 4],
        annots: ptr::null_mut(),
        beads: ptr::null_mut(),
    });
}
unsafe fn doc_get_page_entry<'a>(p: &'a mut pdf_doc, page_no: usize) -> &'a mut pdf_page {
    assert!(page_no < 65536, "Page number {} too large!", page_no);
    assert!(page_no != 0, "Invalid Page number {}.", page_no);
    if page_no > p.pages.entries.len() as _ {
        doc_resize_page_entries(p, page_no);
    }
    &mut p.pages.entries[page_no - 1]
}

pub(crate) unsafe fn pdf_doc_set_bop_content(content: &[u8]) {
    let mut p = &mut pdoc;
    if !p.pages.bop.is_null() {
        pdf_release_obj(p.pages.bop);
        p.pages.bop = ptr::null_mut()
    }
    if !content.is_empty() {
        p.pages.bop = pdf_stream::new(STREAM_COMPRESS).into_obj();
        (*p.pages.bop).as_stream_mut().add_slice(content);
    } else {
        p.pages.bop = ptr::null_mut()
    };
}

pub(crate) unsafe fn pdf_doc_set_eop_content(content: &[u8]) {
    let mut p = &mut pdoc;
    if !p.pages.eop.is_null() {
        pdf_release_obj(p.pages.eop);
        p.pages.eop = ptr::null_mut()
    }
    if !content.is_empty() {
        p.pages.eop = pdf_stream::new(STREAM_COMPRESS).into_obj();
        (*p.pages.eop).as_stream_mut().add_slice(content);
    } else {
        p.pages.eop = ptr::null_mut()
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

unsafe fn pdf_doc_init_docinfo(p: &mut pdf_doc) {
    p.info = pdf_dict::new().into_obj();
    pdf_set_info(p.info);
}
unsafe fn pdf_doc_close_docinfo(p: &mut pdf_doc) {
    let docinfo = &mut *p.info;
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
        if let Some(value) = docinfo.as_dict().get(*key) {
            if let PdfObjVariant::STRING(value) = &value.data {
                if value.len() == 0 {
                    /* The hyperref package often uses emtpy strings. */
                    pdf_remove_dict(docinfo.as_dict_mut(), key);
                }
            } else {
                warn!("\"{}\" in DocInfo dictionary not string type.", key);
                pdf_remove_dict(docinfo.as_dict_mut(), key);
                warn!("\"{}\" removed from DocInfo.", key);
            }
        }
    }
    if !docinfo.as_dict().has("Producer") {
        let banner = b"xdvipdfmx (0.1)";
        docinfo
            .as_dict_mut()
            .set("Producer", pdf_string::new(banner));
    }
    if !docinfo.as_dict().has("CreationDate") {
        let now = asn_date();

        docinfo
            .as_dict_mut()
            .set("CreationDate", pdf_string::new(now));
    }
    pdf_release_obj(docinfo);
    p.info = ptr::null_mut();
}
unsafe fn pdf_doc_get_page_resources(p: *mut pdf_doc, category: &str) -> *mut pdf_obj {
    if p.is_null() {
        return ptr::null_mut();
    }
    let res_dict = if !(*p).pending_forms.is_null() {
        if !(*(*p).pending_forms).form.resources.is_null() {
            (*(*p).pending_forms).form.resources
        } else {
            (*(*p).pending_forms).form.resources = pdf_dict::new().into_obj();
            (*(*p).pending_forms).form.resources
        }
    } else {
        let currentpage = &mut (*p).pages.entries[(*p).pages.num_entries] as *mut pdf_page;
        if !(*currentpage).resources.is_null() {
            (*currentpage).resources
        } else {
            (*currentpage).resources = pdf_dict::new().into_obj();
            (*currentpage).resources
        }
    };
    (*res_dict)
        .as_dict_mut()
        .get_mut(category)
        .unwrap_or_else(|| {
            let resources = pdf_dict::new().into_obj();
            (*res_dict).as_dict_mut().set(category, resources);
            &mut *resources
        })
}

pub(crate) unsafe fn pdf_doc_add_page_resource(
    category: &str,
    resource_name: &[u8],
    mut resource_ref: *mut pdf_obj,
) {
    let p = &mut pdoc;
    if !(!resource_ref.is_null() && (*resource_ref).is_indirect()) {
        warn!("Passed non indirect reference...");
        resource_ref = pdf_ref_obj(resource_ref)
        /* leak */
    }
    let resources = pdf_doc_get_page_resources(p, category);
    if let Some(duplicate) = (*resources).as_dict_mut().get_mut(resource_name) {
        if (*duplicate)
            .as_indirect()
            .compare((*resource_ref).as_indirect())
        {
            warn!(
                "Conflicting page resource found (page: {}, category: {}, name: {}).",
                pdf_doc_current_page_number(),
                category,
                resource_name.display(),
            );
            warn!("Ignoring...");
            pdf_release_obj(resource_ref);
        } else {
            (*resources).as_dict_mut().set(resource_name, resource_ref);
        }
    } else {
        (*resources).as_dict_mut().set(resource_name, resource_ref);
    }
}
unsafe fn doc_flush_page(p: *mut pdf_doc, page: &mut pdf_page, parent_ref: *mut pdf_obj) {
    (*page.page_obj).as_dict_mut().set("Type", "Page");
    (*page.page_obj).as_dict_mut().set("Parent", parent_ref);
    /*
     * Clipping area specified by CropBox is affected by MediaBox which
     * might be inherit from parent node. If MediaBox of the root node
     * does not have enough size to cover all page's imaging area, using
     * CropBox here gives incorrect result.
     */
    if page.flags & 1i32 << 0i32 != 0 {
        let mut mediabox = vec![];
        mediabox.push_obj((page.cropbox.min.x / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((page.cropbox.min.y / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((page.cropbox.max.x / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((page.cropbox.max.y / 0.01 + 0.5).floor() * 0.01);
        (*page.page_obj).as_dict_mut().set("MediaBox", mediabox);
    }
    let mut count = 0_u32;
    let mut contents_array = vec![];
    if !page.content_refs[0].is_null() {
        /* global bop */
        contents_array.push(page.content_refs[0]);
        count = count.wrapping_add(1)
    } else if !(*p).pages.bop.is_null() && (*(*p).pages.bop).as_stream().len() > 0 {
        contents_array.push(pdf_ref_obj((*p).pages.bop));
        count = count.wrapping_add(1)
    }
    if !page.content_refs[1].is_null() {
        /* background */
        contents_array.push(page.content_refs[1]);
        count = count.wrapping_add(1)
    }
    if !page.content_refs[2].is_null() {
        /* page body */
        contents_array.push(page.content_refs[2]);
        count = count.wrapping_add(1)
    }
    if !page.content_refs[3].is_null() {
        /* global eop */
        contents_array.push(page.content_refs[3]);
        count = count.wrapping_add(1)
    } else if !(*p).pages.eop.is_null() && (*(*p).pages.eop).as_stream().len() > 0 {
        contents_array.push(pdf_ref_obj((*p).pages.eop));
        count = count.wrapping_add(1)
    }
    if count == 0_u32 {
        warn!("Page with empty content found!!!");
    }
    page.content_refs[0] = ptr::null_mut();
    page.content_refs[1] = ptr::null_mut();
    page.content_refs[2] = ptr::null_mut();
    page.content_refs[3] = ptr::null_mut();
    (*page.page_obj)
        .as_dict_mut()
        .set("Contents", contents_array);
    if !page.annots.is_null() {
        (*page.page_obj)
            .as_dict_mut()
            .set("Annots", pdf_ref_obj(page.annots));
        pdf_release_obj(page.annots);
    }
    if !page.beads.is_null() {
        (*page.page_obj)
            .as_dict_mut()
            .set("B", pdf_ref_obj(page.beads));
        pdf_release_obj(page.beads);
    }
    pdf_release_obj(page.page_obj);
    pdf_release_obj(page.page_ref);
    page.page_obj = ptr::null_mut();
    page.page_ref = ptr::null_mut();
    page.annots = ptr::null_mut();
    page.beads = ptr::null_mut();
}
unsafe fn build_page_tree(
    p: *mut pdf_doc,
    firstpage: *mut pdf_page,
    num_pages: i32,
    parent_ref: *mut pdf_obj,
) -> *mut pdf_obj {
    let self_0 = pdf_dict::new().into_obj();
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
    (*self_0).as_dict_mut().set("Type", "Pages");
    (*self_0).as_dict_mut().set("Count", num_pages as f64);
    if !parent_ref.is_null() {
        (*self_0).as_dict_mut().set("Parent", parent_ref);
    }
    let mut kids = vec![];
    if num_pages > 0i32 && num_pages <= 4i32 {
        for i in 0..num_pages {
            let page = &mut *firstpage.offset(i as isize);
            if page.page_ref.is_null() {
                page.page_ref = pdf_ref_obj(page.page_obj)
            }
            kids.push(pdf_link_obj(page.page_ref));
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
                kids.push(pdf_ref_obj(subtree));
                pdf_release_obj(subtree);
            } else {
                let page_0 = &mut *firstpage.offset(start as isize);
                if page_0.page_ref.is_null() {
                    page_0.page_ref = pdf_ref_obj(page_0.page_obj)
                }
                kids.push(pdf_link_obj(page_0.page_ref));
                doc_flush_page(p, page_0, pdf_link_obj(self_ref));
            }
        }
    }
    (*self_0).as_dict_mut().set("Kids", kids);
    pdf_release_obj(self_ref);
    self_0
}
unsafe fn pdf_doc_init_page_tree(p: &mut pdf_doc, media_width: f64, media_height: f64) {
    /*
     * Create empty page tree.
     * The docroot.pages is kept open until the document is closed.
     * This allows the user to write to pages if he so choses.
     */
    p.root.pages = pdf_dict::new().into_obj();
    p.pages.num_entries = 0;
    p.pages.entries = Vec::new();
    p.pages.bop = ptr::null_mut();
    p.pages.eop = ptr::null_mut();
    p.pages.mediabox = Rect::new(point2(0., 0.), point2(media_width, media_height));
}
unsafe fn pdf_doc_close_page_tree(p: &mut pdf_doc) {
    /*
     * Do consistency check on forward references to pages.
     */
    for page_no in (p.pages.num_entries + 1)..=p.pages.entries.len() {
        let page = doc_get_page_entry(&mut *p, page_no);
        if !page.page_obj.is_null() {
            warn!("Nonexistent page #{} refered.", page_no);
            pdf_release_obj(page.page_ref);
            page.page_ref = ptr::null_mut()
        }
        if !page.page_obj.is_null() {
            warn!("Entry for a nonexistent page #{} created.", page_no);
            pdf_release_obj(page.page_obj);
            page.page_obj = ptr::null_mut()
        }
        if !page.annots.is_null() {
            warn!("Annotation attached to a nonexistent page #{}.", page_no);
            pdf_release_obj(page.annots);
            page.annots = ptr::null_mut()
        }
        if !page.beads.is_null() {
            warn!("Article beads attached to a nonexistent page #{}.", page_no);
            pdf_release_obj(page.beads);
            page.beads = ptr::null_mut()
        }
        if !page.resources.is_null() {
            pdf_release_obj(page.resources);
            page.resources = ptr::null_mut()
        }
    }
    /*
     * Connect page tree to root node.
     */
    let page_tree_root = build_page_tree(
        p,
        &mut p.pages.entries[0],
        p.pages.num_entries as i32,
        ptr::null_mut(),
    );
    (*p.root.pages)
        .as_dict_mut()
        .merge((*page_tree_root).as_dict());
    pdf_release_obj(page_tree_root);
    /* They must be after build_page_tree() */
    if !p.pages.bop.is_null() {
        (*p.pages.bop).as_stream_mut().add_str("\n");
        pdf_release_obj(p.pages.bop);
        p.pages.bop = ptr::null_mut()
    }
    if !p.pages.eop.is_null() {
        (*p.pages.eop).as_stream_mut().add_str("\n");
        pdf_release_obj(p.pages.eop);
        p.pages.eop = ptr::null_mut()
    }
    /* Create media box at root node and let the other pages inherit it. */
    let mut mediabox = vec![];
    mediabox.push_obj((p.pages.mediabox.min.x / 0.01 + 0.5).floor() * 0.01);
    mediabox.push_obj((p.pages.mediabox.min.y / 0.01 + 0.5).floor() * 0.01);
    mediabox.push_obj((p.pages.mediabox.max.x / 0.01 + 0.5).floor() * 0.01);
    mediabox.push_obj((p.pages.mediabox.max.y / 0.01 + 0.5).floor() * 0.01);
    (*p.root.pages).as_dict_mut().set("MediaBox", mediabox);
    (*p.root.dict)
        .as_dict_mut()
        .set("Pages", pdf_ref_obj(p.root.pages));
    pdf_release_obj(p.root.pages);
    p.root.pages = ptr::null_mut();
    p.pages.entries = Vec::new();
    p.pages.num_entries = 0;
}

pub unsafe fn pdf_doc_get_page_count(pf: &pdf_file) -> i32 {
    let catalog = pdf_file_get_catalog(pf);
    if let Some(mut page_tree) = DerefObj::new((*catalog).as_dict_mut().get_mut("Pages")) {
        if let PdfObjVariant::DICT(page_tree) = &mut page_tree.data {
            if let Some(tmp) = DerefObj::new(page_tree.get_mut("Count")) {
                if let PdfObjVariant::NUMBER(count) = tmp.data {
                    count as i32
                } else {
                    0
                }
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    }
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

pub unsafe fn pdf_doc_get_page(
    pf: &pdf_file,
    page_no: i32,
    options: i32,
    resources_p: *mut *mut pdf_obj,
) -> Option<(DerefObj, Rect, TMatrix)>
/* returned values */ {
    let catalog = pdf_file_get_catalog(pf);
    if let Some(mut page_tree) = DerefObj::new((*catalog).as_dict_mut().get_mut("Pages")) {
        if let PdfObjVariant::DICT(_) = page_tree.data {
            let mut resources: *mut pdf_obj = ptr::null_mut();
            let mut rotate: *mut pdf_obj = ptr::null_mut();
            let count = {
                if let Some(tmp) = DerefObj::new(page_tree.as_dict_mut().get_mut("Count")) {
                    if let PdfObjVariant::NUMBER(count) = tmp.data {
                        count as i32
                    } else {
                        return error(rotate, resources);
                    }
                } else {
                    return error(rotate, resources);
                }
            };
            if page_no <= 0i32 || page_no > count {
                warn!("Page {} does not exist.", page_no);
                return error_silent(rotate, resources);
            }

            /*
             * Seek correct page. Get MediaBox, CropBox and Resources.
             * (Note that these entries can be inherited.)
             */
            let mut art_box: *mut pdf_obj = ptr::null_mut();
            let mut trim_box: *mut pdf_obj = ptr::null_mut();
            let mut bleed_box: *mut pdf_obj = ptr::null_mut();
            let mut media_box: *mut pdf_obj = ptr::null_mut();
            let mut crop_box: *mut pdf_obj = ptr::null_mut();
            let mut depth: i32 = 30;
            let mut page_idx: i32 = page_no - 1;
            let mut kids_length = 1;
            let mut i = 0;
            loop {
                depth -= 1;
                if !(depth != 0 && i != kids_length) {
                    break;
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("MediaBox")).as_mut()
                {
                    pdf_release_obj(media_box);
                    media_box = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("CropBox")).as_mut()
                {
                    pdf_release_obj(crop_box);
                    crop_box = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("ArtBox")).as_mut()
                {
                    pdf_release_obj(art_box);
                    art_box = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("TrimBox")).as_mut()
                {
                    pdf_release_obj(trim_box);
                    trim_box = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("BleedBox")).as_mut()
                {
                    pdf_release_obj(bleed_box);
                    bleed_box = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("Rotate")).as_mut()
                {
                    pdf_release_obj(rotate);
                    rotate = tmp_0
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("Resources")).as_mut()
                {
                    pdf_release_obj(resources);
                    resources = tmp_0
                }
                if let Some(mut kids) = DerefObj::new(page_tree.as_dict_mut().get_mut("Kids")) {
                    if let PdfObjVariant::ARRAY(array) = &mut kids.data {
                        i = 0;
                        kids_length = array.len();
                        while i < kids_length {
                            let count_0;

                            let pt = if i < array.len() {
                                DerefObj::new(Some(&mut *array[i]))
                            } else {
                                None
                            };
                            if let Some(mut pt) = pt {
                                if let PdfObjVariant::DICT(ptdata) = &mut pt.data {
                                    if let Some(tmp_0) = DerefObj::new(ptdata.get_mut("Count")) {
                                        if let PdfObjVariant::NUMBER(v) = tmp_0.data {
                                            /* Pages object */
                                            count_0 = v as i32;
                                        } else {
                                            return error(rotate, resources);
                                        }
                                    } else {
                                        /* Page object */
                                        count_0 = 1;
                                    }
                                    page_tree = pt;
                                    if page_idx < count_0 {
                                        break;
                                    }
                                    page_idx -= count_0;
                                } else {
                                    return error(rotate, resources);
                                }
                            } else {
                                return error(rotate, resources);
                            }
                            i += 1;
                        }
                    } else {
                        return error(rotate, resources);
                    }
                } else {
                    break;
                }
            }

            if depth == 0 || kids_length == i {
                pdf_release_obj(media_box);
                pdf_release_obj(crop_box);
                return error(rotate, resources);
            }

            /* Nasty BBox selection... */
            let box_0 = match options {
                0 | 1 => crop_box
                    .as_mut()
                    .or_else(|| media_box.as_mut())
                    .or_else(|| bleed_box.as_mut())
                    .or_else(|| trim_box.as_mut())
                    .or_else(|| art_box.as_mut()),
                2 => media_box
                    .as_mut()
                    .or_else(|| crop_box.as_mut())
                    .or_else(|| bleed_box.as_mut())
                    .or_else(|| trim_box.as_mut())
                    .or_else(|| art_box.as_mut()),
                3 => art_box
                    .as_mut()
                    .or_else(|| crop_box.as_mut())
                    .or_else(|| media_box.as_mut())
                    .or_else(|| bleed_box.as_mut())
                    .or_else(|| trim_box.as_mut()),
                4 => trim_box
                    .as_mut()
                    .or_else(|| crop_box.as_mut())
                    .or_else(|| media_box.as_mut())
                    .or_else(|| bleed_box.as_mut())
                    .or_else(|| art_box.as_mut()),
                5 => bleed_box
                    .as_mut()
                    .or_else(|| crop_box.as_mut())
                    .or_else(|| media_box.as_mut())
                    .or_else(|| trim_box.as_mut())
                    .or_else(|| art_box.as_mut()),
                _ => None,
            };
            let box_0 = if let Some(box_0) = box_0 {
                box_0 as *mut pdf_obj
            } else {
                ptr::null_mut()
            };
            let medbox = media_box;

            if !((!box_0.is_null() && matches!((*box_0).data, PdfObjVariant::ARRAY(_)))
                && (*box_0).as_array().len() == 4
                && (!resources.is_null() && matches!((*resources).data, PdfObjVariant::DICT(_))))
            {
                pdf_release_obj(box_0);
                return error(rotate, resources);
            }

            let mut bbox = Rect::zero();
            let mut i_0 = 4;
            loop {
                if i_0 == 0 {
                    break;
                }
                i_0 -= 1;

                let array = (*box_0).as_array_mut();
                if let Some(tmp_1) = if i_0 < array.len() {
                    DerefObj::new(Some(&mut *array[i_0]))
                } else {
                    None
                } {
                    match tmp_1.data {
                        PdfObjVariant::NUMBER(x) => match i_0 {
                            0 => bbox.min.x = x,
                            1 => bbox.min.y = x,
                            2 => bbox.max.x = x,
                            3 => bbox.max.y = x,
                            _ => {}
                        },
                        _ => {
                            pdf_release_obj(box_0);
                            return error(rotate, resources);
                        }
                    }
                } else {
                    pdf_release_obj(box_0);
                    return error(rotate, resources);
                }
            }

            if !medbox.is_null() && (is_xdv != 0 || options != 0) {
                let mut i_0 = 4;
                loop {
                    if i_0 == 0 {
                        break;
                    }
                    i_0 -= 1;

                    let array = (*medbox).as_array_mut();
                    if let Some(tmp_2) = if i_0 < array.len() {
                        DerefObj::new(Some(&mut *array[i_0]))
                    } else {
                        None
                    } {
                        match tmp_2.data {
                            PdfObjVariant::NUMBER(x) => match i_0 {
                                0 => {
                                    if bbox.min.x < x {
                                        bbox.min.x = x
                                    }
                                }
                                1 => {
                                    if bbox.min.y < x {
                                        bbox.min.y = x
                                    }
                                }
                                2 => {
                                    if bbox.max.x > x {
                                        bbox.max.x = x
                                    }
                                }
                                3 => {
                                    if bbox.max.y > x {
                                        bbox.max.y = x
                                    }
                                }
                                _ => {}
                            },
                            _ => {
                                pdf_release_obj(box_0);
                                return error(rotate, resources);
                            }
                        }
                    } else {
                        pdf_release_obj(box_0);
                        return error(rotate, resources);
                    }
                }
            }

            pdf_release_obj(box_0);

            let mut matrix = TMatrix::identity();
            if !rotate.is_null() && matches!((*rotate).data, PdfObjVariant::NUMBER(_)) {
                let deg: f64 = (*rotate).as_f64();
                if deg - deg as i32 as f64 != 0.0f64 {
                    warn!("Invalid value specified for /Rotate: {}", deg);
                } else if deg != 0.0f64 {
                    let mut rot: i32 = deg as i32;
                    if (rot % 90i32) as f64 != 0.0f64 {
                        warn!("Invalid value specified for /Rotate: {}", deg);
                    } else {
                        rot = rot % 360i32;
                        if rot < 0i32 {
                            rot += 360i32
                        }
                        match rot {
                            90 => {
                                matrix = TMatrix::row_major(
                                    0.,
                                    -1.,
                                    1.,
                                    0.,
                                    bbox.min.x - bbox.min.y,
                                    bbox.min.y + bbox.max.x,
                                );
                            }
                            180 => {
                                matrix = TMatrix::row_major(
                                    -1.,
                                    0.,
                                    0.,
                                    -1.,
                                    bbox.min.x + bbox.max.x,
                                    bbox.min.y + bbox.max.y,
                                );
                            }
                            270 => {
                                matrix = TMatrix::row_major(
                                    0.,
                                    1.,
                                    -1.,
                                    0.,
                                    bbox.min.x + bbox.max.y,
                                    bbox.min.y - bbox.min.x,
                                );
                            }
                            _ => {}
                        }
                    }
                }
                pdf_release_obj(rotate);
            } else if !rotate.is_null() {
                return error(rotate, resources);
            }

            if !resources_p.is_null() {
                *resources_p = resources;
            } else {
                pdf_release_obj(resources);
            }
            return Some((page_tree, bbox, matrix));

            unsafe fn error(
                rotate: *mut pdf_obj,
                resources: *mut pdf_obj,
            ) -> Option<(DerefObj, Rect, TMatrix)> {
                warn!("Cannot parse document. Broken PDF file?");
                error_silent(rotate, resources)
            }

            unsafe fn error_silent(
                rotate: *mut pdf_obj,
                resources: *mut pdf_obj,
            ) -> Option<(DerefObj, Rect, TMatrix)> {
                pdf_release_obj(rotate);
                pdf_release_obj(resources);
                None
            }
        } else {
            warn!("Cannot parse document. Broken PDF file?");
            return None;
        }
    } else {
        warn!("Cannot parse document. Broken PDF file?");
        return None;
    }
}

unsafe fn pdf_doc_init_bookmarks(p: &mut pdf_doc, bm_open_depth: i32) {
    p.opt.outline_open_depth = (if bm_open_depth >= 0i32 {
        bm_open_depth as u32
    } else {
        256u32.wrapping_sub(bm_open_depth as u32)
    }) as i32;
    p.outlines.current_depth = 1i32;
    let item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
        as *mut pdf_olitem;
    (*item).dict = ptr::null_mut();
    (*item).next = ptr::null_mut();
    (*item).first = ptr::null_mut();
    (*item).parent = ptr::null_mut();
    (*item).is_open = 1i32;
    p.outlines.current = item;
    p.outlines.first = item;
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
    node: &mut pdf_olitem,
    parent_ref: *mut pdf_obj,
    parent_dict: &mut pdf_obj,
) -> i32 {
    assert!(!node.dict.is_null());
    let mut this_ref = pdf_ref_obj(node.dict);
    parent_dict
        .as_dict_mut()
        .set("First", pdf_link_obj(this_ref));
    let mut retval = 0;
    let mut item = node as *mut pdf_olitem;
    let mut prev_ref = ptr::null_mut::<pdf_obj>();
    while !item.is_null() && !(*item).dict.is_null() {
        if !(*item).first.is_null() && !(*(*item).first).dict.is_null() {
            let count = flush_bookmarks(&mut *(*item).first, this_ref, &mut *(*item).dict);
            if (*item).is_open != 0 {
                (*(*item).dict).as_dict_mut().set("Count", count as f64);
                retval += count
            } else {
                (*(*item).dict).as_dict_mut().set("Count", -count as f64);
            }
        }
        (*(*item).dict)
            .as_dict_mut()
            .set("Parent", pdf_link_obj(parent_ref));
        if !prev_ref.is_null() {
            (*(*item).dict).as_dict_mut().set("Prev", prev_ref);
        }
        let next_ref;
        if !(*item).next.is_null() && !(*(*item).next).dict.is_null() {
            next_ref = pdf_ref_obj((*(*item).next).dict);
            (*(*item).dict)
                .as_dict_mut()
                .set("Next", pdf_link_obj(next_ref));
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
    parent_dict
        .as_dict_mut()
        .set("Last", pdf_link_obj(prev_ref));
    pdf_release_obj(prev_ref);
    pdf_release_obj(node.dict);
    node.dict = ptr::null_mut();
    retval
}

pub(crate) unsafe fn pdf_doc_bookmarks_up() -> i32 {
    let mut p = &mut pdoc;
    let item = p.outlines.current;
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
    p.outlines.current = item;
    p.outlines.current_depth -= 1;
    0i32
}

pub(crate) unsafe fn pdf_doc_bookmarks_down() -> i32 {
    let mut p = &mut pdoc;
    let item = p.outlines.current;
    if (*item).dict.is_null() {
        warn!("Empty bookmark node!");
        warn!("You have tried to jump more than 1 level.");
        (*item).dict = pdf_dict::new().into_obj();
        (*(*item).dict)
            .as_dict_mut()
            .set("Title", pdf_string::new(b"<No Title>"));
        let mut tcolor = vec![];
        tcolor.push_obj(1_f64);
        tcolor.push_obj(0_f64);
        tcolor.push_obj(0_f64);
        let tcolor = tcolor.into_obj();
        (*(*item).dict).as_dict_mut().set("C", pdf_link_obj(tcolor));
        pdf_release_obj(tcolor);
        (*(*item).dict).as_dict_mut().set("F", 1_f64);
        let mut action = pdf_dict::new();
        action.set("S", "JavaScript");
        action.set(
            "JS",
            pdf_string::new(
                &b"app.alert(\"The author of this document made this bookmark item empty!\", 3, 0)"
                    [..],
            ),
        );
        let action = action.into_obj();
        (*(*item).dict).as_dict_mut().set("A", pdf_link_obj(action));
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
    p.outlines.current = first;
    p.outlines.current_depth += 1;
    0i32
}

pub(crate) unsafe fn pdf_doc_bookmarks_depth() -> i32 {
    let p = &pdoc;
    p.outlines.current_depth
}

pub(crate) unsafe fn pdf_doc_bookmarks_add(dict: &mut pdf_obj, is_open: i32) {
    let mut p = &mut pdoc;
    let mut item = p.outlines.current;
    if item.is_null() {
        item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
            as *mut pdf_olitem;
        (*item).parent = ptr::null_mut();
        p.outlines.first = item
    } else if !(*item).dict.is_null() {
        /* go to next item */
        item = (*item).next
    }
    (*item).dict = dict;
    (*item).first = ptr::null_mut();
    (*item).is_open = if is_open < 0 {
        if p.outlines.current_depth > p.opt.outline_open_depth {
            0
        } else {
            1
        }
    } else {
        is_open
    };
    let next = &mut *(new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
        as *mut pdf_olitem);
    (*item).next = next;
    next.dict = ptr::null_mut();
    next.parent = (*item).parent;
    next.first = ptr::null_mut();
    next.is_open = -1i32;
    next.next = ptr::null_mut();
    p.outlines.current = item;
    pdf_doc_add_goto((*dict).as_dict_mut());
}
unsafe fn pdf_doc_close_bookmarks(p: &mut pdf_doc) {
    let catalog: *mut pdf_obj = p.root.dict;
    let item = &mut *p.outlines.first;
    if !(*item).dict.is_null() {
        let bm_root = pdf_dict::new().into_obj();
        let bm_root_ref = pdf_ref_obj(bm_root);
        let count = flush_bookmarks(item, bm_root_ref, &mut *bm_root);
        (*bm_root).as_dict_mut().set("Count", count as f64);
        (*catalog).as_dict_mut().set("Outlines", bm_root_ref);
        pdf_release_obj(bm_root);
    }
    clean_bookmarks(item);
    p.outlines.first = ptr::null_mut();
    p.outlines.current = ptr::null_mut();
    p.outlines.current_depth = 0i32;
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
unsafe fn pdf_doc_init_names(p: &mut pdf_doc, check_gotos: i32) {
    p.root.names = ptr::null_mut();
    p.names = new(((::std::mem::size_of::<[*const i8; 10]>() as u64)
        .wrapping_div(::std::mem::size_of::<*const i8>() as u64)
        .wrapping_add(1) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<name_dict>() as u64) as u32)
        as *mut name_dict;
    for i in 0..(::std::mem::size_of::<[*const i8; 10]>() as u64)
        .wrapping_div(::std::mem::size_of::<*const i8>() as u64)
    {
        (*p.names.offset(i as isize)).category = name_dict_categories[i as usize];
        (*p.names.offset(i as isize)).data = if strcmp(
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
    (*p.names.offset(
        (::std::mem::size_of::<[*const i8; 10]>() as u64)
            .wrapping_div(::std::mem::size_of::<*const i8>() as u64) as isize,
    ))
    .category = ptr::null();
    (*p.names.offset(
        (::std::mem::size_of::<[*const i8; 10]>() as u64)
            .wrapping_div(::std::mem::size_of::<*const i8>() as u64) as isize,
    ))
    .data = ptr::null_mut();
    p.check_gotos = check_gotos;
    ht_init_table(
        &mut p.gotos,
        ::std::mem::transmute::<
            Option<unsafe fn(_: *mut pdf_obj) -> ()>,
            Option<unsafe fn(_: *mut libc::c_void) -> ()>,
        >(Some(pdf_release_obj as unsafe fn(_: *mut pdf_obj) -> ())),
    );
}

pub(crate) unsafe fn pdf_doc_add_names(category: &[u8], key: &[u8], value: *mut pdf_obj) -> i32 {
    let p = &mut pdoc;
    let mut i = 0;
    while !(*p.names.offset(i as isize)).category.is_null() {
        if CStr::from_ptr((*p.names.offset(i as isize)).category).to_bytes() == category {
            break;
        }
        i += 1;
    }
    if (*p.names.offset(i as isize)).category.is_null() {
        warn!(
            "Unknown name dictionary category \"{}\".",
            category.display()
        );
        return -1i32;
    }
    if (*p.names.offset(i as isize)).data.is_null() {
        (*p.names.offset(i as isize)).data = pdf_new_name_tree()
    }
    let keyptr = key.as_ptr() as *const libc::c_void;
    let keylen = key.len() as i32;
    pdf_names_add_object((*p.names.offset(i as isize)).data, keyptr, keylen, value)
}
unsafe fn pdf_doc_add_goto(annot_dict: &mut pdf_dict) {
    let mut A: *mut pdf_obj = ptr::null_mut();
    let mut D: *mut pdf_obj = ptr::null_mut();
    if pdoc.check_gotos == 0 {
        return;
    }
    /*
     * An annotation dictionary coming from an annotation special
     * must have a "Subtype". An annotation dictionary coming from
     * an outline special has none.
     */
    if let Some(subtype) = DerefObj::new(annot_dict.get_mut("Subtype")) {
        match &subtype.data {
            PdfObjVariant::UNDEFINED => {
                return undefined(A, D);
            }
            PdfObjVariant::NAME(n) if n.to_bytes() == b"Link" => {}
            PdfObjVariant::NAME(_) => {
                return cleanup(A, D);
            }
            _ => {
                return error(A, D);
            }
        }
    }

    let mut key = "Dest";
    D = pdf_deref_obj(annot_dict.get_mut(key));
    let mut dict = annot_dict;
    match D.as_mut() {
        Some(D) if matches!(D.data, PdfObjVariant::UNDEFINED) => {
            return undefined(A, D);
        }
        _ => {}
    }

    A = pdf_deref_obj(dict.get_mut("A"));
    if let Some(A) = A.as_mut() {
        if let PdfObjVariant::UNDEFINED = A.data {
            return undefined(A, D);
        } else if D.as_ref().is_some() {
            return error(A, D);
        } else {
            if let PdfObjVariant::DICT(a) = &mut A.data {
                if let Some(S) = DerefObj::new(a.get_mut("S")) {
                    match &S.data {
                        PdfObjVariant::UNDEFINED => {
                            return undefined(A, D);
                        }
                        PdfObjVariant::NAME(n) if n.to_bytes() == b"GoTo" => {}
                        PdfObjVariant::NAME(_) => {
                            return cleanup(A, D);
                        }
                        _ => {
                            return error(A, D);
                        }
                    }
                } else {
                    return error(A, D);
                }

                key = "D";
                D = pdf_deref_obj(a.get_mut(key));
                dict = a;
            } else {
                return error(A, D);
            }
        }
    }

    let dest = if let Some(D) = D.as_mut() {
        match &D.data {
            PdfObjVariant::STRING(s) => s.to_bytes(),
            PdfObjVariant::ARRAY(_) => return cleanup(A, D),
            PdfObjVariant::UNDEFINED => return undefined(A, D),
            _ => return error(A, D),
        }
    } else {
        return error(A, D);
    };

    let mut D_new = ht_lookup_table(
        &mut pdoc.gotos,
        dest.as_ptr() as *const libc::c_void,
        dest.len() as i32,
    ) as *mut pdf_obj;
    if D_new.is_null() {
        /* We use hexadecimal notation for our numeric destinations.
         * Other bases (e.g., 10+26 or 10+2*26) would be more efficient.
         */
        let buf = format!("{:x}", ht_table_size(&mut pdoc.gotos));
        /* Maybe reference */
        D_new = pdf_string::new(buf).into_obj();
        ht_append_table(
            &mut pdoc.gotos,
            dest.as_ptr() as *const libc::c_void,
            dest.len() as i32,
            D_new as *mut libc::c_void,
        );
    }

    dict.set(key, pdf_link_obj(D_new));

    return;

    unsafe fn cleanup(A: *mut pdf_obj, D: *mut pdf_obj) {
        pdf_release_obj(A);
        pdf_release_obj(D);
    }

    unsafe fn error(A: *mut pdf_obj, D: *mut pdf_obj) {
        warn!("Unknown PDF annotation format. Output file may be broken.");
        cleanup(A, D)
    }
    unsafe fn undefined(A: *mut pdf_obj, D: *mut pdf_obj) {
        warn!("Cannot optimize PDF annotations. Output file may be broken. Please restart with option \"-C 0x10\"\n");
        cleanup(A, D)
    }
}
unsafe fn warn_undef_dests(dests: *mut ht_table, gotos: *mut ht_table) {
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
        let key: *mut i8 = ht_iter_getkey(&mut iter, &mut keylen);
        if ht_lookup_table(dests, key as *const libc::c_void, keylen).is_null() {
            let dest = std::slice::from_raw_parts(key as *const u8, keylen as usize);
            warn!("PDF destination \"{}\" not defined.", dest.display());
        }
        if !(ht_iter_next(&mut iter) >= 0i32) {
            break;
        }
    }
    ht_clear_iter(&mut iter);
}
unsafe fn pdf_doc_close_names(p: &mut pdf_doc) {
    let mut i = 0;
    while !(*p.names.offset(i as isize)).category.is_null() {
        if !(*p.names.offset(i as isize)).data.is_null() {
            let data: *mut ht_table = (*p.names.offset(i as isize)).data;
            let name_tree;
            if pdoc.check_gotos == 0
                || strcmp(
                    (*p.names.offset(i as isize)).category,
                    b"Dests\x00" as *const u8 as *const i8,
                ) != 0
            {
                let (_name_tree, _) = pdf_names_create_tree(data, ptr::null_mut());
                name_tree = _name_tree;
            } else {
                let (_name_tree, count) = pdf_names_create_tree(data, &mut pdoc.gotos);
                name_tree = _name_tree;
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
            if let Some(name_tree) = name_tree {
                let name_tree = name_tree.into_obj();
                if p.root.names.is_null() {
                    p.root.names = pdf_dict::new().into_obj();
                }
                (*p.root.names).as_dict_mut().set(
                    CStr::from_ptr((*p.names.offset(i as isize)).category)
                        .to_str()
                        .unwrap(),
                    pdf_ref_obj(name_tree),
                );
                pdf_release_obj(name_tree);
            }
            pdf_delete_name_tree(&mut (*p.names.offset(i as isize)).data);
        }
        i += 1;
    }
    if !p.root.names.is_null() {
        if let Some(tmp) = (*p.root.dict).as_dict().get("Names") {
            if let PdfObjVariant::DICT(tmp) = &tmp.data {
                (*p.root.names).as_dict_mut().merge(tmp);
                (*p.root.dict)
                    .as_dict_mut()
                    .set("Names", pdf_ref_obj(p.root.names));
            } else {
                /* What should I do? */
                warn!("Could not modify Names dictionary.");
            }
        } else {
            (*p.root.dict)
                .as_dict_mut()
                .set("Names", pdf_ref_obj(p.root.names));
        }
        pdf_release_obj(p.root.names);
        p.root.names = ptr::null_mut()
    }
    p.names = mfree(p.names as *mut libc::c_void) as *mut name_dict;
    ht_clear_table(&mut p.gotos);
}

pub(crate) unsafe fn pdf_doc_add_annot(
    page_no: usize,
    rect: &Rect,
    annot_dict: *mut pdf_obj,
    new_annot: i32,
) {
    let p = &mut pdoc;
    let annot_grow: f64 = p.opt.annot_grow;
    let page = doc_get_page_entry(p, page_no);
    if page.annots.is_null() {
        page.annots = Vec::new().into_obj();
    }
    let mut mediabox = Rect::zero();
    pdf_doc_get_mediabox(page_no, &mut mediabox);
    let pos = pdf_dev_get_coord();
    let annbox = rect.translate(-pos.to_vector());
    if annbox.min.x < mediabox.min.x
        || annbox.max.x > mediabox.max.x
        || annbox.min.y < mediabox.min.y
        || annbox.max.y > mediabox.max.y
    {
        warn!("Annotation out of page boundary.");

        warn!(
            "Current page\'s MediaBox: {}",
            format!(
                "[{}, {}, {}, {}]",
                mediabox.min.x, mediabox.min.y, mediabox.max.x, mediabox.max.y
            )
        );
        warn!(
            "Annotation: {}",
            format!(
                "[{}, {}, {}, {}]",
                annbox.min.x, annbox.min.y, annbox.max.x, annbox.max.y
            )
        );
        warn!("Maybe incorrect paper size specified.");
    }
    if annbox.min.x > annbox.max.x || annbox.min.y > annbox.max.y {
        warn!(
            "Rectangle with negative width/height: {}",
            format!(
                "[{}, {}, {}, {}]",
                annbox.min.x, annbox.min.y, annbox.max.x, annbox.max.y
            )
        );
    }
    let mut rect_array = vec![];
    rect_array.push_obj(((annbox.min.x - annot_grow) / 0.001 + 0.5).floor() * 0.001);
    rect_array.push_obj(((annbox.min.y - annot_grow) / 0.001 + 0.5).floor() * 0.001);
    rect_array.push_obj(((annbox.max.x + annot_grow) / 0.001 + 0.5).floor() * 0.001);
    rect_array.push_obj(((annbox.max.y + annot_grow) / 0.001 + 0.5).floor() * 0.001);
    (*annot_dict).as_dict_mut().set("Rect", rect_array);
    (*page.annots).as_array_mut().push(pdf_ref_obj(annot_dict));
    if new_annot != 0 {
        pdf_doc_add_goto((*annot_dict).as_dict_mut());
    };
}
/*
 * PDF Article Thread
 */
unsafe fn pdf_doc_init_articles(p: &mut pdf_doc) {
    p.root.threads = ptr::null_mut();
    p.articles = Vec::new();
}

pub(crate) unsafe fn pdf_doc_begin_article(article_id: &str, article_info: *mut pdf_obj) {
    assert!(
        !article_id.is_empty(),
        "Article thread without internal identifier."
    );
    pdoc.articles.push(pdf_article {
        id: article_id.to_string(),
        info: article_info,
        beads: Vec::new(),
    });
}
unsafe fn find_bead<'a>(article: &'a mut pdf_article, bead_id: &str) -> Option<&'a mut pdf_bead> {
    for b in &mut article.beads {
        if b.id == bead_id {
            return Some(b);
        }
    }
    None
}

pub(crate) unsafe fn pdf_doc_add_bead(
    article_id: &str,
    bead_id: &str,
    page_no: usize,
    rect: &Rect,
) {
    let p = &mut pdoc;
    if article_id.is_empty() {
        panic!("No article identifier specified.");
    }
    let mut article = None;
    for a in &mut p.articles {
        if a.id == article_id {
            article = Some(a);
            break;
        }
    }
    let article = article.expect("Specified article thread that doesn\'t exist.");
    let bead = if !bead_id.is_empty() {
        find_bead(article, bead_id)
    } else {
        None
    };
    if let Some(bead) = bead {
        bead.rect = *rect;
        bead.page_no = page_no as i32;
    } else {
        let id = if !bead_id.is_empty() {
            bead_id.to_string()
        } else {
            String::new()
        };
        article.beads.push(pdf_bead {
            id,
            rect: *rect,
            page_no: page_no as i32,
        });
    }
}
unsafe fn make_article(
    p: &mut pdf_doc,
    article: &mut pdf_article,
    bead_ids: &[String],
    article_info: *mut pdf_obj,
) -> *mut pdf_obj {
    let mut art_dict = pdf_dict::new().into_obj();
    let mut last = ptr::null_mut();
    let mut prev = last;
    let mut first = prev;
    /*
     * The bead_ids represents logical order of beads in an article thread.
     * If bead_ids is not given, we create an article thread in the order of
     * beads appeared.
     */
    let n = if !bead_ids.is_empty() {
        bead_ids.len()
    } else {
        article.beads.len()
    };
    for i in 0..n {
        let bead = if !bead_ids.is_empty() {
            find_bead(article, &bead_ids[i])
        } else {
            Some(&mut article.beads[i])
        };
        match bead {
            Some(bead) if bead.page_no >= 0 => {
                last = pdf_dict::new().into_obj();
                if prev.is_null() {
                    first = last;
                    (*first).as_dict_mut().set("T", pdf_ref_obj(art_dict));
                } else {
                    (*prev).as_dict_mut().set("N", pdf_ref_obj(last));
                    (*last).as_dict_mut().set("V", pdf_ref_obj(prev));
                    /* We must link first to last. */
                    if prev != first {
                        pdf_release_obj(prev);
                    }
                }
                /* Realize bead now. */
                let page = doc_get_page_entry(p, bead.page_no as usize);
                if page.beads.is_null() {
                    page.beads = Vec::new().into_obj();
                }
                (*last).as_dict_mut().set("P", pdf_link_obj(page.page_ref));
                let mut rect = vec![];
                rect.push_obj((bead.rect.min.x / 0.01 + 0.5).floor() * 0.01);
                rect.push_obj((bead.rect.min.y / 0.01 + 0.5).floor() * 0.01);
                rect.push_obj((bead.rect.max.x / 0.01 + 0.5).floor() * 0.01);
                rect.push_obj((bead.rect.max.y / 0.01 + 0.5).floor() * 0.01);
                (*last).as_dict_mut().set("R", rect);
                (*page.beads).as_array_mut().push(pdf_ref_obj(last));
                prev = last
            }
            _ => {}
        }
    }
    if !first.is_null() && !last.is_null() {
        (*last).as_dict_mut().set("N", pdf_ref_obj(first));
        (*first).as_dict_mut().set("V", pdf_ref_obj(last));
        if first != last {
            pdf_release_obj(last);
        }
        (*art_dict).as_dict_mut().set("F", pdf_ref_obj(first));
        /* If article_info is supplied, we override article->info. */
        if !article_info.is_null() {
            (*art_dict).as_dict_mut().set("I", article_info);
        } else if !article.info.is_null() {
            (*art_dict)
                .as_dict_mut()
                .set("I", pdf_ref_obj(article.info));
            pdf_release_obj(article.info);
            article.info = ptr::null_mut()
            /* We do not write as object reference. */
        }
        pdf_release_obj(first);
    } else {
        pdf_release_obj(art_dict);
        art_dict = ptr::null_mut()
    }
    art_dict
}
unsafe fn pdf_doc_close_articles(p: *mut pdf_doc) {
    for article in &mut (*p).articles {
        if !article.beads.is_empty() {
            let art_dict = make_article(&mut (*p), article, &[], ptr::null_mut());
            if (*p).root.threads.is_null() {
                (*p).root.threads = Vec::new().into_obj();
            }
            (*(*p).root.threads)
                .as_array_mut()
                .push(pdf_ref_obj(art_dict));
            pdf_release_obj(art_dict);
        }
    }
    (*p).articles = Vec::new();
    if !(*p).root.threads.is_null() {
        (*(*p).root.dict)
            .as_dict_mut()
            .set("Threads", pdf_ref_obj((*p).root.threads));
        pdf_release_obj((*p).root.threads);
        (*p).root.threads = ptr::null_mut()
    };
}
/* page_no = 0 for root page tree node. */

pub(crate) unsafe fn pdf_doc_set_mediabox(page_no: usize, mediabox: &Rect) {
    let mut p = &mut pdoc;
    if page_no == 0 {
        p.pages.mediabox = *mediabox;
    } else {
        let page = doc_get_page_entry(p, page_no);
        page.cropbox = *mediabox;
        page.flags |= 1i32 << 0i32
    };
}
unsafe fn pdf_doc_get_mediabox(page_no: usize, mediabox: &mut Rect) {
    let p = &mut pdoc;
    if page_no == 0 {
        *mediabox = p.pages.mediabox;
    } else {
        let page = doc_get_page_entry(p, page_no);
        if page.flags & 1i32 << 0i32 != 0 {
            *mediabox = page.cropbox;
        } else {
            *mediabox = p.pages.mediabox;
        }
    };
}

pub(crate) unsafe fn pdf_doc_current_page_resources() -> *mut pdf_obj {
    let mut p = &mut pdoc;
    if !p.pending_forms.is_null() {
        if !(*p.pending_forms).form.resources.is_null() {
            (*p.pending_forms).form.resources
        } else {
            (*p.pending_forms).form.resources = pdf_dict::new().into_obj();
            (*p.pending_forms).form.resources
        }
    } else {
        let currentpage = &mut p.pages.entries[p.pages.num_entries];
        if !currentpage.resources.is_null() {
            currentpage.resources
        } else {
            currentpage.resources = pdf_dict::new().into_obj();
            currentpage.resources
        }
    }
}

pub(crate) unsafe fn pdf_doc_get_dictionary(category: &str) -> *mut pdf_obj {
    let mut p = &mut pdoc;
    let dict = match category {
        "Names" => {
            if p.root.names.is_null() {
                p.root.names = pdf_dict::new().into_obj();
            }
            p.root.names
        }
        "Pages" => {
            if p.root.pages.is_null() {
                p.root.pages = pdf_dict::new().into_obj();
            }
            p.root.pages
        }
        "Catalog" => {
            if p.root.dict.is_null() {
                p.root.dict = pdf_dict::new().into_obj();
            }
            p.root.dict
        }
        "Info" => {
            if p.info.is_null() {
                p.info = pdf_dict::new().into_obj();
            }
            p.info
        }
        "@THISPAGE" => {
            /* Sorry for this... */
            let currentpage = &mut p.pages.entries[(*p).pages.num_entries];
            currentpage.page_obj
        }
        _ => ptr::null_mut(),
    };
    if dict.is_null() {
        panic!("Document dict. \"{}\" not exist. ", category);
    }
    dict
}

pub(crate) unsafe fn pdf_doc_current_page_number() -> usize {
    let p: *mut pdf_doc = &mut pdoc;
    (*p).pages.num_entries + 1
}

pub(crate) unsafe fn pdf_doc_ref_page(page_no: usize) -> *mut pdf_obj {
    let p = &mut pdoc;
    let page = doc_get_page_entry(p, page_no);
    if page.page_obj.is_null() {
        page.page_obj = pdf_dict::new().into_obj();
        page.page_ref = pdf_ref_obj(page.page_obj)
    }
    pdf_link_obj(page.page_ref)
}

pub(crate) unsafe fn pdf_doc_get_reference(category: &str) -> *mut pdf_obj {
    let page_no = pdf_doc_current_page_number();
    let ref_0 = match category {
        "@THISPAGE" => pdf_doc_ref_page(page_no),
        "@PREVPAGE" => {
            if page_no <= 1 {
                panic!("Reference to previous page, but no pages have been completed yet.");
            }
            pdf_doc_ref_page(page_no - 1)
        }
        "@NEXTPAGE" => pdf_doc_ref_page(page_no + 1),
        _ => ptr::null_mut(),
    };
    if ref_0.is_null() {
        panic!("Reference to \"{}\" not exist. ", category);
    }
    ref_0
}
unsafe fn pdf_doc_new_page(p: &mut pdf_doc) {
    if p.pages.num_entries >= p.pages.entries.len() as _ {
        doc_resize_page_entries(p, p.pages.num_entries + 1);
    }
    /*
     * This is confusing. pdf_doc_finish_page() have increased page count!
     */
    let currentpage = &mut p.pages.entries[p.pages.num_entries];
    /* Was this page already instantiated by a forward reference to it? */
    if currentpage.page_ref.is_null() {
        currentpage.page_obj = pdf_dict::new().into_obj();
        currentpage.page_ref = pdf_ref_obj(currentpage.page_obj)
    }
    currentpage.background = ptr::null_mut();
    currentpage.contents = pdf_stream::new(STREAM_COMPRESS).into_obj();
    currentpage.resources = pdf_dict::new().into_obj();
    currentpage.annots = ptr::null_mut();
    currentpage.beads = ptr::null_mut();
}
/* This only closes contents and resources. */
unsafe fn pdf_doc_finish_page(mut p: *mut pdf_doc) {
    if !(*p).pending_forms.is_null() {
        panic!("A pending form XObject at the end of page.");
    }
    let currentpage = &mut (*p).pages.entries[(*p).pages.num_entries];
    if currentpage.page_obj.is_null() {
        currentpage.page_obj = pdf_dict::new().into_obj();
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
    if !(*p).pages.bop.is_null() && (*(*p).pages.bop).as_stream().len() > 0 {
        currentpage.content_refs[0] = pdf_ref_obj((*p).pages.bop)
    } else {
        currentpage.content_refs[0] = ptr::null_mut()
    }
    /*
     * Current page background content stream.
     */
    if !currentpage.background.is_null() {
        if (*currentpage.background).as_stream().len() > 0 {
            currentpage.content_refs[1] = pdf_ref_obj(currentpage.background);
            (*currentpage.background).as_stream_mut().add_str("\n");
        }
        pdf_release_obj(currentpage.background);
        currentpage.background = ptr::null_mut()
    } else {
        currentpage.content_refs[1] = ptr::null_mut()
    }
    /* Content body of current page */
    currentpage.content_refs[2] = pdf_ref_obj(currentpage.contents);
    (*currentpage.contents).as_stream_mut().add_str("\n");
    pdf_release_obj(currentpage.contents);
    currentpage.contents = ptr::null_mut();
    /*
     * Global EOP content stream.
     */
    if !(*p).pages.eop.is_null() && (*(*p).pages.eop).as_stream().len() > 0 {
        currentpage.content_refs[3] = pdf_ref_obj((*p).pages.eop)
    } else {
        currentpage.content_refs[3] = ptr::null_mut()
    }
    /*
     * Page resources.
     */
    if !currentpage.resources.is_null() {
        /*
         * ProcSet is obsolete in PDF-1.4 but recommended for compatibility.
         */
        let mut procset = vec![];
        procset.push_obj("PDF");
        procset.push_obj("Text");
        procset.push_obj("ImageC");
        procset.push_obj("ImageB");
        procset.push_obj("ImageI");
        (*currentpage.resources)
            .as_dict_mut()
            .set("ProcSet", procset);
        (*currentpage.page_obj)
            .as_dict_mut()
            .set("Resources", pdf_ref_obj(currentpage.resources));
        pdf_release_obj(currentpage.resources);
        currentpage.resources = ptr::null_mut()
    }
    if manual_thumb_enabled != 0 {
        let thumb_filename = format!(
            "{}.{}",
            thumb_basename,
            ((*p).pages.num_entries % 99999) as i64 + 1
        );
        let thumb_ref = read_thumbnail(&thumb_filename);
        if !thumb_ref.is_null() {
            (*currentpage.page_obj)
                .as_dict_mut()
                .set("Thumb", thumb_ref);
        }
    }
    (*p).pages.num_entries += 1;
}

static mut bgcolor: PdfColor = WHITE;

/* Manual thumbnail */
/* Similar to bop_content */

pub(crate) unsafe fn pdf_doc_set_bgcolor(color: Option<&PdfColor>) {
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
    pdf_doc_get_mediabox(pdf_doc_current_page_number(), &mut r);
    let currentpage = &mut p.pages.entries[p.pages.num_entries];
    if currentpage.background.is_null() {
        currentpage.background = pdf_stream::new(STREAM_COMPRESS).into_obj()
    }
    let saved_content = currentpage.contents;
    currentpage.contents = currentpage.background;
    pdf_dev_gsave();
    pdf_dev_set_color(&bgcolor, 0x20, 0);
    pdf_dev_rectfill(&r);
    pdf_dev_grestore();
    currentpage.contents = saved_content;
}

pub(crate) unsafe fn pdf_doc_begin_page(scale: f64, x_origin: f64, y_origin: f64) {
    let p = &mut pdoc;
    let mut M = TMatrix::row_major(scale, 0., 0., scale, x_origin, y_origin);
    /* pdf_doc_new_page() allocates page content stream. */
    pdf_doc_new_page(p);
    pdf_dev_bop(&mut M);
}

pub(crate) unsafe fn pdf_doc_end_page() {
    let p = &mut pdoc;
    pdf_dev_eop();
    doc_fill_page_background(p);
    pdf_doc_finish_page(p);
}

pub(crate) unsafe fn pdf_doc_add_page_content(buffer: &[u8]) {
    let p = &mut pdoc;
    if !p.pending_forms.is_null() {
        (*(*p.pending_forms).form.contents)
            .as_stream_mut()
            .add_slice(&buffer);
    } else {
        let currentpage = &mut p.pages.entries[p.pages.num_entries];
        (*currentpage.contents).as_stream_mut().add_slice(&buffer);
    };
}

static mut doccreator: *mut i8 = ptr::null_mut();
/* Ugh */

pub(crate) unsafe fn pdf_open_document(
    filename: &str,
    enable_encrypt: bool,
    enable_object_stream: bool,
    media_width: f64,
    media_height: f64,
    annot_grow_amount: f64,
    bookmark_open_depth: i32,
    check_gotos: i32,
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
        (*p.info).as_dict_mut().set(
            "Creator",
            pdf_string::new_from_ptr(doccreator as *const libc::c_void, strlen(doccreator) as _),
        );
        doccreator = mfree(doccreator as *mut libc::c_void) as *mut i8
    }
    pdf_doc_init_bookmarks(p, bookmark_open_depth);
    pdf_doc_init_articles(p);
    pdf_doc_init_names(p, check_gotos);
    pdf_doc_init_page_tree(p, media_width, media_height);
    pdf_doc_set_bgcolor(None);
    if enable_encrypt {
        let encrypt = pdf_encrypt_obj().into_obj();
        pdf_set_encrypt(encrypt);
        pdf_release_obj(encrypt);
    }
    pdf_set_id(pdf_enc_id_array());
    /* Create a default name for thumbnail image files */
    if manual_thumb_enabled != 0 {
        thumb_basename = if filename.ends_with(".pdf") {
            filename[..filename.len() - 4].to_string()
        } else {
            filename.to_string()
        };
    }
    p.pending_forms = ptr::null_mut();
}

pub(crate) unsafe fn pdf_doc_set_creator(creator: *const i8) {
    if creator.is_null() || *creator.offset(0) as i32 == '\u{0}' as i32 {
        return;
    }
    doccreator =
        new((strlen(creator).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy(doccreator, creator);
    /* Ugh */
}

pub(crate) unsafe fn pdf_close_document() {
    let p = &mut pdoc;
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
    thumb_basename = String::new();
}
/*
 * All this routine does is give the form a name and add a unity scaling matrix.
 * It fills in required fields.  The caller must initialize the stream.
 */
unsafe fn pdf_doc_make_xform(
    xform: *mut pdf_obj,
    bbox: &mut Rect,
    matrix: Option<&TMatrix>,
    resources: *mut pdf_obj,
    attrib: *mut pdf_obj,
) {
    let xform_dict = (*xform).as_stream_mut().get_dict_mut();
    xform_dict.set("Type", "XObject");
    xform_dict.set("Subtype", "Form");
    xform_dict.set("FormType", 1_f64);
    let mut tmp = vec![];
    tmp.push_obj((bbox.min.x / 0.001 + 0.5).floor() * 0.001);
    tmp.push_obj((bbox.min.y / 0.001 + 0.5).floor() * 0.001);
    tmp.push_obj((bbox.max.x / 0.001 + 0.5).floor() * 0.001);
    tmp.push_obj((bbox.max.y / 0.001 + 0.5).floor() * 0.001);
    xform_dict.set("BBox", tmp);
    if let Some(matrix) = matrix {
        let mut tmp = vec![];
        tmp.push_obj((matrix.m11 / 0.00001 + 0.5).floor() * 0.00001);
        tmp.push_obj((matrix.m12 / 0.00001 + 0.5).floor() * 0.00001);
        tmp.push_obj((matrix.m21 / 0.00001 + 0.5).floor() * 0.00001);
        tmp.push_obj((matrix.m22 / 0.00001 + 0.5).floor() * 0.00001);
        tmp.push_obj((matrix.m31 / 0.001 + 0.5).floor() * 0.001);
        tmp.push_obj((matrix.m32 / 0.001 + 0.5).floor() * 0.001);
        xform_dict.set("Matrix", tmp);
    }
    if !attrib.is_null() {
        xform_dict.merge((*attrib).as_dict());
    }
    xform_dict.set("Resources", resources);
}
/*
 * begin_form_xobj creates an xobject with its "origin" at
 * xpos and ypos that is clipped to the specified bbox. Note
 * that the origin is not the lower left corner of the bbox.
 */

pub(crate) unsafe fn pdf_doc_begin_grabbing(
    ident: &str,
    ref_x: f64,
    ref_y: f64,
    cropbox: &Rect,
) -> i32 {
    let mut p = &mut pdoc;
    let mut info = Box::new(xform_info::default());
    pdf_dev_push_gstate();
    let mut fnode = new((1_u64).wrapping_mul(::std::mem::size_of::<form_list_node>() as u64) as u32)
        as *mut form_list_node;
    (*fnode).prev = p.pending_forms;
    (*fnode).q_depth = pdf_dev_current_depth() as i32;
    let form = &mut (*fnode).form;
    /*
     * The reference point of an Xobject is at the lower left corner
     * of the bounding box.  Since we would like to have an arbitrary
     * reference point, we use a transformation matrix, translating
     * the reference point to (0,0).
     */
    form.matrix = TMatrix::create_translation(-ref_x, -ref_y);
    let ref_xy = point2(ref_x, ref_y).to_vector();
    form.cropbox = cropbox.translate(ref_xy);
    form.contents = pdf_stream::new(STREAM_COMPRESS).into_obj();
    form.resources = pdf_dict::new().into_obj();
    pdf_ximage_init_form_info(&mut info);
    info.matrix = TMatrix::create_translation(-ref_x, -ref_y);
    info.bbox = *cropbox;
    /* Use reference since content itself isn't available yet. */
    let xobj_id =
        pdf_ximage_defineresource(ident, XInfo::Form(info), pdf_ref_obj((*form).contents));
    p.pending_forms = fnode;
    /*
     * Make sure the object is self-contained by adding the
     * current font and color to the object stream.
     */
    pdf_dev_reset_fonts(1i32); /* force color operators to be added to stream */
    pdf_dev_reset_color(1i32);
    xobj_id
}

pub(crate) unsafe fn pdf_doc_end_grabbing(attrib: *mut pdf_obj) {
    let mut p = &mut pdoc;
    if p.pending_forms.is_null() {
        warn!("Tried to close a nonexistent form XOject.");
        return;
    }
    let fnode = p.pending_forms;
    let form = &mut (*fnode).form;
    pdf_dev_grestore_to((*fnode).q_depth as usize);
    /*
     * ProcSet is obsolete in PDF-1.4 but recommended for compatibility.
     */
    let mut procset = vec![];
    procset.push_obj("PDF");
    procset.push_obj("Text");
    procset.push_obj("ImageC");
    procset.push_obj("ImageB");
    procset.push_obj("ImageI");
    (*(*form).resources).as_dict_mut().set("ProcSet", procset);
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
    p.pending_forms = (*fnode).prev;
    pdf_dev_pop_gstate();
    pdf_dev_reset_fonts(1i32);
    pdf_dev_reset_color(0i32);
    free(fnode as *mut libc::c_void);
}
static mut breaking_state: BreakingState = BreakingState {
    dirty: 0,
    broken: 0,
    annot_dict: ptr::null_mut(),
    rect: Rect::new(point2(0., 0.), point2(0., 0.)),
};
unsafe fn reset_box() {
    breaking_state.rect = Rect::new(
        point2(core::f64::INFINITY, core::f64::INFINITY),
        point2(core::f64::NEG_INFINITY, core::f64::NEG_INFINITY),
    );
    breaking_state.dirty = 0i32;
}

pub(crate) unsafe fn pdf_doc_begin_annot(dict: *mut pdf_obj) {
    breaking_state.annot_dict = dict;
    breaking_state.broken = 0i32;
    reset_box();
}

pub(crate) unsafe fn pdf_doc_end_annot() {
    pdf_doc_break_annot();
    breaking_state.annot_dict = ptr::null_mut();
}

pub(crate) unsafe fn pdf_doc_break_annot() {
    if breaking_state.dirty != 0 {
        /* Copy dict */
        let mut annot_dict = pdf_dict::new();
        annot_dict.merge((*breaking_state.annot_dict).as_dict());
        let annot_dict = annot_dict.into_obj();
        pdf_doc_add_annot(
            pdf_doc_current_page_number(),
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

pub(crate) unsafe fn pdf_doc_expand_box(rect: &Rect) {
    breaking_state.rect.min.x = breaking_state.rect.min.x.min(rect.min.x);
    breaking_state.rect.min.y = breaking_state.rect.min.y.min(rect.min.y);
    breaking_state.rect.max.x = breaking_state.rect.max.x.max(rect.max.x);
    breaking_state.rect.max.y = breaking_state.rect.max.y.max(rect.max.y);
    breaking_state.dirty = 1;
}
