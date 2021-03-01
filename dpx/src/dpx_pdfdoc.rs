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
#![allow(
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::dpx_error::{Result, ERR};

use euclid::point2;

use crate::bridge::DisplayExt;
use crate::{info, warn};
use std::ptr;

use super::dpx_dpxutil::{
    ht_append_table, ht_clear_iter, ht_clear_table, ht_init_table, ht_iter_next, ht_lookup_table,
    ht_set_iter, ht_table_size,
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
    pdf_ximage_get_reference, pdf_ximage_set_verbose, XInfo,
};
use super::dpx_pngimage::check_for_png;
use crate::bridge::{InFile, TTInputFormat};
use crate::dpx_pdfobj::{
    pdf_deref_obj, pdf_dict, pdf_file, pdf_file_get_catalog, pdf_link_obj, pdf_new_ref, pdf_obj,
    pdf_out_flush, pdf_out_init, pdf_ref_obj, pdf_release_obj, pdf_remove_dict, pdf_set_encrypt,
    pdf_set_id, pdf_set_info, pdf_set_root, pdf_stream, pdf_string, DerefObj, IntoObj, IntoRef,
    Object, PushObj, STREAM_COMPRESS,
};
use libc::free;

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
pub(crate) struct PdfDoc {
    pub(crate) root: DocRoot,
    pub(crate) info: *mut pdf_obj,
    pub(crate) pages: DocPages,
    pub(crate) outlines: DocOutlines,
    pub(crate) articles: Vec<pdf_article>,
    pub(crate) names: Vec<name_dict>,
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
    pub(crate) category: &'static str,
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PdfPageBoundary {
    Auto = 0,
    Mediabox = 2,
    Cropbox = 1,
    Artbox = 3,
    Trimbox = 4,
    Bleedbox = 5,
}

static mut verbose: i32 = 0;
static mut manual_thumb_enabled: i8 = 0_i8;
static mut thumb_basename: String = String::new();

/*pub(crate) unsafe fn pdf_doc_enable_manual_thumbnails() {
    manual_thumb_enabled = 1_i8;
    // without HAVE_LIBPNG:
    // warn!("Manual thumbnail is not supported without the libpng library.");
}*/
unsafe fn read_thumbnail(thumb_filename: &str) -> *mut pdf_obj {
    let options: load_options = load_options {
        page_no: 1,
        bbox_type: PdfPageBoundary::Auto,
        dict: ptr::null_mut(),
    };
    let handle = InFile::open(thumb_filename, TTInputFormat::PICT, 0);
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

pub(crate) unsafe fn pdf_doc_set_verbose(level: i32) {
    verbose = level;
    pdf_font_set_verbose(level);
    pdf_color_set_verbose(level);
    pdf_ximage_set_verbose(level);
}
static mut pdoc: PdfDoc = PdfDoc {
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
    names: Vec::new(),
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

pub(crate) unsafe fn pdf_doc() -> &'static PdfDoc {
    &pdoc
}
pub(crate) unsafe fn pdf_doc_mut() -> &'static mut PdfDoc {
    &mut pdoc
}

impl PdfDoc {
    unsafe fn init_catalog(&mut self) {
        self.root.viewerpref = ptr::null_mut();
        self.root.pagelabels = ptr::null_mut();
        self.root.pages = ptr::null_mut();
        self.root.names = ptr::null_mut();
        self.root.threads = ptr::null_mut();
        self.root.dict = pdf_dict::new().into_obj();
        pdf_set_root(&mut *self.root.dict);
    }

    unsafe fn close_catalog(&mut self) {
        if !self.root.viewerpref.is_null() {
            if let Some(tmp) = (*self.root.dict).as_dict().get("ViewerPreferences") {
                if let Object::Dict(tmp) = &tmp.data {
                    (*self.root.viewerpref).as_dict_mut().merge(&tmp);
                    (*self.root.dict)
                        .as_dict_mut()
                        .set("ViewerPreferences", pdf_ref_obj(self.root.viewerpref));
                } else {
                    /* What should I do? */
                    warn!("Could not modify ViewerPreferences.");
                    /* Maybe reference */
                }
            } else {
                (*self.root.dict)
                    .as_dict_mut()
                    .set("ViewerPreferences", pdf_ref_obj(self.root.viewerpref));
            }
            pdf_release_obj(self.root.viewerpref);
            self.root.viewerpref = ptr::null_mut()
        }
        if !self.root.pagelabels.is_null() {
            let tmp = (*self.root.dict).as_dict_mut().get_mut("PageLabels");
            if tmp.is_none() {
                let mut tmp = pdf_dict::new();
                tmp.set("Nums", pdf_link_obj(self.root.pagelabels));
                (*self.root.dict)
                    .as_dict_mut()
                    .set("PageLabels", tmp.into_ref());
            } else {
                /* What should I do? */
                warn!("Could not modify PageLabels.");
            }
            pdf_release_obj(self.root.pagelabels);
            self.root.pagelabels = ptr::null_mut()
        }
        (*self.root.dict).as_dict_mut().set("Type", "Catalog");
        pdf_release_obj(self.root.dict);
        self.root.dict = ptr::null_mut();
    }
    /*
     * Pages are starting at 1.
     * The page count does not increase until the page is finished.
     */
    unsafe fn resize_page_entries(&mut self, size: usize) {
        self.pages.entries.resize_with(size, || pdf_page {
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
    unsafe fn get_page_entry<'a>(&mut self, page_no: usize) -> &mut pdf_page {
        assert!(page_no < 65536, "Page number {} too large!", page_no);
        assert!(page_no != 0, "Invalid Page number {}.", page_no);
        if page_no > self.pages.entries.len() as _ {
            self.resize_page_entries(page_no);
        }
        &mut self.pages.entries[page_no - 1]
    }

    pub(crate) unsafe fn set_bop_content(&mut self, content: &[u8]) {
        if !self.pages.bop.is_null() {
            pdf_release_obj(self.pages.bop);
            self.pages.bop = ptr::null_mut()
        }
        if !content.is_empty() {
            self.pages.bop = pdf_stream::new(STREAM_COMPRESS).into_obj();
            (*self.pages.bop).as_stream_mut().add_slice(content);
        } else {
            self.pages.bop = ptr::null_mut()
        };
    }

    pub(crate) unsafe fn set_eop_content(&mut self, content: &[u8]) {
        if !self.pages.eop.is_null() {
            pdf_release_obj(self.pages.eop);
            self.pages.eop = ptr::null_mut()
        }
        if !content.is_empty() {
            self.pages.eop = pdf_stream::new(STREAM_COMPRESS).into_obj();
            (*self.pages.eop).as_stream_mut().add_slice(content);
        } else {
            self.pages.eop = ptr::null_mut()
        };
    }

    unsafe fn init_docinfo(&mut self) {
        self.info = pdf_dict::new().into_obj();
        pdf_set_info(&mut *self.info);
    }
    unsafe fn close_docinfo(&mut self) {
        let docinfo = &mut *self.info;
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
                if let Object::String(value) = &value.data {
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
        self.info = ptr::null_mut();
    }
    unsafe fn get_page_resources(&mut self, category: &str) -> *mut pdf_obj {
        let res_dict = if !self.pending_forms.is_null() {
            if !(*self.pending_forms).form.resources.is_null() {
                (*self.pending_forms).form.resources
            } else {
                (*self.pending_forms).form.resources = pdf_dict::new().into_obj();
                (*self.pending_forms).form.resources
            }
        } else {
            let currentpage = &mut self.pages.entries[self.pages.num_entries] as *mut pdf_page;
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

    pub(crate) unsafe fn add_page_resource(
        &mut self,
        category: &str,
        resource_name: &[u8],
        mut resource_ref: *mut pdf_obj,
    ) {
        if !(!resource_ref.is_null() && (*resource_ref).is_indirect()) {
            warn!("Passed non indirect reference...");
            resource_ref = pdf_ref_obj(resource_ref)
            /* leak */
        }
        let resources = self.get_page_resources(category);
        if let Some(duplicate) = (*resources).as_dict_mut().get_mut(resource_name) {
            if (*duplicate)
                .as_indirect()
                .compare((*resource_ref).as_indirect())
            {
                warn!(
                    "Conflicting page resource found (page: {}, category: {}, name: {}).",
                    pdf_doc().current_page_number(),
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
}
unsafe fn doc_flush_page(p: *mut PdfDoc, page: &mut pdf_page, parent_ref: *mut pdf_obj) {
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
    p: *mut PdfDoc,
    firstpage: *mut pdf_page,
    num_pages: i32,
    parent_ref: *mut pdf_obj,
) -> *mut pdf_obj {
    let mut self_0 = pdf_dict::new();
    self_0.set("Type", "Pages");
    self_0.set("Count", num_pages as f64);
    if !parent_ref.is_null() {
        self_0.set("Parent", parent_ref);
    }
    /*
     * This is a slight kludge which allow the subtree dictionary
     * generated by this routine to be merged with the real
     * page_tree dictionary, while keeping the indirect object
     * references right.
     */
    let self_0 = &mut *self_0.into_obj();
    let self_ref = if !parent_ref.is_null() {
        pdf_new_ref(self_0).into_obj()
    } else {
        pdf_ref_obj((*p).root.pages)
    };
    let mut kids = vec![];
    match num_pages {
        1..=4 => {
            for i in 0..num_pages {
                let page = &mut *firstpage.offset(i as isize);
                if page.page_ref.is_null() {
                    page.page_ref = pdf_ref_obj(page.page_obj)
                }
                kids.push(pdf_link_obj(page.page_ref));
                doc_flush_page(p, page, pdf_link_obj(self_ref));
            }
        }
        5..=i32::MAX => {
            for i in 0..4 {
                let start = i * num_pages / 4;
                let end = (i + 1) * num_pages / 4;
                if end - start > 1 {
                    let subtree = &mut *build_page_tree(
                        p,
                        firstpage.offset(start as isize),
                        end - start,
                        pdf_link_obj(self_ref),
                    );
                    kids.push_obj(pdf_new_ref(subtree));
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
        _ => {}
    }
    self_0.as_dict_mut().set("Kids", kids);
    pdf_release_obj(self_ref);
    self_0
}
impl PdfDoc {
    unsafe fn init_page_tree(&mut self, media_width: f64, media_height: f64) {
        /*
         * Create empty page tree.
         * The docroot.pages is kept open until the document is closed.
         * This allows the user to write to pages if he so choses.
         */
        self.root.pages = pdf_dict::new().into_obj();
        self.pages.num_entries = 0;
        self.pages.entries = Vec::new();
        self.pages.bop = ptr::null_mut();
        self.pages.eop = ptr::null_mut();
        self.pages.mediabox = Rect::new(point2(0., 0.), point2(media_width, media_height));
    }
    unsafe fn close_page_tree(&mut self) {
        /*
         * Do consistency check on forward references to pages.
         */
        for page_no in (self.pages.num_entries + 1)..=self.pages.entries.len() {
            let page = self.get_page_entry(page_no);
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
            self,
            &mut self.pages.entries[0],
            self.pages.num_entries as i32,
            ptr::null_mut(),
        );
        (*self.root.pages)
            .as_dict_mut()
            .merge((*page_tree_root).as_dict());
        pdf_release_obj(page_tree_root);
        /* They must be after build_page_tree() */
        if !self.pages.bop.is_null() {
            (*self.pages.bop).as_stream_mut().add_str("\n");
            pdf_release_obj(self.pages.bop);
            self.pages.bop = ptr::null_mut()
        }
        if !self.pages.eop.is_null() {
            (*self.pages.eop).as_stream_mut().add_str("\n");
            pdf_release_obj(self.pages.eop);
            self.pages.eop = ptr::null_mut()
        }
        /* Create media box at root node and let the other pages inherit it. */
        let mut mediabox = vec![];
        mediabox.push_obj((self.pages.mediabox.min.x / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((self.pages.mediabox.min.y / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((self.pages.mediabox.max.x / 0.01 + 0.5).floor() * 0.01);
        mediabox.push_obj((self.pages.mediabox.max.y / 0.01 + 0.5).floor() * 0.01);
        (*self.root.pages).as_dict_mut().set("MediaBox", mediabox);
        (*self.root.dict)
            .as_dict_mut()
            .set("Pages", pdf_ref_obj(self.root.pages));
        pdf_release_obj(self.root.pages);
        self.root.pages = ptr::null_mut();
        self.pages.entries = Vec::new();
        self.pages.num_entries = 0;
    }
}

pub unsafe fn pdf_doc_get_page_count(pf: &pdf_file) -> i32 {
    let catalog = pdf_file_get_catalog(pf);
    if let Some(pdf_obj {
        data: Object::Dict(page_tree),
        ..
    }) = DerefObj::new((*catalog).as_dict_mut().get_mut("Pages")).as_deref_mut()
    {
        if let Some(pdf_obj {
            data: Object::Number(count),
            ..
        }) = DerefObj::new(page_tree.get_mut("Count")).as_deref()
        {
            return *count as i32;
        }
    }
    0
}

unsafe fn set_bounding_box(
    opt_bbox: PdfPageBoundary,
    mut media_box: Option<DerefObj>,
    mut crop_box: Option<DerefObj>,
    mut art_box: Option<DerefObj>,
    mut trim_box: Option<DerefObj>,
    mut bleed_box: Option<DerefObj>,
) -> Option<Rect> {
    if media_box.is_none() {
        warn!("MediaBox not found in included PDF...");
        return None;
    }
    unsafe fn VALIDATE_BOX(o: Option<&pdf_obj>) -> Option<()> {
        if let Some(o) = o {
            match &o.data {
                Object::Array(a) if a.len() == 4 => Some(()),
                _ => None,
            }
        } else {
            None
        }
    }
    if media_box.is_some() {
        VALIDATE_BOX(media_box.as_deref())?;
    }
    if crop_box.is_some() {
        VALIDATE_BOX(crop_box.as_deref())?;
    }
    if art_box.is_some() {
        VALIDATE_BOX(art_box.as_deref())?;
    }
    if trim_box.is_some() {
        VALIDATE_BOX(trim_box.as_deref())?;
    }
    if bleed_box.is_some() {
        VALIDATE_BOX(bleed_box.as_deref())?;
    }

    let box_0 = if opt_bbox == PdfPageBoundary::Auto {
        if crop_box.is_some() {
            crop_box.clone()
        } else if art_box.is_some() {
            art_box.clone()
        } else if trim_box.is_some() {
            trim_box.clone()
        } else if bleed_box.is_some() {
            bleed_box.clone()
        } else {
            media_box.clone()
        }
    } else {
        if crop_box.is_none() {
            crop_box = media_box.clone();
        }
        if art_box.is_none() {
            art_box = crop_box.clone();
        }
        if trim_box.is_none() {
            trim_box = crop_box.clone();
        }
        if bleed_box.is_none() {
            bleed_box = crop_box.clone();
        }
        /* At this point all boxes must be defined. */
        match opt_bbox {
            PdfPageBoundary::Cropbox => crop_box.clone(),
            PdfPageBoundary::Mediabox => media_box.clone(),
            PdfPageBoundary::Artbox => art_box.clone(),
            PdfPageBoundary::Trimbox => trim_box.clone(),
            PdfPageBoundary::Bleedbox => bleed_box.clone(),
            _ => crop_box.clone(),
        }
    };

    let mut bbox = Rect::zero();
    if let Some(mut box_0) = box_0 {
        for i in (0..4).rev() {
            let array = box_0.as_array_mut();
            if let Some(pdf_obj {
                data: Object::Number(x),
                ..
            }) = DerefObj::new(Some(&mut *array[i])).as_deref()
            {
                match i {
                    0 => bbox.min.x = *x,
                    1 => bbox.min.y = *x,
                    2 => bbox.max.x = *x,
                    3 => bbox.max.y = *x,
                    _ => {}
                }
            } else {
                return None;
            }
        }

        /* New scheme only for XDV files */
        if
        /*dpx_conf.compat_mode == dpx_mode_xdv_mode*/
        is_xdv != 0 || opt_bbox != PdfPageBoundary::Auto {
            let array = media_box.as_mut().unwrap().as_array_mut();
            for i in (0..4).rev() {
                match DerefObj::new(Some(&mut *array[i])).as_deref() {
                    Some(pdf_obj {
                        data: Object::Number(x),
                        ..
                    }) => {
                        let x = *x;
                        match i {
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
                        }
                    }
                    _ => return None,
                }
            }
        }
    } else {
        /* Impossible */
        warn!("No appropriate page boudary box found???");
        return None;
    }

    Some(bbox)
}

unsafe fn set_transform_matrix(bbox: Rect, rotate: Option<&pdf_obj>) -> Option<TMatrix> {
    let mut matrix = TMatrix::identity();
    /* Handle Rotate */
    if let Some(rotate) = rotate {
        if let Object::Number(deg) = rotate.data {
            if deg - (deg as i32 as f64) != 0. {
                warn!("Invalid value specified for /Rotate: {}", deg);
                return None;
            } else if deg != 0. {
                let mut rot = deg as i32;
                if rot % 90 != 0 {
                    warn!("Invalid value specified for /Rotate: {}", deg);
                } else {
                    rot = rot % 360;
                    if rot < 0 {
                        rot += 360;
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
                        _ => {
                            warn!("Invalid value specified for /Rotate: {}", deg);
                        }
                    }
                }
            }
        } else {
            return None;
        }
    }
    Some(matrix)
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
    opt_bbox: PdfPageBoundary,
    resources_p: *mut *mut pdf_obj,
) -> Option<(DerefObj, Rect, TMatrix)>
/* returned values */ {
    let catalog = pdf_file_get_catalog(pf);
    if let Some(mut page_tree) = DerefObj::new((*catalog).as_dict_mut().get_mut("Pages")) {
        if let Object::Dict(_) = page_tree.data {
            let mut resources: *mut pdf_obj = ptr::null_mut();
            let mut rotate = None;
            let mut art_box = None;
            let mut trim_box = None;
            let mut bleed_box = None;
            let mut media_box = None;
            let mut crop_box = None;

            let error_exit = move || -> Option<(DerefObj, Rect, TMatrix)> {
                warn!("Error found in including PDF image.");
                pdf_release_obj(resources);
                None
            };
            let count = match DerefObj::new(page_tree.as_dict_mut().get_mut("Count")).as_deref() {
                Some(pdf_obj {
                    data: Object::Number(count),
                    ..
                }) => *count as i32,
                _ => return error_exit(),
            };
            if page_no <= 0 || page_no > count {
                warn!("Page {} does not exist.", page_no);
                pdf_release_obj(resources);
                return None;
            }

            /*
             * Seek correct page. Get MediaBox, CropBox and Resources.
             * (Note that these entries can be inherited.)
             */
            let mut depth: i32 = 30;
            let mut page_idx: i32 = page_no - 1;
            let mut kids_length = 1;
            let mut i = 0;
            loop {
                depth -= 1;
                if !(depth != 0 && i != kids_length) {
                    break;
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("MediaBox")) {
                    media_box = Some(tmp_0);
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("CropBox")) {
                    crop_box = Some(tmp_0);
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("ArtBox")) {
                    art_box = Some(tmp_0);
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("TrimBox")) {
                    trim_box = Some(tmp_0);
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("BleedBox")) {
                    bleed_box = Some(tmp_0);
                }
                if let Some(tmp_0) = DerefObj::new(page_tree.as_dict_mut().get_mut("Rotate")) {
                    rotate = Some(tmp_0);
                }
                if let Some(tmp_0) =
                    pdf_deref_obj(page_tree.as_dict_mut().get_mut("Resources")).as_mut()
                {
                    pdf_release_obj(resources);
                    resources = tmp_0
                }
                if let Some(mut kids) = DerefObj::new(page_tree.as_dict_mut().get_mut("Kids")) {
                    if let Object::Array(array) = &mut kids.data {
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
                                if let Object::Dict(ptdata) = &mut pt.data {
                                    if let Some(tmp_0) = DerefObj::new(ptdata.get_mut("Count")) {
                                        if let Object::Number(v) = tmp_0.data {
                                            /* Pages object */
                                            count_0 = v as i32;
                                        } else {
                                            return error_exit();
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
                                    return error_exit();
                                }
                            } else {
                                return error_exit();
                            }
                            i += 1;
                        }
                    } else {
                        return error_exit();
                    }
                } else {
                    break;
                }
            }

            if depth == 0 || kids_length == i {
                return error_exit();
            }

            /* Select page boundary box */
            let bbox = if let Some(b) =
                set_bounding_box(opt_bbox, media_box, crop_box, art_box, trim_box, bleed_box)
            {
                b
            } else {
                return error_exit();
            };

            /* Set transformation matrix */
            let matrix = if let Some(m) = set_transform_matrix(bbox, rotate.as_deref()) {
                m
            } else {
                return error_exit();
            };

            if !resources_p.is_null() {
                *resources_p = resources;
            } else {
                pdf_release_obj(resources);
            }
            return Some((page_tree, bbox, matrix));
        } else {
            warn!("Error found in including PDF image.");
            return None;
        }
    } else {
        warn!("Error found in including PDF image.");
        return None;
    }
}

impl PdfDoc {
    unsafe fn init_bookmarks(&mut self, bm_open_depth: i32) {
        self.opt.outline_open_depth = (if bm_open_depth >= 0 {
            bm_open_depth as u32
        } else {
            256u32.wrapping_sub(bm_open_depth as u32)
        }) as i32;
        self.outlines.current_depth = 1;
        let item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
            as *mut pdf_olitem;
        (*item).dict = ptr::null_mut();
        (*item).next = ptr::null_mut();
        (*item).first = ptr::null_mut();
        (*item).parent = ptr::null_mut();
        (*item).is_open = 1;
        self.outlines.current = item;
        self.outlines.first = item;
    }
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
    0
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

impl PdfDoc {
    pub(crate) unsafe fn bookmarks_up(&mut self) -> i32 {
        let item = self.outlines.current;
        if item.is_null() || (*item).parent.is_null() {
            warn!("Can\'t go up above the bookmark root node!");
            return -1;
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
            (*item).is_open = 0;
            (*item).parent = (*parent).parent
        }
        self.outlines.current = item;
        self.outlines.current_depth -= 1;
        0
    }

    pub(crate) unsafe fn bookmarks_down(&mut self) -> i32 {
        let item = self.outlines.current;
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
        (*first).is_open = 0;
        (*first).parent = item;
        (*first).next = ptr::null_mut();
        (*first).first = ptr::null_mut();
        self.outlines.current = first;
        self.outlines.current_depth += 1;
        0
    }

    pub(crate) unsafe fn bookmarks_depth(&self) -> i32 {
        self.outlines.current_depth
    }

    pub(crate) unsafe fn bookmarks_add(&mut self, dict: &mut pdf_obj, is_open: i32) {
        let mut item = self.outlines.current;
        if item.is_null() {
            item = new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
                as *mut pdf_olitem;
            (*item).parent = ptr::null_mut();
            self.outlines.first = item
        } else if !(*item).dict.is_null() {
            /* go to next item */
            item = (*item).next
        }
        (*item).dict = dict;
        (*item).first = ptr::null_mut();
        (*item).is_open = if is_open < 0 {
            if self.outlines.current_depth > self.opt.outline_open_depth {
                0
            } else {
                1
            }
        } else {
            is_open
        };
        let next =
            &mut *(new((1_u64).wrapping_mul(::std::mem::size_of::<pdf_olitem>() as u64) as u32)
                as *mut pdf_olitem);
        (*item).next = next;
        next.dict = ptr::null_mut();
        next.parent = (*item).parent;
        next.first = ptr::null_mut();
        next.is_open = -1;
        next.next = ptr::null_mut();
        self.outlines.current = item;
        self.add_goto((*dict).as_dict_mut());
    }

    unsafe fn close_bookmarks(&mut self) {
        let catalog: *mut pdf_obj = self.root.dict;
        let item = &mut *self.outlines.first;
        if !(*item).dict.is_null() {
            let bm_root = &mut *pdf_dict::new().into_obj();
            let bm_root_ref = pdf_new_ref(bm_root).into_obj();
            let count = flush_bookmarks(item, bm_root_ref, bm_root);
            bm_root.as_dict_mut().set("Count", count as f64);
            (*catalog).as_dict_mut().set("Outlines", bm_root_ref);
            pdf_release_obj(bm_root);
        }
        clean_bookmarks(item);
        self.outlines.first = ptr::null_mut();
        self.outlines.current = ptr::null_mut();
        self.outlines.current_depth = 0;
    }
}
const name_dict_categories: [&str; 10] = [
    "Dests",
    "AP",
    "JavaScript",
    "Pages",
    "Templates",
    "IDS",
    "URLS",
    "EmbeddedFiles",
    "AlternatePresentations",
    "Renditions",
];
impl PdfDoc {
    unsafe fn init_names(&mut self, check_gotos: i32) {
        self.root.names = ptr::null_mut();
        self.names = (0..10)
            .map(|i| name_dict {
                category: name_dict_categories[i],
                data: if name_dict_categories[i] != "Dests" {
                    ptr::null_mut()
                } else {
                    pdf_new_name_tree()
                },
                /*
                 * We need a non-null entry for PDF destinations in order to find
                 * broken links even if no destination is defined in the DVI file.
                 */
            })
            .collect();
        self.check_gotos = check_gotos;
        ht_init_table(
            &mut self.gotos,
            ::std::mem::transmute::<
                Option<unsafe fn(_: *mut pdf_obj) -> ()>,
                Option<unsafe fn(_: *mut libc::c_void) -> ()>,
            >(Some(pdf_release_obj as unsafe fn(_: *mut pdf_obj) -> ())),
        );
    }

    pub(crate) unsafe fn add_names(
        &mut self,
        category: &[u8],
        key: &[u8],
        value: &mut pdf_obj,
    ) -> Result<()> {
        let mut i = 0;
        for name in &self.names {
            if name.category.as_bytes() == category {
                break;
            }
            i += 1;
        }
        if i == self.names.len() {
            warn!(
                "Unknown name dictionary category \"{}\".",
                category.display()
            );
            return ERR;
        }
        if self.names[i].data.is_null() {
            self.names[i].data = pdf_new_name_tree()
        }
        pdf_names_add_object(&mut *self.names[i].data, key, value)
    }

    unsafe fn add_goto(&mut self, annot_dict: &mut pdf_dict) {
        if self.check_gotos == 0 {
            return;
        }
        /*
         * An annotation dictionary coming from an annotation special
         * must have a "Subtype". An annotation dictionary coming from
         * an outline special has none.
         */
        if let Some(subtype) = DerefObj::new(annot_dict.get_mut("Subtype")) {
            match &subtype.data {
                Object::Undefined => {
                    return warn_undefined();
                }
                Object::Name(n) if n.to_bytes() == b"Link" => {}
                Object::Name(_) => {
                    return;
                }
                _ => {
                    return warn_error();
                }
            }
        }

        let mut key = "Dest";
        let mut D = DerefObj::new(annot_dict.get_mut(key));
        match D.as_deref_mut() {
            Some(pdf_obj {
                data: Object::Undefined,
                ..
            }) => {
                warn_undefined();
                return;
            }
            _ => {
                if let Some(mut A) = DerefObj::new(annot_dict.get_mut("A")) {
                    if let Object::Undefined = A.data {
                        warn_undefined();
                        return;
                    } else if D.as_ref().is_some() {
                        warn_error();
                        return;
                    } else {
                        if let Object::Dict(a) = &mut A.data {
                            if let Some(S) = DerefObj::new(a.get_mut("S")) {
                                match &S.data {
                                    Object::Undefined => {
                                        warn_undefined();
                                        return;
                                    }
                                    Object::Name(n) if n.to_bytes() == b"GoTo" => {}
                                    Object::Name(_) => {
                                        return;
                                    }
                                    _ => {
                                        warn_error();
                                        return;
                                    }
                                }
                            } else {
                                warn_error();
                                return;
                            }

                            key = "D";
                            if let Some(D) = DerefObj::new(a.get_mut(key)).as_deref() {
                                self.replace(a, key, &D.data);
                            } else {
                                warn_error();
                            }
                        } else {
                            warn_error();
                            return;
                        }
                    }
                } else {
                    if let Some(D) = D.as_deref_mut() {
                        self.replace(annot_dict, key, &D.data);
                    } else {
                        warn_error();
                    }
                }
            }
        }

        fn warn_error() {
            warn!("Unknown PDF annotation format. Output file may be broken.");
        }

        fn warn_undefined() {
            warn!("Cannot optimize PDF annotations. Output file may be broken. Please restart with option \"-C 0x10\"\n");
        }
    }

    unsafe fn replace(&mut self, dict: &mut pdf_dict, key: &str, d: &Object) {
        match d {
            Object::String(s) => {
                let dest = s.to_bytes();

                let mut D_new = ht_lookup_table(&mut self.gotos, dest) as *mut pdf_obj;
                if D_new.is_null() {
                    /* We use hexadecimal notation for our numeric destinations.
                     * Other bases (e.g., 10+26 or 10+2*26) would be more efficient.
                     */
                    let buf = format!("{:x}", ht_table_size(&mut self.gotos));
                    /* Maybe reference */
                    D_new = pdf_string::new(buf).into_obj();
                    ht_append_table(&mut self.gotos, dest, D_new as *mut libc::c_void);
                }

                dict.set(key, pdf_link_obj(D_new));
            }
            Object::Array(_) => {}
            Object::Undefined => warn!("Cannot optimize PDF annotations. Output file may be broken. Please restart with option \"-C 0x10\"\n"),
            _ => warn!("Unknown PDF annotation format. Output file may be broken."),
        }
    }
}
unsafe fn warn_undef_dests(dests: *mut ht_table, gotos: *mut ht_table) {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: ptr::null_mut(),
        hash: ptr::null_mut(),
    };
    if ht_set_iter(gotos, &mut iter) < 0 {
        return;
    }
    loop {
        let key = iter.get_key();
        if ht_lookup_table(dests, key).is_null() {
            warn!("PDF destination \"{}\" not defined.", key.display());
        }
        if !(ht_iter_next(&mut iter) >= 0) {
            break;
        }
    }
    ht_clear_iter(&mut iter);
}
impl PdfDoc {
    unsafe fn close_names(&mut self) {
        for name in self.names.iter_mut() {
            let category = name.category;
            if !name.data.is_null() {
                let data: *mut ht_table = name.data;
                let name_tree;
                if self.check_gotos == 0 || category != "Dests" {
                    let (_name_tree, _) = pdf_names_create_tree(&mut *data, None);
                    name_tree = _name_tree;
                } else {
                    let (_name_tree, count) =
                        pdf_names_create_tree(&mut *data, Some(&mut self.gotos));
                    name_tree = _name_tree;
                    if verbose != 0 && count < (*data).count {
                        info!(
                            "\nRemoved {} unused PDF destinations\n",
                            (*data).count - count
                        );
                    }
                    if count < self.gotos.count {
                        warn_undef_dests(data, &mut self.gotos);
                    }
                }
                if let Some(name_tree) = name_tree {
                    if self.root.names.is_null() {
                        self.root.names = pdf_dict::new().into_obj();
                    }
                    (*self.root.names)
                        .as_dict_mut()
                        .set(category, name_tree.into_ref());
                }
                pdf_delete_name_tree(&mut name.data);
            }
        }
        if !self.root.names.is_null() {
            if let Some(tmp) = (*self.root.dict).as_dict().get("Names") {
                if let Object::Dict(tmp) = &tmp.data {
                    (*self.root.names).as_dict_mut().merge(tmp);
                    (*self.root.dict)
                        .as_dict_mut()
                        .set("Names", pdf_ref_obj(self.root.names));
                } else {
                    /* What should I do? */
                    warn!("Could not modify Names dictionary.");
                }
            } else {
                (*self.root.dict)
                    .as_dict_mut()
                    .set("Names", pdf_ref_obj(self.root.names));
            }
            pdf_release_obj(self.root.names);
            self.root.names = ptr::null_mut()
        }
        self.names = Vec::new();
        ht_clear_table(&mut self.gotos);
    }

    pub(crate) unsafe fn add_annot(
        &mut self,
        page_no: usize,
        rect: &Rect,
        annot_dict: *mut pdf_obj,
        new_annot: i32,
    ) {
        let annot_grow: f64 = self.opt.annot_grow;
        let page = self.get_page_entry(page_no);
        if page.annots.is_null() {
            page.annots = Vec::new().into_obj();
        }
        let mediabox = pdf_doc_mut().get_mediabox(page_no);
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
            self.add_goto((*annot_dict).as_dict_mut());
        };
    }
    /*
     * PDF Article Thread
     */
    unsafe fn init_articles(&mut self) {
        self.root.threads = ptr::null_mut();
        self.articles = Vec::new();
    }

    pub(crate) unsafe fn begin_article(&mut self, article_id: &str, article_info: *mut pdf_obj) {
        assert!(
            !article_id.is_empty(),
            "Article thread without internal identifier."
        );
        self.articles.push(pdf_article {
            id: article_id.to_string(),
            info: article_info,
            beads: Vec::new(),
        });
    }
}
unsafe fn find_bead<'a>(article: &'a mut pdf_article, bead_id: &str) -> Option<&'a mut pdf_bead> {
    for b in &mut article.beads {
        if b.id == bead_id {
            return Some(b);
        }
    }
    None
}

impl PdfDoc {
    pub(crate) unsafe fn add_bead(
        &mut self,
        article_id: &str,
        bead_id: &str,
        page_no: usize,
        rect: &Rect,
    ) {
        if article_id.is_empty() {
            panic!("No article identifier specified.");
        }
        let mut article = None;
        for a in &mut self.articles {
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
        &mut self,
        article_num: usize,
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
            self.articles[article_num].beads.len()
        };
        for i in 0..n {
            let bead = if !bead_ids.is_empty() {
                find_bead(&mut self.articles[article_num], &bead_ids[i])
            } else {
                Some(&mut self.articles[article_num].beads[i])
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
                    let bead_rect = bead.rect;
                    let bead_page_no = bead.page_no as usize;
                    let page = self.get_page_entry(bead_page_no);
                    if page.beads.is_null() {
                        page.beads = Vec::new().into_obj();
                    }
                    (*last).as_dict_mut().set("P", pdf_link_obj(page.page_ref));
                    let mut rect = vec![];
                    rect.push_obj((bead_rect.min.x / 0.01 + 0.5).floor() * 0.01);
                    rect.push_obj((bead_rect.min.y / 0.01 + 0.5).floor() * 0.01);
                    rect.push_obj((bead_rect.max.x / 0.01 + 0.5).floor() * 0.01);
                    rect.push_obj((bead_rect.max.y / 0.01 + 0.5).floor() * 0.01);
                    (*last).as_dict_mut().set("R", rect);
                    (*page.beads).as_array_mut().push(pdf_ref_obj(last));
                    prev = last
                }
                _ => {}
            }
        }
        if !first.is_null() && !last.is_null() {
            let article = &mut self.articles[article_num];
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

    unsafe fn close_articles(&mut self) {
        for an in 0..self.articles.len() {
            if !self.articles[an].beads.is_empty() {
                let art_dict = &mut *self.make_article(an, &[], ptr::null_mut());
                if self.root.threads.is_null() {
                    self.root.threads = Vec::new().into_obj();
                }
                (*self.root.threads)
                    .as_array_mut()
                    .push_obj(pdf_new_ref(art_dict));
                pdf_release_obj(art_dict);
            }
        }
        self.articles = Vec::new();
        if !self.root.threads.is_null() {
            (*self.root.dict)
                .as_dict_mut()
                .set("Threads", pdf_ref_obj(self.root.threads));
            pdf_release_obj(self.root.threads);
            self.root.threads = ptr::null_mut()
        };
    }

    /* page_no = 0 for root page tree node. */
    pub(crate) unsafe fn set_mediabox(&mut self, page_no: usize, mediabox: &Rect) {
        if page_no == 0 {
            self.pages.mediabox = *mediabox;
        } else {
            let page = self.get_page_entry(page_no);
            page.cropbox = *mediabox;
            page.flags |= 1 << 0
        };
    }
    unsafe fn get_mediabox(&mut self, page_no: usize) -> Rect {
        if page_no == 0 {
            self.pages.mediabox
        } else {
            let page = self.get_page_entry(page_no);
            if page.flags & 1 << 0 != 0 {
                page.cropbox
            } else {
                self.pages.mediabox
            }
        }
    }

    pub(crate) unsafe fn current_page_resources(&mut self) -> *mut pdf_obj {
        if !self.pending_forms.is_null() {
            if !(*self.pending_forms).form.resources.is_null() {
                (*self.pending_forms).form.resources
            } else {
                (*self.pending_forms).form.resources = pdf_dict::new().into_obj();
                (*self.pending_forms).form.resources
            }
        } else {
            let currentpage = &mut self.pages.entries[self.pages.num_entries];
            if !currentpage.resources.is_null() {
                currentpage.resources
            } else {
                currentpage.resources = pdf_dict::new().into_obj();
                currentpage.resources
            }
        }
    }

    pub(crate) unsafe fn get_dictionary(&mut self, category: &str) -> *mut pdf_obj {
        let dict = match category {
            "Names" => {
                if self.root.names.is_null() {
                    self.root.names = pdf_dict::new().into_obj();
                }
                self.root.names
            }
            "Pages" => {
                if self.root.pages.is_null() {
                    self.root.pages = pdf_dict::new().into_obj();
                }
                self.root.pages
            }
            "Catalog" => {
                if self.root.dict.is_null() {
                    self.root.dict = pdf_dict::new().into_obj();
                }
                self.root.dict
            }
            "Info" => {
                if self.info.is_null() {
                    self.info = pdf_dict::new().into_obj();
                }
                self.info
            }
            "@THISPAGE" => {
                /* Sorry for this... */
                let currentpage = &mut self.pages.entries[self.pages.num_entries];
                currentpage.page_obj
            }
            _ => ptr::null_mut(),
        };
        if dict.is_null() {
            panic!("Document dict. \"{}\" not exist. ", category);
        }
        dict
    }

    pub(crate) fn current_page_number(&self) -> usize {
        self.pages.num_entries + 1
    }

    pub(crate) unsafe fn ref_page(&mut self, page_no: usize) -> *mut pdf_obj {
        let page = self.get_page_entry(page_no);
        if page.page_obj.is_null() {
            page.page_obj = pdf_dict::new().into_obj();
            page.page_ref = pdf_new_ref(&mut *page.page_obj).into_obj();
        }
        pdf_link_obj(page.page_ref)
    }

    pub(crate) unsafe fn get_reference(&mut self, category: &str) -> *mut pdf_obj {
        let page_no = self.current_page_number();
        let ref_0 = match category {
            "@THISPAGE" => self.ref_page(page_no),
            "@PREVPAGE" => {
                if page_no <= 1 {
                    panic!("Reference to previous page, but no pages have been completed yet.");
                }
                self.ref_page(page_no - 1)
            }
            "@NEXTPAGE" => self.ref_page(page_no + 1),
            _ => ptr::null_mut(),
        };
        if ref_0.is_null() {
            panic!("Reference to \"{}\" not exist. ", category);
        }
        ref_0
    }

    unsafe fn new_page(&mut self) {
        if self.pages.num_entries >= self.pages.entries.len() as _ {
            self.resize_page_entries(self.pages.num_entries + 1);
        }
        /*
         * This is confusing. pdf_doc_finish_page() have increased page count!
         */
        let currentpage = &mut self.pages.entries[self.pages.num_entries];
        /* Was this page already instantiated by a forward reference to it? */
        if currentpage.page_ref.is_null() {
            currentpage.page_obj = pdf_dict::new().into_obj();
            currentpage.page_ref = pdf_new_ref(&mut *currentpage.page_obj).into_obj();
        }
        currentpage.background = ptr::null_mut();
        currentpage.contents = pdf_stream::new(STREAM_COMPRESS).into_obj();
        currentpage.resources = pdf_dict::new().into_obj();
        currentpage.annots = ptr::null_mut();
        currentpage.beads = ptr::null_mut();
    }

    /* This only closes contents and resources. */
    unsafe fn finish_page(&mut self) {
        if !self.pending_forms.is_null() {
            panic!("A pending form XObject at the end of page.");
        }
        let currentpage = &mut self.pages.entries[self.pages.num_entries];
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
        if !self.pages.bop.is_null() && (*self.pages.bop).as_stream().len() > 0 {
            currentpage.content_refs[0] = pdf_ref_obj(self.pages.bop)
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
        if !self.pages.eop.is_null() && (*self.pages.eop).as_stream().len() > 0 {
            currentpage.content_refs[3] = pdf_ref_obj(self.pages.eop)
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
                (self.pages.num_entries % 99999) as i64 + 1
            );
            let thumb_ref = read_thumbnail(&thumb_filename);
            if !thumb_ref.is_null() {
                (*currentpage.page_obj)
                    .as_dict_mut()
                    .set("Thumb", thumb_ref);
            }
        }
        self.pages.num_entries += 1;
    }
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
impl PdfDoc {
    unsafe fn fill_page_background(&mut self) {
        let cm = pdf_dev_get_param(2);
        if cm == 0 || bgcolor.is_white() {
            return;
        }
        let r = pdf_doc_mut().get_mediabox(pdf_doc().current_page_number());
        let currentpage = &mut self.pages.entries[self.pages.num_entries];
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

    pub(crate) unsafe fn begin_page(&mut self, scale: f64, x_origin: f64, y_origin: f64) {
        let mut M = TMatrix::row_major(scale, 0., 0., scale, x_origin, y_origin);
        /* pdf_doc_new_page() allocates page content stream. */
        self.new_page();
        pdf_dev_bop(&mut M);
    }

    pub(crate) unsafe fn end_page(&mut self) {
        pdf_dev_eop();
        self.fill_page_background();
        self.finish_page();
    }

    pub(crate) unsafe fn add_page_content(&mut self, buffer: &[u8]) {
        if !self.pending_forms.is_null() {
            (*(*self.pending_forms).form.contents)
                .as_stream_mut()
                .add_slice(&buffer);
        } else {
            let currentpage = &mut self.pages.entries[self.pages.num_entries];
            (*currentpage.contents).as_stream_mut().add_slice(&buffer);
        };
    }

    pub(crate) unsafe fn open_document(
        &mut self,
        filename: &str,
        enable_encrypt: bool,
        enable_object_stream: bool,
        media_width: f64,
        media_height: f64,
        annot_grow_amount: f64,
        bookmark_open_depth: i32,
        check_gotos: i32,
    ) {
        pdf_out_init(filename, enable_encrypt, enable_object_stream);
        self.init_catalog();
        self.opt.annot_grow = annot_grow_amount;
        self.opt.outline_open_depth = bookmark_open_depth;
        pdf_init_resources();
        pdf_init_colors();
        pdf_init_fonts();
        /* Thumbnail want this to be initialized... */
        pdf_init_images();
        self.init_docinfo();
        if !doccreator.is_empty() {
            (*self.info)
                .as_dict_mut()
                .set("Creator", pdf_string::new(&doccreator));
            doccreator = Vec::new();
        }
        self.init_bookmarks(bookmark_open_depth);
        self.init_articles();
        self.init_names(check_gotos);
        self.init_page_tree(media_width, media_height);
        pdf_doc_set_bgcolor(None);
        if enable_encrypt {
            let encrypt = pdf_encrypt_obj();
            pdf_set_encrypt(encrypt);
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
        self.pending_forms = ptr::null_mut();
    }

    pub(crate) unsafe fn close_document(&mut self) {
        /*
         * Following things were kept around so user can add dictionary items.
         */
        self.close_articles(); /* Should be at last. */
        self.close_names();
        self.close_bookmarks();
        self.close_page_tree();
        self.close_docinfo();
        self.close_catalog();
        pdf_close_images();
        pdf_close_fonts();
        pdf_close_colors();
        pdf_close_resources();
        pdf_out_flush();
        thumb_basename = String::new();
    }
}

static mut doccreator: Vec<u8> = Vec::new();
/* Ugh */

pub(crate) unsafe fn pdf_doc_set_creator(creator: &[u8]) {
    if creator.is_empty() {
        return;
    }
    doccreator = creator.to_owned();
    /* Ugh */
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
impl PdfDoc {
    /*
     * begin_form_xobj creates an xobject with its "origin" at
     * xpos and ypos that is clipped to the specified bbox. Note
     * that the origin is not the lower left corner of the bbox.
     */
    pub(crate) unsafe fn begin_grabbing(
        &mut self,
        ident: &str,
        ref_x: f64,
        ref_y: f64,
        cropbox: &Rect,
    ) -> i32 {
        pdf_dev_push_gstate();
        let mut fnode =
            new((1_u64).wrapping_mul(::std::mem::size_of::<form_list_node>() as u64) as u32)
                as *mut form_list_node;
        (*fnode).prev = self.pending_forms;
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
        let mut info = Box::new(xform_info::new());
        info.matrix = TMatrix::create_translation(-ref_x, -ref_y);
        info.bbox = *cropbox;
        /* Use reference since content itself isn't available yet. */
        let xobj_id = pdf_ximage_defineresource(
            ident,
            XInfo::Form(info),
            pdf_new_ref(&mut *(*form).contents).into_obj(),
        );
        self.pending_forms = fnode;
        /*
         * Make sure the object is self-contained by adding the
         * current font and color to the object stream.
         */
        pdf_dev_reset_fonts(1); /* force color operators to be added to stream */
        pdf_dev_reset_color(1);
        xobj_id
    }

    pub(crate) unsafe fn end_grabbing(&mut self, attrib: *mut pdf_obj) {
        if self.pending_forms.is_null() {
            warn!("Tried to close a nonexistent form XOject.");
            return;
        }
        let fnode = self.pending_forms;
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
        self.pending_forms = (*fnode).prev;
        pdf_dev_pop_gstate();
        pdf_dev_reset_fonts(1);
        pdf_dev_reset_color(0);
        free(fnode as *mut libc::c_void);
    }
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
    breaking_state.dirty = 0;
}

pub(crate) unsafe fn pdf_doc_begin_annot(dict: *mut pdf_obj) {
    breaking_state.annot_dict = dict;
    breaking_state.broken = 0;
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
        let p = pdf_doc_mut();
        p.add_annot(
            p.current_page_number(),
            &mut breaking_state.rect,
            annot_dict,
            (breaking_state.broken == 0) as i32,
        );
        pdf_release_obj(annot_dict);
        breaking_state.broken = 1
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
