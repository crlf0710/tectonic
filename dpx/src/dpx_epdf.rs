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
)]

use std::ptr;

use crate::warn;

use super::dpx_pdfdoc::pdf_doc_get_page;
use super::dpx_pdfximage::{pdf_ximage_init_form_info, pdf_ximage_set_form};
use crate::dpx_pdfobj::{
    pdf_close, pdf_concat_stream, pdf_deref_obj, pdf_file_get_catalog, pdf_file_get_version,
    pdf_get_version, pdf_import_object, pdf_obj, pdf_open, pdf_release_obj, pdf_stream, IntoObj,
    PushObj, STREAM_COMPRESS,
};
pub type __off_t = i64;
pub type __off64_t = i64;
use bridge::InputHandleWrapper;

use crate::dpx_pdfximage::{load_options, pdf_ximage, xform_info};
pub const OP_CURVETO2: C2RustUnnamed_0 = 15;
pub const OP_CURVETO1: C2RustUnnamed_0 = 14;
pub const OP_GRESTORE: C2RustUnnamed_0 = 13;
pub const OP_GSAVE: C2RustUnnamed_0 = 12;
pub const OP_NOOP: C2RustUnnamed_0 = 11;
pub const OP_MOVETO: C2RustUnnamed_0 = 10;
pub const OP_LINETO: C2RustUnnamed_0 = 9;
pub const OP_CLOSEPATH: C2RustUnnamed_0 = 8;
pub const OP_CURVETO: C2RustUnnamed_0 = 7;
pub const OP_RECTANGLE: C2RustUnnamed_0 = 6;
pub const OP_SETCOLORSPACE: C2RustUnnamed_0 = 5;
pub const OP_CONCATMATRIX: C2RustUnnamed_0 = 4;
pub const OP_CLIP: C2RustUnnamed_0 = 3;
pub const OP_CLOSEandCLIP: C2RustUnnamed_0 = 2;
pub const OP_SETCOLOR: C2RustUnnamed_0 = 1;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct operator {
    pub token: *const i8,
    pub opcode: i32,
}
pub type C2RustUnnamed_0 = u32;
pub const OP_UNKNOWN: C2RustUnnamed_0 = 16;
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* ximage here is the result. DONT USE IT FOR PASSING OPTIONS! */

pub unsafe fn pdf_include_page(
    ximage: *mut pdf_ximage,
    handle: InputHandleWrapper,
    ident: *const i8,
    mut options: load_options,
) -> i32 {
    let mut contents: *mut pdf_obj = ptr::null_mut();
    let mut resources: *mut pdf_obj = ptr::null_mut();
    let mut markinfo: *mut pdf_obj = ptr::null_mut();
    let pf = pdf_open(ident, handle);
    if pf.is_null() {
        return -1;
    }
    if pdf_file_get_version(pf) > pdf_get_version() {
        warn!(
            "Trying to include PDF file which has newer version number than output PDF: 1.{}.",
            pdf_get_version()
        );
    }
    if options.page_no == 0i32 {
        options.page_no = 1i32
    }

    let error_silent = move || {
        pdf_release_obj(resources);
        pdf_release_obj(markinfo);
        pdf_release_obj(contents);
        pdf_close(pf);
    };
    let error = || {
        warn!("Cannot parse document. Broken PDF file?");
        error_silent();
    };

    if let Some((page, bbox, matrix)) =
        pdf_doc_get_page(pf, options.page_no, options.bbox_type, &mut resources)
    {
        let mut info = xform_info::default();
        pdf_ximage_init_form_info(&mut info);
        info.bbox = bbox;
        info.matrix = matrix;
        let catalog = pdf_file_get_catalog(pf);
        markinfo = pdf_deref_obj((*catalog).as_dict_mut().get_mut("MarkInfo"));
        if !markinfo.is_null() {
            let tmp: *mut pdf_obj = pdf_deref_obj((*markinfo).as_dict_mut().get_mut("Marked"));
            pdf_release_obj(markinfo);
            if tmp.is_null() || !(*tmp).is_bool() {
                pdf_release_obj(tmp);
                pdf_release_obj(page);
                error();
                return -1;
            } else if (*tmp).as_bool() {
                warn!("PDF file is tagged... Ignoring tags.");
            }
            pdf_release_obj(tmp);
        }

        contents = pdf_deref_obj((*page).as_dict_mut().get_mut("Contents"));
        pdf_release_obj(page);
        /*
         * Handle page content stream.
         */
        let content_new = if contents.is_null() {
            /*
             * Empty page
             */
            pdf_stream::new(0i32).into_obj()
        /* TODO: better don't include anything if the page is empty */
        } else if !contents.is_null() && (*contents).is_stream() {
            /*
             * We must import the stream because its dictionary
             * may contain indirect references.
             */
            pdf_import_object(contents)
        } else if !contents.is_null() && (*contents).is_array() {
            /*
             * Concatenate all content streams.
             */
            let len = (*contents).as_array().len();
            let mut content_new = pdf_stream::new(STREAM_COMPRESS);
            for idx in 0..len {
                let array = (*contents).as_array_mut();
                let content_seg = if idx < array.len() {
                    pdf_deref_obj(Some(&mut *array[idx]))
                } else {
                    0 as *mut pdf_obj
                };
                if content_seg.is_null()
                    || !(*content_seg).is_stream()
                    || pdf_concat_stream(&mut content_new, (*content_seg).as_stream_mut()) < 0
                {
                    pdf_release_obj(content_seg);
                    error();
                    return -1;
                }
                pdf_release_obj(content_seg);
            }
            content_new.into_obj()
        } else {
            error();
            return -1;
        };

        pdf_release_obj(contents);
        contents = content_new;

        /*
         * Add entries to contents stream dictionary.
         */
        let contents_dict = (*contents).as_stream_mut().get_dict_mut();
        contents_dict.set("Type", "XObject");
        contents_dict.set("Subtype", "Form");
        contents_dict.set("FormType", 1_f64);
        let mut bbox = vec![];
        bbox.push_obj(info.bbox.min.x);
        bbox.push_obj(info.bbox.min.y);
        bbox.push_obj(info.bbox.max.x);
        bbox.push_obj(info.bbox.max.y);
        contents_dict.set("BBox", bbox);
        let mut matrix = vec![];
        for &val in &info.matrix.to_row_major_array() {
            matrix.push_obj(val);
        }
        contents_dict.set("Matrix", matrix);
        contents_dict.set("Resources", pdf_import_object(resources));
        pdf_release_obj(resources);

        pdf_close(pf);

        pdf_ximage_set_form(ximage, &mut info, contents);

        0
    } else {
        error_silent();
        return -1;
    }
}
