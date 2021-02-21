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

use bridge::InFile;
use std::ptr;

use crate::warn;

use super::dpx_pdfdoc::pdf_doc_get_page;
use super::dpx_pdfximage::{pdf_ximage_init_form_info, pdf_ximage_set_form};
use crate::dpx_pdfobj::{
    pdf_concat_stream, pdf_file_get_catalog, pdf_file_get_version, pdf_get_version,
    pdf_import_object, pdf_obj, pdf_open, pdf_release_obj, pdf_stream, DerefObj, IntoObj, Object,
    PushObj, STREAM_COMPRESS,
};
pub(crate) type __off_t = i64;
pub(crate) type __off64_t = i64;

use crate::dpx_pdfximage::{load_options, pdf_ximage, xform_info};

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct operator {
    pub(crate) token: *const i8,
    pub(crate) opcode: i32,
}

/* ximage here is the result. DONT USE IT FOR PASSING OPTIONS! */

pub(crate) unsafe fn pdf_include_page(
    ximage: &mut pdf_ximage,
    handle: InFile,
    ident: &str,
    mut options: load_options,
) -> i32 {
    let pf = pdf_open(ident, handle);
    if pf.is_none() {
        return -1;
    }
    let pf = pf.unwrap();
    if pdf_file_get_version(pf) > pdf_get_version() {
        warn!(
            "Trying to include PDF file which has newer version number than output PDF: 1.{}.",
            pdf_get_version()
        );
    }
    if options.page_no == 0 {
        options.page_no = 1
    }

    let mut resources: *mut pdf_obj = ptr::null_mut();
    let error_silent = move || -> i32 {
        pdf_release_obj(resources);
        -1
    };
    let error = || -> i32 {
        warn!("Cannot parse document. Broken PDF file?");
        error_silent()
    };

    if let Some((mut page, bbox, matrix)) =
        pdf_doc_get_page(pf, options.page_no, options.bbox_type, &mut resources)
    {
        let mut info = xform_info::default();
        pdf_ximage_init_form_info(&mut info);
        info.bbox = bbox;
        info.matrix = matrix;
        let catalog = pdf_file_get_catalog(pf);
        if let Some(mut markinfo) = DerefObj::new((*catalog).as_dict_mut().get_mut("MarkInfo")) {
            let tmp = DerefObj::new(markinfo.as_dict_mut().get_mut("Marked"));
            if let Some(tmp) = tmp {
                if let Object::Boolean(b) = tmp.data {
                    if b {
                        warn!("PDF file is tagged... Ignoring tags.");
                    }
                } else {
                    return error();
                }
            } else {
                return error();
            }
        }

        let contents = DerefObj::new(page.as_dict_mut().get_mut("Contents"));
        /*
         * Handle page content stream.
         */
        let content_new = if let Some(mut contents) = contents {
            /* TODO: better don't include anything if the page is empty */
            match &mut contents.data {
                Object::Stream(_) => {
                    /*
                     * We must import the stream because its dictionary
                     * may contain indirect references.
                     */
                    use std::ops::DerefMut;
                    pdf_import_object(contents.deref_mut())
                }
                Object::Array(array) => {
                    /*
                     * Concatenate all content streams.
                     */
                    let len = array.len();
                    let mut content_new = pdf_stream::new(STREAM_COMPRESS);
                    for idx in 0..len {
                        if let Some(mut content_seg) = if idx < array.len() {
                            DerefObj::new(Some(&mut *array[idx]))
                        } else {
                            None
                        } {
                            if let Object::Stream(s) = &mut content_seg.data {
                                if pdf_concat_stream(&mut content_new, s) >= 0 {
                                } else {
                                    return error();
                                }
                            } else {
                                return error();
                            }
                        } else {
                            return error();
                        }
                    }
                    content_new.into_obj()
                }
                _ => {
                    return error();
                }
            }
        } else {
            /*
             * Empty page
             */
            pdf_stream::new(0).into_obj()
        };
        let contents = content_new;

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

        pdf_ximage_set_form(ximage, &mut info, contents);

        0
    } else {
        error_silent()
    }
}
