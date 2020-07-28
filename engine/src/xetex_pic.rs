#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::core_memory::xstrdup;
use crate::help;
use crate::xetex_consts::{Picture, WhatsItNST};
use crate::xetex_errors::error;
use crate::xetex_ext::{D2Fix, Fix2D};
use crate::xetex_ini::memory_word;
use crate::xetex_ini::{
    cur_area, cur_ext, cur_list, cur_name, cur_val, file_line_error_style_p, name_of_file,
};
use crate::xetex_output::{
    print, print_cstr, print_file_line, print_file_name, print_nl_cstr, print_scaled,
};
use crate::xetex_xetex0::{
    new_whatsit, pack_file_name, scan_decimal, scan_dimen, scan_file_name, scan_int, scan_keyword,
};

use bridge::InputHandleWrapper;
use bridge::TTInputFormat;
use bridge::{ttstub_input_close, ttstub_input_open};
use dpx::pdf_dev_transform;
use dpx::Corner;
use dpx::{bmp_get_bbox, check_for_bmp};
use dpx::{check_for_jpeg, jpeg_get_bbox};
use dpx::{check_for_png, png_get_bbox};
use dpx::{pdf_close, pdf_file, pdf_obj, pdf_open, pdf_release_obj};
use dpx::{pdf_doc_get_page, pdf_doc_get_page_count};
use libc::{free, strlen};
pub type scaled_t = i32;
pub type Fixed = scaled_t;
pub type str_number = i32;

use euclid::{point2, size2, Angle};
type Transform = euclid::Transform2D<f64, (), ()>;
type Point = euclid::Point2D<f32, ()>;
type Rect = euclid::Rect<f32, ()>;

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

pub(crate) unsafe fn count_pdf_file_pages() -> i32 {
    let handle = ttstub_input_open(name_of_file, TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        return 0;
    }
    let pf = pdf_open(name_of_file, handle.unwrap());
    if pf.is_null() {
        /* TODO: issue warning */
        //ttstub_input_close(handle);
        return 0;
    }
    let pages = pdf_doc_get_page_count(pf);
    pdf_close(pf);
    pages
}
unsafe fn pdf_get_rect(
    mut filename: *mut i8,
    handle: InputHandleWrapper,
    mut page_num: i32,
    mut pdf_box: i32,
) -> Result<Rect, ()> {
    let mut pages: i32 = 0;
    let mut dpx_options: i32 = 0;
    let mut pf: *mut pdf_file = 0 as *mut pdf_file;
    pf = pdf_open(filename, handle);
    if pf.is_null() {
        /* TODO: issue warning */
        return Err(());
    }
    pages = pdf_doc_get_page_count(pf);
    if page_num > pages {
        page_num = pages
    }
    if page_num < 0i32 {
        page_num = pages + 1i32 + page_num
    }
    if page_num < 1i32 {
        page_num = 1i32
    }
    /* OMG, magic numbers specifying page bound types do not agree between
     * xdvipdfmx code (dpx-pdfdoc.c:pdf_doc_get_page) and XeTeX/Apple's
     * pdfbox_* definitions (xetex-ext.h). */
    match pdf_box {
        2 => dpx_options = 2i32,
        3 => dpx_options = 5i32,
        4 => dpx_options = 4i32,
        5 => dpx_options = 3i32,
        1 | _ => dpx_options = 1i32,
    }
    if let Some((page, mut bbox, matrix)) =
        pdf_doc_get_page(pf, page_num, dpx_options, 0 as *mut *mut pdf_obj)
    {
        pdf_close(pf);
        pdf_release_obj(page);
        /* Image's attribute "bbox" here is affected by /Rotate entry of included
         * PDF page.
         */
        let mut p1 = bbox.lower_left();
        pdf_dev_transform(&mut p1, Some(&matrix));
        let mut p2 = bbox.lower_right();
        pdf_dev_transform(&mut p2, Some(&matrix));
        let mut p3 = bbox.upper_right();
        pdf_dev_transform(&mut p3, Some(&matrix));
        let mut p4 = bbox.upper_left();
        pdf_dev_transform(&mut p4, Some(&matrix));
        bbox.min.x = p1.x.min(p2.x).min(p3.x).min(p4.x);
        bbox.min.y = p1.y.min(p2.y).min(p3.y).min(p4.y);
        bbox.max.x = p1.x.max(p2.x).max(p3.x).max(p4.x);
        bbox.max.y = p1.y.max(p2.y).max(p3.y).max(p4.y);
        Ok(Rect::new(
            point2(
                (72.27 / 72. * bbox.min.x) as f32,
                (72.27 / 72. * bbox.min.y) as f32,
            ),
            size2(
                (72.27 / 72. * bbox.size().width) as f32,
                (72.27 / 72. * bbox.size().height) as f32,
            ),
        ))
    } else {
        pdf_close(pf);
        /* TODO: issue warning */
        Err(())
    }
}
unsafe fn get_image_size_in_inches(handle: &mut InputHandleWrapper) -> Result<(f32, f32), i32> {
    let (width_pix, height_pix, xdensity, ydensity) = if check_for_jpeg(handle) != 0 {
        jpeg_get_bbox(handle).map_err(|_| -1)?
    } else if check_for_bmp(handle) {
        bmp_get_bbox(handle).map_err(|_| -1)?
    } else if check_for_png(handle) != 0 {
        png_get_bbox(handle).map_err(|_| -1)?
    } else {
        return Err(1);
    };
    /* xdvipdfmx defines density = 72 / dpi, so ... */
    Ok((
        (width_pix as f64 * xdensity / 72.) as f32,
        (height_pix as f64 * ydensity / 72.) as f32,
    ))
}
/*
  pdfBoxType indicates which pdf bounding box to use (0 for \XeTeXpicfile)
  page indicates which page is wanted (0-based)
  return 0 for success, or non-zero error code for failure
  return full path in *path
  return bounds (tex points) in *bounds
*/
unsafe fn find_pic_file(mut pdfBoxType: i32, mut page: i32) -> Result<(Rect, *mut i8), i32> {
    let handle = ttstub_input_open(name_of_file, TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        return Err(1);
    }
    let mut handle = handle.unwrap();
    let bounds = if pdfBoxType != 0i32 {
        /* if cmd was \XeTeXpdffile, use xpdflib to read it */
        pdf_get_rect(name_of_file, handle, page, pdfBoxType).map_err(|_| -1)?
    } else {
        match get_image_size_in_inches(&mut handle) {
            Ok((wd, ht)) => {
                ttstub_input_close(handle);
                Rect::from_size(size2(
                    (wd as f64 * 72.27) as f32,
                    (ht as f64 * 72.27) as f32,
                ))
            }
            Err(e) => return Err(e),
        }
    };
    Ok((bounds, xstrdup(name_of_file)))
}

fn to_points(r: &Rect) -> [Point; 4] {
    [
        r.min(),
        point2(r.min_x(), r.max_y()),
        r.max(),
        point2(r.max_x(), r.min_y()),
    ]
}

pub(crate) unsafe fn load_picture(mut is_pdf: bool) {
    let mut check_keywords: bool = false;
    let mut page: i32 = 0;
    let mut pdf_box_type: i32 = 0;
    let mut result: i32 = 0;
    scan_file_name();
    pack_file_name(cur_name, cur_area, cur_ext);
    pdf_box_type = 0i32;
    page = 0i32;
    if is_pdf {
        if scan_keyword(b"page") {
            scan_int();
            page = cur_val
        }
        pdf_box_type = 6i32;
        if scan_keyword(b"crop") {
            pdf_box_type = 1i32
        } else if scan_keyword(b"media") {
            pdf_box_type = 2i32
        } else if scan_keyword(b"bleed") {
            pdf_box_type = 3i32
        } else if scan_keyword(b"trim") {
            pdf_box_type = 4i32
        } else if scan_keyword(b"art") {
            pdf_box_type = 5i32
        }
    }
    let (bounds, pic_path) = if pdf_box_type == 6i32 {
        find_pic_file(1i32, page)
    } else {
        find_pic_file(pdf_box_type, page)
    }
    .unwrap_or_else(|e| {
        result = e;
        (Rect::zero(), std::ptr::null_mut())
    });
    let mut corners = bounds;
    let mut x_size_req = 0_f64;
    let mut y_size_req = 0_f64;
    let mut t = Transform::identity();
    check_keywords = true;
    while check_keywords {
        if scan_keyword(b"scaled") {
            scan_int();
            if x_size_req == 0. && y_size_req == 0. {
                let t2 = Transform::create_scale(cur_val as f64 / 1000., cur_val as f64 / 1000.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(b"xscaled") {
            scan_int();
            if x_size_req == 0. && y_size_req == 0. {
                let t2 = Transform::create_scale(cur_val as f64 / 1000., 1.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(b"yscaled") {
            scan_int();
            if x_size_req == 0.0f64 && y_size_req == 0.0f64 {
                let t2 = Transform::create_scale(1., cur_val as f64 / 1000.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(b"width") {
            scan_dimen(false, false, false);
            if cur_val <= 0i32 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Improper image ");
                print_cstr(b"size (");
                print_scaled(cur_val);
                print_cstr(b"pt) will be ignored");
                help!(
                    b"I can\'t scale images to zero or negative sizes,",
                    b"so I\'m ignoring this."
                );
                error();
            } else {
                x_size_req = Fix2D(cur_val)
            }
        } else if scan_keyword(b"height") {
            scan_dimen(false, false, false);
            if cur_val <= 0i32 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Improper image ");
                print_cstr(b"size (");
                print_scaled(cur_val);
                print_cstr(b"pt) will be ignored");
                help!(
                    b"I can\'t scale images to zero or negative sizes,",
                    b"so I\'m ignoring this."
                );
                error();
            } else {
                y_size_req = Fix2D(cur_val)
            }
        } else if scan_keyword(b"rotated") {
            scan_decimal();
            if x_size_req != 0.0f64 || y_size_req != 0.0f64 {
                let brect = Rect::from_points(&to_points(&corners));
                let xmin = brect.min_x() as f64;
                let ymin = brect.min_y() as f64;
                let xmax = brect.max_x() as f64;
                let ymax = brect.max_y() as f64;
                let mut t2 = if x_size_req == 0. {
                    Transform::create_scale(y_size_req / (ymax - ymin), y_size_req / (ymax - ymin))
                } else if y_size_req == 0.0f64 {
                    Transform::create_scale(x_size_req / (xmax - xmin), x_size_req / (xmax - xmin))
                } else {
                    Transform::create_scale(x_size_req / (xmax - xmin), y_size_req / (ymax - ymin))
                };

                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                x_size_req = 0.0f64;
                y_size_req = 0.0f64;
                t = t.post_transform(&t2);
            }
            let mut t2 = Transform::create_rotation(Angle::degrees(Fix2D(cur_val)));

            corners = t2.transform_rect(&corners.to_f64()).to_f32();
            corners = Rect::from_points(&to_points(&corners));
            t = t.post_transform(&t2);
        } else {
            check_keywords = false
        }
    }
    if x_size_req != 0.0f64 || y_size_req != 0.0f64 {
        let brect = Rect::from_points(&to_points(&corners));
        let xmin = brect.min_x() as f64;
        let ymin = brect.min_y() as f64;
        let xmax = brect.max_x() as f64;
        let ymax = brect.max_y() as f64;
        let mut t2 = if x_size_req == 0.0f64 {
            Transform::create_scale(y_size_req / (ymax - ymin), y_size_req / (ymax - ymin))
        } else if y_size_req == 0.0f64 {
            Transform::create_scale(x_size_req / (xmax - xmin), x_size_req / (xmax - xmin))
        } else {
            Transform::create_scale(x_size_req / (xmax - xmin), y_size_req / (ymax - ymin))
        };

        corners = t2.transform_rect(&corners.to_f64()).to_f32();
        x_size_req = 0.0f64;
        y_size_req = 0.0f64;
        t = t.post_transform(&t2);
    }

    let brect = Rect::from_points(&to_points(&corners));
    let xmin = brect.min_x() as f64;
    let ymin = brect.min_y() as f64;
    let xmax = brect.max_x() as f64;
    let ymax = brect.max_y() as f64;

    let mut t2 = Transform::create_translation(
        (-(xmin as i32) * 72i32) as f64 / 72.27,
        (-(ymin as i32) * 72i32) as f64 / 72.27,
    );
    t = t.post_transform(&t2);
    if result == 0 {
        let len = strlen(pic_path);
        new_whatsit(
            WhatsItNST::Pic,
            (crate::xetex_consts::PIC_NODE_SIZE as usize).wrapping_add(
                len.wrapping_add(::std::mem::size_of::<memory_word>())
                    .wrapping_sub(1)
                    .wrapping_div(::std::mem::size_of::<memory_word>()),
            ) as i16,
        );
        let mut tail_pic = Picture::from(cur_list.tail);
        if is_pdf {
            tail_pic.set_pdf();
        }
        tail_pic
            .set_path_len(len as u16)
            .set_page(page as u16)
            .set_pagebox(pdf_box_type as u16);
        tail_pic
            .set_width(D2Fix(xmax - xmin))
            .set_height(D2Fix(ymax - ymin))
            .set_depth(0);
        tail_pic.set_transform_matrix([
            D2Fix(t.m11),
            D2Fix(t.m12),
            D2Fix(t.m21),
            D2Fix(t.m22),
            D2Fix(t.m31),
            D2Fix(t.m32),
        ]);

        let slice = std::slice::from_raw_parts(pic_path as *const u8, len as usize);
        tail_pic.path_mut().copy_from_slice(&slice);

        free(pic_path as *mut libc::c_void);
    } else {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Unable to load picture or PDF file \'");
        print_file_name(cur_name, cur_area, cur_ext);
        print('\'' as i32);
        if result == -43i32 {
            help!(
                b"The requested image couldn\'t be read because",
                b"the file was not found."
            );
        } else {
            help!(
                b"The requested image couldn\'t be read because",
                b"it was not a recognized image format."
            );
        }
        error();
    };
}
