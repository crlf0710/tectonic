#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::t_eprint;
use std::ffi::CString;
use std::ptr;

use crate::help;
use crate::node::Picture;
use crate::xetex_errors::error;
use crate::xetex_ext::{D2Fix, Fix2D};
use crate::xetex_ini::{cur_list, input_state_t};
use crate::xetex_scaledmath::Scaled;
use crate::xetex_xetex0::{scan_decimal, scan_dimen, scan_file_name, scan_int, scan_keyword};
use crate::xetex_xetexd::{LLIST_link, TeXInt};

use bridge::{InFile, TTInputFormat};
use dpx::pdf_dev_transform;
use dpx::pdf_open;
use dpx::Corner;
use dpx::{bmp_get_bbox, check_for_bmp};
use dpx::{check_for_jpeg, jpeg_get_bbox};
use dpx::{check_for_png, png_get_bbox};
use dpx::{pdf_doc_get_page, pdf_doc_get_page_count};

use euclid::{point2, size2, Angle};
type Transform = euclid::Transform2D<f64, (), ()>;
type Point = euclid::Point2D<f32, ()>;
type Rect = euclid::Rect<f32, ()>;

pub(crate) unsafe fn count_pdf_file_pages(filename: &str) -> i32 {
    let handle = InFile::open(filename, TTInputFormat::PICT, 0);
    if handle.is_none() {
        return 0;
    }
    if let Some(pf) = pdf_open(filename, handle.unwrap()) {
        pdf_doc_get_page_count(&*pf)
    } else {
        /* TODO: issue warning */
        0
    }
}
unsafe fn pdf_get_rect(
    filename: *const i8,
    handle: InFile,
    mut page_num: i32,
    pdf_box: i32,
) -> Result<Rect, ()> {
    let pf = pdf_open(crate::c_pointer_to_str(filename), handle);
    if pf.is_none() {
        /* TODO: issue warning */
        return Err(());
    }
    let pf = pf.unwrap();
    let pages = pdf_doc_get_page_count(pf);
    if page_num > pages {
        page_num = pages
    }
    if page_num < 0 {
        page_num += pages + 1;
    }
    if page_num < 1 {
        page_num = 1
    }
    /* OMG, magic numbers specifying page bound types do not agree between
     * xdvipdfmx code (dpx-pdfdoc.c:pdf_doc_get_page) and XeTeX/Apple's
     * pdfbox_* definitions (xetex-ext.h). */
    let dpx_options = match pdf_box {
        2 => 2,
        3 => 5,
        4 => 4,
        5 => 3,
        _ => 1,
    };
    if let Some((_, mut bbox, matrix)) =
        pdf_doc_get_page(pf, page_num, dpx_options, ptr::null_mut())
    {
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
        /* TODO: issue warning */
        Err(())
    }
}
unsafe fn get_image_size_in_inches(handle: &mut InFile) -> Result<(f32, f32), i32> {
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
unsafe fn find_pic_file(filename: &str, pdfBoxType: i32, page: i32) -> Result<(Rect, String), i32> {
    let handle = InFile::open(filename, TTInputFormat::PICT, 0);
    if handle.is_none() {
        return Err(1);
    }
    let mut handle = handle.unwrap();
    let bounds = if pdfBoxType != 0 {
        /* if cmd was \XeTeXpdffile, use xpdflib to read it */
        let name = CString::new(filename).unwrap();
        pdf_get_rect(name.as_ptr(), handle, page, pdfBoxType).map_err(|_| -1)?
    } else {
        match get_image_size_in_inches(&mut handle) {
            Ok((wd, ht)) => Rect::from_size(size2(
                (wd as f64 * 72.27) as f32,
                (ht as f64 * 72.27) as f32,
            )),
            Err(e) => return Err(e),
        }
    };
    Ok((bounds, filename.to_string()))
}

fn to_points(r: &Rect) -> [Point; 4] {
    [
        r.min(),
        point2(r.min_x(), r.max_y()),
        r.max(),
        point2(r.max_x(), r.min_y()),
    ]
}

pub(crate) unsafe fn load_picture(input: &mut input_state_t, is_pdf: bool) {
    let mut result: i32 = 0;
    let file = scan_file_name(input).0;
    let filename = file.to_string();
    let mut pdf_box_type = 0;
    let mut page = 0;
    if is_pdf {
        if scan_keyword(input, "page") {
            page = scan_int(input);
        }
        pdf_box_type = if scan_keyword(input, "crop") {
            1
        } else if scan_keyword(input, "media") {
            2
        } else if scan_keyword(input, "bleed") {
            3
        } else if scan_keyword(input, "trim") {
            4
        } else if scan_keyword(input, "art") {
            5
        } else {
            6
        };
    }
    let (bounds, pic_path) = if pdf_box_type == 6 {
        find_pic_file(&filename, 1, page)
    } else {
        find_pic_file(&filename, pdf_box_type, page)
    }
    .unwrap_or_else(|e| {
        result = e;
        (Rect::zero(), String::new())
    });
    let mut corners = bounds;
    let mut x_size_req = 0_f64;
    let mut y_size_req = 0_f64;
    let mut t = Transform::identity();
    let mut check_keywords = true;
    while check_keywords {
        if scan_keyword(input, "scaled") {
            let val = scan_int(input);
            if x_size_req == 0. && y_size_req == 0. {
                let t2 = Transform::create_scale(val as f64 / 1000., val as f64 / 1000.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(input, "xscaled") {
            let val = scan_int(input);
            if x_size_req == 0. && y_size_req == 0. {
                let t2 = Transform::create_scale(val as f64 / 1000., 1.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(input, "yscaled") {
            let val = scan_int(input);
            if x_size_req == 0.0f64 && y_size_req == 0.0f64 {
                let t2 = Transform::create_scale(1., val as f64 / 1000.);
                corners = t2.transform_rect(&corners.to_f64()).to_f32();
                t = t.post_transform(&t2);
            }
        } else if scan_keyword(input, "width") {
            let val = scan_dimen(input, false, false, None);
            if val <= Scaled::ZERO {
                t_eprint!("Improper image size ({}pt) will be ignored", val);
                help!(
                    "I can\'t scale images to zero or negative sizes,",
                    "so I\'m ignoring this."
                );
                error();
            } else {
                x_size_req = Fix2D(val)
            }
        } else if scan_keyword(input, "height") {
            let val = scan_dimen(input, false, false, None);
            if val <= Scaled::ZERO {
                t_eprint!("Improper image size ({}pt) will be ignored", val);
                help!(
                    "I can\'t scale images to zero or negative sizes,",
                    "so I\'m ignoring this."
                );
                error();
            } else {
                y_size_req = Fix2D(val)
            }
        } else if scan_keyword(input, "rotated") {
            let val = scan_decimal(input);
            if x_size_req != 0.0f64 || y_size_req != 0.0f64 {
                let brect = Rect::from_points(&to_points(&corners));
                let xmin = brect.min_x() as f64;
                let ymin = brect.min_y() as f64;
                let xmax = brect.max_x() as f64;
                let ymax = brect.max_y() as f64;
                let t2 = if x_size_req == 0. {
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
            let t2 = Transform::create_rotation(Angle::degrees(Fix2D(val)));

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
        let t2 = if x_size_req == 0.0f64 {
            Transform::create_scale(y_size_req / (ymax - ymin), y_size_req / (ymax - ymin))
        } else if y_size_req == 0.0f64 {
            Transform::create_scale(x_size_req / (xmax - xmin), x_size_req / (xmax - xmin))
        } else {
            Transform::create_scale(x_size_req / (xmax - xmin), y_size_req / (ymax - ymin))
        };

        corners = t2.transform_rect(&corners.to_f64()).to_f32();
        t = t.post_transform(&t2);
    }

    let brect = Rect::from_points(&to_points(&corners));
    let xmin = brect.min_x() as f64;
    let ymin = brect.min_y() as f64;
    let xmax = brect.max_x() as f64;
    let ymax = brect.max_y() as f64;

    let t2 = Transform::create_translation(
        (-(xmin as i32) * 72) as f64 / 72.27,
        (-(ymin as i32) * 72) as f64 / 72.27,
    );
    t = t.post_transform(&t2);
    if result == 0 {
        let len = pic_path.as_bytes().len();
        let mut tail_pic = if is_pdf {
            Picture::new_pdf_node(len)
        } else {
            Picture::new_pic_node(len)
        };
        *LLIST_link(cur_list.tail) = Some(tail_pic.ptr()).tex_int();
        cur_list.tail = tail_pic.ptr();
        tail_pic
            .set_page(page as u16)
            .set_pagebox(pdf_box_type as u16);
        tail_pic
            .set_width(D2Fix(xmax - xmin))
            .set_height(D2Fix(ymax - ymin))
            .set_depth(Scaled::ZERO);
        tail_pic.set_transform_matrix([
            D2Fix(t.m11),
            D2Fix(t.m12),
            D2Fix(t.m21),
            D2Fix(t.m22),
            D2Fix(t.m31),
            D2Fix(t.m32),
        ]);

        tail_pic.path_mut().copy_from_slice(pic_path.as_bytes());
    } else {
        t_eprint!("Unable to load picture or PDF file \'{:#}\'", file);
        if result == -43i32 {
            help!(
                "The requested image couldn\'t be read because",
                "the file was not found."
            );
        } else {
            help!(
                "The requested image couldn\'t be read because",
                "it was not a recognized image format."
            );
        }
        error();
    };
}
