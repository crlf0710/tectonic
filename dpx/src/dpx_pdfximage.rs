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

use euclid::point2;

use crate::mfree;
use crate::DisplayExt;
use crate::{info, warn};
use crate::{streq_ptr, strstartswith};
use std::ffi::CStr;
use std::ptr;

use super::dpx_bmpimage::{bmp_include_image, check_for_bmp};
use super::dpx_dpxfile::{dpx_delete_temp_file, keep_cache};
use super::dpx_jpegimage::{check_for_jpeg, jpeg_include_image};
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::{tt_mfgets, work_buffer};
use super::dpx_pdfdraw::pdf_dev_transform;
use super::dpx_pngimage::{check_for_png, png_include_image};
use crate::dpx_epdf::pdf_include_page;
use crate::dpx_pdfobj::{
    check_for_pdf, pdf_link_obj, pdf_new_name, pdf_new_number, pdf_obj, pdf_ref_obj,
    pdf_release_obj,
};
use crate::shims::sprintf;
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{free, memset, strcpy, strlen};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;

use super::dpx_pdfdev::{transform_info, Point, Rect, TMatrix};
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub struct ximage_info {
    pub flags: i32,
    pub width: i32,
    pub height: i32,
    pub bits_per_component: i32,
    pub num_components: i32,
    pub min_dpi: i32,
    pub xdensity: f64,
    pub ydensity: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct xform_info {
    pub flags: i32,
    pub bbox: Rect,
    pub matrix: TMatrix,
}

impl Default for xform_info {
    fn default() -> Self {
        Self {
            flags: 0,
            bbox: Rect::new(Point::default(), Point::default()),
            matrix: TMatrix::default(),
        }
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct load_options {
    pub page_no: i32,
    pub bbox_type: i32,
    pub dict: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_ximage {
    pub ident: *mut i8,
    pub res_name: [i8; 16],
    pub subtype: i32,
    pub attr: attr_,
    pub filename: *mut i8,
    pub reference: *mut pdf_obj,
    pub resource: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct attr_ {
    pub width: i32,
    pub height: i32,
    pub xdensity: f64,
    pub ydensity: f64,
    pub bbox: Rect,
    pub page_no: i32,
    pub page_count: i32,
    pub bbox_type: i32,
    pub dict: *mut pdf_obj,
    pub tempfile: i8,
}
/* quasi-hack to get the primary input */
/* verbose, verbose, verbose... */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct opt_ {
    pub verbose: i32,
    pub cmdtmpl: *mut i8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ic_ {
    pub count: i32,
    pub capacity: i32,
    pub ximages: *mut pdf_ximage,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut _opts: opt_ = opt_ {
    verbose: 0i32,
    cmdtmpl: ptr::null_mut(),
};

pub unsafe fn pdf_ximage_set_verbose(mut level: i32) {
    _opts.verbose = level;
}
static mut _ic: ic_ = ic_ {
    count: 0i32,
    capacity: 0i32,
    ximages: ptr::null_mut(),
};
unsafe fn pdf_init_ximage_struct(mut I: *mut pdf_ximage) {
    (*I).ident = ptr::null_mut();
    (*I).filename = ptr::null_mut();
    (*I).subtype = -1i32;
    memset((*I).res_name.as_mut_ptr() as *mut libc::c_void, 0i32, 16);
    (*I).reference = ptr::null_mut();
    (*I).resource = ptr::null_mut();
    (*I).attr.height = 0i32;
    (*I).attr.width = (*I).attr.height;
    (*I).attr.ydensity = 1.0f64;
    (*I).attr.xdensity = (*I).attr.ydensity;
    (*I).attr.bbox = Rect::zero();
    (*I).attr.page_no = 1i32;
    (*I).attr.page_count = 1i32;
    (*I).attr.bbox_type = 0i32;
    (*I).attr.dict = ptr::null_mut();
    (*I).attr.tempfile = 0_i8;
}
unsafe fn pdf_clean_ximage_struct(mut I: *mut pdf_ximage) {
    free((*I).ident as *mut libc::c_void);
    free((*I).filename as *mut libc::c_void);
    pdf_release_obj((*I).reference);
    pdf_release_obj((*I).resource);
    pdf_release_obj((*I).attr.dict);
    pdf_init_ximage_struct(I);
}

pub unsafe fn pdf_init_images() {
    let mut ic: *mut ic_ = &mut _ic;
    (*ic).count = 0i32;
    (*ic).capacity = 0i32;
    (*ic).ximages = ptr::null_mut();
}

pub unsafe fn pdf_close_images() {
    let mut ic: *mut ic_ = &mut _ic;
    if !(*ic).ximages.is_null() {
        for i in 0..(*ic).count {
            let mut I: *mut pdf_ximage = (*ic).ximages.offset(i as isize);
            if (*I).attr.tempfile != 0 {
                /*
                 * It is important to remove temporary files at the end because
                 * we cache file names. Since we use mkstemp to create them, we
                 * might get the same file name again if we delete the first file.
                 * (This happens on NetBSD, reported by Jukka Salmi.)
                 * We also use this to convert a PS file only once if multiple
                 * pages are imported from that file.
                 */
                if _opts.verbose > 1i32 && keep_cache != 1i32 {
                    info!(
                        "pdf_image>> deleting temporary file \"{}\"\n",
                        CStr::from_ptr((*I).filename).display()
                    ); /* temporary filename freed here */
                }
                dpx_delete_temp_file((*I).filename, 0i32);
                (*I).filename = ptr::null_mut()
            }
            pdf_clean_ximage_struct(I);
        }
        (*ic).ximages = mfree((*ic).ximages as *mut libc::c_void) as *mut pdf_ximage;
        (*ic).capacity = 0i32;
        (*ic).count = (*ic).capacity
    }
    _opts.cmdtmpl = mfree(_opts.cmdtmpl as *mut libc::c_void) as *mut i8;
}
unsafe fn source_image_type(handle: &mut InputHandleWrapper) -> i32 {
    handle.seek(SeekFrom::Start(0)).unwrap();
    /* Original check order: jpeg, jp2, png, bmp, pdf, ps */
    let format = if check_for_jpeg(handle) != 0 {
        1
    } else if check_for_png(handle) != 0 {
        2
    } else if check_for_bmp(handle) != 0 {
        6
    } else if check_for_pdf(handle) != false {
        0
    } else if check_for_ps(handle) != 0 {
        5
    } else {
        warn!("Tectonic was unable to detect an image\'s format");
        -1
    };
    handle.seek(SeekFrom::Start(0)).unwrap();
    format
}
unsafe fn load_image(
    mut ident: *const i8,
    mut fullname: *const i8,
    mut format: i32,
    mut handle: InputHandleWrapper,
    mut options: load_options,
) -> i32 {
    let mut ic: *mut ic_ = &mut _ic;
    let id = (*ic).count;
    if (*ic).count >= (*ic).capacity {
        (*ic).capacity += 16i32;
        (*ic).ximages = renew(
            (*ic).ximages as *mut libc::c_void,
            ((*ic).capacity as u32 as u64).wrapping_mul(::std::mem::size_of::<pdf_ximage>() as u64)
                as u32,
        ) as *mut pdf_ximage
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    pdf_init_ximage_struct(I);
    if !ident.is_null() {
        (*I).ident =
            new((strlen(ident).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy((*I).ident, ident);
    }
    if !fullname.is_null() {
        (*I).filename =
            new((strlen(fullname).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy((*I).filename, fullname);
    }
    (*I).attr.page_no = options.page_no;
    (*I).attr.bbox_type = options.bbox_type;
    /* else if (check_for_jp2(fp))
     *    format = IMAGE_TYPE_JP2; */
    (*I).attr.dict = options.dict; /* unsafe? */
    match format {
        1 => {
            if _opts.verbose != 0 {
                info!("[JPEG]");
            }
            if jpeg_include_image(I, &mut handle) < 0 {
                pdf_clean_ximage_struct(I);
                return -1;
            }

            (*I).subtype = 1;
            ttstub_input_close(handle);
            // FIXME: `ttstub_input_close` is not used in this place in
            // https://github.com/tectonic-typesetting/tectonic/blob/e4b884ceeeeda2808289d034480f27008a678746/tectonic/dpx-pdfximage.c#L254
        }
        7 => {
            if _opts.verbose != 0 {
                info!("[JP2]");
            }
            /*if (jp2_include_image(I, fp) < 0)*/
            warn!("Tectonic: JP2 not yet supported");
            ttstub_input_close(handle);
            pdf_clean_ximage_struct(I);
            return -1;
            /*I->subtype = PDF_XOBJECT_TYPE_IMAGE;
            break;*/
        }
        2 => {
            if _opts.verbose != 0 {
                info!("[PNG]");
            }
            if png_include_image(I, &mut handle) < 0 {
                pdf_clean_ximage_struct(I);
                return -1;
            }
            ttstub_input_close(handle);
        }
        6 => {
            if _opts.verbose != 0 {
                info!("[BMP]");
            }
            if bmp_include_image(I, &mut handle) < 0 {
                pdf_clean_ximage_struct(I);
                return -1;
            }
            (*I).subtype = 1;
            ttstub_input_close(handle);
        }
        0 => {
            if _opts.verbose != 0 {
                info!("[PDF]");
            }
            let mut result: i32 = pdf_include_page(I, handle.clone(), fullname, options);
            /* Tectonic: this used to try ps_include_page() */
            if result != 0 {
                pdf_clean_ximage_struct(I);
                return -1;
            }
            if _opts.verbose != 0 {
                info!(",Page:{}", (*I).attr.page_no);
            }
            ttstub_input_close(handle);
            (*I).subtype = 0;
        }
        5 => {
            if _opts.verbose != 0 {
                info!("[EPS]");
            }
            warn!("sorry, PostScript images are not supported by Tectonic");
            warn!("for details, please see https://github.com/tectonic-typesetting/tectonic/issues/27");
            ttstub_input_close(handle);
            pdf_clean_ximage_struct(I);
            return -1;
        }
        _ => {
            if _opts.verbose != 0 {
                info!("[UNKNOWN]");
            }
            /* Tectonic: this used to try ps_include_page() */
            ttstub_input_close(handle);
            pdf_clean_ximage_struct(I);
            return -1;
        }
    }

    match (*I).subtype {
        1 => {
            sprintf(
                (*I).res_name.as_mut_ptr(),
                b"Im%d\x00" as *const u8 as *const i8,
                id,
            );
        }
        0 => {
            sprintf(
                (*I).res_name.as_mut_ptr(),
                b"Fm%d\x00" as *const u8 as *const i8,
                id,
            );
        }
        _ => {
            panic!("Unknown XObject subtype: {}", (*I).subtype);
        }
    }
    (*ic).count += 1;
    id
}

pub unsafe fn pdf_ximage_findresource(mut ident: *const i8, mut options: load_options) -> i32 {
    let mut ic: *mut ic_ = &mut _ic;
    /* "I don't understand why there is comparision against I->attr.dict here...
     * I->attr.dict and options.dict are simply pointers to PDF dictionaries."
     */
    for id in 0..(*ic).count {
        let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
        if !(*I).ident.is_null() && streq_ptr(ident, (*I).ident) as i32 != 0 {
            if (*I).attr.page_no == options.page_no
                && (*I).attr.dict == options.dict
                && (*I).attr.bbox_type == options.bbox_type
            {
                return id;
            }
        }
    }
    /* This happens if we've already inserted the image into the PDF output.
     * In my one test case, it seems to just work to plunge along merrily
     * ahead ...
     *
     * if (f) {
     *   <"we already have converted this file; f is the temporary file name">
     *   fullname = NEW(strlen(f)+1, char);
     *   strcpy(fullname, f);
     * } else { kpse_find_file() }
     */
    let handle = ttstub_input_open(ident, TTInputFormat::PICT, 0i32);
    if handle.is_none() {
        warn!(
            "Error locating image file \"{}\"",
            CStr::from_ptr(ident).display(),
        );
        return -1i32;
    }
    let mut handle = handle.unwrap();
    if _opts.verbose != 0 {
        info!("(Image:{}", CStr::from_ptr(ident).display());
    }
    let format = source_image_type(&mut handle);
    let id = load_image(ident, ident, format, handle, options);
    if _opts.verbose != 0 {
        info!(")");
    }
    if id < 0i32 {
        warn!(
            "pdf: image inclusion failed for \"{}\".",
            CStr::from_ptr(ident).display(),
        );
    }
    id
}
/* Reference: PDF Reference 1.5 v6, pp.321--322
 *
 * TABLE 4.42 Additional entries specific to a type 1 form dictionary
 *
 * BBox rectangle (Required) An array of four numbers in the form coordinate
 *                system, giving the coordinates of the left, bottom, right,
 *                and top edges, respectively, of the form XObject's bounding
 *                box. These boundaries are used to clip the form XObject and
 *                to determine its size for caching.
 *
 * Matrix array   (Optional) An array of six numbers specifying the form
 *                matrix, which maps form space into user space.
 *                Default value: the identity matrix [1 0 0 1 0 0].
 */

pub fn pdf_ximage_init_form_info(info: &mut xform_info) {
    info.flags = 0;
    info.bbox = Rect::zero();
    info.matrix = TMatrix::identity();
}
/* Reference: PDF Reference 1.5 v6, pp.303--306
 *
 * TABLE 4.42 Additional entries specific to an image dictionary
 *
 * Width integer  (Required) The width of the image, in samples.
 *
 * Height integer (Required) The height of the image, in samples.
 *
 * ColorSpace name or array
 *                (Required for images, except those that use the JPXDecode
 *                filter; not allowed for image masks) The color space in
 *                which image samples are specified. This may be any type
 *                of color space except Patter.
 *
 *                If the image uses the JPXDecode filter, this entry is
 *                optional.
 *
 * BitsPerComponent integer
 *                (Required except for image masks and images that use the
 *                JPXDecode filter) The number of bits used to represent
 *                each color component. Only a single value may be specified;
 *                the number of bits is the same for all color components.
 *                Valid values are 1,2,4,8, and (in PDF1.5) 16. If ImageMask
 *                is true, this entry is optional, and if speficified, its
 *                value must be 1.
 *
 *                If the image stream uses the JPXDecode filter, this entry
 *                is optional and ignored if present. The bit depth is
 *                determined in the process of decoding the JPEG2000 image.
 */

pub fn pdf_ximage_init_image_info(info: &mut ximage_info) {
    info.flags = 0; /* The width of the image, in samples */
    info.width = 0; /* The height of the image, in samples */
    info.height = 0;
    info.bits_per_component = 0;
    info.num_components = 0;
    info.min_dpi = 0;
    info.ydensity = 1.;
    info.xdensity = info.ydensity;
}

pub unsafe fn pdf_ximage_set_image(
    mut I: *mut pdf_ximage,
    image_info: &mut ximage_info,
    mut resource: *mut pdf_obj,
) {
    let info = image_info;
    if !(!resource.is_null() && (*resource).is_stream()) {
        panic!("Image XObject must be of stream type.");
    }
    (*I).subtype = 1i32;
    (*I).attr.width = info.width;
    (*I).attr.height = info.height;
    (*I).attr.xdensity = info.xdensity;
    (*I).attr.ydensity = info.ydensity;
    (*I).reference = pdf_ref_obj(resource);
    let dict = (*resource).as_stream_mut().get_dict_mut();
    dict.set("Type", pdf_new_name("XObject"));
    dict.set("Subtype", pdf_new_name("Image"));
    dict.set("Width", pdf_new_number((*info).width as f64));
    dict.set("Height", pdf_new_number((*info).height as f64));
    if (*info).bits_per_component > 0i32 {
        /* Ignored for JPXDecode filter. FIXME */
        dict.set(
            "BitsPerComponent",
            pdf_new_number((*info).bits_per_component as f64),
        ); /* Caller don't know we are using reference. */
    }
    if !(*I).attr.dict.is_null() {
        dict.merge((*(*I).attr.dict).as_dict());
    }
    pdf_release_obj(resource);
    (*I).resource = ptr::null_mut();
}

pub unsafe fn pdf_ximage_set_form(
    mut I: *mut pdf_ximage,
    form_info: &mut xform_info,
    mut resource: *mut pdf_obj,
) {
    let info = form_info;
    (*I).subtype = 0i32;
    /* Image's attribute "bbox" here is affected by /Rotate entry of included
     * PDF page.
     */
    let mut p1 = info.bbox.min;
    pdf_dev_transform(&mut p1, Some(&info.matrix));
    let mut p2 = point2(info.bbox.max.x, info.bbox.min.y);
    pdf_dev_transform(&mut p2, Some(&info.matrix));
    let mut p3 = info.bbox.max;
    pdf_dev_transform(&mut p3, Some(&info.matrix));
    let mut p4 = point2(info.bbox.min.x, info.bbox.max.y);
    pdf_dev_transform(&mut p4, Some(&info.matrix));
    (*I).attr.bbox.min.x = p1.x.min(p2.x).min(p3.x).min(p4.x);
    (*I).attr.bbox.min.y = p1.y.min(p2.y).min(p3.y).min(p4.y);
    (*I).attr.bbox.max.x = p1.x.max(p2.x).max(p3.x).max(p4.x);
    (*I).attr.bbox.max.y = p1.y.max(p2.y).max(p3.y).max(p4.y);
    (*I).reference = pdf_ref_obj(resource);
    pdf_release_obj(resource);
    (*I).resource = ptr::null_mut();
}

pub unsafe fn pdf_ximage_get_page(mut I: *mut pdf_ximage) -> i32 {
    (*I).attr.page_no
}

pub unsafe fn pdf_ximage_get_reference(mut id: i32) -> *mut pdf_obj {
    let mut ic: *mut ic_ = &mut _ic;
    if id < 0i32 || id >= (*ic).count {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    if (*I).reference.is_null() {
        (*I).reference = pdf_ref_obj((*I).resource)
    }
    pdf_link_obj((*I).reference)
}

#[derive(Clone)]
pub enum XInfo {
    Form(Box<xform_info>),
    Image(Box<ximage_info>),
}

/* called from pdfdoc.c only for late binding */

pub unsafe fn pdf_ximage_defineresource(
    mut ident: *const i8,
    info: XInfo,
    mut resource: *mut pdf_obj,
) -> i32 {
    let mut ic: *mut ic_ = &mut _ic;
    let id = (*ic).count;
    if (*ic).count >= (*ic).capacity {
        (*ic).capacity += 16i32;
        (*ic).ximages = renew(
            (*ic).ximages as *mut libc::c_void,
            ((*ic).capacity as u32 as u64).wrapping_mul(::std::mem::size_of::<pdf_ximage>() as u64)
                as u32,
        ) as *mut pdf_ximage
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    pdf_init_ximage_struct(I);
    if !ident.is_null() {
        (*I).ident =
            new((strlen(ident).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy((*I).ident, ident);
    }
    match info {
        XInfo::Image(mut info) => {
            pdf_ximage_set_image(I, &mut info, resource);
            sprintf(
                (*I).res_name.as_mut_ptr(),
                b"Im%d\x00" as *const u8 as *const i8,
                id,
            );
        }
        XInfo::Form(mut info) => {
            pdf_ximage_set_form(I, &mut info, resource);
            sprintf(
                (*I).res_name.as_mut_ptr(),
                b"Fm%d\x00" as *const u8 as *const i8,
                id,
            );
        }
    }
    (*ic).count += 1;
    id
}

pub unsafe fn pdf_ximage_get_resname(mut id: i32) -> *mut i8 {
    let mut ic: *mut ic_ = &mut _ic;
    if id < 0i32 || id >= (*ic).count {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    (*I).res_name.as_mut_ptr()
}

pub unsafe fn pdf_ximage_get_subtype(mut id: i32) -> i32 {
    let mut ic: *mut ic_ = &mut _ic;
    if id < 0i32 || id >= (*ic).count {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    (*I).subtype
}
/* from spc_pdfm.c */

pub unsafe fn pdf_ximage_set_attr(
    mut id: i32,
    mut width: i32,
    mut height: i32,
    mut xdensity: f64,
    mut ydensity: f64,
    mut llx: f64,
    mut lly: f64,
    mut urx: f64,
    mut ury: f64,
) {
    let mut ic: *mut ic_ = &mut _ic;
    if id < 0i32 || id >= (*ic).count {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    (*I).attr.width = width;
    (*I).attr.height = height;
    (*I).attr.xdensity = xdensity;
    (*I).attr.ydensity = ydensity;
    (*I).attr.bbox = Rect::new(point2(llx, lly), point2(urx, ury));
}
/* depth...
 * Dvipdfm treat "depth" as "yoffset" for pdf:image and pdf:uxobj
 * not as vertical dimension of scaled image. (And there are bugs.)
 * This part contains incompatibile behaviour than dvipdfm!
 */
unsafe fn scale_to_fit_I(T: &mut TMatrix, p: &mut transform_info, mut I: *mut pdf_ximage) {
    let s_x;
    let s_y;
    let d_x;
    let d_y;
    let mut wd0;
    let mut ht0;
    let xscale;
    let yscale;
    if p.flags & 1i32 << 0i32 != 0 {
        wd0 = p.bbox.size().width;
        ht0 = p.bbox.size().height;
        xscale = (*I).attr.width as f64 * (*I).attr.xdensity / wd0;
        yscale = (*I).attr.height as f64 * (*I).attr.ydensity / ht0;
        d_x = -p.bbox.min.x / wd0;
        d_y = -p.bbox.min.y / ht0
    } else {
        wd0 = (*I).attr.width as f64 * (*I).attr.xdensity;
        ht0 = (*I).attr.height as f64 * (*I).attr.ydensity;
        yscale = 1.0f64;
        xscale = yscale;
        d_x = 0.0f64;
        d_y = 0.0f64
    }
    if wd0 == 0.0f64 {
        warn!("Image width=0.0!");
        wd0 = 1.0f64
    }
    if ht0 == 0.0f64 {
        warn!("Image height=0.0!");
        ht0 = 1.0f64
    }
    let dp = if p.flags & 1i32 << 1i32 != 0 && p.flags & 1i32 << 2i32 != 0 {
        s_x = p.width * xscale;
        s_y = (p.height + p.depth) * yscale;
        p.depth * yscale
    } else if p.flags & 1i32 << 1i32 != 0 {
        s_x = p.width * xscale;
        s_y = s_x * ((*I).attr.height as f64 / (*I).attr.width as f64);
        0.
    } else if p.flags & 1i32 << 2i32 != 0 {
        s_y = (p.height + p.depth) * yscale;
        s_x = s_y * ((*I).attr.width as f64 / (*I).attr.height as f64);
        p.depth * yscale
    } else {
        s_x = wd0;
        s_y = ht0;
        0.
    };
    *T = TMatrix::row_major(
        s_x,
        0.,
        0.,
        s_y,
        d_x * s_x / xscale,
        d_y * s_y / yscale - dp,
    );
}
unsafe fn scale_to_fit_F(T: &mut TMatrix, p: &mut transform_info, mut I: *mut pdf_ximage) {
    let s_x;
    let s_y;
    let d_x;
    let d_y;
    let mut wd0;
    let mut ht0;
    if p.flags & 1i32 << 0i32 != 0 {
        wd0 = p.bbox.size().width;
        ht0 = p.bbox.size().height;
        d_x = -p.bbox.min.x;
        d_y = -p.bbox.min.y
    } else {
        wd0 = (*I).attr.bbox.size().width;
        ht0 = (*I).attr.bbox.size().height;
        d_x = 0.0f64;
        d_y = 0.0f64
    }
    if wd0 == 0.0f64 {
        warn!("Image width=0.0!");
        wd0 = 1.0f64
    }
    if ht0 == 0.0f64 {
        warn!("Image height=0.0!");
        ht0 = 1.0f64
    }
    let dp = if p.flags & 1i32 << 1i32 != 0 && p.flags & 1i32 << 2i32 != 0 {
        s_x = p.width / wd0;
        s_y = (p.height + p.depth) / ht0;
        p.depth
    } else if p.flags & 1i32 << 1i32 != 0 {
        s_x = p.width / wd0;
        s_y = s_x;
        0.
    } else if p.flags & 1i32 << 2i32 != 0 {
        s_y = (p.height + p.depth) / ht0;
        s_x = s_y;
        p.depth
    } else {
        s_y = 1.0f64;
        s_x = s_y;
        0.
    };
    *T = TMatrix::row_major(s_x, 0., 0., s_y, s_x * d_x, s_y * d_y - dp);
}
/* called from pdfdev.c and spc_html.c */

pub unsafe fn pdf_ximage_scale_image(id: i32, r: &mut Rect, p: &mut transform_info) -> TMatrix
/* argument from specials */ {
    let mut ic: *mut ic_ = &mut _ic;
    if id < 0i32 || id >= (*ic).count {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut *(*ic).ximages.offset(id as isize) as *mut pdf_ximage;
    let mut M = TMatrix::identity();
    match (*I).subtype {
        1 => {
            /* Reference: PDF Reference 1.5 v6, p.302
             *
             * An image can be placed on the output page in any desired position,
             * orientation, and size by using the cm operator to modify the current
             * transformation matrix (CTM) so as to map the unit square of user space
             * to the rectangle or parallelogram in which the image is to be painted.
             *
             * There is neither BBox nor Matrix key in the image XObject.
             * Everything must be controlled by the cm operator.
             *
             * The argument [p] contains the user-defined bounding box, the scailing
             * factor of which is bp as EPS and PDF. On the other hand, I->attr
             * contains the (sampling) width and the (sampling) height of the image.
             *
             * There is no problem if a bitmap image has density information.
             * Otherwise, DVIPDFM's ebb generates bounding box as 100px = 72bp = 1in.
             * In this case, screen captured images look bad. Moreover, DVIPDFM's ebb
             * ignores all density information and use just 100px = 72bp = 1in.
             *
             * On the other hand, pdfTeX uses 100px = 100bp to get a better quality
             * for screen captured images.
             *
             * DVIPDFMx's xbb generates bounding box as 100px = 100bp in the same
             * way as pdfTeX. Furthermore, it takes care of density information too.
             */
            scale_to_fit_I(&mut M, p, I);
            if p.flags & 1i32 << 0i32 != 0 {
                r.min.x = p.bbox.min.x / ((*I).attr.width as f64 * (*I).attr.xdensity);
                r.min.y = p.bbox.min.y / ((*I).attr.height as f64 * (*I).attr.ydensity);
                r.max.x = p.bbox.max.x / ((*I).attr.width as f64 * (*I).attr.xdensity);
                r.max.y = p.bbox.max.y / ((*I).attr.height as f64 * (*I).attr.ydensity)
            } else {
                *r = Rect::new(point2(0., 0.), point2(1., 1.));
            }
        }
        0 => {
            /* User-defined transformation and clipping are controlled by
             * the cm operator and W operator, explicitly */
            scale_to_fit_F(&mut M, p, I); /* I->attr.bbox from the image bounding box */
            if p.flags & 1i32 << 0i32 != 0 {
                *r = p.bbox;
            } else {
                *r = (*I).attr.bbox;
            }
        }
        _ => {}
    }
    M
}
/* Migrated from psimage.c */

pub unsafe fn set_distiller_template(mut s: *mut i8) {
    free(_opts.cmdtmpl as *mut libc::c_void);
    if s.is_null() || *s as i32 == '\u{0}' as i32 {
        _opts.cmdtmpl = ptr::null_mut()
    } else {
        _opts.cmdtmpl =
            new((strlen(s).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy(_opts.cmdtmpl, s);
    };
}
/* NOT USED YET */
/* scale factor for bp */
/* Please use different interface than findresource...
 * This is not intended to be used for specifying page number and others.
 * Only pdf:image special in spc_pdfm.c want optinal dict!
 */
/* Called by pngimage, jpegimage, epdf, mpost, etc. */
/* from pdfximage.c */

pub unsafe fn get_distiller_template() -> *mut i8 {
    _opts.cmdtmpl
}
unsafe fn check_for_ps(handle: &mut InputHandleWrapper) -> i32 {
    handle.seek(SeekFrom::Start(0)).unwrap();
    tt_mfgets(work_buffer.as_mut_ptr(), 1024i32, handle);
    if !strstartswith(
        work_buffer.as_mut_ptr(),
        b"%!\x00" as *const u8 as *const i8,
    )
    .is_null()
    {
        return 1i32;
    }
    0i32
}
