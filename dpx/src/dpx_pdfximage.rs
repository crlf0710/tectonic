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

use euclid::point2;

use crate::{info, warn};
use std::ptr;

use super::dpx_bmpimage::{bmp_include_image, check_for_bmp};
use super::dpx_dpxfile::{dpx_delete_temp_file, keep_cache};
use super::dpx_jpegimage::{check_for_jpeg, jpeg_include_image};
use super::dpx_mfileio::tt_mfreadln;
use super::dpx_pdfdraw::pdf_dev_transform;
use super::dpx_pngimage::{check_for_png, png_include_image};
use crate::dpx_epdf::pdf_include_page;
use crate::dpx_pdfdoc::PdfPageBoundary;
use crate::dpx_pdfobj::{
    check_for_pdf, pdf_link_obj, pdf_obj, pdf_ref_obj, IntoObj, IntoRef, Object,
};

use std::io::{Read, Seek, SeekFrom};

use crate::bridge::{InFile, TTInputFormat};

use super::dpx_pdfdev::{transform_info, Point, Rect, TMatrix};

#[derive(Copy, Clone)]
pub(crate) enum PdfXObjectType {
    None,
    Form,
    Image,
}

#[derive(Copy, Clone)]
pub(crate) enum ImageType {
    Unknown,
    Pdf,
    Jpeg,
    Png,
    Eps,
    Bmp,
    #[allow(unused)]
    Jp2,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct ximage_info {
    pub(crate) flags: i32,
    pub(crate) width: i32,
    pub(crate) height: i32,
    pub(crate) bits_per_component: i32,
    pub(crate) num_components: i32,
    pub(crate) min_dpi: i32,
    pub(crate) xdensity: f64,
    pub(crate) ydensity: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct xform_info {
    pub(crate) flags: i32,
    pub(crate) bbox: Rect,
    pub(crate) matrix: TMatrix,
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
pub(crate) struct load_options {
    pub(crate) page_no: i32,
    pub(crate) bbox_type: PdfPageBoundary,
    pub(crate) dict: *mut pdf_obj,
}
#[derive(Clone)]
pub(crate) struct pdf_ximage {
    pub(crate) ident: String,
    pub(crate) res_name: String,
    pub(crate) subtype: PdfXObjectType,
    pub(crate) attr: attr_,
    pub(crate) filename: String,
    pub(crate) reference: *mut pdf_obj,
    pub(crate) resource: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct attr_ {
    pub(crate) width: i32,
    pub(crate) height: i32,
    pub(crate) xdensity: f64,
    pub(crate) ydensity: f64,
    pub(crate) bbox: Rect,
    pub(crate) page_no: i32,
    pub(crate) page_count: i32,
    pub(crate) bbox_type: PdfPageBoundary,
    pub(crate) dict: *mut pdf_obj,
    pub(crate) tempfile: i8,
}
/* quasi-hack to get the primary input */
/* verbose, verbose, verbose... */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct opt_ {
    pub(crate) verbose: i32,
    //    pub(crate) cmdtmpl: *mut i8,
}
static mut ximages: Vec<pdf_ximage> = Vec::new();

static mut _opts: opt_ = opt_ {
    verbose: 0,
    //    cmdtmpl: ptr::null_mut(),
};

pub(crate) unsafe fn pdf_ximage_set_verbose(level: i32) {
    _opts.verbose = level;
}
impl pdf_ximage {
    pub(crate) fn new() -> Self {
        Self {
            ident: String::new(),
            filename: String::new(),
            subtype: PdfXObjectType::None,
            res_name: String::new(),
            reference: ptr::null_mut(),
            resource: ptr::null_mut(),
            attr: attr_ {
                height: 0,
                width: 0,
                ydensity: 1.,
                xdensity: 1.,
                bbox: Rect::zero(),
                page_no: 1,
                page_count: 1,
                bbox_type: PdfPageBoundary::Auto,
                dict: ptr::null_mut(),
                tempfile: 0,
            },
        }
    }
}
impl Drop for pdf_ximage {
    fn drop(&mut self) {
        unsafe {
            if let Some(reference) = self.reference.as_mut() {
                crate::release!(reference);
            }
            assert!(self.resource.is_null());
            assert!(self.attr.dict.is_null());
        }
    }
}

pub(crate) unsafe fn pdf_init_images() {
    ximages = Vec::new();
}

pub(crate) unsafe fn pdf_close_images() {
    for I in &mut ximages {
        if I.attr.tempfile != 0 {
            /*
             * It is important to remove temporary files at the end because
             * we cache file names. Since we use mkstemp to create them, we
             * might get the same file name again if we delete the first file.
             * (This happens on NetBSD, reported by Jukka Salmi.)
             * We also use this to convert a PS file only once if multiple
             * pages are imported from that file.
             */
            if _opts.verbose > 1 && keep_cache != 1 {
                info!("pdf_image>> deleting temporary file \"{}\"\n", I.filename);
                /* temporary filename freed here */
            }
            dpx_delete_temp_file(&I.filename, 0);
            I.filename = String::new();
        }
    }
    ximages = Vec::new();
    //    _opts.cmdtmpl = mfree(_opts.cmdtmpl as *mut libc::c_void) as *mut i8;
}
unsafe fn source_image_type<R: Read + Seek>(handle: &mut R) -> ImageType {
    handle.seek(SeekFrom::Start(0)).unwrap();
    /* Original check order: jpeg, jp2, png, bmp, pdf, ps */
    let format = if check_for_jpeg(handle) != 0 {
        ImageType::Jpeg
    } else if check_for_png(handle) != 0 {
        ImageType::Png
    } else if check_for_bmp(handle) {
        ImageType::Bmp
    } else if check_for_pdf(handle) {
        ImageType::Pdf
    } else if check_for_ps(handle) != 0 {
        ImageType::Eps
    } else {
        warn!("Tectonic was unable to detect an image\'s format");
        ImageType::Unknown
    };
    handle.seek(SeekFrom::Start(0)).unwrap();
    format
}
unsafe fn load_image(
    ident: &str,
    fullname: &str,
    format: ImageType,
    mut handle: InFile,
    options: load_options,
) -> i32 {
    let id = ximages.len();
    let mut I = pdf_ximage::new();
    if !ident.is_empty() {
        I.ident = ident.to_string();
    }
    if !fullname.is_empty() {
        I.filename = fullname.to_string();
    }
    I.attr.page_no = options.page_no;
    I.attr.bbox_type = options.bbox_type;
    /* else if (check_for_jp2(fp))
     *    format = IMAGE_TYPE_JP2; */
    I.attr.dict = options.dict; /* unsafe? */
    match format {
        ImageType::Jpeg => {
            if _opts.verbose != 0 {
                info!("[JPEG]");
            }
            if jpeg_include_image(&mut I, &mut handle) < 0 {
                return -1;
            }

            I.subtype = PdfXObjectType::Image;
        }
        ImageType::Jp2 => {
            if _opts.verbose != 0 {
                info!("[JP2]");
            }
            /*if (jp2_include_image(I, fp) < 0)*/
            warn!("Tectonic: JP2 not yet supported");
            return -1;
            /*I.subtype = PdfXObjectType::Image;
            break;*/
        }
        ImageType::Png => {
            if _opts.verbose != 0 {
                info!("[PNG]");
            }
            if png_include_image(&mut I, &mut handle) < 0 {
                return -1;
            }
            I.subtype = PdfXObjectType::Image;
        }
        ImageType::Bmp => {
            if _opts.verbose != 0 {
                info!("[BMP]");
            }
            if bmp_include_image(&mut I, &mut handle).is_err() {
                return -1;
            }
            I.subtype = PdfXObjectType::Image;
        }
        ImageType::Pdf => {
            if _opts.verbose != 0 {
                info!("[PDF]");
            }
            let result: i32 = pdf_include_page(&mut I, handle, fullname, options);
            /* Tectonic: this used to try ps_include_page() */
            if result != 0 {
                return -1;
            }
            if _opts.verbose != 0 {
                info!(",Page:{}", I.attr.page_no);
            }
            I.subtype = PdfXObjectType::Form;
        }
        ImageType::Eps => {
            if _opts.verbose != 0 {
                info!("[EPS]");
            }
            warn!("sorry, PostScript images are not supported by Tectonic");
            warn!("for details, please see https://github.com/tectonic-typesetting/tectonic/issues/27");
            return -1;
        }
        ImageType::Unknown => {
            if _opts.verbose != 0 {
                info!("[UNKNOWN]");
            }
            /* Tectonic: this used to try ps_include_page() */
            return -1;
        }
    }

    match I.subtype {
        PdfXObjectType::Image => {
            I.res_name = format!("Im{}", id);
        }
        PdfXObjectType::Form => {
            I.res_name = format!("Fm{}", id);
        }
        _ => {
            panic!("Unknown XObject subtype: {}", -1);
        }
    }
    ximages.push(I);
    id as i32
}

pub(crate) unsafe fn pdf_ximage_findresource(ident: &str, options: load_options) -> i32 {
    /* "I don't understand why there is comparision against I->attr.dict here...
     * I->attr.dict and options.dict are simply pointers to PDF dictionaries."
     */
    for (id, I) in ximages.iter().enumerate() {
        if !I.ident.is_empty() && ident == I.ident {
            if I.attr.page_no == options.page_no
                && I.attr.dict == options.dict
                && I.attr.bbox_type == options.bbox_type
            {
                return id as i32;
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
    let handle = InFile::open(ident, TTInputFormat::PICT, 0);
    if handle.is_none() {
        warn!("Error locating image file \"{}\"", ident);
        return -1;
    }
    let mut handle = handle.unwrap();
    if _opts.verbose != 0 {
        info!("(Image:{}", ident);
    }
    let format = source_image_type(&mut handle);
    let id = load_image(ident, ident, format, handle, options);
    if _opts.verbose != 0 {
        info!(")");
    }
    if id < 0 {
        warn!("pdf: image inclusion failed for \"{}\".", ident);
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

impl xform_info {
    pub(crate) fn new() -> Self {
        Self {
            flags: 0,
            bbox: Rect::zero(),
            matrix: TMatrix::identity(),
        }
    }
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

impl ximage_info {
    pub(crate) fn init() -> Self {
        Self {
            flags: 0, /* The width of the image, in samples */
            width: 0, /* The height of the image, in samples */
            height: 0,
            bits_per_component: 0,
            num_components: 0,
            min_dpi: 0,
            ydensity: 1.,
            xdensity: 1.,
        }
    }
}

impl pdf_ximage {
    pub(crate) unsafe fn set_image(&mut self, image_info: &ximage_info, mut resource: Object) {
        let info = image_info;
        if let Object::Stream(res) = &mut resource {
            self.subtype = PdfXObjectType::Image;
            self.attr.width = info.width;
            self.attr.height = info.height;
            self.attr.xdensity = info.xdensity;
            self.attr.ydensity = info.ydensity;
            let dict = res.get_dict_mut();
            dict.set("Type", "XObject");
            dict.set("Subtype", "Image");
            dict.set("Width", (*info).width as f64);
            dict.set("Height", (*info).height as f64);
            if (*info).bits_per_component > 0 {
                /* Ignored for JPXDecode filter. FIXME */
                dict.set("BitsPerComponent", (*info).bits_per_component as f64);
                /* Caller don't know we are using reference. */
            }
            if !self.attr.dict.is_null() {
                dict.merge((*self.attr.dict).as_dict());
            }
            self.reference = resource.into_ref().into_obj();
            self.resource = ptr::null_mut();
        } else {
            panic!("Image XObject must be of stream type.");
        }
    }
    pub(crate) unsafe fn set_image1(&mut self, _image_info: &ximage_info, _resource: *mut pdf_obj) {
        unreachable!()
    }

    pub(crate) unsafe fn set_form(&mut self, form_info: &xform_info, resource: Object) {
        let info = form_info;
        self.subtype = PdfXObjectType::Form;
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
        self.attr.bbox.min.x = p1.x.min(p2.x).min(p3.x).min(p4.x);
        self.attr.bbox.min.y = p1.y.min(p2.y).min(p3.y).min(p4.y);
        self.attr.bbox.max.x = p1.x.max(p2.x).max(p3.x).max(p4.x);
        self.attr.bbox.max.y = p1.y.max(p2.y).max(p3.y).max(p4.y);
        self.reference = resource.into_ref().into_obj();
        self.resource = ptr::null_mut();
    }
    #[deprecated]
    pub(crate) unsafe fn set_form1(&mut self, form_info: &xform_info, resource: *mut pdf_obj) {
        let info = form_info;
        self.subtype = PdfXObjectType::Form;
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
        self.attr.bbox.min.x = p1.x.min(p2.x).min(p3.x).min(p4.x);
        self.attr.bbox.min.y = p1.y.min(p2.y).min(p3.y).min(p4.y);
        self.attr.bbox.max.x = p1.x.max(p2.x).max(p3.x).max(p4.x);
        self.attr.bbox.max.y = p1.y.max(p2.y).max(p3.y).max(p4.y);
        self.reference = pdf_ref_obj(resource);
        crate::release2!(resource);
        self.resource = ptr::null_mut();
    }
}

/*pub(crate) unsafe fn pdf_ximage_get_page(I: &pdf_ximage) -> i32 {
    I.attr.page_no
}*/

pub(crate) unsafe fn pdf_ximage_get_reference(id: i32) -> *mut pdf_obj {
    if id < 0 || id >= ximages.len() as i32 {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut ximages[id as usize];
    if I.reference.is_null() {
        I.reference = pdf_ref_obj(I.resource)
    }
    pdf_link_obj(I.reference)
}

#[derive(Clone)]
pub(crate) enum XInfo {
    Form(Box<xform_info>),
    #[allow(unused)]
    Image(Box<ximage_info>),
}

/* called from pdfdoc.c only for late binding */

pub(crate) unsafe fn pdf_ximage_defineresource(
    ident: &str,
    info: XInfo,
    resource: *mut pdf_obj,
) -> i32 {
    let id = ximages.len();
    let mut I = pdf_ximage::new();
    if !ident.is_empty() {
        I.ident = ident.to_string();
    }
    match info {
        XInfo::Image(info) => {
            I.set_image1(&info, resource);
            I.res_name = format!("Im{}", id);
        }
        XInfo::Form(info) => {
            I.set_form1(&info, resource);
            I.res_name = format!("Fm{}", id);
        }
    }
    ximages.push(I);
    id as i32
}

pub(crate) unsafe fn pdf_ximage_get_resname(id: i32) -> &'static str {
    if id < 0 || id >= ximages.len() as i32 {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &ximages[id as usize];
    &I.res_name
}

/*pub(crate) unsafe fn pdf_ximage_get_subtype(id: i32) -> PdfXObjectType {
    if id < 0 || id >= ximages.len() as i32 {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &ximages[id as usize];
    I.subtype
}*/
/* from spc_pdfm.c */

/*pub(crate) unsafe fn pdf_ximage_set_attr(
    id: i32,
    width: i32,
    height: i32,
    xdensity: f64,
    ydensity: f64,
    llx: f64,
    lly: f64,
    urx: f64,
    ury: f64,
) {
    if id < 0 || id >= ximages.len() as i32 {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &mut ximages[id as usize];
    I.attr.width = width;
    I.attr.height = height;
    I.attr.xdensity = xdensity;
    I.attr.ydensity = ydensity;
    I.attr.bbox = Rect::new(point2(llx, lly), point2(urx, ury));
}*/
/* depth...
 * Dvipdfm treat "depth" as "yoffset" for pdf:image and pdf:uxobj
 * not as vertical dimension of scaled image. (And there are bugs.)
 * This part contains incompatibile behaviour than dvipdfm!
 */
fn scale_to_fit_I(T: &mut TMatrix, p: &transform_info, I: &pdf_ximage) {
    let s_x;
    let s_y;
    let d_x;
    let d_y;
    let mut wd0;
    let mut ht0;
    let xscale;
    let yscale;
    if p.flags & 1 << 0 != 0 {
        wd0 = p.bbox.size().width;
        ht0 = p.bbox.size().height;
        xscale = I.attr.width as f64 * I.attr.xdensity / wd0;
        yscale = I.attr.height as f64 * I.attr.ydensity / ht0;
        d_x = -p.bbox.min.x / wd0;
        d_y = -p.bbox.min.y / ht0
    } else {
        wd0 = I.attr.width as f64 * I.attr.xdensity;
        ht0 = I.attr.height as f64 * I.attr.ydensity;
        yscale = 1.;
        xscale = yscale;
        d_x = 0.;
        d_y = 0.;
    }
    if wd0 == 0. {
        warn!("Image width=0.0!");
        wd0 = 1.;
    }
    if ht0 == 0. {
        warn!("Image height=0.0!");
        ht0 = 1.;
    }
    let dp = if p.flags & 1 << 1 != 0 && p.flags & 1 << 2 != 0 {
        s_x = p.width * xscale;
        s_y = (p.height + p.depth) * yscale;
        p.depth * yscale
    } else if p.flags & 1 << 1 != 0 {
        s_x = p.width * xscale;
        s_y = s_x * (I.attr.height as f64 / I.attr.width as f64);
        0.
    } else if p.flags & 1 << 2 != 0 {
        s_y = (p.height + p.depth) * yscale;
        s_x = s_y * (I.attr.width as f64 / I.attr.height as f64);
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

fn scale_to_fit_F(T: &mut TMatrix, p: &transform_info, I: &pdf_ximage) {
    let s_x;
    let s_y;
    let d_x;
    let d_y;
    let mut wd0;
    let mut ht0;
    if p.flags & 1 << 0 != 0 {
        wd0 = p.bbox.size().width;
        ht0 = p.bbox.size().height;
        d_x = -p.bbox.min.x;
        d_y = -p.bbox.min.y
    } else {
        wd0 = I.attr.bbox.size().width;
        ht0 = I.attr.bbox.size().height;
        d_x = 0.;
        d_y = 0.
    }
    if wd0 == 0. {
        warn!("Image width=0.0!");
        wd0 = 1.;
    }
    if ht0 == 0. {
        warn!("Image height=0.0!");
        ht0 = 1.;
    }
    let dp = if p.flags & 1 << 1 != 0 && p.flags & 1 << 2 != 0 {
        s_x = p.width / wd0;
        s_y = (p.height + p.depth) / ht0;
        p.depth
    } else if p.flags & 1 << 1 != 0 {
        s_x = p.width / wd0;
        s_y = s_x;
        0.
    } else if p.flags & 1 << 2 != 0 {
        s_y = (p.height + p.depth) / ht0;
        s_x = s_y;
        p.depth
    } else {
        s_y = 1.;
        s_x = s_y;
        0.
    };
    *T = TMatrix::row_major(s_x, 0., 0., s_y, s_x * d_x, s_y * d_y - dp);
}
/* called from pdfdev.c and spc_html.c */

pub(crate) unsafe fn pdf_ximage_scale_image(id: i32, p: &transform_info) -> (Rect, TMatrix)
/* argument from specials */ {
    let mut r = Rect::zero();
    if id < 0 || id >= ximages.len() as i32 {
        panic!("Invalid XObject ID: {}", id);
    }
    let I = &ximages[id as usize];
    let mut M = TMatrix::identity();
    match I.subtype {
        PdfXObjectType::Image => {
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
            if p.flags & 1 << 0 != 0 {
                r.min.x = p.bbox.min.x / (I.attr.width as f64 * I.attr.xdensity);
                r.min.y = p.bbox.min.y / (I.attr.height as f64 * I.attr.ydensity);
                r.max.x = p.bbox.max.x / (I.attr.width as f64 * I.attr.xdensity);
                r.max.y = p.bbox.max.y / (I.attr.height as f64 * I.attr.ydensity)
            } else {
                r = Rect::new(point2(0., 0.), point2(1., 1.));
            }
        }
        PdfXObjectType::Form => {
            /* User-defined transformation and clipping are controlled by
             * the cm operator and W operator, explicitly */
            scale_to_fit_F(&mut M, p, I); /* I->attr.bbox from the image bounding box */
            if p.flags & 1 << 0 != 0 {
                r = p.bbox;
            } else {
                r = I.attr.bbox;
            }
        }
        _ => {}
    }
    (r, M)
}
/* Migrated from psimage.c */

/*pub(crate) unsafe fn set_distiller_template(s: *mut i8) {
    free(_opts.cmdtmpl as *mut libc::c_void);
    if s.is_null() || *s as i32 == '\u{0}' as i32 {
        _opts.cmdtmpl = ptr::null_mut()
    } else {
        _opts.cmdtmpl =
            new((strlen(s).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy(_opts.cmdtmpl, s);
    };
}*/
/* NOT USED YET */
/* scale factor for bp */
/* Please use different interface than findresource...
 * This is not intended to be used for specifying page number and others.
 * Only pdf:image special in spc_pdfm.c want optinal dict!
 */
/* Called by pngimage, jpegimage, epdf, mpost, etc. */
/* from pdfximage.c */

/*pub(crate) unsafe fn get_distiller_template() -> *mut i8 {
    _opts.cmdtmpl
}*/
unsafe fn check_for_ps<R: Read + Seek>(handle: &mut R) -> i32 {
    handle.seek(SeekFrom::Start(0)).unwrap();
    if let Ok(buffer) = tt_mfreadln(1024, handle) {
        if buffer.starts_with(b"%!") {
            return 1;
        }
    }
    0
}
