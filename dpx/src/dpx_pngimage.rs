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
use libpng_sys::ffi::*;
use std::io::Read;

use std::ptr;

use crate::warn;

use super::dpx_mem::new;
use super::dpx_pdfcolor::{iccp_check_colorspace, iccp_load_profile, pdf_get_colorspace_reference};
use super::dpx_pdfximage::pdf_ximage_set_image;
use crate::dpx_pdfobj::{
    pdf_dict, pdf_get_version, pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_stream,
    pdf_stream_set_predictor, pdf_string, IntoObj, PushObj, STREAM_COMPRESS,
};
use libc::free;

use std::io::{Seek, SeekFrom};

use crate::bridge::size_t;
use bridge::InputHandleWrapper;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};

pub(crate) type png_byte = u8;
pub(crate) type png_infopp = *mut *mut png_info;
pub(crate) type png_const_charp = *const i8;
pub(crate) type png_bytep = *mut png_byte;
pub(crate) type png_uint_32 = libc::c_uint;

pub unsafe fn check_for_png<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut sigbytes: [u8; 8] = [0; 8];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if handle.read_exact(&mut sigbytes[..]).is_err()
        || png_sig_cmp(
            sigbytes.as_mut_ptr(),
            0,
            ::std::mem::size_of::<[libc::c_uchar; 8]>(),
        ) != 0
    {
        0
    } else {
        1
    }
}
unsafe extern "C" fn _png_warning_callback(
    mut _png_ptr: *mut png_struct,
    mut _msg: png_const_charp,
) {
    /* Make compiler happy */
}
unsafe extern "C" fn _png_read(png_ptr: *mut png_struct, outbytes: *mut u8, n: usize) {
    let outbytes = std::slice::from_raw_parts_mut(outbytes, n);
    let png = png_ptr.as_ref().unwrap();
    let handle =
        InputHandleWrapper::new(png_get_io_ptr(png) as tectonic_bridge::rust_input_handle_t)
            .unwrap();
    if (&handle).read_exact(outbytes).is_err() {
        panic!("error reading PNG");
    };
}

pub(crate) unsafe fn png_include_image(
    ximage: &mut pdf_ximage,
    handle: &mut InputHandleWrapper,
) -> i32 {
    /* Libpng stuff */
    let mut info = ximage_info::init();
    let mut intent = ptr::null_mut();
    let mut mask = intent;
    let mut colorspace = mask;
    handle.seek(SeekFrom::Start(0)).unwrap();

    let png = if let Some(png) = png_create_read_struct(
        b"1.6.37\x00" as *const u8 as *const i8,
        ptr::null_mut(),
        None,
        Some(_png_warning_callback),
    )
    .as_mut()
    {
        png
    } else {
        warn!("{}: Creating Libpng read struct failed.", "PNG");
        return -1i32;
    };

    let png_info = if let Some(png_info) = png_create_info_struct(png).as_mut() {
        png_info
    } else {
        warn!("{}: Creating Libpng info struct failed.", "PNG");
        png_destroy_read_struct(
            &mut (png as *mut _) as *mut *mut _,
            0 as png_infopp,
            0 as png_infopp,
        );
        return -1i32;
    };

    /* ignore possibly incorrect CMF bytes */
    png_set_option(png, 2i32, 3i32);
    /* Rust-backed IO */
    png_set_read_fn(png, handle.as_ptr(), Some(_png_read));
    /* NOTE: could use png_set_sig_bytes() to tell libpng if we started at non-zero file offset */
    /* Read PNG info-header and get some info. */
    png_read_info(png, png_info);
    let color_type = png_get_color_type(png, png_info);
    let width = png_get_image_width(png, png_info);
    let height = png_get_image_height(png, png_info);
    let mut bpc = png_get_bit_depth(png, png_info);
    if bpc as libc::c_int > 8i32 {
        if pdf_get_version() < 5i32 as libc::c_uint {
            /* Ask libpng to convert down to 8-bpc. */
            warn!("{}: 16-bpc PNG requires PDF version 1.5.", "PNG");
            png_set_strip_16(png);
            bpc = 8i32 as png_byte
        }
    } else if (bpc as i32) < 8i32 {
        /* Instruct libpng to scale each pixel color to a full byte while
        reading even though there's only 1/2/4 bits of color associated. */
        if color_type as libc::c_int == 0i32 || color_type as libc::c_int == 4i32 {
            png_set_expand_gray_1_2_4_to_8(png);
        } else {
            png_set_packing(png);
        }
        bpc = 8i32 as png_byte
    }
    /* Ask libpng to gamma-correct.
     * It is wrong to assume screen gamma value 2.2 but...
     * We do gamma correction here only when uncalibrated color space is used.
     */
    if png_get_valid(png, png_info, 0x1000u32) == 0
        && png_get_valid(png, png_info, 0x800u32) == 0
        && png_get_valid(png, png_info, 0x4u32) == 0
        && png_get_valid(png, png_info, 0x1u32) != 0
    {
        let mut G: f64 = 1.0f64;
        png_get_gAMA(png, png_info, &mut G);
        png_set_gamma(png, 2.2f64, G);
    }
    let trans_type = check_transparency(png, png_info);
    /* check_transparency() does not do updata_info() */
    png_read_update_info(png, png_info);
    let mut rowbytes = png_get_rowbytes(png, png_info) as png_uint_32;
    /* Values listed below will not be modified in the remaining process. */
    info.width = width as libc::c_int;
    info.height = height as libc::c_int;
    info.bits_per_component = bpc as libc::c_int;
    let xppm: png_uint_32 = png_get_x_pixels_per_meter(png, png_info);
    let yppm: png_uint_32 = png_get_y_pixels_per_meter(png, png_info);
    if xppm > 0 {
        info.xdensity = 72. / 0.0254 / xppm as f64
    }
    if yppm > 0 {
        info.ydensity = 72. / 0.0254 / yppm as f64
    }
    let stream = pdf_stream::new(STREAM_COMPRESS).into_obj();
    let stream_dict = (*stream).as_stream_mut().get_dict_mut();
    let stream_data_ptr = new((rowbytes.wrapping_mul(height) as u64)
        .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
        as u32) as *mut png_byte;
    read_image_data(png, stream_data_ptr, height, rowbytes);
    /* Non-NULL intent means there is valid sRGB chunk. */
    intent = get_rendering_intent(png, png_info);
    if !intent.is_null() {
        stream_dict.set("Intent", intent);
    }
    match color_type as i32 {
        3 => {
            colorspace = create_cspace_Indexed(png, png_info)
                .map(IntoObj::into_obj)
                .unwrap_or(ptr::null_mut());
            match trans_type {
                1 => {
                    /* Color-key masking */
                    mask = create_ckey_mask(png, png_info)
                        .map(IntoObj::into_obj)
                        .unwrap_or(ptr::null_mut())
                }
                2 => {
                    /* Soft mask */
                    mask = create_soft_mask(png, png_info, stream_data_ptr, width, height)
                        .map(IntoObj::into_obj)
                        .unwrap_or(ptr::null_mut())
                }
                _ => {}
            }
            info.num_components = 1i32
        }
        2 | 6 => {
            if png_get_valid(png, png_info, 0x1000u32) != 0 {
                colorspace = create_cspace_ICCBased(png, png_info)
            } else if !intent.is_null() {
                colorspace = create_cspace_sRGB(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut())
            } else {
                colorspace = create_cspace_CalRGB(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut())
            }
            if colorspace.is_null() {
                colorspace = "DeviceRGB".into_obj()
            }
            mask = match trans_type {
                1 => create_ckey_mask(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut()),
                2 => {
                    /* rowbytes changes 4 to 3 at here */
                    strip_soft_mask(png, png_info, stream_data_ptr, &mut rowbytes, width, height)
                        .map(IntoObj::into_obj)
                        .unwrap_or(ptr::null_mut())
                }
                _ => ptr::null_mut(),
            };
            info.num_components = 3i32
        }
        0 | 4 => {
            if png_get_valid(png, png_info, 0x1000u32) != 0 {
                colorspace = create_cspace_ICCBased(png, png_info)
            } else if !intent.is_null() {
                colorspace = create_cspace_sRGB(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut())
            } else {
                colorspace = create_cspace_CalGray(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut())
            }
            if colorspace.is_null() {
                colorspace = "DeviceGray".into_obj()
            }
            mask = match trans_type {
                1 => create_ckey_mask(png, png_info)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut()),
                2 => strip_soft_mask(png, png_info, stream_data_ptr, &mut rowbytes, width, height)
                    .map(IntoObj::into_obj)
                    .unwrap_or(ptr::null_mut()),
                _ => ptr::null_mut(),
            };
            info.num_components = 1i32
        }
        _ => {
            warn!("{}: Unknown PNG colortype {}.", "PNG", color_type as i32,);
        }
    }
    stream_dict.set("ColorSpace", colorspace);
    (*stream).as_stream_mut().add(
        stream_data_ptr as *const libc::c_void,
        rowbytes.wrapping_mul(height) as i32,
    );
    free(stream_data_ptr as *mut libc::c_void);
    if !mask.is_null() {
        if trans_type == 1i32 {
            stream_dict.set("Mask", mask);
        } else if trans_type == 2i32 {
            if info.bits_per_component >= 8i32 && info.width > 64i32 {
                pdf_stream_set_predictor(mask, 2i32, info.width, info.bits_per_component, 1i32);
            }
            stream_dict.set("SMask", pdf_ref_obj(mask));
            pdf_release_obj(mask);
        } else {
            warn!("{}: Unknown transparency type...???", "PNG");
            pdf_release_obj(mask);
        }
    }
    /* Finally read XMP Metadata
     * See, XMP Specification Part 3, Storage in Files
     * http://www.adobe.com/jp/devnet/xmp.html
     *
     * We require libpng version >= 1.6.14 since prior versions
     * of libpng had a bug that incorrectly treat the compression
     * flag of iTxt chunks.
     */
    if pdf_get_version() >= 4i32 as libc::c_uint {
        let mut text_ptr = ptr::null_mut();
        let mut have_XMP: libc::c_int = 0i32;
        let num_text = png_get_text(png, png_info, &mut text_ptr, &mut 0);
        for i in 0..num_text {
            if libc::memcmp(
                (*text_ptr.offset(i as isize)).key as *const libc::c_void,
                b"XML:com.adobe.xmp\x00" as *const u8 as *const i8 as *const libc::c_void,
                17usize,
            ) == 0
            {
                /* XMP found */
                if (*text_ptr.offset(i as isize)).compression != 1i32
                    || (*text_ptr.offset(i as isize)).itxt_length == 0
                {
                    warn!(
                        "{}: Invalid value(s) in iTXt chunk for XMP Metadata.",
                        "PNG",
                    );
                } else if have_XMP != 0 {
                    warn!(
                        "{}: Multiple XMP Metadata. Don\'t know how to treat it.",
                        "PNG",
                    );
                } else {
                    /* We compress XMP metadata for included images here.
                     * It is not recommended to compress XMP metadata for PDF documents but
                     * we compress XMP metadata for included images here to avoid confusing
                     * application programs that only want PDF document global XMP metadata
                     * and scan for that.
                     */
                    let mut XMP_stream = pdf_stream::new(STREAM_COMPRESS);
                    let XMP_stream_dict = XMP_stream.get_dict_mut();
                    XMP_stream_dict.set("Type", "Metadata");
                    XMP_stream_dict.set("Subtype", "XML");
                    XMP_stream.add(
                        (*text_ptr.offset(i as isize)).text as *const libc::c_void,
                        (*text_ptr.offset(i as isize)).itxt_length as i32,
                    );
                    let XMP_stream = XMP_stream.into_obj();
                    stream_dict.set("Metadata", pdf_ref_obj(XMP_stream));
                    pdf_release_obj(XMP_stream);
                    have_XMP = 1i32
                }
            }
        }
    }
    /* PNG_LIBPNG_VER */
    png_read_end(png, png_info);
    /* Cleanup */
    png_destroy_info_struct(png, &mut (png_info as *mut _) as *mut *mut _);
    png_destroy_read_struct(
        &mut (png as *mut _) as *mut *mut _,
        0 as png_infopp,
        0 as png_infopp,
    );
    if color_type as libc::c_int != 2i32 | 1i32
        && info.bits_per_component >= 8i32
        && info.height > 64i32
    {
        pdf_stream_set_predictor(
            stream,
            15i32,
            info.width,
            info.bits_per_component,
            info.num_components,
        );
    }
    pdf_ximage_set_image(ximage, &mut info, stream);
    0i32
}
/* Transparency */
/*
 * The returned value trans_type is the type of transparency to be used for
 * this image. Possible values are:
 *
 *   PDF_TRANS_TYPE_NONE    No Masking will be used/required.
 *   PDF_TRANS_TYPE_BINARY  Pixels are either fully opaque/fully transparent.
 *   PDF_TRANS_TYPE_ALPHA   Uses alpha channel, requies SMask.(PDF-1.4)
 *
 * check_transparency() must check the current setting of output PDF version
 * and must choose appropriate trans_type value according to PDF version of
 * current output PDF document.
 *
 * If the PDF version is less than 1.3, no transparency is supported for this
 * version of PDF, hence PDF_TRANS_TYPE_NONE must be returned. And when the PDF
 * version is equal to 1.3, possible retrun values are PDF_TRANS_TYPE_BINARY or
 * PDF_TRANS_TYPE_NONE. The latter case arises when PNG file uses alpha channel
 * explicitly (color type PNG_COLOR_TYPE_XXX_ALPHA), or the tRNS chunk for the
 * PNG_COLOR_TYPE_PALETTE image contains intermediate values of opacity.
 *
 * Finally, in the case of PDF version 1.4, all kind of translucent pixels can
 * be represented with Soft-Mask.
 */
unsafe fn check_transparency(png: &mut png_struct, info: &mut png_info) -> libc::c_int {
    let mut trans_type;
    let mut trans_values = ptr::null_mut();
    let mut trans: png_bytep = ptr::null_mut();
    let mut num_trans: i32 = 0;
    let pdf_version = pdf_get_version();
    let color_type = png_get_color_type(png, info);
    /*
     * First we set trans_type to appropriate value for PNG image.
     */
    if color_type as i32 == 2i32 | 4i32 || color_type as i32 == 4i32 {
        trans_type = 2i32
    } else if png_get_valid(png, info, 0x10u32) != 0
        && png_get_tRNS(png, info, &mut trans, &mut num_trans, &mut trans_values) != 0
    {
        match color_type as i32 {
            3 => {
                /* no transparency */
                /* Have valid tRNS chunk. */
                /* Use color-key mask if possible. */
                trans_type = 1i32;
                for i in (0..num_trans).rev() {
                    if !(*trans.offset(i as isize) as i32 != 0i32
                        && *trans.offset(i as isize) as i32 != 0xff)
                    {
                        continue;
                    }
                    /* This seems not binary transparency */
                    trans_type = 2i32;
                    break;
                }
            }
            0 | 2 => {
                /* RGB or GRAY, single color specified by trans_values is transparent. */
                trans_type = 1i32
            }
            _ => {
                /* Else tRNS silently ignored. */
                trans_type = 0
            }
        }
    } else {
        trans_type = 0
    }
    /*
     * Now we check PDF version.
     * We can convert alpha cahnnels to explicit mask via user supplied alpha-
     * threshold value. But I will not do that.
     */
    if pdf_version < 3 && trans_type != 0 || pdf_version < 4 && trans_type == 2 {
        /*
         *   No transparency supported but PNG uses transparency, or Soft-Mask
         * required but no support for it is available in this version of PDF.
         * We must do pre-composition of image with the background image here. But,
         * we cannot do that in general since dvipdfmx is not a rasterizer. What we
         * can do here is to composite image with a rectangle filled with the
         * background color. However, images are stored as an Image XObject which
         * can be referenced anywhere in the PDF document content. Hence, we cannot
         * know the correct background color at this time. So we will choose white
         * as background color, which is most probable color in our cases.
         * We ignore bKGD chunk.
         */
        let mut bg = png_color_16 {
            index: 0,
            red: 255,
            green: 255,
            blue: 255,
            gray: 255,
        };
        png_set_background(png, &mut bg as *mut png_color_16, 1i32, 0i32, 1.0f64);
        warn!(
            "{}: Transparency will be ignored. (no support in PDF ver. < 1.3)",
            "PNG",
        );
        if pdf_version < 3_u32 {
            warn!(
                "{}: Please use -V 3 option to enable binary transparency support.",
                "PNG"
            );
        }
        if pdf_version < 4_u32 {
            warn!(
                "{}: Please use -V 4 option to enable full alpha channel support.",
                "PNG",
            );
        }
        trans_type = 0
    }
    trans_type
}
/*
 * sRGB:
 *
 *   If sRGB chunk is present, cHRM and gAMA chunk must be ignored.
 *
 */
unsafe fn get_rendering_intent(png: &mut png_struct, info: &mut png_info) -> *mut pdf_obj {
    let mut srgb_intent: libc::c_int = 0;
    if png_get_valid(png, info, 0x800u32) != 0 && png_get_sRGB(png, info, &mut srgb_intent) != 0 {
        match srgb_intent {
            2 => "Saturation".into_obj(),
            0 => "Perceptual".into_obj(),
            3 => "AbsoluteColorimetric".into_obj(),
            1 => "RelativeColorimetric".into_obj(),
            _ => {
                warn!(
                    "{}: Invalid value in PNG sRGB chunk: {}",
                    "PNG", srgb_intent,
                );
                ptr::null_mut()
            }
        }
    } else {
        ptr::null_mut()
    }
}
/* sRGB:
 *
 * We (and PDF) do not have direct sRGB support. The sRGB color space can be
 * precisely represented by ICC profile, but we use approximate CalRGB color
 * space.
 */
/* Approximated sRGB */
unsafe fn create_cspace_sRGB(png: &png_struct, info: &png_info) -> Option<Vec<*mut pdf_obj>> {
    let color_type = png_get_color_type(png, info);
    /* Parameters taken from PNG spec. section 4.2.2.3. */
    let cal_param = make_param_Cal(
        color_type, 2.2f64, 0.3127f64, 0.329f64, 0.64f64, 0.33f64, 0.3f64, 0.6f64, 0.15f64, 0.06f64,
    )?;
    let mut colorspace = vec![];
    match color_type as i32 {
        2 | 6 | 3 => {
            colorspace.push_obj("CalRGB");
        }
        0 | 4 => {
            colorspace.push_obj("CalGray");
        }
        _ => {}
    }
    colorspace.push_obj(cal_param);
    Some(colorspace)
}
/* ICCBased:
 *
 * Not supported yet.
 * Must check if ICC profile is valid and can be imported to PDF.
 * There are few restrictions (should be applied to PNG too?) in ICC profile
 * support in PDF. Some information should be obtained from profile.
 */
unsafe fn create_cspace_ICCBased(png: &mut png_struct, png_info: &mut png_info) -> *mut pdf_obj {
    let mut name = ptr::null_mut();
    let mut compression_type: libc::c_int = 0;
    let mut profile: png_bytep = ptr::null_mut();
    let mut proflen: png_uint_32 = 0;
    if png_get_valid(png, png_info, 0x1000u32) == 0
        || png_get_iCCP(
            png,
            png_info,
            &mut name,
            &mut compression_type,
            &mut profile,
            &mut proflen,
        ) == 0
    {
        return ptr::null_mut();
    }
    let profile = core::slice::from_raw_parts(profile, proflen as usize);
    let color_type = png_get_color_type(png, png_info);
    let colortype = if color_type as libc::c_int & 2i32 != 0 {
        -3i32
    } else {
        -1i32
    };
    if iccp_check_colorspace(colortype, profile) < 0i32 {
        ptr::null_mut() /* Manual page for libpng does not
                         * clarify whether profile data is inflated by libpng.
                         */
    } else {
        let csp_id = iccp_load_profile(name as *const i8, profile);
        if csp_id < 0i32 {
            ptr::null_mut()
        } else {
            pdf_get_colorspace_reference(csp_id)
        }
    }
    /* Rendering intent ... */
}
/* CIE-Based: CalRGB/CalGray */
/*
 * gAMA, cHRM:
 *
 *   If cHRM is present, we use CIE-Based color space. gAMA is also used here
 * if available.
 */
unsafe fn create_cspace_CalRGB(
    png: &mut png_struct,
    png_info: &mut png_info,
) -> Option<Vec<*mut pdf_obj>> {
    let mut xw: f64 = 0.;
    let mut yw: f64 = 0.;
    let mut xr: f64 = 0.;
    let mut yr: f64 = 0.;
    let mut xg: f64 = 0.;
    let mut yg: f64 = 0.;
    let mut xb: f64 = 0.;
    let mut yb: f64 = 0.;
    let mut G: f64 = 0.;
    if png_get_valid(png, png_info, 0x4u32) == 0
        || png_get_cHRM(
            png, png_info, &mut xw, &mut yw, &mut xr, &mut yr, &mut xg, &mut yg, &mut xb, &mut yb,
        ) == 0
    {
        return None;
    }
    if xw <= 0.
        || yw < 1.0e-10
        || xr < 0.
        || yr < 0.
        || xg < 0.
        || yg < 0.
        || xb < 0.
        || yb < 0.
    {
        warn!("{}: Invalid cHRM chunk parameters found.", "PNG");
        return None;
    }
    if png_get_valid(png, png_info, 0x1) != 0 && png_get_gAMA(png, png_info, &mut G) != 0 {
        if G < 1e-2 {
            warn!("{}: Unusual Gamma value: 1.0 / {}", "PNG", G,);
            return None;
        }
        G = 1. / G
    /* Gamma is inverted. */
    } else {
        G = 2.2
    }
    let cal_param = make_param_Cal(2 as png_byte, G, xw, yw, xr, yr, xg, yg, xb, yb)?;
    let mut colorspace = vec![];
    colorspace.push_obj("CalRGB");
    colorspace.push_obj(cal_param);
    Some(colorspace)
}
unsafe fn create_cspace_CalGray(
    png: &mut png_struct,
    info: &mut png_info,
) -> Option<Vec<*mut pdf_obj>> {
    let mut xw: f64 = 0.;
    let mut yw: f64 = 0.;
    let mut xr: f64 = 0.;
    let mut yr: f64 = 0.;
    let mut xg: f64 = 0.;
    let mut yg: f64 = 0.;
    let mut xb: f64 = 0.;
    let mut yb: f64 = 0.;
    let mut G: f64 = 0.;
    if png_get_valid(png, info, 0x4u32) == 0
        || png_get_cHRM(
            png, info, &mut xw, &mut yw, &mut xr, &mut yr, &mut xg, &mut yg, &mut xb, &mut yb,
        ) == 0
    {
        return None;
    }
    if xw <= 0.0f64
        || yw < 1.0e-10f64
        || xr < 0.0f64
        || yr < 0.0f64
        || xg < 0.0f64
        || yg < 0.0f64
        || xb < 0.0f64
        || yb < 0.0f64
    {
        warn!("{}: Invalid cHRM chunk parameters found.", "PNG");
        return None;
    }
    if png_get_valid(png, info, 0x1u32) != 0 && png_get_gAMA(png, info, &mut G) != 0 {
        if G < 1.0e-2f64 {
            warn!("{}: Unusual Gamma value: 1.0 / {}", "PNG", G,);
            return None;
        }
        G = 1.0f64 / G
    /* Gamma is inverted. */
    } else {
        G = 2.2f64
    } /* Yw = 1.0 */
    let cal_param = make_param_Cal(0i32 as png_byte, G, xw, yw, xr, yr, xg, yg, xb, yb)?;
    let mut colorspace = vec![];
    colorspace.push_obj("CalGray");
    colorspace.push_obj(cal_param);
    Some(colorspace)
}
unsafe fn make_param_Cal(
    color_type: png_byte,
    G: f64,
    xw: f64,
    yw: f64,
    xr: f64,
    yr: f64,
    xg: f64,
    yg: f64,
    xb: f64,
    yb: f64,
) -> Option<pdf_dict> {
    /*
     * TODO: Check validity
     *
     * Conversion found in
     *
     *  com.sixlegs.image.png - Java package to read and display PNG images
     *  Copyright (C) 1998, 1999, 2001 Chris Nokleberg
     *
     *  http://www.sixlegs.com/software/png/
     *
     */
    /* WhitePoint */
    let zw = 1. - (xw + yw);
    let zr = 1. - (xr + yr);
    let zg = 1. - (xg + yg);
    let zb = 1. - (xb + yb);
    let Xw = xw / yw;
    let Yw = 1.0f64;
    let Zw = zw / yw;
    /* Matrix */
    let det = xr * (yg * zb - zg * yb) - xg * (yr * zb - zr * yb) + xb * (yr * zg - zr * yg);
    if (if det < 0i32 as f64 { -det } else { det }) < 1.0e-10f64 {
        warn!("Non invertible matrix: Maybe invalid value(s) specified in cHRM chunk.");
        return None;
    }
    let fr = (Xw * (yg * zb - zg * yb) - xg * (zb - Zw * yb) + xb * (zg - Zw * yg)) / det;
    let fg = (xr * (zb - Zw * yb) - Xw * (yr * zb - zr * yb) + xb * (yr * Zw - zr)) / det;
    let fb = (xr * (yg * Zw - zg) - xg * (yr * Zw - zr) + Xw * (yr * zg - zr * yg)) / det;
    let Xr = fr * xr;
    let Yr = fr * yr;
    let Zr = fr * zr;
    let Xg = fg * xg;
    let Yg = fg * yg;
    let Zg = fg * zg;
    let Xb = fb * xb;
    let Yb = fb * yb;
    let Zb = fb * zb;
    if G < 1.0e-2f64 {
        warn!("Unusual Gamma specified: 1.0 / {}", G);
        return None;
    }
    let mut cal_param = pdf_dict::new();
    /* White point is always required. */
    let mut white_point = vec![];
    for val in &[Xw, Yw, Zw] {
        white_point.push_obj((val / 0.00001 + 0.5).floor() * 0.00001);
    }
    cal_param.set("WhitePoint", white_point);
    /* Matrix - default: Identity */
    if color_type as i32 & 2i32 != 0 {
        if G != 1.0f64 {
            let mut dev_gamma = vec![]; /* Gray */
            for _ in 0..3 {
                dev_gamma.push_obj((G / 0.00001 + 0.5).floor() * 0.00001);
            }
            cal_param.set("Gamma", dev_gamma);
        }
        let mut matrix = vec![];
        for val in &[Xr, Yr, Zr, Xg, Yg, Zg, Xb, Yb, Zb] {
            matrix.push_obj((val / 0.00001 + 0.5).floor() * 0.00001);
        }
        cal_param.set("Matrix", matrix);
    } else if G != 1.0f64 {
        cal_param.set("Gamma", (G / 0.00001 + 0.5).floor() * 0.00001);
    }
    Some(cal_param)
}
/* ColorSpace */
/*
 * Set up Indexed ColorSpace for color-type PALETTE:
 *
 *  PNG allows only RGB color for base color space. If gAMA and/or cHRM
 *  chunk is available, we can use CalRGB color space instead of DeviceRGB
 *  for base color space.
 *
 */
unsafe fn create_cspace_Indexed(
    png: &mut png_struct,
    info: &mut png_info,
) -> Option<Vec<*mut pdf_obj>> {
    let mut plte = ptr::null_mut();
    let mut num_plte: libc::c_int = 0;
    if png_get_valid(png, info, 0x8u32) == 0
        || png_get_PLTE(png, info, &mut plte, &mut num_plte) == 0
    {
        warn!("{}: PNG does not have valid PLTE chunk.", "PNG");
        return None;
    }
    /* Order is important. */
    let mut colorspace = vec![];
    colorspace.push_obj("Indexed");
    let mut base = if png_get_valid(png, info, 0x1000u32) != 0 {
        create_cspace_ICCBased(png, info)
    } else if png_get_valid(png, info, 0x800u32) != 0 {
        create_cspace_sRGB(png, info)
            .map(IntoObj::into_obj)
            .unwrap_or(ptr::null_mut())
    } else {
        create_cspace_CalRGB(png, info)
            .map(IntoObj::into_obj)
            .unwrap_or(ptr::null_mut())
    };
    if base.is_null() {
        base = "DeviceRGB".into_obj();
    }
    colorspace.push(base);
    colorspace.push_obj((num_plte - 1) as f64);
    let data_ptr = new(((num_plte * 3) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<png_byte>() as u64) as u32)
        as *mut png_byte;
    for i in 0..num_plte {
        *data_ptr.offset((3 * i) as isize) = (*plte.offset(i as isize)).red;
        *data_ptr.offset((3 * i + 1) as isize) = (*plte.offset(i as isize)).green;
        *data_ptr.offset((3 * i + 2) as isize) = (*plte.offset(i as isize)).blue;
    }
    let lookup =
        pdf_string::new_from_ptr(data_ptr as *const libc::c_void, (num_plte * 3) as size_t);
    free(data_ptr as *mut libc::c_void);
    colorspace.push_obj(lookup);
    Some(colorspace)
}
/* Color-Key Mask */
/*
 * Colorkey Mask: array
 *
 *  [component_0_min component_0_max ... component_n_min component_n_max]
 *
 */
unsafe fn create_ckey_mask(
    png: &png_struct_def,
    png_info: &mut png_info,
) -> Option<Vec<*mut pdf_obj>> {
    let mut trans: png_bytep = ptr::null_mut();
    let mut num_trans: libc::c_int = 0;
    let mut colors = ptr::null_mut();
    if png_get_valid(png, png_info, 0x10) == 0
        || png_get_tRNS(png, png_info, &mut trans, &mut num_trans, &mut colors) == 0
    {
        warn!("{}: PNG does not have valid tRNS chunk!", "PNG");
        return None;
    }
    let mut colorkeys = vec![];
    let color_type = png_get_color_type(png, png_info);
    match color_type as libc::c_int {
        3 => {
            for i in 0..num_trans {
                if *trans.offset(i as isize) as i32 == 0 {
                    colorkeys.push_obj(i as f64);
                    colorkeys.push_obj(i as f64);
                } else if *trans.offset(i as isize) as i32 != 0xff {
                    warn!("{}: You found a bug in pngimage.c.", "PNG");
                }
            }
        }
        2 => {
            colorkeys.push_obj((*colors).red as f64);
            colorkeys.push_obj((*colors).red as f64);
            colorkeys.push_obj((*colors).green as f64);
            colorkeys.push_obj((*colors).green as f64);
            colorkeys.push_obj((*colors).blue as f64);
            colorkeys.push_obj((*colors).blue as f64);
        }
        0 => {
            colorkeys.push_obj((*colors).gray as f64);
            colorkeys.push_obj((*colors).gray as f64);
        }
        _ => {
            warn!("{}: You found a bug in pngimage.c.", "PNG");
            return None;
        }
    }
    Some(colorkeys)
}
/* Soft Mask:
 *
 * create_soft_mask() is for PNG_COLOR_TYPE_PALLETE.
 * Images with alpha chunnel use strip_soft_mask().
 * An object representing mask itself is returned.
 */
/*
 * Soft-Mask: stream
 *
 *   <<
 *      /Type             /XObject
 *      /Subtype          /Image
 *      /Width            -int-
 *      /Height           -int-
 *      /BitsPerComponent bpc
 *   >>
 *   stream .... endstream
 *
 *   ColorSpace, Mask, SMask must be absent. ImageMask must be false or absent.
 */
unsafe fn create_soft_mask(
    png: &mut png_struct_def,
    info: &mut png_info,
    image_data_ptr: png_bytep,
    width: png_uint_32,
    height: png_uint_32,
) -> Option<pdf_stream> {
    let mut trans: png_bytep = ptr::null_mut();
    let mut num_trans: i32 = 0;
    if png_get_valid(png, info, 0x10u32) == 0
        || png_get_tRNS(
            png,
            info,
            &mut trans,
            &mut num_trans,
            0 as *mut *mut png_color_16,
        ) == 0
    {
        warn!(
            "{}: PNG does not have valid tRNS chunk but tRNS is requested.",
            "PNG",
        );
        return None;
    }
    let mut smask = pdf_stream::new(STREAM_COMPRESS);
    let dict = smask.get_dict_mut();
    let smask_data_ptr = new((width.wrapping_mul(height) as u64)
        .wrapping_mul(::std::mem::size_of::<png_byte>() as u64) as u32)
        as *mut png_byte;
    dict.set("Type", "XObject");
    dict.set("Subtype", "Image");
    dict.set("Width", width as f64);
    dict.set("Height", height as f64);
    dict.set("ColorSpace", "DeviceGray");
    dict.set("BitsPerComponent", 8_f64);
    for i in 0..width.wrapping_mul(height) {
        let idx: png_byte = *image_data_ptr.offset(i as isize);
        *smask_data_ptr.offset(i as isize) = (if (idx as i32) < num_trans {
            *trans.offset(idx as isize) as i32
        } else {
            0xff
        }) as png_byte;
    }
    smask.add(
        smask_data_ptr as *mut i8 as *const libc::c_void,
        width.wrapping_mul(height) as i32,
    );
    free(smask_data_ptr as *mut libc::c_void);
    Some(smask)
}
/* bitdepth is always 8 (16 is not supported) */
unsafe fn strip_soft_mask(
    png: &png_struct,
    png_info: &png_info,
    image_data_ptr: *mut png_byte,
    rowbytes_ptr: *mut png_uint_32,
    width: png_uint_32,
    height: png_uint_32,
) -> Option<pdf_stream> {
    let color_type = png_get_color_type(png, png_info);
    let bpc = png_get_bit_depth(png, png_info);
    if color_type as libc::c_int & 2 != 0 {
        let bps: libc::c_int = if bpc as libc::c_int == 8 {
            4
        } else {
            8
        };
        if *rowbytes_ptr as u64
            != ((bps as libc::c_uint).wrapping_mul(width) as u64)
                .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
        {
            /* Something wrong */
            warn!("{}: Inconsistent rowbytes value.", "PNG");
            return None;
        }
    } else {
        let bps_0: i32 = if bpc as i32 == 8i32 { 2i32 } else { 4i32 };
        if *rowbytes_ptr as u64
            != ((bps_0 as u32).wrapping_mul(width) as u64)
                .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
        {
            /* Something wrong */
            warn!("{}: Inconsistent rowbytes value.", "PNG");
            return None;
        }
    }
    let mut smask = pdf_stream::new(STREAM_COMPRESS);
    let dict = smask.get_dict_mut();
    dict.set("Type", "XObject");
    dict.set("Subtype", "Image");
    dict.set("Width", width as f64);
    dict.set("Height", height as f64);
    dict.set("ColorSpace", "DeviceGray");
    dict.set("BitsPerComponent", bpc as f64);
    let smask_data_ptr = new((((bpc as i32 / 8) as u32)
        .wrapping_mul(width)
        .wrapping_mul(height) as u64)
        .wrapping_mul(::std::mem::size_of::<png_byte>() as u64) as u32)
        as *mut png_byte;
    match color_type as i32 {
        6 => {
            if bpc as i32 == 8i32 {
                for i in 0..width.wrapping_mul(height) {
                    libc::memmove(
                        image_data_ptr.offset((3_u32).wrapping_mul(i) as isize)
                            as *mut libc::c_void,
                        image_data_ptr.offset((4_u32).wrapping_mul(i) as isize)
                            as *const libc::c_void,
                        3usize,
                    );
                    *smask_data_ptr.offset(i as isize) = *image_data_ptr
                        .offset((4_u32).wrapping_mul(i).wrapping_add(3_u32) as isize);
                }
                *rowbytes_ptr = ((3_u32).wrapping_mul(width) as u64)
                    .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
                    as png_uint_32
            } else {
                for i in 0..width.wrapping_mul(height) {
                    libc::memmove(
                        image_data_ptr.offset((6_u32).wrapping_mul(i) as isize)
                            as *mut libc::c_void,
                        image_data_ptr.offset((8_u32).wrapping_mul(i) as isize)
                            as *const libc::c_void,
                        6usize,
                    );
                    *smask_data_ptr.offset((2_u32).wrapping_mul(i) as isize) = *image_data_ptr
                        .offset((8_u32).wrapping_mul(i).wrapping_add(6_u32) as isize);
                    *smask_data_ptr.offset((2_u32).wrapping_mul(i).wrapping_add(1_u32) as isize) =
                        *image_data_ptr
                            .offset((8_u32).wrapping_mul(i).wrapping_add(7_u32) as isize);
                }
                *rowbytes_ptr = ((6_u32).wrapping_mul(width) as u64)
                    .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
                    as png_uint_32
            }
        }
        4 => {
            if bpc as i32 == 8i32 {
                for i in 0..width.wrapping_mul(height) {
                    *image_data_ptr.offset(i as isize) =
                        *image_data_ptr.offset((2_u32).wrapping_mul(i) as isize);
                    *smask_data_ptr.offset(i as isize) = *image_data_ptr
                        .offset((2_u32).wrapping_mul(i).wrapping_add(1_u32) as isize);
                }
                *rowbytes_ptr = (width as u64)
                    .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
                    as png_uint_32
            } else {
                for i in 0..width.wrapping_mul(height) {
                    *image_data_ptr.offset((2_u32).wrapping_mul(i) as isize) =
                        *image_data_ptr.offset((4_u32).wrapping_mul(i) as isize);
                    *image_data_ptr.offset((2_u32).wrapping_mul(i).wrapping_add(1_u32) as isize) =
                        *image_data_ptr
                            .offset((4_u32).wrapping_mul(i).wrapping_add(1_u32) as isize);
                    *smask_data_ptr.offset((2_u32).wrapping_mul(i) as isize) = *image_data_ptr
                        .offset((4_u32).wrapping_mul(i).wrapping_add(2_u32) as isize);
                    *smask_data_ptr.offset((2_u32).wrapping_mul(i).wrapping_add(1_u32) as isize) =
                        *image_data_ptr
                            .offset((4_u32).wrapping_mul(i).wrapping_add(3_u32) as isize);
                }
                *rowbytes_ptr = ((2_u32).wrapping_mul(width) as u64)
                    .wrapping_mul(::std::mem::size_of::<png_byte>() as u64)
                    as png_uint_32
            }
        }
        _ => {
            warn!("You found a bug in pngimage.c!");
            free(smask_data_ptr as *mut libc::c_void);
            return None;
        }
    }
    smask.add(
        smask_data_ptr as *const libc::c_void,
        ((bpc as i32 / 8i32) as u32)
            .wrapping_mul(width)
            .wrapping_mul(height) as i32,
    );
    free(smask_data_ptr as *mut libc::c_void);
    Some(smask)
}
/* Read image body */
unsafe fn read_image_data(
    png: &mut png_struct,
    dest_ptr: png_bytep,
    height: png_uint_32,
    rowbytes: png_uint_32,
) {
    let rows_p = new((height as u64).wrapping_mul(::std::mem::size_of::<png_bytep>() as u64) as u32)
        as *mut png_bytep;
    for i in 0..height {
        *rows_p.offset(i as isize) = dest_ptr.offset(rowbytes.wrapping_mul(i) as isize);
    }
    png_read_image(png, rows_p);
    free(rows_p as *mut libc::c_void);
}

pub unsafe fn png_get_bbox(handle: &InputHandleWrapper) -> Result<(u32, u32, f64, f64), ()> {
    (&*handle).seek(SeekFrom::Start(0)).unwrap();
    let png = png_create_read_struct(
        b"1.6.37\x00" as *const u8 as *const i8,
        ptr::null_mut(),
        None,
        Some(_png_warning_callback),
    )
    .as_mut();
    let mut png_info = None;
    if png.is_none() || {
        png_info = png_create_info_struct(png.as_ref().unwrap()).as_mut();
        png_info.is_none()
    } {
        warn!("{}: Creating Libpng read/info struct failed.", "PNG");
        if let Some(png) = png {
            png_destroy_read_struct(
                &mut (png as *mut _) as *mut *mut _,
                0 as png_infopp,
                0 as png_infopp,
            );
        }
        return Err(());
    }

    let png = png.unwrap();
    let png_info = png_info.unwrap();

    /* Rust-backed IO */
    png_set_read_fn(png, handle.as_ptr(), Some(_png_read));
    /* NOTE: could use png_set_sig_bytes() to tell libpng if we started at non-zero file offset */
    /* Read PNG info-header and get some info. */
    png_read_info(png, png_info);
    let width = png_get_image_width(png, png_info);
    let height = png_get_image_height(png, png_info);
    let xppm: png_uint_32 = png_get_x_pixels_per_meter(png, png_info);
    let yppm: png_uint_32 = png_get_y_pixels_per_meter(png, png_info);
    let xdensity = if xppm != 0 {
        72. / 0.0254 / xppm as f64
    } else {
        1.
    };
    let ydensity = if yppm != 0 {
        72. / 0.0254 / yppm as f64
    } else {
        1.
    };
    /* Cleanup */
    png_destroy_info_struct(png, &mut (png_info as *mut png_info) as png_infopp);
    png_destroy_read_struct(
        &mut (png as *mut png_struct) as _,
        0 as png_infopp,
        0 as png_infopp,
    );
    Ok((width, height, xdensity, ydensity))
}
