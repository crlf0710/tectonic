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

use crate::mfree;
use crate::warn;

use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer;
use super::dpx_numbers::{tt_get_unsigned_byte, tt_get_unsigned_pair};
use super::dpx_pdfcolor::{
    iccp_check_colorspace, iccp_get_rendering_intent, iccp_load_profile,
    pdf_get_colorspace_reference,
};
use super::dpx_pdfximage::{pdf_ximage_init_image_info, pdf_ximage_set_image};
use crate::dpx_pdfobj::{
    pdf_get_version, pdf_new_name, pdf_new_number, pdf_new_stream, pdf_obj, pdf_ref_obj,
    pdf_release_obj, pdf_stream_dataptr, pdf_stream_length, IntoObj, STREAM_COMPRESS,
};
use crate::{ttstub_input_get_size, ttstub_input_getc, ttstub_input_read};
use libc::{free, memcmp, memset};

use std::io::{Seek, SeekFrom};
use std::ptr;

pub type __ssize_t = i64;
pub type size_t = u64;
use bridge::InputHandleWrapper;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
pub const JM_SOI: JPEG_marker = 216;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_info {
    pub height: u16,
    pub width: u16,
    pub bits_per_component: u8,
    pub num_components: u8,
    pub xdpi: f64,
    pub ydpi: f64,
    pub flags: i32,
    pub num_appn: i32,
    pub max_appn: i32,
    pub appn: *mut JPEG_ext,
    pub skipbits: [i8; 129],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_ext {
    pub marker: JPEG_marker,
    pub app_sig: JPEG_APPn_sig,
    pub app_data: *mut libc::c_void,
}
pub type JPEG_APPn_sig = u32;
pub const JS_APPn_XMP: JPEG_APPn_sig = 3;
pub const JS_APPn_ICC: JPEG_APPn_sig = 2;
pub const JS_APPn_ADOBE: JPEG_APPn_sig = 1;
pub const JS_APPn_JFIF: JPEG_APPn_sig = 0;
pub type JPEG_marker = u32;
pub const JM_COM: JPEG_marker = 254;
pub const JM_APP15: JPEG_marker = 239;
pub const JM_APP14: JPEG_marker = 238;
pub const JM_APP2: JPEG_marker = 226;
pub const JM_APP1: JPEG_marker = 225;
pub const JM_APP0: JPEG_marker = 224;
pub const JM_EXP: JPEG_marker = 223;
pub const JM_DHP: JPEG_marker = 222;
pub const JM_DRI: JPEG_marker = 221;
pub const JM_DNL: JPEG_marker = 220;
pub const JM_DQT: JPEG_marker = 219;
pub const JM_SOS: JPEG_marker = 218;
pub const JM_EOI: JPEG_marker = 217;
pub const JM_RST7: JPEG_marker = 215;
pub const JM_RST6: JPEG_marker = 214;
pub const JM_RST5: JPEG_marker = 213;
pub const JM_RST4: JPEG_marker = 212;
pub const JM_RST3: JPEG_marker = 211;
pub const JM_RST2: JPEG_marker = 210;
pub const JM_RST1: JPEG_marker = 209;
pub const JM_RST0: JPEG_marker = 208;
pub const JM_SOF15: JPEG_marker = 207;
pub const JM_SOF14: JPEG_marker = 206;
pub const JM_SOF13: JPEG_marker = 205;
pub const JM_DAC: JPEG_marker = 204;
pub const JM_SOF11: JPEG_marker = 203;
pub const JM_SOF10: JPEG_marker = 202;
pub const JM_SOF9: JPEG_marker = 201;
pub const JM_SOF7: JPEG_marker = 199;
pub const JM_SOF6: JPEG_marker = 198;
pub const JM_DHT: JPEG_marker = 196;
pub const JM_SOF5: JPEG_marker = 197;
pub const JM_SOF3: JPEG_marker = 195;
pub const JM_SOF2: JPEG_marker = 194;
pub const JM_SOF1: JPEG_marker = 193;
pub const JM_SOF0: JPEG_marker = 192;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_APPn_XMP {
    pub packet: *mut u8,
    pub length: size_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_APPn_Adobe {
    pub version: u16,
    pub flag0: u16,
    pub flag1: u16,
    pub transform: u8,
    /* color transform code */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_APPn_ICC {
    pub seq_id: u8,
    pub num_chunks: u8,
    pub chunk: *mut u8,
    pub length: size_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct JPEG_APPn_JFIF {
    pub version: u16,
    pub units: u8,
    pub Xdensity: u16,
    pub Ydensity: u16,
    pub Xthumbnail: u8,
    pub Ythumbnail: u8,
    pub thumbnail: *mut u8,
    /* Thumbnail data. */
}
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

pub unsafe fn check_for_jpeg(handle: &mut InputHandleWrapper) -> i32 {
    let mut jpeg_sig: [u8; 2] = [0; 2];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if ttstub_input_read(
        handle.0.as_ptr(),
        jpeg_sig.as_mut_ptr() as *mut i8,
        2i32 as size_t,
    ) != 2i32 as i64
    {
        return 0i32;
    } else {
        if jpeg_sig[0] as i32 != 0xffi32 || jpeg_sig[1] as i32 != JM_SOI as i32 {
            return 0i32;
        }
    }
    1i32
}

pub unsafe fn jpeg_include_image(ximage: *mut pdf_ximage, handle: &mut InputHandleWrapper) -> i32 {
    let mut info = ximage_info::default();
    let mut j_info: JPEG_info = JPEG_info {
        height: 0,
        width: 0,
        bits_per_component: 0,
        num_components: 0,
        xdpi: 0.,
        ydpi: 0.,
        flags: 0,
        num_appn: 0,
        max_appn: 0,
        appn: ptr::null_mut(),
        skipbits: [0; 129],
    };
    if check_for_jpeg(handle) == 0 {
        warn!("{}: Not a JPEG file?", "JPEG");
        handle.seek(SeekFrom::Start(0)).unwrap();
        return -1i32;
    }
    /* File position is 2 here... */
    pdf_ximage_init_image_info(&mut info);
    JPEG_info_init(&mut j_info);
    if JPEG_scan_file(&mut j_info, handle) < 0i32 {
        warn!("{}: Not a JPEG file?", "JPEG");
        JPEG_info_clear(&mut j_info);
        return -1i32;
    }
    let colortype = match j_info.num_components as i32 {
        1 => -1,
        3 => -3,
        4 => -4,
        _ => {
            warn!(
                "{}: Unknown color space (num components: {})",
                "JPEG", info.num_components,
            );
            JPEG_info_clear(&mut j_info);
            return -1i32;
        }
    };
    /* JPEG image use DCTDecode. */
    let stream = pdf_new_stream(0i32);
    let stream_dict = (*stream).as_stream_mut().get_dict_mut();
    stream_dict.set("Filter", pdf_new_name("DCTDecode"));
    /* XMP Metadata */
    if pdf_get_version() >= 4_u32 {
        if j_info.flags & 1i32 << 4i32 != 0 {
            let XMP_stream = JPEG_get_XMP(&mut j_info);
            stream_dict.set("Metadata", pdf_ref_obj(XMP_stream));
            pdf_release_obj(XMP_stream);
        }
    }
    /* Check embedded ICC Profile */
    let mut colorspace = ptr::null_mut();
    if j_info.flags & 1i32 << 2i32 != 0 {
        let icc_stream = JPEG_get_iccp(&mut j_info);
        if icc_stream.is_null() {
            colorspace = ptr::null_mut()
        } else {
            let icc_stream = &*icc_stream;
            if iccp_check_colorspace(
                colortype,
                pdf_stream_dataptr(icc_stream),
                pdf_stream_length(icc_stream),
            ) < 0i32
            {
                colorspace = ptr::null_mut()
            } else {
                let cspc_id = iccp_load_profile(
                    std::ptr::null(),
                    pdf_stream_dataptr(icc_stream),
                    pdf_stream_length(icc_stream),
                );
                if cspc_id < 0i32 {
                    colorspace = ptr::null_mut()
                } else {
                    colorspace = pdf_get_colorspace_reference(cspc_id);
                    let intent = iccp_get_rendering_intent(
                        pdf_stream_dataptr(icc_stream),
                        pdf_stream_length(icc_stream),
                    );
                    if !intent.is_null() {
                        stream_dict.set("Intent", intent);
                    }
                }
            }
            pdf_release_obj(icc_stream as *const pdf_obj as *mut pdf_obj);
        }
    }
    /* No ICC or invalid ICC profile. */
    if colorspace.is_null() {
        match colortype {
            -1 => colorspace = pdf_new_name("DeviceGray"),
            -3 => colorspace = pdf_new_name("DeviceRGB"),
            -4 => colorspace = pdf_new_name("DeviceCMYK"),
            _ => {}
        }
    }
    stream_dict.set("ColorSpace", colorspace);
    if j_info.flags & 1i32 << 1i32 != 0 && j_info.num_components as i32 == 4i32 {
        warn!("Adobe CMYK JPEG: Inverted color assumed.");
        let mut decode = vec![];
        for _ in 0..j_info.num_components as u32 {
            decode.push(pdf_new_number(1.0f64));
            decode.push(pdf_new_number(0.0f64));
        }
        stream_dict.set("Decode", decode.into_obj());
    }
    /* Copy file */
    JPEG_copy_stream(&mut j_info, stream, handle);
    info.width = j_info.width as i32;
    info.height = j_info.height as i32;
    info.bits_per_component = j_info.bits_per_component as i32;
    info.num_components = j_info.num_components as i32;
    jpeg_get_density(&mut j_info, &mut info.xdensity, &mut info.ydensity);
    pdf_ximage_set_image(ximage, &mut info, stream);
    JPEG_info_clear(&mut j_info);
    0i32
}
unsafe fn jpeg_get_density(mut j_info: *mut JPEG_info, xdensity: *mut f64, ydensity: *mut f64) {
    /*
     * j_info->xdpi and j_info->ydpi are determined in most cases
     * in JPEG_scan_file(). FIXME: However, in some kinds of JPEG files,
     * j_info->xdpi, and j_info->ydpi are not determined in
     * JPEG_scan_file(). In this case we assume
     * that j_info->xdpi = j_info->ydpi = 72.0.
     */
    if (*j_info).xdpi < 0.1f64 && (*j_info).ydpi < 0.1f64 {
        (*j_info).ydpi = 72.0f64;
        (*j_info).xdpi = (*j_info).ydpi
    }
    *xdensity = 72.0f64 / (*j_info).xdpi;
    *ydensity = 72.0f64 / (*j_info).ydpi;
}
unsafe fn JPEG_info_init(mut j_info: *mut JPEG_info) {
    (*j_info).width = 0_u16;
    (*j_info).height = 0_u16;
    (*j_info).bits_per_component = 0_u8;
    (*j_info).num_components = 0_u8;
    (*j_info).xdpi = 0.0f64;
    (*j_info).ydpi = 0.0f64;
    (*j_info).flags = 0i32;
    (*j_info).num_appn = 0i32;
    (*j_info).max_appn = 0i32;
    (*j_info).appn = ptr::null_mut();
    memset(
        (*j_info).skipbits.as_mut_ptr() as *mut libc::c_void,
        0i32,
        1024 / 8 + 1,
    );
}
unsafe fn JPEG_release_APPn_data(
    marker: JPEG_marker,
    app_sig: JPEG_APPn_sig,
    app_data: *mut libc::c_void,
) {
    if marker as u32 == JM_APP0 as i32 as u32 && app_sig as u32 == JS_APPn_JFIF as i32 as u32 {
        let data = app_data as *mut JPEG_APPn_JFIF;
        (*data).thumbnail = mfree((*data).thumbnail as *mut libc::c_void) as *mut u8;
        free(data as *mut libc::c_void);
    } else if marker as u32 == JM_APP2 as i32 as u32 && app_sig as u32 == JS_APPn_ICC as i32 as u32
    {
        let data_0 = app_data as *mut JPEG_APPn_ICC;
        (*data_0).chunk = mfree((*data_0).chunk as *mut libc::c_void) as *mut u8;
        free(data_0 as *mut libc::c_void);
    } else if marker as u32 == JM_APP14 as i32 as u32
        && app_sig as u32 == JS_APPn_ADOBE as i32 as u32
    {
        let data_1 = app_data as *mut JPEG_APPn_Adobe;
        free(data_1 as *mut libc::c_void);
    } else if marker as u32 == JM_APP1 as i32 as u32 && app_sig as u32 == JS_APPn_XMP as i32 as u32
    {
        let data_2 = app_data as *mut JPEG_APPn_XMP;
        free((*data_2).packet as *mut libc::c_void);
        free(data_2 as *mut libc::c_void);
    };
}
unsafe fn JPEG_info_clear(mut j_info: *mut JPEG_info) {
    if (*j_info).num_appn > 0i32 && !(*j_info).appn.is_null() {
        for i in 0..(*j_info).num_appn {
            JPEG_release_APPn_data(
                (*(*j_info).appn.offset(i as isize)).marker,
                (*(*j_info).appn.offset(i as isize)).app_sig,
                (*(*j_info).appn.offset(i as isize)).app_data,
            );
        }
        free((*j_info).appn as *mut libc::c_void);
    }
    (*j_info).appn = ptr::null_mut();
    (*j_info).num_appn = 0i32;
    (*j_info).max_appn = 0i32;
    (*j_info).flags = 0i32;
}
unsafe fn JPEG_get_iccp(j_info: *mut JPEG_info) -> *mut pdf_obj {
    let mut prev_id: i32 = 0i32;
    let mut num_icc_seg: i32 = -1i32;
    let mut icc_stream = pdf_new_stream(STREAM_COMPRESS);
    for i in 0..(*j_info).num_appn {
        if !((*(*j_info).appn.offset(i as isize)).marker as u32 != JM_APP2 as i32 as u32
            || (*(*j_info).appn.offset(i as isize)).app_sig as u32 != JS_APPn_ICC as i32 as u32)
        {
            let icc = (*(*j_info).appn.offset(i as isize)).app_data as *mut JPEG_APPn_ICC;
            /*if num_icc_seg < 0i32 && prev_id == 0i32 { // TODO: check unused
                num_icc_seg = (*icc).num_chunks as i32
            /* ICC chunks are sorted? */
            } else */
            if (*icc).seq_id as i32 != prev_id + 1i32
                || num_icc_seg != (*icc).num_chunks as i32
                || (*icc).seq_id as i32 > (*icc).num_chunks as i32
            {
                warn!(
                    "Invalid JPEG ICC chunk: {} (p:{}, n:{})",
                    (*icc).seq_id as i32,
                    prev_id,
                    (*icc).num_chunks as i32,
                );
                pdf_release_obj(icc_stream);
                icc_stream = ptr::null_mut();
                break;
            }
            (*icc_stream)
                .as_stream_mut()
                .add((*icc).chunk as *const libc::c_void, (*icc).length as i32);
            prev_id = (*icc).seq_id as i32;
            num_icc_seg = (*icc).num_chunks as i32
        }
    }
    icc_stream
}
unsafe fn JPEG_get_XMP(j_info: *mut JPEG_info) -> *mut pdf_obj {
    let mut count: i32 = 0i32;
    /* I don't know if XMP Metadata should be compressed here.*/
    let XMP_stream = pdf_new_stream(STREAM_COMPRESS);
    let stream_dict = (*XMP_stream).as_stream_mut().get_dict_mut();
    stream_dict.set("Type", pdf_new_name("Metadata"));
    stream_dict.set("Subtype", pdf_new_name("XML"));
    for i in 0..(*j_info).num_appn {
        /* Not sure for the case of multiple segments */
        if !((*(*j_info).appn.offset(i as isize)).marker as u32 != JM_APP1 as i32 as u32
            || (*(*j_info).appn.offset(i as isize)).app_sig as u32 != JS_APPn_XMP as i32 as u32)
        {
            let XMP = (*(*j_info).appn.offset(i as isize)).app_data as *mut JPEG_APPn_XMP;
            (*XMP_stream)
                .as_stream_mut()
                .add((*XMP).packet as *const libc::c_void, (*XMP).length as i32);
            count += 1
        }
    }
    if count > 1i32 {
        warn!(
            "{}: Multiple XMP segments found in JPEG file. (untested)",
            "JPEG",
        );
    }
    XMP_stream
}
unsafe fn JPEG_get_marker(handle: &mut InputHandleWrapper) -> JPEG_marker {
    let mut c = ttstub_input_getc(handle);
    if c != 255i32 {
        return 4294967295 as JPEG_marker;
    }
    loop {
        c = ttstub_input_getc(handle);
        if c < 0i32 {
            return 4294967295 as JPEG_marker;
        } else {
            if c > 0i32 && c < 255i32 {
                return c as JPEG_marker;
            }
        }
    }
}
unsafe fn add_APPn_marker(
    mut j_info: *mut JPEG_info,
    marker: JPEG_marker,
    app_sig: i32,
    app_data: *mut libc::c_void,
) -> i32 {
    if (*j_info).num_appn >= (*j_info).max_appn {
        (*j_info).max_appn += 16i32;
        (*j_info).appn = renew(
            (*j_info).appn as *mut libc::c_void,
            ((*j_info).max_appn as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<JPEG_ext>() as u64) as u32,
        ) as *mut JPEG_ext
    }
    let n = (*j_info).num_appn;
    (*(*j_info).appn.offset(n as isize)).marker = marker;
    (*(*j_info).appn.offset(n as isize)).app_sig = app_sig as JPEG_APPn_sig;
    let ref mut fresh0 = (*(*j_info).appn.offset(n as isize)).app_data;
    *fresh0 = app_data;
    (*j_info).num_appn += 1i32;
    n
}
unsafe fn read_APP14_Adobe(j_info: *mut JPEG_info, handle: &mut InputHandleWrapper) -> u16 {
    let app_data = new((1_u64).wrapping_mul(::std::mem::size_of::<JPEG_APPn_Adobe>() as u64) as u32)
        as *mut JPEG_APPn_Adobe;
    (*app_data).version = tt_get_unsigned_pair(handle);
    (*app_data).flag0 = tt_get_unsigned_pair(handle);
    (*app_data).flag1 = tt_get_unsigned_pair(handle);
    (*app_data).transform = tt_get_unsigned_byte(handle);
    add_APPn_marker(
        j_info,
        JM_APP14,
        JS_APPn_ADOBE as i32,
        app_data as *mut libc::c_void,
    );
    7_u16
}
unsafe fn read_exif_bytes(pp: *mut *mut u8, n: i32, endian: i32) -> i32 {
    let mut rval: i32 = 0i32;
    let p: *mut u8 = *pp;
    match endian {
        0 => {
            for i in 0..n {
                rval = (rval << 8i32) + *p.offset(i as isize) as i32;
            }
        }
        1 => {
            for i in (0..n).rev() {
                rval = (rval << 8i32) + *p.offset(i as isize) as i32;
            }
        }
        _ => {}
    }
    *pp = (*pp).offset(n as isize);
    rval
}
unsafe fn read_APP1_Exif(
    mut info: *mut JPEG_info,
    handle: &mut InputHandleWrapper,
    length: size_t,
) -> size_t {
    let bigendian: i8;
    let mut type_0;
    let mut value;
    let mut num: i32 = 0i32;
    let mut den: i32 = 0i32;
    let mut xres: f64 = 0.0f64;
    let mut yres: f64 = 0.0f64;
    let mut res_unit: f64 = 1.0f64;
    let mut xres_ms: u32 = 0_u32;
    let mut yres_ms: u32 = 0_u32;
    let mut res_unit_ms: f64 = 0.0f64;
    let mut buffer_box: Box<[u8]> = vec![0u8; length as usize].into_boxed_slice(); // auto destruct
    let buffer = buffer_box.as_mut_ptr();
    let r = ttstub_input_read(handle.0.as_ptr(), buffer as *mut i8, length);
    if r < 0 || r as size_t != length {
        return length;
    }

    let mut p = buffer;
    let endptr = buffer.offset(length as isize);
    while p < buffer.offset(length as isize) && *p == 0 {
        p = p.offset(1)
    }
    if !(p.offset(8) >= endptr) {
        return length;
    }

    let tiff_header = p;
    if *p as i32 == 'M' as i32 && *p.offset(1) as i32 == 'M' as i32 {
        bigendian = 0_i8;
    } else if *p as i32 == 'I' as i32 && *p.offset(1) as i32 == 'I' as i32 {
        bigendian = 1_i8;
    } else {
        warn!("JPEG: Invalid value in Exif TIFF header.");
        return length;
    }

    p = p.offset(2);
    let mut i = read_exif_bytes(&mut p, 2i32, bigendian as i32);
    if i != 42 {
        warn!("JPEG: Invalid value in Exif TIFF header.");
        return length;
    }

    i = read_exif_bytes(&mut p, 4i32, bigendian as i32);
    p = tiff_header.offset(i as isize);
    let mut num_fields = read_exif_bytes(&mut p, 2i32, bigendian as i32);
    while num_fields > 0 {
        num_fields -= num_fields - 1;

        let tag = read_exif_bytes(&mut p, 2, bigendian as i32);
        type_0 = read_exif_bytes(&mut p, 2, bigendian as i32);
        let count = read_exif_bytes(&mut p, 4, bigendian as i32);
        match type_0 {
            1 => {
                value = *p as i32;
                p = p.offset(4)
            }
            3 => {
                value = read_exif_bytes(&mut p, 2, bigendian as i32);
                p = p.offset(2)
            }
            4 | 9 => value = read_exif_bytes(&mut p, 4, bigendian as i32),
            5 | 10 => {
                value = read_exif_bytes(&mut p, 4, bigendian as i32);
                let mut rp = tiff_header.offset(value as isize);
                num = read_exif_bytes(&mut rp, 4, bigendian as i32);
                den = read_exif_bytes(&mut rp, 4, bigendian as i32)
            }
            7 => {
                value = *p as i32;
                p = p.offset(4);
            }
            2 | _ => {
                value = 0;
                p = p.offset(4)
            }
        }
        match tag {
            282 => {
                if den != 0 {
                    xres = (num / den) as f64
                }
            }
            283 => {
                if den != 0 {
                    yres = (num / den) as f64
                }
            }
            296 => {
                match value {
                    2 => {
                        /* inch */
                        res_unit = 1.0f64
                    }
                    3 => {
                        /* cm */
                        res_unit = 2.54f64
                    }
                    _ => {}
                }
            }
            20752 => {
                /* PixelUnit */
                if type_0 != 1i32 || count != 1i32 {
                    warn!("{}: Invalid data for ResolutionUnit in Exif chunk.", "JPEG",);
                    return length;
                } else {
                    value = read_exif_bytes(&mut p, 1, bigendian as i32);
                    p = p.offset(3);
                    if value == 1 {
                        res_unit_ms = 0.0254f64; /* Unit is meter */
                    } else {
                        res_unit_ms = 0.0f64;
                    }
                }
            }
            20753 => {
                /* PixelPerUnitX */
                if type_0 != 4 || count != 1 {
                    warn!("{}: Invalid data for PixelPerUnitX in Exif chunk.", "JPEG",);
                    return length;
                } else {
                    value = read_exif_bytes(&mut p, 4, bigendian as i32);
                    xres_ms = value as u32;
                }
            }
            20754 => {
                /* PixelPerUnitY */
                if type_0 != 4 || count != 1 {
                    warn!("{}: Invalid data for PixelPerUnitY in Exif chunk.", "JPEG",);
                    return length;
                } else {
                    value = read_exif_bytes(&mut p, 4, bigendian as i32);
                    yres_ms = value as u32;
                }
            }
            _ => {}
        }
    }

    /* Calculate Exif resolution, if given. */

    let exifxdpi;
    let exifydpi;
    if xres > 0.0 && yres > 0.0 {
        exifxdpi = xres * res_unit;
        exifydpi = yres * res_unit;
    } else if xres_ms > 0 && yres_ms > 0 && res_unit_ms > 0.0 {
        exifxdpi = xres_ms as f64 * res_unit_ms;
        exifydpi = yres_ms as f64 * res_unit_ms;
    } else {
        exifxdpi = 72.0 * res_unit;
        exifydpi = 72.0 * res_unit;
    };

    /* Do not overwrite if already specified in JFIF */

    if (*info).xdpi < 0.1 && (*info).ydpi < 0.1 {
        (*info).xdpi = exifxdpi;
        (*info).ydpi = exifydpi;
    } else {
        let xxx1: f64 = (exifxdpi + 0.5).floor();
        let xxx2: f64 = ((*info).xdpi + 0.5).floor();
        let yyy1: f64 = (exifydpi + 0.5).floor();
        let yyy2: f64 = ((*info).ydpi + 0.5).floor();
        if xxx1 != xxx2 || yyy1 != yyy2 {
            warn!(
                "JPEG: Inconsistent resolution may have been specified in Exif and JFIF: {}x{} - {}x{}",
                xres * res_unit,
                yres * res_unit,
                (*info).xdpi,
                (*info).ydpi,
            );
        }
    }

    length
}
unsafe fn read_APP0_JFIF(j_info: *mut JPEG_info, handle: &mut InputHandleWrapper) -> size_t {
    let app_data = new((1_u64).wrapping_mul(::std::mem::size_of::<JPEG_APPn_JFIF>() as u64) as u32)
        as *mut JPEG_APPn_JFIF;
    (*app_data).version = tt_get_unsigned_pair(handle);
    (*app_data).units = tt_get_unsigned_byte(handle);
    (*app_data).Xdensity = tt_get_unsigned_pair(handle);
    (*app_data).Ydensity = tt_get_unsigned_pair(handle);
    (*app_data).Xthumbnail = tt_get_unsigned_byte(handle);
    (*app_data).Ythumbnail = tt_get_unsigned_byte(handle);
    let thumb_data_len =
        (3i32 * (*app_data).Xthumbnail as i32 * (*app_data).Ythumbnail as i32) as size_t;
    if thumb_data_len > 0i32 as u64 {
        (*app_data).thumbnail = new((thumb_data_len as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64)
            as u32) as *mut u8;
        ttstub_input_read(
            handle.0.as_ptr(),
            (*app_data).thumbnail as *mut i8,
            thumb_data_len,
        );
    } else {
        (*app_data).thumbnail = ptr::null_mut()
    }
    add_APPn_marker(
        j_info,
        JM_APP0,
        JS_APPn_JFIF as i32,
        app_data as *mut libc::c_void,
    );
    match (*app_data).units as i32 {
        1 => {
            (*j_info).xdpi = (*app_data).Xdensity as f64;
            (*j_info).ydpi = (*app_data).Ydensity as f64
        }
        2 => {
            /* density is in pixels per cm */
            (*j_info).xdpi = (*app_data).Xdensity as i32 as f64 * 2.54f64;
            (*j_info).ydpi = (*app_data).Ydensity as i32 as f64 * 2.54f64
        }
        _ => {
            /* FIXME: not sure what to do with this.... */
            (*j_info).xdpi = 72.0f64;
            (*j_info).ydpi = 72.0f64
        }
    }
    (9i32 as u64).wrapping_add(thumb_data_len)
}
unsafe fn read_APP0_JFXX(handle: &mut InputHandleWrapper, length: size_t) -> size_t {
    tt_get_unsigned_byte(handle);
    /* Extension Code:
     *
     * 0x10: Thumbnail coded using JPEG
     * 0x11: Thumbnail stored using 1 byte/pixel
     * 0x13: Thumbnail stored using 3 bytes/pixel
     */
    handle.seek(SeekFrom::Current(length as i64 - 1)).unwrap(); /* Thunbnail image */
    /* Ignore */
    return length; /* Starting at 1 */
}
unsafe fn read_APP1_XMP(
    j_info: *mut JPEG_info,
    handle: &mut InputHandleWrapper,
    length: size_t,
) -> size_t {
    let app_data = new((1_u64).wrapping_mul(::std::mem::size_of::<JPEG_APPn_XMP>() as u64) as u32)
        as *mut JPEG_APPn_XMP;
    (*app_data).length = length;
    (*app_data).packet = new(
        ((*app_data).length as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32
    ) as *mut u8;
    ttstub_input_read(
        handle.0.as_ptr(),
        (*app_data).packet as *mut i8,
        (*app_data).length,
    );
    add_APPn_marker(
        j_info,
        JM_APP1,
        JS_APPn_XMP as i32,
        app_data as *mut libc::c_void,
    );
    length
}
unsafe fn read_APP2_ICC(
    j_info: *mut JPEG_info,
    handle: &mut InputHandleWrapper,
    length: size_t,
) -> size_t {
    let app_data = new((1_u64).wrapping_mul(::std::mem::size_of::<JPEG_APPn_ICC>() as u64) as u32)
        as *mut JPEG_APPn_ICC;
    (*app_data).seq_id = tt_get_unsigned_byte(handle);
    (*app_data).num_chunks = tt_get_unsigned_byte(handle);
    (*app_data).length = length.wrapping_sub(2i32 as u64);
    (*app_data).chunk = new(
        ((*app_data).length as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32
    ) as *mut u8;
    ttstub_input_read(
        handle.0.as_ptr(),
        (*app_data).chunk as *mut i8,
        (*app_data).length,
    );
    add_APPn_marker(
        j_info,
        JM_APP2,
        JS_APPn_ICC as i32,
        app_data as *mut libc::c_void,
    );
    length
}
unsafe fn JPEG_copy_stream(
    j_info: *mut JPEG_info,
    stream: *mut pdf_obj,
    handle: &mut InputHandleWrapper,
) -> i32 {
    let mut marker: JPEG_marker = 0 as JPEG_marker;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let mut count = 0i32;
    let mut found_SOFn = 0i32;
    while found_SOFn == 0 && count < 1024i32 && {
        marker = JPEG_get_marker(handle);
        marker as u32 != 4294967295 as JPEG_marker as u32
    } {
        if marker as u32 == JM_SOI as i32 as u32
            || marker as u32 >= JM_RST0 as i32 as u32 && marker as u32 <= JM_RST7 as i32 as u32
        {
            *work_buffer.as_mut_ptr().offset(0) = 0xffi32 as i8;
            *work_buffer.as_mut_ptr().offset(1) = marker as i8;
            (*stream)
                .as_stream_mut()
                .add(work_buffer.as_mut_ptr() as *const libc::c_void, 2i32);
        } else {
            let mut length = tt_get_unsigned_pair(handle) as i32 - 2i32;
            match marker as u32 {
                192 | 193 | 194 | 195 | 197 | 198 | 199 | 201 | 202 | 203 | 205 | 206 | 207 => {
                    *work_buffer.as_mut_ptr().offset(0) = 0xffi32 as i8;
                    *work_buffer.as_mut_ptr().offset(1) = marker as i8;
                    *work_buffer.as_mut_ptr().offset(2) = (length + 2i32 >> 8i32 & 0xffi32) as i8;
                    *work_buffer.as_mut_ptr().offset(3) = (length + 2i32 & 0xffi32) as i8;
                    (*stream)
                        .as_stream_mut()
                        .add(work_buffer.as_mut_ptr() as *const libc::c_void, 4i32);
                    while length > 0i32 {
                        let nb_read: i32 = ttstub_input_read(
                            handle.0.as_ptr(),
                            work_buffer.as_mut_ptr(),
                            (if length < 1024i32 { length } else { 1024i32 }) as size_t,
                        ) as i32;
                        if nb_read > 0i32 {
                            (*stream)
                                .as_stream_mut()
                                .add(work_buffer.as_mut_ptr() as *const libc::c_void, nb_read);
                        }
                        length -= nb_read
                    }
                    found_SOFn = 1i32
                }
                _ => {
                    if (*j_info).skipbits[(count / 8i32) as usize] as i32
                        & 1i32 << 7i32 - count % 8i32
                        != 0
                    {
                        handle.seek(SeekFrom::Current(length as i64)).unwrap();
                    } else {
                        *work_buffer.as_mut_ptr().offset(0) = 0xffi32 as i8;
                        *work_buffer.as_mut_ptr().offset(1) = marker as i8;
                        *work_buffer.as_mut_ptr().offset(2) =
                            (length + 2i32 >> 8i32 & 0xffi32) as i8;
                        *work_buffer.as_mut_ptr().offset(3) = (length + 2i32 & 0xffi32) as i8;
                        (*stream)
                            .as_stream_mut()
                            .add(work_buffer.as_mut_ptr() as *const libc::c_void, 4i32);
                        while length > 0i32 {
                            let nb_read_0: i32 = ttstub_input_read(
                                handle.0.as_ptr(),
                                work_buffer.as_mut_ptr(),
                                (if length < 1024i32 { length } else { 1024i32 }) as size_t,
                            ) as i32;
                            if nb_read_0 > 0i32 {
                                (*stream).as_stream_mut().add(
                                    work_buffer.as_mut_ptr() as *const libc::c_void,
                                    nb_read_0,
                                );
                            }
                            length -= nb_read_0
                        }
                    }
                }
            }
        }
        count += 1
    }
    let total_size: size_t = ttstub_input_get_size(handle);
    let mut pos = handle.seek(SeekFrom::Current(0)).unwrap();
    loop {
        let length = ttstub_input_read(
            handle.0.as_ptr(),
            work_buffer.as_mut_ptr(),
            if (1024i32 as u64) < total_size.wrapping_sub(pos) {
                1024i32 as u64
            } else {
                total_size.wrapping_sub(pos)
            },
        ) as i32;
        if !(length > 0i32) {
            break;
        }
        (*stream)
            .as_stream_mut()
            .add(work_buffer.as_mut_ptr() as *const libc::c_void, length);
        pos = (pos as u64).wrapping_add(length as u64) as size_t as size_t
    }
    if found_SOFn != 0 {
        0i32
    } else {
        -1i32
    }
}
unsafe fn JPEG_scan_file(mut j_info: *mut JPEG_info, handle: &mut InputHandleWrapper) -> i32 {
    let mut marker: JPEG_marker = 0 as JPEG_marker;
    let mut app_sig: [i8; 128] = [0; 128];
    handle.seek(SeekFrom::Start(0)).unwrap();
    let mut count = 0i32;
    let mut found_SOFn = 0i32;
    while found_SOFn == 0 && {
        marker = JPEG_get_marker(handle);
        marker as u32 != 4294967295 as JPEG_marker as u32
    } {
        if marker as u32 != JM_SOI as i32 as u32
            && ((marker as u32) < JM_RST0 as i32 as u32 || marker as u32 > JM_RST7 as i32 as u32)
        {
            let mut length: i32 = tt_get_unsigned_pair(handle) as i32 - 2i32;
            match marker as u32 {
                192 | 193 | 194 | 195 | 197 | 198 | 199 | 201 | 202 | 203 | 205 | 206 | 207 => {
                    (*j_info).bits_per_component = tt_get_unsigned_byte(handle);
                    (*j_info).height = tt_get_unsigned_pair(handle);
                    (*j_info).width = tt_get_unsigned_pair(handle);
                    (*j_info).num_components = tt_get_unsigned_byte(handle);
                    found_SOFn = 1i32
                }
                224 => {
                    if length > 5i32 {
                        if ttstub_input_read(
                            handle.0.as_ptr(),
                            app_sig.as_mut_ptr(),
                            5i32 as size_t,
                        ) != 5i32 as i64
                        {
                            return -1i32;
                        }
                        length -= 5i32;
                        if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"JFIF\x00\x00" as *const u8 as *const i8 as *const libc::c_void,
                            5,
                        ) == 0
                        {
                            (*j_info).flags |= 1i32 << 0i32;
                            length = (length as u64).wrapping_sub(read_APP0_JFIF(j_info, handle))
                                as i32 as i32
                        } else if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"JFXX\x00" as *const u8 as *const i8 as *const libc::c_void,
                            5,
                        ) == 0
                        {
                            length = (length as u64)
                                .wrapping_sub(read_APP0_JFXX(handle, length as size_t))
                                as i32 as i32
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                225 => {
                    if length > 5i32 {
                        if ttstub_input_read(
                            handle.0.as_ptr(),
                            app_sig.as_mut_ptr(),
                            5i32 as size_t,
                        ) != 5i32 as i64
                        {
                            return -1i32;
                        }
                        length -= 5i32;
                        if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"Exif\x00\x00" as *const u8 as *const i8 as *const libc::c_void,
                            5,
                        ) == 0
                        {
                            (*j_info).flags |= 1i32 << 3i32;
                            length = (length as u64).wrapping_sub(read_APP1_Exif(
                                j_info,
                                handle,
                                length as size_t,
                            )) as i32 as i32
                        } else if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"http:\x00" as *const u8 as *const i8 as *const libc::c_void,
                            5,
                        ) == 0
                            && length > 24i32
                        {
                            if ttstub_input_read(
                                handle.0.as_ptr(),
                                app_sig.as_mut_ptr(),
                                24i32 as size_t,
                            ) != 24i32 as i64
                            {
                                return -1i32;
                            }
                            length -= 24i32;
                            if memcmp(
                                app_sig.as_mut_ptr() as *const libc::c_void,
                                b"//ns.adobe.com/xap/1.0/\x00\x00" as *const u8 as *const i8
                                    as *const libc::c_void,
                                24,
                            ) == 0
                            {
                                (*j_info).flags |= 1i32 << 4i32;
                                length = (length as u64).wrapping_sub(read_APP1_XMP(
                                    j_info,
                                    handle,
                                    length as size_t,
                                )) as i32 as i32;
                                if count < 1024i32 {
                                    (*j_info).skipbits[(count / 8i32) as usize] =
                                        ((*j_info).skipbits[(count / 8i32) as usize] as i32
                                            | 1i32 << 7i32 - count % 8i32)
                                            as i8
                                }
                            }
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                226 => {
                    if length >= 14i32 {
                        if ttstub_input_read(
                            handle.0.as_ptr(),
                            app_sig.as_mut_ptr(),
                            12i32 as size_t,
                        ) != 12i32 as i64
                        {
                            return -1i32;
                        }
                        length -= 12i32;
                        if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"ICC_PROFILE\x00\x00" as *const u8 as *const i8 as *const libc::c_void,
                            12,
                        ) == 0
                        {
                            (*j_info).flags |= 1i32 << 2i32;
                            length = (length as u64).wrapping_sub(read_APP2_ICC(
                                j_info,
                                handle,
                                length as size_t,
                            )) as i32 as i32;
                            if count < 1024i32 {
                                (*j_info).skipbits[(count / 8i32) as usize] =
                                    ((*j_info).skipbits[(count / 8i32) as usize] as i32
                                        | 1i32 << 7i32 - count % 8i32)
                                        as i8
                            }
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                238 => {
                    if length > 5i32 {
                        if ttstub_input_read(
                            handle.0.as_ptr(),
                            app_sig.as_mut_ptr(),
                            5i32 as size_t,
                        ) != 5i32 as i64
                        {
                            return -1i32;
                        }
                        length -= 5i32;
                        if memcmp(
                            app_sig.as_mut_ptr() as *const libc::c_void,
                            b"Adobe\x00" as *const u8 as *const i8 as *const libc::c_void,
                            5,
                        ) == 0
                        {
                            (*j_info).flags |= 1i32 << 1i32;
                            length -= read_APP14_Adobe(j_info, handle) as i32
                        } else if count < 1024i32 {
                            (*j_info).skipbits[(count / 8i32) as usize] =
                                ((*j_info).skipbits[(count / 8i32) as usize] as i32
                                    | 1i32 << 7i32 - count % 8i32)
                                    as i8
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                _ => {
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                    if marker as u32 >= JM_APP0 as i32 as u32
                        && marker as u32 <= JM_APP15 as i32 as u32
                    {
                        if count < 1024i32 {
                            (*j_info).skipbits[(count / 8i32) as usize] =
                                ((*j_info).skipbits[(count / 8i32) as usize] as i32
                                    | 1i32 << 7i32 - count % 8i32)
                                    as i8
                        }
                    }
                }
            }
        }
        count += 1
    }
    /*
     * If j_info->xdpi, and j_info->ydpi are not yet determined,
     * they are assumed to be 72.0 to avoid division by zero.
     */
    if (*j_info).xdpi < 0.1f64 && (*j_info).ydpi < 0.1f64 {
        (*j_info).ydpi = 72.0f64;
        (*j_info).xdpi = (*j_info).ydpi
    }
    if found_SOFn != 0 {
        0i32
    } else {
        -1i32
    }
}

pub unsafe fn jpeg_get_bbox(
    handle: &mut InputHandleWrapper,
    width: *mut u32,
    height: *mut u32,
    xdensity: *mut f64,
    ydensity: *mut f64,
) -> i32 {
    let mut j_info: JPEG_info = JPEG_info {
        height: 0,
        width: 0,
        bits_per_component: 0,
        num_components: 0,
        xdpi: 0.,
        ydpi: 0.,
        flags: 0,
        num_appn: 0,
        max_appn: 0,
        appn: ptr::null_mut(),
        skipbits: [0; 129],
    };
    JPEG_info_init(&mut j_info);
    if JPEG_scan_file(&mut j_info, handle) < 0i32 {
        warn!("{}: Not a JPEG file?", "JPEG");
        JPEG_info_clear(&mut j_info);
        return -1i32;
    }
    *width = j_info.width as u32;
    *height = j_info.height as u32;
    jpeg_get_density(&mut j_info, xdensity, ydensity);
    JPEG_info_clear(&mut j_info);
    0i32
}
