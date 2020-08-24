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

use crate::warn;

use super::dpx_numbers::GetFromFile;
use super::dpx_pdfcolor::{
    iccp_check_colorspace, iccp_get_rendering_intent, iccp_load_profile,
    pdf_get_colorspace_reference,
};
use super::dpx_pdfximage::pdf_ximage_set_image;
use crate::bridge::{ttstub_input_get_size, ttstub_input_getc};
use crate::dpx_pdfobj::{
    pdf_get_version, pdf_ref_obj, pdf_release_obj, pdf_stream, IntoObj, PushObj, STREAM_COMPRESS,
};
use libc::memset;

use crate::bridge::size_t;
use std::io::{Read, Seek, SeekFrom};
use std::ptr;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
pub(crate) const JM_SOI: JPEG_marker = 216;
#[derive(Clone)]
pub(crate) struct JPEG_info {
    pub(crate) height: u16,
    pub(crate) width: u16,
    pub(crate) bits_per_component: u8,
    pub(crate) num_components: u8,
    pub(crate) xdpi: f64,
    pub(crate) ydpi: f64,
    pub(crate) flags: i32,
    pub(crate) appn: Vec<JPEG_ext>,
    pub(crate) skipbits: [i8; 129],
}
#[derive(Clone)]
pub(crate) struct JPEG_ext {
    pub(crate) marker: JPEG_marker,
    pub(crate) app_data: AppData,
}

#[derive(Clone)]
pub(crate) enum AppData {
    JFIF(Box<JPEG_APPn_JFIF>),
    ADOBE(Box<JPEG_APPn_Adobe>),
    ICC(Box<JPEG_APPn_ICC>),
    XMP(Box<JPEG_APPn_XMP>),
}

pub(crate) type JPEG_APPn_sig = u32;
pub(crate) const JS_APPn_XMP: JPEG_APPn_sig = 3;
pub(crate) const JS_APPn_ICC: JPEG_APPn_sig = 2;
pub(crate) const JS_APPn_ADOBE: JPEG_APPn_sig = 1;
pub(crate) const JS_APPn_JFIF: JPEG_APPn_sig = 0;
pub(crate) type JPEG_marker = u8;
pub(crate) const JM_COM: JPEG_marker = 254;
pub(crate) const JM_APP15: JPEG_marker = 239;
pub(crate) const JM_APP14: JPEG_marker = 238;
pub(crate) const JM_APP2: JPEG_marker = 226;
pub(crate) const JM_APP1: JPEG_marker = 225;
pub(crate) const JM_APP0: JPEG_marker = 224;
pub(crate) const JM_EXP: JPEG_marker = 223;
pub(crate) const JM_DHP: JPEG_marker = 222;
pub(crate) const JM_DRI: JPEG_marker = 221;
pub(crate) const JM_DNL: JPEG_marker = 220;
pub(crate) const JM_DQT: JPEG_marker = 219;
pub(crate) const JM_SOS: JPEG_marker = 218;
pub(crate) const JM_EOI: JPEG_marker = 217;
pub(crate) const JM_RST7: JPEG_marker = 215;
pub(crate) const JM_RST6: JPEG_marker = 214;
pub(crate) const JM_RST5: JPEG_marker = 213;
pub(crate) const JM_RST4: JPEG_marker = 212;
pub(crate) const JM_RST3: JPEG_marker = 211;
pub(crate) const JM_RST2: JPEG_marker = 210;
pub(crate) const JM_RST1: JPEG_marker = 209;
pub(crate) const JM_RST0: JPEG_marker = 208;
pub(crate) const JM_SOF15: JPEG_marker = 207;
pub(crate) const JM_SOF14: JPEG_marker = 206;
pub(crate) const JM_SOF13: JPEG_marker = 205;
pub(crate) const JM_DAC: JPEG_marker = 204;
pub(crate) const JM_SOF11: JPEG_marker = 203;
pub(crate) const JM_SOF10: JPEG_marker = 202;
pub(crate) const JM_SOF9: JPEG_marker = 201;
pub(crate) const JM_SOF7: JPEG_marker = 199;
pub(crate) const JM_SOF6: JPEG_marker = 198;
pub(crate) const JM_DHT: JPEG_marker = 196;
pub(crate) const JM_SOF5: JPEG_marker = 197;
pub(crate) const JM_SOF3: JPEG_marker = 195;
pub(crate) const JM_SOF2: JPEG_marker = 194;
pub(crate) const JM_SOF1: JPEG_marker = 193;
pub(crate) const JM_SOF0: JPEG_marker = 192;
#[derive(Clone)]
pub(crate) struct JPEG_APPn_XMP {
    pub(crate) packet: Vec<u8>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct JPEG_APPn_Adobe {
    pub(crate) version: u16,
    pub(crate) flag0: u16,
    pub(crate) flag1: u16,
    pub(crate) transform: u8,
    /* color transform code */
}
#[derive(Clone)]
pub(crate) struct JPEG_APPn_ICC {
    pub(crate) seq_id: u8,
    pub(crate) num_chunks: u8,
    pub(crate) chunk: Vec<u8>,
}
#[derive(Clone)]
pub(crate) struct JPEG_APPn_JFIF {
    pub(crate) version: u16,
    pub(crate) units: u8,
    pub(crate) Xdensity: u16,
    pub(crate) Ydensity: u16,
    pub(crate) Xthumbnail: u8,
    pub(crate) Ythumbnail: u8,
    pub(crate) thumbnail: Vec<u8>,
    /* Thumbnail data. */
}
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

pub unsafe fn check_for_jpeg<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut jpeg_sig: [u8; 2] = [0; 2];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if handle.read_exact(jpeg_sig.as_mut()).is_err() {
        return 0;
    } else {
        if jpeg_sig[0] != 0xff || jpeg_sig[1] != JM_SOI {
            return 0;
        }
    }
    1
}

pub(crate) unsafe fn jpeg_include_image<R: Read + Seek>(
    ximage: &mut pdf_ximage,
    handle: &mut R,
) -> i32 {
    let mut j_info: JPEG_info = JPEG_info {
        height: 0,
        width: 0,
        bits_per_component: 0,
        num_components: 0,
        xdpi: 0.,
        ydpi: 0.,
        flags: 0,
        appn: Vec::new(),
        skipbits: [0; 129],
    };
    if check_for_jpeg(handle) == 0 {
        warn!("{}: Not a JPEG file?", "JPEG");
        handle.seek(SeekFrom::Start(0)).unwrap();
        return -1i32;
    }
    /* File position is 2 here... */
    let mut info = ximage_info::init();
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
    let mut stream = pdf_stream::new(0i32);
    let stream_dict = stream.get_dict_mut();
    stream_dict.set("Filter", "DCTDecode");
    /* XMP Metadata */
    if pdf_get_version() >= 4 {
        if j_info.flags & 1 << 4 != 0 {
            let XMP_stream = JPEG_get_XMP(&mut j_info).into_obj();
            stream_dict.set("Metadata", pdf_ref_obj(XMP_stream));
            pdf_release_obj(XMP_stream);
        }
    }
    /* Check embedded ICC Profile */
    let mut colorspace = ptr::null_mut();
    if j_info.flags & 1 << 2 != 0 {
        if let Some(icc_stream) = JPEG_get_iccp(&mut j_info) {
            if iccp_check_colorspace(colortype, &icc_stream.content) < 0 {
                colorspace = ptr::null_mut()
            } else {
                let cspc_id = iccp_load_profile(std::ptr::null(), &icc_stream.content);
                if cspc_id < 0 {
                    colorspace = ptr::null_mut()
                } else {
                    colorspace = pdf_get_colorspace_reference(cspc_id);
                    let intent = iccp_get_rendering_intent(&icc_stream.content);
                    if !intent.is_null() {
                        stream_dict.set("Intent", intent);
                    }
                }
            }
        } else {
            colorspace = ptr::null_mut()
        }
    }
    /* No ICC or invalid ICC profile. */
    if colorspace.is_null() {
        match colortype {
            -1 => colorspace = "DeviceGray".into_obj(),
            -3 => colorspace = "DeviceRGB".into_obj(),
            -4 => colorspace = "DeviceCMYK".into_obj(),
            _ => {}
        }
    }
    stream_dict.set("ColorSpace", colorspace);
    if j_info.flags & 1 << 1 != 0 && j_info.num_components as i32 == 4 {
        warn!("Adobe CMYK JPEG: Inverted color assumed.");
        let mut decode = vec![];
        for _ in 0..j_info.num_components as u32 {
            decode.push_obj(1_f64);
            decode.push_obj(0_f64);
        }
        stream_dict.set("Decode", decode);
    }
    /* Copy file */
    JPEG_copy_stream(&mut j_info, &mut stream, handle);
    info.width = j_info.width as i32;
    info.height = j_info.height as i32;
    info.bits_per_component = j_info.bits_per_component as i32;
    info.num_components = j_info.num_components as i32;
    let (xdensity, ydensity) = jpeg_get_density(&mut j_info);
    info.xdensity = xdensity;
    info.ydensity = ydensity;
    pdf_ximage_set_image(ximage, &mut info, stream.into_obj());
    JPEG_info_clear(&mut j_info);
    0i32
}
unsafe fn jpeg_get_density(mut j_info: *mut JPEG_info) -> (f64, f64) {
    /*
     * j_info->xdpi and j_info->ydpi are determined in most cases
     * in JPEG_scan_file(). FIXME: However, in some kinds of JPEG files,
     * j_info->xdpi, and j_info->ydpi are not determined in
     * JPEG_scan_file(). In this case we assume
     * that j_info->xdpi = j_info->ydpi = 72.0.
     */
    if (*j_info).xdpi < 0.1 && (*j_info).ydpi < 0.1 {
        (*j_info).ydpi = 72.;
        (*j_info).xdpi = (*j_info).ydpi
    }
    (72. / (*j_info).xdpi, 72. / (*j_info).ydpi)
}
unsafe fn JPEG_info_init(mut j_info: *mut JPEG_info) {
    (*j_info).width = 0_u16;
    (*j_info).height = 0_u16;
    (*j_info).bits_per_component = 0_u8;
    (*j_info).num_components = 0_u8;
    (*j_info).xdpi = 0.0f64;
    (*j_info).ydpi = 0.0f64;
    (*j_info).flags = 0i32;
    (*j_info).appn = Vec::new();
    memset(
        (*j_info).skipbits.as_mut_ptr() as *mut libc::c_void,
        0i32,
        1024 / 8 + 1,
    );
}
unsafe fn JPEG_info_clear(mut j_info: *mut JPEG_info) {
    (*j_info).appn = Vec::new();
    (*j_info).flags = 0;
}
unsafe fn JPEG_get_iccp(j_info: *mut JPEG_info) -> Option<pdf_stream> {
    let mut prev_id = 0_i32;
    let mut num_icc_seg = -1_i32;
    let mut icc_stream = pdf_stream::new(STREAM_COMPRESS);
    for app in &(*j_info).appn {
        if app.marker != JM_APP2 {
            continue;
        }
        if let AppData::ICC(icc) = &app.app_data {
            if num_icc_seg < 0 && prev_id == 0 {
            } else if icc.seq_id as i32 != prev_id + 1
                || num_icc_seg != icc.num_chunks as i32
                || icc.seq_id as i32 > icc.num_chunks as i32
            {
                /* ICC chunks are sorted? */
                warn!(
                    "Invalid JPEG ICC chunk: {} (p:{}, n:{})",
                    icc.seq_id as i32, prev_id, icc.num_chunks as i32,
                );
                return None;
            }
            icc_stream.add_slice(&icc.chunk);
            prev_id = icc.seq_id as i32;
            num_icc_seg = icc.num_chunks as i32
        }
    }
    Some(icc_stream)
}
unsafe fn JPEG_get_XMP(j_info: *mut JPEG_info) -> pdf_stream {
    let mut count: i32 = 0i32;
    /* I don't know if XMP Metadata should be compressed here.*/
    let mut XMP_stream = pdf_stream::new(STREAM_COMPRESS);
    let stream_dict = XMP_stream.get_dict_mut();
    stream_dict.set("Type", "Metadata");
    stream_dict.set("Subtype", "XML");
    for app in &(*j_info).appn {
        /* Not sure for the case of multiple segments */
        if app.marker != JM_APP1 {
            continue;
        }
        if let AppData::XMP(xmp) = &app.app_data {
            XMP_stream.add_slice(&xmp.packet);
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
unsafe fn JPEG_get_marker<R: Read>(handle: &mut R) -> Option<JPEG_marker> {
    let mut c = ttstub_input_getc(handle);
    if c != 0xff {
        return None;
    }
    loop {
        c = ttstub_input_getc(handle);
        if c < 0 {
            return None;
        } else {
            if c > 0 && c < 255 {
                return Some(c as JPEG_marker);
            }
        }
    }
}
unsafe fn add_APPn_marker(j_info: *mut JPEG_info, marker: JPEG_marker, app_data: AppData) {
    (*j_info).appn.push(JPEG_ext { marker, app_data });
}
unsafe fn read_APP14_Adobe<R: Read>(j_info: *mut JPEG_info, handle: &mut R) -> u16 {
    let version = u16::get(handle);
    let flag0 = u16::get(handle);
    let flag1 = u16::get(handle);
    let transform = u8::get(handle);
    let app_data = Box::new(JPEG_APPn_Adobe {
        version,
        flag0,
        flag1,
        transform,
    });
    add_APPn_marker(j_info, JM_APP14, AppData::ADOBE(app_data));
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
unsafe fn read_APP1_Exif<R: Read>(
    mut info: *mut JPEG_info,
    handle: &mut R,
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
    let mut buffer = vec![0u8; length as usize];
    if handle.read_exact(&mut buffer).is_err() {
        return length;
    }

    let buffer = buffer.as_mut_ptr();
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
                        res_unit = 1.;
                    }
                    3 => {
                        /* cm */
                        res_unit = 2.54;
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
                    res_unit_ms = if value == 1 {
                        0.0254 /* Unit is meter */
                    } else {
                        0.
                    };
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
unsafe fn read_APP0_JFIF<R: Read>(j_info: *mut JPEG_info, handle: &mut R) -> size_t {
    let version = u16::get(handle);
    let units = u8::get(handle);
    let Xdensity = u16::get(handle);
    let Ydensity = u16::get(handle);
    let Xthumbnail = u8::get(handle);
    let Ythumbnail = u8::get(handle);
    let thumb_data_len = (3i32 * Xthumbnail as i32 * Ythumbnail as i32) as size_t;
    let mut thumbnail = vec![0; thumb_data_len as usize];
    if thumb_data_len > 0 {
        handle.read_exact(&mut thumbnail);
    }

    let app_data = Box::new(JPEG_APPn_JFIF {
        version,
        units,
        Xdensity,
        Ydensity,
        Xthumbnail,
        Ythumbnail,
        thumbnail,
    });

    add_APPn_marker(j_info, JM_APP0, AppData::JFIF(app_data));
    match units as i32 {
        1 => {
            (*j_info).xdpi = Xdensity as f64;
            (*j_info).ydpi = Ydensity as f64
        }
        2 => {
            /* density is in pixels per cm */
            (*j_info).xdpi = Xdensity as i32 as f64 * 2.54;
            (*j_info).ydpi = Ydensity as i32 as f64 * 2.54;
        }
        _ => {
            /* FIXME: not sure what to do with this.... */
            (*j_info).xdpi = 72.;
            (*j_info).ydpi = 72.;
        }
    }
    (9i32 as u64).wrapping_add(thumb_data_len as _) as _
}
unsafe fn read_APP0_JFXX<R: Read + Seek>(handle: &mut R, length: size_t) -> size_t {
    u8::get(handle);
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
unsafe fn read_APP1_XMP<R: Read>(j_info: *mut JPEG_info, handle: &mut R, length: size_t) -> size_t {
    let mut packet = vec![0; length];
    handle.read_exact(&mut packet).unwrap();
    let app_data = Box::new(JPEG_APPn_XMP { packet });
    add_APPn_marker(j_info, JM_APP1, AppData::XMP(app_data));
    length
}
unsafe fn read_APP2_ICC<R: Read>(j_info: *mut JPEG_info, handle: &mut R, length: size_t) -> size_t {
    let seq_id = u8::get(handle);
    let num_chunks = u8::get(handle);
    let mut chunk = vec![0; length as usize - 2];
    handle.read_exact(&mut chunk).unwrap();
    let app_data = Box::new(JPEG_APPn_ICC {
        seq_id,
        num_chunks,
        chunk,
    });
    add_APPn_marker(j_info, JM_APP2, AppData::ICC(app_data));
    length
}
unsafe fn JPEG_copy_stream<R: Read + Seek>(
    j_info: *mut JPEG_info,
    stream: &mut pdf_stream,
    handle: &mut R,
) -> i32 {
    handle.seek(SeekFrom::Start(0)).unwrap();
    let mut count = 0;
    let mut found_SOFn = 0;
    let mut buffer = Vec::with_capacity(1024);
    while found_SOFn == 0 && count < 1024 {
        let marker = JPEG_get_marker(handle);
        if marker.is_none() {
            break;
        }
        let marker = marker.unwrap();
        match marker {
            JM_SOI | JM_RST0..=JM_RST7 => stream.add_slice([0xff, marker as u8].as_ref()),
            JM_SOF0 | JM_SOF1 | JM_SOF2 | JM_SOF3 | JM_SOF5 | JM_SOF6 | JM_SOF7 | JM_SOF9
            | JM_SOF10 | JM_SOF11 | JM_SOF13 | JM_SOF14 | JM_SOF15 => {
                let mut length = u16::get(handle) as i32 - 2;
                stream.add_slice(
                    [
                        0xff,
                        marker as u8,
                        (length + 2 >> 8 & 0xff) as u8,
                        (length + 2 & 0xff) as u8,
                    ]
                    .as_ref(),
                );
                while length > 0 {
                    let nb_read = length.min(1024);
                    buffer.resize_with(nb_read as usize, Default::default);
                    if handle.read_exact(&mut buffer).is_ok() {
                        stream.add_slice(&buffer);
                    }
                    buffer.clear();
                    length -= nb_read;
                }
                found_SOFn = 1;
            }
            _ => {
                let mut length = u16::get(handle) as i32 - 2;
                if (*j_info).skipbits[(count / 8) as usize] as i32 & 1 << 7 - count % 8 != 0 {
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                } else {
                    stream.add_slice(
                        [
                            0xff,
                            marker as u8,
                            (length + 2 >> 8 & 0xff) as u8,
                            (length + 2 & 0xff) as u8,
                        ]
                        .as_ref(),
                    );
                    while length > 0 {
                        let nb_read = length.min(1024);
                        buffer.resize_with(nb_read as usize, Default::default);
                        if handle.read_exact(&mut buffer).is_ok() {
                            stream.add_slice(&buffer);
                        }
                        buffer.clear();
                        length -= nb_read;
                    }
                }
            }
        }
        count += 1;
    }
    let total_size = ttstub_input_get_size(handle) as usize;
    let mut pos = handle.seek(SeekFrom::Current(0)).unwrap() as usize;
    loop {
        let length = (total_size - pos).min(1024);
        buffer.resize_with(length, Default::default);
        handle.read_exact(&mut buffer).unwrap();
        stream.add_slice(&buffer);
        pos += length;
        if total_size - pos == 0 {
            break;
        }
    }
    if found_SOFn != 0 {
        0
    } else {
        -1
    }
}
unsafe fn JPEG_scan_file<R: Read + Seek>(mut j_info: *mut JPEG_info, handle: &mut R) -> i32 {
    let mut app_sig: [u8; 128] = [0; 128];
    handle.seek(SeekFrom::Start(0)).unwrap();
    let mut count = 0i32;
    let mut found_SOFn = 0i32;
    while found_SOFn == 0 {
        let marker = JPEG_get_marker(handle);
        if marker.is_none() {
            break;
        }
        let marker = marker.unwrap();
        if marker as u32 != JM_SOI as i32 as u32
            && ((marker as u32) < JM_RST0 as i32 as u32 || marker as u32 > JM_RST7 as i32 as u32)
        {
            let mut length: i32 = u16::get(handle) as i32 - 2i32;
            match marker {
                JM_SOF0 | JM_SOF1 | JM_SOF2 | JM_SOF3 | JM_SOF5 | JM_SOF6 | JM_SOF7 | JM_SOF9
                | JM_SOF10 | JM_SOF11 | JM_SOF13 | JM_SOF14 | JM_SOF15 => {
                    (*j_info).bits_per_component = u8::get(handle);
                    (*j_info).height = u16::get(handle);
                    (*j_info).width = u16::get(handle);
                    (*j_info).num_components = u8::get(handle);
                    found_SOFn = 1i32
                }
                JM_APP0 => {
                    if length > 5 {
                        if handle.read_exact(&mut app_sig[..5]).is_err() {
                            return -1;
                        }
                        length -= 5;
                        if app_sig.starts_with(b"JFIF\x00") {
                            (*j_info).flags |= 1 << 0;
                            length = (length as u64)
                                .wrapping_sub(read_APP0_JFIF(j_info, handle) as _)
                                as i32 as i32
                        } else if app_sig.starts_with(b"JFXX\x00") {
                            length =
                                (length as u64)
                                    .wrapping_sub(read_APP0_JFXX(handle, length as size_t) as _)
                                    as i32 as i32
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                JM_APP1 => {
                    if length > 5 {
                        if handle.read_exact(&mut app_sig[..5]).is_err() {
                            return -1i32;
                        }
                        length -= 5i32;
                        if app_sig.starts_with(b"Exif\x00") {
                            (*j_info).flags |= 1i32 << 3i32;
                            length = (length as u64).wrapping_sub(read_APP1_Exif(
                                j_info,
                                handle,
                                length as size_t,
                            )
                                as _) as i32 as i32
                        } else if app_sig.starts_with(b"http:") && length > 24 {
                            if handle.read_exact(&mut app_sig[..24]).is_err() {
                                return -1;
                            }
                            length -= 24;
                            if app_sig.starts_with(b"//ns.adobe.com/xap/1.0/\x00") {
                                (*j_info).flags |= 1i32 << 4i32;
                                length = (length as u64).wrapping_sub(read_APP1_XMP(
                                    j_info,
                                    handle,
                                    length as size_t,
                                )
                                    as _) as i32 as i32;
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
                JM_APP2 => {
                    if length >= 14 {
                        if handle.read_exact(&mut app_sig[..12]).is_err() {
                            return -1i32;
                        }
                        length -= 12;
                        if app_sig.starts_with(b"ICC_PROFILE\x00") {
                            (*j_info).flags |= 1i32 << 2i32;
                            length = (length as u64).wrapping_sub(read_APP2_ICC(
                                j_info,
                                handle,
                                length as size_t,
                            )
                                as _) as i32 as i32;
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
                JM_APP14 => {
                    if length > 5 {
                        if handle.read_exact(&mut app_sig[..5]).is_err() {
                            return -1;
                        }
                        length -= 5;
                        if app_sig.starts_with(b"Adobe") {
                            (*j_info).flags |= 1 << 1;
                            length -= read_APP14_Adobe(j_info, handle) as i32
                        } else if count < 1024 {
                            (*j_info).skipbits[(count / 8) as usize] =
                                ((*j_info).skipbits[(count / 8) as usize] as i32
                                    | 1 << 7 - count % 8) as i8
                        }
                    }
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                }
                _ => {
                    handle.seek(SeekFrom::Current(length as i64)).unwrap();
                    if marker as u32 >= JM_APP0 as i32 as u32
                        && marker as u32 <= JM_APP15 as i32 as u32
                    {
                        if count < 1024 {
                            (*j_info).skipbits[(count / 8) as usize] =
                                ((*j_info).skipbits[(count / 8) as usize] as i32
                                    | 1 << 7 - count % 8) as i8
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
    if (*j_info).xdpi < 0.1 && (*j_info).ydpi < 0.1 {
        (*j_info).ydpi = 72.;
        (*j_info).xdpi = (*j_info).ydpi
    }
    if found_SOFn != 0 {
        0i32
    } else {
        -1i32
    }
}

pub unsafe fn jpeg_get_bbox<R: Read + Seek>(handle: &mut R) -> Result<(u32, u32, f64, f64), ()> {
    let mut j_info: JPEG_info = JPEG_info {
        height: 0,
        width: 0,
        bits_per_component: 0,
        num_components: 0,
        xdpi: 0.,
        ydpi: 0.,
        flags: 0,
        appn: Vec::new(),
        skipbits: [0; 129],
    };
    JPEG_info_init(&mut j_info);
    if JPEG_scan_file(&mut j_info, handle) < 0i32 {
        warn!("{}: Not a JPEG file?", "JPEG");
        JPEG_info_clear(&mut j_info);
        return Err(());
    }
    let width = j_info.width as u32;
    let height = j_info.height as u32;
    let (xdensity, ydensity) = jpeg_get_density(&mut j_info);
    JPEG_info_clear(&mut j_info);
    Ok((width, height, xdensity, ydensity))
}
