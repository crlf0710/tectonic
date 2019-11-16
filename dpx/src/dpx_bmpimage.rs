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

use super::dpx_mem::new;
use super::dpx_numbers::tt_get_unsigned_byte;
use super::dpx_pdfximage::{pdf_ximage_init_image_info, pdf_ximage_set_image};
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_new_array, pdf_new_name, pdf_new_number,
    pdf_new_stream, pdf_new_string, pdf_release_obj,
    pdf_stream_set_predictor, STREAM_COMPRESS,
};
use crate::warn;
use crate::{ttstub_input_read};
use libc::{free, memset};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;
use bridge::InputHandleWrapper;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
#[derive(Copy, Clone)]
#[repr(C)]
pub struct hdr_info {
    pub offset: u32,
    pub hsize: u32,
    pub width: u32,
    pub height: i32,
    pub compression: i32,
    pub bit_count: u16,
    pub psize: i32,
    pub x_pix_per_meter: u32,
    pub y_pix_per_meter: u32,
}
#[no_mangle]
pub unsafe fn check_for_bmp(handle: &mut InputHandleWrapper) -> i32 {
    let mut sigbytes: [u8; 2] = [0; 2];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if ttstub_input_read(
        handle.0.as_ptr(),
        sigbytes.as_mut_ptr() as *mut i8,
        ::std::mem::size_of::<[u8; 2]>() as u64,
    ) as u64
        != ::std::mem::size_of::<[u8; 2]>() as u64
        || sigbytes[0] as i32 != 'B' as i32
        || sigbytes[1] as i32 != 'M' as i32
    {
        return 0i32;
    }
    1i32
}
unsafe fn get_density(mut xdensity: *mut f64, mut ydensity: *mut f64, mut hdr: *mut hdr_info) {
    if (*hdr).x_pix_per_meter > 0_u32 && (*hdr).y_pix_per_meter > 0_u32 {
        /* 0 for undefined. FIXME */
        *xdensity = 72.0f64 / ((*hdr).x_pix_per_meter as f64 * 0.0254f64);
        *ydensity = 72.0f64 / ((*hdr).y_pix_per_meter as f64 * 0.0254f64)
    } else {
        *xdensity = 1.0f64;
        *ydensity = 1.0f64
    };
}
#[no_mangle]
pub unsafe fn bmp_get_bbox(
    handle: &mut InputHandleWrapper,
    mut width: *mut u32,
    mut height: *mut u32,
    mut xdensity: *mut f64,
    mut ydensity: *mut f64,
) -> i32 {
    let mut hdr: hdr_info = hdr_info {
            offset: 0_u32,
            hsize: 0_u32,
            width: 0_u32,
            height: 0i32,
            compression: 0i32,
            bit_count: 0_u16,
            psize: 0i32,
            x_pix_per_meter: 0_u32,
            y_pix_per_meter: 0_u32,
        };
    handle.seek(SeekFrom::Start(0)).unwrap();
    let r = read_header(handle, &mut hdr);
    *width = hdr.width;
    *height = (if hdr.height < 0i32 {
        -hdr.height
    } else {
        hdr.height
    }) as u32;
    get_density(xdensity, ydensity, &mut hdr);
    r
}
#[no_mangle]
pub unsafe fn bmp_include_image(
    mut ximage: *mut pdf_ximage,
    handle: &mut InputHandleWrapper,
) -> i32 {
    let mut info = ximage_info::default();
    let mut hdr: hdr_info = hdr_info {
            offset: 0_u32,
            hsize: 0_u32,
            width: 0_u32,
            height: 0i32,
            compression: 0i32,
            bit_count: 0_u16,
            psize: 0i32,
            x_pix_per_meter: 0_u32,
            y_pix_per_meter: 0_u32,
        };
    let num_palette;
    pdf_ximage_init_image_info(&mut info);
    let colorspace;
    handle.seek(SeekFrom::Start(0)).unwrap();
    if read_header(handle, &mut hdr) < 0i32 {
        return -1i32;
    }
    get_density(&mut info.xdensity, &mut info.ydensity, &mut hdr);
    info.width = hdr.width as i32;
    info.height = hdr.height;
    let flip = if info.height < 0i32 {
        info.height = -info.height;
        0
    } else {
        1
    };
    if (hdr.bit_count as i32) < 24i32 {
        if hdr.bit_count as i32 != 1i32
            && hdr.bit_count as i32 != 4i32
            && hdr.bit_count as i32 != 8i32
        {
            warn!("Unsupported palette size: {}", hdr.bit_count as i32,);
            return -1i32;
        }
        num_palette = hdr
            .offset
            .wrapping_sub(hdr.hsize)
            .wrapping_sub(14_u32)
            .wrapping_div(hdr.psize as u32) as i32;
        info.bits_per_component = hdr.bit_count as i32;
        info.num_components = 1i32
    } else if hdr.bit_count as i32 == 24i32 {
        /* full color */
        num_palette = 1i32; /* dummy */
        info.bits_per_component = 8i32;
        info.num_components = 3i32
    } else {
        warn!(
            "Unkown/Unsupported BMP bitCount value: {}",
            hdr.bit_count as i32,
        );
        return -1i32;
    }
    if info.width == 0i32 || info.height == 0i32 || num_palette < 1i32 {
        warn!(
            "Invalid BMP file: width={}, height={}, #palette={}",
            info.width, info.height, num_palette,
        );
        return -1i32;
    }
    /* Start reading raster data */
    let stream = pdf_new_stream(STREAM_COMPRESS);
    let stream_dict = (*stream).as_stream_mut().get_dict_mut();
    /* Color space: Indexed or DeviceRGB */
    if (hdr.bit_count as i32) < 24i32 {
        let mut bgrq: [u8; 4] = [0; 4];
        let palette = new(((num_palette * 3i32 + 1i32) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        for i in 0..num_palette {
            if ttstub_input_read(handle.0.as_ptr(), bgrq.as_mut_ptr() as *mut i8, hdr.psize as size_t)
                != hdr.psize as i64
            {
                warn!("Reading file failed...");
                free(palette as *mut libc::c_void);
                return -1i32;
            }
            /* BGR data */
            *palette.offset((3i32 * i) as isize) = bgrq[2];
            *palette.offset((3i32 * i + 1i32) as isize) = bgrq[1];
            *palette.offset((3i32 * i + 2i32) as isize) = bgrq[0];
        }
        let lookup = pdf_new_string(
            palette as *const libc::c_void,
            (num_palette * 3i32) as size_t,
        );
        free(palette as *mut libc::c_void);
        colorspace = pdf_new_array();
        pdf_add_array(&mut *colorspace, pdf_new_name("Indexed"));
        pdf_add_array(&mut *colorspace, pdf_new_name("DeviceRGB"));
        pdf_add_array(&mut *colorspace, pdf_new_number((num_palette - 1i32) as f64));
        pdf_add_array(&mut *colorspace, lookup);
    } else {
        colorspace = pdf_new_name("DeviceRGB")
    }
    pdf_add_dict(stream_dict, "ColorSpace", colorspace);
    /* Raster data of BMP is four-byte aligned. */
    let stream_data_ptr;
    let mut rowbytes = (info.width * hdr.bit_count as i32 + 7i32) / 8i32;
    handle.seek(SeekFrom::Start(hdr.offset as u64)).unwrap();
    if hdr.compression == 0i32 {
        let padding = if rowbytes % 4i32 != 0 {
            4i32 - rowbytes % 4i32
        } else {
            0i32
        };
        let dib_rowbytes = rowbytes + padding;
        stream_data_ptr = new(((rowbytes * info.height + padding) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        let mut n = 0i32;
        while n < info.height {
            let p = stream_data_ptr.offset((n * rowbytes) as isize);
            if ttstub_input_read(handle.0.as_ptr(), p as *mut i8, dib_rowbytes as size_t)
                != dib_rowbytes as i64
            {
                warn!("Reading BMP raster data failed...");
                pdf_release_obj(stream);
                free(stream_data_ptr as *mut libc::c_void);
                return -1i32;
            }
            n += 1
        }
    } else if hdr.compression == 1i32 {
        stream_data_ptr = new(((rowbytes * info.height) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        if read_raster_rle8(stream_data_ptr, info.width, info.height, handle) < 0i32 {
            warn!("Reading BMP raster data failed...");
            pdf_release_obj(stream);
            free(stream_data_ptr as *mut libc::c_void);
            return -1i32;
        }
    } else if hdr.compression == 2i32 {
        stream_data_ptr = new(((rowbytes * info.height) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        if read_raster_rle4(stream_data_ptr, info.width, info.height, handle) < 0i32 {
            warn!("Reading BMP raster data failed...");
            pdf_release_obj(stream);
            free(stream_data_ptr as *mut libc::c_void);
            return -1i32;
        }
    } else {
        warn!(
            "Unknown/Unsupported compression type for BMP image: {}",
            hdr.compression
        );
        pdf_release_obj(stream);
        return -1i32;
    }
    /* gbr --> rgb */
    if hdr.bit_count as i32 == 24i32 {
        let mut n = 0i32;
        while n < info.width * info.height * 3i32 {
            let g = *stream_data_ptr.offset(n as isize);
            *stream_data_ptr.offset(n as isize) = *stream_data_ptr.offset((n + 2i32) as isize);
            *stream_data_ptr.offset((n + 2i32) as isize) = g;
            n += 3i32
        }
    }
    if flip != 0 {
        let mut n = info.height - 1i32;
        while n >= 0i32 {
            let p = stream_data_ptr.offset((n * rowbytes) as isize);
            pdf_add_stream(&mut *stream, p as *const libc::c_void, rowbytes);
            n -= 1
        }
    } else {
        pdf_add_stream(
            &mut *stream,
            stream_data_ptr as *const libc::c_void,
            rowbytes * info.height,
        );
    }
    free(stream_data_ptr as *mut libc::c_void);
    /* Predictor is usually not so efficient for indexed images. */
    if hdr.bit_count as i32 >= 24i32 && info.bits_per_component >= 8i32 && info.height > 64i32 {
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

use crate::FromLEByteSlice;
unsafe fn read_header(handle: &mut InputHandleWrapper, hdr: &mut hdr_info) -> i32 {
    let mut buf: [u8; 142] = [0; 142];
    let p = &mut buf;
    if ttstub_input_read(handle.0.as_ptr(), p.as_mut_ptr() as *mut i8, (14i32 + 4i32) as size_t)
        != (14i32 + 4i32) as i64
    {
        warn!("Could not read BMP file header...");
        return -1i32;
    }
    if p[0] != b'B' || p[1] != b'M' {
        warn!("File not starting with \'B\' \'M\'... Not a BMP file?");
        return -1i32;
    }
    let p = &mut p[2..];
    /* fsize  = ULONG_LE(p); */
    let p = &mut p[4..];
    if u32::from_le_byte_slice(&p[..4]) != 0 {
        warn!("Not a BMP file???");
        return -1i32;
    }
    let p = &mut p[4..];
    hdr.offset = u32::from_le_byte_slice(&p[..4]);
    let p = &mut p[4..];
    /* info header */
    hdr.hsize = u32::from_le_byte_slice(&p[..4]); /* undefined. FIXME */
    let p = &mut p[4..]; /* undefined. FIXME */
    if ttstub_input_read(
        handle.0.as_ptr(),
        p.as_mut_ptr() as *mut i8,
        hdr.hsize.wrapping_sub(4_u32) as size_t,
    ) != hdr.hsize.wrapping_sub(4_u32) as i64
    {
        warn!("Could not read BMP file header...");
        return -1i32;
    }
    match hdr.hsize {
        12 => {
            hdr.width = u16::from_le_byte_slice(&p[..2]) as u32;
            let p = &mut p[2..];
            hdr.height = u16::from_le_byte_slice(&p[..2]) as i32;
            let p = &mut p[2..];
            hdr.x_pix_per_meter = 0_u32;
            hdr.y_pix_per_meter = 0_u32;
            if u16::from_le_byte_slice(&p[..2]) != 1 {
                warn!("Unknown bcPlanes value in BMP COREHEADER.");
                return -1i32;
            }
            let p = &mut p[2..];
            hdr.bit_count = u16::from_le_byte_slice(&p[..2]);
            //let p = &mut p[2..];
            hdr.compression = 0i32;
            hdr.psize = 3i32
        }
        40 | 64 | 108 | 124 => {
            hdr.width = u32::from_le_byte_slice(&p[..4]);
            let p = &mut p[4..];
            hdr.height = u32::from_le_byte_slice(&p[..4]) as i32;
            let p = &mut p[4..];
            if u16::from_le_byte_slice(&p[..2]) != 1 {
                warn!("Unknown biPlanes value in BMP INFOHEADER.");
                return -1i32;
            }
            let p = &mut p[2..];
            hdr.bit_count = u16::from_le_byte_slice(&p[..2]);
            let p = &mut p[2..];
            hdr.compression = u32::from_le_byte_slice(&p[..4]) as i32;
            let p = &mut p[4..];
            /* ignore biSizeImage */
            let p = &mut p[4..];
            hdr.x_pix_per_meter = u32::from_le_byte_slice(&p[..4]);
            let p = &mut p[4..];
            hdr.y_pix_per_meter = u32::from_le_byte_slice(&p[..4]);
            //let p = &mut p[4..];
            hdr.psize = 4i32
        }
        _ => {
            warn!("Unknown BMP header type.");
            return -1i32;
        }
    }
    0i32
}
unsafe fn read_raster_rle8(
    mut data_ptr: *mut u8,
    mut width: i32,
    mut height: i32,
    handle: &mut InputHandleWrapper,
) -> i32 {
    let mut count: i32 = 0i32;
    let rowbytes = width;
    memset(
        data_ptr as *mut libc::c_void,
        0i32,
        (rowbytes * height) as _,
    );
    let mut v = 0;
    let mut eoi = 0i32;
    while v < height && eoi == 0 {
        let mut h = 0;
        let mut eol = 0i32;
        while h < width && eol == 0 {
            let mut b0 = tt_get_unsigned_byte(handle);
            let mut b1 = tt_get_unsigned_byte(handle);
            count += 2i32;
            let mut p = data_ptr.offset((v * rowbytes) as isize).offset(h as isize);
            if b0 as i32 == 0i32 {
                match b1 as i32 {
                    0 => {
                        /* EOL */
                        eol = 1i32
                    }
                    1 => {
                        /* EOI */
                        eoi = 1i32
                    }
                    2 => {
                        h += tt_get_unsigned_byte(handle) as i32;
                        v += tt_get_unsigned_byte(handle) as i32;
                        count += 2i32
                    }
                    _ => {
                        h += b1 as i32;
                        if h > width {
                            warn!("RLE decode failed...");
                            return -1i32;
                        }
                        if ttstub_input_read(handle.0.as_ptr(), p as *mut i8, b1 as size_t) != b1 as i64 {
                            return -1i32;
                        }
                        count += b1 as i32;
                        if b1 as i32 % 2i32 != 0 {
                            tt_get_unsigned_byte(handle);
                            count += 1
                        }
                    }
                }
            } else {
                h += b0 as i32;
                if h > width {
                    warn!("RLE decode failed...");
                    return -1i32;
                }
                memset(p as *mut libc::c_void, b1 as i32, b0 as _);
            }
        }
        /* next row ... */
        if eol == 0 && eoi == 0 {
            let b0 = tt_get_unsigned_byte(handle);
            let b1 = tt_get_unsigned_byte(handle);
            if b0 as i32 != 0i32 {
                warn!("RLE decode failed...");
                return -1i32;
            } else {
                if b1 as i32 == 0x1i32 {
                    eoi = 1i32
                } else if b1 as i32 != 0i32 {
                    warn!("RLE decode failed...");
                    return -1i32;
                }
            }
        }
        v += 1
    }
    count
}
unsafe fn read_raster_rle4(
    mut data_ptr: *mut u8,
    mut width: i32,
    mut height: i32,
    handle: &mut InputHandleWrapper,
) -> i32 {
    let mut count: i32 = 0i32;
    let rowbytes = (width + 1i32) / 2i32;
    memset(
        data_ptr as *mut libc::c_void,
        0i32,
        (rowbytes * height) as _,
    );
    let mut v = 0i32;
    let mut eoi = 0i32;
    while v < height && eoi == 0 {
        let mut h = 0i32;
        let mut eol = 0i32;
        while h < width && eol == 0 {
            let mut b0 = tt_get_unsigned_byte(handle);
            let mut b1 = tt_get_unsigned_byte(handle);
            count += 2i32;
            let mut p = data_ptr
                .offset((v * rowbytes) as isize)
                .offset((h / 2i32) as isize);
            if b0 as i32 == 0i32 {
                match b1 as i32 {
                    0 => {
                        /* Check for EOL and EOI marker */
                        /* EOL */
                        eol = 1i32
                    }
                    1 => {
                        /* EOI */
                        eoi = 1i32
                    }
                    2 => {
                        h += tt_get_unsigned_byte(handle) as i32;
                        v += tt_get_unsigned_byte(handle) as i32;
                        count += 2i32
                    }
                    _ => {
                        if h + b1 as i32 > width {
                            warn!("RLE decode failed...");
                            return -1i32;
                        }
                        let nbytes = (b1 as i32 + 1i32) / 2i32;
                        if h % 2i32 != 0 {
                            /* starting at hi-nib */
                            for _ in 0..nbytes {
                                let b = tt_get_unsigned_byte(handle);
                                let fresh0 = p;
                                p = p.offset(1);
                                *fresh0 = (*fresh0 as i32 | b as i32 >> 4i32 & 0xfi32) as u8;
                                *p = ((b as i32) << 4i32 & 0xf0i32) as u8;
                            }
                        } else if ttstub_input_read(handle.0.as_ptr(), p as *mut i8, nbytes as size_t)
                            != nbytes as i64
                        {
                            return -1i32;
                        }
                        h += b1 as i32;
                        count += nbytes;
                        if nbytes % 2i32 != 0 {
                            tt_get_unsigned_byte(handle);
                            count += 1
                        }
                    }
                }
            } else {
                if h + b0 as i32 > width {
                    warn!("RLE decode failed...");
                    return -1i32;
                }
                if h % 2i32 != 0 {
                    let fresh1 = p;
                    p = p.offset(1);
                    *fresh1 = (b1 as i32 >> 4i32 & 0xfi32) as u8;
                    b1 = ((b1 as i32) << 4i32 & 0xf0i32 | b1 as i32 >> 4i32 & 0xfi32) as u8;
                    b0 = b0.wrapping_sub(1);
                    h += 1
                }
                let nbytes = (b0 as i32 + 1i32) / 2i32;
                memset(p as *mut libc::c_void, b1 as i32, nbytes as _);
                h += b0 as i32;
                if h % 2i32 != 0 {
                    let ref mut fresh2 = *p.offset((nbytes - 1i32) as isize);
                    *fresh2 = (*fresh2 as i32 & 0xf0i32) as u8
                }
            }
        }
        /* next row ... */
        if eol == 0 && eoi == 0 {
            let b0 = tt_get_unsigned_byte(handle);
            let b1 = tt_get_unsigned_byte(handle);
            if b0 as i32 != 0i32 {
                warn!("No EOL/EOI marker. RLE decode failed...");
                return -1i32;
            } else {
                if b1 as i32 == 0x1i32 {
                    eoi = 1i32
                } else if b1 as i32 != 0i32 {
                    warn!("No EOL/EOI marker. RLE decode failed...");
                    return -1i32;
                }
            }
        }
        v += 1
    }
    count
}
/* Check for EOL and EOI marker */
