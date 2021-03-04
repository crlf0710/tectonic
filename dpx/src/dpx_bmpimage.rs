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

use super::dpx_mem::new;
use super::dpx_numbers::GetFromFile;
use crate::dpx_pdfobj::{
    pdf_stream, pdf_stream_set_predictor, pdf_string, IntoObj, PushObj, STREAM_COMPRESS,
};
use crate::warn;
use libc::free;

use std::io::{Read, Seek, SeekFrom};

use crate::bridge::size_t;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct hdr_info {
    pub(crate) offset: u32,
    pub(crate) hsize: u32,
    pub(crate) width: u32,
    pub(crate) height: i32,
    pub(crate) compression: i32,
    pub(crate) bit_count: u16,
    pub(crate) psize: i32,
    pub(crate) x_pix_per_meter: u32,
    pub(crate) y_pix_per_meter: u32,
}

pub fn check_for_bmp<R: Read + Seek>(handle: &mut R) -> bool {
    let mut sigbytes: [u8; 2] = [0; 2];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if handle.read_exact(&mut sigbytes[..]).is_err() || sigbytes != [b'B', b'M'] {
        false
    } else {
        true
    }
}
fn get_density(hdr: &hdr_info) -> (f64, f64) {
    if hdr.x_pix_per_meter > 0 && hdr.y_pix_per_meter > 0 {
        /* 0 for undefined. FIXME */
        (
            72. / (hdr.x_pix_per_meter as f64 * 0.0254),
            72. / (hdr.y_pix_per_meter as f64 * 0.0254),
        )
    } else {
        (1., 1.)
    }
}

pub fn bmp_get_bbox<R: Read + Seek>(handle: &mut R) -> Result<(u32, u32, f64, f64), ()> {
    handle.seek(SeekFrom::Start(0)).unwrap();
    let hdr = read_header(handle)?;
    let width = hdr.width;
    let height = (if hdr.height < 0 {
        -hdr.height
    } else {
        hdr.height
    }) as u32;
    let (xdensity, ydensity) = get_density(&hdr);
    Ok((width, height, xdensity, ydensity))
}

pub(crate) unsafe fn bmp_include_image<R: Read + Seek>(
    ximage: &mut pdf_ximage,
    handle: &mut R,
) -> Result<(), ()> {
    let num_palette;
    let mut info = ximage_info::init();
    handle.seek(SeekFrom::Start(0)).unwrap();
    let hdr = read_header(handle)?;
    let (xdensity, ydensity) = get_density(&hdr);
    info.xdensity = xdensity;
    info.ydensity = ydensity;
    info.width = hdr.width as i32;
    info.height = hdr.height;
    let flip = if info.height < 0 {
        info.height = -info.height;
        false
    } else {
        true
    };
    if (hdr.bit_count as i32) < 24 {
        if hdr.bit_count as i32 != 1 && hdr.bit_count as i32 != 4 && hdr.bit_count as i32 != 8 {
            warn!("Unsupported palette size: {}", hdr.bit_count as i32);
            return Err(());
        }
        num_palette = hdr
            .offset
            .wrapping_sub(hdr.hsize)
            .wrapping_sub(14_u32)
            .wrapping_div(hdr.psize as u32) as i32;
        info.bits_per_component = hdr.bit_count as i32;
        info.num_components = 1
    } else if hdr.bit_count as i32 == 24 {
        /* full color */
        num_palette = 1; /* dummy */
        info.bits_per_component = 8;
        info.num_components = 3
    } else {
        warn!(
            "Unkown/Unsupported BMP bitCount value: {}",
            hdr.bit_count as i32,
        );
        return Err(());
    }
    if info.width == 0 || info.height == 0 || num_palette < 1 {
        warn!(
            "Invalid BMP file: width={}, height={}, #palette={}",
            info.width, info.height, num_palette,
        );
        return Err(());
    }
    /* Start reading raster data */
    let mut stream = pdf_stream::new(STREAM_COMPRESS);
    let stream_dict = stream.get_dict_mut();
    /* Color space: Indexed or DeviceRGB */
    let colorspace = if (hdr.bit_count as i32) < 24 {
        let mut bgrq: [u8; 4] = [0; 4];
        let palette = new(((num_palette * 3 + 1) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        for i in 0..num_palette {
            if handle.read_exact(&mut bgrq[..hdr.psize as usize]).is_err() {
                warn!("Reading file failed...");
                free(palette as *mut libc::c_void);
                return Err(());
            }
            /* BGR data */
            *palette.offset((3 * i) as isize) = bgrq[2];
            *palette.offset((3 * i + 1) as isize) = bgrq[1];
            *palette.offset((3 * i + 2) as isize) = bgrq[0];
        }
        let lookup =
            pdf_string::new_from_ptr(palette as *const libc::c_void, (num_palette * 3) as size_t);
        free(palette as *mut libc::c_void);
        let mut colorspace = vec![];
        colorspace.push_obj("Indexed");
        colorspace.push_obj("DeviceRGB");
        colorspace.push_obj((num_palette - 1) as f64);
        colorspace.push_obj(lookup);
        colorspace.into_obj()
    } else {
        "DeviceRGB".into_obj()
    };
    stream_dict.set("ColorSpace", colorspace);
    /* Raster data of BMP is four-byte aligned. */
    let mut stream_data;
    let rowbytes = (info.width * hdr.bit_count as i32 + 7) / 8;
    handle.seek(SeekFrom::Start(hdr.offset as u64)).unwrap();
    if hdr.compression == 0 {
        let padding = if rowbytes % 4 != 0 {
            4 - rowbytes % 4
        } else {
            0
        };
        let dib_rowbytes = rowbytes + padding;
        stream_data = vec![0_u8; (rowbytes * info.height + padding) as usize];
        let mut n = 0;
        while n < info.height {
            let p =
                &mut stream_data[(n * rowbytes) as usize..(n * rowbytes + dib_rowbytes) as usize];
            if handle.read_exact(p).is_err() {
                warn!("Reading BMP raster data failed...");
                return Err(());
            }
            n += 1
        }
    } else if hdr.compression == 1 {
        if let Ok((data, _)) = read_raster_rle8(info.width, info.height, handle) {
            stream_data = data;
        } else {
            warn!("Reading BMP raster data failed...");
            return Err(());
        }
    } else if hdr.compression == 2 {
        if let Ok((data, _)) = read_raster_rle4(info.width, info.height, handle) {
            stream_data = data;
        } else {
            warn!("Reading BMP raster data failed...");
            return Err(());
        }
    } else {
        warn!(
            "Unknown/Unsupported compression type for BMP image: {}",
            hdr.compression
        );
        return Err(());
    }
    /* gbr --> rgb */
    if hdr.bit_count as i32 == 24 {
        let mut n = 0;
        while n < info.width * info.height * 3 {
            stream_data.swap(n as usize, (n + 2) as usize);
            n += 3;
        }
    }
    if flip {
        let mut n = info.height - 1;
        while n >= 0 {
            let p = &stream_data[(n * rowbytes) as usize..(n * rowbytes + rowbytes) as usize];
            stream.add_slice(p);
            n -= 1;
        }
    } else {
        stream.add_slice(&stream_data[..(rowbytes * info.height) as usize]);
    }
    /* Predictor is usually not so efficient for indexed images. */
    if hdr.bit_count as i32 >= 24 && info.bits_per_component >= 8 && info.height > 64 {
        pdf_stream_set_predictor(
            &mut stream,
            15,
            info.width,
            info.bits_per_component,
            info.num_components,
        );
    }
    ximage.set_image(&info, stream.into_obj());
    Ok(())
}

use crate::FromLEByteSlice;
fn read_header<R: Read>(handle: &mut R) -> Result<hdr_info, ()> {
    let mut buf: [u8; 142] = [0; 142];
    let p = &mut buf;
    if handle.read_exact(&mut p[..14 + 4]).is_err() {
        warn!("Could not read BMP file header...");
        return Err(());
    }
    if p[..2] != [b'B', b'M'] {
        warn!("File not starting with \'B\' \'M\'... Not a BMP file?");
        return Err(());
    }
    let p = &mut p[2..];
    /* fsize  = ULONG_LE(p); */
    let p = &mut p[4..];
    if u32::from_le_byte_slice(&p[..4]) != 0 {
        warn!("Not a BMP file???");
        return Err(());
    }
    let p = &mut p[4..];
    let mut hdr = hdr_info::default();
    hdr.offset = u32::from_le_byte_slice(&p[..4]);
    let p = &mut p[4..];
    /* info header */
    hdr.hsize = u32::from_le_byte_slice(&p[..4]); /* undefined. FIXME */
    let p = &mut p[4..]; /* undefined. FIXME */
    if handle
        .read_exact(&mut p[..(hdr.hsize - 4) as usize])
        .is_err()
    {
        warn!("Could not read BMP file header...");
        return Err(());
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
                return Err(());
            }
            let p = &mut p[2..];
            hdr.bit_count = u16::from_le_byte_slice(&p[..2]);
            //let p = &mut p[2..];
            hdr.compression = 0;
            hdr.psize = 3
        }
        40 | 64 | 108 | 124 => {
            hdr.width = u32::from_le_byte_slice(&p[..4]);
            let p = &mut p[4..];
            hdr.height = u32::from_le_byte_slice(&p[..4]) as i32;
            let p = &mut p[4..];
            if u16::from_le_byte_slice(&p[..2]) != 1 {
                warn!("Unknown biPlanes value in BMP INFOHEADER.");
                return Err(());
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
            hdr.psize = 4
        }
        _ => {
            warn!("Unknown BMP header type.");
            return Err(());
        }
    }
    Ok(hdr)
}
fn read_raster_rle8<R: Read>(
    width: i32,
    height: i32,
    handle: &mut R,
) -> Result<(Vec<u8>, u32), ()> {
    let mut count: u32 = 0;
    let rowbytes = width;
    let mut data = vec![0_u8, (rowbytes * height) as _];
    let mut v = 0;
    let mut eoi = 0;
    while v < height && eoi == 0 {
        let mut h = 0;
        let mut eol = 0;
        while h < width && eol == 0 {
            let b0 = u8::get(handle);
            let b1 = u8::get(handle);
            count += 2;
            let p = &mut data[(v * rowbytes + h) as usize..];
            if b0 as i32 == 0 {
                match b1 as i32 {
                    0 => {
                        /* EOL */
                        eol = 1
                    }
                    1 => {
                        /* EOI */
                        eoi = 1
                    }
                    2 => {
                        h += u8::get(handle) as i32;
                        v += u8::get(handle) as i32;
                        count += 2
                    }
                    _ => {
                        h += b1 as i32;
                        if h > width {
                            warn!("RLE decode failed...");
                            return Err(());
                        }
                        if handle.read_exact(&mut p[..b1 as usize]).is_err() {
                            return Err(());
                        }
                        count += u32::from(b1);
                        if b1 as i32 % 2 != 0 {
                            u8::get(handle);
                            count += 1
                        }
                    }
                }
            } else {
                h += b0 as i32;
                if h > width {
                    warn!("RLE decode failed...");
                    return Err(());
                }
                for b in &mut p[..b0 as usize] {
                    *b = b1;
                }
            }
        }
        /* next row ... */
        if eol == 0 && eoi == 0 {
            let b0 = u8::get(handle);
            let b1 = u8::get(handle);
            if b0 as i32 != 0 {
                warn!("RLE decode failed...");
                return Err(());
            } else if b1 as i32 == 0x1 {
                eoi = 1;
            } else if b1 as i32 != 0 {
                warn!("RLE decode failed...");
                return Err(());
            }
        }
        v += 1
    }
    Ok((data, count))
}
fn read_raster_rle4<R: Read>(
    width: i32,
    height: i32,
    handle: &mut R,
) -> Result<(Vec<u8>, u32), ()> {
    let mut count: u32 = 0;
    let rowbytes = (width + 1) / 2;
    let mut data = vec![0_u8, (rowbytes * height) as _];
    let mut v = 0;
    let mut eoi = 0;
    while v < height && eoi == 0 {
        let mut h = 0;
        let mut eol = 0;
        while h < width && eol == 0 {
            let mut b0 = u8::get(handle);
            let mut b1 = u8::get(handle);
            count += 2;
            let mut p = &mut data[(v * rowbytes + h / 2) as usize..];
            if b0 as i32 == 0 {
                match b1 as i32 {
                    0 => {
                        /* Check for EOL and EOI marker */
                        /* EOL */
                        eol = 1
                    }
                    1 => {
                        /* EOI */
                        eoi = 1
                    }
                    2 => {
                        h += u8::get(handle) as i32;
                        v += u8::get(handle) as i32;
                        count += 2
                    }
                    _ => {
                        if h + b1 as i32 > width {
                            warn!("RLE decode failed...");
                            return Err(());
                        }
                        let nbytes = (u32::from(b1) + 1) / 2;
                        if h % 2 != 0 {
                            /* starting at hi-nib */
                            for _ in 0..nbytes {
                                let b = u8::get(handle);
                                p[0] = p[0] | b >> 4 & 0xf;
                                p = &mut p[1..];
                                p[0] = ((b as i32) << 4 & 0xf0) as u8;
                            }
                        } else if handle.read_exact(&mut p[..nbytes as usize]).is_err() {
                            return Err(());
                        }
                        h += b1 as i32;
                        count += nbytes;
                        if nbytes % 2 != 0 {
                            u8::get(handle);
                            count += 1
                        }
                    }
                }
            } else {
                if h + b0 as i32 > width {
                    warn!("RLE decode failed...");
                    return Err(());
                }
                if h % 2 != 0 {
                    p[0] = (b1 as i32 >> 4 & 0xf) as u8;
                    p = &mut p[1..];
                    b1 = ((b1 as i32) << 4 & 0xf0 | b1 as i32 >> 4 & 0xf) as u8;
                    b0 = b0.wrapping_sub(1);
                    h += 1
                }
                let nbytes = (b0 as i32 + 1) / 2;
                for b in &mut p[..nbytes as usize] {
                    *b = b1;
                }
                h += b0 as i32;
                if h % 2 != 0 {
                    p[(nbytes - 1) as usize] &= 0xf0;
                }
            }
        }
        /* next row ... */
        if eol == 0 && eoi == 0 {
            let b0 = u8::get(handle);
            let b1 = u8::get(handle);
            if b0 as i32 != 0 {
                warn!("No EOL/EOI marker. RLE decode failed...");
                return Err(());
            } else if b1 as i32 == 0x1 {
                eoi = 1;
            } else if b1 as i32 != 0 {
                warn!("No EOL/EOI marker. RLE decode failed...");
                return Err(());
            }
        }
        v += 1
    }
    Ok((data, count))
}
/* Check for EOL and EOI marker */
