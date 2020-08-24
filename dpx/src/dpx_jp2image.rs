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
    non_upper_case_globals
)]

use crate::warn;

use super::dpx_numbers::GetFromFile;
use super::dpx_pdfximage::pdf_ximage_set_image;
use crate::bridge::ttstub_input_get_size;
use crate::dpx_pdfobj::{pdf_get_version, pdf_stream, IntoObj};
use std::io::{Read, Seek, SeekFrom};

pub(crate) type __off_t = i64;
pub(crate) type __off64_t = i64;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
/* Label */
unsafe fn read_box_hdr<R: Read>(fp: &mut R, lbox: *mut u32, tbox: *mut u32) -> u32 {
    let mut bytesread = 0_u32;
    *lbox = u32::get(fp);
    *tbox = u32::get(fp);
    bytesread = bytesread.wrapping_add(8_u32);
    if *lbox == 1_u32 {
        if u32::get(fp) != 0 {
            panic!("JPEG2000: LBox value in JP2 file >32 bits.\nI can\'t handle this!");
        }
        *lbox = u32::get(fp);
        bytesread = bytesread.wrapping_add(8_u32)
    } else if *lbox > 1_u32 && *lbox < 8_u32 {
        warn!("JPEG2000: Unknown LBox value {} in JP2 file!", *lbox);
    }
    bytesread
}
unsafe fn check_jp___box<R: Read>(fp: &mut R) -> i32 {
    if u32::get(fp) != 0xc {
        return 0;
    }
    if u32::get(fp) != 0x6a502020 {
        return 0;
    }
    /* Next 4 bytes shall be 0D 0A 87 0A */
    if u32::get(fp) != 0xd0a870a {
        return 0;
    }
    1
}
unsafe fn check_ftyp_data<R: Read + Seek>(fp: &mut R, mut size: u32) -> i32 {
    let mut supported: i32 = 0i32;
    let BR = u32::get(fp);
    size = size.wrapping_sub(4_u32);
    /* MinV = */
    u32::get(fp);
    size = size.wrapping_sub(4_u32);
    match BR {
        0x6a703220 => {
            /* "jp2 " ... supported */
            fp.seek(SeekFrom::Current(size as i64)).unwrap();
            supported = 1i32
        }
        0x6a707820 => {
            /* "jpx " ... baseline subset supported */
            while size > 0_u32 {
                let CLi = u32::get(fp);
                if CLi == 0x6a707862_u32 {
                    supported = 1i32
                }
                size = size.wrapping_sub(4_u32)
            }
        }
        _ => {
            warn!("JPEG2000: Unknown JPEG 2000 File Type box Brand field value.");
            fp.seek(SeekFrom::Current(size as i64)).unwrap();
            supported = 0i32
        }
    }
    supported
}
unsafe fn read_res__data<R: Read>(info: &mut ximage_info, fp: &mut R, mut _size: u32) {
    let VR_N = u16::get(fp) as u32;
    let VR_D = u16::get(fp) as u32;
    let HR_N = u16::get(fp) as u32;
    let HR_D = u16::get(fp) as u32;
    let VR_E = u8::get(fp);
    let HR_E = u8::get(fp);
    info.xdensity = 72. / (HR_N as f64 / HR_D as f64 * (10f64).powf(HR_E as f64) * 0.0254);
    info.ydensity = 72. / (VR_N as f64 / VR_D as f64 * (10f64).powf(VR_E as f64) * 0.0254);
}
unsafe fn scan_res_<R: Read + Seek>(info: &mut ximage_info, fp: &mut R, mut size: u32) -> i32 {
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    let mut have_resd: i32 = 0i32;
    while size > 0_u32 {
        let len = read_box_hdr(fp, &mut lbox, &mut tbox);
        if lbox == 0_u32 {
            warn!("JPEG2000: Unexpected lbox value 0 in JP2 Resolution box.");
            break;
        } else {
            match tbox {
                0x72657363 => {
                    if have_resd == 0 {
                        read_res__data(info, fp, lbox.wrapping_sub(len));
                    } else {
                        fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                            .unwrap();
                    }
                }
                0x72657364 => {
                    read_res__data(info, fp, lbox.wrapping_sub(len));
                    have_resd = 1i32
                }
                _ => {
                    warn!("JPEG2000: Unknown JPEG 2000 box type in Resolution box.");
                    fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                        .unwrap();
                }
            }
            size = size.wrapping_sub(lbox)
        }
    }
    if size == 0_u32 {
        0i32
    } else {
        -1i32
    }
}
/* Acrobat seems require Channel Definition box to be defined when image data
 * contains opacity channel. However, OpenJPEG (and maybe most of JPEG 2000 coders?)
 * does not write Channel Definition box so transparency will be ignored.
 */
unsafe fn scan_cdef<R: Read>(
    _info: &mut ximage_info,
    smask: *mut i32,
    fp: &mut R,
    size: u32,
) -> i32 {
    let mut opacity_channels: i32 = 0i32; /* Cn */
    let mut have_type0: i32 = 0i32; /* must be 0 for SMask */
    *smask = 0i32;
    let N = u16::get(fp) as u32;
    if size < N.wrapping_mul(6_u32).wrapping_add(2_u32) {
        warn!("JPEG2000: Inconsistent N value in Channel Definition box.");
        return -1i32;
    }
    for _ in 0..N {
        let Cn = u16::get(fp) as u32;
        let Typ = u16::get(fp) as u32;
        let Asoc = u16::get(fp) as u32;
        if Cn > N {
            warn!("JPEG2000: Invalid Cn value in Channel Definition box.");
        }
        if Typ == 1_u32 {
            if Asoc == 0_u32 {
                have_type0 = 1i32
            }
            opacity_channels += 1
        } else if Typ == 2_u32 {
            opacity_channels += 1
        }
    }
    if opacity_channels == 1i32 {
        *smask = if have_type0 != 0 { 1i32 } else { 0i32 }
    } else if opacity_channels > 1i32 {
        warn!("JPEG2000: Unsupported transparency type. (ignored)");
    }
    0i32
}
unsafe fn scan_jp2h<R: Read + Seek>(
    info: &mut ximage_info,
    smask: *mut i32,
    fp: &mut R,
    mut size: u32,
) -> i32 {
    let mut error: i32 = 0i32;
    let mut have_ihdr: i32 = 0i32;
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    while size > 0_u32 && error == 0 {
        let len = read_box_hdr(fp, &mut lbox, &mut tbox);
        if lbox == 0_u32 {
            warn!("JPEG2000: Unexpected lbox value 0 in JP2 Header box...");
            error = -1i32;
            break;
        } else {
            match tbox {
                1768449138 => {
                    info.height = u32::get(fp) as i32;
                    info.width = u32::get(fp) as i32;
                    info.num_components = u16::get(fp) as i32;
                    /* c = */
                    u8::get(fp); /* BPC - 1 */
                    /* c = */
                    u8::get(fp); /* C: Compression type */
                    /* c = */
                    u8::get(fp); /* UnkC */
                    /* c = */
                    u8::get(fp); /* IPR */
                    have_ihdr = 1i32
                }
                1919251232 => error = scan_res_(info, fp, lbox.wrapping_sub(len)),
                1667523942 => error = scan_cdef(info, smask, fp, lbox.wrapping_sub(len)),
                1651532643 | 1668246642 | 1885564018 | 1668112752 | 1818389536 => {
                    fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                        .unwrap();
                }
                _ => {
                    warn!("JPEG2000: Unknown JPEG 2000 box in JP2 Header box.");
                    fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                        .unwrap();
                    error = -1i32
                }
            }
            size = size.wrapping_sub(lbox)
        }
    }
    if have_ihdr == 0 {
        warn!("JPEG2000: Expecting JPEG 2000 Image Header box but could not find.");
    }
    return if error == 0 && have_ihdr != 0 && size == 0_u32 {
        0i32
    } else {
        -1i32
    };
}
unsafe fn scan_file<R: Read + Seek>(info: &mut ximage_info, smask: *mut i32, fp: &mut R) -> i32 {
    let mut error: i32 = 0i32;
    let mut have_jp2h: i32 = 0i32;
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    let mut size = ttstub_input_get_size(fp);
    /* Should have already been checked before. */
    /* JPEG 2000 Singature box */
    if check_jp___box(fp) == 0 {
        return -1i32;
    }
    size -= 12;
    /* File Type box shall immediately follow */
    let mut len = read_box_hdr(fp, &mut lbox, &mut tbox);
    if tbox != 0x66747970_u32 {
        return -1i32;
    }
    if check_ftyp_data(fp, lbox.wrapping_sub(len)) == 0 {
        return -1i32;
    }
    size -= lbox as usize;
    /* Search for JP2 Header box */
    while size > 0 && error == 0 {
        len = read_box_hdr(fp, &mut lbox, &mut tbox);
        if lbox == 0_u32 {
            lbox = size as u32
        }
        match tbox {
            1785737832 => {
                error = scan_jp2h(info, smask, fp, lbox.wrapping_sub(len));
                have_jp2h = 1i32
            }
            1785737827 => {
                /* JP2 requires JP2H appears before JP2C. */
                if have_jp2h == 0 {
                    warn!("JPEG2000: JPEG 2000 Codestream box found before JP2 Header box.");
                }
                fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                    .unwrap();
            }
            _ => {
                fp.seek(SeekFrom::Current(lbox.wrapping_sub(len) as i64))
                    .unwrap();
            }
        }
        size -= lbox as usize
    }
    /* From ISO/IEC 15444-2 M.9.2.7
     * The JP2 Header box shall be found in the file before the first
     * Contiguous Codestream box, Fragment Table box, Media Data box,
     * Codestream Header box, and Compositing Layer Header box. ...
     */
    if have_jp2h == 0 && error == 0 {
        warn!("JPEG2000: No JP2 Header box found. Not a JP2/JPX baseline file?");
        error = -1i32
    }
    error
}

pub(crate) unsafe fn check_for_jp2<R: Read + Seek>(fp: &mut R) -> i32 {
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    fp.seek(SeekFrom::Start(0)).unwrap();
    /* JPEG 2000 Singature box */
    if check_jp___box(fp) == 0 {
        return 0i32;
    }
    /* File Type box shall immediately follow */
    let len = read_box_hdr(fp, &mut lbox, &mut tbox);
    if tbox != 0x66747970_u32 {
        return 0i32;
    }
    if check_ftyp_data(fp, lbox.wrapping_sub(len)) == 0 {
        return 0i32;
    }
    1i32
}

pub(crate) unsafe fn jp2_include_image<R: Read + Seek>(ximage: *mut pdf_ximage, fp: &mut R) -> i32 {
    let mut smask: i32 = 0i32;
    let pdf_version = pdf_get_version();
    if pdf_version < 5_u32 {
        warn!(
            "JPEG 2000 support requires PDF version >= 1.5 (Current setting 1.{})\n",
            pdf_version
        );
        return -1i32;
    }
    let mut info = ximage_info::init();
    fp.seek(SeekFrom::Start(0)).unwrap();
    if scan_file(&mut info, &mut smask, fp) < 0i32 {
        warn!("JPEG2000: Reading JPEG 2000 file failed.");
        return -1i32;
    }
    let mut stream = pdf_stream::new(0i32);
    let stream_dict = stream.get_dict_mut();
    stream_dict.set("Filter", "JPXDecode");
    if smask != 0 {
        stream_dict.set("SMaskInData", 1_f64);
    }
    /* Read whole file */
    fp.seek(SeekFrom::Start(0)).unwrap();
    let mut buffer = vec![0u8; 1024];
    loop {
        let nb_read = fp.read(&mut buffer).unwrap_or(0);
        if nb_read == 0 {
            break;
        }
        stream.add(buffer.as_mut_ptr() as *const libc::c_void, nb_read as i32);
    }
    pdf_ximage_set_image(ximage, &mut info, stream.into_obj());
    0i32
}

pub(crate) unsafe fn jp2_get_bbox<R: Read + Seek>(
    fp: &mut R,
    width: *mut i32,
    height: *mut i32,
    xdensity: *mut f64,
    ydensity: *mut f64,
) -> i32 {
    let mut smask: i32 = 0i32;
    let mut info = ximage_info::init();
    fp.seek(SeekFrom::Start(0)).unwrap();
    let r = scan_file(&mut info, &mut smask, fp);
    *width = info.width;
    *height = info.height;
    *xdensity = info.xdensity;
    *ydensity = info.ydensity;
    r
}
