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

use crate::warn;

use super::dpx_mfileio::{file_size, seek_relative, work_buffer};
use super::dpx_numbers::{get_unsigned_byte, get_unsigned_pair, get_unsigned_quad};
use super::dpx_pdfximage::{pdf_ximage_init_image_info, pdf_ximage_set_image};
use crate::dpx_pdfobj::{
    pdf_add_dict, pdf_add_stream, pdf_get_version, pdf_new_name, pdf_new_number, pdf_new_stream,
};
use libc::{fread, rewind, FILE};

pub type __off_t = i64;
pub type __off64_t = i64;

use crate::dpx_pdfximage::{pdf_ximage, ximage_info};
/* Label */
unsafe fn read_box_hdr(mut fp: *mut FILE, mut lbox: *mut u32, mut tbox: *mut u32) -> u32 {
    let mut bytesread: u32 = 0_u32;
    *lbox = get_unsigned_quad(fp);
    *tbox = get_unsigned_quad(fp);
    bytesread = bytesread.wrapping_add(8_u32);
    if *lbox == 1_u32 {
        if get_unsigned_quad(fp) != 0_u32 {
            panic!("JPEG2000: LBox value in JP2 file >32 bits.\nI can\'t handle this!");
        }
        *lbox = get_unsigned_quad(fp);
        bytesread = bytesread.wrapping_add(8_u32)
    } else if *lbox > 1_u32 && *lbox < 8_u32 {
        warn!("JPEG2000: Unknown LBox value {} in JP2 file!", *lbox);
    }
    bytesread
}
unsafe fn check_jp___box(mut fp: *mut FILE) -> i32 {
    if get_unsigned_quad(fp) != 0xc_u32 {
        return 0i32;
    }
    if get_unsigned_quad(fp) != 0x6a502020_u32 {
        return 0i32;
    }
    /* Next 4 bytes shall be 0D 0A 87 0A */
    if get_unsigned_quad(fp) != 0xd0a870a_u32 {
        return 0i32;
    }
    1i32
}
unsafe fn check_ftyp_data(mut fp: *mut FILE, mut size: u32) -> i32 {
    let mut supported: i32 = 0i32;
    let BR = get_unsigned_quad(fp);
    size = size.wrapping_sub(4_u32);
    /* MinV = */
    get_unsigned_quad(fp);
    size = size.wrapping_sub(4_u32);
    match BR {
        1785737760 => {
            /* "jp2 " ... supported */
            seek_relative(fp, size as i32);
            supported = 1i32
        }
        1785755680 => {
            /* "jpx " ... baseline subset supported */
            while size > 0_u32 {
                let CLi = get_unsigned_quad(fp);
                if CLi == 0x6a707862_u32 {
                    supported = 1i32
                }
                size = size.wrapping_sub(4_u32)
            }
        }
        _ => {
            warn!("JPEG2000: Unknown JPEG 2000 File Type box Brand field value.");
            seek_relative(fp, size as i32);
            supported = 0i32
        }
    }
    supported
}
unsafe fn read_res__data(info: &mut ximage_info, mut fp: *mut FILE, mut _size: u32) {
    let VR_N = get_unsigned_pair(fp) as u32;
    let VR_D = get_unsigned_pair(fp) as u32;
    let HR_N = get_unsigned_pair(fp) as u32;
    let HR_D = get_unsigned_pair(fp) as u32;
    let VR_E = get_unsigned_byte(fp);
    let HR_E = get_unsigned_byte(fp);
    info.xdensity = 72. / (HR_N as f64 / HR_D as f64 * (10f64).powf(HR_E as f64) * 0.0254);
    info.ydensity = 72. / (VR_N as f64 / VR_D as f64 * (10f64).powf(VR_E as f64) * 0.0254);
}
unsafe fn scan_res_(info: &mut ximage_info, mut fp: *mut FILE, mut size: u32) -> i32 {
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
                1919251299 => {
                    if have_resd == 0 {
                        read_res__data(info, fp, lbox.wrapping_sub(len));
                    } else {
                        seek_relative(fp, lbox.wrapping_sub(len) as i32);
                    }
                }
                1919251300 => {
                    read_res__data(info, fp, lbox.wrapping_sub(len));
                    have_resd = 1i32
                }
                _ => {
                    warn!("JPEG2000: Unknown JPEG 2000 box type in Resolution box.");
                    seek_relative(fp, lbox.wrapping_sub(len) as i32);
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
unsafe fn scan_cdef(
    _info: &mut ximage_info,
    mut smask: *mut i32,
    mut fp: *mut FILE,
    mut size: u32,
) -> i32 {
    let mut opacity_channels: i32 = 0i32; /* Cn */
    let mut have_type0: i32 = 0i32; /* must be 0 for SMask */
    *smask = 0i32;
    let N = get_unsigned_pair(fp) as u32;
    if size < N.wrapping_mul(6_u32).wrapping_add(2_u32) {
        warn!("JPEG2000: Inconsistent N value in Channel Definition box.");
        return -1i32;
    }
    for _ in 0..N {
        let Cn = get_unsigned_pair(fp) as u32;
        let Typ = get_unsigned_pair(fp) as u32;
        let Asoc = get_unsigned_pair(fp) as u32;
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
unsafe fn scan_jp2h(
    info: &mut ximage_info,
    mut smask: *mut i32,
    mut fp: *mut FILE,
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
                    info.height = get_unsigned_quad(fp) as i32;
                    info.width = get_unsigned_quad(fp) as i32;
                    info.num_components = get_unsigned_pair(fp) as i32;
                    /* c = */
                    get_unsigned_byte(fp); /* BPC - 1 */
                    /* c = */
                    get_unsigned_byte(fp); /* C: Compression type */
                    /* c = */
                    get_unsigned_byte(fp); /* UnkC */
                    /* c = */
                    get_unsigned_byte(fp); /* IPR */
                    have_ihdr = 1i32
                }
                1919251232 => error = scan_res_(info, fp, lbox.wrapping_sub(len)),
                1667523942 => error = scan_cdef(info, smask, fp, lbox.wrapping_sub(len)),
                1651532643 | 1668246642 | 1885564018 | 1668112752 | 1818389536 => {
                    seek_relative(fp, lbox.wrapping_sub(len) as i32);
                }
                _ => {
                    warn!("JPEG2000: Unknown JPEG 2000 box in JP2 Header box.");
                    seek_relative(fp, lbox.wrapping_sub(len) as i32);
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
unsafe fn scan_file(info: &mut ximage_info, mut smask: *mut i32, mut fp: *mut FILE) -> i32 {
    let mut error: i32 = 0i32;
    let mut have_jp2h: i32 = 0i32;
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    let mut size = file_size(fp);
    /* Should have already been checked before. */
    /* JPEG 2000 Singature box */
    if check_jp___box(fp) == 0 {
        return -1i32;
    }
    size -= 12i32;
    /* File Type box shall immediately follow */
    let mut len = read_box_hdr(fp, &mut lbox, &mut tbox);
    if tbox != 0x66747970_u32 {
        return -1i32;
    }
    if check_ftyp_data(fp, lbox.wrapping_sub(len)) == 0 {
        return -1i32;
    }
    size = (size as u32).wrapping_sub(lbox) as i32 as i32;
    /* Search for JP2 Header box */
    while size > 0i32 && error == 0 {
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
                seek_relative(fp, lbox.wrapping_sub(len) as i32);
            }
            _ => {
                seek_relative(fp, lbox.wrapping_sub(len) as i32);
            }
        }
        size = (size as u32).wrapping_sub(lbox) as i32 as i32
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
#[no_mangle]
pub unsafe fn check_for_jp2(mut fp: *mut FILE) -> i32 {
    let mut lbox: u32 = 0;
    let mut tbox: u32 = 0;
    if fp.is_null() {
        return 0i32;
    }
    rewind(fp);
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
#[no_mangle]
pub unsafe fn jp2_include_image(mut ximage: *mut pdf_ximage, mut fp: *mut FILE) -> i32 {
    let mut smask: i32 = 0i32;
    let mut info = ximage_info::default();
    let pdf_version = pdf_get_version();
    if pdf_version < 5_u32 {
        warn!(
            "JPEG 2000 support requires PDF version >= 1.5 (Current setting 1.{})\n",
            pdf_version
        );
        return -1i32;
    }
    pdf_ximage_init_image_info(&mut info);
    rewind(fp);
    if scan_file(&mut info, &mut smask, fp) < 0i32 {
        warn!("JPEG2000: Reading JPEG 2000 file failed.");
        return -1i32;
    }
    let stream = pdf_new_stream(0i32);
    let stream_dict = (*stream).as_stream_mut().get_dict_mut();
    pdf_add_dict(stream_dict, "Filter", pdf_new_name("JPXDecode"));
    if smask != 0 {
        pdf_add_dict(stream_dict, "SMaskInData", pdf_new_number(1i32 as f64));
    }
    /* Read whole file */
    rewind(fp);
    loop {
        let nb_read = fread(
            work_buffer.as_mut_ptr() as *mut libc::c_void,
            ::std::mem::size_of::<i8>(),
            1024,
            fp,
        ) as i32;
        if !(nb_read > 0i32) {
            break;
        }
        pdf_add_stream(
            &mut *stream,
            work_buffer.as_mut_ptr() as *const libc::c_void,
            nb_read,
        );
    }
    pdf_ximage_set_image(ximage, &mut info, stream);
    0i32
}
#[no_mangle]
pub unsafe fn jp2_get_bbox(
    mut fp: *mut FILE,
    mut width: *mut i32,
    mut height: *mut i32,
    mut xdensity: *mut f64,
    mut ydensity: *mut f64,
) -> i32 {
    let mut smask: i32 = 0i32;
    let mut info = ximage_info::default();
    pdf_ximage_init_image_info(&mut info);
    rewind(fp);
    let r = scan_file(&mut info, &mut smask, fp);
    *width = info.width;
    *height = info.height;
    *xdensity = info.xdensity;
    *ydensity = info.ydensity;
    r
}
