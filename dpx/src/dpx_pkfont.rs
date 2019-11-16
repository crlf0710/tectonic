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
use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_mem::new;
use super::dpx_mfileio::work_buffer_u8 as work_buffer;
use super::dpx_pdfdev::pdf_sprint_number;
use super::dpx_pdfencoding::{
    pdf_encoding_get_encoding, pdf_encoding_get_name, pdf_encoding_used_by_type3,
};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_encoding, pdf_font_get_ident, pdf_font_get_param, pdf_font_get_resource,
    pdf_font_get_usedchars, pdf_font_is_in_use, pdf_font_set_fontname,
};
use super::dpx_tfm::{tfm_get_design_size, tfm_open};
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_copy_name, pdf_new_array, pdf_new_dict,
    pdf_new_name, pdf_new_number, pdf_new_stream, pdf_obj, pdf_ref_obj, pdf_release_obj, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use libc::{fclose, fgetc, fopen, fread, free, memset};

use crate::dpx_numbers::{
    get_positive_quad, get_signed_byte, get_signed_pair, get_signed_quad, get_unsigned_byte,
    get_unsigned_num, get_unsigned_pair, get_unsigned_triple, skip_bytes,
};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type size_t = u64;
use super::dpx_pdfdev::Rect;
use libc::FILE;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pk_header_ {
    pub pkt_len: u32,
    pub chrcode: i32,
    pub wd: i32,
    pub dx: i32,
    pub dy: i32,
    pub bm_wd: u32,
    pub bm_ht: u32,
    pub bm_hoff: i32,
    pub bm_voff: i32,
    pub dyn_f: i32,
    pub run_color: i32,
}
static mut base_dpi: u32 = 600u32;
#[no_mangle]
pub unsafe extern "C" fn PKFont_set_dpi(mut dpi: i32) {
    if dpi <= 0i32 {
        panic!("Invalid DPI: {}\n", dpi);
    }
    base_dpi = dpi as u32;
}
/* (Only) This requires TFM to get design size... */
unsafe fn truedpi(mut ident: *const i8, mut point_size: f64, mut bdpi: u32) -> u32 {
    let mut dpi: u32 = bdpi;
    let tfm_id = tfm_open(ident, 0i32);
    if tfm_id < 0i32 {
        return dpi;
    }
    let design_size = tfm_get_design_size(tfm_id);
    if design_size <= 0.0f64 {
        warn!(
            "DESGIN_SIZE <= 0.0? (TFM=\"{}\")",
            CStr::from_ptr(ident).display(),
        );
    } else {
        dpi =
            ((base_dpi as f64 * point_size / design_size / 1.0f64 + 0.5f64).floor() * 1.0f64) as u32
    }
    dpi
}
unsafe fn dpx_open_pk_font_at(_ident: *const i8, _dpi: u32) -> *mut FILE {
    /*kpse_glyph_file_type kpse_file_info;*/
    let fqpn = ptr::null_mut::<i8>(); /*kpse_find_glyph(ident, dpi, kpse_pk_format, &kpse_file_info);*/
    if fqpn.is_null() {
        return ptr::null_mut();
    }
    let fp = fopen(fqpn, b"rb\x00" as *const u8 as *const i8);
    free(fqpn as *mut libc::c_void);
    fp
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_open_pkfont(mut font: *mut pdf_font) -> i32 {
    let ident = pdf_font_get_ident(font);
    let point_size = pdf_font_get_param(font, 2i32);
    let encoding_id = pdf_font_get_encoding(font);
    if ident.is_null() || point_size <= 0.0f64 {
        return -1i32;
    }
    let dpi = truedpi(ident, point_size, base_dpi);
    let fp = dpx_open_pk_font_at(ident, dpi);
    if fp.is_null() {
        return -1i32;
    }
    fclose(fp);
    /* Type 3 fonts doesn't have FontName.
     * FontFamily is recommended for PDF 1.5.
     */
    pdf_font_set_fontname(font, ident);
    if encoding_id >= 0i32 {
        pdf_encoding_used_by_type3(encoding_id);
        warn!(
            "PK font is found for font \"{}\" but non built-in encoding \"{}\" is specified.",
            CStr::from_ptr(ident).display(),
            CStr::from_ptr(pdf_encoding_get_name(encoding_id)).display(),
        );
        warn!(">> Assuming this is for glyph name assignment.");
    }
    0i32
}
/* We are using Mask Image. Fill black is bit clear.
 * Optimizing those codes doesn't improve things.
 */
unsafe fn fill_black_run(mut dp: *mut u8, mut left: u32, mut run_count: u32) -> u32 {
    static mut mask: [u8; 8] = [
        127u32 as u8,
        191u32 as u8,
        223u32 as u8,
        239u32 as u8,
        247u32 as u8,
        251u32 as u8,
        253u32 as u8,
        254u32 as u8,
    ];
    let mut right: u32 = left.wrapping_add(run_count).wrapping_sub(1_u32);
    while left <= right {
        let ref mut fresh0 = *dp.offset(left.wrapping_div(8_u32) as isize);
        *fresh0 = (*fresh0 as i32 & mask[left.wrapping_rem(8_u32) as usize] as i32) as u8;
        left = left.wrapping_add(1)
    }
    run_count
}
/* Just skip bits. See decode_packed() */
unsafe fn fill_white_run(mut run_count: u32) -> u32 {
    run_count
}
unsafe fn pk_packed_num(mut np: *mut u32, mut dyn_f: i32, mut dp: *mut u8, mut pl: u32) -> u32 {
    let mut nmbr: u32 = 0_u32;
    let mut i: u32 = *np;
    if i.wrapping_div(2_u32) == pl {
        warn!("EOD reached while unpacking pk_packed_num.");
        return 0_u32;
    }
    let mut nyb = if i.wrapping_rem(2_u32) != 0 {
        *dp.offset(i.wrapping_div(2_u32) as isize) as i32 & 0xfi32
    } else {
        *dp.offset(i.wrapping_div(2_u32) as isize) as i32 >> 4i32 & 0xfi32
    };
    i = i.wrapping_add(1);
    if nyb == 0i32 {
        let mut j = 0i32;
        loop {
            if i.wrapping_div(2_u32) == pl {
                warn!("EOD reached while unpacking pk_packed_num.");
                break;
            } else {
                nyb = if i.wrapping_rem(2_u32) != 0 {
                    *dp.offset(i.wrapping_div(2_u32) as isize) as i32 & 0xfi32
                } else {
                    *dp.offset(i.wrapping_div(2_u32) as isize) as i32 >> 4i32 & 0xfi32
                };
                i = i.wrapping_add(1);
                j += 1;
                if !(nyb == 0i32) {
                    break;
                }
            }
        }
        nmbr = nyb as u32;
        loop {
            let fresh1 = j;
            j = j - 1;
            if !(fresh1 > 0i32) {
                break;
            }
            if i.wrapping_div(2_u32) == pl {
                warn!("EOD reached while unpacking pk_packed_num.");
                break;
            } else {
                nyb = if i.wrapping_rem(2_u32) != 0 {
                    *dp.offset(i.wrapping_div(2_u32) as isize) as i32 & 0xfi32
                } else {
                    *dp.offset(i.wrapping_div(2_u32) as isize) as i32 >> 4i32 & 0xfi32
                };
                i = i.wrapping_add(1);
                nmbr = nmbr.wrapping_mul(16_u32).wrapping_add(nyb as u32)
            }
        }
        nmbr = (nmbr as u32).wrapping_add(((13i32 - dyn_f) * 16i32 + dyn_f - 15i32) as u32) as u32
    } else if nyb <= dyn_f {
        nmbr = nyb as u32
    } else if nyb < 14i32 {
        if i.wrapping_div(2_u32) == pl {
            warn!("EOD reached while unpacking pk_packed_num.");
            return 0_u32;
        }
        nmbr = ((nyb - dyn_f - 1i32) * 16i32
            + (if i.wrapping_rem(2_u32) != 0 {
                *dp.offset(i.wrapping_div(2_u32) as isize) as i32 & 0xfi32
            } else {
                *dp.offset(i.wrapping_div(2_u32) as isize) as i32 >> 4i32 & 0xfi32
            })
            + dyn_f
            + 1i32) as u32;
        i = i.wrapping_add(1)
    }
    *np = i;
    nmbr
}
unsafe fn send_out(mut rowptr: *mut u8, mut rowbytes: u32, mut stream: *mut pdf_obj) {
    pdf_add_stream(&mut *stream, rowptr as *mut libc::c_void, rowbytes as i32);
}
unsafe fn pk_decode_packed(
    mut stream: *mut pdf_obj,
    mut wd: u32,
    mut ht: u32,
    mut dyn_f: i32,
    mut run_color: i32,
    mut dp: *mut u8,
    mut pl: u32,
) -> i32 {
    let mut run_count: u32 = 0_u32;
    let rowbytes = wd.wrapping_add(7_u32).wrapping_div(8_u32);
    let rowptr =
        new((rowbytes as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    /* repeat count is applied to the *current* row.
     * "run" can span across rows.
     * If there are non-zero repeat count and if run
     * spans across row, first repeat and then continue.
     */
    let mut np = 0_u32; /* 1 is white */
    let mut i = 0;
    while i < ht {
        let mut nbits;
        let mut repeat_count = 0;
        memset(rowptr as *mut libc::c_void, 0xffi32, rowbytes as _);
        let mut rowbits_left = wd;
        /* Fill run left over from previous row */
        if run_count > 0_u32 {
            nbits = if rowbits_left < run_count {
                rowbits_left
            } else {
                run_count
            };
            match run_color {
                0 => {
                    rowbits_left = (rowbits_left as u32)
                        .wrapping_sub(fill_black_run(rowptr, 0_u32, nbits))
                        as u32
                }
                1 => {
                    rowbits_left = (rowbits_left as u32).wrapping_sub(fill_white_run(nbits)) as u32
                }
                _ => {}
            }
            run_count = (run_count as u32).wrapping_sub(nbits) as u32
        }
        /* Read nybbles until we have a full row */
        while np.wrapping_div(2_u32) < pl && rowbits_left > 0_u32 {
            let nyb = if np.wrapping_rem(2_u32) != 0 {
                *dp.offset(np.wrapping_div(2_u32) as isize) as i32 & 0xfi32
            } else {
                *dp.offset(np.wrapping_div(2_u32) as isize) as i32 >> 4i32 & 0xfi32
            };
            if nyb == 14i32 {
                /* packed number "repeat_count" follows */
                if repeat_count != 0_u32 {
                    warn!("Second repeat count for this row!");
                    /* Consume this nybble */
                } /* run_count */
                np = np.wrapping_add(1); /* Consume this nybble */
                repeat_count = pk_packed_num(&mut np, dyn_f, dp, pl)
            } else if nyb == 15i32 {
                if repeat_count != 0_u32 {
                    warn!("Second repeat count for this row!");
                }
                np = np.wrapping_add(1);
                repeat_count = 1_u32
            } else {
                /* Interprete current nybble as packed number */
                run_count = pk_packed_num(&mut np, dyn_f, dp, pl);
                nbits = if rowbits_left < run_count {
                    rowbits_left
                } else {
                    run_count
                };
                run_color = (run_color == 0) as i32;
                run_count = (run_count as u32).wrapping_sub(nbits) as u32;
                match run_color {
                    0 => {
                        rowbits_left = (rowbits_left as u32).wrapping_sub(fill_black_run(
                            rowptr,
                            wd.wrapping_sub(rowbits_left),
                            nbits,
                        )) as u32
                    }
                    1 => {
                        rowbits_left =
                            (rowbits_left as u32).wrapping_sub(fill_white_run(nbits)) as u32
                    }
                    _ => {}
                }
            }
        }
        /* We got bitmap row data. */
        send_out(rowptr, rowbytes, stream);
        while i < ht && repeat_count > 0_u32 {
            send_out(rowptr, rowbytes, stream);
            repeat_count = repeat_count.wrapping_sub(1);
            i = i.wrapping_add(1)
        }
        i = i.wrapping_add(1)
    }
    free(rowptr as *mut libc::c_void);
    0i32
}
unsafe fn pk_decode_bitmap(
    mut stream: *mut pdf_obj,
    mut wd: u32,
    mut ht: u32,
    mut dyn_f: i32,
    mut run_color: i32,
    mut dp: *mut u8,
    mut pl: u32,
) -> i32 {
    static mut mask: [u8; 8] = [
        0x80u32 as u8,
        0x40u32 as u8,
        0x20u32 as u8,
        0x10u32 as u8,
        0x8u32 as u8,
        0x4u32 as u8,
        0x2u32 as u8,
        0x1u32 as u8,
    ];
    assert!(dyn_f == 14i32);
    if run_color != 0i32 {
        warn!("run_color != 0 for bitmap pk data?");
    } else if pl < wd.wrapping_mul(ht).wrapping_add(7_u32).wrapping_div(8_u32) {
        warn!(
            "Insufficient bitmap pk data. {}bytes expected but only {}bytes read.",
            wd.wrapping_mul(ht).wrapping_add(7_u32).wrapping_div(8_u32),
            pl,
        );
        return -1i32;
    }
    let rowbytes = wd.wrapping_add(7_u32).wrapping_div(8_u32);
    let rowptr =
        new((rowbytes as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memset(rowptr as *mut libc::c_void, 0i32, rowbytes as _);
    /* Flip. PK bitmap is not byte aligned for each rows. */
    /* flip bit */
    let mut j = 0_u32;
    for i in 0..ht.wrapping_mul(wd) {
        let c = (*dp.offset(i.wrapping_div(8_u32) as isize) as i32
            & mask[i.wrapping_rem(8_u32) as usize] as i32) as u8;
        if c as i32 == 0i32 {
            let ref mut fresh2 = *rowptr.offset(j.wrapping_div(8_u32) as isize);
            *fresh2 = (*fresh2 as i32 | mask[i.wrapping_rem(8_u32) as usize] as i32) as u8
        }
        j += 1;
        if j == wd {
            send_out(rowptr, rowbytes, stream);
            memset(rowptr as *mut libc::c_void, 0i32, rowbytes as _);
            j = 0
        }
    }
    0i32
}
unsafe fn do_preamble(mut fp: *mut FILE) {
    /* Check for id byte */
    if fgetc(fp) == 89i32 {
        /* Skip comment */
        skip_bytes(get_unsigned_byte(fp) as u32, fp);
        /* Skip other header info.  It's normally used for verifying this
        is the file wethink it is */
        skip_bytes(16_u32, fp);
    } else {
        panic!("embed_pk_font: PK ID byte is incorrect.  Are you sure this is a PK file?");
    };
}
unsafe fn read_pk_char_header(mut h: *mut pk_header_, mut opcode: u8, mut fp: *mut FILE) -> i32 {
    assert!(!h.is_null());
    if opcode as i32 & 4i32 == 0i32 {
        /* short */
        (*h).pkt_len = ((opcode as i32 & 3i32) << 8i32 | get_unsigned_byte(fp) as i32) as u32; /* TFM width */
        (*h).chrcode = get_unsigned_byte(fp) as i32; /* horizontal escapement */
        (*h).wd = get_unsigned_triple(fp) as i32; /* extended short */
        (*h).dx = (get_unsigned_byte(fp) as i32) << 16i32;
        (*h).dy = 0i32;
        (*h).bm_wd = get_unsigned_byte(fp) as u32;
        (*h).bm_ht = get_unsigned_byte(fp) as u32;
        (*h).bm_hoff = get_signed_byte(fp) as i32;
        (*h).bm_voff = get_signed_byte(fp) as i32;
        (*h).pkt_len = ((*h).pkt_len as u32).wrapping_sub(8_u32) as u32 as u32
    } else if opcode as i32 & 7i32 == 7i32 {
        /* long */
        (*h).pkt_len = get_positive_quad(
            fp,
            b"PK\x00" as *const u8 as *const i8,
            b"pkt_len\x00" as *const u8 as *const i8,
        ); /* 16.16 fixed point number in pixels */
        (*h).chrcode = get_signed_quad(fp);
        (*h).wd = get_signed_quad(fp);
        (*h).dx = get_signed_quad(fp);
        (*h).dy = get_signed_quad(fp);
        (*h).bm_wd = get_positive_quad(
            fp,
            b"PK\x00" as *const u8 as *const i8,
            b"bm_wd\x00" as *const u8 as *const i8,
        );
        (*h).bm_ht = get_positive_quad(
            fp,
            b"PK\x00" as *const u8 as *const i8,
            b"bm_ht\x00" as *const u8 as *const i8,
        );
        (*h).bm_hoff = get_signed_quad(fp);
        (*h).bm_voff = get_signed_quad(fp);
        (*h).pkt_len = ((*h).pkt_len as u32).wrapping_sub(28_u32) as u32
    } else {
        (*h).pkt_len = ((opcode as i32 & 3i32) << 16i32 | get_unsigned_pair(fp) as i32) as u32;
        (*h).chrcode = get_unsigned_byte(fp) as i32;
        (*h).wd = get_unsigned_triple(fp) as i32;
        (*h).dx = (get_unsigned_pair(fp) as i32) << 16i32;
        (*h).dy = 0i32;
        (*h).bm_wd = get_unsigned_pair(fp) as u32;
        (*h).bm_ht = get_unsigned_pair(fp) as u32;
        (*h).bm_hoff = get_signed_pair(fp) as i32;
        (*h).bm_voff = get_signed_pair(fp) as i32;
        (*h).pkt_len = ((*h).pkt_len as u32).wrapping_sub(13_u32) as u32
    }
    (*h).dyn_f = opcode as i32 / 16i32;
    (*h).run_color = if opcode as i32 & 8i32 != 0 {
        1i32
    } else {
        0i32
    };
    if (*h).chrcode as u32 > 0xff_u32 {
        warn!(
            "Unable to handle long characters in PK files: code=0x{:04x}",
            (*h).chrcode,
        );
        return -1i32;
    }
    0i32
}
/* CCITT Group 4 filter may reduce file size. */
unsafe fn create_pk_CharProc_stream(
    mut pkh: *mut pk_header_,
    mut chrwid: f64,
    mut pkt_ptr: *mut u8,
    mut pkt_len: u32,
) -> *mut pdf_obj {
    let llx = -(*pkh).bm_hoff;
    let lly = ((*pkh).bm_voff as u32).wrapping_sub((*pkh).bm_ht) as i32;
    let urx = (*pkh).bm_wd.wrapping_sub((*pkh).bm_hoff as u32) as i32;
    let ury = (*pkh).bm_voff;
    let stream = pdf_new_stream(STREAM_COMPRESS); /* charproc */
    /*
     * The following line is a "metric" for the PDF reader:
     *
     * PDF Reference Reference, 4th ed., p.385.
     *
     * The wx (first operand of d1) must be consistent with the corresponding
     * width in the font's Widths array. The format string of sprint() must be
     * consistent with write_number() in pdfobj.c.
     */
    let mut len = pdf_sprint_number(&mut work_buffer[..], chrwid);
    len += sprintf(
        work_buffer.as_mut_ptr().offset(len as isize) as *mut i8,
        b" 0 %d %d %d %d d1\n\x00" as *const u8 as *const i8,
        llx,
        lly,
        urx,
        ury,
    ) as usize;
    pdf_add_stream(
        &mut *stream,
        work_buffer.as_mut_ptr() as *const libc::c_void,
        len as i32,
    );
    /*
     * Acrobat dislike transformation [0 0 0 0 dx dy].
     * PDF Reference, 4th ed., p.147, says,
     *
     *   Use of a noninvertible matrix when painting graphics objects can result in
     *   unpredictable behavior.
     *
     * but it does not forbid use of such transformation.
     */
    if (*pkh).bm_wd != 0_u32 && (*pkh).bm_ht != 0_u32 && pkt_len > 0_u32 {
        /* Otherwise we embed an empty stream :-( */
        /* Scale and translate origin to lower left corner for raster data */
        len = sprintf(
            work_buffer.as_mut_ptr() as *mut i8,
            b"q\n%u 0 0 %u %d %d cm\n\x00" as *const u8 as *const i8,
            (*pkh).bm_wd,
            (*pkh).bm_ht,
            llx,
            lly,
        ) as usize;
        pdf_add_stream(
            &mut *stream,
            work_buffer.as_mut_ptr() as *const libc::c_void,
            len as i32,
        );
        len = sprintf(
            work_buffer.as_mut_ptr() as *mut i8,
            b"BI\n/W %u\n/H %u\n/IM true\n/BPC 1\nID \x00" as *const u8 as *const i8,
            (*pkh).bm_wd,
            (*pkh).bm_ht,
        ) as usize;
        pdf_add_stream(
            &mut *stream,
            work_buffer.as_mut_ptr() as *const libc::c_void,
            len as i32,
        );
        /* Add bitmap data */
        if (*pkh).dyn_f == 14i32 {
            /* bitmap */
            pk_decode_bitmap(
                stream,
                (*pkh).bm_wd,
                (*pkh).bm_ht,
                (*pkh).dyn_f,
                (*pkh).run_color,
                pkt_ptr,
                pkt_len,
            );
        } else {
            pk_decode_packed(
                stream,
                (*pkh).bm_wd,
                (*pkh).bm_ht,
                (*pkh).dyn_f,
                (*pkh).run_color,
                pkt_ptr,
                pkt_len,
            );
        }
        len = sprintf(
            work_buffer.as_mut_ptr() as *mut i8,
            b"\nEI\nQ\x00" as *const u8 as *const i8,
        ) as usize;
        pdf_add_stream(
            &mut *stream,
            work_buffer.as_mut_ptr() as *const libc::c_void,
            len as i32,
        );
    }
    stream
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_load_pkfont(mut font: *mut pdf_font) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    let mut charavail: [i8; 256] = [0; 256];
    /* ENABLE_GLYPHENC */
    if !pdf_font_is_in_use(font) {
        return 0i32;
    }
    let ident = pdf_font_get_ident(font);
    let point_size = pdf_font_get_param(font, 2i32);
    let usedchars = pdf_font_get_usedchars(font);
    let encoding_id = pdf_font_get_encoding(font);
    let enc_vec = if encoding_id < 0i32 {
        0 as *mut *mut i8
    } else {
        pdf_encoding_get_encoding(encoding_id)
    };
    /* ENABLE_GLYPHENC */
    assert!(!ident.is_null() && !usedchars.is_null() && point_size > 0.0f64);
    let dpi = truedpi(ident, point_size, base_dpi);
    let fp = dpx_open_pk_font_at(ident, dpi);
    if fp.is_null() {
        panic!(
            "Could not find/open PK font file: {} (at {}dpi)",
            CStr::from_ptr(ident).display(),
            dpi,
        );
    }
    memset(charavail.as_mut_ptr() as *mut libc::c_void, 0i32, 256);
    let charprocs = pdf_new_dict();
    /* Include bitmap as 72dpi image:
     * There seems to be problems in "scaled" bitmap glyph
     * rendering in several viewers.
     */
    let pix2charu = 72.0f64 * 1000.0f64 / base_dpi as f64 / point_size; /* A command byte */
    let mut bbox = Rect::new(
        (core::f64::INFINITY, core::f64::INFINITY),
        (core::f64::NEG_INFINITY, core::f64::NEG_INFINITY)
    );
    loop {
        let opcode = fgetc(fp);
        if !(opcode >= 0i32 && opcode != 245i32) {
            break;
        }
        if opcode < 240i32 {
            let mut pkh: pk_header_ = pk_header_ {
                pkt_len: 0,
                chrcode: 0,
                wd: 0,
                dx: 0,
                dy: 0,
                bm_wd: 0,
                bm_ht: 0,
                bm_hoff: 0,
                bm_voff: 0,
                dyn_f: 0,
                run_color: 0,
            };
            let error = read_pk_char_header(&mut pkh, opcode as u8, fp);
            if error != 0 {
                panic!("Error in reading PK character header.");
            } else {
                if charavail[(pkh.chrcode & 0xffi32) as usize] != 0 {
                    warn!(
                        "More than two bitmap image for single glyph?: font=\"{}\" code=0x{:02x}",
                        CStr::from_ptr(ident).display(),
                        pkh.chrcode,
                    );
                }
            }
            if *usedchars.offset((pkh.chrcode & 0xffi32) as isize) == 0 {
                skip_bytes(pkh.pkt_len, fp);
            } else {
                let mut charname;
                /* Charwidth in PDF units */
                let charwidth =
                    (1000.0f64 * pkh.wd as f64 / ((1i32 << 20i32) as f64 * pix2charu) / 0.1f64
                        + 0.5f64)
                        .floor()
                        * 0.1f64;
                widths[(pkh.chrcode & 0xffi32) as usize] = charwidth;
                /* Update font BBox info */
                bbox.ll.x = bbox.ll.x.min(-pkh.bm_hoff as f64);
                bbox.ll.y = bbox.ll.y.min(pkh.bm_voff as f64 - pkh.bm_ht as f64);
                bbox.ur.x = bbox.ur.x.max(pkh.bm_wd as f64 - pkh.bm_hoff as f64);
                bbox.ur.y = bbox.ur.y.max(pkh.bm_voff as f64);
                let pkt_ptr = new((pkh.pkt_len as u64)
                    .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                    as u32) as *mut u8;
                let bytesread =
                    fread(pkt_ptr as *mut libc::c_void, 1, pkh.pkt_len as _, fp) as size_t;
                if bytesread != pkh.pkt_len as u64 {
                    panic!(
                        "Only {} bytes PK packet read. (expected {} bytes)",
                        bytesread, pkh.pkt_len,
                    );
                }
                let charproc =
                    create_pk_CharProc_stream(&mut pkh, charwidth, pkt_ptr, bytesread as u32);
                free(pkt_ptr as *mut libc::c_void);
                if charproc.is_null() {
                    panic!("Unpacking PK character data failed.");
                }
                if encoding_id >= 0i32 && !enc_vec.is_null() {
                    charname = *enc_vec.offset((pkh.chrcode & 0xffi32) as isize);
                    if charname.is_null() {
                        warn!(
                            "\".notdef\" glyph used in font (code=0x{:02x}): {}",
                            pkh.chrcode,
                            CStr::from_ptr(ident).display(),
                        );
                        charname = work_buffer.as_mut_ptr() as *mut i8;
                        sprintf(
                            charname,
                            b"x%02X\x00" as *const u8 as *const i8,
                            pkh.chrcode as u8 as i32,
                        );
                    }
                } else {
                    /* ENABLE_GLYPHENC */
                    charname = work_buffer.as_mut_ptr() as *mut i8; /* _FIXME_ */
                    sprintf(
                        charname,
                        b"x%02X\x00" as *const u8 as *const i8,
                        pkh.chrcode as u8 as i32,
                    );
                }
                pdf_add_dict(
                    &mut *charprocs,
                    CStr::from_ptr(charname).to_bytes(),
                    pdf_ref_obj(charproc),
                );
                pdf_release_obj(charproc);
            }
            charavail[(pkh.chrcode & 0xffi32) as usize] = 1_i8
        } else {
            match opcode {
                240 | 241 | 242 | 243 => {
                    let mut len: i32 = get_unsigned_num(fp, (opcode - 240i32) as u8) as i32;
                    if len < 0i32 {
                        warn!("PK: Special with {} bytes???", len);
                    } else {
                        skip_bytes(len as u32, fp);
                    }
                }
                244 => {
                    skip_bytes(4_u32, fp);
                }
                247 => {
                    do_preamble(fp);
                }
                246 | _ => {}
            }
        }
    }
    fclose(fp);
    /* Check if we really got all glyphs needed. */
    for code in 0..256 {
        if *usedchars.offset(code as isize) as i32 != 0 && charavail[code as usize] == 0 {
            warn!(
                "Missing glyph code=0x{:02x} in PK font \"{}\".",
                code,
                CStr::from_ptr(ident).display(),
            );
        }
    }
    /* Now actually fill fontdict. */
    let fontdict = pdf_font_get_resource(&mut *font);
    pdf_add_dict(fontdict, "CharProcs", pdf_ref_obj(charprocs));
    pdf_release_obj(charprocs);
    /*
     * Resources:
     *
     *  PDF Reference 4th ed. describes it as "Optional but strongly recommended".
     *  There are no reason to put it in our case, but we will put this.
     *  We do not care about compatibility with Acrobat 2.x. (See implementation
     *  note 47, Appendix H of PDF Ref., 4th ed.).
     */
    let procset = pdf_new_dict();
    let tmp_array = pdf_new_array();
    pdf_add_array(&mut *tmp_array, pdf_new_name("PDF"));
    pdf_add_array(&mut *tmp_array, pdf_new_name("ImageB"));
    pdf_add_dict(&mut *procset, "ProcSet", tmp_array);
    pdf_add_dict(fontdict, "Resources", procset);
    /* Encoding */
    let tmp_array = pdf_new_array();
    let mut prev = -2i32;
    let mut firstchar = 255i32;
    let mut lastchar = 0i32;
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            let mut charname_0;
            if code < firstchar {
                firstchar = code
            }
            if code > lastchar {
                lastchar = code
            }
            if code != prev + 1i32 {
                pdf_add_array(&mut *tmp_array, pdf_new_number(code as f64));
            }
            if encoding_id >= 0i32 && !enc_vec.is_null() {
                charname_0 = *enc_vec.offset(code as u8 as isize);
                if charname_0.is_null() {
                    charname_0 = work_buffer.as_mut_ptr() as *mut i8;
                    sprintf(
                        charname_0,
                        b"x%02X\x00" as *const u8 as *const i8,
                        code as u8 as i32,
                    );
                }
            } else {
                /* ENABLE_GLYPHENC */
                charname_0 = work_buffer.as_mut_ptr() as *mut i8;
                sprintf(
                    charname_0,
                    b"x%02X\x00" as *const u8 as *const i8,
                    code as u8 as i32,
                );
            }
            pdf_add_array(&mut *tmp_array, pdf_copy_name(charname_0));
            prev = code
        }
    }
    if firstchar > lastchar {
        pdf_release_obj(tmp_array);
        panic!(
            "Unexpected error: firstchar > lastchar ({} {})",
            firstchar, lastchar
        );
    }
    if encoding_id < 0i32 || enc_vec.is_null() {
        /* ENABLE_GLYPHENC */
        let encoding = pdf_new_dict();
        pdf_add_dict(&mut *encoding, "Type", pdf_new_name("Encoding"));
        pdf_add_dict(&mut *encoding, "Differences", tmp_array);
        pdf_add_dict(fontdict, "Encoding", pdf_ref_obj(encoding));
        pdf_release_obj(encoding);
    } else {
        pdf_release_obj(tmp_array);
    }
    /* FontBBox: Accurate value is important.
     */
    let tmp_array = pdf_new_array();
    pdf_add_array(&mut *tmp_array, pdf_new_number(bbox.ll.x));
    pdf_add_array(&mut *tmp_array, pdf_new_number(bbox.ll.y));
    pdf_add_array(&mut *tmp_array, pdf_new_number(bbox.ur.x));
    pdf_add_array(&mut *tmp_array, pdf_new_number(bbox.ur.y));
    pdf_add_dict(fontdict, "FontBBox", tmp_array);
    /* Widths:
     *  Indirect reference preffered. (See PDF Reference)
     */
    let tmp_array = pdf_new_array();
    for code in firstchar..=lastchar {
        if *usedchars.offset(code as isize) != 0 {
            pdf_add_array(&mut *tmp_array, pdf_new_number(widths[code as usize]));
        } else {
            pdf_add_array(&mut *tmp_array, pdf_new_number(0i32 as f64));
        }
    }
    pdf_add_dict(fontdict, "Widths", pdf_ref_obj(tmp_array));
    pdf_release_obj(tmp_array);
    /* FontMatrix */
    let tmp_array = pdf_new_array();
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.001f64 * pix2charu));
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.001f64 * pix2charu));
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
    pdf_add_array(&mut *tmp_array, pdf_new_number(0.0f64));
    pdf_add_dict(fontdict, "FontMatrix", tmp_array);
    pdf_add_dict(fontdict, "FirstChar", pdf_new_number(firstchar as f64));
    pdf_add_dict(fontdict, "LastChar", pdf_new_number(lastchar as f64));
    0i32
}
