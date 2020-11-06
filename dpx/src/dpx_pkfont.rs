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

use euclid::point2;
use std::io::Write;

use crate::warn;

use super::dpx_mem::new;
use super::dpx_pdfdev::{pdf_sprint_number, Rect};
use super::dpx_pdfencoding::{
    pdf_encoding_get_encoding, pdf_encoding_get_name, pdf_encoding_used_by_type3,
};
use super::dpx_pdffont::{
    pdf_font, pdf_font_get_encoding, pdf_font_get_param, pdf_font_get_resource,
    pdf_font_get_usedchars, pdf_font_is_in_use,
};
use super::dpx_tfm::{tfm_get_design_size, tfm_open};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_name, pdf_ref_obj, pdf_release_obj, pdf_stream, IntoObj, PushObj, STREAM_COMPRESS,
};
use libc::{free, memset};

use crate::dpx_numbers::{
    get_positive_quad, get_unsigned_num, get_unsigned_triple, skip_bytes, GetFromFile,
};

use bridge::{InFile, TTInputFormat};
use std::io::Read;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pk_header_ {
    pub(crate) pkt_len: u32,
    pub(crate) chrcode: i32,
    pub(crate) wd: i32,
    pub(crate) dx: i32,
    pub(crate) dy: i32,
    pub(crate) bm_wd: u32,
    pub(crate) bm_ht: u32,
    pub(crate) bm_hoff: i32,
    pub(crate) bm_voff: i32,
    pub(crate) dyn_f: i32,
    pub(crate) run_color: i32,
}
static mut base_dpi: u32 = 600u32;

pub(crate) unsafe fn PKFont_set_dpi(dpi: i32) {
    if dpi <= 0i32 {
        panic!("Invalid DPI: {}\n", dpi);
    }
    base_dpi = dpi as u32;
}
/* (Only) This requires TFM to get design size... */
unsafe fn truedpi(ident: &str, point_size: f64, bdpi: u32) -> u32 {
    let mut dpi: u32 = bdpi;
    let tfm_id = tfm_open(ident, 0i32);
    if tfm_id < 0i32 {
        return dpi;
    }
    let design_size = tfm_get_design_size(tfm_id);
    if design_size <= 0.0f64 {
        warn!("DESGIN_SIZE <= 0.0? (TFM=\"{}\")", ident);
    } else {
        dpi =
            ((base_dpi as f64 * point_size / design_size / 1.0f64 + 0.5f64).floor() * 1.0f64) as u32
    }
    dpi
}
unsafe fn dpx_open_pk_font_at(_ident: &str, _dpi: u32) -> Option<InFile> {
    /*kpse_glyph_file_type kpse_file_info;*/
    let fqpn = ""; /*kpse_find_glyph(ident, dpi, kpse_pk_format, &kpse_file_info);*/
    if fqpn.is_empty() {
        return None;
    }
    InFile::open(fqpn, TTInputFormat::PK, 0)
}

pub(crate) unsafe fn pdf_font_open_pkfont(font: &mut pdf_font) -> i32 {
    let point_size = pdf_font_get_param(font, 2i32);
    let encoding_id = pdf_font_get_encoding(font);
    let ident = &*font.ident;
    if ident.is_empty() || point_size <= 0.0f64 {
        return -1i32;
    }
    let dpi = truedpi(ident, point_size, base_dpi);
    if dpx_open_pk_font_at(ident, dpi).is_none() {
        return -1i32;
    }
    /* Type 3 fonts doesn't have FontName.
     * FontFamily is recommended for PDF 1.5.
     */
    font.fontname = ident.to_owned();
    if encoding_id >= 0i32 {
        pdf_encoding_used_by_type3(encoding_id);
        warn!(
            "PK font is found for font \"{}\" but non built-in encoding \"{}\" is specified.",
            ident,
            pdf_encoding_get_name(encoding_id)
        );
        warn!(">> Assuming this is for glyph name assignment.");
    }
    0i32
}
/* We are using Mask Image. Fill black is bit clear.
 * Optimizing those codes doesn't improve things.
 */
unsafe fn fill_black_run(dp: *mut u8, mut left: u32, run_count: u32) -> u32 {
    static mut mask: [u8; 8] = [127, 191, 223, 239, 247, 251, 253, 254];
    let right: u32 = left + run_count - 1;
    while left <= right {
        *dp.offset((left / 8) as isize) &= mask[(left % 8) as usize];
        left = left.wrapping_add(1)
    }
    run_count
}
/* Just skip bits. See decode_packed() */
unsafe fn fill_white_run(run_count: u32) -> u32 {
    run_count
}
unsafe fn pk_packed_num(np: *mut u32, dyn_f: i32, dp: *mut u8, pl: u32) -> u32 {
    let mut nmbr: u32 = 0_u32;
    let mut i: u32 = *np;
    if i / 2 == pl {
        warn!("EOD reached while unpacking pk_packed_num.");
        return 0_u32;
    }
    let mut nyb = if i % 2 != 0 {
        *dp.offset((i / 2) as isize) as i32 & 0xf
    } else {
        *dp.offset((i / 2) as isize) as i32 >> 4 & 0xf
    };
    i = i.wrapping_add(1);
    if nyb == 0 {
        let mut j = 0;
        loop {
            if i / 2 == pl {
                warn!("EOD reached while unpacking pk_packed_num.");
                break;
            } else {
                nyb = if i % 2 != 0 {
                    *dp.offset((i / 2) as isize) as i32 & 0xf
                } else {
                    *dp.offset((i / 2) as isize) as i32 >> 4 & 0xf
                };
                i += 1;
                j += 1;
                if !(nyb == 0i32) {
                    break;
                }
            }
        }
        nmbr = nyb as u32;
        for _ in 0..j {
            if i / 2 == pl {
                warn!("EOD reached while unpacking pk_packed_num.");
                break;
            } else {
                nyb = if i % 2 != 0 {
                    *dp.offset((i / 2) as isize) as i32 & 0xf
                } else {
                    *dp.offset((i / 2) as isize) as i32 >> 4 & 0xf
                };
                i += 1;
                nmbr = nmbr * 16 + (nyb as u32)
            }
        }
        nmbr += ((13 - dyn_f) * 16 + dyn_f - 15) as u32;
    } else if nyb <= dyn_f {
        nmbr = nyb as u32
    } else if nyb < 14 {
        if i / 2 == pl {
            warn!("EOD reached while unpacking pk_packed_num.");
            return 0;
        }
        nmbr = ((nyb - dyn_f - 1) * 16
            + (if i % 2 != 0 {
                *dp.offset((i / 2) as isize) as i32 & 0xf
            } else {
                *dp.offset((i / 2) as isize) as i32 >> 4 & 0xf
            })
            + dyn_f
            + 1) as u32;
        i += 1;
    }
    *np = i;
    nmbr
}
unsafe fn send_out(rowptr: *mut u8, rowbytes: u32, stream: &mut pdf_stream) {
    (*stream).add(rowptr as *mut libc::c_void, rowbytes as i32);
}
unsafe fn pk_decode_packed(
    stream: &mut pdf_stream,
    wd: u32,
    ht: u32,
    dyn_f: i32,
    mut run_color: i32,
    dp: *mut u8,
    pl: u32,
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
    stream: &mut pdf_stream,
    wd: u32,
    ht: u32,
    dyn_f: i32,
    run_color: i32,
    dp: *mut u8,
    pl: u32,
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
            *rowptr.offset(j.wrapping_div(8_u32) as isize) |= mask[i.wrapping_rem(8_u32) as usize];
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
unsafe fn do_preamble<R: Read>(fp: &mut R) {
    /* Check for id byte */
    if u8::get(fp) == 89 {
        /* Skip comment */
        skip_bytes(u8::get(fp) as u32, fp);
        /* Skip other header info.  It's normally used for verifying this
        is the file wethink it is */
        skip_bytes(16_u32, fp);
    } else {
        panic!("embed_pk_font: PK ID byte is incorrect.  Are you sure this is a PK file?");
    };
}
unsafe fn read_pk_char_header<R: Read>(mut h: *mut pk_header_, opcode: u8, fp: &mut R) -> i32 {
    assert!(!h.is_null());
    if opcode as i32 & 4i32 == 0i32 {
        /* short */
        (*h).pkt_len = ((opcode as i32 & 3i32) << 8i32 | u8::get(fp) as i32) as u32; /* TFM width */
        (*h).chrcode = u8::get(fp) as i32; /* horizontal escapement */
        (*h).wd = get_unsigned_triple(fp) as i32; /* extended short */
        (*h).dx = (u8::get(fp) as i32) << 16i32;
        (*h).dy = 0i32;
        (*h).bm_wd = u8::get(fp) as u32;
        (*h).bm_ht = u8::get(fp) as u32;
        (*h).bm_hoff = i8::get(fp) as i32;
        (*h).bm_voff = i8::get(fp) as i32;
        (*h).pkt_len = ((*h).pkt_len as u32).wrapping_sub(8_u32) as u32 as u32
    } else if opcode as i32 & 7i32 == 7i32 {
        /* long */
        (*h).pkt_len = get_positive_quad(fp, "PK", "pkt_len"); /* 16.16 fixed point number in pixels */
        (*h).chrcode = i32::get(fp);
        (*h).wd = i32::get(fp);
        (*h).dx = i32::get(fp);
        (*h).dy = i32::get(fp);
        (*h).bm_wd = get_positive_quad(fp, "PK", "bm_wd");
        (*h).bm_ht = get_positive_quad(fp, "PK", "bm_ht");
        (*h).bm_hoff = i32::get(fp);
        (*h).bm_voff = i32::get(fp);
        (*h).pkt_len = ((*h).pkt_len as u32).wrapping_sub(28_u32) as u32
    } else {
        (*h).pkt_len = ((opcode as i32 & 3i32) << 16i32 | u16::get(fp) as i32) as u32;
        (*h).chrcode = u8::get(fp) as i32;
        (*h).wd = get_unsigned_triple(fp) as i32;
        (*h).dx = (u16::get(fp) as i32) << 16i32;
        (*h).dy = 0i32;
        (*h).bm_wd = u16::get(fp) as u32;
        (*h).bm_ht = u16::get(fp) as u32;
        (*h).bm_hoff = i16::get(fp) as i32;
        (*h).bm_voff = i16::get(fp) as i32;
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
    pkh: *mut pk_header_,
    chrwid: f64,
    pkt_ptr: *mut u8,
    pkt_len: u32,
) -> pdf_stream {
    let llx = -(*pkh).bm_hoff;
    let lly = ((*pkh).bm_voff as u32).wrapping_sub((*pkh).bm_ht) as i32;
    let urx = (*pkh).bm_wd.wrapping_sub((*pkh).bm_hoff as u32) as i32;
    let ury = (*pkh).bm_voff;
    let mut stream = pdf_stream::new(STREAM_COMPRESS); /* charproc */
    /*
     * The following line is a "metric" for the PDF reader:
     *
     * PDF Reference Reference, 4th ed., p.385.
     *
     * The wx (first operand of d1) must be consistent with the corresponding
     * width in the font's Widths array. The format string of sprint() must be
     * consistent with write_number() in pdfobj.c.
     */
    let mut buf = Vec::new();
    pdf_sprint_number(&mut buf, chrwid);
    write!(buf, " 0 {} {} {} {} d1\n", llx, lly, urx, ury,).unwrap();
    stream.add_slice(&buf);
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
        let slice = format!(
            "q\n{} 0 0 {} {} {} cm\n",
            (*pkh).bm_wd,
            (*pkh).bm_ht,
            llx,
            lly,
        );
        stream.add_slice(slice.as_bytes());
        let slice = format!(
            "BI\n/W {}\n/H {}\n/IM true\n/BPC 1\nID ",
            (*pkh).bm_wd,
            (*pkh).bm_ht
        );
        stream.add_slice(slice.as_bytes());
        /* Add bitmap data */
        if (*pkh).dyn_f == 14i32 {
            /* bitmap */
            pk_decode_bitmap(
                &mut stream,
                (*pkh).bm_wd,
                (*pkh).bm_ht,
                (*pkh).dyn_f,
                (*pkh).run_color,
                pkt_ptr,
                pkt_len,
            );
        } else {
            pk_decode_packed(
                &mut stream,
                (*pkh).bm_wd,
                (*pkh).bm_ht,
                (*pkh).dyn_f,
                (*pkh).run_color,
                pkt_ptr,
                pkt_len,
            );
        }
        stream.add_slice(b"\nEI\nQ");
    }
    stream
}

pub(crate) unsafe fn pdf_font_load_pkfont(font: &mut pdf_font) -> i32 {
    let mut widths: [f64; 256] = [0.; 256];
    let mut charavail: [i8; 256] = [0; 256];
    /* ENABLE_GLYPHENC */
    if !pdf_font_is_in_use(font) {
        return 0i32;
    }
    let point_size = pdf_font_get_param(font, 2i32);
    let usedchars = pdf_font_get_usedchars(font);
    let encoding_id = pdf_font_get_encoding(font);
    let mut enc_vec: &mut [String] = &mut &mut [][..];
    if encoding_id >= 0 {
        enc_vec = pdf_encoding_get_encoding(encoding_id)
    };
    /* ENABLE_GLYPHENC */
    let ident = &*font.ident;
    assert!(!ident.is_empty() && !usedchars.is_null() && point_size > 0.);
    let dpi = truedpi(ident, point_size, base_dpi);
    let mut fp = dpx_open_pk_font_at(ident, dpi).unwrap_or_else(|| {
        panic!(
            "Could not find/open PK font file: {} (at {}dpi)",
            ident, dpi
        )
    });
    memset(charavail.as_mut_ptr() as *mut libc::c_void, 0i32, 256);
    let mut charprocs = pdf_dict::new();
    /* Include bitmap as 72dpi image:
     * There seems to be problems in "scaled" bitmap glyph
     * rendering in several viewers.
     */
    let pix2charu = 72.0f64 * 1000.0f64 / base_dpi as f64 / point_size; /* A command byte */
    let mut bbox = Rect::new(
        point2(core::f64::INFINITY, core::f64::INFINITY),
        point2(core::f64::NEG_INFINITY, core::f64::NEG_INFINITY),
    );
    loop {
        let opcode = u8::get(&mut fp);
        if opcode == 245 {
            break;
        }
        if opcode < 240 {
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
            let error = read_pk_char_header(&mut pkh, opcode as u8, &mut fp);
            if error != 0 {
                panic!("Error in reading PK character header.");
            } else {
                if charavail[(pkh.chrcode & 0xffi32) as usize] != 0 {
                    warn!(
                        "More than two bitmap image for single glyph?: font=\"{}\" code=0x{:02x}",
                        ident, pkh.chrcode,
                    );
                }
            }
            if *usedchars.offset((pkh.chrcode & 0xffi32) as isize) == 0 {
                skip_bytes(pkh.pkt_len, &mut fp);
            } else {
                /* Charwidth in PDF units */
                let charwidth =
                    (1000.0f64 * pkh.wd as f64 / ((1i32 << 20i32) as f64 * pix2charu) / 0.1f64
                        + 0.5f64)
                        .floor()
                        * 0.1f64;
                widths[(pkh.chrcode & 0xffi32) as usize] = charwidth;
                /* Update font BBox info */
                bbox.min.x = bbox.min.x.min(-pkh.bm_hoff as f64);
                bbox.min.y = bbox.min.y.min(pkh.bm_voff as f64 - pkh.bm_ht as f64);
                bbox.max.x = bbox.max.x.max(pkh.bm_wd as f64 - pkh.bm_hoff as f64);
                bbox.max.y = bbox.max.y.max(pkh.bm_voff as f64);
                let mut pkt_ptr = vec![0u8; pkh.pkt_len as usize];
                let bytesread = fp.read(&mut pkt_ptr).unwrap();
                if bytesread as u64 != pkh.pkt_len as u64 {
                    panic!(
                        "Only {} bytes PK packet read. (expected {} bytes)",
                        bytesread, pkh.pkt_len,
                    );
                }
                let charproc = create_pk_CharProc_stream(
                    &mut pkh,
                    charwidth,
                    pkt_ptr.as_mut_ptr(),
                    bytesread as u32,
                )
                .into_obj();
                let charname = if encoding_id >= 0i32 && !enc_vec[0].is_empty() {
                    if enc_vec[(pkh.chrcode & 0xff) as usize].is_empty() {
                        warn!(
                            "\".notdef\" glyph used in font (code=0x{:02x}): {}",
                            pkh.chrcode, ident,
                        );
                        format!("x{:02X}", pkh.chrcode)
                    } else {
                        enc_vec[(pkh.chrcode & 0xff) as usize].clone()
                    }
                } else {
                    /* ENABLE_GLYPHENC */
                    format!("x{:02X}", pkh.chrcode)
                };
                charprocs.set(charname.as_bytes(), pdf_ref_obj(charproc));
                pdf_release_obj(charproc);
            }
            charavail[(pkh.chrcode & 0xffi32) as usize] = 1_i8
        } else {
            match opcode {
                240 | 241 | 242 | 243 => {
                    let len: i32 = get_unsigned_num(&mut fp, (opcode as i32 - 240i32) as u8) as i32;
                    if len < 0i32 {
                        warn!("PK: Special with {} bytes???", len);
                    } else {
                        skip_bytes(len as u32, &mut fp);
                    }
                }
                244 => {
                    skip_bytes(4_u32, &mut fp);
                }
                247 => {
                    do_preamble(&mut fp);
                }
                246 | _ => {}
            }
        }
    }
    /* Check if we really got all glyphs needed. */
    for code in 0..256 {
        if *usedchars.offset(code as isize) as i32 != 0 && charavail[code as usize] == 0 {
            warn!(
                "Missing glyph code=0x{:02x} in PK font \"{}\".",
                code, ident,
            );
        }
    }
    /* Now actually fill fontdict. */
    let fontdict = pdf_font_get_resource(&mut *font);
    let charprocs = charprocs.into_obj();
    fontdict
        .as_dict_mut()
        .set("CharProcs", pdf_ref_obj(charprocs));
    pdf_release_obj(charprocs);
    /*
     * Resources:
     *
     *  PDF Reference 4th ed. describes it as "Optional but strongly recommended".
     *  There are no reason to put it in our case, but we will put this.
     *  We do not care about compatibility with Acrobat 2.x. (See implementation
     *  note 47, Appendix H of PDF Ref., 4th ed.).
     */
    let mut procset = pdf_dict::new();
    let mut tmp_array = vec![];
    tmp_array.push_obj("PDF");
    tmp_array.push_obj("ImageB");
    procset.set("ProcSet", tmp_array);
    fontdict.as_dict_mut().set("Resources", procset);
    /* Encoding */
    let mut tmp_array = vec![];
    let mut prev = -2i32;
    let mut firstchar = 255i32;
    let mut lastchar = 0i32;
    for code in 0..256 {
        if *usedchars.offset(code as isize) != 0 {
            if code < firstchar {
                firstchar = code
            }
            if code > lastchar {
                lastchar = code
            }
            if code != prev + 1i32 {
                tmp_array.push_obj(code as f64);
            }
            let charname_0 = if encoding_id >= 0i32 && !enc_vec[0].is_empty() {
                if enc_vec[code as usize].is_empty() {
                    format!("x{:02X}", code)
                } else {
                    enc_vec[code as usize].clone()
                }
            } else {
                /* ENABLE_GLYPHENC */
                format!("x{:02X}", code)
            };
            tmp_array.push(pdf_name::new(charname_0.as_bytes()).into_obj());
            prev = code
        }
    }
    if firstchar > lastchar {
        panic!(
            "Unexpected error: firstchar > lastchar ({} {})",
            firstchar, lastchar
        );
    }
    if encoding_id < 0 || enc_vec[0].is_empty() {
        /* ENABLE_GLYPHENC */
        let mut encoding = pdf_dict::new();
        encoding.set("Type", "Encoding");
        encoding.set("Differences", tmp_array);
        let encoding = encoding.into_obj();
        fontdict
            .as_dict_mut()
            .set("Encoding", pdf_ref_obj(encoding));
        pdf_release_obj(encoding);
    }
    /* FontBBox: Accurate value is important.
     */
    let mut tmp_array = vec![];
    tmp_array.push_obj(bbox.min.x);
    tmp_array.push_obj(bbox.min.y);
    tmp_array.push_obj(bbox.max.x);
    tmp_array.push_obj(bbox.max.y);
    fontdict.as_dict_mut().set("FontBBox", tmp_array);
    /* Widths:
     *  Indirect reference preffered. (See PDF Reference)
     */
    let mut tmp_array = vec![];
    for code in firstchar..=lastchar {
        if *usedchars.offset(code as isize) != 0 {
            tmp_array.push_obj(widths[code as usize]);
        } else {
            tmp_array.push_obj(0_f64);
        }
    }
    let tmp_array = tmp_array.into_obj();
    fontdict.as_dict_mut().set("Widths", pdf_ref_obj(tmp_array));
    pdf_release_obj(tmp_array);
    /* FontMatrix */
    let mut tmp_array = vec![];
    tmp_array.push_obj(0.001_f64 * pix2charu);
    tmp_array.push_obj(0_f64);
    tmp_array.push_obj(0_f64);
    tmp_array.push_obj(0.001_f64 * pix2charu);
    tmp_array.push_obj(0_f64);
    tmp_array.push_obj(0_f64);
    fontdict.as_dict_mut().set("FontMatrix", tmp_array);
    fontdict.as_dict_mut().set("FirstChar", firstchar as f64);
    fontdict.as_dict_mut().set("LastChar", lastchar as f64);
    0i32
}
