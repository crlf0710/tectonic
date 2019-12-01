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

use crate::DisplayExt;
use std::ffi::CStr;

use super::dpx_dvipdfmx::always_embed;
use super::dpx_numbers::tt_get_unsigned_quad;
use super::dpx_tt_post::{tt_read_post_table, tt_release_post_table};
use super::dpx_tt_table::{tt_read_head_table, tt_read_os2__table};
use crate::dpx_pdfobj::{pdf_dict, pdf_string, PushObj};

use libc::free;
use std::io::{Seek, SeekFrom};

use super::dpx_sfnt::sfnt;

static mut verbose: i32 = 0i32;

pub unsafe fn tt_aux_set_verbose(level: i32) {
    verbose = level; /* skip version tag */
}

pub unsafe fn ttc_read_offset(sfont: *mut sfnt, ttc_idx: i32) -> u32 {
    if sfont.is_null() {
        panic!("file not opened");
    }
    if (*sfont).type_0 != 1i32 << 4i32 {
        panic!("ttc_read_offset(): invalid font type");
    }
    let handle = &mut (*sfont).handle;
    handle.seek(SeekFrom::Start(4)).unwrap();
    /* version = */
    tt_get_unsigned_quad(handle);
    let num_dirs = tt_get_unsigned_quad(handle);
    if ttc_idx < 0i32 || ttc_idx as u32 > num_dirs.wrapping_sub(1_u32) {
        panic!("Invalid TTC index number");
    }
    handle
        .seek(SeekFrom::Start((12 + ttc_idx * 4) as u64))
        .unwrap();
    tt_get_unsigned_quad(handle)
}
/* flag declared in dvipdfmx.c */
/* TTC (TrueType Collection) */
/* FontDescriptor */
/* Force bold at small text sizes */

pub unsafe fn tt_get_fontdesc(
    sfont: *mut sfnt,
    embed: *mut i32,
    mut stemv: i32,
    type_0: i32,
    fontname: *const i8,
) -> Option<pdf_dict> {
    let mut flag: i32 = 1i32 << 2i32;
    if sfont.is_null() {
        panic!("font file not opened");
    }
    /* TrueType tables */
    let os2 = tt_read_os2__table(sfont);
    let head = tt_read_head_table(sfont);
    let post = tt_read_post_table(sfont);
    if post.is_null() {
        free(os2 as *mut libc::c_void);
        free(head as *mut libc::c_void);
        return None;
    }
    let mut descriptor = pdf_dict::new();
    descriptor.set("Type", "FontDescriptor");
    if *embed != 0 && !os2.is_null() {
        /*
          License:

           "Preview & Print embedding" (0x004) requires the document containing
           Preview & Print font to be opened in read-only mode. However, licensing
           information are lost when fonts are embedded in PDF document and
           the only way to make the PDF document "read-only" is to encrypt it.
           But we have no support for encryption yet. We do not embed any fonts
           with "Preview & Print embedding" setting.

           2001/11/22: Changed to allow `Preview & Print' only fonts embedding

           2006/04/19: Added support for always_embed option
        */
        if (*os2).fsType as i32 == 0i32 || (*os2).fsType as i32 & 0x8i32 != 0 {
            /* the least restrictive license granted takes precedence. */
            *embed = 1i32
        } else if (*os2).fsType as i32 & 0x4i32 != 0 {
            if verbose > 0i32 {
                warn!(
                    "Font \"{}\" permits \"Preview & Print\" embedding only **\n",
                    CStr::from_ptr(fontname).display(),
                );
            }
            *embed = 1i32
        } else if always_embed != 0 {
            if verbose > 0i32 {
                warn!(
                    "Font \"{}\" may be subject to embedding restrictions **\n",
                    CStr::from_ptr(fontname).display(),
                );
            }
            *embed = 1i32
        } else {
            if verbose > 0i32 {
                warn!(
                    "Embedding of font \"{}\" disabled due to license restrictions",
                    CStr::from_ptr(fontname).display(),
                );
            }
            *embed = 0i32
        }
    }
    if !os2.is_null() {
        descriptor.set(
            "Ascent",
            (1000_f64 * (*os2).sTypoAscender as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1.
                + 0.5)
                .floor()
                * 1.,
        );
        descriptor.set(
            "Descent",
            (1000_f64 * (*os2).sTypoDescender as i32 as f64
                / (*head).unitsPerEm as i32 as f64
                / 1.
                + 0.5)
                .floor()
                * 1.,
        );
        if stemv < 0i32 {
            /* if not given by the option '-v' */
            stemv = ((*os2).usWeightClass as i32 as f64 / 65.0f64
                * ((*os2).usWeightClass as i32 as f64 / 65.0f64)
                + 50i32 as f64) as i32
        } /* arbitrary */
        descriptor.set("StemV", stemv as f64);
        if (*os2).version as i32 == 0x2i32 {
            descriptor.set(
                "CapHeight",
                (1000_f64 * (*os2).sCapHeight as i32 as f64
                    / (*head).unitsPerEm as i32 as f64
                    / 1.
                    + 0.5)
                    .floor()
                    * 1.,
            );
            /* optional */
            descriptor.set(
                "XHeight",
                (1000_f64 * (*os2).sxHeight as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1.
                    + 0.5)
                    .floor()
                    * 1.,
            );
        } else {
            descriptor.set(
                "CapHeight",
                (1000_f64 * (*os2).sTypoAscender as i32 as f64
                    / (*head).unitsPerEm as i32 as f64
                    / 1.
                    + 0.5)
                    .floor()
                    * 1.,
            );
        }
        /* optional */
        if (*os2).xAvgCharWidth as i32 != 0i32 {
            descriptor.set(
                "AvgWidth",
                (1000_f64 * (*os2).xAvgCharWidth as i32 as f64
                    / (*head).unitsPerEm as i32 as f64
                    / 1.
                    + 0.5)
                    .floor()
                    * 1.,
            );
        }
    }
    /* BoundingBox (array) */
    let mut bbox = vec![];
    bbox.push_obj(
        (1000_f64 * (*head).xMin as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1. + 0.5)
            .floor()
            * 1.,
    );
    bbox.push_obj(
        (1000_f64 * (*head).yMin as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1. + 0.5)
            .floor()
            * 1.,
    );
    bbox.push_obj(
        (1000_f64 * (*head).xMax as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1. + 0.5)
            .floor()
            * 1.,
    );
    bbox.push_obj(
        (1000_f64 * (*head).yMax as i32 as f64 / (*head).unitsPerEm as i32 as f64 / 1. + 0.5)
            .floor()
            * 1.,
    );
    descriptor.set("FontBBox", bbox);
    /* post */
    descriptor.set(
        "ItalicAngle",
        ((*post).italicAngle as i64 % 0x10000) as f64 / 0x10000 as f64
            + ((*post).italicAngle as i64 / 0x10000) as f64
            - (if (*post).italicAngle as i64 / 0x10000 > 0x7fff {
                0x10000
            } else {
                0i64
            }) as f64,
    );
    /* Flags */
    if !os2.is_null() {
        if (*os2).fsSelection as i32 & 1i32 << 0i32 != 0 {
            flag |= 1i32 << 6i32
        }
        if (*os2).fsSelection as i32 & 1i32 << 5i32 != 0 {
            flag |= 1i32 << 18i32
        }
        if (*os2).sFamilyClass as i32 >> 8i32 & 0xffi32 != 8i32 {
            flag |= 1i32 << 1i32
        }
        if (*os2).sFamilyClass as i32 >> 8i32 & 0xffi32 == 10i32 {
            flag |= 1i32 << 3i32
        }
        if (*post).isFixedPitch != 0 {
            flag |= 1i32 << 0i32
        }
    }
    descriptor.set("Flags", flag as f64);
    /* insert panose if you want */
    if type_0 == 0i32 && !os2.is_null() {
        /* cid-keyed font - add panose */
        let mut panose: [u8; 12] = [0; 12];
        panose[0..2].copy_from_slice(&(*os2).sFamilyClass.to_be_bytes());
        panose[2..12].copy_from_slice((*os2).panose.as_ref());
        let mut styledict = pdf_dict::new();
        styledict.set("Panose", pdf_string::new(panose));
        descriptor.set("Style", styledict);
    }
    free(head as *mut libc::c_void);
    free(os2 as *mut libc::c_void);
    tt_release_post_table(post);
    Some(descriptor)
}
