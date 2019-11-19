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

use super::dpx_numbers::{
    tt_get_positive_quad, tt_get_signed_quad, tt_get_unsigned_byte, tt_get_unsigned_pair,
    tt_get_unsigned_quad,
};
use crate::mfree;
use crate::streq_ptr;
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::{CString, CStr};

use super::dpx_mem::{new, renew};
use super::dpx_numbers::tt_skip_bytes;
use crate::{ttstub_input_close, ttstub_input_get_size, ttstub_input_open};
use libc::{free, strcat, strcmp, strcpy, strlen, strrchr};

use std::io::{Seek, SeekFrom};

pub type __off_t = i64;
pub type __ssize_t = i64;
pub type off_t = __off_t;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
pub type fixword = i32;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct font_metric {
    pub tex_name: *mut i8,
    pub designsize: fixword,
    pub codingscheme: *mut i8,
    pub fontdir: i32,
    pub firstchar: i32,
    pub lastchar: i32,
    pub widths: *mut fixword,
    pub heights: *mut fixword,
    pub depths: *mut fixword,
    pub charmap: C2RustUnnamed,
    pub source: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub type_0: i32,
    pub data: *mut libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct coverage {
    pub first_char: i32,
    pub num_chars: i32,
}
/* quasi-hack to get the primary input */
/*
 * TFM Record structure:
 * Multiple TFM's may be read in at once.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tfm_font {
    pub level: i32,
    pub wlenfile: u32,
    pub wlenheader: u32,
    pub bc: u32,
    pub ec: u32,
    pub nwidths: u32,
    pub nheights: u32,
    pub ndepths: u32,
    pub nitcor: u32,
    pub nlig: u32,
    pub nkern: u32,
    pub nextens: u32,
    pub nfonparm: u32,
    pub fontdir: u32,
    pub nco: u32,
    pub ncw: u32,
    pub npc: u32,
    pub header: *mut fixword,
    pub char_info: *mut u32,
    pub width_index: *mut u16,
    pub height_index: *mut u8,
    pub depth_index: *mut u8,
    pub width: *mut fixword,
    pub height: *mut fixword,
    pub depth: *mut fixword,
}
/*
 * All characters in the same range have same metrics.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct range_map {
    pub num_coverages: u16,
    pub coverages: *mut coverage,
    pub indices: *mut u16,
}
/* Special case of num_coverages = 1 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct char_map {
    pub coverage: coverage,
    pub indices: *mut u16,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0i32;
unsafe fn tfm_font_init(mut tfm: *mut tfm_font) {
    (*tfm).header = 0 as *mut fixword;
    (*tfm).level = 0i32;
    (*tfm).fontdir = 0_u32;
    (*tfm).npc = 0_u32;
    (*tfm).ncw = (*tfm).npc;
    (*tfm).nco = (*tfm).ncw;
    (*tfm).char_info = 0 as *mut u32;
    (*tfm).width_index = 0 as *mut u16;
    (*tfm).height_index = 0 as *mut u8;
    (*tfm).depth_index = 0 as *mut u8;
    (*tfm).depth = 0 as *mut fixword;
    (*tfm).height = (*tfm).depth;
    (*tfm).width = (*tfm).height;
}
unsafe fn tfm_font_clear(mut tfm: *mut tfm_font) {
    if !tfm.is_null() {
        (*tfm).header = mfree((*tfm).header as *mut libc::c_void) as *mut fixword;
        (*tfm).char_info = mfree((*tfm).char_info as *mut libc::c_void) as *mut u32;
        (*tfm).width = mfree((*tfm).width as *mut libc::c_void) as *mut fixword;
        (*tfm).height = mfree((*tfm).height as *mut libc::c_void) as *mut fixword;
        (*tfm).depth = mfree((*tfm).depth as *mut libc::c_void) as *mut fixword;
        (*tfm).width_index = mfree((*tfm).width_index as *mut libc::c_void) as *mut u16;
        (*tfm).height_index = mfree((*tfm).height_index as *mut libc::c_void) as *mut u8;
        (*tfm).depth_index = mfree((*tfm).depth_index as *mut libc::c_void) as *mut u8
    };
}
unsafe fn release_char_map(mut map: *mut char_map) {
    (*map).indices = mfree((*map).indices as *mut libc::c_void) as *mut u16;
    free(map as *mut libc::c_void);
}
unsafe fn release_range_map(mut map: *mut range_map) {
    free((*map).coverages as *mut libc::c_void);
    free((*map).indices as *mut libc::c_void);
    (*map).coverages = 0 as *mut coverage;
    (*map).indices = 0 as *mut u16;
    free(map as *mut libc::c_void);
}
unsafe fn lookup_char(mut map: *const char_map, mut charcode: i32) -> i32 {
    if charcode >= (*map).coverage.first_char
        && charcode <= (*map).coverage.first_char + (*map).coverage.num_chars
    {
        return *(*map)
            .indices
            .offset((charcode - (*map).coverage.first_char) as isize) as i32;
    } else {
        return -1i32;
    };
}
unsafe fn lookup_range(mut map: *const range_map, mut charcode: i32) -> i32 {
    let mut idx = (*map).num_coverages as i32 - 1i32;
    while idx >= 0i32 && charcode >= (*(*map).coverages.offset(idx as isize)).first_char {
        if charcode
            <= (*(*map).coverages.offset(idx as isize)).first_char
                + (*(*map).coverages.offset(idx as isize)).num_chars
        {
            return *(*map).indices.offset(idx as isize) as i32;
        }
        idx -= 1
    }
    -1i32
}
unsafe fn fm_init(mut fm: *mut font_metric) {
    (*fm).tex_name = 0 as *mut i8;
    (*fm).firstchar = 0i32;
    (*fm).lastchar = 0i32;
    (*fm).fontdir = 0i32;
    (*fm).codingscheme = 0 as *mut i8;
    (*fm).designsize = 0i32;
    (*fm).widths = 0 as *mut fixword;
    (*fm).heights = 0 as *mut fixword;
    (*fm).depths = 0 as *mut fixword;
    (*fm).charmap.type_0 = 0i32;
    (*fm).charmap.data = 0 as *mut libc::c_void;
    (*fm).source = 0i32;
}
unsafe fn fm_clear(mut fm: *mut font_metric) {
    if !fm.is_null() {
        free((*fm).tex_name as *mut libc::c_void);
        free((*fm).widths as *mut libc::c_void);
        free((*fm).heights as *mut libc::c_void);
        free((*fm).depths as *mut libc::c_void);
        free((*fm).codingscheme as *mut libc::c_void);
        match (*fm).charmap.type_0 {
            1 => {
                release_char_map((*fm).charmap.data as *mut char_map);
            }
            2 => {
                release_range_map((*fm).charmap.data as *mut range_map);
            }
            _ => {}
        }
    };
}
static mut fms: *mut font_metric = std::ptr::null_mut();
static mut numfms: u32 = 0_u32;
static mut max_fms: u32 = 0_u32;
#[no_mangle]
pub unsafe extern "C" fn tfm_reset_global_state() {
    fms = 0 as *mut font_metric;
    numfms = 0_u32;
    max_fms = 0_u32;
}
unsafe fn fms_need(mut n: u32) {
    if n > max_fms {
        max_fms = if max_fms.wrapping_add(16_u32) > n {
            max_fms.wrapping_add(16_u32)
        } else {
            n
        };
        fms = renew(
            fms as *mut libc::c_void,
            (max_fms as u64).wrapping_mul(::std::mem::size_of::<font_metric>() as u64) as u32,
        ) as *mut font_metric
    };
}
#[no_mangle]
pub unsafe extern "C" fn tfm_set_verbose(mut level: i32) {
    verbose = level;
}
unsafe fn fread_fwords(
    mut words: *mut fixword,
    mut nmemb: u32,
    handle: &mut InputHandleWrapper,
) -> i32 {
    for i in 0..nmemb {
        *words.offset(i as isize) = tt_get_signed_quad(handle);
    }
    nmemb.wrapping_mul(4_u32) as i32
}
unsafe fn fread_uquads(
    mut quads: *mut u32,
    mut nmemb: u32,
    handle: &mut InputHandleWrapper,
) -> i32 {
    for i in 0..nmemb {
        *quads.offset(i as isize) = tt_get_unsigned_quad(handle);
    }
    nmemb.wrapping_mul(4_u32) as i32
}
/*
 * TFM and JFM
 */
unsafe fn tfm_check_size(mut tfm: *mut tfm_font, mut tfm_file_size: off_t) {
    let mut expected_size: u32 = 6_u32;
    /* Removed the warning message caused by EC TFM metric files.
     *
     if (tfm->wlenfile != tfm_file_size / 4) {
     warn!("TFM file size is {} bytes but it says it is {} bytes!",
     tfm_file_size, tfm->wlenfile * 4);
     if (tfm_file_size > tfm->wlenfile * 4) {
     warn!("Proceeding nervously...");
     } else {
     panic!("Can't proceed...");
     }
     }
    */
    if tfm_file_size < (*tfm).wlenfile as i64 * 4i32 as i64 {
        panic!("Can\'t proceed...");
    }
    expected_size = (expected_size as u32)
        .wrapping_add((*tfm).ec.wrapping_sub((*tfm).bc).wrapping_add(1_u32))
        as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).wlenheader) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nwidths) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nheights) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).ndepths) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nitcor) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nlig) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nkern) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nextens) as u32;
    expected_size = (expected_size as u32).wrapping_add((*tfm).nfonparm) as u32;
    if expected_size != (*tfm).wlenfile {
        warn!(
            "TFM file size is expected to be {} bytes but it says it is {}bytes!",
            expected_size as i64 * 4i32 as i64,
            (*tfm).wlenfile as i64 * 4i32 as i64,
        );
        if tfm_file_size > expected_size as i64 * 4i32 as i64 {
            warn!("Proceeding nervously...");
        } else {
            panic!("Can\'t proceed...");
        }
    };
}
unsafe fn tfm_get_sizes(
    tfm_handle: &mut InputHandleWrapper,
    mut tfm_file_size: off_t,
    mut tfm: *mut tfm_font,
) {
    (*tfm).wlenfile = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).wlenheader = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).bc = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).ec = tt_get_unsigned_pair(tfm_handle) as u32;
    if (*tfm).ec < (*tfm).bc {
        panic!("TFM file error: ec({}) < bc({}) ???", (*tfm).ec, (*tfm).bc);
    }
    (*tfm).nwidths = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nheights = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).ndepths = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nitcor = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nlig = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nkern = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nextens = tt_get_unsigned_pair(tfm_handle) as u32;
    (*tfm).nfonparm = tt_get_unsigned_pair(tfm_handle) as u32;
    tfm_check_size(tfm, tfm_file_size);
}
unsafe fn tfm_unpack_arrays(mut fm: *mut font_metric, mut tfm: *mut tfm_font) {
    (*fm).widths =
        new((256_u32 as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
            as *mut fixword;
    (*fm).heights =
        new((256_u32 as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
            as *mut fixword;
    (*fm).depths =
        new((256_u32 as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
            as *mut fixword;
    for i in 0..256 {
        *(*fm).widths.offset(i as isize) = 0i32;
        *(*fm).heights.offset(i as isize) = 0i32;
        *(*fm).depths.offset(i as isize) = 0i32;
    }
    for i in (*tfm).bc..=(*tfm).ec {
        let charinfo = *(*tfm).char_info.offset(i.wrapping_sub((*tfm).bc) as isize);
        let width_index = (charinfo >> 24i32) as u16;
        let height_index = (charinfo >> 20i32 & 0xf_u32) as u8;
        let depth_index = (charinfo >> 16i32 & 0xf_u32) as u8;
        *(*fm).widths.offset(i as isize) = *(*tfm).width.offset(width_index as isize);
        *(*fm).heights.offset(i as isize) = *(*tfm).height.offset(height_index as isize);
        *(*fm).depths.offset(i as isize) = *(*tfm).depth.offset(depth_index as isize);
    }
}
unsafe fn sput_bigendian(mut s: *mut i8, mut v: i32, mut n: i32) -> i32 {
    let mut i = n - 1i32;
    while i >= 0i32 {
        *s.offset(i as isize) = (v & 0xffi32) as i8;
        v >>= 8i32;
        i -= 1
    }
    n
}
unsafe fn tfm_unpack_header(mut fm: *mut font_metric, mut tfm: *mut tfm_font) {
    if (*tfm).wlenheader < 12_u32 {
        (*fm).codingscheme = 0 as *mut i8
    } else {
        let len = *(*tfm).header.offset(2) >> 24i32;
        if len < 0i32 || len > 39i32 {
            panic!("Invalid TFM header.");
        }
        if len > 0i32 {
            (*fm).codingscheme = new((40_u32 as u64)
                .wrapping_mul(::std::mem::size_of::<i8>() as u64)
                as u32) as *mut i8;
            let mut p = (*fm).codingscheme;
            p = p.offset(sput_bigendian(p, *(*tfm).header.offset(2), 3i32) as isize);
            for i in 1..=(len / 4) {
                p = p.offset(
                    sput_bigendian(p, *(*tfm).header.offset((2i32 + i) as isize), 4i32) as isize,
                );
            }
            *(*fm).codingscheme.offset(len as isize) = '\u{0}' as i32 as i8
        } else {
            (*fm).codingscheme = 0 as *mut i8
        }
    }
    (*fm).designsize = *(*tfm).header.offset(1);
}
unsafe fn ofm_check_size_one(mut tfm: *mut tfm_font, mut ofm_file_size: off_t) {
    let mut ofm_size: u32 = 14_u32;
    ofm_size = (ofm_size as u32)
        .wrapping_add((2_u32).wrapping_mul((*tfm).ec.wrapping_sub((*tfm).bc).wrapping_add(1_u32)))
        as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).wlenheader) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).nwidths) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).nheights) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).ndepths) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).nitcor) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((2_u32).wrapping_mul((*tfm).nlig)) as u32 as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).nkern) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((2_u32).wrapping_mul((*tfm).nextens)) as u32;
    ofm_size = (ofm_size as u32).wrapping_add((*tfm).nfonparm) as u32;
    if (*tfm).wlenfile as i64 != ofm_file_size / 4i32 as i64 || (*tfm).wlenfile != ofm_size {
        panic!("OFM file problem.  Table sizes don\'t agree.");
    };
}
unsafe fn ofm_get_sizes(
    ofm_handle: &mut InputHandleWrapper,
    mut ofm_file_size: off_t,
    mut tfm: *mut tfm_font,
) {
    (*tfm).level = tt_get_signed_quad(ofm_handle);
    (*tfm).wlenfile = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "wlenfile",
    );
    (*tfm).wlenheader = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "wlenheader",
    );
    (*tfm).bc = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "bc",
    );
    (*tfm).ec = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "ec",
    );
    if (*tfm).ec < (*tfm).bc {
        panic!("OFM file error: ec({}) < bc({}) ???", (*tfm).ec, (*tfm).bc);
    }
    (*tfm).nwidths = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nwidths",
    );
    (*tfm).nheights = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nheights",
    );
    (*tfm).ndepths = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "ndepths",
    );
    (*tfm).nitcor = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nitcor",
    );
    (*tfm).nlig = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nlig",
    );
    (*tfm).nkern = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nkern",
    );
    (*tfm).nextens = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nextens",
    );
    (*tfm).nfonparm = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "nfonparm",
    );
    (*tfm).fontdir = tt_get_positive_quad(
        ofm_handle,
        "OFM",
        "fontdir",
    );
    if (*tfm).fontdir != 0 {
        warn!("I may be interpreting a font direction incorrectly.");
    }
    if (*tfm).level == 0i32 {
        ofm_check_size_one(tfm, ofm_file_size);
    } else if (*tfm).level == 1i32 {
        (*tfm).nco = tt_get_positive_quad(
            ofm_handle,
            "OFM",
            "nco",
        );
        (*tfm).ncw = tt_get_positive_quad(
            ofm_handle,
            "OFM",
            "nco",
        );
        (*tfm).npc = tt_get_positive_quad(
            ofm_handle,
            "OFM",
            "npc",
        );
        ofm_handle.seek(SeekFrom::Start(4 * ((*tfm).nco - (*tfm).wlenheader) as u64)).unwrap();
    } else {
        panic!("can\'t handle OFM files with level > 1");
    };
}
unsafe fn ofm_do_char_info_zero(ofm_handle: &mut InputHandleWrapper, mut tfm: *mut tfm_font) {
    let mut num_chars = (*tfm).ec.wrapping_sub((*tfm).bc).wrapping_add(1_u32);
    if num_chars != 0_u32 {
        (*tfm).width_index = new((num_chars as u64)
            .wrapping_mul(::std::mem::size_of::<u16>() as u64)
            as u32) as *mut u16;
        (*tfm).height_index = new((num_chars as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64)
            as u32) as *mut u8;
        (*tfm).depth_index =
            new((num_chars as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
                as *mut u8;
        for i in 0..num_chars {
            *(*tfm).width_index.offset(i as isize) = tt_get_unsigned_pair(ofm_handle);
            *(*tfm).height_index.offset(i as isize) = tt_get_unsigned_byte(ofm_handle);
            *(*tfm).depth_index.offset(i as isize) = tt_get_unsigned_byte(ofm_handle);
            /* Ignore remaining quad */
            tt_skip_bytes(4_u32, ofm_handle);
        }
    };
}
unsafe fn ofm_do_char_info_one(ofm_handle: &mut InputHandleWrapper, mut tfm: *mut tfm_font) {
    let num_char_infos = (*tfm)
        .ncw
        .wrapping_div((3_u32).wrapping_add((*tfm).npc.wrapping_div(2_u32)));
    let mut num_chars = (*tfm).ec.wrapping_sub((*tfm).bc).wrapping_add(1_u32);
    if num_chars != 0_u32 {
        (*tfm).width_index = new((num_chars as u64)
            .wrapping_mul(::std::mem::size_of::<u16>() as u64)
            as u32) as *mut u16;
        (*tfm).height_index = new((num_chars as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64)
            as u32) as *mut u8;
        (*tfm).depth_index =
            new((num_chars as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
                as *mut u8;
        let mut char_infos_read = 0;
        let mut i = 0;
        while i < num_chars && char_infos_read < num_char_infos {
            *(*tfm).width_index.offset(i as isize) = tt_get_unsigned_pair(ofm_handle);
            *(*tfm).height_index.offset(i as isize) = tt_get_unsigned_byte(ofm_handle);
            *(*tfm).depth_index.offset(i as isize) = tt_get_unsigned_byte(ofm_handle);
            /* Ignore next quad */
            tt_skip_bytes(4_u32, ofm_handle);
            let repeats = tt_get_unsigned_pair(ofm_handle) as u32;
            /* Skip params */
            for _ in 0..(*tfm).npc {
                tt_get_unsigned_pair(ofm_handle);
            }
            /* Remove word padding if necessary */
            if (*tfm).npc.wrapping_div(2_u32).wrapping_mul(2_u32) == (*tfm).npc {
                tt_get_unsigned_pair(ofm_handle);
            }
            char_infos_read = char_infos_read.wrapping_add(1);
            if i + repeats > num_chars {
                panic!("OFM \"repeats\" causes number of characters to be exceeded.");
            }
            for j in 0..repeats {
                *(*tfm)
                    .width_index
                    .offset(i.wrapping_add(j).wrapping_add(1_u32) as isize) =
                    *(*tfm).width_index.offset(i as isize);
                *(*tfm)
                    .height_index
                    .offset(i.wrapping_add(j).wrapping_add(1_u32) as isize) =
                    *(*tfm).height_index.offset(i as isize);
                *(*tfm)
                    .depth_index
                    .offset(i.wrapping_add(j).wrapping_add(1_u32) as isize) =
                    *(*tfm).depth_index.offset(i as isize);
            }
            /* Skip ahead because we have already handled repeats */
            i += repeats;
            i += 1;
        }
    };
}
unsafe fn ofm_unpack_arrays(mut fm: *mut font_metric, mut tfm: *mut tfm_font, mut num_chars: u32) {
    (*fm).widths = new(((*tfm).bc.wrapping_add(num_chars) as u64)
        .wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
        as *mut fixword;
    (*fm).heights = new(((*tfm).bc.wrapping_add(num_chars) as u64)
        .wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
        as *mut fixword;
    (*fm).depths = new(((*tfm).bc.wrapping_add(num_chars) as u64)
        .wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
        as *mut fixword;
    for i in 0..num_chars {
        *(*fm).widths.offset((*tfm).bc.wrapping_add(i) as isize) = *(*tfm)
            .width
            .offset(*(*tfm).width_index.offset(i as isize) as isize);
        *(*fm).heights.offset((*tfm).bc.wrapping_add(i) as isize) = *(*tfm)
            .height
            .offset(*(*tfm).height_index.offset(i as isize) as isize);
        *(*fm).depths.offset((*tfm).bc.wrapping_add(i) as isize) = *(*tfm)
            .depth
            .offset(*(*tfm).depth_index.offset(i as isize) as isize);
    }
}
unsafe fn read_ofm(
    mut fm: *mut font_metric,
    ofm_handle: &mut InputHandleWrapper,
    mut ofm_file_size: off_t,
) {
    let mut tfm: tfm_font = tfm_font {
        level: 0,
        wlenfile: 0,
        wlenheader: 0,
        bc: 0,
        ec: 0,
        nwidths: 0,
        nheights: 0,
        ndepths: 0,
        nitcor: 0,
        nlig: 0,
        nkern: 0,
        nextens: 0,
        nfonparm: 0,
        fontdir: 0,
        nco: 0,
        ncw: 0,
        npc: 0,
        header: 0 as *mut fixword,
        char_info: 0 as *mut u32,
        width_index: 0 as *mut u16,
        height_index: 0 as *mut u8,
        depth_index: 0 as *mut u8,
        width: 0 as *mut fixword,
        height: 0 as *mut fixword,
        depth: 0 as *mut fixword,
    };
    tfm_font_init(&mut tfm);
    ofm_get_sizes(ofm_handle, ofm_file_size, &mut tfm);
    if tfm.level < 0i32 || tfm.level > 1i32 {
        panic!("OFM level {} not supported.", tfm.level);
    }
    if tfm.wlenheader > 0_u32 {
        tfm.header = new(
            (tfm.wlenheader as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32
        ) as *mut fixword;
        fread_fwords(tfm.header, tfm.wlenheader, ofm_handle);
    }
    if tfm.level == 0i32 {
        ofm_do_char_info_zero(ofm_handle, &mut tfm);
    } else if tfm.level == 1i32 {
        ofm_do_char_info_one(ofm_handle, &mut tfm);
    }
    if tfm.nwidths > 0_u32 {
        tfm.width =
            new((tfm.nwidths as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.width, tfm.nwidths, ofm_handle);
    }
    if tfm.nheights > 0_u32 {
        tfm.height =
            new((tfm.nheights as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.height, tfm.nheights, ofm_handle);
    }
    if tfm.ndepths > 0_u32 {
        tfm.depth =
            new((tfm.ndepths as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.depth, tfm.ndepths, ofm_handle);
    }
    ofm_unpack_arrays(
        fm,
        &mut tfm,
        tfm.ec.wrapping_sub(tfm.bc).wrapping_add(1_u32),
    );
    tfm_unpack_header(fm, &mut tfm);
    (*fm).firstchar = tfm.bc as i32;
    (*fm).lastchar = tfm.ec as i32;
    (*fm).source = 2i32;
    tfm_font_clear(&mut tfm);
}
unsafe fn read_tfm(
    mut fm: *mut font_metric,
    tfm_handle: &mut InputHandleWrapper,
    mut tfm_file_size: off_t,
) {
    let mut tfm: tfm_font = tfm_font {
        level: 0,
        wlenfile: 0,
        wlenheader: 0,
        bc: 0,
        ec: 0,
        nwidths: 0,
        nheights: 0,
        ndepths: 0,
        nitcor: 0,
        nlig: 0,
        nkern: 0,
        nextens: 0,
        nfonparm: 0,
        fontdir: 0,
        nco: 0,
        ncw: 0,
        npc: 0,
        header: 0 as *mut fixword,
        char_info: 0 as *mut u32,
        width_index: 0 as *mut u16,
        height_index: 0 as *mut u8,
        depth_index: 0 as *mut u8,
        width: 0 as *mut fixword,
        height: 0 as *mut fixword,
        depth: 0 as *mut fixword,
    };
    tfm_font_init(&mut tfm);
    tfm_get_sizes(tfm_handle, tfm_file_size, &mut tfm);
    (*fm).firstchar = tfm.bc as i32;
    (*fm).lastchar = tfm.ec as i32;
    if tfm.wlenheader > 0_u32 {
        tfm.header = new(
            (tfm.wlenheader as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32
        ) as *mut fixword;
        fread_fwords(tfm.header, tfm.wlenheader, tfm_handle);
    }
    if tfm.ec.wrapping_sub(tfm.bc).wrapping_add(1_u32) > 0_u32 {
        tfm.char_info = new((tfm.ec.wrapping_sub(tfm.bc).wrapping_add(1_u32) as u64)
            .wrapping_mul(::std::mem::size_of::<u32>() as u64) as u32)
            as *mut u32;
        fread_uquads(
            tfm.char_info,
            tfm.ec.wrapping_sub(tfm.bc).wrapping_add(1_u32),
            tfm_handle,
        );
    }
    if tfm.nwidths > 0_u32 {
        tfm.width =
            new((tfm.nwidths as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.width, tfm.nwidths, tfm_handle);
    }
    if tfm.nheights > 0_u32 {
        tfm.height =
            new((tfm.nheights as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.height, tfm.nheights, tfm_handle);
    }
    if tfm.ndepths > 0_u32 {
        tfm.depth =
            new((tfm.ndepths as u64).wrapping_mul(::std::mem::size_of::<fixword>() as u64) as u32)
                as *mut fixword;
        fread_fwords(tfm.depth, tfm.ndepths, tfm_handle);
    }
    tfm_unpack_arrays(fm, &mut tfm);
    tfm_unpack_header(fm, &mut tfm);
    tfm_font_clear(&mut tfm);
}
#[no_mangle]
pub unsafe extern "C" fn tfm_open(mut tfm_name: *const i8, mut must_exist: i32) -> i32 {
    let mut tfm_handle = None;
    let mut format: i32 = 1i32;
    let ofm_name;
    for i in 0..numfms {
        if streq_ptr(tfm_name, (*fms.offset(i as isize)).tex_name) {
            return i as i32;
        }
    }
    /* NOTE: the following comment is no longer operative with the switch to
     * the Tectonic I/O system since we don't have `must_exist`. The logic
     * of the current implementation might not be right; to be investigated.
     * Comment preserved for posterity.
     *
     * "The procedure to search tfm or ofm files:
     * 1. Search tfm file with the given name with the must_exist flag unset.
     * 2. Search ofm file with the given name with the must_exist flag unset.
     * 3. If not found and must_exist flag is set, try again to search
     *    tfm file with the must_exist flag set.
     * 4. If not found and must_exist flag is not set, return -1.
     *
     * We first look for OFM and then TFM.
     * The reason for this change is incompatibility introduced when dvipdfmx
     * started to write correct glyph metrics to output PDF for CID fonts.
     * I'll not explain this in detail... This change is mostly specific to
     * Japanese support."
     */
    let suffix = strrchr(tfm_name, '.' as i32);
    if suffix.is_null()
        || strcmp(suffix, b".tfm\x00" as *const u8 as *const i8) != 0i32
            && strcmp(suffix, b".ofm\x00" as *const u8 as *const i8) != 0i32
    {
        ofm_name = new((strlen(tfm_name)
            .wrapping_add(strlen(b".ofm\x00" as *const u8 as *const i8))
            .wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
        strcpy(ofm_name, tfm_name);
        strcat(ofm_name, b".ofm\x00" as *const u8 as *const i8);
    } else {
        ofm_name = 0 as *mut i8
    }
    if !ofm_name.is_null() && {
        tfm_handle = ttstub_input_open(ofm_name, TTInputFormat::OFM, 0i32);
        tfm_handle.is_some()
    } {
        format = 2i32
    } else {
        tfm_handle = ttstub_input_open(tfm_name, TTInputFormat::TFM, 0i32);
        if tfm_handle.is_some() {
            format = 1i32
        } else {
            tfm_handle = ttstub_input_open(tfm_name, TTInputFormat::OFM, 0i32);
            if tfm_handle.is_some() {
                format = 2i32
            }
        }
    }
    free(ofm_name as *mut libc::c_void);
    if tfm_handle.is_none() {
        if must_exist != 0 {
            panic!(
                "Unable to find TFM file \"{}\".",
                CStr::from_ptr(tfm_name).display()
            );
        }
        return -1i32;
    }
    let mut tfm_handle = tfm_handle.unwrap();
    if verbose != 0 {
        if format == 1i32 {
            info!("(TFM:{}", CStr::from_ptr(tfm_name).display());
        } else if format == 2i32 {
            info!("(OFM:{}", CStr::from_ptr(tfm_name).display());
        }
    }
    let tfm_file_size = ttstub_input_get_size(&mut tfm_handle) as off_t;
    if tfm_file_size as u64 > 0x1ffffffffu64 {
        panic!("TFM/OFM file size exceeds 33-bit");
    }
    if tfm_file_size < 24i32 as i64 {
        panic!("TFM/OFM file too small to be a valid file.");
    }
    fms_need(numfms.wrapping_add(1_u32));
    fm_init(fms.offset(numfms as isize));
    if format == 2i32 {
        read_ofm(&mut *fms.offset(numfms as isize), &mut tfm_handle, tfm_file_size);
    } else {
        read_tfm(&mut *fms.offset(numfms as isize), &mut tfm_handle, tfm_file_size);
    }
    ttstub_input_close(tfm_handle);
    let ref mut fresh0 = (*fms.offset(numfms as isize)).tex_name;
    *fresh0 = new((strlen(tfm_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
        as *mut i8;
    strcpy((*fms.offset(numfms as isize)).tex_name, tfm_name);
    if verbose != 0 {
        info!(")");
    }
    let fresh1 = numfms;
    numfms = numfms.wrapping_add(1);
    fresh1 as i32
}
#[no_mangle]
pub unsafe extern "C" fn tfm_close_all() {
    if !fms.is_null() {
        for i in 0..numfms {
            fm_clear(&mut *fms.offset(i as isize));
        }
        free(fms as *mut libc::c_void);
    };
}
#[no_mangle]
pub unsafe extern "C" fn tfm_get_fw_width(mut font_id: i32, mut ch: i32) -> fixword {
    let idx;
    if font_id < 0i32 || font_id as u32 >= numfms {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut *fms.offset(font_id as isize) as *mut font_metric;
    if ch >= (*fm).firstchar && ch <= (*fm).lastchar {
        match (*fm).charmap.type_0 {
            1 => {
                idx = lookup_char((*fm).charmap.data as *const char_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            2 => {
                idx = lookup_range((*fm).charmap.data as *const range_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            _ => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    *(*fm).widths.offset(idx as isize)
}
#[no_mangle]
pub unsafe extern "C" fn tfm_get_fw_height(mut font_id: i32, mut ch: i32) -> fixword {
    let idx;
    if font_id < 0i32 || font_id as u32 >= numfms {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut *fms.offset(font_id as isize) as *mut font_metric;
    if ch >= (*fm).firstchar && ch <= (*fm).lastchar {
        match (*fm).charmap.type_0 {
            1 => {
                idx = lookup_char((*fm).charmap.data as *const char_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            2 => {
                idx = lookup_range((*fm).charmap.data as *const range_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            _ => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    *(*fm).heights.offset(idx as isize)
}
#[no_mangle]
pub unsafe extern "C" fn tfm_get_fw_depth(mut font_id: i32, mut ch: i32) -> fixword {
    let idx;
    if font_id < 0i32 || font_id as u32 >= numfms {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut *fms.offset(font_id as isize) as *mut font_metric;
    if ch >= (*fm).firstchar && ch <= (*fm).lastchar {
        match (*fm).charmap.type_0 {
            1 => {
                idx = lookup_char((*fm).charmap.data as *const char_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            2 => {
                idx = lookup_range((*fm).charmap.data as *const range_map, ch);
                if idx < 0i32 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            _ => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    *(*fm).depths.offset(idx as isize)
}
/*
 * tfm_get_width returns the width of the font
 * as a (double) fraction of the design size.
 */
#[no_mangle]
pub unsafe extern "C" fn tfm_get_width(mut font_id: i32, mut ch: i32) -> f64 {
    tfm_get_fw_width(font_id, ch) as f64 / (1i32 << 20i32) as f64
}
/* tfm_string_xxx() do not work for OFM... */
#[no_mangle]
pub unsafe extern "C" fn tfm_string_width(
    mut font_id: i32,
    mut s: *const u8,
    mut len: u32,
) -> fixword {
    let mut result: fixword = 0i32;
    if font_id < 0i32 || font_id as u32 >= numfms {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    for i in 0..len {
        result += tfm_get_fw_width(font_id, *s.offset(i as isize) as i32);
    }
    result
}
#[no_mangle]
pub unsafe extern "C" fn tfm_get_design_size(mut font_id: i32) -> f64 {
    if font_id < 0i32 || font_id as u32 >= numfms {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    return (*fms.offset(font_id as isize)).designsize as f64 / (1i32 << 20i32) as f64
        * (72.0f64 / 72.27f64);
}
/* From TFM header */
#[no_mangle]
pub unsafe extern "C" fn tfm_exists(tfm_name: &[u8]) -> bool {
    if tfm_name.is_empty() {
        return false;
    }
    let tfm_name = CString::new(tfm_name).unwrap();
    let handle = ttstub_input_open(tfm_name.as_ptr(), TTInputFormat::OFM, 0);
    if let Some(handle) = handle {
        ttstub_input_close(handle);
        return true;
    }
    let handle = ttstub_input_open(tfm_name.as_ptr(), TTInputFormat::TFM, 0);
    if let Some(handle) = handle {
        ttstub_input_close(handle);
        return true;
    }
    false
}
