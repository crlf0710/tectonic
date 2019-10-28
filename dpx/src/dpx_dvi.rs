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
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_mut
)]

use std::io::{Read, Seek, SeekFrom};

use crate::FromBEByteSlice;
use crate::DisplayExt;
use std::ffi::{CStr, CString};

use super::dpx_sfnt::{
    dfont_open, sfnt_close, sfnt_find_table_pos, sfnt_locate_table, sfnt_open,
    sfnt_read_table_directory,
};
use crate::mfree;
use crate::warn;
use crate::size_t;

use super::dpx_cff::cff_close;
use super::dpx_cff_dict::{cff_dict_get, cff_dict_known};
use super::dpx_dpxfile::{
    dpx_open_dfont_file, dpx_open_opentype_file, dpx_open_truetype_file, dpx_open_type1_file,
};
use super::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use super::dpx_dvipdfmx::{is_xdv, landscape_mode, paper_height, paper_width};
use super::dpx_fontmap::{pdf_insert_native_fontmap_record, pdf_lookup_fontmap_record};
use super::dpx_mem::{new, renew, xmalloc};
use super::dpx_numbers::{
    sqxfw, tt_get_positive_quad, tt_get_signed_quad, tt_get_unsigned_byte, tt_get_unsigned_num,
    tt_get_unsigned_pair, tt_get_unsigned_quad, tt_skip_bytes,
};
use super::dpx_pdfcolor::{pdf_color_pop, pdf_color_push, PdfColor};
use super::dpx_pdfdev::{
    graphics_mode, pdf_dev_begin_actualtext, pdf_dev_end_actualtext, pdf_dev_locate_font,
    pdf_dev_set_dirmode, pdf_dev_set_rect, pdf_dev_set_rule, pdf_dev_set_string,
};
use super::dpx_pdfdoc::{
    pdf_doc_begin_page, pdf_doc_break_annot, pdf_doc_end_page, pdf_doc_expand_box,
};
use super::dpx_pdfparse::{dump_slice, SkipWhite, ParsePdfObj};
use super::dpx_subfont::{lookup_sfd_record, sfd_load_record, subfont_set_verbose};
use super::dpx_t1_char::t1char_get_metrics;
use super::dpx_t1_load::t1_load_font;
use super::dpx_tfm::{
    tfm_close_all, tfm_get_fw_depth, tfm_get_fw_height, tfm_get_fw_width, tfm_open, tfm_set_verbose,
};
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_table::{
    tt_read_head_table, tt_read_hhea_table, tt_read_longMetrics, tt_read_maxp_table,
    tt_read_vhea_table,
};
use super::dpx_vf::{vf_close_all_fonts, vf_locate_font, vf_set_char, vf_set_verbose};
use crate::dpx_pdfobj::{
    pdf_number_value, pdf_release_obj, pdf_string_value,
};
use crate::dpx_truetype::sfnt_table_info;
use crate::shims::sprintf;
use crate::specials::{
    spc_exec_at_begin_page, spc_exec_at_end_page, spc_exec_special, spc_set_verbose,
};
use crate::{
    ttstub_input_close, ttstub_input_get_size, ttstub_input_getc, ttstub_input_open,
    ttstub_input_read, ttstub_input_ungetc,
};

use libc::{atof, free, memset, strlen, strncpy, strtol};

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
pub type fixword = i32;
/* quasi-hack to get the primary input */

pub type spt_t = i32;
use super::dpx_pdfdev::Rect;
/*
 * The section below this line deals with the actual processing of the
 * dvi file.
 *
 * The dvi file processor state is contained in the following variables:
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct dvi_registers {
    pub h: i32,
    pub v: i32,
    pub w: i32,
    pub x: i32,
    pub y: i32,
    pub z: i32,
    pub d: u32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct font_def {
    pub tex_id: u32,
    pub point_size: spt_t,
    pub design_size: spt_t,
    pub font_name: *mut i8,
    pub font_id: i32,
    pub used: i32,
    pub native: i32,
    pub rgba_color: u32,
    pub face_index: u32,
    pub layout_dir: i32,
    pub extend: i32,
    pub slant: i32,
    pub embolden: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct loaded_font<'a> {
    pub type_0: i32,
    pub font_id: i32,
    pub subfont_id: i32,
    pub tfm_id: i32,
    pub size: spt_t,
    pub source: i32,
    pub rgba_color: u32,
    pub hvmt: *mut tt_longMetrics,
    pub ascent: i32,
    pub descent: i32,
    pub unitsPerEm: u32,
    pub cffont: *mut cff_font<'a>,
    pub numGlyphs: u32,
    pub layout_dir: i32,
    pub extend: f32,
    pub slant: f32,
    pub embolden: f32,
}

use super::dpx_cff::cff_font;

use super::dpx_cff::cff_index;
use super::dpx_fontmap::fontmap_rec;
use super::dpx_tt_table::tt_longMetrics;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct dvi_header {
    pub unit_num: u32,
    pub unit_den: u32,
    pub mag: u32,
    pub media_width: u32,
    pub media_height: u32,
    pub stackdepth: u32,
    pub comment: [u8; 257],
}
/* skimming through reflected segment measuring its width */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct dvi_lr {
    pub state: i32,
    pub font: i32,
    pub buf_index: u32,
}

use super::dpx_t1_char::t1_ginfo;

/* 16.16-bit signed fixed-point number */
pub type FWord = i16;
/* Acoid conflict with CHAR ... from <winnt.h>.  */
/* Data Types as described in Apple's TTRefMan */
pub type Fixed = u32;

pub type uFWord = u16;

use super::dpx_tt_table::tt_vhea_table;
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/* UTF-32 over U+FFFF -> UTF-16 surrogate pair */
/* Interal Variables */
static mut dvi_handle: Option<InputHandleWrapper> = None;
static mut linear: i8 = 0_i8;
/* set to 1 for strict linear processing of the input */
static mut page_loc: *mut u32 = 0 as *const u32 as *mut u32;
static mut num_pages: u32 = 0_u32;
static mut dvi_file_size: u32 = 0_u32;
static mut DVI_INFO: dvi_header = {
    let mut init = dvi_header {
        unit_num: 25400000_u32,
        unit_den: 473628672_u32,
        mag: 1000_u32,
        media_width: 0_u32,
        media_height: 0_u32,
        stackdepth: 0_u32,
        comment: [0; 257],
    };
    init
};
static mut dev_origin_x: f64 = 72.0f64;
static mut dev_origin_y: f64 = 770.0f64;
#[no_mangle]
pub unsafe extern "C" fn get_origin(mut x: i32) -> f64 {
    if x != 0 {
        dev_origin_x
    } else {
        dev_origin_y
    }
}
static mut lr_state: dvi_lr = dvi_lr {
    state: 0,
    font: 0,
    buf_index: 0,
};
/* state at start of current skimming  */
static mut lr_mode: i32 = 0;
/* current direction or skimming depth */
static mut lr_width: u32 = 0;
/* total width of reflected segment    */
static mut lr_width_stack: [u32; 256] = [0; 256];
static mut lr_width_stack_depth: u32 = 0_u32;
static mut loaded_fonts: *mut loaded_font = 0 as *const loaded_font as *mut loaded_font;
static mut num_loaded_fonts: u32 = 0_u32;
static mut max_loaded_fonts: u32 = 0_u32;
unsafe fn need_more_fonts(mut n: u32) {
    if num_loaded_fonts.wrapping_add(n) > max_loaded_fonts {
        max_loaded_fonts = max_loaded_fonts.wrapping_add(16u32);
        loaded_fonts = renew(
            loaded_fonts as *mut libc::c_void,
            (max_loaded_fonts as u64).wrapping_mul(::std::mem::size_of::<loaded_font>() as u64)
                as u32,
        ) as *mut loaded_font
    };
}
static mut def_fonts: *mut font_def = 0 as *const font_def as *mut font_def;
static mut num_def_fonts: u32 = 0_u32;
static mut max_def_fonts: u32 = 0_u32;
static mut compute_boxes: i32 = 0i32;
static mut link_annot: i32 = 1i32;
static mut verbose: i32 = 0i32;
/* 64K should be plenty for most pages */
static mut DVI_PAGE_BUFFER: Vec<u8> = Vec::new();
static mut DVI_PAGE_BUF_INDEX: usize = 0;
/* functions to read numbers from the dvi file and store them in DVI_PAGE_BUFFER */
unsafe fn get_and_buffer_unsigned_byte(handle: &mut InputHandleWrapper) -> i32 {
    let ch = ttstub_input_getc(handle);
    if ch < 0i32 {
        panic!("File ended prematurely\n");
    }
    if DVI_PAGE_BUF_INDEX == DVI_PAGE_BUFFER.len() {
        DVI_PAGE_BUFFER.push(ch as u8);
    } else {
        DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] = ch as u8;
    }
    DVI_PAGE_BUF_INDEX += 1;
    ch
}
unsafe fn get_and_buffer_unsigned_pair(handle: &mut InputHandleWrapper) -> u32 {
    let mut pair: u32 = get_and_buffer_unsigned_byte(handle) as u32;
    pair = pair << 8i32 | get_and_buffer_unsigned_byte(handle) as u32;
    pair
}
unsafe fn get_and_buffer_bytes(handle: &mut InputHandleWrapper, mut count: u32) {
    DVI_PAGE_BUFFER.resize_with(DVI_PAGE_BUF_INDEX+count as usize, Default::default);
    handle.read_exact(&mut DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..])
        .expect("File ended prematurely\n");
    DVI_PAGE_BUF_INDEX += count as usize;
}
/* functions to fetch values from DVI_PAGE_BUFFER */
unsafe fn get_buffered_unsigned_byte() -> u8 {
    let b = DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX];
    DVI_PAGE_BUF_INDEX += 1;
    b
}
unsafe fn get_buffered_unsigned_pair() -> u16 {
    let pair = u16::from_be_byte_slice(&DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX+2]);
    DVI_PAGE_BUF_INDEX += 2;
    pair
}
unsafe fn get_buffered_signed_quad() -> i32 {
    let quad = i32::from_be_byte_slice(&DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX+4]);
    DVI_PAGE_BUF_INDEX += 4;
    quad
}
unsafe fn get_buffered_signed_num(mut num: u8) -> i32 {
    let mut quad: i32 = DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
    DVI_PAGE_BUF_INDEX += 1;
    if quad > 0x7f {
        quad -= 0x100
    }
    if num == 3 {
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    if num == 2 || num == 3 {
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    if num == 1 || num == 2 || num == 3 {
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    quad
}
unsafe fn get_buffered_unsigned_num(mut num: u8) -> i32 {
    let mut quad: i32 = DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
    DVI_PAGE_BUF_INDEX += 1;
    if num == 3 {
        if quad > 0x7f {
            quad -= 0x100
        }
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    if num == 2 || num == 3 {
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    if num == 1 || num == 2 || num == 3 {
        quad = (quad << 8) | DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] as i32;
        DVI_PAGE_BUF_INDEX += 1;
    }
    quad
}
#[no_mangle]
pub unsafe extern "C" fn dvi_set_verbose(mut level: i32) {
    verbose = level;
    subfont_set_verbose(level);
    tfm_set_verbose(level);
    vf_set_verbose(level);
    spc_set_verbose(level);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_npages() -> u32 {
    num_pages
}

const invalid_signature: &str = "Something is wrong. Are you sure this is a DVI file?";

static mut pre_id_byte: i32 = 0;
static mut post_id_byte: i32 = 0;
static mut is_ptex: i32 = 0i32;
static mut has_ptex: i32 = 0i32;
unsafe fn check_id_bytes() {
    if pre_id_byte != post_id_byte && (pre_id_byte != 2i32 || post_id_byte != 3i32) {
        panic!(
            "Inconsistent DVI id_bytes {} (pre) and {} (post)",
            pre_id_byte, post_id_byte
        );
    };
}
unsafe fn need_XeTeX(mut c: i32) {
    if is_xdv == 0 {
        panic!("DVI opcode {} only valid for XeTeX", c,);
    };
}
unsafe fn need_pTeX(mut c: i32) {
    if is_ptex == 0 {
        panic!("DVI opcode {} only valid for Ascii pTeX", c,);
    }
    has_ptex = 1i32;
}
unsafe fn find_post() -> i32 {
    let handle = dvi_handle.as_mut().unwrap();
    let mut dvi_size = ttstub_input_get_size(handle) as libc::off_t;
    if dvi_size > 0x7fffffffi32 as libc::off_t {
        panic!("DVI file size exceeds 31-bit");
    }
    dvi_file_size = dvi_size as u32;
    handle.seek(SeekFrom::End(0)).unwrap();
    let mut current = dvi_size as i32;
    let ch = loop
    /* Scan backwards through PADDING */
    {
        current -= 1;
        handle.seek(SeekFrom::Start(current as u64)).unwrap();
        let ch = ttstub_input_getc(handle);
        if !(ch == 223i32 && current > 0i32) {
            break ch;
        }
    };
    /* file_position now points to last non padding character or
     * beginning of file */
    if dvi_file_size.wrapping_sub(current as u32) < 4_u32
        || current == 0i32
        || !(ch == 2i32 || ch == 3i32 || ch == 7i32 || ch == 6i32)
    {
        info!("DVI ID = {}\n", ch);
        panic!(invalid_signature);
    }
    post_id_byte = ch;
    is_xdv = (ch == 7i32 || ch == 6i32) as i32;
    is_ptex = (ch == 3i32) as i32;
    /* Make sure post_post is really there */
    current = current - 5i32;
    handle.seek(SeekFrom::Start(current as u64)).unwrap();
    let ch = ttstub_input_getc(handle);
    if ch != 249i32 {
        info!("Found {} where post_post opcode should be\n", ch);
        panic!(invalid_signature);
    }
    current = tt_get_signed_quad(handle);
    handle.seek(SeekFrom::Start(current as u64)).unwrap();
    let ch = ttstub_input_getc(handle);
    if ch != 248i32 {
        info!("Found {} where post_post opcode should be\n", ch);
        panic!(invalid_signature);
    }
    /* Finally check the ID byte in the preamble */
    /* An Ascii pTeX DVI file has id_byte DVI_ID in the preamble but DVIV_ID in the postamble. */
    handle.seek(SeekFrom::Start(0)).unwrap();
    let ch = tt_get_unsigned_byte(handle) as i32;
    if ch != 247i32 {
        info!("Found {} where PRE was expected\n", ch);
        panic!(invalid_signature);
    }
    let ch = tt_get_unsigned_byte(handle) as i32;
    if !(ch == 2i32 || ch == 7i32 || ch == 6i32) {
        info!("DVI ID = {}\n", ch);
        panic!(invalid_signature);
    }
    pre_id_byte = ch;
    check_id_bytes();
    current
}
unsafe fn get_page_info(mut post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(post_location as u64 + 27)).unwrap();
    num_pages = tt_get_unsigned_pair(handle) as u32;
    if num_pages == 0_u32 {
        panic!("Page count is 0!");
    }
    if verbose > 2i32 {
        info!("Page count:\t {:4}\n", num_pages,);
    }
    page_loc = new((num_pages as u64).wrapping_mul(::std::mem::size_of::<u32>() as u64) as u32)
        as *mut u32;
    handle.seek(SeekFrom::Start(post_location as u64 + 1)).unwrap();
    *page_loc.offset(num_pages.wrapping_sub(1_u32) as isize) = tt_get_unsigned_quad(handle);
    if (*page_loc.offset(num_pages.wrapping_sub(1_u32) as isize)).wrapping_add(41_u32)
        > dvi_file_size
    {
        panic!(invalid_signature);
    }
    let mut i = num_pages.wrapping_sub(2_u32) as i32;
    while i >= 0i32 {
        handle.seek(SeekFrom::Start(*page_loc.offset((i + 1) as isize) as u64 + 41)).unwrap();
        *page_loc.offset(i as isize) = tt_get_unsigned_quad(handle);
        if (*page_loc.offset(num_pages.wrapping_sub(1_u32) as isize)).wrapping_add(41_u32)
            > dvi_file_size
        {
            panic!(invalid_signature);
        }
        i -= 1
    }
}
/* Following are computed "constants" used for unit conversion */
static mut dvi2pts: f64 = 1.52018f64;
static mut total_mag: f64 = 1.0f64;
#[no_mangle]
pub unsafe extern "C" fn dvi_tell_mag() -> f64 {
    return total_mag; /* unused */
}
unsafe fn do_scales(mut mag: f64) {
    total_mag = DVI_INFO.mag as f64 / 1000.0f64 * mag; /* 1.0 */
    dvi2pts = DVI_INFO.unit_num as f64 / DVI_INFO.unit_den as f64; /* font name length */
    dvi2pts *= 72.0f64 / 254000.0f64; /* hard-code as 10pt for now, not used anyway */
}
unsafe fn get_dvi_info(mut post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(post_location as u64 + 5)).unwrap(); /* direction */
    DVI_INFO.unit_num = tt_get_unsigned_quad(handle);
    DVI_INFO.unit_den = tt_get_unsigned_quad(handle);
    DVI_INFO.mag = tt_get_unsigned_quad(handle);
    DVI_INFO.media_height = tt_get_unsigned_quad(handle);
    DVI_INFO.media_width = tt_get_unsigned_quad(handle);
    DVI_INFO.stackdepth = tt_get_unsigned_pair(handle) as u32;
    if DVI_INFO.stackdepth > 256u32 {
        warn!("DVI need stack depth of {},", DVI_INFO.stackdepth);
        warn!("but DVI_STACK_DEPTH_MAX is {}.", 256u32);
        panic!("Capacity exceeded.");
    }
    if verbose > 2i32 {
        info!("DVI File Info\n");
        info!("Unit: {} / {}\n", DVI_INFO.unit_num, DVI_INFO.unit_den);
        info!("Magnification: {}\n", DVI_INFO.mag);
        info!("Media Height: {}\n", DVI_INFO.media_height);
        info!("Media Width: {}\n", DVI_INFO.media_width);
        info!("Stack Depth: {}\n", DVI_INFO.stackdepth);
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_comment() -> *const i8 {
    DVI_INFO.comment.as_mut_ptr() as *const i8
}
unsafe fn read_font_record(mut tex_id: u32) {
    if num_def_fonts >= max_def_fonts {
        max_def_fonts = max_def_fonts.wrapping_add(16u32);
        def_fonts = renew(
            def_fonts as *mut libc::c_void,
            (max_def_fonts as u64).wrapping_mul(::std::mem::size_of::<font_def>() as u64) as u32,
        ) as *mut font_def
    }
    let handle = dvi_handle.as_mut().unwrap();
    tt_get_unsigned_quad(handle);
    let point_size = tt_get_positive_quad(
        handle,
        b"DVI\x00" as *const u8 as *const i8,
        b"point_size\x00" as *const u8 as *const i8,
    );
    let design_size = tt_get_positive_quad(
        handle,
        b"DVI\x00" as *const u8 as *const i8,
        b"design_size\x00" as *const u8 as *const i8,
    );
    let dir_length = tt_get_unsigned_byte(handle) as i32;
    let name_length = tt_get_unsigned_byte(handle) as i32;
    let directory = new(
        ((dir_length + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32
    ) as *mut i8;
    if ttstub_input_read(handle.0.as_ptr(), directory, dir_length as size_t) != dir_length as i64 {
        panic!(invalid_signature);
    }
    *directory.offset(dir_length as isize) = '\u{0}' as i32 as i8;
    free(directory as *mut libc::c_void);
    let font_name = new(((name_length + 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    if ttstub_input_read(handle.0.as_ptr(), font_name, name_length as size_t) != name_length as i64 {
        panic!(invalid_signature);
    }
    *font_name.offset(name_length as isize) = '\u{0}' as i32 as i8;
    (*def_fonts.offset(num_def_fonts as isize)).tex_id = tex_id;
    let ref mut fresh14 = (*def_fonts.offset(num_def_fonts as isize)).font_name;
    *fresh14 = font_name;
    (*def_fonts.offset(num_def_fonts as isize)).point_size = point_size as spt_t;
    (*def_fonts.offset(num_def_fonts as isize)).design_size = design_size as spt_t;
    (*def_fonts.offset(num_def_fonts as isize)).used = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).native = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).rgba_color = 0xffffffffu32;
    (*def_fonts.offset(num_def_fonts as isize)).face_index = 0_u32;
    (*def_fonts.offset(num_def_fonts as isize)).layout_dir = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).extend = 0x10000i32;
    (*def_fonts.offset(num_def_fonts as isize)).slant = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).embolden = 0i32;
    num_def_fonts = num_def_fonts.wrapping_add(1);
}
unsafe fn read_native_font_record(mut tex_id: u32) {
    if num_def_fonts >= max_def_fonts {
        max_def_fonts = max_def_fonts.wrapping_add(16u32);
        def_fonts = renew(
            def_fonts as *mut libc::c_void,
            (max_def_fonts as u64).wrapping_mul(::std::mem::size_of::<font_def>() as u64) as u32,
        ) as *mut font_def
    }
    let handle = dvi_handle.as_mut().unwrap();
    let point_size = tt_get_positive_quad(
        handle,
        b"DVI\x00" as *const u8 as *const i8,
        b"point_size\x00" as *const u8 as *const i8,
    );
    let flags = tt_get_unsigned_pair(handle) as u32;
    let len = tt_get_unsigned_byte(handle) as i32;
    let font_name =
        new(((len + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    if ttstub_input_read(handle.0.as_ptr(), font_name, len as size_t) != len as i64 {
        panic!(invalid_signature);
    }
    *font_name.offset(len as isize) = '\u{0}' as i32 as i8;
    let index = tt_get_positive_quad(
        handle,
        b"DVI\x00" as *const u8 as *const i8,
        b"index\x00" as *const u8 as *const i8,
    );
    (*def_fonts.offset(num_def_fonts as isize)).tex_id = tex_id;
    let ref mut fresh15 = (*def_fonts.offset(num_def_fonts as isize)).font_name;
    *fresh15 = font_name;
    (*def_fonts.offset(num_def_fonts as isize)).face_index = index;
    (*def_fonts.offset(num_def_fonts as isize)).point_size = point_size as spt_t;
    (*def_fonts.offset(num_def_fonts as isize)).design_size = 655360i32;
    (*def_fonts.offset(num_def_fonts as isize)).used = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).native = 1i32;
    (*def_fonts.offset(num_def_fonts as isize)).layout_dir = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).rgba_color = 0xffffffffu32;
    (*def_fonts.offset(num_def_fonts as isize)).extend = 0x10000i32;
    (*def_fonts.offset(num_def_fonts as isize)).slant = 0i32;
    (*def_fonts.offset(num_def_fonts as isize)).embolden = 0i32;
    if flags & 0x100_u32 != 0 {
        (*def_fonts.offset(num_def_fonts as isize)).layout_dir = 1i32
    }
    if flags & 0x200_u32 != 0 {
        (*def_fonts.offset(num_def_fonts as isize)).rgba_color = tt_get_unsigned_quad(handle)
    }
    if flags & 0x1000_u32 != 0 {
        (*def_fonts.offset(num_def_fonts as isize)).extend = tt_get_signed_quad(handle)
    }
    if flags & 0x2000_u32 != 0 {
        (*def_fonts.offset(num_def_fonts as isize)).slant = tt_get_signed_quad(handle)
    }
    if flags & 0x4000_u32 != 0 {
        (*def_fonts.offset(num_def_fonts as isize)).embolden = tt_get_signed_quad(handle)
    }
    num_def_fonts = num_def_fonts.wrapping_add(1);
}
unsafe fn get_dvi_fonts(mut post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(post_location as u64 + 29)).unwrap();
    loop {
        let code = tt_get_unsigned_byte(handle) as i32;
        if !(code != 249i32) {
            break;
        }
        match code {
            243 | 244 | 245 | 246 => {
                read_font_record(tt_get_unsigned_num(handle, (code - 243) as u8));
            }
            252 => {
                need_XeTeX(code);
                read_native_font_record(tt_get_signed_quad(handle) as u32);
            }
            _ => {
                info!("Unexpected op code: {:3}\n", code,);
                panic!(invalid_signature);
            }
        }
    }
    if verbose > 2i32 {
        info!("\n");
        info!("DVI file font info\n");
        for i in 0..num_def_fonts {
            info!(
                "TeX Font: {:10} loaded at ID={:5}, ",
                CStr::from_ptr((*def_fonts.offset(i as isize)).font_name).display(),
                (*def_fonts.offset(i as isize)).tex_id,
            );
            info!(
                "size={:5.2}pt (scaled {:4.1}%)",
                (*def_fonts.offset(i as isize)).point_size as f64 * dvi2pts,
                100.0f64
                    * ((*def_fonts.offset(i as isize)).point_size as f64
                        / (*def_fonts.offset(i as isize)).design_size as f64),
            );
            info!("\n");
        }
    };
}
unsafe fn get_comment() {
    let handle = dvi_handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(14)).unwrap();
    let length = tt_get_unsigned_byte(handle) as usize;
    handle.read_exact(&mut DVI_INFO.comment[..length]).expect(invalid_signature);
    DVI_INFO.comment[length as usize] = '\u{0}' as u8;
    if verbose != 0 {
        info!(
            "DVI Comment: {}\n",
            DVI_INFO.comment[..length].display()
        );
    };
}
static mut dvi_state: dvi_registers = dvi_registers {
    h: 0,
    v: 0,
    w: 0,
    x: 0,
    y: 0,
    z: 0,
    d: 0,
};
static mut dvi_stack: [dvi_registers; 256] = [dvi_registers {
    h: 0,
    v: 0,
    w: 0,
    x: 0,
    y: 0,
    z: 0,
    d: 0,
}; 256];
static mut dvi_stack_depth: i32 = 0i32;
static mut current_font: i32 = -1i32;
static mut processing_page: i32 = 0i32;
unsafe fn clear_state() {
    dvi_state.h = 0i32;
    dvi_state.v = 0i32;
    dvi_state.w = 0i32;
    dvi_state.x = 0i32;
    dvi_state.y = 0i32;
    dvi_state.z = 0i32;
    dvi_state.d = 0_u32;
    pdf_dev_set_dirmode(0i32);
    dvi_stack_depth = 0i32;
    current_font = -1i32;
}
/* Migrated from pdfdev.c:
 * The following codes are originally put into pdfdev.c.
 * But they are moved to here to make PDF output independent
 * from DVI input.
 * pdfdoc, pdfspecial and htex are also modified. pdfspecial
 * and htex does tag/untag depth. pdfdev and pdfdoc now does
 * not care about line-breaking at all.
 */
static mut marked_depth: i32 = 0i32;
static mut tagged_depth: i32 = -1i32;
unsafe fn dvi_mark_depth() {
    /* If decreasing below tagged_depth */
    if link_annot != 0 && marked_depth == tagged_depth && dvi_stack_depth == tagged_depth - 1i32 {
        /*
         * See if this appears to be the end of a "logical unit"
         * that's been broken.  If so, flush the logical unit.
         */
        pdf_doc_break_annot();
    }
    marked_depth = dvi_stack_depth;
}
/*
 * The following routines setup and tear down a callback at a
 * certain stack depth. This is used to handle broken (linewise)
 * links.
 */
#[no_mangle]
pub unsafe extern "C" fn dvi_tag_depth() {
    tagged_depth = marked_depth;
    dvi_compute_boxes(1i32);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_untag_depth() {
    tagged_depth = -1i32;
    dvi_compute_boxes(0i32);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_compute_boxes(mut flag: i32) {
    compute_boxes = flag;
}
#[no_mangle]
pub unsafe extern "C" fn dvi_link_annot(mut flag: i32) {
    link_annot = flag;
}
/* allow other modules (pdfdev) to ask whether we're collecting box areas */
#[no_mangle]
pub unsafe extern "C" fn dvi_is_tracking_boxes() -> bool {
    compute_boxes != 0 && link_annot != 0 && marked_depth >= tagged_depth
}
/* link or nolink:
 * See dvipdfm (not x) user's manual on pdf:link and pdf:nolink.
 * This is workaround for preventing inclusion of pagenation artifact such as
 * footnote and page number in link annotation.
 */
/* The followings are for calculating bounding box of text for annotation.
 * DVI uses push/pop to do line-feed-carriage-return. So line breaking is
 * handled by inspecting current depth of DVI register stack.
 */
#[no_mangle]
pub unsafe extern "C" fn dvi_do_special(buffer: &[u8]) {
    /* VF or device font ID */
    graphics_mode();
    let x_user = dvi_state.h as f64 * dvi2pts;
    let y_user = -dvi_state.v as f64 * dvi2pts;
    let mag = dvi_tell_mag();
    if spc_exec_special(buffer, x_user, y_user, mag) < 0i32 {
        if verbose != 0 {
            dump_slice(buffer);
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_unit_size() -> f64 {
    dvi2pts
}
#[no_mangle]
pub unsafe extern "C" fn dvi_locate_font(mut tfm_name: *const i8, mut ptsize: spt_t) -> u32 {
    let mut subfont_id: i32 = -1i32;
    if verbose != 0 {
        info!(
            "<{}@{:.2}pt",
            CStr::from_ptr(tfm_name).display(),
            ptsize as f64 * dvi2pts,
        );
    }
    need_more_fonts(1_u32);
    /* This routine needs to be recursive/reentrant. Load current high water
     * mark into an automatic variable.
     */
    let fresh16 = num_loaded_fonts;
    num_loaded_fonts = num_loaded_fonts.wrapping_add(1);
    let cur_id = fresh16;
    let mrec = pdf_lookup_fontmap_record(CStr::from_ptr(tfm_name).to_bytes());
    /* Load subfont mapping table */
    if !mrec.is_null()
        && !(*mrec).charmap.sfd_name.is_null()
        && !(*mrec).charmap.subfont_id.is_null()
    {
        subfont_id = sfd_load_record((*mrec).charmap.sfd_name, (*mrec).charmap.subfont_id)
    }
    memset(
        &mut *loaded_fonts.offset(cur_id as isize) as *mut loaded_font as *mut libc::c_void,
        0i32,
        ::std::mem::size_of::<loaded_font>(),
    );
    /* TFM must exist here. */
    (*loaded_fonts.offset(cur_id as isize)).tfm_id = tfm_open(tfm_name, 1i32);
    (*loaded_fonts.offset(cur_id as isize)).subfont_id = subfont_id;
    (*loaded_fonts.offset(cur_id as isize)).size = ptsize;
    /* This will be reset later if it was really generated by the dvi file. */
    (*loaded_fonts.offset(cur_id as isize)).source = 2i32;
    /* The order of searching fonts is as follows:
     *
     * 1. If mrec is null, that is, there is no map entry matching
     *    with tfm_name, then search a virtual font matching with
     *    tfm_name at first. If no virtual font is found, search a
     *    PK font matching with tfm_name.
     *
     * 2. If mrec is non-null, search a physical scalable font.
     *
     * 3. Notice that every subfont gets non-null mrec. In this case,
     *    enc_name corresponding to mrec will be used instead of mrec.
     *    That is enc_name is NULL, search a virtual font for Omega (.ovf)
     *    matching with the base name of the subfont. If no virtual font
     *    for Omega is found, it is a fatal error because there is no PK font
     *    for Omega.
     */
    if mrec.is_null() {
        let font_id = vf_locate_font(tfm_name, ptsize);
        if font_id >= 0i32 {
            (*loaded_fonts.offset(cur_id as isize)).type_0 = 2i32;
            (*loaded_fonts.offset(cur_id as isize)).font_id = font_id;
            if verbose != 0 {
                info!("(VF)>");
            }
            return cur_id;
        }
    } else if subfont_id >= 0i32 && !(*mrec).map_name.is_null() {
        let mut mrec1: *mut fontmap_rec = pdf_lookup_fontmap_record(CStr::from_ptr((*mrec).map_name).to_bytes());
        /* Sorry, I don't understand this well... Please fix.
         * The purpose of this seems to be:
         *
         *   Map 8-bit char codes in subfont to 16-bit code with SFD mapping
         *   and map subfonts to single OVF font.
         *
         * But it apparently only does TFM -> OVF mapping but no character
         * code mapping. Please see dvi_set(), you can't have both font->type
         * VIRTUAL and font->subfont_id >= 0. Am I missing something?
         */
        /* enc_name=NULL should be used only for 'built-in' encoding.
         * Please fix this!
         */
        if !mrec1.is_null() && (*mrec1).enc_name.is_null() {
            let font_id = vf_locate_font((*mrec1).font_name, ptsize);
            if font_id < 0i32 {
                warn!(
                    "Could not locate Omega Virtual Font \"{}\" for \"{}\".",
                    CStr::from_ptr((*mrec1).font_name).display(),
                    CStr::from_ptr(tfm_name).display(),
                );
            } else {
                (*loaded_fonts.offset(cur_id as isize)).type_0 = 2i32;
                (*loaded_fonts.offset(cur_id as isize)).font_id = font_id;
                if verbose != 0 {
                    info!("(OVF)>");
                }
                return cur_id;
            }
        }
    }
    /* 1 */
    /* Failed to load a virtual font so we try to load a physical font. */
    /* If mrec->map_name is not NULL, font name identified in PDF output
     * is different than tfm_name, this can happen for subfonts grouped
     * into a single "intermediate" font foo@SFD@.
     * This is necessary for optimal output; to avoid unnecessary creation
     * of multiple instances of a same font, to avoid frequent font selection
     * and break of string_mode.
     */
    let mut name = if !mrec.is_null() && !(*mrec).map_name.is_null() {
        (*mrec).map_name
    } else {
        tfm_name
    };
    /* We need ptsize for PK font creation. */
    let font_id = pdf_dev_locate_font(name, ptsize);
    if font_id < 0i32 {
        warn!(
            "Could not locate a virtual/physical font for TFM \"{}\".",
            CStr::from_ptr(tfm_name).display()
        );
        if !mrec.is_null() && !(*mrec).map_name.is_null() {
            /* has map_name */
            let mut mrec1_0: *mut fontmap_rec = pdf_lookup_fontmap_record(CStr::from_ptr((*mrec).map_name).to_bytes()); // CHECK this is enough
            warn!(">> This font is mapped to an intermediate 16-bit font \"{}\" with SFD charmap=<{},{}>,",
                        CStr::from_ptr((*mrec).map_name).display(), CStr::from_ptr((*mrec).charmap.sfd_name).display(),
                        CStr::from_ptr((*mrec).charmap.subfont_id).display()
                        );
            if mrec1_0.is_null() {
                warn!(
                    ">> but I couldn\'t find font mapping for \"{}\".",
                    CStr::from_ptr((*mrec).map_name).display()
                );
            } else {
                warn!(
                    ">> and then mapped to a physical font \"{}\" by fontmap.",
                    CStr::from_ptr((*mrec1_0).font_name).display()
                );
                warn!(
                    ">> Please check if kpathsea library can find this font: {}",
                    CStr::from_ptr((*mrec1_0).font_name).display(),
                );
            }
        } else if !mrec.is_null() && (*mrec).map_name.is_null() {
            warn!(
                ">> This font is mapped to a physical font \"{}\".",
                CStr::from_ptr((*mrec).font_name).display()
            );
            warn!(
                ">> Please check if kpathsea library can find this font: {}",
                CStr::from_ptr((*mrec).font_name).display(),
            );
        } else {
            warn!(">> There are no valid font mapping entry for this font.");
            warn!(
                ">> Font file name \"{}\" was assumed but failed to locate that font.",
                CStr::from_ptr(tfm_name).display()
            );
        }
        panic!("Cannot proceed without .vf or \"physical\" font for PDF output...");
    }
    (*loaded_fonts.offset(cur_id as isize)).type_0 = 1i32;
    (*loaded_fonts.offset(cur_id as isize)).font_id = font_id;
    if verbose != 0 {
        info!(">");
    }
    cur_id
}
unsafe fn dvi_locate_native_font(
    mut filename: *const i8,
    mut index: u32,
    mut ptsize: spt_t,
    mut layout_dir: i32,
    mut extend: i32,
    mut slant: i32,
    mut embolden: i32,
) -> i32 {
    let mut offset: u32 = 0_u32;
    let mut is_dfont: i32 = 0i32;
    let mut is_type1: i32 = 0i32;
    if verbose != 0 {
        info!(
            "<{}@{:.2}pt",
            CStr::from_ptr(filename).display(),
            ptsize as f64 * dvi2pts,
        );
    }
    let mut handle = dpx_open_dfont_file(filename);
    if handle.is_some() {
        is_dfont = 1
    } else {
        handle = dpx_open_type1_file(filename);
        if handle.is_some() {
            is_type1 = 1
        } else {
            handle = dpx_open_opentype_file(filename);
            if handle.is_none() && {
                handle = dpx_open_truetype_file(filename);
                handle.is_none()
            } {
                panic!(
                    "Cannot proceed without the font: {}",
                    CStr::from_ptr(filename).display()
                );
            }
        }
    }
    let mut handle = handle.unwrap();
    need_more_fonts(1_u32);
    let fresh17 = num_loaded_fonts;
    num_loaded_fonts = num_loaded_fonts.wrapping_add(1);
    let cur_id = fresh17 as i32;
    let fontmap_key = xmalloc(strlen(filename).wrapping_add(40) as _) as *mut i8;
    sprintf(
        fontmap_key,
        b"%s/%u/%c/%d/%d/%d\x00" as *const u8 as *const i8,
        filename,
        index,
        if layout_dir == 0i32 {
            'H' as i32
        } else {
            'V' as i32
        },
        extend,
        slant,
        embolden,
    );
    let mut mrec = pdf_lookup_fontmap_record(CStr::from_ptr(fontmap_key).to_bytes());
    if mrec.is_null() {
        mrec =
            pdf_insert_native_fontmap_record(filename, index, layout_dir, extend, slant, embolden);
        if mrec.is_null() {
            panic!(
                "Failed to insert font record for font: {}",
                CStr::from_ptr(filename).display()
            );
        }
    }
    memset(
        &mut *loaded_fonts.offset(cur_id as isize) as *mut loaded_font as *mut libc::c_void,
        0i32,
        ::std::mem::size_of::<loaded_font>(),
    );
    (*loaded_fonts.offset(cur_id as isize)).font_id = pdf_dev_locate_font(fontmap_key, ptsize);
    (*loaded_fonts.offset(cur_id as isize)).size = ptsize;
    (*loaded_fonts.offset(cur_id as isize)).type_0 = 4i32;
    free(fontmap_key as *mut libc::c_void);
    if is_type1 != 0 {
        let mut enc_vec: [*mut i8; 256] = [0 as *mut i8; 256];
        /*if (!is_pfb(fp))
         *  panic!("Failed to read Type 1 font \"{}\".", filename);
         */
        warn!("skipping PFB sanity check -- needs Tectonic I/O update");
        memset(
            enc_vec.as_mut_ptr() as *mut libc::c_void,
            0i32,
            (256usize).wrapping_mul(::std::mem::size_of::<*mut i8>()),
        );
        let cffont = t1_load_font(enc_vec.as_mut_ptr(), 0i32, handle);
        if cffont.is_null() {
            panic!(
                "Failed to read Type 1 font \"{}\".",
                CStr::from_ptr(filename).display()
            );
        }
        let ref mut fresh18 = (*loaded_fonts.offset(cur_id as isize)).cffont;
        *fresh18 = cffont;
        if cff_dict_known((*cffont).topdict, b"FontBBox\x00" as *const u8 as *const i8) != 0 {
            (*loaded_fonts.offset(cur_id as isize)).ascent = cff_dict_get(
                (*cffont).topdict,
                b"FontBBox\x00" as *const u8 as *const i8,
                3i32,
            ) as i32;
            (*loaded_fonts.offset(cur_id as isize)).descent = cff_dict_get(
                (*cffont).topdict,
                b"FontBBox\x00" as *const u8 as *const i8,
                1i32,
            ) as i32
        } else {
            (*loaded_fonts.offset(cur_id as isize)).ascent = 690i32;
            (*loaded_fonts.offset(cur_id as isize)).descent = -190i32
        }
        (*loaded_fonts.offset(cur_id as isize)).unitsPerEm = 1000_u32;
        (*loaded_fonts.offset(cur_id as isize)).numGlyphs = (*cffont).num_glyphs as u32;
    } else {
        let sfont = if is_dfont != 0 {
            dfont_open(handle, index as i32)
        } else {
            sfnt_open(handle)
        };
        if (*sfont).type_0 == 1i32 << 4i32 {
            offset = ttc_read_offset(sfont, index as i32)
        } else if (*sfont).type_0 == 1i32 << 8i32 {
            offset = (*sfont).offset
        }
        sfnt_read_table_directory(sfont, offset);
        let head = tt_read_head_table(sfont);
        let maxp = tt_read_maxp_table(sfont);
        let hhea = tt_read_hhea_table(sfont);
        (*loaded_fonts.offset(cur_id as isize)).ascent = (*hhea).ascent as i32;
        (*loaded_fonts.offset(cur_id as isize)).descent = (*hhea).descent as i32;
        (*loaded_fonts.offset(cur_id as isize)).unitsPerEm = (*head).unitsPerEm as u32;
        (*loaded_fonts.offset(cur_id as isize)).numGlyphs = (*maxp).numGlyphs as u32;
        if layout_dir == 1i32 && sfnt_find_table_pos(sfont, b"vmtx") > 0_u32 {
            let mut vhea: *mut tt_vhea_table = tt_read_vhea_table(sfont);
            sfnt_locate_table(sfont, b"vmtx");
            let ref mut fresh19 = (*loaded_fonts.offset(cur_id as isize)).hvmt;
            *fresh19 = tt_read_longMetrics(
                sfont,
                (*maxp).numGlyphs,
                (*vhea).numOfLongVerMetrics,
                (*vhea).numOfExSideBearings,
            );
            free(vhea as *mut libc::c_void);
        } else {
            sfnt_locate_table(sfont, sfnt_table_info::HMTX);
            let ref mut fresh20 = (*loaded_fonts.offset(cur_id as isize)).hvmt;
            *fresh20 = tt_read_longMetrics(
                sfont,
                (*maxp).numGlyphs,
                (*hhea).numOfLongHorMetrics,
                (*hhea).numOfExSideBearings,
            )
        }
        free(hhea as *mut libc::c_void);
        free(maxp as *mut libc::c_void);
        free(head as *mut libc::c_void);
        sfnt_close(sfont);
    }
    (*loaded_fonts.offset(cur_id as isize)).layout_dir = layout_dir;
    (*loaded_fonts.offset(cur_id as isize)).extend = (*mrec).opt.extend as f32;
    (*loaded_fonts.offset(cur_id as isize)).slant = (*mrec).opt.slant as f32;
    (*loaded_fonts.offset(cur_id as isize)).embolden = (*mrec).opt.bold as f32;
    if verbose != 0 {
        info!(">");
    }
    cur_id
}
#[no_mangle]
pub unsafe extern "C" fn dvi_dev_xpos() -> f64 {
    dvi_state.h as f64 * dvi2pts
}
#[no_mangle]
pub unsafe extern "C" fn dvi_dev_ypos() -> f64 {
    -(dvi_state.v as f64 * dvi2pts)
}
unsafe fn do_moveto(mut x: i32, mut y: i32) {
    dvi_state.h = x;
    dvi_state.v = y;
}
/* FIXME: dvi_forward() might be a better name */
#[no_mangle]
pub unsafe extern "C" fn dvi_right(mut x: i32) {
    if lr_mode >= 2i32 {
        lr_width = (lr_width as u32).wrapping_add(x as u32) as u32;
        return;
    }
    if lr_mode == 1i32 {
        x = -x
    }
    match dvi_state.d {
        0 => dvi_state.h += x,
        1 => dvi_state.v += x,
        3 => dvi_state.v -= x,
        _ => {}
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_down(mut y: i32) {
    if lr_mode < 2i32 {
        match dvi_state.d {
            0 => dvi_state.v += y,
            1 => dvi_state.h -= y,
            3 => dvi_state.h += y,
            _ => {}
        }
    };
}
/* _FIXME_
 * CMap decoder wants multibyte strings as input but
 * how DVI char codes are converted to multibyte sting
 * is not clear.
 */
#[no_mangle]
pub unsafe extern "C" fn dvi_set(mut ch: i32) {
    let mut wbuf: [u8; 4] = [0; 4];
    if current_font < 0i32 {
        panic!("No font selected!");
    }
    /* The division by dvi2pts seems strange since we actually know the
     * "dvi" size of the fonts contained in the DVI file.  In other
     * words, we converted from DVI units to pts and back again!
     * The problem comes from fonts defined in VF files where we don't know
     * the DVI size.  It's keeping me sane to keep *point sizes* of *all*
     * fonts in the dev.c file and convert them back if necessary.
     */
    let font = &mut *loaded_fonts.offset(current_font as isize) as *mut loaded_font; /* Will actually move left */
    let mut width = tfm_get_fw_width((*font).tfm_id, ch);
    width = sqxfw((*font).size, width);
    if lr_mode >= 2i32 {
        lr_width = (lr_width as u32).wrapping_add(width as u32) as u32;
        return;
    }
    if lr_mode == 1i32 {
        dvi_right(width);
    }
    match (*font).type_0 {
        1 => {
            if ch > 65535i32 {
                /* _FIXME_ */
                wbuf[0] =
                    (0xd800i32 + (ch - 0x10000i32 >> 10i32 & 0x3ffi32) >> 8i32 & 0xffi32) as u8;
                wbuf[1] = (0xd800i32 + (ch - 0x10000i32 >> 10i32 & 0x3ffi32) & 0xffi32) as u8;
                wbuf[2] = (0xdc00i32 + (ch & 0x3ffi32) >> 8i32 & 0xffi32) as u8;
                wbuf[3] = (0xdc00i32 + (ch & 0x3ffi32) & 0xffi32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    4i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else if ch > 255i32 {
                /* _FIXME_ */
                wbuf[0] = (ch >> 8i32 & 0xffi32) as u8; /* push/pop invoked */
                wbuf[1] = (ch & 0xffi32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    2i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else if (*font).subfont_id >= 0i32 {
                let mut uch: u16 = lookup_sfd_record((*font).subfont_id, ch as u8);
                wbuf[0] = (uch as i32 >> 8i32 & 0xffi32) as u8;
                wbuf[1] = (uch as i32 & 0xffi32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    2i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else {
                wbuf[0] = ch as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    1i32 as size_t,
                    width,
                    (*font).font_id,
                    1i32,
                );
            }
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let mut height = tfm_get_fw_height((*font).tfm_id, ch);
                let mut depth = tfm_get_fw_depth((*font).tfm_id, ch);
                height = sqxfw((*font).size, height);
                depth = sqxfw((*font).size, depth);
                pdf_dev_set_rect(&mut rect, dvi_state.h, -dvi_state.v, width, height, depth);
                pdf_doc_expand_box(&mut rect);
            }
        }
        2 => {
            vf_set_char(ch, (*font).font_id);
        }
        _ => {}
    }
    if lr_mode == 0i32 {
        dvi_right(width);
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_put(mut ch: i32) {
    let mut wbuf: [u8; 4] = [0; 4];
    if current_font < 0i32 {
        panic!("No font selected!");
    }
    let font = &mut *loaded_fonts.offset(current_font as isize) as *mut loaded_font;
    match (*font).type_0 {
        1 => {
            let mut width = tfm_get_fw_width((*font).tfm_id, ch);
            width = sqxfw((*font).size, width);
            /* Treat a single character as a one byte string and use the
             * string routine.
             */
            if ch > 65535i32 {
                /* _FIXME_ */
                wbuf[0] =
                    (0xd800i32 + (ch - 0x10000i32 >> 10i32 & 0x3ffi32) >> 8i32 & 0xffi32) as u8;
                wbuf[1] = (0xd800i32 + (ch - 0x10000i32 >> 10i32 & 0x3ffi32) & 0xffi32) as u8;
                wbuf[2] = (0xdc00i32 + (ch & 0x3ffi32) >> 8i32 & 0xffi32) as u8;
                wbuf[3] = (0xdc00i32 + (ch & 0x3ffi32) & 0xffi32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    4i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else if ch > 255i32 {
                /* _FIXME_ */
                wbuf[0] = (ch >> 8i32 & 0xffi32) as u8;
                wbuf[1] = (ch & 0xffi32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    2i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else if (*font).subfont_id >= 0i32 {
                let uch = lookup_sfd_record((*font).subfont_id, ch as u8) as u32;
                wbuf[0] = (uch >> 8i32 & 0xff_u32) as u8;
                wbuf[1] = (uch & 0xff_u32) as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    2i32 as size_t,
                    width,
                    (*font).font_id,
                    2i32,
                );
            } else {
                wbuf[0] = ch as u8;
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    wbuf.as_mut_ptr() as *const libc::c_void,
                    1i32 as size_t,
                    width,
                    (*font).font_id,
                    1i32,
                );
            }
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let mut height = tfm_get_fw_height((*font).tfm_id, ch);
                let mut depth = tfm_get_fw_depth((*font).tfm_id, ch);
                height = sqxfw((*font).size, height);
                depth = sqxfw((*font).size, depth);
                pdf_dev_set_rect(&mut rect, dvi_state.h, -dvi_state.v, width, height, depth);
                pdf_doc_expand_box(&mut rect);
            }
        }
        2 => {
            vf_set_char(ch, (*font).font_id);
        }
        _ => {}
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_rule(mut width: i32, mut height: i32) {
    if width > 0i32 && height > 0i32 {
        do_moveto(dvi_state.h, dvi_state.v);
        match dvi_state.d {
            0 => {
                pdf_dev_set_rule(dvi_state.h, -dvi_state.v, width, height);
            }
            1 => {
                pdf_dev_set_rule(dvi_state.h, -dvi_state.v - width, height, width);
            }
            3 => {
                pdf_dev_set_rule(dvi_state.h - height, -dvi_state.v, height, width);
            }
            _ => {}
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_dirchg(mut dir: u8) {
    if verbose != 0 {
        eprintln!("  > dvi_dirchg {}", dir as i32);
    }
    dvi_state.d = dir as u32;
    pdf_dev_set_dirmode(dvi_state.d as i32);
    /* 0: horizontal, 1,3: vertical */
}
unsafe fn do_setrule() {
    let height = get_buffered_signed_quad();
    let width = get_buffered_signed_quad();
    match lr_mode {
        0 => {
            dvi_rule(width, height);
            dvi_right(width);
        }
        1 => {
            dvi_right(width);
            dvi_rule(width, height);
        }
        _ => lr_width = (lr_width as u32).wrapping_add(width as u32) as u32 as u32,
    };
}
unsafe fn do_putrule() {
    let height = get_buffered_signed_quad();
    let width = get_buffered_signed_quad();
    match lr_mode {
        0 => {
            dvi_rule(width, height);
        }
        1 => {
            dvi_right(width);
            dvi_rule(width, height);
            dvi_right(-width);
        }
        _ => {}
    };
}
#[no_mangle]
pub unsafe extern "C" fn dvi_push() {
    if dvi_stack_depth as u32 >= 256u32 {
        panic!("DVI stack exceeded limit.");
    }
    let fresh21 = dvi_stack_depth;
    dvi_stack_depth = dvi_stack_depth + 1;
    dvi_stack[fresh21 as usize] = dvi_state;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_dvi_pop() {
    if dvi_stack_depth <= 0i32 {
        panic!("Tried to pop an empty stack.");
    }
    dvi_stack_depth -= 1;
    dvi_state = dvi_stack[dvi_stack_depth as usize];
    do_moveto(dvi_state.h, dvi_state.v);
    pdf_dev_set_dirmode(dvi_state.d as i32);
    /* 0: horizontal, 1,3: vertical */
}
#[no_mangle]
pub unsafe extern "C" fn dvi_w(mut ch: i32) {
    dvi_state.w = ch;
    dvi_right(ch);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_w0() {
    dvi_right(dvi_state.w);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_x(mut ch: i32) {
    dvi_state.x = ch;
    dvi_right(ch);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_x0() {
    dvi_right(dvi_state.x);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_y(mut ch: i32) {
    dvi_state.y = ch;
    dvi_down(ch);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_y0() {
    dvi_down(dvi_state.y);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_z(mut ch: i32) {
    dvi_state.z = ch;
    dvi_down(ch);
}
#[no_mangle]
pub unsafe extern "C" fn dvi_z0() {
    dvi_down(dvi_state.z);
}
unsafe fn skip_fntdef() {
    let handle = dvi_handle.as_mut().unwrap();
    tt_skip_bytes(12_u32, handle);
    let area_len = tt_get_unsigned_byte(handle) as i32;
    let name_len = tt_get_unsigned_byte(handle) as i32;
    tt_skip_bytes((area_len + name_len) as u32, handle);
}
/* when pre-scanning the page, we process fntdef
and remove the fntdef opcode from the buffer */
unsafe fn do_fntdef(mut tex_id: u32) {
    if linear != 0 {
        read_font_record(tex_id);
    } else {
        skip_fntdef();
    }
    DVI_PAGE_BUF_INDEX -= 1;
}
#[no_mangle]
pub unsafe extern "C" fn dvi_set_font(mut font_id: i32) {
    current_font = font_id;
}
unsafe fn do_fnt(mut tex_id: u32) {
    let mut i = 0;
    while i < num_def_fonts {
        if (*def_fonts.offset(i as isize)).tex_id == tex_id {
            break;
        }
        i += 1;
    }
    if i == num_def_fonts {
        panic!(
            "Tried to select a font that hasn\'t been defined: id={}",
            tex_id
        );
    }
    if (*def_fonts.offset(i as isize)).used == 0 {
        let font_id = if (*def_fonts.offset(i as isize)).native != 0 {
            dvi_locate_native_font(
                (*def_fonts.offset(i as isize)).font_name,
                (*def_fonts.offset(i as isize)).face_index,
                (*def_fonts.offset(i as isize)).point_size,
                (*def_fonts.offset(i as isize)).layout_dir,
                (*def_fonts.offset(i as isize)).extend,
                (*def_fonts.offset(i as isize)).slant,
                (*def_fonts.offset(i as isize)).embolden,
            ) as u32
        } else {
            dvi_locate_font(
                (*def_fonts.offset(i as isize)).font_name,
                (*def_fonts.offset(i as isize)).point_size,
            )
        };
        (*loaded_fonts.offset(font_id as isize)).rgba_color =
            (*def_fonts.offset(i as isize)).rgba_color;
        (*loaded_fonts.offset(font_id as isize)).source = 1i32;
        (*def_fonts.offset(i as isize)).used = 1i32;
        (*def_fonts.offset(i as isize)).font_id = font_id as i32
    }
    current_font = (*def_fonts.offset(i as isize)).font_id;
}
unsafe fn do_xxx(mut size: i32) {
    if lr_mode < 2i32 {
        dvi_do_special(
            &DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX+size as usize]
        );
    }
    DVI_PAGE_BUF_INDEX += size as usize;
}
unsafe fn do_bop() {
    if processing_page != 0 {
        panic!("Got a bop in the middle of a page!");
    }
    /* For now, ignore TeX's count registers */
    for _ in 0..10 {
        DVI_PAGE_BUF_INDEX += 4;
    }
    /* Ignore previous page pointer since we have already
     * saved this information
     */
    DVI_PAGE_BUF_INDEX += 4;
    clear_state();
    processing_page = 1i32;
    pdf_doc_begin_page(dvi_tell_mag(), dev_origin_x, dev_origin_y);
    spc_exec_at_begin_page();
}
unsafe fn do_eop() {
    processing_page = 0i32;
    if dvi_stack_depth != 0i32 {
        panic!("DVI stack depth is not zero at end of page");
    }
    spc_exec_at_end_page();
    pdf_doc_end_page();
}
unsafe fn do_dir() {
    dvi_state.d = get_buffered_unsigned_byte() as u32;
    pdf_dev_set_dirmode(dvi_state.d as i32);
    /* 0: horizontal, 1,3: vertical */
}
unsafe fn lr_width_push() {
    if lr_width_stack_depth >= 256u32 {
        panic!("Segment width stack exceeded limit.");
        /* must precede dvi_right */
    }
    let fresh22 = lr_width_stack_depth;
    lr_width_stack_depth = lr_width_stack_depth.wrapping_add(1);
    lr_width_stack[fresh22 as usize] = lr_width;
}
unsafe fn lr_width_pop() {
    if lr_width_stack_depth <= 0_u32 {
        panic!("Tried to pop an empty segment width stack.");
    }
    lr_width_stack_depth -= 1;
    lr_width = lr_width_stack[lr_width_stack_depth as usize];
}
unsafe fn dvi_begin_reflect() {
    if lr_mode >= 2i32 {
        lr_mode += 1
    } else {
        lr_state.buf_index = DVI_PAGE_BUF_INDEX as u32;
        lr_state.font = current_font;
        lr_state.state = lr_mode;
        lr_mode = 2i32;
        lr_width = 0_u32
    };
}
unsafe fn dvi_end_reflect() {
    match lr_mode {
        2 => {
            current_font = lr_state.font;
            DVI_PAGE_BUF_INDEX = lr_state.buf_index as usize;
            lr_mode = 0i32 + 1i32 - lr_state.state;
            dvi_right(-(lr_width as i32));
            lr_width_push();
        }
        0 | 1 => {
            lr_width_pop();
            dvi_right(-(lr_width as i32));
            lr_mode = 0i32 + 1i32 - lr_mode
        }
        _ => {
            /* lr_mode > SKIMMING */
            lr_mode -= 1
        }
    }; /* skip point size */
}
unsafe fn skip_native_font_def() {
    let handle = dvi_handle.as_mut().unwrap();
    tt_skip_bytes(4, handle);
    let flags = tt_get_unsigned_pair(handle) as u32;
    let name_length = tt_get_unsigned_byte(handle) as i32;
    tt_skip_bytes((name_length + 4) as u32, handle);
    if flags & 0x200_u32 != 0 {
        tt_skip_bytes(4, handle);
    }
    if flags & 0x1000_u32 != 0 {
        tt_skip_bytes(4, handle);
    }
    if flags & 0x2000_u32 != 0 {
        tt_skip_bytes(4, handle);
    }
    if flags & 0x4000_u32 != 0 {
        tt_skip_bytes(4, handle);
    };
}
unsafe fn do_native_font_def(mut tex_id: i32) {
    if linear != 0 {
        read_native_font_record(tex_id as u32);
    } else {
        skip_native_font_def();
    }
    DVI_PAGE_BUF_INDEX -= 1;
    /* don't buffer the opcode */
}
unsafe fn skip_glyphs() {
    /* Will actually move left */
    let slen = get_buffered_unsigned_pair(); /* freetype glyph index */
    for _ in 0..slen {
        DVI_PAGE_BUF_INDEX += 4;
        DVI_PAGE_BUF_INDEX += 4;
        DVI_PAGE_BUF_INDEX += 2;
    }
}
unsafe fn do_glyphs(mut do_actual_text: i32) {
    let mut glyph_width: spt_t = 0i32;
    if current_font < 0i32 {
        panic!("No font selected!");
    }
    let font = &mut *loaded_fonts.offset(current_font as isize) as *mut loaded_font;
    if do_actual_text != 0 {
        let slen = get_buffered_unsigned_pair();
        if lr_mode >= 2i32 {
            for _ in 0..slen {
                DVI_PAGE_BUF_INDEX += 2;
            }
        } else {
            let mut unicodes: *mut u16 = new((slen as u64)
                .wrapping_mul(::std::mem::size_of::<u16>() as u64)
                as u32) as *mut u16;
            for i in 0..slen {
                *unicodes.offset(i as isize) = get_buffered_unsigned_pair();
            }
            pdf_dev_begin_actualtext(unicodes, slen as i32);
            free(unicodes as *mut libc::c_void);
        }
    }
    let width = get_buffered_signed_quad();
    if lr_mode >= 2i32 {
        lr_width = (lr_width as u32).wrapping_add(width as u32) as u32;
        skip_glyphs();
        return;
    }
    if lr_mode == 1i32 {
        dvi_right(width);
    }
    let slen = get_buffered_unsigned_pair();
    let xloc =
        new((slen as u64).wrapping_mul(::std::mem::size_of::<spt_t>() as u64) as u32) as *mut spt_t;
    let yloc =
        new((slen as u64).wrapping_mul(::std::mem::size_of::<spt_t>() as u64) as u32) as *mut spt_t;
    for i in 0..slen {
        *xloc.offset(i as isize) = get_buffered_signed_quad();
        *yloc.offset(i as isize) = get_buffered_signed_quad();
    }
    if (*font).rgba_color != 0xffffffffu32 {
        let mut color = PdfColor::from_rgb(
            (((*font).rgba_color >> 24i32) as u8 as i32 & 0xffi32) as f64 / 255i32 as f64,
            (((*font).rgba_color >> 16i32) as u8 as i32 & 0xffi32) as f64 / 255i32 as f64,
            (((*font).rgba_color >> 8i32) as u8 as i32 & 0xffi32) as f64 / 255i32 as f64,
        )
        .unwrap();
        let color_clone = color.clone();
        pdf_color_push(&mut color, &color_clone);
    }
    for i in 0..slen {
        let mut glyph_id = get_buffered_unsigned_pair();
        if (glyph_id as u32) < (*font).numGlyphs {
            let advance;
            let mut ascent: f64 = (*font).ascent as f64;
            let mut descent: f64 = (*font).descent as f64;
            if !(*font).cffont.is_null() {
                let mut cstrings: *mut cff_index = (*(*font).cffont).cstrings;
                let mut gm = t1_ginfo::new();
                /* If .notdef is not the 1st glyph in CharStrings, glyph_id given by
                FreeType should be increased by 1 */
                if (*(*font).cffont).is_notdef_notzero != 0 {
                    glyph_id += 1;
                }
                t1char_get_metrics(
                    (*cstrings)
                        .data
                        .offset(*(*cstrings).offset.offset(glyph_id as isize) as isize)
                        .offset(-1),
                    (*(*cstrings)
                        .offset
                        .offset((glyph_id + 1) as isize))
                    .wrapping_sub(*(*cstrings).offset.offset(glyph_id as isize))
                        as i32,
                    *(*(*font).cffont).subrs.offset(0),
                    &mut gm,
                );
                advance = (if (*font).layout_dir == 0i32 {
                    gm.wx
                } else {
                    gm.wy
                }) as u32;
                ascent = gm.bbox.ury;
                descent = gm.bbox.lly
            } else {
                advance = (*(*font).hvmt.offset(glyph_id as isize)).advance as u32
            }
            glyph_width =
                ((*font).size as f64 * advance as f64 / (*font).unitsPerEm as f64) as spt_t;
            glyph_width = (glyph_width as f32 * (*font).extend) as spt_t;
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let height = ((*font).size as f64 * ascent / (*font).unitsPerEm as f64) as spt_t;
                let depth = ((*font).size as f64 * -descent / (*font).unitsPerEm as f64) as spt_t;
                pdf_dev_set_rect(
                    &mut rect,
                    dvi_state.h + *xloc.offset(i as isize),
                    -dvi_state.v - *yloc.offset(i as isize),
                    glyph_width,
                    height,
                    depth,
                );
                pdf_doc_expand_box(&mut rect);
            }
        }
        let wbuf = glyph_id.to_be_bytes();
        pdf_dev_set_string(
            dvi_state.h + *xloc.offset(i as isize),
            -dvi_state.v - *yloc.offset(i as isize),
            wbuf.as_ptr() as *const libc::c_void,
            2i32 as size_t,
            glyph_width,
            (*font).font_id,
            -1i32,
        );
    }
    if (*font).rgba_color != 0xffffffffu32 {
        pdf_color_pop();
    }
    free(xloc as *mut libc::c_void);
    free(yloc as *mut libc::c_void);
    if do_actual_text != 0 {
        pdf_dev_end_actualtext();
    }
    if lr_mode == 0i32 {
        dvi_right(width);
    };
}
unsafe fn check_postamble() {
    let handle = dvi_handle.as_mut().unwrap();
    tt_skip_bytes(28_u32, handle);
    loop {
        let code = tt_get_unsigned_byte(handle) as i32;
        if !(code != 249i32) {
            break;
        }
        match code {
            243 | 244 | 245 | 246 => {
                tt_skip_bytes((code + 1i32 - 243i32) as u32, handle);
                skip_fntdef();
            }
            252 => {
                tt_skip_bytes(4_u32, handle);
                skip_native_font_def();
            }
            _ => {
                panic!("Unexpected op code ({}) in postamble", code);
            }
        }
    }
    tt_skip_bytes(4_u32, handle);
    post_id_byte = tt_get_unsigned_byte(handle) as i32;
    if !(post_id_byte == 2i32
        || post_id_byte == 3i32
        || post_id_byte == 7i32
        || post_id_byte == 6i32)
    {
        info!("DVI ID = {}\n", post_id_byte);
        panic!(invalid_signature);
    }
    check_id_bytes();
    if has_ptex != 0 && post_id_byte != 3i32 {
        panic!("Saw opcode {} in DVI file not for Ascii pTeX", 255i32,);
    }
    num_pages = 0_u32;
    /* force loop to terminate */
}
/* Most of the work of actually interpreting
 * the dvi file is here.
 */
#[no_mangle]
pub unsafe extern "C" fn dvi_do_page(
    mut page_paper_height: f64,
    mut hmargin: f64,
    mut vmargin: f64,
) {
    /* before this is called, we have scanned the page for papersize specials
    and the complete DVI data is now in DVI_PAGE_BUFFER */
    DVI_PAGE_BUF_INDEX = 0;
    /* DVI coordinate */
    dev_origin_x = hmargin;
    dev_origin_y = page_paper_height - vmargin;
    dvi_stack_depth = 0i32;
    loop {
        let opcode = get_buffered_unsigned_byte();
        if opcode as i32 <= 127i32 {
            dvi_set(opcode as i32);
        } else if opcode as i32 >= 171i32 && opcode as i32 <= 234i32 {
            do_fnt((opcode as i32 - 171i32) as u32);
        } else {
            let mut current_block_59: u64;
            match opcode as i32 {
                128 | 129 | 130 => {
                    dvi_set(get_buffered_unsigned_num((opcode as i32 - 128i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                131 => {
                    panic!("Multibyte (>24 bits) character not supported!");
                }
                132 => {
                    do_setrule();
                    current_block_59 = 6471821049853688503;
                }
                133 | 134 | 135 => {
                    dvi_put(get_buffered_unsigned_num((opcode as i32 - 133i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                136 => {
                    panic!("Multibyte (>24 bits) character not supported!");
                }
                137 => {
                    do_putrule();
                    current_block_59 = 6471821049853688503;
                }
                139 => {
                    do_bop();
                    current_block_59 = 6471821049853688503;
                }
                140 => {
                    do_eop();
                    if linear != 0 {
                        let handle = dvi_handle.as_mut().unwrap();
                        let opcode = tt_get_unsigned_byte(handle);
                        if opcode as i32 == 248i32 {
                            check_postamble();
                        } else {
                            ttstub_input_ungetc(handle, opcode as i32);
                        }
                    }
                    return;
                }
                141 => {
                    dvi_push();
                    if lr_mode >= 2i32 {
                        lr_width_push();
                    }
                    /* If we are here, we have an opcode that is something
                     * other than SET_CHAR.
                     */
                    /* The following line needs to go here instead of in
                     * dvi_push() since logical structure of document is
                     * oblivous to virtual fonts. For example the last line on a
                     * page could be at stack level 3 and the page footer should
                     * be at stack level 3.  However, if the page footer contains
                     * virtual fonts (or other nested constructions), it could
                     * fool the link breaker into thinking it was a continuation
                     * of the link */
                    dvi_mark_depth();
                    current_block_59 = 6471821049853688503;
                }
                142 => {
                    dpx_dvi_pop();
                    if lr_mode >= 2i32 {
                        lr_width_pop();
                    }
                    /* Above explanation holds for following line too */
                    dvi_mark_depth();
                    current_block_59 = 6471821049853688503;
                }
                143 | 144 | 145 | 146 => {
                    dvi_right(get_buffered_signed_num((opcode as i32 - 143i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                147 => {
                    dvi_w0();
                    current_block_59 = 6471821049853688503;
                }
                148 | 149 | 150 | 151 => {
                    dvi_w(get_buffered_signed_num((opcode as i32 - 148i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                152 => {
                    dvi_x0();
                    current_block_59 = 6471821049853688503;
                }
                153 | 154 | 155 | 156 => {
                    dvi_x(get_buffered_signed_num((opcode as i32 - 153i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                157 | 158 | 159 | 160 => {
                    dvi_down(get_buffered_signed_num((opcode as i32 - 157i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                161 => {
                    dvi_y0();
                    current_block_59 = 6471821049853688503;
                }
                162 | 163 | 164 | 165 => {
                    dvi_y(get_buffered_signed_num((opcode as i32 - 162i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                166 => {
                    dvi_z0();
                    current_block_59 = 6471821049853688503;
                }
                167 | 168 | 169 | 170 => {
                    dvi_z(get_buffered_signed_num((opcode as i32 - 167i32) as u8));
                    current_block_59 = 6471821049853688503;
                }
                235 | 236 | 237 | 238 => {
                    do_fnt(get_buffered_unsigned_num((opcode as i32 - 235i32) as u8) as u32);
                    current_block_59 = 6471821049853688503;
                }
                239 | 240 | 241 | 242 => {
                    /* Specials */
                    let mut size: i32 = get_buffered_unsigned_num((opcode as i32 - 239i32) as u8);
                    if size < 0i32 {
                        warn!("DVI: Special with {} bytes???", size);
                    } else {
                        do_xxx(size);
                    }
                    current_block_59 = 6471821049853688503;
                }
                138 | 243 | 244 | 245 | 246 => {
                    current_block_59 = 6471821049853688503;
                }
                255 => {
                    /* pTeX extension */
                    need_pTeX(opcode as i32);
                    do_dir();
                    current_block_59 = 6471821049853688503;
                }
                253 => {
                    /* XeTeX extensions */
                    need_XeTeX(opcode as i32);
                    do_glyphs(0i32);
                    current_block_59 = 6471821049853688503;
                }
                254 => {
                    need_XeTeX(opcode as i32);
                    do_glyphs(1i32);
                    current_block_59 = 6471821049853688503;
                }
                252 => {
                    /* should not occur - processed during pre-scanning */
                    need_XeTeX(opcode as i32);
                    current_block_59 = 6471821049853688503;
                }
                250 => {
                    need_XeTeX(opcode as i32);
                    dvi_begin_reflect();
                    current_block_59 = 6471821049853688503;
                }
                251 => {
                    need_XeTeX(opcode as i32);
                    dvi_end_reflect();
                    current_block_59 = 6471821049853688503;
                }
                248 => {
                    if linear as i32 != 0 && processing_page == 0 {
                        /* for linear processing, this means there are no more pages */
                        num_pages = 0_u32; /* force loop to terminate */
                        return;
                    }
                    current_block_59 = 17253953464124104480;
                }
                247 | 249 => {
                    current_block_59 = 17253953464124104480;
                }
                _ => {
                    panic!("Unexpected opcode or DVI file ended prematurely");
                }
            }
            match current_block_59 {
                17253953464124104480 =>
                /* else fall through to error case */
                {
                    panic!("Unexpected preamble or postamble in dvi file");
                }
                _ =>
                    /* These should not occur - processed during pre-scanning */
                    {}
            }
        }
    }
}
#[no_mangle]
pub unsafe extern "C" fn dvi_init(mut dvi_filename: *const i8, mut mag: f64) -> f64 {
    if dvi_filename.is_null() {
        panic!("filename must be specified");
    }
    dvi_handle = ttstub_input_open(dvi_filename, TTInputFormat::BINARY, 0i32);
    if dvi_handle.is_none() {
        panic!("cannot open \"{}\"", CStr::from_ptr(dvi_filename).display());
    }
    /* DVI files are most easily read backwards by searching for post_post and
     * then post opcode.
     */
    let post_location = find_post();
    get_dvi_info(post_location);
    do_scales(mag);
    get_page_info(post_location);
    get_comment();
    get_dvi_fonts(post_location);
    clear_state();
    DVI_PAGE_BUFFER = Vec::with_capacity(0x10000);
    dvi2pts
}
#[no_mangle]
pub unsafe extern "C" fn dvi_close() {
    if linear != 0 {
        /* probably reading a pipe from xetex; consume any remaining data */
        while ttstub_input_getc(dvi_handle.as_mut().unwrap()) != -1i32 {}
    }
    /* We add comment in dvi_close instead of dvi_init so user
     * has a change to overwrite it.  The docinfo dictionary is
     * treated as a write-once record.
     */
    /* Do some house cleaning */
    ttstub_input_close(dvi_handle.take().unwrap());
    if !def_fonts.is_null() {
        for i in 0..num_def_fonts {
            let ref mut fresh23 = (*def_fonts.offset(i as isize)).font_name;
            *fresh23 =
                mfree((*def_fonts.offset(i as isize)).font_name as *mut libc::c_void) as *mut i8;
        }
        free(def_fonts as *mut libc::c_void);
    }
    def_fonts = 0 as *mut font_def;
    page_loc = mfree(page_loc as *mut libc::c_void) as *mut u32;
    num_pages = 0_u32;
    for i in 0..num_loaded_fonts {
        free((*loaded_fonts.offset(i as isize)).hvmt as *mut libc::c_void);
        let ref mut fresh24 = (*loaded_fonts.offset(i as isize)).hvmt;
        *fresh24 = 0 as *mut tt_longMetrics;
        if !(*loaded_fonts.offset(i as isize)).cffont.is_null() {
            cff_close((*loaded_fonts.offset(i as isize)).cffont);
        }
        let ref mut fresh25 = (*loaded_fonts.offset(i as isize)).cffont;
        *fresh25 = 0 as *mut cff_font;
    }
    loaded_fonts = mfree(loaded_fonts as *mut libc::c_void) as *mut loaded_font;
    num_loaded_fonts = 0_u32;
    vf_close_all_fonts();
    tfm_close_all();
    if !DVI_PAGE_BUFFER.is_empty() {
        DVI_PAGE_BUFFER = Vec::new();
    };
}
/* The following are need to implement virtual fonts
According to documentation, the vf "subroutine"
must have state pushed and must have
w,v,y, and z set to zero.  The current font
is determined by the virtual font header, which
may be undefined */
static mut saved_dvi_font: [i32; 16] = [0; 16];
static mut num_saved_fonts: u32 = 0_u32;
#[no_mangle]
pub unsafe extern "C" fn dvi_vf_init(mut dev_font_id: i32) {
    dvi_push();
    dvi_state.w = 0i32;
    dvi_state.x = 0i32;
    dvi_state.y = 0i32;
    dvi_state.z = 0i32;
    /* do not reset dvi_state.d. */
    if num_saved_fonts < 16u32 {
        let fresh26 = num_saved_fonts;
        num_saved_fonts = num_saved_fonts.wrapping_add(1);
        saved_dvi_font[fresh26 as usize] = current_font
    } else {
        panic!("Virtual fonts nested too deeply!");
    }
    current_font = dev_font_id;
}
/* After VF subroutine is finished, we simply pop the DVI stack */
#[no_mangle]
pub unsafe extern "C" fn dvi_vf_finish() {
    dpx_dvi_pop();
    if num_saved_fonts > 0_u32 {
        num_saved_fonts -= 1;
        current_font = saved_dvi_font[num_saved_fonts as usize]
    } else {
        panic!("Tried to pop an empty font stack");
    };
}
/* Scan various specials */
/* This need to allow 'true' prefix for unit and
 * length value must be divided by current magnification.
 */
/* XXX: there are four quasi-redundant versions of this; grp for K_UNIT__PT */
pub trait ReadLength {
    fn read_length(&mut self, mag: f64) -> Result<f64, ()>;
    fn read_length_no_mag(&mut self) -> Result<f64, ()>;
}
impl ReadLength for &[u8] {
    fn read_length(&mut self, mag: f64) -> Result<f64, ()> {
        let mut p = *self; /* inverse magnify */
        let mut u: f64 = 1.0f64;
        let mut error: i32 = 0i32;
        let q = p.parse_float_decimal();
        if q.is_none() {
            *self = p;
            return Err(());
        }
        let v = unsafe { atof(q.unwrap().as_ptr()) };
        p.skip_white();
        if let Some(q) = p.parse_c_ident() {
            let mut bytes = q.to_bytes();
            if bytes.starts_with(b"true") {
                u /= if mag != 0.0f64 { mag } else { 1.0f64 };
                bytes = &bytes[b"true".len()..];
            }
            let q = if bytes.is_empty() {
                /* "true" was a separate word from the units */
                p.skip_white();
                p.parse_c_ident()
            } else {
                Some(CString::new(bytes).unwrap())
            };
            if let Some(ident) = q {
                match ident.to_bytes() {
                    b"pt" => u *= 72. / 72.27,
                    b"in" => u *= 72.,
                    b"cm" => u *= 72. / 2.54,
                    b"mm" => u *= 72. / 25.4,
                    b"bp" => u *= 1.,
                    b"pc" => u *= 12. * 72. / 72.27,
                    b"dd" => u *= 1238. / 1157. * 72. / 72.27,
                    b"cc" => u *= 12. * 1238. / 1157. * 72. / 72.27,
                    b"sp" => u *= 72. / (72.27 * 65536.),
                    _ => {
                        warn!("Unknown unit of measure: {}", ident.display(),);
                        error = -1i32
                    }
                }
            } else {
                warn!("Missing unit of measure after \"true\"");
                error = -1i32
            }
        }
        *self = p;
        if error == 0 {
            Ok( v * u )
        } else {
            Err(())
        }
    }
    fn read_length_no_mag(&mut self) -> Result<f64, ()> {
        self.read_length(1.)
    }
}

unsafe fn scan_special(
    mut wd: *mut f64,
    mut ht: *mut f64,
    mut xo: *mut f64,
    mut yo: *mut f64,
    mut lm: *mut i32,
    mut majorversion: *mut i32,
    mut minorversion: *mut i32,
    mut do_enc: *mut i32,
    mut key_bits: *mut i32,
    mut permission: *mut i32,
    mut owner_pw: *mut i8,
    mut user_pw: *mut i8,
    mut buf: &[u8],
) -> i32 {
    let mut ns_pdf: i32 = 0i32;
    let mut ns_dvipdfmx: i32 = 0i32;
    let mut error: i32 = 0i32;
    buf.skip_white();
    let mut q = buf.parse_c_ident();
    if let Some(ident) = q.as_ref() {
        match ident.to_bytes() {
            b"pdf" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                    ns_pdf = 1;
                }
            },
            b"x" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                }
            },
            b"dvipdfmx" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                    ns_dvipdfmx = 1
                }
            },
            _ => {},
        }
    }
    buf.skip_white();
    if let Some(q) = q {
        if q.to_bytes() == b"landscape" {
            *lm = 1
        } else if ns_pdf != 0 && q.to_bytes() == b"pagesize" {
            while error == 0 && !buf.is_empty() {
                if let Some(kp) = buf.parse_c_ident() {
                    buf.skip_white();
                    match kp.to_bytes() {
                        b"width" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *wd = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        },
                        b"height" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *ht = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        },
                        b"xoffset" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *xo = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        },
                        b"yoffset" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *yo = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        },
                        b"default" => {
                            *wd = paper_width;
                            *ht = paper_height;
                            *lm = landscape_mode;
                            *yo = 72.0f64;
                            *xo = *yo
                        },
                        _ => {},
                    }
                    buf.skip_white();
                } else {
                    break;
                }
            }
        } else if q.to_bytes() == b"papersize" {
            let mut qchr = 0_u8;
            if buf[0] == b'=' {
                buf = &buf[1..];
            }
            buf.skip_white();
            if !buf.is_empty() && (buf[0] == b'\'' || buf[0] == b'\"') {
                qchr = buf[0];
                buf = &buf[1..];
                buf.skip_white();
            }
            if let Ok(tmp) = buf.read_length(1.) {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b',' {
                    buf = &buf[1..];
                    buf.skip_white();
                }
                if let Ok(tmp1) = buf.read_length(1.) {
                    *wd = tmp;
                    *ht = tmp1;
                    buf.skip_white();
                } else {
                    error = -1;
                }
            } else {
                error = -1;
            }
            if error == 0 && qchr != 0 {
                /* Check if properly quoted */
                if buf.is_empty() || buf[0] != qchr {
                    error = -1i32
                }
            }
            if error == 0i32 {
                paper_width = *wd;
                paper_height = *ht
            }
        } else if !minorversion.is_null()
            && ns_pdf != 0
            && q.to_bytes() == b"minorversion"
        {
            if buf[0] == b'=' {
                buf = &buf[1..];
            }
            buf.skip_white();
            if let Some(kv) = buf.parse_float_decimal() {
                *minorversion = strtol(kv.as_ptr(), 0 as *mut *mut i8, 10i32) as i32;
            }
        } else if !majorversion.is_null()
            && ns_pdf != 0
            && q.to_bytes() == b"majorversion"
        {
            if buf[0] == b'=' {
                buf = &buf[1..];
            }
            buf.skip_white();
            if let Some(kv_0) = buf.parse_float_decimal() {
                *majorversion = strtol(kv_0.as_ptr(), 0 as *mut *mut i8, 10) as i32;
            }
        } else if ns_pdf != 0
            && q.to_bytes() == b"encrypt"
            && !do_enc.is_null()
        {
            *do_enc = 1i32;
            *user_pw = 0_i8;
            *owner_pw = *user_pw;
            while error == 0 && !buf.is_empty() {
                if let Some(kp) = buf.parse_c_ident() {
                    buf.skip_white();
                    match kp.to_bytes() {
                        b"ownerpw" => {
                            if let Some(obj) = buf.parse_pdf_string() {
                                if !pdf_string_value(obj).is_null() {
                                    strncpy(owner_pw, pdf_string_value(obj) as *const i8, 127);
                                }
                                pdf_release_obj(obj);
                            } else {
                                error = -1i32
                            }
                        },
                        b"userpw" => {
                            if let Some(obj) = buf.parse_pdf_string() {
                                if !pdf_string_value(obj).is_null() {
                                    strncpy(user_pw, pdf_string_value(obj) as *const i8, 127);
                                }
                                pdf_release_obj(obj);
                            } else {
                                error = -1i32
                            }
                        },
                        b"length" => {
                            if let Some(obj) = buf.parse_pdf_number() {
                                if (*obj).is_number() {
                                    *key_bits = pdf_number_value(obj) as u32 as i32
                                } else {
                                    error = -1i32
                                }
                                pdf_release_obj(obj);
                            } else {
                                error = -1i32
                            }
                        },
                        b"perm" => {
                            if let Some(obj) = buf.parse_pdf_number() {
                                if (*obj).is_number() {
                                    *permission = pdf_number_value(obj) as u32 as i32
                                } else {
                                    error = -1i32
                                }
                                pdf_release_obj(obj);
                            } else {
                                error = -1i32
                            }
                        },
                        _ => {
                            error = -1i32
                        }
                    }
                    buf.skip_white();
                } else {
                    break;
                }
            }
        } else if ns_dvipdfmx != 0 && q.to_bytes() == b"config"
        {
            warn!("Tectonic does not support `config\' special. Ignored.");
        }
    }
    error
}
static mut buffered_page: i32 = -1i32;
/* returns scale (dvi2pts) */
/* may append .dvi or .xdv to filename */
/* Closes data structures created by dvi_open */
/* Renamed to avoid clash with XeTeX */
#[no_mangle]
pub unsafe extern "C" fn dvi_scan_specials(
    mut page_no: i32,
    mut page_width: *mut f64,
    mut page_height: *mut f64,
    mut x_offset: *mut f64,
    mut y_offset: *mut f64,
    mut landscape: *mut i32,
    mut majorversion: *mut i32,
    mut minorversion: *mut i32,
    mut do_enc: *mut i32,
    mut key_bits: *mut i32,
    mut permission: *mut i32,
    mut owner_pw: *mut i8,
    mut user_pw: *mut i8,
) {
    /* because dvipdfmx wants to scan first page twice! */
    if page_no == buffered_page || num_pages == 0_u32 {
        return;
    }
    buffered_page = page_no;
    DVI_PAGE_BUF_INDEX = 0;
    let handle = dvi_handle.as_mut().unwrap();
    if linear == 0 {
        if page_no as u32 >= num_pages {
            panic!("Invalid page number: {}", page_no);
        }
        let offset = *page_loc.offset(page_no as isize);
        handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    }
    loop {
        let opcode = get_and_buffer_unsigned_byte(handle) as u8;
        if !(opcode as i32 != 140i32) {
            break;
        }
        if opcode as i32 <= 127i32 || opcode as i32 >= 171i32 && opcode as i32 <= 234i32 {
            continue;
        }
        if opcode as i32 == 239i32
            || opcode as i32 == 240i32
            || opcode as i32 == 241i32
            || opcode as i32 == 242i32
        {
            let mut size: u32 = get_and_buffer_unsigned_byte(handle) as u32;
            let mut current_block_14: u64;
            match opcode as i32 {
                242 => {
                    size = size
                        .wrapping_mul(0x100u32)
                        .wrapping_add(get_and_buffer_unsigned_byte(handle) as u32);
                    if size > 0x7fff_u32 {
                        warn!(
                            "Unsigned number starting with {:x} exceeds 0x7fffffff",
                            size,
                        );
                    }
                    current_block_14 = 2922806634731202080;
                }
                241 => {
                    current_block_14 = 2922806634731202080;
                }
                240 => {
                    current_block_14 = 7135116673376365024;
                }
                _ => {
                    current_block_14 = 26972500619410423;
                }
            }
            match current_block_14 {
                2922806634731202080 => {
                    size = size
                        .wrapping_mul(0x100u32)
                        .wrapping_add(get_and_buffer_unsigned_byte(handle) as u32);
                    current_block_14 = 7135116673376365024;
                }
                _ => {}
            }
            match current_block_14 {
                7135116673376365024 => {
                    size = size
                        .wrapping_mul(0x100u32)
                        .wrapping_add(get_and_buffer_unsigned_byte(handle) as u32)
                }
                _ => {}
            }
            DVI_PAGE_BUFFER.resize_with(DVI_PAGE_BUF_INDEX + size as usize, Default::default);
            handle.read(&mut DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX+size as usize])
                .expect("Reading DVI file failed!");
            if scan_special(
                page_width,
                page_height,
                x_offset,
                y_offset,
                landscape,
                majorversion,
                minorversion,
                do_enc,
                key_bits,
                permission,
                owner_pw,
                user_pw,
                &DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..],
            ) != 0
            {
                warn!(
                    "Reading special command failed: \"{}\"",
                    DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..].display(),
                );
            }
            DVI_PAGE_BUF_INDEX += size as usize;
        } else {
            let mut current_block_50: u64;
            /* Skipping... */
            match opcode as i32 {
                139 => {
                    get_and_buffer_bytes(handle, 44_u32); /* width */
                    current_block_50 = 6033931424626438518; /* glyph count */
                }
                138 | 141 | 142 | 147 | 152 | 161 | 166 => {
                    current_block_50 = 6033931424626438518; /* 2 bytes ID + 8 bytes x,y-location per glyph */
                }
                128 | 133 | 143 | 157 | 148 | 153 | 162 | 167 | 235 => {
                    get_and_buffer_bytes(handle, 1_u32); /* utf16 code unit count */
                    current_block_50 = 6033931424626438518; /* 2 bytes per code unit */
                }
                129 | 134 | 144 | 158 | 149 | 154 | 163 | 168 | 236 => {
                    get_and_buffer_bytes(handle, 2_u32); /* width */
                    current_block_50 = 6033931424626438518; /* glyph count */
                }
                130 | 135 | 145 | 159 | 150 | 155 | 164 | 169 | 237 => {
                    get_and_buffer_bytes(handle, 3_u32); /* 2 bytes ID + 8 bytes x,y-location per glyph */
                    current_block_50 = 6033931424626438518;
                }
                131 | 136 | 146 | 160 | 151 | 156 | 165 | 170 | 238 => {
                    get_and_buffer_bytes(handle, 4_u32);
                    current_block_50 = 6033931424626438518;
                }
                132 | 137 => {
                    get_and_buffer_bytes(handle, 8_u32);
                    current_block_50 = 6033931424626438518;
                }
                243 | 244 | 245 | 246 => {
                    do_fntdef(tt_get_unsigned_num(
                        handle,
                        (opcode as i32 - 243i32) as u8,
                    ));
                    current_block_50 = 6033931424626438518;
                }
                253 => {
                    need_XeTeX(opcode as i32);
                    get_and_buffer_bytes(handle, 4_u32);
                    let len = get_and_buffer_unsigned_pair(handle);
                    get_and_buffer_bytes(handle, len.wrapping_mul(10_u32));
                    current_block_50 = 6033931424626438518;
                }
                254 => {
                    need_XeTeX(opcode as i32);
                    let len = get_and_buffer_unsigned_pair(handle);
                    get_and_buffer_bytes(handle, len.wrapping_mul(2_u32));
                    get_and_buffer_bytes(handle, 4_u32);
                    let len = get_and_buffer_unsigned_pair(handle);
                    get_and_buffer_bytes(handle, len.wrapping_mul(10_u32));
                    current_block_50 = 6033931424626438518;
                }
                252 => {
                    need_XeTeX(opcode as i32);
                    do_native_font_def(tt_get_signed_quad(handle));
                    current_block_50 = 6033931424626438518;
                }
                250 | 251 => {
                    need_XeTeX(opcode as i32);
                    current_block_50 = 6033931424626438518;
                }
                255 => {
                    need_pTeX(opcode as i32);
                    get_and_buffer_bytes(handle, 1_u32);
                    current_block_50 = 6033931424626438518;
                }
                248 => {
                    if linear as i32 != 0 && DVI_PAGE_BUF_INDEX == 1 {
                        /* this is actually an indication that we've reached the end of the input */
                        return;
                    }
                    current_block_50 = 1349400641705233371;
                }
                _ => {
                    current_block_50 = 1349400641705233371;
                }
            }
            match current_block_50 {
                1349400641705233371 =>
                /* else fall through to error case */
                /* case PRE: case POST_POST: and others */
                {
                    panic!("Unexpected opcode {}", opcode as i32);
                }
                _ => {}
            }
        }
    }
}
/* spt_t */
/* instantiated in dvipdfmx.c */
#[no_mangle]
pub unsafe extern "C" fn dvi_reset_global_state() {
    buffered_page = -1i32;
    num_def_fonts = 0_u32;
    max_def_fonts = 0_u32;
    compute_boxes = 0i32;
    link_annot = 1i32;
    verbose = 0i32;
    num_loaded_fonts = 0_u32;
    max_loaded_fonts = 0_u32;
}
