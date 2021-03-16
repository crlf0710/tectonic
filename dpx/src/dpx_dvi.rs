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
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use std::io::{Read, Seek, SeekFrom};
use std::ptr;

use crate::bridge::DisplayExt;
use crate::FromBEByteSlice;
use std::ffi::{CStr, CString};

use super::dpx_sfnt::{
    dfont_open, sfnt_find_table_pos, sfnt_locate_table, sfnt_open, sfnt_read_table_directory,
};
use crate::mfree;
use crate::warn;

use super::dpx_dpxfile::{
    dpx_open_dfont_file, dpx_open_opentype_file, dpx_open_truetype_file, dpx_open_type1_file,
};
use super::dpx_dpxutil::{ParseCIdent, ParseFloatDecimal};
use super::dpx_dvipdfmx::{is_xdv, landscape_mode, paper_height, paper_width};
use super::dpx_fontmap::{fontmap, pdf_insert_native_fontmap_record};
use super::dpx_mem::new;
use super::dpx_numbers::{get_positive_quad, get_unsigned_num, skip_bytes, sqxfw, GetFromFile};
use super::dpx_pdfcolor::{pdf_color_pop, pdf_color_push, PdfColor};
use super::dpx_pdfdev::{
    graphics_mode, pdf_dev_begin_actualtext, pdf_dev_end_actualtext, pdf_dev_locate_font,
    pdf_dev_set_dirmode, pdf_dev_set_rect, pdf_dev_set_rule, pdf_dev_set_string,
};
use super::dpx_pdfdoc::{pdf_doc_break_annot, pdf_doc_expand_box, pdf_doc_mut};
use super::dpx_pdfparse::{dump, ParsePdfObj, SkipWhite};
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
use crate::bridge::{ttstub_input_get_size, ReadByte};
use crate::dpx_dvicodes::*;
use crate::dpx_truetype::sfnt_table_info;
use crate::specials::{
    spc_exec_at_begin_page, spc_exec_at_end_page, spc_exec_special, spc_set_verbose,
};

use libc::{atof, free, strncpy, strtol};

use bridge::{InFile, TTInputFormat};

pub(crate) type fixword = i32;
/* quasi-hack to get the primary input */

pub(crate) type spt_t = i32;
use super::dpx_pdfdev::Rect;
/*
 * The section below this line deals with the actual processing of the
 * dvi file.
 *
 * The dvi file processor state is contained in the following variables:
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct dvi_registers {
    pub(crate) h: i32,
    pub(crate) v: i32,
    pub(crate) w: i32,
    pub(crate) x: i32,
    pub(crate) y: i32,
    pub(crate) z: i32,
    pub(crate) d: u32,
}
#[derive(Clone)]
pub(crate) struct font_def {
    pub(crate) tex_id: u32,
    pub(crate) point_size: spt_t,
    pub(crate) design_size: spt_t,
    pub(crate) font_name: String,
    pub(crate) font_id: i32,
    pub(crate) used: i32,
    pub(crate) native: i32,
    pub(crate) rgba_color: u32,
    pub(crate) face_index: u32,
    pub(crate) layout_dir: i32,
    pub(crate) extend: i32,
    pub(crate) slant: i32,
    pub(crate) embolden: i32,
}
#[derive(Clone)]
pub(crate) struct loaded_font {
    pub(crate) type_0: i32,
    pub(crate) font_id: i32,
    pub(crate) subfont_id: i32,
    pub(crate) tfm_id: i32,
    pub(crate) size: spt_t,
    pub(crate) source: i32,
    pub(crate) rgba_color: u32,
    pub(crate) hvmt: Vec<tt_longMetrics>,
    pub(crate) ascent: i32,
    pub(crate) descent: i32,
    pub(crate) unitsPerEm: u32,
    pub(crate) cffont: *mut cff_font,
    pub(crate) numGlyphs: u32,
    pub(crate) layout_dir: i32,
    pub(crate) extend: f32,
    pub(crate) slant: f32,
    pub(crate) embolden: f32,
}

use super::dpx_cff::cff_font;

use super::dpx_cff::cff_index;
use super::dpx_tt_table::tt_longMetrics;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct dvi_header {
    pub(crate) unit_num: u32,
    pub(crate) unit_den: u32,
    pub(crate) mag: u32,
    pub(crate) media_width: u32,
    pub(crate) media_height: u32,
    pub(crate) stackdepth: u32,
    pub(crate) comment: [u8; 257],
}
/* skimming through reflected segment measuring its width */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct dvi_lr {
    pub(crate) state: i32,
    pub(crate) font: i32,
    pub(crate) buf_index: u32,
}

use super::dpx_t1_char::t1_ginfo;

/* Acoid conflict with CHAR ... from <winnt.h>.  */
/* Data Types as described in Apple's TTRefMan */
pub(crate) type Fixed = u32;

/* UTF-32 over U+FFFF -> UTF-16 surrogate pair */
/* Interal Variables */
static mut dvi_handle: Option<InFile> = None;
static mut linear: i8 = 0_i8;
/* set to 1 for strict linear processing of the input */
static mut page_loc: *mut u32 = std::ptr::null_mut();
static mut num_pages: u32 = 0_u32;
static mut dvi_file_size: u32 = 0_u32;
static mut DVI_INFO: dvi_header = dvi_header {
    unit_num: 25400000_u32,
    unit_den: 473628672_u32,
    mag: 1000_u32,
    media_width: 0_u32,
    media_height: 0_u32,
    stackdepth: 0_u32,
    comment: [0; 257],
};
static mut dev_origin_x: f64 = 72.0f64;
static mut dev_origin_y: f64 = 770.0f64;

pub(crate) unsafe fn get_origin(x: i32) -> f64 {
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
static mut loaded_fonts: Vec<loaded_font> = Vec::new();
static mut def_fonts: Vec<font_def> = Vec::new();
static mut compute_boxes: i32 = 0;
static mut link_annot: i32 = 1;
static mut verbose: i32 = 0;
/* 64K should be plenty for most pages */
static mut DVI_PAGE_BUFFER: Vec<u8> = Vec::new();
static mut DVI_PAGE_BUF_INDEX: usize = 0;
/* functions to read numbers from the dvi file and store them in DVI_PAGE_BUFFER */
unsafe fn get_and_buffer_unsigned_byte<R: Read>(handle: &mut R) -> i32 {
    let ch = handle
        .read_byte()
        .unwrap_or_else(|| panic!("File ended prematurely\n"));
    if DVI_PAGE_BUF_INDEX == DVI_PAGE_BUFFER.len() {
        DVI_PAGE_BUFFER.push(ch);
    } else {
        DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX] = ch;
    }
    DVI_PAGE_BUF_INDEX += 1;
    ch as i32
}
unsafe fn get_and_buffer_unsigned_pair<R: Read>(handle: &mut R) -> u32 {
    let mut pair: u32 = get_and_buffer_unsigned_byte(handle) as u32;
    pair = pair << 8 | get_and_buffer_unsigned_byte(handle) as u32;
    pair
}
unsafe fn get_and_buffer_bytes<R: Read>(handle: &mut R, count: u32) {
    DVI_PAGE_BUFFER.resize_with(DVI_PAGE_BUF_INDEX + count as usize, Default::default);
    handle
        .read_exact(&mut DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..])
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
    let pair =
        u16::from_be_byte_slice(&DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX + 2]);
    DVI_PAGE_BUF_INDEX += 2;
    pair
}
unsafe fn get_buffered_signed_quad() -> i32 {
    let quad =
        i32::from_be_byte_slice(&DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX + 4]);
    DVI_PAGE_BUF_INDEX += 4;
    quad
}
unsafe fn get_buffered_signed_num(num: u8) -> i32 {
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
unsafe fn get_buffered_unsigned_num(num: u8) -> i32 {
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

pub(crate) unsafe fn dvi_set_verbose(level: i32) {
    verbose = level;
    subfont_set_verbose(level);
    tfm_set_verbose(level);
    vf_set_verbose(level);
    spc_set_verbose(level);
}

pub(crate) unsafe fn dvi_npages() -> u32 {
    num_pages
}

const invalid_signature: &str = "Something is wrong. Are you sure this is a DVI file?";

static mut pre_id_byte: i32 = 0;
static mut post_id_byte: i32 = 0;
static mut is_ptex: i32 = 0;
static mut has_ptex: i32 = 0;
unsafe fn check_id_bytes() {
    if pre_id_byte != post_id_byte && (pre_id_byte != 2 || post_id_byte != 3) {
        panic!(
            "Inconsistent DVI id_bytes {} (pre) and {} (post)",
            pre_id_byte, post_id_byte
        );
    };
}
unsafe fn need_XeTeX(c: i32) {
    if is_xdv == 0 {
        panic!("DVI opcode {} only valid for XeTeX", c);
    };
}
unsafe fn need_pTeX(c: i32) {
    if is_ptex == 0 {
        panic!("DVI opcode {} only valid for Ascii pTeX", c);
    }
    has_ptex = 1;
}
unsafe fn find_post() -> i32 {
    let handle = dvi_handle.as_mut().unwrap();
    let dvi_size = ttstub_input_get_size(handle) as libc::off_t;
    if dvi_size > 0x7fffffff as libc::off_t {
        panic!("DVI file size exceeds 31-bit");
    }
    dvi_file_size = dvi_size as u32;
    handle.seek(SeekFrom::End(0)).unwrap();
    let mut current = dvi_size as i32;
    let ch: u8 = loop
    /* Scan backwards through PADDING */
    {
        current -= 1;
        handle.seek(SeekFrom::Start(current as u64)).unwrap();
        let ch = handle.read_byte().unwrap();
        if !(ch == PADDING && current > 0) {
            break ch;
        }
    };
    /* file_position now points to last non padding character or
     * beginning of file */
    if dvi_file_size.wrapping_sub(current as u32) < 4
        || current == 0
        || !(ch == DVI_ID || ch == DVIV_ID || ch == XDV_ID || ch == XDV_ID_OLD)
    {
        info!("DVI ID = {}\n", ch);
        panic!("{}", invalid_signature);
    }
    post_id_byte = ch as i32;
    is_xdv = (ch == XDV_ID || ch == XDV_ID_OLD) as i32;
    is_ptex = (ch == DVIV_ID) as i32;
    /* Make sure post_post is really there */
    current -= 5;
    handle.seek(SeekFrom::Start(current as u64)).unwrap();
    let ch = handle.read_byte().unwrap();
    if ch != POST_POST {
        info!("Found {} where post_post opcode should be\n", ch);
        panic!("{}", invalid_signature);
    }
    current = i32::get(handle);
    handle.seek(SeekFrom::Start(current as u64)).unwrap();
    let ch = handle.read_byte().unwrap();
    if ch != POST {
        info!("Found {} where post_post opcode should be\n", ch);
        panic!("{}", invalid_signature);
    }
    /* Finally check the ID byte in the preamble */
    /* An Ascii pTeX DVI file has id_byte DVI_ID in the preamble but DVIV_ID in the postamble. */
    handle.seek(SeekFrom::Start(0)).unwrap();
    let ch = u8::get(handle);
    if ch != PRE {
        info!("Found {} where PRE was expected\n", ch);
        panic!("{}", invalid_signature);
    }
    let ch = u8::get(handle);
    if !(ch == DVI_ID || ch == XDV_ID || ch == XDV_ID_OLD) {
        info!("DVI ID = {}\n", ch);
        panic!("{}", invalid_signature);
    }
    pre_id_byte = ch as i32;
    check_id_bytes();
    current
}
unsafe fn get_page_info(post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle
        .seek(SeekFrom::Start(post_location as u64 + 27))
        .unwrap();
    num_pages = u16::get(handle) as u32;
    if num_pages == 0_u32 {
        panic!("Page count is 0!");
    }
    if verbose > 2 {
        info!("Page count:\t {:4}\n", num_pages);
    }
    page_loc = new((num_pages as u64).wrapping_mul(::std::mem::size_of::<u32>() as u64) as u32)
        as *mut u32;
    handle
        .seek(SeekFrom::Start(post_location as u64 + 1))
        .unwrap();
    *page_loc.offset(num_pages.wrapping_sub(1_u32) as isize) = u32::get(handle);
    if (*page_loc.offset(num_pages.wrapping_sub(1_u32) as isize)).wrapping_add(41_u32)
        > dvi_file_size
    {
        panic!("{}", invalid_signature);
    }
    for i in (0..num_pages - 1).rev() {
        handle
            .seek(SeekFrom::Start(
                *page_loc.offset((i + 1) as isize) as u64 + 41,
            ))
            .unwrap();
        *page_loc.offset(i as isize) = u32::get(handle);
        if (*page_loc.offset(num_pages.wrapping_sub(1_u32) as isize)).wrapping_add(41_u32)
            > dvi_file_size
        {
            panic!("{}", invalid_signature);
        }
    }
}
/* Following are computed "constants" used for unit conversion */
static mut dvi2pts: f64 = 1.52018f64;
static mut total_mag: f64 = 1.0f64;

pub(crate) unsafe fn dvi_tell_mag() -> f64 {
    return total_mag; /* unused */
}
unsafe fn do_scales(mag: f64) {
    total_mag = DVI_INFO.mag as f64 / 1000.0f64 * mag; /* 1.0 */
    dvi2pts = DVI_INFO.unit_num as f64 / DVI_INFO.unit_den as f64; /* font name length */
    dvi2pts *= 72.0f64 / 254000.0f64; /* hard-code as 10pt for now, not used anyway */
}
unsafe fn get_dvi_info(post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle
        .seek(SeekFrom::Start(post_location as u64 + 5))
        .unwrap(); /* direction */
    DVI_INFO.unit_num = u32::get(handle);
    DVI_INFO.unit_den = u32::get(handle);
    DVI_INFO.mag = u32::get(handle);
    DVI_INFO.media_height = u32::get(handle);
    DVI_INFO.media_width = u32::get(handle);
    DVI_INFO.stackdepth = u16::get(handle) as u32;
    if DVI_INFO.stackdepth > 256u32 {
        warn!("DVI need stack depth of {},", DVI_INFO.stackdepth);
        warn!("but DVI_STACK_DEPTH_MAX is {}.", 256u32);
        panic!("Capacity exceeded.");
    }
    if verbose > 2 {
        info!("DVI File Info\n");
        info!("Unit: {} / {}\n", DVI_INFO.unit_num, DVI_INFO.unit_den);
        info!("Magnification: {}\n", DVI_INFO.mag);
        info!("Media Height: {}\n", DVI_INFO.media_height);
        info!("Media Width: {}\n", DVI_INFO.media_width);
        info!("Stack Depth: {}\n", DVI_INFO.stackdepth);
    }
}

pub(crate) unsafe fn dvi_comment() -> &'static [u8] {
    let pos = DVI_INFO.comment.iter().position(|&x| x == 0).unwrap();
    &DVI_INFO.comment[..pos]
}
unsafe fn read_font_record(tex_id: u32) {
    let handle = dvi_handle.as_mut().unwrap();
    u32::get(handle);
    let point_size = get_positive_quad(handle, "DVI", "point_size");
    let design_size = get_positive_quad(handle, "DVI", "design_size");
    let dir_length = u8::get(handle) as usize;
    let name_length = u8::get(handle) as usize;
    let mut directory = vec![0_u8; dir_length];
    handle
        .read_exact(directory.as_mut_slice())
        .expect(invalid_signature);
    let mut font_name = vec![0_u8; name_length];
    handle
        .read_exact(font_name.as_mut_slice())
        .expect(invalid_signature);
    def_fonts.push(font_def {
        font_id: -1,
        tex_id,
        font_name: String::from_utf8(font_name).unwrap(),
        point_size: point_size as spt_t,
        design_size: design_size as spt_t,
        used: 0,
        native: 0,
        rgba_color: 0xffffffffu32,
        face_index: 0_u32,
        layout_dir: 0,
        extend: 0x10000,
        slant: 0,
        embolden: 0,
    });
}
unsafe fn read_native_font_record(tex_id: u32) {
    let handle = dvi_handle.as_mut().unwrap();
    let point_size = get_positive_quad(handle, "DVI", "point_size");
    let flags = u16::get(handle) as u32;
    let len = u8::get(handle) as i32;
    let font_name =
        new(((len + 1) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;

    let slice = std::slice::from_raw_parts_mut(font_name as *mut u8, len as usize);
    handle.read_exact(slice).expect(invalid_signature);
    *font_name.offset(len as isize) = '\u{0}' as i32 as i8;

    let index = get_positive_quad(handle, "DVI", "index");
    let mut font = font_def {
        font_id: -1,
        tex_id: tex_id,
        font_name: CStr::from_ptr(font_name).to_str().unwrap().to_owned(),
        face_index: index,
        point_size: point_size as spt_t,
        design_size: 655360,
        used: 0,
        native: 1,
        layout_dir: 0,
        rgba_color: 0xffffffffu32,
        extend: 0x10000,
        slant: 0,
        embolden: 0,
    };
    if flags & 0x100_u32 != 0 {
        font.layout_dir = 1
    }
    if flags & 0x200_u32 != 0 {
        font.rgba_color = u32::get(handle)
    }
    if flags & 0x1000_u32 != 0 {
        font.extend = i32::get(handle)
    }
    if flags & 0x2000_u32 != 0 {
        font.slant = i32::get(handle)
    }
    if flags & 0x4000_u32 != 0 {
        font.embolden = i32::get(handle)
    }
    def_fonts.push(font);
    free(font_name as *mut _);
}
unsafe fn get_dvi_fonts(post_location: i32) {
    let handle = dvi_handle.as_mut().unwrap();
    handle
        .seek(SeekFrom::Start(post_location as u64 + 29))
        .unwrap();
    loop {
        let code = u8::get(handle);
        if code == POST_POST {
            break;
        }
        match code {
            FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {
                read_font_record(get_unsigned_num(handle, code - FNT_DEF1));
            }
            XDV_NATIVE_FONT_DEF => {
                need_XeTeX(code as i32);
                read_native_font_record(i32::get(handle) as u32);
            }
            _ => {
                info!("Unexpected op code: {:3}\n", code);
                panic!("{}", invalid_signature);
            }
        }
    }
    if verbose > 2 {
        info!("\n");
        info!("DVI file font info\n");
        for font in &def_fonts {
            info!(
                "TeX Font: {:10} loaded at ID={:5}, ",
                font.font_name, font.tex_id,
            );
            info!(
                "size={:5.2}pt (scaled {:4.1}%)",
                font.point_size as f64 * dvi2pts,
                100.0 * (font.point_size as f64 / font.design_size as f64),
            );
            info!("\n");
        }
    }
}
unsafe fn get_comment() {
    let handle = dvi_handle.as_mut().unwrap();
    handle.seek(SeekFrom::Start(14)).unwrap();
    let length = u8::get(handle) as usize;
    handle
        .read_exact(&mut DVI_INFO.comment[..length])
        .expect(invalid_signature);
    DVI_INFO.comment[length as usize] = '\u{0}' as u8;
    if verbose != 0 {
        info!("DVI Comment: {}\n", DVI_INFO.comment[..length].display());
    }
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
static mut dvi_stack_depth: i32 = 0;
static mut current_font: i32 = -1; // TODO: Option<usize> or Option<&loaded_font>
static mut processing_page: i32 = 0;
unsafe fn clear_state() {
    dvi_state = dvi_registers {
        h: 0,
        v: 0,
        w: 0,
        x: 0,
        y: 0,
        z: 0,
        d: 0,
    };
    pdf_dev_set_dirmode(0);
    dvi_stack_depth = 0;
    current_font = -1;
}
/* Migrated from pdfdev.c:
 * The following codes are originally put into pdfdev.c.
 * But they are moved to here to make PDF output independent
 * from DVI input.
 * pdfdoc, pdfspecial and htex are also modified. pdfspecial
 * and htex does tag/untag depth. pdfdev and pdfdoc now does
 * not care about line-breaking at all.
 */
static mut marked_depth: i32 = 0;
static mut tagged_depth: i32 = -1;
unsafe fn dvi_mark_depth() {
    /* If decreasing below tagged_depth */
    if link_annot != 0 && marked_depth == tagged_depth && dvi_stack_depth == tagged_depth - 1 {
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

pub(crate) unsafe fn dvi_tag_depth() {
    tagged_depth = marked_depth;
    dvi_compute_boxes(1);
}

pub(crate) unsafe fn dvi_untag_depth() {
    tagged_depth = -1;
    dvi_compute_boxes(0);
}

pub(crate) unsafe fn dvi_compute_boxes(flag: i32) {
    compute_boxes = flag;
}

pub(crate) unsafe fn dvi_link_annot(flag: i32) {
    link_annot = flag;
}
/* allow other modules (pdfdev) to ask whether we're collecting box areas */

pub(crate) unsafe fn dvi_is_tracking_boxes() -> bool {
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

pub(crate) unsafe fn dvi_do_special(buffer: &[u8]) {
    /* VF or device font ID */
    graphics_mode();
    let x_user = dvi_state.h as f64 * dvi2pts;
    let y_user = -dvi_state.v as f64 * dvi2pts;
    let mag = dvi_tell_mag();
    if spc_exec_special(buffer, x_user, y_user, mag) == crate::dpx_error::ERR {
        if verbose != 0 {
            dump(buffer);
        }
    }
}

pub(crate) unsafe fn dvi_unit_size() -> f64 {
    dvi2pts
}

pub(crate) unsafe fn dvi_locate_font(tfm_name: &str, ptsize: spt_t) -> u32 {
    let mut subfont_id: i32 = -1;
    if verbose != 0 {
        info!("<{}@{:.2}pt", tfm_name, ptsize as f64 * dvi2pts);
    }
    /* This routine needs to be recursive/reentrant. Load current high water
     * mark into an automatic variable.
     */
    let mrec = fontmap.get(tfm_name);
    /* Load subfont mapping table */
    if let Some(mrec) = mrec {
        if !mrec.charmap.sfd_name.is_empty() && !mrec.charmap.subfont_id.is_empty() {
            subfont_id = sfd_load_record(&mrec.charmap.sfd_name, &mrec.charmap.subfont_id)
        }
    }

    let mut new_font = loaded_font {
        /* TFM must exist here. */
        tfm_id: tfm_open(&tfm_name, 1),
        subfont_id,
        size: ptsize,
        /* This will be reset later if it was really generated by the dvi file. */
        source: 2,

        // zero-initialize other fields
        type_0: 0,
        font_id: 0,
        rgba_color: 0,
        hvmt: Vec::new(),
        ascent: 0,
        descent: 0,
        unitsPerEm: 0,
        cffont: ptr::null_mut(),
        numGlyphs: 0,
        layout_dir: 0,
        extend: 0.,
        slant: 0.,
        embolden: 0.,
    };
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
    if let Some(mrec) = mrec {
        if subfont_id >= 0 && !mrec.map_name.is_empty() {
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
            if let Some(mrec1) = fontmap.get(&mrec.map_name) {
                if mrec1.enc_name.is_empty() {
                    let font_id = vf_locate_font(&mrec1.font_name, ptsize);
                    if font_id < 0 {
                        warn!(
                            "Could not locate Omega Virtual Font \"{}\" for \"{}\".",
                            mrec1.font_name, tfm_name,
                        );
                    } else {
                        new_font.type_0 = 2;
                        new_font.font_id = font_id;
                        if verbose != 0 {
                            info!("(OVF)>");
                        }
                        loaded_fonts.push(new_font);
                        return loaded_fonts.len() as u32 - 1;
                    }
                }
            }
        }
    } else {
        let font_id = vf_locate_font(tfm_name, ptsize);
        if font_id >= 0 {
            new_font.type_0 = 2;
            new_font.font_id = font_id;
            if verbose != 0 {
                info!("(VF)>");
            }
            loaded_fonts.push(new_font);
            return loaded_fonts.len() as u32 - 1;
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
    let name = match mrec {
        Some(mrec) if !mrec.map_name.is_empty() => &mrec.map_name,
        _ => tfm_name,
    };
    /* We need ptsize for PK font creation. */
    let font_id = pdf_dev_locate_font(&name, ptsize);
    if font_id < 0 {
        warn!(
            "Could not locate a virtual/physical font for TFM \"{}\".",
            tfm_name
        );
        match mrec {
            Some(mrec) if !mrec.map_name.is_empty() => {
                /* has map_name */
                let mrec1_0 = fontmap.get(&mrec.map_name); // CHECK this is enough
                warn!(">> This font is mapped to an intermediate 16-bit font \"{}\" with SFD charmap=<{},{}>,",
                    mrec.map_name, mrec.charmap.sfd_name,
                    mrec.charmap.subfont_id
                );
                if let Some(mrec1_0) = mrec1_0 {
                    warn!(
                        ">> and then mapped to a physical font \"{}\" by fontmap.",
                        mrec1_0.font_name
                    );
                    warn!(
                        ">> Please check if kpathsea library can find this font: {}",
                        mrec1_0.font_name,
                    );
                } else {
                    warn!(
                        ">> but I couldn\'t find font mapping for \"{}\".",
                        mrec.map_name
                    );
                }
            }
            Some(mrec) => {
                warn!(
                    ">> This font is mapped to a physical font \"{}\".",
                    mrec.font_name
                );
                warn!(
                    ">> Please check if kpathsea library can find this font: {}",
                    mrec.font_name
                );
            }
            None => {
                warn!(">> There are no valid font mapping entry for this font.");
                warn!(
                    ">> Font file name \"{}\" was assumed but failed to locate that font.",
                    tfm_name
                );
            }
        }
        panic!("Cannot proceed without .vf or \"physical\" font for PDF output...");
    }
    new_font.type_0 = 1;
    new_font.font_id = font_id;
    if verbose != 0 {
        info!(">");
    }
    loaded_fonts.push(new_font);
    loaded_fonts.len() as u32 - 1
}
unsafe fn dvi_locate_native_font(
    filename: &str,
    index: u32,
    ptsize: spt_t,
    layout_dir: i32,
    extend: i32,
    slant: i32,
    embolden: i32,
) -> i32 {
    let mut offset: u32 = 0_u32;
    let mut is_dfont: i32 = 0;
    let mut is_type1: i32 = 0;
    if verbose != 0 {
        info!("<{}@{:.2}pt", filename, ptsize as f64 * dvi2pts);
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
                panic!("Cannot proceed without the font: {}", filename);
            }
        }
    }
    let handle = handle.unwrap();

    let fontmap_key = format!(
        "{}/{}/{}/{}/{}/{}",
        filename,
        index,
        if layout_dir == 0 { 'H' } else { 'V' },
        extend,
        slant,
        embolden,
    );
    let mrec = if let Some(mrec) = fontmap.get(&fontmap_key) {
        mrec
    } else {
        pdf_insert_native_fontmap_record(filename, index, layout_dir, extend, slant, embolden)
            .expect(&format!(
                "Failed to insert font record for font: {}",
                filename
            ))
    };
    let mut font = loaded_font {
        font_id: pdf_dev_locate_font(&fontmap_key, ptsize),
        size: ptsize,
        type_0: 4,

        // zero-initialize other fields
        rgba_color: 0,
        hvmt: Vec::new(),
        ascent: 0,
        descent: 0,
        unitsPerEm: 0,
        cffont: ptr::null_mut(),
        numGlyphs: 0,
        layout_dir: 0,
        extend: 0.,
        slant: 0.,
        embolden: 0.,
        subfont_id: 0,
        tfm_id: 0,
        source: 0,
    };
    if is_type1 != 0 {
        let mut enc_vec = vec![String::new(); 256];
        /*if (!is_pfb(fp))
         *  panic!("Failed to read Type 1 font \"{}\".", filename);
         */
        warn!("skipping PFB sanity check -- needs Tectonic I/O update");
        let cffont = t1_load_font(&mut enc_vec[..], 0, handle);
        if cffont.topdict.contains_key("FontBBox") {
            font.ascent = cffont.topdict.get("FontBBox", 3) as i32;
            font.descent = cffont.topdict.get("FontBBox", 1) as i32
        } else {
            font.ascent = 690;
            font.descent = -190
        }
        font.unitsPerEm = 1000_u32;
        font.numGlyphs = cffont.num_glyphs as u32;
        font.cffont = Box::into_raw(cffont);
    } else {
        let mut sfont = if is_dfont != 0 {
            dfont_open(handle, index as i32).unwrap()
        } else {
            sfnt_open(handle)
        };
        if sfont.type_0 == 1 << 4 {
            offset = ttc_read_offset(&mut sfont, index as i32)
        } else if sfont.type_0 == 1 << 8 {
            offset = sfont.offset
        }
        sfnt_read_table_directory(&mut sfont, offset);
        let head = tt_read_head_table(&mut sfont);
        let maxp = tt_read_maxp_table(&mut sfont);
        let hhea = tt_read_hhea_table(&mut sfont);
        font.ascent = hhea.ascent as i32;
        font.descent = hhea.descent as i32;
        font.unitsPerEm = head.unitsPerEm as u32;
        font.numGlyphs = maxp.numGlyphs as u32;
        if layout_dir == 1 && sfnt_find_table_pos(&sfont, b"vmtx") > 0_u32 {
            let vhea = tt_read_vhea_table(&mut sfont);
            sfnt_locate_table(&mut sfont, b"vmtx");
            font.hvmt = tt_read_longMetrics(
                &mut &*sfont.handle,
                maxp.numGlyphs,
                vhea.numOfLongVerMetrics,
                vhea.numOfExSideBearings,
            );
        } else {
            sfnt_locate_table(&mut sfont, sfnt_table_info::HMTX);
            font.hvmt = tt_read_longMetrics(
                &mut &*sfont.handle,
                maxp.numGlyphs,
                hhea.numOfLongHorMetrics,
                hhea.numOfExSideBearings,
            )
        }
    }
    font.layout_dir = layout_dir;
    font.extend = (*mrec).opt.extend as f32;
    font.slant = (*mrec).opt.slant as f32;
    font.embolden = (*mrec).opt.bold as f32;
    if verbose != 0 {
        info!(">");
    }
    loaded_fonts.push(font);
    loaded_fonts.len() as i32 - 1
}

pub(crate) unsafe fn dvi_dev_xpos() -> f64 {
    dvi_state.h as f64 * dvi2pts
}

pub(crate) unsafe fn dvi_dev_ypos() -> f64 {
    -(dvi_state.v as f64 * dvi2pts)
}
unsafe fn do_moveto(x: i32, y: i32) {
    dvi_state.h = x;
    dvi_state.v = y;
}
/* FIXME: dvi_forward() might be a better name */

pub(crate) unsafe fn dvi_right(mut x: i32) {
    if lr_mode >= 2 {
        lr_width = (lr_width as u32).wrapping_add(x as u32) as u32;
        return;
    }
    if lr_mode == 1 {
        x = -x
    }
    match dvi_state.d {
        0 => dvi_state.h += x,
        1 => dvi_state.v += x,
        3 => dvi_state.v -= x,
        _ => {}
    };
}

pub(crate) unsafe fn dvi_down(y: i32) {
    if lr_mode < 2 {
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

pub(crate) unsafe fn dvi_set(ch: i32) {
    if current_font < 0 {
        panic!("No font selected!");
    }
    /* The division by dvi2pts seems strange since we actually know the
     * "dvi" size of the fonts contained in the DVI file.  In other
     * words, we converted from DVI units to pts and back again!
     * The problem comes from fonts defined in VF files where we don't know
     * the DVI size.  It's keeping me sane to keep *point sizes* of *all*
     * fonts in the dev.c file and convert them back if necessary.
     */
    let font = &mut loaded_fonts[current_font as usize]; /* Will actually move left */
    let mut width = tfm_get_fw_width(font.tfm_id, ch);
    width = sqxfw(font.size, width);
    if lr_mode >= 2 {
        lr_width += width as u32;
        return;
    }
    if lr_mode == 1 {
        dvi_right(width);
    }
    match font.type_0 {
        1 => {
            if ch > 65535 {
                /* _FIXME_ */
                let wbuf = [
                    (0xd800 + (ch - 0x10000 >> 10 & 0x3ff) >> 8 & 0xff) as u8,
                    (0xd800 + (ch - 0x10000 >> 10 & 0x3ff) & 0xff) as u8,
                    (0xdc00 + (ch & 0x3ff) >> 8 & 0xff) as u8,
                    (0xdc00 + (ch & 0x3ff) & 0xff) as u8,
                ];
                pdf_dev_set_string(
                    dvi_state.h,
                    -dvi_state.v,
                    &wbuf[..4],
                    width,
                    font.font_id,
                    2,
                );
            } else if ch > 255 {
                /* _FIXME_ */
                /* push/pop invoked */
                let wbuf = [(ch >> 8 & 0xff) as u8, (ch & 0xff) as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 2);
            } else if font.subfont_id >= 0 {
                let uch: u16 = lookup_sfd_record(font.subfont_id, ch as u8);
                let wbuf = [(uch as i32 >> 8 & 0xff) as u8, (uch as i32 & 0xff) as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 2);
            } else {
                let wbuf = [ch as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 1);
            }
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let mut height = tfm_get_fw_height(font.tfm_id, ch);
                let mut depth = tfm_get_fw_depth(font.tfm_id, ch);
                height = sqxfw(font.size, height);
                depth = sqxfw(font.size, depth);
                pdf_dev_set_rect(&mut rect, dvi_state.h, -dvi_state.v, width, height, depth);
                pdf_doc_expand_box(&mut rect);
            }
        }
        2 => {
            vf_set_char(ch, font.font_id);
        }
        _ => {}
    }
    if lr_mode == 0 {
        dvi_right(width);
    };
}

pub(crate) unsafe fn dvi_put(ch: i32) {
    if current_font < 0 {
        panic!("No font selected!");
    }
    let font = &mut loaded_fonts[current_font as usize];
    match font.type_0 {
        1 => {
            let mut width = tfm_get_fw_width(font.tfm_id, ch);
            width = sqxfw(font.size, width);
            /* Treat a single character as a one byte string and use the
             * string routine.
             */
            if ch > 65535 {
                /* _FIXME_ */
                let wbuf = [
                    (0xd800 + (ch - 0x10000 >> 10 & 0x3ff) >> 8 & 0xff) as u8,
                    (0xd800 + (ch - 0x10000 >> 10 & 0x3ff) & 0xff) as u8,
                    (0xdc00 + (ch & 0x3ff) >> 8 & 0xff) as u8,
                    (0xdc00 + (ch & 0x3ff) & 0xff) as u8,
                ];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 2);
            } else if ch > 255 {
                /* _FIXME_ */
                let wbuf = [(ch >> 8 & 0xff) as u8, (ch & 0xff) as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 2);
            } else if font.subfont_id >= 0 {
                let uch = lookup_sfd_record(font.subfont_id, ch as u8) as u32;
                let wbuf = [(uch >> 8 & 0xff_u32) as u8, (uch & 0xff_u32) as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 2);
            } else {
                let wbuf = [ch as u8];
                pdf_dev_set_string(dvi_state.h, -dvi_state.v, &wbuf, width, font.font_id, 1);
            }
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let mut height = tfm_get_fw_height(font.tfm_id, ch);
                let mut depth = tfm_get_fw_depth(font.tfm_id, ch);
                height = sqxfw(font.size, height);
                depth = sqxfw(font.size, depth);
                pdf_dev_set_rect(&mut rect, dvi_state.h, -dvi_state.v, width, height, depth);
                pdf_doc_expand_box(&mut rect);
            }
        }
        2 => {
            vf_set_char(ch, font.font_id);
        }
        _ => {}
    };
}

pub(crate) unsafe fn dvi_rule(width: i32, height: i32) {
    if width > 0 && height > 0 {
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

pub(crate) unsafe fn dvi_dirchg(dir: u8) {
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

pub(crate) unsafe fn dvi_push() {
    if dvi_stack_depth as u32 >= 256 {
        panic!("DVI stack exceeded limit.");
    }
    dvi_stack[dvi_stack_depth as usize] = dvi_state;
    dvi_stack_depth += 1;
}

pub(crate) unsafe fn dpx_dvi_pop() {
    if dvi_stack_depth <= 0 {
        panic!("Tried to pop an empty stack.");
    }
    dvi_stack_depth -= 1;
    dvi_state = dvi_stack[dvi_stack_depth as usize];
    do_moveto(dvi_state.h, dvi_state.v);
    pdf_dev_set_dirmode(dvi_state.d as i32);
    /* 0: horizontal, 1,3: vertical */
}

pub(crate) unsafe fn dvi_w(ch: i32) {
    dvi_state.w = ch;
    dvi_right(ch);
}

pub(crate) unsafe fn dvi_w0() {
    dvi_right(dvi_state.w);
}

pub(crate) unsafe fn dvi_x(ch: i32) {
    dvi_state.x = ch;
    dvi_right(ch);
}

pub(crate) unsafe fn dvi_x0() {
    dvi_right(dvi_state.x);
}

pub(crate) unsafe fn dvi_y(ch: i32) {
    dvi_state.y = ch;
    dvi_down(ch);
}

pub(crate) unsafe fn dvi_y0() {
    dvi_down(dvi_state.y);
}

pub(crate) unsafe fn dvi_z(ch: i32) {
    dvi_state.z = ch;
    dvi_down(ch);
}

pub(crate) unsafe fn dvi_z0() {
    dvi_down(dvi_state.z);
}
unsafe fn skip_fntdef() {
    let handle = dvi_handle.as_mut().unwrap();
    skip_bytes(12_u32, handle);
    let area_len = u8::get(handle) as i32;
    let name_len = u8::get(handle) as i32;
    skip_bytes((area_len + name_len) as u32, handle);
}
/* when pre-scanning the page, we process fntdef
and remove the fntdef opcode from the buffer */
unsafe fn do_fntdef(tex_id: u32) {
    if linear != 0 {
        read_font_record(tex_id);
    } else {
        skip_fntdef();
    }
    DVI_PAGE_BUF_INDEX -= 1;
}

pub(crate) unsafe fn dvi_set_font(font_id: i32) {
    current_font = font_id;
}
unsafe fn do_fnt(tex_id: u32) {
    let font = def_fonts
        .iter_mut()
        .find(|font| font.tex_id == tex_id)
        .expect("Tried to select a font that hasen't been defined");

    if font.used == 0 {
        let font_id = if font.native != 0 {
            dvi_locate_native_font(
                &font.font_name,
                font.face_index,
                font.point_size,
                font.layout_dir,
                font.extend,
                font.slant,
                font.embolden,
            ) as usize
        } else {
            dvi_locate_font(&font.font_name, font.point_size) as usize
        };
        loaded_fonts[font_id].rgba_color = font.rgba_color;
        loaded_fonts[font_id].source = 1;
        font.used = 1;
        font.font_id = font_id as i32
    }
    current_font = font.font_id;
}
unsafe fn do_xxx(size: i32) {
    if lr_mode < 2 {
        dvi_do_special(&DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX + size as usize]);
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
    processing_page = 1;
    pdf_doc_mut().begin_page(dvi_tell_mag(), dev_origin_x, dev_origin_y);
    spc_exec_at_begin_page().ok();
}
unsafe fn do_eop() {
    processing_page = 0;
    if dvi_stack_depth != 0 {
        panic!("DVI stack depth is not zero at end of page");
    }
    spc_exec_at_end_page().ok();
    pdf_doc_mut().end_page();
}
unsafe fn do_dir() {
    dvi_state.d = get_buffered_unsigned_byte() as u32;
    pdf_dev_set_dirmode(dvi_state.d as i32);
    /* 0: horizontal, 1,3: vertical */
}
unsafe fn lr_width_push() {
    if lr_width_stack_depth >= 256 {
        panic!("Segment width stack exceeded limit.");
        /* must precede dvi_right */
    }
    lr_width_stack[lr_width_stack_depth as usize] = lr_width;
    lr_width_stack_depth += 1;
}
unsafe fn lr_width_pop() {
    if lr_width_stack_depth <= 0_u32 {
        panic!("Tried to pop an empty segment width stack.");
    }
    lr_width_stack_depth -= 1;
    lr_width = lr_width_stack[lr_width_stack_depth as usize];
}
unsafe fn dvi_begin_reflect() {
    if lr_mode >= 2 {
        lr_mode += 1
    } else {
        lr_state.buf_index = DVI_PAGE_BUF_INDEX as u32;
        lr_state.font = current_font;
        lr_state.state = lr_mode;
        lr_mode = 2;
        lr_width = 0_u32
    };
}
unsafe fn dvi_end_reflect() {
    match lr_mode {
        2 => {
            current_font = lr_state.font;
            DVI_PAGE_BUF_INDEX = lr_state.buf_index as usize;
            lr_mode = 0 + 1 - lr_state.state;
            dvi_right(-(lr_width as i32));
            lr_width_push();
        }
        0 | 1 => {
            lr_width_pop();
            dvi_right(-(lr_width as i32));
            lr_mode = 0 + 1 - lr_mode
        }
        _ => {
            /* lr_mode > SKIMMING */
            lr_mode -= 1
        }
    }; /* skip point size */
}
unsafe fn skip_native_font_def() {
    let handle = dvi_handle.as_mut().unwrap();
    skip_bytes(4, handle);
    let flags = u16::get(handle) as u32;
    let name_length = u8::get(handle) as i32;
    skip_bytes((name_length + 4) as u32, handle);
    if flags & 0x200_u32 != 0 {
        skip_bytes(4, handle);
    }
    if flags & 0x1000_u32 != 0 {
        skip_bytes(4, handle);
    }
    if flags & 0x2000_u32 != 0 {
        skip_bytes(4, handle);
    }
    if flags & 0x4000_u32 != 0 {
        skip_bytes(4, handle);
    };
}
unsafe fn do_native_font_def(tex_id: i32) {
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
unsafe fn do_glyphs(do_actual_text: i32) {
    let mut glyph_width: spt_t = 0;
    if current_font < 0 {
        panic!("No font selected!");
    }
    let font = &mut loaded_fonts[current_font as usize];
    if do_actual_text != 0 {
        let slen = get_buffered_unsigned_pair();
        if lr_mode >= 2 {
            for _ in 0..slen {
                DVI_PAGE_BUF_INDEX += 2;
            }
        } else {
            let unicodes: *mut u16 = new((slen as u64)
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
    if lr_mode >= 2 {
        lr_width = (lr_width as u32).wrapping_add(width as u32) as u32;
        skip_glyphs();
        return;
    }
    if lr_mode == 1 {
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
    if font.rgba_color != 0xffffffffu32 {
        let mut color = PdfColor::from_rgb(
            ((font.rgba_color >> 24) as u8 as i32 & 0xff) as f64 / 255 as f64,
            ((font.rgba_color >> 16) as u8 as i32 & 0xff) as f64 / 255 as f64,
            ((font.rgba_color >> 8) as u8 as i32 & 0xff) as f64 / 255 as f64,
        )
        .unwrap();
        let color_clone = color.clone();
        pdf_color_push(&mut color, &color_clone);
    }
    for i in 0..slen {
        let mut glyph_id = get_buffered_unsigned_pair();
        if (glyph_id as u32) < font.numGlyphs {
            let advance;
            let mut ascent: f64 = font.ascent as f64;
            let mut descent: f64 = font.descent as f64;
            if !font.cffont.is_null() {
                let cstrings: *mut cff_index = (*font.cffont).cstrings;
                let mut gm = t1_ginfo::new();
                /* If .notdef is not the 1st glyph in CharStrings, glyph_id given by
                FreeType should be increased by 1 */
                if (*font.cffont).is_notdef_notzero != 0 {
                    glyph_id += 1;
                }
                t1char_get_metrics(
                    (*cstrings)
                        .data
                        .offset(*(*cstrings).offset.offset(glyph_id as isize) as isize)
                        .offset(-1),
                    (*(*cstrings).offset.offset((glyph_id + 1) as isize))
                        .wrapping_sub(*(*cstrings).offset.offset(glyph_id as isize))
                        as i32,
                    &(*font.cffont).subrs[0],
                    &mut gm,
                );
                advance = (if font.layout_dir == 0 { gm.wx } else { gm.wy }) as u32;
                ascent = gm.bbox.ury;
                descent = gm.bbox.lly
            } else {
                advance = font.hvmt[glyph_id as usize].advance as u32
            }
            glyph_width = (font.size as f64 * advance as f64 / font.unitsPerEm as f64) as spt_t;
            glyph_width = (glyph_width as f32 * font.extend) as spt_t;
            if dvi_is_tracking_boxes() {
                let mut rect = Rect::zero();
                let height = (font.size as f64 * ascent / font.unitsPerEm as f64) as spt_t;
                let depth = (font.size as f64 * -descent / font.unitsPerEm as f64) as spt_t;
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
            &wbuf,
            glyph_width,
            font.font_id,
            -1,
        );
    }
    if font.rgba_color != 0xffffffffu32 {
        pdf_color_pop();
    }
    free(xloc as *mut libc::c_void);
    free(yloc as *mut libc::c_void);
    if do_actual_text != 0 {
        pdf_dev_end_actualtext();
    }
    if lr_mode == 0 {
        dvi_right(width);
    };
}
unsafe fn check_postamble() {
    let handle = dvi_handle.as_mut().unwrap();
    skip_bytes(28, handle);
    loop {
        let code = u8::get(handle) as u8;
        if code == POST_POST {
            break;
        }
        match code {
            FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {
                skip_bytes((code + 1 - FNT_DEF1) as u32, handle);
                skip_fntdef();
            }
            XDV_NATIVE_FONT_DEF => {
                skip_bytes(4, handle);
                skip_native_font_def();
            }
            _ => {
                panic!("Unexpected op code ({}) in postamble", code);
            }
        }
    }
    skip_bytes(4, handle);
    post_id_byte = u8::get(handle) as i32;
    let post_id_byte_0 = post_id_byte as u8;
    if !(post_id_byte_0 == DVI_ID
        || post_id_byte_0 == DVIV_ID
        || post_id_byte_0 == XDV_ID
        || post_id_byte_0 == XDV_ID_OLD)
    {
        info!("DVI ID = {}\n", post_id_byte_0);
        panic!("{}", invalid_signature);
    }
    check_id_bytes();
    if has_ptex != 0 && post_id_byte_0 != DVIV_ID {
        panic!("Saw opcode {} in DVI file not for Ascii pTeX", PTEXDIR);
    }
    num_pages = 0;
    /* force loop to terminate */
}
/* Most of the work of actually interpreting
 * the dvi file is here.
 */

pub(crate) unsafe fn dvi_do_page(page_paper_height: f64, hmargin: f64, vmargin: f64) {
    /* before this is called, we have scanned the page for papersize specials
    and the complete DVI data is now in DVI_PAGE_BUFFER */
    DVI_PAGE_BUF_INDEX = 0;
    /* DVI coordinate */
    dev_origin_x = hmargin;
    dev_origin_y = page_paper_height - vmargin;
    dvi_stack_depth = 0;
    loop {
        let opcode = get_buffered_unsigned_byte();
        if opcode <= SET_CHAR_127 {
            dvi_set(opcode as i32);
            continue;
        }

        /* If we are here, we have an opcode that is something
         * other than SET_CHAR.
         */
        if opcode >= FNT_NUM_0 && opcode <= FNT_NUM_63 {
            do_fnt((opcode - FNT_NUM_0) as u32);
            continue;
        }

        match opcode {
            SET1 | SET2 | SET3 => {
                dvi_set(get_buffered_unsigned_num(opcode - SET1));
            }
            SET4 => {
                panic!("Multibyte (>24 bits) character not supported!");
            }
            SET_RULE => {
                do_setrule();
            }
            PUT1 | PUT2 | PUT3 => {
                dvi_put(get_buffered_unsigned_num(opcode - PUT1));
            }
            PUT4 => {
                panic!("Multibyte (>24 bits) character not supported!");
            }
            PUT_RULE => {
                do_putrule();
            }
            NOP => {}
            BOP => {
                do_bop();
            }
            EOP => {
                do_eop();
                if linear != 0 {
                    let handle = dvi_handle.as_mut().unwrap();
                    let opcode = u8::get(handle);
                    if opcode == POST {
                        check_postamble();
                    } else {
                        handle.seek(SeekFrom::Current(-1)).unwrap();
                    }
                }
                return;
            }
            PUSH => {
                dvi_push();
                if lr_mode >= 2 {
                    lr_width_push();
                }
                /* The following line needs to go here instead of in
                 * dvi_push() since logical structure of document is
                 * oblivous to virtual fonts. For example the last line on a
                 * page could be at stack level 3 and the page footer should
                 * be at stack level 3.  However, if the page footer contains
                 * virtual fonts (or other nested constructions), it could
                 * fool the link breaker into thinking it was a continuation
                 * of the link */
                dvi_mark_depth();
            }
            POP => {
                dpx_dvi_pop();
                if lr_mode >= 2 {
                    lr_width_pop();
                }
                /* Above explanation holds for following line too */
                dvi_mark_depth();
            }
            RIGHT1 | RIGHT2 | RIGHT3 | RIGHT4 => {
                dvi_right(get_buffered_signed_num(opcode - RIGHT1));
            }
            W0 => {
                dvi_w0();
            }
            W1 | W2 | W3 | W4 => {
                dvi_w(get_buffered_signed_num(opcode - W1));
            }
            X0 => {
                dvi_x0();
            }
            X1 | X2 | X3 | X4 => {
                dvi_x(get_buffered_signed_num(opcode - X1));
            }
            DOWN1 | DOWN2 | DOWN3 | DOWN4 => {
                dvi_down(get_buffered_signed_num(opcode - DOWN1));
            }
            Y0 => {
                dvi_y0();
            }
            Y1 | Y2 | Y3 | Y4 => {
                dvi_y(get_buffered_signed_num(opcode - Y1));
            }
            Z0 => {
                dvi_z0();
            }
            Z1 | Z2 | Z3 | Z4 => {
                dvi_z(get_buffered_signed_num(opcode - Z1));
            }
            FNT1 | FNT2 | FNT3 | FNT4 => {
                do_fnt(get_buffered_unsigned_num(opcode - FNT1) as u32);
            }
            /* Specials */
            XXX1 | XXX2 | XXX3 | XXX4 => {
                let size: i32 = get_buffered_unsigned_num(opcode - XXX1);
                if size < 0 {
                    warn!("DVI: Special with {} bytes???", size);
                } else {
                    do_xxx(size);
                }
            }
            /* These should not occur - processed during pre-scanning */
            FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {}
            PTEXDIR => {
                /* pTeX extension */
                need_pTeX(opcode as i32);
                do_dir();
            }
            XDV_GLYPHS => {
                /* XeTeX extensions */
                need_XeTeX(opcode as i32);
                do_glyphs(0);
            }
            XDV_TEXT_AND_GLYPHS => {
                need_XeTeX(opcode as i32);
                do_glyphs(1);
            }
            XDV_NATIVE_FONT_DEF => {
                /* should not occur - processed during pre-scanning */
                need_XeTeX(opcode as i32);
            }
            BEGIN_REFLECT => {
                need_XeTeX(opcode as i32);
                dvi_begin_reflect();
            }
            END_REFLECT => {
                need_XeTeX(opcode as i32);
                dvi_end_reflect();
            }
            POST => {
                if linear != 0 && processing_page == 0 {
                    /* for linear processing, this means there are no more pages */
                    num_pages = 0; /* force loop to terminate */
                    return;
                }
                /* else fall through to error case */
                panic!("Unexpected preamble or postamble in dvi file");
            }
            PRE | POST_POST => {
                panic!("Unexpected preamble or postamble in dvi file");
            }
            _ => {
                panic!("Unexpected opcode or DVI file ended prematurely");
            }
        }
    }
}

pub(crate) unsafe fn dvi_init(dvi_filename: &str, mag: f64) -> f64 {
    if dvi_filename.is_empty() {
        panic!("filename must be specified");
    }
    dvi_handle = InFile::open(dvi_filename, TTInputFormat::BINARY, 0);
    if dvi_handle.is_none() {
        panic!("cannot open \"{}\"", dvi_filename);
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

pub(crate) unsafe fn dvi_close() {
    if linear != 0 {
        /* probably reading a pipe from xetex; consume any remaining data */
        while dvi_handle.as_mut().unwrap().read_byte().is_some() {}
    }
    /* We add comment in dvi_close instead of dvi_init so user
     * has a change to overwrite it.  The docinfo dictionary is
     * treated as a write-once record.
     */
    /* Do some house cleaning */
    let _ = dvi_handle.take();

    for font in &mut def_fonts {
        font.font_name.clear();
    }
    def_fonts.clear();

    page_loc = mfree(page_loc as *mut libc::c_void) as *mut u32;
    num_pages = 0_u32;

    for font in &mut loaded_fonts {
        font.hvmt = Vec::new();
        if !(font.cffont.is_null()) {
            let _ = Box::from_raw(font.cffont);
        }
        font.cffont = ptr::null_mut();
    }
    loaded_fonts.clear();
    vf_close_all_fonts();
    tfm_close_all();
    if !DVI_PAGE_BUFFER.is_empty() {
        DVI_PAGE_BUFFER = Vec::new();
    }
}
/* The following are need to implement virtual fonts
According to documentation, the vf "subroutine"
must have state pushed and must have
w,v,y, and z set to zero.  The current font
is determined by the virtual font header, which
may be undefined */
static mut saved_dvi_font: [i32; 16] = [0; 16];
static mut num_saved_fonts: u32 = 0_u32;

pub(crate) unsafe fn dvi_vf_init(dev_font_id: i32) {
    dvi_push();
    dvi_state.w = 0;
    dvi_state.x = 0;
    dvi_state.y = 0;
    dvi_state.z = 0;
    /* do not reset dvi_state.d. */
    if num_saved_fonts < 16 {
        saved_dvi_font[num_saved_fonts as usize] = current_font;
        num_saved_fonts += 1;
    } else {
        panic!("Virtual fonts nested too deeply!");
    }
    current_font = dev_font_id;
}
/* After VF subroutine is finished, we simply pop the DVI stack */

pub(crate) unsafe fn dvi_vf_finish() {
    dpx_dvi_pop();
    if num_saved_fonts > 0_u32 {
        num_saved_fonts -= 1;
        current_font = saved_dvi_font[num_saved_fonts as usize];
    } else {
        panic!("Tried to pop an empty font stack");
    }
}
/* Scan various specials */
/* This need to allow 'true' prefix for unit and
 * length value must be divided by current magnification.
 */
/* XXX: there are four quasi-redundant versions of this; grp for K_UNIT__PT */
pub(crate) trait ReadLength {
    fn read_length(&mut self, mag: f64) -> Result<f64, ()>;
    fn read_length_no_mag(&mut self) -> Result<f64, ()>;
}
impl ReadLength for &[u8] {
    fn read_length(&mut self, mag: f64) -> Result<f64, ()> {
        let mut p = *self; /* inverse magnify */
        let mut u: f64 = 1.0f64;
        let mut error: i32 = 0;
        let q = p.parse_float_decimal();
        if q.is_none() {
            *self = p;
            return Err(());
        }
        let v = unsafe { atof(q.unwrap().as_ptr()) };
        p.skip_white();
        if let Some(q) = p.parse_c_ident() {
            let mut bytes = q.as_str();
            if bytes.starts_with("true") {
                u /= if mag != 0. { mag } else { 1. };
                bytes = &bytes["true".len()..];
            }
            let q = if bytes.is_empty() {
                /* "true" was a separate word from the units */
                p.skip_white();
                p.parse_c_ident()
            } else {
                Some(String::from(bytes))
            };
            if let Some(ident) = q {
                match ident.as_str() {
                    "pt" => u *= 72. / 72.27,
                    "in" => u *= 72.,
                    "cm" => u *= 72. / 2.54,
                    "mm" => u *= 72. / 25.4,
                    "bp" => u *= 1.,
                    "pc" => u *= 12. * 72. / 72.27,
                    "dd" => u *= 1238. / 1157. * 72. / 72.27,
                    "cc" => u *= 12. * 1238. / 1157. * 72. / 72.27,
                    "sp" => u *= 72. / (72.27 * 65536.),
                    _ => {
                        warn!("Unknown unit of measure: {}", ident);
                        error = -1
                    }
                }
            } else {
                warn!("Missing unit of measure after \"true\"");
                error = -1
            }
        }
        *self = p;
        if error == 0 {
            Ok(v * u)
        } else {
            Err(())
        }
    }
    fn read_length_no_mag(&mut self) -> Result<f64, ()> {
        self.read_length(1.)
    }
}

unsafe fn scan_special(
    wd: *mut f64,
    ht: *mut f64,
    xo: *mut f64,
    yo: *mut f64,
    lm: *mut i32,
    majorversion: *mut i32,
    minorversion: *mut i32,
    do_enc: *mut i32,
    key_bits: *mut i32,
    permission: *mut i32,
    owner_pw: *mut i8,
    user_pw: *mut i8,
    mut buf: &[u8],
) -> i32 {
    let mut ns_pdf: i32 = 0;
    let mut ns_dvipdfmx: i32 = 0;
    let mut error: i32 = 0;
    buf.skip_white();
    let mut q = buf.parse_c_ident();
    if let Some(ident) = q.as_ref() {
        match ident.as_str() {
            "pdf" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                    ns_pdf = 1;
                }
            }
            "x" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                }
            }
            "dvipdfmx" => {
                buf.skip_white();
                if !buf.is_empty() && buf[0] == b':' {
                    buf = &buf[1..];
                    buf.skip_white();
                    q = buf.parse_c_ident();
                    ns_dvipdfmx = 1
                }
            }
            _ => {}
        }
    }
    buf.skip_white();
    if let Some(q) = q {
        if q == "landscape" {
            *lm = 1
        } else if ns_pdf != 0 && q == "pagesize" {
            while error == 0 && !buf.is_empty() {
                if let Some(kp) = buf.parse_c_ident() {
                    buf.skip_white();
                    match kp.as_str() {
                        "width" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *wd = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        }
                        "height" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *ht = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        }
                        "xoffset" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *xo = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        }
                        "yoffset" => {
                            if let Ok(tmp) = buf.read_length(dvi_tell_mag()) {
                                *yo = tmp * dvi_tell_mag()
                            } else {
                                error = -1;
                            }
                        }
                        "default" => {
                            *wd = paper_width;
                            *ht = paper_height;
                            *lm = landscape_mode;
                            *yo = 72.0f64;
                            *xo = *yo
                        }
                        _ => {}
                    }
                    buf.skip_white();
                } else {
                    break;
                }
            }
        } else if q == "papersize" {
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
                    error = -1
                }
            }
            if error == 0 {
                paper_width = *wd;
                paper_height = *ht
            }
        } else if !minorversion.is_null() && ns_pdf != 0 && q == "minorversion" {
            if buf[0] == b'=' {
                buf = &buf[1..];
            }
            buf.skip_white();
            if let Some(kv) = buf.parse_float_decimal() {
                *minorversion = strtol(kv.as_ptr(), 0 as *mut *mut i8, 10) as i32;
            }
        } else if !majorversion.is_null() && ns_pdf != 0 && q == "majorversion" {
            if buf[0] == b'=' {
                buf = &buf[1..];
            }
            buf.skip_white();
            if let Some(kv_0) = buf.parse_float_decimal() {
                *majorversion = strtol(kv_0.as_ptr(), 0 as *mut *mut i8, 10) as i32;
            }
        } else if ns_pdf != 0 && q == "encrypt" && !do_enc.is_null() {
            *do_enc = 1;
            *user_pw = 0;
            *owner_pw = 0;
            while error == 0 && !buf.is_empty() {
                if let Some(kp) = buf.parse_c_ident() {
                    buf.skip_white();
                    match kp.as_str() {
                        "ownerpw" => {
                            if let Some(obj) = buf.parse_pdf_string() {
                                let bytes = obj.to_bytes();
                                if !bytes.is_empty() {
                                    let cstr = CString::new(bytes).unwrap();
                                    strncpy(owner_pw, cstr.as_ptr(), 127);
                                }
                            } else {
                                error = -1
                            }
                        }
                        "userpw" => {
                            if let Some(obj) = buf.parse_pdf_string() {
                                let bytes = obj.to_bytes();
                                if !bytes.is_empty() {
                                    let cstr = CString::new(bytes).unwrap();
                                    strncpy(user_pw, cstr.as_ptr(), 127);
                                }
                            } else {
                                error = -1
                            }
                        }
                        "length" => {
                            if let Some(num) = buf.parse_pdf_number() {
                                *key_bits = num as u32 as i32;
                            } else {
                                error = -1
                            }
                        }
                        "perm" => {
                            if let Some(num) = buf.parse_pdf_number() {
                                *permission = num as u32 as i32;
                            } else {
                                error = -1
                            }
                        }
                        _ => error = -1,
                    }
                    buf.skip_white();
                } else {
                    break;
                }
            }
        } else if ns_dvipdfmx != 0 && q == "config" {
            warn!("Tectonic does not support `config\' special. Ignored.");
        }
    }
    error
}
static mut buffered_page: i32 = -1;
/* returns scale (dvi2pts) */
/* may append .dvi or .xdv to filename */
/* Closes data structures created by dvi_open */
/* Renamed to avoid clash with XeTeX */

pub(crate) unsafe fn dvi_scan_specials(
    page_no: i32,
    page_width: *mut f64,
    page_height: *mut f64,
    x_offset: *mut f64,
    y_offset: *mut f64,
    landscape: *mut i32,
    majorversion: *mut i32,
    minorversion: *mut i32,
    do_enc: *mut i32,
    key_bits: *mut i32,
    permission: *mut i32,
    owner_pw: *mut i8,
    user_pw: *mut i8,
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
        if opcode == EOP {
            break;
        }
        if opcode <= SET_CHAR_127 || opcode >= FNT_NUM_0 && opcode <= FNT_NUM_63 {
            continue;
        }
        if opcode == XXX1 || opcode == XXX2 || opcode == XXX3 || opcode == XXX4 {
            let mut size: u32 = get_and_buffer_unsigned_byte(handle) as u32;
            match opcode {
                XXX4 => {
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                    if size > 0x7fff {
                        warn!(
                            "Unsigned number starting with {:x} exceeds 0x7fffffff",
                            size,
                        );
                    }
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                }
                XXX3 => {
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                }
                XXX2 => {
                    size = (size << 8) | (get_and_buffer_unsigned_byte(handle) as u32);
                }
                _ => {}
            }

            DVI_PAGE_BUFFER.resize_with(DVI_PAGE_BUF_INDEX + size as usize, Default::default);
            handle
                .read(&mut DVI_PAGE_BUFFER[DVI_PAGE_BUF_INDEX..DVI_PAGE_BUF_INDEX + size as usize])
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
            /* Skipping... */
            match opcode {
                BOP => {
                    get_and_buffer_bytes(handle, 44);
                }
                NOP | PUSH | POP | W0 | X0 | Y0 | Z0 => {}
                SET1 | PUT1 | RIGHT1 | DOWN1 | W1 | X1 | Y1 | Z1 | FNT1 => {
                    get_and_buffer_bytes(handle, 1);
                }
                SET2 | PUT2 | RIGHT2 | DOWN2 | W2 | X2 | Y2 | Z2 | FNT2 => {
                    get_and_buffer_bytes(handle, 2); /* width */
                }
                SET3 | PUT3 | RIGHT3 | DOWN3 | W3 | X3 | Y3 | Z3 | FNT3 => {
                    get_and_buffer_bytes(handle, 3);
                }
                SET4 | PUT4 | RIGHT4 | DOWN4 | W4 | X4 | Y4 | Z4 | FNT4 => {
                    get_and_buffer_bytes(handle, 4);
                }
                SET_RULE | PUT_RULE => {
                    get_and_buffer_bytes(handle, 8_u32);
                }
                FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {
                    do_fntdef(get_unsigned_num(handle, opcode - FNT_DEF1));
                }
                XDV_GLYPHS => {
                    need_XeTeX(opcode as i32);
                    get_and_buffer_bytes(handle, 4); /* width */
                    let len = get_and_buffer_unsigned_pair(handle); /* glyph count */
                    get_and_buffer_bytes(handle, len.wrapping_mul(10)); /* 2 bytes ID + 8 bytes x,y-location per glyph */
                }
                XDV_TEXT_AND_GLYPHS => {
                    need_XeTeX(opcode as i32);
                    let len = get_and_buffer_unsigned_pair(handle); /* utf16 code unit count */
                    get_and_buffer_bytes(handle, len.wrapping_mul(2)); /* 2 bytes per code unit */
                    get_and_buffer_bytes(handle, 4); /* width */
                    let len = get_and_buffer_unsigned_pair(handle); /* glyph count */
                    get_and_buffer_bytes(handle, len.wrapping_mul(10)); /* 2 bytes ID + 8 bytes x,y-location per glyph */
                }
                XDV_NATIVE_FONT_DEF => {
                    need_XeTeX(opcode as i32);
                    do_native_font_def(i32::get(handle));
                }
                BEGIN_REFLECT | END_REFLECT => {
                    need_XeTeX(opcode as i32);
                }
                PTEXDIR => {
                    need_pTeX(opcode as i32);
                    get_and_buffer_bytes(handle, 1);
                }
                POST => {
                    if linear != 0 && DVI_PAGE_BUF_INDEX == 1 {
                        /* this is actually an indication that we've reached the end of the input */
                        return;
                    }
                    /* else fall through to error case */
                    panic!("Unexpected opcode {}", opcode as i32);
                }
                _ => {
                    /* case PRE: case POST_POST: and others */
                    panic!("Unexpected opcode {}", opcode as i32);
                }
            }
        }
    }
}
/* spt_t */
/* instantiated in dvipdfmx.c */

pub(crate) unsafe fn dvi_reset_global_state() {
    buffered_page = -1;
    def_fonts = Vec::new();
    compute_boxes = 0;
    link_annot = 1;
    verbose = 0;
    loaded_fonts = Vec::new();
}
