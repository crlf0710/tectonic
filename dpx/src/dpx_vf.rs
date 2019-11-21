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

use super::dpx_numbers::{
    tt_get_positive_quad, tt_get_unsigned_byte, tt_get_unsigned_num, tt_get_unsigned_quad,
};
use crate::streq_ptr;
use crate::warn;
use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_dvi::{
    dpx_dvi_pop, dvi_dirchg, dvi_do_special, dvi_down, dvi_locate_font, dvi_push, dvi_put,
    dvi_right, dvi_rule, dvi_set, dvi_set_font, dvi_vf_finish, dvi_vf_init, dvi_w, dvi_w0, dvi_x,
    dvi_x0, dvi_y, dvi_y0, dvi_z, dvi_z0,
};
use super::dpx_dvicodes::*;
use super::dpx_mem::{new, renew};
use super::dpx_numbers::{sqxfw, tt_skip_bytes};
use super::dpx_tfm::tfm_open;
use crate::{ttstub_input_close, ttstub_input_open, ttstub_input_read};
use libc::{free, strcpy, strlen};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type __ssize_t = i64;
pub type size_t = u64;

const VF_ID: u8 = 202;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
pub type fixword = i32;
pub type spt_t = i32;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct vf {
    pub tex_name: *mut i8,
    pub ptsize: spt_t,
    pub design_size: u32,
    pub num_dev_fonts: u32,
    pub max_dev_fonts: u32,
    pub dev_fonts: *mut font_def,
    pub ch_pkt: *mut *mut u8,
    pub pkt_len: *mut u32,
    pub num_chars: u32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct font_def {
    pub font_id: i32,
    pub checksum: u32,
    pub size: u32,
    pub design_size: u32,
    pub directory: *mut i8,
    pub name: *mut i8,
    pub tfm_id: i32,
    pub dev_id: i32,
    /* quasi-hack to get the primary input */
    /* id returned by DEV module */
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: u8 = 0_u8;

pub unsafe fn vf_set_verbose(level: i32) {
    verbose = level as u8;
}
static mut vf_fonts: *mut vf = std::ptr::null_mut();
static mut num_vf_fonts: u32 = 0_u32;
static mut max_vf_fonts: u32 = 0_u32;

pub unsafe fn vf_reset_global_state() {
    num_vf_fonts = 0_u32;
    max_vf_fonts = 0_u32;
    vf_fonts = ptr::null_mut();
}
unsafe fn read_header(vf_handle: &mut InputHandleWrapper, thisfont: i32) {
    if tt_get_unsigned_byte(vf_handle) != PRE || tt_get_unsigned_byte(vf_handle) != VF_ID {
        eprintln!("VF file may be corrupt");
        return;
    }
    /* skip comment */
    tt_skip_bytes(tt_get_unsigned_byte(vf_handle) as u32, vf_handle);
    /* Skip checksum */
    tt_skip_bytes(4_u32, vf_handle);
    (*vf_fonts.offset(thisfont as isize)).design_size =
        tt_get_positive_quad(vf_handle, "VF", "design_size");
}
unsafe fn resize_vf_fonts(size: i32) {
    if size as u32 > max_vf_fonts {
        vf_fonts = renew(
            vf_fonts as *mut libc::c_void,
            (size as u32 as u64).wrapping_mul(::std::mem::size_of::<vf>() as u64) as u32,
        ) as *mut vf;
        for i in max_vf_fonts..size as u32 {
            (*vf_fonts.offset(i as isize)).num_dev_fonts = 0_u32;
            (*vf_fonts.offset(i as isize)).max_dev_fonts = 0_u32;
            let ref mut fresh0 = (*vf_fonts.offset(i as isize)).dev_fonts;
            *fresh0 = ptr::null_mut();
        }
        max_vf_fonts = size as u32
    };
}
unsafe fn resize_one_vf_font(mut a_vf: *mut vf, mut size: u32) {
    if size > (*a_vf).num_chars {
        size = if size > (*a_vf).num_chars.wrapping_add(256_u32) {
            size
        } else {
            (*a_vf).num_chars.wrapping_add(256_u32)
        };
        (*a_vf).ch_pkt = renew(
            (*a_vf).ch_pkt as *mut libc::c_void,
            (size as u64).wrapping_mul(::std::mem::size_of::<*mut u8>() as u64) as u32,
        ) as *mut *mut u8;
        (*a_vf).pkt_len = renew(
            (*a_vf).pkt_len as *mut libc::c_void,
            (size as u64).wrapping_mul(::std::mem::size_of::<u32>() as u64) as u32,
        ) as *mut u32;
        for i in (*a_vf).num_chars..size {
            let ref mut fresh1 = *(*a_vf).ch_pkt.offset(i as isize);
            *fresh1 = ptr::null_mut();
            *(*a_vf).pkt_len.offset(i as isize) = 0_u32;
        }
        (*a_vf).num_chars = size
    };
}
unsafe fn read_a_char_def(
    vf_handle: &mut InputHandleWrapper,
    thisfont: i32,
    pkt_len: u32,
    ch: u32,
) {
    /* Resize and initialize character arrays if necessary */
    if ch >= (*vf_fonts.offset(thisfont as isize)).num_chars {
        resize_one_vf_font(vf_fonts.offset(thisfont as isize), ch.wrapping_add(1_u32));
        /* must exist */
    }
    if pkt_len > 0_u32 {
        let pkt = new((pkt_len as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        if ttstub_input_read(vf_handle.0.as_ptr(), pkt as *mut i8, pkt_len as size_t)
            != pkt_len as i64
        {
            panic!("VF file ended prematurely.");
        }
        let ref mut fresh2 = *(*vf_fonts.offset(thisfont as isize))
            .ch_pkt
            .offset(ch as isize);
        *fresh2 = pkt
    }
    *(*vf_fonts.offset(thisfont as isize))
        .pkt_len
        .offset(ch as isize) = pkt_len;
}
unsafe fn read_a_font_def(vf_handle: &mut InputHandleWrapper, font_id: i32, thisfont: i32) {
    if (*vf_fonts.offset(thisfont as isize)).num_dev_fonts
        >= (*vf_fonts.offset(thisfont as isize)).max_dev_fonts
    {
        let ref mut fresh3 = (*vf_fonts.offset(thisfont as isize)).max_dev_fonts;
        *fresh3 = (*fresh3).wrapping_add(16u32);
        let ref mut fresh4 = (*vf_fonts.offset(thisfont as isize)).dev_fonts;
        *fresh4 = renew(
            (*vf_fonts.offset(thisfont as isize)).dev_fonts as *mut libc::c_void,
            ((*vf_fonts.offset(thisfont as isize)).max_dev_fonts as u64)
                .wrapping_mul(::std::mem::size_of::<font_def>() as u64) as u32,
        ) as *mut font_def
    }
    let dev_font = (*vf_fonts.offset(thisfont as isize))
        .dev_fonts
        .offset((*vf_fonts.offset(thisfont as isize)).num_dev_fonts as isize);
    (*dev_font).font_id = font_id;
    (*dev_font).checksum = tt_get_unsigned_quad(vf_handle);
    (*dev_font).size = tt_get_positive_quad(vf_handle, "VF", "font_size");
    (*dev_font).design_size = tt_get_positive_quad(vf_handle, "VF", "font_design_size");
    let dir_length = tt_get_unsigned_byte(vf_handle) as i32;
    let name_length = tt_get_unsigned_byte(vf_handle) as i32;
    (*dev_font).directory = new(
        ((dir_length + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32
    ) as *mut i8;
    if ttstub_input_read(
        vf_handle.0.as_ptr(),
        (*dev_font).directory,
        dir_length as size_t,
    ) != dir_length as i64
    {
        panic!("directory read failed");
    }
    (*dev_font).name = new(((name_length + 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    if ttstub_input_read(
        vf_handle.0.as_ptr(),
        (*dev_font).name,
        name_length as size_t,
    ) != name_length as i64
    {
        panic!("directory read failed");
    }
    *(*dev_font).directory.offset(dir_length as isize) = 0_i8;
    *(*dev_font).name.offset(name_length as isize) = 0_i8;
    let ref mut fresh5 = (*vf_fonts.offset(thisfont as isize)).num_dev_fonts;
    *fresh5 = (*fresh5).wrapping_add(1_u32);
    (*dev_font).tfm_id = tfm_open((*dev_font).name, 1i32);
    (*dev_font).dev_id = dvi_locate_font(
        (*dev_font).name,
        sqxfw(
            (*vf_fonts.offset(thisfont as isize)).ptsize,
            (*dev_font).size as fixword,
        ),
    ) as i32;
}
unsafe fn process_vf_file(vf_handle: &mut InputHandleWrapper, thisfont: i32) {
    loop {
        let code = tt_get_unsigned_byte(vf_handle);
        match code {
            FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {
                let font_id = tt_get_unsigned_num(vf_handle, code - FNT_DEF1);
                read_a_font_def(vf_handle, font_id as i32, thisfont);
            }
            XXX4 => {
                let pkt_len: u32 = tt_get_positive_quad(vf_handle, "VF", "pkt_len");
                let ch: u32 = tt_get_unsigned_quad(vf_handle);
                /* Skip over TFM width since we already know it */
                tt_skip_bytes(4, vf_handle);
                if ch < 0x1000000 {
                    read_a_char_def(vf_handle, thisfont, pkt_len, ch);
                } else {
                    eprintln!("char={}", ch);
                    panic!(
                        "Long character (>24 bits) in VF file.\nI can\'t handle long characters!\n"
                    );
                }
            }
            POST => {
                break;
            }
            _ if code < XXX4 => {
                /* For a short packet, code is the pkt_len */
                let ch = tt_get_unsigned_byte(vf_handle) as u32;
                /* Skip over TFM width since we already know it */
                tt_skip_bytes(3, vf_handle);
                read_a_char_def(vf_handle, thisfont, code as u32, ch);
            }
            _ => {
                eprintln!("Quitting on code={}", code);
                break;
            }
        }
    }
}
/* Unfortunately, the following code isn't smart enough
to load the vf only once for multiple point sizes.
You will get a separate copy of each VF in memory (and a separate
opening and reading of the file) for
each point size.  Since VFs are pretty small, I guess
this is tolerable for now.  In any case,
the PDF file will never repeat a physical font name */
/* Note: This code needs to be able to recurse */
/* Global variables such as num_vf_fonts require careful attention */

pub unsafe fn vf_locate_font(tex_name: *const i8, ptsize: spt_t) -> i32 {
    /* Has this name and ptsize already been loaded as a VF? */
    let mut i = 0;
    while (i as u32) < num_vf_fonts {
        if streq_ptr((*vf_fonts.offset(i as isize)).tex_name, tex_name) as i32 != 0
            && (*vf_fonts.offset(i as isize)).ptsize == ptsize
        {
            break;
        }
        i += 1
    }
    if i as u32 != num_vf_fonts {
        return i;
    }
    let vf_handle = ttstub_input_open(tex_name, TTInputFormat::VF, 0i32)
        .or_else(|| ttstub_input_open(tex_name, TTInputFormat::OVF, 0i32));
    if vf_handle.is_none() {
        return -1i32;
    }
    let mut vf_handle = vf_handle.unwrap();
    if verbose as i32 == 1i32 {
        let tex_name = CStr::from_ptr(tex_name);
        eprint!("(VF:{}", tex_name.display());
    }
    if num_vf_fonts >= max_vf_fonts {
        resize_vf_fonts(max_vf_fonts.wrapping_add(16u32) as i32);
    }
    let fresh6 = num_vf_fonts;
    num_vf_fonts = num_vf_fonts.wrapping_add(1);
    let thisfont = fresh6 as i32;
    /* Initialize some pointers and such */
    let ref mut fresh7 = (*vf_fonts.offset(thisfont as isize)).tex_name;
    *fresh7 = new((strlen(tex_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
        as *mut i8;
    strcpy((*vf_fonts.offset(thisfont as isize)).tex_name, tex_name);
    (*vf_fonts.offset(thisfont as isize)).ptsize = ptsize;
    (*vf_fonts.offset(thisfont as isize)).num_chars = 0_u32;
    let ref mut fresh8 = (*vf_fonts.offset(thisfont as isize)).ch_pkt;
    *fresh8 = 0 as *mut *mut u8;
    let ref mut fresh9 = (*vf_fonts.offset(thisfont as isize)).pkt_len;
    *fresh9 = ptr::null_mut();
    read_header(&mut vf_handle, thisfont);
    process_vf_file(&mut vf_handle, thisfont);
    if verbose != 0 {
        eprint!(")");
    }
    ttstub_input_close(vf_handle);
    thisfont
}
unsafe fn unsigned_byte(start: *mut *mut u8, end: *mut u8) -> i32 {
    if *start < end {
        let fresh10 = *start;
        *start = (*start).offset(1);
        return *fresh10 as i32;
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
}
unsafe fn get_pkt_signed_num(start: *mut *mut u8, end: *mut u8, num: u8) -> i32 {
    let mut val;
    if end.wrapping_offset_from(*start) as i64 > num as i64 {
        val = **start as i32;
        *start = (*start).offset(1);
        if val > 0x7fi32 {
            val -= 0x100i32
        }
        if 1 <= num && num <= 3 {
            for _ in 0..num {
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
            }
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn get_pkt_unsigned_num(start: *mut *mut u8, end: *mut u8, num: u8) -> i32 {
    let mut val;
    if end.wrapping_offset_from(*start) as i64 > num as i64 {
        val = **start as i32;
        *start = (*start).offset(1);
        match num as i32 {
            3 => {
                if val > 0x7fi32 {
                    val -= 0x100i32
                }
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
            }
            2 => {
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
            }
            1 => {
                val = (val << 8) | **start as i32;
                *start = (*start).offset(1);
            }
            _ => {}
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn vf_putrule(start: *mut *mut u8, end: *mut u8, ptsize: spt_t) {
    let height: i32 = get_pkt_signed_num(start, end, 3_u8);
    let width: i32 = get_pkt_signed_num(start, end, 3_u8);
    dvi_rule(sqxfw(ptsize, width), sqxfw(ptsize, height));
}
unsafe fn vf_setrule(start: *mut *mut u8, end: *mut u8, ptsize: spt_t) {
    let height: i32 = get_pkt_signed_num(start, end, 3_u8);
    let s_width: i32 = sqxfw(ptsize, get_pkt_signed_num(start, end, 3_u8));
    dvi_rule(s_width, sqxfw(ptsize, height));
    dvi_right(s_width);
}
unsafe fn vf_fnt(font_id: i32, vf_font: i32) {
    let mut i: i32 = 0;
    while (i as u32) < (*vf_fonts.offset(vf_font as isize)).num_dev_fonts {
        if font_id
            == (*(*vf_fonts.offset(vf_font as isize))
                .dev_fonts
                .offset(i as isize))
            .font_id
        {
            break;
        }
        i += 1
    }
    if (i as u32) < (*vf_fonts.offset(vf_font as isize)).num_dev_fonts {
        /* Font was found */
        dvi_set_font(
            (*(*vf_fonts.offset(vf_font as isize))
                .dev_fonts
                .offset(i as isize))
            .dev_id,
        );
    } else {
        eprintln!("Font_id: {} not found in VF", font_id);
    };
}
/* identical to do_xxx in dvi.c */
unsafe fn vf_xxx(len: i32, start: *mut *mut u8, end: *mut u8) {
    if *start <= end.offset(-(len as isize)) {
        let mut buffer = Vec::with_capacity(len as usize);
        for i in 0..len {
            buffer.push(*(*start).offset(i as isize))
        }
        let mut i = 0;
        for &p in &buffer {
            if p != b' ' {
                break;
            }
            i += 1;
        }
        /*
         * Warning message from virtual font.
         */
        if buffer[i..].starts_with(b"Warning:") {
            if verbose != 0 {
                warn!("VF:{}", buffer[i + 8..].display());
            }
        } else {
            dvi_do_special(buffer.as_slice());
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font.");
    }
    *start = (*start).offset(len as isize);
}

pub unsafe fn vf_set_char(ch: i32, vf_font: i32) {
    let mut start: *mut u8 = ptr::null_mut();
    let end;
    let mut default_font: i32 = -1i32;
    if (vf_font as u32) < num_vf_fonts {
        /* Initialize to the first font or -1 if undefined */
        let ptsize = (*vf_fonts.offset(vf_font as isize)).ptsize;
        if (*vf_fonts.offset(vf_font as isize)).num_dev_fonts > 0 {
            default_font = (*(*vf_fonts.offset(vf_font as isize)).dev_fonts).dev_id
        }
        dvi_vf_init(default_font);
        if ch as u32 >= (*vf_fonts.offset(vf_font as isize)).num_chars || {
            start = *(*vf_fonts.offset(vf_font as isize))
                .ch_pkt
                .offset(ch as isize);
            start.is_null()
        } {
            eprint!("\nchar=0x{ch:x}({ch})\n", ch = ch);
            eprint!("Tried to set a nonexistent character in a virtual font");
            end = ptr::null_mut();
            start = end
        } else {
            end = start.offset(
                *(*vf_fonts.offset(vf_font as isize))
                    .pkt_len
                    .offset(ch as isize) as isize,
            )
        }
        while !start.is_null() && start < end {
            let opcode = *start;
            start = start.offset(1);
            match opcode {
                SET1 | SET2 | SET3 => {
                    dvi_set(get_pkt_unsigned_num(&mut start, end, opcode - SET1));
                }
                SET4 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                SET_RULE => {
                    vf_setrule(&mut start, end, ptsize);
                }
                PUT1 | PUT2 | PUT3 => {
                    dvi_put(get_pkt_unsigned_num(&mut start, end, opcode - PUT1));
                }
                PUT4 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                PUT_RULE => {
                    vf_putrule(&mut start, end, ptsize);
                }
                NOP => {}
                PUSH => {
                    dvi_push();
                }
                POP => {
                    dpx_dvi_pop();
                }
                RIGHT1 | RIGHT2 | RIGHT3 | RIGHT4 => {
                    dvi_right(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - RIGHT1),
                    ));
                }
                W0 => {
                    dvi_w0();
                }
                W1 | W2 | W3 | W4 => {
                    dvi_w(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - W1),
                    ));
                }
                X0 => {
                    dvi_x0();
                }
                X1 | X2 | X3 | X4 => {
                    dvi_x(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - X1),
                    ));
                }
                DOWN1 | DOWN2 | DOWN3 | DOWN4 => {
                    dvi_down(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - DOWN1),
                    ));
                }
                Y0 => {
                    dvi_y0();
                }
                Y1 | Y2 | Y3 | Y4 => {
                    dvi_y(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - Y1),
                    ));
                }
                Z0 => {
                    dvi_z0();
                }
                Z1 | Z2 | Z3 | Z4 => {
                    dvi_z(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, opcode - Z1),
                    ));
                }
                FNT1 | FNT2 | FNT3 | FNT4 => {
                    vf_fnt(get_pkt_signed_num(&mut start, end, opcode - FNT1), vf_font);
                }
                XXX1 | XXX2 | XXX3 | XXX4 => {
                    let len = get_pkt_unsigned_num(&mut start, end, opcode - XXX1);
                    if len < 0 {
                        warn!("VF: Special with {} bytes???", len);
                    } else {
                        vf_xxx(len, &mut start, end);
                    }
                }
                PTEXDIR => {
                    dvi_dirchg(unsigned_byte(&mut start, end) as u8);
                }
                _ => {
                    if opcode <= SET_CHAR_127 {
                        dvi_set(opcode as i32);
                    } else if opcode >= FNT_NUM_0 && opcode <= FNT_NUM_63 {
                        vf_fnt((opcode - FNT_NUM_0) as i32, vf_font);
                    } else {
                        eprintln!("Unexpected opcode: {}", opcode);
                        panic!("Unexpected opcode in vf file\n");
                    }
                }
            }
        }
        dvi_vf_finish();
    } else {
        eprint!("vf_set_char: font: {}", vf_font);
        panic!("Font not loaded\n");
    };
}

pub unsafe fn vf_close_all_fonts() {
    for i in 0..num_vf_fonts {
        /* Release the packet for each character */
        if !(*vf_fonts.offset(i as isize)).ch_pkt.is_null() {
            for j in 0..(*vf_fonts.offset(i as isize)).num_chars {
                free(*(*vf_fonts.offset(i as isize)).ch_pkt.offset(j as isize) as *mut libc::c_void);
            }
            free((*vf_fonts.offset(i as isize)).ch_pkt as *mut libc::c_void);
        }
        free((*vf_fonts.offset(i as isize)).pkt_len as *mut libc::c_void);
        free((*vf_fonts.offset(i as isize)).tex_name as *mut libc::c_void);
        /* Release each font record */
        for j in 0..(*vf_fonts.offset(i as isize)).num_dev_fonts {
            let one_font =
                &mut *(*vf_fonts.offset(i as isize)).dev_fonts.offset(j as isize) as *mut font_def;
            free((*one_font).directory as *mut libc::c_void);
            free((*one_font).name as *mut libc::c_void);
        }
        free((*vf_fonts.offset(i as isize)).dev_fonts as *mut libc::c_void);
    }
    free(vf_fonts as *mut libc::c_void);
}
