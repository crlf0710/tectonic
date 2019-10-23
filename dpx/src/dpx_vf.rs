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
    tt_get_positive_quad, tt_get_unsigned_byte, tt_get_unsigned_num, tt_get_unsigned_quad,
};
use crate::streq_ptr;
use crate::warn;
use crate::DisplayExt;
use std::ffi::CStr;

use super::dpx_dvi::{
    dpx_dvi_pop, dvi_dirchg, dvi_do_special, dvi_down, dvi_locate_font, dvi_push, dvi_put,
    dvi_right, dvi_rule, dvi_set, dvi_set_font, dvi_vf_finish, dvi_vf_init, dvi_w, dvi_w0, dvi_x,
    dvi_x0, dvi_y, dvi_y0, dvi_z, dvi_z0,
};
use super::dpx_mem::{new, renew};
use super::dpx_numbers::{sqxfw, tt_skip_bytes};
use super::dpx_tfm::tfm_open;
use crate::{ttstub_input_close, ttstub_input_open, ttstub_input_read};
use libc::{free, strcpy, strlen};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type __ssize_t = i64;
pub type size_t = u64;

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
#[no_mangle]
pub unsafe extern "C" fn vf_set_verbose(mut level: i32) {
    verbose = level as u8;
}
static mut vf_fonts: *mut vf = 0 as *const vf as *mut vf;
static mut num_vf_fonts: u32 = 0_u32;
static mut max_vf_fonts: u32 = 0_u32;
#[no_mangle]
pub unsafe extern "C" fn vf_reset_global_state() {
    num_vf_fonts = 0_u32;
    max_vf_fonts = 0_u32;
    vf_fonts = 0 as *mut vf;
}
unsafe fn read_header(vf_handle: &mut InputHandleWrapper, mut thisfont: i32) {
    if tt_get_unsigned_byte(vf_handle) as i32 != 247i32
        || tt_get_unsigned_byte(vf_handle) as i32 != 202i32
    {
        eprintln!("VF file may be corrupt");
        return;
    }
    /* skip comment */
    tt_skip_bytes(tt_get_unsigned_byte(vf_handle) as u32, vf_handle);
    /* Skip checksum */
    tt_skip_bytes(4_u32, vf_handle);
    (*vf_fonts.offset(thisfont as isize)).design_size = tt_get_positive_quad(
        vf_handle,
        b"VF\x00" as *const u8 as *const i8,
        b"design_size\x00" as *const u8 as *const i8,
    );
}
unsafe fn resize_vf_fonts(mut size: i32) {
    if size as u32 > max_vf_fonts {
        vf_fonts = renew(
            vf_fonts as *mut libc::c_void,
            (size as u32 as u64).wrapping_mul(::std::mem::size_of::<vf>() as u64) as u32,
        ) as *mut vf;
        for i in max_vf_fonts..size as u32 {
            (*vf_fonts.offset(i as isize)).num_dev_fonts = 0_u32;
            (*vf_fonts.offset(i as isize)).max_dev_fonts = 0_u32;
            let ref mut fresh0 = (*vf_fonts.offset(i as isize)).dev_fonts;
            *fresh0 = 0 as *mut font_def;
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
            *fresh1 = 0 as *mut u8;
            *(*a_vf).pkt_len.offset(i as isize) = 0_u32;
        }
        (*a_vf).num_chars = size
    };
}
unsafe fn read_a_char_def(
    vf_handle: &mut InputHandleWrapper,
    mut thisfont: i32,
    mut pkt_len: u32,
    mut ch: u32,
) {
    /* Resize and initialize character arrays if necessary */
    if ch >= (*vf_fonts.offset(thisfont as isize)).num_chars {
        resize_one_vf_font(vf_fonts.offset(thisfont as isize), ch.wrapping_add(1_u32));
        /* must exist */
    }
    if pkt_len > 0_u32 {
        let pkt = new((pkt_len as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        if ttstub_input_read(vf_handle.0.as_ptr(), pkt as *mut i8, pkt_len as size_t) != pkt_len as i64 {
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
unsafe fn read_a_font_def(vf_handle: &mut InputHandleWrapper, mut font_id: i32, mut thisfont: i32) {
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
    (*dev_font).size = tt_get_positive_quad(
        vf_handle,
        b"VF\x00" as *const u8 as *const i8,
        b"font_size\x00" as *const u8 as *const i8,
    );
    (*dev_font).design_size = tt_get_positive_quad(
        vf_handle,
        b"VF\x00" as *const u8 as *const i8,
        b"font_design_size\x00" as *const u8 as *const i8,
    );
    let dir_length = tt_get_unsigned_byte(vf_handle) as i32;
    let name_length = tt_get_unsigned_byte(vf_handle) as i32;
    (*dev_font).directory = new(
        ((dir_length + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32
    ) as *mut i8;
    if ttstub_input_read(vf_handle.0.as_ptr(), (*dev_font).directory, dir_length as size_t)
        != dir_length as i64
    {
        panic!("directory read failed");
    }
    (*dev_font).name = new(((name_length + 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    if ttstub_input_read(vf_handle.0.as_ptr(), (*dev_font).name, name_length as size_t) != name_length as i64 {
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
unsafe fn process_vf_file(vf_handle: &mut InputHandleWrapper, mut thisfont: i32) {
    let mut eof: i32 = 0i32;
    while eof == 0 {
        let code = tt_get_unsigned_byte(vf_handle) as i32;
        match code {
            243 | 244 | 245 | 246 => {
                let font_id = tt_get_unsigned_num(vf_handle, (code - 243i32) as u8);
                read_a_font_def(vf_handle, font_id as i32, thisfont);
            }
            _ => {
                if code < 242i32 {
                    /* For a short packet, code is the pkt_len */
                    let mut ch: u32 = tt_get_unsigned_byte(vf_handle) as u32;
                    /* Skip over TFM width since we already know it */
                    tt_skip_bytes(3_u32, vf_handle);
                    read_a_char_def(vf_handle, thisfont, code as u32, ch);
                } else if code == 242i32 {
                    let mut pkt_len: u32 = tt_get_positive_quad(
                        vf_handle,
                        b"VF\x00" as *const u8 as *const i8,
                        b"pkt_len\x00" as *const u8 as *const i8,
                    );
                    let mut ch_0: u32 = tt_get_unsigned_quad(vf_handle);
                    /* Skip over TFM width since we already know it */
                    tt_skip_bytes(4_u32, vf_handle);
                    if ch_0 < 0x1000000u32 {
                        read_a_char_def(vf_handle, thisfont, pkt_len, ch_0);
                    } else {
                        eprintln!("char={}", ch_0);
                        panic!("Long character (>24 bits) in VF file.\nI can\'t handle long characters!\n");
                    }
                } else if code == 248i32 {
                    eof = 1i32
                } else {
                    eprintln!("Quitting on code={}", code);
                    eof = 1i32
                }
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
#[no_mangle]
pub unsafe extern "C" fn vf_locate_font(mut tex_name: *const i8, mut ptsize: spt_t) -> i32 {
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
    let mut vf_handle = ttstub_input_open(tex_name, TTInputFormat::VF, 0i32).or_else(||
        ttstub_input_open(tex_name, TTInputFormat::OVF, 0i32)
    );
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
    *fresh9 = 0 as *mut u32;
    read_header(&mut vf_handle, thisfont);
    process_vf_file(&mut vf_handle, thisfont);
    if verbose != 0 {
        eprint!(")");
    }
    ttstub_input_close(vf_handle);
    thisfont
}
unsafe fn unsigned_byte(mut start: *mut *mut u8, mut end: *mut u8) -> i32 {
    if *start < end {
        let fresh10 = *start;
        *start = (*start).offset(1);
        return *fresh10 as i32;
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
}
unsafe fn get_pkt_signed_num(mut start: *mut *mut u8, mut end: *mut u8, mut num: u8) -> i32 {
    let mut val;
    if end.wrapping_offset_from(*start) as i64 > num as i64 {
        let fresh11 = *start;
        *start = (*start).offset(1);
        val = *fresh11 as i32;
        if val > 0x7fi32 {
            val -= 0x100i32
        }
        let mut current_block_5: u64;
        match num as i32 {
            3 => {
                let fresh12 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh12 as i32;
                current_block_5 = 9698575669066167445;
            }
            2 => {
                current_block_5 = 9698575669066167445;
            }
            1 => {
                current_block_5 = 18113473374131038547;
            }
            _ => {
                current_block_5 = 13183875560443969876;
            }
        }
        match current_block_5 {
            9698575669066167445 => {
                let fresh13 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh13 as i32;
                current_block_5 = 18113473374131038547;
            }
            _ => {}
        }
        match current_block_5 {
            18113473374131038547 => {
                let fresh14 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh14 as i32
            }
            _ => {}
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn get_pkt_unsigned_num(mut start: *mut *mut u8, mut end: *mut u8, mut num: u8) -> i32 {
    let mut val;
    if end.wrapping_offset_from(*start) as i64 > num as i64 {
        let fresh15 = *start;
        *start = (*start).offset(1);
        val = *fresh15 as i32;
        let mut current_block_5: u64;
        match num as i32 {
            3 => {
                if val > 0x7fi32 {
                    val -= 0x100i32
                }
                let fresh16 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh16 as i32;
                current_block_5 = 5559910912116893431;
            }
            2 => {
                current_block_5 = 5559910912116893431;
            }
            1 => {
                current_block_5 = 15700427407090132107;
            }
            _ => {
                current_block_5 = 13183875560443969876;
            }
        }
        match current_block_5 {
            5559910912116893431 => {
                let fresh17 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh17 as i32;
                current_block_5 = 15700427407090132107;
            }
            _ => {}
        }
        match current_block_5 {
            15700427407090132107 => {
                let fresh18 = *start;
                *start = (*start).offset(1);
                val = val << 8i32 | *fresh18 as i32
            }
            _ => {}
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn vf_putrule(mut start: *mut *mut u8, mut end: *mut u8, mut ptsize: spt_t) {
    let mut height: i32 = get_pkt_signed_num(start, end, 3_u8);
    let mut width: i32 = get_pkt_signed_num(start, end, 3_u8);
    dvi_rule(sqxfw(ptsize, width), sqxfw(ptsize, height));
}
unsafe fn vf_setrule(mut start: *mut *mut u8, mut end: *mut u8, mut ptsize: spt_t) {
    let mut height: i32 = get_pkt_signed_num(start, end, 3_u8);
    let mut s_width: i32 = sqxfw(ptsize, get_pkt_signed_num(start, end, 3_u8));
    dvi_rule(s_width, sqxfw(ptsize, height));
    dvi_right(s_width);
}
unsafe fn vf_fnt(mut font_id: i32, mut vf_font: i32) {
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
unsafe fn vf_xxx(mut len: i32, mut start: *mut *mut u8, mut end: *mut u8) {
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
                warn!("VF:{}", buffer[i+8..].display());
            }
        } else {
            dvi_do_special(buffer.as_slice());
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font.");
    }
    *start = (*start).offset(len as isize);
}
#[no_mangle]
pub unsafe extern "C" fn vf_set_char(mut ch: i32, mut vf_font: i32) {
    let mut start: *mut u8 = 0 as *mut u8;
    let end;
    let mut default_font: i32 = -1i32;
    if (vf_font as u32) < num_vf_fonts {
        /* Initialize to the first font or -1 if undefined */
        let ptsize = (*vf_fonts.offset(vf_font as isize)).ptsize;
        if (*vf_fonts.offset(vf_font as isize)).num_dev_fonts > 0_u32 {
            default_font = (*(*vf_fonts.offset(vf_font as isize)).dev_fonts.offset(0)).dev_id
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
            end = 0 as *mut u8;
            start = end
        } else {
            end = start.offset(
                *(*vf_fonts.offset(vf_font as isize))
                    .pkt_len
                    .offset(ch as isize) as isize,
            )
        }
        while !start.is_null() && start < end {
            let fresh19 = start;
            start = start.offset(1);
            let opcode = *fresh19;
            match opcode as i32 {
                128 | 129 | 130 => {
                    dvi_set(get_pkt_unsigned_num(
                        &mut start,
                        end,
                        (opcode as i32 - 128i32) as u8,
                    ));
                }
                131 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                132 => {
                    vf_setrule(&mut start, end, ptsize);
                }
                133 | 134 | 135 => {
                    dvi_put(get_pkt_unsigned_num(
                        &mut start,
                        end,
                        (opcode as i32 - 133i32) as u8,
                    ));
                }
                136 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                137 => {
                    vf_putrule(&mut start, end, ptsize);
                }
                138 => {}
                141 => {
                    dvi_push();
                }
                142 => {
                    dpx_dvi_pop();
                }
                143 | 144 | 145 | 146 => {
                    dvi_right(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 143i32) as u8),
                    ));
                }
                147 => {
                    dvi_w0();
                }
                148 | 149 | 150 | 151 => {
                    dvi_w(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 148i32) as u8),
                    ));
                }
                152 => {
                    dvi_x0();
                }
                153 | 154 | 155 | 156 => {
                    dvi_x(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 153i32) as u8),
                    ));
                }
                157 | 158 | 159 | 160 => {
                    dvi_down(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 157i32) as u8),
                    ));
                }
                161 => {
                    dvi_y0();
                }
                162 | 163 | 164 | 165 => {
                    dvi_y(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 162i32) as u8),
                    ));
                }
                166 => {
                    dvi_z0();
                }
                167 | 168 | 169 | 170 => {
                    dvi_z(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 167i32) as u8),
                    ));
                }
                235 | 236 | 237 | 238 => {
                    vf_fnt(
                        get_pkt_signed_num(&mut start, end, (opcode as i32 - 235i32) as u8),
                        vf_font,
                    );
                }
                239 | 240 | 241 | 242 => {
                    let mut len: i32 =
                        get_pkt_unsigned_num(&mut start, end, (opcode as i32 - 239i32) as u8);
                    if len < 0i32 {
                        warn!("VF: Special with {} bytes???", len);
                    } else {
                        vf_xxx(len, &mut start, end);
                    }
                }
                255 => {
                    dvi_dirchg(unsigned_byte(&mut start, end) as u8);
                }
                _ => {
                    if opcode as i32 <= 127i32 {
                        dvi_set(opcode as i32);
                    } else if opcode as i32 >= 171i32 && opcode as i32 <= 234i32 {
                        vf_fnt(opcode as i32 - 171i32, vf_font);
                    } else {
                        eprintln!("Unexpected opcode: {}", opcode as i32);
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
#[no_mangle]
pub unsafe extern "C" fn vf_close_all_fonts() {
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
