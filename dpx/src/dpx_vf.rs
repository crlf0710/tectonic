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

use super::dpx_numbers::{get_positive_quad, get_unsigned_num, GetFromFile};
use crate::bridge::DisplayExt;
use crate::warn;
use std::ffi::CString;
use std::io::Read;

use super::dpx_dvi::{
    dpx_dvi_pop, dvi_dirchg, dvi_do_special, dvi_down, dvi_locate_font, dvi_push, dvi_put,
    dvi_right, dvi_rule, dvi_set, dvi_set_font, dvi_vf_finish, dvi_vf_init, dvi_w, dvi_w0, dvi_x,
    dvi_x0, dvi_y, dvi_y0, dvi_z, dvi_z0,
};
use super::dpx_dvicodes::*;
use super::dpx_numbers::{skip_bytes, sqxfw};
use super::dpx_tfm::tfm_open;
use crate::bridge::{ttstub_input_close, ttstub_input_open};

const VF_ID: u8 = 202;

use crate::bridge::TTInputFormat;

pub(crate) type fixword = i32;
pub(crate) type spt_t = i32;
#[derive(Clone)]
#[repr(C)]
pub(crate) struct vf {
    pub(crate) tex_name: String,
    pub(crate) ptsize: spt_t,
    pub(crate) design_size: u32,
    pub(crate) dev_fonts: Vec<font_def>,
    pub(crate) ch_pkt: Vec<Vec<u8>>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct font_def {
    pub(crate) font_id: i32,
    pub(crate) checksum: u32,
    pub(crate) size: u32,
    pub(crate) design_size: u32,
    pub(crate) directory: String,
    pub(crate) name: String,
    pub(crate) tfm_id: i32,
    pub(crate) dev_id: i32,
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

pub(crate) unsafe fn vf_set_verbose(level: i32) {
    verbose = level as u8;
}
static mut vf_fonts: Vec<vf> = Vec::new();

pub(crate) unsafe fn vf_reset_global_state() {
    vf_fonts = Vec::new();
}
unsafe fn read_header<R: Read>(vf_handle: &mut R, thisfont: i32) {
    if u8::get(vf_handle) != PRE || u8::get(vf_handle) != VF_ID {
        eprintln!("VF file may be corrupt");
        return;
    }
    /* skip comment */
    skip_bytes(u8::get(vf_handle) as u32, vf_handle);
    /* Skip checksum */
    skip_bytes(4_u32, vf_handle);
    vf_fonts[thisfont as usize].design_size = get_positive_quad(vf_handle, "VF", "design_size");
}
unsafe fn resize_one_vf_font(a_vf: &mut vf, mut size: usize) {
    if size > a_vf.ch_pkt.len() {
        size = size.max(a_vf.ch_pkt.len() + 256);
        a_vf.ch_pkt.resize_with(size, Default::default);
    };
}
unsafe fn read_a_char_def<R: Read>(vf_handle: &mut R, thisfont: i32, pkt_len: u32, ch: u32) {
    /* Resize and initialize character arrays if necessary */
    if (ch as usize) >= vf_fonts[thisfont as usize].ch_pkt.len() {
        resize_one_vf_font(&mut vf_fonts[thisfont as usize], (ch + 1) as usize);
        /* must exist */
    }
    if pkt_len > 0 {
        let mut pkt = vec![0; pkt_len as usize];
        if vf_handle.read_exact(&mut pkt).is_err() {
            panic!("VF file ended prematurely.");
        }
        vf_fonts[thisfont as usize].ch_pkt[ch as usize] = pkt;
    }
}
unsafe fn read_a_font_def<R: Read>(vf_handle: &mut R, font_id: i32, thisfont: i32) {
    let checksum = u32::get(vf_handle);
    let size = get_positive_quad(vf_handle, "VF", "font_size");
    let design_size = get_positive_quad(vf_handle, "VF", "font_design_size");
    let dir_length = u8::get(vf_handle) as usize;
    let name_length = u8::get(vf_handle) as usize;

    let mut directory = vec![0; dir_length];
    if vf_handle.read_exact(&mut directory).is_err() {
        panic!("directory read failed")
    }
    let directory = String::from_utf8(directory).unwrap();
    let mut name = vec![0; name_length];
    if vf_handle.read_exact(&mut name).is_err() {
        panic!("directory read failed")
    }
    let name = String::from_utf8(name).unwrap();

    vf_fonts[thisfont as usize].dev_fonts.push(font_def {
        font_id,
        checksum,
        size,
        design_size,
        directory,
        name,
        tfm_id: 0,
        dev_id: 0,
    });
    let dev_font = vf_fonts[thisfont as usize].dev_fonts.last_mut().unwrap();

    dev_font.tfm_id = tfm_open(&dev_font.name, 1i32);
    dev_font.dev_id = dvi_locate_font(
        &dev_font.name,
        sqxfw(vf_fonts[thisfont as usize].ptsize, dev_font.size as fixword),
    ) as i32;
}
unsafe fn process_vf_file<R: Read>(vf_handle: &mut R, thisfont: i32) {
    loop {
        let code = u8::get(vf_handle);
        match code {
            FNT_DEF1 | FNT_DEF2 | FNT_DEF3 | FNT_DEF4 => {
                let font_id = get_unsigned_num(vf_handle, code - FNT_DEF1);
                read_a_font_def(vf_handle, font_id as i32, thisfont);
            }
            XXX4 => {
                let pkt_len: u32 = get_positive_quad(vf_handle, "VF", "pkt_len");
                let ch: u32 = u32::get(vf_handle);
                /* Skip over TFM width since we already know it */
                skip_bytes(4, vf_handle);
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
                let ch = u8::get(vf_handle) as u32;
                /* Skip over TFM width since we already know it */
                skip_bytes(3, vf_handle);
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

pub(crate) unsafe fn vf_locate_font(tex_name: &str, ptsize: spt_t) -> i32 {
    let tex_name_ = CString::new(tex_name).unwrap();
    /* Has this name and ptsize already been loaded as a VF? */
    let mut i = 0;
    while i < vf_fonts.len() {
        if vf_fonts[i].tex_name == tex_name && vf_fonts[i].ptsize == ptsize {
            break;
        }
        i += 1;
    }
    if i != vf_fonts.len() {
        return i as i32;
    }
    let vf_handle = ttstub_input_open(tex_name_.as_ptr(), TTInputFormat::VF, 0i32)
        .or_else(|| ttstub_input_open(tex_name_.as_ptr(), TTInputFormat::OVF, 0i32));
    if vf_handle.is_none() {
        return -1i32;
    }
    let mut vf_handle = vf_handle.unwrap();
    if verbose as i32 == 1i32 {
        eprint!("(VF:{}", tex_name);
    }
    let thisfont = vf_fonts.len();
    /* Initialize some pointers and such */
    vf_fonts.push(vf {
        tex_name: tex_name.to_string(),
        ptsize,
        design_size: 0,
        dev_fonts: Vec::new(),
        ch_pkt: Vec::new(),
    });
    read_header(&mut vf_handle, thisfont as i32);
    process_vf_file(&mut vf_handle, thisfont as i32);
    if verbose != 0 {
        eprint!(")");
    }
    ttstub_input_close(vf_handle);
    thisfont as i32
}
unsafe fn unsigned_byte(slice: &mut &[u8]) -> i32 {
    if !slice.is_empty() {
        let fresh10 = slice[0];
        *slice = &slice[1..];
        return fresh10 as i32;
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
}
unsafe fn get_pkt_signed_num(slice: &mut &[u8], num: u8) -> i32 {
    let mut val;
    if slice.len() > num as usize {
        val = slice[0] as i32;
        *slice = &slice[1..];
        if val > 0x7f {
            val -= 0x100
        }
        if 1 <= num && num <= 3 {
            for _ in 0..num {
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
            }
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn get_pkt_unsigned_num(slice: &mut &[u8], num: u8) -> i32 {
    let mut val;
    if slice.len() > num as usize {
        val = slice[0] as i32;
        *slice = &slice[1..];
        match num as i32 {
            3 => {
                if val > 0x7f {
                    val -= 0x100
                }
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
            }
            2 => {
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
            }
            1 => {
                val = (val << 8) | slice[0] as i32;
                *slice = &slice[1..];
            }
            _ => {}
        }
    } else {
        panic!("Premature end of DVI byte stream in VF font\n");
    }
    val
}
unsafe fn vf_putrule(slice: &mut &[u8], ptsize: spt_t) {
    let height: i32 = get_pkt_signed_num(slice, 3_u8);
    let width: i32 = get_pkt_signed_num(slice, 3_u8);
    dvi_rule(sqxfw(ptsize, width), sqxfw(ptsize, height));
}
unsafe fn vf_setrule(slice: &mut &[u8], ptsize: spt_t) {
    let height: i32 = get_pkt_signed_num(slice, 3_u8);
    let s_width: i32 = sqxfw(ptsize, get_pkt_signed_num(slice, 3_u8));
    dvi_rule(s_width, sqxfw(ptsize, height));
    dvi_right(s_width);
}
unsafe fn vf_fnt(font_id: i32, vf_font: i32) {
    let mut i = 0;
    while i < vf_fonts[vf_font as usize].dev_fonts.len() {
        if font_id == vf_fonts[vf_font as usize].dev_fonts[i].font_id {
            break;
        }
        i += 1;
    }
    if i < vf_fonts[vf_font as usize].dev_fonts.len() {
        /* Font was found */
        dvi_set_font(vf_fonts[vf_font as usize].dev_fonts[i].dev_id);
    } else {
        eprintln!("Font_id: {} not found in VF", font_id);
    };
}
/* identical to do_xxx in dvi.c */
unsafe fn vf_xxx(len: usize, slice: &mut &[u8]) {
    if slice.len() >= len {
        let buffer = Vec::from(&slice[..len]);
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
    *slice = &slice[len..];
}

pub(crate) unsafe fn vf_set_char(ch: i32, vf_font: i32) {
    let mut default_font: i32 = -1i32;
    if (vf_font as usize) < vf_fonts.len() {
        /* Initialize to the first font or -1 if undefined */
        let ptsize = vf_fonts[vf_font as usize].ptsize;
        if vf_fonts[vf_font as usize].dev_fonts.len() > 0 {
            default_font = vf_fonts[vf_font as usize].dev_fonts[0].dev_id
        }
        dvi_vf_init(default_font);
        let mut slice: &[u8] = &[];
        if (ch as usize) >= vf_fonts[vf_font as usize].ch_pkt.len() || {
            slice = vf_fonts[vf_font as usize].ch_pkt[ch as usize].as_slice();
            slice.is_empty()
        } {
            eprint!("\nchar=0x{ch:x}({ch})\n", ch = ch);
            eprint!("Tried to set a nonexistent character in a virtual font");
        }
        while !slice.is_empty() {
            let opcode = slice[0];
            slice = &slice[1..];
            match opcode {
                SET1 | SET2 | SET3 => {
                    dvi_set(get_pkt_unsigned_num(&mut slice, opcode - SET1));
                }
                SET4 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                SET_RULE => {
                    vf_setrule(&mut slice, ptsize);
                }
                PUT1 | PUT2 | PUT3 => {
                    dvi_put(get_pkt_unsigned_num(&mut slice, opcode - PUT1));
                }
                PUT4 => {
                    panic!("Multibyte (>24 bits) character in VF packet.\nI can\'t handle this!");
                }
                PUT_RULE => {
                    vf_putrule(&mut slice, ptsize);
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
                        get_pkt_signed_num(&mut slice, opcode - RIGHT1),
                    ));
                }
                W0 => {
                    dvi_w0();
                }
                W1 | W2 | W3 | W4 => {
                    dvi_w(sqxfw(ptsize, get_pkt_signed_num(&mut slice, opcode - W1)));
                }
                X0 => {
                    dvi_x0();
                }
                X1 | X2 | X3 | X4 => {
                    dvi_x(sqxfw(ptsize, get_pkt_signed_num(&mut slice, opcode - X1)));
                }
                DOWN1 | DOWN2 | DOWN3 | DOWN4 => {
                    dvi_down(sqxfw(
                        ptsize,
                        get_pkt_signed_num(&mut slice, opcode - DOWN1),
                    ));
                }
                Y0 => {
                    dvi_y0();
                }
                Y1 | Y2 | Y3 | Y4 => {
                    dvi_y(sqxfw(ptsize, get_pkt_signed_num(&mut slice, opcode - Y1)));
                }
                Z0 => {
                    dvi_z0();
                }
                Z1 | Z2 | Z3 | Z4 => {
                    dvi_z(sqxfw(ptsize, get_pkt_signed_num(&mut slice, opcode - Z1)));
                }
                FNT1 | FNT2 | FNT3 | FNT4 => {
                    vf_fnt(get_pkt_signed_num(&mut slice, opcode - FNT1), vf_font);
                }
                XXX1 | XXX2 | XXX3 | XXX4 => {
                    let len = get_pkt_unsigned_num(&mut slice, opcode - XXX1);
                    if len < 0 {
                        warn!("VF: Special with {} bytes???", len);
                    } else {
                        vf_xxx(len as usize, &mut slice);
                    }
                }
                PTEXDIR => {
                    dvi_dirchg(unsigned_byte(&mut slice) as u8);
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

pub(crate) unsafe fn vf_close_all_fonts() {
    for i in 0..vf_fonts.len() {
        /* Release the packet for each character */
        vf_fonts[i].ch_pkt = Vec::new();
        vf_fonts[i].dev_fonts = Vec::new();
    }
    vf_fonts = Vec::new();
}
