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
    non_camel_case_types,
)]

use crate::bridge::ttstub_input_getc;
use crate::bridge::InputHandleWrapper;
use std::io::Read;
pub(crate) type fixword = i32;

pub(crate) trait GetFromFile {
    fn get<R>(file: &mut R) -> Self
    where
        R: Read;
}

impl GetFromFile for u8 {
    fn get<R>(file: &mut R) -> Self
    where
        R: Read,
    {
        let mut buf = [0u8; 1];
        file.read(&mut buf).expect("File ended prematurely");
        buf[0]
    }
}

pub(crate) fn skip_bytes<R>(mut n: u32, file: &mut R)
where
    R: Read,
{
    loop {
        let fresh0 = n;
        n = n.wrapping_sub(1);
        if !(fresh0 > 0_u32) {
            break;
        }
        u8::get(file);
    }
}

pub(crate) fn get_signed_byte<R>(file: &mut R) -> i8
where
    R: Read,
{
    let mut byte = u8::get(file) as i32;
    if byte >= 0x80i32 {
        byte -= 0x100i32
    }
    byte as i8
}

pub(crate) fn get_unsigned_pair<R>(file: &mut R) -> u16
where
    R: Read,
{
    let mut pair: u16 = u8::get(file) as u16;
    pair = ((pair as i32) << 8i32 | u8::get(file) as i32) as u16;
    pair
}

pub(crate) fn get_signed_pair<R>(file: &mut R) -> i16
where
    R: Read,
{
    let mut pair: i16 = get_signed_byte(file) as i16;
    pair = ((pair as i32) << 8i32 | u8::get(file) as i32) as i16;
    pair
}

pub(crate) fn get_unsigned_triple<R>(file: &mut R) -> u32
where
    R: Read,
{
    let mut triple: u32 = 0_u32;
    for _ in 0..3 {
        triple = triple << 8i32 | u8::get(file) as u32;
    }
    triple
}

pub(crate) fn get_signed_triple<R>(file: &mut R) -> i32
where
    R: Read,
{
    let mut triple: i32 = get_signed_byte(file) as i32;
    for _ in 0..2 {
        triple = triple << 8i32 | u8::get(file) as i32;
    }
    triple
}

pub(crate) fn get_signed_quad<R>(file: &mut R) -> i32
where
    R: Read,
{
    let mut quad: i32 = get_signed_byte(file) as i32;
    for _ in 0..3 {
        quad = quad << 8i32 | u8::get(file) as i32;
    }
    quad
}

pub(crate) fn get_unsigned_quad<R>(file: &mut R) -> u32
where
    R: Read,
{
    let mut quad = 0u32;
    for _ in 0..4 {
        quad = quad << 8i32 | u8::get(file) as u32;
    }
    quad
}

pub(crate) fn get_unsigned_num<R>(file: &mut R, num: u8) -> u32
where
    R: Read,
{
    let mut val = u8::get(file) as u32;
    match num {
        3 => {
            if val > 0x7f_u32 {
                val = val.wrapping_sub(0x100_u32);
            }
            val = (val << 8) | u8::get(file) as u32;
            val = (val << 8) | u8::get(file) as u32;
            val = (val << 8) | u8::get(file) as u32;
        }
        2 => {
            val = (val << 8) | u8::get(file) as u32;
            val = (val << 8) | u8::get(file) as u32;
        }
        1 => {
            val = (val << 8) | u8::get(file) as u32;
        }
        _ => {}
    }
    val
}
/* Compute a signed quad that must be positive */

pub(crate) fn get_positive_quad<R>(file: &mut R, type_0: &str, name: &str) -> u32
where
    R: Read,
{
    let val: i32 = get_signed_quad(file);
    if val < 0i32 {
        panic!("Bad {}: negative {}: {}", type_0, name, val,);
    }
    val as u32
}

pub(crate) fn sqxfw(mut sq: i32, mut fw: fixword) -> i32 {
    let mut sign: i32 = 1i32;
    /* Make positive. */
    if sq < 0i32 {
        sign = -sign; /* 1<<3 is for rounding */
        sq = -sq
    }
    if fw < 0i32 {
        sign = -sign;
        fw = -fw
    }
    let a = sq as u32 >> 16i32;
    let b = sq as u32 & 0xffffu32;
    let c = fw as u32 >> 16i32;
    let d = fw as u32 & 0xffffu32;
    let ad = a.wrapping_mul(d);
    let bd = b.wrapping_mul(d);
    let bc = b.wrapping_mul(c);
    let ac = a.wrapping_mul(c);
    let e = bd >> 16i32;
    let f = ad >> 16i32;
    let g = ad & 0xffffu32;
    let h = bc >> 16i32;
    let i = bc & 0xffffu32;
    let j = ac >> 16i32;
    let k = ac & 0xffffu32;
    let mut result = (e
        .wrapping_add(g)
        .wrapping_add(i)
        .wrapping_add((1i32 << 3i32) as u32)
        >> 4i32) as i32;
    result = (result as u32).wrapping_add(f.wrapping_add(h).wrapping_add(k) << 12i32) as i32 as i32;
    result = (result as u32).wrapping_add(j << 28i32) as i32 as i32;
    if sign > 0i32 {
        result
    } else {
        -result
    }
}
/* Tectonic-ified versions */

pub(crate) fn tt_skip_bytes(mut n: u32, handle: &InputHandleWrapper) {
    loop {
        let fresh3 = n;
        n = n.wrapping_sub(1);
        if !(fresh3 > 0_u32) {
            break;
        }
        tt_get_unsigned_byte(handle);
    }
}

pub(crate) fn tt_get_unsigned_byte(handle: &InputHandleWrapper) -> u8 {
    let ch = ttstub_input_getc(handle);
    if ch < 0 {
        panic!("File ended prematurely\n");
    }
    ch as u8
}

pub(crate) fn tt_get_signed_byte(handle: &InputHandleWrapper) -> i8 {
    let mut byte = tt_get_unsigned_byte(handle) as i32;
    if byte >= 0x80 {
        byte -= 0x100
    }
    byte as i8
}

pub(crate) fn tt_get_unsigned_pair(handle: &InputHandleWrapper) -> u16 {
    let mut pair: u16 = tt_get_unsigned_byte(handle) as u16;
    pair = ((pair as i32) << 8 | tt_get_unsigned_byte(handle) as i32) as u16;
    pair
}

pub(crate) fn tt_get_signed_pair(handle: &InputHandleWrapper) -> i16 {
    let mut pair: i16 = tt_get_signed_byte(handle) as i16;
    pair = ((pair as i32) << 8 | tt_get_unsigned_byte(handle) as i32) as i16;
    pair
}

pub(crate) fn tt_get_unsigned_quad(handle: &InputHandleWrapper) -> u32 {
    let mut quad = 0_u32;
    for _ in 0..4 {
        quad = quad << 8 | tt_get_unsigned_byte(handle) as u32;
    }
    quad
}

pub(crate) fn tt_get_signed_quad(handle: &InputHandleWrapper) -> i32 {
    let mut quad: i32 = tt_get_signed_byte(handle) as i32;
    for _ in 0..3 {
        quad = quad << 8i32 | tt_get_unsigned_byte(handle) as i32;
    }
    quad
}

pub(crate) fn tt_get_unsigned_num(handle: &InputHandleWrapper, num: u8) -> u32 {
    let mut val: u32 = tt_get_unsigned_byte(handle) as u32;
    match num {
        3 => {
            if val > 0x7f_u32 {
                val = val.wrapping_sub(0x100_u32);
            }
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
        }
        2 => {
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
        }
        1 => {
            val = (val << 8) | tt_get_unsigned_byte(handle) as u32;
        }
        _ => {}
    }
    val
}
/* When reading numbers from binary files 1, 2, or 3 bytes are
   interpreted as either signed or unsigned.

   Four bytes from DVI, PK, TFM, or VF files always yield a signed
   32-bit integer (i32), but some of them must not be negative.

   Four byte numbers from JPEG2000, OpenType, or TrueType files are
   mostly unsigned (u32) and occasionally signed (i32).
*/
/* Tectonic enabled */

pub(crate) fn tt_get_positive_quad(handle: &InputHandleWrapper, type_0: &str, name: &str) -> u32 {
    let val: i32 = tt_get_signed_quad(handle);
    if val < 0i32 {
        panic!("Bad {}: negative {}: {}", type_0, name, val,);
    }
    val as u32
}
