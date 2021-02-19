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

use crate::bridge::size_t;

pub(crate) unsafe fn UC_is_valid(ucv: i32) -> bool {
    !(ucv < 0 || ucv as i64 > 0x10ffff || ucv as i64 >= 0xd800 && ucv as i64 <= 0xdfff)
}

pub(crate) unsafe fn UC_UTF16BE_is_valid_string(mut p: *const u8, endptr: *const u8) -> bool {
    if p.offset(1) >= endptr {
        return false;
    }
    while p < endptr {
        let ucv: i32 = UC_UTF16BE_decode_char(&mut p, endptr);
        if !UC_is_valid(ucv) {
            return false;
        }
    }
    true
}

pub(crate) unsafe fn UC_UTF8_is_valid_string(p: *const u8, endptr: *const u8) -> bool {
    if p.offset(1) >= endptr {
        return false;
    }
    std::str::from_utf8(std::slice::from_raw_parts(p, endptr.offset_from(p) as _)).is_ok()
}

pub(crate) unsafe fn UC_UTF16BE_decode_char(pp: *mut *const u8, endptr: *const u8) -> i32 {
    let mut p: *const u8 = *pp;
    let mut ucv;
    if p.offset(1) >= endptr {
        return -1;
    }
    let first = ((*p.offset(0) as i32) << 8 | *p.offset(1) as i32) as u16;
    p = p.offset(2);
    if first as u32 >= 0xd800u32 && (first as u32) < 0xdc00u32 {
        if p.offset(1) >= endptr {
            return -1;
        }
        let second = ((*p.offset(0) as i32) << 8 | *p.offset(1) as i32) as u16;
        p = p.offset(2);
        ucv = (second as u32 & 0x3ffu32) as i32;
        ucv = (ucv as u32 | (first as u32 & 0x3ffu32) << 10) as i32;
        ucv += 0x10000
    } else if first as u32 >= 0xdc00u32 && (first as u32) < 0xe000u32 {
        return -1;
    } else {
        ucv = first as i32
    }
    *pp = p;
    ucv
}

pub(crate) unsafe fn UC_UTF16BE_encode_char(ucv: i32, pp: &mut *mut u8, endptr: *mut u8) -> size_t {
    if let Some(ucv) = std::char::from_u32(ucv as u32) {
        let mut b = [0; 2];
        let c16 = ucv.encode_utf16(&mut b);
        let p: *mut u8 = *pp;
        if p.add(c16.len() * 2) > endptr {
            return 0;
        }
        if c16.len() == 1 {
            let b16 = c16[0].to_be_bytes();
            *p.offset(0) = b16[0];
            *p.offset(1) = b16[1];
            *pp = (*pp).offset(2);
            2
        } else {
            let high = c16[0].to_be_bytes();
            let low = c16[1].to_be_bytes();
            *p.offset(0) = high[0];
            *p.offset(1) = high[1];
            *p.offset(2) = low[0];
            *p.offset(3) = low[1];
            *pp = (*pp).offset(2);
            4
        }
    } else {
        0
    }
}
