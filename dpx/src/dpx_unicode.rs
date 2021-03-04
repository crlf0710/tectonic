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
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::bridge::size_t;

pub(crate) unsafe fn UC_is_valid(ucv: i32) -> bool {
    !(ucv < 0 || ucv as i64 > 0x10ffff || ucv as i64 >= 0xd800 && ucv as i64 <= 0xdfff)
}

pub(crate) unsafe fn UC_UTF16BE_is_valid_string(slice: &[u8]) -> bool {
    if slice.len() < 2 || slice.len() % 2 != 0 {
        return false;
    }
    for c in std::char::decode_utf16(
        slice
            .chunks_exact(2)
            .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]])),
    ) {
        if c.is_err() {
            return false;
        }
    }
    true
}

pub(crate) unsafe fn UC_UTF8_is_valid_string(slice: &[u8]) -> bool {
    if slice.is_empty() {
        return false;
    }
    std::str::from_utf8(slice).is_ok()
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
