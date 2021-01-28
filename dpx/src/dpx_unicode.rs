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

pub(crate) unsafe fn UC_UTF8_is_valid_string(mut p: *const u8, endptr: *const u8) -> bool {
    if p.offset(1) >= endptr {
        return false;
    }
    while p < endptr {
        let ucv: i32 = UC_UTF8_decode_char(&mut p, endptr);
        if !UC_is_valid(ucv) {
            return false;
        }
    }
    true
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

pub(crate) unsafe fn UC_UTF16BE_encode_char(
    mut ucv: i32,
    pp: &mut *mut u8,
    endptr: *mut u8,
) -> size_t {
    let p: *mut u8 = *pp;
    let count = if ucv >= 0 && ucv <= 0xffff {
        if p.offset(2) >= endptr {
            return 0 as size_t;
        }
        *p.offset(0) = (ucv >> 8 & 0xff) as u8;
        *p.offset(1) = (ucv & 0xff) as u8;
        2
    } else if ucv >= 0x10000 && ucv <= 0x10ffff {
        if p.offset(4) >= endptr {
            return 0 as size_t;
        }
        ucv -= 0x10000;
        let high = ((ucv >> 10) as u32).wrapping_add(0xd800u32) as u16;
        let low = (ucv as u32 & 0x3ffu32).wrapping_add(0xdc00u32) as u16;
        *p.offset(0) = (high as i32 >> 8 & 0xff) as u8;
        *p.offset(1) = (high as i32 & 0xff) as u8;
        *p.offset(2) = (low as i32 >> 8 & 0xff) as u8;
        *p.offset(3) = (low as i32 & 0xff) as u8;
        4
    } else {
        if p.offset(2) >= endptr {
            return 0 as size_t;
        }
        *p.offset(0) = (0xfffd >> 8 & 0xff) as u8;
        *p.offset(1) = (0xfffd & 0xff) as u8;
        2
    };
    *pp = (*pp).offset(count as isize);
    count as size_t
}

pub(crate) unsafe fn UC_UTF8_decode_char(pp: *mut *const u8, endptr: *const u8) -> i32 {
    let mut p: *const u8 = *pp;
    let mut c: u8 = *p;
    p = p.offset(1);
    let (mut ucv, nbytes) = if c <= 0x7f {
        (c as i32, 0)
    } else if c & 0xe0 == 0xc0 {
        /* 110x xxxx */
        (c as i32 & 31, 1)
    } else if c & 0xf0 == 0xe0 {
        /* 1110 xxxx */
        (c as i32 & 0xf, 2)
    } else if c & 0xf8 == 0xf0 {
        /* 1111 0xxx */
        (c as i32 & 0x7, 3)
    } else if c & 0xfc == 0xf8 {
        /* 1111 10xx */
        (c as i32 & 0x3, 4)
    } else if c & 0xfe == 0xfc {
        /* 1111 110x */
        (c as i32 & 0x1, 5)
    } else {
        return -1;
    };
    if p.offset(nbytes as isize) > endptr {
        return -1;
    }
    for _ in 0..nbytes {
        c = *p;
        if c as i32 & 0xc0 != 0x80 {
            return -1;
        }
        p = p.offset(1);
        ucv = ucv << 6 | c as i32 & 0x3f
    }
    *pp = p;
    ucv
}
