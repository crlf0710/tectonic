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
use super::dpx_dpxutil::{is_delim, is_space, skip_white_spaces};
use super::dpx_mem::new;
use super::dpx_pst_obj::pst_obj;
use super::dpx_pst_obj::{
    pst_new_mark, pst_parse_boolean, pst_parse_name, pst_parse_null, pst_parse_number,
    pst_parse_string,
};
use libc::memcpy;

pub(crate) fn pst_token_end(s: u8) -> bool {
    is_delim(&s) || is_space(&s)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum PstType {
    Unknown = -1,
    Null = 0,
    Boolean = 1,
    Integer = 2,
    Real = 3,
    String = 5,
    Name = 6,
    Mark = 7,
}

unsafe fn pst_parse_any(inbuf: *mut *mut u8, inbufend: *mut u8) -> pst_obj {
    let mut cur: *mut u8 = *inbuf;
    while cur < inbufend && !(cur == inbufend || pst_token_end(*cur)) {
        cur = cur.offset(1)
    }
    let len = cur.offset_from(*inbuf) as i64 as u32;
    let slice = std::slice::from_raw_parts(*inbuf, len as _);
    let data = Vec::from(slice);
    *data.offset(len as isize) = 0;
    *inbuf = cur;
    pst_obj::new(PstType::Unknown, Box::into_raw(data.into_boxed_slice()) as *mut libc::c_void)
}
unsafe fn skip_line(inbuf: *mut *mut u8, inbufend: *mut u8) {
    while *inbuf < inbufend && **inbuf != b'\n' && **inbuf != b'\r' {
        *inbuf = (*inbuf).offset(1)
    }
    if *inbuf < inbufend && **inbuf == b'\r' {
        *inbuf = (*inbuf).offset(1)
    }
    if *inbuf < inbufend && **inbuf == b'\n' {
        *inbuf = (*inbuf).offset(1)
    };
}
unsafe fn skip_comments(inbuf: *mut *mut u8, inbufend: *mut u8) {
    while *inbuf < inbufend && **inbuf == b'%' {
        skip_line(inbuf, inbufend);
        skip_white_spaces(inbuf, inbufend);
    }
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */

pub(crate) unsafe fn pst_get_token(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj> {
    let mut obj = None;
    assert!(*inbuf <= inbufend && *inbufend == 0);
    skip_white_spaces(inbuf, inbufend);
    skip_comments(inbuf, inbufend);
    if *inbuf >= inbufend {
        return None;
    }
    let mut c = **inbuf;
    match c {
        b'/' => obj = pst_parse_name(inbuf, inbufend),
        b'[' | b'{' => {
            /* This is wrong */
            obj = Some(pst_new_mark());
            *inbuf = (*inbuf).offset(1)
        }
        b'<' => {
            if (*inbuf).offset(1) >= inbufend {
                return None;
            }
            c = *(*inbuf).offset(1);
            if c == b'<' {
                obj = Some(pst_new_mark());
                *inbuf = (*inbuf).offset(2)
            } else if c.is_ascii_hexdigit() {
                obj = pst_parse_string(inbuf, inbufend)
            } else if c == b'~' {
                /* ASCII85 */
                obj = pst_parse_string(inbuf, inbufend)
            }
        }
        b'(' => obj = pst_parse_string(inbuf, inbufend),
        b'>' => {
            if (*inbuf).offset(1) >= inbufend || *(*inbuf).offset(1) != b'>' {
                panic!("Unexpected end of ASCII hex string marker.");
            } else {
                let mark = Vec::from(b">>");
                obj = Some(pst_obj::new(PstType::Unknown, Box::into_raw(mark.into_boxed_slice()) as *mut libc::c_void));
                *inbuf = (*inbuf).offset(2)
            }
        }
        b']' | b'}' => {
            let mark = vec![c];
            obj = Some(pst_obj::new(PstType::Unknown, Box::into_raw(mark.into_boxed_slice()) as *mut libc::c_void));
            *inbuf = (*inbuf).offset(1)
        }
        _ => {
            if c == b't' || c == b'f' {
                obj = pst_parse_boolean(inbuf, inbufend)
            } else if c == b'n' {
                obj = pst_parse_null(inbuf, inbufend)
            } else if c == b'+' || c == b'-' || c.is_ascii_digit() || c == b'.' {
                obj = pst_parse_number(inbuf, inbufend)
            }
        }
    }
    obj.or_else(|| Some(pst_parse_any(inbuf, inbufend)))
}
