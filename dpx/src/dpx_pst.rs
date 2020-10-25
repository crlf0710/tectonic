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

use std::ffi::CString;

use super::dpx_dpxutil::{is_delim, is_space, skip_white_spaces, xtoi};
use crate::bridge::stub_errno as errno;
use crate::warn;

use crate::shims::sprintf;
use libc::{strtod, strtol};
use std::ptr;

#[derive(Clone, Debug)]
pub(crate) enum PstObj {
    String(String),
    Name(CString),
    Real(f64),
    Integer(i32),
    Boolean(bool),
    Mark,
    Null,
    Unknown(Box<[u8]>),
}

impl PstObj {
    pub(crate) unsafe fn into_string(self) -> String {
        match self {
            PstObj::Boolean(data) => format!("{}", data),
            PstObj::Integer(data) => format!("{}", data),
            PstObj::Real(data) => pst_real_SV(data),
            PstObj::Name(data) => data.into_string().unwrap(),
            PstObj::String(data) => data,
            PstObj::Null | PstObj::Mark => panic!("Operation not defined for this type of object."),
            PstObj::Unknown(data) => String::from_utf8_lossy(&data).into(),
        }
    }
}

/* BOOLEAN */
pub(crate) unsafe fn pst_parse_boolean(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj> {
    let slice = std::slice::from_raw_parts(*inbuf, inbufend.offset_from(*inbuf) as usize);
    if slice.len() >= 4
        && slice.starts_with(b"true")
        && (slice.len() == 4 || pst_token_end(slice[4]))
    {
        *inbuf = (*inbuf).offset(4);
        Some(PstObj::Boolean(true))
    } else if slice.len() >= 5
        && slice.starts_with(b"false")
        && (slice.len() == 5 || pst_token_end(slice[5]))
    {
        *inbuf = (*inbuf).offset(5);
        Some(PstObj::Boolean(false))
    } else {
        None
    }
}
/* NULL */

pub(crate) unsafe fn pst_parse_null(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj> {
    let slice = std::slice::from_raw_parts(*inbuf, inbufend.offset_from(*inbuf) as usize);
    if slice.len() >= 4
        && slice.starts_with(b"null")
        && (slice.len() == 4 || pst_token_end(slice[4]))
    {
        *inbuf = (*inbuf).offset(4);
        Some(PstObj::Null)
    } else {
        None
    }
}
/* NUMBERS */
/* REAL */
unsafe fn pst_real_SV(obj: f64) -> String {
    let mut fmt_buf: [u8; 15] = [0; 15];
    let len = sprintf(
        fmt_buf.as_mut_ptr() as *mut i8,
        b"%.5g\x00" as *const u8 as *const i8,
        obj,
    ) as usize;
    String::from_utf8_lossy(&fmt_buf[..len]).into()
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */
/* leading white-space is ignored */

pub(crate) unsafe fn pst_parse_number(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj> {
    let mut cur: *mut u8 = ptr::null_mut();
    errno::set_errno(errno::ZERO);
    let mut lval = strtol(
        *inbuf as *mut i8,
        &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        10i32,
    ) as i32;
    if errno::errno() != errno::ZERO || b".eE".contains(&*cur) {
        /* real */
        errno::set_errno(errno::ZERO);
        let dval = strtod(
            *inbuf as *mut i8,
            &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        );
        if errno::errno() == errno::ZERO && (cur == inbufend || pst_token_end(*cur)) {
            *inbuf = cur;
            return Some(PstObj::Real(dval));
        }
    } else if cur != *inbuf && (cur == inbufend || pst_token_end(*cur)) {
        /* integer */
        *inbuf = cur;
        return Some(PstObj::Integer(lval));
    } else {
        if lval >= 2i32
            && lval <= 36i32
            && *cur as i32 == '#' as i32
            && {
                cur = cur.offset(1);
                libc::isalnum(*cur as _) != 0
            }
            && (lval != 16i32
                || *cur.offset(1) as i32 != 'x' as i32 && *cur.offset(1) as i32 != 'X' as i32)
        {
            /* integer with radix */
            /* Can the base have a (plus) sign? I think yes. */
            errno::set_errno(errno::ZERO);
            lval = strtol(
                cur as *mut i8,
                &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
                lval,
            ) as i32;
            if errno::errno() == errno::ZERO && (cur == inbufend || pst_token_end(*cur)) {
                *inbuf = cur;
                return Some(PstObj::Integer(lval));
            }
        }
    }
    /* error */
    None
}
/* NAME */
/* NAME */
/*
 * \0 is not allowed for name object.
 */
unsafe fn getxpair(s: *mut *mut u8) -> i32 {
    let hi = xtoi(**s);
    if hi < 0i32 {
        return hi;
    }
    *s = (*s).offset(1);
    let lo = xtoi(**s);
    if lo < 0i32 {
        return lo;
    }
    *s = (*s).offset(1);
    hi << 4i32 | lo
}

pub(crate) unsafe fn pst_parse_name(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj>
/* / is required */ {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    if *cur as i32 != '/' as i32 {
        return None;
    }
    cur = cur.offset(1);
    while !(cur == inbufend || pst_token_end(*cur)) {
        let mut c = *cur;
        cur = cur.offset(1);
        if c as i32 == '#' as i32 {
            if cur.offset(2) >= inbufend {
                warn!("Premature end of input name string.");
                break;
            } else {
                let val = getxpair(&mut cur);
                if val <= 0 {
                    warn!("Invalid char for name object. (ignored)");
                    continue;
                } else {
                    c = val as u8;
                }
            }
        }
        if wbuf.len() < 127 {
            wbuf.push(c);
        }
    }
    if wbuf.len() > 127 {
        warn!("String too long for name object. Output will be truncated.");
    }
    *inbuf = cur;
    Some(PstObj::Name(CString::new(wbuf).unwrap()))
}
/* STRING */
/*
 * TODO: ascii85 string <~ .... ~>
 */
pub(crate) unsafe fn pst_parse_string(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj> {
    if (*inbuf).offset(2) >= inbufend {
        return None;
    } else {
        if **inbuf as i32 == '(' as i32 {
            return pst_string_parse_literal(inbuf, inbufend).map(PstObj::String);
        } else {
            if **inbuf as i32 == '<' as i32 && *(*inbuf).offset(1) as i32 == '~' as i32 {
                panic!("ASCII85 string not supported yet.");
            } else {
                if **inbuf as i32 == '<' as i32 {
                    return pst_string_parse_hex(inbuf, inbufend).map(PstObj::String);
                }
            }
        }
    }
    None
}
/* Overflowed value is set to invalid char.  */
unsafe fn ostrtouc(inbuf: *mut *mut u8, inbufend: *mut u8, valid: *mut u8) -> u8 {
    let mut cur: *mut u8 = *inbuf;
    let mut val: u32 = 0_u32;
    while cur < inbufend && cur < (*inbuf).offset(3) && (*cur >= b'0' && *cur <= b'7') {
        val = val << 3 | (*cur - b'0') as u32;
        cur = cur.offset(1)
    }
    *valid = if val > 255 || cur == *inbuf { 0 } else { 1 };
    *inbuf = cur;
    val as u8
}
unsafe fn esctouc(inbuf: *mut *mut u8, inbufend: *mut u8, valid: *mut u8) -> u8 {
    let unescaped;
    let escaped = **inbuf;
    *valid = 1_u8;
    match escaped {
        b'\\' | b')' | b'(' => {
            /* Backslash, unbalanced paranthes */
            unescaped = escaped;
            *inbuf = (*inbuf).offset(1)
        }
        b'n' => {
            /* Other escaped char */
            unescaped = b'\n';
            *inbuf = (*inbuf).offset(1)
        }
        b'r' => {
            unescaped = b'\r';
            *inbuf = (*inbuf).offset(1)
        }
        b't' => {
            unescaped = b'\t';
            *inbuf = (*inbuf).offset(1)
        }
        b'b' => {
            unescaped = '\u{8}' as u8;
            *inbuf = (*inbuf).offset(1)
        }
        b'f' => {
            unescaped = '\u{c}' as u8;
            *inbuf = (*inbuf).offset(1)
        }
        b'\r' => {
            /*
             * An end-of-line marker preceeded by backslash is not part of a
             * literal string
             */
            unescaped = 0_u8;
            *valid = 0_u8;
            *inbuf = (*inbuf).offset(
                (if *inbuf < inbufend.offset(-1) && *(*inbuf).offset(1) as i32 == '\n' as i32 {
                    2i32
                } else {
                    1i32
                }) as isize,
            )
        }
        b'\n' => {
            unescaped = 0_u8;
            *valid = 0_u8;
            *inbuf = (*inbuf).offset(1)
        }
        _ => {
            /* Possibly octal notion */
            unescaped = ostrtouc(inbuf, inbufend, valid)
        }
    }
    unescaped
}
/* STRING */
unsafe fn pst_string_parse_literal(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<String> {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    let mut c: u8 = 0_u8;
    let mut balance: i32 = 1i32;
    if cur.offset(2) > inbufend || *cur as i32 != '(' as i32 {
        return None;
    }
    cur = cur.offset(1);
    while cur < inbufend && wbuf.len() < 4096 && balance > 0 {
        c = *cur;
        cur = cur.offset(1);
        match c {
            b'\\' => {
                let mut valid: u8 = 0;
                let unescaped = esctouc(&mut cur, inbufend, &mut valid);
                if valid != 0 {
                    wbuf.push(unescaped);
                }
            }
            b'(' => {
                balance += 1;
                wbuf.push(b'(');
            }
            b')' => {
                balance -= 1;
                if balance > 0 {
                    wbuf.push(b')');
                }
            }
            b'\r' => {
                /*
                 * An end-of-line marker (\n, \r or \r\n), not preceeded by a backslash,
                 * must be converted to single \n.
                 */
                if cur < inbufend && *cur as i32 == '\n' as i32 {
                    cur = cur.offset(1)
                }
                wbuf.push(b'\n');
            }
            _ => {
                wbuf.push(c);
            }
        }
    }
    if c != b')' {
        return None;
    }
    *inbuf = cur;
    Some(String::from_utf8(wbuf).unwrap())
}
unsafe fn pst_string_parse_hex(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<String> {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    if cur.offset(2) > inbufend || *cur != b'<' || *cur == b'<' && *cur.offset(1) == b'<' {
        return None;
    }
    cur = cur.offset(1);
    /* PDF Reference does not specify how to treat invalid char */
    while cur < inbufend && wbuf.len() < 4096 {
        skip_white_spaces(&mut cur, inbufend);
        if *cur == b'>' {
            break;
        }
        let byte = *cur;
        cur = cur.offset(1);
        let mut hi = xtoi(byte);
        if hi < 0i32 {
            warn!(
                "Invalid char for hex string <{:x}> treated as <0>.",
                *cur.offset(-1) as i32,
            );
            hi = 0;
        }
        skip_white_spaces(&mut cur, inbufend);
        if *cur == b'>' {
            break;
        }
        /* 0 is appended if final hex digit is missing */
        let mut lo = if cur < inbufend {
            let byte = *cur;
            cur = cur.offset(1);
            xtoi(byte)
        } else {
            0
        };
        if lo < 0 {
            warn!(
                "Invalid char for hex string <{:x}> treated as <0>.",
                *cur.offset(-1) as i32,
            );
            lo = 0;
        }
        wbuf.push((hi << 4 | lo) as u8);
    }
    if *cur != b'>' {
        return None;
    }
    cur = cur.offset(1);
    *inbuf = cur;
    Some(String::from_utf8(wbuf).unwrap())
}
unsafe fn pst_string_RV(obj: &String) -> f64 {
    let mut p = obj.as_ptr() as *mut u8;
    let end = p.offset(obj.len() as isize);
    match pst_parse_number(&mut p, end) {
        Some(nobj) if p == end => match nobj {
            PstObj::Integer(data) => data as f64,
            PstObj::Real(data) => data,
            _ => unreachable!(),
        },
        _ => panic!("Cound not convert string to real value."),
    }
}

pub(crate) fn pst_token_end(s: u8) -> bool {
    is_delim(&s) || is_space(&s)
}

unsafe fn pst_parse_any(inbuf: *mut *mut u8, inbufend: *mut u8) -> PstObj {
    let mut cur: *mut u8 = *inbuf;
    while cur < inbufend && !(cur == inbufend || pst_token_end(*cur)) {
        cur = cur.offset(1)
    }
    let len = cur.offset_from(*inbuf) as i64 as u32;
    let slice = std::slice::from_raw_parts(*inbuf, len as _);
    let data = Vec::from(slice);
    *inbuf = cur;
    PstObj::Unknown(data.into_boxed_slice())
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

pub(crate) unsafe fn pst_get_token(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<PstObj> {
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
            obj = Some(PstObj::Mark);
            *inbuf = (*inbuf).offset(1)
        }
        b'<' => {
            if (*inbuf).offset(1) >= inbufend {
                return None;
            }
            c = *(*inbuf).offset(1);
            if c == b'<' {
                obj = Some(PstObj::Mark);
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
                let mark = Vec::from(&b">>"[..]);
                obj = Some(PstObj::Unknown(mark.into_boxed_slice()));
                *inbuf = (*inbuf).offset(2)
            }
        }
        b']' | b'}' => {
            let mark = vec![c];
            obj = Some(PstObj::Unknown(mark.into_boxed_slice()));
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
