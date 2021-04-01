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

use std::ffi::CString;

use super::dpx_dpxutil::{is_delim, is_space, skip_white_spaces, xtoi};
use crate::bridge::stub_errno as errno;
use crate::warn;

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
pub(crate) unsafe fn pst_parse_boolean(inbuf: &mut &[u8]) -> Option<PstObj> {
    if inbuf.len() >= 4
        && inbuf.starts_with(b"true")
        && (inbuf.len() == 4 || pst_token_end(inbuf[4]))
    {
        *inbuf = &inbuf[4..];
        Some(PstObj::Boolean(true))
    } else if inbuf.len() >= 5
        && inbuf.starts_with(b"false")
        && (inbuf.len() == 5 || pst_token_end(inbuf[5]))
    {
        *inbuf = &inbuf[5..];
        Some(PstObj::Boolean(false))
    } else {
        None
    }
}
/* NULL */

pub(crate) unsafe fn pst_parse_null(inbuf: &mut &[u8]) -> Option<PstObj> {
    if inbuf.len() >= 4
        && inbuf.starts_with(b"null")
        && (inbuf.len() == 4 || pst_token_end(inbuf[4]))
    {
        *inbuf = &inbuf[4..];
        Some(PstObj::Null)
    } else {
        None
    }
}
/* NUMBERS */
/* REAL */
unsafe fn pst_real_SV(obj: f64) -> String {
    let mut buf = Vec::<u8>::new();
    crate::g_format::write_engineering(&mut buf, obj, Some(5)).unwrap();
    String::from_utf8(buf).unwrap()
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */
/* leading white-space is ignored */

pub(crate) unsafe fn pst_parse_number(inbuf: &mut &[u8]) -> Option<PstObj> {
    let inbufend = inbuf.as_ptr().add(inbuf.len());
    let mut cur: *mut u8 = ptr::null_mut();
    errno::set_errno(errno::ZERO);
    let mut lval = strtol(
        inbuf.as_ptr() as *const i8,
        &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        10,
    ) as i32;
    if errno::errno() != errno::ZERO || b".eE".contains(&*cur) {
        /* real */
        errno::set_errno(errno::ZERO);
        let dval = strtod(
            inbuf.as_ptr() as *const i8,
            &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        );
        if errno::errno() == errno::ZERO && (cur as *const u8 == inbufend || pst_token_end(*cur)) {
            *inbuf = std::slice::from_raw_parts(cur, inbufend.offset_from(cur) as usize);
            return Some(PstObj::Real(dval));
        }
    } else if cur as *const u8 != inbuf.as_ptr()
        && (cur as *const u8 == inbufend || pst_token_end(*cur))
    {
        /* integer */
        *inbuf = std::slice::from_raw_parts(cur, inbufend.offset_from(cur) as usize);
        return Some(PstObj::Integer(lval));
    } else {
        if lval >= 2
            && lval <= 36
            && *cur as i32 == '#' as i32
            && {
                cur = cur.offset(1);
                (*cur).is_ascii_alphanumeric()
            }
            && (lval != 16
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
            if errno::errno() == errno::ZERO
                && (cur as *const u8 == inbufend || pst_token_end(*cur))
            {
                *inbuf = std::slice::from_raw_parts(cur, inbufend.offset_from(cur) as usize);
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
unsafe fn getxpair(s: &mut &[u8]) -> i32 {
    let hi = xtoi(s[0]);
    if hi < 0 {
        return hi;
    }
    *s = &s[1..];
    let lo = xtoi(s[0]);
    if lo < 0 {
        return lo;
    }
    *s = &s[1..];
    hi << 4 | lo
}

pub(crate) unsafe fn pst_parse_name(inbuf: &mut &[u8]) -> Option<PstObj>
/* / is required */ {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur = *inbuf;
    if cur[0] != b'/' {
        return None;
    }
    cur = &cur[1..];
    while !cur.is_empty() && !pst_token_end(cur[0]) {
        let mut c = cur[0];
        cur = &cur[1..];
        if c == b'#' {
            if cur.len() <= 2 {
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
pub(crate) unsafe fn pst_parse_string(inbuf: &mut &[u8]) -> Option<PstObj> {
    if inbuf.len() <= 2 {
        return None;
    } else {
        if inbuf[0] == b'(' {
            return pst_string_parse_literal(inbuf).map(PstObj::String);
        } else {
            if inbuf[0] == b'<' && inbuf[1] == b'~' {
                panic!("ASCII85 string not supported yet.");
            } else {
                if inbuf[0] == b'<' {
                    return pst_string_parse_hex(inbuf).map(PstObj::String);
                }
            }
        }
    }
    None
}
/* Overflowed value is set to invalid char.  */
unsafe fn ostrtouc(inbuf: &mut &[u8], valid: *mut u8) -> u8 {
    let mut cur = *inbuf;
    let mut val: u32 = 0_u32;
    let mut i = 0;
    while !cur.is_empty() && i < 3 && (cur[0] >= b'0' && cur[0] <= b'7') {
        val = val << 3 | (cur[0] - b'0') as u32;
        cur = &cur[1..];
        i += 1;
    }
    *valid = if val > 255 || i == 0 { 0 } else { 1 };
    *inbuf = cur;
    val as u8
}
unsafe fn esctouc(inbuf: &mut &[u8], valid: *mut u8) -> u8 {
    let unescaped;
    let escaped = inbuf[0];
    *valid = 1_u8;
    match escaped {
        b'\\' | b')' | b'(' => {
            /* Backslash, unbalanced paranthes */
            unescaped = escaped;
            *inbuf = &inbuf[1..];
        }
        b'n' => {
            /* Other escaped char */
            unescaped = b'\n';
            *inbuf = &inbuf[1..];
        }
        b'r' => {
            unescaped = b'\r';
            *inbuf = &inbuf[1..];
        }
        b't' => {
            unescaped = b'\t';
            *inbuf = &inbuf[1..];
        }
        b'b' => {
            unescaped = '\u{8}' as u8;
            *inbuf = &inbuf[1..];
        }
        b'f' => {
            unescaped = '\u{c}' as u8;
            *inbuf = &inbuf[1..];
        }
        b'\r' => {
            /*
             * An end-of-line marker preceeded by backslash is not part of a
             * literal string
             */
            unescaped = 0_u8;
            *valid = 0_u8;
            *inbuf = &inbuf[(if inbuf.len() > 1 && inbuf[1] == b'\n' {
                2
            } else {
                1
            })..];
        }
        b'\n' => {
            unescaped = 0_u8;
            *valid = 0_u8;
            *inbuf = &inbuf[1..];
        }
        _ => {
            /* Possibly octal notion */
            unescaped = ostrtouc(inbuf, valid)
        }
    }
    unescaped
}
/* STRING */
unsafe fn pst_string_parse_literal(inbuf: &mut &[u8]) -> Option<String> {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur = *inbuf;
    let mut c: u8 = 0_u8;
    let mut balance: i32 = 1;
    if cur.len() < 2 || cur[0] != b'(' {
        return None;
    }
    cur = &cur[1..];
    while !cur.is_empty() && wbuf.len() < 4096 && balance > 0 {
        c = cur[0];
        cur = &cur[1..];
        match c {
            b'\\' => {
                let mut valid: u8 = 0;
                let unescaped = esctouc(&mut cur, &mut valid);
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
                if !cur.is_empty() && cur[0] == b'\n' {
                    cur = &cur[1..];
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
unsafe fn pst_string_parse_hex(inbuf: &mut &[u8]) -> Option<String> {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur = *inbuf;
    if cur.len() < 2 || cur[0] != b'<' || cur[0] == b'<' && cur[1] == b'<' {
        return None;
    }
    cur = &cur[1..];
    /* PDF Reference does not specify how to treat invalid char */
    while !cur.is_empty() && wbuf.len() < 4096 {
        skip_white_spaces(&mut cur);
        if cur[0] == b'>' {
            break;
        }
        let byte = cur[0];
        cur = &cur[1..];
        let mut hi = xtoi(byte);
        if hi < 0 {
            warn!(
                "Invalid char for hex string <{:x}> treated as <0>.",
                byte as i32,
            );
            hi = 0;
        }
        skip_white_spaces(&mut cur);
        if cur[0] == b'>' {
            break;
        }
        /* 0 is appended if final hex digit is missing */
        let lo = if !cur.is_empty() {
            let byte = cur[0];
            cur = &cur[1..];
            let mut lo = xtoi(byte);
            if lo < 0 {
                warn!(
                    "Invalid char for hex string <{:x}> treated as <0>.",
                    byte as i32,
                );
                lo = 0;
            }
            lo
        } else {
            0
        };
        wbuf.push((hi << 4 | lo) as u8);
    }
    if cur[0] != b'>' {
        return None;
    }
    cur = &cur[1..];
    *inbuf = cur;
    Some(String::from_utf8(wbuf).unwrap())
}

pub(crate) fn pst_token_end(s: u8) -> bool {
    is_delim(&s) || is_space(&s)
}

unsafe fn pst_parse_any(inbuf: &mut &[u8]) -> PstObj {
    let mut cur = *inbuf;
    while !cur.is_empty() && !pst_token_end(cur[0]) {
        cur = &cur[1..];
    }
    let len = inbuf.len() - cur.len();
    let data = Vec::from(&inbuf[..len]);
    *inbuf = cur;
    PstObj::Unknown(data.into_boxed_slice())
}
fn skip_line(inbuf: &mut &[u8]) {
    while !inbuf.is_empty() && inbuf[0] != b'\n' && inbuf[0] != b'\r' {
        *inbuf = &inbuf[1..];
    }
    if !inbuf.is_empty() && inbuf[0] == b'\r' {
        *inbuf = &inbuf[1..];
    }
    if !inbuf.is_empty() && inbuf[0] == b'\n' {
        *inbuf = &inbuf[1..];
    };
}
unsafe fn skip_comments(inbuf: &mut &[u8]) {
    while !inbuf.is_empty() && inbuf[0] == b'%' {
        skip_line(inbuf);
        skip_white_spaces(inbuf);
    }
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */

pub(crate) unsafe fn pst_get_token(inbuf: &mut *const u8, inbufend: *const u8) -> Option<PstObj> {
    assert!(*inbuf <= inbufend && *inbufend == 0);
    let mut slice = std::slice::from_raw_parts(*inbuf, inbufend.offset_from(*inbuf) as _);
    let res = pstgettoken(&mut slice);
    *inbuf = slice.as_ptr();
    res
}

unsafe fn pstgettoken(inbuf: &mut &[u8]) -> Option<PstObj> {
    let mut obj = None;
    skip_white_spaces(inbuf);
    skip_comments(inbuf);
    if inbuf.is_empty() {
        return None;
    }
    let mut c = inbuf[0];
    match c {
        b'/' => obj = pst_parse_name(inbuf),
        b'[' | b'{' => {
            /* This is wrong */
            obj = Some(PstObj::Mark);
            *inbuf = &inbuf[1..];
        }
        b'<' => {
            if inbuf.len() <= 1 {
                return None;
            }
            c = inbuf[1];
            if c == b'<' {
                obj = Some(PstObj::Mark);
                *inbuf = &inbuf[2..];
            } else if c.is_ascii_hexdigit() {
                obj = pst_parse_string(inbuf)
            } else if c == b'~' {
                /* ASCII85 */
                obj = pst_parse_string(inbuf)
            }
        }
        b'(' => obj = pst_parse_string(inbuf),
        b'>' => {
            if inbuf.len() <= 1 || inbuf[1] != b'>' {
                panic!("Unexpected end of ASCII hex string marker.");
            } else {
                let mark = Vec::from(&b">>"[..]);
                obj = Some(PstObj::Unknown(mark.into_boxed_slice()));
                *inbuf = &inbuf[2..];
            }
        }
        b']' | b'}' => {
            let mark = vec![c];
            obj = Some(PstObj::Unknown(mark.into_boxed_slice()));
            *inbuf = &inbuf[1..];
        }
        _ => {
            if c == b't' || c == b'f' {
                obj = pst_parse_boolean(inbuf)
            } else if c == b'n' {
                obj = pst_parse_null(inbuf)
            } else if c == b'+' || c == b'-' || c.is_ascii_digit() || c == b'.' {
                obj = pst_parse_number(inbuf)
            }
        }
    }
    obj.or_else(|| Some(pst_parse_any(inbuf)))
}
