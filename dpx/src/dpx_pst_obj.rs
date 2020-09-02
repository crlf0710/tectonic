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

use std::ffi::CString;

use super::dpx_dpxutil::skip_white_spaces;
use crate::bridge::stub_errno as errno;
use crate::dpx_pst::pst_token_end;
use crate::warn;

use super::dpx_dpxutil::xtoi;
use super::dpx_mem::new;
use crate::shims::sprintf;
use libc::{free, strcpy, strlen, strtod, strtol};
use std::ptr;

use super::dpx_pst::PstType;

#[derive(Clone)]
#[repr(C)]
pub(crate) struct pst_obj {
    pub(crate) type_0: PstType,
    pub(crate) data: *mut libc::c_void,
}
#[derive(Clone)]
pub(crate) struct pst_string(pub(crate) String);

#[derive(Clone)]
pub(crate) struct pst_name(pub(crate) CString);

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_real(pub(crate) f64);

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_integer(pub(crate) i32);

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_boolean(pub(crate) i8);

const pst_const_null: *const i8 = b"null\x00" as *const u8 as *const i8;
const pst_const_mark: *const i8 = b"mark\x00" as *const u8 as *const i8;

impl pst_obj {
    pub(crate) fn new(type_0: PstType, data: *mut libc::c_void) -> Self {
        Self { type_0, data }
    }
    pub(crate) fn typ(&self) -> PstType {
        self.type_0
    }
}
impl Drop for pst_obj {
    fn drop(&mut self) {
        match self.type_0 {
            PstType::Boolean => unsafe {
                let _ = Box::from_raw(self.data as *mut pst_boolean);
            },
            PstType::Integer => unsafe {
                let _ = Box::from_raw(self.data as *mut pst_integer);
            },
            PstType::Real => unsafe {
                let _ = Box::from_raw(self.data as *mut pst_real);
            },
            PstType::Name => unsafe {
                let _ = Box::from_raw(self.data as *mut pst_name);
            },
            PstType::String => unsafe {
                let _ = Box::from_raw(self.data as *mut pst_string);
            },
            PstType::Null | PstType::Mark => unsafe {
                free(self.data);
            },
            PstType::Unknown => unsafe {
                let _ = Box::from_raw(self.data as *mut [u8]);
            },
        }
    }
}

pub(crate) unsafe fn pst_new_mark() -> pst_obj {
    let q = new(
        (strlen(pst_const_mark).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _,
    ) as *mut i8;
    strcpy(q, pst_const_mark);
    pst_obj::new(PstType::Mark, q as *mut libc::c_void)
}

impl pst_obj {
    pub(crate) unsafe fn as_boolean(&self) -> &pst_boolean {
        assert_eq!(self.type_0, PstType::Boolean);
        &*(self.data as *const pst_boolean)
    }
    pub(crate) unsafe fn as_integer(&self) -> &pst_integer {
        assert_eq!(self.type_0, PstType::Integer);
        &*(self.data as *const pst_integer)
    }
    pub(crate) unsafe fn as_real(&self) -> &pst_real {
        assert_eq!(self.type_0, PstType::Real);
        &*(self.data as *const pst_real)
    }
    pub(crate) unsafe fn as_name(&self) -> &pst_name {
        assert_eq!(self.type_0, PstType::Name);
        &*(self.data as *const pst_name)
    }
    pub(crate) unsafe fn as_string(&self) -> &pst_string {
        assert_eq!(self.type_0, PstType::String);
        &*(self.data as *const pst_string)
    }
    pub(crate) unsafe fn as_unknown(&self) -> &[u8] {
        assert_eq!(self.type_0, PstType::Unknown);
        &*(self.data as *const [u8])
    }
    pub(crate) unsafe fn length(&self) -> i32 {
        match self.type_0 {
            PstType::Boolean | PstType::Integer | PstType::Real | PstType::Null | PstType::Mark => {
                panic!("Operation not defined for this type of object.")
            }
            PstType::Name => self.as_name().0.to_bytes().len() as i32,
            PstType::String => self.as_string().0.len() as _,
            PstType::Unknown => self.as_unknown().len() as i32,
        }
    }
    pub(crate) unsafe fn getIV(&self) -> i32 {
        match self.type_0 {
            PstType::Boolean => self.as_boolean().0 as i32,
            PstType::Integer => self.as_integer().0,
            PstType::Real => self.as_real().0 as i32,
            PstType::String => pst_string_RV(self.as_string()) as i32,
            PstType::Name | PstType::Null | PstType::Mark => {
                panic!("Operation not defined for this type of object.");
            }
            PstType::Unknown => {
                panic!("Cannot convert object of type UNKNOWN to integer value.");
            }
        }
    }
    pub(crate) unsafe fn getRV(&self) -> f64 {
        match self.type_0 {
            PstType::Boolean => self.as_boolean().0 as f64,
            PstType::Integer => self.as_integer().0 as f64,
            PstType::Real => self.as_real().0,
            PstType::String => pst_string_RV(self.as_string()),
            PstType::Name | PstType::Null | PstType::Mark => {
                panic!("Operation not defined for this type of object.");
            }
            PstType::Unknown => {
                panic!("Cannot convert object of type UNKNOWN to real value.");
            }
        }
    }
    /* Length can be obtained by pst_length_of(). */

    pub(crate) unsafe fn getSV(&self) -> Option<String> {
        let sv;
        match self.type_0 {
            PstType::Boolean => sv = pst_boolean_SV(self.as_boolean()),
            PstType::Integer => sv = pst_integer_SV(self.as_integer()),
            PstType::Real => sv = pst_real_SV(self.as_real()),
            PstType::Name => sv = pst_name_SV(self.as_name()),
            PstType::String => sv = pst_string_SV(self.as_string()),
            PstType::Null | PstType::Mark => {
                panic!("Operation not defined for this type of object.");
            }
            PstType::Unknown => {
                let data = self.as_unknown();
                if !data.is_empty() {
                    sv = Some(String::from_utf8_lossy(data).into());
                } else {
                    sv = None
                }
            }
        }
        sv
    }

    pub(crate) unsafe fn data(&self) -> &[u8] {
        match self.type_0 {
            PstType::Boolean => std::slice::from_raw_parts(&self.as_boolean().0 as *const i8 as *const u8, 1),
            PstType::Integer => std::slice::from_raw_parts(&self.as_integer().0 as *const i32 as *const u8, 4),
            PstType::Real => std::slice::from_raw_parts(&self.as_real().0 as *const f64 as *const u8, 8),
            PstType::Name => self.as_name().0.as_bytes(),
            PstType::String => self.as_string().0.as_bytes(),
            PstType::Null | PstType::Mark => {
                panic!("Operation not defined for this type of object.");
            }
            PstType::Unknown => {
                self.as_unknown()
            }
        }
    }
}

/* BOOLEAN */
/* BOOLEAN */
unsafe fn pst_boolean_new(value: i8) -> *mut pst_boolean {
    Box::into_raw(Box::new(pst_boolean(value)))
}
unsafe fn pst_boolean_SV(obj: &pst_boolean) -> Option<String> {
    Some(String::from(if (*obj).0 != 0 { "true" } else { "false" }))
}

pub(crate) unsafe fn pst_parse_boolean(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj> {
    let slice = std::slice::from_raw_parts(*inbuf, inbufend.offset_from(*inbuf) as usize);
    if slice.len() >= 4
        && slice.starts_with(b"true")
        && (slice.len() == 4 || pst_token_end(slice[4]))
    {
        *inbuf = (*inbuf).offset(4);
        Some(pst_obj::new(
            PstType::Boolean,
            pst_boolean_new(1_i8) as *mut libc::c_void,
        ))
    } else if slice.len() >= 5
        && slice.starts_with(b"false")
        && (slice.len() == 5 || pst_token_end(slice[5]))
    {
        *inbuf = (*inbuf).offset(5);
        Some(pst_obj::new(
            PstType::Boolean,
            pst_boolean_new(0_i8) as *mut libc::c_void,
        ))
    } else {
        None
    }
}
/* NULL */

pub(crate) unsafe fn pst_parse_null(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj> {
    let slice = std::slice::from_raw_parts(*inbuf, inbufend.offset_from(*inbuf) as usize);
    if slice.len() >= 4
        && slice.starts_with(b"null")
        && (slice.len() == 4 || pst_token_end(slice[4]))
    {
        *inbuf = (*inbuf).offset(4);
        let q = new(
            (strlen(pst_const_null).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
        ) as *mut i8;
        strcpy(q, pst_const_null);
        Some(pst_obj::new(PstType::Null, q as *mut libc::c_void))
    } else {
        None
    }
}
/* NUMBERS */
/* INTEGER */
unsafe fn pst_integer_new(value: i32) -> *mut pst_integer {
    Box::into_raw(Box::new(pst_integer(value)))
}
unsafe fn pst_integer_SV(obj: &pst_integer) -> Option<String> {
    Some(format!("{}", obj.0))
}
/* REAL */
unsafe fn pst_real_new(value: f64) -> *mut pst_real {
    Box::into_raw(Box::new(pst_real(value)))
}
unsafe fn pst_real_SV(obj: &pst_real) -> Option<String> {
    let mut fmt_buf: [u8; 15] = [0; 15];
    let len = sprintf(
        fmt_buf.as_mut_ptr() as *mut i8,
        b"%.5g\x00" as *const u8 as *const i8,
        obj.0,
    ) as usize;
    Some(String::from_utf8_lossy(&fmt_buf[..len]).into())
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */
/* leading white-space is ignored */

pub(crate) unsafe fn pst_parse_number(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj> {
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
            return Some(pst_obj::new(
                PstType::Real,
                pst_real_new(dval) as *mut libc::c_void,
            ));
        }
    } else if cur != *inbuf && (cur == inbufend || pst_token_end(*cur)) {
        /* integer */
        *inbuf = cur;
        return Some(pst_obj::new(
            PstType::Integer,
            pst_integer_new(lval) as *mut libc::c_void,
        ));
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
                return Some(pst_obj::new(
                    PstType::Integer,
                    pst_integer_new(lval) as *mut libc::c_void,
                ));
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
unsafe fn pst_name_new(name: &[u8]) -> *mut pst_name {
    Box::into_raw(Box::new(pst_name(CString::new(name).unwrap())))
}
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

pub(crate) unsafe fn pst_parse_name(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj>
/* / is required */ {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    if *cur as i32 != '/' as i32 {
        return None;
    }
    cur = cur.offset(1);
    while !(cur == inbufend || pst_token_end(*cur)) {
        let fresh0 = cur;
        cur = cur.offset(1);
        let mut c = *fresh0;
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
    Some(pst_obj::new(
        PstType::Name,
        pst_name_new(&wbuf) as *mut libc::c_void,
    ))
}
unsafe fn pst_name_SV(obj: &pst_name) -> Option<String> {
    Some(obj.0.clone().into_string().unwrap())
}
/* STRING */
/*
 * TODO: ascii85 string <~ .... ~>
 */
unsafe fn pst_string_new(value: String) -> *mut pst_string {
    Box::into_raw(Box::new(pst_string(value)))
}

pub(crate) unsafe fn pst_parse_string(inbuf: *mut *mut u8, inbufend: *mut u8) -> Option<pst_obj> {
    if (*inbuf).offset(2) >= inbufend {
        return None;
    } else {
        if **inbuf as i32 == '(' as i32 {
            return Some(pst_obj::new(
                PstType::String,
                pst_string_parse_literal(inbuf, inbufend) as *mut libc::c_void,
            ));
        } else {
            if **inbuf as i32 == '<' as i32 && *(*inbuf).offset(1) as i32 == '~' as i32 {
                panic!("ASCII85 string not supported yet.");
            } else {
                if **inbuf as i32 == '<' as i32 {
                    return Some(pst_obj::new(
                        PstType::String,
                        pst_string_parse_hex(inbuf, inbufend) as *mut libc::c_void,
                    ));
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
unsafe fn pst_string_parse_literal(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_string {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    let mut c: u8 = 0_u8;
    let mut balance: i32 = 1i32;
    if cur.offset(2) > inbufend || *cur as i32 != '(' as i32 {
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    while cur < inbufend && wbuf.len() < 4096 && balance > 0 {
        let fresh2 = cur;
        cur = cur.offset(1);
        c = *fresh2;
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
        return ptr::null_mut();
    }
    *inbuf = cur;
    pst_string_new(String::from_utf8(wbuf).unwrap())
}
unsafe fn pst_string_parse_hex(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_string {
    let mut wbuf: Vec<u8> = Vec::new();
    let mut cur: *mut u8 = *inbuf;
    if cur.offset(2) > inbufend || *cur != b'<' || *cur == b'<' && *cur.offset(1) == b'<' {
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    /* PDF Reference does not specify how to treat invalid char */
    while cur < inbufend && wbuf.len() < 4096 {
        skip_white_spaces(&mut cur, inbufend);
        if *cur == b'>' {
            break;
        }
        let fresh8 = cur;
        cur = cur.offset(1);
        let mut hi = xtoi(*fresh8);
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
            let fresh9 = cur;
            cur = cur.offset(1);
            xtoi(*fresh9)
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
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    *inbuf = cur;
    pst_string_new(String::from_utf8(wbuf).unwrap())
}
unsafe fn pst_string_RV(obj: &pst_string) -> f64 {
    let mut p = obj.0.as_ptr() as *mut u8;
    let end = p.offset(obj.0.len() as isize);
    let nobj = pst_parse_number(&mut p, end);
    if nobj.is_none() || p != end {
        panic!("Cound not convert string to real value.");
    }
    let nobj = nobj.unwrap();
    let rv = nobj.getRV();
    rv
}
unsafe fn pst_string_SV(obj: &pst_string) -> Option<String> {
    Some(obj.0.clone())
}
