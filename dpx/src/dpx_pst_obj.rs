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

use super::dpx_dpxutil::skip_white_spaces;
use crate::bridge::stub_errno as errno;
use crate::warn;

use super::dpx_dpxutil::xtoi;
use super::dpx_mem::new;
use crate::shims::sprintf;
use libc::{free, memcmp, memcpy, strcpy, strlen, strtod, strtol};
use std::ptr;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_obj {
    pub(crate) type_0: pst_type,
    pub(crate) data: *mut libc::c_void,
}
pub(crate) type pst_type = i32;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_string {
    pub(crate) length: u32,
    pub(crate) value: *mut u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_name {
    pub(crate) value: *mut i8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_real {
    pub(crate) value: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_integer {
    pub(crate) value: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pst_boolean {
    pub(crate) value: i8,
}
static mut pst_const_null: *const i8 = b"null\x00" as *const u8 as *const i8;
static mut pst_const_mark: *const i8 = b"mark\x00" as *const u8 as *const i8;

pub(crate) unsafe fn pst_new_obj(type_0: pst_type, data: *mut libc::c_void) -> *mut pst_obj {
    let obj =
        new((1_u64).wrapping_mul(::std::mem::size_of::<pst_obj>() as u64) as u32) as *mut pst_obj;
    (*obj).type_0 = type_0;
    (*obj).data = data;
    obj
}

pub(crate) unsafe fn pst_new_mark() -> *mut pst_obj {
    let q = new(
        (strlen(pst_const_mark).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _,
    ) as *mut i8;
    strcpy(q, pst_const_mark);
    pst_new_obj(7i32, q as *mut libc::c_void)
}

pub(crate) unsafe fn pst_release_obj(obj: *mut pst_obj) {
    assert!(!obj.is_null());
    match (*obj).type_0 {
        1 => {
            pst_boolean_release((*obj).data as *mut pst_boolean);
        }
        2 => {
            pst_integer_release((*obj).data as *mut pst_integer);
        }
        3 => {
            pst_real_release((*obj).data as *mut pst_real);
        }
        6 => {
            pst_name_release((*obj).data as *mut pst_name);
        }
        5 => {
            pst_string_release((*obj).data as *mut pst_string);
        }
        0 | 7 | -1 => {
            free((*obj).data);
        }
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }
    free(obj as *mut libc::c_void);
}

pub(crate) unsafe fn pst_type_of(obj: *mut pst_obj) -> pst_type {
    assert!(!obj.is_null());
    (*obj).type_0
}

pub(crate) unsafe fn pst_length_of(obj: *mut pst_obj) -> i32 {
    assert!(!obj.is_null());
    match (*obj).type_0 {
        1 => pst_boolean_length() as i32,
        2 => pst_integer_length() as i32,
        3 => pst_real_length() as i32,
        6 => pst_name_length((*obj).data as *mut pst_name) as i32,
        5 => pst_string_length((*obj).data as *mut pst_string) as i32,
        0 | 7 => {
            panic!("Operation not defined for this type of object.");
        }
        -1 => strlen((*obj).data as *const i8) as i32,
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }
}

pub(crate) unsafe fn pst_getIV(obj: *mut pst_obj) -> i32 {
    assert!(!obj.is_null());
    match (*obj).type_0 {
        1 => pst_boolean_IV((*obj).data as *mut pst_boolean),
        2 => pst_integer_IV((*obj).data as *mut pst_integer),
        3 => pst_real_IV((*obj).data as *mut pst_real),
        6 => pst_name_IV(),
        5 => pst_string_IV((*obj).data as *mut pst_string),
        0 | 7 => {
            panic!("Operation not defined for this type of object.");
        }
        -1 => {
            panic!("Cannot convert object of type UNKNOWN to integer value.");
        }
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }
}

pub(crate) unsafe fn pst_getRV(obj: *mut pst_obj) -> f64 {
    assert!(!obj.is_null());
    match (*obj).type_0 {
        1 => pst_boolean_RV((*obj).data as *mut pst_boolean),
        2 => pst_integer_RV((*obj).data as *mut pst_integer),
        3 => pst_real_RV((*obj).data as *mut pst_real),
        6 => pst_name_RV(),
        5 => pst_string_RV((*obj).data as *mut pst_string),
        0 | 7 => {
            panic!("Operation not defined for this type of object.");
        }
        -1 => {
            panic!("Cannot convert object of type UNKNOWN to real value.");
        }
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }
}
/* Length can be obtained by pst_length_of(). */

pub(crate) unsafe fn pst_getSV(obj: *mut pst_obj) -> *mut u8 {
    let sv;
    assert!(!obj.is_null());
    match (*obj).type_0 {
        1 => sv = pst_boolean_SV((*obj).data as *mut pst_boolean),
        2 => sv = pst_integer_SV((*obj).data as *mut pst_integer),
        3 => sv = pst_real_SV((*obj).data as *mut pst_real),
        6 => sv = pst_name_SV((*obj).data as *mut pst_name),
        5 => sv = pst_string_SV((*obj).data as *mut pst_string),
        0 | 7 => {
            panic!("Operation not defined for this type of object.");
        }
        -1 => {
            let len = strlen((*obj).data as *mut i8) as i32;
            if len > 0i32 {
                sv = new(((len + 1i32) as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                    as u32) as *mut u8;
                memcpy(sv as *mut libc::c_void, (*obj).data, len as _);
                *sv.offset(len as isize) = '\u{0}' as i32 as u8
            } else {
                sv = ptr::null_mut()
            }
        }
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }
    sv
}

pub(crate) unsafe fn pst_data_ptr(obj: *mut pst_obj) -> *mut libc::c_void {
    assert!(!obj.is_null());
    (match (*obj).type_0 {
        1 => pst_boolean_data_ptr((*obj).data as *mut pst_boolean) as *mut i8,
        2 => pst_integer_data_ptr((*obj).data as *mut pst_integer) as *mut i8,
        3 => pst_real_data_ptr((*obj).data as *mut pst_real) as *mut i8,
        6 => pst_name_data_ptr((*obj).data as *mut pst_name) as *mut i8,
        5 => pst_string_data_ptr((*obj).data as *mut pst_string) as *mut i8,
        0 | 7 => {
            panic!("Operation not defined for this type of object.");
        }
        -1 => (*obj).data as *mut i8,
        _ => {
            panic!("Unrecognized object type: {}", (*obj).type_0);
        }
    }) as *mut libc::c_void
}
/* BOOLEAN */
/* BOOLEAN */
unsafe fn pst_boolean_new(value: i8) -> *mut pst_boolean {
    let obj = new((1_u64).wrapping_mul(::std::mem::size_of::<pst_boolean>() as u64) as u32)
        as *mut pst_boolean;
    (*obj).value = value;
    obj
}
unsafe fn pst_boolean_release(obj: *mut pst_boolean) {
    assert!(!obj.is_null());
    free(obj as *mut libc::c_void);
}
unsafe fn pst_boolean_IV(obj: *mut pst_boolean) -> i32 {
    assert!(!obj.is_null());
    (*obj).value as i32
}
unsafe fn pst_boolean_RV(obj: *mut pst_boolean) -> f64 {
    assert!(!obj.is_null());
    (*obj).value as f64
}
unsafe fn pst_boolean_SV(obj: *mut pst_boolean) -> *mut u8 {
    let str;
    assert!(!obj.is_null());
    if (*obj).value != 0 {
        str = new((5_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        memcpy(
            str as *mut libc::c_void,
            b"true\x00" as *const u8 as *const i8 as *const libc::c_void,
            4,
        );
        *str.offset(4) = '\u{0}' as i32 as u8
    } else {
        str = new((6_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        memcpy(
            str as *mut libc::c_void,
            b"false\x00" as *const u8 as *const i8 as *const libc::c_void,
            5,
        );
        *str.offset(5) = '\u{0}' as i32 as u8
    }
    str
}
unsafe fn pst_boolean_length() -> u32 {
    panic!("Operation not defined for this type of object.");
}
unsafe fn pst_boolean_data_ptr(obj: *mut pst_boolean) -> *mut libc::c_void {
    assert!(!obj.is_null());
    &mut (*obj).value as *mut i8 as *mut libc::c_void
}

pub(crate) unsafe fn pst_parse_boolean(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_obj {
    if (*inbuf).offset(4) <= inbufend
        && memcmp(
            *inbuf as *const libc::c_void,
            b"true\x00" as *const u8 as *const i8 as *const libc::c_void,
            4,
        ) == 0i32
        && ((*inbuf).offset(4) == inbufend
            || (*(*inbuf).offset(4) as i32 == '(' as i32
                || *(*inbuf).offset(4) as i32 == ')' as i32
                || *(*inbuf).offset(4) as i32 == '/' as i32
                || *(*inbuf).offset(4) as i32 == '<' as i32
                || *(*inbuf).offset(4) as i32 == '>' as i32
                || *(*inbuf).offset(4) as i32 == '[' as i32
                || *(*inbuf).offset(4) as i32 == ']' as i32
                || *(*inbuf).offset(4) as i32 == '{' as i32
                || *(*inbuf).offset(4) as i32 == '}' as i32
                || *(*inbuf).offset(4) as i32 == '%' as i32)
            || (*(*inbuf).offset(4) as i32 == ' ' as i32
                || *(*inbuf).offset(4) as i32 == '\t' as i32
                || *(*inbuf).offset(4) as i32 == '\u{c}' as i32
                || *(*inbuf).offset(4) as i32 == '\r' as i32
                || *(*inbuf).offset(4) as i32 == '\n' as i32
                || *(*inbuf).offset(4) as i32 == '\u{0}' as i32))
    {
        *inbuf = (*inbuf).offset(4);
        return pst_new_obj(1i32, pst_boolean_new(1_i8) as *mut libc::c_void);
    } else if (*inbuf).offset(5) <= inbufend
        && memcmp(
            *inbuf as *const libc::c_void,
            b"false\x00" as *const u8 as *const i8 as *const libc::c_void,
            5,
        ) == 0i32
        && ((*inbuf).offset(5) == inbufend
            || (*(*inbuf).offset(5) as i32 == '(' as i32
                || *(*inbuf).offset(5) as i32 == ')' as i32
                || *(*inbuf).offset(5) as i32 == '/' as i32
                || *(*inbuf).offset(5) as i32 == '<' as i32
                || *(*inbuf).offset(5) as i32 == '>' as i32
                || *(*inbuf).offset(5) as i32 == '[' as i32
                || *(*inbuf).offset(5) as i32 == ']' as i32
                || *(*inbuf).offset(5) as i32 == '{' as i32
                || *(*inbuf).offset(5) as i32 == '}' as i32
                || *(*inbuf).offset(5) as i32 == '%' as i32)
            || (*(*inbuf).offset(5) as i32 == ' ' as i32
                || *(*inbuf).offset(5) as i32 == '\t' as i32
                || *(*inbuf).offset(5) as i32 == '\u{c}' as i32
                || *(*inbuf).offset(5) as i32 == '\r' as i32
                || *(*inbuf).offset(5) as i32 == '\n' as i32
                || *(*inbuf).offset(5) as i32 == '\u{0}' as i32))
    {
        *inbuf = (*inbuf).offset(5);
        return pst_new_obj(1i32, pst_boolean_new(0_i8) as *mut libc::c_void);
    } else {
        return ptr::null_mut();
    };
}
/* NULL */

pub(crate) unsafe fn pst_parse_null(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_obj {
    if (*inbuf).offset(4) <= inbufend
        && memcmp(
            *inbuf as *const libc::c_void,
            b"null\x00" as *const u8 as *const i8 as *const libc::c_void,
            4,
        ) == 0i32
        && ((*inbuf).offset(4) == inbufend
            || (*(*inbuf).offset(4) as i32 == '(' as i32
                || *(*inbuf).offset(4) as i32 == ')' as i32
                || *(*inbuf).offset(4) as i32 == '/' as i32
                || *(*inbuf).offset(4) as i32 == '<' as i32
                || *(*inbuf).offset(4) as i32 == '>' as i32
                || *(*inbuf).offset(4) as i32 == '[' as i32
                || *(*inbuf).offset(4) as i32 == ']' as i32
                || *(*inbuf).offset(4) as i32 == '{' as i32
                || *(*inbuf).offset(4) as i32 == '}' as i32
                || *(*inbuf).offset(4) as i32 == '%' as i32)
            || (*(*inbuf).offset(4) as i32 == ' ' as i32
                || *(*inbuf).offset(4) as i32 == '\t' as i32
                || *(*inbuf).offset(4) as i32 == '\u{c}' as i32
                || *(*inbuf).offset(4) as i32 == '\r' as i32
                || *(*inbuf).offset(4) as i32 == '\n' as i32
                || *(*inbuf).offset(4) as i32 == '\u{0}' as i32))
    {
        *inbuf = (*inbuf).offset(4);
        let q = new(
            (strlen(pst_const_null).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
        ) as *mut i8;
        strcpy(q, pst_const_null);
        return pst_new_obj(0i32, q as *mut libc::c_void);
    } else {
        return ptr::null_mut();
    };
}
/* NUMBERS */
/* INTEGER */
unsafe fn pst_integer_new(value: i32) -> *mut pst_integer {
    let obj = new((1_u64).wrapping_mul(::std::mem::size_of::<pst_integer>() as u64) as u32)
        as *mut pst_integer;
    (*obj).value = value;
    obj
}
unsafe fn pst_integer_release(obj: *mut pst_integer) {
    assert!(!obj.is_null());
    free(obj as *mut libc::c_void);
}
unsafe fn pst_integer_IV(obj: *mut pst_integer) -> i32 {
    assert!(!obj.is_null());
    (*obj).value
}
unsafe fn pst_integer_RV(obj: *mut pst_integer) -> f64 {
    assert!(!obj.is_null());
    (*obj).value as f64
}
unsafe fn pst_integer_SV(obj: *mut pst_integer) -> *mut u8 {
    let mut fmt_buf: [i8; 15] = [0; 15];
    assert!(!obj.is_null());
    let len = sprintf(
        fmt_buf.as_mut_ptr(),
        b"%d\x00" as *const u8 as *const i8,
        (*obj).value,
    );
    let value =
        new(((len + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    strcpy(value, fmt_buf.as_mut_ptr());
    value as *mut u8
}
unsafe fn pst_integer_data_ptr(obj: *mut pst_integer) -> *mut libc::c_void {
    assert!(!obj.is_null());
    &mut (*obj).value as *mut i32 as *mut libc::c_void
}
unsafe fn pst_integer_length() -> u32 {
    panic!("Operation not defined for this type of object.");
}
/* REAL */
unsafe fn pst_real_new(value: f64) -> *mut pst_real {
    let obj =
        new((1_u64).wrapping_mul(::std::mem::size_of::<pst_real>() as u64) as u32) as *mut pst_real;
    (*obj).value = value;
    obj
}
unsafe fn pst_real_release(obj: *mut pst_real) {
    assert!(!obj.is_null());
    free(obj as *mut libc::c_void);
}
unsafe fn pst_real_IV(obj: *mut pst_real) -> i32 {
    assert!(!obj.is_null());
    (*obj).value as i32
}
unsafe fn pst_real_RV(obj: *mut pst_real) -> f64 {
    assert!(!obj.is_null());
    (*obj).value
}
unsafe fn pst_real_SV(obj: *mut pst_real) -> *mut u8 {
    let mut fmt_buf: [i8; 15] = [0; 15];
    assert!(!obj.is_null());
    let len = sprintf(
        fmt_buf.as_mut_ptr(),
        b"%.5g\x00" as *const u8 as *const i8,
        (*obj).value,
    );
    let value =
        new((len as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    strcpy(value, fmt_buf.as_mut_ptr());
    value as *mut u8
}
unsafe fn pst_real_data_ptr(obj: *mut pst_real) -> *mut libc::c_void {
    assert!(!obj.is_null());
    &mut (*obj).value as *mut f64 as *mut libc::c_void
}
unsafe fn pst_real_length() -> u32 {
    panic!("Operation not defined for this type of object.");
}
/* NOTE: the input buffer must be null-terminated, i.e., *inbufend == 0 */
/* leading white-space is ignored */

pub(crate) unsafe fn pst_parse_number(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_obj {
    let mut cur: *mut u8 = ptr::null_mut();
    errno::set_errno(errno::ZERO);
    let mut lval = strtol(
        *inbuf as *mut i8,
        &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        10i32,
    ) as i32;
    if errno::errno() != errno::ZERO
        || *cur as i32 == '.' as i32
        || *cur as i32 == 'e' as i32
        || *cur as i32 == 'E' as i32
    {
        /* real */
        errno::set_errno(errno::ZERO);
        let dval = strtod(
            *inbuf as *mut i8,
            &mut cur as *mut *mut u8 as *mut libc::c_void as *mut *mut i8,
        );
        if errno::errno() == errno::ZERO
            && (cur == inbufend
                || (*cur as i32 == '(' as i32
                    || *cur as i32 == ')' as i32
                    || *cur as i32 == '/' as i32
                    || *cur as i32 == '<' as i32
                    || *cur as i32 == '>' as i32
                    || *cur as i32 == '[' as i32
                    || *cur as i32 == ']' as i32
                    || *cur as i32 == '{' as i32
                    || *cur as i32 == '}' as i32
                    || *cur as i32 == '%' as i32)
                || (*cur as i32 == ' ' as i32
                    || *cur as i32 == '\t' as i32
                    || *cur as i32 == '\u{c}' as i32
                    || *cur as i32 == '\r' as i32
                    || *cur as i32 == '\n' as i32
                    || *cur as i32 == '\u{0}' as i32))
        {
            *inbuf = cur;
            return pst_new_obj(3i32, pst_real_new(dval) as *mut libc::c_void);
        }
    } else if cur != *inbuf
        && (cur == inbufend
            || (*cur as i32 == '(' as i32
                || *cur as i32 == ')' as i32
                || *cur as i32 == '/' as i32
                || *cur as i32 == '<' as i32
                || *cur as i32 == '>' as i32
                || *cur as i32 == '[' as i32
                || *cur as i32 == ']' as i32
                || *cur as i32 == '{' as i32
                || *cur as i32 == '}' as i32
                || *cur as i32 == '%' as i32)
            || (*cur as i32 == ' ' as i32
                || *cur as i32 == '\t' as i32
                || *cur as i32 == '\u{c}' as i32
                || *cur as i32 == '\r' as i32
                || *cur as i32 == '\n' as i32
                || *cur as i32 == '\u{0}' as i32))
    {
        /* integer */
        *inbuf = cur;
        return pst_new_obj(2i32, pst_integer_new(lval) as *mut libc::c_void);
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
            if errno::errno() == errno::ZERO
                && (cur == inbufend
                    || (*cur as i32 == '(' as i32
                        || *cur as i32 == ')' as i32
                        || *cur as i32 == '/' as i32
                        || *cur as i32 == '<' as i32
                        || *cur as i32 == '>' as i32
                        || *cur as i32 == '[' as i32
                        || *cur as i32 == ']' as i32
                        || *cur as i32 == '{' as i32
                        || *cur as i32 == '}' as i32
                        || *cur as i32 == '%' as i32)
                    || (*cur as i32 == ' ' as i32
                        || *cur as i32 == '\t' as i32
                        || *cur as i32 == '\u{c}' as i32
                        || *cur as i32 == '\r' as i32
                        || *cur as i32 == '\n' as i32
                        || *cur as i32 == '\u{0}' as i32))
            {
                *inbuf = cur;
                return pst_new_obj(2i32, pst_integer_new(lval) as *mut libc::c_void);
            }
        }
    }
    /* error */
    ptr::null_mut()
}
/* NAME */
/* NAME */
/*
 * \0 is not allowed for name object.
 */
unsafe fn pst_name_new(name: *const i8) -> *mut pst_name {
    let obj =
        new((1_u64).wrapping_mul(::std::mem::size_of::<pst_name>() as u64) as u32) as *mut pst_name;
    (*obj).value =
        new((strlen(name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*obj).value, name);
    obj
}
unsafe fn pst_name_release(obj: *mut pst_name) {
    assert!(!obj.is_null());
    free((*obj).value as *mut libc::c_void);
    free(obj as *mut libc::c_void);
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

pub(crate) unsafe fn pst_parse_name(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_obj
/* / is required */ {
    let mut wbuf: [u8; 128] = [0; 128];
    let mut p: *mut u8 = wbuf.as_mut_ptr();
    let mut cur: *mut u8 = *inbuf;
    let mut len: i32 = 0i32;
    if *cur as i32 != '/' as i32 {
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    while !(cur == inbufend
        || (*cur as i32 == '(' as i32
            || *cur as i32 == ')' as i32
            || *cur as i32 == '/' as i32
            || *cur as i32 == '<' as i32
            || *cur as i32 == '>' as i32
            || *cur as i32 == '[' as i32
            || *cur as i32 == ']' as i32
            || *cur as i32 == '{' as i32
            || *cur as i32 == '}' as i32
            || *cur as i32 == '%' as i32)
        || (*cur as i32 == ' ' as i32
            || *cur as i32 == '\t' as i32
            || *cur as i32 == '\u{c}' as i32
            || *cur as i32 == '\r' as i32
            || *cur as i32 == '\n' as i32
            || *cur as i32 == '\u{0}' as i32))
    {
        let fresh0 = cur;
        cur = cur.offset(1);
        let mut c = *fresh0;
        if c as i32 == '#' as i32 {
            if cur.offset(2) >= inbufend {
                warn!("Premature end of input name string.");
                break;
            } else {
                let val = getxpair(&mut cur);
                if val <= 0i32 {
                    warn!("Invalid char for name object. (ignored)");
                    continue;
                } else {
                    c = val as u8
                }
            }
        }
        if len < 127i32 {
            let fresh1 = p;
            p = p.offset(1);
            *fresh1 = c
        }
        len += 1
    }
    *p = '\u{0}' as i32 as u8;
    if len > 127i32 {
        warn!("String too long for name object. Output will be truncated.");
    }
    *inbuf = cur;
    return pst_new_obj(
        6i32,
        pst_name_new(wbuf.as_mut_ptr() as *mut i8) as *mut libc::c_void,
    );
}
unsafe fn pst_name_IV() -> i32 {
    panic!("Operation not defined for this type of object.");
}
unsafe fn pst_name_RV() -> f64 {
    panic!("Operation not defined for this type of object.");
}
unsafe fn pst_name_SV(obj: *mut pst_name) -> *mut u8 {
    let value =
        new((strlen((*obj).value).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy(value, (*obj).value);
    value as *mut u8
}
unsafe fn pst_name_data_ptr(obj: *mut pst_name) -> *mut libc::c_void {
    assert!(!obj.is_null());
    (*obj).value as *mut libc::c_void
}
unsafe fn pst_name_length(obj: *mut pst_name) -> u32 {
    assert!(!obj.is_null());
    strlen((*obj).value) as u32
}
/* STRING */
/*
 * TODO: ascii85 string <~ .... ~>
 */
unsafe fn pst_string_new(str: *mut u8, len: u32) -> *mut pst_string {
    let obj = new((1_u64).wrapping_mul(::std::mem::size_of::<pst_string>() as u64) as u32)
        as *mut pst_string;
    (*obj).length = len;
    (*obj).value = ptr::null_mut();
    if len > 0_u32 {
        (*obj).value =
            new((len as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        if !str.is_null() {
            memcpy(
                (*obj).value as *mut libc::c_void,
                str as *const libc::c_void,
                len as _,
            );
        }
    }
    obj
}
unsafe fn pst_string_release(obj: *mut pst_string) {
    assert!(!obj.is_null());
    free((*obj).value as *mut libc::c_void);
    free(obj as *mut libc::c_void);
}

pub(crate) unsafe fn pst_parse_string(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_obj {
    if (*inbuf).offset(2) >= inbufend {
        return ptr::null_mut();
    } else {
        if **inbuf as i32 == '(' as i32 {
            return pst_new_obj(
                5i32,
                pst_string_parse_literal(inbuf, inbufend) as *mut libc::c_void,
            );
        } else {
            if **inbuf as i32 == '<' as i32 && *(*inbuf).offset(1) as i32 == '~' as i32 {
                panic!("ASCII85 string not supported yet.");
            } else {
                if **inbuf as i32 == '<' as i32 {
                    return pst_new_obj(
                        5i32,
                        pst_string_parse_hex(inbuf, inbufend) as *mut libc::c_void,
                    );
                }
            }
        }
    }
    ptr::null_mut()
}
/* Overflowed value is set to invalid char.  */
unsafe fn ostrtouc(inbuf: *mut *mut u8, inbufend: *mut u8, valid: *mut u8) -> u8 {
    let mut cur: *mut u8 = *inbuf;
    let mut val: u32 = 0_u32;
    while cur < inbufend
        && cur < (*inbuf).offset(3)
        && (*cur as i32 >= '0' as i32 && *cur as i32 <= '7' as i32)
    {
        val = val << 3i32 | (*cur as i32 - '0' as i32) as u32;
        cur = cur.offset(1)
    }
    if val > 255_u32 || cur == *inbuf {
        *valid = 0_u8
    } else {
        *valid = 1_u8
    }
    *inbuf = cur;
    val as u8
}
unsafe fn esctouc(inbuf: *mut *mut u8, inbufend: *mut u8, valid: *mut u8) -> u8 {
    let unescaped;
    let escaped = **inbuf;
    *valid = 1_u8;
    match escaped as i32 {
        92 | 41 | 40 => {
            /* Backslash, unbalanced paranthes */
            unescaped = escaped;
            *inbuf = (*inbuf).offset(1)
        }
        110 => {
            /* Other escaped char */
            unescaped = '\n' as i32 as u8;
            *inbuf = (*inbuf).offset(1)
        }
        114 => {
            unescaped = '\r' as i32 as u8;
            *inbuf = (*inbuf).offset(1)
        }
        116 => {
            unescaped = '\t' as i32 as u8;
            *inbuf = (*inbuf).offset(1)
        }
        98 => {
            unescaped = '\u{8}' as i32 as u8;
            *inbuf = (*inbuf).offset(1)
        }
        102 => {
            unescaped = '\u{c}' as i32 as u8;
            *inbuf = (*inbuf).offset(1)
        }
        13 => {
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
        10 => {
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
    let mut wbuf: [u8; 4096] = [0; 4096];
    let mut cur: *mut u8 = *inbuf;
    let mut c: u8 = 0_u8;
    let mut len: i32 = 0i32;
    let mut balance: i32 = 1i32;
    if cur.offset(2) > inbufend || *cur as i32 != '(' as i32 {
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    while cur < inbufend && len < 4096i32 && balance > 0i32 {
        let fresh2 = cur;
        cur = cur.offset(1);
        c = *fresh2;
        match c as i32 {
            92 => {
                let mut valid: u8 = 0;
                let unescaped = esctouc(&mut cur, inbufend, &mut valid);
                if valid != 0 {
                    let fresh3 = len;
                    len = len + 1;
                    wbuf[fresh3 as usize] = unescaped
                }
            }
            40 => {
                balance += 1;
                let fresh4 = len;
                len = len + 1;
                wbuf[fresh4 as usize] = '(' as i32 as u8
            }
            41 => {
                balance -= 1;
                if balance > 0i32 {
                    let fresh5 = len;
                    len = len + 1;
                    wbuf[fresh5 as usize] = ')' as i32 as u8
                }
            }
            13 => {
                /*
                 * An end-of-line marker (\n, \r or \r\n), not preceeded by a backslash,
                 * must be converted to single \n.
                 */
                if cur < inbufend && *cur as i32 == '\n' as i32 {
                    cur = cur.offset(1)
                }
                let fresh6 = len;
                len = len + 1;
                wbuf[fresh6 as usize] = '\n' as i32 as u8
            }
            _ => {
                let fresh7 = len;
                len = len + 1;
                wbuf[fresh7 as usize] = c
            }
        }
    }
    if c as i32 != ')' as i32 {
        return ptr::null_mut();
    }
    *inbuf = cur;
    pst_string_new(wbuf.as_mut_ptr(), len as u32)
}
unsafe fn pst_string_parse_hex(inbuf: *mut *mut u8, inbufend: *mut u8) -> *mut pst_string {
    let mut wbuf: [u8; 4096] = [0; 4096];
    let mut cur: *mut u8 = *inbuf;
    let mut len: u32 = 0_u32;
    if cur.offset(2) > inbufend
        || *cur as i32 != '<' as i32
        || *cur as i32 == '<' as i32 && *cur.offset(1) as i32 == '<' as i32
    {
        return ptr::null_mut();
    }
    cur = cur.offset(1);
    /* PDF Reference does not specify how to treat invalid char */
    while cur < inbufend && len < 4096_u32 {
        skip_white_spaces(&mut cur, inbufend);
        if *cur as i32 == '>' as i32 {
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
            hi = 0i32
        }
        skip_white_spaces(&mut cur, inbufend);
        if *cur as i32 == '>' as i32 {
            break;
        }
        /* 0 is appended if final hex digit is missing */
        let mut lo = if cur < inbufend {
            let fresh9 = cur;
            cur = cur.offset(1);
            xtoi(*fresh9)
        } else {
            0i32
        };
        if lo < 0i32 {
            warn!(
                "Invalid char for hex string <{:x}> treated as <0>.",
                *cur.offset(-1) as i32,
            );
            lo = 0i32
        }
        let fresh10 = len;
        len = len.wrapping_add(1);
        wbuf[fresh10 as usize] = (hi << 4i32 | lo) as u8
    }
    let fresh11 = cur;
    cur = cur.offset(1);
    if *fresh11 as i32 != '>' as i32 {
        return ptr::null_mut();
    }
    *inbuf = cur;
    pst_string_new(wbuf.as_mut_ptr(), len)
}
unsafe fn pst_string_IV(obj: *mut pst_string) -> i32 {
    pst_string_RV(obj) as i32
}
unsafe fn pst_string_RV(obj: *mut pst_string) -> f64 {
    assert!(!obj.is_null());
    let mut p = (*obj).value;
    let end = p.offset((*obj).length as isize);
    let nobj = pst_parse_number(&mut p, end);
    if nobj.is_null() || p != end {
        panic!("Cound not convert string to real value.");
    }
    let rv = pst_getRV(nobj);
    pst_release_obj(nobj);
    rv
}
unsafe fn pst_string_SV(obj: *mut pst_string) -> *mut u8 {
    assert!(!obj.is_null());
    let str = new(((*obj).length.wrapping_add(1_u32) as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memcpy(
        str as *mut libc::c_void,
        (*obj).value as *const libc::c_void,
        (*obj).length as _,
    );
    *str.offset((*obj).length as isize) = '\u{0}' as i32 as u8;
    str
}
unsafe fn pst_string_data_ptr(obj: *mut pst_string) -> *mut libc::c_void {
    assert!(!obj.is_null());
    (*obj).value as *mut libc::c_void
}
unsafe fn pst_string_length(obj: *mut pst_string) -> u32 {
    assert!(!obj.is_null());
    (*obj).length
}
