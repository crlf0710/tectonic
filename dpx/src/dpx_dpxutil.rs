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

use std::ffi::{CStr, CString};
use std::ptr;

use super::dpx_mem::new;
use crate::mfree;
use libc::{free, memcmp, memcpy};

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct ht_entry {
    pub(crate) key: *mut i8,
    pub(crate) keylen: i32,
    pub(crate) value: *mut libc::c_void,
    pub(crate) next: *mut ht_entry,
}
pub(crate) type hval_free_func = Option<unsafe fn(_: *mut libc::c_void) -> ()>;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct ht_table {
    pub(crate) count: i32,
    pub(crate) hval_free_fn: hval_free_func,
    pub(crate) table: [*mut ht_entry; 503],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct ht_iter {
    pub(crate) index: i32,
    pub(crate) curr: *mut libc::c_void,
    pub(crate) hash: *mut ht_table,
}
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

pub(crate) fn xtoi(c: u8) -> i32 {
    if c.is_ascii_digit() {
        return (c - b'0') as i32;
    }
    if (b'A'..=b'F').contains(&c) {
        return (c - b'A' + 10) as i32;
    }
    if (b'a'..=b'f').contains(&c) {
        return (c - b'a' + 10) as i32;
    }
    -1
}

pub(crate) unsafe fn skip_white_spaces(s: *mut *mut u8, endptr: *mut u8) {
    while *s < endptr {
        if !(**s as i32 == ' ' as i32
            || **s as i32 == '\t' as i32
            || **s as i32 == '\u{c}' as i32
            || **s as i32 == '\r' as i32
            || **s as i32 == '\n' as i32
            || **s as i32 == '\u{0}' as i32)
        {
            break;
        }
        *s = (*s).offset(1)
    }
}

pub(crate) unsafe fn ht_init_table(mut ht: *mut ht_table, hval_free_fn: hval_free_func) {
    assert!(!ht.is_null());
    for i in 0..503 {
        (*ht).table[i] = ptr::null_mut();
    }
    (*ht).count = 0i32;
    (*ht).hval_free_fn = hval_free_fn;
}

pub(crate) unsafe fn ht_clear_table(mut ht: *mut ht_table) {
    assert!(!ht.is_null());
    for i in 0..503 {
        let mut hent = (*ht).table[i];
        while !hent.is_null() {
            if !(*hent).value.is_null() && (*ht).hval_free_fn.is_some() {
                (*ht).hval_free_fn.expect("non-null function pointer")((*hent).value);
            }
            (*hent).value = ptr::null_mut();
            if !(*hent).key.is_null() {
                free((*hent).key as *mut libc::c_void);
            }
            (*hent).key = ptr::null_mut();
            let next = (*hent).next;
            free(hent as *mut libc::c_void);
            hent = next
        }
        (*ht).table[i] = ptr::null_mut();
    }
    (*ht).count = 0i32;
    (*ht).hval_free_fn = None;
}

pub(crate) unsafe fn ht_table_size(ht: *mut ht_table) -> i32 {
    assert!(!ht.is_null());
    (*ht).count
}
unsafe fn get_hash(key: *const libc::c_void, keylen: i32) -> u32 {
    let mut hkey: u32 = 0_u32;
    for i in 0..keylen {
        hkey = (hkey << 5i32)
            .wrapping_add(hkey)
            .wrapping_add(*(key as *const i8).offset(i as isize) as u32);
    }
    hkey.wrapping_rem(503_u32)
}

pub(crate) unsafe fn ht_lookup_table(
    ht: *mut ht_table,
    key: *const libc::c_void,
    keylen: i32,
) -> *mut libc::c_void {
    assert!(!ht.is_null() && !key.is_null());
    let hkey = get_hash(key, keylen);
    let mut hent = (*ht).table[hkey as usize];
    while !hent.is_null() {
        if (*hent).keylen == keylen
            && memcmp((*hent).key as *const libc::c_void, key, keylen as _) == 0
        {
            return (*hent).value;
        }
        hent = (*hent).next
    }
    ptr::null_mut()
}

pub(crate) unsafe fn ht_remove_table(
    mut ht: *mut ht_table,
    key: *const libc::c_void,
    keylen: i32,
) -> i32
/* returns 1 if the element was found and removed and 0 otherwise */ {
    assert!(!ht.is_null() && !key.is_null());
    let hkey = get_hash(key, keylen) as usize;
    let mut hent = (*ht).table[hkey];
    let mut prev = ptr::null_mut();
    while !hent.is_null() {
        if (*hent).keylen == keylen
            && memcmp((*hent).key as *const libc::c_void, key, keylen as _) == 0
        {
            break;
        }
        prev = hent;
        hent = (*hent).next
    }
    if !hent.is_null() {
        (*hent).key = mfree((*hent).key as *mut libc::c_void) as *mut i8;
        (*hent).keylen = 0i32;
        if !(*hent).value.is_null() && (*ht).hval_free_fn.is_some() {
            (*ht).hval_free_fn.expect("non-null function pointer")((*hent).value);
        }
        (*hent).value = ptr::null_mut();
        if !prev.is_null() {
            (*prev).next = (*hent).next
        } else {
            (*ht).table[hkey] = (*hent).next
        }
        free(hent as *mut libc::c_void);
        (*ht).count -= 1;
        return 1i32;
    } else {
        return 0i32;
    };
}
/* replace... */

pub(crate) unsafe fn ht_insert_table(
    mut ht: *mut ht_table,
    key: *const libc::c_void,
    keylen: i32,
    value: *mut libc::c_void,
) {
    assert!(!ht.is_null() && !key.is_null());
    let hkey = get_hash(key, keylen) as usize;
    let mut hent = (*ht).table[hkey];
    let mut prev = ptr::null_mut();
    while !hent.is_null() {
        if (*hent).keylen == keylen
            && memcmp((*hent).key as *const libc::c_void, key, keylen as _) == 0
        {
            break;
        }
        prev = hent;
        hent = (*hent).next
    }
    if !hent.is_null() {
        if !(*hent).value.is_null() && (*ht).hval_free_fn.is_some() {
            (*ht).hval_free_fn.expect("non-null function pointer")((*hent).value);
        }
        (*hent).value = value
    } else {
        hent = new((1_u64).wrapping_mul(::std::mem::size_of::<ht_entry>() as u64) as u32)
            as *mut ht_entry;
        (*hent).key = new((keylen).wrapping_mul(::std::mem::size_of::<i8>() as _) as _) as *mut i8;
        memcpy((*hent).key as *mut libc::c_void, key, keylen as _);
        (*hent).keylen = keylen;
        (*hent).value = value;
        (*hent).next = ptr::null_mut();
        if !prev.is_null() {
            (*prev).next = hent
        } else {
            (*ht).table[hkey] = hent
        }
        (*ht).count += 1
    };
}

pub(crate) unsafe fn ht_append_table(
    mut ht: *mut ht_table,
    key: *const libc::c_void,
    keylen: i32,
    value: *mut libc::c_void,
) {
    let mut last: *mut ht_entry = ptr::null_mut();
    let hkey = get_hash(key, keylen) as usize;
    let mut hent = (*ht).table[hkey];
    if hent.is_null() {
        hent = new((1_u64).wrapping_mul(::std::mem::size_of::<ht_entry>() as u64) as u32)
            as *mut ht_entry;
        (*ht).table[hkey] = hent
    } else {
        while !hent.is_null() {
            last = hent;
            hent = (*hent).next
        }
        hent = new((1_u64).wrapping_mul(::std::mem::size_of::<ht_entry>() as u64) as u32)
            as *mut ht_entry;
        (*last).next = hent
    }
    (*hent).key =
        new((keylen as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    memcpy((*hent).key as *mut libc::c_void, key, keylen as _);
    (*hent).keylen = keylen;
    (*hent).value = value;
    (*hent).next = ptr::null_mut();
    (*ht).count += 1;
}

pub(crate) unsafe fn ht_set_iter(ht: *mut ht_table, mut iter: *mut ht_iter) -> i32 {
    assert!(!ht.is_null() && !iter.is_null());
    for i in 0..503 {
        if !(*ht).table[i as usize].is_null() {
            (*iter).index = i;
            (*iter).curr = (*ht).table[i as usize] as *mut libc::c_void;
            (*iter).hash = ht;
            return 0i32;
        }
    }
    -1i32
}

pub(crate) unsafe fn ht_clear_iter(mut iter: *mut ht_iter) {
    if !iter.is_null() {
        (*iter).index = 503i32;
        (*iter).curr = ptr::null_mut();
        (*iter).hash = ptr::null_mut()
    };
}

pub(crate) unsafe fn ht_iter_getkey(iter: *mut ht_iter, keylen: *mut i32) -> *mut i8 {
    let hent = (*iter).curr as *mut ht_entry;
    if !iter.is_null() && !hent.is_null() {
        *keylen = (*hent).keylen;
        return (*hent).key;
    } else {
        *keylen = 0i32;
        return ptr::null_mut();
    };
}

pub(crate) unsafe fn ht_iter_getval(iter: *mut ht_iter) -> *mut libc::c_void {
    let hent = (*iter).curr as *mut ht_entry;
    if !iter.is_null() && !hent.is_null() {
        return (*hent).value;
    } else {
        return ptr::null_mut();
    };
}

pub(crate) unsafe fn ht_iter_next(mut iter: *mut ht_iter) -> i32 {
    assert!(!iter.is_null());
    let ht = (*iter).hash;
    let mut hent = (*iter).curr as *mut ht_entry;
    hent = (*hent).next;
    while hent.is_null() && {
        (*iter).index += 1;
        (*iter).index < 503i32
    } {
        hent = (*ht).table[(*iter).index as usize]
    }
    (*iter).curr = hent as *mut libc::c_void;
    if !hent.is_null() {
        0i32
    } else {
        -1i32
    }
}
pub(crate) trait ParseCString {
    fn parse_c_string(&mut self) -> Option<CString>;
}

impl ParseCString for &[u8] {
    fn parse_c_string(&mut self) -> Option<CString> {
        fn read_c_escchar(pp: &mut &[u8]) -> (i32, u8) {
            let mut c = 0i32;
            let mut l = 1;
            let mut p = *pp;
            match p[0] {
                b'a' => {
                    c = '\u{7}' as i32;
                    p = &p[1..];
                }
                b'b' => {
                    c = '\u{8}' as i32;
                    p = &p[1..];
                }
                b'f' => {
                    c = '\u{c}' as i32;
                    p = &p[1..];
                }
                b'n' => {
                    c = '\n' as i32;
                    p = &p[1..];
                }
                b'r' => {
                    c = '\r' as i32;
                    p = &p[1..];
                }
                b't' => {
                    c = '\t' as i32;
                    p = &p[1..];
                }
                b'v' => {
                    c = '\u{b}' as i32;
                    p = &p[1..];
                }
                b'\\' | b'?' | b'\'' | b'\"' => {
                    c = p[0] as i32;
                    p = &p[1..];
                }
                b'\n' => {
                    l = 0;
                    p = &p[1..];
                }
                b'\r' => {
                    p = &p[1..];
                    if !p.is_empty() && p[0] == b'\n' {
                        p = &p[1..];
                    }
                    l = 0;
                }
                b'0'..=b'7' => {
                    c = 0;
                    let mut i = 0;
                    while i < 3 && !p.is_empty() && p[0] >= b'0' && p[0] <= b'7' {
                        c = (c << 3) + (p[0] as i32 - b'0' as i32);
                        i += 1;
                        p = &p[1..];
                    }
                }
                b'x' => {
                    c = 0;
                    let mut i_0 = 0;
                    p = &p[1..];
                    while i_0 < 2 && !p.is_empty() && p[0].is_ascii_hexdigit() {
                        c = (c << 4)
                            + (if p[0].is_ascii_digit() {
                                (p[0] - b'0') as i32
                            } else {
                                (if p[0].is_ascii_lowercase() {
                                    (p[0] - b'a' + 10) as i32
                                } else {
                                    (p[0] - b'A' + 10) as i32
                                })
                            });
                        i_0 += 1;
                        p = &p[1..];
                    }
                }
                _ => {
                    warn!("Unknown escape char sequence: \\{}", char::from(p[0]),);
                    l = 0;
                    p = &p[1..];
                }
            }
            *pp = p;
            (l, c as u8)
        }

        const C_QUOTE: u8 = b'"';
        const C_ESCAPE: u8 = b'\\';
        fn read_c_litstrc(q: &mut Option<Vec<u8>>, len: i32, pp: &mut &[u8]) -> i32 {
            let mut s = -1i32;
            let mut l = 0;
            let mut p = *pp;
            while s == -1 && !p.is_empty() {
                match p[0] {
                    C_QUOTE => {
                        s = 0;
                        p = &p[1..];
                    }
                    C_ESCAPE => {
                        if q.is_some() && l == len {
                            s = -3
                        } else {
                            p = &p[1..];
                            let (size, r) = read_c_escchar(&mut p);
                            l += size;
                            if let Some(v) = q.as_mut() {
                                v[l as usize] = r;
                            }
                        }
                    }
                    b'\n' | b'\r' => s = -2,
                    _ => {
                        if q.is_some() && l == len {
                            s = -3
                        } else {
                            if let Some(v) = q.as_mut() {
                                v[l as usize] = p[0];
                            }
                            l += 1;
                            p = &p[1..];
                        }
                    }
                }
            }
            if s == 0 {
                if q.is_some() && l == len {
                    s = -3
                } else if let Some(v) = q.as_mut() {
                    v[l as usize] = '\u{0}' as u8;
                    l += 1;
                }
            }
            *pp = p;
            if s == 0 {
                l
            } else {
                s
            }
        }

        let mut q = None;
        let mut p = *self;
        if p.is_empty() || p[0] != b'\"' {
            return None;
        }
        p = &p[1..];
        let l = read_c_litstrc(&mut None, 0i32, &mut p);
        if l >= 0 {
            let mut v = Some(vec![0u8; l as usize + 1]);
            p = &(*self)[1..];
            read_c_litstrc(&mut v, l + 1, &mut p);
            let v = v.unwrap();
            let pos = v.iter().position(|&x| x == 0).unwrap();
            q = Some(CStr::from_bytes_with_nul(&v[..pos + 1]).unwrap().to_owned());
        }
        *self = p;
        q
    }
}

pub(crate) trait ParseCIdent {
    fn parse_c_ident(&mut self) -> Option<CString>;
}
impl ParseCIdent for &[u8] {
    fn parse_c_ident(&mut self) -> Option<CString> {
        if self.len() == 0 || !(self[0] == b'_' || self[0].is_ascii_alphabetic()) {
            return None;
        }
        let mut n = 0;
        for p in *self {
            if !(*p == b'_' || p.is_ascii_alphanumeric()) {
                break;
            }
            n += 1;
        }
        let s = Some(CString::new(&self[..n]).unwrap());
        *self = &self[n..];
        s
    }
}

pub(crate) trait ParseFloatDecimal {
    fn parse_float_decimal(&mut self) -> Option<CString>;
}

impl ParseFloatDecimal for &[u8] {
    fn parse_float_decimal(&mut self) -> Option<CString> {
        let len = self.len();
        let mut q = None;
        let mut p = *self;
        if p.is_empty() {
            return None;
        }
        if p[0] == b'+' || p[0] == b'-' {
            p = &p[1..];
        }
        /* 1. .01 001 001E-001 */
        let mut s = 0;
        let mut n = 0;
        while !p.is_empty() && s >= 0 {
            match p[0] {
                b'+' | b'-' => {
                    if s != 2 {
                        s = -1;
                    } else {
                        s = 3;
                        p = &p[1..];
                    }
                }
                b'.' => {
                    if s > 0 {
                        s = -1;
                    } else {
                        s = 1;
                        p = &p[1..];
                    }
                }
                b'0'..=b'9' => {
                    n += 1;
                    p = &p[1..];
                }
                b'E' | b'e' => {
                    if n == 0 || s == 2 {
                        s = -1;
                    } else {
                        s = 2;
                        p = &p[1..];
                    }
                }
                _ => {
                    s = -1;
                }
            }
        }
        if n != 0 {
            n = len - p.len();
            q = Some(CString::new(&self[..n]).unwrap());
        }
        *self = p;
        q
    }
}
