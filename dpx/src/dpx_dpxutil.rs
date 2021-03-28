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

use std::ffi::{CStr, CString};
use std::ptr;

use std::collections::HashMap;

// HtTable implements defers to a normal std::collections::HashMap
// but tracks the iteration order of dvipdfmx's ht_table for backwards compat
// Note: Elements are wrapped in a Box to allow the (highly unsafe!)
// access pattern of storing .get_mut(x).as_mut_ptr() pointers.
pub(crate) struct HtTable<T> {
    backing: HashMap<Vec<u8>, Box<T>>,
    compat_iteration_order: HashMap<u32, Vec<Vec<u8>>>,
}

impl<T> HtTable<T> {
    pub(crate) fn new() -> Self {
        unsafe {
            let backing = HashMap::new();
            let compat_iteration_order = HashMap::new();

            HtTable {
                backing,
                compat_iteration_order,
            }
        }
    }

    pub(crate) fn hash_key(key: &[u8]) -> u32 {
        key.iter()
            .fold(0u32, |a, &b| {
                (a << 5).wrapping_add(a).wrapping_add(b as u32)
            })
            .wrapping_rem(503)
    }

    pub(crate) fn insert(&mut self, key: Vec<u8>, val: Box<T>) {
        self.compat_iteration_order
            .entry(Self::hash_key(&key))
            .or_default()
            .push(key.clone());
        self.backing.insert(key, val);
    }

    pub(crate) fn get_mut(&mut self, key: &[u8]) -> Option<&mut Box<T>> {
        self.backing.get_mut(key)
    }

    pub(crate) fn clear(&mut self) {
        *self = Self::new();
    }
}

impl<T> Drop for HtTable<T> {
    fn drop(&mut self) {
        // drop entries in iteration order
        for i in 0u32..503 {
            if let Some(keys) = self.compat_iteration_order.get(&i) {
                for key in keys {
                    self.backing.remove(key);
                }
            }
        }
    }
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct ht_entry {
    pub(crate) key: Vec<u8>,
    pub(crate) value: *mut libc::c_void,
    pub(crate) next: *mut ht_entry,
}
impl ht_entry {
    pub(crate) fn get_key(&self) -> &[u8] {
        &self.key
    }
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

pub(crate) fn is_space(c: &u8) -> bool {
    c.is_ascii_whitespace() || *c == 0
}
pub(crate) fn is_delim(c: &u8) -> bool {
    b"()/<>[]{}%".contains(c)
}

pub(crate) unsafe fn skip_white_spaces(s: &mut &[u8]) {
    while !s.is_empty() {
        if !(s[0].is_ascii_whitespace() || s[0] == 0) {
            break;
        }
        *s = &s[1..];
    }
}

pub(crate) unsafe fn ht_init_table(mut ht: *mut ht_table, hval_free_fn: hval_free_func) {
    assert!(!ht.is_null());
    for i in 0..503 {
        (*ht).table[i] = ptr::null_mut();
    }
    (*ht).count = 0;
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
            let next = (*hent).next;
            let _ = Box::from_raw(hent);
            hent = next
        }
        (*ht).table[i] = ptr::null_mut();
    }
    (*ht).count = 0;
    (*ht).hval_free_fn = None;
}

pub(crate) unsafe fn ht_table_size(ht: *mut ht_table) -> i32 {
    assert!(!ht.is_null());
    (*ht).count
}
unsafe fn get_hash(key: &[u8]) -> u32 {
    let mut hkey: u32 = 0_u32;
    for i in 0..key.len() {
        hkey = (hkey << 5)
            .wrapping_add(hkey)
            .wrapping_add(key[i] as i8 as u32);
    }
    hkey.wrapping_rem(503_u32)
}

pub(crate) unsafe fn ht_lookup_table(ht: *mut ht_table, key: &[u8]) -> *mut libc::c_void {
    assert!(!ht.is_null() && !key.is_empty());
    let hkey = get_hash(key);
    let mut hent = (*ht).table[hkey as usize];
    while !hent.is_null() {
        if (*hent).get_key() == key {
            return (*hent).value;
        }
        hent = (*hent).next
    }
    ptr::null_mut()
}

pub(crate) unsafe fn ht_append_table(mut ht: *mut ht_table, key: &[u8], value: *mut libc::c_void) {
    let mut last: *mut ht_entry = ptr::null_mut();
    let hkey = get_hash(key) as usize;
    let mut hent = (*ht).table[hkey];
    if hent.is_null() {
        hent = Box::into_raw(Box::new(ht_entry {
            key: Vec::new(),
            value: 0 as *mut libc::c_void,
            next: ptr::null_mut(),
        }));
        (*ht).table[hkey] = hent
    } else {
        while !hent.is_null() {
            last = hent;
            hent = (*hent).next
        }
        hent = Box::into_raw(Box::new(ht_entry {
            key: Vec::new(),
            value: 0 as *mut libc::c_void,
            next: ptr::null_mut(),
        }));
        (*last).next = hent
    }
    (*hent).key = Vec::from(key);
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
            return 0;
        }
    }
    -1
}

pub(crate) unsafe fn ht_clear_iter(mut iter: *mut ht_iter) {
    if !iter.is_null() {
        (*iter).index = 503;
        (*iter).curr = ptr::null_mut();
        (*iter).hash = ptr::null_mut()
    };
}

impl ht_iter {
    pub(crate) unsafe fn get_key(&self) -> &[u8] {
        let hent = self.curr as *mut ht_entry;
        if !hent.is_null() {
            &(*hent).key
        } else {
            &[]
        }
    }
}

pub(crate) unsafe fn ht_iter_getval(iter: *const ht_iter) -> *const libc::c_void {
    let hent = (*iter).curr as *mut ht_entry;
    if !iter.is_null() && !hent.is_null() {
        (*hent).value
    } else {
        ptr::null_mut()
    }
}

pub(crate) unsafe fn ht_iter_next(mut iter: *mut ht_iter) -> i32 {
    assert!(!iter.is_null());
    let ht = (*iter).hash;
    let mut hent = (*iter).curr as *mut ht_entry;
    hent = (*hent).next;
    while hent.is_null() && {
        (*iter).index += 1;
        (*iter).index < 503
    } {
        hent = (*ht).table[(*iter).index as usize]
    }
    (*iter).curr = hent as *mut libc::c_void;
    if !hent.is_null() {
        0
    } else {
        -1
    }
}
pub(crate) trait ParseCString {
    fn parse_c_string(&mut self) -> Option<CString>;
}

impl ParseCString for &[u8] {
    fn parse_c_string(&mut self) -> Option<CString> {
        fn read_c_escchar(pp: &mut &[u8]) -> (i32, u8) {
            let mut c = 0;
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
                                if p[0].is_ascii_lowercase() {
                                    (p[0] - b'a' + 10) as i32
                                } else {
                                    (p[0] - b'A' + 10) as i32
                                }
                            });
                        i_0 += 1;
                        p = &p[1..];
                    }
                }
                _ => {
                    warn!("Unknown escape char sequence: \\{}", char::from(p[0]));
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
            let mut s = -1;
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
        let l = read_c_litstrc(&mut None, 0, &mut p);
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
    fn parse_c_ident(&mut self) -> Option<String>;
}
impl ParseCIdent for &[u8] {
    fn parse_c_ident(&mut self) -> Option<String> {
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
        let s = Some(String::from_utf8_lossy(&self[..n]).to_string());
        *self = &self[n..];
        s
    }
}

pub(crate) trait ParseFloatDecimal {
    fn parse_float_decimal(&mut self) -> Option<CString>;

    fn parse_float_decimal_to_f64(&mut self) -> Option<f64> {
        self.parse_float_decimal()
            .map(|f| f.to_str().unwrap().parse().unwrap())
    }
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
