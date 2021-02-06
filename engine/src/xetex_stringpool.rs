#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_errors::overflow;
use crate::xetex_ini::BUFFER;

/// the characters
pub(crate) static mut str_pool: Vec<u16> = Vec::new();

/// the starting pointers
pub(crate) static mut str_start: Vec<usize> = Vec::new();

/// first unused position in |str_pool|
pub(crate) static mut pool_ptr: usize = 0;

/// number of the current string being created
pub(crate) static mut str_ptr: str_number = 0;

/// the starting value of `pool_ptr`
pub(crate) static mut init_pool_ptr: usize = 0;

/// the starting value of `str_ptr`
pub(crate) static mut init_str_ptr: str_number = 0;

pub(crate) static mut max_strings: usize = 0;

pub(crate) static mut strings_free: usize = 0;

pub(crate) static mut string_vacancies: usize = 0;

pub(crate) static mut pool_size: usize = 0;

pub(crate) static mut pool_free: usize = 0;

pub(crate) const TOO_BIG_CHAR: i32 = 0x10000;
pub(crate) const EMPTY_STRING: i32 = TOO_BIG_CHAR + 1;

pub(crate) type str_number = i32;
/* tectonic/xetex-stringpool.c: preloaded "string pool" constants
   Copyright 2017-2018 the Tectonic Project
   Licensed under the MIT License.
*/

use std::convert::TryInto;
use std::slice;

type Utf16 = u16;
type StrNumber = i32;

pub enum PoolString {
    Char(Utf16),
    Span(&'static [Utf16]),
}

impl std::cmp::PartialEq for PoolString {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PoolString {
    // gets (str_start[s - TOO_BIG_CHAR])
    unsafe fn str_offset(s: StrNumber) -> Option<usize> {
        let offset: usize = (s - TOO_BIG_CHAR).try_into().ok()?;
        Some(str_start[offset])
    }
    /// Get the string which begins at str_pool[str_start[s - TOO_BIG_CHAR]]
    pub fn from(s: StrNumber) -> Self {
        unsafe fn str_slice(s: StrNumber) -> Option<&'static [Utf16]> {
            let offset = PoolString::str_offset(s)?;
            let len = PoolString::str_offset(s + 1)? - offset;
            Some(&str_pool[offset..offset + len])
        }

        if let Some(slice) = unsafe { str_slice(s) } {
            Self::Span(slice)
        } else {
            Self::Char(s as _)
        }
    }

    pub fn current() -> Self {
        unsafe {
            let offset = Self::str_offset(str_ptr).unwrap();
            let len = pool_ptr - offset;
            Self::Span(&str_pool[offset..offset + len])
        }
    }

    /*/// Get string of certain length from str_ptr (which has no inherent length)
    pub fn from_strptr_with_len(len: usize) -> Self {
        unsafe {
            let offset = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
            let slice = &str_pool[offset..offset + len];
            Self::Span(slice)
        }
    }*/

    pub fn as_slice(&self) -> &[Utf16] {
        match self {
            Self::Char(_) => unreachable!(),
            Self::Span(s) => s,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Span(s) => s.len(),
            Self::Char(_) => unreachable!(),
        }
    }

    pub unsafe fn flush() {
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
    }

    pub fn check_capacity(len: usize) {
        unsafe {
            if pool_ptr + len > pool_size {
                overflow("pool size", pool_size - init_pool_ptr);
            }
        }
    }
    pub fn add_new_from_str(s: &str) -> i32 {
        unsafe {
            for i in s.encode_utf16() {
                if pool_ptr < pool_size {
                    str_pool[pool_ptr as usize] = i;
                    pool_ptr += 1
                }
            }
            Self::check_capacity(1);
            make_string()
        }
    }
}

impl std::fmt::Display for PoolString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let slice = match self {
            Self::Char(s) => slice::from_ref(s),
            Self::Span(s) => s,
        };
        for c in std::char::decode_utf16(slice.iter().cloned()) {
            if let Ok(c) = c {
                c.fmt(f)?;
            } else {
                return Err(std::fmt::Error);
            }
        }
        Ok(())
    }
}

const string_constants: [&str; 2] = ["this marks the start of the stringpool", ""];
pub(crate) unsafe fn load_pool_strings(spare_size: usize) -> i32 {
    let mut g: str_number = 0;
    for s in &string_constants {
        let total_len = s.len();
        if total_len >= spare_size {
            return 0;
        }
        for b in s.as_bytes() {
            str_pool[pool_ptr] = *b as _;
            pool_ptr += 1;
        }
        g = make_string()
        /* Returns 0 on error. */
    }
    g
}

pub(crate) unsafe fn make_string() -> str_number {
    if str_ptr == max_strings as i32 {
        overflow("number of strings", max_strings - init_str_ptr as usize);
    }
    str_ptr += 1;
    str_start[(str_ptr - TOO_BIG_CHAR) as usize] = pool_ptr;
    str_ptr - 1
}
pub(crate) unsafe fn append_str(s: str_number) {
    let ps = PoolString::from(s);
    PoolString::check_capacity(ps.len());
    for &c in ps.as_slice() {
        str_pool[pool_ptr] = c;
        pool_ptr += 1;
    }
}
pub(crate) unsafe fn str_eq_buf(s: &PoolString, mut k: usize) -> bool {
    for &j in s.as_slice() {
        let mut b = [0; 2];
        for c16 in std::char::from_u32(BUFFER[k] as u32)
            .unwrap()
            .encode_utf16(&mut b)
        {
            if j != *c16 {
                return false;
            }
        }
        k += 1
    }
    true
}
pub(crate) unsafe fn search_string(search: str_number) -> Option<str_number> {
    let ps = PoolString::from(search);
    if ps.len() == 0 {
        return Some(EMPTY_STRING);
    } else {
        for s in (TOO_BIG_CHAR..search).rev() {
            if PoolString::from(s) == ps {
                return Some(s);
            }
        }
    }
    None
}
pub(crate) unsafe fn slow_make_string() -> str_number {
    let t = make_string();
    if let Some(s) = search_string(t) {
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        return s;
    }
    t
}
