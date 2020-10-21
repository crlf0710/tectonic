#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_errors::overflow;
use crate::xetex_ini::{
    init_pool_ptr, init_str_ptr, max_strings, pool_ptr, pool_size, str_pool, str_ptr, str_start,
    BUFFER,
};

pub(crate) const EMPTY_STRING: i32 = TOO_BIG_CHAR + 1;

pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
/* tectonic/xetex-stringpool.c: preloaded "string pool" constants
   Copyright 2017-2018 the Tectonic Project
   Licensed under the MIT License.
*/

use std::convert::TryInto;
use std::slice;

type Utf16 = u16;
type StrNumber = i32;
type PoolPointer = i32;

const TOO_BIG_CHAR: i32 = 0x10000;

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
    /// Get the string which begins at str_pool[str_start[s - 65536L]]
    pub fn from(s: StrNumber) -> Self {
        // gets (str_start[s - 65536L])
        unsafe fn str_offset(s: StrNumber) -> Option<usize> {
            let offset: usize = (s - TOO_BIG_CHAR).try_into().ok()?;
            Some(str_start[offset] as _)
        }

        unsafe fn str_slice(s: StrNumber) -> Option<&'static [Utf16]> {
            let offset = str_offset(s)?;
            let len = str_offset(s + 1)? - offset;
            Some(&str_pool[offset..offset + len])
        }

        if let Some(slice) = unsafe { str_slice(s) } {
            PoolString::Span(slice)
        } else {
            PoolString::Char(s as _)
        }
    }

    /// Get string of certain length from str_ptr (which has no inherent length)
    pub fn from_strptr_with_len(len: usize) -> Self {
        unsafe {
            let offset = str_start[(str_ptr - TOO_BIG_CHAR) as usize] as usize;
            let slice = &str_pool[offset..offset + len];
            PoolString::Span(slice)
        }
    }

    pub fn as_slice(&self) -> &[Utf16] {
        match self {
            PoolString::Char(s) => slice::from_ref(s),
            PoolString::Span(s) => s,
        }
    }
}

pub fn length(s: StrNumber) -> i32 {
    // I have no idea what these cases do and why these specific numbers are used
    if let PoolString::Span(string) = PoolString::from(s) {
        string.len() as _
    } else if s >= 32 && s < 127 {
        1
    } else if s <= 127 {
        3
    } else if s < 256 {
        4
    } else {
        8
    }
}

const string_constants: [&str; 2] = ["this marks the start of the stringpool", ""];
pub(crate) unsafe fn load_pool_strings(mut spare_size: i32) -> i32 {
    let mut g: str_number = 0i32;
    for s in &string_constants {
        let total_len = s.len();
        if total_len >= spare_size as usize {
            return 0i32;
        }
        for b in s.as_bytes() {
            str_pool[pool_ptr as usize] = *b as packed_UTF16_code;
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
pub(crate) unsafe fn append_str(mut s: str_number) {
    let mut i = length(s);
    if pool_ptr + i > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    let mut j = str_start[(s - TOO_BIG_CHAR) as usize];
    while i > 0 {
        str_pool[pool_ptr as usize] = str_pool[j as usize];
        pool_ptr += 1;
        j += 1;
        i -= 1
    }
}
pub(crate) unsafe fn str_eq_buf(mut s: str_number, mut k: i32) -> bool {
    let mut j = str_start[s as usize - TOO_BIG_CHAR as usize] as usize;
    while j < str_start[s as usize + 1 - TOO_BIG_CHAR as usize] as usize {
        let mut b = [0; 2];
        for c16 in std::char::from_u32(BUFFER[k as usize] as u32)
            .unwrap()
            .encode_utf16(&mut b)
        {
            if str_pool[j as usize] != *c16 {
                return false;
            }
            j += 1
        }
        k += 1
    }
    true
}
pub(crate) unsafe fn search_string(mut search: str_number) -> Option<str_number> {
    let mut len = length(search);
    if len == 0 {
        return Some(EMPTY_STRING);
    } else {
        for s in (TOO_BIG_CHAR..search).rev() {
            if length(s) == len {
                if PoolString::from(s) == PoolString::from(search) {
                    return Some(s);
                }
            }
        }
    }
    None
}
/* tectonic/xetex-stringpool.h: preloaded "string pool" constants
   Copyright 2017 the Tectonic Project
   Licensed under the MIT License.
*/
pub(crate) unsafe fn slow_make_string() -> str_number {
    let mut t = make_string();
    if let Some(s) = search_string(t) {
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        return s;
    }
    t
}
