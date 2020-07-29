#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_ini::{pool_ptr, pool_size, str_pool, str_start};
use crate::xetex_stringpool::make_string;
use bridge::{ttstub_get_data_md5, ttstub_get_file_md5};
use std::ffi::CString;

use std::env;

pub(crate) type size_t = usize;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
pub(crate) type UInt32 = u32;
pub(crate) type pool_pointer = i32;
pub(crate) type UInt16 = u16;
/* texmfmp.c: Hand-coded routines for TeX or Metafont in C.  Originally
written by Tim Morgan, drawing from other Unix ports of TeX.  This is
a collection of miscellany, everything that's easier (or only
possible) to do in C.

This file is public domain.  */
static mut last_source_name: String = String::new();
static mut last_lineno: i32 = 0;
pub(crate) fn get_date_and_time() -> (i32, i32, i32, i32) {
    use chrono::prelude::*;

    let tm = match env::var("SOURCE_DATE_EPOCH").ok() {
        Some(s) => {
            let epoch = u64::from_str_radix(&s, 10).expect("invalid build date (not a number)");
            std::time::SystemTime::UNIX_EPOCH
                .checked_add(std::time::Duration::from_secs(epoch))
                .expect("time overflow")
                .into()
        }
        None => Local::now(),
    };

    let year = tm.year();
    let month = tm.month();
    let day = tm.day();
    let minutes = tm.hour() * 60 + tm.minute();

    (minutes as _, day as _, month as _, year)
}
unsafe fn checkpool_pointer(mut pool_ptr_0: pool_pointer, mut len: size_t) {
    assert!(
        (pool_ptr_0 as u64) + (len as u64) < pool_size as u64,
        "string pool overflow [{} bytes]",
        pool_size,
    );
}
pub(crate) unsafe fn maketexstring(s: &str) -> i32 {
    if s.is_empty() {
        return (65536 + 1i32 as i64) as i32;
    }
    let len = s.as_bytes().len();
    checkpool_pointer(pool_ptr, len as _);
    let v: Vec<u16> = s.encode_utf16().collect();
    let len = v.len();
    str_pool[pool_ptr as usize..(pool_ptr as usize + len)].copy_from_slice(v.as_slice());
    pool_ptr += len as i32;
    make_string()
}
pub(crate) unsafe fn gettexstring(s: str_number) -> String {
    if s >= 65536 {
        String::from_utf16(
            &str_pool[(str_start[s as usize - 65536] as usize)
                ..(str_start[s as usize + 1 - 65536] as usize)],
        )
        .unwrap()
    } else {
        String::new()
    }
}
pub(crate) unsafe fn to_rust_string(string: *const i8) -> String {
    std::ffi::CStr::from_ptr(string)
        .to_string_lossy()
        .to_string()
}
pub(crate) unsafe fn is_new_source(mut srcfilename: str_number, mut lineno: i32) -> bool {
    use std::path::Path;
    Path::new(&gettexstring(srcfilename)) != Path::new(&last_source_name) || lineno != last_lineno
}
pub(crate) unsafe fn remember_source_info(mut srcfilename: str_number, mut lineno: i32) {
    last_source_name = gettexstring(srcfilename);
    last_lineno = lineno;
}
pub(crate) unsafe fn make_src_special(srcfilename: str_number, lineno: i32) -> pool_pointer {
    let oldpool_ptr: pool_pointer = pool_ptr;

    // Always put a space after the number, which makes things easier to parse.
    let src = format!("src:{} {}", lineno, &gettexstring(srcfilename));

    assert!(
        (pool_ptr as usize) + src.as_bytes().len() < pool_size as usize,
        "string pool overflow"
    );

    for &b in src.as_bytes() {
        str_pool[pool_ptr as usize] = b as u16;
        pool_ptr += 1;
    }
    oldpool_ptr
}
/* Converts any given string in into an allowed PDF string which is
 * hexadecimal encoded;
 * sizeof(out) should be at least lin*2+1.
 */
unsafe fn convertStringToHexString(mut in_0: *const i8, mut out: *mut i8, mut lin: i32) {
    static mut hexchars: [i8; 17] = [
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70, 0,
    ];
    let mut i: i32 = 0;
    let mut j: i32 = 0;
    j = 0i32;
    i = 0i32;
    while i < lin {
        let mut c: u8 = *in_0.offset(i as isize) as u8;
        let fresh13 = j;
        j = j + 1;
        *out.offset(fresh13 as isize) = hexchars[(c as i32 >> 4i32 & 0xfi32) as usize];
        let fresh14 = j;
        j = j + 1;
        *out.offset(fresh14 as isize) = hexchars[(c as i32 & 0xfi32) as usize];
        i += 1
    }
    *out.offset(j as isize) = '\u{0}' as i32 as i8;
}
/* Functions originating in texmfmp.c */
pub(crate) unsafe fn getmd5sum(mut s: str_number, mut file: bool) {
    let mut digest: [i8; 16] = [0; 16];
    let mut outbuf: [i8; 33] = [0; 33];
    let mut ret: i32 = 0;
    let mut i: i32 = 0;
    let mut xname = CString::new(gettexstring(s).as_str()).unwrap();
    if file {
        ret = ttstub_get_file_md5(xname.as_ptr(), digest.as_mut_ptr())
    } else {
        ret = ttstub_get_data_md5(
            xname.as_ptr(),
            xname.as_bytes().len() as _,
            digest.as_mut_ptr(),
        )
    }
    if ret != 0 {
        return;
    }
    if pool_ptr + 2i32 * 16i32 >= pool_size {
        /* error by str_toks that calls str_room(1) */
        return;
    }
    convertStringToHexString(digest.as_mut_ptr(), outbuf.as_mut_ptr(), 16i32);
    i = 0i32;
    while i < 2i32 * 16i32 {
        let fresh15 = pool_ptr;
        pool_ptr = pool_ptr + 1;
        str_pool[fresh15 as usize] = outbuf[i as usize] as u16;
        i += 1
    }
}
