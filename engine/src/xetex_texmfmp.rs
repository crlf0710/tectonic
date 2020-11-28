#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_ini::{pool_ptr, pool_size, str_pool};
use crate::xetex_stringpool::{make_string, PoolString, EMPTY_STRING, TOO_BIG_CHAR};
use bridge::ttstub_get_file_md5;
use std::ffi::CString;

use std::env;

pub(crate) type size_t = usize;
pub(crate) type str_number = i32;
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
unsafe fn checkpool_pointer(pool_ptr_0: usize, len: size_t) {
    assert!(
        (pool_ptr_0 as u64) + (len as u64) < pool_size as u64,
        "string pool overflow [{} bytes]",
        pool_size,
    );
}
pub(crate) unsafe fn maketexstring(s: &str) -> i32 {
    if s.is_empty() {
        return EMPTY_STRING;
    }
    let len = s.as_bytes().len();
    checkpool_pointer(pool_ptr, len as _);
    let v: Vec<u16> = s.encode_utf16().collect();
    let len = v.len();
    str_pool[pool_ptr..pool_ptr + len].copy_from_slice(v.as_slice());
    pool_ptr += len;
    make_string()
}
pub(crate) fn gettexstring(s: str_number) -> String {
    if s >= TOO_BIG_CHAR {
        String::from_utf16(PoolString::from(s).as_slice()).unwrap()
    } else {
        String::new()
    }
}
pub(crate) unsafe fn is_new_source(srcfilename: str_number, lineno: i32) -> bool {
    use std::path::Path;
    Path::new(&gettexstring(srcfilename)) != Path::new(&last_source_name) || lineno != last_lineno
}
pub(crate) unsafe fn remember_source_info(srcfilename: str_number, lineno: i32) {
    last_source_name = gettexstring(srcfilename);
    last_lineno = lineno;
}
pub(crate) unsafe fn make_src_special(srcfilename: str_number, lineno: i32) -> String {
    // Always put a space after the number, which makes things easier to parse.
    format!("src:{} {}", lineno, &gettexstring(srcfilename))
}
/* Converts any given string in into an allowed PDF string which is
 * hexadecimal encoded;
 * sizeof(out) should be at least lin*2+1.
 */
unsafe fn convertStringToHexString(in_0: &[u8; 16], out: &mut [u8; 33]) {
    const HEXCHARS: &[u8] = b"0123456789ABCDEF";
    let mut j = 0;
    for &i in in_0 {
        let c = i;
        out[j] = HEXCHARS[(c >> 4 & 0xf) as usize];
        j += 1;
        out[j] = HEXCHARS[(c & 0xf) as usize];
        j += 1;
    }
    out[j] = 0;
}
/* Functions originating in texmfmp.c */
pub(crate) unsafe fn getmd5sum(s: &str, file: bool) -> String {
    let ret;
    let xname = CString::new(s).unwrap();
    let digest = if file {
        let mut digest: [i8; 16] = [0; 16];
        ret = ttstub_get_file_md5(xname.as_ptr(), digest.as_mut_ptr());
        std::mem::transmute(digest)
    } else {
        ret = 0;
        md5::compute(xname.as_bytes())
    };
    if ret != 0 {
        return String::new();
    }
    let mut outbuf: [u8; 33] = [0; 33];
    convertStringToHexString(&digest, &mut outbuf);
    let mut v = Vec::with_capacity(2 * 16);
    for i in 0..(2 * 16) {
        v.push(outbuf[i as usize])
    }
    String::from_utf8(v).unwrap()
}
