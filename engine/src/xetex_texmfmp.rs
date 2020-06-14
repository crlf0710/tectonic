#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::core_memory::xmalloc;
use crate::stub_stdio::sprintf;
use crate::xetex_ini::{pool_ptr, pool_size, str_pool, str_start};
use crate::xetex_io::{bytesFromUTF8, firstByteMark, offsetsFromUTF8};
use crate::xetex_stringpool::make_string;
use bridge::{ttstub_get_data_md5, ttstub_get_file_md5};
use libc::{free, strlen};

use std::env;
use std::ptr;

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
static mut last_source_name: *mut i8 = ptr::null_mut();
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
        !((pool_ptr_0 as u64).wrapping_add(len as u64) >= pool_size as u64),
        "string pool overflow [{} bytes]",
        pool_size,
    );
}
pub(crate) unsafe fn maketexstring(s: &[u8]) -> i32 {
    let mut rval: UInt32 = 0;
    let mut cp = s;
    if s.is_empty() {
        return (65536 + 1i32 as i64) as i32;
    }
    let len = s.len();
    checkpool_pointer(pool_ptr, len as _);
    while !cp.is_empty() {
        rval = cp[0] as UInt32;
        cp = &cp[1..];
        let mut extraBytes: UInt16 = bytesFromUTF8[rval as usize] as UInt16;

        assert!(extraBytes < 6);
        for _ in (1..=extraBytes).rev() {
            /* note: code falls through cases! */
            rval <<= 6i32; /* max UTF16->UTF8 expansion (code units, not bytes) */
            if !cp.is_empty() {
                rval += cp[0] as u32;
                cp = &cp[1..];
            }
        }
        rval = (rval as u32).wrapping_sub(offsetsFromUTF8[extraBytes as usize]) as UInt32 as UInt32;
        if rval > 0xffff_u32 {
            rval = (rval as u32).wrapping_sub(0x10000_u32) as UInt32 as UInt32;
            let fresh6 = pool_ptr;
            pool_ptr = pool_ptr + 1;
            str_pool[fresh6 as usize] =
                (0xd800_u32).wrapping_add(rval.wrapping_div(0x400_u32)) as packed_UTF16_code;
            let fresh7 = pool_ptr;
            pool_ptr = pool_ptr + 1;
            str_pool[fresh7 as usize] =
                (0xdc00_u32).wrapping_add(rval.wrapping_rem(0x400_u32)) as packed_UTF16_code
        } else {
            let fresh8 = pool_ptr;
            pool_ptr = pool_ptr + 1;
            str_pool[fresh8 as usize] = rval as packed_UTF16_code
        }
    }
    make_string()
}
pub(crate) unsafe fn gettexstring(mut s: str_number) -> *mut i8 {
    let mut len: pool_pointer = 0;
    let mut i: pool_pointer = 0;
    let mut j: pool_pointer = 0;
    let mut name: *mut i8 = ptr::null_mut();
    if s as i64 >= 65536 {
        len =
            str_start[((s + 1i32) as i64 - 65536) as usize] - str_start[(s as i64 - 65536) as usize]
    } else {
        len = 0i32
    }
    name = xmalloc((len * 3i32 + 1i32) as size_t) as *mut i8;
    i = 0i32;
    j = 0i32;
    while i < len {
        let mut c: u32 = str_pool[(i + str_start[(s as i64 - 65536) as usize]) as usize] as u32;
        if c >= 0xd800_u32 && c <= 0xdbff_u32 {
            i += 1;
            let mut lo: u32 =
                str_pool[(i + str_start[(s as i64 - 65536) as usize]) as usize] as u32;
            if lo >= 0xdc00_u32 && lo <= 0xdfff_u32 {
                c = c
                    .wrapping_sub(0xd800_u32)
                    .wrapping_mul(0x400_u32)
                    .wrapping_add(lo)
                    .wrapping_sub(0xdc00_u32)
                    .wrapping_add(0x10000_u32)
            } else {
                c = 0xfffd_u32
            }
        }
        let bytesToWrite = if c < 0x80_u32 {
            1_usize
        } else if c < 0x800_u32 {
            2
        } else if c < 0x10000_u32 {
            3
        } else if c < 0x110000_u32 {
            4
        } else {
            c = 0xfffd_u32;
            3
        };
        j += bytesToWrite as i32;
        for k in (1..=bytesToWrite).rev() {
            /* note: everything falls through. */
            j -= 1;
            if k == 1 {
                *name.offset(j as isize) = (c | firstByteMark[bytesToWrite] as u32) as i8;
            } else {
                *name.offset(j as isize) = ((c | 0x80) & 0xbf) as i8;
                c >>= 6;
            }
        }
        j += bytesToWrite as i32;
        i += 1
    }
    *name.offset(j as isize) = 0_i8;
    name
}
unsafe fn compare_paths(mut p1: *const i8, mut p2: *const i8) -> i32 {
    let mut ret: i32 = 0;
    loop {
        ret = *p1 as i32 - *p2 as i32;
        if !(ret == 0i32 && *p2 != 0 || *p1 as i32 == '/' as i32 && *p2 as i32 == '/' as i32) {
            break;
        }
        p1 = p1.offset(1);
        p2 = p2.offset(1)
    }
    ret = if ret < 0i32 {
        -1i32
    } else if ret > 0i32 {
        1i32
    } else {
        0i32
    };
    ret
}
pub(crate) unsafe fn is_new_source(mut srcfilename: str_number, mut lineno: i32) -> bool {
    let mut name: *mut i8 = gettexstring(srcfilename);
    compare_paths(name, last_source_name) != 0i32 || lineno != last_lineno
}
pub(crate) unsafe fn remember_source_info(mut srcfilename: str_number, mut lineno: i32) {
    free(last_source_name as *mut libc::c_void);
    last_source_name = gettexstring(srcfilename);
    last_lineno = lineno;
}
pub(crate) unsafe fn make_src_special(
    mut srcfilename: str_number,
    mut lineno: i32,
) -> pool_pointer {
    let mut oldpool_ptr: pool_pointer = pool_ptr;
    let mut filename: *mut i8 = gettexstring(srcfilename);
    /* FIXME: Magic number. */
    let mut buf: [i8; 40] = [0; 40];
    let mut s: *mut i8 = buf.as_mut_ptr();
    /* Always put a space after the number, which makes things easier
     * to parse.
     */
    sprintf(
        buf.as_mut_ptr(),
        b"src:%d \x00" as *const u8 as *const i8,
        lineno,
    );
    assert!(
        !((pool_ptr as usize)
            .wrapping_add(strlen(buf.as_mut_ptr()))
            .wrapping_add(strlen(filename))
            >= pool_size as usize),
        "string pool overflow"
    );
    s = buf.as_mut_ptr();
    while *s != 0 {
        let fresh9 = s;
        s = s.offset(1);
        let fresh10 = pool_ptr;
        pool_ptr = pool_ptr + 1;
        str_pool[fresh10 as usize] = *fresh9 as packed_UTF16_code
    }
    s = filename;
    while *s != 0 {
        let fresh11 = s;
        s = s.offset(1);
        let fresh12 = pool_ptr;
        pool_ptr = pool_ptr + 1;
        str_pool[fresh12 as usize] = *fresh11 as packed_UTF16_code
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
    let mut xname: *mut i8 = ptr::null_mut();
    let mut ret: i32 = 0;
    let mut i: i32 = 0;
    xname = gettexstring(s);
    if file {
        ret = ttstub_get_file_md5(xname, digest.as_mut_ptr())
    } else {
        ret = ttstub_get_data_md5(xname, strlen(xname) as _, digest.as_mut_ptr())
    }
    free(xname as *mut libc::c_void);
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
