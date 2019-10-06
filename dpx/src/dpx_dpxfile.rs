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
    unused_mut
)]

use std::io::{Read, Seek, SeekFrom};

use super::dpx_mem::{new, xstrdup};
use super::dpx_numbers::{tt_get_unsigned_pair, tt_get_unsigned_quad};
use crate::mfree;
use crate::{ttstub_input_close, ttstub_input_open};
#[cfg(not(target_env = "msvc"))]
use libc::mkstemp;
use libc::{close, free, getenv, remove, strcat, strcpy, strlen, strncmp, strrchr};
#[cfg(target_env = "msvc")]
extern "C" {
    #[link_name = "dpx_win32_mktemp_s"]
    fn mktemp_s(template: *mut libc::c_char, size: libc::size_t) -> libc::c_int;
}

pub type __ssize_t = i64;
pub type size_t = u64;
pub type ssize_t = __ssize_t;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
/* quasi-hack to get the primary input */
static mut verbose: i32 = 0i32;
#[no_mangle]
pub static mut keep_cache: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn dpx_file_set_verbose(mut level: i32) {
    verbose = level;
}
static mut _SBUF: [u8; 128] = [0; 128];
/*
 * SFNT type sigs:
 *  `true' (0x74727565): TrueType (Mac)
 *  `typ1' (0x74797031) (Mac): PostScript font housed in a sfnt wrapper
 *  0x00010000: TrueType (Win)/OpenType
 *  `OTTO': PostScript CFF font with OpenType wrapper
 *  `ttcf': TrueType Collection
 */
unsafe fn check_stream_is_truetype(handle: &mut InputHandleWrapper) -> bool {
    handle.seek(SeekFrom::Start(0)).unwrap();
    let n = handle.read(&mut _SBUF[..4]).unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if n != 4 {
        return false;
    }
    if &_SBUF[..4] == b"true" || _SBUF[..4] == [0, 1, 0, 0] {
        /* This doesn't help... */
        return true;
    }
    &_SBUF[..4] == b"ttcf"
}
/* "OpenType" is only for ".otf" here */
unsafe fn check_stream_is_opentype(handle: &mut InputHandleWrapper) -> bool {
    handle.seek(SeekFrom::Start(0)).unwrap();
    let n = handle.read(&mut _SBUF[..4]).unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if n != 4 {
        return false;
    }
    &_SBUF[..4] == b"OTTO"
}
unsafe fn check_stream_is_type1(handle: &mut InputHandleWrapper) -> bool {
    let p = &_SBUF;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let n = handle.read(&mut _SBUF[..21]).unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if n != 21 {
        return false;
    }
    if p[0] != 0x80 || p[1] < 0 || p[1] > 3 {
        return false;
    }
    if &p[6..20] == b"%!PS-"  || &p[6..17] == b"%!FontType1" {
        return true;
    }
    if &p[6..10] == b"%!PS" {
        /* This was #if-0'd out:
         * p[20] = '\0'; p += 6;
         * warn!("Ambiguous PostScript resource type: {}", (char *) p);
         */
        return true;
    }
    false
}
unsafe fn check_stream_is_dfont(handle: &mut InputHandleWrapper) -> bool {
    handle.seek(SeekFrom::Start(0)).unwrap();
    tt_get_unsigned_quad(handle);
    let pos = tt_get_unsigned_quad(handle) as u64;
    handle.seek(SeekFrom::Start(pos + 0x18)).unwrap();
    let n = tt_get_unsigned_pair(handle) as u64;
    handle.seek(SeekFrom::Start(pos + n)).unwrap();
    let n = tt_get_unsigned_pair(handle) as i32;
    for _ in 0..=n {
        if tt_get_unsigned_quad(handle) as u64 == 0x73666e74 {
            /* "sfnt" */
            return true;
        }
        tt_get_unsigned_quad(handle);
    }
    false
}
/* ensuresuffix() returns a copy of basename if sfx is "". */
unsafe fn ensuresuffix(mut basename: *const i8, mut sfx: *const i8) -> *mut i8 {
    let p = new((strlen(basename).wrapping_add(strlen(sfx)).wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    strcpy(p, basename);
    let q = strrchr(p, '.' as i32);
    if q.is_null() && *sfx.offset(0) as i32 != 0 {
        strcat(p, sfx);
    }
    p
}
/* tmp freed here */
/* Tectonic-enabled I/O alternatives */
#[no_mangle]
pub unsafe extern "C" fn dpx_tt_open(
    mut filename: *const i8,
    mut suffix: *const i8,
    mut format: TTInputFormat,
) -> Option<InputHandleWrapper> {
    let q = ensuresuffix(filename, suffix);
    let handle = ttstub_input_open(q, format, 0i32);
    free(q as *mut libc::c_void);
    handle
}
/* Search order:
 *   SFDFONTS (TDS 1.1)
 *   ttf2pk   (text file)
 *   ttf2tfm  (text file)
 *   dvipdfm  (text file)
 */
#[no_mangle]
pub unsafe extern "C" fn dpx_open_type1_file(mut filename: *const i8) -> Option<InputHandleWrapper> {
    match ttstub_input_open(filename, TTInputFormat::TYPE1, 0) {
        Some(mut handle) => {
            if !check_stream_is_type1(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        },
        None => None,
    }
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_truetype_file(mut filename: *const i8) -> Option<InputHandleWrapper> {
    match ttstub_input_open(filename, TTInputFormat::TRUETYPE, 0) {
        Some(mut handle) => {
            if !check_stream_is_truetype(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        },
        None => None,
    }
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_opentype_file(mut filename: *const i8) -> Option<InputHandleWrapper> {
    let q = ensuresuffix(filename, b".otf\x00" as *const u8 as *const i8);
    let handle = ttstub_input_open(q, TTInputFormat::OPENTYPE, 0i32);
    free(q as *mut libc::c_void);
    match handle {
        Some(mut handle) => {
            if !check_stream_is_opentype(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        },
        None => None,
    }
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_dfont_file(mut filename: *const i8) -> Option<InputHandleWrapper> {
    let q;
    let mut len: i32 = strlen(filename) as i32;
    if len > 6i32
        && strncmp(
            filename.offset(len as isize).offset(-6),
            b".dfont\x00" as *const u8 as *const i8,
            6,
        ) != 0
    {
        /* I've double-checked that we're accurately representing the original
         * code -- the above strncmp() is *not* missing a logical negation.
         */
        q = new(
            ((len + 6i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32,
        ) as *mut i8;
        strcpy(q, filename);
        strcat(q, b"/rsrc\x00" as *const u8 as *const i8);
    } else {
        q = xstrdup(filename)
    }
    let handle = ttstub_input_open(q, TTInputFormat::TRUETYPE, 0i32);
    free(q as *mut libc::c_void);
    match handle {
        Some(mut handle) => {
            if !check_stream_is_dfont(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        },
        None => None,
    }
}
unsafe fn dpx_get_tmpdir() -> *mut i8 {
    let mut _tmpd: *const i8 = 0 as *const i8;
    _tmpd = getenv(b"TMPDIR\x00" as *const u8 as *const i8);
    if _tmpd.is_null() {
        _tmpd = b"/tmp\x00" as *const u8 as *const i8
    }
    let ret = xstrdup(_tmpd);
    let mut i = strlen(ret) as u64;
    while i > 1i32 as u64 && *ret.offset(i.wrapping_sub(1i32 as u64) as isize) as i32 == '/' as i32
    {
        *ret.offset(i.wrapping_sub(1i32 as u64) as isize) = '\u{0}' as i32 as i8;
        i -= 1;
    }
    ret
}
#[no_mangle]
pub unsafe extern "C" fn dpx_create_temp_file() -> *mut i8 {
    #[cfg(not(target_env = "msvc"))]
    const TEMPLATE: &[u8] = b"/dvipdfmx.XXXXXX\x00";
    #[cfg(target_env = "msvc")]
    const TEMPLATE: &[u8] = b"\\dvipdfmx.XXXXXX\x00";
    let tmpdir = dpx_get_tmpdir();
    let n = strlen(tmpdir)
        .wrapping_add(strlen(TEMPLATE.as_ptr() as *const u8 as *const i8))
        .wrapping_add(1) as u64;
    let mut tmp =
        new((n as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
    strcpy(tmp, tmpdir);
    free(tmpdir as *mut libc::c_void);
    strcat(tmp, TEMPLATE.as_ptr() as *const u8 as *const i8);
    #[cfg(not(target_env = "msvc"))]
    {
        let mut _fd: i32 = mkstemp(tmp);
        if _fd != -1i32 {
            close(_fd);
        } else {
            tmp = mfree(tmp as *mut libc::c_void) as *mut i8
        }
    }
    #[cfg(target_env = "msvc")]
    {
        if mktemp_s(tmp, n as _) != 0 {
            tmp = mfree(tmp as *mut libc::c_void) as *mut i8;
        }
    }
    tmp
}
#[no_mangle]
pub unsafe extern "C" fn dpx_delete_old_cache(mut life: i32) {
    /* This used to delete files in tmpdir, but that code was ripped out since
     * it would have been annoying to port to Windows. */
    if life == -2i32 {
        keep_cache = -1i32
    };
}
#[no_mangle]
pub unsafe extern "C" fn dpx_delete_temp_file(mut tmp: *mut i8, mut force: i32) {
    if tmp.is_null() {
        return;
    }
    if force != 0 || keep_cache != 1i32 {
        remove(tmp);
    }
    free(tmp as *mut libc::c_void);
}
/* dpx_file_apply_filter() is used for converting unsupported graphics
 * format to one of the formats that dvipdfmx can natively handle.
 * 'input' is the filename of the original file and 'output' is actually
 * temporal files 'generated' by the above routine.
 * This should be system dependent. (MiKTeX may want something different)
 * Please modify as appropriate (see also pdfximage.c and dvipdfmx.c).
 */
#[no_mangle]
pub unsafe extern "C" fn dpx_file_apply_filter(
    mut _cmdtmpl: *const i8,
    mut _input: *const i8,
    mut _output: *const i8,
    mut _version: u8,
) -> i32 {
    /* Tectonic: defused */
    -1i32
}
