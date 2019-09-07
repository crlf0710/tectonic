#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]
extern crate libc;
extern "C" {
    #[no_mangle]
    fn strcat(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn getenv(__name: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn mkstemp(__template: *mut libc::c_char) -> libc::c_int;
    #[no_mangle]
    fn memcmp(_: *const libc::c_void, _: *const libc::c_void, _: libc::c_ulong) -> libc::c_int;
    #[no_mangle]
    fn strcpy(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn tt_get_unsigned_quad(handle: rust_input_handle_t) -> uint32_t;
    #[no_mangle]
    fn tt_get_unsigned_pair(handle: rust_input_handle_t) -> libc::c_ushort;
    #[no_mangle]
    fn ttstub_input_open(
        path: *const libc::c_char,
        format: tt_input_format_type,
        is_gz: libc::c_int,
    ) -> rust_input_handle_t;
    #[no_mangle]
    fn ttstub_input_seek(
        handle: rust_input_handle_t,
        offset: ssize_t,
        whence: libc::c_int,
    ) -> size_t;
    #[no_mangle]
    fn ttstub_input_read(
        handle: rust_input_handle_t,
        data: *mut libc::c_char,
        len: size_t,
    ) -> ssize_t;
    #[no_mangle]
    fn ttstub_input_close(handle: rust_input_handle_t) -> libc::c_int;
    /* tectonic/core-memory.h: basic dynamic memory helpers
       Copyright 2016-2018 the Tectonic Project
       Licensed under the MIT License.
    */
    #[no_mangle]
    fn xstrdup(s: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn remove(__filename: *const libc::c_char) -> libc::c_int;
    #[no_mangle]
    fn strncmp(_: *const libc::c_char, _: *const libc::c_char, _: libc::c_ulong) -> libc::c_int;
    #[no_mangle]
    fn strrchr(_: *const libc::c_char, _: libc::c_int) -> *mut libc::c_char;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    #[no_mangle]
    fn close(__fd: libc::c_int) -> libc::c_int;
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
    #[no_mangle]
    fn new(size: uint32_t) -> *mut libc::c_void;
}
pub type __uint32_t = libc::c_uint;
pub type __ssize_t = libc::c_long;
pub type uint32_t = __uint32_t;
pub type size_t = libc::c_ulong;
pub type ssize_t = __ssize_t;
/* The weird enum values are historical and could be rationalized. But it is
 * good to write them explicitly since they must be kept in sync with
 * `src/engines/mod.rs`.
 */
pub type tt_input_format_type = libc::c_uint;
pub const TTIF_TECTONIC_PRIMARY: tt_input_format_type = 59;
pub const TTIF_OPENTYPE: tt_input_format_type = 47;
pub const TTIF_SFD: tt_input_format_type = 46;
pub const TTIF_CMAP: tt_input_format_type = 45;
pub const TTIF_ENC: tt_input_format_type = 44;
pub const TTIF_MISCFONTS: tt_input_format_type = 41;
pub const TTIF_BINARY: tt_input_format_type = 40;
pub const TTIF_TRUETYPE: tt_input_format_type = 36;
pub const TTIF_VF: tt_input_format_type = 33;
pub const TTIF_TYPE1: tt_input_format_type = 32;
pub const TTIF_TEX_PS_HEADER: tt_input_format_type = 30;
pub const TTIF_TEX: tt_input_format_type = 26;
pub const TTIF_PICT: tt_input_format_type = 25;
pub const TTIF_OVF: tt_input_format_type = 23;
pub const TTIF_OFM: tt_input_format_type = 20;
pub const TTIF_FONTMAP: tt_input_format_type = 11;
pub const TTIF_FORMAT: tt_input_format_type = 10;
pub const TTIF_CNF: tt_input_format_type = 8;
pub const TTIF_BST: tt_input_format_type = 7;
pub const TTIF_BIB: tt_input_format_type = 6;
pub const TTIF_AFM: tt_input_format_type = 4;
pub const TTIF_TFM: tt_input_format_type = 3;
pub type rust_input_handle_t = *mut libc::c_void;
#[inline]
unsafe extern "C" fn mfree(mut ptr: *mut libc::c_void) -> *mut libc::c_void {
    free(ptr);
    return 0 as *mut libc::c_void;
}
/* quasi-hack to get the primary input */
/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.
   Copyright (C) 2007-2016 by Jin-Hwan Cho and Shunsaku Hirata,
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
static mut verbose: libc::c_int = 0i32;
#[no_mangle]
pub static mut keep_cache: libc::c_int = 0i32;
#[no_mangle]
pub unsafe extern "C" fn dpx_file_set_verbose(mut level: libc::c_int) {
    verbose = level;
}
static mut _sbuf: [libc::c_char; 128] = [0; 128];
/*
 * SFNT type sigs:
 *  `true' (0x74727565): TrueType (Mac)
 *  `typ1' (0x74797031) (Mac): PostScript font housed in a sfnt wrapper
 *  0x00010000: TrueType (Win)/OpenType
 *  `OTTO': PostScript CFF font with OpenType wrapper
 *  `ttcf': TrueType Collection
 */
unsafe extern "C" fn check_stream_is_truetype(mut handle: rust_input_handle_t) -> bool {
    let mut n: libc::c_int = 0;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    n = ttstub_input_read(handle, _sbuf.as_mut_ptr(), 4i32 as size_t) as libc::c_int;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    if n != 4i32 {
        return 0i32 != 0;
    }
    if memcmp(
        _sbuf.as_mut_ptr() as *const libc::c_void,
        b"true\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
        4i32 as libc::c_ulong,
    ) == 0
        || memcmp(
            _sbuf.as_mut_ptr() as *const libc::c_void,
            b"\x00\x01\x00\x00\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
            4i32 as libc::c_ulong,
        ) == 0
    {
        /* This doesn't help... */
        return 1i32 != 0;
    }
    if memcmp(
        _sbuf.as_mut_ptr() as *const libc::c_void,
        b"ttcf\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
        4i32 as libc::c_ulong,
    ) == 0
    {
        return 1i32 != 0;
    }
    return 0i32 != 0;
}
/* "OpenType" is only for ".otf" here */
unsafe extern "C" fn check_stream_is_opentype(mut handle: rust_input_handle_t) -> bool {
    let mut n: libc::c_int = 0;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    n = ttstub_input_read(handle, _sbuf.as_mut_ptr(), 4i32 as size_t) as libc::c_int;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    if n != 4i32 {
        return 0i32 != 0;
    }
    if memcmp(
        _sbuf.as_mut_ptr() as *const libc::c_void,
        b"OTTO\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
        4i32 as libc::c_ulong,
    ) == 0
    {
        return 1i32 != 0;
    }
    return 0i32 != 0;
}
unsafe extern "C" fn check_stream_is_type1(mut handle: rust_input_handle_t) -> bool {
    let mut p: *mut libc::c_char = _sbuf.as_mut_ptr();
    let mut n: libc::c_int = 0;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    n = ttstub_input_read(handle, p, 21i32 as size_t) as libc::c_int;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    if n != 21i32 {
        return 0i32 != 0;
    }
    if *p.offset(0) as libc::c_int != 0x80i32 as libc::c_char as libc::c_int
        || (*p.offset(1) as libc::c_int) < 0i32
        || *p.offset(1) as libc::c_int > 3i32
    {
        return 0i32 != 0;
    }
    if memcmp(
        p.offset(6) as *const libc::c_void,
        b"%!PS-AdobeFont\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
        14i32 as libc::c_ulong,
    ) == 0
        || memcmp(
            p.offset(6) as *const libc::c_void,
            b"%!FontType1\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
            11i32 as libc::c_ulong,
        ) == 0
    {
        return 1i32 != 0;
    }
    if memcmp(
        p.offset(6) as *const libc::c_void,
        b"%!PS\x00" as *const u8 as *const libc::c_char as *const libc::c_void,
        4i32 as libc::c_ulong,
    ) == 0
    {
        /* This was #if-0'd out:
         * p[20] = '\0'; p += 6;
         * dpx_warning("Ambiguous PostScript resource type: %s", (char *) p);
         */
        return 1i32 != 0;
    }
    return 0i32 != 0;
}
unsafe extern "C" fn check_stream_is_dfont(mut handle: rust_input_handle_t) -> bool {
    let mut i: libc::c_int = 0;
    let mut n: libc::c_int = 0;
    let mut pos: uint32_t = 0;
    ttstub_input_seek(handle, 0i32 as ssize_t, 0i32);
    tt_get_unsigned_quad(handle);
    pos = tt_get_unsigned_quad(handle);
    ttstub_input_seek(
        handle,
        pos.wrapping_add(0x18i32 as libc::c_uint) as ssize_t,
        0i32,
    );
    ttstub_input_seek(
        handle,
        pos.wrapping_add(tt_get_unsigned_pair(handle) as libc::c_uint) as ssize_t,
        0i32,
    );
    n = tt_get_unsigned_pair(handle) as libc::c_int;
    i = 0i32;
    while i <= n {
        if tt_get_unsigned_quad(handle) as libc::c_ulong == 0x73666e74 {
            /* "sfnt" */
            return 1i32 != 0;
        }
        tt_get_unsigned_quad(handle);
        i += 1
    }
    return 0i32 != 0;
}
/* ensuresuffix() returns a copy of basename if sfx is "". */
unsafe extern "C" fn ensuresuffix(
    mut basename: *const libc::c_char,
    mut sfx: *const libc::c_char,
) -> *mut libc::c_char {
    let mut q: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut p: *mut libc::c_char = 0 as *mut libc::c_char;
    p = new((strlen(basename)
        .wrapping_add(strlen(sfx))
        .wrapping_add(1i32 as libc::c_ulong) as uint32_t as libc::c_ulong)
        .wrapping_mul(::std::mem::size_of::<libc::c_char>() as libc::c_ulong)
        as uint32_t) as *mut libc::c_char;
    strcpy(p, basename);
    q = strrchr(p, '.' as i32);
    if q.is_null() && *sfx.offset(0) as libc::c_int != 0 {
        strcat(p, sfx);
    }
    return p;
}
/* tmp freed here */
/* Tectonic-enabled I/O alternatives */
#[no_mangle]
pub unsafe extern "C" fn dpx_tt_open(
    mut filename: *const libc::c_char,
    mut suffix: *const libc::c_char,
    mut format: tt_input_format_type,
) -> rust_input_handle_t {
    let mut q: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut handle: rust_input_handle_t = 0 as *mut libc::c_void;
    q = ensuresuffix(filename, suffix);
    handle = ttstub_input_open(q, format, 0i32);
    free(q as *mut libc::c_void);
    return handle;
}
/* Search order:
 *   SFDFONTS (TDS 1.1)
 *   ttf2pk   (text file)
 *   ttf2tfm  (text file)
 *   dvipdfm  (text file)
 */
#[no_mangle]
pub unsafe extern "C" fn dpx_open_type1_file(
    mut filename: *const libc::c_char,
) -> rust_input_handle_t {
    let mut handle: rust_input_handle_t = 0 as *mut libc::c_void;
    handle = ttstub_input_open(filename, TTIF_TYPE1, 0i32);
    if handle.is_null() {
        return 0 as *mut libc::c_void;
    }
    if !check_stream_is_type1(handle) {
        ttstub_input_close(handle);
        return 0 as *mut libc::c_void;
    }
    return handle;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_truetype_file(
    mut filename: *const libc::c_char,
) -> rust_input_handle_t {
    let mut handle: rust_input_handle_t = 0 as *mut libc::c_void;
    handle = ttstub_input_open(filename, TTIF_TRUETYPE, 0i32);
    if handle.is_null() {
        return 0 as *mut libc::c_void;
    }
    if !check_stream_is_truetype(handle) {
        ttstub_input_close(handle);
        return 0 as *mut libc::c_void;
    }
    return handle;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_opentype_file(
    mut filename: *const libc::c_char,
) -> rust_input_handle_t {
    let mut handle: rust_input_handle_t = 0 as *mut libc::c_void;
    let mut q: *mut libc::c_char = 0 as *mut libc::c_char;
    q = ensuresuffix(filename, b".otf\x00" as *const u8 as *const libc::c_char);
    handle = ttstub_input_open(q, TTIF_OPENTYPE, 0i32);
    free(q as *mut libc::c_void);
    if handle.is_null() {
        return 0 as *mut libc::c_void;
    }
    if !check_stream_is_opentype(handle) {
        ttstub_input_close(handle);
        return 0 as *mut libc::c_void;
    }
    return handle;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_open_dfont_file(
    mut filename: *const libc::c_char,
) -> rust_input_handle_t {
    let mut q: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut handle: rust_input_handle_t = 0 as *mut libc::c_void;
    let mut len: libc::c_int = strlen(filename) as libc::c_int;
    if len > 6i32
        && strncmp(
            filename.offset(len as isize).offset(-6),
            b".dfont\x00" as *const u8 as *const libc::c_char,
            6i32 as libc::c_ulong,
        ) != 0
    {
        /* I've double-checked that we're accurately representing the original
         * code -- the above strncmp() is *not* missing a logical negation.
         */
        q = new(((len + 6i32) as uint32_t as libc::c_ulong)
            .wrapping_mul(::std::mem::size_of::<libc::c_char>() as libc::c_ulong)
            as uint32_t) as *mut libc::c_char;
        strcpy(q, filename);
        strcat(q, b"/rsrc\x00" as *const u8 as *const libc::c_char);
    } else {
        q = xstrdup(filename)
    }
    handle = ttstub_input_open(q, TTIF_TRUETYPE, 0i32);
    free(q as *mut libc::c_void);
    if handle.is_null() {
        return 0 as *mut libc::c_void;
    }
    if !check_stream_is_dfont(handle) {
        ttstub_input_close(handle);
        return 0 as *mut libc::c_void;
    }
    return handle;
}
unsafe extern "C" fn dpx_get_tmpdir() -> *mut libc::c_char {
    let mut i: size_t = 0;
    let mut ret: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut _tmpd: *const libc::c_char = 0 as *const libc::c_char;
    _tmpd = getenv(b"TMPDIR\x00" as *const u8 as *const libc::c_char);
    if _tmpd.is_null() {
        _tmpd = b"/tmp\x00" as *const u8 as *const libc::c_char
    }
    ret = xstrdup(_tmpd);
    i = strlen(ret);
    while i > 1i32 as libc::c_ulong
        && *ret.offset(i.wrapping_sub(1i32 as libc::c_ulong) as isize) as libc::c_int == '/' as i32
    {
        *ret.offset(i.wrapping_sub(1i32 as libc::c_ulong) as isize) =
            '\u{0}' as i32 as libc::c_char;
        i = i.wrapping_sub(1)
    }
    return ret;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_create_temp_file() -> *mut libc::c_char {
    let mut tmpdir: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut n: size_t = 0;
    let mut tmp: *mut libc::c_char = 0 as *mut libc::c_char;
    tmpdir = dpx_get_tmpdir();
    n = strlen(tmpdir)
        .wrapping_add(strlen(
            b"/dvipdfmx.XXXXXX\x00" as *const u8 as *const libc::c_char,
        ))
        .wrapping_add(1i32 as libc::c_ulong);
    tmp = new((n as uint32_t as libc::c_ulong)
        .wrapping_mul(::std::mem::size_of::<libc::c_char>() as libc::c_ulong)
        as uint32_t) as *mut libc::c_char;
    strcpy(tmp, tmpdir);
    free(tmpdir as *mut libc::c_void);
    strcat(
        tmp,
        b"/dvipdfmx.XXXXXX\x00" as *const u8 as *const libc::c_char,
    );
    let mut _fd: libc::c_int = mkstemp(tmp);
    if _fd != -1i32 {
        close(_fd);
    } else {
        tmp = mfree(tmp as *mut libc::c_void) as *mut libc::c_char
    }
    return tmp;
}
#[no_mangle]
pub unsafe extern "C" fn dpx_delete_old_cache(mut life: libc::c_int) {
    /* This used to delete files in tmpdir, but that code was ripped out since
     * it would have been annoying to port to Windows. */
    if life == -2i32 {
        keep_cache = -1i32
    };
}
#[no_mangle]
pub unsafe extern "C" fn dpx_delete_temp_file(mut tmp: *mut libc::c_char, mut force: libc::c_int) {
    if tmp.is_null() {
        return;
    }
    if force != 0 || keep_cache != 1i32 {
        remove(tmp);
    }
    free(tmp as *mut libc::c_void);
}
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
/* dpx_file_apply_filter() is used for converting unsupported graphics
 * format to one of the formats that dvipdfmx can natively handle.
 * 'input' is the filename of the original file and 'output' is actually
 * temporal files 'generated' by the above routine.
 * This should be system dependent. (MiKTeX may want something different)
 * Please modify as appropriate (see also pdfximage.c and dvipdfmx.c).
 */
#[no_mangle]
pub unsafe extern "C" fn dpx_file_apply_filter(
    mut cmdtmpl: *const libc::c_char,
    mut input: *const libc::c_char,
    mut output: *const libc::c_char,
    mut version: libc::c_uchar,
) -> libc::c_int {
    /* Tectonic: defused */
    return -1i32;
}
