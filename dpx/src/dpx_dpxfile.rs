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
    non_upper_case_globals
)]

use std::ffi::CString;
use std::io::{Read, Seek, SeekFrom};

use super::dpx_numbers::GetFromFile;
use crate::bridge::{ttstub_input_close, ttstub_input_open};
use libc::{free, remove};

pub(crate) type __ssize_t = i64;

use crate::bridge::TTInputFormat;

use bridge::InputHandleWrapper;
/* quasi-hack to get the primary input */

pub(crate) static mut keep_cache: i32 = 0i32;

static mut _SBUF: [u8; 128] = [0; 128];
/*
 * SFNT type sigs:
 *  `true' (0x74727565): TrueType (Mac)
 *  `typ1' (0x74797031) (Mac): PostScript font housed in a sfnt wrapper
 *  0x00010000: TrueType (Win)/OpenType
 *  `OTTO': PostScript CFF font with OpenType wrapper
 *  `ttcf': TrueType Collection
 */
unsafe fn check_stream_is_truetype<R: Read + Seek>(handle: &mut R) -> bool {
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
unsafe fn check_stream_is_opentype<R: Read + Seek>(handle: &mut R) -> bool {
    handle.seek(SeekFrom::Start(0)).unwrap();
    let n = handle.read(&mut _SBUF[..4]).unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if n != 4 {
        return false;
    }
    &_SBUF[..4] == b"OTTO"
}
unsafe fn check_stream_is_type1<R: Read + Seek>(handle: &mut R) -> bool {
    let p = &_SBUF;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let n = handle.read(&mut _SBUF[..21]).unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if n != 21 {
        return false;
    }
    if p[0] != 0x80 || p[1] >= 0x80 || p[1] > 3 {
        return false;
    }
    if &p[6..20] == b"%!PS-" || &p[6..17] == b"%!FontType1" {
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
unsafe fn check_stream_is_dfont<R: Read + Seek>(handle: &mut R) -> bool {
    handle.seek(SeekFrom::Start(0)).unwrap();
    u32::get(handle);
    let pos = u32::get(handle) as u64;
    handle.seek(SeekFrom::Start(pos + 0x18)).unwrap();
    let n = u16::get(handle) as u64;
    handle.seek(SeekFrom::Start(pos + n)).unwrap();
    let n = u16::get(handle) as i32;
    for _ in 0..=n {
        if u32::get(handle) as u64 == 0x73666e74 {
            /* "sfnt" */
            return true;
        }
        u32::get(handle);
    }
    false
}
/* ensuresuffix() returns a copy of basename if sfx is "". */
unsafe fn ensuresuffix(basename: &str, sfx: &str) -> String {
    let mut p = String::from(basename);
    if !p.contains('.') && !sfx.is_empty() {
        p += sfx;
    }
    p
}
/* tmp freed here */
/* Tectonic-enabled I/O alternatives */

pub(crate) unsafe fn dpx_tt_open(
    filename: &str,
    suffix: &str,
    format: TTInputFormat,
) -> Option<InputHandleWrapper> {
    let q = CString::new(ensuresuffix(filename, suffix)).unwrap();
    let handle = ttstub_input_open(q.as_ptr(), format, 0i32);
    handle
}
/* Search order:
 *   SFDFONTS (TDS 1.1)
 *   ttf2pk   (text file)
 *   ttf2tfm  (text file)
 *   dvipdfm  (text file)
 */

pub(crate) unsafe fn dpx_open_type1_file(filename: &str) -> Option<InputHandleWrapper> {
    let filename_ = CString::new(filename).unwrap();
    let filename = filename_.as_ptr();
    match ttstub_input_open(filename, TTInputFormat::TYPE1, 0) {
        Some(mut handle) => {
            if !check_stream_is_type1(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        }
        None => None,
    }
}

pub(crate) unsafe fn dpx_open_truetype_file(filename: &str) -> Option<InputHandleWrapper> {
    let filename_ = CString::new(filename).unwrap();
    let filename = filename_.as_ptr();
    match ttstub_input_open(filename, TTInputFormat::TRUETYPE, 0) {
        Some(mut handle) => {
            if !check_stream_is_truetype(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        }
        None => None,
    }
}

pub(crate) unsafe fn dpx_open_opentype_file(filename: &str) -> Option<InputHandleWrapper> {
    let q = CString::new(ensuresuffix(filename, ".otf")).unwrap();
    let handle = ttstub_input_open(q.as_ptr(), TTInputFormat::OPENTYPE, 0i32);
    match handle {
        Some(mut handle) => {
            if !check_stream_is_opentype(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        }
        None => None,
    }
}

pub(crate) unsafe fn dpx_open_dfont_file(filename: &str) -> Option<InputHandleWrapper> {
    let q;
    if filename.len() > 6 && !filename.ends_with(".dfont") {
        // FIXME: we might want to invert this
        /* I've double-checked that we're accurately representing the original
         * code -- the above strncmp() is *not* missing a logical negation.
         */
        q = filename.to_owned() + "/rsrc";
    } else {
        q = filename.to_owned();
    }
    let q_ = CString::new(q).unwrap();
    let q = q_.as_ptr();
    let handle = ttstub_input_open(q, TTInputFormat::TRUETYPE, 0i32);
    match handle {
        Some(mut handle) => {
            if !check_stream_is_dfont(&mut handle) {
                ttstub_input_close(handle);
                None
            } else {
                Some(handle)
            }
        }
        None => None,
    }
}

pub(crate) unsafe fn dpx_delete_old_cache(life: i32) {
    /* This used to delete files in tmpdir, but that code was ripped out since
     * it would have been annoying to port to Windows. */
    if life == -2i32 {
        keep_cache = -1i32
    };
}

pub(crate) unsafe fn dpx_delete_temp_file(tmp: *mut i8, force: i32) {
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

pub(crate) unsafe fn dpx_file_apply_filter(
    mut _cmdtmpl: *const i8,
    mut _input: *const i8,
    mut _output: *const i8,
    mut _version: u8,
) -> i32 {
    /* Tectonic: defused */
    -1i32
}
