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

use crate::bridge::ttstub_input_getc;
use std::io::{Read, Seek, SeekFrom};

use std::ptr;

/* Note: this is really just a random array used in other files. */
pub(crate) static mut work_buffer: [i8; 1024] = [0; 1024];
pub(crate) static mut work_buffer_u8: [u8; 1024] = [0; 1024];
/* Tectonic-enabled versions */
/* Modified versions of the above functions based on the Tectonic I/O system. */

pub(crate) unsafe fn tt_mfgets<R: Read + Seek>(
    buffer: *mut i8,
    length: i32,
    file: &mut R,
) -> *mut i8 {
    let mut ch: i32 = 0i32;
    let mut i: i32 = 0i32;
    while i < length - 1i32
        && {
            ch = ttstub_input_getc(file);
            ch >= 0
        }
        && ch != '\n' as i32
        && ch != '\r' as i32
    {
        let fresh1 = i;
        i = i + 1;
        *buffer.offset(fresh1 as isize) = ch as i8
    }
    *buffer.offset(i as isize) = '\u{0}' as i32 as i8;
    if ch < 0i32 && i == 0i32 {
        return ptr::null_mut();
    }
    if ch == '\r' as i32
        && {
            ch = ttstub_input_getc(file);
            ch >= 0
        }
        && ch != '\n' as i32
    {
        file.seek(SeekFrom::Current(-1)).unwrap();
    }
    buffer
}
