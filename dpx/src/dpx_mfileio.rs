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
         unused_mut)]

use crate::{ttstub_input_getc, ttstub_input_ungetc};

use std::ptr;

use libc::{fseek, ftell, rewind, FILE};
pub type __off_t = i64;
pub type __off64_t = i64;
use bridge::InputHandleWrapper;
unsafe fn os_error() {
    panic!("io:  An OS command failed that should not have.\n");
}
#[no_mangle]
pub unsafe extern "C" fn seek_relative(mut file: *mut FILE, mut pos: i32) {
    if fseek(file, pos as _, 1i32) != 0 {
        os_error();
    };
}
unsafe fn seek_end(mut file: *mut FILE) {
    if fseek(file, 0, 2i32) != 0 {
        os_error();
    };
}
unsafe fn tell_position(mut file: *mut FILE) -> i32 {
    let mut size = ftell(file);
    if size < 0 {
        os_error();
    }
    if size as i64 > 0x7fffffffi32 as i64 {
        panic!("ftell: file size {} exceeds 0x7fffffff.\n", size);
    }
    size as i32
}
#[no_mangle]
pub unsafe extern "C" fn file_size(mut file: *mut FILE) -> i32 {
    seek_end(file);
    let mut size = tell_position(file);
    rewind(file);
    size
}
/* Note: this is really just a random array used in other files. */
#[no_mangle]
pub static mut work_buffer: [i8; 1024] = [0; 1024];
pub static mut work_buffer_u8: [u8; 1024] = [0; 1024];
/* Tectonic-enabled versions */
/* Modified versions of the above functions based on the Tectonic I/O system. */
#[no_mangle]
pub unsafe extern "C" fn tt_mfgets(
    mut buffer: *mut i8,
    mut length: i32,
    file: &mut InputHandleWrapper,
) -> *mut i8 {
    let mut ch: i32 = 0i32;
    let mut i: i32 = 0i32;
    while i < length - 1i32
        && {
            ch = ttstub_input_getc(file);
            ch >= 0i32
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
            ch >= 0i32
        }
        && ch != '\n' as i32
    {
        ttstub_input_ungetc(file, ch);
    }
    buffer
}
