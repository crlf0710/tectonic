/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2018 by Jin-Hwan Cho and Shunsaku Hirata,
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

use crate::bridge::ReadByte;
use std::io::{Read, Seek, SeekFrom};

use std::ptr;

/* Note: this is really just a random array used in other files. */
pub(crate) static mut work_buffer_u8: [u8; 1024] = [0; 1024];
/* Tectonic-enabled versions */
/* Modified versions of the above functions based on the Tectonic I/O system. */

pub(crate) unsafe fn tt_mfgets<R: Read + Seek>(
    buffer: *mut i8,
    length: usize,
    file: &mut R,
) -> *mut i8 {
    let mut ch = Some(0);
    let mut i = 0;
    while i < length - 1 {
        ch = file.read_byte();
        if let Some(ch) = ch.filter(|&c| c != b'\n' && c != b'\r') {
            *buffer.add(i) = ch as i8;
            i += 1;
        } else {
            break;
        }
    }
    *buffer.add(i) = '\u{0}' as i32 as i8;
    if ch.is_none() && i == 0 {
        return ptr::null_mut();
    }
    if ch == Some(b'\r') {
        if file.read_byte().filter(|&c| c != b'\n').is_some() {
            file.seek(SeekFrom::Current(-1)).unwrap();
        }
    }
    buffer
}

/* PDF reading starts around here */
/* As each lines may contain null-characters, so outptr here is NOT
 * null-terminated string. Returns -1 for when EOF is already reached, and -2
 * if buffer has no enough space.
 */
#[derive(Copy, Clone, Debug)]
pub(crate) enum MfReadErr {
    Eof,
    NotEnoughSpace,
}

use arrayvec::ArrayVec;
pub(crate) unsafe fn tt_mfreadln<R: Read + Seek>(
    size: usize,
    handle: &mut R,
) -> Result<ArrayVec<[u8; 1024]>, MfReadErr> {
    let mut c;
    let mut buf = ArrayVec::<[u8; 1024]>::new();
    loop {
        c = handle.read_byte();
        if let Some(c) = c.filter(|&c| c != b'\n' && c != b'\r') {
            if buf.len() >= size {
                return Err(MfReadErr::NotEnoughSpace);
            }
            buf.push(c as u8);
        } else {
            break;
        }
    }
    if c.is_none() && buf.is_empty() {
        return Err(MfReadErr::Eof);
    }
    if c == Some(b'\r') {
        if handle.read_byte().filter(|&c| c != b'\n').is_some() {
            handle.seek(SeekFrom::Current(-1)).unwrap();
        }
    }
    Ok(buf)
}
