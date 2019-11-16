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

use std::io::Write;

use crate::{ttstub_issue_warning, ttstub_output_open_stdout};
use bridge::vsnprintf;

use bridge::OutputHandleWrapper;
pub type message_type_t = _message_type;
pub type _message_type = u32;
pub const DPX_MESG_WARN: _message_type = 1;
pub const DPX_MESG_INFO: _message_type = 0;
pub static mut _last_message_type: message_type_t = DPX_MESG_INFO;
pub static mut _dpx_quietness: i32 = 0i32;

pub unsafe fn shut_up(mut quietness: i32) {
    _dpx_quietness = quietness;
}
pub static mut _dpx_message_handle: Option<OutputHandleWrapper> = None;

static mut _dpx_message_buf: [u8; 1024] = [0; 1024];
pub fn _dpx_ensure_output_handle() {
    if let Some(handle) = unsafe { ttstub_output_open_stdout() } {
        unsafe {
            _dpx_message_handle = Some(handle);
        }
    } else {
        panic!("xdvipdfmx cannot get output logging handle?!");
    }
}
unsafe fn _dpx_print_to_stdout(
    mut fmt: *const i8,
    mut argp: ::std::ffi::VaList,
    mut warn: bool,
) {
    let mut n = vsnprintf(
        _dpx_message_buf.as_mut_ptr() as *mut i8,
        ::std::mem::size_of::<[i8; 1024]>() as u64,
        fmt,
        argp.as_va_list(),
    );
    /* n is the number of bytes the vsnprintf() wanted to write -- it might be
     * bigger than sizeof(buf). */
    if n as u64 >= ::std::mem::size_of::<[i8; 1024]>() as u64 {
        n = (::std::mem::size_of::<[i8; 1024]>() as u64).wrapping_sub(1i32 as u64) as i32;
        _dpx_message_buf[n as usize] = 0
    }
    if warn {
        ttstub_issue_warning(
            b"%s\x00" as *const u8 as *const i8,
            _dpx_message_buf.as_mut_ptr() as *mut i8,
        );
    }
    _dpx_ensure_output_handle();
    _dpx_message_handle
        .as_mut()
        .unwrap()
        .write(&_dpx_message_buf[..n as usize])
        .unwrap();
}


pub unsafe extern "C" fn dpx_warning(mut fmt: *const i8, mut args: ...) {
    let mut argp: ::std::ffi::VaListImpl;
    if _dpx_quietness > 1i32 {
        return;
    }
    if _last_message_type as u32 == DPX_MESG_INFO as i32 as u32 {
        _dpx_ensure_output_handle();
        _dpx_message_handle.as_mut().unwrap().write(b"\n").unwrap();
    }
    _dpx_ensure_output_handle();
    _dpx_message_handle
        .as_mut()
        .unwrap()
        .write(b"warning: ")
        .unwrap();
    argp = args.clone();
    _dpx_print_to_stdout(fmt, argp.as_va_list(), true);
    _dpx_ensure_output_handle();
    _dpx_message_handle.as_mut().unwrap().write(b"\n").unwrap();
    _last_message_type = DPX_MESG_WARN;
}
