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

use crate::ttstub_output_open_stdout;

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
