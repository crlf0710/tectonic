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

use std::ffi::CStr;
use std::io::{Seek, SeekFrom};
use std::ptr;
use tectonic_bridge::ttstub_input_close;

use crate::strstartswith;
use crate::warn;

use super::dpx_cmap::{
    CMap_add_bfchar, CMap_add_bfrange, CMap_add_cidchar, CMap_add_cidrange,
    CMap_add_codespacerange, CMap_add_notdefchar, CMap_add_notdefrange, CMap_cache_find,
    CMap_cache_get, CMap_is_valid, CMap_set_CIDSysInfo, CMap_set_name, CMap_set_type,
    CMap_set_usecmap, CMap_set_wmode,
};
use super::dpx_mem::{new, renew};
use super::dpx_pst::{pst_get_token, PstType};
use super::dpx_pst_obj::pst_obj;
use super::dpx_pst_obj::{
    pst_data_ptr, pst_getIV, pst_getSV, pst_length_of, pst_release_obj, pst_type_of,
};
use crate::bridge::{ttstub_input_get_size, ttstub_input_read};
use libc::{free, memcmp, memcpy, memmove, strcmp, strlen, strstr};

pub(crate) type __ssize_t = i64;
use crate::bridge::size_t;
use bridge::InputHandleWrapper;

use super::dpx_cid::CIDSysInfo;
/* Mapping types, MAP_IS_NAME is not supported. */
/* Lookup flags */
/* DEBUG */
/* Codespacerange */

use super::dpx_cmap::CMap;

pub(crate) type CID = u16;
#[repr(C)]
pub(crate) struct ifreader {
    pub(crate) cursor: *mut u8,
    pub(crate) endptr: *mut u8,
    pub(crate) buf: *mut u8,
    pub(crate) max: size_t,
    pub(crate) handle: InputHandleWrapper,
    pub(crate) unread: size_t,
}
static mut __verbose: i32 = 0i32;
unsafe fn ifreader_create(
    handle: InputHandleWrapper,
    size: size_t,
    bufsize: size_t,
) -> *mut ifreader {
    let reader =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ifreader>() as u64) as u32) as *mut ifreader;
    (*reader).buf = new((bufsize.wrapping_add(1) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    (*reader).max = bufsize;
    (*reader).handle = handle;
    (*reader).unread = size;
    (*reader).endptr = (*reader).buf;
    (*reader).cursor = (*reader).endptr;
    *(*reader).endptr = 0_u8;
    reader
}
unsafe fn ifreader_destroy(reader: *mut ifreader) {
    assert!(!reader.is_null());
    ttstub_input_close((*reader).handle.clone()); // TODO: use drop
    free((*reader).buf as *mut libc::c_void);
    free(reader as *mut libc::c_void);
}
unsafe fn ifreader_read(mut reader: *mut ifreader, size: size_t) -> size_t {
    let mut bytesread: size_t = 0i32 as size_t;
    assert!(!reader.is_null());
    let bytesrem = ((*reader).endptr as size_t).wrapping_sub((*reader).cursor as size_t);
    if size > (*reader).max {
        if __verbose != 0 {
            info!("\nExtending buffer ({} bytes)...\n", size,);
        }
        (*reader).buf = renew(
            (*reader).buf as *mut libc::c_void,
            (size.wrapping_add(1) as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                as u32,
        ) as *mut u8;
        (*reader).max = size
    }
    if (*reader).unread > 0 && bytesrem < size {
        bytesread = if (*reader).max.wrapping_sub(bytesrem) < (*reader).unread {
            (*reader).max.wrapping_sub(bytesrem)
        } else {
            (*reader).unread
        };
        memmove(
            (*reader).buf as *mut libc::c_void,
            (*reader).cursor as *const libc::c_void,
            bytesrem as _,
        );
        (*reader).cursor = (*reader).buf;
        (*reader).endptr = (*reader).buf.offset(bytesrem as isize);
        if ttstub_input_read(
            (*reader).handle.as_ptr(),
            (*reader).endptr as *mut i8,
            bytesread,
        ) as u64
            != bytesread as _
        {
            panic!("Reading file failed.");
        }
        (*reader).endptr = (*reader).endptr.offset(bytesread as isize);
        (*reader).unread =
            ((*reader).unread as u64).wrapping_sub(bytesread as _) as size_t as size_t;
        if __verbose != 0 {
            info!(
                "Reading more {} bytes ({} bytes remains in buffer)...\n",
                bytesread, bytesrem,
            );
        }
    }
    *(*reader).endptr = 0_u8;
    bytesread.wrapping_add(bytesrem)
}
unsafe fn check_next_token(input: *mut ifreader, key: *const i8) -> i32 {
    if ifreader_read(input, strlen(key) as _) == 0 {
        return -1i32;
    }
    let token = pst_get_token(&mut (*input).cursor, (*input).endptr);
    if token.is_null() {
        return -1i32;
    }
    let str = pst_getSV(token) as *mut i8;
    let cmp = if strcmp(str, key) != 0 { -1i32 } else { 0i32 };
    free(str as *mut libc::c_void);
    pst_release_obj(token);
    cmp
}
unsafe fn get_coderange(
    input: *mut ifreader,
    codeLo: *mut u8,
    codeHi: *mut u8,
    dim: *mut i32,
    maxlen: i32,
) -> i32 {
    let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
    if tok1.is_null() {
        return -1i32;
    }
    let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
    if tok2.is_null() {
        pst_release_obj(tok1);
        return -1i32;
    }
    if !(pst_type_of(tok1) == PstType::String) || !(pst_type_of(tok2) == PstType::String) {
        pst_release_obj(tok1);
        pst_release_obj(tok2);
        return -1i32;
    }
    let dim1 = pst_length_of(tok1);
    let dim2 = pst_length_of(tok2);
    if dim1 != dim2 || dim1 > maxlen {
        pst_release_obj(tok1);
        pst_release_obj(tok2);
        return -1i32;
    }
    memcpy(codeLo as *mut libc::c_void, pst_data_ptr(tok1), dim1 as _);
    memcpy(codeHi as *mut libc::c_void, pst_data_ptr(tok2), dim2 as _);
    pst_release_obj(tok1);
    pst_release_obj(tok2);
    *dim = dim1;
    0i32
}
unsafe fn do_codespacerange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    let mut codeLo: [u8; 127] = [0; 127];
    let mut codeHi: [u8; 127] = [0; 127];
    let mut dim: i32 = 0;
    loop {
        let fresh0 = count;
        count = count - 1;
        if !(fresh0 > 0i32) {
            break;
        }
        if get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        ) < 0i32
        {
            return -1i32;
        }
        CMap_add_codespacerange(
            cmap,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            dim as size_t,
        );
    }
    check_next_token(input, b"endcodespacerange\x00" as *const u8 as *const i8)
}
/*
 * bfrange
 *  <codeLo> <codeHi> [destCode1 destCode2 ...]
 */
unsafe fn handle_codearray(
    cmap: *mut CMap,
    input: *mut ifreader,
    codeLo: *mut u8,
    dim: i32,
    mut count: i32,
) -> i32 {
    if dim < 1i32 {
        panic!("Invalid code range.");
    }
    loop {
        let fresh1 = count;
        count = count - 1;
        if !(fresh1 > 0i32) {
            break;
        }
        let tok = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok.is_null() {
            return -1i32;
        } else {
            if pst_type_of(tok) == PstType::String {
                CMap_add_bfchar(
                    cmap,
                    codeLo,
                    dim as size_t,
                    pst_data_ptr(tok) as *mut u8,
                    pst_length_of(tok) as size_t,
                );
            } else if pst_type_of(tok) == PstType::Mark || !(pst_type_of(tok) == PstType::Name) {
                panic!("{}: Invalid CMap mapping record.", "CMap_parse:",);
            } else {
                panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
            }
        }
        pst_release_obj(tok);
        let ref mut fresh2 = *codeLo.offset((dim - 1i32) as isize);
        *fresh2 = (*fresh2 as i32 + 1i32) as u8
    }
    check_next_token(input, b"]\x00" as *const u8 as *const i8)
}
unsafe fn do_notdefrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    let mut tok: *mut pst_obj = ptr::null_mut();
    let mut codeLo: [u8; 127] = [0; 127];
    let mut codeHi: [u8; 127] = [0; 127];
    let mut dim: i32 = 0;
    loop {
        let fresh3 = count;
        count = count - 1;
        if !(fresh3 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return -1i32;
        }
        if get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        ) < 0i32
            || {
                tok = pst_get_token(&mut (*input).cursor, (*input).endptr);
                tok.is_null()
            }
        {
            return -1i32;
        }
        if pst_type_of(tok) == PstType::Integer {
            let dstCID = pst_getIV(tok);
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_notdefrange(
                    cmap,
                    codeLo.as_mut_ptr(),
                    codeHi.as_mut_ptr(),
                    dim as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok);
    }
    check_next_token(input, b"endnotdefrange\x00" as *const u8 as *const i8)
}
unsafe fn do_bfrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    let mut tok: *mut pst_obj = ptr::null_mut();
    let mut codeLo: [u8; 127] = [0; 127];
    let mut codeHi: [u8; 127] = [0; 127];
    let mut srcdim: i32 = 0;
    loop {
        let fresh4 = count;
        count = count - 1;
        if !(fresh4 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return -1i32;
        }
        if get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut srcdim,
            127i32,
        ) < 0i32
            || {
                tok = pst_get_token(&mut (*input).cursor, (*input).endptr);
                tok.is_null()
            }
        {
            return -1i32;
        }
        if pst_type_of(tok) == PstType::String {
            CMap_add_bfrange(
                cmap,
                codeLo.as_mut_ptr(),
                codeHi.as_mut_ptr(),
                srcdim as size_t,
                pst_data_ptr(tok) as *mut u8,
                pst_length_of(tok) as size_t,
            );
        } else if pst_type_of(tok) == PstType::Mark {
            if handle_codearray(
                cmap,
                input,
                codeLo.as_mut_ptr(),
                srcdim,
                codeHi[(srcdim - 1i32) as usize] as i32 - codeLo[(srcdim - 1i32) as usize] as i32
                    + 1i32,
            ) < 0i32
            {
                pst_release_obj(tok);
                return -1i32;
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok);
    }
    check_next_token(input, b"endbfrange\x00" as *const u8 as *const i8)
}
unsafe fn do_cidrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    let mut tok: *mut pst_obj = ptr::null_mut();
    let mut codeLo: [u8; 127] = [0; 127];
    let mut codeHi: [u8; 127] = [0; 127];
    let mut dim: i32 = 0;
    loop {
        let fresh5 = count;
        count = count - 1;
        if !(fresh5 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return -1i32;
        }
        if get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        ) < 0i32
            || {
                tok = pst_get_token(&mut (*input).cursor, (*input).endptr);
                tok.is_null()
            }
        {
            return -1i32;
        }
        if pst_type_of(tok) == PstType::Integer {
            let dstCID = pst_getIV(tok);
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_cidrange(
                    cmap,
                    codeLo.as_mut_ptr(),
                    codeHi.as_mut_ptr(),
                    dim as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok);
    }
    check_next_token(input, b"endcidrange\x00" as *const u8 as *const i8)
}
unsafe fn do_notdefchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    loop {
        let fresh6 = count;
        count = count - 1;
        if !(fresh6 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return -1i32;
        }
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_null() {
            return -1i32;
        }
        let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok2.is_null() {
            pst_release_obj(tok1);
            return -1i32;
        }
        if pst_type_of(tok1) == PstType::String && pst_type_of(tok2) == PstType::Integer {
            let dstCID = pst_getIV(tok2);
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_notdefchar(
                    cmap,
                    pst_data_ptr(tok1) as *const u8,
                    pst_length_of(tok1) as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok1);
        pst_release_obj(tok2);
    }
    check_next_token(input, b"endnotdefchar\x00" as *const u8 as *const i8)
}
unsafe fn do_bfchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    loop {
        let fresh7 = count;
        count = count - 1;
        if !(fresh7 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return -1i32;
        }
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_null() {
            return -1i32;
        }
        let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok2.is_null() {
            pst_release_obj(tok1);
            return -1i32;
        }
        /* We only support single CID font as descendant font, charName should not come here. */
        if pst_type_of(tok1) == PstType::String && pst_type_of(tok2) == PstType::String {
            CMap_add_bfchar(
                cmap,
                pst_data_ptr(tok1) as *mut u8,
                pst_length_of(tok1) as size_t,
                pst_data_ptr(tok2) as *mut u8,
                pst_length_of(tok2) as size_t,
            );
        } else if pst_type_of(tok2) == PstType::Name {
            panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok1);
        pst_release_obj(tok2);
    }
    check_next_token(input, b"endbfchar\x00" as *const u8 as *const i8)
}
unsafe fn do_cidchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> i32 {
    loop {
        let fresh8 = count;
        count = count - 1;
        if !(fresh8 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return -1i32;
        }
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_null() {
            return -1i32;
        }
        let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok2.is_null() {
            pst_release_obj(tok1);
            return -1i32;
        }
        if pst_type_of(tok1) == PstType::String && pst_type_of(tok2) == PstType::Integer {
            let dstCID = pst_getIV(tok2);
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_cidchar(
                    cmap,
                    pst_data_ptr(tok1) as *const u8,
                    pst_length_of(tok1) as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
        pst_release_obj(tok1);
        pst_release_obj(tok2);
    }
    check_next_token(input, b"endcidchar\x00" as *const u8 as *const i8)
}
unsafe fn do_cidsysteminfo(cmap: *mut CMap, input: *mut ifreader) -> i32 {
    let mut csi: CIDSysInfo = CIDSysInfo {
        registry: "".into(),
        ordering: "".into(),
        supplement: -1i32,
    };
    let mut simpledict: i32 = 0i32;
    let mut error: i32 = 0i32;
    ifreader_read(input, (127i32 * 2i32) as size_t);
    loop
    /*
     * Assuming /CIDSystemInfo 3 dict dup begin .... end def
     * or /CIDSystemInfo << ... >> def
     */
    {
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_null() {
            break;
        }
        if pst_type_of(tok1) == PstType::Mark {
            simpledict = 1i32;
            pst_release_obj(tok1);
            break;
        } else if pst_type_of(tok1) == PstType::Unknown
            && memcmp(
                pst_data_ptr(tok1),
                b"begin\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"begin\x00" as *const u8 as *const i8),
            ) == 0
        {
            simpledict = 0i32;
            pst_release_obj(tok1);
            break;
        } else {
            pst_release_obj(tok1);
            /* continue */
        }
    }
    let mut tok2 = ptr::null_mut();
    let mut tok1 = tok2;
    while error == 0 && {
        tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        !tok1.is_null()
    } {
        if pst_type_of(tok1) == PstType::Unknown
            && memcmp(
                pst_data_ptr(tok1),
                b">>\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b">>\x00" as *const u8 as *const i8),
            ) == 0
            && simpledict != 0
        {
            pst_release_obj(tok1);
            break;
        } else if pst_type_of(tok1) == PstType::Unknown
            && memcmp(
                pst_data_ptr(tok1),
                b"end\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"end\x00" as *const u8 as *const i8),
            ) == 0
            && simpledict == 0
        {
            pst_release_obj(tok1);
            break;
        } else {
            if pst_type_of(tok1) == PstType::Name
                && memcmp(
                    pst_data_ptr(tok1),
                    b"Registry\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Registry\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    !tok2.is_null()
                }
            {
                if !(pst_type_of(tok2) == PstType::String) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8) != 0
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.registry = CStr::from_ptr(pst_getSV(tok2) as *const i8)
                        .to_str()
                        .unwrap()
                        .to_owned()
                        .into();
                }
            } else if pst_type_of(tok1) == PstType::Name
                && memcmp(
                    pst_data_ptr(tok1),
                    b"Ordering\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Ordering\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    !tok2.is_null()
                }
            {
                if !(pst_type_of(tok2) == PstType::String) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8) != 0
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.ordering = CStr::from_ptr(pst_getSV(tok2) as *const i8)
                        .to_str()
                        .unwrap()
                        .to_owned()
                        .into();
                }
            } else if pst_type_of(tok1) == PstType::Name
                && memcmp(
                    pst_data_ptr(tok1),
                    b"Supplement\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Supplement\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    !tok2.is_null()
                }
            {
                if !(pst_type_of(tok2) == PstType::Integer) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8) != 0
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.supplement = pst_getIV(tok2)
                }
            }
            if !tok2.is_null() {
                pst_release_obj(tok2);
            }
            if !tok1.is_null() {
                pst_release_obj(tok1);
            }
            tok2 = ptr::null_mut();
            tok1 = tok2
        }
    }
    if error == 0 && check_next_token(input, b"def\x00" as *const u8 as *const i8) != 0 {
        error = -1i32
    }
    if error == 0 && !csi.registry.is_empty() && !csi.ordering.is_empty() && csi.supplement >= 0i32
    {
        CMap_set_CIDSysInfo(cmap, &mut csi);
    }
    error
}

pub(crate) unsafe fn CMap_parse_check_sig(handle: Option<&mut InputHandleWrapper>) -> i32 {
    let mut result: i32 = -1i32;
    let mut sig: [i8; 65] = [0; 65];
    if handle.is_none() {
        return -1i32;
    }
    let handle = handle.unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if ttstub_input_read(handle.as_ptr(), sig.as_mut_ptr(), 64i32 as size_t) != 64isize {
        result = -1i32
    } else {
        sig[64] = 0_i8;
        if strstartswith(sig.as_mut_ptr(), b"%!PS\x00" as *const u8 as *const i8).is_null() {
            result = -1i32
        } else if !strstr(
            sig.as_mut_ptr().offset(4),
            b"Resource-CMap\x00" as *const u8 as *const i8,
        )
        .is_null()
        {
            result = 0i32
        }
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    result
}

pub(crate) unsafe fn CMap_parse(cmap: *mut CMap, mut handle: InputHandleWrapper) -> i32 {
    let mut status: i32 = 0i32;
    let mut tmpint: i32 = -1i32;
    assert!(!cmap.is_null());
    let size = ttstub_input_get_size(&mut handle);
    let input = ifreader_create(handle, size, (4096i32 - 1i32) as size_t);
    while status >= 0i32 {
        let mut tok2 = ptr::null_mut();
        ifreader_read(input, (4096i32 / 2i32) as size_t);
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_null() {
            break;
        }
        if pst_type_of(tok1) == PstType::Name
            && memcmp(
                pst_data_ptr(tok1),
                b"CMapName\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CMapName\x00" as *const u8 as *const i8),
            ) == 0
        {
            tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
            if tok2.is_null()
                || !(pst_type_of(tok2) == PstType::Name || pst_type_of(tok2) == PstType::String)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8) < 0i32
            {
                status = -1i32
            } else {
                CMap_set_name(
                    cmap,
                    &CStr::from_ptr(pst_data_ptr(tok2) as *const i8).to_string_lossy(),
                );
            }
        } else if pst_type_of(tok1) == PstType::Name
            && memcmp(
                pst_data_ptr(tok1),
                b"CMapType\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CMapType\x00" as *const u8 as *const i8),
            ) == 0
        {
            tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
            if tok2.is_null()
                || !(pst_type_of(tok2) == PstType::Integer)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8) < 0i32
            {
                status = -1i32
            } else {
                CMap_set_type(cmap, pst_getIV(tok2));
            }
        } else if pst_type_of(tok1) == PstType::Name
            && memcmp(
                pst_data_ptr(tok1),
                b"WMode\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"WMode\x00" as *const u8 as *const i8),
            ) == 0
        {
            tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
            if tok2.is_null()
                || !(pst_type_of(tok2) == PstType::Integer)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8) < 0i32
            {
                status = -1i32
            } else {
                CMap_set_wmode(cmap, pst_getIV(tok2));
            }
        } else if pst_type_of(tok1) == PstType::Name
            && memcmp(
                pst_data_ptr(tok1),
                b"CIDSystemInfo\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CIDSystemInfo\x00" as *const u8 as *const i8),
            ) == 0
        {
            status = do_cidsysteminfo(cmap, input)
        } else if !(pst_type_of(tok1) == PstType::Name
            && memcmp(
                pst_data_ptr(tok1),
                b"Version\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"Version\x00" as *const u8 as *const i8),
            ) == 0
            || pst_type_of(tok1) == PstType::Name
                && memcmp(
                    pst_data_ptr(tok1),
                    b"UIDOffset\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"UIDOffset\x00" as *const u8 as *const i8),
                ) == 0
            || pst_type_of(tok1) == PstType::Name
                && memcmp(
                    pst_data_ptr(tok1),
                    b"XUID\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"XUID\x00" as *const u8 as *const i8),
                ) == 0)
        {
            if pst_type_of(tok1) == PstType::Name {
                /* Possibly usecmap comes next */
                tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                if !tok2.is_null()
                    && (pst_type_of(tok2) == PstType::Unknown
                        && memcmp(
                            pst_data_ptr(tok2),
                            b"usecmap\x00" as *const u8 as *const i8 as *const libc::c_void,
                            strlen(b"usecmap\x00" as *const u8 as *const i8),
                        ) == 0)
                {
                    let id = CMap_cache_find(
                        &CStr::from_ptr(pst_data_ptr(tok1) as *const i8).to_string_lossy(),
                    );
                    if id < 0i32 {
                        status = -1i32
                    } else {
                        let ucmap = CMap_cache_get(id);
                        CMap_set_usecmap(cmap, ucmap);
                    }
                }
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"begincodespacerange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincodespacerange\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_codespacerange(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"beginnotdefrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginnotdefrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_notdefrange(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"beginnotdefchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginnotdefchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_notdefchar(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"beginbfrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginbfrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_bfrange(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"beginbfchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginbfchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_bfchar(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"begincidrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincidrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_cidrange(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Unknown
                && memcmp(
                    pst_data_ptr(tok1),
                    b"begincidchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincidchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                status = do_cidchar(cmap, input, tmpint)
            } else if pst_type_of(tok1) == PstType::Integer {
                tmpint = pst_getIV(tok1)
            }
        }
        if !tok1.is_null() {
            pst_release_obj(tok1);
        }
        if !tok2.is_null() {
            pst_release_obj(tok2);
        }
    }
    ifreader_destroy(input);
    if status < 0i32 {
        -1i32
    } else {
        CMap_is_valid(cmap) as i32
    }
}
