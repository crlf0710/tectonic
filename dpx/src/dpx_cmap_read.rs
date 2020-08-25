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
use std::io::{Read, Seek, SeekFrom};
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
use crate::bridge::ttstub_input_get_size;
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
        let slice = std::slice::from_raw_parts_mut((*reader).endptr, bytesread as usize);
        (*reader)
            .handle
            .read_exact(slice)
            .expect("Reading file failed.");
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
unsafe fn check_next_token(input: *mut ifreader, key: *const i8) -> Result<(), ()> {
    if ifreader_read(input, strlen(key) as _) == 0 {
        return Err(());
    }
    let token = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
    let str = token.getSV().ok_or(())?;
    let cmp = if strcmp(str.as_ptr(), key) != 0 {
        Err(())
    } else {
        Ok(())
    };
    cmp
}
unsafe fn get_coderange(
    input: *mut ifreader,
    codeLo: *mut u8,
    codeHi: *mut u8,
    dim: *mut i32,
    maxlen: i32,
) -> Result<(), ()> {
    let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
    let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
    if !(tok1.typ() == PstType::String) || !(tok2.typ() == PstType::String) {
        return Err(());
    }
    let dim1 = tok1.length();
    let dim2 = tok2.length();
    if dim1 != dim2 || dim1 > maxlen {
        return Err(());
    }
    memcpy(codeLo as *mut libc::c_void, tok1.data_ptr(), dim1 as _);
    memcpy(codeHi as *mut libc::c_void, tok2.data_ptr(), dim2 as _);
    *dim = dim1;
    Ok(())
}
unsafe fn do_codespacerange(
    cmap: *mut CMap,
    input: *mut ifreader,
    mut count: i32,
) -> Result<(), ()> {
    let mut codeLo: [u8; 127] = [0; 127];
    let mut codeHi: [u8; 127] = [0; 127];
    let mut dim: i32 = 0;
    loop {
        let fresh0 = count;
        count = count - 1;
        if !(fresh0 > 0i32) {
            break;
        }
        get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        )?;
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
) -> Result<(), ()> {
    if dim < 1i32 {
        panic!("Invalid code range.");
    }
    loop {
        let fresh1 = count;
        count = count - 1;
        if !(fresh1 > 0i32) {
            break;
        }
        let mut tok = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok.typ() == PstType::String {
            CMap_add_bfchar(
                cmap,
                codeLo,
                dim as size_t,
                tok.data_mut_ptr() as *mut u8,
                tok.length() as size_t,
            );
        } else if tok.typ() == PstType::Mark || !(tok.typ() == PstType::Name) {
            panic!("{}: Invalid CMap mapping record.", "CMap_parse:",);
        } else {
            panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
        }
        let ref mut fresh2 = *codeLo.offset((dim - 1i32) as isize);
        *fresh2 = (*fresh2 as i32 + 1i32) as u8
    }
    check_next_token(input, b"]\x00" as *const u8 as *const i8)
}
unsafe fn do_notdefrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
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
            return Err(());
        }
        get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        )?;
        let tok = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok.typ() == PstType::Integer {
            let dstCID = tok.getIV();
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
    }
    check_next_token(input, b"endnotdefrange\x00" as *const u8 as *const i8)
}
unsafe fn do_bfrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
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
            return Err(());
        }
        get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut srcdim,
            127i32,
        )?;
        let mut tok = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok.typ() == PstType::String {
            CMap_add_bfrange(
                cmap,
                codeLo.as_mut_ptr(),
                codeHi.as_mut_ptr(),
                srcdim as size_t,
                tok.data_mut_ptr() as *mut u8,
                tok.length() as size_t,
            );
        } else if tok.typ() == PstType::Mark {
            handle_codearray(
                cmap,
                input,
                codeLo.as_mut_ptr(),
                srcdim,
                codeHi[(srcdim - 1i32) as usize] as i32 - codeLo[(srcdim - 1i32) as usize] as i32
                    + 1i32,
            )?;
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, b"endbfrange\x00" as *const u8 as *const i8)
}
unsafe fn do_cidrange(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
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
            return Err(());
        }
        get_coderange(
            input,
            codeLo.as_mut_ptr(),
            codeHi.as_mut_ptr(),
            &mut dim,
            127i32,
        )?;
        let tok = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok.typ() == PstType::Integer {
            let dstCID = tok.getIV();
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
    }
    check_next_token(input, b"endcidrange\x00" as *const u8 as *const i8)
}
unsafe fn do_notdefchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh6 = count;
        count = count - 1;
        if !(fresh6 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok1.typ() == PstType::String && tok2.typ() == PstType::Integer {
            let dstCID = tok2.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_notdefchar(
                    cmap,
                    tok1.data_ptr() as *const u8,
                    tok1.length() as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, b"endnotdefchar\x00" as *const u8 as *const i8)
}
unsafe fn do_bfchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh7 = count;
        count = count - 1;
        if !(fresh7 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let mut tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        let mut tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        /* We only support single CID font as descendant font, charName should not come here. */
        if tok1.typ() == PstType::String && tok2.typ() == PstType::String {
            CMap_add_bfchar(
                cmap,
                tok1.data_mut_ptr() as *mut u8,
                tok1.length() as size_t,
                tok2.data_mut_ptr() as *mut u8,
                tok2.length() as size_t,
            );
        } else if tok2.typ() == PstType::Name {
            panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, b"endbfchar\x00" as *const u8 as *const i8)
}
unsafe fn do_cidchar(cmap: *mut CMap, input: *mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh8 = count;
        count = count - 1;
        if !(fresh8 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
        if tok1.typ() == PstType::String && tok2.typ() == PstType::Integer {
            let dstCID = tok2.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_cidchar(
                    cmap,
                    tok1.data_ptr() as *const u8,
                    tok1.length() as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
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
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Mark {
            simpledict = 1i32;
            break;
        } else if tok1.typ() == PstType::Unknown
            && memcmp(
                tok1.data_ptr(),
                b"begin\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"begin\x00" as *const u8 as *const i8),
            ) == 0
        {
            simpledict = 0i32;
            break;
        }
    }
    while error == 0 {
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Unknown
            && memcmp(
                tok1.data_ptr(),
                b">>\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b">>\x00" as *const u8 as *const i8),
            ) == 0
            && simpledict != 0
        {
            break;
        } else if tok1.typ() == PstType::Unknown
            && memcmp(
                tok1.data_ptr(),
                b"end\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"end\x00" as *const u8 as *const i8),
            ) == 0
            && simpledict == 0
        {
            break;
        } else {
            let mut tok2 = None;
            if tok1.typ() == PstType::Name
                && memcmp(
                    tok1.data_ptr(),
                    b"Registry\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Registry\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::String) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.registry = tok2.getSV().unwrap().to_str().unwrap().to_owned().into();
                }
            } else if tok1.typ() == PstType::Name
                && memcmp(
                    tok1.data_ptr(),
                    b"Ordering\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Ordering\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::String) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.ordering = tok2.getSV().unwrap().to_str().unwrap().to_owned().into();
                }
            } else if tok1.typ() == PstType::Name
                && memcmp(
                    tok1.data_ptr(),
                    b"Supplement\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"Supplement\x00" as *const u8 as *const i8),
                ) == 0
                && {
                    tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::Integer) {
                    error = -1i32
                } else if simpledict == 0
                    && check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
                {
                    error = -1i32
                }
                if error == 0 {
                    csi.supplement = tok2.getIV()
                }
            }
        }
    }
    if error == 0 && check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err() {
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
    let mut sig: [u8; 65] = [0; 65];
    if handle.is_none() {
        return -1i32;
    }
    let handle = handle.unwrap();
    handle.seek(SeekFrom::Start(0)).unwrap();
    if handle.read_exact(&mut sig[..64]).is_err() {
        result = -1i32
    } else {
        sig[64] = 0;
        if strstartswith(
            sig.as_ptr() as *const i8,
            b"%!PS\x00" as *const u8 as *const i8,
        )
        .is_null()
        {
            result = -1i32
        } else if !strstr(
            sig.as_ptr().offset(4) as *const i8,
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

pub(crate) unsafe fn CMap_parse(
    cmap: *mut CMap,
    mut handle: InputHandleWrapper,
) -> Result<i32, ()> {
    let mut status: i32 = 0i32;
    let mut tmpint: i32 = -1i32;
    assert!(!cmap.is_null());
    let size = ttstub_input_get_size(&mut handle);
    let input = ifreader_create(handle, size, (4096i32 - 1i32) as size_t);
    while status >= 0i32 {
        ifreader_read(input, (4096i32 / 2i32) as size_t);
        let tok1 = pst_get_token(&mut (*input).cursor, (*input).endptr);
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Name
            && memcmp(
                tok1.data_ptr(),
                b"CMapName\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CMapName\x00" as *const u8 as *const i8),
            ) == 0
        {
            let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Name || tok2.typ() == PstType::String)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
            {
                status = -1i32
            } else {
                CMap_set_name(
                    cmap,
                    &CStr::from_ptr(tok2.data_ptr() as *const i8).to_string_lossy(),
                );
            }
        } else if tok1.typ() == PstType::Name
            && memcmp(
                tok1.data_ptr(),
                b"CMapType\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CMapType\x00" as *const u8 as *const i8),
            ) == 0
        {
            let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Integer)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
            {
                status = -1i32
            } else {
                CMap_set_type(cmap, tok2.getIV());
            }
        } else if tok1.typ() == PstType::Name
            && memcmp(
                tok1.data_ptr(),
                b"WMode\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"WMode\x00" as *const u8 as *const i8),
            ) == 0
        {
            let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Integer)
                || check_next_token(input, b"def\x00" as *const u8 as *const i8).is_err()
            {
                status = -1i32
            } else {
                CMap_set_wmode(cmap, tok2.getIV());
            }
        } else if tok1.typ() == PstType::Name
            && memcmp(
                tok1.data_ptr(),
                b"CIDSystemInfo\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"CIDSystemInfo\x00" as *const u8 as *const i8),
            ) == 0
        {
            status = do_cidsysteminfo(cmap, input)
        } else if !(tok1.typ() == PstType::Name
            && memcmp(
                tok1.data_ptr(),
                b"Version\x00" as *const u8 as *const i8 as *const libc::c_void,
                strlen(b"Version\x00" as *const u8 as *const i8),
            ) == 0
            || tok1.typ() == PstType::Name
                && memcmp(
                    tok1.data_ptr(),
                    b"UIDOffset\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"UIDOffset\x00" as *const u8 as *const i8),
                ) == 0
            || tok1.typ() == PstType::Name
                && memcmp(
                    tok1.data_ptr(),
                    b"XUID\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"XUID\x00" as *const u8 as *const i8),
                ) == 0)
        {
            if tok1.typ() == PstType::Name {
                /* Possibly usecmap comes next */
                let tok2 = pst_get_token(&mut (*input).cursor, (*input).endptr).unwrap();
                if tok2.typ() == PstType::Unknown
                    && memcmp(
                        tok2.data_ptr(),
                        b"usecmap\x00" as *const u8 as *const i8 as *const libc::c_void,
                        strlen(b"usecmap\x00" as *const u8 as *const i8),
                    ) == 0
                {
                    let id = CMap_cache_find(
                        &CStr::from_ptr(tok1.data_ptr() as *const i8).to_string_lossy(),
                    );
                    if id < 0i32 {
                        status = -1i32
                    } else {
                        let ucmap = CMap_cache_get(id);
                        CMap_set_usecmap(cmap, ucmap);
                    }
                }
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"begincodespacerange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincodespacerange\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_codespacerange(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"beginnotdefrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginnotdefrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_notdefrange(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"beginnotdefchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginnotdefchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_notdefchar(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"beginbfrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginbfrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_bfrange(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"beginbfchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"beginbfchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_bfchar(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"begincidrange\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincidrange\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_cidrange(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && memcmp(
                    tok1.data_ptr(),
                    b"begincidchar\x00" as *const u8 as *const i8 as *const libc::c_void,
                    strlen(b"begincidchar\x00" as *const u8 as *const i8),
                ) == 0
            {
                do_cidchar(cmap, input, tmpint)?;
            } else if tok1.typ() == PstType::Integer {
                tmpint = tok1.getIV()
            }
        }
    }
    ifreader_destroy(input);
    if status < 0i32 {
        Err(())
    } else {
        Ok(CMap_is_valid(cmap) as i32)
    }
}
