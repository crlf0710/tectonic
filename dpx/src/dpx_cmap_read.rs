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

use std::io::{Read, Seek, SeekFrom};

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
use libc::{free, memmove, strstr};

pub(crate) type __ssize_t = i64;
use crate::bridge::size_t;
use bridge::DroppableInputHandleWrapper;

use super::dpx_cid::CIDSysInfo;
/* Mapping types, MAP_IS_NAME is not supported. */
/* Lookup flags */
/* DEBUG */
/* Codespacerange */

use super::dpx_cmap::CMap;
static mut __verbose: i32 = 0i32;

pub(crate) type CID = u16;
#[repr(C)]
pub(crate) struct ifreader {
    pub(crate) cursor: *mut u8,
    pub(crate) endptr: *mut u8,
    pub(crate) buf: *mut u8,
    pub(crate) max: size_t,
    pub(crate) handle: DroppableInputHandleWrapper,
    pub(crate) unread: size_t,
}
impl ifreader {
    unsafe fn new(handle: DroppableInputHandleWrapper, size: size_t, bufsize: size_t) -> Self {
        let buf = new((bufsize.wrapping_add(1) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        let reader = Self {
            buf,
            max: bufsize,
            handle,
            unread: size,
            endptr: buf,
            cursor: buf,
        };
        *reader.endptr = 0;
        reader
    }
}
impl Drop for ifreader {
    fn drop(&mut self) {
        unsafe {
            free(self.buf as *mut libc::c_void);
        }
    }
}
unsafe fn ifreader_read(reader: &mut ifreader, size: size_t) -> size_t {
    let mut bytesread: size_t = 0i32 as size_t;
    let bytesrem = (reader.endptr as size_t).wrapping_sub(reader.cursor as size_t);
    if size > reader.max {
        if __verbose != 0 {
            info!("\nExtending buffer ({} bytes)...\n", size,);
        }
        reader.buf = renew(
            reader.buf as *mut libc::c_void,
            (size.wrapping_add(1) as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                as u32,
        ) as *mut u8;
        reader.max = size
    }
    if reader.unread > 0 && bytesrem < size {
        bytesread = if reader.max.wrapping_sub(bytesrem) < reader.unread {
            reader.max.wrapping_sub(bytesrem)
        } else {
            reader.unread
        };
        memmove(
            reader.buf as *mut libc::c_void,
            reader.cursor as *const libc::c_void,
            bytesrem as _,
        );
        reader.cursor = reader.buf;
        reader.endptr = reader.buf.offset(bytesrem as isize);
        let slice = std::slice::from_raw_parts_mut(reader.endptr, bytesread as usize);
        reader
            .handle
            .read_exact(slice)
            .expect("Reading file failed.");
        reader.endptr = reader.endptr.offset(bytesread as isize);
        reader.unread = (reader.unread as u64).wrapping_sub(bytesread as _) as size_t as size_t;
        if __verbose != 0 {
            info!(
                "Reading more {} bytes ({} bytes remains in buffer)...\n",
                bytesread, bytesrem,
            );
        }
    }
    *reader.endptr = 0_u8;
    bytesread.wrapping_add(bytesrem)
}
unsafe fn check_next_token(input: &mut ifreader, key: &str) -> Result<(), ()> {
    if ifreader_read(input, key.len() as _) == 0 {
        return Err(());
    }
    let token = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
    if token.getSV().ok_or(())? != key {
        Err(())
    } else {
        Ok(())
    }
}
unsafe fn get_coderange(
    input: &mut ifreader,
    maxlen: usize,
) -> Result<(Vec<u8>, Vec<u8>, usize), ()> {
    let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
    let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
    if !(tok1.typ() == PstType::String) || !(tok2.typ() == PstType::String) {
        return Err(());
    }
    let dim1 = tok1.as_string().0.len();
    let dim2 = tok2.as_string().0.len();
    if dim1 != dim2 || dim1 > maxlen {
        return Err(());
    }
    let codeLo = tok1.as_string().0.clone().into_bytes();
    let codeHi = tok2.as_string().0.clone().into_bytes();
    Ok((codeLo, codeHi, dim1))
}
unsafe fn do_codespacerange(
    cmap: *mut CMap,
    input: &mut ifreader,
    mut count: i32,
) -> Result<(), ()> {
    loop {
        let fresh0 = count;
        count = count - 1;
        if !(fresh0 > 0i32) {
            break;
        }
        let (codeLo, codeHi, dim) = get_coderange(input, 127)?;
        CMap_add_codespacerange(cmap, codeLo.as_ptr(), codeHi.as_ptr(), dim as size_t);
    }
    check_next_token(input, "endcodespacerange")
}
/*
 * bfrange
 *  <codeLo> <codeHi> [destCode1 destCode2 ...]
 */
unsafe fn handle_codearray(
    cmap: *mut CMap,
    input: &mut ifreader,
    codeLo: &mut Vec<u8>,
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
        let tok = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok.typ() == PstType::String {
            let data = tok.data();
            CMap_add_bfchar(
                cmap,
                codeLo.as_ptr(),
                dim as size_t,
                data.as_ptr(),
                data.len() as size_t,
            );
        } else if tok.typ() == PstType::Mark || !(tok.typ() == PstType::Name) {
            panic!("{}: Invalid CMap mapping record.", "CMap_parse:",);
        } else {
            panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
        }
        codeLo[(dim - 1) as usize] += 1;
    }
    check_next_token(input, "]")
}
unsafe fn do_notdefrange(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh3 = count;
        count = count - 1;
        if !(fresh3 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return Err(());
        }
        let (codeLo, codeHi, dim) = get_coderange(input, 127)?;
        let tok = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok.typ() == PstType::Integer {
            let dstCID = tok.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_notdefrange(
                    cmap,
                    codeLo.as_ptr(),
                    codeHi.as_ptr(),
                    dim as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endnotdefrange")
}
unsafe fn do_bfrange(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh4 = count;
        count = count - 1;
        if !(fresh4 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return Err(());
        }
        let (mut codeLo, codeHi, srcdim) = get_coderange(input, 127)?;
        let tok = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok.typ() == PstType::String {
            let data = tok.data();
            CMap_add_bfrange(
                cmap,
                codeLo.as_ptr(),
                codeHi.as_ptr(),
                srcdim as size_t,
                data.as_ptr(),
                data.len() as size_t,
            );
        } else if tok.typ() == PstType::Mark {
            let last_lo = codeLo[(srcdim - 1) as usize];
            handle_codearray(
                cmap,
                input,
                &mut codeLo,
                srcdim as _,
                codeHi[(srcdim - 1) as usize] as i32 - last_lo as i32 + 1,
            )?;
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endbfrange")
}
unsafe fn do_cidrange(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh5 = count;
        count = count - 1;
        if !(fresh5 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 3i32) as size_t) == 0 {
            return Err(());
        }
        let (codeLo, codeHi, dim) = get_coderange(input, 127)?;
        let tok = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok.typ() == PstType::Integer {
            let dstCID = tok.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                CMap_add_cidrange(
                    cmap,
                    codeLo.as_ptr(),
                    codeHi.as_ptr(),
                    dim as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endcidrange")
}
unsafe fn do_notdefchar(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh6 = count;
        count = count - 1;
        if !(fresh6 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok1.typ() == PstType::String && tok2.typ() == PstType::Integer {
            let dstCID = tok2.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                let data = tok1.data();
                CMap_add_notdefchar(
                    cmap,
                    data.as_ptr(),
                    data.len() as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endnotdefchar")
}
unsafe fn do_bfchar(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh7 = count;
        count = count - 1;
        if !(fresh7 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        /* We only support single CID font as descendant font, charName should not come here. */
        if tok1.typ() == PstType::String && tok2.typ() == PstType::String {
            let data1 = tok1.data();
            let data2 = tok2.data();
            CMap_add_bfchar(
                cmap,
                data1.as_ptr(),
                data1.len() as size_t,
                data2.as_ptr(),
                data2.len() as size_t,
            );
        } else if tok2.typ() == PstType::Name {
            panic!("{}: Mapping to charName not supported.", "CMap_parse:",);
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endbfchar")
}
unsafe fn do_cidchar(cmap: *mut CMap, input: &mut ifreader, mut count: i32) -> Result<(), ()> {
    loop {
        let fresh8 = count;
        count = count - 1;
        if !(fresh8 > 0i32) {
            break;
        }
        if ifreader_read(input, (127i32 * 2i32) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if tok1.typ() == PstType::String && tok2.typ() == PstType::Integer {
            let dstCID = tok2.getIV();
            if dstCID >= 0i32 && dstCID <= 65535i32 {
                let data = tok1.data();
                CMap_add_cidchar(
                    cmap,
                    data.as_ptr(),
                    data.len() as size_t,
                    dstCID as CID,
                );
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endcidchar")
}
unsafe fn do_cidsysteminfo(cmap: *mut CMap, input: &mut ifreader) -> i32 {
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
        let tok1 = pst_get_token(&mut input.cursor, input.endptr);
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Mark {
            simpledict = 1i32;
            break;
        } else if tok1.typ() == PstType::Unknown
            && tok1.as_name().0.as_bytes().starts_with(b"begin")
        {
            simpledict = 0i32;
            break;
        }
    }
    while error == 0 {
        let tok1 = pst_get_token(&mut input.cursor, input.endptr);
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Unknown
            && tok1.as_name().0.as_bytes().starts_with(b">>")
            && simpledict != 0
        {
            break;
        } else if tok1.typ() == PstType::Unknown
            && tok1.as_name().0.as_bytes().starts_with(b"end")
            && simpledict == 0
        {
            break;
        } else {
            let mut tok2 = None;
            if tok1.typ() == PstType::Name
                && tok1.as_name().0.as_bytes().starts_with(b"Registry")
                && {
                    tok2 = pst_get_token(&mut input.cursor, input.endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::String) {
                    error = -1i32
                } else if simpledict == 0 && check_next_token(input, "def").is_err() {
                    error = -1i32
                }
                if error == 0 {
                    csi.registry = tok2.getSV().unwrap().into();
                }
            } else if tok1.typ() == PstType::Name
                && tok1.as_name().0.as_bytes().starts_with(b"Ordering")
                && {
                    tok2 = pst_get_token(&mut input.cursor, input.endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::String) {
                    error = -1i32
                } else if simpledict == 0 && check_next_token(input, "def").is_err() {
                    error = -1i32
                }
                if error == 0 {
                    csi.ordering = tok2.getSV().unwrap().into();
                }
            } else if tok1.typ() == PstType::Name
                && tok1.as_name().0.as_bytes().starts_with(b"Supplement")
                && {
                    tok2 = pst_get_token(&mut input.cursor, input.endptr);
                    tok2.is_some()
                }
            {
                let tok2 = tok2.take().unwrap();
                if !(tok2.typ() == PstType::Integer) {
                    error = -1i32
                } else if simpledict == 0 && check_next_token(input, "def").is_err() {
                    error = -1i32
                }
                if error == 0 {
                    csi.supplement = tok2.getIV()
                }
            }
        }
    }
    if error == 0 && check_next_token(input, "def").is_err() {
        error = -1i32
    }
    if error == 0 && !csi.registry.is_empty() && !csi.ordering.is_empty() && csi.supplement >= 0i32
    {
        CMap_set_CIDSysInfo(cmap, &mut csi);
    }
    error
}

pub(crate) unsafe fn CMap_parse_check_sig<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut result: i32 = -1i32;
    let mut sig: [u8; 65] = [0; 65];
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
    mut handle: DroppableInputHandleWrapper,
) -> Result<i32, ()> {
    let mut status: i32 = 0i32;
    let mut tmpint: i32 = -1i32;
    assert!(!cmap.is_null());
    let size = ttstub_input_get_size(&mut handle);
    let mut input = ifreader::new(handle, size, (4096i32 - 1i32) as size_t);
    while status >= 0i32 {
        ifreader_read(&mut input, (4096i32 / 2i32) as size_t);
        let tok1 = pst_get_token(&mut input.cursor, input.endptr);
        if tok1.is_none() {
            break;
        }
        let tok1 = tok1.unwrap();
        if tok1.typ() == PstType::Name
            && tok1.as_name().0.as_bytes().starts_with(b"CMapName")
        {
            let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Name || tok2.typ() == PstType::String)
                || check_next_token(&mut input, "def").is_err()
            {
                status = -1i32
            } else {
                CMap_set_name(
                    cmap,
                    &tok2.getSV().unwrap(),
                );
            }
        } else if tok1.typ() == PstType::Name
            && tok1.as_name().0.as_bytes().starts_with(b"CMapType")
        {
            let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Integer) || check_next_token(&mut input, "def").is_err() {
                status = -1i32
            } else {
                CMap_set_type(cmap, tok2.getIV());
            }
        } else if tok1.typ() == PstType::Name
            && tok1.as_name().0.as_bytes().starts_with(b"WMode")
        {
            let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
            if !(tok2.typ() == PstType::Integer) || check_next_token(&mut input, "def").is_err() {
                status = -1i32
            } else {
                CMap_set_wmode(cmap, tok2.getIV());
            }
        } else if tok1.typ() == PstType::Name
            && tok1.as_name().0.as_bytes().starts_with(b"CIDSystemInfo")
        {
            status = do_cidsysteminfo(cmap, &mut input)
        } else if !(tok1.typ() == PstType::Name
            && tok1.as_name().0.as_bytes().starts_with(b"Version")
            || tok1.typ() == PstType::Name
                && tok1.as_name().0.as_bytes().starts_with(b"UIDOffset")
            || tok1.typ() == PstType::Name
                && tok1.as_name().0.as_bytes().starts_with(b"XUID"))
        {
            if tok1.typ() == PstType::Name {
                /* Possibly usecmap comes next */
                let tok2 = pst_get_token(&mut input.cursor, input.endptr).unwrap();
                if tok2.typ() == PstType::Unknown
                    && tok2.as_unknown().starts_with(b"usecmap")
                {
                    let id = CMap_cache_find(
                        &tok1.getSV().unwrap(),
                    );
                    if id < 0i32 {
                        status = -1i32
                    } else {
                        let ucmap = CMap_cache_get(id);
                        CMap_set_usecmap(cmap, ucmap);
                    }
                }
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"begincodespacerange")
            {
                do_codespacerange(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"beginnotdefrange")
            {
                do_notdefrange(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"beginnotdefchar")
            {
                do_notdefchar(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"beginbfrange")
            {
                do_bfrange(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"beginbfchar")
            {
                do_bfchar(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"begincidrange")
            {
                do_cidrange(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Unknown
                && tok1.as_unknown().starts_with(b"begincidchar")
            {
                do_cidchar(cmap, &mut input, tmpint)?;
            } else if tok1.typ() == PstType::Integer {
                tmpint = tok1.getIV()
            }
        }
    }
    if status < 0i32 {
        Err(())
    } else {
        Ok(CMap_is_valid(cmap) as i32)
    }
}
