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
use super::dpx_pst::{pst_get_token, PstObj};
use crate::bridge::{size_t, ttstub_input_get_size, InFile};
use libc::{free, memmove, strstr};

use super::dpx_cid::CIDSysInfo;
/* Mapping types, MAP_IS_NAME is not supported. */
/* Lookup flags */
/* DEBUG */
/* Codespacerange */

use super::dpx_cmap::CMap;
static mut __verbose: i32 = 0;

pub(crate) type CID = u16;
#[repr(C)]
pub(crate) struct ifreader {
    pub(crate) cursor: *const u8,
    pub(crate) endptr: *const u8,
    pub(crate) buf: *mut u8,
    pub(crate) max: size_t,
    pub(crate) handle: InFile,
    pub(crate) unread: size_t,
}
impl ifreader {
    unsafe fn new(handle: InFile, size: size_t, bufsize: size_t) -> Self {
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
        *buf = 0;
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
    let mut bytesread: size_t = 0 as size_t;
    let bytesrem = (reader.endptr as size_t).wrapping_sub(reader.cursor as size_t);
    if size > reader.max {
        if __verbose != 0 {
            info!("\nExtending buffer ({} bytes)...\n", size);
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
        let slice = std::slice::from_raw_parts_mut(
            reader.buf.offset(bytesrem as isize),
            bytesread as usize,
        );
        reader
            .handle
            .read_exact(slice)
            .expect("Reading file failed.");
        reader.cursor = reader.buf;
        reader.endptr = reader.buf.offset((bytesrem + bytesread) as isize);
        reader.unread = (reader.unread as u64).wrapping_sub(bytesread as _) as size_t as size_t;
        if __verbose != 0 {
            info!(
                "Reading more {} bytes ({} bytes remains in buffer)...\n",
                bytesread, bytesrem,
            );
        }
    }
    let len = reader.endptr.offset_from(reader.cursor);
    *reader.buf.offset(len) = 0_u8;
    bytesread.wrapping_add(bytesrem)
}
unsafe fn check_next_token(input: &mut ifreader, key: &str) -> Result<(), ()> {
    if ifreader_read(input, key.len() as _) == 0 {
        return Err(());
    }
    let token = pst_get_token(&mut input.cursor, input.endptr)
        .ok_or(())?
        .into_string();
    if token != key {
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
    if let (PstObj::String(tok1), PstObj::String(tok2)) = (tok1, tok2) {
        let dim1 = tok1.len();
        let dim2 = tok2.len();
        if dim1 != dim2 || dim1 > maxlen {
            return Err(());
        }
        let codeLo = tok1.into_bytes();
        let codeHi = tok2.into_bytes();
        Ok((codeLo, codeHi, dim1))
    } else {
        return Err(());
    }
}
unsafe fn do_codespacerange(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
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
    cmap: &mut CMap,
    input: &mut ifreader,
    codeLo: &mut Vec<u8>,
    dim: i32,
    count: i32,
) -> Result<(), ()> {
    if dim < 1 {
        panic!("Invalid code range.");
    }
    for _ in 0..count {
        match pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
            PstObj::String(data) => {
                let data = data.as_bytes();
                CMap_add_bfchar(
                    cmap,
                    codeLo.as_ptr(),
                    dim as size_t,
                    data.as_ptr(),
                    data.len() as size_t,
                );
            }
            PstObj::Name(_) => {
                panic!("{}: Mapping to charName not supported.", "CMap_parse:");
            }
            _ => {
                panic!("{}: Invalid CMap mapping record.", "CMap_parse:");
            }
        }
        codeLo[(dim - 1) as usize] += 1;
    }
    check_next_token(input, "]")
}
unsafe fn do_notdefrange(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 3) as size_t) == 0 {
            return Err(());
        }
        let (codeLo, codeHi, dim) = get_coderange(input, 127)?;
        if let PstObj::Integer(dstCID) = pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
            if dstCID >= 0 && dstCID <= 65535 {
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
unsafe fn do_bfrange(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 3) as size_t) == 0 {
            return Err(());
        }
        let (mut codeLo, codeHi, srcdim) = get_coderange(input, 127)?;
        match pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
            PstObj::String(data) => {
                let data = data.as_bytes();
                CMap_add_bfrange(
                    cmap,
                    codeLo.as_ptr(),
                    codeHi.as_ptr(),
                    srcdim as size_t,
                    data.as_ptr(),
                    data.len() as size_t,
                );
            }
            PstObj::Mark => {
                let last_lo = codeLo[(srcdim - 1) as usize];
                handle_codearray(
                    cmap,
                    input,
                    &mut codeLo,
                    srcdim as _,
                    codeHi[(srcdim - 1) as usize] as i32 - last_lo as i32 + 1,
                )?;
            }
            _ => warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:"),
        }
    }
    check_next_token(input, "endbfrange")
}
unsafe fn do_cidrange(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 3) as size_t) == 0 {
            return Err(());
        }
        let (codeLo, codeHi, dim) = get_coderange(input, 127)?;
        if let PstObj::Integer(dstCID) = pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
            if dstCID >= 0 && dstCID <= 65535 {
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
unsafe fn do_notdefchar(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 2) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if let (PstObj::String(data), PstObj::Integer(dstCID)) = (tok1, tok2) {
            if dstCID >= 0 && dstCID <= 65535 {
                let data = data.as_bytes();
                CMap_add_notdefchar(cmap, data.as_ptr(), data.len() as size_t, dstCID as CID);
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endnotdefchar")
}
unsafe fn do_bfchar(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 2) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        /* We only support single CID font as descendant font, charName should not come here. */
        match (tok1, tok2) {
            (PstObj::String(tok1), PstObj::String(tok2)) => {
                let data1 = tok1.as_bytes();
                let data2 = tok2.as_bytes();
                CMap_add_bfchar(
                    cmap,
                    data1.as_ptr(),
                    data1.len() as size_t,
                    data2.as_ptr(),
                    data2.len() as size_t,
                );
            }
            (_, PstObj::Name(_)) => panic!("{}: Mapping to charName not supported.", "CMap_parse:"),
            _ => warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:"),
        }
    }
    check_next_token(input, "endbfchar")
}
unsafe fn do_cidchar(cmap: &mut CMap, input: &mut ifreader, count: i32) -> Result<(), ()> {
    for _ in 0..count {
        if ifreader_read(input, (127 * 2) as size_t) == 0 {
            return Err(());
        }
        let tok1 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        let tok2 = pst_get_token(&mut input.cursor, input.endptr).ok_or(())?;
        if let (PstObj::String(data), PstObj::Integer(dstCID)) = (tok1, tok2) {
            if dstCID >= 0 && dstCID <= 65535 {
                let data = data.as_bytes();
                CMap_add_cidchar(cmap, data.as_ptr(), data.len() as size_t, dstCID as CID);
            }
        } else {
            warn!("{}: Invalid CMap mapping record. (ignored)", "CMap_parse:");
        }
    }
    check_next_token(input, "endcidchar")
}
unsafe fn do_cidsysteminfo(cmap: &mut CMap, input: &mut ifreader) -> i32 {
    let mut csi: CIDSysInfo = CIDSysInfo {
        registry: "".into(),
        ordering: "".into(),
        supplement: -1,
    };
    let mut simpledict: i32 = 0;
    let mut error: i32 = 0;
    ifreader_read(input, (127 * 2) as size_t);
    loop
    /*
     * Assuming /CIDSystemInfo 3 dict dup begin .... end def
     * or /CIDSystemInfo << ... >> def
     */
    {
        match pst_get_token(&mut input.cursor, input.endptr) {
            None => break,
            Some(PstObj::Mark) => {
                simpledict = 1;
                break;
            }
            Some(PstObj::Unknown(tok1)) if tok1.starts_with(b"begin") => {
                simpledict = 0;
                break;
            }
            _ => {}
        }
    }
    while error == 0 {
        match pst_get_token(&mut input.cursor, input.endptr) {
            None => break,
            Some(tok1) => match tok1 {
                PstObj::Unknown(data) if (data.starts_with(b">>") && simpledict != 0) => break,

                PstObj::Unknown(data) if (data.starts_with(b"end") && simpledict == 0) => break,
                _ => {
                    let mut tok2 = None;
                    match tok1 {
                        PstObj::Name(data1)
                            if (data1.as_bytes().starts_with(b"Registry") && {
                                tok2 = pst_get_token(&mut input.cursor, input.endptr);
                                tok2.is_some()
                            }) =>
                        {
                            let tok2 = tok2.take().unwrap();
                            if let PstObj::String(data) = tok2 {
                                if !(simpledict == 0 && check_next_token(input, "def").is_err()) {
                                    csi.registry = data.into()
                                } else {
                                    error = -1;
                                }
                            } else {
                                error = -1;
                            }
                        }
                        PstObj::Name(data1)
                            if (data1.as_bytes().starts_with(b"Ordering") && {
                                tok2 = pst_get_token(&mut input.cursor, input.endptr);
                                tok2.is_some()
                            }) =>
                        {
                            let tok2 = tok2.take().unwrap();
                            if let PstObj::String(data) = tok2 {
                                if !(simpledict == 0 && check_next_token(input, "def").is_err()) {
                                    csi.ordering = data.into();
                                } else {
                                    error = -1;
                                }
                            } else {
                                error = -1;
                            }
                        }
                        PstObj::Name(data1)
                            if (data1.as_bytes().starts_with(b"Supplement") && {
                                tok2 = pst_get_token(&mut input.cursor, input.endptr);
                                tok2.is_some()
                            }) =>
                        {
                            let tok2 = tok2.take().unwrap();
                            if let PstObj::Integer(data) = tok2 {
                                if !(simpledict == 0 && check_next_token(input, "def").is_err()) {
                                    csi.supplement = data;
                                } else {
                                    error = -1;
                                }
                            } else {
                                error = -1;
                            }
                        }
                        _ => {}
                    }
                }
            },
        }
    }
    if error == 0 && check_next_token(input, "def").is_err() {
        error = -1
    }
    if error == 0 && !csi.registry.is_empty() && !csi.ordering.is_empty() && csi.supplement >= 0 {
        CMap_set_CIDSysInfo(cmap, &mut csi);
    }
    error
}

pub(crate) unsafe fn CMap_parse_check_sig<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut result: i32 = -1;
    let mut sig: [u8; 65] = [0; 65];
    handle.seek(SeekFrom::Start(0)).unwrap();
    if handle.read_exact(&mut sig[..64]).is_err() {
        result = -1
    } else {
        sig[64] = 0;
        if strstartswith(
            sig.as_ptr() as *const i8,
            b"%!PS\x00" as *const u8 as *const i8,
        )
        .is_null()
        {
            result = -1
        } else if !strstr(
            sig.as_ptr().offset(4) as *const i8,
            b"Resource-CMap\x00" as *const u8 as *const i8,
        )
        .is_null()
        {
            result = 0
        }
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    result
}

pub(crate) unsafe fn CMap_parse(cmap: &mut CMap, mut handle: InFile) -> Result<i32, ()> {
    let mut status: i32 = 0;
    let mut tmpint: i32 = -1;
    let size = ttstub_input_get_size(&mut handle);
    let mut input = ifreader::new(handle, size, (4096 - 1) as size_t);
    while status >= 0 {
        ifreader_read(&mut input, (4096 / 2) as size_t);
        match pst_get_token(&mut input.cursor, input.endptr) {
            None => break,
            Some(tok1) => match tok1 {
                PstObj::Name(data1) if data1.as_bytes().starts_with(b"CMapName") => {
                    match pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
                        PstObj::Name(data) if check_next_token(&mut input, "def").is_ok() => {
                            CMap_set_name(cmap, data.to_str().unwrap());
                        }
                        PstObj::String(data) if check_next_token(&mut input, "def").is_ok() => {
                            CMap_set_name(cmap, &data);
                        }
                        _ => status = -1,
                    }
                }
                PstObj::Name(data1) if data1.as_bytes().starts_with(b"CMapType") => {
                    match pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
                        PstObj::Integer(data) if check_next_token(&mut input, "def").is_ok() => {
                            CMap_set_type(cmap, data);
                        }
                        _ => status = -1,
                    }
                }
                PstObj::Name(data1) if data1.as_bytes().starts_with(b"WMode") => {
                    match pst_get_token(&mut input.cursor, input.endptr).ok_or(())? {
                        PstObj::Integer(data) if check_next_token(&mut input, "def").is_ok() => {
                            CMap_set_wmode(cmap, data);
                        }
                        _ => status = -1,
                    }
                }
                PstObj::Name(data1) if data1.as_bytes().starts_with(b"CIDSystemInfo") => {
                    status = do_cidsysteminfo(cmap, &mut input);
                }
                PstObj::Name(data1)
                    if (data1.as_bytes().starts_with(b"Version")
                        || data1.as_bytes().starts_with(b"UIDOffset")
                        || data1.as_bytes().starts_with(b"XUID")) => {}
                PstObj::Name(data1) => {
                    /* Possibly usecmap comes next */
                    match pst_get_token(&mut input.cursor, input.endptr).unwrap() {
                        PstObj::Unknown(data2) if data2.starts_with(b"usecmap") => {
                            let id = CMap_cache_find(data1.to_str().unwrap());
                            if id < 0 {
                                status = -1;
                            } else {
                                let ucmap = CMap_cache_get(id);
                                CMap_set_usecmap(cmap, ucmap);
                            }
                        }
                        _ => {}
                    }
                }
                PstObj::Unknown(data1) => {
                    if data1.starts_with(b"begincodespacerange") {
                        do_codespacerange(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"beginnotdefrange") {
                        do_notdefrange(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"beginnotdefchar") {
                        do_notdefchar(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"beginbfrange") {
                        do_bfrange(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"beginbfchar") {
                        do_bfchar(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"begincidrange") {
                        do_cidrange(cmap, &mut input, tmpint)?;
                    } else if data1.starts_with(b"begincidchar") {
                        do_cidchar(cmap, &mut input, tmpint)?;
                    }
                }
                PstObj::Integer(data) => {
                    tmpint = data;
                }
                _ => {}
            },
        }
    }
    if status < 0 {
        Err(())
    } else {
        Ok(CMap_is_valid(cmap) as i32)
    }
}
