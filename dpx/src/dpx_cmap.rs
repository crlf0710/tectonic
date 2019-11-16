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

use crate::mfree;
use crate::streq_ptr;
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_cid::CSI_IDENTITY;
use super::dpx_cmap_read::{CMap_parse, CMap_parse_check_sig};
use super::dpx_mem::{new, renew};
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{free, memcmp, memcpy, memset, strcmp, strcpy, strlen};

pub type size_t = u64;

use crate::TTInputFormat;

use super::dpx_cid::CIDSysInfo;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct rangeDef {
    pub dim: size_t,
    pub codeLo: *mut u8,
    pub codeHi: *mut u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct mapDef {
    pub flag: i32,
    pub len: size_t,
    pub code: *mut u8,
    pub next: *mut mapDef,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct mapData {
    pub data: *mut u8,
    pub prev: *mut mapData,
    pub pos: i32,
}
/* quasi-hack to get the primary input */
/* CID, Code... MEM_ALLOC_SIZE bytes  */
/* Previous mapData data segment      */
/* Position of next free data segment */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CMap {
    pub name: *mut i8,
    pub type_0: i32,
    pub wmode: i32,
    pub CSI: *mut CIDSysInfo,
    pub useCMap: *mut CMap,
    pub codespace: C2RustUnnamed_0,
    pub mapTbl: *mut mapDef,
    pub mapData: *mut mapData,
    pub flags: i32,
    pub profile: C2RustUnnamed,
    pub reverseMap: *mut i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub minBytesIn: size_t,
    pub maxBytesIn: size_t,
    pub minBytesOut: size_t,
    pub maxBytesOut: size_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub num: u32,
    pub max: u32,
    pub ranges: *mut rangeDef,
}
pub type CID = u16;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CMap_cache {
    pub num: i32,
    pub max: i32,
    pub cmaps: *mut *mut CMap,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/*
 * References:
 *
 *  PostScript Language Reference Manual, 3rd. ed. (Adobe Systems Inc.)
 *    5.11.4 CMap Dictionaries
 *    5.11.5 FMapType 9 Composite Fonts
 *  Building CMap Files for CID-Keyed Fonts, Adobe Technical Note #5099
 *  CID-Keyed Font Technology Overview, Adobe Technical Note #5092
 *  Adobe CMap and CIDFont Files Specification, Adobe Technical Specification #5014
 *
 *  Undefined Character Handling:
 *    PLRM 3rd. ed., sec. 5.11.5., "Handling Undefined Characters"
 *
 * TODO:
 *   Only cid(range|char) allowed for CODE_TO_CID and bf(range|char) for CID_TO_CODE ?
 */
static mut __verbose: i32 = 0i32;
static mut __silent: i32 = 0i32;
#[no_mangle]
pub unsafe fn CMap_set_verbose(mut level: i32) {
    __verbose = level;
}
#[no_mangle]
pub unsafe fn CMap_set_silent(mut value: i32) {
    __silent = if value != 0 { 1i32 } else { 0i32 };
}
#[no_mangle]
pub unsafe fn CMap_new() -> *mut CMap {
    let cmap = new((1_u64).wrapping_mul(::std::mem::size_of::<CMap>() as u64) as u32) as *mut CMap;
    (*cmap).name = ptr::null_mut();
    (*cmap).type_0 = 1i32;
    (*cmap).wmode = 0i32;
    (*cmap).useCMap = ptr::null_mut();
    (*cmap).CSI = ptr::null_mut();
    (*cmap).profile.minBytesIn = 2i32 as size_t;
    (*cmap).profile.maxBytesIn = 2i32 as size_t;
    (*cmap).profile.minBytesOut = 2i32 as size_t;
    (*cmap).profile.maxBytesOut = 2i32 as size_t;
    (*cmap).flags = 0i32;
    (*cmap).codespace.num = 0_u32;
    (*cmap).codespace.max = 10_u32;
    (*cmap).codespace.ranges =
        new((10_u64).wrapping_mul(::std::mem::size_of::<rangeDef>() as u64) as u32)
            as *mut rangeDef;
    (*cmap).mapTbl = ptr::null_mut();
    (*cmap).mapData =
        new((1_u64).wrapping_mul(::std::mem::size_of::<mapData>() as u64) as u32) as *mut mapData;
    (*(*cmap).mapData).prev = ptr::null_mut();
    (*(*cmap).mapData).pos = 0i32;
    (*(*cmap).mapData).data =
        new((4096_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    (*cmap).reverseMap =
        new((65536_u64).wrapping_mul(::std::mem::size_of::<i32>() as u64) as u32) as *mut i32;
    memset(
        (*cmap).reverseMap as *mut libc::c_void,
        0i32,
        (65536usize).wrapping_mul(::std::mem::size_of::<i32>()),
    );
    cmap
}
#[no_mangle]
pub unsafe fn CMap_release(mut cmap: *mut CMap) {
    if cmap.is_null() {
        return;
    }
    free((*cmap).name as *mut libc::c_void);
    if !(*cmap).CSI.is_null() {
        free((*(*cmap).CSI).registry as *mut libc::c_void);
        free((*(*cmap).CSI).ordering as *mut libc::c_void);
        free((*cmap).CSI as *mut libc::c_void);
    }
    free((*cmap).codespace.ranges as *mut libc::c_void);
    if !(*cmap).mapTbl.is_null() {
        mapDef_release((*cmap).mapTbl);
    }
    let mut map: *mut mapData = (*cmap).mapData;
    while !map.is_null() {
        let mut prev: *mut mapData = (*map).prev;
        free((*map).data as *mut libc::c_void);
        free(map as *mut libc::c_void);
        map = prev
    }
    free((*cmap).reverseMap as *mut libc::c_void);
    free(cmap as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn CMap_is_Identity(mut cmap: *mut CMap) -> bool {
    assert!(!cmap.is_null());
    return streq_ptr((*cmap).name, b"Identity-H\x00" as *const u8 as *const i8) as i32 != 0
        || streq_ptr((*cmap).name, b"Identity-V\x00" as *const u8 as *const i8) as i32 != 0;
}
#[no_mangle]
pub unsafe fn CMap_is_valid(mut cmap: *mut CMap) -> bool {
    /* Quick check */
    if cmap.is_null()
        || (*cmap).name.is_null()
        || (*cmap).type_0 < 0i32
        || (*cmap).type_0 > 3i32
        || (*cmap).codespace.num < 1_u32
        || (*cmap).type_0 != 0i32 && (*cmap).mapTbl.is_null()
    {
        return false;
    }
    if !(*cmap).useCMap.is_null() {
        let csi1 = CMap_get_CIDSysInfo(cmap);
        let csi2 = CMap_get_CIDSysInfo((*cmap).useCMap);
        if strcmp((*csi1).registry, (*csi2).registry) != 0
            || strcmp((*csi1).ordering, (*csi2).ordering) != 0
        {
            warn!(
                "CIDSystemInfo mismatched {} <--> {}",
                CStr::from_ptr(CMap_get_name(cmap)).display(),
                CStr::from_ptr(CMap_get_name((*cmap).useCMap)).display(),
            );
            return false;
        }
    }
    true
}
#[no_mangle]
pub unsafe fn CMap_get_profile(mut cmap: *mut CMap, mut type_0: i32) -> i32 {
    assert!(!cmap.is_null());
    match type_0 {
        0 => (*cmap).profile.minBytesIn as i32,
        1 => (*cmap).profile.maxBytesIn as i32,
        2 => (*cmap).profile.maxBytesOut as i32,
        3 => (*cmap).profile.maxBytesOut as i32,
        _ => {
            panic!("{}: Unrecognized profile type {}.", "CMap", type_0,);
        }
    }
}
/*
 * Put notdef chars for codes not declared in notdef(range|char)
 */
unsafe fn handle_undefined(
    mut cmap: *mut CMap,
    mut inbuf: *mut *const u8,
    mut inbytesleft: *mut size_t,
    mut outbuf: *mut *mut u8,
    mut outbytesleft: *mut size_t,
) {
    if *outbytesleft < 2i32 as u64 {
        panic!("{}: Buffer overflow.", "CMap",);
    }
    match (*cmap).type_0 {
        1 => {
            memcpy(
                *outbuf as *mut libc::c_void,
                b"\x00\x00\x00" as *const u8 as *const i8 as *const libc::c_void,
                2,
            );
        }
        2 => {
            memcpy(
                *outbuf as *mut libc::c_void,
                b"\xff\xfd\x00" as *const u8 as *const i8 as *const libc::c_void,
                2,
            );
        }
        _ => {
            warn!(
                "Cannot handle undefined mapping for this type of CMap mapping: {}",
                (*cmap).type_0
            );
            warn!("<0000> is used for .notdef char.");
            memset(*outbuf as *mut libc::c_void, 0i32, 2);
        }
    }
    *outbuf = (*outbuf).offset(2);
    *outbytesleft = (*outbytesleft as u64).wrapping_sub(2i32 as u64) as size_t as size_t;
    let len = bytes_consumed(cmap, *inbuf, *inbytesleft);
    *inbuf = (*inbuf).offset(len as isize);
    *inbytesleft = (*inbytesleft as u64).wrapping_sub(len) as size_t as size_t;
}
#[no_mangle]
pub unsafe fn CMap_decode_char(
    mut cmap: *mut CMap,
    mut inbuf: *mut *const u8,
    mut inbytesleft: *mut size_t,
    mut outbuf: *mut *mut u8,
    mut outbytesleft: *mut size_t,
) {
    let mut c: u8 = 0_u8;
    let mut count: size_t = 0i32 as size_t;
    let mut save = *inbuf;
    let mut p = save as *const u8;
    /*
     * First handle some special cases:
     */
    if (*cmap).type_0 == 0i32 {
        if (*inbytesleft).wrapping_rem(2i32 as u64) != 0 {
            panic!("{}: Invalid/truncated input string.", "CMap",);
        }
        if *outbytesleft < 2i32 as u64 {
            panic!("{}: Buffer overflow.", "CMap",);
        }
        memcpy(
            *outbuf as *mut libc::c_void,
            *inbuf as *const libc::c_void,
            2,
        );
        *inbuf = (*inbuf).offset(2);
        *outbuf = (*outbuf).offset(2);
        *outbytesleft = (*outbytesleft as u64).wrapping_sub(2i32 as u64) as size_t as size_t;
        *inbytesleft = (*inbytesleft as u64).wrapping_sub(2i32 as u64) as size_t as size_t;
        return;
    } else {
        if (*cmap).mapTbl.is_null() {
            if !(*cmap).useCMap.is_null() {
                CMap_decode_char((*cmap).useCMap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            } else {
                /* no mapping available in this CMap */
                warn!("No mapping available for this character.");
                handle_undefined(cmap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            }
        }
    }
    assert!(!(*cmap).mapTbl.is_null());
    let mut t = (*cmap).mapTbl;
    while count < *inbytesleft {
        let fresh0 = p;
        p = p.offset(1);
        c = *fresh0;
        count = count.wrapping_add(1);
        if (*t.offset(c as isize)).flag & 1i32 << 4i32 == 0 {
            break;
        }
        t = (*t.offset(c as isize)).next
    }
    if (*t.offset(c as isize)).flag & 1i32 << 4i32 != 0 {
        /* need more bytes */
        panic!("{}: Premature end of input string.", "CMap",);
    } else {
        if if (*t.offset(c as isize)).flag & 0xfi32 != 0i32 {
            1i32
        } else {
            0i32
        } == 0
        {
            if !(*cmap).useCMap.is_null() {
                CMap_decode_char((*cmap).useCMap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            } else {
                /* no mapping available in this CMap */
                warn!("No character mapping available.");
                info!(
                    " CMap name: {}\n",
                    CStr::from_ptr(CMap_get_name(cmap)).display()
                );
                info!(" input str: ");
                info!("<");
                while save < p {
                    info!("{:02x}", *save as i32);
                    save = save.offset(1)
                }
                info!(">\n");
                /*
                 * We know partial match found up to `count' bytes,
                 * but we will not use this information for the sake of simplicity.
                 */
                handle_undefined(cmap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            }
        } else {
            match (*t.offset(c as isize)).flag & 0xfi32 {
                8 => {
                    warn!("Character mapped to .notdef found.");
                }
                1 | 4 => {}
                2 => {
                    panic!("{}: CharName mapping not supported.", "CMap",);
                }
                _ => {
                    panic!("{}: Unknown mapping type.", "CMap",);
                }
            }
            /* continue */
            if *outbytesleft >= (*t.offset(c as isize)).len {
                memcpy(
                    *outbuf as *mut libc::c_void,
                    (*t.offset(c as isize)).code as *const libc::c_void,
                    (*t.offset(c as isize)).len as _,
                );
            } else {
                panic!("{}: Buffer overflow.", "CMap",);
            }
            *outbuf = (*outbuf).offset((*t.offset(c as isize)).len as isize);
            *outbytesleft = (*outbytesleft as u64).wrapping_sub((*t.offset(c as isize)).len)
                as size_t as size_t;
            if !inbytesleft.is_null() {
                *inbytesleft = (*inbytesleft as u64).wrapping_sub(count) as size_t as size_t
            }
            *inbuf = p
        }
    };
}
/*
 * For convenience, it does not do decoding to CIDs.
 */
#[no_mangle]
pub unsafe fn CMap_decode(
    mut cmap: *mut CMap,
    mut inbuf: *mut *const u8,
    mut inbytesleft: *mut size_t,
    mut outbuf: *mut *mut u8,
    mut outbytesleft: *mut size_t,
) -> size_t {
    assert!(!cmap.is_null() && !inbuf.is_null() && !outbuf.is_null());
    assert!(!inbytesleft.is_null() && !outbytesleft.is_null());
    let mut count = 0i32 as size_t;
    while *inbytesleft > 0i32 as u64 && *outbytesleft > 0i32 as u64 {
        CMap_decode_char(cmap, inbuf, inbytesleft, outbuf, outbytesleft);
        count += 1;
    }
    count
}
#[no_mangle]
pub unsafe fn CMap_reverse_decode(mut cmap: *mut CMap, mut cid: CID) -> i32 {
    let mut ch: i32 = if !(*cmap).reverseMap.is_null() {
        *(*cmap).reverseMap.offset(cid as isize)
    } else {
        -1i32
    };
    if ch == 0i32 && !(*cmap).useCMap.is_null() {
        return CMap_reverse_decode((*cmap).useCMap, cid);
    }
    ch
}
#[no_mangle]
pub unsafe fn CMap_get_name(mut cmap: *mut CMap) -> *mut i8 {
    assert!(!cmap.is_null());
    (*cmap).name
}
#[no_mangle]
pub unsafe fn CMap_get_type(mut cmap: *mut CMap) -> i32 {
    assert!(!cmap.is_null());
    (*cmap).type_0
}
#[no_mangle]
pub unsafe fn CMap_get_wmode(mut cmap: *mut CMap) -> i32 {
    assert!(!cmap.is_null());
    (*cmap).wmode
}
#[no_mangle]
pub unsafe fn CMap_get_CIDSysInfo(mut cmap: *mut CMap) -> *mut CIDSysInfo {
    assert!(!cmap.is_null());
    (*cmap).CSI
}
#[no_mangle]
pub unsafe fn CMap_set_name(mut cmap: *mut CMap, mut name: *const i8) {
    assert!(!cmap.is_null());
    free((*cmap).name as *mut libc::c_void);
    (*cmap).name =
        new((strlen(name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*cmap).name, name);
}
#[no_mangle]
pub unsafe fn CMap_set_type(mut cmap: *mut CMap, mut type_0: i32) {
    assert!(!cmap.is_null());
    (*cmap).type_0 = type_0;
}
#[no_mangle]
pub unsafe fn CMap_set_wmode(mut cmap: *mut CMap, mut wmode: i32) {
    assert!(!cmap.is_null());
    (*cmap).wmode = wmode;
}
#[no_mangle]
pub unsafe fn CMap_set_CIDSysInfo(mut cmap: *mut CMap, mut csi: *const CIDSysInfo) {
    assert!(!cmap.is_null());
    if !(*cmap).CSI.is_null() {
        free((*(*cmap).CSI).registry as *mut libc::c_void);
        free((*(*cmap).CSI).ordering as *mut libc::c_void);
        free((*cmap).CSI as *mut libc::c_void);
    }
    if !csi.is_null() && !(*csi).registry.is_null() && !(*csi).ordering.is_null() {
        (*cmap).CSI =
            new((1usize).wrapping_mul(::std::mem::size_of::<CIDSysInfo>()) as _) as *mut CIDSysInfo;
        (*(*cmap).CSI).registry = new((strlen((*csi).registry).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*cmap).CSI).registry, (*csi).registry);
        (*(*cmap).CSI).ordering = new((strlen((*csi).ordering).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*cmap).CSI).ordering, (*csi).ordering);
        (*(*cmap).CSI).supplement = (*csi).supplement
    } else {
        warn!("Invalid CIDSystemInfo.");
        (*cmap).CSI = ptr::null_mut()
    };
}
/*
 * Can have muliple entry ?
 */
#[no_mangle]
pub unsafe fn CMap_set_usecmap(mut cmap: *mut CMap, mut ucmap: *mut CMap) {
    /* Maybe if (!ucmap) panic! is better for this. */
    assert!(!cmap.is_null());
    assert!(!ucmap.is_null());
    if cmap == ucmap {
        panic!(
            "{}: Identical CMap object cannot be used for usecmap CMap: 0x{:p}=0x{:p}",
            "CMap", cmap, ucmap,
        );
    }
    /* Check if ucmap have neccesary information. */
    if !CMap_is_valid(ucmap) {
        panic!("{}: Invalid CMap.", "CMap",);
    }
    /*
     *  CMapName of cmap can be undefined when usecmap is executed in CMap parsing.
     *  And it is also possible CSI is not defined at that time.
     */
    if streq_ptr((*cmap).name, (*ucmap).name) {
        panic!(
            "{}: CMap refering itself not allowed: CMap {} --> {}",
            "CMap",
            CStr::from_ptr((*cmap).name).display(),
            CStr::from_ptr((*ucmap).name).display(),
        );
    }
    if !(*cmap).CSI.is_null()
        && !(*(*cmap).CSI).registry.is_null()
        && !(*(*cmap).CSI).ordering.is_null()
    {
        if strcmp((*(*cmap).CSI).registry, (*(*ucmap).CSI).registry) != 0
            || strcmp((*(*cmap).CSI).ordering, (*(*ucmap).CSI).ordering) != 0
        {
            panic!(
                "{}: CMap {} required by {} have different CSI.",
                "CMap",
                CStr::from_ptr(CMap_get_name(cmap)).display(),
                CStr::from_ptr(CMap_get_name(ucmap)).display(),
            );
        }
    }
    /* We must copy codespaceranges. */
    for i in 0..(*ucmap).codespace.num {
        let mut csr: *mut rangeDef = (*ucmap).codespace.ranges.offset(i as isize);
        CMap_add_codespacerange(cmap, (*csr).codeLo, (*csr).codeHi, (*csr).dim);
    }
    (*cmap).useCMap = ucmap;
}
/* Test the validity of character c. */
unsafe fn CMap_match_codespace(mut cmap: *mut CMap, mut c: *const u8, mut dim: size_t) -> i32 {
    assert!(!cmap.is_null());
    for i in 0..(*cmap).codespace.num {
        let mut csr: *mut rangeDef = (*cmap).codespace.ranges.offset(i as isize);
        if !((*csr).dim != dim) {
            let mut pos = 0_u32;
            while (pos as u64) < dim {
                if *c.offset(pos as isize) as i32 > *(*csr).codeHi.offset(pos as isize) as i32
                    || (*c.offset(pos as isize) as i32) < *(*csr).codeLo.offset(pos as isize) as i32
                {
                    break;
                }
                pos += 1;
            }
            if pos as u64 == dim {
                return 0i32;
            }
        }
        /* Valid */
    }
    return -1i32;
    /* Invalid */
}
/*
 * No overlapping codespace ranges are allowed, otherwise mapping is ambiguous.
 */
#[no_mangle]
pub unsafe fn CMap_add_codespacerange(
    mut cmap: *mut CMap,
    mut codelo: *const u8,
    mut codehi: *const u8,
    mut dim: size_t,
) -> i32 {
    assert!(!cmap.is_null() && dim > 0i32 as u64);
    for i in 0..(*cmap).codespace.num {
        let mut overlap: bool = true;
        let csr = (*cmap).codespace.ranges.offset(i as isize);
        let mut j = 0i32 as size_t;
        while j < (if (*csr).dim < dim { (*csr).dim } else { dim }) && overlap as i32 != 0 {
            if *codelo.offset(j as isize) as i32 >= *(*csr).codeLo.offset(j as isize) as i32
                && *codelo.offset(j as isize) as i32 <= *(*csr).codeHi.offset(j as isize) as i32
                || *codehi.offset(j as isize) as i32 >= *(*csr).codeLo.offset(j as isize) as i32
                    && *codehi.offset(j as isize) as i32 <= *(*csr).codeHi.offset(j as isize) as i32
            {
                overlap = true
            } else {
                overlap = false
            }
            j += 1;
        }
        if overlap {
            warn!("Overlapping codespace found. (ingored)");
            return -1i32;
        }
    }
    if dim < (*cmap).profile.minBytesIn {
        (*cmap).profile.minBytesIn = dim
    }
    if dim > (*cmap).profile.maxBytesIn {
        (*cmap).profile.maxBytesIn = dim
    }
    if (*cmap).codespace.num.wrapping_add(1_u32) > (*cmap).codespace.max {
        (*cmap).codespace.max = (*cmap).codespace.max.wrapping_add(10_u32);
        (*cmap).codespace.ranges = renew(
            (*cmap).codespace.ranges as *mut libc::c_void,
            ((*cmap).codespace.max as u64).wrapping_mul(::std::mem::size_of::<rangeDef>() as u64)
                as u32,
        ) as *mut rangeDef
    }
    let csr = (*cmap)
        .codespace
        .ranges
        .offset((*cmap).codespace.num as isize);
    (*csr).dim = dim;
    (*csr).codeHi = get_mem(cmap, dim as i32);
    (*csr).codeLo = get_mem(cmap, dim as i32);
    memcpy(
        (*csr).codeHi as *mut libc::c_void,
        codehi as *const libc::c_void,
        dim as _,
    );
    memcpy(
        (*csr).codeLo as *mut libc::c_void,
        codelo as *const libc::c_void,
        dim as _,
    );
    (*cmap).codespace.num = (*cmap).codespace.num.wrapping_add(1);
    0i32
}
#[no_mangle]
pub unsafe fn CMap_add_notdefchar(
    mut cmap: *mut CMap,
    mut src: *const u8,
    mut srcdim: size_t,
    mut dst: CID,
) -> i32 {
    CMap_add_notdefrange(cmap, src, src, srcdim, dst)
}
#[no_mangle]
pub unsafe fn CMap_add_notdefrange(
    mut cmap: *mut CMap,
    mut srclo: *const u8,
    mut srchi: *const u8,
    mut srcdim: size_t,
    mut dst: CID,
) -> i32 {
    assert!(!cmap.is_null());
    /* dst not used here */
    /* FIXME */
    if check_range(
        cmap,
        srclo,
        srchi,
        srcdim,
        &mut dst as *mut CID as *const u8,
        2i32 as size_t,
    ) < 0i32
    {
        return -1i32;
    }
    if (*cmap).mapTbl.is_null() {
        (*cmap).mapTbl = mapDef_new()
    }
    let mut cur = (*cmap).mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0i32 {
        return -1i32;
    }
    for c in *srclo.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
        ..=*srchi.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
    {
        if if (*cur.offset(c as isize)).flag & 0xfi32 != 0i32 {
            1i32
        } else {
            0i32
        } != 0
        {
            if __silent == 0 {
                warn!("Trying to redefine already defined code mapping. (ignored)");
            }
        } else {
            (*cur.offset(c as isize)).flag = 0i32 | 1i32 << 3i32;
            let ref mut fresh1 = (*cur.offset(c as isize)).code;
            *fresh1 = get_mem(cmap, 2i32);
            (*cur.offset(c as isize)).len = 2i32 as size_t;
            *(*cur.offset(c as isize)).code.offset(0) = (dst as i32 >> 8i32) as u8;
            *(*cur.offset(c as isize)).code.offset(1) = (dst as i32 & 0xffi32) as u8
        }
        /* Do not do dst++ for notdefrange  */
    }
    0i32
}
#[no_mangle]
pub unsafe fn CMap_add_bfchar(
    mut cmap: *mut CMap,
    mut src: *const u8,
    mut srcdim: size_t,
    mut dst: *const u8,
    mut dstdim: size_t,
) -> i32 {
    CMap_add_bfrange(cmap, src, src, srcdim, dst, dstdim)
}
#[no_mangle]
pub unsafe fn CMap_add_bfrange(
    mut cmap: *mut CMap,
    mut srclo: *const u8,
    mut srchi: *const u8,
    mut srcdim: size_t,
    mut base: *const u8,
    mut dstdim: size_t,
) -> i32 {
    assert!(!cmap.is_null());
    if check_range(cmap, srclo, srchi, srcdim, base, dstdim) < 0i32 {
        return -1i32;
    }
    if (*cmap).mapTbl.is_null() {
        (*cmap).mapTbl = mapDef_new()
    }
    let mut cur = (*cmap).mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0i32 {
        return -1i32;
    }
    for c in *srclo.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
        ..=*srchi.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
    {
        /* According to 5014.CIDFont_Spec.pdf (p.52),
         * Code mappings (unlike codespace ranges) may overlap,
         * but succeeding maps superceded preceding maps.
         * (reported and patched by Luo Jie on 2007/12/2)
         */
        if (if (*cur.offset(c as isize)).flag & 0xfi32 != 0i32 {
            1i32
        } else {
            0i32
        }) == 0
            || (*cur.offset(c as isize)).len < dstdim
        {
            (*cur.offset(c as isize)).flag = 0i32 | 1i32 << 2i32;
            let ref mut fresh2 = (*cur.offset(c as isize)).code;
            *fresh2 = get_mem(cmap, dstdim as i32)
        }
        /*
         * We assume restriction to code ranges also applied here.
         * Addition <00FF> + 1 is undefined.
         *
         * Changed on 2004-03-20:
         *
         *  Should be treated as <0100> in Acrobat's "ToUnicode" CMap.
         */
        (*cur.offset(c as isize)).len = dstdim;
        memcpy(
            (*cur.offset(c as isize)).code as *mut libc::c_void,
            base as *const libc::c_void,
            dstdim as _,
        );
        let mut last_byte = c - *srclo.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
            + *base.offset(dstdim.wrapping_sub(1i32 as u64) as isize) as i32;
        *(*cur.offset(c as isize))
            .code
            .offset(dstdim.wrapping_sub(1i32 as u64) as isize) = (last_byte & 0xffi32) as u8;
        let mut i = dstdim.wrapping_sub(2i32 as u64) as i32;
        while i >= 0i32 && last_byte > 255i32 {
            last_byte = *(*cur.offset(c as isize)).code.offset(i as isize) as i32 + 1i32;
            *(*cur.offset(c as isize)).code.offset(i as isize) = (last_byte & 0xffi32) as u8;
            i -= 1
        }
    }
    0i32
}
#[no_mangle]
pub unsafe fn CMap_add_cidchar(
    mut cmap: *mut CMap,
    mut src: *const u8,
    mut srcdim: size_t,
    mut dst: CID,
) -> i32 {
    CMap_add_cidrange(cmap, src, src, srcdim, dst)
}
#[no_mangle]
pub unsafe fn CMap_add_cidrange(
    mut cmap: *mut CMap,
    mut srclo: *const u8,
    mut srchi: *const u8,
    mut srcdim: size_t,
    mut base: CID,
) -> i32 {
    assert!(!cmap.is_null());
    /* base not used here */
    if check_range(
        cmap,
        srclo,
        srchi,
        srcdim,
        &mut base as *mut CID as *const u8,
        2i32 as size_t,
    ) < 0i32
    {
        /* FIXME */
        return -1i32;
    }
    if (*cmap).mapTbl.is_null() {
        (*cmap).mapTbl = mapDef_new()
    }
    let mut cur = (*cmap).mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0i32 {
        return -1i32;
    }
    let mut v = 0i32 as size_t;
    for i in 0..srcdim.wrapping_sub(1i32 as u64) {
        v = (v << 8i32).wrapping_add(*srclo.offset(i as isize) as u64);
    }
    *(*cmap).reverseMap.offset(base as isize) = v as i32;
    for c in (*srclo.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as u64)
        ..=*srchi.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as u64
    {
        if (*cur.offset(c as isize)).flag != 0i32 {
            if __silent == 0 {
                warn!("Trying to redefine already defined CID mapping. (ignored)");
            }
        } else {
            (*cur.offset(c as isize)).flag = 0i32 | 1i32 << 0i32;
            (*cur.offset(c as isize)).len = 2i32 as size_t;
            let ref mut fresh3 = (*cur.offset(c as isize)).code;
            *fresh3 = get_mem(cmap, 2i32);
            *(*cur.offset(c as isize)).code.offset(0) = (base as i32 >> 8i32) as u8;
            *(*cur.offset(c as isize)).code.offset(1) = (base as i32 & 0xffi32) as u8;
            *(*cmap).reverseMap.offset(base as isize) = (v << 8i32).wrapping_add(c) as i32
        }
        if base as i32 >= 65535i32 {
            warn!("CID number too large.");
        }
        base += 1;
    }
    0i32
}
unsafe fn mapDef_release(mut t: *mut mapDef) {
    assert!(!t.is_null());
    for c in 0..256 {
        if (*t.offset(c as isize)).flag & 1i32 << 4i32 != 0 {
            mapDef_release((*t.offset(c as isize)).next);
        }
    }
    free(t as *mut libc::c_void);
}
unsafe fn mapDef_new() -> *mut mapDef {
    let t =
        new((256_u64).wrapping_mul(::std::mem::size_of::<mapDef>() as u64) as u32) as *mut mapDef;
    for c in 0..256 {
        (*t.offset(c as isize)).flag = 0i32 | 0i32;
        let ref mut fresh4 = (*t.offset(c as isize)).code;
        *fresh4 = ptr::null_mut();
        let ref mut fresh5 = (*t.offset(c as isize)).next;
        *fresh5 = ptr::null_mut();
    }
    t
}
unsafe fn get_mem(mut cmap: *mut CMap, mut size: i32) -> *mut u8 {
    assert!(!cmap.is_null() && !(*cmap).mapData.is_null() && size >= 0i32);
    let mut map = (*cmap).mapData;
    if (*map).pos + size >= 4096i32 {
        let mut prev: *mut mapData = map;
        map = new((1_u64).wrapping_mul(::std::mem::size_of::<mapData>() as u64) as u32)
            as *mut mapData;
        (*map).data =
            new((4096_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        (*map).prev = prev;
        (*map).pos = 0i32;
        (*cmap).mapData = map
    }
    let p = (*map).data.offset((*map).pos as isize);
    (*map).pos += size;
    p
}
unsafe fn locate_tbl(mut cur: *mut *mut mapDef, mut code: *const u8, mut dim: i32) -> i32 {
    assert!(!cur.is_null() && !(*cur).is_null());
    for i in 0..(dim - 1) {
        let mut c = *code.offset(i as isize) as i32;
        if if (*(*cur).offset(c as isize)).flag & 0xfi32 != 0i32 {
            1i32
        } else {
            0i32
        } != 0
        {
            warn!("Ambiguous CMap entry.");
            return -1i32;
        }
        if (*(*cur).offset(c as isize)).next.is_null() {
            /* create new node */
            let ref mut fresh6 = (*(*cur).offset(c as isize)).next;
            *fresh6 = mapDef_new()
        }
        (*(*cur).offset(c as isize)).flag |= 1i32 << 4i32;
        *cur = (*(*cur).offset(c as isize)).next;
    }
    0i32
}
/* Private funcs. */
/*
 * Guess how many bytes consumed as a `single' character:
 * Substring of length bytesconsumed bytes of input string is interpreted as
 * a `single' character by CMap_decode().
 */
unsafe fn bytes_consumed(mut cmap: *mut CMap, mut instr: *const u8, mut inbytes: size_t) -> size_t {
    let mut i = 0 as size_t;
    let mut longest: size_t = 0i32 as size_t;
    let mut bytesconsumed;
    assert!(!cmap.is_null());
    while i < (*cmap).codespace.num as u64 {
        let mut csr: *mut rangeDef = (*cmap).codespace.ranges.offset(i as isize);
        let mut pos = 0;
        while pos
            < (if (*csr).dim < inbytes {
                (*csr).dim
            } else {
                inbytes
            })
        {
            if *instr.offset(pos as isize) as i32 > *(*csr).codeHi.offset(pos as isize) as i32
                || (*instr.offset(pos as isize) as i32) < *(*csr).codeLo.offset(pos as isize) as i32
            {
                break;
            }
            pos = pos.wrapping_add(1)
        }
        if pos == (*csr).dim {
            /* part of instr is totally valid in this codespace. */
            return (*csr).dim;
        }
        if pos > longest {
            longest = pos
        }
        i += 1;
    }
    if i == (*cmap).codespace.num as u64 {
        /* No matching at all */
        bytesconsumed = (*cmap).profile.minBytesIn
    } else {
        bytesconsumed = (*cmap).profile.maxBytesIn;
        for i in 0..(*cmap).codespace.num as u64 {
            let mut csr_0: *mut rangeDef = (*cmap).codespace.ranges.offset(i as isize);
            if (*csr_0).dim > longest && (*csr_0).dim < bytesconsumed {
                bytesconsumed = (*csr_0).dim
            }
        }
    }
    bytesconsumed
}
unsafe fn check_range(
    mut cmap: *mut CMap,
    mut srclo: *const u8,
    mut srchi: *const u8,
    mut srcdim: size_t,
    mut dst: *const u8,
    mut dstdim: size_t,
) -> i32 {
    if srcdim < 1i32 as u64
        || dstdim < 1i32 as u64
        || (srclo.is_null() || srchi.is_null() || dst.is_null())
        || memcmp(
            srclo as *const libc::c_void,
            srchi as *const libc::c_void,
            srcdim.wrapping_sub(1) as _,
        ) != 0
        || *srclo.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
            > *srchi.offset(srcdim.wrapping_sub(1i32 as u64) as isize) as i32
    {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1i32;
    }
    if CMap_match_codespace(cmap, srclo, srcdim) < 0i32
        || CMap_match_codespace(cmap, srchi, srcdim) < 0i32
    {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1i32;
    }
    if srcdim < (*cmap).profile.minBytesIn {
        (*cmap).profile.minBytesIn = srcdim
    }
    if srcdim > (*cmap).profile.maxBytesIn {
        (*cmap).profile.maxBytesIn = srcdim
    }
    if dstdim < (*cmap).profile.minBytesOut {
        (*cmap).profile.minBytesOut = dstdim
    }
    if dstdim > (*cmap).profile.maxBytesOut {
        (*cmap).profile.maxBytesOut = dstdim
    }
    0i32
}
static mut __cache: *mut CMap_cache = std::ptr::null_mut();
#[no_mangle]
pub unsafe fn CMap_cache_init() {
    static mut range_min: [u8; 2] = [0; 2];
    static mut range_max: [u8; 2] = [0xff_u8, 0xff_u8];
    if !__cache.is_null() {
        panic!("{}: Already initialized.", "CMap",);
    }
    __cache = new((1_u64).wrapping_mul(::std::mem::size_of::<CMap_cache>() as u64) as u32)
        as *mut CMap_cache;
    (*__cache).max = 16u32 as i32;
    (*__cache).cmaps = new(((*__cache).max as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<*mut CMap>() as u64) as u32)
        as *mut *mut CMap;
    (*__cache).num = 0i32;
    /* Create Identity mapping */
    let ref mut fresh7 = *(*__cache).cmaps.offset(0);
    *fresh7 = CMap_new();
    CMap_set_name(
        *(*__cache).cmaps.offset(0),
        b"Identity-H\x00" as *const u8 as *const i8,
    );
    CMap_set_type(*(*__cache).cmaps.offset(0), 0i32);
    CMap_set_wmode(*(*__cache).cmaps.offset(0), 0i32);
    CMap_set_CIDSysInfo(*(*__cache).cmaps.offset(0), &mut CSI_IDENTITY);
    CMap_add_codespacerange(
        *(*__cache).cmaps.offset(0),
        range_min.as_mut_ptr(),
        range_max.as_mut_ptr(),
        2i32 as size_t,
    );
    let ref mut fresh8 = *(*__cache).cmaps.offset(1);
    *fresh8 = CMap_new();
    CMap_set_name(
        *(*__cache).cmaps.offset(1),
        b"Identity-V\x00" as *const u8 as *const i8,
    );
    CMap_set_type(*(*__cache).cmaps.offset(1), 0i32);
    CMap_set_wmode(*(*__cache).cmaps.offset(1), 1i32);
    CMap_set_CIDSysInfo(*(*__cache).cmaps.offset(1), &mut CSI_IDENTITY);
    CMap_add_codespacerange(
        *(*__cache).cmaps.offset(1),
        range_min.as_mut_ptr(),
        range_max.as_mut_ptr(),
        2i32 as size_t,
    );
    (*__cache).num += 2i32;
}
#[no_mangle]
pub unsafe fn CMap_cache_get(mut id: i32) -> *mut CMap {
    if __cache.is_null() {
        panic!("{}: CMap cache not initialized.", "CMap",);
    }
    if id < 0i32 || id >= (*__cache).num {
        panic!("Invalid CMap ID {}", id);
    }
    *(*__cache).cmaps.offset(id as isize)
}
#[no_mangle]
pub unsafe fn CMap_cache_find(mut cmap_name: *const i8) -> i32 {
    if __cache.is_null() {
        CMap_cache_init();
    }
    assert!(!__cache.is_null());
    for id in 0..(*__cache).num {
        /* CMapName may be undefined when processing usecmap. */
        let name = CMap_get_name(*(*__cache).cmaps.offset(id as isize));
        if !name.is_null() && streq_ptr(cmap_name, name) as i32 != 0 {
            return id;
        }
    }
    let mut handle = ttstub_input_open(cmap_name, TTInputFormat::CMAP, 0i32);
    if handle.is_none() {
        return -1i32;
    }
    if CMap_parse_check_sig(handle.as_mut()) < 0i32 {
        ttstub_input_close(handle.unwrap());
        return -1i32;
    }
    let handle = handle.unwrap();
    if __verbose != 0 {
        info!("(CMap:{}", CStr::from_ptr(cmap_name).display());
    }
    if (*__cache).num >= (*__cache).max {
        (*__cache).max = ((*__cache).max as u32).wrapping_add(16u32) as i32 as i32;
        (*__cache).cmaps = renew(
            (*__cache).cmaps as *mut libc::c_void,
            ((*__cache).max as u32 as u64).wrapping_mul(::std::mem::size_of::<*mut CMap>() as u64)
                as u32,
        ) as *mut *mut CMap
    }
    let id = (*__cache).num;
    (*__cache).num += 1;
    let ref mut fresh9 = *(*__cache).cmaps.offset(id as isize);
    *fresh9 = CMap_new();
    if CMap_parse(*(*__cache).cmaps.offset(id as isize), handle) < 0i32 {
        panic!("{}: Parsing CMap file failed.", "CMap",);
    }
    if __verbose != 0 {
        info!(")");
    }
    id
}
#[no_mangle]
pub unsafe fn CMap_cache_add(mut cmap: *mut CMap) -> i32 {
    if !CMap_is_valid(cmap) {
        panic!("{}: Invalid CMap.", "CMap",);
    }
    for id in 0..(*__cache).num {
        let cmap_name0 = CMap_get_name(cmap);
        let cmap_name1 = CMap_get_name(*(*__cache).cmaps.offset(id as isize));
        if streq_ptr(cmap_name0, cmap_name1) {
            panic!(
                "{}: CMap \"{}\" already defined.",
                "CMap",
                CStr::from_ptr(cmap_name0).display(),
            );
        }
    }
    if (*__cache).num >= (*__cache).max {
        (*__cache).max = ((*__cache).max as u32).wrapping_add(16u32) as i32 as i32;
        (*__cache).cmaps = renew(
            (*__cache).cmaps as *mut libc::c_void,
            ((*__cache).max as u32 as u64).wrapping_mul(::std::mem::size_of::<*mut CMap>() as u64)
                as u32,
        ) as *mut *mut CMap
    }
    let id = (*__cache).num;
    (*__cache).num += 1;
    let ref mut fresh10 = *(*__cache).cmaps.offset(id as isize);
    *fresh10 = cmap;
    id
}
/* Limits */
/*
 * TYPE_IDENTITY and TYPE_CID_TO_CODE is not defined in the CMap spec.
 */
/* ************************* CMAP_MAIN **************************/
/* charName not supported */
#[no_mangle]
pub unsafe fn CMap_cache_close() {
    if !__cache.is_null() {
        for id in 0..(*__cache).num {
            CMap_release(*(*__cache).cmaps.offset(id as isize));
        }
        free((*__cache).cmaps as *mut libc::c_void);
        __cache = mfree(__cache as *mut libc::c_void) as *mut CMap_cache
    };
}
