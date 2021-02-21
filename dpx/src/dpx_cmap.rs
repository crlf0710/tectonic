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
use crate::{info, warn};
use std::ptr;

use super::dpx_cid::CSI_IDENTITY;
use super::dpx_cmap_read::{CMap_parse, CMap_parse_check_sig};
use super::dpx_mem::{new, renew};
use libc::{free, memcmp, memcpy, memset};

use bridge::{size_t, InFile, TTInputFormat};

use super::dpx_cid::CIDSysInfo;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct rangeDef {
    pub(crate) dim: size_t,
    pub(crate) codeLo: *mut u8,
    pub(crate) codeHi: *mut u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct mapDef {
    pub(crate) flag: i32,
    pub(crate) len: size_t,
    pub(crate) code: *mut u8,
    pub(crate) next: *mut mapDef,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct mapData {
    pub(crate) data: *mut u8,
    pub(crate) prev: *mut mapData,
    pub(crate) pos: i32,
}
/* quasi-hack to get the primary input */
/* CID, Code... MEM_ALLOC_SIZE bytes  */
/* Previous mapData data segment      */
/* Position of next free data segment */
#[derive(Clone)]
pub(crate) struct CMap {
    pub(crate) name: String,
    pub(crate) type_0: i32,
    pub(crate) wmode: i32,
    pub(crate) CSI: *mut CIDSysInfo,
    pub(crate) useCMap: *mut CMap,
    pub(crate) codespace: C2RustUnnamed_0,
    pub(crate) mapTbl: *mut mapDef,
    pub(crate) mapData: *mut mapData,
    pub(crate) flags: i32,
    pub(crate) profile: C2RustUnnamed,
    pub(crate) reverseMap: *mut i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed {
    pub(crate) minBytesIn: size_t,
    pub(crate) maxBytesIn: size_t,
    pub(crate) minBytesOut: size_t,
    pub(crate) maxBytesOut: size_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) num: u32,
    pub(crate) max: u32,
    pub(crate) ranges: *mut rangeDef,
}
pub(crate) type CID = u16;

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
static mut __verbose: i32 = 0;
static mut __silent: i32 = 0;

pub(crate) unsafe fn CMap_set_verbose(level: i32) {
    __verbose = level;
}

pub(crate) unsafe fn CMap_set_silent(value: i32) {
    __silent = if value != 0 { 1 } else { 0 };
}

pub(crate) unsafe fn CMap_new() -> CMap {
    let profile = C2RustUnnamed {
        minBytesIn: 2,
        maxBytesIn: 2,
        minBytesOut: 2,
        maxBytesOut: 2,
    };
    let map_data = Box::into_raw(Box::new(mapData {
        prev: ptr::null_mut(),
        pos: 0,
        data: new((4096_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8,
    }));
    let reverse_map =
        new((65536_u64).wrapping_mul(::std::mem::size_of::<i32>() as u64) as u32) as *mut i32;
    memset(
        reverse_map as *mut libc::c_void,
        0,
        (65536usize).wrapping_mul(::std::mem::size_of::<i32>()),
    );

    CMap {
        profile,
        name: String::new(),
        type_0: 1,
        wmode: 0,
        useCMap: ptr::null_mut(),
        CSI: ptr::null_mut(),
        flags: 0,
        codespace: C2RustUnnamed_0 {
            num: 0,
            max: 10,
            ranges: new(10 * ::std::mem::size_of::<rangeDef>() as u32) as *mut rangeDef,
        },
        mapTbl: ptr::null_mut(),
        mapData: map_data,
        reverseMap: reverse_map,
    }
}

pub(crate) unsafe fn CMap_release(cmap: *mut CMap) {
    if cmap.is_null() {
        return;
    }
    (*cmap).name = String::new();
    if !(*cmap).CSI.is_null() {
        free((*cmap).CSI as *mut libc::c_void);
    }
    free((*cmap).codespace.ranges as *mut libc::c_void);
    if !(*cmap).mapTbl.is_null() {
        mapDef_release((*cmap).mapTbl);
    }
    let mut map: *mut mapData = (*cmap).mapData;
    while !map.is_null() {
        let prev: *mut mapData = (*map).prev;
        free((*map).data as *mut libc::c_void);
        free(map as *mut libc::c_void);
        map = prev
    }
    free((*cmap).reverseMap as *mut libc::c_void);
}

pub(crate) unsafe fn CMap_is_Identity(cmap: &CMap) -> bool {
    cmap.name == "Identity-H" || cmap.name == "Identity-V"
}

pub(crate) unsafe fn CMap_is_valid(cmap: *mut CMap) -> bool {
    /* Quick check */
    if cmap.is_null()
        || (*cmap).name.is_empty()
        || (*cmap).type_0 < 0
        || (*cmap).type_0 > 3
        || (*cmap).codespace.num < 1
        || (*cmap).type_0 != 0 && (*cmap).mapTbl.is_null()
    {
        return false;
    }
    if !(*cmap).useCMap.is_null() {
        let csi1 = CMap_get_CIDSysInfo(cmap);
        let csi2 = CMap_get_CIDSysInfo((*cmap).useCMap);
        if (*csi1).registry != (*csi2).registry || (*csi1).ordering != (*csi2).ordering {
            warn!(
                "CIDSystemInfo mismatched {} <--> {}",
                CMap_get_name(&*cmap),
                CMap_get_name(&*(*cmap).useCMap),
            );
            return false;
        }
    }
    true
}

pub(crate) unsafe fn CMap_get_profile(cmap: &CMap, type_0: i32) -> i32 {
    match type_0 {
        0 => (*cmap).profile.minBytesIn as i32,
        1 => (*cmap).profile.maxBytesIn as i32,
        2 => (*cmap).profile.maxBytesOut as i32,
        3 => (*cmap).profile.maxBytesOut as i32,
        _ => {
            panic!("{}: Unrecognized profile type {}.", "CMap", type_0);
        }
    }
}
/*
 * Put notdef chars for codes not declared in notdef(range|char)
 */
unsafe fn handle_undefined(
    cmap: &CMap,
    inbuf: &mut *const u8,
    inbytesleft: *mut size_t,
    outbuf: &mut *mut u8,
    outbytesleft: *mut size_t,
) {
    if *outbytesleft < 2 {
        panic!("{}: Buffer overflow.", "CMap");
    }
    match cmap.type_0 {
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
                cmap.type_0
            );
            warn!("<0000> is used for .notdef char.");
            memset(*outbuf as *mut libc::c_void, 0, 2);
        }
    }
    *outbuf = (*outbuf).offset(2);
    *outbytesleft = (*outbytesleft as u64).wrapping_sub(2) as size_t as size_t;
    let len = bytes_consumed(cmap, *inbuf, *inbytesleft);
    *inbuf = (*inbuf).offset(len as isize);
    *inbytesleft = (*inbytesleft as u64).wrapping_sub(len as _) as size_t as size_t;
}

pub(crate) unsafe fn CMap_decode_char(
    cmap: &CMap,
    inbuf: &mut *const u8,
    inbytesleft: *mut size_t,
    outbuf: &mut *mut u8,
    outbytesleft: *mut size_t,
) {
    let mut c: u8 = 0_u8;
    let mut count: size_t = 0;
    let mut save = *inbuf;
    let mut p = save as *const u8;
    /*
     * First handle some special cases:
     */
    if cmap.type_0 == 0 {
        if (*inbytesleft).wrapping_rem(2) != 0 {
            panic!("{}: Invalid/truncated input string.", "CMap");
        }
        if *outbytesleft < 2 {
            panic!("{}: Buffer overflow.", "CMap");
        }
        memcpy(
            *outbuf as *mut libc::c_void,
            *inbuf as *const libc::c_void,
            2,
        );
        *inbuf = (*inbuf).offset(2);
        *outbuf = (*outbuf).offset(2);
        *outbytesleft = (*outbytesleft as u64).wrapping_sub(2) as size_t as size_t;
        *inbytesleft = (*inbytesleft as u64).wrapping_sub(2) as size_t as size_t;
        return;
    } else {
        if cmap.mapTbl.is_null() {
            if let Some(useCMap) = cmap.useCMap.as_ref() {
                CMap_decode_char(useCMap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            } else {
                /* no mapping available in this CMap */
                warn!("No mapping available for this character.");
                handle_undefined(cmap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            }
        }
    }
    assert!(!cmap.mapTbl.is_null());
    let mut t = cmap.mapTbl;
    while count < *inbytesleft {
        c = *p;
        p = p.offset(1);
        count += 1;
        if (*t.offset(c as isize)).flag & 1 << 4 == 0 {
            break;
        }
        t = (*t.offset(c as isize)).next
    }
    if (*t.offset(c as isize)).flag & 1 << 4 != 0 {
        /* need more bytes */
        panic!("{}: Premature end of input string.", "CMap");
    } else {
        if if (*t.offset(c as isize)).flag & 0xf != 0 {
            1
        } else {
            0
        } == 0
        {
            if !cmap.useCMap.is_null() {
                CMap_decode_char(&*cmap.useCMap, inbuf, inbytesleft, outbuf, outbytesleft);
                return;
            } else {
                /* no mapping available in this CMap */
                warn!("No character mapping available.");
                info!(" CMap name: {}\n", CMap_get_name(&*cmap));
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
            match (*t.offset(c as isize)).flag & 0xf {
                8 => {
                    warn!("Character mapped to .notdef found.");
                }
                1 | 4 => {}
                2 => {
                    panic!("{}: CharName mapping not supported.", "CMap");
                }
                _ => {
                    panic!("{}: Unknown mapping type.", "CMap");
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
                panic!("{}: Buffer overflow.", "CMap");
            }
            *outbuf = (*outbuf).offset((*t.offset(c as isize)).len as isize);
            *outbytesleft = (*outbytesleft as u64).wrapping_sub((*t.offset(c as isize)).len as _)
                as size_t as size_t;
            if !inbytesleft.is_null() {
                *inbytesleft = (*inbytesleft as u64).wrapping_sub(count as _) as size_t as size_t
            }
            *inbuf = p
        }
    };
}
/*
 * For convenience, it does not do decoding to CIDs.
 */

pub(crate) unsafe fn CMap_decode(
    cmap: &CMap,
    inbuf: &mut *const u8,
    inbytesleft: *mut size_t,
    outbuf: &mut *mut u8,
    outbytesleft: *mut size_t,
) -> size_t {
    assert!(!inbuf.is_null() && !outbuf.is_null());
    assert!(!inbytesleft.is_null() && !outbytesleft.is_null());
    let mut count = 0;
    while *inbytesleft > 0 && *outbytesleft > 0 {
        CMap_decode_char(cmap, inbuf, inbytesleft, outbuf, outbytesleft);
        count += 1;
    }
    count
}

pub(crate) unsafe fn CMap_reverse_decode(cmap: &CMap, cid: CID) -> i32 {
    let ch: i32 = if !cmap.reverseMap.is_null() {
        *cmap.reverseMap.offset(cid as isize)
    } else {
        -1
    };
    if ch == 0 {
        if let Some(useCMap) = cmap.useCMap.as_ref() {
            return CMap_reverse_decode(useCMap, cid);
        }
    }
    ch
}

pub(crate) unsafe fn CMap_get_name(cmap: &CMap) -> &str {
    cmap.name.as_str()
}

pub(crate) unsafe fn CMap_get_type(cmap: &CMap) -> i32 {
    cmap.type_0
}

pub(crate) unsafe fn CMap_get_wmode(cmap: &CMap) -> i32 {
    cmap.wmode
}

pub(crate) unsafe fn CMap_get_CIDSysInfo(cmap: *mut CMap) -> *mut CIDSysInfo {
    assert!(!cmap.is_null());
    (*cmap).CSI
}

pub(crate) unsafe fn CMap_set_name(cmap: &mut CMap, name: &str) {
    cmap.name = name.to_string();
}

pub(crate) unsafe fn CMap_set_type(cmap: &mut CMap, type_0: i32) {
    cmap.type_0 = type_0;
}

pub(crate) unsafe fn CMap_set_wmode(cmap: &mut CMap, wmode: i32) {
    cmap.wmode = wmode;
}

pub(crate) unsafe fn CMap_set_CIDSysInfo(cmap: &mut CMap, csi: *const CIDSysInfo) {
    if !cmap.CSI.is_null() {
        free(cmap.CSI as *mut libc::c_void);
    }
    if !csi.is_null() && !(*csi).registry.is_empty() && !(*csi).ordering.is_empty() {
        cmap.CSI = Box::into_raw(Box::new((*csi).clone()));
    } else {
        warn!("Invalid CIDSystemInfo.");
        cmap.CSI = ptr::null_mut()
    };
}
/*
 * Can have muliple entry ?
 */

pub(crate) unsafe fn CMap_set_usecmap(cmap: &mut CMap, ucmap: *mut CMap) {
    /* Maybe if (!ucmap) panic! is better for this. */
    assert!(!ucmap.is_null());
    if (cmap as *mut CMap) == ucmap {
        panic!(
            "{}: Identical CMap object cannot be used for usecmap CMap: 0x{:p}=0x{:p}",
            "CMap", cmap, ucmap,
        );
    }
    /* Check if ucmap have neccesary information. */
    if !CMap_is_valid(ucmap) {
        panic!("{}: Invalid CMap.", "CMap");
    }
    /*
     *  CMapName of cmap can be undefined when usecmap is executed in CMap parsing.
     *  And it is also possible CSI is not defined at that time.
     */
    if cmap.name == (*ucmap).name {
        panic!(
            "{}: CMap refering itself not allowed: CMap {} --> {}",
            "CMap",
            cmap.name,
            (*ucmap).name,
        );
    }
    if !cmap.CSI.is_null() {
        if (*cmap.CSI).registry != (*(*ucmap).CSI).registry
            || (*cmap.CSI).ordering != (*(*ucmap).CSI).ordering
        {
            panic!(
                "CMap: CMap {} required by {} have different CSI.",
                CMap_get_name(&*cmap),
                CMap_get_name(&*ucmap),
            );
        }
    }
    /* We must copy codespaceranges. */
    for i in 0..(*ucmap).codespace.num {
        let csr: *mut rangeDef = (*ucmap).codespace.ranges.offset(i as isize);
        CMap_add_codespacerange(cmap, (*csr).codeLo, (*csr).codeHi, (*csr).dim);
    }
    cmap.useCMap = ucmap;
}
/* Test the validity of character c. */
unsafe fn CMap_match_codespace(cmap: *mut CMap, c: *const u8, dim: size_t) -> i32 {
    assert!(!cmap.is_null());
    for i in 0..(*cmap).codespace.num {
        let csr: *mut rangeDef = (*cmap).codespace.ranges.offset(i as isize);
        if !((*csr).dim != dim) {
            let mut pos = 0_u32;
            while (pos as u64) < dim as _ {
                if *c.offset(pos as isize) as i32 > *(*csr).codeHi.offset(pos as isize) as i32
                    || (*c.offset(pos as isize) as i32) < *(*csr).codeLo.offset(pos as isize) as i32
                {
                    break;
                }
                pos += 1;
            }
            if pos as u64 == dim as u64 {
                return 0;
            }
        }
        /* Valid */
    }
    return -1;
    /* Invalid */
}
/*
 * No overlapping codespace ranges are allowed, otherwise mapping is ambiguous.
 */

pub(crate) unsafe fn CMap_add_codespacerange(
    cmap: &mut CMap,
    codelo: *const u8,
    codehi: *const u8,
    dim: size_t,
) -> i32 {
    assert!(dim > 0);
    for i in 0..cmap.codespace.num {
        let mut overlap: bool = true;
        let csr = cmap.codespace.ranges.offset(i as isize);
        let mut j = 0 as size_t;
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
            return -1;
        }
    }
    if dim < cmap.profile.minBytesIn {
        cmap.profile.minBytesIn = dim
    }
    if dim > cmap.profile.maxBytesIn {
        cmap.profile.maxBytesIn = dim
    }
    if cmap.codespace.num.wrapping_add(1_u32) > cmap.codespace.max {
        cmap.codespace.max = cmap.codespace.max.wrapping_add(10_u32);
        cmap.codespace.ranges = renew(
            cmap.codespace.ranges as *mut libc::c_void,
            (cmap.codespace.max as u64).wrapping_mul(::std::mem::size_of::<rangeDef>() as u64)
                as u32,
        ) as *mut rangeDef
    }
    let csr = cmap.codespace.ranges.offset(cmap.codespace.num as isize);
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
    cmap.codespace.num += 1;
    0
}

pub(crate) unsafe fn CMap_add_notdefchar(
    cmap: &mut CMap,
    src: *const u8,
    srcdim: size_t,
    dst: CID,
) -> i32 {
    CMap_add_notdefrange(cmap, src, src, srcdim, dst)
}

pub(crate) unsafe fn CMap_add_notdefrange(
    cmap: &mut CMap,
    srclo: *const u8,
    srchi: *const u8,
    srcdim: size_t,
    mut dst: CID,
) -> i32 {
    /* dst not used here */
    /* FIXME */
    if check_range(
        cmap,
        srclo,
        srchi,
        srcdim,
        &mut dst as *mut CID as *const u8,
        2 as size_t,
    ) < 0
    {
        return -1;
    }
    if cmap.mapTbl.is_null() {
        cmap.mapTbl = mapDef_new()
    }
    let mut cur = cmap.mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0 {
        return -1;
    }
    for c in *srclo.offset(srcdim.wrapping_sub(1) as isize) as i32
        ..=*srchi.offset(srcdim.wrapping_sub(1) as isize) as i32
    {
        if if (*cur.offset(c as isize)).flag & 0xf != 0 {
            1
        } else {
            0
        } != 0
        {
            if __silent == 0 {
                warn!("Trying to redefine already defined code mapping. (ignored)");
            }
        } else {
            (*cur.offset(c as isize)).flag = 0 | 1 << 3;
            (*cur.offset(c as isize)).code = get_mem(cmap, 2);
            (*cur.offset(c as isize)).len = 2 as size_t;
            *(*cur.offset(c as isize)).code.offset(0) = (dst as i32 >> 8) as u8;
            *(*cur.offset(c as isize)).code.offset(1) = (dst as i32 & 0xff) as u8
        }
        /* Do not do dst++ for notdefrange  */
    }
    0
}

pub(crate) unsafe fn CMap_add_bfchar(
    cmap: &mut CMap,
    src: *const u8,
    srcdim: size_t,
    dst: *const u8,
    dstdim: size_t,
) -> i32 {
    CMap_add_bfrange(cmap, src, src, srcdim, dst, dstdim)
}

pub(crate) unsafe fn CMap_add_bfrange(
    cmap: &mut CMap,
    srclo: *const u8,
    srchi: *const u8,
    srcdim: size_t,
    base: *const u8,
    dstdim: size_t,
) -> i32 {
    if check_range(cmap, srclo, srchi, srcdim, base, dstdim) < 0 {
        return -1;
    }
    if cmap.mapTbl.is_null() {
        cmap.mapTbl = mapDef_new()
    }
    let mut cur = cmap.mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0 {
        return -1;
    }
    for c in *srclo.offset(srcdim.wrapping_sub(1) as isize) as i32
        ..=*srchi.offset(srcdim.wrapping_sub(1) as isize) as i32
    {
        /* According to 5014.CIDFont_Spec.pdf (p.52),
         * Code mappings (unlike codespace ranges) may overlap,
         * but succeeding maps superceded preceding maps.
         * (reported and patched by Luo Jie on 2007/12/2)
         */
        if (if (*cur.offset(c as isize)).flag & 0xf != 0 {
            1
        } else {
            0
        }) == 0
            || (*cur.offset(c as isize)).len < dstdim
        {
            (*cur.offset(c as isize)).flag = 0 | 1 << 2;
            (*cur.offset(c as isize)).code = get_mem(cmap, dstdim as i32)
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
        let mut last_byte = c - *srclo.offset(srcdim.wrapping_sub(1) as isize) as i32
            + *base.offset(dstdim.wrapping_sub(1) as isize) as i32;
        *(*cur.offset(c as isize))
            .code
            .offset(dstdim.wrapping_sub(1) as isize) = (last_byte & 0xff) as u8;
        let mut i = dstdim.wrapping_sub(2) as i32;
        while i >= 0 && last_byte > 255 {
            last_byte = *(*cur.offset(c as isize)).code.offset(i as isize) as i32 + 1;
            *(*cur.offset(c as isize)).code.offset(i as isize) = (last_byte & 0xff) as u8;
            i -= 1
        }
    }
    0
}

pub(crate) unsafe fn CMap_add_cidchar(
    cmap: &mut CMap,
    src: *const u8,
    srcdim: size_t,
    dst: CID,
) -> i32 {
    CMap_add_cidrange(cmap, src, src, srcdim, dst)
}

pub(crate) unsafe fn CMap_add_cidrange(
    cmap: &mut CMap,
    srclo: *const u8,
    srchi: *const u8,
    srcdim: size_t,
    mut base: CID,
) -> i32 {
    /* base not used here */
    if check_range(
        cmap,
        srclo,
        srchi,
        srcdim,
        &mut base as *mut CID as *const u8,
        2 as size_t,
    ) < 0
    {
        /* FIXME */
        return -1;
    }
    if cmap.mapTbl.is_null() {
        cmap.mapTbl = mapDef_new()
    }
    let mut cur = cmap.mapTbl;
    if locate_tbl(&mut cur, srclo, srcdim as i32) < 0 {
        return -1;
    }
    let mut v = 0 as size_t;
    for i in 0..srcdim.wrapping_sub(1) {
        v = (v << 8).wrapping_add(*srclo.offset(i as isize) as size_t);
    }
    *cmap.reverseMap.offset(base as isize) = v as i32;
    for c in (*srclo.offset(srcdim.wrapping_sub(1) as isize) as u64)
        ..=*srchi.offset(srcdim.wrapping_sub(1) as isize) as u64
    {
        if (*cur.offset(c as isize)).flag != 0 {
            if __silent == 0 {
                warn!("Trying to redefine already defined CID mapping. (ignored)");
            }
        } else {
            (*cur.offset(c as isize)).flag = 0 | 1 << 0;
            (*cur.offset(c as isize)).len = 2 as size_t;
            (*cur.offset(c as isize)).code = get_mem(cmap, 2);
            *(*cur.offset(c as isize)).code.offset(0) = (base as i32 >> 8) as u8;
            *(*cur.offset(c as isize)).code.offset(1) = (base as i32 & 0xff) as u8;
            *cmap.reverseMap.offset(base as isize) = (v << 8).wrapping_add(c as _) as i32
        }
        if base as i32 >= 65535 {
            warn!("CID number too large.");
        }
        base += 1;
    }
    0
}
unsafe fn mapDef_release(t: *mut mapDef) {
    assert!(!t.is_null());
    for c in 0..256 {
        if (*t.offset(c as isize)).flag & 1 << 4 != 0 {
            mapDef_release((*t.offset(c as isize)).next);
        }
    }
    free(t as *mut libc::c_void);
}
unsafe fn mapDef_new() -> *mut mapDef {
    let t =
        new((256_u64).wrapping_mul(::std::mem::size_of::<mapDef>() as u64) as u32) as *mut mapDef;
    for c in 0..256 {
        (*t.offset(c as isize)).flag = 0 | 0;
        (*t.offset(c as isize)).code = ptr::null_mut();
        (*t.offset(c as isize)).next = ptr::null_mut();
    }
    t
}
unsafe fn get_mem(cmap: &mut CMap, size: i32) -> *mut u8 {
    assert!(!cmap.mapData.is_null() && size >= 0);
    let mut map = cmap.mapData;
    if (*map).pos + size >= 4096 {
        let prev: *mut mapData = map;
        map = new((1_u64).wrapping_mul(::std::mem::size_of::<mapData>() as u64) as u32)
            as *mut mapData;
        (*map).data =
            new((4096_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
        (*map).prev = prev;
        (*map).pos = 0;
        cmap.mapData = map
    }
    let p = (*map).data.offset((*map).pos as isize);
    (*map).pos += size;
    p
}
unsafe fn locate_tbl(cur: *mut *mut mapDef, code: *const u8, dim: i32) -> i32 {
    assert!(!cur.is_null() && !(*cur).is_null());
    for i in 0..(dim - 1) {
        let c = *code.offset(i as isize) as i32;
        if if (*(*cur).offset(c as isize)).flag & 0xf != 0 {
            1
        } else {
            0
        } != 0
        {
            warn!("Ambiguous CMap entry.");
            return -1;
        }
        if (*(*cur).offset(c as isize)).next.is_null() {
            /* create new node */
            (*(*cur).offset(c as isize)).next = mapDef_new();
        }
        (*(*cur).offset(c as isize)).flag |= 1 << 4;
        *cur = (*(*cur).offset(c as isize)).next;
    }
    0
}
/* Private funcs. */
/*
 * Guess how many bytes consumed as a `single' character:
 * Substring of length bytesconsumed bytes of input string is interpreted as
 * a `single' character by CMap_decode().
 */
unsafe fn bytes_consumed(cmap: &CMap, instr: *const u8, inbytes: size_t) -> size_t {
    let mut i = 0 as size_t;
    let mut longest: size_t = 0 as size_t;
    let mut bytesconsumed;
    while i < cmap.codespace.num as _ {
        let csr: *mut rangeDef = cmap.codespace.ranges.offset(i as isize);
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
    if i == cmap.codespace.num as _ {
        /* No matching at all */
        bytesconsumed = cmap.profile.minBytesIn
    } else {
        bytesconsumed = cmap.profile.maxBytesIn;
        for i in 0..cmap.codespace.num as u64 {
            let csr_0: *mut rangeDef = cmap.codespace.ranges.offset(i as isize);
            if (*csr_0).dim > longest && (*csr_0).dim < bytesconsumed {
                bytesconsumed = (*csr_0).dim
            }
        }
    }
    bytesconsumed
}
unsafe fn check_range(
    cmap: &mut CMap,
    srclo: *const u8,
    srchi: *const u8,
    srcdim: size_t,
    dst: *const u8,
    dstdim: size_t,
) -> i32 {
    if srcdim < 1
        || dstdim < 1
        || (srclo.is_null() || srchi.is_null() || dst.is_null())
        || memcmp(
            srclo as *const libc::c_void,
            srchi as *const libc::c_void,
            srcdim.wrapping_sub(1) as _,
        ) != 0
        || *srclo.offset(srcdim.wrapping_sub(1) as isize) as i32
            > *srchi.offset(srcdim.wrapping_sub(1) as isize) as i32
    {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1;
    }
    if CMap_match_codespace(cmap, srclo, srcdim) < 0
        || CMap_match_codespace(cmap, srchi, srcdim) < 0
    {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1;
    }
    cmap.profile.minBytesIn = cmap.profile.minBytesIn.min(srcdim);
    cmap.profile.maxBytesIn = cmap.profile.maxBytesIn.max(srcdim);
    cmap.profile.minBytesOut = cmap.profile.minBytesOut.min(dstdim);
    cmap.profile.maxBytesOut = cmap.profile.maxBytesOut.max(dstdim);
    0
}

// Note: The elements are boxed to be able
// to get stable pointers to the cached data.
// (CMap_cache_get returns *mut CMap)
static mut __cache: Vec<Box<CMap>> = Vec::new();

pub(crate) unsafe fn CMap_cache_init() {
    static mut range_min: [u8; 2] = [0; 2];
    static mut range_max: [u8; 2] = [0xff_u8, 0xff_u8];
    __cache.clear();
    /* Create Identity mapping */
    __cache.push(Box::new(CMap_new()));
    let cmap = &mut *__cache[0];
    CMap_set_name(cmap, "Identity-H");
    CMap_set_type(cmap, 0);
    CMap_set_wmode(cmap, 0);
    CMap_set_CIDSysInfo(cmap, &mut CSI_IDENTITY);
    CMap_add_codespacerange(cmap, range_min.as_ptr(), range_max.as_ptr(), 2 as size_t);

    __cache.push(Box::new(CMap_new()));
    let cmap = &mut *__cache[1];
    CMap_set_name(cmap, "Identity-V");
    CMap_set_type(cmap, 0);
    CMap_set_wmode(cmap, 1);
    CMap_set_CIDSysInfo(cmap, &mut CSI_IDENTITY);
    CMap_add_codespacerange(cmap, range_min.as_ptr(), range_max.as_ptr(), 2 as size_t);
}

pub(crate) unsafe fn CMap_cache_get(id: i32) -> *mut CMap {
    if id < 0 || id >= __cache.len() as i32 {
        panic!("Invalid CMap ID {}", id);
    }
    &mut *__cache[id as usize]
}

pub(crate) unsafe fn CMap_cache_find(cmap_name: &str) -> i32 {
    for (id, cmap) in __cache.iter_mut().enumerate() {
        /* CMapName may be undefined when processing usecmap. */
        let name = CMap_get_name(&mut **cmap);
        if !name.is_empty() && cmap_name == name {
            return id as i32;
        }
    }
    if let Some(handle) = InFile::open(cmap_name, TTInputFormat::CMAP, 0) {
        if CMap_parse_check_sig(&mut &handle) < 0 {
            return -1;
        }
        if __verbose != 0 {
            info!("(CMap:{}", cmap_name);
        }

        let id = (*__cache).len();

        __cache.push(Box::new(CMap_new()));
        if CMap_parse(&mut *__cache[id], handle).is_err() {
            panic!("{}: Parsing CMap file failed.", "CMap");
        }
        if __verbose != 0 {
            info!(")");
        }
        id as i32
    } else {
        -1
    }
}

pub(crate) unsafe fn CMap_cache_add(mut cmap: Box<CMap>) -> i32 {
    if !CMap_is_valid(&mut *cmap) {
        panic!("{}: Invalid CMap.", "CMap");
    }
    for other in &mut __cache {
        let cmap_name0 = CMap_get_name(&*cmap);
        let cmap_name1 = CMap_get_name(&**other);
        if cmap_name0 == cmap_name1 {
            panic!("{}: CMap \"{}\" already defined.", "CMap", cmap_name0);
        }
    }

    __cache.push(cmap);
    __cache.len() as i32 - 1
}
/* Limits */
/*
 * TYPE_IDENTITY and TYPE_CID_TO_CODE is not defined in the CMap spec.
 */
/* ************************* CMAP_MAIN **************************/
/* charName not supported */

pub(crate) unsafe fn CMap_cache_close() {
    for cmap in &mut __cache {
        CMap_release(&mut **cmap);
    }
    __cache.clear();
}
