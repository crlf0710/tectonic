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

use std::io::Read;

use super::dpx_sfnt::{
    dfont_open, sfnt_find_table_pos, sfnt_locate_table, sfnt_open, sfnt_read_table_directory,
};
use crate::{info, warn};
use std::ptr;

use super::dpx_agl::agl_get_unicodes;
use super::dpx_cff::{
    cff_charsets_lookup_inverse, cff_get_glyphname, cff_get_string, cff_open, cff_read_charsets,
};
use super::dpx_cid::{CSI_IDENTITY, CSI_UNICODE};
use super::dpx_cmap::{
    CMap, CMap_cache_add, CMap_cache_find, CMap_cache_get, CMap_decode, CMap_reverse_decode,
    CMap_set_silent,
};
use super::dpx_cmap_write::CMap_create_stream;
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_opentype_file, dpx_open_truetype_file};
use super::dpx_mem::new;
use super::dpx_numbers::GetFromFile;
use super::dpx_pdfresource::{pdf_defineresource, pdf_findresource, pdf_get_resource_reference};
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_gsub::{
    otl_gsub, otl_gsub_add_feat, otl_gsub_add_feat_list, otl_gsub_apply, otl_gsub_apply_chain,
    otl_gsub_new, otl_gsub_release, otl_gsub_select, otl_gsub_set_chain, otl_gsub_set_verbose,
};
use super::dpx_tt_post::{tt_get_glyphname, tt_read_post_table, tt_release_post_table};
use super::dpx_tt_table::tt_read_maxp_table;
use super::dpx_unicode::UC_UTF16BE_encode_char;
use crate::dpx_pdfobj::{pdf_obj, pdf_stream};
use crate::dpx_truetype::sfnt_table_info;
use libc::free;

use std::ffi::CString;
use std::io::{Seek, SeekFrom};

use super::dpx_sfnt::sfnt;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_cmap {
    pub(crate) format: u16,
    pub(crate) platform: u16,
    pub(crate) encoding: u16,
    pub(crate) language: u32,
    pub(crate) map: *mut libc::c_void,
}

use super::dpx_cid::CIDSysInfo;

use super::dpx_cff::{cff_font, Charsets};

use super::dpx_tt_post::tt_post_table;

#[derive(Clone)]
#[repr(C)]
pub(crate) struct cmap12 {
    pub(crate) groups: Box<[charGroup]>,
}
/* Format 8 and 10 not supported...
 *
 *  format  8: mixed 16-bit and 32-bit coverage
 *  format 10: trimmed array
 */
/*
 * format 12: segmented coverage
 *
 * startGlyphID is 32-bit long, however, GlyphID is still 16-bit long !
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct charGroup {
    pub(crate) startCharCode: u32,
    pub(crate) endCharCode: u32,
    pub(crate) startGlyphID: u32,
}
/* format 6: trimmed table mapping */
#[derive(Clone)]
#[repr(C)]
pub(crate) struct cmap6 {
    pub(crate) firstCode: u16,
    pub(crate) glyphIndexArray: Box<[u16]>,
}
/*
 * format 4: segment mapping to delta values
 * - Microsoft standard character to glyph index mapping table
 */
#[derive(Clone)]
#[repr(C)]
pub(crate) struct cmap4 {
    pub(crate) segCountX2: u16,
    pub(crate) searchRange: u16,
    pub(crate) entrySelector: u16,
    pub(crate) rangeShift: u16,
    pub(crate) endCount: Box<[u16]>,
    pub(crate) reservedPad: u16,
    pub(crate) startCount: Box<[u16]>,
    pub(crate) idDelta: Box<[u16]>,
    pub(crate) idRangeOffset: Box<[u16]>,
    pub(crate) glyphIndexArray: Box<[u16]>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct cmap2 {
    pub(crate) subHeaderKeys: [u16; 256],
    pub(crate) subHeaders: Box<[SubHeader]>,
    pub(crate) glyphIndexArray: Box<[u16]>,
}
/* format 2: high-byte mapping through table */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct SubHeader {
    pub(crate) firstCode: u16,
    pub(crate) entryCount: u16,
    pub(crate) idDelta: i16,
    pub(crate) idRangeOffset: u16,
}
/* format 0: byte encoding table */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cmap0 {
    pub(crate) glyphIndexArray: [u8; 256],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cmap_plat_enc_rec {
    pub(crate) platform: i16,
    pub(crate) encoding: i16,
}
static mut verbose: i32 = 0;

pub(crate) unsafe fn otf_cmap_set_verbose(level: i32) {
    otl_gsub_set_verbose(level);
    verbose = level;
}
unsafe fn read_cmap0<R: Read>(handle: &mut R, len: u32) -> *mut cmap0 {
    if len < 256 {
        panic!("invalid cmap subtable");
    }
    let mut glyphIndexArray = [0; 256];
    for i in 0..256 {
        glyphIndexArray[i] = u8::get(handle);
    }
    Box::into_raw(Box::new(cmap0 { glyphIndexArray }))
}
unsafe fn release_cmap0(map: *mut cmap0) {
    if !map.is_null() {
        let _ = Box::from_raw(map);
    }
}
unsafe fn lookup_cmap0(map: &cmap0, cc: u16) -> u16 {
    return (if cc as i32 > 255 {
        0
    } else {
        map.glyphIndexArray[cc as usize] as i32
    }) as u16;
}
unsafe fn read_cmap2<R: Read>(handle: &mut R, len: u32) -> *mut cmap2 {
    if len < 512 {
        panic!("invalid cmap subtable");
    }
    let mut subHeaderKeys = [0; 256];
    for i in 0..256 {
        subHeaderKeys[i] = u16::get(handle);
    }
    let mut n = 0_u16;
    for i in 0..256 {
        subHeaderKeys[i] = (subHeaderKeys[i] as i32 / 8) as u16;
        if n < subHeaderKeys[i] {
            n = subHeaderKeys[i];
        }
    }
    n += 1;
    let mut subHeaders = Vec::<SubHeader>::with_capacity(n as _);
    for i in 0..n as i32 {
        let mut sh = SubHeader {
            firstCode: u16::get(handle),
            entryCount: u16::get(handle),
            idDelta: i16::get(handle),
            idRangeOffset: u16::get(handle),
        };
        /* It makes things easier to let the offset starts from
         * the beginning of glyphIndexArray.
         */
        if sh.idRangeOffset != 0 {
            sh.idRangeOffset -= (2 + (n as i32 - i as i32 - 1) * 8) as u16
        }
        subHeaders.push(sh);
    }
    /* Caculate the length of glyphIndexArray, this is ugly,
     * there should be a better way to get this information.
     */
    let n = (len
        .wrapping_sub(518_u32)
        .wrapping_sub((n as i32 * 8) as u32) as u16 as i32
        / 2) as u16;
    let mut glyphIndexArray = Vec::<u16>::with_capacity(n as _);
    for _ in 0..n as i32 {
        glyphIndexArray.push(u16::get(handle));
    }
    Box::into_raw(Box::new(cmap2 {
        subHeaderKeys,
        subHeaders: subHeaders.into_boxed_slice(),
        glyphIndexArray: glyphIndexArray.into_boxed_slice(),
    }))
}
unsafe fn release_cmap2(map: *mut cmap2) {
    if !map.is_null() {
        let _ = Box::from_raw(map);
    }
}
unsafe fn lookup_cmap2(map: &cmap2, cc: u16) -> u16 {
    let mut idx: u16 = 0_u16;
    let [hi, lo] = cc.to_be_bytes();
    let hi = hi as i32;
    let lo = lo as i32;
    /* select which subHeader to use */
    let i = map.subHeaderKeys[hi as usize] as usize;
    let firstCode = map.subHeaders[i].firstCode;
    let entryCount = map.subHeaders[i].entryCount;
    let idDelta = map.subHeaders[i].idDelta;
    let mut idRangeOffset = (map.subHeaders[i].idRangeOffset as i32 / 2) as u16;
    if lo >= firstCode as i32 && lo < firstCode as i32 + entryCount as i32 {
        idRangeOffset = (idRangeOffset as i32 + (lo - firstCode as i32)) as u16;
        idx = map.glyphIndexArray[idRangeOffset as usize];
        if idx as i32 != 0 {
            idx = (idx as i32 + idDelta as i32 & 0xffff) as u16
        }
    }
    idx
}
unsafe fn read_cmap4<R: Read>(handle: &mut R, len: u32) -> *mut cmap4 {
    if len < 8 {
        panic!("invalid cmap subtable");
    }
    let segCount = u16::get(handle);
    let segCountX2 = segCount;
    let searchRange = u16::get(handle);
    let entrySelector = u16::get(handle);
    let rangeShift = u16::get(handle);
    let segCount = (segCount as i32 / 2) as u16;
    let mut endCount = Vec::with_capacity(segCount as _);
    for _ in 0..segCount {
        endCount.push(u16::get(handle));
    }
    let reservedPad = u16::get(handle);
    let mut startCount = Vec::with_capacity(segCount as _);
    for _ in 0..segCount {
        startCount.push(u16::get(handle));
    }
    let mut idDelta = Vec::with_capacity(segCount as _);
    for _ in 0..segCount {
        idDelta.push(u16::get(handle));
    }
    let mut idRangeOffset = Vec::with_capacity(segCount as _);
    for _ in 0..segCount {
        idRangeOffset.push(u16::get(handle));
    }
    let n = len
        .wrapping_sub(16_u32)
        .wrapping_sub((8 * segCount as i32) as u32)
        .wrapping_div(2_u32) as u16;
    let mut glyphIndexArray;
    if n as i32 == 0 {
        glyphIndexArray = Vec::new();
    } else {
        glyphIndexArray = Vec::with_capacity(n as _);
        for _ in 0..n {
            glyphIndexArray.push(u16::get(handle));
        }
    }
    Box::into_raw(Box::new(cmap4 {
        segCountX2,
        searchRange,
        entrySelector,
        rangeShift,
        endCount: endCount.into_boxed_slice(),
        reservedPad,
        startCount: startCount.into_boxed_slice(),
        idDelta: idDelta.into_boxed_slice(),
        idRangeOffset: idRangeOffset.into_boxed_slice(),
        glyphIndexArray: glyphIndexArray.into_boxed_slice(),
    }))
}
unsafe fn release_cmap4(map: *mut cmap4) {
    if !map.is_null() {
        let _ = Box::from_raw(map);
    }
}
unsafe fn lookup_cmap4(map: &cmap4, cc: u16) -> u16 {
    let mut gid: u16 = 0_u16;
    /*
     * Segments are sorted in order of increasing endCode values.
     * Last segment maps 0xffff to gid 0 (?)
     */
    let segCount = (map.segCountX2 as i32 / 2) as u16;
    for i in (0..segCount as usize).rev() {
        if !(cc as i32 <= map.endCount[i] as i32) {
            break;
        }
        if !(cc as i32 >= map.startCount[i] as i32) {
            continue;
        }
        if map.idRangeOffset[i] as i32 == 0 {
            gid = (cc as i32 + map.idDelta[i] as i32 & 0xffff) as u16
        } else if cc as i32 == 0xffff && map.idRangeOffset[i] as i32 == 0xffff {
            /* this is for protection against some old broken fonts... */
            gid = 0_u16
        } else {
            let j = (map.idRangeOffset[i] as i32 - (segCount as i32 - i as i32) * 2) as u16;
            let j = (cc as i32 - map.startCount[i] as i32 + j as i32 / 2) as usize;
            gid = map.glyphIndexArray[j];
            if gid != 0 {
                gid = (gid as i32 + map.idDelta[i] as i32 & 0xffff) as u16
            }
        }
        break;
    }
    gid
}
unsafe fn read_cmap6<R: Read>(handle: &mut R, len: u32) -> *mut cmap6 {
    if len < 4_u32 {
        panic!("invalid cmap subtable");
    }
    let firstCode = u16::get(handle);
    let entryCount = u16::get(handle);
    let mut glyphIndexArray = Vec::with_capacity(entryCount as _);
    for _ in 0..entryCount {
        glyphIndexArray.push(u16::get(handle));
    }
    Box::into_raw(Box::new(cmap6 {
        firstCode,
        glyphIndexArray: glyphIndexArray.into_boxed_slice(),
    }))
}
unsafe fn release_cmap6(map: *mut cmap6) {
    if !map.is_null() {
        let _ = Box::from_raw(map);
    }
}
unsafe fn lookup_cmap6(map: &cmap6, cc: u16) -> u16 {
    let idx = (cc as i32 - map.firstCode as i32) as u16;
    if (idx as i32) < map.glyphIndexArray.len() as i32 {
        return map.glyphIndexArray[idx as usize];
    }
    0
}
/* ULONG length */
unsafe fn read_cmap12<R: Read>(handle: &mut R, len: u32) -> *mut cmap12 {
    if len < 4_u32 {
        panic!("invalid cmap subtable");
    }
    let nGroups = u32::get(handle);
    let mut groups = Vec::with_capacity(nGroups as _);
    for _ in 0..nGroups {
        groups.push(charGroup {
            startCharCode: u32::get(handle),
            endCharCode: u32::get(handle),
            startGlyphID: u32::get(handle),
        });
    }
    Box::into_raw(Box::new(cmap12 {
        groups: groups.into_boxed_slice(),
    }))
}
unsafe fn release_cmap12(map: *mut cmap12) {
    if !map.is_null() {
        let _ = Box::from_raw(map);
    }
}
unsafe fn lookup_cmap12(map: &cmap12, cccc: u32) -> u16 {
    let mut gid: u16 = 0_u16;
    for group in map.groups.iter().rev() {
        if !(cccc <= group.endCharCode) {
            break;
        }
        if !(cccc >= group.startCharCode) {
            continue;
        }
        gid = ((cccc - group.startCharCode + group.startGlyphID) & 0xffff_u32) as u16;
        break;
    }
    gid
}
/* read cmap */

pub(crate) unsafe fn tt_cmap_read(sfont: &sfnt, platform: u16, encoding: u16) -> *mut tt_cmap {
    let length;
    let mut offset = sfnt_locate_table(sfont, sfnt_table_info::CMAP);
    let handle = &mut &*sfont.handle;
    u16::get(handle);
    let n_subtabs = u16::get(handle);
    let mut i = 0_u16;
    while (i as i32) < n_subtabs as i32 {
        let p_id = u16::get(handle);
        let e_id = u16::get(handle);
        if p_id as i32 != platform as i32 || e_id as i32 != encoding as i32 {
            u32::get(handle);
            i += 1;
        } else {
            offset = (offset as u32).wrapping_add(u32::get(handle)) as u32 as u32;
            break;
        }
    }
    if i as i32 == n_subtabs as i32 {
        return ptr::null_mut();
    }
    let mut cmap =
        new((1_u64).wrapping_mul(::std::mem::size_of::<tt_cmap>() as u64) as u32) as *mut tt_cmap;
    (*cmap).map = ptr::null_mut();
    (*cmap).platform = platform;
    (*cmap).encoding = encoding;
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    (*cmap).format = u16::get(handle);
    /* Length and version (language) is ULONG for
     * format 8, 10, 12 !
     */
    if (*cmap).format as i32 <= 6 {
        length = u16::get(handle) as u32;
        (*cmap).language = u16::get(handle) as u32
    /* language (Mac) */
    } else if u16::get(handle) as i32 != 0 {
        /* reverved - 0 */
        warn!("Unrecognized cmap subtable format.");
        tt_cmap_release(cmap);
        return ptr::null_mut();
    } else {
        length = u32::get(handle);
        (*cmap).language = u32::get(handle)
    }
    match (*cmap).format as i32 {
        0 => (*cmap).map = read_cmap0(handle, length) as *mut libc::c_void,
        2 => (*cmap).map = read_cmap2(handle, length) as *mut libc::c_void,
        4 => (*cmap).map = read_cmap4(handle, length) as *mut libc::c_void,
        6 => (*cmap).map = read_cmap6(handle, length) as *mut libc::c_void,
        12 => {
            /* warn!("UCS-4 TrueType cmap table..."); */
            (*cmap).map = read_cmap12(handle, length) as *mut libc::c_void
        }
        _ => {
            warn!("Unrecognized OpenType/TrueType cmap format.");
            tt_cmap_release(cmap);
            return ptr::null_mut();
        }
    }
    if (*cmap).map.is_null() {
        tt_cmap_release(cmap);
        cmap = ptr::null_mut()
    }
    cmap
}

pub(crate) unsafe fn tt_cmap_release(cmap: *mut tt_cmap) {
    if !cmap.is_null() {
        if !(*cmap).map.is_null() {
            match (*cmap).format as i32 {
                0 => {
                    release_cmap0((*cmap).map as *mut cmap0);
                }
                2 => {
                    release_cmap2((*cmap).map as *mut cmap2);
                }
                4 => {
                    release_cmap4((*cmap).map as *mut cmap4);
                }
                6 => {
                    release_cmap6((*cmap).map as *mut cmap6);
                }
                12 => {
                    release_cmap12((*cmap).map as *mut cmap12);
                }
                _ => {
                    panic!("Unrecognized OpenType/TrueType cmap format.");
                }
            }
        }
        free(cmap as *mut libc::c_void);
    };
}

pub(crate) unsafe fn tt_cmap_lookup(cmap: *mut tt_cmap, cc: u32) -> u16 {
    assert!(!cmap.is_null());
    if cc as i64 > 0xffff && ((*cmap).format as i32) < 12 {
        warn!("Four bytes charcode not supported in OpenType/TrueType cmap format 0...6.");
        return 0_u16;
    }
    match (*cmap).format as i32 {
        0 => lookup_cmap0(&*((*cmap).map as *mut cmap0), cc as u16),
        2 => lookup_cmap2(&*((*cmap).map as *mut cmap2), cc as u16),
        4 => lookup_cmap4(&*((*cmap).map as *mut cmap4), cc as u16),
        6 => lookup_cmap6(&*((*cmap).map as *mut cmap6), cc as u16),
        12 => lookup_cmap12(&*((*cmap).map as *mut cmap12), cc),
        _ => {
            panic!("Unrecognized OpenType/TrueType cmap subtable format");
        }
    }
}
static mut wbuf: [u8; 1024] = [0; 1024];
static mut srange_min: [u8; 2] = [0; 2];
static mut srange_max: [u8; 2] = [0xff_u8, 0xff_u8];
static mut lrange_min: [u8; 4] = [0; 4];
static mut lrange_max: [u8; 4] = [0x7f_u8, 0xff_u8, 0xff_u8, 0xff_u8];
unsafe fn load_cmap4(
    map: &cmap4,
    GIDToCIDMap: &[u8],
    gsub_vert: *mut otl_gsub,
    gsub_list: *mut otl_gsub,
    cmap: &mut CMap,
    tounicode_add: *mut CMap,
) {
    let mut cid;
    let segCount = (map.segCountX2 as i32 / 2) as u16;
    for i in (0..segCount as usize).rev() {
        let c0 = map.startCount[i];
        let c1 = map.endCount[i];
        let d = (map.idRangeOffset[i] as i32 / 2 - (segCount as i32 - i as i32)) as u16;
        let mut j = 0_u16;
        while j as i32 <= c1 as i32 - c0 as i32 {
            let ch = (c0 as i32 + j as i32) as u16;
            let mut gid = if map.idRangeOffset[i] as i32 == 0 {
                (ch as i32 + map.idDelta[i] as i32 & 0xffff) as u16
            } else if c0 as i32 == 0xffff
                && c1 as i32 == 0xffff
                && map.idRangeOffset[i] as i32 == 0xffff
            {
                /* this is for protection against some old broken fonts... */
                0
            } else {
                (map.glyphIndexArray[(j as i32 + d as i32) as usize] as i32 + map.idDelta[i] as i32
                    & 0xffff) as u16
            }; /* LONG ? */
            if gid as i32 != 0 && gid as i32 != 0xffff {
                if !gsub_list.is_null() {
                    otl_gsub_apply_chain(&*gsub_list, &mut gid);
                }
                if !gsub_vert.is_null() {
                    otl_gsub_apply(gsub_vert, &mut gid);
                }
                if !GIDToCIDMap.is_empty() {
                    cid = ((GIDToCIDMap[2 * gid as usize] as i32) << 8
                        | GIDToCIDMap[2 * gid as usize + 1] as i32)
                        as u16;
                    if cid as i32 == 0 {
                        warn!("GID {} does not have corresponding CID {}.", gid, cid);
                    }
                } else {
                    cid = gid
                }
                wbuf[0] = 0_u8;
                wbuf[1] = 0_u8;
                wbuf[2..4].copy_from_slice(&ch.to_be_bytes());
                wbuf[4..6].copy_from_slice(&cid.to_be_bytes());
                cmap.add_cidchar(&wbuf[..4], cid);
                if let Some(tounicode_add) = tounicode_add.as_mut() {
                    let mut p: *mut u8 = wbuf.as_mut_ptr().offset(6);
                    let uc_len = UC_UTF16BE_encode_char(
                        ch as i32,
                        &mut p,
                        wbuf.as_mut_ptr().offset(1024).offset(-1),
                    );
                    tounicode_add.add_bfchar(&wbuf[4..6], &wbuf[6..6 + uc_len]);
                }
            }
            j += 1;
        }
    }
}
unsafe fn load_cmap12(
    map: &cmap12,
    GIDToCIDMap: &[u8],
    gsub_vert: *mut otl_gsub,
    gsub_list: *mut otl_gsub,
    cmap: &mut CMap,
    tounicode_add: *mut CMap,
) {
    let mut cid;
    for group in map.groups.iter() {
        for ch in group.startCharCode..=group.endCharCode {
            let d: i32 = (ch - group.startCharCode) as i32;
            let mut gid = (group.startGlyphID.wrapping_add(d as u32) & 0xffff_u32) as u16;
            if !gsub_list.is_null() {
                otl_gsub_apply_chain(&*gsub_list, &mut gid);
            }
            if !gsub_vert.is_null() {
                otl_gsub_apply(gsub_vert, &mut gid);
            }
            if !GIDToCIDMap.is_empty() {
                cid = ((GIDToCIDMap[2 * gid as usize] as i32) << 8
                    | GIDToCIDMap[2 * gid as usize + 1] as i32) as u16;
                if cid as i32 == 0 {
                    warn!("GID {} does not have corresponding CID {}.", gid, cid);
                }
            } else {
                cid = gid
            }
            wbuf[0..4].copy_from_slice(&ch.to_be_bytes());
            wbuf[4..6].copy_from_slice(&cid.to_be_bytes());
            cmap.add_cidchar(&wbuf[..4], cid);
            if let Some(tounicode_add) = tounicode_add.as_mut() {
                let mut p: *mut u8 = wbuf.as_mut_ptr().offset(6);
                let uc_len = UC_UTF16BE_encode_char(
                    ch as i32,
                    &mut p,
                    wbuf.as_mut_ptr().offset(1024).offset(-1),
                );
                tounicode_add.add_bfchar(&wbuf[4..6], &wbuf[6..6 + uc_len]);
            }
        }
    }
}
/* OpenType CIDFont:
 *
 *  We don't use GID for them. OpenType cmap table is for
 *  charcode to GID mapping rather than to-CID mapping.
 */
unsafe fn handle_CIDFont(sfont: &mut sfnt, mut csi: *mut CIDSysInfo) -> Option<Vec<u8>> {
    assert!(!csi.is_null());
    let offset = sfnt_find_table_pos(sfont, b"CFF ") as i32;
    if offset == 0 {
        (*csi).registry = "".into();
        (*csi).ordering = "".into();
        return None;
    }
    let maxp = tt_read_maxp_table(sfont);
    let num_glyphs = maxp.numGlyphs;
    if (num_glyphs as i32) < 1 {
        panic!("No glyph contained in this font...");
    }
    let mut cffont = cff_open(sfont.handle.clone(), offset, 0).expect("Could not open CFF font..."); /* CID... */
    if cffont.flag & 1 << 0 == 0 {
        (*csi).registry = "".into();
        (*csi).ordering = "".into();
        return None;
    }
    if !cffont.topdict.contains_key("ROS") {
        panic!("No CIDSystemInfo???");
    } else {
        let reg = cffont.topdict.get("ROS", 0) as u16;
        let ord = cffont.topdict.get("ROS", 1) as u16;
        (*csi).registry = cff_get_string(&cffont, reg).into();
        (*csi).ordering = cff_get_string(&cffont, ord).into();
        (*csi).supplement = cffont.topdict.get("ROS", 2) as i32
    }
    cff_read_charsets(&mut cffont);
    if cffont.charsets.is_none() {
        panic!("No CFF charset data???");
    }
    let mut map = vec![0u8; num_glyphs as usize * 2];
    match cffont.charsets.as_deref().unwrap() {
        Charsets::Glyphs(cids) => {
            let mut gid = 1_u16;
            for &cid in cids.iter() {
                map[2 * gid as usize] = (cid as i32 >> 8 & 0xff) as u8;
                map[2 * gid as usize + 1] = (cid as i32 & 0xff) as u8;
                gid += 1;
            }
        }
        Charsets::Range1(ranges) => {
            let mut gid = 1_u16;
            for range in ranges.iter() {
                let mut cid = range.first;
                for _ in 0..(range.n_left as i32 + 1) as u16 {
                    if !(gid as i32 <= num_glyphs as i32) {
                        break;
                    }
                    map[2 * gid as usize] = (cid as i32 >> 8 & 0xff) as u8;
                    map[2 * gid as usize + 1] = (cid as i32 & 0xff) as u8;
                    gid += 1;
                    cid += 1;
                }
            }
        }
        Charsets::Range2(ranges) => {
            if ranges.len() == 1 && ranges[0].first as i32 == 1 {
                /* "Complete" CIDFont */
                map = Vec::new();
            } else {
                /* Not trivial mapping */
                let mut gid = 1_u16;
                for range in ranges.iter() {
                    let mut cid_0 = range.first;
                    for _ in 0..(range.n_left as i32 + 1) as u16 {
                        if !(gid as i32 <= num_glyphs as i32) {
                            break;
                        }
                        map[2 * gid as usize] = (cid_0 as i32 >> 8 & 0xff) as u8;
                        map[2 * gid as usize + 1] = (cid_0 as i32 & 0xff) as u8;
                        gid += 1;
                        cid_0 += 1;
                    }
                }
            }
        }
    }
    Some(map)
}
unsafe fn is_PUA_or_presentation(uni: u32) -> bool {
    /* KANGXI RADICALs are commonly double encoded. */
    return uni >= 0x2f00_u32 && uni <= 0x2fd5_u32
        || uni >= 0xe000_u32 && uni <= 0xf8ff_u32
        || uni >= 0xfb00_u32 && uni <= 0xfb4f_u32
        || uni >= 0xf0000_u32 && uni <= 0xffffd_u32
        || uni >= 0x100000_u32 && uni <= 0x10fffd_u32;
}
unsafe fn sfnt_get_glyphname(
    post: *mut tt_post_table,
    cffont: Option<&cff_font>,
    gid: u16,
) -> String {
    let mut name = String::new();
    if !post.is_null() {
        name = tt_get_glyphname(post, gid)
    }
    if name.is_empty() {
        if let Some(cffont) = cffont {
            name = cff_get_glyphname(cffont, gid)
        }
    }
    name
}
/*
 * Substituted glyphs:
 *
 *  Mapping information stored in cmap_add.
 */
unsafe fn handle_subst_glyphs(
    cmap: &mut CMap,
    cmap_add: *mut CMap,
    used_glyphs: &[u8],
    sfont: &sfnt,
    cffont: Option<&cff_font>,
) -> u16 {
    let mut post: *mut tt_post_table = ptr::null_mut();
    if cmap_add.is_null() {
        post = tt_read_post_table(sfont)
    }
    let mut count = 0_u16;
    for i in 0..8192 {
        if !(used_glyphs[i as usize] == 0) {
            for j in 0..8 {
                let gid: u16 = ((8 * i as i32) as u32).wrapping_add(j) as u16;
                if !(used_glyphs[(gid as i32 / 8) as usize] as i8 as i32 & 1 << 7 - gid as i32 % 8
                    == 0)
                {
                    if cmap_add.is_null() {
                        /* try to look up Unicode values from the glyph name... */
                        let mut unicodes: [i32; 16] = [0; 16];
                        let mut unicode_count: i32 = -1;
                        let name = sfnt_get_glyphname(post, cffont, gid);
                        if !name.is_empty() {
                            unicode_count = agl_get_unicodes(&name, unicodes.as_mut_ptr(), 16)
                        }
                        if unicode_count == -1 {
                            if !name.is_empty() {
                                info!("No Unicode mapping available: GID={}, name={}\n", gid, name,);
                            } else {
                                info!("No Unicode mapping available: GID={}\n", gid);
                            }
                        } else {
                            /* the Unicode characters go into wbuf[2] and following, in UTF16BE */
                            /* we rely on WBUF_SIZE being more than adequate for MAX_UNICODES  */
                            let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
                            let mut len = 0;
                            for k in 0..unicode_count {
                                len += UC_UTF16BE_encode_char(
                                    unicodes[k as usize],
                                    &mut p,
                                    wbuf.as_mut_ptr().offset(1024),
                                ) as usize;
                            }
                            cmap.add_bfchar(&gid.to_be_bytes()[..2], &wbuf[2..2 + len]);
                        }
                    } else {
                        wbuf[0] = (gid as i32 >> 8 & 0xff) as u8;
                        wbuf[1] = (gid as i32 & 0xff) as u8;
                        let mut inbuf = &wbuf[..2];
                        let (_, outbuf) = CMap_decode(&*cmap_add, &mut inbuf, &mut wbuf[2..1024]);
                        if inbuf.len() != 0 {
                            warn!("CMap conversion failed...");
                        } else {
                            let len =
                                ((1024 - 2) as u64).wrapping_sub(outbuf.len() as u64) as usize;
                            cmap.add_bfchar(&wbuf[..2], &wbuf[2..2 + len]);
                            count += 1;
                            if verbose > 0 {
                                info!(
                                    "otf_cmap>> Additional ToUnicode mapping: <{:04X}> <",
                                    gid as i32,
                                );
                                for _i in 0..len {
                                    info!("{:02X}", wbuf[2 + _i] as i32,);
                                }
                                info!(">\n");
                            }
                        }
                    }
                }
            }
        }
    }
    if !post.is_null() {
        tt_release_post_table(post);
    }
    count
}
unsafe fn add_to_cmap_if_used(
    cmap: &mut CMap,
    cffont: Option<&cff_font>,
    used_chars: &mut [u8],
    gid: u16,
    ch: u32,
) -> u16 {
    let mut count: u16 = 0_u16;
    let cid: u16 = (if let Some(cffont) = cffont {
        cff_charsets_lookup_inverse(cffont, gid) as i32
    } else {
        gid as i32
    }) as u16;
    /* Skip PUA characters and alphabetic presentation forms, allowing
     * handle_subst_glyphs() as it might find better mapping. Fixes the
     * mapping of ligatures encoded in PUA in fonts like Linux Libertine
     * and old Adobe fonts.
     */
    if used_chars[(cid as i32 / 8) as usize] as i8 as i32 & 1 << 7 - cid as i32 % 8 != 0
        && !is_PUA_or_presentation(ch)
    {
        let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
        count = count.wrapping_add(1);
        let len =
            UC_UTF16BE_encode_char(ch as i32, &mut p, wbuf.as_mut_ptr().offset(1024)) as usize;
        cmap.add_bfchar(&cid.to_be_bytes()[..2], &wbuf[2..2 + len]);
        /* Avoid duplicate entry
         * There are problem when two Unicode code is mapped to
         * single glyph...
         */
        used_chars[(cid as i32 / 8) as usize] &= !(1 << 7 - cid as i32 % 8) as i8 as u8;
    }
    count
}
unsafe fn create_ToUnicode_cmap4(
    cmap: &mut CMap,
    map: &cmap4,
    used_chars: &mut [u8],
    cffont: Option<&cff_font>,
) -> u16 {
    let mut count: u16 = 0_u16;
    let segCount: u16 = (map.segCountX2 as i32 / 2) as u16;
    for i in 0..segCount as usize {
        let c0: u16 = map.startCount[i];
        let c1: u16 = map.endCount[i];
        let d: u16 = (map.idRangeOffset[i] as i32 / 2 - (segCount as i32 - i as i32)) as u16;
        let mut j = 0;
        while j as i32 <= c1 as i32 - c0 as i32 {
            let ch: u16 = (c0 as i32 + j as i32) as u16;
            let gid = if map.idRangeOffset[i] as i32 == 0 {
                (ch as i32 + map.idDelta[i] as i32 & 0xffff) as u16
            } else if c0 as i32 == 0xffff
                && c1 as i32 == 0xffff
                && map.idRangeOffset[i] as i32 == 0xffff
            {
                /* this is for protection against some old broken fonts... */
                0
            } else {
                (map.glyphIndexArray[(j as i32 + d as i32) as usize] as i32 + map.idDelta[i] as i32
                    & 0xffff) as u16
            };
            count = (count as i32
                + add_to_cmap_if_used(cmap, cffont, used_chars, gid, ch as u32) as i32)
                as u16;
            j += 1;
        }
    }
    count
}
unsafe fn create_ToUnicode_cmap12(
    cmap: &mut CMap,
    map: &cmap12,
    used_chars: &mut [u8],
    cffont: Option<&cff_font>,
) -> u16 {
    let mut count: u32 = 0_u32;
    for group in map.groups.iter() {
        for ch in group.startCharCode..=group.endCharCode {
            let d: i32 = (ch - group.startCharCode) as i32;
            let gid: u16 = (group.startGlyphID.wrapping_add(d as u32) & 0xffff_u32) as u16;
            count += add_to_cmap_if_used(cmap, cffont, used_chars, gid, ch) as u32;
        }
    }
    count as u16
}
unsafe fn create_ToUnicode_cmap(
    ttcmap: *mut tt_cmap,
    cmap_name: &str,
    cmap_add: *mut CMap,
    used_chars: &[u8],
    sfont: &mut sfnt,
    code_to_cid_cmap: *mut CMap,
) -> Option<pdf_stream> {
    let mut count: u16 = 0_u16;
    // prepare_CIDFont_from_sfnt(sfont);
    let mut offset: u32 = 0;
    let mut flag = true;
    if sfont.type_0 != 1 << 2 || sfnt_read_table_directory(sfont, 0) < 0 || {
        offset = sfnt_find_table_pos(sfont, b"CFF ");
        offset == 0
    } {
        flag = false;
    }
    let cffont = if flag {
        cff_open(sfont.handle.clone(), offset as i32, 0).map(|mut cffont| {
            cff_read_charsets(&mut cffont);
            cffont
        })
    } else {
        None
    };
    let is_cidfont = if let Some(cffont) = &cffont {
        cffont.flag & 1 << 0 != 0
    } else {
        false
    };
    let mut cmap = CMap::new();
    cmap.set_name(cmap_name);
    cmap.set_wmode(0);
    cmap.set_type(2);
    cmap.set_CIDSysInfo(&CSI_UNICODE);
    cmap.add_codespacerange(&srange_min[..2], &srange_max[..2]);
    /* cmap_add here stores information about all unencoded glyphs which can be
     * accessed only through OT Layout GSUB table.
     */
    if !code_to_cid_cmap.is_null() && cffont.is_some() && is_cidfont && cmap_add.is_null() {
        for i in 0..8192 {
            if used_chars[i as usize] != 0 {
                for j in 0..8 {
                    let cid: u16 = (8 * i as i32 + j) as u16;
                    if !(used_chars[(cid as i32 / 8) as usize] as i8 as i32
                        & 1 << 7 - cid as i32 % 8
                        == 0)
                    {
                        let ch = CMap_reverse_decode(&*code_to_cid_cmap, cid);
                        if ch >= 0 {
                            let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
                            let len =
                                UC_UTF16BE_encode_char(ch, &mut p, wbuf.as_mut_ptr().offset(1024))
                                    as usize;
                            cmap.add_bfchar(&cid.to_be_bytes()[..2], &wbuf[2..2 + len]);
                            count = count.wrapping_add(1)
                        }
                    }
                }
            }
        }
    } else {
        let mut used_chars_copy: [u8; 8192] = [0; 8192];
        used_chars_copy.copy_from_slice(&used_chars[0..8192]);
        /* For create_ToUnicode_cmap{4,12}(), cffont is for GID -> CID lookup,
         * so it is only needed for CID fonts. */
        match (*ttcmap).format as i32 {
            4 => {
                count = create_ToUnicode_cmap4(
                    &mut cmap,
                    &*((*ttcmap).map as *mut cmap4),
                    &mut used_chars_copy[..],
                    if is_cidfont {
                        if let Some(cffont) = &cffont {
                            Some(cffont)
                        } else {
                            None
                        }
                    } else {
                        None
                    },
                )
            }
            12 => {
                count = create_ToUnicode_cmap12(
                    &mut cmap,
                    &*((*ttcmap).map as *mut cmap12),
                    &mut used_chars_copy[..],
                    if is_cidfont {
                        if let Some(cffont) = &cffont {
                            Some(cffont)
                        } else {
                            None
                        }
                    } else {
                        None
                    },
                )
            }
            _ => {}
        }
        /* For handle_subst_glyphs(), cffont is for GID -> glyph name lookup, so
         * it is only needed for non-CID fonts. */
        count = (count as i32
            + handle_subst_glyphs(
                &mut cmap,
                cmap_add,
                &used_chars_copy[..],
                sfont,
                if is_cidfont {
                    None
                } else {
                    if let Some(cffont) = &cffont {
                        Some(cffont)
                    } else {
                        None
                    }
                },
            ) as i32) as u16
    }
    let stream = if (count as i32) < 1 {
        None
    } else {
        CMap_create_stream(&mut cmap)
    };
    stream
}
static mut cmap_plat_encs: [cmap_plat_enc_rec; 5] = [
    cmap_plat_enc_rec {
        platform: 3,
        encoding: 10,
    },
    cmap_plat_enc_rec {
        platform: 0,
        encoding: 3,
    },
    cmap_plat_enc_rec {
        platform: 0,
        encoding: 0,
    },
    cmap_plat_enc_rec {
        platform: 3,
        encoding: 1,
    },
    cmap_plat_enc_rec {
        platform: 0,
        encoding: 1,
    },
];

pub(crate) unsafe fn otf_create_ToUnicode_stream(
    font_name: &str,
    ttc_index: i32,
    used_chars: &[u8],
    cmap_id: Option<usize>,
) -> *mut pdf_obj {
    let mut cmap_obj = None;
    let mut ttcmap: *mut tt_cmap = ptr::null_mut();
    let offset;
    /* replace slash in map name with dash to make the output cmap name valid,
     * happens when XeTeX embeds full font path
     * https://sourceforge.net/p/xetex/bugs/52/
     */
    let normalized_font_name = font_name.replace("/", "-");
    let cmap_name = format!("{},{:03}-UTF16", normalized_font_name, ttc_index);
    if let Some(res_id) = pdf_findresource("CMap", &cmap_name) {
        return pdf_get_resource_reference(res_id);
    }
    if verbose > 0 {
        info!("\n");
        info!(
            "otf_cmap>> Creating ToUnicode CMap for \"{}\"...\n",
            font_name
        );
    }
    let mut sfont = if let Some(handle) =
        dpx_open_truetype_file(font_name).or_else(|| dpx_open_opentype_file(font_name))
    {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(font_name) {
        dfont_open(handle, ttc_index).expect(&format!(
            "Could not open OpenType/TrueType font file \"{}\"",
            font_name
        ))
    } else {
        return ptr::null_mut();
    };
    match sfont.type_0 {
        256 => offset = sfont.offset,
        16 => {
            offset = ttc_read_offset(&mut sfont, ttc_index);
            if offset == 0_u32 {
                panic!("Invalid TTC index");
            }
        }
        _ => offset = 0_u32,
    }
    if sfnt_read_table_directory(&mut sfont, offset) < 0 {
        panic!("Could not read OpenType/TrueType table directory.");
    }
    let mut code_to_cid_cmap = CMap_cache_get(cmap_id);
    let cmap_type = (*code_to_cid_cmap).get_type();
    if cmap_type != 1 {
        code_to_cid_cmap = ptr::null_mut()
    }
    let cmap_add = if let Some(cmap_add_id) =
        CMap_cache_find(&format!("{},{:03}-UCS32-Add", font_name, ttc_index))
    {
        CMap_cache_get(Some(cmap_add_id))
    } else {
        ptr::null_mut()
    };
    CMap_set_silent(1); /* many warnings without this... */
    for i in 0..(::std::mem::size_of::<[cmap_plat_enc_rec; 5]>() as u64)
        .wrapping_div(::std::mem::size_of::<cmap_plat_enc_rec>() as u64) as usize
    {
        ttcmap = tt_cmap_read(
            &mut sfont,
            cmap_plat_encs[i].platform as u16,
            cmap_plat_encs[i].encoding as u16,
        );
        if !ttcmap.is_null() {
            if (*ttcmap).format == 4 || (*ttcmap).format == 12 {
                cmap_obj = create_ToUnicode_cmap(
                    ttcmap,
                    &cmap_name,
                    cmap_add,
                    used_chars,
                    &mut sfont,
                    code_to_cid_cmap,
                );
                break;
            }
        }
    }
    if cmap_obj.is_none() {
        warn!("Unable to read OpenType/TrueType Unicode cmap table.");
    }
    tt_cmap_release(ttcmap);
    CMap_set_silent(0);
    if let Some(cmap_obj) = cmap_obj {
        let res_id = pdf_defineresource("CMap", &cmap_name, cmap_obj.into(), 1);
        pdf_get_resource_reference(res_id)
    } else {
        ptr::null_mut()
    }
}
unsafe fn load_base_CMap(
    cmap_name: &str,
    tounicode_add: *mut CMap,
    wmode: i32,
    csi: Option<&CIDSysInfo>,
    GIDToCIDMap: &[u8],
    gsub_vert: *mut otl_gsub,
    gsub_list: *mut otl_gsub,
    ttcmap: *mut tt_cmap,
) -> Option<usize> {
    if let Some(cmap_id) = CMap_cache_find(cmap_name) {
        Some(cmap_id)
    } else {
        let mut cmap = CMap::new();
        cmap.set_name(cmap_name);
        cmap.set_type(1);
        cmap.set_wmode(wmode);
        cmap.add_codespacerange(&lrange_min[..4], &lrange_max[..4]);
        if let Some(csi) = csi {
            /* CID */
            cmap.set_CIDSysInfo(csi);
        } else {
            cmap.set_CIDSysInfo(&CSI_IDENTITY);
        }
        if (*ttcmap).format as i32 == 12 {
            load_cmap12(
                &*((*ttcmap).map as *mut cmap12),
                GIDToCIDMap,
                gsub_vert,
                gsub_list,
                &mut cmap,
                tounicode_add,
            );
        } else if (*ttcmap).format as i32 == 4 {
            load_cmap4(
                &*((*ttcmap).map as *mut cmap4),
                GIDToCIDMap,
                gsub_vert,
                gsub_list,
                &mut cmap,
                tounicode_add,
            );
        }
        Some(CMap_cache_add(Box::new(cmap)))
    }
}
/* TrueType cmap table */
/* or version, only for Mac */
/* Paltform ID */
/* Platform-specific encoding ID */
/* Windows */
/* Mac */
/* Indirect reference */
/* CMap ID */

pub(crate) unsafe fn otf_load_Unicode_CMap(
    map_name: &str,
    ttc_index: i32,
    otl_tags: &str,
    wmode: i32,
) -> Option<usize> {
    /* Additional ToUnicode mappings required by OTL GSUB substitusion */
    let mut tounicode_add: *mut CMap = ptr::null_mut();
    let offset;
    let cmap_name;
    let mut gsub_vert;
    let gsub_list;
    let mut csi: CIDSysInfo = CIDSysInfo {
        registry: "".into(),
        ordering: "".into(),
        supplement: 0,
    };
    if map_name.is_empty() {
        return None;
    }
    if ttc_index > 999 || ttc_index < 0 {
        return None;
        /* Sorry for this... */
    }
    let mut handle = dpx_open_truetype_file(map_name);
    if handle.is_none() {
        handle = dpx_open_opentype_file(map_name);
    }
    let mut sfont = if handle.is_none() {
        let handle = dpx_open_dfont_file(map_name);
        if handle.is_none() {
            return None;
        }
        dfont_open(handle.unwrap(), ttc_index).expect(&format!(
            "Could not open OpenType/TrueType/dfont font file \"{}\"",
            map_name
        ))
    } else {
        sfnt_open(handle.unwrap())
    };
    match sfont.type_0 {
        16 => {
            offset = ttc_read_offset(&mut sfont, ttc_index);
            if offset == 0_u32 {
                panic!("Invalid TTC index");
            }
        }
        1 | 4 => offset = 0_u32,
        256 => offset = sfont.offset,
        _ => {
            panic!("Not a OpenType/TrueType/TTC font?: {}", map_name);
        }
    }
    if sfnt_read_table_directory(&mut sfont, offset) < 0 {
        panic!("Could not read OpenType/TrueType table directory.");
    }

    let base_name = if wmode != 0 {
        format!("{},{:03}-UCS4-V", map_name, ttc_index)
    } else {
        format!("{},{:03}-UCS4-H", map_name, ttc_index)
    };

    if !otl_tags.is_empty() {
        cmap_name = if wmode != 0 {
            format!("{},{:03},{}-UCS4-V", map_name, ttc_index, otl_tags)
        } else {
            format!("{},{:03},{}-UCS4-H", map_name, ttc_index, otl_tags)
        };
        /* tounicode_add here is later refered by otf_create_ToUnicode_stream()
         * for finding additional CID to Unicode mapping entries required by
         * OTL gsub substitution.
         */

        let tounicode_add_name = format!("{},{:03}-UCS32-Add", map_name, ttc_index);
        if let Some(tounicode_add_id) = CMap_cache_find(&tounicode_add_name) {
            /* Unicode 2.0 or later */
            tounicode_add = CMap_cache_get(Some(tounicode_add_id))
        } else {
            let mut cmap = CMap::new();
            cmap.set_name(&tounicode_add_name);
            cmap.set_type(2);
            cmap.set_wmode(0);
            cmap.add_codespacerange(&srange_min[..2], &srange_max[..2]);
            cmap.set_CIDSysInfo(&CSI_UNICODE);
            cmap.add_bfchar(&srange_min[..2], &srange_max[..2]);
            tounicode_add = CMap_cache_get(Some(CMap_cache_add(Box::new(cmap))));
        }
    } else {
        cmap_name = base_name;
    }
    let (is_cidfont, GIDToCIDMap) = if let Some(map) = if sfont.type_0 == 1 << 2 {
        handle_CIDFont(&mut sfont, &mut csi)
    } else {
        None
    } {
        (true, map)
    } else {
        (false, Vec::new())
    };

    if verbose > 0 {
        info!("\n");
        info!(
            "otf_cmap>> Unicode charmap for font=\"{}\" layout=\"{}\"\n",
            map_name,
            if !otl_tags.is_empty() {
                otl_tags
            } else {
                "none"
            },
        );
    }
    if let Some(cmap_id) = CMap_cache_find(&cmap_name) {
        if verbose > 0 {
            info!("otf_cmap>> Found at cmap_id={}.\n", cmap_id);
        }
        return Some(cmap_id);
    }
    let mut ttcmap = tt_cmap_read(&mut sfont, 3_u16, 10_u16);
    if ttcmap.is_null() {
        ttcmap = tt_cmap_read(&mut sfont, 3_u16, 1_u16);
        if ttcmap.is_null() {
            ttcmap = tt_cmap_read(&mut sfont, 0_u16, 3_u16);
            if ttcmap.is_null() {
                panic!("Unable to read OpenType/TrueType Unicode cmap table.");
            }
        }
    }
    if wmode == 1 {
        gsub_vert = otl_gsub_new();
        if otl_gsub_add_feat(&mut *gsub_vert, b"*", b"*", b"vrt2", &sfont) < 0 {
            if otl_gsub_add_feat(&mut *gsub_vert, b"*", b"*", b"vert", &sfont) < 0 {
                warn!("GSUB feature vrt2/vert not found.");
                otl_gsub_release(gsub_vert);
                gsub_vert = ptr::null_mut()
            } else {
                otl_gsub_select(gsub_vert, b"*", b"*", b"vert");
            }
        } else {
            otl_gsub_select(gsub_vert, b"*", b"*", b"vrt2");
        }
    } else {
        gsub_vert = ptr::null_mut()
    }
    if !otl_tags.is_empty() {
        let otl_tags_ = CString::new(otl_tags).unwrap();
        gsub_list = otl_gsub_new();
        if otl_gsub_add_feat_list(gsub_list, otl_tags_.as_ptr(), &sfont) < 0 {
            warn!("Reading GSUB feature table(s) failed for \"{}\"", otl_tags);
        } else {
            otl_gsub_set_chain(gsub_list, otl_tags_.as_ptr());
        }
    } else {
        gsub_list = ptr::null_mut()
    }
    let cmap_id = load_base_CMap(
        &cmap_name,
        tounicode_add,
        wmode,
        if is_cidfont { Some(&csi) } else { None },
        &GIDToCIDMap,
        gsub_vert,
        gsub_list,
        ttcmap,
    );
    if cmap_id.is_none() {
        panic!("Failed to read OpenType/TrueType cmap table.");
    }
    if !gsub_vert.is_null() {
        otl_gsub_release(gsub_vert);
    }
    if !gsub_list.is_null() {
        otl_gsub_release(gsub_list);
    }
    if is_cidfont {
        csi.registry = "".into();
        csi.ordering = "".into();
    }
    tt_cmap_release(ttcmap);
    cmap_id
}
