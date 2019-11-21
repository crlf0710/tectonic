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
)]

use super::dpx_sfnt::{
    dfont_open, sfnt_close, sfnt_find_table_pos, sfnt_locate_table, sfnt_open,
    sfnt_read_table_directory,
};
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_agl::agl_get_unicodes;
use super::dpx_cff::{
    cff_charsets_lookup_inverse, cff_close, cff_get_glyphname, cff_get_string, cff_open,
    cff_read_charsets,
};
use super::dpx_cff_dict::{cff_dict_get, cff_dict_known};
use super::dpx_cid::{CSI_IDENTITY, CSI_UNICODE};
use super::dpx_cmap::{
    CMap_add_bfchar, CMap_add_cidchar, CMap_add_codespacerange, CMap_cache_add, CMap_cache_find,
    CMap_cache_get, CMap_decode, CMap_get_type, CMap_new, CMap_release, CMap_reverse_decode,
    CMap_set_CIDSysInfo, CMap_set_name, CMap_set_silent, CMap_set_type, CMap_set_wmode,
};
use super::dpx_cmap_write::CMap_create_stream;
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_opentype_file, dpx_open_truetype_file};
use super::dpx_mem::new;
use super::dpx_numbers::{
    tt_get_signed_pair, tt_get_unsigned_byte, tt_get_unsigned_pair, tt_get_unsigned_quad,
};
use super::dpx_pdfresource::{pdf_defineresource, pdf_findresource, pdf_get_resource_reference};
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_gsub::{
    otl_gsub, otl_gsub_add_feat, otl_gsub_add_feat_list, otl_gsub_apply, otl_gsub_apply_chain,
    otl_gsub_new, otl_gsub_release, otl_gsub_select, otl_gsub_set_chain, otl_gsub_set_verbose,
};
use super::dpx_tt_post::{tt_get_glyphname, tt_read_post_table, tt_release_post_table};
use super::dpx_tt_table::tt_read_maxp_table;
use super::dpx_unicode::UC_UTF16BE_encode_char;
use crate::dpx_pdfobj::pdf_obj;
use crate::dpx_truetype::sfnt_table_info;
use crate::mfree;
use crate::shims::sprintf;
use libc::{free, memcpy, memset, strcpy, strlen};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;
use super::dpx_sfnt::sfnt;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_cmap {
    pub format: u16,
    pub platform: u16,
    pub encoding: u16,
    pub language: u32,
    pub map: *mut libc::c_void,
}

use super::dpx_cid::CIDSysInfo;

use super::dpx_cff::cff_font;

use super::dpx_tt_post::tt_post_table;

use super::dpx_cmap::CMap;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap12 {
    pub nGroups: u32,
    pub groups: *mut charGroup,
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
pub struct charGroup {
    pub startCharCode: u32,
    pub endCharCode: u32,
    pub startGlyphID: u32,
}
/* format 6: trimmed table mapping */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap6 {
    pub firstCode: u16,
    pub entryCount: u16,
    pub glyphIndexArray: *mut u16,
}
/*
 * format 4: segment mapping to delta values
 * - Microsoft standard character to glyph index mapping table
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap4 {
    pub segCountX2: u16,
    pub searchRange: u16,
    pub entrySelector: u16,
    pub rangeShift: u16,
    pub endCount: *mut u16,
    pub reservedPad: u16,
    pub startCount: *mut u16,
    pub idDelta: *mut u16,
    pub idRangeOffset: *mut u16,
    pub glyphIndexArray: *mut u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap2 {
    pub subHeaderKeys: [u16; 256],
    pub subHeaders: *mut SubHeader,
    pub glyphIndexArray: *mut u16,
}
/* format 2: high-byte mapping through table */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct SubHeader {
    pub firstCode: u16,
    pub entryCount: u16,
    pub idDelta: i16,
    pub idRangeOffset: u16,
}
/* format 0: byte encoding table */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap0 {
    pub glyphIndexArray: [u8; 256],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cmap_plat_enc_rec {
    pub platform: i16,
    pub encoding: i16,
}
static mut verbose: i32 = 0i32;

pub unsafe fn otf_cmap_set_verbose(mut level: i32) {
    otl_gsub_set_verbose(level);
    verbose = level;
}
unsafe fn read_cmap0(mut sfont: *mut sfnt, mut len: u32) -> *mut cmap0 {
    if len < 256_u32 {
        panic!("invalid cmap subtable");
    }
    let map = new((1_u64).wrapping_mul(::std::mem::size_of::<cmap0>() as u64) as u32) as *mut cmap0;
    for i in 0..256 {
        (*map).glyphIndexArray[i as usize] = tt_get_unsigned_byte(&mut (*sfont).handle);
    }
    map
}
unsafe fn release_cmap0(mut map: *mut cmap0) {
    free(map as *mut libc::c_void);
}
unsafe fn lookup_cmap0(mut map: *mut cmap0, mut cc: u16) -> u16 {
    return (if cc as i32 > 255i32 {
        0i32
    } else {
        (*map).glyphIndexArray[cc as usize] as i32
    }) as u16;
}
unsafe fn read_cmap2(mut sfont: *mut sfnt, mut len: u32) -> *mut cmap2 {
    if len < 512_u32 {
        panic!("invalid cmap subtable");
    }
    let handle = &mut (*sfont).handle;
    let map = new((1_u64).wrapping_mul(::std::mem::size_of::<cmap2>() as u64) as u32) as *mut cmap2;
    for i in 0..256 {
        (*map).subHeaderKeys[i as usize] = tt_get_unsigned_pair(handle);
    }
    let mut n = 0_u16;
    for i in 0..256 {
        (*map).subHeaderKeys[i as usize] = ((*map).subHeaderKeys[i as usize] as i32 / 8i32) as u16;
        if (n as i32) < (*map).subHeaderKeys[i as usize] as i32 {
            n = (*map).subHeaderKeys[i as usize]
        }
    }
    n = (n as i32 + 1i32) as u16;
    (*map).subHeaders =
        new((n as u32 as u64).wrapping_mul(::std::mem::size_of::<SubHeader>() as u64) as u32)
            as *mut SubHeader;
    for i in 0..n as i32 {
        (*(*map).subHeaders.offset(i as isize)).firstCode = tt_get_unsigned_pair(handle);
        (*(*map).subHeaders.offset(i as isize)).entryCount = tt_get_unsigned_pair(handle);
        (*(*map).subHeaders.offset(i as isize)).idDelta = tt_get_signed_pair(handle);
        (*(*map).subHeaders.offset(i as isize)).idRangeOffset = tt_get_unsigned_pair(handle);
        /* It makes things easier to let the offset starts from
         * the beginning of glyphIndexArray.
         */
        if (*(*map).subHeaders.offset(i as isize)).idRangeOffset as i32 != 0i32 {
            let ref mut fresh0 = (*(*map).subHeaders.offset(i as isize)).idRangeOffset;
            *fresh0 = (*fresh0 as i32 - (2i32 + (n as i32 - i as i32 - 1i32) * 8i32)) as u16
        }
    }
    /* Caculate the length of glyphIndexArray, this is ugly,
     * there should be a better way to get this information.
     */
    let n = (len
        .wrapping_sub(518_u32)
        .wrapping_sub((n as i32 * 8i32) as u32) as u16 as i32
        / 2i32) as u16;
    (*map).glyphIndexArray =
        new((n as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32) as *mut u16;
    for i in 0..n as i32 {
        *(*map).glyphIndexArray.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    map
}
unsafe fn release_cmap2(mut map: *mut cmap2) {
    if !map.is_null() {
        free((*map).subHeaders as *mut libc::c_void);
        free((*map).glyphIndexArray as *mut libc::c_void);
        free(map as *mut libc::c_void);
    };
}
unsafe fn lookup_cmap2(mut map: *mut cmap2, mut cc: u16) -> u16 {
    let mut idx: u16 = 0_u16;
    let [hi, lo] = cc.to_be_bytes();
    let hi = hi as i32;
    let lo = lo as i32;
    /* select which subHeader to use */
    let i = (*map).subHeaderKeys[hi as usize];
    let firstCode = (*(*map).subHeaders.offset(i as isize)).firstCode;
    let entryCount = (*(*map).subHeaders.offset(i as isize)).entryCount;
    let idDelta = (*(*map).subHeaders.offset(i as isize)).idDelta;
    let mut idRangeOffset =
        ((*(*map).subHeaders.offset(i as isize)).idRangeOffset as i32 / 2i32) as u16;
    if lo >= firstCode as i32 && lo < firstCode as i32 + entryCount as i32 {
        idRangeOffset = (idRangeOffset as i32 + (lo - firstCode as i32)) as u16;
        idx = *(*map).glyphIndexArray.offset(idRangeOffset as isize);
        if idx as i32 != 0i32 {
            idx = (idx as i32 + idDelta as i32 & 0xffffi32) as u16
        }
    }
    idx
}
unsafe fn read_cmap4(mut sfont: *mut sfnt, mut len: u32) -> *mut cmap4 {
    if len < 8_u32 {
        panic!("invalid cmap subtable");
    }
    let handle = &mut (*sfont).handle;
    let map = new((1_u64).wrapping_mul(::std::mem::size_of::<cmap4>() as u64) as u32) as *mut cmap4;
    let segCount = tt_get_unsigned_pair(handle);
    (*map).segCountX2 = segCount;
    (*map).searchRange = tt_get_unsigned_pair(handle);
    (*map).entrySelector = tt_get_unsigned_pair(handle);
    (*map).rangeShift = tt_get_unsigned_pair(handle);
    let segCount = (segCount as i32 / 2i32) as u16;
    (*map).endCount =
        new((segCount as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    for i in 0..segCount as i32 {
        *(*map).endCount.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    (*map).reservedPad = tt_get_unsigned_pair(handle);
    (*map).startCount =
        new((segCount as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    for i in 0..segCount as i32 {
        *(*map).startCount.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    (*map).idDelta =
        new((segCount as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    for i in 0..segCount as i32 {
        *(*map).idDelta.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    (*map).idRangeOffset =
        new((segCount as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    for i in 0..segCount as i32 {
        *(*map).idRangeOffset.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    let n = len
        .wrapping_sub(16_u32)
        .wrapping_sub((8i32 * segCount as i32) as u32)
        .wrapping_div(2_u32) as u16;
    if n as i32 == 0i32 {
        (*map).glyphIndexArray = ptr::null_mut()
    } else {
        (*map).glyphIndexArray = new((n as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u16>() as u64)
            as u32) as *mut u16;
        for i in 0..n as i32 {
            *(*map).glyphIndexArray.offset(i as isize) = tt_get_unsigned_pair(handle);
        }
    }
    map
}
unsafe fn release_cmap4(mut map: *mut cmap4) {
    if !map.is_null() {
        free((*map).endCount as *mut libc::c_void);
        free((*map).startCount as *mut libc::c_void);
        free((*map).idDelta as *mut libc::c_void);
        free((*map).idRangeOffset as *mut libc::c_void);
        free((*map).glyphIndexArray as *mut libc::c_void);
        free(map as *mut libc::c_void);
    };
}
unsafe fn lookup_cmap4(mut map: *mut cmap4, mut cc: u16) -> u16 {
    let mut gid: u16 = 0_u16;
    /*
     * Segments are sorted in order of increasing endCode values.
     * Last segment maps 0xffff to gid 0 (?)
     */
    let segCount = ((*map).segCountX2 as i32 / 2i32) as u16;
    let mut i = segCount;
    loop {
        let fresh1 = i;
        i -= 1;
        if !(fresh1 as i32 > 0i32 && cc as i32 <= *(*map).endCount.offset(i as isize) as i32) {
            break;
        }
        if !(cc as i32 >= *(*map).startCount.offset(i as isize) as i32) {
            continue;
        }
        if *(*map).idRangeOffset.offset(i as isize) as i32 == 0i32 {
            gid = (cc as i32 + *(*map).idDelta.offset(i as isize) as i32 & 0xffffi32) as u16
        } else if cc as i32 == 0xffffi32
            && *(*map).idRangeOffset.offset(i as isize) as i32 == 0xffffi32
        {
            /* this is for protection against some old broken fonts... */
            gid = 0_u16
        } else {
            let j = (*(*map).idRangeOffset.offset(i as isize) as i32
                - (segCount as i32 - i as i32) * 2i32) as u16;
            let j =
                (cc as i32 - *(*map).startCount.offset(i as isize) as i32 + j as i32 / 2i32) as u16;
            gid = *(*map).glyphIndexArray.offset(j as isize);
            if gid as i32 != 0i32 {
                gid = (gid as i32 + *(*map).idDelta.offset(i as isize) as i32 & 0xffffi32) as u16
            }
        }
        break;
    }
    gid
}
unsafe fn read_cmap6(mut sfont: *mut sfnt, mut len: u32) -> *mut cmap6 {
    if len < 4_u32 {
        panic!("invalid cmap subtable");
    }
    let handle = &mut (*sfont).handle;
    let map = new((1_u64).wrapping_mul(::std::mem::size_of::<cmap6>() as u64) as u32) as *mut cmap6;
    (*map).firstCode = tt_get_unsigned_pair(handle);
    (*map).entryCount = tt_get_unsigned_pair(handle);
    (*map).glyphIndexArray = new(
        ((*map).entryCount as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32
    ) as *mut u16;
    for i in 0..(*map).entryCount as i32 {
        *(*map).glyphIndexArray.offset(i as isize) = tt_get_unsigned_pair(handle);
    }
    map
}
unsafe fn release_cmap6(mut map: *mut cmap6) {
    if !map.is_null() {
        free((*map).glyphIndexArray as *mut libc::c_void);
        free(map as *mut libc::c_void);
    };
}
unsafe fn lookup_cmap6(mut map: *mut cmap6, mut cc: u16) -> u16 {
    let idx = (cc as i32 - (*map).firstCode as i32) as u16;
    if (idx as i32) < (*map).entryCount as i32 {
        return *(*map).glyphIndexArray.offset(idx as isize);
    }
    0_u16
}
/* ULONG length */
unsafe fn read_cmap12(mut sfont: *mut sfnt, mut len: u32) -> *mut cmap12 {
    if len < 4_u32 {
        panic!("invalid cmap subtable");
    }
    let handle = &mut (*sfont).handle;
    let map =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cmap12>() as u64) as u32) as *mut cmap12;
    (*map).nGroups = tt_get_unsigned_quad(handle);
    (*map).groups =
        new(((*map).nGroups as u64).wrapping_mul(::std::mem::size_of::<charGroup>() as u64) as u32)
            as *mut charGroup;
    for i in 0..(*map).nGroups {
        (*(*map).groups.offset(i as isize)).startCharCode = tt_get_unsigned_quad(handle);
        (*(*map).groups.offset(i as isize)).endCharCode = tt_get_unsigned_quad(handle);
        (*(*map).groups.offset(i as isize)).startGlyphID = tt_get_unsigned_quad(handle);
    }
    map
}
unsafe fn release_cmap12(mut map: *mut cmap12) {
    if !map.is_null() {
        free((*map).groups as *mut libc::c_void);
        free(map as *mut libc::c_void);
    };
}
unsafe fn lookup_cmap12(mut map: *mut cmap12, mut cccc: u32) -> u16 {
    let mut gid: u16 = 0_u16;
    let mut i = (*map).nGroups as i32;
    loop {
        let fresh2 = i;
        i = i - 1;
        if !(fresh2 >= 0i32 && cccc <= (*(*map).groups.offset(i as isize)).endCharCode) {
            break;
        }
        if !(cccc >= (*(*map).groups.offset(i as isize)).startCharCode) {
            continue;
        }
        gid = (cccc
            .wrapping_sub((*(*map).groups.offset(i as isize)).startCharCode)
            .wrapping_add((*(*map).groups.offset(i as isize)).startGlyphID)
            & 0xffff_u32) as u16;
        break;
    }
    gid
}
/* read cmap */

pub unsafe fn tt_cmap_read(
    mut sfont: *mut sfnt,
    mut platform: u16,
    mut encoding: u16,
) -> *mut tt_cmap {
    let length;
    assert!(!sfont.is_null());
    let mut offset = sfnt_locate_table(sfont, sfnt_table_info::CMAP);
    let handle = &mut (*sfont).handle;
    tt_get_unsigned_pair(handle);
    let n_subtabs = tt_get_unsigned_pair(handle);
    let mut i = 0_u16;
    while (i as i32) < n_subtabs as i32 {
        let p_id = tt_get_unsigned_pair(handle);
        let e_id = tt_get_unsigned_pair(handle);
        if p_id as i32 != platform as i32 || e_id as i32 != encoding as i32 {
            tt_get_unsigned_quad(handle);
            i += 1;
        } else {
            offset = (offset as u32).wrapping_add(tt_get_unsigned_quad(handle)) as u32 as u32;
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
    (*cmap).format = tt_get_unsigned_pair(handle);
    /* Length and version (language) is ULONG for
     * format 8, 10, 12 !
     */
    if (*cmap).format as i32 <= 6i32 {
        length = tt_get_unsigned_pair(handle) as u32;
        (*cmap).language = tt_get_unsigned_pair(handle) as u32
    /* language (Mac) */
    } else if tt_get_unsigned_pair(handle) as i32 != 0i32 {
        /* reverved - 0 */
        warn!("Unrecognized cmap subtable format.");
        tt_cmap_release(cmap);
        return ptr::null_mut();
    } else {
        length = tt_get_unsigned_quad(handle);
        (*cmap).language = tt_get_unsigned_quad(handle)
    }
    match (*cmap).format as i32 {
        0 => (*cmap).map = read_cmap0(sfont, length) as *mut libc::c_void,
        2 => (*cmap).map = read_cmap2(sfont, length) as *mut libc::c_void,
        4 => (*cmap).map = read_cmap4(sfont, length) as *mut libc::c_void,
        6 => (*cmap).map = read_cmap6(sfont, length) as *mut libc::c_void,
        12 => {
            /* warn!("UCS-4 TrueType cmap table..."); */
            (*cmap).map = read_cmap12(sfont, length) as *mut libc::c_void
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

pub unsafe fn tt_cmap_release(mut cmap: *mut tt_cmap) {
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

pub unsafe fn tt_cmap_lookup(mut cmap: *mut tt_cmap, mut cc: u32) -> u16 {
    assert!(!cmap.is_null());
    if cc as i64 > 0xffff && ((*cmap).format as i32) < 12i32 {
        warn!("Four bytes charcode not supported in OpenType/TrueType cmap format 0...6.");
        return 0_u16;
    }
    match (*cmap).format as i32 {
        0 => lookup_cmap0((*cmap).map as *mut cmap0, cc as u16),
        2 => lookup_cmap2((*cmap).map as *mut cmap2, cc as u16),
        4 => lookup_cmap4((*cmap).map as *mut cmap4, cc as u16),
        6 => lookup_cmap6((*cmap).map as *mut cmap6, cc as u16),
        12 => lookup_cmap12((*cmap).map as *mut cmap12, cc),
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
    mut map: *mut cmap4,
    mut GIDToCIDMap: *mut u8,
    mut gsub_vert: *mut otl_gsub,
    mut gsub_list: *mut otl_gsub,
    mut cmap: *mut CMap,
    mut tounicode_add: *mut CMap,
) {
    let mut cid;
    let segCount = ((*map).segCountX2 as i32 / 2i32) as u16;
    let mut i = segCount as i32 - 1i32;
    while i >= 0i32 {
        let c0 = *(*map).startCount.offset(i as isize);
        let c1 = *(*map).endCount.offset(i as isize);
        let d =
            (*(*map).idRangeOffset.offset(i as isize) as i32 / 2i32 - (segCount as i32 - i)) as u16;
        let mut j = 0_u16;
        while j as i32 <= c1 as i32 - c0 as i32 {
            let ch = (c0 as i32 + j as i32) as u16;
            let mut gid = if *(*map).idRangeOffset.offset(i as isize) as i32 == 0i32 {
                (ch as i32 + *(*map).idDelta.offset(i as isize) as i32 & 0xffffi32) as u16
            } else if c0 as i32 == 0xffffi32
                && c1 as i32 == 0xffffi32
                && *(*map).idRangeOffset.offset(i as isize) as i32 == 0xffffi32
            {
                /* this is for protection against some old broken fonts... */
                0
            } else {
                (*(*map)
                    .glyphIndexArray
                    .offset((j as i32 + d as i32) as isize) as i32
                    + *(*map).idDelta.offset(i as isize) as i32
                    & 0xffffi32) as u16
            }; /* LONG ? */
            if gid as i32 != 0i32 && gid as i32 != 0xffffi32 {
                if !gsub_list.is_null() {
                    otl_gsub_apply_chain(gsub_list, &mut gid);
                }
                if !gsub_vert.is_null() {
                    otl_gsub_apply(gsub_vert, &mut gid);
                }
                if !GIDToCIDMap.is_null() {
                    cid = ((*GIDToCIDMap.offset((2i32 * gid as i32) as isize) as i32) << 8i32
                        | *GIDToCIDMap.offset((2i32 * gid as i32 + 1i32) as isize) as i32)
                        as u16;
                    if cid as i32 == 0i32 {
                        warn!("GID {} does not have corresponding CID {}.", gid, cid,);
                    }
                } else {
                    cid = gid
                }
                wbuf[0] = 0_u8;
                wbuf[1] = 0_u8;
                wbuf[2..4].copy_from_slice(&ch.to_be_bytes());
                wbuf[4..6].copy_from_slice(&cid.to_be_bytes());
                CMap_add_cidchar(cmap, wbuf.as_mut_ptr(), 4i32 as size_t, cid);
                if !tounicode_add.is_null() {
                    let mut p: *mut u8 = wbuf.as_mut_ptr().offset(6);
                    let uc_len = UC_UTF16BE_encode_char(
                        ch as i32,
                        &mut p,
                        wbuf.as_mut_ptr().offset(1024).offset(-1),
                    );
                    CMap_add_bfchar(
                        tounicode_add,
                        wbuf.as_mut_ptr().offset(4),
                        2i32 as size_t,
                        wbuf.as_mut_ptr().offset(6),
                        uc_len,
                    );
                }
            }
            j += 1;
        }
        i -= 1
    }
}
unsafe fn load_cmap12(
    mut map: *mut cmap12,
    mut GIDToCIDMap: *mut u8,
    mut gsub_vert: *mut otl_gsub,
    mut gsub_list: *mut otl_gsub,
    mut cmap: *mut CMap,
    mut tounicode_add: *mut CMap,
) {
    let mut cid;
    for i in 0..(*map).nGroups {
        for ch in (*(*map).groups.offset(i as isize)).startCharCode
            ..=(*(*map).groups.offset(i as isize)).endCharCode
        {
            let mut d: i32 =
                ch.wrapping_sub((*(*map).groups.offset(i as isize)).startCharCode) as i32;
            let mut gid = ((*(*map).groups.offset(i as isize))
                .startGlyphID
                .wrapping_add(d as u32)
                & 0xffff_u32) as u16;
            if !gsub_list.is_null() {
                otl_gsub_apply_chain(gsub_list, &mut gid);
            }
            if !gsub_vert.is_null() {
                otl_gsub_apply(gsub_vert, &mut gid);
            }
            if !GIDToCIDMap.is_null() {
                cid = ((*GIDToCIDMap.offset((2i32 * gid as i32) as isize) as i32) << 8i32
                    | *GIDToCIDMap.offset((2i32 * gid as i32 + 1i32) as isize) as i32)
                    as u16;
                if cid as i32 == 0i32 {
                    warn!("GID {} does not have corresponding CID {}.", gid, cid,);
                }
            } else {
                cid = gid
            }
            wbuf[0..4].copy_from_slice(&ch.to_be_bytes());
            wbuf[4..6].copy_from_slice(&cid.to_be_bytes());
            CMap_add_cidchar(cmap, wbuf.as_mut_ptr(), 4i32 as size_t, cid);
            if !tounicode_add.is_null() {
                let mut p: *mut u8 = wbuf.as_mut_ptr().offset(6);
                let uc_len = UC_UTF16BE_encode_char(
                    ch as i32,
                    &mut p,
                    wbuf.as_mut_ptr().offset(1024).offset(-1),
                );
                CMap_add_bfchar(
                    tounicode_add,
                    wbuf.as_mut_ptr().offset(4),
                    2i32 as size_t,
                    wbuf.as_mut_ptr().offset(6),
                    uc_len,
                );
            }
        }
    }
}
/* OpenType CIDFont:
 *
 *  We don't use GID for them. OpenType cmap table is for
 *  charcode to GID mapping rather than to-CID mapping.
 */
unsafe fn handle_CIDFont(
    mut sfont: *mut sfnt,
    mut GIDToCIDMap: *mut *mut u8,
    mut csi: *mut CIDSysInfo,
) -> i32 {
    assert!(!csi.is_null());
    let offset = sfnt_find_table_pos(sfont, b"CFF ") as i32;
    if offset == 0i32 {
        (*csi).registry = ptr::null_mut();
        (*csi).ordering = ptr::null_mut();
        *GIDToCIDMap = ptr::null_mut();
        return 0i32;
    }
    let maxp = tt_read_maxp_table(sfont);
    let num_glyphs = (*maxp).numGlyphs;
    free(maxp as *mut libc::c_void);
    if (num_glyphs as i32) < 1i32 {
        panic!("No glyph contained in this font...");
    }
    let cffont = cff_open(&mut (*sfont).handle, offset, 0i32); /* CID... */
 // TODO: use link
    if cffont.is_null() {
        panic!("Could not open CFF font...");
    }
    let cffont = &mut *cffont;
    if cffont.flag & 1i32 << 0i32 == 0 {
        cff_close(cffont);
        (*csi).registry = ptr::null_mut();
        (*csi).ordering = ptr::null_mut();
        *GIDToCIDMap = ptr::null_mut();
        return 0i32;
    }
    if cff_dict_known(cffont.topdict, b"ROS\x00" as *const u8 as *const i8) == 0 {
        panic!("No CIDSystemInfo???");
    } else {
        let reg = cff_dict_get(cffont.topdict, b"ROS\x00" as *const u8 as *const i8, 0i32) as u16;
        let ord = cff_dict_get(cffont.topdict, b"ROS\x00" as *const u8 as *const i8, 1i32) as u16;
        (*csi).registry = cff_get_string(cffont, reg);
        (*csi).ordering = cff_get_string(cffont, ord);
        (*csi).supplement =
            cff_dict_get(cffont.topdict, b"ROS\x00" as *const u8 as *const i8, 2i32) as i32
    }
    cff_read_charsets(cffont);
    let charset = cffont.charsets;
    if charset.is_null() {
        panic!("No CFF charset data???");
    }
    let mut map = new(((num_glyphs as i32 * 2i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memset(map as *mut libc::c_void, 0i32, (num_glyphs * 2) as _);
    match (*charset).format as i32 {
        0 => {
            let cids = (*charset).data.glyphs;
            let mut gid = 1_u16;
            for i in 0..(*charset).num_entries as i32 {
                *map.offset((2i32 * gid as i32) as isize) =
                    (*cids.offset(i as isize) as i32 >> 8i32 & 0xffi32) as u8;
                *map.offset((2i32 * gid as i32 + 1i32) as isize) =
                    (*cids.offset(i as isize) as i32 & 0xffi32) as u8;
                gid = gid.wrapping_add(1);
            }
        }
        1 => {
            let ranges = (*charset).data.range1;
            let mut gid = 1_u16;
            for i in 0..(*charset).num_entries as i32 {
                let mut cid = (*ranges.offset(i as isize)).first;
                let mut count = ((*ranges.offset(i as isize)).n_left as i32 + 1i32) as u16;
                loop {
                    let fresh3 = count;
                    count -= 1;
                    if !(fresh3 as i32 > 0i32 && gid as i32 <= num_glyphs as i32) {
                        break;
                    }
                    *map.offset((2i32 * gid as i32) as isize) =
                        (cid as i32 >> 8i32 & 0xffi32) as u8;
                    *map.offset((2i32 * gid as i32 + 1i32) as isize) = (cid as i32 & 0xffi32) as u8;
                    gid += 1;
                    cid += 1;
                }
            }
        }
        2 => {
            let ranges_0 = (*charset).data.range2;
            if (*charset).num_entries as i32 == 1i32 && (*ranges_0.offset(0)).first as i32 == 1i32 {
                /* "Complete" CIDFont */
                map = mfree(map as *mut libc::c_void) as *mut u8
            } else {
                /* Not trivial mapping */
                let mut gid = 1_u16;
                for i in 0..(*charset).num_entries as i32 {
                    let mut cid_0 = (*ranges_0.offset(i as isize)).first;
                    let mut count_0 = ((*ranges_0.offset(i as isize)).n_left as i32 + 1i32) as u16;
                    loop {
                        let fresh4 = count_0;
                        count_0 -= 1;
                        if !(fresh4 as i32 > 0i32 && gid as i32 <= num_glyphs as i32) {
                            break;
                        }
                        *map.offset((2i32 * gid as i32) as isize) =
                            (cid_0 as i32 >> 8i32 & 0xffi32) as u8;
                        *map.offset((2i32 * gid as i32 + 1i32) as isize) =
                            (cid_0 as i32 & 0xffi32) as u8;
                        gid += 1;
                        cid_0 += 1;
                    }
                }
            }
        }
        _ => {
            mfree(map as *mut libc::c_void) as *mut u8;
            panic!(
                "Unknown CFF charset format...: {}",
                (*charset).format as i32
            );
        }
    }
    cff_close(cffont);
    *GIDToCIDMap = map;
    1i32
}
unsafe fn is_PUA_or_presentation(mut uni: u32) -> bool {
    /* KANGXI RADICALs are commonly double encoded. */
    return uni >= 0x2f00_u32 && uni <= 0x2fd5_u32
        || uni >= 0xe000_u32 && uni <= 0xf8ff_u32
        || uni >= 0xfb00_u32 && uni <= 0xfb4f_u32
        || uni >= 0xf0000_u32 && uni <= 0xffffd_u32
        || uni >= 0x100000_u32 && uni <= 0x10fffd_u32;
}
unsafe fn sfnt_get_glyphname(
    mut post: *mut tt_post_table,
    cffont: Option<&cff_font>,
    mut gid: u16,
) -> *mut i8 {
    let mut name: *mut i8 = ptr::null_mut();
    if !post.is_null() {
        name = tt_get_glyphname(post, gid)
    }
    if name.is_null() {
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
    mut cmap: *mut CMap,
    mut cmap_add: *mut CMap,
    mut used_glyphs: *const i8,
    mut sfont: *mut sfnt,
    cffont: Option<&cff_font>,
) -> u16 {
    let mut post: *mut tt_post_table = ptr::null_mut();
    if cmap_add.is_null() {
        post = tt_read_post_table(sfont)
    }
    let mut count = 0_u16;
    for i in 0..8192 {
        if !(*used_glyphs.offset(i as isize) as i32 == 0i32) {
            for j in 0..8 {
                let mut gid: u16 = ((8i32 * i as i32) as u32).wrapping_add(j) as u16;
                if !(*used_glyphs.offset((gid as i32 / 8i32) as isize) as i32
                    & 1i32 << 7i32 - gid as i32 % 8i32
                    == 0)
                {
                    if cmap_add.is_null() {
                        /* try to look up Unicode values from the glyph name... */
                        let mut unicodes: [i32; 16] = [0; 16];
                        let mut unicode_count: i32 = -1i32;
                        let name = sfnt_get_glyphname(post, cffont, gid);
                        if !name.is_null() {
                            unicode_count = agl_get_unicodes(name, unicodes.as_mut_ptr(), 16i32)
                        }
                        if unicode_count == -1i32 {
                            if !name.is_null() {
                                info!(
                                    "No Unicode mapping available: GID={}, name={}\n",
                                    gid,
                                    CStr::from_ptr(name).display(),
                                );
                            } else {
                                info!("No Unicode mapping available: GID={}\n", gid);
                            }
                        } else {
                            /* the Unicode characters go into wbuf[2] and following, in UTF16BE */
                            /* we rely on WBUF_SIZE being more than adequate for MAX_UNICODES  */
                            let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
                            let mut len = 0;
                            for k in 0..unicode_count {
                                len = (len as u64).wrapping_add(UC_UTF16BE_encode_char(
                                    unicodes[k as usize],
                                    &mut p,
                                    wbuf.as_mut_ptr().offset(1024),
                                )) as size_t as size_t;
                            }
                            wbuf[0..2].copy_from_slice(&gid.to_be_bytes());
                            CMap_add_bfchar(
                                cmap,
                                wbuf.as_mut_ptr(),
                                2i32 as size_t,
                                wbuf.as_mut_ptr().offset(2),
                                len,
                            );
                        }
                        free(name as *mut libc::c_void);
                    } else {
                        wbuf[0] = (gid as i32 >> 8i32 & 0xffi32) as u8;
                        wbuf[1] = (gid as i32 & 0xffi32) as u8;
                        let mut inbuf = wbuf.as_mut_ptr() as *const u8;
                        let mut inbytesleft = 2i32 as size_t;
                        let mut outbuf = wbuf.as_mut_ptr().offset(2);
                        let mut outbytesleft = (1024i32 - 2i32) as size_t;
                        CMap_decode(
                            cmap_add,
                            &mut inbuf,
                            &mut inbytesleft,
                            &mut outbuf,
                            &mut outbytesleft,
                        );
                        if inbytesleft != 0i32 as u64 {
                            warn!("CMap conversion failed...");
                        } else {
                            let len = ((1024i32 - 2i32) as u64).wrapping_sub(outbytesleft);
                            CMap_add_bfchar(
                                cmap,
                                wbuf.as_mut_ptr(),
                                2i32 as size_t,
                                wbuf.as_mut_ptr().offset(2),
                                len,
                            );
                            count += 1;
                            if verbose > 0i32 {
                                info!(
                                    "otf_cmap>> Additional ToUnicode mapping: <{:04X}> <",
                                    gid as i32,
                                );
                                for _i in 0..len {
                                    info!(
                                        "{:02X}",
                                        wbuf[(2i32 as u64).wrapping_add(_i) as usize] as i32,
                                    );
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
unsafe fn prepare_CIDFont_from_sfnt<'a>(sfont: &'a mut sfnt) -> Option<&'a mut cff_font> {
    let mut offset: u32 = 0_u32;
    if (*sfont).type_0 != 1i32 << 2i32 || sfnt_read_table_directory(sfont, 0_u32) < 0i32 || {
        offset = sfnt_find_table_pos(sfont, b"CFF ");
        offset == 0_u32
    } {
        return None;
    }
    let cffont = cff_open(&mut (*sfont).handle, offset as i32, 0i32); // TODO: use link
    if cffont.is_null() {
        return None;
    }
    cff_read_charsets(&mut *cffont);
    Some(&mut *cffont)
}
unsafe fn add_to_cmap_if_used(
    mut cmap: *mut CMap,
    cffont: Option<&cff_font>,
    mut used_chars: *mut i8,
    mut gid: u16,
    mut ch: u32,
) -> u16 {
    let mut count: u16 = 0_u16;
    let mut cid: u16 = (if let Some(cffont) = cffont {
        cff_charsets_lookup_inverse(cffont, gid) as i32
    } else {
        gid as i32
    }) as u16;
    /* Skip PUA characters and alphabetic presentation forms, allowing
     * handle_subst_glyphs() as it might find better mapping. Fixes the
     * mapping of ligatures encoded in PUA in fonts like Linux Libertine
     * and old Adobe fonts.
     */
    if *used_chars.offset((cid as i32 / 8i32) as isize) as i32 & 1i32 << 7i32 - cid as i32 % 8i32
        != 0
        && !is_PUA_or_presentation(ch)
    {
        let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
        count = count.wrapping_add(1);
        wbuf[0..2].copy_from_slice(&cid.to_be_bytes());
        let len = UC_UTF16BE_encode_char(ch as i32, &mut p, wbuf.as_mut_ptr().offset(1024)) as i32;
        CMap_add_bfchar(
            cmap,
            wbuf.as_mut_ptr(),
            2i32 as size_t,
            wbuf.as_mut_ptr().offset(2),
            len as size_t,
        );
        /* Avoid duplicate entry
         * There are problem when two Unicode code is mapped to
         * single glyph...
         */
        let ref mut fresh5 = *used_chars.offset((cid as i32 / 8i32) as isize);
        *fresh5 = (*fresh5 as i32 & !(1i32 << 7i32 - cid as i32 % 8i32)) as i8
    }
    count
}
unsafe fn create_ToUnicode_cmap4(
    mut cmap: *mut CMap,
    mut map: *mut cmap4,
    mut used_chars: *mut i8,
    cffont: Option<&cff_font>,
) -> u16 {
    let mut count: u16 = 0_u16;
    let mut segCount: u16 = ((*map).segCountX2 as i32 / 2i32) as u16;
    for i in 0..segCount as i32 {
        let mut c0: u16 = *(*map).startCount.offset(i as isize);
        let mut c1: u16 = *(*map).endCount.offset(i as isize);
        let mut d: u16 = (*(*map).idRangeOffset.offset(i as isize) as i32 / 2i32
            - (segCount as i32 - i as i32)) as u16;
        let mut j = 0;
        while j as i32 <= c1 as i32 - c0 as i32 {
            let mut ch: u16 = (c0 as i32 + j as i32) as u16;
            let gid = if *(*map).idRangeOffset.offset(i as isize) as i32 == 0i32 {
                (ch as i32 + *(*map).idDelta.offset(i as isize) as i32 & 0xffffi32) as u16
            } else if c0 as i32 == 0xffffi32
                && c1 as i32 == 0xffffi32
                && *(*map).idRangeOffset.offset(i as isize) as i32 == 0xffffi32
            {
                /* this is for protection against some old broken fonts... */
                0
            } else {
                (*(*map)
                    .glyphIndexArray
                    .offset((j as i32 + d as i32) as isize) as i32
                    + *(*map).idDelta.offset(i as isize) as i32
                    & 0xffffi32) as u16
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
    mut cmap: *mut CMap,
    mut map: *mut cmap12,
    mut used_chars: *mut i8,
    cffont: Option<&cff_font>,
) -> u16 {
    let mut count: u32 = 0_u32;
    for i in 0..(*map).nGroups {
        for ch in (*(*map).groups.offset(i as isize)).startCharCode
            ..=(*(*map).groups.offset(i as isize)).endCharCode
        {
            let mut d: i32 =
                ch.wrapping_sub((*(*map).groups.offset(i as isize)).startCharCode) as i32;
            let mut gid: u16 = ((*(*map).groups.offset(i as isize))
                .startGlyphID
                .wrapping_add(d as u32)
                & 0xffff_u32) as u16;
            count += add_to_cmap_if_used(cmap, cffont, used_chars, gid, ch) as u32;
        }
    }
    count as u16
}
unsafe fn create_ToUnicode_cmap(
    mut ttcmap: *mut tt_cmap,
    mut cmap_name: *const i8,
    mut cmap_add: *mut CMap,
    mut used_chars: *const i8,
    mut sfont: *mut sfnt,
    mut code_to_cid_cmap: *mut CMap,
) -> *mut pdf_obj {
    let mut count: u16 = 0_u16;
    let mut cffont = prepare_CIDFont_from_sfnt(&mut *sfont);
    let mut is_cidfont = if let Some(cffont) = &cffont {
        cffont.flag & 1i32 << 0i32 != 0
    } else {
        false
    };
    let cmap = CMap_new();
    CMap_set_name(cmap, &CStr::from_ptr(cmap_name).to_string_lossy());
    CMap_set_wmode(cmap, 0i32);
    CMap_set_type(cmap, 2i32);
    CMap_set_CIDSysInfo(cmap, &mut CSI_UNICODE);
    CMap_add_codespacerange(
        cmap,
        srange_min.as_mut_ptr(),
        srange_max.as_mut_ptr(),
        2i32 as size_t,
    );
    /* cmap_add here stores information about all unencoded glyphs which can be
     * accessed only through OT Layout GSUB table.
     */
    if !code_to_cid_cmap.is_null() && cffont.is_some() && is_cidfont && cmap_add.is_null() {
        for i in 0..8192 {
            if !(*used_chars.offset(i as isize) as i32 == 0i32) {
                for j in 0..8 {
                    let mut cid: u16 = (8i32 * i as i32 + j) as u16;
                    if !(*used_chars.offset((cid as i32 / 8i32) as isize) as i32
                        & 1i32 << 7i32 - cid as i32 % 8i32
                        == 0)
                    {
                        let ch = CMap_reverse_decode(code_to_cid_cmap, cid);
                        if ch >= 0i32 {
                            let mut p: *mut u8 = wbuf.as_mut_ptr().offset(2);
                            wbuf[0] = (cid as i32 >> 8i32 & 0xffi32) as u8;
                            wbuf[1] = (cid as i32 & 0xffi32) as u8;
                            let len =
                                UC_UTF16BE_encode_char(ch, &mut p, wbuf.as_mut_ptr().offset(1024))
                                    as i32;
                            CMap_add_bfchar(
                                cmap,
                                wbuf.as_mut_ptr(),
                                2i32 as size_t,
                                wbuf.as_mut_ptr().offset(2),
                                len as size_t,
                            );
                            count = count.wrapping_add(1)
                        }
                    }
                }
            }
        }
    } else {
        let mut used_chars_copy: [i8; 8192] = [0; 8192];
        memcpy(
            used_chars_copy.as_mut_ptr() as *mut libc::c_void,
            used_chars as *const libc::c_void,
            8192,
        );
        /* For create_ToUnicode_cmap{4,12}(), cffont is for GID -> CID lookup,
         * so it is only needed for CID fonts. */
        match (*ttcmap).format as i32 {
            4 => {
                count = create_ToUnicode_cmap4(
                    cmap,
                    (*ttcmap).map as *mut cmap4,
                    used_chars_copy.as_mut_ptr(),
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
                    cmap,
                    (*ttcmap).map as *mut cmap12,
                    used_chars_copy.as_mut_ptr(),
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
                cmap,
                cmap_add,
                used_chars_copy.as_mut_ptr(),
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
    let stream = if (count as i32) < 1i32 {
        ptr::null_mut()
    } else {
        CMap_create_stream(cmap)
    };
    CMap_release(cmap);
    if let Some(cffont) = cffont {
        cff_close(cffont);
    }
    stream
}
static mut cmap_plat_encs: [cmap_plat_enc_rec; 5] = [
    cmap_plat_enc_rec {
        platform: 3_i16,
        encoding: 10_i16,
    },
    cmap_plat_enc_rec {
        platform: 0_i16,
        encoding: 3_i16,
    },
    cmap_plat_enc_rec {
        platform: 0_i16,
        encoding: 0_i16,
    },
    cmap_plat_enc_rec {
        platform: 3_i16,
        encoding: 1_i16,
    },
    cmap_plat_enc_rec {
        platform: 0_i16,
        encoding: 1_i16,
    },
];

pub unsafe fn otf_create_ToUnicode_stream(
    mut font_name: *const i8,
    mut ttc_index: i32,
    mut used_chars: *const i8,
    mut cmap_id: i32,
) -> *mut pdf_obj {
    let mut cmap_obj: *mut pdf_obj = ptr::null_mut();
    let mut ttcmap: *mut tt_cmap = ptr::null_mut();
    let offset;
    /* replace slash in map name with dash to make the output cmap name valid,
     * happens when XeTeX embeds full font path
     * https://sourceforge.net/p/xetex/bugs/52/
     */
    let normalized_font_name =
        new((strlen(font_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy(normalized_font_name, font_name);
    for i in 0..strlen(font_name) as _ {
        if *normalized_font_name.offset(i as isize) as i32 == '/' as i32 {
            *normalized_font_name.offset(i as isize) = '-' as i32 as i8
        }
    }
    let cmap_name = new((strlen(font_name)
        .wrapping_add(strlen(b"-UTF16\x00" as *const u8 as *const i8))
        .wrapping_add(5))
    .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    sprintf(
        cmap_name,
        b"%s,%03d-UTF16\x00" as *const u8 as *const i8,
        normalized_font_name,
        ttc_index,
    );
    free(normalized_font_name as *mut libc::c_void);
    let res_id = pdf_findresource(b"CMap\x00" as *const u8 as *const i8, cmap_name);
    if res_id >= 0i32 {
        free(cmap_name as *mut libc::c_void);
        let cmap_ref = pdf_get_resource_reference(res_id);
        return cmap_ref;
    }
    if verbose > 0i32 {
        info!("\n");
        info!(
            "otf_cmap>> Creating ToUnicode CMap for \"{}\"...\n",
            CStr::from_ptr(font_name).display()
        );
    }
    let sfont = if let Some(handle) =
        dpx_open_truetype_file(font_name).or_else(|| dpx_open_opentype_file(font_name))
    {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(font_name) {
        dfont_open(handle, ttc_index)
    } else {
        free(cmap_name as *mut libc::c_void);
        return ptr::null_mut();
    };
    if sfont.is_null() {
        panic!(
            "Could not open OpenType/TrueType font file \"{}\"",
            CStr::from_ptr(font_name).display()
        );
    }
    match (*sfont).type_0 {
        256 => offset = (*sfont).offset,
        16 => {
            offset = ttc_read_offset(sfont, ttc_index);
            if offset == 0_u32 {
                panic!("Invalid TTC index");
            }
        }
        _ => offset = 0_u32,
    }
    if sfnt_read_table_directory(sfont, offset) < 0i32 {
        panic!("Could not read OpenType/TrueType table directory.");
    }
    let mut code_to_cid_cmap = CMap_cache_get(cmap_id);
    let cmap_type = CMap_get_type(code_to_cid_cmap);
    if cmap_type != 1i32 {
        code_to_cid_cmap = ptr::null_mut()
    }
    let cmap_add_id = CMap_cache_find(&format!(
        "{},{:03}-UCS32-Add",
        CStr::from_ptr(font_name).display(),
        ttc_index,
    ));
    let cmap_add = if cmap_add_id < 0i32 {
        ptr::null_mut()
    } else {
        CMap_cache_get(cmap_add_id)
    };
    CMap_set_silent(1i32); /* many warnings without this... */
    for i in 0..(::std::mem::size_of::<[cmap_plat_enc_rec; 5]>() as u64)
        .wrapping_div(::std::mem::size_of::<cmap_plat_enc_rec>() as u64) as usize
    {
        ttcmap = tt_cmap_read(
            sfont,
            cmap_plat_encs[i].platform as u16,
            cmap_plat_encs[i].encoding as u16,
        );
        if !ttcmap.is_null() {
            if (*ttcmap).format as i32 == 4i32 || (*ttcmap).format as i32 == 12i32 {
                cmap_obj = create_ToUnicode_cmap(
                    ttcmap,
                    cmap_name,
                    cmap_add,
                    used_chars,
                    sfont,
                    code_to_cid_cmap,
                );
                break;
            }
        }
    }
    if cmap_obj.is_null() {
        warn!("Unable to read OpenType/TrueType Unicode cmap table.");
    }
    tt_cmap_release(ttcmap);
    CMap_set_silent(0i32);
    let cmap_ref = if !cmap_obj.is_null() {
        let res_id = pdf_defineresource(
            b"CMap\x00" as *const u8 as *const i8,
            cmap_name,
            cmap_obj,
            1i32,
        );
        pdf_get_resource_reference(res_id)
    } else {
        ptr::null_mut()
    };
    free(cmap_name as *mut libc::c_void);
    sfnt_close(sfont);
    cmap_ref
}
unsafe fn load_base_CMap(
    mut cmap_name: &str,
    mut tounicode_add: *mut CMap,
    mut wmode: i32,
    mut csi: *mut CIDSysInfo,
    mut GIDToCIDMap: *mut u8,
    mut gsub_vert: *mut otl_gsub,
    mut gsub_list: *mut otl_gsub,
    mut ttcmap: *mut tt_cmap,
) -> i32 {
    let mut cmap_id = CMap_cache_find(cmap_name);
    if cmap_id < 0i32 {
        let cmap = CMap_new();
        CMap_set_name(cmap, cmap_name);
        CMap_set_type(cmap, 1i32);
        CMap_set_wmode(cmap, wmode);
        CMap_add_codespacerange(
            cmap,
            lrange_min.as_mut_ptr(),
            lrange_max.as_mut_ptr(),
            4i32 as size_t,
        );
        if !csi.is_null() {
            /* CID */
            CMap_set_CIDSysInfo(cmap, csi);
        } else {
            CMap_set_CIDSysInfo(cmap, &mut CSI_IDENTITY);
        }
        if (*ttcmap).format as i32 == 12i32 {
            load_cmap12(
                (*ttcmap).map as *mut cmap12,
                GIDToCIDMap,
                gsub_vert,
                gsub_list,
                cmap,
                tounicode_add,
            );
        } else if (*ttcmap).format as i32 == 4i32 {
            load_cmap4(
                (*ttcmap).map as *mut cmap4,
                GIDToCIDMap,
                gsub_vert,
                gsub_list,
                cmap,
                tounicode_add,
            );
        }
        cmap_id = CMap_cache_add(cmap)
    }
    cmap_id
}
/* TrueType cmap table */
/* or version, only for Mac */
/* Paltform ID */
/* Platform-specific encoding ID */
/* Windows */
/* Mac */
/* Indirect reference */
/* CMap ID */

pub unsafe fn otf_load_Unicode_CMap(
    mut map_name: *const i8,
    mut ttc_index: i32,
    mut otl_tags: *const i8,
    mut wmode: i32,
) -> i32 {
    /* Additional ToUnicode mappings required by OTL GSUB substitusion */
    let mut tounicode_add: *mut CMap = ptr::null_mut();
    let mut offset;
    let cmap_name;
    let mut gsub_vert;
    let gsub_list;
    let mut csi: CIDSysInfo = CIDSysInfo {
        registry: ptr::null_mut(),
        ordering: ptr::null_mut(),
        supplement: 0i32,
    };
    let mut GIDToCIDMap: *mut u8 = ptr::null_mut();
    if map_name.is_null() {
        return -1i32;
    }
    if ttc_index > 999i32 || ttc_index < 0i32 {
        return -1i32;
        /* Sorry for this... */
    }
    let mut handle = dpx_open_truetype_file(map_name);
    if handle.is_none() {
        handle = dpx_open_opentype_file(map_name);
    }
    let sfont = if handle.is_none() {
        let handle = dpx_open_dfont_file(map_name);
        if handle.is_none() {
            return -1i32;
        }
        dfont_open(handle.unwrap(), ttc_index)
    } else {
        sfnt_open(handle.unwrap())
    };

    let map_name = CStr::from_ptr(map_name).to_string_lossy().to_string();

    if sfont.is_null() {
        panic!(
            "Could not open OpenType/TrueType/dfont font file \"{}\"",
            map_name
        );
    }
    match (*sfont).type_0 {
        16 => {
            offset = ttc_read_offset(sfont, ttc_index);
            if offset == 0_u32 {
                panic!("Invalid TTC index");
            }
        }
        1 | 4 => offset = 0_u32,
        256 => offset = (*sfont).offset,
        _ => {
            panic!("Not a OpenType/TrueType/TTC font?: {}", map_name,);
        }
    }
    if sfnt_read_table_directory(sfont, offset) < 0i32 {
        panic!("Could not read OpenType/TrueType table directory.");
    }

    let base_name = if wmode != 0 {
        format!("{},{:03}-UCS4-V", map_name, ttc_index)
    } else {
        format!("{},{:03}-UCS4-H", map_name, ttc_index)
    };

    if !otl_tags.is_null() {
        cmap_name = if wmode != 0 {
            format!(
                "{},{:03},{}-UCS4-V",
                map_name,
                ttc_index,
                CStr::from_ptr(otl_tags).to_string_lossy()
            )
        } else {
            format!(
                "{},{:03},{}-UCS4-H",
                map_name,
                ttc_index,
                CStr::from_ptr(otl_tags).to_string_lossy()
            )
        };
        /* tounicode_add here is later refered by otf_create_ToUnicode_stream()
         * for finding additional CID to Unicode mapping entries required by
         * OTL gsub substitution.
         */

        let tounicode_add_name = format!("{},{:03}-UCS32-Add", map_name, ttc_index);
        let mut tounicode_add_id = CMap_cache_find(&tounicode_add_name); /* Unicode 2.0 or later */
        if tounicode_add_id >= 0i32 {
            tounicode_add = CMap_cache_get(tounicode_add_id)
        } else {
            tounicode_add = CMap_new();
            CMap_set_name(tounicode_add, &tounicode_add_name);
            CMap_set_type(tounicode_add, 2i32);
            CMap_set_wmode(tounicode_add, 0i32);
            CMap_add_codespacerange(
                tounicode_add,
                srange_min.as_mut_ptr(),
                srange_max.as_mut_ptr(),
                2i32 as size_t,
            );
            CMap_set_CIDSysInfo(tounicode_add, &mut CSI_UNICODE);
            CMap_add_bfchar(
                tounicode_add,
                srange_min.as_mut_ptr(),
                2i32 as size_t,
                srange_max.as_mut_ptr(),
                2i32 as size_t,
            );
            CMap_cache_add(tounicode_add);
        }
    } else {
        cmap_name = base_name;
    }
    let is_cidfont = if (*sfont).type_0 == 1i32 << 2i32 {
        handle_CIDFont(sfont, &mut GIDToCIDMap, &mut csi)
    } else {
        0
    };
    if verbose > 0i32 {
        info!("\n");
        info!(
            "otf_cmap>> Unicode charmap for font=\"{}\" layout=\"{}\"\n",
            map_name,
            if !otl_tags.is_null() {
                CStr::from_ptr(otl_tags).to_str().unwrap()
            } else {
                "none"
            },
        );
    }
    let cmap_id = CMap_cache_find(&cmap_name);
    if cmap_id >= 0i32 {
        free(GIDToCIDMap as *mut libc::c_void);
        sfnt_close(sfont);
        if verbose > 0i32 {
            info!("otf_cmap>> Found at cmap_id={}.\n", cmap_id);
        }
        return cmap_id;
    }
    let mut ttcmap = tt_cmap_read(sfont, 3_u16, 10_u16);
    if ttcmap.is_null() {
        ttcmap = tt_cmap_read(sfont, 3_u16, 1_u16);
        if ttcmap.is_null() {
            ttcmap = tt_cmap_read(sfont, 0_u16, 3_u16);
            if ttcmap.is_null() {
                panic!("Unable to read OpenType/TrueType Unicode cmap table.");
            }
        }
    }
    if wmode == 1i32 {
        gsub_vert = otl_gsub_new();
        if otl_gsub_add_feat(gsub_vert, b"*", b"*", b"vrt2", sfont) < 0i32 {
            if otl_gsub_add_feat(gsub_vert, b"*", b"*", b"vert", sfont) < 0i32 {
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
    if !otl_tags.is_null() {
        gsub_list = otl_gsub_new();
        if otl_gsub_add_feat_list(gsub_list, otl_tags, sfont) < 0i32 {
            warn!(
                "Reading GSUB feature table(s) failed for \"{}\"",
                CStr::from_ptr(otl_tags).display(),
            );
        } else {
            otl_gsub_set_chain(gsub_list, otl_tags);
        }
    } else {
        gsub_list = ptr::null_mut()
    }
    let cmap_id = load_base_CMap(
        &cmap_name,
        tounicode_add,
        wmode,
        if is_cidfont != 0 {
            &mut csi
        } else {
            ptr::null_mut()
        },
        GIDToCIDMap,
        gsub_vert,
        gsub_list,
        ttcmap,
    );
    if cmap_id < 0i32 {
        panic!("Failed to read OpenType/TrueType cmap table.");
    }
    if !gsub_vert.is_null() {
        otl_gsub_release(gsub_vert);
    }
    if !gsub_list.is_null() {
        otl_gsub_release(gsub_list);
    }
    free(GIDToCIDMap as *mut libc::c_void);
    if is_cidfont != 0 {
        free(csi.registry as *mut libc::c_void);
        free(csi.ordering as *mut libc::c_void);
    }
    tt_cmap_release(ttcmap);
    sfnt_close(sfont);
    cmap_id
}
