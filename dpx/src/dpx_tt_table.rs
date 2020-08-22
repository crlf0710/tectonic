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
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use super::dpx_numbers::{
    tt_get_signed_byte, tt_get_signed_pair, GetFromFile,
    tt_get_unsigned_quad,
};
use crate::warn;

use super::dpx_mem::new;
use super::dpx_sfnt::{sfnt_find_table_len, sfnt_find_table_pos, sfnt_locate_table};
use crate::bridge::ttstub_input_read;
use crate::dpx_truetype::sfnt_table_info;

use std::ffi::CStr;
use std::io::{Seek, SeekFrom};
use std::ptr;

pub(crate) type __ssize_t = i64;
use crate::bridge::size_t;
pub(crate) type Fixed = u32;
pub(crate) type FWord = i16;
pub(crate) type uFWord = u16;

use super::dpx_sfnt::{put_big_endian, sfnt};

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_head_table {
    pub(crate) version: Fixed,
    pub(crate) fontRevision: Fixed,
    pub(crate) checkSumAdjustment: u32,
    pub(crate) magicNumber: u32,
    pub(crate) flags: u16,
    pub(crate) unitsPerEm: u16,
    pub(crate) created: [u8; 8],
    pub(crate) modified: [u8; 8],
    pub(crate) xMin: FWord,
    pub(crate) yMin: FWord,
    pub(crate) xMax: FWord,
    pub(crate) yMax: FWord,
    pub(crate) macStyle: u16,
    pub(crate) lowestRecPPEM: u16,
    pub(crate) fontDirectionHint: i16,
    pub(crate) indexToLocFormat: i16,
    pub(crate) glyphDataFormat: i16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_hhea_table {
    pub(crate) version: Fixed,
    pub(crate) ascent: FWord,
    pub(crate) descent: FWord,
    pub(crate) lineGap: FWord,
    pub(crate) advanceWidthMax: uFWord,
    pub(crate) minLeftSideBearing: FWord,
    pub(crate) minRightSideBearing: FWord,
    pub(crate) xMaxExtent: FWord,
    pub(crate) caretSlopeRise: i16,
    pub(crate) caretSlopeRun: i16,
    pub(crate) caretOffset: FWord,
    pub(crate) reserved: [i16; 4],
    pub(crate) metricDataFormat: i16,
    pub(crate) numOfLongHorMetrics: u16,
    pub(crate) numOfExSideBearings: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_vhea_table {
    pub(crate) version: Fixed,
    pub(crate) vertTypoAscender: i16,
    pub(crate) vertTypoDescender: i16,
    pub(crate) vertTypoLineGap: i16,
    pub(crate) advanceHeightMax: i16,
    pub(crate) minTopSideBearing: i16,
    pub(crate) minBottomSideBearing: i16,
    pub(crate) yMaxExtent: i16,
    pub(crate) caretSlopeRise: i16,
    pub(crate) caretSlopeRun: i16,
    pub(crate) caretOffset: i16,
    pub(crate) reserved: [i16; 4],
    pub(crate) metricDataFormat: i16,
    pub(crate) numOfLongVerMetrics: u16,
    pub(crate) numOfExSideBearings: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_maxp_table {
    pub(crate) version: Fixed,
    pub(crate) numGlyphs: u16,
    pub(crate) maxPoints: u16,
    pub(crate) maxContours: u16,
    pub(crate) maxComponentPoints: u16,
    pub(crate) maxComponentContours: u16,
    pub(crate) maxZones: u16,
    pub(crate) maxTwilightPoints: u16,
    pub(crate) maxStorage: u16,
    pub(crate) maxFunctionDefs: u16,
    pub(crate) maxInstructionDefs: u16,
    pub(crate) maxStackElements: u16,
    pub(crate) maxSizeOfInstructions: u16,
    pub(crate) maxComponentElements: u16,
    pub(crate) maxComponentDepth: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_os2__table {
    pub(crate) version: u16,
    pub(crate) xAvgCharWidth: i16,
    pub(crate) usWeightClass: u16,
    pub(crate) usWidthClass: u16,
    pub(crate) fsType: i16,
    pub(crate) ySubscriptXSize: i16,
    pub(crate) ySubscriptYSize: i16,
    pub(crate) ySubscriptXOffset: i16,
    pub(crate) ySubscriptYOffset: i16,
    pub(crate) ySuperscriptXSize: i16,
    pub(crate) ySuperscriptYSize: i16,
    pub(crate) ySuperscriptXOffset: i16,
    pub(crate) ySuperscriptYOffset: i16,
    pub(crate) yStrikeoutSize: i16,
    pub(crate) yStrikeoutPosition: i16,
    pub(crate) sFamilyClass: i16,
    pub(crate) panose: [u8; 10],
    pub(crate) ulUnicodeRange1: u32,
    pub(crate) ulUnicodeRange2: u32,
    pub(crate) ulUnicodeRange3: u32,
    pub(crate) ulUnicodeRange4: u32,
    pub(crate) achVendID: [i8; 4],
    pub(crate) fsSelection: u16,
    pub(crate) usFirstCharIndex: u16,
    pub(crate) usLastCharIndex: u16,
    pub(crate) sTypoAscender: i16,
    pub(crate) sTypoDescender: i16,
    pub(crate) sTypoLineGap: i16,
    pub(crate) usWinAscent: u16,
    pub(crate) usWinDescent: u16,
    pub(crate) ulCodePageRange1: u32,
    pub(crate) ulCodePageRange2: u32,
    pub(crate) sxHeight: i16,
    pub(crate) sCapHeight: i16,
    pub(crate) usDefaultChar: u16,
    pub(crate) usBreakChar: u16,
    pub(crate) usMaxContext: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_vertOriginYMetrics {
    pub(crate) glyphIndex: u16,
    pub(crate) vertOriginY: i16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_VORG_table {
    pub(crate) defaultVertOriginY: i16,
    pub(crate) numVertOriginYMetrics: u16,
    pub(crate) vertOriginYMetrics: *mut tt_vertOriginYMetrics,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct tt_longMetrics {
    pub(crate) advance: u16,
    pub(crate) sideBearing: i16,
}
/*
  tables contains information refered by other tables
  maxp->numGlyphs, etc --> loca, etc
  hhea->numOfLongHorMetrics --> hmtx
  head->indexToLocFormat --> loca
  head->glyphDataFormat --> glyf
*/

pub(crate) unsafe fn tt_pack_head_table(table: *mut tt_head_table) -> *mut i8 {
    if table.is_null() {
        panic!("passed NULL pointer\n");
    }
    let data = new((54u64 as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    let mut p = data;
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).version as i32, 4i32) as isize);
    p = p.offset(
        put_big_endian(p as *mut libc::c_void, (*table).fontRevision as i32, 4i32) as isize,
    );
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).checkSumAdjustment as i32,
        4i32,
    ) as isize);
    p = p
        .offset(put_big_endian(p as *mut libc::c_void, (*table).magicNumber as i32, 4i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).flags as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).unitsPerEm as i32, 2i32) as isize);
    for i in 0..8 {
        let fresh0 = p;
        p = p.offset(1);
        *fresh0 = (*table).created[i] as i8;
    }
    for i in 0..8 {
        let fresh1 = p;
        p = p.offset(1);
        *fresh1 = (*table).modified[i] as i8;
    }
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).xMin as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).yMin as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).xMax as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).yMax as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).macStyle as i32, 2i32) as isize);
    p = p.offset(
        put_big_endian(p as *mut libc::c_void, (*table).lowestRecPPEM as i32, 2i32) as isize,
    );
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).fontDirectionHint as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).indexToLocFormat as i32,
        2i32,
    ) as isize);
    p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).glyphDataFormat as i32,
        2i32,
    ) as isize);
    data
}

pub(crate) unsafe fn tt_read_head_table(sfont: *mut sfnt) -> *mut tt_head_table {
    let mut table: *mut tt_head_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_head_table>() as u64)
        as u32) as *mut tt_head_table;
    sfnt_locate_table(sfont, sfnt_table_info::HEAD);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).fontRevision = tt_get_unsigned_quad(handle);
    (*table).checkSumAdjustment = tt_get_unsigned_quad(handle);
    (*table).magicNumber = tt_get_unsigned_quad(handle);
    (*table).flags = u16::get(handle);
    (*table).unitsPerEm = u16::get(handle);
    for i in 0..8 {
        (*table).created[i] = u8::get(handle);
    }
    for i in 0..8 {
        (*table).modified[i] = u8::get(handle);
    }
    (*table).xMin = tt_get_signed_pair(handle);
    (*table).yMin = tt_get_signed_pair(handle);
    (*table).xMax = tt_get_signed_pair(handle);
    (*table).yMax = tt_get_signed_pair(handle);
    (*table).macStyle = tt_get_signed_pair(handle) as u16;
    (*table).lowestRecPPEM = tt_get_signed_pair(handle) as u16;
    (*table).fontDirectionHint = tt_get_signed_pair(handle);
    (*table).indexToLocFormat = tt_get_signed_pair(handle);
    (*table).glyphDataFormat = tt_get_signed_pair(handle);
    table
}

pub(crate) unsafe fn tt_pack_maxp_table(table: *mut tt_maxp_table) -> *mut i8 {
    let data = new((32u64 as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    let mut p = data;
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).version as i32, 4i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).numGlyphs as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).maxPoints as i32, 2i32) as isize);
    p = p
        .offset(put_big_endian(p as *mut libc::c_void, (*table).maxContours as i32, 2i32) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxComponentPoints as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxComponentContours as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).maxZones as i32, 2i32) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxTwilightPoints as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).maxStorage as i32, 2i32) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxFunctionDefs as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxInstructionDefs as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxStackElements as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxSizeOfInstructions as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).maxComponentElements as i32,
        2i32,
    ) as isize);
    put_big_endian(
        p as *mut libc::c_void,
        (*table).maxComponentDepth as i32,
        2i32,
    );
    data
}

pub(crate) unsafe fn tt_read_maxp_table(sfont: *mut sfnt) -> *mut tt_maxp_table {
    let mut table: *mut tt_maxp_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_maxp_table>() as u64)
        as u32) as *mut tt_maxp_table;
    sfnt_locate_table(sfont, sfnt_table_info::MAXP);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).numGlyphs = u16::get(handle);
    (*table).maxPoints = u16::get(handle);
    (*table).maxContours = u16::get(handle);
    (*table).maxComponentPoints = u16::get(handle);
    (*table).maxComponentContours = u16::get(handle);
    (*table).maxZones = u16::get(handle);
    (*table).maxTwilightPoints = u16::get(handle);
    (*table).maxStorage = u16::get(handle);
    (*table).maxFunctionDefs = u16::get(handle);
    (*table).maxInstructionDefs = u16::get(handle);
    (*table).maxStackElements = u16::get(handle);
    (*table).maxSizeOfInstructions = u16::get(handle);
    (*table).maxComponentElements = u16::get(handle);
    (*table).maxComponentDepth = u16::get(handle);
    table
}

pub(crate) unsafe fn tt_pack_hhea_table(table: *mut tt_hhea_table) -> *mut i8 {
    let data = new((36u64 as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    let mut p = data;
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).version as i32, 4i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).ascent as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).descent as i32, 2i32) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).lineGap as i32, 2i32) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).advanceWidthMax as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).minLeftSideBearing as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).minRightSideBearing as i32,
        2i32,
    ) as isize);
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*table).xMaxExtent as i32, 2i32) as isize);
    p = p.offset(
        put_big_endian(p as *mut libc::c_void, (*table).caretSlopeRise as i32, 2i32) as isize,
    );
    p = p.offset(
        put_big_endian(p as *mut libc::c_void, (*table).caretSlopeRun as i32, 2i32) as isize,
    );
    p = p
        .offset(put_big_endian(p as *mut libc::c_void, (*table).caretOffset as i32, 2i32) as isize);
    for i in 0..4 {
        p = p.offset(
            put_big_endian(p as *mut libc::c_void, (*table).reserved[i] as i32, 2i32) as isize,
        );
    }
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).metricDataFormat as i32,
        2i32,
    ) as isize);
    p.offset(put_big_endian(
        p as *mut libc::c_void,
        (*table).numOfLongHorMetrics as i32,
        2i32,
    ) as isize);
    data
}

pub(crate) unsafe fn tt_read_hhea_table(sfont: *mut sfnt) -> *mut tt_hhea_table {
    let mut table: *mut tt_hhea_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_hhea_table>() as u64)
        as u32) as *mut tt_hhea_table;
    sfnt_locate_table(sfont, sfnt_table_info::HHEA);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).ascent = tt_get_signed_pair(handle);
    (*table).descent = tt_get_signed_pair(handle);
    (*table).lineGap = tt_get_signed_pair(handle);
    (*table).advanceWidthMax = u16::get(handle);
    (*table).minLeftSideBearing = tt_get_signed_pair(handle);
    (*table).minRightSideBearing = tt_get_signed_pair(handle);
    (*table).xMaxExtent = tt_get_signed_pair(handle);
    (*table).caretSlopeRise = tt_get_signed_pair(handle);
    (*table).caretSlopeRun = tt_get_signed_pair(handle);
    (*table).caretOffset = tt_get_signed_pair(handle);
    for i in 0..4 {
        (*table).reserved[i] = tt_get_signed_pair(handle);
    }
    (*table).metricDataFormat = tt_get_signed_pair(handle);
    if (*table).metricDataFormat as i32 != 0i32 {
        panic!("unknown metricDataFormat");
    }
    (*table).numOfLongHorMetrics = u16::get(handle);
    let len = sfnt_find_table_len(sfont, sfnt_table_info::HMTX);
    (*table).numOfExSideBearings = len
        .wrapping_sub(((*table).numOfLongHorMetrics as i32 * 4i32) as u32)
        .wrapping_div(2_u32) as u16;
    table
}
/* vhea */

pub(crate) unsafe fn tt_read_vhea_table(sfont: *mut sfnt) -> *mut tt_vhea_table {
    let mut table: *mut tt_vhea_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_vhea_table>() as u64)
        as u32) as *mut tt_vhea_table;
    sfnt_locate_table(sfont, b"vhea");
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).vertTypoAscender = tt_get_signed_pair(handle);
    (*table).vertTypoDescender = tt_get_signed_pair(handle);
    (*table).vertTypoLineGap = tt_get_signed_pair(handle);
    (*table).advanceHeightMax = tt_get_signed_pair(handle);
    (*table).minTopSideBearing = tt_get_signed_pair(handle);
    (*table).minBottomSideBearing = tt_get_signed_pair(handle);
    (*table).yMaxExtent = tt_get_signed_pair(handle);
    (*table).caretSlopeRise = tt_get_signed_pair(handle);
    (*table).caretSlopeRun = tt_get_signed_pair(handle);
    (*table).caretOffset = tt_get_signed_pair(handle);
    for i in 0..4 {
        (*table).reserved[i] = tt_get_signed_pair(handle);
    }
    (*table).metricDataFormat = tt_get_signed_pair(handle);
    (*table).numOfLongVerMetrics = u16::get(handle);
    let len = sfnt_find_table_len(sfont, b"vmtx");
    (*table).numOfExSideBearings = len
        .wrapping_sub(((*table).numOfLongVerMetrics as i32 * 4i32) as u32)
        .wrapping_div(2_u32) as u16;
    table
}

pub(crate) unsafe fn tt_read_VORG_table(sfont: *mut sfnt) -> *mut tt_VORG_table {
    let offset = sfnt_find_table_pos(sfont, b"VORG");
    let handle = &mut (*sfont).handle;
    if offset > 0_u32 {
        let vorg = new((1_u64).wrapping_mul(::std::mem::size_of::<tt_VORG_table>() as u64) as u32)
            as *mut tt_VORG_table;
        sfnt_locate_table(sfont, b"VORG");
        if u16::get(handle) as i32 != 1i32
            || u16::get(handle) as i32 != 0i32
        {
            panic!("Unsupported VORG version.");
        }
        (*vorg).defaultVertOriginY = tt_get_signed_pair(handle);
        (*vorg).numVertOriginYMetrics = u16::get(handle);
        (*vorg).vertOriginYMetrics = new(((*vorg).numVertOriginYMetrics as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<tt_vertOriginYMetrics>() as u64)
            as u32) as *mut tt_vertOriginYMetrics;
        /*
         * The vertOriginYMetrics array must be sorted in increasing
         * glyphIndex order.
         */
        for i in 0..(*vorg).numVertOriginYMetrics {
            (*(*vorg).vertOriginYMetrics.offset(i as isize)).glyphIndex =
                u16::get(handle);
            (*(*vorg).vertOriginYMetrics.offset(i as isize)).vertOriginY =
                tt_get_signed_pair(handle);
        }
        vorg
    } else {
        ptr::null_mut()
    }
}
/*
 * hmtx and vmtx
 *
 *  Reading/writing hmtx and vmtx depend on other tables, maxp and hhea/vhea.
 */

pub(crate) unsafe fn tt_read_longMetrics(
    sfont: *mut sfnt,
    numGlyphs: u16,
    numLongMetrics: u16,
    numExSideBearings: u16,
) -> *mut tt_longMetrics {
    let mut last_adv: u16 = 0_u16;
    let mut last_esb: i16 = 0_i16;
    let m = new((numGlyphs as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<tt_longMetrics>() as u64) as u32)
        as *mut tt_longMetrics;
    let handle = &mut (*sfont).handle;
    for gid in 0..numGlyphs {
        if (gid as i32) < numLongMetrics as i32 {
            last_adv = u16::get(handle)
        }
        if (gid as i32) < numLongMetrics as i32 + numExSideBearings as i32 {
            last_esb = tt_get_signed_pair(handle)
        }
        (*m.offset(gid as isize)).advance = last_adv;
        (*m.offset(gid as isize)).sideBearing = last_esb;
    }
    m
}
/* OS/2 table */
/* this table may not exist */

pub(crate) unsafe fn tt_read_os2__table(sfont: *mut sfnt) -> *mut tt_os2__table {
    let table = new((1_u64).wrapping_mul(::std::mem::size_of::<tt_os2__table>() as u64) as u32)
        as *mut tt_os2__table;
    let handle = &mut (*sfont).handle;
    if sfnt_find_table_pos(sfont, sfnt_table_info::OS_2) > 0_u32 {
        sfnt_locate_table(sfont, sfnt_table_info::OS_2);
        (*table).version = u16::get(handle);
        (*table).xAvgCharWidth = tt_get_signed_pair(handle);
        (*table).usWeightClass = u16::get(handle);
        (*table).usWidthClass = u16::get(handle);
        (*table).fsType = tt_get_signed_pair(handle);
        (*table).ySubscriptXSize = tt_get_signed_pair(handle);
        (*table).ySubscriptYSize = tt_get_signed_pair(handle);
        (*table).ySubscriptXOffset = tt_get_signed_pair(handle);
        (*table).ySubscriptYOffset = tt_get_signed_pair(handle);
        (*table).ySuperscriptXSize = tt_get_signed_pair(handle);
        (*table).ySuperscriptYSize = tt_get_signed_pair(handle);
        (*table).ySuperscriptXOffset = tt_get_signed_pair(handle);
        (*table).ySuperscriptYOffset = tt_get_signed_pair(handle);
        (*table).yStrikeoutSize = tt_get_signed_pair(handle);
        (*table).yStrikeoutPosition = tt_get_signed_pair(handle);
        (*table).sFamilyClass = tt_get_signed_pair(handle);
        for i in 0..10 {
            (*table).panose[i] = u8::get(handle);
        }
        (*table).ulUnicodeRange1 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange2 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange3 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange4 = tt_get_unsigned_quad(handle);
        for i in 0..4 {
            (*table).achVendID[i] = tt_get_signed_byte(handle);
        }
        (*table).fsSelection = u16::get(handle);
        (*table).usFirstCharIndex = u16::get(handle);
        (*table).usLastCharIndex = u16::get(handle);
        if sfnt_find_table_len(sfont, sfnt_table_info::OS_2) >= 78_u32 {
            /* these fields are not present in the original Apple spec (68-byte table),
            but Microsoft's version of "format 0" does include them... grr! */
            (*table).sTypoAscender = tt_get_signed_pair(handle);
            (*table).sTypoDescender = tt_get_signed_pair(handle);
            (*table).sTypoLineGap = tt_get_signed_pair(handle);
            (*table).usWinAscent = u16::get(handle);
            (*table).usWinDescent = u16::get(handle);
            if (*table).version as i32 > 0i32 {
                /* format 1 adds the following 2 fields */
                (*table).ulCodePageRange1 = tt_get_unsigned_quad(handle);
                (*table).ulCodePageRange2 = tt_get_unsigned_quad(handle);
                if (*table).version as i32 > 1i32 {
                    /* and formats 2 and 3 (current) include 5 more.... these share the
                    same fields, only the precise definition of some was changed */
                    (*table).sxHeight = tt_get_signed_pair(handle);
                    (*table).sCapHeight = tt_get_signed_pair(handle);
                    (*table).usDefaultChar = u16::get(handle);
                    (*table).usBreakChar = u16::get(handle);
                    (*table).usMaxContext = u16::get(handle)
                }
            }
        }
    } else {
        /* used in add_CIDVMetrics() of cidtype0.c */
        (*table).sTypoAscender = 880_i16;
        (*table).sTypoDescender = -120_i16;
        /* used in tt_get_fontdesc() of tt_aux.c */
        (*table).usWeightClass = 400_u16; /* Normal(Regular) */
        (*table).xAvgCharWidth = 0_i16; /* ignore */
        (*table).version = 0_u16; /* TrueType rev 1.5 */
        (*table).fsType = 0_i16; /* Installable Embedding */
        (*table).fsSelection = 0_u16; /* All undefined */
        (*table).sFamilyClass = 0_i16; /* No Classification */
        for i in 0..10 {
            (*table).panose[i] = 0_u8;
            /* All Any */
        }
    }
    table
}
unsafe fn tt_get_name(
    sfont: *mut sfnt,
    dest: *mut i8,
    destlen: u16,
    plat_id: u16,
    enco_id: u16,
    lang_id: u16,
    name_id: u16,
) -> u16 {
    let mut length: u16 = 0_u16;
    let name_offset = sfnt_locate_table(sfont, sfnt_table_info::NAME);
    let handle = &mut (*sfont).handle;
    if u16::get(handle) != 0 {
        panic!("Expecting zero");
    }
    let num_names = u16::get(handle);
    let string_offset = u16::get(handle);
    let mut i = 0;
    while i < num_names as i32 {
        let p_id = u16::get(handle);
        let e_id = u16::get(handle);
        let l_id = u16::get(handle);
        let n_id = u16::get(handle);
        length = u16::get(handle);
        let offset = u16::get(handle);
        /* language ID value 0xffffu for `accept any language ID' */
        if p_id as i32 == plat_id as i32
            && e_id as i32 == enco_id as i32
            && (lang_id as u32 == 0xffffu32 || l_id as i32 == lang_id as i32)
            && n_id as i32 == name_id as i32
        {
            if length as i32 > destlen as i32 - 1i32 {
                warn!(
                    "Name string too long ({}), truncating to {}",
                    length, destlen,
                );
                length = (destlen as i32 - 1i32) as u16
            }
            handle
                .seek(SeekFrom::Start(
                    name_offset as u64 + string_offset as u64 + offset as u64,
                ))
                .unwrap();
            ttstub_input_read(
                handle.as_ptr(),
                dest as *mut u8 as *mut i8,
                length as size_t,
            );
            *dest.offset(length as isize) = '\u{0}' as i32 as i8;
            break;
        } else {
            i += 1
        }
    }
    if i == num_names as i32 {
        length = 0_u16
    }
    length
}
/* set to 0 */
/* extra information */
/* v.1.1 name */
/* v.1.1 name */
/* v.1.1 name */
/* set to 0 */
/* extra information */
/* 0x0001 or 0x0002 */
/* if (faType & 0x08) editable_embedding */
/* TTF spec. from MS is wrong */
/* TTF spec. from MS is wrong */
/* TTF spec. from MS is wrong */
/* version 0x0002 */
/* hmtx and vmtx */
/* head, hhea, maxp */
/* vhea */
/* VORG */
/* hmtx and vmtx */
/* OS/2 table */
/* name table */

pub(crate) unsafe fn tt_get_ps_fontname(sfont: *mut sfnt) -> Option<String> {
    let mut dest = [0; 128];
    /* First try Mac-Roman PS name and then Win-Unicode PS name */
    let mut namelen = tt_get_name(sfont, dest.as_mut_ptr(), 127, 1_u16, 0_u16, 0_u16, 6_u16);
    if namelen as i32 != 0i32
        || {
            namelen = tt_get_name(
                sfont,
                dest.as_mut_ptr(),
                127,
                3_u16,
                1_u16,
                0x409_u16,
                6_u16,
            );
            namelen as i32 != 0i32
        }
        || {
            namelen = tt_get_name(
                sfont,
                dest.as_mut_ptr(),
                127,
                3_u16,
                5_u16,
                0x412_u16,
                6_u16,
            );
            namelen as i32 != 0i32
        }
    {
        return Some(CStr::from_ptr(dest.as_ptr()).to_str().unwrap().to_owned());
    }
    warn!("No valid PostScript name available");
    /*
      Workaround for some bad TTfonts:
      Language ID value 0xffffu for `accept any language ID'
    */
    namelen = tt_get_name(
        sfont,
        dest.as_mut_ptr(),
        127,
        1_u16,
        0_u16,
        0xffff_u16,
        6_u16,
    );
    if namelen as i32 == 0i32 {
        /*
          Finally falling back to Mac Roman name field.
          Warning: Some bad Japanese TTfonts using SJIS encoded string in the
          Mac Roman name field.
        */
        namelen = tt_get_name(sfont, dest.as_mut_ptr(), 127, 1_u16, 0_u16, 0_u16, 1_u16)
    }
    if namelen != 0 {
        Some(CStr::from_ptr(dest.as_ptr()).to_str().unwrap().to_owned())
    } else {
        None
    }
}
