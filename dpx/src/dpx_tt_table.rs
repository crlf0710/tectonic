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
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_mut
)]

use super::dpx_numbers::{
    tt_get_signed_byte, tt_get_signed_pair, tt_get_unsigned_byte, tt_get_unsigned_pair,
    tt_get_unsigned_quad,
};
use crate::warn;

use super::dpx_mem::new;
use super::dpx_sfnt::{sfnt_find_table_len, sfnt_find_table_pos, sfnt_locate_table};
use crate::dpx_truetype::sfnt_table_info;
use crate::ttstub_input_read;

use std::io::{Seek, SeekFrom};
use std::ptr;

pub type __ssize_t = i64;
pub type size_t = u64;
pub type Fixed = u32;
pub type FWord = i16;
pub type uFWord = u16;

use super::dpx_sfnt::{put_big_endian, sfnt};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_head_table {
    pub version: Fixed,
    pub fontRevision: Fixed,
    pub checkSumAdjustment: u32,
    pub magicNumber: u32,
    pub flags: u16,
    pub unitsPerEm: u16,
    pub created: [u8; 8],
    pub modified: [u8; 8],
    pub xMin: FWord,
    pub yMin: FWord,
    pub xMax: FWord,
    pub yMax: FWord,
    pub macStyle: u16,
    pub lowestRecPPEM: u16,
    pub fontDirectionHint: i16,
    pub indexToLocFormat: i16,
    pub glyphDataFormat: i16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_hhea_table {
    pub version: Fixed,
    pub ascent: FWord,
    pub descent: FWord,
    pub lineGap: FWord,
    pub advanceWidthMax: uFWord,
    pub minLeftSideBearing: FWord,
    pub minRightSideBearing: FWord,
    pub xMaxExtent: FWord,
    pub caretSlopeRise: i16,
    pub caretSlopeRun: i16,
    pub caretOffset: FWord,
    pub reserved: [i16; 4],
    pub metricDataFormat: i16,
    pub numOfLongHorMetrics: u16,
    pub numOfExSideBearings: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_vhea_table {
    pub version: Fixed,
    pub vertTypoAscender: i16,
    pub vertTypoDescender: i16,
    pub vertTypoLineGap: i16,
    pub advanceHeightMax: i16,
    pub minTopSideBearing: i16,
    pub minBottomSideBearing: i16,
    pub yMaxExtent: i16,
    pub caretSlopeRise: i16,
    pub caretSlopeRun: i16,
    pub caretOffset: i16,
    pub reserved: [i16; 4],
    pub metricDataFormat: i16,
    pub numOfLongVerMetrics: u16,
    pub numOfExSideBearings: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_maxp_table {
    pub version: Fixed,
    pub numGlyphs: u16,
    pub maxPoints: u16,
    pub maxContours: u16,
    pub maxComponentPoints: u16,
    pub maxComponentContours: u16,
    pub maxZones: u16,
    pub maxTwilightPoints: u16,
    pub maxStorage: u16,
    pub maxFunctionDefs: u16,
    pub maxInstructionDefs: u16,
    pub maxStackElements: u16,
    pub maxSizeOfInstructions: u16,
    pub maxComponentElements: u16,
    pub maxComponentDepth: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_os2__table {
    pub version: u16,
    pub xAvgCharWidth: i16,
    pub usWeightClass: u16,
    pub usWidthClass: u16,
    pub fsType: i16,
    pub ySubscriptXSize: i16,
    pub ySubscriptYSize: i16,
    pub ySubscriptXOffset: i16,
    pub ySubscriptYOffset: i16,
    pub ySuperscriptXSize: i16,
    pub ySuperscriptYSize: i16,
    pub ySuperscriptXOffset: i16,
    pub ySuperscriptYOffset: i16,
    pub yStrikeoutSize: i16,
    pub yStrikeoutPosition: i16,
    pub sFamilyClass: i16,
    pub panose: [u8; 10],
    pub ulUnicodeRange1: u32,
    pub ulUnicodeRange2: u32,
    pub ulUnicodeRange3: u32,
    pub ulUnicodeRange4: u32,
    pub achVendID: [i8; 4],
    pub fsSelection: u16,
    pub usFirstCharIndex: u16,
    pub usLastCharIndex: u16,
    pub sTypoAscender: i16,
    pub sTypoDescender: i16,
    pub sTypoLineGap: i16,
    pub usWinAscent: u16,
    pub usWinDescent: u16,
    pub ulCodePageRange1: u32,
    pub ulCodePageRange2: u32,
    pub sxHeight: i16,
    pub sCapHeight: i16,
    pub usDefaultChar: u16,
    pub usBreakChar: u16,
    pub usMaxContext: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_vertOriginYMetrics {
    pub glyphIndex: u16,
    pub vertOriginY: i16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_VORG_table {
    pub defaultVertOriginY: i16,
    pub numVertOriginYMetrics: u16,
    pub vertOriginYMetrics: *mut tt_vertOriginYMetrics,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_longMetrics {
    pub advance: u16,
    pub sideBearing: i16,
}
/*
  tables contains information refered by other tables
  maxp->numGlyphs, etc --> loca, etc
  hhea->numOfLongHorMetrics --> hmtx
  head->indexToLocFormat --> loca
  head->glyphDataFormat --> glyf
*/

pub unsafe fn tt_pack_head_table(mut table: *mut tt_head_table) -> *mut i8 {
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

pub unsafe fn tt_read_head_table(mut sfont: *mut sfnt) -> *mut tt_head_table {
    let mut table: *mut tt_head_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_head_table>() as u64)
        as u32) as *mut tt_head_table;
    sfnt_locate_table(sfont, sfnt_table_info::HEAD);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).fontRevision = tt_get_unsigned_quad(handle);
    (*table).checkSumAdjustment = tt_get_unsigned_quad(handle);
    (*table).magicNumber = tt_get_unsigned_quad(handle);
    (*table).flags = tt_get_unsigned_pair(handle);
    (*table).unitsPerEm = tt_get_unsigned_pair(handle);
    for i in 0..8 {
        (*table).created[i] = tt_get_unsigned_byte(handle);
    }
    for i in 0..8 {
        (*table).modified[i] = tt_get_unsigned_byte(handle);
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

pub unsafe fn tt_pack_maxp_table(mut table: *mut tt_maxp_table) -> *mut i8 {
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

pub unsafe fn tt_read_maxp_table(mut sfont: *mut sfnt) -> *mut tt_maxp_table {
    let mut table: *mut tt_maxp_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_maxp_table>() as u64)
        as u32) as *mut tt_maxp_table;
    sfnt_locate_table(sfont, sfnt_table_info::MAXP);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).numGlyphs = tt_get_unsigned_pair(handle);
    (*table).maxPoints = tt_get_unsigned_pair(handle);
    (*table).maxContours = tt_get_unsigned_pair(handle);
    (*table).maxComponentPoints = tt_get_unsigned_pair(handle);
    (*table).maxComponentContours = tt_get_unsigned_pair(handle);
    (*table).maxZones = tt_get_unsigned_pair(handle);
    (*table).maxTwilightPoints = tt_get_unsigned_pair(handle);
    (*table).maxStorage = tt_get_unsigned_pair(handle);
    (*table).maxFunctionDefs = tt_get_unsigned_pair(handle);
    (*table).maxInstructionDefs = tt_get_unsigned_pair(handle);
    (*table).maxStackElements = tt_get_unsigned_pair(handle);
    (*table).maxSizeOfInstructions = tt_get_unsigned_pair(handle);
    (*table).maxComponentElements = tt_get_unsigned_pair(handle);
    (*table).maxComponentDepth = tt_get_unsigned_pair(handle);
    table
}

pub unsafe fn tt_pack_hhea_table(mut table: *mut tt_hhea_table) -> *mut i8 {
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

pub unsafe fn tt_read_hhea_table(mut sfont: *mut sfnt) -> *mut tt_hhea_table {
    let mut table: *mut tt_hhea_table = new((1_u64)
        .wrapping_mul(::std::mem::size_of::<tt_hhea_table>() as u64)
        as u32) as *mut tt_hhea_table;
    sfnt_locate_table(sfont, sfnt_table_info::HHEA);
    let handle = &mut (*sfont).handle;
    (*table).version = tt_get_unsigned_quad(handle);
    (*table).ascent = tt_get_signed_pair(handle);
    (*table).descent = tt_get_signed_pair(handle);
    (*table).lineGap = tt_get_signed_pair(handle);
    (*table).advanceWidthMax = tt_get_unsigned_pair(handle);
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
    (*table).numOfLongHorMetrics = tt_get_unsigned_pair(handle);
    let len = sfnt_find_table_len(sfont, sfnt_table_info::HMTX);
    (*table).numOfExSideBearings = len
        .wrapping_sub(((*table).numOfLongHorMetrics as i32 * 4i32) as u32)
        .wrapping_div(2_u32) as u16;
    table
}
/* vhea */

pub unsafe fn tt_read_vhea_table(mut sfont: *mut sfnt) -> *mut tt_vhea_table {
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
    (*table).numOfLongVerMetrics = tt_get_unsigned_pair(handle);
    let len = sfnt_find_table_len(sfont, b"vmtx");
    (*table).numOfExSideBearings = len
        .wrapping_sub(((*table).numOfLongVerMetrics as i32 * 4i32) as u32)
        .wrapping_div(2_u32) as u16;
    table
}

pub unsafe fn tt_read_VORG_table(mut sfont: *mut sfnt) -> *mut tt_VORG_table {
    let offset = sfnt_find_table_pos(sfont, b"VORG");
    let handle = &mut (*sfont).handle;
    if offset > 0_u32 {
        let vorg = new((1_u64).wrapping_mul(::std::mem::size_of::<tt_VORG_table>() as u64) as u32)
            as *mut tt_VORG_table;
        sfnt_locate_table(sfont, b"VORG");
        if tt_get_unsigned_pair(handle) as i32 != 1i32
            || tt_get_unsigned_pair(handle) as i32 != 0i32
        {
            panic!("Unsupported VORG version.");
        }
        (*vorg).defaultVertOriginY = tt_get_signed_pair(handle);
        (*vorg).numVertOriginYMetrics = tt_get_unsigned_pair(handle);
        (*vorg).vertOriginYMetrics = new(((*vorg).numVertOriginYMetrics as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<tt_vertOriginYMetrics>() as u64)
            as u32) as *mut tt_vertOriginYMetrics;
        /*
         * The vertOriginYMetrics array must be sorted in increasing
         * glyphIndex order.
         */
        for i in 0..(*vorg).numVertOriginYMetrics {
            (*(*vorg).vertOriginYMetrics.offset(i as isize)).glyphIndex =
                tt_get_unsigned_pair(handle);
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

pub unsafe fn tt_read_longMetrics(
    mut sfont: *mut sfnt,
    mut numGlyphs: u16,
    mut numLongMetrics: u16,
    mut numExSideBearings: u16,
) -> *mut tt_longMetrics {
    let mut last_adv: u16 = 0_u16;
    let mut last_esb: i16 = 0_i16;
    let m = new((numGlyphs as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<tt_longMetrics>() as u64) as u32)
        as *mut tt_longMetrics;
    let handle = &mut (*sfont).handle;
    for gid in 0..numGlyphs {
        if (gid as i32) < numLongMetrics as i32 {
            last_adv = tt_get_unsigned_pair(handle)
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

pub unsafe fn tt_read_os2__table(mut sfont: *mut sfnt) -> *mut tt_os2__table {
    let table = new((1_u64).wrapping_mul(::std::mem::size_of::<tt_os2__table>() as u64) as u32)
        as *mut tt_os2__table;
    let handle = &mut (*sfont).handle;
    if sfnt_find_table_pos(sfont, sfnt_table_info::OS_2) > 0_u32 {
        sfnt_locate_table(sfont, sfnt_table_info::OS_2);
        (*table).version = tt_get_unsigned_pair(handle);
        (*table).xAvgCharWidth = tt_get_signed_pair(handle);
        (*table).usWeightClass = tt_get_unsigned_pair(handle);
        (*table).usWidthClass = tt_get_unsigned_pair(handle);
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
            (*table).panose[i] = tt_get_unsigned_byte(handle);
        }
        (*table).ulUnicodeRange1 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange2 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange3 = tt_get_unsigned_quad(handle);
        (*table).ulUnicodeRange4 = tt_get_unsigned_quad(handle);
        for i in 0..4 {
            (*table).achVendID[i] = tt_get_signed_byte(handle);
        }
        (*table).fsSelection = tt_get_unsigned_pair(handle);
        (*table).usFirstCharIndex = tt_get_unsigned_pair(handle);
        (*table).usLastCharIndex = tt_get_unsigned_pair(handle);
        if sfnt_find_table_len(sfont, sfnt_table_info::OS_2) >= 78_u32 {
            /* these fields are not present in the original Apple spec (68-byte table),
            but Microsoft's version of "format 0" does include them... grr! */
            (*table).sTypoAscender = tt_get_signed_pair(handle);
            (*table).sTypoDescender = tt_get_signed_pair(handle);
            (*table).sTypoLineGap = tt_get_signed_pair(handle);
            (*table).usWinAscent = tt_get_unsigned_pair(handle);
            (*table).usWinDescent = tt_get_unsigned_pair(handle);
            if (*table).version as i32 > 0i32 {
                /* format 1 adds the following 2 fields */
                (*table).ulCodePageRange1 = tt_get_unsigned_quad(handle);
                (*table).ulCodePageRange2 = tt_get_unsigned_quad(handle);
                if (*table).version as i32 > 1i32 {
                    /* and formats 2 and 3 (current) include 5 more.... these share the
                    same fields, only the precise definition of some was changed */
                    (*table).sxHeight = tt_get_signed_pair(handle);
                    (*table).sCapHeight = tt_get_signed_pair(handle);
                    (*table).usDefaultChar = tt_get_unsigned_pair(handle);
                    (*table).usBreakChar = tt_get_unsigned_pair(handle);
                    (*table).usMaxContext = tt_get_unsigned_pair(handle)
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
    mut sfont: *mut sfnt,
    mut dest: *mut i8,
    mut destlen: u16,
    mut plat_id: u16,
    mut enco_id: u16,
    mut lang_id: u16,
    mut name_id: u16,
) -> u16 {
    let mut length: u16 = 0_u16;
    let name_offset = sfnt_locate_table(sfont, sfnt_table_info::NAME);
    let handle = &mut (*sfont).handle;
    if tt_get_unsigned_pair(handle) != 0 {
        panic!("Expecting zero");
    }
    let num_names = tt_get_unsigned_pair(handle);
    let string_offset = tt_get_unsigned_pair(handle);
    let mut i = 0;
    while i < num_names as i32 {
        let mut p_id = tt_get_unsigned_pair(handle);
        let mut e_id = tt_get_unsigned_pair(handle);
        let mut l_id = tt_get_unsigned_pair(handle);
        let mut n_id = tt_get_unsigned_pair(handle);
        length = tt_get_unsigned_pair(handle);
        let mut offset = tt_get_unsigned_pair(handle);
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
                handle.0.as_ptr(),
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

pub unsafe fn tt_get_ps_fontname(mut sfont: *mut sfnt, mut dest: *mut i8, mut destlen: u16) -> u16 {
    /* First try Mac-Roman PS name and then Win-Unicode PS name */
    let mut namelen = tt_get_name(sfont, dest, destlen, 1_u16, 0_u16, 0_u16, 6_u16);
    if namelen as i32 != 0i32
        || {
            namelen = tt_get_name(sfont, dest, destlen, 3_u16, 1_u16, 0x409_u16, 6_u16);
            namelen as i32 != 0i32
        }
        || {
            namelen = tt_get_name(sfont, dest, destlen, 3_u16, 5_u16, 0x412_u16, 6_u16);
            namelen as i32 != 0i32
        }
    {
        return namelen;
    }
    warn!("No valid PostScript name available");
    /*
      Workaround for some bad TTfonts:
      Language ID value 0xffffu for `accept any language ID'
    */
    namelen = tt_get_name(sfont, dest, destlen, 1_u16, 0_u16, 0xffff_u16, 6_u16);
    if namelen as i32 == 0i32 {
        /*
          Finally falling back to Mac Roman name field.
          Warning: Some bad Japanese TTfonts using SJIS encoded string in the
          Mac Roman name field.
        */
        namelen = tt_get_name(sfont, dest, destlen, 1_u16, 0_u16, 0_u16, 1_u16)
    }
    namelen
}
