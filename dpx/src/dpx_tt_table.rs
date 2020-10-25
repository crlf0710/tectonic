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

use super::dpx_numbers::GetFromFile;
use crate::warn;

use super::dpx_sfnt::{sfnt_find_table_len, sfnt_find_table_pos, sfnt_locate_table};
use crate::dpx_truetype::sfnt_table_info;

use std::io::{Read, Seek, SeekFrom};

pub(crate) type Fixed = u32;

use super::dpx_sfnt::{sfnt, PutBE};

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
    pub(crate) xMin: i16,
    pub(crate) yMin: i16,
    pub(crate) xMax: i16,
    pub(crate) yMax: i16,
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
    pub(crate) ascent: i16,
    pub(crate) descent: i16,
    pub(crate) lineGap: i16,
    pub(crate) advanceWidthMax: u16,
    pub(crate) minLeftSideBearing: i16,
    pub(crate) minRightSideBearing: i16,
    pub(crate) xMaxExtent: i16,
    pub(crate) caretSlopeRise: i16,
    pub(crate) caretSlopeRun: i16,
    pub(crate) caretOffset: i16,
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
#[derive(Clone)]
pub(crate) struct tt_VORG_table {
    pub(crate) defaultVertOriginY: i16,
    pub(crate) vertOriginYMetrics: Vec<tt_vertOriginYMetrics>,
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

pub(crate) fn tt_pack_head_table(table: &tt_head_table) -> Vec<u8> {
    let mut data = Vec::with_capacity(54);
    data.put_be(table.version);
    data.put_be(table.fontRevision);
    data.put_be(table.checkSumAdjustment);
    data.put_be(table.magicNumber);
    data.put_be(table.flags);
    data.put_be(table.unitsPerEm);
    for b in &table.created {
        data.push(*b);
    }
    for b in &table.modified {
        data.push(*b);
    }
    data.put_be(table.xMin);
    data.put_be(table.yMin);
    data.put_be(table.xMax);
    data.put_be(table.yMax);
    data.put_be(table.macStyle);
    data.put_be(table.lowestRecPPEM);
    data.put_be(table.fontDirectionHint);
    data.put_be(table.indexToLocFormat);
    data.put_be(table.glyphDataFormat);

    data
}

pub(crate) fn tt_read_head_table(sfont: &sfnt) -> Box<tt_head_table> {
    sfnt_locate_table(sfont, sfnt_table_info::HEAD);
    let handle = &mut &*sfont.handle;
    let version = u32::get(handle);
    let fontRevision = u32::get(handle);
    let checkSumAdjustment = u32::get(handle);
    let magicNumber = u32::get(handle);
    let flags = u16::get(handle);
    let unitsPerEm = u16::get(handle);
    let mut created = [0u8; 8];
    for b in &mut created {
        *b = u8::get(handle);
    }
    let mut modified = [0u8; 8];
    for b in &mut modified {
        *b = u8::get(handle);
    }
    let xMin = i16::get(handle);
    let yMin = i16::get(handle);
    let xMax = i16::get(handle);
    let yMax = i16::get(handle);
    let macStyle = i16::get(handle) as u16;
    let lowestRecPPEM = i16::get(handle) as u16;
    let fontDirectionHint = i16::get(handle);
    let indexToLocFormat = i16::get(handle);
    let glyphDataFormat = i16::get(handle);

    Box::new(tt_head_table {
        version,
        fontRevision,
        checkSumAdjustment,
        magicNumber,
        flags,
        unitsPerEm,
        created,
        modified,
        xMin,
        yMin,
        xMax,
        yMax,
        macStyle,
        lowestRecPPEM,
        fontDirectionHint,
        indexToLocFormat,
        glyphDataFormat,
    })
}

pub(crate) fn tt_pack_maxp_table(table: &tt_maxp_table) -> Vec<u8> {
    let mut data = Vec::with_capacity(32);
    data.put_be(table.version);
    data.put_be(table.numGlyphs);
    data.put_be(table.maxPoints);
    data.put_be(table.maxContours);
    data.put_be(table.maxComponentPoints);
    data.put_be(table.maxComponentContours);
    data.put_be(table.maxZones);
    data.put_be(table.maxTwilightPoints);
    data.put_be(table.maxStorage);
    data.put_be(table.maxFunctionDefs);
    data.put_be(table.maxInstructionDefs);
    data.put_be(table.maxStackElements);
    data.put_be(table.maxSizeOfInstructions);
    data.put_be(table.maxComponentElements);
    data.put_be(table.maxComponentDepth);
    data
}

pub(crate) fn tt_read_maxp_table(sfont: &sfnt) -> Box<tt_maxp_table> {
    sfnt_locate_table(sfont, sfnt_table_info::MAXP);
    let handle = &mut &*sfont.handle;
    let version = u32::get(handle);
    let numGlyphs = u16::get(handle);
    let maxPoints = u16::get(handle);
    let maxContours = u16::get(handle);
    let maxComponentPoints = u16::get(handle);
    let maxComponentContours = u16::get(handle);
    let maxZones = u16::get(handle);
    let maxTwilightPoints = u16::get(handle);
    let maxStorage = u16::get(handle);
    let maxFunctionDefs = u16::get(handle);
    let maxInstructionDefs = u16::get(handle);
    let maxStackElements = u16::get(handle);
    let maxSizeOfInstructions = u16::get(handle);
    let maxComponentElements = u16::get(handle);
    let maxComponentDepth = u16::get(handle);

    Box::new(tt_maxp_table {
        version,
        numGlyphs,
        maxPoints,
        maxContours,
        maxComponentPoints,
        maxComponentContours,
        maxZones,
        maxTwilightPoints,
        maxStorage,
        maxFunctionDefs,
        maxInstructionDefs,
        maxStackElements,
        maxSizeOfInstructions,
        maxComponentElements,
        maxComponentDepth,
    })
}

pub(crate) fn tt_pack_hhea_table(table: &tt_hhea_table) -> Vec<u8> {
    let mut data = Vec::with_capacity(36);
    data.put_be(table.version);
    data.put_be(table.ascent);
    data.put_be(table.descent);
    data.put_be(table.lineGap);
    data.put_be(table.advanceWidthMax);
    data.put_be(table.minLeftSideBearing);
    data.put_be(table.minRightSideBearing);
    data.put_be(table.xMaxExtent);
    data.put_be(table.caretSlopeRise);
    data.put_be(table.caretSlopeRun);
    data.put_be(table.caretOffset);
    for i in &table.reserved {
        data.put_be(*i);
    }
    data.put_be(table.metricDataFormat);
    data.put_be(table.numOfLongHorMetrics);

    data
}

pub(crate) fn tt_read_hhea_table(sfont: &sfnt) -> Box<tt_hhea_table> {
    sfnt_locate_table(sfont, sfnt_table_info::HHEA);
    let handle = &mut &*sfont.handle;
    let version = u32::get(handle);
    let ascent = i16::get(handle);
    let descent = i16::get(handle);
    let lineGap = i16::get(handle);
    let advanceWidthMax = u16::get(handle);
    let minLeftSideBearing = i16::get(handle);
    let minRightSideBearing = i16::get(handle);
    let xMaxExtent = i16::get(handle);
    let caretSlopeRise = i16::get(handle);
    let caretSlopeRun = i16::get(handle);
    let caretOffset = i16::get(handle);
    let mut reserved = [0_i16; 4];
    for i in &mut reserved {
        *i = i16::get(handle);
    }
    let metricDataFormat = i16::get(handle);
    if metricDataFormat != 0 {
        panic!("unknown metricDataFormat");
    }
    let numOfLongHorMetrics = u16::get(handle);
    let len = sfnt_find_table_len(sfont, sfnt_table_info::HMTX);
    let numOfExSideBearings = len
        .wrapping_sub((numOfLongHorMetrics as i32 * 4) as u32)
        .wrapping_div(2_u32) as u16;

    Box::new(tt_hhea_table {
        version,
        ascent,
        descent,
        lineGap,
        advanceWidthMax,
        minLeftSideBearing,
        minRightSideBearing,
        xMaxExtent,
        caretSlopeRise,
        caretSlopeRun,
        caretOffset,
        reserved,
        metricDataFormat,
        numOfLongHorMetrics,
        numOfExSideBearings,
    })
}
/* vhea */

pub(crate) fn tt_read_vhea_table(sfont: &sfnt) -> Box<tt_vhea_table> {
    sfnt_locate_table(sfont, b"vhea");
    let handle = &mut &*sfont.handle;
    let version = u32::get(handle);
    let vertTypoAscender = i16::get(handle);
    let vertTypoDescender = i16::get(handle);
    let vertTypoLineGap = i16::get(handle);
    let advanceHeightMax = i16::get(handle);
    let minTopSideBearing = i16::get(handle);
    let minBottomSideBearing = i16::get(handle);
    let yMaxExtent = i16::get(handle);
    let caretSlopeRise = i16::get(handle);
    let caretSlopeRun = i16::get(handle);
    let caretOffset = i16::get(handle);
    let mut reserved = [0_i16; 4];
    for i in &mut reserved {
        *i = i16::get(handle);
    }
    let metricDataFormat = i16::get(handle);
    let numOfLongVerMetrics = u16::get(handle);
    let len = sfnt_find_table_len(sfont, b"vmtx");
    let numOfExSideBearings = len
        .wrapping_sub((numOfLongVerMetrics as i32 * 4i32) as u32)
        .wrapping_div(2_u32) as u16;

    Box::new(tt_vhea_table {
        version,
        vertTypoAscender,
        vertTypoDescender,
        vertTypoLineGap,
        advanceHeightMax,
        minTopSideBearing,
        minBottomSideBearing,
        yMaxExtent,
        caretSlopeRise,
        caretSlopeRun,
        caretOffset,
        reserved,
        metricDataFormat,
        numOfLongVerMetrics,
        numOfExSideBearings,
    })
}

pub(crate) fn tt_read_VORG_table(sfont: &sfnt) -> Option<Box<tt_VORG_table>> {
    let offset = sfnt_find_table_pos(sfont, b"VORG");
    let handle = &mut &*sfont.handle;
    if offset > 0 {
        sfnt_locate_table(sfont, b"VORG");
        if u16::get(handle) as i32 != 1i32 || u16::get(handle) as i32 != 0i32 {
            panic!("Unsupported VORG version.");
        }
        let defaultVertOriginY = i16::get(handle);
        let numVertOriginYMetrics = u16::get(handle) as usize;
        let mut vertOriginYMetrics = Vec::with_capacity(numVertOriginYMetrics);
        /*
         * The vertOriginYMetrics array must be sorted in increasing
         * glyphIndex order.
         */
        for _ in 0..numVertOriginYMetrics {
            let glyphIndex = u16::get(handle);
            let vertOriginY = i16::get(handle);
            vertOriginYMetrics.push(tt_vertOriginYMetrics {
                glyphIndex,
                vertOriginY,
            });
        }
        Some(Box::new(tt_VORG_table {
            defaultVertOriginY,
            vertOriginYMetrics,
        }))
    } else {
        None
    }
}
/*
 * hmtx and vmtx
 *
 *  Reading/writing hmtx and vmtx depend on other tables, maxp and hhea/vhea.
 */

pub(crate) fn tt_read_longMetrics<R: Read>(
    handle: &mut R,
    numGlyphs: u16,
    numLongMetrics: u16,
    numExSideBearings: u16,
) -> Vec<tt_longMetrics> {
    let mut last_adv: u16 = 0_u16;
    let mut last_esb: i16 = 0_i16;
    let mut m = Vec::with_capacity(numGlyphs as usize);
    for gid in 0..numGlyphs {
        if (gid as i32) < numLongMetrics as i32 {
            last_adv = u16::get(handle)
        }
        if (gid as i32) < numLongMetrics as i32 + numExSideBearings as i32 {
            last_esb = i16::get(handle)
        }
        m.push(tt_longMetrics {
            advance: last_adv,
            sideBearing: last_esb,
        });
    }
    m
}
/* OS/2 table */
/* this table may not exist */

pub(crate) fn tt_read_os2__table(sfont: &sfnt) -> Box<tt_os2__table> {
    let handle = &mut &*sfont.handle;
    let version;
    let xAvgCharWidth;
    let usWeightClass;
    let fsType;
    let fsSelection;
    let sFamilyClass;
    let mut panose = [0_u8; 10];

    let mut achVendID = [0_i8; 4];
    let mut usWidthClass = 0;
    let mut ySubscriptXSize = 0;
    let mut ySubscriptYSize = 0;
    let mut ySubscriptXOffset = 0;
    let mut ySubscriptYOffset = 0;
    let mut ySuperscriptXSize = 0;
    let mut ySuperscriptYSize = 0;
    let mut ySuperscriptXOffset = 0;
    let mut ySuperscriptYOffset = 0;
    let mut yStrikeoutSize = 0;
    let mut yStrikeoutPosition = 0;
    let mut ulUnicodeRange1 = 0;
    let mut ulUnicodeRange2 = 0;
    let mut ulUnicodeRange3 = 0;
    let mut ulUnicodeRange4 = 0;
    let mut usFirstCharIndex = 0;
    let mut usLastCharIndex = 0;
    let mut sTypoAscender = 0;
    let mut sTypoDescender = 0;
    let mut sTypoLineGap = 0;
    let mut usWinAscent = 0;
    let mut usWinDescent = 0;
    let mut ulCodePageRange1 = 0;
    let mut ulCodePageRange2 = 0;
    let mut sxHeight = 0;
    let mut sCapHeight = 0;
    let mut usDefaultChar = 0;
    let mut usBreakChar = 0;
    let mut usMaxContext = 0;

    if sfnt_find_table_pos(sfont, sfnt_table_info::OS_2) > 0 {
        sfnt_locate_table(sfont, sfnt_table_info::OS_2);
        version = u16::get(handle);
        xAvgCharWidth = i16::get(handle);
        usWeightClass = u16::get(handle);
        usWidthClass = u16::get(handle);
        fsType = i16::get(handle);
        ySubscriptXSize = i16::get(handle);
        ySubscriptYSize = i16::get(handle);
        ySubscriptXOffset = i16::get(handle);
        ySubscriptYOffset = i16::get(handle);
        ySuperscriptXSize = i16::get(handle);
        ySuperscriptYSize = i16::get(handle);
        ySuperscriptXOffset = i16::get(handle);
        ySuperscriptYOffset = i16::get(handle);
        yStrikeoutSize = i16::get(handle);
        yStrikeoutPosition = i16::get(handle);
        sFamilyClass = i16::get(handle);
        for i in &mut panose {
            *i = u8::get(handle);
        }
        ulUnicodeRange1 = u32::get(handle);
        ulUnicodeRange2 = u32::get(handle);
        ulUnicodeRange3 = u32::get(handle);
        ulUnicodeRange4 = u32::get(handle);
        for i in &mut achVendID {
            *i = i8::get(handle);
        }
        fsSelection = u16::get(handle);
        usFirstCharIndex = u16::get(handle);
        usLastCharIndex = u16::get(handle);
        if sfnt_find_table_len(sfont, sfnt_table_info::OS_2) >= 78 {
            /* these fields are not present in the original Apple spec (68-byte table),
            but Microsoft's version of "format 0" does include them... grr! */
            sTypoAscender = i16::get(handle);
            sTypoDescender = i16::get(handle);
            sTypoLineGap = i16::get(handle);
            usWinAscent = u16::get(handle);
            usWinDescent = u16::get(handle);
            if version > 0 {
                /* format 1 adds the following 2 fields */
                ulCodePageRange1 = u32::get(handle);
                ulCodePageRange2 = u32::get(handle);
                if version > 1 {
                    /* and formats 2 and 3 (current) include 5 more.... these share the
                    same fields, only the precise definition of some was changed */
                    sxHeight = i16::get(handle);
                    sCapHeight = i16::get(handle);
                    usDefaultChar = u16::get(handle);
                    usBreakChar = u16::get(handle);
                    usMaxContext = u16::get(handle);
                }
            }
        }
    } else {
        /* used in add_CIDVMetrics() of cidtype0.c */
        sTypoAscender = 880;
        sTypoDescender = -120;
        /* used in tt_get_fontdesc() of tt_aux.c */
        usWeightClass = 400; /* Normal(Regular) */
        xAvgCharWidth = 0; /* ignore */
        version = 0; /* TrueType rev 1.5 */
        fsType = 0; /* Installable Embedding */
        fsSelection = 0; /* All undefined */
        sFamilyClass = 0; /* No Classification */
        panose = [0; 10]; /* All Any */
    }

    Box::new(tt_os2__table {
        version,
        xAvgCharWidth,
        usWeightClass,
        usWidthClass,
        fsType,
        ySubscriptXSize,
        ySubscriptYSize,
        ySubscriptXOffset,
        ySubscriptYOffset,
        ySuperscriptXSize,
        ySuperscriptYSize,
        ySuperscriptXOffset,
        ySuperscriptYOffset,
        yStrikeoutSize,
        yStrikeoutPosition,
        sFamilyClass,
        panose,
        ulUnicodeRange1,
        ulUnicodeRange2,
        ulUnicodeRange3,
        ulUnicodeRange4,
        achVendID,
        fsSelection,
        usFirstCharIndex,
        usLastCharIndex,
        sTypoAscender,
        sTypoDescender,
        sTypoLineGap,
        usWinAscent,
        usWinDescent,
        ulCodePageRange1,
        ulCodePageRange2,
        sxHeight,
        sCapHeight,
        usDefaultChar,
        usBreakChar,
        usMaxContext,
    })
}
fn tt_get_name(
    sfont: &sfnt,
    dest: &mut [u8],
    plat_id: u16,
    enco_id: u16,
    lang_id: u16,
    name_id: u16,
) -> usize {
    let mut length = 0;
    let name_offset = sfnt_locate_table(sfont, sfnt_table_info::NAME);
    let handle = &mut &*sfont.handle;
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
        length = u16::get(handle) as usize;
        let offset = u16::get(handle);
        /* language ID value 0xffffu for `accept any language ID' */
        if p_id as i32 == plat_id as i32
            && e_id as i32 == enco_id as i32
            && (lang_id as u32 == 0xffffu32 || l_id as i32 == lang_id as i32)
            && n_id as i32 == name_id as i32
        {
            if length > dest.len() - 1 {
                warn!(
                    "Name string too long ({}), truncating to {}",
                    length,
                    dest.len(),
                );
                length = dest.len() - 1;
            }
            handle
                .seek(SeekFrom::Start(
                    name_offset as u64 + string_offset as u64 + offset as u64,
                ))
                .unwrap();

            handle.read(&mut dest[..length]).unwrap();
            break;
        } else {
            i += 1
        }
    }
    if i == num_names as i32 {
        length = 0;
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

pub(crate) fn tt_get_ps_fontname(sfont: &sfnt) -> Option<String> {
    let mut dest = [0; 128];
    /* First try Mac-Roman PS name and then Win-Unicode PS name */
    let mut namelen = tt_get_name(sfont, &mut dest[..127], 1_u16, 0_u16, 0_u16, 6_u16);
    if namelen != 0
        || {
            namelen = tt_get_name(sfont, &mut dest[..127], 3_u16, 1_u16, 0x409_u16, 6_u16);
            namelen != 0
        }
        || {
            namelen = tt_get_name(sfont, &mut dest[..127], 3_u16, 5_u16, 0x412_u16, 6_u16);
            namelen != 0
        }
    {
        return Some(std::str::from_utf8(&dest[..namelen]).unwrap().to_string());
    }
    warn!("No valid PostScript name available");
    /*
      Workaround for some bad TTfonts:
      Language ID value 0xffffu for `accept any language ID'
    */
    namelen = tt_get_name(sfont, &mut dest[..127], 1_u16, 0_u16, 0xffff_u16, 6_u16);
    if namelen == 0 {
        /*
          Finally falling back to Mac Roman name field.
          Warning: Some bad Japanese TTfonts using SJIS encoded string in the
          Mac Roman name field.
        */
        namelen = tt_get_name(sfont, &mut dest[..127], 1_u16, 0_u16, 0_u16, 1_u16)
    }
    if namelen != 0 {
        Some(std::str::from_utf8(&dest[..namelen]).unwrap().to_string())
    } else {
        None
    }
}
