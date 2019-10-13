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

use crate::DisplayExt;
use std::ffi::{CStr, CString};

use super::dpx_numbers::{
    tt_get_signed_byte, tt_get_signed_pair, tt_get_unsigned_pair, tt_get_unsigned_quad,
};
use super::dpx_sfnt::sfnt_find_table_pos;
use crate::mfree;
use crate::{info, warn};

use super::dpx_mem::{new, renew};
use super::dpx_otl_opt::{otl_match_optrule, otl_new_opt, otl_parse_optstring, otl_release_opt};
use libc::{free, memset};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;
pub type ssize_t = __ssize_t;
pub type Fixed = u32;

use super::dpx_sfnt::sfnt;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub {
    pub num_gsubs: i32,
    pub select: i32,
    pub first: *mut gsub_entry,
    pub gsubs: [otl_gsub_tab; 32],
    /* _TT_GSUB_H_ */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_tab {
    pub script: *mut i8,
    pub language: *mut i8,
    pub feature: *mut i8,
    pub num_subtables: i32,
    pub subtables: *mut otl_gsub_subtab,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_subtab {
    pub LookupType: u16,
    pub SubstFormat: u16,
    pub table: C2RustUnnamed,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub single1: *mut otl_gsub_single1,
    pub single2: *mut otl_gsub_single2,
    pub alternate1: *mut otl_gsub_alternate1,
    pub ligature1: *mut otl_gsub_ligature1,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_ligature1 {
    pub LigSetCount: u16,
    pub LigatureSet: *mut otl_gsub_ligset,
    pub coverage: clt_coverage,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_coverage {
    pub format: u16,
    pub count: u16,
    pub list: *mut GlyphID,
    pub range: *mut clt_range,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_range {
    pub Start: GlyphID,
    pub End: GlyphID,
    pub StartCoverageIndex: u16,
}
pub type GlyphID = u16;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_ligset {
    pub LigatureCount: u16,
    pub Ligature: *mut otl_gsub_ligtab,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_ligtab {
    pub LigGlyph: GlyphID,
    pub CompCount: u16,
    pub Component: *mut GlyphID,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_alternate1 {
    pub AlternateSetCount: u16,
    pub AlternateSet: *mut otl_gsub_altset,
    pub coverage: clt_coverage,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_altset {
    pub GlyphCount: u16,
    pub Alternate: *mut GlyphID,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_single2 {
    pub GlyphCount: u16,
    pub Substitute: *mut GlyphID,
    pub coverage: clt_coverage,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_single1 {
    pub DeltaGlyphID: i16,
    pub coverage: clt_coverage,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct gsub_entry {
    pub index: i32,
    pub next: *mut gsub_entry,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_record {
    pub tag: [i8; 5],
    pub offset: Offset,
}
pub type Offset = u16;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_record_list {
    pub count: u16,
    pub record: *mut clt_record,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_number_list {
    pub count: u16,
    pub value: *mut u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_feature_table {
    pub FeatureParams: Offset,
    pub LookupListIndex: clt_number_list,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_lookup_table {
    pub LookupType: u16,
    pub LookupFlag: u16,
    pub SubTableList: clt_number_list,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct otl_gsub_header {
    pub version: Fixed,
    pub ScriptList: Offset,
    pub FeatureList: Offset,
    pub LookupList: Offset,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_script_table {
    pub DefaultLangSys: Offset,
    pub LangSysRecord: clt_record_list,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct clt_langsys_table {
    pub LookupOrder: Offset,
    pub ReqFeatureIndex: u16,
    pub FeatureIndex: clt_number_list,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_set_verbose(mut level: i32) {
    verbose = level;
}
unsafe fn clt_read_record(mut rec: *mut clt_record, mut sfont: *mut sfnt) -> i32 {
    assert!(!rec.is_null() && !sfont.is_null());
    let handle = &mut (*sfont).handle;
    for i in 0..4 {
        (*rec).tag[i] = tt_get_signed_byte(handle) as i8;
    }
    (*rec).tag[4] = '\u{0}' as i32 as i8;
    (*rec).offset = tt_get_unsigned_pair(handle);
    6i32
}
unsafe fn clt_read_range(mut rec: *mut clt_range, mut sfont: *mut sfnt) -> i32 {
    assert!(!rec.is_null() && !sfont.is_null());
    let handle = &mut (*sfont).handle;
    (*rec).Start = tt_get_unsigned_pair(handle);
    (*rec).End = tt_get_unsigned_pair(handle);
    (*rec).StartCoverageIndex = tt_get_unsigned_pair(handle);
    6i32
}
unsafe fn clt_read_record_list(mut list: *mut clt_record_list, mut sfont: *mut sfnt) -> i32 {
    assert!(!list.is_null() && !sfont.is_null());
    (*list).count = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 2;
    if (*list).count as i32 == 0i32 {
        (*list).record = 0 as *mut clt_record
    } else {
        (*list).record = new(((*list).count as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<clt_record>() as u64)
            as u32) as *mut clt_record;
        for i in 0..(*list).count as i32 {
            len += clt_read_record(&mut *(*list).record.offset(i as isize), sfont);
        }
    }
    len
}
unsafe fn clt_release_record_list(mut list: *mut clt_record_list) {
    if !list.is_null() {
        (*list).record = mfree((*list).record as *mut libc::c_void) as *mut clt_record;
        (*list).count = 0_u16
    };
}
unsafe fn clt_read_number_list(mut list: *mut clt_number_list, mut sfont: *mut sfnt) -> i32 {
    assert!(!list.is_null() && !sfont.is_null());
    (*list).count = tt_get_unsigned_pair(&mut (*sfont).handle);
    if (*list).count as i32 == 0i32 {
        (*list).value = 0 as *mut u16
    } else {
        (*list).value = new(
            ((*list).count as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32
        ) as *mut u16;
        for i in 0..(*list).count as i32 {
            *(*list).value.offset(i as isize) = tt_get_unsigned_pair(&mut (*sfont).handle);
        }
    }
    2i32 + 2i32 * (*list).count as i32
}
unsafe fn clt_release_number_list(mut list: *mut clt_number_list) {
    if !list.is_null() {
        (*list).value = mfree((*list).value as *mut libc::c_void) as *mut u16;
        (*list).count = 0_u16
    };
}
unsafe fn clt_read_script_table(mut tab: *mut clt_script_table, mut sfont: *mut sfnt) -> i32 {
    assert!(!tab.is_null() && !sfont.is_null());
    (*tab).DefaultLangSys = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 2;
    len += clt_read_record_list(&mut (*tab).LangSysRecord, sfont);
    len
}
unsafe fn clt_release_script_table(mut tab: *mut clt_script_table) {
    if !tab.is_null() {
        clt_release_record_list(&mut (*tab).LangSysRecord);
    };
}
unsafe fn clt_read_langsys_table(mut tab: *mut clt_langsys_table, mut sfont: *mut sfnt) -> i32 {
    assert!(!tab.is_null() && !sfont.is_null());
    (*tab).LookupOrder = tt_get_unsigned_pair(&mut (*sfont).handle);
    (*tab).ReqFeatureIndex = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 4;
    len += clt_read_number_list(&mut (*tab).FeatureIndex, sfont);
    len
}
unsafe fn clt_release_langsys_table(mut tab: *mut clt_langsys_table) {
    if !tab.is_null() {
        clt_release_number_list(&mut (*tab).FeatureIndex);
    };
}
unsafe fn clt_read_feature_table(mut tab: *mut clt_feature_table, mut sfont: *mut sfnt) -> i32 {
    assert!(!tab.is_null() && !sfont.is_null());
    (*tab).FeatureParams = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 2;
    len += clt_read_number_list(&mut (*tab).LookupListIndex, sfont);
    len
}
unsafe fn clt_release_feature_table(mut tab: *mut clt_feature_table) {
    if !tab.is_null() {
        clt_release_number_list(&mut (*tab).LookupListIndex);
    };
}
unsafe fn clt_read_lookup_table(mut tab: *mut clt_lookup_table, mut sfont: *mut sfnt) -> i32 {
    assert!(!tab.is_null() && !sfont.is_null());
    (*tab).LookupType = tt_get_unsigned_pair(&mut (*sfont).handle);
    (*tab).LookupFlag = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 4;
    len += clt_read_number_list(&mut (*tab).SubTableList, sfont);
    len
}
unsafe fn clt_release_lookup_table(mut tab: *mut clt_lookup_table) {
    if !tab.is_null() {
        clt_release_number_list(&mut (*tab).SubTableList);
    };
}
unsafe fn clt_read_coverage(mut cov: *mut clt_coverage, mut sfont: *mut sfnt) -> i32 {
    assert!(!cov.is_null() && !sfont.is_null());
    (*cov).format = tt_get_unsigned_pair(&mut (*sfont).handle);
    (*cov).count = tt_get_unsigned_pair(&mut (*sfont).handle);
    let mut len = 4;
    match (*cov).format as i32 {
        1 => {
            if (*cov).count as i32 == 0i32 {
                (*cov).list = 0 as *mut GlyphID
            } else {
                (*cov).list = new(((*cov).count as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<u16>() as u64)
                    as u32) as *mut u16;
                for i in 0..(*cov).count as i32 {
                    *(*cov).list.offset(i as isize) = tt_get_unsigned_pair(&mut (*sfont).handle);
                }
            }
            (*cov).range = 0 as *mut clt_range;
            len += 2 * (*cov).count as i32
        }
        2 => {
            if (*cov).count as i32 == 0i32 {
                (*cov).range = 0 as *mut clt_range
            } else {
                (*cov).range = new(((*cov).count as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<clt_range>() as u64)
                    as u32) as *mut clt_range;
                for i in 0..(*cov).count as i32 {
                    len += clt_read_range(&mut *(*cov).range.offset(i as isize), sfont);
                }
            }
            (*cov).list = 0 as *mut GlyphID
        }
        _ => {
            panic!("Unknown coverage format");
        }
    }
    len
}
unsafe fn clt_release_coverage(mut cov: *mut clt_coverage) {
    if !cov.is_null() {
        match (*cov).format as i32 {
            1 => (*cov).list = mfree((*cov).list as *mut libc::c_void) as *mut GlyphID,
            2 => (*cov).range = mfree((*cov).range as *mut libc::c_void) as *mut clt_range,
            _ => {
                panic!("Unknown coverage format");
            }
        }
    }
    (*cov).count = 0_u16;
}
unsafe fn clt_lookup_coverage(mut cov: *mut clt_coverage, mut gid: u16) -> i32 {
    assert!(!cov.is_null());
    match (*cov).format as i32 {
        1 => {
            for i in 0..(*cov).count as i32 {
                if *(*cov).list.offset(i as isize) as i32 > gid as i32 {
                    break;
                }
                if *(*cov).list.offset(i as isize) as i32 == gid as i32 {
                    return i;
                }
            }
        }
        2 => {
            for i in 0..(*cov).count as i32 {
                if (gid as i32) < (*(*cov).range.offset(i as isize)).Start as i32 {
                    break;
                }
                if gid as i32 <= (*(*cov).range.offset(i as isize)).End as i32 {
                    return (*(*cov).range.offset(i as isize)).StartCoverageIndex as i32
                        + gid as i32
                        - (*(*cov).range.offset(i as isize)).Start as i32;
                }
            }
        }
        _ => {
            panic!("Unknown coverage format");
        }
    }
    -1i32
}
unsafe fn otl_gsub_read_single(mut subtab: *mut otl_gsub_subtab, mut sfont: *mut sfnt) -> i32 {
    assert!(!subtab.is_null() && !sfont.is_null());
    let handle = &mut (*sfont).handle;
    let offset = handle.seek(SeekFrom::Current(0)).unwrap();
    (*subtab).LookupType = 1_u16;
    (*subtab).SubstFormat = tt_get_unsigned_pair(handle);
    let mut len = 2;
    if (*subtab).SubstFormat as i32 == 1i32 {
        let data =
            new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub_single1>() as u64) as u32)
                as *mut otl_gsub_single1;
        (*subtab).table.single1 = data;
        let cov_offset = tt_get_unsigned_pair(handle);
        (*data).DeltaGlyphID = tt_get_signed_pair(handle);
        len += 4;
        handle.seek(SeekFrom::Start(offset as u64 + cov_offset as u64)).unwrap();
        len += clt_read_coverage(&mut (*data).coverage, sfont)
    } else if (*subtab).SubstFormat as i32 == 2i32 {
        let data_0 =
            new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub_single2>() as u64) as u32)
                as *mut otl_gsub_single2;
        (*subtab).table.single2 = data_0;
        let cov_offset = tt_get_unsigned_pair(handle);
        (*data_0).GlyphCount = tt_get_unsigned_pair(handle);
        len += 4;
        if (*data_0).GlyphCount as i32 == 0i32 {
            (*data_0).Substitute = 0 as *mut GlyphID
        } else {
            (*data_0).Substitute = new(((*data_0).GlyphCount as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<GlyphID>() as u64)
                as u32) as *mut GlyphID;
            for count in 0..(*data_0).GlyphCount as i32 {
                *(*data_0).Substitute.offset(count as isize) =
                    tt_get_unsigned_pair(handle);
            }
            len += 2 * (*data_0).GlyphCount as i32
        }
        handle.seek(SeekFrom::Start(offset as u64 + cov_offset as u64)).unwrap();
        len += clt_read_coverage(&mut (*data_0).coverage, sfont)
    } else {
        panic!("unexpected SubstFormat");
    }
    len
}
unsafe fn otl_gsub_read_alternate(mut subtab: *mut otl_gsub_subtab, mut sfont: *mut sfnt) -> i32 {
    let mut altset_offsets: clt_number_list = clt_number_list {
        count: 0,
        value: 0 as *mut u16,
    };
    assert!(!subtab.is_null() && !sfont.is_null());
    let offset = (*sfont).handle.seek(SeekFrom::Current(0)).unwrap();
    (*subtab).LookupType = 3_u16;
    (*subtab).SubstFormat = tt_get_unsigned_pair(&mut (*sfont).handle);
    if (*subtab).SubstFormat as i32 != 1i32 {
        warn!(
            "Unknown GSUB SubstFormat for Alternate: {}",
            (*subtab).SubstFormat
        );
        return -1i32;
    }
    let mut len = 2;
    let data = new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub_alternate1>() as u64) as u32)
        as *mut otl_gsub_alternate1;
    (*subtab).table.alternate1 = data;
    let cov_offset = tt_get_unsigned_pair(&mut (*sfont).handle);
    len += 2;
    len += clt_read_number_list(&mut altset_offsets, sfont);
    (*data).AlternateSetCount = altset_offsets.count;
    if (*data).AlternateSetCount as i32 == 0i32 {
        (*data).AlternateSet = 0 as *mut otl_gsub_altset;
        (*data).coverage.count = 0_u16;
        (*data).coverage.format = 0_u16;
        (*data).coverage.list = 0 as *mut GlyphID;
        return len;
    }
    (*data).AlternateSet = new(((*data).AlternateSetCount as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<otl_gsub_altset>() as u64)
        as u32) as *mut otl_gsub_altset;
    let handle = &mut (*sfont).handle;
    for i in 0..(*data).AlternateSetCount as i32 {
        let altset = &mut *(*data).AlternateSet.offset(i as isize) as *mut otl_gsub_altset;
        let altset_offset = offset + (*altset_offsets.value.offset(i as isize) as u64);
        handle.seek(SeekFrom::Start(altset_offset as u64)).unwrap();
        (*altset).GlyphCount = tt_get_unsigned_pair(handle);
        len += 2i32;
        if (*altset).GlyphCount as i32 == 0i32 {
            (*altset).Alternate = 0 as *mut GlyphID;
            break;
        } else {
            (*altset).Alternate = new(((*altset).GlyphCount as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<GlyphID>() as u64)
                as u32) as *mut GlyphID;
            for j in 0..(*altset).GlyphCount as i32 {
                *(*altset).Alternate.offset(j as isize) = tt_get_unsigned_pair(handle);
                len += 2;
            }
        }
    }
    clt_release_number_list(&mut altset_offsets);
    (*sfont).handle.seek(SeekFrom::Start(cov_offset as u64)).unwrap();
    len += clt_read_coverage(&mut (*data).coverage, sfont);
    len
}
unsafe fn otl_gsub_read_ligature(mut subtab: *mut otl_gsub_subtab, mut sfont: *mut sfnt) -> i32 {
    let mut ligset_offsets: clt_number_list = clt_number_list {
        count: 0,
        value: 0 as *mut u16,
    };
    assert!(!subtab.is_null() && !sfont.is_null());
    let offset = (*sfont).handle.seek(SeekFrom::Current(0)).unwrap() as u32;
    (*subtab).LookupType = 4_u16;
    (*subtab).SubstFormat = tt_get_unsigned_pair(&mut (*sfont).handle);
    if (*subtab).SubstFormat as i32 != 1i32 {
        warn!(
            "Unknown GSUB SubstFormat for Ligature: {}",
            (*subtab).SubstFormat
        );
        return -1i32;
    }
    let mut len = 2;
    let data = new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub_ligature1>() as u64) as u32)
        as *mut otl_gsub_ligature1;
    (*subtab).table.ligature1 = data;
    let cov_offset = tt_get_unsigned_pair(&mut (*sfont).handle);
    len += 2;
    len += clt_read_number_list(&mut ligset_offsets, sfont);
    (*data).LigSetCount = ligset_offsets.count;
    if (*data).LigSetCount as i32 == 0i32 {
        (*data).LigatureSet = 0 as *mut otl_gsub_ligset;
        (*data).coverage.count = 0_u16;
        (*data).coverage.format = 0_u16;
        (*data).coverage.list = 0 as *mut GlyphID;
        return len;
    }
    (*data).LigatureSet = new(((*data).LigSetCount as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<otl_gsub_ligset>() as u64)
        as u32) as *mut otl_gsub_ligset;
    for i in 0..(*data).LigSetCount as i32 {
        let mut ligset_tab: clt_number_list = clt_number_list {
            count: 0,
            value: 0 as *mut u16,
        };
        let ligset = &mut *(*data).LigatureSet.offset(i as isize) as *mut otl_gsub_ligset;
        let ligset_offset = offset.wrapping_add(*ligset_offsets.value.offset(i as isize) as u32);
        (*sfont).handle.seek(SeekFrom::Start(ligset_offset as u64)).unwrap();
        len += clt_read_number_list(&mut ligset_tab, sfont);
        (*ligset).LigatureCount = ligset_tab.count;
        if ligset_tab.count as i32 == 0i32 {
            (*ligset).Ligature = 0 as *mut otl_gsub_ligtab;
            break;
        } else {
            let handle = &mut (*sfont).handle;
            (*ligset).Ligature = new((ligset_tab.count as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<otl_gsub_ligtab>() as u64)
                as u32) as *mut otl_gsub_ligtab;
            for j in 0..ligset_tab.count as i32 {
                handle.seek(SeekFrom::Start(ligset_offset as u64 + *ligset_tab.value.offset(j as isize) as u64)).unwrap();
                (*(*ligset).Ligature.offset(j as isize)).LigGlyph =
                    tt_get_unsigned_pair(handle);
                (*(*ligset).Ligature.offset(j as isize)).CompCount =
                    tt_get_unsigned_pair(handle);
                if (*(*ligset).Ligature.offset(j as isize)).CompCount as i32 == 0i32 {
                    let ref mut fresh0 = (*(*ligset).Ligature.offset(j as isize)).Component;
                    *fresh0 = 0 as *mut GlyphID;
                    break;
                } else {
                    let ref mut fresh1 = (*(*ligset).Ligature.offset(j as isize)).Component;
                    *fresh1 = new((((*(*ligset).Ligature.offset(j as isize)).CompCount as i32
                        - 1i32) as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<GlyphID>() as u64)
                        as u32) as *mut GlyphID;
                    let mut count = 0;
                    while (count as i32)
                        < (*(*ligset).Ligature.offset(j as isize)).CompCount as i32 - 1i32
                    {
                        *(*(*ligset).Ligature.offset(j as isize))
                            .Component
                            .offset(count as isize) = tt_get_unsigned_pair(handle);
                        count += 1;
                    }
                    len += 4 + count as i32 * 2;
                }
            }
            clt_release_number_list(&mut ligset_tab);
        }
    }
    clt_release_number_list(&mut ligset_offsets);
    (*sfont).handle.seek(SeekFrom::Start(offset as u64 + cov_offset as u64)).unwrap();
    len += clt_read_coverage(&mut (*data).coverage, sfont);
    len
}
unsafe fn otl_gsub_release_single(mut subtab: *mut otl_gsub_subtab) {
    if !subtab.is_null() {
        match (*subtab).SubstFormat as i32 {
            1 => {
                let data = (*subtab).table.single1;
                if !data.is_null() {
                    clt_release_coverage(&mut (*data).coverage);
                    free(data as *mut libc::c_void);
                }
                (*subtab).table.single1 = 0 as *mut otl_gsub_single1
            }
            2 => {
                let data_0 = (*subtab).table.single2;
                if !data_0.is_null() {
                    free((*data_0).Substitute as *mut libc::c_void);
                    clt_release_coverage(&mut (*data_0).coverage);
                    free(data_0 as *mut libc::c_void);
                }
                (*subtab).table.single2 = 0 as *mut otl_gsub_single2
            }
            _ => {
                panic!("Unknown format for single substitution");
            }
        }
    };
}
unsafe fn otl_gsub_release_ligature(mut subtab: *mut otl_gsub_subtab) {
    if !subtab.is_null() {
        let data = (*subtab).table.ligature1;
        if !data.is_null() && !(*data).LigatureSet.is_null() {
            for i in 0..(*data).LigSetCount as i32 {
                let ligset = &mut *(*data).LigatureSet.offset(i as isize) as *mut otl_gsub_ligset;
                for j in 0..(*ligset).LigatureCount as i32 {
                    let ref mut fresh2 = (*(*ligset).Ligature.offset(j as isize)).Component;
                    *fresh2 = mfree(
                        (*(*ligset).Ligature.offset(j as isize)).Component as *mut libc::c_void,
                    ) as *mut GlyphID;
                }
                (*ligset).Ligature =
                    mfree((*ligset).Ligature as *mut libc::c_void) as *mut otl_gsub_ligtab;
            }
            free((*data).LigatureSet as *mut libc::c_void);
        }
        clt_release_coverage(&mut (*data).coverage);
        (*data).LigatureSet = 0 as *mut otl_gsub_ligset;
        free(data as *mut libc::c_void);
        (*subtab).table.ligature1 = 0 as *mut otl_gsub_ligature1
    };
}
unsafe fn otl_gsub_release_alternate(mut subtab: *mut otl_gsub_subtab) {
    if !subtab.is_null() {
        let data = (*subtab).table.alternate1;
        if !data.is_null() && !(*data).AlternateSet.is_null() {
            for i in 0..(*data).AlternateSetCount as i32 {
                let altset = &mut *(*data).AlternateSet.offset(i as isize) as *mut otl_gsub_altset;
                (*altset).Alternate =
                    mfree((*altset).Alternate as *mut libc::c_void) as *mut GlyphID;
            }
            free((*data).AlternateSet as *mut libc::c_void);
        }
        clt_release_coverage(&mut (*data).coverage);
        (*data).AlternateSet = 0 as *mut otl_gsub_altset;
        free(data as *mut libc::c_void);
        (*subtab).table.alternate1 = 0 as *mut otl_gsub_alternate1
    };
}
unsafe fn otl_gsub_read_header(mut head: *mut otl_gsub_header, mut sfont: *mut sfnt) -> i32 {
    assert!(!head.is_null() && !sfont.is_null());
    let handle = &mut (*sfont).handle;
    (*head).version = tt_get_unsigned_quad(handle);
    (*head).ScriptList = tt_get_unsigned_pair(handle);
    (*head).FeatureList = tt_get_unsigned_pair(handle);
    (*head).LookupList = tt_get_unsigned_pair(handle);
    10i32
}
unsafe fn otl_gsub_read_feat(mut gsub: *mut otl_gsub_tab, mut sfont: *mut sfnt) -> i32 {
    let mut head: otl_gsub_header = otl_gsub_header {
        version: 0,
        ScriptList: 0,
        FeatureList: 0,
        LookupList: 0,
    };
    let mut subtab: *mut otl_gsub_subtab = 0 as *mut otl_gsub_subtab;
    let mut num_subtabs: u16 = 0_u16;
    let mut feat_bits: [u8; 8192] = [0; 8192];
    let mut feature_list: clt_record_list = clt_record_list {
        count: 0,
        record: 0 as *mut clt_record,
    };
    let mut script_list: clt_record_list = clt_record_list {
        count: 0,
        record: 0 as *mut clt_record,
    };
    let mut lookup_list: clt_number_list = clt_number_list {
        count: 0,
        value: 0 as *mut u16,
    };
    assert!(!gsub.is_null() && !sfont.is_null());
    let gsub_offset = sfnt_find_table_pos(sfont, b"GSUB");
    if gsub_offset == 0_u32 {
        return -1i32;
    }
    let script = otl_new_opt();
    otl_parse_optstring(script, (*gsub).script);
    let language = otl_new_opt();
    otl_parse_optstring(language, (*gsub).language);
    let feature = otl_new_opt();
    otl_parse_optstring(feature, (*gsub).feature);
    memset(feat_bits.as_mut_ptr() as *mut libc::c_void, 0i32, 8192);
    (*sfont).handle.seek(SeekFrom::Start(gsub_offset as u64)).unwrap();
    otl_gsub_read_header(&mut head, sfont);
    let mut offset = gsub_offset.wrapping_add(head.ScriptList as u32);
    (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_record_list(&mut script_list, sfont);
    for script_idx in 0..script_list.count as i32 {
        if otl_match_optrule(
            script,
            (*script_list.record.offset(script_idx as isize))
                .tag
                .as_mut_ptr(),
        ) != 0
        {
            let mut script_tab: clt_script_table = clt_script_table {
                DefaultLangSys: 0,
                LangSysRecord: clt_record_list {
                    count: 0,
                    record: 0 as *mut clt_record,
                },
            };
            offset = gsub_offset
                .wrapping_add(head.ScriptList as u32)
                .wrapping_add((*script_list.record.offset(script_idx as isize)).offset as u32);
            (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
            clt_read_script_table(&mut script_tab, sfont);
            if otl_match_optrule(language, b"dflt\x00" as *const u8 as *const i8) != 0
                && script_tab.DefaultLangSys as i32 != 0i32
            {
                let mut langsys_tab: clt_langsys_table = clt_langsys_table {
                    LookupOrder: 0,
                    ReqFeatureIndex: 0,
                    FeatureIndex: clt_number_list {
                        count: 0,
                        value: 0 as *mut u16,
                    },
                };
                if verbose > 0i32 {
                    info!(
                        "otl_gsub>> OTL script-language enabled: {}{}{}{}.dflt\n",
                        char::from((*script_list.record.offset(script_idx as isize)).tag[0] as u8),
                        char::from((*script_list.record.offset(script_idx as isize)).tag[1] as u8),
                        char::from((*script_list.record.offset(script_idx as isize)).tag[2] as u8),
                        char::from((*script_list.record.offset(script_idx as isize)).tag[3] as u8),
                    );
                }
                (*sfont).handle.seek(SeekFrom::Start(offset as u64 + script_tab.DefaultLangSys as u64)).unwrap();
                clt_read_langsys_table(&mut langsys_tab, sfont);
                if otl_match_optrule(feature, b"____\x00" as *const u8 as *const i8) != 0
                    && langsys_tab.ReqFeatureIndex as i32 != 0xffffi32
                {
                    feat_bits[(langsys_tab.ReqFeatureIndex as i32 / 8i32) as usize] =
                        (feat_bits[(langsys_tab.ReqFeatureIndex as i32 / 8i32) as usize] as i32
                            | 1i32 << 7i32 - langsys_tab.ReqFeatureIndex as i32 % 8i32)
                            as u8
                }
                for feat_idx in 0..langsys_tab.FeatureIndex.count as i32 {
                    feat_bits[(*langsys_tab.FeatureIndex.value.offset(feat_idx as isize) as i32
                        / 8i32) as usize] =
                        (feat_bits[(*langsys_tab.FeatureIndex.value.offset(feat_idx as isize)
                            as i32
                            / 8i32) as usize] as i32
                            | 1i32
                                << 7i32
                                    - *langsys_tab.FeatureIndex.value.offset(feat_idx as isize)
                                        as i32
                                        % 8i32) as u8;
                }
                clt_release_langsys_table(&mut langsys_tab);
            }
            for langsys_idx in 0..script_tab.LangSysRecord.count as i32 {
                let langsys_rec = &mut *script_tab.LangSysRecord.record.offset(langsys_idx as isize)
                    as *mut clt_record;
                if otl_match_optrule(language, (*langsys_rec).tag.as_mut_ptr()) != 0 {
                    let mut langsys_tab_0: clt_langsys_table = clt_langsys_table {
                        LookupOrder: 0,
                        ReqFeatureIndex: 0,
                        FeatureIndex: clt_number_list {
                            count: 0,
                            value: 0 as *mut u16,
                        },
                    };
                    if verbose > 0i32 {
                        info!(
                            "otl_gsub>> OTL script-language enabled: {}{}{}{}.{}{}{}{}\n",
                            char::from(
                                (*script_list.record.offset(script_idx as isize)).tag[0] as u8
                            ),
                            char::from(
                                (*script_list.record.offset(script_idx as isize)).tag[1] as u8
                            ),
                            char::from(
                                (*script_list.record.offset(script_idx as isize)).tag[2] as u8
                            ),
                            char::from(
                                (*script_list.record.offset(script_idx as isize)).tag[3] as u8
                            ),
                            char::from((*langsys_rec).tag[0] as u8),
                            char::from((*langsys_rec).tag[1] as u8),
                            char::from((*langsys_rec).tag[2] as u8),
                            char::from((*langsys_rec).tag[3] as u8),
                        );
                    }
                    (*sfont).handle.seek(SeekFrom::Start(offset as u64 + (*langsys_rec).offset as u64)).unwrap();
                    clt_read_langsys_table(&mut langsys_tab_0, sfont);
                    if otl_match_optrule(feature, b"____\x00" as *const u8 as *const i8) != 0
                        || langsys_tab_0.ReqFeatureIndex as i32 != 0xffffi32
                    {
                        feat_bits[(langsys_tab_0.ReqFeatureIndex as i32 / 8i32) as usize] =
                            (feat_bits[(langsys_tab_0.ReqFeatureIndex as i32 / 8i32) as usize]
                                as i32
                                | 1i32 << 7i32 - langsys_tab_0.ReqFeatureIndex as i32 % 8i32)
                                as u8
                    }
                    for feat_idx in 0..langsys_tab_0.FeatureIndex.count as i32 {
                        feat_bits[(*langsys_tab_0.FeatureIndex.value.offset(feat_idx as isize)
                            as i32
                            / 8i32) as usize] =
                            (feat_bits[(*langsys_tab_0.FeatureIndex.value.offset(feat_idx as isize)
                                as i32
                                / 8i32) as usize] as i32
                                | 1i32
                                    << 7i32
                                        - *langsys_tab_0
                                            .FeatureIndex
                                            .value
                                            .offset(feat_idx as isize)
                                            as i32
                                            % 8i32) as u8;
                    }
                    clt_release_langsys_table(&mut langsys_tab_0);
                }
            }
            clt_release_script_table(&mut script_tab);
        }
    }
    offset = gsub_offset.wrapping_add(head.FeatureList as u32);
    (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_record_list(&mut feature_list, sfont);
    offset = gsub_offset.wrapping_add(head.LookupList as u32);
    (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_number_list(&mut lookup_list, sfont);
    if verbose > 0i32 {
        info!("otl_gsub>> Reading OTL feature(s):");
    }
    for feat_idx in 0..feature_list.count as i32 {
        if feat_bits[(feat_idx / 8i32) as usize] as i32 & 1i32 << 7i32 - feat_idx % 8i32 != 0
            && otl_match_optrule(
                feature,
                (*feature_list.record.offset(feat_idx as isize))
                    .tag
                    .as_mut_ptr(),
            ) != 0
        {
            let mut feature_table: clt_feature_table = clt_feature_table {
                FeatureParams: 0,
                LookupListIndex: clt_number_list {
                    count: 0,
                    value: 0 as *mut u16,
                },
            };
            if verbose > 0i32 {
                info!(
                    " {}{}{}{}",
                    char::from((*feature_list.record.offset(feat_idx as isize)).tag[0] as u8),
                    char::from((*feature_list.record.offset(feat_idx as isize)).tag[1] as u8),
                    char::from((*feature_list.record.offset(feat_idx as isize)).tag[2] as u8),
                    char::from((*feature_list.record.offset(feat_idx as isize)).tag[3] as u8),
                );
            }
            offset = gsub_offset
                .wrapping_add(head.FeatureList as u32)
                .wrapping_add((*feature_list.record.offset(feat_idx as isize)).offset as u32);
            (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
            clt_read_feature_table(&mut feature_table, sfont);
            if feature_table.FeatureParams as i32 != 0i32 {
                panic!("unrecognized FeatureParams");
            }
            for i in 0..feature_table.LookupListIndex.count as i32 {
                let mut lookup_table: clt_lookup_table = clt_lookup_table {
                    LookupType: 0,
                    LookupFlag: 0,
                    SubTableList: clt_number_list {
                        count: 0,
                        value: 0 as *mut u16,
                    },
                };
                let ll_idx = *feature_table.LookupListIndex.value.offset(i as isize) as i32;
                if ll_idx >= lookup_list.count as i32 {
                    panic!("invalid Lookup index.");
                }
                offset = gsub_offset
                    .wrapping_add(head.LookupList as u32)
                    .wrapping_add(*lookup_list.value.offset(ll_idx as isize) as u32);
                (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
                clt_read_lookup_table(&mut lookup_table, sfont);
                if lookup_table.LookupType as i32 != 1i32
                    && lookup_table.LookupType as i32 != 3i32
                    && lookup_table.LookupType as i32 != 4i32
                    && lookup_table.LookupType as i32 != 7i32
                {
                    if verbose > 0i32 {
                        warn!(
                            "Skipping unsupported GSUB subtable: LookupType={}",
                            lookup_table.LookupType as i32
                        );
                    }
                } else {
                    subtab = renew(
                        subtab as *mut libc::c_void,
                        ((num_subtabs as i32 + lookup_table.SubTableList.count as i32) as u32
                            as u64)
                            .wrapping_mul(::std::mem::size_of::<otl_gsub_subtab>() as u64)
                            as u32,
                    ) as *mut otl_gsub_subtab;
                    let mut n_st = 0;
                    for st_idx in 0..lookup_table.SubTableList.count as i32 {
                        offset = gsub_offset
                            .wrapping_add(head.LookupList as u32)
                            .wrapping_add(*lookup_list.value.offset(ll_idx as isize) as u32)
                            .wrapping_add(
                                *lookup_table.SubTableList.value.offset(st_idx as isize) as u32
                            );
                        (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
                        match lookup_table.LookupType as i32 {
                            1 => {
                                let r = otl_gsub_read_single(
                                    &mut *subtab.offset((num_subtabs as i32 + n_st) as isize),
                                    sfont,
                                );
                                if r <= 0i32 {
                                    warn!("Reading GSUB subtable (single) failed...");
                                } else {
                                    if verbose > 0i32 {
                                        info!("(single)");
                                    }
                                    n_st += 1
                                }
                            }
                            3 => {
                                let r = otl_gsub_read_alternate(
                                    &mut *subtab.offset((num_subtabs as i32 + n_st) as isize),
                                    sfont,
                                );
                                if r <= 0i32 {
                                    warn!("Reading GSUB subtable (alternate) failed...");
                                } else {
                                    if verbose > 0i32 {
                                        info!("(alternate)");
                                    }
                                    n_st += 1
                                }
                            }
                            4 => {
                                let r = otl_gsub_read_ligature(
                                    &mut *subtab.offset((num_subtabs as i32 + n_st) as isize),
                                    sfont,
                                );
                                if r <= 0i32 {
                                    warn!("Reading GSUB subtable (ligature) failed...");
                                } else {
                                    if verbose > 0i32 {
                                        info!("(ligature)");
                                    }
                                    n_st += 1
                                }
                            }
                            7 => {
                                let SubstFormat = tt_get_unsigned_pair(&mut (*sfont).handle);
                                if !(SubstFormat as i32 != 1i32) {
                                    let ExtensionLookupType = tt_get_unsigned_pair(&mut (*sfont).handle);
                                    let ExtensionOffset = tt_get_unsigned_quad(&mut (*sfont).handle) as u64;
                                    (*sfont).handle.seek(SeekFrom::Start(offset as u64 + ExtensionOffset)).unwrap();
                                    match ExtensionLookupType as i32 {
                                        1 => {
                                            let r = otl_gsub_read_single(
                                                &mut *subtab
                                                    .offset((num_subtabs as i32 + n_st) as isize),
                                                sfont,
                                            );
                                            if r <= 0i32 {
                                                warn!(
                                                    "Reading GSUB subtable (ext:single) failed..."
                                                );
                                            } else {
                                                if verbose > 0i32 {
                                                    info!("(ext:single)",);
                                                }
                                                n_st += 1
                                            }
                                        }
                                        3 => {
                                            let r = otl_gsub_read_alternate(
                                                &mut *subtab
                                                    .offset((num_subtabs as i32 + n_st) as isize),
                                                sfont,
                                            );
                                            if r <= 0i32 {
                                                warn!(
                                                    "Reading GSUB subtable (alternate) failed..."
                                                );
                                            } else {
                                                if verbose > 0i32 {
                                                    info!("(alternate)",);
                                                }
                                                n_st += 1
                                            }
                                        }
                                        4 => {
                                            let r = otl_gsub_read_ligature(
                                                &mut *subtab
                                                    .offset((num_subtabs as i32 + n_st) as isize),
                                                sfont,
                                            );
                                            if r <= 0i32 {
                                                warn!("Reading GSUB subtable (ext:ligature) failed...");
                                            } else {
                                                if verbose > 0i32 {
                                                    info!("(ext:ligature)",);
                                                }
                                                n_st += 1
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    num_subtabs = (num_subtabs as i32 + n_st) as u16;
                    clt_release_lookup_table(&mut lookup_table);
                }
            }
            clt_release_feature_table(&mut feature_table);
        }
    }
    if verbose > 0i32 {
        info!("\n");
        info!("otl_gsub>> {} subtable(s) read.\n", num_subtabs as i32);
    }
    clt_release_number_list(&mut lookup_list);
    clt_release_record_list(&mut feature_list);
    clt_release_record_list(&mut script_list);
    otl_release_opt(script);
    otl_release_opt(language);
    otl_release_opt(feature);
    if !subtab.is_null() {
        (*gsub).num_subtables = num_subtabs as i32;
        (*gsub).subtables = subtab
    } else {
        return -1i32;
    }
    0i32
}
unsafe fn otl_gsub_apply_single(mut subtab: *mut otl_gsub_subtab, mut gid: *mut u16) -> i32 {
    assert!(!subtab.is_null() && !gid.is_null());
    if (*subtab).SubstFormat as i32 == 1i32 {
        let data = (*subtab).table.single1;
        let idx = clt_lookup_coverage(&mut (*data).coverage, *gid);
        if idx >= 0i32 {
            *gid = (*gid as i32 + (*data).DeltaGlyphID as i32) as u16;
            return 0i32;
        }
    } else if (*subtab).SubstFormat as i32 == 2i32 {
        let data_0 = (*subtab).table.single2;
        let idx = clt_lookup_coverage(&mut (*data_0).coverage, *gid);
        if idx >= 0i32 && idx < (*data_0).GlyphCount as i32 {
            *gid = *(*data_0).Substitute.offset(idx as isize);
            return 0i32;
        }
    }
    -1i32
}
unsafe fn otl_gsub_apply_alternate(
    mut subtab: *mut otl_gsub_subtab,
    mut alt_idx: u16,
    mut gid: *mut u16,
) -> i32 {
    assert!(!subtab.is_null() && !gid.is_null());
    if (*subtab).SubstFormat as i32 == 1i32 {
        let data = (*subtab).table.alternate1;
        let idx = clt_lookup_coverage(&mut (*data).coverage, *gid);
        if idx < 0i32 || idx >= (*data).AlternateSetCount as i32 {
            return -1i32;
        } else {
            let altset = &mut *(*data).AlternateSet.offset(idx as isize) as *mut otl_gsub_altset;
            if alt_idx as i32 >= (*altset).GlyphCount as i32 {
                return -1i32;
            } else {
                *gid = *(*altset).Alternate.offset(alt_idx as isize);
                return 0i32;
            }
        }
    }
    -1i32
}
unsafe fn glyph_seq_cmp(
    mut glyph_seq0: *mut GlyphID,
    mut n_glyphs0: u16,
    mut glyph_seq1: *mut GlyphID,
    mut n_glyphs1: u16,
) -> i32 {
    if n_glyphs0 as i32 != n_glyphs1 as i32 {
        return n_glyphs0 as i32 - n_glyphs1 as i32;
    }
    for i in 0..n_glyphs0 as i32 {
        if *glyph_seq0.offset(i as isize) as i32 != *glyph_seq1.offset(i as isize) as i32 {
            return *glyph_seq0.offset(i as isize) as i32 - *glyph_seq1.offset(i as isize) as i32;
        }
    }
    0i32
}
unsafe fn otl_gsub_apply_ligature(
    mut subtab: *mut otl_gsub_subtab,
    mut gid_in: *mut u16,
    mut num_gids: u16,
    mut gid_out: *mut u16,
) -> i32 {
    assert!(!subtab.is_null() && !gid_out.is_null());
    if gid_in.is_null() || (num_gids as i32) < 1i32 {
        return -1i32;
    }
    if (*subtab).SubstFormat as i32 == 1i32 {
        let data = (*subtab).table.ligature1;
        let idx = clt_lookup_coverage(&mut (*data).coverage, *gid_in.offset(0));
        if idx >= 0i32 && idx < (*data).LigSetCount as i32 {
            let ligset = &mut *(*data).LigatureSet.offset(idx as isize) as *mut otl_gsub_ligset;
            for j in 0..(*ligset).LigatureCount as i32 {
                if glyph_seq_cmp(
                    &mut *gid_in.offset(1),
                    (num_gids as i32 - 1i32) as u16,
                    (*(*ligset).Ligature.offset(j as isize)).Component,
                    ((*(*ligset).Ligature.offset(j as isize)).CompCount as i32 - 1i32) as u16,
                ) == 0
                {
                    *gid_out = (*(*ligset).Ligature.offset(j as isize)).LigGlyph;
                    return 0i32;
                }
            }
        }
    }
    -1i32
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_new() -> *mut otl_gsub {
    let gsub_list =
        new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub>() as u64) as u32) as *mut otl_gsub;
    (*gsub_list).num_gsubs = 0i32;
    (*gsub_list).select = -1i32;
    (*gsub_list).first = 0 as *mut gsub_entry;
    gsub_list as *mut otl_gsub
}
unsafe fn clear_chain(mut gsub_list: *mut otl_gsub) {
    let mut entry = (*gsub_list).first;
    while !entry.is_null() {
        let next = (*entry).next;
        free(entry as *mut libc::c_void);
        entry = next
    }
    (*gsub_list).first = 0 as *mut gsub_entry;
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_add_feat(
    mut gsub_list: *mut otl_gsub,
    mut script: &[u8],
    mut language: &[u8],
    mut feature: &[u8],
    mut sfont: *mut sfnt,
) -> i32 {
    if (*gsub_list).num_gsubs > 32i32 {
        panic!("Too many GSUB features...");
    }
    let mut i = 0;
    while i < (*gsub_list).num_gsubs {
        let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
        if script == CStr::from_ptr((*gsub).script).to_bytes()
            && language == CStr::from_ptr((*gsub).language).to_bytes()
            && feature == CStr::from_ptr((*gsub).feature).to_bytes()
        {
            (*gsub_list).select = i;
            return 0i32;
        }
        i += 1
    }
    let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
    (*gsub).script = CString::new(script).unwrap().into_raw();
    (*gsub).language = CString::new(language).unwrap().into_raw();
    (*gsub).feature = CString::new(feature).unwrap().into_raw();
    if verbose > 0i32 {
        info!("\n");
        info!(
            "otl_gsub>> Reading \"{}.{}.{}\"...\n",
            CString::new(script).unwrap().display(),
            CString::new(language).unwrap().display(),
            CString::new(feature).unwrap().display(),
        );
    }
    let retval = otl_gsub_read_feat(gsub, sfont);
    if retval >= 0i32 {
        (*gsub_list).select = i;
        (*gsub_list).num_gsubs += 1
    } else {
        if verbose > 0i32 {
            info!("otl_gsub>> Failed\n");
        }
        let _ = CString::from_raw((*gsub).script);
        let _ = CString::from_raw((*gsub).language);
        let _ = CString::from_raw((*gsub).feature);
    }
    retval
}
fn scan_otl_tag(mut otl_tags: &[u8]) -> Result<(Vec<u8>, Vec<u8>, Vec<u8>), ()> {
    let mut script;
    let mut language = vec![b' '; 4];
    if otl_tags.is_empty() {
        return Err(());
    }
    /* First parse otl_tags variable */
    let mut p = otl_tags;

    if let Some(slen) = p.iter().position(|&x| x == b'.') {
        /* Format scrp.lang.feat */
        if slen < 5 {
            script = Vec::from(&p[..slen]);
        } else {
            warn!("Invalid OTL script tag found: {}", p.display());
            return Err(());
        }
        p = &p[slen + 1..];
        if let Some(llen) = p.iter().position(|&x| x == b'.') {
            /* Now lang part */
            if llen < 5 {
                language = Vec::from(&p[..llen]);
            } else {
                warn!("Invalid OTL lanuage tag found: {}", p.display(),);
                return Err(());
            }
            p = &p[llen + 1..];
        }
    } else {
        script = vec![b'*'];
        language = vec![b'*'];
    }
    /* Finally feature */
    let feature = if p.len() < 5 {
        Vec::from(p)
    } else {
        warn!("No valid OTL feature tag specified.");
        return Err(());
    };
    Ok((script, language, feature))
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_release(mut gsub_list: *mut otl_gsub) {
    if gsub_list.is_null() {
        return;
    }
    for i in 0..(*gsub_list).num_gsubs {
        let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
        let _ = CString::from_raw((*gsub).script);
        let _ = CString::from_raw((*gsub).language);
        let _ = CString::from_raw((*gsub).feature);
        for j in 0..(*gsub).num_subtables {
            let subtab = &mut *(*gsub).subtables.offset(j as isize) as *mut otl_gsub_subtab;
            match (*subtab).LookupType as i32 {
                1 => {
                    otl_gsub_release_single(subtab);
                }
                3 => {
                    otl_gsub_release_alternate(subtab);
                }
                4 => {
                    otl_gsub_release_ligature(subtab);
                }
                _ => {
                    panic!("???");
                }
            }
        }
        free((*gsub).subtables as *mut libc::c_void);
    }
    clear_chain(gsub_list);
    free(gsub_list as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_apply(mut gsub_list: *mut otl_gsub, mut gid: *mut u16) -> i32 {
    let mut retval: i32 = -1i32;
    if gsub_list.is_null() || gid.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0i32 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
    let mut j = 0;
    while retval < 0i32 && j < (*gsub).num_subtables {
        let subtab = &mut *(*gsub).subtables.offset(j as isize) as *mut otl_gsub_subtab;
        match (*subtab).LookupType as i32 {
            1 => retval = otl_gsub_apply_single(subtab, gid),
            _ => {}
        }
        j += 1
    }
    retval
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_apply_alt(
    mut gsub_list: *mut otl_gsub,
    mut alt_idx: u16,
    mut gid: *mut u16,
) -> i32 {
    let mut retval: i32 = -1i32;
    if gsub_list.is_null() || gid.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0i32 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
    let mut j = 0;
    while retval < 0i32 && j < (*gsub).num_subtables {
        let subtab = &mut *(*gsub).subtables.offset(j as isize) as *mut otl_gsub_subtab;
        match (*subtab).LookupType as i32 {
            3 => retval = otl_gsub_apply_alternate(subtab, alt_idx, gid),
            _ => {}
        }
        j += 1
    }
    retval
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_apply_lig(
    mut gsub_list: *mut otl_gsub,
    mut gid_in: *mut u16,
    mut num_gids: u16,
    mut gid_out: *mut u16,
) -> i32 {
    let mut retval: i32 = -1i32;
    if gsub_list.is_null() || gid_out.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0i32 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
    let mut j = 0;
    while retval < 0i32 && j < (*gsub).num_subtables {
        let subtab = &mut *(*gsub).subtables.offset(j as isize) as *mut otl_gsub_subtab;
        match (*subtab).LookupType as i32 {
            4 => retval = otl_gsub_apply_ligature(subtab, gid_in, num_gids, gid_out),
            _ => {}
        }
        j += 1
    }
    retval
}
unsafe fn gsub_find(
    mut gsub_list: *mut otl_gsub,
    script: &[u8],
    language: &[u8],
    feature: &[u8],
) -> i32 {
    for i in 0..(*gsub_list).num_gsubs {
        let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
        if CStr::from_ptr((*gsub).script).to_bytes() == script
            && CStr::from_ptr((*gsub).language).to_bytes() == language
            && CStr::from_ptr((*gsub).feature).to_bytes() == feature
        {
            return i;
        }
    }
    -1i32
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_select(
    mut gsub_list: *mut otl_gsub,
    mut script: &[u8],
    mut language: &[u8],
    mut feature: &[u8],
) -> i32 {
    (*gsub_list).select = gsub_find(gsub_list, script, language, feature);
    (*gsub_list).select
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_set_chain(
    mut gsub_list: *mut otl_gsub,
    mut otl_tags: *const i8,
) -> i32 {
    let mut prev: *mut gsub_entry = 0 as *mut gsub_entry;
    clear_chain(gsub_list);
    for p in CStr::from_ptr(otl_tags).to_bytes().split(|&c| c == b':') {
        if let Ok((script, language, feature)) = scan_otl_tag(p) {
            let idx = gsub_find(gsub_list, &script, &language, &feature);
            if idx >= 0i32 && idx <= (*gsub_list).num_gsubs {
                let entry =
                    new((1_u64).wrapping_mul(::std::mem::size_of::<gsub_entry>() as u64) as u32)
                        as *mut gsub_entry;
                if (*gsub_list).first.is_null() {
                    (*gsub_list).first = entry
                }
                if !prev.is_null() {
                    (*prev).next = entry
                }
                (*entry).index = idx;
                prev = entry
            }
        }
    }
    if !prev.is_null() {
        (*prev).next = 0 as *mut gsub_entry
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_add_feat_list(
    mut gsub_list: *mut otl_gsub,
    mut otl_tags: *const i8,
    mut sfont: *mut sfnt,
) -> i32 {
    if gsub_list.is_null() || otl_tags.is_null() || sfont.is_null() {
        return -1i32;
    }
    clear_chain(gsub_list);
    for p in CStr::from_ptr(otl_tags).to_bytes().split(|&c| c == b':') {
        if let Ok((script, language, feature)) = scan_otl_tag(p) {
            let idx = gsub_find(gsub_list, &script, &language, &feature);
            if idx < 0i32 {
                otl_gsub_add_feat(gsub_list, &script, &language, &feature, sfont);
            }
        }
    }
    0i32
}
/* LookupType for GSUB */
/* Handle a list of OTL features */
#[no_mangle]
pub unsafe extern "C" fn otl_gsub_apply_chain(
    mut gsub_list: *mut otl_gsub,
    mut gid: *mut u16,
) -> i32 {
    let mut retval: i32 = -1i32;
    if gsub_list.is_null() || gid.is_null() {
        return retval;
    }
    let mut entry = (*gsub_list).first;
    while !entry.is_null() {
        let idx = (*entry).index;
        if !(idx < 0i32 || idx >= (*gsub_list).num_gsubs) {
            let gsub =
                &mut *(*gsub_list).gsubs.as_mut_ptr().offset(idx as isize) as *mut otl_gsub_tab;
            let mut i = 0;
            retval = -1i32;
            while retval < 0i32 && i < (*gsub).num_subtables {
                let subtab = &mut *(*gsub).subtables.offset(i as isize) as *mut otl_gsub_subtab;
                match (*subtab).LookupType as i32 {
                    1 => retval = otl_gsub_apply_single(subtab, gid),
                    _ => {}
                }
                i += 1
            }
        }
        entry = (*entry).next
    }
    retval
}
