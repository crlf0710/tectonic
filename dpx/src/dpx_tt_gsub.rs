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

use crate::bridge::DisplayExt;
use std::ffi::{CStr, CString};
use std::io::Read;
use std::ptr;

use super::dpx_numbers::GetFromFile;
use super::dpx_sfnt::sfnt_find_table_pos;
use crate::{info, warn};

use super::dpx_mem::new;
use super::dpx_otl_opt::OtlOpt;
use libc::{free, memset};

use std::io::{Seek, SeekFrom};

pub(crate) type Fixed = u32;

use super::dpx_sfnt::sfnt;

#[derive(Clone)]
#[repr(C)]
pub(crate) struct otl_gsub {
    pub(crate) num_gsubs: i32,
    pub(crate) select: i32,
    pub(crate) first: *mut gsub_entry,
    pub(crate) gsubs: [otl_gsub_tab; 32],
    /* _TT_GSUB_H_ */
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct otl_gsub_tab {
    pub(crate) script: *mut i8,
    pub(crate) language: *mut i8,
    pub(crate) feature: *mut i8,
    pub(crate) subtables: Vec<SubTable>,
}

#[derive(Clone)]
pub(crate) enum SingleSubTable {
    Single1(otl_gsub_single1),
    Single2(otl_gsub_single2),
}
#[derive(Clone)]
pub(crate) enum SubTable {
    Single(SingleSubTable),
    Alternate1(otl_gsub_alternate1),
    Ligature1(otl_gsub_ligature1),
}

#[derive(Clone)]
pub(crate) struct otl_gsub_ligature1 {
    pub(crate) LigatureSet: Vec<otl_gsub_ligset>,
    pub(crate) coverage: Coverage,
}

#[derive(Clone)]
pub(crate) enum Coverage {
    Format1(Vec<GlyphID>),
    Format2(Vec<Range>),
}

#[derive(Copy, Clone)]
pub(crate) struct Range {
    start: GlyphID,
    end: GlyphID,
    start_coverage_index: u16,
}

pub(crate) type GlyphID = u16;
#[derive(Clone)]
#[repr(C)]
pub(crate) struct otl_gsub_ligset {
    pub(crate) Ligature: Vec<otl_gsub_ligtab>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct otl_gsub_ligtab {
    pub(crate) LigGlyph: GlyphID,
    pub(crate) Component: Vec<GlyphID>,
}
#[derive(Clone)]
pub(crate) struct otl_gsub_alternate1 {
    pub(crate) AlternateSet: Vec<otl_gsub_altset>,
    pub(crate) coverage: Coverage,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct otl_gsub_altset {
    pub(crate) Alternate: Vec<GlyphID>,
}
#[derive(Clone)]
pub(crate) struct otl_gsub_single2 {
    pub(crate) Substitute: Vec<GlyphID>,
    pub(crate) coverage: Coverage,
}
#[derive(Clone)]
pub(crate) struct otl_gsub_single1 {
    pub(crate) DeltaGlyphID: i16,
    pub(crate) coverage: Coverage,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct gsub_entry {
    pub(crate) index: i32,
    pub(crate) next: *mut gsub_entry,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct clt_record {
    pub(crate) tag: [i8; 5],
    pub(crate) offset: Offset,
}
pub(crate) type Offset = u16;
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_record_list {
    pub(crate) record: Vec<clt_record>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_number_list {
    pub(crate) value: Vec<u16>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_feature_table {
    pub(crate) FeatureParams: Offset,
    pub(crate) LookupListIndex: clt_number_list,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_lookup_table {
    pub(crate) LookupType: u16,
    pub(crate) LookupFlag: u16,
    pub(crate) SubTableList: clt_number_list,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct otl_gsub_header {
    pub(crate) version: Fixed,
    pub(crate) ScriptList: Offset,
    pub(crate) FeatureList: Offset,
    pub(crate) LookupList: Offset,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_script_table {
    pub(crate) DefaultLangSys: Offset,
    pub(crate) LangSysRecord: clt_record_list,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct clt_langsys_table {
    pub(crate) LookupOrder: Offset,
    pub(crate) ReqFeatureIndex: u16,
    pub(crate) FeatureIndex: clt_number_list,
}

static mut verbose: i32 = 0;

pub(crate) unsafe fn otl_gsub_set_verbose(level: i32) {
    verbose = level;
}

unsafe fn clt_read_record<R: Read>(handle: &mut R) -> (clt_record, i32) {
    let mut tag = [0; 5];
    for i in 0..4 {
        tag[i] = i8::get(handle) as i8;
    }
    tag[4] = '\u{0}' as i32 as i8;
    let offset = u16::get(handle);
    (clt_record { tag, offset }, 6)
}

fn read_range<R: Read>(handle: &mut R) -> (Range, usize) {
    (
        Range {
            start: u16::get(handle),
            end: u16::get(handle),
            start_coverage_index: u16::get(handle),
        },
        6,
    )
}

unsafe fn clt_read_record_list<R: Read>(list: &mut clt_record_list, handle: &mut R) -> i32 {
    let count = u16::get(handle);
    let mut len = 2;
    if count == 0 {
        list.record = Vec::new()
    } else {
        list.record = Vec::with_capacity(count as _);
        for _ in 0..count {
            let (rec, dlen) = clt_read_record(handle);
            list.record.push(rec);
            len += dlen;
        }
    }
    len
}
unsafe fn clt_read_number_list<R: Read>(list: &mut clt_number_list, handle: &mut R) -> i32 {
    let count = u16::get(handle);
    if count == 0 {
        list.value = Vec::new();
    } else {
        list.value = Vec::with_capacity(count as _);
        for _ in 0..count {
            list.value.push(u16::get(handle));
        }
    }
    2 + 2 * count as i32
}
unsafe fn clt_read_script_table<R: Read>(tab: &mut clt_script_table, handle: &mut R) -> i32 {
    tab.DefaultLangSys = u16::get(handle);
    let mut len = 2;
    len += clt_read_record_list(&mut tab.LangSysRecord, handle);
    len
}
unsafe fn clt_read_langsys_table<R: Read>(tab: &mut clt_langsys_table, handle: &mut R) -> i32 {
    tab.LookupOrder = u16::get(handle);
    tab.ReqFeatureIndex = u16::get(handle);
    let mut len = 4;
    len += clt_read_number_list(&mut tab.FeatureIndex, handle);
    len
}
unsafe fn clt_read_feature_table<R: Read>(tab: &mut clt_feature_table, handle: &mut R) -> i32 {
    tab.FeatureParams = u16::get(handle);
    let mut len = 2;
    len += clt_read_number_list(&mut tab.LookupListIndex, handle);
    len
}
unsafe fn clt_read_lookup_table<R: Read>(tab: &mut clt_lookup_table, handle: &mut R) -> i32 {
    tab.LookupType = u16::get(handle);
    tab.LookupFlag = u16::get(handle);
    let mut len = 4;
    len += clt_read_number_list(&mut tab.SubTableList, handle);
    len
}

// TODO: Maybe return result?
fn read_coverage<R: Read>(handle: &mut R) -> (Coverage, usize) {
    let format = u16::get(handle);
    let count = u16::get(handle);
    let mut len = 4;
    match format {
        1 => {
            let mut list = Vec::new();
            if count != 0 {
                for _ in 0..count {
                    list.push(u16::get(handle));
                }
            }
            len += 2 * count as usize;
            (Coverage::Format1(list), len)
        }
        2 => {
            let mut range = Vec::new();
            if count != 0 {
                for _ in 0..count {
                    let (r, l) = read_range(handle);
                    range.push(r);
                    len += l;
                }
            }
            (Coverage::Format2(range), len)
        }
        _ => {
            panic!("Unknown coverage format");
        }
    }
}

impl Coverage {
    fn lookup(&self, gid: u16) -> Option<i32> {
        match self {
            &Coverage::Format1(ref list) => list
                .iter()
                .enumerate()
                .take_while(|(_pos, i)| **i <= gid)
                .find(|(_pos, i)| **i == gid)
                .map(|(pos, _)| pos as i32),
            &Coverage::Format2(ref range) => range
                .iter()
                .take_while(|range| gid >= range.start)
                .find(|range| gid <= range.end)
                .map(|range| (range.start_coverage_index + gid - range.start) as i32),
        }
    }
}

impl Default for Coverage {
    fn default() -> Self {
        Coverage::Format1(Vec::new())
    }
}

unsafe fn otl_gsub_read_single<R: Read + Seek>(handle: &mut R) -> Option<SubTable> {
    let offset = handle.seek(SeekFrom::Current(0)).unwrap();
    let SubstFormat = u16::get(handle);
    if SubstFormat == 1 {
        let cov_offset = u16::get(handle);
        let DeltaGlyphID = i16::get(handle);
        handle
            .seek(SeekFrom::Start(offset as u64 + cov_offset as u64))
            .unwrap();
        let (coverage, _) = read_coverage(handle);
        Some(SubTable::Single(SingleSubTable::Single1(
            otl_gsub_single1 {
                DeltaGlyphID,
                coverage,
            },
        )))
    } else if SubstFormat == 2 {
        let cov_offset = u16::get(handle);
        let GlyphCount = u16::get(handle);
        let mut Substitute = Vec::new();
        if GlyphCount as i32 == 0 {
        } else {
            Substitute = Vec::with_capacity(GlyphCount as _);
            for _ in 0..GlyphCount as i32 {
                Substitute.push(u16::get(handle));
            }
        }
        handle
            .seek(SeekFrom::Start(offset as u64 + cov_offset as u64))
            .unwrap();
        let (coverage, _) = read_coverage(handle);
        Some(SubTable::Single(SingleSubTable::Single2(
            otl_gsub_single2 {
                Substitute,
                coverage,
            },
        )))
    } else {
        panic!("unexpected SubstFormat");
    }
}
unsafe fn otl_gsub_read_alternate<R: Read + Seek>(handle: &mut R) -> Option<SubTable> {
    let mut altset_offsets: clt_number_list = clt_number_list { value: Vec::new() };
    let offset = handle.seek(SeekFrom::Current(0)).unwrap();
    let SubstFormat = u16::get(handle);
    if SubstFormat as i32 != 1 {
        warn!("Unknown GSUB SubstFormat for Alternate: {}", SubstFormat);
        return None;
    }
    let cov_offset = u16::get(handle);
    clt_read_number_list(&mut altset_offsets, handle);
    let AlternateSetCount = altset_offsets.value.len() as usize;
    if AlternateSetCount == 0 {
        return Some(SubTable::Alternate1(otl_gsub_alternate1 {
            AlternateSet: Vec::new(),
            coverage: Coverage::default(),
        }));
    }
    let mut AlternateSet = Vec::with_capacity(AlternateSetCount);
    for i in 0..AlternateSetCount {
        let altset_offset = offset + (altset_offsets.value[i] as u64);
        handle.seek(SeekFrom::Start(altset_offset as u64)).unwrap();
        let GlyphCount = u16::get(handle) as usize;
        if GlyphCount == 0 {
            break;
        } else {
            let mut Alternate = Vec::with_capacity(GlyphCount);
            for _ in 0..GlyphCount {
                Alternate.push(u16::get(handle));
            }
            AlternateSet.push(otl_gsub_altset { Alternate });
        }
    }
    handle.seek(SeekFrom::Start(cov_offset as u64)).unwrap();
    let (coverage, _) = read_coverage(handle);
    Some(SubTable::Alternate1(otl_gsub_alternate1 {
        AlternateSet,
        coverage,
    }))
}
unsafe fn otl_gsub_read_ligature<R: Read + Seek>(handle: &mut R) -> Option<SubTable> {
    let mut ligset_offsets: clt_number_list = clt_number_list { value: Vec::new() };
    let offset = handle.seek(SeekFrom::Current(0)).unwrap() as u32;
    let SubstFormat = u16::get(handle);
    if SubstFormat as i32 != 1 {
        warn!("Unknown GSUB SubstFormat for Ligature: {}", SubstFormat);
        return None;
    }
    let cov_offset = u16::get(handle);
    clt_read_number_list(&mut ligset_offsets, handle);
    let LigSetCount = ligset_offsets.value.len();
    if LigSetCount == 0 {
        return Some(SubTable::Ligature1(otl_gsub_ligature1 {
            LigatureSet: Vec::new(),
            coverage: Coverage::default(),
        }));
    }
    let mut LigatureSet = Vec::with_capacity(LigSetCount);
    for i in 0..LigSetCount {
        let mut ligset_tab: clt_number_list = clt_number_list { value: Vec::new() };
        let ligset_offset = offset.wrapping_add(ligset_offsets.value[i as usize] as u32);
        handle.seek(SeekFrom::Start(ligset_offset as u64)).unwrap();
        clt_read_number_list(&mut ligset_tab, handle);
        if ligset_tab.value.is_empty() {
            break;
        } else {
            let mut Ligature = Vec::with_capacity(ligset_tab.value.len());
            for &lt in ligset_tab.value.iter() {
                handle
                    .seek(SeekFrom::Start(ligset_offset as u64 + lt as u64))
                    .unwrap();
                let LigGlyph = u16::get(handle);
                let CompCount = u16::get(handle);
                if CompCount == 0 {
                    break;
                } else {
                    let mut Component = Vec::with_capacity((CompCount - 1) as _);
                    for _ in 0..CompCount - 1 {
                        Component.push(u16::get(handle));
                    }
                    Ligature.push(otl_gsub_ligtab {
                        LigGlyph,
                        Component,
                    });
                }
            }
            LigatureSet.push(otl_gsub_ligset { Ligature });
        }
    }
    handle
        .seek(SeekFrom::Start(offset as u64 + cov_offset as u64))
        .unwrap();
    let (coverage, _) = read_coverage(handle);
    Some(SubTable::Ligature1(otl_gsub_ligature1 {
        LigatureSet,
        coverage,
    }))
}
unsafe fn otl_gsub_read_header<R: Read>(head: &mut otl_gsub_header, handle: &mut R) -> i32 {
    head.version = u32::get(handle);
    head.ScriptList = u16::get(handle);
    head.FeatureList = u16::get(handle);
    head.LookupList = u16::get(handle);
    10
}
unsafe fn otl_gsub_read_feat(gsub: &mut otl_gsub_tab, sfont: &sfnt) -> i32 {
    let mut head: otl_gsub_header = otl_gsub_header {
        version: 0,
        ScriptList: 0,
        FeatureList: 0,
        LookupList: 0,
    };
    let mut subtab = Vec::new();
    let mut feat_bits: [u8; 8192] = [0; 8192];
    let mut feature_list = clt_record_list { record: Vec::new() };
    let mut script_list = clt_record_list { record: Vec::new() };
    let mut lookup_list = clt_number_list { value: Vec::new() };
    let gsub_offset = sfnt_find_table_pos(sfont, b"GSUB");
    let handle = &mut &*sfont.handle;
    if gsub_offset == 0_u32 {
        return -1;
    }
    let script = OtlOpt::parse_optstring(CStr::from_ptr(gsub.script).to_str().unwrap());
    let language = OtlOpt::parse_optstring(CStr::from_ptr(gsub.language).to_str().unwrap());
    let feature = OtlOpt::parse_optstring(CStr::from_ptr(gsub.feature).to_str().unwrap());
    memset(feat_bits.as_mut_ptr() as *mut libc::c_void, 0, 8192);
    handle.seek(SeekFrom::Start(gsub_offset as u64)).unwrap();
    otl_gsub_read_header(&mut head, handle);
    let mut offset = gsub_offset.wrapping_add(head.ScriptList as u32);
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_record_list(&mut script_list, handle);
    for rec in script_list.record.iter() {
        if script.match_expr(CStr::from_ptr(rec.tag.as_ptr()).to_str().unwrap()) {
            let mut script_tab = clt_script_table {
                DefaultLangSys: 0,
                LangSysRecord: clt_record_list { record: Vec::new() },
            };
            offset = gsub_offset
                .wrapping_add(head.ScriptList as u32)
                .wrapping_add(rec.offset as u32);
            handle.seek(SeekFrom::Start(offset as u64)).unwrap();
            clt_read_script_table(&mut script_tab, handle);
            if language.match_expr("dflt") && script_tab.DefaultLangSys as i32 != 0 {
                let mut langsys_tab = clt_langsys_table {
                    LookupOrder: 0,
                    ReqFeatureIndex: 0,
                    FeatureIndex: clt_number_list { value: Vec::new() },
                };
                if verbose > 0 {
                    info!(
                        "otl_gsub>> OTL script-language enabled: {}{}{}{}.dflt\n",
                        char::from(rec.tag[0] as u8),
                        char::from(rec.tag[1] as u8),
                        char::from(rec.tag[2] as u8),
                        char::from(rec.tag[3] as u8),
                    );
                }
                handle
                    .seek(SeekFrom::Start(
                        offset as u64 + script_tab.DefaultLangSys as u64,
                    ))
                    .unwrap();
                clt_read_langsys_table(&mut langsys_tab, handle);
                if feature.match_expr("____") && langsys_tab.ReqFeatureIndex as i32 != 0xffff {
                    feat_bits[(langsys_tab.ReqFeatureIndex as i32 / 8) as usize] =
                        (feat_bits[(langsys_tab.ReqFeatureIndex as i32 / 8) as usize] as i32
                            | 1 << 7 - langsys_tab.ReqFeatureIndex as i32 % 8)
                            as u8
                }
                for &feat in langsys_tab.FeatureIndex.value.iter() {
                    feat_bits[(feat as i32 / 8) as usize] =
                        (feat_bits[(feat as i32 / 8) as usize] as i32 | 1 << 7 - feat as i32 % 8)
                            as u8;
                }
            }
            for langsys_rec in script_tab.LangSysRecord.record.iter() {
                if language.match_expr(CStr::from_ptr(langsys_rec.tag.as_ptr()).to_str().unwrap()) {
                    let mut langsys_tab_0 = clt_langsys_table {
                        LookupOrder: 0,
                        ReqFeatureIndex: 0,
                        FeatureIndex: clt_number_list { value: Vec::new() },
                    };
                    if verbose > 0 {
                        info!(
                            "otl_gsub>> OTL script-language enabled: {}{}{}{}.{}{}{}{}\n",
                            char::from(rec.tag[0] as u8),
                            char::from(rec.tag[1] as u8),
                            char::from(rec.tag[2] as u8),
                            char::from(rec.tag[3] as u8),
                            char::from(langsys_rec.tag[0] as u8),
                            char::from(langsys_rec.tag[1] as u8),
                            char::from(langsys_rec.tag[2] as u8),
                            char::from(langsys_rec.tag[3] as u8),
                        );
                    }
                    handle
                        .seek(SeekFrom::Start(offset as u64 + langsys_rec.offset as u64))
                        .unwrap();
                    clt_read_langsys_table(&mut langsys_tab_0, handle);
                    if feature.match_expr("____") || langsys_tab_0.ReqFeatureIndex as i32 != 0xffff
                    {
                        feat_bits[(langsys_tab_0.ReqFeatureIndex as i32 / 8) as usize] =
                            (feat_bits[(langsys_tab_0.ReqFeatureIndex as i32 / 8) as usize] as i32
                                | 1 << 7 - langsys_tab_0.ReqFeatureIndex as i32 % 8)
                                as u8
                    }
                    for &feat in langsys_tab_0.FeatureIndex.value.iter() {
                        feat_bits[(feat as i32 / 8) as usize] =
                            (feat_bits[(feat as i32 / 8) as usize] as i32
                                | 1 << 7 - feat as i32 % 8) as u8;
                    }
                }
            }
        }
    }
    offset = gsub_offset.wrapping_add(head.FeatureList as u32);
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_record_list(&mut feature_list, handle);
    offset = gsub_offset.wrapping_add(head.LookupList as u32);
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    clt_read_number_list(&mut lookup_list, handle);
    if verbose > 0 {
        info!("otl_gsub>> Reading OTL feature(s):");
    }
    for (feat_idx, rec) in feature_list.record.iter().enumerate() {
        if feat_bits[(feat_idx / 8) as usize] as i32 & 1 << 7 - feat_idx % 8 != 0
            && feature.match_expr(CStr::from_ptr(rec.tag.as_ptr()).to_str().unwrap())
        {
            let mut feature_table: clt_feature_table = clt_feature_table {
                FeatureParams: 0,
                LookupListIndex: clt_number_list { value: Vec::new() },
            };
            if verbose > 0 {
                info!(
                    " {}{}{}{}",
                    char::from(rec.tag[0] as u8),
                    char::from(rec.tag[1] as u8),
                    char::from(rec.tag[2] as u8),
                    char::from(rec.tag[3] as u8),
                );
            }
            offset = gsub_offset
                .wrapping_add(head.FeatureList as u32)
                .wrapping_add(rec.offset as u32);
            handle.seek(SeekFrom::Start(offset as u64)).unwrap();
            clt_read_feature_table(&mut feature_table, handle);
            if feature_table.FeatureParams as i32 != 0 {
                panic!("unrecognized FeatureParams");
            }
            for &lli in feature_table.LookupListIndex.value.iter() {
                let mut lookup_table: clt_lookup_table = clt_lookup_table {
                    LookupType: 0,
                    LookupFlag: 0,
                    SubTableList: clt_number_list { value: Vec::new() },
                };
                let ll_idx = lli as usize;
                if ll_idx >= lookup_list.value.len() {
                    panic!("invalid Lookup index.");
                }
                offset = gsub_offset
                    .wrapping_add(head.LookupList as u32)
                    .wrapping_add(lookup_list.value[ll_idx] as u32);
                handle.seek(SeekFrom::Start(offset as u64)).unwrap();
                clt_read_lookup_table(&mut lookup_table, handle);
                if lookup_table.LookupType as i32 != 1
                    && lookup_table.LookupType as i32 != 3
                    && lookup_table.LookupType as i32 != 4
                    && lookup_table.LookupType as i32 != 7
                {
                    if verbose > 0 {
                        warn!(
                            "Skipping unsupported GSUB subtable: LookupType={}",
                            lookup_table.LookupType as i32
                        );
                    }
                } else {
                    for &st in lookup_table.SubTableList.value.iter() {
                        offset = gsub_offset
                            .wrapping_add(head.LookupList as u32)
                            .wrapping_add(lookup_list.value[ll_idx] as u32)
                            .wrapping_add(st as u32);
                        handle.seek(SeekFrom::Start(offset as u64)).unwrap();
                        match lookup_table.LookupType as i32 {
                            1 => {
                                if let Some(st) = otl_gsub_read_single(handle) {
                                    subtab.push(st);
                                    if verbose > 0 {
                                        info!("(single)");
                                    }
                                } else {
                                    warn!("Reading GSUB subtable (single) failed...");
                                }
                            }
                            3 => {
                                if let Some(st) = otl_gsub_read_alternate(handle) {
                                    subtab.push(st);
                                    if verbose > 0 {
                                        info!("(alternate)");
                                    }
                                } else {
                                    warn!("Reading GSUB subtable (alternate) failed...");
                                }
                            }
                            4 => {
                                if let Some(st) = otl_gsub_read_ligature(handle) {
                                    subtab.push(st);
                                    if verbose > 0 {
                                        info!("(ligature)");
                                    }
                                } else {
                                    warn!("Reading GSUB subtable (ligature) failed...");
                                }
                            }
                            7 => {
                                let SubstFormat = u16::get(handle);
                                if !(SubstFormat as i32 != 1) {
                                    let ExtensionLookupType = u16::get(handle);
                                    let ExtensionOffset = u32::get(handle) as u64;
                                    handle
                                        .seek(SeekFrom::Start(offset as u64 + ExtensionOffset))
                                        .unwrap();
                                    match ExtensionLookupType as i32 {
                                        1 => {
                                            if let Some(st) = otl_gsub_read_single(handle) {
                                                subtab.push(st);
                                                if verbose > 0 {
                                                    info!("(ext:single)");
                                                }
                                            } else {
                                                warn!(
                                                    "Reading GSUB subtable (ext:single) failed..."
                                                );
                                            }
                                        }
                                        3 => {
                                            if let Some(st) = otl_gsub_read_alternate(handle) {
                                                subtab.push(st);
                                                if verbose > 0 {
                                                    info!("(alternate)");
                                                }
                                            } else {
                                                warn!(
                                                    "Reading GSUB subtable (alternate) failed..."
                                                );
                                            }
                                        }
                                        4 => {
                                            if let Some(st) = otl_gsub_read_ligature(handle) {
                                                subtab.push(st);
                                                if verbose > 0 {
                                                    info!("(ext:ligature)");
                                                }
                                            } else {
                                                warn!("Reading GSUB subtable (ext:ligature) failed...");
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }
    if verbose > 0 {
        info!("\n");
        info!("otl_gsub>> {} subtable(s) read.\n", subtab.len());
    }
    if !subtab.is_empty() {
        gsub.subtables = subtab;
    } else {
        return -1;
    }
    0
}
unsafe fn otl_gsub_apply_single(subtab: &SingleSubTable, gid: *mut u16) -> i32 {
    assert!(!gid.is_null());
    match subtab {
        SingleSubTable::Single1(data) => {
            if let Some(_idx) = data.coverage.lookup(*gid) {
                *gid = (*gid as i32 + data.DeltaGlyphID as i32) as u16;
                return 0;
            }
        }
        SingleSubTable::Single2(data) => {
            if let Some(idx) = data.coverage.lookup(*gid) {
                if idx < data.Substitute.len() as i32 {
                    *gid = data.Substitute[idx as usize];
                    return 0;
                }
            }
        }
    }
    -1
}
unsafe fn otl_gsub_apply_alternate(
    subtab: &otl_gsub_alternate1,
    alt_idx: u16,
    gid: *mut u16,
) -> i32 {
    assert!(!gid.is_null());
    if let Some(idx) = subtab.coverage.lookup(*gid) {
        if idx >= subtab.AlternateSet.len() as i32 {
            return -1;
        }
        let altset = &subtab.AlternateSet[idx as usize];
        if alt_idx as i32 >= altset.Alternate.len() as i32 {
            return -1;
        } else {
            *gid = altset.Alternate[alt_idx as usize];
            return 0;
        }
    } else {
        return -1;
    }
}
unsafe fn glyph_seq_cmp(glyph_seq0: &[GlyphID], glyph_seq1: &[GlyphID]) -> i32 {
    if glyph_seq0.len() != glyph_seq1.len() {
        return glyph_seq0.len() as i32 - glyph_seq1.len() as i32;
    }
    for i in 0..glyph_seq0.len() {
        if glyph_seq0[i] != glyph_seq1[i] {
            return (glyph_seq0[i] as i32) - (glyph_seq1[i] as i32);
        }
    }
    0
}
unsafe fn otl_gsub_apply_ligature(
    subtab: &otl_gsub_ligature1,
    gid_in: &[u16],
    gid_out: *mut u16,
) -> i32 {
    assert!(!gid_out.is_null());
    if gid_in.is_empty() {
        return -1;
    }
    if let Some(idx) = subtab.coverage.lookup(gid_in[0]) {
        if idx < subtab.LigatureSet.len() as i32 {
            let ligset = &subtab.LigatureSet[idx as usize];
            for lig in ligset.Ligature.iter() {
                if glyph_seq_cmp(&gid_in[1..], &lig.Component[..lig.Component.len() - 1]) == 0 {
                    *gid_out = lig.LigGlyph;
                    return 0;
                }
            }
        }
    }
    -1
}

pub(crate) unsafe fn otl_gsub_new() -> *mut otl_gsub {
    let gsub_list =
        new((1_u64).wrapping_mul(::std::mem::size_of::<otl_gsub>() as u64) as u32) as *mut otl_gsub;
    (*gsub_list).num_gsubs = 0;
    (*gsub_list).select = -1;
    (*gsub_list).first = ptr::null_mut();
    gsub_list as *mut otl_gsub
}
unsafe fn clear_chain(mut gsub_list: *mut otl_gsub) {
    let mut entry = (*gsub_list).first;
    while !entry.is_null() {
        let next = (*entry).next;
        free(entry as *mut libc::c_void);
        entry = next
    }
    (*gsub_list).first = ptr::null_mut();
}

pub(crate) unsafe fn otl_gsub_add_feat(
    gsub_list: &mut otl_gsub,
    script: &[u8],
    language: &[u8],
    feature: &[u8],
    sfont: &sfnt,
) -> i32 {
    if gsub_list.num_gsubs > 32 {
        panic!("Too many GSUB features...");
    }
    let mut i = 0;
    while i < gsub_list.num_gsubs {
        let gsub = &*gsub_list.gsubs.as_mut_ptr().offset(i as isize);
        if script == CStr::from_ptr(gsub.script).to_bytes()
            && language == CStr::from_ptr(gsub.language).to_bytes()
            && feature == CStr::from_ptr(gsub.feature).to_bytes()
        {
            gsub_list.select = i;
            return 0;
        }
        i += 1
    }
    let gsub = &mut *gsub_list.gsubs.as_mut_ptr().offset(i as isize);
    gsub.script = CString::new(script).unwrap().into_raw();
    gsub.language = CString::new(language).unwrap().into_raw();
    gsub.feature = CString::new(feature).unwrap().into_raw();
    if verbose > 0 {
        info!("\n");
        info!(
            "otl_gsub>> Reading \"{}.{}.{}\"...\n",
            CString::new(script).unwrap().display(),
            CString::new(language).unwrap().display(),
            CString::new(feature).unwrap().display(),
        );
    }
    let retval = otl_gsub_read_feat(gsub, sfont);
    if retval >= 0 {
        gsub_list.select = i;
        gsub_list.num_gsubs += 1
    } else {
        if verbose > 0 {
            info!("otl_gsub>> Failed\n");
        }
        let _ = CString::from_raw(gsub.script);
        let _ = CString::from_raw(gsub.language);
        let _ = CString::from_raw(gsub.feature);
    }
    retval
}
fn scan_otl_tag(otl_tags: &[u8]) -> Result<(Vec<u8>, Vec<u8>, Vec<u8>), ()> {
    let script;
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
                warn!("Invalid OTL lanuage tag found: {}", p.display());
                return Err(());
            }
            p = &p[llen + 1..];
        }
    } else {
        script = vec![b'*'];
        language = vec![b'*'];
    }
    /* Finally feature */
    let feature = if p.len() >= 4 {
        Vec::from(p)
    } else {
        warn!("No valid OTL feature tag specified.");
        return Err(());
    };
    Ok((script, language, feature))
}

pub(crate) unsafe fn otl_gsub_release(gsub_list: *mut otl_gsub) {
    if gsub_list.is_null() {
        return;
    }
    for i in 0..(*gsub_list).num_gsubs {
        let gsub = &mut (*gsub_list).gsubs[i as usize];
        let _ = CString::from_raw(gsub.script);
        let _ = CString::from_raw(gsub.language);
        let _ = CString::from_raw(gsub.feature);
        gsub.subtables = Vec::new();
    }
    clear_chain(gsub_list);
    free(gsub_list as *mut libc::c_void);
}

pub(crate) unsafe fn otl_gsub_apply(gsub_list: *mut otl_gsub, gid: *mut u16) -> i32 {
    let mut retval: i32 = -1;
    if gsub_list.is_null() || gid.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &(*gsub_list).gsubs[i as usize];
    let mut j = 0;
    while retval < 0 && j < gsub.subtables.len() {
        if let SubTable::Single(subtab) = &gsub.subtables[j as usize] {
            retval = otl_gsub_apply_single(subtab, gid);
        }
        j += 1
    }
    retval
}

pub(crate) unsafe fn otl_gsub_apply_alt(
    gsub_list: *mut otl_gsub,
    alt_idx: u16,
    gid: *mut u16,
) -> i32 {
    let mut retval: i32 = -1;
    if gsub_list.is_null() || gid.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &(*gsub_list).gsubs[i as usize];
    let mut j = 0;
    while retval < 0 && j < gsub.subtables.len() {
        if let SubTable::Alternate1(subtab) = &gsub.subtables[j] {
            retval = otl_gsub_apply_alternate(subtab, alt_idx, gid);
        }
        j += 1
    }
    retval
}

pub(crate) unsafe fn otl_gsub_apply_lig(
    gsub_list: *mut otl_gsub,
    gid_in: &[u16],
    gid_out: *mut u16,
) -> i32 {
    let mut retval: i32 = -1;
    if gsub_list.is_null() || gid_out.is_null() {
        return retval;
    }
    let i = (*gsub_list).select;
    if i < 0 || i >= (*gsub_list).num_gsubs {
        panic!("GSUB not selected...");
    }
    let gsub = &mut *(*gsub_list).gsubs.as_mut_ptr().offset(i as isize) as *mut otl_gsub_tab;
    let mut j = 0;
    while retval < 0 && j < (*gsub).subtables.len() {
        if let SubTable::Ligature1(subtab) = &(*gsub).subtables[j] {
            retval = otl_gsub_apply_ligature(subtab, gid_in, gid_out);
        }
        j += 1
    }
    retval
}
unsafe fn gsub_find(gsub_list: &otl_gsub, script: &[u8], language: &[u8], feature: &[u8]) -> i32 {
    for i in 0..gsub_list.num_gsubs {
        let gsub = &*gsub_list.gsubs.as_ptr().offset(i as isize);
        if CStr::from_ptr(gsub.script).to_bytes() == script
            && CStr::from_ptr(gsub.language).to_bytes() == language
            && CStr::from_ptr(gsub.feature).to_bytes() == feature
        {
            return i;
        }
    }
    -1
}

pub(crate) unsafe fn otl_gsub_select(
    mut gsub_list: *mut otl_gsub,
    script: &[u8],
    language: &[u8],
    feature: &[u8],
) -> i32 {
    (*gsub_list).select = gsub_find(&*gsub_list, script, language, feature);
    (*gsub_list).select
}

pub(crate) unsafe fn otl_gsub_set_chain(mut gsub_list: *mut otl_gsub, otl_tags: *const i8) -> i32 {
    let mut prev: *mut gsub_entry = ptr::null_mut();
    clear_chain(gsub_list);
    for p in CStr::from_ptr(otl_tags).to_bytes().split(|&c| c == b':') {
        if let Ok((script, language, feature)) = scan_otl_tag(p) {
            let idx = gsub_find(&*gsub_list, &script, &language, &feature);
            if idx >= 0 && idx <= (*gsub_list).num_gsubs {
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
        (*prev).next = ptr::null_mut()
    }
    0
}

pub(crate) unsafe fn otl_gsub_add_feat_list(
    gsub_list: *mut otl_gsub,
    otl_tags: *const i8,
    sfont: &sfnt,
) -> i32 {
    if gsub_list.is_null() || otl_tags.is_null() {
        return -1;
    }
    clear_chain(gsub_list);
    for p in CStr::from_ptr(otl_tags).to_bytes().split(|&c| c == b':') {
        if let Ok((script, language, feature)) = scan_otl_tag(p) {
            let idx = gsub_find(&*gsub_list, &script, &language, &feature);
            if idx < 0 {
                otl_gsub_add_feat(&mut *gsub_list, &script, &language, &feature, sfont);
            }
        }
    }
    0
}
/* LookupType for GSUB */
/* Handle a list of OTL features */

pub(crate) unsafe fn otl_gsub_apply_chain(gsub_list: &otl_gsub, gid: *mut u16) -> i32 {
    let mut retval: i32 = -1;
    if gid.is_null() {
        return retval;
    }
    let mut entry = gsub_list.first;
    while !entry.is_null() {
        let idx = (*entry).index;
        if !(idx < 0 || idx >= gsub_list.num_gsubs) {
            let gsub = &gsub_list.gsubs[idx as usize];
            let mut i = 0;
            retval = -1;
            while retval < 0 && i < gsub.subtables.len() {
                if let SubTable::Single(subtab) = &gsub.subtables[i] {
                    retval = otl_gsub_apply_single(subtab, gid);
                }
                i += 1;
            }
        }
        entry = (*entry).next
    }
    retval
}
