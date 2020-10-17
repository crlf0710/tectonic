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

use super::dpx_numbers::{get_positive_quad, GetFromFile};
use crate::FromBEByteSlice;
use crate::{info, warn};

use super::dpx_numbers::skip_bytes;
use crate::bridge::{ttstub_input_get_size, InFile, TTInputFormat};

use std::io::{Read, Seek, SeekFrom};

pub(crate) type __off_t = i64;
pub(crate) type off_t = __off_t;

pub(crate) type fixword = i32;
#[derive(Clone, Default)]
pub(crate) struct font_metric {
    pub(crate) tex_name: String,
    pub(crate) designsize: fixword,
    pub(crate) codingscheme: Vec<u8>,
    pub(crate) fontdir: i32,
    pub(crate) firstchar: i32,
    pub(crate) lastchar: i32,
    pub(crate) widths: Vec<fixword>,
    pub(crate) heights: Vec<fixword>,
    pub(crate) depths: Vec<fixword>,
    #[allow(unused)]
    pub(crate) charmap: CharMap,
    pub(crate) source: i32,
}

#[allow(unused)]
#[derive(Clone)]
pub(crate) enum CharMap {
    None,
    Char(char_map),
    Range(range_map),
}

impl Default for CharMap {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed {
    pub(crate) type_0: i32,
    pub(crate) data: *mut libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct coverage {
    pub(crate) first_char: i32,
    pub(crate) num_chars: i32,
}
/* quasi-hack to get the primary input */
/*
 * TFM Record structure:
 * Multiple TFM's may be read in at once.
 */
#[derive(Clone, Default)]
pub(crate) struct tfm_font {
    pub(crate) level: i32,
    pub(crate) wlenfile: u32,
    pub(crate) wlenheader: u32,
    pub(crate) bc: u32,
    pub(crate) ec: u32,
    pub(crate) nwidths: u32,
    pub(crate) nheights: u32,
    pub(crate) ndepths: u32,
    pub(crate) nitcor: u32,
    pub(crate) nlig: u32,
    pub(crate) nkern: u32,
    pub(crate) nextens: u32,
    pub(crate) nfonparm: u32,
    pub(crate) fontdir: u32,
    pub(crate) nco: u32,
    pub(crate) ncw: u32,
    pub(crate) npc: u32,
    pub(crate) header: Vec<u8>,
    pub(crate) char_info: Vec<u32>,
    pub(crate) width_index: Vec<u16>,
    pub(crate) height_index: Vec<u8>,
    pub(crate) depth_index: Vec<u8>,
    pub(crate) width: Vec<fixword>,
    pub(crate) height: Vec<fixword>,
    pub(crate) depth: Vec<fixword>,
}

/*
 * All characters in the same range have same metrics.
 */
#[derive(Clone)]
pub(crate) struct range_map {
    pub(crate) coverages: Vec<coverage>,
    pub(crate) indices: Vec<u16>,
}
/* Special case of num_coverages = 1 */
#[derive(Clone)]
pub(crate) struct char_map {
    pub(crate) coverage: coverage,
    pub(crate) indices: Vec<u16>,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0;

fn lookup_char(map: &char_map, charcode: i32) -> i32 {
    if charcode >= map.coverage.first_char
        && charcode <= map.coverage.first_char + map.coverage.num_chars
    {
        return map.indices[(charcode - map.coverage.first_char) as usize] as i32;
    } else {
        return -1i32;
    };
}
fn lookup_range(map: &range_map, charcode: i32) -> i32 {
    let mut idx = map.coverages.len() as i32 - 1;
    while idx >= 0 && charcode >= map.coverages[idx as usize].first_char {
        if charcode
            <= map.coverages[idx as usize].first_char + map.coverages[idx as usize].num_chars
        {
            return map.indices[idx as usize] as i32;
        }
        idx -= 1
    }
    -1i32
}

static mut fms: Vec<font_metric> = Vec::new();

pub(crate) unsafe fn tfm_reset_global_state() {
    fms = Vec::new();
}

pub(crate) unsafe fn tfm_set_verbose(level: i32) {
    verbose = level;
}
fn fread_fwords<R: Read>(words: &mut [fixword], handle: &mut R) -> i32 {
    let len = words.len();
    for w in words {
        *w = i32::get(handle);
    }
    (len * 4) as i32
}
fn fread_uquads<R: Read>(quads: &mut [u32], handle: &mut R) -> i32 {
    let len = quads.len();
    for w in quads {
        *w = u32::get(handle);
    }
    (len * 4) as i32
}
/*
 * TFM and JFM
 */
fn tfm_check_size(tfm: &tfm_font, tfm_file_size: off_t) {
    let mut expected_size = 6_u32;
    /* Removed the warning message caused by EC TFM metric files.
     *
     if (tfm->wlenfile != tfm_file_size / 4) {
     warn!("TFM file size is {} bytes but it says it is {} bytes!",
     tfm_file_size, tfm->wlenfile * 4);
     if (tfm_file_size > tfm->wlenfile * 4) {
     warn!("Proceeding nervously...");
     } else {
     panic!("Can't proceed...");
     }
     }
    */
    if tfm_file_size < tfm.wlenfile as i64 * 4i32 as i64 {
        panic!("Can\'t proceed...");
    }
    expected_size += (tfm.ec - tfm.bc + 1) as u32;
    expected_size += tfm.wlenheader as u32;
    expected_size += tfm.nwidths as u32;
    expected_size += tfm.nheights as u32;
    expected_size += tfm.ndepths as u32;
    expected_size += tfm.nitcor as u32;
    expected_size += tfm.nlig as u32;
    expected_size += tfm.nkern as u32;
    expected_size += tfm.nextens as u32;
    expected_size += tfm.nfonparm as u32;
    if expected_size != tfm.wlenfile {
        warn!(
            "TFM file size is expected to be {} bytes but it says it is {}bytes!",
            expected_size as i64 * 4,
            tfm.wlenfile as i64 * 4,
        );
        if tfm_file_size > expected_size as i64 * 4 {
            warn!("Proceeding nervously...");
        } else {
            panic!("Can\'t proceed...");
        }
    };
}
fn tfm_get_sizes<R: Read>(tfm_handle: &mut R, tfm_file_size: off_t, tfm: &mut tfm_font) {
    tfm.wlenfile = u16::get(tfm_handle) as u32;
    tfm.wlenheader = u16::get(tfm_handle) as u32;
    tfm.bc = u16::get(tfm_handle) as u32;
    tfm.ec = u16::get(tfm_handle) as u32;
    if tfm.ec < tfm.bc {
        panic!("TFM file error: ec({}) < bc({}) ???", tfm.ec, tfm.bc);
    }
    tfm.nwidths = u16::get(tfm_handle) as u32;
    tfm.nheights = u16::get(tfm_handle) as u32;
    tfm.ndepths = u16::get(tfm_handle) as u32;
    tfm.nitcor = u16::get(tfm_handle) as u32;
    tfm.nlig = u16::get(tfm_handle) as u32;
    tfm.nkern = u16::get(tfm_handle) as u32;
    tfm.nextens = u16::get(tfm_handle) as u32;
    tfm.nfonparm = u16::get(tfm_handle) as u32;
    tfm_check_size(tfm, tfm_file_size);
}
fn tfm_unpack_arrays(fm: &mut font_metric, tfm: &tfm_font) {
    fm.widths = vec![0; 256];
    fm.heights = vec![0; 256];
    fm.depths = vec![0; 256];
    for i in tfm.bc..=tfm.ec {
        let charinfo = tfm.char_info[(i - tfm.bc) as usize];
        let width_index = (charinfo >> 24) as u16;
        let height_index = (charinfo >> 20 & 0xf) as u8;
        let depth_index = (charinfo >> 16 & 0xf) as u8;
        fm.widths[i as usize] = tfm.width[width_index as usize];
        fm.heights[i as usize] = tfm.height[height_index as usize];
        fm.depths[i as usize] = tfm.depth[depth_index as usize];
    }
}
fn tfm_unpack_header(fm: &mut font_metric, tfm: &tfm_font) {
    if tfm.wlenheader < 12 {
        fm.codingscheme = Vec::new();
    } else {
        let len = tfm.header[8] as usize;
        if len > 39 {
            panic!("Invalid TFM header.");
        }
        if len > 0 {
            fm.codingscheme = Vec::from(&tfm.header[9..9 + len]);
            fm.codingscheme.push(0);
        } else {
            fm.codingscheme = Vec::new();
        }
    }
    fm.designsize = i32::from_be_byte_slice(&tfm.header[4..8]);
}
fn ofm_check_size_one(tfm: &tfm_font, ofm_file_size: off_t) {
    let mut ofm_size = 14_u32;
    ofm_size += (2 * (tfm.ec - tfm.bc + 1)) as u32;
    ofm_size += tfm.wlenheader as u32;
    ofm_size += tfm.nwidths as u32;
    ofm_size += tfm.nheights as u32;
    ofm_size += tfm.ndepths as u32;
    ofm_size += tfm.nitcor as u32;
    ofm_size += (2 * tfm.nlig) as u32;
    ofm_size += tfm.nkern as u32;
    ofm_size += (2 * tfm.nextens) as u32;
    ofm_size += tfm.nfonparm as u32;
    if tfm.wlenfile as i64 != ofm_file_size / 4 || tfm.wlenfile != ofm_size {
        panic!("OFM file problem.  Table sizes don\'t agree.");
    };
}
fn ofm_get_sizes<R: Read + Seek>(ofm_handle: &mut R, ofm_file_size: off_t, tfm: &mut tfm_font) {
    tfm.level = i32::get(ofm_handle);
    tfm.wlenfile = get_positive_quad(ofm_handle, "OFM", "wlenfile");
    tfm.wlenheader = get_positive_quad(ofm_handle, "OFM", "wlenheader");
    tfm.bc = get_positive_quad(ofm_handle, "OFM", "bc");
    tfm.ec = get_positive_quad(ofm_handle, "OFM", "ec");
    if tfm.ec < tfm.bc {
        panic!("OFM file error: ec({}) < bc({}) ???", tfm.ec, tfm.bc);
    }
    tfm.nwidths = get_positive_quad(ofm_handle, "OFM", "nwidths");
    tfm.nheights = get_positive_quad(ofm_handle, "OFM", "nheights");
    tfm.ndepths = get_positive_quad(ofm_handle, "OFM", "ndepths");
    tfm.nitcor = get_positive_quad(ofm_handle, "OFM", "nitcor");
    tfm.nlig = get_positive_quad(ofm_handle, "OFM", "nlig");
    tfm.nkern = get_positive_quad(ofm_handle, "OFM", "nkern");
    tfm.nextens = get_positive_quad(ofm_handle, "OFM", "nextens");
    tfm.nfonparm = get_positive_quad(ofm_handle, "OFM", "nfonparm");
    tfm.fontdir = get_positive_quad(ofm_handle, "OFM", "fontdir");
    if tfm.fontdir != 0 {
        warn!("I may be interpreting a font direction incorrectly.");
    }
    if tfm.level == 0i32 {
        ofm_check_size_one(tfm, ofm_file_size);
    } else if tfm.level == 1i32 {
        tfm.nco = get_positive_quad(ofm_handle, "OFM", "nco");
        tfm.ncw = get_positive_quad(ofm_handle, "OFM", "nco");
        tfm.npc = get_positive_quad(ofm_handle, "OFM", "npc");
        ofm_handle
            .seek(SeekFrom::Start(4 * (tfm.nco - tfm.wlenheader) as u64))
            .unwrap();
    } else {
        panic!("can\'t handle OFM files with level > 1");
    };
}
fn ofm_do_char_info_zero<R: Read>(ofm_handle: &mut R, tfm: &mut tfm_font) {
    let num_chars = (tfm.ec - tfm.bc + 1) as usize;
    if num_chars != 0 {
        tfm.width_index = vec![0; num_chars];
        tfm.height_index = vec![0; num_chars];
        tfm.depth_index = vec![0; num_chars];
        for i in 0..num_chars {
            tfm.width_index[i] = u16::get(ofm_handle);
            tfm.height_index[i] = u8::get(ofm_handle);
            tfm.depth_index[i] = u8::get(ofm_handle);
            /* Ignore remaining quad */
            skip_bytes(4, ofm_handle);
        }
    };
}
fn ofm_do_char_info_one<R: Read>(ofm_handle: &mut R, tfm: &mut tfm_font) {
    let num_char_infos = tfm
        .ncw
        .wrapping_div((3_u32).wrapping_add(tfm.npc.wrapping_div(2_u32)));
    let num_chars = (tfm.ec - tfm.bc + 1) as usize;
    if num_chars != 0 {
        tfm.width_index = vec![0; num_chars];
        tfm.height_index = vec![0; num_chars];
        tfm.depth_index = vec![0; num_chars];
        let mut char_infos_read = 0;
        let mut i = 0;
        while i < num_chars && char_infos_read < num_char_infos {
            tfm.width_index[i] = u16::get(ofm_handle);
            tfm.height_index[i] = u8::get(ofm_handle);
            tfm.depth_index[i] = u8::get(ofm_handle);
            /* Ignore next quad */
            skip_bytes(4_u32, ofm_handle);
            let repeats = u16::get(ofm_handle) as usize;
            /* Skip params */
            for _ in 0..tfm.npc {
                u16::get(ofm_handle);
            }
            /* Remove word padding if necessary */
            if tfm.npc / 2 * 2 == tfm.npc {
                u16::get(ofm_handle);
            }
            char_infos_read += 1;
            if i + repeats > num_chars {
                panic!("OFM \"repeats\" causes number of characters to be exceeded.");
            }
            for j in 0..repeats {
                tfm.width_index[i + j + 1] = tfm.width_index[i];
                tfm.height_index[i + j + 1] = tfm.height_index[i];
                tfm.depth_index[i + j + 1] = tfm.depth_index[i];
            }
            /* Skip ahead because we have already handled repeats */
            i += repeats;
            i += 1;
        }
    };
}
fn ofm_unpack_arrays(fm: &mut font_metric, tfm: &tfm_font, num_chars: u32) {
    fm.widths = vec![0; (tfm.bc + num_chars) as usize];
    fm.heights = vec![0; (tfm.bc + num_chars) as usize];
    fm.depths = vec![0; (tfm.bc + num_chars) as usize];
    for i in 0..num_chars as usize {
        fm.widths[tfm.bc as usize + i] = tfm.width[tfm.width_index[i] as usize];
        fm.heights[tfm.bc as usize + i] = tfm.height[tfm.height_index[i] as usize];
        fm.depths[tfm.bc as usize + i] = tfm.depth[tfm.depth_index[i] as usize];
    }
}
fn read_ofm<R: Read + Seek>(ofm_handle: &mut R, ofm_file_size: off_t) -> font_metric {
    let mut tfm: tfm_font = tfm_font::default();
    ofm_get_sizes(ofm_handle, ofm_file_size, &mut tfm);
    if tfm.level < 0 || tfm.level > 1 {
        panic!("OFM level {} not supported.", tfm.level);
    }
    if tfm.wlenheader > 0 {
        tfm.header = vec![0; (tfm.wlenheader as usize) * 4];
        ofm_handle.read_exact(tfm.header.as_mut_slice()).unwrap();
    }
    if tfm.level == 0 {
        ofm_do_char_info_zero(ofm_handle, &mut tfm);
    } else if tfm.level == 1 {
        ofm_do_char_info_one(ofm_handle, &mut tfm);
    }
    if tfm.nwidths > 0 {
        tfm.width = vec![0; tfm.nwidths as usize];
        fread_fwords(tfm.width.as_mut_slice(), ofm_handle);
    }
    if tfm.nheights > 0 {
        tfm.height = vec![0; tfm.nheights as usize];
        fread_fwords(tfm.height.as_mut_slice(), ofm_handle);
    }
    if tfm.ndepths > 0 {
        tfm.depth = vec![0; tfm.ndepths as usize];
        fread_fwords(tfm.depth.as_mut_slice(), ofm_handle);
    }
    let num_chars = tfm.ec - tfm.bc + 1;

    let mut fm = font_metric::default();
    ofm_unpack_arrays(&mut fm, &mut tfm, num_chars);
    tfm_unpack_header(&mut fm, &mut tfm);
    fm.firstchar = tfm.bc as i32;
    fm.lastchar = tfm.ec as i32;
    fm.source = 2;
    fm
}
fn read_tfm<R: Read>(tfm_handle: &mut R, tfm_file_size: off_t) -> font_metric {
    let mut tfm: tfm_font = tfm_font::default();
    tfm_get_sizes(tfm_handle, tfm_file_size, &mut tfm);

    let mut fm = font_metric::default();
    fm.firstchar = tfm.bc as i32;
    fm.lastchar = tfm.ec as i32;
    if tfm.wlenheader > 0 {
        tfm.header = vec![0; (tfm.wlenheader * 4) as usize];
        tfm_handle.read_exact(tfm.header.as_mut_slice()).unwrap();
    }
    if tfm.ec.wrapping_sub(tfm.bc).wrapping_add(1_u32) > 0_u32 {
        tfm.char_info = vec![0; (tfm.ec - tfm.bc + 1) as usize];
        fread_uquads(tfm.char_info.as_mut_slice(), tfm_handle);
    }
    if tfm.nwidths > 0 {
        tfm.width = vec![0; tfm.nwidths as usize];
        fread_fwords(tfm.width.as_mut_slice(), tfm_handle);
    }
    if tfm.nheights > 0 {
        tfm.height = vec![0; tfm.nheights as usize];
        fread_fwords(tfm.height.as_mut_slice(), tfm_handle);
    }
    if tfm.ndepths > 0 {
        tfm.depth = vec![0; tfm.ndepths as usize];
        fread_fwords(tfm.depth.as_mut_slice(), tfm_handle);
    }
    tfm_unpack_arrays(&mut fm, &mut tfm);
    tfm_unpack_header(&mut fm, &mut tfm);
    fm
}

pub(crate) unsafe fn tfm_open(tfm_name: &str, must_exist: i32) -> i32 {
    let mut tfm_handle = None;
    let mut format: i32 = 1;
    for i in 0..fms.len() {
        if tfm_name == fms[i].tex_name {
            return i as i32;
        }
    }
    /* NOTE: the following comment is no longer operative with the switch to
     * the Tectonic I/O system since we don't have `must_exist`. The logic
     * of the current implementation might not be right; to be investigated.
     * Comment preserved for posterity.
     *
     * "The procedure to search tfm or ofm files:
     * 1. Search tfm file with the given name with the must_exist flag unset.
     * 2. Search ofm file with the given name with the must_exist flag unset.
     * 3. If not found and must_exist flag is set, try again to search
     *    tfm file with the must_exist flag set.
     * 4. If not found and must_exist flag is not set, return -1.
     *
     * We first look for OFM and then TFM.
     * The reason for this change is incompatibility introduced when dvipdfmx
     * started to write correct glyph metrics to output PDF for CID fonts.
     * I'll not explain this in detail... This change is mostly specific to
     * Japanese support."
     */
    let suffix = tfm_name.split(".").last();
    let ofm_name = match suffix {
        Some("tfm") | Some("ofm") => None,
        _ => Some(tfm_name.to_string() + ".ofm"),
    };
    if ofm_name.is_some() && {
        tfm_handle = InFile::open(&ofm_name.unwrap(), TTInputFormat::OFM, 0i32);
        tfm_handle.is_some()
    } {
        format = 2;
    } else {
        tfm_handle = InFile::open(tfm_name, TTInputFormat::TFM, 0i32);
        if tfm_handle.is_some() {
            format = 1;
        } else {
            tfm_handle = InFile::open(tfm_name, TTInputFormat::OFM, 0i32);
            if tfm_handle.is_some() {
                format = 2;
            }
        }
    }
    if tfm_handle.is_none() {
        if must_exist != 0 {
            panic!("Unable to find TFM file \"{}\".", tfm_name);
        }
        return -1i32;
    }
    let mut tfm_handle = tfm_handle.unwrap();
    if verbose != 0 {
        if format == 1i32 {
            info!("(TFM:{}", tfm_name);
        } else if format == 2 {
            info!("(OFM:{}", tfm_name);
        }
    }
    let tfm_file_size = ttstub_input_get_size(&mut tfm_handle) as off_t;
    if tfm_file_size as u64 > 0x1ffffffff {
        panic!("TFM/OFM file size exceeds 33-bit");
    }
    if tfm_file_size < 24 {
        panic!("TFM/OFM file too small to be a valid file.");
    }
    let mut fm = if format == 2 {
        read_ofm(&mut tfm_handle, tfm_file_size)
    } else {
        read_tfm(&mut tfm_handle, tfm_file_size)
    };
    fm.tex_name = tfm_name.to_string();
    if verbose != 0 {
        info!(")");
    }
    let last = fms.len();
    fms.push(fm);
    last as i32
}

pub(crate) unsafe fn tfm_close_all() {
    fms = Vec::new();
}

pub(crate) unsafe fn tfm_get_fw_width(font_id: i32, ch: i32) -> fixword {
    let idx;
    if font_id < 0 || font_id as usize >= fms.len() {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut fms[font_id as usize];
    if ch >= fm.firstchar && ch <= fm.lastchar {
        match &fm.charmap {
            CharMap::Char(data) => {
                idx = lookup_char(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::Range(data) => {
                idx = lookup_range(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::None => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    fm.widths[idx as usize]
}

pub(crate) unsafe fn tfm_get_fw_height(font_id: i32, ch: i32) -> fixword {
    let idx;
    if font_id < 0 || font_id as usize >= fms.len() {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut fms[font_id as usize];
    if ch >= fm.firstchar && ch <= fm.lastchar {
        match &fm.charmap {
            CharMap::Char(data) => {
                idx = lookup_char(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::Range(data) => {
                idx = lookup_range(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::None => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    fm.heights[idx as usize]
}

pub(crate) unsafe fn tfm_get_fw_depth(font_id: i32, ch: i32) -> fixword {
    let idx;
    if font_id < 0 || font_id as usize >= fms.len() {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    let fm = &mut fms[font_id as usize];
    if ch >= fm.firstchar && ch <= fm.lastchar {
        match &fm.charmap {
            CharMap::Char(data) => {
                idx = lookup_char(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::Range(data) => {
                idx = lookup_range(data, ch);
                if idx < 0 {
                    panic!("Invalid char: {}\n", ch);
                }
            }
            CharMap::None => idx = ch,
        }
    } else {
        panic!("Invalid char: {}\n", ch);
    }
    fm.depths[idx as usize]
}
/*
 * tfm_get_width returns the width of the font
 * as a (double) fraction of the design size.
 */

pub(crate) unsafe fn tfm_get_width(font_id: i32, ch: i32) -> f64 {
    tfm_get_fw_width(font_id, ch) as f64 / (1i32 << 20i32) as f64
}
/* tfm_string_xxx() do not work for OFM... */

pub(crate) unsafe fn tfm_string_width(font_id: i32, s: &[u8]) -> fixword {
    let mut result: fixword = 0i32;
    if font_id < 0i32 || font_id as usize >= fms.len() {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    for &i in s.iter() {
        result += tfm_get_fw_width(font_id, i as i32);
    }
    result
}

pub(crate) unsafe fn tfm_get_design_size(font_id: i32) -> f64 {
    if font_id < 0i32 || font_id as usize >= fms.len() {
        panic!("TFM: Invalid TFM ID: {}", font_id);
    }
    return fms[font_id as usize].designsize as f64 / (1i32 << 20i32) as f64 * (72.0f64 / 72.27f64);
}
/* From TFM header */

pub(crate) unsafe fn tfm_exists(tfm_name: &str) -> bool {
    if tfm_name.is_empty() {
        return false;
    }
    if InFile::open(tfm_name, TTInputFormat::OFM, 0).is_some() {
        return true;
    }
    if InFile::open(tfm_name, TTInputFormat::TFM, 0).is_some() {
        return true;
    }
    false
}
