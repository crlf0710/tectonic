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

use crate::bridge::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_sfnt::{
    dfont_open, sfnt_close, sfnt_create_FontFile_stream, sfnt_find_table_pos, sfnt_open,
    sfnt_read_table_directory, sfnt_require_table,
};
use crate::dpx_truetype::SfntTableInfo;
use crate::{info, warn, FromBEByteSlice};

use super::dpx_cid::{CIDFont_get_embedding, CIDFont_get_parent_id, CIDFont_is_BaseFont};
use super::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode_char};
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_truetype_file};
use super::dpx_pdffont::pdf_font_make_uniqueTag;
use super::dpx_tt_aux::tt_get_fontdesc;
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_cmap::{tt_cmap_lookup, tt_cmap_read, tt_cmap_release};
use super::dpx_tt_glyf::{
    tt_add_glyph, tt_build_finish, tt_build_init, tt_build_tables, tt_get_index, tt_get_metrics,
};
use super::dpx_tt_gsub::{
    otl_gsub_add_feat, otl_gsub_apply, otl_gsub_new, otl_gsub_release, otl_gsub_select,
};
use super::dpx_tt_table::tt_get_ps_fontname;
use super::dpx_type0::{Type0Font_cache_get, Type0Font_get_usedchars};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_name, pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_stream, IntoObj, PushObj,
    STREAM_COMPRESS,
};
use libc::free;

use crate::bridge::size_t;

use super::dpx_cid::{cid_opt, CIDFont, CIDSysInfo};

use super::dpx_cmap::CMap;
use super::dpx_tt_cmap::tt_cmap;

/* 2 for CID, variable for Code..  */
/* CID (as 16-bit BE), Code ...    */
/* Next Subtbl for LOOKUP_CONTINUE */

pub(crate) type CID = u16;
/*
 * PDF viewer applications use following tables (CIDFontType 2)
 *
 *  head, hhea, loca, maxp, glyf, hmtx, fpgm, cvt_, prep
 *
 *                                         - from PDF Ref. v.1.3, 2nd ed.
 *
 * The fpgm, cvt_, and prep tables appears only when TrueType instructions
 * requires them. Those tables must be preserved if they exist.
 * We use must_exist flag to indicate `preserve it if present'
 * and to make sure not to cause an error when it does not exist.
 *
 * post and name table must exist in ordinary TrueType font file,
 * but when a TrueType font is converted to CIDFontType 2 font, those tables
 * are no longer required.
 *
 * The OS/2 table (required for TrueType font for Windows and OS/2) contains
 * liscencing information, but PDF viewers seems not using them.
 *
 * The 'name' table added. See comments in ttf.c.
 */

use super::dpx_tt_glyf::tt_glyphs;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_2 {
    pub(crate) alt1: u16,
    pub(crate) alt2: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_3 {
    pub(crate) platform: u16,
    pub(crate) encoding: u16,
    pub(crate) pdfnames: [*const i8; 5],
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/*
 * TrueType glyf table is sorted by CID and no CIDToGIDMap is used here.
 * GhostScript can't handle CIDToGIDMap correctly.
 */
/* pseudo unique tag */
/* CID font */
/* TrueType */
static mut verbose: i32 = 0i32;
static mut opt_flags: i32 = 0i32;

pub(crate) unsafe fn CIDFont_type2_set_verbose(level: i32) {
    verbose = level;
}

pub(crate) unsafe fn CIDFont_type2_set_flags(flags: i32) {
    opt_flags = flags;
}

const required_table: [SfntTableInfo; 11] = {
    use crate::dpx_truetype::sfnt_table_info::*;
    [
        SfntTableInfo::new(OS_2, false),
        SfntTableInfo::new(HEAD, true),
        SfntTableInfo::new(HHEA, true),
        SfntTableInfo::new(LOCA, true),
        SfntTableInfo::new(MAXP, true),
        SfntTableInfo::new(NAME, true),
        SfntTableInfo::new(GLYF, true),
        SfntTableInfo::new(HMTX, true),
        SfntTableInfo::new(FPGM, false),
        SfntTableInfo::new(CVT, false),
        SfntTableInfo::new(PREP, false),
    ]
};
unsafe fn validate_name(mut fontname: String) -> String {
    const badstrlist: [&str; 4] = [
        "-WIN-RKSJ-H",
        "-WINP-RKSJ-H",
        "-WING-RKSJ-H",
        "-90pv-RKSJ-H",
    ];
    /* For some fonts that have bad PS name. ad hoc. remove me. */
    for badstr in &badstrlist {
        if fontname.ends_with(badstr) {
            warn!(
                "Removed string \"{}\" from fontname \"{}\".",
                badstr, fontname
            );
            fontname = fontname.replace(badstr, "");
        }
    }
    if fontname.is_empty() {
        panic!("No valid character found in fontname string.");
    };
    fontname
}
static mut known_encodings: [C2RustUnnamed_3; 11] = [
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 10_u16,
        pdfnames: [
            b"UCSms-UCS4\x00" as *const u8 as *const i8,
            b"UCSms-UCS2\x00" as *const u8 as *const i8,
            b"UCS4\x00" as *const u8 as *const i8,
            b"UCS2\x00" as *const u8 as *const i8,
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 1_u16,
        pdfnames: [
            b"UCSms-UCS4\x00" as *const u8 as *const i8,
            b"UCSms-UCS2\x00" as *const u8 as *const i8,
            b"UCS4\x00" as *const u8 as *const i8,
            b"UCS2\x00" as *const u8 as *const i8,
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 2_u16,
        pdfnames: [
            b"90ms-RKSJ\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 3_u16,
        pdfnames: [
            b"GBK-EUC\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 4_u16,
        pdfnames: [
            b"ETen-B5\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 3_u16,
        encoding: 5_u16,
        pdfnames: [
            b"KSCms-UHC\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 1_u16,
        encoding: 1_u16,
        pdfnames: [
            b"90pv-RKSJ\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 1_u16,
        encoding: 2_u16,
        pdfnames: [
            b"B5pc\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 1_u16,
        encoding: 25_u16,
        pdfnames: [
            b"GBpc-EUC\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 1_u16,
        encoding: 3_u16,
        pdfnames: [
            b"KSCpc-EUC\x00" as *const u8 as *const i8,
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
    C2RustUnnamed_3 {
        platform: 0_u16,
        encoding: 0_u16,
        pdfnames: [
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
            ptr::null(),
        ],
    },
];
unsafe fn find_tocode_cmap(reg: &str, ord: &str, select: i32) -> *mut CMap {
    let mut cmap_id: i32 = -1i32;
    if select < 0i32 || select > 9i32 {
        panic!("Character set unknown.");
    }
    if ord == "UCS" && select <= 1i32 {
        return ptr::null_mut();
    }
    let mut i = 0;
    while cmap_id < 0i32 && i < 5i32 {
        let append = known_encodings[select as usize].pdfnames[i as usize];
        if append.is_null() {
            break;
        }
        let cmap_name = format!("{}-{}-{}", reg, ord, CStr::from_ptr(append).display());
        cmap_id = CMap_cache_find(&cmap_name);
        i += 1
    }
    if cmap_id < 0i32 {
        warn!(
            "Could not find CID-to-Code mapping for \"{}-{}\".",
            reg, ord,
        );
        warn!("I tried to load (one of) the following file(s):");
        for i in 0..5 {
            let append = known_encodings[select as usize].pdfnames[i];
            if append.is_null() {
                break;
            }
            info!(" {}-{}-{}", reg, ord, CStr::from_ptr(append).display());
        }
        warn!("Please check if this file exists.");
        panic!("Cannot continue...");
    }
    CMap_cache_get(cmap_id)
}
/*
 * CIDFont glyph metrics:
 * Mostly same as add_CID[HV]Metrics in cidtype0.c.
 */
unsafe fn add_TTCIDHMetrics(
    fontdict: *mut pdf_obj,
    g: *mut tt_glyphs,
    used_chars: *mut i8,
    cidtogidmap: *mut u8,
    last_cid: u16,
) {
    let mut start: i32 = 0i32;
    let mut prev: i32 = 0i32;
    let mut an_array = None;
    let mut empty: i32 = 1i32;
    let mut w_array = vec![];
    let dw = if (*g).dw as i32 != 0i32 && (*g).dw as i32 <= (*g).emsize as i32 {
        (1000.0f64 * (*g).dw as i32 as f64 / (*g).emsize as i32 as f64 / 1i32 as f64 + 0.5f64)
            .floor()
            * 1i32 as f64
    } else {
        (1000.0f64 * (*(*g).gd.offset(0)).advw as i32 as f64
            / (*g).emsize as i32 as f64
            / 1i32 as f64
            + 0.5f64)
            .floor()
            * 1i32 as f64
    };
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 == 0) {
            let gid = (if !cidtogidmap.is_null() {
                (*cidtogidmap.offset((2i32 * cid) as isize) as i32) << 8i32
                    | *cidtogidmap.offset((2i32 * cid + 1i32) as isize) as i32
            } else {
                cid
            }) as u16;
            let idx = tt_get_index(g, gid);
            if !(cid != 0i32 && idx as i32 == 0i32) {
                let width = (1000.0f64 * (*(*g).gd.offset(idx as isize)).advw as i32 as f64
                    / (*g).emsize as i32 as f64
                    / 1i32 as f64
                    + 0.5f64)
                    .floor()
                    * 1i32 as f64;
                if width == dw {
                    if an_array.is_some() {
                        w_array.push_obj(start as f64);
                        w_array.push_obj(an_array.take().unwrap());
                        empty = 0i32
                    }
                } else {
                    if cid != prev + 1i32 {
                        if an_array.is_some() {
                            w_array.push_obj(start as f64);
                            w_array.push_obj(an_array.take().unwrap());
                            empty = 0i32
                        }
                    }
                    if an_array.is_none() {
                        an_array = Some(Vec::new());
                        start = cid
                    }
                    an_array.as_mut().unwrap().push_obj(width);
                    prev = cid
                }
            }
        }
    }
    if an_array.is_some() {
        w_array.push_obj(start as f64);
        w_array.push_obj(an_array.take().unwrap());
        empty = 0i32
    }
    (*fontdict).as_dict_mut().set("DW", dw);
    let w_array = w_array.into_obj();
    if empty == 0 {
        (*fontdict).as_dict_mut().set("W", pdf_ref_obj(w_array));
    }
    pdf_release_obj(w_array);
}
unsafe fn add_TTCIDVMetrics(
    fontdict: *mut pdf_obj,
    g: *mut tt_glyphs,
    used_chars: *mut i8,
    last_cid: u16,
) {
    let mut empty: i32 = 1i32;
    let defaultVertOriginY = (1000.0f64
        * ((*g).default_advh as i32 - (*g).default_tsb as i32) as f64
        / (*g).emsize as i32 as f64
        / 1i32 as f64
        + 0.5f64)
        .floor()
        * 1i32 as f64;
    let defaultAdvanceHeight =
        (1000.0f64 * (*g).default_advh as i32 as f64 / (*g).emsize as i32 as f64 / 1i32 as f64
            + 0.5f64)
            .floor()
            * 1i32 as f64;
    let mut w2_array = vec![];
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 == 0) {
            let idx = tt_get_index(g, cid as u16);
            if !(cid != 0i32 && idx as i32 == 0i32) {
                let advanceHeight = (1000.0f64
                    * (*(*g).gd.offset(idx as isize)).advh as i32 as f64
                    / (*g).emsize as i32 as f64
                    / 1i32 as f64
                    + 0.5f64)
                    .floor()
                    * 1i32 as f64;
                let vertOriginX = (1000.0f64
                    * (0.5f64 * (*(*g).gd.offset(idx as isize)).advw as i32 as f64)
                    / (*g).emsize as i32 as f64
                    / 1i32 as f64
                    + 0.5f64)
                    .floor()
                    * 1i32 as f64;
                let vertOriginY = (1000.0f64
                    * ((*(*g).gd.offset(idx as isize)).tsb as i32
                        + (*(*g).gd.offset(idx as isize)).ury as i32) as f64
                    / (*g).emsize as i32 as f64
                    / 1i32 as f64
                    + 0.5f64)
                    .floor()
                    * 1i32 as f64;
                /*
                 * c_first c_last w1_y v_x v_y
                 * This form may hit Acrobat's implementation limit of array element size,
                 * 8192. AFPL GhostScript 8.11 stops with rangecheck error with this.
                 * Maybe GS's bug?
                 */
                if vertOriginY != defaultVertOriginY || advanceHeight != defaultAdvanceHeight {
                    w2_array.push_obj(cid as f64);
                    w2_array.push_obj(cid as f64);
                    w2_array.push_obj(-advanceHeight);
                    w2_array.push_obj(vertOriginX);
                    w2_array.push_obj(vertOriginY);
                    empty = 0i32
                }
            }
        }
    }
    if defaultVertOriginY != 880i32 as f64 || defaultAdvanceHeight != 1000i32 as f64 {
        let mut an_array = vec![];
        an_array.push_obj(defaultVertOriginY);
        an_array.push_obj(-defaultAdvanceHeight);
        (*fontdict).as_dict_mut().set("DW2", an_array);
    }
    let w2_array = w2_array.into_obj();
    if empty == 0 {
        (*fontdict).as_dict_mut().set("W2", pdf_ref_obj(w2_array));
    }
    pdf_release_obj(w2_array);
}
/*
 * The following routine fixes few problems caused by vendor specific
 * Unicode mappings.
 */
unsafe fn fix_CJK_symbols(code: u16) -> u16 {
    static mut CJK_Uni_symbols: [C2RustUnnamed_2; 10] = [
        C2RustUnnamed_2 {
            alt1: 0x2014_u16,
            alt2: 0x2015_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0x2016_u16,
            alt2: 0x2225_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0x203e_u16,
            alt2: 0xffe3_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0x2026_u16,
            alt2: 0x22ef_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0x2212_u16,
            alt2: 0xff0d_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0x301c_u16,
            alt2: 0xff5e_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe0_u16,
            alt2: 0xa2_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe1_u16,
            alt2: 0xa3_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe2_u16,
            alt2: 0xac_u16,
        },
        C2RustUnnamed_2 {
            alt1: 0xffff_u16,
            alt2: 0xffff_u16,
        },
    ];
    let mut alt_code = code;
    for i in 0..(::std::mem::size_of::<[C2RustUnnamed_2; 10]>() as u64)
        .wrapping_div(::std::mem::size_of::<C2RustUnnamed_2>() as u64) as usize
    {
        if CJK_Uni_symbols[i].alt1 as i32 == code as i32 {
            alt_code = CJK_Uni_symbols[i].alt2;
            break;
        } else if CJK_Uni_symbols[i].alt2 as i32 == code as i32 {
            alt_code = CJK_Uni_symbols[i].alt1;
            break;
        }
    }
    alt_code
}
unsafe fn cid_to_code(cmap: *mut CMap, cid: CID) -> i32 {
    let mut outbuf: [u8; 32] = [0; 32];
    let mut inbytesleft: size_t = 2i32 as size_t;
    let mut outbytesleft: size_t = 32i32 as size_t;
    if cmap.is_null() {
        return cid as i32;
    }
    let mut inbuf = cid.to_be_bytes().as_ptr();
    let mut q = outbuf.as_mut_ptr();
    CMap_decode_char(
        cmap,
        &mut inbuf,
        &mut inbytesleft,
        &mut q,
        &mut outbytesleft,
    );
    if inbytesleft != 0 {
        return 0i32;
    } else {
        if outbytesleft == 31 {
            return outbuf[0] as i32;
        } else {
            if outbytesleft == 30 {
                return (outbuf[0] as i32) << 8i32 | outbuf[1] as i32;
            } else {
                if outbytesleft == 28 {
                    /* We assume the output encoding is UTF-16. */
                    let hi: CID = u16::from_be_byte_slice(&outbuf[0..2]);
                    let lo: CID = u16::from_be_byte_slice(&outbuf[2..4]);
                    if hi as i32 >= 0xd800i32
                        && hi as i32 <= 0xdbffi32
                        && lo as i32 >= 0xdc00i32
                        && lo as i32 <= 0xdfffi32
                    {
                        return (hi as i32 - 0xd800i32) * 0x400i32 + 0x10000i32 + lo as i32
                            - 0xdc00i32;
                    } else {
                        return (hi as i32) << 16i32 | lo as i32;
                    }
                }
            }
        }
    }
    0i32
}
/* #define NO_GHOSTSCRIPT_BUG 1 */

pub(crate) unsafe fn CIDFont_type2_dofont(font: *mut CIDFont) {
    let cmap;
    let mut ttcmap: *mut tt_cmap = ptr::null_mut();
    let offset;
    let mut i: i32 = 0;
    let mut unicode_cmap: i32 = 0i32;
    if (*font).indirect.is_null() {
        return;
    }
    (*(*font).fontdict)
        .as_dict_mut()
        .set("FontDescriptor", pdf_ref_obj((*font).descriptor));
    if CIDFont_is_BaseFont(font) {
        return;
    }
    /*
     * CIDSystemInfo comes here since Supplement can be increased.
     */
    let mut tmp = pdf_dict::new();
    tmp.set("Registry", (*(*font).csi).registry.into_obj());
    tmp.set("Ordering", (*(*font).csi).ordering.into_obj());
    tmp.set("Supplement", (*(*font).csi).supplement as f64);
    (*(*font).fontdict).as_dict_mut().set("CIDSystemInfo", tmp);
    /* Quick exit for non-embedded & fixed-pitch font. */
    if CIDFont_get_embedding(font) == 0 && opt_flags & 1i32 << 1i32 != 0 {
        (*(*font).fontdict).as_dict_mut().set("DW", 1000_f64);
        return;
    }
    let sfont = if let Some(handle) = dpx_open_truetype_file(&(*font).ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&(*font).ident) {
        dfont_open(handle, (*(*font).options).index)
    } else {
        panic!("Could not open TTF/dfont file: {}", (*font).ident);
    };
    if sfont.is_null() {
        panic!("Could not open TTF file: {}", (*font).ident);
    }
    match (*sfont).type_0 {
        16 => {
            offset = ttc_read_offset(sfont, (*(*font).options).index);
            if offset == 0_u32 {
                panic!("Invalid TTC index in {}.", (*font).ident);
            }
        }
        1 => {
            if (*(*font).options).index > 0i32 {
                panic!(
                    "Found TrueType font file while expecting TTC file ({}).",
                    (*font).ident
                );
            }
            offset = 0_u32
        }
        256 => offset = (*sfont).offset,
        _ => {
            panic!("Not a TrueType/TTC font ({})?", (*font).ident);
        }
    }
    if sfnt_read_table_directory(sfont, offset) < 0i32 {
        panic!(
            "Could not read TrueType table directory ({}).",
            (*font).ident
        );
    }
    /*
     * Adobe-Identity means font's internal glyph ordering here.
     */
    let glyph_ordering =
        (*(*font).csi).registry == "Adobe" && (*(*font).csi).ordering == "Identity";
    /*
     * Select TrueType cmap table, find ToCode CMap for each TrueType encodings.
     */
    if glyph_ordering {
        ttcmap = ptr::null_mut();
        cmap = ptr::null_mut()
    } else {
        /*
         * This part contains a bug. It may choose SJIS encoding TrueType cmap
         * table for Adobe-GB1.
         */
        for i in 0..=9 {
            ttcmap = tt_cmap_read(
                sfont,
                known_encodings[i].platform,
                known_encodings[i].encoding,
            );
            if !ttcmap.is_null() {
                break;
            }
        }
        if ttcmap.is_null() {
            warn!(
                "No usable TrueType cmap table found for font \"{}\".",
                (*font).ident
            );
            warn!(
                "CID character collection for this font is set to \"{}-{}\"",
                (*(*font).csi).registry,
                (*(*font).csi).ordering,
            );
            panic!("Cannot continue without this...");
        } else {
            if i <= 1i32 {
                unicode_cmap = 1i32
            } else {
                unicode_cmap = 0i32
            }
        }
        /*
         * NULL is returned if CMap is Identity CMap.
         */
        cmap = find_tocode_cmap(&(*(*font).csi).registry, &(*(*font).csi).ordering, i)
    } /* .notdef */
    let glyphs = tt_build_init();
    let mut last_cid = 0i32 as CID;
    let mut num_glyphs = 1_u16;
    let mut v_used_chars = ptr::null_mut();
    let mut h_used_chars = v_used_chars;
    let mut used_chars = h_used_chars;
    let mut parent;
    let parent_id = CIDFont_get_parent_id(font, 0i32);
    if parent_id >= 0i32 {
        parent = Type0Font_cache_get(parent_id);
        h_used_chars = Type0Font_get_usedchars(parent)
    }
    let parent_id = CIDFont_get_parent_id(font, 1i32);
    if parent_id >= 0i32 {
        parent = Type0Font_cache_get(parent_id);
        v_used_chars = Type0Font_get_usedchars(parent)
    }
    if h_used_chars.is_null() && v_used_chars.is_null() {
        panic!("Unexpected error.");
    }
    /*
     * Quick check of max CID.
     */
    let mut c = 0;
    i = 8191i32;
    while i >= 0i32 {
        if !h_used_chars.is_null() && *h_used_chars.offset(i as isize) as i32 != 0i32 {
            last_cid = (i * 8i32 + 7i32) as CID;
            c = *h_used_chars.offset(i as isize) as i32;
            break;
        } else {
            i -= 1
        }
    }
    i = 8191i32;
    while i >= 0i32 {
        if !v_used_chars.is_null() && *v_used_chars.offset(i as isize) as i32 != 0i32 {
            if i * 8i32 + 7i32 >= last_cid as i32 {
                c = if i * 8i32 + 7i32 > last_cid as i32 {
                    *v_used_chars.offset(i as isize) as i32
                } else {
                    c | *v_used_chars.offset(i as isize) as i32
                };
                last_cid = (i * 8i32 + 7i32) as CID;
                break;
            }
        }
        i -= 1
    }
    if last_cid as i32 > 0i32 {
        for i in 0..8 {
            if c >> i & 1i32 != 0 {
                break;
            }
            last_cid = last_cid.wrapping_sub(1);
        }
    }
    if last_cid as u32 >= 0xffffu32 {
        panic!("CID count > 65535");
    }
    let cidtogidmap = ptr::null_mut();
    /* !NO_GHOSTSCRIPT_BUG */
    /*
     * Map CIDs to GIDs.
     * Horizontal and vertical used_chars are merged.
     */
    /*
     * Horizontal
     */
    if !h_used_chars.is_null() {
        used_chars = h_used_chars;
        for cid in 1..=last_cid as CID {
            let code;
            let mut gid;
            if !(*h_used_chars.offset((cid as i32 / 8i32) as isize) as i32
                & 1i32 << 7i32 - cid as i32 % 8i32
                == 0)
            {
                if glyph_ordering {
                    gid = cid;
                    code = cid as i32
                } else {
                    code = cid_to_code(cmap, cid);
                    gid = tt_cmap_lookup(ttcmap, code as u32);
                    if gid as i32 == 0i32 && unicode_cmap != 0 {
                        let alt_code = fix_CJK_symbols(code as u16) as i32;
                        if alt_code != code {
                            gid = tt_cmap_lookup(ttcmap, alt_code as u32);
                            if gid as i32 != 0i32 {
                                warn!(
                                    "Unicode char U+{:04x} replaced with U+{:04x}.",
                                    code, alt_code,
                                );
                            }
                        }
                    }
                    /* FIX_CJK_UNIOCDE_SYMBOLS */
                }
                if gid as i32 == 0i32 {
                    warn!("Glyph missing in font. (CID={}, code=0x{:04x})", cid, code,);
                }
                /* TODO: duplicated glyph */
                tt_add_glyph(glyphs, gid, cid);
                /* !NO_GHOSTSCRIPT_BUG */
                num_glyphs = num_glyphs.wrapping_add(1)
            }
        }
    }
    /*
     * Vertical
     */
    if !v_used_chars.is_null() {
        let mut gsub_list;
        /*
         * Require `vrt2' or `vert'.
         */
        if glyph_ordering {
            gsub_list = ptr::null_mut()
        } else {
            gsub_list = otl_gsub_new();
            if otl_gsub_add_feat(gsub_list, b"*", b"*", b"vrt2", sfont) < 0i32 {
                if otl_gsub_add_feat(gsub_list, b"*", b"*", b"vert", sfont) < 0i32 {
                    warn!("GSUB feature vrt2/vert not found.");
                    otl_gsub_release(gsub_list);
                    gsub_list = ptr::null_mut()
                } else {
                    otl_gsub_select(gsub_list, b"*", b"*", b"vert");
                }
            } else {
                otl_gsub_select(gsub_list, b"*", b"*", b"vrt2");
            }
        }
        for cid in 1..=last_cid as CID {
            let code_0;
            let mut gid_0;
            if !(*v_used_chars.offset((cid as i32 / 8i32) as isize) as i32
                & 1i32 << 7i32 - cid as i32 % 8i32
                == 0)
            {
                /* There may be conflict of horizontal and vertical glyphs
                 * when font is used with /UCS. However, we simply ignore
                 * that...
                 */
                if !(!h_used_chars.is_null()
                    && *h_used_chars.offset((cid as i32 / 8i32) as isize) as i32
                        & 1i32 << 7i32 - cid as i32 % 8i32
                        != 0)
                {
                    if glyph_ordering {
                        gid_0 = cid;
                        code_0 = cid as i32
                    } else {
                        code_0 = cid_to_code(cmap, cid);
                        gid_0 = tt_cmap_lookup(ttcmap, code_0 as u32);
                        if gid_0 as i32 == 0i32 && unicode_cmap != 0 {
                            let alt_code_0 = fix_CJK_symbols(code_0 as u16) as i32;
                            if alt_code_0 != code_0 {
                                gid_0 = tt_cmap_lookup(ttcmap, alt_code_0 as u32);
                                if gid_0 as i32 != 0i32 {
                                    warn!(
                                        "Unicode char U+{:04x} replaced with U+{:04x}.",
                                        code_0, alt_code_0,
                                    );
                                }
                            }
                        }
                        /* FIX_CJK_UNIOCDE_SYMBOLS */
                    }
                    if gid_0 as i32 == 0i32 {
                        warn!(
                            "Glyph missing in font. (CID={}, code=0x{:04x})",
                            cid, code_0,
                        );
                    } else if !gsub_list.is_null() {
                        otl_gsub_apply(gsub_list, &mut gid_0);
                    }
                    tt_add_glyph(glyphs, gid_0, cid);
                    /* !NO_GHOSTSCRIPT_BUG */
                    if !used_chars.is_null() {
                        /* merge vertical used_chars to horizontal */
                        let ref mut fresh0 = *used_chars.offset((cid as i32 / 8i32) as isize);
                        *fresh0 = (*fresh0 as i32 | 1i32 << 7i32 - cid as i32 % 8i32) as i8
                    }
                    num_glyphs = num_glyphs.wrapping_add(1)
                }
            }
        }
        if !gsub_list.is_null() {
            otl_gsub_release(gsub_list);
        }
        if used_chars.is_null() {
            /* We have no horizontal. */
            used_chars = v_used_chars
        }
    }
    if used_chars.is_null() {
        panic!("Unexpected error.");
    }
    tt_cmap_release(ttcmap);
    if CIDFont_get_embedding(font) != 0 {
        if tt_build_tables(sfont, glyphs) < 0i32 {
            panic!("Could not created FontFile stream.");
        }
        if verbose > 1i32 {
            info!("[{} glyphs (Max CID: {})]", (*glyphs).num_glyphs, last_cid);
        }
    } else if tt_get_metrics(sfont, glyphs) < 0i32 {
        panic!("Reading glyph metrics failed...");
    }
    /*
     * DW, W, DW2, and W2
     */
    if opt_flags & 1i32 << 1i32 != 0 {
        (*(*font).fontdict).as_dict_mut().set("DW", 1000_f64);
    } else {
        add_TTCIDHMetrics((*font).fontdict, glyphs, used_chars, cidtogidmap, last_cid);
        if !v_used_chars.is_null() {
            add_TTCIDVMetrics((*font).fontdict, glyphs, used_chars, last_cid);
        }
    }
    tt_build_finish(glyphs);
    /* Finish here if not embedded. */
    if CIDFont_get_embedding(font) == 0 {
        free(cidtogidmap as *mut libc::c_void);
        sfnt_close(sfont);
        return;
    }
    /* Create font file */
    for table in &required_table {
        if sfnt_require_table(sfont.as_mut().unwrap(), table).is_err() {
            panic!(
                "Some required TrueType table ({}) does not exist.",
                table.name_str()
            );
        }
    }
    /*
     * FontFile2
     */
    let fontfile = sfnt_create_FontFile_stream(sfont);
    sfnt_close(sfont);
    if verbose > 1i32 {
        info!("[{} bytes]", fontfile.len());
    }
    let fontfile = fontfile.into_obj();
    (*(*font).descriptor)
        .as_dict_mut()
        .set("FontFile2", pdf_ref_obj(fontfile));
    pdf_release_obj(fontfile);
    /*
     * CIDSet
     */
    let mut cidset = pdf_stream::new(STREAM_COMPRESS);
    cidset.add(
        used_chars as *const libc::c_void,
        last_cid as i32 / 8i32 + 1i32,
    );
    let cidset = cidset.into_obj();
    (*(*font).descriptor)
        .as_dict_mut()
        .set("CIDSet", pdf_ref_obj(cidset));
    pdf_release_obj(cidset);
    /*
     * CIDToGIDMap
     * Adobe's PDF Reference had been describing it as "optional" and
     * default value as "Identity". However, ISO 32000-1 requires it
     * for Type 2 CIDFonts with embedded font programs.
     */
    if cidtogidmap.is_null() {
        (*(*font).fontdict)
            .as_dict_mut()
            .set("CIDToGIDMap", "Identity");
    } else {
        let mut c2gmstream = pdf_stream::new(STREAM_COMPRESS);
        c2gmstream.add(
            cidtogidmap as *const libc::c_void,
            (last_cid as i32 + 1i32) * 2i32,
        );
        let c2gmstream = c2gmstream.into_obj();
        (*(*font).fontdict)
            .as_dict_mut()
            .set("CIDToGIDMap", pdf_ref_obj(c2gmstream));
        pdf_release_obj(c2gmstream);
        free(cidtogidmap as *mut libc::c_void);
    };
}

pub(crate) unsafe fn CIDFont_type2_open(
    mut font: &mut CIDFont,
    name: &str,
    cmap_csi: *mut CIDSysInfo,
    mut opt: *mut cid_opt,
) -> i32 {
    let offset;
    assert!(!opt.is_null());

    let sfont = if let Some(handle) = dpx_open_truetype_file(name) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(name) {
        dfont_open(handle, (*opt).index)
    } else {
        return -1i32;
    };
    if sfont.is_null() {
        return -1i32;
    }
    match (*sfont).type_0 {
        16 => offset = ttc_read_offset(sfont, (*opt).index),
        1 => {
            if (*opt).index > 0i32 {
                panic!("Invalid TTC index (not TTC font): {}", name);
            }
            offset = 0_u32
        }
        256 => offset = (*sfont).offset,
        _ => {
            sfnt_close(sfont);
            return -1i32;
        }
    }
    if sfnt_read_table_directory(sfont, offset) < 0i32 {
        panic!("Reading TrueType table directory failed.");
    }
    /* Ignore TrueType Collection with CFF table. */
    if (*sfont).type_0 == 1i32 << 4i32 && sfnt_find_table_pos(sfont, b"CFF ") != 0 {
        sfnt_close(sfont);
        return -1i32;
    }
    /* MAC-ROMAN-EN-POSTSCRIPT or WIN-UNICODE-EN(US)-POSTSCRIPT */
    /* for SJIS, UTF-16, ... string */
    let shortname = tt_get_ps_fontname(sfont).unwrap_or_else(|| name.to_owned());
    let shortname = validate_name(shortname);

    if (*opt).embed != 0 && (*opt).style != 0i32 {
        warn!("Embedding disabled due to style option for {}.", name);
        (*opt).embed = 0i32
    }

    let mut fontname = shortname
        + match (*opt).style {
            1 => ",Bold",
            2 => ",Italic",
            3 => ",BoldItalic",
            _ => "",
        };
    /*
     * CIDSystemInfo is determined from CMap or from map record option.
     */
    (*font).subtype = 2i32;
    (*font).csi = Box::into_raw(Box::new(CIDSysInfo {
        ordering: "".into(),
        registry: "".into(),
        supplement: 0,
    }));
    if !(*opt).csi.is_null() {
        if !cmap_csi.is_null() {
            if (*(*opt).csi).registry != (*cmap_csi).registry
                || (*(*opt).csi).ordering != (*cmap_csi).ordering
            {
                warn!("CID character collection mismatched:\n");
                info!(
                    "\tFont: {}-{}-{}\n",
                    (*(*opt).csi).registry,
                    (*(*opt).csi).ordering,
                    (*(*opt).csi).supplement,
                );
                info!(
                    "\tCMap: {}-{}-{}\n",
                    (*cmap_csi).registry,
                    (*cmap_csi).ordering,
                    (*cmap_csi).supplement,
                );
                panic!("Incompatible CMap specified for this font.");
            }
            if (*(*opt).csi).supplement < (*cmap_csi).supplement {
                warn!("Supplmement value in CIDSystemInfo increased.");
                warn!("Some characters may not shown.");
                (*(*opt).csi).supplement = (*cmap_csi).supplement
            }
        }
        (*(*font).csi).registry = (*(*opt).csi).registry.clone();
        (*(*font).csi).ordering = (*(*opt).csi).ordering.clone();
        (*(*font).csi).supplement = (*(*opt).csi).supplement
    } else if !cmap_csi.is_null() {
        (*(*font).csi).registry = (*cmap_csi).registry.clone();
        (*(*font).csi).ordering = (*cmap_csi).ordering.clone();
        (*(*font).csi).supplement = (*cmap_csi).supplement
    } else {
        (*(*font).csi).registry = "Adobe".into();
        (*(*font).csi).ordering = "Identity".into();
        (*(*font).csi).supplement = 0i32
    }
    (*font).fontdict = pdf_dict::new().into_obj();
    (*(*font).fontdict).as_dict_mut().set("Type", "Font");
    (*(*font).fontdict)
        .as_dict_mut()
        .set("Subtype", "CIDFontType2");
    if let Some(descriptor) = tt_get_fontdesc(sfont, &mut (*opt).embed, (*opt).stemv, 0i32, name) {
        (*font).descriptor = descriptor.into_obj();
    } else {
        panic!("Could not obtain necessary font info.");
    }
    if (*opt).embed != 0 {
        let tag = pdf_font_make_uniqueTag();
        fontname = format!("{}+{}", tag, fontname);
    }
    (*(*font).descriptor)
        .as_dict_mut()
        .set("FontName", pdf_name::new(fontname.as_bytes()));
    (*(*font).fontdict)
        .as_dict_mut()
        .set("BaseFont", pdf_name::new(fontname.as_bytes()));

    (*font).fontname = fontname.clone();

    sfnt_close(sfont);
    /*
     * Don't write fontdict here.
     * /Supplement in /CIDSystemInfo may change.
     */
    0i32
}
