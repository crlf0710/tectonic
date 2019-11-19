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
use std::ffi::CStr;
use std::ptr;

use super::dpx_sfnt::{
    dfont_open, sfnt_close, sfnt_create_FontFile_stream, sfnt_find_table_pos, sfnt_open,
    sfnt_read_table_directory, sfnt_require_table,
};
use crate::dpx_truetype::SfntTableInfo;
use crate::streq_ptr;
use crate::{info, warn, FromBEByteSlice};

use super::dpx_cid::{CIDFont_get_embedding, CIDFont_get_parent_id, CIDFont_is_BaseFont};
use super::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode_char};
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_truetype_file};
use super::dpx_mem::new;
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
    pdf_add_array, pdf_add_dict, pdf_add_stream, pdf_copy_name, pdf_new_array, pdf_new_dict,
    pdf_new_name, pdf_new_number, pdf_new_stream, pdf_new_string, pdf_obj, pdf_ref_obj,
    pdf_release_obj, pdf_stream_length, STREAM_COMPRESS,
};
use libc::{free, memmove, memset, strcat, strcmp, strcpy, strlen, strncpy, strstr};

pub type size_t = u64;

use super::dpx_cid::{cid_opt, CIDFont, CIDSysInfo};

use super::dpx_cmap::CMap;
use super::dpx_tt_cmap::tt_cmap;

/* 2 for CID, variable for Code..  */
/* CID (as 16-bit BE), Code ...    */
/* Next Subtbl for LOOKUP_CONTINUE */

pub type CID = u16;
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
pub struct C2RustUnnamed_2 {
    pub alt1: u16,
    pub alt2: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_3 {
    pub platform: u16,
    pub encoding: u16,
    pub pdfnames: [*const i8; 5],
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

pub unsafe fn CIDFont_type2_set_verbose(mut level: i32) {
    verbose = level;
}

pub unsafe fn CIDFont_type2_set_flags(mut flags: i32) {
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
unsafe fn validate_name(mut fontname: *mut i8, mut len: i32) {
    static mut badstrlist: [*const i8; 5] = [
        b"-WIN-RKSJ-H\x00" as *const u8 as *const i8,
        b"-WINP-RKSJ-H\x00" as *const u8 as *const i8,
        b"-WING-RKSJ-H\x00" as *const u8 as *const i8,
        b"-90pv-RKSJ-H\x00" as *const u8 as *const i8,
        ptr::null(),
    ];
    let mut count = 0;
    for i in 0..len {
        if *fontname.offset(i as isize) as i32 == 0i32 {
            memmove(
                fontname.offset(i as isize) as *mut libc::c_void,
                fontname.offset(i as isize).offset(1) as *const libc::c_void,
                (len - i) as _,
            );
            count += 1;
            len -= 1
        }
    }
    if count > 0i32 {
        warn!(
            "Removed {} null character(s) from fontname --> {}",
            count,
            CStr::from_ptr(fontname).display(),
        );
    }
    *fontname.offset(len as isize) = '\u{0}' as i32 as i8;
    /* For some fonts that have bad PS name. ad hoc. remove me. */
    let mut i = 0;
    while !badstrlist[i as usize].is_null() {
        let p = strstr(fontname, badstrlist[i as usize]);
        if !p.is_null() && p > fontname {
            warn!(
                "Removed string \"{}\" from fontname \"{}\".",
                CStr::from_ptr(badstrlist[i as usize]).display(),
                CStr::from_ptr(fontname).display(),
            );
            *p.offset(0) = '\u{0}' as i32 as i8;
            len = p.wrapping_offset_from(fontname) as i64 as i32;
            break;
        } else {
            i += 1
        }
    }
    if len < 1i32 {
        panic!("No valid character found in fontname string.");
    };
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
unsafe fn find_tocode_cmap(mut reg: *const i8, mut ord: *const i8, mut select: i32) -> *mut CMap {
    let mut cmap_id: i32 = -1i32;
    if reg.is_null() || ord.is_null() || select < 0i32 || select > 9i32 {
        panic!("Character set unknown.");
    }
    if streq_ptr(ord, b"UCS\x00" as *const u8 as *const i8) as i32 != 0 && select <= 1i32 {
        return ptr::null_mut();
    }
    let mut i = 0;
    while cmap_id < 0i32 && i < 5i32 {
        let append = known_encodings[select as usize].pdfnames[i as usize];
        if append.is_null() {
            break;
        }
        let cmap_name = format!(
            "{}-{}-{}",
            CStr::from_ptr(reg).display(),
            CStr::from_ptr(ord).display(),
            CStr::from_ptr(append).display()
        );
        cmap_id = CMap_cache_find(&cmap_name);
        i += 1
    }
    if cmap_id < 0i32 {
        warn!(
            "Could not find CID-to-Code mapping for \"{}-{}\".",
            CStr::from_ptr(reg).display(),
            CStr::from_ptr(ord).display(),
        );
        warn!("I tried to load (one of) the following file(s):");
        for i in 0..5 {
            let append = known_encodings[select as usize].pdfnames[i];
            if append.is_null() {
                break;
            }
            info!(
                " {}-{}-{}",
                CStr::from_ptr(reg).display(),
                CStr::from_ptr(ord).display(),
                CStr::from_ptr(append).display()
            );
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
    mut fontdict: *mut pdf_obj,
    mut g: *mut tt_glyphs,
    mut used_chars: *mut i8,
    mut cidtogidmap: *mut u8,
    mut last_cid: u16,
) {
    let mut start: i32 = 0i32;
    let mut prev: i32 = 0i32;
    let mut an_array: *mut pdf_obj = ptr::null_mut();
    let mut empty: i32 = 1i32;
    let w_array = pdf_new_array();
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
                    if !an_array.is_null() {
                        pdf_add_array(&mut *w_array, pdf_new_number(start as f64));
                        pdf_add_array(&mut *w_array, an_array);
                        an_array = ptr::null_mut();
                        empty = 0i32
                    }
                } else {
                    if cid != prev + 1i32 {
                        if !an_array.is_null() {
                            pdf_add_array(&mut *w_array, pdf_new_number(start as f64));
                            pdf_add_array(&mut *w_array, an_array);
                            an_array = ptr::null_mut();
                            empty = 0i32
                        }
                    }
                    if an_array.is_null() {
                        an_array = pdf_new_array();
                        start = cid
                    }
                    pdf_add_array(&mut *an_array, pdf_new_number(width));
                    prev = cid
                }
            }
        }
    }
    if !an_array.is_null() {
        pdf_add_array(&mut *w_array, pdf_new_number(start as f64));
        pdf_add_array(&mut *w_array, an_array);
        empty = 0i32
    }
    pdf_add_dict(&mut *fontdict, "DW", pdf_new_number(dw));
    if empty == 0 {
        pdf_add_dict(&mut *fontdict, "W", pdf_ref_obj(w_array));
    }
    pdf_release_obj(w_array);
}
unsafe fn add_TTCIDVMetrics(
    mut fontdict: *mut pdf_obj,
    mut g: *mut tt_glyphs,
    mut used_chars: *mut i8,
    mut last_cid: u16,
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
    let w2_array = pdf_new_array();
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
                    pdf_add_array(&mut *w2_array, pdf_new_number(cid as f64));
                    pdf_add_array(&mut *w2_array, pdf_new_number(cid as f64));
                    pdf_add_array(&mut *w2_array, pdf_new_number(-advanceHeight));
                    pdf_add_array(&mut *w2_array, pdf_new_number(vertOriginX));
                    pdf_add_array(&mut *w2_array, pdf_new_number(vertOriginY));
                    empty = 0i32
                }
            }
        }
    }
    if defaultVertOriginY != 880i32 as f64 || defaultAdvanceHeight != 1000i32 as f64 {
        let an_array = pdf_new_array();
        pdf_add_array(&mut *an_array, pdf_new_number(defaultVertOriginY));
        pdf_add_array(&mut *an_array, pdf_new_number(-defaultAdvanceHeight));
        pdf_add_dict(&mut *fontdict, "DW2", an_array);
    }
    if empty == 0 {
        pdf_add_dict(&mut *fontdict, "W2", pdf_ref_obj(w2_array));
    }
    pdf_release_obj(w2_array);
}
/*
 * The following routine fixes few problems caused by vendor specific
 * Unicode mappings.
 */
unsafe fn fix_CJK_symbols(mut code: u16) -> u16 {
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
unsafe fn cid_to_code(mut cmap: *mut CMap, mut cid: CID) -> i32 {
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
    if inbytesleft != 0i32 as u64 {
        return 0i32;
    } else {
        if outbytesleft == 31i32 as u64 {
            return outbuf[0] as i32;
        } else {
            if outbytesleft == 30i32 as u64 {
                return (outbuf[0] as i32) << 8i32 | outbuf[1] as i32;
            } else {
                if outbytesleft == 28i32 as u64 {
                    /* We assume the output encoding is UTF-16. */
                    let mut hi: CID = u16::from_be_byte_slice(&outbuf[0..2]);
                    let mut lo: CID = u16::from_be_byte_slice(&outbuf[2..4]);
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

pub unsafe fn CIDFont_type2_dofont(mut font: *mut CIDFont) {
    let mut cmap;
    let mut ttcmap: *mut tt_cmap = ptr::null_mut();
    let offset;
    let mut i: i32 = 0;
    let mut unicode_cmap: i32 = 0i32;
    if (*font).indirect.is_null() {
        return;
    }
    pdf_add_dict(
        &mut *(*font).fontdict,
        "FontDescriptor",
        pdf_ref_obj((*font).descriptor),
    );
    if CIDFont_is_BaseFont(font) {
        return;
    }
    /*
     * CIDSystemInfo comes here since Supplement can be increased.
     */
    let tmp = pdf_new_dict();
    pdf_add_dict(
        &mut *tmp,
        "Registry",
        pdf_new_string(
            (*(*font).csi).registry as *const libc::c_void,
            strlen((*(*font).csi).registry) as _,
        ),
    );
    pdf_add_dict(
        &mut *tmp,
        "Ordering",
        pdf_new_string(
            (*(*font).csi).ordering as *const libc::c_void,
            strlen((*(*font).csi).ordering) as _,
        ),
    );
    pdf_add_dict(
        &mut *tmp,
        "Supplement",
        pdf_new_number((*(*font).csi).supplement as f64),
    );
    pdf_add_dict(&mut *(*font).fontdict, "CIDSystemInfo", tmp);
    /* Quick exit for non-embedded & fixed-pitch font. */
    if CIDFont_get_embedding(font) == 0 && opt_flags & 1i32 << 1i32 != 0 {
        pdf_add_dict(&mut *(*font).fontdict, "DW", pdf_new_number(1000.0f64));
        return;
    }
    let sfont = if let Some(handle) = dpx_open_truetype_file((*font).ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file((*font).ident) {
        dfont_open(handle, (*(*font).options).index)
    } else {
        panic!(
            "Could not open TTF/dfont file: {}",
            CStr::from_ptr((*font).ident).display()
        );
    };
    if sfont.is_null() {
        panic!(
            "Could not open TTF file: {}",
            CStr::from_ptr((*font).ident).display()
        );
    }
    match (*sfont).type_0 {
        16 => {
            offset = ttc_read_offset(sfont, (*(*font).options).index);
            if offset == 0_u32 {
                panic!(
                    "Invalid TTC index in {}.",
                    CStr::from_ptr((*font).ident).display()
                );
            }
        }
        1 => {
            if (*(*font).options).index > 0i32 {
                panic!(
                    "Found TrueType font file while expecting TTC file ({}).",
                    CStr::from_ptr((*font).ident).display()
                );
            }
            offset = 0_u32
        }
        256 => offset = (*sfont).offset,
        _ => {
            panic!(
                "Not a TrueType/TTC font ({})?",
                CStr::from_ptr((*font).ident).display()
            );
        }
    }
    if sfnt_read_table_directory(sfont, offset) < 0i32 {
        panic!(
            "Could not read TrueType table directory ({}).",
            CStr::from_ptr((*font).ident).display()
        );
    }
    /*
     * Adobe-Identity means font's internal glyph ordering here.
     */
    let mut glyph_ordering = if streq_ptr(
        (*(*font).csi).registry,
        b"Adobe\x00" as *const u8 as *const i8,
    ) as i32
        != 0
        && streq_ptr(
            (*(*font).csi).ordering,
            b"Identity\x00" as *const u8 as *const i8,
        ) as i32
            != 0
    {
        1
    } else {
        0
    };
    /*
     * Select TrueType cmap table, find ToCode CMap for each TrueType encodings.
     */
    if glyph_ordering != 0 {
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
                CStr::from_ptr((*font).ident).display()
            );
            warn!(
                "CID character collection for this font is set to \"{}-{}\"",
                CStr::from_ptr((*(*font).csi).registry).display(),
                CStr::from_ptr((*(*font).csi).ordering).display(),
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
        cmap = find_tocode_cmap((*(*font).csi).registry, (*(*font).csi).ordering, i)
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
    let mut cidtogidmap = ptr::null_mut();
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
                if glyph_ordering != 0 {
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
        if glyph_ordering != 0 {
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
                    if glyph_ordering != 0 {
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
        pdf_add_dict(&mut *(*font).fontdict, "DW", pdf_new_number(1000.0f64));
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
    if fontfile.is_null() {
        panic!(
            "Could not created FontFile stream for \"{}\".",
            CStr::from_ptr((*font).ident).display()
        );
    }
    if verbose > 1i32 {
        info!("[{} bytes]", pdf_stream_length(&*fontfile));
    }
    pdf_add_dict(&mut *(*font).descriptor, "FontFile2", pdf_ref_obj(fontfile));
    pdf_release_obj(fontfile);
    /*
     * CIDSet
     */
    let cidset = pdf_new_stream(STREAM_COMPRESS);
    pdf_add_stream(
        &mut *cidset,
        used_chars as *const libc::c_void,
        last_cid as i32 / 8i32 + 1i32,
    );
    pdf_add_dict(&mut *(*font).descriptor, "CIDSet", pdf_ref_obj(cidset));
    pdf_release_obj(cidset);
    /*
     * CIDToGIDMap
     * Adobe's PDF Reference had been describing it as "optional" and
     * default value as "Identity". However, ISO 32000-1 requires it
     * for Type 2 CIDFonts with embedded font programs.
     */
    if cidtogidmap.is_null() {
        pdf_add_dict(&mut *(*font).fontdict, "CIDToGIDMap", pdf_new_name("Identity"));
    } else {
        let c2gmstream = pdf_new_stream(STREAM_COMPRESS);
        pdf_add_stream(
            &mut *c2gmstream,
            cidtogidmap as *const libc::c_void,
            (last_cid as i32 + 1i32) * 2i32,
        );
        pdf_add_dict(&mut *(*font).fontdict, "CIDToGIDMap", pdf_ref_obj(c2gmstream));
        pdf_release_obj(c2gmstream);
        free(cidtogidmap as *mut libc::c_void);
    };
}

pub unsafe fn CIDFont_type2_open(
    mut font: *mut CIDFont,
    mut name: *const i8,
    mut cmap_csi: *mut CIDSysInfo,
    mut opt: *mut cid_opt,
) -> i32 {
    let offset;
    assert!(!font.is_null() && !opt.is_null());
    
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
                panic!(
                    "Invalid TTC index (not TTC font): {}",
                    CStr::from_ptr(name).display()
                );
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
    let shortname =
        new((127_u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8; /* for SJIS, UTF-16, ... string */
    let mut namelen = tt_get_ps_fontname(sfont, shortname, 127_u16) as i32;
    if namelen == 0i32 {
        memset(shortname as *mut libc::c_void, 0i32, 127);
        strncpy(shortname, name, 127);
        namelen = strlen(shortname) as i32
    }
    validate_name(shortname, namelen);
    /*
     * Strlen works, after validate_named string.
     * Mangled name requires more 7 bytes.
     * Style requires more 11 bytes.
     */
    let fontname =
        new((strlen(shortname).wrapping_add(19)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy(fontname, shortname);
    free(shortname as *mut libc::c_void);
    if (*opt).embed != 0 && (*opt).style != 0i32 {
        warn!(
            "Embedding disabled due to style option for {}.",
            CStr::from_ptr(name).display()
        );
        (*opt).embed = 0i32
    }
    match (*opt).style {
        1 => {
            strcat(fontname, b",Bold\x00" as *const u8 as *const i8);
        }
        2 => {
            strcat(fontname, b",Italic\x00" as *const u8 as *const i8);
        }
        3 => {
            strcat(fontname, b",BoldItalic\x00" as *const u8 as *const i8);
        }
        _ => {}
    }
    /*
     * CIDSystemInfo is determined from CMap or from map record option.
     */
    (*font).fontname = fontname; /* This means font's internal glyph ordering. */
    (*font).subtype = 2i32;
    (*font).csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
        as *mut CIDSysInfo;
    if !(*opt).csi.is_null() {
        if !cmap_csi.is_null() {
            if strcmp((*(*opt).csi).registry, (*cmap_csi).registry) != 0
                || strcmp((*(*opt).csi).ordering, (*cmap_csi).ordering) != 0
            {
                warn!("CID character collection mismatched:\n");
                info!(
                    "\tFont: {}-{}-{}\n",
                    CStr::from_ptr((*(*opt).csi).registry).display(),
                    CStr::from_ptr((*(*opt).csi).ordering).display(),
                    (*(*opt).csi).supplement,
                );
                info!(
                    "\tCMap: {}-{}-{}\n",
                    CStr::from_ptr((*cmap_csi).registry).display(),
                    CStr::from_ptr((*cmap_csi).ordering).display(),
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
        (*(*font).csi).registry = new((strlen((*(*opt).csi).registry).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*font).csi).registry, (*(*opt).csi).registry);
        (*(*font).csi).ordering = new((strlen((*(*opt).csi).ordering).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*font).csi).ordering, (*(*opt).csi).ordering);
        (*(*font).csi).supplement = (*(*opt).csi).supplement
    } else if !cmap_csi.is_null() {
        (*(*font).csi).registry = new((strlen((*cmap_csi).registry).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*font).csi).registry, (*cmap_csi).registry);
        (*(*font).csi).ordering = new((strlen((*cmap_csi).ordering).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*font).csi).ordering, (*cmap_csi).ordering);
        (*(*font).csi).supplement = (*cmap_csi).supplement
    } else {
        (*(*font).csi).registry = new((strlen(b"Adobe\x00" as *const u8 as *const i8)
            .wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy(
            (*(*font).csi).registry,
            b"Adobe\x00" as *const u8 as *const i8,
        );
        (*(*font).csi).ordering = new((strlen(b"Identity\x00" as *const u8 as *const i8)
            .wrapping_add(1))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy(
            (*(*font).csi).ordering,
            b"Identity\x00" as *const u8 as *const i8,
        );
        (*(*font).csi).supplement = 0i32
    }
    (*font).fontdict = pdf_new_dict();
    pdf_add_dict(&mut *(*font).fontdict, "Type", pdf_new_name("Font"));
    pdf_add_dict(&mut *(*font).fontdict, "Subtype", pdf_new_name("CIDFontType2"));
    (*font).descriptor = tt_get_fontdesc(sfont, &mut (*opt).embed, (*opt).stemv, 0i32, name);
    if (*font).descriptor.is_null() {
        panic!("Could not obtain necessary font info.");
    }
    if (*opt).embed != 0 {
        memmove(
            fontname.offset(7) as *mut libc::c_void,
            fontname as *const libc::c_void,
            strlen(fontname).wrapping_add(1),
        );
        pdf_font_make_uniqueTag(fontname);
        *fontname.offset(6) = '+' as i32 as i8
    }
    pdf_add_dict(&mut *(*font).descriptor, "FontName", pdf_copy_name(fontname));
    pdf_add_dict(&mut *(*font).fontdict, "BaseFont", pdf_copy_name(fontname));
    sfnt_close(sfont);
    /*
     * Don't write fontdict here.
     * /Supplement in /CIDSystemInfo may change.
     */
    0i32
}
