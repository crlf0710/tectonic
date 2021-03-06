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

use std::ptr;

use super::dpx_sfnt::{
    dfont_open, sfnt_create_FontFile_stream, sfnt_find_table_pos, sfnt_open,
    sfnt_read_table_directory, sfnt_require_table,
};
use crate::dpx_truetype::SfntTableInfo;
use crate::{info, warn, FromBEByteSlice};

use super::dpx_cid::{CIDFont_get_embedding, CIDFont_get_parent_id, CIDFont_is_BaseFont, CidFont};
use super::dpx_cmap::{CMap_cache_find, CMap_cache_get, CMap_decode_char};
use super::dpx_dpxfile::{dpx_open_dfont_file, dpx_open_truetype_file};
use super::dpx_pdffont::pdf_font_make_uniqueTag;
use super::dpx_tt_aux::tt_get_fontdesc;
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_cmap::{tt_cmap_lookup, tt_cmap_read};
use super::dpx_tt_glyf::{tt_add_glyph, tt_build_tables, tt_get_index, tt_get_metrics};
use super::dpx_tt_gsub::{otl_gsub, otl_gsub_add_feat, otl_gsub_apply, otl_gsub_select};
use super::dpx_tt_table::tt_get_ps_fontname;
use super::dpx_type0::{Type0Font_cache_get, Type0Font_get_usedchars};
use crate::dpx_pdfobj::{
    pdf_dict, pdf_name, pdf_ref_obj, pdf_stream, pdf_string, IntoObj, IntoRef, PushObj,
    STREAM_COMPRESS,
};

use super::dpx_cid::{cid_opt, CIDFont, CIDSysInfo};

use super::dpx_cmap::CMap;

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
    pub(crate) pdfnames: &'static [&'static str],
}
/*
 * TrueType glyf table is sorted by CID and no CIDToGIDMap is used here.
 * GhostScript can't handle CIDToGIDMap correctly.
 */
/* pseudo unique tag */
/* CID font */
/* TrueType */
static mut verbose: i32 = 0;
static mut opt_flags: i32 = 0;

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
        platform: 3,
        encoding: 10,
        pdfnames: &["UCSms-UCS4", "UCSms-UCS2", "UCS4", "UCS2"],
    },
    C2RustUnnamed_3 {
        platform: 3,
        encoding: 1,
        pdfnames: &["UCSms-UCS4", "UCSms-UCS2", "UCS4", "UCS2"],
    },
    C2RustUnnamed_3 {
        platform: 3,
        encoding: 2,
        pdfnames: &["90ms-RKSJ"],
    },
    C2RustUnnamed_3 {
        platform: 3,
        encoding: 3,
        pdfnames: &["GBK-EUC"],
    },
    C2RustUnnamed_3 {
        platform: 3,
        encoding: 4,
        pdfnames: &["ETen-B5"],
    },
    C2RustUnnamed_3 {
        platform: 3,
        encoding: 5,
        pdfnames: &["KSCms-UHC"],
    },
    C2RustUnnamed_3 {
        platform: 1,
        encoding: 1,
        pdfnames: &["90pv-RKSJ"],
    },
    C2RustUnnamed_3 {
        platform: 1,
        encoding: 2,
        pdfnames: &["B5pc"],
    },
    C2RustUnnamed_3 {
        platform: 1,
        encoding: 25,
        pdfnames: &["GBpc-EUC"],
    },
    C2RustUnnamed_3 {
        platform: 1,
        encoding: 3,
        pdfnames: &["KSCpc-EUC"],
    },
    C2RustUnnamed_3 {
        platform: 0,
        encoding: 0,
        pdfnames: &[],
    },
];
unsafe fn find_tocode_cmap(reg: &str, ord: &str, select: i32) -> *mut CMap {
    let mut cmap_id = None;
    if select < 0 || select > 9 {
        panic!("Character set unknown.");
    }
    if ord == "UCS" && select <= 1 {
        return ptr::null_mut();
    }
    for append in known_encodings[select as usize].pdfnames {
        let cmap_name = format!("{}-{}-{}", reg, ord, append);
        cmap_id = CMap_cache_find(&cmap_name);
        if cmap_id.is_some() {
            break;
        }
    }
    if cmap_id.is_some() {
        CMap_cache_get(cmap_id)
    } else {
        warn!(
            "Could not find CID-to-Code mapping for \"{}-{}\".",
            reg, ord,
        );
        warn!("I tried to load (one of) the following file(s):");
        for append in known_encodings[select as usize].pdfnames {
            info!(" {}-{}-{}", reg, ord, append);
        }
        warn!("Please check if this file exists.");
        panic!("Cannot continue...");
    }
}
/*
 * CIDFont glyph metrics:
 * Mostly same as add_CID[HV]Metrics in cidtype0.c.
 */
unsafe fn add_TTCIDHMetrics(
    fontdict: &mut pdf_dict,
    g: &tt_glyphs,
    used_chars: *mut i8,
    cidtogidmap: &[u8],
    last_cid: u16,
) {
    let mut start: i32 = 0;
    let mut prev: i32 = 0;
    let mut an_array = None;
    let mut empty: i32 = 1;
    let mut w_array = vec![];
    let dw = if g.dw as i32 != 0 && g.dw as i32 <= g.emsize as i32 {
        (1000.0f64 * g.dw as i32 as f64 / g.emsize as i32 as f64 / 1 as f64 + 0.5f64).floor()
            * 1 as f64
    } else {
        (1000.0f64 * g.gd[0].advw as i32 as f64 / g.emsize as i32 as f64 / 1 as f64 + 0.5f64)
            .floor()
            * 1 as f64
    };
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8) as isize) as i32 & 1 << 7 - cid % 8 == 0) {
            let gid = (if !cidtogidmap.is_empty() {
                (cidtogidmap[2 * cid as usize] as i32) << 8
                    | cidtogidmap[2 * cid as usize + 1] as i32
            } else {
                cid
            }) as u16;
            let idx = tt_get_index(g, gid);
            if !(cid != 0 && idx == 0) {
                let width = (1000.0f64 * g.gd[idx as usize].advw as i32 as f64
                    / g.emsize as i32 as f64
                    / 1 as f64
                    + 0.5f64)
                    .floor()
                    * 1 as f64;
                if width == dw {
                    if an_array.is_some() {
                        w_array.push_obj(start as f64);
                        w_array.push_obj(an_array.take().unwrap());
                        empty = 0
                    }
                } else {
                    if cid != prev + 1 {
                        if an_array.is_some() {
                            w_array.push_obj(start as f64);
                            w_array.push_obj(an_array.take().unwrap());
                            empty = 0
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
        empty = 0
    }
    fontdict.set("DW", dw);
    if empty == 0 {
        fontdict.set("W", w_array.into_ref());
    }
}
unsafe fn add_TTCIDVMetrics(
    fontdict: &mut pdf_dict,
    g: &tt_glyphs,
    used_chars: *mut i8,
    last_cid: u16,
) {
    let mut empty: i32 = 1;
    let defaultVertOriginY = (1000.0f64 * (g.default_advh as i32 - g.default_tsb as i32) as f64
        / g.emsize as i32 as f64
        / 1 as f64
        + 0.5f64)
        .floor()
        * 1 as f64;
    let defaultAdvanceHeight =
        (1000.0f64 * g.default_advh as i32 as f64 / g.emsize as i32 as f64 / 1 as f64 + 0.5f64)
            .floor()
            * 1 as f64;
    let mut w2_array = vec![];
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8) as isize) as i32 & 1 << 7 - cid % 8 == 0) {
            let idx = tt_get_index(g, cid as u16);
            if !(cid != 0 && idx == 0) {
                let advanceHeight = (1000.0f64 * g.gd[idx as usize].advh as i32 as f64
                    / g.emsize as i32 as f64
                    / 1 as f64
                    + 0.5f64)
                    .floor()
                    * 1 as f64;
                let vertOriginX = (1000.0f64 * (0.5f64 * g.gd[idx as usize].advw as i32 as f64)
                    / g.emsize as i32 as f64
                    / 1 as f64
                    + 0.5f64)
                    .floor()
                    * 1 as f64;
                let vertOriginY = (1000.0f64
                    * (g.gd[idx as usize].tsb as i32 + g.gd[idx as usize].ury as i32) as f64
                    / g.emsize as i32 as f64
                    / 1 as f64
                    + 0.5f64)
                    .floor()
                    * 1 as f64;
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
                    empty = 0
                }
            }
        }
    }
    if defaultVertOriginY != 880 as f64 || defaultAdvanceHeight != 1000 as f64 {
        let mut an_array = vec![];
        an_array.push_obj(defaultVertOriginY);
        an_array.push_obj(-defaultAdvanceHeight);
        fontdict.set("DW2", an_array);
    }
    if empty == 0 {
        fontdict.set("W2", w2_array.into_ref());
    }
}
/*
 * The following routine fixes few problems caused by vendor specific
 * Unicode mappings.
 */
unsafe fn fix_CJK_symbols(code: u16) -> u16 {
    static mut CJK_Uni_symbols: [C2RustUnnamed_2; 10] = [
        C2RustUnnamed_2 {
            alt1: 0x2014,
            alt2: 0x2015,
        },
        C2RustUnnamed_2 {
            alt1: 0x2016,
            alt2: 0x2225,
        },
        C2RustUnnamed_2 {
            alt1: 0x203e,
            alt2: 0xffe3,
        },
        C2RustUnnamed_2 {
            alt1: 0x2026,
            alt2: 0x22ef,
        },
        C2RustUnnamed_2 {
            alt1: 0x2212,
            alt2: 0xff0d,
        },
        C2RustUnnamed_2 {
            alt1: 0x301c,
            alt2: 0xff5e,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe0,
            alt2: 0xa2,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe1,
            alt2: 0xa3,
        },
        C2RustUnnamed_2 {
            alt1: 0xffe2,
            alt2: 0xac,
        },
        C2RustUnnamed_2 {
            alt1: 0xffff,
            alt2: 0xffff,
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
    if cmap.is_null() {
        return cid as i32;
    }
    let mut inbuf = &cid.to_be_bytes()[..2];
    let q = CMap_decode_char(&*cmap, &mut inbuf, &mut outbuf[..]);
    if inbuf.len() != 0 {
        return 0;
    } else {
        match q.len() {
            31 => return outbuf[0] as i32,
            30 => return (outbuf[0] as i32) << 8 | outbuf[1] as i32,
            28 => {
                /* We assume the output encoding is UTF-16. */
                let hi: CID = u16::from_be_byte_slice(&outbuf[0..2]);
                let lo: CID = u16::from_be_byte_slice(&outbuf[2..4]);
                if hi as i32 >= 0xd800
                    && hi as i32 <= 0xdbff
                    && lo as i32 >= 0xdc00
                    && lo as i32 <= 0xdfff
                {
                    return (hi as i32 - 0xd800) * 0x400 + 0x10000 + lo as i32 - 0xdc00;
                } else {
                    return (hi as i32) << 16 | lo as i32;
                }
            }
            _ => {}
        }
    }
    0
}
/* #define NO_GHOSTSCRIPT_BUG 1 */

pub(crate) unsafe fn CIDFont_type2_dofont(font: &mut CIDFont) {
    let cmap;
    let mut ttcmap = None;
    let offset;
    let mut i: i32 = 0;
    let mut unicode_cmap: i32 = 0;
    if font.indirect.is_null() {
        return;
    }
    (*font.fontdict)
        .as_dict_mut()
        .set("FontDescriptor", pdf_ref_obj(font.descriptor));
    if CIDFont_is_BaseFont(font) {
        return;
    }
    /*
     * CIDSystemInfo comes here since Supplement can be increased.
     */
    let mut tmp = pdf_dict::new();
    let csi = &font.csi;
    tmp.set("Registry", pdf_string::new(csi.registry.as_bytes()));
    tmp.set("Ordering", pdf_string::new(csi.ordering.as_bytes()));
    tmp.set("Supplement", csi.supplement as f64);
    (*font.fontdict).as_dict_mut().set("CIDSystemInfo", tmp);
    /* Quick exit for non-embedded & fixed-pitch font. */
    if CIDFont_get_embedding(font) == 0 && opt_flags & 1 << 1 != 0 {
        (*font.fontdict).as_dict_mut().set("DW", 1000_f64);
        return;
    }
    let mut sfont = if let Some(handle) = dpx_open_truetype_file(&font.ident) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(&font.ident) {
        dfont_open(handle, (*font.options).index)
            .expect(&format!("Could not open TTF file: {}", font.ident))
    } else {
        panic!("Could not open TTF/dfont file: {}", font.ident);
    };
    match sfont.type_0 {
        16 => {
            offset = ttc_read_offset(&mut sfont, (*font.options).index);
            if offset == 0_u32 {
                panic!("Invalid TTC index in {}.", font.ident);
            }
        }
        1 => {
            if (*font.options).index > 0 {
                panic!(
                    "Found TrueType font file while expecting TTC file ({}).",
                    font.ident
                );
            }
            offset = 0_u32
        }
        256 => offset = sfont.offset,
        _ => {
            panic!("Not a TrueType/TTC font ({})?", font.ident);
        }
    }
    if sfnt_read_table_directory(&mut sfont, offset) < 0 {
        panic!("Could not read TrueType table directory ({}).", font.ident);
    }
    /*
     * Adobe-Identity means font's internal glyph ordering here.
     */
    let glyph_ordering = csi.registry == "Adobe" && csi.ordering == "Identity";
    /*
     * Select TrueType cmap table, find ToCode CMap for each TrueType encodings.
     */
    if glyph_ordering {
        ttcmap = None;
        cmap = ptr::null_mut()
    } else {
        /*
         * This part contains a bug. It may choose SJIS encoding TrueType cmap
         * table for Adobe-GB1.
         */
        for i in 0..=9 {
            ttcmap = tt_cmap_read(
                &mut sfont,
                known_encodings[i].platform,
                known_encodings[i].encoding,
            );
            if ttcmap.is_some() {
                break;
            }
        }
        if ttcmap.is_none() {
            warn!(
                "No usable TrueType cmap table found for font \"{}\".",
                font.ident
            );
            warn!(
                "CID character collection for this font is set to \"{}-{}\"",
                csi.registry, csi.ordering,
            );
            panic!("Cannot continue without this...");
        } else {
            if i <= 1 {
                unicode_cmap = 1
            } else {
                unicode_cmap = 0
            }
        }
        /*
         * NULL is returned if CMap is Identity CMap.
         */
        cmap = find_tocode_cmap(&csi.registry, &csi.ordering, i)
    } /* .notdef */
    let mut glyphs = tt_glyphs::init();
    let mut last_cid = 0 as CID;
    let mut v_used_chars = ptr::null_mut();
    let mut h_used_chars = v_used_chars;
    let mut used_chars = h_used_chars;
    let parent_id = CIDFont_get_parent_id(font, 0);
    if parent_id >= 0 {
        let parent = Type0Font_cache_get(parent_id);
        h_used_chars = Type0Font_get_usedchars(&*parent)
    }
    let parent_id = CIDFont_get_parent_id(font, 1);
    if parent_id >= 0 {
        let parent = Type0Font_cache_get(parent_id);
        v_used_chars = Type0Font_get_usedchars(&*parent)
    }
    if h_used_chars.is_null() && v_used_chars.is_null() {
        panic!("Unexpected error.");
    }
    /*
     * Quick check of max CID.
     */
    let mut c = 0;
    i = 8191;
    while i >= 0 {
        if !h_used_chars.is_null() && *h_used_chars.offset(i as isize) as i32 != 0 {
            last_cid = (i * 8 + 7) as CID;
            c = *h_used_chars.offset(i as isize) as i32;
            break;
        } else {
            i -= 1
        }
    }
    i = 8191;
    while i >= 0 {
        if !v_used_chars.is_null() && *v_used_chars.offset(i as isize) as i32 != 0 {
            if i * 8 + 7 >= last_cid as i32 {
                c = if i * 8 + 7 > last_cid as i32 {
                    *v_used_chars.offset(i as isize) as i32
                } else {
                    c | *v_used_chars.offset(i as isize) as i32
                };
                last_cid = (i * 8 + 7) as CID;
                break;
            }
        }
        i -= 1
    }
    if last_cid as i32 > 0 {
        for i in 0..8 {
            if c >> i & 1 != 0 {
                break;
            }
            last_cid = last_cid.wrapping_sub(1);
        }
    }
    if last_cid as u32 >= 0xffffu32 {
        panic!("CID count > 65535");
    }
    let cidtogidmap = Vec::new();
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
            if !(*h_used_chars.offset((cid as i32 / 8) as isize) as i32 & 1 << 7 - cid as i32 % 8
                == 0)
            {
                if glyph_ordering {
                    gid = cid;
                    code = cid as i32
                } else {
                    code = cid_to_code(cmap, cid);
                    let ttcmap = ttcmap.as_ref().unwrap();
                    gid = tt_cmap_lookup(ttcmap, code as u32);
                    if gid as i32 == 0 && unicode_cmap != 0 {
                        let alt_code = fix_CJK_symbols(code as u16) as i32;
                        if alt_code != code {
                            gid = tt_cmap_lookup(ttcmap, alt_code as u32);
                            if gid as i32 != 0 {
                                warn!(
                                    "Unicode char U+{:04x} replaced with U+{:04x}.",
                                    code, alt_code,
                                );
                            }
                        }
                    }
                    /* FIX_CJK_UNIOCDE_SYMBOLS */
                }
                if gid as i32 == 0 {
                    warn!("Glyph missing in font. (CID={}, code=0x{:04x})", cid, code);
                }
                /* TODO: duplicated glyph */
                tt_add_glyph(&mut glyphs, gid, cid);
            }
        }
    }
    /*
     * Vertical
     */
    if !v_used_chars.is_null() {
        /*
         * Require `vrt2' or `vert'.
         */
        let gsub_list = if glyph_ordering {
            None
        } else {
            let mut gsub_list = otl_gsub::new();
            if otl_gsub_add_feat(&mut gsub_list, "*", "*", "vrt2", &sfont).is_none() {
                if otl_gsub_add_feat(&mut gsub_list, "*", "*", "vert", &sfont).is_none() {
                    warn!("GSUB feature vrt2/vert not found.");
                    None
                } else {
                    otl_gsub_select(&mut gsub_list, "*", "*", "vert");
                    Some(gsub_list)
                }
            } else {
                otl_gsub_select(&mut gsub_list, "*", "*", "vrt2");
                Some(gsub_list)
            }
        };
        for cid in 1..=last_cid as CID {
            let code_0;
            let mut gid_0;
            if !(*v_used_chars.offset((cid as i32 / 8) as isize) as i32 & 1 << 7 - cid as i32 % 8
                == 0)
            {
                /* There may be conflict of horizontal and vertical glyphs
                 * when font is used with /UCS. However, we simply ignore
                 * that...
                 */
                if !(!h_used_chars.is_null()
                    && *h_used_chars.offset((cid as i32 / 8) as isize) as i32
                        & 1 << 7 - cid as i32 % 8
                        != 0)
                {
                    if glyph_ordering {
                        gid_0 = cid;
                        code_0 = cid as i32
                    } else {
                        code_0 = cid_to_code(cmap, cid);
                        let ttcmap = ttcmap.as_ref().unwrap();
                        gid_0 = tt_cmap_lookup(ttcmap, code_0 as u32);
                        if gid_0 as i32 == 0 && unicode_cmap != 0 {
                            let alt_code_0 = fix_CJK_symbols(code_0 as u16) as i32;
                            if alt_code_0 != code_0 {
                                gid_0 = tt_cmap_lookup(ttcmap, alt_code_0 as u32);
                                if gid_0 as i32 != 0 {
                                    warn!(
                                        "Unicode char U+{:04x} replaced with U+{:04x}.",
                                        code_0, alt_code_0,
                                    );
                                }
                            }
                        }
                        /* FIX_CJK_UNIOCDE_SYMBOLS */
                    }
                    if gid_0 as i32 == 0 {
                        warn!(
                            "Glyph missing in font. (CID={}, code=0x{:04x})",
                            cid, code_0,
                        );
                    } else if let Some(gsub) = gsub_list.as_ref() {
                        otl_gsub_apply(gsub, &mut gid_0);
                    }
                    tt_add_glyph(&mut glyphs, gid_0, cid);
                    /* !NO_GHOSTSCRIPT_BUG */
                    if !used_chars.is_null() {
                        /* merge vertical used_chars to horizontal */
                        *used_chars.offset((cid as i32 / 8) as isize) |=
                            (1 << 7 - cid as i32 % 8) as i8;
                    }
                }
            }
        }
        drop(gsub_list);
        if used_chars.is_null() {
            /* We have no horizontal. */
            used_chars = v_used_chars
        }
    }
    if used_chars.is_null() {
        panic!("Unexpected error.");
    }
    if CIDFont_get_embedding(font) != 0 {
        if tt_build_tables(&mut sfont, &mut glyphs) < 0 {
            panic!("Could not created FontFile stream.");
        }
        if verbose > 1 {
            info!("[{} glyphs (Max CID: {})]", glyphs.gd.len(), last_cid);
        }
    } else if tt_get_metrics(&mut sfont, &mut glyphs) < 0 {
        panic!("Reading glyph metrics failed...");
    }
    /*
     * DW, W, DW2, and W2
     */
    if opt_flags & 1 << 1 != 0 {
        (*font.fontdict).as_dict_mut().set("DW", 1000_f64);
    } else {
        add_TTCIDHMetrics(
            (*font.fontdict).as_dict_mut(),
            &glyphs,
            used_chars,
            &cidtogidmap,
            last_cid,
        );
        if !v_used_chars.is_null() {
            add_TTCIDVMetrics(
                (*font.fontdict).as_dict_mut(),
                &glyphs,
                used_chars,
                last_cid,
            );
        }
    }
    /* Finish here if not embedded. */
    if CIDFont_get_embedding(font) == 0 {
        return;
    }
    /* Create font file */
    for table in &required_table {
        if sfnt_require_table(&mut sfont, table).is_err() {
            panic!(
                "Some required TrueType table ({}) does not exist.",
                table.name_str()
            );
        }
    }
    /*
     * FontFile2
     */
    let fontfile = sfnt_create_FontFile_stream(&mut sfont);
    if verbose > 1 {
        info!("[{} bytes]", fontfile.len());
    }
    (*font.descriptor)
        .as_dict_mut()
        .set("FontFile2", fontfile.into_ref());
    /*
     * CIDSet
     */
    let mut cidset = pdf_stream::new(STREAM_COMPRESS);
    cidset.add(used_chars as *const libc::c_void, last_cid as i32 / 8 + 1);
    (*font.descriptor)
        .as_dict_mut()
        .set("CIDSet", cidset.into_ref());
    /*
     * CIDToGIDMap
     * Adobe's PDF Reference had been describing it as "optional" and
     * default value as "Identity". However, ISO 32000-1 requires it
     * for Type 2 CIDFonts with embedded font programs.
     */
    if cidtogidmap.is_empty() {
        (*font.fontdict)
            .as_dict_mut()
            .set("CIDToGIDMap", "Identity");
    } else {
        let mut c2gmstream = pdf_stream::new(STREAM_COMPRESS);
        c2gmstream.add_slice(&cidtogidmap[..(last_cid as usize + 1) * 2]);
        (*font.fontdict)
            .as_dict_mut()
            .set("CIDToGIDMap", c2gmstream.into_ref());
    };
}

pub(crate) unsafe fn CIDFont_type2_open(
    name: &str,
    cmap_csi: &Option<CIDSysInfo>,
    mut opt: Box<cid_opt>,
) -> Result<Box<CIDFont>, Box<cid_opt>> {
    let offset;
    let mut sfont = if let Some(handle) = dpx_open_truetype_file(name) {
        sfnt_open(handle)
    } else if let Some(handle) = dpx_open_dfont_file(name) {
        if let Some(sfont) = dfont_open(handle, opt.index) {
            sfont
        } else {
            return Err(opt);
        }
    } else {
        return Err(opt);
    };
    match sfont.type_0 {
        16 => offset = ttc_read_offset(&mut sfont, opt.index),
        1 => {
            if opt.index > 0 {
                panic!("Invalid TTC index (not TTC font): {}", name);
            }
            offset = 0_u32
        }
        256 => offset = sfont.offset,
        _ => {
            return Err(opt);
        }
    }
    if sfnt_read_table_directory(&mut sfont, offset) < 0 {
        panic!("Reading TrueType table directory failed.");
    }
    /* Ignore TrueType Collection with CFF table. */
    if sfont.type_0 == 1 << 4 && sfnt_find_table_pos(&sfont, b"CFF ") != 0 {
        return Err(opt);
    }
    /* MAC-ROMAN-EN-POSTSCRIPT or WIN-UNICODE-EN(US)-POSTSCRIPT */
    /* for SJIS, UTF-16, ... string */
    let shortname = tt_get_ps_fontname(&mut sfont).unwrap_or_else(|| name.to_owned());
    let shortname = validate_name(shortname);

    if opt.embed != 0 && opt.style != 0 {
        warn!("Embedding disabled due to style option for {}.", name);
        opt.embed = 0;
    }

    let mut fontname = shortname
        + match opt.style {
            1 => ",Bold",
            2 => ",Italic",
            3 => ",BoldItalic",
            _ => "",
        };
    /*
     * CIDSystemInfo is determined from CMap or from map record option.
     */
    let subtype = CidFont::Type2;
    let csi = Box::new(if let Some(opt_csi) = opt.csi.as_deref_mut() {
        if let Some(cmap_csi) = cmap_csi.as_ref() {
            if opt_csi.registry != cmap_csi.registry || opt_csi.ordering != cmap_csi.ordering {
                warn!("CID character collection mismatched:\n");
                info!(
                    "\tFont: {}-{}-{}\n",
                    opt_csi.registry, opt_csi.ordering, opt_csi.supplement,
                );
                info!(
                    "\tCMap: {}-{}-{}\n",
                    cmap_csi.registry, cmap_csi.ordering, cmap_csi.supplement,
                );
                panic!("Incompatible CMap specified for this font.");
            }
            if opt_csi.supplement < cmap_csi.supplement {
                warn!("Supplmement value in CIDSystemInfo increased.");
                warn!("Some characters may not shown.");
                opt_csi.supplement = cmap_csi.supplement;
            }
        }
        CIDSysInfo {
            registry: opt_csi.registry.clone(),
            ordering: opt_csi.ordering.clone(),
            supplement: opt_csi.supplement,
        }
    } else {
        if let Some(cmap_csi) = cmap_csi.as_ref() {
            CIDSysInfo {
                registry: cmap_csi.registry.clone(),
                ordering: cmap_csi.ordering.clone(),
                supplement: cmap_csi.supplement,
            }
        } else {
            CIDSysInfo {
                registry: "Adobe".into(),
                ordering: "Identity".into(),
                supplement: 0,
            }
        }
    });
    let mut fontdict = pdf_dict::new();
    fontdict.set("Type", "Font");
    fontdict.set("Subtype", "CIDFontType2");
    let mut descriptor = tt_get_fontdesc(&sfont, &mut opt.embed, opt.stemv, 0, name)
        .unwrap_or_else(|| panic!("Could not obtain necessary font info."));
    if opt.embed != 0 {
        let tag = pdf_font_make_uniqueTag();
        fontname = format!("{}+{}", tag, fontname);
    }
    descriptor.set("FontName", pdf_name::new(fontname.as_bytes()));
    fontdict.set("BaseFont", pdf_name::new(fontname.as_bytes()));

    /*
     * Don't write fontdict here.
     * /Supplement in /CIDSystemInfo may change.
     */
    Ok(Box::new(CIDFont {
        ident: name.to_string(),
        name: name.to_string(),
        fontname,
        subtype,
        flags: 0,
        parent: [-1, -1],
        csi,
        options: opt,
        indirect: ptr::null_mut(),
        fontdict: fontdict.into_obj(),
        descriptor: descriptor.into_obj(),
    }))
}
