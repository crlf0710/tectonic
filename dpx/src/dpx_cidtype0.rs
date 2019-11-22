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

use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_sfnt::{
    sfnt_close, sfnt_find_table_pos, sfnt_locate_table, sfnt_open, sfnt_read_table_directory,
};
use crate::{info, warn};

use super::dpx_agl::{
    agl_chop_suffix, agl_lookup_list, agl_name_convert_unicode, agl_name_is_unicode,
    agl_sput_UTF16BE,
};
use super::dpx_cff::{
    cff_add_string, cff_charsets_lookup_inverse, cff_close, cff_get_index_header, cff_get_name,
    cff_get_sid, cff_get_string, cff_glyph_lookup, cff_index_size, cff_new_index, cff_open,
    cff_pack_charsets, cff_pack_fdselect, cff_pack_index, cff_put_header, cff_read_charsets,
    cff_read_fdselect, cff_read_subrs, cff_release_index, cff_set_name, cff_update_string,
};
use super::dpx_cff::{
    cff_charsets_lookup, cff_fdselect_lookup, cff_read_fdarray, cff_read_private,
    cff_release_charsets, cff_release_fdselect, CffIndex, Pack,
};
use super::dpx_cff_dict::{
    cff_dict_add, cff_dict_get, cff_dict_known, cff_dict_pack, cff_dict_remove, cff_dict_set,
    cff_dict_update, cff_new_dict,
};
use super::dpx_cid::{
    CIDFont_get_embedding, CIDFont_get_opt_index, CIDFont_get_parent_id, CIDFont_is_BaseFont,
};
use super::dpx_cid::{CSI_IDENTITY, CSI_UNICODE};
use super::dpx_cmap::{
    CMap_add_bfchar, CMap_add_cidchar, CMap_add_codespacerange, CMap_cache_add, CMap_cache_find,
    CMap_new, CMap_release, CMap_set_CIDSysInfo, CMap_set_name, CMap_set_type, CMap_set_wmode,
};
use super::dpx_cmap_write::CMap_create_stream;
use super::dpx_cs_type2::cs_copy_charstring;
use super::dpx_dpxfile::{dpx_open_opentype_file, dpx_open_truetype_file, dpx_open_type1_file};
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer_u8 as work_buffer;
use super::dpx_pdffont::pdf_font_make_uniqueTag;
use super::dpx_t1_char::{t1char_convert_charstring, t1char_get_metrics};
use super::dpx_t1_load::t1_load_font;
use super::dpx_tt_aux::tt_get_fontdesc;
use super::dpx_tt_aux::ttc_read_offset;
use super::dpx_tt_table::{
    tt_read_VORG_table, tt_read_head_table, tt_read_hhea_table, tt_read_longMetrics,
    tt_read_maxp_table, tt_read_os2__table, tt_read_vhea_table,
};
use super::dpx_type0::{Type0Font_cache_get, Type0Font_get_usedchars, Type0Font_set_ToUnicode};
use crate::dpx_pdfobj::{
    pdf_copy_name, pdf_dict, pdf_new_string, pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_stream,
    IntoObj, PushObj, STREAM_COMPRESS,
};
use crate::dpx_truetype::sfnt_table_info;
use crate::ttstub_input_read;
use libc::{free, memmove, memset, strcat, strcmp, strcpy, strlen, strstr};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;

use super::dpx_cid::{cid_opt, CIDFont, CIDSysInfo};

use super::dpx_sfnt::sfnt;

use super::dpx_cff::cff_charsets;
pub type s_SID = u16;

pub type l_offset = u32;
use super::dpx_cff::cff_fdselect;
use super::dpx_cff::cff_range3;
use super::dpx_tt_table::tt_longMetrics;
use super::dpx_tt_table::tt_vhea_table;

use super::dpx_cff::cff_dict;
use super::dpx_cff::cff_font;

#[repr(C)]
pub struct CIDType0Info<'a> {
    pub sfont: *mut sfnt,
    pub cffont: *mut cff_font<'a>,
}
use super::dpx_tt_table::{tt_head_table, tt_maxp_table};
/* Acoid conflict with CHAR ... from <winnt.h>.  */
/* Data Types as described in Apple's TTRefMan */

pub type CID = u16;

pub enum CidOpenError {
    IS_CIDFONT = -6,
    NOT_CIDFONT = -5,
    CANNOT_OPEN_CFF_FONT = -4,
    NO_CFF_TABLE = -3,
    NOT_SFNT_FONT = -2,
    CANNOT_OPEN_FILE = -1,
}

/* Mapping types, MAP_IS_NAME is not supported. */
/* Lookup flags */
/* DEBUG */
/* Codespacerange */
/* Dimension of this codespacerange */
/* Lower bounds of valid input code */
/* Upper bounds of valid input code */

use super::dpx_t1_char::t1_ginfo;

/*
 * CID-Keyed Font support:
 *
 *  Only CFF/OpenType CID-Keyed Font with Type 2 charstrings is supported.
 *
 */
/* typedef CID in cmap.h */
/* pseudo unique tag */
/* Font info. from OpenType tables */
/* Metrics */
static mut verbose: i32 = 0i32;
static mut opt_flags: i32 = 0i32;

pub unsafe fn CIDFont_type0_set_verbose(level: i32) {
    verbose = level;
}

pub unsafe fn CIDFont_type0_set_flags(flags: i32) {
    opt_flags = flags;
}
/*
 * PDF Reference 3rd. ed., p.340, "Glyph Metrics in CID Fonts".
 */
unsafe fn add_CIDHMetrics(
    fontdict: *mut pdf_obj,
    CIDToGIDMap: *mut u8,
    last_cid: u16,
    maxp: *mut tt_maxp_table,
    head: *mut tt_head_table,
    hmtx: *mut tt_longMetrics,
) {
    let mut an_array: *mut pdf_obj = ptr::null_mut();
    let mut start: i32 = 0i32;
    let mut prev: i32 = 0i32;
    let mut empty: i32 = 1i32;
    let defaultAdvanceWidth = (1000.0f64 * (*hmtx.offset(0)).advance as i32 as f64
        / (*head).unitsPerEm as i32 as f64
        / 1i32 as f64
        + 0.5f64)
        .floor()
        * 1i32 as f64;
    /*
     * We alway use format:
     *  c [w_1 w_2 ... w_n]
     */
    let mut w_array = vec![];
    for cid in 0..=last_cid as i32 {
        let gid = (if !CIDToGIDMap.is_null() {
            (*CIDToGIDMap.offset((2i32 * cid) as isize) as i32) << 8i32
                | *CIDToGIDMap.offset((2i32 * cid + 1i32) as isize) as i32
        } else {
            cid
        }) as u16;
        if !(gid as i32 >= (*maxp).numGlyphs as i32 || cid != 0i32 && gid as i32 == 0i32) {
            let advanceWidth = (1000.0f64 * (*hmtx.offset(gid as isize)).advance as i32 as f64
                / (*head).unitsPerEm as i32 as f64
                / 1i32 as f64
                + 0.5f64)
                .floor()
                * 1i32 as f64;
            if advanceWidth == defaultAdvanceWidth {
                if !an_array.is_null() {
                    w_array.push_obj(start as f64);
                    w_array.push(an_array);
                    an_array = ptr::null_mut();
                    empty = 0i32
                }
            } else {
                if cid != prev + 1i32 && !an_array.is_null() {
                    w_array.push_obj(start as f64);
                    w_array.push(an_array);
                    an_array = ptr::null_mut();
                    empty = 0i32
                }
                if an_array.is_null() {
                    an_array = Vec::new().into_obj();
                    start = cid
                }
                (*an_array).as_array_mut().push_obj(advanceWidth);
                prev = cid
            }
        }
    }
    if !an_array.is_null() {
        w_array.push_obj(start as f64);
        w_array.push(an_array);
        empty = 0i32
    }
    /*
     * We always write DW for older MacOS X's preview app.
     * PDF Reference 2nd. ed, wrongly described default value of DW as 0, and
     * MacOS X's (up to 10.2.8) preview app. implements this wrong description.
     */
    (*fontdict).as_dict_mut().set("DW", defaultAdvanceWidth);
    let w_array = w_array.into_obj();
    if empty == 0 {
        (*fontdict).as_dict_mut().set("W", pdf_ref_obj(w_array));
    }
    pdf_release_obj(w_array);
}
unsafe fn add_CIDVMetrics(
    sfont: *mut sfnt,
    fontdict: *mut pdf_obj,
    CIDToGIDMap: *mut u8,
    last_cid: u16,
    maxp: *mut tt_maxp_table,
    head: *mut tt_head_table,
    hmtx: *mut tt_longMetrics,
) {
    let mut vhea: *mut tt_vhea_table = ptr::null_mut();
    let mut vmtx: *mut tt_longMetrics = ptr::null_mut();
    let defaultAdvanceHeight;
    let mut empty: i32 = 1i32;
    /*
     * No accurate vertical metrics can be obtained by simple way if the
     * font does not have VORG table. Only CJK fonts may have VORG.
     */
    if sfnt_find_table_pos(sfont, b"VORG") <= 0_u32 {
        return;
    }
    let vorg = tt_read_VORG_table(sfont);
    let mut defaultVertOriginY = (1000.0f64 * (*vorg).defaultVertOriginY as i32 as f64
        / (*head).unitsPerEm as i32 as f64
        / 1i32 as f64
        + 0.5f64)
        .floor()
        * 1i32 as f64;
    if sfnt_find_table_pos(sfont, b"vhea") > 0_u32 {
        vhea = tt_read_vhea_table(sfont)
    }
    if !vhea.is_null() && sfnt_find_table_pos(sfont, b"vmtx") > 0_u32 {
        sfnt_locate_table(sfont, b"vmtx");
        vmtx = tt_read_longMetrics(
            sfont,
            (*maxp).numGlyphs,
            (*vhea).numOfLongVerMetrics,
            (*vhea).numOfExSideBearings,
        )
    }
    if sfnt_find_table_pos(sfont, sfnt_table_info::OS_2) <= 0_u32 {
        /* OpenType font must have OS/2 table. */
        let os2 = tt_read_os2__table(sfont);
        defaultVertOriginY = (1000.0f64 * (*os2).sTypoAscender as i32 as f64
            / (*head).unitsPerEm as i32 as f64
            / 1i32 as f64
            + 0.5f64)
            .floor()
            * 1i32 as f64;
        defaultAdvanceHeight = (1000.0f64
            * ((*os2).sTypoAscender as i32 - (*os2).sTypoDescender as i32) as f64
            / (*head).unitsPerEm as i32 as f64
            / 1i32 as f64
            + 0.5f64)
            .floor()
            * 1i32 as f64;
        free(os2 as *mut libc::c_void);
    } else {
        /* Some TrueType fonts used in Macintosh does not have OS/2 table. */
        defaultAdvanceHeight = 1000i32 as f64
    }
    let mut w2_array = vec![];
    for cid in 0..=last_cid as i32 {
        let gid = (if !CIDToGIDMap.is_null() {
            (*CIDToGIDMap.offset((2i32 * cid) as isize) as i32) << 8i32
                | *CIDToGIDMap.offset((2i32 * cid + 1i32) as isize) as i32
        } else {
            cid
        }) as u16;
        if !(gid as i32 >= (*maxp).numGlyphs as i32 || cid != 0i32 && gid as i32 == 0i32) {
            let advanceHeight = if !vmtx.is_null() {
                (1000.0f64 * (*vmtx.offset(gid as isize)).advance as i32 as f64
                    / (*head).unitsPerEm as i32 as f64
                    / 1i32 as f64
                    + 0.5f64)
                    .floor()
                    * 1i32 as f64
            } else {
                defaultAdvanceHeight
            };
            let vertOriginX = (1000.0f64
                * ((*hmtx.offset(gid as isize)).advance as i32 as f64 * 0.5f64)
                / (*head).unitsPerEm as i32 as f64
                / 1i32 as f64
                + 0.5f64)
                .floor()
                * 1i32 as f64;
            let mut vertOriginY = defaultVertOriginY;
            let mut i = 0;
            while (i as i32) < (*vorg).numVertOriginYMetrics as i32
                && gid as i32 > (*(*vorg).vertOriginYMetrics.offset(i as isize)).glyphIndex as i32
            {
                if gid as i32 == (*(*vorg).vertOriginYMetrics.offset(i as isize)).glyphIndex as i32
                {
                    vertOriginY = (1000.0f64
                        * (*(*vorg).vertOriginYMetrics.offset(i as isize)).vertOriginY as i32
                            as f64
                        / (*head).unitsPerEm as i32 as f64
                        / 1i32 as f64
                        + 0.5f64)
                        .floor()
                        * 1i32 as f64
                }
                i += 1;
            }
            /*
             * c_first c_last w1_y v_x v_y
             * This form may hit Acrobat's implementation limit of array element size, 8192.
             * AFPL GhostScript 8.11 stops with rangecheck error with this. Maybe GS's bug?
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
    if defaultVertOriginY != 880i32 as f64 || defaultAdvanceHeight != 1000i32 as f64 {
        let mut an_array = vec![];
        an_array.push_obj(defaultVertOriginY);
        an_array.push_obj(-defaultAdvanceHeight);
        (*fontdict).as_dict_mut().set("DW2", an_array.into_obj());
    }
    let w2_array = w2_array.into_obj();
    if empty == 0 {
        (*fontdict).as_dict_mut().set("W2", pdf_ref_obj(w2_array));
    }
    pdf_release_obj(w2_array);
    free((*vorg).vertOriginYMetrics as *mut libc::c_void);
    free(vorg as *mut libc::c_void);
    free(vmtx as *mut libc::c_void);
    free(vhea as *mut libc::c_void);
}
unsafe fn add_CIDMetrics(
    sfont: *mut sfnt,
    fontdict: *mut pdf_obj,
    CIDToGIDMap: *mut u8,
    last_cid: u16,
    need_vmetrics: i32,
) {
    /*
     * Read head, hhea, maxp:
     *
     *   unitsPerEm       --> head
     *   numHMetrics      --> hhea
     *   numGlyphs        --> maxp
     */
    let head = tt_read_head_table(sfont);
    let maxp = tt_read_maxp_table(sfont);
    let hhea = tt_read_hhea_table(sfont);
    sfnt_locate_table(sfont, sfnt_table_info::HMTX);
    let hmtx = tt_read_longMetrics(
        sfont,
        (*maxp).numGlyphs,
        (*hhea).numOfLongHorMetrics,
        (*hhea).numOfExSideBearings,
    );
    add_CIDHMetrics(fontdict, CIDToGIDMap, last_cid, maxp, head, hmtx);
    if need_vmetrics != 0 {
        add_CIDVMetrics(sfont, fontdict, CIDToGIDMap, last_cid, maxp, head, hmtx);
    }
    free(hmtx as *mut libc::c_void);
    free(hhea as *mut libc::c_void);
    free(maxp as *mut libc::c_void);
    free(head as *mut libc::c_void);
}
/*
 * Create an instance of embeddable font.
 */
unsafe fn write_fontfile(font: *mut CIDFont, cffont: &mut cff_font) -> i32 {
    /*  DICT sizes (offset set to long int) */
    let mut topdict = CffIndex::new(1); /* some bad font may have */
    let mut fdarray = CffIndex::new(cffont.num_fds as u16); /* some bad font may have */
    let mut private = CffIndex::new(cffont.num_fds as u16);
    cff_dict_remove(cffont.topdict, b"UniqueID\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"XUID\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"Private\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"Encoding\x00" as *const u8 as *const i8);
    topdict.offset[1] = (cff_dict_pack(cffont.topdict, &mut work_buffer[..]) + 1) as l_offset;
    for i in 0..cffont.num_fds as usize {
        let mut size = 0;
        if !cffont.private.is_null() && !(*cffont.private.offset(i as isize)).is_null() {
            size = cff_dict_pack(*cffont.private.offset(i as isize), &mut work_buffer[..]);
            if size < 1 {
                /* Private had contained only Subr */
                cff_dict_remove(
                    *cffont.fdarray.offset(i as isize),
                    b"Private\x00" as *const u8 as *const i8,
                ); /* header size */
            }
        } /* charset format 0 */
        private.offset[i + 1] = private.offset[i] + size as u32; /* fdselect format 3 */
        fdarray.offset[i + 1] = fdarray.offset[i]
            + (cff_dict_pack(*cffont.fdarray.offset(i as isize), &mut work_buffer[..]) as u32);
        /* Private is not INDEX */
    }
    let mut destlen = 4_usize;
    destlen += cff_set_name(cffont, (*font).fontname) as usize;
    destlen += topdict.size();
    destlen += cff_index_size(cffont.string);
    destlen += cff_index_size(cffont.gsubr);
    destlen += (*cffont.charsets).num_entries as usize * 2 + 1;
    destlen += (*cffont.fdselect).num_entries as usize * 3 + 5;
    destlen += cff_index_size(cffont.cstrings);
    destlen += fdarray.size();
    destlen = destlen + private.offset[private.count as usize] as usize - 1;

    let mut dest = vec![0u8; destlen];

    let mut offset = 0;
    /* Header */
    offset += cff_put_header(cffont, &mut dest[offset..offset + 4]);
    /* Name */
    offset += cff_pack_index(cffont.name, &mut dest[offset..]);
    /* Top DICT */
    let topdict_offset = offset;
    offset += topdict.size();
    /* Strings */
    offset += cff_pack_index(cffont.string, &mut dest[offset..]);
    /* Global Subrs */
    offset += cff_pack_index(cffont.gsubr, &mut dest[offset..]);
    /* charset */
    cff_dict_set(
        cffont.topdict,
        b"charset\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    offset += cff_pack_charsets(cffont, &mut dest[offset..]);
    /* FDSelect */
    cff_dict_set(
        cffont.topdict,
        b"FDSelect\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    offset += cff_pack_fdselect(cffont, &mut dest[offset..]);
    /* CharStrings */
    cff_dict_set(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    ); /* Charstrings cosumes huge memory */
    offset += cff_pack_index(
        cffont.cstrings,
        &mut dest[offset..offset + cff_index_size(cffont.cstrings)],
    );
    cff_release_index(cffont.cstrings);
    cffont.cstrings = ptr::null_mut();
    /* FDArray and Private */
    cff_dict_set(
        cffont.topdict,
        b"FDArray\x00" as *const u8 as *const i8,
        0i32,
        offset as f64,
    );
    let fdarray_offset = offset;
    offset += fdarray.size();
    fdarray.data = vec![0; (fdarray.offset[fdarray.count as usize]) as usize - 1];
    for i in 0..cffont.num_fds as usize {
        let size = (private.offset[i + 1] - private.offset[i]) as usize;
        if !(*cffont.private.offset(i as isize)).is_null() && size > 0 {
            cff_dict_pack(
                *cffont.private.offset(i as isize),
                &mut dest[offset..offset + size],
            );
            cff_dict_set(
                *cffont.fdarray.offset(i as isize),
                b"Private\x00" as *const u8 as *const i8,
                0i32,
                size as f64,
            );
            cff_dict_set(
                *cffont.fdarray.offset(i as isize),
                b"Private\x00" as *const u8 as *const i8,
                1i32,
                offset as f64,
            );
        }
        cff_dict_pack(
            *cffont.fdarray.offset(i as isize),
            &mut fdarray.data[fdarray.offset[i] as usize - 1..],
        );
        offset += size;
    }
    let len = fdarray.size();
    fdarray.pack(&mut dest[fdarray_offset..fdarray_offset + len]);
    /* Finally Top DICT */

    topdict.data = vec![0; (topdict.offset[topdict.count as usize]) as usize - 1];

    cff_dict_pack(cffont.topdict, &mut topdict.data[..]);
    let len = topdict.size();
    topdict.pack(&mut dest[topdict_offset..topdict_offset + len]);
    /*
     * FontFile
     */
    let fontfile = pdf_stream::new(STREAM_COMPRESS).into_obj();
    let stream_dict = (*fontfile).as_stream_mut().get_dict_mut();
    (*(*font).descriptor)
        .as_dict_mut()
        .set("FontFile3", pdf_ref_obj(fontfile));
    stream_dict.set("Subtype", "CIDFontType0C");
    (*fontfile)
        .as_stream_mut()
        .add_slice(&dest[..offset as usize]);
    pdf_release_obj(fontfile);
    destlen as i32
}
unsafe fn CIDFont_type0_get_used_chars(font: *mut CIDFont) -> *mut i8 {
    let mut parent_id = CIDFont_get_parent_id(font, 0i32);
    if parent_id < 0i32 && {
        parent_id = CIDFont_get_parent_id(font, 1i32);
        parent_id < 0i32
    } {
        panic!("No parent Type 0 font !");
    }
    let used_chars = Type0Font_get_usedchars(Type0Font_cache_get(parent_id));
    if used_chars.is_null() {
        panic!("Unexpected error: Font not actually used???");
    }
    used_chars
}
unsafe fn CIDType0Error_Show(error: CidOpenError, name: *const i8) {
    match error {
        CidOpenError::CANNOT_OPEN_FILE => {
            panic!(
                "Could not open OpenType font file: {}",
                CStr::from_ptr(name).display()
            );
        }
        CidOpenError::NOT_SFNT_FONT => {
            panic!(
                "Could not open SFNT font file: {}",
                CStr::from_ptr(name).display()
            );
        }
        CidOpenError::NO_CFF_TABLE => {
            panic!(
                "Not a CFF/OpenType font: {}",
                CStr::from_ptr(name).display()
            );
        }
        CidOpenError::CANNOT_OPEN_CFF_FONT => {
            panic!(
                "Could not open CFF font: {}",
                CStr::from_ptr(name).display()
            );
        }
        CidOpenError::NOT_CIDFONT => {
            panic!("Not a CIDFont: {}", CStr::from_ptr(name).display());
        }
        CidOpenError::IS_CIDFONT => {
            panic!(
                "Should not be a CIDFont: {}",
                CStr::from_ptr(name).display()
            );
        }
    };
}
unsafe fn CIDFontInfo_init(info: *mut CIDType0Info) {
    memset(
        info as *mut libc::c_void,
        0i32,
        ::std::mem::size_of::<CIDType0Info>(),
    );
}
unsafe fn CIDFontInfo_close(info: *mut CIDType0Info) {
    if !(*info).cffont.is_null() {
        cff_close((*info).cffont);
    }
    if !(*info).sfont.is_null() {
        sfnt_close((*info).sfont);
    }
    /*if let Some(handle) = (*info).handle {
        ttstub_input_close(handle);
    }*/
    CIDFontInfo_init(info);
}
unsafe fn CIDFont_type0_try_open(
    name: *const i8,
    index: i32,
    required_cid: i32,
    mut info: *mut CIDType0Info,
) -> Result<(), CidOpenError> {
    let mut offset: u32 = 0_u32;
    CIDFontInfo_init(info);
    let handle = dpx_open_opentype_file(name).or_else(|| dpx_open_truetype_file(name));
    if handle.is_none() {
        return Err(CidOpenError::CANNOT_OPEN_FILE);
    }
    (*info).sfont = sfnt_open(handle.unwrap());
    if (*info).sfont.is_null() {
        return Err(CidOpenError::NOT_SFNT_FONT);
    }
    if (*(*info).sfont).type_0 == 1i32 << 4i32 {
        offset = ttc_read_offset((*info).sfont, index)
    }
    if (*(*info).sfont).type_0 != 1i32 << 4i32 && (*(*info).sfont).type_0 != 1i32 << 2i32
        || sfnt_read_table_directory((*info).sfont, offset) < 0i32
        || {
            offset = sfnt_find_table_pos((*info).sfont, b"CFF ");
            offset == 0_u32
        }
    {
        CIDFontInfo_close(info);
        return Err(CidOpenError::NO_CFF_TABLE);
    }
    (*info).cffont = cff_open(&mut (*(*info).sfont).handle, offset as i32, 0i32); // TODO: use link
    if (*info).cffont.is_null() {
        return Err(CidOpenError::CANNOT_OPEN_CFF_FONT);
    }
    let is_cid = (*(*info).cffont).flag & 1i32 << 0i32;
    if required_cid != is_cid {
        CIDFontInfo_close(info);
        return Err(if required_cid != 0 {
            CidOpenError::NOT_CIDFONT
        } else {
            CidOpenError::IS_CIDFONT
        });
    }
    Ok(())
}
unsafe fn CIDFont_type0_add_CIDSet(font: *mut CIDFont, used_chars: *mut i8, last_cid: u16) {
    /*
     * CIDSet:
     * Length of CIDSet stream is not clear. Must be 8192 bytes long?
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
}

pub unsafe fn CIDFont_type0_dofont(font: *mut CIDFont) {
    let mut num_glyphs: u16 = 0i32 as u16;
    let mut last_cid: u16 = 0i32 as u16;
    let mut CIDToGIDMap: *mut u8 = ptr::null_mut();
    let mut info: CIDType0Info = CIDType0Info {
        sfont: ptr::null_mut(),
        cffont: ptr::null_mut(),
    };
    assert!(!font.is_null());
    if (*font).indirect.is_null() {
        return;
    }
    (*(*font).fontdict)
        .as_dict_mut()
        .set("FontDescriptor", pdf_ref_obj((*font).descriptor));
    if CIDFont_is_BaseFont(font) {
        return;
    } else {
        if CIDFont_get_embedding(font) == 0 && opt_flags & 1i32 << 1i32 != 0 {
            /* No metrics needed. */
            (*(*font).fontdict).as_dict_mut().set("DW", 1000_f64);
            return;
        }
    }
    let used_chars = CIDFont_type0_get_used_chars(font);
    if let Err(error) =
        CIDFont_type0_try_open((*font).ident, CIDFont_get_opt_index(font), 1i32, &mut info)
    {
        CIDType0Error_Show(error, (*font).ident);
        return;
    }
    let mut cffont = &mut *info.cffont;
    cff_read_charsets(cffont);
    /*
     * DW, W, DW2 and W2:
     * Those values are obtained from OpenType table (not TFM).
     */
    if opt_flags & 1i32 << 1i32 != 0 {
        (*(*font).fontdict).as_dict_mut().set("DW", 1000_f64);
    } else {
        let cid_count =
            if cff_dict_known((*cffont).topdict, b"CIDCount\x00" as *const u8 as *const i8) != 0 {
                cff_dict_get(
                    (*cffont).topdict,
                    b"CIDCount\x00" as *const u8 as *const i8,
                    0i32,
                ) as i32
            } else {
                65535i32 + 1i32
            };
        CIDToGIDMap = new(((2i32 * cid_count) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        memset(CIDToGIDMap as *mut libc::c_void, 0i32, (2 * cid_count) as _);
        let ref mut fresh0 = *used_chars.offset((0i32 / 8i32) as isize);
        *fresh0 = (*fresh0 as i32 | 1i32 << 7i32 - 0i32 % 8i32) as i8;
        /* .notdef */
        for cid in 0..=65535 {
            if *used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 != 0 {
                let gid = cff_charsets_lookup(cffont, cid as u16);
                if cid != 0i32 && gid as i32 == 0i32 {
                    warn!(
                        "Glyph for CID {} missing in font \"{}\".",
                        cid as CID,
                        CStr::from_ptr((*font).ident).display(),
                    );
                    let ref mut fresh1 = *used_chars.offset((cid / 8i32) as isize);
                    *fresh1 = (*fresh1 as i32 & !(1i32 << 7i32 - cid % 8i32)) as i8
                } else {
                    *CIDToGIDMap.offset((2i32 * cid) as isize) =
                        (gid as i32 >> 8i32 & 0xffi32) as u8;
                    *CIDToGIDMap.offset((2i32 * cid + 1i32) as isize) =
                        (gid as i32 & 0xffi32) as u8;
                    last_cid = cid as u16;
                    num_glyphs = num_glyphs.wrapping_add(1)
                }
            }
        }
        add_CIDMetrics(
            info.sfont,
            (*font).fontdict,
            CIDToGIDMap,
            last_cid,
            if CIDFont_get_parent_id(font, 1i32) < 0i32 {
                0i32
            } else {
                1i32
            },
        );
    }
    if CIDFont_get_embedding(font) == 0 {
        free(CIDToGIDMap as *mut libc::c_void);
        CIDFontInfo_close(&mut info);
        return;
    }
    /*
     * Embed font subset.
     */
    cff_read_fdselect(cffont);
    cff_read_fdarray(cffont);
    cff_read_private(cffont);
    cff_read_subrs(cffont);
    let offset = cff_dict_get(
        (*cffont).topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
    ) as u64;
    cffont
        .handle
        .as_mut()
        .unwrap()
        .seek(SeekFrom::Start((*cffont).offset as u64 + offset))
        .unwrap();
    let idx = cff_get_index_header(cffont);
    /* offset is now absolute offset ... bad */
    let offset = cffont
        .handle
        .as_mut()
        .unwrap()
        .seek(SeekFrom::Current(0))
        .unwrap();
    let cs_count = (*idx).count;
    if (cs_count as i32) < 2i32 {
        panic!("No valid charstring data found.");
    }
    /* New Charsets data */
    let charset = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    (*charset).format = 0i32 as u8;
    (*charset).num_entries = 0i32 as u16;
    (*charset).data.glyphs = new((num_glyphs as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<s_SID>() as u64)
        as u32) as *mut s_SID;
    /* New FDSelect data */
    let fdselect = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_fdselect>() as u64) as u32)
        as *mut cff_fdselect;
    (*fdselect).format = 3i32 as u8;
    (*fdselect).num_entries = 0i32 as u16;
    (*fdselect).data.ranges = new((num_glyphs as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<cff_range3>() as u64)
        as u32) as *mut cff_range3;
    /* New CharStrings INDEX */
    let charstrings = cff_new_index((num_glyphs as i32 + 1i32) as u16);
    let mut max_len = 2 * 65536;
    (*charstrings).data =
        new((max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
    let mut charstring_len = 0i32;
    /*
     * TODO: Re-assign FD number.
     */
    let mut prev_fd = -1;
    let mut gid = 0_u16;
    let data = new((65536_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 == 0) {
            let gid_org = ((*CIDToGIDMap.offset((2i32 * cid) as isize) as i32) << 8i32
                | *CIDToGIDMap.offset((2i32 * cid + 1i32) as isize) as i32)
                as u16;
            let size = (*(*idx).offset.offset((gid_org as i32 + 1i32) as isize))
                .wrapping_sub(*(*idx).offset.offset(gid_org as isize))
                as i32;
            if size > 65536i32 {
                panic!("Charstring too long: gid={}", gid_org);
            }
            if charstring_len + 65536i32 >= max_len {
                max_len = charstring_len + 2i32 * 65536i32;
                (*charstrings).data = renew(
                    (*charstrings).data as *mut libc::c_void,
                    (max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
                ) as *mut u8
            }
            *(*charstrings).offset.offset(gid as isize) = (charstring_len + 1i32) as l_offset;
            let handle = cffont.handle.as_mut().unwrap();
            handle
                .seek(SeekFrom::Start(
                    offset as u64 + *(*idx).offset.offset(gid_org as isize) as u64 - 1,
                ))
                .unwrap();
            ttstub_input_read(handle.0.as_ptr(), data as *mut i8, size as size_t);
            let fd = cff_fdselect_lookup(cffont, gid_org) as i32;
            charstring_len += cs_copy_charstring(
                (*charstrings).data.offset(charstring_len as isize),
                max_len - charstring_len,
                data,
                size,
                (*cffont).gsubr,
                *(*cffont).subrs.offset(fd as isize),
                0i32 as f64,
                0i32 as f64,
                ptr::null_mut(),
            );
            if cid > 0i32 && gid_org as i32 > 0i32 {
                *(*charset)
                    .data
                    .glyphs
                    .offset((*charset).num_entries as isize) = cid as s_SID;
                (*charset).num_entries = ((*charset).num_entries as i32 + 1i32) as u16
            }
            if fd != prev_fd {
                (*(*fdselect)
                    .data
                    .ranges
                    .offset((*fdselect).num_entries as isize))
                .first = gid;
                (*(*fdselect)
                    .data
                    .ranges
                    .offset((*fdselect).num_entries as isize))
                .fd = fd as u8;
                (*fdselect).num_entries = ((*fdselect).num_entries as i32 + 1i32) as u16;
                prev_fd = fd
            }
            gid = gid.wrapping_add(1)
        }
    }
    if gid as i32 != num_glyphs as i32 {
        panic!("Unexpeced error: ?????");
    }
    free(data as *mut libc::c_void);
    cff_release_index(idx);
    free(CIDToGIDMap as *mut libc::c_void);
    *(*charstrings).offset.offset(num_glyphs as isize) = (charstring_len + 1i32) as l_offset;
    (*charstrings).count = num_glyphs;
    (*cffont).num_glyphs = num_glyphs;
    (*cffont).cstrings = charstrings;
    /* discard old one, set new data */
    cff_release_charsets((*cffont).charsets);
    (*cffont).charsets = charset;
    cff_release_fdselect((*cffont).fdselect);
    (*cffont).fdselect = fdselect;
    /* no Global subr */
    if !(*cffont).gsubr.is_null() {
        cff_release_index((*cffont).gsubr);
    }
    (*cffont).gsubr = cff_new_index(0i32 as u16);
    for fd in 0..(*cffont).num_fds as i32 {
        if !(*cffont).subrs.is_null() && !(*(*cffont).subrs.offset(fd as isize)).is_null() {
            cff_release_index(*(*cffont).subrs.offset(fd as isize));
            let ref mut fresh2 = *(*cffont).subrs.offset(fd as isize);
            *fresh2 = ptr::null_mut()
        }
        if !(*cffont).private.is_null() && !(*(*cffont).private.offset(fd as isize)).is_null() {
            cff_dict_remove(
                *(*cffont).private.offset(fd as isize),
                b"Subrs\x00" as *const u8 as *const i8,
            );
            /* no Subrs */
        }
    }
    let destlen = write_fontfile(font, cffont);
    CIDFontInfo_close(&mut info);
    if verbose > 1i32 {
        info!("[{}/{} glyphs][{} bytes]", num_glyphs, cs_count, destlen,);
    }
    CIDFont_type0_add_CIDSet(font, used_chars, last_cid);
}

pub unsafe fn CIDFont_type0_open(
    mut font: *mut CIDFont,
    name: *const i8,
    cmap_csi: *mut CIDSysInfo,
    mut opt: *mut cid_opt,
    expected_flag: i32,
) -> i32 {
    let mut sfont: *mut sfnt = ptr::null_mut();
    let cffont;
    let mut offset: u32 = 0_u32;
    let mut is_cid_font: i32 = 0i32;
    let expect_cid_font: i32 = (expected_flag == 0i32) as i32;
    let expect_type1_font: i32 = expected_flag & 1i32 << 8i32;
    assert!(!font.is_null());
    if expect_type1_font != 0 {
        if !cmap_csi.is_null()
            && (strcmp((*cmap_csi).registry, b"Adobe\x00" as *const u8 as *const i8) != 0i32
                || strcmp(
                    (*cmap_csi).ordering,
                    b"Identity\x00" as *const u8 as *const i8,
                ) != 0i32)
        {
            return -1i32;
        }
    }
    let handle = if expect_type1_font != 0 {
        dpx_open_type1_file(name)
    } else {
        dpx_open_opentype_file(name)
    };
    if expect_type1_font == 0 {
        sfont = if let Some(handle) = handle.or_else(|| dpx_open_truetype_file(name)) {
            sfnt_open(handle)
        } else {
            return -1i32;
        };
        if sfont.is_null() {
            panic!(
                "Not a CFF/OpenType font: {}",
                CStr::from_ptr(name).display()
            );
        }
        if (*sfont).type_0 == 1i32 << 4i32 {
            offset = ttc_read_offset(sfont, (*opt).index)
        }
        if (*sfont).type_0 != 1i32 << 4i32 && (*sfont).type_0 != 1i32 << 2i32
            || sfnt_read_table_directory(sfont, offset) < 0i32
            || {
                offset = sfnt_find_table_pos(sfont, b"CFF ");
                offset == 0_u32
            }
        {
            sfnt_close(sfont);
            return -1i32;
        }
        cffont = cff_open(&mut (*sfont).handle, offset as i32, 0i32); // TODO: use link
        if cffont.is_null() {
            panic!("Cannot read CFF font data");
        }
        let cffont = &mut *cffont;
        is_cid_font = cffont.flag & 1i32 << 0i32;
        if expect_cid_font != is_cid_font {
            cff_close(cffont);
            sfnt_close(sfont);
            return -1i32;
        }
        if is_cid_font != 0 {
            cff_read_charsets(cffont);
            (*opt).cff_charsets = cffont.charsets as *mut libc::c_void;
            cffont.charsets = ptr::null_mut()
        }
    } else {
        if handle.is_none() {
            return -1i32;
        }
        let handle = handle.unwrap();
        cffont = t1_load_font(0 as *mut *mut i8, 1i32, handle);
        if cffont.is_null() {
            return -1i32;
        }
    }
    let csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
        as *mut CIDSysInfo;
    if is_cid_font != 0 {
        (*csi).registry = cff_get_string(
            cffont,
            cff_dict_get(
                (*cffont).topdict,
                b"ROS\x00" as *const u8 as *const i8,
                0i32,
            ) as s_SID,
        );
        (*csi).ordering = cff_get_string(
            cffont,
            cff_dict_get(
                (*cffont).topdict,
                b"ROS\x00" as *const u8 as *const i8,
                1i32,
            ) as s_SID,
        );
        (*csi).supplement = cff_dict_get(
            (*cffont).topdict,
            b"ROS\x00" as *const u8 as *const i8,
            2i32,
        ) as i32
    } else {
        (*csi).registry = new(
            (strlen(b"Adobe\x00" as *const u8 as *const i8).wrapping_add(1))
                .wrapping_mul(::std::mem::size_of::<i8>()) as _,
        ) as *mut i8;
        strcpy((*csi).registry, b"Adobe\x00" as *const u8 as *const i8);
        (*csi).ordering = new(
            (strlen(b"Identity\x00" as *const u8 as *const i8).wrapping_add(1))
                .wrapping_mul(::std::mem::size_of::<i8>()) as _,
        ) as *mut i8;
        strcpy((*csi).ordering, b"Identity\x00" as *const u8 as *const i8);
        (*csi).supplement = 0i32
    }
    if expect_type1_font == 0 && !cmap_csi.is_null() {
        if strcmp((*csi).registry, (*cmap_csi).registry) != 0i32
            || strcmp((*csi).ordering, (*cmap_csi).ordering) != 0i32
        {
            info!("\nCharacter collection mismatched:\n");
            info!(
                "\tFont: {}-{}-{}\n",
                CStr::from_ptr((*csi).registry).display(),
                CStr::from_ptr((*csi).ordering).display(),
                (*csi).supplement,
            );
            info!(
                "\tCMap: {}-{}-{}\n",
                CStr::from_ptr((*cmap_csi).registry).display(),
                CStr::from_ptr((*cmap_csi).ordering).display(),
                (*cmap_csi).supplement,
            );
            panic!("Inconsistent CMap specified for this font.");
        }
        if (*csi).supplement < (*cmap_csi).supplement {
            warn!("CMap have higher supplmement number.");
            warn!("Some characters may not be displayed or printed.");
        }
    }
    let mut fontname_len: i32 = 8i32;
    let shortname = cff_get_name(&*cffont);
    if shortname.is_null() {
        panic!("No valid FontName found.");
    }
    /*
     * Mangled name requires more 7 bytes.
     * Style requires more 11 bytes.
     */
    if is_cid_font != 0 {
        fontname_len += 11i32
    }
    let fontname = new((strlen(shortname).wrapping_add(fontname_len as _))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    memset(
        fontname as *mut libc::c_void,
        0i32,
        strlen(shortname).wrapping_add(fontname_len as _) as _,
    );
    strcpy(fontname, shortname);
    free(shortname as *mut libc::c_void);
    cff_close(cffont);
    if is_cid_font != 0 {
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
    } else if expect_type1_font != 0 {
        if (*opt).style != 0i32 {
            warn!(",Bold, ,Italic, ... not supported for this type of font...");
            (*opt).style = 0i32
        }
    } else {
        (*opt).embed = 1i32
    }
    (*font).fontname = fontname;
    (*font).subtype = 1i32;
    (*font).csi = csi;
    (*font).flags |= expected_flag;
    (*font).fontdict = pdf_dict::new().into_obj();
    (*(*font).fontdict).as_dict_mut().set("Type", "Font");
    (*(*font).fontdict)
        .as_dict_mut()
        .set("Subtype", "CIDFontType0");
    if expect_type1_font != 0 || (*opt).embed != 0 {
        memmove(
            fontname.offset(7) as *mut libc::c_void,
            fontname as *const libc::c_void,
            strlen(fontname).wrapping_add(1),
        );
        pdf_font_make_uniqueTag(fontname);
        *fontname.offset(6) = '+' as i32 as i8
    }
    if expect_type1_font != 0 {
        (*font).descriptor = pdf_dict::new().into_obj()
    } else {
        /* getting font info. from TrueType tables */
        (*font).descriptor = tt_get_fontdesc(sfont, &mut (*opt).embed, (*opt).stemv, 0i32, name);
        if (*font).descriptor.is_null() {
            panic!("Could not obtain necessary font info.");
        }
    }
    (*(*font).descriptor)
        .as_dict_mut()
        .set("FontName", pdf_copy_name(fontname));
    (*(*font).fontdict)
        .as_dict_mut()
        .set("BaseFont", pdf_copy_name(fontname));
    let mut csi_dict = pdf_dict::new();
    csi_dict.set(
        "Registry",
        pdf_new_string(
            (*csi).registry as *const libc::c_void,
            strlen((*csi).registry) as _,
        ),
    );
    csi_dict.set(
        "Ordering",
        pdf_new_string(
            (*csi).ordering as *const libc::c_void,
            strlen((*csi).ordering) as _,
        ),
    );
    csi_dict.set("Supplement", (*csi).supplement as f64);
    (*(*font).fontdict)
        .as_dict_mut()
        .set("CIDSystemInfo", csi_dict.into_obj());
    if is_cid_font != 0 {
        (*(*font).fontdict).as_dict_mut().set("DW", 1000_f64);
        /* not sure */
    }
    if expect_type1_font == 0 {
        sfnt_close(sfont);
    }
    0i32
}

pub unsafe fn CIDFont_type0_t1cdofont(font: *mut CIDFont) {
    let mut info: CIDType0Info = CIDType0Info {
        sfont: ptr::null_mut(),
        cffont: ptr::null_mut(),
    };
    assert!(!font.is_null());
    if (*font).indirect.is_null() {
        return;
    }
    (*(*font).fontdict)
        .as_dict_mut()
        .set("FontDescriptor", pdf_ref_obj((*font).descriptor));
    let used_chars = CIDFont_type0_get_used_chars(font);
    if let Err(error) =
        CIDFont_type0_try_open((*font).ident, CIDFont_get_opt_index(font), 0i32, &mut info)
    {
        CIDType0Error_Show(error, (*font).ident);
        return;
    }
    let cffont = &mut *info.cffont;
    cff_read_private(cffont);
    cff_read_subrs(cffont);
    if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"StdVW\x00" as *const u8 as *const i8,
        ) != 0
    {
        let stemv = cff_dict_get(
            *cffont.private.offset(0),
            b"StdVW\x00" as *const u8 as *const i8,
            0i32,
        );
        (*(*font).descriptor).as_dict_mut().set("StemV", stemv);
    }
    let default_width = if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
        ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let nominal_width = if !(*cffont.private.offset(0)).is_null()
        && cff_dict_known(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
        ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let mut num_glyphs = 0_u16;
    let mut last_cid = 0_u16;
    let ref mut fresh3 = *used_chars.offset((0i32 / 8i32) as isize);
    *fresh3 = (*fresh3 as i32 | 1i32 << 7i32 - 0i32 % 8i32) as i8;
    /* .notdef */
    for i in 0..(cffont.num_glyphs as i32 + 7i32) / 8i32 {
        let c = *used_chars.offset(i as isize) as i32;
        for j in (0..8).rev() {
            if c & 1i32 << j != 0 {
                num_glyphs += 1;
                last_cid = ((i + 1i32) * 8i32 - j - 1i32) as u16
            }
        }
    }
    let fdselect = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_fdselect>() as u64) as u32)
        as *mut cff_fdselect;
    (*fdselect).format = 3i32 as u8;
    (*fdselect).num_entries = 1i32 as u16;
    (*fdselect).data.ranges =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cff_range3>() as u64) as u32)
            as *mut cff_range3;
    (*(*fdselect).data.ranges.offset(0)).first = 0i32 as u16;
    (*(*fdselect).data.ranges.offset(0)).fd = 0i32 as u8;
    cffont.fdselect = fdselect;
    let charset = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    (*charset).format = 0i32 as u8;
    (*charset).num_entries = (num_glyphs as i32 - 1i32) as u16;
    (*charset).data.glyphs = new(((num_glyphs as i32 - 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<s_SID>() as u64)
        as u32) as *mut s_SID;
    let mut gid = 0_u16;
    for cid in 0..=last_cid as i32 {
        if *used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 != 0 {
            if gid as i32 > 0i32 {
                *(*charset).data.glyphs.offset((gid as i32 - 1i32) as isize) = cid as s_SID
            }
            gid += 1;
        }
    }
    /* cff_release_charsets(cffont->charsets); */
    cffont.charsets = charset; /* FIXME: Skip XXXXXX+ */
    cff_dict_add(
        cffont.topdict,
        b"CIDCount\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"CIDCount\x00" as *const u8 as *const i8,
        0i32,
        (last_cid as i32 + 1i32) as f64,
    );
    cffont.fdarray = new((1_u64).wrapping_mul(::std::mem::size_of::<*mut cff_dict>() as u64) as u32)
        as *mut *mut cff_dict;
    let ref mut fresh4 = *cffont.fdarray.offset(0);
    *fresh4 = cff_new_dict();
    cff_dict_add(
        *cffont.fdarray.offset(0),
        b"FontName\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"FontName\x00" as *const u8 as *const i8,
        0i32,
        cff_add_string(cffont, (*font).fontname.offset(7), 1i32) as f64,
    );
    cff_dict_add(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        2i32,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    /* FDArray  - index offset, not known yet */
    cff_dict_add(
        cffont.topdict,
        b"FDArray\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"FDArray\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    /* FDSelect - offset, not known yet */
    cff_dict_add(
        cffont.topdict,
        b"FDSelect\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"FDSelect\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    cff_dict_remove(cffont.topdict, b"UniqueID\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"XUID\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"Private\x00" as *const u8 as *const i8);
    cff_dict_remove(cffont.topdict, b"Encoding\x00" as *const u8 as *const i8);
    /* */
    let offset = cff_dict_get(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
    ) as i32;
    cffont
        .handle
        .as_mut()
        .unwrap()
        .seek(SeekFrom::Start(cffont.offset as u64 + offset as u64))
        .unwrap();
    let idx = cff_get_index_header(cffont);
    /* offset is now absolute offset ... bad */
    let offset = cffont
        .handle
        .as_mut()
        .unwrap()
        .seek(SeekFrom::Current(0))
        .unwrap();
    if ((*idx).count as i32) < 2i32 {
        panic!("No valid charstring data found.");
    }
    /* New CharStrings INDEX */
    let charstrings = cff_new_index((num_glyphs as i32 + 1i32) as u16);
    let mut max_len = 2i32 * 65536i32;
    (*charstrings).data =
        new((max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
    let mut charstring_len = 0;
    gid = 0i32 as u16;
    let data = new((65536_u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    for cid in 0..=last_cid as i32 {
        if !(*used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 == 0) {
            let size = (*(*idx).offset.offset((cid + 1i32) as isize))
                .wrapping_sub(*(*idx).offset.offset(cid as isize)) as i32;
            if size > 65536i32 {
                panic!("Charstring too long: gid={}", cid);
            }
            if charstring_len + 65536i32 >= max_len {
                max_len = charstring_len + 2i32 * 65536i32;
                (*charstrings).data = renew(
                    (*charstrings).data as *mut libc::c_void,
                    (max_len as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
                ) as *mut u8
            }
            *(*charstrings).offset.offset(gid as isize) = (charstring_len + 1i32) as l_offset;
            let handle = cffont.handle.as_mut().unwrap();
            handle
                .seek(SeekFrom::Start(
                    offset as u64 + *(*idx).offset.offset(cid as isize) as u64 - 1,
                ))
                .unwrap();
            ttstub_input_read(handle.0.as_ptr(), data as *mut i8, size as size_t);
            charstring_len += cs_copy_charstring(
                (*charstrings).data.offset(charstring_len as isize),
                max_len - charstring_len,
                data,
                size,
                cffont.gsubr,
                *cffont.subrs.offset(0),
                default_width,
                nominal_width,
                ptr::null_mut(),
            );
            gid = gid.wrapping_add(1)
        }
    }
    if gid as i32 != num_glyphs as i32 {
        panic!("Unexpeced error: ?????");
    }
    free(data as *mut libc::c_void);
    cff_release_index(idx);
    *(*charstrings).offset.offset(num_glyphs as isize) = (charstring_len + 1i32) as l_offset;
    (*charstrings).count = num_glyphs;
    cffont.num_glyphs = num_glyphs;
    cffont.cstrings = charstrings;
    /* no Global subr */
    if !cffont.gsubr.is_null() {
        cff_release_index(cffont.gsubr);
    }
    cffont.gsubr = cff_new_index(0i32 as u16);
    if !cffont.subrs.is_null() && !(*cffont.subrs.offset(0)).is_null() {
        cff_release_index(*cffont.subrs.offset(0));
        let ref mut fresh5 = *cffont.subrs.offset(0);
        *fresh5 = ptr::null_mut()
    }
    if !cffont.private.is_null() && !(*cffont.private.offset(0)).is_null() {
        cff_dict_remove(
            *cffont.private.offset(0),
            b"Subrs\x00" as *const u8 as *const i8,
        );
        /* no Subrs */
    }
    cff_add_string(cffont, b"Adobe\x00" as *const u8 as *const i8, 1i32);
    cff_add_string(cffont, b"Identity\x00" as *const u8 as *const i8, 1i32);
    cff_dict_update(cffont.topdict, cffont);
    cff_dict_update(*cffont.private.offset(0), cffont);
    cff_update_string(cffont);
    /* CFF code need to be rewrote... */
    cff_dict_add(cffont.topdict, b"ROS\x00" as *const u8 as *const i8, 3i32);
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        0i32,
        cff_get_sid(cffont, b"Adobe\x00" as *const u8 as *const i8) as f64,
    );
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        1i32,
        cff_get_sid(cffont, b"Identity\x00" as *const u8 as *const i8) as f64,
    );
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        2i32,
        0.0f64,
    );
    let destlen = write_fontfile(font, cffont);
    /*
     * DW, W, DW2 and W2:
     * Those values are obtained from OpenType table (not TFM).
     */
    let CIDToGIDMap = new(((2i32 * (last_cid as i32 + 1i32)) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memset(
        CIDToGIDMap as *mut libc::c_void,
        0i32,
        (2 * (last_cid + 1)) as _,
    );
    for cid in 0..=last_cid as i32 {
        if *used_chars.offset((cid / 8i32) as isize) as i32 & 1i32 << 7i32 - cid % 8i32 != 0 {
            *CIDToGIDMap.offset((2i32 * cid) as isize) = (cid >> 8i32 & 0xffi32) as u8;
            *CIDToGIDMap.offset((2i32 * cid + 1i32) as isize) = (cid & 0xffi32) as u8
        }
    }
    add_CIDMetrics(
        info.sfont,
        (*font).fontdict,
        CIDToGIDMap,
        last_cid,
        if CIDFont_get_parent_id(font, 1i32) < 0i32 {
            0i32
        } else {
            1i32
        },
    );
    free(CIDToGIDMap as *mut libc::c_void);
    CIDFontInfo_close(&mut info);
    if verbose > 1i32 {
        info!("[{} glyphs][{} bytes]", num_glyphs, destlen);
    }
    CIDFont_type0_add_CIDSet(font, used_chars, last_cid);
}

unsafe fn load_base_CMap(font_name: &str, wmode: i32, cffont: &cff_font) -> i32 {
    let mut range_min: [u8; 4] = [0; 4];
    let mut range_max: [u8; 4] = [0x7f, 0xff, 0xff, 0xff];
    let cmap_name = if wmode != 0 {
        format!("{}-UCS4-V", font_name)
    } else {
        format!("{}-UCS4-H", font_name)
    };
    let cmap_id = CMap_cache_find(&cmap_name);
    if cmap_id >= 0i32 {
        return cmap_id;
    }
    let cmap = CMap_new();
    CMap_set_name(cmap, &cmap_name);
    CMap_set_type(cmap, 1i32);
    CMap_set_wmode(cmap, wmode);
    CMap_add_codespacerange(
        cmap,
        range_min.as_mut_ptr(),
        range_max.as_mut_ptr(),
        4i32 as size_t,
    );
    CMap_set_CIDSysInfo(cmap, &mut CSI_IDENTITY);
    for gid in 1..cffont.num_glyphs as u16 {
        let sid = cff_charsets_lookup_inverse(cffont, gid);
        let glyph = cff_get_string(cffont, sid);
        if let (Some(name), None) = agl_chop_suffix(CStr::from_ptr(glyph).to_bytes()) {
            if agl_name_is_unicode(name.to_bytes()) {
                let ucv = agl_name_convert_unicode(name.as_ptr());
                let mut srcCode = ucv.to_be_bytes();
                CMap_add_cidchar(cmap, srcCode.as_mut_ptr(), 4i32 as size_t, gid);
            } else {
                let mut agln = agl_lookup_list(name.as_ptr());
                if agln.is_null() {
                    warn!(
                        "Glyph \"{}\" inaccessible (no Unicode mapping)",
                        CStr::from_ptr(glyph).display()
                    );
                }
                while !agln.is_null() {
                    if (*agln).n_components > 1i32 {
                        warn!(
                            "Glyph \"{}\" inaccessible (composite character)",
                            CStr::from_ptr(glyph).display()
                        );
                    } else if (*agln).n_components == 1i32 {
                        let ucv = (*agln).unicodes[0];
                        let mut srcCode = ucv.to_be_bytes();
                        CMap_add_cidchar(cmap, srcCode.as_mut_ptr(), 4i32 as size_t, gid);
                    }
                    agln = (*agln).alternate
                }
            }
            free(glyph as *mut libc::c_void);
        } else {
            free(glyph as *mut libc::c_void);
        }
    }
    CMap_cache_add(cmap)
}

pub unsafe fn t1_load_UnicodeCMap(font_name: *const i8, otl_tags: *const i8, wmode: i32) -> i32 {
    if font_name.is_null() {
        return -1i32;
    }
    let handle = dpx_open_type1_file(font_name);
    if handle.is_none() {
        return -1i32;
    }
    let handle = handle.unwrap();
    let cffont = t1_load_font(0 as *mut *mut i8, 1i32, handle);
    if cffont.is_null() {
        return -1i32;
    }
    let cmap_id = load_base_CMap(
        &CStr::from_ptr(font_name).to_string_lossy(),
        wmode,
        &*cffont,
    );
    cff_close(cffont);
    if cmap_id < 0i32 {
        panic!(
            "Failed to create Unicode charmap for font \"{}\".",
            CStr::from_ptr(font_name).display()
        );
    }
    if !otl_tags.is_null() {
        warn!("Glyph substitution not supported for Type1 font yet...");
    }
    cmap_id
}
/*
 * ToUnicode CMap
 */
unsafe fn create_ToUnicode_stream(
    cffont: &cff_font,
    font_name: *const i8,
    used_glyphs: *const i8,
) -> Option<pdf_stream> {
    let mut stream = None;
    let mut wbuf: [u8; 1024] = [0; 1024];
    static mut range_min: [u8; 2] = [0; 2];
    static mut range_max: [u8; 2] = [0xff, 0xff];
    if font_name.is_null() || used_glyphs.is_null() {
        return None;
    }
    let cmap = CMap_new();
    CMap_set_name(
        cmap,
        &format!("{}-UTF16", CStr::from_ptr(font_name).display()),
    );
    CMap_set_wmode(cmap, 0i32);
    CMap_set_type(cmap, 2i32);
    CMap_set_CIDSysInfo(cmap, &mut CSI_UNICODE);
    CMap_add_codespacerange(
        cmap,
        range_min.as_mut_ptr(),
        range_max.as_mut_ptr(),
        2i32 as size_t,
    );
    let mut total_fail_count = 0i32;
    let mut glyph_count = total_fail_count;
    //p = wbuf.as_mut_ptr();
    let endptr = wbuf.as_mut_ptr().offset(1024);
    for cid in 1..cffont.num_glyphs as CID {
        /* Skip .notdef */
        if *used_glyphs.offset((cid as i32 / 8i32) as isize) as i32
            & 1i32 << 7i32 - cid as i32 % 8i32
            != 0
        {
            let mut fail_count: i32 = 0;
            wbuf[0..2].copy_from_slice(&cid.to_be_bytes());
            let mut p = wbuf.as_mut_ptr().offset(2);
            let gid = cff_charsets_lookup_inverse(cffont, cid);
            if !(gid as i32 == 0i32) {
                let glyph = cff_get_string(cffont, gid);
                if !glyph.is_null() {
                    let len = agl_sput_UTF16BE(glyph, &mut p, endptr, &mut fail_count);
                    if len < 1i32 || fail_count != 0 {
                        total_fail_count += fail_count
                    } else {
                        CMap_add_bfchar(
                            cmap,
                            wbuf.as_mut_ptr(),
                            2i32 as size_t,
                            wbuf.as_mut_ptr().offset(2),
                            len as size_t,
                        );
                    }
                    free(glyph as *mut libc::c_void);
                }
                glyph_count += 1
            }
        }
    }
    if total_fail_count != 0i32 && total_fail_count >= glyph_count / 10i32 {
        warn!(
            "{} glyph names (out of {}) missing Unicode mapping.",
            total_fail_count, glyph_count,
        );
        warn!(
            "ToUnicode CMap \"{}-UTF16\" removed.",
            CStr::from_ptr(font_name).display()
        );
    } else {
        stream = CMap_create_stream(cmap)
    }
    CMap_release(cmap);
    stream
}
/* Force bold at small text sizes */
/* pdf_font --> CIDFont */
unsafe fn get_font_attr(font: *mut CIDFont, cffont: &cff_font) {
    let italicangle;
    let mut flags: i32 = 0i32;
    static mut L_c: [*const i8; 5] = [
        b"H\x00" as *const u8 as *const i8,
        b"P\x00" as *const u8 as *const i8,
        b"Pi\x00" as *const u8 as *const i8,
        b"Rho\x00" as *const u8 as *const i8,
        ptr::null(),
    ];
    static mut L_d: [*const i8; 5] = [
        b"p\x00" as *const u8 as *const i8,
        b"q\x00" as *const u8 as *const i8,
        b"mu\x00" as *const u8 as *const i8,
        b"eta\x00" as *const u8 as *const i8,
        ptr::null(),
    ];
    static mut L_a: [*const i8; 4] = [
        b"b\x00" as *const u8 as *const i8,
        b"h\x00" as *const u8 as *const i8,
        b"lambda\x00" as *const u8 as *const i8,
        ptr::null(),
    ];
    let mut gm = t1_ginfo::new();
    let mut defaultwidth = 500.0f64;
    let nominalwidth = 0.0f64;
    /*
     * CapHeight, Ascent, and Descent is meaningfull only for Latin/Greek/Cyrillic.
     * The BlueValues and OtherBlues also have those information.
     */
    let mut capheight;
    let mut ascent;
    let mut descent;
    if cff_dict_known(cffont.topdict, b"FontBBox\x00" as *const u8 as *const i8) != 0 {
        /* Default values */
        ascent = cff_dict_get(
            cffont.topdict,
            b"FontBBox\x00" as *const u8 as *const i8,
            3i32,
        );
        capheight = ascent;
        descent = cff_dict_get(
            cffont.topdict,
            b"FontBBox\x00" as *const u8 as *const i8,
            1i32,
        )
    } else {
        capheight = 680.0f64;
        ascent = 690.0f64;
        descent = -190.0f64
    }
    let stemv = if cff_dict_known(
        *cffont.private.offset(0),
        b"StdVW\x00" as *const u8 as *const i8,
    ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"StdVW\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        /*
         * We may use the following values for StemV:
         *  Thin - ExtraLight: <= 50
         *  Light: 71
         *  Regular(Normal): 88
         *  Medium: 109
         *  SemiBold(DemiBold): 135
         *  Bold - Heavy: >= 166
         */
        88.
    };
    if cff_dict_known(cffont.topdict, b"ItalicAngle\x00" as *const u8 as *const i8) != 0 {
        italicangle = cff_dict_get(
            cffont.topdict,
            b"ItalicAngle\x00" as *const u8 as *const i8,
            0i32,
        );
        if italicangle != 0.0f64 {
            flags |= 1i32 << 6i32
        }
    } else {
        italicangle = 0.0f64
    }
    /*
     * Use "space", "H", "p", and "b" for various values.
     * Those characters should not "seac". (no accent)
     */
    let gid = cff_glyph_lookup(cffont, b"space\x00" as *const u8 as *const i8) as i32;
    if gid >= 0i32 && gid < (*cffont.cstrings).count as i32 {
        t1char_get_metrics(
            (*cffont.cstrings)
                .data
                .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                .offset(-1),
            (*(*cffont.cstrings).offset.offset((gid + 1i32) as isize))
                .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize)) as i32,
            *cffont.subrs.offset(0),
            &mut gm,
        );
        defaultwidth = gm.wx
    }
    let mut i = 0;
    while !L_c[i].is_null() {
        let gid = cff_glyph_lookup(cffont, L_c[i]) as i32;
        if gid >= 0i32 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1i32) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                *cffont.subrs.offset(0),
                &mut gm,
            );
            capheight = gm.bbox.ury;
            break;
        } else {
            i += 1
        }
    }
    let mut i = 0;
    while !L_d[i].is_null() {
        let gid = cff_glyph_lookup(cffont, L_d[i]) as i32;
        if gid >= 0i32 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1i32) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                *cffont.subrs.offset(0),
                &mut gm,
            );
            descent = gm.bbox.lly;
            break;
        } else {
            i += 1
        }
    }
    let mut i = 0;
    while !L_a[i].is_null() {
        let gid = cff_glyph_lookup(cffont, L_a[i]) as i32;
        if gid >= 0i32 && gid < (*cffont.cstrings).count as i32 {
            t1char_get_metrics(
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(gid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings).offset.offset((gid + 1i32) as isize))
                    .wrapping_sub(*(*cffont.cstrings).offset.offset(gid as isize))
                    as i32,
                *cffont.subrs.offset(0),
                &mut gm,
            );
            ascent = gm.bbox.ury;
            break;
        } else {
            i += 1
        }
    }
    if defaultwidth != 0.0f64 {
        cff_dict_add(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
            1i32,
        );
        cff_dict_set(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
            0i32,
            defaultwidth,
        );
    }
    if nominalwidth != 0.0f64 {
        cff_dict_add(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
            1i32,
        );
        cff_dict_set(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
            0i32,
            nominalwidth,
        );
    }
    if cff_dict_known(
        *cffont.private.offset(0),
        b"ForceBold\x00" as *const u8 as *const i8,
    ) != 0
        && cff_dict_get(
            *cffont.private.offset(0),
            b"ForceBold\x00" as *const u8 as *const i8,
            0i32,
        ) != 0.
    {
        flags |= 1i32 << 18i32
    }
    if cff_dict_known(
        *cffont.private.offset(0),
        b"IsFixedPitch\x00" as *const u8 as *const i8,
    ) != 0
        && cff_dict_get(
            *cffont.private.offset(0),
            b"IsFixedPitch\x00" as *const u8 as *const i8,
            0i32,
        ) != 0.
    {
        flags |= 1i32 << 0i32
    }
    if !(*font).fontname.is_null()
        && strstr((*font).fontname, b"Sans\x00" as *const u8 as *const i8).is_null()
    {
        flags |= 1i32 << 1i32
    }
    flags |= 1i32 << 2i32;
    (*(*font).descriptor)
        .as_dict_mut()
        .set("CapHeight", capheight);
    (*(*font).descriptor).as_dict_mut().set("Ascent", ascent);
    (*(*font).descriptor).as_dict_mut().set("Descent", descent);
    (*(*font).descriptor)
        .as_dict_mut()
        .set("ItalicAngle", italicangle);
    (*(*font).descriptor).as_dict_mut().set("StemV", stemv);
    (*(*font).descriptor)
        .as_dict_mut()
        .set("Flags", flags as f64);
}
unsafe fn add_metrics(
    font: *mut CIDFont,
    cffont: *mut cff_font,
    CIDToGIDMap: *mut u8,
    widths: *mut f64,
    default_width: f64,
    last_cid: CID,
) {
    /*
     * The original FontBBox of the font is preserved, instead
     * of replacing it with tight bounding box calculated from
     * charstrings, to prevent Acrobat 4 from greeking text as
     * much as possible.
     */
    if cff_dict_known((*cffont).topdict, b"FontBBox\x00" as *const u8 as *const i8) == 0 {
        panic!("No FontBBox?");
    }
    let mut tmp = vec![];
    for i in 0..4 {
        let val = cff_dict_get(
            (*cffont).topdict,
            b"FontBBox\x00" as *const u8 as *const i8,
            i,
        );
        tmp.push_obj((val / 1. + 0.5).floor() * 1.);
    }
    (*(*font).descriptor)
        .as_dict_mut()
        .set("FontBBox", tmp.into_obj());
    let mut parent_id = CIDFont_get_parent_id(font, 0i32);
    if parent_id < 0i32 && {
        parent_id = CIDFont_get_parent_id(font, 1i32);
        parent_id < 0i32
    } {
        panic!("No parent Type 0 font !");
    }
    let used_chars = Type0Font_get_usedchars(Type0Font_cache_get(parent_id));
    if used_chars.is_null() {
        panic!("Unexpected error: Font not actually used???");
    }
    /* FIXME:
     * This writes "CID CID width".
     * I think it's better to handle each 8 char block
     * and to use "CID_start [ w0 w1 ...]".
     */
    let mut tmp = vec![];
    for cid in 0..=last_cid as u16 {
        if *used_chars.offset((cid as i32 / 8i32) as isize) as i32
            & 1i32 << 7i32 - cid as i32 % 8i32
            != 0
        {
            let gid = ((*CIDToGIDMap.offset((2i32 * cid as i32) as isize) as i32) << 8i32
                | *CIDToGIDMap.offset((2i32 * cid as i32 + 1i32) as isize) as i32)
                as u16;
            if *widths.offset(gid as isize) != default_width {
                tmp.push_obj(cid as f64);
                tmp.push_obj(cid as f64);
                tmp.push_obj((*widths.offset(gid as isize) / 1. + 0.5).floor() * 1.);
            }
        }
    }
    (*(*font).fontdict).as_dict_mut().set("DW", default_width);
    let empty = tmp.is_empty();
    let tmp = tmp.into_obj();
    if !empty {
        (*(*font).fontdict).as_dict_mut().set("W", pdf_ref_obj(tmp));
    }
    pdf_release_obj(tmp);
}
/* Type1 --> CFF CIDFont */

pub unsafe fn CIDFont_type0_t1dofont(font: *mut CIDFont) {
    let mut used_chars: *mut i8 = ptr::null_mut();
    assert!(!font.is_null());
    if (*font).indirect.is_null() {
        return;
    }
    (*(*font).fontdict)
        .as_dict_mut()
        .set("FontDescriptor", pdf_ref_obj((*font).descriptor));
    let handle = dpx_open_type1_file((*font).ident);
    if handle.is_none() {
        panic!("Type1: Could not open Type1 font.");
    }
    let handle = handle.unwrap();
    let cffont = t1_load_font(0 as *mut *mut i8, 0i32, handle);
    if cffont.is_null() {
        panic!("Could not read Type 1 font...");
    }
    let cffont = &mut *cffont;
    if (*font).fontname.is_null() {
        panic!("Fontname undefined...");
    }
    let hparent;
    let vparent;
    let hparent_id = CIDFont_get_parent_id(font, 0i32);
    let vparent_id = CIDFont_get_parent_id(font, 1i32);
    if hparent_id < 0i32 && vparent_id < 0i32 {
        panic!("No parent Type 0 font !");
    }
    /* usedchars is same for h and v */
    if hparent_id < 0i32 {
        hparent = ptr::null_mut()
    } else {
        hparent = Type0Font_cache_get(hparent_id);
        used_chars = Type0Font_get_usedchars(hparent)
    }
    if vparent_id < 0i32 {
        vparent = ptr::null_mut()
    } else {
        vparent = Type0Font_cache_get(vparent_id);
        used_chars = Type0Font_get_usedchars(vparent)
    }
    if used_chars.is_null() {
        panic!("Unexpected error: Font not actually used???");
    }
    let tounicode = create_ToUnicode_stream(cffont, (*font).fontname, used_chars).map(IntoObj::into_obj).unwrap_or(ptr::null_mut());
    if !hparent.is_null() {
        Type0Font_set_ToUnicode(hparent, pdf_ref_obj(tounicode));
    }
    if !vparent.is_null() {
        Type0Font_set_ToUnicode(vparent, pdf_ref_obj(tounicode));
    }
    pdf_release_obj(tounicode);
    cff_set_name(cffont, (*font).fontname);
    /* defaultWidthX, CapHeight, etc. */
    get_font_attr(font, cffont);
    let defaultwidth = if cff_dict_known(
        *cffont.private.offset(0),
        b"defaultWidthX\x00" as *const u8 as *const i8,
    ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"defaultWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let nominalwidth = if cff_dict_known(
        *cffont.private.offset(0),
        b"nominalWidthX\x00" as *const u8 as *const i8,
    ) != 0
    {
        cff_dict_get(
            *cffont.private.offset(0),
            b"nominalWidthX\x00" as *const u8 as *const i8,
            0i32,
        )
    } else {
        0.
    };
    let mut num_glyphs = 0;
    let mut last_cid = 0_u16;
    let ref mut fresh6 = *used_chars.offset((0i32 / 8i32) as isize);
    *fresh6 = (*fresh6 as i32 | 1i32 << 7i32 - 0i32 % 8i32) as i8;
    /* .notdef */
    for i in 0..(cffont.num_glyphs as i32 + 7i32) / 8i32 {
        let c = *used_chars.offset(i as isize) as i32;
        for j in (0..8).rev() {
            if c & 1i32 << j != 0 {
                num_glyphs += 1;
                last_cid = ((i + 1i32) * 8i32 - j - 1i32) as u16
            }
        }
    }
    let fdselect = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_fdselect>() as u64) as u32)
        as *mut cff_fdselect;
    (*fdselect).format = 3i32 as u8;
    (*fdselect).num_entries = 1i32 as u16;
    (*fdselect).data.ranges =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cff_range3>() as u64) as u32)
            as *mut cff_range3;
    (*(*fdselect).data.ranges.offset(0)).first = 0i32 as u16;
    (*(*fdselect).data.ranges.offset(0)).fd = 0i32 as u8;
    cffont.fdselect = fdselect;
    let CIDToGIDMap = new(((2i32 * (last_cid as i32 + 1i32)) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memset(
        CIDToGIDMap as *mut libc::c_void,
        0i32,
        (2 * (last_cid + 1)) as _,
    );
    let charset = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    (*charset).format = 0i32 as u8;
    (*charset).num_entries = (num_glyphs - 1i32) as u16;
    (*charset).data.glyphs = new(((num_glyphs - 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<s_SID>() as u64)
        as u32) as *mut s_SID;
    let mut gid = 0_u16;
    for cid in 0..=last_cid as u16 {
        if *used_chars.offset((cid as i32 / 8i32) as isize) as i32
            & 1i32 << 7i32 - cid as i32 % 8i32
            != 0
        {
            if gid as i32 > 0i32 {
                *(*charset).data.glyphs.offset((gid as i32 - 1i32) as isize) = cid
            }
            *CIDToGIDMap.offset((2i32 * cid as i32) as isize) =
                (gid as i32 >> 8i32 & 0xffi32) as u8;
            *CIDToGIDMap.offset((2i32 * cid as i32 + 1i32) as isize) = (gid as i32 & 0xffi32) as u8;
            gid += 1;
        }
    }
    cff_release_charsets(cffont.charsets);
    cffont.charsets = charset;
    cff_dict_add(
        cffont.topdict,
        b"CIDCount\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"CIDCount\x00" as *const u8 as *const i8,
        0i32,
        (last_cid as i32 + 1i32) as f64,
    );
    cffont.fdarray = new((1_u64).wrapping_mul(::std::mem::size_of::<*mut cff_dict>() as u64) as u32)
        as *mut *mut cff_dict;
    let ref mut fresh7 = *cffont.fdarray.offset(0);
    *fresh7 = cff_new_dict();
    cff_dict_add(
        *cffont.fdarray.offset(0),
        b"FontName\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"FontName\x00" as *const u8 as *const i8,
        0i32,
        cff_add_string(cffont, (*font).fontname.offset(7), 1i32) as f64,
    );
    cff_dict_add(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        2i32,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    cff_dict_set(
        *cffont.fdarray.offset(0),
        b"Private\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    /* FDArray  - index offset, not known yet */
    cff_dict_add(
        cffont.topdict,
        b"FDArray\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"FDArray\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    /* FDSelect - offset, not known yet */
    cff_dict_add(
        cffont.topdict,
        b"FDSelect\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"FDSelect\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    cff_dict_add(
        cffont.topdict,
        b"charset\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"charset\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    cff_dict_add(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        1i32,
    );
    cff_dict_set(
        cffont.topdict,
        b"CharStrings\x00" as *const u8 as *const i8,
        0i32,
        0.0f64,
    );
    let mut gm = t1_ginfo::new();
    let mut max: i32 = 0i32;
    let mut w_stat: [i32; 1001] = [0; 1001];
    let widths =
        new((num_glyphs as u32 as u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32)
            as *mut f64;
    memset(
        w_stat.as_mut_ptr() as *mut libc::c_void,
        0i32,
        (::std::mem::size_of::<i32>()).wrapping_mul(1001),
    );
    let mut offset = 0i32;
    let cstring = cff_new_index(num_glyphs as u16);
    (*cstring).data = ptr::null_mut();
    *(*cstring).offset.offset(0) = 1i32 as l_offset;
    gid = 0i32 as u16;
    for cid in 0..=last_cid as u16 {
        if !(*used_chars.offset((cid as i32 / 8i32) as isize) as i32
            & 1i32 << 7i32 - cid as i32 % 8i32
            == 0)
        {
            if offset + 65536i32 >= max {
                max += 65536i32 * 2i32;
                (*cstring).data = renew(
                    (*cstring).data as *mut libc::c_void,
                    (max as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
                ) as *mut u8
            }
            offset += t1char_convert_charstring(
                (*cstring)
                    .data
                    .offset(*(*cstring).offset.offset(gid as isize) as isize)
                    .offset(-1),
                65536i32,
                (*cffont.cstrings)
                    .data
                    .offset(*(*cffont.cstrings).offset.offset(cid as isize) as isize)
                    .offset(-1),
                (*(*cffont.cstrings)
                    .offset
                    .offset((cid as i32 + 1i32) as isize))
                .wrapping_sub(*(*cffont.cstrings).offset.offset(cid as isize))
                    as i32,
                *cffont.subrs.offset(0),
                defaultwidth,
                nominalwidth,
                &mut gm,
            );
            *(*cstring).offset.offset((gid as i32 + 1i32) as isize) = (offset + 1i32) as l_offset;
            if gm.use_seac != 0 {
                panic!("This font using the \"seac\" command for accented characters...");
            }
            *widths.offset(gid as isize) = gm.wx;
            if gm.wx >= 0.0f64 && gm.wx <= 1000.0f64 {
                w_stat[gm.wx as i32 as usize] += 1i32
            }
            gid = gid.wrapping_add(1)
        }
    }
    cff_release_index(cffont.cstrings);
    cffont.cstrings = cstring;
    let mut max_count = 0;
    let mut dw = -1i32;
    for i in 0..=1000 {
        if w_stat[i as usize] > max_count {
            dw = i;
            max_count = w_stat[i as usize]
        }
    }
    if dw >= 0i32 {
        add_metrics(font, cffont, CIDToGIDMap, widths, dw as f64, last_cid);
    } else {
        add_metrics(font, cffont, CIDToGIDMap, widths, defaultwidth, last_cid);
    }
    free(widths as *mut libc::c_void);
    cff_release_index(*cffont.subrs.offset(0));
    let ref mut fresh8 = *cffont.subrs.offset(0);
    *fresh8 = ptr::null_mut();
    free(CIDToGIDMap as *mut libc::c_void);
    cff_add_string(cffont, b"Adobe\x00" as *const u8 as *const i8, 1i32);
    cff_add_string(cffont, b"Identity\x00" as *const u8 as *const i8, 1i32);
    cff_dict_update(cffont.topdict, cffont);
    cff_dict_update(*cffont.private.offset(0), cffont);
    cff_update_string(cffont);
    /* CFF code need to be rewrote... */
    cff_dict_add(cffont.topdict, b"ROS\x00" as *const u8 as *const i8, 3i32);
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        0i32,
        cff_get_sid(cffont, b"Adobe\x00" as *const u8 as *const i8) as f64,
    );
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        1i32,
        cff_get_sid(cffont, b"Identity\x00" as *const u8 as *const i8) as f64,
    );
    cff_dict_set(
        cffont.topdict,
        b"ROS\x00" as *const u8 as *const i8,
        2i32,
        0.0f64,
    );
    cffont.num_glyphs = num_glyphs as u16;
    write_fontfile(font, cffont);
    cff_close(cffont);
    CIDFont_type0_add_CIDSet(font, used_chars, last_cid);
}
