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

use super::dpx_mfileio::work_buffer;
use crate::mfree;
use crate::{info, warn};
use crate::{streq_ptr, strstartswith};

use super::dpx_dpxfile::dpx_tt_open;
use super::dpx_dpxutil::{
    ht_clear_table, ht_init_table, ht_insert_table, ht_lookup_table, ht_remove_table,
};
use super::dpx_dpxutil::{parse_c_string, parse_float_decimal};
use super::dpx_mem::{new, xmalloc};
use super::dpx_mfileio::tt_mfgets;
use super::dpx_subfont::{release_sfd_record, sfd_get_subfont_ids};
use crate::shims::sprintf;
use crate::ttstub_input_close;
use libc::{
    atof, atoi, free, memcmp, memcpy, strcat, strchr, strcmp, strcpy, strlen, strstr, strtol,
    strtoul,
};

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct fontmap_opt {
    pub slant: f64,
    pub extend: f64,
    pub bold: f64,
    pub mapc: i32,
    pub flags: i32,
    pub otl_tags: *mut i8,
    pub tounicode: *mut i8,
    pub cff_charsets: *mut libc::c_void,
    pub design_size: f64,
    pub charcoll: *mut i8,
    pub index: i32,
    pub style: i32,
    pub stemv: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct fontmap_rec {
    pub map_name: *mut i8,
    pub font_name: *mut i8,
    pub enc_name: *mut i8,
    pub charmap: C2RustUnnamed_0,
    pub opt: fontmap_opt,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub sfd_name: *mut i8,
    pub subfont_id: *mut i8,
}

use super::dpx_dpxutil::ht_table;
/* quasi-hack to get the primary input */
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut verbose: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn pdf_fontmap_set_verbose(mut level: i32) {
    verbose = level;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_init_fontmap_record(mut mrec: *mut fontmap_rec) {
    assert!(!mrec.is_null());
    (*mrec).map_name = 0 as *mut i8;
    /* SFD char mapping */
    (*mrec).charmap.sfd_name = 0 as *mut i8;
    (*mrec).charmap.subfont_id = 0 as *mut i8;
    /* for OFM */
    (*mrec).opt.mapc = -1i32; /* compatibility */
    (*mrec).font_name = 0 as *mut i8; /* not given explicitly by an option */
    (*mrec).enc_name = 0 as *mut i8;
    (*mrec).opt.slant = 0.0f64;
    (*mrec).opt.extend = 1.0f64;
    (*mrec).opt.bold = 0.0f64;
    (*mrec).opt.flags = 0i32;
    (*mrec).opt.design_size = -1.0f64;
    (*mrec).opt.tounicode = 0 as *mut i8;
    (*mrec).opt.otl_tags = 0 as *mut i8;
    (*mrec).opt.index = 0i32;
    (*mrec).opt.charcoll = 0 as *mut i8;
    (*mrec).opt.style = 0i32;
    (*mrec).opt.stemv = -1i32;
    (*mrec).opt.cff_charsets = 0 as *mut libc::c_void;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_clear_fontmap_record(mut mrec: *mut fontmap_rec) {
    assert!(!mrec.is_null());
    free((*mrec).map_name as *mut libc::c_void);
    free((*mrec).charmap.sfd_name as *mut libc::c_void);
    free((*mrec).charmap.subfont_id as *mut libc::c_void);
    free((*mrec).enc_name as *mut libc::c_void);
    free((*mrec).font_name as *mut libc::c_void);
    free((*mrec).opt.tounicode as *mut libc::c_void);
    free((*mrec).opt.otl_tags as *mut libc::c_void);
    free((*mrec).opt.charcoll as *mut libc::c_void);
    pdf_init_fontmap_record(mrec);
}
/* strdup: just returns NULL for NULL */
unsafe fn mstrdup(mut s: *const i8) -> *mut i8 {
    if s.is_null() {
        return 0 as *mut i8;
    }
    let r = new((strlen(s).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    strcpy(r, s);
    r
}
unsafe fn pdf_copy_fontmap_record(mut dst: *mut fontmap_rec, mut src: *const fontmap_rec) {
    assert!(!dst.is_null() && !src.is_null());
    (*dst).map_name = mstrdup((*src).map_name);
    (*dst).charmap.sfd_name = mstrdup((*src).charmap.sfd_name);
    (*dst).charmap.subfont_id = mstrdup((*src).charmap.subfont_id);
    (*dst).font_name = mstrdup((*src).font_name);
    (*dst).enc_name = mstrdup((*src).enc_name);
    (*dst).opt.slant = (*src).opt.slant;
    (*dst).opt.extend = (*src).opt.extend;
    (*dst).opt.bold = (*src).opt.bold;
    (*dst).opt.flags = (*src).opt.flags;
    (*dst).opt.mapc = (*src).opt.mapc;
    (*dst).opt.tounicode = mstrdup((*src).opt.tounicode);
    (*dst).opt.otl_tags = mstrdup((*src).opt.otl_tags);
    (*dst).opt.index = (*src).opt.index;
    (*dst).opt.charcoll = mstrdup((*src).opt.charcoll);
    (*dst).opt.style = (*src).opt.style;
    (*dst).opt.stemv = (*src).opt.stemv;
    (*dst).opt.cff_charsets = (*src).opt.cff_charsets;
}
unsafe extern "C" fn hval_free(mut vp: *mut libc::c_void) {
    let mut mrec: *mut fontmap_rec = vp as *mut fontmap_rec;
    pdf_clear_fontmap_record(mrec);
    free(mrec as *mut libc::c_void);
}
unsafe fn fill_in_defaults(mut mrec: *mut fontmap_rec, mut tex_name: *const i8) {
    if !(*mrec).enc_name.is_null()
        && (streq_ptr((*mrec).enc_name, b"default\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr((*mrec).enc_name, b"none\x00" as *const u8 as *const i8) as i32 != 0)
    {
        (*mrec).enc_name = mfree((*mrec).enc_name as *mut libc::c_void) as *mut i8
    }
    if !(*mrec).font_name.is_null()
        && (streq_ptr((*mrec).font_name, b"default\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr((*mrec).font_name, b"none\x00" as *const u8 as *const i8) as i32 != 0)
    {
        (*mrec).font_name = mfree((*mrec).font_name as *mut libc::c_void) as *mut i8
    }
    /* We *must* fill font_name either explicitly or by default */
    if (*mrec).font_name.is_null() {
        (*mrec).font_name =
            new((strlen(tex_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy((*mrec).font_name, tex_name);
    }
    (*mrec).map_name =
        new((strlen(tex_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*mrec).map_name, tex_name);
    /* Use "UCS" character collection for Unicode SFD
     * and Identity CMap combination. For backward
     * compatibility.
     */
    if !(*mrec).charmap.sfd_name.is_null()
        && !(*mrec).enc_name.is_null()
        && (*mrec).opt.charcoll.is_null()
    {
        if (streq_ptr(
            (*mrec).enc_name,
            b"Identity-H\x00" as *const u8 as *const i8,
        ) as i32
            != 0
            || streq_ptr(
                (*mrec).enc_name,
                b"Identity-V\x00" as *const u8 as *const i8,
            ) as i32
                != 0)
            && (!strstr(
                (*mrec).charmap.sfd_name,
                b"Uni\x00" as *const u8 as *const i8,
            )
            .is_null()
                || !strstr(
                    (*mrec).charmap.sfd_name,
                    b"UBig\x00" as *const u8 as *const i8,
                )
                .is_null()
                || !strstr(
                    (*mrec).charmap.sfd_name,
                    b"UBg\x00" as *const u8 as *const i8,
                )
                .is_null()
                || !strstr(
                    (*mrec).charmap.sfd_name,
                    b"UGB\x00" as *const u8 as *const i8,
                )
                .is_null()
                || !strstr(
                    (*mrec).charmap.sfd_name,
                    b"UKS\x00" as *const u8 as *const i8,
                )
                .is_null()
                || !strstr(
                    (*mrec).charmap.sfd_name,
                    b"UJIS\x00" as *const u8 as *const i8,
                )
                .is_null())
        {
            (*mrec).opt.charcoll = new((strlen(b"UCS\x00" as *const u8 as *const i8)
                .wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8; /* we don't have quoted string */
            strcpy((*mrec).opt.charcoll, b"UCS\x00" as *const u8 as *const i8);
        }
    };
}
unsafe fn tt_readline(
    mut buf: *mut i8,
    mut buf_len: i32,
    handle: &mut InputHandleWrapper,
) -> *mut i8 {
    assert!(!buf.is_null() && buf_len > 0i32);
    let p = tt_mfgets(buf, buf_len, handle);
    if p.is_null() {
        return 0 as *mut i8;
    }
    let q = strchr(p, '%' as i32);
    if !q.is_null() {
        *q = '\u{0}' as i32 as i8
    }
    p
}
unsafe fn skip_blank(mut pp: *mut *const i8, mut endptr: *const i8) {
    let mut p: *const i8 = *pp;
    if p.is_null() || p >= endptr {
        return;
    }
    while p < endptr && (*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _) != 0) {
        p = p.offset(1)
    }
    *pp = p;
}
unsafe fn parse_string_value(mut pp: *mut *const i8, mut endptr: *const i8) -> *mut i8 {
    let q;
    let mut p: *const i8 = *pp;
    if p.is_null() || p >= endptr {
        return 0 as *mut i8;
    }
    if *p as i32 == '\"' as i32 {
        q = parse_c_string(&mut p, endptr)
    } else {
        let mut n = 0;
        while p < endptr && libc::isspace(*p as _) == 0 {
            p = p.offset(1);
            n += 1;
        }
        if n == 0_u32 {
            return 0 as *mut i8;
        }
        q = new(
            (n.wrapping_add(1_u32) as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32,
        ) as *mut i8;
        memcpy(q as *mut libc::c_void, *pp as *const libc::c_void, n as _);
        *q.offset(n as isize) = '\u{0}' as i32 as i8
    }
    *pp = p;
    q
}
/* no preceeding spaces allowed */
unsafe fn parse_integer_value(
    mut pp: *mut *const i8,
    mut endptr: *const i8,
    mut base: i32,
) -> *mut i8 {
    let mut p: *const i8 = *pp;
    let mut has_sign: i32 = 0i32;
    let mut has_prefix: i32 = 0i32;
    assert!(base == 0i32 || base >= 2i32 && base <= 36i32);
    if p.is_null() || p >= endptr {
        return 0 as *mut i8;
    }
    if *p as i32 == '-' as i32 || *p as i32 == '+' as i32 {
        p = p.offset(1);
        has_sign = 1i32
    }
    if (base == 0i32 || base == 16i32)
        && p.offset(2) <= endptr
        && *p.offset(0) as i32 == '0' as i32
        && *p.offset(1) as i32 == 'x' as i32
    {
        p = p.offset(2);
        has_prefix = 1i32
    }
    if base == 0i32 {
        if has_prefix != 0 {
            base = 16i32
        } else if p < endptr && *p as i32 == '0' as i32 {
            base = 8i32
        } else {
            base = 10i32
        }
    }
    let mut n = 0;
    while p < endptr
        && (base <= 10i32 && *p as i32 >= '0' as i32 && (*p as i32) < '0' as i32 + base
            || base > 10i32
                && (*p as i32 >= '0' as i32 && *p as i32 <= '9' as i32
                    || *p as i32 >= 'a' as i32 && (*p as i32) < 'a' as i32 + (base - 10i32)
                    || *p as i32 >= 'A' as i32 && (*p as i32) < 'A' as i32 + (base - 10i32)))
    {
        p = p.offset(1);
        n += 1
    }
    if n == 0i32 {
        return 0 as *mut i8;
    }
    if has_sign != 0 {
        n += 1i32
    }
    if has_prefix != 0 {
        n += 2i32
    }
    let q = new(((n + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    memcpy(q as *mut libc::c_void, *pp as *const libc::c_void, n as _);
    *q.offset(n as isize) = '\u{0}' as i32 as i8;
    *pp = p;
    q
}
unsafe fn fontmap_parse_mapdef_dpm(
    mut mrec: *mut fontmap_rec,
    mut mapdef: *const i8,
    mut endptr: *const i8,
) -> i32 {
    let mut p: *const i8 = mapdef;
    /*
     * Parse record line in map file.  First two fields (after TeX font
     * name) are position specific.  Arguments start at the first token
     * beginning with a  '-'.
     *
     * NOTE:
     *   Dvipdfm basically uses parse_ident() for parsing enc_name,
     *   font_name, and other string values which assumes PostScript-like
     *   syntax.
     *   skip_white() skips '\r' and '\n' but they should terminate
     *   fontmap line here.
     */
    skip_blank(&mut p, endptr);
    /* encoding field */
    if p < endptr && *p as i32 != '-' as i32 {
        /* May be NULL */
        (*mrec).enc_name = parse_string_value(&mut p, endptr);
        skip_blank(&mut p, endptr);
    }
    /* fontname or font filename field */
    if p < endptr && *p as i32 != '-' as i32 {
        /* May be NULL */
        (*mrec).font_name = parse_string_value(&mut p, endptr);
        skip_blank(&mut p, endptr);
    }
    if !(*mrec).font_name.is_null() {
        /* Several options are encoded in font_name for
         * compatibility with dvipdfm.
         */
        let tmp = strip_options((*mrec).font_name, &mut (*mrec).opt);
        if !tmp.is_null() {
            free((*mrec).font_name as *mut libc::c_void);
            (*mrec).font_name = tmp
        }
    }
    skip_blank(&mut p, endptr);
    /* Parse any remaining arguments */
    while p.offset(1) < endptr
        && *p as i32 != '\r' as i32
        && *p as i32 != '\n' as i32
        && *p as i32 == '-' as i32
    {
        let mut mopt: i8 = *p.offset(1);
        p = p.offset(2);
        skip_blank(&mut p, endptr);
        match mopt as i32 {
            115 => {
                /* Slant option */
                let q = parse_float_decimal(&mut p, endptr);
                if q.is_null() {
                    warn!("Missing a number value for \'s\' option.");
                    return -1i32;
                }
                (*mrec).opt.slant = atof(q);
                free(q as *mut libc::c_void);
            }
            101 => {
                /* Extend option */
                let q = parse_float_decimal(&mut p, endptr);
                if q.is_null() {
                    warn!("Missing a number value for \'e\' option.");
                    return -1i32;
                }
                (*mrec).opt.extend = atof(q);
                if (*mrec).opt.extend <= 0.0f64 {
                    warn!(
                        "Invalid value for \'e\' option: {}",
                        CStr::from_ptr(q).display(),
                    );
                    return -1i32;
                }
                free(q as *mut libc::c_void);
            }
            98 => {
                /* Fake-bold option */
                let q = parse_float_decimal(&mut p, endptr);
                if q.is_null() {
                    warn!("Missing a number value for \'b\' option.");
                    return -1i32;
                }
                (*mrec).opt.bold = atof(q);
                if (*mrec).opt.bold <= 0.0f64 {
                    warn!(
                        "Invalid value for \'b\' option: {}",
                        CStr::from_ptr(q).display(),
                    );
                    return -1i32;
                }
                free(q as *mut libc::c_void);
            }
            114 => {}
            105 => {
                /* TTC index */
                let q = parse_integer_value(&mut p, endptr, 10i32);
                if q.is_null() {
                    warn!("Missing TTC index number...");
                    return -1i32;
                }
                (*mrec).opt.index = atoi(q);
                if (*mrec).opt.index < 0i32 {
                    warn!("Invalid TTC index number: {}", CStr::from_ptr(q).display(),);
                    return -1i32;
                }
                free(q as *mut libc::c_void);
            }
            112 => {
                /* UCS plane: just for testing */
                let q = parse_integer_value(&mut p, endptr, 0i32);
                if q.is_null() {
                    warn!("Missing a number for \'p\' option.");
                    return -1i32;
                }
                let v = strtol(q, 0 as *mut *mut i8, 0i32) as i32;
                if v < 0i32 || v > 16i32 {
                    warn!(
                        "Invalid value for option \'p\': {}",
                        CStr::from_ptr(q).display(),
                    );
                } else {
                    (*mrec).opt.mapc = v << 16i32
                }
                free(q as *mut libc::c_void);
            }
            117 => {
                /* ToUnicode */
                let q = parse_string_value(&mut p, endptr);
                if !q.is_null() {
                    (*mrec).opt.tounicode = q
                } else {
                    warn!("Missing string value for option \'u\'.");
                    return -1i32;
                }
            }
            118 => {
                /* StemV */
                let q = parse_integer_value(&mut p, endptr, 10i32);
                if q.is_null() {
                    warn!("Missing a number for \'v\' option.");
                    return -1i32;
                }
                (*mrec).opt.stemv = strtol(q, 0 as *mut *mut i8, 0i32) as i32;
                free(q as *mut libc::c_void);
            }
            108 => {
                /* 2017.4.15 back again */
                let q = parse_string_value(&mut p, endptr);
                if !q.is_null() {
                    (*mrec).opt.otl_tags = q
                } else {
                    warn!("Missing string value for option \'l\'.");
                    return -1i32;
                }
            }
            109 => {
                /* Omega uses both single-byte and double-byte set_char command
                 * even for double-byte OFMs. This confuses CMap decoder.
                 */
                /* Map single bytes char 0xab to double byte char 0xcdab  */
                if p.offset(4) <= endptr
                    && *p.offset(0) as i32 == '<' as i32
                    && *p.offset(3) as i32 == '>' as i32
                {
                    p = p.offset(1);
                    let q = parse_integer_value(&mut p, endptr, 16i32);
                    if q.is_null() {
                        warn!("Invalid value for option \'m\'.");
                        return -1i32;
                    } else {
                        if p < endptr && *p as i32 != '>' as i32 {
                            warn!(
                                "Invalid value for option \'m\': {}",
                                CStr::from_ptr(q).display(),
                            );
                            free(q as *mut libc::c_void);
                            return -1i32;
                        }
                    }
                    let v = strtol(q, 0 as *mut *mut i8, 16i32) as i32;
                    (*mrec).opt.mapc = ((v << 8i32) as i64 & 0xff00) as i32;
                    free(q as *mut libc::c_void);
                    p = p.offset(1)
                } else if p.offset(4) <= endptr
                    && memcmp(
                        p as *const libc::c_void,
                        b"sfd:\x00" as *const u8 as *const i8 as *const libc::c_void,
                        strlen(b"sfd:\x00" as *const u8 as *const i8),
                    ) == 0
                {
                    /* SFD mapping: sfd:Big5,00 */
                    p = p.offset(4);
                    skip_blank(&mut p, endptr);
                    let q = parse_string_value(&mut p, endptr);
                    if q.is_null() {
                        warn!("Missing value for option \'m\'.");
                        return -1i32;
                    }
                    let mut r = strchr(q, ',' as i32);
                    if r.is_null() {
                        warn!(
                            "Invalid value for option \'m\': {}",
                            CStr::from_ptr(q).display(),
                        );
                        free(q as *mut libc::c_void);
                        return -1i32;
                    }
                    *r = 0_i8;
                    r = r.offset(1);
                    let mut rr = r as *const i8;
                    skip_blank(&mut rr, r.offset(strlen(r) as isize));
                    if *rr as i32 == '\u{0}' as i32 {
                        warn!(
                            "Invalid value for option \'m\': {},",
                            CStr::from_ptr(q).display()
                        );
                        free(q as *mut libc::c_void);
                        return -1i32;
                    }
                    (*mrec).charmap.sfd_name = mstrdup(q);
                    (*mrec).charmap.subfont_id = mstrdup(rr);
                    free(q as *mut libc::c_void);
                } else if p.offset(4) < endptr
                    && memcmp(
                        p as *const libc::c_void,
                        b"pad:\x00" as *const u8 as *const i8 as *const libc::c_void,
                        strlen(b"pad:\x00" as *const u8 as *const i8),
                    ) == 0
                {
                    p = p.offset(4);
                    skip_blank(&mut p, endptr);
                    let q = parse_integer_value(&mut p, endptr, 16i32);
                    if q.is_null() {
                        warn!("Invalid value for option \'m\'.");
                        return -1i32;
                    } else {
                        if p < endptr && libc::isspace(*p as _) == 0 {
                            warn!(
                                "Invalid value for option \'m\': {}",
                                CStr::from_ptr(q).display(),
                            );
                            free(q as *mut libc::c_void);
                            return -1i32;
                        }
                    }
                    let v = strtol(q, 0 as *mut *mut i8, 16i32) as i32;
                    (*mrec).opt.mapc = ((v << 8i32) as i64 & 0xff00) as i32;
                    free(q as *mut libc::c_void);
                } else {
                    warn!("Invalid value for option \'m\'.");
                    return -1i32;
                }
            }
            119 => {
                /* Writing mode (for unicode encoding) */
                if (*mrec).enc_name.is_null()
                    || strcmp((*mrec).enc_name, b"unicode\x00" as *const u8 as *const i8) != 0
                {
                    warn!("Fontmap option \'w\' meaningless for encoding other than \"unicode\".");
                    return -1i32;
                }
                let q = parse_integer_value(&mut p, endptr, 10i32);
                if q.is_null() {
                    warn!("Missing wmode value...");
                    return -1i32;
                }
                if atoi(q) == 1i32 {
                    (*mrec).opt.flags |= 1i32 << 2i32
                } else if atoi(q) == 0i32 {
                    (*mrec).opt.flags &= !(1i32 << 2i32)
                } else {
                    warn!(
                        "Invalid value for option \'w\': {}",
                        CStr::from_ptr(q).display(),
                    );
                }
                free(q as *mut libc::c_void);
            }
            _ => {
                warn!(
                    "Unrecognized font map option: \'{}\'",
                    char::from(mopt as u8),
                );
                return -1i32;
            }
        }
        skip_blank(&mut p, endptr);
    }
    if p < endptr && *p as i32 != '\r' as i32 && *p as i32 != '\n' as i32 {
        warn!("Invalid char in fontmap line: {}", char::from(*p as u8),);
        return -1i32;
    }
    0i32
}
/* Parse record line in map file of DVIPS/pdfTeX format. */
unsafe fn fontmap_parse_mapdef_dps(
    mut mrec: *mut fontmap_rec,
    mut mapdef: *const i8,
    mut endptr: *const i8,
) -> i32 {
    let mut p: *const i8 = mapdef;
    skip_blank(&mut p, endptr);
    /* The first field (after TFM name) must be PostScript name. */
    /* However, pdftex.map allows a line without PostScript name. */
    if *p as i32 != '\"' as i32 && *p as i32 != '<' as i32 {
        if p < endptr {
            let q = parse_string_value(&mut p, endptr);
            free(q as *mut libc::c_void);
            skip_blank(&mut p, endptr);
        } else {
            warn!("Missing a PostScript font name.");
            return -1i32;
        }
    }
    if p >= endptr {
        return 0i32;
    }
    /* Parse any remaining arguments */
    while p < endptr
        && *p as i32 != '\r' as i32
        && *p as i32 != '\n' as i32
        && (*p as i32 == '<' as i32 || *p as i32 == '\"' as i32)
    {
        match *p as i32 {
            60 => {
                p = p.offset(1); /*skip */
                if p < endptr && (*p as i32 == '[' as i32 || *p as i32 == '<' as i32) {
                    p = p.offset(1)
                }
                skip_blank(&mut p, endptr);
                let q = parse_string_value(&mut p, endptr);
                if !q.is_null() {
                    let mut n: i32 = strlen(q) as i32;
                    if n > 4i32
                        && !strstartswith(
                            q.offset(n as isize).offset(-4),
                            b".enc\x00" as *const u8 as *const i8,
                        )
                        .is_null()
                    {
                        (*mrec).enc_name = q
                    } else {
                        (*mrec).font_name = q
                    }
                }
                skip_blank(&mut p, endptr);
            }
            34 => {
                /* encoding or fontfile field */
                /* If we see <[ or <<, just ignore the second char instead
                of doing as directed (define encoding file, fully embed); sorry.  */
                /* Options */
                let q = parse_string_value(&mut p, endptr);
                if !q.is_null() {
                    let mut r: *const i8 = q;
                    let mut e: *const i8 = q.offset(strlen(q) as isize);
                    skip_blank(&mut r, e);
                    while r < e {
                        let mut s = parse_float_decimal(&mut r, e);
                        if !s.is_null() {
                            skip_blank(&mut r, e);
                            let t = parse_string_value(&mut r, e);
                            if !t.is_null() {
                                if streq_ptr(t, b"SlantFont\x00" as *const u8 as *const i8) {
                                    (*mrec).opt.slant = atof(s)
                                } else if streq_ptr(t, b"ExtendFont\x00" as *const u8 as *const i8)
                                {
                                    (*mrec).opt.extend = atof(s)
                                }
                                free(t as *mut libc::c_void);
                            }
                            free(s as *mut libc::c_void);
                        } else {
                            s = parse_string_value(&mut r, e);
                            if !s.is_null() {
                                /* skip */
                                free(s as *mut libc::c_void); /* including two '@' */
                            }
                        }
                        skip_blank(&mut r, e);
                    }
                    free(q as *mut libc::c_void);
                }
                skip_blank(&mut p, endptr);
            }
            _ => {
                warn!("Found an invalid entry: {}", CStr::from_ptr(p).display(),);
                return -1i32;
            }
        }
        skip_blank(&mut p, endptr);
    }
    if p < endptr && *p as i32 != '\r' as i32 && *p as i32 != '\n' as i32 {
        warn!("Invalid char in fontmap line: {}", char::from(*p as u8),);
        return -1i32;
    }
    0i32
}
static mut fontmap: *mut ht_table = std::ptr::null_mut();
unsafe fn chop_sfd_name(mut tex_name: *const i8, mut sfd_name: *mut *mut i8) -> *mut i8 {
    *sfd_name = 0 as *mut i8;
    let mut p = strchr(tex_name, '@' as i32);
    if p.is_null() || *p.offset(1) as i32 == '\u{0}' as i32 || p == tex_name as *mut i8 {
        return 0 as *mut i8;
    }
    let m = p.wrapping_offset_from(tex_name) as i64 as i32;
    p = p.offset(1);
    let mut q = strchr(p, '@' as i32);
    if q.is_null() || q == p {
        return 0 as *mut i8;
    }
    let n = q.wrapping_offset_from(p) as i64 as i32;
    q = q.offset(1);
    let len = strlen(tex_name).wrapping_sub(n as _) as i32;
    let fontname =
        new(((len + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    memcpy(
        fontname as *mut libc::c_void,
        tex_name as *const libc::c_void,
        m as _,
    );
    *fontname.offset(m as isize) = '\u{0}' as i32 as i8;
    if *q != 0 {
        strcat(fontname, q);
    }
    *sfd_name =
        new(((n + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
    memcpy(
        *sfd_name as *mut libc::c_void,
        p as *const libc::c_void,
        n as _,
    );
    *(*sfd_name).offset(n as isize) = '\u{0}' as i32 as i8;
    fontname
}
unsafe fn make_subfont_name(
    mut map_name: *const i8,
    mut sfd_name: *const i8,
    mut sub_id: *const i8,
) -> *mut i8 {
    let mut p = strchr(map_name, '@' as i32);
    if p.is_null() || p == map_name as *mut i8 {
        return 0 as *mut i8;
    }
    let m = p.wrapping_offset_from(map_name) as i64 as i32;
    let q = strchr(p.offset(1), '@' as i32);
    if q.is_null() || q == p.offset(1) {
        return 0 as *mut i8;
    }
    let n = q.wrapping_offset_from(p) as i64 as i32 + 1i32;
    if strlen(sfd_name) != (n - 2i32) as _
        || memcmp(
            p.offset(1) as *const libc::c_void,
            sfd_name as *const libc::c_void,
            (n - 2) as _,
        ) != 0
    {
        return 0 as *mut i8;
    }
    let tfm_name = new((strlen(map_name)
        .wrapping_sub(n as _)
        .wrapping_add(strlen(sub_id))
        .wrapping_add(1))
    .wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    memcpy(
        tfm_name as *mut libc::c_void,
        map_name as *const libc::c_void,
        m as _,
    );
    *tfm_name.offset(m as isize) = '\u{0}' as i32 as i8;
    strcat(tfm_name, sub_id);
    if *q.offset(1) != 0 {
        /* not ending with '@' */
        strcat(tfm_name, q.offset(1));
    }
    tfm_name
}
/* "foo@A@ ..." is expanded to
 *   fooab ... -m sfd:A,ab
 *   ...
 *   fooyz ... -m sfd:A,yz
 * where 'ab' ... 'yz' is subfont IDs in SFD 'A'.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_append_fontmap_record(
    mut kp: *const i8,
    mut vp: *const fontmap_rec,
) -> i32 {
    let mut sfd_name: *mut i8 = 0 as *mut i8;
    if kp.is_null() || (vp.is_null() || (*vp).map_name.is_null() || (*vp).font_name.is_null()) {
        warn!("Invalid fontmap record...");
        return -1i32;
    }
    if verbose > 3i32 {
        info!(
            "fontmap>> append key=\"{}\"...",
            CStr::from_ptr(kp).display()
        );
    }
    let mut fnt_name = chop_sfd_name(kp, &mut sfd_name);/* link to this entry */
    if !fnt_name.is_null() && !sfd_name.is_null() {
        let mut n: i32 = 0i32;
        let mut subfont_ids = sfd_get_subfont_ids(sfd_name, &mut n);
        if subfont_ids.is_null() {
            return -1i32;
        }
        loop {
            let fresh0 = n;
            n = n - 1;
            if !(fresh0 > 0i32) {
                break;
            }
            let mut tfm_name = make_subfont_name(kp, sfd_name, *subfont_ids.offset(n as isize));
            if tfm_name.is_null() {
                continue;
            }
            let mut mrec = ht_lookup_table(
                fontmap,
                tfm_name as *const libc::c_void,
                strlen(tfm_name) as i32,
            ) as *mut fontmap_rec;
            if mrec.is_null() {
                mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                    as *mut fontmap_rec;
                pdf_init_fontmap_record(mrec);
                (*mrec).map_name = mstrdup(kp);
                (*mrec).charmap.sfd_name = mstrdup(sfd_name);
                (*mrec).charmap.subfont_id = mstrdup(*subfont_ids.offset(n as isize));
                ht_insert_table(
                    fontmap,
                    tfm_name as *const libc::c_void,
                    strlen(tfm_name) as i32,
                    mrec as *mut libc::c_void,
                );
            }
            free(tfm_name as *mut libc::c_void);
        }
        free(fnt_name as *mut libc::c_void);
        free(sfd_name as *mut libc::c_void);
    }
    let mut mrec =
        ht_lookup_table(fontmap, kp as *const libc::c_void, strlen(kp) as i32) as *mut fontmap_rec;
    if mrec.is_null() {
        mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
            as *mut fontmap_rec;
        pdf_copy_fontmap_record(mrec, vp);
        if !(*mrec).map_name.is_null() && streq_ptr(kp, (*mrec).map_name) as i32 != 0 {
            (*mrec).map_name = mfree((*mrec).map_name as *mut libc::c_void) as *mut i8
        }
        ht_insert_table(
            fontmap,
            kp as *const libc::c_void,
            strlen(kp) as i32,
            mrec as *mut libc::c_void,
        );
    }
    if verbose > 3i32 {
        info!("\n");
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_remove_fontmap_record(mut kp: *const i8) -> i32 {
    let mut sfd_name: *mut i8 = 0 as *mut i8;
    if kp.is_null() {
        return -1i32;
    }
    if verbose > 3i32 {
        info!(
            "fontmap>> remove key=\"{}\"...",
            CStr::from_ptr(kp).display()
        );
    }
    let fnt_name = chop_sfd_name(kp, &mut sfd_name);
    if !fnt_name.is_null() && !sfd_name.is_null() {
        let mut n: i32 = 0i32;
        let subfont_ids = sfd_get_subfont_ids(sfd_name, &mut n);
        if subfont_ids.is_null() {
            return -1i32;
        }
        if verbose > 3i32 {
            info!(
                "\nfontmap>> Expand @{}@:",
                CStr::from_ptr(sfd_name).display()
            );
        }
        loop {
            let fresh1 = n;
            n = n - 1;
            if !(fresh1 > 0i32) {
                break;
            }
            let tfm_name = make_subfont_name(kp, sfd_name, *subfont_ids.offset(n as isize));
            if tfm_name.is_null() {
                continue;
            }
            if verbose > 3i32 {
                info!(" {}", CStr::from_ptr(tfm_name).display());
            }
            ht_remove_table(
                fontmap,
                tfm_name as *const libc::c_void,
                strlen(tfm_name) as i32,
            );
            free(tfm_name as *mut libc::c_void);
        }
        free(fnt_name as *mut libc::c_void);
        free(sfd_name as *mut libc::c_void);
    }
    ht_remove_table(fontmap, kp as *const libc::c_void, strlen(kp) as i32);
    if verbose > 3i32 {
        info!("\n");
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_insert_fontmap_record(
    mut kp: *const i8,
    mut vp: *const fontmap_rec,
) -> *mut fontmap_rec {
    let mut sfd_name: *mut i8 = 0 as *mut i8;
    if kp.is_null() || (vp.is_null() || (*vp).map_name.is_null() || (*vp).font_name.is_null()) {
        warn!("Invalid fontmap record...");
        return 0 as *mut fontmap_rec;
    }
    if verbose > 3i32 {
        info!(
            "fontmap>> insert key=\"{}\"...",
            CStr::from_ptr(kp).display()
        );
    }
    let fnt_name = chop_sfd_name(kp, &mut sfd_name);
    if !fnt_name.is_null() && !sfd_name.is_null() {
        let mut n: i32 = 0i32;
        let subfont_ids = sfd_get_subfont_ids(sfd_name, &mut n);
        if subfont_ids.is_null() {
            warn!(
                "Could not open SFD file: {}",
                CStr::from_ptr(sfd_name).display(),
            );
            free(fnt_name as *mut libc::c_void);
            free(sfd_name as *mut libc::c_void);
            return 0 as *mut fontmap_rec;
        }
        if verbose > 3i32 {
            info!(
                "\nfontmap>> Expand @{}@:",
                CStr::from_ptr(sfd_name).display()
            );
        }
        loop {
            let fresh2 = n;
            n = n - 1;
            if !(fresh2 > 0i32) {
                break;
            }
            let tfm_name = make_subfont_name(kp, sfd_name, *subfont_ids.offset(n as isize));
            if tfm_name.is_null() {
                continue;
            }
            if verbose > 3i32 {
                info!(" {}", CStr::from_ptr(tfm_name).display());
            }
            let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                as *mut fontmap_rec;
            pdf_init_fontmap_record(mrec);
            (*mrec).map_name = mstrdup(kp);
            (*mrec).charmap.sfd_name = mstrdup(sfd_name);
            (*mrec).charmap.subfont_id = mstrdup(*subfont_ids.offset(n as isize));
            ht_insert_table(
                fontmap,
                tfm_name as *const libc::c_void,
                strlen(tfm_name) as i32,
                mrec as *mut libc::c_void,
            );
            free(tfm_name as *mut libc::c_void);
        }
        free(fnt_name as *mut libc::c_void);
        free(sfd_name as *mut libc::c_void);
    }
    let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
        as *mut fontmap_rec;
    pdf_copy_fontmap_record(mrec, vp);
    if !(*mrec).map_name.is_null() && streq_ptr(kp, (*mrec).map_name) as i32 != 0 {
        (*mrec).map_name = mfree((*mrec).map_name as *mut libc::c_void) as *mut i8
    }
    ht_insert_table(
        fontmap,
        kp as *const libc::c_void,
        strlen(kp) as i32,
        mrec as *mut libc::c_void,
    );
    if verbose > 3i32 {
        info!("\n");
    }
    mrec
}
#[no_mangle]
pub unsafe extern "C" fn pdf_read_fontmap_line(
    mut mrec: *mut fontmap_rec,
    mut mline: *const i8,
    mut mline_len: i32,
    mut format: i32,
) -> i32 {
    assert!(!mrec.is_null());
    let mut p = mline;
    let endptr = p.offset(mline_len as isize);
    skip_blank(&mut p, endptr);
    if p >= endptr {
        return -1i32;
    }
    let q = parse_string_value(&mut p, endptr);
    if q.is_null() {
        return -1i32;
    }
    let error = if format > 0i32 {
        /* DVIPDFM format */
        fontmap_parse_mapdef_dpm(mrec, p, endptr)
    } else {
        /* DVIPS/pdfTeX format */
        fontmap_parse_mapdef_dps(mrec, p, endptr)
    };
    if error == 0 {
        let mut sfd_name: *mut i8 = 0 as *mut i8;
        let mut fnt_name = chop_sfd_name(q, &mut sfd_name);
        if !fnt_name.is_null() && !sfd_name.is_null() {
            if (*mrec).font_name.is_null() {
                /* In the case of subfonts, the base name (before the character '@')
                 * will be used as a font_name by default.
                 * Otherwise tex_name will be used as a font_name by default.
                 */
                (*mrec).font_name = fnt_name
            } else {
                free(fnt_name as *mut libc::c_void);
            }
            free((*mrec).charmap.sfd_name as *mut libc::c_void);
            (*mrec).charmap.sfd_name = sfd_name
        }
        fill_in_defaults(mrec, q);
    }
    free(q as *mut libc::c_void);
    error
}
/* DVIPS/pdfTeX fontmap line if one of the following three cases found:
 *
 * (1) any line including the character '"'
 * (2) any line including the character '<'
 * (3) if the line consists of two entries (tfmname and psname)
 *
 * DVIPDFM fontmap line otherwise.
 */
#[no_mangle]
pub unsafe extern "C" fn is_pdfm_mapline(mut mline: *const i8) -> i32
/* NULL terminated. */ {
    let mut n: u32 = 0_u32; /* DVIPS/pdfTeX format */
    if !strchr(mline, '\"' as i32).is_null() || !strchr(mline, '<' as i32).is_null() {
        return -1i32;
    }
    let mut p = mline;
    let endptr = p.offset(strlen(mline) as isize);
    skip_blank(&mut p, endptr);
    while p < endptr {
        /* Break if '-' preceeded by blanks is found. (DVIPDFM format) */
        if *p as i32 == '-' as i32 {
            return 1i32;
        }
        n = n.wrapping_add(1);
        while p < endptr && !(*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _) != 0) {
            p = p.offset(1)
        }
        skip_blank(&mut p, endptr);
    }
    /* Two entries: TFM_NAME PS_NAME only (DVIPS format)
     * Otherwise (DVIPDFM format) */
    if n == 2_u32 {
        0i32
    } else {
        1i32
    }
}
#[no_mangle]
pub unsafe extern "C" fn pdf_load_fontmap_file(filename: &CStr, mut mode: i32) -> i32 {
    let mut p: *const i8 = std::ptr::null();
    let mut lpos: i32 = 0i32;
    let mut error: i32 = 0i32;
    let mut format: i32 = 0i32;
    assert!(!fontmap.is_null());
    if verbose != 0 {
        info!("<FONTMAP:");
    }
    let handle = dpx_tt_open(
        filename.as_ptr(),
        b".map\x00" as *const u8 as *const i8,
        TTInputFormat::FONTMAP,
    );
    if handle.is_none() {
        warn!(
            "Couldn\'t open font map file \"{}\".",
            filename.display()
        );
        return -1i32;
    }
    let mut handle = handle.unwrap();
    while error == 0 && {
        p = tt_readline(work_buffer.as_mut_ptr(), 1024i32, &mut handle);
        !p.is_null()
    } {
        lpos += 1;
        let llen = strlen(work_buffer.as_mut_ptr()) as i32;
        let endptr = p.offset(llen as isize);
        skip_blank(&mut p, endptr);
        if p == endptr {
            continue;
        }
        let m = is_pdfm_mapline(p);
        if format * m < 0i32 {
            /* mismatch */
            warn!(
                "Found a mismatched fontmap line {} from {}.",
                lpos,
                filename.display(),
            );
            warn!(
                "-- Ignore the current input buffer: {}",
                CStr::from_ptr(p).display(),
            );
        } else {
            format += m;
            let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
                as *mut fontmap_rec;
            pdf_init_fontmap_record(mrec);
            /* format > 0: DVIPDFM, format <= 0: DVIPS/pdfTeX */
            error = pdf_read_fontmap_line(mrec, p, llen, format); // CHECK
            if error != 0 {
                warn!(
                    "Invalid map record in fontmap line {} from {}.",
                    lpos,
                    filename.display(),
                );
                warn!(
                    "-- Ignore the current input buffer: {}",
                    CStr::from_ptr(p).display(),
                );
                pdf_clear_fontmap_record(mrec);
                free(mrec as *mut libc::c_void);
            } else {
                match mode {
                    0 => {
                        pdf_insert_fontmap_record((*mrec).map_name, mrec);
                    }
                    43 => {
                        pdf_append_fontmap_record((*mrec).map_name, mrec);
                    }
                    45 => {
                        pdf_remove_fontmap_record((*mrec).map_name);
                    }
                    _ => {}
                }
                pdf_clear_fontmap_record(mrec);
                free(mrec as *mut libc::c_void);
            }
        }
    }
    ttstub_input_close(handle);
    if verbose != 0 {
        info!(">");
    }
    error
}
#[no_mangle]
pub unsafe extern "C" fn pdf_insert_native_fontmap_record(
    mut path: *const i8,
    mut index: u32,
    mut layout_dir: i32,
    mut extend: i32,
    mut slant: i32,
    mut embolden: i32,
) -> *mut fontmap_rec {
    assert!(!path.is_null());
    let fontmap_key = xmalloc(strlen(path).wrapping_add(40) as _) as *mut i8;
    sprintf(
        fontmap_key,
        b"%s/%d/%c/%d/%d/%d\x00" as *const u8 as *const i8,
        path,
        index,
        if layout_dir == 0i32 {
            'H' as i32
        } else {
            'V' as i32
        },
        extend,
        slant,
        embolden,
    );
    if verbose != 0 {
        info!("<NATIVE-FONTMAP:{}", CStr::from_ptr(fontmap_key).display(),);
    }
    let mrec = new((1_u64).wrapping_mul(::std::mem::size_of::<fontmap_rec>() as u64) as u32)
        as *mut fontmap_rec;
    pdf_init_fontmap_record(mrec);
    (*mrec).map_name = fontmap_key;
    (*mrec).enc_name = mstrdup(if layout_dir == 0i32 {
        b"Identity-H\x00" as *const u8 as *const i8
    } else {
        b"Identity-V\x00" as *const u8 as *const i8
    });
    (*mrec).font_name = mstrdup(path);
    (*mrec).opt.index = index as i32;
    if layout_dir != 0i32 {
        (*mrec).opt.flags |= 1i32 << 2i32
    }
    fill_in_defaults(mrec, fontmap_key);
    free(fontmap_key as *mut libc::c_void);
    (*mrec).opt.extend = extend as f64 / 65536.0f64;
    (*mrec).opt.slant = slant as f64 / 65536.0f64;
    (*mrec).opt.bold = embolden as f64 / 65536.0f64;
    let ret = pdf_insert_fontmap_record((*mrec).map_name, mrec);
    pdf_clear_fontmap_record(mrec);
    free(mrec as *mut libc::c_void);
    if verbose != 0 {
        info!(">");
    }
    ret
}
#[no_mangle]
pub unsafe extern "C" fn pdf_lookup_fontmap_record(tfm_name: &[u8]) -> *mut fontmap_rec {
    let mut mrec: *mut fontmap_rec = 0 as *mut fontmap_rec;
    if !fontmap.is_null() && !tfm_name.is_empty() {
        mrec = ht_lookup_table(
            fontmap,
            tfm_name.as_ptr() as *const libc::c_void,
            tfm_name.len() as i32,
        ) as *mut fontmap_rec
    }
    mrec
}
#[no_mangle]
pub unsafe extern "C" fn pdf_init_fontmaps() {
    fontmap =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        fontmap,
        Some(hval_free as unsafe extern "C" fn(_: *mut libc::c_void) -> ()),
    );
}
/* Options */
/* Synthetic font */
/* comaptibility and other flags */
/* currently unused */
/* not implemented yet */
/* unused */
/* Adobe-Japan1-4, etc. */
/* TTC index */
/* ,Bold, etc. */
/* StemV value especially for CJK fonts */
/* Subfont mapping: translate 8-bit charcode to 16-bit charcode
 * via SFD.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_close_fontmaps() {
    if !fontmap.is_null() {
        ht_clear_table(fontmap);
        free(fontmap as *mut libc::c_void);
    }
    fontmap = 0 as *mut ht_table;
    release_sfd_record();
}
/* CIDFont options
 *
 * FORMAT:
 *
 *   (:int:)?!?string(/string)?(,string)?
 */
unsafe fn substr(mut str: *mut *const i8, mut stop: i8) -> *mut i8 {
    let endptr = strchr(*str, stop as i32) as *const i8;
    if endptr.is_null() || endptr == *str {
        return 0 as *mut i8;
    }
    let sstr = new(
        ((endptr.wrapping_offset_from(*str) as i64 + 1i32 as i64) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32,
    ) as *mut i8;
    memcpy(
        sstr as *mut libc::c_void,
        *str as *const libc::c_void,
        endptr.wrapping_offset_from(*str) as _,
    );
    *sstr.offset(endptr.wrapping_offset_from(*str) as i64 as isize) = '\u{0}' as i32 as i8;
    *str = endptr.offset(1);
    sstr
}
/* CIDFont */
unsafe fn strip_options(mut map_name: *const i8, mut opt: *mut fontmap_opt) -> *mut i8 {
    let font_name;
    let mut next: *mut i8 = 0 as *mut i8;
    let mut have_csi: i32 = 0i32;
    let mut have_style: i32 = 0i32;
    assert!(!opt.is_null());
    let mut p = map_name;
    (*opt).charcoll = 0 as *mut i8;
    (*opt).index = 0i32;
    (*opt).style = 0i32;
    (*opt).flags = 0i32;
    if *p as i32 == ':' as i32 && (*p.offset(1) as u8).is_ascii_digit() {
        (*opt).index = strtoul(p.offset(1), &mut next, 10i32) as i32;
        if *next as i32 == ':' as i32 {
            p = next.offset(1)
        } else {
            (*opt).index = 0i32
        }
    }
    if *p as i32 == '!' as i32 {
        /* no-embedding */
        p = p.offset(1);
        if *p as i32 == '\u{0}' as i32 {
            panic!(
                "Invalid map record: {} (--> {})",
                CStr::from_ptr(map_name).display(),
                CStr::from_ptr(p).display(),
            );
        }
        (*opt).flags |= 1i32 << 1i32
    }
    next = strchr(p, '/' as i32);
    if !next.is_null() {
        if next == p as *mut i8 {
            panic!(
                "Invalid map record: {} (--> {})",
                CStr::from_ptr(map_name).display(),
                CStr::from_ptr(p).display(),
            );
        }
        font_name = substr(&mut p, '/' as i32 as i8);
        have_csi = 1i32
    } else {
        next = strchr(p, ',' as i32);
        if !next.is_null() {
            if next == p as *mut i8 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    CStr::from_ptr(map_name).display(),
                    CStr::from_ptr(p).display(),
                );
            }
            font_name = substr(&mut p, ',' as i32 as i8);
            have_style = 1i32
        } else {
            font_name =
                new((strlen(p).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                    as *mut i8;
            strcpy(font_name, p);
        }
    }
    if have_csi != 0 {
        next = strchr(p, ',' as i32);
        if !next.is_null() {
            (*opt).charcoll = substr(&mut p, ',' as i32 as i8);
            have_style = 1i32
        } else if *p.offset(0) as i32 == '\u{0}' as i32 {
            panic!(
                "Invalid map record: {}.",
                CStr::from_ptr(map_name).display()
            );
        } else {
            (*opt).charcoll =
                new((strlen(p).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                    as *mut i8;
            strcpy((*opt).charcoll, p);
        }
    }
    if have_style != 0 {
        if !strstartswith(p, b"BoldItalic\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(10) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    CStr::from_ptr(map_name).display(),
                    CStr::from_ptr(p).display(),
                );
            }
            (*opt).style = 3i32
        } else if !strstartswith(p, b"Bold\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(4) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    CStr::from_ptr(map_name).display(),
                    CStr::from_ptr(p).display(),
                );
            }
            (*opt).style = 1i32
        } else if !strstartswith(p, b"Italic\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(6) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    CStr::from_ptr(map_name).display(),
                    CStr::from_ptr(p).display(),
                );
            }
            (*opt).style = 2i32
        }
    }
    font_name
}
