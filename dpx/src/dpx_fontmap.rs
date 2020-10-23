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

use std::io::{Read, Seek};

use crate::bridge::DisplayExt;
use std::ffi::{CStr, CString};
use std::ptr;

use super::dpx_mfileio::work_buffer;
use crate::strstartswith;
use crate::{info, warn, SkipBlank};

use super::dpx_dpxfile::dpx_tt_open;
use super::dpx_dpxutil::{
    ht_clear_table, ht_init_table, ht_insert_table, ht_lookup_table, ht_remove_table,
};
use super::dpx_dpxutil::{ParseCString, ParseFloatDecimal};
use super::dpx_mem::new;
use super::dpx_mfileio::tt_mfgets;
use super::dpx_subfont::{release_sfd_record, sfd_get_subfont_ids};
use libc::{atof, atoi, free, memcpy, strchr, strcpy, strlen, strtol, strtoul};

use crate::bridge::TTInputFormat;

#[derive(Clone)]
pub(crate) struct fontmap_opt {
    pub(crate) slant: f64,
    pub(crate) extend: f64,
    pub(crate) bold: f64,
    pub(crate) mapc: i32,
    pub(crate) flags: i32,
    pub(crate) otl_tags: String,
    pub(crate) tounicode: String,
    pub(crate) cff_charsets: *mut libc::c_void,
    pub(crate) design_size: f64,
    pub(crate) charcoll: String,
    pub(crate) index: i32,
    pub(crate) style: i32,
    pub(crate) stemv: i32,
}
#[derive(Clone)]
pub(crate) struct fontmap_rec {
    pub(crate) map_name: String,
    pub(crate) font_name: String,
    pub(crate) enc_name: String,
    pub(crate) charmap: C2RustUnnamed_0,
    pub(crate) opt: fontmap_opt,
}
#[derive(Clone)]
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) sfd_name: String,
    pub(crate) subfont_id: String,
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

pub(crate) unsafe fn pdf_fontmap_set_verbose(level: i32) {
    verbose = level;
}

pub(crate) unsafe fn pdf_init_fontmap_record() -> fontmap_rec {
    fontmap_rec {
        map_name: String::new(),
        /* SFD char mapping */
        charmap: C2RustUnnamed_0 {
            sfd_name: String::new(),
            subfont_id: String::new(),
        },
        /* for OFM */
        font_name: String::new(), /* not given explicitly by an option */
        enc_name: String::new(),

        opt: fontmap_opt {
            mapc: -1i32, /* compatibility */
            slant: 0.0f64,
            extend: 1.0f64,
            bold: 0.0f64,
            flags: 0i32,
            design_size: -1.0f64,
            tounicode: String::new(),
            otl_tags: String::new(),
            index: 0i32,
            charcoll: String::new(),
            style: 0i32,
            stemv: -1i32,
            cff_charsets: ptr::null_mut(),
        },
    }
}

unsafe fn pdf_copy_fontmap_record(src: &fontmap_rec) -> fontmap_rec {
    fontmap_rec {
        map_name: src.map_name.clone(),
        charmap: C2RustUnnamed_0 {
            sfd_name: src.charmap.sfd_name.clone(),
            subfont_id: src.charmap.subfont_id.clone(),
        },
        font_name: src.font_name.clone(),
        enc_name: src.enc_name.clone(),
        opt: fontmap_opt {
            design_size: src.opt.design_size, // FIXME?: this was not copied before
            slant: src.opt.slant,
            extend: src.opt.extend,
            bold: src.opt.bold,
            flags: src.opt.flags,
            mapc: src.opt.mapc,
            tounicode: src.opt.tounicode.clone(),
            otl_tags: src.opt.otl_tags.clone(),
            index: src.opt.index,
            charcoll: src.opt.charcoll.clone(),
            style: src.opt.style,
            stemv: src.opt.stemv,
            cff_charsets: src.opt.cff_charsets,
        },
    }
}
unsafe fn hval_free(vp: *mut libc::c_void) {
    let mrec: *mut fontmap_rec = vp as *mut fontmap_rec;
    Box::from_raw(mrec);
}
unsafe fn fill_in_defaults(mrec: &mut fontmap_rec, tex_name: &str) {
    if mrec.enc_name == "default" || mrec.enc_name == "none" {
        mrec.enc_name.clear();
    }

    if mrec.font_name == "default" || mrec.font_name == "none" {
        mrec.font_name.clear();
    }

    /* We *must* fill font_name either explicitly or by default */
    if mrec.font_name.is_empty() {
        mrec.font_name = tex_name.to_owned();
    }
    mrec.map_name = tex_name.to_owned();
    /* Use "UCS" character collection for Unicode SFD
     * and Identity CMap combination. For backward
     * compatibility.
     */
    if mrec.opt.charcoll.is_empty() {
        if (mrec.enc_name == "Identity-H" || mrec.enc_name == "Identity-V")
            && (mrec.charmap.sfd_name.contains("Uni")
                || mrec.charmap.sfd_name.contains("UBig")
                || mrec.charmap.sfd_name.contains("UBg")
                || mrec.charmap.sfd_name.contains("UBg")
                || mrec.charmap.sfd_name.contains("UGB")
                || mrec.charmap.sfd_name.contains("UKS")
                || mrec.charmap.sfd_name.contains("UJIS"))
        {
            mrec.opt.charcoll = "UCS".to_owned();
        }
    };
}
unsafe fn tt_readline<R: Read + Seek>(buf: *mut i8, buf_len: i32, handle: &mut R) -> *mut i8 {
    assert!(!buf.is_null() && buf_len > 0i32);
    let p = tt_mfgets(buf, buf_len, handle);
    if p.is_null() {
        return ptr::null_mut();
    }
    let q = strchr(p, '%' as i32); /* we don't have quoted string */
    if !q.is_null() {
        *q = '\u{0}' as i32 as i8
    }
    p
}
unsafe fn skip_blank(pp: *mut *const i8, endptr: *const i8) {
    let mut p: *const i8 = *pp;
    if p.is_null() || p >= endptr {
        return;
    }
    while p < endptr && (*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _)) {
        p = p.offset(1)
    }
    *pp = p;
}

trait ParseStringValue {
    fn parse_string_value(&mut self) -> String;
    fn parse_string_value_(&mut self) -> Option<CString>;
}

impl ParseStringValue for &[u8] {
    fn parse_string_value(&mut self) -> String {
        if let Some(q) = self.parse_string_value_() {
            q.into_string().unwrap()
        } else {
            String::new()
        }
    }
    fn parse_string_value_(&mut self) -> Option<CString> {
        let q;
        let mut p = *self;
        if p.is_empty() {
            return None;
        }
        if p[0] == b'\"' {
            q = p.parse_c_string();
        } else {
            let mut n = 0;
            while !p.is_empty() && unsafe { libc::isspace(p[0] as _) } == 0 {
                p = &p[1..];
                n += 1;
            }
            if n == 0 {
                return None;
            }
            q = Some(CString::new(&self[..n]).unwrap());
        }
        *self = p;
        q
    }
}

trait ParseIntegerValue {
    fn parse_integer_value(&mut self, base: u8) -> Option<CString>;
}

impl ParseIntegerValue for &[u8] {
    fn parse_integer_value(&mut self, mut base: u8) -> Option<CString> {
        /* no preceeding spaces allowed */
        let mut p = *self;
        let mut has_sign = false;
        let mut has_prefix = false;
        assert!(base == 0 || base >= 2 && base <= 36);
        if p.is_empty() {
            return None;
        }
        if p[0] == b'-' || p[0] == b'+' {
            p = &p[1..];
            has_sign = true
        }
        if (base == 0 || base == 16) && p.len() >= 2 && p[0] == b'0' && p[1] == b'x' {
            p = &p[2..];
            has_prefix = true
        }
        if base == 0 {
            if has_prefix {
                base = 16
            } else if !p.is_empty() && p[0] == b'0' {
                base = 8
            } else {
                base = 10
            }
        }
        let mut n = 0;
        while !p.is_empty()
            && (base <= 10 && p[0] >= b'0' && p[0] < b'0' + base
                || base > 10
                    && (p[0] >= b'0' && p[0] <= b'9'
                        || p[0] >= b'a' && p[0] < b'a' + (base - 10)
                        || p[0] >= b'A' && p[0] < b'A' + (base - 10)))
        {
            p = &p[1..];
            n += 1;
        }
        if n == 0 {
            return None;
        }
        if has_sign {
            n += 1;
        }
        if has_prefix {
            n += 2;
        }
        let q = Some(CString::new(&self[..n]).unwrap());
        *self = p;
        q
    }
}

unsafe fn fontmap_parse_mapdef_dpm(mrec: &mut fontmap_rec, mapdef: &[u8]) -> i32 {
    let mut p = mapdef;
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
    p.skip_blank();
    /* encoding field */
    if !p.is_empty() && p[0] != b'-' {
        /* May be NULL */
        mrec.enc_name = p.parse_string_value();
        p.skip_blank();
    }
    /* fontname or font filename field */
    if !p.is_empty() && p[0] != b'-' {
        /* May be NULL */
        mrec.font_name = p.parse_string_value();
        p.skip_blank();
    }
    if !mrec.font_name.is_empty() {
        /* Several options are encoded in font_name for
         * compatibility with dvipdfm.
         */
        let tmp = strip_options(&mrec.font_name, &mut mrec.opt);
        if !tmp.is_null() {
            mrec.font_name = CStr::from_ptr(tmp).to_str().unwrap().to_owned()
        }
    }
    p.skip_blank();
    /* Parse any remaining arguments */
    while p.len() > 1 && p[0] != b'\r' && p[0] != b'\n' && p[0] == b'-' {
        let mopt = p[1] as u8;
        p = &p[2..];
        p.skip_blank();
        match mopt {
            b's' => {
                /* Slant option */
                if let Some(q) = p.parse_float_decimal() {
                    mrec.opt.slant = atof(q.as_ptr());
                } else {
                    warn!("Missing a number value for \'s\' option.");
                    return -1i32;
                }
            }
            b'e' => {
                /* Extend option */
                if let Some(q) = p.parse_float_decimal() {
                    mrec.opt.extend = atof(q.as_ptr());
                    if mrec.opt.extend <= 0. {
                        warn!("Invalid value for \'e\' option: {}", q.display());
                        return -1i32;
                    }
                } else {
                    warn!("Missing a number value for \'e\' option.");
                    return -1i32;
                }
            }
            b'b' => {
                /* Fake-bold option */
                if let Some(q) = p.parse_float_decimal() {
                    mrec.opt.bold = atof(q.as_ptr());
                    if mrec.opt.bold <= 0. {
                        warn!("Invalid value for \'b\' option: {}", q.display());
                        return -1i32;
                    }
                } else {
                    warn!("Missing a number value for \'b\' option.");
                    return -1i32;
                }
            }
            b'r' => {}
            b'i' => {
                /* TTC index */
                if let Some(q) = p.parse_integer_value(10) {
                    mrec.opt.index = atoi(q.as_ptr());
                    if mrec.opt.index < 0 {
                        warn!("Invalid TTC index number: {}", q.display());
                        return -1i32;
                    }
                } else {
                    warn!("Missing TTC index number...");
                    return -1i32;
                }
            }
            b'p' => {
                /* UCS plane: just for testing */
                if let Some(q) = p.parse_integer_value(0) {
                    let v = strtol(q.as_ptr(), 0 as *mut *mut i8, 0) as i32;
                    if v < 0 || v > 16 {
                        warn!("Invalid value for option \'p\': {}", q.display());
                    } else {
                        mrec.opt.mapc = v << 16;
                    }
                } else {
                    warn!("Missing a number for \'p\' option.");
                    return -1i32;
                }
            }
            b'u' => {
                /* ToUnicode */
                let q = p.parse_string_value();
                if !q.is_empty() {
                    mrec.opt.tounicode = q;
                } else {
                    warn!("Missing string value for option \'u\'.");
                    return -1i32;
                }
            }
            b'v' => {
                /* StemV */
                if let Some(q) = p.parse_integer_value(10) {
                    mrec.opt.stemv = strtol(q.as_ptr(), 0 as *mut *mut i8, 0i32) as i32;
                } else {
                    warn!("Missing a number for \'v\' option.");
                    return -1i32;
                }
            }
            b'l' => {
                /* 2017.4.15 back again */
                let q = p.parse_string_value();
                if !q.is_empty() {
                    mrec.opt.otl_tags = q;
                } else {
                    warn!("Missing string value for option \'l\'.");
                    return -1i32;
                }
            }
            b'm' => {
                /* Omega uses both single-byte and double-byte set_char command
                 * even for double-byte OFMs. This confuses CMap decoder.
                 */
                /* Map single bytes char 0xab to double byte char 0xcdab  */
                if p.len() >= 4 && p[0] == b'<' && p[3] == b'>' {
                    p = &p[1..];
                    if let Some(q) = p.parse_integer_value(16) {
                        if !p.is_empty() && p[0] != b'>' {
                            warn!("Invalid value for option \'m\': {}", q.display());
                            return -1i32;
                        }
                        let v = strtol(q.as_ptr(), 0 as *mut *mut i8, 16i32) as i32;
                        mrec.opt.mapc = ((v << 8i32) as i64 & 0xff00) as i32;
                        p = &p[1..];
                    } else {
                        warn!("Invalid value for option \'m\'.");
                        return -1i32;
                    }
                } else if p.starts_with(b"sfd:") {
                    /* SFD mapping: sfd:Big5,00 */
                    p = &p[4..];
                    p.skip_blank();
                    let q = p.parse_string_value();
                    if q.is_empty() {
                        warn!("Missing value for option \'m\'.");
                        return -1i32;
                    }
                    let comma = match q.find(',') {
                        Some(idx) => idx,
                        None => {
                            warn!("Invalid value for option \'m\': {}", q,);
                            return -1i32;
                        }
                    };
                    let sfd_name = q[1..comma].to_owned();
                    let mut subfont_id = &q[comma + 1..];
                    while !subfont_id.is_empty() && crate::isblank(subfont_id.as_bytes()[0] as _) {
                        subfont_id = &subfont_id[1..];
                    }
                    if subfont_id.is_empty() {
                        warn!("Invalid value for option \'m\': {},", q);
                        return -1i32;
                    }
                    mrec.charmap.sfd_name = sfd_name;
                    mrec.charmap.subfont_id = subfont_id.to_owned();
                } else if p.starts_with(b"pad:") {
                    p = &p[4..];
                    p.skip_blank();
                    if let Some(q) = p.parse_integer_value(16) {
                        if !p.is_empty() && libc::isspace(p[0] as _) == 0 {
                            warn!("Invalid value for option \'m\': {}", q.display());
                            return -1i32;
                        }
                        let v = strtol(q.as_ptr(), 0 as *mut *mut i8, 16i32) as i32;
                        mrec.opt.mapc = ((v << 8i32) as i64 & 0xff00) as i32;
                    } else {
                        warn!("Invalid value for option \'m\'.");
                        return -1i32;
                    }
                } else {
                    warn!("Invalid value for option \'m\'.");
                    return -1i32;
                }
            }
            b'w' => {
                /* Writing mode (for unicode encoding) */
                if mrec.enc_name != "unicode" {
                    warn!("Fontmap option \'w\' meaningless for encoding other than \"unicode\".");
                    return -1i32;
                }
                if let Some(q) = p.parse_integer_value(10) {
                    if atoi(q.as_ptr()) == 1 {
                        mrec.opt.flags |= 1 << 2;
                    } else if atoi(q.as_ptr()) == 0 {
                        mrec.opt.flags &= !(1 << 2)
                    } else {
                        warn!("Invalid value for option \'w\': {}", q.display());
                    }
                } else {
                    warn!("Missing wmode value...");
                    return -1i32;
                }
            }
            _ => {
                warn!("Unrecognized font map option: \'{}\'", char::from(mopt),);
                return -1i32;
            }
        }
        p.skip_blank();
    }
    if !p.is_empty() && p[0] != b'\r' && p[0] != b'\n' {
        warn!("Invalid char in fontmap line: {}", char::from(p[0]));
        return -1i32;
    }
    0i32
}
/* Parse record line in map file of DVIPS/pdfTeX format. */
unsafe fn fontmap_parse_mapdef_dps(mrec: &mut fontmap_rec, mapdef: &[u8]) -> i32 {
    let mut p = mapdef;
    p.skip_blank();
    /* The first field (after TFM name) must be PostScript name. */
    /* However, pdftex.map allows a line without PostScript name. */
    if p[0] != b'\"' && p[0] != b'<' {
        if !p.is_empty() {
            let _ = p.parse_string_value();
            p.skip_blank();
        } else {
            warn!("Missing a PostScript font name.");
            return -1i32;
        }
    }
    if p.is_empty() {
        return 0i32;
    }
    /* Parse any remaining arguments */
    while !p.is_empty() && p[0] != b'\r' && p[0] != b'\n' && (p[0] == b'<' || p[0] == b'\"') {
        match p[0] {
            b'<' => {
                p = &p[1..]; /*skip */
                if !p.is_empty() && (p[0] == b'[' || p[0] == b'<') {
                    p = &p[1..];
                }
                p.skip_blank();
                let q = p.parse_string_value();
                if !q.is_empty() {
                    if q.ends_with(".enc") {
                        mrec.enc_name = q;
                    } else {
                        mrec.font_name = q;
                    }
                }
                p.skip_blank();
            }
            b'"' => {
                /* encoding or fontfile field */
                /* If we see <[ or <<, just ignore the second char instead
                of doing as directed (define encoding file, fully embed); sorry.  */
                /* Options */
                if let Some(q) = p.parse_string_value_() {
                    let mut r = q.to_bytes();
                    p.skip_blank();
                    while !r.is_empty() {
                        if let Some(s) = r.parse_float_decimal() {
                            r.skip_blank();
                            if let Some(t) = r.parse_string_value_() {
                                let t = t.to_bytes();
                                if t == b"SlantFont" {
                                    mrec.opt.slant = atof(s.as_ptr())
                                } else if t == b"ExtendFont" {
                                    mrec.opt.extend = atof(s.as_ptr())
                                }
                            }
                        } else {
                            let _ = r.parse_string_value_(); /* skip */
                        }
                        r.skip_blank();
                    }
                }
                p.skip_blank();
            }
            _ => {
                warn!("Found an invalid entry: {}", p.display());
                return -1i32;
            }
        }
        p.skip_blank();
    }
    if !p.is_empty() && p[0] != b'\r' && p[0] != b'\n' {
        warn!("Invalid char in fontmap line: {}", char::from(p[0]));
        return -1i32;
    }
    0i32
}
static mut fontmap: *mut ht_table = std::ptr::null_mut();
fn chop_sfd_name(tex_name: &str) -> Option<(String, String)> {
    let mut it = tex_name.split('@');
    let fontname = it.next()?;
    if fontname.is_empty() {
        return None;
    }
    let sfd_name = it.next()?;
    if sfd_name.is_empty() {
        return None;
    }
    if let Some(q) = it.next() {
        // TODO: check '@' more than 2
        Some((fontname.to_string() + q, sfd_name.to_string()))
    } else {
        Some((fontname.to_string(), sfd_name.to_string()))
    }
}
unsafe fn make_subfont_name(map_name: &str, sfd_name: &str, sub_id: &str) -> Option<String> {
    let mut it = map_name.split('@');
    let m = it.next()?;
    if m.is_empty() {
        return None;
    }
    let n = it.next()?;
    if n.is_empty() {
        return None;
    }
    if n != sfd_name {
        return None;
    }
    let tfm_name = m.to_string() + sub_id;
    if let Some(q) = it.next() {
        Some(tfm_name + q)
    } else {
        Some(tfm_name)
    }
}
/* "foo@A@ ..." is expanded to
 *   fooab ... -m sfd:A,ab
 *   ...
 *   fooyz ... -m sfd:A,yz
 * where 'ab' ... 'yz' is subfont IDs in SFD 'A'.
 */

pub(crate) unsafe fn pdf_append_fontmap_record(kp: &str, vp: &fontmap_rec) -> i32 {
    if kp.is_empty() || (vp.map_name.is_empty() || vp.font_name.is_empty()) {
        warn!("Invalid fontmap record...");
        return -1i32;
    }
    if verbose > 3i32 {
        info!("fontmap>> append key=\"{}\"...", kp);
    }
    if let Some((_, sfd_name)) = chop_sfd_name(kp) {
        /* link to this entry */
        let subfont_ids = sfd_get_subfont_ids(&sfd_name);
        if subfont_ids.is_empty() {
            return -1i32;
        }
        for n in (0..subfont_ids.len()).rev() {
            if let Some(tfm_name) = make_subfont_name(kp, &sfd_name, &subfont_ids[n]) {
                let mrec = ht_lookup_table(
                    fontmap,
                    tfm_name.as_bytes().as_ptr() as *const libc::c_void,
                    tfm_name.as_bytes().len() as i32,
                ) as *mut fontmap_rec;
                if mrec.is_null() {
                    let mut mrec = pdf_init_fontmap_record();
                    mrec.map_name = kp.to_string();
                    mrec.charmap.sfd_name = sfd_name.clone();
                    mrec.charmap.subfont_id = subfont_ids[n].clone();
                    ht_insert_table(
                        fontmap,
                        tfm_name.as_bytes().as_ptr() as *const libc::c_void,
                        tfm_name.as_bytes().len() as i32,
                        Box::into_raw(Box::new(mrec)) as *mut libc::c_void,
                    );
                }
            }
        }
    }
    let mrec = ht_lookup_table(
        fontmap,
        kp.as_bytes().as_ptr() as *const libc::c_void,
        kp.as_bytes().len() as i32,
    ) as *mut fontmap_rec;
    if mrec.is_null() {
        let mrec = Box::into_raw(Box::new(pdf_copy_fontmap_record(vp)));
        if !(*mrec).map_name.is_empty() && kp == (*mrec).map_name {
            (*mrec).map_name.clear();
        }
        ht_insert_table(
            fontmap,
            kp.as_bytes().as_ptr() as *const libc::c_void,
            kp.as_bytes().len() as i32,
            mrec as *mut libc::c_void,
        );
    }
    if verbose > 3i32 {
        info!("\n");
    }
    0i32
}

pub(crate) unsafe fn pdf_remove_fontmap_record(kp: &str) -> i32 {
    if kp.is_empty() {
        return -1;
    }
    if verbose > 3 {
        info!("fontmap>> remove key=\"{}\"...", kp);
    }

    if let Some((_, sfd_name)) = chop_sfd_name(kp) {
        let subfont_ids = sfd_get_subfont_ids(&sfd_name);
        if subfont_ids.is_empty() {
            return -1;
        }
        let len = subfont_ids.len();
        if verbose > 3 {
            info!("\nfontmap>> Expand @{}@:", sfd_name);
        }
        for n in (0..len).rev() {
            if let Some(tfm_name) = make_subfont_name(kp, &sfd_name, &subfont_ids[n]) {
                if verbose > 3 {
                    info!(" {}", tfm_name);
                }
                ht_remove_table(
                    fontmap,
                    tfm_name.as_bytes().as_ptr() as *const libc::c_void,
                    tfm_name.as_bytes().len() as i32,
                );
            }
        }
    }
    ht_remove_table(
        fontmap,
        kp.as_bytes().as_ptr() as *const libc::c_void,
        kp.as_bytes().len() as i32,
    );
    if verbose > 3i32 {
        info!("\n");
    }
    0i32
}

pub(crate) unsafe fn pdf_insert_fontmap_record(kp: &str, vp: &fontmap_rec) -> *mut fontmap_rec {
    if kp.is_empty() || (vp.map_name.is_empty() || vp.font_name.is_empty()) {
        warn!("Invalid fontmap record...");
        return ptr::null_mut();
    }
    if verbose > 3i32 {
        info!("fontmap>> insert key=\"{}\"...", kp);
    }
    if let Some((_, sfd_name)) = chop_sfd_name(kp) {
        let subfont_ids = sfd_get_subfont_ids(&sfd_name);
        if subfont_ids.is_empty() {
            warn!("Could not open SFD file: {}", sfd_name);
            return ptr::null_mut();
        }
        let len = subfont_ids.len();
        if verbose > 3i32 {
            info!("\nfontmap>> Expand @{}@:", sfd_name);
        }
        for n in (0..len).rev() {
            if let Some(tfm_name) = make_subfont_name(kp, &sfd_name, &subfont_ids[n]) {
                if verbose > 3i32 {
                    info!(" {}", tfm_name);
                }
                let mut mrec = pdf_init_fontmap_record();
                mrec.map_name = kp.to_string();
                mrec.charmap.sfd_name = sfd_name.clone();
                mrec.charmap.subfont_id = subfont_ids[n].clone();
                ht_insert_table(
                    fontmap,
                    tfm_name.as_bytes().as_ptr() as *const libc::c_void,
                    tfm_name.as_bytes().len() as i32,
                    Box::into_raw(Box::new(mrec)) as *mut libc::c_void,
                );
            }
        }
    }
    let mrec = Box::into_raw(Box::new(pdf_copy_fontmap_record(vp)));
    if kp == (*mrec).map_name {
        (*mrec).map_name.clear();
    }
    ht_insert_table(
        fontmap,
        kp.as_bytes().as_ptr() as *const libc::c_void,
        kp.as_bytes().len() as i32,
        mrec as *mut libc::c_void,
    );
    if verbose > 3i32 {
        info!("\n");
    }
    mrec
}

pub(crate) unsafe fn pdf_read_fontmap_line(
    mrec: &mut fontmap_rec,
    mline: *const i8,
    mline_len: i32,
    format: i32,
) -> i32 {
    let mut p = std::slice::from_raw_parts(mline as *const u8, mline_len as usize);
    p.skip_blank();
    if p.is_empty() {
        return -1i32;
    }
    let q = p.parse_string_value_();
    if q.is_none() {
        return -1i32;
    }
    let q = q.unwrap();
    let qstr = q.to_str().unwrap();
    let error = if format > 0i32 {
        /* DVIPDFM format */
        fontmap_parse_mapdef_dpm(mrec, p)
    } else {
        /* DVIPS/pdfTeX format */
        fontmap_parse_mapdef_dps(mrec, p)
    };
    if error == 0 {
        if let Some((fnt_name, sfd_name)) = chop_sfd_name(qstr) {
            if mrec.font_name.is_empty() {
                /* In the case of subfonts, the base name (before the character '@')
                 * will be used as a font_name by default.
                 * Otherwise tex_name will be used as a font_name by default.
                 */
                mrec.font_name = fnt_name;
            }
            mrec.charmap.sfd_name = sfd_name;
        }
        fill_in_defaults(mrec, &qstr);
    }
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

pub(crate) unsafe fn is_pdfm_mapline(mline: *const i8) -> i32
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
        while p < endptr && !(*p as i32 & !0x7fi32 == 0i32 && crate::isblank(*p as _)) {
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

pub(crate) unsafe fn pdf_load_fontmap_file(filename: &str, mode: i32) -> i32 {
    let mut p: *const i8 = std::ptr::null();
    let mut lpos: i32 = 0i32;
    let mut error: i32 = 0i32;
    let mut format: i32 = 0i32;
    assert!(!fontmap.is_null());
    if verbose != 0 {
        info!("<FONTMAP:");
    }
    let handle = dpx_tt_open(filename, ".map", TTInputFormat::FONTMAP);
    if handle.is_none() {
        warn!("Couldn\'t open font map file \"{}\".", filename);
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
                lpos, filename,
            );
            warn!(
                "-- Ignore the current input buffer: {}",
                CStr::from_ptr(p).display(),
            );
        } else {
            format += m;
            let mut mrec = pdf_init_fontmap_record();
            /* format > 0: DVIPDFM, format <= 0: DVIPS/pdfTeX */
            error = pdf_read_fontmap_line(&mut mrec, p, llen, format); // CHECK
            if error != 0 {
                warn!(
                    "Invalid map record in fontmap line {} from {}.",
                    lpos, filename,
                );
                warn!(
                    "-- Ignore the current input buffer: {}",
                    CStr::from_ptr(p).display(),
                );
            } else {
                match mode {
                    0 => {
                        pdf_insert_fontmap_record(&mrec.map_name, &mrec);
                    }
                    43 => {
                        pdf_append_fontmap_record(&mrec.map_name, &mrec);
                    }
                    45 => {
                        pdf_remove_fontmap_record(&mrec.map_name);
                    }
                    _ => {}
                }
            }
        }
    }
    if verbose != 0 {
        info!(">");
    }
    error
}

pub(crate) unsafe fn pdf_insert_native_fontmap_record(
    path: &str,
    index: u32,
    layout_dir: i32,
    extend: i32,
    slant: i32,
    embolden: i32,
) -> *mut fontmap_rec {
    let fontmap_key = format!(
        "{}/{}/{}/{}/{}/{}",
        path,
        index,
        if layout_dir == 0i32 { 'H' } else { 'V' },
        extend,
        slant,
        embolden,
    );
    if verbose != 0 {
        info!("<NATIVE-FONTMAP:{}", fontmap_key);
    }
    let mut mrec = pdf_init_fontmap_record();
    mrec.enc_name = (if layout_dir == 0 {
        "Identity-H"
    } else {
        "Identity-V"
    })
    .to_owned();
    mrec.font_name = path.to_owned();
    mrec.opt.index = index as i32;
    if layout_dir != 0i32 {
        mrec.opt.flags |= 1i32 << 2i32
    }
    fill_in_defaults(&mut mrec, &fontmap_key);
    mrec.opt.extend = extend as f64 / 65536.;
    mrec.opt.slant = slant as f64 / 65536.;
    mrec.opt.bold = embolden as f64 / 65536.;
    let ret = pdf_insert_fontmap_record(&mrec.map_name, &mrec);
    if verbose != 0 {
        info!(">");
    }
    ret
}

pub(crate) unsafe fn pdf_lookup_fontmap_record(tfm_name: &[u8]) -> *mut fontmap_rec {
    let mut mrec: *mut fontmap_rec = ptr::null_mut();
    if !fontmap.is_null() && !tfm_name.is_empty() {
        mrec = ht_lookup_table(
            fontmap,
            tfm_name.as_ptr() as *const libc::c_void,
            tfm_name.len() as i32,
        ) as *mut fontmap_rec
    }
    mrec
}

pub(crate) unsafe fn pdf_init_fontmaps() {
    fontmap =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        fontmap,
        Some(hval_free as unsafe fn(_: *mut libc::c_void) -> ()),
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

pub(crate) unsafe fn pdf_close_fontmaps() {
    if !fontmap.is_null() {
        ht_clear_table(fontmap);
        free(fontmap as *mut libc::c_void);
    }
    fontmap = ptr::null_mut();
    release_sfd_record();
}
/* CIDFont options
 *
 * FORMAT:
 *
 *   (:int:)?!?string(/string)?(,string)?
 */
unsafe fn substr(str: *mut *const i8, stop: i8) -> *mut i8 {
    let endptr = strchr(*str, stop as i32) as *const i8;
    if endptr.is_null() || endptr == *str {
        return ptr::null_mut();
    }
    let sstr = new(
        ((endptr.offset_from(*str) as i64 + 1i32 as i64) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32,
    ) as *mut i8;
    memcpy(
        sstr as *mut libc::c_void,
        *str as *const libc::c_void,
        endptr.offset_from(*str) as _,
    );
    *sstr.offset(endptr.offset_from(*str) as i64 as isize) = '\u{0}' as i32 as i8;
    *str = endptr.offset(1);
    sstr
}
/* CIDFont */
unsafe fn strip_options(map_name: &str, opt: &mut fontmap_opt) -> *mut i8 {
    let font_name;
    let mut next: *mut i8 = ptr::null_mut();
    let mut have_csi = false;
    let mut have_style = false;
    let map_name_ = CString::new(map_name).unwrap();
    let mut p = map_name_.as_ptr();
    opt.charcoll.clear();
    opt.index = 0;
    opt.style = 0;
    opt.flags = 0;
    if *p as i32 == ':' as i32 && (*p.offset(1) as u8).is_ascii_digit() {
        opt.index = strtoul(p.offset(1), &mut next, 10i32) as i32;
        if *next as i32 == ':' as i32 {
            p = next.offset(1)
        } else {
            opt.index = 0;
        }
    }
    if *p as i32 == '!' as i32 {
        /* no-embedding */
        p = p.offset(1);
        if *p as i32 == '\u{0}' as i32 {
            panic!(
                "Invalid map record: {} (--> {})",
                map_name,
                CStr::from_ptr(p).display(),
            );
        }
        opt.flags |= 1 << 1;
    }
    next = strchr(p, '/' as i32);
    if !next.is_null() {
        if next == p as *mut i8 {
            panic!(
                "Invalid map record: {} (--> {})",
                map_name,
                CStr::from_ptr(p).display(),
            );
        }
        font_name = substr(&mut p, '/' as i32 as i8);
        have_csi = true;
    } else {
        next = strchr(p, ',' as i32);
        if !next.is_null() {
            if next == p as *mut i8 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    map_name,
                    CStr::from_ptr(p).display(),
                );
            }
            font_name = substr(&mut p, ',' as i32 as i8);
            have_style = true;
        } else {
            font_name =
                new((strlen(p).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                    as *mut i8;
            strcpy(font_name, p);
        }
    }
    if have_csi {
        next = strchr(p, ',' as i32);
        if !next.is_null() {
            opt.charcoll = CStr::from_ptr(substr(&mut p, ',' as i32 as i8))
                .to_str()
                .unwrap()
                .to_owned();
            have_style = true;
        } else if *p.offset(0) as i32 == '\u{0}' as i32 {
            panic!("Invalid map record: {}.", map_name);
        } else {
            opt.charcoll = CStr::from_ptr(p).to_str().unwrap().to_owned();
        }
    }
    if have_style {
        if !strstartswith(p, b"BoldItalic\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(10) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    map_name,
                    CStr::from_ptr(p).display(),
                );
            }
            opt.style = 3i32
        } else if !strstartswith(p, b"Bold\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(4) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    map_name,
                    CStr::from_ptr(p).display(),
                );
            }
            opt.style = 1i32
        } else if !strstartswith(p, b"Italic\x00" as *const u8 as *const i8).is_null() {
            if *p.offset(6) != 0 {
                panic!(
                    "Invalid map record: {} (--> {})",
                    map_name,
                    CStr::from_ptr(p).display(),
                );
            }
            opt.style = 2i32
        }
    }
    font_name
}
