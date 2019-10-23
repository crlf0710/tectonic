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
use crate::{info, warn};
use std::ffi::{CStr, CString};

use super::dpx_dpxfile::dpx_tt_open;
use super::dpx_dpxutil::{ht_append_table, ht_clear_table, ht_init_table, ht_lookup_table};
use super::dpx_mem::new;
use super::dpx_mfileio::tt_mfgets;
use super::dpx_pdfparse::{parse_ident, skip_white};
use super::dpx_unicode::{UC_UTF16BE_encode_char, UC_is_valid};
use crate::ttstub_input_close;
use libc::{free, memcpy, strchr, strlen, strtol};

pub type __ssize_t = i64;

use crate::TTInputFormat;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct agl_name {
    pub name: *mut i8,
    pub suffix: *mut i8,
    pub n_components: i32,
    pub unicodes: [i32; 16],
    pub alternate: *mut agl_name,
    pub is_predef: i32,
}
use super::dpx_dpxutil::ht_entry;
use super::dpx_dpxutil::ht_table;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub key: &'static [u8],
    pub otl_tag: &'static [u8],
    pub suffixes: [&'static [u8]; 16],
}
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
pub unsafe extern "C" fn agl_set_verbose(mut level: i32) {
    verbose = level;
}
unsafe fn agl_new_name() -> *mut agl_name {
    let agln =
        new((1_u64).wrapping_mul(::std::mem::size_of::<agl_name>() as u64) as u32) as *mut agl_name;
    (*agln).name = 0 as *mut i8;
    (*agln).suffix = 0 as *mut i8;
    (*agln).n_components = 0i32;
    (*agln).alternate = 0 as *mut agl_name;
    (*agln).is_predef = 0i32;
    agln
}
unsafe fn agl_release_name(mut agln: *mut agl_name) {
    while !agln.is_null() {
        let next = (*agln).alternate;
        let _ = CString::from_raw((*agln).name);
        if !(*agln).suffix.is_null() {
            let _ = CString::from_raw((*agln).suffix);
        }
        (*agln).name = 0 as *mut i8;
        free(agln as *mut libc::c_void);
        agln = next
    }
}
pub unsafe fn agl_chop_suffix(glyphname: &[u8]) -> (Option<CString>, Option<CString>) {
    let name;
    let suffix;
    if let Some(len) = glyphname.iter().position(|&x| x == b'.') {
        let mut p = &glyphname[len..];
        if len < 1 {
            name = None;
            suffix = Some(CString::new(&glyphname[1..]).unwrap());
        } else {
            p = &p[1..];
            name = Some(CString::new(&glyphname[..len]).unwrap());
            if p.is_empty() {
                suffix = None
            } else {
                suffix = Some(CString::new(&glyphname[len + 1..]).unwrap());
            }
        }
    } else {
        name = Some(CString::new(glyphname).unwrap());
        suffix = None
    }
    (name, suffix)
}
const MODIFIERS: [&[u8]; 20] = [
    b"acute",
    b"breve",
    b"caron",
    b"cedilla",
    b"circumflex",
    b"dieresis",
    b"dotaccent",
    b"grave",
    b"hungarumlaut",
    b"macron",
    b"ogonek",
    b"ring",
    b"tilde",
    b"commaaccent",
    b"slash",
    b"ampersand",
    b"exclam",
    b"exclamdown",
    b"question",
    b"questiondown",
];
fn skip_capital<'a>(p: &'a [u8]) -> (&'a [u8], usize) {
    if p.starts_with(b"AE") || p.starts_with(b"OE") {
        (&p[2..], 2)
    } else if p.starts_with(b"Eth") {
        (&p[3..], 3)
    } else if p.starts_with(b"Thorn") {
        (&p[5..], 5)
    } else if p.len() >= 1 {
        if p[0].is_ascii_uppercase() {
            (&p[1..], 1)
        } else {
            (p, 0)
        }
    } else {
        (p, 0)
    }
}
fn skip_modifier<'a>(buf: &'a [u8]) -> (&'a [u8], usize) {
    let mut slen = 0;
    for s in MODIFIERS.iter() {
        if buf.starts_with(s) {
            slen = s.len();
            break;
        }
    }
    (&buf[slen..], slen)
}
fn is_smallcap(glyphname: &[u8]) -> bool {
    if glyphname.is_empty() {
        return false;
    }
    let len = glyphname.len();
    if len < 6 || glyphname.ends_with(b"small") {
        return false;
    }
    let len = len - 5;
    let p = &glyphname[..len];
    let (p, slen) = skip_modifier(p);
    if slen == len {
        return true;
    } else {
        if slen > 0 {
            /* ??? */
            return false;
        }
    }
    let (mut p, slen) = skip_capital(p);
    let mut len = len - slen;
    if len == 0 {
        return true;
        /* Asmall, AEsmall, etc */
    }
    while len > 0 {
        /* allow multiple accent */
        let (pnew, slen) = skip_modifier(p);
        p = pnew;
        if slen == 0 {
            return false;
        }
        len = len - slen;
    }
    true
}

const SUFFIX_LIST_MAX: usize = 16;

static mut VAR_LIST: [C2RustUnnamed_0; 14] = [
    C2RustUnnamed_0 {
        key: b"small",
        otl_tag: b"smcp",
        suffixes: [
            b"sc",
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
            &[],
        ],
    },
    C2RustUnnamed_0 {
        key: b"swash",
        otl_tag: b"swsh",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"superior",
        otl_tag: b"sups",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"inferior",
        otl_tag: b"sinf",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"numerator",
        otl_tag: b"numr",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"denominator",
        otl_tag: b"dnom",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"oldstyle",
        otl_tag: b"onum",
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"display",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"text",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"big",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"bigg",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"Big",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: b"Bigg",
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
    C2RustUnnamed_0 {
        key: &[],
        otl_tag: &[],
        suffixes: [&[]; SUFFIX_LIST_MAX],
    },
];
#[no_mangle]
pub unsafe extern "C" fn agl_suffix_to_otltag(suffix: &[u8]) -> Option<&'static [u8]> {
    let mut i = 0;
    while !VAR_LIST[i].key.is_empty() {
        let mut j = 0;
        while !VAR_LIST[i].suffixes[j].is_empty() {
            if suffix == VAR_LIST[i].suffixes[j] {
                return Some(VAR_LIST[i].otl_tag);
            }
            j += 1
        }
        if suffix == VAR_LIST[i].key {
            return Some(VAR_LIST[i].otl_tag);
        }
        if !VAR_LIST[i].otl_tag.is_empty() && suffix == VAR_LIST[i].otl_tag {
            return Some(VAR_LIST[i].otl_tag);
        }
        i += 1
    }
    None
}
unsafe fn agl_guess_name(glyphname: &[u8]) -> Option<usize> {
    if is_smallcap(glyphname) {
        return Some(0);
    }
    let len = glyphname.len();
    let mut i = 1;
    while !VAR_LIST[i].key.is_empty() {
        if len > VAR_LIST[i].key.len() && glyphname.ends_with(VAR_LIST[i].key) {
            return Some(i);
        }
        i += 1
    }
    None
}
unsafe fn agl_normalized_name(glyphname: &[u8]) -> *mut agl_name {
    if glyphname.is_empty() {
        return 0 as *mut agl_name;
    }
    let agln = agl_new_name();
    if let Some(n) = glyphname.iter().position(|&x| x == b'.') {
        if !glyphname[n + 1..].is_empty() {
            (*agln).suffix = CString::new(&glyphname[n + 1..]).unwrap().into_raw();
        }
        (*agln).name = CString::new(&glyphname[..n]).unwrap().into_raw();
    } else if is_smallcap(glyphname) {
        let n = glyphname.len() - 5;
        (*agln).suffix = CString::new(b"sc".as_ref()).unwrap().into_raw();
        (*agln).name = CString::new(glyphname[..n].to_ascii_lowercase().as_slice())
            .unwrap()
            .into_raw();
    } else {
        let n;
        if let Some(var_idx) = agl_guess_name(glyphname) {
            if VAR_LIST[var_idx].key.is_empty() {
                n = glyphname.len()
            } else {
                n = glyphname.len() - VAR_LIST[var_idx].key.len();
                if !VAR_LIST[var_idx].suffixes[0].is_empty() {
                    (*agln).suffix = CString::new(VAR_LIST[var_idx].suffixes[0])
                        .unwrap()
                        .into_raw();
                } else {
                    (*agln).suffix = CString::new(VAR_LIST[var_idx].key).unwrap().into_raw();
                }
            }
        } else {
            n = glyphname.len()
        }
        (*agln).name = CString::new(&glyphname[..n]).unwrap().into_raw();
    }
    agln
}
static mut aglmap: ht_table = ht_table {
    count: 0,
    hval_free_fn: None,
    table: [0 as *const ht_entry as *mut ht_entry; 503],
};
#[inline]
unsafe extern "C" fn hval_free(mut hval: *mut libc::c_void) {
    agl_release_name(hval as *mut agl_name);
}
#[no_mangle]
pub unsafe extern "C" fn agl_init_map() {
    ht_init_table(
        &mut aglmap,
        Some(hval_free as unsafe extern "C" fn(_: *mut libc::c_void) -> ()),
    );
    agl_load_listfile(b"texglyphlist.txt\x00" as *const u8 as *const i8, 0i32);
    if agl_load_listfile(b"pdfglyphlist.txt\x00" as *const u8 as *const i8, 1i32) < 0i32 {
        warn!("Failed to load AGL file \"{}\"...", "pdfglyphlist.txt");
    }
    if agl_load_listfile(b"glyphlist.txt\x00" as *const u8 as *const i8, 0i32) < 0i32 {
        warn!("Failed to load AGL file \"{}\"...", "glyphlist.txt");
    };
}
#[no_mangle]
pub unsafe extern "C" fn agl_close_map() {
    ht_clear_table(&mut aglmap);
}
/*
 * References:
 *
 *  Unicode and Glyph Names, ver. 2.3., Adobe Solution Network
 *  http://partners.adobe.com/asn/tech/type/unicodegn.jsp
 */
/* Hash */
unsafe fn agl_load_listfile(mut filename: *const i8, mut is_predef: i32) -> i32 {
    let mut count: i32 = 0i32;
    let mut wbuf: [i8; 1024] = [0; 1024];
    if filename.is_null() {
        return -1i32;
    }
    let handle = dpx_tt_open(
        filename,
        b".txt\x00" as *const u8 as *const i8,
        TTInputFormat::FONTMAP,
    );
    if handle.is_none() {
        return -1i32;
    }
    let mut handle = handle.unwrap();
    if verbose != 0 {
        info!("<AGL:{}", CStr::from_ptr(filename).display());
    }
    loop {
        let mut p = tt_mfgets(wbuf.as_mut_ptr(), 1024i32, &mut handle) as *const i8;
        if p.is_null() {
            break;
        }
        let mut unicodes: [i32; 16] = [0; 16];
        let endptr = p.offset(strlen(p) as isize);
        skip_white(&mut p, endptr);
        /* Need table version check. */
        if p.is_null() || *p.offset(0) as i32 == '#' as i32 || p >= endptr {
            continue;
        }
        let mut nextptr = strchr(p, ';' as i32) as *mut i8;
        if nextptr.is_null() || nextptr == p as *mut i8 {
            continue;
        }
        let name = parse_ident(&mut p, nextptr);
        skip_white(&mut p, endptr);
        if name.is_null() || *p.offset(0) as i32 != ';' as i32 {
            warn!(
                "Invalid AGL entry: {}",
                CStr::from_ptr(wbuf.as_ptr()).display()
            );
            free(name as *mut libc::c_void);
        } else {
            p = p.offset(1);
            skip_white(&mut p, endptr);
            let mut n_unicodes = 0i32;
            while p < endptr
                && (*p.offset(0) as i32 >= '0' as i32 && *p.offset(0) as i32 <= '9' as i32
                    || *p.offset(0) as i32 >= 'A' as i32 && *p.offset(0) as i32 <= 'F' as i32)
            {
                if n_unicodes >= 16i32 {
                    warn!("Too many Unicode values");
                    break;
                } else {
                    let fresh0 = n_unicodes;
                    n_unicodes += 1;
                    unicodes[fresh0 as usize] = strtol(p, &mut nextptr, 16i32) as i32;
                    p = nextptr;
                    skip_white(&mut p, endptr);
                }
            }
            if n_unicodes == 0i32 {
                warn!(
                    "AGL entry ignored (no mapping): {}",
                    CStr::from_ptr(wbuf.as_ptr()).display(),
                );
                free(name as *mut libc::c_void);
            } else {
                let agln = agl_normalized_name(CStr::from_ptr(name).to_bytes());
                (*agln).is_predef = is_predef;
                (*agln).n_components = n_unicodes;
                for i in 0..n_unicodes as usize {
                    (*agln).unicodes[i] = unicodes[i];
                }
                let mut duplicate = ht_lookup_table(
                    &mut aglmap,
                    name as *const libc::c_void,
                    strlen(name) as i32,
                ) as *mut agl_name;
                if duplicate.is_null() {
                    ht_append_table(
                        &mut aglmap,
                        name as *const libc::c_void,
                        strlen(name) as i32,
                        agln as *mut libc::c_void,
                    );
                } else {
                    while !(*duplicate).alternate.is_null() {
                        duplicate = (*duplicate).alternate
                    }
                    (*duplicate).alternate = agln
                }
                if verbose > 3i32 {
                    if !(*agln).suffix.is_null() {
                        info!(
                            "agl: {} [{}.{}] -->",
                            CStr::from_ptr(name).display(),
                            CStr::from_ptr((*agln).name).display(),
                            CStr::from_ptr((*agln).suffix).display(),
                        );
                    } else {
                        info!(
                            "agl: {} [{}] -->",
                            CStr::from_ptr(name).display(),
                            CStr::from_ptr((*agln).name).display(),
                        );
                    }
                    for i in 0..(*agln).n_components as usize {
                        if (*agln).unicodes[i] > 0xffffi32 {
                            info!(" U+{:06X}", (*agln).unicodes[i],);
                        } else {
                            info!(" U+{:04X}", (*agln).unicodes[i],);
                        }
                    }
                    info!("\n");
                }
                free(name as *mut libc::c_void);
                count += 1
            }
        }
    }
    ttstub_input_close(handle);
    if verbose != 0 {
        info!(">");
    }
    count
}
#[no_mangle]
pub unsafe extern "C" fn agl_lookup_list(mut glyphname: *const i8) -> *mut agl_name {
    if glyphname.is_null() {
        return 0 as *mut agl_name;
    }
    ht_lookup_table(
        &mut aglmap,
        glyphname as *const libc::c_void,
        strlen(glyphname) as i32,
    ) as *mut agl_name
}
pub fn agl_name_is_unicode(glyphname: &[u8]) -> bool {
    if glyphname.is_empty() {
        return false;
    }
    let len = glyphname
        .iter()
        .position(|&x| x == b'.')
        .unwrap_or(glyphname.len());
    /*
     * uni02ac is invalid glyph name and mapped to th empty string.
     */
    if len >= 7 && (len - 3) % 4 == 0 && glyphname.starts_with(b"uni") {
        let c = glyphname[3];
        /*
         * Check if the 4th character is uppercase hexadecimal digit.
         * "union" should not be treated as Unicode glyph name.
         */
        if c.is_ascii_digit() || c >= b'A' && c <= b'F' {
            return true;
        } else {
            return false;
        }
    } else {
        if len <= 7 && len >= 5 && glyphname[0] == b'u' {
            for c in &glyphname[1..(len - 1)] {
                if !c.is_ascii_digit() && (*c < b'A' || *c > b'F') {
                    return false;
                }
            }
            return true;
        }
    }
    false
}
#[no_mangle]
pub unsafe extern "C" fn agl_name_convert_unicode(mut glyphname: *const i8) -> i32 {
    if !agl_name_is_unicode(CStr::from_ptr(glyphname).to_bytes()) {
        return -1i32;
    }
    if strlen(glyphname) > 7 && *glyphname.offset(7) as i32 != '.' as i32 {
        warn!("Mapping to multiple Unicode characters not supported.");
        return -1i32;
    }
    let mut p = if *glyphname.offset(1) as i32 == 'n' as i32 {
        glyphname.offset(3)
    } else {
        glyphname.offset(1)
    };
    let mut ucv = 0;
    while *p as i32 != '\u{0}' as i32 && *p as i32 != '.' as i32 {
        if libc::isdigit(*p as _) == 0 && ((*p as i32) < 'A' as i32 || *p as i32 > 'F' as i32) {
            warn!(
                "Invalid char {} in Unicode glyph name {}.",
                char::from(*p as u8),
                CStr::from_ptr(glyphname).display(),
            );
            return -1i32;
        }
        ucv <<= 4i32;
        ucv += if libc::isdigit(*p as _) != 0 {
            *p as i32 - '0' as i32
        } else {
            *p as i32 - 'A' as i32 + 10i32
        };
        p = p.offset(1)
    }
    if !UC_is_valid(ucv) {
        if ucv < 0x10000i32 {
            warn!("Invalid Unicode code value U+{:04X}.", ucv,);
        } else {
            warn!("Invalid Unicode code value U+{:06X}.", ucv,);
        }
        ucv = -1i32
    }
    ucv
}

fn xtol(mut buf: &[u8]) -> i32 {
    let mut v: i32 = 0i32;
    for &b in buf {
        v <<= 4;
        if b.is_ascii_digit() {
            v += (b - b'0') as i32;
        } else if b >= b'A' && b <= b'F' {
            v += (b - b'A' + 10) as i32;
        } else {
            return -1;
        }
    }
    v
}

unsafe fn put_unicode_glyph(name: &[u8], mut dstpp: *mut *mut u8, mut limptr: *mut u8) -> i32 {
    let mut len = 0;
    let mut p = name;
    if p[1] != b'n' {
        p = &p[1..];
        let ucv = xtol(p);
        len = ((len as u64) + UC_UTF16BE_encode_char(ucv, dstpp, limptr)) as i32;
    } else {
        p = &p[3..];
        while !p.is_empty() {
            let ucv = xtol(&p[..4]);
            len = ((len as u64) + UC_UTF16BE_encode_char(ucv, dstpp, limptr)) as i32;
            p = &p[4..];
        }
    }
    len
}
#[no_mangle]
pub unsafe extern "C" fn agl_sput_UTF16BE(
    mut glyphstr: *const i8,
    mut dstpp: *mut *mut u8,
    mut limptr: *mut u8,
    mut fail_count: *mut i32,
) -> i32 {
    let mut len: i32 = 0i32;
    let mut count: i32 = 0i32;
    assert!(!glyphstr.is_null() && !dstpp.is_null());
    let mut p = glyphstr;
    let mut endptr = strchr(p, '.' as i32) as *const i8;
    if endptr.is_null() {
        endptr = p.offset(strlen(p) as isize)
    }
    while p < endptr {
        let mut delim = strchr(p, '_' as i32) as *const i8;
        if delim == p {
            /*
             * Glyph names starting with a underscore or two subsequent
             * underscore in glyph name not allowed?
             */
            warn!(
                "Invalid glyph name component in \"{}\".",
                CStr::from_ptr(glyphstr).display()
            );
            count += 1;
            if !fail_count.is_null() {
                *fail_count = count
            }
            return len;
        /* Cannot continue */
        } else {
            if delim.is_null() || delim > endptr {
                delim = endptr
            }
        }
        let sub_len = delim.wrapping_offset_from(p) as i64 as i32;
        let name_p = new(((sub_len + 1i32) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
        memcpy(
            name_p as *mut libc::c_void,
            p as *const libc::c_void,
            sub_len as _,
        );
        *name_p.offset(sub_len as isize) = '\u{0}' as i32 as i8;
        let name = CStr::from_ptr(name_p).to_owned();
        free(name_p as *mut libc::c_void);
        if agl_name_is_unicode(name.to_bytes()) {
            let sub_len = put_unicode_glyph(name.to_bytes(), dstpp, limptr);
            if sub_len > 0i32 {
                len += sub_len
            } else {
                count += 1
            }
        } else {
            let mut agln1 = agl_lookup_list(name.as_ptr());
            if agln1.is_null()
                || (*agln1).n_components == 1i32
                    && ((*agln1).unicodes[0] as i64 >= 0xe000
                        && (*agln1).unicodes[0] as i64 <= 0xf8ff
                        || (*agln1).unicodes[0] as i64 >= 0xf0000
                            && (*agln1).unicodes[0] as i64 <= 0xffffd
                        || (*agln1).unicodes[0] as i64 >= 0x100000
                            && (*agln1).unicodes[0] as i64 <= 0x10fffd)
            {
                let agln0 = agl_normalized_name(name.to_bytes());
                if !agln0.is_null() {
                    if verbose > 1i32 && !(*agln0).suffix.is_null() {
                        warn!(
                            "agl: fix {} --> {}.{}",
                            name.display(),
                            CStr::from_ptr((*agln0).name).display(),
                            CStr::from_ptr((*agln0).suffix).display(),
                        );
                    }
                    agln1 = agl_lookup_list((*agln0).name);
                    agl_release_name(agln0);
                }
            }
            if !agln1.is_null() {
                for i in 0..(*agln1).n_components as usize {
                    len = (len as u64).wrapping_add(UC_UTF16BE_encode_char(
                        (*agln1).unicodes[i],
                        dstpp,
                        limptr,
                    )) as i32 as i32;
                }
            } else {
                if verbose != 0 {
                    warn!(
                        "No Unicode mapping for glyph name \"{}\" found.",
                        name.display()
                    )
                }
                count += 1
            }
        }
        p = delim.offset(1)
    }
    if !fail_count.is_null() {
        *fail_count = count
    }
    len
}
#[no_mangle]
pub unsafe extern "C" fn agl_get_unicodes(
    mut glyphstr: *const i8,
    mut unicodes: *mut i32,
    mut max_unicodes: i32,
) -> i32 {
    let mut count: i32 = 0i32;
    let mut p = glyphstr;
    let mut endptr = strchr(p, '.' as i32) as *const i8;
    if endptr.is_null() {
        endptr = p.offset(strlen(p) as isize)
    }
    while p < endptr {
        let mut delim = strchr(p, '_' as i32) as *const i8;
        if delim == p {
            /*
             * Glyph names starting with a underscore or two subsequent
             * underscore in glyph name not allowed?
             */
            warn!(
                "Invalid glyph name component in \"{}\".",
                CStr::from_ptr(glyphstr).display()
            );
            return -1i32;
        /* Cannot continue */
        } else {
            if delim.is_null() || delim > endptr {
                delim = endptr
            }
        }
        let sub_len = delim.wrapping_offset_from(p) as i32;
        let name_p = new(((sub_len + 1i32) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
        memcpy(
            name_p as *mut libc::c_void,
            p as *const libc::c_void,
            sub_len as _,
        );
        *name_p.offset(sub_len as isize) = '\u{0}' as i32 as i8;
        let name = CStr::from_ptr(name_p).to_owned();
        free(name_p as *mut libc::c_void);
        if agl_name_is_unicode(name.to_bytes()) {
            let mut p = name.to_bytes();
            if p[1] != b'n' {
                /* uXXXXXXXX */
                if count >= max_unicodes {
                    return -1i32;
                }
                p = &p[1..];
                *unicodes.offset(count as isize) = xtol(p);
                count += 1;
            } else {
                p = &p[3..];
                while !p.is_empty() {
                    if count >= max_unicodes {
                        return -1i32;
                    }
                    *unicodes.offset(count as isize) = xtol(&p[..4]);
                    count += 1;
                    p = &p[4..];
                }
            }
        } else {
            let mut agln1 = agl_lookup_list(name.as_ptr());
            if agln1.is_null()
                || (*agln1).n_components == 1i32
                    && ((*agln1).unicodes[0] as i64 >= 0xe000
                        && (*agln1).unicodes[0] as i64 <= 0xf8ff
                        || (*agln1).unicodes[0] as i64 >= 0xf0000
                            && (*agln1).unicodes[0] as i64 <= 0xffffd
                        || (*agln1).unicodes[0] as i64 >= 0x100000
                            && (*agln1).unicodes[0] as i64 <= 0x10fffd)
            {
                let mut agln0 = agl_normalized_name(name.to_bytes());
                if !agln0.is_null() {
                    if verbose > 1i32 && !(*agln0).suffix.is_null() {
                        warn!(
                            "agl: fix {} --> {}.{}",
                            name.display(),
                            CStr::from_ptr((*agln0).name).display(),
                            CStr::from_ptr((*agln0).suffix).display(),
                        );
                    }
                    agln1 = agl_lookup_list((*agln0).name);
                    agl_release_name(agln0);
                }
            }
            if !agln1.is_null() {
                if count + (*agln1).n_components > max_unicodes {
                    return -1i32;
                }
                for i in 0..(*agln1).n_components {
                    let fresh4 = count;
                    count = count + 1;
                    *unicodes.offset(fresh4 as isize) = (*agln1).unicodes[i as usize];
                }
            } else {
                if verbose > 1i32 {
                    warn!(
                        "No Unicode mapping for glyph name \"{}\" found.",
                        name.display()
                    )
                }
                return -1i32;
            }
        }
        p = delim.offset(1)
    }
    count
}
