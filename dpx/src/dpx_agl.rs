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
use crate::{info, warn};
use std::ffi::{CStr, CString};
use std::ptr;

use super::dpx_dpxfile::dpx_tt_open;
use super::dpx_dpxutil::{ht_append_table, ht_clear_table, ht_init_table, ht_lookup_table};
use super::dpx_mem::new;
use super::dpx_mfileio::tt_mfgets;
use super::dpx_pdfparse::{parse_ident, skip_white};
use super::dpx_unicode::{UC_UTF16BE_encode_char, UC_is_valid};
use libc::{free, memcpy, strchr, strlen, strtol};

use crate::bridge::TTInputFormat;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct agl_name {
    pub(crate) name: *mut i8,
    pub(crate) suffix: *mut i8,
    pub(crate) n_components: i32,
    pub(crate) unicodes: [i32; 16],
    pub(crate) alternate: *mut agl_name,
    pub(crate) is_predef: i32,
}
use super::dpx_dpxutil::ht_table;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) key: &'static [u8],
    pub(crate) otl_tag: &'static [u8],
    pub(crate) suffixes: &'static [&'static [u8]],
}

static mut verbose: i32 = 0;

pub(crate) unsafe fn agl_set_verbose(level: i32) {
    verbose = level;
}
unsafe fn agl_new_name() -> *mut agl_name {
    let agln =
        new((1_u64).wrapping_mul(::std::mem::size_of::<agl_name>() as u64) as u32) as *mut agl_name;
    (*agln).name = ptr::null_mut();
    (*agln).suffix = ptr::null_mut();
    (*agln).n_components = 0;
    (*agln).alternate = ptr::null_mut();
    (*agln).is_predef = 0;
    agln
}
unsafe fn agl_release_name(mut agln: *mut agl_name) {
    while !agln.is_null() {
        let next = (*agln).alternate;
        let _ = CString::from_raw((*agln).name);
        if !(*agln).suffix.is_null() {
            let _ = CString::from_raw((*agln).suffix);
        }
        (*agln).name = ptr::null_mut();
        free(agln as *mut libc::c_void);
        agln = next
    }
}
pub(crate) unsafe fn agl_chop_suffix(glyphname: &[u8]) -> (Option<CString>, Option<CString>) {
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
    } else if slen > 0 {
        /* ??? */
        return false;
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

static mut VAR_LIST: [C2RustUnnamed_0; 13] = [
    C2RustUnnamed_0 {
        key: b"small",
        otl_tag: b"smcp",
        suffixes: &[b"sc"],
    },
    C2RustUnnamed_0 {
        key: b"swash",
        otl_tag: b"swsh",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"superior",
        otl_tag: b"sups",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"inferior",
        otl_tag: b"sinf",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"numerator",
        otl_tag: b"numr",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"denominator",
        otl_tag: b"dnom",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"oldstyle",
        otl_tag: b"onum",
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"display",
        otl_tag: &[],
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"text",
        otl_tag: &[],
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"big",
        otl_tag: &[],
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"bigg",
        otl_tag: &[],
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"Big",
        otl_tag: &[],
        suffixes: &[],
    },
    C2RustUnnamed_0 {
        key: b"Bigg",
        otl_tag: &[],
        suffixes: &[],
    },
];

pub(crate) unsafe fn agl_suffix_to_otltag(suffix: &[u8]) -> Option<&'static [u8]> {
    for vl in &VAR_LIST {
        for &vlsuffix in vl.suffixes {
            if suffix == vlsuffix {
                return Some(vl.otl_tag);
            }
        }
        if suffix == vl.key {
            return Some(vl.otl_tag);
        }
        if !vl.otl_tag.is_empty() && suffix == vl.otl_tag {
            return Some(vl.otl_tag);
        }
    }
    None
}
unsafe fn agl_guess_name(glyphname: &[u8]) -> Option<usize> {
    if is_smallcap(glyphname) {
        return Some(0);
    }
    let len = glyphname.len();
    for i in 1..VAR_LIST.len() {
        if len > VAR_LIST[i].key.len() && glyphname.ends_with(VAR_LIST[i].key) {
            return Some(i);
        }
    }
    None
}
unsafe fn agl_normalized_name(glyphname: &[u8]) -> *mut agl_name {
    if glyphname.is_empty() {
        return ptr::null_mut();
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
            n = glyphname.len() - VAR_LIST[var_idx].key.len();
            if !VAR_LIST[var_idx].suffixes.is_empty() {
                (*agln).suffix = CString::new(VAR_LIST[var_idx].suffixes[0])
                    .unwrap()
                    .into_raw();
            } else {
                (*agln).suffix = CString::new(VAR_LIST[var_idx].key).unwrap().into_raw();
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
    table: [std::ptr::null_mut(); 503],
};
#[inline]
unsafe fn hval_free(hval: *mut libc::c_void) {
    agl_release_name(hval as *mut agl_name);
}

pub(crate) unsafe fn agl_init_map() {
    ht_init_table(
        &mut aglmap,
        Some(hval_free as unsafe fn(_: *mut libc::c_void) -> ()),
    );
    agl_load_listfile("texglyphlist.txt", 0).ok();
    if agl_load_listfile("pdfglyphlist.txt", 1).is_err() {
        warn!("Failed to load AGL file \"{}\"...", "pdfglyphlist.txt");
    }
    if agl_load_listfile("glyphlist.txt", 0).is_err() {
        warn!("Failed to load AGL file \"{}\"...", "glyphlist.txt");
    };
}

pub(crate) unsafe fn agl_close_map() {
    ht_clear_table(&mut aglmap);
}
/*
 * References:
 *
 *  Unicode and Glyph Names, ver. 2.3., Adobe Solution Network
 *  http://partners.adobe.com/asn/tech/type/unicodegn.jsp
 */
/* Hash */
unsafe fn agl_load_listfile(filename: &str, is_predef: i32) -> Result<u32, ()> {
    let mut count: u32 = 0;
    let mut wbuf: [i8; 1024] = [0; 1024];
    if filename.is_empty() {
        return Err(());
    }
    let handle = dpx_tt_open(filename, ".txt", TTInputFormat::FONTMAP);
    if handle.is_none() {
        return Err(());
    }
    let mut handle = handle.unwrap();
    if verbose != 0 {
        info!("<AGL:{}", filename);
    }
    loop {
        let mut p = tt_mfgets(wbuf.as_mut_ptr(), 1024, &mut handle) as *const i8;
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
            let mut n_unicodes = 0;
            while p < endptr
                && (*p.offset(0) as i32 >= '0' as i32 && *p.offset(0) as i32 <= '9' as i32
                    || *p.offset(0) as i32 >= 'A' as i32 && *p.offset(0) as i32 <= 'F' as i32)
            {
                if n_unicodes >= 16 {
                    warn!("Too many Unicode values");
                    break;
                } else {
                    unicodes[n_unicodes as usize] = strtol(p, &mut nextptr, 16) as i32;
                    n_unicodes += 1;
                    p = nextptr;
                    skip_white(&mut p, endptr);
                }
            }
            if n_unicodes == 0 {
                warn!(
                    "AGL entry ignored (no mapping): {}",
                    CStr::from_ptr(wbuf.as_ptr()).display(),
                );
                free(name as *mut libc::c_void);
            } else {
                let bname = CStr::from_ptr(name).to_bytes();
                let agln = agl_normalized_name(bname);
                (*agln).is_predef = is_predef;
                (*agln).n_components = n_unicodes;
                for i in 0..n_unicodes as usize {
                    (*agln).unicodes[i] = unicodes[i];
                }
                let mut duplicate = ht_lookup_table(&mut aglmap, bname) as *mut agl_name;
                if duplicate.is_null() {
                    ht_append_table(&mut aglmap, bname, agln as *mut libc::c_void);
                } else {
                    while !(*duplicate).alternate.is_null() {
                        duplicate = (*duplicate).alternate
                    }
                    (*duplicate).alternate = agln
                }
                if verbose > 3 {
                    if !(*agln).suffix.is_null() {
                        info!(
                            "agl: {} [{}.{}] -->",
                            bname.display(),
                            CStr::from_ptr((*agln).name).display(),
                            CStr::from_ptr((*agln).suffix).display(),
                        );
                    } else {
                        info!(
                            "agl: {} [{}] -->",
                            bname.display(),
                            CStr::from_ptr((*agln).name).display(),
                        );
                    }
                    for i in 0..(*agln).n_components as usize {
                        if (*agln).unicodes[i] > 0xffff {
                            info!(" U+{:06X}", (*agln).unicodes[i]);
                        } else {
                            info!(" U+{:04X}", (*agln).unicodes[i]);
                        }
                    }
                    info!("\n");
                }
                free(name as *mut libc::c_void);
                count += 1
            }
        }
    }
    if verbose != 0 {
        info!(">");
    }
    Ok(count)
}

pub(crate) unsafe fn agl_lookup_list(glyphname: &[u8]) -> *mut agl_name {
    if glyphname.is_empty() {
        return ptr::null_mut();
    }
    ht_lookup_table(&mut aglmap, glyphname) as *mut agl_name
}
pub(crate) fn agl_name_is_unicode(glyphname: &[u8]) -> bool {
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
        return c.is_ascii_digit() || (b'A'..=b'F').contains(&c);
    } else if len <= 7 && len >= 5 && glyphname[0] == b'u' {
        for c in &glyphname[1..(len - 1)] {
            if !c.is_ascii_digit() && !(b'A'..=b'F').contains(c) {
                return false;
            }
        }
        return true;
    }
    false
}

pub(crate) unsafe fn agl_name_convert_unicode(glyphname: *const i8) -> i32 {
    if !agl_name_is_unicode(CStr::from_ptr(glyphname).to_bytes()) {
        return -1;
    }
    if strlen(glyphname) > 7 && *glyphname.offset(7) as i32 != '.' as i32 {
        warn!("Mapping to multiple Unicode characters not supported.");
        return -1;
    }
    let mut p = if *glyphname.offset(1) as i32 == 'n' as i32 {
        glyphname.offset(3)
    } else {
        glyphname.offset(1)
    };
    let mut ucv = 0;
    while *p as i32 != '\u{0}' as i32 && *p as i32 != '.' as i32 {
        if !(*p as u8).is_ascii_digit() && ((*p as i32) < 'A' as i32 || *p as i32 > 'F' as i32) {
            warn!(
                "Invalid char {} in Unicode glyph name {}.",
                char::from(*p as u8),
                CStr::from_ptr(glyphname).display(),
            );
            return -1;
        }
        ucv <<= 4;
        ucv += if (*p as u8).is_ascii_digit() {
            *p as i32 - '0' as i32
        } else {
            *p as i32 - 'A' as i32 + 10
        };
        p = p.offset(1)
    }
    if !UC_is_valid(ucv) {
        if ucv < 0x10000 {
            warn!("Invalid Unicode code value U+{:04X}.", ucv);
        } else {
            warn!("Invalid Unicode code value U+{:06X}.", ucv);
        }
        ucv = -1
    }
    ucv
}

fn xtol(buf: &[u8]) -> i32 {
    let mut v: i32 = 0;
    for b in buf {
        v <<= 4;
        if b.is_ascii_digit() {
            v += (*b - b'0') as i32;
        } else if (b'A'..=b'F').contains(b) {
            v += (*b - b'A' + 10) as i32;
        } else {
            return -1;
        }
    }
    v
}

unsafe fn put_unicode_glyph(name: &[u8], dstpp: &mut *mut u8, limptr: *mut u8) -> i32 {
    let mut len = 0;
    let mut p = name;
    if p[1] != b'n' {
        p = &p[1..];
        let ucv = xtol(p);
        len = ((len as u64) + UC_UTF16BE_encode_char(ucv, dstpp, limptr) as u64) as i32;
    } else {
        p = &p[3..];
        while !p.is_empty() {
            let ucv = xtol(&p[..4]);
            len = ((len as u64) + UC_UTF16BE_encode_char(ucv, dstpp, limptr) as u64) as i32;
            p = &p[4..];
        }
    }
    len
}

pub(crate) unsafe fn agl_sput_UTF16BE(
    glyphstr: &str,
    dstpp: &mut *mut u8,
    limptr: *mut u8,
    fail_count: *mut i32,
) -> i32 {
    let glyphstr = CString::new(glyphstr.as_bytes()).unwrap();
    let mut len: i32 = 0;
    let mut count: i32 = 0;
    assert!(!dstpp.is_null());
    let mut p = glyphstr.as_ptr();
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
                glyphstr.display()
            );
            count += 1;
            if !fail_count.is_null() {
                *fail_count = count
            }
            return len;
        /* Cannot continue */
        } else if delim.is_null() || delim > endptr {
            delim = endptr;
        }
        let sub_len = delim.offset_from(p) as i64 as i32;
        let name = CString::new(std::slice::from_raw_parts(p as *const u8, sub_len as _)).unwrap();
        if agl_name_is_unicode(name.to_bytes()) {
            let sub_len = put_unicode_glyph(name.to_bytes(), dstpp, limptr);
            if sub_len > 0 {
                len += sub_len
            } else {
                count += 1
            }
        } else {
            let mut agln1 = agl_lookup_list(name.to_bytes());
            if agln1.is_null()
                || (*agln1).n_components == 1
                    && ((*agln1).unicodes[0] as i64 >= 0xe000
                        && (*agln1).unicodes[0] as i64 <= 0xf8ff
                        || (*agln1).unicodes[0] as i64 >= 0xf0000
                            && (*agln1).unicodes[0] as i64 <= 0xffffd
                        || (*agln1).unicodes[0] as i64 >= 0x100000
                            && (*agln1).unicodes[0] as i64 <= 0x10fffd)
            {
                let agln0 = agl_normalized_name(name.to_bytes());
                if !agln0.is_null() {
                    if verbose > 1 && !(*agln0).suffix.is_null() {
                        warn!(
                            "agl: fix {} --> {}.{}",
                            name.display(),
                            CStr::from_ptr((*agln0).name).display(),
                            CStr::from_ptr((*agln0).suffix).display(),
                        );
                    }
                    agln1 = agl_lookup_list(CStr::from_ptr((*agln0).name).to_bytes());
                    agl_release_name(agln0);
                }
            }
            if !agln1.is_null() {
                for i in 0..(*agln1).n_components as usize {
                    len = (len as u64).wrapping_add(UC_UTF16BE_encode_char(
                        (*agln1).unicodes[i],
                        dstpp,
                        limptr,
                    ) as u64) as i32 as i32;
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

pub(crate) unsafe fn agl_get_unicodes(
    glyphstr: &str,
    unicodes: *mut i32,
    max_unicodes: i32,
) -> i32 {
    let glyphstr = CString::new(glyphstr).unwrap();
    let mut count: i32 = 0;
    let mut p = glyphstr.as_ptr();
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
                glyphstr.display()
            );
            return -1;
        /* Cannot continue */
        } else if delim.is_null() || delim > endptr {
            delim = endptr;
        }
        let sub_len = delim.offset_from(p) as i32;
        let name_p = new(
            ((sub_len + 1) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32
        ) as *mut i8;
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
                    return -1;
                }
                p = &p[1..];
                *unicodes.offset(count as isize) = xtol(p);
                count += 1;
            } else {
                p = &p[3..];
                while !p.is_empty() {
                    if count >= max_unicodes {
                        return -1;
                    }
                    *unicodes.offset(count as isize) = xtol(&p[..4]);
                    count += 1;
                    p = &p[4..];
                }
            }
        } else {
            let mut agln1 = agl_lookup_list(name.to_bytes());
            if agln1.is_null()
                || (*agln1).n_components == 1
                    && ((*agln1).unicodes[0] as i64 >= 0xe000
                        && (*agln1).unicodes[0] as i64 <= 0xf8ff
                        || (*agln1).unicodes[0] as i64 >= 0xf0000
                            && (*agln1).unicodes[0] as i64 <= 0xffffd
                        || (*agln1).unicodes[0] as i64 >= 0x100000
                            && (*agln1).unicodes[0] as i64 <= 0x10fffd)
            {
                let agln0 = agl_normalized_name(name.to_bytes());
                if !agln0.is_null() {
                    if verbose > 1 && !(*agln0).suffix.is_null() {
                        warn!(
                            "agl: fix {} --> {}.{}",
                            name.display(),
                            CStr::from_ptr((*agln0).name).display(),
                            CStr::from_ptr((*agln0).suffix).display(),
                        );
                    }
                    agln1 = agl_lookup_list(CStr::from_ptr((*agln0).name).to_bytes());
                    agl_release_name(agln0);
                }
            }
            if !agln1.is_null() {
                if count + (*agln1).n_components > max_unicodes {
                    return -1;
                }
                for i in 0..(*agln1).n_components {
                    *unicodes.offset(count as isize) = (*agln1).unicodes[i as usize];
                    count += 1;
                }
            } else {
                if verbose > 1 {
                    warn!(
                        "No Unicode mapping for glyph name \"{}\" found.",
                        name.display()
                    )
                }
                return -1;
            }
        }
        p = delim.offset(1)
    }
    count
}
