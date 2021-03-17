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
use crate::warn;
use std::ptr;
use std::rc::Rc;

use super::dpx_cff::cff_set_name;
use super::dpx_cff::{cff_add_string, cff_get_sid, cff_update_string, CffIndex};
use super::dpx_mem::{new, renew};
use super::dpx_pst::{pst_get_token, PstObj};
use crate::bridge::InFile;
use libc::{free, memcpy, memmove, memset};

use std::io::{Read, Seek, SeekFrom};

/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
pub(crate) type c_offsize = u8;
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
pub(crate) type l_offset = u32;
/* 1, 2, 3, or 4-byte offset */
pub(crate) type s_SID = u16;
/* 2-byte string identifier  */

use super::dpx_cff_dict::cff_dict;
/* Encoding, Charset and FDSelect */
use super::dpx_cff::Charsets;

use super::dpx_cff::cff_font;

unsafe fn t1_decrypt(mut key: u16, mut dst: *mut u8, mut src: *const u8, skip: i32, mut len: i32) {
    len -= skip;
    for _ in 0..skip {
        key = ((key as i32 + *src as i32) as u32)
            .wrapping_mul(52845u32)
            .wrapping_add(22719u32) as u16;
        src = src.offset(1);
    }
    for _ in 0..len {
        let c: u8 = *src;
        src = src.offset(1);
        *dst = (c as i32 ^ key as i32 >> 8) as u8;
        dst = dst.offset(1);
        key = ((key as i32 + c as i32) as u32)
            .wrapping_mul(52845u32)
            .wrapping_add(22719u32) as u16
    }
}
/* T1CRYPT */
unsafe fn get_next_key(start: &mut *const u8, end: *const u8) -> Option<String> {
    let mut key = None;
    while *start < end {
        match pst_get_token(start, end) {
            None => break,
            Some(PstObj::Name(data)) => {
                key = Some(data.into_string().unwrap());
                break;
            }
            _ => {}
        }
    }
    key
}
unsafe fn seek_operator(start: &mut *const u8, end: *const u8, op: &[u8]) -> i32 {
    while *start < end {
        match pst_get_token(start, end) {
            None => break,
            Some(PstObj::Unknown(data)) if data.starts_with(op) => {
                return 0;
            }
            _ => {}
        }
    }
    -1
}
unsafe fn parse_svalue(start: &mut *const u8, end: *const u8) -> Result<String, ()> {
    match pst_get_token(start, end).ok_or(())? {
        PstObj::Name(data) => Ok(data.into_string().unwrap()),
        PstObj::String(data) => Ok(data),
        _ => Err(()),
    }
}
unsafe fn parse_bvalue(
    start: &mut *const u8,
    end: *const u8,
    value: *mut f64,
) -> Result<usize, ()> {
    if let PstObj::Boolean(data) = pst_get_token(start, end).ok_or(())? {
        *value = data as i32 as f64;
        Ok(1)
    } else {
        Err(())
    }
}
unsafe fn parse_nvalue(
    // TODO: check
    start: &mut *const u8,
    end: *const u8,
    value: *mut f64,
    max: i32,
) -> Result<usize, ()> {
    let mut argn = 0;
    /*
     * All array elements must be numeric token. (ATM compatible)
     */
    match pst_get_token(start, end).ok_or(())? {
        PstObj::Integer(data) if max > 0 => {
            *value.offset(0) = data as f64;
            argn = 1;
        }
        PstObj::Real(data) if max > 0 => {
            *value.offset(0) = data;
            argn = 1;
        }
        PstObj::Mark => {
            /* It does not distinguish '[' and '{'... */
            let mut tok = None;
            while *start < end {
                if let Some(tok1) = pst_get_token(start, end) {
                    match tok1 {
                        PstObj::Integer(data) if argn < max as usize => {
                            *value.offset(argn as isize) = data as f64;
                            argn += 1;
                            tok = None;
                        }
                        PstObj::Real(data) if argn < max as usize => {
                            *value.offset(argn as isize) = data;
                            argn += 1;
                            tok = None;
                        }
                        _ => {
                            tok = Some(tok1);
                            break;
                        }
                    }
                } else {
                    tok = None;
                    break;
                }
            }

            if !matches!(tok, Some(PstObj::Unknown(data)) if data.starts_with(b"]")
                    || data.starts_with(b"}"))
            {
                return Err(());
            }
        }
        _ => {}
    }
    Ok(argn)
}
static mut StandardEncoding: [&str; 256] = [
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quoteright",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "quoteleft",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "exclamdown",
    "cent",
    "sterling",
    "fraction",
    "yen",
    "florin",
    "section",
    "currency",
    "quotesingle",
    "quotedblleft",
    "guillemotleft",
    "guilsinglleft",
    "guilsinglright",
    "fi",
    "fl",
    ".notdef",
    "endash",
    "dagger",
    "daggerdbl",
    "periodcentered",
    ".notdef",
    "paragraph",
    "bullet",
    "quotesinglbase",
    "quotedblbase",
    "quotedblright",
    "guillemotright",
    "ellipsis",
    "perthousand",
    ".notdef",
    "questiondown",
    ".notdef",
    "grave",
    "acute",
    "circumflex",
    "tilde",
    "macron",
    "breve",
    "dotaccent",
    "dieresis",
    ".notdef",
    "ring",
    "cedilla",
    ".notdef",
    "hungarumlaut",
    "ogonek",
    "caron",
    "emdash",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "AE",
    ".notdef",
    "ordfeminine",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "Lslash",
    "Oslash",
    "OE",
    "ordmasculine",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "ae",
    ".notdef",
    ".notdef",
    ".notdef",
    "dotlessi",
    ".notdef",
    ".notdef",
    "lslash",
    "oslash",
    "oe",
    "germandbls",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
];
static mut ISOLatin1Encoding: [&str; 256] = [
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quotesingle",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "grave",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    ".notdef",
    "dotlessi",
    "quoteleft",
    "quoteright",
    "circumflex",
    "tilde",
    "macron",
    "breve",
    "dotaccent",
    "dieresis",
    ".notdef",
    "ring",
    "cedilla",
    ".notdef",
    "hungarumlaut",
    "ogonek",
    "caron",
    "space",
    "exclamdown",
    "cent",
    "sterling",
    "currency",
    "yen",
    "brokenbar",
    "section",
    "dieresis",
    "copyright",
    "ordfeminine",
    "guillemotleft",
    "logicalnot",
    "hyphen",
    "registered",
    "macron",
    "degree",
    "plusminus",
    "twosuperior",
    "threesuperior",
    "acute",
    "mu",
    "paragraph",
    "periodcentered",
    "cedilla",
    "onesuperior",
    "ordmasculine",
    "guillemotright",
    "onequarter",
    "onehalf",
    "threequarters",
    "questiondown",
    "Agrave",
    "Aacute",
    "Acircumflex",
    "Atilde",
    "Adieresis",
    "Aring",
    "AE",
    "Ccedilla",
    "Egrave",
    "Eacute",
    "Ecircumflex",
    "Edieresis",
    "Igrave",
    "Iacute",
    "Icircumflex",
    "Idieresis",
    "Eth",
    "Ntilde",
    "Ograve",
    "Oacute",
    "Ocircumflex",
    "Otilde",
    "Odieresis",
    "multiply",
    "Oslash",
    "Ugrave",
    "Uacute",
    "Ucircumflex",
    "Udieresis",
    "Yacute",
    "Thorn",
    "germandbls",
    "agrave",
    "aacute",
    "acircumflex",
    "atilde",
    "adieresis",
    "aring",
    "ae",
    "ccedilla",
    "egrave",
    "eacute",
    "ecircumflex",
    "edieresis",
    "igrave",
    "iacute",
    "icircumflex",
    "idieresis",
    "eth",
    "ntilde",
    "ograve",
    "oacute",
    "ocircumflex",
    "otilde",
    "odieresis",
    "divide",
    "oslash",
    "ugrave",
    "uacute",
    "ucircumflex",
    "udieresis",
    "yacute",
    "thorn",
    "ydieresis",
];
/* Treat cases such as "dup num num getinterval num exch putinterval"
 * or "dup num exch num get put"
 */
unsafe fn try_put_or_putinterval(
    enc_vec: &mut [String],
    start: &mut *const u8,
    end: *const u8,
) -> Result<(), ()> {
    let num1 = match pst_get_token(start, end) {
        Some(PstObj::Integer(num1)) if (num1 >= 0 && num1 < 256) => num1,
        _ => return Err(()),
    };
    match pst_get_token(start, end) {
        Some(PstObj::Unknown(data)) if data.starts_with(b"exch") => {
            /* dup num exch num get put */
            let num2 = match pst_get_token(start, end) {
                Some(PstObj::Integer(num2)) if (num2 >= 0 && num2 < 256) => num2,
                _ => return Err(()),
            };
            match pst_get_token(start, end) {
                Some(PstObj::Unknown(data)) if data.starts_with(b"get") => {}
                _ => return Err(()),
            }
            match pst_get_token(start, end) {
                Some(PstObj::Unknown(data)) if data.starts_with(b"put") => {}
                _ => return Err(()),
            }
            enc_vec[num1 as usize] = enc_vec[num2 as usize].clone();
            Ok(())
        }
        Some(PstObj::Integer(num2)) if (num2 >= 0 && num2 + num1 < 256) => {
            match pst_get_token(start, end) {
                Some(PstObj::Unknown(data)) if data.starts_with(b"getinterval") => {}
                _ => return Err(()),
            }
            let num3 = match pst_get_token(start, end) {
                Some(PstObj::Integer(num3)) if (num3 >= 0 && num3 + num2 < 256) => num3,
                _ => return Err(()),
            };
            match pst_get_token(start, end) {
                Some(PstObj::Unknown(data)) if data.starts_with(b"exch") => {}
                _ => return Err(()),
            }
            match pst_get_token(start, end) {
                Some(PstObj::Unknown(data)) if data.starts_with(b"putinterval") => {}
                _ => return Err(()),
            }
            for i in 0..num2 {
                if !(enc_vec[(num1 + i) as usize]).is_empty() {
                    /* num1 + i < 256 here */
                    enc_vec[(num3 + i) as usize] = enc_vec[(num1 + i) as usize].clone();
                }
            }
            Ok(())
        }
        _ => Err(()),
    }
}
unsafe fn parse_encoding(enc_vec: &mut [String], start: &mut *const u8, end: *const u8) -> i32 {
    /*
     *  StandardEncoding def
     * or
     *  ISOLatin1Encoding def
     * or
     *  0 1 255 {1 index exch /.notdef put } for
     *  dup int name put
     *  ...
     *  [readonly] def
     */
    match pst_get_token(start, end).unwrap() {
        PstObj::Unknown(data) if data.starts_with(b"StandardEncoding") => {
            if !enc_vec.is_empty() {
                for code in 0..256 {
                    enc_vec[code] = if !StandardEncoding[code].is_empty()
                        && StandardEncoding[code] != ".notdef"
                    {
                        StandardEncoding[code].to_string()
                    } else {
                        String::new()
                    };
                }
            }
        }
        PstObj::Unknown(data) if data.starts_with(b"ISOLatin1Encoding") => {
            if !enc_vec.is_empty() {
                for code in 0..256 {
                    enc_vec[code] = if !ISOLatin1Encoding[code].is_empty()
                        && ISOLatin1Encoding[code] != ".notdef"
                    {
                        ISOLatin1Encoding[code].to_string()
                    } else {
                        String::new()
                    };
                }
            }
        }
        PstObj::Unknown(data) if data.starts_with(b"ExpertEncoding") => {
            if !enc_vec.is_empty() {
                warn!("ExpertEncoding not supported.");
                return -1;
            }
        }
        // Not supported yet.
        _ => {
            seek_operator(start, end, b"array");
            /*
             * Pick all seaquences that matches "dup n /Name put" until
             * occurrence of "def" or "readonly".
             */
            while *start < end {
                match pst_get_token(start, end) {
                    None => break,
                    Some(tok) => match tok {
                        PstObj::Unknown(data)
                            if (data.starts_with(b"def") || data.starts_with(b"readonly")) =>
                        {
                            break;
                        }
                        PstObj::Unknown(data) if data.starts_with(b"dup") => {
                            /* cmctt10.pfb for examples contains the following PS code
                             *     dup num num getinterval num exch putinterval
                             *     dup num exch num get put
                             */
                            match pst_get_token(start, end).unwrap() {
                                PstObj::Unknown(data) if data.starts_with(b"dup") => {
                                    /* possibly putinterval type */
                                    if !enc_vec.is_empty() {
                                        try_put_or_putinterval(enc_vec, start, end).ok();
                                    } else {
                                        warn!("This kind of type1 fonts are not supported as native fonts.\n                   They are supported if used with tfm fonts.\n");
                                    }
                                }
                                PstObj::Integer(code) if (code >= 0 && code < 256) => {
                                    let code = code as usize;
                                    if let PstObj::Name(name) = pst_get_token(start, end).unwrap() {
                                        if !enc_vec.is_empty() {
                                            enc_vec[code] = String::new();
                                            if !name.to_bytes().is_empty() {
                                                enc_vec[code] = name.to_string_lossy().to_string();
                                            }
                                        }
                                        match pst_get_token(start, end).unwrap() {
                                            PstObj::Unknown(data) if data.starts_with(b"put") => {}
                                            _ => {
                                                if !enc_vec.is_empty() {
                                                    enc_vec[code] = String::new();
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                }
            }
        }
    }
    0
}
unsafe fn parse_subrs(
    font: &mut cff_font,
    start: &mut *const u8,
    end: *const u8,
    lenIV: i32,
    mode: i32,
) -> Result<(), ()> {
    let mut max_size;
    let offsets;
    let lengths;
    let count = match pst_get_token(start, end) {
        Some(PstObj::Integer(count)) if count >= 0 => count,
        _ => {
            warn!("Parsing Subrs failed.");
            return Err(());
        }
    };
    if count == 0 {
        font.subrs[0] = None;
        return Ok(());
    }
    match pst_get_token(start, end) {
        Some(PstObj::Unknown(data)) if data.starts_with(b"array") => {}
        _ => return Err(()),
    }
    let mut data;
    if mode != 1 {
        max_size = 65536;
        data = new((max_size as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
            as *mut u8;
        offsets =
            new((count as u32 as u64).wrapping_mul(::std::mem::size_of::<i32>() as u64) as u32)
                as *mut i32;
        lengths =
            new((count as u32 as u64).wrapping_mul(::std::mem::size_of::<i32>() as u64) as u32)
                as *mut i32;
        memset(
            offsets as *mut libc::c_void,
            0,
            (::std::mem::size_of::<i32>()).wrapping_mul(count as _),
        );
        memset(
            lengths as *mut libc::c_void,
            0,
            (::std::mem::size_of::<i32>()).wrapping_mul(count as _),
        );
    } else {
        max_size = 0;
        data = ptr::null_mut();
        offsets = ptr::null_mut();
        lengths = ptr::null_mut()
    }
    let mut offset = 0;
    /* dup subr# n-bytes RD n-binary-bytes NP */
    let mut i = 0;
    while i < count {
        match pst_get_token(start, end) {
            None => {
                free(data as *mut libc::c_void);
                free(offsets as *mut libc::c_void);
                free(lengths as *mut libc::c_void);
                return Err(());
            }
            Some(PstObj::Unknown(tok))
                if (tok.starts_with(b"ND")
                    || tok.starts_with(b"|-")
                    || tok.starts_with(b"def")) =>
            {
                break
            }
            Some(PstObj::Unknown(tok)) if tok.starts_with(b"dup") => {
                /* Found "dup" */
                let idx = match pst_get_token(start, end) {
                    Some(PstObj::Integer(idx)) if (idx >= 0 && idx < count) => idx,
                    _ => {
                        free(data as *mut libc::c_void);
                        free(offsets as *mut libc::c_void);
                        free(lengths as *mut libc::c_void);
                        return Err(());
                    }
                };
                let len = match pst_get_token(start, end) {
                    Some(PstObj::Integer(len)) if (len >= 0 && len <= 65536) => len,
                    _ => return Err(()),
                };
                pst_get_token(start, end)
                    .filter(|tok| {
                        matches!(tok, PstObj::Unknown(tok) if (tok.starts_with(b"RD")) || tok.starts_with(b"-|"))
                            || seek_operator(start, end, b"readstring")
                                >= 0
                    })
                    .ok_or_else(|| {
                        free(data as *mut libc::c_void);
                        free(offsets as *mut libc::c_void);
                        free(lengths as *mut libc::c_void);
                        ()
                    })?;
                *start = (*start).offset(1);
                if (*start).offset(len as isize) >= end {
                    free(data as *mut libc::c_void);
                    free(offsets as *mut libc::c_void);
                    free(lengths as *mut libc::c_void);
                    return Err(());
                }
                if mode != 1 {
                    if offset + len >= max_size {
                        max_size += 65536;
                        data = renew(
                            data as *mut libc::c_void,
                            (max_size as u32 as u64)
                                .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                                as u32,
                        ) as *mut u8
                    }
                    if lenIV >= 0 {
                        t1_decrypt(4330_u16, data.offset(offset as isize), *start, lenIV, len);
                        *offsets.offset(idx as isize) = offset;
                        let ref mut fresh16 = *lengths.offset(idx as isize);
                        *fresh16 = len - lenIV;
                        offset += *fresh16
                    } else if len > 0 {
                        *offsets.offset(idx as isize) = offset;
                        *lengths.offset(idx as isize) = len;
                        memcpy(
                            &mut *data.offset(offset as isize) as *mut u8 as *mut libc::c_void,
                            *start as *const libc::c_void,
                            len as _,
                        );
                        offset += len
                    }
                }
                *start = (*start).offset(len as isize);
                i += 1
            }
            _ => {}
        }
    }
    if mode != 1 {
        if font.subrs[0].is_none() {
            let mut subrs = CffIndex::new(count as u16);
            subrs.data = Vec::with_capacity(offset as usize);
            for i in 0..count as usize {
                subrs.offset[i] = (subrs.data.len() + 1) as l_offset;
                if *lengths.add(i) > 0 {
                    subrs.data.extend(std::slice::from_raw_parts(
                        data.offset(*offsets.add(i) as isize),
                        *lengths.add(i) as _,
                    ));
                }
            }
            subrs.offset[count as usize] = (subrs.data.len() + 1) as l_offset;
            font.subrs[0] = Some(subrs);
        } else {
            /* Adobe's OPO_____.PFB and OPBO____.PFB have two /Subrs dicts,
             * and also have /CharStrings not followed by dicts.
             * Simply ignores those data. By ChoF on 2009/04/08. */
            warn!("Already found /Subrs; ignores the other /Subrs dicts.");
        }
        free(data as *mut libc::c_void);
        free(offsets as *mut libc::c_void);
        free(lengths as *mut libc::c_void);
    }
    Ok(())
}
unsafe fn parse_charstrings(
    font: &mut cff_font,
    start: &mut *const u8,
    end: *const u8,
    lenIV: i32,
    mode: i32,
) -> Result<(), ()> {
    let mut max_size;
    /* /CharStrings n dict dup begin
     * /GlyphName n-bytes RD -n-binary-bytes- ND
     * ...
     * end
     *  - stack - ... /CharStrings dict
     */
    let tok = pst_get_token(start, end).unwrap(); /* .notdef must be at gid = 0 in CFF */
    match tok {
        PstObj::Integer(count) if (count >= 0 && count < 65000) => {
            font.cstrings = if mode != 1 {
                let mut charstrings = CffIndex::new(count as u16);
                max_size = 65536;
                charstrings.data = vec![0; max_size as _];
                Some(charstrings)
            } else {
                max_size = 0;
                None
            };
            let mut charset_glyphs = vec![0; count as usize - 1];
            let mut offset = 0;
            let mut have_notdef = 0;
            font.is_notdef_notzero = 0;
            seek_operator(start, end, b"begin");
            let mut i = 0;
            while i < count {
                /* BUG-20061126 (by ChoF):
                 * Some fonts (e.g., belleek/blsy.pfb) does not have the correct number
                 * of glyphs. Modify the codes even to work with these broken fonts.
                 */
                let tok = pst_get_token(start, end).ok_or(())?;
                let glyph_name = tok.clone().into_string();
                if i == 0 && !glyph_name.is_empty() && glyph_name != ".notdef" {
                    font.is_notdef_notzero = 1
                }
                match tok {
                    PstObj::Name(_) => {
                        if glyph_name.is_empty() {
                            return Err(());
                        }
                        let gid;
                        if glyph_name == ".notdef" {
                            have_notdef = 1;
                            gid = 0;
                        } else if have_notdef != 0 {
                            gid = i;
                        } else if i == count - 1 {
                            warn!("No .notdef glyph???");
                            return Err(());
                        } else {
                            gid = i + 1;
                        }
                        if gid > 0 {
                            charset_glyphs[(gid - 1) as usize] =
                                cff_add_string(font, &glyph_name, 0)
                        }
                        /*
                         * We don't care about duplicate strings here since
                         * later a subset font of this font will be generated.
                         */
                        let len = match pst_get_token(start, end) {
                            Some(PstObj::Integer(len)) if (len >= 0 && len <= 65536) => len,
                            _ => return Err(()),
                        };
                        pst_get_token(start, end)
                            .filter(|tok| {
                                matches!(tok, PstObj::Unknown(data) if
                                    (data.starts_with(b"RD") || data.starts_with(b"-|")))
                                    || seek_operator(start, end, b"readstring") >= 0
                            })
                            .ok_or(())?;
                        if (*start).offset(len as isize).offset(1) >= end {
                            return Err(());
                        }
                        let charstrings = font.cstrings.as_mut().unwrap();
                        if mode != 1 {
                            if offset + len >= max_size {
                                max_size += if len > 65536 { len } else { 65536 };
                                charstrings.data.resize(max_size as _, 0);
                            }
                            if gid == 0 {
                                if lenIV >= 0 {
                                    memmove(
                                        charstrings.data[(len - lenIV) as usize..].as_mut_ptr()
                                            as *mut libc::c_void,
                                        charstrings.data.as_ptr() as *const libc::c_void,
                                        offset as _,
                                    );
                                    for j in 1..=i {
                                        charstrings.offset[j as usize] += (len - lenIV) as l_offset;
                                    }
                                } else {
                                    memmove(
                                        charstrings.data[len as usize..].as_mut_ptr()
                                            as *mut libc::c_void,
                                        charstrings.data.as_ptr() as *const libc::c_void,
                                        offset as _,
                                    );
                                    for j in 1..=i {
                                        charstrings.offset[j as usize] += len as l_offset;
                                    }
                                }
                            }
                        }
                        *start = (*start).offset(1);
                        if mode != 1 {
                            if lenIV >= 0 {
                                let offs: i32 = if gid != 0 { offset } else { 0 };
                                charstrings.offset[gid as usize] = (offs + 1) as l_offset;
                                t1_decrypt(
                                    4330_u16,
                                    charstrings.data[offs as usize..].as_mut_ptr(),
                                    *start,
                                    lenIV,
                                    len,
                                );
                                offset += len - lenIV
                            } else {
                                if gid == 0 {
                                    charstrings.offset[gid as usize] = 1 as l_offset;
                                    memcpy(
                                        charstrings.data[0..].as_mut_ptr() as *mut libc::c_void,
                                        *start as *const libc::c_void,
                                        len as _,
                                    );
                                } else {
                                    charstrings.offset[gid as usize] = (offset + 1) as l_offset;
                                    memcpy(
                                        charstrings.data[offset as usize..].as_mut_ptr()
                                            as *mut libc::c_void,
                                        *start as *const libc::c_void,
                                        len as _,
                                    );
                                }
                                offset += len
                            }
                        }
                        *start = (*start).offset(len as isize);
                        match pst_get_token(start, end) {
                            Some(PstObj::Unknown(data))
                                if (data.starts_with(b"ND") || data.starts_with(b"|-")) => {}
                            _ => return Err(()),
                        }
                        i += 1
                    }
                    PstObj::Unknown(_) if (!glyph_name.is_empty() && glyph_name == "end") => {
                        break;
                    }
                    _ => {
                        return Err(());
                    }
                }
            }
            font.charsets = Some(Rc::new(Charsets::Glyphs(charset_glyphs.into_boxed_slice())));
            if mode != 1 {
                font.cstrings.as_mut().unwrap().offset[count as usize] = (offset + 1) as l_offset;
            }
            font.num_glyphs = count as u16;
        }
        _ => {
            let s = tok.into_string();
            warn!("Ignores non dict \"/CharStrings {} ...\"", s);
        }
    }
    Ok(())
}
unsafe fn parse_part2(
    font: &mut cff_font,
    start: &mut *const u8,
    end: *const u8,
    mode: i32,
) -> Result<(), ()> {
    let mut argv: [f64; 127] = [0.; 127];
    let mut lenIV: i32 = 4;
    while *start < end {
        match get_next_key(start, end) {
            None => break,
            Some(key) => match key.as_str() {
                "Subrs" => {
                    /* levIV must appear before Subrs */
                    parse_subrs(font, start, end, lenIV, mode)?;
                }
                "CharStrings" => {
                    parse_charstrings(font, start, end, lenIV, mode)?;
                }
                "lenIV" => {
                    parse_nvalue(start, end, argv.as_mut_ptr(), 1)
                        .and_then(|a| check_size(a, 1))?;
                    lenIV = argv[0] as i32
                }
                "BlueValues" | "OtherBlues" | "FamilyBlues" | "FamilyOtherBlues" | "StemSnapH"
                | "StemSnapV" => {
                    /*
                     * Operand values are delta in CFF font dictionary encoding.
                     */
                    let mut argn =
                        parse_nvalue(start, end, argv.as_mut_ptr(), 127).map_err(|_| {
                            warn!("{} values expected but only {} read.", 0, -1);
                            ()
                        })?;
                    let private = font.private[0].as_mut().unwrap();
                    private.add(&key, argn as i32);
                    loop {
                        if argn == 0 {
                            break;
                        }
                        argn -= 1;
                        private.set(
                            &key,
                            argn as i32,
                            if argn == 0 {
                                argv[argn]
                            } else {
                                argv[argn] - argv[argn - 1]
                            },
                        );
                    }
                }
                "StdHW" | "StdVW" | "BlueScale" | "BlueShift" | "BlueFuzz" | "LanguageGroup"
                | "ExpansionFactor" => {
                    /*
                     * Value of StdHW and StdVW is described as an array in the
                     * Type 1 Font Specification but is a number in CFF format.
                     */
                    parse_nvalue(start, end, argv.as_mut_ptr(), 1)
                        .and_then(|a| check_size(a, 1))?;
                    let private = font.private[0].as_mut().unwrap();
                    private.add(&key, 1);
                    private.set(&key, 0, argv[0]);
                }
                "ForceBold" => {
                    parse_bvalue(start, end, &mut *argv.as_mut_ptr().offset(0))
                        .and_then(|a| check_size(a, 1))?;
                    if argv[0] != 0. {
                        let private = font.private[0].as_mut().unwrap();
                        private.add(&key, 1);
                        private.set(&key, 0, 1.);
                    }
                }
                /*
                 * MinFeature, RndStemUp, UniqueID, Password ignored.
                 */
                _ => {}
            },
        }
    }
    Ok(())
}

fn check_size(a: usize, b: usize) -> Result<usize, ()> {
    if a == b {
        Ok(a)
    } else {
        warn!("{} values expected but only {} read.", b, a);
        Err(())
    }
}

unsafe fn parse_part1(
    font: &mut cff_font,
    enc_vec: &mut [String],
    start: &mut *const u8,
    end: *const u8,
) -> Result<(), ()> {
    let mut argv: [f64; 127] = [0.; 127];
    /*
     * We skip PostScript code inserted before the beginning of
     * font dictionary so that parser will not be confused with
     * it. See LMRoman10-Regular (lmr10.pfb) for example.
     */
    if seek_operator(start, end, b"begin") < 0 {
        return Err(());
    }
    while *start < end {
        match get_next_key(start, end) {
            None => break,
            Some(key) => match key.as_str() {
                "Encoding" => {
                    if parse_encoding(enc_vec, start, end) < 0 {
                        return Err(());
                    }
                }
                "FontName" => {
                    let mut strval = parse_svalue(start, end)?;
                    let len = strval.len();
                    if len > 127 {
                        warn!("FontName too long: {} ({} bytes)", strval, len);
                        strval.truncate(127);
                    }
                    cff_set_name(font, &strval);
                }
                "FontType" => {
                    parse_nvalue(start, end, argv.as_mut_ptr(), 1)
                        .and_then(|a| check_size(a, 1))?;
                    if argv[0] != 1.0f64 {
                        warn!("FontType {} not supported.", argv[0] as i32);
                        return Err(());
                    }
                }
                "ItalicAngle" | "StrokeWidth" | "PaintType" => {
                    parse_nvalue(start, end, argv.as_mut_ptr(), 1)
                        .and_then(|a| check_size(a, 1))?;
                    if argv[0] != 0.0f64 {
                        font.topdict.add(&key, 1);
                        font.topdict.set(&key, 0, argv[0]);
                    }
                }
                "UnderLinePosition" | "UnderLineThickness" => {
                    parse_nvalue(start, end, argv.as_mut_ptr(), 1)
                        .and_then(|a| check_size(a, 1))?;
                    font.topdict.add(&key, 1);
                    font.topdict.set(&key, 0, argv[0]);
                }
                "FontBBox" => {
                    let mut argn = parse_nvalue(start, end, argv.as_mut_ptr(), 4)
                        .and_then(|a| check_size(a, 4))?;
                    font.topdict.add(&key, 4);
                    loop {
                        if argn == 0 {
                            break;
                        }
                        argn -= 1;
                        font.topdict.set(&key, argn as i32, argv[argn]);
                    }
                }
                "FontMatrix" => {
                    let mut argn = parse_nvalue(start, end, argv.as_mut_ptr(), 6)
                        .and_then(|a| check_size(a, 6))?;
                    if argv[0] != 0.001
                        || argv[1] != 0.
                        || argv[2] != 0.
                        || argv[3] != 0.001
                        || argv[4] != 0.
                        || argv[5] != 0.
                    {
                        font.topdict.add(&key, 6);
                        loop {
                            if argn == 0 {
                                break;
                            }
                            argn -= 1;
                            font.topdict.set(&key, argn as i32, argv[argn]);
                        }
                    }
                }
                "version" | "Notice" | "FullName" | "FamilyName" | "Weight" | "Copyright" => {
                    /*
                     * FontInfo
                     */
                    let strval = parse_svalue(start, end)?;
                    font.topdict.add(&key, 1);
                    let mut sid = cff_get_sid(&font, &strval) as s_SID;
                    if sid as i32 == 65535 {
                        sid = cff_add_string(font, &strval, 0)
                    }
                    /*
                     * We don't care about duplicate strings here since
                     * later a subset font of this font will be generated.
                     */
                    font.topdict.set(&key, 0, sid as f64); /* No Global Subr */
                }
                "IsFixedPitch" => {
                    parse_bvalue(start, end, &mut *argv.as_mut_ptr().offset(0))
                        .and_then(|a| check_size(a, 1))?;
                    if argv[0] != 0. {
                        let private = font.private[0].as_mut().unwrap();
                        private.add(&key, 1);
                        private.set(&key, 0, 1.);
                    }
                }
                _ => {}
            },
        }
    }
    Ok(())
}

pub(crate) unsafe fn is_pfb<R: Read + Seek>(handle: &mut R) -> bool {
    let mut sig: [u8; 14] = [0; 14];
    handle.seek(SeekFrom::Start(0)).unwrap();
    handle.read_exact(&mut sig[..1]).ok();
    if sig[0] != 128 {
        return false;
    }
    match handle.read_exact(&mut sig[..1]) {
        Err(_) => return false,
        Ok(()) if sig[0] > 3 => return false,
        _ => {}
    }
    if handle.read_exact(&mut sig[..4]).is_err() {
        // skip bytes
        return false;
    }
    if handle.read_exact(&mut sig[..]).is_err() {
        return false;
    }
    if &sig[..] == b"%!PS-AdobeFont" || &sig[..11] == b"%!FontType1" {
        return true;
    }
    if &sig[..4] == b"%!PS" {
        warn!("Ambiguous PostScript resource type: {}", &sig[..].display());
        return true;
    }
    warn!("Not a PFB font file?");
    false
}
unsafe fn get_pfb_segment<R: Read + Seek>(handle: &mut R, expected_type: i32) -> Option<Vec<u8>> {
    let mut buffer = Vec::<u8>::new();
    let mut bytesread = 0;
    let mut buf = [0_u8; 4];
    loop {
        if handle.read_exact(&mut buf[..1]).is_err() {
            break;
        }
        if buf[0] != 128 {
            panic!("Not a pfb file?");
        }
        let res = handle.read_exact(&mut buf[..1]);
        if res.is_err() || buf[0] as i32 != expected_type {
            handle.seek(SeekFrom::Current(-2)).unwrap();
            break;
        } else {
            if handle.read_exact(&mut buf[..]).is_err() {
                return None;
            }
            let mut slen = u32::from_le_bytes(buf) as usize;
            buffer.resize(bytesread + slen, 0);
            while slen > 0 {
                if let Ok(rlen) = handle.read(&mut buffer[bytesread..bytesread + slen]) {
                    slen -= rlen as usize;
                    bytesread += rlen as usize;
                } else {
                    return None;
                }
            }
        }
    }
    if bytesread == 0 {
        panic!("PFB segment length zero?");
    }
    buffer.push(0);
    Some(buffer)
}

pub(crate) unsafe fn t1_get_standard_glyph(code: i32) -> &'static str {
    if StandardEncoding[code as usize].is_empty() {
        return "";
    }
    StandardEncoding[code as usize]
}

pub(crate) unsafe fn t1_get_fontname<R: Read + Seek>(handle: &mut R, fontname: &mut String) -> i32 {
    let mut fn_found: i32 = 0;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let buffer = get_pfb_segment(handle, 1)
        .filter(|v| v.len() > 0)
        .expect("Reading PFB (ASCII part) file failed.");
    let mut start = buffer.as_ptr();
    let end = buffer.as_ptr().offset(buffer.len() as isize - 1);
    if seek_operator(&mut start, end, b"begin") < 0 {
        return -1;
    }
    while fn_found == 0 && start < end {
        match get_next_key(&mut start, end) {
            None => break,
            Some(key) => match key.as_str() {
                "FontName" => {
                    if let Ok(mut strval) = parse_svalue(&mut start, end) {
                        let len = strval.len();
                        if len > 127 {
                            warn!("FontName \"{}\" too long. ({} bytes)", strval, len);
                            strval.truncate(127);
                        }
                        *fontname = strval;
                        fn_found = 1
                    }
                }
                _ => {}
            },
        }
    }
    0
}

impl cff_font {
    unsafe fn new() -> Self {
        Self {
            handle: None,
            filter: 0,
            fontname: String::new(),
            index: 0,
            flag: 1 << 1,
            header: crate::dpx_cff::cff_header {
                major: 1 as u8,
                minor: 0 as u8,
                hdr_size: 4 as u8,
                offsize: 4 as c_offsize,
            },
            name: CffIndex::new(1),
            topdict: cff_dict::new(),
            string: None,
            gsubr: Some(CffIndex::new(0)),
            encoding: None,
            charsets: None,
            fdselect: None,
            cstrings: None,
            fdarray: Vec::new(),
            private: vec![Some(cff_dict::new())],
            subrs: vec![None],
            offset: 0 as l_offset,
            gsubr_offset: 0 as l_offset,
            num_glyphs: 0,
            num_fds: 1,
            _string: Some(CffIndex::new(0)),
            is_notdef_notzero: 0,
        }
    }
}

pub(crate) unsafe fn t1_load_font(
    enc_vec: &mut [String],
    mode: i32,
    mut handle: InFile,
) -> Box<cff_font> {
    handle.seek(SeekFrom::Start(0)).unwrap();
    /* ASCII section */
    let buffer = get_pfb_segment(&mut handle, 1)
        .filter(|v| v.len() - 1 > 0)
        .expect("Reading PFB (ASCII part) file failed.");
    let mut cff = Box::new(cff_font::new());
    let mut start = buffer.as_ptr();
    let end = buffer.as_ptr().offset(buffer.len() as isize - 1);
    if parse_part1(&mut cff, enc_vec, &mut start, end).is_err() {
        panic!("Reading PFB (ASCII part) file failed.");
    }
    /* Binary section */
    let mut buffer = get_pfb_segment(&mut handle, 2)
        .filter(|v| v.len() - 1 > 0)
        .expect("Reading PFB (BINARY part) file failed.");
    t1_decrypt(
        55665_u16,
        buffer.as_mut_ptr(),
        buffer.as_ptr(),
        0,
        (buffer.len() - 1) as _,
    );
    let mut start = buffer[4..].as_ptr();
    let end = buffer.as_ptr().offset(buffer.len() as isize - 1);
    if parse_part2(&mut cff, &mut start, end, mode).is_err() {
        panic!("Reading PFB (BINARY part) file failed.");
    }
    /* Remaining section ignored. */
    cff_update_string(&mut cff);
    cff
}
