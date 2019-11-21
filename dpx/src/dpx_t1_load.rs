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

use crate::mfree;
use crate::warn;
use crate::DisplayExt;
use crate::{streq_ptr, strstartswith};
use std::ffi::CStr;
use std::ptr;

use super::dpx_cff::{cff_add_string, cff_get_sid, cff_update_string};
use super::dpx_cff::{cff_close, cff_new_index, cff_set_name};
use super::dpx_cff_dict::{cff_dict_add, cff_dict_set, cff_new_dict};
use super::dpx_mem::{new, renew, xstrdup};
use super::dpx_pst::pst_get_token;
use super::dpx_pst_obj::pst_obj;
use super::dpx_pst_obj::{
    pst_data_ptr, pst_getIV, pst_getRV, pst_getSV, pst_release_obj, pst_type_of,
};
use crate::{ttstub_input_getc, ttstub_input_read};
use libc::{free, memcpy, memmove, memset, strcmp, strcpy, strlen};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;
use bridge::InputHandleWrapper;

/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
pub type c_offsize = u8;
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
pub type l_offset = u32;
/* 1, 2, 3, or 4-byte offset */
pub type s_SID = u16;
/* 2-byte string identifier  */

use super::dpx_cff::cff_index;

use super::dpx_cff::cff_dict;
/* Encoding, Charset and FDSelect */
use super::dpx_cff::cff_charsets;

use super::dpx_cff::cff_font;
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
unsafe fn t1_decrypt(
    mut key: u16,
    mut dst: *mut u8,
    mut src: *const u8,
    mut skip: i32,
    mut len: i32,
) {
    len -= skip;
    loop {
        let fresh0 = skip;
        skip = skip - 1;
        if !(fresh0 != 0) {
            break;
        }
        let fresh1 = src;
        src = src.offset(1);
        key = ((key as i32 + *fresh1 as i32) as u32)
            .wrapping_mul(52845u32)
            .wrapping_add(22719u32) as u16
    }
    loop {
        let fresh2 = len;
        len = len - 1;
        if !(fresh2 != 0) {
            break;
        }
        let fresh3 = src;
        src = src.offset(1);
        let c: u8 = *fresh3;
        let fresh4 = dst;
        dst = dst.offset(1);
        *fresh4 = (c as i32 ^ key as i32 >> 8i32) as u8;
        key = ((key as i32 + c as i32) as u32)
            .wrapping_mul(52845u32)
            .wrapping_add(22719u32) as u16
    }
}
/* T1CRYPT */
unsafe fn get_next_key(start: *mut *mut u8, end: *mut u8) -> *mut i8 {
    let mut key: *mut i8 = ptr::null_mut();
    let mut tok: *mut pst_obj = ptr::null_mut();
    while *start < end && {
        tok = pst_get_token(start, end);
        !tok.is_null()
    } {
        if pst_type_of(tok) == 6i32 {
            key = pst_getSV(tok) as *mut i8;
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            break;
        } else if !tok.is_null() {
            pst_release_obj(tok);
            tok = ptr::null_mut()
        }
    }
    key
}
unsafe fn seek_operator(start: *mut *mut u8, end: *mut u8, op: *const i8) -> i32 {
    let mut tok: *mut pst_obj = ptr::null_mut();
    while *start < end && {
        tok = pst_get_token(start, end);
        !tok.is_null()
    } {
        if !tok.is_null()
            && pst_type_of(tok) < 0i32
            && !strstartswith(pst_data_ptr(tok) as *const i8, op).is_null()
        {
            break;
        }
        if !tok.is_null() {
            pst_release_obj(tok);
            tok = ptr::null_mut()
        }
    }
    if tok.is_null() {
        return -1i32;
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    0i32
}
unsafe fn parse_svalue(start: *mut *mut u8, end: *mut u8, value: *mut *mut i8) -> i32 {
    let tok = pst_get_token(start, end);
    if tok.is_null() {
        return -1i32;
    } else {
        if pst_type_of(tok) == 6i32 || pst_type_of(tok) == 5i32 {
            *value = pst_getSV(tok) as *mut i8
        } else {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            return -1i32;
        }
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    1i32
}
unsafe fn parse_bvalue(start: *mut *mut u8, end: *mut u8, value: *mut f64) -> i32 {
    let tok = pst_get_token(start, end);
    if tok.is_null() {
        return -1i32;
    } else {
        if pst_type_of(tok) == 1i32 {
            *value = pst_getIV(tok) as f64
        } else {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            return -1i32;
        }
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    1i32
}
unsafe fn parse_nvalue(
    start: *mut *mut u8,
    end: *mut u8,
    value: *mut f64,
    max: i32,
) -> i32 {
    let mut argn: i32 = 0i32;
    let mut tok = pst_get_token(start, end);
    if tok.is_null() {
        return -1i32;
    }
    /*
     * All array elements must be numeric token. (ATM compatible)
     */
    if (pst_type_of(tok) == 2i32 || pst_type_of(tok) == 3i32) && max > 0i32 {
        *value.offset(0) = pst_getRV(tok);
        argn = 1i32
    } else if pst_type_of(tok) == 7i32 {
        /* It does not distinguish '[' and '{'... */
        if !tok.is_null() {
            pst_release_obj(tok);
            tok = ptr::null_mut()
        }
        while *start < end
            && {
                tok = pst_get_token(start, end);
                !tok.is_null()
            }
            && (pst_type_of(tok) == 2i32 || pst_type_of(tok) == 3i32)
            && argn < max
        {
            let fresh5 = argn;
            argn = argn + 1;
            *value.offset(fresh5 as isize) = pst_getRV(tok);
            if !tok.is_null() {
                pst_release_obj(tok);
                tok = ptr::null_mut()
            }
        }
        if tok.is_null() {
            return -1i32;
        }
        if !(!tok.is_null()
            && pst_type_of(tok) < 0i32
            && !strstartswith(
                pst_data_ptr(tok) as *const i8,
                b"]\x00" as *const u8 as *const i8,
            )
            .is_null())
            && !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"}\x00" as *const u8 as *const i8,
                )
                .is_null())
        {
            argn = -1i32
        }
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    argn
}
static mut StandardEncoding: [*const i8; 256] = [
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclam\x00" as *const u8 as *const i8,
    b"quotedbl\x00" as *const u8 as *const i8,
    b"numbersign\x00" as *const u8 as *const i8,
    b"dollar\x00" as *const u8 as *const i8,
    b"percent\x00" as *const u8 as *const i8,
    b"ampersand\x00" as *const u8 as *const i8,
    b"quoteright\x00" as *const u8 as *const i8,
    b"parenleft\x00" as *const u8 as *const i8,
    b"parenright\x00" as *const u8 as *const i8,
    b"asterisk\x00" as *const u8 as *const i8,
    b"plus\x00" as *const u8 as *const i8,
    b"comma\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"period\x00" as *const u8 as *const i8,
    b"slash\x00" as *const u8 as *const i8,
    b"zero\x00" as *const u8 as *const i8,
    b"one\x00" as *const u8 as *const i8,
    b"two\x00" as *const u8 as *const i8,
    b"three\x00" as *const u8 as *const i8,
    b"four\x00" as *const u8 as *const i8,
    b"five\x00" as *const u8 as *const i8,
    b"six\x00" as *const u8 as *const i8,
    b"seven\x00" as *const u8 as *const i8,
    b"eight\x00" as *const u8 as *const i8,
    b"nine\x00" as *const u8 as *const i8,
    b"colon\x00" as *const u8 as *const i8,
    b"semicolon\x00" as *const u8 as *const i8,
    b"less\x00" as *const u8 as *const i8,
    b"equal\x00" as *const u8 as *const i8,
    b"greater\x00" as *const u8 as *const i8,
    b"question\x00" as *const u8 as *const i8,
    b"at\x00" as *const u8 as *const i8,
    b"A\x00" as *const u8 as *const i8,
    b"B\x00" as *const u8 as *const i8,
    b"C\x00" as *const u8 as *const i8,
    b"D\x00" as *const u8 as *const i8,
    b"E\x00" as *const u8 as *const i8,
    b"F\x00" as *const u8 as *const i8,
    b"G\x00" as *const u8 as *const i8,
    b"H\x00" as *const u8 as *const i8,
    b"I\x00" as *const u8 as *const i8,
    b"J\x00" as *const u8 as *const i8,
    b"K\x00" as *const u8 as *const i8,
    b"L\x00" as *const u8 as *const i8,
    b"M\x00" as *const u8 as *const i8,
    b"N\x00" as *const u8 as *const i8,
    b"O\x00" as *const u8 as *const i8,
    b"P\x00" as *const u8 as *const i8,
    b"Q\x00" as *const u8 as *const i8,
    b"R\x00" as *const u8 as *const i8,
    b"S\x00" as *const u8 as *const i8,
    b"T\x00" as *const u8 as *const i8,
    b"U\x00" as *const u8 as *const i8,
    b"V\x00" as *const u8 as *const i8,
    b"W\x00" as *const u8 as *const i8,
    b"X\x00" as *const u8 as *const i8,
    b"Y\x00" as *const u8 as *const i8,
    b"Z\x00" as *const u8 as *const i8,
    b"bracketleft\x00" as *const u8 as *const i8,
    b"backslash\x00" as *const u8 as *const i8,
    b"bracketright\x00" as *const u8 as *const i8,
    b"asciicircum\x00" as *const u8 as *const i8,
    b"underscore\x00" as *const u8 as *const i8,
    b"quoteleft\x00" as *const u8 as *const i8,
    b"a\x00" as *const u8 as *const i8,
    b"b\x00" as *const u8 as *const i8,
    b"c\x00" as *const u8 as *const i8,
    b"d\x00" as *const u8 as *const i8,
    b"e\x00" as *const u8 as *const i8,
    b"f\x00" as *const u8 as *const i8,
    b"g\x00" as *const u8 as *const i8,
    b"h\x00" as *const u8 as *const i8,
    b"i\x00" as *const u8 as *const i8,
    b"j\x00" as *const u8 as *const i8,
    b"k\x00" as *const u8 as *const i8,
    b"l\x00" as *const u8 as *const i8,
    b"m\x00" as *const u8 as *const i8,
    b"n\x00" as *const u8 as *const i8,
    b"o\x00" as *const u8 as *const i8,
    b"p\x00" as *const u8 as *const i8,
    b"q\x00" as *const u8 as *const i8,
    b"r\x00" as *const u8 as *const i8,
    b"s\x00" as *const u8 as *const i8,
    b"t\x00" as *const u8 as *const i8,
    b"u\x00" as *const u8 as *const i8,
    b"v\x00" as *const u8 as *const i8,
    b"w\x00" as *const u8 as *const i8,
    b"x\x00" as *const u8 as *const i8,
    b"y\x00" as *const u8 as *const i8,
    b"z\x00" as *const u8 as *const i8,
    b"braceleft\x00" as *const u8 as *const i8,
    b"bar\x00" as *const u8 as *const i8,
    b"braceright\x00" as *const u8 as *const i8,
    b"asciitilde\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"exclamdown\x00" as *const u8 as *const i8,
    b"cent\x00" as *const u8 as *const i8,
    b"sterling\x00" as *const u8 as *const i8,
    b"fraction\x00" as *const u8 as *const i8,
    b"yen\x00" as *const u8 as *const i8,
    b"florin\x00" as *const u8 as *const i8,
    b"section\x00" as *const u8 as *const i8,
    b"currency\x00" as *const u8 as *const i8,
    b"quotesingle\x00" as *const u8 as *const i8,
    b"quotedblleft\x00" as *const u8 as *const i8,
    b"guillemotleft\x00" as *const u8 as *const i8,
    b"guilsinglleft\x00" as *const u8 as *const i8,
    b"guilsinglright\x00" as *const u8 as *const i8,
    b"fi\x00" as *const u8 as *const i8,
    b"fl\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"endash\x00" as *const u8 as *const i8,
    b"dagger\x00" as *const u8 as *const i8,
    b"daggerdbl\x00" as *const u8 as *const i8,
    b"periodcentered\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"paragraph\x00" as *const u8 as *const i8,
    b"bullet\x00" as *const u8 as *const i8,
    b"quotesinglbase\x00" as *const u8 as *const i8,
    b"quotedblbase\x00" as *const u8 as *const i8,
    b"quotedblright\x00" as *const u8 as *const i8,
    b"guillemotright\x00" as *const u8 as *const i8,
    b"ellipsis\x00" as *const u8 as *const i8,
    b"perthousand\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"questiondown\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"grave\x00" as *const u8 as *const i8,
    b"acute\x00" as *const u8 as *const i8,
    b"circumflex\x00" as *const u8 as *const i8,
    b"tilde\x00" as *const u8 as *const i8,
    b"macron\x00" as *const u8 as *const i8,
    b"breve\x00" as *const u8 as *const i8,
    b"dotaccent\x00" as *const u8 as *const i8,
    b"dieresis\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"ring\x00" as *const u8 as *const i8,
    b"cedilla\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"hungarumlaut\x00" as *const u8 as *const i8,
    b"ogonek\x00" as *const u8 as *const i8,
    b"caron\x00" as *const u8 as *const i8,
    b"emdash\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"AE\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"ordfeminine\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"Lslash\x00" as *const u8 as *const i8,
    b"Oslash\x00" as *const u8 as *const i8,
    b"OE\x00" as *const u8 as *const i8,
    b"ordmasculine\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"ae\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"dotlessi\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"lslash\x00" as *const u8 as *const i8,
    b"oslash\x00" as *const u8 as *const i8,
    b"oe\x00" as *const u8 as *const i8,
    b"germandbls\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
];
static mut ISOLatin1Encoding: [*const i8; 256] = [
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclam\x00" as *const u8 as *const i8,
    b"quotedbl\x00" as *const u8 as *const i8,
    b"numbersign\x00" as *const u8 as *const i8,
    b"dollar\x00" as *const u8 as *const i8,
    b"percent\x00" as *const u8 as *const i8,
    b"ampersand\x00" as *const u8 as *const i8,
    b"quotesingle\x00" as *const u8 as *const i8,
    b"parenleft\x00" as *const u8 as *const i8,
    b"parenright\x00" as *const u8 as *const i8,
    b"asterisk\x00" as *const u8 as *const i8,
    b"plus\x00" as *const u8 as *const i8,
    b"comma\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"period\x00" as *const u8 as *const i8,
    b"slash\x00" as *const u8 as *const i8,
    b"zero\x00" as *const u8 as *const i8,
    b"one\x00" as *const u8 as *const i8,
    b"two\x00" as *const u8 as *const i8,
    b"three\x00" as *const u8 as *const i8,
    b"four\x00" as *const u8 as *const i8,
    b"five\x00" as *const u8 as *const i8,
    b"six\x00" as *const u8 as *const i8,
    b"seven\x00" as *const u8 as *const i8,
    b"eight\x00" as *const u8 as *const i8,
    b"nine\x00" as *const u8 as *const i8,
    b"colon\x00" as *const u8 as *const i8,
    b"semicolon\x00" as *const u8 as *const i8,
    b"less\x00" as *const u8 as *const i8,
    b"equal\x00" as *const u8 as *const i8,
    b"greater\x00" as *const u8 as *const i8,
    b"question\x00" as *const u8 as *const i8,
    b"at\x00" as *const u8 as *const i8,
    b"A\x00" as *const u8 as *const i8,
    b"B\x00" as *const u8 as *const i8,
    b"C\x00" as *const u8 as *const i8,
    b"D\x00" as *const u8 as *const i8,
    b"E\x00" as *const u8 as *const i8,
    b"F\x00" as *const u8 as *const i8,
    b"G\x00" as *const u8 as *const i8,
    b"H\x00" as *const u8 as *const i8,
    b"I\x00" as *const u8 as *const i8,
    b"J\x00" as *const u8 as *const i8,
    b"K\x00" as *const u8 as *const i8,
    b"L\x00" as *const u8 as *const i8,
    b"M\x00" as *const u8 as *const i8,
    b"N\x00" as *const u8 as *const i8,
    b"O\x00" as *const u8 as *const i8,
    b"P\x00" as *const u8 as *const i8,
    b"Q\x00" as *const u8 as *const i8,
    b"R\x00" as *const u8 as *const i8,
    b"S\x00" as *const u8 as *const i8,
    b"T\x00" as *const u8 as *const i8,
    b"U\x00" as *const u8 as *const i8,
    b"V\x00" as *const u8 as *const i8,
    b"W\x00" as *const u8 as *const i8,
    b"X\x00" as *const u8 as *const i8,
    b"Y\x00" as *const u8 as *const i8,
    b"Z\x00" as *const u8 as *const i8,
    b"bracketleft\x00" as *const u8 as *const i8,
    b"backslash\x00" as *const u8 as *const i8,
    b"bracketright\x00" as *const u8 as *const i8,
    b"asciicircum\x00" as *const u8 as *const i8,
    b"underscore\x00" as *const u8 as *const i8,
    b"grave\x00" as *const u8 as *const i8,
    b"a\x00" as *const u8 as *const i8,
    b"b\x00" as *const u8 as *const i8,
    b"c\x00" as *const u8 as *const i8,
    b"d\x00" as *const u8 as *const i8,
    b"e\x00" as *const u8 as *const i8,
    b"f\x00" as *const u8 as *const i8,
    b"g\x00" as *const u8 as *const i8,
    b"h\x00" as *const u8 as *const i8,
    b"i\x00" as *const u8 as *const i8,
    b"j\x00" as *const u8 as *const i8,
    b"k\x00" as *const u8 as *const i8,
    b"l\x00" as *const u8 as *const i8,
    b"m\x00" as *const u8 as *const i8,
    b"n\x00" as *const u8 as *const i8,
    b"o\x00" as *const u8 as *const i8,
    b"p\x00" as *const u8 as *const i8,
    b"q\x00" as *const u8 as *const i8,
    b"r\x00" as *const u8 as *const i8,
    b"s\x00" as *const u8 as *const i8,
    b"t\x00" as *const u8 as *const i8,
    b"u\x00" as *const u8 as *const i8,
    b"v\x00" as *const u8 as *const i8,
    b"w\x00" as *const u8 as *const i8,
    b"x\x00" as *const u8 as *const i8,
    b"y\x00" as *const u8 as *const i8,
    b"z\x00" as *const u8 as *const i8,
    b"braceleft\x00" as *const u8 as *const i8,
    b"bar\x00" as *const u8 as *const i8,
    b"braceright\x00" as *const u8 as *const i8,
    b"asciitilde\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"dotlessi\x00" as *const u8 as *const i8,
    b"quoteleft\x00" as *const u8 as *const i8,
    b"quoteright\x00" as *const u8 as *const i8,
    b"circumflex\x00" as *const u8 as *const i8,
    b"tilde\x00" as *const u8 as *const i8,
    b"macron\x00" as *const u8 as *const i8,
    b"breve\x00" as *const u8 as *const i8,
    b"dotaccent\x00" as *const u8 as *const i8,
    b"dieresis\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"ring\x00" as *const u8 as *const i8,
    b"cedilla\x00" as *const u8 as *const i8,
    b".notdef\x00" as *const u8 as *const i8,
    b"hungarumlaut\x00" as *const u8 as *const i8,
    b"ogonek\x00" as *const u8 as *const i8,
    b"caron\x00" as *const u8 as *const i8,
    b"space\x00" as *const u8 as *const i8,
    b"exclamdown\x00" as *const u8 as *const i8,
    b"cent\x00" as *const u8 as *const i8,
    b"sterling\x00" as *const u8 as *const i8,
    b"currency\x00" as *const u8 as *const i8,
    b"yen\x00" as *const u8 as *const i8,
    b"brokenbar\x00" as *const u8 as *const i8,
    b"section\x00" as *const u8 as *const i8,
    b"dieresis\x00" as *const u8 as *const i8,
    b"copyright\x00" as *const u8 as *const i8,
    b"ordfeminine\x00" as *const u8 as *const i8,
    b"guillemotleft\x00" as *const u8 as *const i8,
    b"logicalnot\x00" as *const u8 as *const i8,
    b"hyphen\x00" as *const u8 as *const i8,
    b"registered\x00" as *const u8 as *const i8,
    b"macron\x00" as *const u8 as *const i8,
    b"degree\x00" as *const u8 as *const i8,
    b"plusminus\x00" as *const u8 as *const i8,
    b"twosuperior\x00" as *const u8 as *const i8,
    b"threesuperior\x00" as *const u8 as *const i8,
    b"acute\x00" as *const u8 as *const i8,
    b"mu\x00" as *const u8 as *const i8,
    b"paragraph\x00" as *const u8 as *const i8,
    b"periodcentered\x00" as *const u8 as *const i8,
    b"cedilla\x00" as *const u8 as *const i8,
    b"onesuperior\x00" as *const u8 as *const i8,
    b"ordmasculine\x00" as *const u8 as *const i8,
    b"guillemotright\x00" as *const u8 as *const i8,
    b"onequarter\x00" as *const u8 as *const i8,
    b"onehalf\x00" as *const u8 as *const i8,
    b"threequarters\x00" as *const u8 as *const i8,
    b"questiondown\x00" as *const u8 as *const i8,
    b"Agrave\x00" as *const u8 as *const i8,
    b"Aacute\x00" as *const u8 as *const i8,
    b"Acircumflex\x00" as *const u8 as *const i8,
    b"Atilde\x00" as *const u8 as *const i8,
    b"Adieresis\x00" as *const u8 as *const i8,
    b"Aring\x00" as *const u8 as *const i8,
    b"AE\x00" as *const u8 as *const i8,
    b"Ccedilla\x00" as *const u8 as *const i8,
    b"Egrave\x00" as *const u8 as *const i8,
    b"Eacute\x00" as *const u8 as *const i8,
    b"Ecircumflex\x00" as *const u8 as *const i8,
    b"Edieresis\x00" as *const u8 as *const i8,
    b"Igrave\x00" as *const u8 as *const i8,
    b"Iacute\x00" as *const u8 as *const i8,
    b"Icircumflex\x00" as *const u8 as *const i8,
    b"Idieresis\x00" as *const u8 as *const i8,
    b"Eth\x00" as *const u8 as *const i8,
    b"Ntilde\x00" as *const u8 as *const i8,
    b"Ograve\x00" as *const u8 as *const i8,
    b"Oacute\x00" as *const u8 as *const i8,
    b"Ocircumflex\x00" as *const u8 as *const i8,
    b"Otilde\x00" as *const u8 as *const i8,
    b"Odieresis\x00" as *const u8 as *const i8,
    b"multiply\x00" as *const u8 as *const i8,
    b"Oslash\x00" as *const u8 as *const i8,
    b"Ugrave\x00" as *const u8 as *const i8,
    b"Uacute\x00" as *const u8 as *const i8,
    b"Ucircumflex\x00" as *const u8 as *const i8,
    b"Udieresis\x00" as *const u8 as *const i8,
    b"Yacute\x00" as *const u8 as *const i8,
    b"Thorn\x00" as *const u8 as *const i8,
    b"germandbls\x00" as *const u8 as *const i8,
    b"agrave\x00" as *const u8 as *const i8,
    b"aacute\x00" as *const u8 as *const i8,
    b"acircumflex\x00" as *const u8 as *const i8,
    b"atilde\x00" as *const u8 as *const i8,
    b"adieresis\x00" as *const u8 as *const i8,
    b"aring\x00" as *const u8 as *const i8,
    b"ae\x00" as *const u8 as *const i8,
    b"ccedilla\x00" as *const u8 as *const i8,
    b"egrave\x00" as *const u8 as *const i8,
    b"eacute\x00" as *const u8 as *const i8,
    b"ecircumflex\x00" as *const u8 as *const i8,
    b"edieresis\x00" as *const u8 as *const i8,
    b"igrave\x00" as *const u8 as *const i8,
    b"iacute\x00" as *const u8 as *const i8,
    b"icircumflex\x00" as *const u8 as *const i8,
    b"idieresis\x00" as *const u8 as *const i8,
    b"eth\x00" as *const u8 as *const i8,
    b"ntilde\x00" as *const u8 as *const i8,
    b"ograve\x00" as *const u8 as *const i8,
    b"oacute\x00" as *const u8 as *const i8,
    b"ocircumflex\x00" as *const u8 as *const i8,
    b"otilde\x00" as *const u8 as *const i8,
    b"odieresis\x00" as *const u8 as *const i8,
    b"divide\x00" as *const u8 as *const i8,
    b"oslash\x00" as *const u8 as *const i8,
    b"ugrave\x00" as *const u8 as *const i8,
    b"uacute\x00" as *const u8 as *const i8,
    b"ucircumflex\x00" as *const u8 as *const i8,
    b"udieresis\x00" as *const u8 as *const i8,
    b"yacute\x00" as *const u8 as *const i8,
    b"thorn\x00" as *const u8 as *const i8,
    b"ydieresis\x00" as *const u8 as *const i8,
];
/* Treat cases such as "dup num num getinterval num exch putinterval"
 * or "dup num exch num get put"
 */
unsafe fn try_put_or_putinterval(
    enc_vec: *mut *mut i8,
    start: *mut *mut u8,
    end: *mut u8,
) -> i32 {
    let mut num1: i32 = 0;
    let mut num2: i32 = 0;
    let mut num3: i32 = 0;
    let mut tok = pst_get_token(start, end);
    if tok.is_null()
        || !(pst_type_of(tok) == 2i32)
        || {
            num1 = pst_getIV(tok);
            num1 > 255i32
        }
        || num1 < 0i32
    {
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        return -1i32;
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    tok = pst_get_token(start, end);
    if tok.is_null() {
        return -1i32;
    } else {
        if !tok.is_null()
            && pst_type_of(tok) < 0i32
            && !strstartswith(
                pst_data_ptr(tok) as *const i8,
                b"exch\x00" as *const u8 as *const i8,
            )
            .is_null()
        {
            /* dup num exch num get put */
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if tok.is_null()
                || !(pst_type_of(tok) == 2i32)
                || {
                    num2 = pst_getIV(tok);
                    num2 > 255i32
                }
                || num2 < 0i32
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"get\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"put\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            free(*enc_vec.offset(num1 as isize) as *mut libc::c_void);
            let ref mut fresh6 = *enc_vec.offset(num1 as isize);
            *fresh6 = xstrdup(*enc_vec.offset(num2 as isize))
        } else if pst_type_of(tok) == 2i32
            && {
                num2 = pst_getIV(tok);
                num2 + num1 <= 255i32
            }
            && num2 >= 0i32
        {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"getinterval\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if tok.is_null()
                || !(pst_type_of(tok) == 2i32)
                || {
                    num3 = pst_getIV(tok);
                    num3 + num2 > 255i32
                }
                || num3 < 0i32
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"exch\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"putinterval\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            for i in 0..num2 {
                if !(*enc_vec.offset((num1 + i) as isize)).is_null() {
                    /* num1 + i < 256 here */
                    let ref mut fresh7 = *enc_vec.offset((num3 + i) as isize);
                    *fresh7 =
                        mfree(*enc_vec.offset((num3 + i) as isize) as *mut libc::c_void) as *mut i8;
                    let ref mut fresh8 = *enc_vec.offset((num3 + i) as isize);
                    *fresh8 = xstrdup(*enc_vec.offset((num1 + i) as isize))
                }
            }
        } else {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            return -1i32;
        }
    }
    0i32
}
unsafe fn parse_encoding(
    enc_vec: *mut *mut i8,
    start: *mut *mut u8,
    end: *mut u8,
) -> i32 {
    let mut code: i32 = 0;
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
    let mut tok = pst_get_token(start, end);
    if !tok.is_null()
        && pst_type_of(tok) < 0i32
        && !strstartswith(
            pst_data_ptr(tok) as *const i8,
            b"StandardEncoding\x00" as *const u8 as *const i8,
        )
        .is_null()
    {
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        if !enc_vec.is_null() {
            code = 0i32;
            while code < 256i32 {
                if !StandardEncoding[code as usize].is_null()
                    && strcmp(
                        StandardEncoding[code as usize],
                        b".notdef\x00" as *const u8 as *const i8,
                    ) != 0i32
                {
                    let ref mut fresh9 = *enc_vec.offset(code as isize);
                    *fresh9 = new((strlen(StandardEncoding[code as usize]).wrapping_add(1))
                        .wrapping_mul(::std::mem::size_of::<i8>())
                        as _) as *mut i8;
                    strcpy(
                        *enc_vec.offset(code as isize),
                        StandardEncoding[code as usize],
                    );
                } else {
                    let ref mut fresh10 = *enc_vec.offset(code as isize);
                    *fresh10 = ptr::null_mut()
                }
                code += 1
            }
        }
    } else if !tok.is_null()
        && pst_type_of(tok) < 0i32
        && !strstartswith(
            pst_data_ptr(tok) as *const i8,
            b"ISOLatin1Encoding\x00" as *const u8 as *const i8,
        )
        .is_null()
    {
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        if !enc_vec.is_null() {
            code = 0i32;
            while code < 256i32 {
                if !ISOLatin1Encoding[code as usize].is_null()
                    && strcmp(
                        ISOLatin1Encoding[code as usize],
                        b".notdef\x00" as *const u8 as *const i8,
                    ) != 0i32
                {
                    let ref mut fresh11 = *enc_vec.offset(code as isize);
                    *fresh11 = new((strlen(ISOLatin1Encoding[code as usize]).wrapping_add(1))
                        .wrapping_mul(::std::mem::size_of::<i8>())
                        as _) as *mut i8;
                    strcpy(
                        *enc_vec.offset(code as isize),
                        ISOLatin1Encoding[code as usize],
                    );
                } else {
                    let ref mut fresh12 = *enc_vec.offset(code as isize);
                    *fresh12 = ptr::null_mut()
                }
                code += 1
            }
        }
    } else if !tok.is_null()
        && pst_type_of(tok) < 0i32
        && !strstartswith(
            pst_data_ptr(tok) as *const i8,
            b"ExpertEncoding\x00" as *const u8 as *const i8,
        )
        .is_null()
    {
        if !tok.is_null() {
            pst_release_obj(tok);
            tok = ptr::null_mut()
        }
        if !enc_vec.is_null() {
            warn!("ExpertEncoding not supported.");
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            return -1i32;
        }
    /*
     * Not supported yet.
     */
    } else {
        if !tok.is_null() {
            pst_release_obj(tok);
            tok = ptr::null_mut()
        }
        seek_operator(start, end, b"array\x00" as *const u8 as *const i8);
        /*
         * Pick all seaquences that matches "dup n /Name put" until
         * occurrence of "def" or "readonly".
         */
        while *start < end && {
            tok = pst_get_token(start, end);
            !tok.is_null()
        } {
            if !tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"def\x00" as *const u8 as *const i8,
                )
                .is_null()
                || !tok.is_null()
                    && pst_type_of(tok) < 0i32
                    && !strstartswith(
                        pst_data_ptr(tok) as *const i8,
                        b"readonly\x00" as *const u8 as *const i8,
                    )
                    .is_null()
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                break;
            } else if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"dup\x00" as *const u8 as *const i8,
                )
                .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    tok = ptr::null_mut()
                }
            } else {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                /* cmctt10.pfb for examples contains the following PS code
                 *     dup num num getinterval num exch putinterval
                 *     dup num exch num get put
                 */
                tok = pst_get_token(start, end);
                if !tok.is_null()
                    && pst_type_of(tok) < 0i32
                    && !strstartswith(
                        pst_data_ptr(tok) as *const i8,
                        b"dup\x00" as *const u8 as *const i8,
                    )
                    .is_null()
                {
                    /* possibly putinterval type */
                    if enc_vec.is_null() {
                        warn!("This kind of type1 fonts are not supported as native fonts.\n                   They are supported if used with tfm fonts.\n");
                    } else {
                        try_put_or_putinterval(enc_vec, start, end);
                    }
                    if !tok.is_null() {
                        pst_release_obj(tok);
                        tok = ptr::null_mut()
                    }
                } else if tok.is_null()
                    || !(pst_type_of(tok) == 2i32)
                    || {
                        code = pst_getIV(tok);
                        code > 255i32
                    }
                    || code < 0i32
                {
                    if !tok.is_null() {
                        pst_release_obj(tok);
                        tok = ptr::null_mut()
                    }
                } else {
                    if !tok.is_null() {
                        pst_release_obj(tok);
                        //tok = ptr::null_mut()
                    }
                    tok = pst_get_token(start, end);
                    if tok.is_null() || !(pst_type_of(tok) == 6i32) {
                        if !tok.is_null() {
                            pst_release_obj(tok);
                            tok = ptr::null_mut()
                        }
                    } else {
                        if !enc_vec.is_null() {
                            free(*enc_vec.offset(code as isize) as *mut libc::c_void);
                            let ref mut fresh13 = *enc_vec.offset(code as isize);
                            *fresh13 = pst_getSV(tok) as *mut i8
                        }
                        if !tok.is_null() {
                            pst_release_obj(tok);
                            //tok = ptr::null_mut()
                        }
                        tok = pst_get_token(start, end);
                        if !(!tok.is_null()
                            && pst_type_of(tok) < 0i32
                            && !strstartswith(
                                pst_data_ptr(tok) as *const i8,
                                b"put\x00" as *const u8 as *const i8,
                            )
                            .is_null())
                        {
                            let ref mut fresh14 = *enc_vec.offset(code as isize);
                            *fresh14 = mfree(*enc_vec.offset(code as isize) as *mut libc::c_void)
                                as *mut i8;
                            if !tok.is_null() {
                                pst_release_obj(tok);
                                tok = ptr::null_mut()
                            }
                        } else if !tok.is_null() {
                            pst_release_obj(tok);
                            tok = ptr::null_mut()
                        }
                    }
                }
            }
        }
    }
    0i32
}
unsafe fn parse_subrs(
    font: &cff_font,
    start: *mut *mut u8,
    end: *mut u8,
    lenIV: i32,
    mode: i32,
) -> i32 {
    let mut max_size;
    let offsets;
    let lengths;
    let mut data;
    let tok = pst_get_token(start, end);
    if !(pst_type_of(tok) == 2i32) || pst_getIV(tok) < 0i32 {
        warn!("Parsing Subrs failed.");
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        return -1i32;
    }
    let count = pst_getIV(tok);
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    if count == 0i32 {
        let ref mut fresh15 = *font.subrs.offset(0);
        *fresh15 = ptr::null_mut();
        return 0i32;
    }
    let mut tok = pst_get_token(start, end);
    if !(!tok.is_null()
        && pst_type_of(tok) < 0i32
        && !strstartswith(
            pst_data_ptr(tok) as *const i8,
            b"array\x00" as *const u8 as *const i8,
        )
        .is_null())
    {
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        return -1i32;
    }
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    if mode != 1i32 {
        max_size = 65536i32;
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
            0i32,
            (::std::mem::size_of::<i32>()).wrapping_mul(count as _),
        );
        memset(
            lengths as *mut libc::c_void,
            0i32,
            (::std::mem::size_of::<i32>()).wrapping_mul(count as _),
        );
    } else {
        max_size = 0i32;
        data = ptr::null_mut();
        offsets = ptr::null_mut();
        lengths = ptr::null_mut()
    }
    let mut offset = 0;
    /* dup subr# n-bytes RD n-binary-bytes NP */
    let mut i = 0;
    while i < count {
        tok = pst_get_token(start, end);
        if tok.is_null() {
            free(data as *mut libc::c_void);
            free(offsets as *mut libc::c_void);
            free(lengths as *mut libc::c_void);
            return -1i32;
        } else if !tok.is_null()
            && pst_type_of(tok) < 0i32
            && !strstartswith(
                pst_data_ptr(tok) as *const i8,
                b"ND\x00" as *const u8 as *const i8,
            )
            .is_null()
            || !tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"|-\x00" as *const u8 as *const i8,
                )
                .is_null()
            || !tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"def\x00" as *const u8 as *const i8,
                )
                .is_null()
        {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            break;
        } else if !(!tok.is_null()
            && pst_type_of(tok) < 0i32
            && !strstartswith(
                pst_data_ptr(tok) as *const i8,
                b"dup\x00" as *const u8 as *const i8,
            )
            .is_null())
        {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
        } else {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            /* Found "dup" */
            tok = pst_get_token(start, end);
            if !(pst_type_of(tok) == 2i32) || pst_getIV(tok) < 0i32 || pst_getIV(tok) >= count {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                free(data as *mut libc::c_void);
                free(offsets as *mut libc::c_void);
                free(lengths as *mut libc::c_void);
                return -1i32;
            }
            let idx = pst_getIV(tok);
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(pst_type_of(tok) == 2i32) || pst_getIV(tok) < 0i32 || pst_getIV(tok) > 65536i32 {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            let len = pst_getIV(tok);
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"RD\x00" as *const u8 as *const i8,
                )
                .is_null())
                && !(!tok.is_null()
                    && pst_type_of(tok) < 0i32
                    && !strstartswith(
                        pst_data_ptr(tok) as *const i8,
                        b"-|\x00" as *const u8 as *const i8,
                    )
                    .is_null())
                && seek_operator(start, end, b"readstring\x00" as *const u8 as *const i8) < 0i32
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                free(data as *mut libc::c_void);
                free(offsets as *mut libc::c_void);
                free(lengths as *mut libc::c_void);
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            *start = (*start).offset(1);
            if (*start).offset(len as isize) >= end {
                free(data as *mut libc::c_void);
                free(offsets as *mut libc::c_void);
                free(lengths as *mut libc::c_void);
                return -1i32;
            }
            if mode != 1i32 {
                if offset + len >= max_size {
                    max_size += 65536i32;
                    data = renew(
                        data as *mut libc::c_void,
                        (max_size as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                            as u32,
                    ) as *mut u8
                }
                if lenIV >= 0i32 {
                    t1_decrypt(4330_u16, data.offset(offset as isize), *start, lenIV, len);
                    *offsets.offset(idx as isize) = offset;
                    let ref mut fresh16 = *lengths.offset(idx as isize);
                    *fresh16 = len - lenIV;
                    offset += *fresh16
                } else if len > 0i32 {
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
    }
    if mode != 1i32 {
        if (*font.subrs.offset(0)).is_null() {
            let ref mut fresh17 = *font.subrs.offset(0);
            *fresh17 = cff_new_index(count as u16);
            let subrs = *fresh17;
            (*subrs).data = new((offset as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                as u32) as *mut u8;
            offset = 0i32;
            for i in 0..count {
                *(*subrs).offset.offset(i as isize) = (offset + 1i32) as l_offset;
                if *lengths.offset(i as isize) > 0i32 {
                    memcpy(
                        (*subrs).data.offset(offset as isize) as *mut libc::c_void,
                        data.offset(*offsets.offset(i as isize) as isize) as *const libc::c_void,
                        *lengths.offset(i as isize) as _,
                    );
                    offset += *lengths.offset(i as isize)
                }
            }
            *(*subrs).offset.offset(count as isize) = (offset + 1i32) as l_offset
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
    0i32
}
unsafe fn parse_charstrings(
    font: &mut cff_font,
    start: *mut *mut u8,
    end: *mut u8,
    lenIV: i32,
    mode: i32,
) -> i32 {
    let charstrings;
    let mut max_size;
    /* /CharStrings n dict dup begin
     * /GlyphName n-bytes RD -n-binary-bytes- ND
     * ...
     * end
     *  - stack - ... /CharStrings dict
     */
    let mut tok = pst_get_token(start, end); /* .notdef must be at gid = 0 in CFF */
    if !(pst_type_of(tok) == 2i32) || pst_getIV(tok) < 0i32 || pst_getIV(tok) > 64999i32 {
        let s: *mut u8 = pst_getSV(tok);
        warn!(
            "Ignores non dict \"/CharStrings {} ...\"",
            CStr::from_ptr(s as *mut i8).display(),
        );
        free(s as *mut libc::c_void);
        if !tok.is_null() {
            pst_release_obj(tok);
            //tok = ptr::null_mut()
        }
        return 0i32;
    }
    let count = pst_getIV(tok);
    if !tok.is_null() {
        pst_release_obj(tok);
        //tok = ptr::null_mut()
    }
    if mode != 1i32 {
        charstrings = cff_new_index(count as u16);
        max_size = 65536i32;
        (*charstrings).data = new((max_size as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<u8>() as u64)
            as u32) as *mut u8
    } else {
        charstrings = ptr::null_mut();
        max_size = 0i32
    }
    font.cstrings = charstrings;
    font.charsets = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    let charset = font.charsets;
    (*charset).format = 0i32 as u8;
    (*charset).num_entries = (count - 1i32) as u16;
    (*charset).data.glyphs = new(((count - 1i32) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<s_SID>() as u64)
        as u32) as *mut s_SID;
    memset(
        (*charset).data.glyphs as *mut libc::c_void,
        0i32,
        (::std::mem::size_of::<s_SID>()).wrapping_mul(count as usize - 1),
    );
    let mut offset = 0i32;
    let mut have_notdef = 0i32;
    font.is_notdef_notzero = 0i32;
    seek_operator(start, end, b"begin\x00" as *const u8 as *const i8);
    let mut i = 0;
    while i < count {
        /* BUG-20061126 (by ChoF):
         * Some fonts (e.g., belleek/blsy.pfb) does not have the correct number
         * of glyphs. Modify the codes even to work with these broken fonts.
         */
        tok = pst_get_token(start, end);
        let glyph_name = pst_getSV(tok) as *mut i8;
        if i == 0i32
            && !glyph_name.is_null()
            && strcmp(glyph_name, b".notdef\x00" as *const u8 as *const i8) != 0i32
        {
            font.is_notdef_notzero = 1i32
        }
        if pst_type_of(tok) == 6i32 {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            let gid = if glyph_name.is_null() {
                return -1i32;
            } else {
                if streq_ptr(glyph_name, b".notdef\x00" as *const u8 as *const i8) {
                    have_notdef = 1;
                    0
                } else if have_notdef != 0 {
                    i
                } else if i == count - 1i32 {
                    warn!("No .notdef glyph???");
                    return -1i32;
                } else {
                    i + 1
                }
            };
            if gid > 0 {
                *(*charset).data.glyphs.offset((gid - 1i32) as isize) =
                    cff_add_string(font, glyph_name, 0i32)
            }
            /*
             * We don't care about duplicate strings here since
             * later a subset font of this font will be generated.
             */
            free(glyph_name as *mut libc::c_void); /* start at 1 */
            tok = pst_get_token(start, end);
            if !(pst_type_of(tok) == 2i32) || pst_getIV(tok) < 0i32 || pst_getIV(tok) > 65536i32 {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            let len = pst_getIV(tok);
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"RD\x00" as *const u8 as *const i8,
                )
                .is_null())
                && !(!tok.is_null()
                    && pst_type_of(tok) < 0i32
                    && !strstartswith(
                        pst_data_ptr(tok) as *const i8,
                        b"-|\x00" as *const u8 as *const i8,
                    )
                    .is_null())
                && seek_operator(start, end, b"readstring\x00" as *const u8 as *const i8) < 0i32
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            if (*start).offset(len as isize).offset(1) >= end {
                return -1i32;
            }
            if mode != 1i32 {
                if offset + len >= max_size {
                    max_size += if len > 65536i32 { len } else { 65536i32 };
                    (*charstrings).data = renew(
                        (*charstrings).data as *mut libc::c_void,
                        (max_size as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                            as u32,
                    ) as *mut u8
                }
                if gid == 0i32 {
                    if lenIV >= 0i32 {
                        memmove(
                            (*charstrings)
                                .data
                                .offset(len as isize)
                                .offset(-(lenIV as isize))
                                as *mut libc::c_void,
                            (*charstrings).data as *const libc::c_void,
                            offset as _,
                        );
                        for j in 1..=i {
                            let ref mut fresh18 = *(*charstrings).offset.offset(j as isize);
                            *fresh18 = (*fresh18 as u32).wrapping_add((len - lenIV) as u32)
                                as l_offset as l_offset;
                        }
                    } else {
                        memmove(
                            (*charstrings).data.offset(len as isize) as *mut libc::c_void,
                            (*charstrings).data as *const libc::c_void,
                            offset as _,
                        );
                        for j in 1..=i {
                            let ref mut fresh19 = *(*charstrings).offset.offset(j as isize);
                            *fresh19 =
                                (*fresh19 as u32).wrapping_add(len as u32) as l_offset as l_offset;
                        }
                    }
                }
            }
            *start = (*start).offset(1);
            if mode != 1i32 {
                if lenIV >= 0i32 {
                    let offs: i32 = if gid != 0 { offset } else { 0i32 };
                    *(*charstrings).offset.offset(gid as isize) = (offs + 1i32) as l_offset;
                    t1_decrypt(
                        4330_u16,
                        (*charstrings).data.offset(offs as isize),
                        *start,
                        lenIV,
                        len,
                    );
                    offset += len - lenIV
                } else {
                    if gid == 0i32 {
                        *(*charstrings).offset.offset(gid as isize) = 1i32 as l_offset;
                        memcpy(
                            &mut *(*charstrings).data.offset(0) as *mut u8 as *mut libc::c_void,
                            *start as *const libc::c_void,
                            len as _,
                        );
                    } else {
                        *(*charstrings).offset.offset(gid as isize) = (offset + 1i32) as l_offset;
                        memcpy(
                            &mut *(*charstrings).data.offset(offset as isize) as *mut u8
                                as *mut libc::c_void,
                            *start as *const libc::c_void,
                            len as _,
                        );
                    }
                    offset += len
                }
            }
            *start = (*start).offset(len as isize);
            tok = pst_get_token(start, end);
            if !(!tok.is_null()
                && pst_type_of(tok) < 0i32
                && !strstartswith(
                    pst_data_ptr(tok) as *const i8,
                    b"ND\x00" as *const u8 as *const i8,
                )
                .is_null())
                && !(!tok.is_null()
                    && pst_type_of(tok) < 0i32
                    && !strstartswith(
                        pst_data_ptr(tok) as *const i8,
                        b"|-\x00" as *const u8 as *const i8,
                    )
                    .is_null())
            {
                if !tok.is_null() {
                    pst_release_obj(tok);
                    //tok = ptr::null_mut()
                }
                return -1i32;
            }
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            i += 1
        } else if pst_type_of(tok) < 0i32
            && streq_ptr(glyph_name, b"end\x00" as *const u8 as *const i8) as i32 != 0
        {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            break;
        } else {
            if !tok.is_null() {
                pst_release_obj(tok);
                //tok = ptr::null_mut()
            }
            return -1i32;
        }
    }
    if mode != 1i32 {
        *(*charstrings).offset.offset(count as isize) = (offset + 1i32) as l_offset
    }
    font.num_glyphs = count as u16;
    0i32
}
unsafe fn parse_part2(
    font: &mut cff_font,
    start: *mut *mut u8,
    end: *mut u8,
    mode: i32,
) -> i32 {
    let mut key: *mut i8 = ptr::null_mut();
    let mut argv: [f64; 127] = [0.; 127];
    let mut lenIV: i32 = 4i32;
    while *start < end && {
        key = get_next_key(start, end);
        !key.is_null()
    } {
        if streq_ptr(key, b"Subrs\x00" as *const u8 as *const i8) {
            /* levIV must appear before Subrs */
            if parse_subrs(font, start, end, lenIV, mode) < 0i32 {
                free(key as *mut libc::c_void);
                return -1i32;
            }
        } else if streq_ptr(key, b"CharStrings\x00" as *const u8 as *const i8) {
            if parse_charstrings(font, start, end, lenIV, mode) < 0i32 {
                free(key as *mut libc::c_void);
                return -1i32;
            }
        } else if streq_ptr(key, b"lenIV\x00" as *const u8 as *const i8) {
            let argn = parse_nvalue(start, end, argv.as_mut_ptr(), 1i32);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            lenIV = argv[0] as i32
        } else if streq_ptr(key, b"BlueValues\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"OtherBlues\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"FamilyBlues\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"FamilyOtherBlues\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"StemSnapH\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"StemSnapV\x00" as *const u8 as *const i8) as i32 != 0
        {
            /*
             * Operand values are delta in CFF font dictionary encoding.
             */
            let mut argn = parse_nvalue(start, end, argv.as_mut_ptr(), 127i32);
            if argn < 0i32 {
                warn!("{} values expected but only {} read.", 0i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            cff_dict_add(*font.private.offset(0), key, argn);
            loop {
                let fresh20 = argn;
                argn = argn - 1;
                if !(fresh20 > 0i32) {
                    break;
                }
                cff_dict_set(
                    *font.private.offset(0),
                    key,
                    argn,
                    if argn == 0i32 {
                        argv[argn as usize]
                    } else {
                        argv[argn as usize] - argv[(argn - 1i32) as usize]
                    },
                );
            }
        } else if streq_ptr(key, b"StdHW\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"StdVW\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"BlueScale\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"BlueShift\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"BlueFuzz\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"LanguageGroup\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"ExpansionFactor\x00" as *const u8 as *const i8) as i32 != 0
        {
            /*
             * Value of StdHW and StdVW is described as an array in the
             * Type 1 Font Specification but is a number in CFF format.
             */
            let argn = parse_nvalue(start, end, argv.as_mut_ptr(), 1i32);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            cff_dict_add(*font.private.offset(0), key, 1i32);
            cff_dict_set(*font.private.offset(0), key, 0i32, argv[0]);
        } else if streq_ptr(key, b"ForceBold\x00" as *const u8 as *const i8) {
            let argn = parse_bvalue(start, end, &mut *argv.as_mut_ptr().offset(0));
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if argv[0] != 0i32 as f64 {
                cff_dict_add(*font.private.offset(0), key, 1i32);
                cff_dict_set(*font.private.offset(0), key, 0i32, 1i32 as f64);
            }
        }
        /*
         * MinFeature, RndStemUp, UniqueID, Password ignored.
         */
        free(key as *mut libc::c_void); /* Macro CHECK_ARGN_XX assume 'argn' is used. */
    }
    0i32
}
unsafe fn parse_part1(
    font: &mut cff_font,
    enc_vec: *mut *mut i8,
    start: *mut *mut u8,
    end: *mut u8,
) -> i32 {
    let mut key: *mut i8 = ptr::null_mut();
    let mut strval: *mut i8 = ptr::null_mut();
    let mut argv: [f64; 127] = [0.; 127];
    /*
     * We skip PostScript code inserted before the beginning of
     * font dictionary so that parser will not be confused with
     * it. See LMRoman10-Regular (lmr10.pfb) for example.
     */
    if seek_operator(start, end, b"begin\x00" as *const u8 as *const i8) < 0i32 {
        return -1i32;
    }
    while *start < end && {
        key = get_next_key(start, end);
        !key.is_null()
    } {
        if streq_ptr(key, b"Encoding\x00" as *const u8 as *const i8) {
            if parse_encoding(enc_vec, start, end) < 0i32 {
                free(key as *mut libc::c_void);
                return -1i32;
            }
        } else if streq_ptr(key, b"FontName\x00" as *const u8 as *const i8) {
            let argn = parse_svalue(start, end, &mut strval);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if strlen(strval) > 127 {
                warn!(
                    "FontName too long: {} ({} bytes)",
                    CStr::from_ptr(strval).display(),
                    strlen(strval),
                );
                *strval.offset(127) = '\u{0}' as i32 as i8
            }
            cff_set_name(font, strval);
            free(strval as *mut libc::c_void);
        } else if streq_ptr(key, b"FontType\x00" as *const u8 as *const i8) {
            let argn = parse_nvalue(start, end, argv.as_mut_ptr(), 1i32);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if argv[0] != 1.0f64 {
                warn!("FontType {} not supported.", argv[0] as i32);
                free(key as *mut libc::c_void);
                return -1i32;
            }
        } else if streq_ptr(key, b"ItalicAngle\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"StrokeWidth\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"PaintType\x00" as *const u8 as *const i8) as i32 != 0
        {
            let argn = parse_nvalue(start, end, argv.as_mut_ptr(), 1i32);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if argv[0] != 0.0f64 {
                cff_dict_add(font.topdict, key, 1i32);
                cff_dict_set(font.topdict, key, 0i32, argv[0]);
            }
        } else if streq_ptr(key, b"UnderLinePosition\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"UnderLineThickness\x00" as *const u8 as *const i8) as i32 != 0
        {
            let argn = parse_nvalue(start, end, argv.as_mut_ptr(), 1i32);
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            cff_dict_add(font.topdict, key, 1i32);
            cff_dict_set(font.topdict, key, 0i32, argv[0]);
        } else if streq_ptr(key, b"FontBBox\x00" as *const u8 as *const i8) {
            let mut argn = parse_nvalue(start, end, argv.as_mut_ptr(), 4i32);
            if argn != 4i32 {
                warn!("{} values expected but only {} read.", 4i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            cff_dict_add(font.topdict, key, 4i32);
            loop {
                let fresh21 = argn;
                argn = argn - 1;
                if !(fresh21 > 0i32) {
                    break;
                }
                cff_dict_set(font.topdict, key, argn, argv[argn as usize]);
            }
        } else if streq_ptr(key, b"FontMatrix\x00" as *const u8 as *const i8) {
            let mut argn = parse_nvalue(start, end, argv.as_mut_ptr(), 6i32);
            if argn != 6i32 {
                warn!("{} values expected but only {} read.", 6i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if argv[0] != 0.001f64
                || argv[1] != 0.0f64
                || argv[2] != 0.0f64
                || argv[3] != 0.001f64
                || argv[4] != 0.0f64
                || argv[5] != 0.0f64
            {
                cff_dict_add(font.topdict, key, 6i32);
                loop {
                    let fresh22 = argn;
                    argn = argn - 1;
                    if !(fresh22 > 0i32) {
                        break;
                    }
                    cff_dict_set(font.topdict, key, argn, argv[argn as usize]);
                }
            }
        } else if streq_ptr(key, b"version\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"Notice\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"FullName\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"FamilyName\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"Weight\x00" as *const u8 as *const i8) as i32 != 0
            || streq_ptr(key, b"Copyright\x00" as *const u8 as *const i8) as i32 != 0
        {
            /*
             * FontInfo
             */
            let argn = parse_svalue(start, end, &mut strval); /* FIXME */
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            cff_dict_add(font.topdict, key, 1i32);
            let mut sid = cff_get_sid(font, strval) as s_SID;
            if sid as i32 == 65535i32 {
                sid = cff_add_string(font, strval, 0i32)
            }
            /*
             * We don't care about duplicate strings here since
             * later a subset font of this font will be generated.
             */
            cff_dict_set(font.topdict, key, 0i32, sid as f64); /* No Global Subr */
            free(strval as *mut libc::c_void);
        } else if streq_ptr(key, b"IsFixedPitch\x00" as *const u8 as *const i8) {
            let argn = parse_bvalue(start, end, &mut *argv.as_mut_ptr().offset(0));
            if argn != 1i32 {
                warn!("{} values expected but only {} read.", 1i32, argn,);
                free(key as *mut libc::c_void);
                return -1i32;
            }
            if argv[0] != 0.0f64 {
                cff_dict_add(*font.private.offset(0), key, 1i32);
                cff_dict_set(*font.private.offset(0), key, 0i32, 1i32 as f64);
            }
        }
        free(key as *mut libc::c_void);
    }
    0i32
}

pub unsafe fn is_pfb(handle: &mut InputHandleWrapper) -> bool {
    let mut sig: [u8; 14] = [0; 14];
    handle.seek(SeekFrom::Start(0)).unwrap();
    let mut ch = ttstub_input_getc(handle);
    if ch != 128i32
        || {
            ch = ttstub_input_getc(handle);
            ch < 0i32
        }
        || ch > 3i32
    {
        return false;
    }
    for _ in 0..4 {
        let ch = ttstub_input_getc(handle);
        if ch < 0i32 {
            return false;
        }
    }
    for i in 0..14 {
        let ch = ttstub_input_getc(handle);
        if ch < 0i32 {
            return false;
        }
        sig[i] = ch as u8;
    }
    if &sig[..] == b"%!PS-AdobeFont" || &sig[..11] == b"%!FontType1" {
        return true;
    }
    if &sig[..4] == b"%!PS" {
        warn!("Ambiguous PostScript resource type: {}", &sig[..].display(),);
        return true;
    }
    warn!("Not a PFB font file?");
    false
}
unsafe fn get_pfb_segment(
    handle: &mut InputHandleWrapper,
    expected_type: i32,
    length: *mut i32,
) -> *mut u8 {
    let mut buffer: *mut u8 = ptr::null_mut();
    let mut bytesread: i32 = 0i32;
    loop {
        let ch = ttstub_input_getc(handle);
        if ch < 0i32 {
            break;
        }
        if ch != 128i32 {
            panic!("Not a pfb file?");
        }
        let ch = ttstub_input_getc(handle);
        if ch < 0i32 || ch != expected_type {
            handle.seek(SeekFrom::Current(-2)).unwrap();
            break;
        } else {
            let mut slen = 0;
            for i in 0..4 {
                let ch = ttstub_input_getc(handle);
                if ch < 0i32 {
                    free(buffer as *mut libc::c_void);
                    return ptr::null_mut();
                }
                slen += ch << 8i32 * i;
            }
            buffer = renew(
                buffer as *mut libc::c_void,
                ((bytesread + slen) as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64)
                    as u32,
            ) as *mut u8;
            while slen > 0i32 {
                let rlen = ttstub_input_read(
                    handle.0.as_ptr(),
                    (buffer as *mut i8).offset(bytesread as isize),
                    slen as size_t,
                ) as i32;
                if rlen < 0i32 {
                    free(buffer as *mut libc::c_void);
                    return ptr::null_mut();
                }
                slen -= rlen;
                bytesread += rlen
            }
        }
    }
    if bytesread == 0i32 {
        panic!("PFB segment length zero?");
    }
    buffer = renew(
        buffer as *mut libc::c_void,
        ((bytesread + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32,
    ) as *mut u8;
    *buffer.offset(bytesread as isize) = 0_u8;
    if !length.is_null() {
        *length = bytesread
    }
    buffer
}

pub unsafe fn t1_get_standard_glyph(code: i32) -> *const i8 {
    if StandardEncoding[code as usize].is_null() {
        return std::ptr::null();
    }
    StandardEncoding[code as usize]
}

pub unsafe fn t1_get_fontname(handle: &mut InputHandleWrapper, fontname: *mut i8) -> i32 {
    let mut length: i32 = 0;
    let mut key: *mut i8 = ptr::null_mut();
    let mut fn_found: i32 = 0i32;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let buffer = get_pfb_segment(handle, 1i32, &mut length);
    if buffer.is_null() || length == 0i32 {
        panic!("Reading PFB (ASCII part) file failed.");
    }
    let mut start = buffer;
    let end = buffer.offset(length as isize);
    if seek_operator(&mut start, end, b"begin\x00" as *const u8 as *const i8) < 0i32 {
        free(buffer as *mut libc::c_void);
        return -1i32;
    }
    while fn_found == 0 && start < end && {
        key = get_next_key(&mut start, end);
        !key.is_null()
    } {
        if streq_ptr(key, b"FontName\x00" as *const u8 as *const i8) {
            let mut strval: *mut i8 = ptr::null_mut();
            if parse_svalue(&mut start, end, &mut strval) == 1i32 {
                if strlen(strval) > 127 {
                    warn!(
                        "FontName \"{}\" too long. ({} bytes)",
                        CStr::from_ptr(strval).display(),
                        strlen(strval),
                    );
                    *strval.offset(127) = '\u{0}' as i32 as i8
                }
                strcpy(fontname, strval);
                free(strval as *mut libc::c_void);
                fn_found = 1i32
            }
        }
        free(key as *mut libc::c_void);
    }
    free(buffer as *mut libc::c_void);
    0i32
}
unsafe fn init_cff_font(cff: &mut cff_font) {
    cff.handle = None;
    cff.filter = 0;
    cff.fontname = ptr::null_mut();
    cff.index = 0;
    cff.flag = 1 << 1;
    cff.header.major = 1 as u8;
    cff.header.minor = 0 as u8;
    cff.header.hdr_size = 4 as u8;
    cff.header.offsize = 4 as c_offsize;
    cff.name = cff_new_index(1);
    cff.topdict = cff_new_dict();
    cff.string = ptr::null_mut();
    cff.gsubr = cff_new_index(0);
    cff.encoding = ptr::null_mut();
    cff.charsets = ptr::null_mut();
    cff.fdselect = ptr::null_mut();
    cff.cstrings = ptr::null_mut();
    cff.fdarray = 0 as *mut *mut cff_dict;
    cff.private = new((1_u64).wrapping_mul(::std::mem::size_of::<*mut cff_dict>() as u64) as u32)
        as *mut *mut cff_dict;
    let ref mut fresh23 = *cff.private.offset(0);
    *fresh23 = cff_new_dict();
    cff.subrs = new((1_u64).wrapping_mul(::std::mem::size_of::<*mut cff_index>() as u64) as u32)
        as *mut *mut cff_index;
    let ref mut fresh24 = *cff.subrs.offset(0);
    *fresh24 = ptr::null_mut();
    cff.offset = 0 as l_offset;
    cff.gsubr_offset = 0 as l_offset;
    cff.num_glyphs = 0;
    cff.num_fds = 1;
    cff._string = cff_new_index(0);
}

pub unsafe fn t1_load_font<'a>(
    enc_vec: *mut *mut i8,
    mode: i32,
    mut handle: InputHandleWrapper,
) -> *mut cff_font<'a> {
    let mut length: i32 = 0;
    handle.seek(SeekFrom::Start(0)).unwrap();
    /* ASCII section */
    let buffer = get_pfb_segment(&mut handle, 1i32, &mut length);
    if buffer.is_null() || length == 0i32 {
        panic!("Reading PFB (ASCII part) file failed.");
    }
    let cff =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cff_font>() as u64) as u32) as *mut cff_font;
    init_cff_font(&mut *cff);
    let mut start = buffer;
    let end = buffer.offset(length as isize);
    if parse_part1(&mut *cff, enc_vec, &mut start, end) < 0i32 {
        cff_close(cff);
        free(buffer as *mut libc::c_void);
        panic!("Reading PFB (ASCII part) file failed.");
    }
    free(buffer as *mut libc::c_void);
    /* Binary section */
    let buffer = get_pfb_segment(&mut handle, 2i32, &mut length);
    if buffer.is_null() || length == 0i32 {
        cff_close(cff);
        free(buffer as *mut libc::c_void);
        panic!("Reading PFB (BINARY part) file failed.");
    } else {
        t1_decrypt(55665_u16, buffer, buffer, 0i32, length);
    }
    let mut start = buffer.offset(4);
    let end = buffer.offset(length as isize);
    if parse_part2(&mut *cff, &mut start, end, mode) < 0i32 {
        cff_close(cff);
        free(buffer as *mut libc::c_void);
        panic!("Reading PFB (BINARY part) file failed.");
    }
    /* Remaining section ignored. */
    free(buffer as *mut libc::c_void);
    cff_update_string(&mut *cff);
    bridge::ttstub_input_close(handle);
    cff
}
