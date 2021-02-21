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
use std::ffi::CString;
use std::ptr;

use super::dpx_dpxutil::xtoi;
use super::dpx_mem::new;
use crate::dpx_pdfobj::{
    pdf_dict, pdf_file, pdf_indirect, pdf_name, pdf_new_null, pdf_obj, pdf_stream, pdf_string,
    DerefObj, IntoObj, Object, STREAM_COMPRESS,
};
use crate::specials::spc_lookup_reference;
use libc::memcpy;

fn is_space(c: &u8) -> bool {
    c.is_ascii_whitespace() || *c == 0
}
fn is_delim(c: &u8) -> bool {
    b"()/<>[]%".contains(c)
}

fn istokensep(c: &u8) -> bool {
    is_space(c) || is_delim(c)
}

/* pow() */
/* PDF */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct ParserState {
    pub(crate) tainted: i32,
}
static mut parser_state: ParserState = ParserState { tainted: 0 };

pub(crate) fn dump(buf: &[u8]) {
    info!("\nCurrent input buffer is -->");
    if buf.len() > 50 {
        info!("{}...", buf[..50].display());
    } else {
        info!("{}", buf.display());
    }
    info!("<--\n");
}

pub(crate) unsafe fn pdfparse_skip_line(start: *mut *const i8, end: *const i8) {
    while *start < end && **start as i32 != '\n' as i32 && **start as i32 != '\r' as i32 {
        *start = (*start).offset(1)
    }
    /* The carriage return (CR; \r; 0x0D) and line feed (LF; \n; 0x0A)
     * characters, also called newline characters, are treated as
     * end-of-line (EOL) markers. The combination of a carriage return
     * followed immediately by a line feed is treated as one EOL marker.
     */
    if *start < end && **start as i32 == '\r' as i32 {
        *start = (*start).offset(1)
    }
    if *start < end && **start as i32 == '\n' as i32 {
        *start = (*start).offset(1)
    };
}

pub(crate) unsafe fn skip_white(start: *mut *const i8, end: *const i8) {
    /*
     * The null (NUL; 0x00) character is a white-space character in PDF spec
     * but isspace(0x00) returns FALSE; on the other hand, the vertical tab
     * (VT; 0x0B) character is not a white-space character in PDF spec but
     * isspace(0x0B) returns TRUE.
     */
    while *start < end
        && ((**start as u8).is_ascii_whitespace()
            || **start as i32 == '\u{0}' as i32
            || **start as i32 == '%' as i32)
    {
        if **start as i32 == '%' as i32 {
            pdfparse_skip_line(start, end);
        } else {
            *start = (*start).offset(1)
        }
    }
}
pub(crate) trait SkipWhite {
    fn skip_white(&mut self);
    fn skip_line(&mut self);
}
impl SkipWhite for &[u8] {
    fn skip_white(&mut self) {
        /*
         * The null (NUL; 0x00) character is a white-space character in PDF spec
         * but isspace(0x00) returns FALSE; on the other hand, the vertical tab
         * (VT; 0x0B) character is not a white-space character in PDF spec but
         * isspace(0x0B) returns TRUE.
         */
        while !self.is_empty() && (is_space(&self[0]) || self[0] == b'%') {
            if self[0] == b'%' {
                self.skip_line();
            } else {
                *self = &self[1..]
            }
        }
    }
    fn skip_line(&mut self) {
        while !self.is_empty() && self[0] != b'\n' && self[0] != b'\r' {
            *self = &self[1..]
        }
        /* The carriage return (CR; \r; 0x0D) and line feed (LF; \n; 0x0A)
         * characters, also called newline characters, are treated as
         * end-of-line (EOL) markers. The combination of a carriage return
         * followed immediately by a line feed is treated as one EOL marker.
         */
        if !self.is_empty() && self[0] == b'\r' {
            *self = &self[1..]
        }
        if !self.is_empty() && self[0] == b'\n' {
            *self = &self[1..]
        };
    }
}
unsafe fn parsed_string(start: *const i8, end: *const i8) -> *mut i8 {
    let mut result: *mut i8 = ptr::null_mut();
    let len = end.offset_from(start) as i64 as i32;
    if len > 0 {
        result =
            new(((len + 1) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
                as *mut i8;
        memcpy(
            result as *mut libc::c_void,
            start as *const libc::c_void,
            len as _,
        );
        *result.offset(len as isize) = '\u{0}' as i32 as i8
    }
    result
}
fn parsed_string_slice(buf: &[u8]) -> Option<CString> {
    if !buf.is_empty() {
        Some(CString::new(buf).unwrap())
    } else {
        None
    }
}
fn parsed_string_slice_string(buf: &[u8]) -> Option<String> {
    if !buf.is_empty() {
        Some(std::str::from_utf8(buf).unwrap().to_string())
    } else {
        None
    }
}

pub(crate) unsafe fn parse_number(start: *mut *const i8, end: *const i8) -> *mut i8 {
    skip_white(start, end);
    let mut p = *start;
    if p < end && (*p as i32 == '+' as i32 || *p as i32 == '-' as i32) {
        p = p.offset(1)
    }
    while p < end && (*p as u8).is_ascii_digit() {
        p = p.offset(1)
    }
    if p < end && *p as i32 == '.' as i32 {
        p = p.offset(1);
        while p < end && (*p as u8).is_ascii_digit() {
            p = p.offset(1)
        }
    }
    let number = parsed_string(*start, p);
    *start = p;
    number
}

pub(crate) unsafe fn parse_unsigned(start: *mut *const i8, end: *const i8) -> *mut i8 {
    skip_white(start, end);
    let mut p = *start;
    while p < end {
        if !(*p as u8).is_ascii_digit() {
            break;
        }
        p = p.offset(1)
    }
    let number = parsed_string(*start, p);
    *start = p;
    number
}

pub(crate) trait ParseNumber {
    fn parse_number(&mut self) -> Option<CString>;
    fn parse_unsigned(&mut self) -> Option<CString>;
}

impl ParseNumber for &[u8] {
    fn parse_number(&mut self) -> Option<CString> {
        self.skip_white();
        let mut i = 0;
        if !self.is_empty() && (self[0] == b'+' || self[0] == b'-') {
            i += 1;
        }
        while !self[i..].is_empty() && self[0].is_ascii_digit() {
            i += 1;
        }
        if !self[i..].is_empty() && self[0] == b'.' {
            i += 1;
            while !self[i..].is_empty() && self[0].is_ascii_digit() {
                i += 1;
            }
        }
        let number = parsed_string_slice(&self[..i]);
        *self = &self[i..];
        number
    }

    fn parse_unsigned(&mut self) -> Option<CString> {
        self.skip_white();
        let mut i = 0;
        for p in *self {
            if !p.is_ascii_digit() {
                break;
            }
            i += 1;
        }
        let number = parsed_string_slice(&self[..i]);
        *self = &self[i..];
        number
    }
}

unsafe fn parse_gen_ident(start: *mut *const i8, end: *const i8, valid_chars: &[u8]) -> *mut i8 {
    /* No skip_white(start, end)? */
    let mut p = *start;
    while p < end {
        if !valid_chars.contains(&(*p as u8)) {
            break;
        }
        p = p.offset(1)
    }
    let ident = parsed_string(*start, p);
    *start = p;
    ident
}
fn parse_gen_ident_slice(buf: &mut &[u8], valid_chars: &[u8]) -> Option<String> {
    /* No skip_white(start, end)? */
    let mut i = 0;
    for p in *buf {
        if !valid_chars.contains(p) {
            break;
        }
        i += 1;
    }
    let ident = parsed_string_slice_string(&buf[..i]);
    *buf = &buf[i..];
    ident
}

pub(crate) unsafe fn parse_ident(start: *mut *const i8, end: *const i8) -> *mut i8 {
    const VALID_CHARS: &[u8] =
        b"!\"#$&\'*+,-.0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz|~";
    parse_gen_ident(start, end, VALID_CHARS)
}

pub(crate) trait ParseIdent {
    fn parse_ident(&mut self) -> Option<String>;
    fn parse_val_ident(&mut self) -> Option<String>;
    fn parse_opt_ident(&mut self) -> Option<String>;
}

impl ParseIdent for &[u8] {
    fn parse_ident(&mut self) -> Option<String> {
        const VALID_CHARS: &[u8] =
            b"!\"#$&\'*+,-.0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz|~";
        parse_gen_ident_slice(self, VALID_CHARS)
    }
    fn parse_val_ident(&mut self) -> Option<String> {
        const VALID_CHARS: &[u8] =
            b"!\"#$&\'*+,-./0123456789:;?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz|~";
        parse_gen_ident_slice(self, VALID_CHARS)
    }
    fn parse_opt_ident(&mut self) -> Option<String> {
        if !self.is_empty() && self[0] == b'@' {
            *self = &self[1..];
            self.parse_ident()
        } else {
            None
        }
    }
}

//#ifndef PDF_NAME_LEN_MAX
const PDF_NAME_LEN_MAX: usize = 128;

//#ifndef PDF_STRING_LEN_MAX
const PDF_STRING_LEN_MAX: usize = 65535;

const STRING_BUFFER_SIZE: usize = PDF_STRING_LEN_MAX + 1;
static mut sbuf: [u8; PDF_STRING_LEN_MAX + 1] = [0; PDF_STRING_LEN_MAX + 1];

/* !PDF_PARSE_STRICT */
unsafe fn try_pdf_reference(mut p: &[u8], pf: *mut pdf_file) -> Option<(*mut pdf_obj, &[u8])> {
    let mut id: u32 = 0_u32;
    let mut gen: u16 = 0_u16;
    assert!(!pf.is_null());
    p.skip_white();
    if p.len() < 5 || !p[0].is_ascii_digit() {
        return None;
    }
    while !is_space(&p[0]) {
        if p.is_empty() || !p[0].is_ascii_digit() {
            return None;
        }
        id = id * 10 + (p[0] as u32 - '0' as u32);
        p = &p[1..];
    }
    p.skip_white();
    if p.is_empty() || !p[0].is_ascii_digit() {
        return None;
    }
    while !is_space(&(p[0])) {
        if p.is_empty() || !p[0].is_ascii_digit() {
            return None;
        }
        gen = (gen as i32 * 10 + (p[0] as i32 - '0' as i32)) as u16;
        p = &p[1..];
    }
    p.skip_white();
    if p.is_empty() || p[0] != b'R' {
        return None;
    }
    p = &p[1..];
    if !(p.is_empty() || is_space(&(p[0])) || is_delim(&(p[0]))) {
        return None;
    }
    Some((pdf_indirect::new(pf, (id, gen)).into_obj(), p))
}

pub(crate) trait ParsePdfObj {
    fn parse_pdf_object(&mut self, pf: *mut pdf_file) -> Option<*mut pdf_obj>;
    fn parse_pdf_reference(&mut self) -> Option<*mut pdf_obj>;
    fn parse_pdf_stream(&mut self, dict: &mut pdf_dict) -> Option<pdf_stream>;
    fn parse_pdf_array(&mut self, pf: *mut pdf_file) -> Option<Vec<*mut pdf_obj>>;
    fn parse_pdf_tainted_dict(&mut self) -> Option<pdf_dict>;
    fn parse_pdf_dict(&mut self, pf: *mut pdf_file) -> Option<pdf_dict>;
    fn parse_pdf_literal_string(&mut self) -> Option<pdf_string>;
    fn parse_pdf_hex_string(&mut self) -> Option<pdf_string>;
    fn parse_pdf_string(&mut self) -> Option<pdf_string>;
    fn parse_pdf_null(&mut self) -> Option<()>;
    fn parse_pdf_boolean(&mut self) -> Option<bool>;
    fn parse_pdf_name(&mut self) -> Option<pdf_name>;
    fn parse_pdf_number(&mut self) -> Option<f64>;
}

impl ParsePdfObj for &[u8] {
    fn parse_pdf_object(&mut self, pf: *mut pdf_file) -> Option<*mut pdf_obj> {
        self.skip_white();
        if self.is_empty() {
            warn!("Could not find any valid object.");
            return None;
        }
        match self[0] {
            b'<' => {
                if self[1] != b'<' {
                    self.parse_pdf_hex_string().map(IntoObj::into_obj)
                } else {
                    let dict = self.parse_pdf_dict(pf);
                    self.skip_white();
                    if dict.is_some() && self.len() >= 15 && self.starts_with(b"stream") {
                        let mut dict = dict.unwrap();
                        self.parse_pdf_stream(&mut dict).map(IntoObj::into_obj)
                    } else {
                        dict.map(IntoObj::into_obj)
                    }
                }
            }
            b'(' => self.parse_pdf_string().map(IntoObj::into_obj),
            b'[' => self.parse_pdf_array(pf).map(IntoObj::into_obj),
            b'/' => self.parse_pdf_name().map(IntoObj::into_obj),
            b'n' => self.parse_pdf_null().map(|_| pdf_new_null()),
            b't' | b'f' => self.parse_pdf_boolean().map(IntoObj::into_obj),
            b'+' | b'-' | b'.' => self.parse_pdf_number().map(IntoObj::into_obj),
            b'0'..=b'9' => {
                /*
                 * If pf != NULL, then we are parsing a PDF file,
                 * and indirect references are allowed.
                 */
                if !pf.is_null() {
                    if let Some((res, next)) = unsafe { try_pdf_reference(*self, pf) } {
                        *self = next;
                        Some(res)
                    } else {
                        self.parse_pdf_number().map(IntoObj::into_obj)
                    }
                } else {
                    self.parse_pdf_number().map(IntoObj::into_obj)
                }
            }
            b'@' => self.parse_pdf_reference(),
            _ => {
                warn!("Unknown PDF object type.");
                None
            }
        }
    }
    fn parse_pdf_reference(&mut self) -> Option<*mut pdf_obj> {
        let result;
        let save2 = *self; // TODO: check
        self.skip_white();
        if let Some(name) = self.parse_opt_ident() {
            result = unsafe { spc_lookup_reference(&name) };
            if result.is_none() {
                // DEAD code
                warn!("Could not find the named reference (@{}).", name);
                dump(save2);
                *self = save2
            }
        } else {
            warn!("Could not find a reference name.");
            dump(save2);
            *self = save2;
            result = None;
        }
        result
    }
    fn parse_pdf_stream(&mut self, dict: &mut pdf_dict) -> Option<pdf_stream> {
        let stream_length;
        let mut p = *self;
        p.skip_white();
        if !p.starts_with(b"stream") {
            return None;
        }
        p = &p[6..];
        /* The keyword stream that follows the stream dictionary
         * should be followed by an end-of-line marker consisting of
         * either a carriage return (0x0D;\r) and a line feed (0x0A;\n)
         * or just a line feed, and not by a carriage return alone.
         * [PDF Reference, 6th ed., version 1.7, self. 60-61] */
        /* Notice that TeX translates an end-of-line marker to a single space. */
        if !p.is_empty() && p[0] == b'\n' {
            p = &p[1..];
        } else if p.len() > 1 && (p[0] == b'\r' && p[1] == b'\n') {
            p = &p[2..];
        }
        /* Stream length */
        if let Some(tmp) = unsafe { dict.get_mut("Length") } {
            unsafe {
                stream_length = if let Some(tmp2) = DerefObj::new(Some(tmp)) {
                    let l = if let Object::Number(v) = tmp2.data {
                        v as i32
                    } else {
                        -1
                    };
                    l
                } else {
                    -1
                }
            }
        } else {
            return None;
        }
        if stream_length < 0 || p.len() < stream_length as usize {
            return None;
        }
        let stream_length = stream_length as usize;
        /*
         * If Filter is not aselflied, set STREAM_COMPRESS flag.
         * Should we use filter for ASCIIHexEncode/ASCII85Encode-ed streams?
         */
        let mut stream = if !dict.has("Filter") && stream_length > 10 {
            pdf_stream::new(STREAM_COMPRESS)
        } else {
            pdf_stream::new(0)
        };
        let stream_dict = stream.get_dict_mut();
        unsafe {
            stream_dict.merge(dict);
        }
        stream.add_slice(&p[..stream_length]);
        p = &p[stream_length..];
        /* Check "endsteam" */
        /* It is recommended that there be an end-of-line marker
         * after the data and before endstream; this marker is not included
         * in the stream length.
         * [PDF Reference, 6th ed., version 1.7, self. 61] */
        if !p.is_empty() && p[0] == b'\r' {
            p = &p[1..];
        }
        if !p.is_empty() && p[0] == b'\n' {
            p = &p[1..];
        }
        if !p.starts_with(b"endstream") {
            return None;
        }
        p = &p[9..];
        *self = p;
        Some(stream)
    }
    fn parse_pdf_array(&mut self, pf: *mut pdf_file) -> Option<Vec<*mut pdf_obj>> {
        let mut p = *self;
        p.skip_white();
        if p.len() < 2 || p[0] != b'[' {
            warn!("Could not find an array object.");
            return None;
        }
        let mut result = vec![];
        p = &p[1..];
        p.skip_white();
        while !p.is_empty() && p[0] != b']' {
            if let Some(elem) = p.parse_pdf_object(pf) {
                result.push(elem);
                p.skip_white();
            } else {
                warn!("Could not find a valid object in array object.");
                return None;
            }
        }
        if p.is_empty() || p[0] != b']' {
            warn!("Array object ended prematurely.");
            return None;
        }
        *self = &p[1..];
        Some(result)
    }
    fn parse_pdf_tainted_dict(&mut self) -> Option<pdf_dict> {
        unsafe {
            parser_state.tainted = 1;
        }
        let result = self.parse_pdf_dict(ptr::null_mut());
        unsafe {
            parser_state.tainted = 0;
        }
        result
    }
    fn parse_pdf_dict(&mut self, pf: *mut pdf_file) -> Option<pdf_dict> {
        let mut p = *self;
        p.skip_white();
        /* At least four letter <<>>. */
        if p.len() < 4 || p[0] != b'<' || p[1] != b'<' {
            return None;
        } /* skip >> */
        p = &p[2..]; /* skip ] */
        let mut result = pdf_dict::new();
        p.skip_white();
        while !p.is_empty() && p[0] != b'>' {
            p.skip_white();
            if let Some(key) = p.parse_pdf_name() {
                p.skip_white();
                if let Some(value) = p.parse_pdf_object(pf) {
                    unsafe {
                        result.set(key.to_bytes(), value);
                    }
                    p.skip_white();
                } else {
                    warn!("Could not find a value in dictionary object.");
                    return None;
                }
            } else {
                warn!("Could not find a key in dictionary object.");
                return None;
            }
        }
        if p.len() < 2 || p[0] != b'>' || p[1] != b'>' {
            warn!("Syntax error: Dictionary object ended prematurely.");
            return None;
        }
        *self = &p[2..];
        Some(result)
    }
    fn parse_pdf_literal_string(&mut self) -> Option<pdf_string> {
        /*
         * PDF Literal String
         */
        fn ps_getescc(pp: &mut &[u8]) -> i32 {
            let mut ch; /* backslash assumed. */
            let mut p = &(*pp)[1..];
            match p[0] {
                b'n' => {
                    ch = '\n' as i32;
                    p = &p[1..];
                }
                b'r' => {
                    ch = '\r' as i32;
                    p = &p[1..];
                }
                b't' => {
                    ch = '\t' as i32;
                    p = &p[1..];
                }
                b'b' => {
                    ch = '\u{8}' as i32;
                    p = &p[1..];
                }
                b'f' => {
                    ch = '\u{c}' as i32;
                    p = &p[1..];
                }
                /*
                 * An end-of-line marker preceded by a backslash must be ignored.
                 */
                b'\n' => {
                    ch = -1;
                    p = &p[1..];
                }
                b'\r' => {
                    ch = -1;
                    p = &p[1..];
                    if !p.is_empty() && p[0] == b'\n' {
                        p = &p[1..];
                    }
                }
                _ => {
                    if p[0] == b'\\' || p[0] == b'(' || p[0] == b')' {
                        ch = p[0] as i32;
                        p = &p[1..];
                    } else if p[0] >= b'0' && p[0] <= b'7' {
                        ch = 0;
                        /* Ignore overflow. */
                        let mut i = 0;
                        while i < 3 && !p.is_empty() && (p[0] >= b'0' && p[0] <= b'7') {
                            ch = (ch << 3) + (p[0] as i32 - '0' as i32);
                            p = &p[1..];
                            i += 1;
                        }
                        ch = ch & 0xff
                    } else {
                        /* Don't forget isodigit() is a macro. */
                        ch = p[0] as i32; /* Ignore only backslash. */
                        p = &p[1..];
                    }
                }
            }
            *pp = p;
            ch
        }
        let mut op_count: i32 = 0;
        let mut len = 0;
        let mut p = *self;
        p.skip_white();
        if p.is_empty() || p[0] != b'(' {
            return None;
        }
        p = &p[1..];
        /* The carriage return (CR, 0x0d) and line feed (LF, 0x0a) characters,
         * also called newline characters, are treated as end-of-line (EOL)
         * markers. The combination of a carriage return followed immediately
         * by a line feed is treated as one EOL marker.
         * [PDF Reference, 6th ed., version 1.7, p. 50] */
        /* If an end-of-line marker aselfears within a literal string
         * without a preceding backslash, the result is equivalent to
         * \n (regardless of whether the end-of-line marker was
         * a carriage return, a line feed, or both).
         * [PDF Reference, 6th ed., version 1.7, p. 55] */
        while !p.is_empty() {
            let mut ch = p[0] as i32;
            if ch == ')' as i32 && op_count < 1 {
                break;
            }
            if unsafe { parser_state.tainted != 0 } {
                if p.len() > 1 && ch & 0x80 != 0 {
                    if len + 2 >= 65535 {
                        warn!("PDF string length too long. (limit: {})", 65535);
                        return None;
                    }
                    unsafe {
                        sbuf[len] = p[0] as u8;
                    }
                    len += 1;
                    unsafe {
                        sbuf[len] = p[1] as u8;
                    }
                    len += 1;
                    p = &p[2..];
                    continue;
                }
            }
            /* !PDF_PARSE_STRICT */
            if len + 1 >= 65535 {
                warn!("PDF string length too long. (limit: {})", 65535);
                return None;
            }
            match ch as u8 {
                b'\\' => {
                    ch = ps_getescc(&mut p);
                    if ch >= 0 {
                        unsafe {
                            sbuf[len] = (ch & 0xff) as u8;
                        }
                        len += 1;
                    }
                }
                b'\r' => {
                    p = &p[1..];
                    if !p.is_empty() && p[0] == b'\n' {
                        p = &p[1..];
                    }
                    unsafe {
                        sbuf[len] = b'\n';
                    }
                    len += 1;
                }
                _ => {
                    if ch == '(' as i32 {
                        op_count += 1
                    } else if ch == ')' as i32 {
                        op_count -= 1
                    }
                    unsafe {
                        sbuf[len] = ch as u8;
                    }
                    len += 1;
                    p = &p[1..]
                }
            }
        }
        if op_count > 0 || p.is_empty() || p[0] != b')' {
            warn!("Unbalanced parens/truncated PDF literal string.");
            return None;
        }
        *self = &p[1..];
        unsafe { Some(pdf_string::new(&sbuf[..len])) }
    }

    /*
     * PDF Hex String
     */
    fn parse_pdf_hex_string(&mut self) -> Option<pdf_string> {
        let mut p = *self;
        p.skip_white();
        if p.is_empty() || p[0] != b'<' {
            return None;
        }
        p = &p[1..];
        let mut len = 0;
        /*
         * PDF Reference does not describe how to treat invalid char.
         * Zero is aselfended if final hex digit is missing.
         */
        while !p.is_empty() && p[0] != b'>' && len < 65535 {
            p.skip_white();
            if p.is_empty() || p[0] == b'>' {
                break;
            }
            let mut ch = xtoi(p[0]) << 4;
            p = &p[1..];
            p.skip_white();
            if !p.is_empty() && p[0] != b'>' {
                ch += xtoi(p[0]);
                p = &p[1..]
            }
            unsafe {
                sbuf[len] = (ch & 0xff) as u8;
            }
            len += 1;
        }
        if p.is_empty() {
            warn!("Premature end of input hex string.");
            return None;
        } else {
            if p[0] != b'>' {
                warn!("PDF string length too long. (limit: {})", 65535);
                return None;
            }
        }
        *self = &p[1..];
        unsafe { Some(pdf_string::new(&sbuf[..len])) }
    }
    fn parse_pdf_string(&mut self) -> Option<pdf_string> {
        self.skip_white();
        if self.len() >= 2 {
            if self[0] == b'(' {
                return unsafe { self.parse_pdf_literal_string() };
            } else {
                if self[0] == b'<' && (self[1] == b'>' || self[1].is_ascii_hexdigit()) {
                    return unsafe { self.parse_pdf_hex_string() };
                }
            }
        }
        warn!("Could not find a string object.");
        None
    }
    fn parse_pdf_null(&mut self) -> Option<()> {
        self.skip_white();
        if (*self).len() < 4 {
            warn!("Not a null object.");
            return None;
        } else if (*self).len() > 4 && !istokensep(&self[4]) {
            warn!("Not a null object.");
            return None;
        } else if self.starts_with(b"null") {
            *self = &(*self)[4..];
            return Some(());
        } else {
            warn!("Not a null object.");
            None
        }
    }
    fn parse_pdf_boolean(&mut self) -> Option<bool> {
        self.skip_white();
        if self.starts_with(b"true") {
            if (*self).len() == 4 || istokensep(&self[4]) {
                *self = &(*self)[4..];
                return Some(true);
            }
        } else if self.starts_with(b"false") {
            if (*self).len() == 5 || istokensep(&self[5]) {
                *self = &(*self)[5..];
                return Some(false);
            }
        }
        warn!("Not a boolean object.");
        None
    }
    fn parse_pdf_name(&mut self) -> Option<pdf_name> {
        unsafe fn pn_getc(pp: &mut &[u8]) -> i32 {
            let mut ch;
            let p = *pp;
            if p[0] == b'#' {
                if p.len() <= 2 {
                    *pp = &[];
                    return -1;
                }
                if !p[1].is_ascii_hexdigit() || !p[2].is_ascii_hexdigit() {
                    *pp = &(*pp)[3..];
                    return -1;
                }
                ch = xtoi(p[1]) << 4;
                ch += xtoi(p[2]);
                *pp = &(*pp)[3..];
            } else {
                ch = p[0] as i8 as i32; // TODO: more tests
                *pp = &(*pp)[1..];
            }
            ch
        }
        let mut name = Vec::<u8>::with_capacity(PDF_NAME_LEN_MAX);
        let mut len = 0;
        self.skip_white();
        if self.is_empty() || self[0] != b'/' {
            warn!("Could not find a name object.");
            return None;
        }
        *self = &(*self)[1..];
        while !self.is_empty() && !istokensep(&self[0]) {
            let ch = unsafe { pn_getc(self) };
            if ch < 0 || ch > 0xff {
                warn!("Invalid char in PDF name object. (ignored)");
            } else if ch == 0 {
                warn!("Null char not allowed in PDF name object. (ignored)");
            } else if len < STRING_BUFFER_SIZE {
                if len == PDF_NAME_LEN_MAX {
                    warn!("PDF name length too long. (>= {} bytes)", PDF_NAME_LEN_MAX);
                }
                name.push(ch as u8);
                len += 1;
            } else {
                warn!(
                    "PDF name length too long. (>= {} bytes, truncated)",
                    STRING_BUFFER_SIZE
                );
            }
        }
        if len < 1 {
            warn!("No valid name object found.");
            return None;
        }
        Some(pdf_name::new(name))
    }
    fn parse_pdf_number(&mut self) -> Option<f64> {
        let mut v = 0_f64;
        let mut nddigits = 0;
        let mut sign = 1;
        let mut has_dot = false;
        let mut p = *self;
        p.skip_white();
        if p.is_empty() || !p[0].is_ascii_digit() && !(b".+-".contains(&p[0])) {
            warn!("Could not find a numeric object.");
            return None;
        }
        if p[0] == b'-' {
            if p.len() <= 1 {
                warn!("Could not find a numeric object.");
                return None;
            }
            sign = -1;
            p = &p[1..];
        } else if p[0] as i32 == '+' as i32 {
            if p.len() <= 1 {
                warn!("Could not find a numeric object.");
                return None;
            }
            sign = 1;
            p = &p[1..];
        }
        while !p.is_empty() && !(is_space(&p[0]) | is_delim(&p[0])) {
            if p[0] == b'.' {
                if has_dot {
                    /* Two dots */
                    warn!("Could not find a numeric object.");
                    return None;
                } else {
                    has_dot = true
                }
            } else if p[0].is_ascii_digit() {
                if has_dot {
                    v += (p[0] - b'0') as f64 / (10_f64).powf((nddigits + 1) as f64);
                    nddigits += 1
                } else {
                    v = v * 10. + p[0] as f64 - b'0' as f64
                }
            } else {
                warn!("Could not find a numeric object.");
                return None;
            }
            p = &p[1..];
        }
        *self = p;
        Some(sign as f64 * v)
    }
}
