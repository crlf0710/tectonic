#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use bridge::DisplayExt;
use std::ffi::{CStr, CString};

use crate::strstartswith;
use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::xetex_consts::{Glyph, NativeWord, Side, UnicodeMode};
use crate::xetex_xetexd::{print_c_str, print_c_string};
use bridge::{ttstub_input_close, ttstub_input_get_size, ttstub_input_open, ttstub_input_read};
use libc::free;
use std::ptr;

#[cfg(target_os = "macos")]
use super::xetex_aatfont as aat;
#[cfg(target_os = "macos")]
use crate::cf_prelude::{
    kCFNumberFloatType, kCTFontAttributeName, kCTForegroundColorAttributeName,
    kCTVerticalFormsAttributeName, CFDictionaryGetValue, CFDictionaryRef, CFNumberGetValue,
    CFNumberRef, CFNumberType, CFRelease, CFTypeRef, CGAffineTransform, CGColorGetComponents,
    CGColorRef, CGFloat, CTFontGetMatrix, CTFontGetSize, CTFontRef,
};
use crate::core_memory::{xcalloc, xmalloc, xrealloc};
use crate::xetex_ini::{
    loaded_font_design_size, loaded_font_flags, loaded_font_letter_space, loaded_font_mapping,
    mapped_text, name_of_file, native_font_type_flag, xdv_buffer, DEPTH_BASE, FONT_AREA,
    FONT_FLAGS, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_LETTER_SPACE, HEIGHT_BASE, PARAM_BASE,
};
use crate::xetex_output::{print_char, print_int, print_nl, print_raw_char};
use crate::xetex_scaledmath::xn_over_d;
use crate::xetex_texmfmp::{gettexstring, maketexstring, to_rust_string};
use crate::xetex_xetex0::{
    begin_diagnostic, end_diagnostic, font_feature_warning, font_mapping_warning,
    get_tracing_fonts_state,
};

use crate::xetex_layout_interface::*;
use harfbuzz_sys::{hb_feature_t, hb_tag_from_string, hb_tag_t};
use libc::{memcpy, strcat, strdup, strlen, strncpy};

pub(crate) const AAT_FONT_FLAG: u32 = 0xFFFF;
pub(crate) const OTGR_FONT_FLAG: u32 = 0xFFFE;

pub(crate) type size_t = usize;
pub(crate) type ssize_t = isize;

use bridge::TTInputFormat;

pub(crate) type Boolean = libc::c_uchar;

pub(crate) type str_number = i32;

pub(crate) type UTF16_code = u16;

/* 16.16 version number */

pub(crate) type UniChar = u16;

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew

 SIL Author(s): Jonathan Kew

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the copyright holders
shall not be used in advertising or otherwise to promote the sale,
use or other dealings in this Software without prior written
authorization from the copyright holders.
\****************************************************************************/

#[inline]
unsafe fn SWAP16(p: u16) -> u16 {
    ((p as i32 >> 8i32) + ((p as i32) << 8i32)) as u16
}

#[inline]
unsafe fn SWAP32(p: u32) -> u32 {
    (p >> 24i32)
        .wrapping_add(p >> 8i32 & 0xff00_u32)
        .wrapping_add(p << 8i32 & 0xff0000_u32)
        .wrapping_add(p << 24i32)
}

/* xetex-shipout */
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009, 2011 by Jonathan Kew
 Copyright (c) 2012-2015 by Khaled Hosny
 Copyright (c) 2012, 2013 by Jiang Jiang

 SIL Author(s): Jonathan Kew

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the copyright holders
shall not be used in advertising or otherwise to promote the sale,
use or other dealings in this Software without prior written
authorization from the copyright holders.
\****************************************************************************/
/* XeTeX_ext.c
 * additional plain C extensions for XeTeX - mostly platform-neutral
 */
/* for fabs() */

/* OT-related constants we need */
static mut brkIter: *mut icu::UBreakIterator = 0 as *mut icu::UBreakIterator;
static mut brkLocaleStrNum: i32 = 0i32;

/* info for each glyph is location (FixedPoint) + glyph ID (u16) */
/* glyph ID field in a glyph_node */
/* For Unicode encoding form interpretation... */
pub(crate) unsafe fn linebreak_start(
    f: usize,
    mut localeStrNum: i32,
    mut text: *mut u16,
    mut textLength: i32,
) {
    let mut status: icu::UErrorCode = icu::U_ZERO_ERROR;
    let mut locale = gettexstring(localeStrNum);
    if FONT_AREA[f] as u32 == 0xfffeu32 && locale == "G" {
        let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine;
        if initGraphiteBreaking(engine, text, textLength) {
            /* user asked for Graphite line breaking and the font supports it */
            return;
        }
    }
    if localeStrNum != brkLocaleStrNum && !brkIter.is_null() {
        icu::ubrk_close(brkIter);
        brkIter = 0 as *mut icu::UBreakIterator
    }
    if brkIter.is_null() {
        brkIter = icu::ubrk_open(
            icu::UBRK_LINE,
            CString::new(locale.as_str()).unwrap().as_ptr(),
            ptr::null(),
            0i32,
            &mut status,
        );
        if status as i32 > icu::U_ZERO_ERROR as i32 {
            begin_diagnostic();
            print_nl('E' as i32);
            print_c_string(b"rror \x00" as *const u8 as *const i8);
            print_int(status as i32);
            print_c_string(
                b" creating linebreak iterator for locale `\x00" as *const u8 as *const i8,
            );
            print_c_str(&locale);
            print_c_string(b"\'; trying default locale `en_us\'.\x00" as *const u8 as *const i8);
            end_diagnostic(true);
            if !brkIter.is_null() {
                icu::ubrk_close(brkIter);
            }
            status = icu::U_ZERO_ERROR;
            brkIter = icu::ubrk_open(
                icu::UBRK_LINE,
                b"en_us\x00" as *const u8 as *const i8,
                ptr::null(),
                0i32,
                &mut status,
            )
        }
        brkLocaleStrNum = localeStrNum
    }
    if brkIter.is_null() {
        panic!(
            "failed to create linebreak iterator, status={}",
            status as i32
        );
    }
    icu::ubrk_setText(brkIter, text as *mut icu::UChar, textLength, &mut status);
}

pub(crate) unsafe fn linebreak_next() -> i32 {
    if !brkIter.is_null() {
        icu::ubrk_next(brkIter)
    } else {
        findNextGraphiteBreak()
    }
}

pub(crate) unsafe fn get_encoding_mode_and_info(mut info: *mut i32) -> UnicodeMode {
    /* \XeTeXinputencoding "enc-name"
     *   -> name is packed in |nameoffile| as a C string, starting at [1]
     * Check if it's a built-in name; if not, try to open an ICU converter by that name
     */
    let mut err: icu::UErrorCode = icu::U_ZERO_ERROR;
    let mut cnv: *mut icu::UConverter = 0 as *mut icu::UConverter;
    *info = 0i32;
    match name_of_file.to_lowercase().as_str() {
        "auto" => return UnicodeMode::Auto,
        "utf8" => return UnicodeMode::Utf8,
        "utf16" => {
            /* depends on host platform */
            return UnicodeMode::Utf16le;
        }
        "utf16be" => return UnicodeMode::Utf16be,
        "utf16le" => return UnicodeMode::Utf16le,
        "bytes" => return UnicodeMode::Raw,
        _ => {}
    }
    /* try for an ICU converter */
    cnv = icu::ucnv_open(
        CString::new(name_of_file.as_str()).unwrap().as_ptr(),
        &mut err,
    ); /* ensure message starts on a new line */
    let result = if cnv.is_null() {
        begin_diagnostic();
        print_nl('U' as i32);
        print_c_string(b"nknown encoding `\x00" as *const u8 as *const i8);
        print_c_str(&name_of_file);
        print_c_string(b"\'; reading as raw bytes\x00" as *const u8 as *const i8);
        end_diagnostic(true);
        UnicodeMode::Raw
    } else {
        icu::ucnv_close(cnv);
        *info = maketexstring(&name_of_file);
        UnicodeMode::ICUMapping
    };
    result
}

pub(crate) unsafe fn print_utf8_str(mut string: &[u8]) {
    for &c in string {
        print_raw_char(c as UTF16_code, true);
    }
    /* bypass utf-8 encoding done in print_char() */
}

pub(crate) unsafe fn print_chars(mut string: *const u16, mut len: i32) {
    loop {
        let fresh3 = len;
        len = len - 1;
        if !(fresh3 > 0i32) {
            break;
        }
        let fresh4 = string;
        string = string.offset(1);
        print_char(*fresh4 as i32);
    }
}

unsafe fn load_mapping_file(
    mut s: *const i8,
    mut e: *const i8,
    mut byteMapping: i8,
) -> *mut libc::c_void {
    let mut cnv = 0 as teckit::TECkit_Converter;
    let mut buffer: *mut i8 = xmalloc((e.offset_from(s) as i64 + 5i32 as i64) as size_t) as *mut i8;
    strncpy(buffer, s, e.offset_from(s) as usize);
    *buffer.offset(e.offset_from(s) as i64 as isize) = 0_i8;
    strcat(buffer, b".tec\x00" as *const u8 as *const i8);
    if let Some(mut map) = ttstub_input_open(buffer, TTInputFormat::MISCFONTS, 0i32) {
        let mut mappingSize: size_t = ttstub_input_get_size(&mut map);
        let mut mapping: *mut u8 = xmalloc(mappingSize) as *mut u8;
        let mut r: ssize_t = ttstub_input_read(map.as_ptr(), mapping as *mut i8, mappingSize);
        if r < 0 || r as size_t != mappingSize {
            abort!(
                "could not read mapping file \"{}\"",
                CStr::from_ptr(buffer).display()
            );
        }
        ttstub_input_close(map);
        if byteMapping != 0 {
            teckit::TECkit_CreateConverter(
                mapping,
                mappingSize as u32,
                0i32 as u8,
                4i32 as u16,
                1i32 as u16,
                &mut cnv,
            );
        } else {
            teckit::TECkit_CreateConverter(
                mapping,
                mappingSize as u32,
                1i32 as u8,
                4i32 as u16,
                4i32 as u16,
                &mut cnv,
            );
        }
        if cnv.is_null() {
            /* tracing */
            font_mapping_warning(
                std::slice::from_raw_parts(buffer as *const u8, strlen(buffer) as usize),
                2i32,
            );
        /* not loadable */
        } else if get_tracing_fonts_state() > 1i32 {
            font_mapping_warning(
                std::slice::from_raw_parts(buffer as *const u8, strlen(buffer) as usize),
                0i32,
            );
        }
    } else {
        font_mapping_warning(
            std::slice::from_raw_parts(buffer as *const u8, strlen(buffer) as usize),
            1i32,
        );
        /* not found */
    }
    free(buffer as *mut libc::c_void);
    cnv as *mut libc::c_void
}
static mut saved_mapping_name: String = String::new();
pub(crate) unsafe fn check_for_tfm_font_mapping() {
    let cp = name_of_file.find(":mapping=");

    saved_mapping_name = String::new();

    if let Some(cp) = cp {
        let (a, b) = name_of_file.split_at(cp);
        let mut cp = &b.as_bytes()[9..];
        while !cp.is_empty() && cp[0] <= ' ' as u8 {
            cp = &cp[1..];
        }
        if !cp.is_empty() {
            saved_mapping_name = String::from_utf8_lossy(cp).to_string();
        }
        name_of_file = String::from(a);
    }
}
pub(crate) unsafe fn load_tfm_font_mapping() -> *mut libc::c_void {
    let mut rval: *mut libc::c_void = 0 as *mut libc::c_void;
    if !saved_mapping_name.is_empty() {
        let name = CString::new(saved_mapping_name.as_str()).unwrap();
        rval = load_mapping_file(
            name.as_ptr(),
            name.as_ptr().offset(name.as_bytes().len() as isize),
            1_i8,
        );
        saved_mapping_name = String::new();
    }
    rval
}
pub(crate) unsafe fn apply_tfm_font_mapping(mut cnv: *mut libc::c_void, mut c: i32) -> i32 {
    let mut in_0: UniChar = c as UniChar;
    let mut out: [u8; 2] = [0; 2];
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    /* TECkit_Status status; */
    /* status = */
    teckit::TECkit_ConvertBuffer(
        cnv as teckit::TECkit_Converter,
        &mut in_0 as *mut UniChar as *const u8,
        ::std::mem::size_of::<UniChar>() as u64 as u32,
        &mut inUsed,
        out.as_mut_ptr(),
        ::std::mem::size_of::<[u8; 2]>() as u64 as u32,
        &mut outUsed,
        1i32 as u8,
    );
    if outUsed < 1_u32 {
        0i32
    } else {
        out[0] as i32
    }
}
pub(crate) unsafe fn read_double(mut s: *mut *const i8) -> f64 {
    let mut neg: i32 = 0i32;
    let mut val: f64 = 0.0f64;
    let mut cp: *const i8 = *s;
    while *cp as i32 == ' ' as i32 || *cp as i32 == '\t' as i32 {
        cp = cp.offset(1)
    }
    if *cp as i32 == '-' as i32 {
        neg = 1i32;
        cp = cp.offset(1)
    } else if *cp as i32 == '+' as i32 {
        cp = cp.offset(1)
    }
    while *cp as i32 >= '0' as i32 && *cp as i32 <= '9' as i32 {
        val = val * 10.0f64 + *cp as i32 as f64 - '0' as i32 as f64;
        cp = cp.offset(1)
    }
    if *cp as i32 == '.' as i32 {
        let mut dec: f64 = 10.0f64;
        cp = cp.offset(1);
        while *cp as i32 >= '0' as i32 && *cp as i32 <= '9' as i32 {
            val = val + (*cp as i32 - '0' as i32) as f64 / dec;
            cp = cp.offset(1);
            dec = dec * 10.0f64
        }
    }
    *s = cp;
    if neg != 0 {
        -val
    } else {
        val
    }
}
unsafe fn read_tag_with_param(mut cp: *const i8, mut param: *mut i32) -> hb_tag_t {
    let mut cp2: *const i8 = ptr::null();
    let mut tag: hb_tag_t = 0;
    cp2 = cp;
    while *cp2 != 0
        && *cp2 as i32 != ':' as i32
        && *cp2 as i32 != ';' as i32
        && *cp2 as i32 != ',' as i32
        && *cp2 as i32 != '=' as i32
    {
        cp2 = cp2.offset(1)
    }
    tag = hb_tag_from_string(cp, cp2.offset_from(cp) as i64 as i32);
    cp = cp2;
    if *cp as i32 == '=' as i32 {
        let mut neg: i32 = 0i32;
        cp = cp.offset(1);
        if *cp as i32 == '-' as i32 {
            neg += 1;
            cp = cp.offset(1)
        }
        while *cp as i32 >= '0' as i32 && *cp as i32 <= '9' as i32 {
            *param = *param * 10i32 + *cp as i32 - '0' as i32;
            cp = cp.offset(1)
        }
        if neg != 0 {
            *param = -*param
        }
    }
    tag
}
pub(crate) unsafe fn read_rgb_a(mut cp: *mut *const i8) -> u32 {
    let mut rgbValue: u32 = 0_u32;
    let mut alpha: u32 = 0_u32;
    let mut i: i32 = 0;
    i = 0i32;
    while i < 6i32 {
        if **cp as i32 >= '0' as i32 && **cp as i32 <= '9' as i32 {
            rgbValue = (rgbValue << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('0' as i32 as u32)
        } else if **cp as i32 >= 'A' as i32 && **cp as i32 <= 'F' as i32 {
            rgbValue = (rgbValue << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('A' as i32 as u32)
                .wrapping_add(10_u32)
        } else if **cp as i32 >= 'a' as i32 && **cp as i32 <= 'f' as i32 {
            rgbValue = (rgbValue << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('a' as i32 as u32)
                .wrapping_add(10_u32)
        } else {
            return 0xff_u32;
        }
        *cp = (*cp).offset(1);
        i += 1
    }
    rgbValue <<= 8i32;
    i = 0i32;
    while i < 2i32 {
        if **cp as i32 >= '0' as i32 && **cp as i32 <= '9' as i32 {
            alpha = (alpha << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('0' as i32 as u32)
        } else if **cp as i32 >= 'A' as i32 && **cp as i32 <= 'F' as i32 {
            alpha = (alpha << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('A' as i32 as u32)
                .wrapping_add(10_u32)
        } else {
            if !(**cp as i32 >= 'a' as i32 && **cp as i32 <= 'f' as i32) {
                break;
            }
            alpha = (alpha << 4i32)
                .wrapping_add(**cp as u32)
                .wrapping_sub('a' as i32 as u32)
                .wrapping_add(10_u32)
        }
        *cp = (*cp).offset(1);
        i += 1
    }
    if i == 2i32 {
        rgbValue = (rgbValue as u32).wrapping_add(alpha) as u32
    } else {
        rgbValue = (rgbValue as u32).wrapping_add(0xff_u32) as u32
    }
    rgbValue
}
pub(crate) unsafe fn readCommonFeatures(
    mut feat: *const i8,
    mut end: *const i8,
    mut extend: *mut f32,
    mut slant: *mut f32,
    mut embolden: *mut f32,
    mut letterspace: *mut f32,
    mut rgbValue: *mut u32,
) -> i32
// returns 1 to go to next_option, -1 for bad_option, 0 to continue
{
    let mut sep: *const i8 = ptr::null();
    sep = strstartswith(feat, b"mapping\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        loaded_font_mapping = load_mapping_file(sep.offset(1), end, 0_i8);
        return 1i32;
    }
    sep = strstartswith(feat, b"extend\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        sep = sep.offset(1);
        *extend = read_double(&mut sep) as f32;
        return 1i32;
    }
    sep = strstartswith(feat, b"slant\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        sep = sep.offset(1);
        *slant = read_double(&mut sep) as f32;
        return 1i32;
    }
    sep = strstartswith(feat, b"embolden\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        sep = sep.offset(1);
        *embolden = read_double(&mut sep) as f32;
        return 1i32;
    }
    sep = strstartswith(feat, b"letterspace\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        sep = sep.offset(1);
        *letterspace = read_double(&mut sep) as f32;
        return 1i32;
    }
    sep = strstartswith(feat, b"color\x00" as *const u8 as *const i8);
    if !sep.is_null() {
        let mut s: *const i8 = ptr::null();
        if *sep as i32 != '=' as i32 {
            return -1i32;
        }
        sep = sep.offset(1);
        s = sep;
        *rgbValue = read_rgb_a(&mut sep);
        if sep == s.offset(6) || sep == s.offset(8) {
            loaded_font_flags = (loaded_font_flags as i32 | 0x1i32) as i8
        } else {
            return -1i32;
        }
        return 1i32;
    }
    0i32
}
unsafe fn readFeatureNumber(
    mut s: *const i8,
    mut e: *const i8,
    mut f: *mut hb_tag_t,
    mut v: *mut i32,
) -> bool
/* s...e is a "id=setting" string; */ {
    *f = 0i32 as hb_tag_t;
    *v = 0i32;
    if (*s as i32) < '0' as i32 || *s as i32 > '9' as i32 {
        return false;
    }
    while *s as i32 >= '0' as i32 && *s as i32 <= '9' as i32 {
        let fresh5 = s;
        s = s.offset(1);
        *f = (*f)
            .wrapping_mul(10_u32)
            .wrapping_add(*fresh5 as u32)
            .wrapping_sub('0' as i32 as u32)
    }
    while *s as i32 == ' ' as i32 || *s as i32 == '\t' as i32 {
        s = s.offset(1)
    }
    let fresh6 = s;
    s = s.offset(1);
    if *fresh6 as i32 != '=' as i32 {
        /* no setting was specified */
        return false;
    } /* NULL-terminated array */
    if (*s as i32) < '0' as i32 || *s as i32 > '9' as i32 {
        return false;
    }
    while *s as i32 >= '0' as i32 && *s as i32 <= '9' as i32 {
        let fresh7 = s;
        s = s.offset(1);
        *v = *v * 10i32 + *fresh7 as i32 - '0' as i32
    }
    while *s as i32 == ' ' as i32 || *s as i32 == '\t' as i32 {
        s = s.offset(1)
    }
    if s != e {
        return false;
    }
    true
}
unsafe fn loadOTfont(
    mut fontRef: PlatformFontRef,
    mut font: XeTeXFont,
    mut scaled_size: Fixed,
    mut cp1: *mut i8,
) -> *mut libc::c_void {
    let mut current_block: u64;
    let mut engine: XeTeXLayoutEngine = 0 as XeTeXLayoutEngine;
    let mut script: hb_tag_t = (0_u32 & 0xff_u32) << 24i32
        | (0_u32 & 0xff_u32) << 16i32
        | (0_u32 & 0xff_u32) << 8i32
        | 0_u32 & 0xff_u32;
    let mut language: *mut i8 = 0 as *mut i8;
    let mut features: *mut hb_feature_t = 0 as *mut hb_feature_t;
    let mut shapers: *mut *mut i8 = 0 as *mut *mut i8;
    let mut nFeatures: i32 = 0i32;
    let mut nShapers: i32 = 0i32;
    let mut cp2: *mut i8 = 0 as *mut i8;
    let mut cp3: *const i8 = ptr::null();
    let mut tag: hb_tag_t = 0;
    let mut rgbValue: u32 = 0xff_u32;
    let mut extend: f32 = 1.0f64 as f32;
    let mut slant: f32 = 0.0f64 as f32;
    let mut embolden: f32 = 0.0f64 as f32;
    let mut letterspace: f32 = 0.0f64 as f32;
    let mut i: i32 = 0;
    let mut reqEngine: i8 = getReqEngine();
    if reqEngine as i32 == 'O' as i32 || reqEngine as i32 == 'G' as i32 {
        shapers = xrealloc(
            shapers as *mut libc::c_void,
            ((nShapers + 1i32) as u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as _,
        ) as *mut *mut i8;
        if reqEngine as i32 == 'O' as i32 {
            static mut ot_const: [i8; 3] = [111, 116, 0];
            let ref mut fresh8 = *shapers.offset(nShapers as isize);
            *fresh8 = ot_const.as_mut_ptr()
        } else if reqEngine as i32 == 'G' as i32 {
            static mut graphite2_const: [i8; 10] = [103, 114, 97, 112, 104, 105, 116, 101, 50, 0];
            let ref mut fresh9 = *shapers.offset(nShapers as isize);
            *fresh9 = graphite2_const.as_mut_ptr()
        }
        nShapers += 1
    }
    if reqEngine as i32 == 'G' as i32 {
        let mut tmpShapers: [*mut i8; 1] = [*shapers.offset(0)];
        /* create a default engine so we can query the font for Graphite features;
         * because of font caching, it's cheap to discard this and create the real one later */
        engine = createLayoutEngine(
            fontRef,
            font,
            script,
            language,
            features,
            nFeatures,
            tmpShapers.as_mut_ptr(),
            rgbValue,
            extend,
            slant,
            embolden,
        );
        if engine.is_null() {
            return 0 as *mut libc::c_void;
        }
    }
    /* scan the feature string (if any) */
    if !cp1.is_null() {
        while *cp1 != 0 {
            if *cp1 as i32 == ':' as i32 || *cp1 as i32 == ';' as i32 || *cp1 as i32 == ',' as i32 {
                cp1 = cp1.offset(1)
            }
            while *cp1 as i32 == ' ' as i32 || *cp1 as i32 == '\t' as i32 {
                /* skip leading whitespace */
                cp1 = cp1.offset(1)
            }
            if *cp1 == 0 {
                break;
            }
            cp2 = cp1;
            while *cp2 != 0
                && *cp2 as i32 != ':' as i32
                && *cp2 as i32 != ';' as i32
                && *cp2 as i32 != ',' as i32
            {
                cp2 = cp2.offset(1)
            }
            cp3 = strstartswith(cp1, b"script\x00" as *const u8 as *const i8);
            if !cp3.is_null() {
                if *cp3 as i32 != '=' as i32 {
                    current_block = 10622493848381539643;
                } else {
                    cp3 = cp3.offset(1);
                    script = hb_tag_from_string(cp3, cp2.offset_from(cp3) as i64 as i32);
                    current_block = 13857423536159756434;
                }
            } else {
                cp3 = strstartswith(cp1, b"language\x00" as *const u8 as *const i8);
                if !cp3.is_null() {
                    if *cp3 as i32 != '=' as i32 {
                        current_block = 10622493848381539643;
                    } else {
                        cp3 = cp3.offset(1);
                        language = xmalloc((cp2.offset_from(cp3) as i64 + 1i32 as i64) as size_t)
                            as *mut i8;
                        *language.offset(cp2.offset_from(cp3) as i64 as isize) =
                            '\u{0}' as i32 as i8;
                        memcpy(
                            language as *mut libc::c_void,
                            cp3 as *const libc::c_void,
                            cp2.offset_from(cp3) as usize,
                        );
                        current_block = 13857423536159756434;
                    }
                } else {
                    cp3 = strstartswith(cp1, b"shaper\x00" as *const u8 as *const i8);
                    if !cp3.is_null() {
                        if *cp3 as i32 != '=' as i32 {
                            current_block = 10622493848381539643;
                        } else {
                            cp3 = cp3.offset(1);
                            shapers = xrealloc(
                                shapers as *mut libc::c_void,
                                ((nShapers + 1i32) as u64)
                                    .wrapping_mul(::std::mem::size_of::<*mut i8>() as u64)
                                    as _,
                            ) as *mut *mut i8;
                            /* some dumb systems have no strndup() */
                            let ref mut fresh10 = *shapers.offset(nShapers as isize);
                            *fresh10 = strdup(cp3);
                            *(*shapers.offset(nShapers as isize))
                                .offset(cp2.offset_from(cp3) as i64 as isize) =
                                '\u{0}' as i32 as i8;
                            nShapers += 1;
                            current_block = 13857423536159756434;
                        }
                    } else {
                        i = readCommonFeatures(
                            cp1,
                            cp2,
                            &mut extend,
                            &mut slant,
                            &mut embolden,
                            &mut letterspace,
                            &mut rgbValue,
                        );
                        if i == 1i32 {
                            current_block = 13857423536159756434;
                        } else if i == -1i32 {
                            current_block = 10622493848381539643;
                        } else {
                            if reqEngine as i32 == 'G' as i32 {
                                let mut value: i32 = 0i32;
                                if readFeatureNumber(cp1, cp2, &mut tag, &mut value)
                                    || findGraphiteFeature(engine, cp1, cp2, &mut tag, &mut value)
                                        as i32
                                        != 0
                                {
                                    features = xrealloc(
                                        features as *mut libc::c_void,
                                        ((nFeatures + 1i32) as u64).wrapping_mul(
                                            ::std::mem::size_of::<hb_feature_t>() as u64,
                                        ) as _,
                                    )
                                        as *mut hb_feature_t;
                                    (*features.offset(nFeatures as isize)).tag = tag;
                                    (*features.offset(nFeatures as isize)).value = value as u32;
                                    (*features.offset(nFeatures as isize)).start = 0_u32;
                                    (*features.offset(nFeatures as isize)).end = -1i32 as u32;
                                    nFeatures += 1;
                                    current_block = 13857423536159756434;
                                } else {
                                    current_block = 15669289850109000831;
                                }
                            } else {
                                current_block = 15669289850109000831;
                            }
                            match current_block {
                                13857423536159756434 => {}
                                _ => {
                                    if *cp1 as i32 == '+' as i32 {
                                        let mut param: i32 = 0i32;
                                        tag = read_tag_with_param(cp1.offset(1), &mut param);
                                        features = xrealloc(
                                            features as *mut libc::c_void,
                                            ((nFeatures + 1i32) as u64).wrapping_mul(
                                                ::std::mem::size_of::<hb_feature_t>() as u64,
                                            ) as _,
                                        )
                                            as *mut hb_feature_t;
                                        (*features.offset(nFeatures as isize)).tag = tag;
                                        (*features.offset(nFeatures as isize)).start = 0_u32;
                                        (*features.offset(nFeatures as isize)).end = -1i32 as u32;
                                        // for backward compatibility with pre-0.9999 where feature
                                        // indices started from 0
                                        if param >= 0i32 {
                                            param += 1
                                        }
                                        (*features.offset(nFeatures as isize)).value = param as u32;
                                        nFeatures += 1;
                                        current_block = 13857423536159756434;
                                    } else if *cp1 as i32 == '-' as i32 {
                                        cp1 = cp1.offset(1);
                                        tag = hb_tag_from_string(
                                            cp1,
                                            cp2.offset_from(cp1) as i64 as i32,
                                        );
                                        features = xrealloc(
                                            features as *mut libc::c_void,
                                            ((nFeatures + 1i32) as u64).wrapping_mul(
                                                ::std::mem::size_of::<hb_feature_t>() as u64,
                                            ) as _,
                                        )
                                            as *mut hb_feature_t;
                                        (*features.offset(nFeatures as isize)).tag = tag;
                                        (*features.offset(nFeatures as isize)).start = 0_u32;
                                        (*features.offset(nFeatures as isize)).end = -1i32 as u32;
                                        (*features.offset(nFeatures as isize)).value = 0_u32;
                                        nFeatures += 1;
                                        current_block = 13857423536159756434;
                                    } else if !strstartswith(
                                        cp1,
                                        b"vertical\x00" as *const u8 as *const i8,
                                    )
                                    .is_null()
                                    {
                                        cp3 = cp2;
                                        if *cp3 as i32 == ';' as i32
                                            || *cp3 as i32 == ':' as i32
                                            || *cp3 as i32 == ',' as i32
                                        {
                                            cp3 = cp3.offset(-1)
                                        }
                                        while *cp3 as i32 == '\u{0}' as i32
                                            || *cp3 as i32 == ' ' as i32
                                            || *cp3 as i32 == '\t' as i32
                                        {
                                            cp3 = cp3.offset(-1)
                                        }
                                        if *cp3 != 0 {
                                            cp3 = cp3.offset(1)
                                        }
                                        if cp3 == cp1.offset(8) as *const i8 {
                                            loaded_font_flags =
                                                (loaded_font_flags as i32 | 0x2i32) as i8;
                                            current_block = 13857423536159756434;
                                        } else {
                                            current_block = 10622493848381539643;
                                        }
                                    } else {
                                        current_block = 10622493848381539643;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            match current_block {
                10622493848381539643 => {
                    font_feature_warning(
                        std::slice::from_raw_parts(cp1 as *const u8, cp2.offset_from(cp1) as usize),
                        &[],
                    );
                }
                _ => {}
            }
            cp1 = cp2
        }
    }
    /* break if end of string */
    if !shapers.is_null() {
        shapers = xrealloc(
            shapers as *mut libc::c_void,
            ((nShapers + 1i32) as u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as _,
        ) as *mut *mut i8;
        let ref mut fresh11 = *shapers.offset(nShapers as isize);
        *fresh11 = 0 as *mut i8
    }
    if embolden as f64 != 0.0f64 {
        embolden = (embolden as f64 * Fix2D(scaled_size) / 100.0f64) as f32
    }
    if letterspace as f64 != 0.0f64 {
        loaded_font_letter_space = (letterspace as f64 / 100.0f64 * scaled_size as f64) as scaled_t
    }
    if loaded_font_flags as i32 & 0x1i32 == 0i32 {
        rgbValue = 0xff_u32
    }
    if loaded_font_flags as i32 & 0x2i32 != 0i32 {
        setFontLayoutDir(font, 1i32);
    }
    engine = createLayoutEngine(
        fontRef, font, script, language, features, nFeatures, shapers, rgbValue, extend, slant,
        embolden,
    );
    if engine.is_null() {
        // only free these if creation failed, otherwise the engine now owns them
        free(features as *mut libc::c_void);
        free(shapers as *mut libc::c_void);
    } else {
        native_font_type_flag = 0xfffeu32 as i32
    }
    engine as *mut libc::c_void
}

unsafe fn splitFontName(
    mut name: *mut i8,
    mut var: *mut *mut i8,
    mut feat: *mut *mut i8,
    mut end: *mut *mut i8,
    mut index: *mut i32,
) {
    *var = 0 as *mut i8;
    *feat = 0 as *mut i8;
    *index = 0i32;
    if *name as i32 == '[' as i32 {
        let mut withinFileName: i32 = 1i32;
        name = name.offset(1);
        while *name != 0 {
            if withinFileName != 0 && *name as i32 == ']' as i32 {
                withinFileName = 0i32;
                if (*var).is_null() {
                    *var = name
                }
            } else if *name as i32 == ':' as i32 {
                if withinFileName != 0 && (*var).is_null() {
                    *var = name;
                    name = name.offset(1);
                    while *name as i32 >= '0' as i32 && *name as i32 <= '9' as i32 {
                        let fresh12 = name;
                        name = name.offset(1);
                        *index = *index * 10i32 + *fresh12 as i32 - '0' as i32
                    }
                    name = name.offset(-1)
                } else if withinFileName == 0 && (*feat).is_null() {
                    *feat = name
                }
            }
            name = name.offset(1)
        }
        *end = name
    } else {
        while *name != 0 {
            if *name as i32 == '/' as i32 && (*var).is_null() && (*feat).is_null() {
                *var = name
            } else if *name as i32 == ':' as i32 && (*feat).is_null() {
                *feat = name
            }
            name = name.offset(1)
        }
        *end = name
    }
    if (*feat).is_null() {
        *feat = name
    }
    if (*var).is_null() {
        *var = *feat
    };
}
pub(crate) unsafe fn find_native_font(
    mut uname: *const i8,
    mut scaled_size: i32,
) -> *mut libc::c_void
/* scaled_size here is in TeX points, or is a negative integer for 'scaled_t' */ {
    let mut rval: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut nameString: *mut i8 = 0 as *mut i8;
    let mut var: *mut i8 = 0 as *mut i8;
    let mut feat: *mut i8 = 0 as *mut i8;
    let mut end: *mut i8 = 0 as *mut i8;
    let mut name: *mut i8 = uname as *mut i8;
    let mut varString: *mut i8 = 0 as *mut i8;
    let mut featString: *mut i8 = 0 as *mut i8;
    let mut fontRef: PlatformFontRef = 0 as PlatformFontRef;
    let mut font: XeTeXFont = 0 as XeTeXFont;
    let mut index: i32 = 0i32;
    loaded_font_mapping = 0 as *mut libc::c_void;
    loaded_font_flags = 0_i8;
    loaded_font_letter_space = 0i32;
    splitFontName(name, &mut var, &mut feat, &mut end, &mut index);
    nameString = xmalloc((var.offset_from(name) as i64 + 1i32 as i64) as size_t) as *mut i8;
    strncpy(nameString, name, var.offset_from(name) as usize);
    *nameString.offset(var.offset_from(name) as i64 as isize) = 0_i8;
    if feat > var {
        varString = xmalloc(feat.offset_from(var) as i64 as size_t) as *mut i8;
        strncpy(
            varString,
            var.offset(1),
            (feat.offset_from(var) as i64 - 1i32 as i64) as usize,
        );
        *varString.offset((feat.offset_from(var) as i64 - 1i32 as i64) as isize) = 0_i8
    }
    if end > feat {
        featString = xmalloc(end.offset_from(feat) as i64 as size_t) as *mut i8;
        strncpy(
            featString,
            feat.offset(1),
            (end.offset_from(feat) as i64 - 1i32 as i64) as usize,
        );
        *featString.offset((end.offset_from(feat) as i64 - 1i32 as i64) as isize) = 0_i8
    }
    // check for "[filename]" form, don't search maps in this case
    if *nameString.offset(0) as i32 == '[' as i32 {
        if scaled_size < 0i32 {
            font = createFontFromFile(nameString.offset(1), index, 655360i64 as Fixed);
            if !font.is_null() {
                let mut dsize: Fixed = D2Fix(getDesignSize(font));
                if scaled_size == -1000i32 {
                    scaled_size = dsize
                } else {
                    scaled_size = xn_over_d(dsize, -scaled_size, 1000i32)
                }
                deleteFont(font);
            }
        }
        font = createFontFromFile(nameString.offset(1), index, scaled_size);
        if !font.is_null() {
            loaded_font_design_size = D2Fix(getDesignSize(font));
            /* This is duplicated in XeTeXFontMgr::findFont! */
            setReqEngine(0_i8);
            if !varString.is_null() {
                if !strstartswith(varString, b"/AAT\x00" as *const u8 as *const i8).is_null() {
                    setReqEngine('A' as i32 as i8);
                } else if !strstartswith(varString, b"/OT\x00" as *const u8 as *const i8).is_null()
                    || !strstartswith(varString, b"/ICU\x00" as *const u8 as *const i8).is_null()
                {
                    setReqEngine('O' as i32 as i8);
                } else if !strstartswith(varString, b"/GR\x00" as *const u8 as *const i8).is_null()
                {
                    setReqEngine('G' as i32 as i8);
                }
            }
            rval = loadOTfont(0 as PlatformFontRef, font, scaled_size, featString);
            if rval.is_null() {
                deleteFont(font);
            }
            if !rval.is_null() && get_tracing_fonts_state() > 0i32 {
                begin_diagnostic();
                print_nl(' ' as i32);
                print_c_string(b"-> \x00" as *const u8 as *const i8);
                print_c_string(nameString.offset(1));
                end_diagnostic(false);
            }
        }
    } else {
        fontRef = findFontByName(nameString, varString, Fix2D(scaled_size));
        if !fontRef.is_null() {
            /* update name_of_file to the full name of the font, for error messages during font loading */
            let mut fullName: *const i8 = getFullName(fontRef);
            name_of_file = to_rust_string(fullName);
            if scaled_size < 0i32 {
                font = createFont(fontRef, scaled_size);
                if !font.is_null() {
                    let mut dsize_0: Fixed = D2Fix(getDesignSize(font));
                    if scaled_size == -1000i32 {
                        scaled_size = dsize_0
                    } else {
                        scaled_size = xn_over_d(dsize_0, -scaled_size, 1000i32)
                    }
                    deleteFont(font);
                }
            }
            font = createFont(fontRef, scaled_size);
            if !font.is_null() {
                #[cfg(not(target_os = "macos"))]
                {
                    rval = loadOTfont(fontRef, font, scaled_size, featString);
                    if rval.is_null() {
                        deleteFont(font);
                    }
                }
                #[cfg(target_os = "macos")]
                {
                    /* decide whether to use AAT or OpenType rendering with this font */
                    if getReqEngine() as libc::c_int == 'A' as i32 {
                        rval = aat::loadAATfont(fontRef, scaled_size, featString);
                        if rval.is_null() {
                            deleteFont(font);
                        }
                    } else {
                        if getReqEngine() as libc::c_int == 'O' as i32
                            || getReqEngine() as libc::c_int == 'G' as i32
                            || !getFontTablePtr(
                                font,
                                ('G' as i32 as u32 & 0xffi32 as libc::c_uint) << 24i32
                                    | ('S' as i32 as u32 & 0xffi32 as libc::c_uint) << 16i32
                                    | ('U' as i32 as u32 & 0xffi32 as libc::c_uint) << 8i32
                                    | 'B' as i32 as u32 & 0xffi32 as libc::c_uint,
                            )
                            .is_null()
                            || !getFontTablePtr(
                                font,
                                ('G' as i32 as u32 & 0xffi32 as libc::c_uint) << 24i32
                                    | ('P' as i32 as u32 & 0xffi32 as libc::c_uint) << 16i32
                                    | ('O' as i32 as u32 & 0xffi32 as libc::c_uint) << 8i32
                                    | 'S' as i32 as u32 & 0xffi32 as libc::c_uint,
                            )
                            .is_null()
                        {
                            rval = loadOTfont(fontRef, font, scaled_size, featString)
                        }
                        /* loadOTfont failed or the above check was false */
                        if rval.is_null() {
                            rval = aat::loadAATfont(fontRef, scaled_size, featString)
                        }
                        if rval.is_null() {
                            deleteFont(font);
                        }
                    }
                }
            }
            /* append the style and feature strings, so that \show\fontID will give a full result */
            if !varString.is_null() && *varString != 0 {
                name_of_file.push('/');
                name_of_file.push_str(&to_rust_string(varString));
            }
            if !featString.is_null() && *featString != 0 {
                name_of_file.push(':');
                name_of_file.push_str(&to_rust_string(featString));
            }
        }
    }
    free(varString as *mut libc::c_void);
    free(featString as *mut libc::c_void);
    free(nameString as *mut libc::c_void);
    rval
}
pub(crate) unsafe fn release_font_engine(mut engine: *mut libc::c_void, mut type_flag: i32) {
    match type_flag as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => CFRelease(engine as CFDictionaryRef as CFTypeRef),
        0xfffeu32 => deleteLayoutEngine(engine as XeTeXLayoutEngine),
        _ => {}
    }
}
pub(crate) unsafe fn ot_get_font_metrics(
    mut pEngine: *mut libc::c_void,
    mut ascent: *mut scaled_t,
    mut descent: *mut scaled_t,
    mut xheight: *mut scaled_t,
    mut capheight: *mut scaled_t,
    mut slant: *mut scaled_t,
) {
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    let mut a: f32 = 0.;
    let mut d: f32 = 0.;
    getAscentAndDescent(engine, &mut a, &mut d);
    *ascent = D2Fix(a as f64);
    *descent = D2Fix(d as f64);
    *slant = D2Fix(
        Fix2D(getSlant(getFont(engine))) * getExtendFactor(engine) as f64
            + getSlantFactor(engine) as f64,
    );
    /* get cap and x height from OS/2 table */
    getCapAndXHeight(engine, &mut a, &mut d);
    *capheight = D2Fix(a as f64);
    *xheight = D2Fix(d as f64);
    /* fallback in case the font does not have OS/2 table */
    if *xheight == 0i32 {
        let mut glyphID: i32 = mapCharToGlyph(engine, 'x' as i32 as u32) as i32;
        if glyphID != 0i32 {
            getGlyphHeightDepth(engine, glyphID as u32, &mut a, &mut d);
            *xheight = D2Fix(a as f64)
        } else {
            *xheight = *ascent / 2i32
            /* arbitrary figure if there's no 'x' in the font */
        }
    }
    if *capheight == 0i32 {
        let mut glyphID_0: i32 = mapCharToGlyph(engine, 'X' as i32 as u32) as i32;
        if glyphID_0 != 0i32 {
            getGlyphHeightDepth(engine, glyphID_0 as u32, &mut a, &mut d);
            *capheight = D2Fix(a as f64)
        } else {
            *capheight = *ascent
            /* arbitrary figure if there's no 'X' in the font */
        }
    };
}
pub(crate) unsafe fn ot_font_get(mut what: i32, mut pEngine: *mut libc::c_void) -> i32 {
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    let mut fontInst: XeTeXFont = getFont(engine);
    match what {
        1 => return countGlyphs(fontInst) as i32,
        8 => {
            /* ie Graphite features */
            return countGraphiteFeatures(engine) as i32;
        }
        16 => return countScripts(fontInst) as i32,
        _ => {}
    }
    0i32
}
pub(crate) unsafe fn ot_font_get_1(
    mut what: i32,
    mut pEngine: *mut libc::c_void,
    mut param: i32,
) -> i32 {
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    let mut fontInst: XeTeXFont = getFont(engine);
    match what {
        17 => return countLanguages(fontInst, param as hb_tag_t) as i32,
        19 => return getIndScript(fontInst, param as u32) as i32,
        9 => {
            /* for graphite fonts...*/
            return getGraphiteFeatureCode(engine, param as u32) as i32;
        }
        11 => return 1i32,
        12 => return countGraphiteFeatureSettings(engine, param as u32) as i32,
        _ => {}
    }
    0i32
}
pub(crate) unsafe fn ot_font_get_2(
    mut what: i32,
    mut pEngine: *mut libc::c_void,
    mut param1: i32,
    mut param2: i32,
) -> i32 {
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    let mut fontInst: XeTeXFont = getFont(engine);
    match what {
        20 => return getIndLanguage(fontInst, param1 as hb_tag_t, param2 as u32) as i32,
        18 => return countFeatures(fontInst, param1 as hb_tag_t, param2 as hb_tag_t) as i32,
        13 => {
            /* for graphite fonts */
            return getGraphiteFeatureSettingCode(engine, param1 as u32, param2 as u32) as i32;
        }
        15 => {
            return (getGraphiteFeatureDefaultSetting(engine, param1 as u32) == param2 as u32)
                as i32
        }
        _ => {}
    } /* to guarantee enough space in the buffer */
    0i32
}
pub(crate) unsafe fn ot_font_get_3(
    mut what: i32,
    mut pEngine: *mut libc::c_void,
    mut param1: i32,
    mut param2: i32,
    mut param3: i32,
) -> i32 {
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    let mut fontInst: XeTeXFont = getFont(engine);
    match what {
        21 => {
            return getIndFeature(
                fontInst,
                param1 as hb_tag_t,
                param2 as hb_tag_t,
                param3 as u32,
            ) as i32
        }
        _ => {}
    }
    0i32
}
pub(crate) unsafe fn gr_print_font_name(
    mut what: i32,
    mut pEngine: *mut libc::c_void,
    mut param1: i32,
    mut param2: i32,
) {
    let mut name: *mut i8 = 0 as *mut i8;
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    match what {
        8 => name = getGraphiteFeatureLabel(engine, param1 as u32),
        9 => name = getGraphiteFeatureSettingLabel(engine, param1 as u32, param2 as u32),
        _ => {}
    }
    if !name.is_null() {
        print_c_string(name);
        gr_label_destroy(name as *mut libc::c_void);
    };
}
pub(crate) unsafe fn gr_font_get_named(mut what: i32, mut pEngine: *mut libc::c_void) -> i32 {
    let mut rval: i64 = -1i32 as i64;
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    match what {
        10 => {
            let name = CString::new(name_of_file.as_str()).unwrap();
            rval =
                findGraphiteFeatureNamed(engine, name.as_ptr(), name_of_file.as_bytes().len() as _)
                    as _;
        }
        _ => {}
    }
    rval as i32
}
pub(crate) unsafe fn gr_font_get_named_1(
    mut what: i32,
    mut pEngine: *mut libc::c_void,
    mut param: i32,
) -> i32 {
    let mut rval: i64 = -1i32 as i64;
    let mut engine: XeTeXLayoutEngine = pEngine as XeTeXLayoutEngine;
    match what {
        14 => {
            let name = CString::new(name_of_file.as_str()).unwrap();
            rval = findGraphiteFeatureSettingNamed(
                engine,
                param as u32,
                name.as_ptr(),
                name_of_file.as_bytes().len() as _,
            ) as _;
        }
        _ => {}
    }
    rval as i32
}
#[cfg(target_os = "macos")]
unsafe fn cgColorToRGBA32(mut color: CGColorRef) -> u32 {
    let mut components: *const CGFloat = CGColorGetComponents(color);
    let mut rval: u32 = (*components.offset(0) * 255.0f64 + 0.5f64) as u8 as u32;
    rval <<= 8i32;
    rval = (rval as libc::c_uint)
        .wrapping_add((*components.offset(1) * 255.0f64 + 0.5f64) as u8 as libc::c_uint)
        as u32 as u32;
    rval <<= 8i32;
    rval = (rval as libc::c_uint)
        .wrapping_add((*components.offset(2) * 255.0f64 + 0.5f64) as u8 as libc::c_uint)
        as u32 as u32;
    rval <<= 8i32;
    rval = (rval as libc::c_uint)
        .wrapping_add((*components.offset(3) * 255.0f64 + 0.5f64) as u8 as libc::c_uint)
        as u32 as u32;
    return rval;
}
static mut xdvBufSize: i32 = 0i32;
pub(crate) unsafe fn makeXDVGlyphArrayData(p: &NativeWord) -> i32 {
    let mut cp: *mut u8 = 0 as *mut u8;
    let mut glyphIDs: *mut u16 = 0 as *mut u16;
    let mut glyph_info: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut locations: *mut FixedPoint = 0 as *mut FixedPoint;
    let mut width: Fixed = 0;
    let mut glyphCount: u16 = p.glyph_count();
    let mut i: i32 = glyphCount as i32 * 10i32 + 8i32;
    if i > xdvBufSize {
        free(xdv_buffer as *mut libc::c_void);
        xdvBufSize = (i / 1024i32 + 1i32) * 1024i32;
        xdv_buffer = xmalloc(xdvBufSize as size_t) as *mut i8
    }
    glyph_info = p.glyph_info_ptr();
    locations = glyph_info as *mut FixedPoint;
    glyphIDs = locations.offset(glyphCount as i32 as isize) as *mut u16;
    cp = xdv_buffer as *mut u8;
    width = p.width();
    let fresh13 = cp;
    cp = cp.offset(1);
    *fresh13 = (width >> 24i32 & 0xffi32) as u8;
    let fresh14 = cp;
    cp = cp.offset(1);
    *fresh14 = (width >> 16i32 & 0xffi32) as u8;
    let fresh15 = cp;
    cp = cp.offset(1);
    *fresh15 = (width >> 8i32 & 0xffi32) as u8;
    let fresh16 = cp;
    cp = cp.offset(1);
    *fresh16 = (width & 0xffi32) as u8;
    let fresh17 = cp;
    cp = cp.offset(1);
    *fresh17 = (glyphCount as i32 >> 8i32 & 0xffi32) as u8;
    let fresh18 = cp;
    cp = cp.offset(1);
    *fresh18 = (glyphCount as i32 & 0xffi32) as u8;
    i = 0i32;
    while i < glyphCount as i32 {
        let mut x: Fixed = (*locations.offset(i as isize)).x;
        let mut y: Fixed = (*locations.offset(i as isize)).y;
        let fresh19 = cp;
        cp = cp.offset(1);
        *fresh19 = (x >> 24i32 & 0xffi32) as u8;
        let fresh20 = cp;
        cp = cp.offset(1);
        *fresh20 = (x >> 16i32 & 0xffi32) as u8;
        let fresh21 = cp;
        cp = cp.offset(1);
        *fresh21 = (x >> 8i32 & 0xffi32) as u8;
        let fresh22 = cp;
        cp = cp.offset(1);
        *fresh22 = (x & 0xffi32) as u8;
        let fresh23 = cp;
        cp = cp.offset(1);
        *fresh23 = (y >> 24i32 & 0xffi32) as u8;
        let fresh24 = cp;
        cp = cp.offset(1);
        *fresh24 = (y >> 16i32 & 0xffi32) as u8;
        let fresh25 = cp;
        cp = cp.offset(1);
        *fresh25 = (y >> 8i32 & 0xffi32) as u8;
        let fresh26 = cp;
        cp = cp.offset(1);
        *fresh26 = (y & 0xffi32) as u8;
        i += 1
    }
    i = 0i32;
    while i < glyphCount as i32 {
        let mut g: u16 = *glyphIDs.offset(i as isize);
        let fresh27 = cp;
        cp = cp.offset(1);
        *fresh27 = (g as i32 >> 8i32 & 0xffi32) as u8;
        let fresh28 = cp;
        cp = cp.offset(1);
        *fresh28 = (g as i32 & 0xffi32) as u8;
        i += 1
    }
    (cp as *mut i8).offset_from(xdv_buffer) as i64 as i32
}
pub(crate) unsafe fn make_font_def(f: usize) -> i32 {
    // XXX: seems like a good idea to make a struct FontDef
    let mut flags: u16 = 0_u16;
    let mut rgba: u32 = 0;
    let mut size: Fixed = 0;
    let mut filename: *mut i8 = 0 as *mut i8;
    let mut index: u32 = 0;
    let mut filenameLen: u8 = 0;
    let mut fontDefLength: i32 = 0;
    let mut cp: *mut i8 = 0 as *mut i8;
    /* PlatformFontRef fontRef = 0; */
    let mut extend: f32 = 1.0f64 as f32;
    let mut slant: f32 = 0.0f64 as f32;
    let mut embolden: f32 = 0.0f64 as f32;
    match FONT_AREA[f] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = 0 as CFDictionaryRef;
            let mut font: CTFontRef = 0 as CTFontRef;
            let mut color: CGColorRef = 0 as CGColorRef;
            let mut t: CGAffineTransform = CGAffineTransform {
                a: 0.,
                b: 0.,
                c: 0.,
                d: 0.,
                tx: 0.,
                ty: 0.,
            };
            let mut emboldenNumber: CFNumberRef = 0 as CFNumberRef;
            let mut fSize: CGFloat = 0.;
            attributes = FONT_LAYOUT_ENGINE[f] as CFDictionaryRef;
            font = CFDictionaryGetValue(attributes, kCTFontAttributeName as *const libc::c_void)
                as CTFontRef;
            filename = aat::getFileNameFromCTFont(font, &mut index);
            assert!(!filename.is_null());
            if !CFDictionaryGetValue(
                attributes,
                kCTVerticalFormsAttributeName as *const libc::c_void,
            )
            .is_null()
            {
                flags = (flags as libc::c_int | 0x100i32) as u16
            }
            color = CFDictionaryGetValue(
                attributes,
                kCTForegroundColorAttributeName as *const libc::c_void,
            ) as CGColorRef;
            if !color.is_null() {
                rgba = cgColorToRGBA32(color)
            }
            t = CTFontGetMatrix(font);
            extend = t.a as f32;
            slant = t.c as f32;
            emboldenNumber = CFDictionaryGetValue(
                attributes,
                aat::getkXeTeXEmboldenAttributeName() as *const libc::c_void,
            ) as CFNumberRef;
            if !emboldenNumber.is_null() {
                CFNumberGetValue(
                    emboldenNumber,
                    kCFNumberFloatType as libc::c_int as CFNumberType,
                    &mut embolden as *mut f32 as *mut libc::c_void,
                );
            }
            fSize = CTFontGetSize(font);
            size = D2Fix(fSize);
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = 0 as *mut XeTeXLayoutEngine_rec;
            engine = FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine;
            /* fontRef = */
            getFontRef(engine);
            filename = getFontFilename(engine, &mut index);
            assert!(!filename.is_null());
            rgba = getRgbValue(engine);
            if FONT_FLAGS[f] as i32 & 0x2i32 != 0i32 {
                flags = (flags as i32 | 0x100i32) as u16
            }
            extend = getExtendFactor(engine);
            slant = getSlantFactor(engine);
            embolden = getEmboldenFactor(engine);
            size = D2Fix(getPointSize(engine) as f64)
        }
        _ => panic!("bad native font flag in `make_font_def`"),
    }
    filenameLen = strlen(filename) as u8;
    /* parameters after internal font ID:
    //  size[4]
    //  flags[2]
    //  l[1] n[l]
    //  if flags & COLORED:
    //      c[4]
     */
    fontDefLength = 4i32 + 2i32 + 1i32 + filenameLen as i32 + 4i32; /* face index */
    if FONT_FLAGS[f] as i32 & 0x1i32 != 0i32 {
        fontDefLength += 4i32; /* 32-bit RGBA value */
        flags = (flags as i32 | 0x200i32) as u16
    }
    if extend as f64 != 1.0f64 {
        fontDefLength += 4i32;
        flags = (flags as i32 | 0x1000i32) as u16
    }
    if slant as f64 != 0.0f64 {
        fontDefLength += 4i32;
        flags = (flags as i32 | 0x2000i32) as u16
    }
    if embolden as f64 != 0.0f64 {
        fontDefLength += 4i32;
        flags = (flags as i32 | 0x4000i32) as u16
    }
    if fontDefLength > xdvBufSize {
        free(xdv_buffer as *mut libc::c_void);
        xdvBufSize = (fontDefLength / 1024i32 + 1i32) * 1024i32;
        xdv_buffer = xmalloc(xdvBufSize as size_t) as *mut i8
    }
    cp = xdv_buffer;
    *(cp as *mut Fixed) = SWAP32(size as u32) as Fixed;
    cp = cp.offset(4);
    *(cp as *mut u16) = SWAP16(flags);
    cp = cp.offset(2);
    *(cp as *mut u8) = filenameLen;
    cp = cp.offset(1);
    memcpy(
        cp as *mut libc::c_void,
        filename as *const libc::c_void,
        filenameLen as _,
    );
    cp = cp.offset(filenameLen as i32 as isize);
    *(cp as *mut u32) = SWAP32(index);
    cp = cp.offset(4);
    if FONT_FLAGS[f] as i32 & 0x1i32 != 0i32 {
        *(cp as *mut u32) = SWAP32(rgba);
        cp = cp.offset(4)
    }
    if flags as i32 & 0x1000i32 != 0 {
        let mut f_0: Fixed = D2Fix(extend as f64);
        *(cp as *mut u32) = SWAP32(f_0 as u32);
        cp = cp.offset(4)
    }
    if flags as i32 & 0x2000i32 != 0 {
        let mut f_1: Fixed = D2Fix(slant as f64);
        *(cp as *mut u32) = SWAP32(f_1 as u32);
        cp = cp.offset(4)
    }
    if flags as i32 & 0x4000i32 != 0 {
        let mut f_2: Fixed = D2Fix(embolden as f64);
        *(cp as *mut u32) = SWAP32(f_2 as u32);
        cp = cp.offset(4)
    }
    free(filename as *mut libc::c_void);
    fontDefLength
}
pub(crate) unsafe fn apply_mapping(
    mut pCnv: *mut libc::c_void,
    mut txtPtr: *mut u16,
    mut txtLen: i32,
) -> i32 {
    let mut cnv = pCnv as teckit::TECkit_Converter;
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    let mut status: teckit::TECkit_Status = 0;
    static mut outLength: u32 = 0i32 as u32;
    /* allocate outBuffer if not big enough */
    if (outLength as u64)
        < (txtLen as u64)
            .wrapping_mul(::std::mem::size_of::<UniChar>() as u64)
            .wrapping_add(32i32 as u64)
    {
        free(mapped_text as *mut libc::c_void);
        outLength = (txtLen as u64)
            .wrapping_mul(::std::mem::size_of::<UniChar>() as u64)
            .wrapping_add(32i32 as u64) as u32;
        mapped_text = xmalloc(outLength as size_t) as *mut UTF16_code
    }
    loop
    /* try the mapping */
    {
        status = teckit::TECkit_ConvertBuffer(
            cnv,
            txtPtr as *mut u8,
            (txtLen as u64).wrapping_mul(::std::mem::size_of::<UniChar>() as u64) as u32,
            &mut inUsed,
            mapped_text as *mut u8,
            outLength,
            &mut outUsed,
            1i32 as u8,
        );
        match status {
            0 => {
                txtPtr = mapped_text as *mut UniChar;
                return (outUsed as u64).wrapping_div(::std::mem::size_of::<UniChar>() as u64)
                    as i32;
            }
            1 => {
                outLength = (outLength as u64).wrapping_add(
                    (txtLen as u64)
                        .wrapping_mul(::std::mem::size_of::<UniChar>() as u64)
                        .wrapping_add(32i32 as u64),
                ) as u32 as u32;
                free(mapped_text as *mut libc::c_void);
                mapped_text = xmalloc(outLength as size_t) as *mut UTF16_code
            }
            _ => return 0i32,
        }
    }
}
unsafe fn snap_zone(value: &mut i32, mut snap_value: scaled_t, mut fuzz: scaled_t) {
    let mut difference: scaled_t = *value - snap_value;
    if difference <= fuzz && difference >= -fuzz {
        *value = snap_value
    };
}
pub(crate) unsafe fn get_native_char_height_depth(font: usize, ch: i32) -> (i32, i32) {
    let mut ht: f32 = 0.0f64 as f32;
    let mut dp: f32 = 0.0f64 as f32;
    let mut fuzz: Fixed = 0;
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[font] as CFDictionaryRef;
            let mut gid: libc::c_int = aat::MapCharToGlyph_AAT(attributes, ch as u32);
            aat::GetGlyphHeightDepth_AAT(attributes, gid as u16, &mut ht, &mut dp);
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine;
            let mut gid: i32 = mapCharToGlyph(engine, ch as u32) as i32;
            getGlyphHeightDepth(engine, gid as u32, &mut ht, &mut dp);
        }
        _ => panic!("bad native font flag in `get_native_char_height_depth`"),
    }
    let mut height = D2Fix(ht as f64);
    let mut depth = D2Fix(dp as f64);
    /* snap to "known" zones for baseline, x-height, cap-height if within 4% of em-size */
    fuzz = FONT_INFO[(6 + PARAM_BASE[font]) as usize].b32.s1 / 25i32;
    snap_zone(&mut depth, 0i32, fuzz);
    snap_zone(&mut height, 0i32, fuzz);
    snap_zone(
        &mut height,
        FONT_INFO[(5 + PARAM_BASE[font]) as usize].b32.s1,
        fuzz,
    );
    snap_zone(
        &mut height,
        FONT_INFO[(8 + PARAM_BASE[font]) as usize].b32.s1,
        fuzz,
    );
    (height, depth)
}
pub(crate) unsafe fn getnativecharht(f: usize, c: i32) -> i32 {
    get_native_char_height_depth(f, c).0
}
pub(crate) unsafe fn getnativechardp(f: usize, c: i32) -> i32 {
    get_native_char_height_depth(f, c).1
}
pub(crate) unsafe fn get_native_char_sidebearings(
    mut font: usize,
    mut ch: i32,
    mut lsb: *mut scaled_t,
    mut rsb: *mut scaled_t,
) {
    let mut l: f32 = 0.;
    let mut r: f32 = 0.;
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[font] as CFDictionaryRef;
            let mut gid: libc::c_int = aat::MapCharToGlyph_AAT(attributes, ch as u32);
            aat::GetGlyphSidebearings_AAT(attributes, gid as u16, &mut l, &mut r);
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine;
            let mut gid: i32 = mapCharToGlyph(engine, ch as u32) as i32;
            getGlyphSidebearings(engine, gid as u32, &mut l, &mut r);
        }
        _ => panic!("bad native font flag in `get_native_char_side_bearings`"),
    }
    *lsb = D2Fix(l as f64);
    *rsb = D2Fix(r as f64);
}
pub(crate) unsafe fn get_glyph_bounds(mut font: usize, mut edge: i32, mut gid: i32) -> scaled_t {
    /* edge codes 1,2,3,4 => L T R B */
    let mut a: f32 = 0.;
    let mut b: f32 = 0.;
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[font] as CFDictionaryRef;
            if edge & 1i32 != 0 {
                aat::GetGlyphSidebearings_AAT(attributes, gid as u16, &mut a, &mut b);
            } else {
                aat::GetGlyphHeightDepth_AAT(attributes, gid as u16, &mut a, &mut b);
            }
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine;
            if edge & 1i32 != 0 {
                getGlyphSidebearings(engine, gid as u32, &mut a, &mut b);
            } else {
                getGlyphHeightDepth(engine, gid as u32, &mut a, &mut b);
            }
        }
        _ => abort!("bad native font flag in `get_glyph_bounds`"),
    }
    D2Fix((if edge <= 2i32 { a } else { b }) as f64)
}
pub(crate) unsafe fn getnativecharic(mut f: usize, mut c: i32) -> scaled_t {
    let mut lsb: scaled_t = 0;
    let mut rsb: scaled_t = 0;
    get_native_char_sidebearings(f, c, &mut lsb, &mut rsb);
    if rsb < 0i32 {
        FONT_LETTER_SPACE[f] - rsb
    } else {
        FONT_LETTER_SPACE[f]
    }
}
/* single-purpose metrics accessors */
pub(crate) unsafe fn getnativecharwd(f: usize, mut c: i32) -> scaled_t {
    let mut wd: scaled_t = 0i32;
    match FONT_AREA[f] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[f] as CFDictionaryRef;
            let mut gid: libc::c_int = aat::MapCharToGlyph_AAT(attributes, c as u32);
            wd = D2Fix(aat::GetGlyphWidth_AAT(attributes, gid as u16))
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine;
            let mut gid: i32 = mapCharToGlyph(engine, c as u32) as i32;
            wd = D2Fix(getGlyphWidthFromEngine(engine, gid as u32) as f64)
        }
        _ => panic!("bad native font flag in `get_native_char_wd`"),
    }
    wd
}
pub(crate) unsafe fn real_get_native_glyph(node: &NativeWord, index: u32) -> u16 {
    let locations: *mut FixedPoint = node.glyph_info_ptr() as *mut FixedPoint;
    let glyphIDs = locations.offset(node.glyph_count() as i32 as isize) as *const u16;
    if index >= node.glyph_count() as u32 {
        0_u16
    } else {
        *glyphIDs.offset(index as isize)
    }
}
pub(crate) unsafe fn store_justified_native_glyphs(node: &mut NativeWord) {
    let f = node.font() as usize;
    match FONT_AREA[f] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            /* separate Mac-only codepath for AAT fonts */
            aat::do_aat_layout(node, true);
            return;
        }
        _ => {
            /* FIXME: 0xfffeu32 case, but the original code wrote it this way */
            /* save desired width */
            let mut savedWidth: i32 = node.width();
            node.set_metrics(false);
            if node.width() != savedWidth {
                /* see how much adjustment is needed overall */
                let mut justAmount: f64 = Fix2D(savedWidth - node.width());
                /* apply justification to spaces (or if there are none, distribute it to all glyphs as a last resort) */
                let mut locations: *mut FixedPoint = node.glyph_info_ptr() as *mut FixedPoint;
                let mut glyphIDs: *mut u16 =
                    locations.offset(node.glyph_count() as i32 as isize) as *mut u16;
                let mut glyphCount: i32 = node.glyph_count() as i32;
                let mut spaceCount: i32 = 0i32;
                let mut i: i32 = 0;
                let mut spaceGlyph: i32 = map_char_to_glyph(f, ' ' as i32);
                i = 0i32;
                while i < glyphCount {
                    if *glyphIDs.offset(i as isize) as i32 == spaceGlyph {
                        spaceCount += 1
                    }
                    i += 1
                }
                if spaceCount > 0i32 {
                    let mut adjustment: f64 = 0i32 as f64;
                    let mut spaceIndex: i32 = 0i32;
                    i = 0i32;
                    while i < glyphCount {
                        (*locations.offset(i as isize)).x =
                            D2Fix(Fix2D((*locations.offset(i as isize)).x) + adjustment);
                        if *glyphIDs.offset(i as isize) as i32 == spaceGlyph {
                            spaceIndex += 1;
                            adjustment = justAmount * spaceIndex as f64 / spaceCount as f64
                        }
                        i += 1
                    }
                } else {
                    i = 1i32;
                    while i < glyphCount {
                        (*locations.offset(i as isize)).x = D2Fix(
                            Fix2D((*locations.offset(i as isize)).x)
                                + justAmount * i as f64 / (glyphCount - 1i32) as f64,
                        );
                        i += 1
                    }
                }
                node.set_width(savedWidth);
            };
        }
    }
}
pub(crate) unsafe fn measure_native_node(node: &mut NativeWord, use_glyph_metrics: bool) {
    let txt = node.text();
    let mut f = node.font() as usize;
    if FONT_AREA[f] as u32 == 0xfffeu32 {
        /* using this font in OT Layout mode, so FONT_LAYOUT_ENGINE[f] is actually a XeTeXLayoutEngine */
        let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine;
        let mut locations: *mut FixedPoint = 0 as *mut FixedPoint;
        let mut glyphIDs: *mut u16 = 0 as *mut u16;
        let mut glyphAdvances: *mut Fixed = 0 as *mut Fixed;
        let mut totalGlyphCount: i32 = 0i32;
        /* need to find direction runs within the text, and call layoutChars separately for each */
        let mut dir: icu::UBiDiDirection = icu::UBIDI_LTR;
        let mut glyph_info: *mut libc::c_void = 0 as *mut libc::c_void;
        static mut positions: *mut FloatPoint = ptr::null_mut();
        static mut advances: *mut f32 = ptr::null_mut();
        static mut glyphs: *mut u32 = ptr::null_mut();
        let mut pBiDi: *mut icu::UBiDi = icu::ubidi_open();
        let mut errorCode: icu::UErrorCode = icu::U_ZERO_ERROR;
        icu::ubidi_setPara(
            pBiDi,
            txt.as_ptr() as *const icu::UChar,
            txt.len() as i32,
            getDefaultDirection(engine) as icu::UBiDiLevel,
            0 as *mut icu::UBiDiLevel,
            &mut errorCode,
        );
        dir = icu::ubidi_getDirection(pBiDi);
        if dir as u32 == icu::UBIDI_MIXED as i32 as u32 {
            /* we actually do the layout twice here, once to count glyphs and then again to get them;
               which is inefficient, but i figure that MIXED is a relatively rare occurrence, so i can't be
               bothered to deal with the memory reallocation headache of doing it differently
            */
            let mut nRuns: i32 = icu::ubidi_countRuns(pBiDi, &mut errorCode);
            let mut width: f64 = 0i32 as f64;
            let mut i: i32 = 0;
            let mut runIndex: i32 = 0;
            let mut logicalStart: i32 = 0;
            let mut length: i32 = 0;
            runIndex = 0i32;
            while runIndex < nRuns {
                dir = icu::ubidi_getVisualRun(pBiDi, runIndex, &mut logicalStart, &mut length);
                totalGlyphCount += layoutChars(
                    engine,
                    txt.as_ptr(),
                    logicalStart,
                    length,
                    txt.len() as i32,
                    dir as u32 == icu::UBIDI_RTL as i32 as u32,
                );
                runIndex += 1
            }
            if totalGlyphCount > 0i32 {
                let mut x: f64 = 0.;
                let mut y: f64 = 0.;
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t);
                locations = glyph_info as *mut FixedPoint;
                glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(
                    totalGlyphCount as size_t,
                    ::std::mem::size_of::<Fixed>() as _,
                ) as *mut Fixed;
                totalGlyphCount = 0i32;
                y = 0.0f64;
                x = y;
                runIndex = 0i32;
                while runIndex < nRuns {
                    let mut nGlyphs: i32 = 0;
                    dir = icu::ubidi_getVisualRun(pBiDi, runIndex, &mut logicalStart, &mut length);
                    nGlyphs = layoutChars(
                        engine,
                        txt.as_ptr(),
                        logicalStart,
                        length,
                        txt.len() as i32,
                        dir as u32 == icu::UBIDI_RTL as i32 as u32,
                    );
                    glyphs = xcalloc(nGlyphs as size_t, ::std::mem::size_of::<u32>()) as *mut u32;
                    positions = xcalloc(
                        (nGlyphs + 1i32) as size_t,
                        ::std::mem::size_of::<FloatPoint>(),
                    ) as *mut FloatPoint;
                    advances = xcalloc(nGlyphs as size_t, ::std::mem::size_of::<f32>()) as *mut f32;
                    getGlyphs(engine, glyphs);
                    getGlyphAdvances(engine, advances);
                    getGlyphPositions(engine, positions);
                    i = 0i32;
                    while i < nGlyphs {
                        *glyphIDs.offset(totalGlyphCount as isize) =
                            *glyphs.offset(i as isize) as u16;
                        (*locations.offset(totalGlyphCount as isize)).x =
                            D2Fix((*positions.offset(i as isize)).x as f64 + x);
                        (*locations.offset(totalGlyphCount as isize)).y =
                            D2Fix((*positions.offset(i as isize)).y as f64 + y);
                        *glyphAdvances.offset(totalGlyphCount as isize) =
                            D2Fix(*advances.offset(i as isize) as f64);
                        totalGlyphCount += 1;
                        i += 1
                    }
                    x += (*positions.offset(nGlyphs as isize)).x as f64;
                    y += (*positions.offset(nGlyphs as isize)).y as f64;
                    free(glyphs as *mut libc::c_void);
                    free(positions as *mut libc::c_void);
                    free(advances as *mut libc::c_void);
                    runIndex += 1
                }
                width = x
            }
            node.set_width(D2Fix(width));
            node.set_glyph_count(totalGlyphCount as u16);
            node.set_glyph_info_ptr(glyph_info);
        } else {
            let mut width_0: f64 = 0i32 as f64;
            totalGlyphCount = layoutChars(
                engine,
                txt.as_ptr(),
                0i32,
                txt.len() as i32,
                txt.len() as i32,
                dir as u32 == icu::UBIDI_RTL as i32 as u32,
            );
            glyphs = xcalloc(totalGlyphCount as size_t, ::std::mem::size_of::<u32>()) as *mut u32;
            positions = xcalloc(
                (totalGlyphCount + 1i32) as size_t,
                ::std::mem::size_of::<FloatPoint>(),
            ) as *mut FloatPoint;
            advances = xcalloc(totalGlyphCount as size_t, ::std::mem::size_of::<f32>()) as *mut f32;
            getGlyphs(engine, glyphs);
            getGlyphAdvances(engine, advances);
            getGlyphPositions(engine, positions);
            if totalGlyphCount > 0i32 {
                let mut i_0: i32 = 0;
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t);
                locations = glyph_info as *mut FixedPoint;
                glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(totalGlyphCount as size_t, ::std::mem::size_of::<Fixed>())
                    as *mut Fixed;
                i_0 = 0i32;
                while i_0 < totalGlyphCount {
                    *glyphIDs.offset(i_0 as isize) = *glyphs.offset(i_0 as isize) as u16;
                    *glyphAdvances.offset(i_0 as isize) =
                        D2Fix(*advances.offset(i_0 as isize) as f64);
                    (*locations.offset(i_0 as isize)).x =
                        D2Fix((*positions.offset(i_0 as isize)).x as f64);
                    (*locations.offset(i_0 as isize)).y =
                        D2Fix((*positions.offset(i_0 as isize)).y as f64);
                    i_0 += 1
                }
                width_0 = (*positions.offset(totalGlyphCount as isize)).x as f64
            }
            node.set_width(D2Fix(width_0));
            node.set_glyph_count(totalGlyphCount as u16);
            node.set_glyph_info_ptr(glyph_info);
            free(glyphs as *mut libc::c_void);
            free(positions as *mut libc::c_void);
            free(advances as *mut libc::c_void);
        }
        icu::ubidi_close(pBiDi);
        if FONT_LETTER_SPACE[f] != 0i32 {
            let mut lsDelta: Fixed = 0i32;
            let mut lsUnit: Fixed = FONT_LETTER_SPACE[f];
            let mut i_1: i32 = 0;
            i_1 = 0i32;
            while i_1 < totalGlyphCount {
                if *glyphAdvances.offset(i_1 as isize) == 0i32 && lsDelta != 0i32 {
                    lsDelta -= lsUnit
                }
                let ref mut fresh31 = (*locations.offset(i_1 as isize)).x;
                *fresh31 += lsDelta;
                lsDelta += lsUnit;
                i_1 += 1
            }
            if lsDelta != 0 {
                lsDelta -= lsUnit;
                let w = node.width();
                node.set_width(w + lsDelta);
            }
        }
        free(glyphAdvances as *mut libc::c_void);
    } else {
        panic!("bad native font flag in `measure_native_node`");
    }
    if !use_glyph_metrics || node.glyph_count() == 0 {
        /* for efficiency, height and depth are the font's ascent/descent,
        not true values based on the actual content of the word,
        unless use_glyph_metrics is non-zero */
        node.set_height(HEIGHT_BASE[f]);
        node.set_depth(DEPTH_BASE[f]);
    } else {
        /* this iterates over the glyph data whether it comes from AAT or OT layout */
        let mut locations_0: *mut FixedPoint = node.glyph_info_ptr() as *mut FixedPoint; /* NB negative is upwards in locations[].y! */
        let mut glyphIDs_0: *mut u16 = locations_0.offset(node.glyph_count() as isize) as *mut u16;
        let mut yMin: f32 = 65536.0f64 as f32;
        let mut yMax: f32 = -65536.0f64 as f32;
        let mut i_2 = 0;
        while i_2 < node.glyph_count() {
            let mut ht: f32 = 0.;
            let mut dp: f32 = 0.;
            let mut y_0: f32 = Fix2D(-(*locations_0.offset(i_2 as isize)).y) as f32;
            let mut bbox: GlyphBBox = GlyphBBox {
                xMin: 0.,
                yMin: 0.,
                xMax: 0.,
                yMax: 0.,
            };
            if getCachedGlyphBBox(f as u16, *glyphIDs_0.offset(i_2 as isize), &mut bbox) == 0i32 {
                match FONT_AREA[f] as u32 {
                    #[cfg(target_os = "macos")]
                    0xffffu32 => {
                        aat::GetGlyphBBox_AAT(
                            FONT_LAYOUT_ENGINE[f] as CFDictionaryRef,
                            *glyphIDs_0.offset(i_2 as isize),
                            &mut bbox,
                        );
                    }
                    0xfffeu32 => {
                        getGlyphBounds(
                            FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine,
                            *glyphIDs_0.offset(i_2 as isize) as u32,
                            &mut bbox,
                        );
                    }
                    _ => {}
                }
                cacheGlyphBBox(f as u16, *glyphIDs_0.offset(i_2 as isize), &mut bbox);
            }
            ht = bbox.yMax;
            dp = -bbox.yMin;
            if y_0 + ht > yMax {
                yMax = y_0 + ht
            }
            if y_0 - dp < yMin {
                yMin = y_0 - dp
            }
            i_2 += 1
        }
        node.set_height(D2Fix(yMax as f64));
        node.set_depth(-D2Fix(yMin as f64));
    };
}
pub(crate) unsafe fn real_get_native_italic_correction(node: &NativeWord) -> Fixed {
    let f = node.font() as usize;
    let n = node.glyph_count() as u32;
    if n > 0_u32 {
        let mut locations: *mut FixedPoint = node.glyph_info_ptr() as *mut FixedPoint;
        let mut glyphIDs: *mut u16 = locations.offset(n as isize) as *mut u16;
        match FONT_AREA[f] as u32 {
            #[cfg(target_os = "macos")]
            0xffffu32 => {
                return D2Fix(aat::GetGlyphItalCorr_AAT(
                    FONT_LAYOUT_ENGINE[f] as CFDictionaryRef,
                    *glyphIDs.offset(n.wrapping_sub(1i32 as libc::c_uint) as isize),
                )) + FONT_LETTER_SPACE[f];
            }
            0xfffeu32 => {
                return D2Fix(getGlyphItalCorr(
                    FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine,
                    *glyphIDs.offset(n.wrapping_sub(1_u32) as isize) as u32,
                ) as f64)
                    + FONT_LETTER_SPACE[f];
            }
            _ => 0i32,
        }
    } else {
        0i32
    }
}
pub(crate) unsafe fn real_get_native_glyph_italic_correction(node: &Glyph) -> Fixed {
    let gid: u16 = node.glyph();
    let f = node.font() as usize;
    match FONT_AREA[f] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            return D2Fix(aat::GetGlyphItalCorr_AAT(
                FONT_LAYOUT_ENGINE[f] as CFDictionaryRef,
                gid,
            ));
        }
        0xfffeu32 => {
            return D2Fix(
                getGlyphItalCorr(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine, gid as u32) as f64,
            );
        }
        _ => {
            0i32
            /* can't actually happen */
        }
    }
}
pub(crate) unsafe fn measure_native_glyph(node: &mut Glyph, use_glyph_metrics: bool) {
    let mut gid = node.glyph();
    let mut f = node.font() as usize;
    let mut ht = 0_f32;
    let mut dp = 0_f32;
    match FONT_AREA[f] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[f] as CFDictionaryRef;
            node.set_width(D2Fix(aat::GetGlyphWidth_AAT(attributes, gid)));
            if use_glyph_metrics {
                aat::GetGlyphHeightDepth_AAT(attributes, gid, &mut ht, &mut dp);
            }
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine = FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine;
            let mut fontInst: XeTeXFont = getFont(engine);
            node.set_width(D2Fix(getGlyphWidth(fontInst, gid as u32) as f64));
            if use_glyph_metrics {
                getGlyphHeightDepth(engine, gid as u32, &mut ht, &mut dp);
            }
        }
        _ => panic!("bad native font flag in `measure_native_glyph`"),
    }
    if use_glyph_metrics {
        node.set_height(D2Fix(ht as f64));
        node.set_depth(D2Fix(dp as f64));
    } else {
        node.set_height(HEIGHT_BASE[f]);
        node.set_depth(DEPTH_BASE[f]);
    };
}
pub(crate) unsafe fn map_char_to_glyph(mut font: usize, mut ch: i32) -> i32 {
    if ch > 0x10ffffi32 || ch >= 0xd800i32 && ch <= 0xdfffi32 {
        return 0i32;
    }
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            return aat::MapCharToGlyph_AAT(FONT_LAYOUT_ENGINE[font] as CFDictionaryRef, ch as u32);
        }
        0xfffeu32 => {
            return mapCharToGlyph(FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine, ch as u32) as i32;
        }
        _ => panic!("bad native font flag in `map_char_to_glyph`"),
    }
}
pub(crate) unsafe fn map_glyph_to_index(mut font: usize) -> i32
/* glyph name is at name_of_file */ {
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            return aat::MapGlyphToIndex_AAT(
                FONT_LAYOUT_ENGINE[font] as CFDictionaryRef,
                CString::new(name_of_file.as_str()).unwrap().as_ptr(),
            );
        }
        0xfffeu32 => {
            return mapGlyphToIndex(
                FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine,
                CString::new(name_of_file.as_str()).unwrap().as_ptr(),
            );
        }
        _ => panic!("bad native font flag in `map_glyph_to_index`"),
    }
}
pub(crate) unsafe fn get_font_char_range(mut font: usize, mut first: i32) -> i32 {
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            return aat::GetFontCharRange_AAT(FONT_LAYOUT_ENGINE[font] as CFDictionaryRef, first);
        }
        0xfffeu32 => {
            return getFontCharRange(FONT_LAYOUT_ENGINE[font] as XeTeXLayoutEngine, first);
        }
        _ => panic!("bad native font flag in `get_font_char_range\'`"),
    }
}
pub(crate) unsafe fn D2Fix(mut d: f64) -> Fixed {
    let rval: Fixed = (d * 65536.0f64 + 0.5f64) as i32;
    rval
}
pub(crate) unsafe fn Fix2D(mut f: Fixed) -> f64 {
    f as f64 / 65536.
}

pub(crate) unsafe fn print_glyph_name(mut font: usize, mut gid: i32) {
    let mut s: *const i8 = ptr::null();
    let mut len: i32 = 0i32;
    match FONT_AREA[font] as u32 {
        #[cfg(target_os = "macos")]
        0xffffu32 => {
            s = aat::GetGlyphNameFromCTFont(aat::font_from_integer(font), gid as u16, &mut len)
        }
        0xfffeu32 => {
            let mut engine: XeTeXLayoutEngine =
                FONT_LAYOUT_ENGINE[font as usize] as XeTeXLayoutEngine;
            s = getGlyphName(getFont(engine), gid as u16, &mut len);
        }
        _ => panic!("bad native font flag in `print_glyph_name`"),
    }
    loop {
        let fresh33 = len;
        len = len - 1;
        if !(fresh33 > 0i32) {
            break;
        }
        let fresh34 = s;
        s = s.offset(1);
        print_char(*fresh34 as i32);
    }
}
pub(crate) unsafe fn real_get_native_word_cp(node: &NativeWord, side: Side) -> i32 {
    let mut locations: *mut FixedPoint = node.glyph_info_ptr() as *mut FixedPoint;
    let mut glyphIDs: *mut u16 = locations.offset(node.glyph_count() as isize) as *mut u16;
    let mut glyphCount: u16 = node.glyph_count();
    let mut f = node.font() as usize;
    let mut actual_glyph: u16 = 0;
    if glyphCount == 0 {
        return 0;
    }
    match side {
        Side::Left => {
            actual_glyph = *glyphIDs
            // we should not reach this point
        }
        Side::Right => actual_glyph = *glyphIDs.offset((glyphCount as i32 - 1i32) as isize),
    }
    get_cp_code(f, actual_glyph as u32, side)
}
