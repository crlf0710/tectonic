#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::text_layout_engine::{
    scaled_t, Fixed, FixedPoint, GlyphEdge, LayoutRequest, NodeLayout, TextLayout, TextLayoutEngine,
};
use crate::xetex_font_info::XeTeXFontInst;
use crate::xetex_xetexd::print_c_string;
use crate::{streq_ptr, strstartswith};
use crate::{
    ttstub_input_close, ttstub_input_get_size, ttstub_input_open, ttstub_input_read_exact,
};
use libc::{free, strncat};
use std::ffi::{CStr, CString};
use std::ptr;

#[cfg(target_os = "macos")]
use super::xetex_aatfont as aat;
#[cfg(target_os = "macos")]
use super::xetex_aatfont::cf_prelude::{
    cgColorToRGBA32, kCFNumberFloatType, kCTFontAttributeName, kCTForegroundColorAttributeName,
    kCTVerticalFormsAttributeName, CFDictionaryGetValue, CFDictionaryRef, CFNumberGetValue,
    CFNumberRef, CFNumberType, CFRelease, CFTypeRef, CGAffineTransform, CGColorGetComponents,
    CGColorRef, CGFloat, CTFontGetMatrix, CTFontGetSize, CTFontRef,
};
use crate::core_memory::{mfree, xcalloc, xmalloc, xrealloc, xstrdup};
use crate::xetex_font_info::GlyphBBox;
use crate::xetex_ini::memory_word;
use crate::xetex_ini::{
    depth_base, font_flags, font_info, font_letter_space, get_text_layout_engine, get_text_layout_engine_mut, height_base,
    loaded_font_design_size, loaded_font_flags, loaded_font_letter_space, loaded_font_mapping,
    mapped_text, name_length, name_of_file, native_font_type_flag, param_base, xdv_buffer,
};
use crate::xetex_output::{print_char, print_int, print_nl, print_raw_char};
use crate::xetex_scaledmath::xn_over_d;
use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetex0::{
    begin_diagnostic, end_diagnostic, font_feature_warning, font_mapping_warning,
    get_tracing_fonts_state,
};
use bridge::_tt_abort;

use crate::stub_stdio::strcasecmp;
use crate::xetex_font_manager::{PlatformFontRef, ShaperRequest};
use crate::xetex_layout_engine::*;
use crate::xetex_texmfmp::maketexstring;
use harfbuzz_sys::{hb_feature_t, hb_tag_from_string, hb_tag_t};
use libc::{memcpy, strcat, strcpy, strdup, strlen, strncpy, strstr};

pub type __ssize_t = i64;
pub type size_t = u64;
pub type ssize_t = __ssize_t;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;

pub type Boolean = libc::c_uchar;

pub type str_number = i32;

pub type UTF16_code = u16;

/* 16.16 version number */

pub type UniChar = u16;

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
unsafe extern "C" fn SWAP16(p: u16) -> u16 {
    ((p as i32 >> 8i32) + ((p as i32) << 8i32)) as u16
}

#[inline]
unsafe extern "C" fn SWAP32(p: u32) -> u32 {
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
static mut brkIter: *mut icu::UBreakIterator =
    0 as *const icu::UBreakIterator as *mut icu::UBreakIterator;
static mut brkLocaleStrNum: i32 = 0i32;

/* info for each glyph is location (FixedPoint) + glyph ID (u16) */
/* glyph ID field in a glyph_node */
/* For Unicode encoding form interpretation... */
#[no_mangle]
pub unsafe extern "C" fn linebreak_start(
    mut f: i32,
    mut localeStrNum: i32,
    mut text: *mut u16,
    mut textLength: i32,
) {
    let mut status: icu::UErrorCode = icu::U_ZERO_ERROR;
    let mut locale: *mut i8 = gettexstring(localeStrNum);
    let mut engine = get_text_layout_engine_mut(f as usize);
    match engine.as_mut().map(|x| &mut **x) {
        Some(TextLayoutEngine::XeTeX(eng)) if streq_ptr(locale, b"G\x00" as *const u8 as *const i8) as i32 != 0 => {
            if eng.initGraphiteBreaking(text, textLength) {
                /* user asked for Graphite line breaking and the font supports it */
                return;
            }
        }
        _ => {}
    }
    if localeStrNum != brkLocaleStrNum && !brkIter.is_null() {
        icu::ubrk_close(brkIter);
        brkIter = 0 as *mut icu::UBreakIterator
    }
    if brkIter.is_null() {
        brkIter = icu::ubrk_open(
            icu::UBRK_LINE,
            locale,
            0 as *const icu::UChar,
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
            print_c_string(locale);
            print_c_string(b"\'; trying default locale `en_us\'.\x00" as *const u8 as *const i8);
            end_diagnostic(1i32 != 0);
            if !brkIter.is_null() {
                icu::ubrk_close(brkIter);
            }
            status = icu::U_ZERO_ERROR;
            brkIter = icu::ubrk_open(
                icu::UBRK_LINE,
                b"en_us\x00" as *const u8 as *const i8,
                0 as *const icu::UChar,
                0i32,
                &mut status,
            )
        }
        free(locale as *mut libc::c_void);
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

#[no_mangle]
pub unsafe extern "C" fn linebreak_next() -> i32 {
    if !brkIter.is_null() {
        icu::ubrk_next(brkIter)
    } else {
        findNextGraphiteBreak()
    }
}

#[no_mangle]
pub unsafe extern "C" fn get_encoding_mode_and_info(mut info: *mut i32) -> i32 {
    /* \XeTeXinputencoding "enc-name"
     *   -> name is packed in |nameoffile| as a C string, starting at [1]
     * Check if it's a built-in name; if not, try to open an ICU converter by that name
     */
    let mut err: icu::UErrorCode = icu::U_ZERO_ERROR;
    let mut cnv: *mut icu::UConverter = 0 as *mut icu::UConverter;
    *info = 0i32;
    if strcasecmp(name_of_file, b"auto\x00" as *const u8 as *const i8) == 0i32 {
        return 0i32;
    }
    if strcasecmp(name_of_file, b"utf8\x00" as *const u8 as *const i8) == 0i32 {
        return 1i32;
    }
    if strcasecmp(name_of_file, b"utf16\x00" as *const u8 as *const i8) == 0i32 {
        /* depends on host platform */
        return 3i32;
    }
    if strcasecmp(name_of_file, b"utf16be\x00" as *const u8 as *const i8) == 0i32 {
        return 2i32;
    }
    if strcasecmp(name_of_file, b"utf16le\x00" as *const u8 as *const i8) == 0i32 {
        return 3i32;
    }
    if strcasecmp(name_of_file, b"bytes\x00" as *const u8 as *const i8) == 0i32 {
        return 4i32;
    }
    /* try for an ICU converter */
    cnv = icu::ucnv_open(name_of_file, &mut err); /* ensure message starts on a new line */
    if cnv.is_null() {
        begin_diagnostic();
        print_nl('U' as i32);
        print_c_string(b"nknown encoding `\x00" as *const u8 as *const i8);
        print_c_string(name_of_file);
        print_c_string(b"\'; reading as raw bytes\x00" as *const u8 as *const i8);
        end_diagnostic(1i32 != 0);
        4i32
    } else {
        icu::ucnv_close(cnv);
        *info = maketexstring(name_of_file);
        5i32
    }
}

#[no_mangle]
pub unsafe extern "C" fn print_utf8_str(mut string: *const u8, mut len: i32) {
    loop {
        let fresh1 = len;
        len = len - 1;
        if !(fresh1 > 0i32) {
            break;
        }
        let fresh2 = string;
        string = string.offset(1);
        print_raw_char(*fresh2 as UTF16_code, true);
    }
    /* bypass utf-8 encoding done in print_char() */
}

#[no_mangle]
pub unsafe extern "C" fn print_chars(mut string: *const u16, mut len: i32) {
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

unsafe extern "C" fn load_mapping_file(
    mut s: *const i8,
    mut e: *const i8,
    mut byteMapping: i8,
) -> *mut libc::c_void {
    let mut cnv = 0 as teckit::TECkit_Converter;
    let mut buffer: *mut i8 =
        xmalloc((e.wrapping_offset_from(s) as i64 + 5i32 as i64) as size_t) as *mut i8;
    strncpy(buffer, s, e.wrapping_offset_from(s) as usize);
    *buffer.offset(e.wrapping_offset_from(s) as i64 as isize) = 0_i8;
    strcat(buffer, b".tec\x00" as *const u8 as *const i8);
    if let Some(mut map) = ttstub_input_open(buffer, TTInputFormat::MISCFONTS, 0i32) {
        let mut mappingSize: size_t = ttstub_input_get_size(&mut map);
        let mut mapping: *mut u8 = xmalloc(mappingSize) as *mut u8;
        let mut r: ssize_t =
            ttstub_input_read_exact(map.0.as_ptr(), mapping as *mut i8, mappingSize);
        if r < 0i32 as i64 || r as size_t != mappingSize {
            _tt_abort(
                b"could not read mapping file \"%s\"\x00" as *const u8 as *const i8,
                buffer,
            );
        }
        ttstub_input_close(map);
        if byteMapping as i32 != 0i32 {
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
            font_mapping_warning(buffer as *const libc::c_void, strlen(buffer) as i32, 2i32);
        /* not loadable */
        } else if get_tracing_fonts_state() > 1i32 {
            font_mapping_warning(buffer as *const libc::c_void, strlen(buffer) as i32, 0i32);
        }
    } else {
        font_mapping_warning(buffer as *const libc::c_void, strlen(buffer) as i32, 1i32);
        /* not found */
    }
    free(buffer as *mut libc::c_void);
    cnv as *mut libc::c_void
}
static mut saved_mapping_name: *mut i8 = 0 as *const i8 as *mut i8;
#[no_mangle]
pub unsafe extern "C" fn check_for_tfm_font_mapping() {
    let mut cp: *mut i8 = strstr(name_of_file, b":mapping=\x00" as *const u8 as *const i8);
    saved_mapping_name = mfree(saved_mapping_name as *mut libc::c_void) as *mut i8;
    if !cp.is_null() {
        *cp = 0_i8;
        cp = cp.offset(9);
        while *cp as i32 != 0 && *cp as i32 <= ' ' as i32 {
            cp = cp.offset(1)
        }
        if *cp != 0 {
            saved_mapping_name = xstrdup(cp)
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn load_tfm_font_mapping() -> *mut libc::c_void {
    let mut rval: *mut libc::c_void = 0 as *mut libc::c_void;
    if !saved_mapping_name.is_null() {
        rval = load_mapping_file(
            saved_mapping_name,
            saved_mapping_name.offset(strlen(saved_mapping_name) as isize),
            1_i8,
        );
        saved_mapping_name = mfree(saved_mapping_name as *mut libc::c_void) as *mut i8
    }
    rval
}
#[no_mangle]
pub unsafe extern "C" fn apply_tfm_font_mapping(mut cnv: *mut libc::c_void, mut c: i32) -> i32 {
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
#[no_mangle]
pub unsafe extern "C" fn read_double(mut s: *mut *const i8) -> f64 {
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
unsafe extern "C" fn read_tag_with_param(mut cp: *const i8, mut param: *mut i32) -> hb_tag_t {
    let mut cp2: *const i8 = 0 as *const i8;
    let mut tag: hb_tag_t = 0;
    cp2 = cp;
    while *cp2 as i32 != 0
        && *cp2 as i32 != ':' as i32
        && *cp2 as i32 != ';' as i32
        && *cp2 as i32 != ',' as i32
        && *cp2 as i32 != '=' as i32
    {
        cp2 = cp2.offset(1)
    }
    tag = hb_tag_from_string(cp, cp2.wrapping_offset_from(cp) as i64 as i32);
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
#[no_mangle]
pub unsafe extern "C" fn read_rgb_a(mut cp: *mut *const i8) -> u32 {
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
#[no_mangle]
pub unsafe extern "C" fn readCommonFeatures(
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
    let mut sep: *const i8 = 0 as *const i8;
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
        let mut s: *const i8 = 0 as *const i8;
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
unsafe extern "C" fn readFeatureNumber(
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
unsafe extern "C" fn loadOTfont(
    mut fontRef: PlatformFontRef,
    mut font: *mut XeTeXFontInst,
    mut scaled_size: Fixed,
    mut cp1: Option<&CStr>,
    mut shaperRequest: Option<ShaperRequest>,
) -> Option<TextLayoutEngine> {
    let mut current_block: u64;
    let mut rval = None;
    // ft_make_tag also works for harfbuzz tags.
    let mut script: hb_tag_t = ft_make_tag(&[0, 0, 0, 0]);
    let mut language: *mut i8 = 0 as *mut i8;
    let mut features: *mut hb_feature_t = 0 as *mut hb_feature_t;
    let mut shaper_list = CStringListBuilder::new();
    let mut nFeatures: i32 = 0i32;
    let mut nShapers: i32 = 0i32;
    let mut cp2: *const i8 = ptr::null();
    let mut cp3: *const i8 = ptr::null();
    let mut tag: hb_tag_t = 0;
    let mut rgbValue: u32 = 0xff_u32;
    let mut extend: f32 = 1.0f64 as f32;
    let mut slant: f32 = 0.0f64 as f32;
    let mut embolden: f32 = 0.0f64 as f32;
    let mut letterspace: f32 = 0.0f64 as f32;
    let mut i: i32 = 0;

    if shaperRequest == Some(ShaperRequest::OpenType)
        || shaperRequest == Some(ShaperRequest::Graphite)
    {
        match shaperRequest.expect("already know shaperRequest is Some") {
            ShaperRequest::OpenType => {
                shaper_list.push_non_null_terminated(&b"ot"[..]);
            }
            ShaperRequest::Graphite => {
                shaper_list.push_non_null_terminated(&b"graphite2"[..]);
            }
            _ => unreachable!(),
        }
        nShapers += 1
    }
    if shaperRequest == Some(ShaperRequest::Graphite) {
        let tmp_shapers = shaper_list.clone();
        /* create a default engine so we can query the font for Graphite features;
         * because of font caching, it's cheap to discard this and create the real one later */
        rval = Some(createLayoutEngine(
            fontRef,
            font,
            script,
            language,
            features,
            nFeatures,
            tmp_shapers.none_if_empty(),
            rgbValue,
            extend,
            slant,
            embolden,
            shaperRequest,
        ));
    }
    /* scan the feature string (if any) */
    if let Some(mut cp1) = cp1.map(|x| x.as_ptr()) {
        while *cp1 != 0 {
            if *cp1 as i32 == ':' as i32 || *cp1 as i32 == ';' as i32 || *cp1 as i32 == ',' as i32 {
                cp1 = cp1.offset(1)
            }
            while *cp1 as i32 == ' ' as i32 || *cp1 as i32 == '\t' as i32 {
                /* skip leading whitespace */
                cp1 = cp1.offset(1)
            }
            if *cp1 as i32 == 0i32 {
                break;
            }
            cp2 = cp1;
            while *cp2 as i32 != 0
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
                    script = hb_tag_from_string(cp3, cp2.wrapping_offset_from(cp3) as i64 as i32);
                    current_block = 13857423536159756434;
                }
            } else {
                cp3 = strstartswith(cp1, b"language\x00" as *const u8 as *const i8);
                if !cp3.is_null() {
                    if *cp3 as i32 != '=' as i32 {
                        current_block = 10622493848381539643;
                    } else {
                        cp3 = cp3.offset(1);
                        language =
                            xmalloc((cp2.wrapping_offset_from(cp3) as i64 + 1i32 as i64) as size_t)
                                as *mut i8;
                        *language.offset(cp2.wrapping_offset_from(cp3) as i64 as isize) =
                            '\u{0}' as i32 as i8;
                        memcpy(
                            language as *mut libc::c_void,
                            cp3 as *const libc::c_void,
                            cp2.wrapping_offset_from(cp3) as usize,
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
                            shaper_list.push_cstr(CStr::from_ptr(cp3));
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
                            if shaperRequest == Some(ShaperRequest::Graphite) {
                                let mut value: i32 = 0i32;
                                if readFeatureNumber(cp1, cp2, &mut tag, &mut value) as i32 != 0
                                    || rval.as_mut().map(|engine| findGraphiteFeature(
                                        engine,
                                        cp1,
                                        cp2,
                                        &mut tag,
                                        &mut value,
                                    )).unwrap_or(false)
                                {
                                    features = xrealloc(
                                        features as *mut libc::c_void,
                                        ((nFeatures + 1i32) as u64).wrapping_mul(
                                            ::std::mem::size_of::<hb_feature_t>() as u64,
                                        ),
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
                                            ),
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
                                            cp2.wrapping_offset_from(cp1) as i64 as i32,
                                        );
                                        features = xrealloc(
                                            features as *mut libc::c_void,
                                            ((nFeatures + 1i32) as u64).wrapping_mul(
                                                ::std::mem::size_of::<hb_feature_t>() as u64,
                                            ),
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
                        cp1 as *mut libc::c_void,
                        cp2.wrapping_offset_from(cp1) as i64 as i32,
                        0 as *const libc::c_void,
                        0i32,
                    );
                }
                _ => {}
            }
            cp1 = cp2
        }
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
    rval = Some(createLayoutEngine(
        fontRef,
        font,
        script,
        language,
        features,
        nFeatures,
        shaper_list.none_if_empty(),
        rgbValue,
        extend,
        slant,
        embolden,
        shaperRequest,
    ));
    native_font_type_flag = 0xfffeu32 as i32;
    rval.map(TextLayoutEngine::XeTeX)
}

unsafe extern "C" fn splitFontName(
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

struct SplitName {
    tex_internal: bool,
    name: CString,
    /// Easier: you get starts_with().
    var: Option<String>,
    feat: Option<CString>,
    index: i32,
}

impl SplitName {
    unsafe fn from_packed_name(name: *mut i8) -> Self {
        let mut var: *mut i8 = ptr::null_mut();
        let mut feat: *mut i8 = ptr::null_mut();
        let mut end: *mut i8 = ptr::null_mut();
        let mut index = 0i32;

        splitFontName(name, &mut var, &mut feat, &mut end, &mut index);

        let full_str = CStr::from_ptr(name);
        let full_bytes = full_str.to_bytes();
        let start_index = if full_bytes[0] == b'[' { 1 } else { 0 };
        let name_string =
            CString::new(&full_bytes[start_index..var.wrapping_offset_from(name) as usize])
                .unwrap();

        let mut var_out = None;
        let mut feat_out = None;

        if feat > var {
            let from = var.wrapping_offset_from(name) as usize + 1;
            let to = from + feat.wrapping_offset_from(var) as usize - 1;
            var_out = Some(CString::new(&full_bytes[from..to]).unwrap());
        }
        if end > feat {
            let from = feat.wrapping_offset_from(name) as usize + 1;
            let to = from + end.wrapping_offset_from(feat) as usize - 1;
            feat_out = Some(CString::new(&full_bytes[from..to]).unwrap());
        }

        SplitName {
            tex_internal: start_index == 1,
            name: name_string,
            // XXX: could fail
            var: var_out.map(|v| v.into_string().unwrap()),
            feat: feat_out,
            index,
        }
    }
}

/// scaled_size here is in TeX points, or is a negative integer for 'scaled_t'
#[no_mangle]
pub unsafe extern "C" fn find_native_font(
    mut uname: *mut i8,
    mut scaled_size: i32,
) -> Option<TextLayoutEngine> {
    let mut rval = None;
    let mut fontRef: PlatformFontRef = ptr::null_mut();
    let mut font: *mut XeTeXFontInst = ptr::null_mut();

    let mut name: *mut i8 = uname;
    let SplitName {
        tex_internal,
        name,
        mut var,
        feat,
        index,
    } = SplitName::from_packed_name(name);
    let feat_cstr: Option<&CStr> = feat.as_ref().map(|x| x.as_ref());

    loaded_font_mapping = 0 as *mut libc::c_void;
    loaded_font_flags = 0_i8;
    loaded_font_letter_space = 0i32;

    // the requested rendering technology for the most recent findFont
    // or 0 if no specific technology was requested
    let mut shaperRequest = None;

    // check for "[filename]" form, don't search maps in this case
    if tex_internal {
        if scaled_size < 0i32 {
            font = createFontFromFile(&name, index, 655360i64 as Fixed);
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
        font = createFontFromFile(&name, index, scaled_size);
        if !font.is_null() {
            loaded_font_design_size = D2Fix(getDesignSize(font));
            /* This is duplicated in XeTeXFontMgr::findFont! */
            if let Some(var) = var {
                if var.starts_with("/AAT") {
                    shaperRequest = Some(ShaperRequest::AAT);
                } else if var.starts_with("/OT") || var.starts_with("/ICU") {
                    shaperRequest = Some(ShaperRequest::OpenType);
                } else if var.starts_with("/GR") {
                    shaperRequest = Some(ShaperRequest::Graphite);
                }
            }
            rval = loadOTfont(
                0 as PlatformFontRef,
                font,
                scaled_size,
                feat_cstr,
                shaperRequest,
            );
            if rval.is_none() {
                deleteFont(font);
            }
            if rval.is_some() && get_tracing_fonts_state() > 0i32 {
                begin_diagnostic();
                print_nl(' ' as i32);
                print_c_string(b"-> \x00" as *const u8 as *const i8);
                print_c_string(name.as_ptr());
                end_diagnostic(0i32 != 0);
            }
        }
    } else {
        fontRef = findFontByName(&name, var.as_mut(), Fix2D(scaled_size), &mut shaperRequest);
        if !fontRef.is_null() {
            /* update name_of_file to the full name of the font, for error messages during font loading */
            let mut fullName: *const i8 = getFullName(fontRef);
            name_length = strlen(fullName) as i32;
            if let Some(feat) = &feat {
                name_length = ((name_length as usize) + feat.as_bytes().len() + 1) as _;
            }
            if let Some(var) = &var {
                name_length = ((name_length as usize) + var.as_bytes().len() + 1) as _;
            }
            free(name_of_file as *mut libc::c_void);
            name_of_file = xmalloc((name_length + 1i32) as size_t) as *mut i8;
            strcpy(name_of_file, fullName);
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
                    rval = loadOTfont(fontRef, font, scaled_size, feat_cstr, shaperRequest);
                }
                #[cfg(target_os = "macos")]
                {
                    /* decide whether to use AAT or OpenType rendering with this font */
                    if shaperRequest == Some(ShaperRequest::AAT) {
                        rval = aat::loadAATfont(fontRef, scaled_size, feat_cstr);
                        if rval.is_none() {
                            deleteFont(font);
                        }
                    } else {
                        if shaperRequest == Some(ShaperRequest::OpenType)
                            || shaperRequest == Some(ShaperRequest::Graphite)
                            || !getFontTablePtr(font, ft_make_tag(b"GSUB")).is_null()
                            || !getFontTablePtr(font, ft_make_tag(b"GPOS")).is_null()
                        {
                            rval = loadOTfont(fontRef, font, scaled_size, feat_cstr, shaperRequest)
                        }
                        /* loadOTfont failed or the above check was false */
                        if rval.is_none() {
                            rval = aat::loadAATfont(fontRef, scaled_size, feat_cstr)
                        }
                        if rval.is_none() {
                            deleteFont(font);
                        }
                    }
                }
            }
            /* append the style and feature strings, so that \show\fontID will give a full result */
            if let Some(var) = var {
                if !var.as_bytes().is_empty() {
                    strcat(name_of_file, b"/\x00" as *const u8 as *const i8);
                    strncat(name_of_file, var.as_ptr() as *const i8, var.len());
                }
            }
            if let Some(feat) = feat {
                if !feat.as_bytes().is_empty() {
                    strcat(name_of_file, b":\x00" as *const u8 as *const i8);
                    strcat(name_of_file, feat.as_ptr());
                }
            }
            name_length = strlen(name_of_file) as i32
        }
    }
    rval
}

static mut xdvBufSize: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn makeXDVGlyphArrayData(mut pNode: *mut libc::c_void) -> i32 {
    let mut cp: *mut u8 = 0 as *mut u8;
    let mut glyphIDs: *mut u16 = 0 as *mut u16;
    let mut p: *mut memory_word = pNode as *mut memory_word;
    let mut glyph_info: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut locations: *mut FixedPoint = 0 as *mut FixedPoint;
    let mut width: Fixed = 0;
    let mut glyphCount: u16 = (*p.offset(4)).b16.s0;
    let mut i: i32 = glyphCount as i32 * 10i32 + 8i32;
    if i > xdvBufSize {
        free(xdv_buffer as *mut libc::c_void);
        xdvBufSize = (i / 1024i32 + 1i32) * 1024i32;
        xdv_buffer = xmalloc(xdvBufSize as size_t) as *mut i8
    }
    glyph_info = (*p.offset(5)).ptr;
    locations = glyph_info as *mut FixedPoint;
    glyphIDs = locations.offset(glyphCount as i32 as isize) as *mut u16;
    cp = xdv_buffer as *mut u8;
    width = (*p.offset(1)).b32.s1;
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
    (cp as *mut i8).wrapping_offset_from(xdv_buffer) as i64 as i32
}
#[no_mangle]
pub unsafe extern "C" fn make_font_def(mut f: i32) -> i32 {
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
    let mut extend = 1.0f64;
    let mut slant = 0.0f64;
    let mut embolden = 0.0f64;

    let eng = get_text_layout_engine(f as usize).expect("bad native font flag in `make_font_def`");
    filename = eng.font_filename(&mut index);
    assert!(!filename.is_null());
    filenameLen = strlen(filename) as u8;
    rgba = eng.rgb_value();
    extend = eng.extend_factor();
    slant = eng.slant_factor();
    embolden = eng.embolden_factor() as f64;
    size = D2Fix(eng.point_size());
    flags = (flags as i32 | eng.get_flags(f as u32)) as u16;

    /* parameters after internal font ID:
    //  size[4]
    //  flags[2]
    //  l[1] n[l]
    //  if flags & COLORED:
    //      c[4]
     */
    fontDefLength = 4i32 + 2i32 + 1i32 + filenameLen as i32 + 4i32; /* face index */
    if *font_flags.offset(f as isize) as i32 & 0x1i32 != 0i32 {
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
    if *font_flags.offset(f as isize) as i32 & 0x1i32 != 0i32 {
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
#[no_mangle]
pub unsafe extern "C" fn apply_mapping(
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
unsafe extern "C" fn snap_zone(
    mut value: *mut scaled_t,
    mut snap_value: scaled_t,
    mut fuzz: scaled_t,
) {
    let mut difference: scaled_t = *value - snap_value;
    if difference <= fuzz && difference >= -fuzz {
        *value = snap_value
    };
}
#[no_mangle]
pub unsafe extern "C" fn get_native_char_height_depth(
    mut font: i32,
    mut ch: i32,
    mut height: *mut scaled_t,
    mut depth: *mut scaled_t,
) {
    let mut fuzz: Fixed = 0;

    let engine = get_text_layout_engine(font as usize).expect("no font found with that id");
    let gid = engine.map_char_to_glyph(ch as u32);
    let (h, d) = engine.glyph_height_depth(gid)
        .map(|(h, d)| (D2Fix(h as f64), D2Fix(d as f64)))
        .unwrap_or((0, 0));
    *height = h;
    *depth = d;

    /* snap to "known" zones for baseline, x-height, cap-height if within 4% of em-size */
    fuzz = (*font_info.offset((6i32 + *param_base.offset(font as isize)) as isize))
        .b32
        .s1
        / 25i32;
    snap_zone(depth, 0i32, fuzz);
    snap_zone(height, 0i32, fuzz);
    snap_zone(
        height,
        (*font_info.offset((5i32 + *param_base.offset(font as isize)) as isize))
            .b32
            .s1,
        fuzz,
    );
    snap_zone(
        height,
        (*font_info.offset((8i32 + *param_base.offset(font as isize)) as isize))
            .b32
            .s1,
        fuzz,
    );
}
#[no_mangle]
pub unsafe extern "C" fn getnativecharht(mut f: i32, mut c: i32) -> scaled_t {
    let mut h: scaled_t = 0;
    let mut d: scaled_t = 0;
    get_native_char_height_depth(f, c, &mut h, &mut d);
    h
}
#[no_mangle]
pub unsafe extern "C" fn getnativechardp(mut f: i32, mut c: i32) -> scaled_t {
    let mut h: scaled_t = 0;
    let mut d: scaled_t = 0;
    get_native_char_height_depth(f, c, &mut h, &mut d);
    d
}
#[no_mangle]
pub unsafe extern "C" fn get_native_char_sidebearings(
    mut font: i32,
    mut ch: i32,
    mut lsb: *mut scaled_t,
    mut rsb: *mut scaled_t,
) {
    let eng = get_text_layout_engine(font as usize)
        .expect("bad native font flag in `get_native_char_side_bearings`");
    let gid = eng.map_char_to_glyph(ch as u32);
    let (l, r) = eng.glyph_sidebearings(gid).unwrap_or((0., 0.));
    *lsb = D2Fix(l as f64);
    *rsb = D2Fix(r as f64);
}

#[no_mangle]
pub unsafe extern "C" fn get_glyph_bounds(mut font: i32, mut edge: i32, mut gid: i32) -> scaled_t {
    GlyphEdge::from_int(edge)
        .and_then(|edge| {
            /* edge codes 1,2,3,4 => L T R B */
            let mut a: f32 = 0.;
            let mut b: f32 = 0.;
            let eng = get_text_layout_engine(font as usize)
                .expect("bad native font flag in `get_glyph_bounds`");
            if edge.is_side() {
                eng.glyph_sidebearings(gid as u32)
            } else {
                eng.glyph_height_depth(gid as u32)
            }
            .map(|pair| edge.pick_from(&pair))
        })
        .map(|d| D2Fix(d as f64))
        // This means 'None' to xetex
        .unwrap_or(D2Fix(0.0))
}

#[no_mangle]
pub unsafe extern "C" fn getnativecharic(mut f: i32, mut c: i32) -> scaled_t {
    let mut lsb: scaled_t = 0;
    let mut rsb: scaled_t = 0;
    get_native_char_sidebearings(f, c, &mut lsb, &mut rsb);
    if rsb < 0i32 {
        *font_letter_space.offset(f as isize) - rsb
    } else {
        *font_letter_space.offset(f as isize)
    }
}

/* single-purpose metrics accessors */
#[no_mangle]
pub unsafe extern "C" fn getnativecharwd(mut f: i32, mut c: i32) -> scaled_t {
    let mut wd: scaled_t = 0i32;
    let eng = get_text_layout_engine(f as usize).expect("bad native font flag");
    let gid = eng.map_char_to_glyph(c as u32);
    D2Fix(eng.getGlyphWidthFromEngine(gid as u32))
}

#[no_mangle]
pub unsafe extern "C" fn real_get_native_glyph(
    mut pNode: *mut libc::c_void,
    mut index: u32,
) -> u16 {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut locations: *mut FixedPoint = (*node.offset(5)).ptr as *mut FixedPoint;
    let mut glyphIDs: *mut u16 =
        locations.offset((*node.offset(4)).b16.s0 as i32 as isize) as *mut u16;
    if index >= (*node.offset(4)).b16.s0 as u32 {
        0_u16
    } else {
        *glyphIDs.offset(index as isize)
    }
}

pub unsafe fn store_justified_native_glyphs(mut pNode: *mut libc::c_void) {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut f: u32 = (*node.offset(4)).b16.s2 as u32;
    let mut eng = get_text_layout_engine_mut(f as usize).expect("bad native font flag");
    match &mut *eng {
        TextLayoutEngine::AAT(eng) => {
            /* separate Mac-only codepath for AAT fonts, activated with LayoutRequest.justify */
            let request = LayoutRequest::from_node(node, true);
            let layout = eng.layout_text(request);
            layout.write_node(node);
            return;
        }
        TextLayoutEngine::XeTeX(eng) => {
            /* save desired width */
            let mut savedWidth: i32 = (*node.offset(1)).b32.s1;
            measure_native_node(node as *mut libc::c_void, 0i32);
            if (*node.offset(1)).b32.s1 != savedWidth {
                /* see how much adjustment is needed overall */
                let mut justAmount: f64 = Fix2D(savedWidth - (*node.offset(1)).b32.s1);
                /* apply justification to spaces (or if there are none, distribute it to all glyphs as a last resort) */
                let mut locations: *mut FixedPoint = (*node.offset(5)).ptr as *mut FixedPoint;
                let mut glyphIDs: *mut u16 =
                    locations.offset((*node.offset(4)).b16.s0 as i32 as isize) as *mut u16;
                let mut glyphCount: i32 = (*node.offset(4)).b16.s0 as i32;
                let mut spaceCount: i32 = 0i32;
                let mut i: i32 = 0;
                let mut spaceGlyph: i32 = map_char_to_glyph(f as i32, ' ' as i32);
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
                (*node.offset(1)).b32.s1 = savedWidth
            };
        }
    }
}

pub unsafe fn measure_native_node(mut pNode: *mut libc::c_void, mut use_glyph_metrics: i32) {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut txtLen: i32 = (*node.offset(4)).b16.s1 as i32;
    let mut txtPtr: *mut u16 = node.offset(6) as *mut u16;
    let mut f: u32 = (*node.offset(4)).b16.s2 as u32;

    let mut eng =
        get_text_layout_engine_mut(f as usize).expect("bad native font flag in `measure_native_node`");
    let request = LayoutRequest::from_node(node, false);
    let layout = eng.layout_text(request);
    layout.write_node(node);

    if use_glyph_metrics == 0i32 || (*node.offset(4)).b16.s0 as i32 == 0i32 {
        /* for efficiency, height and depth are the font's ascent/descent,
        not true values based on the actual content of the word,
        unless use_glyph_metrics is non-zero */
        (*node.offset(3)).b32.s1 = *height_base.offset(f as isize);
        (*node.offset(2)).b32.s1 = *depth_base.offset(f as isize)
    } else {
        /* this iterates over the glyph data whether it comes from AAT or OT layout */
        let mut locations_0: *mut FixedPoint = (*node.offset(5)).ptr as *mut FixedPoint; /* NB negative is upwards in locations[].y! */
        let mut glyphIDs_0: *mut u16 =
            locations_0.offset((*node.offset(4)).b16.s0 as i32 as isize) as *mut u16;
        let mut yMin: f32 = 65536.0f64 as f32;
        let mut yMax: f32 = -65536.0f64 as f32;
        let mut i_2: i32 = 0;
        i_2 = 0i32;
        while i_2 < (*node.offset(4)).b16.s0 as i32 {
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
                if let Some(bb) = eng.glyph_bbox(*glyphIDs_0.offset(i_2 as isize) as u32) {
                    bbox = bb;
                    cacheGlyphBBox(f as u16, *glyphIDs_0.offset(i_2 as isize), &mut bbox);
                }
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
        (*node.offset(3)).b32.s1 = D2Fix(yMax as f64);
        (*node.offset(2)).b32.s1 = -D2Fix(yMin as f64)
    };
}
#[no_mangle]
pub unsafe extern "C" fn real_get_native_italic_correction(mut pNode: *mut libc::c_void) -> Fixed {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut f: u32 = (*node.offset(4)).b16.s2 as u32;
    let mut n: u32 = (*node.offset(4)).b16.s0 as u32;
    if n > 0_u32 {
        let mut locations: *mut FixedPoint = (*node.offset(5)).ptr as *mut FixedPoint;
        let mut glyphIDs: *mut u16 = locations.offset(n as isize) as *mut u16;
        get_text_layout_engine(f as usize)
            .and_then(|eng| {
                let gid = *glyphIDs.offset(n.wrapping_sub(1i32 as libc::c_uint) as isize) as u32;
                let lspace = *font_letter_space.offset(f as isize);
                eng.glyph_ital_correction(gid.into())
                    .map(D2Fix)
                    .map(|x| x + lspace)
            })
            .unwrap_or(0) // XXX: Not many functions return 0 instead of panicking. Why this one?
    } else {
        0i32
    }
}

#[no_mangle]
pub unsafe extern "C" fn real_get_native_glyph_italic_correction(
    mut pNode: *mut libc::c_void,
) -> Fixed {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut gid: u16 = (*node.offset(4)).b16.s1;
    let mut f: u32 = (*node.offset(4)).b16.s2 as u32;
    get_text_layout_engine(f as usize)
        .and_then(|eng| eng.glyph_ital_correction(gid.into()))
        .map(D2Fix)
        .unwrap_or(0) // XXX: Apparently the matched-none case was "can't happen"
}

pub unsafe fn measure_native_glyph(mut pNode: *mut libc::c_void, mut use_glyph_metrics: i32) {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut gid: u16 = (*node.offset(4)).b16.s1;
    let mut f: u32 = (*node.offset(4)).b16.s2 as u32;
    let eng =
        get_text_layout_engine(f as usize).expect("bad native font flag in `measure_native_glyph`");

    (*node.offset(1)).b32.s1 = D2Fix(eng.glyph_width(gid as u32));
    if use_glyph_metrics != 0 {
        let (ht, dp) = eng.glyph_height_depth(gid as u32).unwrap_or((0., 0.));
        (*node.offset(3)).b32.s1 = D2Fix(ht as f64);
        (*node.offset(2)).b32.s1 = D2Fix(dp as f64)
    } else {
        (*node.offset(3)).b32.s1 = *height_base.offset(f as isize);
        (*node.offset(2)).b32.s1 = *depth_base.offset(f as isize)
    };
}

pub unsafe fn map_char_to_glyph(mut font: i32, mut ch: i32) -> i32 {
    if ch > 0x10ffffi32 || ch >= 0xd800i32 && ch <= 0xdfffi32 {
        return 0i32;
    }
    let eng =
        get_text_layout_engine(font as usize).expect("bad native font flag in `map_char_to_glyph`");
    eng.map_char_to_glyph(ch as u32) as i32
}

#[no_mangle]
pub unsafe fn map_glyph_to_index(mut font: i32) -> i32 {
    let eng =
        get_text_layout_engine(font as usize).expect("map_glyph_to_index had invalid font number");
    // glyph name is at name_of_file. I guess it was scanned as a filename?
    eng.map_glyph_to_index(name_of_file)
}

pub fn D2Fix(mut d: f64) -> Fixed {
    let rval: Fixed = (d * 65536.0f64 + 0.5f64) as i32;
    rval
}

pub fn Fix2D(mut f: Fixed) -> f64 {
    f as f64 / 65536.
}

pub unsafe fn print_glyph_name(mut font: i32, mut gid: i32) {
    let mut s: *const i8 = 0 as *const i8;
    let mut len: i32 = 0i32;
    let engine = get_text_layout_engine(font as usize).expect("should have already checked");
    s = engine.glyph_name(gid as u16, &mut len);
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
#[no_mangle]
pub unsafe extern "C" fn real_get_native_word_cp(
    mut pNode: *mut libc::c_void,
    mut side: i32,
) -> i32 {
    let mut node: *mut memory_word = pNode as *mut memory_word;
    let mut locations: *mut FixedPoint = (*node.offset(5)).ptr as *mut FixedPoint;
    let mut glyphIDs: *mut u16 =
        locations.offset((*node.offset(4)).b16.s0 as i32 as isize) as *mut u16;
    let mut glyphCount: u16 = (*node.offset(4)).b16.s0;
    let mut f: i32 = (*node.offset(4)).b16.s2 as i32;
    let mut actual_glyph: u16 = 0;
    if glyphCount as i32 == 0i32 {
        return 0i32;
    }
    match side {
        0 => {
            actual_glyph = *glyphIDs
            // we should not reach this point
        }
        1 => actual_glyph = *glyphIDs.offset((glyphCount as i32 - 1i32) as isize),
        _ => unreachable!(),
    }
    get_cp_code(f, actual_glyph as u32, side)
}
