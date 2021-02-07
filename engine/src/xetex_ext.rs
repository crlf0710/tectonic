#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::c_pointer_to_str;
use crate::t_print_nl;
use std::ffi::CString;

use crate::node::{Glyph, NativeWord};
use crate::strstartswith;
use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::text_layout_engine::{LayoutRequest, TextLayoutEngine};
use crate::xetex_consts::{Side, UnicodeMode};
use crate::xetex_font_manager::ShaperRequest;
use bridge::{ttstub_input_get_size, InFile, TTInputFormat};
use std::io::Read;
use std::ptr;

#[cfg(target_os = "macos")]
use super::xetex_aatfont as aat;
#[cfg(target_os = "macos")]
use crate::cf_prelude::{
    kCFNumberFloatType, kCTFontAttributeName, kCTForegroundColorAttributeName,
    kCTVerticalFormsAttributeName, CFDictionaryGetValue, CFDictionaryRef, CFNumberGetValue,
    CFNumberRef, CFNumberType, CFRelease, CFTypeRef, CGColorGetComponents, CGColorRef,
    CTFontGetMatrix, CTFontGetSize, CTFontRef,
};
use crate::xetex_ini::{
    loaded_font_design_size, loaded_font_flags, loaded_font_letter_space, loaded_font_mapping,
    name_of_font, DEPTH_BASE, FONT_FLAGS, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_LETTER_SPACE,
    HEIGHT_BASE, PARAM_BASE,
};
use crate::xetex_scaledmath::xn_over_d;
use crate::xetex_texmfmp::{gettexstring, maketexstring};
use crate::xetex_xetex0::{
    diagnostic, font_feature_warning, font_mapping_warning, get_tracing_fonts_state,
};

pub(crate) use crate::xetex_scaledmath::Scaled;

use crate::xetex_layout_interface::*;
use harfbuzz_sys::{hb_feature_t, hb_tag_from_string, hb_tag_t};

pub(crate) enum Font {
    None,
    Native(NativeFont),
}

impl Font {
    pub(crate) fn as_native(&self) -> &NativeFont {
        match self {
            Font::Native(nf) => nf,
            _ => panic!("Not native font"),
        }
    }
}

pub(crate) enum NativeFont {
    #[cfg(target_os = "macos")]
    Aat(crate::cf_prelude::CFMutableDictionaryRef),
    Otgr(Box<XeTeXLayoutEngine>),
}
impl Drop for NativeFont {
    fn drop(&mut self) {
        #[cfg(target_os = "macos")]
        match self {
            Self::Aat(engine) => unsafe { CFRelease(*engine as CFDictionaryRef as CFTypeRef) },
            _ => {}
        }
    }
}

impl NativeFont {
    pub(crate) fn flag(&self) -> u32 {
        match self {
            #[cfg(target_os = "macos")]
            Self::Aat(_) => 0xFFFF,
            Self::Otgr(_) => 0xFFFE,
        }
    }
}

use NativeFont::*;

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
static mut brkLocaleStrNum: i32 = 0;

/* info for each glyph is location (FixedPoint) + glyph ID (u16) */
/* glyph ID field in a glyph_node */
/* For Unicode encoding form interpretation... */
pub(crate) unsafe fn linebreak_start(f: usize, localeStrNum: i32, text: &[u16]) {
    let mut status: icu::UErrorCode = icu::U_ZERO_ERROR;
    let locale = gettexstring(localeStrNum);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(engine)) if locale == "G" => {
            if initGraphiteBreaking(engine, text) {
                /* user asked for Graphite line breaking and the font supports it */
                return;
            }
        }
        _ => {}
    }
    if localeStrNum != brkLocaleStrNum && !brkIter.is_null() {
        icu::ubrk_close(brkIter);
        brkIter = ptr::null_mut();
    }
    if brkIter.is_null() {
        let name = CString::new(locale.as_str()).unwrap();
        brkIter = icu::ubrk_open(icu::UBRK_LINE, name.as_ptr(), ptr::null(), 0, &mut status);
        if status as i32 > icu::U_ZERO_ERROR as i32 {
            diagnostic(true, || {
                t_print_nl!("Error {} creating linebreak iterator for locale `{}\'; trying default locale `en_us\'.", status as i32, locale);
            });
            if !brkIter.is_null() {
                icu::ubrk_close(brkIter);
            }
            status = icu::U_ZERO_ERROR;
            brkIter = icu::ubrk_open(
                icu::UBRK_LINE,
                b"en_us\x00".as_ptr() as *const i8,
                ptr::null(),
                0,
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
    icu::ubrk_setText(
        brkIter,
        text.as_ptr() as *const icu::UChar,
        text.len() as _,
        &mut status,
    );
}

pub(crate) unsafe fn linebreak_next() -> i32 {
    if !brkIter.is_null() {
        icu::ubrk_next(brkIter)
    } else {
        findNextGraphiteBreak()
    }
}

pub(crate) unsafe fn get_encoding_mode_and_info(name: &str, info: *mut i32) -> UnicodeMode {
    /* \XeTeXinputencoding "enc-name"
     *   -> name is packed in |nameoffile| as a C string, starting at [1]
     * Check if it's a built-in name; if not, try to open an ICU converter by that name
     */
    let mut err: icu::UErrorCode = icu::U_ZERO_ERROR;
    *info = 0;
    match name.to_lowercase().as_str() {
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
    let cname = CString::new(name).unwrap();
    let cnv = icu::ucnv_open(cname.as_ptr(), &mut err); /* ensure message starts on a new line */
    if cnv.is_null() {
        diagnostic(true, || {
            t_print_nl!("Unknown encoding `{}\'; reading as raw bytes", name);
        });
        UnicodeMode::Raw
    } else {
        icu::ucnv_close(cnv);
        *info = maketexstring(name);
        UnicodeMode::ICUMapping
    }
}

unsafe fn load_mapping_file(s: &str, byteMapping: i8) -> *mut libc::c_void {
    let mut cnv = 0 as teckit::TECkit_Converter;
    let buffer = s.to_string() + ".tec";
    if let Some(mut map) = InFile::open(&buffer, TTInputFormat::MISCFONTS, 0) {
        let mapping_size = ttstub_input_get_size(&mut map) as usize;
        let mut mapping = vec![0_u8; mapping_size];
        if map.read_exact(mapping.as_mut_slice()).is_err() {
            abort!("could not read mapping file \"{}\"", buffer);
        }
        if byteMapping != 0 {
            teckit::TECkit_CreateConverter(
                mapping.as_mut_ptr(),
                mapping_size as u32,
                0,
                4,
                1,
                &mut cnv,
            );
        } else {
            teckit::TECkit_CreateConverter(
                mapping.as_mut_ptr(),
                mapping_size as u32,
                1,
                4,
                4,
                &mut cnv,
            );
        }
        if cnv.is_null() {
            /* tracing */
            font_mapping_warning(&buffer, 2);
        /* not loadable */
        } else if get_tracing_fonts_state() > 1 {
            font_mapping_warning(&buffer, 0);
        }
    } else {
        font_mapping_warning(&buffer, 1);
        /* not found */
    }
    cnv as *mut libc::c_void
}
static mut saved_mapping_name: String = String::new();
pub(crate) unsafe fn check_for_tfm_font_mapping() {
    let cp = name_of_font.find(":mapping=");

    saved_mapping_name = String::new();

    if let Some(cp) = cp {
        let (a, b) = name_of_font.split_at(cp);
        let mut cp = &b.as_bytes()[9..];
        while !cp.is_empty() && cp[0] <= b' ' {
            cp = &cp[1..];
        }
        if !cp.is_empty() {
            saved_mapping_name = String::from_utf8_lossy(cp).to_string();
        }
        name_of_font = String::from(a);
    }
}
pub(crate) unsafe fn load_tfm_font_mapping() -> *mut libc::c_void {
    if !saved_mapping_name.is_empty() {
        let rval = load_mapping_file(&saved_mapping_name, 1_i8);
        saved_mapping_name = String::new();
        rval
    } else {
        ptr::null_mut()
    }
}
pub(crate) unsafe fn apply_tfm_font_mapping(cnv: *mut libc::c_void, c: i32) -> i32 {
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
        1,
    );
    if outUsed < 1 {
        0
    } else {
        out[0] as i32
    }
}
pub(crate) fn read_double(s: &mut &[u8]) -> f64 {
    let mut neg = false;
    let mut val = 0_f64;
    let mut cp = *s;
    while b" \t".contains(&cp[0]) {
        cp = &cp[1..];
    }
    if cp[0] == b'-' {
        neg = true;
        cp = &cp[1..];
    } else if cp[0] == b'+' {
        cp = &cp[1..];
    }
    while (b'0'..=b'9').contains(&cp[0]) {
        val = val * 10.0f64 + cp[0] as f64 - '0' as i32 as f64;
        cp = &cp[1..];
    }
    if cp[0] == b'.' {
        let mut dec = 10_f64;
        cp = &cp[1..];
        while (b'0'..=b'9').contains(&cp[0]) {
            val += (cp[0] as i32 - '0' as i32) as f64 / dec;
            cp = &cp[1..];
            dec *= 10.;
        }
    }
    *s = cp;
    if neg {
        -val
    } else {
        val
    }
}
unsafe fn read_tag_with_param(cp: &[u8], param: *mut i32) -> hb_tag_t {
    let mut n = 0;
    while n != cp.len() && !b":;,=".contains(&cp[n]) {
        n += 1;
    }
    let (tag, mut cp) = cp.split_at(n);
    let tag = hb_tag_from_string(tag.as_ptr() as *const i8, tag.len() as _);
    if cp[0] == b'=' {
        let mut neg: i32 = 0;
        cp = &cp[1..];
        if cp[0] == b'-' {
            neg += 1;
            cp = &cp[1..];
        }
        while (b'0'..=b'9').contains(&cp[0]) {
            *param = *param * 10 + cp[0] as i32 - '0' as i32;
            cp = &cp[1..];
        }
        if neg != 0 {
            *param = -*param
        }
    }
    tag
}
pub(crate) fn read_rgb_a(cp: &mut &[u8]) -> u32 {
    let mut rgbValue: u32 = 0_u32;
    let mut alpha: u32 = 0_u32;
    for _ in 0..6 {
        match cp[0] {
            b'0'..=b'9' => {
                rgbValue = (rgbValue << 4) + (cp[0] as u32) - ('0' as u32);
            }
            b'A'..=b'F' => {
                rgbValue = (rgbValue << 4) + (cp[0] as u32) - ('A' as u32) + 10;
            }
            b'a'..=b'f' => {
                rgbValue = (rgbValue << 4) + (cp[0] as u32) - ('a' as u32) + 10;
            }
            _ => return 0xff,
        };
        *cp = &cp[1..];
    }
    rgbValue <<= 8;
    let mut i = 0;
    while i < 2 {
        match cp[0] {
            b'0'..=b'9' => alpha = (alpha << 4) + (cp[0] as u32) - ('0' as u32),
            b'A'..=b'F' => {
                alpha = (alpha << 4) + (cp[0] as u32) - ('A' as u32) + 10;
            }
            b'a'..=b'f' => {
                alpha = (alpha << 4) + (cp[0] as u32) - ('a' as u32) + 10;
            }
            _ => break,
        };
        *cp = &cp[1..];
        i += 1;
    }
    if i == 2 {
        rgbValue = (rgbValue as u32).wrapping_add(alpha) as u32
    } else {
        rgbValue = (rgbValue as u32).wrapping_add(0xff_u32) as u32
    }
    rgbValue
}
pub(crate) unsafe fn readCommonFeatures(
    feat: &[u8],
    extend: *mut f32,
    slant: *mut f32,
    embolden: *mut f32,
    letterspace: *mut f32,
    rgbValue: *mut u32,
) -> i32
// returns 1 to go to next_option, -1 for bad_option, 0 to continue
{
    if let Some(sep) = strstartswith(feat, b"mapping") {
        if sep[0] != b'=' {
            return -1;
        }
        loaded_font_mapping = load_mapping_file(std::str::from_utf8(&sep[1..]).unwrap(), 0);
        1
    } else if let Some(sep) = strstartswith(feat, b"extend") {
        if sep[0] != b'=' {
            return -1;
        }
        *extend = read_double(&mut &sep[1..]) as f32;
        1
    } else if let Some(sep) = strstartswith(feat, b"slant") {
        if sep[0] != b'=' {
            return -1;
        }
        *slant = read_double(&mut &sep[1..]) as f32;
        1
    } else if let Some(sep) = strstartswith(feat, b"embolden") {
        if sep[0] != b'=' {
            return -1;
        }
        *embolden = read_double(&mut &sep[1..]) as f32;
        1
    } else if let Some(sep) = strstartswith(feat, b"letterspace") {
        if sep[0] != b'=' {
            return -1;
        }
        *letterspace = read_double(&mut &sep[1..]) as f32;
        1
    } else if let Some(mut sep) = strstartswith(feat, b"color") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        let seplen = sep.len();
        *rgbValue = read_rgb_a(&mut sep);
        if sep.len() == seplen - 6 || sep.len() == seplen - 8 {
            loaded_font_flags = (loaded_font_flags as i32 | 0x1) as i8
        } else {
            return -1;
        }
        1
    } else {
        0
    }
}
unsafe fn readFeatureNumber(mut s: &[u8]) -> Option<(hb_tag_t, i32)>
/* s...e is a "id=setting" string; */ {
    let mut f = 0;
    let mut v = 0;
    if !(b'0'..=b'9').contains(&s[0]) {
        return None;
    }
    while (b'0'..=b'9').contains(&s[0]) {
        f = f * 10 + (s[0] as u32) - ('0' as u32);
        s = &s[1..];
    }
    while b" \t".contains(&s[0]) {
        s = &s[1..];
    }
    if s[0] != b'=' {
        /* no setting was specified */
        return None;
    } /* NULL-terminated array */
    s = &s[1..];
    if !(b'0'..=b'9').contains(&s[0]) {
        return None;
    }
    while (b'0'..=b'9').contains(&s[0]) {
        v = v * 10 + (s[0] as i32) - ('0' as i32);
        s = &s[1..];
    }
    while b" \t".contains(&s[0]) {
        s = &s[1..];
    }
    if !s.is_empty() {
        return None;
    }
    Some((f, v))
}

use crate::xetex_layout_interface::XeTeXFont;
unsafe fn loadOTfont(
    fontRef: PlatformFontRef,
    font: Box<XeTeXFont>,
    scaled_size: Scaled,
    mut cp1: &[u8],
    shaperRequest: Option<ShaperRequest>,
) -> Option<NativeFont> {
    let mut font = Some(font);
    let mut script = 0;
    let mut shaper_list = CStringListBuilder::new();
    let mut rgbValue: u32 = 0xff_u32;
    let mut extend: f32 = 1.;
    let mut slant: f32 = 0.;
    let mut embolden: f32 = 0.;
    let mut letterspace: f32 = 0.;

    match shaperRequest {
        Some(ShaperRequest::OpenType) => {
            shaper_list.push_non_null_terminated(&b"ot"[..]);
        }
        Some(ShaperRequest::Graphite) => {
            shaper_list.push_non_null_terminated(&b"graphite2"[..]);
        }
        _ => {}
    }
    let engine = if shaperRequest == Some(ShaperRequest::Graphite) {
        let tmp_shapers = shaper_list.clone();
        /* create a default engine so we can query the font for Graphite features;
         * because of font caching, it's cheap to discard this and create the real one later */
        Some(XeTeXLayoutEngine::create(
            fontRef,
            font.take().unwrap(),
            script,
            String::new(),
            Vec::new(),
            tmp_shapers.none_if_empty(),
            rgbValue,
            extend,
            slant,
            embolden,
            shaperRequest,
        ))
    } else {
        None
    };
    let mut language = String::new();
    let mut features = Vec::new();
    /* scan the feature string (if any) */
    while !cp1.is_empty() {
        if b":;,".contains(&cp1[0]) {
            cp1 = &cp1[1..];
        }
        while !cp1.is_empty() && b" \t".contains(&cp1[0]) {
            /* skip leading whitespace */
            cp1 = &cp1[1..];
        }
        if cp1.is_empty() {
            break;
        }
        let mut n = 0;
        while n != cp1.len() && !b":;,".contains(&cp1[n]) {
            n += 1;
        }
        let (feat, cp2) = cp1.split_at(n);
        if let Some(mut cp3) = strstartswith(feat, b"script") {
            if cp3[0] != b'=' {
                font_feature_warning(feat, &[]);
            } else {
                cp3 = &cp3[1..];
                script = hb_tag_from_string(cp3.as_ptr() as *const i8, cp3.len() as _);
            }
        } else if let Some(mut cp3) = strstartswith(feat, b"language") {
            if cp3[0] != b'=' {
                font_feature_warning(feat, &[]);
            } else {
                cp3 = &cp3[1..];
                language = std::str::from_utf8(&cp3).unwrap().to_string();
            }
        } else if let Some(mut cp3) = strstartswith(feat, b"shaper") {
            if cp3[0] != b'=' {
                font_feature_warning(feat, &[]);
            } else {
                cp3 = &cp3[1..];
                shaper_list.push_slice(cp3);
            }
        } else {
            match readCommonFeatures(
                feat,
                &mut extend,
                &mut slant,
                &mut embolden,
                &mut letterspace,
                &mut rgbValue,
            ) {
                1 => {}
                -1 => font_feature_warning(feat, &[]),
                _ => {
                    let mut flag = false;
                    if shaperRequest == Some(ShaperRequest::Graphite) {
                        if let Some((tag, value)) = readFeatureNumber(feat)
                            .or_else(|| findGraphiteFeature(engine.as_ref().unwrap(), feat))
                        {
                            features.push(hb_feature_t {
                                tag,
                                value: value as u32,
                                start: 0,
                                end: -1_i32 as u32,
                            });
                            flag = true;
                        }
                    }
                    if !flag {
                        if feat[0] == b'+' {
                            let mut param = 0;
                            let tag = read_tag_with_param(&cp1[1..], &mut param);
                            let start = 0;
                            let end = -1_i32 as u32;
                            // for backward compatibility with pre-0.9999 where feature
                            // indices started from 0
                            if param >= 0 {
                                param += 1
                            }
                            let value = param as u32;
                            features.push(hb_feature_t {
                                tag,
                                value,
                                start,
                                end,
                            });
                        } else if feat[0] == b'-' {
                            let feat = &feat[1..];
                            let tag =
                                hb_tag_from_string(feat.as_ptr() as *const i8, feat.len() as _);
                            features.push(hb_feature_t {
                                tag,
                                start: 0,
                                end: -1_i32 as u32,
                                value: 0,
                            });
                        } else if feat.starts_with(b"vertical") {
                            let mut n = feat.len();
                            if b";:".contains(&cp2[0]) {
                                n -= 1;
                            }
                            while n != 0 || b" \t".contains(&feat[n]) {
                                n -= 1;
                            }
                            if n != 0 {
                                // TODO: check
                                n += 1;
                            }
                            if n == 8 {
                                loaded_font_flags = (loaded_font_flags as i32 | 0x2) as i8;
                            } else {
                                font_feature_warning(feat, &[]);
                            }
                        } else {
                            font_feature_warning(feat, &[]);
                        }
                    }
                }
            }
        }
        // next option
        cp1 = cp2;
    }
    if embolden as f64 != 0. {
        embolden = (embolden as f64 * f64::from(scaled_size) / 100.) as f32
    }
    if letterspace as f64 != 0. {
        loaded_font_letter_space =
            Scaled((letterspace as f64 / 100. * scaled_size.0 as f64) as i32);
    }
    if loaded_font_flags as i32 & 0x1 == 0 {
        rgbValue = 0xff;
    }
    let mut font = if let Some(engine) = engine {
        engine.release()
    } else {
        font.unwrap()
    };
    if loaded_font_flags as i32 & 0x2 != 0 {
        setFontLayoutDir(&mut font, 1);
    }
    if let Some(engine) = Some(XeTeXLayoutEngine::create(
        fontRef,
        font,
        script,
        language,
        features,
        shaper_list.none_if_empty(),
        rgbValue,
        extend,
        slant,
        embolden,
        shaperRequest,
    )) {
        Some(Otgr(engine))
    } else {
        None
    }
}

fn splitFontName(name_str: &str) -> (String, String, String, u32) {
    let slice = name_str.as_bytes();
    let mut name = 0;
    let mut var = None;
    let mut feat = None;
    let mut index = 0;
    let end;
    if slice[name] == b'[' {
        let mut withinFileName = true;
        name += 1;
        while name != slice.len() {
            if withinFileName && slice[name] == b']' {
                withinFileName = false;
                if var.is_none() {
                    var = Some(name);
                }
            } else if slice[name] == b':' {
                if withinFileName && var.is_none() {
                    var = Some(name);
                    name += 1;
                    while (b'0'..=b'9').contains(&slice[name]) {
                        index = index * 10 + slice[name] as u32 - '0' as u32;
                        name += 1;
                    }
                    name -= 1;
                } else if !withinFileName && feat.is_none() {
                    feat = Some(name);
                }
            }
            name += 1;
        }
        end = name;
    } else {
        while name != slice.len() {
            if slice[name] == b'/' && var.is_none() && feat.is_none() {
                var = Some(name);
            } else if slice[name] == b':' && feat.is_none() {
                feat = Some(name);
            }
            name += 1;
        }
        end = name;
    }
    let feat = feat.unwrap_or(name);
    let var = var.unwrap_or(feat);
    let nameString = String::from(&name_str[..var]);
    let varString = if feat > var {
        String::from(&name_str[var + 1..feat])
    } else {
        String::new()
    };
    let featString = if end > feat {
        String::from(&name_str[feat + 1..end])
    } else {
        String::new()
    };
    (nameString, varString, featString, index)
}
pub(crate) unsafe fn find_native_font(uname: &str, mut scaled_size: Scaled) -> Option<NativeFont> {
    /* scaled_size here is in TeX points, or is a negative integer for 'Scaled' */
    let mut rval = None;
    let name = uname;

    loaded_font_mapping = ptr::null_mut();
    loaded_font_flags = 0_i8;
    loaded_font_letter_space = Scaled::ZERO;

    // the requested rendering technology for the most recent findFont
    // or 0 if no specific technology was requested
    let mut shaperRequest = None;

    let (nameString, mut varString, featString, index) = splitFontName(name);
    // check for "[filename]" form, don't search maps in this case
    if nameString.as_bytes()[0] == b'[' {
        if scaled_size < Scaled::ZERO {
            if let Some(font) = createFontFromFile(&nameString[1..], index, Scaled(655360)) {
                let dsize = font.get_design_size().into();
                if scaled_size == Scaled(-1000) {
                    scaled_size = dsize
                } else {
                    scaled_size = xn_over_d(dsize, -scaled_size, 1000).0
                }
            }
        }
        if let Some(font) = createFontFromFile(&nameString[1..], index, scaled_size) {
            loaded_font_design_size = font.get_design_size().into();
            /* This is duplicated in XeTeXFontMgr::findFont! */
            if !varString.is_empty() {
                if varString.starts_with("/AAT") {
                    shaperRequest = Some(ShaperRequest::AAT);
                } else if varString.starts_with("/OT") || varString.starts_with("/ICU") {
                    shaperRequest = Some(ShaperRequest::OpenType);
                } else if varString.starts_with("/GR") {
                    shaperRequest = Some(ShaperRequest::Graphite);
                }
            }
            rval = loadOTfont(
                0 as PlatformFontRef,
                font,
                scaled_size,
                featString.as_bytes(),
                shaperRequest,
            );
            if rval.is_some() && get_tracing_fonts_state() > 0 {
                diagnostic(false, || {
                    t_print_nl!(" -> {}", &nameString[1..]);
                });
            }
        }
    } else {
        let fontRef = findFontByName(
            &nameString,
            &mut varString,
            scaled_size.into(),
            &mut shaperRequest,
        );
        if !fontRef.is_null() {
            /* update name_of_font to the full name of the font, for error messages during font loading */
            name_of_font = getFullName(fontRef);
            if scaled_size < Scaled::ZERO {
                if let Some(font) = createFont(fontRef, scaled_size) {
                    let dsize_0 = font.get_design_size().into();
                    if scaled_size == Scaled(-1000) {
                        scaled_size = dsize_0
                    } else {
                        scaled_size = xn_over_d(dsize_0, -scaled_size, 1000).0
                    }
                }
            }
            if let Some(font) = createFont(fontRef, scaled_size) {
                #[cfg(not(target_os = "macos"))]
                {
                    rval = loadOTfont(
                        fontRef,
                        font,
                        scaled_size,
                        featString.as_bytes(),
                        shaperRequest,
                    );
                }
                #[cfg(target_os = "macos")]
                {
                    /* decide whether to use AAT or OpenType rendering with this font */
                    if shaperRequest == Some(ShaperRequest::AAT) {
                        rval = aat::loadAATfont(fontRef, scaled_size, featString.as_bytes());
                    } else {
                        if shaperRequest == Some(ShaperRequest::OpenType)
                            || shaperRequest == Some(ShaperRequest::Graphite)
                            || !getFontTablePtr(&font, u32::from_be_bytes([b'G', b'S', b'U', b'B']))
                                .is_null()
                            || !getFontTablePtr(&font, u32::from_be_bytes([b'G', b'P', b'O', b'S']))
                                .is_null()
                        {
                            rval = loadOTfont(
                                fontRef,
                                font,
                                scaled_size,
                                featString.as_bytes(),
                                shaperRequest,
                            )
                        }
                        /* loadOTfont failed or the above check was false */
                        if rval.is_none() {
                            rval = aat::loadAATfont(fontRef, scaled_size, featString.as_bytes())
                        }
                    }
                }
            }
            /* append the style and feature strings, so that \show\fontID will give a full result */
            if !varString.is_empty() {
                name_of_font.push('/');
                name_of_font.push_str(&varString);
            }
            if !featString.is_empty() {
                name_of_font.push(':');
                name_of_font.push_str(&featString);
            }
        }
    }
    rval
}
pub(crate) unsafe fn ot_get_font_metrics(
    engine: &XeTeXLayoutEngine,
) -> (Scaled, Scaled, Scaled, Scaled, Scaled) {
    let (a, d) = engine.ascent_and_descent();
    let ascent = (a as f64).into();
    let descent = (d as f64).into();
    let slant = (f64::from(getSlant(engine.get_font())) * engine.get_extend_factor() as f64
        + engine.get_slant_factor() as f64)
        .into();
    /* get cap and x height from OS/2 table */
    let (a, d) = engine.cap_and_x_height();
    let mut capheight = (a as f64).into();
    let mut xheight = (d as f64).into();
    /* fallback in case the font does not have OS/2 table */
    if xheight == Scaled::ZERO {
        let glyphID = engine.map_char_to_glyph('x') as i32;
        xheight = if glyphID != 0 {
            let (a, _) = engine.glyph_height_depth(glyphID as u32).unwrap();
            (a as f64).into()
        } else {
            ascent / 2
            /* arbitrary figure if there's no 'x' in the font */
        };
    }
    if capheight == Scaled::ZERO {
        let glyphID_0 = engine.map_char_to_glyph('X') as i32;
        capheight = if glyphID_0 != 0 {
            let (a, _) = engine.glyph_height_depth(glyphID_0 as u32).unwrap();
            (a as f64).into()
        } else {
            ascent
            /* arbitrary figure if there's no 'X' in the font */
        };
    };
    (ascent, descent, xheight, capheight, slant)
}
pub(crate) unsafe fn ot_font_get(what: i32, engine: &XeTeXLayoutEngine) -> i32 {
    let fontInst = engine.get_font();
    match what {
        1 => return countGlyphs(fontInst) as i32,
        8 => {
            /* ie Graphite features */
            return countGraphiteFeatures(engine) as i32;
        }
        16 => return countScripts(fontInst) as i32,
        _ => {}
    }
    0
}
pub(crate) unsafe fn ot_font_get_1(what: i32, engine: &XeTeXLayoutEngine, param: i32) -> i32 {
    let fontInst = engine.get_font();
    match what {
        17 => return countLanguages(fontInst, param as hb_tag_t) as i32,
        19 => return getIndScript(fontInst, param as u32) as i32,
        9 => {
            /* for graphite fonts...*/
            return getGraphiteFeatureCode(engine, param as u32) as i32;
        }
        11 => return 1,
        12 => return countGraphiteFeatureSettings(engine, param as u32) as i32,
        _ => {}
    }
    0
}
pub(crate) unsafe fn ot_font_get_2(
    what: i32,
    engine: &XeTeXLayoutEngine,
    param1: i32,
    param2: i32,
) -> i32 {
    let fontInst = engine.get_font();
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
    0
}
pub(crate) unsafe fn ot_font_get_3(
    what: i32,
    engine: &XeTeXLayoutEngine,
    param1: i32,
    param2: i32,
    param3: i32,
) -> i32 {
    let fontInst = engine.get_font();
    match what {
        21 => getIndFeature(
            fontInst,
            param1 as hb_tag_t,
            param2 as hb_tag_t,
            param3 as u32,
        ) as i32,
        _ => 0,
    }
}
pub(crate) unsafe fn gr_get_font_name(
    what: i32,
    engine: &XeTeXLayoutEngine,
    param1: i32,
    param2: i32,
) -> String {
    let mut name = ptr::null_mut();
    match what {
        8 => name = getGraphiteFeatureLabel(engine, param1 as u32),
        9 => name = getGraphiteFeatureSettingLabel(engine, param1 as u32, param2 as u32),
        _ => {}
    }
    if !name.is_null() {
        let s = c_pointer_to_str(name).to_string();
        gr_label_destroy(name as *mut libc::c_void);
        s
    } else {
        String::new()
    }
}
pub(crate) unsafe fn gr_font_get_named(name: &str, what: i32, engine: &XeTeXLayoutEngine) -> i32 {
    match what {
        10 => findGraphiteFeatureNamed(engine, name.as_bytes()) as _,
        _ => -1,
    }
}
pub(crate) unsafe fn gr_font_get_named_1(
    name: &str,
    what: i32,
    engine: &XeTeXLayoutEngine,
    param: i32,
) -> i32 {
    match what {
        14 => findGraphiteFeatureSettingNamed(engine, param as u32, name.as_bytes()) as _,
        _ => -1,
    }
}
#[cfg(target_os = "macos")]
unsafe fn cgColorToRGBA32(color: CGColorRef) -> u32 {
    let components = CGColorGetComponents(color);
    u32::from_be_bytes([
        (*components.offset(0) * 255. + 0.5) as u8,
        (*components.offset(1) * 255. + 0.5) as u8,
        (*components.offset(2) * 255. + 0.5) as u8,
        (*components.offset(3) * 255. + 0.5) as u8,
    ])
}
pub(crate) unsafe fn makeXDVGlyphArrayData(p: &NativeWord) -> Vec<u8> {
    let glyphCount: u16 = p.glyph_count();
    let i = glyphCount as usize * 10 + 8;

    let mut buf = Vec::with_capacity((i / 1024 + 1) * 1024);
    let locations = p.locations();
    let glyph_ids = p.glyph_ids();
    buf.extend_from_slice(&p.width().0.to_be_bytes()[..]);
    buf.extend_from_slice(&glyphCount.to_be_bytes()[..]);
    for loc in locations {
        buf.extend_from_slice(&loc.x.0.to_be_bytes()[..]);
        buf.extend_from_slice(&loc.y.0.to_be_bytes()[..]);
    }
    for gid in glyph_ids {
        buf.extend_from_slice(&gid.to_be_bytes()[..]);
    }
    buf
}
pub(crate) unsafe fn make_font_def(f: usize) -> Vec<u8> {
    // XXX: seems like a good idea to make a struct FontDef
    let mut flags: u16 = 0_u16;
    #[allow(unused_assignments)]
    let mut rgba: u32 = 0;
    let size;
    let filename: String;
    let mut index: u32 = 0;
    /* PlatformFontRef fontRef = 0; */
    let extend;
    let slant;
    #[allow(unused_assignments)]
    let mut embolden: f32 = 0.;
    match &FONT_LAYOUT_ENGINE[f].as_native() {
        #[cfg(target_os = "macos")]
        Aat(attributes) => {
            let font =
                CFDictionaryGetValue(*attributes, kCTFontAttributeName as *const libc::c_void)
                    as CTFontRef;
            filename = crate::xetex_aatfont::getFileNameFromCTFont(font, &mut index);
            assert!(!filename.is_empty());
            if !CFDictionaryGetValue(
                *attributes,
                kCTVerticalFormsAttributeName as *const libc::c_void,
            )
            .is_null()
            {
                flags = (flags as i32 | 0x100) as u16
            }
            let color = CFDictionaryGetValue(
                *attributes,
                kCTForegroundColorAttributeName as *const libc::c_void,
            ) as CGColorRef;
            if !color.is_null() {
                rgba = cgColorToRGBA32(color)
            }
            let t = CTFontGetMatrix(font);
            extend = t.a as f32;
            slant = t.c as f32;
            let emboldenNumber = CFDictionaryGetValue(
                *attributes,
                aat::getkXeTeXEmboldenAttributeName() as *const libc::c_void,
            ) as CFNumberRef;
            if !emboldenNumber.is_null() {
                CFNumberGetValue(
                    emboldenNumber,
                    kCFNumberFloatType as i32 as CFNumberType,
                    &mut embolden as *mut f32 as *mut libc::c_void,
                );
            }
            let fSize = CTFontGetSize(font);
            size = Scaled::from(fSize);
        }
        Otgr(engine) => {
            /* fontRef = */
            getFontRef(engine);
            filename = engine.font_filename(&mut index);
            assert!(!filename.is_empty());
            rgba = engine.rgb_value();
            if FONT_FLAGS[f] as i32 & 0x2 != 0 {
                flags = (flags as i32 | 0x100) as u16
            }
            extend = engine.get_extend_factor();
            slant = engine.get_slant_factor();
            embolden = engine.get_embolden_factor();
            size = Scaled::from(engine.point_size() as f64)
        }
    }
    /* parameters after internal font ID:
    //  size[4]
    //  flags[2]
    //  l[1] n[l]
    //  if flags & COLORED:
    //      c[4]
     */
    let mut fontDefLength = 4 + 2 + 1 + filename.len() as i32 + 4; /* face index */
    if FONT_FLAGS[f] as i32 & 0x1 != 0 {
        fontDefLength += 4; /* 32-bit RGBA value */
        flags |= 0x200;
    }
    if extend as f64 != 1. {
        fontDefLength += 4;
        flags |= 0x1000;
    }
    if slant as f64 != 0. {
        fontDefLength += 4;
        flags |= 0x2000;
    }
    if embolden as f64 != 0. {
        fontDefLength += 4;
        flags |= 0x4000;
    }
    let mut buf = Vec::with_capacity((fontDefLength as usize / 1024 + 1) * 1024);
    buf.extend_from_slice(&(size.0 as u32).to_be_bytes()[..]);
    buf.extend_from_slice(&flags.to_be_bytes()[..]);
    buf.push(filename.len() as u8);

    buf.extend_from_slice(filename.as_bytes());

    buf.extend_from_slice(&index.to_be_bytes()[..]);
    if FONT_FLAGS[f] as i32 & 0x1 != 0 {
        buf.extend_from_slice(&rgba.to_be_bytes()[..]);
    }
    if flags as i32 & 0x1000 != 0 {
        buf.extend_from_slice(&Scaled::from(extend as f64).0.to_be_bytes()[..]);
    }
    if flags as i32 & 0x2000 != 0 {
        buf.extend_from_slice(&Scaled::from(slant as f64).0.to_be_bytes()[..]);
    }
    if flags as i32 & 0x4000 != 0 {
        buf.extend_from_slice(&Scaled::from(embolden as f64).0.to_be_bytes()[..]);
    }
    buf
}
pub(crate) unsafe fn apply_mapping(pCnv: *mut libc::c_void, txt: &[u16]) -> Vec<u16> {
    let cnv = pCnv as teckit::TECkit_Converter;
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    let two = std::mem::size_of::<UniChar>();
    let mut out_length = txt.len() * two + 32;
    let mut mapped_text = vec![0_u16; out_length / 2];
    loop
    /* try the mapping */
    {
        let status = teckit::TECkit_ConvertBuffer(
            cnv,
            txt.as_ptr() as *const u8,
            (txt.len() * two) as u32,
            &mut inUsed,
            mapped_text.as_mut_ptr() as *mut u8,
            out_length as _,
            &mut outUsed,
            1,
        );
        match status {
            0 => {
                mapped_text.truncate((outUsed as usize) / 2);
                return mapped_text;
            }
            1 => {
                out_length += txt.len() * two + 32;
                mapped_text = vec![0_u16; out_length / 2];
            }
            _ => return Vec::new(),
        }
    }
}
unsafe fn snap_zone(value: &mut Scaled, snap_value: Scaled, fuzz: Scaled) {
    let difference = *value - snap_value;
    if difference <= fuzz && difference >= -fuzz {
        *value = snap_value
    };
}
pub(crate) unsafe fn get_native_char_height_depth(font: usize, ch: char) -> (Scaled, Scaled) {
    use crate::xetex_consts::{QUAD_CODE, X_HEIGHT_CODE};
    const CAP_HEIGHT: i32 = 8;
    let (ht, dp) = match &FONT_LAYOUT_ENGINE[font] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(attributes)) => {
            let mut ht: f32 = 0.;
            let mut dp: f32 = 0.;
            let gid: i32 = aat::MapCharToGlyph_AAT(*attributes, ch);
            aat::GetGlyphHeightDepth_AAT(*attributes, gid as u16, &mut ht, &mut dp);
            (ht, dp)
        }
        Font::Native(Otgr(engine)) => {
            let gid = engine.map_char_to_glyph(ch) as i32;
            engine.glyph_height_depth(gid as u32).unwrap()
        }
        _ => panic!("bad native font flag in `get_native_char_height_depth`"),
    };
    let mut height = (ht as f64).into();
    let mut depth = (dp as f64).into();
    /* snap to "known" zones for baseline, x-height, cap-height if within 4% of em-size */
    let fuzz = Scaled(FONT_INFO[(QUAD_CODE + PARAM_BASE[font]) as usize].b32.s1) / 25;
    snap_zone(&mut depth, Scaled::ZERO, fuzz);
    snap_zone(&mut height, Scaled::ZERO, fuzz);
    snap_zone(
        &mut height,
        Scaled(
            FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[font]) as usize]
                .b32
                .s1,
        ),
        fuzz,
    );
    snap_zone(
        &mut height,
        Scaled(FONT_INFO[(CAP_HEIGHT + PARAM_BASE[font]) as usize].b32.s1),
        fuzz,
    );
    (height, depth)
}
pub(crate) unsafe fn getnativecharht(f: usize, c: char) -> Scaled {
    get_native_char_height_depth(f, c).0
}
pub(crate) unsafe fn getnativechardp(f: usize, c: char) -> Scaled {
    get_native_char_height_depth(f, c).1
}
pub(crate) unsafe fn get_native_char_sidebearings(font: &NativeFont, ch: char) -> (Scaled, Scaled) {
    let (l, r) = match font {
        #[cfg(target_os = "macos")]
        Aat(attributes) => {
            let mut l: f32 = 0.;
            let mut r: f32 = 0.;
            let gid = aat::MapCharToGlyph_AAT(*attributes, ch);
            aat::GetGlyphSidebearings_AAT(*attributes, gid as u16, &mut l, &mut r);
            (l, r)
        }
        Otgr(engine) => {
            let gid = engine.map_char_to_glyph(ch) as i32;
            engine.glyph_sidebearings(gid as u32).unwrap()
        }
    };
    ((l as f64).into(), (r as f64).into())
}

use crate::text_layout_engine::GlyphEdge;

pub(crate) unsafe fn get_glyph_bounds(font: usize, edge: i32, gid: i32) -> Scaled {
    GlyphEdge::from_int(edge)
        .map(|edge| {
            /* edge codes 1,2,3,4 => L T R B */
            let (a, b) = match &FONT_LAYOUT_ENGINE[font] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(attributes)) => {
                    let mut a: f32 = 0.;
                    let mut b: f32 = 0.;
                    if edge & 1 != 0 {
                        aat::GetGlyphSidebearings_AAT(*attributes, gid as u16, &mut a, &mut b);
                    } else {
                        aat::GetGlyphHeightDepth_AAT(*attributes, gid as u16, &mut a, &mut b);
                    }
                    (a, b)
                }
                Font::Native(Otgr(engine)) => {
                    if edge.is_side() {
                        engine.glyph_sidebearings(gid as u32).unwrap()
                    } else {
                        engine.glyph_height_depth(gid as u32).unwrap()
                    }
                }
                _ => abort!("bad native font flag in `get_glyph_bounds`"),
            };
            edge.pick_from(&(a, b))
        })
        .map(|d| Scaled::from(d as f64))
        // This means 'None' to xetex
        .unwrap_or(Scaled::ZERO)
}
pub(crate) unsafe fn getnativecharic(f: &NativeFont, letter_space: Scaled, c: char) -> Scaled {
    let (_, rsb) = get_native_char_sidebearings(f, c);
    if rsb < Scaled::ZERO {
        letter_space - rsb
    } else {
        letter_space
    }
}
/* single-purpose metrics accessors */
pub(crate) unsafe fn getnativecharwd(f: usize, c: char) -> Scaled {
    match &FONT_LAYOUT_ENGINE[f] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(attributes)) => {
            let gid = aat::MapCharToGlyph_AAT(*attributes, c);
            (aat::GetGlyphWidth_AAT(*attributes, gid as u16)).into()
        }
        Font::Native(Otgr(engine)) => {
            let gid = engine.map_char_to_glyph(c) as i32;
            (engine.get_glyph_width_from_engine(gid as u32) as f64).into()
        }
        _ => panic!("bad native font flag in `get_native_char_wd`"),
    }
}
pub(crate) unsafe fn real_get_native_glyph(node: &NativeWord, index: u32) -> u16 {
    let glyph_ids = node.glyph_ids();
    if index >= node.glyph_count() as u32 {
        0_u16
    } else {
        glyph_ids[index as usize]
    }
}
pub(crate) unsafe fn store_justified_native_glyphs(node: &mut NativeWord) {
    let f = node.font() as usize;
    let nf = FONT_LAYOUT_ENGINE[f].as_native();
    match nf {
        #[cfg(target_os = "macos")]
        Aat(_) => {
            /* separate Mac-only codepath for AAT fonts */
            aat::do_aat_layout(node, true);
            return;
        }
        Otgr(_) => {
            /* save desired width */
            let savedWidth = node.width();
            node.set_metrics(false);
            if node.width() != savedWidth {
                /* see how much adjustment is needed overall */
                let justAmount = f64::from(savedWidth - node.width());
                /* apply justification to spaces (or if there are none, distribute it to all glyphs as a last resort) */
                let glyph_count = node.glyph_count() as usize;
                let mut spaceCount: i32 = 0;
                let spaceGlyph: i32 = map_char_to_glyph(nf, ' ');
                for i in 0..glyph_count {
                    if node.glyph_ids()[i] as i32 == spaceGlyph {
                        spaceCount += 1
                    }
                }
                if spaceCount > 0 {
                    let mut adjustment = 0_f64;
                    let mut spaceIndex = 0_i32;
                    for i in 0..glyph_count as usize {
                        let loc = &mut node.locations_mut()[i];
                        loc.x = (f64::from(loc.x) + adjustment).into();
                        if node.glyph_ids()[i] as i32 == spaceGlyph {
                            spaceIndex += 1;
                            adjustment = justAmount * spaceIndex as f64 / spaceCount as f64
                        }
                    }
                } else {
                    for i in 1..glyph_count {
                        let loc = &mut node.locations_mut()[i];
                        loc.x = (f64::from(loc.x)
                            + justAmount * i as f64 / (glyph_count - 1) as f64)
                            .into();
                    }
                }
                node.set_width(savedWidth);
            };
        }
    }
}
pub(crate) unsafe fn measure_native_node(node: &mut NativeWord, use_glyph_metrics: bool) {
    let f = node.font() as usize;
    match &mut FONT_LAYOUT_ENGINE[f] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(_)) => {
            /* we're using this font in AAT mode, so font_layout_engine[f] is actually a CFDictionaryRef */
            aat::do_aat_layout(node, false);
        }
        Font::Native(Otgr(engine)) => {
            let request = LayoutRequest::from_node(node);
            let layout = engine.layout_text(request);
            layout.write_node(node);
        }
        _ => panic!("bad native font flag in `measure_native_node`"),
    }
    if !use_glyph_metrics || node.glyph_count() == 0 {
        /* for efficiency, height and depth are the font's ascent/descent,
        not true values based on the actual content of the word,
        unless use_glyph_metrics is non-zero */
        node.set_height(Scaled(HEIGHT_BASE[f]));
        node.set_depth(Scaled(DEPTH_BASE[f]));
    } else {
        /* this iterates over the glyph data whether it comes from AAT or OT layout */
        let locations = node.locations(); /* NB negative is upwards in locations[].y! */
        let glyph_ids = node.glyph_ids();
        let mut yMin: f32 = 65536.0f64 as f32;
        let mut yMax: f32 = -65536.0f64 as f32;
        for i in 0..node.glyph_count() as usize {
            let y_0 = f32::from(-locations[i].y);
            let bbox = getCachedGlyphBBox(f as u16, glyph_ids[i]).unwrap_or_else(|| {
                let bbox = match &FONT_LAYOUT_ENGINE[f] {
                    #[cfg(target_os = "macos")]
                    Font::Native(Aat(engine)) => aat::GetGlyphBBox_AAT(*engine, glyph_ids[i]),
                    Font::Native(Otgr(engine)) => engine
                        .font
                        .get_glyph_bounds(glyph_ids[i])
                        .unwrap_or(GlyphBBox::zero()),
                    _ => GlyphBBox::zero(),
                };
                cacheGlyphBBox(f as u16, glyph_ids[i], &bbox);
                bbox
            });
            let ht = bbox.yMax;
            let dp = -bbox.yMin;
            if y_0 + ht > yMax {
                yMax = y_0 + ht
            }
            if y_0 - dp < yMin {
                yMin = y_0 - dp
            }
        }
        node.set_height((yMax as f64).into());
        node.set_depth(-Scaled::from(yMin as f64));
    };
}
pub(crate) unsafe fn real_get_native_italic_correction(node: &NativeWord) -> Scaled {
    let f = node.font() as usize;
    let n = node.glyph_count() as usize;
    if n > 0 {
        let glyph_ids = node.glyph_ids();
        match &FONT_LAYOUT_ENGINE[f] {
            #[cfg(target_os = "macos")]
            Font::Native(Aat(engine)) => {
                Scaled::from(aat::GetGlyphItalCorr_AAT(*engine, glyph_ids[n - 1]))
                    + FONT_LETTER_SPACE[f]
            }
            Font::Native(Otgr(engine)) => {
                Scaled::from(
                    engine
                        .glyph_ital_correction(glyph_ids[n - 1] as u32)
                        .unwrap() as f64,
                ) + FONT_LETTER_SPACE[f]
            }
            _ => Scaled::ZERO,
        }
    } else {
        Scaled::ZERO
    }
}
pub(crate) unsafe fn real_get_native_glyph_italic_correction(node: &Glyph) -> Scaled {
    let gid: u16 = node.glyph();
    let f = node.font() as usize;
    match &FONT_LAYOUT_ENGINE[f] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(engine)) => (aat::GetGlyphItalCorr_AAT(*engine, gid)).into(),
        Font::Native(Otgr(engine)) => {
            (engine.glyph_ital_correction(gid as u32).unwrap() as f64).into()
        }
        _ => {
            Scaled::ZERO
            /* can't actually happen */
        }
    }
}
pub(crate) unsafe fn measure_native_glyph(node: &mut Glyph, use_glyph_metrics: bool) {
    let gid = node.glyph();
    let f = node.font() as usize;
    let (ht, dp) = match &FONT_LAYOUT_ENGINE[f] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(attributes)) => {
            node.set_width((aat::GetGlyphWidth_AAT(*attributes, gid)).into());
            let mut ht = 0_f32;
            let mut dp = 0_f32;
            if use_glyph_metrics {
                aat::GetGlyphHeightDepth_AAT(*attributes, gid, &mut ht, &mut dp);
            }
            (ht, dp)
        }
        Font::Native(Otgr(engine)) => {
            let fontInst = engine.get_font();
            node.set_width((fontInst.get_glyph_width(gid as u16) as f64).into());
            if use_glyph_metrics {
                engine.glyph_height_depth(gid as u32).unwrap()
            } else {
                (0., 0.)
            }
        }
        _ => panic!("bad native font flag in `measure_native_glyph`"),
    };
    if use_glyph_metrics {
        node.set_height((ht as f64).into());
        node.set_depth((dp as f64).into());
    } else {
        node.set_height(Scaled(HEIGHT_BASE[f]));
        node.set_depth(Scaled(DEPTH_BASE[f]));
    };
}
pub(crate) unsafe fn map_char_to_glyph(font: &NativeFont, ch: char) -> i32 {
    match font {
        #[cfg(target_os = "macos")]
        Aat(engine) => aat::MapCharToGlyph_AAT(*engine, ch),
        Otgr(engine) => engine.map_char_to_glyph(ch) as i32,
    }
}
pub(crate) unsafe fn map_glyph_to_index(font: &NativeFont, name: &str) -> i32 {
    let name = CString::new(name).unwrap();
    match font {
        #[cfg(target_os = "macos")]
        Aat(engine) => aat::MapGlyphToIndex_AAT(*engine, name.as_ptr()),
        Otgr(engine) => engine.map_glyph_to_index(name.as_ptr()),
    }
}
pub(crate) unsafe fn get_font_char_range(font: usize, first: i32) -> i32 {
    match &mut FONT_LAYOUT_ENGINE[font] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(engine)) => aat::GetFontCharRange_AAT(*engine, first),
        Font::Native(Otgr(engine)) => engine.font_char_range(first),
        _ => panic!("bad native font flag in `get_font_char_range\'`"),
    }
}

pub(crate) unsafe fn real_get_native_word_cp(node: &NativeWord, side: Side) -> i32 {
    let glyph_ids = node.glyph_ids();
    let glyph_count = node.glyph_count() as usize;
    let f = node.font() as usize;
    if glyph_count == 0 {
        return 0;
    }
    let actual_glyph = match side {
        Side::Left => glyph_ids[0], // we should not reach this point
        Side::Right => glyph_ids[glyph_count - 1],
    };
    get_cp_code(f, actual_glyph as u32, side)
}
