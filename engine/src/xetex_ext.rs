#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::c_pointer_to_str;
use crate::t_print_nl;
use std::ffi::CString;

use crate::node::{Glyph, NativeWord};
use crate::strstartswith;
use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::xetex_consts::{Side, UnicodeMode};
use bridge::{ttstub_input_get_size, InFile, TTInputFormat};
use libc::free;
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
use crate::core_memory::{xcalloc, xrealloc};
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
use libc::strdup;

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

pub(crate) type size_t = usize;

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
static mut brkLocaleStrNum: i32 = 0i32;

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
    *info = 0i32;
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
    if let Some(mut map) = InFile::open(&buffer, TTInputFormat::MISCFONTS, 0i32) {
        let mapping_size = ttstub_input_get_size(&mut map) as usize;
        let mut mapping = vec![0_u8; mapping_size];
        if map.read_exact(mapping.as_mut_slice()).is_err() {
            abort!("could not read mapping file \"{}\"", buffer);
        }
        if byteMapping != 0 {
            teckit::TECkit_CreateConverter(
                mapping.as_mut_ptr(),
                mapping_size as u32,
                0i32 as u8,
                4i32 as u16,
                1i32 as u16,
                &mut cnv,
            );
        } else {
            teckit::TECkit_CreateConverter(
                mapping.as_mut_ptr(),
                mapping_size as u32,
                1i32 as u8,
                4i32 as u16,
                4i32 as u16,
                &mut cnv,
            );
        }
        if cnv.is_null() {
            /* tracing */
            font_mapping_warning(&buffer, 2i32);
        /* not loadable */
        } else if get_tracing_fonts_state() > 1i32 {
            font_mapping_warning(&buffer, 0i32);
        }
    } else {
        font_mapping_warning(&buffer, 1i32);
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
        1i32 as u8,
    );
    if outUsed < 1_u32 {
        0i32
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
unsafe fn read_tag_with_param(mut cp: &[u8], param: *mut i32) -> hb_tag_t {
    let mut cp2 = cp;
    while !cp2.is_empty() && !b":;,=".contains(&cp2[0]) {
        cp2 = &cp2[1..]
    }
    let tag = hb_tag_from_string(cp.as_ptr() as *const i8, (cp.len() - cp2.len()) as _);
    cp = cp2;
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
    rgbValue <<= 8i32;
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
    if i == 2i32 {
        rgbValue = (rgbValue as u32).wrapping_add(alpha) as u32
    } else {
        rgbValue = (rgbValue as u32).wrapping_add(0xff_u32) as u32
    }
    rgbValue
}
pub(crate) unsafe fn readCommonFeatures(
    feat: &[u8],
    end: usize,
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
        loaded_font_mapping =
            load_mapping_file(std::str::from_utf8(&sep[1..end - 7]).unwrap(), 0_i8);
        return 1i32;
    }
    if let Some(mut sep) = strstartswith(feat, b"extend") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        *extend = read_double(&mut sep) as f32;
        return 1i32;
    }
    if let Some(mut sep) = strstartswith(feat, b"slant") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        *slant = read_double(&mut sep) as f32;
        return 1;
    }
    if let Some(mut sep) = strstartswith(feat, b"embolden") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        *embolden = read_double(&mut sep) as f32;
        return 1i32;
    }
    if let Some(mut sep) = strstartswith(feat, b"letterspace") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        *letterspace = read_double(&mut sep) as f32;
        return 1;
    }
    if let Some(mut sep) = strstartswith(feat, b"color") {
        if sep[0] != b'=' {
            return -1;
        }
        sep = &sep[1..];
        let s = sep;
        *rgbValue = read_rgb_a(&mut sep);
        if sep.len() == s.len() - 6 || sep.len() == s.len() - 8 {
            loaded_font_flags = (loaded_font_flags as i32 | 0x1) as i8
        } else {
            return -1;
        }
        return 1;
    }
    0i32
}
unsafe fn readFeatureNumber(mut s: &[u8], f: *mut hb_tag_t, v: *mut i32) -> bool
/* s...e is a "id=setting" string; */ {
    *f = 0i32 as hb_tag_t;
    *v = 0i32;
    if !(b'0'..=b'9').contains(&s[0]) {
        return false;
    }
    while (b'0'..=b'9').contains(&s[0]) {
        *f = (*f) * 10 + (s[0] as u32) - ('0' as u32);
        s = &s[1..];
    }
    while b" \t".contains(&s[0]) {
        s = &s[1..];
    }
    if s[0] != b'=' {
        /* no setting was specified */
        return false;
    } /* NULL-terminated array */
    s = &s[1..];
    if !(b'0'..=b'9').contains(&s[0]) {
        return false;
    }
    while (b'0'..=b'9').contains(&s[0]) {
        *v = *v * 10 + (s[0] as i32) - ('0' as i32);
        s = &s[1..];
    }
    while b" \t".contains(&s[0]) {
        s = &s[1..];
    }
    if !s.is_empty() {
        return false;
    }
    true
}

use crate::xetex_layout_interface::XeTeXFont;
unsafe fn loadOTfont(
    fontRef: PlatformFontRef,
    font: Box<XeTeXFont>,
    scaled_size: Scaled,
    mut cp1: &[u8],
) -> Option<NativeFont> {
    let mut font = Some(font);
    let mut current_block: u64;
    let mut script = 0;
    let mut shapers: *mut *mut i8 = ptr::null_mut();
    let mut nShapers: i32 = 0i32;
    let mut tag: hb_tag_t = 0;
    let mut rgbValue: u32 = 0xff_u32;
    let mut extend: f32 = 1.;
    let mut slant: f32 = 0.;
    let mut embolden: f32 = 0.;
    let mut letterspace: f32 = 0.;
    let reqEngine: i8 = getReqEngine();
    if reqEngine as i32 == 'O' as i32 || reqEngine as i32 == 'G' as i32 {
        shapers = xrealloc(
            shapers as *mut libc::c_void,
            ((nShapers + 1i32) as u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as _,
        ) as *mut *mut i8;
        if reqEngine as i32 == 'O' as i32 {
            static mut ot_const: [i8; 3] = [111, 116, 0];
            *shapers.offset(nShapers as isize) = ot_const.as_mut_ptr()
        } else if reqEngine as i32 == 'G' as i32 {
            static mut graphite2_const: [i8; 10] = [103, 114, 97, 112, 104, 105, 116, 101, 50, 0];
            *shapers.offset(nShapers as isize) = graphite2_const.as_mut_ptr()
        }
        nShapers += 1
    }
    let engine = if reqEngine as i32 == 'G' as i32 {
        let mut tmpShapers: [*mut i8; 1] = [*shapers.offset(0)];
        /* create a default engine so we can query the font for Graphite features;
         * because of font caching, it's cheap to discard this and create the real one later */
        Some(XeTeXLayoutEngine::create(
            fontRef,
            font.take().unwrap(),
            script,
            String::new(),
            Vec::new(),
            tmpShapers.as_mut_ptr(),
            rgbValue,
            extend,
            slant,
            embolden,
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
        let mut cp2 = cp1;
        while !cp2.is_empty() && !b":;,".contains(&cp2[0]) {
            cp2 = &cp2[1..];
        }
        if let Some(mut cp3) = strstartswith(cp1, b"script") {
            if cp3[0] != b'=' {
                current_block = 10622493848381539643;
            } else {
                cp3 = &cp3[1..];
                script =
                    hb_tag_from_string(cp3.as_ptr() as *const i8, (cp3.len() - cp2.len()) as _);
                current_block = 13857423536159756434;
            }
        } else if let Some(mut cp3) = strstartswith(cp1, b"language") {
            if cp3[0] != b'=' {
                current_block = 10622493848381539643;
            } else {
                cp3 = &cp3[1..];
                language = std::str::from_utf8(&cp3[..cp3.len() - cp2.len()])
                    .unwrap()
                    .to_string();
                current_block = 13857423536159756434;
            }
        } else if let Some(mut cp3) = strstartswith(cp1, b"shaper") {
            if cp3[0] != b'=' {
                current_block = 10622493848381539643;
            } else {
                cp3 = &cp3[1..];
                shapers = xrealloc(
                    shapers as *mut libc::c_void,
                    ((nShapers + 1i32) as u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64)
                        as _,
                ) as *mut *mut i8;
                /* some dumb systems have no strndup() */
                let len = cp3.len() - cp2.len();
                let ccp3 = CString::new(cp3).unwrap();
                *shapers.offset(nShapers as isize) = strdup(ccp3.as_ptr());
                *(*shapers.add(nShapers as usize)).add(len) = '\u{0}' as i32 as i8;
                nShapers += 1;
                current_block = 13857423536159756434;
            }
        } else {
            let i = readCommonFeatures(
                cp1,
                cp1.len() - cp2.len(),
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
                    if readFeatureNumber(&cp1[..cp1.len() - cp2.len()], &mut tag, &mut value)
                        || findGraphiteFeature(
                            engine.as_ref().unwrap(),
                            &cp1[..cp1.len() - cp2.len()],
                            &mut tag,
                            &mut value,
                        ) as i32
                            != 0
                    {
                        features.push(hb_feature_t {
                            tag,
                            value: value as u32,
                            start: 0,
                            end: -1i32 as u32,
                        });
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
                        if cp1[0] == b'+' {
                            let mut param: i32 = 0i32;
                            tag = read_tag_with_param(&cp1[1..], &mut param);
                            let start = 0;
                            let end = -1i32 as u32;
                            // for backward compatibility with pre-0.9999 where feature
                            // indices started from 0
                            if param >= 0i32 {
                                param += 1
                            }
                            let value = param as u32;
                            features.push(hb_feature_t {
                                tag,
                                value,
                                start,
                                end,
                            });
                            current_block = 13857423536159756434;
                        } else if cp1[0] == b'-' {
                            cp1 = &cp1[1..];
                            tag = hb_tag_from_string(
                                cp1.as_ptr() as *const i8,
                                (cp1.len() - cp2.len()) as _,
                            );
                            features.push(hb_feature_t {
                                tag,
                                start: 0,
                                end: -1i32 as u32,
                                value: 0,
                            });
                            current_block = 13857423536159756434;
                        } else if cp1.starts_with(b"vertical") {
                            let mut n = cp1.len() - cp2.len();
                            if b";:".contains(&cp1[n]) {
                                n -= 1;
                            }
                            while n != 0 || b" \t".contains(&cp1[n]) {
                                n -= 1;
                            }
                            if n != 0 {
                                // TODO: check
                                n += 1;
                            }
                            if n == 8 {
                                loaded_font_flags = (loaded_font_flags as i32 | 0x2i32) as i8;
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
        match current_block {
            10622493848381539643 => {
                font_feature_warning(&cp1[..cp1.len() - cp2.len()], &[]);
            }
            _ => {}
        }
        cp1 = cp2;
    }
    /* break if end of string */
    if !shapers.is_null() {
        shapers = xrealloc(
            shapers as *mut libc::c_void,
            ((nShapers + 1i32) as u64).wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as _,
        ) as *mut *mut i8;
        *shapers.offset(nShapers as isize) = ptr::null_mut();
    }
    if embolden as f64 != 0.0f64 {
        embolden = (embolden as f64 * Fix2D(scaled_size) / 100.0f64) as f32
    }
    if letterspace as f64 != 0.0f64 {
        loaded_font_letter_space =
            Scaled((letterspace as f64 / 100.0f64 * scaled_size.0 as f64) as i32);
    }
    if loaded_font_flags as i32 & 0x1i32 == 0i32 {
        rgbValue = 0xff_u32
    }
    let mut font = if let Some(engine) = engine {
        engine.release()
    } else {
        font.unwrap()
    };
    if loaded_font_flags as i32 & 0x2i32 != 0i32 {
        setFontLayoutDir(&mut font, 1i32);
    }
    if let Some(engine) = Some(XeTeXLayoutEngine::create(
        fontRef, font, script, language, features, shapers, rgbValue, extend, slant, embolden,
    )) {
        Some(Otgr(engine))
    } else {
        // only free these if creation failed, otherwise the engine now owns them
        free(shapers as *mut libc::c_void);
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
    let (nameString, mut varString, featString, index) = splitFontName(name);
    // check for "[filename]" form, don't search maps in this case
    if nameString.as_bytes()[0] == b'[' {
        if scaled_size < Scaled::ZERO {
            if let Some(font) = createFontFromFile(&nameString[1..], index, Scaled(655360)) {
                let dsize = D2Fix(font.get_design_size());
                if scaled_size == Scaled(-1000) {
                    scaled_size = dsize
                } else {
                    scaled_size = xn_over_d(dsize, -scaled_size, 1000).0
                }
            }
        }
        if let Some(font) = createFontFromFile(&nameString[1..], index, scaled_size) {
            loaded_font_design_size = D2Fix(font.get_design_size());
            /* This is duplicated in XeTeXFontMgr::findFont! */
            setReqEngine(0_i8);
            if !varString.is_empty() {
                if varString.starts_with("/AAT") {
                    setReqEngine('A' as i32 as i8);
                } else if varString.starts_with("/OT") || varString.starts_with("/ICU") {
                    setReqEngine('O' as i32 as i8);
                } else if varString.starts_with("/GR") {
                    setReqEngine('G' as i32 as i8);
                }
            }
            rval = loadOTfont(
                0 as PlatformFontRef,
                font,
                scaled_size,
                featString.as_bytes(),
            );
            if rval.is_some() && get_tracing_fonts_state() > 0 {
                diagnostic(false, || {
                    t_print_nl!(" -> {}", &nameString[1..]);
                });
            }
        }
    } else {
        let fontRef = findFontByName(&nameString, &mut varString, Fix2D(scaled_size));
        if !fontRef.is_null() {
            /* update name_of_font to the full name of the font, for error messages during font loading */
            name_of_font = getFullName(fontRef);
            if scaled_size < Scaled::ZERO {
                if let Some(font) = createFont(fontRef, scaled_size) {
                    let dsize_0 = D2Fix(font.get_design_size());
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
                    rval = loadOTfont(fontRef, font, scaled_size, featString.as_bytes());
                }
                #[cfg(target_os = "macos")]
                {
                    /* decide whether to use AAT or OpenType rendering with this font */
                    if getReqEngine() as libc::c_int == 'A' as i32 {
                        rval = aat::loadAATfont(fontRef, scaled_size, featString.as_bytes());
                    } else {
                        if getReqEngine() as libc::c_int == 'O' as i32
                            || getReqEngine() as libc::c_int == 'G' as i32
                            || !getFontTablePtr(&font, u32::from_be_bytes([b'G', b'S', b'U', b'B']))
                                .is_null()
                            || !getFontTablePtr(&font, u32::from_be_bytes([b'G', b'P', b'O', b'S']))
                                .is_null()
                        {
                            rval = loadOTfont(fontRef, font, scaled_size, featString.as_bytes())
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
    let (a, d) = engine.get_ascent_and_descent();
    let ascent = D2Fix(a as f64);
    let descent = D2Fix(d as f64);
    let slant = D2Fix(
        Fix2D(getSlant(engine.get_font())) * engine.get_extend_factor() as f64
            + engine.get_slant_factor() as f64,
    );
    /* get cap and x height from OS/2 table */
    let (a, d) = engine.get_cap_and_x_height();
    let mut capheight = D2Fix(a as f64);
    let mut xheight = D2Fix(d as f64);
    /* fallback in case the font does not have OS/2 table */
    if xheight == Scaled::ZERO {
        let glyphID = engine.map_char_to_glyph('x') as i32;
        if glyphID != 0i32 {
            let (a, _) = engine.get_glyph_height_depth(glyphID as u32);
            xheight = D2Fix(a as f64)
        } else {
            xheight = ascent / 2
            /* arbitrary figure if there's no 'x' in the font */
        }
    }
    if capheight == Scaled::ZERO {
        let glyphID_0 = engine.map_char_to_glyph('X') as i32;
        if glyphID_0 != 0i32 {
            let (a, _) = engine.get_glyph_height_depth(glyphID_0 as u32);
            capheight = D2Fix(a as f64)
        } else {
            capheight = ascent
            /* arbitrary figure if there's no 'X' in the font */
        }
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
    0i32
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
        11 => return 1i32,
        12 => return countGraphiteFeatureSettings(engine, param as u32) as i32,
        _ => {}
    }
    0i32
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
    0i32
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
    for i in 0..glyphCount as usize {
        buf.extend_from_slice(&locations[i].x.0.to_be_bytes()[..]);
        buf.extend_from_slice(&locations[i].y.0.to_be_bytes()[..]);
    }
    for i in 0..glyphCount as usize {
        buf.extend_from_slice(&glyph_ids[i].to_be_bytes()[..]);
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
    let mut embolden: f32 = 0.0f64 as f32;
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
                flags = (flags as libc::c_int | 0x100i32) as u16
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
                    kCFNumberFloatType as libc::c_int as CFNumberType,
                    &mut embolden as *mut f32 as *mut libc::c_void,
                );
            }
            let fSize = CTFontGetSize(font);
            size = D2Fix(fSize);
        }
        Otgr(engine) => {
            /* fontRef = */
            getFontRef(engine);
            filename = engine.get_font_filename(&mut index);
            assert!(!filename.is_empty());
            rgba = engine.get_rgb_value();
            if FONT_FLAGS[f] as i32 & 0x2i32 != 0i32 {
                flags = (flags as i32 | 0x100i32) as u16
            }
            extend = engine.get_extend_factor();
            slant = engine.get_slant_factor();
            embolden = engine.get_embolden_factor();
            size = D2Fix(engine.get_point_size() as f64)
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
    if FONT_FLAGS[f] as i32 & 0x1i32 != 0i32 {
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
    if FONT_FLAGS[f] as i32 & 0x1i32 != 0i32 {
        buf.extend_from_slice(&rgba.to_be_bytes()[..]);
    }
    if flags as i32 & 0x1000i32 != 0 {
        buf.extend_from_slice(&D2Fix(extend as f64).0.to_be_bytes()[..]);
    }
    if flags as i32 & 0x2000i32 != 0 {
        buf.extend_from_slice(&D2Fix(slant as f64).0.to_be_bytes()[..]);
    }
    if flags as i32 & 0x4000i32 != 0 {
        buf.extend_from_slice(&D2Fix(embolden as f64).0.to_be_bytes()[..]);
    }
    buf
}
pub(crate) unsafe fn apply_mapping(pCnv: *mut libc::c_void, txt: &[u16]) -> Vec<u16> {
    let cnv = pCnv as teckit::TECkit_Converter;
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    let _2 = std::mem::size_of::<UniChar>();
    let mut out_length = txt.len() * _2 + 32;
    let mut mapped_text = vec![0_u16; out_length / 2];
    loop
    /* try the mapping */
    {
        let status = teckit::TECkit_ConvertBuffer(
            cnv,
            txt.as_ptr() as *const u8,
            (txt.len() * _2) as u32,
            &mut inUsed,
            mapped_text.as_mut_ptr() as *mut u8,
            out_length as _,
            &mut outUsed,
            1i32 as u8,
        );
        match status {
            0 => {
                mapped_text.truncate((outUsed as usize) / 2);
                return mapped_text;
            }
            1 => {
                out_length += txt.len() * _2 + 32;
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
            let mut ht: f32 = 0.0f64 as f32;
            let mut dp: f32 = 0.0f64 as f32;
            let gid: libc::c_int = aat::MapCharToGlyph_AAT(*attributes, ch);
            aat::GetGlyphHeightDepth_AAT(*attributes, gid as u16, &mut ht, &mut dp);
            (ht, dp)
        }
        Font::Native(Otgr(engine)) => {
            let gid = engine.map_char_to_glyph(ch) as i32;
            engine.get_glyph_height_depth(gid as u32)
        }
        _ => panic!("bad native font flag in `get_native_char_height_depth`"),
    };
    let mut height = D2Fix(ht as f64);
    let mut depth = D2Fix(dp as f64);
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
            engine.get_glyph_sidebearings(gid as u32)
        }
    };
    (D2Fix(l as f64), D2Fix(r as f64))
}
pub(crate) unsafe fn get_glyph_bounds(font: usize, edge: i32, gid: i32) -> Scaled {
    /* edge codes 1,2,3,4 => L T R B */
    let (a, b) = match &FONT_LAYOUT_ENGINE[font] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(attributes)) => {
            let mut a: f32 = 0.;
            let mut b: f32 = 0.;
            if edge & 1i32 != 0 {
                aat::GetGlyphSidebearings_AAT(*attributes, gid as u16, &mut a, &mut b);
            } else {
                aat::GetGlyphHeightDepth_AAT(*attributes, gid as u16, &mut a, &mut b);
            }
            (a, b)
        }
        Font::Native(Otgr(engine)) => {
            if edge & 1i32 != 0 {
                engine.get_glyph_sidebearings(gid as u32)
            } else {
                engine.get_glyph_height_depth(gid as u32)
            }
        }
        _ => abort!("bad native font flag in `get_glyph_bounds`"),
    };
    D2Fix((if edge <= 2i32 { a } else { b }) as f64)
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
            D2Fix(aat::GetGlyphWidth_AAT(*attributes, gid as u16))
        }
        Font::Native(Otgr(engine)) => {
            let gid = engine.map_char_to_glyph(c) as i32;
            D2Fix(engine.get_glyph_width_from_engine(gid as u32) as f64)
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
                let justAmount = Fix2D(savedWidth - node.width());
                /* apply justification to spaces (or if there are none, distribute it to all glyphs as a last resort) */
                let glyph_count = node.glyph_count() as usize;
                let mut spaceCount: i32 = 0i32;
                let spaceGlyph: i32 = map_char_to_glyph(nf, ' ');
                for i in 0..glyph_count {
                    if node.glyph_ids()[i] as i32 == spaceGlyph {
                        spaceCount += 1
                    }
                }
                if spaceCount > 0i32 {
                    let mut adjustment: f64 = 0i32 as f64;
                    let mut spaceIndex: i32 = 0i32;
                    for i in 0..glyph_count as usize {
                        let loc = &mut node.locations_mut()[i];
                        loc.x = D2Fix(Fix2D(loc.x) + adjustment);
                        if node.glyph_ids()[i] as i32 == spaceGlyph {
                            spaceIndex += 1;
                            adjustment = justAmount * spaceIndex as f64 / spaceCount as f64
                        }
                    }
                } else {
                    for i in 1..glyph_count {
                        let loc = &mut node.locations_mut()[i];
                        loc.x =
                            D2Fix(Fix2D(loc.x) + justAmount * i as f64 / (glyph_count - 1) as f64);
                    }
                }
                node.set_width(savedWidth);
            };
        }
    }
}
pub(crate) unsafe fn measure_native_node(node: &mut NativeWord, use_glyph_metrics: bool) {
    let txt = node.text();
    let f = node.font() as usize;
    if let Font::Native(Otgr(engine)) = &mut FONT_LAYOUT_ENGINE[f] {
        /* using this font in OT Layout mode, so FONT_LAYOUT_ENGINE[f] is actually a *mut XeTeXLayoutEngine */
        let mut locations: *mut FixedPoint = ptr::null_mut();
        let mut glyphAdvances: *mut Scaled = ptr::null_mut();
        let mut totalGlyphCount: i32 = 0i32;
        /* need to find direction runs within the text, and call layoutChars separately for each */
        let mut glyph_info: *mut libc::c_void = ptr::null_mut();
        let pBiDi: *mut icu::UBiDi = icu::ubidi_open();
        let mut errorCode: icu::UErrorCode = icu::U_ZERO_ERROR;
        icu::ubidi_setPara(
            pBiDi,
            txt.as_ptr() as *const icu::UChar,
            txt.len() as i32,
            engine.get_default_direction() as icu::UBiDiLevel,
            ptr::null_mut(),
            &mut errorCode,
        );
        let mut dir = icu::ubidi_getDirection(pBiDi);
        if dir as u32 == icu::UBIDI_MIXED as i32 as u32 {
            /* we actually do the layout twice here, once to count glyphs and then again to get them;
               which is inefficient, but i figure that MIXED is a relatively rare occurrence, so i can't be
               bothered to deal with the memory reallocation headache of doing it differently
            */
            let nRuns: i32 = icu::ubidi_countRuns(pBiDi, &mut errorCode);
            let mut width: f64 = 0i32 as f64;
            let mut logicalStart: i32 = 0;
            let mut length: i32 = 0;
            for runIndex in 0..nRuns {
                dir = icu::ubidi_getVisualRun(pBiDi, runIndex, &mut logicalStart, &mut length);
                totalGlyphCount += engine.layout_chars(
                    txt,
                    logicalStart,
                    length,
                    dir as u32 == icu::UBIDI_RTL as i32 as u32,
                );
            }
            if totalGlyphCount > 0 {
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t);
                locations = glyph_info as *mut FixedPoint;
                let glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(
                    totalGlyphCount as size_t,
                    ::std::mem::size_of::<Scaled>() as _,
                ) as *mut Scaled;
                totalGlyphCount = 0i32;
                let mut y = 0.0f64;
                let mut x = y;
                for runIndex in 0..nRuns {
                    dir = icu::ubidi_getVisualRun(pBiDi, runIndex, &mut logicalStart, &mut length);
                    let nGlyphs = engine.layout_chars(
                        txt,
                        logicalStart,
                        length,
                        dir as u32 == icu::UBIDI_RTL as i32 as u32,
                    );
                    let glyphs = engine.get_glyphs();
                    let advances = engine.get_glyph_advances();
                    let positions = engine.get_glyph_positions();
                    for i in 0..nGlyphs {
                        *glyphIDs.offset(totalGlyphCount as isize) = glyphs[i as usize] as u16;
                        (*locations.offset(totalGlyphCount as isize)).x =
                            D2Fix(positions[i as usize].x as f64 + x);
                        (*locations.offset(totalGlyphCount as isize)).y =
                            D2Fix(positions[i as usize].y as f64 + y);
                        *glyphAdvances.offset(totalGlyphCount as isize) =
                            D2Fix(advances[i as usize] as f64);
                        totalGlyphCount += 1;
                    }
                    x += positions[nGlyphs as usize].x as f64;
                    y += positions[nGlyphs as usize].y as f64;
                }
                width = x
            }
            node.set_width(D2Fix(width));
            node.set_glyph_count(totalGlyphCount as u16);
            node.set_glyph_info_ptr(glyph_info);
        } else {
            let mut width_0: f64 = 0i32 as f64;
            totalGlyphCount = engine.layout_chars(
                txt,
                0i32,
                txt.len() as i32,
                dir as u32 == icu::UBIDI_RTL as i32 as u32,
            );
            let glyphs = engine.get_glyphs();
            let advances = engine.get_glyph_advances();
            let positions = engine.get_glyph_positions();
            if totalGlyphCount > 0 {
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t);
                locations = glyph_info as *mut FixedPoint;
                let glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(totalGlyphCount as size_t, ::std::mem::size_of::<Scaled>())
                    as *mut Scaled;
                for i_0 in 0..totalGlyphCount {
                    *glyphIDs.offset(i_0 as isize) = glyphs[i_0 as usize] as u16;
                    *glyphAdvances.offset(i_0 as isize) = D2Fix(advances[i_0 as usize] as f64);
                    (*locations.offset(i_0 as isize)).x = D2Fix(positions[i_0 as usize].x as f64);
                    (*locations.offset(i_0 as isize)).y = D2Fix(positions[i_0 as usize].y as f64);
                }
                width_0 = positions[totalGlyphCount as usize].x as f64
            }
            node.set_width(D2Fix(width_0));
            node.set_glyph_count(totalGlyphCount as u16);
            node.set_glyph_info_ptr(glyph_info);
        }
        icu::ubidi_close(pBiDi);
        if FONT_LETTER_SPACE[f] != Scaled::ZERO {
            let mut lsDelta = Scaled::ZERO;
            let lsUnit = FONT_LETTER_SPACE[f];
            for i_1 in 0..totalGlyphCount {
                if *glyphAdvances.offset(i_1 as isize) == Scaled::ZERO && lsDelta != Scaled::ZERO {
                    lsDelta -= lsUnit
                }
                (*locations.offset(i_1 as isize)).x += lsDelta;
                lsDelta += lsUnit;
            }
            if lsDelta != Scaled::ZERO {
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
        node.set_height(Scaled(HEIGHT_BASE[f]));
        node.set_depth(Scaled(DEPTH_BASE[f]));
    } else {
        /* this iterates over the glyph data whether it comes from AAT or OT layout */
        let locations = node.locations(); /* NB negative is upwards in locations[].y! */
        let glyph_ids = node.glyph_ids();
        let mut yMin: f32 = 65536.0f64 as f32;
        let mut yMax: f32 = -65536.0f64 as f32;
        for i in 0..node.glyph_count() as usize {
            let y_0: f32 = Fix2D(-locations[i].y) as f32;
            let mut bbox: GlyphBBox = GlyphBBox {
                xMin: 0.,
                yMin: 0.,
                xMax: 0.,
                yMax: 0.,
            };
            if getCachedGlyphBBox(f as u16, glyph_ids[i], &mut bbox) == 0i32 {
                match &FONT_LAYOUT_ENGINE[f] {
                    #[cfg(target_os = "macos")]
                    Font::Native(Aat(engine)) => {
                        aat::GetGlyphBBox_AAT(*engine, glyph_ids[i], &mut bbox);
                    }
                    Font::Native(Otgr(engine)) => {
                        engine.get_glyph_bounds(glyph_ids[i] as u32, &mut bbox);
                    }
                    _ => {}
                }
                cacheGlyphBBox(f as u16, glyph_ids[i], &bbox);
            }
            let ht = bbox.yMax;
            let dp = -bbox.yMin;
            if y_0 + ht > yMax {
                yMax = y_0 + ht
            }
            if y_0 - dp < yMin {
                yMin = y_0 - dp
            }
        }
        node.set_height(D2Fix(yMax as f64));
        node.set_depth(-D2Fix(yMin as f64));
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
                D2Fix(aat::GetGlyphItalCorr_AAT(*engine, glyph_ids[n - 1])) + FONT_LETTER_SPACE[f]
            }
            Font::Native(Otgr(engine)) => {
                D2Fix(engine.get_glyph_ital_corr(glyph_ids[n - 1] as u32) as f64)
                    + FONT_LETTER_SPACE[f]
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
        Font::Native(Aat(engine)) => D2Fix(aat::GetGlyphItalCorr_AAT(*engine, gid)),
        Font::Native(Otgr(engine)) => D2Fix(engine.get_glyph_ital_corr(gid as u32) as f64),
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
            node.set_width(D2Fix(aat::GetGlyphWidth_AAT(*attributes, gid)));
            let mut ht = 0_f32;
            let mut dp = 0_f32;
            if use_glyph_metrics {
                aat::GetGlyphHeightDepth_AAT(*attributes, gid, &mut ht, &mut dp);
            }
            (ht, dp)
        }
        Font::Native(Otgr(engine)) => {
            let fontInst = engine.get_font();
            node.set_width(D2Fix(fontInst.get_glyph_width(gid as u16) as f64));
            if use_glyph_metrics {
                engine.get_glyph_height_depth(gid as u32)
            } else {
                (0., 0.)
            }
        }
        _ => panic!("bad native font flag in `measure_native_glyph`"),
    };
    if use_glyph_metrics {
        node.set_height(D2Fix(ht as f64));
        node.set_depth(D2Fix(dp as f64));
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
        Otgr(engine) => mapGlyphToIndex(engine, name.as_ptr()),
    }
}
pub(crate) unsafe fn get_font_char_range(font: usize, first: i32) -> i32 {
    match &mut FONT_LAYOUT_ENGINE[font] {
        #[cfg(target_os = "macos")]
        Font::Native(Aat(engine)) => aat::GetFontCharRange_AAT(*engine, first),
        Font::Native(Otgr(engine)) => engine.get_font_char_range(first),
        _ => panic!("bad native font flag in `get_font_char_range\'`"),
    }
}
pub(crate) unsafe fn D2Fix(d: f64) -> Scaled {
    Scaled((d * 65536.0f64 + 0.5f64) as i32)
}
pub(crate) unsafe fn Fix2D(f: Scaled) -> f64 {
    f.0 as f64 / 65536.
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
