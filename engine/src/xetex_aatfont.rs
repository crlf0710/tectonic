#![cfg(target_os = "macos")]
#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use super::xetex_layout_interface::GlyphBBox;

use crate::cf_prelude::*;

use core_foundation::base::TCFType;
use core_foundation::string::CFString;
use core_foundation::url::CFURL;
use freetype::Library as FreeTypeLibrary;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ptr;

use crate::core_memory::{xcalloc, xmalloc};
use crate::xetex_consts::ExtCmd;
use crate::xetex_ext::{print_chars, readCommonFeatures, read_double, D2Fix, Fix2D};
use crate::xetex_ini::memory_word;
use crate::xetex_ini::{
    loaded_font_flags, loaded_font_letter_space, name_length, name_of_file, native_font_type_flag,
    FONT_AREA, FONT_LAYOUT_ENGINE, FONT_LETTER_SPACE,
};
use crate::xetex_xetex0::font_feature_warning;
use libc::{free, strlen};
type int32_t = libc::c_int;
type uint16_t = libc::c_ushort;
pub(crate) type Boolean = libc::c_uchar;
type scaled_t = int32_t;
type UInt32 = libc::c_uint;
type SInt32 = libc::c_int;

type Fixed = SInt32;
type Fract = SInt32;
#[derive(Copy, Clone)]
#[repr(C, packed(2))]
struct FixedPoint {
    x: Fixed,
    y: Fixed,
}

pub(crate) type str_number = int32_t;
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
use crate::strstartswith;
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew
 Copyright (c) 2012, 2013 by Jiang Jiang
 Copyright (c) 2012-2015 by Khaled Hosny

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
/* XeTeX_mac.c
 * additional plain C extensions for XeTeX - MacOS-specific routines
 */
#[inline]
unsafe fn TeXtoPSPoints(mut pts: f64) -> f64 {
    return pts * 72.0f64 / 72.27f64;
}
#[inline]
unsafe fn PStoTeXPoints(mut pts: f64) -> f64 {
    return pts * 72.27f64 / 72.0f64;
}
#[inline]
unsafe fn FixedPStoTeXPoints(mut pts: f64) -> Fixed {
    return D2Fix(PStoTeXPoints(pts));
}

unsafe fn font_from_attributes(mut attributes: CFDictionaryRef) -> CTFontRef {
    return CFDictionaryGetValue(attributes, kCTFontAttributeName as *const libc::c_void)
        as CTFontRef;
}

pub(crate) unsafe fn font_from_integer(mut font: usize) -> CTFontRef {
    let mut attributes: CFDictionaryRef = FONT_LAYOUT_ENGINE[font] as CFDictionaryRef;
    return font_from_attributes(attributes);
}

pub(crate) unsafe fn do_aat_layout(mut p: *mut libc::c_void, mut justify: libc::c_int) {
    let mut glyphRuns: CFArrayRef = ptr::null_mut();
    let mut i: CFIndex = 0;
    let mut j: CFIndex = 0;
    let mut runCount: CFIndex = 0;
    let mut totalGlyphCount: CFIndex = 0;
    let mut glyphIDs: *mut UInt16 = ptr::null_mut();
    let mut glyphAdvances: *mut Fixed = ptr::null_mut();
    let mut glyph_info: *mut libc::c_void = ptr::null_mut();
    let mut locations: *mut FixedPoint = ptr::null_mut();
    let mut width: CGFloat = 0.;
    let mut txtLen: CFIndex = 0;
    let mut txtPtr: *const UniChar = ptr::null_mut();
    let mut attributes: CFDictionaryRef = ptr::null_mut();
    let mut string: CFStringRef = ptr::null_mut();
    let mut attrString: CFAttributedStringRef = ptr::null_mut();
    let mut typesetter: CTTypesetterRef = 0 as CTTypesetterRef;
    let mut line: CTLineRef = ptr::null_mut();
    let mut node: *mut memory_word = p as *mut memory_word;
    let mut f: libc::c_uint = (*node.offset(4)).b16.s2 as libc::c_uint;
    if FONT_AREA[f as usize] as libc::c_uint != 0xffffu32 {
        panic!("do_aat_layout called for non-AAT font");
    }
    txtLen = (*node.offset(4)).b16.s1 as CFIndex;
    txtPtr = node.offset(6) as *mut UniChar;
    attributes = FONT_LAYOUT_ENGINE[(*node.offset(4)).b16.s2 as usize] as CFDictionaryRef;
    string = CFStringCreateWithCharactersNoCopy(ptr::null(), txtPtr, txtLen, kCFAllocatorNull);
    attrString = CFAttributedStringCreate(ptr::null(), string, attributes);
    CFRelease(string as CFTypeRef);
    typesetter = CTTypesetterCreateWithAttributedString(attrString);
    CFRelease(attrString as CFTypeRef);
    line = CTTypesetterCreateLine(typesetter, CFRangeMake(0i32 as CFIndex, txtLen));
    if justify != 0 {
        let mut lineWidth: CGFloat = TeXtoPSPoints(Fix2D((*node.offset(1)).b32.s1));
        let mut justifiedLine: CTLineRef = CTLineCreateJustifiedLine(
            line,
            TeXtoPSPoints(Fix2D(0x40000000i64 as Fract)),
            lineWidth,
        );
        // TODO(jjgod): how to handle the case when justification failed? for
        // now we just fallback to use the original line.
        if !justifiedLine.is_null() {
            CFRelease(line as CFTypeRef);
            line = justifiedLine
        }
    }
    glyphRuns = CTLineGetGlyphRuns(line);
    runCount = CFArrayGetCount(glyphRuns);
    totalGlyphCount = CTLineGetGlyphCount(line);
    if totalGlyphCount > 0 {
        glyph_info = xmalloc((totalGlyphCount * 10) as _);
        locations = glyph_info as *mut FixedPoint;
        glyphIDs = locations.offset(totalGlyphCount as isize) as *mut UInt16;
        glyphAdvances =
            xmalloc((totalGlyphCount as usize).wrapping_mul(::std::mem::size_of::<Fixed>()) as _)
                as *mut Fixed;
        totalGlyphCount = 0i32 as CFIndex;
        width = 0i32 as CGFloat;
        i = 0i32 as CFIndex;
        while i < runCount {
            let mut run: CTRunRef = CFArrayGetValueAtIndex(glyphRuns, i) as CTRunRef;
            let mut count: CFIndex = CTRunGetGlyphCount(run);
            let mut runAttributes: CFDictionaryRef = CTRunGetAttributes(run);
            let mut vertical: CFBooleanRef = CFDictionaryGetValue(
                runAttributes,
                kCTVerticalFormsAttributeName as *const libc::c_void,
            ) as CFBooleanRef;
            // TODO(jjgod): Avoid unnecessary allocation with CTRunGetFoosPtr().
            let mut glyphs: *mut CGGlyph =
                xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGGlyph>()) as _)
                    as *mut CGGlyph;
            let mut positions: *mut CGPoint =
                xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGPoint>()) as _)
                    as *mut CGPoint;
            let mut advances: *mut CGSize =
                xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGSize>()) as _)
                    as *mut CGSize;
            let mut runWidth: CGFloat = CTRunGetTypographicBounds(
                run,
                CFRangeMake(0i32 as CFIndex, 0i32 as CFIndex),
                ptr::null_mut(),
                ptr::null_mut(),
                ptr::null_mut(),
            );
            CTRunGetGlyphs(run, CFRangeMake(0i32 as CFIndex, 0i32 as CFIndex), glyphs);
            CTRunGetPositions(
                run,
                CFRangeMake(0i32 as CFIndex, 0i32 as CFIndex),
                positions,
            );
            CTRunGetAdvances(run, CFRangeMake(0i32 as CFIndex, 0i32 as CFIndex), advances);
            j = 0i32 as CFIndex;
            while j < count {
                // XXX Core Text has that font cascading thing that will do
                // font substitution for missing glyphs, which we do not want
                // but I can not find a way to disable it yet, so if the font
                // of the resulting run is not the same font we asked for, use
                // the glyph at index 0 (usually .notdef) instead or we will be
                // showing garbage or even invalid glyphs
                if CFEqual(
                    font_from_attributes(attributes) as CFTypeRef,
                    font_from_attributes(runAttributes) as CFTypeRef,
                ) == 0
                {
                    *glyphIDs.offset(totalGlyphCount as isize) = 0i32 as UInt16
                } else {
                    *glyphIDs.offset(totalGlyphCount as isize) = *glyphs.offset(j as isize)
                }
                // Swap X and Y when doing vertical layout
                if vertical == kCFBooleanTrue {
                    (*locations.offset(totalGlyphCount as isize)).x =
                        -FixedPStoTeXPoints((*positions.offset(j as isize)).y);
                    (*locations.offset(totalGlyphCount as isize)).y =
                        FixedPStoTeXPoints((*positions.offset(j as isize)).x)
                } else {
                    (*locations.offset(totalGlyphCount as isize)).x =
                        FixedPStoTeXPoints((*positions.offset(j as isize)).x);
                    (*locations.offset(totalGlyphCount as isize)).y =
                        -FixedPStoTeXPoints((*positions.offset(j as isize)).y)
                }
                *glyphAdvances.offset(totalGlyphCount as isize) =
                    (*advances.offset(j as isize)).width as Fixed;
                totalGlyphCount += 1;
                j += 1
            }
            width += FixedPStoTeXPoints(runWidth) as f64;
            free(glyphs as *mut libc::c_void);
            free(positions as *mut libc::c_void);
            free(advances as *mut libc::c_void);
            i += 1
        }
    }
    (*node.offset(4)).b16.s0 = totalGlyphCount as uint16_t;
    let ref mut fresh0 = (*node.offset(5)).ptr;
    *fresh0 = glyph_info;
    if justify == 0 {
        (*node.offset(1)).b32.s1 = width as int32_t;
        if totalGlyphCount > 0 {
            /* this is essentially a copy from similar code in XeTeX_ext.c, easier
             * to be done here */
            if FONT_LETTER_SPACE[f as usize] != 0 {
                let mut lsDelta: Fixed = 0i32;
                let mut lsUnit: Fixed = FONT_LETTER_SPACE[f as usize];
                let mut i_0 = 0;
                while i_0 < totalGlyphCount {
                    if *glyphAdvances.offset(i_0 as isize) == 0i32 && lsDelta != 0i32 {
                        lsDelta -= lsUnit
                    }
                    let ref mut fresh1 = (*locations.offset(i_0 as isize)).x;
                    *fresh1 += lsDelta;
                    lsDelta += lsUnit;
                    i_0 += 1
                }
                if lsDelta != 0i32 {
                    lsDelta -= lsUnit;
                    let ref mut fresh2 = (*node.offset(1)).b32.s1;
                    *fresh2 += lsDelta
                }
            }
        }
    }
    free(glyphAdvances as *mut libc::c_void);
    CFRelease(line as CFTypeRef);
    CFRelease(typesetter as CFTypeRef);
}

unsafe fn getGlyphBBoxFromCTFont(mut font: CTFontRef, mut gid: UInt16, mut bbox: *mut GlyphBBox) {
    let mut rect: CGRect = CGRect {
        origin: CGPoint { x: 0., y: 0. },
        size: CGSize {
            width: 0.,
            height: 0.,
        },
    };
    (*bbox).xMin = 65536.0f64 as libc::c_float;
    (*bbox).yMin = 65536.0f64 as libc::c_float;
    (*bbox).xMax = -65536.0f64 as libc::c_float;
    (*bbox).yMax = -65536.0f64 as libc::c_float;
    rect = CTFontGetBoundingRectsForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut UInt16 as *const CGGlyph,
        ptr::null_mut(),
        1i32 as CFIndex,
    );
    if CGRectIsNull(rect) {
        (*bbox).yMax = 0i32 as libc::c_float;
        (*bbox).xMax = (*bbox).yMax;
        (*bbox).yMin = (*bbox).xMax;
        (*bbox).xMin = (*bbox).yMin
    } else {
        (*bbox).yMin = PStoTeXPoints(rect.origin.y) as libc::c_float;
        (*bbox).yMax = PStoTeXPoints(rect.origin.y + rect.size.height) as libc::c_float;
        (*bbox).xMin = PStoTeXPoints(rect.origin.x) as libc::c_float;
        (*bbox).xMax = PStoTeXPoints(rect.origin.x + rect.size.width) as libc::c_float
    };
}

/// returns glyph bounding box in TeX points
pub(crate) unsafe fn GetGlyphBBox_AAT(
    mut attributes: CFDictionaryRef,
    mut gid: UInt16,
    mut bbox: *mut GlyphBBox,
) {
    let mut font: CTFontRef = font_from_attributes(attributes);
    return getGlyphBBoxFromCTFont(font, gid, bbox);
}

unsafe fn getGlyphWidthFromCTFont(mut font: CTFontRef, mut gid: UInt16) -> f64 {
    return PStoTeXPoints(CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationHorizontal,
        &mut gid as *mut UInt16 as *const CGGlyph,
        ptr::null_mut(),
        1i32 as CFIndex,
    ));
}

/// returns TeX points
pub(crate) unsafe fn GetGlyphWidth_AAT(mut attributes: CFDictionaryRef, mut gid: UInt16) -> f64 {
    let mut font: CTFontRef = font_from_attributes(attributes);
    return getGlyphWidthFromCTFont(font, gid);
}

// returns TeX points
pub(crate) unsafe fn GetGlyphHeightDepth_AAT(
    mut attributes: CFDictionaryRef,
    mut gid: UInt16,
    mut ht: *mut libc::c_float,
    mut dp: *mut libc::c_float,
) {
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    GetGlyphBBox_AAT(attributes, gid, &mut bbox);
    *ht = bbox.yMax;
    *dp = -bbox.yMin;
}

/// returns TeX points
pub(crate) unsafe fn GetGlyphSidebearings_AAT(
    mut attributes: CFDictionaryRef,
    mut gid: UInt16,
    mut lsb: *mut libc::c_float,
    mut rsb: *mut libc::c_float,
) {
    let mut font: CTFontRef = font_from_attributes(attributes);
    let mut advances: [CGSize; 1] = [CGSizeMake(0i32 as CGFloat, 0i32 as CGFloat)];
    let mut advance: f64 = CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut UInt16 as *const CGGlyph,
        advances.as_mut_ptr(),
        1i32 as CFIndex,
    );
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    getGlyphBBoxFromCTFont(font, gid, &mut bbox);
    *lsb = bbox.xMin;
    *rsb = (PStoTeXPoints(advance) - bbox.xMax as f64) as libc::c_float;
}
#[inline]
unsafe fn CGSizeMake(mut width: CGFloat, mut height: CGFloat) -> CGSize {
    let mut size: CGSize = CGSize {
        width: 0.,
        height: 0.,
    };
    size.width = width;
    size.height = height;
    return size;
}

pub(crate) unsafe fn GetGlyphItalCorr_AAT(mut attributes: CFDictionaryRef, mut gid: UInt16) -> f64 {
    let mut font: CTFontRef = font_from_attributes(attributes);
    let mut advances: [CGSize; 1] = [CGSizeMake(0i32 as CGFloat, 0i32 as CGFloat)];
    let mut advance: f64 = CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut UInt16 as *const CGGlyph,
        advances.as_mut_ptr(),
        1i32 as CFIndex,
    );
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    getGlyphBBoxFromCTFont(font, gid, &mut bbox);
    if bbox.xMax as f64 > PStoTeXPoints(advance) {
        return bbox.xMax as f64 - PStoTeXPoints(advance);
    }
    return 0i32 as f64;
}
unsafe fn mapCharToGlyphFromCTFont(mut font: CTFontRef, mut ch: UInt32) -> libc::c_int {
    let mut glyphs: [CGGlyph; 2] = [0i32 as CGGlyph, 0];
    let mut txt: [UniChar; 2] = [0; 2];
    let mut len: libc::c_int = 1i32;
    if ch > 0xffffi32 as libc::c_uint {
        ch = (ch as libc::c_uint).wrapping_sub(0x10000i32 as libc::c_uint) as UInt32 as UInt32;
        txt[0] = (0xd800i32 as libc::c_uint).wrapping_add(ch.wrapping_div(1024i32 as libc::c_uint))
            as UniChar;
        txt[1] = (0xdc00i32 as libc::c_uint).wrapping_add(ch.wrapping_rem(1024i32 as libc::c_uint))
            as UniChar;
        len = 2i32
    } else {
        txt[0] = ch as UniChar
    }
    if CTFontGetGlyphsForCharacters(
        font,
        txt.as_mut_ptr() as *const UniChar,
        glyphs.as_mut_ptr(),
        len as CFIndex,
    ) {
        return glyphs[0] as libc::c_int;
    }
    return 0i32;
}

pub(crate) unsafe fn MapCharToGlyph_AAT(
    mut attributes: CFDictionaryRef,
    mut ch: UInt32,
) -> libc::c_int {
    let mut font: CTFontRef = font_from_attributes(attributes);
    return mapCharToGlyphFromCTFont(font, ch);
}

unsafe fn GetGlyphIDFromCTFont(
    mut ctFontRef: CTFontRef,
    mut glyphName: *const libc::c_char,
) -> libc::c_int {
    let mut glyphname: CFStringRef = CFStringCreateWithCStringNoCopy(
        kCFAllocatorDefault,
        glyphName,
        kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        kCFAllocatorNull,
    );
    let mut rval: libc::c_int = CTFontGetGlyphWithName(ctFontRef, glyphname) as libc::c_int;
    CFRelease(glyphname as CFTypeRef);
    return rval;
}

/* single-purpose metrics accessors */
/* the metrics params here are really TeX 'scaled' (or MacOS 'Fixed') values, but that typedef isn't available every place this is included */
/* functions in XeTeX_mac.c */
pub(crate) unsafe fn MapGlyphToIndex_AAT(
    mut attributes: CFDictionaryRef,
    mut glyphName: *const libc::c_char,
) -> libc::c_int {
    let mut font: CTFontRef = font_from_attributes(attributes);
    return GetGlyphIDFromCTFont(font, glyphName);
}

pub(crate) unsafe fn GetGlyphNameFromCTFont(
    mut ctFontRef: CTFontRef,
    mut gid: UInt16,
    mut len: *mut libc::c_int,
) -> *mut libc::c_char {
    let mut cgfont: CGFontRef = 0 as CGFontRef;
    static mut buffer: [libc::c_char; 256] = [0; 256];
    buffer[0] = 0i32 as libc::c_char;
    *len = 0i32;
    cgfont = CTFontCopyGraphicsFont(ctFontRef, ptr::null_mut());
    if !cgfont.is_null() && (gid as usize) < CGFontGetNumberOfGlyphs(cgfont) {
        let mut glyphname: CFStringRef = CGFontCopyGlyphNameForGlyph(cgfont, gid);
        if !glyphname.is_null() {
            if CFStringGetCString(
                glyphname,
                buffer.as_mut_ptr(),
                256i32 as CFIndex,
                kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
            ) != 0
            {
                *len = strlen(buffer.as_mut_ptr()) as libc::c_int
            }
            CFRelease(glyphname as CFTypeRef);
        }
        CGFontRelease(cgfont);
    }
    return &mut *buffer.as_mut_ptr().offset(0) as *mut libc::c_char;
}

pub(crate) unsafe fn GetFontCharRange_AAT(
    mut attributes: CFDictionaryRef,
    mut reqFirst: libc::c_int,
) -> libc::c_int {
    if reqFirst != 0 {
        let mut ch: libc::c_int = 0i32;
        while MapCharToGlyph_AAT(attributes, ch as UInt32) == 0i32 && ch < 0x10ffffi32 {
            ch += 1
        }
        return ch;
    } else {
        let mut ch_0: libc::c_int = 0x10ffffi32;
        while MapCharToGlyph_AAT(attributes, ch_0 as UInt32) == 0i32 && ch_0 > 0i32 {
            ch_0 -= 1
        }
        return ch_0;
    };
}

// CFString wrapper takes ownership & frees on drop
unsafe fn ct_font_get_postscript_name(ctFontRef: CTFontRef, nameKey: CFStringRef) -> CFString {
    let name: CFStringRef = CTFontCopyName(ctFontRef, nameKey);
    // Owned, by "Create Rule" because "Copy" in name
    CFString::wrap_under_create_rule(name)
}

thread_local!(static FREETYPE_LIBRARY: RefCell<FreeTypeLibrary> = RefCell::new(FreeTypeLibrary::init().unwrap()));

// This needs to be linked from C++, hence extern "C"
pub(crate) unsafe fn getFileNameFromCTFont(
    mut ctFontRef: CTFontRef,
    mut index: *mut u32,
) -> *mut i8 {
    let mut ix: i32 = -1;
    let mut ret: *mut libc::c_char = ptr::null_mut();
    let urlRef = CTFontCopyAttribute(ctFontRef, kCTFontURLAttribute) as CFURLRef;
    if !urlRef.is_null() {
        let url = CFURL::wrap_under_create_rule(urlRef);
        if let Some(pathbuf) = url.to_path() {
            let ps_name1 = ct_font_get_postscript_name(ctFontRef, kCTFontPostScriptNameKey);
            let ps_name = Cow::from(&ps_name1);

            let mut i: isize = 0;
            while let Ok(face) = FREETYPE_LIBRARY.with(|l| l.borrow().new_face(&pathbuf, i)) {
                if let Some(ps_name2) = face.postscript_name() {
                    if ps_name2 == ps_name {
                        ix = i as i32;
                        break;
                    }
                }
                i += 1;
            }
            if ix > -1 {
                *index = ix as u32;
                let osstr = pathbuf.as_os_str();
                #[cfg(unix)]
                {
                    use std::os::unix::ffi::OsStrExt;
                    let bytes = osstr.as_bytes();
                    ret =
                        xcalloc((bytes.len() + 1) as _, std::mem::size_of::<i8>() as _) as *mut i8;
                    for i in 0..bytes.len() {
                        *ret.offset(i as isize) = bytes[i] as i8;
                    }
                }
                #[cfg(not(unix))]
                {
                    // On Windows, given the limitations of the bridge API, we don't actually
                    // support full-on OsStrings anyway, so we'll just work with utf8.
                    let cstring = CString::from(osstr.to_string_lossy());
                    let bytes = cstring.as_bytes();
                    ret = strdup(bytes.as_ptr());
                }
            }
        }
    }
    return ret;
}

pub(crate) unsafe fn findDictionaryInArrayWithIdentifier(
    mut array: CFArrayRef,
    mut identifierKey: *const libc::c_void,
    mut identifier: libc::c_int,
) -> CFDictionaryRef {
    let mut dict: CFDictionaryRef = 0 as CFDictionaryRef;
    if !array.is_null() {
        let mut value: libc::c_int = -1i32;
        let mut i: CFIndex = 0;
        i = 0i32 as CFIndex;
        while i < CFArrayGetCount(array) {
            let mut item: CFDictionaryRef = CFArrayGetValueAtIndex(array, i) as CFDictionaryRef;
            let mut itemId: CFNumberRef = CFDictionaryGetValue(item, identifierKey) as CFNumberRef;
            if !itemId.is_null() {
                CFNumberGetValue(
                    itemId,
                    kCFNumberIntType as libc::c_int as CFNumberType,
                    &mut value as *mut libc::c_int as *mut libc::c_void,
                );
                if value == identifier {
                    dict = item;
                    break;
                }
            }
            i += 1
        }
    }
    return dict;
}

#[inline(always)]
unsafe fn CFRangeMake(mut loc: CFIndex, mut len: CFIndex) -> CFRange {
    let mut range: CFRange = CFRange {
        location: 0,
        length: 0,
    };
    range.location = loc;
    range.length = len;
    return range;
}

pub(crate) unsafe fn findDictionaryInArray(
    mut array: CFArrayRef,
    mut nameKey: *const libc::c_void,
    mut name: *const libc::c_char,
    mut nameLength: libc::c_int,
) -> CFDictionaryRef {
    let mut dict: CFDictionaryRef = 0 as CFDictionaryRef;
    if !array.is_null() {
        let mut itemName: CFStringRef = 0 as CFStringRef;
        let mut i: CFIndex = 0;
        itemName = CFStringCreateWithBytes(
            0 as CFAllocatorRef,
            name as *mut u8,
            nameLength as CFIndex,
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
            0i32 as Boolean,
        );
        i = 0i32 as CFIndex;
        while i < CFArrayGetCount(array) {
            let mut item: CFDictionaryRef = CFArrayGetValueAtIndex(array, i) as CFDictionaryRef;
            let mut iName: CFStringRef = CFDictionaryGetValue(item, nameKey) as CFStringRef;
            if !iName.is_null()
                && comparison_was(
                    CFStringCompare(itemName, iName, kCFCompareCaseInsensitive),
                    CFComparisonResult::EqualTo,
                )
            {
                dict = item;
                break;
            } else {
                i += 1
            }
        }
        CFRelease(itemName as CFTypeRef);
    }
    return dict;
}

pub(crate) unsafe fn findSelectorByName(
    mut feature: CFDictionaryRef,
    mut name: *const libc::c_char,
    mut nameLength: libc::c_int,
) -> CFNumberRef {
    let mut selector: CFNumberRef = 0 as CFNumberRef;
    let mut selectors: CFArrayRef = CFDictionaryGetValue(
        feature,
        kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
    ) as CFArrayRef;
    if !selectors.is_null() {
        let mut s: CFDictionaryRef = findDictionaryInArray(
            selectors,
            kCTFontFeatureSelectorNameKey as *const libc::c_void,
            name,
            nameLength,
        );
        if !s.is_null() {
            selector = CFDictionaryGetValue(
                s,
                kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
            ) as CFNumberRef
        }
    }
    return selector;
}
unsafe fn createFeatureSettingDictionary(
    mut featureTypeIdentifier: CFNumberRef,
    mut featureSelectorIdentifier: CFNumberRef,
) -> CFDictionaryRef {
    let mut settingKeys: [*const libc::c_void; 2] = [
        kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
        kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
    ];
    let mut settingValues: [*const libc::c_void; 2] = [
        featureTypeIdentifier as *const libc::c_void,
        featureSelectorIdentifier as *const libc::c_void,
    ];
    return CFDictionaryCreate(
        kCFAllocatorDefault,
        settingKeys.as_mut_ptr(),
        settingValues.as_mut_ptr(),
        2i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
}

// CFSTR causes undefined builtin errors with c2rust
static mut kXeTeXEmboldenAttributeName: CFStringRef = 0 as CFStringRef;
static mut kLastResort: CFStringRef = 0 as CFStringRef;

pub(crate) unsafe fn getkXeTeXEmboldenAttributeName() -> CFStringRef {
    if kXeTeXEmboldenAttributeName.is_null() {
        kXeTeXEmboldenAttributeName = CFStringCreateWithCString(
            0 as CFAllocatorRef,
            b"XeTeXEmbolden\x00" as *const u8 as *const libc::c_char,
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        )
    }
    return kXeTeXEmboldenAttributeName;
}

unsafe fn getLastResort() -> CFStringRef {
    if kLastResort.is_null() {
        kLastResort = CFStringCreateWithCString(
            0 as CFAllocatorRef,
            b"LastResort\x00" as *const u8 as *const libc::c_char,
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        )
    }
    return kLastResort;
}

pub(crate) unsafe fn loadAATfont(
    mut descriptor: CTFontDescriptorRef,
    mut scaled_size: int32_t,
    mut cp1: *const libc::c_char,
) -> *mut libc::c_void {
    let mut current_block: u64;
    let mut font: CTFontRef = 0 as CTFontRef;
    let mut actualFont: CTFontRef = 0 as CTFontRef;
    let mut ctSize: CGFloat = 0.;
    let mut stringAttributes: CFMutableDictionaryRef = 0 as CFMutableDictionaryRef;
    let mut attributes: CFMutableDictionaryRef = 0 as CFMutableDictionaryRef;
    let mut matrix: CGAffineTransform = CGAffineTransform {
        a: 0.,
        b: 0.,
        c: 0.,
        d: 0.,
        tx: 0.,
        ty: 0.,
    };
    let mut cascadeList: CFMutableArrayRef = 0 as CFMutableArrayRef;
    let mut lastResort: CTFontDescriptorRef = 0 as CTFontDescriptorRef;
    let mut tracking: f64 = 0.0f64;
    let mut extend: libc::c_float = 1.0f64 as libc::c_float;
    let mut slant: libc::c_float = 0.0f64 as libc::c_float;
    let mut embolden: libc::c_float = 0.0f64 as libc::c_float;
    let mut letterspace: libc::c_float = 0.0f64 as libc::c_float;
    let mut rgbValue: u32 = 0;
    // create a base font instance for applying further attributes
    ctSize = TeXtoPSPoints(Fix2D(scaled_size));
    font = CTFontCreateWithFontDescriptor(descriptor, ctSize, ptr::null());
    if font.is_null() {
        return ptr::null_mut();
    }
    stringAttributes = CFDictionaryCreateMutable(
        0 as CFAllocatorRef,
        0i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    attributes = CFDictionaryCreateMutable(
        0 as CFAllocatorRef,
        0i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    if !cp1.is_null() {
        let mut features: CFArrayRef = CTFontCopyFeatures(font);
        let mut featureSettings: CFMutableArrayRef =
            CFArrayCreateMutable(0 as CFAllocatorRef, 0i32 as CFIndex, &kCFTypeArrayCallBacks);
        // interpret features following ":"
        while *cp1 != 0 {
            let mut feature: CFDictionaryRef = 0 as CFDictionaryRef;
            let mut ret: libc::c_int = 0;
            let mut cp2: *const libc::c_char = ptr::null();
            let mut cp3: *const libc::c_char = ptr::null();
            // locate beginning of name=value pair
            if *cp1 as libc::c_int == ':' as i32 || *cp1 as libc::c_int == ';' as i32 {
                // skip over separator
                cp1 = cp1.offset(1)
            }
            while *cp1 as libc::c_int == ' ' as i32 || *cp1 as libc::c_int == '\t' as i32 {
                // skip leading whitespace
                cp1 = cp1.offset(1)
            }
            if *cp1 as libc::c_int == 0i32 {
                break;
            }
            // scan to end of pair
            cp2 = cp1;
            while *cp2 as libc::c_int != 0
                && *cp2 as libc::c_int != ';' as i32
                && *cp2 as libc::c_int != ':' as i32
            {
                cp2 = cp2.offset(1)
            }
            // look for the '=' separator
            cp3 = cp1;
            while cp3 < cp2 && *cp3 as libc::c_int != '=' as i32 {
                cp3 = cp3.offset(1)
            }
            if cp3 == cp2 {
                current_block = 4154772336439402900;
            } else {
                // now cp1 points to option name, cp3 to '=', cp2 to ';' or null
                // first try for a feature by this name
                feature = findDictionaryInArray(
                    features,
                    kCTFontFeatureTypeNameKey as *const libc::c_void,
                    cp1,
                    cp3.offset_from(cp1) as libc::c_long as libc::c_int,
                );
                if !feature.is_null() {
                    // look past the '=' separator for setting names
                    let mut featLen: libc::c_int =
                        cp3.offset_from(cp1) as libc::c_long as libc::c_int;
                    let mut zeroInteger: libc::c_int = 0i32;
                    let mut zero: CFNumberRef = CFNumberCreate(
                        0 as CFAllocatorRef,
                        kCFNumberIntType as libc::c_int as CFNumberType,
                        &mut zeroInteger as *mut libc::c_int as *const libc::c_void,
                    );
                    cp3 = cp3.offset(1);
                    while cp3 < cp2 {
                        let mut selector: CFNumberRef = 0 as CFNumberRef;
                        let mut disable: libc::c_int = 0i32;
                        let mut cp4: *const libc::c_char = ptr::null();
                        // skip leading whitespace
                        while *cp3 as libc::c_int == ' ' as i32
                            || *cp3 as libc::c_int == '\t' as i32
                        {
                            cp3 = cp3.offset(1)
                        }
                        // possibly multiple settings...
                        if *cp3 as libc::c_int == '!' as i32 {
                            // check for negation
                            disable = 1i32;
                            cp3 = cp3.offset(1)
                        }
                        // scan for end of setting name
                        cp4 = cp3;
                        while cp4 < cp2 && *cp4 as libc::c_int != ',' as i32 {
                            cp4 = cp4.offset(1)
                        }
                        // now cp3 points to name, cp4 to ',' or ';' or null
                        selector = findSelectorByName(
                            feature,
                            cp3,
                            cp4.offset_from(cp3) as libc::c_long as libc::c_int,
                        );
                        if !selector.is_null()
                            && comparison_was(
                                CFNumberCompare(selector, zero, ptr::null_mut()),
                                CFComparisonResult::GreaterThan,
                            )
                        {
                            let mut featureType: CFNumberRef = CFDictionaryGetValue(
                                feature,
                                kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                            )
                                as CFNumberRef;
                            let mut featureSetting: CFDictionaryRef =
                                createFeatureSettingDictionary(featureType, selector);
                            CFArrayAppendValue(
                                featureSettings,
                                featureSetting as *const libc::c_void,
                            );
                            CFRelease(featureSetting as CFTypeRef);
                        } else {
                            font_feature_warning(
                                std::slice::from_raw_parts(cp1 as *const u8, featLen as usize),
                                std::slice::from_raw_parts(
                                    cp3 as *const u8,
                                    cp4.offset_from(cp3) as usize,
                                ),
                            );
                        }
                        // point beyond setting name terminator
                        cp3 = cp4.offset(1)
                    }
                    CFRelease(zero as CFTypeRef);
                    current_block = 15938117740974259152;
                } else {
                    // didn't find feature, try other options...
                    ret = readCommonFeatures(
                        cp1,
                        cp2,
                        &mut extend,
                        &mut slant,
                        &mut embolden,
                        &mut letterspace,
                        &mut rgbValue,
                    );
                    if ret == 1i32 {
                        current_block = 15938117740974259152;
                    } else if ret == -1i32 {
                        current_block = 4154772336439402900;
                    } else {
                        cp3 =
                            strstartswith(cp1, b"tracking\x00" as *const u8 as *const libc::c_char);
                        if !cp3.is_null() {
                            let mut trackingNumber: CFNumberRef = 0 as CFNumberRef;
                            if *cp3 as libc::c_int != '=' as i32 {
                                current_block = 4154772336439402900;
                            } else {
                                cp3 = cp3.offset(1);
                                tracking = read_double(&mut cp3);
                                trackingNumber = CFNumberCreate(
                                    0 as CFAllocatorRef,
                                    kCFNumberDoubleType as libc::c_int as CFNumberType,
                                    &mut tracking as *mut f64 as *const libc::c_void,
                                );
                                CFDictionaryAddValue(
                                    stringAttributes,
                                    kCTKernAttributeName as *const libc::c_void,
                                    trackingNumber as *const libc::c_void,
                                );
                                CFRelease(trackingNumber as CFTypeRef);
                                current_block = 15938117740974259152;
                            }
                        } else {
                            current_block = 4154772336439402900;
                        }
                    }
                }
            }
            match current_block {
                4154772336439402900 =>
                // not a name=value pair, or not recognized....
                // check for plain "vertical" before complaining
                {
                    if !strstartswith(cp1, b"vertical\x00" as *const u8 as *const libc::c_char)
                        .is_null()
                    {
                        cp3 = cp2;
                        if *cp3 as libc::c_int == ';' as i32 || *cp3 as libc::c_int == ':' as i32 {
                            cp3 = cp3.offset(-1)
                        }
                        while *cp3 as libc::c_int == '\u{0}' as i32
                            || *cp3 as libc::c_int == ' ' as i32
                            || *cp3 as libc::c_int == '\t' as i32
                        {
                            cp3 = cp3.offset(-1)
                        }
                        if *cp3 != 0 {
                            cp3 = cp3.offset(1)
                        }
                        if cp3 == cp1.offset(8) {
                            let mut orientation: libc::c_int =
                                kCTFontOrientationVertical as libc::c_int;
                            let mut orientationNumber: CFNumberRef = CFNumberCreate(
                                0 as CFAllocatorRef,
                                kCFNumberIntType as libc::c_int as CFNumberType,
                                &mut orientation as *mut libc::c_int as *const libc::c_void,
                            );
                            CFDictionaryAddValue(
                                attributes,
                                kCTFontOrientationAttribute as *const libc::c_void,
                                orientationNumber as *const libc::c_void,
                            );
                            CFRelease(orientationNumber as CFTypeRef);
                            CFDictionaryAddValue(
                                stringAttributes,
                                kCTVerticalFormsAttributeName as *const libc::c_void,
                                kCFBooleanTrue as *const libc::c_void,
                            );
                            current_block = 15938117740974259152;
                        } else {
                            current_block = 8464383504555462953;
                        }
                    } else {
                        current_block = 8464383504555462953;
                    }
                    match current_block {
                        15938117740974259152 => {}
                        _ => {
                            font_feature_warning(
                                std::slice::from_raw_parts(
                                    cp1 as *const u8,
                                    cp2.offset_from(cp1) as usize,
                                ),
                                &[],
                            );
                        }
                    }
                }
                _ => {}
            }
            // go to next name=value pair
            cp1 = cp2
        }
        // break if end of string
        if !features.is_null() {
            CFRelease(features as CFTypeRef);
        }
        if CFArrayGetCount(featureSettings as CFArrayRef) != 0 {
            CFDictionaryAddValue(
                attributes,
                kCTFontFeatureSettingsAttribute as *const libc::c_void,
                featureSettings as *const libc::c_void,
            );
        }
        CFRelease(featureSettings as CFTypeRef);
    }
    if loaded_font_flags as libc::c_int & 0x1i32 != 0i32 {
        let mut red: CGFloat = ((rgbValue & 0xff000000u32) >> 24i32) as f64 / 255.0f64;
        let mut green: CGFloat =
            ((rgbValue & 0xff0000i32 as libc::c_uint) >> 16i32) as f64 / 255.0f64;
        let mut blue: CGFloat = ((rgbValue & 0xff00i32 as libc::c_uint) >> 8i32) as f64 / 255.0f64;
        let mut alpha: CGFloat = (rgbValue & 0xffi32 as libc::c_uint) as f64 / 255.0f64;
        // this wrapper CGColor is already at retain count zero
        let mut color = CGColor::rgb(red, green, blue, alpha);
        CFDictionaryAddValue(
            stringAttributes,
            kCTForegroundColorAttributeName as *const libc::c_void,
            color.to_void(),
        );
    }
    matrix = CGAffineTransformIdentity;
    if extend as f64 != 1.0f64 || slant as f64 != 0.0f64 {
        matrix = CGAffineTransform::new(
            extend as CGFloat,
            0i32 as CGFloat,
            slant as CGFloat,
            1.0f64,
            0i32 as CGFloat,
            0i32 as CGFloat,
        )
    }
    if embolden as f64 != 0.0f64 {
        let mut emboldenNumber: CFNumberRef = 0 as CFNumberRef;
        embolden = (embolden as f64 * Fix2D(scaled_size) / 100.0f64) as libc::c_float;
        emboldenNumber = CFNumberCreate(
            0 as CFAllocatorRef,
            kCFNumberFloatType as libc::c_int as CFNumberType,
            &mut embolden as *mut libc::c_float as *const libc::c_void,
        );
        CFDictionaryAddValue(
            stringAttributes,
            getkXeTeXEmboldenAttributeName() as *const libc::c_void,
            emboldenNumber as *const libc::c_void,
        );
        CFRelease(emboldenNumber as CFTypeRef);
    }
    if letterspace as f64 != 0.0f64 {
        loaded_font_letter_space = (letterspace as f64 / 100.0f64 * scaled_size as f64) as scaled_t
    }
    // Disable Core Text font fallback (cascading) with only the last resort font
    // in the cascade list.
    cascadeList =
        CFArrayCreateMutable(0 as CFAllocatorRef, 1i32 as CFIndex, &kCFTypeArrayCallBacks);
    lastResort = CTFontDescriptorCreateWithNameAndSize(getLastResort(), 0i32 as CGFloat);
    CFArrayAppendValue(cascadeList, lastResort as *const libc::c_void);
    CFRelease(lastResort as CFTypeRef);
    CFDictionaryAddValue(
        attributes,
        kCTFontCascadeListAttribute as *const libc::c_void,
        cascadeList as *const libc::c_void,
    );
    CFRelease(cascadeList as CFTypeRef);
    descriptor = CTFontDescriptorCreateWithAttributes(attributes as CFDictionaryRef);
    CFRelease(attributes as CFTypeRef);
    actualFont = CTFontCreateCopyWithAttributes(
        font,
        ctSize,
        &mut matrix as *mut CGAffineTransform as *const CGAffineTransform,
        descriptor,
    );
    CFRelease(font as CFTypeRef);
    CFDictionaryAddValue(
        stringAttributes,
        kCTFontAttributeName as *const libc::c_void,
        actualFont as *const libc::c_void,
    );
    CFRelease(actualFont as CFTypeRef);
    native_font_type_flag = 0xffffu32 as int32_t;
    return stringAttributes as *mut libc::c_void;
}

/* the metrics params here are really TeX 'scaled' (or MacOS 'Fixed') values, but that typedef isn't available every place this is included */
/* these are here, not XeTeX_mac.c, because we need stubs on other platforms */
pub(crate) unsafe fn aat_get_font_metrics(
    mut attributes: CFDictionaryRef,
    mut ascent: *mut i32,
    mut descent: *mut i32,
    mut xheight: *mut i32,
    mut capheight: *mut i32,
    mut slant: *mut i32,
) {
    let mut font: CTFontRef = font_from_attributes(attributes);
    *ascent = D2Fix(CTFontGetAscent(font));
    *descent = D2Fix(CTFontGetDescent(font));
    *xheight = D2Fix(CTFontGetXHeight(font));
    *capheight = D2Fix(CTFontGetCapHeight(font));
    *slant = D2Fix(
        (-CTFontGetSlantAngle(font) * 3.14159265358979323846264338327950288f64 / 180.0f64).tan(),
    );
}

pub(crate) unsafe fn aat_font_get(what: ExtCmd, attributes: CFDictionaryRef) -> i32 {
    let mut rval: libc::c_int = -1i32;
    let mut font: CTFontRef = font_from_attributes(attributes);
    let mut list: CFArrayRef = ptr::null();
    match what {
        ExtCmd::XetexCountGlyphs => rval = CTFontGetGlyphCount(font) as libc::c_int,
        ExtCmd::XetexCountFeatures => {
            list = CTFontCopyFeatures(font);
            if !list.is_null() {
                rval = CFArrayGetCount(list) as libc::c_int;
                CFRelease(list as CFTypeRef);
            }
        }
        _ => {}
    }
    return rval;
}

pub(crate) unsafe fn aat_font_get_1(what: ExtCmd, attributes: CFDictionaryRef, param: i32) -> i32 {
    let mut rval: libc::c_int = -1i32;
    let mut font: CTFontRef = font_from_attributes(attributes);
    match what {
        ExtCmd::XetexFeatureCode => {
            let mut features: CFArrayRef = CTFontCopyFeatures(font);
            if !features.is_null() {
                if CFArrayGetCount(features) > param as CFIndex {
                    let mut feature: CFDictionaryRef =
                        CFArrayGetValueAtIndex(features, param as CFIndex) as CFDictionaryRef;
                    let mut identifier: CFNumberRef = CFDictionaryGetValue(
                        feature,
                        kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                    ) as CFNumberRef;
                    if !identifier.is_null() {
                        CFNumberGetValue(
                            identifier,
                            kCFNumberIntType as libc::c_int as CFNumberType,
                            &mut rval as *mut libc::c_int as *mut libc::c_void,
                        );
                    }
                }
                CFRelease(features as CFTypeRef);
            }
        }
        ExtCmd::XetexIsExclusiveFeature => {
            let mut features_0: CFArrayRef = CTFontCopyFeatures(font);
            if !features_0.is_null() {
                let mut value: CFBooleanRef = ptr::null_mut();
                let mut feature_0: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
                    features_0,
                    kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                    param,
                );
                let mut found: Boolean = CFDictionaryGetValueIfPresent(
                    feature_0,
                    kCTFontFeatureTypeExclusiveKey as *const libc::c_void,
                    &mut value as *mut CFBooleanRef as *mut *const libc::c_void,
                );
                if found != 0 {
                    rval = CFBooleanGetValue(value) as libc::c_int
                }
                CFRelease(features_0 as CFTypeRef);
            }
        }
        ExtCmd::XetexCountSelectors => {
            let mut features_1: CFArrayRef = CTFontCopyFeatures(font);
            if !features_1.is_null() {
                let mut feature_1: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
                    features_1,
                    kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                    param,
                );
                if !feature_1.is_null() {
                    let mut selectors: CFArrayRef = CFDictionaryGetValue(
                        feature_1,
                        kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
                    ) as CFArrayRef;
                    if !selectors.is_null() {
                        rval = CFArrayGetCount(selectors) as libc::c_int
                    }
                }
                CFRelease(features_1 as CFTypeRef);
            }
        }
        _ => {}
    }
    return rval;
}

pub(crate) unsafe fn aat_font_get_2(
    what: ExtCmd,
    attributes: CFDictionaryRef,
    param1: i32,
    param2: i32,
) -> i32 {
    let mut rval: libc::c_int = -1i32;
    let mut font: CTFontRef = font_from_attributes(attributes);
    let mut features: CFArrayRef = CTFontCopyFeatures(font);
    if !features.is_null() {
        let mut feature: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
            features,
            kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
            param1,
        );
        if !feature.is_null() {
            let mut selectors: CFArrayRef = CFDictionaryGetValue(
                feature,
                kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
            ) as CFArrayRef;
            if !selectors.is_null() {
                let mut selector: CFDictionaryRef = ptr::null_mut();
                match what {
                    ExtCmd::XetexSelectorCode => {
                        if CFArrayGetCount(selectors) > param2 as CFIndex {
                            let mut identifier: CFNumberRef = 0 as CFNumberRef;
                            selector = CFArrayGetValueAtIndex(selectors, param2 as CFIndex)
                                as CFDictionaryRef;
                            identifier = CFDictionaryGetValue(
                                selector,
                                kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
                            ) as CFNumberRef;
                            if !identifier.is_null() {
                                CFNumberGetValue(
                                    identifier,
                                    kCFNumberIntType as libc::c_int as CFNumberType,
                                    &mut rval as *mut libc::c_int as *mut libc::c_void,
                                );
                            }
                        }
                    }
                    ExtCmd::XetexIsDefaultSelector => {
                        selector = findDictionaryInArrayWithIdentifier(
                            selectors,
                            kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
                            param2,
                        );
                        if !selector.is_null() {
                            let mut isDefault: CFBooleanRef = 0 as CFBooleanRef;
                            let mut found: Boolean = CFDictionaryGetValueIfPresent(
                                selector,
                                kCTFontFeatureSelectorDefaultKey as *const libc::c_void,
                                &mut isDefault as *mut CFBooleanRef as *mut *const libc::c_void,
                            );
                            if found != 0 {
                                rval = CFBooleanGetValue(isDefault) as libc::c_int
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        CFRelease(features as CFTypeRef);
    }
    return rval;
}

pub(crate) unsafe fn aat_font_get_named(what: ExtCmd, attributes: CFDictionaryRef) -> libc::c_int {
    let mut rval: libc::c_int = -1i32;
    if what == ExtCmd::XetexFindFeatureByName {
        let mut font: CTFontRef = font_from_attributes(attributes);
        let mut features: CFArrayRef = CTFontCopyFeatures(font);
        if !features.is_null() {
            let mut feature: CFDictionaryRef = findDictionaryInArray(
                features,
                kCTFontFeatureTypeNameKey as *const libc::c_void,
                name_of_file,
                name_length,
            );
            if !feature.is_null() {
                let mut identifier: CFNumberRef = CFDictionaryGetValue(
                    feature,
                    kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                ) as CFNumberRef;
                CFNumberGetValue(
                    identifier,
                    kCFNumberIntType as libc::c_int as CFNumberType,
                    &mut rval as *mut libc::c_int as *mut libc::c_void,
                );
            }
            CFRelease(features as CFTypeRef);
        }
    }
    return rval;
}

pub(crate) unsafe fn aat_font_get_named_1(
    what: ExtCmd,
    attributes: CFDictionaryRef,
    param: i32,
) -> i32 {
    let mut rval: libc::c_int = -1i32;
    let mut font: CTFontRef = font_from_attributes(attributes);
    if what == ExtCmd::XetexFindSelectorByName {
        let mut features: CFArrayRef = CTFontCopyFeatures(font);
        if !features.is_null() {
            let mut feature: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
                features,
                kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                param,
            );
            if !feature.is_null() {
                let mut selector: CFNumberRef =
                    findSelectorByName(feature, name_of_file, name_length);
                if !selector.is_null() {
                    CFNumberGetValue(
                        selector,
                        kCFNumberIntType as libc::c_int as CFNumberType,
                        &mut rval as *mut libc::c_int as *mut libc::c_void,
                    );
                }
            }
            CFRelease(features as CFTypeRef);
        }
    }
    return rval;
}

pub(crate) unsafe fn aat_print_font_name(
    mut what: i32,
    mut attributes: CFDictionaryRef,
    mut param1: i32,
    mut param2: i32,
) {
    let mut name: CFStringRef = 0 as CFStringRef;
    if what == 8i32 || what == 9i32 {
        let mut font: CTFontRef = font_from_attributes(attributes);
        let mut features: CFArrayRef = CTFontCopyFeatures(font);
        if !features.is_null() {
            let mut feature: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
                features,
                kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                param1,
            );
            if !feature.is_null() {
                if what == 8i32 {
                    name = CFDictionaryGetValue(
                        feature,
                        kCTFontFeatureTypeNameKey as *const libc::c_void,
                    ) as CFStringRef
                } else {
                    let mut selectors: CFArrayRef = CFDictionaryGetValue(
                        feature,
                        kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
                    ) as CFArrayRef;
                    let mut selector: CFDictionaryRef = findDictionaryInArrayWithIdentifier(
                        selectors,
                        kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
                        param2,
                    );
                    if !selector.is_null() {
                        name = CFDictionaryGetValue(
                            selector,
                            kCTFontFeatureSelectorNameKey as *const libc::c_void,
                        ) as CFStringRef
                    }
                }
            }
            CFRelease(features as CFTypeRef);
        }
    }
    if !name.is_null() {
        let mut len: CFIndex = CFStringGetLength(name);
        let mut buf: *mut UniChar =
            xcalloc(len as _, ::std::mem::size_of::<UniChar>() as _) as *mut UniChar;
        CFStringGetCharacters(name, CFRangeMake(0i32 as CFIndex, len), buf);
        print_chars(buf, len as libc::c_int);
        free(buf as *mut libc::c_void);
    };
}
