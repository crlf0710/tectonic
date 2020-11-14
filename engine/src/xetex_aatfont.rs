#![cfg(target_os = "macos")]
#![allow(
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
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

use crate::cmd::ExtCmd;
use crate::core_memory::{xcalloc, xmalloc};
use crate::node::NativeWord;
use crate::xetex_ext::{print_chars, readCommonFeatures, read_double, D2Fix, Fix2D};
use crate::xetex_ini::{
    loaded_font_flags, loaded_font_letter_space, name_of_file, FONT_LAYOUT_ENGINE,
    FONT_LETTER_SPACE,
};
use crate::xetex_xetex0::font_feature_warning;
use libc::{free, strlen};
pub(crate) type Boolean = libc::c_uchar;

use crate::xetex_scaledmath::Scaled;
type Fract = i32;
#[derive(Copy, Clone)]
#[repr(C, packed(2))]
struct FixedPoint {
    x: Scaled,
    y: Scaled,
}

pub(crate) type str_number = i32;
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
fn TeXtoPSPoints(pts: f64) -> f64 {
    pts * 72. / 72.27
}
#[inline]
fn PStoTeXPoints(pts: f64) -> f64 {
    pts * 72.27 / 72.
}
#[inline]
unsafe fn FixedPStoTeXPoints(pts: f64) -> Scaled {
    D2Fix(PStoTeXPoints(pts))
}

pub(crate) unsafe fn font_from_attributes(attributes: CFDictionaryRef) -> CTFontRef {
    return CFDictionaryGetValue(attributes, kCTFontAttributeName as *const libc::c_void)
        as CTFontRef;
}

pub(crate) unsafe fn do_aat_layout(node: &mut NativeWord, justify: bool) {
    let mut totalGlyphCount;
    let mut glyphAdvances: *mut Scaled = ptr::null_mut();
    let mut glyph_info: *mut libc::c_void = ptr::null_mut();
    let mut locations: *mut FixedPoint = ptr::null_mut();
    let mut width: CGFloat = 0.;
    let mut typesetter;
    let mut line;
    let f = node.font() as libc::c_uint;
    if let Font::Native(Aat(attributes)) = &FONT_LAYOUT_ENGINE[f as usize] {
        let txt = node.text();
        let txtLen = txt.len() as CFIndex;
        let txtPtr = txt.as_ptr() as *const UniChar;
        let string =
            CFStringCreateWithCharactersNoCopy(ptr::null(), txtPtr, txtLen, kCFAllocatorNull);
        let attrString = CFAttributedStringCreate(ptr::null(), string, *attributes);
        CFRelease(string as CFTypeRef);
        typesetter = CTTypesetterCreateWithAttributedString(attrString);
        CFRelease(attrString as CFTypeRef);
        line = CTTypesetterCreateLine(typesetter, CFRangeMake(0i32 as CFIndex, txtLen));
        if justify {
            let lineWidth = TeXtoPSPoints(Fix2D(node.width()));
            let justifiedLine = CTLineCreateJustifiedLine(
                line,
                TeXtoPSPoints(Fix2D(Scaled(0x40000000))),
                lineWidth,
            );
            // TODO(jjgod): how to handle the case when justification failed? for
            // now we just fallback to use the original line.
            if !justifiedLine.is_null() {
                CFRelease(line as CFTypeRef);
                line = justifiedLine
            }
        }
        let glyphRuns = CTLineGetGlyphRuns(line);
        let runCount = CFArrayGetCount(glyphRuns);
        totalGlyphCount = CTLineGetGlyphCount(line);
        if totalGlyphCount > 0 {
            glyph_info = xmalloc((totalGlyphCount * 10) as _);
            locations = glyph_info as *mut FixedPoint;
            let glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
            glyphAdvances = xmalloc(
                (totalGlyphCount as usize).wrapping_mul(::std::mem::size_of::<Scaled>()) as _,
            ) as *mut Scaled;
            totalGlyphCount = 0i32 as CFIndex;
            width = 0i32 as CGFloat;
            for i in 0..runCount {
                let run = CFArrayGetValueAtIndex(glyphRuns, i) as CTRunRef;
                let count = CTRunGetGlyphCount(run);
                let runAttributes = CTRunGetAttributes(run);
                let vertical = CFDictionaryGetValue(
                    runAttributes,
                    kCTVerticalFormsAttributeName as *const libc::c_void,
                ) as CFBooleanRef;
                // TODO(jjgod): Avoid unnecessary allocation with CTRunGetFoosPtr().
                let glyphs =
                    xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGGlyph>()) as _)
                        as *mut CGGlyph;
                let positions =
                    xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGPoint>()) as _)
                        as *mut CGPoint;
                let advances =
                    xmalloc((count as usize).wrapping_mul(::std::mem::size_of::<CGSize>()) as _)
                        as *mut CGSize;
                let runWidth = CTRunGetTypographicBounds(
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
                for j in 0..count {
                    // XXX Core Text has that font cascading thing that will do
                    // font substitution for missing glyphs, which we do not want
                    // but I can not find a way to disable it yet, so if the font
                    // of the resulting run is not the same font we asked for, use
                    // the glyph at index 0 (usually .notdef) instead or we will be
                    // showing garbage or even invalid glyphs
                    if CFEqual(
                        font_from_attributes(*attributes) as CFTypeRef,
                        font_from_attributes(runAttributes) as CFTypeRef,
                    ) == 0
                    {
                        *glyphIDs.offset(totalGlyphCount as isize) = 0i32 as u16
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
                        Scaled((*advances.offset(j as isize)).width as i32);
                    totalGlyphCount += 1;
                }
                width += FixedPStoTeXPoints(runWidth).0 as f64;
                free(glyphs as *mut libc::c_void);
                free(positions as *mut libc::c_void);
                free(advances as *mut libc::c_void);
            }
        }
    } else {
        panic!("do_aat_layout called for non-AAT font");
    }
    node.set_glyph_count(totalGlyphCount as u16);
    node.set_glyph_info_ptr(glyph_info);
    if !justify {
        node.set_width(Scaled(width as i32));
        if totalGlyphCount > 0 {
            /* this is essentially a copy from similar code in XeTeX_ext.c, easier
             * to be done here */
            if FONT_LETTER_SPACE[f as usize] != Scaled::ZERO {
                let mut lsDelta = Scaled::ZERO;
                let lsUnit = FONT_LETTER_SPACE[f as usize];
                let mut i_0 = 0;
                while i_0 < totalGlyphCount {
                    if *glyphAdvances.offset(i_0 as isize) == Scaled::ZERO
                        && lsDelta != Scaled::ZERO
                    {
                        lsDelta -= lsUnit
                    }
                    let ref mut fresh1 = (*locations.offset(i_0 as isize)).x;
                    *fresh1 += lsDelta;
                    lsDelta += lsUnit;
                    i_0 += 1
                }
                if lsDelta != Scaled::ZERO {
                    lsDelta -= lsUnit;
                    let w = node.width();
                    node.set_width(w + lsDelta);
                }
            }
        }
    }
    free(glyphAdvances as *mut libc::c_void);
    CFRelease(line as CFTypeRef);
    CFRelease(typesetter as CFTypeRef);
}

unsafe fn getGlyphBBoxFromCTFont(font: CTFontRef, mut gid: u16, mut bbox: *mut GlyphBBox) {
    (*bbox).xMin = 65536.;
    (*bbox).yMin = 65536.;
    (*bbox).xMax = -65536.;
    (*bbox).yMax = -65536.;
    let rect = CTFontGetBoundingRectsForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut u16 as *const CGGlyph,
        ptr::null_mut(),
        1i32 as CFIndex,
    );
    if CGRectIsNull(rect) {
        (*bbox).yMax = 0.;
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
pub(crate) unsafe fn GetGlyphBBox_AAT(attributes: CFDictionaryRef, gid: u16, bbox: *mut GlyphBBox) {
    let font = font_from_attributes(attributes);
    getGlyphBBoxFromCTFont(font, gid, bbox)
}

unsafe fn getGlyphWidthFromCTFont(font: CTFontRef, mut gid: u16) -> f64 {
    PStoTeXPoints(CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationHorizontal,
        &mut gid as *mut u16 as *const CGGlyph,
        ptr::null_mut(),
        1i32 as CFIndex,
    ))
}

/// returns TeX points
pub(crate) unsafe fn GetGlyphWidth_AAT(attributes: CFDictionaryRef, gid: u16) -> f64 {
    let font = font_from_attributes(attributes);
    getGlyphWidthFromCTFont(font, gid)
}

// returns TeX points
pub(crate) unsafe fn GetGlyphHeightDepth_AAT(
    attributes: CFDictionaryRef,
    gid: u16,
    ht: *mut libc::c_float,
    dp: *mut libc::c_float,
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
    attributes: CFDictionaryRef,
    mut gid: u16,
    lsb: *mut libc::c_float,
    rsb: *mut libc::c_float,
) {
    let font = font_from_attributes(attributes);
    let mut advances: [CGSize; 1] = [CGSizeMake(0i32 as CGFloat, 0i32 as CGFloat)];
    let advance = CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut u16 as *const CGGlyph,
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
unsafe fn CGSizeMake(width: CGFloat, height: CGFloat) -> CGSize {
    CGSize { width, height }
}

pub(crate) unsafe fn GetGlyphItalCorr_AAT(attributes: CFDictionaryRef, mut gid: u16) -> f64 {
    let font = font_from_attributes(attributes);
    let mut advances: [CGSize; 1] = [CGSizeMake(0i32 as CGFloat, 0i32 as CGFloat)];
    let advance = CTFontGetAdvancesForGlyphs(
        font,
        kCTFontOrientationDefault,
        &mut gid as *mut u16 as *const CGGlyph,
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
unsafe fn mapCharToGlyphFromCTFont(font: CTFontRef, mut ch: u32) -> libc::c_int {
    let mut glyphs: [CGGlyph; 2] = [0i32 as CGGlyph, 0];
    let mut txt: [UniChar; 2] = [0; 2];
    let mut len: libc::c_int = 1i32;
    if ch > 0xffffi32 as libc::c_uint {
        ch = (ch as libc::c_uint).wrapping_sub(0x10000i32 as libc::c_uint) as u32;
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

pub(crate) unsafe fn MapCharToGlyph_AAT(attributes: CFDictionaryRef, ch: u32) -> libc::c_int {
    let font = font_from_attributes(attributes);
    mapCharToGlyphFromCTFont(font, ch)
}

unsafe fn GetGlyphIDFromCTFont(
    ctFontRef: CTFontRef,
    glyphName: *const libc::c_char,
) -> libc::c_int {
    let glyphname = CFStringCreateWithCStringNoCopy(
        kCFAllocatorDefault,
        glyphName,
        kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        kCFAllocatorNull,
    );
    let rval = CTFontGetGlyphWithName(ctFontRef, glyphname) as libc::c_int;
    CFRelease(glyphname as CFTypeRef);
    rval
}

/* single-purpose metrics accessors */
/* the metrics params here are really TeX 'scaled' (or MacOS 'Scaled') values, but that typedef isn't available every place this is included */
/* functions in XeTeX_mac.c */
pub(crate) unsafe fn MapGlyphToIndex_AAT(
    attributes: CFDictionaryRef,
    glyphName: *const libc::c_char,
) -> libc::c_int {
    let font = font_from_attributes(attributes);
    GetGlyphIDFromCTFont(font, glyphName)
}

pub(crate) unsafe fn GetGlyphNameFromCTFont(
    ctFontRef: CTFontRef,
    gid: u16,
    len: *mut libc::c_int,
) -> *mut libc::c_char {
    static mut buffer: [libc::c_char; 256] = [0; 256];
    buffer[0] = 0i32 as libc::c_char;
    *len = 0i32;
    let cgfont = CTFontCopyGraphicsFont(ctFontRef, ptr::null_mut());
    if !cgfont.is_null() && (gid as usize) < CGFontGetNumberOfGlyphs(cgfont) {
        let glyphname = CGFontCopyGlyphNameForGlyph(cgfont, gid);
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
    attributes: CFDictionaryRef,
    reqFirst: libc::c_int,
) -> libc::c_int {
    if reqFirst != 0 {
        let mut ch: libc::c_int = 0i32;
        while MapCharToGlyph_AAT(attributes, ch as u32) == 0i32 && ch < 0x10ffffi32 {
            ch += 1
        }
        return ch;
    } else {
        let mut ch_0: libc::c_int = 0x10ffffi32;
        while MapCharToGlyph_AAT(attributes, ch_0 as u32) == 0i32 && ch_0 > 0i32 {
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
pub(crate) unsafe fn getFileNameFromCTFont(ctFontRef: CTFontRef, index: *mut u32) -> String {
    let mut ix: i32 = -1;
    let mut ret = String::new();
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
                /*let osstr = pathbuf.as_os_str();
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
                }*/
                ret = pathbuf.as_os_str().to_string_lossy().into();
            }
        }
    }
    return ret;
}

pub(crate) unsafe fn findDictionaryInArrayWithIdentifier(
    array: CFArrayRef,
    identifierKey: *const libc::c_void,
    identifier: libc::c_int,
) -> CFDictionaryRef {
    let mut dict: CFDictionaryRef = 0 as CFDictionaryRef;
    if !array.is_null() {
        let mut value: libc::c_int = -1i32;
        for i in 0..CFArrayGetCount(array) {
            let item = CFArrayGetValueAtIndex(array, i) as CFDictionaryRef;
            let itemId = CFDictionaryGetValue(item, identifierKey) as CFNumberRef;
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
        }
    }
    return dict;
}

#[inline(always)]
unsafe fn CFRangeMake(loc: CFIndex, len: CFIndex) -> CFRange {
    let mut range: CFRange = CFRange {
        location: 0,
        length: 0,
    };
    range.location = loc;
    range.length = len;
    return range;
}

pub(crate) unsafe fn findDictionaryInArray(
    array: CFArrayRef,
    nameKey: *const libc::c_void,
    name: &[u8],
) -> CFDictionaryRef {
    let mut dict: CFDictionaryRef = 0 as CFDictionaryRef;
    if !array.is_null() {
        let itemName = CFStringCreateWithBytes(
            0 as CFAllocatorRef,
            name.as_ptr(),
            name.len() as CFIndex,
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
            0i32 as Boolean,
        );
        let mut i = 0i32 as CFIndex;
        while i < CFArrayGetCount(array) {
            let item = CFArrayGetValueAtIndex(array, i) as CFDictionaryRef;
            let iName = CFDictionaryGetValue(item, nameKey) as CFStringRef;
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

pub(crate) unsafe fn findSelectorByName(feature: CFDictionaryRef, name: &[u8]) -> CFNumberRef {
    let mut selector: CFNumberRef = 0 as CFNumberRef;
    let selectors = CFDictionaryGetValue(
        feature,
        kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
    ) as CFArrayRef;
    if !selectors.is_null() {
        let s = findDictionaryInArray(
            selectors,
            kCTFontFeatureSelectorNameKey as *const libc::c_void,
            name,
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
    featureTypeIdentifier: CFNumberRef,
    featureSelectorIdentifier: CFNumberRef,
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

use crate::xetex_ext::{Font, NativeFont, NativeFont::*};

pub(crate) unsafe fn loadAATfont(
    mut descriptor: CTFontDescriptorRef,
    scaled_size: Scaled,
    mut cp1: &[u8],
) -> Option<NativeFont> {
    let mut current_block: u64;
    let mut extend = 1_f32;
    let mut slant = 0_f32;
    let mut embolden = 0_f32;
    let mut letterspace = 0_f32;
    let mut rgbValue: u32 = 0;
    // create a base font instance for applying further attributes
    let ctSize = TeXtoPSPoints(Fix2D(scaled_size));
    let font = CTFontCreateWithFontDescriptor(descriptor, ctSize, ptr::null());
    if font.is_null() {
        return None;
    }
    let stringAttributes = CFDictionaryCreateMutable(
        0 as CFAllocatorRef,
        0i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    let attributes = CFDictionaryCreateMutable(
        0 as CFAllocatorRef,
        0i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    if !cp1.is_empty() {
        let features = CTFontCopyFeatures(font);
        let featureSettings =
            CFArrayCreateMutable(0 as CFAllocatorRef, 0i32 as CFIndex, &kCFTypeArrayCallBacks);
        // interpret features following ":"
        while !cp1.is_empty() {
            // locate beginning of name=value pair
            if !cp1.is_empty() && b":;".contains(&cp1[0]) {
                // skip over separator
                cp1 = &cp1[1..];
            }
            while !cp1.is_empty() && b" \t".contains(&cp1[0]) {
                // skip leading whitespace
                cp1 = &cp1[1..];
            }
            if cp1.is_empty() {
                break;
            }
            // scan to end of pair
            let mut cp2 = cp1;
            while !cp2.is_empty() && !b";:".contains(&cp2[0]) {
                cp2 = &cp2[1..];
            }
            // look for the '=' separator
            let mut cp3 = cp1;
            while cp3.len() > cp2.len() && cp3[0] != b'=' {
                cp3 = &cp3[1..];
            }
            if cp3.len() == cp2.len() {
                current_block = 4154772336439402900;
            } else {
                // now cp1 points to option name, cp3 to '=', cp2 to ';' or null
                // first try for a feature by this name
                let feature = findDictionaryInArray(
                    features,
                    kCTFontFeatureTypeNameKey as *const libc::c_void,
                    &cp1[..cp1.len() - cp3.len()],
                );
                if !feature.is_null() {
                    // look past the '=' separator for setting names
                    let featLen = cp1.len() - cp3.len();
                    let mut zeroInteger: libc::c_int = 0i32;
                    let zero = CFNumberCreate(
                        0 as CFAllocatorRef,
                        kCFNumberIntType as libc::c_int as CFNumberType,
                        &mut zeroInteger as *mut libc::c_int as *const libc::c_void,
                    );
                    cp3 = &cp3[1..];
                    while cp3.len() > cp2.len() {
                        //let mut disable: libc::c_int = 0i32;
                        // skip leading whitespace
                        while b" \t".contains(&cp3[0]) {
                            cp3 = &cp3[1..];
                        }
                        // possibly multiple settings...
                        if !cp3.is_empty() && cp3[0] == b'!' {
                            // check for negation
                            //disable = 1i32;
                            cp3 = &cp3[1..];
                        }
                        // scan for end of setting name
                        let mut cp4 = cp3;
                        while cp4.len() > cp2.len() && cp4[0] != b',' {
                            cp4 = &cp4[1..];
                        }
                        // now cp3 points to name, cp4 to ',' or ';' or null
                        let selector = findSelectorByName(feature, &cp3[..cp3.len() - cp4.len()]);
                        if !selector.is_null()
                            && comparison_was(
                                CFNumberCompare(selector, zero, ptr::null_mut()),
                                CFComparisonResult::GreaterThan,
                            )
                        {
                            let featureType = CFDictionaryGetValue(
                                feature,
                                kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                            ) as CFNumberRef;
                            let featureSetting =
                                createFeatureSettingDictionary(featureType, selector);
                            CFArrayAppendValue(
                                featureSettings,
                                featureSetting as *const libc::c_void,
                            );
                            CFRelease(featureSetting as CFTypeRef);
                        } else {
                            font_feature_warning(&cp1[..featLen], &cp3[..cp3.len() - cp4.len()]);
                        }
                        // point beyond setting name terminator
                        cp3 = &cp4[1..];
                    }
                    CFRelease(zero as CFTypeRef);
                    current_block = 15938117740974259152;
                } else {
                    // didn't find feature, try other options...
                    let ret = readCommonFeatures(
                        cp1,
                        cp1.len() - cp2.len(),
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
                        if let Some(mut cp3) = strstartswith(cp1, b"tracking") {
                            if cp3[0] != b'=' {
                                current_block = 4154772336439402900;
                            } else {
                                cp3 = &cp3[1..];
                                let mut tracking = read_double(&mut cp3);
                                let trackingNumber = CFNumberCreate(
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
                    if cp1.starts_with(b"vertical") {
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
                            let mut orientation: libc::c_int =
                                kCTFontOrientationVertical as libc::c_int;
                            let orientationNumber = CFNumberCreate(
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
                            font_feature_warning(&cp1[..cp1.len() - cp2.len()], &[]);
                        }
                    }
                }
                _ => {}
            }
            // go to next name=value pair
            cp1 = cp2;
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
        let red: CGFloat = ((rgbValue & 0xff000000u32) >> 24i32) as f64 / 255.0f64;
        let green: CGFloat = ((rgbValue & 0xff0000i32 as libc::c_uint) >> 16i32) as f64 / 255.0f64;
        let blue: CGFloat = ((rgbValue & 0xff00i32 as libc::c_uint) >> 8i32) as f64 / 255.0f64;
        let alpha: CGFloat = (rgbValue & 0xffi32 as libc::c_uint) as f64 / 255.0f64;
        // this wrapper CGColor is already at retain count zero
        let color = CGColor::rgb(red, green, blue, alpha);
        CFDictionaryAddValue(
            stringAttributes,
            kCTForegroundColorAttributeName as *const libc::c_void,
            color.to_void(),
        );
    }
    let mut matrix = CGAffineTransformIdentity;
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
        embolden = (embolden as f64 * Fix2D(scaled_size) / 100.0f64) as libc::c_float;
        let emboldenNumber = CFNumberCreate(
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
        loaded_font_letter_space =
            Scaled((letterspace as f64 / 100.0f64 * scaled_size.0 as f64) as i32)
    }
    // Disable Core Text font fallback (cascading) with only the last resort font
    // in the cascade list.
    let cascadeList =
        CFArrayCreateMutable(0 as CFAllocatorRef, 1i32 as CFIndex, &kCFTypeArrayCallBacks);
    let lastResort = CTFontDescriptorCreateWithNameAndSize(getLastResort(), 0i32 as CGFloat);
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
    let actualFont = CTFontCreateCopyWithAttributes(
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
    if stringAttributes.is_null() {
        None
    } else {
        Some(Aat(stringAttributes))
    }
}

/* the metrics params here are really TeX 'scaled' (or MacOS 'Scaled') values, but that typedef isn't available every place this is included */
/* these are here, not XeTeX_mac.c, because we need stubs on other platforms */
pub(crate) unsafe fn aat_get_font_metrics(
    attributes: CFDictionaryRef,
) -> (Scaled, Scaled, Scaled, Scaled, Scaled) {
    let font = font_from_attributes(attributes);
    let ascent = D2Fix(CTFontGetAscent(font));
    let descent = D2Fix(CTFontGetDescent(font));
    let xheight = D2Fix(CTFontGetXHeight(font));
    let capheight = D2Fix(CTFontGetCapHeight(font));
    let slant = D2Fix(
        (-CTFontGetSlantAngle(font) * 3.14159265358979323846264338327950288f64 / 180.0f64).tan(),
    );
    (ascent, descent, xheight, capheight, slant)
}

pub(crate) unsafe fn aat_font_get(what: ExtCmd, attributes: CFDictionaryRef) -> i32 {
    let mut rval: libc::c_int = -1i32;
    let font = font_from_attributes(attributes);
    match what {
        ExtCmd::XetexCountGlyphs => rval = CTFontGetGlyphCount(font) as libc::c_int,
        ExtCmd::XetexCountFeatures => {
            let list = CTFontCopyFeatures(font);
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
    let font = font_from_attributes(attributes);
    match what {
        ExtCmd::XetexFeatureCode => {
            let features = CTFontCopyFeatures(font);
            if !features.is_null() {
                if CFArrayGetCount(features) > param as CFIndex {
                    let feature =
                        CFArrayGetValueAtIndex(features, param as CFIndex) as CFDictionaryRef;
                    let identifier = CFDictionaryGetValue(
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
            let features_0 = CTFontCopyFeatures(font);
            if !features_0.is_null() {
                let mut value: CFBooleanRef = ptr::null_mut();
                let feature_0 = findDictionaryInArrayWithIdentifier(
                    features_0,
                    kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                    param,
                );
                let found = CFDictionaryGetValueIfPresent(
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
            let features_1 = CTFontCopyFeatures(font);
            if !features_1.is_null() {
                let feature_1 = findDictionaryInArrayWithIdentifier(
                    features_1,
                    kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                    param,
                );
                if !feature_1.is_null() {
                    let selectors = CFDictionaryGetValue(
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
    let font = font_from_attributes(attributes);
    let features = CTFontCopyFeatures(font);
    if !features.is_null() {
        let feature = findDictionaryInArrayWithIdentifier(
            features,
            kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
            param1,
        );
        if !feature.is_null() {
            let selectors = CFDictionaryGetValue(
                feature,
                kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
            ) as CFArrayRef;
            if !selectors.is_null() {
                match what {
                    ExtCmd::XetexSelectorCode => {
                        if CFArrayGetCount(selectors) > param2 as CFIndex {
                            let selector = CFArrayGetValueAtIndex(selectors, param2 as CFIndex)
                                as CFDictionaryRef;
                            let identifier = CFDictionaryGetValue(
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
                        let selector = findDictionaryInArrayWithIdentifier(
                            selectors,
                            kCTFontFeatureSelectorIdentifierKey as *const libc::c_void,
                            param2,
                        );
                        if !selector.is_null() {
                            let mut isDefault: CFBooleanRef = 0 as CFBooleanRef;
                            let found = CFDictionaryGetValueIfPresent(
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
        let font = font_from_attributes(attributes);
        let features = CTFontCopyFeatures(font);
        if !features.is_null() {
            let feature = findDictionaryInArray(
                features,
                kCTFontFeatureTypeNameKey as *const libc::c_void,
                name_of_file.as_bytes(),
            );
            if !feature.is_null() {
                let identifier = CFDictionaryGetValue(
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
    let font = font_from_attributes(attributes);
    if what == ExtCmd::XetexFindSelectorByName {
        let features = CTFontCopyFeatures(font);
        if !features.is_null() {
            let feature = findDictionaryInArrayWithIdentifier(
                features,
                kCTFontFeatureTypeIdentifierKey as *const libc::c_void,
                param,
            );
            if !feature.is_null() {
                let selector = findSelectorByName(feature, name_of_file.as_bytes());
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
    what: i32,
    attributes: CFDictionaryRef,
    param1: i32,
    param2: i32,
) {
    let mut name: CFStringRef = 0 as CFStringRef;
    if what == 8i32 || what == 9i32 {
        let font = font_from_attributes(attributes);
        let features = CTFontCopyFeatures(font);
        if !features.is_null() {
            let feature = findDictionaryInArrayWithIdentifier(
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
                    let selectors = CFDictionaryGetValue(
                        feature,
                        kCTFontFeatureTypeSelectorsKey as *const libc::c_void,
                    ) as CFArrayRef;
                    let selector = findDictionaryInArrayWithIdentifier(
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
        let len = CFStringGetLength(name);
        let buf = xcalloc(len as _, ::std::mem::size_of::<UniChar>() as _) as *mut UniChar;
        CFStringGetCharacters(name, CFRangeMake(0i32 as CFIndex, len), buf);
        print_chars(buf, len as libc::c_int);
        free(buf as *mut libc::c_void);
    };
}
