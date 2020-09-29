#![feature(extern_types)]
#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_unsafe,
    unused_attributes
)]
#[macro_use]
extern crate bridge;

use std::ptr;

// For the msg_send macro
#[cfg(target_os = "macos")]
#[macro_use]
extern crate objc;

//use log::{info, warn};

pub(crate) type __off_t = i64;
pub(crate) type __off64_t = i64;
pub(crate) type size_t = usize;
pub(crate) type off_t = __off_t;
pub(crate) type ssize_t = isize;

use bibtex::bibtex_main;
use bridge::TTHistory;
use dpx::dvipdfmx_main;
use xetex_ini::tt_run_engine;

pub use bridge::tt_bridge_api_t;
pub use bridge::tt_get_error_message;
pub use xetex_engine_interface::tt_xetex_set_int_variable;

pub unsafe fn tex_simple_main(
    mut api: *const tt_bridge_api_t,
    mut dump_name: *const i8,
    mut input_file_name: *const i8,
) -> i32 {
    bridge::tt_with_bridge(api, || tt_run_engine(dump_name, input_file_name) as i32)
        .unwrap_or(TTHistory::FATAL_ERROR as i32)
}

pub unsafe fn dvipdfmx_simple_main(
    mut api: *const tt_bridge_api_t,
    dviname: &str,
    mut pdfname: *const i8,
    mut compress: bool,
    mut deterministic_tags: bool,
) -> i32 {
    bridge::tt_with_bridge(api, || {
        dvipdfmx_main(
            pdfname,
            dviname,
            ptr::null(),
            0i32,
            false,
            compress,
            deterministic_tags,
            false,
            0_u32,
        ) as i32
    })
    .unwrap_or(99)
}

pub unsafe fn bibtex_simple_main(
    mut api: *const tt_bridge_api_t,
    mut aux_file_name: *const i8,
) -> i32 {
    bridge::tt_with_bridge(api, || bibtex_main(aux_file_name) as i32).unwrap_or(99)
}

mod core_memory {
    use bridge::size_t;
    /* tectonic/core-memory.c: basic C dynamic memory helpers

    Copyright 1993, 1994, 1995, 2008, 2009, 2010, 2011 Karl Berry.
    Copyright 1997, 2002, 2005 Olaf Weber.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this library; if not, see <http://www.gnu.org/licenses/>.  */
    #[no_mangle]
    pub(crate) unsafe fn xcalloc(mut nelem: size_t, mut elsize: size_t) -> *mut libc::c_void {
        let nelem = nelem as libc::size_t; //FIXME
        let elsize = elsize as libc::size_t; //FIXME
        let mut new_mem: *mut libc::c_void = libc::calloc(
            if nelem != 0 { nelem } else { 1 },
            if elsize != 0 { elsize } else { 1 },
        );
        if new_mem.is_null() {
            panic!(
                "xcalloc request for {} elements of size {} failed",
                nelem, elsize,
            );
        }
        new_mem
    }
    #[no_mangle]
    pub(crate) unsafe fn xmalloc(mut size: size_t) -> *mut libc::c_void {
        let size = size as libc::size_t; //FIXME

        let mut new_mem: *mut libc::c_void = libc::malloc(if size != 0 { size } else { 1 });
        if new_mem.is_null() {
            panic!("xmalloc request for {} bytes failed", size,);
        }
        new_mem
    }
    #[no_mangle]
    pub(crate) unsafe fn xrealloc(
        mut old_ptr: *mut libc::c_void,
        mut size: size_t,
    ) -> *mut libc::c_void {
        let size = size as libc::size_t; //FIXME
        let mut new_mem: *mut libc::c_void = 0 as *mut libc::c_void;
        if old_ptr.is_null() {
            new_mem = xmalloc(size as size_t)
        } else {
            new_mem = libc::realloc(old_ptr, if size != 0 { size } else { 1 });
            if new_mem.is_null() {
                panic!("xrealloc() to {} bytes failed", size,);
            }
        }
        new_mem
    }
    pub(crate) unsafe fn strdup(mut s: &str) -> *mut i8 {
        let mut new_string: *mut i8 = xmalloc((s.len() + 1) as size_t) as *mut i8;
        let s = std::ffi::CString::new(s).unwrap();
        libc::strcpy(new_string, s.as_ptr())
    }

    #[inline]
    pub(crate) unsafe fn mfree(ptr: *mut libc::c_void) -> *mut libc::c_void {
        libc::free(ptr);
        std::ptr::null_mut()
    }

    #[inline]
    pub(crate) unsafe fn xmalloc_array<T>(size: usize) -> *mut T {
        xmalloc(((size + 1) * std::mem::size_of::<T>()) as _) as *mut T
    }
    #[inline]
    pub(crate) unsafe fn xcalloc_array<T>(size: usize) -> *mut T {
        xcalloc((size + 1) as _, std::mem::size_of::<T>() as _) as *mut T
    }
}

mod cmd;
mod node;
mod xetex_aatfont;
mod xetex_consts;
mod xetex_engine_interface;
mod xetex_errors;
mod xetex_ext;
mod xetex_ini;
mod xetex_io;
mod xetex_linebreak;
mod xetex_math;
mod xetex_output;
mod xetex_pagebuilder;
mod xetex_pic;
mod xetex_scaledmath;
mod xetex_shipout;
mod xetex_stringpool;
mod xetex_synctex;
mod xetex_texmfmp;
mod xetex_xetex0;
mod xetex_xetexd;

mod stub_icu;
mod stub_stdio;
mod stub_teckit;

mod fmt_file;
mod tfm;
mod trie;

#[inline]
pub(crate) fn strstartswith<'a>(s: &'a [u8], prefix: &[u8]) -> Option<&'a [u8]> {
    if s.starts_with(prefix) {
        return Some(&s[prefix.len()..]);
    }
    None
}

mod xetex_font_info;
mod xetex_font_manager;
mod xetex_layout_interface;

pub(crate) mod freetype_sys_patch {
    use freetype::freetype_sys::{
        FT_Byte, FT_Error, FT_Face, FT_Fixed, FT_Int32, FT_Long, FT_Sfnt_Tag, FT_Short, FT_UInt,
        FT_ULong, FT_UShort,
    };

    extern "C" {
        #[no_mangle]
        pub(crate) fn FT_Face_GetCharVariantIndex(
            face: FT_Face,
            charcode: FT_ULong,
            variantSelector: FT_ULong,
        ) -> FT_UInt;

        #[no_mangle]
        pub(crate) fn FT_Get_Advance(
            face: FT_Face,
            gindex: FT_UInt,
            load_flags: FT_Int32,
            padvance: *mut FT_Fixed,
        ) -> FT_Error;

        #[no_mangle]
        pub(crate) fn FT_Load_Sfnt_Table(
            face: FT_Face,
            tag: FT_ULong,
            offset: FT_Long,
            buffer: *mut FT_Byte,
            length: *mut FT_ULong,
        ) -> FT_Error;

        #[no_mangle]
        pub(crate) fn FT_Get_Sfnt_Name_Count(face: FT_Face) -> FT_UInt;

        #[no_mangle]
        pub(crate) fn FT_Get_Sfnt_Name(
            face: FT_Face,
            idx: FT_UInt,
            aname: *mut FT_SfntName,
        ) -> FT_Error;
    }

    pub(crate) const FT_SFNT_MAX: FT_Sfnt_Tag = 7;
    pub(crate) const FT_SFNT_PCLT: FT_Sfnt_Tag = 6;
    pub(crate) const FT_SFNT_POST: FT_Sfnt_Tag = 5;
    pub(crate) const FT_SFNT_VHEA: FT_Sfnt_Tag = 4;
    pub(crate) const FT_SFNT_HHEA: FT_Sfnt_Tag = 3;
    pub(crate) const FT_SFNT_OS2: FT_Sfnt_Tag = 2;
    pub(crate) const FT_SFNT_MAXP: FT_Sfnt_Tag = 1;
    pub(crate) const FT_SFNT_HEAD: FT_Sfnt_Tag = 0;

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub(crate) struct TT_Header_ {
        pub(crate) Table_Version: FT_Fixed,
        pub(crate) Font_Revision: FT_Fixed,
        pub(crate) CheckSum_Adjust: FT_Long,
        pub(crate) Magic_Number: FT_Long,
        pub(crate) Flags: FT_UShort,
        pub(crate) Units_Per_EM: FT_UShort,
        pub(crate) Created: [FT_ULong; 2],
        pub(crate) Modified: [FT_ULong; 2],
        pub(crate) xMin: FT_Short,
        pub(crate) yMin: FT_Short,
        pub(crate) xMax: FT_Short,
        pub(crate) yMax: FT_Short,
        pub(crate) Mac_Style: FT_UShort,
        pub(crate) Lowest_Rec_PPEM: FT_UShort,
        pub(crate) Font_Direction: FT_Short,
        pub(crate) Index_To_Loc_Format: FT_Short,
        pub(crate) Glyph_Data_Format: FT_Short,
    }
    pub(crate) type TT_Header = TT_Header_;

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub(crate) struct FT_SfntName_ {
        pub(crate) platform_id: FT_UShort,
        pub(crate) encoding_id: FT_UShort,
        pub(crate) language_id: FT_UShort,
        pub(crate) name_id: FT_UShort,
        pub(crate) string: *mut FT_Byte,
        pub(crate) string_len: FT_UInt,
    }
    pub(crate) type FT_SfntName = FT_SfntName_;
}

#[cfg(target_os = "macos")]
pub(crate) mod cf_prelude {
    pub(crate) use core_foundation::{
        array::{
            kCFTypeArrayCallBacks, CFArrayCallBacks, CFArrayCreate, CFArrayGetCount, CFArrayRef,
            __CFArray,
        },
        attributed_string::{CFAttributedString, CFAttributedStringCreate, CFAttributedStringRef},
        base::{
            kCFAllocatorDefault, kCFAllocatorNull, CFAllocatorRef, CFComparisonResult, CFEqual,
            CFHashCode, CFIndex, CFOptionFlags, CFRange, CFRelease, CFRetain, CFTypeRef, ToVoid,
        },
        boolean::{kCFBooleanTrue, CFBooleanRef},
        dictionary::{
            kCFTypeDictionaryKeyCallBacks, kCFTypeDictionaryValueCallBacks, CFDictionary,
            CFDictionaryAddValue, CFDictionaryCopyDescriptionCallBack, CFDictionaryCreate,
            CFDictionaryCreateMutable, CFDictionaryEqualCallBack, CFDictionaryGetValueIfPresent,
            CFDictionaryHashCallBack, CFDictionaryKeyCallBacks, CFDictionaryRef,
            CFDictionaryReleaseCallBack, CFDictionaryRetainCallBack, CFDictionaryValueCallBacks,
            CFMutableDictionaryRef,
        },
        number::{CFNumberCompare, CFNumberCreate, CFNumberGetValue, CFNumberRef, CFNumberType},
        set::{kCFTypeSetCallBacks, CFSetCreate, CFSetRef},
        string::{
            kCFStringEncodingUTF8, CFStringCompareFlags, CFStringCreateWithBytes,
            CFStringCreateWithCString, CFStringEncoding, CFStringGetCString, CFStringGetLength,
            CFStringRef,
        },
        url::{CFURLGetFileSystemRepresentation, CFURLRef},
    };
    pub(crate) const kCFNumberMaxType: CFNumberType = 16;
    pub(crate) const kCFNumberCGFloatType: CFNumberType = 16;
    pub(crate) const kCFNumberNSIntegerType: CFNumberType = 15;
    pub(crate) const kCFNumberCFIndexType: CFNumberType = 14;
    pub(crate) const kCFNumberDoubleType: CFNumberType = 13;
    pub(crate) const kCFNumberFloatType: CFNumberType = 12;
    pub(crate) const kCFNumberLongLongType: CFNumberType = 11;
    pub(crate) const kCFNumberLongType: CFNumberType = 10;
    pub(crate) const kCFNumberIntType: CFNumberType = 9;
    pub(crate) const kCFNumberShortType: CFNumberType = 8;
    pub(crate) const kCFNumberCharType: CFNumberType = 7;
    pub(crate) const kCFNumberFloat64Type: CFNumberType = 6;
    pub(crate) const kCFNumberFloat32Type: CFNumberType = 5;
    pub(crate) const kCFNumberSInt64Type: CFNumberType = 4;
    pub(crate) const kCFNumberSInt32Type: CFNumberType = 3;
    pub(crate) const kCFNumberSInt16Type: CFNumberType = 2;
    pub(crate) const kCFNumberSInt8Type: CFNumberType = 1;

    pub(crate) const kCFStringEncodingUTF32LE: CFStringEncoding = 469762304;
    pub(crate) const kCFStringEncodingUTF32BE: CFStringEncoding = 402653440;
    pub(crate) const kCFStringEncodingUTF32: CFStringEncoding = 201326848;
    pub(crate) const kCFStringEncodingUTF16LE: CFStringEncoding = 335544576;
    pub(crate) const kCFStringEncodingUTF16BE: CFStringEncoding = 268435712;
    pub(crate) const kCFStringEncodingUTF16: CFStringEncoding = 256;
    pub(crate) const kCFStringEncodingNonLossyASCII: CFStringEncoding = 3071;
    pub(crate) const kCFStringEncodingUnicode: CFStringEncoding = 256;
    pub(crate) const kCFStringEncodingASCII: CFStringEncoding = 1536;
    pub(crate) const kCFStringEncodingNextStepLatin: CFStringEncoding = 2817;
    pub(crate) const kCFStringEncodingISOLatin1: CFStringEncoding = 513;
    pub(crate) const kCFStringEncodingWindowsLatin1: CFStringEncoding = 1280;
    pub(crate) const kCFStringEncodingMacRoman: CFStringEncoding = 0;

    pub(crate) const kCFCompareForcedOrdering: CFStringCompareFlags = 512;
    pub(crate) const kCFCompareWidthInsensitive: CFStringCompareFlags = 256;
    pub(crate) const kCFCompareDiacriticInsensitive: CFStringCompareFlags = 128;
    pub(crate) const kCFCompareNumerically: CFStringCompareFlags = 64;
    pub(crate) const kCFCompareLocalized: CFStringCompareFlags = 32;
    pub(crate) const kCFCompareNonliteral: CFStringCompareFlags = 16;
    pub(crate) const kCFCompareAnchored: CFStringCompareFlags = 8;
    pub(crate) const kCFCompareBackwards: CFStringCompareFlags = 4;
    pub(crate) const kCFCompareCaseInsensitive: CFStringCompareFlags = 1;

    // The CFArray wrapper is not mutable, so we use the APIs directly
    pub(crate) type CFMutableArrayRef = *mut __CFArray;
    extern "C" {
        #[no_mangle]
        pub(crate) fn CFArrayCreateMutable(
            allocator: CFAllocatorRef,
            capacity: CFIndex,
            callBacks: *const CFArrayCallBacks,
        ) -> CFMutableArrayRef;
        #[no_mangle]
        pub(crate) fn CFArrayAppendValue(theArray: CFMutableArrayRef, value: *const libc::c_void);
    }
    extern "C" {
        // Missing
        #[no_mangle]
        pub(crate) fn CFStringCreateWithCStringNoCopy(
            alloc: CFAllocatorRef,
            cStr: *const libc::c_char,
            encoding: CFStringEncoding,
            contentsDeallocator: CFAllocatorRef,
        ) -> CFStringRef;
        #[no_mangle]
        pub(crate) fn CFStringCompare(
            theString1: CFStringRef,
            theString2: CFStringRef,
            compareOptions: CFStringCompareFlags,
        ) -> CFComparisonResult;
    }
    extern "C" {
        #[no_mangle]
        pub(crate) fn CFDictionaryGetValue(
            theDict: CFDictionaryRef,
            key: *const libc::c_void,
        ) -> *const libc::c_void;
    }

    // CFComparisonResult is missing PartialEq
    pub(crate) fn comparison_was(a: CFComparisonResult, b: CFComparisonResult) -> bool {
        match (a, b) {
            (CFComparisonResult::LessThan, CFComparisonResult::LessThan) => true,
            (CFComparisonResult::EqualTo, CFComparisonResult::EqualTo) => true,
            (CFComparisonResult::GreaterThan, CFComparisonResult::GreaterThan) => true,
            _ => false,
        }
    }

    pub(crate) use core_graphics::{
        base::CGFloat,
        color::{CGColor, SysCGColorRef as CGColorRef},
        font::CGGlyph,
        geometry::{CGAffineTransform, CGPoint, CGRect, CGSize},
    };
    extern "C" {
        #[no_mangle]
        pub(crate) static CGAffineTransformIdentity: CGAffineTransform;
        #[no_mangle]
        pub(crate) fn CGRectIsNull(rect: CGRect) -> bool;
    }
    pub(crate) use core_text::run::CTRunRef;
    extern "C" {
        #[no_mangle]
        pub(crate) fn CTRunGetGlyphCount(run: CTRunRef) -> CFIndex;
        #[no_mangle]
        pub(crate) fn CTRunGetAttributes(run: CTRunRef) -> CFDictionaryRef;
        #[no_mangle]
        pub(crate) fn CTRunGetGlyphs(run: CTRunRef, range: CFRange, buffer: *mut CGGlyph);
        #[no_mangle]
        pub(crate) fn CTRunGetPositions(run: CTRunRef, range: CFRange, buffer: *mut CGPoint);
        #[no_mangle]
        pub(crate) fn CTRunGetAdvances(run: CTRunRef, range: CFRange, buffer: *mut CGSize);
        #[no_mangle]
        pub(crate) fn CTLineGetGlyphCount(line: CTLineRef) -> CFIndex;
        #[no_mangle]
        pub(crate) fn CTLineGetGlyphRuns(line: CTLineRef) -> CFArrayRef;
        #[no_mangle]
        pub(crate) fn CTRunGetTypographicBounds(
            run: CTRunRef,
            range: CFRange,
            ascent: *mut CGFloat,
            descent: *mut CGFloat,
            leading: *mut CGFloat,
        ) -> libc::c_double;
    }

    pub(crate) use core_text::{
        font::CTFontRef,
        font_descriptor::{
            kCTFontCascadeListAttribute, kCTFontDisplayNameAttribute, kCTFontFamilyNameAttribute,
            kCTFontFeatureSettingsAttribute, kCTFontNameAttribute, kCTFontOrientationAttribute,
            kCTFontURLAttribute, CTFontDescriptor, CTFontDescriptorCopyAttribute,
            CTFontDescriptorCreateCopyWithAttributes,
            CTFontDescriptorCreateMatchingFontDescriptors, CTFontDescriptorCreateWithAttributes,
            CTFontDescriptorCreateWithNameAndSize, CTFontDescriptorRef, CTFontOrientation,
        },
        line::CTLineRef,
        string_attributes::{
            kCTFontAttributeName, kCTForegroundColorAttributeName, kCTKernAttributeName,
            kCTVerticalFormsAttributeName,
        },
    };

    pub(crate) const kCTFontVerticalOrientation: CTFontOrientation = 2;
    pub(crate) const kCTFontHorizontalOrientation: CTFontOrientation = 1;
    pub(crate) const kCTFontDefaultOrientation: CTFontOrientation = 0;
    pub(crate) const kCTFontOrientationVertical: CTFontOrientation = 2;
    pub(crate) const kCTFontOrientationHorizontal: CTFontOrientation = 1;
    pub(crate) const kCTFontOrientationDefault: CTFontOrientation = 0;

    // The CGFont wrapper is not feature complete.
    pub(crate) type CGFontRef = *const __CGFont;
    extern "C" {
        pub(crate) type __CGFont;
        #[no_mangle]
        pub(crate) fn CGFontGetNumberOfGlyphs(font: CGFontRef) -> usize;
        #[no_mangle]
        pub(crate) fn CGFontRelease(font: CGFontRef);
        #[no_mangle]
        pub(crate) fn CGFontCopyGlyphNameForGlyph(font: CGFontRef, glyph: CGGlyph) -> CFStringRef;
        #[no_mangle]
        pub(crate) fn CTFontCopyGraphicsFont(
            font: CTFontRef,
            attributes: *mut CTFontDescriptorRef,
        ) -> CGFontRef;
        #[no_mangle]
        pub(crate) static kCTFontPostScriptNameKey: CFStringRef;
    }
    // Typesetters
    pub(crate) type CTTypesetterRef = *const __CTTypesetter;
    extern "C" {
        pub(crate) type __CTTypesetter;
        #[no_mangle]
        pub(crate) fn CTTypesetterCreateWithAttributedString(
            string: CFAttributedStringRef,
        ) -> CTTypesetterRef;
        #[no_mangle]
        pub(crate) fn CTTypesetterCreateLine(
            typesetter: CTTypesetterRef,
            stringRange: CFRange,
        ) -> CTLineRef;
    }

    // misc
    extern "C" {
        #[no_mangle]
        pub(crate) fn CTFontCreateWithFontDescriptor(
            descriptor: CTFontDescriptorRef,
            size: CGFloat,
            matrix: *const CGAffineTransform,
        ) -> CTFontRef;
        #[no_mangle]
        pub(crate) fn CTLineCreateJustifiedLine(
            line: CTLineRef,
            justificationFactor: CGFloat,
            justificationWidth: libc::c_double,
        ) -> CTLineRef;
        #[no_mangle]
        pub(crate) fn CFStringCreateWithCharactersNoCopy(
            alloc: CFAllocatorRef,
            chars: *const UniChar,
            numChars: CFIndex,
            contentsDeallocator: CFAllocatorRef,
        ) -> CFStringRef;
        #[no_mangle]
        pub(crate) fn CTFontCreateCopyWithAttributes(
            font: CTFontRef,
            size: CGFloat,
            matrix: *const CGAffineTransform,
            attributes: CTFontDescriptorRef,
        ) -> CTFontRef;
        #[no_mangle]
        pub(crate) fn CTFontCopyAttribute(font: CTFontRef, attribute: CFStringRef) -> CFTypeRef;
        #[no_mangle]
        pub(crate) fn CTFontCopyName(font: CTFontRef, nameKey: CFStringRef) -> CFStringRef;
        #[no_mangle]
        pub(crate) fn CTFontCopyLocalizedName(
            font: CTFontRef,
            nameKey: CFStringRef,
            actualLanguage: *mut CFStringRef,
        ) -> CFStringRef;
        #[no_mangle]
        pub(crate) fn CTFontGetGlyphsForCharacters(
            font: CTFontRef,
            characters: *const UniChar,
            glyphs: *mut CGGlyph,
            count: CFIndex,
        ) -> bool;
        #[no_mangle]
        pub(crate) fn CTFontGetGlyphWithName(font: CTFontRef, glyphName: CFStringRef) -> CGGlyph;
        #[no_mangle]
        pub(crate) fn CTFontGetBoundingRectsForGlyphs(
            font: CTFontRef,
            orientation: CTFontOrientation,
            glyphs: *const CGGlyph,
            boundingRects: *mut CGRect,
            count: CFIndex,
        ) -> CGRect;
        #[no_mangle]
        pub(crate) fn CTFontGetAdvancesForGlyphs(
            font: CTFontRef,
            orientation: CTFontOrientation,
            glyphs: *const CGGlyph,
            advances: *mut CGSize,
            count: CFIndex,
        ) -> libc::c_double;
        #[no_mangle]
        pub(crate) static kCTFontFeatureTypeIdentifierKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureTypeNameKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureTypeSelectorsKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureTypeExclusiveKey: CFStringRef;
        #[no_mangle]
        pub(crate) fn CTFontCopyFeatures(font: CTFontRef) -> CFArrayRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureSelectorNameKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureSelectorDefaultKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFeatureSelectorIdentifierKey: CFStringRef;

        #[no_mangle]
        pub(crate) static kCTFontFullNameKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontStyleNameKey: CFStringRef;
        #[no_mangle]
        pub(crate) static kCTFontFamilyNameKey: CFStringRef;

        #[no_mangle]
        pub(crate) fn CTFontGetXHeight(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CTFontGetAscent(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CTFontGetDescent(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CTFontGetGlyphCount(font: CTFontRef) -> CFIndex;
        #[no_mangle]
        pub(crate) fn CTFontGetSlantAngle(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CTFontGetCapHeight(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CFStringGetCharacters(
            theString: CFStringRef,
            range: CFRange,
            buffer: *mut UniChar,
        );
        #[no_mangle]
        pub(crate) fn CFArrayGetValueAtIndex(
            theArray: CFArrayRef,
            idx: CFIndex,
        ) -> *const libc::c_void;
        #[no_mangle]
        pub(crate) fn CTFontGetSize(font: CTFontRef) -> CGFloat;
        #[no_mangle]
        pub(crate) fn CTFontGetMatrix(font: CTFontRef) -> CGAffineTransform;
        #[no_mangle]
        pub(crate) fn CGColorGetComponents(color: CGColorRef) -> *const CGFloat;
        #[no_mangle]
        pub(crate) fn CFBooleanGetValue(boolean: CFBooleanRef) -> libc::c_uchar;
    }

    pub(crate) type UniChar = u16;

    #[inline(always)]
    pub(crate) fn CFRangeMake(mut loc: CFIndex, mut len: CFIndex) -> CFRange {
        let mut range: CFRange = CFRange {
            location: 0,
            length: 0,
        };
        range.location = loc;
        range.length = len;
        range
    }

    pub(crate) unsafe fn cgColorToRGBA32(mut color: CGColorRef) -> u32 {
        let mut components: *const CGFloat = CGColorGetComponents(color);
        let mut rval: u32 = (*components.offset(0) * 255.0f64 + 0.5f64) as u8 as u32;
        rval <<= 8i32;
        rval = (rval as u32).wrapping_add((*components.offset(1) * 255.0f64 + 0.5f64) as u8 as u32);
        rval <<= 8i32;
        rval = (rval as u32).wrapping_add((*components.offset(2) * 255.0f64 + 0.5f64) as u8 as u32);
        rval <<= 8i32;
        rval = (rval as u32).wrapping_add((*components.offset(3) * 255.0f64 + 0.5f64) as u8 as u32);
        return rval;
    }
}

#[macro_export]
macro_rules! help(
    () => {
        crate::xetex_ini::help_ptr = 0;
    };
    ($s0: expr) => {
        crate::xetex_ini::help_ptr = 1;
        crate::xetex_ini::help_line[0] = $s0;
    };
    ($s1: expr, $s0: expr) => {
        crate::xetex_ini::help_ptr = 2;
        crate::xetex_ini::help_line[1] = $s1;
        crate::xetex_ini::help_line[0] = $s0;
    };
    ($s2: expr, $s1: expr, $s0: expr) => {
        crate::xetex_ini::help_ptr = 3;
        crate::xetex_ini::help_line[2] = $s2;
        crate::xetex_ini::help_line[1] = $s1;
        crate::xetex_ini::help_line[0] = $s0;
    };
    ($s3: expr, $s2: expr, $s1: expr, $s0: expr) => {
        crate::xetex_ini::help_ptr = 4;
        crate::xetex_ini::help_line[3] = $s3;
        crate::xetex_ini::help_line[2] = $s2;
        crate::xetex_ini::help_line[1] = $s1;
        crate::xetex_ini::help_line[0] = $s0;
    };
    ($s4: expr, $s3: expr, $s2: expr, $s1: expr, $s0: expr) => {
        crate::xetex_ini::help_ptr = 5;
        crate::xetex_ini::help_line[4] = $s4;
        crate::xetex_ini::help_line[3] = $s3;
        crate::xetex_ini::help_line[2] = $s2;
        crate::xetex_ini::help_line[1] = $s1;
        crate::xetex_ini::help_line[0] = $s0;
    };
    ($s5: expr, $s4: expr, $s3: expr, $s2: expr, $s1: expr, $s0: expr) => {
        crate::xetex_ini::help_ptr = 6;
        crate::xetex_ini::help_line[5] = $s5;
        crate::xetex_ini::help_line[4] = $s4;
        crate::xetex_ini::help_line[3] = $s3;
        crate::xetex_ini::help_line[2] = $s2;
        crate::xetex_ini::help_line[1] = $s1;
        crate::xetex_ini::help_line[0] = $s0;
    };
);

pub(crate) fn c_pointer_to_str<'a>(p: *const i8) -> &'a str {
    if p.is_null() {
        ""
    } else {
        unsafe { std::ffi::CStr::from_ptr(p).to_str().unwrap() }
    }
}
