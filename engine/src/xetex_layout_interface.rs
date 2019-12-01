#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

use harfbuzz_sys::{hb_feature_t, hb_ot_math_glyph_part_t, hb_tag_t};

#[cfg(target_os = "macos")]
use crate::xetex_aatfont::cf_prelude::{CTFontDescriptorRef, CTFontRef};

#[cfg(not(target_os = "macos"))]
pub(crate) type PlatformFontRef = *mut FcPattern;
#[cfg(target_os = "macos")]
pub(crate) type PlatformFontRef = CTFontDescriptorRef;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct GlyphAssembly {
    pub(crate) count: u32,
    pub(crate) parts: *mut hb_ot_math_glyph_part_t,
}

pub(crate) use crate::xetex_font_info::{Fixed, GlyphBBox};
pub(crate) type scaled_t = i32;

#[derive(Copy, Clone)]
#[cfg_attr(not(target_os = "macos"), repr(C))]
#[cfg_attr(target_os = "macos", repr(C, packed(2)))]
pub(crate) struct FixedPoint {
    pub(crate) x: Fixed,
    pub(crate) y: Fixed,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct FloatPoint {
    pub(crate) x: f32,
    pub(crate) y: f32,
}

use crate::core_memory::{xcalloc, xmalloc};
use harfbuzz_sys::*;
use std::ptr;

use freetype::freetype_sys;

#[path = "xetex_opentype_math.rs"]
mod opentype_math;

pub(crate) use opentype_math::*;

use crate::core_memory::xstrdup;
use libc::{free, malloc, strcmp, strdup, strlen, strncmp};

extern "C" {
    pub(crate) type gr_face;
    pub(crate) type gr_font;
    pub(crate) type gr_feature_ref;
    pub(crate) type gr_feature_val;
    pub(crate) type gr_char_info;
    pub(crate) type gr_segment;
    pub(crate) type gr_slot;
    #[no_mangle]
    #[cfg(not(target_os = "macos"))]
    fn FcPatternGetInteger(
        p: *const FcPattern,
        object: *const libc::c_char,
        n: libc::c_int,
        i: *mut libc::c_int,
    ) -> FcResult;
    #[no_mangle]
    #[cfg(not(target_os = "macos"))]
    fn FcPatternGetString(
        p: *const FcPattern,
        object: *const libc::c_char,
        n: libc::c_int,
        s: *mut *mut FcChar8,
    ) -> FcResult;
    #[no_mangle]
    fn hb_unicode_funcs_set_decompose_compatibility_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_decompose_compatibility_func_t,
        user_data: *mut libc::c_void,
        destroy: hb_destroy_func_t,
    );
    #[no_mangle]
    fn hb_ot_layout_script_find_language(
        face: *mut hb_face_t,
        table_tag: hb_tag_t,
        script_index: libc::c_uint,
        language_tag: hb_tag_t,
        language_index: *mut libc::c_uint,
    ) -> hb_bool_t;

    #[no_mangle]
    fn gr_face_featureval_for_lang(
        pFace: *const gr_face,
        langname: gr_uint32,
    ) -> *mut gr_feature_val;
    #[no_mangle]
    fn gr_face_find_fref(pFace: *const gr_face, featId: gr_uint32) -> *const gr_feature_ref;
    #[no_mangle]
    fn gr_face_n_fref(pFace: *const gr_face) -> gr_uint16;
    #[no_mangle]
    fn gr_face_fref(pFace: *const gr_face, i: gr_uint16) -> *const gr_feature_ref;
    #[no_mangle]
    fn gr_fref_feature_value(
        pfeatureref: *const gr_feature_ref,
        feats: *const gr_feature_val,
    ) -> gr_uint16;
    #[no_mangle]
    fn gr_fref_set_feature_value(
        pfeatureref: *const gr_feature_ref,
        val: gr_uint16,
        pDest: *mut gr_feature_val,
    ) -> libc::c_int;
    #[no_mangle]
    fn gr_fref_id(pfeatureref: *const gr_feature_ref) -> gr_uint32;
    #[no_mangle]
    fn gr_fref_n_values(pfeatureref: *const gr_feature_ref) -> gr_uint16;
    #[no_mangle]
    fn gr_fref_value(pfeatureref: *const gr_feature_ref, settingno: gr_uint16) -> gr_int16;
    #[no_mangle]
    fn gr_fref_label(
        pfeatureref: *const gr_feature_ref,
        langId: *mut gr_uint16,
        utf: gr_encform,
        length: *mut gr_uint32,
    ) -> *mut libc::c_void;
    #[no_mangle]
    fn gr_fref_value_label(
        pfeatureref: *const gr_feature_ref,
        settingno: gr_uint16,
        langId: *mut gr_uint16,
        utf: gr_encform,
        length: *mut gr_uint32,
    ) -> *mut libc::c_void;
    #[no_mangle]
    pub(crate) fn gr_label_destroy(label: *mut libc::c_void);
    #[no_mangle]
    fn gr_cinfo_break_weight(p: *const gr_char_info) -> libc::c_int;
    #[no_mangle]
    fn gr_cinfo_base(p: *const gr_char_info) -> size_t;
    #[no_mangle]
    fn gr_make_seg(
        font: *const gr_font,
        face: *const gr_face,
        script: gr_uint32,
        pFeats: *const gr_feature_val,
        enc: gr_encform,
        pStart: *const libc::c_void,
        nChars: size_t,
        dir: libc::c_int,
    ) -> *mut gr_segment;
    #[no_mangle]
    fn gr_seg_destroy(p: *mut gr_segment);
    #[no_mangle]
    fn gr_seg_cinfo(pSeg: *const gr_segment, index: libc::c_uint) -> *const gr_char_info;
    #[no_mangle]
    fn gr_seg_first_slot(pSeg: *mut gr_segment) -> *const gr_slot;
    #[no_mangle]
    fn gr_seg_last_slot(pSeg: *mut gr_segment) -> *const gr_slot;
    #[no_mangle]
    fn gr_slot_next_in_segment(p: *const gr_slot) -> *const gr_slot;
    #[no_mangle]
    fn gr_slot_index(p: *const gr_slot) -> libc::c_uint;
    #[no_mangle]
    fn hb_graphite2_face_get_gr_face(face: *mut hb_face_t) -> *mut gr_face;
    #[no_mangle]
    fn hb_graphite2_font_get_gr_font(font: *mut hb_font_t) -> *mut gr_font;
    #[no_mangle]
    fn hb_icu_get_unicode_funcs() -> *mut hb_unicode_funcs_t;
    #[no_mangle]
    fn Fix2D(f: Fixed) -> libc::c_double;
    #[no_mangle]
    fn D2Fix(d: libc::c_double) -> Fixed;
}

pub(crate) use crate::xetex_font_manager::XeTeXFont;

use crate::xetex_font_manager::{
    XeTeXFontMgr_Destroy, XeTeXFontMgr_GetFontManager, XeTeXFontMgr_Terminate,
    XeTeXFontMgr_findFont, XeTeXFontMgr_getDesignSize, XeTeXFontMgr_getFullName,
    XeTeXFontMgr_getReqEngine, XeTeXFontMgr_setReqEngine,
};

use crate::xetex_font_info::XeTeXFontInst;

use crate::xetex_font_info::{
    XeTeXFontInst_getFirstCharCode, XeTeXFontInst_getFontTable, XeTeXFontInst_getGlyphBounds,
    XeTeXFontInst_getGlyphHeightDepth, XeTeXFontInst_getGlyphItalCorr, XeTeXFontInst_getGlyphName,
    XeTeXFontInst_getGlyphSidebearings, XeTeXFontInst_getGlyphWidth, XeTeXFontInst_getHbFont,
    XeTeXFontInst_getLastCharCode, XeTeXFontInst_getNumGlyphs, XeTeXFontInst_mapCharToGlyph,
    XeTeXFontInst_mapGlyphToIndex, XeTeXFontInst_pointsToUnits, XeTeXFontInst_setLayoutDirVertical,
    XeTeXFontInst_unitsToPoints,
};

pub(crate) mod collection_types {

    use std::collections::{BTreeMap, VecDeque};
    use std::ffi::CStr;
    use std::ffi::CString;

    pub(crate) type CppStdString = CString;
    pub(crate) type CppStdListOfString = VecDeque<CString>;
    pub(crate) type CppStdMap<K, V> = BTreeMap<K, V>;

    pub(crate) fn CppStdString_create() -> *mut CppStdString {
        Box::into_raw(Box::new(CString::default()))
    }

    pub(crate) unsafe fn CppStdString_delete(self_0: *mut CppStdString) {
        let _: Box<CppStdString> = Box::from_raw(self_0);
    }
    pub(crate) unsafe fn CppStdString_length(self_0: *const CppStdString) -> libc::size_t {
        self_0.as_ref().unwrap().to_bytes().len() as _
    }
    pub(crate) unsafe fn CppStdString_cstr(self_0: *const CppStdString) -> *const libc::c_char {
        let v = self_0.as_ref().unwrap();
        v.as_ptr()
    }

    pub(crate) fn CppStdListOfString_create() -> *mut CppStdListOfString {
        Box::into_raw(Box::new(CppStdListOfString::default()))
    }

    pub(crate) unsafe fn CppStdListOfString_delete(self_0: *mut CppStdListOfString) {
        let _: Box<CppStdListOfString> = Box::from_raw(self_0);
    }

    pub(crate) fn CppStdMap_create<K: Ord, V>() -> *mut CppStdMap<K, V> {
        Box::into_raw(Box::new(CppStdMap::default()))
    }

    pub(crate) unsafe fn CppStdMap_put<K: Ord, V>(self_0: *mut CppStdMap<K, V>, key: K, val: V) {
        (*self_0).insert(key, val);
    }

    pub(crate) unsafe fn CppStdMap_put_with_string_key<V>(
        self_0: *mut CppStdMap<CString, V>,
        key: *const libc::c_char,
        val: V,
    ) {
        let key = CStr::from_ptr(key);
        match (*self_0).get_mut(key) {
            Some(v) => {
                *v = val;
            }
            None => {
                (*self_0).insert(key.to_owned(), val);
            }
        }
    }

    pub(crate) unsafe fn CppStdMap_delete<K: Ord, V>(self_0: *mut CppStdMap<K, V>) {
        let _: Box<CppStdMap<K, V>> = Box::from_raw(self_0);
    }

    pub(crate) unsafe fn CppStdString_last(self_0: *const CppStdString) -> libc::c_char {
        let val = &*self_0;
        *val.to_bytes().last().expect("must not be empty") as libc::c_char
    }
    pub(crate) unsafe fn CppStdString_clone(self_0: *const CppStdString) -> *mut CppStdString {
        let v: Box<CppStdString> = Box::new((*self_0).clone());
        Box::into_raw(v)
    }

    pub(crate) unsafe fn CppStdString_append_const_char_ptr(
        self_0: *mut CppStdString,
        val: *const libc::c_char,
    ) {
        use std::mem::swap;
        let o: &mut CppStdString = &mut *self_0;
        let mut v: CppStdString = Default::default();
        swap(o, &mut v);
        let mut u = v.into_bytes();
        u.extend(CStr::from_ptr(val).to_bytes());
        v = CString::from_vec_unchecked(u);
        swap(o, &mut v);
    }

    pub(crate) unsafe fn CppStdString_assign_from_const_char_ptr(
        self_0: *mut CppStdString,
        val: *const libc::c_char,
    ) {
        let o: &mut CppStdString = &mut *self_0;
        *o = CStr::from_ptr(val).to_owned();
    }

    pub(crate) unsafe fn CppStdString_assign_n_chars(
        self_0: *mut CppStdString,
        val: *const libc::c_char,
        count: usize,
    ) {
        let o: &mut CppStdString = &mut *self_0;
        let slice = std::slice::from_raw_parts(val as *const u8, count);
        *o = CString::from_vec_unchecked(slice.to_owned());
    }
}

use self::collection_types::*;

pub(crate) type size_t = usize;
pub(crate) type int8_t = i8;
pub(crate) type int16_t = i16;
pub(crate) type int32_t = i32;
pub(crate) type uint8_t = u8;
pub(crate) type uint16_t = u16;
pub(crate) type uint32_t = u32;

pub(crate) type UChar32 = int32_t;
#[cfg(not(target_os = "macos"))]
pub(crate) type FcChar8 = libc::c_uchar;
#[cfg(not(target_os = "macos"))]
use crate::xetex_font_manager::imp::{FcPattern, FcResult};

pub(crate) type hb_unicode_decompose_compatibility_func_t = Option<
    unsafe extern "C" fn(
        _: *mut hb_unicode_funcs_t,
        _: hb_codepoint_t,
        _: *mut hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> libc::c_uint,
>;

pub(crate) type OTTag = uint32_t;
pub(crate) type GlyphID = uint16_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXLayoutEngine_rec {
    pub(crate) font: *mut XeTeXFontInst,
    pub(crate) fontRef: PlatformFontRef,
    pub(crate) script: hb_tag_t,
    pub(crate) language: hb_language_t,
    pub(crate) features: *mut hb_feature_t,
    pub(crate) ShaperList: *mut *mut libc::c_char,
    pub(crate) shaper: *mut libc::c_char,
    pub(crate) nFeatures: libc::c_int,
    pub(crate) rgbValue: uint32_t,
    pub(crate) extend: libc::c_float,
    pub(crate) slant: libc::c_float,
    pub(crate) embolden: libc::c_float,
    pub(crate) hbBuffer: *mut hb_buffer_t,
}

pub(crate) type XeTeXLayoutEngine = *mut XeTeXLayoutEngine_rec;
pub(crate) type gr_uint16 = libc::c_ushort;
pub(crate) type gr_int16 = libc::c_short;
pub(crate) type gr_uint32 = libc::c_uint;
pub(crate) type gr_encform = libc::c_uint;
pub(crate) const gr_utf32: gr_encform = 4;
pub(crate) const gr_utf16: gr_encform = 2;
pub(crate) const gr_utf8: gr_encform = 1;
pub(crate) type gr_break_weight = libc::c_int;
pub(crate) const gr_breakBeforeClip: gr_break_weight = -40;
pub(crate) const gr_breakBeforeLetter: gr_break_weight = -30;
pub(crate) const gr_breakBeforeIntra: gr_break_weight = -20;
pub(crate) const gr_breakBeforeWord: gr_break_weight = -15;
pub(crate) const gr_breakBeforeWhitespace: gr_break_weight = -10;
pub(crate) const gr_breakClip: gr_break_weight = 40;
pub(crate) const gr_breakLetter: gr_break_weight = 30;
pub(crate) const gr_breakIntra: gr_break_weight = 20;
pub(crate) const gr_breakWord: gr_break_weight = 15;
pub(crate) const gr_breakWhitespace: gr_break_weight = 10;
pub(crate) const gr_breakNone: gr_break_weight = 0;

pub(crate) type ProtrusionFactor = CppStdMap<GlyphId, libc::c_int>;

use crate::xetex_font_manager::XeTeXFontMgr;

/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew
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

/* The following code used to be in a file called "hz.cpp" and there's no
 * particular reason for it to be here, but it was a tiny file with a weird
 * name so I wanted to get rid of it. The functions are invoked from the C
 * code. */
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub(crate) struct GlyphId {
    pub(crate) fontNum: libc::c_int,
    pub(crate) code: libc::c_uint,
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getDescent(mut self_0: *const XeTeXFontInst) -> libc::c_float {
    return (*self_0).m_descent;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getLayoutDirVertical(mut self_0: *const XeTeXFontInst) -> bool {
    return (*self_0).m_vertical;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getPointSize(mut self_0: *const XeTeXFontInst) -> libc::c_float {
    return (*self_0).m_pointSize;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getAscent(mut self_0: *const XeTeXFontInst) -> libc::c_float {
    return (*self_0).m_ascent;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getCapHeight(mut self_0: *const XeTeXFontInst) -> libc::c_float {
    return (*self_0).m_capHeight;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getXHeight(mut self_0: *const XeTeXFontInst) -> libc::c_float {
    return (*self_0).m_xHeight;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getItalicAngle(
    mut self_0: *const XeTeXFontInst,
) -> libc::c_float {
    return (*self_0).m_italicAngle;
}
#[inline]
unsafe extern "C" fn XeTeXFontInst_getFilename(
    mut self_0: *const XeTeXFontInst,
    mut index: *mut uint32_t,
) -> *const libc::c_char {
    *index = (*self_0).m_index;
    return (*self_0).m_filename;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphBBoxCache() -> *mut CppStdMap<u32, GlyphBBox> {
    static mut cache: *mut CppStdMap<u32, GlyphBBox> = ptr::null_mut();
    if cache.is_null() {
        cache = CppStdMap_create()
    }
    return cache;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getCachedGlyphBBox(
    mut fontID: uint16_t,
    mut glyphID: uint16_t,
    mut bbox: *mut GlyphBBox,
) -> libc::c_int {
    let mut sGlyphBoxes: *mut CppStdMap<u32, GlyphBBox> = getGlyphBBoxCache();
    let mut key: uint32_t = ((fontID as uint32_t) << 16i32).wrapping_add(glyphID as libc::c_uint);
    if let Some(v) = (*sGlyphBoxes).get(&key) {
        *bbox = v.clone();
        1
    } else {
        0
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn cacheGlyphBBox(
    mut fontID: uint16_t,
    mut glyphID: uint16_t,
    mut bbox: *const GlyphBBox,
) {
    let mut sGlyphBoxes: *mut CppStdMap<u32, GlyphBBox> = getGlyphBBoxCache();
    let mut key: uint32_t = ((fontID as uint32_t) << 16i32).wrapping_add(glyphID as libc::c_uint);
    CppStdMap_put(sGlyphBoxes, key, *bbox);
}
#[inline]
unsafe extern "C" fn GlyphId_create(mut fontNum: libc::c_int, mut code: libc::c_uint) -> GlyphId {
    let mut id: GlyphId = GlyphId {
        fontNum: 0,
        code: 0,
    };
    id.fontNum = fontNum;
    id.code = code;
    return id;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getProtrusionFactor(
    mut side: libc::c_int,
) -> *mut ProtrusionFactor {
    static mut leftProt: *mut ProtrusionFactor = ptr::null_mut();
    static mut rightProt: *mut ProtrusionFactor = ptr::null_mut();
    let mut container: *mut ProtrusionFactor = 0 as *mut ProtrusionFactor;
    match side {
        0 => {
            if leftProt.is_null() {
                leftProt = CppStdMap_create()
            }
            container = leftProt
            // we should not reach here
        }
        1 => {
            if rightProt.is_null() {
                rightProt = CppStdMap_create()
            }
            container = rightProt
        }
        _ => {
            unreachable!();
        }
    }
    return container;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn set_cp_code(
    mut fontNum: libc::c_int,
    mut code: libc::c_uint,
    mut side: libc::c_int,
    mut value: libc::c_int,
) {
    let mut id: GlyphId = GlyphId_create(fontNum, code);
    let mut container: *mut ProtrusionFactor = getProtrusionFactor(side);
    CppStdMap_put(container, id, value);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn get_cp_code(
    mut fontNum: libc::c_int,
    mut code: libc::c_uint,
    mut side: libc::c_int,
) -> libc::c_int {
    let mut id: GlyphId = GlyphId_create(fontNum, code);
    let mut container: *mut ProtrusionFactor = getProtrusionFactor(side);
    (*container).get(&id).cloned().unwrap_or(0)
}
/* ******************************************************************/
#[no_mangle]
pub(crate) unsafe extern "C" fn terminate_font_manager() {
    XeTeXFontMgr_Terminate();
}
#[no_mangle]
pub(crate) unsafe extern "C" fn destroy_font_manager() {
    XeTeXFontMgr_Destroy();
}
#[no_mangle]
pub(crate) unsafe extern "C" fn createFont(
    mut fontRef: PlatformFontRef,
    mut pointSize: Fixed,
) -> XeTeXFont {
    use crate::xetex_font_info::{XeTeXFontInst_create, XeTeXFontInst_delete};
    let mut status: libc::c_int = 0i32;
    let mut font: *mut XeTeXFontInst;
    #[cfg(not(target_os = "macos"))]
    {
        let mut pathname: *mut FcChar8 = 0 as *mut FcChar8;
        FcPatternGetString(
            fontRef as *const FcPattern,
            b"file\x00" as *const u8 as *const libc::c_char,
            0i32,
            &mut pathname,
        );
        let mut index: libc::c_int = 0;
        FcPatternGetInteger(
            fontRef as *const FcPattern,
            b"index\x00" as *const u8 as *const libc::c_char,
            0i32,
            &mut index,
        );
        font = XeTeXFontInst_create(
            pathname as *const libc::c_char,
            index,
            Fix2D(pointSize) as libc::c_float,
            &mut status,
        );
    }
    #[cfg(target_os = "macos")]
    {
        use crate::xetex_font_info::imp::XeTeXFontInst_Mac;
        use crate::xetex_font_info::imp::XeTeXFontInst_Mac_create;
        font = &mut (*(XeTeXFontInst_Mac_create
            as unsafe extern "C" fn(
                _: CTFontDescriptorRef,
                _: libc::c_float,
                _: *mut libc::c_int,
            ) -> *mut XeTeXFontInst_Mac)(
            fontRef,
            (Fix2D as unsafe extern "C" fn(_: Fixed) -> libc::c_double)(pointSize) as libc::c_float,
            &mut status,
        ))
        .super_;
    }
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return 0 as XeTeXFont;
    }
    return font as XeTeXFont;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn createFontFromFile(
    mut filename: *const libc::c_char,
    mut index: libc::c_int,
    mut pointSize: Fixed,
) -> XeTeXFont {
    use crate::xetex_font_info::{XeTeXFontInst_create, XeTeXFontInst_delete};
    let mut status: libc::c_int = 0i32;
    let mut font: *mut XeTeXFontInst = XeTeXFontInst_create(
        filename,
        index,
        Fix2D(pointSize) as libc::c_float,
        &mut status,
    );
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return 0 as XeTeXFont;
    }
    return font as XeTeXFont;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn setFontLayoutDir(mut font: XeTeXFont, mut vertical: libc::c_int) {
    XeTeXFontInst_setLayoutDirVertical(font as *mut XeTeXFontInst, vertical != 0i32);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn findFontByName(
    mut name: *const libc::c_char,
    mut var: *mut libc::c_char,
    mut size: libc::c_double,
) -> PlatformFontRef {
    return XeTeXFontMgr_findFont(XeTeXFontMgr_GetFontManager(), name, var, size);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getReqEngine() -> libc::c_char {
    return XeTeXFontMgr_getReqEngine(XeTeXFontMgr_GetFontManager());
}
#[no_mangle]
pub(crate) unsafe extern "C" fn setReqEngine(mut reqEngine: libc::c_char) {
    XeTeXFontMgr_setReqEngine(XeTeXFontMgr_GetFontManager(), reqEngine);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFullName(mut fontRef: PlatformFontRef) -> *const libc::c_char {
    return XeTeXFontMgr_getFullName(XeTeXFontMgr_GetFontManager(), fontRef);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getDesignSize(mut font: XeTeXFont) -> libc::c_double {
    return XeTeXFontMgr_getDesignSize(XeTeXFontMgr_GetFontManager(), font);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFontFilename(
    mut engine: XeTeXLayoutEngine,
    mut index: *mut uint32_t,
) -> *mut libc::c_char {
    return xstrdup(XeTeXFontInst_getFilename((*engine).font, index));
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFontRef(mut engine: XeTeXLayoutEngine) -> PlatformFontRef {
    return (*engine).fontRef;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn deleteFont(mut font: XeTeXFont) {
    use crate::xetex_font_info::XeTeXFontInst_delete;
    XeTeXFontInst_delete(font as *mut XeTeXFontInst);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFontTablePtr(
    mut font: XeTeXFont,
    mut tableTag: uint32_t,
) -> *mut libc::c_void {
    return XeTeXFontInst_getFontTable(font as *mut XeTeXFontInst, tableTag);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getSlant(mut font: XeTeXFont) -> Fixed {
    let mut italAngle: libc::c_float = XeTeXFontInst_getItalicAngle(font as *mut XeTeXFontInst);
    return D2Fix((-italAngle as libc::c_double * std::f64::consts::PI / 180.0f64).tan());
}
unsafe extern "C" fn getLargerScriptListTable(
    mut font: XeTeXFont,
    mut scriptList: *mut *mut hb_tag_t,
) -> libc::c_uint {
    use crate::bridge::size_t;
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t =
        hb_font_get_face(XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst));
    let mut scriptListSub: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptListPos: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCountSub: libc::c_uint = hb_ot_layout_table_get_script_tags(
        face,
        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
            | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
            | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
            | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
        0i32 as libc::c_uint,
        0 as *mut libc::c_uint,
        0 as *mut hb_tag_t,
    );
    scriptListSub = xcalloc(
        scriptCountSub as size_t,
        ::std::mem::size_of::<*mut hb_tag_t>() as _,
    ) as *mut hb_tag_t;
    hb_ot_layout_table_get_script_tags(
        face,
        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
            | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
            | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
            | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
        0i32 as libc::c_uint,
        &mut scriptCountSub,
        scriptListSub,
    );
    let mut scriptCountPos: libc::c_uint = hb_ot_layout_table_get_script_tags(
        face,
        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
            | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
            | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
            | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint,
        0i32 as libc::c_uint,
        0 as *mut libc::c_uint,
        0 as *mut hb_tag_t,
    );
    scriptListPos = xcalloc(
        scriptCountPos as size_t,
        ::std::mem::size_of::<*mut hb_tag_t>() as _,
    ) as *mut hb_tag_t;
    hb_ot_layout_table_get_script_tags(
        face,
        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
            | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
            | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
            | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
        0i32 as libc::c_uint,
        &mut scriptCountPos,
        scriptListPos,
    );
    if scriptCountSub > scriptCountPos {
        if !scriptList.is_null() {
            *scriptList = scriptListSub
        }
        rval = scriptCountSub
    } else {
        if !scriptList.is_null() {
            *scriptList = scriptListPos
        }
        rval = scriptCountPos
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countScripts(mut font: XeTeXFont) -> libc::c_uint {
    return getLargerScriptListTable(font, 0 as *mut *mut hb_tag_t);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getIndScript(
    mut font: XeTeXFont,
    mut index: libc::c_uint,
) -> hb_tag_t {
    let mut rval: hb_tag_t = 0i32 as hb_tag_t;
    let mut scriptList: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCount: libc::c_uint = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        if index < scriptCount {
            rval = *scriptList.offset(index as isize)
        }
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countLanguages(
    mut font: XeTeXFont,
    mut script: hb_tag_t,
) -> libc::c_uint {
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t =
        hb_font_get_face(XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst));
    let mut scriptList: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCount: libc::c_uint = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        let mut i: libc::c_uint = 0i32 as libc::c_uint;
        while i < scriptCount {
            if *scriptList.offset(i as isize) == script {
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                        | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                        | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                        | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                    i,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                ));
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                        | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                        | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                        | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                    i,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                ));
                break;
            } else {
                i = i.wrapping_add(1)
            }
        }
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getIndLanguage(
    mut font: XeTeXFont,
    mut script: hb_tag_t,
    mut index: libc::c_uint,
) -> hb_tag_t {
    use crate::bridge::size_t;
    let mut rval: hb_tag_t = 0i32 as hb_tag_t;
    let mut face: *mut hb_face_t =
        hb_font_get_face(XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst));
    let mut scriptList: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCount: libc::c_uint = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        let mut i: libc::c_uint = 0i32 as libc::c_uint;
        while i < scriptCount {
            if *scriptList.offset(i as isize) == script {
                let mut langCount: libc::c_uint = 0;
                let mut langList: *mut hb_tag_t = 0 as *mut hb_tag_t;
                langCount = hb_ot_layout_script_get_language_tags(
                    face,
                    ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                        | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                        | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                        | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                    i,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                );
                langList = xcalloc(
                    langCount as size_t,
                    ::std::mem::size_of::<*mut hb_tag_t>() as _,
                ) as *mut hb_tag_t;
                hb_ot_layout_script_get_language_tags(
                    face,
                    ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                        | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                        | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                        | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                    i,
                    0i32 as libc::c_uint,
                    &mut langCount,
                    langList,
                );
                if index < langCount {
                    rval = *langList.offset(index as isize);
                    break;
                } else {
                    free(langList as *mut libc::c_void);
                    langCount = hb_ot_layout_script_get_language_tags(
                        face,
                        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                            | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                            | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                            | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                        i,
                        0i32 as libc::c_uint,
                        0 as *mut libc::c_uint,
                        0 as *mut hb_tag_t,
                    );
                    langList = xcalloc(
                        langCount as size_t,
                        ::std::mem::size_of::<*mut hb_tag_t>() as _,
                    ) as *mut hb_tag_t;
                    hb_ot_layout_script_get_language_tags(
                        face,
                        ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                            | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                            | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                            | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint,
                        i,
                        0i32 as libc::c_uint,
                        &mut langCount,
                        langList,
                    );
                    if index < langCount {
                        rval = *langList.offset(index as isize);
                        break;
                    } else {
                        free(langList as *mut libc::c_void);
                    }
                }
            }
            i = i.wrapping_add(1)
        }
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countFeatures(
    mut font: XeTeXFont,
    mut script: hb_tag_t,
    mut language: hb_tag_t,
) -> libc::c_uint {
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t =
        hb_font_get_face(XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst));
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        let mut scriptIndex: libc::c_uint = 0;
        let mut langIndex: libc::c_uint = 0i32 as libc::c_uint;
        let mut tableTag: hb_tag_t = if i == 0i32 {
            ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint
        } else {
            ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint
        };
        if hb_ot_layout_table_find_script(face, tableTag, script, &mut scriptIndex) != 0 {
            if hb_ot_layout_script_find_language(
                face,
                tableTag,
                scriptIndex,
                language,
                &mut langIndex,
            ) != 0
                || language == 0i32 as libc::c_uint
            {
                rval = rval.wrapping_add(hb_ot_layout_language_get_feature_tags(
                    face,
                    tableTag,
                    scriptIndex,
                    langIndex,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                ))
            }
        }
        i += 1
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getIndFeature(
    mut font: XeTeXFont,
    mut script: hb_tag_t,
    mut language: hb_tag_t,
    mut index: libc::c_uint,
) -> hb_tag_t {
    use crate::bridge::size_t;
    let mut rval: hb_tag_t = 0i32 as hb_tag_t;
    let mut face: *mut hb_face_t =
        hb_font_get_face(XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst));
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        let mut scriptIndex: libc::c_uint = 0;
        let mut langIndex: libc::c_uint = 0i32 as libc::c_uint;
        let mut tableTag: hb_tag_t = if i == 0i32 {
            ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                | ('S' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                | ('U' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                | 'B' as i32 as uint32_t & 0xffi32 as libc::c_uint
        } else {
            ('G' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 24i32
                | ('P' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 16i32
                | ('O' as i32 as uint32_t & 0xffi32 as libc::c_uint) << 8i32
                | 'S' as i32 as uint32_t & 0xffi32 as libc::c_uint
        };
        if hb_ot_layout_table_find_script(face, tableTag, script, &mut scriptIndex) != 0 {
            if hb_ot_layout_script_find_language(
                face,
                tableTag,
                scriptIndex,
                language,
                &mut langIndex,
            ) != 0
                || language == 0i32 as libc::c_uint
            {
                let mut featCount: libc::c_uint = hb_ot_layout_language_get_feature_tags(
                    face,
                    tableTag,
                    scriptIndex,
                    langIndex,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                );
                let mut featList: *mut hb_tag_t = xcalloc(
                    featCount as size_t,
                    ::std::mem::size_of::<*mut hb_tag_t>() as _,
                ) as *mut hb_tag_t;
                hb_ot_layout_language_get_feature_tags(
                    face,
                    tableTag,
                    scriptIndex,
                    langIndex,
                    0i32 as libc::c_uint,
                    &mut featCount,
                    featList,
                );
                if index < featCount {
                    rval = *featList.offset(index as isize);
                    break;
                } else {
                    index = index.wrapping_sub(featCount)
                }
            }
        }
        i += 1
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countGraphiteFeatures(mut engine: XeTeXLayoutEngine) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        rval = gr_face_n_fref(grFace) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGraphiteFeatureCode(
    mut engine: XeTeXLayoutEngine,
    mut index: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_fref(grFace, index as gr_uint16);
        rval = gr_fref_id(feature)
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countGraphiteFeatureSettings(
    mut engine: XeTeXLayoutEngine,
    mut featureID: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        rval = gr_fref_n_values(feature) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGraphiteFeatureSettingCode(
    mut engine: XeTeXLayoutEngine,
    mut featureID: uint32_t,
    mut index: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        rval = gr_fref_value(feature, index as gr_uint16) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGraphiteFeatureDefaultSetting(
    mut engine: XeTeXLayoutEngine,
    mut featureID: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        let mut featureValues: *mut gr_feature_val = gr_face_featureval_for_lang(
            grFace,
            hb_tag_from_string(
                hb_language_to_string((*engine).language),
                strlen(hb_language_to_string((*engine).language)) as libc::c_int,
            ),
        );
        rval = gr_fref_feature_value(feature, featureValues) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGraphiteFeatureLabel(
    mut engine: XeTeXLayoutEngine,
    mut featureID: uint32_t,
) -> *mut libc::c_char {
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        let mut len: uint32_t = 0i32 as uint32_t;
        let mut langID: uint16_t = 0x409i32 as uint16_t;
        return gr_fref_label(feature, &mut langID, gr_utf8, &mut len) as *mut libc::c_char;
    }
    return 0 as *mut libc::c_char;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGraphiteFeatureSettingLabel(
    mut engine: XeTeXLayoutEngine,
    mut featureID: uint32_t,
    mut settingID: uint32_t,
) -> *mut libc::c_char {
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        let mut i: libc::c_int = 0i32;
        while i < gr_fref_n_values(feature) as libc::c_int {
            if settingID as libc::c_int == gr_fref_value(feature, i as gr_uint16) as libc::c_int {
                let mut len: uint32_t = 0i32 as uint32_t;
                let mut langID: uint16_t = 0x409i32 as uint16_t;
                return gr_fref_value_label(feature, i as gr_uint16, &mut langID, gr_utf8, &mut len)
                    as *mut libc::c_char;
            }
            i += 1
        }
    }
    return 0 as *mut libc::c_char;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn findGraphiteFeature(
    mut engine: XeTeXLayoutEngine,
    mut s: *const libc::c_char,
    mut e: *const libc::c_char,
    mut f: *mut hb_tag_t,
    mut v: *mut libc::c_int,
) -> bool
/* s...e is a "feature=setting" string; look for this in the font */ {
    let mut tmp: libc::c_long = 0;
    *f = 0i32 as hb_tag_t;
    *v = 0i32;
    while *s as libc::c_int == ' ' as i32 || *s as libc::c_int == '\t' as i32 {
        s = s.offset(1)
    }
    let mut cp: *const libc::c_char = s;
    while cp < e && *cp as libc::c_int != '=' as i32 {
        cp = cp.offset(1)
    }
    tmp = findGraphiteFeatureNamed(
        engine,
        s,
        cp.wrapping_offset_from(s) as libc::c_long as libc::c_int,
    );
    *f = tmp as hb_tag_t;
    if tmp == -1i32 as libc::c_long {
        return 0i32 != 0;
    }
    cp = cp.offset(1);
    while cp < e && (*cp as libc::c_int == ' ' as i32 || *cp as libc::c_int == '\t' as i32) {
        cp = cp.offset(1)
    }
    if cp == e {
        /* no setting was specified */
        return 0i32 != 0;
    }
    *v = findGraphiteFeatureSettingNamed(
        engine,
        *f,
        cp,
        e.wrapping_offset_from(cp) as libc::c_long as libc::c_int,
    ) as libc::c_int;
    if *v == -1i32 {
        return 0i32 != 0;
    }
    return 1i32 != 0;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn findGraphiteFeatureNamed(
    mut engine: XeTeXLayoutEngine,
    mut name: *const libc::c_char,
    mut namelength: libc::c_int,
) -> libc::c_long {
    use crate::bridge::size_t;
    let mut rval: libc::c_long = -1i32 as libc::c_long;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut i: libc::c_int = 0i32;
        while i < gr_face_n_fref(grFace) as libc::c_int {
            let mut feature: *const gr_feature_ref = gr_face_fref(grFace, i as gr_uint16);
            let mut len: uint32_t = 0i32 as uint32_t;
            let mut langID: uint16_t = 0x409i32 as uint16_t;
            // the first call is to get the length of the string
            gr_fref_label(feature, &mut langID, gr_utf8, &mut len);
            let mut label: *mut libc::c_char = xmalloc(len as size_t) as *mut libc::c_char;
            label = gr_fref_label(feature, &mut langID, gr_utf8, &mut len) as *mut libc::c_char;
            if strncmp(label, name, namelength as usize) == 0i32 {
                rval = gr_fref_id(feature) as libc::c_long;
                gr_label_destroy(label as *mut libc::c_void);
                break;
            } else {
                gr_label_destroy(label as *mut libc::c_void);
                i += 1
            }
        }
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn findGraphiteFeatureSettingNamed(
    mut engine: XeTeXLayoutEngine,
    mut id: uint32_t,
    mut name: *const libc::c_char,
    mut namelength: libc::c_int,
) -> libc::c_long {
    use crate::bridge::size_t;
    let mut rval: libc::c_long = -1i32 as libc::c_long;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, id);
        let mut i: libc::c_int = 0i32;
        while i < gr_fref_n_values(feature) as libc::c_int {
            let mut len: uint32_t = 0i32 as uint32_t;
            let mut langID: uint16_t = 0x409i32 as uint16_t;
            // the first call is to get the length of the string
            gr_fref_value_label(feature, i as gr_uint16, &mut langID, gr_utf8, &mut len);
            let mut label: *mut libc::c_char = xmalloc(len as size_t) as *mut libc::c_char;
            label = gr_fref_value_label(feature, i as gr_uint16, &mut langID, gr_utf8, &mut len)
                as *mut libc::c_char;
            if strncmp(label, name, namelength as usize) == 0i32 {
                rval = gr_fref_value(feature, i as gr_uint16) as libc::c_long;
                gr_label_destroy(label as *mut libc::c_void);
                break;
            } else {
                gr_label_destroy(label as *mut libc::c_void);
                i += 1
            }
        }
    }
    return rval;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphWidth(
    mut font: XeTeXFont,
    mut gid: uint32_t,
) -> libc::c_float {
    return XeTeXFontInst_getGlyphWidth(font as *mut XeTeXFontInst, gid as GlyphID);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn countGlyphs(mut font: XeTeXFont) -> libc::c_uint {
    return XeTeXFontInst_getNumGlyphs(font as *mut XeTeXFontInst) as libc::c_uint;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFont(mut engine: XeTeXLayoutEngine) -> XeTeXFont {
    return (*engine).font as XeTeXFont;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getExtendFactor(mut engine: XeTeXLayoutEngine) -> libc::c_float {
    return (*engine).extend;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getSlantFactor(mut engine: XeTeXLayoutEngine) -> libc::c_float {
    return (*engine).slant;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getEmboldenFactor(mut engine: XeTeXLayoutEngine) -> libc::c_float {
    return (*engine).embolden;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXLayoutEngine_create() -> *mut XeTeXLayoutEngine_rec {
    return malloc(::std::mem::size_of::<XeTeXLayoutEngine_rec>()) as *mut XeTeXLayoutEngine_rec;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXLayoutEngine_delete(mut engine: *mut XeTeXLayoutEngine_rec) {
    free(engine as *mut libc::c_void);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn createLayoutEngine(
    mut fontRef: PlatformFontRef,
    mut font: XeTeXFont,
    mut script: hb_tag_t,
    mut language: *mut libc::c_char,
    mut features: *mut hb_feature_t,
    mut nFeatures: libc::c_int,
    mut shapers: *mut *mut libc::c_char,
    mut rgbValue: uint32_t,
    mut extend: libc::c_float,
    mut slant: libc::c_float,
    mut embolden: libc::c_float,
) -> XeTeXLayoutEngine {
    let mut result: XeTeXLayoutEngine = XeTeXLayoutEngine_create();
    (*result).fontRef = fontRef;
    (*result).font = font as *mut XeTeXFontInst;
    (*result).script = script;
    (*result).features = features;
    (*result).ShaperList = shapers;
    (*result).shaper = 0 as *mut libc::c_char;
    (*result).nFeatures = nFeatures;
    (*result).rgbValue = rgbValue;
    (*result).extend = extend;
    (*result).slant = slant;
    (*result).embolden = embolden;
    (*result).hbBuffer = hb_buffer_create();
    // For Graphite fonts treat the language as BCP 47 tag, for OpenType we
    // treat it as a OT language tag for backward compatibility with pre-0.9999
    // XeTeX.
    if getReqEngine() as libc::c_int == 'G' as i32 {
        (*result).language = hb_language_from_string(language, -1i32)
    } else {
        (*result).language = hb_ot_tag_to_language(hb_tag_from_string(language, -1i32))
    }
    free(language as *mut libc::c_void);
    return result;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn deleteLayoutEngine(mut engine: XeTeXLayoutEngine) {
    use crate::xetex_font_info::XeTeXFontInst_delete;
    hb_buffer_destroy((*engine).hbBuffer);
    XeTeXFontInst_delete((*engine).font);
    free((*engine).shaper as *mut libc::c_void);
    XeTeXLayoutEngine_delete(engine);
}
unsafe extern "C" fn _decompose_compat(
    mut _ufuncs: *mut hb_unicode_funcs_t,
    mut _u: hb_codepoint_t,
    mut _decomposed: *mut hb_codepoint_t,
    mut _user_data: *mut libc::c_void,
) -> libc::c_uint {
    return 0i32 as libc::c_uint;
}
unsafe extern "C" fn _get_unicode_funcs() -> *mut hb_unicode_funcs_t {
    static mut ufuncs: *mut hb_unicode_funcs_t = ptr::null_mut();
    if ufuncs.is_null() {
        ufuncs = hb_unicode_funcs_create(hb_icu_get_unicode_funcs())
    }
    hb_unicode_funcs_set_decompose_compatibility_func(
        ufuncs,
        Some(
            _decompose_compat
                as unsafe extern "C" fn(
                    _: *mut hb_unicode_funcs_t,
                    _: hb_codepoint_t,
                    _: *mut hb_codepoint_t,
                    _: *mut libc::c_void,
                ) -> libc::c_uint,
        ),
        0 as *mut libc::c_void,
        None,
    );
    return ufuncs;
}
static mut hbUnicodeFuncs: *mut hb_unicode_funcs_t = ptr::null_mut();
#[no_mangle]
pub(crate) unsafe extern "C" fn layoutChars(
    mut engine: XeTeXLayoutEngine,
    mut chars: *mut uint16_t,
    mut offset: int32_t,
    mut count: int32_t,
    mut max: int32_t,
    mut rightToLeft: bool,
) -> libc::c_int {
    use crate::bridge::size_t;
    let mut res: bool = false;
    let mut script: hb_script_t = HB_SCRIPT_INVALID;
    let mut direction: hb_direction_t = HB_DIRECTION_LTR;
    let mut segment_props: hb_segment_properties_t = hb_segment_properties_t {
        direction: HB_DIRECTION_INVALID,
        script: HB_SCRIPT_INVALID,
        language: ptr::null(),
        reserved1: 0 as *mut libc::c_void,
        reserved2: 0 as *mut libc::c_void,
    };
    let mut shape_plan: *mut hb_shape_plan_t = 0 as *mut hb_shape_plan_t;
    let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont((*engine).font);
    let mut hbFace: *mut hb_face_t = hb_font_get_face(hbFont);
    if XeTeXFontInst_getLayoutDirVertical((*engine).font) {
        direction = HB_DIRECTION_TTB
    } else if rightToLeft {
        direction = HB_DIRECTION_RTL
    }
    script = hb_ot_tag_to_script((*engine).script);
    if hbUnicodeFuncs.is_null() {
        hbUnicodeFuncs = _get_unicode_funcs()
    }
    hb_buffer_reset((*engine).hbBuffer);
    hb_buffer_set_unicode_funcs((*engine).hbBuffer, hbUnicodeFuncs);
    hb_buffer_add_utf16(
        (*engine).hbBuffer,
        chars as *const uint16_t,
        max,
        offset as libc::c_uint,
        count,
    );
    hb_buffer_set_direction((*engine).hbBuffer, direction);
    hb_buffer_set_script((*engine).hbBuffer, script);
    hb_buffer_set_language((*engine).hbBuffer, (*engine).language);
    hb_buffer_guess_segment_properties((*engine).hbBuffer);
    hb_buffer_get_segment_properties((*engine).hbBuffer, &mut segment_props);
    if (*engine).ShaperList.is_null() {
        // HarfBuzz gives graphite2 shaper a priority, so that for hybrid
        // Graphite/OpenType fonts, Graphite will be used. However, pre-0.9999
        // XeTeX preferred OpenType over Graphite, so we are doing the same
        // here for sake of backward compatibility. Since "ot" shaper never
        // fails, we set the shaper list to just include it.
        (*engine).ShaperList = xcalloc(
            2i32 as size_t,
            ::std::mem::size_of::<*mut libc::c_char>() as _,
        ) as *mut *mut libc::c_char;
        let ref mut fresh0 = *(*engine).ShaperList.offset(0);
        *fresh0 = b"ot\x00" as *const u8 as *const libc::c_char as *mut libc::c_char;
        let ref mut fresh1 = *(*engine).ShaperList.offset(1);
        *fresh1 = 0 as *mut libc::c_char
    }
    shape_plan = hb_shape_plan_create_cached(
        hbFace,
        &mut segment_props,
        (*engine).features,
        (*engine).nFeatures as libc::c_uint,
        (*engine).ShaperList as *const *const libc::c_char,
    );
    res = hb_shape_plan_execute(
        shape_plan,
        hbFont,
        (*engine).hbBuffer,
        (*engine).features,
        (*engine).nFeatures as libc::c_uint,
    ) != 0;
    if !(*engine).shaper.is_null() {
        free((*engine).shaper as *mut libc::c_void);
        (*engine).shaper = 0 as *mut libc::c_char
    }
    if res {
        (*engine).shaper = strdup(hb_shape_plan_get_shaper(shape_plan));
        hb_buffer_set_content_type((*engine).hbBuffer, HB_BUFFER_CONTENT_TYPE_GLYPHS);
    } else {
        // all selected shapers failed, retrying with default
        // we don't use _cached here as the cached plain will always fail.
        hb_shape_plan_destroy(shape_plan); /* negative is forwards */
        shape_plan = hb_shape_plan_create(
            hbFace,
            &mut segment_props,
            (*engine).features,
            (*engine).nFeatures as libc::c_uint,
            0 as *const *const libc::c_char,
        ); /* negative is upwards */
        res = hb_shape_plan_execute(
            shape_plan,
            hbFont,
            (*engine).hbBuffer,
            (*engine).features,
            (*engine).nFeatures as libc::c_uint,
        ) != 0;
        if res {
            (*engine).shaper = strdup(hb_shape_plan_get_shaper(shape_plan));
            hb_buffer_set_content_type((*engine).hbBuffer, HB_BUFFER_CONTENT_TYPE_GLYPHS);
        } else {
            abort!("all shapers failed");
        }
    }
    hb_shape_plan_destroy(shape_plan);
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    return glyphCount;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphs(
    mut engine: XeTeXLayoutEngine,
    mut glyphs: *mut uint32_t,
) {
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    let mut hbGlyphs: *mut hb_glyph_info_t =
        hb_buffer_get_glyph_infos((*engine).hbBuffer, 0 as *mut libc::c_uint);
    let mut i: libc::c_int = 0i32;
    while i < glyphCount {
        *glyphs.offset(i as isize) = (*hbGlyphs.offset(i as isize)).codepoint;
        i += 1
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphAdvances(
    mut engine: XeTeXLayoutEngine,
    mut advances: *mut libc::c_float,
) {
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    let mut hbPositions: *mut hb_glyph_position_t =
        hb_buffer_get_glyph_positions((*engine).hbBuffer, 0 as *mut libc::c_uint);
    let mut i: libc::c_int = 0i32;
    while i < glyphCount {
        if XeTeXFontInst_getLayoutDirVertical((*engine).font) {
            *advances.offset(i as isize) = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                (*hbPositions.offset(i as isize)).y_advance as libc::c_float,
            )
        } else {
            *advances.offset(i as isize) = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                (*hbPositions.offset(i as isize)).x_advance as libc::c_float,
            )
        }
        i += 1
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphPositions(
    mut engine: XeTeXLayoutEngine,
    mut positions: *mut FloatPoint,
) {
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    let mut hbPositions: *mut hb_glyph_position_t =
        hb_buffer_get_glyph_positions((*engine).hbBuffer, 0 as *mut libc::c_uint);
    let mut x: libc::c_float = 0i32 as libc::c_float;
    let mut y: libc::c_float = 0i32 as libc::c_float;
    if XeTeXFontInst_getLayoutDirVertical((*engine).font) {
        let mut i: libc::c_int = 0i32;
        while i < glyphCount {
            (*positions.offset(i as isize)).x = -XeTeXFontInst_unitsToPoints(
                (*engine).font,
                x + (*hbPositions.offset(i as isize)).y_offset as libc::c_float,
            );
            (*positions.offset(i as isize)).y = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                y - (*hbPositions.offset(i as isize)).x_offset as libc::c_float,
            );
            x += (*hbPositions.offset(i as isize)).y_advance as libc::c_float;
            y += (*hbPositions.offset(i as isize)).x_advance as libc::c_float;
            i += 1
        }
        (*positions.offset(glyphCount as isize)).x =
            -XeTeXFontInst_unitsToPoints((*engine).font, x);
        (*positions.offset(glyphCount as isize)).y = XeTeXFontInst_unitsToPoints((*engine).font, y)
    } else {
        let mut i_0: libc::c_int = 0i32;
        while i_0 < glyphCount {
            (*positions.offset(i_0 as isize)).x = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                x + (*hbPositions.offset(i_0 as isize)).x_offset as libc::c_float,
            );
            (*positions.offset(i_0 as isize)).y = -XeTeXFontInst_unitsToPoints(
                (*engine).font,
                y + (*hbPositions.offset(i_0 as isize)).y_offset as libc::c_float,
            );
            x += (*hbPositions.offset(i_0 as isize)).x_advance as libc::c_float;
            y += (*hbPositions.offset(i_0 as isize)).y_advance as libc::c_float;
            i_0 += 1
        }
        (*positions.offset(glyphCount as isize)).x = XeTeXFontInst_unitsToPoints((*engine).font, x);
        (*positions.offset(glyphCount as isize)).y = -XeTeXFontInst_unitsToPoints((*engine).font, y)
    }
    if (*engine).extend as libc::c_double != 1.0f64 || (*engine).slant as libc::c_double != 0.0f64 {
        let mut i_1: libc::c_int = 0i32;
        while i_1 <= glyphCount {
            (*positions.offset(i_1 as isize)).x = (*positions.offset(i_1 as isize)).x
                * (*engine).extend
                - (*positions.offset(i_1 as isize)).y * (*engine).slant;
            i_1 += 1
        }
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getPointSize(mut engine: XeTeXLayoutEngine) -> libc::c_float {
    return XeTeXFontInst_getPointSize((*engine).font);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getAscentAndDescent(
    mut engine: XeTeXLayoutEngine,
    mut ascent: *mut libc::c_float,
    mut descent: *mut libc::c_float,
) {
    *ascent = XeTeXFontInst_getAscent((*engine).font);
    *descent = XeTeXFontInst_getDescent((*engine).font);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getCapAndXHeight(
    mut engine: XeTeXLayoutEngine,
    mut capheight: *mut libc::c_float,
    mut xheight: *mut libc::c_float,
) {
    *capheight = XeTeXFontInst_getCapHeight((*engine).font);
    *xheight = XeTeXFontInst_getXHeight((*engine).font);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getDefaultDirection(mut engine: XeTeXLayoutEngine) -> libc::c_int {
    let mut script: hb_script_t = hb_buffer_get_script((*engine).hbBuffer);
    if hb_script_get_horizontal_direction(script) as libc::c_uint
        == HB_DIRECTION_RTL as libc::c_int as libc::c_uint
    {
        return 0xffi32;
    } else {
        return 0xfei32;
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getRgbValue(mut engine: XeTeXLayoutEngine) -> uint32_t {
    return (*engine).rgbValue;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphBounds(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
    mut bbox: *mut GlyphBBox,
) {
    XeTeXFontInst_getGlyphBounds((*engine).font, glyphID as GlyphID, bbox);
    if (*engine).extend as libc::c_double != 0.0f64 {
        (*bbox).xMin *= (*engine).extend;
        (*bbox).xMax *= (*engine).extend
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphWidthFromEngine(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
) -> libc::c_float {
    return (*engine).extend * XeTeXFontInst_getGlyphWidth((*engine).font, glyphID as GlyphID);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphHeightDepth(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
    mut height: *mut libc::c_float,
    mut depth: *mut libc::c_float,
) {
    XeTeXFontInst_getGlyphHeightDepth((*engine).font, glyphID as GlyphID, height, depth);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphSidebearings(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
    mut lsb: *mut libc::c_float,
    mut rsb: *mut libc::c_float,
) {
    XeTeXFontInst_getGlyphSidebearings((*engine).font, glyphID as GlyphID, lsb, rsb);
    if (*engine).extend as libc::c_double != 0.0f64 {
        *lsb *= (*engine).extend;
        *rsb *= (*engine).extend
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphItalCorr(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
) -> libc::c_float {
    return (*engine).extend * XeTeXFontInst_getGlyphItalCorr((*engine).font, glyphID as GlyphID);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn mapCharToGlyph(
    mut engine: XeTeXLayoutEngine,
    mut charCode: uint32_t,
) -> uint32_t {
    return XeTeXFontInst_mapCharToGlyph((*engine).font, charCode as UChar32) as uint32_t;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getFontCharRange(
    mut engine: XeTeXLayoutEngine,
    mut reqFirst: libc::c_int,
) -> libc::c_int {
    if reqFirst != 0 {
        return XeTeXFontInst_getFirstCharCode((*engine).font);
    } else {
        return XeTeXFontInst_getLastCharCode((*engine).font);
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn getGlyphName(
    mut font: XeTeXFont,
    mut gid: uint16_t,
    mut len: *mut libc::c_int,
) -> *const libc::c_char {
    return XeTeXFontInst_getGlyphName(font as *mut XeTeXFontInst, gid, len);
}
#[no_mangle]
pub(crate) unsafe extern "C" fn mapGlyphToIndex(
    mut engine: XeTeXLayoutEngine,
    mut glyphName: *const libc::c_char,
) -> libc::c_int {
    return XeTeXFontInst_mapGlyphToIndex((*engine).font, glyphName) as libc::c_int;
}
static mut grSegment: *mut gr_segment = 0 as *mut gr_segment;
static mut grPrevSlot: *const gr_slot = 0 as *const gr_slot;
static mut grTextLen: libc::c_int = 0;
#[no_mangle]
pub(crate) unsafe extern "C" fn initGraphiteBreaking(
    mut engine: XeTeXLayoutEngine,
    mut txtPtr: *const uint16_t,
    mut txtLen: libc::c_int,
) -> bool {
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    let mut grFont: *mut gr_font =
        hb_graphite2_font_get_gr_font(XeTeXFontInst_getHbFont((*engine).font));
    if !grFace.is_null() && !grFont.is_null() {
        if !grSegment.is_null() {
            gr_seg_destroy(grSegment);
            grSegment = 0 as *mut gr_segment;
            grPrevSlot = 0 as *const gr_slot;
        }
        let mut grFeatureValues: *mut gr_feature_val = gr_face_featureval_for_lang(
            grFace,
            hb_tag_from_string(
                hb_language_to_string((*engine).language),
                strlen(hb_language_to_string((*engine).language)) as libc::c_int,
            ),
        );
        let mut nFeatures: libc::c_int = (*engine).nFeatures;
        let mut features: *mut hb_feature_t = (*engine).features;
        loop {
            let fresh2 = nFeatures;
            nFeatures = nFeatures - 1;
            if !(fresh2 != 0) {
                break;
            }
            let mut fref: *const gr_feature_ref = gr_face_find_fref(grFace, (*features).tag);
            if !fref.is_null() {
                gr_fref_set_feature_value(fref, (*features).value as gr_uint16, grFeatureValues);
            }
            features = features.offset(1)
        }
        grSegment = gr_make_seg(
            grFont,
            grFace,
            (*engine).script,
            grFeatureValues,
            gr_utf16,
            txtPtr as *const libc::c_void,
            txtLen as size_t,
            0i32,
        );
        grPrevSlot = gr_seg_first_slot(grSegment);
        grTextLen = txtLen;
        return 1i32 != 0;
    }
    return 0i32 != 0;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn findNextGraphiteBreak() -> libc::c_int {
    let mut ret: libc::c_int = -1i32;
    if !grSegment.is_null() {
        if !grPrevSlot.is_null() && grPrevSlot != gr_seg_last_slot(grSegment) {
            let mut s: *const gr_slot = gr_slot_next_in_segment(grPrevSlot);
            while !s.is_null() {
                let mut ci: *const gr_char_info = 0 as *const gr_char_info;
                let mut bw: libc::c_int = 0;
                ci = gr_seg_cinfo(grSegment, gr_slot_index(s));
                bw = gr_cinfo_break_weight(ci);
                if bw < gr_breakNone as libc::c_int && bw >= gr_breakBeforeWord as libc::c_int {
                    grPrevSlot = s;
                    ret = gr_cinfo_base(ci) as libc::c_int
                } else if bw > gr_breakNone as libc::c_int && bw <= gr_breakWord as libc::c_int {
                    grPrevSlot = gr_slot_next_in_segment(s);
                    ret = gr_cinfo_base(ci).wrapping_add(1) as libc::c_int
                }
                if ret != -1i32 {
                    break;
                }
                s = gr_slot_next_in_segment(s)
            }
            if ret == -1i32 {
                grPrevSlot = gr_seg_last_slot(grSegment);
                ret = grTextLen
            }
        }
    }
    return ret;
}
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew
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
/* graphite interface functions... */
#[no_mangle]
pub(crate) unsafe extern "C" fn usingGraphite(mut engine: XeTeXLayoutEngine) -> bool {
    if !(*engine).shaper.is_null()
        && strcmp(
            b"graphite2\x00" as *const u8 as *const libc::c_char,
            (*engine).shaper,
        ) == 0i32
    {
        return 1i32 != 0;
    } else {
        return 0i32 != 0;
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn usingOpenType(mut engine: XeTeXLayoutEngine) -> bool {
    if (*engine).shaper.is_null()
        || strcmp(
            b"ot\x00" as *const u8 as *const libc::c_char,
            (*engine).shaper,
        ) == 0i32
    {
        return 1i32 != 0;
    } else {
        return 0i32 != 0;
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn isOpenTypeMathFont(mut engine: XeTeXLayoutEngine) -> bool {
    return hb_ot_math_has_data(hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font))) != 0;
}
