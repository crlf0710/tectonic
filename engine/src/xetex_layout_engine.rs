#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

// Fc* functions are from fontconfig.

use crate::core_memory::{xcalloc, xmalloc};
use crate::stub_icu as icu;
use harfbuzz_sys::*;

use crate::text_layout_engine::{
    Fixed, FixedPoint, FloatPoint, LayoutRequest, NodeLayout, TextLayout,
};
use crate::xetex_font_info::XeTeXFontInst;
#[cfg(target_os = "macos")]
use crate::xetex_font_info::XeTeXFontInst_Mac_create;
use crate::xetex_font_manager::{
    PlatformFontRef, ShaperRequest, XeTeXFontMgr, XeTeXFontMgrFamily, XeTeXFontMgrFont,
};
use std::ffi::{CStr, CString};
use std::ptr;

// https://www.freetype.org/freetype2/docs/reference/ft2-basic_types.html#ft_make_tag
pub const fn ft_make_tag(buf: &[u8; 4]) -> u32 {
    (buf[0] as u32) << 24 | (buf[1] as u32) << 16 | (buf[2] as u32) << 8 | buf[3] as u32
}

extern "C" {
    pub type gr_face;
    pub type gr_font;
    pub type gr_feature_ref;
    pub type gr_feature_val;
    pub type gr_char_info;
    pub type gr_segment;
    pub type gr_slot;
    /* ******************************************************************/
    /* Glyph bounding box cache to speed up \XeTeXuseglyphmetrics mode */
    /* ******************************************************************/
    // key is combined value representing (font_id << 16) + glyph
    // value is glyph bounding box in TeX points
    #[no_mangle]
    fn tan(_: f64) -> f64;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
    #[no_mangle]
    fn strncmp(_: *const libc::c_char, _: *const libc::c_char, _: libc::c_ulong) -> libc::c_int;
    #[no_mangle]
    fn strdup(_: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    /* The internal, C/C++ interface: */
    /* tectonic/core-memory.h: basic dynamic memory helpers
    Copyright 2016-2018 the Tectonic Project
    Licensed under the MIT License.
    */
    #[no_mangle]
    fn xstrdup(s: *const libc::c_char) -> *mut libc::c_char;
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
    pub fn gr_label_destroy(label: *mut libc::c_void);
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
}

use crate::xetex_ext::{D2Fix, Fix2D};

use crate::xetex_font_manager::{
    XeTeXFontMgr_Destroy, XeTeXFontMgr_GetFontManager, XeTeXFontMgr_Terminate,
    XeTeXFontMgr_findFont, XeTeXFontMgr_getDesignSize, XeTeXFontMgr_getFullName,
};

// use crate::xetex_font_info::{XeTeXFontInst_unitsToPoints, XeTeXFontInst_mapGlyphToIndex, XeTeXFontInst_getGlyphName, XeTeXFontInst_getLastCharCode, XeTeXFontInst_getFirstCharCode, XeTeXFontInst_delete, XeTeXFontInst_create, XeTeXFontInst_setLayoutDirVertical, XeTeXFontInst_mapCharToGlyph, XeTeXFontInst_getFontTable, XeTeXFontInst_getGlyphSidebearings, XeTeXFontInst_getGlyphHeightDepth};

use crate::xetex_font_info::*;

pub mod collection_types {
    use super::size_t;
    use core::ptr::NonNull;
    use std::collections::{BTreeMap, LinkedList, VecDeque};
    use std::ffi::CStr;
    use std::ffi::CString;

    pub type CppStdString = CString;
    pub type CppStdListOfString = VecDeque<CString>;
    pub type CppStdMap<K, V> = BTreeMap<K, V>;

    pub fn CppStdString_create() -> *mut CppStdString {
        Box::into_raw(Box::new(CString::default()))
    }

    pub unsafe fn CppStdString_delete(self_0: *mut CppStdString) {
        let _: Box<CppStdString> = Box::from_raw(self_0);
    }
    pub unsafe fn CppStdString_length(self_0: *const CppStdString) -> libc::size_t {
        self_0.as_ref().unwrap().to_bytes().len() as _
    }
    pub unsafe fn CppStdString_cstr(self_0: *const CppStdString) -> *const libc::c_char {
        let v = self_0.as_ref().unwrap();
        v.as_ptr()
    }

    pub fn CppStdListOfString_create() -> *mut CppStdListOfString {
        Box::into_raw(Box::new(CppStdListOfString::default()))
    }

    pub unsafe fn CppStdListOfString_delete(self_0: *mut CppStdListOfString) {
        let _: Box<CppStdListOfString> = Box::from_raw(self_0);
    }

    pub fn CppStdMap_create<K: Ord, V>() -> *mut CppStdMap<K, V> {
        Box::into_raw(Box::new(CppStdMap::default()))
    }

    pub unsafe fn CppStdMap_put<K: Ord, V>(self_0: *mut CppStdMap<K, V>, key: K, val: V) {
        (*self_0).insert(key, val);
    }

    pub unsafe fn CppStdMap_put_with_string_key<V>(
        self_0: *mut CppStdMap<CString, V>,
        key: *const libc::c_char,
        val: V,
    ) {
        use std::ffi::CStr;
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

    pub unsafe fn CppStdMap_delete<K: Ord, V>(self_0: *mut CppStdMap<K, V>) {
        let _: Box<CppStdMap<K, V>> = Box::from_raw(self_0);
    }

    pub unsafe fn CppStdString_last(self_0: *const CppStdString) -> libc::c_char {
        let val = &*self_0;
        *val.to_bytes().last().expect("must not be empty") as libc::c_char
    }
    pub unsafe fn CppStdString_clone(self_0: *const CppStdString) -> *mut CppStdString {
        let v: Box<CppStdString> = Box::new((*self_0).clone());
        Box::into_raw(v)
    }

    pub unsafe fn CppStdString_append_const_char_ptr(
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

    pub unsafe fn CppStdString_assign_from_const_char_ptr(
        self_0: *mut CppStdString,
        val: *const libc::c_char,
    ) {
        let o: &mut CppStdString = &mut *self_0;
        *o = CStr::from_ptr(val).to_owned();
    }

    pub unsafe fn CppStdString_assign_n_chars(
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

pub type size_t = usize;
pub type int8_t = i8;
pub type int16_t = i16;
pub type int32_t = i32;
pub type uint8_t = u8;
pub type uint16_t = u16;
pub type uint32_t = u32;

pub type UChar32 = int32_t;
#[cfg(not(target_os = "macos"))]
pub type FcChar8 = libc::c_uchar;
#[cfg(not(target_os = "macos"))]
use crate::xetex_font_manager::imp::{FcPattern, FcResult};

pub type hb_unicode_decompose_compatibility_func_t = Option<
    unsafe fn(
        _: *mut hb_unicode_funcs_t,
        _: hb_codepoint_t,
        _: *mut hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> libc::c_uint,
>;

pub struct ImmutableCStringList {
    // Option to make freeing manually easier
    inner: Option<CStringListBuilder>,
    pointer: *const *const libc::c_char,
}

impl ImmutableCStringList {
    // Warning: You must ensure that you do not drop this HarfBuzzShaperList
    // while the resulting pointer here is still in use. Store this struct somewhere, or keep it in
    // scope.
    #[inline]
    pub fn as_ptr(&self) -> *const *const libc::c_char {
        self.pointer
    }
}

/// A convenient way to build a null-terminated list of null-terminated C pointer strings.
///
/// Motivation: HarfBuzz shaper lists are pretty tiny, but storing each shaper id as a CString and
/// also having a *const *const c_char available for HarfBuzz to use is otherwise pretty tricky.
/// The original C code simply never freed these things as it was too hard.
#[derive(Clone)]
pub struct CStringListBuilder {
    inner: Vec<CString>,
    pointers: Option<Vec<*const libc::c_char>>,
    dirty: bool,
}

impl CStringListBuilder {
    pub fn freeze(mut self) -> ImmutableCStringList {
        let mut pointers = Vec::with_capacity(self.inner.len() + 1);
        for cstring in &self.inner {
            pointers.push(cstring.as_ptr());
        }
        // HarfBuzz expects a null at the end.
        pointers.push(ptr::null());
        let p = pointers.as_ptr();
        self.pointers = Some(pointers);
        ImmutableCStringList {
            pointer: p,
            inner: Some(self),
        }
    }
    pub fn new() -> Self {
        CStringListBuilder {
            inner: Vec::new(),
            pointers: None,
            dirty: true,
        }
    }
    // Helps you select the default "ot" shaper if you haven't found any shapers to add.
    pub fn none_if_empty(self) -> Option<Self> {
        if self.inner.is_empty() {
            return None;
        }
        Some(self)
    }
    pub fn push_non_null_terminated(&mut self, shaper: impl Into<Vec<u8>>) {
        self.dirty = true;
        self.inner.push(
            CString::new(shaper)
                .expect("push_non_null_terminated called with a null terminated C string"),
        );
    }
    pub fn push_cstr(&mut self, cstr: &CStr) {
        self.dirty = true;
        self.inner.push(cstr.to_owned())
    }
}

pub type OTTag = uint32_t;
pub type GlyphID = uint16_t;

#[repr(C)]
pub struct XeTeXLayoutEngine_rec {
    pub font: *mut XeTeXFontInst,
    pub fontRef: PlatformFontRef,
    pub script: hb_tag_t,
    pub language: hb_language_t,
    pub features: *mut hb_feature_t,
    shaper_list: ImmutableCStringList,
    pub shaper: *mut libc::c_char,
    pub nFeatures: libc::c_int,
    pub rgbValue: uint32_t,
    pub extend: f32,
    pub slant: f32,
    pub embolden: f32,
    pub hbBuffer: *mut hb_buffer_t,
}

pub type XeTeXLayoutEngine = *mut XeTeXLayoutEngine_rec;

impl TextLayout for XeTeXLayoutEngine_rec {
    // AAT casualties

    // /// getFontRef
    // fn platform_font_ref(&self) -> PlatformFontRef {
    //     self.fontRef
    // }

    // /// getFontInst
    // fn font_instance(&self) -> *mut XeTeXFontInst {
    //     self.font
    // }

    pub unsafe fn get_flags(&self, font_number: u32) -> i32 {
        if *font_flags.offset(font_number as isize) as i32 & 0x2i32 != 0i32 {
            0x100i32
        }
    }

    // getGlyphWidth
    unsafe fn glyph_width(&self, gid: u32) -> f64 {
        XeTeXFontInst_getGlyphWidth(self.font, gid as GlyphID) as f64
    }

    /// getFontFilename
    unsafe fn font_filename(&self, index: &mut u32) -> *mut libc::c_char {
        unsafe { xstrdup(XeTeXFontInst_getFilename(self.font, index)) }
    }

    /// getExtendFactor
    unsafe fn extend_factor(&self) -> f64 {
        self.extend as f64
    }

    unsafe fn get_font_metrics(&self, ascent: &mut Fixed, descent: &mut Fixed, x_ht: &mut Fixed, cap_ht: &mut Fixed, slant: &mut Fixed) {
        crate::xetex_ext::ot_get_font_metrics(self.attributes, ascent, descent, x_ht, cap_ht, slant);
    }

    /// ot_font_get, aat_font_get
    unsafe fn poorly_named_getter(&self, what: i32) {
        let mut fontInst = self.font;
        match what {
            1 => return countGlyphs(fontInst) as i32,
            8 => {
                /* ie Graphite features */
                return countGraphiteFeatures(self) as i32;
            }
            16 => return countScripts(fontInst) as i32,
            _ => {}
        }
        0i32
    }

    /// ot_font_get_1, aat_font_get_1
    unsafe fn poorly_named_getter_1(&self, what: i32) {
        let mut fontInst = self.font;
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

    /// ot_font_get_2, aat_font_get_2
    unsafe fn poorly_named_getter_2(
        &self,
        mut what: i32,
        mut param1: i32,
        mut param2: i32,
    ) -> i32 {
        let mut fontInst = self.font;
        match what {
            20 => return getIndLanguage(fontInst, param1 as hb_tag_t, param2 as u32) as i32,
            18 => return countFeatures(fontInst, param1 as hb_tag_t, param2 as hb_tag_t) as i32,
            13 => {
                /* for graphite fonts */
                return getGraphiteFeatureSettingCode(self, param1 as u32, param2 as u32) as i32;
            }
            15 => {
                return (getGraphiteFeatureDefaultSetting(self, param1 as u32) == param2 as u32)
                    as i32
            }
            _ => {}
        } /* to guarantee enough space in the buffer */
        0i32
    }

    unsafe fn ot_font_get_3(
        &self,
        mut what: i32,
        mut param1: i32,
        mut param2: i32,
        mut param3: i32,
    ) -> i32 {
        let mut fontInst = self.font;
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


    /// getPointSize
    unsafe fn point_size(&self) -> f64 {
        unsafe { XeTeXFontInst_getPointSize(self.font) as f64 }
    }
    /// getAscentAndDescent
    unsafe fn ascent_and_descent(&self, ascent: &mut f32, descent: &mut f32) {
        unsafe {
            *ascent = XeTeXFontInst_getAscent(self.font);
            *descent = XeTeXFontInst_getDescent(self.font);
        }
    }

    /// gr_print_font_name
    unsafe fn print_font_name(&self, c: i32, arg1: i32, arg2: i32) {
        if self.usingGraphite() {
            let mut name: *mut i8 = 0 as *mut i8;
            match what {
                8 => name = getGraphiteFeatureLabel(self, param1 as u32),
                9 => name = getGraphiteFeatureSettingLabel(self, param1 as u32, param2 as u32),
                _ => {}
            }
            if !name.is_null() {
                print_c_string(name);
                gr_label_destroy(name as *mut libc::c_void);
            };
        }
        // Not sure why non-graphite font names aren't printed
        // Originally in xetex0.c.
    }

    /// getGlyphName
    /// Only used for debugging. Should be a String/CStr then!
    unsafe fn glyph_name(&self, gid: u16, len: &mut libc::c_int) -> *const libc::c_char {
        getGlyphName(self.font, gid, len)
    }

    /// getCapAndXHeight
    unsafe fn cap_and_x_height(&self, capheight: &mut f32, xheight: &mut f32) {
        unsafe {
            *capheight = XeTeXFontInst_getCapHeight(self.font);
            *xheight = XeTeXFontInst_getXHeight(self.font);
        }
    }
    /// getEmboldenFactor
    unsafe fn embolden_factor(&self) -> f32 {
        self.embolden
    }

    /// getRgbValue
    unsafe fn rgb_value(&self) -> u32 {
        self.rgbValue
    }

    unsafe fn slant_factor(&self) -> f64 {
        self.slant as f64
    }

    /// getGlyphBounds (had out param)
    unsafe fn glyph_bbox(&self, glyphID: u32) -> Option<GlyphBBox> {
        let font_info = &*self.font;

        // TODO: xetex_font_info uses u16 (why??????), but glyph IDs should be u32
        font_info.get_glyph_bounds(glyphID as u16).map(|mut bbox| {
            if self.extend != 0.0f32 {
                bbox.xMin *= self.extend;
                bbox.xMax *= self.extend;
            }
            bbox
        })
    }

    unsafe fn getGlyphWidthFromEngine(&self, glyphID: u32) -> f64 {
        (self.extend * XeTeXFontInst_getGlyphWidth(self.font, glyphID as GlyphID)) as f64
    }

    /// getGlyphHeightDepth (had out params height, depth)
    unsafe fn glyph_height_depth(&self, glyphID: u32) -> Option<(f32, f32)> {
        let mut height: f32 = 0.;
        let mut depth: f32 = 0.;
        // TODO: None if glyph not found
        unsafe {
            XeTeXFontInst_getGlyphHeightDepth(
                self.font,
                glyphID as GlyphID,
                &mut height,
                &mut depth,
            );
        }
        Some((height, depth))
    }

    /// getGlyphSidebearings (had out params lsb, rsb)
    unsafe fn glyph_sidebearings(&self, mut glyphID: u32) -> Option<(f32, f32)> {
        let mut lsb = 0.;
        let mut rsb = 0.;
        // TODO: None if glyph not found
        unsafe {
            XeTeXFontInst_getGlyphSidebearings(self.font, glyphID as GlyphID, &mut lsb, &mut rsb);
        }
        if self.extend as f64 != 0.0f64 {
            lsb *= self.extend;
            rsb *= self.extend;
        };
        Some((lsb, rsb))
    }

    /// getGlyphItalCorr
    unsafe fn glyph_ital_correction(&self, glyphID: u32) -> Option<f64> {
        // XXX: return none if glyph not found
        Some(
            self.extend as f64
                * XeTeXFontInst_getGlyphItalCorr(self.font, glyphID as GlyphID) as f64,
        )
    }

    /// mapCharToGlyph
    unsafe fn map_codepoint_to_glyph(&self, codepoint: u32) -> u32 {
        XeTeXFontInst_mapCharToGlyph(self.font, codepoint as UChar32) as u32
    }

    /// getFontCharRange
    /// Another candidate for using XeTeXFontInst directly
    unsafe fn font_char_range(&self, mut reqFirst: libc::c_int) -> libc::c_int {
        if reqFirst != 0 {
            return XeTeXFontInst_getFirstCharCode(self.font);
        } else {
            return XeTeXFontInst_getLastCharCode(self.font);
        }
    }

    /// mapGlyphToIndex
    unsafe fn map_glyph_to_index(&self, mut glyphName: *const libc::c_char) -> i32 {
        XeTeXFontInst_mapGlyphToIndex(self.font, glyphName) as i32
    }

    unsafe fn usingGraphite(&self) -> bool {
        if !self.shaper.is_null()
            && strcmp(
                b"graphite2\x00" as *const u8 as *const libc::c_char,
                self.shaper,
            ) == 0i32
        {
            true
        } else {
            false
        }
    }

    unsafe fn initGraphiteBreaking(&mut self, txtPtr: *const uint16_t, txtLen: i32) -> bool {
        initGraphiteBreaking(self, txtPtr, txtLen)
    }

    unsafe fn usingOpenType(&self) -> bool {
        if self.shaper.is_null()
            || strcmp(b"ot\x00" as *const u8 as *const libc::c_char, self.shaper) == 0i32
        {
            true
        } else {
            false
        }
    }

    unsafe fn isOpenTypeMathFont(&self) -> bool {
        hb_ot_math_has_data(hb_font_get_face(XeTeXFontInst_getHbFont(self.font))) != 0
    }

    unsafe fn layout_text(&mut self, request: LayoutRequest) -> NodeLayout {
        use crate::bridge::size_t;

        // XXX use slices
        let txtPtr = request.text.as_ptr();
        let txtLen = request.text.len() as i32;

        let mut locations: *mut FixedPoint = ptr::null_mut();
        let mut glyphIDs: *mut u16 = ptr::null_mut();
        let mut glyphAdvances: *mut Fixed = ptr::null_mut();
        let mut totalGlyphCount: i32 = 0i32;
        /* need to find direction runs within the text, and call layoutChars separately for each */
        let mut dir: icu::UBiDiDirection = icu::UBIDI_LTR;
        let mut glyph_info: *mut FixedPoint = ptr::null_mut();
        static mut positions: *mut FloatPoint = ptr::null_mut();
        static mut advances: *mut f32 = ptr::null_mut();
        static mut glyphs: *mut u32 = ptr::null_mut();
        let mut pBiDi: *mut icu::UBiDi = icu::ubidi_open();
        let mut errorCode: icu::UErrorCode = icu::U_ZERO_ERROR;
        icu::ubidi_setPara(
            pBiDi,
            txtPtr,
            txtLen,
            getDefaultDirection(self) as icu::UBiDiLevel,
            0 as *mut icu::UBiDiLevel,
            &mut errorCode,
        );
        dir = icu::ubidi_getDirection(pBiDi);

        let mut layout = if dir as u32 == icu::UBIDI_MIXED as i32 as u32 {
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
                totalGlyphCount += self.layoutChars(
                    txtPtr,
                    logicalStart,
                    length,
                    txtLen,
                    dir as u32 == icu::UBIDI_RTL as i32 as u32,
                );
                runIndex += 1
            }
            if totalGlyphCount > 0i32 {
                let mut x: f64 = 0.;
                let mut y: f64 = 0.;
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t) as *mut FixedPoint;
                locations = glyph_info;
                glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(
                    totalGlyphCount as size_t,
                    ::std::mem::size_of::<Fixed>() as u64,
                ) as *mut Fixed;
                totalGlyphCount = 0i32;
                y = 0.0f64;
                x = y;
                runIndex = 0i32;
                while runIndex < nRuns {
                    let mut nGlyphs: i32 = 0;
                    dir = icu::ubidi_getVisualRun(pBiDi, runIndex, &mut logicalStart, &mut length);
                    nGlyphs = self.layoutChars(
                        txtPtr,
                        logicalStart,
                        length,
                        txtLen,
                        dir as u32 == icu::UBIDI_RTL as i32 as u32,
                    );
                    glyphs =
                        xcalloc(nGlyphs as size_t, ::std::mem::size_of::<u32>() as u64) as *mut u32;
                    positions = xcalloc(
                        (nGlyphs + 1i32) as size_t,
                        ::std::mem::size_of::<FloatPoint>() as u64,
                    ) as *mut FloatPoint;
                    advances =
                        xcalloc(nGlyphs as size_t, ::std::mem::size_of::<f32>() as u64) as *mut f32;
                    getGlyphs(self, glyphs);
                    getGlyphAdvances(self, advances);
                    getGlyphPositions(self, positions);
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
            NodeLayout {
                lsDelta: None,
                width: D2Fix(width),
                total_glyph_count: totalGlyphCount as u16,
                glyph_info,
            }
        } else {
            let mut width_0: f64 = 0i32 as f64;
            totalGlyphCount = self.layoutChars(
                txtPtr,
                0i32,
                txtLen,
                txtLen,
                dir as u32 == icu::UBIDI_RTL as i32 as u32,
            );
            glyphs = xcalloc(
                totalGlyphCount as size_t,
                ::std::mem::size_of::<u32>() as u64,
            ) as *mut u32;
            positions = xcalloc(
                (totalGlyphCount + 1i32) as size_t,
                ::std::mem::size_of::<FloatPoint>() as u64,
            ) as *mut FloatPoint;
            advances = xcalloc(
                totalGlyphCount as size_t,
                ::std::mem::size_of::<f32>() as u64,
            ) as *mut f32;
            getGlyphs(self, glyphs);
            getGlyphAdvances(self, advances);
            getGlyphPositions(self, positions);
            if totalGlyphCount > 0i32 {
                let mut i_0: i32 = 0;
                glyph_info = xcalloc(totalGlyphCount as size_t, 10i32 as size_t) as *mut FixedPoint;
                locations = glyph_info;
                glyphIDs = locations.offset(totalGlyphCount as isize) as *mut u16;
                glyphAdvances = xcalloc(
                    totalGlyphCount as size_t,
                    ::std::mem::size_of::<Fixed>() as u64,
                ) as *mut Fixed;
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
            free(glyphs as *mut libc::c_void);
            free(positions as *mut libc::c_void);
            free(advances as *mut libc::c_void);
            NodeLayout {
                lsDelta: None,
                width: D2Fix(width_0),
                total_glyph_count: totalGlyphCount as u16,
                glyph_info,
            }
        };

        icu::ubidi_close(pBiDi);

        if request.letter_space_unit != 0i32 {
            let mut lsDelta: Fixed = 0i32;
            let mut lsUnit: Fixed = request.letter_space_unit;
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
            if lsDelta != 0i32 {
                lsDelta -= lsUnit;
                layout.lsDelta = Some(lsDelta);
            }
        }
        free(glyphAdvances as *mut libc::c_void);
        layout
    }
}

pub type gr_uint16 = libc::c_ushort;
pub type gr_int16 = libc::c_short;
pub type gr_uint32 = libc::c_uint;
pub type gr_encform = libc::c_uint;
pub const gr_utf32: gr_encform = 4;
pub const gr_utf16: gr_encform = 2;
pub const gr_utf8: gr_encform = 1;
pub type gr_break_weight = libc::c_int;
pub const gr_breakBeforeClip: gr_break_weight = -40;
pub const gr_breakBeforeLetter: gr_break_weight = -30;
pub const gr_breakBeforeIntra: gr_break_weight = -20;
pub const gr_breakBeforeWord: gr_break_weight = -15;
pub const gr_breakBeforeWhitespace: gr_break_weight = -10;
pub const gr_breakClip: gr_break_weight = 40;
pub const gr_breakLetter: gr_break_weight = 30;
pub const gr_breakIntra: gr_break_weight = 20;
pub const gr_breakWord: gr_break_weight = 15;
pub const gr_breakWhitespace: gr_break_weight = 10;
pub const gr_breakNone: gr_break_weight = 0;

pub type ProtrusionFactor = CppStdMap<GlyphId, libc::c_int>;

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
pub struct GlyphId {
    pub fontNum: libc::c_int,
    pub code: libc::c_uint,
}
#[inline]
unsafe fn XeTeXFontInst_getDescent(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_descent;
}
#[inline]
unsafe fn XeTeXFontInst_getLayoutDirVertical(mut self_0: *const XeTeXFontInst) -> bool {
    return (*self_0).m_vertical;
}
#[inline]
unsafe fn XeTeXFontInst_getPointSize(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_pointSize;
}
#[inline]
unsafe fn XeTeXFontInst_getAscent(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_ascent;
}
#[inline]
unsafe fn XeTeXFontInst_getCapHeight(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_capHeight;
}
#[inline]
unsafe fn XeTeXFontInst_getXHeight(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_xHeight;
}
#[inline]
unsafe fn XeTeXFontInst_getItalicAngle(mut self_0: *const XeTeXFontInst) -> f32 {
    return (*self_0).m_italicAngle;
}
#[inline]
unsafe fn XeTeXFontInst_getFilename(
    mut self_0: *const XeTeXFontInst,
    mut index: *mut uint32_t,
) -> *const libc::c_char {
    *index = (*self_0).m_index;
    if let Some(f) = (*self_0).m_filename.as_ref() {
        f.as_ptr()
    } else {
        ptr::null()
    }
}
#[no_mangle]
pub unsafe fn getGlyphBBoxCache() -> *mut CppStdMap<u32, GlyphBBox> {
    static mut cache: *mut CppStdMap<u32, GlyphBBox> =
        0 as *const CppStdMap<u32, GlyphBBox> as *mut CppStdMap<u32, GlyphBBox>;
    if cache.is_null() {
        cache = CppStdMap_create()
    }
    return cache;
}
#[no_mangle]
pub unsafe fn getCachedGlyphBBox(
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
pub unsafe fn cacheGlyphBBox(
    mut fontID: uint16_t,
    mut glyphID: uint16_t,
    mut bbox: *const GlyphBBox,
) {
    let mut sGlyphBoxes: *mut CppStdMap<u32, GlyphBBox> = getGlyphBBoxCache();
    let mut key: uint32_t = ((fontID as uint32_t) << 16i32).wrapping_add(glyphID as libc::c_uint);
    CppStdMap_put(sGlyphBoxes, key, *bbox);
}
#[inline]
unsafe fn GlyphId_create(mut fontNum: libc::c_int, mut code: libc::c_uint) -> GlyphId {
    let mut id: GlyphId = GlyphId {
        fontNum: 0,
        code: 0,
    };
    id.fontNum = fontNum;
    id.code = code;
    return id;
}
#[no_mangle]
pub unsafe fn getProtrusionFactor(mut side: libc::c_int) -> *mut ProtrusionFactor {
    static mut leftProt: *mut ProtrusionFactor =
        0 as *const ProtrusionFactor as *mut ProtrusionFactor;
    static mut rightProt: *mut ProtrusionFactor =
        0 as *const ProtrusionFactor as *mut ProtrusionFactor;
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
pub unsafe fn set_cp_code(
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
pub unsafe fn get_cp_code(
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
pub unsafe fn terminate_font_manager() {
    XeTeXFontMgr_Terminate();
}
#[no_mangle]
pub unsafe fn destroy_font_manager() {
    XeTeXFontMgr_Destroy();
}

#[no_mangle]
pub unsafe fn createFont(mut fontRef: PlatformFontRef, mut pointSize: Fixed) -> *mut XeTeXFontInst {
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
            CStr::from_ptr(pathname as *const libc::c_char),
            index,
            Fix2D(pointSize) as f32,
            &mut status,
        );
    }
    #[cfg(target_os = "macos")]
    {
        let mac_font = XeTeXFontInst_Mac_create(fontRef, Fix2D(pointSize) as f32, &mut status);
        // XXX: this only works because free() uses addresses only, and super_ is the first field.
        font = &mut (*mac_font).super_;
    }
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return ptr::null_mut();
    }
    font
}

#[no_mangle]
pub unsafe fn createFontFromFile(
    filename: &CStr,
    index: libc::c_int,
    pointSize: Fixed,
) -> *mut XeTeXFontInst {
    let mut status: libc::c_int = 0i32;
    let mut font: *mut XeTeXFontInst =
        XeTeXFontInst_create(filename, index, Fix2D(pointSize) as f32, &mut status);
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return ptr::null_mut();
    }
    return font;
}

#[no_mangle]
pub unsafe fn setFontLayoutDir(mut font: *mut XeTeXFontInst, mut vertical: libc::c_int) {
    XeTeXFontInst_setLayoutDirVertical(font, vertical != 0i32);
}

#[no_mangle]
pub unsafe fn findFontByName(
    mut name: &CStr,
    mut var: Option<&mut String>,
    mut size: f64,
    shaperRequest: &mut Option<ShaperRequest>,
) -> PlatformFontRef {
    return XeTeXFontMgr_findFont(
        XeTeXFontMgr_GetFontManager(),
        name,
        var,
        size,
        shaperRequest,
    );
}

#[no_mangle]
pub unsafe fn getFullName(mut fontRef: PlatformFontRef) -> *const libc::c_char {
    return XeTeXFontMgr_getFullName(XeTeXFontMgr_GetFontManager(), fontRef);
}
#[no_mangle]
pub unsafe fn getDesignSize(mut font: *mut XeTeXFontInst) -> f64 {
    return XeTeXFontMgr_getDesignSize(XeTeXFontMgr_GetFontManager(), font);
}
#[no_mangle]
pub unsafe fn getFontFilename(
    mut engine: XeTeXLayoutEngine,
    mut index: *mut uint32_t,
) -> *mut libc::c_char {
    xstrdup(XeTeXFontInst_getFilename((*engine).font, index))
}
#[no_mangle]
pub unsafe fn getFontRef(mut engine: XeTeXLayoutEngine) -> PlatformFontRef {
    return (*engine).fontRef;
}

#[no_mangle]
pub unsafe fn deleteFont(mut font: *mut XeTeXFontInst) {
    XeTeXFontInst_delete(font);
}

#[no_mangle]
pub unsafe fn getFontTablePtr(
    mut font: *mut XeTeXFontInst,
    mut tableTag: uint32_t,
) -> *mut libc::c_void {
    return XeTeXFontInst_getFontTable(font, tableTag);
}

#[no_mangle]
pub unsafe fn getSlant(mut font: *mut XeTeXFontInst) -> Fixed {
    let mut italAngle: f32 = XeTeXFontInst_getItalicAngle(font);
    let radians = -italAngle as f64 * std::f64::consts::PI / 180.0f64;
    return D2Fix(radians.tan());
}

unsafe fn getLargerScriptListTable(
    mut font: *mut XeTeXFontInst,
    mut scriptList: *mut *mut hb_tag_t,
) -> libc::c_uint {
    use crate::bridge::size_t;
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(font));
    let mut scriptListSub: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptListPos: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCountSub: libc::c_uint = hb_ot_layout_table_get_script_tags(
        face,
        ft_make_tag(b"GSUB"),
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
        ft_make_tag(b"GSUB"),
        0i32 as libc::c_uint,
        &mut scriptCountSub,
        scriptListSub,
    );
    let mut scriptCountPos: libc::c_uint = hb_ot_layout_table_get_script_tags(
        face,
        ft_make_tag(b"GPOS"),
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
        ft_make_tag(b"GSUB"),
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
pub unsafe fn countScripts(mut font: *mut XeTeXFontInst) -> libc::c_uint {
    return getLargerScriptListTable(font, 0 as *mut *mut hb_tag_t);
}
#[no_mangle]
pub unsafe fn getIndScript(mut font: *mut XeTeXFontInst, mut index: libc::c_uint) -> hb_tag_t {
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
pub unsafe fn countLanguages(mut font: *mut XeTeXFontInst, mut script: hb_tag_t) -> libc::c_uint {
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(font));
    let mut scriptList: *mut hb_tag_t = 0 as *mut hb_tag_t;
    let mut scriptCount: libc::c_uint = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        let mut i: libc::c_uint = 0i32 as libc::c_uint;
        while i < scriptCount {
            if *scriptList.offset(i as isize) == script {
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    ft_make_tag(b"GSUB"),
                    i,
                    0i32 as libc::c_uint,
                    0 as *mut libc::c_uint,
                    0 as *mut hb_tag_t,
                ));
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    ft_make_tag(b"GPOS"),
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
pub unsafe fn getIndLanguage(
    mut font: *mut XeTeXFontInst,
    mut script: hb_tag_t,
    mut index: libc::c_uint,
) -> hb_tag_t {
    use crate::bridge::size_t;
    let mut rval: hb_tag_t = 0i32 as hb_tag_t;
    let mut face: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(font));
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
                    ft_make_tag(b"GSUB"),
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
                    ft_make_tag(b"GSUB"),
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
                        ft_make_tag(b"GPOS"),
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
                        ft_make_tag(b"GPOS"),
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
pub unsafe fn countFeatures(
    mut font: *mut XeTeXFontInst,
    mut script: hb_tag_t,
    mut language: hb_tag_t,
) -> libc::c_uint {
    let mut rval: libc::c_uint = 0i32 as libc::c_uint;
    let mut face: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(font));
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        let mut scriptIndex: libc::c_uint = 0;
        let mut langIndex: libc::c_uint = 0i32 as libc::c_uint;
        let mut tableTag: hb_tag_t = if i == 0i32 {
            ft_make_tag(b"GSUB")
        } else {
            ft_make_tag(b"GPOS")
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
pub unsafe fn getIndFeature(
    mut font: *mut XeTeXFontInst,
    mut script: hb_tag_t,
    mut language: hb_tag_t,
    mut index: libc::c_uint,
) -> hb_tag_t {
    use crate::bridge::size_t;
    let mut rval: hb_tag_t = 0i32 as hb_tag_t;
    let mut face: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(font));
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        let mut scriptIndex: libc::c_uint = 0;
        let mut langIndex: libc::c_uint = 0i32 as libc::c_uint;
        let mut tableTag: hb_tag_t = if i == 0i32 {
            ft_make_tag(b"GSUB")
        } else {
            ft_make_tag(b"GPOS")
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
pub unsafe fn countGraphiteFeatures(engine: &XeTeXLayoutEngine_rec) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(engine.font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        rval = gr_face_n_fref(grFace) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub unsafe fn getGraphiteFeatureCode(
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
pub unsafe fn countGraphiteFeatureSettings(
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
pub unsafe fn getGraphiteFeatureSettingCode(
    engine: &XeTeXLayoutEngine_rec,
    mut featureID: uint32_t,
    mut index: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(engine.font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        rval = gr_fref_value(feature, index as gr_uint16) as uint32_t
    }
    return rval;
}

#[no_mangle]
pub unsafe fn getGraphiteFeatureDefaultSetting(
    engine: &XeTeXLayoutEngine_rec,
    mut featureID: uint32_t,
) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont(engine.font));
    let mut grFace: *mut gr_face = hb_graphite2_face_get_gr_face(hbFace);
    if !grFace.is_null() {
        let mut feature: *const gr_feature_ref = gr_face_find_fref(grFace, featureID);
        let mut featureValues: *mut gr_feature_val = gr_face_featureval_for_lang(
            grFace,
            hb_tag_from_string(
                hb_language_to_string(engine.language),
                strlen(hb_language_to_string(engine.language)) as libc::c_int,
            ),
        );
        rval = gr_fref_feature_value(feature, featureValues) as uint32_t
    }
    return rval;
}
#[no_mangle]
pub unsafe fn getGraphiteFeatureLabel(
    engine: &XeTeXLayoutEngine_rec,
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
pub unsafe fn getGraphiteFeatureSettingLabel(
    engine: &XeTeXLayoutEngine_rec,
    featureID: uint32_t,
    settingID: uint32_t,
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
pub unsafe fn findGraphiteFeature(
    engine: &mut XeTeXLayoutEngine_rec,
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
pub unsafe fn findGraphiteFeatureNamed(
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
            if strncmp(label, name, namelength as libc::c_ulong) == 0i32 {
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
pub unsafe fn findGraphiteFeatureSettingNamed(
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
            if strncmp(label, name, namelength as libc::c_ulong) == 0i32 {
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
pub unsafe fn getGlyphWidth(mut font: *mut XeTeXFontInst, mut gid: uint32_t) -> f32 {
    return XeTeXFontInst_getGlyphWidth(font, gid as GlyphID);
}
#[no_mangle]
pub unsafe fn countGlyphs(mut font: *mut XeTeXFontInst) -> libc::c_uint {
    return XeTeXFontInst_getNumGlyphs(font) as libc::c_uint;
}
#[no_mangle]
pub unsafe fn getFontInst(mut engine: XeTeXLayoutEngine) -> *mut XeTeXFontInst {
    return (*engine).font;
}
#[no_mangle]
pub unsafe fn getExtendFactor(mut engine: XeTeXLayoutEngine) -> f32 {
    return (*engine).extend;
}
#[no_mangle]
pub unsafe fn getSlantFactor(mut engine: XeTeXLayoutEngine) -> f32 {
    return (*engine).slant;
}
#[no_mangle]
pub unsafe fn getEmboldenFactor(mut engine: XeTeXLayoutEngine) -> f32 {
    return (*engine).embolden;
}

#[no_mangle]
pub unsafe fn createLayoutEngine(
    fontRef: PlatformFontRef,
    font: *mut XeTeXFontInst,
    script: hb_tag_t,
    language_s: *mut libc::c_char,
    features: *mut hb_feature_t,
    nFeatures: libc::c_int,
    shapers: Option<CStringListBuilder>,
    rgbValue: uint32_t,
    extend: f32,
    slant: f32,
    embolden: f32,
    shaperRequest: Option<ShaperRequest>,
) -> XeTeXLayoutEngine_rec {
    // For Graphite fonts treat the language as BCP 47 tag, for OpenType we
    // treat it as a OT language tag for backward compatibility with pre-0.9999
    // XeTeX.
    let language = if shaperRequest == Some(ShaperRequest::Graphite) {
        hb_language_from_string(language_s, -1i32)
    } else {
        hb_ot_tag_to_language(hb_tag_from_string(language_s, -1i32))
    };
    free(language_s as *mut libc::c_void);

    // HarfBuzz gives graphite2 shaper a priority, so that for hybrid
    // Graphite/OpenType fonts, Graphite will be used. However, pre-0.9999
    // XeTeX preferred OpenType over Graphite, so we are doing the same
    // here for sake of backward compatibility. Since "ot" shaper never
    // fails, we set the shaper list to just include it.
    let shaper_list = shapers
        .unwrap_or_else(|| {
            let mut default_ot = CStringListBuilder::new();
            default_ot.push_non_null_terminated(&b"ot"[..]);
            default_ot
        })
        .freeze();

    XeTeXLayoutEngine_rec {
        font,
        fontRef,
        script,
        language,
        features,
        shaper_list,
        shaper: ptr::null_mut(),
        nFeatures,
        rgbValue,
        extend,
        slant,
        embolden,
        hbBuffer: hb_buffer_create(),
    }
}

impl Drop for XeTeXLayoutEngine_rec {
    fn drop(&mut self) {
        unsafe {
            hb_buffer_destroy(self.hbBuffer);
            XeTeXFontInst_delete(self.font);
            if !self.shaper.is_null() {
                free(self.shaper as *mut libc::c_void);
            }
        }
    }
}

#[no_mangle]
pub unsafe fn deleteLayoutEngine(mut engine: XeTeXLayoutEngine) {
}

unsafe fn _decompose_compat(
    mut ufuncs: *mut hb_unicode_funcs_t,
    mut u: hb_codepoint_t,
    mut decomposed: *mut hb_codepoint_t,
    mut user_data: *mut libc::c_void,
) -> libc::c_uint {
    return 0i32 as libc::c_uint;
}

unsafe fn _get_unicode_funcs() -> *mut hb_unicode_funcs_t {
    static mut ufuncs: *mut hb_unicode_funcs_t =
        0 as *const hb_unicode_funcs_t as *mut hb_unicode_funcs_t;
    if ufuncs.is_null() {
        ufuncs = hb_unicode_funcs_create(hb_icu_get_unicode_funcs())
    }
    hb_unicode_funcs_set_decompose_compatibility_func(
        ufuncs,
        Some(
            _decompose_compat
                as unsafe fn(
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
static mut hbUnicodeFuncs: *mut hb_unicode_funcs_t =
    0 as *const hb_unicode_funcs_t as *mut hb_unicode_funcs_t;

impl XeTeXLayoutEngine_rec {
    pub unsafe fn layoutChars(
        &mut self,
        chars: *const u16,
        offset: i32,
        count: i32,
        max: i32,
        rightToLeft: bool,
    ) -> libc::c_int {
        use crate::bridge::size_t;
        let mut res: bool = false;
        let mut script: hb_script_t = HB_SCRIPT_INVALID;
        let mut direction: hb_direction_t = HB_DIRECTION_LTR;
        let mut segment_props: hb_segment_properties_t = hb_segment_properties_t {
            direction: HB_DIRECTION_INVALID,
            script: HB_SCRIPT_INVALID,
            language: 0 as *const hb_language_impl_t,
            reserved1: 0 as *mut libc::c_void,
            reserved2: 0 as *mut libc::c_void,
        };
        let mut shape_plan: *mut hb_shape_plan_t = 0 as *mut hb_shape_plan_t;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(self.font);
        let mut hbFace: *mut hb_face_t = hb_font_get_face(hbFont);
        if XeTeXFontInst_getLayoutDirVertical(self.font) {
            direction = HB_DIRECTION_TTB
        } else if rightToLeft {
            direction = HB_DIRECTION_RTL
        }
        script = hb_ot_tag_to_script(self.script);
        if hbUnicodeFuncs.is_null() {
            hbUnicodeFuncs = _get_unicode_funcs()
        }
        hb_buffer_reset(self.hbBuffer);
        hb_buffer_set_unicode_funcs(self.hbBuffer, hbUnicodeFuncs);
        hb_buffer_add_utf16(self.hbBuffer, chars, max, offset as libc::c_uint, count);
        hb_buffer_set_direction(self.hbBuffer, direction);
        hb_buffer_set_script(self.hbBuffer, script);
        hb_buffer_set_language(self.hbBuffer, self.language);
        hb_buffer_guess_segment_properties(self.hbBuffer);
        hb_buffer_get_segment_properties(self.hbBuffer, &mut segment_props);
        shape_plan = hb_shape_plan_create_cached(
            hbFace,
            &mut segment_props,
            self.features,
            self.nFeatures as libc::c_uint,
            self.shaper_list.as_ptr(),
        );
        res = hb_shape_plan_execute(
            shape_plan,
            hbFont,
            self.hbBuffer,
            self.features,
            self.nFeatures as libc::c_uint,
        ) != 0;
        if !self.shaper.is_null() {
            free(self.shaper as *mut libc::c_void);
            self.shaper = 0 as *mut libc::c_char
        }
        if res {
            self.shaper = strdup(hb_shape_plan_get_shaper(shape_plan));
            hb_buffer_set_content_type(self.hbBuffer, HB_BUFFER_CONTENT_TYPE_GLYPHS);
        } else {
            // all selected shapers failed, retrying with default
            // we don't use _cached here as the cached plain will always fail.
            hb_shape_plan_destroy(shape_plan); /* negative is forwards */
            shape_plan = hb_shape_plan_create(
                hbFace,
                &mut segment_props,
                self.features,
                self.nFeatures as libc::c_uint,
                0 as *const *const libc::c_char,
            ); /* negative is upwards */
            res = hb_shape_plan_execute(
                shape_plan,
                hbFont,
                self.hbBuffer,
                self.features,
                self.nFeatures as libc::c_uint,
            ) != 0;
            if res {
                self.shaper = strdup(hb_shape_plan_get_shaper(shape_plan));
                hb_buffer_set_content_type(self.hbBuffer, HB_BUFFER_CONTENT_TYPE_GLYPHS);
            } else {
                panic!("all shapers failed");
            }
        }
        hb_shape_plan_destroy(shape_plan);
        let mut glyphCount: libc::c_int = hb_buffer_get_length(self.hbBuffer) as libc::c_int;
        return glyphCount;
    }
}

#[no_mangle]
pub unsafe fn getGlyphs(engine: &XeTeXLayoutEngine_rec, mut glyphs: *mut uint32_t) {
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
pub unsafe fn getGlyphAdvances(engine: &XeTeXLayoutEngine_rec, mut advances: *mut f32) {
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    let mut hbPositions: *mut hb_glyph_position_t =
        hb_buffer_get_glyph_positions((*engine).hbBuffer, 0 as *mut libc::c_uint);
    let mut i: libc::c_int = 0i32;
    while i < glyphCount {
        if XeTeXFontInst_getLayoutDirVertical((*engine).font) {
            *advances.offset(i as isize) = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                (*hbPositions.offset(i as isize)).y_advance as f32,
            )
        } else {
            *advances.offset(i as isize) = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                (*hbPositions.offset(i as isize)).x_advance as f32,
            )
        }
        i += 1
    }
}
#[no_mangle]
pub unsafe fn getGlyphPositions(engine: &XeTeXLayoutEngine_rec, mut positions: *mut FloatPoint) {
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    let mut hbPositions: *mut hb_glyph_position_t =
        hb_buffer_get_glyph_positions((*engine).hbBuffer, 0 as *mut libc::c_uint);
    let mut x: f32 = 0i32 as f32;
    let mut y: f32 = 0i32 as f32;
    if XeTeXFontInst_getLayoutDirVertical((*engine).font) {
        let mut i: libc::c_int = 0i32;
        while i < glyphCount {
            (*positions.offset(i as isize)).x = -XeTeXFontInst_unitsToPoints(
                (*engine).font,
                x + (*hbPositions.offset(i as isize)).y_offset as f32,
            );
            (*positions.offset(i as isize)).y = XeTeXFontInst_unitsToPoints(
                (*engine).font,
                y - (*hbPositions.offset(i as isize)).x_offset as f32,
            );
            x += (*hbPositions.offset(i as isize)).y_advance as f32;
            y += (*hbPositions.offset(i as isize)).x_advance as f32;
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
                x + (*hbPositions.offset(i_0 as isize)).x_offset as f32,
            );
            (*positions.offset(i_0 as isize)).y = -XeTeXFontInst_unitsToPoints(
                (*engine).font,
                y + (*hbPositions.offset(i_0 as isize)).y_offset as f32,
            );
            x += (*hbPositions.offset(i_0 as isize)).x_advance as f32;
            y += (*hbPositions.offset(i_0 as isize)).y_advance as f32;
            i_0 += 1
        }
        (*positions.offset(glyphCount as isize)).x = XeTeXFontInst_unitsToPoints((*engine).font, x);
        (*positions.offset(glyphCount as isize)).y = -XeTeXFontInst_unitsToPoints((*engine).font, y)
    }
    if (*engine).extend as f64 != 1.0f64 || (*engine).slant as f64 != 0.0f64 {
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
pub unsafe fn getPointSize(mut engine: XeTeXLayoutEngine) -> f32 {
    return XeTeXFontInst_getPointSize((*engine).font);
}
#[no_mangle]
pub unsafe fn getAscentAndDescent(
    mut engine: XeTeXLayoutEngine,
    mut ascent: *mut f32,
    mut descent: *mut f32,
) {
    *ascent = XeTeXFontInst_getAscent((*engine).font);
    *descent = XeTeXFontInst_getDescent((*engine).font);
}
#[no_mangle]
pub unsafe fn getCapAndXHeight(
    mut engine: XeTeXLayoutEngine,
    mut capheight: *mut f32,
    mut xheight: *mut f32,
) {
    *capheight = XeTeXFontInst_getCapHeight((*engine).font);
    *xheight = XeTeXFontInst_getXHeight((*engine).font);
}
#[no_mangle]
pub unsafe fn getDefaultDirection(engine: XeTeXLayoutEngine) -> libc::c_int {
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
pub unsafe fn getRgbValue(mut engine: XeTeXLayoutEngine) -> uint32_t {
    return (*engine).rgbValue;
}
#[no_mangle]
pub unsafe fn getGlyphBounds(engine: XeTeXLayoutEngine, glyphID: uint32_t, bbox: *mut GlyphBBox) {
    let font_info = &*((*engine).font);

    // TODO: xetex_font_info uses u16 (why??????), but glyph IDs should be u32
    if let Some(bb) = font_info.get_glyph_bounds(glyphID as u16) {
        ptr::write(bbox, bb);
    }

    if (*engine).extend as f64 != 0.0f64 {
        (*bbox).xMin *= (*engine).extend;
        (*bbox).xMax *= (*engine).extend
    };
}
#[no_mangle]
pub unsafe fn getGlyphWidthFromEngine(
    engine: *mut XeTeXLayoutEngine_rec,
    mut glyphID: uint32_t,
) -> f32 {
    return (*engine).extend * XeTeXFontInst_getGlyphWidth((*engine).font, glyphID as GlyphID);
}
#[no_mangle]
pub unsafe fn getGlyphHeightDepth(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
    mut height: *mut f32,
    mut depth: *mut f32,
) {
    XeTeXFontInst_getGlyphHeightDepth((*engine).font, glyphID as GlyphID, height, depth);
}
#[no_mangle]
pub unsafe fn getGlyphSidebearings(
    mut engine: XeTeXLayoutEngine,
    mut glyphID: uint32_t,
    mut lsb: &mut f32,
    mut rsb: &mut f32,
) {
    XeTeXFontInst_getGlyphSidebearings((*engine).font, glyphID as GlyphID, lsb, rsb);
    if (*engine).extend as f64 != 0.0f64 {
        *lsb *= (*engine).extend;
        *rsb *= (*engine).extend
    };
}
#[no_mangle]
pub unsafe fn getGlyphItalCorr(mut engine: XeTeXLayoutEngine, mut glyphID: uint32_t) -> f32 {
    return (*engine).extend * XeTeXFontInst_getGlyphItalCorr((*engine).font, glyphID as GlyphID);
}
#[no_mangle]
pub unsafe fn mapCharToGlyph(mut engine: XeTeXLayoutEngine, mut charCode: uint32_t) -> uint32_t {
    return XeTeXFontInst_mapCharToGlyph((*engine).font, charCode as UChar32) as uint32_t;
}
#[no_mangle]
pub unsafe fn getFontCharRange(
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
pub unsafe fn getGlyphName(
    mut font: *mut XeTeXFontInst,
    mut gid: uint16_t,
    mut len: &mut libc::c_int,
) -> *const libc::c_char {
    return XeTeXFontInst_getGlyphName(font, gid, len);
}
#[no_mangle]
pub unsafe fn mapGlyphToIndex(
    mut engine: XeTeXLayoutEngine,
    mut glyphName: *const libc::c_char,
) -> libc::c_int {
    return XeTeXFontInst_mapGlyphToIndex((*engine).font, glyphName) as libc::c_int;
}
static mut grSegment: *mut gr_segment = 0 as *const gr_segment as *mut gr_segment;
static mut grPrevSlot: *const gr_slot = 0 as *const gr_slot;
static mut grTextLen: libc::c_int = 0;
#[no_mangle]
pub unsafe fn initGraphiteBreaking(
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
            grPrevSlot = 0 as *const gr_slot
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
pub unsafe fn findNextGraphiteBreak() -> libc::c_int {
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
pub unsafe fn usingGraphite(mut engine: XeTeXLayoutEngine) -> bool {
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
pub unsafe fn usingOpenType(mut engine: XeTeXLayoutEngine) -> bool {
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
pub unsafe fn isOpenTypeMathFont(mut engine: XeTeXLayoutEngine) -> bool {
    return hb_ot_math_has_data(hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font))) != 0;
}
