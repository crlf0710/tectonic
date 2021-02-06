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
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_font_manager::FindFont;
use once_cell::sync::Lazy;
use std::collections::BTreeMap;

#[cfg(not(target_os = "macos"))]
use crate::c_pointer_to_str;

use crate::xetex_consts::Side;
use harfbuzz_sys::{hb_feature_t, hb_ot_math_glyph_part_t, hb_tag_t};
use std::ffi::CStr;

#[cfg(target_os = "macos")]
use crate::cf_prelude::CTFontDescriptorRef;

#[cfg(not(target_os = "macos"))]
pub(crate) type PlatformFontRef = *mut FcPattern;
#[cfg(target_os = "macos")]
pub(crate) type PlatformFontRef = CTFontDescriptorRef;

#[derive(Clone)]
pub(crate) struct GlyphAssembly {
    pub(crate) parts: Vec<hb_ot_math_glyph_part_t>,
}

pub(crate) use crate::xetex_font_info::GlyphBBox;
use crate::xetex_scaledmath::Scaled;

#[derive(Copy, Clone)]
#[cfg_attr(not(target_os = "macos"), repr(C))]
#[cfg_attr(target_os = "macos", repr(C, packed(2)))]
pub(crate) struct FixedPoint {
    pub(crate) x: Scaled,
    pub(crate) y: Scaled,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct FloatPoint {
    pub(crate) x: f32,
    pub(crate) y: f32,
}

use crate::core_memory::xcalloc;
use harfbuzz_sys::*;
use std::ptr;

#[path = "xetex_opentype_math.rs"]
mod opentype_math;

pub(crate) use opentype_math::*;

use libc::{free, strlen};

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_face {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_font {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_feature_ref {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_feature_val {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_char_info {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_segment {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct gr_slot {
    _unused: [u8; 0],
}

extern "C" {
    #[cfg(not(target_os = "macos"))]
    fn FcPatternGetInteger(p: *const FcPattern, object: *const i8, n: i32, i: *mut i32)
        -> FcResult;
    #[cfg(not(target_os = "macos"))]
    fn FcPatternGetString(
        p: *const FcPattern,
        object: *const i8,
        n: i32,
        s: *mut *mut u8,
    ) -> FcResult;
    fn hb_unicode_funcs_set_decompose_compatibility_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_decompose_compatibility_func_t,
        user_data: *mut libc::c_void,
        destroy: hb_destroy_func_t,
    );
    fn hb_ot_layout_script_find_language(
        face: *mut hb_face_t,
        table_tag: hb_tag_t,
        script_index: u32,
        language_tag: hb_tag_t,
        language_index: *mut u32,
    ) -> hb_bool_t;

    fn gr_face_featureval_for_lang(pFace: *const gr_face, langname: u32) -> *mut gr_feature_val;
    fn gr_face_find_fref(pFace: *const gr_face, featId: u32) -> *const gr_feature_ref;
    fn gr_face_n_fref(pFace: *const gr_face) -> u16;
    fn gr_face_fref(pFace: *const gr_face, i: u16) -> *const gr_feature_ref;
    fn gr_fref_feature_value(
        pfeatureref: *const gr_feature_ref,
        feats: *const gr_feature_val,
    ) -> u16;
    fn gr_fref_set_feature_value(
        pfeatureref: *const gr_feature_ref,
        val: u16,
        pDest: *mut gr_feature_val,
    ) -> i32;
    fn gr_fref_id(pfeatureref: *const gr_feature_ref) -> u32;
    fn gr_fref_n_values(pfeatureref: *const gr_feature_ref) -> u16;
    fn gr_fref_value(pfeatureref: *const gr_feature_ref, settingno: u16) -> i16;
    fn gr_fref_label(
        pfeatureref: *const gr_feature_ref,
        langId: *mut u16,
        utf: gr_encform,
        length: *mut u32,
    ) -> *mut libc::c_void;
    fn gr_fref_value_label(
        pfeatureref: *const gr_feature_ref,
        settingno: u16,
        langId: *mut u16,
        utf: gr_encform,
        length: *mut u32,
    ) -> *mut libc::c_void;
    pub(crate) fn gr_label_destroy(label: *mut libc::c_void);
    fn gr_cinfo_break_weight(p: *const gr_char_info) -> i32;
    fn gr_cinfo_base(p: *const gr_char_info) -> size_t;
    fn gr_make_seg(
        font: *const gr_font,
        face: *const gr_face,
        script: u32,
        pFeats: *const gr_feature_val,
        enc: gr_encform,
        pStart: *const libc::c_void,
        nChars: size_t,
        dir: i32,
    ) -> *mut gr_segment;
    fn gr_seg_destroy(p: *mut gr_segment);
    fn gr_seg_cinfo(pSeg: *const gr_segment, index: u32) -> *const gr_char_info;
    fn gr_seg_first_slot(pSeg: *mut gr_segment) -> *const gr_slot;
    fn gr_seg_last_slot(pSeg: *mut gr_segment) -> *const gr_slot;
    fn gr_slot_next_in_segment(p: *const gr_slot) -> *const gr_slot;
    fn gr_slot_index(p: *const gr_slot) -> u32;
    fn hb_graphite2_face_get_gr_face(face: *mut hb_face_t) -> *mut gr_face;
    fn hb_graphite2_font_get_gr_font(font: *mut hb_font_t) -> *mut gr_font;
    fn hb_icu_get_unicode_funcs() -> *mut hb_unicode_funcs_t;
}

use crate::xetex_font_manager::{
    XeTeXFontMgr_Destroy, XeTeXFontMgr_GetFontManager, XeTeXFontMgr_Terminate,
    XeTeXFontMgr_getReqEngine, XeTeXFontMgr_setReqEngine,
};

use crate::xetex_font_info::XeTeXFontInst;

pub(crate) type size_t = usize;

#[cfg(not(target_os = "macos"))]
use crate::xetex_font_manager::imp::{FcPattern, FcResult};

pub(crate) type hb_unicode_decompose_compatibility_func_t = Option<
    unsafe extern "C" fn(
        _: *mut hb_unicode_funcs_t,
        _: hb_codepoint_t,
        _: *mut hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> u32,
>;

pub(crate) type GlyphID = u16;

pub(crate) struct XeTeXLayoutEngine {
    pub(crate) font: Box<XeTeXFont>,
    pub(crate) fontRef: PlatformFontRef,
    pub(crate) script: hb_tag_t,
    pub(crate) language: hb_language_t,
    pub(crate) features: Vec<hb_feature_t>,
    pub(crate) shaper_list: ShaperList,
    pub(crate) shaper: String,
    pub(crate) rgbValue: u32,
    pub(crate) extend: f32,
    pub(crate) slant: f32,
    pub(crate) embolden: f32,
    pub(crate) hbBuffer: hb::HbBuffer,
}

pub(crate) struct ShaperList {
    pub(crate) list: *mut *mut i8,
    pub(crate) to_free: bool,
}

impl Drop for ShaperList {
    fn drop(&mut self) {
        if self.to_free {
            unsafe {
                free(self.list as *mut libc::c_void);
            }
        }
    }
}

pub(crate) type gr_encform = u32;
//pub(crate) const gr_utf32: gr_encform = 4;
pub(crate) const gr_utf16: gr_encform = 2;
pub(crate) const gr_utf8: gr_encform = 1;
pub(crate) type gr_break_weight = i32;
//pub(crate) const gr_breakBeforeClip: gr_break_weight = -40;
//pub(crate) const gr_breakBeforeLetter: gr_break_weight = -30;
//pub(crate) const gr_breakBeforeIntra: gr_break_weight = -20;
pub(crate) const gr_breakBeforeWord: gr_break_weight = -15;
//pub(crate) const gr_breakBeforeWhitespace: gr_break_weight = -10;
//pub(crate) const gr_breakClip: gr_break_weight = 40;
//pub(crate) const gr_breakLetter: gr_break_weight = 30;
//pub(crate) const gr_breakIntra: gr_break_weight = 20;
pub(crate) const gr_breakWord: gr_break_weight = 15;
//pub(crate) const gr_breakWhitespace: gr_break_weight = 10;
pub(crate) const gr_breakNone: gr_break_weight = 0;

pub(crate) type ProtrusionFactor = BTreeMap<GlyphId, i32>;

/* The following code used to be in a file called "hz.cpp" and there's no
 * particular reason for it to be here, but it was a tiny file with a weird
 * name so I wanted to get rid of it. The functions are invoked from the C
 * code. */
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub(crate) struct GlyphId {
    pub(crate) fontNum: i32,
    pub(crate) code: u32,
}

impl XeTeXFontInst {
    #[inline]
    fn get_descent(&self) -> f32 {
        self.m_descent
    }
    #[inline]
    fn get_layout_dir_vertical(&self) -> bool {
        self.m_vertical
    }
    #[inline]
    fn get_point_size(&self) -> f32 {
        self.m_pointSize
    }
    #[inline]
    fn get_ascent(&self) -> f32 {
        self.m_ascent
    }
    #[inline]
    fn get_cap_height(&self) -> f32 {
        self.m_capHeight
    }
    #[inline]
    fn get_x_height(&self) -> f32 {
        self.m_xHeight
    }
    #[inline]
    fn get_italic_angle(&self) -> f32 {
        self.m_italicAngle
    }
    #[inline]
    unsafe fn get_filename(&self, index: &mut u32) -> &str {
        *index = self.m_index;
        &self.m_filename
    }
}

pub(crate) unsafe fn getGlyphBBoxCache() -> &'static mut BTreeMap<u32, GlyphBBox> {
    static mut cache: Lazy<BTreeMap<u32, GlyphBBox>> = Lazy::new(BTreeMap::new);
    &mut cache
}
pub(crate) unsafe fn getCachedGlyphBBox(fontID: u16, glyphID: u16) -> Option<GlyphBBox> {
    let sGlyphBoxes = getGlyphBBoxCache();
    let key: u32 = ((fontID as u32) << 16).wrapping_add(glyphID as u32);
    (*sGlyphBoxes).get(&key).cloned()
}
pub(crate) unsafe fn cacheGlyphBBox(fontID: u16, glyphID: u16, bbox: *const GlyphBBox) {
    let sGlyphBoxes = getGlyphBBoxCache();
    let key: u32 = ((fontID as u32) << 16).wrapping_add(glyphID as u32);
    sGlyphBoxes.insert(key, *bbox);
}
#[inline]
fn GlyphId_create(fontNum: usize, code: u32) -> GlyphId {
    GlyphId {
        fontNum: fontNum as i32,
        code,
    }
}
pub(crate) unsafe fn getProtrusionFactor(side: Side) -> &'static mut ProtrusionFactor {
    static mut leftProt: Lazy<ProtrusionFactor> = Lazy::new(ProtrusionFactor::new);
    static mut rightProt: Lazy<ProtrusionFactor> = Lazy::new(ProtrusionFactor::new);
    match side {
        Side::Left => {
            &mut leftProt // we should not reach here
        }
        Side::Right => &mut rightProt,
    }
}
pub(crate) unsafe fn set_cp_code(fontNum: usize, code: u32, side: Side, value: i32) {
    let id = GlyphId_create(fontNum, code);
    let container = getProtrusionFactor(side);
    container.insert(id, value);
}
pub(crate) unsafe fn get_cp_code(fontNum: usize, code: u32, side: Side) -> i32 {
    let id = GlyphId_create(fontNum, code);
    let container = getProtrusionFactor(side);
    container.get(&id).cloned().unwrap_or(0)
}
/* ******************************************************************/
pub(crate) unsafe fn terminate_font_manager() {
    XeTeXFontMgr_Terminate();
}
pub(crate) unsafe fn destroy_font_manager() {
    XeTeXFontMgr_Destroy();
}
#[cfg(not(target_os = "macos"))]
pub(crate) type XeTeXFont = XeTeXFontInst;
#[cfg(target_os = "macos")]
pub(crate) type XeTeXFont = crate::xetex_font_info::imp::XeTeXFontInst_Mac;

pub(crate) unsafe fn createFont(
    fontRef: PlatformFontRef,
    pointSize: Scaled,
) -> Option<Box<XeTeXFont>> {
    let mut status = false;
    let font;
    #[cfg(not(target_os = "macos"))]
    {
        let mut pathname = ptr::null_mut();
        FcPatternGetString(
            fontRef as *const FcPattern,
            b"file\x00".as_ptr() as *const i8,
            0,
            &mut pathname,
        );
        let mut index: i32 = 0;
        FcPatternGetInteger(
            fontRef as *const FcPattern,
            b"index\x00".as_ptr() as *const i8,
            0,
            &mut index,
        );
        font = XeTeXFont::create(
            c_pointer_to_str(pathname as *const i8),
            index,
            pointSize.into(),
            &mut status,
        );
    }
    #[cfg(target_os = "macos")]
    {
        font = XeTeXFont::create(fontRef, pointSize, into(), &mut status);
    }
    if status {
        None
    } else {
        Some(font)
    }
}
pub(crate) unsafe fn createFontFromFile(
    filename: &str,
    index: u32,
    pointSize: Scaled,
) -> Option<Box<XeTeXFont>> {
    let mut status = false;
    let font = {
        #[cfg(not(target_os = "macos"))]
        {
            XeTeXFont::create(filename, index as _, pointSize.into(), &mut status)
        }
        #[cfg(target_os = "macos")]
        {
            XeTeXFont::wrapper(filename, index as _, pointSize.into(), &mut status)
        }
    };
    if status {
        None
    } else {
        Some(font)
    }
}
pub(crate) unsafe fn setFontLayoutDir(font: &mut XeTeXFontInst, vertical: i32) {
    font.set_layout_dir_vertical(vertical != 0);
}
pub(crate) unsafe fn findFontByName(name: &str, var: &mut String, size: f64) -> PlatformFontRef {
    XeTeXFontMgr_GetFontManager().find_font(name, var, size)
}
pub(crate) unsafe fn getReqEngine() -> u8 {
    XeTeXFontMgr_getReqEngine(XeTeXFontMgr_GetFontManager())
}
pub(crate) unsafe fn setReqEngine(reqEngine: u8) {
    XeTeXFontMgr_setReqEngine(XeTeXFontMgr_GetFontManager(), reqEngine);
}
pub(crate) unsafe fn getFullName(fontRef: PlatformFontRef) -> String {
    XeTeXFontMgr_GetFontManager().get_full_name(fontRef)
}
impl XeTeXLayoutEngine {
    pub(crate) unsafe fn get_font_filename(&self, index: &mut u32) -> String {
        self.font.get_filename(index).to_string()
    }
}
pub(crate) unsafe fn getFontRef(engine: &XeTeXLayoutEngine) -> PlatformFontRef {
    engine.fontRef
}
#[cfg(target_os = "macos")]
pub(crate) unsafe fn getFontTablePtr(font: &XeTeXFontInst, tableTag: u32) -> *mut libc::c_void {
    font.get_font_table(tableTag)
}
pub(crate) unsafe fn getSlant(font: &XeTeXFontInst) -> Scaled {
    let italAngle = font.get_italic_angle();
    (-italAngle as f64 * std::f64::consts::PI / 180.)
        .tan()
        .into()
}
unsafe fn getLargerScriptListTable(font: &XeTeXFontInst, scriptList: *mut *mut hb_tag_t) -> u32 {
    let face = hb_font_get_face(font.get_hb_font());
    let mut scriptCountSub = hb_ot_layout_table_get_script_tags(
        face,
        u32::from_be_bytes([b'G', b'S', b'U', b'B']),
        0,
        ptr::null_mut(),
        ptr::null_mut(),
    );
    let scriptListSub = xcalloc(
        scriptCountSub as _,
        ::std::mem::size_of::<*mut hb_tag_t>() as _,
    ) as *mut hb_tag_t;
    hb_ot_layout_table_get_script_tags(
        face,
        u32::from_be_bytes([b'G', b'S', b'U', b'B']),
        0,
        &mut scriptCountSub,
        scriptListSub,
    );
    let mut scriptCountPos = hb_ot_layout_table_get_script_tags(
        face,
        u32::from_be_bytes([b'G', b'P', b'O', b'S']),
        0,
        ptr::null_mut(),
        ptr::null_mut(),
    );
    let scriptListPos = xcalloc(
        scriptCountPos as _,
        ::std::mem::size_of::<*mut hb_tag_t>() as _,
    ) as *mut hb_tag_t;
    hb_ot_layout_table_get_script_tags(
        face,
        u32::from_be_bytes([b'G', b'S', b'U', b'B']),
        0,
        &mut scriptCountPos,
        scriptListPos,
    );
    if scriptCountSub > scriptCountPos {
        if !scriptList.is_null() {
            *scriptList = scriptListSub
        }
        scriptCountSub
    } else {
        if !scriptList.is_null() {
            *scriptList = scriptListPos
        }
        scriptCountPos
    }
}
pub(crate) unsafe fn countScripts(font: &XeTeXFontInst) -> u32 {
    getLargerScriptListTable(font, ptr::null_mut())
}
pub(crate) unsafe fn getIndScript(font: &XeTeXFontInst, index: u32) -> hb_tag_t {
    let mut scriptList = ptr::null_mut();
    let scriptCount = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() && index < scriptCount {
        *scriptList.offset(index as isize)
    } else {
        0
    }
}
pub(crate) unsafe fn countLanguages(font: &XeTeXFontInst, script: hb_tag_t) -> u32 {
    let mut rval: u32 = 0;
    let face = hb_font_get_face(font.get_hb_font());
    let mut scriptList = ptr::null_mut();
    let scriptCount = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        for i in 0..scriptCount {
            if *scriptList.offset(i as isize) == script {
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    u32::from_be_bytes([b'G', b'S', b'U', b'B']),
                    i,
                    0,
                    ptr::null_mut(),
                    ptr::null_mut(),
                ));
                rval = rval.wrapping_add(hb_ot_layout_script_get_language_tags(
                    face,
                    u32::from_be_bytes([b'G', b'P', b'O', b'S']),
                    i,
                    0,
                    ptr::null_mut(),
                    ptr::null_mut(),
                ));
                break;
            }
        }
    }
    rval
}
pub(crate) unsafe fn getIndLanguage(
    font: &XeTeXFontInst,
    script: hb_tag_t,
    index: u32,
) -> hb_tag_t {
    use bridge::size_t;
    let face = hb_font_get_face(font.get_hb_font());
    let mut scriptList = ptr::null_mut();
    let scriptCount = getLargerScriptListTable(font, &mut scriptList);
    if !scriptList.is_null() {
        for i in 0..scriptCount {
            if *scriptList.offset(i as isize) == script {
                let mut langCount = hb_ot_layout_script_get_language_tags(
                    face,
                    u32::from_be_bytes([b'G', b'S', b'U', b'B']),
                    i,
                    0,
                    ptr::null_mut(),
                    ptr::null_mut(),
                );
                let langList = xcalloc(
                    langCount as size_t,
                    ::std::mem::size_of::<*mut hb_tag_t>() as _,
                ) as *mut hb_tag_t;
                hb_ot_layout_script_get_language_tags(
                    face,
                    u32::from_be_bytes([b'G', b'S', b'U', b'B']),
                    i,
                    0,
                    &mut langCount,
                    langList,
                );
                if index < langCount {
                    return *langList.offset(index as isize);
                } else {
                    free(langList as *mut libc::c_void);
                    langCount = hb_ot_layout_script_get_language_tags(
                        face,
                        u32::from_be_bytes([b'G', b'P', b'O', b'S']),
                        i,
                        0,
                        ptr::null_mut(),
                        ptr::null_mut(),
                    );
                    let langList = xcalloc(
                        langCount as size_t,
                        ::std::mem::size_of::<*mut hb_tag_t>() as _,
                    ) as *mut hb_tag_t;
                    hb_ot_layout_script_get_language_tags(
                        face,
                        u32::from_be_bytes([b'G', b'P', b'O', b'S']),
                        i,
                        0,
                        &mut langCount,
                        langList,
                    );
                    if index < langCount {
                        return *langList.offset(index as isize);
                    } else {
                        free(langList as *mut libc::c_void);
                    }
                }
            }
        }
    }
    0
}
pub(crate) unsafe fn countFeatures(
    font: &XeTeXFontInst,
    script: hb_tag_t,
    language: hb_tag_t,
) -> u32 {
    let mut rval = 0;
    let face: *mut hb_face_t = hb_font_get_face(font.get_hb_font());
    for i in 0..2 {
        let mut scriptIndex = 0;
        let mut langIndex = 0;
        let tableTag: hb_tag_t = if i == 0 {
            u32::from_be_bytes([b'G', b'S', b'U', b'B'])
        } else {
            u32::from_be_bytes([b'G', b'P', b'O', b'S'])
        };
        if hb_ot_layout_table_find_script(face, tableTag, script, &mut scriptIndex) != 0
            && (hb_ot_layout_script_find_language(
                face,
                tableTag,
                scriptIndex,
                language,
                &mut langIndex,
            ) != 0
                || language == 0)
        {
            rval += hb_ot_layout_language_get_feature_tags(
                face,
                tableTag,
                scriptIndex,
                langIndex,
                0,
                ptr::null_mut(),
                ptr::null_mut(),
            );
        }
    }
    rval
}
pub(crate) unsafe fn getIndFeature(
    font: &XeTeXFontInst,
    script: hb_tag_t,
    language: hb_tag_t,
    mut index: u32,
) -> hb_tag_t {
    use bridge::size_t;
    let face = hb_font_get_face(font.get_hb_font());
    for i in 0..2 {
        let mut scriptIndex = 0;
        let mut langIndex = 0;
        let tableTag: hb_tag_t = if i == 0 {
            u32::from_be_bytes([b'G', b'S', b'U', b'B'])
        } else {
            u32::from_be_bytes([b'G', b'P', b'O', b'S'])
        };
        if hb_ot_layout_table_find_script(face, tableTag, script, &mut scriptIndex) != 0
            && (hb_ot_layout_script_find_language(
                face,
                tableTag,
                scriptIndex,
                language,
                &mut langIndex,
            ) != 0
                || language == 0)
        {
            let mut featCount = hb_ot_layout_language_get_feature_tags(
                face,
                tableTag,
                scriptIndex,
                langIndex,
                0,
                ptr::null_mut(),
                ptr::null_mut(),
            );
            let featList = xcalloc(
                featCount as size_t,
                ::std::mem::size_of::<*mut hb_tag_t>() as _,
            ) as *mut hb_tag_t;
            hb_ot_layout_language_get_feature_tags(
                face,
                tableTag,
                scriptIndex,
                langIndex,
                0,
                &mut featCount,
                featList,
            );
            if index < featCount {
                return *featList.offset(index as isize);
            } else {
                index -= featCount;
            }
        }
    }
    0
}
pub(crate) unsafe fn countGraphiteFeatures(engine: &XeTeXLayoutEngine) -> u32 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        gr_face_n_fref(grFace) as u32
    } else {
        0
    }
}
pub(crate) unsafe fn getGraphiteFeatureCode(engine: &XeTeXLayoutEngine, index: u32) -> u32 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_fref(grFace, index as u16);
        gr_fref_id(feature)
    } else {
        0
    }
}
pub(crate) unsafe fn countGraphiteFeatureSettings(
    engine: &XeTeXLayoutEngine,
    featureID: u32,
) -> u32 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, featureID);
        gr_fref_n_values(feature) as u32
    } else {
        0
    }
}
pub(crate) unsafe fn getGraphiteFeatureSettingCode(
    engine: &XeTeXLayoutEngine,
    featureID: u32,
    index: u32,
) -> u32 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, featureID);
        gr_fref_value(feature, index as u16) as u32
    } else {
        0
    }
}
pub(crate) unsafe fn getGraphiteFeatureDefaultSetting(
    engine: &XeTeXLayoutEngine,
    featureID: u32,
) -> u32 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, featureID);
        let featureValues = gr_face_featureval_for_lang(
            grFace,
            hb_tag_from_string(
                hb_language_to_string(engine.language),
                strlen(hb_language_to_string(engine.language)) as i32,
            ),
        );
        gr_fref_feature_value(feature, featureValues) as u32
    } else {
        0
    }
}
pub(crate) unsafe fn getGraphiteFeatureLabel(
    engine: &XeTeXLayoutEngine,
    featureID: u32,
) -> *mut i8 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, featureID);
        let mut len = 0;
        let mut langID = 0x409;
        gr_fref_label(feature, &mut langID, gr_utf8, &mut len) as *mut i8
    } else {
        ptr::null_mut()
    }
}
pub(crate) unsafe fn getGraphiteFeatureSettingLabel(
    engine: &XeTeXLayoutEngine,
    featureID: u32,
    settingID: u32,
) -> *mut i8 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, featureID);
        for i in 0..gr_fref_n_values(feature) {
            if settingID as i32 == gr_fref_value(feature, i) as i32 {
                let mut len = 0;
                let mut langID = 0x409;
                return gr_fref_value_label(feature, i, &mut langID, gr_utf8, &mut len) as *mut i8;
            }
        }
    }
    ptr::null_mut()
}
pub(crate) unsafe fn findGraphiteFeature(
    engine: &XeTeXLayoutEngine,
    mut s: &[u8],
) -> Option<(hb_tag_t, i32)>
/* s...e is a "feature=setting" string; look for this in the font */ {
    while b" \t".contains(&s[0]) {
        s = &s[1..];
    }
    let mut cp = s;
    while !cp.is_empty() && cp[0] != b'=' {
        cp = &cp[1..];
    }
    let tmp = findGraphiteFeatureNamed(engine, &s[..s.len() - cp.len()]);
    let f = tmp as hb_tag_t;
    if tmp == -1 {
        return None;
    }
    cp = &cp[1..];
    while !cp.is_empty() && b" \t".contains(&cp[0]) {
        cp = &cp[1..];
    }
    if cp.is_empty() {
        /* no setting was specified */
        return None;
    }
    let v = findGraphiteFeatureSettingNamed(engine, f, cp) as i32;
    if v != -1 {
        Some((f, v))
    } else {
        None
    }
}
pub(crate) unsafe fn findGraphiteFeatureNamed(engine: &XeTeXLayoutEngine, name: &[u8]) -> i64 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        for i in 0..gr_face_n_fref(grFace) {
            let feature = gr_face_fref(grFace, i);
            let mut len = 0;
            let mut langID = 0x409;
            // the first call is to get the length of the string
            gr_fref_label(feature, &mut langID, gr_utf8, &mut len);
            let label = gr_fref_label(feature, &mut langID, gr_utf8, &mut len) as *mut i8;
            if std::ffi::CStr::from_ptr(label).to_bytes() == name {
                let rval = gr_fref_id(feature) as i64;
                gr_label_destroy(label as *mut libc::c_void);
                return rval;
            } else {
                gr_label_destroy(label as *mut libc::c_void);
            }
        }
    }
    -1
}
pub(crate) unsafe fn findGraphiteFeatureSettingNamed(
    engine: &XeTeXLayoutEngine,
    id: u32,
    name: &[u8],
) -> i64 {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let Some(grFace) = hb_graphite2_face_get_gr_face(hbFace).as_ref() {
        let feature = gr_face_find_fref(grFace, id);
        for i in 0..gr_fref_n_values(feature) {
            let mut len = 0;
            let mut langID = 0x409;
            // the first call is to get the length of the string
            gr_fref_value_label(feature, i, &mut langID, gr_utf8, &mut len);
            let label = gr_fref_value_label(feature, i, &mut langID, gr_utf8, &mut len) as *mut i8;
            if std::ffi::CStr::from_ptr(label).to_bytes() == name {
                let rval = gr_fref_value(feature, i) as i64;
                gr_label_destroy(label as *mut libc::c_void);
                return rval;
            } else {
                gr_label_destroy(label as *mut libc::c_void);
            }
        }
    }
    -1
}
pub(crate) unsafe fn countGlyphs(font: &XeTeXFontInst) -> u32 {
    font.get_num_glyphs() as u32
}

impl XeTeXLayoutEngine {
    pub(crate) unsafe fn get_font(&self) -> &XeTeXFontInst {
        &*self.font
    }
    pub(crate) fn get_extend_factor(&self) -> f32 {
        self.extend
    }
    pub(crate) fn get_slant_factor(&self) -> f32 {
        self.slant
    }
    pub(crate) fn get_embolden_factor(&self) -> f32 {
        self.embolden
    }
}

impl XeTeXLayoutEngine {
    #[allow(clippy::too_many_arguments)]
    pub(crate) unsafe fn create(
        fontRef: PlatformFontRef,
        font: Box<XeTeXFont>,
        script: hb_tag_t,
        language: String,
        features: Vec<hb_feature_t>,
        shapers: *mut *mut i8,
        rgbValue: u32,
        extend: f32,
        slant: f32,
        embolden: f32,
    ) -> Box<Self> {
        let language = if getReqEngine() == b'G' {
            hb_language_from_string(language.as_ptr() as *const i8, language.len() as _)
        } else {
            hb_ot_tag_to_language(hb_tag_from_string(
                language.as_ptr() as *const i8,
                language.len() as _,
            ))
        };
        Box::new(Self {
            fontRef,
            font,
            script,
            features,
            shaper_list: ShaperList {
                list: shapers,
                to_free: false,
            },
            shaper: String::new(),
            rgbValue,
            extend,
            slant,
            embolden,
            hbBuffer: hb::HbBuffer::new(),
            // For Graphite fonts treat the language as BCP 47 tag, for OpenType we
            // treat it as a OT language tag for backward compatibility with pre-0.9999
            // XeTeX.
            language,
        })
    }
    pub(crate) fn release(self) -> Box<XeTeXFont> {
        let Self { font, .. } = self;
        font
    }
}

unsafe extern "C" fn _decompose_compat(
    mut _ufuncs: *mut hb_unicode_funcs_t,
    mut _u: hb_codepoint_t,
    mut _decomposed: *mut hb_codepoint_t,
    mut _user_data: *mut libc::c_void,
) -> u32 {
    0
}
unsafe fn _get_unicode_funcs() -> *mut hb_unicode_funcs_t {
    static mut ufuncs: *mut hb_unicode_funcs_t = ptr::null_mut();
    if ufuncs.is_null() {
        ufuncs = hb_unicode_funcs_create(hb_icu_get_unicode_funcs())
    }
    hb_unicode_funcs_set_decompose_compatibility_func(
        ufuncs,
        Some(_decompose_compat),
        ptr::null_mut(),
        None,
    );
    ufuncs
}
static mut hbUnicodeFuncs: *mut hb_unicode_funcs_t = ptr::null_mut();

impl XeTeXLayoutEngine {
    pub(crate) unsafe fn layout_chars(
        &mut self,
        chars: &[u16],
        offset: i32,
        count: i32,
        rightToLeft: bool,
    ) -> i32 {
        let mut direction: hb_direction_t = HB_DIRECTION_LTR;
        let mut segment_props: hb_segment_properties_t = hb_segment_properties_t {
            direction: HB_DIRECTION_INVALID,
            script: HB_SCRIPT_INVALID,
            language: ptr::null(),
            reserved1: ptr::null_mut(),
            reserved2: ptr::null_mut(),
        };
        let hbFont = self.font.get_hb_font();
        let hbFace = hb_font_get_face(hbFont);
        if self.font.get_layout_dir_vertical() {
            direction = HB_DIRECTION_TTB
        } else if rightToLeft {
            direction = HB_DIRECTION_RTL
        }
        let script = hb_ot_tag_to_script(self.script);
        if hbUnicodeFuncs.is_null() {
            hbUnicodeFuncs = _get_unicode_funcs()
        }
        self.hbBuffer.reset();
        hb_buffer_set_unicode_funcs(self.hbBuffer.0, hbUnicodeFuncs);
        self.hbBuffer.add_utf16(chars, offset as u32, count);
        self.hbBuffer.set_direction(direction);
        self.hbBuffer.set_script(script);
        hb_buffer_set_language(self.hbBuffer.0, self.language);
        self.hbBuffer.guess_segment_properties();
        hb_buffer_get_segment_properties(self.hbBuffer.0, &mut segment_props);
        if self.shaper_list.list.is_null() {
            // HarfBuzz gives graphite2 shaper a priority, so that for hybrid
            // Graphite/OpenType fonts, Graphite will be used. However, pre-0.9999
            // XeTeX preferred OpenType over Graphite, so we are doing the same
            // here for sake of backward compatibility. Since "ot" shaper never
            // fails, we set the shaper list to just include it.
            self.shaper_list.list =
                xcalloc(2, ::std::mem::size_of::<*mut i8>() as _) as *mut *mut i8;
            *self.shaper_list.list.offset(0) = b"ot\x00".as_ptr() as *const i8 as *mut i8;
            *self.shaper_list.list.offset(1) = ptr::null_mut();
            self.shaper_list.to_free = true;
        }
        let mut shape_plan = hb_shape_plan_create_cached(
            hbFace,
            &segment_props,
            self.features.as_ptr(),
            self.features.len() as u32,
            self.shaper_list.list as *const *const i8,
        );
        let res = hb_shape_plan_execute(
            shape_plan,
            hbFont,
            self.hbBuffer.0,
            self.features.as_ptr(),
            self.features.len() as u32,
        ) != 0;
        if res {
            self.shaper = CStr::from_ptr(hb_shape_plan_get_shaper(shape_plan))
                .to_str()
                .unwrap()
                .to_string();
            self.hbBuffer
                .set_content_type(HB_BUFFER_CONTENT_TYPE_GLYPHS);
        } else {
            // all selected shapers failed, retrying with default
            // we don't use _cached here as the cached plain will always fail.
            hb_shape_plan_destroy(shape_plan); /* negative is forwards */
            shape_plan = hb_shape_plan_create(
                hbFace,
                &segment_props,
                self.features.as_ptr(),
                self.features.len() as u32,
                ptr::null(),
            ); /* negative is upwards */
            let res = hb_shape_plan_execute(
                shape_plan,
                hbFont,
                self.hbBuffer.0,
                self.features.as_ptr(),
                self.features.len() as u32,
            ) != 0;
            if res {
                self.shaper = CStr::from_ptr(hb_shape_plan_get_shaper(shape_plan))
                    .to_str()
                    .unwrap()
                    .to_string();
                self.hbBuffer
                    .set_content_type(HB_BUFFER_CONTENT_TYPE_GLYPHS);
            } else {
                abort!("all shapers failed");
            }
        }
        hb_shape_plan_destroy(shape_plan);
        self.hbBuffer.get_length() as i32
    }

    pub(crate) fn get_glyphs(&self) -> Vec<u32> {
        let glyphCount = self.hbBuffer.get_length() as usize;
        self.hbBuffer
            .get_glyph_infos()
            .iter()
            .take(glyphCount)
            .map(|glyph| glyph.codepoint)
            .collect()
    }

    pub(crate) fn get_glyph_advances(&self) -> Vec<f32> {
        let glyphCount = self.hbBuffer.get_length() as usize;
        self.hbBuffer
            .get_glyph_positions()
            .iter()
            .take(glyphCount)
            .map(|hb_pos| {
                if self.font.get_layout_dir_vertical() {
                    self.font.units_to_points(hb_pos.y_advance as f32)
                } else {
                    self.font.units_to_points(hb_pos.x_advance as f32)
                }
            })
            .collect()
    }

    pub(crate) fn get_glyph_positions(&self) -> Vec<FloatPoint> {
        let glyphCount = self.hbBuffer.get_length() as usize;
        let hbPositions = self.hbBuffer.get_glyph_positions();
        let mut x = 0_f32;
        let mut y = 0_f32;
        let mut positions = Vec::with_capacity(glyphCount + 1);
        if self.font.get_layout_dir_vertical() {
            for hb_pos in hbPositions.iter().take(glyphCount) {
                positions.push(FloatPoint {
                    x: -self.font.units_to_points(x + hb_pos.y_offset as f32),
                    y: self.font.units_to_points(y - hb_pos.x_offset as f32),
                });
                x += hb_pos.y_advance as f32;
                y += hb_pos.x_advance as f32;
            }
            positions.push(FloatPoint {
                x: -self.font.units_to_points(x),
                y: self.font.units_to_points(y),
            });
        } else {
            for pos in hbPositions.iter().take(glyphCount as usize) {
                positions.push(FloatPoint {
                    x: self.font.units_to_points(x + pos.x_offset as f32),
                    y: -self.font.units_to_points(y + pos.y_offset as f32),
                });
                x += pos.x_advance as f32;
                y += pos.y_advance as f32;
            }
            positions.push(FloatPoint {
                x: self.font.units_to_points(x),
                y: -self.font.units_to_points(y),
            });
        }
        if self.extend as f64 != 1. || self.slant as f64 != 0. {
            for pos in &mut positions {
                pos.x = pos.x * self.extend - pos.y * self.slant;
            }
        };
        positions
    }
    pub(crate) fn get_point_size(&self) -> f32 {
        self.font.get_point_size()
    }
    pub(crate) unsafe fn get_ascent_and_descent(&self) -> (f32, f32) {
        (self.font.get_ascent(), self.font.get_descent())
    }
    pub(crate) unsafe fn get_cap_and_x_height(&self) -> (f32, f32) {
        (self.font.get_cap_height(), self.font.get_x_height())
    }
    pub(crate) unsafe fn get_default_direction(&self) -> i32 {
        let script: hb_script_t = self.hbBuffer.get_script();
        if hb_script_get_horizontal_direction(script) as u32 == HB_DIRECTION_RTL as u32 {
            0xff
        } else {
            0xfe
        }
    }
    pub(crate) fn get_rgb_value(&self) -> u32 {
        self.rgbValue
    }
    pub(crate) unsafe fn get_glyph_bounds(&self, glyphID: u32) -> GlyphBBox {
        let mut bbox = self.font.get_glyph_bounds(glyphID as GlyphID);
        if self.extend as f64 != 0. {
            bbox.xMin *= self.extend;
            bbox.xMax *= self.extend
        };
        bbox
    }
    pub(crate) unsafe fn get_glyph_width_from_engine(&self, glyphID: u32) -> f32 {
        self.extend * self.font.get_glyph_width(glyphID as GlyphID)
    }
    pub(crate) unsafe fn get_glyph_height_depth(&self, glyphID: u32) -> (f32, f32) {
        self.font.get_glyph_height_depth(glyphID as GlyphID)
    }
    pub(crate) unsafe fn get_glyph_sidebearings(&self, glyphID: u32) -> (f32, f32) {
        let (lsb, rsb) = self.font.get_glyph_sidebearings(glyphID as GlyphID);
        if self.extend as f64 != 0. {
            (lsb * self.extend, rsb * self.extend)
        } else {
            (lsb, rsb)
        }
    }
    pub(crate) unsafe fn get_glyph_ital_corr(&self, glyphID: u32) -> f32 {
        self.extend * self.font.get_glyph_ital_corr(glyphID as GlyphID)
    }
    pub(crate) unsafe fn map_char_to_glyph(&self, charCode: char) -> u32 {
        self.font.map_char_to_glyph(charCode) as u32
    }
    pub(crate) unsafe fn get_font_char_range(&mut self, reqFirst: i32) -> i32 {
        if reqFirst != 0 {
            self.font.get_first_char_code()
        } else {
            self.font.get_last_char_code()
        }
    }
}
pub(crate) unsafe fn mapGlyphToIndex(engine: &XeTeXLayoutEngine, glyphName: *const i8) -> i32 {
    engine.font.map_glyph_to_index(glyphName) as i32
}
static mut grSegment: *mut gr_segment = ptr::null_mut();
static mut grPrevSlot: *const gr_slot = ptr::null();
static mut grTextLen: i32 = 0;
pub(crate) unsafe fn initGraphiteBreaking(engine: &XeTeXLayoutEngine, txt: &[u16]) -> bool {
    let hbFace = hb_font_get_face(engine.font.get_hb_font());
    if let (Some(grFace), Some(grFont)) = (
        hb_graphite2_face_get_gr_face(hbFace).as_ref(),
        hb_graphite2_font_get_gr_font(engine.font.get_hb_font()).as_ref(),
    ) {
        if !grSegment.is_null() {
            gr_seg_destroy(grSegment);
            grSegment = ptr::null_mut();
            grPrevSlot = ptr::null();
        }
        let grFeatureValues = gr_face_featureval_for_lang(
            grFace,
            hb_tag_from_string(
                hb_language_to_string(engine.language),
                strlen(hb_language_to_string(engine.language)) as i32,
            ),
        );
        for f in &engine.features {
            let fref = gr_face_find_fref(grFace, f.tag);
            if !fref.is_null() {
                gr_fref_set_feature_value(fref, f.value as u16, grFeatureValues);
            }
        }
        grSegment = gr_make_seg(
            grFont,
            grFace,
            engine.script,
            grFeatureValues,
            gr_utf16,
            txt.as_ptr() as *const libc::c_void,
            txt.len() as _,
            0,
        );
        grPrevSlot = gr_seg_first_slot(grSegment);
        grTextLen = txt.len() as _;
        return true;
    }
    false
}
pub(crate) unsafe fn findNextGraphiteBreak() -> i32 {
    let mut ret = -1;
    if !grSegment.is_null() && !grPrevSlot.is_null() && grPrevSlot != gr_seg_last_slot(grSegment) {
        let mut s: *const gr_slot = gr_slot_next_in_segment(grPrevSlot);
        while !s.is_null() {
            let ci = gr_seg_cinfo(grSegment, gr_slot_index(s));
            let bw = gr_cinfo_break_weight(ci);
            if bw < gr_breakNone as i32 && bw >= gr_breakBeforeWord as i32 {
                grPrevSlot = s;
                ret = gr_cinfo_base(ci) as i32
            } else if bw > gr_breakNone as i32 && bw <= gr_breakWord as i32 {
                grPrevSlot = gr_slot_next_in_segment(s);
                ret = gr_cinfo_base(ci).wrapping_add(1) as i32
            }
            if ret != -1 {
                break;
            }
            s = gr_slot_next_in_segment(s)
        }
        if ret == -1 {
            grPrevSlot = gr_seg_last_slot(grSegment);
            ret = grTextLen
        }
    }
    ret
}

/* graphite interface functions... */
impl XeTeXLayoutEngine {
    pub(crate) fn using_graphite(&self) -> bool {
        self.shaper == "graphite2"
    }
    pub(crate) fn using_open_type(&self) -> bool {
        self.shaper.is_empty() || self.shaper == "ot"
    }
    pub(crate) unsafe fn is_open_type_math_font(&self) -> bool {
        hb_ot_math_has_data(hb_font_get_face(self.font.get_hb_font())) != 0
    }
}

pub(crate) mod hb {
    use harfbuzz_sys::*;
    pub struct HbBuffer(pub(crate) *mut hb_buffer_t);

    impl HbBuffer {
        pub fn new() -> Self {
            unsafe { Self(hb_buffer_create()) }
        }
        pub fn reset(&mut self) {
            unsafe { hb_buffer_reset(self.0) }
        }

        pub fn get_length(&self) -> u32 {
            unsafe { hb_buffer_get_length(self.0) }
        }
        pub fn get_glyph_positions(&self) -> &[hb_glyph_position_t] {
            let mut length: u32 = 0;
            unsafe {
                let ptr = hb_buffer_get_glyph_positions(self.0, &mut length);
                std::slice::from_raw_parts(ptr, length as usize)
            }
        }
        pub fn get_glyph_infos(&self) -> &[hb_glyph_info_t] {
            let mut length: u32 = 0;
            unsafe {
                let ptr = hb_buffer_get_glyph_infos(self.0, &mut length);
                std::slice::from_raw_parts(ptr, length as usize)
            }
        }
        pub fn set_script(&mut self, script: hb_script_t) {
            unsafe { hb_buffer_set_script(self.0, script) }
        }
        pub fn get_script(&self) -> hb_script_t {
            unsafe { hb_buffer_get_script(self.0) }
        }

        pub fn set_content_type(&mut self, content_type: hb_buffer_content_type_t) {
            unsafe { hb_buffer_set_content_type(self.0, content_type) }
        }
        pub fn guess_segment_properties(&mut self) {
            unsafe { hb_buffer_guess_segment_properties(self.0) }
        }
        pub fn set_direction(&mut self, direction: hb_direction_t) {
            unsafe { hb_buffer_set_direction(self.0, direction) }
        }
        pub fn add_utf16(&mut self, text: &[u16], item_offset: u32, item_length: i32) {
            unsafe {
                hb_buffer_add_utf16(
                    self.0,
                    text.as_ptr(),
                    text.len() as _,
                    item_offset,
                    item_length,
                )
            }
        }
    }

    impl Drop for HbBuffer {
        fn drop(&mut self) {
            unsafe { hb_buffer_destroy(self.0) }
        }
    }
}
