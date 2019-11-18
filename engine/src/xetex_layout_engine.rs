#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#![feature(
    const_raw_ptr_to_usize_cast,
    extern_types,
    label_break_value,
    ptr_wrapping_offset_from
)]

// Fc* functions are from fontconfig.

use crate::core_memory::{xcalloc, xmalloc};
use harfbuzz_sys::*;

use crate::xetex_font_info::XeTeXFontInst;
#[cfg(target_os = "macos")]
use crate::xetex_font_info::XeTeXFontInst_Mac_create;
use crate::xetex_font_manager::{PlatformFontRef, XeTeXFontMgr, XeTeXFontMgrFamily, XeTeXFontMgrFont};

pub type Fixed = i32;

#[derive(Copy, Clone)]
#[cfg_attr(not(target_os = "macos"), repr(C))]
#[cfg_attr(target_os = "macos", repr(C, packed(2)))]
pub struct FixedPoint {
    pub x: Fixed,
    pub y: Fixed,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct FloatPoint {
    pub x: f32,
    pub y: f32,
}

#[cfg(not(target_os = "macos"))]
extern "C" {
    pub type _FcPattern;
}
#[cfg(not(target_os = "macos"))]
pub type FcPattern = _FcPattern;

pub type scaled_t = i32;


extern "C" {
    /* *************************************************************************
     *
     * @type:
     *   FT_Library
     *
     * @description:
     *   A handle to a FreeType library instance.  Each 'library' is completely
     *   independent from the others; it is the 'root' of a set of objects like
     *   fonts, faces, sizes, etc.
     *
     *   It also embeds a memory manager (see @FT_Memory), as well as a
     *   scan-line converter object (see @FT_Raster).
     *
     *   [Since 2.5.6] In multi-threaded applications it is easiest to use one
     *   `FT_Library` object per thread.  In case this is too cumbersome, a
     *   single `FT_Library` object across threads is possible also, as long as
     *   a mutex lock is used around @FT_New_Face and @FT_Done_Face.
     *
     * @note:
     *   Library objects are normally created by @FT_Init_FreeType, and
     *   destroyed with @FT_Done_FreeType.  If you need reference-counting
     *   (cf. @FT_Reference_Library), use @FT_New_Library and @FT_Done_Library.
     */
    pub type FT_LibraryRec_;
    /* *************************************************************************
     *
     * @type:
     *   FT_Driver
     *
     * @description:
     *   A handle to a given FreeType font driver object.  A font driver is a
     *   module capable of creating faces from font files.
     */
    pub type FT_DriverRec_;
    pub type FT_Face_InternalRec_;
    pub type FT_Size_InternalRec_;
    pub type FT_Slot_InternalRec_;
    pub type FT_SubGlyphRec_;
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
    XeTeXFontMgr_getReqEngine, XeTeXFontMgr_setReqEngine,
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

/* ***************************************************************************
 *
 * ftsystem.h
 *
 *   FreeType low-level system interface definition (specification).
 *
 * Copyright (C) 1996-2019 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */
/* *************************************************************************
 *
 * @section:
 *  system_interface
 *
 * @title:
 *  System Interface
 *
 * @abstract:
 *  How FreeType manages memory and i/o.
 *
 * @description:
 *  This section contains various definitions related to memory management
 *  and i/o access.  You need to understand this information if you want to
 *  use a custom memory manager or you own i/o streams.
 *
 */
/* *************************************************************************
 *
 *                 M E M O R Y   M A N A G E M E N T
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Memory
 *
 * @description:
 *   A handle to a given memory manager object, defined with an
 *   @FT_MemoryRec structure.
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_MemoryRec_ {
    pub user: *mut libc::c_void,
    pub alloc: FT_Alloc_Func,
    pub free: FT_Free_Func,
    pub realloc: FT_Realloc_Func,
}
pub type FT_Realloc_Func = Option<
    unsafe fn(
        _: FT_Memory,
        _: libc::c_long,
        _: libc::c_long,
        _: *mut libc::c_void,
    ) -> *mut libc::c_void,
>;
pub type FT_Memory = *mut FT_MemoryRec_;
pub type FT_Free_Func = Option<unsafe fn(_: FT_Memory, _: *mut libc::c_void) -> ()>;
pub type FT_Alloc_Func = Option<unsafe fn(_: FT_Memory, _: libc::c_long) -> *mut libc::c_void>;
/* *************************************************************************
 *
 *                      I / O   M A N A G E M E N T
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Stream
 *
 * @description:
 *   A handle to an input stream.
 *
 * @also:
 *   See @FT_StreamRec for the publicly accessible fields of a given stream
 *   object.
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_StreamRec_ {
    pub base: *mut libc::c_uchar,
    pub size: libc::c_ulong,
    pub pos: libc::c_ulong,
    pub descriptor: FT_StreamDesc,
    pub pathname: FT_StreamDesc,
    pub read: FT_Stream_IoFunc,
    pub close: FT_Stream_CloseFunc,
    pub memory: FT_Memory,
    pub cursor: *mut libc::c_uchar,
    pub limit: *mut libc::c_uchar,
}
pub type FT_Stream_CloseFunc = Option<unsafe fn(_: FT_Stream) -> ()>;
pub type FT_Stream = *mut FT_StreamRec_;
pub type FT_Stream_IoFunc = Option<
    unsafe fn(
        _: FT_Stream,
        _: libc::c_ulong,
        _: *mut libc::c_uchar,
        _: libc::c_ulong,
    ) -> libc::c_ulong,
>;
pub type FT_StreamDesc = FT_StreamDesc_;
#[derive(Copy, Clone)]
#[repr(C)]
pub union FT_StreamDesc_ {
    pub value: libc::c_long,
    pub pointer: *mut libc::c_void,
}
/* ***************************************************************************
 *
 * ftimage.h
 *
 *   FreeType glyph image formats and default raster interface
 *   (specification).
 *
 * Copyright (C) 1996-2019 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */
/* *************************************************************************
 *
 * Note: A 'raster' is simply a scan-line converter, used to render
 *       FT_Outlines into FT_Bitmaps.
 *
 */
/* STANDALONE_ is from ftgrays.c */
/* *************************************************************************
 *
 * @section:
 *   basic_types
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Pos
 *
 * @description:
 *   The type FT_Pos is used to store vectorial coordinates.  Depending on
 *   the context, these can represent distances in integer font units, or
 *   16.16, or 26.6 fixed-point pixel coordinates.
 */
pub type FT_Pos = libc::c_long;
/* *************************************************************************
 *
 * @struct:
 *   FT_Vector
 *
 * @description:
 *   A simple structure used to store a 2D vector; coordinates are of the
 *   FT_Pos type.
 *
 * @fields:
 *   x ::
 *     The horizontal coordinate.
 *   y ::
 *     The vertical coordinate.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Vector_ {
    pub x: FT_Pos,
    pub y: FT_Pos,
}
pub type FT_Vector = FT_Vector_;
/* *************************************************************************
 *
 * @struct:
 *   FT_BBox
 *
 * @description:
 *   A structure used to hold an outline's bounding box, i.e., the
 *   coordinates of its extrema in the horizontal and vertical directions.
 *
 * @fields:
 *   xMin ::
 *     The horizontal minimum (left-most).
 *
 *   yMin ::
 *     The vertical minimum (bottom-most).
 *
 *   xMax ::
 *     The horizontal maximum (right-most).
 *
 *   yMax ::
 *     The vertical maximum (top-most).
 *
 * @note:
 *   The bounding box is specified with the coordinates of the lower left
 *   and the upper right corner.  In PostScript, those values are often
 *   called (llx,lly) and (urx,ury), respectively.
 *
 *   If `yMin` is negative, this value gives the glyph's descender.
 *   Otherwise, the glyph doesn't descend below the baseline.  Similarly,
 *   if `ymax` is positive, this value gives the glyph's ascender.
 *
 *   `xMin` gives the horizontal distance from the glyph's origin to the
 *   left edge of the glyph's bounding box.  If `xMin` is negative, the
 *   glyph extends to the left of the origin.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_BBox_ {
    pub xMin: FT_Pos,
    pub yMin: FT_Pos,
    pub xMax: FT_Pos,
    pub yMax: FT_Pos,
}
pub type FT_BBox = FT_BBox_;
/* these constants are deprecated; use the corresponding `FT_Pixel_Mode` */
/* values instead.                                                       */
/* *************************************************************************
 *
 * @struct:
 *   FT_Bitmap
 *
 * @description:
 *   A structure used to describe a bitmap or pixmap to the raster.  Note
 *   that we now manage pixmaps of various depths through the `pixel_mode`
 *   field.
 *
 * @fields:
 *   rows ::
 *     The number of bitmap rows.
 *
 *   width ::
 *     The number of pixels in bitmap row.
 *
 *   pitch ::
 *     The pitch's absolute value is the number of bytes taken by one
 *     bitmap row, including padding.  However, the pitch is positive when
 *     the bitmap has a 'down' flow, and negative when it has an 'up' flow.
 *     In all cases, the pitch is an offset to add to a bitmap pointer in
 *     order to go down one row.
 *
 *     Note that 'padding' means the alignment of a bitmap to a byte
 *     border, and FreeType functions normally align to the smallest
 *     possible integer value.
 *
 *     For the B/W rasterizer, `pitch` is always an even number.
 *
 *     To change the pitch of a bitmap (say, to make it a multiple of 4),
 *     use @FT_Bitmap_Convert.  Alternatively, you might use callback
 *     functions to directly render to the application's surface; see the
 *     file `example2.cpp` in the tutorial for a demonstration.
 *
 *   buffer ::
 *     A typeless pointer to the bitmap buffer.  This value should be
 *     aligned on 32-bit boundaries in most cases.
 *
 *   num_grays ::
 *     This field is only used with @FT_PIXEL_MODE_GRAY; it gives the
 *     number of gray levels used in the bitmap.
 *
 *   pixel_mode ::
 *     The pixel mode, i.e., how pixel bits are stored.  See @FT_Pixel_Mode
 *     for possible values.
 *
 *   palette_mode ::
 *     This field is intended for paletted pixel modes; it indicates how
 *     the palette is stored.  Not used currently.
 *
 *   palette ::
 *     A typeless pointer to the bitmap palette; this field is intended for
 *     paletted pixel modes.  Not used currently.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Bitmap_ {
    pub rows: libc::c_uint,
    pub width: libc::c_uint,
    pub pitch: libc::c_int,
    pub buffer: *mut libc::c_uchar,
    pub num_grays: libc::c_ushort,
    pub pixel_mode: libc::c_uchar,
    pub palette_mode: libc::c_uchar,
    pub palette: *mut libc::c_void,
}
pub type FT_Bitmap = FT_Bitmap_;
/* *************************************************************************
 *
 * @section:
 *   outline_processing
 *
 */
/* *************************************************************************
 *
 * @struct:
 *   FT_Outline
 *
 * @description:
 *   This structure is used to describe an outline to the scan-line
 *   converter.
 *
 * @fields:
 *   n_contours ::
 *     The number of contours in the outline.
 *
 *   n_points ::
 *     The number of points in the outline.
 *
 *   points ::
 *     A pointer to an array of `n_points` @FT_Vector elements, giving the
 *     outline's point coordinates.
 *
 *   tags ::
 *     A pointer to an array of `n_points` chars, giving each outline
 *     point's type.
 *
 *     If bit~0 is unset, the point is 'off' the curve, i.e., a Bezier
 *     control point, while it is 'on' if set.
 *
 *     Bit~1 is meaningful for 'off' points only.  If set, it indicates a
 *     third-order Bezier arc control point; and a second-order control
 *     point if unset.
 *
 *     If bit~2 is set, bits 5-7 contain the drop-out mode (as defined in
 *     the OpenType specification; the value is the same as the argument to
 *     the 'SCANMODE' instruction).
 *
 *     Bits 3 and~4 are reserved for internal purposes.
 *
 *   contours ::
 *     An array of `n_contours` shorts, giving the end point of each
 *     contour within the outline.  For example, the first contour is
 *     defined by the points '0' to `contours[0]`, the second one is
 *     defined by the points `contours[0]+1` to `contours[1]`, etc.
 *
 *   flags ::
 *     A set of bit flags used to characterize the outline and give hints
 *     to the scan-converter and hinter on how to convert/grid-fit it.  See
 *     @FT_OUTLINE_XXX.
 *
 * @note:
 *   The B/W rasterizer only checks bit~2 in the `tags` array for the first
 *   point of each contour.  The drop-out mode as given with
 *   @FT_OUTLINE_IGNORE_DROPOUTS, @FT_OUTLINE_SMART_DROPOUTS, and
 *   @FT_OUTLINE_INCLUDE_STUBS in `flags` is then overridden.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Outline_ {
    pub n_contours: libc::c_short,
    pub n_points: libc::c_short,
    pub points: *mut FT_Vector,
    pub tags: *mut libc::c_char,
    pub contours: *mut libc::c_short,
    pub flags: libc::c_int,
    /* outline masks                      */
}
pub type FT_Outline = FT_Outline_;
/* *************************************************************************
 *
 * @section:
 *   basic_types
 *
 */
/* *************************************************************************
 *
 * @macro:
 *   FT_IMAGE_TAG
 *
 * @description:
 *   This macro converts four-letter tags to an unsigned long type.
 *
 * @note:
 *   Since many 16-bit compilers don't like 32-bit enumerations, you should
 *   redefine this macro in case of problems to something like this:
 *
 *   ```
 *     #define FT_IMAGE_TAG( value, _x1, _x2, _x3, _x4 )  value
 *   ```
 *
 *   to get a simple enumeration without assigning special numbers.
 */
/* FT_IMAGE_TAG */
/* *************************************************************************
 *
 * @enum:
 *   FT_Glyph_Format
 *
 * @description:
 *   An enumeration type used to describe the format of a given glyph
 *   image.  Note that this version of FreeType only supports two image
 *   formats, even though future font drivers will be able to register
 *   their own format.
 *
 * @values:
 *   FT_GLYPH_FORMAT_NONE ::
 *     The value~0 is reserved.
 *
 *   FT_GLYPH_FORMAT_COMPOSITE ::
 *     The glyph image is a composite of several other images.  This format
 *     is _only_ used with @FT_LOAD_NO_RECURSE, and is used to report
 *     compound glyphs (like accented characters).
 *
 *   FT_GLYPH_FORMAT_BITMAP ::
 *     The glyph image is a bitmap, and can be described as an @FT_Bitmap.
 *     You generally need to access the `bitmap` field of the
 *     @FT_GlyphSlotRec structure to read it.
 *
 *   FT_GLYPH_FORMAT_OUTLINE ::
 *     The glyph image is a vectorial outline made of line segments and
 *     Bezier arcs; it can be described as an @FT_Outline; you generally
 *     want to access the `outline` field of the @FT_GlyphSlotRec structure
 *     to read it.
 *
 *   FT_GLYPH_FORMAT_PLOTTER ::
 *     The glyph image is a vectorial path with no inside and outside
 *     contours.  Some Type~1 fonts, like those in the Hershey family,
 *     contain glyphs in this format.  These are described as @FT_Outline,
 *     but FreeType isn't currently capable of rendering them correctly.
 */
pub type FT_Glyph_Format_ = libc::c_uint;
pub const FT_GLYPH_FORMAT_PLOTTER: FT_Glyph_Format_ = 1886154612;
pub const FT_GLYPH_FORMAT_OUTLINE: FT_Glyph_Format_ = 1869968492;
pub const FT_GLYPH_FORMAT_BITMAP: FT_Glyph_Format_ = 1651078259;
pub const FT_GLYPH_FORMAT_COMPOSITE: FT_Glyph_Format_ = 1668246896;
pub const FT_GLYPH_FORMAT_NONE: FT_Glyph_Format_ = 0;
pub type FT_Glyph_Format = FT_Glyph_Format_;
/* *************************************************************************
 *
 * @type:
 *   FT_Byte
 *
 * @description:
 *   A simple typedef for the _unsigned_ char type.
 */
pub type FT_Byte = libc::c_uchar;
/* *************************************************************************
 *
 * @type:
 *   FT_String
 *
 * @description:
 *   A simple typedef for the char type, usually used for strings.
 */
pub type FT_String = libc::c_char;
/* *************************************************************************
 *
 * @type:
 *   FT_Short
 *
 * @description:
 *   A typedef for signed short.
 */
pub type FT_Short = libc::c_short;
/* *************************************************************************
 *
 * @type:
 *   FT_UShort
 *
 * @description:
 *   A typedef for unsigned short.
 */
pub type FT_UShort = libc::c_ushort;
/* *************************************************************************
 *
 * @type:
 *   FT_Int
 *
 * @description:
 *   A typedef for the int type.
 */
pub type FT_Int = libc::c_int;
/* *************************************************************************
 *
 * @type:
 *   FT_UInt
 *
 * @description:
 *   A typedef for the unsigned int type.
 */
pub type FT_UInt = libc::c_uint;
/* *************************************************************************
 *
 * @type:
 *   FT_Long
 *
 * @description:
 *   A typedef for signed long.
 */
pub type FT_Long = libc::c_long;
/* *************************************************************************
 *
 * @type:
 *   FT_Fixed
 *
 * @description:
 *   This type is used to store 16.16 fixed-point values, like scaling
 *   values or matrix coefficients.
 */
pub type FT_Fixed = libc::c_long;
/* *************************************************************************
 *
 * @functype:
 *   FT_Generic_Finalizer
 *
 * @description:
 *   Describe a function used to destroy the 'client' data of any FreeType
 *   object.  See the description of the @FT_Generic type for details of
 *   usage.
 *
 * @input:
 *   The address of the FreeType object that is under finalization.  Its
 *   client data is accessed through its `generic` field.
 */
pub type FT_Generic_Finalizer = Option<unsafe fn(_: *mut libc::c_void) -> ()>;
/* *************************************************************************
 *
 * @struct:
 *   FT_Generic
 *
 * @description:
 *   Client applications often need to associate their own data to a
 *   variety of FreeType core objects.  For example, a text layout API
 *   might want to associate a glyph cache to a given size object.
 *
 *   Some FreeType object contains a `generic` field, of type `FT_Generic`,
 *   which usage is left to client applications and font servers.
 *
 *   It can be used to store a pointer to client-specific data, as well as
 *   the address of a 'finalizer' function, which will be called by
 *   FreeType when the object is destroyed (for example, the previous
 *   client example would put the address of the glyph cache destructor in
 *   the `finalizer` field).
 *
 * @fields:
 *   data ::
 *     A typeless pointer to any client-specified data. This field is
 *     completely ignored by the FreeType library.
 *
 *   finalizer ::
 *     A pointer to a 'generic finalizer' function, which will be called
 *     when the object is destroyed.  If this field is set to `NULL`, no
 *     code will be called.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Generic_ {
    pub data: *mut libc::c_void,
    pub finalizer: FT_Generic_Finalizer,
}
pub type FT_Generic = FT_Generic_;
/* *************************************************************************
 *
 * @macro:
 *   FT_MAKE_TAG
 *
 * @description:
 *   This macro converts four-letter tags that are used to label TrueType
 *   tables into an unsigned long, to be used within FreeType.
 *
 * @note:
 *   The produced values **must** be 32-bit integers.  Don't redefine this
 *   macro.
 */
/* ************************************************************************/
/* ************************************************************************/
/*                                                                       */
/*                    L I S T   M A N A G E M E N T                      */
/*                                                                       */
/* ************************************************************************/
/* ************************************************************************/
/* *************************************************************************
 *
 * @section:
 *   list_processing
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_ListNode
 *
 * @description:
 *    Many elements and objects in FreeType are listed through an @FT_List
 *    record (see @FT_ListRec).  As its name suggests, an FT_ListNode is a
 *    handle to a single list element.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_ListNodeRec_ {
    pub prev: FT_ListNode,
    pub next: FT_ListNode,
    pub data: *mut libc::c_void,
}
pub type FT_ListNode = *mut FT_ListNodeRec_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_ListRec_ {
    pub head: FT_ListNode,
    pub tail: FT_ListNode,
}
pub type FT_ListRec = FT_ListRec_;
/* ***************************************************************************
 *
 * freetype.h
 *
 *   FreeType high-level API and common types (specification only).
 *
 * Copyright (C) 1996-2019 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
 */
/* *************************************************************************
 *
 * @section:
 *   header_inclusion
 *
 * @title:
 *   FreeType's header inclusion scheme
 *
 * @abstract:
 *   How client applications should include FreeType header files.
 *
 * @description:
 *   To be as flexible as possible (and for historical reasons), FreeType
 *   uses a very special inclusion scheme to load header files, for example
 *
 *   ```
 *     #include <ft2build.h>
 *
 *     #include FT_FREETYPE_H
 *     #include FT_OUTLINE_H
 *   ```
 *
 *   A compiler and its preprocessor only needs an include path to find the
 *   file `ft2build.h`; the exact locations and names of the other FreeType
 *   header files are hidden by @header_file_macros, loaded by
 *   `ft2build.h`.  The API documentation always gives the header macro
 *   name needed for a particular function.
 *
 */
/* *************************************************************************
 *
 * @section:
 *   user_allocation
 *
 * @title:
 *   User allocation
 *
 * @abstract:
 *   How client applications should allocate FreeType data structures.
 *
 * @description:
 *   FreeType assumes that structures allocated by the user and passed as
 *   arguments are zeroed out except for the actual data.  In other words,
 *   it is recommended to use `calloc` (or variants of it) instead of
 *   `malloc` for allocation.
 *
 */
/* ************************************************************************/
/* ************************************************************************/
/*                                                                       */
/*                        B A S I C   T Y P E S                          */
/*                                                                       */
/* ************************************************************************/
/* ************************************************************************/
/* *************************************************************************
 *
 * @section:
 *   base_interface
 *
 * @title:
 *   Base Interface
 *
 * @abstract:
 *   The FreeType~2 base font interface.
 *
 * @description:
 *   This section describes the most important public high-level API
 *   functions of FreeType~2.
 *
 * @order:
 *   FT_Library
 *   FT_Face
 *   FT_Size
 *   FT_GlyphSlot
 *   FT_CharMap
 *   FT_Encoding
 *   FT_ENC_TAG
 *
 *   FT_FaceRec
 *
 *   FT_FACE_FLAG_SCALABLE
 *   FT_FACE_FLAG_FIXED_SIZES
 *   FT_FACE_FLAG_FIXED_WIDTH
 *   FT_FACE_FLAG_HORIZONTAL
 *   FT_FACE_FLAG_VERTICAL
 *   FT_FACE_FLAG_COLOR
 *   FT_FACE_FLAG_SFNT
 *   FT_FACE_FLAG_CID_KEYED
 *   FT_FACE_FLAG_TRICKY
 *   FT_FACE_FLAG_KERNING
 *   FT_FACE_FLAG_MULTIPLE_MASTERS
 *   FT_FACE_FLAG_VARIATION
 *   FT_FACE_FLAG_GLYPH_NAMES
 *   FT_FACE_FLAG_EXTERNAL_STREAM
 *   FT_FACE_FLAG_HINTER
 *
 *   FT_HAS_HORIZONTAL
 *   FT_HAS_VERTICAL
 *   FT_HAS_KERNING
 *   FT_HAS_FIXED_SIZES
 *   FT_HAS_GLYPH_NAMES
 *   FT_HAS_COLOR
 *   FT_HAS_MULTIPLE_MASTERS
 *
 *   FT_IS_SFNT
 *   FT_IS_SCALABLE
 *   FT_IS_FIXED_WIDTH
 *   FT_IS_CID_KEYED
 *   FT_IS_TRICKY
 *   FT_IS_NAMED_INSTANCE
 *   FT_IS_VARIATION
 *
 *   FT_STYLE_FLAG_BOLD
 *   FT_STYLE_FLAG_ITALIC
 *
 *   FT_SizeRec
 *   FT_Size_Metrics
 *
 *   FT_GlyphSlotRec
 *   FT_Glyph_Metrics
 *   FT_SubGlyph
 *
 *   FT_Bitmap_Size
 *
 *   FT_Init_FreeType
*   FT_Done_FreeType
*
*   FT_New_Face
*   FT_Done_Face
*   FT_Reference_Face
*   FT_New_Memory_Face
*   FT_Face_Properties
*   FT_Open_Face
*   FT_Open_Args
*   FT_Parameter
*   FT_Attach_File
*   FT_Attach_Stream
*
*   FT_Set_Char_Size
*   FT_Set_Pixel_Sizes
*   FT_Request_Size
*   FT_Select_Size
*   FT_Size_Request_Type
*   FT_Size_RequestRec
*   FT_Size_Request
*   FT_Set_Transform
*   FT_Load_Glyph
*   FT_Get_Char_Index
*   FT_Get_First_Char
*   FT_Get_Next_Char
*   FT_Get_Name_Index
*   FT_Load_Char
*
*   FT_OPEN_MEMORY
*   FT_OPEN_STREAM
*   FT_OPEN_PATHNAME
*   FT_OPEN_DRIVER
*   FT_OPEN_PARAMS
*
*   FT_LOAD_DEFAULT
*   FT_LOAD_RENDER
*   FT_LOAD_MONOCHROME
*   FT_LOAD_LINEAR_DESIGN
*   FT_LOAD_NO_SCALE
*   FT_LOAD_NO_HINTING
*   FT_LOAD_NO_BITMAP
*   FT_LOAD_NO_AUTOHINT
*   FT_LOAD_COLOR
*
*   FT_LOAD_VERTICAL_LAYOUT
*   FT_LOAD_IGNORE_TRANSFORM
*   FT_LOAD_FORCE_AUTOHINT
*   FT_LOAD_NO_RECURSE
*   FT_LOAD_PEDANTIC
*
*   FT_LOAD_TARGET_NORMAL
*   FT_LOAD_TARGET_LIGHT
*   FT_LOAD_TARGET_MONO
*   FT_LOAD_TARGET_LCD
*   FT_LOAD_TARGET_LCD_V
*
*   FT_LOAD_TARGET_MODE
*
*   FT_Render_Glyph
*   FT_Render_Mode
*   FT_Get_Kerning
*   FT_Kerning_Mode
*   FT_Get_Track_Kerning
*   FT_Get_Glyph_Name
*   FT_Get_Postscript_Name
*
*   FT_CharMapRec
*   FT_Select_Charmap
*   FT_Set_Charmap
*   FT_Get_Charmap_Index
*
*   FT_Get_FSType_Flags
*   FT_Get_SubGlyph_Info
*
*   FT_Face_Internal
*   FT_Size_Internal
*   FT_Slot_Internal
*
*   FT_FACE_FLAG_XXX
*   FT_STYLE_FLAG_XXX
*   FT_OPEN_XXX
*   FT_LOAD_XXX
*   FT_LOAD_TARGET_XXX
*   FT_SUBGLYPH_FLAG_XXX
*   FT_FSTYPE_XXX
*
*   FT_HAS_FAST_GLYPHS
*
*/
/* *************************************************************************
 *
 * @struct:
 *   FT_Glyph_Metrics
 *
 * @description:
 *   A structure to model the metrics of a single glyph.  The values are
 *   expressed in 26.6 fractional pixel format; if the flag
 *   @FT_LOAD_NO_SCALE has been used while loading the glyph, values are
 *   expressed in font units instead.
 *
 * @fields:
 *   width ::
 *     The glyph's width.
 *
 *   height ::
 *     The glyph's height.
 *
 *   horiBearingX ::
 *     Left side bearing for horizontal layout.
 *
 *   horiBearingY ::
 *     Top side bearing for horizontal layout.
 *
 *   horiAdvance ::
 *     Advance width for horizontal layout.
 *
 *   vertBearingX ::
 *     Left side bearing for vertical layout.
 *
 *   vertBearingY ::
 *     Top side bearing for vertical layout.  Larger positive values mean
 *     further below the vertical glyph origin.
 *
 *   vertAdvance ::
 *     Advance height for vertical layout.  Positive values mean the glyph
 *     has a positive advance downward.
 *
 * @note:
 *   If not disabled with @FT_LOAD_NO_HINTING, the values represent
 *   dimensions of the hinted glyph (in case hinting is applicable).
 *
 *   Stroking a glyph with an outside border does not increase
 *   `horiAdvance` or `vertAdvance`; you have to manually adjust these
 *   values to account for the added width and height.
 *
 *   FreeType doesn't use the 'VORG' table data for CFF fonts because it
 *   doesn't have an interface to quickly retrieve the glyph height.  The
 *   y~coordinate of the vertical origin can be simply computed as
 *   `vertBearingY + height` after loading a glyph.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Glyph_Metrics_ {
    pub width: FT_Pos,
    pub height: FT_Pos,
    pub horiBearingX: FT_Pos,
    pub horiBearingY: FT_Pos,
    pub horiAdvance: FT_Pos,
    pub vertBearingX: FT_Pos,
    pub vertBearingY: FT_Pos,
    pub vertAdvance: FT_Pos,
}
pub type FT_Glyph_Metrics = FT_Glyph_Metrics_;
/* *************************************************************************
 *
 * @struct:
 *   FT_Bitmap_Size
 *
 * @description:
 *   This structure models the metrics of a bitmap strike (i.e., a set of
 *   glyphs for a given point size and resolution) in a bitmap font.  It is
 *   used for the `available_sizes` field of @FT_Face.
 *
 * @fields:
 *   height ::
 *     The vertical distance, in pixels, between two consecutive baselines.
 *     It is always positive.
 *
 *   width ::
 *     The average width, in pixels, of all glyphs in the strike.
 *
 *   size ::
 *     The nominal size of the strike in 26.6 fractional points.  This
 *     field is not very useful.
 *
 *   x_ppem ::
 *     The horizontal ppem (nominal width) in 26.6 fractional pixels.
 *
 *   y_ppem ::
 *     The vertical ppem (nominal height) in 26.6 fractional pixels.
 *
 * @note:
 *   Windows FNT:
 *     The nominal size given in a FNT font is not reliable.  If the driver
 *     finds it incorrect, it sets `size` to some calculated values, and
 *     `x_ppem` and `y_ppem` to the pixel width and height given in the
 *     font, respectively.
 *
 *   TrueType embedded bitmaps:
 *     `size`, `width`, and `height` values are not contained in the bitmap
 *     strike itself.  They are computed from the global font parameters.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Bitmap_Size_ {
    pub height: FT_Short,
    pub width: FT_Short,
    pub size: FT_Pos,
    pub x_ppem: FT_Pos,
    pub y_ppem: FT_Pos,
}
pub type FT_Bitmap_Size = FT_Bitmap_Size_;
pub type FT_Library = *mut FT_LibraryRec_;
pub type FT_Driver = *mut FT_DriverRec_;
/* *************************************************************************
 *
 * @section:
 *   base_interface
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Face
 *
 * @description:
 *   A handle to a typographic face object.  A face object models a given
 *   typeface, in a given style.
 *
 * @note:
 *   A face object also owns a single @FT_GlyphSlot object, as well as one
 *   or more @FT_Size objects.
 *
 *   Use @FT_New_Face or @FT_Open_Face to create a new face object from a
 *   given filepath or a custom input stream.
 *
 *   Use @FT_Done_Face to destroy it (along with its slot and sizes).
 *
 *   An `FT_Face` object can only be safely used from one thread at a time.
 *   Similarly, creation and destruction of `FT_Face` with the same
 *   @FT_Library object can only be done from one thread at a time.  On the
 *   other hand, functions like @FT_Load_Glyph and its siblings are
 *   thread-safe and do not need the lock to be held as long as the same
 *   `FT_Face` object is not used from multiple threads at the same time.
 *
 * @also:
 *   See @FT_FaceRec for the publicly accessible fields of a given face
 *   object.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_FaceRec_ {
    pub num_faces: FT_Long,
    pub face_index: FT_Long,
    pub face_flags: FT_Long,
    pub style_flags: FT_Long,
    pub num_glyphs: FT_Long,
    pub family_name: *mut FT_String,
    pub style_name: *mut FT_String,
    pub num_fixed_sizes: FT_Int,
    pub available_sizes: *mut FT_Bitmap_Size,
    pub num_charmaps: FT_Int,
    pub charmaps: *mut FT_CharMap,
    pub generic: FT_Generic,
    pub bbox: FT_BBox,
    pub units_per_EM: FT_UShort,
    pub ascender: FT_Short,
    pub descender: FT_Short,
    pub height: FT_Short,
    pub max_advance_width: FT_Short,
    pub max_advance_height: FT_Short,
    pub underline_position: FT_Short,
    pub underline_thickness: FT_Short,
    pub glyph: FT_GlyphSlot,
    pub size: FT_Size,
    pub charmap: FT_CharMap,
    pub driver: FT_Driver,
    pub memory: FT_Memory,
    pub stream: FT_Stream,
    pub sizes_list: FT_ListRec,
    pub autohint: FT_Generic,
    pub extensions: *mut libc::c_void,
    pub internal: FT_Face_Internal,
}
pub type FT_Face_Internal = *mut FT_Face_InternalRec_;
pub type FT_CharMap = *mut FT_CharMapRec_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_CharMapRec_ {
    pub face: FT_Face,
    pub encoding: FT_Encoding,
    pub platform_id: FT_UShort,
    pub encoding_id: FT_UShort,
}
pub type FT_Encoding = FT_Encoding_;
pub type FT_Encoding_ = libc::c_uint;
pub const FT_ENCODING_APPLE_ROMAN: FT_Encoding_ = 1634889070;
pub const FT_ENCODING_OLD_LATIN_2: FT_Encoding_ = 1818326066;
pub const FT_ENCODING_ADOBE_LATIN_1: FT_Encoding_ = 1818326065;
pub const FT_ENCODING_ADOBE_CUSTOM: FT_Encoding_ = 1094992451;
pub const FT_ENCODING_ADOBE_EXPERT: FT_Encoding_ = 1094992453;
pub const FT_ENCODING_ADOBE_STANDARD: FT_Encoding_ = 1094995778;
pub const FT_ENCODING_MS_JOHAB: FT_Encoding_ = 1785686113;
pub const FT_ENCODING_MS_WANSUNG: FT_Encoding_ = 2002873971;
pub const FT_ENCODING_MS_BIG5: FT_Encoding_ = 1651074869;
pub const FT_ENCODING_MS_GB2312: FT_Encoding_ = 1734484000;
pub const FT_ENCODING_MS_SJIS: FT_Encoding_ = 1936353651;
pub const FT_ENCODING_GB2312: FT_Encoding_ = 1734484000;
pub const FT_ENCODING_JOHAB: FT_Encoding_ = 1785686113;
pub const FT_ENCODING_WANSUNG: FT_Encoding_ = 2002873971;
pub const FT_ENCODING_BIG5: FT_Encoding_ = 1651074869;
pub const FT_ENCODING_PRC: FT_Encoding_ = 1734484000;
pub const FT_ENCODING_SJIS: FT_Encoding_ = 1936353651;
pub const FT_ENCODING_UNICODE: FT_Encoding_ = 1970170211;
pub const FT_ENCODING_MS_SYMBOL: FT_Encoding_ = 1937337698;
pub const FT_ENCODING_NONE: FT_Encoding_ = 0;
pub type FT_Face = *mut FT_FaceRec_;
pub type FT_Size = *mut FT_SizeRec_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_SizeRec_ {
    pub face: FT_Face,
    pub generic: FT_Generic,
    pub metrics: FT_Size_Metrics,
    pub internal: FT_Size_Internal,
}
pub type FT_Size_Internal = *mut FT_Size_InternalRec_;
pub type FT_Size_Metrics = FT_Size_Metrics_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_Size_Metrics_ {
    pub x_ppem: FT_UShort,
    pub y_ppem: FT_UShort,
    pub x_scale: FT_Fixed,
    pub y_scale: FT_Fixed,
    pub ascender: FT_Pos,
    pub descender: FT_Pos,
    pub height: FT_Pos,
    pub max_advance: FT_Pos,
}
pub type FT_GlyphSlot = *mut FT_GlyphSlotRec_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_GlyphSlotRec_ {
    pub library: FT_Library,
    pub face: FT_Face,
    pub next: FT_GlyphSlot,
    pub glyph_index: FT_UInt,
    pub generic: FT_Generic,
    pub metrics: FT_Glyph_Metrics,
    pub linearHoriAdvance: FT_Fixed,
    pub linearVertAdvance: FT_Fixed,
    pub advance: FT_Vector,
    pub format: FT_Glyph_Format,
    pub bitmap: FT_Bitmap,
    pub bitmap_left: FT_Int,
    pub bitmap_top: FT_Int,
    pub outline: FT_Outline,
    pub num_subglyphs: FT_UInt,
    pub subglyphs: FT_SubGlyph,
    pub control_data: *mut libc::c_void,
    pub control_len: libc::c_long,
    pub lsb_delta: FT_Pos,
    pub rsb_delta: FT_Pos,
    pub other: *mut libc::c_void,
    pub internal: FT_Slot_Internal,
}
pub type FT_Slot_Internal = *mut FT_Slot_InternalRec_;
pub type FT_SubGlyph = *mut FT_SubGlyphRec_;

pub type hb_unicode_decompose_compatibility_func_t = Option<
    unsafe fn(
        _: *mut hb_unicode_funcs_t,
        _: hb_codepoint_t,
        _: *mut hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> libc::c_uint,
>;

pub type OTTag = uint32_t;
pub type GlyphID = uint16_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXLayoutEngine_rec {
    pub font: *mut XeTeXFontInst,
    pub fontRef: PlatformFontRef,
    pub script: hb_tag_t,
    pub language: hb_language_t,
    pub features: *mut hb_feature_t,
    pub ShaperList: *mut *mut libc::c_char,
    pub shaper: *mut libc::c_char,
    pub nFeatures: libc::c_int,
    pub rgbValue: uint32_t,
    pub extend: f32,
    pub slant: f32,
    pub embolden: f32,
    pub hbBuffer: *mut hb_buffer_t,
}

pub type XeTeXLayoutEngine = *mut XeTeXLayoutEngine_rec;
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
    return (*self_0).m_filename;
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
pub unsafe fn createFont(mut fontRef: PlatformFontRef, mut pointSize: Fixed) -> PlatformFontRef {
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
            Fix2D(pointSize) as f32,
            &mut status,
        );
    }
    #[cfg(target_os = "macos")]
    {
        let mac_font = XeTeXFontInst_Mac_create(fontRef, Fix2D(pointSize) as f32, &mut status);
        font = &mut (*mac_font).super_;
    }
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return 0 as PlatformFontRef;
    }
    return font as PlatformFontRef;
}

#[no_mangle]
pub unsafe fn createFontFromFile(
    mut filename: *const libc::c_char,
    mut index: libc::c_int,
    mut pointSize: Fixed,
) -> PlatformFontRef {
    let mut status: libc::c_int = 0i32;
    let mut font: *mut XeTeXFontInst =
        XeTeXFontInst_create(filename, index, Fix2D(pointSize) as f32, &mut status);
    if status != 0i32 {
        XeTeXFontInst_delete(font);
        return 0 as PlatformFontRef;
    }
    return font as PlatformFontRef;
}
#[no_mangle]
pub unsafe fn setFontLayoutDir(mut font: PlatformFontRef, mut vertical: libc::c_int) {
    XeTeXFontInst_setLayoutDirVertical(font as *mut XeTeXFontInst, vertical != 0i32);
}
#[no_mangle]
pub unsafe fn findFontByName(
    mut name: *const libc::c_char,
    mut var: *mut libc::c_char,
    mut size: f64,
) -> PlatformFontRef {
    return XeTeXFontMgr_findFont(XeTeXFontMgr_GetFontManager(), name, var, size);
}
#[no_mangle]
pub unsafe fn getReqEngine() -> libc::c_char {
    return XeTeXFontMgr_getReqEngine(XeTeXFontMgr_GetFontManager());
}
#[no_mangle]
pub unsafe fn setReqEngine(mut reqEngine: libc::c_char) {
    XeTeXFontMgr_setReqEngine(XeTeXFontMgr_GetFontManager(), reqEngine);
}
#[no_mangle]
pub unsafe fn getFullName(mut fontRef: PlatformFontRef) -> *const libc::c_char {
    return XeTeXFontMgr_getFullName(XeTeXFontMgr_GetFontManager(), fontRef);
}
#[no_mangle]
pub unsafe fn getDesignSize(mut font: PlatformFontRef) -> f64 {
    return XeTeXFontMgr_getDesignSize(XeTeXFontMgr_GetFontManager(), font);
}
#[no_mangle]
pub unsafe fn getFontFilename(
    mut engine: XeTeXLayoutEngine,
    mut index: *mut uint32_t,
) -> *mut libc::c_char {
    return xstrdup(XeTeXFontInst_getFilename((*engine).font, index));
}
#[no_mangle]
pub unsafe fn getFontRef(mut engine: XeTeXLayoutEngine) -> PlatformFontRef {
    return (*engine).fontRef;
}
#[no_mangle]
pub unsafe fn deleteFont(mut font: PlatformFontRef) {
    XeTeXFontInst_delete(font as *mut XeTeXFontInst);
}
#[no_mangle]
pub unsafe fn getFontTablePtr(mut font: PlatformFontRef, mut tableTag: uint32_t) -> *mut libc::c_void {
    return XeTeXFontInst_getFontTable(font as *mut XeTeXFontInst, tableTag);
}
#[no_mangle]
pub unsafe fn getSlant(mut font: PlatformFontRef) -> Fixed {
    let mut italAngle: f32 = XeTeXFontInst_getItalicAngle(font as *mut XeTeXFontInst);
    let radians = -italAngle as f64 * std::f64::consts::PI / 180.0f64;
    return D2Fix(radians.tan());
}

unsafe fn getLargerScriptListTable(
    mut font: PlatformFontRef,
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
pub unsafe fn countScripts(mut font: PlatformFontRef) -> libc::c_uint {
    return getLargerScriptListTable(font, 0 as *mut *mut hb_tag_t);
}
#[no_mangle]
pub unsafe fn getIndScript(mut font: PlatformFontRef, mut index: libc::c_uint) -> hb_tag_t {
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
pub unsafe fn countLanguages(mut font: PlatformFontRef, mut script: hb_tag_t) -> libc::c_uint {
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
pub unsafe fn getIndLanguage(
    mut font: PlatformFontRef,
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
pub unsafe fn countFeatures(
    mut font: PlatformFontRef,
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
pub unsafe fn getIndFeature(
    mut font: PlatformFontRef,
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
pub unsafe fn countGraphiteFeatures(mut engine: XeTeXLayoutEngine) -> uint32_t {
    let mut rval: uint32_t = 0i32 as uint32_t;
    let mut hbFace: *mut hb_face_t = hb_font_get_face(XeTeXFontInst_getHbFont((*engine).font));
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
pub unsafe fn getGraphiteFeatureDefaultSetting(
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
pub unsafe fn getGraphiteFeatureLabel(
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
pub unsafe fn getGraphiteFeatureSettingLabel(
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
pub unsafe fn findGraphiteFeature(
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
pub unsafe fn getGlyphWidth(mut font: PlatformFontRef, mut gid: uint32_t) -> f32 {
    return XeTeXFontInst_getGlyphWidth(font as *mut XeTeXFontInst, gid as GlyphID);
}
#[no_mangle]
pub unsafe fn countGlyphs(mut font: PlatformFontRef) -> libc::c_uint {
    return XeTeXFontInst_getNumGlyphs(font as *mut XeTeXFontInst) as libc::c_uint;
}
#[no_mangle]
pub unsafe fn getFont(mut engine: XeTeXLayoutEngine) -> PlatformFontRef {
    return (*engine).font as PlatformFontRef;
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
pub unsafe fn XeTeXLayoutEngine_create() -> *mut XeTeXLayoutEngine_rec {
    return malloc(::std::mem::size_of::<XeTeXLayoutEngine_rec>() as libc::c_ulong)
        as *mut XeTeXLayoutEngine_rec;
}
#[no_mangle]
pub unsafe fn XeTeXLayoutEngine_delete(mut engine: *mut XeTeXLayoutEngine_rec) {
    free(engine as *mut libc::c_void);
}
#[no_mangle]
pub unsafe fn createLayoutEngine(
    mut fontRef: PlatformFontRef,
    mut font: PlatformFontRef,
    mut script: hb_tag_t,
    mut language: *mut libc::c_char,
    mut features: *mut hb_feature_t,
    mut nFeatures: libc::c_int,
    mut shapers: *mut *mut libc::c_char,
    mut rgbValue: uint32_t,
    mut extend: f32,
    mut slant: f32,
    mut embolden: f32,
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
pub unsafe fn deleteLayoutEngine(mut engine: XeTeXLayoutEngine) {
    hb_buffer_destroy((*engine).hbBuffer);
    XeTeXFontInst_delete((*engine).font);
    free((*engine).shaper as *mut libc::c_void);
    XeTeXLayoutEngine_delete(engine);
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
#[no_mangle]
pub unsafe fn layoutChars(
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
        language: 0 as *const hb_language_impl_t,
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
            panic!("all shapers failed");
        }
    }
    hb_shape_plan_destroy(shape_plan);
    let mut glyphCount: libc::c_int = hb_buffer_get_length((*engine).hbBuffer) as libc::c_int;
    return glyphCount;
}
#[no_mangle]
pub unsafe fn getGlyphs(mut engine: XeTeXLayoutEngine, mut glyphs: *mut uint32_t) {
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
pub unsafe fn getGlyphAdvances(mut engine: XeTeXLayoutEngine, mut advances: *mut f32) {
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
pub unsafe fn getGlyphPositions(mut engine: XeTeXLayoutEngine, mut positions: *mut FloatPoint) {
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
pub unsafe fn getDefaultDirection(mut engine: XeTeXLayoutEngine) -> libc::c_int {
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
pub unsafe fn getGlyphBounds(
    engine: XeTeXLayoutEngine,
    glyphID: uint32_t,
    bbox: *mut GlyphBBox,
) {
    let font_info = &*((*engine).font);

    // TODO: xetex_font_info uses u16 (why??????), but glyph IDs should be u32
    std::ptr::write(bbox, font_info.get_glyph_bounds(glyphID as u16));

    if (*engine).extend as f64 != 0.0f64 {
        (*bbox).xMin *= (*engine).extend;
        (*bbox).xMax *= (*engine).extend
    };
}
#[no_mangle]
pub unsafe fn getGlyphWidthFromEngine(mut engine: XeTeXLayoutEngine, mut glyphID: uint32_t) -> f32 {
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
    mut lsb: *mut f32,
    mut rsb: *mut f32,
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
    mut font: PlatformFontRef,
    mut gid: uint16_t,
    mut len: *mut libc::c_int,
) -> *const libc::c_char {
    return XeTeXFontInst_getGlyphName(font as *mut XeTeXFontInst, gid, len);
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
