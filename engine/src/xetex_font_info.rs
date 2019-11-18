#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use std::ptr;

use freetype::freetype::{
    FT_Attach_Stream, FT_BBox, FT_Byte, FT_Encoding, FT_Error, FT_Face,
    FT_Face_GetCharVariantIndex, FT_Fixed, FT_Generic, FT_Generic_Finalizer, FT_Get_Char_Index,
    FT_Get_First_Char, FT_Get_Glyph_Name, FT_Get_Kerning, FT_Get_Name_Index, FT_Get_Next_Char,
    FT_Get_Sfnt_Table, FT_GlyphSlot, FT_Glyph_Format, FT_Glyph_Metrics, FT_Int, FT_Int32,
    FT_Library, FT_Load_Glyph, FT_Load_Sfnt_Table, FT_Long, FT_Memory, FT_MemoryRec_, FT_Open_Args,
    FT_Parameter, FT_Pointer, FT_Pos, FT_Sfnt_Tag, FT_Short, FT_Size, FT_Stream, FT_String,
    FT_UInt, FT_ULong, FT_Vector,
};
use freetype::tt_os2::TT_OS2;

use freetype_missing::*;

mod freetype_missing {

    use freetype::freetype::{FT_ULong, FT_Short, FT_Fixed, FT_Int32, FT_UInt, FT_Face, FT_Error};

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct TT_Postscript_ {
        pub FormatType: FT_Fixed,
        pub italicAngle: FT_Fixed,
        pub underlinePosition: FT_Short,
        pub underlineThickness: FT_Short,
        pub isFixedPitch: FT_ULong,
        pub minMemType42: FT_ULong,
        pub maxMemType42: FT_ULong,
        pub minMemType1: FT_ULong,
        pub maxMemType1: FT_ULong,
    }
    pub type TT_Postscript = TT_Postscript_;

    extern "C" {

        #[no_mangle]
        pub fn FT_Get_Advance(
            face: FT_Face,
            gindex: FT_UInt,
            load_flags: FT_Int32,
            padvance: *mut FT_Fixed,
        ) -> FT_Error;

    }
}

use crate::core_memory::xmalloc;
use harfbuzz_sys::{
    hb_blob_create, hb_blob_t, hb_bool_t, hb_codepoint_t, hb_destroy_func_t,
    hb_face_create_for_tables, hb_face_destroy, hb_face_set_index, hb_face_set_upem, hb_face_t,
    hb_font_create, hb_font_destroy, hb_font_funcs_create,
    hb_font_funcs_set_glyph_contour_point_func, hb_font_funcs_set_glyph_extents_func,
    hb_font_funcs_set_glyph_h_advance_func, hb_font_funcs_set_glyph_h_origin_func,
    hb_font_funcs_set_glyph_name_func, hb_font_funcs_set_glyph_v_advance_func,
    hb_font_funcs_set_glyph_v_origin_func, hb_font_funcs_t, hb_font_set_funcs, hb_font_set_ppem,
    hb_font_set_scale, hb_font_t, hb_glyph_extents_t, hb_position_t, hb_tag_t,
    HB_MEMORY_MODE_WRITABLE,
};

use crate::{
    ttstub_input_close, ttstub_input_get_size, ttstub_input_getc, ttstub_input_open,
    ttstub_input_read_exact,
};

use crate::xetex_ext::Fix2D;

use bridge::TTInputFormat;

#[cfg(not(target_os = "macos"))]
mod imp {}

#[cfg(target_os = "macos")]
#[path = "xetex_font_info_coretext.rs"]
mod imp;

#[cfg(target_os = "macos")]
pub use imp::{XeTeXFontInst_Mac, XeTeXFontInst_Mac_create};

extern crate libc;
extern "C" {
    pub type FT_ModuleRec_;
    pub type FT_DriverRec_;
    pub type FT_Face_InternalRec_;
    pub type FT_Size_InternalRec_;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn strcpy(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strrchr(_: *const libc::c_char, _: libc::c_int) -> *mut libc::c_char;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    /* tectonic/core-memory.h: basic dynamic memory helpers
       Copyright 2016-2018 the Tectonic Project
       Licensed under the MIT License.
    */
    #[no_mangle]
    fn xstrdup(s: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn tolower(_: libc::c_int) -> libc::c_int;

    // TODO: NOTE: this api doesn't included in harfbuzz_sys
    #[no_mangle]
    fn hb_font_funcs_set_glyph_h_kerning_func(
        ffuncs: *mut hb_font_funcs_t,
        func: hb_font_get_glyph_h_kerning_func_t,
        user_data: *mut libc::c_void,
        destroy: hb_destroy_func_t,
    );

    #[no_mangle]
    fn hb_font_funcs_set_glyph_func(
        ffuncs: *mut hb_font_funcs_t,
        func: hb_font_get_glyph_func_t,
        user_data: *mut libc::c_void,
        destroy: hb_destroy_func_t,
    );

    #[no_mangle]
    fn hb_font_funcs_set_glyph_v_kerning_func(
        ffuncs: *mut hb_font_funcs_t,
        func: hb_font_get_glyph_v_kerning_func_t,
        user_data: *mut libc::c_void,
        destroy: hb_destroy_func_t,
    );
}

pub type size_t = usize;
pub type int32_t = i32;
pub type uint16_t = u16;
pub type uint32_t = u32;
pub type ssize_t = isize;

use bridge::InputHandleWrapper;
pub type UChar32 = int32_t;

/* *************************************************************************
 *
 * @enum:
 *   FT_Kerning_Mode
 *
 * @description:
 *   An enumeration to specify the format of kerning values returned by
 *   @FT_Get_Kerning.
 *
 * @values:
 *   FT_KERNING_DEFAULT ::
 *     Return grid-fitted kerning distances in 26.6 fractional pixels.
 *
 *   FT_KERNING_UNFITTED ::
 *     Return un-grid-fitted kerning distances in 26.6 fractional pixels.
 *
 *   FT_KERNING_UNSCALED ::
 *     Return the kerning vector in original font units.
 *
 * @note:
 *   `FT_KERNING_DEFAULT` returns full pixel values; it also makes FreeType
 *   heuristically scale down kerning distances at small ppem values so
 *   that they don't become too big.
 *
 *   Both `FT_KERNING_DEFAULT` and `FT_KERNING_UNFITTED` use the current
 *   horizontal scaling factor (as set e.g. with @FT_Set_Char_Size) to
 *   convert font units to pixels.
 */

pub type FT_Kerning_Mode_ = libc::c_uint;
pub const FT_KERNING_UNSCALED: FT_Kerning_Mode_ = 2;
pub const FT_KERNING_UNFITTED: FT_Kerning_Mode_ = 1;
pub const FT_KERNING_DEFAULT: FT_Kerning_Mode_ = 0;

pub type hb_font_get_glyph_kerning_func_t = Option<
    unsafe extern "C" fn(
        _: *mut hb_font_t,
        _: *mut libc::c_void,
        _: hb_codepoint_t,
        _: hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> hb_position_t,
>;
pub type hb_font_get_glyph_h_kerning_func_t = hb_font_get_glyph_kerning_func_t;
pub type hb_font_get_glyph_func_t = Option<
    unsafe extern "C" fn(
        _: *mut hb_font_t,
        _: *mut libc::c_void,
        _: hb_codepoint_t,
        _: hb_codepoint_t,
        _: *mut hb_codepoint_t,
        _: *mut libc::c_void,
    ) -> hb_bool_t,
>;
pub type hb_font_get_glyph_v_kerning_func_t = hb_font_get_glyph_kerning_func_t;

pub type OTTag = uint32_t;
pub type GlyphID = uint16_t;

pub type Fixed = i32;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct GlyphBBox {
    pub xMin: libc::c_float,
    pub yMin: libc::c_float,
    pub xMax: libc::c_float,
    pub yMax: libc::c_float,
}

impl GlyphBBox {
    pub fn zero() -> Self {
        GlyphBBox {
            xMin: 0.0,
            yMin: 0.0,
            xMax: 0.0,
            yMax: 0.0,
        }
    }
}

use font_kit::loaders::default::{Font, NativeFont};

#[derive(Clone)]
#[repr(C)]
pub struct XeTeXFontInst {
    pub m_unitsPerEM: libc::c_ushort,
    pub m_pointSize: libc::c_float,
    pub m_ascent: libc::c_float,
    pub m_descent: libc::c_float,
    pub m_capHeight: libc::c_float,
    pub m_xHeight: libc::c_float,
    pub m_italicAngle: libc::c_float,
    pub m_vertical: bool,
    pub m_filename: *mut libc::c_char,
    pub m_index: uint32_t,
    pub m_ftFace: FT_Face,
    pub m_afm_backing_data: Vec<u8>,
    pub m_hbFont: *mut hb_font_t,
    pub m_subdtor: Option<unsafe fn(_: *mut XeTeXFontInst) -> ()>,
    pub fk_font: Option<Font>,
}

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
/*
 *   file name:  XeTeXFontInst.cpp
 *
 *   created on: 2005-10-22
 *   created by: Jonathan Kew
 *
 *     originally based on PortableFontInstance.cpp from ICU
 */
/* Return NAME with any leading path stripped off.  This returns a
pointer into NAME.  For example, `basename ("/foo/bar.baz")'
returns "bar.baz".  */
unsafe extern "C" fn xbasename(mut name: *const libc::c_char) -> *const libc::c_char {
    let mut base: *const libc::c_char = name;
    let mut p: *const libc::c_char = 0 as *const libc::c_char;
    p = base;
    while *p != 0 {
        if *p as libc::c_int == '/' as i32 {
            base = p.offset(1)
        }
        p = p.offset(1)
    }
    return base;
}

#[no_mangle]
pub static mut gFreeTypeLibrary: FT_Library = ptr::null_mut();
static mut hbFontFuncs: *mut hb_font_funcs_t = ptr::null_mut();

impl XeTeXFontInst {
    pub unsafe fn new(
        mut pathname: *const libc::c_char,
        mut index: libc::c_int,
        mut pointSize: libc::c_float,
        mut status: *mut libc::c_int,
    ) -> Self {
        let mut neu = XeTeXFontInst {
            m_unitsPerEM: 0,
            m_pointSize: pointSize,
            m_ascent: 0.0,
            m_descent: 0.0,
            m_capHeight: 0.0,
            m_xHeight: 0.0,
            m_italicAngle: 0.0,
            m_vertical: false,
            m_filename: std::ptr::null_mut(),
            m_index: 0,
            m_ftFace: std::ptr::null_mut(),
            m_afm_backing_data: Vec::new(),
            m_hbFont: std::ptr::null_mut(),
            m_subdtor: None,
            fk_font: None,
        };
        if !pathname.is_null() {
            if let Err(e) = neu.init(pathname, index) {
                *status = e;
            }
        }
        neu
    }
    pub unsafe fn init(
        &mut self,
        mut pathname: *const libc::c_char,
        mut index: libc::c_int,
    ) -> Result<(), i32> {
        XeTeXFontInst_initialize(self, pathname, index)
    }

    pub unsafe extern "C" fn units_to_points(&self, units: f32) -> f32 {
        (units * self.m_pointSize) / (self.m_unitsPerEM as f32)
    }

    pub unsafe fn get_glyph_bounds(&self, gid: GlyphID) -> GlyphBBox {
        if let Some(font) = &self.fk_font {
            if let Ok(rect) = font.typographic_bounds(gid as u32) {
                let bbox = GlyphBBox {
                    xMin: self.units_to_points(rect.origin.x),
                    xMax: self.units_to_points(rect.origin.x + rect.size.width),
                    yMin: self.units_to_points(rect.origin.y),
                    yMax: self.units_to_points(rect.origin.y + rect.size.height),
                };
                return bbox;
            }
        }
        GlyphBBox::zero()
    }
}

#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_create(
    mut pathname: *const libc::c_char,
    mut index: libc::c_int,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) -> *mut XeTeXFontInst {
    let mut self_0: *mut XeTeXFontInst =
        malloc(::std::mem::size_of::<XeTeXFontInst>() as libc::c_ulong) as *mut XeTeXFontInst;
    std::ptr::write(
        self_0,
        XeTeXFontInst::new(pathname, index, pointSize, status),
    );
    self_0
}

#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_delete(mut self_0: *mut XeTeXFontInst) {
    if self_0.is_null() {
        return;
    }
    if (*self_0).m_subdtor.is_some() {
        (*self_0).m_subdtor.expect("non-null function pointer")(self_0);
    }
    if !(*self_0).m_ftFace.is_null() {
        (*self_0).m_ftFace = 0 as FT_Face
    }
    hb_font_destroy((*self_0).m_hbFont);
    // Writing to these drops what's in there
    (*self_0).m_afm_backing_data = Vec::new();
    // Also drops the underlying FreeType font, so we don't have to FT_Done_Face ourselves
    (*self_0).fk_font = None;
    free((*self_0).m_filename as *mut libc::c_void);
    free(self_0 as *mut libc::c_void);
}

/* HarfBuzz font functions */
unsafe extern "C" fn _get_glyph(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut ch: hb_codepoint_t,
    mut vs: hb_codepoint_t,
    mut gid: *mut hb_codepoint_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    let mut face: FT_Face = font_data as FT_Face;
    *gid = 0i32 as hb_codepoint_t;
    if vs != 0 {
        *gid = FT_Face_GetCharVariantIndex(face, ch as FT_ULong, vs as FT_ULong)
    }
    if *gid == 0i32 as libc::c_uint {
        *gid = FT_Get_Char_Index(face, ch as FT_ULong)
    }
    return (*gid != 0i32 as libc::c_uint) as libc::c_int;
}

unsafe extern "C" fn _get_glyph_advance(
    mut face: FT_Face,
    mut gid: FT_UInt,
    mut vertical: bool,
) -> FT_Fixed {
    let mut error: FT_Error = 0;
    let mut advance: FT_Fixed = 0;
    let mut flags: libc::c_int = (1i64 << 0i32) as libc::c_int;
    if vertical {
        flags = (flags as libc::c_long | 1 << 4i32) as libc::c_int
    }
    error = FT_Get_Advance(face, gid, flags, &mut advance);
    if error != 0 {
        advance = 0i32 as FT_Fixed
    }
    /* FreeType's vertical metrics grows downward */
    if vertical {
        advance = -advance
    }
    return advance;
}

unsafe extern "C" fn _get_glyph_h_advance(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut _p: *mut libc::c_void,
) -> hb_position_t {
    return _get_glyph_advance(font_data as FT_Face, gid, 0i32 != 0) as hb_position_t;
}

unsafe extern "C" fn _get_glyph_v_advance(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut _p: *mut libc::c_void,
) -> hb_position_t {
    return _get_glyph_advance(font_data as FT_Face, gid, 1i32 != 0) as hb_position_t;
}
unsafe extern "C" fn _get_glyph_h_origin(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut x: *mut hb_position_t,
    mut y: *mut hb_position_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    // horizontal origin is (0, 0)
    return 1i32;
}

unsafe extern "C" fn _get_glyph_v_origin(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut x: *mut hb_position_t,
    mut y: *mut hb_position_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    // vertical origin is (0, 0) for now
    return 1i32;
}

unsafe extern "C" fn _get_glyph_h_kerning(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid1: hb_codepoint_t,
    mut gid2: hb_codepoint_t,
    mut _p: *mut libc::c_void,
) -> hb_position_t {
    let mut face: FT_Face = font_data as FT_Face;
    let mut error: FT_Error = 0;
    let mut kerning: FT_Vector = FT_Vector { x: 0, y: 0 };
    let mut ret: hb_position_t = 0;
    error = FT_Get_Kerning(
        face,
        gid1,
        gid2,
        FT_KERNING_UNSCALED as libc::c_int as FT_UInt,
        &mut kerning,
    );
    if error != 0 {
        ret = 0i32
    } else {
        ret = kerning.x as hb_position_t
    }
    return ret;
}

unsafe extern "C" fn _get_glyph_v_kerning(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid1: hb_codepoint_t,
    mut gid2: hb_codepoint_t,
    mut _p: *mut libc::c_void,
) -> hb_position_t {
    /* FreeType does not support vertical kerning */
    return 0i32;
}

unsafe extern "C" fn _get_glyph_extents(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut extents: *mut hb_glyph_extents_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    let mut face: FT_Face = font_data as FT_Face;
    let mut error: FT_Error = 0;
    error = FT_Load_Glyph(face, gid, (1i64 << 0i32) as FT_Int32);
    if error == 0 {
        (*extents).x_bearing = (*(*face).glyph).metrics.horiBearingX as hb_position_t;
        (*extents).y_bearing = (*(*face).glyph).metrics.horiBearingY as hb_position_t;
        (*extents).width = (*(*face).glyph).metrics.width as hb_position_t;
        (*extents).height = -(*(*face).glyph).metrics.height as hb_position_t
    }
    return (error == 0) as libc::c_int;
}

unsafe extern "C" fn _get_glyph_contour_point(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut point_index: libc::c_uint,
    mut x: *mut hb_position_t,
    mut y: *mut hb_position_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    let mut face: FT_Face = font_data as FT_Face;
    let mut error: FT_Error = 0;
    let mut ret: bool = 0i32 != 0;
    error = FT_Load_Glyph(face, gid, (1i64 << 0i32) as FT_Int32);
    if error == 0 {
        if (*(*face).glyph).format as libc::c_uint
            == FT_Glyph_Format::FT_GLYPH_FORMAT_OUTLINE as libc::c_int as libc::c_uint
        {
            if point_index < (*(*face).glyph).outline.n_points as libc::c_uint {
                *x = (*(*(*face).glyph).outline.points.offset(point_index as isize)).x
                    as hb_position_t;
                *y = (*(*(*face).glyph).outline.points.offset(point_index as isize)).y
                    as hb_position_t;
                ret = 1i32 != 0
            }
        }
    }
    return ret as hb_bool_t;
}

unsafe extern "C" fn _get_glyph_name(
    mut _hbf: *mut hb_font_t,
    mut font_data: *mut libc::c_void,
    mut gid: hb_codepoint_t,
    mut name: *mut libc::c_char,
    mut size: libc::c_uint,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    let mut face: FT_Face = font_data as FT_Face;
    let mut ret: bool = 0i32 != 0;
    ret = FT_Get_Glyph_Name(face, gid, name as FT_Pointer, size) == 0;
    if ret as libc::c_int != 0 && (size != 0 && *name == 0) {
        ret = 0i32 != 0
    }
    return ret as hb_bool_t;
}

unsafe extern "C" fn _get_font_funcs() -> *mut hb_font_funcs_t {
    static mut funcs: *mut hb_font_funcs_t = 0 as *const hb_font_funcs_t as *mut hb_font_funcs_t;
    if funcs.is_null() {
        funcs = hb_font_funcs_create()
    }
    hb_font_funcs_set_glyph_func(
        funcs,
        Some(
            _get_glyph
                as unsafe extern "C" fn(
                    _: *mut hb_font_t,
                    _: *mut libc::c_void,
                    _: hb_codepoint_t,
                    _: hb_codepoint_t,
                    _: *mut hb_codepoint_t,
                    _: *mut libc::c_void,
                ) -> hb_bool_t,
        ),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_h_advance_func(
        funcs,
        Some(_get_glyph_h_advance),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_v_advance_func(
        funcs,
        Some(_get_glyph_v_advance),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_h_origin_func(
        funcs,
        Some(_get_glyph_h_origin),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_v_origin_func(
        funcs,
        Some(_get_glyph_v_origin),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_h_kerning_func(
        funcs,
        Some(
            _get_glyph_h_kerning
                as unsafe extern "C" fn(
                    _: *mut hb_font_t,
                    _: *mut libc::c_void,
                    _: hb_codepoint_t,
                    _: hb_codepoint_t,
                    _: *mut libc::c_void,
                ) -> hb_position_t,
        ),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_v_kerning_func(
        funcs,
        Some(
            _get_glyph_v_kerning
                as unsafe extern "C" fn(
                    _: *mut hb_font_t,
                    _: *mut libc::c_void,
                    _: hb_codepoint_t,
                    _: hb_codepoint_t,
                    _: *mut libc::c_void,
                ) -> hb_position_t,
        ),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_extents_func(
        funcs,
        Some(_get_glyph_extents),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_contour_point_func(
        funcs,
        Some(_get_glyph_contour_point),
        0 as *mut libc::c_void,
        None,
    );
    hb_font_funcs_set_glyph_name_func(funcs, Some(_get_glyph_name), 0 as *mut libc::c_void, None);
    return funcs;
}

unsafe extern "C" fn _get_table(
    mut _hfc: *mut hb_face_t,
    mut tag: hb_tag_t,
    mut user_data: *mut libc::c_void,
) -> *mut hb_blob_t {
    let mut face: FT_Face = user_data as FT_Face;
    let mut length: FT_ULong = 0i32 as FT_ULong;
    let mut table: *mut FT_Byte = 0 as *mut FT_Byte;
    let mut error: FT_Error = 0;
    let mut blob: *mut hb_blob_t = 0 as *mut hb_blob_t;
    error = FT_Load_Sfnt_Table(
        face,
        tag as FT_ULong,
        0i32 as FT_Long,
        0 as *mut FT_Byte,
        &mut length,
    );
    if error == 0 {
        table = xmalloc(
            length.wrapping_mul(::std::mem::size_of::<libc::c_char>() as libc::c_ulong) as _,
        ) as *mut FT_Byte;
        if !table.is_null() {
            error = FT_Load_Sfnt_Table(face, tag as FT_ULong, 0i32 as FT_Long, table, &mut length);
            if error == 0 {
                blob = hb_blob_create(
                    table as *const libc::c_char,
                    length as libc::c_uint,
                    HB_MEMORY_MODE_WRITABLE,
                    table as *mut libc::c_void,
                    Some(free as unsafe extern "C" fn(_: *mut libc::c_void) -> ()),
                )
            } else {
                free(table as *mut libc::c_void);
            }
        }
    }
    return blob;
}

use font_kit::family_name::FamilyName;
use font_kit::handle::Handle;
use font_kit::hinting::HintingOptions;
use font_kit::loader::FontTransform;
use font_kit::properties::Properties;
use font_kit::source::Source;
use font_kit::source::SystemSource;

#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_initialize(
    mut self_0: &mut XeTeXFontInst,
    mut pathname: *const libc::c_char,
    mut index: libc::c_int,
) -> Result<(), i32> {
    let mut postTable: *mut TT_Postscript = 0 as *mut TT_Postscript;
    let mut os2Table: *mut TT_OS2 = 0 as *mut TT_OS2;
    let mut error: FT_Error = 0;
    let mut hbFace: *mut hb_face_t = 0 as *mut hb_face_t;
    // Here we emulate some logic that was originally in find_native_font();
    let mut handle = ttstub_input_open(pathname, TTInputFormat::OPENTYPE, 0)
        .or_else(|| ttstub_input_open(pathname, TTInputFormat::TRUETYPE, 0))
        .or_else(|| ttstub_input_open(pathname, TTInputFormat::TYPE1, 0))
        .ok_or(1)?;

    use std::ffi::CStr;
    use std::io::prelude::*;
    use std::path::PathBuf;
    use std::sync::Arc;
    let pathname_str = CStr::from_ptr(pathname).to_str().map_err(|_| 1)?;
    let pathbuf = PathBuf::from(pathname_str);
    let mut bytes = Vec::new();
    handle.seek(std::io::SeekFrom::Start(0));
    handle.read_to_end(&mut bytes).unwrap();
    let len = bytes.len();

    let fk_handle = Handle::Memory {
        bytes: Arc::new(bytes),
        font_index: index as u32,
    };
    let fk_font = fk_handle.load().map_err(|e| {
        panic!(
            "font-kit: Couldn't load Handle for {}, got error {:?}",
            pathname_str, e
        );
        1
    })?;

    let ft_face = fk_font.native_font();

    // https://www.freetype.org/freetype2/docs/reference/ft2-base_interface.html#ft_face_flag_xxx
    const FT_FACE_FLAG_SCALABLE: FT_Long = 1 << 0;
    const FT_FACE_FLAG_SFNT: FT_Long = 1 << 3;

    if (*ft_face).face_flags & FT_FACE_FLAG_SCALABLE == 0 {
        // Not an outline font
        return Err(1);
    }

    self_0.fk_font = Some(fk_font);
    self_0.m_ftFace = ft_face;

    ttstub_input_close(handle);

    /* for non-sfnt-packaged fonts (presumably Type 1), see if there is an AFM file we can attach */
    if index == 0i32 && (*self_0.m_ftFace).face_flags & FT_FACE_FLAG_SFNT == 0 {
        // Tectonic: this code used to use kpse_find_file and FT_Attach_File
        // to try to find metrics for this font. Thanks to the existence of
        // FT_Attach_Stream we can emulate this behavior while going through
        // the Rust I/O layer.
        let mut afm: *mut libc::c_char = xstrdup(xbasename(pathname));
        let mut p: *mut libc::c_char = strrchr(afm, '.' as i32);
        if !p.is_null()
            && strlen(p) == 4i32 as libc::c_ulong
            && tolower(*p.offset(1) as libc::c_int) == 'p' as i32
            && tolower(*p.offset(2) as libc::c_int) == 'f' as i32
        {
            strcpy(p, b".afm\x00" as *const u8 as *const libc::c_char);
        }
        let mut afm_handle = ttstub_input_open(afm, TTInputFormat::AFM, 0i32);
        free(afm as *mut libc::c_void);
        if let Some(mut afm_handle) = afm_handle {
            self_0.m_afm_backing_data.clear();
            afm_handle
                .read_to_end(&mut self_0.m_afm_backing_data)
                .unwrap();
            ttstub_input_close(afm_handle);
            let mut open_args: FT_Open_Args = FT_Open_Args {
                flags: 0,
                memory_base: ptr::null(),
                memory_size: 0,
                pathname: ptr::null_mut(),
                stream: ptr::null_mut(),
                driver: ptr::null_mut(),
                num_params: 0,
                params: ptr::null_mut(),
            };
            open_args.flags = 1;
            open_args.memory_base = self_0.m_afm_backing_data.as_ptr();
            open_args.memory_size = self_0.m_afm_backing_data.len() as FT_Long;
            FT_Attach_Stream(self_0.m_ftFace, &mut open_args);
        }
    }
    self_0.m_filename = xstrdup(pathname);
    self_0.m_index = index as uint32_t;
    self_0.m_unitsPerEM = (*self_0.m_ftFace).units_per_EM;
    self_0.m_ascent =
        XeTeXFontInst_unitsToPoints(self_0, (*self_0.m_ftFace).ascender as libc::c_float);
    self_0.m_descent =
        XeTeXFontInst_unitsToPoints(self_0, (*self_0.m_ftFace).descender as libc::c_float);
    postTable = XeTeXFontInst_getFontTableFT(self_0, FT_Sfnt_Tag::FT_SFNT_POST) as *mut TT_Postscript;
    if !postTable.is_null() {
        self_0.m_italicAngle = Fix2D((*postTable).italicAngle as Fixed) as libc::c_float
    }
    os2Table = XeTeXFontInst_getFontTableFT(self_0, FT_Sfnt_Tag::FT_SFNT_OS2) as *mut TT_OS2;
    if !os2Table.is_null() {
        self_0.m_capHeight =
            XeTeXFontInst_unitsToPoints(self_0, (*os2Table).sCapHeight as libc::c_float);
        self_0.m_xHeight =
            XeTeXFontInst_unitsToPoints(self_0, (*os2Table).sxHeight as libc::c_float)
    }
    // Set up HarfBuzz font
    hbFace = hb_face_create_for_tables(
        Some(
            _get_table
                as unsafe extern "C" fn(
                    _: *mut hb_face_t,
                    _: hb_tag_t,
                    _: *mut libc::c_void,
                ) -> *mut hb_blob_t,
        ),
        self_0.m_ftFace as *mut libc::c_void,
        None,
    );
    hb_face_set_index(hbFace, index as libc::c_uint);
    hb_face_set_upem(hbFace, self_0.m_unitsPerEM as libc::c_uint);
    self_0.m_hbFont = hb_font_create(hbFace);
    hb_face_destroy(hbFace);
    if hbFontFuncs.is_null() {
        hbFontFuncs = _get_font_funcs()
    }
    hb_font_set_funcs(
        self_0.m_hbFont,
        hbFontFuncs,
        self_0.m_ftFace as *mut libc::c_void,
        None,
    );
    hb_font_set_scale(
        self_0.m_hbFont,
        self_0.m_unitsPerEM as libc::c_int,
        self_0.m_unitsPerEM as libc::c_int,
    );
    // We don’t want device tables adjustments
    hb_font_set_ppem(self_0.m_hbFont, 0i32 as libc::c_uint, 0i32 as libc::c_uint);
    Ok(())
}

#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_setLayoutDirVertical(
    mut self_0: *mut XeTeXFontInst,
    mut vertical: bool,
) {
    (*self_0).m_vertical = vertical;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getFontTable(
    mut self_0: *const XeTeXFontInst,
    mut tag: OTTag,
) -> *mut libc::c_void {
    let mut tmpLength: FT_ULong = 0i32 as FT_ULong;
    let mut error: FT_Error = FT_Load_Sfnt_Table(
        (*self_0).m_ftFace,
        tag as FT_ULong,
        0i32 as FT_Long,
        0 as *mut FT_Byte,
        &mut tmpLength,
    );
    if error != 0 {
        return 0 as *mut libc::c_void;
    }
    let mut table: *mut libc::c_void = xmalloc(
        tmpLength.wrapping_mul(::std::mem::size_of::<libc::c_char>() as libc::c_ulong) as _,
    );
    if !table.is_null() {
        error = FT_Load_Sfnt_Table(
            (*self_0).m_ftFace,
            tag as FT_ULong,
            0i32 as FT_Long,
            table as *mut FT_Byte,
            &mut tmpLength,
        );
        if error != 0 {
            free(table);
            return 0 as *mut libc::c_void;
        }
    }
    return table;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getFontTableFT(
    mut self_0: *const XeTeXFontInst,
    mut tag: FT_Sfnt_Tag,
) -> *mut libc::c_void {
    return FT_Get_Sfnt_Table((*self_0).m_ftFace, tag);
}

#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_mapCharToGlyph(
    mut self_0: *const XeTeXFontInst,
    mut ch: UChar32,
) -> GlyphID {
    return FT_Get_Char_Index((*self_0).m_ftFace, ch as FT_ULong) as GlyphID;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getNumGlyphs(mut self_0: *const XeTeXFontInst) -> uint16_t {
    return (*(*self_0).m_ftFace).num_glyphs as uint16_t;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphWidth(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
) -> libc::c_float {
    return XeTeXFontInst_unitsToPoints(
        self_0,
        _get_glyph_advance((*self_0).m_ftFace, gid as FT_UInt, 0i32 != 0) as libc::c_float,
    );
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphHeightDepth(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
    mut ht: *mut libc::c_float,
    mut dp: *mut libc::c_float,
) {
    let self_1 = &*(self_0 as *const XeTeXFontInst);
    let bbox = self_1.get_glyph_bounds(gid);
    if !ht.is_null() {
        *ht = bbox.yMax
    }
    if !dp.is_null() {
        *dp = -bbox.yMin
    };
}
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009, 2011 by Jonathan Kew

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
*/
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphSidebearings(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
    mut lsb: *mut libc::c_float,
    mut rsb: *mut libc::c_float,
) {
    let mut width: libc::c_float = XeTeXFontInst_getGlyphWidth(self_0, gid);

    let self_1 = &*(self_0 as *const XeTeXFontInst);
    let bbox = self_1.get_glyph_bounds(gid);

    if !lsb.is_null() {
        *lsb = bbox.xMin
    }
    if !rsb.is_null() {
        *rsb = width - bbox.xMax
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphItalCorr(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
) -> libc::c_float {
    let mut rval: libc::c_float = 0.0f64 as libc::c_float;
    let mut width: libc::c_float = XeTeXFontInst_getGlyphWidth(self_0, gid);
    let self_1 = &*(self_0 as *const XeTeXFontInst);
    let bbox = self_1.get_glyph_bounds(gid);
    if bbox.xMax > width {
        rval = bbox.xMax - width
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_mapGlyphToIndex(
    mut self_0: *const XeTeXFontInst,
    mut glyphName: *const libc::c_char,
) -> GlyphID {
    return FT_Get_Name_Index((*self_0).m_ftFace, glyphName as *mut libc::c_char) as GlyphID;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphName(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
    mut nameLen: *mut libc::c_int,
) -> *const libc::c_char {
    if (*(*self_0).m_ftFace).face_flags & 1 << 9i32 != 0 {
        static mut buffer: [libc::c_char; 256] = [0; 256];
        FT_Get_Glyph_Name(
            (*self_0).m_ftFace,
            gid as FT_UInt,
            buffer.as_mut_ptr() as FT_Pointer,
            256i32 as FT_UInt,
        );
        *nameLen = strlen(buffer.as_mut_ptr()) as libc::c_int;
        return &mut *buffer.as_mut_ptr().offset(0) as *mut libc::c_char;
    } else {
        *nameLen = 0i32;
        return 0 as *const libc::c_char;
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getFirstCharCode(mut self_0: *mut XeTeXFontInst) -> UChar32 {
    let mut gindex: FT_UInt = 0;
    return FT_Get_First_Char((*self_0).m_ftFace, &mut gindex) as UChar32;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getLastCharCode(mut self_0: *mut XeTeXFontInst) -> UChar32 {
    let mut gindex: FT_UInt = 0;
    let mut ch: UChar32 = FT_Get_First_Char((*self_0).m_ftFace, &mut gindex) as UChar32;
    let mut prev: UChar32 = ch;
    while gindex != 0i32 as libc::c_uint {
        prev = ch;
        ch = FT_Get_Next_Char((*self_0).m_ftFace, ch as FT_ULong, &mut gindex) as UChar32
    }
    return prev;
}

#[no_mangle]
//#[inline]
pub unsafe extern "C" fn XeTeXFontInst_getHbFont(self_0: *const XeTeXFontInst) -> *mut hb_font_t {
    (*self_0).m_hbFont
}

#[no_mangle]
//#[inline]
pub unsafe extern "C" fn XeTeXFontInst_unitsToPoints(
    self_0: *const XeTeXFontInst,
    units: libc::c_float,
) -> libc::c_float {
    let self_1 = &*self_0;
    self_1.units_to_points(units)
}

#[no_mangle]
//#[inline]
pub unsafe extern "C" fn XeTeXFontInst_pointsToUnits(
    self_0: *const XeTeXFontInst,
    points: libc::c_float,
) -> libc::c_float {
    (points * ((*self_0).m_unitsPerEM as libc::c_float)) / (*self_0).m_pointSize
}
