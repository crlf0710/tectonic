#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

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

use crate::freetype_sys_patch::{FT_Face_GetCharVariantIndex, FT_Get_Advance, FT_Load_Sfnt_Table};
use freetype::freetype_sys::{
    FT_Attach_Stream, FT_Done_Face, FT_Done_Glyph, FT_Get_Char_Index, FT_Get_First_Char,
    FT_Get_Glyph, FT_Get_Glyph_Name, FT_Get_Kerning, FT_Get_Name_Index, FT_Get_Next_Char,
    FT_Get_Sfnt_Table, FT_Glyph_Get_CBox, FT_Init_FreeType, FT_Load_Glyph, FT_New_Memory_Face,
};
use freetype::freetype_sys::{
    FT_BBox, FT_Byte, FT_Error, FT_Face, FT_Fixed, FT_Glyph, FT_Int32, FT_Library, FT_Long,
    FT_Parameter, FT_Pointer, FT_Sfnt_Tag, FT_String, FT_UInt, FT_ULong, FT_Vector,
};

use crate::{ttstub_input_close, ttstub_input_get_size, ttstub_input_open, ttstub_input_read};

use std::ptr;

use bridge::TTInputFormat;

#[cfg(not(target_os = "macos"))]
mod imp {}

#[cfg(target_os = "macos")]
#[path = "xetex_font_info_coretext.rs"]
mod imp;

extern crate libc;
extern "C" {
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
    #[no_mangle]
    fn Fix2D(f: Fixed) -> libc::c_double;
}
pub type size_t = usize;
pub type int32_t = i32;
pub type uint16_t = u16;
pub type uint32_t = u32;
pub type ssize_t = isize;

pub type UChar32 = int32_t;
/* quasi-hack to get the primary input */
/* */
/* this #if 0 ... #endif clause is for documentation purposes */

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
#[derive(Copy, Clone)]
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
    pub m_backingData: *mut FT_Byte,
    pub m_backingData2: *mut FT_Byte,
    pub m_hbFont: *mut hb_font_t,
    pub m_subdtor: Option<unsafe extern "C" fn(_: *mut XeTeXFontInst) -> ()>,
}
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
    let mut p: *const libc::c_char = ptr::null();
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
pub static mut gFreeTypeLibrary: FT_Library = 0 as FT_Library;
static mut hbFontFuncs: *mut hb_font_funcs_t = 0 as *mut hb_font_funcs_t;
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_base_ctor(
    mut self_0: *mut XeTeXFontInst,
    mut pathname: *const libc::c_char,
    mut index: libc::c_int,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) {
    (*self_0).m_unitsPerEM = 0i32 as libc::c_ushort;
    (*self_0).m_pointSize = pointSize;
    (*self_0).m_ascent = 0i32 as libc::c_float;
    (*self_0).m_descent = 0i32 as libc::c_float;
    (*self_0).m_capHeight = 0i32 as libc::c_float;
    (*self_0).m_xHeight = 0i32 as libc::c_float;
    (*self_0).m_italicAngle = 0i32 as libc::c_float;
    (*self_0).m_vertical = 0i32 != 0;
    (*self_0).m_filename = 0 as *mut libc::c_char;
    (*self_0).m_index = 0i32 as uint32_t;
    (*self_0).m_ftFace = 0 as FT_Face;
    (*self_0).m_backingData = 0 as *mut FT_Byte;
    (*self_0).m_backingData2 = 0 as *mut FT_Byte;
    (*self_0).m_hbFont = 0 as *mut hb_font_t;
    (*self_0).m_subdtor = None;
    if !pathname.is_null() {
        XeTeXFontInst_initialize(self_0, pathname, index, status);
    };
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
    XeTeXFontInst_base_ctor(self_0, pathname, index, pointSize, status);
    return self_0;
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
        FT_Done_Face((*self_0).m_ftFace);
        (*self_0).m_ftFace = 0 as FT_Face
    }
    hb_font_destroy((*self_0).m_hbFont);
    free((*self_0).m_backingData as *mut libc::c_void);
    free((*self_0).m_backingData2 as *mut libc::c_void);
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
    mut _font_data: *mut libc::c_void,
    mut _gid: hb_codepoint_t,
    mut _x: *mut hb_position_t,
    mut _y: *mut hb_position_t,
    mut _p: *mut libc::c_void,
) -> hb_bool_t {
    // horizontal origin is (0, 0)
    return 1i32;
}
unsafe extern "C" fn _get_glyph_v_origin(
    mut _hbf: *mut hb_font_t,
    mut _font_data: *mut libc::c_void,
    mut _gid: hb_codepoint_t,
    mut _x: *mut hb_position_t,
    mut _y: *mut hb_position_t,
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
    use freetype::freetype_sys::FT_KERNING_UNSCALED;
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
    mut _font_data: *mut libc::c_void,
    mut _gid1: hb_codepoint_t,
    mut _gid2: hb_codepoint_t,
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
    use freetype::freetype_sys::FT_GLYPH_FORMAT_OUTLINE;
    let mut face: FT_Face = font_data as FT_Face;
    let mut error: FT_Error = 0;
    let mut ret: bool = 0i32 != 0;
    error = FT_Load_Glyph(face, gid, (1i64 << 0i32) as FT_Int32);
    if error == 0 {
        if (*(*face).glyph).format as libc::c_uint
            == FT_GLYPH_FORMAT_OUTLINE as libc::c_int as libc::c_uint
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
    static mut funcs: *mut hb_font_funcs_t = ptr::null_mut();
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
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_initialize(
    mut self_0: *mut XeTeXFontInst,
    mut pathname: *const libc::c_char,
    mut index: libc::c_int,
    mut status: *mut libc::c_int,
) {
    use crate::freetype_sys_patch::{FT_SFNT_OS2, FT_SFNT_POST};
    use freetype::freetype_sys::FT_Open_Args;
    use freetype::freetype_sys::{TT_Postscript, TT_OS2};
    let mut postTable: *mut TT_Postscript = 0 as *mut TT_Postscript;
    let mut os2Table: *mut TT_OS2 = 0 as *mut TT_OS2;
    let mut error: FT_Error = 0;
    let mut hbFace: *mut hb_face_t = 0 as *mut hb_face_t;
    if gFreeTypeLibrary.is_null() {
        error = FT_Init_FreeType(&mut gFreeTypeLibrary);
        if error != 0 {
            abort!("FreeType initialization failed, error {}", error);
        }
    }
    // Here we emulate some logic that was originally in find_native_font();
    let mut handle = ttstub_input_open(pathname, TTInputFormat::OPENTYPE, 0)
        .or_else(|| ttstub_input_open(pathname, TTInputFormat::TRUETYPE, 0))
        .or_else(|| ttstub_input_open(pathname, TTInputFormat::TYPE1, 0));
    if handle.is_none() {
        *status = 1i32;
        return;
    }
    let mut handle = handle.unwrap();
    let mut sz = ttstub_input_get_size(&mut handle);
    (*self_0).m_backingData = xmalloc(sz as _) as *mut FT_Byte;
    let mut r = ttstub_input_read(
        handle.0.as_ptr(),
        (*self_0).m_backingData as *mut libc::c_char,
        sz,
    );
    if r < 0 || r != sz as _ {
        abort!("failed to read font file");
    }
    ttstub_input_close(handle);
    error = FT_New_Memory_Face(
        gFreeTypeLibrary,
        (*self_0).m_backingData,
        sz as FT_Long,
        index as FT_Long,
        &mut (*self_0).m_ftFace,
    );
    if (*(*self_0).m_ftFace).face_flags & 1 << 0i32 == 0 {
        *status = 1i32;
        return;
    }
    /* for non-sfnt-packaged fonts (presumably Type 1), see if there is an AFM file we can attach */
    if index == 0i32 && (*(*self_0).m_ftFace).face_flags & 1 << 3i32 == 0 {
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
            sz = ttstub_input_get_size(&mut afm_handle);
            (*self_0).m_backingData2 = xmalloc(sz as _) as *mut FT_Byte;
            r = ttstub_input_read(
                afm_handle.0.as_ptr(),
                (*self_0).m_backingData2 as *mut libc::c_char,
                sz,
            );
            if r < 0 || r != sz as _ {
                abort!("failed to read AFM file");
            }
            ttstub_input_close(afm_handle);
            let mut open_args: FT_Open_Args = FT_Open_Args {
                flags: 0,
                memory_base: ptr::null(),
                memory_size: 0,
                pathname: 0 as *mut FT_String,
                stream: ptr::null_mut(),
                driver: ptr::null_mut(),
                num_params: 0,
                params: 0 as *mut FT_Parameter,
            };
            open_args.flags = 0x1i32 as FT_UInt;
            open_args.memory_base = (*self_0).m_backingData2;
            open_args.memory_size = sz as FT_Long;
            FT_Attach_Stream((*self_0).m_ftFace, &mut open_args);
        }
    }
    (*self_0).m_filename = xstrdup(pathname);
    (*self_0).m_index = index as uint32_t;
    (*self_0).m_unitsPerEM = (*(*self_0).m_ftFace).units_per_EM;
    (*self_0).m_ascent =
        XeTeXFontInst_unitsToPoints(self_0, (*(*self_0).m_ftFace).ascender as libc::c_float);
    (*self_0).m_descent =
        XeTeXFontInst_unitsToPoints(self_0, (*(*self_0).m_ftFace).descender as libc::c_float);
    postTable = XeTeXFontInst_getFontTableFT(self_0, FT_SFNT_POST) as *mut TT_Postscript;
    if !postTable.is_null() {
        (*self_0).m_italicAngle = Fix2D((*postTable).italicAngle as Fixed) as libc::c_float
    }
    os2Table = XeTeXFontInst_getFontTableFT(self_0, FT_SFNT_OS2) as *mut TT_OS2;
    if !os2Table.is_null() {
        (*self_0).m_capHeight =
            XeTeXFontInst_unitsToPoints(self_0, (*os2Table).sCapHeight as libc::c_float);
        (*self_0).m_xHeight =
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
        (*self_0).m_ftFace as *mut libc::c_void,
        None,
    );
    hb_face_set_index(hbFace, index as libc::c_uint);
    hb_face_set_upem(hbFace, (*self_0).m_unitsPerEM as libc::c_uint);
    (*self_0).m_hbFont = hb_font_create(hbFace);
    hb_face_destroy(hbFace);
    if hbFontFuncs.is_null() {
        hbFontFuncs = _get_font_funcs()
    }
    hb_font_set_funcs(
        (*self_0).m_hbFont,
        hbFontFuncs,
        (*self_0).m_ftFace as *mut libc::c_void,
        None,
    );
    hb_font_set_scale(
        (*self_0).m_hbFont,
        (*self_0).m_unitsPerEM as libc::c_int,
        (*self_0).m_unitsPerEM as libc::c_int,
    );
    // We don’t want device tables adjustments
    hb_font_set_ppem(
        (*self_0).m_hbFont,
        0i32 as libc::c_uint,
        0i32 as libc::c_uint,
    );
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
pub unsafe extern "C" fn XeTeXFontInst_getGlyphBounds(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
    mut bbox: *mut GlyphBBox,
) {
    use freetype::freetype_sys::FT_GLYPH_BBOX_UNSCALED;
    (*bbox).yMax = 0.0f64 as libc::c_float;
    (*bbox).xMax = (*bbox).yMax;
    (*bbox).yMin = (*bbox).xMax;
    (*bbox).xMin = (*bbox).yMin;
    let mut error: FT_Error = FT_Load_Glyph(
        (*self_0).m_ftFace,
        gid as FT_UInt,
        (1i64 << 0i32) as FT_Int32,
    );
    if error != 0 {
        return;
    }
    let mut glyph: FT_Glyph = 0 as FT_Glyph;
    error = FT_Get_Glyph((*(*self_0).m_ftFace).glyph, &mut glyph);
    if error == 0i32 {
        let mut ft_bbox: FT_BBox = FT_BBox {
            xMin: 0,
            yMin: 0,
            xMax: 0,
            yMax: 0,
        };
        FT_Glyph_Get_CBox(
            glyph,
            FT_GLYPH_BBOX_UNSCALED as libc::c_int as FT_UInt,
            &mut ft_bbox,
        );
        (*bbox).xMin = XeTeXFontInst_unitsToPoints(self_0, ft_bbox.xMin as libc::c_float);
        (*bbox).yMin = XeTeXFontInst_unitsToPoints(self_0, ft_bbox.yMin as libc::c_float);
        (*bbox).xMax = XeTeXFontInst_unitsToPoints(self_0, ft_bbox.xMax as libc::c_float);
        (*bbox).yMax = XeTeXFontInst_unitsToPoints(self_0, ft_bbox.yMax as libc::c_float);
        FT_Done_Glyph(glyph);
    };
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
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    XeTeXFontInst_getGlyphBounds(self_0, gid, &mut bbox);
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
\****************************************************************************/
/*
 *   file name:  XeTeXFontInst.h
 *
 *   created on: 2005-10-22
 *   created by: Jonathan Kew
 *
 *  originally based on PortableFontInstance.h from ICU
 */
// create specific subclasses for each supported platform
// false = horizontal, true = vertical
// font filename
// face index
/*
class XeTeXFontInst
{
protected:

public:
    XeTeXFontInst(float pointSize, int &status);
    XeTeXFontInst(const char* filename, int index, float pointSize, int &status);

    virtual ~XeTeXFontInst();

    void initialize(const char* pathname, int index, int &status);

    void *getFontTable(OTTag tableTag) const;
    void *getFontTable(FT_Sfnt_Tag tableTag) const;

    hb_font_t *getHbFont() const { return m_hbFont; }
    void setLayoutDirVertical(bool vertical);
    bool getLayoutDirVertical() const { return m_vertical; }

    GlyphID mapCharToGlyph(UChar32 ch) const;
    GlyphID mapGlyphToIndex(const char* glyphName) const;

    uint16_t getNumGlyphs() const;

    void getGlyphBounds(GlyphID glyph, GlyphBBox* bbox);

    float getGlyphWidth(GlyphID glyph);
    void getGlyphHeightDepth(GlyphID glyph, float *ht, float* dp);
    void getGlyphSidebearings(GlyphID glyph, float* lsb, float* rsb);
    float getGlyphItalCorr(GlyphID glyph);

    const char* getGlyphName(GlyphID gid, int& nameLen);

    UChar32 getFirstCharCode();
    UChar32 getLastCharCode();

    float unitsToPoints(float units) const
    {
        return (units * m_pointSize) / (float) m_unitsPerEM;
    }

    float pointsToUnits(float points) const
    {
        return (points * (float) m_unitsPerEM) / m_pointSize;
    }
};
*/
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_getGlyphSidebearings(
    mut self_0: *mut XeTeXFontInst,
    mut gid: GlyphID,
    mut lsb: *mut libc::c_float,
    mut rsb: *mut libc::c_float,
) {
    let mut width: libc::c_float = XeTeXFontInst_getGlyphWidth(self_0, gid);
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    XeTeXFontInst_getGlyphBounds(self_0, gid, &mut bbox);
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
    let mut bbox: GlyphBBox = GlyphBBox {
        xMin: 0.,
        yMin: 0.,
        xMax: 0.,
        yMax: 0.,
    };
    XeTeXFontInst_getGlyphBounds(self_0, gid, &mut bbox);
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
        return ptr::null();
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
    (units * (*self_0).m_pointSize) / ((*self_0).m_unitsPerEM as libc::c_float)
}

#[no_mangle]
//#[inline]
pub unsafe extern "C" fn XeTeXFontInst_pointsToUnits(
    self_0: *const XeTeXFontInst,
    points: libc::c_float,
) -> libc::c_float {
    (points * ((*self_0).m_unitsPerEM as libc::c_float)) / (*self_0).m_pointSize
}
