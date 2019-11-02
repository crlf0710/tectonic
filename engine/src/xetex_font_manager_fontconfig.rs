#![cfg(not(target_os = "macos"))]

#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

use crate::stub_icu as icu;
use crate::xetex_layout_interface::collection_types::*;


#[allow(improper_ctypes)]
extern "C" {
    pub type _FcPattern;
    pub type _FcConfig;
    /* ************************************************************************/
    /* ************************************************************************/
    /*                                                                       */
    /*                     O B J E C T   C L A S S E S                       */
    /*                                                                       */
    /* ************************************************************************/
    /* ************************************************************************/
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
    #[no_mangle]
    fn FcConfigGetCurrent() -> *mut FcConfig;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn strdup(_: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strchr(_: *const libc::c_char, _: libc::c_int) -> *mut libc::c_char;
    /* The internal, C/C++ interface: */
    #[no_mangle]
    fn _tt_abort(format: *const libc::c_char, _: ...) -> !;
    #[no_mangle]
    fn FcFontSetDestroy(s: *mut FcFontSet);
    #[no_mangle]
    fn FcInit() -> FcBool;
    #[no_mangle]
    fn FcObjectSetDestroy(os: *mut FcObjectSet);
    #[no_mangle]
    fn FcObjectSetBuild(first: *const libc::c_char, _: ...) -> *mut FcObjectSet;
    #[no_mangle]
    fn FcFontList(config: *mut FcConfig, p: *mut FcPattern, os: *mut FcObjectSet)
        -> *mut FcFontSet;
    #[no_mangle]
    fn FcNameParse(name: *const FcChar8) -> *mut FcPattern;
    #[no_mangle]
    fn FcPatternDestroy(p: *mut FcPattern);
    #[no_mangle]
    fn FcPatternGetInteger(
        p: *const FcPattern,
        object: *const libc::c_char,
        n: libc::c_int,
        i: *mut libc::c_int,
    ) -> FcResult;
    #[no_mangle]
    fn FcPatternGetString(
        p: *const FcPattern,
        object: *const libc::c_char,
        n: libc::c_int,
        s: *mut *mut FcChar8,
    ) -> FcResult;
    /* ************************************************************************/
    /* ************************************************************************/
    /*                                                                       */
    /*                         F U N C T I O N S                             */
    /*                                                                       */
    /* ************************************************************************/
    /* ************************************************************************/
    /* *************************************************************************
     *
     * @function:
     *   FT_Init_FreeType
     *
     * @description:
     *   Initialize a new FreeType library object.  The set of modules that are
     *   registered by this function is determined at build time.
     *
     * @output:
     *   alibrary ::
     *     A handle to a new library object.
     *
     * @return:
     *   FreeType error code.  0~means success.
     *
     * @note:
     *   In case you want to provide your own memory allocating routines, use
     *   @FT_New_Library instead, followed by a call to @FT_Add_Default_Modules
     *   (or a series of calls to @FT_Add_Module) and
     *   @FT_Set_Default_Properties.
     *
     *   See the documentation of @FT_Library and @FT_Face for multi-threading
     *   issues.
     *
     *   If you need reference-counting (cf. @FT_Reference_Library), use
     *   @FT_New_Library and @FT_Done_Library.
     *
     *   If compilation option `FT_CONFIG_OPTION_ENVIRONMENT_PROPERTIES` is
     *   set, this function reads the `FREETYPE_PROPERTIES` environment
     *   variable to control driver properties.  See section @properties for
     *   more.
     */
    #[no_mangle]
    fn FT_Init_FreeType(alibrary: *mut FT_Library) -> FT_Error;
    /* *************************************************************************
     *
     * @function:
     *   FT_New_Face
     *
     * @description:
     *   Call @FT_Open_Face to open a font by its pathname.
     *
     * @inout:
     *   library ::
     *     A handle to the library resource.
     *
     * @input:
     *   pathname ::
     *     A path to the font file.
     *
     *   face_index ::
     *     See @FT_Open_Face for a detailed description of this parameter.
     *
     * @output:
     *   aface ::
     *     A handle to a new face object.  If `face_index` is greater than or
     *     equal to zero, it must be non-`NULL`.
     *
     * @return:
     *   FreeType error code.  0~means success.
     *
     * @note:
     *   Use @FT_Done_Face to destroy the created @FT_Face object (along with
     *   its slot and sizes).
     */
    #[no_mangle]
    fn FT_New_Face(
        library: FT_Library,
        filepathname: *const libc::c_char,
        face_index: FT_Long,
        aface: *mut FT_Face,
    ) -> FT_Error;
    /* *************************************************************************
     *
     * @function:
     *   FT_Done_Face
     *
     * @description:
     *   Discard a given face object, as well as all of its child slots and
     *   sizes.
     *
     * @input:
     *   face ::
     *     A handle to a target face object.
     *
     * @return:
     *   FreeType error code.  0~means success.
     *
     * @note:
     *   See the discussion of reference counters in the description of
     *   @FT_Reference_Face.
     */
    #[no_mangle]
    fn FT_Done_Face(face: FT_Face) -> FT_Error;
    /* *************************************************************************
     *
     * @function:
     *   FT_Get_Postscript_Name
     *
     * @description:
     *   Retrieve the ASCII PostScript name of a given face, if available.
     *   This only works with PostScript, TrueType, and OpenType fonts.
     *
     * @input:
     *   face ::
     *     A handle to the source face object.
     *
     * @return:
     *   A pointer to the face's PostScript name.  `NULL` if unavailable.
     *
     * @note:
     *   The returned pointer is owned by the face and is destroyed with it.
     *
     *   For variation fonts, this string changes if you select a different
     *   instance, and you have to call `FT_Get_PostScript_Name` again to
     *   retrieve it.  FreeType follows Adobe TechNote #5902, 'Generating
     *   PostScript Names for Fonts Using OpenType Font Variations'.
     *
     *     https://download.macromedia.com/pub/developer/opentype/tech-notes/5902.AdobePSNameGeneration.html
     *
     *   [Since 2.9] Special PostScript names for named instances are only
     *   returned if the named instance is set with @FT_Set_Named_Instance (and
     *   the font has corresponding entries in its 'fvar' table).  If
     *   @FT_IS_VARIATION returns true, the algorithmically derived PostScript
     *   name is provided, not looking up special entries for named instances.
     */
    #[no_mangle]
    fn FT_Get_Postscript_Name(face: FT_Face) -> *const libc::c_char;
    /* tectonic/xetex-core.h: core XeTeX types and #includes.
       Copyright 2016 the Tectonic Project
       Licensed under the MIT License.
    */
    // defines U_IS_BIG_ENDIAN for us
    /* fontconfig */
    /* freetype */
    /* harfbuzz */
    /* Endianness foo */
    /* our typedefs */
    /* Macs provide Fixed and FixedPoint */
    /* dummy declaration just so the stubs can compile */
    /* Misc */
    /* gFreeTypeLibrary is defined in xetex-XeTeXFontInst_FT2.cpp,
     * also used in xetex-XeTeXFontMgr_FC.cpp and xetex-ext.c.  */
    #[no_mangle]
    static mut gFreeTypeLibrary: FT_Library;
    #[no_mangle]
    fn XeTeXFontMgr_base_ctor(self_0: *mut XeTeXFontMgr);
    #[no_mangle]
    fn XeTeXFontMgr_appendToList(
        self_0: *mut XeTeXFontMgr,
        list: *mut CppStdListOfString,
        str: *const libc::c_char,
    );
    #[no_mangle]
    fn XeTeXFontMgr_prependToList(
        self_0: *mut XeTeXFontMgr,
        list: *mut CppStdListOfString,
        str: *const libc::c_char,
    );
    #[no_mangle]
    fn XeTeXFontMgr_addToMaps(
        self_0: *mut XeTeXFontMgr,
        platformFont: PlatformFontRef,
        names: *const XeTeXFontMgrNameCollection,
    );
    #[no_mangle]
    fn XeTeXFontMgr_base_getOpSizeRecAndStyleFlags(
        self_0: *mut XeTeXFontMgr,
        theFont: *mut XeTeXFontMgrFont,
    );
    /* *************************************************************************
     *
     * @function:
     *   FT_Get_Sfnt_Name_Count
     *
     * @description:
     *   Retrieve the number of name strings in the SFNT 'name' table.
     *
     * @input:
     *   face ::
     *     A handle to the source face.
     *
     * @return:
     *   The number of strings in the 'name' table.
     *
     * @note:
     *   This function always returns an error if the config macro
     *   `TT_CONFIG_OPTION_SFNT_NAMES` is not defined in `ftoption.h`.
     */
    #[no_mangle]
    fn FT_Get_Sfnt_Name_Count(face: FT_Face) -> FT_UInt;
    /* *************************************************************************
     *
     * @function:
     *   FT_Get_Sfnt_Name
     *
     * @description:
     *   Retrieve a string of the SFNT 'name' table for a given index.
     *
     * @input:
     *   face ::
     *     A handle to the source face.
     *
     *   idx ::
     *     The index of the 'name' string.
     *
     * @output:
     *   aname ::
     *     The indexed @FT_SfntName structure.
     *
     * @return:
     *   FreeType error code.  0~means success.
     *
     * @note:
     *   The `string` array returned in the `aname` structure is not
     *   null-terminated.  Note that you don't have to deallocate `string` by
     *   yourself; FreeType takes care of it if you call @FT_Done_Face.
     *
     *   Use @FT_Get_Sfnt_Name_Count to get the total number of available
     *   'name' table entries, then do a loop until you get the right platform,
     *   encoding, and name ID.
     *
     *   'name' table format~1 entries can use language tags also, see
     *   @FT_Get_Sfnt_LangTag.
     *
     *   This function always returns an error if the config macro
     *   `TT_CONFIG_OPTION_SFNT_NAMES` is not defined in `ftoption.h`.
     */
    #[no_mangle]
    fn FT_Get_Sfnt_Name(face: FT_Face, idx: FT_UInt, aname: *mut FT_SfntName) -> FT_Error;
}
pub type __int16_t = libc::c_short;
pub type __uint16_t = libc::c_ushort;
pub type __int32_t = libc::c_int;
pub type int16_t = __int16_t;
pub type int32_t = __int32_t;
pub type uint16_t = __uint16_t;
pub type size_t = libc::c_ulong;
pub type FcChar8 = libc::c_uchar;
pub type FcBool = libc::c_int;
pub type _FcResult = libc::c_uint;
pub const FcResultOutOfMemory: _FcResult = 4;
pub const FcResultNoId: _FcResult = 3;
pub const FcResultTypeMismatch: _FcResult = 2;
pub const FcResultNoMatch: _FcResult = 1;
pub const FcResultMatch: _FcResult = 0;
pub type FcResult = _FcResult;
pub type FcPattern = _FcPattern;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _FcFontSet {
    pub nfont: libc::c_int,
    pub sfont: libc::c_int,
    pub fonts: *mut *mut FcPattern,
}
pub type FcFontSet = _FcFontSet;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _FcObjectSet {
    pub nobject: libc::c_int,
    pub sobject: libc::c_int,
    pub objects: *mut *const libc::c_char,
}
pub type FcObjectSet = _FcObjectSet;
pub type FcConfig = _FcConfig;
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
    unsafe extern "C" fn(
        _: FT_Memory,
        _: libc::c_long,
        _: libc::c_long,
        _: *mut libc::c_void,
    ) -> *mut libc::c_void,
>;
pub type FT_Memory = *mut FT_MemoryRec_;
pub type FT_Free_Func = Option<unsafe extern "C" fn(_: FT_Memory, _: *mut libc::c_void) -> ()>;
pub type FT_Alloc_Func =
    Option<unsafe extern "C" fn(_: FT_Memory, _: libc::c_long) -> *mut libc::c_void>;
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
pub type FT_Stream_CloseFunc = Option<unsafe extern "C" fn(_: FT_Stream) -> ()>;
pub type FT_Stream = *mut FT_StreamRec_;
pub type FT_Stream_IoFunc = Option<
    unsafe extern "C" fn(
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
 * @type:
 *   FT_Error
 *
 * @description:
 *   The FreeType error code type.  A value of~0 is always interpreted as a
 *   successful operation.
 */
pub type FT_Error = libc::c_int;
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
pub type FT_Generic_Finalizer = Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>;
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
pub type PlatformFontRef = *mut FcPattern;
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
use super::{
    XeTeXFontMgr, XeTeXFontMgrFont, XeTeXFontMgrNameCollection,
};
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
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgr_FC {
    pub super_: XeTeXFontMgr,
    pub allFonts: *mut FcFontSet,
    pub cachedAll: bool,
}
/* ***************************************************************************
 *
 * ftsnames.h
 *
 *   Simple interface to access SFNT 'name' tables (which are used
 *   to hold font names, copyright info, notices, etc.) (specification).
 *
 *   This is _not_ used to retrieve glyph names!
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
 *   sfnt_names
 *
 * @title:
 *   SFNT Names
 *
 * @abstract:
 *   Access the names embedded in TrueType and OpenType files.
 *
 * @description:
 *   The TrueType and OpenType specifications allow the inclusion of a
 *   special names table ('name') in font files.  This table contains
 *   textual (and internationalized) information regarding the font, like
 *   family name, copyright, version, etc.
 *
 *   The definitions below are used to access them if available.
 *
 *   Note that this has nothing to do with glyph names!
 *
 */
/* *************************************************************************
 *
 * @struct:
 *   FT_SfntName
 *
 * @description:
 *   A structure used to model an SFNT 'name' table entry.
 *
 * @fields:
 *   platform_id ::
 *     The platform ID for `string`.  See @TT_PLATFORM_XXX for possible
 *     values.
 *
 *   encoding_id ::
 *     The encoding ID for `string`.  See @TT_APPLE_ID_XXX, @TT_MAC_ID_XXX,
 *     @TT_ISO_ID_XXX, @TT_MS_ID_XXX, and @TT_ADOBE_ID_XXX for possible
 *     values.
 *
 *   language_id ::
 *     The language ID for `string`.  See @TT_MAC_LANGID_XXX and
 *     @TT_MS_LANGID_XXX for possible values.
 *
 *     Registered OpenType values for `language_id` are always smaller than
 *     0x8000; values equal or larger than 0x8000 usually indicate a
 *     language tag string (introduced in OpenType version 1.6).  Use
 *     function @FT_Get_Sfnt_LangTag with `language_id` as its argument to
 *     retrieve the associated language tag.
 *
 *   name_id ::
 *     An identifier for `string`.  See @TT_NAME_ID_XXX for possible
 *     values.
 *
 *   string ::
 *     The 'name' string.  Note that its format differs depending on the
 *     (platform,encoding) pair, being either a string of bytes (without a
 *     terminating `NULL` byte) or containing UTF-16BE entities.
 *
 *   string_len ::
 *     The length of `string` in bytes.
 *
 * @note:
 *   Please refer to the TrueType or OpenType specification for more
 *   details.
 */
pub type FT_SfntName = FT_SfntName_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FT_SfntName_ {
    pub platform_id: FT_UShort,
    pub encoding_id: FT_UShort,
    pub language_id: FT_UShort,
    pub name_id: FT_UShort,
    pub string: *mut FT_Byte,
    pub string_len: FT_UInt,
}
#[inline]
unsafe extern "C" fn XeTeXFontMgrNameCollection_create() -> *mut XeTeXFontMgrNameCollection {
    let mut self_0: *mut XeTeXFontMgrNameCollection =
        malloc(::std::mem::size_of::<XeTeXFontMgrNameCollection>() as libc::c_ulong)
            as *mut XeTeXFontMgrNameCollection;
    (*self_0).m_familyNames = CppStdListOfString_create();
    (*self_0).m_styleNames = CppStdListOfString_create();
    (*self_0).m_fullNames = CppStdListOfString_create();
    (*self_0).m_psName = CppStdString_create();
    (*self_0).m_subFamily = CppStdString_create();
    return self_0;
}
#[inline]
unsafe extern "C" fn XeTeXFontMgrNameCollection_delete(
    mut self_0: *mut XeTeXFontMgrNameCollection,
) {
    if self_0.is_null() {
        return;
    }
    CppStdListOfString_delete((*self_0).m_familyNames);
    CppStdListOfString_delete((*self_0).m_styleNames);
    CppStdListOfString_delete((*self_0).m_fullNames);
    CppStdString_delete((*self_0).m_psName);
    CppStdString_delete((*self_0).m_subFamily);
    free(self_0 as *mut libc::c_void);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_readNames(
    mut self_0: *mut XeTeXFontMgr,
    mut fontRef: PlatformFontRef,
) -> *mut XeTeXFontMgrNameCollection {
    return (*self_0)
        .m_memfnReadNames
        .expect("non-null function pointer")(self_0, fontRef);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_cacheFamilyMembers(
    mut self_0: *mut XeTeXFontMgr,
    mut familyNames: *const CppStdListOfString,
) {
    XeTeXFontMgr_FC_cacheFamilyMembers(self_0, familyNames);
}
static mut macRomanConv: *mut icu::UConverter = 0 as *mut icu::UConverter;
static mut utf16beConv: *mut icu::UConverter = 0 as *mut icu::UConverter;
static mut utf8Conv: *mut icu::UConverter = 0 as *mut icu::UConverter;
unsafe extern "C" fn convertToUtf8(
    mut conv: *mut icu::UConverter,
    mut name: *const libc::c_uchar,
    mut len: libc::c_int,
) -> *mut libc::c_char {
    let mut buffer1: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut buffer2: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut bufSize: libc::c_int = -1i32;
    if 2i32 * (len + 1i32) > bufSize {
        if !buffer1.is_null() {
            free(buffer1 as *mut libc::c_void);
            free(buffer2 as *mut libc::c_void);
        }
        bufSize = 2i32 * len + 100i32;
        buffer1 = malloc(
            (::std::mem::size_of::<libc::c_char>() as libc::c_ulong)
                .wrapping_mul(bufSize as libc::c_ulong),
        ) as *mut libc::c_char;
        buffer2 = malloc(
            (::std::mem::size_of::<libc::c_char>() as libc::c_ulong)
                .wrapping_mul(bufSize as libc::c_ulong),
        ) as *mut libc::c_char
    }
    let mut status: icu::UErrorCode = icu::U_ZERO_ERROR;
    len = icu::ucnv_toUChars(
        conv,
        buffer1 as *mut icu::UChar,
        bufSize,
        name as *const libc::c_char,
        len,
        &mut status,
    );
    len = icu::ucnv_fromUChars(
        utf8Conv,
        buffer2,
        bufSize,
        buffer1 as *mut icu::UChar,
        len,
        &mut status,
    );
    *buffer2.offset(len as isize) = 0i32 as libc::c_char;
    free(buffer1 as *mut libc::c_void);
    return buffer2;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_readNames(
    mut self_0: *mut XeTeXFontMgr,
    mut pat: *mut FcPattern,
) -> *mut XeTeXFontMgrNameCollection {
    let mut names: *mut XeTeXFontMgrNameCollection = XeTeXFontMgrNameCollection_create();
    let mut pathname: *mut libc::c_char = 0 as *mut libc::c_char;
    if FcPatternGetString(
        pat,
        b"file\x00" as *const u8 as *const libc::c_char,
        0i32,
        &mut pathname as *mut *mut libc::c_char as *mut *mut FcChar8,
    ) as libc::c_uint
        != FcResultMatch as libc::c_int as libc::c_uint
    {
        return names;
    }
    let mut index: libc::c_int = 0;
    if FcPatternGetInteger(
        pat,
        b"index\x00" as *const u8 as *const libc::c_char,
        0i32,
        &mut index,
    ) as libc::c_uint
        != FcResultMatch as libc::c_int as libc::c_uint
    {
        return names;
    }
    let mut face: FT_Face = 0 as *mut FT_FaceRec_;
    if FT_New_Face(gFreeTypeLibrary, pathname, index as FT_Long, &mut face) != 0i32 {
        return names;
    }
    let mut name: *const libc::c_char = FT_Get_Postscript_Name(face);
    if name.is_null() {
        return names;
    }
    CppStdString_assign_from_const_char_ptr((*names).m_psName, name);
    /* this string is *not* null-terminated! */
    /* in bytes                              */
    // for sfnt containers, we'll read the name table ourselves, not rely on Fontconfig
    if (*face).face_flags & 1 << 3i32 != 0 {
        let mut i: libc::c_uint = 0;
        let mut familyNames: *mut CppStdListOfString = CppStdListOfString_create();
        let mut subFamilyNames: *mut CppStdListOfString = CppStdListOfString_create();
        let mut nameRec: FT_SfntName = FT_SfntName {
            platform_id: 0,
            encoding_id: 0,
            language_id: 0,
            name_id: 0,
            string: 0 as *mut FT_Byte,
            string_len: 0,
        };
        i = 0i32 as libc::c_uint;
        while i < FT_Get_Sfnt_Name_Count(face) {
            let mut utf8name: *mut libc::c_char = 0 as *mut libc::c_char;
            if !(FT_Get_Sfnt_Name(face, i, &mut nameRec) != 0i32) {
                match nameRec.name_id as libc::c_int {
                    4 | 1 | 2 | 16 | 17 => {
                        let mut preferredName: bool = 0i32 != 0;
                        if nameRec.platform_id as libc::c_int == 1i32
                            && nameRec.encoding_id as libc::c_int == 0i32
                            && nameRec.language_id as libc::c_int == 0i32
                        {
                            utf8name = convertToUtf8(
                                macRomanConv,
                                nameRec.string,
                                nameRec.string_len as libc::c_int,
                            );
                            preferredName = 1i32 != 0
                        } else if nameRec.platform_id as libc::c_int == 0i32
                            || nameRec.platform_id as libc::c_int == 3i32
                        {
                            utf8name = convertToUtf8(
                                utf16beConv,
                                nameRec.string,
                                nameRec.string_len as libc::c_int,
                            )
                        }
                        if !utf8name.is_null() {
                            let mut nameList: *mut CppStdListOfString =
                                0 as *mut CppStdListOfString;
                            match nameRec.name_id as libc::c_int {
                                4 => nameList = (*names).m_fullNames,
                                1 => nameList = (*names).m_familyNames,
                                2 => nameList = (*names).m_styleNames,
                                16 => nameList = familyNames,
                                17 => nameList = subFamilyNames,
                                _ => {}
                            }
                            if preferredName {
                                XeTeXFontMgr_prependToList(self_0, nameList, utf8name);
                            } else {
                                XeTeXFontMgr_appendToList(self_0, nameList, utf8name);
                            }
                            free(utf8name as *mut libc::c_void);
                        }
                    }
                    _ => {}
                }
            }
            i = i.wrapping_add(1)
        }
        if !(*familyNames).is_empty() {
            *(*names).m_familyNames = (*familyNames).clone();
        }
        if !(*subFamilyNames).is_empty() {
            *(*names).m_styleNames = (*subFamilyNames).clone();
        }
        CppStdListOfString_delete(subFamilyNames);
        CppStdListOfString_delete(familyNames);
    } else {
        index = 0i32;
        loop {
            let fresh0 = index;
            index = index + 1;
            if !(FcPatternGetString(
                pat,
                b"fullname\x00" as *const u8 as *const libc::c_char,
                fresh0,
                &mut name as *mut *const libc::c_char as *mut *mut FcChar8,
            ) as libc::c_uint
                == FcResultMatch as libc::c_int as libc::c_uint)
            {
                break;
            }
            XeTeXFontMgr_appendToList(self_0, (*names).m_fullNames, name);
        }
        index = 0i32;
        loop {
            let fresh1 = index;
            index = index + 1;
            if !(FcPatternGetString(
                pat,
                b"family\x00" as *const u8 as *const libc::c_char,
                fresh1,
                &mut name as *mut *const libc::c_char as *mut *mut FcChar8,
            ) as libc::c_uint
                == FcResultMatch as libc::c_int as libc::c_uint)
            {
                break;
            }
            XeTeXFontMgr_appendToList(self_0, (*names).m_familyNames, name);
        }
        index = 0i32;
        loop {
            let fresh2 = index;
            index = index + 1;
            if !(FcPatternGetString(
                pat,
                b"style\x00" as *const u8 as *const libc::c_char,
                fresh2,
                &mut name as *mut *const libc::c_char as *mut *mut FcChar8,
            ) as libc::c_uint
                == FcResultMatch as libc::c_int as libc::c_uint)
            {
                break;
            }
            XeTeXFontMgr_appendToList(self_0, (*names).m_styleNames, name);
        }
        if (*(*names).m_fullNames).is_empty() {
            let mut fullName: *mut CppStdString = CppStdString_create();
            CppStdString_append_const_char_ptr(fullName, (*(*names).m_familyNames)[0].as_ptr());
            if !(*(*names).m_styleNames).is_empty() {
                CppStdString_append_const_char_ptr(
                    fullName,
                    b" \x00" as *const u8 as *const libc::c_char,
                );
                CppStdString_append_const_char_ptr(fullName, (*(*names).m_styleNames)[0].as_ptr());
            }
            (*(*names).m_fullNames).push_back((*fullName).clone());
            CppStdString_delete(fullName);
        }
    }
    FT_Done_Face(face);
    return names;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_getOpSizeRecAndStyleFlags(
    mut self_0: *mut XeTeXFontMgr,
    mut theFont: *mut XeTeXFontMgrFont,
) {
    XeTeXFontMgr_base_getOpSizeRecAndStyleFlags(self_0, theFont);
    if (*theFont).weight as libc::c_int == 0i32 && (*theFont).width as libc::c_int == 0i32 {
        // try to get values from FontConfig, as it apparently wasn't an sfnt
        let mut pat: *mut FcPattern = (*theFont).fontRef;
        let mut value: libc::c_int = 0;
        if FcPatternGetInteger(
            pat,
            b"weight\x00" as *const u8 as *const libc::c_char,
            0i32,
            &mut value,
        ) as libc::c_uint
            == FcResultMatch as libc::c_int as libc::c_uint
        {
            (*theFont).weight = value as uint16_t
        }
        if FcPatternGetInteger(
            pat,
            b"width\x00" as *const u8 as *const libc::c_char,
            0i32,
            &mut value,
        ) as libc::c_uint
            == FcResultMatch as libc::c_int as libc::c_uint
        {
            (*theFont).width = value as uint16_t
        }
        if FcPatternGetInteger(
            pat,
            b"slant\x00" as *const u8 as *const libc::c_char,
            0i32,
            &mut value,
        ) as libc::c_uint
            == FcResultMatch as libc::c_int as libc::c_uint
        {
            (*theFont).slant = value as int16_t
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_cacheFamilyMembers(
    mut self_0: *mut XeTeXFontMgr,
    mut familyNames: *const CppStdListOfString,
) {
    use std::ffi::CStr;
    let mut real_self: *mut XeTeXFontMgr_FC = self_0 as *mut XeTeXFontMgr_FC;
    if (*familyNames).is_empty() {
        return;
    }
    for f in 0i32..(*(*real_self).allFonts).nfont {
        let mut pat: *mut FcPattern = *(*(*real_self).allFonts).fonts.offset(f as isize);
        if (*(*self_0).m_platformRefToFont).contains_key(&pat) {
            continue;
        }

        let mut s: *mut libc::c_char = 0 as *mut libc::c_char;
        for i in 0i32.. {
            if FcPatternGetString(
                pat,
                b"family\x00" as *const u8 as *const libc::c_char,
                i,
                &mut s as *mut *mut libc::c_char as *mut *mut FcChar8,
            ) as libc::c_uint
                != FcResultMatch as _
            {
                break;
            }
            let s = CStr::from_ptr(s);
            if !(*familyNames).iter().any(|family_name| &**family_name == s) {
                continue;
            }
            let mut names: *mut XeTeXFontMgrNameCollection = XeTeXFontMgr_readNames(self_0, pat);
            XeTeXFontMgr_addToMaps(self_0, pat, names);
            XeTeXFontMgrNameCollection_delete(names);
            break;
        }
    }
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_searchForHostPlatformFonts(
    mut self_0: *mut XeTeXFontMgr,
    mut name: *const libc::c_char,
) {
    use std::ffi::CStr;
    let mut real_self: *mut XeTeXFontMgr_FC = self_0 as *mut XeTeXFontMgr_FC;
    if (*real_self).cachedAll {
        // we've already loaded everything on an earlier search
        return;
    }
    let mut famName: *mut CppStdString = CppStdString_create();
    let mut hyph_pos: *mut libc::c_char = strchr(name, '-' as i32);
    let mut hyph: libc::c_int = 0;
    if !hyph_pos.is_null() {
        hyph = hyph_pos.wrapping_offset_from(name) as libc::c_long as libc::c_int;
        CppStdString_assign_n_chars(famName, name, hyph as libc::size_t);
    } else {
        hyph = 0i32
    }
    let mut found: bool = 0i32 != 0;
    loop {
        let mut f: libc::c_int = 0i32;
        while f < (*(*real_self).allFonts).nfont {
            let mut pat: *mut FcPattern = *(*(*real_self).allFonts).fonts.offset(f as isize);
            if !(*(*self_0).m_platformRefToFont).contains_key(&pat) {
                if (*real_self).cachedAll {
                    // failed to find it via FC; add everything to our maps (potentially slow) as a last resort
                    let mut names: *mut XeTeXFontMgrNameCollection =
                        XeTeXFontMgr_readNames(self_0, pat);
                    XeTeXFontMgr_addToMaps(self_0, pat, names);
                    XeTeXFontMgrNameCollection_delete(names);
                } else {
                    let mut s: *mut libc::c_char = 0 as *mut libc::c_char;
                    let mut i: libc::c_int = 0;
                    i = 0i32;
                    let mut current_block: u64;
                    loop {
                        if !(FcPatternGetString(
                            pat,
                            b"fullname\x00" as *const u8 as *const libc::c_char,
                            i,
                            &mut s as *mut *mut libc::c_char as *mut *mut FcChar8,
                        ) as libc::c_uint
                            == FcResultMatch as libc::c_int as libc::c_uint)
                        {
                            current_block = 3437258052017859086;
                            break;
                        }
                        if CStr::from_ptr(name) == CStr::from_ptr(s) {
                            let mut names_0: *mut XeTeXFontMgrNameCollection =
                                XeTeXFontMgr_readNames(self_0, pat);
                            XeTeXFontMgr_addToMaps(self_0, pat, names_0);
                            XeTeXFontMgr_cacheFamilyMembers(self_0, (*names_0).m_familyNames);
                            XeTeXFontMgrNameCollection_delete(names_0);
                            found = 1i32 != 0;
                            current_block = 12209867499936983673;
                            break;
                        } else {
                            i += 1
                        }
                    }
                    match current_block {
                        12209867499936983673 => {}
                        _ => {
                            i = 0i32;
                            's_144: while FcPatternGetString(
                                pat,
                                b"family\x00" as *const u8 as *const libc::c_char,
                                i,
                                &mut s as *mut *mut libc::c_char as *mut *mut FcChar8,
                            ) as libc::c_uint
                                == FcResultMatch as libc::c_int as libc::c_uint
                            {
                                if CStr::from_ptr(name) == CStr::from_ptr(s)
                                    || hyph != 0 && (&**famName == CStr::from_ptr(s))
                                {
                                    let mut names_1: *mut XeTeXFontMgrNameCollection =
                                        XeTeXFontMgr_readNames(self_0, pat);
                                    XeTeXFontMgr_addToMaps(self_0, pat, names_1);
                                    XeTeXFontMgr_cacheFamilyMembers(
                                        self_0,
                                        (*names_1).m_familyNames,
                                    );
                                    XeTeXFontMgrNameCollection_delete(names_1);
                                    found = 1i32 != 0;
                                    break;
                                } else {
                                    let mut t: *mut libc::c_char = 0 as *mut libc::c_char;
                                    let mut j: libc::c_int = 0i32;
                                    while FcPatternGetString(
                                        pat,
                                        b"style\x00" as *const u8 as *const libc::c_char,
                                        j,
                                        &mut t as *mut *mut libc::c_char as *mut *mut FcChar8,
                                    ) as libc::c_uint
                                        == FcResultMatch as libc::c_int as libc::c_uint
                                    {
                                        let mut full: *mut CppStdString = CppStdString_create();
                                        CppStdString_append_const_char_ptr(full, s);
                                        CppStdString_append_const_char_ptr(
                                            full,
                                            b" \x00" as *const u8 as *const libc::c_char,
                                        );
                                        CppStdString_append_const_char_ptr(full, t);
                                        let mut matched: bool = &**full == CStr::from_ptr(name);
                                        CppStdString_delete(full);
                                        if matched {
                                            let mut names_2: *mut XeTeXFontMgrNameCollection =
                                                XeTeXFontMgr_readNames(self_0, pat);
                                            XeTeXFontMgr_addToMaps(self_0, pat, names_2);
                                            XeTeXFontMgr_cacheFamilyMembers(
                                                self_0,
                                                (*names_2).m_familyNames,
                                            );
                                            XeTeXFontMgrNameCollection_delete(names_2);
                                            found = 1i32 != 0;
                                            break 's_144;
                                        } else {
                                            j += 1
                                        }
                                    }
                                    i += 1
                                }
                            }
                        }
                    }
                }
            }
            f += 1
        }
        if found as libc::c_int != 0 || (*real_self).cachedAll as libc::c_int != 0 {
            break;
        }
        (*real_self).cachedAll = 1i32 != 0
    }
    CppStdString_delete(famName);
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_initialize(mut self_0: *mut XeTeXFontMgr) {
    let mut real_self: *mut XeTeXFontMgr_FC = self_0 as *mut XeTeXFontMgr_FC;
    if FcInit() == 0i32 {
        _tt_abort(b"fontconfig initialization failed\x00" as *const u8 as *const libc::c_char);
    }
    if gFreeTypeLibrary.is_null() && FT_Init_FreeType(&mut gFreeTypeLibrary) != 0i32 {
        _tt_abort(b"FreeType initialization failed\x00" as *const u8 as *const libc::c_char);
    }
    let mut err: icu::UErrorCode = icu::U_ZERO_ERROR;
    macRomanConv = icu::ucnv_open(
        b"macintosh\x00" as *const u8 as *const libc::c_char,
        &mut err,
    );
    utf16beConv = icu::ucnv_open(b"UTF16BE\x00" as *const u8 as *const libc::c_char, &mut err);
    utf8Conv = icu::ucnv_open(b"UTF8\x00" as *const u8 as *const libc::c_char, &mut err);
    if err as u64 != 0 {
        _tt_abort(b"cannot read font names\x00" as *const u8 as *const libc::c_char);
    }
    let mut pat: *mut FcPattern =
        FcNameParse(b":outline=true\x00" as *const u8 as *const libc::c_char as *const FcChar8);
    let mut os: *mut FcObjectSet = FcObjectSetBuild(
        b"family\x00" as *const u8 as *const libc::c_char,
        b"style\x00" as *const u8 as *const libc::c_char,
        b"file\x00" as *const u8 as *const libc::c_char,
        b"index\x00" as *const u8 as *const libc::c_char,
        b"fullname\x00" as *const u8 as *const libc::c_char,
        b"weight\x00" as *const u8 as *const libc::c_char,
        b"width\x00" as *const u8 as *const libc::c_char,
        b"slant\x00" as *const u8 as *const libc::c_char,
        b"fontformat\x00" as *const u8 as *const libc::c_char,
        0 as *mut libc::c_void,
    );
    (*real_self).allFonts = FcFontList(FcConfigGetCurrent(), pat, os);
    FcObjectSetDestroy(os);
    FcPatternDestroy(pat);
    (*real_self).cachedAll = 0i32 != 0;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_terminate(mut self_0: *mut XeTeXFontMgr) {
    let mut real_self: *mut XeTeXFontMgr_FC = self_0 as *mut XeTeXFontMgr_FC;
    FcFontSetDestroy((*real_self).allFonts);
    (*real_self).allFonts = 0 as *mut FcFontSet;
    if !macRomanConv.is_null() {
        icu::ucnv_close(macRomanConv);
        macRomanConv = 0 as *mut icu::UConverter
    }
    if !utf16beConv.is_null() {
        icu::ucnv_close(utf16beConv);
        utf16beConv = 0 as *mut icu::UConverter
    }
    if !utf8Conv.is_null() {
        icu::ucnv_close(utf8Conv);
        utf8Conv = 0 as *mut icu::UConverter
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_getPlatformFontDesc(
    mut _self_0: *const XeTeXFontMgr,
    mut font: PlatformFontRef,
) -> *mut libc::c_char {
    let mut s: *mut FcChar8 = 0 as *mut FcChar8;
    let mut path: *mut libc::c_char = 0 as *mut libc::c_char;
    if FcPatternGetString(
        font as *const FcPattern,
        b"file\x00" as *const u8 as *const libc::c_char,
        0i32,
        &mut s as *mut *mut FcChar8,
    ) as libc::c_uint
        == FcResultMatch as libc::c_int as libc::c_uint
    {
        path = strdup(s as *const libc::c_char)
    } else {
        path = strdup(b"[unknown]\x00" as *const u8 as *const libc::c_char)
    }
    return path;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_ctor(mut self_0: *mut XeTeXFontMgr_FC) {
    XeTeXFontMgr_base_ctor(&mut (*self_0).super_);
    (*self_0).super_.m_memfnInitialize =
        Some(XeTeXFontMgr_FC_initialize as unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ());
    (*self_0).super_.m_memfnTerminate =
        Some(XeTeXFontMgr_FC_terminate as unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ());
    (*self_0).super_.m_memfnGetOpSizeRecAndStyleFlags = Some(
        XeTeXFontMgr_FC_getOpSizeRecAndStyleFlags
            as unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *mut XeTeXFontMgrFont) -> (),
    );
    (*self_0).super_.m_memfnGetPlatformFontDesc = Some(
        XeTeXFontMgr_FC_getPlatformFontDesc
            as unsafe extern "C" fn(
                _: *const XeTeXFontMgr,
                _: PlatformFontRef,
            ) -> *mut libc::c_char,
    );
    (*self_0).super_.m_memfnSearchForHostPlatformFonts = Some(
        XeTeXFontMgr_FC_searchForHostPlatformFonts
            as unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *const libc::c_char) -> (),
    );
    (*self_0).super_.m_memfnReadNames = Some(
        XeTeXFontMgr_FC_readNames
            as unsafe extern "C" fn(
                _: *mut XeTeXFontMgr,
                _: *mut FcPattern,
            ) -> *mut XeTeXFontMgrNameCollection,
    );
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_FC_create() -> *mut XeTeXFontMgr_FC {
    let mut self_0: *mut XeTeXFontMgr_FC =
        malloc(::std::mem::size_of::<XeTeXFontMgr_FC>() as libc::c_ulong) as *mut XeTeXFontMgr_FC;
    XeTeXFontMgr_FC_ctor(self_0);
    return self_0;
}
