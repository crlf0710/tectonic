#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

use crate::core_memory::xmalloc;
use harfbuzz_sys::*;

extern "C" {
    pub type FT_LibraryRec_;
    pub type FT_DriverRec_;
    pub type FT_Face_InternalRec_;
    pub type FT_Size_InternalRec_;
    pub type FT_Slot_InternalRec_;
    pub type FT_SubGlyphRec_;
    pub type XeTeXFont_rec;
    pub type XeTeXLayoutEngine_rec;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn getFont(engine: XeTeXLayoutEngine) -> XeTeXFont;
    #[no_mangle]
    fn getGlyphHeightDepth(
        engine: XeTeXLayoutEngine,
        glyphID: uint32_t,
        height: *mut libc::c_float,
        depth: *mut libc::c_float,
    );
    #[no_mangle]
    fn Fix2D(f: Fixed) -> libc::c_double;
    #[no_mangle]
    fn D2Fix(d: libc::c_double) -> Fixed;
    #[no_mangle]
    static mut font_layout_engine: *mut *mut libc::c_void;
    #[no_mangle]
    static mut font_area: *mut int32_t;
    #[no_mangle]
    static mut font_size: *mut int32_t;
    #[no_mangle]
    fn XeTeXFontInst_getHbFont(self_0: *const XeTeXFontInst) -> *mut hb_font_t;
    #[no_mangle]
    fn XeTeXFontInst_unitsToPoints(
        self_0: *const XeTeXFontInst,
        units: libc::c_float,
    ) -> libc::c_float;
    #[no_mangle]
    fn XeTeXFontInst_pointsToUnits(
        self_0: *const XeTeXFontInst,
        points: libc::c_float,
    ) -> libc::c_float;
}
pub type size_t = usize;
pub type int32_t = i32;
pub type uint32_t = u32;
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
/* *************************************************************************
 *
 * @section:
 *   module_management
 *
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Module
 *
 * @description:
 *   A handle to a given FreeType module object.  A module can be a font
 *   driver, a renderer, or anything else that provides services to the
 *   former.
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Driver
 *
 * @description:
 *   A handle to a given FreeType font driver object.  A font driver is a
 *   module capable of creating faces from font files.
 */
/* *************************************************************************
 *
 * @type:
 *   FT_Renderer
 *
 * @description:
 *   A handle to a given FreeType renderer.  A renderer is a module in
 *   charge of converting a glyph's outline image to a bitmap.  It supports
 *   a single glyph image format, and one or more target surface depths.
 */
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
/* *************************************************************************
 *
 * @type:
 *   FT_Size
 *
 * @description:
 *   A handle to an object that models a face scaled to a given character
 *   size.
 *
 * @note:
 *   An @FT_Face has one _active_ @FT_Size object that is used by functions
 *   like @FT_Load_Glyph to determine the scaling transformation that in
 *   turn is used to load and hint glyphs and metrics.
 *
 *   You can use @FT_Set_Char_Size, @FT_Set_Pixel_Sizes, @FT_Request_Size
 *   or even @FT_Select_Size to change the content (i.e., the scaling
 *   values) of the active @FT_Size.
 *
 *   You can use @FT_New_Size to create additional size objects for a given
 *   @FT_Face, but they won't be used by other functions until you activate
 *   it through @FT_Activate_Size.  Only one size can be activated at any
 *   given time per face.
 *
 * @also:
 *   See @FT_SizeRec for the publicly accessible fields of a given size
 *   object.
 */
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

pub const HB_OT_MATH_GLYPH_PART_FLAG_EXTENDER: hb_ot_math_glyph_part_flags_t = 1;

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

pub type Fixed = i32;
pub type XeTeXFont = *mut XeTeXFont_rec;
pub type XeTeXLayoutEngine = *mut XeTeXLayoutEngine_rec;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct GlyphAssembly {
    pub count: libc::c_uint,
    pub parts: *mut hb_ot_math_glyph_part_t,
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
#[no_mangle]
pub unsafe extern "C" fn get_ot_math_constant(
    mut f: libc::c_int,
    mut n: libc::c_int,
) -> libc::c_int {
    let mut constant: hb_ot_math_constant_t = n as hb_ot_math_constant_t;
    let mut rval: hb_position_t = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        rval = hb_ot_math_get_constant(hbFont, constant);
        /* scale according to font size, except the ones that are percentages */
        match constant as libc::c_uint {
            0 | 1 | 55 => {}
            _ => {
                rval = D2Fix(
                    XeTeXFontInst_unitsToPoints(font, rval as libc::c_float) as libc::c_double
                )
            }
        }
    }
    return rval;
}
/* size of \.{\\atopwithdelims} delimiters in non-displays */
/* height of fraction lines above the baseline */
#[no_mangle]
pub static mut TeX_sym_to_OT_map: [hb_ot_math_constant_t; 23] = [
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP,
    HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP,
    HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP,
    HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN,
    HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN,
    HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP,
    HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP,
    HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED,
    HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN,
    HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN,
    HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX,
    HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN,
    HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT,
    4294967295 as hb_ot_math_constant_t,
    HB_OT_MATH_CONSTANT_AXIS_HEIGHT,
];
unsafe extern "C" fn min_int(mut a: libc::c_int, mut b: libc::c_int) -> libc::c_int {
    return if a < b { a } else { b };
}
#[no_mangle]
pub unsafe extern "C" fn get_native_mathsy_param(
    mut f: libc::c_int,
    mut n: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if n == 6i32 {
        rval = *font_size.offset(f as isize)
    } else if n == 21i32 {
        // XXX not sure what OT parameter we should use here;
        // for now we use 1.5em, clamped to delim1 height
        rval = min_int(
            (1.5f64 * *font_size.offset(f as isize) as libc::c_double) as libc::c_int,
            get_native_mathsy_param(f, 20i32),
        )
    } else if n
        < (::std::mem::size_of::<[hb_ot_math_constant_t; 23]>() as libc::c_ulong)
            .wrapping_div(::std::mem::size_of::<hb_ot_math_constant_t>() as libc::c_ulong)
            as libc::c_int
    {
        let mut ot_index: hb_ot_math_constant_t = TeX_sym_to_OT_map[n as usize];
        if ot_index as libc::c_uint != 4294967295 as hb_ot_math_constant_t as libc::c_uint {
            rval = get_ot_math_constant(f, ot_index as libc::c_int)
        }
    }
    //  fprintf(stderr, " math_sy(%d, %d) returns %.3f\n", f, n, Fix2D(rval));
    return rval;
}
/* fontdimen IDs for math extension font (family 3) */
/* thickness of \.{\\over} bars */
/* minimum clearance above a displayed op */
/* minimum clearance below a displayed op */
/* minimum baselineskip above displayed op */
/* minimum baselineskip below displayed op */
/* padding above and below displayed limits */
#[no_mangle]
pub static mut TeX_ext_to_OT_map: [hb_ot_math_constant_t; 14] = [
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT,
    4294967295 as hb_ot_math_constant_t,
    4294967295 as hb_ot_math_constant_t,
    HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS,
    HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN,
    HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN,
    HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN,
    HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN,
    HB_OT_MATH_CONSTANT_STACK_GAP_MIN,
];
#[no_mangle]
pub unsafe extern "C" fn get_native_mathex_param(
    mut f: libc::c_int,
    mut n: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if n == 6i32 {
        rval = *font_size.offset(f as isize)
    } else if n
        < (::std::mem::size_of::<[hb_ot_math_constant_t; 14]>() as libc::c_ulong)
            .wrapping_div(::std::mem::size_of::<hb_ot_math_constant_t>() as libc::c_ulong)
            as libc::c_int
    {
        let mut ot_index: hb_ot_math_constant_t = TeX_ext_to_OT_map[n as usize];
        if ot_index as libc::c_uint != 4294967295 as hb_ot_math_constant_t as libc::c_uint {
            rval = get_ot_math_constant(f, ot_index as libc::c_int)
        }
    }
    //  fprintf(stderr, " math_ex(%d, %d) returns %.3f\n", f, n, Fix2D(rval));
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn get_ot_math_variant(
    mut f: libc::c_int,
    mut g: libc::c_int,
    mut v: libc::c_int,
    mut adv: *mut int32_t,
    mut horiz: libc::c_int,
) -> libc::c_int {
    let mut rval: hb_codepoint_t = g as hb_codepoint_t;
    *adv = -1i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        let mut variant: [hb_ot_math_glyph_variant_t; 1] = [hb_ot_math_glyph_variant_t {
            glyph: 0,
            advance: 0,
        }; 1];
        let mut count: libc::c_uint = 1i32 as libc::c_uint;
        hb_ot_math_get_glyph_variants(
            hbFont,
            g as hb_codepoint_t,
            (if horiz != 0 {
                HB_DIRECTION_RTL as libc::c_int
            } else {
                HB_DIRECTION_TTB as libc::c_int
            }) as hb_direction_t,
            v as libc::c_uint,
            &mut count,
            variant.as_mut_ptr(),
        );
        if count > 0i32 as libc::c_uint {
            rval = (*variant.as_mut_ptr()).glyph;
            *adv = D2Fix(XeTeXFontInst_unitsToPoints(
                font,
                (*variant.as_mut_ptr()).advance as libc::c_float,
            ) as libc::c_double)
        }
    }
    return rval as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn get_ot_assembly_ptr(
    mut f: libc::c_int,
    mut g: libc::c_int,
    mut horiz: libc::c_int,
) -> *mut libc::c_void {
    let mut rval: *mut libc::c_void = 0 as *mut libc::c_void;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        let mut count: libc::c_uint = hb_ot_math_get_glyph_assembly(
            hbFont,
            g as hb_codepoint_t,
            (if horiz != 0 {
                HB_DIRECTION_RTL as libc::c_int
            } else {
                HB_DIRECTION_TTB as libc::c_int
            }) as hb_direction_t,
            0i32 as libc::c_uint,
            0 as *mut libc::c_uint,
            0 as *mut hb_ot_math_glyph_part_t,
            0 as *mut hb_position_t,
        );
        if count > 0i32 as libc::c_uint {
            let mut a: *mut GlyphAssembly =
                xmalloc(::std::mem::size_of::<GlyphAssembly>() as _) as *mut GlyphAssembly;
            (*a).count = count;
            (*a).parts = xmalloc(
                (count as libc::c_ulong)
                    .wrapping_mul(::std::mem::size_of::<hb_ot_math_glyph_part_t>() as libc::c_ulong)
                    as _,
            ) as *mut hb_ot_math_glyph_part_t;
            hb_ot_math_get_glyph_assembly(
                hbFont,
                g as hb_codepoint_t,
                (if horiz != 0 {
                    HB_DIRECTION_RTL as libc::c_int
                } else {
                    HB_DIRECTION_TTB as libc::c_int
                }) as hb_direction_t,
                0i32 as libc::c_uint,
                &mut (*a).count,
                (*a).parts,
                0 as *mut hb_position_t,
            );
            rval = a as *mut libc::c_void
        }
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn free_ot_assembly(mut a: *mut GlyphAssembly) {
    if a.is_null() {
        return;
    }
    free((*a).parts as *mut libc::c_void);
    free(a as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn get_ot_math_ital_corr(
    mut f: libc::c_int,
    mut g: libc::c_int,
) -> libc::c_int {
    let mut rval: hb_position_t = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        rval = hb_ot_math_get_glyph_italics_correction(hbFont, g as hb_codepoint_t);
        rval = D2Fix(XeTeXFontInst_unitsToPoints(font, rval as libc::c_float) as libc::c_double)
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn get_ot_math_accent_pos(
    mut f: libc::c_int,
    mut g: libc::c_int,
) -> libc::c_int {
    let mut rval: hb_position_t = 0x7fffffffu64 as hb_position_t;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        rval = hb_ot_math_get_glyph_top_accent_attachment(hbFont, g as hb_codepoint_t);
        rval = D2Fix(XeTeXFontInst_unitsToPoints(font, rval as libc::c_float) as libc::c_double)
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn ot_min_connector_overlap(mut f: libc::c_int) -> libc::c_int {
    let mut rval: hb_position_t = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        rval = hb_ot_math_get_min_connector_overlap(hbFont, HB_DIRECTION_RTL);
        rval = D2Fix(XeTeXFontInst_unitsToPoints(font, rval as libc::c_float) as libc::c_double)
    }
    return rval;
}
unsafe extern "C" fn getMathKernAt(
    mut f: libc::c_int,
    mut g: libc::c_int,
    mut side: hb_ot_math_kern_t,
    mut height: libc::c_int,
) -> libc::c_int {
    let mut rval: hb_position_t = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font);
        rval = hb_ot_math_get_glyph_kerning(hbFont, g as hb_codepoint_t, side, height)
    }
    return rval;
}
unsafe extern "C" fn glyph_height(mut f: libc::c_int, mut g: libc::c_int) -> libc::c_float {
    let mut rval: libc::c_float = 0.0f64 as libc::c_float;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut engine: XeTeXLayoutEngine =
            *font_layout_engine.offset(f as isize) as XeTeXLayoutEngine;
        getGlyphHeightDepth(engine, g as uint32_t, &mut rval, 0 as *mut libc::c_float);
    }
    return rval;
}
unsafe extern "C" fn glyph_depth(mut f: libc::c_int, mut g: libc::c_int) -> libc::c_float {
    let mut rval: libc::c_float = 0.0f64 as libc::c_float;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut engine: XeTeXLayoutEngine =
            *font_layout_engine.offset(f as isize) as XeTeXLayoutEngine;
        getGlyphHeightDepth(engine, g as uint32_t, 0 as *mut libc::c_float, &mut rval);
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn get_ot_math_kern(
    mut f: libc::c_int,
    mut g: libc::c_int,
    mut sf: libc::c_int,
    mut sg: libc::c_int,
    mut cmd: libc::c_int,
    mut shift: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        let mut kern: libc::c_int = 0i32;
        let mut skern: libc::c_int = 0i32;
        let mut corr_height_top: libc::c_float = 0.0f64 as libc::c_float;
        let mut corr_height_bot: libc::c_float = 0.0f64 as libc::c_float;
        if cmd == 0i32 {
            // superscript
            corr_height_top = XeTeXFontInst_pointsToUnits(font, glyph_height(f, g));
            corr_height_bot = -XeTeXFontInst_pointsToUnits(
                font,
                (glyph_depth(sf, sg) as libc::c_double + Fix2D(shift)) as libc::c_float,
            );
            kern = getMathKernAt(
                f,
                g,
                HB_OT_MATH_KERN_TOP_RIGHT,
                corr_height_top as libc::c_int,
            );
            skern = getMathKernAt(
                sf,
                sg,
                HB_OT_MATH_KERN_BOTTOM_LEFT,
                corr_height_top as libc::c_int,
            );
            rval = kern + skern;
            kern = getMathKernAt(
                f,
                g,
                HB_OT_MATH_KERN_TOP_RIGHT,
                corr_height_bot as libc::c_int,
            );
            skern = getMathKernAt(
                sf,
                sg,
                HB_OT_MATH_KERN_BOTTOM_LEFT,
                corr_height_bot as libc::c_int,
            );
            if kern + skern < rval {
                rval = kern + skern
            }
        } else if cmd == 1i32 {
            // subscript
            corr_height_top = XeTeXFontInst_pointsToUnits(
                font,
                (glyph_height(sf, sg) as libc::c_double - Fix2D(shift)) as libc::c_float,
            );
            corr_height_bot = -XeTeXFontInst_pointsToUnits(font, glyph_depth(f, g));
            kern = getMathKernAt(
                f,
                g,
                HB_OT_MATH_KERN_BOTTOM_RIGHT,
                corr_height_top as libc::c_int,
            );
            skern = getMathKernAt(
                sf,
                sg,
                HB_OT_MATH_KERN_TOP_LEFT,
                corr_height_top as libc::c_int,
            );
            rval = kern + skern;
            kern = getMathKernAt(
                f,
                g,
                HB_OT_MATH_KERN_BOTTOM_RIGHT,
                corr_height_bot as libc::c_int,
            );
            skern = getMathKernAt(
                sf,
                sg,
                HB_OT_MATH_KERN_TOP_LEFT,
                corr_height_bot as libc::c_int,
            );
            if kern + skern < rval {
                rval = kern + skern
            }
        } else {
            unreachable!()
            // we should not reach here
        }
        return D2Fix(XeTeXFontInst_unitsToPoints(font, rval as libc::c_float) as libc::c_double);
    }
    return 0i32;
}
#[no_mangle]
pub unsafe extern "C" fn ot_part_count(mut a: *const GlyphAssembly) -> libc::c_int {
    return (*a).count as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn ot_part_glyph(
    mut a: *const GlyphAssembly,
    mut i: libc::c_int,
) -> libc::c_int {
    return (*(*a).parts.offset(i as isize)).glyph as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn ot_part_is_extender(
    mut a: *const GlyphAssembly,
    mut i: libc::c_int,
) -> bool {
    return (*(*a).parts.offset(i as isize)).flags as libc::c_uint
        & HB_OT_MATH_GLYPH_PART_FLAG_EXTENDER as libc::c_int as libc::c_uint
        != 0i32 as libc::c_uint;
}
#[no_mangle]
pub unsafe extern "C" fn ot_part_start_connector(
    mut f: libc::c_int,
    mut a: *const GlyphAssembly,
    mut i: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        rval = D2Fix(XeTeXFontInst_unitsToPoints(
            font,
            (*(*a).parts.offset(i as isize)).start_connector_length as libc::c_float,
        ) as libc::c_double)
    }
    return rval;
}
#[no_mangle]
pub unsafe extern "C" fn ot_part_end_connector(
    mut f: libc::c_int,
    mut a: *const GlyphAssembly,
    mut i: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        rval = D2Fix(XeTeXFontInst_unitsToPoints(
            font,
            (*(*a).parts.offset(i as isize)).end_connector_length as libc::c_float,
        ) as libc::c_double)
    }
    return rval;
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
#[no_mangle]
pub unsafe extern "C" fn ot_part_full_advance(
    mut f: libc::c_int,
    mut a: *const GlyphAssembly,
    mut i: libc::c_int,
) -> libc::c_int {
    let mut rval: libc::c_int = 0i32;
    if *font_area.offset(f as isize) as libc::c_uint == 0xfffeu32 {
        let mut font: *mut XeTeXFontInst =
            getFont(*font_layout_engine.offset(f as isize) as XeTeXLayoutEngine)
                as *mut XeTeXFontInst;
        rval = D2Fix(XeTeXFontInst_unitsToPoints(
            font,
            (*(*a).parts.offset(i as isize)).full_advance as libc::c_float,
        ) as libc::c_double)
    }
    return rval;
}
