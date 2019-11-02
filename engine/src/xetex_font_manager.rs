#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

#[cfg(not(target_os = "macos"))]
#[path = "xetex_font_manager_fontconfig.rs"]
pub mod imp;

#[cfg(target_os = "macos")]
#[path = "xetex_font_manager_coretext.rs"]
pub mod imp;

use std::ffi::CString;
use std::ptr::NonNull;

use crate::core_memory::xmalloc;

use crate::xetex_layout_interface::collection_types::*;

#[cfg(target_os = "macos")]
use crate::xetex_layout_interface::__CTFontDescriptor;

use harfbuzz_sys::{hb_font_t, hb_face_t, hb_font_get_face, hb_ot_layout_get_size_params};

#[allow(improper_ctypes)]
extern "C" {
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
    pub type XeTeXFont_rec;
    #[no_mangle]
    fn tan(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn labs(_: libc::c_long) -> libc::c_long;
    #[no_mangle]
    fn strcpy(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strncmp(_: *const libc::c_char, _: *const libc::c_char, _: libc::c_ulong) -> libc::c_int;
    #[no_mangle]
    fn strchr(_: *const libc::c_char, _: libc::c_int) -> *mut libc::c_char;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    /* The internal, C/C++ interface: */
    #[no_mangle]
    fn _tt_abort(format: *const libc::c_char, _: ...) -> !;
    #[no_mangle]
    fn createFont(fontRef: PlatformFontRef, pointSize: Fixed) -> XeTeXFont;
    #[no_mangle]
    fn deleteFont(font: XeTeXFont);
    #[no_mangle]
    static mut loaded_font_design_size: Fixed;
    #[no_mangle]
    fn get_tracing_fonts_state() -> libc::c_int;
    #[no_mangle]
    fn end_diagnostic(nl: libc::c_int);
    #[no_mangle]
    fn begin_diagnostic();
    #[no_mangle]
    fn print_char(c: libc::c_int);
    #[no_mangle]
    fn print_nl(s: libc::c_int);
    #[no_mangle]
    fn Fix2D(f: Fixed) -> libc::c_double;
    #[no_mangle]
    #[cfg(not(target_os = "macos"))]
    fn XeTeXFontMgr_FC_create() -> *mut XeTeXFontMgr_FC;
    #[no_mangle]
    #[cfg(target_os = "macos")]
    fn XeTeXFontMgr_Mac_create() -> *mut XeTeXFontMgr_Mac;
    fn XeTeXFontInst_getHbFont(self_0: *const XeTeXFontInst) -> *mut hb_font_t;
    #[no_mangle]
    fn XeTeXFontInst_getFontTableFT(
        self_0: *const XeTeXFontInst,
        tag: FT_Sfnt_Tag,
    ) -> *mut libc::c_void;
}
pub type size_t = usize;
pub type int16_t = i16;
pub type int32_t = i32;
pub type uint16_t = u16;
pub type uint32_t = u32;
pub type ssize_t = isize;

#[cfg(not(target_os = "macos"))]
use imp::FcPattern;

#[cfg(not(target_os = "macos"))]
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _FcFontSet {
    pub nfont: libc::c_int,
    pub sfont: libc::c_int,
    pub fonts: *mut *mut FcPattern,
}

#[cfg(not(target_os = "macos"))]
pub type FcFontSet = _FcFontSet;
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
/* unsigned distance */
/* *************************************************************************
 *
 * @type:
 *   FT_Char
 *
 * @description:
 *   A simple typedef for the _signed_ char type.
 */
pub type FT_Char = libc::c_schar;
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
 *   FT_ULong
 *
 * @description:
 *   A typedef for unsigned long.
 */
pub type FT_ULong = libc::c_ulong;
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
#[derive(Copy, Clone)]
#[repr(C)]
pub struct TT_Header_ {
    pub Table_Version: FT_Fixed,
    pub Font_Revision: FT_Fixed,
    pub CheckSum_Adjust: FT_Long,
    pub Magic_Number: FT_Long,
    pub Flags: FT_UShort,
    pub Units_Per_EM: FT_UShort,
    pub Created: [FT_ULong; 2],
    pub Modified: [FT_ULong; 2],
    pub xMin: FT_Short,
    pub yMin: FT_Short,
    pub xMax: FT_Short,
    pub yMax: FT_Short,
    pub Mac_Style: FT_UShort,
    pub Lowest_Rec_PPEM: FT_UShort,
    pub Font_Direction: FT_Short,
    pub Index_To_Loc_Format: FT_Short,
    pub Glyph_Data_Format: FT_Short,
}
pub type TT_Header = TT_Header_;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct TT_OS2_ {
    pub version: FT_UShort,
    pub xAvgCharWidth: FT_Short,
    pub usWeightClass: FT_UShort,
    pub usWidthClass: FT_UShort,
    pub fsType: FT_UShort,
    pub ySubscriptXSize: FT_Short,
    pub ySubscriptYSize: FT_Short,
    pub ySubscriptXOffset: FT_Short,
    pub ySubscriptYOffset: FT_Short,
    pub ySuperscriptXSize: FT_Short,
    pub ySuperscriptYSize: FT_Short,
    pub ySuperscriptXOffset: FT_Short,
    pub ySuperscriptYOffset: FT_Short,
    pub yStrikeoutSize: FT_Short,
    pub yStrikeoutPosition: FT_Short,
    pub sFamilyClass: FT_Short,
    pub panose: [FT_Byte; 10],
    pub ulUnicodeRange1: FT_ULong,
    pub ulUnicodeRange2: FT_ULong,
    pub ulUnicodeRange3: FT_ULong,
    pub ulUnicodeRange4: FT_ULong,
    pub achVendID: [FT_Char; 4],
    pub fsSelection: FT_UShort,
    pub usFirstCharIndex: FT_UShort,
    pub usLastCharIndex: FT_UShort,
    pub sTypoAscender: FT_Short,
    pub sTypoDescender: FT_Short,
    pub sTypoLineGap: FT_Short,
    pub usWinAscent: FT_UShort,
    pub usWinDescent: FT_UShort,
    pub ulCodePageRange1: FT_ULong,
    pub ulCodePageRange2: FT_ULong,
    pub sxHeight: FT_Short,
    pub sCapHeight: FT_Short,
    pub usDefaultChar: FT_UShort,
    pub usBreakChar: FT_UShort,
    pub usMaxContext: FT_UShort,
    pub usLowerOpticalPointSize: FT_UShort,
    pub usUpperOpticalPointSize: FT_UShort,
}
pub type TT_OS2 = TT_OS2_;
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
pub type FT_Sfnt_Tag_ = libc::c_uint;
pub const FT_SFNT_MAX: FT_Sfnt_Tag_ = 7;
pub const FT_SFNT_PCLT: FT_Sfnt_Tag_ = 6;
pub const FT_SFNT_POST: FT_Sfnt_Tag_ = 5;
pub const FT_SFNT_VHEA: FT_Sfnt_Tag_ = 4;
pub const FT_SFNT_HHEA: FT_Sfnt_Tag_ = 3;
pub const FT_SFNT_OS2: FT_Sfnt_Tag_ = 2;
pub const FT_SFNT_MAXP: FT_Sfnt_Tag_ = 1;
pub const FT_SFNT_HEAD: FT_Sfnt_Tag_ = 0;
pub type FT_Sfnt_Tag = FT_Sfnt_Tag_;
pub type Fixed = i32;
#[cfg(not(target_os = "macos"))]
pub type PlatformFontRef = *mut FcPattern;

#[cfg(target_os = "macos")]
pub type PlatformFontRef = CTFontDescriptorRef;
#[cfg(target_os = "macos")]
pub type CTFontDescriptorRef = *const __CTFontDescriptor;

pub type XeTeXFont = *mut XeTeXFont_rec;
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
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgrOpSizeRec {
    pub designSize: libc::c_uint,
    pub subFamilyID: libc::c_uint,
    pub nameCode: libc::c_uint,
    pub minSize: libc::c_uint,
    pub maxSize: libc::c_uint,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgrFamily {
    pub styles: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    pub minWeight: uint16_t,
    pub maxWeight: uint16_t,
    pub minWidth: uint16_t,
    pub maxWidth: uint16_t,
    pub minSlant: int16_t,
    pub maxSlant: int16_t,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgrFont {
    pub m_fullName: *mut CppStdString,
    pub m_psName: *mut CppStdString,
    pub m_familyName: *mut CppStdString,
    pub m_styleName: *mut CppStdString,
    pub parent: *mut XeTeXFontMgrFamily,
    pub fontRef: PlatformFontRef,
    pub opSizeInfo: XeTeXFontMgrOpSizeRec,
    pub weight: uint16_t,
    pub width: uint16_t,
    pub slant: int16_t,
    pub isReg: bool,
    pub isBold: bool,
    pub isItalic: bool,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgrNameCollection {
    pub m_familyNames: *mut CppStdListOfString,
    pub m_styleNames: *mut CppStdListOfString,
    pub m_fullNames: *mut CppStdListOfString,
    pub m_psName: *mut CppStdString,
    pub m_subFamily: *mut CppStdString,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgr {
    pub m_subdtor: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub m_memfnInitialize: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub m_memfnTerminate: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub m_memfnGetPlatformFontDesc: Option<
        unsafe extern "C" fn(_: *const XeTeXFontMgr, _: PlatformFontRef) -> *mut libc::c_char,
    >,
    pub m_memfnGetOpSizeRecAndStyleFlags:
        Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *mut XeTeXFontMgrFont) -> ()>,
    pub m_memfnSearchForHostPlatformFonts:
        Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *const libc::c_char) -> ()>,
    pub m_memfnReadNames: Option<
        unsafe extern "C" fn(
            _: *mut XeTeXFontMgr,
            _: PlatformFontRef,
        ) -> *mut XeTeXFontMgrNameCollection,
    >,
    pub m_nameToFont: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    pub m_nameToFamily: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFamily>>,
    pub m_platformRefToFont: *mut CppStdMap<PlatformFontRef, NonNull<XeTeXFontMgrFont>>,
    pub m_psNameToFont: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    // maps PS name (as used in .xdv) to font record
}

#[cfg(not(target_os = "macos"))]
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgr_FC {
    pub super_: XeTeXFontMgr,
    pub allFonts: *mut FcFontSet,
    pub cachedAll: bool,
}

#[cfg(target_os = "macos")]
#[derive(Copy, Clone)]
#[repr(C)]
pub struct XeTeXFontMgr_Mac {
    pub super_: XeTeXFontMgr,
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
#[inline]
unsafe extern "C" fn XeTeXFontMgrFamily_create() -> *mut XeTeXFontMgrFamily {
    let mut self_0: *mut XeTeXFontMgrFamily =
        malloc(::std::mem::size_of::<XeTeXFontMgrFamily>() as libc::c_ulong)
            as *mut XeTeXFontMgrFamily; /* default to 10bp */
    (*self_0).minWeight = 0i32 as uint16_t;
    (*self_0).maxWeight = 0i32 as uint16_t;
    (*self_0).minWidth = 0i32 as uint16_t;
    (*self_0).maxWidth = 0i32 as uint16_t;
    (*self_0).minSlant = 0i32 as int16_t;
    (*self_0).maxSlant = 0i32 as int16_t;
    (*self_0).styles = CppStdMap_create();
    return self_0;
}
#[inline]
unsafe extern "C" fn XeTeXFontMgrFont_create(mut ref_0: PlatformFontRef) -> *mut XeTeXFontMgrFont {
    let mut self_0: *mut XeTeXFontMgrFont =
        malloc(::std::mem::size_of::<XeTeXFontMgrFont>() as libc::c_ulong) as *mut XeTeXFontMgrFont;
    (*self_0).m_fullName = 0 as *mut CppStdString;
    (*self_0).m_psName = 0 as *mut CppStdString;
    (*self_0).m_familyName = 0 as *mut CppStdString;
    (*self_0).m_styleName = 0 as *mut CppStdString;
    (*self_0).parent = 0 as *mut XeTeXFontMgrFamily;
    (*self_0).fontRef = ref_0;
    (*self_0).weight = 0i32 as uint16_t;
    (*self_0).width = 0i32 as uint16_t;
    (*self_0).slant = 0i32 as int16_t;
    (*self_0).isReg = 0i32 != 0;
    (*self_0).isBold = 0i32 != 0;
    (*self_0).isItalic = 0i32 != 0;
    (*self_0).opSizeInfo.subFamilyID = 0i32 as libc::c_uint;
    (*self_0).opSizeInfo.designSize = 100i32 as libc::c_uint;
    return self_0;
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_initialize(mut self_0: *mut XeTeXFontMgr) {
    (*self_0)
        .m_memfnInitialize
        .expect("non-null function pointer")(self_0);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_terminate(mut self_0: *mut XeTeXFontMgr) {
    (*self_0)
        .m_memfnTerminate
        .expect("non-null function pointer")(self_0);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_getPlatformFontDesc(
    mut self_0: *const XeTeXFontMgr,
    mut font: PlatformFontRef,
) -> *mut libc::c_char {
    return (*self_0)
        .m_memfnGetPlatformFontDesc
        .expect("non-null function pointer")(self_0, font);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_searchForHostPlatformFonts(
    mut self_0: *mut XeTeXFontMgr,
    mut name: *const libc::c_char,
) {
    (*self_0)
        .m_memfnSearchForHostPlatformFonts
        .expect("non-null function pointer")(self_0, name);
}
#[inline]
unsafe extern "C" fn XeTeXFontMgr_getOpSizeRecAndStyleFlags(
    mut self_0: *mut XeTeXFontMgr,
    mut theFont: *mut XeTeXFontMgrFont,
) {
    (*self_0)
        .m_memfnGetOpSizeRecAndStyleFlags
        .expect("non-null function pointer")(self_0, theFont);
}
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009-2014 by Jonathan Kew

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
// see cpascal.h
#[no_mangle]
pub static mut XeTeXFontMgr_sFontManager: *mut XeTeXFontMgr =
    0 as *const XeTeXFontMgr as *mut XeTeXFontMgr;
#[no_mangle]
pub static mut XeTeXFontMgr_sReqEngine: libc::c_char = 0i32 as libc::c_char;
/* use our own fmax function because it seems to be missing on certain platforms
(solaris2.9, at least) */
#[inline]
unsafe extern "C" fn my_fmax(mut x: libc::c_double, mut y: libc::c_double) -> libc::c_double {
    return if x > y { x } else { y };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_GetFontManager() -> *mut XeTeXFontMgr {
    #[cfg(not(target_os = "macos"))]
    {
        if XeTeXFontMgr_sFontManager.is_null() {
            XeTeXFontMgr_sFontManager = &mut (*(XeTeXFontMgr_FC_create
                as unsafe extern "C" fn() -> *mut XeTeXFontMgr_FC)(
            ))
            .super_;
            XeTeXFontMgr_initialize(XeTeXFontMgr_sFontManager);
        }
    }
    #[cfg(target_os = "macos")]
    {
        if XeTeXFontMgr_sFontManager.is_null() {
            XeTeXFontMgr_sFontManager = &mut (*(XeTeXFontMgr_Mac_create
                as unsafe extern "C" fn() -> *mut XeTeXFontMgr_Mac)(
            ))
            .super_;
            XeTeXFontMgr_initialize(XeTeXFontMgr_sFontManager);
        }
    }
    return XeTeXFontMgr_sFontManager;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_Terminate() {
    if !XeTeXFontMgr_sFontManager.is_null() {
        XeTeXFontMgr_terminate(XeTeXFontMgr_sFontManager);
        // we don't actually deallocate the manager, just ask it to clean up
        // any auxiliary data such as the cocoa pool or freetype/fontconfig stuff
        // as we still need to access font names after this is called
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_Destroy() {
    // Here we actually fully destroy the font manager.
    if !XeTeXFontMgr_sFontManager.is_null() {
        XeTeXFontMgr_delete(XeTeXFontMgr_sFontManager);
        XeTeXFontMgr_sFontManager = 0 as *mut XeTeXFontMgr
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_getReqEngine(
    mut _self_0: *const XeTeXFontMgr,
) -> libc::c_char {
    // return the requested rendering technology for the most recent findFont
    // or 0 if no specific technology was requested
    return XeTeXFontMgr_sReqEngine;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_setReqEngine(
    mut _self_0: *const XeTeXFontMgr,
    mut reqEngine: libc::c_char,
) {
    XeTeXFontMgr_sReqEngine = reqEngine;
}
// above are singleton operation.
// /////////////
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_delete(mut self_0: *mut XeTeXFontMgr) {
    if self_0.is_null() {
        return;
    }
    if (*self_0).m_subdtor.is_some() {
        (*self_0).m_subdtor.expect("non-null function pointer")(self_0);
    }
    CppStdMap_delete((*self_0).m_nameToFont);
    CppStdMap_delete((*self_0).m_nameToFamily);
    CppStdMap_delete((*self_0).m_platformRefToFont);
    CppStdMap_delete((*self_0).m_psNameToFont);
    free(self_0 as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_findFont(
    mut self_0: *mut XeTeXFontMgr,
    mut name: *const libc::c_char,
    mut variant: *mut libc::c_char,
    mut ptSize: libc::c_double,
) -> PlatformFontRef {
    // 1st arg is name as specified by user (C string, UTF-8)
    // 2nd is /B/I/AAT/OT/ICU/GR/S=## qualifiers
    // 1. try name given as "full name"
    // 2. if there's a hyphen, split and try "family-style"
    // 3. try as PostScript name
    // 4. try name as family with "Regular/Plain/Normal" style
    // apply style qualifiers and optical sizing if present
    // SIDE EFFECT: sets sReqEngine to 'A' or 'O' or 'G' if appropriate,
    //   else clears it to 0
    // SIDE EFFECT: updates TeX variables /nameoffile/ and /namelength/,
    //   to match the actual font found
    // SIDE EFFECT: edits /variant/ string in-place removing /B or /I
    // ptSize is in TeX points, or negative for 'scaled' factor
    // "variant" string will be shortened (in-place) by removal of /B and /I if present
    let mut nameStr = CString::default();
    CppStdString_assign_from_const_char_ptr(&mut nameStr, name);
    let mut font: *mut XeTeXFontMgrFont = 0 as *mut XeTeXFontMgrFont;
    let mut dsize: libc::c_int = 100i32;
    loaded_font_design_size = 655360i64 as Fixed;
    for pass in 0..2i32 {
        // try full name as given
        if let Some(name_font_ptr) = (*(*self_0).m_nameToFont).get(&nameStr).cloned() {
            font = name_font_ptr.as_ptr();
            if (*font).opSizeInfo.designSize != 0i32 as libc::c_uint {
                dsize = (*font).opSizeInfo.designSize as libc::c_int
            }
            break;
        }
        // if there's a hyphen, split there and try Family-Style
        let mut nameStr_cstr: *const libc::c_char = CppStdString_cstr(&mut nameStr);
        let mut nameStr_len: libc::c_int = strlen(nameStr_cstr) as libc::c_int;
        let mut hyph_pos: *const libc::c_char = strchr(nameStr_cstr, '-' as i32);
        let mut hyph: libc::c_int = (if !hyph_pos.is_null() {
            hyph_pos.wrapping_offset_from(nameStr_cstr) as libc::c_long
        } else {
            -1i32 as libc::c_long
        }) as libc::c_int;
        if hyph > 0i32 && hyph < nameStr_len - 1i32 {
            let mut family = CString::default();
            CppStdString_assign_n_chars(&mut family, nameStr_cstr, hyph as size_t);
            if let Some(family_ptr) = (*(*self_0).m_nameToFamily).get(&family).cloned() {
                let mut style = CString::default();
                CppStdString_assign_n_chars(
                    &mut style,
                    nameStr_cstr.offset(hyph as isize).offset(1),
                    (nameStr_len - hyph - 1i32) as size_t,
                );
                if let Some(style_font_ptr) = (*(*family_ptr.as_ptr()).styles).get(&style).cloned()
                {
                    font = style_font_ptr.as_ptr();
                    if (*font).opSizeInfo.designSize != 0i32 as libc::c_uint {
                        dsize = (*font).opSizeInfo.designSize as libc::c_int
                    }
                    break;
                }
            }
        }
        // try as PostScript name
        if let Some(ps_font_ptr) = (*(*self_0).m_psNameToFont).get(&nameStr).cloned() {
            font = ps_font_ptr.as_ptr();
            if (*font).opSizeInfo.designSize != 0i32 as libc::c_uint {
                dsize = (*font).opSizeInfo.designSize as libc::c_int
            }
            break;
        }
        // try for the name as a family name
        if let Some(family_ptr) = (*(*self_0).m_nameToFamily).get(&nameStr).cloned() {
            let family_ptr = family_ptr.as_ptr();
            // look for a family member with the "regular" bit set in OS/2
            let mut regFonts: libc::c_int = 0i32;
            for (_k, v) in (*(*family_ptr).styles).iter() {
                if v.as_ref().isReg {
                    if regFonts == 0i32 {
                        font = v.as_ptr();
                    }
                    regFonts += 1
                }
            }
            // families with Ornament or similar fonts may flag those as Regular,
            // which confuses the search above... so try some known names
            if font.is_null() || regFonts > 1i32 {
                // try for style "Regular", "Plain", "Normal", "Roman"
                let regular_style_names = [
                    &b"Regular\x00"[..],
                    &b"Plain\x00"[..],
                    &b"Normal\x00"[..],
                    &b"Roman\x00"[..],
                ];
                'style_name_loop: for style in &regular_style_names {
                    use std::ffi::CStr;
                    let style: &[u8] = *style;
                    let style = CStr::from_ptr(style.as_ptr() as *const i8);
                    if let Some(style_font_ptr) = (*(*family_ptr).styles).get(style) {
                        font = style_font_ptr.as_ptr();
                        break 'style_name_loop;
                    }
                }
            }
            if font.is_null() {
                // look through the family for the (weight, width, slant) nearest to (80, 100, 0)
                font = XeTeXFontMgr_bestMatchFromFamily(self_0, family_ptr, 80i32, 100i32, 0i32)
            }
            if !font.is_null() {
                break;
            }
        }
        if pass == 0i32 {
            // didn't find it in our caches, so do a platform search (may be relatively expensive);
            // this will update the caches with any fonts that seem to match the name given,
            // so that the second pass might find it
            XeTeXFontMgr_searchForHostPlatformFonts(self_0, nameStr.as_ptr());
        }
    }
    if font.is_null() {
        return 0 as PlatformFontRef;
    }
    let mut parent: *mut XeTeXFontMgrFamily = (*font).parent;
    // if there are variant requests, try to apply them
    // and delete B, I, and S=... codes from the string, just retain /engine option
    XeTeXFontMgr_sReqEngine = 0i32 as libc::c_char;
    let mut reqBold: bool = 0i32 != 0;
    let mut reqItal: bool = 0i32 != 0;
    if !variant.is_null() {
        let mut varString: *mut CppStdString = CppStdString_create();
        let mut cp: *mut libc::c_char = variant;
        while *cp != 0 {
            if strncmp(
                cp,
                b"AAT\x00" as *const u8 as *const libc::c_char,
                3i32 as libc::c_ulong,
            ) == 0i32
            {
                XeTeXFontMgr_sReqEngine = 'A' as i32 as libc::c_char;
                cp = cp.offset(3);
                if CppStdString_length(varString) > 0
                    && CppStdString_last(varString) as libc::c_int != '/' as i32
                {
                    CppStdString_append_const_char_ptr(
                        varString,
                        b"/\x00" as *const u8 as *const libc::c_char,
                    );
                }
                CppStdString_append_const_char_ptr(
                    varString,
                    b"AAT\x00" as *const u8 as *const libc::c_char,
                );
            } else if strncmp(
                cp,
                b"ICU\x00" as *const u8 as *const libc::c_char,
                3i32 as libc::c_ulong,
            ) == 0i32
            {
                // for backword compatability
                XeTeXFontMgr_sReqEngine = 'O' as i32 as libc::c_char;
                cp = cp.offset(3);
                if CppStdString_length(varString) > 0
                    && CppStdString_last(varString) as libc::c_int != '/' as i32
                {
                    CppStdString_append_const_char_ptr(
                        varString,
                        b"/\x00" as *const u8 as *const libc::c_char,
                    );
                }
                CppStdString_append_const_char_ptr(
                    varString,
                    b"OT\x00" as *const u8 as *const libc::c_char,
                );
            } else if strncmp(
                cp,
                b"OT\x00" as *const u8 as *const libc::c_char,
                2i32 as libc::c_ulong,
            ) == 0i32
            {
                XeTeXFontMgr_sReqEngine = 'O' as i32 as libc::c_char;
                cp = cp.offset(2);
                if CppStdString_length(varString) > 0
                    && CppStdString_last(varString) as libc::c_int != '/' as i32
                {
                    CppStdString_append_const_char_ptr(
                        varString,
                        b"/\x00" as *const u8 as *const libc::c_char,
                    );
                }
                CppStdString_append_const_char_ptr(
                    varString,
                    b"OT\x00" as *const u8 as *const libc::c_char,
                );
            } else if strncmp(
                cp,
                b"GR\x00" as *const u8 as *const libc::c_char,
                2i32 as libc::c_ulong,
            ) == 0i32
            {
                XeTeXFontMgr_sReqEngine = 'G' as i32 as libc::c_char;
                cp = cp.offset(2);
                if CppStdString_length(varString) > 0
                    && CppStdString_last(varString) as libc::c_int != '/' as i32
                {
                    CppStdString_append_const_char_ptr(
                        varString,
                        b"/\x00" as *const u8 as *const libc::c_char,
                    );
                }
                CppStdString_append_const_char_ptr(
                    varString,
                    b"GR\x00" as *const u8 as *const libc::c_char,
                );
            } else if *cp as libc::c_int == 'S' as i32 {
                cp = cp.offset(1);
                if *cp as libc::c_int == '=' as i32 {
                    cp = cp.offset(1)
                }
                ptSize = 0.0f64;
                while *cp as libc::c_int >= '0' as i32 && *cp as libc::c_int <= '9' as i32 {
                    ptSize = ptSize * 10i32 as libc::c_double
                        + *cp as libc::c_int as libc::c_double
                        - '0' as i32 as libc::c_double;
                    cp = cp.offset(1)
                }
                if *cp as libc::c_int == '.' as i32 {
                    let mut dec: libc::c_double = 1.0f64;
                    cp = cp.offset(1);
                    while *cp as libc::c_int >= '0' as i32 && *cp as libc::c_int <= '9' as i32 {
                        dec = dec * 10.0f64;
                        ptSize = ptSize + (*cp as libc::c_int - '0' as i32) as libc::c_double / dec;
                        cp = cp.offset(1)
                    }
                }
            } else {
                loop
                /* if the code is "B" or "I", we skip putting it in varString */
                {
                    if *cp as libc::c_int == 'B' as i32 {
                        reqBold = 1i32 != 0;
                        cp = cp.offset(1)
                    } else {
                        if !(*cp as libc::c_int == 'I' as i32) {
                            break;
                        }
                        reqItal = 1i32 != 0;
                        cp = cp.offset(1)
                    }
                }
            }
            while *cp as libc::c_int != 0 && *cp as libc::c_int != '/' as i32 {
                cp = cp.offset(1)
            }
            if *cp as libc::c_int == '/' as i32 {
                cp = cp.offset(1)
            }
        }
        strcpy(variant, CppStdString_cstr(varString));
        CppStdString_delete(varString);
        if reqItal {
            let mut bestMatch: *mut XeTeXFontMgrFont = font;
            if ((*font).slant as libc::c_int) < (*parent).maxSlant as libc::c_int {
                // try for a face with more slant
                bestMatch = XeTeXFontMgr_bestMatchFromFamily(
                    self_0,
                    parent,
                    (*font).weight as libc::c_int,
                    (*font).width as libc::c_int,
                    (*parent).maxSlant as libc::c_int,
                )
            }
            if bestMatch == font && (*font).slant as libc::c_int > (*parent).minSlant as libc::c_int
            {
                // maybe the slant is negated, or maybe this was something like "Times-Italic/I"
                bestMatch = XeTeXFontMgr_bestMatchFromFamily(
                    self_0,
                    parent,
                    (*font).weight as libc::c_int,
                    (*font).width as libc::c_int,
                    (*parent).minSlant as libc::c_int,
                )
            }
            if (*parent).minWeight as libc::c_int == (*parent).maxWeight as libc::c_int
                && (*bestMatch).isBold as libc::c_int != (*font).isBold as libc::c_int
            {
                // try again using the bold flag, as we can't trust weight values
                let mut newBest: *mut XeTeXFontMgrFont = 0 as *mut XeTeXFontMgrFont;
                for (_, v) in (*(*parent).styles).iter() {
                    if v.as_ref().isBold == (*font).isBold {
                        if newBest.is_null() && v.as_ref().isItalic != (*font).isItalic {
                            newBest = v.as_ptr();
                            break;
                        }
                    }
                }
                if !newBest.is_null() {
                    bestMatch = newBest
                }
            }
            if bestMatch == font {
                // maybe slant values weren't present; try the style bits as a fallback
                bestMatch = 0 as *mut XeTeXFontMgrFont;
                for (_, v) in (*(*parent).styles).iter() {
                    let style_font_ptr = v.as_ptr();
                    if (*style_font_ptr).isItalic == !(*font).isItalic {
                        if (*parent).minWeight != (*parent).maxWeight {
                            // weight info was available, so try to match that
                            if bestMatch.is_null()
                                || XeTeXFontMgr_weightAndWidthDiff(self_0, style_font_ptr, font)
                                    < XeTeXFontMgr_weightAndWidthDiff(self_0, bestMatch, font)
                            {
                                bestMatch = style_font_ptr;
                            }
                        } else if bestMatch.is_null() && (*style_font_ptr).isBold == (*font).isBold
                        {
                            bestMatch = style_font_ptr;
                            break;
                            // no weight info, so try matching style bits
                            // found a match, no need to look further as we can't distinguish!
                        }
                    }
                }
            }
            if !bestMatch.is_null() {
                font = bestMatch
            }
        }
        if reqBold {
            // try for more boldness, with the same width and slant
            let mut bestMatch_0: *mut XeTeXFontMgrFont = font;
            if ((*font).weight as libc::c_int) < (*parent).maxWeight as libc::c_int {
                // try to increase weight by 1/2 x (max - min), rounding up
                bestMatch_0 = XeTeXFontMgr_bestMatchFromFamily(
                    self_0,
                    parent,
                    (*font).weight as libc::c_int
                        + ((*parent).maxWeight as libc::c_int - (*parent).minWeight as libc::c_int)
                            / 2i32
                        + 1i32,
                    (*font).width as libc::c_int,
                    (*font).slant as libc::c_int,
                );
                if (*parent).minSlant as libc::c_int == (*parent).maxSlant as libc::c_int {
                    // double-check the italic flag, as we can't trust slant values
                    let mut newBest_0: *mut XeTeXFontMgrFont = 0 as *mut XeTeXFontMgrFont;
                    for (_, v) in (*(*parent).styles).iter() {
                        let style_font_ptr = v.as_ptr();
                        if (*style_font_ptr).isItalic == (*font).isItalic {
                            if newBest_0.is_null()
                                || XeTeXFontMgr_weightAndWidthDiff(
                                    self_0,
                                    style_font_ptr,
                                    bestMatch_0,
                                ) < XeTeXFontMgr_weightAndWidthDiff(
                                    self_0,
                                    newBest_0,
                                    bestMatch_0,
                                )
                            {
                                newBest_0 = style_font_ptr;
                            }
                        }
                    }
                    if !newBest_0.is_null() {
                        bestMatch_0 = newBest_0
                    }
                }
            }
            if bestMatch_0 == font && !(*font).isBold {
                for (_, v) in (*(*parent).styles).iter() {
                    let style_font_ptr = v.as_ptr();
                    if (*style_font_ptr).isItalic == (*font).isItalic && (*style_font_ptr).isBold {
                        bestMatch_0 = style_font_ptr;
                        break;
                    }
                }
            }
            font = bestMatch_0
        }
    }
    // if there's optical size info, try to apply it
    if ptSize < 0.0f64 {
        ptSize = dsize as libc::c_double / 10.0f64
    } // convert to decipoints for comparison with the opSize values
    if !font.is_null() && (*font).opSizeInfo.subFamilyID != 0i32 as libc::c_uint && ptSize > 0.0f64
    {
        ptSize = ptSize * 10.0f64;
        let mut bestMismatch: libc::c_double = my_fmax(
            (*font).opSizeInfo.minSize as libc::c_double - ptSize,
            ptSize - (*font).opSizeInfo.maxSize as libc::c_double,
        );
        if bestMismatch > 0.0f64 {
            let mut bestMatch_1: *mut XeTeXFontMgrFont = font;
            for (_, v) in (*(*parent).styles).iter() {
                let style_font_ptr = v.as_ptr();
                if !((*style_font_ptr).opSizeInfo.subFamilyID != (*font).opSizeInfo.subFamilyID) {
                    let mut mismatch: libc::c_double = my_fmax(
                        (*style_font_ptr).opSizeInfo.minSize as libc::c_double - ptSize,
                        ptSize - (*style_font_ptr).opSizeInfo.maxSize as libc::c_double,
                    );
                    if mismatch < bestMismatch {
                        bestMatch_1 = style_font_ptr;
                        bestMismatch = mismatch
                    }
                    if bestMismatch <= 0.0f64 {
                        break;
                    }
                }
            }
            font = bestMatch_1
        }
    }
    if !font.is_null() && (*font).opSizeInfo.designSize != 0i32 as libc::c_uint {
        loaded_font_design_size =
            ((*font).opSizeInfo.designSize << 16i64).wrapping_div(10i32 as libc::c_uint) as Fixed
    }
    if get_tracing_fonts_state() > 0i32 {
        begin_diagnostic();
        print_nl(' ' as i32);
        let mut ch_ptr: *const libc::c_char = b"-> \x00" as *const u8 as *const libc::c_char;
        while *ch_ptr != 0 {
            let fresh0 = ch_ptr;
            ch_ptr = ch_ptr.offset(1);
            print_char(*fresh0 as libc::c_int);
        }
        let mut font_desc: *mut libc::c_char =
            XeTeXFontMgr_getPlatformFontDesc(self_0, (*font).fontRef);
        let mut ch_ptr_0: *const libc::c_char = font_desc;
        while *ch_ptr_0 != 0 {
            let fresh1 = ch_ptr_0;
            ch_ptr_0 = ch_ptr_0.offset(1);
            print_char(*fresh1 as libc::c_int);
        }
        free(font_desc as *mut libc::c_void);
        end_diagnostic(0i32);
    }
    return (*font).fontRef;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_getFullName(
    mut self_0: *const XeTeXFontMgr,
    mut font: PlatformFontRef,
) -> *const libc::c_char {
    // return the full name of the font, suitable for use in XeTeX source
    // without requiring style qualifiers
    let font_ptr = if let Some(font_ptr) = (*(*self_0).m_platformRefToFont).get(&font).cloned() {
        font_ptr
    } else {
        _tt_abort(
            b"internal error %d in XeTeXFontMgr\x00" as *const u8 as *const libc::c_char,
            2i32,
        );
    };
    let font_ptr = font_ptr.as_ptr();

    if !(*font_ptr).m_fullName.is_null() {
        return CppStdString_cstr((*font_ptr).m_fullName);
    }
    return CppStdString_cstr((*font_ptr).m_psName);
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_weightAndWidthDiff(
    mut _self_0: *const XeTeXFontMgr,
    mut a: *const XeTeXFontMgrFont,
    mut b: *const XeTeXFontMgrFont,
) -> libc::c_int {
    if (*a).weight as libc::c_int == 0i32 && (*a).width as libc::c_int == 0i32 {
        // assume there was no OS/2 info
        if (*a).isBold as libc::c_int == (*b).isBold as libc::c_int {
            return 0i32;
        } else {
            return 10000i32;
        }
    }
    let mut widDiff: libc::c_int =
        labs(((*a).width as libc::c_int - (*b).width as libc::c_int) as libc::c_long)
            as libc::c_int;
    if widDiff < 10i32 {
        widDiff *= 50i32
    }
    return (labs(((*a).weight as libc::c_int - (*b).weight as libc::c_int) as libc::c_long)
        + widDiff as libc::c_long) as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_styleDiff(
    mut _self_0: *const XeTeXFontMgr,
    mut a: *const XeTeXFontMgrFont,
    mut wt: libc::c_int,
    mut wd: libc::c_int,
    mut slant: libc::c_int,
) -> libc::c_int {
    let mut widDiff: libc::c_int =
        labs(((*a).width as libc::c_int - wd) as libc::c_long) as libc::c_int;
    if widDiff < 10i32 {
        widDiff *= 200i32
    }
    return (labs(labs((*a).slant as libc::c_long) - labs(slant as libc::c_long))
        * 2i32 as libc::c_long
        + labs(((*a).weight as libc::c_int - wt) as libc::c_long)
        + widDiff as libc::c_long) as libc::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_bestMatchFromFamily(
    mut self_0: *const XeTeXFontMgr,
    mut fam: *const XeTeXFontMgrFamily,
    mut wt: libc::c_int,
    mut wd: libc::c_int,
    mut slant: libc::c_int,
) -> *mut XeTeXFontMgrFont {
    let mut bestMatch: *mut XeTeXFontMgrFont = 0 as *mut XeTeXFontMgrFont;
    for (_, v) in (*(*fam).styles).iter() {
        if bestMatch.is_null()
            || XeTeXFontMgr_styleDiff(self_0, v.as_ptr(), wt, wd, slant)
                < XeTeXFontMgr_styleDiff(self_0, bestMatch, wt, wd, slant)
        {
            bestMatch = v.as_ptr()
        }
    }
    return bestMatch;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_getOpSize(
    mut _self_0: *mut XeTeXFontMgr,
    mut font: XeTeXFont,
) -> *mut XeTeXFontMgrOpSizeRec {
    let mut hbFont: *mut hb_font_t = XeTeXFontInst_getHbFont(font as *mut XeTeXFontInst);
    if hbFont.is_null() {
        return 0 as *mut XeTeXFontMgrOpSizeRec;
    }
    let mut face: *mut hb_face_t = hb_font_get_face(hbFont);
    let mut pSizeRec: *mut XeTeXFontMgrOpSizeRec =
        xmalloc(::std::mem::size_of::<XeTeXFontMgrOpSizeRec>() as _) as *mut XeTeXFontMgrOpSizeRec;
    let mut ok: bool = hb_ot_layout_get_size_params(
        face,
        &mut (*pSizeRec).designSize,
        &mut (*pSizeRec).subFamilyID,
        &mut (*pSizeRec).nameCode,
        &mut (*pSizeRec).minSize,
        &mut (*pSizeRec).maxSize,
    ) != 0;
    if ok {
        return pSizeRec;
    }
    free(pSizeRec as *mut libc::c_void);
    return 0 as *mut XeTeXFontMgrOpSizeRec;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_getDesignSize(
    mut self_0: *mut XeTeXFontMgr,
    mut font: XeTeXFont,
) -> libc::c_double {
    let mut pSizeRec: *mut XeTeXFontMgrOpSizeRec = XeTeXFontMgr_getOpSize(self_0, font);
    if pSizeRec.is_null() {
        return 10.0f64;
    }
    let mut result: libc::c_double = (*pSizeRec).designSize as libc::c_double / 10.0f64;
    free(pSizeRec as *mut libc::c_void);
    return result;
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_base_getOpSizeRecAndStyleFlags(
    mut self_0: *mut XeTeXFontMgr,
    mut theFont: *mut XeTeXFontMgrFont,
) {
    let mut font: XeTeXFont = createFont((*theFont).fontRef, 655360i32);
    let mut fontInst: *mut XeTeXFontInst = font as *mut XeTeXFontInst;
    if !font.is_null() {
        let mut pSizeRec: *mut XeTeXFontMgrOpSizeRec = XeTeXFontMgr_getOpSize(self_0, font);
        if !pSizeRec.is_null() {
            (*theFont).opSizeInfo.designSize = (*pSizeRec).designSize;
            if (*pSizeRec).subFamilyID == 0i32 as libc::c_uint
                && (*pSizeRec).nameCode == 0i32 as libc::c_uint
                && (*pSizeRec).minSize == 0i32 as libc::c_uint
                && (*pSizeRec).maxSize == 0i32 as libc::c_uint
            {
                free(pSizeRec as *mut libc::c_void);
            // feature is valid, but no 'size' range
            } else {
                (*theFont).opSizeInfo.subFamilyID = (*pSizeRec).subFamilyID;
                (*theFont).opSizeInfo.nameCode = (*pSizeRec).nameCode;
                (*theFont).opSizeInfo.minSize = (*pSizeRec).minSize;
                (*theFont).opSizeInfo.maxSize = (*pSizeRec).maxSize;
                free(pSizeRec as *mut libc::c_void);
            }
        }
        let mut os2Table: *const TT_OS2 =
            XeTeXFontInst_getFontTableFT(fontInst, FT_SFNT_OS2) as *mut TT_OS2;
        if !os2Table.is_null() {
            (*theFont).weight = (*os2Table).usWeightClass;
            (*theFont).width = (*os2Table).usWidthClass;
            let mut sel: uint16_t = (*os2Table).fsSelection;
            (*theFont).isReg = sel as libc::c_int & 1i32 << 6i32 != 0i32;
            (*theFont).isBold = sel as libc::c_int & 1i32 << 5i32 != 0i32;
            (*theFont).isItalic = sel as libc::c_int & 1i32 << 0i32 != 0i32
        }
        let mut headTable: *const TT_Header =
            XeTeXFontInst_getFontTableFT(fontInst, FT_SFNT_HEAD) as *mut TT_Header;
        if !headTable.is_null() {
            let mut ms: uint16_t = (*headTable).Mac_Style;
            if ms as libc::c_int & 1i32 << 0i32 != 0i32 {
                (*theFont).isBold = 1i32 != 0
            }
            if ms as libc::c_int & 1i32 << 1i32 != 0i32 {
                (*theFont).isItalic = 1i32 != 0
            }
        }
        let mut postTable: *const TT_Postscript =
            XeTeXFontInst_getFontTableFT(fontInst, FT_SFNT_POST) as *const TT_Postscript;
        if !postTable.is_null() {
            (*theFont).slant = (1000i32 as libc::c_double
                * tan(Fix2D(-(*postTable).italicAngle as Fixed) * std::f64::consts::PI / 180.0f64))
                as libc::c_int as int16_t
        }
        deleteFont(font);
    };
}
// append a name but only if it's not already in the list
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_appendToList(
    mut _self_0: *mut XeTeXFontMgr,
    mut list: *mut CppStdListOfString,
    mut str: *const libc::c_char,
) {
    use std::ffi::CStr;
    fn has_occur(list: &CppStdListOfString, val: &CStr) -> bool {
        for item in list.iter() {
            if &**item == val {
                return true;
            }
        }
        false
    }
    if has_occur(&*list, CStr::from_ptr(str)) {
        return;
    }
    (*list).push_back(CStr::from_ptr(str).to_owned());
}
// prepend a name, removing it from later in the list if present
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_prependToList(
    mut _self_0: *mut XeTeXFontMgr,
    mut list: *mut CppStdListOfString,
    mut str: *const libc::c_char,
) {
    use std::ffi::CStr;
    fn remove_first_occur(list: &mut CppStdListOfString, val: &CStr) -> bool {
        let mut found_idx = None;
        for (idx, item) in list.iter().enumerate() {
            if &**item == val {
                found_idx = Some(idx);
                break;
            }
        }
        if let Some(idx) = found_idx {
            list.remove(idx);
            true
        } else {
            false
        }
    }

    remove_first_occur(&mut *list, CStr::from_ptr(str));
    (*list).push_front(CStr::from_ptr(str).to_owned());
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_addToMaps(
    mut self_0: *mut XeTeXFontMgr,
    mut platformFont: PlatformFontRef,
    mut names: *const XeTeXFontMgrNameCollection,
) {
    if (*(*self_0).m_platformRefToFont).contains_key(&platformFont) {
        return;
    }
    if CppStdString_length((*names).m_psName) == 0 {
        return;
    }
    if (*(*self_0).m_psNameToFont).contains_key(&*(*names).m_psName) {
        return;
    }
    let mut thisFont_nonnull: NonNull<XeTeXFontMgrFont> =
        NonNull::new(XeTeXFontMgrFont_create(platformFont)).expect("should be non-null pointer");
    let thisFont = thisFont_nonnull.as_ptr();
    (*thisFont).m_psName = CppStdString_clone((*names).m_psName);
    XeTeXFontMgr_getOpSizeRecAndStyleFlags(self_0, thisFont);
    CppStdMap_put_with_string_key(
        (*self_0).m_psNameToFont,
        CppStdString_cstr((*names).m_psName),
        thisFont_nonnull,
    );
    CppStdMap_put(
        (*self_0).m_platformRefToFont,
        platformFont,
        thisFont_nonnull,
    );
    if !(*(*names).m_fullNames).is_empty() {
        (*thisFont).m_fullName = CppStdString_clone(&(*(*names).m_fullNames)[0]);
    }
    if !(*(*names).m_familyNames).is_empty() {
        (*thisFont).m_familyName = CppStdString_clone(&(*(*names).m_familyNames)[0]);
    } else {
        (*thisFont).m_familyName = CppStdString_clone((*names).m_psName);
    }
    if !(*(*names).m_styleNames).is_empty() {
        (*thisFont).m_styleName = CppStdString_clone(&(*(*names).m_styleNames)[0]);
    } else {
        (*thisFont).m_styleName = CppStdString_create()
    }
    for familyName in (*(*names).m_familyNames).iter() {
        let mut family: *mut XeTeXFontMgrFamily;
        if let Some(family_mut) = (*(*self_0).m_nameToFamily).get_mut(familyName) {
            family = family_mut.as_mut();
            if ((*thisFont).weight as libc::c_int) < (*family).minWeight as libc::c_int {
                (*family).minWeight = (*thisFont).weight
            }
            if (*thisFont).weight as libc::c_int > (*family).maxWeight as libc::c_int {
                (*family).maxWeight = (*thisFont).weight
            }
            if ((*thisFont).width as libc::c_int) < (*family).minWidth as libc::c_int {
                (*family).minWidth = (*thisFont).width
            }
            if (*thisFont).width as libc::c_int > (*family).maxWidth as libc::c_int {
                (*family).maxWidth = (*thisFont).width
            }
            if ((*thisFont).slant as libc::c_int) < (*family).minSlant as libc::c_int {
                (*family).minSlant = (*thisFont).slant
            }
            if (*thisFont).slant as libc::c_int > (*family).maxSlant as libc::c_int {
                (*family).maxSlant = (*thisFont).slant
            }
        } else {
            family = XeTeXFontMgrFamily_create();
            CppStdMap_put(
                (*self_0).m_nameToFamily,
                familyName.clone(),
                NonNull::new(family).expect("expect non-null pointer"),
            );
            (*family).minWeight = (*thisFont).weight;
            (*family).maxWeight = (*thisFont).weight;
            (*family).minWidth = (*thisFont).width;
            (*family).maxWidth = (*thisFont).width;
            (*family).minSlant = (*thisFont).slant;
            (*family).maxSlant = (*thisFont).slant;
        }
        if (*thisFont).parent.is_null() {
            (*thisFont).parent = family;
        }
        // ensure all style names in the family point to thisFont
        for styleName in (*(*names).m_styleNames).iter() {
            if !(*(*family).styles).contains_key(styleName) {
                CppStdMap_put((*family).styles, styleName.clone(), thisFont_nonnull);
            }
            /*
                else if (iFont->second != thisFont)
                    fprintf(stderr, "# Font name warning: ambiguous Style \"%s\" in Family \"%s\" (PSNames \"%s\" and \"%s\")\n",
                                j->c_str(), i->c_str(), iFont->second->m_psName->c_str(), thisFont->m_psName->c_str());
            */
        }
    }
    for fullName in (*(*names).m_fullNames).iter() {
        if !(*(*self_0).m_nameToFont).contains_key(fullName) {
            CppStdMap_put((*self_0).m_nameToFont, fullName.clone(), thisFont_nonnull);
        }
        /*
                else if (iFont->second != thisFont)
                    fprintf(stderr, "# Font name warning: ambiguous FullName \"%s\" (PSNames \"%s\" and \"%s\")\n",
                                i->c_str(), iFont->second->m_psName->c_str(), thisFont->m_psName->c_str());
        */
    }
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_base_terminate(mut _self_0: *mut XeTeXFontMgr) {}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontMgr_base_ctor(mut self_0: *mut XeTeXFontMgr) {
    (*self_0).m_subdtor = None; /*abstract*/
    (*self_0).m_memfnInitialize = None; /*abstract*/
    (*self_0).m_memfnTerminate =
        Some(XeTeXFontMgr_base_terminate as unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()); /*abstract*/
    (*self_0).m_memfnGetPlatformFontDesc = None;
    (*self_0).m_memfnGetOpSizeRecAndStyleFlags = Some(
        XeTeXFontMgr_base_getOpSizeRecAndStyleFlags
            as unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *mut XeTeXFontMgrFont) -> (),
    );
    (*self_0).m_memfnSearchForHostPlatformFonts = None;
    (*self_0).m_memfnReadNames = None;
    (*self_0).m_nameToFont = CppStdMap_create();
    (*self_0).m_nameToFamily = CppStdMap_create();
    (*self_0).m_platformRefToFont = CppStdMap_create();
    (*self_0).m_psNameToFont = CppStdMap_create();
}
