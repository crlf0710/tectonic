#![feature(extern_types)]
#![feature(ptr_wrapping_offset_from)]
#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_unsafe
)]
#[macro_use]
extern crate tectonic_bridge as bridge;
extern crate tectonic_dvipdfmx as dpx;

pub use bridge::*;

use std::ptr;

// For the msg_send macro
#[cfg(target_os = "macos")]
#[macro_use]
extern crate objc;

//use log::{info, warn};

pub type __off_t = i64;
pub type __off64_t = i64;
pub type __ssize_t = i64;
pub type size_t = u64;
pub type off_t = __off_t;
pub type ssize_t = __ssize_t;

use bibtex::bibtex_main;
use dpx::dvipdfmx_main;
use xetex_ini::tt_run_engine;

#[no_mangle]
pub unsafe extern "C" fn tex_simple_main(
    mut api: *const tt_bridge_api_t,
    mut dump_name: *const i8,
    mut input_file_name: *const i8,
) -> i32 {
    bridge::tt_with_bridge(api, || tt_run_engine(dump_name, input_file_name) as i32)
        .unwrap_or(TTHistory::FATAL_ERROR as i32)
}
#[no_mangle]
pub unsafe extern "C" fn dvipdfmx_simple_main(
    mut api: *const tt_bridge_api_t,
    mut dviname: *const i8,
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
#[no_mangle]
pub unsafe extern "C" fn bibtex_simple_main(
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
    pub unsafe extern "C" fn xcalloc(mut nelem: size_t, mut elsize: size_t) -> *mut libc::c_void {
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
    pub unsafe extern "C" fn xmalloc(mut size: size_t) -> *mut libc::c_void {
        let size = size as libc::size_t; //FIXME

        let mut new_mem: *mut libc::c_void = libc::malloc(if size != 0 { size } else { 1 });
        if new_mem.is_null() {
            panic!("xmalloc request for {} bytes failed", size,);
        }
        new_mem
    }
    #[no_mangle]
    pub unsafe extern "C" fn xrealloc(
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
    #[no_mangle]
    pub unsafe extern "C" fn xstrdup(mut s: *const i8) -> *mut i8 {
        let mut new_string: *mut i8 = xmalloc(libc::strlen(s).wrapping_add(1) as size_t) as *mut i8;
        libc::strcpy(new_string, s)
    }

    #[inline]
    pub(crate) unsafe extern "C" fn mfree(ptr: *mut libc::c_void) -> *mut libc::c_void {
        libc::free(ptr);
        std::ptr::null_mut()
    }
}

mod bibtex;
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

pub use xetex_engine_interface::tt_xetex_set_int_variable;

#[inline]
pub(crate) unsafe extern "C" fn strstartswith(s: *const i8, prefix: *const i8) -> *const i8 {
    let length = libc::strlen(prefix);
    if libc::strncmp(s, prefix, length) == 0i32 {
        return s.offset(length as isize);
    }
    ptr::null()
}

#[inline]
pub(crate) unsafe extern "C" fn streq_ptr(s1: *const i8, s2: *const i8) -> bool {
    if !s1.is_null() && !s2.is_null() {
        return libc::strcmp(s1, s2) == 0i32;
    }
    false
}

mod xetex_font_info;
mod xetex_font_manager;
mod xetex_layout_engine;
mod xetex_layout_interface;
mod xetex_opentype_math;

pub(crate) mod freetype_sys_patch {
    use freetype::freetype_sys::{FT_Face, FT_Byte, FT_Short, FT_UShort, FT_Long, FT_ULong, FT_UInt, FT_Int32, FT_Fixed, FT_Error, FT_Sfnt_Tag};

    extern "C" {
        #[no_mangle]
        pub fn FT_Face_GetCharVariantIndex(
            face: FT_Face,
            charcode: FT_ULong,
            variantSelector: FT_ULong,
        ) -> FT_UInt;
    
        #[no_mangle]
        pub fn FT_Get_Advance(
            face: FT_Face,
            gindex: FT_UInt,
            load_flags: FT_Int32,
            padvance: *mut FT_Fixed,
        ) -> FT_Error;

        #[no_mangle]
        pub fn FT_Load_Sfnt_Table(
            face: FT_Face,
            tag: FT_ULong,
            offset: FT_Long,
            buffer: *mut FT_Byte,
            length: *mut FT_ULong,
        ) -> FT_Error;

        #[no_mangle]
        pub fn FT_Get_Sfnt_Name_Count(face: FT_Face) -> FT_UInt;

        #[no_mangle]
        pub fn FT_Get_Sfnt_Name(face: FT_Face, idx: FT_UInt, aname: *mut FT_SfntName) -> FT_Error;            
    }

    pub const FT_SFNT_MAX: FT_Sfnt_Tag = 7;
    pub const FT_SFNT_PCLT: FT_Sfnt_Tag = 6;
    pub const FT_SFNT_POST: FT_Sfnt_Tag = 5;
    pub const FT_SFNT_VHEA: FT_Sfnt_Tag = 4;
    pub const FT_SFNT_HHEA: FT_Sfnt_Tag = 3;
    pub const FT_SFNT_OS2: FT_Sfnt_Tag = 2;
    pub const FT_SFNT_MAXP: FT_Sfnt_Tag = 1;
    pub const FT_SFNT_HEAD: FT_Sfnt_Tag = 0;

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
    pub struct FT_SfntName_ {
        pub platform_id: FT_UShort,
        pub encoding_id: FT_UShort,
        pub language_id: FT_UShort,
        pub name_id: FT_UShort,
        pub string: *mut FT_Byte,
        pub string_len: FT_UInt,
    }    
    pub type FT_SfntName = FT_SfntName_;
}
