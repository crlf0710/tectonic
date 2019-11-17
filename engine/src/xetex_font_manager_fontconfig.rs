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
use std::ffi::CString;
use std::ptr::NonNull;

use freetype::freetype_sys::{FT_Byte, FT_UInt, FT_Long, FT_ULong, FT_Int32, FT_Pointer, FT_Error, FT_Fixed, 
    FT_Library, FT_Face, FT_Glyph, FT_String, FT_Parameter};
use freetype::freetype_sys::{FT_New_Face, FT_Done_Face, FT_Get_Postscript_Name, FT_Init_FreeType};
use crate::freetype_sys_patch::{FT_Get_Sfnt_Name, FT_Get_Sfnt_Name_Count};

extern "C" {
    pub type _FcPattern;
    pub type _FcConfig;
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
    XeTeXFontMgr, XeTeXFontMgrFamily, XeTeXFontMgrFont, XeTeXFontMgrNameCollection,
    XeTeXFontMgrOpSizeRec,
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
    use crate::freetype_sys_patch::FT_SfntName;
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
    let mut face: FT_Face = 0 as FT_Face;
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
    mut self_0: *const XeTeXFontMgr,
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
