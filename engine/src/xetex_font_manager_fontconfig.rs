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

#![cfg(not(target_os = "macos"))]
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use std::ffi::CStr;
use std::ptr;

use std::collections::VecDeque;

use crate::xetex_font_manager::FontMgrExt;

use super::{AddToList, AddToMaps};
use crate::stub_icu as icu;
use crate::xetex_font_info::gFreeTypeLibrary;

use crate::freetype_sys_patch::{FT_Get_Sfnt_Name, FT_Get_Sfnt_Name_Count};
use freetype::freetype_sys::FT_Long;
use freetype::freetype_sys::{FT_Done_Face, FT_Get_Postscript_Name, FT_Init_FreeType, FT_New_Face};

pub(crate) use fontconfig_sys::fontconfig::{
    enum__FcResult as FcResult, struct__FcPattern as FcPattern,
};
use fontconfig_sys::fontconfig::{
    struct__FcFontSet as FcFontSet, FcConfigGetCurrent, FcFontList, FcFontSetDestroy, FcInit,
    FcNameParse, FcObjectSetBuild, FcObjectSetDestroy, FcPatternDestroy, FcPatternGetInteger,
    FcPatternGetString, FcResultMatch,
};
pub(crate) type PlatformFontRef = *mut FcPattern;

use super::{XeTeXFontMgr, XeTeXFontMgrFont, XeTeXFontMgrNameCollection};

#[derive(Clone)]
pub(crate) struct XeTeXFontMgr_FC {
    pub(crate) super_: XeTeXFontMgr,
    pub(crate) allFonts: *mut FcFontSet,
    pub(crate) cachedAll: bool,
}

impl core::ops::Deref for XeTeXFontMgr_FC {
    type Target = XeTeXFontMgr;
    fn deref(&self) -> &Self::Target {
        &self.super_
    }
}
impl core::ops::DerefMut for XeTeXFontMgr_FC {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.super_
    }
}
static mut macRomanConv: *mut icu::UConverter = 0 as *mut icu::UConverter;
static mut utf16beConv: *mut icu::UConverter = 0 as *mut icu::UConverter;
static mut utf8Conv: *mut icu::UConverter = 0 as *mut icu::UConverter;
unsafe fn convertToUtf8(
    conv: *mut icu::UConverter,
    name: *const libc::c_uchar,
    mut len: libc::c_int,
) -> String {
    let mut buffer1 = vec![0_u16; len as usize + 50];
    let bufSize = 2 * len + 100;
    let mut status: icu::UErrorCode = icu::U_ZERO_ERROR;
    len = icu::ucnv_toUChars(
        conv,
        buffer1.as_mut_ptr(),
        bufSize,
        name as *const libc::c_char,
        len,
        &mut status,
    );
    let mut buffer2 = vec![0_u8; bufSize as usize];
    len = icu::ucnv_fromUChars(
        utf8Conv,
        buffer2.as_mut_ptr() as *mut i8,
        bufSize,
        buffer1.as_ptr(),
        len,
        &mut status,
    );
    buffer2[len as usize] = 0;
    buffer2.truncate(len as usize + 1);
    String::from_utf8(buffer2).unwrap()
}

impl XeTeXFontMgr_FC {
    pub(crate) fn ctor() -> Self {
        Self {
            super_: XeTeXFontMgr::base_ctor(),
            allFonts: ptr::null_mut(),
            cachedAll: false,
        }
    }

    pub(crate) fn create() -> Box<Self> {
        Box::new(Self::ctor())
    }

    pub(crate) unsafe fn cache_family_members(&mut self, familyNames: &VecDeque<String>) {
        if familyNames.is_empty() {
            return;
        }
        for f in 0..(*self.allFonts).nfont {
            let pat = *(*self.allFonts).fonts.offset(f as isize);
            if self.m_platformRefToFont.contains_key(&pat) {
                continue;
            }

            let mut s = ptr::null_mut();
            for i in 0.. {
                if FcPatternGetString(
                    pat,
                    b"family\x00" as *const u8 as *const libc::c_char,
                    i,
                    &mut s as *mut *mut libc::c_char as *mut *mut u8,
                ) as u32
                    != FcResultMatch as _
                {
                    break;
                }
                let s = CStr::from_ptr(s).to_bytes();
                if !familyNames
                    .iter()
                    .any(|family_name| family_name.as_bytes() == s)
                {
                    continue;
                }
                let names = self.read_names(pat);
                self.add_to_maps(pat, &names);
                break;
            }
        }
    }

    pub(crate) unsafe fn initialize(&mut self) {
        if FcInit() == 0 {
            abort!("fontconfig initialization failed");
        }
        if gFreeTypeLibrary.is_null() && FT_Init_FreeType(&mut gFreeTypeLibrary) != 0 {
            abort!("FreeType initialization failed");
        }
        let mut err: icu::UErrorCode = icu::U_ZERO_ERROR;
        macRomanConv = icu::ucnv_open(
            b"macintosh\x00" as *const u8 as *const libc::c_char,
            &mut err,
        );
        utf16beConv = icu::ucnv_open(b"UTF16BE\x00" as *const u8 as *const libc::c_char, &mut err);
        utf8Conv = icu::ucnv_open(b"UTF8\x00" as *const u8 as *const libc::c_char, &mut err);
        if err as u64 != 0 {
            abort!("cannot read font names");
        }
        let pat =
            FcNameParse(b":outline=true\x00" as *const u8 as *const libc::c_char as *const u8);
        let os = FcObjectSetBuild(
            b"family\x00" as *const u8 as *mut u8 as *mut i8,
            b"style\x00" as *const u8 as *mut u8 as *mut i8,
            b"file\x00" as *const u8 as *mut u8 as *mut i8,
            b"index\x00" as *const u8 as *mut u8 as *mut i8,
            b"fullname\x00" as *const u8 as *mut u8 as *mut i8,
            b"weight\x00" as *const u8 as *mut u8 as *mut i8,
            b"width\x00" as *const u8 as *mut u8 as *mut i8,
            b"slant\x00" as *const u8 as *mut u8 as *mut i8,
            b"fontformat\x00" as *const u8 as *mut u8 as *mut i8,
            ptr::null_mut::<i8>(),
        );
        self.allFonts = FcFontList(FcConfigGetCurrent(), pat, os);
        FcObjectSetDestroy(os);
        FcPatternDestroy(pat);
        self.cachedAll = false;
    }
}

impl FontMgrExt for XeTeXFontMgr_FC {
    type FontRef = PlatformFontRef;
    unsafe fn terminate(&mut self) {
        if !self.allFonts.is_null() {
            FcFontSetDestroy(self.allFonts);
            self.allFonts = ptr::null_mut();
        }
        if !macRomanConv.is_null() {
            icu::ucnv_close(macRomanConv);
            macRomanConv = ptr::null_mut();
        }
        if !utf16beConv.is_null() {
            icu::ucnv_close(utf16beConv);
            utf16beConv = ptr::null_mut();
        }
        if !utf8Conv.is_null() {
            icu::ucnv_close(utf8Conv);
            utf8Conv = ptr::null_mut();
        };
    }
    unsafe fn get_platform_font_desc(&self, font: Self::FontRef) -> String {
        let mut s: *mut u8 = ptr::null_mut();
        if FcPatternGetString(
            font,
            b"file\x00" as *const u8 as *const libc::c_char,
            0,
            &mut s as *mut *mut u8,
        ) as u32
            == FcResultMatch as u32
        {
            crate::c_pointer_to_str(s as *const libc::c_char).to_string()
        } else {
            "[unknown]".to_string()
        }
    }

    unsafe fn search_for_host_platform_fonts(&mut self, name: &str) {
        if self.cachedAll {
            // we've already loaded everything on an earlier search
            return;
        }
        let (hyph, famName) = match name.find('-') {
            Some(pos) => (pos, name[..pos].to_string()),
            _ => (0, String::new()),
        };
        let mut found = false;
        loop {
            'traverse_fonts: for f in 0..(*self.allFonts).nfont {
                let pat = *(*self.allFonts).fonts.offset(f as isize);
                if !self.m_platformRefToFont.contains_key(&pat) {
                    continue;
                }

                if self.cachedAll {
                    // failed to find it via FC; add everything to our maps (potentially slow) as a last resort
                    let names = self.read_names(pat);
                    self.add_to_maps(pat, &names);
                    continue;
                }

                let mut s: *mut libc::c_char = ptr::null_mut();
                let mut i = 0;
                while FcPatternGetString(
                    pat,
                    b"fullname\x00" as *const u8 as *const libc::c_char,
                    i,
                    &mut s as *mut *mut libc::c_char as *mut *mut u8,
                ) == FcResultMatch
                {
                    if name.as_bytes() == CStr::from_ptr(s).to_bytes() {
                        let names_0 = self.read_names(pat);
                        self.add_to_maps(pat, &names_0);
                        self.cache_family_members(&names_0.m_familyNames);
                        found = true;
                        continue 'traverse_fonts;
                    }
                    i += 1;
                }

                let mut i = 0;
                while FcPatternGetString(
                    pat,
                    b"family\x00" as *const u8 as *const libc::c_char,
                    i,
                    &mut s as *mut *mut libc::c_char as *mut *mut u8,
                ) == FcResultMatch
                {
                    if name.as_bytes() == CStr::from_ptr(s).to_bytes()
                        || hyph != 0 && (famName.as_bytes() == CStr::from_ptr(s).to_bytes())
                    {
                        let names_1 = self.read_names(pat);
                        self.add_to_maps(pat, &names_1);
                        self.cache_family_members(&names_1.m_familyNames);
                        found = true;
                        continue 'traverse_fonts;
                    }

                    let mut t: *mut libc::c_char = 0 as _;
                    let mut j = 0;
                    while FcPatternGetString(
                        pat,
                        b"style\x00" as *const u8 as *const libc::c_char,
                        j,
                        &mut t as *mut *mut libc::c_char as *mut *mut u8,
                    ) == FcResultMatch
                    {
                        let mut full = Vec::new();
                        full.extend(CStr::from_ptr(s).to_bytes());
                        full.push(b' ');
                        full.extend(CStr::from_ptr(t).to_bytes());
                        let matched = full == name.as_bytes();
                        if matched {
                            let names_2 = self.read_names(pat);
                            self.add_to_maps(pat, &names_2);
                            self.cache_family_members(&names_2.m_familyNames);
                            found = true;
                            continue 'traverse_fonts;
                        }
                        j += 1;
                    }
                    i += 1;
                }
            }

            if found || self.cachedAll {
                break;
            }
            self.cachedAll = true;
        }
    }

    unsafe fn read_names(&self, pat: Self::FontRef) -> XeTeXFontMgrNameCollection {
        use crate::freetype_sys_patch::FT_SfntName;
        let mut names = XeTeXFontMgrNameCollection::new();
        let mut pathname = ptr::null_mut();
        if FcPatternGetString(
            pat,
            b"file\x00" as *const u8 as *const libc::c_char,
            0,
            &mut pathname as *mut *mut libc::c_char as *mut *mut u8,
        ) as u32
            != FcResultMatch as u32
        {
            return names;
        }
        let mut index = 0;
        if FcPatternGetInteger(
            pat,
            b"index\x00" as *const u8 as *const libc::c_char,
            0,
            &mut index,
        ) as u32
            != FcResultMatch as u32
        {
            return names;
        }
        let mut face = ptr::null_mut();
        if FT_New_Face(gFreeTypeLibrary, pathname, index as FT_Long, &mut face) != 0i32 {
            return names;
        }
        let mut name: *const libc::c_char = FT_Get_Postscript_Name(face);
        if name.is_null() {
            return names;
        }
        names.m_psName = CStr::from_ptr(name).to_str().unwrap().to_string();
        /* this string is *not* null-terminated! */
        /* in bytes                              */
        // for sfnt containers, we'll read the name table ourselves, not rely on Fontconfig
        if (*face).face_flags & 1 << 3i32 != 0 {
            let mut familyNames = VecDeque::default();
            let mut subFamilyNames = VecDeque::default();
            let mut nameRec: FT_SfntName = FT_SfntName {
                platform_id: 0,
                encoding_id: 0,
                language_id: 0,
                name_id: 0,
                string: ptr::null_mut(),
                string_len: 0,
            };
            for i in 0..FT_Get_Sfnt_Name_Count(face) {
                if FT_Get_Sfnt_Name(face, i, &mut nameRec) == 0 {
                    match nameRec.name_id as libc::c_int {
                        4 | 1 | 2 | 16 | 17 => {
                            let mut preferredName = false;
                            let utf8name = if nameRec.platform_id == 1
                                && nameRec.encoding_id == 0
                                && nameRec.language_id == 0
                            {
                                let utf8name = Some(convertToUtf8(
                                    macRomanConv,
                                    nameRec.string,
                                    nameRec.string_len as libc::c_int,
                                ));
                                preferredName = true;
                                utf8name
                            } else if nameRec.platform_id == 0 || nameRec.platform_id == 3 {
                                Some(convertToUtf8(
                                    utf16beConv,
                                    nameRec.string,
                                    nameRec.string_len as libc::c_int,
                                ))
                            } else {
                                None
                            };
                            if let Some(utf8name) = utf8name {
                                let nameList = match nameRec.name_id as libc::c_int {
                                    4 => &mut names.m_fullNames,
                                    1 => &mut names.m_familyNames,
                                    2 => &mut names.m_styleNames,
                                    16 => &mut familyNames,
                                    17 => &mut subFamilyNames,
                                    _ => unreachable!(),
                                };
                                if preferredName {
                                    nameList.prepend_to_list(&utf8name);
                                } else if !nameList.contains(&utf8name) {
                                    nameList.push_back(utf8name);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            if !familyNames.is_empty() {
                names.m_familyNames = familyNames;
            }
            if !subFamilyNames.is_empty() {
                names.m_styleNames = subFamilyNames;
            }
        } else {
            let mut index = 0;
            while FcPatternGetString(
                pat,
                b"fullname\x00" as *const u8 as *const libc::c_char,
                index,
                &mut name as *mut *const libc::c_char as *mut *mut u8,
            ) == FcResultMatch
            {
                index += 1;
                names
                    .m_fullNames
                    .append_to_list(CStr::from_ptr(name).to_str().unwrap());
            }
            let mut index = 0;
            while FcPatternGetString(
                pat,
                b"family\x00" as *const u8 as *const libc::c_char,
                index,
                &mut name as *mut *const libc::c_char as *mut *mut u8,
            ) == FcResultMatch
            {
                index += 1;
                names
                    .m_familyNames
                    .append_to_list(CStr::from_ptr(name).to_str().unwrap());
            }
            let mut index = 0;
            while FcPatternGetString(
                pat,
                b"style\x00" as *const u8 as *const libc::c_char,
                index,
                &mut name as *mut *const libc::c_char as *mut *mut u8,
            ) == FcResultMatch
            {
                index += 1;
                names
                    .m_styleNames
                    .append_to_list(CStr::from_ptr(name).to_str().unwrap());
            }
            if names.m_fullNames.is_empty() {
                let mut fullName = names.m_familyNames[0].clone();
                if !names.m_styleNames.is_empty() {
                    fullName.push(' ');
                    fullName.push_str(&names.m_styleNames[0]);
                }
                names.m_fullNames.push_back(fullName);
            }
        }
        FT_Done_Face(face);
        names
    }
    unsafe fn get_op_size_rec_and_style_flags(&self, theFont: &mut XeTeXFontMgrFont) {
        theFont.base_get_op_size_rec_and_style_flags();
        if theFont.weight == 0 && theFont.width == 0 {
            // try to get values from FontConfig, as it apparently wasn't an sfnt
            let pat = theFont.fontRef;
            let mut value = 0;
            if FcPatternGetInteger(
                pat,
                b"weight\x00" as *const u8 as *const libc::c_char,
                0,
                &mut value,
            ) == FcResultMatch
            {
                theFont.weight = value as u16
            }
            if FcPatternGetInteger(
                pat,
                b"width\x00" as *const u8 as *const libc::c_char,
                0,
                &mut value,
            ) == FcResultMatch
            {
                theFont.width = value as u16
            }
            if FcPatternGetInteger(
                pat,
                b"slant\x00" as *const u8 as *const libc::c_char,
                0,
                &mut value,
            ) == FcResultMatch
            {
                theFont.slant = value as i16
            }
        };
    }
    fn font_ref(font: &XeTeXFontMgrFont) -> Self::FontRef {
        font.fontRef
    }
}
