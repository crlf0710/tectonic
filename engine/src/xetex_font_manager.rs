#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

#[cfg(not(target_os = "macos"))]
#[path = "xetex_font_manager_fontconfig.rs"]
pub(crate) mod imp;

#[cfg(target_os = "macos")]
#[path = "xetex_font_manager_coretext.rs"]
pub(crate) mod imp;

use std::ffi::CString;
use std::ptr;
use std::ptr::NonNull;

use crate::core_memory::xmalloc;

use crate::xetex_ext::Fix2D;
use crate::xetex_ini::loaded_font_design_size;
use crate::xetex_layout_interface::collection_types::*;
use crate::xetex_layout_interface::createFont;
use crate::xetex_output::{print_char, print_nl};
use crate::xetex_xetex0::{diagnostic, get_tracing_fonts_state};

#[cfg(not(target_os = "macos"))]
use self::imp::XeTeXFontMgr_FC_create;
#[cfg(target_os = "macos")]
use self::imp::XeTeXFontMgr_Mac_create;

use harfbuzz_sys::{hb_face_t, hb_font_get_face, hb_font_t, hb_ot_layout_get_size_params};

use libc::{free, malloc, strchr, strlen};

#[cfg(not(target_os = "macos"))]
use imp::FcPattern;

use crate::xetex_scaledmath::Scaled;
#[cfg(not(target_os = "macos"))]
pub(crate) type PlatformFontRef = *mut FcPattern;

#[cfg(target_os = "macos")]
pub(crate) type PlatformFontRef = crate::cf_prelude::CTFontDescriptorRef;

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
pub(crate) struct XeTeXFontMgrOpSizeRec {
    pub(crate) designSize: libc::c_uint,
    pub(crate) subFamilyID: libc::c_uint,
    pub(crate) nameCode: libc::c_uint,
    pub(crate) minSize: libc::c_uint,
    pub(crate) maxSize: libc::c_uint,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrFamily {
    pub(crate) styles: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    pub(crate) minWeight: u16,
    pub(crate) maxWeight: u16,
    pub(crate) minWidth: u16,
    pub(crate) maxWidth: u16,
    pub(crate) minSlant: i16,
    pub(crate) maxSlant: i16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrFont {
    pub(crate) m_fullName: *mut CppStdString,
    pub(crate) m_psName: *mut CppStdString,
    pub(crate) m_familyName: *mut CppStdString,
    pub(crate) m_styleName: *mut CppStdString,
    pub(crate) parent: *mut XeTeXFontMgrFamily,
    pub(crate) fontRef: PlatformFontRef,
    pub(crate) opSizeInfo: XeTeXFontMgrOpSizeRec,
    pub(crate) weight: u16,
    pub(crate) width: u16,
    pub(crate) slant: i16,
    pub(crate) isReg: bool,
    pub(crate) isBold: bool,
    pub(crate) isItalic: bool,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrNameCollection {
    pub(crate) m_familyNames: *mut CppStdListOfString,
    pub(crate) m_styleNames: *mut CppStdListOfString,
    pub(crate) m_fullNames: *mut CppStdListOfString,
    pub(crate) m_psName: *mut CppStdString,
    pub(crate) m_subFamily: *mut CppStdString,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgr {
    pub(crate) m_subdtor: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub(crate) m_memfnInitialize: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub(crate) m_memfnTerminate: Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ()>,
    pub(crate) m_memfnGetPlatformFontDesc: Option<
        unsafe extern "C" fn(_: *const XeTeXFontMgr, _: PlatformFontRef) -> *mut libc::c_char,
    >,
    pub(crate) m_memfnGetOpSizeRecAndStyleFlags:
        Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *mut XeTeXFontMgrFont) -> ()>,
    pub(crate) m_memfnSearchForHostPlatformFonts:
        Option<unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *const libc::c_char) -> ()>,
    pub(crate) m_memfnReadNames: Option<
        unsafe extern "C" fn(
            _: *mut XeTeXFontMgr,
            _: PlatformFontRef,
        ) -> *mut XeTeXFontMgrNameCollection,
    >,
    pub(crate) m_nameToFont: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    pub(crate) m_nameToFamily: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFamily>>,
    pub(crate) m_platformRefToFont: *mut CppStdMap<PlatformFontRef, NonNull<XeTeXFontMgrFont>>,
    pub(crate) m_psNameToFont: *mut CppStdMap<CString, NonNull<XeTeXFontMgrFont>>,
    // maps PS name (as used in .xdv) to font record
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
use crate::xetex_font_info::XeTeXFontInst;

#[inline]
unsafe extern "C" fn XeTeXFontMgrFamily_create() -> *mut XeTeXFontMgrFamily {
    let mut self_0: *mut XeTeXFontMgrFamily =
        malloc(::std::mem::size_of::<XeTeXFontMgrFamily>()) as *mut XeTeXFontMgrFamily; /* default to 10bp */
    (*self_0).minWeight = 0i32 as u16;
    (*self_0).maxWeight = 0i32 as u16;
    (*self_0).minWidth = 0i32 as u16;
    (*self_0).maxWidth = 0i32 as u16;
    (*self_0).minSlant = 0i32 as i16;
    (*self_0).maxSlant = 0i32 as i16;
    (*self_0).styles = CppStdMap_create();
    return self_0;
}
#[inline]
unsafe extern "C" fn XeTeXFontMgrFont_create(mut ref_0: PlatformFontRef) -> *mut XeTeXFontMgrFont {
    let mut self_0: *mut XeTeXFontMgrFont =
        malloc(::std::mem::size_of::<XeTeXFontMgrFont>()) as *mut XeTeXFontMgrFont;
    (*self_0).m_fullName = 0 as *mut CppStdString;
    (*self_0).m_psName = 0 as *mut CppStdString;
    (*self_0).m_familyName = 0 as *mut CppStdString;
    (*self_0).m_styleName = 0 as *mut CppStdString;
    (*self_0).parent = 0 as *mut XeTeXFontMgrFamily;
    (*self_0).fontRef = ref_0;
    (*self_0).weight = 0i32 as u16;
    (*self_0).width = 0i32 as u16;
    (*self_0).slant = 0i32 as i16;
    (*self_0).isReg = false;
    (*self_0).isBold = false;
    (*self_0).isItalic = false;
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
pub(crate) static mut XeTeXFontMgr_sFontManager: *mut XeTeXFontMgr = ptr::null_mut();
#[no_mangle]
pub(crate) static mut XeTeXFontMgr_sReqEngine: libc::c_char = 0i32 as libc::c_char;
/* use our own fmax function because it seems to be missing on certain platforms
(solaris2.9, at least) */
#[inline]
unsafe extern "C" fn my_fmax(mut x: f64, mut y: f64) -> f64 {
    return if x > y { x } else { y };
}
pub(crate) unsafe fn XeTeXFontMgr_GetFontManager() -> *mut XeTeXFontMgr {
    #[cfg(not(target_os = "macos"))]
    {
        if XeTeXFontMgr_sFontManager.is_null() {
            XeTeXFontMgr_sFontManager = &mut (*XeTeXFontMgr_FC_create()).super_;
            XeTeXFontMgr_initialize(XeTeXFontMgr_sFontManager);
        }
    }
    #[cfg(target_os = "macos")]
    {
        if XeTeXFontMgr_sFontManager.is_null() {
            XeTeXFontMgr_sFontManager = &mut (*XeTeXFontMgr_Mac_create()).super_;
            XeTeXFontMgr_initialize(XeTeXFontMgr_sFontManager);
        }
    }
    return XeTeXFontMgr_sFontManager;
}
pub(crate) unsafe fn XeTeXFontMgr_Terminate() {
    if !XeTeXFontMgr_sFontManager.is_null() {
        XeTeXFontMgr_terminate(XeTeXFontMgr_sFontManager);
        // we don't actually deallocate the manager, just ask it to clean up
        // any auxiliary data such as the cocoa pool or freetype/fontconfig stuff
        // as we still need to access font names after this is called
    };
}
pub(crate) unsafe fn XeTeXFontMgr_Destroy() {
    // Here we actually fully destroy the font manager.
    if !XeTeXFontMgr_sFontManager.is_null() {
        XeTeXFontMgr_delete(XeTeXFontMgr_sFontManager);
        XeTeXFontMgr_sFontManager = 0 as *mut XeTeXFontMgr
    };
}
pub(crate) unsafe fn XeTeXFontMgr_getReqEngine(mut _self_0: *const XeTeXFontMgr) -> libc::c_char {
    // return the requested rendering technology for the most recent findFont
    // or 0 if no specific technology was requested
    return XeTeXFontMgr_sReqEngine;
}
pub(crate) unsafe fn XeTeXFontMgr_setReqEngine(
    mut _self_0: *const XeTeXFontMgr,
    mut reqEngine: libc::c_char,
) {
    XeTeXFontMgr_sReqEngine = reqEngine;
}
// above are singleton operation.
// /////////////
pub(crate) unsafe fn XeTeXFontMgr_delete(mut self_0: *mut XeTeXFontMgr) {
    if self_0.is_null() {
        return;
    }
    if let Some(f) = (*self_0).m_subdtor {
        f(self_0);
    }
    CppStdMap_delete((*self_0).m_nameToFont);
    CppStdMap_delete((*self_0).m_nameToFamily);
    CppStdMap_delete((*self_0).m_platformRefToFont);
    CppStdMap_delete((*self_0).m_psNameToFont);
    free(self_0 as *mut libc::c_void);
}
pub(crate) unsafe fn XeTeXFontMgr_findFont(
    mut self_0: *mut XeTeXFontMgr,
    name: &str,
    variant: &mut String,
    mut ptSize: f64,
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
    let mut nameStr = CString::new(name).unwrap();
    let mut font: *mut XeTeXFontMgrFont = 0 as *mut XeTeXFontMgrFont;
    let mut dsize: libc::c_int = 100i32;
    loaded_font_design_size = Scaled(655360);
    for pass in 0..2i32 {
        // try full name as given
        if let Some(name_FONT_PTR) = (*(*self_0).m_nameToFont).get(&nameStr).cloned() {
            font = name_FONT_PTR.as_ptr();
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
            hyph_pos.offset_from(nameStr_cstr) as libc::c_long
        } else {
            -1i32 as libc::c_long
        }) as libc::c_int;
        if hyph > 0i32 && hyph < nameStr_len - 1i32 {
            let mut family = CString::default();
            CppStdString_assign_n_chars(&mut family, nameStr_cstr, hyph as usize);
            if let Some(family_ptr) = (*(*self_0).m_nameToFamily).get(&family).cloned() {
                let mut style = CString::default();
                CppStdString_assign_n_chars(
                    &mut style,
                    nameStr_cstr.offset(hyph as isize).offset(1),
                    (nameStr_len - hyph - 1i32) as usize,
                );
                if let Some(style_FONT_PTR) = (*(*family_ptr.as_ptr()).styles).get(&style).cloned()
                {
                    font = style_FONT_PTR.as_ptr();
                    if (*font).opSizeInfo.designSize != 0i32 as libc::c_uint {
                        dsize = (*font).opSizeInfo.designSize as libc::c_int
                    }
                    break;
                }
            }
        }
        // try as PostScript name
        if let Some(ps_FONT_PTR) = (*(*self_0).m_psNameToFont).get(&nameStr).cloned() {
            font = ps_FONT_PTR.as_ptr();
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
                    if let Some(style_FONT_PTR) = (*(*family_ptr).styles).get(style) {
                        font = style_FONT_PTR.as_ptr();
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
    let mut reqBold = false;
    let mut reqItal = false;
    if !variant.is_empty() {
        let mut varString = String::new();
        let mut cp = variant.as_bytes();
        while !cp.is_empty() {
            if cp.starts_with(b"AAT") {
                XeTeXFontMgr_sReqEngine = 'A' as i32 as libc::c_char;
                cp = &cp[3..];
                match varString.chars().last() {
                    None | Some('/') => {}
                    _ => varString.push('/'),
                }
                varString += "AAT";
            } else if cp.starts_with(b"ICU") {
                // for backword compatability
                XeTeXFontMgr_sReqEngine = 'O' as i32 as libc::c_char;
                cp = &cp[3..];
                match varString.chars().last() {
                    None | Some('/') => {}
                    _ => varString.push('/'),
                }
                varString += "OT";
            } else if cp.starts_with(b"OT") {
                XeTeXFontMgr_sReqEngine = 'O' as i32 as libc::c_char;
                cp = &cp[2..];
                match varString.chars().last() {
                    None | Some('/') => {}
                    _ => varString.push('/'),
                }
                varString += "OT";
            } else if cp.starts_with(b"GR") {
                XeTeXFontMgr_sReqEngine = 'G' as i32 as libc::c_char;
                cp = &cp[2..];
                match varString.chars().last() {
                    None | Some('/') => {}
                    _ => varString.push('/'),
                }
                varString += "GR";
            } else if cp[0] == b'S' {
                cp = &cp[1..];
                if cp[0] == b'=' {
                    cp = &cp[1..];
                }
                ptSize = 0.0f64;
                while (b'0'..=b'9').contains(&cp[0]) {
                    ptSize = ptSize * 10. + cp[0] as f64 - '0' as u32 as f64;
                    cp = &cp[1..];
                }
                if cp[0] == b'.' {
                    let mut dec = 1_f64;
                    cp = &cp[1..];
                    while (b'0'..=b'9').contains(&cp[0]) {
                        dec *= 10.;
                        ptSize = ptSize + (cp[0] as i32 - '0' as i32) as f64 / dec;
                        cp = &cp[1..];
                    }
                }
            } else {
                loop
                /* if the code is "B" or "I", we skip putting it in varString */
                {
                    match cp[0] {
                        b'B' => {
                            reqBold = true;
                            cp = &cp[1..];
                        }
                        b'I' => {
                            reqItal = true;
                            cp = &cp[1..];
                        }
                        _ => break,
                    }
                }
            }
            while !cp.is_empty() && cp[0] != b'/' {
                cp = &cp[1..];
            }
            if cp[0] == b'/' {
                cp = &cp[1..];
            }
        }
        *variant = varString;
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
                    let style_FONT_PTR = v.as_ptr();
                    if (*style_FONT_PTR).isItalic == !(*font).isItalic {
                        if (*parent).minWeight != (*parent).maxWeight {
                            // weight info was available, so try to match that
                            if bestMatch.is_null()
                                || XeTeXFontMgr_weightAndWidthDiff(self_0, style_FONT_PTR, font)
                                    < XeTeXFontMgr_weightAndWidthDiff(self_0, bestMatch, font)
                            {
                                bestMatch = style_FONT_PTR;
                            }
                        } else if bestMatch.is_null() && (*style_FONT_PTR).isBold == (*font).isBold
                        {
                            bestMatch = style_FONT_PTR;
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
                        let style_FONT_PTR = v.as_ptr();
                        if (*style_FONT_PTR).isItalic == (*font).isItalic {
                            if newBest_0.is_null()
                                || XeTeXFontMgr_weightAndWidthDiff(
                                    self_0,
                                    style_FONT_PTR,
                                    bestMatch_0,
                                ) < XeTeXFontMgr_weightAndWidthDiff(
                                    self_0,
                                    newBest_0,
                                    bestMatch_0,
                                )
                            {
                                newBest_0 = style_FONT_PTR;
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
                    let style_FONT_PTR = v.as_ptr();
                    if (*style_FONT_PTR).isItalic == (*font).isItalic && (*style_FONT_PTR).isBold {
                        bestMatch_0 = style_FONT_PTR;
                        break;
                    }
                }
            }
            font = bestMatch_0
        }
    }
    // if there's optical size info, try to apply it
    if ptSize < 0.0f64 {
        ptSize = dsize as f64 / 10.0f64
    } // convert to decipoints for comparison with the opSize values
    if !font.is_null() && (*font).opSizeInfo.subFamilyID != 0i32 as libc::c_uint && ptSize > 0.0f64
    {
        ptSize = ptSize * 10.0f64;
        let mut bestMismatch: f64 = my_fmax(
            (*font).opSizeInfo.minSize as f64 - ptSize,
            ptSize - (*font).opSizeInfo.maxSize as f64,
        );
        if bestMismatch > 0.0f64 {
            let mut bestMatch_1: *mut XeTeXFontMgrFont = font;
            for (_, v) in (*(*parent).styles).iter() {
                let style_FONT_PTR = v.as_ptr();
                if !((*style_FONT_PTR).opSizeInfo.subFamilyID != (*font).opSizeInfo.subFamilyID) {
                    let mut mismatch: f64 = my_fmax(
                        (*style_FONT_PTR).opSizeInfo.minSize as f64 - ptSize,
                        ptSize - (*style_FONT_PTR).opSizeInfo.maxSize as f64,
                    );
                    if mismatch < bestMismatch {
                        bestMatch_1 = style_FONT_PTR;
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
            Scaled(((*font).opSizeInfo.designSize << 16).wrapping_div(10 as libc::c_uint) as i32)
    }
    if get_tracing_fonts_state() > 0i32 {
        diagnostic(false, || {
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
        });
    }
    return (*font).fontRef;
}
pub(crate) unsafe fn XeTeXFontMgr_getFullName(
    mut self_0: *const XeTeXFontMgr,
    mut font: PlatformFontRef,
) -> *const libc::c_char {
    // return the full name of the font, suitable for use in XeTeX source
    // without requiring style qualifiers
    let FONT_PTR = if let Some(FONT_PTR) = (*(*self_0).m_platformRefToFont).get(&font).cloned() {
        FONT_PTR
    } else {
        abort!("internal error {} in XeTeXFontMgr", 2i32,);
    };
    let FONT_PTR = FONT_PTR.as_ptr();

    if !(*FONT_PTR).m_fullName.is_null() {
        return CppStdString_cstr((*FONT_PTR).m_fullName);
    }
    return CppStdString_cstr((*FONT_PTR).m_psName);
}
pub(crate) unsafe fn XeTeXFontMgr_weightAndWidthDiff(
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
    let mut widDiff: libc::c_int = (((*a).width as libc::c_int - (*b).width as libc::c_int)
        as libc::c_long)
        .abs() as libc::c_int;
    if widDiff < 10i32 {
        widDiff *= 50i32
    }
    return ((((*a).weight as libc::c_int - (*b).weight as libc::c_int) as libc::c_long).abs()
        + widDiff as libc::c_long) as libc::c_int;
}
pub(crate) unsafe fn XeTeXFontMgr_styleDiff(
    mut _self_0: *const XeTeXFontMgr,
    mut a: *const XeTeXFontMgrFont,
    mut wt: libc::c_int,
    mut wd: libc::c_int,
    mut slant: libc::c_int,
) -> libc::c_int {
    let mut widDiff: libc::c_int =
        (((*a).width as libc::c_int - wd) as libc::c_long).abs() as libc::c_int;
    if widDiff < 10i32 {
        widDiff *= 200i32
    }
    return ((((*a).slant as libc::c_long).abs() - (slant as libc::c_long).abs()).abs()
        * 2i32 as libc::c_long
        + (((*a).weight as libc::c_int - wt) as libc::c_long).abs()
        + widDiff as libc::c_long) as libc::c_int;
}
pub(crate) unsafe fn XeTeXFontMgr_bestMatchFromFamily(
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
pub(crate) unsafe fn XeTeXFontMgr_getOpSize(
    mut _self_0: *mut XeTeXFontMgr,
    mut font: &XeTeXFontInst,
) -> *mut XeTeXFontMgrOpSizeRec {
    let mut hbFont: *mut hb_font_t = font.get_hb_font();
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
pub(crate) unsafe fn XeTeXFontMgr_getDesignSize(
    mut self_0: *mut XeTeXFontMgr,
    mut font: &XeTeXFontInst,
) -> f64 {
    let mut pSizeRec: *mut XeTeXFontMgrOpSizeRec = XeTeXFontMgr_getOpSize(self_0, font);
    if pSizeRec.is_null() {
        return 10.0f64;
    }
    let mut result: f64 = (*pSizeRec).designSize as f64 / 10.0f64;
    free(pSizeRec as *mut libc::c_void);
    return result;
}
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_base_getOpSizeRecAndStyleFlags(
    mut self_0: *mut XeTeXFontMgr,
    mut theFont: *mut XeTeXFontMgrFont,
) {
    use crate::freetype_sys_patch::{TT_Header, FT_SFNT_HEAD, FT_SFNT_OS2, FT_SFNT_POST};
    use freetype::freetype_sys::{TT_Postscript, TT_OS2};
    if let Some(font) = createFont((*theFont).fontRef, Scaled(655360)) {
        let mut pSizeRec: *mut XeTeXFontMgrOpSizeRec = XeTeXFontMgr_getOpSize(self_0, &font);
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
        let mut os2Table: *const TT_OS2 = font.get_font_table_ft(FT_SFNT_OS2) as *mut TT_OS2;
        if !os2Table.is_null() {
            (*theFont).weight = (*os2Table).usWeightClass;
            (*theFont).width = (*os2Table).usWidthClass;
            let mut sel: u16 = (*os2Table).fsSelection;
            (*theFont).isReg = sel as libc::c_int & 1i32 << 6i32 != 0i32;
            (*theFont).isBold = sel as libc::c_int & 1i32 << 5i32 != 0i32;
            (*theFont).isItalic = sel as libc::c_int & 1i32 << 0i32 != 0i32
        }
        let mut headTable: *const TT_Header =
            font.get_font_table_ft(FT_SFNT_HEAD) as *mut TT_Header;
        if !headTable.is_null() {
            let mut ms: u16 = (*headTable).Mac_Style;
            if ms as libc::c_int & 1i32 << 0i32 != 0i32 {
                (*theFont).isBold = true
            }
            if ms as libc::c_int & 1i32 << 1i32 != 0i32 {
                (*theFont).isItalic = true
            }
        }
        let mut postTable: *const TT_Postscript =
            font.get_font_table_ft(FT_SFNT_POST) as *const TT_Postscript;
        if !postTable.is_null() {
            (*theFont).slant = (1000_f64
                * (Fix2D(Scaled(-(*postTable).italicAngle as i32)) * std::f64::consts::PI / 180.)
                    .tan()) as libc::c_int as i16
        }
    };
}
// append a name but only if it's not already in the list
pub(crate) unsafe fn XeTeXFontMgr_appendToList(
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
pub(crate) unsafe fn XeTeXFontMgr_prependToList(
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
pub(crate) unsafe fn XeTeXFontMgr_addToMaps(
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
        let family: &mut XeTeXFontMgrFamily;
        if let Some(family_mut) = (*(*self_0).m_nameToFamily).get_mut(familyName) {
            family = family_mut.as_mut();
            family.minWeight = family.minWeight.min((*thisFont).weight);
            family.maxWeight = family.maxWeight.max((*thisFont).weight);
            family.minWidth = family.minWidth.min((*thisFont).width);
            family.maxWidth = family.maxWidth.max((*thisFont).width);
            family.minSlant = family.minSlant.min((*thisFont).slant);
            family.maxSlant = family.maxSlant.max((*thisFont).slant);
        } else {
            family = &mut *XeTeXFontMgrFamily_create();
            CppStdMap_put(
                (*self_0).m_nameToFamily,
                familyName.clone(),
                NonNull::new(family).expect("expect non-null pointer"),
            );
            family.minWeight = (*thisFont).weight;
            family.maxWeight = (*thisFont).weight;
            family.minWidth = (*thisFont).width;
            family.maxWidth = (*thisFont).width;
            family.minSlant = (*thisFont).slant;
            family.maxSlant = (*thisFont).slant;
        }
        if (*thisFont).parent.is_null() {
            (*thisFont).parent = family;
        }
        // ensure all style names in the family point to thisFont
        for styleName in (*(*names).m_styleNames).iter() {
            if !(*family.styles).contains_key(styleName) {
                CppStdMap_put(family.styles, styleName.clone(), thisFont_nonnull);
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
pub(crate) unsafe extern "C" fn XeTeXFontMgr_base_terminate(mut _self_0: *mut XeTeXFontMgr) {}
pub(crate) unsafe fn XeTeXFontMgr_base_ctor(mut self_0: *mut XeTeXFontMgr) {
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
