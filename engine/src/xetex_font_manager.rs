#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use std::cell::RefCell;
use std::collections::{BTreeMap, VecDeque};
use std::rc::Rc;

use crate::t_print_nl;

#[cfg(not(target_os = "macos"))]
#[path = "xetex_font_manager_fontconfig.rs"]
pub(crate) mod imp;

#[cfg(target_os = "macos")]
#[path = "xetex_font_manager_coretext.rs"]
pub(crate) mod imp;

use crate::xetex_ext::Fix2D;
use crate::xetex_ini::loaded_font_design_size;
use crate::xetex_layout_interface::createFont;
use crate::xetex_xetex0::{diagnostic, get_tracing_fonts_state};

use harfbuzz_sys::{hb_font_get_face, hb_ot_layout_get_size_params};

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
#[derive(Copy, Clone, Default)]
pub(crate) struct XeTeXFontMgrOpSizeRec {
    pub(crate) designSize: u32,
    pub(crate) subFamilyID: u32,
    pub(crate) nameCode: u32,
    pub(crate) minSize: u32,
    pub(crate) maxSize: u32,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrFamily {
    pub(crate) styles: BTreeMap<String, Rc<XeTeXFontMgrFont>>,
    pub(crate) minWeight: u16,
    pub(crate) maxWeight: u16,
    pub(crate) minWidth: u16,
    pub(crate) maxWidth: u16,
    pub(crate) minSlant: i16,
    pub(crate) maxSlant: i16,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrFont {
    pub(crate) m_fullName: Option<String>,
    pub(crate) m_psName: String,
    pub(crate) m_familyName: Option<String>,
    pub(crate) m_styleName: Option<String>,
    pub(crate) parent: Option<Rc<RefCell<XeTeXFontMgrFamily>>>,
    pub(crate) fontRef: PlatformFontRef,
    pub(crate) opSizeInfo: XeTeXFontMgrOpSizeRec,
    pub(crate) weight: u16,
    pub(crate) width: u16,
    pub(crate) slant: i16,
    pub(crate) isReg: bool,
    pub(crate) isBold: bool,
    pub(crate) isItalic: bool,
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgrNameCollection {
    pub(crate) m_familyNames: VecDeque<String>,
    pub(crate) m_styleNames: VecDeque<String>,
    pub(crate) m_fullNames: VecDeque<String>,
    pub(crate) m_psName: String,
    pub(crate) m_subFamily: String,
}
impl XeTeXFontMgrNameCollection {
    unsafe fn new() -> Self {
        Self {
            m_familyNames: VecDeque::default(),
            m_styleNames: VecDeque::default(),
            m_fullNames: VecDeque::default(),
            m_psName: String::new(),
            m_subFamily: String::new(),
        }
    }
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgr {
    pub(crate) m_nameToFont: BTreeMap<String, Rc<XeTeXFontMgrFont>>,
    pub(crate) m_nameToFamily: BTreeMap<String, Rc<RefCell<XeTeXFontMgrFamily>>>,
    pub(crate) m_platformRefToFont: BTreeMap<PlatformFontRef, Rc<XeTeXFontMgrFont>>,
    pub(crate) m_psNameToFont: BTreeMap<String, Rc<XeTeXFontMgrFont>>,
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

impl XeTeXFontMgrFamily {
    unsafe fn new() -> Self {
        /* default to 10bp */
        Self {
            minWeight: 0,
            maxWeight: 0,
            minWidth: 0,
            maxWidth: 0,
            minSlant: 0,
            maxSlant: 0,
            styles: BTreeMap::default(),
        }
    }
}
impl XeTeXFontMgrFont {
    unsafe fn new(ref_0: PlatformFontRef, ps_name: String) -> Self {
        Self {
            m_fullName: None,
            m_psName: ps_name,
            m_familyName: None,
            m_styleName: None,
            parent: None,
            fontRef: ref_0,
            weight: 0i32 as u16,
            width: 0i32 as u16,
            slant: 0i32 as i16,
            isReg: false,
            isBold: false,
            isItalic: false,
            opSizeInfo: XeTeXFontMgrOpSizeRec {
                subFamilyID: 0,
                designSize: 100,
                nameCode: 0,
                minSize: 0,
                maxSize: 0,
            },
        }
    }
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
#[cfg(not(target_os = "macos"))]
pub(crate) type FontMgr = self::imp::XeTeXFontMgr_FC;
#[cfg(target_os = "macos")]
pub(crate) type FontMgr = self::imp::XeTeXFontMgr_Mac;

pub(crate) static mut XeTeXFontMgr_sFontManager: Option<Box<FontMgr>> = None;
#[no_mangle]
pub(crate) static mut XeTeXFontMgr_sReqEngine: libc::c_char = 0i32 as libc::c_char;
/* use our own fmax function because it seems to be missing on certain platforms
(solaris2.9, at least) */
#[inline]
unsafe fn my_fmax(x: f64, y: f64) -> f64 {
    if x > y {
        x
    } else {
        y
    }
}
pub(crate) unsafe fn XeTeXFontMgr_GetFontManager() -> &'static mut FontMgr {
    if XeTeXFontMgr_sFontManager.is_none() {
        let mut mngr = FontMgr::create();
        mngr.initialize();
        XeTeXFontMgr_sFontManager = Some(mngr);
    }
    XeTeXFontMgr_sFontManager.as_deref_mut().unwrap()
}
pub(crate) unsafe fn XeTeXFontMgr_Terminate() {
    if let Some(mnrg) = XeTeXFontMgr_sFontManager.as_deref_mut() {
        mnrg.terminate();
        // we don't actually deallocate the manager, just ask it to clean up
        // any auxiliary data such as the cocoa pool or freetype/fontconfig stuff
        // as we still need to access font names after this is called
    }
}
pub(crate) unsafe fn XeTeXFontMgr_Destroy() {
    // Here we actually fully destroy the font manager.
    let _ = XeTeXFontMgr_sFontManager.take();
}
pub(crate) unsafe fn XeTeXFontMgr_getReqEngine(mut _self_0: &XeTeXFontMgr) -> libc::c_char {
    // return the requested rendering technology for the most recent findFont
    // or 0 if no specific technology was requested
    XeTeXFontMgr_sReqEngine
}
pub(crate) unsafe fn XeTeXFontMgr_setReqEngine(
    mut _self_0: &XeTeXFontMgr,
    reqEngine: libc::c_char,
) {
    XeTeXFontMgr_sReqEngine = reqEngine;
}
// above are singleton operation.

pub(crate) trait FindFont {
    unsafe fn find_font(
        &mut self,
        name: &str,
        variant: &mut String,
        ptSize: f64,
    ) -> PlatformFontRef;
}
impl<T> FindFont for T
where
    T: std::ops::DerefMut<Target = XeTeXFontMgr> + FontMgrExt,
{
    unsafe fn find_font(
        &mut self,
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
        let mut font: Option<Rc<XeTeXFontMgrFont>> = None;
        let mut dsize: i32 = 100i32;
        loaded_font_design_size = Scaled(655360);
        for pass in 0..2 {
            // try full name as given
            if let Some(font) = self.m_nameToFont.get(name) {
                if font.opSizeInfo.designSize != 0 {
                    dsize = font.opSizeInfo.designSize as i32
                }
                break;
            }
            // if there's a hyphen, split there and try Family-Style
            if let Some(hyph) = name[..name.len() - 1].find('-') {
                let family = name[..hyph].to_string();
                if let Some(family_ptr) = self.m_nameToFamily.get(&family) {
                    let style = name[hyph + 1..].to_string();
                    if let Some(style_FONT_PTR) = family_ptr.borrow().styles.get(&style).cloned() {
                        if style_FONT_PTR.opSizeInfo.designSize != 0 {
                            dsize = style_FONT_PTR.opSizeInfo.designSize as i32
                        }
                        font = Some(style_FONT_PTR.clone());
                        break;
                    }
                }
            }
            // try as PostScript name
            if let Some(font) = self.m_psNameToFont.get(name) {
                if font.opSizeInfo.designSize != 0 {
                    dsize = font.opSizeInfo.designSize as i32
                }
                break;
            }
            // try for the name as a family name
            if let Some(family_ptr) = self.m_nameToFamily.get(name) {
                // look for a family member with the "regular" bit set in OS/2
                let mut regFonts: i32 = 0;
                for (_k, v) in family_ptr.borrow().styles.iter() {
                    if v.isReg {
                        if regFonts == 0 {
                            font = Some(v.clone());
                        }
                        regFonts += 1;
                    }
                }
                // families with Ornament or similar fonts may flag those as Regular,
                // which confuses the search above... so try some known names
                if font.is_none() || regFonts > 1 {
                    // try for style "Regular", "Plain", "Normal", "Roman"
                    const regular_style_names: [&str; 4] = ["Regular", "Plain", "Normal", "Roman"];
                    for &style in regular_style_names.iter() {
                        if let Some(style_FONT_PTR) = family_ptr.borrow().styles.get(style) {
                            font = Some(style_FONT_PTR.clone());
                            break;
                        }
                    }
                }
                if font.is_none() {
                    // look through the family for the (weight, width, slant) nearest to (80, 100, 0)
                    font = self.best_match_from_family(&family_ptr.borrow(), 80, 100, 0)
                }
                if !font.is_none() {
                    break;
                }
            }
            if pass == 0i32 {
                // didn't find it in our caches, so do a platform search (may be relatively expensive);
                // this will update the caches with any fonts that seem to match the name given,
                // so that the second pass might find it
                self.search_for_host_platform_fonts(name);
            }
        }
        if font.is_none() {
            return 0 as PlatformFontRef;
        }
        let mut font = font.unwrap();
        let parent_clone = font.parent.clone();
        let parent = parent_clone.as_ref().unwrap().borrow();
        // if there are variant requests, try to apply them
        // and delete B, I, and S=... codes from the string, just retain /engine option
        XeTeXFontMgr_sReqEngine = 0i32 as libc::c_char;
        let mut reqBold = false;
        let mut reqItal = false;
        let mut font = if !variant.is_empty() {
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
                            ptSize += (cp[0] as i32 - '0' as i32) as f64 / dec;
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
                let mut bestMatch = Some(font.clone());
                if (font.slant as i32) < parent.maxSlant as i32 {
                    // try for a face with more slant
                    bestMatch = self.best_match_from_family(
                        &parent,
                        font.weight as i32,
                        font.width as i32,
                        parent.maxSlant as i32,
                    )
                }
                if bestMatch.is_some()
                    && Rc::ptr_eq(bestMatch.as_ref().unwrap(), &font)
                    && font.slant as i32 > parent.minSlant as i32
                {
                    // maybe the slant is negated, or maybe this was something like "Times-Italic/I"
                    bestMatch = self.best_match_from_family(
                        &parent,
                        font.weight as i32,
                        font.width as i32,
                        parent.minSlant as i32,
                    )
                }
                if parent.minWeight as i32 == parent.maxWeight as i32
                    && bestMatch.as_ref().unwrap().isBold as i32 != font.isBold as i32
                {
                    // try again using the bold flag, as we can't trust weight values
                    let mut newBest = None;
                    for (_, v) in parent.styles.iter() {
                        if v.isBold == font.isBold
                            && newBest.is_none()
                            && v.isItalic != font.isItalic
                        {
                            newBest = Some(v.clone());
                            break;
                        }
                    }
                    if newBest.is_some() {
                        bestMatch = newBest
                    }
                }
                if bestMatch.is_some() && Rc::ptr_eq(bestMatch.as_ref().unwrap(), &font) {
                    // maybe slant values weren't present; try the style bits as a fallback
                    bestMatch = None;
                    for (_, v) in parent.styles.iter() {
                        let style_FONT_PTR = v;
                        if style_FONT_PTR.isItalic != font.isItalic {
                            if parent.minWeight != parent.maxWeight {
                                // weight info was available, so try to match that
                                if bestMatch.is_none()
                                    || self.weight_and_width_diff(&style_FONT_PTR, &font)
                                        < self.weight_and_width_diff(
                                            &bestMatch.as_ref().unwrap(),
                                            &font,
                                        )
                                {
                                    bestMatch = Some(style_FONT_PTR.clone());
                                }
                            } else if bestMatch.is_none() && style_FONT_PTR.isBold == font.isBold {
                                bestMatch = Some(style_FONT_PTR.clone());
                                break;
                                // no weight info, so try matching style bits
                                // found a match, no need to look further as we can't distinguish!
                            }
                        }
                    }
                }
                if let Some(bm) = bestMatch {
                    font = bm;
                }
            }
            if reqBold {
                // try for more boldness, with the same width and slant
                let mut bestMatch_0 = Some(font.clone());
                if (font.weight as i32) < parent.maxWeight as i32 {
                    // try to increase weight by 1/2 x (max - min), rounding up
                    bestMatch_0 = self.best_match_from_family(
                        &parent,
                        font.weight as i32
                            + (parent.maxWeight as i32 - parent.minWeight as i32) / 2i32
                            + 1i32,
                        font.width as i32,
                        font.slant as i32,
                    );
                    if parent.minSlant as i32 == parent.maxSlant as i32 {
                        // double-check the italic flag, as we can't trust slant values
                        let mut newBest_0: Option<Rc<_>> = None;
                        for (_, v) in parent.styles.iter() {
                            let style_FONT_PTR = v;
                            if style_FONT_PTR.isItalic == font.isItalic
                                && (newBest_0.is_none()
                                    || self.weight_and_width_diff(
                                        &style_FONT_PTR,
                                        &bestMatch_0.as_ref().unwrap(),
                                    ) < self.weight_and_width_diff(
                                        &newBest_0.as_ref().unwrap(),
                                        &bestMatch_0.as_ref().unwrap(),
                                    ))
                            {
                                newBest_0 = Some(style_FONT_PTR.clone());
                            }
                        }
                        if newBest_0.is_some() {
                            bestMatch_0 = newBest_0
                        }
                    }
                }
                if bestMatch_0.is_some()
                    && Rc::ptr_eq(&bestMatch_0.as_ref().unwrap(), &font)
                    && !font.isBold
                {
                    for (_, v) in parent.styles.iter() {
                        let style_FONT_PTR = v;
                        if style_FONT_PTR.isItalic == font.isItalic && style_FONT_PTR.isBold {
                            bestMatch_0 = Some(style_FONT_PTR.clone());
                            break;
                        }
                    }
                }
                bestMatch_0
            } else {
                Some(font)
            }
        } else {
            Some(font)
        };
        // if there's optical size info, try to apply it
        if ptSize < 0.0f64 {
            ptSize = dsize as f64 / 10.0f64
        } // convert to decipoints for comparison with the opSize values
        if let Some(fnt) = font.as_ref() {
            if fnt.opSizeInfo.subFamilyID != 0 && ptSize > 0. {
                ptSize *= 10.;
                let mut bestMismatch: f64 = my_fmax(
                    fnt.opSizeInfo.minSize as f64 - ptSize,
                    ptSize - fnt.opSizeInfo.maxSize as f64,
                );
                if bestMismatch > 0. {
                    let mut bestMatch_1 = fnt.clone();
                    for (_, v) in parent.styles.iter() {
                        let style_FONT_PTR = v;
                        if style_FONT_PTR.opSizeInfo.subFamilyID == fnt.opSizeInfo.subFamilyID {
                            let mismatch = my_fmax(
                                style_FONT_PTR.opSizeInfo.minSize as f64 - ptSize,
                                ptSize - style_FONT_PTR.opSizeInfo.maxSize as f64,
                            );
                            if mismatch < bestMismatch {
                                bestMatch_1 = style_FONT_PTR.clone();
                                bestMismatch = mismatch
                            }
                            if bestMismatch <= 0. {
                                break;
                            }
                        }
                    }
                    font = Some(bestMatch_1);
                }
            }
        }
        if let Some(font) = font.as_ref() {
            if font.opSizeInfo.designSize != 0 {
                loaded_font_design_size =
                    Scaled((font.opSizeInfo.designSize << 16).wrapping_div(10) as i32)
            }
        }
        let font = font.unwrap();
        if get_tracing_fonts_state() > 0i32 {
            diagnostic(false, || {
                t_print_nl!(" -> {}", self.get_platform_font_desc(Self::font_ref(&font)));
            });
        }
        let x = font.fontRef;
        x
    }
}
impl XeTeXFontMgr {
    pub(crate) unsafe fn get_full_name(&self, font: PlatformFontRef) -> String {
        // return the full name of the font, suitable for use in XeTeX source
        // without requiring style qualifiers
        let FONT_PTR = if let Some(FONT_PTR) = self.m_platformRefToFont.get(&font) {
            FONT_PTR
        } else {
            abort!("internal error {} in XeTeXFontMgr", 2);
        };
        if let Some(name) = FONT_PTR.m_fullName.as_ref() {
            name.clone()
        } else {
            FONT_PTR.m_psName.clone()
        }
    }
    pub(crate) unsafe fn weight_and_width_diff(
        &self,
        a: &XeTeXFontMgrFont,
        b: &XeTeXFontMgrFont,
    ) -> i32 {
        if a.weight as i32 == 0i32 && a.width as i32 == 0i32 {
            // assume there was no OS/2 info
            if a.isBold as i32 == b.isBold as i32 {
                return 0i32;
            } else {
                return 10000i32;
            }
        }
        let mut widDiff: i32 = ((a.width as i32 - b.width as i32) as i64).abs() as i32;
        if widDiff < 10i32 {
            widDiff *= 50i32
        }
        (((a.weight as i32 - b.weight as i32) as i64).abs() + widDiff as i64) as i32
    }
    pub(crate) unsafe fn style_diff(
        &self,
        a: &XeTeXFontMgrFont,
        wt: i32,
        wd: i32,
        slant: i32,
    ) -> i32 {
        let mut widDiff: i32 = ((a.width as i32 - wd) as i64).abs() as i32;
        if widDiff < 10i32 {
            widDiff *= 200i32
        }
        (((a.slant as i64).abs() - (slant as i64).abs()).abs() * 2i32 as i64
            + ((a.weight as i32 - wt) as i64).abs()
            + widDiff as i64) as i32
    }
    pub(crate) unsafe fn best_match_from_family(
        &self,
        fam: &XeTeXFontMgrFamily,
        wt: i32,
        wd: i32,
        slant: i32,
    ) -> Option<Rc<XeTeXFontMgrFont>> {
        let mut bestMatch: Option<Rc<_>> = None;
        for (_, v) in fam.styles.iter() {
            if bestMatch.is_none()
                || self.style_diff(&v, wt, wd, slant)
                    < self.style_diff(&bestMatch.as_ref().unwrap(), wt, wd, slant)
            {
                bestMatch = Some(v.clone());
            }
        }
        bestMatch
    }
}
impl XeTeXFontInst {
    pub(crate) unsafe fn get_op_size(&self) -> Option<XeTeXFontMgrOpSizeRec> {
        let hbFont = self.get_hb_font();
        if hbFont.is_null() {
            return None;
        }
        let face = hb_font_get_face(hbFont);
        let mut size_rec = XeTeXFontMgrOpSizeRec::default();
        let ok = hb_ot_layout_get_size_params(
            face,
            &mut size_rec.designSize,
            &mut size_rec.subFamilyID,
            &mut size_rec.nameCode,
            &mut size_rec.minSize,
            &mut size_rec.maxSize,
        ) != 0;
        if ok {
            Some(size_rec)
        } else {
            None
        }
    }
    pub(crate) unsafe fn get_design_size(&self) -> f64 {
        if let Some(size_rec) = self.get_op_size() {
            size_rec.designSize as f64 / 10.
        } else {
            10.
        }
    }
}
impl XeTeXFontMgrFont {
    pub(crate) unsafe fn base_get_op_size_rec_and_style_flags(&mut self) {
        use crate::freetype_sys_patch::{TT_Header, FT_SFNT_HEAD, FT_SFNT_OS2, FT_SFNT_POST};
        use freetype::freetype_sys::{TT_Postscript, TT_OS2};
        if let Some(font) = createFont(self.fontRef, Scaled(655360)) {
            if let Some(size_rec) = font.get_op_size() {
                self.opSizeInfo.designSize = size_rec.designSize;
                if size_rec.subFamilyID == 0
                    && size_rec.nameCode == 0
                    && size_rec.minSize == 0
                    && size_rec.maxSize == 0
                {
                    // feature is valid, but no 'size' range
                } else {
                    self.opSizeInfo.subFamilyID = size_rec.subFamilyID;
                    self.opSizeInfo.nameCode = size_rec.nameCode;
                    self.opSizeInfo.minSize = size_rec.minSize;
                    self.opSizeInfo.maxSize = size_rec.maxSize;
                }
            }
            let os2Table = font.get_font_table_ft(FT_SFNT_OS2) as *mut TT_OS2;
            if !os2Table.is_null() {
                self.weight = (*os2Table).usWeightClass;
                self.width = (*os2Table).usWidthClass;
                let sel = (*os2Table).fsSelection;
                self.isReg = sel as i32 & 1i32 << 6i32 != 0i32;
                self.isBold = sel as i32 & 1i32 << 5i32 != 0i32;
                self.isItalic = sel as i32 & 1i32 << 0i32 != 0i32
            }
            let headTable = font.get_font_table_ft(FT_SFNT_HEAD) as *mut TT_Header;
            if !headTable.is_null() {
                let ms = (*headTable).Mac_Style;
                if ms as i32 & 1i32 << 0i32 != 0i32 {
                    self.isBold = true
                }
                if ms as i32 & 1i32 << 1i32 != 0i32 {
                    self.isItalic = true
                }
            }
            let postTable = font.get_font_table_ft(FT_SFNT_POST) as *const TT_Postscript;
            if !postTable.is_null() {
                self.slant = (1000_f64
                    * (Fix2D(Scaled(-(*postTable).italicAngle as i32)) * std::f64::consts::PI
                        / 180.)
                        .tan()) as i32 as i16
            }
        }
    }
}

pub(crate) trait AddToList {
    // append a name but only if it's not already in the list
    fn append_to_list(&mut self, cstr: &str);
    // prepend a name, removing it from later in the list if present
    fn prepend_to_list(&mut self, cstr: &str);
}
impl AddToList for VecDeque<String> {
    fn append_to_list(&mut self, cstr: &str) {
        fn has_occur(list: &VecDeque<String>, val: &str) -> bool {
            for item in list.iter() {
                if item == val {
                    return true;
                }
            }
            false
        }
        if has_occur(self, cstr) {
            return;
        }
        self.push_back(cstr.to_string());
    }
    fn prepend_to_list(&mut self, cstr: &str) {
        fn remove_first_occur(list: &mut VecDeque<String>, val: &str) {
            for (idx, item) in list.iter().enumerate() {
                if item == val {
                    list.remove(idx);
                    return;
                }
            }
        }

        remove_first_occur(self, cstr);
        self.push_front(cstr.to_string());
    }
}

pub(crate) trait AddToMaps {
    unsafe fn add_to_maps(
        &mut self,
        platformFont: PlatformFontRef,
        names: &XeTeXFontMgrNameCollection,
    );
}
impl<T> AddToMaps for T
where
    T: std::ops::DerefMut<Target = XeTeXFontMgr> + FontMgrExt,
{
    unsafe fn add_to_maps(
        &mut self,
        platformFont: PlatformFontRef,
        names: &XeTeXFontMgrNameCollection,
    ) {
        if self.m_platformRefToFont.contains_key(&platformFont) {
            return;
        }
        if names.m_psName.is_empty() {
            return;
        }
        if self.m_psNameToFont.contains_key(&names.m_psName) {
            return;
        }
        let mut thisFont = XeTeXFontMgrFont::new(platformFont, names.m_psName.clone());
        self.get_op_size_rec_and_style_flags(&mut thisFont);
        if !names.m_fullNames.is_empty() {
            thisFont.m_fullName = Some(names.m_fullNames[0].clone());
        }
        thisFont.m_familyName = if !names.m_familyNames.is_empty() {
            Some(names.m_familyNames[0].clone())
        } else {
            Some(names.m_psName.clone())
        };
        thisFont.m_styleName = if !names.m_styleNames.is_empty() {
            Some(names.m_styleNames[0].clone())
        } else {
            Some(String::new())
        };
        let (weight, width, slant) = (thisFont.weight, thisFont.width, thisFont.slant);
        for familyName in names.m_familyNames.iter() {
            let family = if let Some(family_rc) = self.m_nameToFamily.get_mut(familyName) {
                let family = &mut family_rc.borrow_mut();
                family.minWeight = family.minWeight.min(weight);
                family.maxWeight = family.maxWeight.max(weight);
                family.minWidth = family.minWidth.min(width);
                family.maxWidth = family.maxWidth.max(width);
                family.minSlant = family.minSlant.min(slant);
                family.maxSlant = family.maxSlant.max(slant);
                family_rc.clone()
            } else {
                let family_rc = Rc::new(RefCell::new(XeTeXFontMgrFamily::new()));
                self.m_nameToFamily
                    .insert(familyName.clone(), family_rc.clone());
                {
                    let family = &mut family_rc.borrow_mut();
                    family.minWeight = weight;
                    family.maxWeight = weight;
                    family.minWidth = width;
                    family.maxWidth = width;
                    family.minSlant = slant;
                    family.maxSlant = slant;
                }
                family_rc
            };
            if thisFont.parent.is_none() {
                thisFont.parent = Some(family.clone());
            }
        }
        let thisFont = Rc::new(thisFont);
        match self.m_psNameToFont.get_mut(&names.m_psName) {
            Some(v) => {
                *v = thisFont.clone();
            }
            None => {
                self.m_psNameToFont
                    .insert(names.m_psName.clone(), thisFont.clone());
            }
        }
        self.m_platformRefToFont
            .insert(platformFont, thisFont.clone());
        for familyName in names.m_familyNames.iter() {
            if let Some(family) = self.m_nameToFamily.get_mut(familyName) {
                // ensure all style names in the family point to thisFont
                for styleName in names.m_styleNames.iter() {
                    if !family.borrow().styles.contains_key(styleName) {
                        family
                            .borrow_mut()
                            .styles
                            .insert(styleName.clone(), thisFont.clone());
                    }
                    /*
                        else if (iFont->second != thisFont)
                            fprintf(stderr, "# Font name warning: ambiguous Style \"%s\" in Family \"%s\" (PSNames \"%s\" and \"%s\")\n",
                                        j->c_str(), i->c_str(), iFont->second->m_psName->c_str(), thisFont->m_psName->c_str());
                    */
                }
            }
        }
        for fullName in names.m_fullNames.iter() {
            if !self.m_nameToFont.contains_key(fullName) {
                self.m_nameToFont.insert(fullName.clone(), thisFont.clone());
            }
            /*
                    else if (iFont->second != thisFont)
                        fprintf(stderr, "# Font name warning: ambiguous FullName \"%s\" (PSNames \"%s\" and \"%s\")\n",
                                    i->c_str(), iFont->second->m_psName->c_str(), thisFont->m_psName->c_str());
            */
        }
    }
}

impl XeTeXFontMgr {
    pub(crate) fn base_ctor() -> Self {
        Self {
            m_nameToFont: BTreeMap::default(),
            m_nameToFamily: BTreeMap::default(),
            m_platformRefToFont: BTreeMap::default(),
            m_psNameToFont: BTreeMap::default(),
        }
    }
}

pub(crate) trait FontMgrExt {
    type FontRef;
    unsafe fn terminate(&mut self);
    unsafe fn get_platform_font_desc(&self, font: Self::FontRef) -> String;
    unsafe fn search_for_host_platform_fonts(&mut self, name: &str);

    unsafe fn read_names(&self, _: Self::FontRef) -> XeTeXFontMgrNameCollection;
    unsafe fn get_op_size_rec_and_style_flags(&self, _: &mut XeTeXFontMgrFont);
    fn font_ref(_: &XeTeXFontMgrFont) -> Self::FontRef;
}
