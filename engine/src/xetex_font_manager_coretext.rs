#![cfg(target_os = "macos")]
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use std::collections::VecDeque;

use crate::xetex_font_manager::FontMgrExt;

use std::ffi::{CStr, CString};
use std::ptr;

use objc::rc::autoreleasepool;
use objc::runtime::Object;
use objc_foundation::{NSArray, NSEnumerator, NSString};
use objc_id::Shared;
objc_foundation::object_struct!(NSAutoreleasePool);
objc_foundation::object_struct!(NSFontManager);
objc_foundation::object_struct!(NSFont);
type id = *mut Object;

use super::{
    XeTeXFontMgr, XeTeXFontMgrFont, XeTeXFontMgrNameCollection, XeTeXFontMgr_addToMaps,
    XeTeXFontMgr_appendToList, XeTeXFontMgr_base_ctor,
};

use libc::{free, strchr, strdup, strlen};

pub(crate) type Boolean = libc::c_uchar;
use crate::cf_prelude::*;

use super::PlatformFontRef;

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

#[derive(Clone)]
pub(crate) struct XeTeXFontMgr_Mac {
    pub(crate) super_: XeTeXFontMgr,
}
impl core::ops::Deref for XeTeXFontMgr_Mac {
    type Target = XeTeXFontMgr;
    fn deref(&self) -> &Self::Target {
        &self.super_
    }
}
impl core::ops::DerefMut for XeTeXFontMgr_Mac {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.super_
    }
}
pub(crate) unsafe fn XeTeXFontMgr_findFontWithName(
    name: CFStringRef,
    key: CFStringRef,
) -> CTFontDescriptorRef {
    let mut keys: [CFStringRef; 1] = [key];
    let mut values: [CFTypeRef; 1] = [name as CFTypeRef];
    let attributes = CFDictionaryCreate(
        0 as CFAllocatorRef,
        &mut keys as *mut [CFStringRef; 1] as *mut *const libc::c_void,
        &mut values as *mut [CFTypeRef; 1] as *mut *const libc::c_void,
        1i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    let descriptor = CTFontDescriptorCreateWithAttributes(attributes);
    CFRelease(attributes as CFTypeRef);
    let mandatoryAttributes = CFSetCreate(
        0 as CFAllocatorRef,
        &mut keys as *mut [CFStringRef; 1] as *mut *const libc::c_void,
        1i32 as CFIndex,
        &kCFTypeSetCallBacks,
    );
    let matches = CTFontDescriptorCreateMatchingFontDescriptors(descriptor, mandatoryAttributes);
    CFRelease(mandatoryAttributes as CFTypeRef);
    CFRelease(descriptor as CFTypeRef);
    let mut matched: CTFontDescriptorRef = 0 as CTFontDescriptorRef;
    if !matches.is_null() {
        if CFArrayGetCount(matches) != 0 {
            matched = CFArrayGetValueAtIndex(matches, 0i32 as CFIndex) as CTFontDescriptorRef;
            CFRetain(matched as CFTypeRef);
        }
        CFRelease(matches as CFTypeRef);
    }
    return matched;
}
pub(crate) unsafe fn XeTeXFontMgr_Mac_appendNameToList(
    self_0: &XeTeXFontMgr_Mac,
    font: CTFontRef,
    nameList: &mut VecDeque<String>,
    nameKey: CFStringRef,
) {
    let name: CFStringRef = CTFontCopyName(font, nameKey);
    let name: *const NSString = name.cast();
    if !name.is_null() {
        XeTeXFontMgr_appendToList(self_0, nameList, msg_send![name, UTF8String]);
        CFRelease(name as CFTypeRef);
    }
    let mut language: CFStringRef = 0 as CFStringRef;
    let name = CTFontCopyLocalizedName(font, nameKey, &mut language);
    let name: *const NSString = name.cast();
    if !name.is_null() {
        XeTeXFontMgr_appendToList(self_0, nameList, msg_send![name, UTF8String]);
        CFRelease(name as CFTypeRef);
    };
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFontsToCaches(
    self_0: &mut XeTeXFontMgr_Mac,
    fonts: CFArrayRef,
) {
    let fonts: *const NSArray<NSFont, Shared> = fonts.cast();
    let enumerator: id = msg_send![fonts, objectEnumerator];
    for aFont in NSEnumerator::<NSFont>::from_ptr(enumerator) {
        let fontRef =
            XeTeXFontMgr_findFontWithName(msg_send![aFont, objectAtIndex: 0], kCTFontNameAttribute);
        let names = self_0.read_names(fontRef);
        XeTeXFontMgr_addToMaps(self_0, fontRef, &names);
    }
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFamilyToCaches(
    self_0: &mut XeTeXFontMgr_Mac,
    familyRef: CTFontDescriptorRef,
) {
    let nameStr =
        CTFontDescriptorCopyAttribute(familyRef, kCTFontFamilyNameAttribute) as CFStringRef;
    if !nameStr.is_null() {
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let members: *mut NSArray<NSFont, Shared> =
            msg_send![shared_font_manager, availableMembersOfFontFamily: nameStr];
        CFRelease(nameStr as CFTypeRef);
        XeTeXFontMgr_Mac_addFontsToCaches(self_0, members as CFArrayRef);
    };
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(
    self_0: &mut XeTeXFontMgr_Mac,
    fontRef: CTFontDescriptorRef,
) {
    let name = CTFontDescriptorCopyAttribute(fontRef, kCTFontNameAttribute) as CFStringRef;
    if !name.is_null() {
        let font: *mut NSFont = msg_send![class!(NSFont), fontWithName: name size: 10.0];
        CFRelease(name as CFTypeRef);
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let family_name: *const NSString = msg_send![font, familyName];
        let members: *mut NSArray<NSFont, Shared> = msg_send![
            shared_font_manager,
            availableMembersOfFontFamily: family_name
        ];
        XeTeXFontMgr_Mac_addFontsToCaches(self_0, members as CFArrayRef);
    };
}

#[no_mangle]
pub(crate) static mut pool: *mut NSAutoreleasePool = ptr::null_mut();

impl XeTeXFontMgr_Mac {
    pub(crate) unsafe fn initialize(&mut self) {
        pool = msg_send![class!(NSAutoreleasePool), new];
    }
}

impl FontMgrExt for XeTeXFontMgr_Mac {
    type FontRef = PlatformFontRef;
    unsafe fn terminate(&mut self) {
        if !pool.is_null() {
            let _: () = msg_send![pool, drain];
        }
    }
    unsafe fn get_platform_font_desc(&self, descriptor: Self::FontRef) -> String {
        let mut path: *mut libc::c_char = ptr::null_mut();
        let ctFont = CTFontCreateWithFontDescriptor(descriptor, 0.0f64, ptr::null());
        if !ctFont.is_null() {
            let url = CTFontCopyAttribute(ctFont, kCTFontURLAttribute) as CFURLRef;
            if !url.is_null() {
                let mut posixPath: [u8; 1024] = [0; 1024];
                if CFURLGetFileSystemRepresentation(
                    url,
                    1i32 as Boolean,
                    posixPath.as_mut_ptr(),
                    1024i32 as CFIndex,
                ) != 0
                {
                    path = strdup(posixPath.as_mut_ptr() as *mut libc::c_char)
                }
                CFRelease(url as CFTypeRef);
            }
            CFRelease(ctFont as CFTypeRef);
        }
        if strlen(path) == 0 {
            free(path as *mut libc::c_void);
            path = ptr::null_mut()
        }
        if path.is_null() {
            "[unknown]".to_string()
        } else {
            crate::c_pointer_to_str(path).to_string()
        }
    }
    unsafe fn search_for_host_platform_fonts(&mut self, name: &str) {
        // the name might be:
        //  FullName
        //  Family-Style (if there's a hyphen)
        //  PSName
        //  Family
        // ...so we need to try it as each of these
        let cname = CString::new(name).unwrap();
        let nameStr = CFStringCreateWithCString(
            kCFAllocatorDefault,
            cname.as_ptr(),
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        );
        let mut matched: CTFontDescriptorRef =
            XeTeXFontMgr_findFontWithName(nameStr, kCTFontDisplayNameAttribute);
        if !matched.is_null() {
            // found it, so locate the family, and add all members to the caches
            XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(self, matched);
            CFRelease(matched as CFTypeRef);
            return;
        }
        if let Some(hyph) = name[..name.len() - 1].find('-') {
            let family = CString::new(&name[..hyph]).unwrap();
            let familyStr = CFStringCreateWithCString(
                kCFAllocatorDefault,
                family.as_ptr(),
                kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
            );
            let shared_font_manager: *const NSFontManager =
                msg_send![class!(NSFontManager), sharedFontManager];
            let familyMembers: *mut NSArray<NSFont, Shared> =
                msg_send![shared_font_manager, availableMembersOfFontFamily: familyStr];
            let count: i32 = msg_send![familyMembers, count];
            if count > 0i32 {
                XeTeXFontMgr_Mac_addFontsToCaches(self, familyMembers as CFArrayRef);
                return;
            }
            matched = XeTeXFontMgr_findFontWithName(familyStr, kCTFontFamilyNameAttribute);
            if !matched.is_null() {
                XeTeXFontMgr_Mac_addFamilyToCaches(self, matched);
                CFRelease(matched as CFTypeRef);
                return;
            }
        }
        matched = XeTeXFontMgr_findFontWithName(nameStr, kCTFontNameAttribute);
        if !matched.is_null() {
            XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(self, matched);
            CFRelease(matched as CFTypeRef);
            return;
        }
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let familyMembers_0: *mut NSArray<NSFont, Shared> =
            msg_send![shared_font_manager, availableMembersOfFontFamily: nameStr];
        let count: i32 = msg_send![familyMembers_0, count];
        if count > 0 {
            XeTeXFontMgr_Mac_addFontsToCaches(self, familyMembers_0 as CFArrayRef);
            return;
        }
        matched = XeTeXFontMgr_findFontWithName(nameStr, kCTFontFamilyNameAttribute);
        if !matched.is_null() {
            XeTeXFontMgr_Mac_addFamilyToCaches(self, matched);
            CFRelease(matched as CFTypeRef);
            return;
        };
    }

    unsafe fn read_names(&self, fontRef: Self::FontRef) -> XeTeXFontMgrNameCollection {
        let mut names = XeTeXFontMgrNameCollection::new();
        let psName = CTFontDescriptorCopyAttribute(fontRef, kCTFontNameAttribute) as CFStringRef;
        if psName.is_null() {
            return names;
        }
        autoreleasepool(|| {
            let psName: *const NSString = psName.cast();
            names.m_psName = CStr::from_ptr(msg_send![psName, UTF8String])
                .to_str()
                .unwrap()
                .to_string();
            CFRelease(psName as CFTypeRef);
            let font = CTFontCreateWithFontDescriptor(fontRef, 0.0f64, ptr::null());
            XeTeXFontMgr_Mac_appendNameToList(
                self,
                font,
                &mut names.m_fullNames,
                kCTFontFullNameKey,
            );
            XeTeXFontMgr_Mac_appendNameToList(
                self,
                font,
                &mut names.m_familyNames,
                kCTFontFamilyNameKey,
            );
            XeTeXFontMgr_Mac_appendNameToList(
                self,
                font,
                &mut names.m_styleNames,
                kCTFontStyleNameKey,
            );
            CFRelease(font as CFTypeRef);
        });
        return names;
    }
    unsafe fn get_op_size_rec_and_style_flags(&self, theFont: &mut XeTeXFontMgrFont) {
        self.base_get_op_size_rec_and_style_flags(theFont);
    }
}
pub(crate) unsafe fn XeTeXFontMgr_Mac_ctor() -> XeTeXFontMgr_Mac {
    let super_ = XeTeXFontMgr_base_ctor();
    XeTeXFontMgr_Mac { super_ }
}
pub(crate) unsafe fn XeTeXFontMgr_Mac_create() -> Box<XeTeXFontMgr_Mac> {
    Box::new(XeTeXFontMgr_Mac_ctor())
}
