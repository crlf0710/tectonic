#![cfg(target_os = "macos")]
#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_layout_interface::collection_types::*;

use std::ffi::CString;
use std::ptr::{self, NonNull};

use objc::rc::autoreleasepool;
use objc::runtime::Object;
use objc_foundation::{NSArray, NSEnumerator, NSString};
use objc_id::Shared;
objc_foundation::object_struct!(NSAutoreleasePool);
objc_foundation::object_struct!(NSFontManager);
objc_foundation::object_struct!(NSFont);
type id = *mut Object;

use super::{
    XeTeXFontMgr, XeTeXFontMgrFamily, XeTeXFontMgrFont, XeTeXFontMgrNameCollection,
    XeTeXFontMgrOpSizeRec, XeTeXFontMgr_addToMaps, XeTeXFontMgr_appendToList,
    XeTeXFontMgr_base_ctor,
};

use libc::{free, malloc, strchr, strdup, strlen};

use crate::size_t;
pub(crate) type int16_t = libc::c_short;
pub(crate) type uint16_t = libc::c_ushort;
pub(crate) type UniChar = UInt16;
pub(crate) type UInt16 = libc::c_ushort;
pub(crate) type Boolean = libc::c_uchar;
pub(crate) type UInt8 = libc::c_uchar;
pub(crate) type UInt32 = libc::c_uint;
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

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontMgr_Mac {
    pub(crate) super_: XeTeXFontMgr,
}
#[inline]
unsafe extern "C" fn XeTeXFontMgrNameCollection_create() -> *mut XeTeXFontMgrNameCollection {
    let mut self_0: *mut XeTeXFontMgrNameCollection = malloc(::std::mem::size_of::<
        XeTeXFontMgrNameCollection,
    >()) as *mut XeTeXFontMgrNameCollection;
    (*self_0).m_familyNames = CppStdListOfString_create();
    (*self_0).m_styleNames = CppStdListOfString_create();
    (*self_0).m_fullNames = CppStdListOfString_create();
    (*self_0).m_psName = CppStdString_create();
    (*self_0).m_subFamily = CppStdString_create();
    return self_0;
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
    free(self_0 as *mut _);
}
pub(crate) unsafe fn XeTeXFontMgr_findFontWithName(
    mut name: CFStringRef,
    mut key: CFStringRef,
) -> CTFontDescriptorRef {
    let mut keys: [CFStringRef; 1] = [key];
    let mut values: [CFTypeRef; 1] = [name as CFTypeRef];
    let mut attributes: CFDictionaryRef = CFDictionaryCreate(
        0 as CFAllocatorRef,
        &mut keys as *mut [CFStringRef; 1] as *mut *const libc::c_void,
        &mut values as *mut [CFTypeRef; 1] as *mut *const libc::c_void,
        1i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    let mut descriptor: CTFontDescriptorRef = CTFontDescriptorCreateWithAttributes(attributes);
    CFRelease(attributes as CFTypeRef);
    let mut mandatoryAttributes: CFSetRef = CFSetCreate(
        0 as CFAllocatorRef,
        &mut keys as *mut [CFStringRef; 1] as *mut *const libc::c_void,
        1i32 as CFIndex,
        &kCFTypeSetCallBacks,
    );
    let mut matches: CFArrayRef =
        CTFontDescriptorCreateMatchingFontDescriptors(descriptor, mandatoryAttributes);
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
    mut self_0: *mut XeTeXFontMgr,
    mut font: CTFontRef,
    mut nameList: *mut CppStdListOfString,
    mut nameKey: CFStringRef,
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

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_Mac_readNames(
    mut self_0: *mut XeTeXFontMgr,
    mut fontRef: CTFontDescriptorRef,
) -> *mut XeTeXFontMgrNameCollection {
    let mut names: *mut XeTeXFontMgrNameCollection = XeTeXFontMgrNameCollection_create();
    let mut psName: CFStringRef =
        CTFontDescriptorCopyAttribute(fontRef, kCTFontNameAttribute) as CFStringRef;
    if psName.is_null() {
        return names;
    }
    autoreleasepool(|| {
        let psName: *const NSString = psName.cast();
        CppStdString_assign_from_const_char_ptr((*names).m_psName, msg_send![psName, UTF8String]);
        CFRelease(psName as CFTypeRef);
        let mut font: CTFontRef = CTFontCreateWithFontDescriptor(fontRef, 0.0f64, ptr::null());
        XeTeXFontMgr_Mac_appendNameToList(self_0, font, (*names).m_fullNames, kCTFontFullNameKey);
        XeTeXFontMgr_Mac_appendNameToList(
            self_0,
            font,
            (*names).m_familyNames,
            kCTFontFamilyNameKey,
        );
        XeTeXFontMgr_Mac_appendNameToList(self_0, font, (*names).m_styleNames, kCTFontStyleNameKey);
        CFRelease(font as CFTypeRef);
    });
    return names;
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFontsToCaches(
    mut self_0: *mut XeTeXFontMgr,
    fonts: CFArrayRef,
) {
    let fonts: *const NSArray<NSFont, Shared> = fonts.cast();
    let mut enumerator: id = msg_send![fonts, objectEnumerator];
    for mut aFont in NSEnumerator::<NSFont>::from_ptr(enumerator) {
        let mut fontRef: CTFontDescriptorRef =
            XeTeXFontMgr_findFontWithName(msg_send![aFont, objectAtIndex: 0], kCTFontNameAttribute);
        let mut names: *mut XeTeXFontMgrNameCollection = XeTeXFontMgr_readNames(self_0, fontRef);
        XeTeXFontMgr_addToMaps(self_0, fontRef, names);
        XeTeXFontMgrNameCollection_delete(names);
    }
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFamilyToCaches(
    mut self_0: *mut XeTeXFontMgr,
    mut familyRef: CTFontDescriptorRef,
) {
    let mut nameStr: CFStringRef =
        CTFontDescriptorCopyAttribute(familyRef, kCTFontFamilyNameAttribute) as CFStringRef;
    if !nameStr.is_null() {
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let mut members: *mut NSArray<NSFont, Shared> =
            msg_send![shared_font_manager, availableMembersOfFontFamily: nameStr];
        CFRelease(nameStr as CFTypeRef);
        XeTeXFontMgr_Mac_addFontsToCaches(self_0, members as CFArrayRef);
    };
}

pub(crate) unsafe fn XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(
    mut self_0: *mut XeTeXFontMgr,
    mut fontRef: CTFontDescriptorRef,
) {
    let mut name: CFStringRef =
        CTFontDescriptorCopyAttribute(fontRef, kCTFontNameAttribute) as CFStringRef;
    if !name.is_null() {
        let mut font: *mut NSFont = msg_send![class!(NSFont), fontWithName: name size: 10.0];
        CFRelease(name as CFTypeRef);
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let family_name: *const NSString = msg_send![font, familyName];
        let mut members: *mut NSArray<NSFont, Shared> = msg_send![
            shared_font_manager,
            availableMembersOfFontFamily: family_name
        ];
        XeTeXFontMgr_Mac_addFontsToCaches(self_0, members as CFArrayRef);
    };
}

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_Mac_searchForHostPlatformFonts(
    mut self_0: *mut XeTeXFontMgr,
    mut name: *const libc::c_char,
) {
    // the name might be:
    //  FullName
    //  Family-Style (if there's a hyphen)
    //  PSName
    //  Family
    // ...so we need to try it as each of these
    let mut nameStr: CFStringRef = CFStringCreateWithCString(
        kCFAllocatorDefault,
        name,
        kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
    );
    let mut matched: CTFontDescriptorRef =
        XeTeXFontMgr_findFontWithName(nameStr, kCTFontDisplayNameAttribute);
    if !matched.is_null() {
        // found it, so locate the family, and add all members to the caches
        XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(self_0, matched);
        CFRelease(matched as CFTypeRef);
        return;
    }
    let mut hyph_pos: *const libc::c_char = strchr(name, '-' as i32);
    let mut hyph: libc::c_int = (if !hyph_pos.is_null() {
        hyph_pos.wrapping_offset_from(name) as libc::c_long
    } else {
        -1i32 as libc::c_long
    }) as libc::c_int;
    if hyph > 0i32 && (hyph as usize) < strlen(name) - 1 {
        let mut family: *mut CppStdString = CppStdString_create();
        CppStdString_assign_n_chars(family, name, hyph as usize);
        let mut familyStr: CFStringRef = CFStringCreateWithCString(
            kCFAllocatorDefault,
            CppStdString_cstr(family),
            kCFStringEncodingUTF8 as libc::c_int as CFStringEncoding,
        );
        CppStdString_delete(family);
        let shared_font_manager: *const NSFontManager =
            msg_send![class!(NSFontManager), sharedFontManager];
        let mut familyMembers: *mut NSArray<NSFont, Shared> =
            msg_send![shared_font_manager, availableMembersOfFontFamily: familyStr];
        let count: i32 = msg_send![familyMembers, count];
        if count > 0i32 {
            XeTeXFontMgr_Mac_addFontsToCaches(self_0, familyMembers as CFArrayRef);
            return;
        }
        matched = XeTeXFontMgr_findFontWithName(familyStr, kCTFontFamilyNameAttribute);
        if !matched.is_null() {
            XeTeXFontMgr_Mac_addFamilyToCaches(self_0, matched);
            CFRelease(matched as CFTypeRef);
            return;
        }
    }
    matched = XeTeXFontMgr_findFontWithName(nameStr, kCTFontNameAttribute);
    if !matched.is_null() {
        XeTeXFontMgr_Mac_addFontAndSiblingsToCaches(self_0, matched);
        CFRelease(matched as CFTypeRef);
        return;
    }
    let shared_font_manager: *const NSFontManager =
        msg_send![class!(NSFontManager), sharedFontManager];
    let mut familyMembers_0: *mut NSArray<NSFont, Shared> =
        msg_send![shared_font_manager, availableMembersOfFontFamily: nameStr];
    let count: i32 = msg_send![familyMembers_0, count];
    if count > 0 {
        XeTeXFontMgr_Mac_addFontsToCaches(self_0, familyMembers_0 as CFArrayRef);
        return;
    }
    matched = XeTeXFontMgr_findFontWithName(nameStr, kCTFontFamilyNameAttribute);
    if !matched.is_null() {
        XeTeXFontMgr_Mac_addFamilyToCaches(self_0, matched);
        CFRelease(matched as CFTypeRef);
        return;
    };
}
#[no_mangle]
pub(crate) static mut pool: *mut NSAutoreleasePool = ptr::null_mut();

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_Mac_initialize(mut self_0: *mut XeTeXFontMgr) {
    pool = msg_send![class!(NSAutoreleasePool), new];
}

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_Mac_terminate(mut self_0: *mut XeTeXFontMgr) {
    if !pool.is_null() {
        let _: () = msg_send![pool, drain];
    }
}

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontMgr_Mac_getPlatformFontDesc(
    mut self_0: *const XeTeXFontMgr,
    mut descriptor: PlatformFontRef,
) -> *mut libc::c_char {
    let mut path: *mut libc::c_char = ptr::null_mut();
    let mut ctFont: CTFontRef = CTFontCreateWithFontDescriptor(descriptor, 0.0f64, ptr::null());
    if !ctFont.is_null() {
        let mut url: CFURLRef = 0 as CFURLRef;
        url = CTFontCopyAttribute(ctFont, kCTFontURLAttribute) as CFURLRef;
        if !url.is_null() {
            let mut posixPath: [UInt8; 1024] = [0; 1024];
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
        path = strdup(b"[unknown]\x00" as *const u8 as *const libc::c_char)
    }
    return strdup(path);
}
pub(crate) unsafe fn XeTeXFontMgr_Mac_ctor(mut self_0: *mut XeTeXFontMgr_Mac) {
    XeTeXFontMgr_base_ctor(&mut (*self_0).super_);
    (*self_0).super_.m_memfnInitialize =
        Some(XeTeXFontMgr_Mac_initialize as unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ());
    (*self_0).super_.m_memfnTerminate =
        Some(XeTeXFontMgr_Mac_terminate as unsafe extern "C" fn(_: *mut XeTeXFontMgr) -> ());
    (*self_0).super_.m_memfnGetPlatformFontDesc = Some(
        XeTeXFontMgr_Mac_getPlatformFontDesc
            as unsafe extern "C" fn(
                _: *const XeTeXFontMgr,
                _: PlatformFontRef,
            ) -> *mut libc::c_char,
    );
    (*self_0).super_.m_memfnSearchForHostPlatformFonts = Some(
        XeTeXFontMgr_Mac_searchForHostPlatformFonts
            as unsafe extern "C" fn(_: *mut XeTeXFontMgr, _: *const libc::c_char) -> (),
    );
    (*self_0).super_.m_memfnReadNames = Some(
        XeTeXFontMgr_Mac_readNames
            as unsafe extern "C" fn(
                _: *mut XeTeXFontMgr,
                _: CTFontDescriptorRef,
            ) -> *mut XeTeXFontMgrNameCollection,
    );
}
pub(crate) unsafe fn XeTeXFontMgr_Mac_create() -> *mut XeTeXFontMgr_Mac {
    let mut self_0: *mut XeTeXFontMgr_Mac =
        malloc(::std::mem::size_of::<XeTeXFontMgr_Mac>()) as *mut XeTeXFontMgr_Mac;
    XeTeXFontMgr_Mac_ctor(self_0);
    return self_0;
}
