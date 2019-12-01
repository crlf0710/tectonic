#![cfg(target_os = "macos")]
#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

use freetype::freetype_sys;
use harfbuzz_sys::hb_font_t;
use std::ptr;

extern crate libc;
extern "C" {
    pub(crate) type __CFAllocator;
    pub(crate) type __CFString;
    pub(crate) type __CFArray;
    pub(crate) type __CFDictionary;
    pub(crate) type __CTFontDescriptor;
    pub(crate) type __CTFont;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn xpc_debugger_api_misuse_info() -> *const libc::c_char;
    #[no_mangle]
    fn XeTeXFontInst_initialize(
        self_0: *mut XeTeXFontInst,
        pathname: *const libc::c_char,
        index: libc::c_int,
        status: *mut libc::c_int,
    );
    #[no_mangle]
    fn XeTeXFontInst_base_ctor(
        self_0: *mut XeTeXFontInst,
        pathname: *const libc::c_char,
        index: libc::c_int,
        pointSize: libc::c_float,
        status: *mut libc::c_int,
    );
    #[no_mangle]
    fn getFileNameFromCTFont(ctFontRef: CTFontRef, index: *mut uint32_t) -> *mut libc::c_char;
    #[no_mangle]
    static kCFTypeArrayCallBacks: CFArrayCallBacks;
    #[no_mangle]
    static kCFTypeDictionaryKeyCallBacks: CFDictionaryKeyCallBacks;
    #[no_mangle]
    static kCTFontCascadeListAttribute: CFStringRef;
    #[no_mangle]
    static kCFTypeDictionaryValueCallBacks: CFDictionaryValueCallBacks;
    #[no_mangle]
    fn CFArrayCreate(
        allocator: CFAllocatorRef,
        values: *mut *const libc::c_void,
        numValues: CFIndex,
        callBacks: *const CFArrayCallBacks,
    ) -> CFArrayRef;
    #[no_mangle]
    fn CTFontCreateWithFontDescriptor(
        descriptor: CTFontDescriptorRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
    ) -> CTFontRef;
    #[no_mangle]
    fn CFDictionaryCreate(
        allocator: CFAllocatorRef,
        keys: *mut *const libc::c_void,
        values: *mut *const libc::c_void,
        numValues: CFIndex,
        keyCallBacks: *const CFDictionaryKeyCallBacks,
        valueCallBacks: *const CFDictionaryValueCallBacks,
    ) -> CFDictionaryRef;
    #[no_mangle]
    fn CTFontDescriptorCreateCopyWithAttributes(
        original: CTFontDescriptorRef,
        attributes: CFDictionaryRef,
    ) -> CTFontDescriptorRef;
    #[no_mangle]
    fn CFRelease(cf: CFTypeRef);
}
pub(crate) type uint32_t = libc::c_uint;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct CGAffineTransform {
    pub(crate) a: CGFloat,
    pub(crate) b: CGFloat,
    pub(crate) c: CGFloat,
    pub(crate) d: CGFloat,
    pub(crate) tx: CGFloat,
    pub(crate) ty: CGFloat,
}
pub(crate) type CGFloat = libc::c_double;
pub(crate) type CFAllocatorRef = *const __CFAllocator;
pub(crate) type UniChar = UInt16;
pub(crate) type UInt16 = libc::c_ushort;
pub(crate) type Boolean = libc::c_uchar;
pub(crate) type CFHashCode = libc::c_ulong;
pub(crate) type CFIndex = libc::c_long;
pub(crate) type CFTypeRef = *const libc::c_void;
pub(crate) type CFStringRef = *const __CFString;
pub(crate) type CFArrayRetainCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> *const libc::c_void>;
pub(crate) type CFArrayReleaseCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> ()>;
pub(crate) type CFArrayCopyDescriptionCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFStringRef>;
pub(crate) type CFArrayEqualCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> Boolean>;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct CFArrayCallBacks {
    pub(crate) version: CFIndex,
    pub(crate) retain: CFArrayRetainCallBack,
    pub(crate) release: CFArrayReleaseCallBack,
    pub(crate) copyDescription: CFArrayCopyDescriptionCallBack,
    pub(crate) equal: CFArrayEqualCallBack,
}
pub(crate) type CFArrayRef = *const __CFArray;
pub(crate) type CFDictionaryRetainCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> *const libc::c_void>;
pub(crate) type CFDictionaryReleaseCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> ()>;
pub(crate) type CFDictionaryCopyDescriptionCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFStringRef>;
pub(crate) type CFDictionaryEqualCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> Boolean>;
pub(crate) type CFDictionaryHashCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFHashCode>;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct CFDictionaryKeyCallBacks {
    pub(crate) version: CFIndex,
    pub(crate) retain: CFDictionaryRetainCallBack,
    pub(crate) release: CFDictionaryReleaseCallBack,
    pub(crate) copyDescription: CFDictionaryCopyDescriptionCallBack,
    pub(crate) equal: CFDictionaryEqualCallBack,
    pub(crate) hash: CFDictionaryHashCallBack,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct CFDictionaryValueCallBacks {
    pub(crate) version: CFIndex,
    pub(crate) retain: CFDictionaryRetainCallBack,
    pub(crate) release: CFDictionaryReleaseCallBack,
    pub(crate) copyDescription: CFDictionaryCopyDescriptionCallBack,
    pub(crate) equal: CFDictionaryEqualCallBack,
}
pub(crate) type CFDictionaryRef = *const __CFDictionary;
pub(crate) type CTFontDescriptorRef = *const __CTFontDescriptor;
pub(crate) type CTFontRef = *const __CTFont;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontInst {
    pub(crate) m_unitsPerEM: libc::c_ushort,
    pub(crate) m_pointSize: libc::c_float,
    pub(crate) m_ascent: libc::c_float,
    pub(crate) m_descent: libc::c_float,
    pub(crate) m_capHeight: libc::c_float,
    pub(crate) m_xHeight: libc::c_float,
    pub(crate) m_italicAngle: libc::c_float,
    pub(crate) m_vertical: bool,
    pub(crate) m_filename: *mut libc::c_char,
    pub(crate) m_index: uint32_t,
    pub(crate) m_ftFace: freetype_sys::FT_Face,
    pub(crate) m_backingData: *mut freetype_sys::FT_Byte,
    pub(crate) m_backingData2: *mut freetype_sys::FT_Byte,
    pub(crate) m_hbFont: *mut hb_font_t,
    pub(crate) m_subdtor: Option<unsafe extern "C" fn(_: *mut XeTeXFontInst) -> ()>,
}
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
/*
 *   file name:  XeTeXFontInst_Mac.h
 *
 *   created on: 2005-10-22
 *   created by: Jonathan Kew
 */
//#include <ApplicationServices/ApplicationServices.h>
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontInst_Mac {
    pub(crate) super_: XeTeXFontInst,
    pub(crate) m_descriptor: CTFontDescriptorRef,
    pub(crate) m_fontRef: CTFontRef,
}
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew
 Copyright (c) 2012, 2013 by Jiang Jiang
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
/*
 *   file name:  XeTeXFontInst_Mac.cpp
 *
 *   created on: 2005-10-22
 *   created by: Jonathan Kew
 */
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontInst_Mac_dtor(mut self_0: *mut XeTeXFontInst) {
    let mut real_self: *mut XeTeXFontInst_Mac = self_0 as *mut _;
    if !(*real_self).m_descriptor.is_null() {
        CFRelease((*real_self).m_descriptor as CFTypeRef);
    }
    if !(*real_self).m_fontRef.is_null() {
        CFRelease((*real_self).m_fontRef as CFTypeRef);
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontInst_Mac_initialize(
    mut self_0: *mut XeTeXFontInst_Mac,
    mut status: *mut libc::c_int,
) {
    if (*self_0).m_descriptor.is_null() {
        *status = 1i32;
        return;
    }
    if *status != 0i32 {
        (*self_0).m_descriptor = 0 as CTFontDescriptorRef
    }
    // Create a copy of original font descriptor with font cascading (fallback) disabled
    let mut emptyCascadeList: CFArrayRef = CFArrayCreate(
        0 as CFAllocatorRef,
        0 as *mut *const libc::c_void,
        0i32 as CFIndex,
        &kCFTypeArrayCallBacks,
    );
    let mut values: [*const libc::c_void; 1] = [emptyCascadeList as *const libc::c_void];
    let mut attributeKeys: [*const libc::c_void; 1] =
        [kCTFontCascadeListAttribute as *const libc::c_void];
    let mut attributes: CFDictionaryRef = CFDictionaryCreate(
        0 as CFAllocatorRef,
        attributeKeys.as_mut_ptr(),
        values.as_mut_ptr(),
        1i32 as CFIndex,
        &kCFTypeDictionaryKeyCallBacks,
        &kCFTypeDictionaryValueCallBacks,
    );
    CFRelease(emptyCascadeList as CFTypeRef);
    (*self_0).m_descriptor =
        CTFontDescriptorCreateCopyWithAttributes((*self_0).m_descriptor, attributes);
    CFRelease(attributes as CFTypeRef);
    (*self_0).m_fontRef = CTFontCreateWithFontDescriptor(
        (*self_0).m_descriptor,
        (*self_0).super_.m_pointSize as libc::c_double * 72.0f64 / 72.27f64,
        ptr::null(),
    );
    if !(*self_0).m_fontRef.is_null() {
        let mut pathname: *mut libc::c_char = ptr::null_mut();
        let mut index: uint32_t = 0;
        pathname = getFileNameFromCTFont((*self_0).m_fontRef, &mut index);
        XeTeXFontInst_initialize(
            &mut (*self_0).super_,
            pathname,
            index as libc::c_int,
            status,
        );
    } else {
        *status = 1i32;
        CFRelease((*self_0).m_descriptor as CFTypeRef);
        (*self_0).m_descriptor = 0 as CTFontDescriptorRef
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontInst_Mac_ctor(
    mut self_0: *mut XeTeXFontInst_Mac,
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) {
    XeTeXFontInst_base_ctor(&mut (*self_0).super_, ptr::null(), 0i32, pointSize, status);
    (*self_0).super_.m_subdtor =
        Some(XeTeXFontInst_Mac_dtor as unsafe extern "C" fn(_: *mut XeTeXFontInst) -> ());
    (*self_0).m_descriptor = descriptor;
    (*self_0).m_fontRef = 0 as CTFontRef;
    XeTeXFontInst_Mac_initialize(self_0, status);
}
/*
class XeTeXFontInst_Mac : public XeTeXFontInst
{
protected:

public:
                 XeTeXFontInst_Mac(CTFontDescriptorRef descriptor, float pointSize, int &status);

    virtual     ~XeTeXFontInst_Mac();

    virtual void initialize(int &status);
};
*/
#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontInst_Mac_create(
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) -> *mut XeTeXFontInst_Mac {
    let mut value: *mut XeTeXFontInst_Mac =
        malloc(::std::mem::size_of::<XeTeXFontInst_Mac>() as libc::c_ulong)
            as *mut XeTeXFontInst_Mac;
    XeTeXFontInst_Mac_ctor(value, descriptor, pointSize, status);
    return value;
}
