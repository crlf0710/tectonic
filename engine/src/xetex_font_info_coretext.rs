#![cfg(target_os = "macos")]
#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]

use harfbuzz_sys::hb_font_t;
use freetype::freetype_sys;

extern crate libc;
extern "C" {
    pub type __CFAllocator;
    pub type __CFString;
    pub type __CFArray;
    pub type __CFDictionary;
    pub type __CTFontDescriptor;
    pub type __CTFont;
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
pub type uint32_t = libc::c_uint;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CGAffineTransform {
    pub a: CGFloat,
    pub b: CGFloat,
    pub c: CGFloat,
    pub d: CGFloat,
    pub tx: CGFloat,
    pub ty: CGFloat,
}
pub type CGFloat = libc::c_double;
pub type CFAllocatorRef = *const __CFAllocator;
pub type UniChar = UInt16;
pub type UInt16 = libc::c_ushort;
pub type Boolean = libc::c_uchar;
pub type CFHashCode = libc::c_ulong;
pub type CFIndex = libc::c_long;
pub type CFTypeRef = *const libc::c_void;
pub type CFStringRef = *const __CFString;
pub type CFArrayRetainCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> *const libc::c_void>;
pub type CFArrayReleaseCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> ()>;
pub type CFArrayCopyDescriptionCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFStringRef>;
pub type CFArrayEqualCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> Boolean>;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CFArrayCallBacks {
    pub version: CFIndex,
    pub retain: CFArrayRetainCallBack,
    pub release: CFArrayReleaseCallBack,
    pub copyDescription: CFArrayCopyDescriptionCallBack,
    pub equal: CFArrayEqualCallBack,
}
pub type CFArrayRef = *const __CFArray;
pub type CFDictionaryRetainCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> *const libc::c_void>;
pub type CFDictionaryReleaseCallBack =
    Option<unsafe extern "C" fn(_: CFAllocatorRef, _: *const libc::c_void) -> ()>;
pub type CFDictionaryCopyDescriptionCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFStringRef>;
pub type CFDictionaryEqualCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> Boolean>;
pub type CFDictionaryHashCallBack =
    Option<unsafe extern "C" fn(_: *const libc::c_void) -> CFHashCode>;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CFDictionaryKeyCallBacks {
    pub version: CFIndex,
    pub retain: CFDictionaryRetainCallBack,
    pub release: CFDictionaryReleaseCallBack,
    pub copyDescription: CFDictionaryCopyDescriptionCallBack,
    pub equal: CFDictionaryEqualCallBack,
    pub hash: CFDictionaryHashCallBack,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CFDictionaryValueCallBacks {
    pub version: CFIndex,
    pub retain: CFDictionaryRetainCallBack,
    pub release: CFDictionaryReleaseCallBack,
    pub copyDescription: CFDictionaryCopyDescriptionCallBack,
    pub equal: CFDictionaryEqualCallBack,
}
pub type CFDictionaryRef = *const __CFDictionary;
pub type CTFontDescriptorRef = *const __CTFontDescriptor;
pub type CTFontRef = *const __CTFont;
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
    pub m_ftFace: freetype_sys::FT_Face,
    pub m_backingData: *mut freetype_sys::FT_Byte,
    pub m_backingData2: *mut freetype_sys::FT_Byte,
    pub m_hbFont: *mut hb_font_t,
    pub m_subdtor: Option<unsafe extern "C" fn(_: *mut XeTeXFontInst) -> ()>,
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
pub struct XeTeXFontInst_Mac {
    pub super_: XeTeXFontInst,
    pub m_descriptor: CTFontDescriptorRef,
    pub m_fontRef: CTFontRef,
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
pub unsafe extern "C" fn XeTeXFontInst_Mac_dtor(mut self_0: *mut XeTeXFontInst) {
    let mut real_self: *mut XeTeXFontInst_Mac = self_ptr::null_mut();
    if !(*real_self).m_descriptor.is_null() {
        CFRelease((*real_self).m_descriptor as CFTypeRef);
    }
    if !(*real_self).m_fontRef.is_null() {
        CFRelease((*real_self).m_fontRef as CFTypeRef);
    };
}
#[no_mangle]
pub unsafe extern "C" fn XeTeXFontInst_Mac_initialize(
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
pub unsafe extern "C" fn XeTeXFontInst_Mac_ctor(
    mut self_0: *mut XeTeXFontInst_Mac,
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) {
    XeTeXFontInst_base_ctor(
        &mut (*self_0).super_,
        ptr::null()::c_char,
        0i32,
        pointSize,
        status,
    );
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
pub unsafe extern "C" fn XeTeXFontInst_Mac_create(
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
