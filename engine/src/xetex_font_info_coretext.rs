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

use harfbuzz_sys::hb_font_t;
use std::ffi::CStr;

use super::XeTeXFontInst;

use crate::xetex_aatfont::cf_prelude::{
    kCFTypeArrayCallBacks, kCFTypeDictionaryKeyCallBacks, kCFTypeDictionaryValueCallBacks,
    kCTFontCascadeListAttribute, CFAllocatorRef, CFArrayCreate, CFArrayRef, CFDictionaryCreate,
    CFDictionaryRef, CFIndex, CFRelease, CFTypeRef, CGAffineTransform,
    CTFontCreateWithFontDescriptor, CTFontDescriptorCreateCopyWithAttributes, CTFontDescriptorRef,
    CTFontRef,
};
use crate::xetex_aatfont::getFileNameFromCTFont;
use crate::xetex_font_info::XeTeXFontInst_initialize;

extern crate libc;
extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn xpc_debugger_api_misuse_info() -> *const libc::c_char;
}

pub type UniChar = UInt16;
pub type UInt16 = libc::c_ushort;
pub type Boolean = libc::c_uchar;
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
#[derive(Clone)]
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
pub unsafe fn XeTeXFontInst_Mac_dtor(mut self_0: *mut XeTeXFontInst) {
    let mut real_self: *mut XeTeXFontInst_Mac = self_0 as *mut XeTeXFontInst_Mac;
    if !(*real_self).m_descriptor.is_null() {
        CFRelease((*real_self).m_descriptor as CFTypeRef);
    }
    if !(*real_self).m_fontRef.is_null() {
        CFRelease((*real_self).m_fontRef as CFTypeRef);
    };
}
#[no_mangle]
pub unsafe fn XeTeXFontInst_Mac_initialize(
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
        (*self_0).super_.m_pointSize as f64 * 72.0f64 / 72.27f64,
        0 as *const CGAffineTransform,
    );
    if !(*self_0).m_fontRef.is_null() {
        let mut pathname: *mut libc::c_char = 0 as *mut libc::c_char;
        let mut index: u32 = 0;
        pathname = getFileNameFromCTFont((*self_0).m_fontRef, &mut index);
        if let Err(e) = (*self_0)
            .super_
            .init(CStr::from_ptr(pathname), index as libc::c_int)
        {
            *status = e;
        }
    } else {
        *status = 1i32;
        CFRelease((*self_0).m_descriptor as CFTypeRef);
        (*self_0).m_descriptor = 0 as CTFontDescriptorRef
    };
}
#[no_mangle]
pub unsafe fn XeTeXFontInst_Mac_ctor(
    mut self_0: *mut XeTeXFontInst_Mac,
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: libc::c_float,
    mut status: *mut libc::c_int,
) {
    (*self_0).super_ = XeTeXFontInst::new(None, 0, pointSize, status);
    (*self_0).super_.m_subdtor =
        Some(XeTeXFontInst_Mac_dtor as unsafe fn(_: *mut XeTeXFontInst) -> ());
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
pub unsafe fn XeTeXFontInst_Mac_create(
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
