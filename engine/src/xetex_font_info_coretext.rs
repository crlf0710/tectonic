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

use crate::xetex_aatfont::getFileNameFromCTFont;
use crate::xetex_font_info::{XeTeXFontInst_base_ctor, XeTeXFontInst_initialize};
use freetype::freetype_sys;
use harfbuzz_sys::hb_font_t;
use libc::malloc;
use std::ptr;

pub(crate) type uint32_t = libc::c_uint;
pub(crate) type UniChar = UInt16;
pub(crate) type UInt16 = libc::c_ushort;
pub(crate) type Boolean = libc::c_uchar;

use crate::cf_prelude::*;

use crate::xetex_font_info::XeTeXFontInst;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct XeTeXFontInst_Mac {
    pub(crate) super_: XeTeXFontInst,
    pub(crate) m_descriptor: CTFontDescriptorRef,
    pub(crate) m_fontRef: CTFontRef,
}

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
pub(crate) unsafe fn XeTeXFontInst_Mac_initialize(
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
pub(crate) unsafe fn XeTeXFontInst_Mac_ctor(
    mut self_0: *mut XeTeXFontInst_Mac,
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: f32,
    mut status: *mut libc::c_int,
) {
    XeTeXFontInst_base_ctor(&mut (*self_0).super_, ptr::null(), 0i32, pointSize, status);
    (*self_0).super_.m_subdtor =
        Some(XeTeXFontInst_Mac_dtor as unsafe extern "C" fn(_: *mut XeTeXFontInst) -> ());
    (*self_0).m_descriptor = descriptor;
    (*self_0).m_fontRef = 0 as CTFontRef;
    XeTeXFontInst_Mac_initialize(self_0, status);
}

#[no_mangle]
pub(crate) unsafe extern "C" fn XeTeXFontInst_Mac_create(
    mut descriptor: CTFontDescriptorRef,
    mut pointSize: f32,
    mut status: *mut libc::c_int,
) -> *mut XeTeXFontInst_Mac {
    let mut value: *mut XeTeXFontInst_Mac =
        malloc(::std::mem::size_of::<XeTeXFontInst_Mac>()) as *mut XeTeXFontInst_Mac;
    XeTeXFontInst_Mac_ctor(value, descriptor, pointSize, status);
    return value;
}
