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
use std::ptr;

pub(crate) type uint32_t = libc::c_uint;
pub(crate) type Boolean = libc::c_uchar;

use crate::cf_prelude::*;

use crate::xetex_font_info::XeTeXFontInst;

#[derive(Clone)]
pub(crate) struct XeTeXFontInst_Mac {
    pub(crate) super_: XeTeXFontInst,
    pub(crate) m_descriptor: CTFontDescriptorRef,
    pub(crate) m_fontRef: CTFontRef,
}

impl core::ops::Deref for XeTeXFontInst_Mac {
    type Target = XeTeXFontInst;

    fn deref(&self) -> &Self::Target {
        &self.super_
    }
}
impl core::ops::DerefMut for XeTeXFontInst_Mac {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.super_
    }
}

impl Drop for XeTeXFontInst_Mac {
    fn drop(&mut self) {
        unsafe {
            if !self.m_descriptor.is_null() {
                CFRelease(self.m_descriptor as CFTypeRef);
            }
            if !self.m_fontRef.is_null() {
                CFRelease(self.m_fontRef as CFTypeRef);
            };
        }
    }
}

impl XeTeXFontInst_Mac {
    pub(crate) unsafe fn ctor(
        mut descriptor: CTFontDescriptorRef,
        mut pointSize: f32,
        mut status: *mut libc::c_int,
    ) -> Self {
        let mut super_ = XeTeXFontInst::base_ctor("", 0i32, pointSize, status);
        let mut m_descriptor = descriptor;
        let mut m_fontRef = 0 as CTFontRef;

        if m_descriptor.is_null() {
            *status = 1i32;
            return Self {
                super_,
                m_descriptor,
                m_fontRef,
            };
        }
        if *status != 0i32 {
            m_descriptor = 0 as CTFontDescriptorRef
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
        m_descriptor = CTFontDescriptorCreateCopyWithAttributes(m_descriptor, attributes);
        CFRelease(attributes as CFTypeRef);
        m_fontRef = CTFontCreateWithFontDescriptor(
            m_descriptor,
            super_.m_pointSize as f64 * 72.0f64 / 72.27f64,
            ptr::null(),
        );
        if !m_fontRef.is_null() {
            let mut index: uint32_t = 0;
            let mut pathname = getFileNameFromCTFont(m_fontRef, &mut index);
            super_ = XeTeXFontInst::base_ctor(
                &pathname,
                index as libc::c_int,
                super_.m_pointSize,
                status,
            );
        } else {
            *status = 1i32;
            CFRelease(m_descriptor as CFTypeRef);
            m_descriptor = 0 as CTFontDescriptorRef
        };

        Self {
            super_,
            m_descriptor,
            m_fontRef,
        }
    }

    pub(crate) unsafe fn create(
        mut descriptor: CTFontDescriptorRef,
        mut pointSize: f32,
        mut status: *mut libc::c_int,
    ) -> Box<Self> {
        Box::new(Self::ctor(descriptor, pointSize, status))
    }

    pub(crate) unsafe fn wrapper(
        pathname: &str,
        mut index: libc::c_int,
        mut pointSize: f32,
        mut status: *mut libc::c_int,
    ) -> Box<Self> {
        Box::new(Self {
            super_: XeTeXFontInst::base_ctor(pathname, index, pointSize, status),
            m_descriptor: 0 as CTFontDescriptorRef,
            m_fontRef: 0 as CTFontRef,
        })
    }
}
