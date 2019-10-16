#![feature(extern_types)]
#![feature(ptr_wrapping_offset_from)]
#![feature(c_variadic)]
#![feature(const_transmute)]
#![allow(unused_unsafe)]
#![deny(unused_assignments)]
#![deny(clippy::reverse_range_loop)]

extern crate tectonic_bridge as bridge;
use bridge::*;

#[macro_export]
macro_rules! info(
    ($($arg:tt)*) => {{
        use crate::dpx_error::{
            _dpx_ensure_output_handle,
            _dpx_message_handle,
            _dpx_quietness,
            _last_message_type,
            DPX_MESG_INFO
        };
        use std::io::Write;
        if !(unsafe{_dpx_quietness} > 0) {
            _dpx_ensure_output_handle();
            write!(_dpx_message_handle.as_mut().unwrap(), $($arg)*).unwrap();
            unsafe{_last_message_type = DPX_MESG_INFO;}
        }
    }};
);

#[macro_export]
macro_rules! spc_warn(
    ($spe:ident, $($arg:tt)*) => {{
        let _spe = $spe;
        warn!($($arg)*);
    }};
);

#[macro_export]
macro_rules! warn(
    ($fmt:literal) => {
        warn!($fmt,)
    };
    ($fmt:literal, $($arg:tt)*) => {{
        use crate::dpx_error::{
            _dpx_ensure_output_handle,
            _dpx_message_handle,
            _dpx_quietness,
            _last_message_type,
            DPX_MESG_INFO,
            DPX_MESG_WARN,
        };
        use std::io::Write;
        if !(unsafe{_dpx_quietness} > 1) {
            _dpx_ensure_output_handle();
            let handle = unsafe { _dpx_message_handle.as_mut().unwrap() };
            if unsafe{_last_message_type as u32 == DPX_MESG_INFO as u32} {
                writeln!(handle).unwrap();
            }
            writeln!(handle, concat!("warning: ", $fmt), $($arg)*).unwrap();
            let mut v = format!(concat!($fmt, "\x00"), $($arg)*);
            unsafe{crate::ttstub_issue_warning_slice(v.as_bytes());}
            unsafe{_last_message_type = DPX_MESG_WARN;}
        }
    }};
);

pub trait Warn<E>: Sized {}

trait DisplayExt {
    type Adapter: core::fmt::Display;
    fn display(self) -> Self::Adapter;
}

impl<'a> DisplayExt for &'a std::ffi::CStr {
    type Adapter = std::borrow::Cow<'a, str>;
    fn display(self) -> Self::Adapter {
        self.to_string_lossy()
    }
}

impl<'a> DisplayExt for &'a [u8] {
    type Adapter = std::borrow::Cow<'a, str>;
    fn display(self) -> Self::Adapter {
        String::from_utf8_lossy(self)
    }
}

pub(crate) fn isblank(c: libc::c_int) -> libc::c_int {
    (c == ' ' as _ || c == '\t' as _) as _
}

#[inline]
pub(crate) unsafe fn strstartswith(s: *const i8, prefix: *const i8) -> *const i8 {
    let length = libc::strlen(prefix);
    if libc::strncmp(s, prefix, length) == 0i32 {
        return s.offset(length as isize);
    }
    0 as *const i8
}

#[inline]
pub(crate) unsafe fn streq_ptr(s1: *const i8, s2: *const i8) -> bool {
    if !s1.is_null() && !s2.is_null() {
        return libc::strcmp(s1, s2) == 0i32;
    }
    false
}

#[inline]
pub(crate) unsafe fn mfree(ptr: *mut libc::c_void) -> *mut libc::c_void {
    libc::free(ptr);
    std::ptr::null_mut()
}

use core::mem::MaybeUninit;
pub trait FromLEByteSlice {
    fn from_le_byte_slice(b: &[u8]) -> Self;
}
impl FromLEByteSlice for u32 {
    fn from_le_byte_slice(b: &[u8]) -> Self {
        let mut dst: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        dst.copy_from_slice(b);
        u32::from_le_bytes(dst)
    }
}
impl FromLEByteSlice for u16 {
    fn from_le_byte_slice(b: &[u8]) -> Self {
        let mut dst: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        dst.copy_from_slice(b);
        u16::from_le_bytes(dst)
    }
}
pub trait FromBEByteSlice {
    fn from_be_byte_slice(b: &[u8]) -> Self;
}
impl FromBEByteSlice for u32 {
    fn from_be_byte_slice(b: &[u8]) -> Self {
        let mut dst: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        dst.copy_from_slice(b);
        u32::from_be_bytes(dst)
    }
}
impl FromBEByteSlice for u16 {
    fn from_be_byte_slice(b: &[u8]) -> Self {
        let mut dst: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        dst.copy_from_slice(b);
        u16::from_be_bytes(dst)
    }
}

pub mod dpx_agl;
pub mod dpx_bmpimage;
pub mod dpx_cff;
pub mod dpx_cff_dict;
pub mod dpx_cid;
pub mod dpx_cidtype0;
pub mod dpx_cidtype2;
pub mod dpx_cmap;
pub mod dpx_cmap_read;
pub mod dpx_cmap_write;
pub mod dpx_cs_type2;
pub mod dpx_dpxconf;
pub mod dpx_dpxcrypt;
pub mod dpx_dpxfile;
pub mod dpx_dpxutil;
pub mod dpx_dvi;
pub mod dpx_dvipdfmx;
pub mod dpx_epdf;
pub mod dpx_error;
pub mod dpx_fontmap;
pub mod dpx_jp2image;
pub mod dpx_jpegimage;
pub mod dpx_mem;
pub mod dpx_mfileio;
pub mod dpx_mpost;
pub mod dpx_numbers;
pub mod dpx_otl_conf;
pub mod dpx_otl_opt;
pub mod dpx_pdfcolor;
pub mod dpx_pdfdev;
pub mod dpx_pdfdoc;
pub mod dpx_pdfdraw;
pub mod dpx_pdfencoding;
pub mod dpx_pdfencrypt;
pub mod dpx_pdffont;
pub mod dpx_pdfnames;
pub mod dpx_pdfobj;
pub mod dpx_pdfparse;
pub mod dpx_pdfresource;
pub mod dpx_pdfximage;
pub mod dpx_pkfont;
pub mod dpx_pngimage;
pub mod dpx_pst;
pub mod dpx_pst_obj;
pub mod dpx_sfnt;
pub mod dpx_subfont;
pub mod dpx_t1_char;
pub mod dpx_t1_load;
pub mod dpx_tfm;
pub mod dpx_truetype;
pub mod dpx_tt_aux;
pub mod dpx_tt_cmap;
pub mod dpx_tt_glyf;
pub mod dpx_tt_gsub;
pub mod dpx_tt_post;
pub mod dpx_tt_table;
pub mod dpx_type0;
pub mod dpx_type1;
pub mod dpx_type1c;
pub mod dpx_unicode;
pub mod dpx_vf;
mod shims;
pub mod specials;

pub use crate::dpx_dvipdfmx::dvipdfmx_main;

pub type __ComparFn =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> i32>;
extern "C" {
    #[no_mangle]
    fn qsort(__base: *mut libc::c_void, __nmemb: size_t, __size: size_t, __compar: __ComparFn);
    #[no_mangle]
    fn strtoll(_: *const i8, _: *mut *mut i8, _: i32) -> libc::c_longlong;
    #[no_mangle]
    fn modf(_: f64, _: *mut f64) -> f64;
}
