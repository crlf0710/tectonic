#![feature(extern_types)]
#![feature(ptr_wrapping_offset_from)]
#![feature(c_variadic)]
#![feature(const_transmute)]
#![allow(unused_unsafe)]
#![deny(unused_assignments)]
#![deny(clippy::reverse_range_loop)]

extern crate tectonic_bridge as bridge;

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
            write!(unsafe { _dpx_message_handle.as_mut().unwrap() }, $($arg)*).unwrap();
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
            let v = format!(concat!($fmt, "\x00"), $($arg)*);
            unsafe{crate::bridge::ttstub_issue_warning_slice(v.as_bytes());}
            unsafe{_last_message_type = DPX_MESG_WARN;}
        }
    }};
);

pub(crate) trait Warn<E>: Sized {}

fn isblank(c: libc::c_int) -> libc::c_int {
    (c == ' ' as _ || c == '\t' as _) as _
}

trait SkipBlank {
    fn skip_blank(&mut self);
}

impl SkipBlank for &[u8] {
    fn skip_blank(&mut self) {
        let mut i = 0;
        for &p in *self {
            if !(p & !0x7f == 0 && crate::isblank(p as _) != 0) {
                break;
            }
            i += 1;
        }
        *self = &self[i..];
    }
}

#[inline]
unsafe fn strstartswith(s: *const i8, prefix: *const i8) -> *const i8 {
    let length = libc::strlen(prefix);
    if libc::strncmp(s, prefix, length) == 0i32 {
        return s.offset(length as isize);
    }
    std::ptr::null()
}

#[inline]
unsafe fn streq_ptr(s1: *const i8, s2: *const i8) -> bool {
    if !s1.is_null() && !s2.is_null() {
        return libc::strcmp(s1, s2) == 0i32;
    }
    false
}

#[inline]
unsafe fn mfree(ptr: *mut libc::c_void) -> *mut libc::c_void {
    libc::free(ptr);
    std::ptr::null_mut()
}

use core::mem::MaybeUninit;
pub(crate) trait FromLEByteSlice {
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
pub(crate) trait FromBEByteSlice {
    fn from_be_byte_slice(b: &[u8]) -> Self;
}
impl FromBEByteSlice for i32 {
    fn from_be_byte_slice(b: &[u8]) -> Self {
        let mut dst: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        dst.copy_from_slice(b);
        i32::from_be_bytes(dst)
    }
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

pub(crate) mod dpx_agl;
pub(crate) mod dpx_bmpimage;
pub(crate) mod dpx_cff;
pub(crate) mod dpx_cff_dict;
pub(crate) mod dpx_cid;
pub(crate) mod dpx_cidtype0;
pub(crate) mod dpx_cidtype2;
pub(crate) mod dpx_cmap;
pub(crate) mod dpx_cmap_read;
pub(crate) mod dpx_cmap_write;
pub(crate) mod dpx_cs_type2;
pub(crate) mod dpx_dpxconf;
pub(crate) mod dpx_dpxcrypt;
pub(crate) mod dpx_dpxfile;
pub(crate) mod dpx_dpxutil;
pub(crate) mod dpx_dvi;
pub(crate) mod dpx_dvicodes;
pub(crate) mod dpx_dvipdfmx;
pub(crate) mod dpx_epdf;
pub(crate) mod dpx_error;
pub(crate) mod dpx_fontmap;
pub(crate) mod dpx_jp2image;
pub(crate) mod dpx_jpegimage;
pub(crate) mod dpx_mem;
pub(crate) mod dpx_mfileio;
pub(crate) mod dpx_mpost;
pub(crate) mod dpx_numbers;
pub(crate) mod dpx_otl_opt;
pub(crate) mod dpx_pdfcolor;
pub(crate) mod dpx_pdfdev;
pub(crate) mod dpx_pdfdoc;
pub(crate) mod dpx_pdfdraw;
pub(crate) mod dpx_pdfencoding;
pub(crate) mod dpx_pdfencrypt;
pub(crate) mod dpx_pdffont;
pub(crate) mod dpx_pdfnames;
pub(crate) mod dpx_pdfobj;
pub(crate) mod dpx_pdfparse;
pub(crate) mod dpx_pdfresource;
pub(crate) mod dpx_pdfximage;
pub(crate) mod dpx_pkfont;
pub(crate) mod dpx_pngimage;
pub(crate) mod dpx_pst;
pub(crate) mod dpx_pst_obj;
pub(crate) mod dpx_sfnt;
pub(crate) mod dpx_subfont;
pub(crate) mod dpx_t1_char;
pub(crate) mod dpx_t1_load;
pub(crate) mod dpx_tfm;
pub(crate) mod dpx_truetype;
pub(crate) mod dpx_tt_aux;
pub(crate) mod dpx_tt_cmap;
pub(crate) mod dpx_tt_glyf;
pub(crate) mod dpx_tt_gsub;
pub(crate) mod dpx_tt_post;
pub(crate) mod dpx_tt_table;
pub(crate) mod dpx_type0;
pub(crate) mod dpx_type1;
pub(crate) mod dpx_type1c;
pub(crate) mod dpx_unicode;
pub(crate) mod dpx_vf;
mod shims;
pub(crate) mod specials;

pub use crate::dpx_bmpimage::{bmp_get_bbox, check_for_bmp};
pub use crate::dpx_dvipdfmx::dvipdfmx_main;
pub use crate::dpx_jpegimage::{check_for_jpeg, jpeg_get_bbox};
pub use crate::dpx_pdfdev::Corner;
pub use crate::dpx_pdfdoc::{pdf_doc_get_page, pdf_doc_get_page_count};
pub use crate::dpx_pdfdraw::pdf_dev_transform;
pub use crate::dpx_pdfobj::{pdf_close, pdf_file, pdf_obj, pdf_open, pdf_release_obj};
pub use crate::dpx_pdfobj::{pdf_files_close, pdf_files_init};
pub use crate::dpx_pngimage::{check_for_png, png_get_bbox};
