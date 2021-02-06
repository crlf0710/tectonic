#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::type_complexity)]

use std::ffi::CString;
use std::io::SeekFrom;
use std::io::{prelude::*, Result};
use std::ptr::NonNull;

pub type size_t = usize;
pub type ssize_t = isize;

type rust_output_handle_t = *mut libc::c_void;
pub type rust_input_handle_t = *mut libc::c_void;

#[derive(PartialEq)]
#[repr(transparent)]
pub struct OutputHandleWrapper(NonNull<libc::c_void>);

impl OutputHandleWrapper {
    pub(crate) fn new(ptr: rust_output_handle_t) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }
    pub fn as_ptr(&self) -> rust_output_handle_t {
        self.0.as_ptr()
    }
    pub unsafe fn open(path: &str, mut is_gz: i32) -> Option<Self> {
        let cpath = CString::new(path).unwrap();
        ttstub_output_open(cpath.as_ptr(), is_gz)
    }
}

impl Write for OutputHandleWrapper {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        unsafe {
            Ok(ttstub_output_write(self.0.as_ptr(), buf.as_ptr() as *const i8, buf.len()) as usize)
        }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe {
            ttstub_output_flush(self.0.as_ptr());
        }
        Ok(())
    }
}

impl Write for &OutputHandleWrapper {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        unsafe {
            Ok(ttstub_output_write(self.0.as_ptr(), buf.as_ptr() as *const i8, buf.len()) as usize)
        }
    }

    fn flush(&mut self) -> Result<()> {
        unsafe {
            ttstub_output_flush(self.0.as_ptr());
        }
        Ok(())
    }
}

#[derive(PartialEq)]
#[repr(transparent)]
pub struct InFile(NonNull<libc::c_void>);

impl Drop for InFile {
    fn drop(&mut self) {
        unsafe {
            if (*tectonic_global_bridge)
                .input_close
                .expect("non-null function pointer")(
                (*tectonic_global_bridge).context,
                self.0.as_ptr(),
            ) != 0
            {
                // Nonzero return value indicates a serious internal error.
                panic!("ttstub_input_close");
            }
        }
    }
}

impl InFile {
    pub unsafe fn from_raw(ptr: rust_input_handle_t) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub fn as_ptr(&self) -> rust_input_handle_t {
        self.0.as_ptr()
    }

    pub fn open(path: &str, format: TTInputFormat, is_gz: i32) -> Option<Self> {
        let path = CString::new(path).unwrap();
        unsafe {
            Self::from_raw((*tectonic_global_bridge)
                .input_open
                .expect("non-null function pointer")(
                (*tectonic_global_bridge).context,
                path.as_ptr(),
                format,
                is_gz,
            ))
        }
    }

    pub fn open_primary() -> Option<Self> {
        unsafe {
            Self::from_raw((*tectonic_global_bridge)
                .input_open_primary
                .expect("non-null function pointer")(
                (*tectonic_global_bridge).context
            ))
        }
    }
}

impl Read for InFile {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        unsafe {
            let res = ttstub_input_read_rust_style(
                self.0.as_ptr(),
                buf.as_mut_ptr() as *mut i8,
                buf.len(),
            );
            res.ok_or_else(|| std::io::ErrorKind::UnexpectedEof.into())
        }
    }
}

impl Read for &InFile {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        unsafe {
            let res = ttstub_input_read_rust_style(
                self.0.as_ptr(),
                buf.as_mut_ptr() as *mut i8,
                buf.len(),
            );
            res.ok_or_else(|| std::io::ErrorKind::UnexpectedEof.into())
        }
    }
}

impl Seek for InFile {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        use libc::{SEEK_CUR, SEEK_END, SEEK_SET};
        let (offset, whence) = match pos {
            SeekFrom::Start(o) => (o as ssize_t, SEEK_SET),
            SeekFrom::Current(o) => (o as ssize_t, SEEK_CUR),
            SeekFrom::End(o) => (o as ssize_t, SEEK_END),
        };
        unsafe { Ok(ttstub_input_seek(self.0.as_ptr(), offset, whence) as u64) }
    }
}

impl Seek for &InFile {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        use libc::{SEEK_CUR, SEEK_END, SEEK_SET};
        let (offset, whence) = match pos {
            SeekFrom::Start(o) => (o as ssize_t, SEEK_SET),
            SeekFrom::Current(o) => (o as ssize_t, SEEK_CUR),
            SeekFrom::End(o) => (o as ssize_t, SEEK_END),
        };
        unsafe { Ok(ttstub_input_seek(self.0.as_ptr(), offset, whence) as u64) }
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct tt_bridge_api_t {
    pub context: *mut libc::c_void,
    pub issue_warning: Option<unsafe fn(_: *mut libc::c_void, _: *const i8) -> ()>,
    pub issue_error: Option<unsafe fn(_: *mut libc::c_void, _: *const i8) -> ()>,
    pub get_file_md5: Option<unsafe fn(_: *mut libc::c_void, _: *const i8, _: *mut i8) -> i32>,
    pub get_data_md5:
        Option<unsafe fn(_: *mut libc::c_void, _: *const i8, _: size_t, _: *mut i8) -> i32>,
    pub output_open:
        Option<unsafe fn(_: *mut libc::c_void, _: *const i8, _: i32) -> rust_output_handle_t>,
    pub output_open_stdout: Option<unsafe fn(_: *mut libc::c_void) -> rust_output_handle_t>,
    pub output_putc:
        Option<unsafe fn(_: *mut libc::c_void, _: rust_output_handle_t, _: i32) -> i32>,
    pub output_write: Option<
        unsafe fn(_: *mut libc::c_void, _: rust_output_handle_t, _: *const i8, _: size_t) -> size_t,
    >,
    pub output_flush: Option<unsafe fn(_: *mut libc::c_void, _: rust_output_handle_t) -> i32>,
    pub output_close: Option<unsafe fn(_: *mut libc::c_void, _: rust_output_handle_t) -> i32>,
    pub input_open: Option<
        unsafe fn(
            _: *mut libc::c_void,
            _: *const i8,
            _: TTInputFormat,
            _: i32,
        ) -> rust_input_handle_t,
    >,
    pub input_open_primary: Option<unsafe fn(_: *mut libc::c_void) -> rust_input_handle_t>,
    pub input_get_size: Option<unsafe fn(_: *mut libc::c_void, _: rust_input_handle_t) -> size_t>,
    pub input_seek: Option<
        unsafe fn(
            _: *mut libc::c_void,
            _: rust_input_handle_t,
            _: ssize_t,
            _: i32,
            _: *mut i32,
        ) -> size_t,
    >,
    pub input_read: Option<
        unsafe fn(
            _: *mut libc::c_void,
            _: rust_input_handle_t,
            _: *mut i8,
            _: size_t,
        ) -> Option<usize>,
    >,
    pub input_close: Option<unsafe fn(_: *mut libc::c_void, _: rust_input_handle_t) -> i32>,
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub enum TTHistory {
    SPOTLESS = 0,
    WARNING_ISSUED = 1,
    ERROR_ISSUED = 2,
    FATAL_ERROR = 3,
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub enum TTInputFormat {
    PK = 1,
    TFM = 3,
    AFM = 4,
    BIB = 6,
    BST = 7,
    CNF = 8,
    FORMAT = 10,
    FONTMAP = 11,
    OFM = 20,
    OVF = 23,
    PICT = 25,
    TEX = 26,
    TEX_PS_HEADER = 30,
    TYPE1 = 32,
    VF = 33,
    TRUETYPE = 36,
    BINARY = 40,
    MISCFONTS = 41,
    ENC = 44,
    CMAP = 45,
    SFD = 46,
    OPENTYPE = 47,
    TECTONIC_PRIMARY = 59, /* quasi-hack to get the primary input */
}

static mut tectonic_global_bridge: *const tt_bridge_api_t = std::ptr::null();

pub unsafe fn tt_with_bridge<F, T>(bridge: *const tt_bridge_api_t, f: F) -> Option<T>
where
    F: std::panic::UnwindSafe + std::ops::FnOnce() -> T,
{
    use std::panic;
    use std::ptr::null;
    tectonic_global_bridge = bridge;
    let r = panic::catch_unwind(|| (f)()).ok();
    tectonic_global_bridge = null();
    r
}

pub(crate) unsafe fn tt_get_current_bridge() -> Option<&'static tt_bridge_api_t> {
    tectonic_global_bridge.as_ref()
}

/* Global symbols that route through the global API variable. Hopefully we
 * will one day eliminate all of the global state and get rid of all of
 * these. */
/* Global symbols that route through the global API */
pub unsafe fn ttstub_issue_warning_slice(buf: &[u8]) {
    (*tectonic_global_bridge)
        .issue_warning
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        buf.as_ptr() as *const i8,
    );
}

pub unsafe fn ttstub_issue_warning(buf: &str) {
    let buf = std::ffi::CString::new(buf).unwrap();
    (*tectonic_global_bridge)
        .issue_warning
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        buf.as_ptr() as *const i8,
    );
}

pub unsafe fn ttstub_issue_error(buf: &str) {
    let buf = std::ffi::CString::new(buf).unwrap();
    (*tectonic_global_bridge)
        .issue_error
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        buf.as_ptr() as *const i8,
    );
}

pub unsafe fn ttstub_get_file_md5(mut path: *const i8, mut digest: *mut i8) -> i32 {
    (*tectonic_global_bridge)
        .get_file_md5
        .expect("non-null function pointer")((*tectonic_global_bridge).context, path, digest)
}

pub unsafe fn ttstub_output_open(
    mut path: *const i8,
    mut is_gz: i32,
) -> Option<OutputHandleWrapper> {
    OutputHandleWrapper::new((*tectonic_global_bridge)
        .output_open
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        path,
        is_gz,
    ))
}

pub unsafe fn ttstub_output_open_stdout() -> Option<OutputHandleWrapper> {
    OutputHandleWrapper::new((*tectonic_global_bridge)
        .output_open_stdout
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context
    ))
}

pub unsafe fn ttstub_output_putc(handle: &mut OutputHandleWrapper, mut c: i32) -> i32 {
    (*tectonic_global_bridge)
        .output_putc
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        handle.0.as_ptr(),
        c,
    )
}

pub(crate) unsafe fn ttstub_output_write(
    mut handle: rust_output_handle_t,
    mut data: *const i8,
    mut len: size_t,
) -> size_t {
    (*tectonic_global_bridge)
        .output_write
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context, handle, data, len
    )
}

pub(crate) unsafe fn ttstub_output_flush(mut handle: rust_output_handle_t) -> i32 {
    (*tectonic_global_bridge)
        .output_flush
        .expect("non-null function pointer")((*tectonic_global_bridge).context, handle)
}

pub unsafe fn ttstub_output_close(mut handle: OutputHandleWrapper) -> i32 {
    (*tectonic_global_bridge)
        .output_close
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context, handle.0.as_ptr()
    )
}

pub fn ttstub_input_get_size<R: Seek>(handle: &mut R) -> size_t {
    fn stream_len<R: Seek>(handle: &mut R) -> Result<u64> {
        let old_pos = stream_position(handle)?;
        let len = handle.seek(SeekFrom::End(0))?;

        // Avoid seeking a third time when we were already at the end of the
        // stream. The branch is usually way cheaper than a seek operation.
        if old_pos != len {
            handle.seek(SeekFrom::Start(old_pos))?;
        }

        Ok(len)
    }
    fn stream_position<R: Seek>(handle: &mut R) -> Result<u64> {
        handle.seek(SeekFrom::Current(0))
    }

    stream_len(handle).unwrap() as usize
}

pub(crate) unsafe fn ttstub_input_seek(
    mut handle: rust_input_handle_t,
    mut offset: ssize_t,
    mut whence: i32,
) -> size_t {
    let mut internal_error: i32 = 0i32;
    let mut rv: size_t = (*tectonic_global_bridge)
        .input_seek
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context,
        handle,
        offset,
        whence,
        &mut internal_error,
    );
    if internal_error != 0 {
        // Nonzero indicates a serious internal error.
        panic!("ttstub_input_seek");
    }
    rv
}

pub unsafe fn ttstub_input_read_rust_style(
    mut handle: rust_input_handle_t,
    mut data: *mut i8,
    mut len: size_t,
) -> Option<usize> {
    (*tectonic_global_bridge)
        .input_read
        .expect("non-null function pointer")(
        (*tectonic_global_bridge).context, handle, data, len
    )
}

pub trait ReadByte {
    fn read_byte(&mut self) -> Option<u8>;
}
impl<R> ReadByte for R
where
    R: std::io::Read,
{
    fn read_byte(&mut self) -> Option<u8> {
        let mut buf = [0_u8];
        self.read_exact(&mut buf).ok()?;
        Some(buf[0])
    }
}

/* TODO: these are needed for the various *_main routines which should
 * probably be moved out into other files. */
/* The global variable that represents the Rust API. Some fine day we'll get
 * rid of all of the globals ... */
pub static mut error_buf: String = String::new();

#[macro_export]
macro_rules! abort(
    ($($arg:tt)*) => { unsafe {
        use std::io::Write;
        let v = format!($($arg)*);
        bridge::error_buf = v.clone();
        panic!(v);
    }};
);

pub unsafe fn tt_get_error_message<'a>() -> &'a str {
    &error_buf
}

#[macro_use]
pub(crate) mod macro_stub;
pub mod stub_errno;

pub trait DisplayExt {
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
        String::from_utf8_lossy(match self.iter().position(|&x| x == 0) {
            Some(n) => &self[..n],
            None => self,
        })
    }
}
