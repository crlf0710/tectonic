#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use bridge::{stub_errno as errno, ttstub_input_getc, InFile, TTInputFormat};

use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::xetex_consts::UnicodeMode;
use crate::xetex_ini::{
    first, input_state_t, last, max_buf_stack, name_in_progress, read_file, read_open,
    stop_at_space, BUFFER, BUF_SIZE,
};
use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetex0::{
    bad_utf8_warning, diagnostic, get_input_normalization_state, make_name, more_name,
    scan_file_name, scan_four_bit_int, scan_optional_equals,
};
use std::ffi::CString;
use std::io::{Seek, SeekFrom};

use crate::*;
pub(crate) type UErrorCode = i32;
pub(crate) const U_ZERO_ERROR: UErrorCode = 0;
/* quasi-hack to get the primary input */
/* 16.16 version number */
/* these are all predefined if using a Mac prefix */
/* NB: assumes int is 4 bytes */
/* n.b. if also using zlib.h, it must precede TECkit headers */
/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* Extra stuff used in various change files for various reasons.  */
/* Array allocations. Add 1 to size to account for Pascal indexing convention. */
/*11:*/
/*18: */
pub(crate) type UnicodeScalar = i32;

pub(crate) struct UFILE {
    pub(crate) handle: Option<InFile>,
    pub(crate) savedChar: i64,
    pub(crate) skipNextLF: i16,
    pub(crate) encodingMode: UnicodeMode,
    pub(crate) conversionData: *mut libc::c_void,
}

/* tectonic/xetex-io.c: low-level input/output functions tied to the XeTeX engine
   Copyright 2016-2019 The Tectonic Project
   Licensed under the MIT License.
*/
#[no_mangle]
pub(crate) static mut name_of_input_file: String = String::new();
pub(crate) unsafe fn tt_xetex_open_input(filename: &str, filefmt: TTInputFormat) -> Option<InFile> {
    let handle = if filefmt == TTInputFormat::TECTONIC_PRIMARY {
        InFile::open_primary()
    } else {
        InFile::open(&filename, filefmt as TTInputFormat, 0)
    };
    if handle.is_none() {
        None
    } else {
        name_of_input_file = filename.to_string();
        handle
    }
}
/* tables/values used in UTF-8 interpretation -
code is based on ConvertUTF.[ch] sample code
published by the Unicode consortium */
pub(crate) const offsetsFromUTF8: [u32; 6] = [
    0u64 as u32,
    0x3080u64 as u32,
    0xe2080u64 as u32,
    0x3c82080u64 as u32,
    0xfa082080u64 as u32,
    0x82082080u64 as u32,
];
pub(crate) const bytesFromUTF8: [u8; 256] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
];
pub(crate) unsafe fn set_input_file_encoding(f: &mut UFILE, mode: UnicodeMode, encodingData: i32) {
    if f.encodingMode as i32 == 5i32 && !f.conversionData.is_null() {
        icu::ucnv_close(f.conversionData as *mut icu::UConverter);
    }
    f.conversionData = ptr::null_mut();
    match mode {
        UnicodeMode::Utf8 | UnicodeMode::Utf16be | UnicodeMode::Utf16le | UnicodeMode::Raw => {
            f.encodingMode = mode
        }
        UnicodeMode::ICUMapping => {
            let name = gettexstring(encodingData);
            let mut err: UErrorCode = U_ZERO_ERROR;
            let cname = CString::new(name.as_str()).unwrap();
            let cnv = icu::ucnv_open(cname.as_ptr(), &mut err);
            if cnv.is_null() {
                diagnostic(true, || {
                    t_print_nl!(
                        "Error {} creating Unicode converter for `{}\'; reading as raw bytes",
                        err as i32,
                        name
                    );
                });
                f.encodingMode = UnicodeMode::Raw;
            } else {
                f.encodingMode = UnicodeMode::ICUMapping;
                f.conversionData = cnv as *mut libc::c_void
            }
        }
        _ => {}
    };
}
pub(crate) unsafe fn u_open_in(
    filename: &str,
    filefmt: TTInputFormat,
    mut _fopen_mode: &[u8],
    mut mode: UnicodeMode,
    encodingData: i32,
) -> Option<Box<UFILE>> {
    let handle = tt_xetex_open_input(filename, filefmt)?;
    let mut ufile = Box::new(UFILE {
        encodingMode: UnicodeMode::Auto,
        conversionData: ptr::null_mut(),
        savedChar: -1,
        skipNextLF: 0,
        handle: Some(handle),
    });
    if mode == UnicodeMode::Auto {
        /* sniff encoding form */
        let handle = ufile.handle.as_mut().unwrap();
        let B1 = ttstub_input_getc(handle);
        let B2 = ttstub_input_getc(handle);
        if B1 == 0xfe && B2 == 0xff {
            mode = UnicodeMode::Utf16be;
        } else if B2 == 0xfe && B1 == 0xff {
            mode = UnicodeMode::Utf16le;
        } else if B1 == 0 && B2 != 0 {
            mode = UnicodeMode::Utf16be;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B2 == 0 && B1 != 0 {
            mode = UnicodeMode::Utf16le;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B1 == 0xef && B2 == 0xbb {
            let B3 = ttstub_input_getc(handle);
            if B3 == 0xbf {
                mode = UnicodeMode::Utf8;
            }
        }
        if mode == UnicodeMode::Auto {
            handle.seek(SeekFrom::Start(0)).unwrap();
            mode = UnicodeMode::Utf8;
        }
    }
    set_input_file_encoding(&mut ufile, mode, encodingData);
    Some(ufile)
}
unsafe fn buffer_overflow() {
    panic!("unable to read an entire line (buf_size={})", BUF_SIZE,);
}
unsafe fn conversion_error(errcode: i32) {
    diagnostic(true, || {
        t_print_nl!(
            "Unicode conversion failed (ICU error code = {}) discarding any remaining text",
            errcode
        );
    });
}
unsafe fn apply_normalization(buf: *mut u32, len: i32, norm: i32) {
    static mut normalizers: [teckit::TECkit_Converter; 2] =
        [0 as teckit::TECkit_Converter, 0 as teckit::TECkit_Converter];
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    let normPtr = &mut *normalizers.as_mut_ptr().offset((norm - 1i32) as isize)
        as *mut teckit::TECkit_Converter;
    if (*normPtr).is_null() {
        let status = teckit::TECkit_CreateConverter(
            ptr::null_mut(),
            0i32 as u32,
            1i32 as u8,
            6i32 as u16,
            (6i32 | (if norm == 1i32 { 0x100i32 } else { 0x200i32 })) as u16,
            normPtr,
        );
        if status != 0i32 as i64 {
            panic!(
                "failed to create normalizer: error code = {}",
                status as i32,
            );
        }
    }
    let status = teckit::TECkit_ConvertBuffer(
        *normPtr,
        buf as *mut u8,
        (len as u64).wrapping_mul(::std::mem::size_of::<u32>() as _) as u32,
        &mut inUsed,
        &mut BUFFER[first as usize] as *mut UnicodeScalar as *mut u8,
        (std::mem::size_of::<UnicodeScalar>() * (BUF_SIZE - first)) as u32,
        &mut outUsed,
        1i32 as u8,
    );
    if status != 0i32 as i64 {
        buffer_overflow();
    }
    last = first + (outUsed as usize) / std::mem::size_of::<UnicodeScalar>();
}
pub(crate) unsafe fn input_line(f: &mut UFILE) -> bool {
    static mut byteBuffer: Vec<i8> = Vec::new();
    static mut utf32Buf: Vec<u32> = Vec::new();
    let mut i;
    let norm = get_input_normalization_state();
    if f.handle.is_none() {
        /* NULL 'handle' means this: */
        panic!("reads from synthetic \"terminal\" file #0 should never happen");
    }
    last = first;
    if f.encodingMode == UnicodeMode::ICUMapping {
        let mut errorCode: UErrorCode = U_ZERO_ERROR;
        if byteBuffer.is_empty() {
            byteBuffer = Vec::with_capacity(BUF_SIZE + 1);
        }
        byteBuffer.clear();
        /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
        let handle = f.handle.as_mut().unwrap();
        i = ttstub_input_getc(handle);
        if f.skipNextLF != 0 {
            f.skipNextLF = 0_i16;
            if i == '\n' as i32 {
                i = ttstub_input_getc(handle)
            }
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            byteBuffer.push(i as i8);
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            while byteBuffer.len() < BUF_SIZE
                && {
                    i = ttstub_input_getc(handle);
                    i != -1i32
                }
                && i != '\n' as i32
                && i != '\r' as i32
            {
                byteBuffer.push(i as i8);
            }
        }
        if i == -1i32 && errno::errno() != errno::EINTR && byteBuffer.is_empty() {
            return false;
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            buffer_overflow();
        }
        /* now apply the mapping to turn external bytes into Unicode characters in buffer */
        let cnv = f.conversionData as *mut icu::UConverter;
        match norm {
            1 | 2 => {
                // NFC
                // NFD
                if utf32Buf.is_empty() {
                    utf32Buf = vec![0; BUF_SIZE];
                } // sets 'last' correctly
                let tmpLen = icu::ucnv_toAlgorithmic(
                    icu::UCNV_UTF32_LittleEndian,
                    cnv,
                    utf32Buf.as_ptr() as *mut i8,
                    (BUF_SIZE as u64).wrapping_mul(::std::mem::size_of::<u32>() as _) as i32,
                    byteBuffer.as_ptr(),
                    byteBuffer.len() as i32,
                    &mut errorCode,
                );
                if errorCode != 0 {
                    conversion_error(errorCode as i32);
                    return false;
                }
                apply_normalization(
                    utf32Buf.as_mut_ptr(),
                    (tmpLen as u64).wrapping_div(::std::mem::size_of::<u32>() as _) as i32,
                    norm,
                );
            }
            _ => {
                // none
                let mut outLen = icu::ucnv_toAlgorithmic(
                    icu::UCNV_UTF32_LittleEndian,
                    cnv,
                    &mut BUFFER[first as usize] as *mut UnicodeScalar as *mut i8,
                    (std::mem::size_of::<UnicodeScalar>() * (BUF_SIZE - first)) as i32,
                    byteBuffer.as_ptr(),
                    byteBuffer.len() as i32,
                    &mut errorCode,
                ) as usize;
                if errorCode != 0 {
                    conversion_error(errorCode as i32);
                    return false;
                }
                outLen /= std::mem::size_of::<UnicodeScalar>();
                last = first + outLen
            }
        }
    } else {
        /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
        i = get_uni_c(f);
        if f.skipNextLF != 0 {
            f.skipNextLF = 0_i16;
            if i == '\n' as i32 {
                i = get_uni_c(f)
            }
        }
        match norm {
            1 | 2 => {
                // NFC
                // NFD
                // read Unicode chars into utf32Buf as UTF32
                if utf32Buf.is_empty() {
                    utf32Buf = Vec::with_capacity(BUF_SIZE);
                }
                utf32Buf.clear();
                if i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    utf32Buf.push(i as u32);
                }
                if i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    while utf32Buf.len() < BUF_SIZE
                        && {
                            i = get_uni_c(f);
                            i != -1
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        utf32Buf.push(i as u32);
                    }
                }
                if i == -1 && errno::errno() != errno::EINTR && utf32Buf.is_empty() {
                    return false;
                }
                /* We didn't get the whole line because our buffer was too small.  */
                if i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    buffer_overflow();
                }
                apply_normalization(utf32Buf.as_mut_ptr(), utf32Buf.len() as _, norm);
            }
            _ => {
                // none
                if last < BUF_SIZE && i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    BUFFER[last as usize] = i;
                    last += 1;
                }
                if i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    while last < BUF_SIZE
                        && {
                            i = get_uni_c(f);
                            i != -1
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        BUFFER[last as usize] = i;
                        last += 1;
                    }
                }
                if i == -1 && errno::errno() != errno::EINTR && last == first {
                    return false;
                }
                /* We didn't get the whole line because our buffer was too small.  */
                if i != -1 && i != '\n' as i32 && i != '\r' as i32 {
                    buffer_overflow();
                }
            }
        }
    }
    /* If line ended with CR, remember to skip following LF. */
    if i == '\r' as i32 {
        f.skipNextLF = 1_i16
    }
    BUFFER[last as usize] = ' ' as i32;
    if last >= max_buf_stack {
        max_buf_stack = last
    }
    /* Trim trailing space or EOL characters.  */
    while last > first
        && (BUFFER[(last - 1) as usize] == ' ' as i32
            || BUFFER[(last - 1) as usize] == '\r' as i32
            || BUFFER[(last - 1) as usize] == '\n' as i32)
    {
        last -= 1
    }
    true
}
impl Drop for UFILE {
    fn drop(&mut self) {
        if self.encodingMode == UnicodeMode::ICUMapping && !self.conversionData.is_null() {
            unsafe {
                icu::ucnv_close(self.conversionData as *mut icu::UConverter);
            }
        }
    }
}

pub(crate) unsafe fn get_uni_c(f: &mut UFILE) -> i32 {
    if f.savedChar != -1 {
        let rval = f.savedChar as i32;
        f.savedChar = -1;
        return rval;
    }
    let handle = f.handle.as_mut().unwrap();
    let mut rval;
    match f.encodingMode {
        UnicodeMode::Utf8 => {
            rval = ttstub_input_getc(handle);
            //c = rval;
            if rval != -1 {
                let extraBytes = bytesFromUTF8[rval as usize] as u16;
                match extraBytes {
                    0..=3 => {
                        for _ in 0..extraBytes {
                            let c = ttstub_input_getc(handle);
                            if c < 0x80 || c >= 0xC0 {
                                if c != -1 {
                                    handle.seek(SeekFrom::Current(-1)).unwrap();
                                }
                                bad_utf8_warning();
                                return 0xFFFD; /* return without adjusting by offsetsFromUTF8 */
                            }
                            rval <<= 6;
                            rval += c;
                        }
                    }
                    5 | 4 => {
                        bad_utf8_warning();
                        return 0xFFFD; /* return without adjusting by offsetsFromUTF8 */
                    }
                    _ => {}
                }

                rval -= offsetsFromUTF8[extraBytes as usize] as i32;

                if rval < 0 || rval > 0x10ffff {
                    bad_utf8_warning();
                    return 0xfffd;
                }
            }
        }
        UnicodeMode::Utf16be => {
            rval = ttstub_input_getc(handle);
            if rval != -1 {
                rval <<= 8;
                rval += ttstub_input_getc(handle);
                if rval >= 0xd800 && rval <= 0xdbff {
                    let mut lo: i32 = ttstub_input_getc(handle);
                    lo <<= 8;
                    lo += ttstub_input_getc(handle);
                    if lo >= 0xdc00 && lo <= 0xdfff {
                        rval = 0x10000 + (rval - 0xd800) * 0x400 + (lo - 0xdc00)
                    } else {
                        rval = 0xfffd;
                        f.savedChar = lo as i64
                    }
                } else if rval >= 0xdc00 && rval <= 0xdfff {
                    rval = 0xfffd
                }
            }
        }
        UnicodeMode::Utf16le => {
            rval = ttstub_input_getc(handle);
            if rval != -1 {
                rval += ttstub_input_getc(handle) << 8;
                if rval >= 0xd800 && rval <= 0xdbff {
                    let mut lo: i32 = ttstub_input_getc(handle);
                    lo += ttstub_input_getc(handle) << 8;
                    if lo >= 0xdc00 && lo <= 0xdfff {
                        rval = 0x10000 + (rval - 0xd800) * 0x400 + (lo - 0xdc00)
                    } else {
                        rval = 0xfffd;
                        f.savedChar = lo as i64
                    }
                } else if rval >= 0xdc00 && rval <= 0xdfff {
                    rval = 0xfffd
                }
            }
        }
        UnicodeMode::Raw => rval = ttstub_input_getc(handle),
        _ => {
            panic!("internal error; file input mode={:?}", f.encodingMode,);
        }
    }
    rval
}
pub(crate) unsafe fn open_or_close_in(input: &mut input_state_t, chr: i32) {
    use xetex_consts::*;
    let c = chr as u8;
    let n = scan_four_bit_int(input) as u8;
    if read_open[n as usize] != OpenMode::Closed {
        let _ = read_file[n as usize].take();
        read_open[n as usize] = OpenMode::Closed;
    }
    if c != 0 {
        scan_optional_equals(input);
        let filename = scan_file_name(input).0.to_string();
        let ufile = u_open_in(
            &filename,
            TTInputFormat::TEX,
            b"rb",
            UnicodeMode::from(get_int_par(IntPar::xetex_default_input_mode)),
            get_int_par(IntPar::xetex_default_input_encoding),
        );
        if ufile.is_some() {
            read_file[n as usize] = ufile;
            name_in_progress = true;
            make_name(|a, e, q, qc| {
                for k in filename.encode_utf16() {
                    if !more_name(k, false, a, e, q, qc) {
                        break;
                    }
                }
                stop_at_space = true;
            });
            name_in_progress = false;
            read_open[n as usize] = OpenMode::JustOpen;
        }
    };
}
