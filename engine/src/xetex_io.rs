#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use bridge::{InFile, ReadByte, TTInputFormat};

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
    pub(crate) savedChar: Option<char>,
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
    if f.encodingMode as i32 == 5 && !f.conversionData.is_null() {
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
        savedChar: None,
        skipNextLF: 0,
        handle: Some(handle),
    });
    if mode == UnicodeMode::Auto {
        /* sniff encoding form */
        let handle = ufile.handle.as_mut().unwrap();
        let B1 = handle.read_byte();
        let B2 = handle.read_byte();
        if B1 == Some(0xfe) && B2 == Some(0xff) {
            mode = UnicodeMode::Utf16be;
        } else if B2 == Some(0xfe) && B1 == Some(0xff) {
            mode = UnicodeMode::Utf16le;
        } else if B1 == Some(0) && B2 != Some(0) {
            mode = UnicodeMode::Utf16be;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B2 == Some(0) && B1 != Some(0) {
            mode = UnicodeMode::Utf16le;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B1 == Some(0xef) && B2 == Some(0xbb) {
            let B3 = handle.read_byte();
            if B3 == Some(0xbf) {
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
    let normPtr =
        &mut *normalizers.as_mut_ptr().offset((norm - 1) as isize) as *mut teckit::TECkit_Converter;
    if (*normPtr).is_null() {
        let status = teckit::TECkit_CreateConverter(
            ptr::null_mut(),
            0,
            1,
            6,
            (6 | (if norm == 1 { 0x100 } else { 0x200 })) as u16,
            normPtr,
        );
        if status != 0 {
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
        1,
    );
    if status != 0 {
        buffer_overflow();
    }
    last = first + (outUsed as usize) / std::mem::size_of::<UnicodeScalar>();
}
impl UFILE {
    pub(crate) unsafe fn input_line(&mut self) -> bool {
        static mut byteBuffer: Vec<u8> = Vec::new();
        static mut utf32Buf: Vec<u32> = Vec::new();
        let norm = get_input_normalization_state();
        if self.handle.is_none() {
            /* NULL 'handle' means this: */
            panic!("reads from synthetic \"terminal\" file #0 should never happen");
        }
        last = first;
        let skip_lf = if self.encodingMode == UnicodeMode::ICUMapping {
            let mut errorCode: UErrorCode = U_ZERO_ERROR;
            if byteBuffer.is_empty() {
                byteBuffer = Vec::with_capacity(BUF_SIZE + 1);
            }
            byteBuffer.clear();
            /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
            let handle = self.handle.as_mut().unwrap();
            let mut i: Option<u8> = handle.read_byte();
            if self.skipNextLF != 0 {
                self.skipNextLF = 0_i16;
                if i == Some(b'\n') {
                    i = handle.read_byte();
                }
            }
            if let Some(i) = i.filter(|&i| i != b'\n' && i != b'\r') {
                byteBuffer.push(i);
            }
            if i.filter(|&i| i != b'\n' && i != b'\r').is_some() {
                while byteBuffer.len() < BUF_SIZE {
                    i = handle.read_byte();
                    if let Some(i) = i.filter(|&i| i != b'\n' && i != b'\r') {
                        byteBuffer.push(i);
                    } else {
                        break;
                    }
                }
            }
            if i.is_none() && byteBuffer.is_empty() {
                return false;
            }
            if i.filter(|&i| i != b'\n' && i != b'\r').is_some() {
                buffer_overflow();
            }
            /* now apply the mapping to turn external bytes into Unicode characters in buffer */
            let cnv = self.conversionData as *mut icu::UConverter;
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
                        byteBuffer.as_ptr() as *const i8,
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
                        byteBuffer.as_ptr() as *const i8,
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
            i == Some(b'\r')
        } else {
            /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
            let mut i: Option<char> = self.get_uni_c();
            if self.skipNextLF != 0 {
                self.skipNextLF = 0_i16;
                if i == Some('\n') {
                    i = self.get_uni_c()
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
                    if let Some(i) = i.filter(|&i| i != '\n' && i != '\r') {
                        utf32Buf.push(i as u32);
                    }
                    if i.filter(|&i| i != '\n' && i != '\r').is_some() {
                        while utf32Buf.len() < BUF_SIZE {
                            i = self.get_uni_c();
                            if let Some(i) = i.filter(|&i| i != '\n' && i != '\r') {
                                utf32Buf.push(i as u32);
                            } else {
                                break;
                            }
                        }
                    }
                    if i.is_none() && utf32Buf.is_empty() {
                        return false;
                    }
                    /* We didn't get the whole line because our buffer was too small.  */
                    if i.filter(|&i| i != '\n' && i != '\r').is_some() {
                        buffer_overflow();
                    }
                    apply_normalization(utf32Buf.as_mut_ptr(), utf32Buf.len() as _, norm);
                }
                _ => {
                    // none
                    if last < BUF_SIZE {
                        if let Some(i) = i.filter(|&i| i != '\n' && i != '\r') {
                            BUFFER[last as usize] = i as i32;
                            last += 1;
                        }
                    }
                    if i.filter(|&i| i != '\n' && i != '\r').is_some() {
                        while last < BUF_SIZE {
                            i = self.get_uni_c();
                            if let Some(i) = i.filter(|&i| i != '\n' && i != '\r') {
                                BUFFER[last as usize] = i as i32;
                                last += 1;
                            } else {
                                break;
                            }
                        }
                    }
                    if i.is_none() && last == first {
                        return false;
                    }
                    /* We didn't get the whole line because our buffer was too small.  */
                    if i.filter(|&i| i != '\n' && i != '\r').is_some() {
                        buffer_overflow();
                    }
                }
            }
            i == Some('\r')
        };
        /* If line ended with CR, remember to skip following LF. */
        if skip_lf {
            self.skipNextLF = 1_i16
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

impl UFILE {
    pub(crate) unsafe fn get_uni_c(&mut self) -> Option<char> {
        if let Some(rval) = self.savedChar {
            self.savedChar = None;
            return Some(rval);
        }
        let handle = self.handle.as_mut().unwrap();
        match self.encodingMode {
            UnicodeMode::Utf8 => {
                if let Some(val8) = handle.read_byte() {
                    let mut rval = val8 as i32;
                    let extraBytes = bytesFromUTF8[rval as usize] as u16;
                    match extraBytes {
                        0..=3 => {
                            for _ in 0..extraBytes {
                                if let Some(c) = handle.read_byte() {
                                    if c < 0x80 || c >= 0xC0 {
                                        handle.seek(SeekFrom::Current(-1)).unwrap();
                                        bad_utf8_warning();
                                        return Some('\u{fffd}'); /* return without adjusting by offsetsFromUTF8 */
                                    }
                                    rval <<= 6;
                                    rval += c as i32;
                                } else {
                                    bad_utf8_warning();
                                    return Some('\u{fffd}');
                                }
                            }
                        }
                        5 | 4 => {
                            bad_utf8_warning();
                            return Some('\u{fffd}'); /* return without adjusting by offsetsFromUTF8 */
                        }
                        _ => {}
                    }

                    rval -= offsetsFromUTF8[extraBytes as usize] as i32;

                    if rval < 0 || rval > 0x10ffff {
                        bad_utf8_warning();
                        return Some('\u{fffd}');
                    }
                    Some(std::char::from_u32_unchecked(rval as u32))
                } else {
                    None
                }
            }
            UnicodeMode::Utf16be => {
                if let Some(val8) = handle.read_byte() {
                    let mut rval = val8 as i32;
                    rval <<= 8;
                    rval += handle.read_byte().unwrap() as i32;
                    if rval >= 0xd800 && rval <= 0xdbff {
                        let mut lo = handle.read_byte().unwrap() as i32;
                        lo <<= 8;
                        lo += handle.read_byte().unwrap() as i32;
                        if lo >= 0xdc00 && lo <= 0xdfff {
                            rval = 0x10000 + (rval - 0xd800) * 0x400 + (lo - 0xdc00)
                        } else {
                            rval = 0xfffd;
                            self.savedChar = Some(std::char::from_u32_unchecked(lo as u32))
                        }
                    } else if rval >= 0xdc00 && rval <= 0xdfff {
                        rval = 0xfffd
                    }
                    Some(std::char::from_u32_unchecked(rval as u32))
                } else {
                    None
                }
            }
            UnicodeMode::Utf16le => {
                if let Some(val8) = handle.read_byte() {
                    let mut rval = val8 as i32;
                    rval += (handle.read_byte().unwrap() as i32) << 8;
                    if rval >= 0xd800 && rval <= 0xdbff {
                        let mut lo = handle.read_byte().unwrap() as i32;
                        lo += (handle.read_byte().unwrap() as i32) << 8;
                        if lo >= 0xdc00 && lo <= 0xdfff {
                            rval = 0x10000 + (rval - 0xd800) * 0x400 + (lo - 0xdc00)
                        } else {
                            rval = 0xfffd;
                            self.savedChar = Some(std::char::from_u32_unchecked(lo as u32));
                        }
                    } else if rval >= 0xdc00 && rval <= 0xdfff {
                        rval = 0xfffd
                    }
                    Some(std::char::from_u32_unchecked(rval as u32))
                } else {
                    None
                }
            }
            UnicodeMode::Raw => handle
                .read_byte()
                .map(|c| std::char::from_u32_unchecked(c as u32)),
            _ => {
                panic!("internal error; file input mode={:?}", self.encodingMode);
            }
        }
    }
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
