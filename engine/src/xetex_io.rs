#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use bridge::{stub_errno as errno, ttstub_input_getc, InFile, TTInputFormat};

use crate::core_memory::{xcalloc, xmalloc};
use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::xetex_consts::UnicodeMode;
use crate::xetex_ini::{
    cur_area, cur_ext, cur_name, first, input_state_t, last, max_buf_stack, name_in_progress,
    name_of_file, read_file, read_open, stop_at_space, BUFFER, BUF_SIZE,
};
use crate::xetex_output::{print_int, print_nl};
use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetex0::{
    bad_utf8_warning, diagnostic, get_input_normalization_state, make_name, more_name,
    pack_file_name, scan_file_name, scan_four_bit_int, scan_optional_equals,
};
use crate::xetex_xetexd::print_c_str;
use std::ffi::CString;
use std::io::{Seek, SeekFrom};

use crate::*;
pub(crate) type UErrorCode = i32;
pub(crate) const U_ERROR_LIMIT: UErrorCode = 66818;
pub(crate) const U_PLUGIN_ERROR_LIMIT: UErrorCode = 66818;
pub(crate) const U_PLUGIN_DIDNT_SET_LEVEL: UErrorCode = 66817;
pub(crate) const U_PLUGIN_TOO_HIGH: UErrorCode = 66816;
pub(crate) const U_PLUGIN_ERROR_START: UErrorCode = 66816;
pub(crate) const U_STRINGPREP_CHECK_BIDI_ERROR: UErrorCode = 66562;
pub(crate) const U_STRINGPREP_UNASSIGNED_ERROR: UErrorCode = 66561;
pub(crate) const U_STRINGPREP_PROHIBITED_ERROR: UErrorCode = 66560;
pub(crate) const U_IDNA_ERROR_LIMIT: UErrorCode = 66569;
pub(crate) const U_IDNA_DOMAIN_NAME_TOO_LONG_ERROR: UErrorCode = 66568;
pub(crate) const U_IDNA_ZERO_LENGTH_LABEL_ERROR: UErrorCode = 66567;
pub(crate) const U_IDNA_LABEL_TOO_LONG_ERROR: UErrorCode = 66566;
pub(crate) const U_IDNA_VERIFICATION_ERROR: UErrorCode = 66565;
pub(crate) const U_IDNA_ACE_PREFIX_ERROR: UErrorCode = 66564;
pub(crate) const U_IDNA_STD3_ASCII_RULES_ERROR: UErrorCode = 66563;
pub(crate) const U_IDNA_CHECK_BIDI_ERROR: UErrorCode = 66562;
pub(crate) const U_IDNA_UNASSIGNED_ERROR: UErrorCode = 66561;
pub(crate) const U_IDNA_ERROR_START: UErrorCode = 66560;
pub(crate) const U_IDNA_PROHIBITED_ERROR: UErrorCode = 66560;
pub(crate) const U_REGEX_ERROR_LIMIT: UErrorCode = 66326;
pub(crate) const U_REGEX_INVALID_CAPTURE_GROUP_NAME: UErrorCode = 66325;
pub(crate) const U_REGEX_PATTERN_TOO_BIG: UErrorCode = 66324;
pub(crate) const U_REGEX_STOPPED_BY_CALLER: UErrorCode = 66323;
pub(crate) const U_REGEX_TIME_OUT: UErrorCode = 66322;
pub(crate) const U_REGEX_STACK_OVERFLOW: UErrorCode = 66321;
pub(crate) const U_REGEX_INVALID_RANGE: UErrorCode = 66320;
pub(crate) const U_REGEX_MISSING_CLOSE_BRACKET: UErrorCode = 66319;
pub(crate) const U_REGEX_OCTAL_TOO_BIG: UErrorCode = 66318;
pub(crate) const U_REGEX_SET_CONTAINS_STRING: UErrorCode = 66317;
pub(crate) const U_REGEX_LOOK_BEHIND_LIMIT: UErrorCode = 66316;
pub(crate) const U_REGEX_INVALID_FLAG: UErrorCode = 66315;
pub(crate) const U_REGEX_INVALID_BACK_REF: UErrorCode = 66314;
pub(crate) const U_REGEX_MAX_LT_MIN: UErrorCode = 66313;
pub(crate) const U_REGEX_BAD_INTERVAL: UErrorCode = 66312;
pub(crate) const U_REGEX_NUMBER_TOO_BIG: UErrorCode = 66311;
pub(crate) const U_REGEX_MISMATCHED_PAREN: UErrorCode = 66310;
pub(crate) const U_REGEX_UNIMPLEMENTED: UErrorCode = 66309;
pub(crate) const U_REGEX_PROPERTY_SYNTAX: UErrorCode = 66308;
pub(crate) const U_REGEX_BAD_ESCAPE_SEQUENCE: UErrorCode = 66307;
pub(crate) const U_REGEX_INVALID_STATE: UErrorCode = 66306;
pub(crate) const U_REGEX_RULE_SYNTAX: UErrorCode = 66305;
pub(crate) const U_REGEX_ERROR_START: UErrorCode = 66304;
pub(crate) const U_REGEX_INTERNAL_ERROR: UErrorCode = 66304;
pub(crate) const U_BRK_ERROR_LIMIT: UErrorCode = 66062;
pub(crate) const U_BRK_MALFORMED_RULE_TAG: UErrorCode = 66061;
pub(crate) const U_BRK_UNRECOGNIZED_OPTION: UErrorCode = 66060;
pub(crate) const U_BRK_RULE_EMPTY_SET: UErrorCode = 66059;
pub(crate) const U_BRK_INIT_ERROR: UErrorCode = 66058;
pub(crate) const U_BRK_UNDEFINED_VARIABLE: UErrorCode = 66057;
pub(crate) const U_BRK_NEW_LINE_IN_QUOTED_STRING: UErrorCode = 66056;
pub(crate) const U_BRK_MISMATCHED_PAREN: UErrorCode = 66055;
pub(crate) const U_BRK_VARIABLE_REDFINITION: UErrorCode = 66054;
pub(crate) const U_BRK_ASSIGN_ERROR: UErrorCode = 66053;
pub(crate) const U_BRK_UNCLOSED_SET: UErrorCode = 66052;
pub(crate) const U_BRK_RULE_SYNTAX: UErrorCode = 66051;
pub(crate) const U_BRK_SEMICOLON_EXPECTED: UErrorCode = 66050;
pub(crate) const U_BRK_HEX_DIGITS_EXPECTED: UErrorCode = 66049;
pub(crate) const U_BRK_ERROR_START: UErrorCode = 66048;
pub(crate) const U_BRK_INTERNAL_ERROR: UErrorCode = 66048;
pub(crate) const U_FMT_PARSE_ERROR_LIMIT: UErrorCode = 65812;
pub(crate) const U_NUMBER_SKELETON_SYNTAX_ERROR: UErrorCode = 65811;
pub(crate) const U_NUMBER_ARG_OUTOFBOUNDS_ERROR: UErrorCode = 65810;
pub(crate) const U_FORMAT_INEXACT_ERROR: UErrorCode = 65809;
pub(crate) const U_DECIMAL_NUMBER_SYNTAX_ERROR: UErrorCode = 65808;
pub(crate) const U_DEFAULT_KEYWORD_MISSING: UErrorCode = 65807;
pub(crate) const U_UNDEFINED_KEYWORD: UErrorCode = 65806;
pub(crate) const U_DUPLICATE_KEYWORD: UErrorCode = 65805;
pub(crate) const U_ARGUMENT_TYPE_MISMATCH: UErrorCode = 65804;
pub(crate) const U_UNSUPPORTED_ATTRIBUTE: UErrorCode = 65803;
pub(crate) const U_UNSUPPORTED_PROPERTY: UErrorCode = 65802;
pub(crate) const U_UNMATCHED_BRACES: UErrorCode = 65801;
pub(crate) const U_ILLEGAL_PAD_POSITION: UErrorCode = 65800;
pub(crate) const U_PATTERN_SYNTAX_ERROR: UErrorCode = 65799;
pub(crate) const U_MULTIPLE_PAD_SPECIFIERS: UErrorCode = 65798;
pub(crate) const U_MULTIPLE_PERMILL_SYMBOLS: UErrorCode = 65797;
pub(crate) const U_MULTIPLE_PERCENT_SYMBOLS: UErrorCode = 65796;
pub(crate) const U_MALFORMED_EXPONENTIAL_PATTERN: UErrorCode = 65795;
pub(crate) const U_MULTIPLE_EXPONENTIAL_SYMBOLS: UErrorCode = 65794;
pub(crate) const U_MULTIPLE_DECIMAL_SEPERATORS: UErrorCode = 65793;
pub(crate) const U_MULTIPLE_DECIMAL_SEPARATORS: UErrorCode = 65793;
pub(crate) const U_FMT_PARSE_ERROR_START: UErrorCode = 65792;
pub(crate) const U_UNEXPECTED_TOKEN: UErrorCode = 65792;
pub(crate) const U_PARSE_ERROR_LIMIT: UErrorCode = 65571;
pub(crate) const U_INVALID_FUNCTION: UErrorCode = 65570;
pub(crate) const U_INVALID_ID: UErrorCode = 65569;
pub(crate) const U_INTERNAL_TRANSLITERATOR_ERROR: UErrorCode = 65568;
pub(crate) const U_ILLEGAL_CHARACTER: UErrorCode = 65567;
pub(crate) const U_VARIABLE_RANGE_OVERLAP: UErrorCode = 65566;
pub(crate) const U_VARIABLE_RANGE_EXHAUSTED: UErrorCode = 65565;
pub(crate) const U_ILLEGAL_CHAR_IN_SEGMENT: UErrorCode = 65564;
pub(crate) const U_UNCLOSED_SEGMENT: UErrorCode = 65563;
pub(crate) const U_MALFORMED_PRAGMA: UErrorCode = 65562;
pub(crate) const U_INVALID_PROPERTY_PATTERN: UErrorCode = 65561;
pub(crate) const U_INVALID_RBT_SYNTAX: UErrorCode = 65560;
pub(crate) const U_MULTIPLE_COMPOUND_FILTERS: UErrorCode = 65559;
pub(crate) const U_MISPLACED_COMPOUND_FILTER: UErrorCode = 65558;
pub(crate) const U_RULE_MASK_ERROR: UErrorCode = 65557;
pub(crate) const U_UNTERMINATED_QUOTE: UErrorCode = 65556;
pub(crate) const U_UNQUOTED_SPECIAL: UErrorCode = 65555;
pub(crate) const U_UNDEFINED_VARIABLE: UErrorCode = 65554;
pub(crate) const U_UNDEFINED_SEGMENT_REFERENCE: UErrorCode = 65553;
pub(crate) const U_TRAILING_BACKSLASH: UErrorCode = 65552;
pub(crate) const U_MULTIPLE_POST_CONTEXTS: UErrorCode = 65551;
pub(crate) const U_MULTIPLE_CURSORS: UErrorCode = 65550;
pub(crate) const U_MULTIPLE_ANTE_CONTEXTS: UErrorCode = 65549;
pub(crate) const U_MISSING_SEGMENT_CLOSE: UErrorCode = 65548;
pub(crate) const U_MISSING_OPERATOR: UErrorCode = 65547;
pub(crate) const U_MISPLACED_QUANTIFIER: UErrorCode = 65546;
pub(crate) const U_MISPLACED_CURSOR_OFFSET: UErrorCode = 65545;
pub(crate) const U_MISPLACED_ANCHOR_START: UErrorCode = 65544;
pub(crate) const U_MISMATCHED_SEGMENT_DELIMITERS: UErrorCode = 65543;
pub(crate) const U_MALFORMED_VARIABLE_REFERENCE: UErrorCode = 65542;
pub(crate) const U_MALFORMED_VARIABLE_DEFINITION: UErrorCode = 65541;
pub(crate) const U_MALFORMED_UNICODE_ESCAPE: UErrorCode = 65540;
pub(crate) const U_MALFORMED_SYMBOL_REFERENCE: UErrorCode = 65539;
pub(crate) const U_MALFORMED_SET: UErrorCode = 65538;
pub(crate) const U_MALFORMED_RULE: UErrorCode = 65537;
pub(crate) const U_PARSE_ERROR_START: UErrorCode = 65536;
pub(crate) const U_BAD_VARIABLE_DEFINITION: UErrorCode = 65536;
pub(crate) const U_STANDARD_ERROR_LIMIT: UErrorCode = 31;
pub(crate) const U_NO_WRITE_PERMISSION: UErrorCode = 30;
pub(crate) const U_USELESS_COLLATOR_ERROR: UErrorCode = 29;
pub(crate) const U_COLLATOR_VERSION_MISMATCH: UErrorCode = 28;
pub(crate) const U_INVALID_STATE_ERROR: UErrorCode = 27;
pub(crate) const U_INVARIANT_CONVERSION_ERROR: UErrorCode = 26;
pub(crate) const U_ENUM_OUT_OF_SYNC_ERROR: UErrorCode = 25;
pub(crate) const U_TOO_MANY_ALIASES_ERROR: UErrorCode = 24;
pub(crate) const U_STATE_TOO_OLD_ERROR: UErrorCode = 23;
pub(crate) const U_PRIMARY_TOO_LONG_ERROR: UErrorCode = 22;
pub(crate) const U_CE_NOT_FOUND_ERROR: UErrorCode = 21;
pub(crate) const U_NO_SPACE_AVAILABLE: UErrorCode = 20;
pub(crate) const U_UNSUPPORTED_ESCAPE_SEQUENCE: UErrorCode = 19;
pub(crate) const U_ILLEGAL_ESCAPE_SEQUENCE: UErrorCode = 18;
pub(crate) const U_RESOURCE_TYPE_MISMATCH: UErrorCode = 17;
pub(crate) const U_UNSUPPORTED_ERROR: UErrorCode = 16;
pub(crate) const U_BUFFER_OVERFLOW_ERROR: UErrorCode = 15;
pub(crate) const U_INVALID_TABLE_FILE: UErrorCode = 14;
pub(crate) const U_INVALID_TABLE_FORMAT: UErrorCode = 13;
pub(crate) const U_ILLEGAL_CHAR_FOUND: UErrorCode = 12;
pub(crate) const U_TRUNCATED_CHAR_FOUND: UErrorCode = 11;
pub(crate) const U_INVALID_CHAR_FOUND: UErrorCode = 10;
pub(crate) const U_PARSE_ERROR: UErrorCode = 9;
pub(crate) const U_INDEX_OUTOFBOUNDS_ERROR: UErrorCode = 8;
pub(crate) const U_MEMORY_ALLOCATION_ERROR: UErrorCode = 7;
pub(crate) const U_MESSAGE_PARSE_ERROR: UErrorCode = 6;
pub(crate) const U_INTERNAL_PROGRAM_ERROR: UErrorCode = 5;
pub(crate) const U_FILE_ACCESS_ERROR: UErrorCode = 4;
pub(crate) const U_INVALID_FORMAT_ERROR: UErrorCode = 3;
pub(crate) const U_MISSING_RESOURCE_ERROR: UErrorCode = 2;
pub(crate) const U_ILLEGAL_ARGUMENT_ERROR: UErrorCode = 1;
pub(crate) const U_ZERO_ERROR: UErrorCode = 0;
pub(crate) const U_ERROR_WARNING_LIMIT: UErrorCode = -119;
pub(crate) const U_PLUGIN_CHANGED_LEVEL_WARNING: UErrorCode = -120;
pub(crate) const U_DIFFERENT_UCA_VERSION: UErrorCode = -121;
pub(crate) const U_AMBIGUOUS_ALIAS_WARNING: UErrorCode = -122;
pub(crate) const U_SORT_KEY_TOO_SHORT_WARNING: UErrorCode = -123;
pub(crate) const U_STRING_NOT_TERMINATED_WARNING: UErrorCode = -124;
pub(crate) const U_STATE_OLD_WARNING: UErrorCode = -125;
pub(crate) const U_SAFECLONE_ALLOCATED_WARNING: UErrorCode = -126;
pub(crate) const U_USING_DEFAULT_WARNING: UErrorCode = -127;
pub(crate) const U_ERROR_WARNING_START: UErrorCode = -128;
pub(crate) const U_USING_FALLBACK_WARNING: UErrorCode = -128;
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
pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type str_number = i32;

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
pub(crate) unsafe fn tt_xetex_open_input(mut filefmt: TTInputFormat) -> Option<InFile> {
    let handle = if filefmt == TTInputFormat::TECTONIC_PRIMARY {
        InFile::open_primary()
    } else {
        InFile::open(&name_of_file, filefmt as TTInputFormat, 0)
    };
    if handle.is_none() {
        return None;
    }
    name_of_input_file = name_of_file.clone();
    handle
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
pub(crate) unsafe fn set_input_file_encoding(
    f: &mut UFILE,
    mode: UnicodeMode,
    mut encodingData: i32,
) {
    if f.encodingMode as i32 == 5i32 && !f.conversionData.is_null() {
        icu::ucnv_close(f.conversionData as *mut icu::UConverter);
    }
    f.conversionData = 0 as *mut libc::c_void;
    match mode {
        UnicodeMode::Utf8 | UnicodeMode::Utf16be | UnicodeMode::Utf16le | UnicodeMode::Raw => {
            f.encodingMode = mode
        }
        UnicodeMode::ICUMapping => {
            let mut name = gettexstring(encodingData);
            let mut err: UErrorCode = U_ZERO_ERROR;
            let mut cnv: *mut icu::UConverter =
                icu::ucnv_open(CString::new(name.as_str()).unwrap().as_ptr(), &mut err);
            if cnv.is_null() {
                diagnostic(true, || {
                    print_nl('E' as i32);
                    print_c_str("rror ");
                    print_int(err as i32);
                    print_c_str(" creating Unicode converter for `");
                    print_c_str(&name);
                    print_c_str("\'; reading as raw bytes");
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
    mut filefmt: TTInputFormat,
    mut _fopen_mode: &[u8],
    mut mode: UnicodeMode,
    mut encodingData: i32,
) -> Option<Box<UFILE>> {
    let mut B1: i32 = 0;
    let mut B2: i32 = 0;
    let handle = tt_xetex_open_input(filefmt);
    if handle.is_none() {
        return None;
    }
    let mut ufile = Box::new(UFILE {
        encodingMode: UnicodeMode::Auto,
        conversionData: 0 as *mut libc::c_void,
        savedChar: -1,
        skipNextLF: 0,
        handle,
    });
    if mode == UnicodeMode::Auto {
        /* sniff encoding form */
        let handle = ufile.handle.as_mut().unwrap();
        B1 = ttstub_input_getc(handle);
        B2 = ttstub_input_getc(handle);
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
            let mut B3: i32 = ttstub_input_getc(handle);
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
unsafe extern "C" fn buffer_overflow() {
    panic!("unable to read an entire line (buf_size={})", BUF_SIZE,);
}
unsafe extern "C" fn conversion_error(mut errcode: i32) {
    diagnostic(true, || {
        print_nl('U' as i32);
        print_c_str(&"nicode conversion failed (ICU error code = "[..]);
        print_int(errcode);
        print_c_str(&") discarding any remaining text"[..]);
    });
}
unsafe extern "C" fn apply_normalization(mut buf: *mut u32, mut len: i32, mut norm: i32) {
    static mut normalizers: [teckit::TECkit_Converter; 2] =
        [0 as teckit::TECkit_Converter, 0 as teckit::TECkit_Converter];
    let mut status: teckit::TECkit_Status = 0;
    let mut inUsed: u32 = 0;
    let mut outUsed: u32 = 0;
    let mut normPtr: *mut teckit::TECkit_Converter =
        &mut *normalizers.as_mut_ptr().offset((norm - 1i32) as isize)
            as *mut teckit::TECkit_Converter;
    if (*normPtr).is_null() {
        status = teckit::TECkit_CreateConverter(
            0 as *mut u8,
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
    status = teckit::TECkit_ConvertBuffer(
        *normPtr,
        buf as *mut u8,
        (len as u64).wrapping_mul(::std::mem::size_of::<u32>() as _) as u32,
        &mut inUsed,
        &mut BUFFER[first as usize] as *mut UnicodeScalar as *mut u8,
        (::std::mem::size_of::<UnicodeScalar>() as u64)
            .wrapping_mul((BUF_SIZE as i32 - first) as u64) as u32,
        &mut outUsed,
        1i32 as u8,
    );
    if status != 0i32 as i64 {
        buffer_overflow();
    }
    last = (first as u64)
        .wrapping_add((outUsed as u64).wrapping_div(::std::mem::size_of::<UnicodeScalar>() as u64))
        as i32;
}
pub(crate) unsafe fn input_line(f: &mut UFILE) -> bool {
    static mut byteBuffer: *mut i8 = ptr::null_mut();
    static mut utf32Buf: *mut u32 = ptr::null_mut();
    let mut i;
    let mut tmpLen: i32 = 0;
    let mut norm: i32 = get_input_normalization_state();
    if f.handle.is_none() {
        /* NULL 'handle' means this: */
        panic!("reads from synthetic \"terminal\" file #0 should never happen");
    }
    last = first;
    if f.encodingMode == UnicodeMode::ICUMapping {
        let mut bytesRead: u32 = 0_u32;
        let mut cnv: *mut icu::UConverter = 0 as *mut icu::UConverter;
        let mut outLen: i32 = 0;
        let mut errorCode: UErrorCode = U_ZERO_ERROR;
        if byteBuffer.is_null() {
            byteBuffer = xmalloc((BUF_SIZE + 1) as size_t) as *mut i8
        }
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
            let fresh1 = bytesRead;
            bytesRead = bytesRead.wrapping_add(1);
            *byteBuffer.offset(fresh1 as isize) = i as i8
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            while bytesRead < BUF_SIZE as u32
                && {
                    i = ttstub_input_getc(handle);
                    i != -1i32
                }
                && i != '\n' as i32
                && i != '\r' as i32
            {
                let fresh2 = bytesRead;
                bytesRead = bytesRead.wrapping_add(1);
                *byteBuffer.offset(fresh2 as isize) = i as i8
            }
        }
        if i == -1i32 && errno::errno() != errno::EINTR && bytesRead == 0_u32 {
            return false;
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            buffer_overflow();
        }
        /* now apply the mapping to turn external bytes into Unicode characters in buffer */
        cnv = f.conversionData as *mut icu::UConverter;
        match norm {
            1 | 2 => {
                // NFC
                // NFD
                if utf32Buf.is_null() {
                    utf32Buf =
                        xcalloc(BUF_SIZE as size_t, ::std::mem::size_of::<u32>() as _) as *mut u32
                } // sets 'last' correctly
                tmpLen = icu::ucnv_toAlgorithmic(
                    icu::UCNV_UTF32_LittleEndian,
                    cnv,
                    utf32Buf as *mut i8,
                    (BUF_SIZE as u64).wrapping_mul(::std::mem::size_of::<u32>() as _) as i32,
                    byteBuffer,
                    bytesRead as i32,
                    &mut errorCode,
                );
                if errorCode != 0 {
                    conversion_error(errorCode as i32);
                    return false;
                }
                apply_normalization(
                    utf32Buf,
                    (tmpLen as u64).wrapping_div(::std::mem::size_of::<u32>() as _) as i32,
                    norm,
                );
            }
            _ => {
                // none
                outLen = icu::ucnv_toAlgorithmic(
                    icu::UCNV_UTF32_LittleEndian,
                    cnv,
                    &mut BUFFER[first as usize] as *mut UnicodeScalar as *mut i8,
                    (::std::mem::size_of::<UnicodeScalar>() as u64)
                        .wrapping_mul((BUF_SIZE as i32 - first) as u64) as i32,
                    byteBuffer,
                    bytesRead as i32,
                    &mut errorCode,
                );
                if errorCode != 0 {
                    conversion_error(errorCode as i32);
                    return false;
                }
                outLen = (outLen as u64).wrapping_div(::std::mem::size_of::<UnicodeScalar>() as u64)
                    as i32 as i32;
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
                if utf32Buf.is_null() {
                    utf32Buf =
                        xcalloc(BUF_SIZE as size_t, ::std::mem::size_of::<u32>() as _) as *mut u32
                }
                tmpLen = 0i32;
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    *utf32Buf.offset(tmpLen as isize) = i as u32;
                    tmpLen += 1;
                }
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    while tmpLen < BUF_SIZE as i32
                        && {
                            i = get_uni_c(f);
                            i != -1i32
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        *utf32Buf.offset(tmpLen as isize) = i as u32;
                        tmpLen += 1;
                    }
                }
                if i == -1i32 && errno::errno() != errno::EINTR && tmpLen == 0i32 {
                    return false;
                }
                /* We didn't get the whole line because our buffer was too small.  */
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    buffer_overflow();
                }
                apply_normalization(utf32Buf, tmpLen, norm);
            }
            _ => {
                // none
                if last < BUF_SIZE as i32 && i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    BUFFER[last as usize] = i;
                    last += 1;
                }
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    while last < BUF_SIZE as i32
                        && {
                            i = get_uni_c(f);
                            i != -1i32
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        BUFFER[last as usize] = i;
                        last += 1;
                    }
                }
                if i == -1i32 && errno::errno() != errno::EINTR && last == first {
                    return false;
                }
                /* We didn't get the whole line because our buffer was too small.  */
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
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
    let mut rval: i32 = 0;
    let mut c: i32 = 0;
    if f.savedChar != -1 {
        rval = f.savedChar as i32;
        f.savedChar = -1;
        return rval;
    }
    let handle = f.handle.as_mut().unwrap();
    match f.encodingMode {
        UnicodeMode::Utf8 => {
            rval = ttstub_input_getc(handle);
            c = rval;
            if rval != -1 {
                let mut extraBytes: u16 = bytesFromUTF8[rval as usize] as u16;
                match extraBytes {
                    0..=3 => {
                        for _ in 0..extraBytes {
                            c = ttstub_input_getc(handle);
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
                    let mut lo_0: i32 = ttstub_input_getc(handle);
                    lo_0 += ttstub_input_getc(handle) << 8;
                    if lo_0 >= 0xdc00 && lo_0 <= 0xdfff {
                        rval = 0x10000 + (rval - 0xd800) * 0x400 + (lo_0 - 0xdc00)
                    } else {
                        rval = 0xfffd;
                        f.savedChar = lo_0 as i64
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
        scan_file_name(input);
        pack_file_name(cur_name, cur_area, cur_ext);
        let ufile = u_open_in(
            TTInputFormat::TEX,
            b"rb",
            UnicodeMode::from(*INTPAR(IntPar::xetex_default_input_mode)),
            *INTPAR(IntPar::xetex_default_input_encoding),
        );
        if ufile.is_some() {
            read_file[n as usize] = ufile;
            name_in_progress = true;
            make_name(|a, e, q, qc| {
                for k in name_of_file.encode_utf16() {
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
