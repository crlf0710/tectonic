#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::bridge::{
    ttstub_input_close, ttstub_input_getc, ttstub_input_open, ttstub_input_open_primary,
    ttstub_input_ungetc,
};
use crate::core_memory::{xcalloc, xmalloc, xstrdup};
use crate::stub_icu as icu;
use crate::stub_teckit as teckit;
use crate::xetex_ini::{
    buf_size, buffer, cur_area, cur_chr, cur_ext, cur_name, cur_val, first, last, max_buf_stack,
    name_in_progress, name_length, name_length16, name_of_file, name_of_file16, read_file,
    read_open, stop_at_space,
};
use crate::xetex_output::{print_int, print_nl};
use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetex0::{
    bad_utf8_warning, begin_diagnostic, begin_name, end_diagnostic, end_name,
    get_input_normalization_state, more_name, pack_file_name, scan_file_name, scan_four_bit_int,
    scan_optional_equals,
};
use crate::xetex_xetexd::print_c_string;
use bridge::stub_errno as errno;
use libc::{free, strlen};
use std::io::{Seek, SeekFrom};

use crate::*;

use bridge::TTInputFormat;

use bridge::InputHandleWrapper;
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

#[repr(C)]
pub(crate) struct UFILE {
    pub(crate) handle: Option<InputHandleWrapper>,
    pub(crate) savedChar: i64,
    pub(crate) skipNextLF: i16,
    pub(crate) encodingMode: i16,
    pub(crate) conversionData: *mut libc::c_void,
}

/* tectonic/xetex-io.c: low-level input/output functions tied to the XeTeX engine
   Copyright 2016-2019 The Tectonic Project
   Licensed under the MIT License.
*/
#[no_mangle]
pub(crate) static mut name_of_input_file: *mut i8 = ptr::null_mut();
pub(crate) unsafe fn tt_xetex_open_input(mut filefmt: TTInputFormat) -> Option<InputHandleWrapper> {
    let handle = if filefmt == TTInputFormat::TECTONIC_PRIMARY {
        ttstub_input_open_primary()
    } else {
        ttstub_input_open(name_of_file, filefmt as TTInputFormat, 0)
    };
    if handle.is_none() {
        return None;
    }
    name_length = strlen(name_of_file) as i32;
    free(name_of_input_file as *mut libc::c_void);
    name_of_input_file = xstrdup(name_of_file);
    handle
}
/* tables/values used in UTF-8 interpretation -
code is based on ConvertUTF.[ch] sample code
published by the Unicode consortium */
#[no_mangle]
pub(crate) static mut offsetsFromUTF8: [u32; 6] = [
    0u64 as u32,
    0x3080u64 as u32,
    0xe2080u64 as u32,
    0x3c82080u64 as u32,
    0xfa082080u64 as u32,
    0x82082080u64 as u32,
];
#[no_mangle]
pub(crate) static mut bytesFromUTF8: [u8; 256] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
];
#[no_mangle]
pub(crate) static mut firstByteMark: [u8; 7] = [0, 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc];
pub(crate) unsafe fn set_input_file_encoding(
    mut f: *mut UFILE,
    mut mode: i32,
    mut encodingData: i32,
) {
    if (*f).encodingMode as i32 == 5i32 && !(*f).conversionData.is_null() {
        icu::ucnv_close((*f).conversionData as *mut icu::UConverter);
    }
    (*f).conversionData = 0 as *mut libc::c_void;
    match mode {
        1 | 2 | 3 | 4 => (*f).encodingMode = mode as i16,
        5 => {
            let mut name: *mut i8 = gettexstring(encodingData);
            let mut err: UErrorCode = U_ZERO_ERROR;
            let mut cnv: *mut icu::UConverter = icu::ucnv_open(name, &mut err);
            if cnv.is_null() {
                begin_diagnostic();
                print_nl('E' as i32);
                print_c_string(b"rror \x00" as *const u8 as *const i8);
                print_int(err as i32);
                print_c_string(b" creating Unicode converter for `\x00" as *const u8 as *const i8);
                print_c_string(name);
                print_c_string(b"\'; reading as raw bytes\x00" as *const u8 as *const i8);
                end_diagnostic(1i32 != 0);
                (*f).encodingMode = 4_i16
            } else {
                (*f).encodingMode = 5_i16;
                (*f).conversionData = cnv as *mut libc::c_void
            }
            free(name as *mut libc::c_void);
        }
        _ => {}
    };
}
pub(crate) unsafe fn u_open_in(
    mut f: *mut *mut UFILE,
    mut filefmt: TTInputFormat,
    mut _fopen_mode: *const i8,
    mut mode: i32,
    mut encodingData: i32,
) -> i32 {
    let mut B1: i32 = 0;
    let mut B2: i32 = 0;
    let handle = tt_xetex_open_input(filefmt);
    if handle.is_none() {
        return 0i32;
    }
    *f = xmalloc(::std::mem::size_of::<UFILE>() as _) as *mut UFILE;
    (**f).encodingMode = 0_i16;
    (**f).conversionData = 0 as *mut libc::c_void;
    (**f).savedChar = -1i32 as i64;
    (**f).skipNextLF = 0_i16;
    (**f).handle = handle;
    if mode == 0i32 {
        /* sniff encoding form */
        let handle = (**f).handle.as_mut().unwrap();
        B1 = ttstub_input_getc(handle);
        B2 = ttstub_input_getc(handle);
        if B1 == 0xfei32 && B2 == 0xffi32 {
            mode = 2i32
        } else if B2 == 0xfei32 && B1 == 0xffi32 {
            mode = 3i32
        } else if B1 == 0i32 && B2 != 0i32 {
            mode = 2i32;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B2 == 0i32 && B1 != 0i32 {
            mode = 3i32;
            handle.seek(SeekFrom::Start(0)).unwrap();
        } else if B1 == 0xefi32 && B2 == 0xbbi32 {
            let mut B3: i32 = ttstub_input_getc(handle);
            if B3 == 0xbfi32 {
                mode = 1i32
            }
        }
        if mode == 0i32 {
            handle.seek(SeekFrom::Start(0)).unwrap();
            mode = 1i32
        }
    }
    set_input_file_encoding(*f, mode, encodingData);
    1i32
}
unsafe extern "C" fn buffer_overflow() {
    panic!(
        "unable to read an entire line (buf_size={})",
        buf_size as u32,
    );
}
unsafe extern "C" fn conversion_error(mut errcode: i32) {
    begin_diagnostic();
    print_nl('U' as i32);
    print_c_string(b"nicode conversion failed (ICU error code = \x00" as *const u8 as *const i8);
    print_int(errcode);
    print_c_string(b") discarding any remaining text\x00" as *const u8 as *const i8);
    end_diagnostic(1i32 != 0);
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
        &mut *buffer.offset(first as isize) as *mut UnicodeScalar as *mut u8,
        (::std::mem::size_of::<UnicodeScalar>() as u64).wrapping_mul((buf_size - first) as u64)
            as u32,
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
pub(crate) unsafe fn input_line(mut f: *mut UFILE) -> i32 {
    static mut byteBuffer: *mut i8 = ptr::null_mut();
    static mut utf32Buf: *mut u32 = ptr::null_mut();
    let mut i: i32 = 0;
    let mut tmpLen: i32 = 0;
    let mut norm: i32 = get_input_normalization_state();
    if (*f).handle.is_none() {
        /* NULL 'handle' means this: */
        panic!("reads from synthetic \"terminal\" file #0 should never happen");
    }
    last = first;
    if (*f).encodingMode as i32 == 5i32 {
        let mut bytesRead: u32 = 0_u32;
        let mut cnv: *mut icu::UConverter = 0 as *mut icu::UConverter;
        let mut outLen: i32 = 0;
        let mut errorCode: UErrorCode = U_ZERO_ERROR;
        if byteBuffer.is_null() {
            byteBuffer = xmalloc((buf_size + 1i32) as size_t) as *mut i8
        }
        /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
        let handle = (*f).handle.as_mut().unwrap();
        i = ttstub_input_getc(handle);
        if (*f).skipNextLF != 0 {
            (*f).skipNextLF = 0_i16;
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
            while bytesRead < buf_size as u32
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
            return 0i32;
        }
        if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
            buffer_overflow();
        }
        /* now apply the mapping to turn external bytes into Unicode characters in buffer */
        cnv = (*f).conversionData as *mut icu::UConverter;
        match norm {
            1 | 2 => {
                // NFC
                // NFD
                if utf32Buf.is_null() {
                    utf32Buf =
                        xcalloc(buf_size as size_t, ::std::mem::size_of::<u32>() as _) as *mut u32
                } // sets 'last' correctly
                tmpLen = icu::ucnv_toAlgorithmic(
                    icu::UCNV_UTF32_LittleEndian,
                    cnv,
                    utf32Buf as *mut i8,
                    (buf_size as u64).wrapping_mul(::std::mem::size_of::<u32>() as _) as i32,
                    byteBuffer,
                    bytesRead as i32,
                    &mut errorCode,
                );
                if errorCode as i32 != 0i32 {
                    conversion_error(errorCode as i32);
                    return 0i32;
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
                    &mut *buffer.offset(first as isize) as *mut UnicodeScalar as *mut i8,
                    (::std::mem::size_of::<UnicodeScalar>() as u64)
                        .wrapping_mul((buf_size - first) as u64) as i32,
                    byteBuffer,
                    bytesRead as i32,
                    &mut errorCode,
                );
                if errorCode as i32 != 0i32 {
                    conversion_error(errorCode as i32);
                    return 0i32;
                }
                outLen = (outLen as u64).wrapping_div(::std::mem::size_of::<UnicodeScalar>() as u64)
                    as i32 as i32;
                last = first + outLen
            }
        }
    } else {
        /* Recognize either LF or CR as a line terminator; skip initial LF if prev line ended with CR.  */
        i = get_uni_c(f);
        if (*f).skipNextLF != 0 {
            (*f).skipNextLF = 0_i16;
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
                        xcalloc(buf_size as size_t, ::std::mem::size_of::<u32>() as _) as *mut u32
                }
                tmpLen = 0i32;
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    let fresh3 = tmpLen;
                    tmpLen = tmpLen + 1;
                    *utf32Buf.offset(fresh3 as isize) = i as u32
                }
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    while tmpLen < buf_size
                        && {
                            i = get_uni_c(f);
                            i != -1i32
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        let fresh4 = tmpLen;
                        tmpLen = tmpLen + 1;
                        *utf32Buf.offset(fresh4 as isize) = i as u32
                    }
                }
                if i == -1i32 && errno::errno() != errno::EINTR && tmpLen == 0i32 {
                    return 0i32;
                }
                /* We didn't get the whole line because our buffer was too small.  */
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    buffer_overflow();
                }
                apply_normalization(utf32Buf, tmpLen, norm);
            }
            _ => {
                // none
                if last < buf_size && i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    let fresh5 = last;
                    last = last + 1;
                    *buffer.offset(fresh5 as isize) = i
                }
                if i != -1i32 && i != '\n' as i32 && i != '\r' as i32 {
                    while last < buf_size
                        && {
                            i = get_uni_c(f);
                            i != -1i32
                        }
                        && i != '\n' as i32
                        && i != '\r' as i32
                    {
                        let fresh6 = last;
                        last = last + 1;
                        *buffer.offset(fresh6 as isize) = i
                    }
                }
                if i == -1i32 && errno::errno() != errno::EINTR && last == first {
                    return 0i32;
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
        (*f).skipNextLF = 1_i16
    }
    *buffer.offset(last as isize) = ' ' as i32;
    if last >= max_buf_stack {
        max_buf_stack = last
    }
    /* Trim trailing space or EOL characters.  */
    while last > first
        && (*buffer.offset((last - 1i32) as isize) == ' ' as i32
            || *buffer.offset((last - 1i32) as isize) == '\r' as i32
            || *buffer.offset((last - 1i32) as isize) == '\n' as i32)
    {
        last -= 1
    }
    1i32
}
pub(crate) unsafe fn u_close(mut f: *mut UFILE) {
    if f.is_null() || (*f).handle.is_none() {
        /* NULL handle is stdin/terminal file. Shouldn't happen but meh. */
        return;
    }
    ttstub_input_close((*f).handle.take().unwrap());
    if (*f).encodingMode as i32 == 5i32 && !(*f).conversionData.is_null() {
        icu::ucnv_close((*f).conversionData as *mut icu::UConverter);
    }
    free(f as *mut libc::c_void);
}
pub(crate) unsafe fn get_uni_c(mut f: *mut UFILE) -> i32 {
    let mut rval: i32 = 0;
    let mut c: i32 = 0;
    if (*f).savedChar != -1 {
        rval = (*f).savedChar as i32;
        (*f).savedChar = -1;
        return rval;
    }
    let handle = (*f).handle.as_mut().unwrap();
    match (*f).encodingMode {
        1 => {
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
                                    ttstub_input_ungetc(handle, c);
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
        2 => {
            rval = ttstub_input_getc(handle);
            if rval != -1i32 {
                rval <<= 8i32;
                rval += ttstub_input_getc(handle);
                if rval >= 0xd800i32 && rval <= 0xdbffi32 {
                    let mut lo: i32 = ttstub_input_getc(handle);
                    lo <<= 8i32;
                    lo += ttstub_input_getc(handle);
                    if lo >= 0xdc00i32 && lo <= 0xdfffi32 {
                        rval = 0x10000i32 + (rval - 0xd800i32) * 0x400i32 + (lo - 0xdc00i32)
                    } else {
                        rval = 0xfffdi32;
                        (*f).savedChar = lo as i64
                    }
                } else if rval >= 0xdc00i32 && rval <= 0xdfffi32 {
                    rval = 0xfffdi32
                }
            }
        }
        3 => {
            rval = ttstub_input_getc(handle);
            if rval != -1i32 {
                rval += ttstub_input_getc(handle) << 8i32;
                if rval >= 0xd800i32 && rval <= 0xdbffi32 {
                    let mut lo_0: i32 = ttstub_input_getc(handle);
                    lo_0 += ttstub_input_getc(handle) << 8i32;
                    if lo_0 >= 0xdc00i32 && lo_0 <= 0xdfffi32 {
                        rval = 0x10000i32 + (rval - 0xd800i32) * 0x400i32 + (lo_0 - 0xdc00i32)
                    } else {
                        rval = 0xfffdi32;
                        (*f).savedChar = lo_0 as i64
                    }
                } else if rval >= 0xdc00i32 && rval <= 0xdfffi32 {
                    rval = 0xfffdi32
                }
            }
        }
        4 => rval = ttstub_input_getc(handle),
        _ => {
            panic!(
                "internal error; file input mode={}",
                (*f).encodingMode as i32,
            );
        }
    }
    rval
}
/* tectonic/xetex-io.h: XeTeX-specific low-level I/O routines
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
pub(crate) unsafe fn make_utf16_name() {
    let mut s: *mut u8 = name_of_file as *mut u8;
    let mut rval: u32 = 0;
    let mut t: *mut u16 = 0 as *mut u16;
    static mut name16len: i32 = 0i32;
    if name16len <= name_length {
        free(name_of_file16 as *mut libc::c_void);
        name16len = name_length + 10i32;
        name_of_file16 =
            xcalloc(name16len as size_t, ::std::mem::size_of::<u16>() as _) as *mut UTF16_code
    }
    t = name_of_file16;
    while s < (name_of_file as *mut u8).offset(name_length as isize) {
        let mut extraBytes: u16 = 0;
        rval = *s as u32;
        s = s.offset(1);
        extraBytes = bytesFromUTF8[rval as usize] as u16;
        for _ in 0..extraBytes {
            rval <<= 6;
            if *s != 0 {
                rval = (rval as u32).wrapping_add(*s as u32);
                s = s.offset(1);
            }
        }
        rval = (rval as u32).wrapping_sub(offsetsFromUTF8[extraBytes as usize]) as u32;
        if rval > 0xffff_u32 {
            rval = (rval as u32).wrapping_sub(0x10000_u32) as u32;
            let fresh13 = t;
            t = t.offset(1);
            *fresh13 = (0xd800_u32).wrapping_add(rval.wrapping_div(0x400_u32)) as u16;
            let fresh14 = t;
            t = t.offset(1);
            *fresh14 = (0xdc00_u32).wrapping_add(rval.wrapping_rem(0x400_u32)) as u16
        } else {
            let fresh15 = t;
            t = t.offset(1);
            *fresh15 = rval as u16
        }
    }
    name_length16 = t.wrapping_offset_from(name_of_file16) as i64 as i32;
}
pub(crate) unsafe fn open_or_close_in() {
    use xetex_consts::*;
    let mut c: u8 = 0;
    let mut n: u8 = 0;
    let mut k: i32 = 0;
    c = cur_chr as u8;
    scan_four_bit_int();
    n = cur_val as u8;
    if read_open[n as usize] as i32 != 2i32 {
        u_close(read_file[n as usize]);
        read_open[n as usize] = 2_u8
    }
    if c as i32 != 0i32 {
        scan_optional_equals();
        scan_file_name();
        pack_file_name(cur_name, cur_area, cur_ext);
        if u_open_in(
            &mut *read_file.as_mut_ptr().offset(n as isize),
            TTInputFormat::TEX,
            b"rb\x00" as *const u8 as *const i8,
            INTPAR(INT_PAR__xetex_default_input_mode),
            INTPAR(INT_PAR__xetex_default_input_encoding),
        ) != 0
        {
            make_utf16_name();
            name_in_progress = true;
            begin_name();
            stop_at_space = false;
            k = 0i32;
            while k < name_length16 && more_name(*name_of_file16.offset(k as isize)) as i32 != 0 {
                k += 1
            }
            stop_at_space = true;
            end_name();
            name_in_progress = false;
            read_open[n as usize] = 1_u8
        }
    };
}
