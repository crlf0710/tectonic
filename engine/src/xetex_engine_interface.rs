#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
)]

use crate::xetex_ini::{
    halt_on_error_p, in_initex_mode, semantic_pagination_enabled, shell_escape_enabled,
    synctex_enabled,
};

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/* engine-interface.c: programmatic interface to control the engine behavior
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* These functions aren't used within the C/C++ library, but are called
 * by the Rust code to configure the XeTeX engine before launching it. */
pub unsafe fn tt_xetex_set_int_variable(var_name: &str, value: i32) -> i32 {
    if var_name == "halt_on_error_p" {
        halt_on_error_p = value
    } else if var_name == "in_initex_mode" {
        in_initex_mode = value != 0i32
    } else if var_name == "synctex_enabled" {
        synctex_enabled = (value != 0i32) as i32
    } else if var_name == "semantic_pagination_enabled" {
        semantic_pagination_enabled = value != 0i32
    } else if var_name == "shell_escape_enabled" {
        shell_escape_enabled = value != 0i32
    } else {
        return 1i32;
    } /* Uh oh: unrecognized variable */
    0i32
    /* success */
}
pub(crate) unsafe fn tt_xetex_set_string_variable(
    mut _var_name: *mut i8,
    mut _value: *mut i8,
) -> i32 {
    /* Currently unused; see Git history for how we used to set output_comment */
    1
}
