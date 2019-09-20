#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::core_memory::xmalloc;
use crate::xetex_errors::{confusion, error, fatal_error, overflow};
use crate::xetex_ext::{
    apply_tfm_font_mapping, makeXDVGlyphArrayData, make_font_def, store_justified_native_glyphs,
};
use crate::xetex_ini::{
    avail, char_base, cur_area, cur_cs, cur_dir, cur_ext, cur_h, cur_h_offset, cur_list, cur_name,
    cur_page_height, cur_page_width, cur_tok, cur_v, cur_v_offset, dead_cycles, def_ref,
    doing_leaders, doing_special, eqtb, file_line_error_style_p, file_offset, font_area, font_bc,
    font_check, font_dsize, font_ec, font_glue, font_info, font_letter_space, font_mapping,
    font_name, font_ptr, font_size, font_used, help_line, help_ptr, init_pool_ptr, job_name,
    last_bop, log_opened, max_h, max_print_line, max_push, max_v, mem, name_of_file,
    output_file_extension, pdf_last_x_pos, pdf_last_y_pos, pool_ptr, pool_size, rule_dp, rule_ht,
    rule_wd, rust_stdout, selector, semantic_pagination_enabled, str_pool, str_ptr, str_start,
    temp_ptr, term_offset, total_pages, width_base, write_file, write_loc, write_open, xdv_buffer,
    xtx_ligature_present, LR_problems, LR_ptr,
};
use crate::xetex_ini::{memory_word, Selector};
use crate::xetex_output::{
    print, print_char, print_cstr, print_file_line, print_file_name, print_int, print_ln,
    print_nl_cstr, print_raw_char, print_scaled,
};
use crate::xetex_scaledmath::tex_round;
use crate::xetex_stringpool::length;
use crate::xetex_synctex::{
    synctex_current, synctex_hlist, synctex_horizontal_rule_or_glue, synctex_kern, synctex_math,
    synctex_sheet, synctex_teehs, synctex_tsilh, synctex_tsilv, synctex_vlist, synctex_void_hlist,
    synctex_void_vlist,
};
use crate::xetex_texmfmp::maketexstring;
use crate::xetex_xetex0::{
    begin_diagnostic, begin_token_list, effective_char, end_diagnostic, end_token_list, flush_list,
    flush_node_list, free_node, get_avail, get_node, get_token, make_name_string, new_kern,
    new_math, new_native_word_node, open_log_file, pack_file_name, pack_job_name, prepare_mag,
    scan_toks, show_box, show_token_list, token_show,
};
use crate::xetex_xetexd::{is_char_node, print_c_string};
use crate::{ttstub_output_close, ttstub_output_flush, ttstub_output_open, ttstub_output_write};
use bridge::_tt_abort;
use libc::{free, strerror, strlen};

pub type size_t = u64;
pub type rust_output_handle_t = *mut libc::c_void;
pub type scaled_t = i32;

/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* Extra stuff used in various change files for various reasons.  */
/* Array allocations. Add 1 to size to account for Pascal indexing convention. */
/*11:*/
/*18: */
pub type UTF16_code = u16;
pub type eight_bits = u8;
pub type pool_pointer = i32;
pub type str_number = i32;
pub type packed_UTF16_code = u16;
pub type small_number = i16;
/* Symbolic accessors for various TeX data structures. I would loooove to turn these
 * into actual structs, but the path to doing that is not currently clear. Making
 * field references symbolic seems like a decent start. Sadly I don't see how to do
 * this conversion besides painstakingly annotating things.
 */
/* half of LLIST_info(p) */
/* the other half of LLIST_info(p) */
/* subtype; records L/R direction mode */
/* a scaled; 1 <=> WEB const `width_offset` */
/* a scaled; 2 <=> WEB const `depth_offset` */
/* a scaled; 3 <=> WEB const `height_offset` */
/* a scaled */
/* aka `link` of p+5 */
/* aka `type` of p+5 */
/* aka `subtype` of p+5 */
/* the glue ratio */
/* aka "subtype" of a node */
/* aka "rlink" in double-linked list */
/* aka "llink" in doubly-linked list */
/* was originally the `mem[x+2].int` field */
/* a scaled; "active_short" in the WEB */
/* a scaled */
/* aka "type" of a node */
/* aka "subtype" of a node */
/* the "natural width" difference */
/* the stretch difference in points */
/* the stretch difference in fil */
/* the stretch difference in fill */
/* the stretch difference in fill */
/* the shrink difference */
/* aka "subtype" of a node */
/* aka "llink" in doubly-linked list */
/* aka "rlink" in double-linked list */
/* "new left_edge position relative to cur_h" */
/* aka "llink" in doubly-linked list */
/* aka "rlink" in double-linked list */
/* "the floating_penalty to be used" */
/* a glue pointer */
/* a pointer to a vlist */
/* language number, 0..255 */
/* "minimum left fragment, range 1..63" */
/* "minimum right fragment, range 1..63" */
/* WEB: font(lig_char(p)) */
/* WEB: character(lig_char(p)) */
/* WEB: link(lig_char(p)) */
/* "head of the token list for the mark" */
/* "the mark class" */
/* To check: do these really only apply to MATH_NODEs? */
/* number of UTF16 items in the text */
/* ... or the glyph number, if subtype==GLYPH_NODE */
/* "an insertion for this class will break here if anywhere" */
/* "this insertion might break at broken_ptr" */
/* "the most recent insertion for this subtype" */
/* "the optimum most recent insertion" */
/* aka "llink" in doubly-linked list */
/* siggggghhhhh */
/* aka "rlink" in double-linked list */
/* aka "info" */
/* was originally the `mem[x+1].int` field */
/* number of bytes in the path item */
/* "reference count of token list to write" */
/* Synctex hacks various nodes to add an extra word at the end to store its
 * information, hence the need to know the node size to get the synctex
 * info. */
/* aka "link" of a link-list node */
/* aka "type" of a node */
/* aka "subtype" of a node */
/* a scaled */
/* a scaled */
/* e-TeX extended marks stuff ... not sure where to put these */
/* \topmarks<n> */
/* \firstmarks<n> */
/* \botmarks<n> */
/* \splitfirstmarks<n> */
/* \splitbotmarks<n> */
pub type glue_ord = u8;
pub type internal_font_number = i32;
#[inline]
unsafe extern "C" fn cur_length() -> pool_pointer {
    pool_ptr - *str_start.offset((str_ptr - 65536i32) as isize)
}
/* DVI code */
static mut dvi_file: rust_output_handle_t = 0 as *const libc::c_void as *mut libc::c_void;
static mut output_file_name: str_number = 0;
static mut dvi_buf: *mut eight_bits = 0 as *const eight_bits as *mut eight_bits;
static mut dvi_limit: i32 = 0;
static mut g: i32 = 0;
static mut lq: i32 = 0;
static mut lr: i32 = 0;
static mut dvi_ptr: i32 = 0;
static mut dvi_offset: i32 = 0;
static mut dvi_gone: i32 = 0;
static mut down_ptr: i32 = 0;
static mut right_ptr: i32 = 0;
static mut dvi_h: scaled_t = 0;
static mut dvi_v: scaled_t = 0;
static mut dvi_f: internal_font_number = 0;
static mut cur_s: i32 = 0;
#[no_mangle]
pub unsafe extern "C" fn initialize_shipout_variables() {
    output_file_name = 0i32;
    dvi_buf = xmalloc(
        ((16384i32 + 1i32) as u64).wrapping_mul(::std::mem::size_of::<eight_bits>() as u64),
    ) as *mut eight_bits;
    dvi_limit = 16384i32;
    dvi_ptr = 0i32;
    dvi_offset = 0i32;
    dvi_gone = 0i32;
    down_ptr = -0xfffffffi32;
    right_ptr = -0xfffffffi32;
    cur_s = -1i32;
}
#[no_mangle]
pub unsafe extern "C" fn deinitialize_shipout_variables() {
    free(dvi_buf as *mut libc::c_void);
    dvi_buf = 0 as *mut eight_bits;
}
#[inline]
unsafe extern "C" fn dvi_out(mut c: eight_bits) {
    let fresh1 = dvi_ptr;
    dvi_ptr = dvi_ptr + 1;
    *dvi_buf.offset(fresh1 as isize) = c;
    if dvi_ptr == dvi_limit {
        dvi_swap();
    };
}
/*660: output the box `p` */
#[no_mangle]
pub unsafe extern "C" fn ship_out(mut p: i32) {
    let mut page_loc: i32 = 0;
    let mut j: u8 = 0;
    let mut k: u8 = 0;
    let mut s: pool_pointer = 0;
    let mut l: u8 = 0;
    let mut output_comment: *const i8 = b"tectonic\x00" as *const u8 as *const i8;
    synctex_sheet(
        (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 17i32) as isize,
        ))
        .b32
        .s1,
    );
    if job_name == 0i32 {
        open_log_file();
    }
    if (*eqtb.offset(
        (1i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 34i32) as isize,
    ))
    .b32
    .s1 > 0i32
    {
        print_nl_cstr(b"\x00" as *const u8 as *const i8);
        print_ln();
        print_cstr(b"Completed box being shipped out\x00" as *const u8 as *const i8);
    }
    if term_offset > max_print_line - 9i32 {
        print_ln();
    } else if term_offset > 0i32 || file_offset > 0i32 {
        print_char(' ' as i32);
    }
    print_char('[' as i32);
    j = 9_u8;
    while j as i32 > 0i32
        && (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + j as i32) as isize,
        ))
        .b32
        .s1 == 0i32
    {
        j = j.wrapping_sub(1)
    }
    k = 0_u8;
    while k as i32 <= j as i32 {
        print_int(
            (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + k as i32) as isize,
            ))
            .b32
            .s1,
        );
        if (k as i32) < j as i32 {
            print_char('.' as i32);
        }
        k = k.wrapping_add(1)
    }
    ttstub_output_flush(rust_stdout);
    if (*eqtb.offset(
        (1i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 34i32) as isize,
    ))
    .b32
    .s1 > 0i32
    {
        print_char(']' as i32);
        begin_diagnostic();
        show_box(p);
        end_diagnostic(1i32 != 0);
    }
    /*662: "Ship box `p` out." */
    /*663: "Update the values of max_h and max_v; but if the page is too
     * large, goto done". */
    if (*mem.offset((p + 3i32) as isize)).b32.s1 > 0x3fffffffi32
        || (*mem.offset((p + 2i32) as isize)).b32.s1 > 0x3fffffffi32
        || (*mem.offset((p + 3i32) as isize)).b32.s1
            + (*mem.offset((p + 2i32) as isize)).b32.s1
            + (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 19i32) as isize,
            ))
            .b32
            .s1
            > 0x3fffffffi32
        || (*mem.offset((p + 1i32) as isize)).b32.s1
            + (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 18i32) as isize,
            ))
            .b32
            .s1
            > 0x3fffffffi32
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! \x00" as *const u8 as *const i8);
        }
        print_cstr(b"Huge page cannot be shipped out\x00" as *const u8 as *const i8);
        help_ptr = 2_u8;
        help_line[1] =
            b"The page just created is more than 18 feet tall or\x00" as *const u8 as *const i8;
        help_line[0] = b"more than 18 feet wide, so I suspect something went wrong.\x00"
            as *const u8 as *const i8;
        error();
        if (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 34i32) as isize,
        ))
        .b32
        .s1 <= 0i32
        {
            begin_diagnostic();
            print_nl_cstr(b"The following box has been deleted:\x00" as *const u8 as *const i8);
            show_box(p);
            end_diagnostic(1i32 != 0);
        }
    } else {
        if (*mem.offset((p + 3i32) as isize)).b32.s1
            + (*mem.offset((p + 2i32) as isize)).b32.s1
            + (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 19i32) as isize,
            ))
            .b32
            .s1
            > max_v
        {
            max_v = (*mem.offset((p + 3i32) as isize)).b32.s1
                + (*mem.offset((p + 2i32) as isize)).b32.s1
                + (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 85i32
                        + 256i32
                        + (0x10ffffi32 + 1i32)
                        + 19i32) as isize,
                ))
                .b32
                .s1
        }
        if (*mem.offset((p + 1i32) as isize)).b32.s1
            + (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 18i32) as isize,
            ))
            .b32
            .s1
            > max_h
        {
            max_h = (*mem.offset((p + 1i32) as isize)).b32.s1
                + (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 85i32
                        + 256i32
                        + (0x10ffffi32 + 1i32)
                        + 18i32) as isize,
                ))
                .b32
                .s1
        }
        /*637: "Initialize variables as ship_out begins." */
        dvi_h = 0i32;
        dvi_v = 0i32;
        cur_h = (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 18i32) as isize,
        ))
        .b32
        .s1;
        dvi_f = 0i32;
        /*1405: "Calculate page dimensions and margins" */
        /* 4736287 = round(0xFFFF * 72.27) ; i.e., 1 inch expressed as a scaled_t */
        cur_h_offset = (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 18i32) as isize,
        ))
        .b32
        .s1 + 4736287i32;
        cur_v_offset = (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 19i32) as isize,
        ))
        .b32
        .s1 + 4736287i32;
        if (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 21i32) as isize,
        ))
        .b32
        .s1 != 0i32
        {
            cur_page_width = (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 21i32) as isize,
            ))
            .b32
            .s1
        } else {
            cur_page_width = (*mem.offset((p + 1i32) as isize)).b32.s1 + 2i32 * cur_h_offset
        }
        if (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 22i32) as isize,
        ))
        .b32
        .s1 != 0i32
        {
            cur_page_height = (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 22i32) as isize,
            ))
            .b32
            .s1
        } else {
            cur_page_height = (*mem.offset((p + 3i32) as isize)).b32.s1
                + (*mem.offset((p + 2i32) as isize)).b32.s1
                + 2i32 * cur_v_offset
        }
        /* ... resuming 637 ... open up the DVI file if needed */
        if output_file_name == 0i32 {
            if job_name == 0i32 {
                open_log_file();
            }
            pack_job_name(output_file_extension);
            dvi_file = ttstub_output_open(name_of_file, 0i32);
            if dvi_file.is_null() {
                _tt_abort(
                    b"cannot open output file \"%s\"\x00" as *const u8 as *const i8,
                    name_of_file,
                );
            }
            output_file_name = make_name_string()
        }
        /* First page? Emit preamble items. */
        if total_pages == 0i32 {
            dvi_out(247i32 as eight_bits); /* magic values: conversion ratio for sp */
            if semantic_pagination_enabled {
                dvi_out(100i32 as eight_bits); /* magic values: conversion ratio for sp */
            } else {
                dvi_out(7i32 as eight_bits);
            }
            dvi_four(25400000i64 as i32);
            dvi_four(473628672i64 as i32);
            prepare_mag();
            dvi_four(
                (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 17i32) as isize,
                ))
                .b32
                .s1,
            );
            l = strlen(output_comment) as u8;
            dvi_out(l);
            s = 0i32;
            while s < l as i32 {
                dvi_out(*output_comment.offset(s as isize) as eight_bits);
                s += 1
            }
        }
        /* ... resuming 662 ... Emit per-page preamble. */
        page_loc = dvi_offset + dvi_ptr;
        dvi_out(139i32 as eight_bits);
        k = 0_u8;
        while (k as i32) < 10i32 {
            dvi_four(
                (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 85i32
                        + k as i32) as isize,
                ))
                .b32
                .s1,
            );
            k = k.wrapping_add(1)
        }
        dvi_four(last_bop);
        last_bop = page_loc;
        /* Generate a PDF pagesize special unilaterally */
        let old_setting = selector;
        selector = Selector::NEW_STRING;
        print_cstr(b"pdf:pagesize \x00" as *const u8 as *const i8);
        if (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 85i32
                + 256i32
                + (0x10ffffi32 + 1i32)
                + 21i32) as isize,
        ))
        .b32
        .s1 <= 0i32
            || (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 22i32) as isize,
            ))
            .b32
            .s1 <= 0i32
        {
            print_cstr(b"default\x00" as *const u8 as *const i8);
        } else {
            print_cstr(b"width\x00" as *const u8 as *const i8);
            print(' ' as i32);
            print_scaled(
                (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 85i32
                        + 256i32
                        + (0x10ffffi32 + 1i32)
                        + 21i32) as isize,
                ))
                .b32
                .s1,
            );
            print_cstr(b"pt\x00" as *const u8 as *const i8);
            print(' ' as i32);
            print_cstr(b"height\x00" as *const u8 as *const i8);
            print(' ' as i32);
            print_scaled(
                (*eqtb.offset(
                    (1i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 1i32
                        + 15000i32
                        + 12i32
                        + 9000i32
                        + 1i32
                        + 1i32
                        + 19i32
                        + 256i32
                        + 256i32
                        + 13i32
                        + 256i32
                        + 4i32
                        + 256i32
                        + 1i32
                        + 3i32 * 256i32
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + (0x10ffffi32 + 1i32)
                        + 85i32
                        + 256i32
                        + (0x10ffffi32 + 1i32)
                        + 22i32) as isize,
                ))
                .b32
                .s1,
            );
            print_cstr(b"pt\x00" as *const u8 as *const i8);
        }
        selector = old_setting;
        dvi_out(239i32 as eight_bits);
        dvi_out(cur_length() as eight_bits);
        s = *str_start.offset((str_ptr - 65536i32) as isize);
        while s < pool_ptr {
            dvi_out(*str_pool.offset(s as isize) as eight_bits);
            s += 1
        }
        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
        /* Done with the synthesized special. The meat: emit this page box. */
        cur_v = (*mem.offset((p + 3i32) as isize)).b32.s1
            + (*eqtb.offset(
                (1i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 1i32
                    + 15000i32
                    + 12i32
                    + 9000i32
                    + 1i32
                    + 1i32
                    + 19i32
                    + 256i32
                    + 256i32
                    + 13i32
                    + 256i32
                    + 4i32
                    + 256i32
                    + 1i32
                    + 3i32 * 256i32
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + (0x10ffffi32 + 1i32)
                    + 85i32
                    + 256i32
                    + (0x10ffffi32 + 1i32)
                    + 19i32) as isize,
            ))
            .b32
            .s1; /*"Does this need changing for upwards mode???"*/
        temp_ptr = p;
        if (*mem.offset(p as isize)).b16.s1 as i32 == 1i32 {
            vlist_out();
        } else {
            hlist_out();
        }
        dvi_out(140i32 as eight_bits);
        total_pages += 1;
        cur_s = -1i32
    }
    /*1518: "Check for LR anomalies at the end of ship_out" */
    if LR_problems > 0i32 {
        print_ln();
        print_nl_cstr(b"\\endL or \\endR problem (\x00" as *const u8 as *const i8);
        print_int(LR_problems / 10000i32);
        print_cstr(b" missing, \x00" as *const u8 as *const i8);
        print_int(LR_problems % 10000i32);
        print_cstr(b" extra\x00" as *const u8 as *const i8);
        LR_problems = 0i32;
        print_char(')' as i32);
        print_ln();
    }
    if LR_ptr != -0xfffffffi32 || cur_dir as i32 != 0i32 {
        confusion(b"LR3\x00" as *const u8 as *const i8);
    }
    if (*eqtb.offset(
        (1i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 34i32) as isize,
    ))
    .b32
    .s1 <= 0i32
    {
        print_char(']' as i32);
    }
    dead_cycles = 0i32;
    ttstub_output_flush(rust_stdout);
    flush_node_list(p);
    synctex_teehs();
}
/*639: Output an hlist */
unsafe extern "C" fn hlist_out() {
    let mut current_block: u64;
    let mut base_line: scaled_t = 0;
    let mut left_edge: scaled_t = 0;
    let mut save_h: scaled_t = 0;
    let mut save_v: scaled_t = 0;
    let mut this_box: i32 = 0;
    let mut g_order: glue_ord = 0;
    let mut g_sign: u8 = 0;
    let mut p: i32 = 0;
    let mut save_loc: i32 = 0;
    let mut leader_box: i32 = 0;
    let mut leader_wd: scaled_t = 0;
    let mut lx: scaled_t = 0;
    let mut outer_doing_leaders: bool = false;
    let mut edge: scaled_t = 0;
    let mut prev_p: i32 = 0;
    let mut len: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut k: i32 = 0;
    let mut j: i32 = 0;
    let mut glue_temp: f64 = 0.;
    let mut cur_glue: f64 = 0.;
    let mut cur_g: scaled_t = 0;
    let mut c: u16 = 0;
    let mut f: internal_font_number = 0;
    cur_g = 0i32;
    cur_glue = 0.0f64;
    this_box = temp_ptr;
    g_order = (*mem.offset((this_box + 5i32) as isize)).b16.s0 as glue_ord;
    g_sign = (*mem.offset((this_box + 5i32) as isize)).b16.s1 as u8;
    if (*eqtb.offset(
        (1i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 80i32) as isize,
    ))
    .b32
    .s1 > 1i32
    {
        /*640: "Extra stuff for justifiable AAT text..." "Merge sequences of
         * words using native fonts and inter-word spaces into single
         * nodes" */
        p = (*mem.offset((this_box + 5i32) as isize)).b32.s1; /* this gets the list within the box */
        prev_p = this_box + 5i32;
        while p != -0xfffffffi32 {
            if (*mem.offset(p as isize)).b32.s1 != -0xfffffffi32 {
                if p != -0xfffffffi32
                    && !is_char_node(p)
                    && (*mem.offset(p as isize)).b16.s1 as i32 == 8i32
                    && ((*mem.offset(p as isize)).b16.s0 as i32 == 40i32
                        || (*mem.offset(p as isize)).b16.s0 as i32 == 41i32)
                    && *font_letter_space.offset((*mem.offset((p + 4i32) as isize)).b16.s2 as isize)
                        == 0i32
                {
                    /* "got a word in an AAT font, might be the start of a run" */
                    r = p;
                    k = (*mem.offset((r + 4i32) as isize)).b16.s1 as i32;
                    q = (*mem.offset(p as isize)).b32.s1;
                    loop {
                        /*641: "Advance `q` past ignorable nodes." This test is
                         * mostly `node_is_invisible_to_interword_space`. 641 is
                         * reused a few times here. */
                        while q != -0xfffffffi32
                            && !is_char_node(q)
                            && ((*mem.offset(q as isize)).b16.s1 as i32 == 12i32
                                || (*mem.offset(q as isize)).b16.s1 as i32 == 3i32
                                || (*mem.offset(q as isize)).b16.s1 as i32 == 4i32
                                || (*mem.offset(q as isize)).b16.s1 as i32 == 5i32
                                || (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                    && (*mem.offset(q as isize)).b16.s0 as i32 <= 4i32)
                        {
                            q = (*mem.offset(q as isize)).b32.s1
                        }
                        if !(q != -0xfffffffi32 && !is_char_node(q)) {
                            break;
                        }
                        if (*mem.offset(q as isize)).b16.s1 as i32 == 10i32
                            && (*mem.offset(q as isize)).b16.s0 as i32 == 0i32
                        {
                            if (*mem.offset((q + 1i32) as isize)).b32.s0
                                == *font_glue
                                    .offset((*mem.offset((r + 4i32) as isize)).b16.s2 as isize)
                            {
                                /* "Found a normal space; if the next node is
                                 * another word in the same font, we'll
                                 * merge." */
                                q = (*mem.offset(q as isize)).b32.s1;
                                while q != -0xfffffffi32
                                    && !is_char_node(q)
                                    && ((*mem.offset(q as isize)).b16.s1 as i32 == 12i32
                                        || (*mem.offset(q as isize)).b16.s1 as i32 == 3i32
                                        || (*mem.offset(q as isize)).b16.s1 as i32 == 4i32
                                        || (*mem.offset(q as isize)).b16.s1 as i32 == 5i32
                                        || (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                            && (*mem.offset(q as isize)).b16.s0 as i32 <= 4i32)
                                {
                                    q = (*mem.offset(q as isize)).b32.s1
                                }
                                if q != -0xfffffffi32
                                    && !is_char_node(q)
                                    && (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                    && ((*mem.offset(q as isize)).b16.s0 as i32 == 40i32
                                        || (*mem.offset(q as isize)).b16.s0 as i32 == 41i32)
                                    && (*mem.offset((q + 4i32) as isize)).b16.s2 as i32
                                        == (*mem.offset((r + 4i32) as isize)).b16.s2 as i32
                                {
                                    p = q;
                                    k += 1i32 + (*mem.offset((q + 4i32) as isize)).b16.s1 as i32;
                                    q = (*mem.offset(q as isize)).b32.s1;
                                    continue;
                                }
                            } else {
                                q = (*mem.offset(q as isize)).b32.s1
                            }
                            if !(q != -0xfffffffi32
                                && !is_char_node(q)
                                && (*mem.offset(q as isize)).b16.s1 as i32 == 11i32
                                && (*mem.offset(q as isize)).b16.s0 as i32 == 3i32)
                            {
                                break;
                            }
                            q = (*mem.offset(q as isize)).b32.s1;
                            while q != -0xfffffffi32
                                && !is_char_node(q)
                                && ((*mem.offset(q as isize)).b16.s1 as i32 == 12i32
                                    || (*mem.offset(q as isize)).b16.s1 as i32 == 3i32
                                    || (*mem.offset(q as isize)).b16.s1 as i32 == 4i32
                                    || (*mem.offset(q as isize)).b16.s1 as i32 == 5i32
                                    || (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                        && (*mem.offset(q as isize)).b16.s0 as i32 <= 4i32)
                            {
                                q = (*mem.offset(q as isize)).b32.s1
                            }
                            if !(q != -0xfffffffi32
                                && !is_char_node(q)
                                && (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                && ((*mem.offset(q as isize)).b16.s0 as i32 == 40i32
                                    || (*mem.offset(q as isize)).b16.s0 as i32 == 41i32)
                                && (*mem.offset((q + 4i32) as isize)).b16.s2 as i32
                                    == (*mem.offset((r + 4i32) as isize)).b16.s2 as i32)
                            {
                                break;
                            }
                            p = q;
                            k += 1i32 + (*mem.offset((q + 4i32) as isize)).b16.s1 as i32;
                            q = (*mem.offset(q as isize)).b32.s1
                        } else {
                            if !(q != -0xfffffffi32
                                && !is_char_node(q)
                                && (*mem.offset(q as isize)).b16.s1 as i32 == 8i32
                                && ((*mem.offset(q as isize)).b16.s0 as i32 == 40i32
                                    || (*mem.offset(q as isize)).b16.s0 as i32 == 41i32)
                                && (*mem.offset((q + 4i32) as isize)).b16.s2 as i32
                                    == (*mem.offset((r + 4i32) as isize)).b16.s2 as i32)
                            {
                                break;
                            }
                            p = q;
                            q = (*mem.offset(q as isize)).b32.s1
                        }
                    }
                    /* "Now r points to the first native_word_node of the run,
                     * and p to the last." */
                    if p != r {
                        if pool_ptr + k > pool_size {
                            overflow(
                                b"pool size\x00" as *const u8 as *const i8,
                                pool_size - init_pool_ptr,
                            );
                        }
                        k = 0i32;
                        q = r;
                        loop {
                            if (*mem.offset(q as isize)).b16.s1 as i32 == 8i32 {
                                if (*mem.offset(q as isize)).b16.s0 as i32 == 40i32
                                    || (*mem.offset(q as isize)).b16.s0 as i32 == 41i32
                                {
                                    j = 0i32;
                                    while j < (*mem.offset((q + 4i32) as isize)).b16.s1 as i32 {
                                        *str_pool.offset(pool_ptr as isize) = *(&mut *mem
                                            .offset((q + 6i32) as isize)
                                            as *mut memory_word
                                            as *mut u16)
                                            .offset(j as isize);
                                        pool_ptr += 1;
                                        j += 1
                                    }
                                    k += (*mem.offset((q + 1i32) as isize)).b32.s1
                                }
                            } else if (*mem.offset(q as isize)).b16.s1 as i32 == 10i32 {
                                *str_pool.offset(pool_ptr as isize) =
                                    ' ' as i32 as packed_UTF16_code;
                                pool_ptr += 1;
                                g = (*mem.offset((q + 1i32) as isize)).b32.s0;
                                k += (*mem.offset((g + 1i32) as isize)).b32.s1;
                                if g_sign as i32 != 0i32 {
                                    if g_sign as i32 == 1i32 {
                                        if (*mem.offset(g as isize)).b16.s1 as i32 == g_order as i32
                                        {
                                            k += tex_round(
                                                (*mem.offset((this_box + 6i32) as isize)).gr
                                                    * (*mem.offset((g + 2i32) as isize)).b32.s1
                                                        as f64,
                                            )
                                        }
                                    } else if (*mem.offset(g as isize)).b16.s0 as i32
                                        == g_order as i32
                                    {
                                        k -= tex_round(
                                            (*mem.offset((this_box + 6i32) as isize)).gr
                                                * (*mem.offset((g + 3i32) as isize)).b32.s1 as f64,
                                        )
                                    }
                                }
                            } else if (*mem.offset(q as isize)).b16.s1 as i32 == 11i32 {
                                k += (*mem.offset((q + 1i32) as isize)).b32.s1
                            }
                            if q == p {
                                break;
                            }
                            q = (*mem.offset(q as isize)).b32.s1
                        }
                        q = new_native_word_node(
                            (*mem.offset((r + 4i32) as isize)).b16.s2 as internal_font_number,
                            cur_length(),
                        );
                        (*mem.offset(q as isize)).b16.s0 = (*mem.offset(r as isize)).b16.s0;
                        j = 0i32;
                        while j < cur_length() {
                            *(&mut *mem.offset((q + 6i32) as isize) as *mut memory_word
                                as *mut u16)
                                .offset(j as isize) = *str_pool.offset(
                                (*str_start.offset((str_ptr - 65536i32) as isize) + j) as isize,
                            );
                            j += 1
                        }
                        /* "Link q into the list in place of r...p" */
                        (*mem.offset((q + 1i32) as isize)).b32.s1 = k;
                        store_justified_native_glyphs(&mut *mem.offset(q as isize)
                            as *mut memory_word
                            as *mut libc::c_void);
                        (*mem.offset(prev_p as isize)).b32.s1 = q;
                        (*mem.offset(q as isize)).b32.s1 = (*mem.offset(p as isize)).b32.s1;
                        (*mem.offset(p as isize)).b32.s1 = -0xfffffffi32;
                        prev_p = r;
                        p = (*mem.offset(r as isize)).b32.s1;
                        /* "Extract any 'invisible' nodes from the old list
                         * and insert them after the new node, so we don't
                         * lose them altogether. Note that the first node
                         * cannot be one of these, as we always start merging
                         * at a native_word node." */
                        while p != -0xfffffffi32 {
                            if !is_char_node(p)
                                && ((*mem.offset(p as isize)).b16.s1 as i32 == 12i32
                                    || (*mem.offset(p as isize)).b16.s1 as i32 == 3i32
                                    || (*mem.offset(p as isize)).b16.s1 as i32 == 4i32
                                    || (*mem.offset(p as isize)).b16.s1 as i32 == 5i32
                                    || (*mem.offset(p as isize)).b16.s1 as i32 == 8i32
                                        && (*mem.offset(p as isize)).b16.s0 as i32 <= 4i32)
                            {
                                (*mem.offset(prev_p as isize)).b32.s1 =
                                    (*mem.offset(p as isize)).b32.s1;
                                (*mem.offset(p as isize)).b32.s1 = (*mem.offset(q as isize)).b32.s1;
                                (*mem.offset(q as isize)).b32.s1 = p;
                                q = p
                            }
                            prev_p = p;
                            p = (*mem.offset(p as isize)).b32.s1
                        }
                        flush_node_list(r);
                        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
                        p = q
                    }
                }
                prev_p = p
            }
            p = (*mem.offset(p as isize)).b32.s1
        }
    }
    /* ... resuming 639 ... */
    p = (*mem.offset((this_box + 5i32) as isize)).b32.s1; /* this is list_offset, the offset of the box list pointer */
    cur_s += 1;
    if cur_s > 0i32 {
        dvi_out(141i32 as eight_bits);
    }
    if cur_s > max_push {
        max_push = cur_s
    }
    save_loc = dvi_offset + dvi_ptr;
    base_line = cur_v;
    prev_p = this_box + 5i32;
    /*1501: "Initialize hlist_out for mixed direction typesetting" */
    temp_ptr = get_avail();
    (*mem.offset(temp_ptr as isize)).b32.s0 = 0i32;
    (*mem.offset(temp_ptr as isize)).b32.s1 = LR_ptr;
    LR_ptr = temp_ptr;
    if (*mem.offset(this_box as isize)).b16.s0 as i32 == 2i32 {
        if cur_dir as i32 == 1i32 {
            cur_dir = 0i32 as small_number;
            cur_h -= (*mem.offset((this_box + 1i32) as isize)).b32.s1
        } else {
            (*mem.offset(this_box as isize)).b16.s0 = 0_u16
        }
    }
    if cur_dir as i32 == 1i32 && (*mem.offset(this_box as isize)).b16.s0 as i32 != 1i32 {
        /*1508: "Reverse the complete hlist and set the subtype to reversed." */
        save_h = cur_h; /* "SyncTeX: do nothing, it is too late" */
        temp_ptr = p;
        p = new_kern(0i32);
        (*mem.offset((p + 3i32 - 1i32) as isize)).b32.s0 = 0i32;
        (*mem.offset(prev_p as isize)).b32.s1 = p;
        cur_h = 0i32;
        (*mem.offset(p as isize)).b32.s1 =
            reverse(this_box, -0xfffffffi32, &mut cur_g, &mut cur_glue);
        (*mem.offset((p + 1i32) as isize)).b32.s1 = -cur_h;
        cur_h = save_h;
        (*mem.offset(this_box as isize)).b16.s0 = 1_u16
    }
    /* ... resuming 639 ... */
    left_edge = cur_h;
    synctex_hlist(this_box);
    's_726: while p != -0xfffffffi32 {
        loop
                 /*642: "Output node `p` for `hlist_out` and move to the next node,
        * maintaining the condition `cur_v = base_line`." ... "We ought to
        * give special care to the efficiency [here] since it belongs to TeX's
        * inner loop. When a `char_node` is encountered, we save a little time
        * by processing several nodes in succession[.] The program uses the
        * fact that `set_char_0 = 0`. */
                 {
                if is_char_node(p) {
                    if cur_h != dvi_h {
                        movement(cur_h - dvi_h, 143i32 as eight_bits);
                        dvi_h = cur_h
                    }
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, 157i32 as eight_bits);
                        dvi_v = cur_v
                    }
                    loop  {
                        f =
                            (*mem.offset(p as isize)).b16.s1 as
                                internal_font_number;
                        c = (*mem.offset(p as isize)).b16.s0;
                        if p != 4999999i32 - 12i32 &&
                               !(*font_mapping.offset(f as isize)).is_null() {
                            c =
                                apply_tfm_font_mapping(*font_mapping.offset(f
                                                                                as
                                                                                isize),
                                                       c as i32) as
                                    u16
                        }
                        if f != dvi_f {
                            /*643: "Change font dvi_f to f" */
                            if !*font_used.offset(f as isize) {
                                dvi_font_def(f);
                                *font_used.offset(f as isize) = true
                            }
                            if f <= 64i32 {
                                dvi_out((f + 171i32 - 1i32) as eight_bits);
                            } else if f <= 256i32 {
                                dvi_out(235i32 as eight_bits);
                                dvi_out((f - 1i32) as eight_bits);
                            } else {
                                dvi_out((235i32 + 1i32) as eight_bits);
                                dvi_out(((f - 1i32) / 256i32) as eight_bits);
                                dvi_out(((f - 1i32) % 256i32) as eight_bits);
                            }
                            dvi_f = f
                        }
                        if *font_ec.offset(f as isize) as i32 >=
                               c as i32 {
                            if *font_bc.offset(f as isize) as i32 <=
                                   c as i32 {
                                if (*font_info.offset((*char_base.offset(f as
                                                                             isize)
                                                           + c as i32)
                                                          as isize)).b16.s3 as
                                       i32 > 0i32 {
                                    /* if (char_exists(orig_char_info(f)(c))) */
                                    if c as i32 >= 128i32 {
                                        dvi_out(128i32 as eight_bits);
                                    }
                                    dvi_out(c as eight_bits);
                                    cur_h +=
                                        (*font_info.offset((*width_base.offset(f
                                                                                   as
                                                                                   isize)
                                                                +
                                                                (*font_info.offset((*char_base.offset(f
                                                                                                          as
                                                                                                          isize)
                                                                                        +
                                                                                        c
                                                                                            as
                                                                                            i32)
                                                                                       as
                                                                                       isize)).b16.s3
                                                                    as
                                                                    i32)
                                                               as
                                                               isize)).b32.s1
                                }
                            }
                        }
                        prev_p = (*mem.offset(prev_p as isize)).b32.s1;
                        p = (*mem.offset(p as isize)).b32.s1;
                        if !is_char_node(p) { break ; }
                    }
                    synctex_current();
                    dvi_h = cur_h;
                    continue 's_726 ;
                } else {
                    /*644: "Output the non-char_node `p` and move to the next node" */
                    match (*mem.offset(p as isize)).b16.s1 as i32 {
                        0 | 1 => {
                            if (*mem.offset((p + 5i32) as isize)).b32.s1 ==
                                   -0xfffffffi32 {
                                if (*mem.offset(p as isize)).b16.s1 as
                                       i32 == 1i32 {
                                    synctex_void_vlist(p, this_box);
                                } else { synctex_void_hlist(p, this_box); }
                                cur_h +=
                                    (*mem.offset((p + 1i32) as isize)).b32.s1
                            } else {
                                save_h = dvi_h;
                                save_v = dvi_v;
                                cur_v =
                                    base_line +
                                        (*mem.offset((p + 4i32) as
                                                         isize)).b32.s1;
                                temp_ptr = p;
                                edge =
                                    cur_h +
                                        (*mem.offset((p + 1i32) as
                                                         isize)).b32.s1;
                                if cur_dir as i32 == 1i32 {
                                    cur_h = edge
                                }
                                if (*mem.offset(p as isize)).b16.s1 as
                                       i32 == 1i32 {
                                    vlist_out();
                                } else { hlist_out(); }
                                dvi_h = save_h;
                                dvi_v = save_v;
                                cur_h = edge;
                                cur_v = base_line
                            }
                            current_block = 13889995436552222973;
                            break ;
                        }
                        2 => {
                            rule_ht =
                                (*mem.offset((p + 3i32) as isize)).b32.s1;
                            rule_dp =
                                (*mem.offset((p + 2i32) as isize)).b32.s1;
                            rule_wd =
                                (*mem.offset((p + 1i32) as isize)).b32.s1;
                            current_block = 18357984655869314713;
                            break ;
                        }
                        8 => {
                            /*1407: "Output the whatsit node p in an hlist" */
                            match (*mem.offset(p as isize)).b16.s0 as
                                      i32 {
                                40 | 41 | 42 => {
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h,
                                                 143i32 as
                                                     eight_bits); /* glyph count */
                                        dvi_h = cur_h
                                    } /* x offset, as fixed-point */
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v,
                                                 157i32 as
                                                     eight_bits); /* y offset, as fixed-point */
                                        dvi_v = cur_v
                                    } /* end of WHATSIT_NODE case */
                                    f =
                                        (*mem.offset((p + 4i32) as
                                                         isize)).b16.s2 as
                                            internal_font_number;
                                    if f != dvi_f {
                                        if !*font_used.offset(f as isize) {
                                            dvi_font_def(f);
                                            *font_used.offset(f as isize) =
                                                true
                                        }
                                        if f <= 64i32 {
                                            dvi_out((f + 170i32) as
                                                        eight_bits);
                                        } else if f <= 256i32 {
                                            dvi_out(235i32 as eight_bits);
                                            dvi_out((f - 1i32) as eight_bits);
                                        } else {
                                            dvi_out((235i32 + 1i32) as
                                                        eight_bits);
                                            dvi_out(((f - 1i32) / 256i32) as
                                                        eight_bits);
                                            dvi_out(((f - 1i32) % 256i32) as
                                                        eight_bits);
                                        }
                                        dvi_f = f
                                    }
                                    if (*mem.offset(p as isize)).b16.s0 as
                                           i32 == 42i32 {
                                        dvi_out(253i32 as eight_bits);
                                        dvi_four((*mem.offset((p + 1i32) as
                                                                  isize)).b32.s1);
                                        dvi_two(1i32 as UTF16_code);
                                        dvi_four(0i32);
                                        dvi_four(0i32);
                                        dvi_two((*mem.offset((p + 4i32) as
                                                                 isize)).b16.s1);
                                        cur_h +=
                                            (*mem.offset((p + 1i32) as
                                                             isize)).b32.s1
                                    } else {
                                        if (*mem.offset(p as isize)).b16.s0 as
                                               i32 == 41i32 {
                                            if (*mem.offset((p + 4i32) as
                                                                isize)).b16.s1
                                                   as i32 > 0i32 ||
                                                   !(*mem.offset((p + 5i32) as
                                                                     isize)).ptr.is_null()
                                               {
                                                dvi_out(254i32 as eight_bits);
                                                len =
                                                    (*mem.offset((p + 4i32) as
                                                                     isize)).b16.s1
                                                        as i32;
                                                dvi_two(len as UTF16_code);
                                                k = 0i32;
                                                while k < len {
                                                    dvi_two(*(&mut *mem.offset((p
                                                                                    +
                                                                                    6i32)
                                                                                   as
                                                                                   isize)
                                                                  as
                                                                  *mut memory_word
                                                                  as
                                                                  *mut u16).offset(k
                                                                                                  as
                                                                                                  isize));
                                                    k += 1
                                                }
                                                len =
                                                    makeXDVGlyphArrayData(&mut *mem.offset(p
                                                                                               as
                                                                                               isize)
                                                                              as
                                                                              *mut memory_word
                                                                              as
                                                                              *mut libc::c_void);
                                                k = 0i32;
                                                while k < len {
                                                    dvi_out(*xdv_buffer.offset(k
                                                                                   as
                                                                                   isize)
                                                                as
                                                                eight_bits);
                                                    k += 1
                                                }
                                            }
                                        } else if !(*mem.offset((p + 5i32) as
                                                                    isize)).ptr.is_null()
                                         {
                                            dvi_out(253i32 as eight_bits);
                                            len =
                                                makeXDVGlyphArrayData(&mut *mem.offset(p
                                                                                           as
                                                                                           isize)
                                                                          as
                                                                          *mut memory_word
                                                                          as
                                                                          *mut libc::c_void);
                                            k = 0i32;
                                            while k < len {
                                                dvi_out(*xdv_buffer.offset(k
                                                                               as
                                                                               isize)
                                                            as eight_bits);
                                                k += 1
                                            }
                                        }
                                        cur_h +=
                                            (*mem.offset((p + 1i32) as
                                                             isize)).b32.s1
                                    }
                                    dvi_h = cur_h
                                }
                                43 | 44 => {
                                    save_h = dvi_h;
                                    save_v = dvi_v;
                                    cur_v = base_line;
                                    edge =
                                        cur_h +
                                            (*mem.offset((p + 1i32) as
                                                             isize)).b32.s1;
                                    pic_out(p);
                                    dvi_h = save_h;
                                    dvi_v = save_v;
                                    cur_h = edge;
                                    cur_v = base_line
                                }
                                6 => {
                                    pdf_last_x_pos = cur_h + cur_h_offset;
                                    pdf_last_y_pos =
                                        cur_page_height - cur_v - cur_v_offset
                                }
                                _ => { out_what(p); }
                            }
                            current_block = 13889995436552222973;
                            break ;
                        }
                        10 => {
                            /*647: "Move right or output leaders" */
                            g = (*mem.offset((p + 1i32) as isize)).b32.s0;
                            rule_wd =
                                (*mem.offset((g + 1i32) as isize)).b32.s1 -
                                    cur_g;
                            if g_sign as i32 != 0i32 {
                                if g_sign as i32 == 1i32 {
                                    if (*mem.offset(g as isize)).b16.s1 as
                                           i32 ==
                                           g_order as i32 {
                                        cur_glue +=
                                            (*mem.offset((g + 2i32) as
                                                             isize)).b32.s1 as
                                                f64;
                                        glue_temp =
                                            (*mem.offset((this_box + 6i32) as
                                                             isize)).gr *
                                                cur_glue;
                                        if glue_temp > 1000000000.0f64 {
                                            glue_temp = 1000000000.0f64
                                        } else if glue_temp < -1000000000.0f64
                                         {
                                            glue_temp = -1000000000.0f64
                                        }
                                        cur_g = tex_round(glue_temp)
                                    }
                                } else if (*mem.offset(g as isize)).b16.s0 as
                                              i32 ==
                                              g_order as i32 {
                                    cur_glue -=
                                        (*mem.offset((g + 3i32) as
                                                         isize)).b32.s1 as
                                            f64;
                                    glue_temp =
                                        (*mem.offset((this_box + 6i32) as
                                                         isize)).gr *
                                            cur_glue;
                                    if glue_temp > 1000000000.0f64 {
                                        glue_temp = 1000000000.0f64
                                    } else if glue_temp < -1000000000.0f64 {
                                        glue_temp = -1000000000.0f64
                                    }
                                    cur_g = tex_round(glue_temp)
                                }
                            }
                            rule_wd += cur_g;
                            /*1486: "Handle a glue node for mixed direction typesetting". */
                            if g_sign as i32 == 1i32 &&
                                   (*mem.offset(g as isize)).b16.s1 as
                                       i32 == g_order as i32
                                   ||
                                   g_sign as i32 == 2i32 &&
                                       (*mem.offset(g as isize)).b16.s0 as
                                           i32 ==
                                           g_order as i32 {
                                if (*mem.offset(g as isize)).b32.s1 ==
                                       -0xfffffffi32 {
                                    free_node(g,
                                              4i32); /* "will never match" */
                                } else {
                                    let ref mut fresh2 =
                                        (*mem.offset(g as isize)).b32.s1;
                                    *fresh2 -= 1
                                }
                                if ((*mem.offset(p as isize)).b16.s0 as
                                        i32) < 100i32 {
                                    (*mem.offset(p as isize)).b16.s1 =
                                        11_u16;
                                    (*mem.offset((p + 1i32) as isize)).b32.s1
                                        = rule_wd
                                } else {
                                    g = get_node(4i32);
                                    (*mem.offset(g as isize)).b16.s1 =
                                        (3i32 + 1i32) as u16;
                                    (*mem.offset(g as isize)).b16.s0 =
                                        (3i32 + 1i32) as u16;
                                    (*mem.offset((g + 1i32) as isize)).b32.s1
                                        = rule_wd;
                                    (*mem.offset((g + 2i32) as isize)).b32.s1
                                        = 0i32;
                                    (*mem.offset((g + 3i32) as isize)).b32.s1
                                        = 0i32;
                                    (*mem.offset((p + 1i32) as isize)).b32.s0
                                        = g
                                }
                            }
                            if (*mem.offset(p as isize)).b16.s0 as i32
                                   >= 100i32 {
                                current_block = 14898553815918780345;
                                break ;
                            } else {
                                current_block = 7364881209357675324;
                                break ;
                            }
                        }
                        40 => {
                            cur_h +=
                                (*mem.offset((p + 1i32) as isize)).b32.s1;
                            current_block = 13889995436552222973;
                            break ;
                        }
                        11 => {
                            synctex_kern(p, this_box);
                            cur_h +=
                                (*mem.offset((p + 1i32) as isize)).b32.s1;
                            current_block = 13889995436552222973;
                            break ;
                        }
                        9 => {
                            synctex_math(p, this_box);
                            /* 1504: "Adjust the LR stack...; if necessary reverse and
                 * hlist segment and goto reswitch." "Breaking a paragraph
                 * into lines while TeXXeT is disabled may result in lines
                 * with unpaired math nodes. Such hlists are silently accepted
                 * in the absence of text direction directives." */
                            if (*mem.offset(p as isize)).b16.s0 as i32
                                   & 1i32 != 0 {
                                /* <= this is end_LR(p) */
                                if (*mem.offset(LR_ptr as isize)).b32.s0 ==
                                       4i32 *
                                           ((*mem.offset(p as isize)).b16.s0
                                                as i32 / 4i32) + 3i32
                                   {
                                    temp_ptr = LR_ptr;
                                    LR_ptr =
                                        (*mem.offset(temp_ptr as
                                                         isize)).b32.s1;
                                    (*mem.offset(temp_ptr as isize)).b32.s1 =
                                        avail;
                                    avail = temp_ptr
                                } else if (*mem.offset(p as isize)).b16.s0 as
                                              i32 > 4i32 {
                                    LR_problems += 1
                                }
                                current_block = 330672039582001856;
                                break ;
                            } else {
                                temp_ptr = get_avail();
                                (*mem.offset(temp_ptr as isize)).b32.s0 =
                                    4i32 *
                                        ((*mem.offset(p as isize)).b16.s0 as
                                             i32 / 4i32) + 3i32;
                                (*mem.offset(temp_ptr as isize)).b32.s1 =
                                    LR_ptr;
                                LR_ptr = temp_ptr;
                                if !((*mem.offset(p as isize)).b16.s0 as
                                         i32 / 8i32 !=
                                         cur_dir as i32) {
                                    current_block = 330672039582001856;
                                    break ;
                                }
                                /*1509: "Reverse an hlist segment and goto reswitch" */
                                save_h = cur_h; /* = lig_char(p) */
                                temp_ptr = (*mem.offset(p as isize)).b32.s1;
                                rule_wd =
                                    (*mem.offset((p + 1i32) as isize)).b32.s1;
                                free_node(p, 3i32);
                                cur_dir =
                                    (1i32 - cur_dir as i32) as
                                        small_number;
                                p = new_edge(cur_dir, rule_wd);
                                (*mem.offset(prev_p as isize)).b32.s1 = p;
                                cur_h = cur_h - left_edge + rule_wd;
                                (*mem.offset(p as isize)).b32.s1 =
                                    reverse(this_box,
                                            new_edge((1i32 -
                                                          cur_dir as
                                                              i32) as
                                                         small_number, 0i32),
                                            &mut cur_g, &mut cur_glue);
                                (*mem.offset((p + 2i32) as isize)).b32.s1 =
                                    cur_h;
                                cur_dir =
                                    (1i32 - cur_dir as i32) as
                                        small_number;
                                cur_h = save_h
                            }
                        }
                        6 => {
                            /* 675: "Make node p look like a char_node and goto reswitch" */
                            *mem.offset((4999999i32 - 12i32) as isize) =
                                *mem.offset((p + 1i32) as isize);
                            (*mem.offset((4999999i32 - 12i32) as
                                             isize)).b32.s1 =
                                (*mem.offset(p as isize)).b32.s1;
                            p = 4999999i32 - 12i32;
                            xtx_ligature_present = true
                        }
                        14 => {
                            /*1507: "Cases of hlist_out that arise in mixed direction text only" */
                            cur_h +=
                                (*mem.offset((p + 1i32) as isize)).b32.s1;
                            left_edge =
                                cur_h +
                                    (*mem.offset((p + 2i32) as isize)).b32.s1;
                            cur_dir =
                                (*mem.offset(p as isize)).b16.s0 as
                                    small_number;
                            current_block = 13889995436552222973;
                            break ;
                        }
                        _ => { current_block = 13889995436552222973; break ; }
                    }
                }
            }
        match current_block {
            14898553815918780345 => {
                /*648: "Output leaders into an hlist, goto fin_rule if a
                 * rule or next_p if done." */
                leader_box = (*mem.offset((p + 1i32) as isize)).b32.s1; /* "compensate for floating-point rounding" ?? */
                if (*mem.offset(leader_box as isize)).b16.s1 as i32 == 2i32 {
                    rule_ht = (*mem.offset((leader_box + 3i32) as isize)).b32.s1;
                    rule_dp = (*mem.offset((leader_box + 2i32) as isize)).b32.s1;
                    current_block = 18357984655869314713;
                } else {
                    leader_wd = (*mem.offset((leader_box + 1i32) as isize)).b32.s1;
                    if leader_wd > 0i32 && rule_wd > 0i32 {
                        rule_wd += 10i32;
                        if cur_dir as i32 == 1i32 {
                            cur_h -= 10i32
                        }
                        edge = cur_h + rule_wd;
                        lx = 0i32;
                        /*649: "Let cur_h be the position of the first pox,
                         * and set leader_wd + lx to the spacing between
                         * corresponding parts of boxes". Additional
                         * explanator comments in XTTP. */
                        if (*mem.offset(p as isize)).b16.s0 as i32 == 100i32 {
                            save_h = cur_h;
                            cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);
                            if cur_h < save_h {
                                cur_h = cur_h + leader_wd
                            }
                        } else {
                            lq = rule_wd / leader_wd;
                            lr = rule_wd % leader_wd;
                            if (*mem.offset(p as isize)).b16.s0 as i32 == 101i32 {
                                cur_h = cur_h + lr / 2i32
                            } else {
                                lx = lr / (lq + 1i32);
                                cur_h = cur_h + (lr - (lq - 1i32) * lx) / 2i32
                            }
                        }
                        while cur_h + leader_wd <= edge {
                            /*650: "Output a leader box at cur_h, then advance cur_h by leader_wd + lx" */
                            cur_v = base_line + (*mem.offset((leader_box + 4i32) as isize)).b32.s1;
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, 157i32 as eight_bits);
                                dvi_v = cur_v
                            }
                            save_v = dvi_v;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, 143i32 as eight_bits);
                                dvi_h = cur_h
                            }
                            save_h = dvi_h;
                            temp_ptr = leader_box;
                            if cur_dir as i32 == 1i32 {
                                cur_h += leader_wd
                            }
                            outer_doing_leaders = doing_leaders;
                            doing_leaders = true;
                            if (*mem.offset(leader_box as isize)).b16.s1 as i32 == 1i32 {
                                vlist_out();
                            } else {
                                hlist_out();
                            }
                            doing_leaders = outer_doing_leaders;
                            dvi_v = save_v;
                            dvi_h = save_h;
                            cur_v = base_line;
                            cur_h = save_h + leader_wd + lx
                        }
                        if cur_dir as i32 == 1i32 {
                            cur_h = edge
                        } else {
                            cur_h = edge - 10i32
                        }
                        current_block = 13889995436552222973;
                    } else {
                        current_block = 7364881209357675324;
                    }
                }
            }
            330672039582001856 => {
                (*mem.offset(p as isize)).b16.s1 = 11_u16;
                cur_h += (*mem.offset((p + 1i32) as isize)).b32.s1;
                current_block = 13889995436552222973;
            }
            _ => {}
        }
        match current_block {
            18357984655869314713 => {
                /*646: "Output a rule in an hlist" */
                if rule_ht == -0x40000000i32 {
                    rule_ht = (*mem.offset((this_box + 3i32) as isize)).b32.s1
                }
                if rule_dp == -0x40000000i32 {
                    rule_dp = (*mem.offset((this_box + 2i32) as isize)).b32.s1
                }
                rule_ht += rule_dp;
                if rule_ht > 0i32 && rule_wd > 0i32 {
                    if cur_h != dvi_h {
                        movement(cur_h - dvi_h, 143i32 as eight_bits);
                        dvi_h = cur_h
                    }
                    cur_v = base_line + rule_dp;
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, 157i32 as eight_bits);
                        dvi_v = cur_v
                    }
                    dvi_out(132i32 as eight_bits);
                    dvi_four(rule_ht);
                    dvi_four(rule_wd);
                    cur_v = base_line;
                    dvi_h += rule_wd
                }
                current_block = 7364881209357675324;
            }
            _ => {}
        }
        match current_block {
            7364881209357675324 =>
            /* ... resuming 644 ... */
            {
                cur_h += rule_wd; /* end GLUE_NODE case */
                synctex_horizontal_rule_or_glue(p, this_box);
            }
            _ => {}
        }
        prev_p = p;
        p = (*mem.offset(p as isize)).b32.s1
    }
    synctex_tsilh(this_box);
    /*1502: "Finish hlist_out for mixed direction typesetting" */
    /*1505: "Check for LR anomalies" */
    while (*mem.offset(LR_ptr as isize)).b32.s0 != 0i32 {
        if (*mem.offset(LR_ptr as isize)).b32.s0 > 4i32 {
            LR_problems += 10000i32
        }
        temp_ptr = LR_ptr;
        LR_ptr = (*mem.offset(temp_ptr as isize)).b32.s1;
        (*mem.offset(temp_ptr as isize)).b32.s1 = avail;
        avail = temp_ptr
    }
    temp_ptr = LR_ptr;
    LR_ptr = (*mem.offset(temp_ptr as isize)).b32.s1;
    (*mem.offset(temp_ptr as isize)).b32.s1 = avail;
    avail = temp_ptr;
    if (*mem.offset(this_box as isize)).b16.s0 as i32 == 2i32 {
        cur_dir = 1i32 as small_number
    }
    /* ... finishing 639 */
    prune_movements(save_loc);
    if cur_s > 0i32 {
        dvi_pop(save_loc);
    }
    cur_s -= 1;
}
/*651: "When vlist_out is called, its duty is to output the box represented by
 * the vlist_node pointed to by temp_ptr. The reference point of that box has
 * coordinates (cur_h, cur_v)." */
unsafe extern "C" fn vlist_out() {
    let mut current_block: u64;
    let mut left_edge: scaled_t = 0;
    let mut top_edge: scaled_t = 0;
    let mut save_h: scaled_t = 0;
    let mut save_v: scaled_t = 0;
    let mut this_box: i32 = 0;
    let mut g_order: glue_ord = 0;
    let mut g_sign: u8 = 0;
    let mut p: i32 = 0;
    let mut save_loc: i32 = 0;
    let mut leader_box: i32 = 0;
    let mut leader_ht: scaled_t = 0;
    let mut lx: scaled_t = 0;
    let mut outer_doing_leaders: bool = false;
    let mut edge: scaled_t = 0;
    let mut glue_temp: f64 = 0.;
    let mut cur_glue: f64 = 0.;
    let mut cur_g: scaled_t = 0;
    let mut upwards: bool = false;
    let mut f: internal_font_number = 0;
    cur_g = 0i32;
    cur_glue = 0.0f64;
    this_box = temp_ptr;
    g_order = (*mem.offset((this_box + 5i32) as isize)).b16.s0 as glue_ord;
    g_sign = (*mem.offset((this_box + 5i32) as isize)).b16.s1 as u8;
    p = (*mem.offset((this_box + 5i32) as isize)).b32.s1;
    upwards = (*mem.offset(this_box as isize)).b16.s0 as i32 == 1i32;
    cur_s += 1;
    if cur_s > 0i32 {
        dvi_out(141i32 as eight_bits);
    }
    if cur_s > max_push {
        max_push = cur_s
    }
    save_loc = dvi_offset + dvi_ptr;
    left_edge = cur_h;
    synctex_vlist(this_box);
    if upwards {
        cur_v += (*mem.offset((this_box + 2i32) as isize)).b32.s1
    } else {
        cur_v -= (*mem.offset((this_box + 3i32) as isize)).b32.s1
    }
    top_edge = cur_v;
    while p != -0xfffffffi32 {
        /*652: "Output node p and move to the next node, maintaining the
         * condition cur_h = left_edge" */
        if is_char_node(p) {
            confusion(b"vlistout\x00" as *const u8 as *const i8);
        } else {
            /*653: "Output the non-char_node p" */
            match (*mem.offset(p as isize)).b16.s1 as i32 {
                0 | 1 => {
                    /*654: "Output a box in a vlist" */
                    if (*mem.offset((p + 5i32) as isize)).b32.s1 == -0xfffffffi32 {
                        if upwards {
                            cur_v -= (*mem.offset((p + 2i32) as isize)).b32.s1
                        } else {
                            cur_v += (*mem.offset((p + 3i32) as isize)).b32.s1
                        }
                        if (*mem.offset(p as isize)).b16.s1 as i32 == 1i32 {
                            synctex_void_vlist(p, this_box);
                        } else {
                            synctex_void_hlist(p, this_box);
                        }
                        if upwards {
                            cur_v -= (*mem.offset((p + 3i32) as isize)).b32.s1
                        } else {
                            cur_v += (*mem.offset((p + 2i32) as isize)).b32.s1
                        }
                    } else {
                        if upwards {
                            cur_v -= (*mem.offset((p + 2i32) as isize)).b32.s1
                        } else {
                            cur_v += (*mem.offset((p + 3i32) as isize)).b32.s1
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, 157i32 as eight_bits);
                            dvi_v = cur_v
                        }
                        save_h = dvi_h;
                        save_v = dvi_v;
                        if cur_dir as i32 == 1i32 {
                            cur_h = left_edge - (*mem.offset((p + 4i32) as isize)).b32.s1
                        } else {
                            cur_h = left_edge + (*mem.offset((p + 4i32) as isize)).b32.s1
                        }
                        temp_ptr = p;
                        if (*mem.offset(p as isize)).b16.s1 as i32 == 1i32 {
                            vlist_out();
                        } else {
                            hlist_out();
                        }
                        dvi_h = save_h;
                        dvi_v = save_v;
                        if upwards {
                            cur_v = save_v - (*mem.offset((p + 3i32) as isize)).b32.s1
                        } else {
                            cur_v = save_v + (*mem.offset((p + 2i32) as isize)).b32.s1
                        }
                        cur_h = left_edge
                    }
                    current_block = 5241535548500397784;
                }
                2 => {
                    rule_ht = (*mem.offset((p + 3i32) as isize)).b32.s1;
                    rule_dp = (*mem.offset((p + 2i32) as isize)).b32.s1;
                    rule_wd = (*mem.offset((p + 1i32) as isize)).b32.s1;
                    current_block = 9653381107620864133;
                }
                8 => {
                    /*1403: "Output the whatsit node p in a vlist" */
                    match (*mem.offset(p as isize)).b16.s0 as i32 {
                        42 => {
                            cur_v = cur_v + (*mem.offset((p + 3i32) as isize)).b32.s1;
                            cur_h = left_edge;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, 143i32 as eight_bits);
                                dvi_h = cur_h
                            }
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, 157i32 as eight_bits);
                                dvi_v = cur_v
                            }
                            f = (*mem.offset((p + 4i32) as isize)).b16.s2 as internal_font_number;
                            if f != dvi_f {
                                /*643:*/
                                if !*font_used.offset(f as isize) {
                                    dvi_font_def(f); /* width */
                                    *font_used.offset(f as isize) = true
                                } /* glyph count */
                                if f <= 64i32 {
                                    dvi_out((f + 170i32) as eight_bits); /* x offset as fixed-point */
                                } else if f <= 256i32 {
                                    dvi_out(235i32 as eight_bits); /* y offset as fixed-point */
                                    dvi_out((f - 1i32) as eight_bits);
                                } else {
                                    dvi_out((235i32 + 1i32) as eight_bits);
                                    dvi_out(((f - 1i32) / 256i32) as eight_bits);
                                    dvi_out(((f - 1i32) % 256i32) as eight_bits);
                                }
                                dvi_f = f
                            }
                            dvi_out(253i32 as eight_bits);
                            dvi_four(0i32);
                            dvi_two(1i32 as UTF16_code);
                            dvi_four(0i32);
                            dvi_four(0i32);
                            dvi_two((*mem.offset((p + 4i32) as isize)).b16.s1);
                            cur_v += (*mem.offset((p + 2i32) as isize)).b32.s1;
                            cur_h = left_edge
                        }
                        43 | 44 => {
                            save_h = dvi_h;
                            save_v = dvi_v;
                            cur_v = cur_v + (*mem.offset((p + 3i32) as isize)).b32.s1;
                            pic_out(p);
                            dvi_h = save_h;
                            dvi_v = save_v;
                            cur_v = save_v + (*mem.offset((p + 2i32) as isize)).b32.s1;
                            cur_h = left_edge
                        }
                        6 => {
                            pdf_last_x_pos = cur_h + cur_h_offset;
                            pdf_last_y_pos = cur_page_height - cur_v - cur_v_offset
                        }
                        _ => {
                            out_what(p);
                        }
                    }
                    current_block = 5241535548500397784;
                }
                10 => {
                    /*656: "Move down or output leaders" */
                    g = (*mem.offset((p + 1i32) as isize)).b32.s0;
                    rule_ht = (*mem.offset((g + 1i32) as isize)).b32.s1 - cur_g;
                    if g_sign as i32 != 0i32 {
                        if g_sign as i32 == 1i32 {
                            if (*mem.offset(g as isize)).b16.s1 as i32 == g_order as i32 {
                                cur_glue += (*mem.offset((g + 2i32) as isize)).b32.s1 as f64;
                                glue_temp = (*mem.offset((this_box + 6i32) as isize)).gr * cur_glue;
                                if glue_temp > 1000000000.0f64 {
                                    glue_temp = 1000000000.0f64
                                } else if glue_temp < -1000000000.0f64 {
                                    glue_temp = -1000000000.0f64
                                }
                                cur_g = tex_round(glue_temp)
                            }
                        } else if (*mem.offset(g as isize)).b16.s0 as i32 == g_order as i32 {
                            cur_glue -= (*mem.offset((g + 3i32) as isize)).b32.s1 as f64;
                            glue_temp = (*mem.offset((this_box + 6i32) as isize)).gr * cur_glue;
                            if glue_temp > 1000000000.0f64 {
                                glue_temp = 1000000000.0f64
                            } else if glue_temp < -1000000000.0f64 {
                                glue_temp = -1000000000.0f64
                            }
                            cur_g = tex_round(glue_temp)
                        }
                    }
                    rule_ht += cur_g;
                    if (*mem.offset(p as isize)).b16.s0 as i32 >= 100i32 {
                        /*657: "Output leaders in a vlist, goto fin_rule if a rule
                         * or next_p if done" */
                        leader_box = (*mem.offset((p + 1i32) as isize)).b32.s1; /* "compensate for floating-point rounding" */
                        if (*mem.offset(leader_box as isize)).b16.s1 as i32 == 2i32 {
                            rule_wd = (*mem.offset((leader_box + 1i32) as isize)).b32.s1;
                            rule_dp = 0i32;
                            current_block = 9653381107620864133;
                        } else {
                            leader_ht = (*mem.offset((leader_box + 3i32) as isize)).b32.s1
                                + (*mem.offset((leader_box + 2i32) as isize)).b32.s1;
                            if leader_ht > 0i32 && rule_ht > 0i32 {
                                rule_ht += 10i32;
                                edge = cur_v + rule_ht;
                                lx = 0i32;
                                /*658: "Let cur_v be the position of the first box,
                                 * and set leader_ht + lx to the spacing between
                                 * corresponding parts of boxes" */
                                if (*mem.offset(p as isize)).b16.s0 as i32 == 100i32 {
                                    save_v = cur_v;
                                    cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);
                                    if cur_v < save_v {
                                        cur_v = cur_v + leader_ht
                                    }
                                } else {
                                    lq = rule_ht / leader_ht;
                                    lr = rule_ht % leader_ht;
                                    if (*mem.offset(p as isize)).b16.s0 as i32 == 101i32 {
                                        cur_v = cur_v + lr / 2i32
                                    } else {
                                        lx = lr / (lq + 1i32);
                                        cur_v = cur_v + (lr - (lq - 1i32) * lx) / 2i32
                                    }
                                }
                                while cur_v + leader_ht <= edge {
                                    /*659: "Output a leader box at cur_v, then advance
                                     * cur_v by leader_ht + lx". "When we reach this
                                     * part of the program, cur_v indicates the top of
                                     * a leader box, not its baseline." */
                                    if cur_dir as i32 == 1i32 {
                                        cur_h = left_edge
                                            - (*mem.offset((leader_box + 4i32) as isize)).b32.s1
                                    } else {
                                        cur_h = left_edge
                                            + (*mem.offset((leader_box + 4i32) as isize)).b32.s1
                                    }
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h, 143i32 as eight_bits);
                                        dvi_h = cur_h
                                    }
                                    save_h = dvi_h;
                                    cur_v += (*mem.offset((leader_box + 3i32) as isize)).b32.s1;
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v, 157i32 as eight_bits);
                                        dvi_v = cur_v
                                    }
                                    save_v = dvi_v;
                                    temp_ptr = leader_box;
                                    outer_doing_leaders = doing_leaders;
                                    doing_leaders = true;
                                    if (*mem.offset(leader_box as isize)).b16.s1 as i32 == 1i32 {
                                        vlist_out();
                                    } else {
                                        hlist_out();
                                    }
                                    doing_leaders = outer_doing_leaders;
                                    dvi_v = save_v;
                                    dvi_h = save_h;
                                    cur_h = left_edge;
                                    cur_v = save_v
                                        - (*mem.offset((leader_box + 3i32) as isize)).b32.s1
                                        + leader_ht
                                        + lx
                                }
                                cur_v = edge - 10i32;
                                current_block = 5241535548500397784;
                            } else {
                                current_block = 5246966788635068203;
                            }
                        }
                    } else {
                        current_block = 5246966788635068203;
                    }
                    match current_block {
                        5241535548500397784 => {}
                        9653381107620864133 => {}
                        _ => {
                            if upwards {
                                cur_v -= rule_ht
                            } else {
                                cur_v += rule_ht
                            }
                            current_block = 5241535548500397784;
                        }
                    }
                }
                11 => {
                    if upwards {
                        cur_v -= (*mem.offset((p + 1i32) as isize)).b32.s1
                    } else {
                        cur_v += (*mem.offset((p + 1i32) as isize)).b32.s1
                    }
                    current_block = 5241535548500397784;
                }
                _ => {
                    current_block = 5241535548500397784;
                }
            }
            match current_block {
                9653381107620864133 => {
                    // 655: "Output a rule in a vlist, goto next_p

                    if rule_wd == -0x40000000i32 {
                        rule_wd = (*mem.offset((this_box + 1i32) as isize)).b32.s1
                    } /* end WHATSIT_NODE case */
                    rule_ht += rule_dp;
                    if upwards {
                        cur_v -= rule_ht
                    } else {
                        cur_v += rule_ht
                    }
                    if rule_ht > 0i32 && rule_wd > 0i32 {
                        if cur_dir as i32 == 1i32 {
                            cur_h -= rule_wd
                        }
                        if cur_h != dvi_h {
                            movement(cur_h - dvi_h, 143i32 as eight_bits);
                            dvi_h = cur_h
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, 157i32 as eight_bits);
                            dvi_v = cur_v
                        }
                        dvi_out(137i32 as eight_bits);
                        dvi_four(rule_ht);
                        dvi_four(rule_wd);
                        cur_h = left_edge
                    }
                }
                _ => {}
            }
            p = (*mem.offset(p as isize)).b32.s1
        }
    }
    synctex_tsilv(this_box);
    prune_movements(save_loc);
    if cur_s > 0i32 {
        dvi_pop(save_loc);
    }
    cur_s -= 1;
}
/*1510: "The reverse function defined here is responsible for reversing the
 * nodes of an hlist (segment). this_box is the enclosing hlist_node; t is to
 * become the tail of the reversed list; and the global variable temp_ptr is
 * the head of the list to be reversed. cur_g and cur_glue are the current
 * glue rounding state variables, to be updated by this function. We remove
 * nodes from the original list and add them to the head of the new one."
 */
unsafe extern "C" fn reverse(
    mut this_box: i32,
    mut t: i32,
    mut cur_g: *mut scaled_t,
    mut cur_glue: *mut f64,
) -> i32 {
    let mut current_block: u64;
    let mut l: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut g_order: glue_ord = 0;
    let mut g_sign: u8 = 0;
    let mut glue_temp: f64 = 0.;
    let mut m: i32 = 0;
    let mut n: i32 = 0;
    let mut c: u16 = 0;
    let mut f: internal_font_number = 0;
    g_order = (*mem.offset((this_box + 5i32) as isize)).b16.s0 as glue_ord;
    g_sign = (*mem.offset((this_box + 5i32) as isize)).b16.s1 as u8;
    l = t;
    p = temp_ptr;
    m = -0xfffffffi32;
    n = -0xfffffffi32;
    's_58: loop {
        if p != -0xfffffffi32 {
            loop
            /*1511: "Move node p to the new list and go to the next node; or
             * goto done if the end of the reflected segment has been
             * reached." */
            {
                if is_char_node(p) {
                    loop {
                        f = (*mem.offset(p as isize)).b16.s1 as internal_font_number;
                        c = (*mem.offset(p as isize)).b16.s0;
                        cur_h += (*font_info.offset(
                            (*width_base.offset(f as isize)
                                + (*font_info.offset(
                                    (*char_base.offset(f as isize)
                                        + effective_char(1i32 != 0, f, c))
                                        as isize,
                                ))
                                .b16
                                .s3 as i32) as isize,
                        ))
                        .b32
                        .s1;
                        q = (*mem.offset(p as isize)).b32.s1;
                        (*mem.offset(p as isize)).b32.s1 = l;
                        l = p;
                        p = q;
                        if !is_char_node(p) {
                            break;
                        }
                    }
                    continue 's_58;
                } else {
                    q = (*mem.offset(p as isize)).b32.s1;
                    match (*mem.offset(p as isize)).b16.s1 as i32 {
                        0 | 1 | 2 | 11 => {
                            rule_wd = (*mem.offset((p + 1i32) as isize)).b32.s1;
                            current_block = 3812947724376655173;
                            break;
                        }
                        8 => {
                            if (*mem.offset(p as isize)).b16.s0 as i32 == 40i32
                                || (*mem.offset(p as isize)).b16.s0 as i32 == 41i32
                                || (*mem.offset(p as isize)).b16.s0 as i32 == 42i32
                                || (*mem.offset(p as isize)).b16.s0 as i32 == 43i32
                                || (*mem.offset(p as isize)).b16.s0 as i32 == 44i32
                            {
                                current_block = 7056779235015430508;
                                break;
                            } else {
                                current_block = 10883403804712335414;
                                break;
                            }
                        }
                        10 => {
                            /*1486: "Handle a glue node for mixed direction typesetting" */
                            g = (*mem.offset((p + 1i32) as isize)).b32.s0; /* "will never match" */
                            rule_wd = (*mem.offset((g + 1i32) as isize)).b32.s1 - *cur_g; /* = mem[lig_char(temp_ptr)] */
                            if g_sign as i32 != 0i32 {
                                if g_sign as i32 == 1i32 {
                                    if (*mem.offset(g as isize)).b16.s1 as i32 == g_order as i32 {
                                        *cur_glue = *cur_glue
                                            + (*mem.offset((g + 2i32) as isize)).b32.s1 as f64;
                                        glue_temp = (*mem.offset((this_box + 6i32) as isize)).gr
                                            * *cur_glue;
                                        if glue_temp > 1000000000.0f64 {
                                            glue_temp = 1000000000.0f64
                                        } else if glue_temp < -1000000000.0f64 {
                                            glue_temp = -1000000000.0f64
                                        }
                                        *cur_g = tex_round(glue_temp)
                                    }
                                } else if (*mem.offset(g as isize)).b16.s0 as i32 == g_order as i32
                                {
                                    *cur_glue = *cur_glue
                                        - (*mem.offset((g + 3i32) as isize)).b32.s1 as f64;
                                    glue_temp =
                                        (*mem.offset((this_box + 6i32) as isize)).gr * *cur_glue;
                                    if glue_temp > 1000000000.0f64 {
                                        glue_temp = 1000000000.0f64
                                    } else if glue_temp < -1000000000.0f64 {
                                        glue_temp = -1000000000.0f64
                                    }
                                    *cur_g = tex_round(glue_temp)
                                }
                            }
                            rule_wd += *cur_g;
                            if g_sign as i32 == 1i32
                                && (*mem.offset(g as isize)).b16.s1 as i32 == g_order as i32
                                || g_sign as i32 == 2i32
                                    && (*mem.offset(g as isize)).b16.s0 as i32 == g_order as i32
                            {
                                if (*mem.offset(g as isize)).b32.s1 == -0xfffffffi32 {
                                    free_node(g, 4i32);
                                } else {
                                    let ref mut fresh3 = (*mem.offset(g as isize)).b32.s1;
                                    *fresh3 -= 1
                                }
                                if ((*mem.offset(p as isize)).b16.s0 as i32) < 100i32 {
                                    (*mem.offset(p as isize)).b16.s1 = 11_u16;
                                    (*mem.offset((p + 1i32) as isize)).b32.s1 = rule_wd
                                } else {
                                    g = get_node(4i32);
                                    (*mem.offset(g as isize)).b16.s1 = (3i32 + 1i32) as u16;
                                    (*mem.offset(g as isize)).b16.s0 = (3i32 + 1i32) as u16;
                                    (*mem.offset((g + 1i32) as isize)).b32.s1 = rule_wd;
                                    (*mem.offset((g + 2i32) as isize)).b32.s1 = 0i32;
                                    (*mem.offset((g + 3i32) as isize)).b32.s1 = 0i32;
                                    (*mem.offset((p + 1i32) as isize)).b32.s0 = g
                                }
                            }
                            current_block = 3812947724376655173;
                            break;
                        }
                        6 => {
                            flush_node_list((*mem.offset((p + 1i32) as isize)).b32.s1);
                            temp_ptr = p;
                            p = get_avail();
                            *mem.offset(p as isize) = *mem.offset((temp_ptr + 1i32) as isize);
                            (*mem.offset(p as isize)).b32.s1 = q;
                            free_node(temp_ptr, 2i32);
                        }
                        9 => {
                            /*1516: "Math nodes in an inner reflected segment are
                             * modified, those at the outer level are changed into
                             * kern nodes." */
                            rule_wd = (*mem.offset((p + 1i32) as isize)).b32.s1;
                            if (*mem.offset(p as isize)).b16.s0 as i32 & 1i32 != 0 {
                                current_block = 5873035170358615968;
                                break;
                            } else {
                                current_block = 17239133558811367971;
                                break;
                            }
                        }
                        14 => {
                            confusion(b"LR2\x00" as *const u8 as *const i8);
                        }
                        _ => {
                            current_block = 10883403804712335414;
                            break;
                        }
                    }
                }
            }
            match current_block {
                5873035170358615968 => {
                    if (*mem.offset(LR_ptr as isize)).b32.s0
                        != 4i32 * ((*mem.offset(p as isize)).b16.s0 as i32 / 4i32) + 3i32
                    {
                        (*mem.offset(p as isize)).b16.s1 = 11_u16;
                        LR_problems += 1
                    } else {
                        temp_ptr = LR_ptr;
                        LR_ptr = (*mem.offset(temp_ptr as isize)).b32.s1;
                        (*mem.offset(temp_ptr as isize)).b32.s1 = avail;
                        avail = temp_ptr;
                        if n > -0xfffffffi32 {
                            n -= 1;
                            let ref mut fresh4 = (*mem.offset(p as isize)).b16.s0;
                            *fresh4 = (*fresh4).wrapping_sub(1)
                        } else {
                            (*mem.offset(p as isize)).b16.s1 = 11_u16;
                            if m > -0xfffffffi32 {
                                m -= 1
                            } else {
                                /*1517: "Finish the reverse hlist segment and goto done" */
                                free_node(p, 3i32); /* end GLUE_NODE case */
                                (*mem.offset(t as isize)).b32.s1 = q;
                                (*mem.offset((t + 1i32) as isize)).b32.s1 = rule_wd;
                                (*mem.offset((t + 2i32) as isize)).b32.s1 = -cur_h - rule_wd;
                                break;
                            }
                        }
                    }
                    current_block = 3812947724376655173;
                }
                17239133558811367971 => {
                    temp_ptr = get_avail();
                    (*mem.offset(temp_ptr as isize)).b32.s0 =
                        4i32 * ((*mem.offset(p as isize)).b16.s0 as i32 / 4i32) + 3i32;
                    (*mem.offset(temp_ptr as isize)).b32.s1 = LR_ptr;
                    LR_ptr = temp_ptr;
                    if n > -0xfffffffi32
                        || (*mem.offset(p as isize)).b16.s0 as i32 / 8i32 != cur_dir as i32
                    {
                        n += 1;
                        let ref mut fresh5 = (*mem.offset(p as isize)).b16.s0;
                        *fresh5 = (*fresh5).wrapping_add(1)
                    } else {
                        (*mem.offset(p as isize)).b16.s1 = 11_u16;
                        m += 1
                    }
                    current_block = 3812947724376655173;
                }
                7056779235015430508 => {
                    rule_wd = (*mem.offset((p + 1i32) as isize)).b32.s1;
                    current_block = 3812947724376655173;
                }
                _ => {}
            }
            match current_block {
                3812947724376655173 => cur_h += rule_wd,
                _ => {}
            }
            (*mem.offset(p as isize)).b32.s1 = l;
            if (*mem.offset(p as isize)).b16.s1 as i32 == 11i32 {
                if rule_wd == 0i32 || l == -0xfffffffi32 {
                    free_node(p, 3i32);
                    p = l
                }
            }
            l = p;
            p = q
        } else {
            /* ... resuming 1510 ... */
            if t == -0xfffffffi32 && m == -0xfffffffi32 && n == -0xfffffffi32 {
                break; /* "Manufacture a missing math node" */
            }
            p = new_math(0i32, (*mem.offset(LR_ptr as isize)).b32.s0 as small_number);
            LR_problems += 10000i32
        }
    }
    l
}
/*1506: Create a new edge node of subtype `s` and width `w` */
#[no_mangle]
pub unsafe extern "C" fn new_edge(mut s: small_number, mut w: scaled_t) -> i32 {
    let mut p: i32 = 0;
    p = get_node(3i32);
    (*mem.offset(p as isize)).b16.s1 = 14_u16;
    (*mem.offset(p as isize)).b16.s0 = s as u16;
    (*mem.offset((p + 1i32) as isize)).b32.s1 = w;
    (*mem.offset((p + 2i32) as isize)).b32.s1 = 0i32;
    p
}
#[no_mangle]
pub unsafe extern "C" fn out_what(mut p: i32) {
    let mut j: small_number = 0;
    match (*mem.offset(p as isize)).b16.s0 as i32 {
        0 | 1 | 2 => {
            if !doing_leaders {
                j = (*mem.offset((p + 1i32) as isize)).b32.s0 as small_number;
                if (*mem.offset(p as isize)).b16.s0 as i32 == 1i32 {
                    write_out(p);
                } else {
                    if write_open[j as usize] {
                        ttstub_output_close(write_file[j as usize]);
                    }
                    if (*mem.offset(p as isize)).b16.s0 as i32 == 2i32 {
                        write_open[j as usize] = false
                    } else if !(j as i32 >= 16i32) {
                        cur_name = (*mem.offset((p + 1i32) as isize)).b32.s1;
                        cur_area = (*mem.offset((p + 2i32) as isize)).b32.s0;
                        cur_ext = (*mem.offset((p + 2i32) as isize)).b32.s1;
                        if length(cur_ext) == 0i32 {
                            cur_ext = maketexstring(b".tex\x00" as *const u8 as *const i8)
                        }
                        pack_file_name(cur_name, cur_area, cur_ext);
                        write_file[j as usize] = ttstub_output_open(name_of_file, 0i32);
                        if write_file[j as usize].is_null() {
                            _tt_abort(
                                b"cannot open output file \"%s\"\x00" as *const u8 as *const i8,
                                name_of_file,
                            );
                        }
                        write_open[j as usize] = true;
                        if log_opened {
                            let old_setting = selector;
                            if (*eqtb.offset(
                                (1i32
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + 1i32
                                    + 15000i32
                                    + 12i32
                                    + 9000i32
                                    + 1i32
                                    + 1i32
                                    + 19i32
                                    + 256i32
                                    + 256i32
                                    + 13i32
                                    + 256i32
                                    + 4i32
                                    + 256i32
                                    + 1i32
                                    + 3i32 * 256i32
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + 29i32) as isize,
                            ))
                            .b32
                            .s1 <= 0i32
                            {
                                selector = Selector::LOG_ONLY
                            } else {
                                selector = Selector::TERM_AND_LOG
                            }
                            print_nl_cstr(b"\\openout\x00" as *const u8 as *const i8);
                            print_int(j as i32);
                            print_cstr(b" = `\x00" as *const u8 as *const i8);
                            print_file_name(cur_name, cur_area, cur_ext);
                            print_cstr(b"\'.\x00" as *const u8 as *const i8);
                            print_nl_cstr(b"\x00" as *const u8 as *const i8);
                            print_ln();
                            selector = old_setting
                        }
                    }
                }
            }
        }
        3 => {
            special_out(p);
        }
        4 => {}
        _ => {
            confusion(b"ext4\x00" as *const u8 as *const i8);
        }
    };
}
unsafe extern "C" fn dvi_native_font_def(mut f: internal_font_number) {
    let mut font_def_length: i32 = 0;
    let mut i: i32 = 0;
    dvi_out(252i32 as eight_bits);
    dvi_four(f - 1i32);
    font_def_length = make_font_def(f);
    i = 0i32;
    while i < font_def_length {
        dvi_out(*xdv_buffer.offset(i as isize) as eight_bits);
        i += 1
    }
}
unsafe extern "C" fn dvi_font_def(mut f: internal_font_number) {
    let mut k: pool_pointer = 0;
    let mut l: i32 = 0;
    if *font_area.offset(f as isize) as u32 == 0xffffu32
        || *font_area.offset(f as isize) as u32 == 0xfffeu32
    {
        dvi_native_font_def(f);
    } else {
        if f <= 256i32 {
            dvi_out(243i32 as eight_bits);
            dvi_out((f - 1i32) as eight_bits);
        } else {
            dvi_out((243i32 + 1i32) as eight_bits);
            dvi_out(((f - 1i32) / 256i32) as eight_bits);
            dvi_out(((f - 1i32) % 256i32) as eight_bits);
        }
        dvi_out((*font_check.offset(f as isize)).s3 as eight_bits);
        dvi_out((*font_check.offset(f as isize)).s2 as eight_bits);
        dvi_out((*font_check.offset(f as isize)).s1 as eight_bits);
        dvi_out((*font_check.offset(f as isize)).s0 as eight_bits);
        dvi_four(*font_size.offset(f as isize));
        dvi_four(*font_dsize.offset(f as isize));
        dvi_out(length(*font_area.offset(f as isize)) as eight_bits);
        l = 0i32;
        k = *str_start.offset((*font_name.offset(f as isize) as i64 - 65536) as isize);
        while l == 0i32
            && k < *str_start
                .offset(((*font_name.offset(f as isize) + 1i32) as i64 - 65536) as isize)
        {
            if *str_pool.offset(k as isize) as i32 == ':' as i32 {
                l = k - *str_start.offset((*font_name.offset(f as isize) as i64 - 65536) as isize)
            }
            k += 1
        }
        if l == 0i32 {
            l = length(*font_name.offset(f as isize))
        }
        dvi_out(l as eight_bits);
        let mut for_end: i32 = 0;
        k = *str_start.offset((*font_area.offset(f as isize) as i64 - 65536) as isize);
        for_end = *str_start
            .offset(((*font_area.offset(f as isize) + 1i32) as i64 - 65536) as isize)
            - 1i32;
        if k <= for_end {
            loop {
                dvi_out(*str_pool.offset(k as isize) as eight_bits);
                let fresh6 = k;
                k = k + 1;
                if !(fresh6 < for_end) {
                    break;
                }
            }
        }
        let mut for_end_0: i32 = 0;
        k = *str_start.offset((*font_name.offset(f as isize) as i64 - 65536) as isize);
        for_end_0 =
            *str_start.offset((*font_name.offset(f as isize) as i64 - 65536) as isize) + l - 1i32;
        if k <= for_end_0 {
            loop {
                dvi_out(*str_pool.offset(k as isize) as eight_bits);
                let fresh7 = k;
                k = k + 1;
                if !(fresh7 < for_end_0) {
                    break;
                }
            }
        }
    };
}
unsafe extern "C" fn movement(mut w: scaled_t, mut o: eight_bits) {
    let mut current_block: u64;
    let mut mstate: small_number = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut k: i32 = 0;
    q = get_node(3i32);
    (*mem.offset((q + 1i32) as isize)).b32.s1 = w;
    (*mem.offset((q + 2i32) as isize)).b32.s1 = dvi_offset + dvi_ptr;
    if o as i32 == 157i32 {
        (*mem.offset(q as isize)).b32.s1 = down_ptr;
        down_ptr = q
    } else {
        (*mem.offset(q as isize)).b32.s1 = right_ptr;
        right_ptr = q
    }
    p = (*mem.offset(q as isize)).b32.s1;
    mstate = 0i32 as small_number;
    loop {
        if !(p != -0xfffffffi32) {
            current_block = 18071914750955744041;
            break;
        }
        if (*mem.offset((p + 1i32) as isize)).b32.s1 == w {
            /* By this point must be OPEN_NODE */
            /*632:*/
            match mstate as i32 + (*mem.offset(p as isize)).b32.s0 {
                3 | 4 | 15 | 16 => {
                    current_block = 2415380317517078313; /*633:*/
                    match current_block {
                        15378387224937501455 => {
                            if (*mem.offset((p + 2i32) as isize)).b32.s1 < dvi_gone {
                                current_block = 18071914750955744041;
                                break;
                            }
                            k = (*mem.offset((p + 2i32) as isize)).b32.s1 - dvi_offset;
                            if k < 0i32 {
                                k = k + 16384i32
                            }
                            *dvi_buf.offset(k as isize) =
                                (*dvi_buf.offset(k as isize) as i32 + 10i32) as eight_bits;
                            (*mem.offset(p as isize)).b32.s0 = 2i32;
                            current_block = 8542251818650148540;
                            break;
                        }
                        _ => {
                            if (*mem.offset((p + 2i32) as isize)).b32.s1 < dvi_gone {
                                current_block = 18071914750955744041;
                                break;
                            } else {
                                k = (*mem.offset((p + 2i32) as isize)).b32.s1 - dvi_offset;
                                if k < 0i32 {
                                    k = k + 16384i32
                                }
                                *dvi_buf.offset(k as isize) =
                                    (*dvi_buf.offset(k as isize) as i32 + 5i32) as eight_bits;
                                (*mem.offset(p as isize)).b32.s0 = 1i32;
                                current_block = 8542251818650148540;
                                break;
                            }
                        }
                    }
                }
                5 | 9 | 11 => {
                    current_block = 15378387224937501455;
                    if (*mem.offset((p + 2i32) as isize)).b32.s1 < dvi_gone {
                        current_block = 18071914750955744041;
                        break;
                    }
                    k = (*mem.offset((p + 2i32) as isize)).b32.s1 - dvi_offset;
                    if k < 0i32 {
                        k = k + 16384i32
                    }
                    *dvi_buf.offset(k as isize) =
                        (*dvi_buf.offset(k as isize) as i32 + 10i32) as eight_bits;
                    (*mem.offset(p as isize)).b32.s0 = 2i32;
                    current_block = 8542251818650148540;
                    break;
                }
                1 | 2 | 8 | 13 => {
                    current_block = 8542251818650148540;
                    break;
                }
                _ => {}
            }
        } else {
            match mstate as i32 + (*mem.offset(p as isize)).b32.s0 {
                1 => {
                    current_block = 8114521223357534250;
                    mstate = 6i32 as small_number;
                }
                2 => {
                    current_block = 15905285856240674276;
                    mstate = 12i32 as small_number;
                }
                8 | 13 => {
                    current_block = 18071914750955744041;
                    break;
                }
                _ => {}
            }
        }
        p = (*mem.offset(p as isize)).b32.s1
    }
    match current_block {
        8542251818650148540 => {
            /*629:*/
            (*mem.offset(q as isize)).b32.s0 = (*mem.offset(p as isize)).b32.s0; /*634:*/
            if (*mem.offset(q as isize)).b32.s0 == 1i32 {
                dvi_out((o as i32 + 4i32) as eight_bits); /* max_selector enum */
                while (*mem.offset(q as isize)).b32.s1 != p {
                    q = (*mem.offset(q as isize)).b32.s1;
                    match (*mem.offset(q as isize)).b32.s0 {
                        3 => (*mem.offset(q as isize)).b32.s0 = 5i32,
                        4 => (*mem.offset(q as isize)).b32.s0 = 6i32,
                        _ => {}
                    }
                }
            } else {
                dvi_out((o as i32 + 9i32) as eight_bits);
                while (*mem.offset(q as isize)).b32.s1 != p {
                    q = (*mem.offset(q as isize)).b32.s1;
                    match (*mem.offset(q as isize)).b32.s0 {
                        3 => (*mem.offset(q as isize)).b32.s0 = 4i32,
                        5 => (*mem.offset(q as isize)).b32.s0 = 6i32,
                        _ => {}
                    }
                }
            }
            return;
        }
        _ => {
            (*mem.offset(q as isize)).b32.s0 = 3i32;
            if w.abs() >= 0x800000i32 {
                dvi_out((o as i32 + 3i32) as eight_bits);
                dvi_four(w);
                return;
            }
            if w.abs() >= 0x8000i32 {
                dvi_out((o as i32 + 2i32) as eight_bits);
                if w < 0i32 {
                    w = w + 0x1000000i32
                }
                dvi_out((w / 0x10000i32) as eight_bits);
                w = w % 0x10000i32;
                current_block = 14567512515169274304;
            } else if w.abs() >= 128i32 {
                dvi_out((o as i32 + 1i32) as eight_bits);
                if w < 0i32 {
                    w = w + 0x10000i32
                }
                current_block = 14567512515169274304;
            } else {
                dvi_out(o);
                if w < 0i32 {
                    w = w + 256i32
                }
                current_block = 18026793543132934442;
            }
            match current_block {
                14567512515169274304 => {
                    dvi_out((w / 256i32) as eight_bits);
                }
                _ => {}
            }
            dvi_out((w % 256i32) as eight_bits);
            return;
        }
    };
}
unsafe extern "C" fn prune_movements(mut l: i32) {
    let mut p: i32 = 0;
    while down_ptr != -0xfffffffi32 {
        if (*mem.offset((down_ptr + 2i32) as isize)).b32.s1 < l {
            break;
        }
        p = down_ptr;
        down_ptr = (*mem.offset(p as isize)).b32.s1;
        free_node(p, 3i32);
    }
    while right_ptr != -0xfffffffi32 {
        if (*mem.offset((right_ptr + 2i32) as isize)).b32.s1 < l {
            return;
        }
        p = right_ptr;
        right_ptr = (*mem.offset(p as isize)).b32.s1;
        free_node(p, 3i32);
    }
}
unsafe extern "C" fn special_out(mut p: i32) {
    let mut k: pool_pointer = 0;
    if cur_h != dvi_h {
        movement(cur_h - dvi_h, 143i32 as eight_bits);
        dvi_h = cur_h
    }
    if cur_v != dvi_v {
        movement(cur_v - dvi_v, 157i32 as eight_bits);
        dvi_v = cur_v
    }
    doing_special = true;
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    show_token_list(
        (*mem.offset((*mem.offset((p + 1i32) as isize)).b32.s1 as isize))
            .b32
            .s1,
        -0xfffffffi32,
        pool_size - pool_ptr,
    );
    selector = old_setting;
    if pool_ptr + 1i32 > pool_size {
        overflow(
            b"pool size\x00" as *const u8 as *const i8,
            pool_size - init_pool_ptr,
        );
    }
    if cur_length() < 256i32 {
        dvi_out(239i32 as eight_bits);
        dvi_out(cur_length() as eight_bits);
    } else {
        dvi_out(242i32 as eight_bits);
        dvi_four(cur_length());
    }
    let mut for_end: i32 = 0;
    k = *str_start.offset((str_ptr - 65536i32) as isize);
    for_end = pool_ptr - 1i32;
    if k <= for_end {
        loop {
            dvi_out(*str_pool.offset(k as isize) as eight_bits);
            let fresh8 = k;
            k = k + 1;
            if !(fresh8 < for_end) {
                break;
            }
        }
    }
    pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
    doing_special = false;
}
unsafe extern "C" fn write_out(mut p: i32) {
    let mut old_mode: i32 = 0;
    let mut j: small_number = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut d: i32 = 0;
    q = get_avail();
    (*mem.offset(q as isize)).b32.s0 = 0x400000i32 + '}' as i32;
    r = get_avail();
    (*mem.offset(q as isize)).b32.s1 = r;
    (*mem.offset(r as isize)).b32.s0 = 0x1ffffffi32
        + (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 8i32);
    begin_token_list(q, 5_u16);
    begin_token_list((*mem.offset((p + 1i32) as isize)).b32.s1, 18_u16);
    q = get_avail();
    (*mem.offset(q as isize)).b32.s0 = 0x200000i32 + '{' as i32;
    begin_token_list(q, 5_u16);
    old_mode = cur_list.mode as i32;
    cur_list.mode = 0_i16;
    cur_cs = write_loc;
    q = scan_toks(false, true);
    get_token();
    if cur_tok
        != 0x1ffffffi32
            + (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 8i32)
    {
        /*1412:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! \x00" as *const u8 as *const i8);
        }
        print_cstr(b"Unbalanced write command\x00" as *const u8 as *const i8);
        help_ptr = 2_u8;
        help_line[1] = b"On this page there\'s a \\write with fewer real {\'s than }\'s.\x00"
            as *const u8 as *const i8;
        help_line[0] = b"I can\'t handle that very well; good luck.\x00" as *const u8 as *const i8;
        error();
        loop {
            get_token();
            if !(cur_tok
                != 0x1ffffffi32
                    + (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 8i32))
            {
                break;
            }
        }
    }
    cur_list.mode = old_mode as i16;
    end_token_list();
    let old_setting = selector;
    j = (*mem.offset((p + 1i32) as isize)).b32.s0 as small_number;
    if j == 18 {
        selector = Selector::NEW_STRING
    } else if write_open[j as usize] {
        selector = (j as u8).into()
    } else {
        if j == 17 && (selector == Selector::TERM_AND_LOG) {
            selector = Selector::LOG_ONLY
        }
        print_nl_cstr(b"\x00" as *const u8 as *const i8);
    }
    token_show(def_ref);
    print_ln();
    flush_list(def_ref);
    if j as i32 == 18i32 {
        if (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 29i32) as isize,
        ))
        .b32
        .s1 <= 0i32
        {
            selector = Selector::LOG_ONLY
        } else {
            selector = Selector::TERM_AND_LOG
        }
        if !log_opened {
            selector = Selector::TERM_ONLY
        }
        print_nl_cstr(b"runsystem(\x00" as *const u8 as *const i8);
        d = 0i32;
        while d <= cur_length() - 1i32 {
            print(
                *str_pool.offset((*str_start.offset((str_ptr - 65536i32) as isize) + d) as isize)
                    as i32,
            );
            d += 1
        }
        print_cstr(b")...\x00" as *const u8 as *const i8);
        print_cstr(b"disabled\x00" as *const u8 as *const i8);
        print_char('.' as i32);
        print_nl_cstr(b"\x00" as *const u8 as *const i8);
        print_ln();
        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize)
    }
    selector = old_setting;
}
unsafe extern "C" fn pic_out(mut p: i32) {
    let mut i: i32 = 0;
    let mut k: pool_pointer = 0;
    if cur_h != dvi_h {
        movement(cur_h - dvi_h, 143i32 as eight_bits);
        dvi_h = cur_h
    }
    if cur_v != dvi_v {
        movement(cur_v - dvi_v, 157i32 as eight_bits);
        dvi_v = cur_v
    }
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    print_cstr(b"pdf:image \x00" as *const u8 as *const i8);
    print_cstr(b"matrix \x00" as *const u8 as *const i8);
    print_scaled((*mem.offset((p + 5i32) as isize)).b32.s0);
    print(' ' as i32);
    print_scaled((*mem.offset((p + 5i32) as isize)).b32.s1);
    print(' ' as i32);
    print_scaled((*mem.offset((p + 6i32) as isize)).b32.s0);
    print(' ' as i32);
    print_scaled((*mem.offset((p + 6i32) as isize)).b32.s1);
    print(' ' as i32);
    print_scaled((*mem.offset((p + 7i32) as isize)).b32.s0);
    print(' ' as i32);
    print_scaled((*mem.offset((p + 7i32) as isize)).b32.s1);
    print(' ' as i32);
    print_cstr(b"page \x00" as *const u8 as *const i8);
    print_int((*mem.offset((p + 4i32) as isize)).b16.s0 as i32);
    print(' ' as i32);
    match (*mem.offset((p + 8i32) as isize)).b16.s1 as i32 {
        1 => {
            print_cstr(b"pagebox cropbox \x00" as *const u8 as *const i8);
        }
        2 => {
            print_cstr(b"pagebox mediabox \x00" as *const u8 as *const i8);
        }
        3 => {
            print_cstr(b"pagebox bleedbox \x00" as *const u8 as *const i8);
        }
        5 => {
            print_cstr(b"pagebox artbox \x00" as *const u8 as *const i8);
        }
        4 => {
            print_cstr(b"pagebox trimbox \x00" as *const u8 as *const i8);
        }
        _ => {}
    }
    print('(' as i32);
    i = 0i32;
    while i < (*mem.offset((p + 4i32) as isize)).b16.s1 as i32 {
        print_raw_char(
            *(&mut *mem.offset((p + 9i32) as isize) as *mut memory_word as *mut u8)
                .offset(i as isize) as UTF16_code,
            true,
        );
        i += 1
    }
    print(')' as i32);
    selector = old_setting;
    if cur_length() < 256i32 {
        dvi_out(239i32 as eight_bits);
        dvi_out(cur_length() as eight_bits);
    } else {
        dvi_out(242i32 as eight_bits);
        dvi_four(cur_length());
    }
    k = *str_start.offset((str_ptr - 65536i32) as isize);
    while k < pool_ptr {
        dvi_out(*str_pool.offset(k as isize) as eight_bits);
        k += 1
    }
    pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
    /* discard the string we just made */
}
/* xetex-errors */
/* xetex-math */
/* xetex-output */
/* xetex-pagebuilder */
/* xetex-scaledmath */
/* xetex-shipout */
#[no_mangle]
pub unsafe extern "C" fn finalize_dvi_file() {
    let mut k: u8 = 0;
    while cur_s > -1i32 {
        if cur_s > 0i32 {
            dvi_out(142i32 as eight_bits);
        } else {
            dvi_out(140i32 as eight_bits);
            total_pages += 1
        }
        cur_s -= 1
    }
    if total_pages == 0i32 {
        print_nl_cstr(b"No pages of output.\x00" as *const u8 as *const i8);
        return;
    }
    if cur_s == -2i32 {
        /* This happens when the DVI gets too big; a message has already been printed */
        return;
    } /* magic values: conversion ratio for sp */
    dvi_out(248i32 as eight_bits); /* magic values: conversion ratio for sp */
    dvi_four(last_bop);
    last_bop = dvi_offset + dvi_ptr - 5i32;
    dvi_four(25400000i64 as i32);
    dvi_four(473628672i64 as i32);
    prepare_mag();
    dvi_four(
        (*eqtb.offset(
            (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 17i32) as isize,
        ))
        .b32
        .s1,
    );
    dvi_four(max_v);
    dvi_four(max_h);
    dvi_out((max_push / 256i32) as eight_bits);
    dvi_out((max_push % 256i32) as eight_bits);
    dvi_out((total_pages / 256i32 % 256i32) as eight_bits);
    dvi_out((total_pages % 256i32) as eight_bits);
    while font_ptr > 0i32 {
        if *font_used.offset(font_ptr as isize) {
            dvi_font_def(font_ptr);
        }
        font_ptr -= 1
    }
    dvi_out(249i32 as eight_bits);
    dvi_four(last_bop);
    if semantic_pagination_enabled {
        dvi_out(100i32 as eight_bits);
    } else {
        dvi_out(7i32 as eight_bits);
    }
    k = (4i32 + (16384i32 - dvi_ptr) % 4i32) as u8;
    while k as i32 > 0i32 {
        dvi_out(223i32 as eight_bits);
        k = k.wrapping_sub(1)
    }
    if dvi_limit == 8192i32 {
        write_to_dvi(8192i32, 16384i32 - 1i32);
    }
    if dvi_ptr > 0x7fffffffi32 - dvi_offset {
        cur_s = -2i32;
        fatal_error(b"dvi length exceeds 0x7FFFFFFF\x00" as *const u8 as *const i8);
    }
    if dvi_ptr > 0i32 {
        write_to_dvi(0i32, dvi_ptr - 1i32);
    }
    k = ttstub_output_close(dvi_file) as u8;
    if k as i32 == 0i32 {
        print_nl_cstr(b"Output written on \x00" as *const u8 as *const i8);
        print(output_file_name);
        print_cstr(b" (\x00" as *const u8 as *const i8);
        print_int(total_pages);
        if total_pages != 1i32 {
            print_cstr(b" pages\x00" as *const u8 as *const i8);
        } else {
            print_cstr(b" page\x00" as *const u8 as *const i8);
        }
        print_cstr(b", \x00" as *const u8 as *const i8);
        print_int(dvi_offset + dvi_ptr);
        print_cstr(b" bytes).\x00" as *const u8 as *const i8);
    } else {
        print_nl_cstr(b"Error \x00" as *const u8 as *const i8);
        print_int(k as i32);
        print_cstr(b" (\x00" as *const u8 as *const i8);
        print_c_string(strerror(k as i32));
        print_cstr(b") generating output;\x00" as *const u8 as *const i8);
        print_nl_cstr(b"file \x00" as *const u8 as *const i8);
        print(output_file_name);
        print_cstr(b" may not be valid.\x00" as *const u8 as *const i8);
        /* XeTeX adds history = OUTPUT_FAILURE = 4 here; I'm not implementing that. */
    };
}
unsafe extern "C" fn write_to_dvi(mut a: i32, mut b: i32) {
    let mut n: i32 = b - a + 1i32;
    assert!(
        ttstub_output_write(
            dvi_file,
            &mut *dvi_buf.offset(a as isize) as *mut eight_bits as *mut i8,
            n as size_t,
        ) == n as size_t,
        "failed to write data to XDV file"
    );
}
unsafe extern "C" fn dvi_swap() {
    if dvi_ptr > 0x7fffffffi32 - dvi_offset {
        cur_s = -2i32;
        fatal_error(b"dvi length exceeds 0x7FFFFFFF\x00" as *const u8 as *const i8);
    }
    if dvi_limit == 16384i32 {
        write_to_dvi(0i32, 8192i32 - 1i32);
        dvi_limit = 8192i32;
        dvi_offset = dvi_offset + 16384i32;
        dvi_ptr = 0i32
    } else {
        write_to_dvi(8192i32, 16384i32 - 1i32);
        dvi_limit = 16384i32
    }
    dvi_gone = dvi_gone + 8192i32;
}
unsafe extern "C" fn dvi_four(mut x: i32) {
    if x >= 0i32 {
        dvi_out((x / 0x1000000i32) as eight_bits);
    } else {
        x = x + 0x40000000i32;
        x = x + 0x40000000i32;
        dvi_out((x / 0x1000000i32 + 128i32) as eight_bits);
    }
    x = x % 0x1000000i32;
    dvi_out((x / 0x10000i32) as eight_bits);
    x = x % 0x10000i32;
    dvi_out((x / 0x100i32) as eight_bits);
    dvi_out((x % 0x100i32) as eight_bits);
}
unsafe extern "C" fn dvi_two(mut s: UTF16_code) {
    dvi_out((s as i32 / 0x100i32) as eight_bits);
    dvi_out((s as i32 % 0x100i32) as eight_bits);
}
unsafe extern "C" fn dvi_pop(mut l: i32) {
    if l == dvi_offset + dvi_ptr && dvi_ptr > 0i32 {
        dvi_ptr -= 1
    } else {
        dvi_out(142i32 as eight_bits);
    };
}
