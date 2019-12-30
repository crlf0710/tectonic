use bridge::{abort, DisplayExt};
use std::ffi::CStr;
use std::io::Write;

use crate::core_memory::xmalloc_array;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow};
use crate::xetex_ext::{
    apply_tfm_font_mapping, makeXDVGlyphArrayData, make_font_def, store_justified_native_glyphs,
};
use crate::xetex_ini::{
    avail, cur_area, cur_cs, cur_dir, cur_ext, cur_h, cur_h_offset, cur_list, cur_name,
    cur_page_height, cur_page_width, cur_tok, cur_v, cur_v_offset, dead_cycles, def_ref,
    doing_leaders, doing_special, file_line_error_style_p, file_offset, font_ptr, font_used,
    help_line, help_ptr, init_pool_ptr, job_name, last_bop, log_opened, max_h, max_print_line,
    max_push, max_v, name_of_file, output_file_extension, pdf_last_x_pos, pdf_last_y_pos, pool_ptr,
    pool_size, rule_dp, rule_ht, rule_wd, rust_stdout, selector, semantic_pagination_enabled,
    str_pool, str_ptr, str_start, temp_ptr, term_offset, total_pages, write_file, write_loc,
    write_open, xdv_buffer, xtx_ligature_present, LR_problems, LR_ptr, CHAR_BASE, FONT_AREA,
    FONT_BC, FONT_CHECK, FONT_DSIZE, FONT_EC, FONT_GLUE, FONT_INFO, FONT_LETTER_SPACE,
    FONT_MAPPING, FONT_NAME, FONT_SIZE, MEM, WIDTH_BASE,
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
    begin_diagnostic, begin_token_list, cur_length, effective_char, end_diagnostic, end_token_list,
    flush_list, flush_node_list, free_node, get_avail, get_node, get_token, glue_ord,
    internal_font_number, make_name_string, new_kern, new_math, new_native_word_node,
    open_log_file, pack_file_name, pack_job_name, packed_UTF16_code, pool_pointer, prepare_mag,
    scaled_t, scan_toks, show_box, show_token_list, small_number, str_number, token_show,
    BOX_depth, BOX_glue_order, BOX_glue_sign, BOX_height, BOX_list_ptr, BOX_width,
    EDGE_NODE_edge_dist, GLUE_NODE_glue_ptr, GLUE_SPEC_shrink_order, LLIST_link, NODE_subtype,
    NODE_type, UTF16_code,
};
use crate::xetex_xetexd::{is_char_node, print_c_string};
use bridge::{ttstub_output_close, ttstub_output_open};
use libc::{free, strerror, strlen};

use bridge::OutputHandleWrapper;

const DVI_BUF_SIZE: i32 = 16384;
const HALF_BUF: i32 = 8192;
const FNT_NUM_0: i32 = 171; /* DVI code */

static mut dvi_file: Option<OutputHandleWrapper> = None;
static mut output_file_name: str_number = 0;
static mut dvi_buf: *mut u8 = std::ptr::null_mut();
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
pub(crate) unsafe fn initialize_shipout_variables() {
    output_file_name = 0;
    dvi_buf = xmalloc_array(DVI_BUF_SIZE as usize + 1);
    dvi_limit = DVI_BUF_SIZE;
    dvi_ptr = 0;
    dvi_offset = 0;
    dvi_gone = 0;
    down_ptr = TEX_NULL;
    right_ptr = TEX_NULL;
    cur_s = -1;
}

#[no_mangle]
pub(crate) unsafe fn deinitialize_shipout_variables() {
    free(dvi_buf as *mut _);
    dvi_buf = std::ptr::null_mut();
}

#[inline]
unsafe extern "C" fn dvi_out(c: u8) {
    *dvi_buf.offset(dvi_ptr as isize) = c;
    dvi_ptr += 1;
    if dvi_ptr == dvi_limit {
        dvi_swap();
    };
}

/*660: output the box `p` */
#[no_mangle]
pub(crate) unsafe fn ship_out(mut p: i32) {
    let mut page_loc: i32 = 0;
    let mut j: u8 = 0;
    let mut s: pool_pointer = 0;
    let mut l: u8 = 0;
    let mut output_comment: *const i8 = b"tectonic\x00" as *const u8 as *const i8;

    synctex_sheet(INTPAR(INT_PAR__mag));

    if job_name == 0 {
        open_log_file();
    }

    if INTPAR(INT_PAR__tracing_output) > 0 {
        print_nl_cstr(b"");
        print_ln();
        print_cstr(b"Completed box being shipped out");
    }

    if term_offset > max_print_line - 9 {
        print_ln();
    } else if term_offset > 0 || file_offset > 0 {
        print_char(' ' as i32);
    }

    print_char('[' as i32);
    j = 9;
    while j > 0 && COUNT_REG(j as _) == 0 {
        j -= 1;
    }

    for k in 0..=j {
        print_int(COUNT_REG(k as _));
        if k < j {
            print_char('.' as i32);
        }
    }

    rust_stdout.as_mut().unwrap().flush().unwrap();

    if INTPAR(INT_PAR__tracing_output) > 0 {
        print_char(']' as i32);
        begin_diagnostic();
        show_box(p);
        end_diagnostic(true);
    }

    /*662: "Ship box `p` out." */
    /*663: "Update the values of max_h and max_v; but if the page is too
     * large, goto done". */

    if *BOX_height(p as isize) > MAX_HALFWORD
        || *BOX_depth(p as isize) > MAX_HALFWORD
        || *BOX_height(p as isize) + *BOX_depth(p as isize) + DIMENPAR(DIMEN_PAR__v_offset)
            > MAX_HALFWORD
        || *BOX_width(p as isize) + DIMENPAR(DIMEN_PAR__h_offset) > MAX_HALFWORD
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Huge page cannot be shipped out");
        help_ptr = 2;
        help_line[1] = b"The page just created is more than 18 feet tall or";
        help_line[0] = b"more than 18 feet wide, so I suspect something went wrong.";
        error();

        if INTPAR(INT_PAR__tracing_output) <= 0 {
            begin_diagnostic();
            print_nl_cstr(b"The following box has been deleted:");
            show_box(p);
            end_diagnostic(1i32 != 0);
        }
    } else {
        if *BOX_height(p as isize) + *BOX_depth(p as isize) + DIMENPAR(DIMEN_PAR__v_offset) > max_v
        {
            max_v =
                *BOX_height(p as isize) + *BOX_depth(p as isize) + DIMENPAR(DIMEN_PAR__v_offset);
        }
        if *BOX_width(p as isize) + DIMENPAR(DIMEN_PAR__h_offset) > max_h {
            max_h = *BOX_width(p as isize) + DIMENPAR(DIMEN_PAR__h_offset);
        }

        /*637: "Initialize variables as ship_out begins." */

        dvi_h = 0;
        dvi_v = 0;
        cur_h = DIMENPAR(DIMEN_PAR__h_offset);
        dvi_f = 0;

        /*1405: "Calculate page dimensions and margins" */
        /* 4736287 = round(0xFFFF * 72.27) ; i.e., 1 inch expressed as a scaled_t */
        cur_h_offset = DIMENPAR(DIMEN_PAR__h_offset) + 4736287;
        cur_v_offset = DIMENPAR(DIMEN_PAR__v_offset) + 4736287;

        if DIMENPAR(DIMEN_PAR__pdf_page_width) != 0 {
            cur_page_width = DIMENPAR(DIMEN_PAR__pdf_page_width);
        } else {
            cur_page_width = *BOX_width(p as isize) + 2 * cur_h_offset;
        }
        if DIMENPAR(DIMEN_PAR__pdf_page_height) != 0 {
            cur_page_height = DIMENPAR(DIMEN_PAR__pdf_page_height);
        } else {
            cur_page_height = *BOX_height(p as isize) + *BOX_depth(p as isize) + 2 * cur_v_offset;
        }

        /* ... resuming 637 ... open up the DVI file if needed */

        if output_file_name == 0 {
            if job_name == 0 {
                open_log_file();
            }
            pack_job_name(CStr::from_ptr(output_file_extension).to_bytes());
            dvi_file = ttstub_output_open(name_of_file, 0);
            if dvi_file.is_none() {
                abort!(
                    "cannot open output file \"{}\"",
                    CStr::from_ptr(name_of_file).display()
                );
            }
            output_file_name = make_name_string()
        }

        /* First page? Emit preamble items. */

        if total_pages == 0 {
            dvi_out(PRE as _);
            if semantic_pagination_enabled {
                dvi_out(SPX_ID_BYTE as _);
            } else {
                dvi_out(XDV_ID_BYTE as _);
            }

            dvi_four(25400000); /* magic values: conversion ratio for sp */
            dvi_four(473628672); /* magic values: conversion ratio for sp */

            prepare_mag();
            dvi_four(INTPAR(INT_PAR__mag));

            l = strlen(output_comment) as u8;
            dvi_out(l);
            s = 0i32;
            while s < l as i32 {
                dvi_out(*output_comment.offset(s as isize) as u8);
                s += 1
            }
        }

        /* ... resuming 662 ... Emit per-page preamble. */

        page_loc = dvi_offset + dvi_ptr;

        dvi_out(BOP as _);

        for k in 0..10 {
            dvi_four(COUNT_REG(k));
        }

        dvi_four(last_bop);
        last_bop = page_loc;

        /* Generate a PDF pagesize special unilaterally */

        let old_setting = selector;
        selector = Selector::NEW_STRING;
        print_cstr(b"pdf:pagesize ");
        if DIMENPAR(DIMEN_PAR__pdf_page_width) <= 0 || DIMENPAR(DIMEN_PAR__pdf_page_height) <= 0 {
            print_cstr(b"default");
        } else {
            print_cstr(b"width");
            print(' ' as i32);
            print_scaled(DIMENPAR(DIMEN_PAR__pdf_page_width));
            print_cstr(b"pt");
            print(' ' as i32);
            print_cstr(b"height");
            print(' ' as i32);
            print_scaled(DIMENPAR(DIMEN_PAR__pdf_page_height));
            print_cstr(b"pt");
        }
        selector = old_setting;

        dvi_out(XXX1 as _);
        dvi_out(cur_length() as u8);

        s = *str_start.offset((str_ptr - TOO_BIG_CHAR) as isize);
        while s < pool_ptr {
            dvi_out(*str_pool.offset(s as isize) as u8);
            s += 1
        }

        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);

        /* Done with the synthesized special. The meat: emit this page box. */

        cur_v = *BOX_height(p as isize) + DIMENPAR(DIMEN_PAR__v_offset); /*"Does this need changing for upwards mode???"*/
        temp_ptr = p;
        if *NODE_type(p as isize) == VLIST_NODE {
            vlist_out();
        } else {
            hlist_out();
        }

        dvi_out(EOP as _);
        total_pages += 1;
        cur_s = -1;
    }

    /*1518: "Check for LR anomalies at the end of ship_out" */

    if LR_problems > 0 {
        print_ln();
        print_nl_cstr(b"\\endL or \\endR problem (");
        print_int(LR_problems / 10000);
        print_cstr(b" missing, ");
        print_int(LR_problems % 10000);
        print_cstr(b" extra");
        LR_problems = 0;
        print_char(')' as i32);
        print_ln();
    }

    if LR_ptr != TEX_NULL || cur_dir != LEFT_TO_RIGHT as _ {
        confusion(b"LR3");
    }

    if INTPAR(INT_PAR__tracing_output) <= 0 {
        print_char(']' as i32);
    }

    dead_cycles = 0;
    rust_stdout.as_mut().unwrap().flush().unwrap();
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
    let mut c: u16 = 0;
    let mut f: internal_font_number = 0;

    let mut cur_g: scaled_t = 0;
    let mut cur_glue: f64 = 0.0;
    let mut this_box: i32 = temp_ptr;
    let mut g_order: glue_ord = *BOX_glue_order(this_box as _) as _;
    let mut g_sign: u8 = *BOX_glue_sign(this_box as _) as _;

    if INTPAR(INT_PAR__xetex_interword_space_shaping) > 1 {
        /*640: "Extra stuff for justifiable AAT text..." "Merge sequences of
         * words using native fonts and inter-word spaces into single
         * nodes" */

        p = *BOX_list_ptr(this_box as _);
        prev_p = this_box + 5; /* this gets the list within the box */

        while p != TEX_NULL {
            if *LLIST_link(p as isize) != TEX_NULL {
                if p != TEX_NULL
                    && !is_char_node(p)
                    && *NODE_type(p as isize) != WHATSIT_NODE
                    && (*NODE_subtype(p as isize) == NATIVE_WORD_NODE
                        || *NODE_subtype(p as isize) == NATIVE_WORD_NODE_AT)
                    && FONT_LETTER_SPACE[MEM[(p + 4) as usize].b16.s2 as usize] == 0
                {
                    /* "got a word in an AAT font, might be the start of a run" */
                    r = p;
                    k = MEM[(r + 4) as usize].b16.s1 as i32;
                    q = *LLIST_link(p as isize);
                    loop {
                        /*641: "Advance `q` past ignorable nodes." This test is
                         * mostly `node_is_invisible_to_interword_space`. 641 is
                         * reused a few times here. */
                        while q != TEX_NULL
                            && !is_char_node(q)
                            && (*NODE_type(q as isize) == PENALTY_NODE
                                || *NODE_type(q as isize) == INS_NODE
                                || *NODE_type(q as isize) == MARK_NODE
                                || *NODE_type(q as isize) == ADJUST_NODE
                                || (*NODE_type(q as isize) == WHATSIT_NODE
                                    && *NODE_subtype(q as isize) <= 4))
                        {
                            q = *LLIST_link(q as isize);
                        }
                        if !(q != TEX_NULL && !is_char_node(q)) {
                            break;
                        }
                        if *NODE_type(q as isize) == GLUE_NODE
                            && *GLUE_SPEC_shrink_order(q as isize) == NORMAL as _
                        {
                            if *GLUE_NODE_glue_ptr(q as isize)
                                == FONT_GLUE[MEM[(r + 4) as usize].b16.s2 as usize]
                            {
                                /* "Found a normal space; if the next node is
                                 * another word in the same font, we'll
                                 * merge." */
                                q = *LLIST_link(q as isize);

                                while q != TEX_NULL
                                    && !is_char_node(q)
                                    && (*NODE_type(q as isize) == PENALTY_NODE
                                        || *NODE_type(q as isize) == INS_NODE
                                        || *NODE_type(q as isize) == MARK_NODE
                                        || *NODE_type(q as isize) == ADJUST_NODE
                                        || (*NODE_type(q as isize) == WHATSIT_NODE
                                            && *NODE_subtype(q as isize) <= 4))
                                {
                                    q = *LLIST_link(q as isize);
                                }

                                if q != TEX_NULL
                                    && !is_char_node(q)
                                    && *NODE_type(q as isize) == WHATSIT_NODE
                                    && (*NODE_subtype(q as isize) == NATIVE_WORD_NODE
                                        || *NODE_subtype(q as isize) == NATIVE_WORD_NODE_AT)
                                    && MEM[(q + 4) as usize].b16.s2 as i32
                                        == MEM[(r + 4) as usize].b16.s2 as i32
                                {
                                    p = q;
                                    k += 1 + MEM[(q + 4) as usize].b16.s1 as i32;
                                    q = *LLIST_link(q as isize);
                                    continue;
                                }
                            } else {
                                q = *LLIST_link(q as isize);
                            }
                            if !(q != TEX_NULL
                                && !is_char_node(q)
                                && *NODE_type(q as isize) == KERN_NODE
                                && *NODE_subtype(q as isize) == SPACE_ADJUSTMENT as _)
                            {
                                break;
                            }
                            q = MEM[q as usize].b32.s1;
                            while q != -0xfffffffi32
                                && !is_char_node(q)
                                && (MEM[q as usize].b16.s1 as i32 == 12
                                    || MEM[q as usize].b16.s1 as i32 == 3
                                    || MEM[q as usize].b16.s1 as i32 == 4
                                    || MEM[q as usize].b16.s1 as i32 == 5
                                    || MEM[q as usize].b16.s1 as i32 == 8
                                        && MEM[q as usize].b16.s0 as i32 <= 4)
                            {
                                q = MEM[q as usize].b32.s1
                            }
                            if !(q != -0xfffffffi32
                                && !is_char_node(q)
                                && MEM[q as usize].b16.s1 as i32 == 8
                                && (MEM[q as usize].b16.s0 as i32 == 40
                                    || MEM[q as usize].b16.s0 as i32 == 41)
                                && MEM[(q + 4) as usize].b16.s2 as i32
                                    == MEM[(r + 4) as usize].b16.s2 as i32)
                            {
                                break;
                            }
                            p = q;
                            k += 1i32 + MEM[(q + 4) as usize].b16.s1 as i32;
                            q = MEM[q as usize].b32.s1
                        } else {
                            if !(q != -0xfffffffi32
                                && !is_char_node(q)
                                && MEM[q as usize].b16.s1 as i32 == 8
                                && (MEM[q as usize].b16.s0 as i32 == 40
                                    || MEM[q as usize].b16.s0 as i32 == 41)
                                && MEM[(q + 4) as usize].b16.s2 as i32
                                    == MEM[(r + 4) as usize].b16.s2 as i32)
                            {
                                break;
                            }
                            p = q;
                            q = MEM[q as usize].b32.s1
                        }
                    }
                    /* "Now r points to the first native_word_node of the run,
                     * and p to the last." */
                    if p != r {
                        if pool_ptr + k > pool_size {
                            overflow(b"pool size", pool_size - init_pool_ptr);
                        }
                        k = 0i32;
                        q = r;
                        loop {
                            if MEM[q as usize].b16.s1 as i32 == 8 {
                                if MEM[q as usize].b16.s0 as i32 == 40
                                    || MEM[q as usize].b16.s0 as i32 == 41
                                {
                                    j = 0i32;
                                    while j < MEM[(q + 4) as usize].b16.s1 as i32 {
                                        *str_pool.offset(pool_ptr as isize) =
                                            *(&mut MEM[(q + 6) as usize] as *mut memory_word
                                                as *mut u16)
                                                .offset(j as isize);
                                        pool_ptr += 1;
                                        j += 1
                                    }
                                    k += MEM[(q + 1) as usize].b32.s1
                                }
                            } else if MEM[q as usize].b16.s1 as i32 == 10 {
                                *str_pool.offset(pool_ptr as isize) =
                                    ' ' as i32 as packed_UTF16_code;
                                pool_ptr += 1;
                                g = MEM[(q + 1) as usize].b32.s0;
                                k += MEM[(g + 1) as usize].b32.s1;
                                if g_sign as i32 != 0i32 {
                                    if g_sign as i32 == 1i32 {
                                        if MEM[g as usize].b16.s1 as i32 == g_order as i32 {
                                            k += tex_round(
                                                MEM[(this_box + 6) as usize].gr
                                                    * MEM[(g + 2) as usize].b32.s1 as f64,
                                            )
                                        }
                                    } else if MEM[g as usize].b16.s0 as i32 == g_order as i32 {
                                        k -= tex_round(
                                            MEM[(this_box + 6) as usize].gr
                                                * MEM[(g + 3) as usize].b32.s1 as f64,
                                        )
                                    }
                                }
                            } else if MEM[q as usize].b16.s1 as i32 == 11 {
                                k += MEM[(q + 1) as usize].b32.s1
                            }
                            if q == p {
                                break;
                            }
                            q = MEM[q as usize].b32.s1
                        }
                        q = new_native_word_node(
                            MEM[(r + 4) as usize].b16.s2 as internal_font_number,
                            cur_length(),
                        );
                        MEM[q as usize].b16.s0 = MEM[r as usize].b16.s0;
                        j = 0i32;
                        while j < cur_length() {
                            *(&mut MEM[(q + 6) as usize] as *mut memory_word as *mut u16)
                                .offset(j as isize) = *str_pool.offset(
                                (*str_start.offset((str_ptr - 65536i32) as isize) + j) as isize,
                            );
                            j += 1
                        }
                        /* "Link q into the list in place of r...p" */
                        MEM[(q + 1) as usize].b32.s1 = k;
                        store_justified_native_glyphs(
                            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
                        );
                        MEM[prev_p as usize].b32.s1 = q;
                        MEM[q as usize].b32.s1 = MEM[p as usize].b32.s1;
                        MEM[p as usize].b32.s1 = -0xfffffff;
                        prev_p = r;
                        p = MEM[r as usize].b32.s1;
                        /* "Extract any 'invisible' nodes from the old list
                         * and insert them after the new node, so we don't
                         * lose them altogether. Note that the first node
                         * cannot be one of these, as we always start merging
                         * at a native_word node." */
                        while p != -0xfffffffi32 {
                            if !is_char_node(p)
                                && (MEM[p as usize].b16.s1 as i32 == 12
                                    || MEM[p as usize].b16.s1 as i32 == 3
                                    || MEM[p as usize].b16.s1 as i32 == 4
                                    || MEM[p as usize].b16.s1 as i32 == 5
                                    || MEM[p as usize].b16.s1 as i32 == 8
                                        && MEM[p as usize].b16.s0 as i32 <= 4)
                            {
                                MEM[prev_p as usize].b32.s1 = MEM[p as usize].b32.s1;
                                MEM[p as usize].b32.s1 = MEM[q as usize].b32.s1;
                                MEM[q as usize].b32.s1 = p;
                                q = p
                            }
                            prev_p = p;
                            p = MEM[p as usize].b32.s1
                        }
                        flush_node_list(r);
                        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
                        p = q
                    }
                }
                prev_p = p
            }
            p = MEM[p as usize].b32.s1
        }
    }

    /* ... resuming 639 ... */

    p = *BOX_list_ptr(this_box as _);
    cur_s += 1;
    if cur_s > 0 {
        dvi_out(PUSH as _);
    }

    if cur_s > max_push {
        max_push = cur_s
    }

    save_loc = dvi_offset + dvi_ptr;
    base_line = cur_v;
    prev_p = this_box + 5; /* this is list_offset, the offset of the box list pointer */

    /*1501: "Initialize hlist_out for mixed direction typesetting" */

    temp_ptr = get_avail();
    MEM[temp_ptr as usize].b32.s0 = 0;
    MEM[temp_ptr as usize].b32.s1 = LR_ptr;
    LR_ptr = temp_ptr;
    if MEM[this_box as usize].b16.s0 as i32 == 2 {
        if cur_dir as i32 == 1i32 {
            cur_dir = 0i32 as small_number;
            cur_h -= MEM[(this_box + 1) as usize].b32.s1
        } else {
            MEM[this_box as usize].b16.s0 = 0_u16
        }
    }
    if cur_dir as i32 == 1i32 && MEM[this_box as usize].b16.s0 as i32 != 1 {
        /*1508: "Reverse the complete hlist and set the subtype to reversed." */
        save_h = cur_h; /* "SyncTeX: do nothing, it is too late" */
        temp_ptr = p;
        p = new_kern(0i32);
        MEM[(p + 3 - 1) as usize].b32.s0 = 0;
        MEM[prev_p as usize].b32.s1 = p;
        cur_h = 0i32;
        MEM[p as usize].b32.s1 = reverse(this_box, -0xfffffff, &mut cur_g, &mut cur_glue);
        MEM[(p + 1) as usize].b32.s1 = -cur_h;
        cur_h = save_h;
        MEM[this_box as usize].b16.s0 = 1_u16
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
                        movement(cur_h - dvi_h, 143i32 as u8);
                        dvi_h = cur_h
                    }
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, 157i32 as u8);
                        dvi_v = cur_v
                    }
                    loop  {
                        f =
                            MEM[p as usize].b16.s1 as
                                internal_font_number;
                        c = MEM[p as usize].b16.s0;
                        if p != 4999999i32 - 12i32 &&
                               !(FONT_MAPPING[f as usize]).is_null() {
                            c = apply_tfm_font_mapping(FONT_MAPPING[f as usize], c as i32) as u16;
                        }
                        if f != dvi_f {
                            /*643: "Change font dvi_f to f" */
                            if !*font_used.offset(f as isize) {
                                dvi_font_def(f);
                                *font_used.offset(f as isize) = true
                            }
                            if f <= 64i32 {
                                dvi_out((f + 171i32 - 1i32) as u8);
                            } else if f <= 256i32 {
                                dvi_out(235i32 as u8);
                                dvi_out((f - 1i32) as u8);
                            } else {
                                dvi_out((235i32 + 1i32) as u8);
                                dvi_out(((f - 1i32) / 256i32) as u8);
                                dvi_out(((f - 1i32) % 256i32) as u8);
                            }
                            dvi_f = f
                        }
                        if FONT_EC[f as usize] as i32 >=
                               c as i32 {
                            if FONT_BC[f as usize] as i32 <=
                                   c as i32 {
                                if FONT_INFO[(CHAR_BASE[f as usize]
                                                           + c as i32)
                                                          as usize].b16.s3 as
                                       i32 > 0i32 {
                                    /* if (char_exists(orig_char_info(f)(c))) */
                                    if c as i32 >= 128i32 {
                                        dvi_out(128i32 as u8);
                                    }
                                    dvi_out(c as u8);
                                    cur_h +=
                                        FONT_INFO[(WIDTH_BASE[f
                                                                                   as
                                                                                   usize]
                                                                +
                                                                FONT_INFO[(CHAR_BASE[f
                                                                                                          as
                                                                                                          usize]
                                                                                        +
                                                                                        c
                                                                                            as
                                                                                            i32)
                                                                                       as
                                                                                       usize].b16.s3
                                                                    as
                                                                    i32)
                                                               as
                                                               usize].b32.s1
                                }
                            }
                        }
                        prev_p = MEM[prev_p as usize].b32.s1;
                        p = MEM[p as usize].b32.s1;
                        if !is_char_node(p) { break ; }
                    }
                    synctex_current();
                    dvi_h = cur_h;
                    continue 's_726 ;
                } else {
                    /*644: "Output the non-char_node `p` and move to the next node" */
                    match MEM[p as usize].b16.s1 as i32 {
                        0 | 1 => {
                            if MEM[(p + 5) as usize].b32.s1 ==
                                   -0xfffffffi32 {
                                if MEM[p as usize].b16.s1 as
                                       i32 == 1i32 {
                                    synctex_void_vlist(p, this_box);
                                } else { synctex_void_hlist(p, this_box); }
                                cur_h +=
                                    MEM[(p + 1) as usize].b32.s1
                            } else {
                                save_h = dvi_h;
                                save_v = dvi_v;
                                cur_v =
                                    base_line +
                                        MEM[(p + 4) as
                                                         usize].b32.s1;
                                temp_ptr = p;
                                edge =
                                    cur_h +
                                        MEM[(p + 1) as
                                                         usize].b32.s1;
                                if cur_dir as i32 == 1i32 {
                                    cur_h = edge
                                }
                                if MEM[p as usize].b16.s1 as
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
                                MEM[(p + 3) as usize].b32.s1;
                            rule_dp =
                                MEM[(p + 2) as usize].b32.s1;
                            rule_wd =
                                MEM[(p + 1) as usize].b32.s1;
                            current_block = 18357984655869314713;
                            break ;
                        }
                        8 => {
                            /*1407: "Output the whatsit node p in an hlist" */
                            match MEM[p as usize].b16.s0 as
                                      i32 {
                                40 | 41 | 42 => {
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h,
                                                 143i32 as
                                                     u8); /* glyph count */
                                        dvi_h = cur_h
                                    } /* x offset, as fixed-point */
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v,
                                                 157i32 as
                                                     u8); /* y offset, as fixed-point */
                                        dvi_v = cur_v
                                    } /* end of WHATSIT_NODE case */
                                    f =
                                        MEM[(p + 4) as
                                                         usize].b16.s2 as
                                            internal_font_number;
                                    if f != dvi_f {
                                        if !*font_used.offset(f as isize) {
                                            dvi_font_def(f);
                                            *font_used.offset(f as isize) =
                                                true
                                        }
                                        if f <= 64i32 {
                                            dvi_out((f + 170i32) as
                                                        u8);
                                        } else if f <= 256i32 {
                                            dvi_out(235i32 as u8);
                                            dvi_out((f - 1i32) as u8);
                                        } else {
                                            dvi_out((235i32 + 1i32) as
                                                        u8);
                                            dvi_out(((f - 1i32) / 256i32) as
                                                        u8);
                                            dvi_out(((f - 1i32) % 256i32) as
                                                        u8);
                                        }
                                        dvi_f = f
                                    }
                                    if MEM[p as usize].b16.s0 as
                                           i32 == 42i32 {
                                        dvi_out(253i32 as u8);
                                        dvi_four(MEM[(p + 1) as
                                                                  usize].b32.s1);
                                        dvi_two(1i32 as UTF16_code);
                                        dvi_four(0i32);
                                        dvi_four(0i32);
                                        dvi_two(MEM[(p + 4) as
                                                                 usize].b16.s1);
                                        cur_h +=
                                            MEM[(p + 1) as
                                                             usize].b32.s1
                                    } else {
                                        if MEM[p as usize].b16.s0 as
                                               i32 == 41i32 {
                                            if MEM[(p + 4) as
                                                                usize].b16.s1
                                                   as i32 > 0i32 ||
                                                   !MEM[(p + 5) as
                                                                     usize].ptr.is_null()
                                               {
                                                dvi_out(254i32 as u8);
                                                len =
                                                    MEM[(p + 4) as
                                                                     usize].b16.s1
                                                        as i32;
                                                dvi_two(len as UTF16_code);
                                                k = 0i32;
                                                while k < len {
                                                    dvi_two(*(&mut MEM[(p
                                                                                    +
                                                                                    6i32)
                                                                                   as
                                                                                   usize]
                                                                  as
                                                                  *mut memory_word
                                                                  as
                                                                  *mut u16).offset(k
                                                                                                  as
                                                                                                  isize));
                                                    k += 1
                                                }
                                                len =
                                                    makeXDVGlyphArrayData(&mut MEM[p
                                                                                               as
                                                                                               usize]
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
                                                                u8);
                                                    k += 1
                                                }
                                            }
                                        } else if !MEM[(p + 5) as
                                                                    usize].ptr.is_null()
                                         {
                                            dvi_out(253i32 as u8);
                                            len =
                                                makeXDVGlyphArrayData(&mut MEM[p
                                                                                           as
                                                                                           usize]
                                                                          as
                                                                          *mut memory_word
                                                                          as
                                                                          *mut libc::c_void);
                                            k = 0i32;
                                            while k < len {
                                                dvi_out(*xdv_buffer.offset(k
                                                                               as
                                                                               isize)
                                                            as u8);
                                                k += 1
                                            }
                                        }
                                        cur_h +=
                                            MEM[(p + 1) as
                                                             usize].b32.s1
                                    }
                                    dvi_h = cur_h
                                }
                                43 | 44 => {
                                    save_h = dvi_h;
                                    save_v = dvi_v;
                                    cur_v = base_line;
                                    edge =
                                        cur_h +
                                            MEM[(p + 1) as
                                                             usize].b32.s1;
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
                            g = MEM[(p + 1) as usize].b32.s0;
                            rule_wd =
                                MEM[(g + 1) as usize].b32.s1 -
                                    cur_g;
                            if g_sign as i32 != 0i32 {
                                if g_sign as i32 == 1i32 {
                                    if MEM[g as usize].b16.s1 as
                                           i32 ==
                                           g_order as i32 {
                                        cur_glue +=
                                            MEM[(g + 2) as
                                                             usize].b32.s1 as
                                                f64;
                                        glue_temp =
                                            MEM[(this_box + 6) as
                                                             usize].gr *
                                                cur_glue;
                                        if glue_temp > 1000000000.0f64 {
                                            glue_temp = 1000000000.0f64
                                        } else if glue_temp < -1000000000.0f64
                                         {
                                            glue_temp = -1000000000.0f64
                                        }
                                        cur_g = tex_round(glue_temp)
                                    }
                                } else if MEM[g as usize].b16.s0 as
                                              i32 ==
                                              g_order as i32 {
                                    cur_glue -=
                                        MEM[(g + 3) as
                                                         usize].b32.s1 as
                                            f64;
                                    glue_temp =
                                        MEM[(this_box + 6) as
                                                         usize].gr *
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
                                   MEM[g as usize].b16.s1 as
                                       i32 == g_order as i32
                                   ||
                                   g_sign as i32 == 2i32 &&
                                       MEM[g as usize].b16.s0 as
                                           i32 ==
                                           g_order as i32 {
                                if MEM[g as usize].b32.s1 ==
                                       -0xfffffffi32 {
                                    free_node(g,
                                              4i32); /* "will never match" */
                                } else {
                                    MEM[g as usize].b32.s1 -= 1
                                }
                                if (MEM[p as usize].b16.s0 as
                                        i32) < 100i32 {
                                    MEM[p as usize].b16.s1 =
                                        11_u16;
                                    MEM[(p + 1) as usize].b32.s1
                                        = rule_wd
                                } else {
                                    g = get_node(4i32);
                                    MEM[g as usize].b16.s1 =
                                        (3i32 + 1i32) as u16;
                                    MEM[g as usize].b16.s0 =
                                        (3i32 + 1i32) as u16;
                                    MEM[(g + 1) as usize].b32.s1
                                        = rule_wd;
                                    MEM[(g + 2) as usize].b32.s1
                                        = 0i32;
                                    MEM[(g + 3) as usize].b32.s1
                                        = 0i32;
                                    MEM[(p + 1) as usize].b32.s0
                                        = g
                                }
                            }
                            if MEM[p as usize].b16.s0 as i32
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
                                MEM[(p + 1) as usize].b32.s1;
                            current_block = 13889995436552222973;
                            break ;
                        }
                        11 => {
                            synctex_kern(p, this_box);
                            cur_h +=
                                MEM[(p + 1) as usize].b32.s1;
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
                            if MEM[p as usize].b16.s0 as i32
                                   & 1i32 != 0 {
                                /* <= this is end_LR(p) */
                                if MEM[LR_ptr as usize].b32.s0 ==
                                       4i32 *
                                           (MEM[p as usize].b16.s0
                                                as i32 / 4i32) + 3i32
                                   {
                                    temp_ptr = LR_ptr;
                                    LR_ptr =
                                        MEM[temp_ptr as
                                                         usize].b32.s1;
                                    MEM[temp_ptr as usize].b32.s1 =
                                        avail;
                                    avail = temp_ptr
                                } else if MEM[p as usize].b16.s0 as
                                              i32 > 4i32 {
                                    LR_problems += 1
                                }
                                current_block = 330672039582001856;
                                break ;
                            } else {
                                temp_ptr = get_avail();
                                MEM[temp_ptr as usize].b32.s0 =
                                    4i32 *
                                        (MEM[p as usize].b16.s0 as
                                             i32 / 4i32) + 3i32;
                                MEM[temp_ptr as usize].b32.s1 =
                                    LR_ptr;
                                LR_ptr = temp_ptr;
                                if !(MEM[p as usize].b16.s0 as
                                         i32 / 8i32 !=
                                         cur_dir as i32) {
                                    current_block = 330672039582001856;
                                    break ;
                                }
                                /*1509: "Reverse an hlist segment and goto reswitch" */
                                save_h = cur_h; /* = lig_char(p) */
                                temp_ptr = MEM[p as usize].b32.s1;
                                rule_wd =
                                    MEM[(p + 1) as usize].b32.s1;
                                free_node(p, 3i32);
                                cur_dir =
                                    (1i32 - cur_dir as i32) as
                                        small_number;
                                p = new_edge(cur_dir, rule_wd);
                                MEM[prev_p as usize].b32.s1 = p;
                                cur_h = cur_h - left_edge + rule_wd;
                                MEM[p as usize].b32.s1 =
                                    reverse(this_box,
                                            new_edge((1i32 -
                                                          cur_dir as
                                                              i32) as
                                                         small_number, 0i32),
                                            &mut cur_g, &mut cur_glue);
                                MEM[(p + 2) as usize].b32.s1 =
                                    cur_h;
                                cur_dir =
                                    (1i32 - cur_dir as i32) as
                                        small_number;
                                cur_h = save_h
                            }
                        }
                        6 => {
                            /* 675: "Make node p look like a char_node and goto reswitch" */
                            MEM[(4999999 - 12) as usize] =
                                MEM[(p + 1) as usize];
                            MEM[(4999999 - 12) as
                                             usize].b32.s1 =
                                MEM[p as usize].b32.s1;
                            p = 4999999i32 - 12i32;
                            xtx_ligature_present = true
                        }
                        14 => {
                            /*1507: "Cases of hlist_out that arise in mixed direction text only" */
                            cur_h +=
                                MEM[(p + 1) as usize].b32.s1;
                            left_edge =
                                cur_h +
                                    MEM[(p + 2) as usize].b32.s1;
                            cur_dir =
                                MEM[p as usize].b16.s0 as
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
                leader_box = MEM[(p + 1) as usize].b32.s1; /* "compensate for floating-point rounding" ?? */
                if MEM[leader_box as usize].b16.s1 as i32 == 2 {
                    rule_ht = MEM[(leader_box + 3) as usize].b32.s1;
                    rule_dp = MEM[(leader_box + 2) as usize].b32.s1;
                    current_block = 18357984655869314713;
                } else {
                    leader_wd = MEM[(leader_box + 1) as usize].b32.s1;
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
                        if MEM[p as usize].b16.s0 as i32 == 100 {
                            save_h = cur_h;
                            cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);
                            if cur_h < save_h {
                                cur_h = cur_h + leader_wd
                            }
                        } else {
                            lq = rule_wd / leader_wd;
                            lr = rule_wd % leader_wd;
                            if MEM[p as usize].b16.s0 as i32 == 101 {
                                cur_h = cur_h + lr / 2i32
                            } else {
                                lx = lr / (lq + 1i32);
                                cur_h = cur_h + (lr - (lq - 1i32) * lx) / 2i32
                            }
                        }
                        while cur_h + leader_wd <= edge {
                            /*650: "Output a leader box at cur_h, then advance cur_h by leader_wd + lx" */
                            cur_v = base_line + MEM[(leader_box + 4) as usize].b32.s1;
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, 157i32 as u8);
                                dvi_v = cur_v
                            }
                            save_v = dvi_v;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, 143i32 as u8);
                                dvi_h = cur_h
                            }
                            save_h = dvi_h;
                            temp_ptr = leader_box;
                            if cur_dir as i32 == 1i32 {
                                cur_h += leader_wd
                            }
                            outer_doing_leaders = doing_leaders;
                            doing_leaders = true;
                            if MEM[leader_box as usize].b16.s1 as i32 == 1 {
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
                MEM[p as usize].b16.s1 = 11_u16;
                cur_h += MEM[(p + 1) as usize].b32.s1;
                current_block = 13889995436552222973;
            }
            _ => {}
        }
        match current_block {
            18357984655869314713 => {
                /*646: "Output a rule in an hlist" */
                if rule_ht == -0x40000000i32 {
                    rule_ht = MEM[(this_box + 3) as usize].b32.s1
                }
                if rule_dp == -0x40000000i32 {
                    rule_dp = MEM[(this_box + 2) as usize].b32.s1
                }
                rule_ht += rule_dp;
                if rule_ht > 0i32 && rule_wd > 0i32 {
                    if cur_h != dvi_h {
                        movement(cur_h - dvi_h, 143i32 as u8);
                        dvi_h = cur_h
                    }
                    cur_v = base_line + rule_dp;
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, 157i32 as u8);
                        dvi_v = cur_v
                    }
                    dvi_out(132i32 as u8);
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
        p = MEM[p as usize].b32.s1
    }
    synctex_tsilh(this_box);
    /*1502: "Finish hlist_out for mixed direction typesetting" */
    /*1505: "Check for LR anomalies" */
    while MEM[LR_ptr as usize].b32.s0 != 0 {
        if MEM[LR_ptr as usize].b32.s0 > 4 {
            LR_problems += 10000i32
        }
        temp_ptr = LR_ptr;
        LR_ptr = MEM[temp_ptr as usize].b32.s1;
        MEM[temp_ptr as usize].b32.s1 = avail;
        avail = temp_ptr
    }
    temp_ptr = LR_ptr;
    LR_ptr = MEM[temp_ptr as usize].b32.s1;
    MEM[temp_ptr as usize].b32.s1 = avail;
    avail = temp_ptr;
    if MEM[this_box as usize].b16.s0 as i32 == 2 {
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
    g_order = MEM[(this_box + 5) as usize].b16.s0 as glue_ord;
    g_sign = MEM[(this_box + 5) as usize].b16.s1 as u8;
    p = MEM[(this_box + 5) as usize].b32.s1;
    upwards = MEM[this_box as usize].b16.s0 as i32 == 1;
    cur_s += 1;
    if cur_s > 0i32 {
        dvi_out(141i32 as u8);
    }
    if cur_s > max_push {
        max_push = cur_s
    }
    save_loc = dvi_offset + dvi_ptr;
    left_edge = cur_h;
    synctex_vlist(this_box);
    if upwards {
        cur_v += MEM[(this_box + 2) as usize].b32.s1
    } else {
        cur_v -= MEM[(this_box + 3) as usize].b32.s1
    }
    top_edge = cur_v;
    while p != -0xfffffffi32 {
        /*652: "Output node p and move to the next node, maintaining the
         * condition cur_h = left_edge" */
        if is_char_node(p) {
            confusion(b"vlistout");
        } else {
            /*653: "Output the non-char_node p" */
            match MEM[p as usize].b16.s1 as i32 {
                0 | 1 => {
                    /*654: "Output a box in a vlist" */
                    if MEM[(p + 5) as usize].b32.s1 == -0xfffffff {
                        if upwards {
                            cur_v -= MEM[(p + 2) as usize].b32.s1
                        } else {
                            cur_v += MEM[(p + 3) as usize].b32.s1
                        }
                        if MEM[p as usize].b16.s1 as i32 == 1 {
                            synctex_void_vlist(p, this_box);
                        } else {
                            synctex_void_hlist(p, this_box);
                        }
                        if upwards {
                            cur_v -= MEM[(p + 3) as usize].b32.s1
                        } else {
                            cur_v += MEM[(p + 2) as usize].b32.s1
                        }
                    } else {
                        if upwards {
                            cur_v -= MEM[(p + 2) as usize].b32.s1
                        } else {
                            cur_v += MEM[(p + 3) as usize].b32.s1
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, 157i32 as u8);
                            dvi_v = cur_v
                        }
                        save_h = dvi_h;
                        save_v = dvi_v;
                        if cur_dir as i32 == 1i32 {
                            cur_h = left_edge - MEM[(p + 4) as usize].b32.s1
                        } else {
                            cur_h = left_edge + MEM[(p + 4) as usize].b32.s1
                        }
                        temp_ptr = p;
                        if MEM[p as usize].b16.s1 as i32 == 1 {
                            vlist_out();
                        } else {
                            hlist_out();
                        }
                        dvi_h = save_h;
                        dvi_v = save_v;
                        if upwards {
                            cur_v = save_v - MEM[(p + 3) as usize].b32.s1
                        } else {
                            cur_v = save_v + MEM[(p + 2) as usize].b32.s1
                        }
                        cur_h = left_edge
                    }
                    current_block = 5241535548500397784;
                }
                2 => {
                    rule_ht = MEM[(p + 3) as usize].b32.s1;
                    rule_dp = MEM[(p + 2) as usize].b32.s1;
                    rule_wd = MEM[(p + 1) as usize].b32.s1;
                    current_block = 9653381107620864133;
                }
                8 => {
                    /*1403: "Output the whatsit node p in a vlist" */
                    match MEM[p as usize].b16.s0 as i32 {
                        42 => {
                            cur_v = cur_v + MEM[(p + 3) as usize].b32.s1;
                            cur_h = left_edge;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, 143i32 as u8);
                                dvi_h = cur_h
                            }
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, 157i32 as u8);
                                dvi_v = cur_v
                            }
                            f = MEM[(p + 4) as usize].b16.s2 as internal_font_number;
                            if f != dvi_f {
                                /*643:*/
                                if !*font_used.offset(f as isize) {
                                    dvi_font_def(f); /* width */
                                    *font_used.offset(f as isize) = true
                                } /* glyph count */
                                if f <= 64i32 {
                                    dvi_out((f + 170i32) as u8); /* x offset as fixed-point */
                                } else if f <= 256i32 {
                                    dvi_out(235i32 as u8); /* y offset as fixed-point */
                                    dvi_out((f - 1i32) as u8);
                                } else {
                                    dvi_out((235i32 + 1i32) as u8);
                                    dvi_out(((f - 1i32) / 256i32) as u8);
                                    dvi_out(((f - 1i32) % 256i32) as u8);
                                }
                                dvi_f = f
                            }
                            dvi_out(253i32 as u8);
                            dvi_four(0i32);
                            dvi_two(1i32 as UTF16_code);
                            dvi_four(0i32);
                            dvi_four(0i32);
                            dvi_two(MEM[(p + 4) as usize].b16.s1);
                            cur_v += MEM[(p + 2) as usize].b32.s1;
                            cur_h = left_edge
                        }
                        43 | 44 => {
                            save_h = dvi_h;
                            save_v = dvi_v;
                            cur_v = cur_v + MEM[(p + 3) as usize].b32.s1;
                            pic_out(p);
                            dvi_h = save_h;
                            dvi_v = save_v;
                            cur_v = save_v + MEM[(p + 2) as usize].b32.s1;
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
                    g = MEM[(p + 1) as usize].b32.s0;
                    rule_ht = MEM[(g + 1) as usize].b32.s1 - cur_g;
                    if g_sign as i32 != 0i32 {
                        if g_sign as i32 == 1i32 {
                            if MEM[g as usize].b16.s1 as i32 == g_order as i32 {
                                cur_glue += MEM[(g + 2) as usize].b32.s1 as f64;
                                glue_temp = MEM[(this_box + 6) as usize].gr * cur_glue;
                                if glue_temp > 1000000000.0f64 {
                                    glue_temp = 1000000000.0f64
                                } else if glue_temp < -1000000000.0f64 {
                                    glue_temp = -1000000000.0f64
                                }
                                cur_g = tex_round(glue_temp)
                            }
                        } else if MEM[g as usize].b16.s0 as i32 == g_order as i32 {
                            cur_glue -= MEM[(g + 3) as usize].b32.s1 as f64;
                            glue_temp = MEM[(this_box + 6) as usize].gr * cur_glue;
                            if glue_temp > 1000000000.0f64 {
                                glue_temp = 1000000000.0f64
                            } else if glue_temp < -1000000000.0f64 {
                                glue_temp = -1000000000.0f64
                            }
                            cur_g = tex_round(glue_temp)
                        }
                    }
                    rule_ht += cur_g;
                    if MEM[p as usize].b16.s0 as i32 >= 100 {
                        /*657: "Output leaders in a vlist, goto fin_rule if a rule
                         * or next_p if done" */
                        leader_box = MEM[(p + 1) as usize].b32.s1; /* "compensate for floating-point rounding" */
                        if MEM[leader_box as usize].b16.s1 as i32 == 2 {
                            rule_wd = MEM[(leader_box + 1) as usize].b32.s1;
                            rule_dp = 0i32;
                            current_block = 9653381107620864133;
                        } else {
                            leader_ht = MEM[(leader_box + 3) as usize].b32.s1
                                + MEM[(leader_box + 2) as usize].b32.s1;
                            if leader_ht > 0i32 && rule_ht > 0i32 {
                                rule_ht += 10i32;
                                edge = cur_v + rule_ht;
                                lx = 0i32;
                                /*658: "Let cur_v be the position of the first box,
                                 * and set leader_ht + lx to the spacing between
                                 * corresponding parts of boxes" */
                                if MEM[p as usize].b16.s0 as i32 == 100 {
                                    save_v = cur_v;
                                    cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);
                                    if cur_v < save_v {
                                        cur_v = cur_v + leader_ht
                                    }
                                } else {
                                    lq = rule_ht / leader_ht;
                                    lr = rule_ht % leader_ht;
                                    if MEM[p as usize].b16.s0 as i32 == 101 {
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
                                        cur_h = left_edge - MEM[(leader_box + 4) as usize].b32.s1
                                    } else {
                                        cur_h = left_edge + MEM[(leader_box + 4) as usize].b32.s1
                                    }
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h, 143i32 as u8);
                                        dvi_h = cur_h
                                    }
                                    save_h = dvi_h;
                                    cur_v += MEM[(leader_box + 3) as usize].b32.s1;
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v, 157i32 as u8);
                                        dvi_v = cur_v
                                    }
                                    save_v = dvi_v;
                                    temp_ptr = leader_box;
                                    outer_doing_leaders = doing_leaders;
                                    doing_leaders = true;
                                    if MEM[leader_box as usize].b16.s1 as i32 == 1 {
                                        vlist_out();
                                    } else {
                                        hlist_out();
                                    }
                                    doing_leaders = outer_doing_leaders;
                                    dvi_v = save_v;
                                    dvi_h = save_h;
                                    cur_h = left_edge;
                                    cur_v = save_v - MEM[(leader_box + 3) as usize].b32.s1
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
                        cur_v -= MEM[(p + 1) as usize].b32.s1
                    } else {
                        cur_v += MEM[(p + 1) as usize].b32.s1
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
                        rule_wd = MEM[(this_box + 1) as usize].b32.s1
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
                            movement(cur_h - dvi_h, 143i32 as u8);
                            dvi_h = cur_h
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, 157i32 as u8);
                            dvi_v = cur_v
                        }
                        dvi_out(137i32 as u8);
                        dvi_four(rule_ht);
                        dvi_four(rule_wd);
                        cur_h = left_edge
                    }
                }
                _ => {}
            }
            p = MEM[p as usize].b32.s1
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
    g_order = MEM[(this_box + 5) as usize].b16.s0 as glue_ord;
    g_sign = MEM[(this_box + 5) as usize].b16.s1 as u8;
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
                        f = MEM[p as usize].b16.s1 as internal_font_number;
                        c = MEM[p as usize].b16.s0;
                        cur_h += FONT_INFO[(WIDTH_BASE[f as usize]
                            + FONT_INFO
                                [(CHAR_BASE[f as usize] + effective_char(1i32 != 0, f, c)) as usize]
                                .b16
                                .s3 as i32) as usize]
                            .b32
                            .s1;
                        q = MEM[p as usize].b32.s1;
                        MEM[p as usize].b32.s1 = l;
                        l = p;
                        p = q;
                        if !is_char_node(p) {
                            break;
                        }
                    }
                    continue 's_58;
                } else {
                    q = MEM[p as usize].b32.s1;
                    match MEM[p as usize].b16.s1 as i32 {
                        0 | 1 | 2 | 11 => {
                            rule_wd = MEM[(p + 1) as usize].b32.s1;
                            current_block = 3812947724376655173;
                            break;
                        }
                        8 => {
                            if MEM[p as usize].b16.s0 as i32 == 40
                                || MEM[p as usize].b16.s0 as i32 == 41
                                || MEM[p as usize].b16.s0 as i32 == 42
                                || MEM[p as usize].b16.s0 as i32 == 43
                                || MEM[p as usize].b16.s0 as i32 == 44
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
                            g = MEM[(p + 1) as usize].b32.s0; /* "will never match" */
                            rule_wd = MEM[(g + 1) as usize].b32.s1 - *cur_g; /* = mem[lig_char(temp_ptr)] */
                            if g_sign as i32 != 0i32 {
                                if g_sign as i32 == 1i32 {
                                    if MEM[g as usize].b16.s1 as i32 == g_order as i32 {
                                        *cur_glue = *cur_glue + MEM[(g + 2) as usize].b32.s1 as f64;
                                        glue_temp = MEM[(this_box + 6) as usize].gr * *cur_glue;
                                        if glue_temp > 1000000000.0f64 {
                                            glue_temp = 1000000000.0f64
                                        } else if glue_temp < -1000000000.0f64 {
                                            glue_temp = -1000000000.0f64
                                        }
                                        *cur_g = tex_round(glue_temp)
                                    }
                                } else if MEM[g as usize].b16.s0 as i32 == g_order as i32 {
                                    *cur_glue = *cur_glue - MEM[(g + 3) as usize].b32.s1 as f64;
                                    glue_temp = MEM[(this_box + 6) as usize].gr * *cur_glue;
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
                                && MEM[g as usize].b16.s1 as i32 == g_order as i32
                                || g_sign as i32 == 2i32
                                    && MEM[g as usize].b16.s0 as i32 == g_order as i32
                            {
                                if MEM[g as usize].b32.s1 == -0xfffffff {
                                    free_node(g, 4i32);
                                } else {
                                    MEM[g as usize].b32.s1 -= 1;
                                }
                                if (MEM[p as usize].b16.s0 as i32) < 100 {
                                    MEM[p as usize].b16.s1 = 11_u16;
                                    MEM[(p + 1) as usize].b32.s1 = rule_wd
                                } else {
                                    g = get_node(4i32);
                                    MEM[g as usize].b16.s1 = (3 + 1) as u16;
                                    MEM[g as usize].b16.s0 = (3 + 1) as u16;
                                    MEM[(g + 1) as usize].b32.s1 = rule_wd;
                                    MEM[(g + 2) as usize].b32.s1 = 0;
                                    MEM[(g + 3) as usize].b32.s1 = 0;
                                    MEM[(p + 1) as usize].b32.s0 = g
                                }
                            }
                            current_block = 3812947724376655173;
                            break;
                        }
                        6 => {
                            flush_node_list(MEM[(p + 1) as usize].b32.s1);
                            temp_ptr = p;
                            p = get_avail();
                            MEM[p as usize] = MEM[(temp_ptr + 1) as usize];
                            MEM[p as usize].b32.s1 = q;
                            free_node(temp_ptr, 2i32);
                        }
                        9 => {
                            /*1516: "Math nodes in an inner reflected segment are
                             * modified, those at the outer level are changed into
                             * kern nodes." */
                            rule_wd = MEM[(p + 1) as usize].b32.s1;
                            if MEM[p as usize].b16.s0 as i32 & 1 != 0 {
                                current_block = 5873035170358615968;
                                break;
                            } else {
                                current_block = 17239133558811367971;
                                break;
                            }
                        }
                        14 => {
                            confusion(b"LR2");
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
                    if MEM[LR_ptr as usize].b32.s0 != 4 * (MEM[p as usize].b16.s0 as i32 / 4) + 3 {
                        MEM[p as usize].b16.s1 = 11_u16;
                        LR_problems += 1
                    } else {
                        temp_ptr = LR_ptr;
                        LR_ptr = MEM[temp_ptr as usize].b32.s1;
                        MEM[temp_ptr as usize].b32.s1 = avail;
                        avail = temp_ptr;
                        if n > -0xfffffffi32 {
                            n -= 1;
                            MEM[p as usize].b16.s0 -= 1;
                        } else {
                            MEM[p as usize].b16.s1 = 11_u16;
                            if m > -0xfffffffi32 {
                                m -= 1
                            } else {
                                /*1517: "Finish the reverse hlist segment and goto done" */
                                free_node(p, 3i32); /* end GLUE_NODE case */
                                MEM[t as usize].b32.s1 = q;
                                MEM[(t + 1) as usize].b32.s1 = rule_wd;
                                MEM[(t + 2) as usize].b32.s1 = -cur_h - rule_wd;
                                break;
                            }
                        }
                    }
                    current_block = 3812947724376655173;
                }
                17239133558811367971 => {
                    temp_ptr = get_avail();
                    MEM[temp_ptr as usize].b32.s0 = 4 * (MEM[p as usize].b16.s0 as i32 / 4) + 3;
                    MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                    LR_ptr = temp_ptr;
                    if n > -0xfffffffi32 || MEM[p as usize].b16.s0 as i32 / 8 != cur_dir as i32 {
                        n += 1;
                        MEM[p as usize].b16.s0 += 1;
                    } else {
                        MEM[p as usize].b16.s1 = 11_u16;
                        m += 1
                    }
                    current_block = 3812947724376655173;
                }
                7056779235015430508 => {
                    rule_wd = MEM[(p + 1) as usize].b32.s1;
                    current_block = 3812947724376655173;
                }
                _ => {}
            }
            match current_block {
                3812947724376655173 => cur_h += rule_wd,
                _ => {}
            }
            MEM[p as usize].b32.s1 = l;
            if MEM[p as usize].b16.s1 as i32 == 11 {
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
            p = new_math(0i32, MEM[LR_ptr as usize].b32.s0 as small_number);
            LR_problems += 10000i32
        }
    }
    l
}

/*1506: Create a new edge node of subtype `s` and width `w` */
#[no_mangle]
pub(crate) unsafe fn new_edge(s: small_number, w: scaled_t) -> i32 {
    let p = get_node(EDGE_NODE_SIZE);
    *NODE_type(p as isize) = EDGE_NODE;
    *NODE_subtype(p as isize) = s as _;
    *BOX_width(p as isize) = w;
    *EDGE_NODE_edge_dist(p as isize) = 0;
    p
}

#[no_mangle]
pub(crate) unsafe fn out_what(mut p: i32) {
    let mut j: small_number;
    match MEM[p as usize].b16.s0 as i32 {
        OPEN_NODE | WRITE_NODE | CLOSE_NODE => {
            if doing_leaders {
                return;
            }

            j = MEM[(p + 1) as usize].b32.s0 as small_number;
            if MEM[p as usize].b16.s0 as i32 == WRITE_NODE {
                write_out(p);
                return;
            }

            if write_open[j as usize] {
                ttstub_output_close(write_file[j as usize].take().unwrap());
            }

            if MEM[p as usize].b16.s0 as i32 == CLOSE_NODE {
                write_open[j as usize] = false;
                return;
            }

            /* By this point must be OPEN_NODE */
            if j >= 16 {
                return;
            }

            cur_name = MEM[(p + 1) as usize].b32.s1;
            cur_area = MEM[(p + 2) as usize].b32.s0;
            cur_ext = MEM[(p + 2) as usize].b32.s1;
            if length(cur_ext) == 0 {
                cur_ext = maketexstring(b".tex")
            }

            pack_file_name(cur_name, cur_area, cur_ext);

            write_file[j as usize] = ttstub_output_open(name_of_file, 0);
            if write_file[j as usize].is_none() {
                abort!(
                    "cannot open output file \"{}\"",
                    CStr::from_ptr(name_of_file).display()
                );
            }

            write_open[j as usize] = true;

            if log_opened {
                let old_setting = selector;
                if INTPAR(INT_PAR__tracing_online) <= 0 {
                    selector = Selector::LOG_ONLY
                } else {
                    selector = Selector::TERM_AND_LOG
                }
                print_nl_cstr(b"\\openout");
                print_int(j as i32);
                print_cstr(b" = `");
                print_file_name(cur_name, cur_area, cur_ext);
                print_cstr(b"\'.");
                print_nl_cstr(b"");
                print_ln();
                selector = old_setting
            }
        }
        SPECIAL_NODE => {
            special_out(p);
        }
        LANGUAGE_NODE => {}
        _ => {
            confusion(b"ext4");
        }
    };
}

unsafe fn dvi_native_font_def(f: internal_font_number) {
    dvi_out(DEFINE_NATIVE_FONT as _);
    dvi_four(f - 1);
    let font_def_length = make_font_def(f);

    for i in 0..font_def_length {
        dvi_out(*xdv_buffer.offset(i as isize) as u8);
    }
}

unsafe extern "C" fn dvi_font_def(f: internal_font_number) {
    let mut k: pool_pointer = 0;
    let mut l: i32 = 0;
    if FONT_AREA[f as usize] as u32 == 0xffffu32 || FONT_AREA[f as usize] as u32 == 0xfffeu32 {
        dvi_native_font_def(f);
    } else {
        if f <= 256 {
            dvi_out(FNT_DEF1 as _);
            dvi_out((f - 1) as u8);
        } else {
            dvi_out(FNT_DEF1 as u8 + 1);
            dvi_out(((f - 1) / 256) as u8);
            dvi_out(((f - 1) % 256) as u8);
        }
        dvi_out(FONT_CHECK[f as usize].s3 as u8);
        dvi_out(FONT_CHECK[f as usize].s2 as u8);
        dvi_out(FONT_CHECK[f as usize].s1 as u8);
        dvi_out(FONT_CHECK[f as usize].s0 as u8);
        dvi_four(FONT_SIZE[f as usize]);
        dvi_four(FONT_DSIZE[f as usize]);
        dvi_out(length(FONT_AREA[f as usize]) as u8);
        l = 0;
        k = *str_start.offset((FONT_NAME[f as usize] as i64 - 65536) as isize);

        while l == 0i32
            && k < *str_start.offset(((FONT_NAME[f as usize] + 1i32) as i64 - 65536) as isize)
        {
            if *str_pool.offset(k as isize) as i32 == ':' as i32 {
                l = k - *str_start.offset((FONT_NAME[f as usize] as i64 - 65536) as isize)
            }
            k += 1
        }
        if l == 0i32 {
            l = length(FONT_NAME[f as usize])
        }
        dvi_out(l as u8);
        let mut for_end: i32 = 0;
        k = *str_start.offset((FONT_AREA[f as usize] as i64 - 65536) as isize);
        for_end =
            *str_start.offset(((FONT_AREA[f as usize] + 1i32) as i64 - 65536) as isize) - 1i32;
        if k <= for_end {
            loop {
                dvi_out(*str_pool.offset(k as isize) as u8);
                let fresh6 = k;
                k = k + 1;
                if !(fresh6 < for_end) {
                    break;
                }
            }
        }
        let mut for_end_0: i32 = 0;
        k = *str_start.offset((FONT_NAME[f as usize] as i64 - 65536) as isize);
        for_end_0 = *str_start.offset((FONT_NAME[f as usize] as i64 - 65536) as isize) + l - 1i32;
        if k <= for_end_0 {
            loop {
                dvi_out(*str_pool.offset(k as isize) as u8);
                let fresh7 = k;
                k = k + 1;
                if !(fresh7 < for_end_0) {
                    break;
                }
            }
        }
    };
}

unsafe fn movement(mut w: scaled_t, mut o: u8) {
    let mut current_block: u64;
    let mut mstate: small_number = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut k: i32 = 0;
    q = get_node(3i32);
    MEM[(q + 1) as usize].b32.s1 = w;
    MEM[(q + 2) as usize].b32.s1 = dvi_offset + dvi_ptr;
    if o as i32 == 157i32 {
        MEM[q as usize].b32.s1 = down_ptr;
        down_ptr = q
    } else {
        MEM[q as usize].b32.s1 = right_ptr;
        right_ptr = q
    }
    p = MEM[q as usize].b32.s1;
    mstate = 0i32 as small_number;
    loop {
        if !(p != -0xfffffffi32) {
            current_block = 18071914750955744041;
            break;
        }
        if MEM[(p + 1) as usize].b32.s1 == w {
            /* By this point must be OPEN_NODE */
            /*632:*/
            match mstate as i32 + MEM[p as usize].b32.s0 {
                3 | 4 | 15 | 16 => {
                    current_block = 2415380317517078313; /*633:*/
                    match current_block {
                        15378387224937501455 => {
                            if MEM[(p + 2) as usize].b32.s1 < dvi_gone {
                                current_block = 18071914750955744041;
                                break;
                            }
                            k = MEM[(p + 2) as usize].b32.s1 - dvi_offset;
                            if k < 0i32 {
                                k = k + 16384i32
                            }
                            *dvi_buf.offset(k as isize) =
                                (*dvi_buf.offset(k as isize) as i32 + 10i32) as u8;
                            MEM[p as usize].b32.s0 = 2;
                            current_block = 8542251818650148540;
                            break;
                        }
                        _ => {
                            if MEM[(p + 2) as usize].b32.s1 < dvi_gone {
                                current_block = 18071914750955744041;
                                break;
                            } else {
                                k = MEM[(p + 2) as usize].b32.s1 - dvi_offset;
                                if k < 0i32 {
                                    k = k + 16384i32
                                }
                                *dvi_buf.offset(k as isize) =
                                    (*dvi_buf.offset(k as isize) as i32 + 5i32) as u8;
                                MEM[p as usize].b32.s0 = 1;
                                current_block = 8542251818650148540;
                                break;
                            }
                        }
                    }
                }
                5 | 9 | 11 => {
                    current_block = 15378387224937501455;
                    if MEM[(p + 2) as usize].b32.s1 < dvi_gone {
                        current_block = 18071914750955744041;
                        break;
                    }
                    k = MEM[(p + 2) as usize].b32.s1 - dvi_offset;
                    if k < 0i32 {
                        k = k + 16384i32
                    }
                    *dvi_buf.offset(k as isize) =
                        (*dvi_buf.offset(k as isize) as i32 + 10i32) as u8;
                    MEM[p as usize].b32.s0 = 2;
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
            match mstate as i32 + MEM[p as usize].b32.s0 {
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
        p = MEM[p as usize].b32.s1
    }
    match current_block {
        8542251818650148540 => {
            /*629:*/
            MEM[q as usize].b32.s0 = MEM[p as usize].b32.s0; /*634:*/
            if MEM[q as usize].b32.s0 == 1 {
                dvi_out((o as i32 + 4i32) as u8); /* max_selector enum */
                while MEM[q as usize].b32.s1 != p {
                    q = MEM[q as usize].b32.s1;
                    match MEM[q as usize].b32.s0 {
                        3 => MEM[q as usize].b32.s0 = 5,
                        4 => MEM[q as usize].b32.s0 = 6,
                        _ => {}
                    }
                }
            } else {
                dvi_out((o as i32 + 9i32) as u8);
                while MEM[q as usize].b32.s1 != p {
                    q = MEM[q as usize].b32.s1;
                    match MEM[q as usize].b32.s0 {
                        3 => MEM[q as usize].b32.s0 = 4,
                        5 => MEM[q as usize].b32.s0 = 6,
                        _ => {}
                    }
                }
            }
            return;
        }
        _ => {
            MEM[q as usize].b32.s0 = 3;
            if w.abs() >= 0x800000i32 {
                dvi_out((o as i32 + 3i32) as u8);
                dvi_four(w);
                return;
            }
            if w.abs() >= 0x8000i32 {
                dvi_out((o as i32 + 2i32) as u8);
                if w < 0i32 {
                    w = w + 0x1000000i32
                }
                dvi_out((w / 0x10000i32) as u8);
                w = w % 0x10000i32;
                current_block = 14567512515169274304;
            } else if w.abs() >= 128i32 {
                dvi_out((o as i32 + 1i32) as u8);
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
                    dvi_out((w / 256i32) as u8);
                }
                _ => {}
            }
            dvi_out((w % 256i32) as u8);
            return;
        }
    };
}

unsafe extern "C" fn prune_movements(l: i32) {
    while down_ptr != TEX_NULL {
        if MEM[(down_ptr + 2) as usize].b32.s1 < l {
            break;
        }

        let p = down_ptr;
        down_ptr = MEM[p as usize].b32.s1;
        free_node(p, MOVEMENT_NODE_SIZE);
    }
    while right_ptr != TEX_NULL {
        if MEM[(right_ptr + 2) as usize].b32.s1 < l {
            return;
        }
        let p = right_ptr;
        right_ptr = MEM[p as usize].b32.s1;
        free_node(p, MOVEMENT_NODE_SIZE);
    }
}

unsafe fn special_out(mut p: i32) {
    if cur_h != dvi_h {
        movement(cur_h - dvi_h, RIGHT1 as _);
        dvi_h = cur_h
    }
    if cur_v != dvi_v {
        movement(cur_v - dvi_v, DOWN1 as _);
        dvi_v = cur_v
    }
    doing_special = true;
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    show_token_list(
        MEM[MEM[(p + 1) as usize].b32.s1 as usize].b32.s1,
        TEX_NULL,
        pool_size - pool_ptr,
    );
    selector = old_setting;

    if pool_ptr + 1 > pool_size {
        overflow(b"pool size", pool_size - init_pool_ptr);
    }

    if cur_length() < 256 {
        dvi_out(XXX1 as u8);
        dvi_out(cur_length() as u8);
    } else {
        dvi_out(XXX4 as u8);
        dvi_four(cur_length());
    }

    {
        let mut for_end: i32 = 0;
        let mut k = *str_start.offset((str_ptr - TOO_BIG_CHAR) as isize);
        for_end = pool_ptr - 1;
        if k <= for_end {
            loop {
                dvi_out(*str_pool.offset(k as isize) as u8);
                let fresh8 = k;
                k = k + 1;
                if !(fresh8 < for_end) {
                    break;
                }
            }
        }
    }
    pool_ptr = *str_start.offset((str_ptr - TOO_BIG_CHAR) as isize);
    doing_special = false;
}

unsafe extern "C" fn write_out(mut p: i32) {
    let mut q = get_avail();
    MEM[q as usize].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
    let mut r = get_avail();
    MEM[q as usize].b32.s1 = r;
    MEM[r as usize].b32.s0 = CS_TOKEN_FLAG + END_WRITE;
    begin_token_list(q, INSERTED);
    begin_token_list(MEM[(p + 1) as usize].b32.s1, WRITE_TEXT);
    q = get_avail();
    MEM[q as usize].b32.s0 = LEFT_BRACE_TOKEN + '{' as i32;
    begin_token_list(q, INSERTED);

    let old_mode = cur_list.mode as i32;
    cur_list.mode = 0;
    cur_cs = write_loc;
    q = scan_toks(false, true);
    get_token();

    if cur_tok != CS_TOKEN_FLAG + END_WRITE {
        /*1412:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Unbalanced write command");
        help_ptr = 2;
        help_line[1] = b"On this page there\'s a \\write with fewer real {\'s than }\'s.";
        help_line[0] = b"I can\'t handle that very well; good luck.";
        error();

        loop {
            get_token();
            if !(cur_tok != CS_TOKEN_FLAG + END_WRITE) {
                break;
            }
        }
    }

    cur_list.mode = old_mode as i16;
    end_token_list();
    let old_setting = selector;
    let j = MEM[(p + 1) as usize].b32.s0 as small_number;

    if j == 18 {
        selector = Selector::NEW_STRING
    } else if write_open[j as usize] {
        selector = (j as u8).into()
    } else {
        if j == 17 && (selector == Selector::TERM_AND_LOG) {
            selector = Selector::LOG_ONLY
        }
        print_nl_cstr(b"");
    }

    token_show(def_ref);
    print_ln();
    flush_list(def_ref);

    if j == 18 {
        if INTPAR(INT_PAR__tracing_online) <= 0 {
            selector = Selector::LOG_ONLY
        } else {
            selector = Selector::TERM_AND_LOG
        }
        if !log_opened {
            selector = Selector::TERM_ONLY
        }

        print_nl_cstr(b"runsystem(");
        let mut d = 0;
        while d <= cur_length() - 1 {
            print(
                *str_pool
                    .offset((*str_start.offset((str_ptr - TOO_BIG_CHAR) as isize) + d) as isize)
                    as i32,
            );
            d += 1
        }
        print_cstr(b")...");
        print_cstr(b"disabled");
        print_char('.' as i32);
        print_nl_cstr(b"");
        print_ln();
        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize)
    }
    selector = old_setting;
}

unsafe fn pic_out(mut p: i32) {
    let mut i: i32 = 0;

    if cur_h != dvi_h {
        movement(cur_h - dvi_h, RIGHT1 as u8);
        dvi_h = cur_h
    }

    if cur_v != dvi_v {
        movement(cur_v - dvi_v, DOWN1 as u8);
        dvi_v = cur_v
    }

    let old_setting = selector;
    selector = Selector::NEW_STRING;
    print_cstr(b"pdf:image ");
    print_cstr(b"matrix ");
    print_scaled(MEM[(p + 5) as usize].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[(p + 5) as usize].b32.s1);
    print(' ' as i32);
    print_scaled(MEM[(p + 6) as usize].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[(p + 6) as usize].b32.s1);
    print(' ' as i32);
    print_scaled(MEM[(p + 7) as usize].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[(p + 7) as usize].b32.s1);
    print(' ' as i32);
    print_cstr(b"page ");
    print_int(MEM[(p + 4) as usize].b16.s0 as i32);
    print(' ' as i32);

    match MEM[(p + 8) as usize].b16.s1 {
        1 => {
            print_cstr(b"pagebox cropbox ");
        }
        2 => {
            print_cstr(b"pagebox mediabox ");
        }
        3 => {
            print_cstr(b"pagebox bleedbox ");
        }
        5 => {
            print_cstr(b"pagebox artbox ");
        }
        4 => {
            print_cstr(b"pagebox trimbox ");
        }
        _ => {}
    }

    print('(' as i32);
    i = 0i32;
    while i < MEM[(p + 4) as usize].b16.s1 as i32 {
        print_raw_char(
            *(&mut MEM[(p + 9) as usize] as *mut memory_word as *mut u8).offset(i as isize)
                as UTF16_code,
            true,
        );
        i += 1
    }
    print(')' as i32);

    selector = old_setting;
    if cur_length() < 256 {
        dvi_out(XXX1 as _);
        dvi_out(cur_length() as u8);
    } else {
        dvi_out(XXX4 as _);
        dvi_four(cur_length());
    }

    let mut k = *str_start.offset((str_ptr - TOO_BIG_CHAR) as isize);
    while k < pool_ptr {
        dvi_out(*str_pool.offset(k as isize) as u8);
        k += 1
    }
    pool_ptr = *str_start.offset((str_ptr - TOO_BIG_CHAR) as isize); /* discard the string we just made */
}

#[no_mangle]
pub(crate) unsafe fn finalize_dvi_file() {
    while cur_s > -1 {
        if cur_s > 0 {
            dvi_out(POP as u8);
        } else {
            dvi_out(EOP as u8);
            total_pages += 1
        }
        cur_s -= 1
    }

    if total_pages == 0 {
        print_nl_cstr(b"No pages of output.");
        return;
    }

    if cur_s == -2 {
        /* This happens when the DVI gets too big; a message has already been printed */
        return;
    }

    dvi_out(POST as u8); /* magic values: conversion ratio for sp */
    dvi_four(last_bop);
    last_bop = dvi_offset + dvi_ptr - 5;
    dvi_four(25400000); /* magic values: conversion ratio for sp */
    dvi_four(473628672i64 as i32); /* magic values: conversion ratio for sp */
    prepare_mag();
    dvi_four(INTPAR(INT_PAR__mag));
    dvi_four(max_v);
    dvi_four(max_h);
    dvi_out((max_push / 256) as u8);
    dvi_out((max_push % 256) as u8);
    dvi_out((total_pages / 256i32 % 256i32) as u8);
    dvi_out((total_pages % 256i32) as u8);

    while font_ptr > 0i32 {
        if *font_used.offset(font_ptr as isize) {
            dvi_font_def(font_ptr);
        }
        font_ptr -= 1
    }

    dvi_out(POST_POST as u8);
    dvi_four(last_bop);

    if semantic_pagination_enabled {
        dvi_out(SPX_ID_BYTE as u8);
    } else {
        dvi_out(XDV_ID_BYTE as u8);
    }

    let mut k = (4 + (DVI_BUF_SIZE - dvi_ptr) % 4) as u8;

    while k > 0 {
        dvi_out(223);
        k -= 1;
    }

    if dvi_limit == HALF_BUF {
        write_to_dvi(HALF_BUF, DVI_BUF_SIZE - 1);
    }

    if dvi_ptr > TEX_INFINITY - dvi_offset {
        cur_s = -2;
        fatal_error(b"dvi length exceeds 0x7FFFFFFF");
    }

    if dvi_ptr > 0 {
        write_to_dvi(0, dvi_ptr - 1);
    }

    let mut k = ttstub_output_close(dvi_file.take().unwrap()) as u8;

    if k == 0 {
        print_nl_cstr(b"Output written on ");
        print(output_file_name);
        print_cstr(b" (");
        print_int(total_pages);
        if total_pages != 1i32 {
            print_cstr(b" pages");
        } else {
            print_cstr(b" page");
        }
        print_cstr(b", ");
        print_int(dvi_offset + dvi_ptr);
        print_cstr(b" bytes).");
    } else {
        print_nl_cstr(b"Error ");
        print_int(k as i32);
        print_cstr(b" (");
        print_c_string(strerror(k as i32));
        print_cstr(b") generating output;");
        print_nl_cstr(b"file ");
        print(output_file_name);
        print_cstr(b" may not be valid.");
        /* XeTeX adds history = OUTPUT_FAILURE = 4 here; I'm not implementing that. */
    };
}

unsafe fn write_to_dvi(a: i32, b: i32) {
    let mut n: i32 = b - a + 1;
    let mut v = Vec::<u8>::new();
    for i in 0..n {
        v.push(*dvi_buf.offset((a + i) as isize));
    }
    dvi_file
        .as_mut()
        .unwrap()
        .write(&v)
        .expect("failed to write data to XDV file");
}

unsafe fn dvi_swap() {
    if dvi_ptr > TEX_INFINITY - dvi_offset {
        cur_s = -2;
        fatal_error(b"dvi length exceeds 0x7FFFFFFF");
    }
    if dvi_limit == DVI_BUF_SIZE {
        write_to_dvi(0, HALF_BUF - 1);
        dvi_limit = HALF_BUF;
        dvi_offset = dvi_offset + DVI_BUF_SIZE;
        dvi_ptr = 0;
    } else {
        write_to_dvi(HALF_BUF, DVI_BUF_SIZE - 1);
        dvi_limit = DVI_BUF_SIZE;
    }
    dvi_gone = dvi_gone + HALF_BUF;
}

unsafe fn dvi_four(mut x: i32) {
    if x >= 0 {
        dvi_out((x / 0x1000000) as u8);
    } else {
        x = x + 0x40000000;
        x = x + 0x40000000;
        dvi_out((x / 0x1000000 + 128) as u8);
    }

    x = x % 0x1000000;
    dvi_out((x / 0x10000) as u8);

    x = x % 0x10000;
    dvi_out((x / 0x100) as u8);
    dvi_out((x % 0x100) as u8);
}

unsafe fn dvi_two(s: UTF16_code) {
    dvi_out((s as i32 / 0x100) as u8);
    dvi_out((s as i32 % 0x100) as u8);
}

unsafe fn dvi_pop(l: i32) {
    if l == dvi_offset + dvi_ptr && dvi_ptr > 0 {
        dvi_ptr -= 1
    } else {
        dvi_out(POP as u8);
    };
}
