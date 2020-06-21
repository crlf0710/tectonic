use bridge::{abort, DisplayExt};
use std::ffi::CStr;
use std::io::Write;

use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow};
use crate::xetex_ext::{
    apply_tfm_font_mapping, makeXDVGlyphArrayData, make_font_def, store_justified_native_glyphs,
    AAT_FONT_FLAG, OTGR_FONT_FLAG,
};
use crate::xetex_ini::{
    avail, cur_area, cur_cs, cur_dir, cur_ext, cur_h, cur_h_offset, cur_list, cur_name,
    cur_page_height, cur_page_width, cur_tok, cur_v, cur_v_offset, dead_cycles, def_ref,
    doing_leaders, doing_special, file_line_error_style_p, file_offset, font_used, help_line,
    help_ptr, init_pool_ptr, job_name, last_bop, log_opened, max_h, max_print_line, max_push,
    max_v, name_of_file, output_file_extension, pdf_last_x_pos, pdf_last_y_pos, pool_ptr,
    pool_size, rule_dp, rule_ht, rule_wd, rust_stdout, selector, semantic_pagination_enabled,
    str_pool, str_ptr, str_start, temp_ptr, term_offset, write_file, write_loc, write_open,
    xdv_buffer, xtx_ligature_present, LR_problems, LR_ptr, CHAR_BASE, FONT_AREA, FONT_BC,
    FONT_CHECK, FONT_DSIZE, FONT_EC, FONT_GLUE, FONT_INFO, FONT_LETTER_SPACE, FONT_MAPPING,
    FONT_NAME, FONT_PTR, FONT_SIZE, MEM, TOTAL_PAGES, WIDTH_BASE,
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
    scaled_t, scan_toks, show_box, show_token_list, str_number, token_show, UTF16_code,
};
use crate::xetex_xetexd::{
    is_char_node, kern_NODE_subtype, print_c_string, set_BOX_lr_mode,
    /*set_NODE_subtype, */ set_NODE_type, text_NODE_type, whatsit_NODE_subtype, BOX_depth,
    BOX_glue_order, BOX_glue_set, BOX_glue_sign, BOX_height, BOX_list_ptr, BOX_lr_mode,
    BOX_shift_amount, BOX_width, CHAR_NODE_character, CHAR_NODE_font, EDGE_NODE_edge_dist,
    GLUE_NODE_glue_ptr, GLUE_NODE_leader_ptr, GLUE_SPEC_ref_count, GLUE_SPEC_shrink,
    GLUE_SPEC_shrink_order, GLUE_SPEC_stretch, GLUE_SPEC_stretch_order, LIGATURE_NODE_lig_ptr,
    LLIST_info, LLIST_link, NATIVE_NODE_font, NATIVE_NODE_glyph, NATIVE_NODE_glyph_info_ptr,
    NATIVE_NODE_length, NODE_type, SYNCTEX_tag, TeXInt, TeXOpt, FONT_CHARACTER_WIDTH,
};
use bridge::{ttstub_output_close, ttstub_output_open};
use libc::{strerror, strlen};

use bridge::OutputHandleWrapper;

const DVI_BUF_SIZE: usize = 16384;
const HALF_BUF: usize = 8192;
const FNT_NUM_0: u8 = 171; /* DVI code */

static mut dvi_file: Option<OutputHandleWrapper> = None;
static mut output_file_name: str_number = 0;
static mut dvi_buf: Vec<u8> = Vec::new();
static mut dvi_limit: usize = 0;
static mut g: usize = 0;
static mut lq: i32 = 0;
static mut lr: i32 = 0;
static mut dvi_ptr: usize = 0;
static mut dvi_offset: usize = 0;
static mut dvi_gone: i32 = 0;
static mut down_ptr: Option<usize> = Some(0);
static mut right_ptr: i32 = 0;
static mut dvi_h: scaled_t = 0;
static mut dvi_v: scaled_t = 0;
static mut dvi_f: internal_font_number = 0;
static mut cur_s: i32 = 0;

pub(crate) unsafe fn initialize_shipout_variables() {
    output_file_name = 0;
    dvi_buf = vec![0; DVI_BUF_SIZE as usize + 2]; // TODO: check size
    dvi_limit = DVI_BUF_SIZE;
    dvi_ptr = 0;
    dvi_offset = 0;
    dvi_gone = 0;
    down_ptr = None;
    right_ptr = None.tex_int();
    cur_s = -1;
}

pub(crate) unsafe fn deinitialize_shipout_variables() {
    dvi_buf = Vec::new();
}

#[inline]
unsafe fn dvi_out(c: u8) {
    dvi_buf[dvi_ptr] = c;
    dvi_ptr += 1;
    if dvi_ptr == dvi_limit {
        dvi_swap();
    };
}

/*660: output the box `p` */
pub(crate) unsafe fn ship_out(p: usize) {
    let mut s: pool_pointer = 0;
    let mut l: u8 = 0;
    let mut output_comment: *const i8 = b"tectonic\x00" as *const u8 as *const i8;

    synctex_sheet(*INTPAR(IntPar::mag));

    if job_name == 0 {
        open_log_file();
    }

    if *INTPAR(IntPar::tracing_output) > 0 {
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
    let mut j = 9;
    while j > 0 && *COUNT_REG(j as _) == 0 {
        j -= 1;
    }

    for k in 0..=j {
        print_int(*COUNT_REG(k as _));
        if k < j {
            print_char('.' as i32);
        }
    }

    rust_stdout.as_mut().unwrap().flush().unwrap();

    if *INTPAR(IntPar::tracing_output) > 0 {
        print_char(']' as i32);
        begin_diagnostic();
        show_box(p as i32);
        end_diagnostic(true);
    }

    /*662: "Ship box `p` out." */
    /*663: "Update the values of max_h and max_v; but if the page is too
     * large, goto done". */

    if *BOX_height(p) > MAX_HALFWORD
        || *BOX_depth(p) > MAX_HALFWORD
        || *BOX_height(p) + *BOX_depth(p) + *DIMENPAR(DimenPar::v_offset) > MAX_HALFWORD
        || *BOX_width(p) + *DIMENPAR(DimenPar::h_offset) > MAX_HALFWORD
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

        if *INTPAR(IntPar::tracing_output) <= 0 {
            begin_diagnostic();
            print_nl_cstr(b"The following box has been deleted:");
            show_box(p as i32);
            end_diagnostic(true);
        }
    } else {
        if *BOX_height(p) + *BOX_depth(p) + *DIMENPAR(DimenPar::v_offset) > max_v {
            max_v = *BOX_height(p) + *BOX_depth(p) + *DIMENPAR(DimenPar::v_offset);
        }
        if *BOX_width(p) + *DIMENPAR(DimenPar::h_offset) > max_h {
            max_h = *BOX_width(p) + *DIMENPAR(DimenPar::h_offset);
        }

        /*637: "Initialize variables as ship_out begins." */

        dvi_h = 0;
        dvi_v = 0;
        cur_h = *DIMENPAR(DimenPar::h_offset);
        dvi_f = 0;

        /*1405: "Calculate page dimensions and margins" */
        /* 4736287 = round(0xFFFF * 72.27) ; i.e., 1 inch expressed as a scaled_t */
        cur_h_offset = *DIMENPAR(DimenPar::h_offset) + 4736287;
        cur_v_offset = *DIMENPAR(DimenPar::v_offset) + 4736287;

        if *DIMENPAR(DimenPar::pdf_page_width) != 0 {
            cur_page_width = *DIMENPAR(DimenPar::pdf_page_width);
        } else {
            cur_page_width = *BOX_width(p as usize) + 2 * cur_h_offset;
        }
        if *DIMENPAR(DimenPar::pdf_page_height) != 0 {
            cur_page_height = *DIMENPAR(DimenPar::pdf_page_height);
        } else {
            cur_page_height = *BOX_height(p as usize) + *BOX_depth(p as usize) + 2 * cur_v_offset;
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

        if TOTAL_PAGES == 0 {
            dvi_out(PRE);
            if semantic_pagination_enabled {
                dvi_out(SPX_ID_BYTE);
            } else {
                dvi_out(XDV_ID_BYTE);
            }

            dvi_four(25400000); /* magic values: conversion ratio for sp */
            dvi_four(473628672); /* magic values: conversion ratio for sp */

            prepare_mag();
            dvi_four(*INTPAR(IntPar::mag));

            l = strlen(output_comment) as u8;
            dvi_out(l);
            s = 0i32;
            while s < l as i32 {
                dvi_out(*output_comment.offset(s as isize) as u8);
                s += 1
            }
        }

        /* ... resuming 662 ... Emit per-page preamble. */

        let page_loc = dvi_offset + dvi_ptr;

        dvi_out(BOP);

        for k in 0..10 {
            dvi_four(*COUNT_REG(k));
        }

        dvi_four(last_bop);
        last_bop = page_loc as i32;

        /* Generate a PDF pagesize special unilaterally */

        let old_setting = selector;
        selector = Selector::NEW_STRING;
        print_cstr(b"pdf:pagesize ");
        if *DIMENPAR(DimenPar::pdf_page_width) <= 0 || *DIMENPAR(DimenPar::pdf_page_height) <= 0 {
            print_cstr(b"default");
        } else {
            print_cstr(b"width");
            print(' ' as i32);
            print_scaled(*DIMENPAR(DimenPar::pdf_page_width));
            print_cstr(b"pt");
            print(' ' as i32);
            print_cstr(b"height");
            print(' ' as i32);
            print_scaled(*DIMENPAR(DimenPar::pdf_page_height));
            print_cstr(b"pt");
        }
        selector = old_setting;

        dvi_out(XXX1);
        dvi_out(cur_length() as u8);

        s = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        while s < pool_ptr {
            dvi_out(str_pool[s as usize] as u8);
            s += 1
        }

        pool_ptr = str_start[(str_ptr - 65536) as usize];

        /* Done with the synthesized special. The meat: emit this page box. */

        cur_v = *BOX_height(p) + *DIMENPAR(DimenPar::v_offset); /*"Does this need changing for upwards mode???"*/
        temp_ptr = p as i32;
        if NODE_type(p) == TextNode::VList.into() {
            vlist_out();
        } else {
            hlist_out();
        }

        dvi_out(EOP);
        TOTAL_PAGES += 1;
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

    if !LR_ptr.is_texnull() || cur_dir != LR::LeftToRight {
        confusion(b"LR3");
    }

    if *INTPAR(IntPar::tracing_output) <= 0 {
        print_char(']' as i32);
    }

    dead_cycles = 0;
    rust_stdout.as_mut().unwrap().flush().unwrap();
    flush_node_list(Some(p));
    synctex_teehs();
}

/*639: Output an hlist */
unsafe fn hlist_out() {
    let mut current_block: u64;
    let mut base_line: scaled_t = 0;
    let mut left_edge: scaled_t = 0;
    let mut save_h: scaled_t = 0;
    let mut save_v: scaled_t = 0;
    let mut save_loc: usize = 0;
    let mut leader_box: i32 = 0;
    let mut leader_wd: scaled_t = 0;
    let mut lx: scaled_t = 0;
    let mut outer_doing_leaders: bool = false;
    let mut edge: scaled_t = 0;
    let mut prev_p: i32 = 0;
    let mut len: i32 = 0;
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
    let mut g_sign = GlueSign::from(*BOX_glue_sign(this_box as _));

    if *INTPAR(IntPar::xetex_interword_space_shaping) > 1 {
        /*640: "Extra stuff for justifiable AAT text..." "Merge sequences of
         * words using native fonts and inter-word spaces into single
         * nodes" */

        let mut p = *BOX_list_ptr(this_box as _);
        prev_p = this_box + 5; /* this gets the list within the box */

        while !p.is_texnull() {
            if !LLIST_link(p as usize).is_texnull() {
                if !p.is_texnull()
                    && !is_char_node(p)
                    && NODE_type(p as usize) != TextNode::WhatsIt.into()
                    && (whatsit_NODE_subtype(p as usize) == WhatsItNST::NativeWord
                        || whatsit_NODE_subtype(p as usize) == WhatsItNST::NativeWordAt)
                    && FONT_LETTER_SPACE[MEM[(p + 4) as usize].b16.s2 as usize] == 0
                {
                    /* "got a word in an AAT font, might be the start of a run" */
                    r = p;
                    k = MEM[(r + 4) as usize].b16.s1 as i32;
                    let mut q = *LLIST_link(p as usize);
                    loop {
                        /*641: "Advance `q` past ignorable nodes." This test is
                         * mostly `node_is_invisible_to_interword_space`. 641 is
                         * reused a few times here. */
                        while !q.is_texnull()
                            && !is_char_node(q)
                            && (NODE_type(q as usize) == TextNode::Penalty.into()
                                || NODE_type(q as usize) == TextNode::Ins.into()
                                || NODE_type(q as usize) == TextNode::Mark.into()
                                || NODE_type(q as usize) == TextNode::Adjust.into()
                                || (NODE_type(q as usize) == TextNode::WhatsIt.into()
                                    && [
                                        WhatsItNST::Open,
                                        WhatsItNST::Write,
                                        WhatsItNST::Close,
                                        WhatsItNST::Special,
                                        WhatsItNST::Language,
                                    ]
                                    .contains(&whatsit_NODE_subtype(q as usize))))
                        {
                            q = *LLIST_link(q as usize);
                        }
                        if !(!q.is_texnull() && !is_char_node(q)) {
                            break;
                        }
                        if NODE_type(q as usize) == TextNode::Glue.into()
                            && *GLUE_SPEC_shrink_order(q as usize) == GlueOrder::Normal as _
                        {
                            if *GLUE_NODE_glue_ptr(q as usize)
                                == FONT_GLUE[MEM[(r + 4) as usize].b16.s2 as usize]
                            {
                                /* "Found a normal space; if the next node is
                                 * another word in the same font, we'll
                                 * merge." */
                                q = *LLIST_link(q as usize);

                                while !q.is_texnull()
                                    && !is_char_node(q)
                                    && (NODE_type(q as usize) == TextNode::Penalty.into()
                                        || NODE_type(q as usize) == TextNode::Ins.into()
                                        || NODE_type(q as usize) == TextNode::Mark.into()
                                        || NODE_type(q as usize) == TextNode::Adjust.into()
                                        || (NODE_type(q as usize) == TextNode::WhatsIt.into()
                                            && [
                                                WhatsItNST::Open,
                                                WhatsItNST::Write,
                                                WhatsItNST::Close,
                                                WhatsItNST::Special,
                                                WhatsItNST::Language,
                                            ]
                                            .contains(&whatsit_NODE_subtype(q as usize))))
                                {
                                    q = *LLIST_link(q as usize);
                                }

                                if !q.is_texnull()
                                    && !is_char_node(q)
                                    && NODE_type(q as usize) == TextNode::WhatsIt.into()
                                    && (whatsit_NODE_subtype(q as usize) == WhatsItNST::NativeWord
                                        || whatsit_NODE_subtype(q as usize)
                                            == WhatsItNST::NativeWordAt)
                                    && MEM[(q + 4) as usize].b16.s2 as i32
                                        == MEM[(r + 4) as usize].b16.s2 as i32
                                {
                                    p = q;
                                    k += 1 + MEM[(q + 4) as usize].b16.s1 as i32;
                                    q = *LLIST_link(q as usize);
                                    continue;
                                }
                            } else {
                                q = *LLIST_link(q as usize);
                            }
                            if !(!q.is_texnull()
                                && !is_char_node(q)
                                && NODE_type(q as usize) == TextNode::Kern.into()
                                && kern_NODE_subtype(q as usize) == KernNST::SpaceAdjustment)
                            {
                                break;
                            }
                            q = MEM[q as usize].b32.s1;
                            while !q.is_texnull()
                                && !is_char_node(q)
                                && (NODE_type(q as usize) == TextNode::Penalty.into()
                                    || NODE_type(q as usize) == TextNode::Ins.into()
                                    || NODE_type(q as usize) == TextNode::Mark.into()
                                    || NODE_type(q as usize) == TextNode::Adjust.into()
                                    || NODE_type(q as usize) == TextNode::WhatsIt.into()
                                        && whatsit_NODE_subtype(q as usize) as u16 <= 4)
                            {
                                q = *LLIST_link(q as usize);
                            }
                            if !(!q.is_texnull()
                                && !is_char_node(q)
                                && NODE_type(q as usize) == TextNode::WhatsIt.into()
                                && (whatsit_NODE_subtype(q as usize) == WhatsItNST::NativeWord
                                    || whatsit_NODE_subtype(q as usize)
                                        == WhatsItNST::NativeWordAt)
                                && *NATIVE_NODE_font(q as usize) == *NATIVE_NODE_font(r as usize))
                            {
                                break;
                            }
                            p = q;
                            k += (1 + *NATIVE_NODE_length(q as usize)) as i32;
                            q = *LLIST_link(q as usize);
                        } else {
                            if !(!q.is_texnull()
                                && !is_char_node(q)
                                && NODE_type(q as usize) == TextNode::WhatsIt.into()
                                && (whatsit_NODE_subtype(q as usize) == WhatsItNST::NativeWord
                                    || whatsit_NODE_subtype(q as usize)
                                        == WhatsItNST::NativeWordAt)
                                && *NATIVE_NODE_font(q as usize) == *NATIVE_NODE_font(r as usize))
                            {
                                break;
                            }
                            p = q;
                            q = *LLIST_link(q as usize);
                        }
                    }
                    /* "Now r points to the first native_word_node of the run,
                     * and p to the last." */
                    if p != r {
                        if pool_ptr + k > pool_size {
                            overflow(b"pool size", (pool_size - init_pool_ptr) as usize);
                        }
                        k = 0;
                        let mut q = r;
                        loop {
                            if NODE_type(q as usize) == TextNode::WhatsIt.into() {
                                if whatsit_NODE_subtype(q as usize) == WhatsItNST::NativeWord
                                    || whatsit_NODE_subtype(q as usize) == WhatsItNST::NativeWordAt
                                {
                                    j = 0;
                                    while j < *NATIVE_NODE_length(q as usize) as i32 {
                                        str_pool[pool_ptr as usize] = *(&mut MEM[(q + 6) as usize]
                                            as *mut memory_word
                                            as *mut u16)
                                            .offset(j as isize);
                                        pool_ptr += 1;
                                        j += 1
                                    }
                                    k += *BOX_width(q as usize);
                                }
                            } else if NODE_type(q as usize) == TextNode::Glue.into() {
                                str_pool[pool_ptr as usize] = ' ' as i32 as packed_UTF16_code;
                                pool_ptr += 1;
                                g = *GLUE_NODE_glue_ptr(q as usize) as usize;
                                k += *BOX_width(g);
                                if g_sign != GlueSign::Normal {
                                    if g_sign == GlueSign::Stretching {
                                        if *GLUE_SPEC_stretch_order(g) == g_order as u16 {
                                            k += tex_round(
                                                *BOX_glue_set(this_box as usize)
                                                    * *GLUE_SPEC_stretch(g) as f64,
                                            )
                                        }
                                    } else if *GLUE_SPEC_shrink_order(g) == g_order as u16 {
                                        k -= tex_round(
                                            *BOX_glue_set(this_box as usize)
                                                * *GLUE_SPEC_shrink(g) as f64,
                                        )
                                    }
                                }
                            } else if NODE_type(q as usize) == TextNode::Kern.into() {
                                k += *BOX_width(q as usize);
                            }
                            if q == p {
                                break;
                            }
                            q = *LLIST_link(q as usize);
                        }
                        let mut q = new_native_word_node(
                            MEM[(r + 4) as usize].b16.s2 as internal_font_number,
                            cur_length(),
                        ) as i32;
                        //set_NODE_subtype(q as usize, NODE_subtype(r as usize));
                        MEM[q as usize].b16.s0 = MEM[r as usize].b16.s0;

                        j = 0;
                        while j < cur_length() {
                            *(&mut MEM[(q + 6) as usize] as *mut memory_word as *mut u16)
                                .offset(j as isize) = str_pool
                                [(str_start[(str_ptr - TOO_BIG_CHAR) as usize] + j) as usize];
                            j += 1;
                        }
                        /* "Link q into the list in place of r...p" */
                        *BOX_width(q as usize) = k;
                        store_justified_native_glyphs(
                            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
                        );
                        *LLIST_link(prev_p as usize) = q;
                        *LLIST_link(q as usize) = *LLIST_link(p as usize);
                        *LLIST_link(p as usize) = None.tex_int();
                        prev_p = r;
                        let mut popt2 = LLIST_link(r as usize).opt();

                        /* "Extract any 'invisible' nodes from the old list
                         * and insert them after the new node, so we don't
                         * lose them altogether. Note that the first node
                         * cannot be one of these, as we always start merging
                         * at a native_word node." */

                        while let Some(p) = popt2 {
                            if !is_char_node(p as i32)
                                && (NODE_type(p) == TextNode::Penalty.into()
                                    || NODE_type(p) == TextNode::Ins.into()
                                    || NODE_type(p) == TextNode::Mark.into()
                                    || NODE_type(p) == TextNode::Adjust.into()
                                    || NODE_type(p) == TextNode::WhatsIt.into()
                                        && whatsit_NODE_subtype(p) as u16 <= 4)
                            {
                                *LLIST_link(prev_p as usize) = *LLIST_link(p);
                                *LLIST_link(p) = *LLIST_link(q as usize);
                                *LLIST_link(q as usize) = p as i32;
                                q = p as i32;
                            }
                            prev_p = p as i32;
                            popt2 = LLIST_link(p as usize).opt();
                        }
                        flush_node_list(r.opt());
                        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                        p = q
                    }
                }
                prev_p = p;
            }
            p = *LLIST_link(p as usize);
        }
    }

    /* ... resuming 639 ... */

    let mut p = *BOX_list_ptr(this_box as _);
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

    temp_ptr = get_avail() as i32;
    *LLIST_info(temp_ptr as usize) = BEFORE as i32;
    *LLIST_link(temp_ptr as usize) = LR_ptr;
    LR_ptr = temp_ptr;
    if BOX_lr_mode(this_box as usize) == LRMode::DList {
        if cur_dir == LR::RightToLeft {
            cur_dir = LR::LeftToRight;
            cur_h -= *BOX_width(this_box as usize);
        } else {
            MEM[this_box as usize].b16.s0 = 0;
        }
    }
    if cur_dir == LR::RightToLeft && BOX_lr_mode(this_box as usize) != LRMode::Reversed {
        /*1508: "Reverse the complete hlist and set the subtype to reversed." */
        save_h = cur_h; /* "SyncTeX: do nothing, it is too late" */
        temp_ptr = p;
        p = new_kern(0) as i32;
        *SYNCTEX_tag(p as usize, MEDIUM_NODE_SIZE) = 0;
        *LLIST_link(prev_p as usize) = p;
        cur_h = 0;
        *LLIST_link(p as usize) = reverse(this_box, None.tex_int(), &mut cur_g, &mut cur_glue);
        MEM[(p + 1) as usize].b32.s1 = -cur_h;
        cur_h = save_h;
        set_BOX_lr_mode(this_box as usize, LRMode::Reversed);
    }

    /* ... resuming 639 ... */

    left_edge = cur_h;
    synctex_hlist(this_box);

    's_726: while !p.is_texnull() {
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
                        movement(cur_h - dvi_h, RIGHT1);
                        dvi_h = cur_h
                    }
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, DOWN1);
                        dvi_v = cur_v
                    }
                    loop  {
                        f = *CHAR_NODE_font(p as usize) as usize;
                        c = *CHAR_NODE_character(p as usize);
                        if p != LIG_TRICK as i32 &&
                               !(FONT_MAPPING[f]).is_null() {
                            c = apply_tfm_font_mapping(FONT_MAPPING[f], c as i32) as u16;
                        }
                        if f != dvi_f {
                            /*643: "Change font dvi_f to f" */
                            if !font_used[f] {
                                dvi_font_def(f);
                                font_used[f] = true
                            }
                            if f <= 64 {
                                dvi_out(f as u8 + FNT_NUM_0 - 1);
                            } else if f <= 256 {
                                dvi_out(FNT1);
                                dvi_out((f - 1) as u8);
                            } else {
                                dvi_out(FNT1 + 1);
                                dvi_out(((f - 1) / 256) as u8);
                                dvi_out(((f - 1) % 256) as u8);
                            }
                            dvi_f = f
                        }
                        if FONT_EC[f] as i32 >=
                               c as i32 {
                            if FONT_BC[f] as i32 <=
                                   c as i32 {
                                if FONT_INFO[(CHAR_BASE[f]
                                                           + c as i32)
                                                          as usize].b16.s3 > 0 {
                                    /* if (char_exists(orig_char_info(f)(c))) */
                                    if c >= 128 {
                                        dvi_out(SET1);
                                    }
                                    dvi_out(c as u8);
                                    cur_h +=
                                        *FONT_CHARACTER_WIDTH(f, c as usize);
                                }
                            }
                        }
                        prev_p = *LLIST_link(prev_p as usize);
                        p = *LLIST_link(p as usize);
                        if !is_char_node(p) { break ; }
                    }
                    synctex_current();
                    dvi_h = cur_h;
                    continue 's_726 ;
                } else {
                    /*644: "Output the non-char_node `p` and move to the next node" */
                    let n = text_NODE_type(p as usize).unwrap();
                    match n {
                        TextNode::HList | TextNode::VList => {
                            if BOX_list_ptr(p as usize).is_texnull() {
                                if n == TextNode::VList {
                                    synctex_void_vlist(p, this_box);
                                } else { synctex_void_hlist(p, this_box); }
                                cur_h += *BOX_width(p as usize);
                            } else {
                                save_h = dvi_h;
                                save_v = dvi_v;
                                cur_v =
                                    base_line +
                                        *BOX_shift_amount(p as usize);
                                temp_ptr = p;
                                edge =
                                    cur_h +
                                        *BOX_width(p as usize);
                                if cur_dir == LR::RightToLeft {
                                    cur_h = edge;
                                }
                                if n == TextNode::VList {
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
                        TextNode::Rule => {
                            rule_ht =
                                *BOX_height(p as usize);
                            rule_dp =
                                *BOX_depth(p as usize);
                            rule_wd =
                                *BOX_width(p as usize);
                            current_block = 18357984655869314713;
                            break ;
                        }
                        TextNode::WhatsIt => {
                            /*1407: "Output the whatsit node p in an hlist" */
                            match whatsit_NODE_subtype(p as usize) {
                                WhatsItNST::NativeWord | WhatsItNST::NativeWordAt | WhatsItNST::Glyph => {
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h,
                                                 RIGHT1); /* glyph count */
                                        dvi_h = cur_h
                                    } /* x offset, as fixed-point */
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v,
                                                 DOWN1); /* y offset, as fixed-point */
                                        dvi_v = cur_v
                                    } /* end of TextNode::WhatsIt case */
                                    f =
                                        *NATIVE_NODE_font(p as usize) as
                                            internal_font_number;
                                    if f != dvi_f {
                                        if !font_used[f] {
                                            dvi_font_def(f);
                                            font_used[f] =
                                                true
                                        }
                                        if f <= 64 {
                                            dvi_out((f + 170) as
                                                        u8);
                                        } else if f <= 256 {
                                            dvi_out(FNT1);
                                            dvi_out((f - 1) as u8);
                                        } else {
                                            dvi_out(FNT1 + 1);
                                            dvi_out(((f - 1) / 256) as
                                                        u8);
                                            dvi_out(((f - 1) % 256) as
                                                        u8);
                                        }
                                        dvi_f = f
                                    }
                                    if whatsit_NODE_subtype(p as usize) == WhatsItNST::Glyph {
                                        dvi_out(SET_GLYPHS);
                                        dvi_four(*BOX_width(p as usize));
                                        dvi_two(1);
                                        dvi_four(0);
                                        dvi_four(0);
                                        dvi_two(*NATIVE_NODE_glyph(p as usize));
                                        cur_h +=
                                            *BOX_width(p as usize);
                                    } else {
                                        if whatsit_NODE_subtype(p as usize) == WhatsItNST::NativeWordAt {
                                            if *NATIVE_NODE_length(p as usize) > 0 ||
                                                   !NATIVE_NODE_glyph_info_ptr(p as usize).is_null()
                                               {
                                                dvi_out(SET_TEXT_AND_GLYPHS);
                                                len =
                                                    *NATIVE_NODE_length(p as usize) as i32;
                                                dvi_two(len as UTF16_code);
                                                k = 0;
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
                                        } else if !NATIVE_NODE_glyph_info_ptr(p as usize).is_null()
                                         {
                                            dvi_out(SET_GLYPHS);
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
                                            *BOX_width(p as usize);
                                    }
                                    dvi_h = cur_h
                                }
                                WhatsItNST::Pic | WhatsItNST::Pdf => {
                                    save_h = dvi_h;
                                    save_v = dvi_v;
                                    cur_v = base_line;
                                    edge =
                                        cur_h +
                                            *BOX_width(p as usize);
                                    pic_out(p as usize);
                                    dvi_h = save_h;
                                    dvi_v = save_v;
                                    cur_h = edge;
                                    cur_v = base_line
                                }
                                WhatsItNST::PdfSavePos => {
                                    pdf_last_x_pos = cur_h + cur_h_offset;
                                    pdf_last_y_pos =
                                        cur_page_height - cur_v - cur_v_offset
                                }
                                _ => { out_what(p as usize); }
                            }
                            current_block = 13889995436552222973;
                            break ;
                        }
                        TextNode::Glue => {
                            /*647: "Move right or output leaders" */
                            g = *GLUE_NODE_glue_ptr(p as usize) as usize;
                            rule_wd =
                                *BOX_width(g) -
                                    cur_g;
                            if g_sign != GlueSign::Normal {
                                if g_sign == GlueSign::Stretching {
                                    if *GLUE_SPEC_stretch_order(g) ==
                                           g_order as u16 {
                                        cur_glue +=
                                            *GLUE_SPEC_stretch(g) as f64;
                                        glue_temp =
                                            *BOX_glue_set(this_box as usize) *
                                                cur_glue;
                                        if glue_temp > 1000000000. {
                                            glue_temp = 1000000000.
                                        } else if glue_temp < -1000000000.
                                         {
                                            glue_temp = -1000000000.
                                        }
                                        cur_g = tex_round(glue_temp)
                                    }
                                } else if *GLUE_SPEC_shrink_order(g) ==
                                              g_order as u16 {
                                    cur_glue -=
                                        *GLUE_SPEC_shrink(g) as
                                            f64;
                                    glue_temp =
                                        *BOX_glue_set(this_box as usize) *
                                            cur_glue;
                                    if glue_temp > 1000000000. {
                                        glue_temp = 1000000000.
                                    } else if glue_temp < -1000000000. {
                                        glue_temp = -1000000000.
                                    }
                                    cur_g = tex_round(glue_temp)
                                }
                            }
                            rule_wd += cur_g;
                            /*1486: "Handle a glue node for mixed direction typesetting". */
                            if g_sign == GlueSign::Stretching &&
                                   *GLUE_SPEC_stretch_order(g) == g_order as u16
                                   ||
                                   g_sign == GlueSign::Shrinking &&
                                       *GLUE_SPEC_shrink_order(g) ==
                                           g_order as u16 {
                                if GLUE_SPEC_ref_count(g).is_texnull() {
                                    free_node(g,
                                              GLUE_SPEC_SIZE);
                                } else {
                                    *GLUE_SPEC_ref_count(g) -= 1;
                                }
                                if MEM[p as usize].b16.s0 < A_LEADERS { // NODE_subtype(p)
                                    set_NODE_type(p as usize, TextNode::Kern);
                                    *BOX_width(p as usize)
                                        = rule_wd;
                                } else {
                                    g = get_node(GLUE_SPEC_SIZE);
                                    *GLUE_SPEC_stretch_order(g) =
                                        GlueOrder::Incorrect as u16; /* "will never match" */
                                    *GLUE_SPEC_shrink_order(g) =
                                        GlueOrder::Incorrect as u16;
                                    *BOX_width(g) = rule_wd;
                                    *GLUE_SPEC_stretch(g) = 0;
                                    *GLUE_SPEC_shrink(g) = 0;
                                    *GLUE_NODE_glue_ptr(p as usize) = g as i32;
                                }
                            }
                            if MEM[p as usize].b16.s0
                                   >= A_LEADERS { // NODE_subtype(p)
                                current_block = 14898553815918780345;
                                break ;
                            } else {
                                current_block = 7364881209357675324;
                                break ;
                            }
                        }
                        TextNode::MarginKern => {
                            cur_h +=
                                *BOX_width(p as usize);
                            current_block = 13889995436552222973;
                            break ;
                        }
                        TextNode::Kern => {
                            synctex_kern(p, this_box);
                            cur_h +=
                                *BOX_width(p as usize);
                            current_block = 13889995436552222973;
                            break ;
                        }
                        TextNode::Math => {
                            synctex_math(p, this_box);
                            /* 1504: "Adjust the LR stack...; if necessary reverse and
                 * hlist segment and goto reswitch." "Breaking a paragraph
                 * into lines while TeXXeT is disabled may result in lines
                 * with unpaired math nodes. Such hlists are silently accepted
                 * in the absence of text direction directives." */
                            if MEM[p as usize].b16.s0 as i32
                                   & 1i32 != 0 { // odd(NODE_subtype(p))
                                /* <= this is end_LR(p) */
                                if MEM[LR_ptr as usize].b32.s0 ==
                                       4i32 *
                                           (MEM[p as usize].b16.s0
                                                as i32 / 4i32) + 3i32
                                   {
                                    temp_ptr = LR_ptr;
                                    LR_ptr =
                                        *LLIST_link(temp_ptr as usize);
                                    *LLIST_link(temp_ptr as usize) =
                                        avail;
                                    avail = temp_ptr;
                                } else if MEM[p as usize].b16.s0 > L_CODE { // NODE_subtype(p)
                                    LR_problems += 1;
                                }
                                current_block = 330672039582001856;
                                break ;
                            } else {
                                temp_ptr = get_avail() as i32;
                                *LLIST_info(temp_ptr as usize) =
                                    4i32 *
                                        (MEM[p as usize].b16.s0 as
                                             i32 / 4i32) + 3i32;
                                *LLIST_link(temp_ptr as usize) =
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
                                temp_ptr = *LLIST_link(p as usize);
                                rule_wd =
                                    *BOX_width(p as usize);
                                free_node(p as usize, MEDIUM_NODE_SIZE);
                                cur_dir = !cur_dir;
                                p = new_edge(cur_dir, rule_wd) as i32;
                                *LLIST_link(prev_p as usize) = p;
                                cur_h = cur_h - left_edge + rule_wd;
                                *LLIST_link(p as usize) =
                                    reverse(this_box,
                                            new_edge(!cur_dir, 0) as i32,
                                            &mut cur_g, &mut cur_glue);
                                *EDGE_NODE_edge_dist(p as usize) =
                                    cur_h;
                                cur_dir = !cur_dir;
                                cur_h = save_h;
                            }
                        }
                        TextNode::Ligature => {
                            /* 675: "Make node p look like a char_node and goto reswitch" */
                            MEM[LIG_TRICK] =
                                MEM[(p + 1) as usize];
                            *LLIST_link(LIG_TRICK) = *LLIST_link(p as usize);
                            p = LIG_TRICK as i32;
                            xtx_ligature_present = true;
                        }
                        EDGE_NODE => {
                            /*1507: "Cases of hlist_out that arise in mixed direction text only" */
                            cur_h +=
                                *BOX_width(p as usize);
                            left_edge =
                                cur_h +
                                    *EDGE_NODE_edge_dist(p as usize);
                            cur_dir = LR::n(MEM[p as usize].b16.s0).unwrap();
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
                leader_box = *GLUE_NODE_leader_ptr(p as usize); /* "compensate for floating-point rounding" ?? */
                if NODE_type(leader_box as usize) == TextNode::Rule.into() {
                    rule_ht = *BOX_height(leader_box as usize);
                    rule_dp = *BOX_depth(leader_box as usize);
                    current_block = 18357984655869314713;
                } else {
                    leader_wd = *BOX_width(leader_box as usize);
                    if leader_wd > 0 && rule_wd > 0 {
                        rule_wd += 10;
                        if cur_dir == LR::RightToLeft {
                            cur_h -= 10;
                        }
                        edge = cur_h + rule_wd;
                        lx = 0;
                        /*649: "Let cur_h be the position of the first pox,
                         * and set leader_wd + lx to the spacing between
                         * corresponding parts of boxes". Additional
                         * explanator comments in XTTP. */
                        if MEM[p as usize].b16.s0 == A_LEADERS {
                            // NODE_subtype(p)
                            save_h = cur_h;
                            cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);
                            if cur_h < save_h {
                                cur_h = cur_h + leader_wd
                            }
                        } else {
                            lq = rule_wd / leader_wd;
                            lr = rule_wd % leader_wd;
                            if MEM[p as usize].b16.s0 == C_LEADERS {
                                // NODE_subtype(p)
                                cur_h = cur_h + lr / 2;
                            } else {
                                lx = lr / (lq + 1);
                                cur_h = cur_h + (lr - (lq - 1) * lx) / 2;
                            }
                        }
                        while cur_h + leader_wd <= edge {
                            /*650: "Output a leader box at cur_h, then advance cur_h by leader_wd + lx" */
                            cur_v = base_line + *BOX_shift_amount(leader_box as usize);
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, DOWN1);
                                dvi_v = cur_v
                            }
                            save_v = dvi_v;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, RIGHT1);
                                dvi_h = cur_h
                            }
                            save_h = dvi_h;
                            temp_ptr = leader_box;
                            if cur_dir == LR::RightToLeft {
                                cur_h += leader_wd;
                            }
                            outer_doing_leaders = doing_leaders;
                            doing_leaders = true;
                            if NODE_type(leader_box as usize) == TextNode::VList.into() {
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
                        if cur_dir == LR::RightToLeft {
                            cur_h = edge;
                        } else {
                            cur_h = edge - 10;
                        }
                        current_block = 13889995436552222973;
                    } else {
                        current_block = 7364881209357675324;
                    }
                }
            }
            330672039582001856 => {
                set_NODE_type(p as usize, TextNode::Kern);
                cur_h += *BOX_width(p as usize);
                current_block = 13889995436552222973;
            }
            _ => {}
        }
        match current_block {
            18357984655869314713 => {
                /*646: "Output a rule in an hlist" */
                if rule_ht == NULL_FLAG {
                    rule_ht = *BOX_height(this_box as usize);
                }
                if rule_dp == NULL_FLAG {
                    rule_dp = *BOX_depth(this_box as usize);
                }
                rule_ht += rule_dp;
                if rule_ht > 0 && rule_wd > 0 {
                    if cur_h != dvi_h {
                        movement(cur_h - dvi_h, RIGHT1);
                        dvi_h = cur_h
                    }
                    cur_v = base_line + rule_dp;
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, DOWN1);
                        dvi_v = cur_v
                    }
                    dvi_out(SET_RULE);
                    dvi_four(rule_ht);
                    dvi_four(rule_wd);
                    cur_v = base_line;
                    dvi_h += rule_wd;
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
    while *LLIST_info(LR_ptr as usize) != BEFORE as i32 {
        if MEM[LR_ptr as usize].b32.s0 > L_CODE as i32 {
            // LLIST_info(LR_ptr)
            LR_problems += 10000;
        }
        temp_ptr = LR_ptr;
        LR_ptr = *LLIST_link(temp_ptr as usize);
        *LLIST_link(temp_ptr as usize) = avail;
        avail = temp_ptr
    }

    temp_ptr = LR_ptr;
    LR_ptr = *LLIST_link(temp_ptr as usize);
    *LLIST_link(temp_ptr as usize) = avail;
    avail = temp_ptr;

    if BOX_lr_mode(this_box as usize) == LRMode::DList {
        cur_dir = LR::RightToLeft;
    }

    /* ... finishing 639 */

    prune_movements(save_loc);
    if cur_s > 0 {
        dvi_pop(save_loc);
    }
    cur_s -= 1;
}

/*651: "When vlist_out is called, its duty is to output the box represented by
 * the vlist_node pointed to by temp_ptr. The reference point of that box has
 * coordinates (cur_h, cur_v)." */
unsafe fn vlist_out() {
    let mut current_block: u64;
    let mut left_edge: scaled_t = 0;
    let mut top_edge: scaled_t = 0;
    let mut save_h: scaled_t = 0;
    let mut save_v: scaled_t = 0;
    let mut save_loc: usize = 0;
    let mut leader_box: i32 = 0;
    let mut leader_ht: scaled_t = 0;
    let mut lx: scaled_t = 0;
    let mut outer_doing_leaders: bool = false;
    let mut edge: scaled_t = 0;
    let mut glue_temp: f64 = 0.;
    let mut upwards: bool = false;
    let mut f: internal_font_number = 0;

    let mut cur_g = 0i32;
    let mut cur_glue = 0.0f64;
    let mut this_box = temp_ptr;
    let mut g_order = MEM[(this_box + 5) as usize].b16.s0 as glue_ord;
    let mut g_sign = GlueSign::from(*BOX_glue_sign(this_box as usize));
    let mut popt = BOX_list_ptr(this_box as usize).opt();
    upwards = MEM[this_box as usize].b16.s0 as i32 == 1; // NODE_subtype(this_box)

    cur_s += 1;
    if cur_s > 0 {
        dvi_out(PUSH);
    }

    if cur_s > max_push {
        max_push = cur_s
    }

    save_loc = dvi_offset + dvi_ptr;
    left_edge = cur_h;
    synctex_vlist(this_box);

    if upwards {
        cur_v += *BOX_depth(this_box as usize);
    } else {
        cur_v -= *BOX_height(this_box as usize);
    }

    top_edge = cur_v;

    while let Some(p) = popt {
        /*652: "Output node p and move to the next node, maintaining the
         * condition cur_h = left_edge" */
        if is_char_node(p as i32) {
            confusion(b"vlistout");
        } else {
            /*653: "Output the non-char_node p" */
            let n = text_NODE_type(p).unwrap();
            match n {
                TextNode::HList | TextNode::VList => {
                    /*654: "Output a box in a vlist" */
                    if BOX_list_ptr(p).is_texnull() {
                        if upwards {
                            cur_v -= *BOX_depth(p);
                        } else {
                            cur_v += *BOX_height(p);
                        }
                        if n == TextNode::VList {
                            synctex_void_vlist(p as i32, this_box);
                        } else {
                            synctex_void_hlist(p as i32, this_box);
                        }
                        if upwards {
                            cur_v -= *BOX_height(p);
                        } else {
                            cur_v += *BOX_depth(p);
                        }
                    } else {
                        if upwards {
                            cur_v -= *BOX_depth(p);
                        } else {
                            cur_v += *BOX_height(p);
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, DOWN1);
                            dvi_v = cur_v
                        }
                        save_h = dvi_h;
                        save_v = dvi_v;
                        if cur_dir == LR::RightToLeft {
                            cur_h = left_edge - *BOX_shift_amount(p);
                        } else {
                            cur_h = left_edge + *BOX_shift_amount(p);
                        }
                        temp_ptr = p as i32;
                        if n == TextNode::VList {
                            vlist_out();
                        } else {
                            hlist_out();
                        }
                        dvi_h = save_h;
                        dvi_v = save_v;
                        if upwards {
                            cur_v = save_v - *BOX_height(p);
                        } else {
                            cur_v = save_v + *BOX_depth(p);
                        }
                        cur_h = left_edge
                    }
                    current_block = 5241535548500397784;
                }
                TextNode::Rule => {
                    rule_ht = *BOX_height(p);
                    rule_dp = *BOX_depth(p);
                    rule_wd = *BOX_width(p);
                    current_block = 9653381107620864133;
                }
                TextNode::WhatsIt => {
                    /*1403: "Output the whatsit node p in a vlist" */
                    match whatsit_NODE_subtype(p) {
                        WhatsItNST::Glyph => {
                            cur_v = cur_v + *BOX_height(p);
                            cur_h = left_edge;
                            if cur_h != dvi_h {
                                movement(cur_h - dvi_h, RIGHT1);
                                dvi_h = cur_h
                            }
                            if cur_v != dvi_v {
                                movement(cur_v - dvi_v, DOWN1);
                                dvi_v = cur_v
                            }
                            f = *NATIVE_NODE_font(p) as usize;
                            if f != dvi_f {
                                /*643:*/
                                if !font_used[f] {
                                    dvi_font_def(f); /* width */
                                    font_used[f] = true
                                } /* glyph count */
                                if f <= 64 {
                                    dvi_out((f + 170) as u8); /* x offset as fixed-point */
                                } else if f <= 256 {
                                    dvi_out(FNT1); /* y offset as fixed-point */
                                    dvi_out((f - 1) as u8);
                                } else {
                                    dvi_out(FNT1 + 1);
                                    dvi_out(((f - 1) / 256) as u8);
                                    dvi_out(((f - 1) % 256) as u8);
                                }
                                dvi_f = f
                            }
                            dvi_out(SET_GLYPHS);
                            dvi_four(0); /* width */
                            dvi_two(1 as UTF16_code); /* glyph count */
                            dvi_four(0); /* x offset as fixed-point */
                            dvi_four(0); /* y offset as fixed-point */
                            dvi_two(*NATIVE_NODE_glyph(p));

                            cur_v += *BOX_depth(p);
                            cur_h = left_edge;
                        }
                        WhatsItNST::Pic | WhatsItNST::Pdf => {
                            save_h = dvi_h;
                            save_v = dvi_v;
                            cur_v = cur_v + *BOX_height(p);
                            pic_out(p);
                            dvi_h = save_h;
                            dvi_v = save_v;
                            cur_v = save_v + *BOX_depth(p);
                            cur_h = left_edge;
                        }
                        WhatsItNST::PdfSavePos => {
                            pdf_last_x_pos = cur_h + cur_h_offset;
                            pdf_last_y_pos = cur_page_height - cur_v - cur_v_offset
                        }
                        _ => out_what(p),
                    }
                    current_block = 5241535548500397784;
                }
                TextNode::Glue => {
                    /*656: "Move down or output leaders" */
                    g = *GLUE_NODE_glue_ptr(p) as usize;
                    rule_ht = *BOX_width(g) - cur_g;

                    if g_sign != GlueSign::Normal {
                        if g_sign == GlueSign::Stretching {
                            if *GLUE_SPEC_stretch_order(g) == g_order as u16 {
                                cur_glue += *GLUE_SPEC_stretch(g) as f64;
                                glue_temp = *BOX_glue_set(this_box as usize) * cur_glue;
                                if glue_temp > 1000000000. {
                                    glue_temp = 1000000000.
                                } else if glue_temp < -1000000000. {
                                    glue_temp = -1000000000.
                                }
                                cur_g = tex_round(glue_temp)
                            }
                        } else if *GLUE_SPEC_shrink_order(g) == g_order as u16 {
                            cur_glue -= *GLUE_SPEC_shrink(g) as f64;
                            glue_temp = *BOX_glue_set(this_box as usize) * cur_glue;
                            if glue_temp > 1000000000. {
                                glue_temp = 1000000000.
                            } else if glue_temp < -1000000000. {
                                glue_temp = -1000000000.
                            }
                            cur_g = tex_round(glue_temp)
                        }
                    }

                    rule_ht += cur_g;

                    if MEM[p].b16.s0 >= A_LEADERS {
                        // NODE_subtype(p)
                        /*657: "Output leaders in a vlist, goto fin_rule if a rule
                         * or next_p if done" */
                        leader_box = *GLUE_NODE_leader_ptr(p); /* "compensate for floating-point rounding" */

                        if NODE_type(leader_box as usize) == TextNode::Rule.into() {
                            rule_wd = *BOX_width(leader_box as usize);
                            rule_dp = 0;
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
                                if MEM[p].b16.s0 == A_LEADERS {
                                    // NODE_subtype(p)
                                    save_v = cur_v;
                                    cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);
                                    if cur_v < save_v {
                                        cur_v = cur_v + leader_ht
                                    }
                                } else {
                                    lq = rule_ht / leader_ht;
                                    lr = rule_ht % leader_ht;
                                    if MEM[p].b16.s0 == C_LEADERS {
                                        cur_v = cur_v + lr / 2;
                                    } else {
                                        lx = lr / (lq + 1);
                                        cur_v = cur_v + (lr - (lq - 1) * lx) / 2;
                                    }
                                }

                                while cur_v + leader_ht <= edge {
                                    /*659: "Output a leader box at cur_v, then advance
                                     * cur_v by leader_ht + lx". "When we reach this
                                     * part of the program, cur_v indicates the top of
                                     * a leader box, not its baseline." */
                                    if cur_dir == LR::RightToLeft {
                                        cur_h = left_edge - *BOX_shift_amount(leader_box as usize);
                                    } else {
                                        cur_h = left_edge + *BOX_shift_amount(leader_box as usize);
                                    }
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h, RIGHT1);
                                        dvi_h = cur_h
                                    }

                                    save_h = dvi_h;
                                    cur_v += *BOX_height(leader_box as usize);

                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v, DOWN1);
                                        dvi_v = cur_v
                                    }

                                    save_v = dvi_v;
                                    temp_ptr = leader_box;
                                    outer_doing_leaders = doing_leaders;
                                    doing_leaders = true;

                                    if NODE_type(leader_box as usize) == TextNode::VList.into() {
                                        vlist_out();
                                    } else {
                                        hlist_out();
                                    }

                                    doing_leaders = outer_doing_leaders;
                                    dvi_v = save_v;
                                    dvi_h = save_h;
                                    cur_h = left_edge;
                                    cur_v =
                                        save_v - *BOX_height(leader_box as usize) + leader_ht + lx
                                }
                                cur_v = edge - 10;
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
                TextNode::Kern => {
                    if upwards {
                        cur_v -= *BOX_width(p);
                    } else {
                        cur_v += *BOX_width(p);
                    }
                    current_block = 5241535548500397784;
                }
                _ => current_block = 5241535548500397784,
            }
            match current_block {
                9653381107620864133 => {
                    // 655: "Output a rule in a vlist, goto next_p

                    if rule_wd == NULL_FLAG {
                        rule_wd = *BOX_width(this_box as usize);
                    }

                    rule_ht += rule_dp;

                    if upwards {
                        cur_v -= rule_ht
                    } else {
                        cur_v += rule_ht
                    }

                    if rule_ht > 0 && rule_wd > 0 {
                        if cur_dir == LR::RightToLeft {
                            cur_h -= rule_wd
                        }
                        if cur_h != dvi_h {
                            movement(cur_h - dvi_h, RIGHT1);
                            dvi_h = cur_h
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, DOWN1);
                            dvi_v = cur_v
                        }
                        dvi_out(PUT_RULE);
                        dvi_four(rule_ht);
                        dvi_four(rule_wd);
                        cur_h = left_edge
                    }
                }
                _ => {}
            }
            popt = LLIST_link(p).opt();
        }
    }
    synctex_tsilv(this_box);
    prune_movements(save_loc);
    if cur_s > 0 {
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
unsafe fn reverse(
    mut this_box: i32,
    mut t: i32,
    mut cur_g: *mut scaled_t,
    mut cur_glue: *mut f64,
) -> i32 {
    let mut current_block: u64;
    let mut l: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut glue_temp: f64 = 0.;
    let mut m: i32 = 0;
    let mut n: i32 = 0;
    let mut c: u16 = 0;
    let mut f: internal_font_number = 0;
    let mut g_order = *BOX_glue_order(this_box as usize);
    let mut g_sign = GlueSign::from(*BOX_glue_sign(this_box as usize));
    l = t;
    p = temp_ptr;
    m = MIN_HALFWORD;
    n = MIN_HALFWORD;
    's_58: loop {
        if !p.is_texnull() {
            loop
            /*1511: "Move node p to the new list and go to the next node; or
             * goto done if the end of the reflected segment has been
             * reached." */
            {
                if is_char_node(p) {
                    loop {
                        f = *CHAR_NODE_font(p as usize) as usize;
                        c = *CHAR_NODE_character(p as usize);
                        cur_h += FONT_INFO[(WIDTH_BASE[f]
                            + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize]
                                .b16
                                .s3 as i32) as usize]
                            .b32
                            .s1;
                        q = *LLIST_link(p as usize);
                        *LLIST_link(p as usize) = l;
                        l = p;
                        p = q;
                        if !is_char_node(p) {
                            break;
                        }
                    }
                    continue 's_58;
                } else {
                    q = *LLIST_link(p as usize);
                    match text_NODE_type(p as usize).unwrap() {
                        TextNode::HList | TextNode::VList | TextNode::Rule | TextNode::Kern => {
                            rule_wd = *BOX_width(p as usize);
                            current_block = 3812947724376655173;
                            break;
                        }
                        TextNode::WhatsIt => {
                            if whatsit_NODE_subtype(p as usize) == WhatsItNST::NativeWord
                                || whatsit_NODE_subtype(p as usize) == WhatsItNST::NativeWordAt
                                || whatsit_NODE_subtype(p as usize) == WhatsItNST::Glyph
                                || whatsit_NODE_subtype(p as usize) == WhatsItNST::Pic
                                || whatsit_NODE_subtype(p as usize) == WhatsItNST::Pdf
                            {
                                current_block = 7056779235015430508;
                                break;
                            } else {
                                current_block = 10883403804712335414;
                                break;
                            }
                        }
                        TextNode::Glue => {
                            /*1486: "Handle a glue node for mixed direction typesetting" */
                            g = *GLUE_NODE_glue_ptr(p as usize) as usize; /* "will never match" */
                            rule_wd = *BOX_width(g) - *cur_g; /* = mem[lig_char(temp_ptr)] */

                            if g_sign != GlueSign::Normal {
                                if g_sign == GlueSign::Stretching {
                                    if *GLUE_SPEC_stretch_order(g) == g_order as u16 {
                                        *cur_glue = *cur_glue + *GLUE_SPEC_stretch(g) as f64;
                                        glue_temp = *BOX_glue_set(this_box as usize) * *cur_glue;
                                        if glue_temp > 1000000000. {
                                            glue_temp = 1000000000.
                                        } else if glue_temp < -1000000000. {
                                            glue_temp = -1000000000.
                                        }
                                        *cur_g = tex_round(glue_temp)
                                    }
                                } else if *GLUE_SPEC_shrink_order(g) == g_order as u16 {
                                    *cur_glue = *cur_glue - *GLUE_SPEC_shrink(g) as f64;
                                    glue_temp = *BOX_glue_set(this_box as usize) * *cur_glue;
                                    if glue_temp > 1000000000. {
                                        glue_temp = 1000000000.
                                    } else if glue_temp < -1000000000. {
                                        glue_temp = -1000000000.
                                    }
                                    *cur_g = tex_round(glue_temp)
                                }
                            }
                            rule_wd += *cur_g;
                            if g_sign == GlueSign::Stretching && MEM[g].b16.s1 == g_order as u16
                                || g_sign == GlueSign::Shrinking && MEM[g].b16.s0 == g_order as u16
                            {
                                if GLUE_SPEC_ref_count(g).is_texnull() {
                                    free_node(g, GLUE_SPEC_SIZE);
                                } else {
                                    *GLUE_SPEC_ref_count(g) -= 1;
                                }
                                if MEM[p as usize].b16.s0 < A_LEADERS {
                                    // NODE_subtype(p)
                                    set_NODE_type(p as usize, TextNode::Kern);
                                    *BOX_width(p as usize) = rule_wd;
                                } else {
                                    g = get_node(GLUE_SPEC_SIZE);
                                    *GLUE_SPEC_stretch_order(g) = GlueOrder::Incorrect as u16;
                                    *GLUE_SPEC_shrink_order(g) = GlueOrder::Incorrect as u16;
                                    *BOX_width(g) = rule_wd;
                                    *GLUE_SPEC_stretch(g) = 0;
                                    *GLUE_SPEC_shrink(g) = 0;
                                    *GLUE_NODE_glue_ptr(p as usize) = g as i32;
                                }
                            }
                            current_block = 3812947724376655173;
                            break;
                        }
                        TextNode::Ligature => {
                            flush_node_list(LIGATURE_NODE_lig_ptr(p as usize).opt());
                            temp_ptr = p;
                            p = get_avail() as i32;
                            MEM[p as usize] = MEM[(temp_ptr + 1) as usize];
                            *LLIST_link(p as usize) = q;
                            free_node(temp_ptr as usize, SMALL_NODE_SIZE);
                        }
                        TextNode::Math => {
                            /*1516: "Math nodes in an inner reflected segment are
                             * modified, those at the outer level are changed into
                             * kern nodes." */
                            rule_wd = *BOX_width(p as usize);

                            if (BOX_lr_mode(p as usize) as u16) & 1 != 0 {
                                current_block = 5873035170358615968;
                                break;
                            } else {
                                current_block = 17239133558811367971;
                                break;
                            }
                        }
                        EDGE_NODE => confusion(b"LR2"),
                        _ => {
                            current_block = 10883403804712335414;
                            break;
                        }
                    }
                }
            }
            match current_block {
                5873035170358615968 => {
                    if *LLIST_info(LR_ptr as usize) != 4 * (MEM[p as usize].b16.s0 as i32 / 4) + 3 {
                        set_NODE_type(p as usize, TextNode::Kern);
                        LR_problems += 1;
                    } else {
                        temp_ptr = LR_ptr;
                        LR_ptr = *LLIST_link(temp_ptr as usize);
                        *LLIST_link(temp_ptr as usize) = avail;
                        avail = temp_ptr;

                        if n > MIN_HALFWORD {
                            n -= 1;
                            MEM[p as usize].b16.s0 -= 1; // NODE_subtype(p)
                        } else {
                            set_NODE_type(p as usize, TextNode::Kern);
                            if m > MIN_HALFWORD {
                                m -= 1
                            } else {
                                /*1517: "Finish the reverse hlist segment and goto done" */
                                free_node(p as usize, MEDIUM_NODE_SIZE); /* end GLUE_NODE case */
                                *LLIST_link(t as usize) = q;
                                *BOX_width(t as usize) = rule_wd;
                                *EDGE_NODE_edge_dist(t as usize) = -cur_h - rule_wd;
                                break;
                            }
                        }
                    }
                    current_block = 3812947724376655173;
                }
                17239133558811367971 => {
                    temp_ptr = get_avail() as i32;
                    *LLIST_info(temp_ptr as usize) = 4 * (MEM[p as usize].b16.s0 as i32 / 4) + 3;
                    *LLIST_link(temp_ptr as usize) = LR_ptr;
                    LR_ptr = temp_ptr;
                    if n > MIN_HALFWORD || MEM[p as usize].b16.s0 / 8 != cur_dir as u16 {
                        n += 1;
                        MEM[p as usize].b16.s0 += 1; // NODE_subtype(p)
                    } else {
                        set_NODE_type(p as usize, TextNode::Kern);
                        m += 1;
                    }
                    current_block = 3812947724376655173;
                }
                7056779235015430508 => {
                    rule_wd = *BOX_width(p as usize);
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
                if rule_wd == 0 || l.is_texnull() {
                    free_node(p as usize, 3i32);
                    p = l
                }
            }
            l = p;
            p = q
        } else {
            /* ... resuming 1510 ... */
            if t.is_texnull() && m.is_texnull() && n.is_texnull() {
                break; /* "Manufacture a missing math node" */
            }
            p = new_math(0, *LLIST_info(LR_ptr as usize) as i16) as i32;
            LR_problems += 10000i32
        }
    }
    l
}

/*1506: Create a new edge node of subtype `s` and width `w` */
pub(crate) unsafe fn new_edge(s: LR, w: scaled_t) -> usize {
    let p = get_node(EDGE_NODE_SIZE) as usize;
    set_NODE_type(p, EDGE_NODE);
    MEM[p].b16.s0 = s as u16; // set_NODE_subtype
    *BOX_width(p) = w;
    *EDGE_NODE_edge_dist(p) = 0;
    p
}

pub(crate) unsafe fn out_what(p: usize) {
    let mut j: i16;
    match whatsit_NODE_subtype(p) {
        WhatsItNST::Open | WhatsItNST::Write | WhatsItNST::Close => {
            if doing_leaders {
                return;
            }

            j = MEM[p + 1].b32.s0 as i16;
            if whatsit_NODE_subtype(p) == WhatsItNST::Write {
                write_out(p);
                return;
            }

            if write_open[j as usize] {
                ttstub_output_close(write_file[j as usize].take().unwrap());
            }

            if whatsit_NODE_subtype(p) == WhatsItNST::Close {
                write_open[j as usize] = false;
                return;
            }

            /* By this point must be WhatsItNST::Open */
            if j >= 16 {
                return;
            }

            cur_name = MEM[p + 1].b32.s1;
            cur_area = MEM[p + 2].b32.s0;
            cur_ext = MEM[p + 2].b32.s1;
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
                if *INTPAR(IntPar::tracing_online) <= 0 {
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
        WhatsItNST::Special => special_out(p),
        WhatsItNST::Language => {}
        _ => confusion(b"ext4"),
    };
}

unsafe fn dvi_native_font_def(f: internal_font_number) {
    dvi_out(DEFINE_NATIVE_FONT as _);
    dvi_four(f as i32 - 1);
    let font_def_length = make_font_def(f);

    for i in 0..font_def_length {
        dvi_out(*xdv_buffer.offset(i as isize) as u8);
    }
}

unsafe fn dvi_font_def(f: internal_font_number) {
    let mut k: pool_pointer = 0;
    let mut l: i32 = 0;
    if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
        dvi_native_font_def(f);
    } else {
        if f <= 256 {
            dvi_out(FNT_DEF1);
            dvi_out((f - 1) as u8);
        } else {
            dvi_out(FNT_DEF1 + 1);
            dvi_out(((f - 1) / 256) as u8);
            dvi_out(((f - 1) % 256) as u8);
        }
        dvi_out(FONT_CHECK[f].s3 as u8);
        dvi_out(FONT_CHECK[f].s2 as u8);
        dvi_out(FONT_CHECK[f].s1 as u8);
        dvi_out(FONT_CHECK[f].s0 as u8);
        dvi_four(FONT_SIZE[f]);
        dvi_four(FONT_DSIZE[f]);
        dvi_out(length(FONT_AREA[f]) as u8);
        l = 0;
        k = str_start[(FONT_NAME[f] as i64 - 65536) as usize];

        while l == 0i32 && k < str_start[((FONT_NAME[f] + 1) as i64 - 65536) as usize] {
            if str_pool[k as usize] as i32 == ':' as i32 {
                l = k - str_start[(FONT_NAME[f] as i64 - 65536) as usize]
            }
            k += 1;
        }
        if l == 0i32 {
            l = length(FONT_NAME[f])
        }
        dvi_out(l as u8);
        k = str_start[(FONT_AREA[f] as i64 - 65536) as usize];
        let mut for_end = str_start[((FONT_AREA[f] + 1) as i64 - 65536) as usize] - 1i32;
        if k <= for_end {
            loop {
                dvi_out(str_pool[k as usize] as u8);
                let fresh6 = k;
                k = k + 1;
                if !(fresh6 < for_end) {
                    break;
                }
            }
        }
        k = str_start[(FONT_NAME[f] as i64 - 65536) as usize];
        let mut for_end_0 = str_start[(FONT_NAME[f] as i64 - 65536) as usize] + l - 1i32;
        if k <= for_end_0 {
            loop {
                dvi_out(str_pool[k as usize] as u8);
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
    let mut p: i32 = 0;
    let mut k: i32 = 0;
    let mut q = get_node(MOVEMENT_NODE_SIZE);
    MEM[q + 1].b32.s1 = w;
    MEM[q + 2].b32.s1 = (dvi_offset + dvi_ptr) as i32;
    if o == DOWN1 {
        MEM[q].b32.s1 = down_ptr.tex_int();
        down_ptr = Some(q);
    } else {
        MEM[q].b32.s1 = right_ptr;
        right_ptr = q as i32;
    }

    p = MEM[q].b32.s1;
    let mut mstate = MoveSeen::None;

    loop {
        if p.is_texnull() {
            return not_found(q, o, w);
        }
        if MEM[(p + 1) as usize].b32.s1 == w {
            /* By this point must be WhatsItNST::Open */
            /*632:*/
            match (mstate, MoveDir::from(MEM[p as usize].b32.s0)) {
                (MoveSeen::None, MoveDir::YZOk)
                | (MoveSeen::None, MoveDir::YOk)
                | (MoveSeen::Z, MoveDir::YZOk)
                | (MoveSeen::Z, MoveDir::YOk) => {
                    if MEM[(p + 2) as usize].b32.s1 < dvi_gone {
                        return not_found(q, o, w);
                    } else {
                        /*633:*/
                        k = MEM[(p + 2) as usize].b32.s1 - dvi_offset as i32;
                        if k < 0 {
                            k = k + DVI_BUF_SIZE as i32;
                        }
                        dvi_buf[k as usize] += 5;
                        MEM[p as usize].b32.s0 = MoveDir::YHere as i32;
                        return found(q, o, p as usize);
                    }
                }
                (MoveSeen::None, MoveDir::ZOk)
                | (MoveSeen::Y, MoveDir::YZOk)
                | (MoveSeen::Y, MoveDir::ZOk) => {
                    if MEM[(p + 2) as usize].b32.s1 < dvi_gone {
                        return not_found(q, o, w);
                    }
                    k = MEM[(p + 2) as usize].b32.s1 - dvi_offset as i32;
                    if k < 0 {
                        k = k + DVI_BUF_SIZE as i32;
                    }
                    dvi_buf[k as usize] += 10;
                    MEM[p as usize].b32.s0 = MoveDir::ZHere as i32;
                    return found(q, o, p as usize);
                }
                (MoveSeen::None, MoveDir::YHere)
                | (MoveSeen::None, MoveDir::ZHere)
                | (MoveSeen::Y, MoveDir::ZHere)
                | (MoveSeen::Z, MoveDir::YHere) => {
                    return found(q, o, p as usize);
                }
                _ => {}
            }
        } else {
            match (mstate, MoveDir::from(MEM[p as usize].b32.s0)) {
                (MoveSeen::None, MoveDir::YHere) => {
                    mstate = MoveSeen::Y;
                }
                (MoveSeen::None, MoveDir::ZHere) => {
                    mstate = MoveSeen::Z;
                }
                (MoveSeen::Y, MoveDir::ZHere) | (MoveSeen::Z, MoveDir::YHere) => {
                    return not_found(q, o, w);
                }
                _ => {}
            }
        }
        p = *LLIST_link(p as usize);
    }

    /*629: found:*/
    unsafe fn found(mut q: usize, o: u8, p: usize) {
        MEM[q].b32.s0 = MEM[p as usize].b32.s0; /*634:*/
        if MEM[q].b32.s0 == MoveDir::YHere as i32 {
            dvi_out(o + 4); /* max_selector enum */
            while MEM[q].b32.s1 != p as i32 {
                q = *LLIST_link(q) as usize;

                match MoveDir::from(MEM[q].b32.s0) {
                    MoveDir::YZOk => MEM[q].b32.s0 = MoveDir::ZOk as i32,
                    MoveDir::YOk => MEM[q].b32.s0 = MoveDir::DFixed as i32,
                    _ => {}
                }
            }
        } else {
            dvi_out(o + 9);
            while MEM[q].b32.s1 != p as i32 {
                q = *LLIST_link(q) as usize;

                match MoveDir::from(MEM[q].b32.s0) {
                    MoveDir::YZOk => MEM[q].b32.s0 = MoveDir::YOk as i32,
                    MoveDir::ZOk => MEM[q].b32.s0 = MoveDir::DFixed as i32,
                    _ => {}
                }
            }
        }
    }
    unsafe fn not_found(q: usize, o: u8, mut w: scaled_t) {
        MEM[q].b32.s0 = MoveDir::YZOk as i32;

        if w.abs() >= 0x800000 {
            dvi_out(o + 3);
            dvi_four(w);
            return;
        }
        if w.abs() >= 0x8000 {
            dvi_out(o + 2);
            if w < 0 {
                w = w + 0x1000000;
            }
            dvi_out((w / 0x10000) as u8);
            w = w % 0x10000;
            // lab2:
            dvi_out((w / 256) as u8);
        } else if w.abs() >= 128 {
            dvi_out(o + 1);
            if w < 0 {
                w = w + 0x10000;
            }
            // lab2:
            dvi_out((w / 256) as u8);
        } else {
            dvi_out(o);
            if w < 0 {
                w = w + 256;
            }
        }
        // lab1:
        dvi_out((w % 256) as u8);
    }
}

unsafe fn prune_movements(l: usize) {
    while let Some(p) = down_ptr {
        if MEM[p + 2].b32.s1 < l as i32 {
            break;
        }

        down_ptr = LLIST_link(p).opt();
        free_node(p, MOVEMENT_NODE_SIZE);
    }
    while !right_ptr.is_texnull() {
        if MEM[(right_ptr + 2) as usize].b32.s1 < l as i32 {
            return;
        }
        let p = right_ptr;
        right_ptr = MEM[p as usize].b32.s1;
        free_node(p as usize, MOVEMENT_NODE_SIZE);
    }
}

unsafe fn special_out(p: usize) {
    if cur_h != dvi_h {
        movement(cur_h - dvi_h, RIGHT1);
        dvi_h = cur_h
    }
    if cur_v != dvi_v {
        movement(cur_v - dvi_v, DOWN1);
        dvi_v = cur_v
    }
    doing_special = true;
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    show_token_list(
        MEM[MEM[p + 1].b32.s1 as usize].b32.s1.opt(),
        None,
        pool_size - pool_ptr,
    );
    selector = old_setting;

    if pool_ptr + 1 > pool_size {
        overflow(b"pool size", (pool_size - init_pool_ptr) as usize);
    }

    if cur_length() < 256 {
        dvi_out(XXX1);
        dvi_out(cur_length() as u8);
    } else {
        dvi_out(XXX4);
        dvi_four(cur_length());
    }

    {
        let mut k = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        let mut for_end = pool_ptr - 1;
        if k <= for_end {
            loop {
                dvi_out(str_pool[k as usize] as u8);
                let fresh8 = k;
                k = k + 1;
                if !(fresh8 < for_end) {
                    break;
                }
            }
        }
    }
    pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
    doing_special = false;
}

unsafe fn write_out(p: usize) {
    let q = get_avail();
    MEM[q].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
    let mut r = get_avail();
    MEM[q].b32.s1 = r as i32;
    MEM[r].b32.s0 = CS_TOKEN_FLAG + END_WRITE as i32;
    begin_token_list(q, Btl::Inserted);
    begin_token_list(MEM[p + 1].b32.s1 as usize, Btl::WriteText);
    let q = get_avail();
    MEM[q].b32.s0 = LEFT_BRACE_TOKEN + '{' as i32;
    begin_token_list(q, Btl::Inserted);

    let old_mode = cur_list.mode;
    cur_list.mode = (false, ListMode::NoMode);
    cur_cs = write_loc;
    let _q = scan_toks(false, true);
    get_token();

    if cur_tok != CS_TOKEN_FLAG + END_WRITE as i32 {
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
            if !(cur_tok != CS_TOKEN_FLAG + END_WRITE as i32) {
                break;
            }
        }
    }

    cur_list.mode = old_mode;
    end_token_list();
    let old_setting = selector;
    let j = MEM[p + 1].b32.s0 as i16;

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

    token_show(Some(def_ref));
    print_ln();
    flush_list(Some(def_ref));

    if j == 18 {
        if *INTPAR(IntPar::tracing_online) <= 0 {
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
            print(str_pool[(str_start[(str_ptr - TOO_BIG_CHAR) as usize] + d) as usize] as i32);
            d += 1
        }
        print_cstr(b")...");
        print_cstr(b"disabled");
        print_char('.' as i32);
        print_nl_cstr(b"");
        print_ln();
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
    }
    selector = old_setting;
}

unsafe fn pic_out(p: usize) {
    if cur_h != dvi_h {
        movement(cur_h - dvi_h, RIGHT1);
        dvi_h = cur_h
    }

    if cur_v != dvi_v {
        movement(cur_v - dvi_v, DOWN1);
        dvi_v = cur_v
    }

    let old_setting = selector;
    selector = Selector::NEW_STRING;
    print_cstr(b"pdf:image ");
    print_cstr(b"matrix ");
    print_scaled(MEM[p + 5].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[p + 5].b32.s1);
    print(' ' as i32);
    print_scaled(MEM[p + 6].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[p + 6].b32.s1);
    print(' ' as i32);
    print_scaled(MEM[p + 7].b32.s0);
    print(' ' as i32);
    print_scaled(MEM[p + 7].b32.s1);
    print(' ' as i32);
    print_cstr(b"page ");
    print_int(MEM[p + 4].b16.s0 as i32);
    print(' ' as i32);

    match MEM[p + 8].b16.s1 {
        1 => print_cstr(b"pagebox cropbox "),
        2 => print_cstr(b"pagebox mediabox "),
        3 => print_cstr(b"pagebox bleedbox "),
        5 => print_cstr(b"pagebox artbox "),
        4 => print_cstr(b"pagebox trimbox "),
        _ => {}
    }

    print('(' as i32);
    for i in 0..(MEM[p + 4].b16.s1) {
        print_raw_char(
            *(&mut MEM[p + 9] as *mut memory_word as *mut u8).offset(i as isize) as UTF16_code,
            true,
        );
    }
    print(')' as i32);

    selector = old_setting;
    if cur_length() < 256 {
        dvi_out(XXX1);
        dvi_out(cur_length() as u8);
    } else {
        dvi_out(XXX4);
        dvi_four(cur_length());
    }

    let mut k = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
    while k < pool_ptr {
        dvi_out(str_pool[k as usize] as u8);
        k += 1
    }
    pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]; /* discard the string we just made */
}

pub(crate) unsafe fn finalize_dvi_file() {
    while cur_s > -1 {
        if cur_s > 0 {
            dvi_out(POP);
        } else {
            dvi_out(EOP);
            TOTAL_PAGES += 1
        }
        cur_s -= 1;
    }

    if TOTAL_PAGES == 0 {
        print_nl_cstr(b"No pages of output.");
        return;
    }

    if cur_s == -2 {
        /* This happens when the DVI gets too big; a message has already been printed */
        return;
    }

    dvi_out(POST); /* magic values: conversion ratio for sp */
    dvi_four(last_bop);
    last_bop = (dvi_offset + dvi_ptr - 5) as i32;
    dvi_four(25400000); /* magic values: conversion ratio for sp */
    dvi_four(473628672i64 as i32); /* magic values: conversion ratio for sp */
    prepare_mag();
    dvi_four(*INTPAR(IntPar::mag));
    dvi_four(max_v);
    dvi_four(max_h);
    dvi_out((max_push / 256) as u8);
    dvi_out((max_push % 256) as u8);
    dvi_out((TOTAL_PAGES / 256 % 256) as u8);
    dvi_out((TOTAL_PAGES % 256) as u8);

    while FONT_PTR > 0 {
        if font_used[FONT_PTR] {
            dvi_font_def(FONT_PTR);
        }
        FONT_PTR -= 1
    }

    dvi_out(POST_POST);
    dvi_four(last_bop);

    if semantic_pagination_enabled {
        dvi_out(SPX_ID_BYTE);
    } else {
        dvi_out(XDV_ID_BYTE);
    }

    let mut k = (4 + (DVI_BUF_SIZE - dvi_ptr) % 4) as u8;

    while k > 0 {
        dvi_out(223);
        k -= 1;
    }

    if dvi_limit == HALF_BUF {
        write_to_dvi(HALF_BUF, DVI_BUF_SIZE - 1);
    }

    if dvi_ptr as i32 > TEX_INFINITY - dvi_offset as i32 {
        cur_s = -2;
        fatal_error(b"dvi length exceeds 0x7FFFFFFF");
    }

    if dvi_ptr > 0 {
        write_to_dvi(0, dvi_ptr as usize - 1);
    }

    let mut k = ttstub_output_close(dvi_file.take().unwrap()) as u8;

    if k == 0 {
        print_nl_cstr(b"Output written on ");
        print(output_file_name);
        print_cstr(b" (");
        print_int(TOTAL_PAGES as i32);
        if TOTAL_PAGES != 1 {
            print_cstr(b" pages");
        } else {
            print_cstr(b" page");
        }
        print_cstr(b", ");
        print_int((dvi_offset + dvi_ptr) as i32);
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

unsafe fn write_to_dvi(a: usize, b: usize) {
    dvi_file
        .as_mut()
        .unwrap()
        .write(&dvi_buf[a..=b])
        .expect("failed to write data to XDV file");
}

unsafe fn dvi_swap() {
    if dvi_ptr as i32 > TEX_INFINITY - dvi_offset as i32 {
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
    dvi_gone = dvi_gone + HALF_BUF as i32;
}

unsafe fn dvi_four(x: i32) {
    let b = x.to_be_bytes();
    dvi_out(b[0]);
    dvi_out(b[1]);
    dvi_out(b[2]);
    dvi_out(b[3]);
}

unsafe fn dvi_two(s: UTF16_code) {
    let b = s.to_be_bytes();
    dvi_out(b[0]);
    dvi_out(b[1]);
}

unsafe fn dvi_pop(l: usize) {
    if l == dvi_offset + dvi_ptr && dvi_ptr > 0 {
        dvi_ptr -= 1
    } else {
        dvi_out(POP as u8);
    };
}
