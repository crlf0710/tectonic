use bridge::{abort, DisplayExt};
use std::ffi::CStr;
use std::io::Write;

use crate::help;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow};
use crate::xetex_ext::{apply_tfm_font_mapping, make_font_def, AAT_FONT_FLAG, OTGR_FONT_FLAG};
use crate::xetex_ini::Selector;
use crate::xetex_ini::{
    avail, cur_area, cur_cs, cur_dir, cur_ext, cur_h, cur_h_offset, cur_list, cur_name,
    cur_page_height, cur_page_width, cur_tok, cur_v, cur_v_offset, dead_cycles, def_ref,
    doing_leaders, doing_special, file_line_error_style_p, file_offset, font_used, init_pool_ptr,
    job_name, last_bop, log_opened, max_h, max_print_line, max_push, max_v, name_of_file,
    output_file_extension, pdf_last_x_pos, pdf_last_y_pos, pool_ptr, pool_size, rule_dp, rule_ht,
    rule_wd, rust_stdout, selector, semantic_pagination_enabled, str_pool, str_ptr, str_start,
    term_offset, write_file, write_loc, write_open, xdv_buffer, xtx_ligature_present, LR_problems,
    LR_ptr, CHAR_BASE, FONT_AREA, FONT_BC, FONT_CHECK, FONT_DSIZE, FONT_EC, FONT_GLUE, FONT_INFO,
    FONT_LETTER_SPACE, FONT_MAPPING, FONT_NAME, FONT_PTR, FONT_SIZE, MEM, TOTAL_PAGES, WIDTH_BASE,
};
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
    flush_list, flush_node_list, free_node, get_avail, get_node, get_token, internal_font_number,
    make_name_string, new_kern, new_math, new_native_word_node, open_log_file, pack_file_name,
    pack_job_name, packed_UTF16_code, prepare_mag, scaled_t, scan_toks, show_box, show_token_list,
    str_number, token_show, UTF16_code,
};
use crate::xetex_xetexd::{
    is_char_node, kern_NODE_subtype, kern_NODE_width, llist_link, print_c_string, set_NODE_type,
    set_whatsit_NODE_subtype, text_NODE_type, whatsit_NODE_subtype, BOX_depth, BOX_height,
    BOX_width, CHAR_NODE_character, CHAR_NODE_font, EDGE_NODE_edge_dist, GLUE_NODE_glue_ptr,
    GLUE_NODE_leader_ptr, GLUE_NODE_param, LIGATURE_NODE_lig_char, LIGATURE_NODE_lig_font,
    LIGATURE_NODE_lig_ptr, LLIST_info, LLIST_link, NODE_type, PIC_NODE_page, PIC_NODE_pagebox,
    PIC_NODE_path, PIC_NODE_transform_matrix, SYNCTEX_tag, TeXInt, TeXOpt, FONT_CHARACTER_WIDTH,
};
use bridge::{ttstub_output_close, ttstub_output_open};
use libc::strerror;

use bridge::OutputHandleWrapper;

const DVI_BUF_SIZE: usize = 16384;
const HALF_BUF: usize = 8192;
const FNT_NUM_0: u8 = 171; /* DVI code */

static mut dvi_file: Option<OutputHandleWrapper> = None;
static mut output_file_name: str_number = 0;
static mut dvi_buf: Vec<u8> = Vec::new();
static mut dvi_limit: usize = 0;
static mut dvi_ptr: usize = 0;
static mut dvi_offset: usize = 0;
static mut dvi_gone: i32 = 0;
static mut down_ptr: Option<usize> = Some(0);
static mut right_ptr: Option<usize> = Some(0);
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
    right_ptr = None;
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
pub(crate) unsafe fn ship_out(mut p: Box) {
    const output_comment: &[u8] = b"tectonic";

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
        show_box(Some(p.ptr()));
        end_diagnostic(true);
    }

    /*662: "Ship box `p` out." */
    /*663: "Update the values of max_h and max_v; but if the page is too
     * large, goto done". */

    if p.height() > MAX_HALFWORD
        || p.depth() > MAX_HALFWORD
        || p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset) > MAX_HALFWORD
        || p.width() + *DIMENPAR(DimenPar::h_offset) > MAX_HALFWORD
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Huge page cannot be shipped out");
        help!(
            b"The page just created is more than 18 feet tall or",
            b"more than 18 feet wide, so I suspect something went wrong."
        );
        error();

        if *INTPAR(IntPar::tracing_output) <= 0 {
            begin_diagnostic();
            print_nl_cstr(b"The following box has been deleted:");
            show_box(Some(p.ptr()));
            end_diagnostic(true);
        }
    } else {
        if p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset) > max_v {
            max_v = p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset);
        }
        if p.width() + *DIMENPAR(DimenPar::h_offset) > max_h {
            max_h = p.width() + *DIMENPAR(DimenPar::h_offset);
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
            cur_page_width = p.width() + 2 * cur_h_offset;
        }
        if *DIMENPAR(DimenPar::pdf_page_height) != 0 {
            cur_page_height = *DIMENPAR(DimenPar::pdf_page_height);
        } else {
            cur_page_height = p.height() + p.depth() + 2 * cur_v_offset;
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

            let l = output_comment.len();
            dvi_out(l as u8);
            for s in 0..l {
                dvi_out(output_comment[s]);
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

        for s in str_start[(str_ptr - TOO_BIG_CHAR) as usize]..pool_ptr {
            dvi_out(str_pool[s as usize] as u8);
        }

        pool_ptr = str_start[(str_ptr - 65536) as usize];

        /* Done with the synthesized special. The meat: emit this page box. */

        cur_v = p.height() + *DIMENPAR(DimenPar::v_offset); /*"Does this need changing for upwards mode???"*/
        if NODE_type(p.ptr()) == TextNode::VList.into() {
            vlist_out(&p);
        } else {
            hlist_out(&mut p);
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

    if LR_ptr.opt().is_some() || cur_dir != LR::LeftToRight {
        confusion(b"LR3");
    }

    if *INTPAR(IntPar::tracing_output) <= 0 {
        print_char(']' as i32);
    }

    dead_cycles = 0;
    rust_stdout.as_mut().unwrap().flush().unwrap();
    flush_node_list(Some(p.ptr()));
    synctex_teehs();
}

/*639: Output an hlist */
unsafe fn hlist_out(this_box: &mut Box) {
    let g_order = this_box.glue_order();
    let g_sign = this_box.glue_sign();

    if *INTPAR(IntPar::xetex_interword_space_shaping) > 1 {
        /*640: "Extra stuff for justifiable AAT text..." "Merge sequences of
         * words using native fonts and inter-word spaces into single
         * nodes" */

        let mut popt = this_box.list_ptr().opt();
        let mut prev_p = this_box.ptr() + 5; /* this gets the list within the box */

        while let Some(mut p) = popt {
            if llist_link(p).is_some() {
                if !is_char_node(Some(p))
                    && NODE_type(p) != TextNode::WhatsIt.into()
                    && (whatsit_NODE_subtype(p) == WhatsItNST::NativeWord
                        || whatsit_NODE_subtype(p) == WhatsItNST::NativeWordAt)
                    && FONT_LETTER_SPACE[NativeWord::from(p).font() as usize] == 0
                {
                    /* "got a word in an AAT font, might be the start of a run" */
                    let r = p;
                    let r_nw = NativeWord::from(r);
                    let mut k = r_nw.text().len() as i32;
                    let mut qopt = llist_link(p);
                    loop {
                        /*641: "Advance `q` past ignorable nodes." This test is
                         * mostly `node_is_invisible_to_interword_space`. 641 is
                         * reused a few times here. */
                        while let Some(q) = qopt.filter(|&q| {
                            !is_char_node(Some(q))
                                && (NODE_type(q) == TextNode::Penalty.into()
                                    || NODE_type(q) == TextNode::Ins.into()
                                    || NODE_type(q) == TextNode::Mark.into()
                                    || NODE_type(q) == TextNode::Adjust.into()
                                    || (NODE_type(q) == TextNode::WhatsIt.into()
                                        && [
                                            WhatsItNST::Open,
                                            WhatsItNST::Write,
                                            WhatsItNST::Close,
                                            WhatsItNST::Special,
                                            WhatsItNST::Language,
                                        ]
                                        .contains(&whatsit_NODE_subtype(q))))
                        }) {
                            qopt = llist_link(q);
                        }
                        if let Some(q) = qopt.filter(|&q| !is_char_node(Some(q))) {
                            if NODE_type(q) == TextNode::Glue.into() && *GLUE_NODE_param(q) == 0 {
                                if *GLUE_NODE_glue_ptr(q) == FONT_GLUE[r_nw.font() as usize] {
                                    /* "Found a normal space; if the next node is
                                     * another word in the same font, we'll
                                     * merge." */
                                    qopt = llist_link(q);

                                    while let Some(q) = qopt.filter(|&q| {
                                        !is_char_node(Some(q))
                                            && (NODE_type(q) == TextNode::Penalty.into()
                                                || NODE_type(q) == TextNode::Ins.into()
                                                || NODE_type(q) == TextNode::Mark.into()
                                                || NODE_type(q) == TextNode::Adjust.into()
                                                || (NODE_type(q) == TextNode::WhatsIt.into()
                                                    && [
                                                        WhatsItNST::Open,
                                                        WhatsItNST::Write,
                                                        WhatsItNST::Close,
                                                        WhatsItNST::Special,
                                                        WhatsItNST::Language,
                                                    ]
                                                    .contains(&whatsit_NODE_subtype(q))))
                                    }) {
                                        qopt = llist_link(q);
                                    }

                                    if let Some(q) = qopt.filter(|&q| {
                                        !is_char_node(Some(q))
                                            && NODE_type(q) == TextNode::WhatsIt.into()
                                            && (whatsit_NODE_subtype(q) == WhatsItNST::NativeWord
                                                || whatsit_NODE_subtype(q)
                                                    == WhatsItNST::NativeWordAt)
                                            && NativeWord::from(q).font() == r_nw.font()
                                    }) {
                                        p = q;
                                        k += 1 + NativeWord::from(q).text().len() as i32;
                                        qopt = llist_link(q);
                                        continue;
                                    }
                                } else {
                                    qopt = llist_link(q);
                                }
                                if let Some(q) = qopt.filter(|&q| {
                                    !is_char_node(Some(q))
                                        && NODE_type(q) == TextNode::Kern.into()
                                        && kern_NODE_subtype(q) == KernNST::SpaceAdjustment
                                }) {
                                    qopt = llist_link(q);
                                    while let Some(q) = qopt.filter(|&q| {
                                        !is_char_node(Some(q))
                                            && (NODE_type(q) == TextNode::Penalty.into()
                                                || NODE_type(q) == TextNode::Ins.into()
                                                || NODE_type(q) == TextNode::Mark.into()
                                                || NODE_type(q) == TextNode::Adjust.into()
                                                || NODE_type(q) == TextNode::WhatsIt.into()
                                                    && [
                                                        WhatsItNST::Open,
                                                        WhatsItNST::Write,
                                                        WhatsItNST::Close,
                                                        WhatsItNST::Special,
                                                        WhatsItNST::Language,
                                                    ]
                                                    .contains(&whatsit_NODE_subtype(q)))
                                    }) {
                                        qopt = llist_link(q);
                                    }
                                    if let Some(q) = qopt.filter(|&q| {
                                        !is_char_node(Some(q))
                                            && NODE_type(q) == TextNode::WhatsIt.into()
                                            && (whatsit_NODE_subtype(q) == WhatsItNST::NativeWord
                                                || whatsit_NODE_subtype(q)
                                                    == WhatsItNST::NativeWordAt)
                                            && NativeWord::from(q).font() == r_nw.font()
                                    }) {
                                        p = q;
                                        k += (1 + NativeWord::from(q).text().len()) as i32;
                                        qopt = llist_link(q);
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            } else {
                                if let Some(q) = qopt.filter(|&q| {
                                    !is_char_node(Some(q))
                                        && NODE_type(q) == TextNode::WhatsIt.into()
                                        && (whatsit_NODE_subtype(q) == WhatsItNST::NativeWord
                                            || whatsit_NODE_subtype(q) == WhatsItNST::NativeWordAt)
                                        && NativeWord::from(q).font() == r_nw.font()
                                }) {
                                    p = q;
                                    qopt = llist_link(q);
                                } else {
                                    break;
                                }
                            }
                        } else {
                            break;
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
                            if NODE_type(q) == TextNode::WhatsIt.into() {
                                match whatsit_NODE_subtype(q) {
                                    WhatsItNST::NativeWord | WhatsItNST::NativeWordAt => {
                                        let q = NativeWord::from(q);
                                        for j in q.text() {
                                            str_pool[pool_ptr as usize] = *j;
                                            pool_ptr += 1;
                                        }
                                        k += q.width();
                                    }
                                    _ => {}
                                }
                            } else if NODE_type(q) == TextNode::Glue.into() {
                                str_pool[pool_ptr as usize] = ' ' as i32 as packed_UTF16_code;
                                pool_ptr += 1;
                                let mut g = GlueSpec(*GLUE_NODE_glue_ptr(q) as usize);
                                k += g.size();
                                if g_sign != GlueSign::Normal {
                                    if g_sign == GlueSign::Stretching {
                                        if g.stretch_order() == g_order {
                                            k += tex_round(this_box.glue_set() * g.stretch() as f64)
                                        }
                                    } else if g.shrink_order() == g_order {
                                        k -= tex_round(this_box.glue_set() * g.shrink() as f64)
                                    }
                                }
                            } else if NODE_type(q) == TextNode::Kern.into() {
                                k += *BOX_width(q);
                            }
                            if q == p {
                                break;
                            }
                            q = *LLIST_link(q) as usize;
                        }
                        let mut nw =
                            new_native_word_node(r_nw.font() as internal_font_number, cur_length());
                        set_whatsit_NODE_subtype(nw.ptr(), whatsit_NODE_subtype(r));

                        let start = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                        nw.text_mut().copy_from_slice(
                            &str_pool[start as usize..(start + cur_length()) as usize],
                        );

                        /* "Link q into the list in place of r...p" */
                        nw.set_width(k);
                        nw.set_justified_native_glyphs();
                        let mut q = nw.ptr();
                        *LLIST_link(prev_p) = Some(q).tex_int();
                        *LLIST_link(q) = *LLIST_link(p);
                        *LLIST_link(p) = None.tex_int();
                        prev_p = r;
                        let mut popt2 = llist_link(r);

                        /* "Extract any 'invisible' nodes from the old list
                         * and insert them after the new node, so we don't
                         * lose them altogether. Note that the first node
                         * cannot be one of these, as we always start merging
                         * at a native_word node." */

                        while let Some(p) = popt2 {
                            if !is_char_node(Some(p))
                                && (NODE_type(p) == TextNode::Penalty.into()
                                    || NODE_type(p) == TextNode::Ins.into()
                                    || NODE_type(p) == TextNode::Mark.into()
                                    || NODE_type(p) == TextNode::Adjust.into()
                                    || NODE_type(p) == TextNode::WhatsIt.into()
                                        && whatsit_NODE_subtype(p) as u16 <= 4)
                            {
                                *LLIST_link(prev_p) = *LLIST_link(p);
                                *LLIST_link(p) = *LLIST_link(q);
                                *LLIST_link(q) = Some(p).tex_int();
                                q = p;
                            }
                            prev_p = p;
                            popt2 = llist_link(p);
                        }
                        flush_node_list(Some(r));
                        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                        p = q
                    }
                }
                prev_p = p;
            }
            popt = llist_link(p);
        }
    }

    /* ... resuming 639 ... */

    let mut popt = this_box.list_ptr().opt();
    cur_s += 1;
    if cur_s > 0 {
        dvi_out(PUSH as _);
    }

    max_push = max_push.max(cur_s);

    let save_loc = dvi_offset + dvi_ptr;
    let base_line = cur_v;
    let mut prev_p = this_box.ptr() + 5; /* this is list_offset, the offset of the box list pointer */

    /*1501: "Initialize hlist_out for mixed direction typesetting" */

    let tmp_ptr = get_avail();
    *LLIST_info(tmp_ptr) = u16::from(MathNST::Before) as i32;
    *LLIST_link(tmp_ptr) = LR_ptr;
    LR_ptr = Some(tmp_ptr).tex_int();
    if this_box.lr_mode() == LRMode::DList {
        if cur_dir == LR::RightToLeft {
            cur_dir = LR::LeftToRight;
            cur_h -= this_box.width();
        } else {
            this_box.set_lr_mode(LRMode::Normal);
        }
    }

    let mut cur_g: scaled_t = 0;
    let mut cur_glue: f64 = 0.0;
    if cur_dir == LR::RightToLeft && this_box.lr_mode() != LRMode::Reversed {
        /*1508: "Reverse the complete hlist and set the subtype to reversed." */
        let save_h = cur_h; /* "SyncTeX: do nothing, it is too late" */
        let tmp_ptr = popt.unwrap();
        let p = new_kern(0);
        *SYNCTEX_tag(p, MEDIUM_NODE_SIZE) = 0;
        *LLIST_link(prev_p) = Some(p).tex_int();
        cur_h = 0;
        *LLIST_link(p) = reverse(this_box, tmp_ptr, None, &mut cur_g, &mut cur_glue);
        *kern_NODE_width(p) = -cur_h;
        popt = Some(p);
        cur_h = save_h;
        this_box.set_lr_mode(LRMode::Reversed);
    }

    /* ... resuming 639 ... */

    let mut left_edge = cur_h;
    synctex_hlist(this_box.ptr());

    while let Some(mut p) = popt {
                 /*642: "Output node `p` for `hlist_out` and move to the next node,
        * maintaining the condition `cur_v = base_line`." ... "We ought to
        * give special care to the efficiency [here] since it belongs to TeX's
        * inner loop. When a `char_node` is encountered, we save a little time
        * by processing several nodes in succession[.] The program uses the
        * fact that `set_char_0 = 0`. */
        if is_char_node(Some(p)) {
            if cur_h != dvi_h {
                movement(cur_h - dvi_h, RIGHT1);
                dvi_h = cur_h
            }
            if cur_v != dvi_v {
                movement(cur_v - dvi_v, DOWN1);
                dvi_v = cur_v
            }
            loop  {
                let f = *CHAR_NODE_font(p) as usize;
                let mut c = *CHAR_NODE_character(p);
                if p != LIG_TRICK &&
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
                prev_p = *LLIST_link(prev_p) as usize;
                popt = llist_link(p);
                if !is_char_node(popt) {
                    break;
                } else {
                    p = popt.unwrap();
                }
            }
            synctex_current();
            dvi_h = cur_h;
        } else {
            /*644: "Output the non-char_node `p` and move to the next node" */
            let n = text_NODE_type(p).unwrap();
            match n {
                TextNode::HList | TextNode::VList => {
                    let mut p = Box::from(p);
                    if p.list_ptr().opt().is_none() {
                        if n == TextNode::VList {
                            synctex_void_vlist(p.ptr(), this_box.ptr());
                        } else { synctex_void_hlist(p.ptr(), this_box.ptr()); }
                        cur_h += p.width();
                    } else {
                        let save_h = dvi_h;
                        let save_v = dvi_v;
                        cur_v =
                            base_line +
                                p.shift_amount();
                        let edge =
                            cur_h +
                                p.width();
                        if cur_dir == LR::RightToLeft {
                            cur_h = edge;
                        }
                        if n == TextNode::VList {
                            vlist_out(&p);
                        } else { hlist_out(&mut p); }
                        dvi_h = save_h;
                        dvi_v = save_v;
                        cur_h = edge;
                        cur_v = base_line
                    }
                }
                TextNode::Rule => {
                    rule_ht =
                        *BOX_height(p);
                    rule_dp =
                        *BOX_depth(p);
                    rule_wd =
                        *BOX_width(p);
                    /*646: "Output a rule in an hlist" */
                    if rule_ht == NULL_FLAG {
                        rule_ht = this_box.height();
                    }
                    if rule_dp == NULL_FLAG {
                        rule_dp = this_box.depth();
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
                    /* ... resuming 644 ... */
                    {
                        cur_h += rule_wd; /* end GLUE_NODE case */
                        synctex_horizontal_rule_or_glue(p, this_box.ptr());
                    }
                }
                TextNode::WhatsIt => {
                    /*1407: "Output the whatsit node p in an hlist" */
                    unsafe fn out_font(f: usize) {
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
                    }
                    match whatsit_NODE_subtype(p) {
                        WhatsItNST::Glyph => {
                            let g = Glyph::from(p);
                            out_font(g.font() as usize);
                            dvi_out(SET_GLYPHS);
                            dvi_four(g.width());
                            dvi_two(1);
                            dvi_four(0);
                            dvi_four(0);
                            dvi_two(g.glyph());
                            cur_h +=
                                g.width();
                            dvi_h = cur_h;
                        }
                        WhatsItNST::NativeWord | WhatsItNST::NativeWordAt => {
                            let nw = NativeWord::from(p);
                            out_font(nw.font() as usize);
                            if whatsit_NODE_subtype(p) == WhatsItNST::NativeWordAt {
                                if nw.text().len() > 0 ||
                                       !nw.glyph_info_ptr().is_null()
                                   {
                                    dvi_out(SET_TEXT_AND_GLYPHS);
                                    let text = nw.text();
                                    dvi_two(text.len() as UTF16_code);
                                    for k in text {
                                        dvi_two(*k);
                                    }
                                    let len = nw.make_xdv_glyph_array_data();
                                    for k in 0..len {
                                        dvi_out(*xdv_buffer.offset(k
                                                                       as
                                                                       isize)
                                                    as
                                                    u8);
                                    }
                                }
                            } else if !nw.glyph_info_ptr().is_null()
                             {
                                dvi_out(SET_GLYPHS);
                                let len = nw.make_xdv_glyph_array_data();
                                for k in 0..len {
                                    dvi_out(*xdv_buffer.offset(k
                                                                   as
                                                                   isize)
                                                as u8);
                                }
                            }
                            cur_h +=
                                *BOX_width(p);
                            dvi_h = cur_h;
                        }
                        WhatsItNST::Pic | WhatsItNST::Pdf => {
                            let save_h = dvi_h;
                            let save_v = dvi_v;
                            cur_v = base_line;
                            let edge =
                                cur_h +
                                    *BOX_width(p);
                            pic_out(p);
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
                        _ => { out_what(p); }
                    }
                }
                TextNode::Glue => {
                    /*647: "Move right or output leaders" */
                    let mut g = GlueSpec(*GLUE_NODE_glue_ptr(p) as usize);
                    rule_wd =
                        g.size() -
                            cur_g;

                    if g_sign != GlueSign::Normal {
                        if g_sign == GlueSign::Stretching {
                            if g.stretch_order() ==
                                   g_order {
                                cur_glue +=
                                    g.stretch() as f64;
                                cur_g = tex_round((this_box.glue_set() *
                                        cur_glue).min(1_000_000_000.).max(-1_000_000_000.));
                            }
                        } else if g.shrink_order() ==
                                      g_order {
                            cur_glue -=
                                g.shrink() as
                                    f64;
                            cur_g = tex_round((this_box.glue_set() *
                                    cur_glue).min(1_000_000_000.).max(-1_000_000_000.));
                        }
                    }

                    rule_wd += cur_g;

                    /*1486: "Handle a glue node for mixed direction typesetting". */

                    if g_sign == GlueSign::Stretching &&
                           g.stretch_order() == g_order
                           ||
                           g_sign == GlueSign::Shrinking &&
                               g.shrink_order() ==
                                   g_order {
                        if g.rc().opt().is_none() {
                            free_node(g.ptr(),
                                      GLUE_SPEC_SIZE);
                        } else {
                            g.rc_dec();
                        }
                        if MEM[p].b16.s0 < A_LEADERS { // NODE_subtype(p)
                            set_NODE_type(p, TextNode::Kern);
                            *kern_NODE_width(p)
                                = rule_wd;
                        } else {
                            let mut g = GlueSpec(get_node(GLUE_SPEC_SIZE));
                            g.set_stretch_order(GlueOrder::Incorrect) /* "will never match" */
                            .set_shrink_order(GlueOrder::Incorrect)
                            .set_size(rule_wd)
                            .set_stretch(0)
                            .set_shrink(0);
                            *GLUE_NODE_glue_ptr(p) = g.ptr() as i32;
                        }
                    }
                    if MEM[p as usize].b16.s0
                           >= A_LEADERS { // NODE_subtype(p)
                        /*648: "Output leaders into an hlist, goto fin_rule if a
                         * rule or next_p if done." */
                        let leader_box = *GLUE_NODE_leader_ptr(p); /* "compensate for floating-point rounding" ?? */
                        if NODE_type(leader_box as usize) == TextNode::Rule.into() {
                            let lb = Rule::from(leader_box as usize);
                            rule_ht = lb.height();
                            rule_dp = lb.depth();
                            /*646: "Output a rule in an hlist" */
                            if rule_ht == NULL_FLAG {
                                rule_ht = this_box.height();
                            }
                            if rule_dp == NULL_FLAG {
                                rule_dp = this_box.depth();
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
                        } else {
                            let mut lb = Box::from(leader_box as usize);
                            let leader_wd = lb.width();
                            if leader_wd > 0 && rule_wd > 0 {
                                rule_wd += 10;
                                if cur_dir == LR::RightToLeft {
                                    cur_h -= 10;
                                }
                                let edge = cur_h + rule_wd;
                                let mut lx = 0;
                                /*649: "Let cur_h be the position of the first pox,
                                 * and set leader_wd + lx to the spacing between
                                 * corresponding parts of boxes". Additional
                                 * explanator comments in XTTP. */
                                if MEM[p as usize].b16.s0 == A_LEADERS {
                                    // NODE_subtype(p)
                                    let save_h = cur_h;
                                    cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);
                                    if cur_h < save_h {
                                        cur_h = cur_h + leader_wd
                                    }
                                } else {
                                    let lq = rule_wd / leader_wd;
                                    let lr = rule_wd % leader_wd;
                                    if MEM[p].b16.s0 == C_LEADERS {
                                        // NODE_subtype(p)
                                        cur_h = cur_h + lr / 2;
                                    } else {
                                        lx = lr / (lq + 1);
                                        cur_h = cur_h + (lr - (lq - 1) * lx) / 2;
                                    }
                                }

                                while cur_h + leader_wd <= edge {
                                    /*650: "Output a leader box at cur_h, then advance cur_h by leader_wd + lx" */
                                    cur_v = base_line + lb.shift_amount();
                                    if cur_v != dvi_v {
                                        movement(cur_v - dvi_v, DOWN1);
                                        dvi_v = cur_v
                                    }
                                    let save_v = dvi_v;
                                    if cur_h != dvi_h {
                                        movement(cur_h - dvi_h, RIGHT1);
                                        dvi_h = cur_h
                                    }
                                    let save_h = dvi_h;
                                    if cur_dir == LR::RightToLeft {
                                        cur_h += leader_wd;
                                    }
                                    let outer_doing_leaders = doing_leaders;
                                    doing_leaders = true;
                                    if NODE_type(leader_box as usize) == TextNode::VList.into() {
                                        vlist_out(&lb);
                                    } else {
                                        hlist_out(&mut lb);
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
                                prev_p = p;
                                popt = llist_link(p);
                                continue;
                            }
                        }
                    }

                    /* ... resuming 644 ... */
                    cur_h += rule_wd; /* end GLUE_NODE case */
                    synctex_horizontal_rule_or_glue(p, this_box.ptr());
                }
                TextNode::MarginKern => {
                    cur_h +=
                        *BOX_width(p);
                }
                TextNode::Kern => {
                    synctex_kern(p, this_box.ptr());
                    cur_h +=
                        *BOX_width(p);
                }
                TextNode::Math => {
                    synctex_math(p, this_box.ptr());
                    /* 1504: "Adjust the LR stack...; if necessary reverse and
                     * hlist segment and goto reswitch." "Breaking a paragraph
                     * into lines while TeXXeT is disabled may result in lines
                     * with unpaired math nodes. Such hlists are silently accepted
                     * in the absence of text direction directives." */
                    let (be, mode) = MathNST::from(MEM[p].b16.s0).equ();
                    if be == BE::End { // odd(NODE_subtype(p))
                        /* <= this is end_LR(p) */
                        if MathNST::from(MEM[LR_ptr as usize].b32.s0 as u16) == MathNST::Eq(BE::End, mode)
                           {
                            let tmp_ptr = LR_ptr as usize;
                            LR_ptr =
                                *LLIST_link(tmp_ptr);
                            *LLIST_link(tmp_ptr) =
                                avail.tex_int();
                            avail = Some(tmp_ptr);
                        } else if mode != MathMode::Middle { // NODE_subtype(p)
                            LR_problems += 1;
                        }
                    } else {
                        let tmp_ptr = get_avail();
                        *LLIST_info(tmp_ptr) = u16::from(MathNST::Eq(BE::End, mode)) as i32;
                        *LLIST_link(tmp_ptr) =
                            LR_ptr;
                        LR_ptr = Some(tmp_ptr).tex_int();
                        if MathNST::from(MEM[p].b16.s0).dir() !=
                                 cur_dir {
                            /*1509: "Reverse an hlist segment and goto reswitch" */
                            let save_h = cur_h; /* = lig_char(p) */
                            let tmp_ptr = llist_link(p).unwrap();
                            rule_wd =
                                *BOX_width(p);
                            free_node(p, MEDIUM_NODE_SIZE);
                            cur_dir = !cur_dir;
                            p = new_edge(cur_dir, rule_wd);
                            *LLIST_link(prev_p) = Some(p).tex_int();
                            cur_h = cur_h - left_edge + rule_wd;
                            *LLIST_link(p) =
                                reverse(this_box, tmp_ptr,
                                        Some(new_edge(!cur_dir, 0)),
                                        &mut cur_g, &mut cur_glue);
                            *EDGE_NODE_edge_dist(p) =
                                cur_h;
                            cur_dir = !cur_dir;
                            cur_h = save_h;
                            popt = Some(p);
                            continue;
                        }
                    }

                    set_NODE_type(p, TextNode::Kern);
                    cur_h += *BOX_width(p);
                }
                TextNode::Ligature => {
                    /* 675: "Make node p look like a char_node and goto reswitch" */
                    *CHAR_NODE_character(LIG_TRICK) = *LIGATURE_NODE_lig_char(p);
                    *CHAR_NODE_font(LIG_TRICK) = *LIGATURE_NODE_lig_font(p);
                    *LLIST_link(LIG_TRICK) = *LLIST_link(p);
                    popt = Some(LIG_TRICK);
                    xtx_ligature_present = true;
                    continue;
                }
                EDGE_NODE => {
                    /*1507: "Cases of hlist_out that arise in mixed direction text only" */
                    cur_h +=
                        *BOX_width(p);
                    left_edge =
                        cur_h +
                            *EDGE_NODE_edge_dist(p as usize);
                    cur_dir = LR::n(MEM[p as usize].b16.s0).unwrap();
                }
                _ => {  }
            }
            // next_p
            prev_p = p;
            popt = llist_link(p);
        }

    }

    synctex_tsilh(this_box.ptr());

    /*1502: "Finish hlist_out for mixed direction typesetting" */
    /*1505: "Check for LR anomalies" */
    while MathNST::from(*LLIST_info(LR_ptr as usize) as u16) != MathNST::Before {
        match MathNST::from(*LLIST_info(LR_ptr as usize) as u16) {
            MathNST::Eq(_, MathMode::Left) | MathNST::Eq(_, MathMode::Right) => {
                // LLIST_info(LR_ptr)
                LR_problems += 10000;
            }
            _ => {}
        }
        let tmp_ptr = LR_ptr as usize;
        LR_ptr = *LLIST_link(tmp_ptr);
        *LLIST_link(tmp_ptr) = avail.tex_int();
        avail = Some(tmp_ptr);
    }

    let tmp_ptr = LR_ptr as usize;
    LR_ptr = *LLIST_link(tmp_ptr);
    *LLIST_link(tmp_ptr) = avail.tex_int();
    avail = Some(tmp_ptr);

    if this_box.lr_mode() == LRMode::DList {
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
 * the vlist_node pointed to by tmp_ptr. The reference point of that box has
 * coordinates (cur_h, cur_v)." */
unsafe fn vlist_out(this_box: &Box) {
    let mut cur_g = 0;
    let mut cur_glue = 0_f64;
    let g_order = this_box.glue_order();
    let g_sign = this_box.glue_sign();
    let mut popt = this_box.list_ptr().opt();
    let upwards = this_box.lr_mode() == LRMode::Reversed;

    cur_s += 1;
    if cur_s > 0 {
        dvi_out(PUSH);
    }

    if cur_s > max_push {
        max_push = cur_s
    }

    let save_loc = dvi_offset + dvi_ptr;
    let left_edge = cur_h;
    synctex_vlist(this_box.ptr());

    if upwards {
        cur_v += this_box.depth();
    } else {
        cur_v -= this_box.height();
    }

    let top_edge = cur_v;

    while let Some(p) = popt {
        /*652: "Output node p and move to the next node, maintaining the
         * condition cur_h = left_edge" */
        if is_char_node(Some(p)) {
            confusion(b"vlistout");
        }
        /*653: "Output the non-char_node p" */
        let n = text_NODE_type(p).unwrap();
        match n {
            TextNode::HList | TextNode::VList => {
                let mut p = Box::from(p);
                /*654: "Output a box in a vlist" */
                if p.list_ptr().opt().is_none() {
                    if upwards {
                        cur_v -= p.depth();
                    } else {
                        cur_v += p.height();
                    }
                    if n == TextNode::VList {
                        synctex_void_vlist(p.ptr(), this_box.ptr());
                    } else {
                        synctex_void_hlist(p.ptr(), this_box.ptr());
                    }
                    if upwards {
                        cur_v -= p.height();
                    } else {
                        cur_v += p.depth();
                    }
                } else {
                    if upwards {
                        cur_v -= p.depth();
                    } else {
                        cur_v += p.height();
                    }
                    if cur_v != dvi_v {
                        movement(cur_v - dvi_v, DOWN1);
                        dvi_v = cur_v
                    }
                    let save_h = dvi_h;
                    let save_v = dvi_v;
                    if cur_dir == LR::RightToLeft {
                        cur_h = left_edge - p.shift_amount();
                    } else {
                        cur_h = left_edge + p.shift_amount();
                    }
                    if n == TextNode::VList {
                        vlist_out(&p);
                    } else {
                        hlist_out(&mut p);
                    }
                    dvi_h = save_h;
                    dvi_v = save_v;
                    if upwards {
                        cur_v = save_v - p.height();
                    } else {
                        cur_v = save_v + p.depth();
                    }
                    cur_h = left_edge
                }
            }
            TextNode::Rule => {
                rule_ht = *BOX_height(p);
                rule_dp = *BOX_depth(p);
                rule_wd = *BOX_width(p);
                // 655: "Output a rule in a vlist, goto next_p

                if rule_wd == NULL_FLAG {
                    rule_wd = this_box.width();
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
            TextNode::WhatsIt => {
                /*1403: "Output the whatsit node p in a vlist" */
                match whatsit_NODE_subtype(p) {
                    WhatsItNST::Glyph => {
                        let g = Glyph::from(p);
                        cur_v = cur_v + g.height();
                        cur_h = left_edge;
                        if cur_h != dvi_h {
                            movement(cur_h - dvi_h, RIGHT1);
                            dvi_h = cur_h
                        }
                        if cur_v != dvi_v {
                            movement(cur_v - dvi_v, DOWN1);
                            dvi_v = cur_v
                        }
                        let f = g.font() as usize;
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
                        dvi_two(g.glyph());

                        cur_v += g.depth();
                        cur_h = left_edge;
                    }
                    WhatsItNST::Pic | WhatsItNST::Pdf => {
                        let save_h = dvi_h;
                        let save_v = dvi_v;
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
            }
            TextNode::Glue => {
                /*656: "Move down or output leaders" */
                let g = GlueSpec(*GLUE_NODE_glue_ptr(p) as usize);
                rule_ht = g.size() - cur_g;

                if g_sign != GlueSign::Normal {
                    if g_sign == GlueSign::Stretching {
                        if g.stretch_order() == g_order {
                            cur_glue += g.stretch() as f64;
                            cur_g = tex_round(
                                (this_box.glue_set() * cur_glue)
                                    .min(1_000_000_000.)
                                    .max(-1_000_000_000.),
                            )
                        }
                    } else if g.shrink_order() == g_order {
                        cur_glue -= g.shrink() as f64;
                        cur_g = tex_round(
                            (this_box.glue_set() * cur_glue)
                                .min(1_000_000_000.)
                                .max(-1_000_000_000.),
                        )
                    }
                }

                rule_ht += cur_g;

                if MEM[p].b16.s0 >= A_LEADERS {
                    // NODE_subtype(p)
                    /*657: "Output leaders in a vlist, goto fin_rule if a rule
                     * or next_p if done" */
                    let leader_box = *GLUE_NODE_leader_ptr(p) as usize; /* "compensate for floating-point rounding" */

                    if NODE_type(leader_box) == TextNode::Rule.into() {
                        rule_wd = Rule::from(leader_box).width();
                        rule_dp = 0;
                        // 655: "Output a rule in a vlist, goto next_p

                        if rule_wd == NULL_FLAG {
                            rule_wd = this_box.width();
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
                            cur_h = left_edge;
                        }
                        popt = llist_link(p);
                        continue;
                    } else {
                        let mut lb = Box::from(leader_box);
                        let leader_ht = lb.height() + lb.depth();
                        if leader_ht > 0i32 && rule_ht > 0i32 {
                            rule_ht += 10i32;
                            let edge = cur_v + rule_ht;
                            let mut lx = 0;
                            /*658: "Let cur_v be the position of the first box,
                             * and set leader_ht + lx to the spacing between
                             * corresponding parts of boxes" */
                            if MEM[p].b16.s0 == A_LEADERS {
                                // NODE_subtype(p)
                                let save_v = cur_v;
                                cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);
                                if cur_v < save_v {
                                    cur_v = cur_v + leader_ht
                                }
                            } else {
                                let lq = rule_ht / leader_ht;
                                let lr = rule_ht % leader_ht;
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
                                    cur_h = left_edge - lb.shift_amount();
                                } else {
                                    cur_h = left_edge + lb.shift_amount();
                                }
                                if cur_h != dvi_h {
                                    movement(cur_h - dvi_h, RIGHT1);
                                    dvi_h = cur_h
                                }

                                let save_h = dvi_h;
                                cur_v += lb.height();

                                if cur_v != dvi_v {
                                    movement(cur_v - dvi_v, DOWN1);
                                    dvi_v = cur_v
                                }

                                let save_v = dvi_v;
                                let outer_doing_leaders = doing_leaders;
                                doing_leaders = true;

                                if NODE_type(leader_box) == TextNode::VList.into() {
                                    vlist_out(&lb);
                                } else {
                                    hlist_out(&mut lb);
                                }

                                doing_leaders = outer_doing_leaders;
                                dvi_v = save_v;
                                dvi_h = save_h;
                                cur_h = left_edge;
                                cur_v = save_v - *BOX_height(leader_box) + leader_ht + lx
                            }
                            cur_v = edge - 10;
                            popt = llist_link(p);
                            continue;
                        }
                    }
                }
                if upwards {
                    cur_v -= rule_ht
                } else {
                    cur_v += rule_ht
                }
            }
            TextNode::Kern => {
                if upwards {
                    cur_v -= *BOX_width(p);
                } else {
                    cur_v += *BOX_width(p);
                }
            }
            _ => {}
        }

        popt = llist_link(p);
    }
    synctex_tsilv(this_box.ptr());
    prune_movements(save_loc);
    if cur_s > 0 {
        dvi_pop(save_loc);
    }
    cur_s -= 1;
}

/*1510: "The reverse function defined here is responsible for reversing the
 * nodes of an hlist (segment). this_box is the enclosing hlist_node; t is to
 * become the tail of the reversed list; and the variable tmp_ptr is
 * the head of the list to be reversed. cur_g and cur_glue are the current
 * glue rounding state variables, to be updated by this function. We remove
 * nodes from the original list and add them to the head of the new one."
 */
unsafe fn reverse(
    this_box: &Box,
    tmp_ptr: usize,
    mut t: Option<usize>,
    mut cur_g: *mut scaled_t,
    mut cur_glue: *mut f64,
) -> i32 {
    let g_order = this_box.glue_order();
    let g_sign = this_box.glue_sign();
    let mut l = t;
    let mut popt = Some(tmp_ptr);
    let mut m = MIN_HALFWORD;
    let mut n = MIN_HALFWORD;
    's_58: loop {
        while let Some(mut p) = popt {
            /*1511: "Move node p to the new list and go to the next node; or
             * goto done if the end of the reflected segment has been
             * reached." */
            if is_char_node(Some(p)) {
                loop {
                    let f = *CHAR_NODE_font(p) as usize;
                    let c = *CHAR_NODE_character(p);
                    cur_h += FONT_INFO[(WIDTH_BASE[f]
                        + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1;
                    popt = llist_link(p);
                    *LLIST_link(p) = l.tex_int();
                    l = Some(p);
                    if !is_char_node(popt) {
                        break;
                    } else {
                        p = popt.unwrap();
                    }
                }
            } else {
                let mut add_rule = true;
                let q = *LLIST_link(p);
                match text_NODE_type(p).unwrap() {
                    TextNode::HList | TextNode::VList | TextNode::Rule | TextNode::Kern => {
                        rule_wd = *BOX_width(p);
                    }
                    TextNode::WhatsIt => match whatsit_NODE_subtype(p) {
                        WhatsItNST::NativeWord
                        | WhatsItNST::NativeWordAt
                        | WhatsItNST::Glyph
                        | WhatsItNST::Pic
                        | WhatsItNST::Pdf => {
                            rule_wd = *BOX_width(p);
                        }
                        _ => add_rule = false,
                    },
                    TextNode::Glue => {
                        /*1486: "Handle a glue node for mixed direction typesetting" */
                        let mut g = GlueSpec(*GLUE_NODE_glue_ptr(p) as usize); /* "will never match" */
                        rule_wd = g.size() - *cur_g; /* = mem[lig_char(tmp_ptr)] */

                        match g_sign {
                            GlueSign::Normal => {}
                            GlueSign::Stretching => {
                                if g.stretch_order() == g_order {
                                    *cur_glue = *cur_glue + g.stretch() as f64;
                                    *cur_g = tex_round(
                                        (this_box.glue_set() * *cur_glue)
                                            .min(1_000_000_000.)
                                            .max(-1_000_000_000.),
                                    );
                                }
                            }
                            GlueSign::Shrinking => {
                                if g.shrink_order() == g_order {
                                    *cur_glue = *cur_glue - g.shrink() as f64;
                                    *cur_g = tex_round(
                                        (this_box.glue_set() * *cur_glue)
                                            .min(1_000_000_000.)
                                            .max(-1_000_000_000.),
                                    );
                                }
                            }
                        }

                        rule_wd += *cur_g;

                        if g_sign == GlueSign::Stretching && g.stretch_order() == g_order
                            || g_sign == GlueSign::Shrinking && g.shrink_order() == g_order
                        {
                            if g.rc().opt().is_none() {
                                free_node(g.ptr(), GLUE_SPEC_SIZE);
                            } else {
                                g.rc_dec();
                            }
                            if MEM[p].b16.s0 < A_LEADERS {
                                // NODE_subtype(p)
                                set_NODE_type(p, TextNode::Kern);
                                *BOX_width(p) = rule_wd;
                            } else {
                                let mut g = GlueSpec(get_node(GLUE_SPEC_SIZE));
                                g.set_stretch_order(GlueOrder::Incorrect)
                                    .set_shrink_order(GlueOrder::Incorrect)
                                    .set_size(rule_wd)
                                    .set_stretch(0)
                                    .set_shrink(0);
                                *GLUE_NODE_glue_ptr(p) = g.ptr() as i32;
                            }
                        }
                    }
                    TextNode::Ligature => {
                        flush_node_list(LIGATURE_NODE_lig_ptr(p).opt());
                        let tmp_ptr = p;
                        let p = get_avail();
                        *LIGATURE_NODE_lig_char(p) = *LIGATURE_NODE_lig_char(tmp_ptr);
                        *LIGATURE_NODE_lig_font(p) = *LIGATURE_NODE_lig_font(tmp_ptr);
                        *LIGATURE_NODE_lig_ptr(p) = *LIGATURE_NODE_lig_ptr(tmp_ptr);
                        *LLIST_link(p) = q;
                        popt = Some(p);
                        free_node(tmp_ptr, SMALL_NODE_SIZE);
                        continue;
                    }
                    TextNode::Math => {
                        /*1516: "Math nodes in an inner reflected segment are
                         * modified, those at the outer level are changed into
                         * kern nodes." */
                        rule_wd = *BOX_width(p);

                        let nst = MathNST::from(MEM[p].b16.s0);
                        let (be, mode) = nst.equ();
                        if be == BE::End {
                            if MathNST::from(*LLIST_info(LR_ptr as usize) as u16)
                                != MathNST::Eq(BE::End, mode)
                            {
                                set_NODE_type(p, TextNode::Kern);
                                LR_problems += 1;
                            } else {
                                let tmp_ptr = LR_ptr as usize;
                                LR_ptr = *LLIST_link(tmp_ptr);
                                *LLIST_link(tmp_ptr) = avail.tex_int();
                                avail = Some(tmp_ptr);

                                if n > MIN_HALFWORD {
                                    n -= 1;
                                    MEM[p].b16.s0 -= 1; // NODE_subtype(p)
                                } else {
                                    set_NODE_type(p, TextNode::Kern);
                                    if m > MIN_HALFWORD {
                                        m -= 1
                                    } else {
                                        /*1517: "Finish the reverse hlist segment and goto done" */
                                        free_node(p, MEDIUM_NODE_SIZE); /* end GLUE_NODE case */
                                        let t = t.unwrap();
                                        *LLIST_link(t) = q;
                                        *BOX_width(t) = rule_wd;
                                        *EDGE_NODE_edge_dist(t) = -cur_h - rule_wd;
                                        break 's_58;
                                    }
                                }
                            }
                        } else {
                            let tmp_ptr = get_avail();
                            *LLIST_info(tmp_ptr) = u16::from(MathNST::Eq(BE::End, mode)) as i32;
                            *LLIST_link(tmp_ptr) = LR_ptr;
                            LR_ptr = Some(tmp_ptr).tex_int();
                            if n > MIN_HALFWORD || nst.dir() != cur_dir {
                                n += 1;
                                MEM[p].b16.s0 += 1; // NODE_subtype(p)
                            } else {
                                set_NODE_type(p, TextNode::Kern);
                                m += 1;
                            }
                        }
                    }
                    EDGE_NODE => confusion(b"LR2"),
                    _ => add_rule = false,
                }

                if add_rule {
                    cur_h += rule_wd;
                }

                // next_p
                *LLIST_link(p) = l.tex_int();
                let mut pp = Some(p);
                if text_NODE_type(p) == TextNode::Kern.into() {
                    if rule_wd == 0 || l.is_none() {
                        free_node(p, MEDIUM_NODE_SIZE);
                        pp = l
                    }
                }
                l = pp;
                popt = q.opt();
            }
        }
        /* ... resuming 1510 ... */
        if t.is_none() && m == MIN_HALFWORD && n == MIN_HALFWORD {
            break; /* "Manufacture a missing math node" */
        }
        popt = Some(new_math(
            0,
            MathNST::from(*LLIST_info(LR_ptr as usize) as u16),
        ));
        LR_problems += 10000i32
    }
    l.tex_int()
}

/*1506: Create a new edge node of subtype `s` and width `w` */
pub(crate) unsafe fn new_edge(s: LR, w: scaled_t) -> usize {
    let p = get_node(EDGE_NODE_SIZE);
    set_NODE_type(p, EDGE_NODE);
    MEM[p].b16.s0 = s as u16; // set_NODE_subtype
    *BOX_width(p) = w;
    *EDGE_NODE_edge_dist(p) = 0;
    p
}

pub(crate) unsafe fn out_what(p: usize) {
    let mut j: i16;
    match whatsit_NODE_subtype(p) {
        WhatsItNST::Open => {
            let p = OpenFile(p);
            if doing_leaders {
                return;
            }

            j = p.id() as i16;

            if write_open[j as usize] {
                ttstub_output_close(write_file[j as usize].take().unwrap());
            }

            if j >= 16 {
                return;
            }

            cur_name = p.name();
            cur_area = p.area();
            cur_ext = p.ext();
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
        WhatsItNST::Write => {
            let p = WriteFile(p);
            if doing_leaders {
                return;
            }
            write_out(&p);
            return;
        }
        WhatsItNST::Close => {
            let p = CloseFile(p);
            if doing_leaders {
                return;
            }

            j = p.id() as i16;
            if write_open[j as usize] {
                ttstub_output_close(write_file[j as usize].take().unwrap());
            }

            write_open[j as usize] = false;
            return;
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
        let mut l = 0;
        let mut k = str_start[(FONT_NAME[f] as i64 - 65536) as usize];

        while l == 0 && k < str_start[((FONT_NAME[f] + 1) as i64 - 65536) as usize] {
            if str_pool[k as usize] as i32 == ':' as i32 {
                l = k - str_start[(FONT_NAME[f] as i64 - 65536) as usize]
            }
            k += 1;
        }
        if l == 0i32 {
            l = length(FONT_NAME[f])
        }
        dvi_out(l as u8);
        for k in str_start[(FONT_AREA[f] as i64 - 65536) as usize]
            ..str_start[((FONT_AREA[f] + 1) as i64 - 65536) as usize]
        {
            dvi_out(str_pool[k as usize] as u8);
        }
        for k in str_start[(FONT_NAME[f] as i64 - 65536) as usize]
            ..(str_start[(FONT_NAME[f] as i64 - 65536) as usize] + l)
        {
            dvi_out(str_pool[k as usize] as u8);
        }
    };
}

unsafe fn movement(mut w: scaled_t, mut o: u8) {
    let mut k: i32 = 0;
    let mut q = get_node(MOVEMENT_NODE_SIZE);
    MEM[q + 1].b32.s1 = w;
    MEM[q + 2].b32.s1 = (dvi_offset + dvi_ptr) as i32;
    if o == DOWN1 {
        *LLIST_link(q) = down_ptr.tex_int();
        down_ptr = Some(q);
    } else {
        *LLIST_link(q) = right_ptr.tex_int();
        right_ptr = Some(q);
    }

    let mut popt = llist_link(q);
    let mut mstate = MoveSeen::None;

    loop {
        if let Some(p) = popt {
            if MEM[(p + 1) as usize].b32.s1 == w {
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
            popt = llist_link(p);
        } else {
            return not_found(q, o, w);
        }
    }

    /*629: found:*/
    unsafe fn found(mut q: usize, o: u8, p: usize) {
        MEM[q].b32.s0 = MEM[p].b32.s0; /*634:*/
        if MEM[q].b32.s0 == MoveDir::YHere as i32 {
            dvi_out(o + 4); /* max_selector enum */
            while llist_link(q) != Some(p) {
                q = *LLIST_link(q) as usize;

                match MoveDir::from(MEM[q].b32.s0) {
                    MoveDir::YZOk => MEM[q].b32.s0 = MoveDir::ZOk as i32,
                    MoveDir::YOk => MEM[q].b32.s0 = MoveDir::DFixed as i32,
                    _ => {}
                }
            }
        } else {
            dvi_out(o + 9);
            while llist_link(q) != Some(p) {
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

        down_ptr = llist_link(p);
        free_node(p, MOVEMENT_NODE_SIZE);
    }
    while let Some(p) = right_ptr {
        if MEM[p + 2].b32.s1 < l as i32 {
            return;
        }
        right_ptr = llist_link(p);
        free_node(p, MOVEMENT_NODE_SIZE);
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
        for k in str_start[(str_ptr - TOO_BIG_CHAR) as usize]..pool_ptr {
            dvi_out(str_pool[k as usize] as u8);
        }
    }
    pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
    doing_special = false;
}

unsafe fn write_out(p: &WriteFile) {
    let q = get_avail();
    MEM[q].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
    let mut r = get_avail();
    *LLIST_link(q) = Some(r).tex_int();
    MEM[r].b32.s0 = CS_TOKEN_FLAG + END_WRITE as i32;
    begin_token_list(q, Btl::Inserted);
    begin_token_list(p.tokens() as usize, Btl::WriteText);
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
        help!(
            b"On this page there\'s a \\write with fewer real {\'s than }\'s.",
            b"I can\'t handle that very well; good luck."
        );
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
    let j = p.id() as i16;

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
    let matrix = PIC_NODE_transform_matrix(p);
    print_scaled(matrix.0);
    print(' ' as i32);
    print_scaled(matrix.1);
    print(' ' as i32);
    print_scaled(matrix.2);
    print(' ' as i32);
    print_scaled(matrix.3);
    print(' ' as i32);
    print_scaled(matrix.4);
    print(' ' as i32);
    print_scaled(matrix.5);
    print(' ' as i32);
    print_cstr(b"page ");
    print_int(*PIC_NODE_page(p) as i32);
    print(' ' as i32);

    match *PIC_NODE_pagebox(p) {
        1 => print_cstr(b"pagebox cropbox "),
        2 => print_cstr(b"pagebox mediabox "),
        3 => print_cstr(b"pagebox bleedbox "),
        5 => print_cstr(b"pagebox artbox "),
        4 => print_cstr(b"pagebox trimbox "),
        _ => {}
    }

    print('(' as i32);
    for i in PIC_NODE_path(p) {
        print_raw_char(*i as UTF16_code, true);
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
