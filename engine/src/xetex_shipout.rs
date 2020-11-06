use bridge::abort;
use std::ffi::CString;
use std::io::Write;

use crate::help;
use crate::node::*;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow};
use crate::xetex_ext::{apply_tfm_font_mapping, make_font_def, Font};
use crate::xetex_ini::shell_escape_enabled;
use crate::xetex_ini::Selector;
use crate::xetex_ini::{
    avail, cur_area, cur_dir, cur_ext, cur_h, cur_h_offset, cur_input, cur_list, cur_name,
    cur_page_height, cur_page_width, cur_v, cur_v_offset, dead_cycles, def_ref, doing_leaders,
    doing_special, file_line_error_style_p, file_offset, font_used, init_pool_ptr, input_state_t,
    job_name, last_bop, log_opened, max_h, max_print_line, max_push, max_v, name_of_file,
    output_file_extension, pdf_last_x_pos, pdf_last_y_pos, pool_ptr, pool_size, rule_dp, rule_ht,
    rule_wd, rust_stdout, selector, semantic_pagination_enabled, str_pool, str_ptr, str_start,
    term_offset, write_file, write_loc, write_open, xtx_ligature_present, LR_problems, LR_ptr,
    CHAR_BASE, FONT_AREA, FONT_BC, FONT_CHECK, FONT_DSIZE, FONT_EC, FONT_GLUE, FONT_INFO,
    FONT_LAYOUT_ENGINE, FONT_LETTER_SPACE, FONT_MAPPING, FONT_NAME, FONT_PTR, FONT_SIZE, MEM,
    TOTAL_PAGES, WIDTH_BASE,
};
use crate::xetex_output::{
    print, print_chr, print_cstr, print_file_line, print_file_name, print_int, print_ln,
    print_nl_cstr, print_raw_char, print_scaled,
};
use crate::xetex_scaledmath::{tex_round, Scaled};
use crate::xetex_stringpool::{length, PoolString};
use crate::xetex_synctex::{
    synctex_current, synctex_hlist, synctex_horizontal_rule_or_glue, synctex_kern, synctex_math,
    synctex_sheet, synctex_teehs, synctex_tsilh, synctex_tsilv, synctex_vlist, synctex_void_hlist,
    synctex_void_vlist,
};
use crate::xetex_texmfmp::maketexstring;
use crate::xetex_xetex0::{
    begin_token_list, cur_length, diagnostic, effective_char, end_token_list, flush_list,
    flush_node_list, free_node, get_avail, get_node, get_token, internal_font_number,
    make_name_string, new_kern, new_math, new_native_word_node, open_log_file, pack_file_name,
    pack_job_name, packed_UTF16_code, prepare_mag, scan_toks, show_box, show_token_list,
    str_number, token_show, UTF16_code,
};
use crate::xetex_xetexd::{
    is_char_node, llist_link, print_c_str, set_NODE_type, LLIST_link, SYNCTEX_tag, TeXInt, TeXOpt,
    FONT_CHARACTER_WIDTH,
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
static mut dvi_h: Scaled = Scaled::ZERO;
static mut dvi_v: Scaled = Scaled::ZERO;
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
pub(crate) unsafe fn ship_out(mut p: List) {
    const output_comment: &[u8] = b"tectonic";

    synctex_sheet(*INTPAR(IntPar::mag));

    if job_name == 0 {
        open_log_file();
    }

    if *INTPAR(IntPar::tracing_output) > 0 {
        print_nl_cstr("");
        print_ln();
        print_cstr("Completed box being shipped out");
    }

    if term_offset > max_print_line - 9 {
        print_ln();
    } else if term_offset > 0 || file_offset > 0 {
        print_chr(' ');
    }

    print_chr('[');
    let mut j = 9;
    while j > 0 && *COUNT_REG(j as _) == 0 {
        j -= 1;
    }

    for k in 0..=j {
        print_int(*COUNT_REG(k as _));
        if k < j {
            print_chr('.');
        }
    }

    rust_stdout.as_mut().unwrap().flush().unwrap();

    if *INTPAR(IntPar::tracing_output) > 0 {
        print_chr(']');
        diagnostic(true, || show_box(Some(p.ptr())));
    }

    /*662: "Ship box `p` out." */
    /*663: "Update the values of max_h and max_v; but if the page is too
     * large, goto done". */

    if p.height() > Scaled::MAX_HALFWORD
        || p.depth() > Scaled::MAX_HALFWORD
        || p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset) > Scaled::MAX_HALFWORD
        || p.width() + *DIMENPAR(DimenPar::h_offset) > Scaled::MAX_HALFWORD
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Huge page cannot be shipped out");
        help!(
            "The page just created is more than 18 feet tall or",
            "more than 18 feet wide, so I suspect something went wrong."
        );
        error();

        if *INTPAR(IntPar::tracing_output) <= 0 {
            diagnostic(true, || {
                print_nl_cstr("The following box has been deleted:");
                show_box(Some(p.ptr()));
            });
        }
    } else {
        if p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset) > max_v {
            max_v = p.height() + p.depth() + *DIMENPAR(DimenPar::v_offset);
        }
        if p.width() + *DIMENPAR(DimenPar::h_offset) > max_h {
            max_h = p.width() + *DIMENPAR(DimenPar::h_offset);
        }

        /*637: "Initialize variables as ship_out begins." */

        dvi_h = Scaled::ZERO;
        dvi_v = Scaled::ZERO;
        cur_h = *DIMENPAR(DimenPar::h_offset);
        dvi_f = 0;

        /*1405: "Calculate page dimensions and margins" */
        /* 4736287 = round(0xFFFF * 72.27) ; i.e., 1 inch expressed as a Scaled */
        const S_72_27: Scaled = Scaled(4736287);
        cur_h_offset = *DIMENPAR(DimenPar::h_offset) + S_72_27;
        cur_v_offset = *DIMENPAR(DimenPar::v_offset) + S_72_27;

        if *DIMENPAR(DimenPar::pdf_page_width) != Scaled::ZERO {
            cur_page_width = *DIMENPAR(DimenPar::pdf_page_width);
        } else {
            cur_page_width = p.width() + cur_h_offset * 2;
        }
        if *DIMENPAR(DimenPar::pdf_page_height) != Scaled::ZERO {
            cur_page_height = *DIMENPAR(DimenPar::pdf_page_height);
        } else {
            cur_page_height = p.height() + p.depth() + cur_v_offset * 2;
        }

        /* ... resuming 637 ... open up the DVI file if needed */

        if output_file_name == 0 {
            if job_name == 0 {
                open_log_file();
            }
            pack_job_name(&output_file_extension);
            let name = CString::new(name_of_file.as_str()).unwrap();
            dvi_file = ttstub_output_open(name.as_ptr(), 0);
            if dvi_file.is_none() {
                abort!("cannot open output file \"{}\"", name_of_file);
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

            dvi_four(25_400_000); /* magic values: conversion ratio for sp */
            dvi_four(Scaled(473628672).0); /* 7227 magic values: conversion ratio for sp */

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
        print_cstr("pdf:pagesize ");
        if *DIMENPAR(DimenPar::pdf_page_width) <= Scaled::ZERO
            || *DIMENPAR(DimenPar::pdf_page_height) <= Scaled::ZERO
        {
            print_cstr("default");
        } else {
            print_cstr("width");
            print(' ' as i32);
            print_scaled(*DIMENPAR(DimenPar::pdf_page_width));
            print_cstr("pt");
            print(' ' as i32);
            print_cstr("height");
            print(' ' as i32);
            print_scaled(*DIMENPAR(DimenPar::pdf_page_height));
            print_cstr("pt");
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
        if p.is_vertical() {
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
        print_nl_cstr("\\endL or \\endR problem (");
        print_int(LR_problems / 10000);
        print_cstr(" missing, ");
        print_int(LR_problems % 10000);
        print_cstr(" extra");
        LR_problems = 0;
        print_chr(')');
        print_ln();
    }

    if LR_ptr.opt().is_some() || cur_dir != LR::LeftToRight {
        confusion("LR3");
    }

    if *INTPAR(IntPar::tracing_output) <= 0 {
        print_chr(']');
    }

    dead_cycles = 0;
    rust_stdout.as_mut().unwrap().flush().unwrap();
    flush_node_list(Some(p.ptr()));
    synctex_teehs();
}

/*639: Output an hlist */
unsafe fn hlist_out(this_box: &mut List) {
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
                match CharOrText::from(p) {
                    CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(r_nw)))
                        if FONT_LETTER_SPACE[r_nw.font() as usize] == Scaled::ZERO =>
                    {
                        /* "got a word in an AAT font, might be the start of a run" */
                        let mut k = r_nw.text().len() as i32;
                        let mut qopt = llist_link(p);
                        loop {
                            /*641: "Advance `q` past ignorable nodes." This test is
                             * mostly `node_is_invisible_to_interword_space`. 641 is
                             * reused a few times here. */
                            while let Some(q) = qopt {
                                if match CharOrText::from(p) {
                                    CharOrText::Text(n) => match n {
                                        TxtNode::Penalty(_)
                                        | TxtNode::Ins(_)
                                        | TxtNode::Mark(_)
                                        | TxtNode::Adjust(_) => true,
                                        TxtNode::WhatsIt(n) => match n {
                                            WhatsIt::Open(_)
                                            | WhatsIt::Write(_)
                                            | WhatsIt::Close(_)
                                            | WhatsIt::Special(_)
                                            | WhatsIt::Language(_) => true,
                                            _ => false,
                                        },
                                        _ => false,
                                    },
                                    _ => false,
                                } {
                                    qopt = llist_link(q);
                                } else {
                                    break;
                                }
                            }
                            match qopt.map(|q| CharOrText::from(q)) {
                                Some(CharOrText::Text(TxtNode::Glue(g))) if g.param() == 0 => {
                                    if g.glue_ptr() == FONT_GLUE[r_nw.font() as usize] {
                                        /* "Found a normal space; if the next node is
                                         * another word in the same font, we'll
                                         * merge." */
                                        qopt = llist_link(g.ptr());
                                        while let Some(q) = qopt {
                                            if match CharOrText::from(p) {
                                                CharOrText::Text(n) => match n {
                                                    TxtNode::Penalty(_)
                                                    | TxtNode::Ins(_)
                                                    | TxtNode::Mark(_)
                                                    | TxtNode::Adjust(_) => true,
                                                    TxtNode::WhatsIt(n) => match n {
                                                        WhatsIt::Open(_)
                                                        | WhatsIt::Write(_)
                                                        | WhatsIt::Close(_)
                                                        | WhatsIt::Special(_)
                                                        | WhatsIt::Language(_) => true,
                                                        _ => false,
                                                    },
                                                    _ => false,
                                                },
                                                _ => false,
                                            } {
                                                qopt = llist_link(q);
                                            } else {
                                                break;
                                            }
                                        }

                                        match qopt.map(CharOrText::from) {
                                            Some(CharOrText::Text(TxtNode::WhatsIt(
                                                WhatsIt::NativeWord(q),
                                            ))) if q.font() == r_nw.font() => {
                                                p = q.ptr();
                                                k += 1 + q.text().len() as i32;
                                                qopt = llist_link(q.ptr());
                                                continue;
                                            }
                                            _ => {}
                                        }
                                    } else {
                                        qopt = llist_link(g.ptr());
                                    }
                                    match qopt.map(|q| Node::from(q)) {
                                        Some(Node::Text(TxtNode::Kern(kq)))
                                            if kq.subtype() == KernType::SpaceAdjustment =>
                                        {
                                            qopt = llist_link(kq.ptr());
                                            while let Some(q) = qopt {
                                                if match CharOrText::from(p) {
                                                    CharOrText::Text(n) => match n {
                                                        TxtNode::Penalty(_)
                                                        | TxtNode::Ins(_)
                                                        | TxtNode::Mark(_)
                                                        | TxtNode::Adjust(_) => true,
                                                        TxtNode::WhatsIt(n) => match n {
                                                            WhatsIt::Open(_)
                                                            | WhatsIt::Write(_)
                                                            | WhatsIt::Close(_)
                                                            | WhatsIt::Special(_)
                                                            | WhatsIt::Language(_) => true,
                                                            _ => false,
                                                        },
                                                        _ => false,
                                                    },
                                                    _ => false,
                                                } {
                                                    qopt = llist_link(q);
                                                } else {
                                                    break;
                                                }
                                            }
                                            match qopt.map(CharOrText::from) {
                                                Some(CharOrText::Text(TxtNode::WhatsIt(
                                                    WhatsIt::NativeWord(q),
                                                ))) if q.font() == r_nw.font() => {
                                                    p = q.ptr();
                                                    k += (1 + q.text().len()) as i32;
                                                    qopt = llist_link(q.ptr());
                                                }
                                                _ => break,
                                            }
                                        }
                                        _ => break,
                                    }
                                }
                                Some(CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(
                                    q,
                                )))) if q.font() == r_nw.font() => {
                                    p = q.ptr();
                                    qopt = llist_link(q.ptr());
                                }
                                _ => break,
                            }
                        }
                        /* "Now r points to the first native_word_node of the run,
                         * and p to the last." */
                        if p != r_nw.ptr() {
                            if pool_ptr + k > pool_size {
                                overflow("pool size", (pool_size - init_pool_ptr) as usize);
                            }
                            let mut k = Scaled::ZERO;
                            let mut q = r_nw.ptr();
                            loop {
                                match Node::from(q) {
                                    Node::Text(TxtNode::WhatsIt(q)) => match q {
                                        WhatsIt::NativeWord(q) => {
                                            for j in q.text() {
                                                str_pool[pool_ptr as usize] = *j;
                                                pool_ptr += 1;
                                            }
                                            k += q.width();
                                        }
                                        _ => {}
                                    },
                                    Node::Text(TxtNode::Glue(q)) => {
                                        str_pool[pool_ptr as usize] =
                                            ' ' as i32 as packed_UTF16_code;
                                        pool_ptr += 1;
                                        let mut g = GlueSpec(q.glue_ptr() as usize);
                                        k += g.size();
                                        if g_sign != GlueSign::Normal {
                                            if g_sign == GlueSign::Stretching {
                                                if g.stretch_order() == g_order {
                                                    k += tex_round(
                                                        this_box.glue_set() * g.stretch().0 as f64,
                                                    )
                                                }
                                            } else if g.shrink_order() == g_order {
                                                k -= tex_round(
                                                    this_box.glue_set() * g.shrink().0 as f64,
                                                )
                                            }
                                        }
                                    }
                                    Node::Text(TxtNode::Kern(q)) => {
                                        k += q.width();
                                    }
                                    _ => {}
                                }
                                if q == p {
                                    break;
                                }
                                q = *LLIST_link(q) as usize;
                            }
                            let mut nw = new_native_word_node(
                                r_nw.font() as internal_font_number,
                                cur_length(),
                            );
                            nw.set_actual_text_from(&r_nw);

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
                            prev_p = r_nw.ptr();
                            let mut popt2 = llist_link(r_nw.ptr());

                            /* "Extract any 'invisible' nodes from the old list
                             * and insert them after the new node, so we don't
                             * lose them altogether. Note that the first node
                             * cannot be one of these, as we always start merging
                             * at a native_word node." */

                            while let Some(p) = popt2 {
                                if match CharOrText::from(p) {
                                    CharOrText::Text(n) => match n {
                                        TxtNode::Penalty(_)
                                        | TxtNode::Ins(_)
                                        | TxtNode::Mark(_)
                                        | TxtNode::Adjust(_) => true,
                                        TxtNode::WhatsIt(n) => match n {
                                            WhatsIt::Open(_)
                                            | WhatsIt::Write(_)
                                            | WhatsIt::Close(_)
                                            | WhatsIt::Special(_)
                                            | WhatsIt::Language(_) => true,
                                            _ => false,
                                        },
                                        _ => false,
                                    },
                                    _ => false,
                                } {
                                    *LLIST_link(prev_p) = *LLIST_link(p);
                                    *LLIST_link(p) = *LLIST_link(q);
                                    *LLIST_link(q) = Some(p).tex_int();
                                    q = p;
                                }
                                prev_p = p;
                                popt2 = llist_link(p);
                            }
                            flush_node_list(Some(r_nw.ptr()));
                            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                            p = q
                        }
                    }
                    _ => {}
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

    let mut tmp_ptr = Math(get_avail());
    tmp_ptr.set_subtype_i32(MathType::Before);
    *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
    LR_ptr = Some(tmp_ptr.ptr()).tex_int();
    if this_box.lr_mode() == LRMode::DList {
        if cur_dir == LR::RightToLeft {
            cur_dir = LR::LeftToRight;
            cur_h -= this_box.width();
        } else {
            this_box.set_lr_mode(LRMode::Normal);
        }
    }

    let mut cur_g: Scaled = Scaled::ZERO;
    let mut cur_glue: f64 = 0.0;
    if cur_dir == LR::RightToLeft && this_box.lr_mode() != LRMode::Reversed {
        /*1508: "Reverse the complete hlist and set the subtype to reversed." */
        let save_h = cur_h; /* "SyncTeX: do nothing, it is too late" */
        let tmp_ptr = popt.unwrap();
        let mut p = Kern(new_kern(Scaled::ZERO));
        *SYNCTEX_tag(p.ptr(), MEDIUM_NODE_SIZE) = 0;
        *LLIST_link(prev_p) = Some(p.ptr()).tex_int();
        cur_h = Scaled::ZERO;
        *LLIST_link(p.ptr()) = reverse(this_box, tmp_ptr, None, &mut cur_g, &mut cur_glue);
        p.set_width(-cur_h);
        popt = Some(p.ptr());
        cur_h = save_h;
        this_box.set_lr_mode(LRMode::Reversed);
    }

    /* ... resuming 639 ... */

    let mut left_edge = cur_h;
    synctex_hlist(this_box);

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
                let chr = Char(p);
                let f = chr.font() as usize;
                let mut c = chr.character();
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
            match TxtNode::from(p) {
                TxtNode::List(mut p) => {
                    if p.list_ptr().opt().is_none() {
                        if p.is_vertical() {
                            synctex_void_vlist(&p, this_box);
                        } else { synctex_void_hlist(&p, this_box); }
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
                        if p.is_vertical() {
                            vlist_out(&p);
                        } else { hlist_out(&mut p); }
                        dvi_h = save_h;
                        dvi_v = save_v;
                        cur_h = edge;
                        cur_v = base_line
                    }
                }
                TxtNode::Rule(p) => {
                    rule_ht =
                        p.height();
                    rule_dp =
                        p.depth();
                    rule_wd =
                        p.width();
                    /*646: "Output a rule in an hlist" */
                    if rule_ht == NULL_FLAG {
                        rule_ht = this_box.height();
                    }
                    if rule_dp == NULL_FLAG {
                        rule_dp = this_box.depth();
                    }
                    rule_ht += rule_dp;
                    if rule_ht > Scaled::ZERO && rule_wd > Scaled::ZERO {
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
                        dvi_four(rule_ht.0);
                        dvi_four(rule_wd.0);
                        cur_v = base_line;
                        dvi_h += rule_wd;
                    }
                    /* ... resuming 644 ... */
                    {
                        cur_h += rule_wd; /* end GLUE_NODE case */
                        synctex_horizontal_rule_or_glue(p.ptr(), this_box.ptr());
                    }
                }
                TxtNode::WhatsIt(p) => {
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
                    match p {
                        WhatsIt::Glyph(g) => {
                            out_font(g.font() as usize);
                            dvi_out(SET_GLYPHS);
                            dvi_four(g.width().0);
                            dvi_two(1);
                            dvi_four(0);
                            dvi_four(0);
                            dvi_two(g.glyph());
                            cur_h +=
                                g.width();
                            dvi_h = cur_h;
                        }
                        WhatsIt::NativeWord(nw) => {
                            out_font(nw.font() as usize);
                            if nw.actual_text() {
                                if nw.text().len() > 0 ||
                                       !nw.glyph_info_ptr().is_null()
                                   {
                                    dvi_out(SET_TEXT_AND_GLYPHS);
                                    let text = nw.text();
                                    dvi_two(text.len() as UTF16_code);
                                    for k in text {
                                        dvi_two(*k);
                                    }
                                    for &k in &nw.make_xdv_glyph_array_data() {
                                        dvi_out(k);
                                    }
                                }
                            } else if !nw.glyph_info_ptr().is_null()
                             {
                                dvi_out(SET_GLYPHS);
                                for &k in &nw.make_xdv_glyph_array_data() {
                                    dvi_out(k);
                                }
                            }
                            cur_h +=
                                nw.width();
                            dvi_h = cur_h;
                        }
                        WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                            let save_h = dvi_h;
                            let save_v = dvi_v;
                            cur_v = base_line;
                            let edge =
                                cur_h +
                                    p.width();
                            pic_out(&p);
                            dvi_h = save_h;
                            dvi_v = save_v;
                            cur_h = edge;
                            cur_v = base_line
                        }
                        WhatsIt::PdfSavePos(_) => {
                            pdf_last_x_pos = cur_h + cur_h_offset;
                            pdf_last_y_pos =
                                cur_page_height - cur_v - cur_v_offset
                        }
                        _ => { out_what(&mut cur_input, &p); }
                    }
                }
                TxtNode::Glue(mut p) => {
                    /*647: "Move right or output leaders" */
                    let mut g = GlueSpec(p.glue_ptr() as usize);
                    rule_wd =
                        g.size() -
                            cur_g;

                    if g_sign != GlueSign::Normal {
                        if g_sign == GlueSign::Stretching {
                            if g.stretch_order() ==
                                   g_order {
                                cur_glue +=
                                    g.stretch().0 as f64;
                                cur_g = tex_round((this_box.glue_set() *
                                        cur_glue).min(1_000_000_000.).max(-1_000_000_000.));
                            }
                        } else if g.shrink_order() ==
                                      g_order {
                            cur_glue -=
                                g.shrink().0 as
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
                        if p.param() < A_LEADERS {
                            set_NODE_type(p.ptr(), TextNode::Kern);
                            Kern(p.ptr()).set_width(rule_wd);
                        } else {
                            let mut g = GlueSpec(get_node(GLUE_SPEC_SIZE));
                            g.set_stretch_order(GlueOrder::Incorrect) /* "will never match" */
                            .set_shrink_order(GlueOrder::Incorrect)
                            .set_size(rule_wd)
                            .set_stretch(Scaled::ZERO)
                            .set_shrink(Scaled::ZERO);
                            p.set_glue_ptr(g.ptr() as i32);
                        }
                    }
                    if p.param() >= A_LEADERS {
                        /*648: "Output leaders into an hlist, goto fin_rule if a
                         * rule or next_p if done." */
                        let leader_box = p.leader_ptr(); /* "compensate for floating-point rounding" ?? */
                        match &mut Node::from(leader_box as usize) {
                            Node::Text(TxtNode::Rule(lb)) => {
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
                                if rule_ht > Scaled::ZERO && rule_wd > Scaled::ZERO {
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
                                    dvi_four(rule_ht.0);
                                    dvi_four(rule_wd.0);
                                    cur_v = base_line;
                                    dvi_h += rule_wd;
                                }
                            }
                            Node::Text(TxtNode::List(lb)) => {
                                let leader_wd = lb.width();
                                if leader_wd > Scaled::ZERO && rule_wd > Scaled::ZERO {
                                    rule_wd += Scaled(10);
                                    if cur_dir == LR::RightToLeft {
                                        cur_h -= Scaled(10);
                                    }
                                    let edge = cur_h + rule_wd;
                                    let mut lx = Scaled::ZERO;
                                    /*649: "Let cur_h be the position of the first pox,
                                     * and set leader_wd + lx to the spacing between
                                     * corresponding parts of boxes". Additional
                                     * explanator comments in XTTP. */
                                    if p.param() == A_LEADERS {
                                        let save_h = cur_h;
                                        cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);
                                        if cur_h < save_h {
                                            cur_h = cur_h + leader_wd
                                        }
                                    } else {
                                        let lq = rule_wd / leader_wd;
                                        let lr = rule_wd % leader_wd;
                                        if p.param() == C_LEADERS {
                                            cur_h = cur_h + lr / 2;
                                        } else {
                                            lx = lr / (lq + 1);
                                            cur_h = cur_h + (lr -  lx * (lq - 1)) / 2;
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
                                        if lb.is_vertical() {
                                            vlist_out(lb);
                                        } else {
                                            hlist_out(lb);
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
                                        cur_h = edge - Scaled(10);
                                    }
                                    prev_p = p.ptr();
                                    popt = llist_link(p.ptr());
                                    continue;
                                }
                            }
                            _ => {}
                        }
                    }

                    /* ... resuming 644 ... */
                    cur_h += rule_wd; /* end GLUE_NODE case */
                    synctex_horizontal_rule_or_glue(p.ptr(), this_box.ptr());
                }
                TxtNode::MarginKern(p) => {
                    cur_h +=
                        p.width();
                }
                TxtNode::Kern(p) => {
                    synctex_kern(p.ptr(), this_box.ptr());
                    cur_h +=
                        p.width();
                }
                TxtNode::Math(p) => {
                    synctex_math(p.ptr(), this_box.ptr());
                    /* 1504: "Adjust the LR stack...; if necessary reverse and
                     * hlist segment and goto reswitch." "Breaking a paragraph
                     * into lines while TeXXeT is disabled may result in lines
                     * with unpaired math nodes. Such hlists are silently accepted
                     * in the absence of text direction directives." */
                    let (be, mode) = p.subtype().equ();
                    if be == BE::End {
                        /* <= this is end_LR(p) */
                        if Math(LR_ptr as usize).subtype_i32() == MathType::Eq(BE::End, mode)
                           {
                            let tmp_ptr = LR_ptr as usize;
                            LR_ptr =
                                *LLIST_link(tmp_ptr);
                            *LLIST_link(tmp_ptr) =
                                avail.tex_int();
                            avail = Some(tmp_ptr);
                        } else if mode != MathMode::Middle {
                            LR_problems += 1;
                        }
                    } else {
                        let mut tmp_ptr = Math(get_avail());
                        tmp_ptr.set_subtype_i32(MathType::Eq(BE::End, mode));
                        *LLIST_link(tmp_ptr.ptr()) =
                            LR_ptr;
                        LR_ptr = Some(tmp_ptr.ptr()).tex_int();
                        if p.dir() !=
                                 cur_dir {
                            /*1509: "Reverse an hlist segment and goto reswitch" */
                            let save_h = cur_h; /* = char(p) */
                            let tmp_ptr = llist_link(p.ptr()).unwrap();
                            rule_wd =
                                p.width();
                            p.free();
                            cur_dir = !cur_dir;
                            let mut p = Edge(new_edge(cur_dir, rule_wd));
                            *LLIST_link(prev_p) = Some(p.ptr()).tex_int();
                            cur_h = cur_h - left_edge + rule_wd;
                            *LLIST_link(p.ptr()) =
                                reverse(this_box, tmp_ptr,
                                        Some(new_edge(!cur_dir, Scaled::ZERO)),
                                        &mut cur_g, &mut cur_glue);
                            p.set_edge_dist(cur_h);
                            cur_dir = !cur_dir;
                            cur_h = save_h;
                            popt = Some(p.ptr());
                            continue;
                        }
                    }

                    set_NODE_type(p.ptr(), TextNode::Kern);
                    let p = Kern(p.ptr());
                    cur_h += p.width();
                }
                TxtNode::Ligature(l) => {
                    /* 675: "Make node p look like a char_node and goto reswitch" */
                    let mut c = Char(LIG_TRICK);
                    c.set_character(l.char());
                    c.set_font(l.font());
                    *LLIST_link(LIG_TRICK) = *LLIST_link(l.ptr());
                    popt = Some(LIG_TRICK);
                    xtx_ligature_present = true;
                    continue;
                }
                TxtNode::Style(p) => {
                    /*1507: "Cases of hlist_out that arise in mixed direction text only" */
                    cur_h +=
                        p.width();
                    left_edge =
                        cur_h +
                            p.edge_dist();
                    cur_dir = p.lr();
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
    while Math(LR_ptr as usize).subtype_i32() != MathType::Before {
        match Math(LR_ptr as usize).subtype_i32() {
            MathType::Eq(_, MathMode::Left) | MathType::Eq(_, MathMode::Right) => {
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
unsafe fn vlist_out(this_box: &List) {
    let mut cur_g = Scaled::ZERO;
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
    synctex_vlist(this_box);

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
            confusion("vlistout");
        }
        /*653: "Output the non-char_node p" */
        match &mut TxtNode::from(p) {
            TxtNode::List(mut p) => {
                /*654: "Output a box in a vlist" */
                if p.list_ptr().opt().is_none() {
                    if upwards {
                        cur_v -= p.depth();
                    } else {
                        cur_v += p.height();
                    }
                    if p.is_vertical() {
                        synctex_void_vlist(&p, this_box);
                    } else {
                        synctex_void_hlist(&p, this_box);
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
                    if p.is_vertical() {
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
            TxtNode::Rule(r) => {
                rule_ht = r.height();
                rule_dp = r.depth();
                rule_wd = r.width();
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

                if rule_ht > Scaled::ZERO && rule_wd > Scaled::ZERO {
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
                    dvi_four(rule_ht.0);
                    dvi_four(rule_wd.0);
                    cur_h = left_edge
                }
            }
            TxtNode::WhatsIt(p) => match p {
                /*1403: "Output the whatsit node p in a vlist" */
                WhatsIt::Glyph(g) => {
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
                WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                    let save_h = dvi_h;
                    let save_v = dvi_v;
                    cur_v = cur_v + p.height();
                    pic_out(&p);
                    dvi_h = save_h;
                    dvi_v = save_v;
                    cur_v = save_v + p.depth();
                    cur_h = left_edge;
                }
                WhatsIt::PdfSavePos(_) => {
                    pdf_last_x_pos = cur_h + cur_h_offset;
                    pdf_last_y_pos = cur_page_height - cur_v - cur_v_offset
                }
                _ => out_what(&mut cur_input, p),
            },
            TxtNode::Glue(p) => {
                /*656: "Move down or output leaders" */
                let g = GlueSpec(p.glue_ptr() as usize);
                rule_ht = g.size() - cur_g;

                if g_sign != GlueSign::Normal {
                    if g_sign == GlueSign::Stretching {
                        if g.stretch_order() == g_order {
                            cur_glue += g.stretch().0 as f64;
                            cur_g = tex_round(
                                (this_box.glue_set() * cur_glue)
                                    .min(1_000_000_000.)
                                    .max(-1_000_000_000.),
                            )
                        }
                    } else if g.shrink_order() == g_order {
                        cur_glue -= g.shrink().0 as f64;
                        cur_g = tex_round(
                            (this_box.glue_set() * cur_glue)
                                .min(1_000_000_000.)
                                .max(-1_000_000_000.),
                        )
                    }
                }

                rule_ht += cur_g;

                if p.param() >= A_LEADERS {
                    /*657: "Output leaders in a vlist, goto fin_rule if a rule
                     * or next_p if done" */
                    let leader_box = p.leader_ptr() as usize; /* "compensate for floating-point rounding" */

                    match &mut TxtNode::from(leader_box) {
                        TxtNode::Rule(r) => {
                            rule_wd = r.width();
                            rule_dp = Scaled::ZERO;
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

                            if rule_ht > Scaled::ZERO && rule_wd > Scaled::ZERO {
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
                                dvi_four(rule_ht.0);
                                dvi_four(rule_wd.0);
                                cur_h = left_edge;
                            }
                            popt = llist_link(p.ptr());
                            continue;
                        }
                        TxtNode::List(lb) => {
                            let leader_ht = lb.height() + lb.depth();
                            if leader_ht > Scaled::ZERO && rule_ht > Scaled::ZERO {
                                rule_ht += Scaled(10);
                                let edge = cur_v + rule_ht;
                                let mut lx = Scaled::ZERO;
                                /*658: "Let cur_v be the position of the first box,
                                 * and set leader_ht + lx to the spacing between
                                 * corresponding parts of boxes" */
                                if p.param() == A_LEADERS {
                                    let save_v = cur_v;
                                    cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);
                                    if cur_v < save_v {
                                        cur_v = cur_v + leader_ht
                                    }
                                } else {
                                    let lq = rule_ht / leader_ht;
                                    let lr = rule_ht % leader_ht;
                                    if p.param() == C_LEADERS {
                                        cur_v = cur_v + lr / 2;
                                    } else {
                                        lx = lr / (lq + 1);
                                        cur_v = cur_v + (lr - lx * (lq - 1)) / 2;
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

                                    if lb.is_vertical() {
                                        vlist_out(lb);
                                    } else {
                                        hlist_out(lb);
                                    }

                                    doing_leaders = outer_doing_leaders;
                                    dvi_v = save_v;
                                    dvi_h = save_h;
                                    cur_h = left_edge;
                                    cur_v = save_v - lb.height() + leader_ht + lx
                                }
                                cur_v = edge - Scaled(10);
                                popt = llist_link(p.ptr());
                                continue;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                if upwards {
                    cur_v -= rule_ht
                } else {
                    cur_v += rule_ht
                }
            }
            TxtNode::Kern(k) => {
                if upwards {
                    cur_v -= k.width();
                } else {
                    cur_v += k.width();
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
    this_box: &List,
    tmp_ptr: usize,
    mut t: Option<usize>,
    cur_g: &mut Scaled,
    cur_glue: &mut f64,
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
                    let chr = Char(p);
                    let f = chr.font() as usize;
                    let c = chr.character();
                    cur_h += Scaled(
                        FONT_INFO[(WIDTH_BASE[f]
                            + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize]
                                .b16
                                .s3 as i32) as usize]
                            .b32
                            .s1,
                    );
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
                match TxtNode::from(p) {
                    TxtNode::List(p) => {
                        rule_wd = p.width();
                    }
                    TxtNode::Rule(p) => {
                        rule_wd = p.width();
                    }
                    TxtNode::Kern(p) => {
                        rule_wd = p.width();
                    }
                    TxtNode::WhatsIt(p) => match p {
                        WhatsIt::NativeWord(p) => {
                            rule_wd = p.width();
                        }
                        WhatsIt::Glyph(p) => {
                            rule_wd = p.width();
                        }
                        WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                            rule_wd = p.width();
                        }
                        _ => add_rule = false,
                    },
                    TxtNode::Glue(mut p) => {
                        /*1486: "Handle a glue node for mixed direction typesetting" */
                        let mut g = GlueSpec(p.glue_ptr() as usize); /* "will never match" */
                        rule_wd = g.size() - *cur_g; /* = mem[char(tmp_ptr)] */

                        match g_sign {
                            GlueSign::Normal => {}
                            GlueSign::Stretching => {
                                if g.stretch_order() == g_order {
                                    *cur_glue = *cur_glue + g.stretch().0 as f64;
                                    *cur_g = tex_round(
                                        (this_box.glue_set() * *cur_glue)
                                            .min(1_000_000_000.)
                                            .max(-1_000_000_000.),
                                    );
                                }
                            }
                            GlueSign::Shrinking => {
                                if g.shrink_order() == g_order {
                                    *cur_glue = *cur_glue - g.shrink().0 as f64;
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
                            if p.param() < A_LEADERS {
                                set_NODE_type(p.ptr(), TextNode::Kern);
                                let mut k = Kern(p.ptr());
                                k.set_width(rule_wd);
                            } else {
                                let mut g = GlueSpec(get_node(GLUE_SPEC_SIZE));
                                g.set_stretch_order(GlueOrder::Incorrect)
                                    .set_shrink_order(GlueOrder::Incorrect)
                                    .set_size(rule_wd)
                                    .set_stretch(Scaled::ZERO)
                                    .set_shrink(Scaled::ZERO);
                                p.set_glue_ptr(g.ptr() as i32);
                            }
                        }
                    }
                    TxtNode::Ligature(tmp_ptr) => {
                        flush_node_list(tmp_ptr.lig_ptr().opt());
                        let mut p = Ligature(get_avail());
                        p.set_char(tmp_ptr.char())
                            .set_font(tmp_ptr.font())
                            .set_lig_ptr(tmp_ptr.lig_ptr());
                        *LLIST_link(p.ptr()) = q;
                        popt = Some(p.ptr());
                        tmp_ptr.free();
                        continue;
                    }
                    TxtNode::Math(mut p) => {
                        /*1516: "Math nodes in an inner reflected segment are
                         * modified, those at the outer level are changed into
                         * kern nodes." */
                        rule_wd = p.width();

                        let (be, mode) = p.subtype().equ();
                        if be == BE::End {
                            if Math(LR_ptr as usize).subtype_i32() != MathType::Eq(BE::End, mode) {
                                set_NODE_type(p.ptr(), TextNode::Kern);
                                LR_problems += 1;
                            } else {
                                let tmp_ptr = LR_ptr as usize;
                                LR_ptr = *LLIST_link(tmp_ptr);
                                *LLIST_link(tmp_ptr) = avail.tex_int();
                                avail = Some(tmp_ptr);

                                if n > MIN_HALFWORD {
                                    n -= 1;
                                    p.set_subtype(match p.subtype() {
                                        MathType::After => MathType::Before,
                                        MathType::Eq(BE::End, mode) => {
                                            MathType::Eq(BE::Begin, mode)
                                        }
                                        _ => unreachable!(),
                                    });
                                } else {
                                    if m > MIN_HALFWORD {
                                        set_NODE_type(p.ptr(), TextNode::Kern);
                                        m -= 1
                                    } else {
                                        /*1517: "Finish the reverse hlist segment and goto done" */
                                        p.free(); /* end GLUE_NODE case */
                                        let mut t = Edge(t.unwrap());
                                        *LLIST_link(t.ptr()) = q;
                                        t.set_width(rule_wd).set_edge_dist(-cur_h - rule_wd);
                                        break 's_58;
                                    }
                                }
                            }
                        } else {
                            let mut tmp_ptr = Math(get_avail());
                            tmp_ptr.set_subtype_i32(MathType::Eq(BE::End, mode));
                            *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
                            LR_ptr = Some(tmp_ptr.ptr()).tex_int();
                            if n > MIN_HALFWORD || p.dir() != cur_dir {
                                n += 1;
                                p.set_subtype(match p.subtype() {
                                    MathType::Before => MathType::After,
                                    MathType::Eq(BE::Begin, mode) => MathType::Eq(BE::End, mode),
                                    _ => unreachable!(),
                                });
                            } else {
                                set_NODE_type(p.ptr(), TextNode::Kern);
                                m += 1;
                            }
                        }
                    }
                    TxtNode::Style(_) => confusion("LR2"),
                    _ => add_rule = false,
                }

                if add_rule {
                    cur_h += rule_wd;
                }

                // next_p
                *LLIST_link(p) = l.tex_int();
                let mut pp = Some(p);
                if let TxtNode::Kern(k) = TxtNode::from(p) {
                    if rule_wd == Scaled::ZERO || l.is_none() {
                        k.free();
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
        popt = Some(new_math(Scaled::ZERO, Math(LR_ptr as usize).subtype_i32()));
        LR_problems += 10000i32
    }
    l.tex_int()
}

/*1506: Create a new edge node of subtype `s` and width `w` */
pub(crate) unsafe fn new_edge(s: LR, w: Scaled) -> usize {
    let mut p = Edge(get_node(EDGE_NODE_SIZE));
    set_NODE_type(p.ptr(), EDGE_NODE);
    p.set_lr(s).set_width(w).set_edge_dist(Scaled::ZERO);
    p.ptr()
}

pub(crate) unsafe fn out_what(input: &mut input_state_t, p: &WhatsIt) {
    let mut j: i16;
    match p {
        WhatsIt::Open(p) => {
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
                cur_ext = maketexstring(".tex")
            }

            pack_file_name(cur_name, cur_area, cur_ext);

            let name = CString::new(name_of_file.as_str()).unwrap();
            write_file[j as usize] = ttstub_output_open(name.as_ptr(), 0);
            if write_file[j as usize].is_none() {
                abort!("cannot open output file \"{}\"", name_of_file);
            }

            write_open[j as usize] = true;

            if log_opened {
                let old_setting = selector;
                if *INTPAR(IntPar::tracing_online) <= 0 {
                    selector = Selector::LOG_ONLY
                } else {
                    selector = Selector::TERM_AND_LOG
                }
                print_nl_cstr("\\openout");
                print_int(j as i32);
                print_cstr(" = `");
                print_file_name(cur_name, cur_area, cur_ext);
                print_cstr("\'.");
                print_nl_cstr("");
                print_ln();
                selector = old_setting
            }
        }
        WhatsIt::Write(p) => {
            if doing_leaders {
                return;
            }
            write_out(input, &p);
            return;
        }
        WhatsIt::Close(p) => {
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
        WhatsIt::Special(p) => special_out(&p),
        WhatsIt::Language(_) => {}
        _ => confusion("ext4"),
    };
}

unsafe fn dvi_native_font_def(f: internal_font_number) {
    dvi_out(DEFINE_NATIVE_FONT as _);
    dvi_four(f as i32 - 1);
    for &i in &make_font_def(f) {
        dvi_out(i);
    }
}

unsafe fn dvi_font_def(f: internal_font_number) {
    if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
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
        dvi_four(FONT_SIZE[f].0);
        dvi_four(FONT_DSIZE[f].0);
        dvi_out(length(FONT_AREA[f]) as u8);
        let l = PoolString::from(FONT_NAME[f])
            .as_slice()
            .iter()
            .position(|&x| x == ':' as u16)
            .unwrap_or_else(|| length(FONT_NAME[f]) as usize);
        dvi_out(l as u8);
        for k in PoolString::from(FONT_AREA[f]).as_slice() {
            dvi_out(*k as u8);
        }
        for k in PoolString::from(FONT_NAME[f]).as_slice() {
            dvi_out(*k as u8);
        }
    };
}

unsafe fn movement(w: Scaled, mut o: u8) {
    let mut k: i32 = 0;
    let mut q = get_node(MOVEMENT_NODE_SIZE);
    MEM[q + 1].b32.s1 = w.0;
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
            if MEM[(p + 1) as usize].b32.s1 == w.0 {
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
    unsafe fn not_found(q: usize, o: u8, w: Scaled) {
        let mut w = w.0;
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

unsafe fn special_out(p: &Special) {
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
        MEM[p.tokens() as usize].b32.s1.opt(),
        None,
        pool_size - pool_ptr,
    );
    selector = old_setting;

    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
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

unsafe fn write_out(input: &mut input_state_t, p: &WriteFile) {
    let q = get_avail();
    MEM[q].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
    let mut r = get_avail();
    *LLIST_link(q) = Some(r).tex_int();
    MEM[r].b32.s0 = CS_TOKEN_FLAG + END_WRITE as i32;
    begin_token_list(input, q, Btl::Inserted);
    begin_token_list(input, p.tokens() as usize, Btl::WriteText);
    let q = get_avail();
    MEM[q].b32.s0 = LEFT_BRACE_TOKEN + '{' as i32;
    begin_token_list(input, q, Btl::Inserted);

    let old_mode = cur_list.mode;
    cur_list.mode = (false, ListMode::NoMode);
    let _q = scan_toks(input, write_loc, false, true);
    let tok = get_token(input).0;

    if tok != CS_TOKEN_FLAG + END_WRITE as i32 {
        /*1412:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Unbalanced write command");
        help!(
            "On this page there\'s a \\write with fewer real {\'s than }\'s.",
            "I can\'t handle that very well; good luck."
        );
        error();

        loop {
            let tok = get_token(input).0;
            if tok == CS_TOKEN_FLAG + END_WRITE as i32 {
                break;
            }
        }
    }

    cur_list.mode = old_mode;
    end_token_list(input);
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
        print_nl_cstr("");
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

        print_nl_cstr("runsystem(");
        let mut d = 0;
        while d <= cur_length() - 1 {
            print(str_pool[(str_start[(str_ptr - TOO_BIG_CHAR) as usize] + d) as usize] as i32);
            d += 1
        }
        print_cstr(")...");
        if !shell_escape_enabled {
            print_cstr("disabled");
            print_chr('.');
        } else {
            // Currently, -Z shell-escape is implemented but hidden (see
            // src/unstable_opts.rs). When this gets actually implemented,
            // uncomment the relevant parts in that file.

            print_cstr("enabled but not implemented yet!");
        }

        print_nl_cstr("");
        print_ln();
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
    }
    selector = old_setting;
}

unsafe fn pic_out(p: &Picture) {
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
    print_cstr("pdf:image ");
    print_cstr("matrix ");
    let matrix = p.transform_matrix();
    print_scaled(matrix[0]);
    print(' ' as i32);
    print_scaled(matrix[1]);
    print(' ' as i32);
    print_scaled(matrix[2]);
    print(' ' as i32);
    print_scaled(matrix[3]);
    print(' ' as i32);
    print_scaled(matrix[4]);
    print(' ' as i32);
    print_scaled(matrix[5]);
    print(' ' as i32);
    print_cstr("page ");
    print_int(p.page() as i32);
    print(' ' as i32);

    match p.pagebox() {
        1 => print_cstr("pagebox cropbox "),
        2 => print_cstr("pagebox mediabox "),
        3 => print_cstr("pagebox bleedbox "),
        5 => print_cstr("pagebox artbox "),
        4 => print_cstr("pagebox trimbox "),
        _ => {}
    }

    print('(' as i32);
    for i in p.path() {
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
        print_nl_cstr("No pages of output.");
        return;
    }

    if cur_s == -2 {
        /* This happens when the DVI gets too big; a message has already been printed */
        return;
    }

    dvi_out(POST); /* magic values: conversion ratio for sp */
    dvi_four(last_bop);
    last_bop = (dvi_offset + dvi_ptr - 5) as i32;
    dvi_four(25_400_000); /* magic values: conversion ratio for sp */
    dvi_four(Scaled(473628672).0); /* 7227.0 magic values: conversion ratio for sp */
    prepare_mag();
    dvi_four(*INTPAR(IntPar::mag));
    dvi_four(max_v.0);
    dvi_four(max_h.0);
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
        fatal_error("dvi length exceeds 0x7FFFFFFF");
    }

    if dvi_ptr > 0 {
        write_to_dvi(0, dvi_ptr as usize - 1);
    }

    let mut k = ttstub_output_close(dvi_file.take().unwrap()) as u8;

    if k == 0 {
        print_nl_cstr("Output written on ");
        print(output_file_name);
        print_cstr(" (");
        print_int(TOTAL_PAGES as i32);
        if TOTAL_PAGES != 1 {
            print_cstr(" pages");
        } else {
            print_cstr(" page");
        }
        print_cstr(", ");
        print_int((dvi_offset + dvi_ptr) as i32);
        print_cstr(" bytes).");
    } else {
        print_nl_cstr("Error ");
        print_int(k as i32);
        print_cstr(" (");
        print_c_str(
            std::ffi::CStr::from_ptr(strerror(k as i32))
                .to_str()
                .unwrap(),
        );
        print_cstr(") generating output;");
        print_nl_cstr("file ");
        print(output_file_name);
        print_cstr(" may not be valid.");
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
        fatal_error("dvi length exceeds 0x7FFFFFFF");
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
