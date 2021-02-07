#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::text_layout_engine::TextLayoutEngine;
use crate::xetex_output::{Cs, Esc, Roman};
use crate::{t_eprint, t_print, t_print_nl};
use std::cmp::Ordering;
use std::fmt;
use std::io::Write;
use std::ptr;

use super::xetex_ini::{input_state_t, EqtbWord, Selector};
use super::xetex_io::{name_of_input_file, u_open_in};
use crate::cmd::*;
use crate::core_memory::{mfree, xmalloc_array};
use crate::help;
use crate::node::*;
#[cfg(target_os = "macos")]
use crate::xetex_aatfont as aat;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow, pdf_error, Confuse};
use crate::xetex_ext::{
    apply_mapping, apply_tfm_font_mapping, get_encoding_mode_and_info, get_font_char_range,
    get_glyph_bounds, get_native_char_height_depth, get_native_char_sidebearings, getnativechardp,
    getnativecharht, getnativecharic, getnativecharwd, gr_font_get_named, gr_font_get_named_1,
    gr_get_font_name, linebreak_next, linebreak_start, map_char_to_glyph, map_glyph_to_index,
    ot_font_get, ot_font_get_1, ot_font_get_2, ot_font_get_3, Font, NativeFont, NativeFont::*,
};
use crate::xetex_ini::{
    _xeq_level_array, active_width, adjust_tail, after_token, align_ptr, align_state, arith_error,
    avail, bchar, best_height_plus_depth, breadth_max, cancel_boundary, cond_ptr, cur_align,
    cur_boundary, cur_box, cur_chr, cur_cmd, cur_cs, cur_dir, cur_group, cur_head, cur_if,
    cur_input, cur_l, cur_lang, cur_level, cur_list, cur_loop, cur_mark, cur_name, cur_order,
    cur_pre_head, cur_pre_tail, cur_ptr, cur_q, cur_r, cur_span, cur_tail, cur_tok, dead_cycles,
    def_ref, deletions_allowed, depth_threshold, disc_ptr, error_count, error_line, expand_depth,
    expand_depth_count, false_bchar, file_offset, first, first_count, fmem_ptr,
    font_in_short_display, force_eof, gave_char_warning_help, half_error_line, hash_extra,
    hash_high, hash_used, hi_mem_min, history, if_limit, if_line, ins_disc, insert_penalties,
    insert_src_special_auto, insert_src_special_every_par, insert_src_special_every_vbox,
    interaction, is_hyph, is_in_csname, job_name, last, last_badness, last_glue, last_kern,
    last_leftmost_char, last_node_type, last_penalty, last_rightmost_char, lft_hit, lig_stack,
    ligature_present, line, lo_mem_max, log_file, log_opened, long_help_seen, long_state, mag_set,
    main_f, main_h, main_i, main_j, main_k, main_s, max_buf_stack, max_print_line,
    max_reg_help_line, max_reg_num, mem_end, name_in_progress, name_of_font,
    no_new_control_sequence, open_parens, output_active, pack_begin_line, page_contents,
    page_so_far, page_tail, par_loc, par_token, pdf_last_x_pos, pdf_last_y_pos, pre_adjust_tail,
    prev_class, prim, prim_eqtb, prim_used, pseudo_files, pstack, read_file, read_open, rover,
    rt_hit, rust_stdout, sa_chain, sa_level, sa_root, scanner_status, selector, set_box_allowed,
    shown_mode, skip_line, space_class, stop_at_space, tally, term_offset, texmf_log_name,
    total_shrink, total_stretch, trick_buf, trick_count, use_err_help, used_tectonic_coda_tokens,
    warning_index, write_file, write_open, xtx_ligature_present, yhash, LR_problems, LR_ptr,
    BCHAR_LABEL, BUFFER, BUF_SIZE, EOF_SEEN, EQTB, EQTB_TOP, FONT_AREA, FONT_BC, FONT_BCHAR,
    FONT_DSIZE, FONT_EC, FONT_FALSE_BCHAR, FONT_GLUE, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_MAPPING,
    FONT_MAX, FONT_MEM_SIZE, FONT_NAME, FONT_PARAMS, FONT_PTR, FONT_SIZE,
    FULL_SOURCE_FILENAME_STACK, GRP_STACK, HYPHEN_CHAR, IF_STACK, INPUT_FILE, INPUT_PTR,
    INPUT_STACK, IN_OPEN, KERN_BASE, LIG_KERN_BASE, LINE_STACK, MAX_IN_OPEN, MAX_IN_STACK,
    MAX_NEST_STACK, MAX_PARAM_STACK, MAX_SAVE_STACK, MEM, NEST, NEST_PTR, NEST_SIZE, PARAM_BASE,
    PARAM_PTR, PARAM_SIZE, PARAM_STACK, SAVE_PTR, SAVE_SIZE, SAVE_STACK, SKEW_CHAR,
    SOURCE_FILENAME_STACK, STACK_SIZE,
};
use crate::xetex_ini::{b16x4, memory_word, prefixed_command};
use crate::xetex_ini::{hash_offset, FONT_LETTER_SPACE};
use crate::xetex_io::{open_or_close_in, set_input_file_encoding};
use crate::xetex_layout_interface::*;
use crate::xetex_linebreak::line_break;
use crate::xetex_math::{
    after_math, append_choices, build_choices, fin_mlist, flush_math, init_math, math_ac,
    math_fraction, math_left_right, math_limit_switch, math_radical, resume_after_display,
    start_eq_no, sub_sup,
};
use crate::xetex_output::{print_chr, print_esc_cstr, print_ln, SaNum};
use crate::xetex_pagebuilder::build_page;
use crate::xetex_pic::{count_pdf_file_pages, load_picture};
use crate::xetex_scaledmath::{
    mult_and_add, round_xn_over_d, tex_round, x_over_n, xn_over_d, Scaled,
};
use crate::xetex_shipout::{finalize_dvi_file, new_edge, out_what, ship_out};
use crate::xetex_stringpool::{
    append_str, init_str_ptr, make_string, max_strings, pool_ptr, pool_size, search_string,
    slow_make_string, str_eq_buf, str_pool, str_ptr, str_start, PoolString, EMPTY_STRING,
    TOO_BIG_CHAR,
};
use crate::xetex_synctex::{synctex_start_input, synctex_terminate};
use crate::xetex_texmfmp::{
    getmd5sum, gettexstring, is_new_source, make_src_special, maketexstring, remember_source_info,
};
use crate::xetex_xetexd::*;
use bridge::{ttstub_issue_warning, ttstub_output_close};

use bridge::{OutputHandleWrapper, TTHistory, TTInputFormat};

pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type str_number = i32;
pub(crate) type internal_font_number = usize;
pub(crate) type font_index = i32;

fn IS_LC_HEX(c: i32) -> bool {
    (c >= ('0' as i32) && c <= ('9' as i32)) || (c >= ('a' as i32) && c <= ('f' as i32))
}

unsafe fn int_error(n: i32) {
    t_print!(" ({})", n);
    error();
}
pub(crate) unsafe fn badness(t: Scaled, s: Scaled) -> i32 {
    let t = t.0;
    let s = s.0;
    if t == 0 {
        return 0;
    }
    if s <= 0 {
        return INF_BAD;
    }
    let r;
    if t <= 7230584 {
        /* magic constant */
        r = (t * 297) / s
    } else if s >= 1663497 {
        /* magic constant */
        r = t / (s / 297)
    } else {
        r = t
    }
    if r > 1290 {
        /* magic constant */
        return INF_BAD;
    }
    (r * r * r + 0x20000) / 0x40000
}

pub(crate) struct TokenList(pub Option<usize>);
impl<'a> fmt::Display for TokenList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut popt = self.0;
        let mut match_chr = '#'; // character used in a `match`
        let mut n = b'0'; // the highest parameter number, as an ASCII digit
        while let Some(p) = popt {
            // Display token |p|, and |return|
            if unsafe { p < hi_mem_min as usize || p > mem_end as usize } {
                Esc("CLOBBERED").fmt(f)?;
                ".".fmt(f)?;
                return Ok(());
            }
            let info = unsafe { *LLIST_info(p as usize) };
            if info >= CS_TOKEN_FLAG {
                Cs(info - CS_TOKEN_FLAG).fmt(f)?;
            } else {
                let m = Cmd::from((info / MAX_CHAR_VAL) as u16);
                let c = info % MAX_CHAR_VAL;
                if info < 0 {
                    Esc("BAD").fmt(f)?;
                    ".".fmt(f)?;
                } else {
                    // Display the token `$(m,c)$`
                    /*306:*/
                    match m {
                        Cmd::LeftBrace
                        | Cmd::RightBrace
                        | Cmd::MathShift
                        | Cmd::TabMark
                        | Cmd::SupMark
                        | Cmd::SubMark
                        | Cmd::Spacer
                        | Cmd::Letter
                        | Cmd::OtherChar => std::char::from_u32(c as u32).unwrap().fmt(f)?,
                        Cmd::MacParam => {
                            let c = std::char::from_u32(c as u32).unwrap();
                            c.fmt(f)?;
                            c.fmt(f)?;
                        }
                        OUT_PARAM => {
                            match_chr.fmt(f)?;
                            if c <= 0x9 {
                                char::from((c as u8) + b'0').fmt(f)?;
                            } else {
                                '!'.fmt(f)?;
                                return Ok(());
                            }
                        }
                        MATCH => {
                            match_chr = std::char::from_u32(c as u32).unwrap();
                            match_chr.fmt(f)?;
                            n += 1;
                            char::from(n).fmt(f)?;
                            if n > b'9' {
                                return Ok(());
                            }
                        }
                        END_MATCH => {
                            if c == 0 {
                                "->".fmt(f)?;
                            }
                        }
                        _ => {
                            Esc("BAD").fmt(f)?;
                            ".".fmt(f)?;
                        }
                    }
                }
            }
            popt = unsafe { LLIST_link(p as usize).opt() };
        }
        if popt.is_some() {
            Esc("ETC").fmt(f)?;
            ".".fmt(f)?;
        }
        Ok(())
    }
}
/*:112*/
/*118:*/
pub(crate) unsafe fn show_token_list(mut popt: Option<usize>, q: Option<usize>, l: i32) {
    let mut match_chr = '#'; // character used in a `match`
    let mut n = b'0'; // the highest parameter number, as an ASCII digit
    tally = 0;
    while let Some(p) = popt {
        if tally >= l {
            break;
        }
        /*332:*/
        if let Some(q) = q {
            if p == q {
                first_count = tally;
                trick_count = tally + 1 + error_line - half_error_line;
                if trick_count < error_line {
                    trick_count = error_line
                }
            }
        }
        // Display token |p|, and |return|
        if p < hi_mem_min as usize || p > mem_end as usize {
            print_esc_cstr("CLOBBERED.");
            return;
        }
        let info = *LLIST_info(p as usize);
        if info >= CS_TOKEN_FLAG {
            t_print!("{}", Cs(info - CS_TOKEN_FLAG));
        } else {
            let m = Cmd::from((info / MAX_CHAR_VAL) as u16);
            let c = info % MAX_CHAR_VAL;
            if info < 0 {
                print_esc_cstr("BAD.");
            } else {
                // Display the token `$(m,c)$`
                /*306:*/
                match m {
                    Cmd::LeftBrace
                    | Cmd::RightBrace
                    | Cmd::MathShift
                    | Cmd::TabMark
                    | Cmd::SupMark
                    | Cmd::SubMark
                    | Cmd::Spacer
                    | Cmd::Letter
                    | Cmd::OtherChar => print_chr(std::char::from_u32(c as u32).unwrap()),
                    Cmd::MacParam => {
                        let c = std::char::from_u32(c as u32).unwrap();
                        print_chr(c);
                        print_chr(c);
                    }
                    OUT_PARAM => {
                        print_chr(match_chr);
                        if c <= 0x9 {
                            print_chr(char::from((c as u8) + b'0'));
                        } else {
                            print_chr('!');
                            return;
                        }
                    }
                    MATCH => {
                        match_chr = std::char::from_u32(c as u32).unwrap();
                        print_chr(match_chr);
                        n += 1;
                        print_chr(char::from(n));
                        if n > b'9' {
                            return;
                        }
                    }
                    END_MATCH => {
                        if c == 0 {
                            t_print!("->");
                        }
                    }
                    _ => print_esc_cstr("BAD."),
                }
            }
        }
        popt = LLIST_link(p as usize).opt();
    }
    if popt.is_some() {
        print_esc_cstr("ETC.");
    };
}
/// uses `scanner_status` to print a warning message
/// when a subfile has ended, and at certain other crucial times
pub(crate) unsafe fn runaway() {
    if scanner_status != ScannerStatus::Normal && scanner_status != ScannerStatus::Skipping {
        let p = match scanner_status {
            ScannerStatus::Defining => {
                t_print_nl!("Runaway definition");
                def_ref
            }
            ScannerStatus::Matching => {
                t_print_nl!("Runaway argument");
                TEMP_HEAD
            }
            ScannerStatus::Aligning => {
                t_print_nl!("Runaway preamble");
                HOLD_HEAD
            }
            ScannerStatus::Absorbing => {
                t_print_nl!("Runaway text");
                def_ref
            }
            _ => unreachable!(),
        };
        print_chr('?');
        print_ln();
        show_token_list(llist_link(p), None, error_line - 10);
    };
}
pub(crate) unsafe fn get_avail() -> usize {
    let p = if let Some(p) = avail {
        avail = llist_link(p);
        p
    } else if mem_end < MEM_TOP as i32 {
        mem_end += 1;
        mem_end as usize
    } else {
        hi_mem_min -= 1;
        if is_char_node(lo_mem_max.opt()) {
            runaway();
            overflow("main memory size", MEM_TOP + 1);
        }
        hi_mem_min as usize
    };
    *LLIST_link(p) = None.tex_int();
    p
}
pub(crate) unsafe fn flush_list(p: Option<usize>) {
    if let Some(p) = p {
        let mut r = p;
        let mut q;
        loop {
            q = r;

            if let Some(r_) = LLIST_link(r as usize).opt() {
                r = r_;
            } else {
                break;
            }
        }
        *LLIST_link(q) = avail.tex_int();
        avail = Some(p);
    };
}
pub(crate) unsafe fn get_node(s: i32) -> usize {
    'restart: loop {
        let mut p = rover;
        loop {
            /*131: */
            let mut q = p + MEM[p as usize].b32.s0;
            while MEM[q as usize].b32.s1 == MAX_HALFWORD {
                let t = MEM[(q + 1) as usize].b32.s1;
                if q == rover {
                    rover = t
                }
                MEM[(t + 1) as usize].b32.s0 = MEM[(q + 1) as usize].b32.s0;
                MEM[(MEM[(q + 1) as usize].b32.s0 + 1) as usize].b32.s1 = t;
                q = q + MEM[q as usize].b32.s0
            }
            let r = q - s;
            if r > p + 1 {
                /*132: */
                MEM[p as usize].b32.s0 = r - p;
                rover = p;
                return found(r as usize, s as usize);
            }
            if r == p && MEM[(p + 1) as usize].b32.s1 != p {
                /*133: */
                rover = MEM[(p + 1) as usize].b32.s1;
                let t = MEM[(p + 1) as usize].b32.s0;
                MEM[(rover + 1) as usize].b32.s0 = t;
                MEM[(t + 1) as usize].b32.s1 = rover;
                return found(r as usize, s as usize);
            }
            MEM[p as usize].b32.s0 = q - p;
            p = MEM[(p + 1) as usize].b32.s1;
            if p == rover {
                break;
            }
        }
        if s == 0x40000000 {
            return MAX_HALFWORD as usize;
        }
        if lo_mem_max + 2 < hi_mem_min && lo_mem_max + 2 <= MAX_HALFWORD {
            /*130: */
            let mut t = if hi_mem_min - lo_mem_max >= 1998 {
                lo_mem_max + 1000
            } else {
                lo_mem_max + 1 + (hi_mem_min - lo_mem_max) / 2
            };
            p = MEM[(rover + 1) as usize].b32.s0;
            let q = lo_mem_max;
            MEM[(p + 1) as usize].b32.s1 = q;
            MEM[(rover + 1) as usize].b32.s0 = q;
            if t > MAX_HALFWORD {
                t = MAX_HALFWORD
            }
            MEM[(q + 1) as usize].b32.s1 = rover;
            MEM[(q + 1) as usize].b32.s0 = p;
            MEM[q as usize].b32.s1 = MAX_HALFWORD;
            MEM[q as usize].b32.s0 = t - lo_mem_max;
            lo_mem_max = t;
            *LLIST_link(lo_mem_max as usize) = None.tex_int();
            MEM[lo_mem_max as usize].b32.s0 = None.tex_int();
            rover = q;
            continue 'restart;
        }
        break 'restart;
    }
    overflow("main memory size", MEM_TOP + 1);

    unsafe fn found(r: usize, s: usize) -> usize {
        *LLIST_link(r) = None.tex_int();
        if s >= MEDIUM_NODE_SIZE as usize {
            MEM[(r + s - 1)].b32.s0 = cur_input.synctex_tag;
            MEM[(r + s - 1)].b32.s1 = line
        }
        r
    }
}
pub(crate) unsafe fn free_node(p: usize, s: i32) {
    MEM[p].b32.s0 = s;
    *LLIST_link(p) = MAX_HALFWORD;
    let q = MEM[(rover + 1) as usize].b32.s0;
    MEM[p + 1].b32.s0 = q;
    MEM[p + 1].b32.s1 = rover;
    MEM[(rover + 1) as usize].b32.s0 = p as i32;
    MEM[(q + 1) as usize].b32.s1 = p as i32;
}
pub(crate) unsafe fn new_null_box() -> usize {
    let mut p = List::from(get_node(BOX_NODE_SIZE));
    p.set_horizontal();
    p.set_lr_mode(LRMode::Normal)
        .set_width(Scaled::ZERO)
        .set_depth(Scaled::ZERO)
        .set_height(Scaled::ZERO);
    p.set_shift_amount(Scaled::ZERO)
        .set_list_ptr(None.tex_int())
        .set_glue_sign(GlueSign::Normal)
        .set_glue_order(GlueOrder::Normal)
        .set_glue_set(0.);
    p.ptr()
}
pub(crate) unsafe fn new_rule() -> Rule {
    let mut p = Rule::from(get_node(RULE_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Rule);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_width(NULL_FLAG)
        .set_depth(NULL_FLAG)
        .set_height(NULL_FLAG);
    p
}
pub(crate) unsafe fn new_ligature(f: internal_font_number, c: u16, q: Option<usize>) -> usize {
    let mut p = Ligature(get_node(SMALL_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Ligature);
    p.set_font(f as u16)
        .set_char(c)
        .set_lig_ptr(q)
        .set_hits(false, false);
    p.ptr()
}
pub(crate) unsafe fn new_lig_item(c: u16) -> usize {
    let p = get_node(SMALL_NODE_SIZE);
    MEM[p].b16.s0 = c;
    MEM[p + 1].b32.s1 = None.tex_int();
    p
}
pub(crate) unsafe fn new_disc() -> usize {
    let mut p = Discretionary::new_node();
    p.set_replace_count(0)
        .set_pre_break(None.tex_int())
        .set_post_break(None.tex_int());
    p.ptr()
}
pub(crate) unsafe fn copy_native_glyph_info(src: &NativeWord, dest: &mut NativeWord) {
    if !src.glyph_info_ptr().is_null() {
        let glyph_count = src.glyph_count() as i32;
        let bytesize =
            glyph_count as usize * (std::mem::size_of::<FixedPoint>() + std::mem::size_of::<u16>());
        dest.set_glyph_info_ptr(xmalloc_array::<u8>(bytesize) as *mut _);
        dest.set_glyph_count(glyph_count as u16);
        dest.locations_mut().copy_from_slice(src.locations());
        dest.glyph_ids_mut().copy_from_slice(src.glyph_ids());
    };
}
pub(crate) unsafe fn new_math(w: Scaled, s: MathType) -> Math {
    let mut p = Math(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Math);
    p.set_subtype(s).set_width(w);
    p
}
pub(crate) unsafe fn new_spec(other: &GlueSpec) -> GlueSpec {
    let mut q = GlueSpec(get_node(GLUE_SPEC_SIZE));
    MEM[q.ptr()] = MEM[other.ptr()];
    q.rc_set_none();
    q.set_size(other.size())
        .set_stretch(other.stretch())
        .set_shrink(other.shrink());
    q
}
pub(crate) unsafe fn new_param_glue(n: GluePar) -> Glue {
    let mut p = Glue(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Glue);
    p.set_param(n as u16 + 1).set_leader_ptr(None.tex_int());
    let q = GlueSpec(EQTB[GLUE_BASE + n as usize].val as usize);
    p.set_glue_ptr(q.ptr() as i32);
    q.rc_inc();
    p
}
pub(crate) unsafe fn new_glue(q: &GlueSpec) -> Glue {
    let mut p = Glue(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Glue);
    p.set_param(NORMAL as _)
        .set_leader_ptr(None.tex_int())
        .set_glue_ptr(Some(q.ptr()).tex_int());
    q.rc_inc();
    p
}
pub(crate) unsafe fn new_skip_param(n: GluePar) -> (Glue, GlueSpec) {
    let tmp = new_spec(&GlueSpec(
        EQTB[(GLUE_BASE as i32 + n as i32) as usize].val as usize,
    )); // 232
    let mut p = new_glue(&tmp);
    tmp.rc_set_none();
    p.set_param(n as u16 + 1);
    (p, tmp)
}
pub(crate) unsafe fn new_kern(w: Scaled) -> Kern {
    let mut p = Kern(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Kern);
    p.set_subtype(KernType::Normal).set_width(w);
    p
}
pub(crate) unsafe fn new_penalty(m: i32) -> Penalty {
    let mut p = Penalty(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Penalty);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_penalty(m);
    p
}
/*:165*/
pub(crate) unsafe fn prev_rightmost(s: Option<usize>, e: Option<usize>) -> Option<usize> {
    if let Some(mut p) = s {
        while llist_link(p) != e {
            if let Some(next) = llist_link(p) {
                p = next
            } else {
                return None;
            }
        }
        Some(p)
    } else {
        None
    }
}
pub(crate) unsafe fn short_display(mut popt: Option<usize>) {
    while let Some(mut p) = popt.filter(|&p| p != 0) {
        if is_char_node(Some(p)) {
            let p = Char(p);
            if p.ptr() <= mem_end as usize {
                if p.font() as usize != font_in_short_display {
                    if p.font() as i32 > FONT_MAX as i32 {
                        t_print!("* ");
                    } else {
                        /*279:*/
                        t_print!(
                            "{} ",
                            Esc(&PoolString::from(
                                yhash[FONT_ID_BASE + (p.font() as usize) - hash_offset].s1,
                            )
                            .to_string())
                        );
                    }
                    font_in_short_display = p.font() as usize
                }
                t_print!("{}", std::char::from_u32(p.character() as u32).unwrap());
            }
        } else {
            /*183:*/
            match TxtNode::from(p) {
                TxtNode::List(_)
                | TxtNode::Ins(_)
                | TxtNode::Mark(_)
                | TxtNode::Adjust(_)
                | TxtNode::Unset(_) => t_print!("[]"),
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::NativeWord(nw) => {
                        if nw.font() as usize != font_in_short_display {
                            t_print!(
                                "{} {}",
                                Esc(&PoolString::from(
                                    yhash[FONT_ID_BASE + (nw.font() as usize) - hash_offset].s1
                                )
                                .to_string()),
                                nw
                            );
                            font_in_short_display = nw.font() as usize;
                        } else {
                            t_print!("{}", nw);
                        }
                    }
                    _ => t_print!("[]"),
                },
                TxtNode::Rule(_) => t_print!("|"),
                TxtNode::Glue(_) => {
                    if MEM[p + 1].b32.s0 != 0 {
                        // TODO: strange (special case?)
                        t_print!(" ");
                    }
                }
                TxtNode::Math(m) => match m.subtype() {
                    MathType::Eq(_, MathMode::Left) | MathType::Eq(_, MathMode::Right) => {
                        t_print!("[]")
                    }
                    _ => t_print!("$"),
                },
                TxtNode::Ligature(l) => short_display(l.lig_ptr()),
                TxtNode::Disc(d) => {
                    short_display(d.pre_break().opt());
                    short_display(d.post_break().opt());
                    let mut n = d.replace_count() as i32;
                    while n > 0 {
                        if let Some(next) = llist_link(p) {
                            p = next
                        }
                        n -= 1;
                    }
                }
                _ => {}
            }
        }
        popt = llist_link(p);
    }
}
impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            if self.ptr() > mem_end as usize {
                Esc("CLOBBERED").fmt(f)?;
                '.'.fmt(f)?;
            } else {
                if self.font() as i32 > FONT_MAX as i32 {
                    "* ".fmt(f)?;
                } else {
                    /*279: */
                    Esc(&PoolString::from(
                        yhash[FONT_ID_BASE + self.font() as usize - hash_offset].s1,
                    )
                    .to_string())
                    .fmt(f)?;
                    ' '.fmt(f)?;
                }
                std::char::from_u32(self.character() as u32)
                    .unwrap()
                    .fmt(f)?;
            }
        }
        Ok(())
    }
}
pub(crate) unsafe fn print_mark(p: i32) {
    print_chr('{');
    if p < hi_mem_min || p > mem_end {
        print_esc_cstr("CLOBBERED.");
    } else {
        show_token_list(LLIST_link(p as usize).opt(), None, max_print_line - 10);
        // TODO
    }
    print_chr('}');
}
pub(crate) unsafe fn print_rule_dimen(d: Scaled) {
    if d == NULL_FLAG {
        t_print!("*");
    } else {
        t_print!("{}", d);
    };
}
pub(crate) struct GlueUnit(pub Scaled, pub GlueOrder, pub &'static str);
impl fmt::Display for GlueUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)?;
        match self.1 {
            GlueOrder::Incorrect => "foul",
            GlueOrder::Fil => "fil",
            GlueOrder::Fill => "fill",
            GlueOrder::Filll => "filll",
            GlueOrder::Normal => self.2,
        }
        .fmt(f)
    }
}

pub(crate) struct GlueSpecUnit(pub i32, pub &'static str);
impl fmt::Display for GlueSpecUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let p = self.0;
            let unit = self.1;
            if p < 0 || p >= lo_mem_max {
                '*'.fmt(f)?;
            } else {
                let p = GlueSpec(p as usize);
                p.size().fmt(f)?;
                if !unit.is_empty() {
                    unit.fmt(f)?;
                }
                if p.stretch() != Scaled::ZERO {
                    " plus ".fmt(f)?;
                    GlueUnit(p.stretch(), p.stretch_order(), unit).fmt(f)?;
                }
                if p.shrink() != Scaled::ZERO {
                    " minus ".fmt(f)?;
                    GlueUnit(p.shrink(), p.shrink_order(), unit).fmt(f)?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for MathChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = self.as_utf32();
        Esc("fam").fmt(f)?;
        self.family.fmt(f)?;
        ' '.fmt(f)?;
        std::char::from_u32(c).unwrap().fmt(f)
    }
}
pub(crate) unsafe fn print_delimiter(d: &Delimeter) {
    let a = (d.chr1.family as u32 * 256) + d.chr1.as_utf32();
    let a = ((a * 4096 + d.chr2.family as u32 * 256) as i64 + d.chr2.as_utf32() as i64) as i32;
    if a < 0 {
        t_print!("{}", a);
    } else {
        t_print!("\"{:X}", a);
    };
}
pub(crate) unsafe fn print_subsidiary_data(d: &MCell, c: u8) {
    if PoolString::current().len() as i32 >= depth_threshold {
        if d.typ != MathCell::Empty {
            t_print!(" []");
        }
    } else {
        str_pool[pool_ptr] = c as _;
        pool_ptr += 1;
        match d.typ {
            MathCell::MathChar => {
                print_ln();
                t_print!("{}{}", PoolString::current(), d.val.chr);
            }
            MathCell::SubBox => show_node_list(d.val.ptr.opt()),
            MathCell::SubMList => {
                if d.val.ptr.opt().is_none() {
                    print_ln();
                    t_print!("{}{{}}", PoolString::current());
                } else {
                    show_node_list(d.val.ptr.opt());
                }
            }
            _ => {}
        }
        pool_ptr -= 1
    };
}

impl fmt::Display for GluePar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GluePar::*;
        match self {
            line_skip => Esc("lineskip"),
            baseline_skip => Esc("baselineskip"),
            par_skip => Esc("parskip"),
            above_display_skip => Esc("abovedisplayskip"),
            below_display_skip => Esc("belowdisplayskip"),
            above_display_short_skip => Esc("abovedisplayshortskip"),
            below_display_short_skip => Esc("belowdisplayshortskip"),
            left_skip => Esc("leftskip"),
            right_skip => Esc("rightskip"),
            top_skip => Esc("topskip"),
            split_top_skip => Esc("splittopskip"),
            tab_skip => Esc("tabskip"),
            space_skip => Esc("spaceskip"),
            xspace_skip => Esc("xspaceskip"),
            par_fill_skip => Esc("parfillskip"),
            xetex_linebreak_skip => Esc("XeTeXlinebreakskip"),
            thin_mu_skip => Esc("thinmuskip"),
            med_mu_skip => Esc("medmuskip"),
            thick_mu_skip => Esc("thickmuskip"),
        }
        .fmt(f)
    }
}
pub(crate) unsafe fn show_node_list(mut popt: Option<usize>) {
    if PoolString::current().len() as i32 > depth_threshold {
        if popt.is_some() {
            t_print!(" []");
        }
        return;
    }
    let mut n = 0;
    while let Some(p) = popt.filter(|&p| p != 0) {
        print_ln();
        t_print!("{}", PoolString::current());
        if p > mem_end as usize {
            t_print!("Bad link, display aborted.");
            return;
        }
        n += 1;
        if n > breadth_max {
            t_print!("etc.");
            return;
        }
        let p = p as usize;
        match Node::from(p) {
            Node::Char(c) => t_print!("{}", c),
            Node::Text(n) => match n {
                TxtNode::List(p) => {
                    t_print!(
                        "{}({}+{})x{}",
                        if p.is_horizontal() {
                            Esc("hbox")
                        } else {
                            Esc("vbox")
                        },
                        p.height(),
                        p.depth(),
                        p.width()
                    );
                    let g = p.glue_set();
                    if g != 0. && p.glue_sign() != GlueSign::Normal {
                        t_print!(", glue set ");
                        if p.glue_sign() == GlueSign::Shrinking {
                            t_print!("- ");
                        }
                        if g.abs() > 20000. {
                            if g > 0. {
                                t_print!(">");
                            } else {
                                t_print!("< -");
                            }
                            t_print!("{}", GlueUnit(Scaled::from(20000), p.glue_order(), ""));
                        } else {
                            t_print!("{}", GlueUnit(tex_round(65536_f64 * g), p.glue_order(), ""));
                        }
                    }
                    if p.shift_amount() != Scaled::ZERO {
                        t_print!(", shifted {}", p.shift_amount());
                    }
                    /*1491:*/
                    if p.is_horizontal() && p.lr_mode() == LRMode::DList {
                        t_print!(", display");
                    }
                    str_pool[pool_ptr] = '.' as _;
                    pool_ptr += 1;
                    show_node_list(p.list_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Unset(p) => {
                    t_print!(
                        "{}box({}+{})x{}",
                        Esc("unset"),
                        p.height(),
                        p.depth(),
                        p.width()
                    );
                    /*193:*/
                    if p.columns() != 0 {
                        t_print!(" ({} columns)", p.columns() as i32 + 1);
                    }
                    if p.stretch() != Scaled::ZERO {
                        t_print!(", stretch {}", GlueUnit(p.stretch(), p.stretch_order(), ""));
                    }
                    if p.shrink() != Scaled::ZERO {
                        t_print!(", shrink {}", GlueUnit(p.shrink(), p.shrink_order(), ""));
                    }
                    str_pool[pool_ptr] = '.' as _;
                    pool_ptr += 1;
                    show_node_list(p.list_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Rule(p) => {
                    t_print!("{}(", Esc("rule"));
                    print_rule_dimen(p.height());
                    print_chr('+');
                    print_rule_dimen(p.depth());
                    t_print!(")x");
                    print_rule_dimen(p.width());
                }
                TxtNode::Ins(p_ins) => {
                    t_print!(
                        "{}{}, natural size {}; split({},{}); float cost {}",
                        Esc("insert"),
                        p_ins.box_reg() as i32,
                        p_ins.height(),
                        GlueSpecUnit(p_ins.split_top_ptr(), ""),
                        p_ins.depth(),
                        p_ins.float_cost()
                    );
                    str_pool[pool_ptr] = '.' as _;
                    pool_ptr += 1;
                    show_node_list(p_ins.ins_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::Open(p) => {
                        t_print!(
                            "{}={:#}",
                            p,
                            FileName {
                                name: p.name(),
                                area: p.area(),
                                ext: p.ext()
                            }
                        );
                    }
                    WhatsIt::Write(p) => {
                        t_print!("{}", p);
                        print_mark(p.tokens());
                    }
                    WhatsIt::Close(p) => t_print!("{}", p),
                    WhatsIt::Special(s) => {
                        print_esc_cstr("special");
                        print_mark(s.tokens());
                    }
                    WhatsIt::Language(l) => {
                        t_print!(
                            "{}{} (hyphenmin {},{})",
                            Esc("setlanguage"),
                            l.lang(),
                            l.lhm() as i32,
                            l.rhm() as i32
                        );
                    }
                    WhatsIt::NativeWord(nw) => {
                        t_print!(
                            "{} {}",
                            Esc(&PoolString::from(
                                yhash[FONT_ID_BASE + nw.font() as usize - hash_offset].s1,
                            )
                            .to_string()),
                            nw
                        );
                    }
                    WhatsIt::Glyph(g) => {
                        t_print!(
                            "{} glyph#{}",
                            Esc(&PoolString::from(
                                yhash[FONT_ID_BASE + g.font() as usize - hash_offset].s1,
                            )
                            .to_string()),
                            g.glyph() as i32
                        );
                    }
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        if p.is_pdf() {
                            print_esc_cstr("XeTeXpdffile");
                        } else {
                            print_esc_cstr("XeTeXpicfile");
                        }
                        t_print!("( {}\"", std::str::from_utf8(p.path()).unwrap());
                    }
                    WhatsIt::PdfSavePos(_) => print_esc_cstr("pdfsavepos"),
                },
                TxtNode::Glue(g) => {
                    if g.param() >= A_LEADERS {
                        /*198: */
                        print_esc_cstr(""); /*:244 */
                        if g.param() == C_LEADERS {
                            print_chr('c'); /*214:*/
                        } else if g.param() == X_LEADERS {
                            print_chr('x');
                        }
                        t_print!("leaders {}", GlueSpecUnit(g.glue_ptr(), ""));
                        str_pool[pool_ptr] = '.' as _;
                        pool_ptr += 1;
                        show_node_list(g.leader_ptr().opt());
                        pool_ptr -= 1
                    } else {
                        print_esc_cstr("glue");
                        if g.param() != NORMAL {
                            print_chr('(');
                            match g.param().cmp(&COND_MATH_GLUE) {
                                Ordering::Less => match GluePar::n(g.param() - 1) {
                                    Some(dimen) => t_print!("{}", dimen),
                                    None => t_print!("[unknown glue parameter!]"),
                                },
                                Ordering::Equal => print_esc_cstr("nonscript"),
                                Ordering::Greater => print_esc_cstr("mskip"),
                            }
                            print_chr(')');
                        }
                        if g.param() != COND_MATH_GLUE {
                            t_print!(
                                " {}",
                                if g.param() < COND_MATH_GLUE {
                                    GlueSpecUnit(g.glue_ptr(), "")
                                } else {
                                    GlueSpecUnit(g.glue_ptr(), "mu")
                                }
                            );
                        }
                    }
                }
                TxtNode::Kern(k) => {
                    if k.subtype() != KernType::Math {
                        print_esc_cstr("kern");
                        if k.subtype() != KernType::Normal {
                            t_print!(" ");
                        }
                        t_print!("{}", k.width());
                        if k.subtype() == KernType::AccKern {
                            t_print!(" (for accent)");
                        } else if k.subtype() == KernType::SpaceAdjustment {
                            t_print!(" (space adjustment)");
                        }
                    } else {
                        t_print!("{}{}mu", Esc("mkern"), k.width());
                    }
                }
                TxtNode::MarginKern(m) => {
                    if MEM[m.ptr()].b16.s0 == 0 {
                        t_print!("{}{} (left margin)", Esc("kern"), m.width());
                    } else {
                        t_print!("{}{} (right margin)", Esc("kern"), m.width());
                    }
                }
                TxtNode::Math(p) => match p.subtype() {
                    MathType::Eq(be, mode) => {
                        match be {
                            BE::Begin => print_esc_cstr("begin"),
                            BE::End => print_esc_cstr("end"),
                        }
                        match mode {
                            MathMode::Right => print_chr('R'),
                            MathMode::Left => print_chr('L'),
                            MathMode::Middle => print_chr('M'),
                        }
                    }
                    _ => {
                        print_esc_cstr("math");
                        match p.subtype() {
                            MathType::Before => t_print!("on"),
                            MathType::After => t_print!("off"),
                            _ => unreachable!(),
                        }
                        if p.width() != Scaled::ZERO {
                            t_print!(", surrounded {}", p.width());
                        }
                    }
                },
                TxtNode::Ligature(l) => {
                    t_print!("{}", l.as_char());
                    t_print!(" (ligature ");
                    if l.left_hit() {
                        print_chr('|');
                    }
                    font_in_short_display = l.font() as usize;
                    short_display(l.lig_ptr());
                    if l.right_hit() {
                        print_chr('|');
                    }
                    print_chr(')');
                }
                TxtNode::Penalty(p) => {
                    t_print!("{}{}", Esc("penalty "), p.penalty());
                }
                TxtNode::Disc(d) => {
                    t_print!("{}", Esc("discretionary"));
                    if d.replace_count() > 0 {
                        t_print!(" replacing {}", d.replace_count() as i32);
                    }
                    str_pool[pool_ptr] = '.' as _;
                    pool_ptr += 1;
                    show_node_list(d.pre_break().opt());
                    pool_ptr -= 1;
                    str_pool[pool_ptr] = '|' as _;
                    pool_ptr += 1;
                    show_node_list(d.post_break().opt());
                    pool_ptr -= 1
                }
                TxtNode::Mark(m) => {
                    if m.class() != 0 {
                        t_print!("{}{}", Esc("marks"), m.class());
                    } else {
                        t_print!("{}", Esc("mark"));
                    }
                    print_mark(m.mark_ptr());
                }
                TxtNode::Adjust(a) => {
                    print_esc_cstr("vadjust");
                    if a.subtype() != AdjustType::Post {
                        t_print!(" pre ");
                    }
                    str_pool[pool_ptr] = '.' as _;
                    pool_ptr += 1;
                    show_node_list(a.adj_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Style(_) => match MathStyle::from_cur(MEM[p].b16.s0 as i16) {
                    Some(m) => t_print!("{}", m),
                    None => t_print!("Unknown style!"),
                },
                TxtNode::Choice(c) => {
                    print_esc_cstr("mathchoice");
                    str_pool[pool_ptr] = 'D' as _;
                    pool_ptr += 1;
                    show_node_list(c.display());
                    pool_ptr -= 1;
                    str_pool[pool_ptr] = 'T' as _;
                    pool_ptr += 1;
                    show_node_list(c.text());
                    pool_ptr -= 1;
                    str_pool[pool_ptr] = 'S' as _;
                    pool_ptr += 1;
                    show_node_list(c.script());
                    pool_ptr -= 1;
                    str_pool[pool_ptr] = 's' as _;
                    pool_ptr += 1;
                    show_node_list(c.scriptscript());
                    pool_ptr -= 1
                }
            },
            Node::Math(n) => match n {
                MathNode::Ord
                | MathNode::Op
                | MathNode::Bin
                | MathNode::Rel
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct
                | MathNode::Inner
                | MathNode::Radical
                | MathNode::Over
                | MathNode::Under
                | MathNode::VCenter
                | MathNode::Accent
                | MathNode::Left
                | MathNode::Right => {
                    match n {
                        MathNode::Ord => print_esc_cstr("mathord"),
                        MathNode::Op => print_esc_cstr("mathop"),
                        MathNode::Bin => print_esc_cstr("mathbin"),
                        MathNode::Rel => print_esc_cstr("mathrel"),
                        MathNode::Open => print_esc_cstr("mathopen"),
                        MathNode::Close => print_esc_cstr("mathclose"),
                        MathNode::Punct => print_esc_cstr("mathpunct"),
                        MathNode::Inner => print_esc_cstr("mathinner"),
                        MathNode::Over => print_esc_cstr("overline"),
                        MathNode::Under => print_esc_cstr("underline"),
                        MathNode::VCenter => print_esc_cstr("vcenter"),
                        MathNode::Radical => {
                            let r = Radical::from(p);
                            print_esc_cstr("radical");
                            print_delimiter(r.delimeter());
                        }
                        MathNode::Accent => {
                            t_print!("{}{}", Esc("accent"), Accent::from(p).fourth().val.chr);
                        }
                        MathNode::Left => {
                            let l = LeftRight(p);
                            print_esc_cstr("left");
                            print_delimiter(l.delimeter());
                        }
                        MathNode::Right => {
                            let r = LeftRight(p);
                            if MEM[p].b16.s0 == NORMAL {
                                print_esc_cstr("right");
                            } else {
                                print_esc_cstr("middle");
                            }
                            print_delimiter(r.delimeter());
                        }
                        _ => {}
                    }
                    let bm = BaseMath(p);
                    if n != MathNode::Left && n != MathNode::Right {
                        match Limit::from(MEM[p].b16.s0) {
                            Limit::Limits => print_esc_cstr("limits"),
                            Limit::NoLimits => print_esc_cstr("nolimits"),
                            Limit::Normal => {}
                        }
                        print_subsidiary_data(bm.nucleus(), b'.');
                    }
                    print_subsidiary_data(bm.supscr(), b'^');
                    print_subsidiary_data(bm.subscr(), b'_');
                }
                MathNode::Fraction => {
                    let f = Fraction(p);
                    if f.thickness() == DEFAULT_CODE {
                        t_print!("{}, thickness = default", Esc("fraction"));
                    } else {
                        t_print!("{}, thickness {}", Esc("fraction"), f.thickness());
                    }
                    let ld = f.left_delimeter();
                    if !ld.is_empty() {
                        t_print!(", left-delimiter ");
                        print_delimiter(ld);
                    }
                    let rd = f.right_delimeter();
                    if !rd.is_empty() {
                        t_print!(", right-delimiter ");
                        print_delimiter(rd);
                    }
                    print_subsidiary_data(f.numerator(), b'\\');
                    print_subsidiary_data(f.denumerator(), b'/');
                }
            },
            Node::Unknown(_) => t_print!("Unknown node type!"),
        }
        popt = llist_link(p);
    }
}
pub(crate) unsafe fn show_box(p: Option<usize>) {
    depth_threshold = get_int_par(IntPar::show_box_depth);
    breadth_max = get_int_par(IntPar::show_box_breadth);
    if breadth_max <= 0 {
        breadth_max = 5;
    }
    if (pool_ptr as i32) + depth_threshold >= (pool_size as i32) {
        depth_threshold = (pool_size as i32) - (pool_ptr as i32) - 1;
    }
    show_node_list(p);
    print_ln();
}
/*pub(crate) unsafe fn short_display_n(p: Option<usize>, m: i32) {
    breadth_max = m;
    depth_threshold = (pool_size as i32) - (pool_ptr as i32) - 1;
    show_node_list(p);
}*/
/// is called when
/// a pointer to a token list's reference count is being removed. This means
/// that the token list should disappear if the reference count was `null`,
/// otherwise the count should be decreased by one.
pub(crate) unsafe fn delete_token_ref(p: usize) {
    // `p` points to the reference count of a token list that is losing one reference
    if token_ref_count(p).opt().is_none() {
        flush_list(Some(p));
    } else {
        *token_ref_count(p) -= 1;
    };
}
/// is called when a pointer to a glue specification is being withdrawn
pub(crate) unsafe fn delete_glue_ref(p: usize) {
    let g = GlueSpec(p);
    if g.rc_is_none() {
        g.free();
    } else {
        g.rc_dec();
    };
}

/// Erase list of nodes starting at `p`
///
/// Now we are ready to delete any node list, recursively.
/// In practice, the nodes deleted are usually charnodes (about 2/3 of the time),
/// and they are glue nodes in about half of the remaining cases.
pub(crate) unsafe fn flush_node_list(mut popt: Option<usize>) {
    while let Some(p) = popt {
        let q = *LLIST_link(p);
        match Node::from(p) {
            Node::Char(_) => {
                *LLIST_link(p) = avail.tex_int();
                avail = Some(p);
            }
            Node::Text(n) => match n {
                TxtNode::List(b) => {
                    flush_node_list(b.list_ptr().opt());
                    b.free();
                }
                TxtNode::Unset(u) => {
                    flush_node_list(u.list_ptr().opt());
                    u.free();
                }
                TxtNode::Rule(r) => {
                    r.free();
                }
                TxtNode::Ins(i) => {
                    flush_node_list(i.ins_ptr().opt());
                    delete_glue_ref(i.split_top_ptr() as usize);
                    i.free();
                }
                TxtNode::WhatsIt(p) => match p {
                    // Wipe out the whatsit node `p` and goto `'done`
                    WhatsIt::Open(o) => o.free(),
                    WhatsIt::Write(f) => {
                        delete_token_ref(f.tokens() as usize);
                        f.free();
                    }
                    WhatsIt::Special(s) => {
                        delete_token_ref(s.tokens() as usize);
                        s.free();
                    }
                    WhatsIt::Close(c) => c.free(),
                    WhatsIt::Language(l) => l.free(),
                    WhatsIt::NativeWord(mut nw) => {
                        if let Some(ptr) = nw.glyph_info_ptr().as_mut() {
                            nw.set_glyph_info_ptr(mfree(ptr));
                            nw.set_glyph_count(0);
                        }
                        nw.free();
                    }
                    WhatsIt::Glyph(g) => g.free(),
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        p.free();
                    }
                    WhatsIt::PdfSavePos(p) => p.free(),
                },
                TxtNode::Glue(p) => {
                    let g = GlueSpec(p.glue_ptr() as usize);
                    if let Some(_rc) = g.rc().opt() {
                        g.rc_dec();
                    } else {
                        free_node(g.ptr(), 4);
                    }
                    if let Some(nd) = p.leader_ptr().opt() {
                        flush_node_list(Some(nd));
                    }
                    p.free();
                }
                TxtNode::Kern(k) => {
                    k.free();
                }
                TxtNode::Math(m) => {
                    m.free();
                }
                TxtNode::Penalty(p) => {
                    p.free();
                }
                TxtNode::MarginKern(m) => {
                    m.free();
                }
                TxtNode::Ligature(l) => {
                    flush_node_list(l.lig_ptr());
                    l.free();
                }
                TxtNode::Mark(m) => {
                    delete_token_ref(m.mark_ptr() as usize);
                    free_node(p, SMALL_NODE_SIZE);
                }
                TxtNode::Disc(d) => {
                    flush_node_list(d.pre_break().opt());
                    flush_node_list(d.post_break().opt());
                    d.free();
                }
                TxtNode::Adjust(a) => {
                    flush_node_list(a.adj_ptr().opt());
                    a.free();
                }
                // Cases of `flush_node_list` that arise in mlists only
                TxtNode::Style(_) => {
                    free_node(p, STYLE_NODE_SIZE);
                }
                TxtNode::Choice(c) => {
                    flush_node_list(c.display());
                    flush_node_list(c.text());
                    flush_node_list(c.script());
                    flush_node_list(c.scriptscript());
                    c.free();
                }
            },
            Node::Math(n) => match n {
                MathNode::Ord
                | MathNode::Op
                | MathNode::Bin
                | MathNode::Rel
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct
                | MathNode::Inner
                | MathNode::Radical
                | MathNode::Over
                | MathNode::Under
                | MathNode::VCenter
                | MathNode::Accent => {
                    if MEM[p + 1].b32.s1 >= MathCell::SubBox as _ {
                        flush_node_list(MEM[p + 1].b32.s0.opt());
                    }
                    if MEM[p + 2].b32.s1 >= MathCell::SubBox as _ {
                        flush_node_list(MEM[p + 2].b32.s0.opt());
                    }
                    if MEM[p + 3].b32.s1 >= MathCell::SubBox as _ {
                        flush_node_list(MEM[p + 3].b32.s0.opt());
                    }
                    if MEM[p].b16.s1 == MathNode::Radical as u16 {
                        free_node(p, RADICAL_NOAD_SIZE);
                    } else if MEM[p].b16.s1 == MathNode::Accent as u16 {
                        free_node(p, ACCENT_NOAD_SIZE);
                    } else {
                        free_node(p, NOAD_SIZE);
                    }
                }
                MathNode::Left | MathNode::Right => {
                    free_node(p, NOAD_SIZE);
                }
                MathNode::Fraction => {
                    flush_node_list(MEM[p + 2].b32.s0.opt()); // numerator
                    flush_node_list(MEM[p + 3].b32.s0.opt()); // denumerator
                    free_node(p, FRACTION_NOAD_SIZE);
                }
            },
            Node::Unknown(_) => confusion("flushing"), // this can't happen flushing
        }
        popt = q.opt()
    }
}
pub(crate) unsafe fn copy_node_list(mut popt: Option<usize>) -> i32 {
    let h = get_avail();
    let mut q = h;
    while let Some(p) = popt {
        let r;
        let mut words = 1;
        if is_char_node(Some(p)) {
            r = get_avail();
        } else {
            match &TxtNode::from(p) {
                TxtNode::List(p) => {
                    r = get_node(BOX_NODE_SIZE);
                    let mut r_box = List::from(r);
                    *SYNCTEX_tag(r, BOX_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE);
                    *SYNCTEX_line(r, BOX_NODE_SIZE) = *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE);
                    r_box
                        .set_glue_set(p.glue_set())
                        .set_glue_order(p.glue_order())
                        .set_glue_sign(p.glue_sign())
                        .set_list_ptr(copy_node_list(p.list_ptr().opt()));
                    words = 5_u8
                }
                TxtNode::Unset(p) => {
                    r = get_node(BOX_NODE_SIZE);
                    let mut r_unset = Unset::from(r);
                    *SYNCTEX_tag(r, BOX_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE);
                    *SYNCTEX_line(r, BOX_NODE_SIZE) = *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE);
                    r_unset
                        .set_stretch(p.stretch())
                        .set_stretch_order(p.stretch_order())
                        .set_shrink_order(p.shrink_order())
                        .set_list_ptr(copy_node_list(p.list_ptr().opt()));
                    words = 5_u8
                }
                TxtNode::Rule(_) => {
                    r = get_node(RULE_NODE_SIZE);
                    words = (RULE_NODE_SIZE - 1) as u8
                }
                TxtNode::Ins(p_ins) => {
                    r = get_node(INS_NODE_SIZE);
                    let mut r_ins = Insertion(r);
                    r_ins.set_split_top_ptr(p_ins.split_top_ptr());
                    GlueSpec(p_ins.split_top_ptr() as usize).rc_inc();
                    r_ins.set_ins_ptr(copy_node_list(p_ins.ins_ptr().opt()));
                    words = (INS_NODE_SIZE - 1) as u8
                }
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::Open(_) => {
                        r = get_node(OPEN_NODE_SIZE);
                        words = OPEN_NODE_SIZE as u8
                    }
                    WhatsIt::Write(p) => {
                        r = get_node(WRITE_NODE_SIZE);
                        MEM[p.tokens() as usize].b32.s0 += 1;
                        words = WRITE_NODE_SIZE as u8
                    }
                    WhatsIt::Special(p) => {
                        r = get_node(WRITE_NODE_SIZE);
                        MEM[p.tokens() as usize].b32.s0 += 1;
                        words = WRITE_NODE_SIZE as u8
                    }
                    WhatsIt::Close(_) | WhatsIt::Language(_) => {
                        r = get_node(SMALL_NODE_SIZE);
                        words = SMALL_NODE_SIZE as u8
                    }
                    WhatsIt::NativeWord(p) => {
                        words = p.size() as u8;
                        let mut r_nw = NativeWord::from(get_node(words as i32));
                        r = r_nw.ptr();
                        MEM[r..r + (words as usize)]
                            .copy_from_slice(&MEM[p.ptr()..p.ptr() + (words as usize)]);
                        words = 0;
                        r_nw.set_glyph_info_ptr(ptr::null_mut());
                        r_nw.set_glyph_count(0);
                        copy_native_glyph_info(&p, &mut r_nw);
                    }
                    WhatsIt::Glyph(_) => {
                        r = get_node(GLYPH_NODE_SIZE);
                        words = GLYPH_NODE_SIZE as u8
                    }
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        words = Picture::total_size(p.path_len()) as u8;
                        r = get_node(words as i32);
                    }
                    WhatsIt::PdfSavePos(_) => r = get_node(SMALL_NODE_SIZE),
                    //_ => confusion("ext2"),
                },
                TxtNode::Glue(p) => {
                    r = get_node(MEDIUM_NODE_SIZE);
                    let mut r_glue = Glue(r);
                    GlueSpec(p.glue_ptr() as usize).rc_inc();
                    *SYNCTEX_tag(r, MEDIUM_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), MEDIUM_NODE_SIZE);
                    *SYNCTEX_line(r, MEDIUM_NODE_SIZE) = *SYNCTEX_line(p.ptr(), MEDIUM_NODE_SIZE);
                    r_glue.set_glue_ptr(p.glue_ptr());
                    r_glue.set_leader_ptr(copy_node_list(p.leader_ptr().opt()));
                }
                TxtNode::Kern(_) | TxtNode::Math(_) | TxtNode::Penalty(_) => {
                    r = get_node(MEDIUM_NODE_SIZE);
                    words = MEDIUM_NODE_SIZE as u8
                }
                TxtNode::MarginKern(_) => {
                    r = get_node(MARGIN_KERN_NODE_SIZE);
                    words = MARGIN_KERN_NODE_SIZE as u8
                }
                TxtNode::Ligature(p) => {
                    r = get_node(SMALL_NODE_SIZE);
                    let mut r_lig = Ligature(r);
                    r_lig.set_char(p.char());
                    r_lig.set_font(p.font());
                    r_lig.set_lig_ptr(copy_node_list(p.lig_ptr()).opt());
                }
                TxtNode::Disc(p) => {
                    r = get_node(SMALL_NODE_SIZE);
                    let mut r_disc = Discretionary(r);
                    r_disc.set_pre_break(copy_node_list(p.pre_break().opt()));
                    r_disc.set_post_break(copy_node_list(p.post_break().opt()));
                }
                TxtNode::Mark(p) => {
                    r = get_node(SMALL_NODE_SIZE);
                    MarkClass(p.mark_ptr() as usize).rc_inc();
                    words = SMALL_NODE_SIZE as u8
                }
                TxtNode::Adjust(p) => {
                    r = get_node(SMALL_NODE_SIZE);
                    Adjust(r).set_adj_ptr(copy_node_list(p.adj_ptr().opt()));
                }
                _ => confusion("copying"),
            }
        }
        MEM[r..r + (words as usize)].copy_from_slice(&MEM[p..p + (words as usize)]);
        *LLIST_link(q) = r as i32;
        q = r;
        popt = llist_link(p);
    }
    *LLIST_link(q) = None.tex_int();
    let q = *LLIST_link(h);
    *LLIST_link(h) = avail.tex_int();
    avail = Some(h);
    q
}

pub(crate) unsafe fn print_mode(m: (bool, ListMode)) {
    t_print!(
        "{}",
        match m {
            (_, ListMode::NoMode) => "no mode",
            (false, ListMode::VMode) => "vertical mode",
            (false, ListMode::HMode) => "horizontal mode",
            (false, ListMode::MMode) => "display math mode",
            (true, ListMode::VMode) => "internal vertical mode",
            (true, ListMode::HMode) => "restricted horizontal mode",
            (true, ListMode::MMode) => "math mode",
        }
    );
}
pub(crate) unsafe fn print_in_mode(m: (bool, ListMode)) {
    t_print!(
        "\' in {}",
        match m {
            (_, ListMode::NoMode) => "no mode",
            (false, ListMode::VMode) => "vertical mode",
            (false, ListMode::HMode) => "horizontal mode",
            (false, ListMode::MMode) => "display math mode",
            (true, ListMode::VMode) => "internal vertical mode",
            (true, ListMode::HMode) => "restricted horizontal mode",
            (true, ListMode::MMode) => "math mode",
        }
    );
}
pub(crate) unsafe fn push_nest() {
    if NEST_PTR > MAX_NEST_STACK {
        MAX_NEST_STACK = NEST_PTR;
        if NEST_PTR == NEST_SIZE {
            overflow("semantic nest size", NEST_SIZE);
        }
    }
    NEST[NEST_PTR] = cur_list;
    NEST_PTR += 1;
    cur_list.head = get_avail();
    cur_list.tail = cur_list.head;
    cur_list.prev_graf = 0;
    cur_list.mode_line = line;
    cur_list.eTeX_aux = None;
}
pub(crate) unsafe fn pop_nest() {
    MEM[cur_list.head].b32.s1 = avail.tex_int();
    avail = Some(cur_list.head);
    NEST_PTR -= 1;
    cur_list = NEST[NEST_PTR];
}
pub(crate) unsafe fn show_activities() {
    NEST[NEST_PTR] = cur_list;
    t_print_nl!("");
    print_ln();
    for p in (0..=NEST_PTR as usize).rev() {
        let m = NEST[p].mode;
        let a = NEST[p].aux;
        t_print_nl!("### ");
        print_mode(m);
        t_print!(" entered at line {}", NEST[p].mode_line.abs());
        if m == (false, ListMode::HMode) && NEST[p].prev_graf != 0x830000 {
            t_print!(
                " (language{}:hyphenmin{},{})",
                (NEST[p].prev_graf as i64 % 65536) as i32,
                NEST[p].prev_graf / 0x400000,
                (NEST[p].prev_graf as i64 / 65536 % 64) as i32
            );
        }
        if NEST[p].mode_line < 0 {
            t_print!(" (\\output routine)");
        }
        if p == 0 {
            if PAGE_HEAD != page_tail {
                t_print_nl!("### current page:");
                if output_active {
                    t_print!(" (held over for next output)");
                }
                show_box(llist_link(PAGE_HEAD));
                if page_contents == PageContents::InsertsOnly
                    || page_contents == PageContents::BoxThere
                {
                    t_print_nl!("total height ");
                    print_totals();
                    t_print_nl!(" goal height {}", page_so_far[0]);
                    let mut r = PageInsertion(*LLIST_link(PAGE_INS_HEAD) as usize);
                    while r.ptr() != PAGE_INS_HEAD {
                        print_ln();
                        let t = r.box_reg() as i32;
                        t_print!(
                            "{}{} adds {}",
                            Esc("insert"),
                            t,
                            if get_count_reg(t as usize) == 1000 {
                                r.height()
                            } else {
                                x_over_n(r.height(), 1000).0 * get_count_reg(t as usize)
                            }
                        );
                        if r.subtype() == PageInsType::SplitUp {
                            let mut q = PAGE_HEAD;
                            let mut t = 0;
                            loop {
                                q = *LLIST_link(q as usize) as usize;
                                match Node::from(q as usize) {
                                    Node::Text(TxtNode::Ins(ins))
                                        if ins.box_reg() == r.box_reg() =>
                                    {
                                        t += 1;
                                    }
                                    _ => {}
                                }
                                if q == r.broken_ins() as usize {
                                    break;
                                }
                            }
                            t_print!(", #{} might split", t);
                        }
                        r = PageInsertion(*LLIST_link(r.ptr()) as usize);
                    }
                }
            }
            if llist_link(CONTRIB_HEAD).is_some() {
                t_print_nl!("### recent contributions:");
            }
        }
        show_box(MEM[NEST[p].head as usize].b32.s1.opt());
        match m.1 {
            ListMode::VMode => {
                if a.b32.s1 <= IGNORE_DEPTH {
                    t_print!("prevdepth ignored");
                } else {
                    t_print!("prevdepth {}", Scaled(a.b32.s1));
                }
                let pg = NEST[p].prev_graf;
                if pg != 0 {
                    if pg != 1 {
                        t_print!(", prevgraf {} lines", pg);
                    } else {
                        t_print!(", prevgraf {} line", pg);
                    }
                }
            }
            ListMode::HMode => {
                t_print_nl!("spacefactor {}", a.b32.s0);
                if !m.0 && a.b32.s1 > 0 {
                    t_print!(", current language {}", a.b32.s1);
                }
            }
            ListMode::MMode => {
                if let Some(o) = a.b32.s1.opt() {
                    t_print!("this will be denominator of:");
                    show_box(Some(o));
                }
            }
            ListMode::NoMode => {}
        }
    }
}
impl fmt::Display for IntPar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IntPar::*;
        match self {
            pretolerance => Esc("pretolerance").fmt(f),
            tolerance => Esc("tolerance").fmt(f),
            line_penalty => Esc("linepenalty").fmt(f),
            hyphen_penalty => Esc("hyphenpenalty").fmt(f),
            ex_hyphen_penalty => Esc("exhyphenpenalty").fmt(f),
            club_penalty => Esc("clubpenalty").fmt(f),
            widow_penalty => Esc("widowpenalty").fmt(f),
            display_widow_penalty => Esc("displaywidowpenalty").fmt(f),
            broken_penalty => Esc("brokenpenalty").fmt(f),
            bin_op_penalty => Esc("binoppenalty").fmt(f),
            rel_penalty => Esc("relpenalty").fmt(f),
            pre_display_penalty => Esc("predisplaypenalty").fmt(f),
            post_display_penalty => Esc("postdisplaypenalty").fmt(f),
            inter_line_penalty => Esc("interlinepenalty").fmt(f),
            double_hyphen_demerits => Esc("doublehyphendemerits").fmt(f),
            final_hyphen_demerits => Esc("finalhyphendemerits").fmt(f),
            adj_demerits => Esc("adjdemerits").fmt(f),
            mag => Esc("mag").fmt(f),
            delimiter_factor => Esc("delimiterfactor").fmt(f),
            looseness => Esc("looseness").fmt(f),
            time => Esc("time").fmt(f),
            day => Esc("day").fmt(f),
            month => Esc("month").fmt(f),
            year => Esc("year").fmt(f),
            show_box_breadth => Esc("showboxbreadth").fmt(f),
            show_box_depth => Esc("showboxdepth").fmt(f),
            hbadness => Esc("hbadness").fmt(f),
            vbadness => Esc("vbadness").fmt(f),
            pausing => Esc("pausing").fmt(f),
            tracing_online => Esc("tracingonline").fmt(f),
            tracing_macros => Esc("tracingmacros").fmt(f),
            tracing_stats => Esc("tracingstats").fmt(f),
            tracing_paragraphs => Esc("tracingparagraphs").fmt(f),
            tracing_pages => Esc("tracingpages").fmt(f),
            tracing_output => Esc("tracingoutput").fmt(f),
            tracing_lost_chars => Esc("tracinglostchars").fmt(f),
            tracing_commands => Esc("tracingcommands").fmt(f),
            tracing_restores => Esc("tracingrestores").fmt(f),
            uc_hyph => Esc("uchyph").fmt(f),
            output_penalty => Esc("outputpenalty").fmt(f),
            max_dead_cycles => Esc("maxdeadcycles").fmt(f),
            hang_after => Esc("hangafter").fmt(f),
            floating_penalty => Esc("floatingpenalty").fmt(f),
            global_defs => Esc("globaldefs").fmt(f),
            cur_fam => Esc("fam").fmt(f),
            escape_char => Esc("escapechar").fmt(f),
            default_hyphen_char => Esc("defaulthyphenchar").fmt(f),
            default_skew_char => Esc("defaultskewchar").fmt(f),
            end_line_char => Esc("endlinechar").fmt(f),
            new_line_char => Esc("newlinechar").fmt(f),
            language => Esc("language").fmt(f),
            left_hyphen_min => Esc("lefthyphenmin").fmt(f),
            right_hyphen_min => Esc("righthyphenmin").fmt(f),
            holding_inserts => Esc("holdinginserts").fmt(f),
            error_context_lines => Esc("errorcontextlines").fmt(f),
            char_sub_def_min => Esc("charsubdefmin").fmt(f),
            char_sub_def_max => Esc("charsubdefmax").fmt(f),
            tracing_char_sub_def => Esc("tracingcharsubdef").fmt(f),
            xetex_linebreak_penalty => Esc("XeTeXlinebreakpenalty").fmt(f),
            xetex_protrude_chars => Esc("XeTeXprotrudechars").fmt(f),
            synctex => Esc("synctex").fmt(f),
            tracing_assigns => Esc("tracingassigns").fmt(f),
            tracing_groups => Esc("tracinggroups").fmt(f),
            tracing_ifs => Esc("tracingifs").fmt(f),
            tracing_scan_tokens => Esc("tracingscantokens").fmt(f),
            tracing_nesting => Esc("tracingnesting").fmt(f),
            pre_display_correction => Esc("predisplaydirection").fmt(f),
            last_line_fit => Esc("lastlinefit").fmt(f),
            saving_vdiscards => Esc("savingvdiscards").fmt(f),
            saving_hyphs => Esc("savinghyphcodes").fmt(f),
            suppress_fontnotfound_error => Esc("suppressfontnotfounderror").fmt(f),
            texxet => Esc("TeXXeTstate").fmt(f),
            xetex_upwards => Esc("XeTeXupwardsmode").fmt(f),
            xetex_use_glyph_metrics => Esc("XeTeXuseglyphmetrics").fmt(f),
            xetex_inter_char_tokens => Esc("XeTeXinterchartokenstate").fmt(f),
            xetex_dash_break => Esc("XeTeXdashbreakstate").fmt(f),
            xetex_input_normalization => Esc("XeTeXinputnormalization").fmt(f),
            xetex_tracing_fonts => Esc("XeTeXtracingfonts").fmt(f),
            xetex_interword_space_shaping => Esc("XeTeXinterwordspaceshaping").fmt(f),
            xetex_generate_actual_text => Esc("XeTeXgenerateactualtext").fmt(f),
            xetex_hyphenatable_length => Esc("XeTeXhyphenatablelength").fmt(f),
            pdfoutput => Esc("pdfoutput").fmt(f),
            _ => "[unknown i32 parameter!]".fmt(f), // NOTE: several parameters not covered
        }
    }
}

pub(crate) unsafe fn diagnostic<F>(blank_line: bool, f: F)
where
    F: Fn(),
{
    let oldsetting = selector;
    if get_int_par(IntPar::tracing_online) <= 0 && selector == Selector::TERM_AND_LOG {
        selector = Selector::LOG_ONLY;
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
    f();
    t_print_nl!("");
    if blank_line {
        print_ln();
    }
    selector = oldsetting;
}

impl fmt::Display for DimenPar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DimenPar::*;
        match self {
            par_indent => Esc("parindent"),
            math_surround => Esc("mathsurround"),
            line_skip_limit => Esc("lineskiplimit"),
            hsize => Esc("hsize"),
            vsize => Esc("vsize"),
            max_depth => Esc("maxdepth"),
            split_max_depth => Esc("splitmaxdepth"),
            box_max_depth => Esc("boxmaxdepth"),
            hfuzz => Esc("hfuzz"),
            vfuzz => Esc("vfuzz"),
            delimiter_shortfall => Esc("delimitershortfall"),
            null_delimiter_space => Esc("nulldelimiterspace"),
            script_space => Esc("scriptspace"),
            pre_display_size => Esc("predisplaysize"),
            display_width => Esc("displaywidth"),
            display_indent => Esc("displayindent"),
            overfull_rule => Esc("overfullrule"),
            hang_indent => Esc("hangindent"),
            h_offset => Esc("hoffset"),
            v_offset => Esc("voffset"),
            emergency_stretch => Esc("emergencystretch"),
            pdf_page_width => Esc("pdfpagewidth"),
            pdf_page_height => Esc("pdfpageheight"),
        }
        .fmt(f)
    }
}
/// Cases of `print_cmd_chr` for symbolic printing of primitives
pub(crate) struct CmdChr(pub Cmd, pub i32);
impl fmt::Display for CmdChr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cmd = self.0;
        let mut chr_code = self.1;
        match cmd {
            Cmd::LeftBrace => {
                "begin-group character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::RightBrace => {
                "end-group character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::MathShift => {
                "math shift character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::MacParam => {
                "macro parameter character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::SupMark => {
                "superscript character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::SubMark => {
                "subscript character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::EndV => "end of alignment template".fmt(f),
            Cmd::Spacer => {
                "blank space ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::Letter => {
                "the letter ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::OtherChar => {
                "the character ".fmt(f)?;
                std::char::from_u32(chr_code as u32).unwrap().fmt(f)
            }
            Cmd::AssignGlue | Cmd::AssignMuGlue => {
                if chr_code < SKIP_BASE as i32 {
                    match GluePar::n((chr_code - GLUE_BASE as i32) as u16) {
                        Some(dimen) => dimen.fmt(f),
                        None => "[unknown glue parameter!]".fmt(f),
                    }
                } else if chr_code < MU_SKIP_BASE as i32 {
                    Esc("skip").fmt(f)?;
                    (chr_code - SKIP_BASE as i32).fmt(f)
                } else {
                    Esc("muskip").fmt(f)?;
                    (chr_code - MU_SKIP_BASE as i32).fmt(f)
                }
            }
            Cmd::AssignToks => {
                if chr_code >= TOKS_BASE as i32 {
                    Esc("toks").fmt(f)?;
                    (chr_code - TOKS_BASE as i32).fmt(f)
                } else {
                    match Local::n(chr_code - LOCAL_BASE as i32) {
                        Some(Local::output_routine) => Esc("output"),
                        Some(Local::every_par) => Esc("everypar"),
                        Some(Local::every_math) => Esc("everymath"),
                        Some(Local::every_display) => Esc("everydisplay"),
                        Some(Local::every_hbox) => Esc("everyhbox"),
                        Some(Local::every_vbox) => Esc("everyvbox"),
                        Some(Local::every_job) => Esc("everyjob"),
                        Some(Local::every_cr) => Esc("everycr"),
                        Some(Local::every_eof) => Esc("everyeof"),
                        Some(Local::xetex_inter_char) => Esc("XeTeXinterchartoks"),
                        Some(Local::TectonicCodaTokens) => Esc("TectonicCodaTokens"),
                        _ => Esc("errhelp"),
                    }
                    .fmt(f)
                }
            }
            Cmd::AssignInt => {
                if chr_code < COUNT_BASE as i32 {
                    match IntPar::n(chr_code - INT_BASE as i32) {
                        Some(dimen) => dimen.fmt(f),
                        None => "[unknown i32 parameter!]".fmt(f),
                    }
                } else {
                    Esc("count").fmt(f)?;
                    (chr_code - COUNT_BASE as i32).fmt(f)
                }
            }
            Cmd::AssignDimen => {
                if chr_code < SCALED_BASE as i32 {
                    match DimenPar::n(chr_code - DIMEN_BASE as i32) {
                        Some(dimen) => dimen.fmt(f),
                        None => "[unknown dimen parameter!]".fmt(f),
                    }
                } else {
                    Esc("dimen").fmt(f)?;
                    (chr_code - SCALED_BASE as i32).fmt(f)
                }
            }
            Cmd::Accent => Esc("accent").fmt(f),
            Cmd::Advance => Esc("advance").fmt(f),
            Cmd::AfterAssignment => Esc("afterassignment").fmt(f),
            Cmd::AfterGroup => Esc("aftergroup").fmt(f),
            Cmd::AssignFontDimen => Esc("fontdimen").fmt(f),
            Cmd::BeginGroup => Esc("begingroup").fmt(f),
            Cmd::BreakPenalty => Esc("penalty").fmt(f),
            Cmd::CharNum => Esc("char").fmt(f),
            Cmd::CSName => Esc("csname").fmt(f),
            Cmd::DefFont => Esc("font").fmt(f),
            Cmd::DelimNum => if chr_code == 1 {
                Esc("Udelimiter")
            } else {
                Esc("delimiter")
            }
            .fmt(f),
            Cmd::Divide => Esc("divide").fmt(f),
            Cmd::EndCSName => Esc("endcsname").fmt(f),
            Cmd::EndGroup => Esc("endgroup").fmt(f),
            Cmd::ExSpace => Esc(" ").fmt(f),
            Cmd::ExpandAfter => if chr_code == 0 {
                Esc("expandafter")
            } else {
                Esc("unless")
            }
            .fmt(f),
            Cmd::HAlign => Esc("halign").fmt(f),
            Cmd::HRule => Esc("hrule").fmt(f),
            Cmd::IgnoreSpaces => if chr_code == 0 {
                Esc("ignorespaces")
            } else {
                Esc("primitive")
            }
            .fmt(f),
            Cmd::Insert => Esc("insert").fmt(f),
            Cmd::ItalCorr => Esc("/").fmt(f),
            Cmd::Mark => if chr_code > 0 {
                Esc("marks")
            } else {
                Esc("mark")
            }
            .fmt(f),
            Cmd::MathAccent => if chr_code == 1 {
                Esc("Umathaccent")
            } else {
                Esc("mathaccent")
            }
            .fmt(f),
            Cmd::MathCharNum => match chr_code {
                2 => Esc("Umathchar"),
                1 => Esc("Umathcharnum"),
                _ => Esc("mathchar"),
            }
            .fmt(f),
            Cmd::MathChoice => Esc("mathchoice").fmt(f),
            Cmd::Multiply => Esc("multiply").fmt(f),
            Cmd::NoAlign => Esc("noalign").fmt(f),
            Cmd::NoBoundary => Esc("noboundary").fmt(f),
            Cmd::NoExpand => if chr_code == 0 {
                Esc("noexpand")
            } else {
                Esc("primitive")
            }
            .fmt(f),
            Cmd::NonScript => Esc("nonscript").fmt(f),
            Cmd::Omit => Esc("omit").fmt(f),
            Cmd::Radical => if chr_code == 1 {
                Esc("Uradical")
            } else {
                Esc("radical")
            }
            .fmt(f),
            Cmd::ReadToCS => if chr_code == 0 {
                Esc("read")
            } else {
                Esc("readline")
            }
            .fmt(f),
            Cmd::Relax => Esc("relax").fmt(f),
            Cmd::SetBox => Esc("setbox").fmt(f),
            Cmd::SetPrevGraf => Esc("prevgraf").fmt(f),
            Cmd::SetShape => match chr_code as usize {
                c if c == LOCAL_BASE as usize + Local::par_shape as usize => Esc("parshape"),
                INTER_LINE_PENALTIES_LOC => Esc("interlinepenalties"),
                CLUB_PENALTIES_LOC => Esc("clubpenalties"),
                WIDOW_PENALTIES_LOC => Esc("widowpenalties"),
                DISPLAY_WIDOW_PENALTIES_LOC => Esc("displaywidowpenalties"),
                _ => unreachable!(),
            }
            .fmt(f),
            Cmd::The => match chr_code {
                0 => Esc("the"),
                1 => Esc("unexpanded"),
                _ => Esc("detokenize"),
            }
            .fmt(f),
            Cmd::ToksRegister => {
                Esc("toks").fmt(f)?;
                if chr_code != 0 {
                    SaNum(chr_code.opt().unwrap()).fmt(f)
                } else {
                    Ok(())
                }
            }
            Cmd::VAdjust => Esc("vadjust").fmt(f),
            Cmd::VAlign => if chr_code == 0 {
                Esc("valign")
            } else {
                match MathType::from(chr_code as u16) {
                    MathType::Eq(BE::Begin, MathMode::Left) => Esc("beginL"),
                    MathType::Eq(BE::End, MathMode::Left) => Esc("endL"),
                    MathType::Eq(BE::Begin, MathMode::Right) => Esc("beginR"),
                    _ => Esc("endR"),
                }
            }
            .fmt(f),
            Cmd::VCenter => Esc("vcenter").fmt(f),
            Cmd::VRule => Esc("vrule").fmt(f),
            PAR_END => Esc("par").fmt(f),
            Cmd::Input => match chr_code {
                0 => Esc("input"),
                2 => Esc("scantokens"),
                _ => Esc("endinput"),
            }
            .fmt(f),
            Cmd::TopBotMark => {
                match (chr_code % MARKS_CODE) as usize {
                    FIRST_MARK_CODE => Esc("firstmark"),
                    BOT_MARK_CODE => Esc("botmark"),
                    SPLIT_FIRST_MARK_CODE => Esc("splitfirstmark"),
                    SPLIT_BOT_MARK_CODE => Esc("splitbotmark"),
                    _ => Esc("topmark"),
                }
                .fmt(f)?;
                if chr_code >= MARKS_CODE {
                    's'.fmt(f)
                } else {
                    Ok(())
                }
            }
            Cmd::Register => {
                let cmd;
                if chr_code < 0 || chr_code > LO_MEM_STAT_MAX {
                    cmd = unsafe { (MEM[chr_code as usize].b16.s1 as i32 / 64) as u16 };
                } else {
                    cmd = chr_code as u16;
                    chr_code = None.tex_int();
                }
                if cmd == ValLevel::Int as u16 {
                    Esc("count")
                } else if cmd == ValLevel::Dimen as u16 {
                    Esc("dimen")
                } else if cmd == ValLevel::Glue as u16 {
                    Esc("skip")
                } else {
                    Esc("muskip")
                }
                .fmt(f)?;
                if let Some(q) = chr_code.opt() {
                    SaNum(q).fmt(f)
                } else {
                    Ok(())
                }
            }
            Cmd::SetAux => if chr_code == ListMode::VMode as i32 {
                Esc("prevdepth")
            } else {
                Esc("spacefactor")
            }
            .fmt(f),
            Cmd::SetPageInt => match chr_code {
                0 => Esc("deadcycles"),
                2 => Esc("interactionmode"),
                _ => Esc("insertpenalties"),
            }
            .fmt(f),
            Cmd::SetBoxDimen => match SetBoxDimen::n(chr_code as u8).unwrap() {
                SetBoxDimen::WidthOffset => Esc("wd"),
                SetBoxDimen::HeightOffset => Esc("ht"),
                SetBoxDimen::DepthOffset => Esc("dp"),
            }
            .fmt(f),
            Cmd::LastItem => Esc({
                use LastItemCode::*;
                match LastItemCode::n(chr_code as u8).unwrap() {
                    LastPenalty => "lastpenalty",
                    LastKern => "lastkern",
                    LastSkip => "lastskip",
                    InputLineNo => "inputlineno",
                    PdfShellEscape => "shellescape",
                    LastNodeType => "lastnodetype",
                    EtexVersion => "eTeXversion",
                    XetexVersion => "XeTeXversion",
                    XetexCountGlyphs => "XeTeXcountglyphs",
                    XetexCountVariations => "XeTeXcountvariations",
                    XetexVariation => "XeTeXvariation",
                    XetexFindVariationByName => "XeTeXfindvariationbyname",
                    XetexVariationMin => "XeTeXvariationmin",
                    XetexVariationMax => "XeTeXvariationmax",
                    XetexVariationDefault => "XeTeXvariationdefault",
                    XetexCountFeatures => "XeTeXcountfeatures",
                    XetexFeatureCode => "XeTeXfeaturecode",
                    XetexFindFeatureByName => "XeTeXfindfeaturebyname",
                    XetexIsExclusiveFeature => "XeTeXisexclusivefeature",
                    XetexCountSelectors => "XeTeXcountselectors",
                    XetexSelectorCode => "XeTeXselectorcode",
                    XetexFindSelectorByName => "XeTeXfindselectorbyname",
                    XetexIsDefaultSelector => "XeTeXisdefaultselector",
                    XetexOTCountScripts => "XeTeXOTcountscripts",
                    XetexOTCountLanguages => "XeTeXOTcountlanguages",
                    XetexOTCountFeatures => "XeTeXOTcountfeatures",
                    XetexOTScript => "XeTeXOTscripttag",
                    XetexOTLanguage => "XeTeXOTlanguagetag",
                    XetexOTFeature => "XeTeXOTfeaturetag",
                    XetexMapCharToGlyph => "XeTeXcharglyph",
                    XetexGlyphIndex => "XeTeXglyphindex",
                    XetexGlyphBounds => "XeTeXglyphbounds",
                    XetexFontType => "XeTeXfonttype",
                    XetexFirstChar => "XeTeXfirstfontchar",
                    XetexLastChar => "XeTeXlastfontchar",
                    PdfLastXPos => "pdflastxpos",
                    PdfLastYPos => "pdflastypos",
                    XetexPdfPageCount => "XeTeXpdfpagecount",
                    CurrentGroupLevel => "currentgrouplevel",
                    CurrentGroupType => "currentgrouptype",
                    CurrentIfLevel => "currentiflevel",
                    CurrentIfType => "currentiftype",
                    CurrentIfBranch => "currentifbranch",
                    FontCharWd => "fontcharwd",
                    FontCharHt => "fontcharht",
                    FontCharDp => "fontchardp",
                    FontCharIc => "fontcharic",
                    ParShapeLength => "parshapelength",
                    ParShapeIndent => "parshapeindent",
                    ParShapeDimen => "parshapedimen",
                    EtexExprInt => "numexpr",
                    EtexExprDimen => "dimexpr",
                    EtexExprGlue => "glueexpr",
                    EtexExprMu => "muexpr",
                    GlueStretchOrder => "gluestretchorder",
                    GlueShrinkOrder => "glueshrinkorder",
                    GlueStretch => "gluestretch",
                    GlueShrink => "glueshrink",
                    MuToGlue => "mutoglue",
                    GlueToMu => "gluetomu",
                    Badness => "badness",
                }
            })
            .fmt(f),
            Cmd::Convert => Esc({
                use ConvertCode::*;
                match ConvertCode::n(chr_code as u8).unwrap() {
                    Number => "number",
                    RomanNumeral => "romannumeral",
                    String => "string",
                    Meaning => "meaning",
                    FontName => "fontname",
                    PdfStrcmp => "strcmp",
                    PdfMdfiveSum => "mdfivesum",
                    LeftMarginKern => "leftmarginkern",
                    RightMarginKern => "rightmarginkern",
                    EtexRevision => "eTeXrevision",
                    XetexRevision => "XeTeXrevision",
                    XetexVariationName => "XeTeXvariationname",
                    XetexFeatureName => "XeTeXfeaturename",
                    XetexSelectorName => "XeTeXselectorname",
                    XetexGlyphName => "XeTeXglyphname",
                    XetexUchar => "Uchar",
                    XetexUcharcat => "Ucharcat",
                    JobName => "jobname",
                }
            })
            .fmt(f),
            Cmd::IfTest => {
                if chr_code >= UNLESS_CODE {
                    Esc("unless").fmt(f)?;
                }
                Esc(
                    match IfTestCode::n((chr_code % UNLESS_CODE) as u8).unwrap() {
                        IfTestCode::IfCat => "ifcat",
                        IfTestCode::IfInt => "ifnum",
                        IfTestCode::IfDim => "ifdim",
                        IfTestCode::IfOdd => "ifodd",
                        IfTestCode::IfVMode => "ifvmode",
                        IfTestCode::IfHMode => "ifhmode",
                        IfTestCode::IfMMode => "ifmmode",
                        IfTestCode::IfInner => "ifinner",
                        IfTestCode::IfVoid => "ifvoid",
                        IfTestCode::IfHBox => "ifhbox",
                        IfTestCode::IfVBox => "ifvbox",
                        IfTestCode::Ifx => "ifx",
                        IfTestCode::IfEof => "ifeof",
                        IfTestCode::IfTrue => "iftrue",
                        IfTestCode::IfFalse => "iffalse",
                        IfTestCode::IfCase => "ifcase",
                        IfTestCode::IfPrimitive => "ifprimitive",
                        IfTestCode::IfDef => "ifdefined",
                        IfTestCode::IfCS => "ifcsname",
                        IfTestCode::IfFontChar => "iffontchar",
                        IfTestCode::IfInCSName => "ifincsname",
                        IfTestCode::IfChar => "if",
                    },
                )
                .fmt(f)
            }
            Cmd::FiOrElse => match FiOrElseCode::n(chr_code as u8).unwrap() {
                FiOrElseCode::Fi => Esc("fi"),
                FiOrElseCode::Or => Esc("or"),
                FiOrElseCode::Else => Esc("else"),
                _ => unreachable!(),
            }
            .fmt(f),
            Cmd::TabMark => {
                if chr_code == SPAN_CODE {
                    Esc("span").fmt(f)
                } else {
                    "alignment tab character ".fmt(f)?;
                    std::char::from_u32(chr_code as u32).unwrap().fmt(f)
                }
            }
            Cmd::CarRet => if chr_code == CR_CODE {
                Esc("cr")
            } else {
                Esc("crcr")
            }
            .fmt(f),
            Cmd::SetPageDimen => Esc(match chr_code {
                0 => "pagegoal",         // genuine literal in WEB
                1 => "pagetotal",        // genuine literal in WEB
                2 => "pagestretch",      // genuine literal in WEB
                3 => "pagefilstretch",   // genuine literal in WEB
                4 => "pagefillstretch",  // genuine literal in WEB
                5 => "pagefilllstretch", // genuine literal in WEB
                6 => "pageshrink",       // genuine literal in WEB
                _ => "pagedepth",
            })
            .fmt(f),
            STOP => if chr_code == 1 {
                Esc("dump")
            } else {
                Esc("end")
            }
            .fmt(f),
            Cmd::HSkip => Esc(match SkipCode::n(chr_code as u8).unwrap() {
                SkipCode::Skip => "hskip",
                SkipCode::Fil => "hfil",
                SkipCode::Fill => "hfill",
                SkipCode::Ss => "hss",
                SkipCode::FilNeg => "hfilneg",
                _ => unreachable!(),
            })
            .fmt(f),
            Cmd::VSkip => Esc(match SkipCode::n(chr_code as u8).unwrap() {
                SkipCode::Skip => "vskip",
                SkipCode::Fil => "vfil",
                SkipCode::Fill => "vfill",
                SkipCode::Ss => "vss",
                SkipCode::FilNeg => "vfilneg",
                _ => unreachable!(),
            })
            .fmt(f),
            Cmd::MSkip => Esc("mskip").fmt(f),
            Cmd::Kern => Esc("kern").fmt(f),
            Cmd::MKern => Esc("mkern").fmt(f),
            Cmd::HMove => if chr_code == 1 {
                Esc("moveleft")
            } else {
                Esc("moveright")
            }
            .fmt(f),
            Cmd::VMove => if chr_code == 1 {
                Esc("raise")
            } else {
                Esc("lower")
            }
            .fmt(f),
            Cmd::MakeBox => Esc(match BoxCode::n(chr_code as u8).unwrap() {
                BoxCode::Box => "box",
                BoxCode::Copy => "copy",
                BoxCode::LastBox => "lastbox",
                BoxCode::VSplit => "vsplit",
                BoxCode::VTop => "vtop",
                BoxCode::VBox => "vbox",
                BoxCode::HBox => "hbox",
            })
            .fmt(f),
            Cmd::LeaderShip => Esc(match chr_code as u16 {
                A_LEADERS => "leaders",
                C_LEADERS => "cleaders",
                X_LEADERS => "xleaders",
                _ => "shipout",
            })
            .fmt(f),
            Cmd::StartPar => if chr_code == 0 {
                Esc("noindent")
            } else {
                Esc("indent")
            }
            .fmt(f),
            Cmd::RemoveItem => Esc(match chr_code {
                10 => "unskip", // Node::Glue
                11 => "unkern", // Node::Kern
                _ => "unpenalty",
            })
            .fmt(f),
            Cmd::UnHBox => Esc(match BoxCode::n(chr_code as u8).unwrap() {
                BoxCode::Copy => "unhcopy",
                _ => "unhbox",
            })
            .fmt(f),
            Cmd::UnVBox => Esc(match BoxCode::n(chr_code as u8).unwrap() {
                BoxCode::Copy => "unvcopy",
                BoxCode::LastBox => "pagediscards",
                BoxCode::VSplit => "splitdiscards",
                _ => "unvbox",
            })
            .fmt(f),
            Cmd::Discretionary => {
                if chr_code == 1 {
                    Esc("-").fmt(f)
                } else {
                    Esc("discretionary").fmt(f)
                }
            }
            Cmd::EqNo => if chr_code == 1 {
                Esc("leqno")
            } else {
                Esc("eqno")
            }
            .fmt(f),
            Cmd::MathComp => Esc(match MathNode::n(chr_code as u16).unwrap() {
                MathNode::Ord => "mathord",
                MathNode::Op => "mathop",
                MathNode::Bin => "mathbin",
                MathNode::Rel => "mathrel",
                MathNode::Open => "mathopen",
                MathNode::Close => "mathclose",
                MathNode::Punct => "mathpunct",
                MathNode::Inner => "mathinner",
                MathNode::Under => "underline",
                _ => "overline",
            })
            .fmt(f),
            Cmd::LimitSwitch => match Limit::from(chr_code as u16) {
                Limit::Limits => Esc("limits").fmt(f),
                Limit::NoLimits => Esc("nolimits").fmt(f),
                Limit::Normal => Esc("displaylimits").fmt(f),
            },
            Cmd::MathStyle => match MathStyle::from_cur(chr_code as i16) {
                Some(m) => m.fmt(f),
                None => "Unknown style!".fmt(f),
            },
            Cmd::Above => match chr_code {
                OVER_CODE => Esc("over"),
                ATOP_CODE => Esc("atop"),
                // DELIMITED_CODE + ABOVE_CODE
                3 => Esc("abovewithdelims"),
                // DELIMITED_CODE + OVER_CODE
                4 => Esc("overwithdelims"),
                // DELIMITED_CODE + ATOP_CODE
                5 => Esc("atopwithdelims"),
                _ => Esc("above"),
            }
            .fmt(f),
            Cmd::LeftRight => if chr_code as u16 == MathNode::Left as u16 {
                Esc("left")
            } else if chr_code as u16 == MIDDLE_NOAD {
                Esc("middle")
            } else {
                Esc("right")
            }
            .fmt(f),
            Cmd::Prefix => match chr_code {
                1 => Esc("long"),
                2 => Esc("outer"),
                8 => Esc("protected"),
                _ => Esc("global"),
            }
            .fmt(f),
            Cmd::Def => match chr_code {
                0 => Esc("def"),
                1 => Esc("gdef"),
                2 => Esc("edef"),
                _ => Esc("xdef"),
            }
            .fmt(f),
            Cmd::Let => if chr_code as u16 != NORMAL {
                Esc("futurelet")
            } else {
                Esc("let")
            }
            .fmt(f),
            Cmd::ShorthandDef => Esc(match ShorthandDefCode::n(chr_code as u8).unwrap() {
                ShorthandDefCode::Char => "chardef",
                ShorthandDefCode::MathChar => "mathchardef",
                ShorthandDefCode::XetexMathChar => "Umathchardef",
                ShorthandDefCode::XetexMathCharNum => "Umathcharnumdef",
                ShorthandDefCode::Count => "countdef",
                ShorthandDefCode::Dimen => "dimendef",
                ShorthandDefCode::Skip => "skipdef",
                ShorthandDefCode::MuSkip => "muskipdef",
                ShorthandDefCode::CharSub => "charsubdef",
                ShorthandDefCode::Toks => "toksdef",
            })
            .fmt(f),
            Cmd::CharGiven => format!("{}\"{:X}", Esc("char"), chr_code).fmt(f),
            Cmd::MathGiven => format!("{}\"{:X}", Esc("mathchar"), chr_code).fmt(f),
            Cmd::XetexMathGiven => format!(
                "{}\"{:X}\"{:X}\"{:X}",
                Esc("Umathchar"),
                math_class(chr_code) as i32,
                math_fam(chr_code) as i32,
                math_char(chr_code) as i32
            )
            .fmt(f),
            Cmd::DefCode => Esc(if chr_code == CAT_CODE_BASE as i32 {
                "catcode"
            } else if chr_code == MATH_CODE_BASE as i32 {
                "mathcode"
            } else if chr_code == LC_CODE_BASE as i32 {
                "lccode"
            } else if chr_code == UC_CODE_BASE as i32 {
                "uccode"
            } else if chr_code == SF_CODE_BASE as i32 {
                "sfcode"
            } else {
                "delcode"
            })
            .fmt(f),
            Cmd::XetexDefCode => Esc(if chr_code == SF_CODE_BASE as i32 {
                "XeTeXcharclass"
            } else if chr_code == MATH_CODE_BASE as i32 {
                "Umathcodenum"
            } else if chr_code == MATH_CODE_BASE as i32 + 1 {
                "Umathcode"
            } else if chr_code == DEL_CODE_BASE as i32 {
                "Udelcodenum"
            } else {
                "Udelcode"
            })
            .fmt(f),
            Cmd::DefFamily => FontSize::from((chr_code - (MATH_FONT_BASE as i32)) as usize).fmt(f),
            Cmd::HyphData => if chr_code == 1 {
                Esc("patterns")
            } else {
                Esc("hyphenation")
            }
            .fmt(f),
            Cmd::AssignFontInt => Esc(match AssignFontInt::from(chr_code) {
                AssignFontInt::HyphenChar => "hyphenchar",
                AssignFontInt::SkewChar => "skewchar",
                AssignFontInt::LpCode => "lpcode",
                AssignFontInt::RpCode => "rpcode",
            })
            .fmt(f),
            Cmd::SetFont => {
                "select font ".fmt(f)?;
                let font_name_str = unsafe { PoolString::from(FONT_NAME[chr_code as usize]) };
                if let Font::Native(_) = unsafe { &FONT_LAYOUT_ENGINE[chr_code as usize] } {
                    let mut quote_char = '\"';
                    if font_name_str.as_slice().contains(&('\"' as u16)) {
                        quote_char = '\'';
                    }
                    quote_char.fmt(f)?;
                    font_name_str.fmt(f)?;
                    quote_char.fmt(f)?;
                } else {
                    font_name_str.fmt(f)?
                }
                if unsafe { FONT_SIZE[chr_code as usize] != FONT_DSIZE[chr_code as usize] } {
                    " at ".fmt(f)?;
                    unsafe { FONT_SIZE[chr_code as usize] }.fmt(f)?;
                    "pt".fmt(f)
                } else {
                    Ok(())
                }
            }
            Cmd::SetInteraction => Esc(match InteractionMode::n(chr_code as u8).unwrap() {
                InteractionMode::Batch => "batchmode",
                InteractionMode::NonStop => "nonstopmode",
                InteractionMode::Scroll => "scrollmode",
                InteractionMode::ErrorStop => "errorstopmode",
            })
            .fmt(f),
            Cmd::InStream => if chr_code == 0 {
                Esc("closein")
            } else {
                Esc("openin")
            }
            .fmt(f),
            Cmd::Message => if chr_code == 0 {
                Esc("message")
            } else {
                Esc("errmessage")
            }
            .fmt(f),
            Cmd::CaseShift => if chr_code == LC_CODE_BASE as i32 {
                Esc("lowercase")
            } else {
                Esc("uppercase")
            }
            .fmt(f),
            Cmd::XRay => Esc(match chr_code {
                SHOW_BOX_CODE => "showbox",
                SHOW_THE_CODE => "showthe",
                SHOW_LISTS => "showlists",
                SHOW_GROUPS => "showgroups",
                SHOW_TOKENS => "showtokens",
                SHOW_IFS => "showifs",
                _ => "show",
            })
            .fmt(f),
            Cmd::UndefinedCS => "undefined".fmt(f),
            Cmd::Call | Cmd::LongCall | Cmd::OuterCall | Cmd::LongOuterCall => {
                let mut n = match cmd {
                    Cmd::Call => 0,
                    Cmd::LongCall => 1,
                    Cmd::OuterCall => 2,
                    Cmd::LongOuterCall => 3,
                    _ => unreachable!(),
                };
                if unsafe { MEM[*LLIST_link(chr_code as usize) as usize].b32.s0 == PROTECTED_TOKEN }
                {
                    n += 4
                }
                if (n / 4) & 1 != 0 {
                    Esc("protected").fmt(f)?;
                }
                if n & 1 != 0 {
                    Esc("long").fmt(f)?;
                }
                if (n / 2) & 1 != 0 {
                    Esc("outer").fmt(f)?;
                }
                if n > 0 {
                    " ".fmt(f)?;
                }
                "macro".fmt(f)
            }
            Cmd::EndTemplate => Esc("outer endtemplate").fmt(f),
            Cmd::Extension => match chr_code as u16 {
                0 => Esc("openout").fmt(f),               // WhatsIt::Open
                1 => Esc("write").fmt(f),                 // WhatsIt::Write
                2 => Esc("closeout").fmt(f),              // WhatsIt::Close
                3 => Esc("special").fmt(f),               // WhatsIt::Special
                4 => Esc("immediate").fmt(f),             // IMMEDIATE_CODE
                5 => Esc("setlanguage").fmt(f),           // SET_LANGUAGE_CODE
                41 => Esc("XeTeXpicfile").fmt(f),         // PIC_FILE_CODE
                42 => Esc("XeTeXpdffile").fmt(f),         // PDF_FILE_CODE
                43 => Esc("XeTeXglyph").fmt(f),           // GLYPH_CODE
                46 => Esc("XeTeXlinebreaklocale").fmt(f), // XETEX_LINEBREAK_LOCALE_EXTENSION_CODE
                44 => Esc("XeTeXinputencoding").fmt(f),   // XETEX_INPUT_ENCODING_EXTENSION_CODE
                45 => Esc("XeTeXdefaultencoding").fmt(f), // XETEX_DEFAULT_ENCODING_EXTENSION_CODE
                6 => Esc("pdfsavepos").fmt(f),            // WhatsIt::PdfSavePos
                _ => ("[unknown extension!]").fmt(f),
            },
            _ => ("[unknown command code!]").fmt(f),
        }
    }
}
pub(crate) unsafe fn not_aat_font_error(cmd: Cmd, c: i32, f: usize) {
    t_eprint!(
        "Cannot use {} with {}; not an AAT font",
        CmdChr(cmd, c),
        PoolString::from(FONT_NAME[f])
    );
    error();
}
pub(crate) unsafe fn not_aat_gr_font_error(cmd: Cmd, c: i32, f: usize) {
    t_eprint!(
        "Cannot use {} with {}; not an AAT or Graphite font",
        CmdChr(cmd, c),
        PoolString::from(FONT_NAME[f])
    );
    error();
}
pub(crate) unsafe fn not_ot_font_error(cmd: Cmd, c: i32, f: usize) {
    t_eprint!(
        "Cannot use {} with {}; not an OpenType Layout font",
        CmdChr(cmd, c),
        PoolString::from(FONT_NAME[f])
    );
    error();
}
pub(crate) unsafe fn not_native_font_error(cmd: Cmd, c: i32, f: usize) {
    t_eprint!(
        "Cannot use {} with {}; not a native platform font",
        CmdChr(cmd, c),
        PoolString::from(FONT_NAME[f])
    );
    error();
}
/*:1434*/
pub(crate) unsafe fn id_lookup(j: usize, l: usize) -> i32 {
    let mut h = 0;
    for k in j..=j + l - 1 {
        h = h + h + BUFFER[k as usize];
        while h >= HASH_PRIME as i32 {
            h -= 8501;
        }
    }
    let mut p = (h + HASH_BASE as i32) as usize;
    let mut ll = l;
    for d in 0..=l - 1 {
        if BUFFER[j + d] as i64 >= 65536 {
            ll += 1
        }
    }
    loop {
        if yhash[p as usize - hash_offset].s1 > 0 {
            let ps = PoolString::from(yhash[p as usize - hash_offset].s1);
            if ps.len() == ll && str_eq_buf(&ps, j) {
                break;
            }
        }
        if yhash[p - hash_offset].s0 == 0 {
            if no_new_control_sequence {
                p = UNDEFINED_CONTROL_SEQUENCE;
            } else {
                if yhash[p - hash_offset].s1 > 0 {
                    if hash_high < hash_extra {
                        hash_high += 1;
                        yhash[p - hash_offset].s0 = (hash_high + EQTB_SIZE) as i32;
                        p = hash_high + EQTB_SIZE;
                    } else {
                        loop {
                            if hash_used == HASH_BASE {
                                overflow("hash size", HASH_SIZE + hash_extra);
                            }
                            hash_used -= 1;
                            if yhash[hash_used - hash_offset].s1 == 0 {
                                break;
                            }
                        }
                        yhash[p - hash_offset].s0 = hash_used as i32;
                        p = hash_used
                    }
                }
                PoolString::check_capacity(ll);
                let d = PoolString::current().len();
                while pool_ptr > str_start[(str_ptr - TOO_BIG_CHAR) as usize] {
                    pool_ptr -= 1;
                    str_pool[pool_ptr + l] = str_pool[pool_ptr]
                }
                for k in j..=j + l - 1 {
                    let mut b = [0; 2];
                    for c in std::char::from_u32(BUFFER[k as usize] as u32)
                        .unwrap()
                        .encode_utf16(&mut b)
                    {
                        str_pool[pool_ptr] = *c;
                        pool_ptr += 1
                    }
                }
                yhash[p - hash_offset].s1 = make_string();
                pool_ptr += d;
            }
            break;
        } else {
            p = yhash[p - hash_offset].s0 as usize;
        }
    }
    p as i32
}
pub(crate) unsafe fn prim_lookup(s: str_number) -> usize {
    let mut l = 0;
    let mut p = if s < TOO_BIG_CHAR {
        if s < 0 {
            return UNDEFINED_PRIMITIVE as usize;
        } else {
            ((s as usize) % PRIM_PRIME) + 1
        }
    } else {
        let j = str_start[(s - TOO_BIG_CHAR) as usize];
        l = if s == str_ptr {
            PoolString::current().len()
        } else {
            PoolString::from(s).len()
        };
        let mut h = str_pool[j] as usize;
        for k in (j + 1)..(j + l) {
            h = h + h + str_pool[k] as usize;
            while h >= PRIM_PRIME {
                h -= 431;
            }
        }
        h + 1
    };
    loop {
        if prim[p].s1 as i64 > 65536 {
            if PoolString::from(prim[p].s1).len() - 1 == l {
                // TODO: suspiciously
                if PoolString::from(prim[p].s1 - 1) == PoolString::from(s) {
                    return p;
                }
            }
        } else if prim[p].s1 == 1 + s {
            return p;
        }
        if prim[p].s0 == 0 {
            if no_new_control_sequence {
                p = UNDEFINED_PRIMITIVE as usize;
            } else {
                /*272:*/
                if prim[p].s1 > 0 {
                    loop {
                        if prim_used == PRIM_BASE {
                            overflow("primitive size", PRIM_SIZE as usize);
                        }
                        prim_used -= 1;
                        if prim[prim_used].s1 == 0 {
                            break;
                        }
                    }
                    prim[p].s0 = prim_used as i32;
                    p = prim_used
                }
                prim[p].s1 = s + 1;
            }
            return p;
        }
        p = prim[p].s0 as usize;
    }
}
/*:276*/
/*280: *//*296: */
pub(crate) unsafe fn print_group(group: GroupCode, level: u16, saveptr: usize, e: bool) {
    match group {
        GroupCode::BottomLevel => {
            t_print!("bottom level");
            return;
        }
        GroupCode::Simple | GroupCode::SemiSimple => {
            if group == GroupCode::SemiSimple {
                t_print!("semi ");
            }
            t_print!("simple");
        }
        GroupCode::HBox | GroupCode::AdjustedHBox => {
            if group == GroupCode::AdjustedHBox {
                t_print!("adjusted ");
            }
            t_print!("hbox");
        }
        GroupCode::VBox => t_print!("vbox"),
        GroupCode::VTop => t_print!("vtop"),
        GroupCode::Align | GroupCode::NoAlign => {
            if group == GroupCode::NoAlign {
                t_print!("no ");
            }
            t_print!("align");
        }
        GroupCode::Output => t_print!("output"),
        GroupCode::Disc => t_print!("disc"),
        GroupCode::Insert => t_print!("insert"),
        GroupCode::VCenter => t_print!("vcenter"),
        GroupCode::Math | GroupCode::MathChoice | GroupCode::MathShift | GroupCode::MathLeft => {
            t_print!("math");
            if group == GroupCode::MathChoice {
                t_print!(" choice");
            } else if group == GroupCode::MathShift {
                t_print!(" shift");
            } else if group == GroupCode::MathLeft {
                t_print!(" left");
            }
        }
    }
    t_print!(" group (level {})", level as i32);
    let lin = SAVE_STACK[saveptr - 1].val;
    if lin != 0 {
        if e {
            t_print!(" entered at line {}", lin);
        } else {
            t_print!(" at line {}", lin);
        }
    };
}
/*:1448*/
/*1449: */
pub(crate) unsafe fn pseudo_input(input: &mut input_state_t) -> bool {
    last = first;
    if let Some(p) = MEM[pseudo_files as usize].b32.s0.opt() {
        MEM[pseudo_files as usize].b32.s0 = MEM[p as usize].b32.s1;
        let sz = MEM[p].b32.s0 as usize;
        if 4 * sz - 3 >= BUF_SIZE - last as usize {
            /*35: */
            input.loc = Some(first);
            input.limit = last - 1;
            overflow("buffer size", BUF_SIZE);
        }
        last = first;
        for r in (p + 1)..(p + sz) {
            let w = MEM[r].b16;
            BUFFER[last as usize] = w.s3 as UnicodeScalar;
            BUFFER[(last + 1) as usize] = w.s2 as UnicodeScalar;
            BUFFER[(last + 2) as usize] = w.s1 as UnicodeScalar;
            BUFFER[(last + 3) as usize] = w.s0 as UnicodeScalar;
            last += 4;
        }
        if last >= max_buf_stack {
            max_buf_stack = last + 1
        }
        while last > first && BUFFER[(last - 1) as usize] == ' ' as i32 {
            last -= 1
        }
        free_node(p as usize, sz as i32);
        true
    } else {
        false
    }
}
pub(crate) unsafe fn pseudo_close() {
    let p = MEM[pseudo_files as usize].b32.s1;
    let mut q = MEM[pseudo_files as usize].b32.s0.opt();
    MEM[pseudo_files as usize].b32.s1 = avail.tex_int();
    avail = pseudo_files.opt();
    pseudo_files = p;
    while let Some(p) = q {
        q = llist_link(p);
        free_node(p, MEM[p].b32.s0);
    }
}
pub(crate) unsafe fn group_warning(input: &mut input_state_t, input_stack: &[input_state_t]) {
    let mut base_ptr = input_stack.len() - 1;
    let mut i = IN_OPEN;
    let mut w = false;
    while GRP_STACK[i] == cur_boundary && i > 0 {
        if get_int_par(IntPar::tracing_nesting) > 0 {
            while input_stack[base_ptr].state == InputState::TokenList
                || input_stack[base_ptr].index as usize > i
            {
                base_ptr -= 1;
            }
            if input_stack[base_ptr].name > 17 {
                w = true
            }
        }
        GRP_STACK[i] = SAVE_STACK[SAVE_PTR].val;
        i -= 1;
    }
    if w {
        t_print_nl!("Warning: end of ");
        print_group(cur_group, cur_level, SAVE_PTR, true);
        t_print!(" of a different file");
        print_ln();
        if get_int_par(IntPar::tracing_nesting) > 1 {
            INPUT_STACK[INPUT_PTR] = *input;
            show_context(&INPUT_STACK[..INPUT_PTR + 1]);
        }
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn if_warning(input: &mut input_state_t, input_stack: &[input_state_t]) {
    let mut base_ptr = input_stack.len() - 1;
    let mut i = IN_OPEN;
    let mut w = false;
    while IF_STACK[i] == cond_ptr {
        if get_int_par(IntPar::tracing_nesting) > 0 {
            while input_stack[base_ptr].state == InputState::TokenList
                || input_stack[base_ptr].index as usize > i
            {
                base_ptr -= 1
            }
            if input_stack[base_ptr].name > 17 {
                w = true
            }
        }
        IF_STACK[i] = MEM[cond_ptr.unwrap()].b32.s1.opt();
        i -= 1;
    }
    if w {
        t_print_nl!("Warning: end of {}", CmdChr(Cmd::IfTest, cur_if as i32));
        if if_line != 0 {
            t_print!(" entered on line {}", if_line);
        }
        t_print!(" of a different file");
        print_ln();
        if get_int_par(IntPar::tracing_nesting) > 1 {
            INPUT_STACK[INPUT_PTR] = *input;
            show_context(&INPUT_STACK[..INPUT_PTR + 1]);
        }
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn file_warning(input: &mut input_state_t) {
    let mut level = cur_level;
    let mut group = cur_group;
    let mut saveptr = cur_boundary as usize;
    while GRP_STACK[IN_OPEN] != saveptr as i32 {
        level -= 1;
        t_print_nl!("Warning: end of file when ");
        print_group(group, level, saveptr, true);
        t_print!(" is incomplete");
        group = GroupCode::from(SAVE_STACK[saveptr].lvl);
        saveptr = SAVE_STACK[saveptr].val as usize
    }

    let mut condptr = cond_ptr;
    let mut iflimit = if_limit;
    let mut curif = cur_if;
    let mut ifline = if_line;
    while IF_STACK[IN_OPEN] != condptr {
        t_print_nl!(
            "Warning: end of file when {}",
            CmdChr(Cmd::IfTest, curif as i32)
        );
        if iflimit == FiOrElseCode::Fi {
            t_print!("{}", Esc("else"));
        }
        if ifline != 0 {
            t_print!(" entered on line {}", ifline);
        }
        t_print!(" is incomplete");
        let cp = condptr.unwrap();
        ifline = MEM[cp + 1].b32.s1;
        curif = MEM[cp].b16.s0 as i16;
        iflimit = FiOrElseCode::n(MEM[cp].b16.s1 as u8).unwrap();
        condptr = llist_link(cp);
    }
    print_ln();
    if get_int_par(IntPar::tracing_nesting) > 1 {
        INPUT_STACK[INPUT_PTR] = *input;
        show_context(&INPUT_STACK[..INPUT_PTR + 1]);
    }
    if history == TTHistory::SPOTLESS {
        history = TTHistory::WARNING_ISSUED
    };
}
pub(crate) unsafe fn delete_sa_ref(mut q: usize) {
    let mut s;
    MEM[q + 1].b32.s0 -= 1;
    if MEM[q + 1].b32.s0.opt().is_some() {
        return;
    }
    if MEM[q].b16.s1 < DIMEN_VAL_LIMIT {
        if MEM[q + 2].b32.s1 == 0 {
            s = WORD_NODE_SIZE as i16
        } else {
            return;
        }
    } else {
        if MEM[q].b16.s1 < MU_VAL_LIMIT {
            if MEM[q + 1].b32.s1 == 0 {
                delete_glue_ref(0);
            } else {
                return;
            }
        } else if MEM[q + 1].b32.s1.opt().is_some() {
            return;
        }
        s = POINTER_NODE_SIZE as i16
    }
    loop {
        let i = MEM[q].b16.s1 as usize % 64;
        let qi = llist_link(q);
        free_node(q, s as i32);
        if let Some(qii) = qi {
            q = qii;
            let mut qii = Index(qii);
            qii.indexes_mut()[i] = None.tex_int();
            qii.rc_dec();
            s = INDEX_NODE_SIZE as i16;
            if MEM[q].b16.s0 as i32 > 0 {
                break;
            }
        } else {
            sa_root[i] = None;
            return;
        }
    }
}
/*:1609*/
/*1611: */
pub(crate) unsafe fn sa_save(p: usize) {
    let q: usize;
    if cur_level as i32 != sa_level as i32 {
        if SAVE_PTR > MAX_SAVE_STACK {
            MAX_SAVE_STACK = SAVE_PTR;
            if MAX_SAVE_STACK > SAVE_SIZE - 7 {
                overflow("save size", SAVE_SIZE);
            }
        }
        SAVE_STACK[SAVE_PTR].cmd = SaveCmd::RestoreSA as u16;
        SAVE_STACK[SAVE_PTR].lvl = sa_level;
        SAVE_STACK[SAVE_PTR].val = sa_chain;
        SAVE_PTR += 1;
        sa_chain = None.tex_int();
        sa_level = cur_level
    }
    let mut i = MEM[p].b16.s1;
    if i < DIMEN_VAL_LIMIT {
        if MEM[p + 2].b32.s1 == 0 {
            q = get_node(POINTER_NODE_SIZE) as usize;
            i = TOK_VAL_LIMIT
        } else {
            q = get_node(WORD_NODE_SIZE) as usize;
            MEM[q + 2].b32.s1 = MEM[p + 2].b32.s1
        }
        MEM[q + 1].b32.s1 = None.tex_int()
    } else {
        q = get_node(POINTER_NODE_SIZE) as usize;
        MEM[q + 1].b32.s1 = MEM[p + 1].b32.s1
    }
    MEM[q + 1].b32.s0 = p as i32;
    MEM[q].b16.s1 = i;
    MEM[q].b16.s0 = MEM[p].b16.s0;
    *LLIST_link(q) = sa_chain;
    sa_chain = q as i32;
    MEM[p + 1].b32.s0 += 1;
}
pub(crate) unsafe fn sa_destroy(p: usize) {
    if MEM[p].b16.s1 < MU_VAL_LIMIT {
        delete_glue_ref(MEM[p + 1].b32.s1 as usize);
    } else if let Some(n) = MEM[p + 1].b32.s1.opt() {
        if MEM[p].b16.s1 < BOX_VAL_LIMIT {
            flush_node_list(Some(n));
        } else {
            delete_token_ref(MEM[p + 1].b32.s1 as usize);
        }
    };
}
pub(crate) unsafe fn sa_def(p: usize, e: Option<usize>) {
    MEM[p + 1].b32.s0 += 1;
    if MEM[p + 1].b32.s1.opt() == e {
        sa_destroy(p);
    } else {
        if MEM[p].b16.s0 as i32 == cur_level as i32 {
            sa_destroy(p);
        } else {
            sa_save(p);
        }
        MEM[p].b16.s0 = cur_level;
        MEM[p + 1].b32.s1 = e.tex_int();
    }
    delete_sa_ref(p);
}
pub(crate) unsafe fn sa_w_def(p: usize, w: i32) {
    MEM[p + 1].b32.s0 += 1;
    if MEM[p + 2].b32.s1 != w {
        if MEM[p].b16.s0 as i32 != cur_level as i32 {
            sa_save(p);
        }
        MEM[p].b16.s0 = cur_level;
        MEM[p + 2].b32.s1 = w
    }
    delete_sa_ref(p);
}
pub(crate) unsafe fn gsa_def(p: usize, e: Option<usize>) {
    MEM[p + 1].b32.s0 += 1;
    sa_destroy(p);
    MEM[p].b16.s0 = LEVEL_ONE;
    MEM[p + 1].b32.s1 = e.tex_int();
    delete_sa_ref(p);
}
pub(crate) unsafe fn gsa_w_def(p: usize, w: i32) {
    MEM[p + 1].b32.s0 += 1;
    MEM[p].b16.s0 = LEVEL_ONE;
    MEM[p + 2].b32.s1 = w;
    delete_sa_ref(p);
}
pub(crate) unsafe fn sa_restore() {
    loop {
        let p = MEM[(sa_chain + 1) as usize].b32.s0 as usize;
        if MEM[p].b16.s0 == LEVEL_ONE {
            if MEM[p].b16.s1 >= DIMEN_VAL_LIMIT {
                sa_destroy(sa_chain as usize);
            }
        } else {
            if MEM[p].b16.s1 < DIMEN_VAL_LIMIT {
                if MEM[sa_chain as usize].b16.s1 < DIMEN_VAL_LIMIT {
                    MEM[p + 2].b32.s1 = MEM[(sa_chain + 2) as usize].b32.s1
                } else {
                    MEM[p + 2].b32.s1 = 0
                }
            } else {
                sa_destroy(p);
                MEM[p + 1].b32.s1 = MEM[(sa_chain + 1) as usize].b32.s1
            }
            MEM[p].b16.s0 = MEM[sa_chain as usize].b16.s0
        }
        delete_sa_ref(p);
        let p = sa_chain as usize;
        sa_chain = *LLIST_link(p);
        if MEM[p].b16.s1 < DIMEN_VAL_LIMIT {
            free_node(p, WORD_NODE_SIZE);
        } else {
            free_node(p, POINTER_NODE_SIZE);
        }
        if sa_chain.opt().is_none() {
            break;
        }
    }
}
pub(crate) unsafe fn new_save_level(c: GroupCode) {
    if SAVE_PTR > MAX_SAVE_STACK {
        MAX_SAVE_STACK = SAVE_PTR;
        if MAX_SAVE_STACK > SAVE_SIZE - 7 {
            overflow("save size", SAVE_SIZE);
        }
    }
    SAVE_STACK[SAVE_PTR].val = line;
    SAVE_PTR += 1;
    SAVE_STACK[SAVE_PTR].cmd = SaveCmd::LevelBoundary as u16;
    SAVE_STACK[SAVE_PTR].lvl = cur_group as u16;
    SAVE_STACK[SAVE_PTR].val = cur_boundary;
    if cur_level == u16::max_value() {
        overflow("grouping levels", u16::max_value() as usize);
    }
    cur_boundary = SAVE_PTR as i32;
    cur_group = c;
    cur_level += 1;
    SAVE_PTR += 1;
}
pub(crate) unsafe fn eq_destroy(w: EqtbWord) {
    match Cmd::from(w.cmd) {
        Cmd::Call | Cmd::LongCall | Cmd::OuterCall | Cmd::LongOuterCall => {
            delete_token_ref(w.val as usize)
        }
        Cmd::GlueRef => delete_glue_ref(w.val as usize),
        Cmd::ShapeRef => {
            if let Some(q) = w.val.opt() {
                free_node(q, MEM[q].b32.s0 + MEM[q].b32.s0 + 1);
            }
        }
        Cmd::BoxRef => flush_node_list(w.val.opt()),
        Cmd::ToksRegister | Cmd::Register => {
            if w.val < 0 || w.val > 19 {
                delete_sa_ref(w.val as usize);
            }
        }
        _ => {}
    };
}
pub(crate) unsafe fn eq_save(p: usize, l: u16) {
    if SAVE_PTR > MAX_SAVE_STACK {
        MAX_SAVE_STACK = SAVE_PTR;
        if MAX_SAVE_STACK > SAVE_SIZE - 7 {
            overflow("save size", SAVE_SIZE);
        }
    }
    if l == LEVEL_ZERO {
        SAVE_STACK[SAVE_PTR].cmd = SaveCmd::RestoreZero as u16;
    } else {
        SAVE_STACK[SAVE_PTR] = EQTB[p];
        SAVE_PTR += 1;
        SAVE_STACK[SAVE_PTR].cmd = SaveCmd::RestoreOldValue as u16;
    }
    SAVE_STACK[SAVE_PTR].lvl = l;
    SAVE_STACK[SAVE_PTR].val = p as i32;
    SAVE_PTR += 1;
}
pub(crate) unsafe fn eq_define(p: usize, t: Cmd, e: Option<usize>) {
    if eq_type(p) == t && EQTB[p].val.opt() == e {
        eq_destroy(EQTB[p]);
        return;
    }
    if EQTB[p].lvl as i32 == cur_level as i32 {
        eq_destroy(EQTB[p]);
    } else if cur_level > LEVEL_ONE {
        eq_save(p, EQTB[p].lvl);
    }
    EQTB[p].lvl = cur_level;
    EQTB[p].cmd = t as u16;
    EQTB[p].val = e.tex_int();
}
pub(crate) unsafe fn eq_word_define(p: usize, w: i32) {
    if EQTB[p].val == w {
        return;
    }
    if _xeq_level_array[p - (INT_BASE as usize)] as i32 != cur_level as i32 {
        eq_save(p, _xeq_level_array[p - (INT_BASE as usize)]);
        _xeq_level_array[p - (INT_BASE as usize)] = cur_level
    }
    EQTB[p].val = w;
}
pub(crate) unsafe fn geq_define(p: usize, t: Cmd, e: Option<usize>) {
    eq_destroy(EQTB[p]);
    EQTB[p].lvl = LEVEL_ONE;
    EQTB[p].cmd = t as u16;
    EQTB[p].val = e.tex_int();
}
pub(crate) unsafe fn geq_word_define(p: usize, w: i32) {
    EQTB[p].val = w;
    _xeq_level_array[p - (INT_BASE as usize)] = LEVEL_ONE;
}
pub(crate) unsafe fn save_for_after(t: i32) {
    if cur_level > LEVEL_ONE {
        if SAVE_PTR > MAX_SAVE_STACK {
            MAX_SAVE_STACK = SAVE_PTR;
            if MAX_SAVE_STACK > SAVE_SIZE - 7 {
                overflow("save size", SAVE_SIZE);
            }
        }
        SAVE_STACK[SAVE_PTR].cmd = SaveCmd::InsertToken as u16;
        SAVE_STACK[SAVE_PTR].lvl = LEVEL_ZERO;
        SAVE_STACK[SAVE_PTR].val = t;
        SAVE_PTR += 1;
    };
}
pub(crate) unsafe fn unsave(input: &mut input_state_t) {
    let mut l = 0_u16;
    let mut a = false;
    if cur_level > LEVEL_ONE {
        cur_level -= 1;
        loop {
            SAVE_PTR -= 1;
            if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::LevelBoundary as u16 {
                break;
            }
            let p = SAVE_STACK[SAVE_PTR].val;
            if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::InsertToken as u16 {
                /*338: */
                let tok = p;
                if a {
                    let p = get_avail();
                    MEM[p].b32.s0 = tok;
                    *LLIST_link(p) = input.loc.tex_int();
                    input.loc = Some(p);
                    input.start = Some(p);
                    if tok < RIGHT_BRACE_LIMIT {
                        if tok < LEFT_BRACE_LIMIT {
                            align_state -= 1
                        } else {
                            align_state += 1
                        }
                    }
                } else {
                    back_input(input, tok);
                    a = true
                }
            } else if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::RestoreSA as u16 {
                sa_restore();
                sa_chain = p;
                sa_level = SAVE_STACK[SAVE_PTR].lvl
            } else {
                if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::RestoreOldValue as u16 {
                    l = SAVE_STACK[SAVE_PTR].lvl;
                    SAVE_PTR -= 1;
                } else {
                    SAVE_STACK[SAVE_PTR] = EQTB[UNDEFINED_CONTROL_SEQUENCE as usize];
                }
                if p < INT_BASE as i32 || p > EQTB_SIZE as i32 {
                    if EQTB[p as usize].lvl == LEVEL_ONE {
                        eq_destroy(SAVE_STACK[SAVE_PTR]);
                    } else {
                        eq_destroy(EQTB[p as usize]);
                        EQTB[p as usize] = SAVE_STACK[SAVE_PTR]
                    }
                } else if _xeq_level_array[p as usize - INT_BASE] != LEVEL_ONE {
                    EQTB[p as usize] = SAVE_STACK[SAVE_PTR];
                    _xeq_level_array[p as usize - INT_BASE] = l
                }
            }
        }
        if GRP_STACK[IN_OPEN] == cur_boundary {
            INPUT_STACK[INPUT_PTR] = *input;
            group_warning(input, &INPUT_STACK[..INPUT_PTR + 1]);
        }
        cur_group = GroupCode::from(SAVE_STACK[SAVE_PTR].lvl);
        cur_boundary = SAVE_STACK[SAVE_PTR].val;
        SAVE_PTR -= 1;
    } else {
        confusion("curlevel");
    };
}
pub(crate) unsafe fn prepare_mag() {
    if mag_set > 0 && get_int_par(IntPar::mag) != mag_set {
        t_eprint!("Incompatible magnification ({});", get_int_par(IntPar::mag));
        t_print_nl!(" the previous value will be retained");

        help!(
            "I can handle only one magnification ratio per job. So I\'ve",
            "reverted to the magnification you used earlier on this run."
        );
        int_error(mag_set);
        geq_word_define(INT_BASE as usize + IntPar::mag as usize, mag_set);
    }
    if get_int_par(IntPar::mag) <= 0 || get_int_par(IntPar::mag) as i64 > 32768 {
        t_eprint!("Illegal magnification has been changed to 1000");

        help!("The magnification ratio must be between 1 and 32768.");
        int_error(get_int_par(IntPar::mag));
        geq_word_define(INT_BASE as usize + IntPar::mag as usize, 1000);
    }
    mag_set = get_int_par(IntPar::mag);
}
/// Here's the way we sometimes want to display a token list, given a pointer
/// to its reference count; the pointer may be null.
pub(crate) unsafe fn token_show(p: Option<usize>) {
    if let Some(p) = p {
        show_token_list(llist_link(p), None, 10000000);
    }
}
pub(crate) struct TokenNode(pub Option<usize>);
impl<'a> fmt::Display for TokenNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(p) = self.0 {
            TokenList(unsafe { llist_link(p) }).fmt(f)
        } else {
            Ok(())
        }
    }
}

pub(crate) unsafe fn format_meaning(cmd: Cmd, chr: i32) -> String {
    if cmd >= Cmd::Call {
        format!("{}:{}", CmdChr(cmd, chr), TokenNode(chr.opt()))
    } else if cmd == Cmd::TopBotMark && chr < 5 {
        format!("{}:{}", CmdChr(cmd, chr), TokenNode(cur_mark[chr as usize]))
    } else {
        format!("{}", CmdChr(cmd, chr))
    }
}
pub(crate) unsafe fn print_meaning(cmd: Cmd, chr: i32) {
    t_print!("{}", CmdChr(cmd, chr));
    if cmd >= Cmd::Call {
        print_chr(':');
        print_ln();
        token_show(chr.opt());
    } else if cmd == Cmd::TopBotMark && chr < 5 {
        print_chr(':');
        print_ln();
        token_show(cur_mark[chr as usize]);
    };
}
/// Here is a procedure that displays the current command
pub(crate) unsafe fn show_cur_cmd_chr(cmd: Cmd, chr: i32) {
    diagnostic(false, || {
        t_print_nl!("{{");
        if cur_list.mode != shown_mode {
            print_mode(cur_list.mode);
            t_print!(": ");
            shown_mode = cur_list.mode
        }
        t_print!("{}", CmdChr(cmd, chr));
        if get_int_par(IntPar::tracing_ifs) > 0 && cmd >= Cmd::IfTest && cmd <= Cmd::FiOrElse {
            t_print!(": ");
            let mut n;
            let l;
            if cmd == Cmd::FiOrElse {
                t_print!("{} ", CmdChr(Cmd::IfTest, cur_if as i32));
                n = 0;
                l = if_line
            } else {
                n = 1;
                l = line
            }
            let mut popt = cond_ptr;
            while let Some(p) = popt {
                n += 1;
                popt = llist_link(p)
            }
            t_print!("(level {})", n);
            if l != 0 {
                t_print!(" entered on line {}", l);
            }
        }
        print_chr('}');
    });
}
pub(crate) unsafe fn show_context(input_stack: &[input_state_t]) {
    let last_ptr = input_stack.len() - 1;
    let mut base_ptr = last_ptr;
    let mut nn = -1;
    let mut bottom_line = false;
    loop {
        let input = input_stack[base_ptr];
        if input.state != InputState::TokenList && (input.name > 19 || base_ptr == 0) {
            bottom_line = true;
        }
        if base_ptr == last_ptr || bottom_line || nn < get_int_par(IntPar::error_context_lines) {
            /*324: */
            if base_ptr == last_ptr
                || input.state != InputState::TokenList
                || input.index != Btl::BackedUp
                || input.loc.is_some()
            {
                tally = 0;
                let old_setting = selector;
                let l;
                if input.state != InputState::TokenList {
                    if input.name <= 17 {
                        if input.name == 0 {
                            if base_ptr == 0 {
                                t_print_nl!("<*>");
                            } else {
                                t_print_nl!("<insert> ");
                            }
                        } else if input.name == 17 {
                            t_print_nl!("<read *>");
                        } else {
                            t_print_nl!("<read {}>", input.name - 1);
                        }
                    } else {
                        t_print_nl!(
                            "l.{}",
                            if input.index as usize == IN_OPEN {
                                line
                            } else {
                                LINE_STACK[(input.index as i32 + 1) as usize]
                            }
                        );
                    }
                    print_chr(' ');
                    l = tally;
                    tally = 0;
                    selector = Selector::PSEUDO;
                    trick_count = 1000000;
                    let j = if BUFFER[input.limit as usize] == get_int_par(IntPar::end_line_char) {
                        input.limit
                    } else {
                        input.limit + 1
                    };
                    if j > 0 {
                        for i in input.start.unwrap()..j {
                            if Some(i) == input.loc {
                                first_count = tally;
                                trick_count = tally + 1 + error_line - half_error_line;
                                if trick_count < error_line {
                                    trick_count = error_line
                                }
                            }
                            print_chr(std::char::from_u32(BUFFER[i as usize] as u32).unwrap());
                        }
                    }
                } else {
                    match input.index {
                        Btl::Parameter => t_print_nl!("<argument> "),
                        Btl::UTemplate | Btl::VTemplate => t_print_nl!("<template> "),
                        Btl::BackedUp | Btl::BackedUpChar => {
                            if input.loc.is_none() {
                                t_print_nl!("<recently read> ");
                            } else {
                                t_print_nl!("<to be read again> ");
                            }
                        }
                        Btl::Inserted => t_print_nl!("<inserted text> "),
                        Btl::Macro => {
                            print_ln();
                            t_print!("{}", Cs(input.name));
                        }
                        Btl::OutputText => t_print_nl!("<output> "),
                        Btl::EveryParText => t_print_nl!("<everypar> "),
                        Btl::EveryMathText => t_print_nl!("<everymath> "),
                        Btl::EveryDisplayText => t_print_nl!("<everydisplay> "),
                        Btl::EveryHBoxText => t_print_nl!("<everyhbox> "),
                        Btl::EveryVBoxText => t_print_nl!("<everyvbox> "),
                        Btl::EveryJobText => t_print_nl!("<everyjob> "),
                        Btl::EveryCRText => t_print_nl!("<everycr> "),
                        Btl::MarkText => t_print_nl!("<mark> "),
                        Btl::EveryEOFText => t_print_nl!("<everyeof> "),
                        Btl::InterCharText => t_print_nl!("<XeTeXinterchartoks> "),
                        Btl::WriteText => t_print_nl!("<write> "),
                        Btl::TectonicCodaText => t_print_nl!("<TectonicCodaTokens> "),
                    }
                    l = tally;
                    tally = 0;
                    selector = Selector::PSEUDO;
                    trick_count = 1_000_000;
                    if [
                        Btl::Parameter,
                        Btl::UTemplate,
                        Btl::VTemplate,
                        Btl::BackedUp,
                        Btl::BackedUpChar,
                        Btl::Inserted,
                    ]
                    .contains(&input.index)
                    {
                        show_token_list(input.start, input.loc, 100000);
                    } else {
                        show_token_list(MEM[input.start.unwrap()].b32.s1.opt(), input.loc, 100_000);
                    }
                }
                selector = old_setting;
                if trick_count == 1_000_000 {
                    first_count = tally;
                    trick_count = tally + 1 + error_line - half_error_line;
                    if trick_count < error_line {
                        trick_count = error_line
                    }
                }
                let m = if tally < trick_count {
                    tally - first_count
                } else {
                    trick_count - first_count
                };
                let p;
                let n;
                if l + first_count <= half_error_line {
                    p = 0;
                    n = l + first_count
                } else {
                    t_print!("...");
                    p = l + first_count - half_error_line + 3;
                    n = half_error_line
                }
                let nl = get_int_par(IntPar::new_line_char);
                set_int_par(IntPar::new_line_char, -1);
                for q in p..first_count {
                    print_chr(trick_buf[(q % error_line) as usize]);
                }
                print_ln();
                for _ in 0..n {
                    print_chr(' ');
                }
                let p = if m + n <= error_line {
                    first_count + m
                } else {
                    first_count + (error_line - n - 3)
                };
                for q in first_count..p {
                    print_chr(trick_buf[(q % error_line) as usize]);
                }
                set_int_par(IntPar::new_line_char, nl);
                if m + n > error_line {
                    t_print!("...");
                }
                nn += 1
            }
        } else if nn == get_int_par(IntPar::error_context_lines) {
            t_print_nl!("...");
            nn += 1
        }
        if bottom_line {
            break;
        }
        base_ptr -= 1
    }
}
pub(crate) unsafe fn begin_token_list(input: &mut input_state_t, popt: Option<usize>, t: Btl) {
    if INPUT_PTR > MAX_IN_STACK {
        MAX_IN_STACK = INPUT_PTR;
        if INPUT_PTR == STACK_SIZE {
            overflow("input stack size", STACK_SIZE);
        }
    }
    INPUT_STACK[INPUT_PTR] = *input; // push
    INPUT_PTR += 1;
    input.state = InputState::TokenList;
    input.start = popt;
    input.index = t;
    if ![
        Btl::Parameter,
        Btl::UTemplate,
        Btl::VTemplate,
        Btl::BackedUp,
        Btl::BackedUpChar,
        Btl::Inserted,
    ]
    .contains(&t)
    {
        let p = popt.unwrap();
        MEM[p].b32.s0 += 1;
        if t == Btl::Macro {
            input.limit = PARAM_PTR;
        } else {
            input.loc = llist_link(p);
            if get_int_par(IntPar::tracing_macros) > 1 {
                diagnostic(false, || {
                    t_print_nl!("");
                    match t {
                        Btl::MarkText => t_print!("{}", Esc("mark")),
                        Btl::WriteText => t_print!("{}", Esc("write")),
                        _ => {
                            t_print!(
                                "{}",
                                CmdChr(
                                    Cmd::AssignToks,
                                    (t as i32) + LOCAL_BASE as i32 + (Local::output_routine as i32)
                                        - (Btl::OutputText) as i32,
                                )
                            );
                        }
                    }
                    t_print!("->");
                    token_show(popt);
                });
            }
        }
    } else {
        input.loc = popt;
    };
}
pub(crate) unsafe fn end_token_list(input: &mut input_state_t) {
    if ![Btl::Parameter, Btl::UTemplate, Btl::VTemplate].contains(&input.index) {
        if [Btl::BackedUp, Btl::BackedUpChar, Btl::Inserted].contains(&input.index) {
            flush_list(input.start);
        } else {
            delete_token_ref(input.start.unwrap());
            if input.index == Btl::Macro {
                while PARAM_PTR > input.limit {
                    PARAM_PTR -= 1;
                    flush_list(PARAM_STACK[PARAM_PTR].opt());
                }
            }
        }
    } else if input.index == Btl::UTemplate {
        if align_state > 500000 {
            align_state = 0
        } else {
            fatal_error("(interwoven alignment preambles are not allowed)");
        }
    }
    INPUT_PTR -= 1;
    *input = INPUT_STACK[INPUT_PTR]; // pop
}
pub(crate) unsafe fn back_input(input: &mut input_state_t, tok: i32) {
    while input.state == InputState::TokenList
        && input.loc.is_none()
        && input.index != Btl::VTemplate
    {
        end_token_list(input);
    }
    let p = get_avail();
    MEM[p].b32.s0 = tok;
    if tok < RIGHT_BRACE_LIMIT {
        if tok < LEFT_BRACE_LIMIT {
            align_state -= 1
        } else {
            align_state += 1
        }
    }
    if INPUT_PTR > MAX_IN_STACK {
        MAX_IN_STACK = INPUT_PTR;
        if INPUT_PTR == STACK_SIZE {
            overflow("input stack size", STACK_SIZE);
        }
    }
    INPUT_STACK[INPUT_PTR] = *input; // push
    INPUT_PTR += 1;
    input.state = InputState::TokenList;
    input.start = Some(p);
    input.index = Btl::BackedUp;
    input.loc = Some(p);
}
pub(crate) unsafe fn back_error(input: &mut input_state_t, tok: i32) {
    back_input(input, tok);
    error();
}
pub(crate) unsafe fn ins_error(input: &mut input_state_t, tok: i32) {
    back_input(input, tok);
    input.index = Btl::Inserted;
    error();
}
pub(crate) unsafe fn begin_file_reading(input: &mut input_state_t) {
    if IN_OPEN == MAX_IN_OPEN {
        overflow("text input levels", MAX_IN_OPEN);
    }
    if first as usize == BUF_SIZE {
        overflow("buffer size", BUF_SIZE);
    }
    IN_OPEN += 1;
    if INPUT_PTR > MAX_IN_STACK {
        MAX_IN_STACK = INPUT_PTR;
        if INPUT_PTR == STACK_SIZE {
            overflow("input stack size", STACK_SIZE);
        }
    }
    INPUT_STACK[INPUT_PTR] = *input; // push
    INPUT_PTR += 1;
    input.index = Btl::from(IN_OPEN as u16);
    SOURCE_FILENAME_STACK[input.index as usize] = 0;
    FULL_SOURCE_FILENAME_STACK[input.index as usize] = 0;
    EOF_SEEN[input.index as usize] = false;
    GRP_STACK[input.index as usize] = cur_boundary;
    IF_STACK[input.index as usize] = cond_ptr;
    LINE_STACK[input.index as usize] = line;
    input.start = Some(first);
    input.state = InputState::MidLine;
    input.name = 0;
    input.synctex_tag = 0;
}
pub(crate) unsafe fn end_file_reading(input: &mut input_state_t) {
    first = input.start.unwrap();
    line = LINE_STACK[input.index as usize];
    if input.name == 18 || input.name == 19 {
        pseudo_close();
    } else if input.name > 17 {
        let _ = INPUT_FILE[input.index as usize].take();
    }
    INPUT_PTR -= 1;
    *input = INPUT_STACK[INPUT_PTR]; // pop
    IN_OPEN -= 1;
}

/// Before getting into |get_next|, let's consider the subroutine that
/// is called when an `\outer` control sequence has been scanned or
/// when the end of a file has been reached. These two cases are distinguished
/// by `cur_cs`, which is zero at the end of a file.
pub(crate) unsafe fn check_outer_validity(input: &mut input_state_t, cs: &mut i32) -> bool {
    let mut spacer = false;
    if scanner_status != ScannerStatus::Normal {
        deletions_allowed = false;
        // Back up an outer control sequence so that it can be reread
        if *cs != 0 {
            if input.state == InputState::TokenList || input.name < 1 || input.name > 17 {
                let p = get_avail();
                MEM[p].b32.s0 = CS_TOKEN_FLAG + *cs;
                // prepare to read the control sequence again
                begin_token_list(input, Some(p), Btl::BackedUp);
            }
            spacer = true;
        }
        if scanner_status != ScannerStatus::Normal && scanner_status != ScannerStatus::Skipping {
            /*350:*/
            // Tell the user what has run away...
            // print a definition, argument, or preamble
            runaway();
            if *cs == 0 {
                t_eprint!("File ended");
            // File ended while scanning...
            } else {
                *cs = 0;
                t_eprint!("Forbidden control sequence found");
                // Forbidden control sequence...
            }
            // Print either "definition" or "use" or "preamble" or "text",
            // and insert tokens that should lead to recovery
            //
            // The recovery procedure can't be fully understood without knowing more
            // about the `\TeX` routines that should be aborted, but we can sketch the
            // ideas here:  For a runaway definition we will insert a right brace; for a
            // runaway preamble, we will insert a special `\cr` token and a right
            // brace; and for a runaway argument, we will set `long_state` to
            // `outer_call` and insert `\par`.
            let mut p = get_avail();
            match scanner_status {
                ScannerStatus::Defining => {
                    t_print!(" while scanning definition");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32
                }
                ScannerStatus::Matching => {
                    t_print!(" while scanning use");
                    MEM[p].b32.s0 = par_token;
                    long_state = Cmd::OuterCall;
                }
                ScannerStatus::Aligning => {
                    t_print!(" while scanning preamble");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
                    let q = p;
                    p = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_CR as i32;
                    align_state = -1_000_000;
                }
                ScannerStatus::Absorbing => {
                    t_print!(" while scanning text");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32
                }
                _ => unreachable!(), // there are no other cases
            }
            begin_token_list(input, Some(p), Btl::Inserted);
            t_print!(" of {:#}", Cs(warning_index));
            help!(
                "I suspect you have forgotten a `}\', causing me",
                "to read past where you wanted me to stop.",
                "I\'ll try to recover; but if the error is serious,",
                "you\'d better type `E\' or `X\' now and fix your file."
            );
            error();
        } else {
            // Tell the user what has run away and try to recover
            t_eprint!(
                "Incomplete {}; all text was ignored after line {}",
                CmdChr(Cmd::IfTest, cur_if as i32),
                skip_line
            );
            let help2 = if *cs != 0 {
                *cs = 0;
                &"A forbidden control sequence occurred in skipped text."[..]
            } else {
                &"The file ended while I was skipping conditional text."[..]
            };
            help!(
                help2,
                "This kind of error happens when you say `\\if...\' and forget",
                "the matching `\\fi\'. I\'ve inserted a `\\fi\'; this might work."
            );
            let tok = CS_TOKEN_FLAG + FROZEN_FI as i32;
            ins_error(input, tok);
        }
        deletions_allowed = true
    }
    spacer
}

/// Getting the next token
///
/// The heart of \TeX's input mechanism is the `get_next` procedure, which
/// we shall develop in the next few sections of the program. Perhaps we
/// shouldn't actually call it the "heart", however, because it really acts
/// as \TeX's eyes and mouth, reading the source files and gobbling them up.
/// And it also helps \TeX to regurgitate stored token lists that are to be
/// processed again.
///
/// The main duty of `get_next` is to input one token and to set `cur_cmd`
/// and `cur_chr` to that token's command code and modifier. Furthermore, if
/// the input token is a control sequence, the `EQTB` location of that control
/// sequence is stored in `cur_cs`; otherwise `cur_cs` is set to zero.
///
/// Underlying this simple description is a certain amount of complexity
/// because of all the cases that need to be handled.
/// However, the inner loop of |get_next| is reasonably short and fast.
///
/// When `get_next` is asked to get the next token of a `\read` line,
/// it sets `cur_cmd=cur_chr=cur_cs=0` in the case that no more tokens
/// appear on that line. (There might not be any tokens at all, if the
/// `end_line_char` has `ignore` as its catcode.)
pub(crate) unsafe fn get_next(input: &mut input_state_t) -> (Cmd, i32, i32) {
    // go here to get the next input token
    'restart: loop {
        let mut ochr: i32;
        let mut ocmd: Cmd;
        let mut cs = 0;
        if input.state != InputState::TokenList {
            // Input from external file, goto `'restart` if no input found
            // 355
            // go here to eat the next character from a file
            'switch: loop {
                // 357:
                // current line not yet finished
                let loc = input.loc.as_mut().unwrap();
                if *loc <= input.limit {
                    let mut chr = BUFFER[*loc];
                    *loc += 1;
                    // go here to digest it again
                    'reswitch: loop {
                        ochr = chr;
                        let cmd = Cmd::from(*CAT_CODE(chr as usize) as u16);
                        ocmd = cmd;
                        // Change state if necessary, and goto `'switch` if the
                        // current character should be ignored,
                        // or goto `'reswitch` if the current character
                        // changes to another
                        match (input.state, cmd) {
                            (InputState::MidLine, IGNORE)
                            | (InputState::SkipBlanks, IGNORE)
                            | (InputState::NewLine, IGNORE)
                            | (InputState::SkipBlanks, Cmd::Spacer)
                            | (InputState::NewLine, Cmd::Spacer) => {
                                // Cases where character is ignored
                                break;
                            }
                            (InputState::MidLine, ESCAPE)
                            | (InputState::SkipBlanks, ESCAPE)
                            | (InputState::NewLine, ESCAPE) => {
                                // Scan a control sequence
                                // and set `state:=skip_blanks` or `mid_line`
                                if input.loc.unwrap() > input.limit {
                                    // `state` is irrelevant in this case
                                    cs = NULL_CS as i32;
                                } else {
                                    let mut k;
                                    let mut cat;
                                    let mut chr;
                                    // go here to start looking for a control sequence
                                    'start_cs: loop {
                                        k = input.loc.unwrap();
                                        chr = BUFFER[k as usize];
                                        cat = Cmd::from(*CAT_CODE(chr as usize) as u16);
                                        k += 1;
                                        input.state = match cat {
                                            Cmd::Letter | Cmd::Spacer => InputState::SkipBlanks,
                                            _ => InputState::MidLine,
                                        };
                                        if cat == Cmd::Letter && k <= input.limit {
                                            // Scan ahead in the buffer until finding a nonletter;
                                            // if an expanded code is encountered, reduce it
                                            // and goto `start_cs`; otherwise if a multiletter control
                                            // sequence is found, adjust `cs` and `loc`, and
                                            // goto `'found`
                                            loop
                                            /*368:*/
                                            {
                                                chr = BUFFER[k];
                                                cat = Cmd::from(*CAT_CODE(chr as usize) as u16);
                                                k += 1;
                                                if !(cat == Cmd::Letter && k <= input.limit) {
                                                    break;
                                                }
                                            }
                                            // If an expanded...
                                            if cat == Cmd::SupMark
                                                && BUFFER[k] == chr
                                                && k < input.limit
                                            {
                                                let mut sup_count = 2;
                                                // we have `^^` and another char; check how many `^`s we have altogether, up to a max of 6
                                                while sup_count < 6
                                                    && k + 2 * sup_count - 2 <= input.limit
                                                    && BUFFER[k + sup_count - 1] == chr
                                                {
                                                    sup_count += 1;
                                                }
                                                let sup_count_save = sup_count;
                                                // check whether we have enough hex chars for the number of `^`s
                                                for d in 1..=sup_count_save {
                                                    if !IS_LC_HEX(BUFFER[k + sup_count - 2 + d]) {
                                                        // found a non-hex char, so do single `^^X` style
                                                        let c = BUFFER[k + 1];
                                                        if c < 128 {
                                                            if c < 64 {
                                                                BUFFER[k - 1] = c + 64
                                                            } else {
                                                                BUFFER[k - 1] = c - 64
                                                            }
                                                            let d = 2;
                                                            input.limit -= d;
                                                            while k <= input.limit {
                                                                BUFFER[k] = BUFFER[k + d];
                                                                k += 1
                                                            }
                                                            continue 'start_cs;
                                                        } else {
                                                            sup_count = 0;
                                                        }
                                                    }
                                                }
                                                if sup_count > 0 {
                                                    // there were the right number of hex chars, so convert them
                                                    chr = 0;

                                                    for d in 1..=sup_count {
                                                        let c = BUFFER[k + sup_count - 2 + d];
                                                        chr = if c <= '9' as i32 {
                                                            16 * chr + c - '0' as i32
                                                        } else {
                                                            16 * chr + c - 'a' as i32 + 10
                                                        };
                                                    }

                                                    // check the resulting value is within the valid range
                                                    if chr > BIGGEST_USV as i32 {
                                                        //ochr = Some(BUFFER[k]);
                                                    } else {
                                                        BUFFER[k - 1] = chr;
                                                        let d = 2 * sup_count - 1;
                                                        // shift the rest of the buffer left by `d` chars
                                                        input.limit -= d;
                                                        while k <= input.limit {
                                                            BUFFER[k] = BUFFER[k + d];
                                                            k += 1
                                                        }
                                                        continue 'start_cs;
                                                    }
                                                }
                                            }
                                            // If an expanded...
                                            if cat != Cmd::Letter {
                                                k -= 1;
                                                // now `k` points to first nonletter
                                            }
                                            let loc = input.loc.as_mut().unwrap();
                                            if k > *loc + 1 {
                                                // multiletter control sequence has been scanned
                                                cs = id_lookup(*loc, k - *loc);
                                                *loc = k;
                                            } else {
                                                // If an expanded code is present, reduce it and goto `start_cs`>;
                                                // At this point, we have a single-character cs name in the buffer.
                                                // But if the character code is > 0xFFFF, we treat it like a multiletter name
                                                // for string purposes, because we use UTF-16 in the string pool.
                                                if BUFFER[*loc] as i64 > 0xffff {
                                                    cs = id_lookup(*loc, 1);
                                                    *loc += 1;
                                                } else {
                                                    cs = SINGLE_BASE as i32 + BUFFER[*loc];
                                                    *loc += 1;
                                                }
                                            }
                                        } else {
                                            if cat == Cmd::SupMark
                                                && BUFFER[k] == chr
                                                && k < input.limit
                                            {
                                                let mut sup_count = 2;
                                                while sup_count < 6
                                                    && k + 2 * sup_count - 2 <= input.limit
                                                    && BUFFER[k + sup_count - 1] == chr
                                                {
                                                    sup_count += 1
                                                }
                                                let sup_count_save_0 = sup_count;
                                                for d in 1..=sup_count_save_0 {
                                                    if !IS_LC_HEX(BUFFER[k + sup_count - 2 + d]) {
                                                        let c = BUFFER[k + 1];
                                                        if c < 128 {
                                                            if c < 64 {
                                                                BUFFER[k - 1] = c + 64
                                                            } else {
                                                                BUFFER[k - 1] = c - 64
                                                            }
                                                            let d = 2;
                                                            input.limit -= d;
                                                            while k <= input.limit {
                                                                BUFFER[k] = BUFFER[k + d];
                                                                k += 1
                                                            }
                                                            continue 'start_cs;
                                                        } else {
                                                            sup_count = 0;
                                                        }
                                                    }
                                                }
                                                if sup_count > 0 {
                                                    chr = 0;
                                                    for d in 1..=sup_count {
                                                        let c = BUFFER[k + sup_count - 2 + d];
                                                        if c <= '9' as i32 {
                                                            chr = 16 * chr + c - '0' as i32
                                                        } else {
                                                            chr = 16 * chr + c - 'a' as i32 + 10
                                                        }
                                                    }

                                                    if chr > BIGGEST_USV as i32 {
                                                        //ochr = Some(BUFFER[k]);
                                                    } else {
                                                        BUFFER[k - 1] = chr;
                                                        let d = 2 * sup_count - 1;
                                                        input.limit -= d;
                                                        while k <= input.limit {
                                                            BUFFER[k] = BUFFER[k + d];
                                                            k += 1
                                                        }
                                                        continue 'start_cs;
                                                    }
                                                }
                                            }
                                            // If an expanded code is present, reduce it and goto `start_cs`>;
                                            // At this point, we have a single-character cs name in the buffer.
                                            // But if the character code is > 0xFFFF, we treat it like a multiletter name
                                            // for string purposes, because we use UTF-16 in the string pool.
                                            let loc = input.loc.as_mut().unwrap();
                                            if BUFFER[*loc] as i64 > 0xffff {
                                                cs = id_lookup(*loc, 1);
                                                *loc += 1;
                                            } else {
                                                cs = SINGLE_BASE as i32 + BUFFER[*loc];
                                                *loc += 1;
                                            }
                                        }
                                        break;
                                    }
                                }
                                // found: go here when a control sequence has been found
                                let mut cmd = eq_type(cs as usize);
                                let mut chr = EQTB[cs as usize].val;
                                if cmd >= Cmd::OuterCall && check_outer_validity(input, &mut cs) {
                                    // replace it by a space
                                    cmd = Cmd::Spacer;
                                    chr = ' ' as i32;
                                }
                                ocmd = cmd;
                                ochr = chr;

                                // Whenever we reach the following piece of code, we will have
                                // `cur_chr=buffer[k-1]` and `k<=limit+1` and `cat=cat_code(cur_chr)`. If an
                                // expanded code like `^^A` or `^^df` appears in `buffer[(k-1)..(k+1)]`
                                // or `buffer[(k-1)..(k+2)]`, we
                                // will store the corresponding code in `buffer[k-1]` and shift the rest of
                                // the buffer left two or three places.
                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::ActiveChar)
                            | (InputState::SkipBlanks, Cmd::ActiveChar)
                            | (InputState::NewLine, Cmd::ActiveChar) => {
                                // Process an active-character control sequence
                                cs = chr + 1;
                                let mut cmd = eq_type(cs as usize);
                                let mut chr = EQTB[cs as usize].val;
                                input.state = InputState::MidLine;
                                if cmd >= Cmd::OuterCall && check_outer_validity(input, &mut cs) {
                                    // replace it by a space
                                    cmd = Cmd::Spacer;
                                    chr = ' ' as i32;
                                }
                                ocmd = cmd;
                                ochr = chr;

                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::SupMark)
                            | (InputState::SkipBlanks, Cmd::SupMark)
                            | (InputState::NewLine, Cmd::SupMark) => {
                                // If this `sup_mark` starts an expanded character
                                // like `^^A` or `^^df`, then goto `'reswitch`,
                                // otherwise set `state:=mid_line`
                                let limit = input.limit;
                                let loc = input.loc.as_mut().unwrap();
                                if chr == BUFFER[*loc] && *loc < limit {
                                    let mut sup_count = 2;
                                    // we have `^^` and another char; check how many `^`s we have altogether, up to a max of 6
                                    while sup_count < 6
                                        && *loc + 2 * sup_count - 2 <= limit
                                        && chr == BUFFER[*loc + sup_count - 1]
                                    {
                                        sup_count += 1
                                    }
                                    // check whether we have enough hex chars for the number of `^`s
                                    for d in 1..=sup_count {
                                        if !IS_LC_HEX(BUFFER[*loc + sup_count - 2 + d]) {
                                            // found a non-hex char, so do single `^^X` style
                                            let c = BUFFER[*loc + 1];
                                            if c >= 128 {
                                                ochr = chr;
                                                // not_exp: go here when `^^` turned out not to start an expanded code
                                                input.state = InputState::MidLine;
                                                break 'switch;
                                            }
                                            *loc += 2;
                                            chr = if c < 64 { c + 64 } else { c - 64 };
                                            continue 'reswitch;
                                        }
                                    }
                                    // there were the right number of hex chars, so convert them
                                    chr = 0;
                                    for d in 1..=sup_count {
                                        let c = BUFFER[*loc + sup_count - 2 + d];
                                        if c <= '9' as i32 {
                                            chr = 16 * chr + c - '0' as i32
                                        } else {
                                            chr = 16 * chr + c - 'a' as i32 + 10
                                        }
                                    }
                                    // check the resulting value is within the valid range
                                    if chr > BIGGEST_USV as i32 {
                                        ochr = BUFFER[*loc];
                                        // not_exp: go here when `^^` turned out not to start an expanded code
                                        input.state = InputState::MidLine;
                                        break 'switch;
                                    } else {
                                        *loc += 2 * sup_count - 1;
                                        continue 'reswitch;
                                    }
                                }
                                // not_exp: go here when `^^` turned out not to start an expanded code
                                input.state = InputState::MidLine;
                                break 'switch;
                            }
                            (InputState::MidLine, INVALID_CHAR)
                            | (InputState::SkipBlanks, INVALID_CHAR)
                            | (InputState::NewLine, INVALID_CHAR) => {
                                t_eprint!("Text line contains an invalid character");
                                help!(
                                    "A funny symbol that I can\'t read has just been input.",
                                    "Continue, and I\'ll forget that it ever happened."
                                );
                                deletions_allowed = false;
                                error();
                                deletions_allowed = true;
                                continue 'restart;
                            }
                            (InputState::MidLine, Cmd::Spacer) => {
                                input.state = InputState::SkipBlanks;
                                ochr = ' ' as i32;
                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::CarRet) => {
                                input.loc = Some(input.limit + 1);
                                ocmd = Cmd::Spacer;
                                ochr = ' ' as i32;
                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::Comment)
                            | (InputState::NewLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::CarRet) => {
                                input.loc = Some(input.limit + 1);
                                break;
                            }
                            (InputState::NewLine, Cmd::CarRet) => {
                                input.loc = Some(input.limit + 1);
                                cs = par_loc;
                                let mut cmd = eq_type(cs as usize);
                                let mut chr = EQTB[cs as usize].val;
                                if cmd >= Cmd::OuterCall && check_outer_validity(input, &mut cs) {
                                    // replace it by a space
                                    cmd = Cmd::Spacer;
                                    chr = ' ' as i32;
                                }
                                ocmd = cmd;
                                ochr = chr;

                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::LeftBrace) => {
                                align_state += 1;
                                break 'switch;
                            }
                            (InputState::SkipBlanks, Cmd::LeftBrace)
                            | (InputState::NewLine, Cmd::LeftBrace) => {
                                input.state = InputState::MidLine;
                                align_state += 1;
                                break 'switch;
                            }
                            (InputState::MidLine, Cmd::RightBrace) => {
                                align_state -= 1;
                                break 'switch;
                            }
                            (InputState::SkipBlanks, Cmd::RightBrace)
                            | (InputState::NewLine, Cmd::RightBrace) => {
                                input.state = InputState::MidLine;
                                align_state -= 1;
                                break 'switch;
                            }
                            (InputState::SkipBlanks, Cmd::MathShift)
                            | (InputState::SkipBlanks, Cmd::TabMark)
                            | (InputState::SkipBlanks, Cmd::MacParam)
                            | (InputState::SkipBlanks, Cmd::SubMark)
                            | (InputState::SkipBlanks, Cmd::Letter)
                            | (InputState::SkipBlanks, Cmd::OtherChar)
                            | (InputState::NewLine, Cmd::MathShift)
                            | (InputState::NewLine, Cmd::TabMark)
                            | (InputState::NewLine, Cmd::MacParam)
                            | (InputState::NewLine, Cmd::SubMark)
                            | (InputState::NewLine, Cmd::Letter)
                            | (InputState::NewLine, Cmd::OtherChar) => {
                                input.state = InputState::MidLine;
                                break 'switch;
                            }
                            _ => {
                                break 'switch;
                            }
                        }
                    }
                } else {
                    input.state = InputState::NewLine;
                    // Move to next line of file, or goto `'restart`...
                    if input.name > 17 {
                        /*374:*/
                        // Read next line of file into `buffer`, or goto `'restart` if the file has ended
                        line += 1; /*367:*/
                        first = input.start.unwrap();
                        if !force_eof {
                            if input.name <= 19 {
                                if pseudo_input(input) {
                                    // not end of file
                                    input.limit = last;
                                // this sets `limit`
                                } else if let Some(l) = LOCAL(Local::every_eof)
                                    .opt()
                                    .filter(|_| !EOF_SEEN[input.index as usize])
                                {
                                    input.limit = first - 1;
                                    EOF_SEEN[input.index as usize] = true; // fake one empty line
                                    begin_token_list(input, Some(l), Btl::EveryEOFText);
                                    continue 'restart;
                                } else {
                                    force_eof = true
                                }
                            } else if INPUT_FILE[input.index as usize]
                                .as_mut()
                                .unwrap()
                                .input_line()
                            {
                                // not end of file
                                input.limit = last;
                            // this sets `limit`
                            } else if let Some(l) = LOCAL(Local::every_eof)
                                .opt()
                                .filter(|_| !EOF_SEEN[input.index as usize])
                            {
                                input.limit = first - 1;
                                EOF_SEEN[input.index as usize] = true; // fake one empty line
                                begin_token_list(input, Some(l), Btl::EveryEOFText);
                                continue 'restart;
                            } else {
                                force_eof = true
                            }
                        }
                        if force_eof {
                            if get_int_par(IntPar::tracing_nesting) > 0
                                && (GRP_STACK[IN_OPEN] != cur_boundary
                                    || IF_STACK[IN_OPEN] != cond_ptr)
                            {
                                file_warning(input);
                                // give warning for some unfinished groups and/or conditionals
                            }
                            if input.name >= 19 {
                                print_chr(')');
                                open_parens -= 1;
                                rust_stdout.as_mut().unwrap().flush().unwrap();
                                // show user that file has been read
                            }
                            force_eof = false;
                            end_file_reading(input); // resume previous level
                            if check_outer_validity(input, &mut cs) {
                                // replace it by a space
                                //ocmd = Some(Cmd::Spacer);
                                //ochr = Some(' ' as i32);
                            }
                            continue 'restart;
                        } else {
                            if get_int_par(IntPar::end_line_char) < 0
                                || get_int_par(IntPar::end_line_char) > 255
                            {
                                input.limit -= 1
                            } else {
                                BUFFER[input.limit as usize] = get_int_par(IntPar::end_line_char)
                            }
                            first = (input.limit + 1) as usize;
                            input.loc = input.start;
                        }
                    } else {
                        if input.name != 0 {
                            // `\read` line has ended
                            return (Cmd::Relax, 0, cs);
                        }
                        if INPUT_PTR > 0 {
                            // text was inserted during error recovery
                            end_file_reading(input);
                            // resume previous level
                            continue 'restart;
                        }

                        /* Tectonic extension: we add a \TectonicCodaTokens toklist
                         * that gets inserted at the very very end of processing if no
                         * \end or \dump has been seen. We just use a global state
                         * variable to make sure it only gets inserted once. */
                        match (
                            used_tectonic_coda_tokens,
                            LOCAL(Local::TectonicCodaTokens).opt(),
                        ) {
                            (false, Some(l)) => {
                                used_tectonic_coda_tokens = true; /* token list but no tokens left */
                                begin_token_list(input, Some(l), Btl::TectonicCodaText);
                                continue 'restart;
                            }
                            _ => {
                                if matches!(selector, Selector::NO_PRINT | Selector::TERM_ONLY) {
                                    open_log_file();
                                }
                                fatal_error("*** (job aborted, no legal \\end found)");
                            }
                        }
                    }
                }
            }
        } else {
            // Let's consider now what happens when `get_next` is looking at a token list.

            // Input from token list, goto `'restart` if end of list or
            // if a parameter needs to be expanded
            if let Some(loc) = input.loc {
                // list not exhausted
                let t = *LLIST_info(loc);
                input.loc = llist_link(loc); // move to next
                if t >= CS_TOKEN_FLAG {
                    // a control sequence token
                    cs = t - CS_TOKEN_FLAG;
                    ocmd = eq_type(cs as usize);
                    ochr = EQTB[cs as usize].val;
                    if ocmd >= Cmd::OuterCall {
                        if ocmd == Cmd::DontExpand {
                            // 370:
                            // Get the next token, suppressing expansion
                            cs = MEM[input.loc.unwrap()].b32.s0 - CS_TOKEN_FLAG;
                            input.loc = None;
                            ocmd = eq_type(cs as usize);
                            ochr = EQTB[cs as usize].val;
                            if ocmd > MAX_COMMAND {
                                ocmd = Cmd::Relax;
                                ochr = NO_EXPAND_FLAG;
                            }
                        } else if check_outer_validity(input, &mut cs) {
                            // replace it by a space
                            ocmd = Cmd::Spacer;
                            ochr = ' ' as i32;
                        }
                    }
                } else {
                    ocmd = Cmd::from((t / MAX_CHAR_VAL) as u16);
                    ochr = t % MAX_CHAR_VAL;
                    match ocmd {
                        Cmd::LeftBrace => {
                            align_state += 1;
                        }
                        Cmd::RightBrace => {
                            align_state -= 1;
                        }
                        OUT_PARAM => {
                            // Insert macro parameter and goto `'restart`
                            begin_token_list(
                                input,
                                PARAM_STACK[input.limit + ochr as usize - 1].opt(),
                                Btl::Parameter,
                            );
                            continue;
                        }
                        _ => {}
                    }
                }
            } else {
                // we are done with this token list
                end_token_list(input);
                continue;
                // resume previous level
            }
        }
        // If an alignment entry has just ended, take appropriate action
        let mut cmd = ocmd;
        if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state == 0 {
            /*818:*/
            // Insert the `(v)<v_j>` template and goto `'restart`
            if scanner_status == ScannerStatus::Aligning {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
            if let Some(ca) = cur_align {
                // interwoven alignment preambles...
                let mut ca = Alignment(ca);
                cmd = Cmd::from(ca.extra_info() as u16);
                ca.set_extra_info(ochr);
                if cmd == Cmd::Omit {
                    begin_token_list(input, Some(OMIT_TEMPLATE), Btl::VTemplate);
                } else {
                    begin_token_list(input, ca.v_part().opt(), Btl::VTemplate);
                }
                align_state = 1_000_000;
            } else {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
        } else {
            return (cmd, ochr, cs);
        }
    }
}
pub(crate) unsafe fn get_token(input: &mut input_state_t) -> (i32, Cmd, i32, i32) {
    no_new_control_sequence = false;
    let (cmd, chr, cs) = get_next(input);
    no_new_control_sequence = true;
    let tok = if cs == 0 {
        cmd as i32 * MAX_CHAR_VAL + chr
    } else {
        CS_TOKEN_FLAG + cs
    };
    (tok, cmd, chr, cs)
}
/// invokes a user-defined control sequence
pub(crate) unsafe fn macro_call(input: &mut input_state_t, chr: i32, cs: i32) {
    let mut p: Option<usize> = None; // current node in parameter token list being built
    let mut rbrace_ptr: i32 = None.tex_int(); // one step before the last `right_brace` token
    let save_scanner_status = scanner_status; // `scanner_status` upon entry
    let save_warning_index = warning_index; // `warning_index` upon entry
    warning_index = cs;
    let ref_count = chr as usize; // start of the token list
    let mut r = llist_link(ref_count).unwrap(); // current node in the macro's token list
    let mut n = 0_i16; // the number of parameters scanned
    if get_int_par(IntPar::tracing_macros) > 0 {
        // Show the text of the macro being expanded
        diagnostic(false, || {
            print_ln();
            t_print!("{}", Cs(warning_index));
            token_show(Some(ref_count));
        });
    }
    if *LLIST_info(r) == PROTECTED_TOKEN {
        r = llist_link(r).unwrap();
    }
    if *LLIST_info(r) != END_MATCH_TOKEN {
        // Scan the parameters and make `link(r)` point to the macro body; but
        // return if an illegal `\par` is detected
        scanner_status = ScannerStatus::Matching;
        let mut unbalance = 0; // unmatched left braces in current parameter
        long_state = eq_type(cs as usize);

        long_state = match long_state {
            Cmd::OuterCall => Cmd::Call,
            Cmd::LongOuterCall => Cmd::LongCall,
            Cmd::Call | Cmd::LongCall => long_state,
            _ => unreachable!(),
        };

        let mut m = 0; // the number of tokens or groups (usually)
        let mut s = None; // backup pointer for parameter matching
        let mut cont = false;
        let mut match_chr: UTF16_code = 0; // character used in parameter
        's_135: loop {
            if !cont {
                *LLIST_link(TEMP_HEAD) = None.tex_int();
                if *LLIST_info(r) >= END_MATCH_TOKEN || *LLIST_info(r) < MATCH_TOKEN {
                    s = None;
                } else {
                    match_chr = (*LLIST_info(r) - MATCH_TOKEN) as UTF16_code;
                    s = llist_link(r);
                    r = s.unwrap();
                    p = Some(TEMP_HEAD);
                    m = 0;
                }
            }
            cont = false;

            // 'continue:
            let mut tok = get_token(input).0; // set `tok` to the next token of input
            if tok == *LLIST_info(r) {
                // Advance `(r)|r|`; goto `'found` if the parameter delimiter has been
                // fully matched, otherwise goto `'continue`
                r = llist_link(r).unwrap();
                if *LLIST_info(r) >= MATCH_TOKEN && *LLIST_info(r) <= END_MATCH_TOKEN {
                    if tok < LEFT_BRACE_LIMIT {
                        align_state -= 1
                    }
                } else {
                    cont = true;
                    continue;
                }
            } else {
                // Contribute the recently matched tokens to the current parameter, and
                // goto `'continue` if a partial match is still in effect;
                // but abort if `s=null`
                if s != Some(r) {
                    if let Some(s) = s {
                        let mut t = s; // cycle pointer for backup recovery
                        loop {
                            store_new_token(p.as_mut().unwrap(), *LLIST_info(t));
                            m += 1;
                            // auxiliary pointers for backup recovery
                            let mut u = llist_link(t).unwrap();
                            let mut v = s;
                            loop {
                                if u == r {
                                    if tok != MEM[v].b32.s0 {
                                        break;
                                    } else {
                                        r = llist_link(v).unwrap();
                                        cont = true;
                                        continue 's_135;
                                    }
                                }

                                if MEM[u].b32.s0 != MEM[v].b32.s0 {
                                    break;
                                }

                                u = llist_link(u).unwrap();
                                v = llist_link(v).unwrap();
                            }

                            // done:
                            t = llist_link(t).unwrap();
                            if t == r {
                                break;
                            }
                        }
                        r = s;
                    } else {
                        // Report an improper use of the macro and abort
                        t_eprint!(
                            "Use of {:#} doesn\'t match its definition",
                            Cs(warning_index)
                        );
                        help!(
                            "If you say, e.g., `\\def\\a1{...}\', then you must always",
                            "put `1\' after `\\a\', since control sequence names are",
                            "made up of letters only. The macro here has not been",
                            "followed by the required stuff, so I\'m ignoring it."
                        );
                        error();
                        return exit(save_scanner_status, save_warning_index);
                    }
                }

                if tok == par_token {
                    // Report a runaway argument and abort
                    if long_state != Cmd::LongCall {
                        /*414:*/
                        if long_state == Cmd::Call {
                            runaway(); /*411:*/
                            t_eprint!(
                                "Paragraph ended before {:#} was complete",
                                Cs(warning_index)
                            );
                            help!(
                                "I suspect you\'ve forgotten a `}\', causing me to apply this",
                                "control sequence to too much text. How can we recover?",
                                "My plan is to forget the whole thing and hope for the best."
                            );
                            back_error(input, tok);
                        }

                        pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                        align_state -= unbalance;

                        for m in pstack.iter().take((n as usize) + 1) {
                            flush_list(m.opt());
                        }

                        return exit(save_scanner_status, save_warning_index);
                    }
                }

                if tok < RIGHT_BRACE_LIMIT {
                    if tok < LEFT_BRACE_LIMIT {
                        // Contribute an entire group to the current parameter
                        unbalance = 1;

                        loop {
                            fast_store_new_token(p.as_mut().unwrap(), tok);

                            tok = get_token(input).0;

                            if tok == par_token {
                                // Report a runaway argument and abort
                                if long_state != Cmd::LongCall {
                                    /*414:*/
                                    if long_state == Cmd::Call {
                                        runaway();
                                        t_eprint!(
                                            "Paragraph ended before {:#} was complete",
                                            Cs(warning_index)
                                        );
                                        help!("I suspect you\'ve forgotten a `}\', causing me to apply this",
                                        "control sequence to too much text. How can we recover?",
                                        "My plan is to forget the whole thing and hope for the best.");
                                        back_error(input, tok);
                                    }

                                    pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                                    align_state -= unbalance;

                                    for m in pstack.iter().take((n as usize) + 1) {
                                        flush_list(m.opt());
                                    }

                                    return exit(save_scanner_status, save_warning_index);
                                }
                            }

                            if tok < RIGHT_BRACE_LIMIT {
                                if tok < LEFT_BRACE_LIMIT {
                                    unbalance += 1
                                } else {
                                    unbalance -= 1;
                                    if unbalance == 0 {
                                        break;
                                    }
                                }
                            }
                        }

                        // done1:
                        rbrace_ptr = p.tex_int();
                        store_new_token(p.as_mut().unwrap(), tok);
                    } else {
                        // Report an extra right brace and goto `'continue`
                        /* 413 */
                        back_input(input, tok);

                        t_eprint!("Argument of {:#} has an extra }}", Cs(warning_index));
                        help!(
                            "I\'ve run across a `}\' that doesn\'t seem to match anything.",
                            "For example, `\\def\\a#1{...}\' and `\\a}\' would produce",
                            "this error. If you simply proceed now, the `\\par\' that",
                            "I\'ve just inserted will cause me to report a runaway",
                            "argument that might be the root of the problem. But if",
                            "your `}\' was spurious, just type `2\' and it will go away."
                        );
                        align_state += 1;
                        long_state = Cmd::Call;
                        tok = par_token;
                        ins_error(input, tok);
                        cont = true;
                        continue;
                        // a white lie; the `\par` won't always trigger a runaway
                    }
                } else {
                    // Store the current token, but |goto continue| if it is
                    // a blank space that would become an undelimited parameter
                    if tok == SPACE_TOKEN
                        && *LLIST_info(r) <= END_MATCH_TOKEN
                        && *LLIST_info(r) >= MATCH_TOKEN
                    {
                        cont = true;
                        continue;
                    }

                    store_new_token(p.as_mut().unwrap(), tok);
                }

                m += 1;

                if *LLIST_info(r) > END_MATCH_TOKEN {
                    cont = true;
                    continue;
                }
                if *LLIST_info(r) < MATCH_TOKEN {
                    cont = true;
                    continue;
                }
            }

            // found:
            if s.is_some() {
                /*418:*/
                let p_ = p.unwrap();
                if m == 1 && MEM[p_].b32.s0 < RIGHT_BRACE_LIMIT && p_ != TEMP_HEAD {
                    *LLIST_link(rbrace_ptr as usize) = None.tex_int();
                    *LLIST_link(p_) = avail.tex_int();
                    avail = Some(p_);
                    p = llist_link(TEMP_HEAD);
                    let p_ = p.unwrap();
                    pstack[n as usize] = *LLIST_link(p_);
                    *LLIST_link(p_) = avail.tex_int();
                    avail = p;
                } else {
                    pstack[n as usize] = *LLIST_link(TEMP_HEAD)
                }
                n += 1;
                if get_int_par(IntPar::tracing_macros) > 0 {
                    diagnostic(false, || {
                        t_print_nl!("{}", PoolString::from(match_chr as str_number));
                        t_print!("{}<-", n as i32);
                        show_token_list(pstack[(n as i32 - 1) as usize].opt(), None, 1000);
                    });
                }
            }
            if MEM[r].b32.s0 == END_MATCH_TOKEN {
                break;
            }
        }
    }

    while input.state == InputState::TokenList
        && input.loc.is_none()
        && input.index != Btl::VTemplate
    {
        end_token_list(input);
    }

    begin_token_list(input, Some(ref_count), Btl::Macro);
    input.name = warning_index;
    input.loc = llist_link(r);

    if n > 0 {
        if PARAM_PTR + n as usize > MAX_PARAM_STACK {
            MAX_PARAM_STACK = PARAM_PTR + n as usize;
            if MAX_PARAM_STACK > PARAM_SIZE {
                overflow("parameter stack size", PARAM_SIZE);
            }
        }

        for m in 0..(n as usize) {
            PARAM_STACK[PARAM_PTR + m] = pstack[m];
        }
        PARAM_PTR += n as usize;
    }

    unsafe fn exit(save_scanner_status: ScannerStatus, save_warning_index: i32) {
        scanner_status = save_scanner_status;
        warning_index = save_warning_index;
    }

    exit(save_scanner_status, save_warning_index)
}
pub(crate) unsafe fn insert_relax(input: &mut input_state_t, cs: i32) {
    let tok = CS_TOKEN_FLAG + cs;
    back_input(input, tok);
    let tok = CS_TOKEN_FLAG + FROZEN_RELAX as i32;
    back_input(input, tok);
    input.index = Btl::Inserted;
}
pub(crate) unsafe fn new_index(i: u16, q: Option<usize>) -> usize {
    let mut p = Index(get_node(INDEX_NODE_SIZE));
    MEM[p.ptr()].b16.s1 = i;
    MEM[p.ptr()].b16.s0 = 0_u16;
    *LLIST_link(p.ptr()) = q.tex_int();
    for k in p.indexes_mut() {
        *k = None.tex_int();
    }
    p.ptr()
}
pub(crate) unsafe fn find_sa_element(t: ValLevel, n: i32, w: bool) {
    cur_ptr = sa_root[t as usize];
    if cur_ptr.is_none() {
        if w {
            return not_found(t, n);
        } else {
            return;
        }
    }
    let q = Index(cur_ptr.unwrap());
    let i = (n / 0x40000) as usize;
    cur_ptr = q.indexes()[i].opt();
    if cur_ptr.is_none() {
        if w {
            return lab46(t, n, q.ptr(), i);
        } else {
            return;
        }
    }
    let q = Index(cur_ptr.unwrap());
    let i = (n / 4096 % 64) as usize;
    cur_ptr = q.indexes()[i].opt();
    if cur_ptr.is_none() {
        if w {
            return lab47(t, n, q.ptr(), i);
        } else {
            return;
        }
    }
    let q = Index(cur_ptr.unwrap());
    let i = (n / 64 % 64) as usize;
    cur_ptr = q.indexes()[i].opt();
    if cur_ptr.is_none() {
        if w {
            return lab48(t, n, q.ptr(), i);
        } else {
            return;
        }
    }
    let q = Index(cur_ptr.unwrap());
    let i = (n % 64) as usize;
    cur_ptr = q.indexes()[i].opt();
    if cur_ptr.is_none() && w {
        return lab49(t, n, q.ptr(), i);
    } else {
        return;
    }

    unsafe fn not_found(t: ValLevel, n: i32) {
        let p = new_index(t as u16, None);
        cur_ptr = Some(p);
        sa_root[t as usize] = cur_ptr;
        let i = (n / 0x40000) as usize;
        lab46(t, n, p, i)
    }

    /*not_found1 */
    unsafe fn lab46(t: ValLevel, n: i32, q: usize, mut i: usize) {
        let p = new_index(i as u16, Some(q));
        cur_ptr = Some(p);
        let mut q = Index(q);
        q.indexes_mut()[i] = cur_ptr.tex_int();
        q.rc_inc();
        i = (n / 4096 % 64) as usize;
        lab47(t, n, p, i)
    }

    /*not_found2 */
    unsafe fn lab47(t: ValLevel, n: i32, q: usize, mut i: usize) {
        let p = new_index(i as u16, Some(q));
        cur_ptr = Some(p);
        let mut q = Index(q);
        q.indexes_mut()[i] = cur_ptr.tex_int();
        q.rc_inc();
        i = (n / 64 % 64) as usize;
        lab48(t, n, p, i)
    }

    /*not_found3 */
    unsafe fn lab48(t: ValLevel, n: i32, q: usize, mut i: usize) {
        let p = new_index(i as u16, Some(q));
        cur_ptr = Some(p);
        let mut q = Index(q);
        q.indexes_mut()[i] = cur_ptr.tex_int();
        q.rc_inc();
        i = (n % 64) as usize;
        lab49(t, n, p, i)
    }

    /*not_found4 *//*1608: */
    unsafe fn lab49(t: ValLevel, n: i32, q: usize, i: usize) {
        let p = match t {
            ValLevel::Mark => {
                let mut p = MarkClass(get_node(MARK_CLASS_NODE_SIZE));
                for k in p.indexes_mut() {
                    *k = None.tex_int();
                }
                p.ptr()
            }
            ValLevel::Int | ValLevel::Dimen => {
                let p = get_node(WORD_NODE_SIZE);
                MEM[p + 2].b32.s1 = 0;
                MEM[p + 1].b32.s1 = n;
                MEM[p + 1].b32.s0 = None.tex_int();
                p
            }
            ValLevel::Glue | ValLevel::Mu => {
                let p = get_node(POINTER_NODE_SIZE);
                MEM[p + 1].b32.s1 = ZERO_GLUE.ptr() as _;
                ZERO_GLUE.rc_inc();
                MEM[p + 1].b32.s0 = None.tex_int();
                p
            }
            _ => {
                let p = get_node(POINTER_NODE_SIZE);
                MEM[p + 1].b32.s1 = None.tex_int();
                MEM[p + 1].b32.s0 = None.tex_int();
                p
            }
        };
        cur_ptr = Some(p);
        MEM[p].b16.s1 = (64 * t as i32 + i as i32) as u16;
        MEM[p].b16.s0 = 1; /*level_one *//*:1608 */
        *LLIST_link(p) = Some(q).tex_int();
        let mut q = Index(q);
        q.indexes_mut()[i] = cur_ptr.tex_int();
        q.rc_inc();
    }
}
pub(crate) unsafe fn expand(input: &mut input_state_t, cmd: Cmd, chr: i32, cs: i32) {
    expand_depth_count += 1;
    if expand_depth_count >= expand_depth {
        overflow("expansion depth", expand_depth as usize);
    }
    let co_backup = cur_order;
    let backup_backup = *LLIST_link(BACKUP_HEAD);
    let mut ocmd = cmd;
    let mut ochr = chr;
    let mut ocs = cs;
    loop {
        if ocmd < Cmd::Call {
            /*384:*/
            if get_int_par(IntPar::tracing_commands) > 1 {
                show_cur_cmd_chr(ocmd, ochr); /*1612:*/
            }
            match ocmd {
                Cmd::TopBotMark => {
                    let t = (ochr % 5) as usize;
                    let val = if ochr >= 5 {
                        scan_register_num(input)
                    } else {
                        0
                    };
                    cur_ptr = if val == 0 {
                        cur_mark[t as usize]
                    } else {
                        find_sa_element(ValLevel::Mark, val, false);
                        cur_ptr.and_then(|p| MarkClass(p).indexes()[t].opt())
                    };
                    if let Some(p) = cur_ptr {
                        begin_token_list(input, Some(p), Btl::MarkText);
                    }
                    break;
                }
                Cmd::ExpandAfter => {
                    /*385:*/
                    if ochr == 0 {
                        let (t, _, _, _) = get_token(input);
                        /*1553: "\unless" implementation */
                        let (tok, cmd, chr, cs) = get_token(input);
                        if cmd > MAX_COMMAND {
                            expand(input, cmd, chr, cs);
                        } else {
                            back_input(input, tok);
                        }
                        back_input(input, t);
                        break;
                    } else {
                        let (tok, cmd, chr, cs) = get_token(input);
                        ocmd = cmd;
                        ocs = cs;
                        if cmd == Cmd::IfTest && chr != IfTestCode::IfCase as i32 {
                            ochr = chr + UNLESS_CODE
                        } else {
                            t_eprint!(
                                "You can\'t use `{}\' before `{}\'",
                                Esc("unless"),
                                CmdChr(cmd, chr)
                            );
                            help!("Continue, and I\'ll forget that it ever happened.");
                            back_error(input, tok);
                            break;
                        }
                    }
                }
                Cmd::NoExpand => {
                    /*386:*/
                    if ochr == 0 {
                        let save_scanner_status = scanner_status; /*387: \primitive implementation */
                        scanner_status = ScannerStatus::Normal;
                        let (t, ..) = get_token(input);
                        scanner_status = save_scanner_status;
                        back_input(input, t);
                        if t >= CS_TOKEN_FLAG {
                            let p = get_avail();
                            MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_DONT_EXPAND as i32;
                            *LLIST_link(p) = input.loc.tex_int();
                            input.start = Some(p);
                            input.loc = Some(p);
                        }
                        break;
                    } else {
                        let save_scanner_status = scanner_status;
                        scanner_status = ScannerStatus::Normal;
                        let (tok, _, _, mut cs) = get_token(input);
                        scanner_status = save_scanner_status;
                        cs = if cs < HASH_BASE as i32 {
                            prim_lookup(cs - SINGLE_BASE as i32) as i32
                        } else {
                            prim_lookup(yhash[cs as usize - hash_offset].s1) as i32
                        };
                        if cs == UNDEFINED_PRIMITIVE {
                            break;
                        }
                        let t = prim_eqtb[cs as usize].cmd as i32;
                        if t > MAX_COMMAND as i32 {
                            ocmd = Cmd::from(t as u16);
                            ochr = prim_eqtb[cs as usize].val;
                            //otok = ocmd as i32 * MAX_CHAR_VAL + ochr;
                            ocs = 0;
                        } else {
                            back_input(input, tok);
                            let p = get_avail();
                            MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_PRIMITIVE as i32;
                            *LLIST_link(p) = input.loc.tex_int();
                            input.loc = Some(p);
                            input.start = Some(p);
                            break;
                        }
                    }
                }
                Cmd::CSName => {
                    let r = get_avail();
                    let mut p = r; // head of the list of characters
                    let b = is_in_csname;
                    is_in_csname = true;
                    let (tok, cmd) = loop {
                        let (tok, cmd, _, cs) = get_x_token(input);
                        if cs == 0 {
                            store_new_token(&mut p, tok);
                        }
                        if cs != 0 {
                            break (tok, cmd);
                        }
                    };
                    if cmd != Cmd::EndCSName {
                        // Complain about missing `\endcsname`
                        t_eprint!("Missing {} inserted", Esc("endcsname"));
                        help!(
                            "The control sequence marked <to be read again> should",
                            "not appear between \\csname and \\endcsname."
                        );
                        back_error(input, tok);
                    }
                    // Look up the characters of list `r` in the hash table, and set `cs`
                    is_in_csname = b;
                    let mut j = first;
                    let mut popt = llist_link(r);
                    while let Some(p) = popt {
                        if j >= max_buf_stack {
                            max_buf_stack = j + 1;
                            if max_buf_stack as usize == BUF_SIZE {
                                overflow("buffer size", BUF_SIZE);
                            }
                            // TeX capacity exceeded buffer size
                        }
                        BUFFER[j as usize] = MEM[p].b32.s0 % MAX_CHAR_VAL;
                        j += 1;
                        popt = llist_link(p);
                    }
                    let cs;
                    if j > first + 1 || BUFFER[first as usize] as i64 > 65535 {
                        no_new_control_sequence = false;
                        cs = id_lookup(first as usize, (j - first) as usize);
                        no_new_control_sequence = true
                    } else if j == first {
                        cs = NULL_CS as i32; // the list is empty
                    } else {
                        cs = SINGLE_BASE as i32 + BUFFER[first as usize];
                        // the list has length one
                    }
                    flush_list(Some(r));
                    if eq_type(cs as usize) == Cmd::UndefinedCS {
                        eq_define(cs as usize, Cmd::Relax, Some(TOO_BIG_USV));
                        // N.B.: The `save_stack` might change
                    }
                    // the control sequence will now match `\relax`
                    let tok = cs + CS_TOKEN_FLAG;
                    back_input(input, tok);
                    break;
                }
                Cmd::Convert => {
                    conv_toks(input, ochr, ocs);
                    break;
                }
                Cmd::The => {
                    ins_the_toks(input, ochr, ocs);
                    break;
                }
                Cmd::IfTest => {
                    conditional(input, ocmd, ochr);
                    break;
                }
                Cmd::FiOrElse => {
                    if get_int_par(IntPar::tracing_ifs) > 0
                        && get_int_par(IntPar::tracing_commands) <= 1
                    {
                        show_cur_cmd_chr(ocmd, ochr);
                    }
                    if ochr > if_limit as i32 {
                        if if_limit == FiOrElseCode::If {
                            insert_relax(input, ocs);
                        } else {
                            t_eprint!("Extra {}", CmdChr(Cmd::FiOrElse, ochr));
                            help!("I\'m ignoring this; it doesn\'t match any \\if.");
                            error();
                        }
                    } else {
                        let mut chr = ochr;
                        while chr != FiOrElseCode::Fi as i32 {
                            chr = pass_text(input).1;
                        }
                        if IF_STACK[IN_OPEN] == cond_ptr {
                            INPUT_STACK[INPUT_PTR] = *input;
                            if_warning(input, &INPUT_STACK[..INPUT_PTR + 1]);
                        }
                        let p = cond_ptr.unwrap();
                        if_line = MEM[p + 1].b32.s1;
                        cur_if = MEM[p].b16.s0 as i16;
                        if_limit = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
                        cond_ptr = llist_link(p);
                        free_node(p, IF_NODE_SIZE);
                    }
                    break;
                }
                Cmd::Input => {
                    if ochr == 1 {
                        /* \endinput */
                        force_eof = true
                    } else if ochr == 2 {
                        /*1537:*/
                        /* \scantokens */
                        pseudo_start(input, ocs);
                    } else if name_in_progress {
                        insert_relax(input, ocs);
                    } else {
                        /* \input */
                        start_input(input, ""); /*393:*/
                    }
                    break;
                }
                _ => {
                    t_eprint!("Undefined control sequence");
                    help!(
                        "The control sequence at the end of the top line",
                        "of your error message was never \\def\'ed. If you have",
                        "misspelled it (e.g., `\\hobx\'), type `I\' and the correct",
                        "spelling (e.g., `I\\hbox\'). Otherwise just continue,",
                        "and I\'ll forget about whatever was undefined."
                    );
                    error();
                    break;
                }
            }
        } else {
            if ocmd < Cmd::EndTemplate {
                macro_call(input, ochr, ocs);
            } else {
                let tok = CS_TOKEN_FLAG + FROZEN_ENDV as i32;
                back_input(input, tok);
            }
            break;
        }
    }
    cur_order = co_backup;
    *LLIST_link(BACKUP_HEAD) = backup_backup;
    expand_depth_count -= 1;
}
pub(crate) unsafe fn get_x_token(input: &mut input_state_t) -> (i32, Cmd, i32, i32) {
    let mut cmd;
    let mut chr;
    let mut cs;
    loop {
        let next = get_next(input);
        cmd = next.0;
        chr = next.1;
        cs = next.2;
        if cmd > MAX_COMMAND {
            if cmd >= Cmd::Call {
                if cmd < Cmd::EndTemplate {
                    macro_call(input, chr, cs);
                } else {
                    cs = FROZEN_ENDV as i32;
                    cmd = Cmd::EndV;
                    break;
                }
            } else {
                let e = expand(input, cmd, chr, cs);
                e
            }
        } else {
            break;
        }
    }
    let tok = if cs == 0 {
        cmd as i32 * MAX_CHAR_VAL + chr
    } else {
        CS_TOKEN_FLAG + cs
    };

    (tok, cmd, chr, cs)
}
pub(crate) unsafe fn x_token(
    input: &mut input_state_t,
    cmd: &mut Cmd,
    chr: &mut i32,
    cs: &mut i32,
) -> i32 {
    while *cmd > MAX_COMMAND {
        expand(input, *cmd, *chr, *cs);
        let next = get_next(input);
        *cmd = next.0;
        *chr = next.1;
        *cs = next.2;
    }
    if *cs == 0 {
        *cmd as i32 * MAX_CHAR_VAL + *chr
    } else {
        CS_TOKEN_FLAG + *cs
    }
}
pub(crate) unsafe fn scan_left_brace(input: &mut input_state_t) -> (i32, Cmd, i32, i32) {
    let (mut tok, mut cmd, mut chr, cs) = loop {
        let next = get_x_token(input);
        if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
            break next;
        }
    };
    if cmd != Cmd::LeftBrace {
        t_eprint!("Missing {{ inserted");
        help!(
            "A left brace was mandatory here, so I\'ve put one in.",
            "You might want to delete and/or insert some corrections",
            "so that I will find a matching right brace soon.",
            "(If you\'re confused by all this, try typing `I}\' now.)"
        );
        back_error(input, tok);
        tok = LEFT_BRACE_TOKEN + '{' as i32;
        cmd = Cmd::LeftBrace;
        chr = '{' as i32;
        align_state += 1
    };
    (tok, cmd, chr, cs)
}
pub(crate) unsafe fn scan_optional_equals(input: &mut input_state_t) {
    let tok = loop {
        let (tok, cmd, ..) = get_x_token(input);
        if cmd != Cmd::Spacer {
            break tok;
        }
    };
    if tok != OTHER_TOKEN + 61 {
        /*"="*/
        back_input(input, tok);
    };
}

/// look for a given string
///
/// In case you are getting bored, here is a slightly less trivial routine:
/// Given a string of lowercase letters, like `pt` or `plus` or
/// `width`, the `scan_keyword` routine checks to see whether the next
/// tokens of input match this string. The match must be exact, except that
/// uppercase letters will match their lowercase counterparts; uppercase
/// equivalents are determined by subtracting `"a"-"A"`, rather than using the
/// `uc_code` table, since \TeX uses this routine only for its own limited
/// set of keywords.
///
/// If a match is found, the characters are effectively removed from the input
/// and `true` is returned. Otherwise `false` is returned, and the input
/// is left essentially unchanged (except for the fact that some macros
/// may have been expanded, etc.).
pub(crate) unsafe fn scan_keyword(input: &mut input_state_t, s: &str) -> bool {
    let s = s.as_bytes();
    let mut p = BACKUP_HEAD; // tail of the backup list
    *LLIST_link(p) = None.tex_int();
    if s.len() == 1 {
        let c: i8 = s[0] as i8;
        loop {
            let (tok, cmd, chr, cs) = get_x_token(input); // recursion is possible here
            if cs == 0 && (chr == c as i32 || chr == c as i32 - ('a' as i32) + ('A' as i32)) {
                store_new_token(&mut p, tok);
                flush_list(llist_link(BACKUP_HEAD));
                return true;
            } else if cmd != Cmd::Spacer || p != BACKUP_HEAD {
                back_input(input, tok);
                if p != BACKUP_HEAD {
                    begin_token_list(input, llist_link(BACKUP_HEAD), Btl::BackedUp);
                }
                return false;
            }
        }
    }
    let slen = s.len();
    let mut i = 0;
    while i < slen {
        let (tok, cmd, chr, cs) = get_x_token(input); // recursion is possible here
        if cs == 0
            && (chr == s[i] as i8 as i32 || chr == s[i] as i8 as i32 - ('a' as i32) + ('A' as i32))
        {
            store_new_token(&mut p, tok);
            i += 1;
        } else if cmd != Cmd::Spacer || p != BACKUP_HEAD {
            back_input(input, tok);
            if p != BACKUP_HEAD {
                begin_token_list(input, llist_link(BACKUP_HEAD), Btl::BackedUp);
            }
            return false;
        }
    }
    flush_list(llist_link(BACKUP_HEAD));
    true
}

/// Here is a procedure that sounds an alarm when mu and non-mu units
/// are being switched
pub(crate) unsafe fn mu_error() {
    t_eprint!("Incompatible glue units");
    help!("I\'m going to assume that 1mu=1pt when they\'re mixed.");
    error();
}
pub(crate) unsafe fn scan_glyph_number(input: &mut input_state_t, f: &NativeFont) -> i32 {
    if scan_keyword(input, "/") {
        let (file, ..) = scan_file_name(input);
        let fullname = file.to_string();
        map_glyph_to_index(f, &fullname)
    } else if scan_keyword(input, "u") {
        let val = scan_char_num(input);
        map_char_to_glyph(f, std::char::from_u32(val as u32).unwrap())
    } else {
        scan_int(input)
    }
}
pub(crate) unsafe fn scan_char_class(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > CHAR_CLASS_LIMIT {
        t_eprint!("Bad character class");
        help!(
            "A character class must be between 0 and 4096.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_char_class_not_ignored(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > CHAR_CLASS_LIMIT {
        t_eprint!("Bad character class");
        help!(
            "A class for inter-character transitions must be between 0 and 4095.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_eight_bit_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 255 {
        t_eprint!("Bad register code");
        help!(
            "A register code or char class must be between 0 and 255.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_usv_num(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > BIGGEST_USV as i32 {
        t_eprint!("Bad character code");
        help!(
            "A Unicode scalar value must be between 0 and \"10FFFF.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_char_num(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val >= TOO_BIG_CHAR {
        t_eprint!("Bad character code");
        help!(
            "A character number must be between 0 and 65535.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_xetex_math_char_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if math_char(val) == ACTIVE_MATH_CHAR as u32 {
        if val != ACTIVE_MATH_CHAR {
            t_eprint!("Bad active XeTeX math code");

            help!(
                "Since I ignore class and family for active math chars,",
                "I changed this one to \"1FFFFF."
            );
            int_error(val);
            ACTIVE_MATH_CHAR
        } else {
            val
        }
    } else if math_char(val) as u32 > BIGGEST_USV as u32 {
        t_eprint!("Bad XeTeX math character code");

        help!(
            "Since I expected a character number between 0 and \"10FFFF,",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_math(input: &mut input_state_t, m: &mut MCell, p: usize) {
    let c = 'c_118470: loop {
        let (tok, mut cmd, mut chr, _) = loop {
            /*422:*/
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
                break next;
            }
        };
        loop {
            match cmd {
                Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {
                    let c = *MATH_CODE(chr as usize);
                    if math_char(c) != ACTIVE_MATH_CHAR as u32 {
                        break 'c_118470 c;
                    }
                    let mut cs = chr + 1;
                    cmd = eq_type(cs as usize);
                    chr = EQTB[cs as usize].val;
                    let tok = x_token(input, &mut cmd, &mut chr, &mut cs);
                    back_input(input, tok);
                    break;
                }
                Cmd::CharNum => {
                    chr = scan_char_num(input);
                    cmd = Cmd::CharGiven;
                }
                Cmd::MathCharNum => {
                    let mut c;
                    if chr == 2 {
                        let val = scan_math_class_int(input);
                        c = set_class(val);
                        let val = scan_math_fam_int(input);
                        c += set_family(val);
                        let val = scan_usv_num(input);
                        c += val;
                    } else if chr == 1 {
                        let val = scan_xetex_math_char_int(input);
                        c = val
                    } else {
                        let val = scan_fifteen_bit_int(input);
                        c = set_class(val / 4096) + set_family((val % 4096) / 256) + (val % 256);
                    }
                    break 'c_118470 c;
                }
                Cmd::MathGiven => {
                    let c = set_class(chr / 4096) + set_family((chr % 4096) / 256) + (chr % 256);
                    break 'c_118470 c;
                }
                Cmd::XetexMathGiven => {
                    break 'c_118470 chr;
                }
                Cmd::DelimNum => {
                    let mut c;
                    if chr == 1 {
                        let val = scan_math_class_int(input);
                        c = set_class(val);
                        let val = scan_math_fam_int(input);
                        c += set_family(val);
                        let val = scan_usv_num(input);
                        c += val;
                    } else {
                        let val = scan_delimiter_int(input);
                        c = val / 4096;
                        c = set_class(c / 4096) + set_family((c % 4096) / 256) + (c % 256);
                    }
                    break 'c_118470 c;
                }
                _ => {
                    back_input(input, tok);
                    scan_left_brace(input);
                    SAVE_STACK[SAVE_PTR + 0].val = p as i32;
                    SAVE_PTR += 1;
                    push_math(GroupCode::Math);
                    return;
                }
            }
        }
    };
    m.typ = MathCell::MathChar;
    m.val.chr.character1 = (c as i64 % 65536) as u16;
    let cur_family = get_int_par(IntPar::cur_fam);
    let font =
        if (math_class(c) == 7) && (cur_family >= 0 && cur_family < NUMBER_MATH_FAMILIES as i32) {
            cur_family as u16
        } else {
            math_fam(c) as u16
        };
    m.val.chr.character2 = ((math_char(c) as u32) >> 16) as u8;
    m.val.chr.family = font as u8;
}
pub(crate) unsafe fn set_math_char(input: &mut input_state_t, chr: i32, c: i32) {
    if math_char(c) == ACTIVE_MATH_CHAR as u32 {
        /*1187: */
        let mut cs = chr + 1; /* ... "between 0 and 15" */
        let mut cmd = eq_type(cs as usize); /* ... "between 0 and 15" */
        let mut chr = EQTB[cs as usize].val;
        let tok = x_token(input, &mut cmd, &mut chr, &mut cs);
        back_input(input, tok);
    } else {
        let p = new_noad();
        MEM[p + 1].b32.s1 = MathCell::MathChar as _;
        let ch = math_char(c) as UnicodeScalar;
        MEM[p + 1].b16.s0 = (ch as i64 % 65536) as u16;
        MEM[p + 1].b16.s1 = math_fam(c) as u16;
        if math_class(c) == 7 {
            if get_int_par(IntPar::cur_fam) >= 0
                && get_int_par(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32
            {
                MEM[p + 1].b16.s1 = get_int_par(IntPar::cur_fam) as u16
            }
            MEM[p].b16.s1 = MathNode::Ord as u16;
        } else {
            MEM[p].b16.s1 = (MathNode::Ord as u32).wrapping_add(c as u32 >> 21 & 0x7_u32) as u16
        }
        MEM[p + 1].b16.s1 = (MEM[p + 1].b16.s1 as i64 + ch as i64 / 65536 * 256 as i64) as u16;
        *LLIST_link(cur_list.tail) = p as i32;
        cur_list.tail = p;
    };
}
pub(crate) unsafe fn scan_math_class_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 7 {
        t_eprint!("Bad math class");
        help!(
            "Since I expected to read a number between 0 and 7,",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_math_fam_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > NUMBER_MATH_FAMILIES as i32 - 1 {
        t_eprint!("Bad math family");
        help!(
            "Since I expected to read a number between 0 and 255,",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_four_bit_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 15 {
        t_eprint!("Bad number");
        help!(
            "Since I expected to read a number between 0 and 15,",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_fifteen_bit_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 32767 {
        t_eprint!("Bad mathchar");
        help!(
            "A mathchar number must be between 0 and 32767.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_delimiter_int(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 0x7ffffff {
        t_eprint!("Bad delimiter code");
        help!(
            "A numeric delimiter code must be between 0 and 2^{27}-1.",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_register_num(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > max_reg_num {
        t_eprint!("Bad register code");
        help!(max_reg_help_line, "I changed this one to zero.");
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn scan_four_bit_int_or_18(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > 15 && val != 18 {
        t_eprint!("Bad number");
        help!(
            "Since I expected to read a number between 0 and 15,",
            "I changed this one to zero."
        );
        int_error(val);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn get_x_or_protected(input: &mut input_state_t) -> (i32, Cmd, i32) {
    let mut tok;
    let mut cmd;
    let mut chr;
    loop {
        let next = get_token(input);
        tok = next.0;
        cmd = next.1;
        chr = next.2;
        let cs = next.3;
        if cmd <= MAX_COMMAND {
            return (tok, cmd, chr);
        }
        if cmd >= Cmd::Call
            && cmd < Cmd::EndTemplate
            && MEM[*LLIST_link(chr as usize) as usize].b32.s0 == PROTECTED_TOKEN
        {
            return (tok, cmd, chr);
        }
        expand(input, cmd, chr, cs);
    }
}
pub(crate) unsafe fn effective_char(mut _err_p: bool, f: internal_font_number, mut c: u16) -> i32 {
    if !xtx_ligature_present && !(FONT_MAPPING[f]).is_null() {
        c = apply_tfm_font_mapping(FONT_MAPPING[f], c as i32) as u16
    }
    xtx_ligature_present = false;
    c as i32
}
pub(crate) unsafe fn scan_font_ident(input: &mut input_state_t) -> i32 {
    let (tok, cmd, chr, ..) = loop {
        let next = get_x_token(input);
        if !(next.1 == Cmd::Spacer) {
            break next;
        }
    };
    let f = match cmd {
        Cmd::DefFont => EQTB[CUR_FONT_LOC].val as usize,
        Cmd::SetFont => chr as usize,
        Cmd::DefFamily => {
            let m = chr;
            let val = scan_math_fam_int(input);
            EQTB[(m + val) as usize].val as usize
        }
        _ => {
            t_eprint!("Missing font identifier");
            help!(
                "I was looking for a control sequence whose",
                "current meaning has been defined by \\font."
            );
            back_error(input, tok);
            FONT_BASE
        }
    };
    f as i32
}
pub(crate) unsafe fn find_font_dimen(input: &mut input_state_t, writing: bool) -> i32 {
    let n = scan_int(input);
    let val = scan_font_ident(input);
    let f = val as usize;
    let val = if n <= 0 {
        fmem_ptr
    } else {
        if writing && n <= SPACE_SHRINK_CODE && n >= SPACE_CODE {
            if let Some(g) = FONT_GLUE[f].opt() {
                delete_glue_ref(g);
                FONT_GLUE[f] = None.tex_int()
            }
        }
        if n > FONT_PARAMS[f] {
            if f < FONT_PTR {
                fmem_ptr
            } else {
                loop
                /*599: */
                {
                    if fmem_ptr == FONT_MEM_SIZE as i32 {
                        overflow("font memory", FONT_MEM_SIZE);
                    }
                    FONT_INFO[fmem_ptr as usize].b32.s1 = 0;
                    fmem_ptr += 1;
                    FONT_PARAMS[f] += 1;
                    if n == FONT_PARAMS[f] {
                        break;
                    }
                }
                fmem_ptr - 1
            }
        } else {
            n + PARAM_BASE[f]
        }
    };
    if val == fmem_ptr {
        t_eprint!(
            "Font {} has only {} fontdimen parameters",
            Esc(&PoolString::from(yhash[FROZEN_NULL_FONT + f - hash_offset].s1).to_string()),
            FONT_PARAMS[f]
        );
        help!(
            "To increase the number of font parameters, you must",
            "use \\fontdimen immediately after the \\font is loaded."
        );
        error();
    };
    val
}
unsafe fn restart_scan_something_internal(
    input: &mut input_state_t,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    level: ValLevel,
    negative: bool,
) -> (bool, i32, ValLevel) {
    match cmd {
        Cmd::DefCode => {
            let m = chr;
            let val = scan_usv_num(input);
            let val = if m == MATH_CODE_BASE as i32 {
                let mut val1 = *MATH_CODE(val as usize);
                if math_char(val1) == ACTIVE_MATH_CHAR as u32 {
                    val1 = 0x8000;
                } else if math_class(val1) > 7 || math_fam(val1) > 15 || math_char(val1) > 255 {
                    t_eprint!("Extended mathchar used as mathchar");
                    help!(
                        "A mathchar number must be between 0 and \"7FFF.",
                        "I changed this one to zero."
                    );
                    int_error(val1);
                    val1 = 0;
                }
                (math_class(val1) * 0x1000 + math_fam(val1) * 0x100 + math_char(val1)) as i32
            } else if m == DEL_CODE_BASE as i32 {
                let val1 = EQTB[DEL_CODE_BASE + val as usize].val;
                if val1 >= 0x40000000 {
                    t_eprint!("Extended delcode used as delcode");
                    help!(
                        "I can only go up to 2147483647=\'17777777777=\"7FFFFFFF,",
                        "I changed this one to zero."
                    );
                    error();
                    0
                } else {
                    val1
                }
            } else if m < SF_CODE_BASE as i32 {
                EQTB[(m + val) as usize].val
            } else if m < MATH_CODE_BASE as i32 {
                (EQTB[(m + val) as usize].val as i64 % 65536) as i32
            } else {
                EQTB[(m + val) as usize].val
            };
            (true, val, ValLevel::Int)
        }
        Cmd::XetexDefCode => {
            let m = chr;
            let val = scan_usv_num(input);
            let val = if m == SF_CODE_BASE as i32 {
                (*SF_CODE(val as usize) as i64 / 65536) as i32
            } else if m == MATH_CODE_BASE as i32 {
                *MATH_CODE(val as usize)
            } else if m == MATH_CODE_BASE as i32 + 1 {
                t_eprint!("Can\'t use \\Umathcode as a number (try \\Umathcodenum)");
                help!(
                    "\\Umathcode is for setting a mathcode from separate values;",
                    "use \\Umathcodenum to access them as single values."
                );
                error();
                0
            } else if m == DEL_CODE_BASE as i32 {
                EQTB[DEL_CODE_BASE + val as usize].val
            } else {
                t_eprint!("Can\'t use \\Udelcode as a number (try \\Udelcodenum)");
                help!(
                    "\\Udelcode is for setting a delcode from separate values;",
                    "use \\Udelcodenum to access them as single values."
                );
                error();
                0
            };
            (true, val, ValLevel::Int)
        }
        Cmd::ToksRegister | Cmd::AssignToks | Cmd::DefFamily | Cmd::SetFont | Cmd::DefFont => {
            let m = chr;
            if level != ValLevel::Tok {
                t_eprint!("Missing number, treated as zero");
                help!(
                    "A number should have been here; I inserted `0\'.",
                    "(If you can\'t figure out why I needed to see a number,",
                    "look up `weird error\' in the index to The TeXbook.)"
                );
                back_error(input, tok);
                (true, 0, ValLevel::Dimen)
            } else if cmd <= Cmd::AssignToks {
                let val = if cmd < Cmd::AssignToks {
                    if m == 0 {
                        let val = scan_register_num(input);
                        if val < 256 {
                            EQTB[TOKS_BASE + val as usize].val
                        } else {
                            find_sa_element(ValLevel::Tok, val, false);
                            cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt()).tex_int()
                        }
                    } else {
                        MEM[(m + 1) as usize].b32.s1
                    }
                } else if chr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                    cur_ptr = scan_char_class_not_ignored(input).opt();
                    let val = scan_char_class_not_ignored(input);
                    find_sa_element(
                        ValLevel::InterChar,
                        cur_ptr.tex_int() * CHAR_CLASS_LIMIT + val,
                        false,
                    );
                    if let Some(p) = cur_ptr {
                        MEM[p + 1].b32.s1
                    } else {
                        None.tex_int()
                    }
                } else {
                    EQTB[m as usize].val
                };
                (true, val, ValLevel::Tok)
            } else {
                back_input(input, tok);
                let val = scan_font_ident(input);
                (true, FONT_ID_BASE as i32 + val, ValLevel::Ident)
            }
        }
        Cmd::AssignInt => {
            let m = chr;
            (true, EQTB[m as usize].val, ValLevel::Int)
        }
        Cmd::AssignDimen => {
            let m = chr;
            (true, EQTB[m as usize].val, ValLevel::Dimen)
        }
        Cmd::AssignGlue => {
            let m = chr;
            (true, EQTB[m as usize].val, ValLevel::Glue)
        }
        Cmd::AssignMuGlue => {
            let m = chr;
            (true, EQTB[m as usize].val, ValLevel::Mu)
        }
        Cmd::SetAux => {
            let m = chr;
            if cur_list.mode.1 as i32 != m {
                t_eprint!("Improper {}", CmdChr(Cmd::SetAux, m));
                help!(
                    "You can refer to \\spacefactor only in horizontal mode;",
                    "you can refer to \\prevdepth only in vertical mode; and",
                    "neither of these is meaningful inside \\write. So",
                    "I\'m forgetting what you said and using zero instead."
                );
                error();
                if level != ValLevel::Tok {
                    (true, 0, ValLevel::Dimen)
                } else {
                    (true, 0, ValLevel::Int)
                }
            } else if m == ListMode::VMode as i32 {
                (true, cur_list.aux.b32.s1, ValLevel::Dimen)
            } else {
                (true, cur_list.aux.b32.s0, ValLevel::Int)
            }
        }
        Cmd::SetPrevGraf => {
            if cur_list.mode.1 == ListMode::NoMode {
                (true, 0, ValLevel::Int)
            } else {
                NEST[NEST_PTR] = cur_list;
                let mut p = NEST_PTR;
                while NEST[p].mode.1 != ListMode::VMode {
                    p -= 1
                }
                (true, NEST[p].prev_graf, ValLevel::Int)
            }
        }
        Cmd::SetPageInt => {
            let m = chr;
            let val = if m == 0 {
                dead_cycles
            } else if m == 2 {
                interaction as i32
            } else {
                insert_penalties
            };
            (true, val, ValLevel::Int)
        }
        Cmd::SetPageDimen => {
            let m = chr;
            let val = if page_contents == PageContents::Empty && !output_active {
                if m == 0 {
                    Scaled::MAX_HALFWORD
                } else {
                    Scaled::ZERO
                }
            } else {
                page_so_far[m as usize]
            };
            (true, val.0, ValLevel::Dimen)
        }
        Cmd::SetShape => {
            let m = chr;
            let val = if m > LOCAL_BASE as i32 + Local::par_shape as i32 {
                /*1654:*/
                let mut val = scan_int(input);
                if val < 0 {
                    0
                } else if let Some(v) = EQTB[m as usize].val.opt() {
                    if val > MEM[v + 1].b32.s1 {
                        val = MEM[v + 1].b32.s1
                    }
                    MEM[v + val as usize + 1].b32.s1
                } else {
                    0
                }
            } else if let Some(l) = LOCAL(Local::par_shape).opt() {
                MEM[l].b32.s0
            } else {
                0
            };
            (true, val, ValLevel::Int)
        }
        Cmd::SetBoxDimen => {
            let m = chr;
            let val = scan_register_num(input);
            let q = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                if let Some(p) = cur_ptr {
                    MEM[p + 1].b32.s1.opt()
                } else {
                    None
                }
            };
            let val = if let Some(q) = q {
                MEM[q + (m as usize)].b32.s1
            } else {
                0
            };
            (true, val, ValLevel::Dimen)
        }
        Cmd::CharGiven | Cmd::MathGiven => (true, chr, ValLevel::Int),
        Cmd::AssignFontDimen => {
            let val = find_font_dimen(input, false);
            FONT_INFO[fmem_ptr as usize].b32.s1 = 0;
            (true, FONT_INFO[val as usize].b32.s1, ValLevel::Dimen)
        }
        Cmd::AssignFontInt => {
            let m = AssignFontInt::from(chr);
            let val = scan_font_ident(input);
            match m {
                AssignFontInt::HyphenChar => (true, HYPHEN_CHAR[val as usize], ValLevel::Int),
                AssignFontInt::SkewChar => (true, SKEW_CHAR[val as usize], ValLevel::Int),
                _ => {
                    let n = val;
                    let k = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[n as usize] {
                        scan_glyph_number(input, nf)
                    } else {
                        scan_char_num(input)
                    };
                    match m {
                        AssignFontInt::LpCode => (
                            true,
                            get_cp_code(n as usize, k as u32, Side::Left),
                            ValLevel::Int,
                        ),
                        AssignFontInt::RpCode => (
                            true,
                            get_cp_code(n as usize, k as u32, Side::Right),
                            ValLevel::Int,
                        ),
                        _ => unreachable!(),
                    }
                }
            }
        }
        Cmd::Register => {
            let m = chr;
            if m < 0 || m > LO_MEM_STAT_MAX {
                // TODO: may be bug
                let val_level = ValLevel::from((MEM[m as usize].b16.s1 as i32 / 64) as u8);
                let val = match val_level {
                    ValLevel::Int | ValLevel::Dimen => MEM[(m + 2) as usize].b32.s1,
                    _ => MEM[(m + 1) as usize].b32.s1,
                };
                (true, val, val_level)
            } else {
                let val = scan_register_num(input);
                let val_level = ValLevel::from(m as u8);
                let val = if val > 255 {
                    find_sa_element(val_level, val, false);
                    if let Some(p) = cur_ptr {
                        match val_level {
                            ValLevel::Int | ValLevel::Dimen => MEM[p + 2].b32.s1,
                            _ => MEM[p + 1].b32.s1,
                        }
                    } else {
                        0
                    }
                } else {
                    match val_level {
                        ValLevel::Int => get_count_reg(val as usize),
                        ValLevel::Dimen => get_scaled_reg(val as usize).0,
                        ValLevel::Glue => *SKIP_REG(val as usize),
                        ValLevel::Mu => *MU_SKIP_REG(val as usize),
                        _ => val,
                    }
                };
                (true, val, val_level)
            }
        }
        Cmd::LastItem => {
            let m = LastItemCode::n(chr as u8).unwrap();
            if m >= LastItemCode::InputLineNo {
                if m >= LastItemCode::MuToGlue {
                    /*1568:*/
                    let (mut val, mut val_level) = match m {
                        LastItemCode::MuToGlue => {
                            let val = scan_mu_glue(input).ptr() as i32; // 1595:
                            (val, ValLevel::Glue)
                        }
                        LastItemCode::GlueToMu => {
                            let val = scan_normal_glue(input).ptr() as i32; // 1596:
                            (val, ValLevel::Mu)
                        }
                        _ => {
                            let mut val_level = match m {
                                LastItemCode::EtexExprInt => ValLevel::Int,
                                LastItemCode::EtexExprDimen => ValLevel::Dimen,
                                LastItemCode::EtexExprGlue => ValLevel::Glue,
                                LastItemCode::EtexExprMu => ValLevel::Mu,
                                _ => unreachable!(),
                            };
                            let val = scan_expr(input, &mut val_level);
                            (val, val_level)
                        }
                    };
                    while val_level > level {
                        if val_level == ValLevel::Glue {
                            let m = val as usize;
                            val = MEM[m + 1].b32.s1;
                            delete_glue_ref(m);
                        } else if val_level == ValLevel::Mu {
                            mu_error();
                        }
                        val_level.prev();
                    }
                    if negative {
                        match val_level {
                            ValLevel::Int | ValLevel::Dimen => val = -val,
                            _ => {
                                let m = GlueSpec(val as usize);
                                val = (-&m).ptr() as i32;
                                delete_glue_ref(m.ptr());
                            }
                        }
                    }
                    return (false, val, val_level);
                }
                if m >= XETEX_DIM {
                    let val = match m {
                        LastItemCode::XetexGlyphBounds => {
                            /*1435:*/
                            if let Font::Native(_) =
                                &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize]
                            {
                                let n = scan_int(input); /* shellenabledp */
                                if n < 1 || n > 4 {
                                    t_eprint!(
                                        "\\\\XeTeXglyphbounds requires an edge index from 1 to 4;"
                                    );
                                    t_print_nl!("I don\'t know anything about edge {}", n);
                                    error();
                                    0
                                } else {
                                    let val = scan_int(input);
                                    get_glyph_bounds(EQTB[CUR_FONT_LOC].val as usize, n, val).0
                                }
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                0
                            }
                        }
                        LastItemCode::FontCharWd
                        | LastItemCode::FontCharHt
                        | LastItemCode::FontCharDp
                        | LastItemCode::FontCharIc => {
                            let q = scan_font_ident(input) as usize;
                            let val = scan_usv_num(input);
                            (if let Font::Native(nq) = &FONT_LAYOUT_ENGINE[q] {
                                let val = std::char::from_u32(val as u32).unwrap();
                                match m {
                                    LastItemCode::FontCharWd => getnativecharwd(q, val),
                                    LastItemCode::FontCharHt => getnativecharht(q, val),
                                    LastItemCode::FontCharDp => getnativechardp(q, val),
                                    LastItemCode::FontCharIc => {
                                        getnativecharic(nq, FONT_LETTER_SPACE[q], val)
                                    }
                                    _ => unreachable!(),
                                }
                            } else if FONT_BC[q] as i32 <= val && FONT_EC[q] as i32 >= val {
                                let i = FONT_CHARACTER_INFO(
                                    q,
                                    effective_char(true, q, val as u16) as usize,
                                );
                                match m {
                                    LastItemCode::FontCharWd => *FONT_CHARINFO_WIDTH(q, i),
                                    LastItemCode::FontCharHt => *FONT_CHARINFO_HEIGHT(q, i),
                                    LastItemCode::FontCharDp => *FONT_CHARINFO_DEPTH(q, i),
                                    LastItemCode::FontCharIc => *FONT_CHARINFO_ITALCORR(q, i),
                                    _ => unreachable!(),
                                }
                            } else {
                                Scaled::ZERO
                            })
                            .0
                        }
                        LastItemCode::ParShapeLength
                        | LastItemCode::ParShapeIndent
                        | LastItemCode::ParShapeDimen => {
                            let mut q = chr - (LastItemCode::ParShapeLength as i32);
                            let mut val = scan_int(input);
                            if val <= 0 {
                                0
                            } else if let Some(l) = LOCAL(Local::par_shape).opt() {
                                if q == 2 {
                                    q = val % 2;
                                    val = (val + q) / 2;
                                }
                                if val > MEM[l].b32.s0 {
                                    val = MEM[l].b32.s0
                                }
                                MEM[l + 2 * (val as usize) - (q as usize)].b32.s1
                            } else {
                                0
                            }
                        }
                        LastItemCode::GlueStretch | LastItemCode::GlueShrink => {
                            let q = scan_normal_glue(input);
                            let val = if m == LastItemCode::GlueStretch {
                                q.stretch()
                            } else {
                                q.shrink()
                            }
                            .0;
                            delete_glue_ref(q.ptr());
                            val
                        }
                        _ => unreachable!(),
                    };
                    (true, val, ValLevel::Dimen)
                } else {
                    let val = match m {
                        LastItemCode::InputLineNo => line,
                        LastItemCode::Badness => last_badness,
                        LastItemCode::PdfShellEscape => 0,
                        LastItemCode::EtexVersion => ETEX_VERSION,
                        LastItemCode::XetexVersion => XETEX_VERSION,
                        LastItemCode::XetexCountGlyphs => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => aat::aat_font_get(m.into(), e.attributes),
                                Font::Native(Otgr(e)) => ot_font_get((m as i32) - 14, e),
                                _ => 0,
                            }
                        }
                        LastItemCode::XetexCountFeatures => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => aat::aat_font_get(m.into(), e.attributes),
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    ot_font_get((m as i32) - 14, e)
                                }
                                _ => 0,
                            }
                        }
                        LastItemCode::XetexVariation
                        | LastItemCode::XetexVariationMin
                        | LastItemCode::XetexVariationMax
                        | LastItemCode::XetexVariationDefault
                        | LastItemCode::XetexCountVariations => {
                            let _n = scan_font_ident(input);
                            0
                        }
                        LastItemCode::XetexFeatureCode
                        | LastItemCode::XetexIsExclusiveFeature
                        | LastItemCode::XetexCountSelectors => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => {
                                    let k = scan_int(input);
                                    aat::aat_font_get_1(m.into(), e.attributes, k)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    let k = scan_int(input);
                                    ot_font_get_1((m as i32) - 14, e, k)
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexSelectorCode | LastItemCode::XetexIsDefaultSelector => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => {
                                    let k = scan_int(input);
                                    let val = scan_int(input);
                                    aat::aat_font_get_2(m.into(), e.attributes, k, val)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    let k = scan_int(input);
                                    let val = scan_int(input);
                                    ot_font_get_2((m as i32) - 14, e, k, val)
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexFindVariationByName => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => {
                                    let name = scan_file_name(input).0.to_string();
                                    aat::aat_font_get_named(&name, m.into(), e.attributes)
                                }
                                _ => {
                                    not_aat_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexFindFeatureByName => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => {
                                    let name = scan_file_name(input).0.to_string();
                                    aat::aat_font_get_named(&name, m.into(), e.attributes)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    let name = scan_file_name(input).0.to_string();
                                    gr_font_get_named(&name, (m as i32) - 14, e)
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexFindSelectorByName => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => {
                                    let k = scan_int(input);
                                    let name = scan_file_name(input).0.to_string();
                                    aat::aat_font_get_named_1(&name, m.into(), e.attributes, k)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    let k = scan_int(input);
                                    let name = scan_file_name(input).0.to_string();
                                    gr_font_get_named_1(&name, (m as i32) - 14, e, k)
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexOTCountScripts => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                Font::Native(Otgr(e)) if e.using_open_type() => {
                                    ot_font_get((m as i32) - 14, e)
                                }
                                _ => 0,
                            }
                        }
                        LastItemCode::XetexOTCountLanguages | LastItemCode::XetexOTScript => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                Font::Native(Otgr(e)) if e.using_open_type() => {
                                    let val = scan_int(input);
                                    ot_font_get_1((m as i32) - 14, e, val)
                                }
                                _ => {
                                    not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexOTCountFeatures | LastItemCode::XetexOTLanguage => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                Font::Native(Otgr(e)) if e.using_open_type() => {
                                    let k = scan_int(input);
                                    let val = scan_int(input);
                                    ot_font_get_2((m as i32) - 14, e, k, val)
                                }
                                _ => {
                                    not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexOTFeature => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                Font::Native(Otgr(e)) if e.using_open_type() => {
                                    let k = scan_int(input);
                                    let kk = scan_int(input);
                                    let val = scan_int(input);
                                    ot_font_get_3((m as i32) - 14, e, k, kk, val)
                                }
                                _ => {
                                    not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                    -1
                                }
                            }
                        }
                        LastItemCode::XetexMapCharToGlyph => {
                            if let Font::Native(nf) =
                                &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize]
                            {
                                let n = scan_int(input);
                                map_char_to_glyph(nf, std::char::from_u32(n as u32).unwrap())
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                0
                            }
                        }
                        LastItemCode::XetexGlyphIndex => {
                            if let Font::Native(nf) =
                                &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize]
                            {
                                let name = scan_file_name(input).0.to_string();
                                map_glyph_to_index(nf, &name)
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                0
                            }
                        }
                        LastItemCode::XetexFontType => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(_)) => 1,
                                Font::Native(Otgr(e)) if e.using_open_type() => 2,
                                Font::Native(Otgr(e)) if e.using_graphite() => 3,
                                _ => 0,
                            }
                        }
                        LastItemCode::XetexFirstChar | LastItemCode::XetexLastChar => {
                            let n = scan_font_ident(input);
                            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[n as usize] {
                                get_font_char_range(
                                    n as usize,
                                    (m == LastItemCode::XetexFirstChar) as i32,
                                )
                            } else if m == LastItemCode::XetexFirstChar {
                                FONT_BC[n as usize] as i32
                            } else {
                                FONT_EC[n as usize] as i32
                            }
                        }
                        LastItemCode::PdfLastXPos => pdf_last_x_pos.0,
                        LastItemCode::PdfLastYPos => pdf_last_y_pos.0,
                        LastItemCode::XetexPdfPageCount => {
                            let name = scan_file_name(input).0.to_string();
                            count_pdf_file_pages(&name)
                        }
                        LastItemCode::CurrentGroupLevel => cur_level as i32 - 1,
                        LastItemCode::CurrentGroupType => cur_group as i32,
                        LastItemCode::CurrentIfLevel => {
                            let mut qopt = cond_ptr;
                            let mut val = 0;
                            while let Some(q) = qopt {
                                val += 1;
                                qopt = llist_link(q);
                            }
                            val
                        }
                        LastItemCode::CurrentIfType => {
                            if cond_ptr.is_none() {
                                0
                            } else if (cur_if as i32) < UNLESS_CODE {
                                cur_if as i32 + 1
                            } else {
                                -(cur_if as i32 - 31)
                            }
                        }
                        LastItemCode::CurrentIfBranch => {
                            if if_limit == FiOrElseCode::Or || if_limit == FiOrElseCode::Else {
                                1
                            } else if if_limit == FiOrElseCode::Fi {
                                -1
                            } else {
                                0
                            }
                        }
                        LastItemCode::GlueStretchOrder | LastItemCode::GlueShrinkOrder => {
                            let q = scan_normal_glue(input);
                            let val = if m == LastItemCode::GlueStretchOrder {
                                q.stretch_order() as i32
                            } else {
                                q.shrink_order() as i32
                            };
                            delete_glue_ref(q.ptr());
                            val
                        }
                        _ => unreachable!(),
                    };
                    (true, val, ValLevel::Int)
                }
            } else {
                let mut val = 0;
                let mut tx = cur_list.tail;
                match Node::from(tx) {
                    Node::Text(TxtNode::Math(m))
                        if m.subtype() == MathType::Eq(BE::End, MathMode::Middle) =>
                    {
                        let mut r = cur_list.head as i32;
                        let mut q;
                        loop {
                            q = r as usize;
                            r = *LLIST_link(q);
                            if r == tx as i32 {
                                break;
                            }
                        }
                        tx = q;
                    }
                    _ => {}
                }
                let mut val_level;
                if m == LastItemCode::LastNodeType {
                    val_level = ValLevel::Int;
                    if tx == cur_list.head || cur_list.mode.1 == ListMode::NoMode {
                        val = -1;
                    }
                } else {
                    val_level = ValLevel::from(m as u8);
                }
                if tx < hi_mem_min as usize && cur_list.mode.1 != ListMode::NoMode {
                    let nd = Node::from(tx);
                    match m {
                        LastItemCode::LastPenalty => {
                            if let Node::Text(TxtNode::Penalty(p)) = &nd {
                                val = p.penalty();
                            }
                        }
                        LastItemCode::LastKern => {
                            if let Node::Text(TxtNode::Kern(k)) = &nd {
                                val = k.width().0;
                            }
                        }
                        LastItemCode::LastSkip => {
                            if let Node::Text(TxtNode::Glue(g)) = &nd {
                                val = g.glue_ptr();
                                if g.param() == MU_GLUE {
                                    val_level = ValLevel::Mu;
                                }
                            }
                        }
                        LastItemCode::LastNodeType => {
                            val = match &nd {
                                Node::Text(nd) => match nd {
                                    TxtNode::Style(_)
                                    | TxtNode::Choice(_)
                                    | TxtNode::MarginKern(_) => 15,
                                    nd => nd.get_type_num() as i32 + 1,
                                },
                                _ => 15,
                            };
                        }
                        _ => {}
                    }
                } else if cur_list.mode == (false, ListMode::VMode) && tx == cur_list.head {
                    match m {
                        LastItemCode::LastPenalty => val = last_penalty,
                        LastItemCode::LastKern => val = last_kern.0,
                        LastItemCode::LastSkip => {
                            if last_glue != MAX_HALFWORD {
                                val = last_glue
                            }
                        }
                        LastItemCode::LastNodeType => val = last_node_type,
                        _ => unreachable!(),
                    }
                }
                (true, val, val_level)
            }
        }
        _ => {
            t_eprint!(
                "You can\'t use `{}\' after {}",
                CmdChr(cmd, chr),
                Esc("the")
            );
            help!("I\'m forgetting what you said and using zero instead.");
            error();
            (
                true,
                0,
                if level != ValLevel::Tok {
                    ValLevel::Dimen
                } else {
                    ValLevel::Int
                },
            )
        }
    }
}
pub(crate) unsafe fn scan_something_internal(
    input: &mut input_state_t,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    level: ValLevel,
    negative: bool,
) -> (i32, ValLevel) {
    let (flag, mut val, mut val_level) =
        restart_scan_something_internal(input, tok, cmd, chr, level, negative);

    if flag {
        while val_level > level {
            /*447:*/
            if val_level == ValLevel::Glue {
                val = MEM[(val + 1) as usize].b32.s1
            } else if val_level == ValLevel::Mu {
                mu_error();
            }
            val_level.prev();
        }

        val = if negative {
            match val_level {
                ValLevel::Int | ValLevel::Dimen => -val,
                ValLevel::Glue | ValLevel::Mu => {
                    let val = &GlueSpec(val as usize);
                    let spec = -val;
                    spec.ptr() as i32
                }
                _ => unreachable!(),
            }
        } else {
            if val_level == ValLevel::Glue || val_level == ValLevel::Mu {
                GlueSpec(val as usize).rc_inc();
            }
            val
        }
    };
    (val, val_level)
}
pub(crate) unsafe fn scan_int(input: &mut input_state_t) -> i32 {
    scan_int_with_radix(input).0
}
pub(crate) unsafe fn scan_int_with_radix(input: &mut input_state_t) -> (i32, i16, i32) {
    let mut radix: i16 = 0;
    let mut OK_so_far = true;
    let mut negative = false;
    let mut tok;
    let mut cmd;
    let mut chr;
    loop {
        let next = loop {
            /*424:*/
            let next = get_x_token(input);
            if next.1 != Cmd::Spacer {
                break next;
            }
        };
        tok = next.0;
        cmd = next.1;
        chr = next.2;
        if tok == OTHER_TOKEN + '-' as i32 {
            negative = !negative;
            tok = OTHER_TOKEN + '+' as i32
        }
        if tok != OTHER_TOKEN + '+' as i32 {
            break;
        }
    }

    let mut ival;
    if tok == ALPHA_TOKEN as i32 {
        /*460:*/
        let next = get_token(input);
        tok = next.0;
        cmd = next.1;
        chr = next.2;
        /*461:*/
        ival = if tok < CS_TOKEN_FLAG {
            /*462:*/
            if cmd <= Cmd::RightBrace {
                if cmd == Cmd::RightBrace {
                    align_state += 1;
                } else {
                    align_state -= 1;
                }
            }
            chr
        } else if tok < CS_TOKEN_FLAG + SINGLE_BASE as i32 {
            tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
        } else {
            tok - (CS_TOKEN_FLAG + SINGLE_BASE as i32)
        }; /*:463*/
        if ival > BIGGEST_USV as i32 {
            t_eprint!("Improper alphabetic constant");
            help!(
                "A one-character control sequence belongs after a ` mark.",
                "So I\'m essentially inserting \\0 here."
            );
            ival = '0' as i32;
            back_error(input, tok);
        } else {
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
            if cmd != Cmd::Spacer {
                back_input(input, tok);
            }
        }
    } else if cmd >= MIN_INTERNAL && cmd <= MAX_INTERNAL {
        let (val, _) = scan_something_internal(input, tok, cmd, chr, ValLevel::Int, false);
        ival = val;
    } else {
        radix = 10;
        let mut m = 0xccccccc;
        if tok == OCTAL_TOKEN {
            radix = 8;
            m = 0x10000000;
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
        } else if tok == HEX_TOKEN {
            radix = 16;
            m = 0x8000000;
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
        }
        let mut vacuous = true;
        ival = 0;
        loop {
            let d;
            if tok < ZERO_TOKEN + radix as i32 && tok >= ZERO_TOKEN && tok <= ZERO_TOKEN + 9 {
                d = (tok - ZERO_TOKEN) as i16
            } else {
                if radix as i32 != 16 {
                    break;
                }
                if tok <= A_TOKEN + 5 && tok >= A_TOKEN {
                    d = (tok - A_TOKEN + 10) as i16
                } else {
                    if !(tok <= OTHER_A_TOKEN + 5 && tok >= OTHER_A_TOKEN) {
                        break;
                    }
                    d = (tok - OTHER_A_TOKEN + 10) as i16
                }
            }
            vacuous = false;
            if ival >= m && (ival > m || d as i32 > 7 || radix as i32 != 10) {
                if OK_so_far {
                    t_eprint!("Number too big");
                    help!(
                        "I can only go up to 2147483647=\'17777777777=\"7FFFFFFF,",
                        "so I\'m using that number instead of yours."
                    );
                    error();
                    ival = TEX_INFINITY;
                    OK_so_far = false
                }
            } else {
                ival = ival * radix as i32 + d as i32
            }
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
        }
        if vacuous {
            /*464:*/
            t_eprint!("Missing number, treated as zero");
            help!(
                "A number should have been here; I inserted `0\'.",
                "(If you can\'t figure out why I needed to see a number,",
                "look up `weird error\' in the index to The TeXbook.)"
            );
            back_error(input, tok);
        } else if cmd != Cmd::Spacer {
            back_input(input, tok);
        }
    }
    if negative {
        ival = -ival;
    };
    (ival, radix, tok)
}
pub(crate) unsafe fn xetex_scan_dimen(
    input: &mut input_state_t,
    mu: bool,
    inf: bool,
    shortcut: Option<i32>,
    requires_units: bool,
) -> Scaled {
    let mut f = Scaled::ZERO;
    arith_error = false;
    cur_order = GlueOrder::Normal;
    let mut negative = false;
    let mut val = if let Some(val) = shortcut {
        val
    } else {
        negative = false;
        let mut tok;
        let mut cmd;
        let mut chr;
        loop {
            let next = loop {
                let next = get_x_token(input);
                if !(next.1 == Cmd::Spacer) {
                    break next;
                }
            };
            tok = next.0;
            cmd = next.1;
            chr = next.2;
            if tok == OTHER_TOKEN + '-' as i32 {
                negative = !negative;
                tok = OTHER_TOKEN + '+' as i32
            }
            if tok != OTHER_TOKEN + '+' as i32 {
                break;
            }
        }
        if cmd >= MIN_INTERNAL && cmd <= MAX_INTERNAL {
            /*468:*/
            if mu {
                let (mut val, val_level) =
                    scan_something_internal(input, tok, cmd, chr, ValLevel::Mu, false);
                match val_level {
                    ValLevel::Int | ValLevel::Dimen => {}
                    _ => {
                        let v = MEM[(val + 1) as usize].b32.s1;
                        delete_glue_ref(val as usize);
                        val = v;
                    }
                }
                if val_level == ValLevel::Mu {
                    return attach_sign(negative, Scaled(val));
                } else if val_level != ValLevel::Int {
                    mu_error();
                }
                val
            } else {
                let (val, val_level) =
                    scan_something_internal(input, tok, cmd, chr, ValLevel::Dimen, false);
                if val_level == ValLevel::Dimen {
                    return attach_sign(negative, Scaled(val));
                }
                val
            }
        } else {
            back_input(input, tok);
            if tok == CONTINENTAL_POINT_TOKEN {
                tok = POINT_TOKEN;
            }
            let (val, radix, mut tok) = if tok != POINT_TOKEN {
                scan_int_with_radix(input)
            } else {
                (0, 10, tok)
            };
            if tok == CONTINENTAL_POINT_TOKEN {
                tok = POINT_TOKEN;
            }
            if radix == 10 && tok == POINT_TOKEN {
                /*471:*/
                let mut k = 0; /* if(requires_units) */
                let mut p = None.tex_int();
                let _ = get_token(input);
                let (tok, cmd) = loop {
                    let (tok, cmd, ..) = get_x_token(input);
                    if tok > ZERO_TOKEN + 9 || tok < ZERO_TOKEN {
                        break (tok, cmd);
                    }
                    if (k as i32) < 17 {
                        let q = get_avail();
                        *LLIST_link(q) = p;
                        MEM[q].b32.s0 = tok - ZERO_TOKEN;
                        p = q as i32;
                        k += 1
                    }
                };

                // done1:
                for kk in (0..k as usize).rev() {
                    dig[kk] = MEM[p as usize].b32.s0 as u8;
                    let q = p as usize;
                    p = *LLIST_link(p as usize);
                    *LLIST_link(q) = avail.tex_int();
                    avail = Some(q);
                }

                static mut dig: [u8; 23] = [0; 23];
                unsafe fn round_decimals(mut k: i16) -> Scaled {
                    let mut a: i32 = 0;
                    while k as i32 > 0 {
                        k -= 1;
                        a = (a + dig[k as usize] as i32 * 0x20000) / 10
                    }
                    Scaled((a + 1) / 2)
                }

                f = round_decimals(k);
                if cmd != Cmd::Spacer {
                    back_input(input, tok);
                }
            }
            val
        }
    };

    if val < 0 {
        negative = !negative;
        val = -val
    }
    if requires_units {
        if inf {
            /*473:*/
            if scan_keyword(input, "fil") {
                cur_order = GlueOrder::Fil;
                while scan_keyword(input, "l") {
                    if cur_order == GlueOrder::Filll {
                        t_eprint!("Illegal unit of measure (replaced by filll)");
                        help!("I dddon\'t go any higher than filll.");
                        error();
                    } else {
                        cur_order = GlueOrder::from((cur_order as u16) + 1);
                    }
                }
                return attach_fraction(input, f, negative, val);
            }
        }

        let save_val = val;

        let (tok, cmd, chr, _) = loop {
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer) {
                break next;
            }
        };
        if cmd < MIN_INTERNAL || cmd > MAX_INTERNAL {
            back_input(input, tok);
        } else {
            let v = if mu {
                let (mut val, val_level) =
                    scan_something_internal(input, tok, cmd, chr, ValLevel::Mu, false);
                match val_level {
                    ValLevel::Int | ValLevel::Dimen => {}
                    _ => {
                        let v = MEM[(val + 1) as usize].b32.s1;
                        delete_glue_ref(val as usize);
                        val = v
                    }
                }
                if val_level != ValLevel::Mu {
                    mu_error();
                }
                val
            } else {
                let (val, _) =
                    scan_something_internal(input, tok, cmd, chr, ValLevel::Dimen, false);
                val
            };
            return found(save_val, Scaled(v), f, negative);
        }

        if !mu {
            if scan_keyword(input, "em") {
                let v = Scaled(
                    FONT_INFO[(QUAD_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize]) as usize]
                        .b32
                        .s1,
                );
                let (tok, cmd, ..) = get_x_token(input);
                if cmd != Cmd::Spacer {
                    back_input(input, tok);
                }
                return found(save_val, v, f, negative);
            } else if scan_keyword(input, "ex") {
                let v = Scaled(
                    FONT_INFO
                        [(X_HEIGHT_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize]) as usize]
                        .b32
                        .s1,
                );
                let (tok, cmd, ..) = get_x_token(input);
                if cmd != Cmd::Spacer {
                    back_input(input, tok);
                }
                return found(save_val, v, f, negative);
            }
        }

        // not_found:
        if mu {
            /*475:*/
            if scan_keyword(input, "mu") {
                return attach_fraction(input, f, negative, val);
            } else {
                t_eprint!("Illegal unit of measure (mu inserted)");
                help!(
                    "The unit of measurement in math glue must be mu.",
                    "To recover gracefully from this error, it\'s best to",
                    "delete the erroneous units; e.g., type `2\' to delete",
                    "two letters. (See Chapter 27 of The TeXbook.)"
                );
                error();
                return attach_fraction(input, f, negative, val);
            }
        }

        if scan_keyword(input, "true") {
            /*476:*/
            prepare_mag(); /* magic ratio consant */
            if get_int_par(IntPar::mag) != 1000 {
                let (v, tex_remainder) =
                    xn_over_d(Scaled(val), Scaled(1000), get_int_par(IntPar::mag)); /* magic ratio consant */
                val = v.0;
                let f_ = (((1000 * f.0) as i64 + 65536 * tex_remainder as i64)
                    / get_int_par(IntPar::mag) as i64) as i32;
                val = (val as i64 + f_ as i64 / 65536) as i32;
                f = Scaled((f_ as i64 % 65536) as i32);
            }
        }

        if scan_keyword(input, "pt") {
            return attach_fraction(input, f, negative, val);
        }

        let num;
        let denom;
        if scan_keyword(input, "in") {
            num = 7227;
            denom = 100;
        } else if scan_keyword(input, "pc") {
            num = 12;
            denom = 1;
        } else if scan_keyword(input, "cm") {
            num = 7227; // magic ratio consant
            denom = 254; // magic ratio consant
        } else if scan_keyword(input, "mm") {
            num = 7227; // magic ratio consant
            denom = 2540; // magic ratio consant
        } else if scan_keyword(input, "bp") {
            num = 7227; // magic ratio consant
            denom = 7200; // magic ratio consant
        } else if scan_keyword(input, "dd") {
            num = 1238; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(input, "cc") {
            num = 14856; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(input, "sp") {
            return done(input, negative, Scaled(val));
        /*478:*/
        } else {
            t_eprint!("Illegal unit of measure (pt inserted)");
            help!(
                "Dimensions can be in units of em, ex, in, pt, pc,",
                "cm, mm, dd, cc, bp, or sp; but yours is a new one!",
                "I\'ll assume that you meant to say pt, for printer\'s points.",
                "To recover gracefully from this error, it\'s best to",
                "delete the erroneous units; e.g., type `2\' to delete",
                "two letters. (See Chapter 27 of The TeXbook.)"
            );
            error();
            return attach_fraction(input, f, negative, val);
        }

        let (val, tex_remainder) = xn_over_d(Scaled(val), Scaled(num), denom);
        let f_ = (((num * f.0) as i64 + 65536 * tex_remainder as i64) / denom as i64) as i32;
        let val = (val.0 as i64 + f_ as i64 / 65536) as i32;
        f = Scaled((f_ as i64 % 65536) as i32);
        return attach_fraction(input, f, negative, val);
    } else if val >= 16384 {
        arith_error = true
    } else {
        val = (val as i64 * 65536 + f.0 as i64) as i32
    }

    // done2:
    unsafe fn attach_fraction(
        input: &mut input_state_t,
        f: Scaled,
        negative: bool,
        mut val: i32,
    ) -> Scaled {
        if val >= 16384 {
            arith_error = true
        } else {
            val = (val as i64 * 65536 + f.0 as i64) as i32
        }
        done(input, negative, Scaled(val))
    }

    unsafe fn done(input: &mut input_state_t, negative: bool, val: Scaled) -> Scaled {
        let (tok, cmd, ..) = get_x_token(input);
        if cmd != Cmd::Spacer {
            back_input(input, tok);
        }
        attach_sign(negative, val)
    }

    unsafe fn found(save_cur_val: i32, v: Scaled, f: Scaled, negative: bool) -> Scaled {
        let val = v.mul_add(save_cur_val, xn_over_d(v, f, 65536 as i32).0);
        attach_sign(negative, val)
    }

    unsafe fn attach_sign(negative: bool, mut val: Scaled) -> Scaled {
        if arith_error || val.0.wrapping_abs() >= 0x40000000 {
            // TODO: check
            /*479:*/
            t_eprint!("Dimension too large");
            help!(
                "I can\'t work with sizes bigger than about 19 feet.",
                "Continue and I\'ll use the largest value I can."
            );
            error();
            val = Scaled::MAX_HALFWORD;
            arith_error = false
        }
        if negative {
            val = -val
        };
        val
    }

    attach_sign(negative, Scaled(val))
}
pub(crate) unsafe fn scan_dimen(
    input: &mut input_state_t,
    mu: bool,
    inf: bool,
    shortcut: Option<i32>,
) -> Scaled {
    xetex_scan_dimen(input, mu, inf, shortcut, true)
}
pub(crate) unsafe fn scan_decimal(input: &mut input_state_t) -> Scaled {
    xetex_scan_dimen(input, false, false, None, false)
}
pub(crate) unsafe fn scan_glue(input: &mut input_state_t, level: ValLevel) -> GlueSpec {
    let mu = level == ValLevel::Mu;
    let mut negative = false;
    let (tok, cmd, chr) = loop {
        let (mut tok, cmd, chr, _) = loop {
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer) {
                break next;
            }
        };

        if tok == OTHER_TOKEN + '-' as i32 {
            negative = !negative;
            tok = OTHER_TOKEN + '+' as i32
        }
        if tok != OTHER_TOKEN + '+' as i32 {
            break (tok, cmd, chr);
        }
    };
    let val = if cmd >= MIN_INTERNAL && cmd <= MAX_INTERNAL {
        let (val, val_level) = scan_something_internal(input, tok, cmd, chr, level, negative);
        match val_level {
            ValLevel::Int => scan_dimen(input, mu, false, Some(val)),
            ValLevel::Dimen => Scaled(if level == ValLevel::Mu {
                mu_error();
                val
            } else {
                val
            }),
            ValLevel::Glue | ValLevel::Mu => {
                if val_level != level {
                    mu_error();
                }
                return GlueSpec(val as usize);
            }
            _ => unreachable!(),
        }
    } else {
        back_input(input, tok);
        let val = scan_dimen(input, mu, false, None);
        if negative {
            -val
        } else {
            val
        }
    };
    let mut q = new_spec(&ZERO_GLUE);
    q.set_size(val);
    if scan_keyword(input, "plus") {
        let val = scan_dimen(input, mu, true, None);
        q.set_stretch(val).set_stretch_order(cur_order);
    }
    if scan_keyword(input, "minus") {
        let val = scan_dimen(input, mu, true, None);
        q.set_shrink(val).set_shrink_order(cur_order);
    }
    q
    /*:481*/
}
pub(crate) unsafe fn add_or_sub(x: i32, mut y: i32, max_answer: i32, negative: bool) -> i32 {
    if negative {
        y = -y
    }
    if x >= 0 {
        if y <= max_answer - x {
            x + y
        } else {
            arith_error = true;
            0
        }
    } else if y >= -max_answer - x {
        x + y
    } else {
        arith_error = true;
        0
    }
}
pub(crate) unsafe fn quotient(mut n: i32, mut d: i32) -> i32 {
    if d == 0 {
        arith_error = true;
        0
    } else {
        let mut negative = if d > 0 {
            false
        } else {
            d = -d;
            true
        };
        if n < 0 {
            n = -n;
            negative = !negative;
        }
        let mut a = n / d;
        n -= a * d;
        d = n - d;
        if d + n >= 0 {
            a += 1;
        }
        if negative {
            -a
        } else {
            a
        }
    }
}
pub(crate) unsafe fn fract(mut x: i32, mut n: i32, mut d: i32, max_answer: i32) -> i32 {
    fn too_big() -> i32 {
        unsafe {
            arith_error = true;
        }
        0
    }
    fn found(a: i32, negative: bool) -> i32 {
        if negative {
            -a
        } else {
            a
        }
    }

    if d == 0 {
        return too_big();
    }
    let mut a = 0;
    let mut negative = if d > 0 {
        false
    } else {
        d = -d;
        true
    };
    match x.cmp(&0) {
        Ordering::Less => {
            x = -x;
            negative = !negative;
        }
        Ordering::Equal => return a,
        _ => {}
    }
    if n < 0 {
        n = -n;
        negative = !negative
    }
    let t = n / d;
    if t > max_answer / x {
        return too_big();
    }
    a = t * x;
    n -= t * d;
    if n == 0 {
        return found(a, negative);
    }
    let t = x / d;
    if t > (max_answer - a) / n {
        return too_big();
    }
    a += t * n;
    x -= t * d;
    if x == 0 {
        return found(a, negative);
    }
    if x < n {
        let t = x;
        x = n;
        n = t
    }
    let mut f = 0;
    let mut r = d / 2 - d;
    let h = -r;
    loop {
        if n & 1 != 0 {
            r += x;
            if r >= 0 {
                r -= d;
                f += 1
            }
        }
        n /= 2;
        if n == 0 {
            break;
        }
        if x < h {
            x = x + x
        } else {
            let t = x - d;
            x += t;
            f += n;
            if x >= n {
                continue;
            }
            if x == 0 {
                break;
            }
            let t = x;
            x = n;
            n = t
        }
    }
    if f > max_answer - a {
        too_big()
    } else {
        a += f;
        found(a, negative)
    }
}
pub(crate) unsafe fn scan_expr(input: &mut input_state_t, val_level: &mut ValLevel) -> i32 {
    let mut e;
    let mut l = *val_level;
    let a = arith_error;
    let mut b = false;
    let mut p = None.tex_int();
    'c_78022: loop {
        let mut o;
        let mut r = Expr::None;
        e = 0;
        let mut s = Expr::None;
        let mut t = 0;
        let mut n = 0;
        loop {
            o = if s == Expr::None { l } else { ValLevel::Int };
            let tok = loop {
                let next = get_x_token(input);
                if !(next.1 == Cmd::Spacer) {
                    break next;
                }
            }
            .0;
            if tok == OTHER_TOKEN + 40 {
                break;
            }
            back_input(input, tok);
            let mut f = match o {
                ValLevel::Int => scan_int(input),
                ValLevel::Dimen => scan_dimen(input, false, false, None).0,
                ValLevel::Glue => scan_normal_glue(input).ptr() as i32,
                _ => scan_mu_glue(input).ptr() as i32,
            };
            loop {
                let (tok, cmd, ..) = loop {
                    /*1572:*//*424:*/
                    let next = get_x_token(input);
                    if !(next.1 == Cmd::Spacer) {
                        break next;
                    }
                };
                let mut o;
                if tok == OTHER_TOKEN + 43 {
                    o = Expr::Add;
                } else if tok == OTHER_TOKEN + 45 {
                    o = Expr::Sub;
                } else if tok == OTHER_TOKEN + 42 {
                    o = Expr::Mult;
                } else if tok == OTHER_TOKEN + 47 {
                    o = Expr::Div;
                } else {
                    o = Expr::None;
                    if p.opt().is_none() {
                        if cmd != Cmd::Relax {
                            back_input(input, tok);
                        }
                    } else if tok != OTHER_TOKEN + 41 {
                        t_eprint!("Missing ) inserted for expression");
                        help!("I was expecting to see `+\', `-\', `*\', `/\', or `)\'. Didn\'t.");
                        back_error(input, tok);
                    }
                }
                arith_error = b;
                if l == ValLevel::Int || (s == Expr::Mult || s == Expr::Div || s == Expr::Scale) {
                    if f > TEX_INFINITY || f < -TEX_INFINITY {
                        arith_error = true;
                        f = 0;
                    }
                } else if l == ValLevel::Dimen {
                    if Scaled(f).abs() > Scaled::MAX_HALFWORD {
                        arith_error = true;
                        f = 0;
                    }
                } else if {
                    let f = GlueSpec(f as usize);
                    f.size().abs() > Scaled::MAX_HALFWORD
                        || f.stretch().abs() > Scaled::MAX_HALFWORD
                        || f.shrink().abs() > Scaled::MAX_HALFWORD
                } {
                    arith_error = true;
                    delete_glue_ref(f as usize);
                    f = new_spec(&ZERO_GLUE).ptr() as i32
                }
                match s {
                    Expr::None => {
                        /*1579: */
                        t = if l >= ValLevel::Glue && o != Expr::None {
                            let mut t = new_spec(&GlueSpec(f as usize));
                            delete_glue_ref(f as usize);
                            if t.stretch() == Scaled::ZERO {
                                t.set_stretch_order(GlueOrder::Normal);
                            }
                            if t.shrink() == Scaled::ZERO {
                                t.set_shrink_order(GlueOrder::Normal);
                            }
                            t.ptr() as i32
                        } else {
                            f
                        };
                    }
                    Expr::Mult => {
                        if o == Expr::Div {
                            n = f;
                            o = Expr::Scale;
                        } else if l == ValLevel::Int {
                            t = mult_and_add(t, f, 0, TEX_INFINITY)
                        } else if l == ValLevel::Dimen {
                            t = Scaled(t).mul_add(f, Scaled::ZERO).0;
                        } else {
                            let mut t = GlueSpec(t as usize);
                            t.set_size(t.size().mul_add(f, Scaled::ZERO));
                            t.set_stretch(t.stretch().mul_add(f, Scaled::ZERO));
                            t.set_shrink(t.shrink().mul_add(f, Scaled::ZERO));
                        }
                    }
                    Expr::Div => {
                        if l < ValLevel::Glue {
                            t = quotient(t, f)
                        } else {
                            let mut t = GlueSpec(t as usize);
                            t.set_size(t.size().quotient(f));
                            t.set_stretch(t.stretch().quotient(f));
                            t.set_shrink(t.shrink().quotient(f));
                        }
                    }
                    Expr::Scale => match l {
                        ValLevel::Int => t = fract(t, n, f, TEX_INFINITY),
                        ValLevel::Dimen => t = Scaled(t).fract(Scaled(n), Scaled(f)).0,
                        _ => {
                            let mut e = GlueSpec(e as usize);
                            let mut t = GlueSpec(t as usize);
                            t.set_size(t.size().fract(Scaled(n), Scaled(f)));
                            t.set_stretch(t.stretch().fract(Scaled(n), Scaled(f)));
                            e.set_shrink(e.shrink().fract(Scaled(n), Scaled(f)));
                        }
                    },
                    _ => {}
                }
                if o == Expr::Mult || o == Expr::Div || o == Expr::Scale {
                    s = o;
                } else {
                    /*1580: */
                    s = Expr::None;
                    if r == Expr::None {
                        e = t
                    } else if l == ValLevel::Int {
                        e = add_or_sub(e, t, TEX_INFINITY, r == Expr::Sub)
                    } else if l == ValLevel::Dimen {
                        e = Scaled(e).add_or_sub(Scaled(t), r == Expr::Sub).0
                    } else {
                        /*1582: */
                        let mut e = GlueSpec(e as usize);
                        let t = GlueSpec(t as usize);
                        e.set_size(e.size().add_or_sub(t.size(), r == Expr::Sub));
                        if e.stretch_order() == t.stretch_order() {
                            e.set_stretch(e.stretch().add_or_sub(t.stretch(), r == Expr::Sub));
                        } else if e.stretch_order() < t.stretch_order()
                            && t.stretch() != Scaled::ZERO
                        {
                            e.set_stretch(t.stretch())
                                .set_stretch_order(t.stretch_order());
                        }
                        if e.shrink_order() == t.shrink_order() {
                            e.set_shrink(e.shrink().add_or_sub(t.shrink(), r == Expr::Sub));
                        } else if e.shrink_order() < t.shrink_order() && t.shrink() != Scaled::ZERO
                        {
                            e.set_shrink(t.shrink()).set_shrink_order(t.shrink_order());
                        }
                        delete_glue_ref(t.ptr());
                        if e.stretch() == Scaled::ZERO {
                            e.set_stretch_order(GlueOrder::Normal);
                        }
                        if e.shrink() == Scaled::ZERO {
                            e.set_shrink_order(GlueOrder::Normal);
                        }
                    }
                    r = o;
                }
                b = arith_error;
                if o != Expr::None {
                    break;
                }
                /*1577: */
                if let Some(q) = p.opt() {
                    f = e;
                    e = MEM[q + 1].b32.s1;
                    t = MEM[q + 2].b32.s1;
                    n = MEM[q + 3].b32.s1;
                    s = Expr::from(MEM[q].b16.s0 as i32 / 4);
                    r = Expr::from(MEM[q].b16.s0 as i32 % 4);
                    l = ValLevel::from(MEM[q].b16.s1 as u8);
                    p = *LLIST_link(q);
                    free_node(q, EXPR_NODE_SIZE);
                } else {
                    break 'c_78022;
                }
            }
        }
        /*1576: */
        let q = get_node(EXPR_NODE_SIZE);
        *LLIST_link(q) = p;
        MEM[q].b16.s1 = l as u16;
        MEM[q].b16.s0 = (4 * s as i32 + r as i32) as u16;
        MEM[q + 1].b32.s1 = e;
        MEM[q + 2].b32.s1 = t;
        MEM[q + 3].b32.s1 = n;
        p = q as i32;
        l = o;
    }
    if b {
        t_eprint!("Arithmetic overflow");
        help!(
            "I can\'t evaluate this expression,",
            "since the result is out of range."
        );
        error();
        if l >= ValLevel::Glue {
            delete_glue_ref(e as usize);
            e = 0;
            MEM[e as usize].b32.s1 += 1;
        } else {
            e = 0;
        }
    }
    arith_error = a;
    *val_level = l;
    e
}
pub(crate) unsafe fn scan_normal_glue(input: &mut input_state_t) -> GlueSpec {
    scan_glue(input, ValLevel::Glue)
}
pub(crate) unsafe fn scan_mu_glue(input: &mut input_state_t) -> GlueSpec {
    scan_glue(input, ValLevel::Mu)
}
pub(crate) unsafe fn scan_rule_spec(input: &mut input_state_t, cmd: Cmd) -> Rule {
    let mut q = new_rule();
    if cmd == Cmd::VRule {
        q.set_width(DEFAULT_RULE);
    } else {
        q.set_height(DEFAULT_RULE).set_depth(Scaled::ZERO);
    }
    loop {
        if scan_keyword(input, "width") {
            q.set_width(scan_dimen(input, false, false, None));
        } else if scan_keyword(input, "height") {
            q.set_height(scan_dimen(input, false, false, None));
        } else if scan_keyword(input, "depth") {
            q.set_depth(scan_dimen(input, false, false, None));
        } else {
            break;
        }
    }
    q
}
pub(crate) unsafe fn scan_general_text(input: &mut input_state_t, cs: i32) -> i32 {
    let s = scanner_status;
    let w = warning_index;
    let d = def_ref;
    scanner_status = ScannerStatus::Absorbing;
    warning_index = cs;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let mut p = def_ref;
    scan_left_brace(input);
    let mut unbalance = 1;
    loop {
        let (tok, cmd, ..) = get_token(input);
        if tok < RIGHT_BRACE_LIMIT {
            if cmd < Cmd::RightBrace {
                unbalance += 1
            } else {
                unbalance -= 1;
                if unbalance == 0 {
                    break;
                }
            }
        }
        store_new_token(&mut p, tok);
    }
    let q = llist_link(def_ref);
    *LLIST_link(def_ref) = avail.tex_int();
    avail = Some(def_ref);
    let val = if q.is_none() { TEMP_HEAD } else { p } as i32;
    *LLIST_link(TEMP_HEAD) = q.tex_int();
    scanner_status = s;
    warning_index = w;
    def_ref = d;
    val
}
pub(crate) unsafe fn pseudo_start(input: &mut input_state_t, cs: i32) {
    let _ = scan_general_text(input, cs);
    let s = format!("{}", TokenNode(Some(TEMP_HEAD)));
    let mut s16 = s.encode_utf16().collect::<Vec<_>>();
    s16.push(' ' as u16);
    flush_list(llist_link(TEMP_HEAD));
    PoolString::check_capacity(1);
    let nl = get_int_par(IntPar::new_line_char);
    let p = get_avail();
    let mut q = p;

    for chunk in s16.split(|&c| c as i32 == nl) {
        let l = chunk.len();
        let mut m = 0;
        let mut sz = (l + 7) / 4;
        if sz == 1 {
            sz = 2
        }
        let mut r = get_node(sz as i32);
        *LLIST_link(q) = Some(r).tex_int();
        q = r;
        MEM[q].b32.s0 = sz as i32;
        while sz > 2 {
            sz -= 1;
            r += 1;
            let w = b16x4 {
                s3: chunk[m],
                s2: chunk[m + 1],
                s1: chunk[m + 2],
                s0: chunk[m + 3],
            };
            MEM[r].b16 = w;
            m += 4
        }
        let w = b16x4 {
            s3: if l > m { chunk[m] } else { ' ' as u16 },
            s2: if l > m + 1 { chunk[m + 1] } else { ' ' as u16 },
            s1: if l > m + 2 { chunk[m + 2] } else { ' ' as u16 },
            s0: if l > m + 3 { chunk[m + 3] } else { ' ' as u16 },
        };
        MEM[r + 1].b16 = w;
    }
    MEM[p].b32.s0 = MEM[p].b32.s1;
    MEM[p].b32.s1 = pseudo_files;
    pseudo_files = p as i32;
    begin_file_reading(input);
    line = 0;
    input.limit = input.start.unwrap();
    input.loc = Some(input.limit + 1);
    if get_int_par(IntPar::tracing_scan_tokens) > 0 {
        if term_offset > max_print_line - 3 {
            print_ln();
        } else if term_offset > 0 || file_offset > 0 {
            print_chr(' ');
        }
        input.name = 19;
        t_print!("( ");
        open_parens += 1;
        rust_stdout.as_mut().unwrap().flush().unwrap();
    } else {
        input.name = 18;
        input.synctex_tag = 0;
    };
}
/// converting the current string into a token list.
///
/// The `str_toks` function does this; it classifies spaces as type `spacer`
/// and everything else as type `other_char`.
///
/// The token list created by `str_toks` begins at `link(temp_head)` and ends
/// at the value `p` that is returned. (If `p=temp_head`, the list is empty.)
pub(crate) unsafe fn str_toks_cat_utf8(buf: &str, cat: i16) -> usize {
    let mut p = TEMP_HEAD; // tail of the token list
    *LLIST_link(p) = None.tex_int();
    for c in buf.chars() {
        // token being appended
        let t = if c == ' ' && cat == 0 {
            SPACE_TOKEN
        } else if cat == 0 {
            OTHER_TOKEN + c as i32
        } else {
            MAX_CHAR_VAL * cat as i32 + c as i32
        };
        fast_store_new_token(&mut p, t);
    }
    p
}

/// This procedure is supposed to scan something like `\skip\count12`,
/// i.e., whatever can follow `\the`, and it constructs a token list
/// containing something like `-3.0pt minus 0.5fill`
pub(crate) unsafe fn the_toks(input: &mut input_state_t, chr: i32, cs: i32) -> usize {
    // Handle `\unexpanded` or `\detokenize` and |return|
    if chr & 1 != 0 {
        let c = chr as i16;
        let val = scan_general_text(input, cs);
        if c == 1 {
            assert!(val.opt().is_some());
            return val as usize; // TODO: check TEX_NULL
        } else {
            let p = get_avail();
            *LLIST_link(p) = *LLIST_link(TEMP_HEAD);
            let s = format!("{}", TokenNode(Some(p)));
            flush_list(Some(p));
            return str_toks_cat_utf8(&s, 0);
        }
    }
    let (tok, cmd, chr, _) = get_x_token(input);
    let (val, val_level) = scan_something_internal(input, tok, cmd, chr, ValLevel::Tok, false);
    match val_level {
        ValLevel::Ident | ValLevel::Tok | ValLevel::InterChar | ValLevel::Mark => {
            // Copy the token list
            let mut p = TEMP_HEAD;
            *LLIST_link(p) = None.tex_int();
            if val_level == ValLevel::Ident {
                store_new_token(&mut p, CS_TOKEN_FLAG + val);
            } else if let Some(v) = val.opt() {
                let mut ropt = llist_link(v); // do not copy the reference count
                while let Some(r) = ropt {
                    fast_store_new_token(&mut p, *LLIST_info(r));
                    ropt = llist_link(r);
                }
            }
            p
        }
        _ => {
            let s = match val_level {
                ValLevel::Int => format!("{}", val),
                ValLevel::Dimen => format!("{}pt", Scaled(val)),
                ValLevel::Glue => {
                    let s = format!("{}", GlueSpecUnit(val, "pt"));
                    delete_glue_ref(val as usize);
                    s
                }
                ValLevel::Mu => {
                    let s = format!("{}", GlueSpecUnit(val, "mu"));
                    delete_glue_ref(val as usize);
                    s
                }
                _ => String::new(),
            };

            str_toks_cat_utf8(&s, 0)
        }
    }
}
/// Here's part of the |expand| subroutine
pub(crate) unsafe fn ins_the_toks(input: &mut input_state_t, chr: i32, cs: i32) {
    *LLIST_link(GARBAGE as usize) = Some(the_toks(input, chr, cs)).tex_int();
    begin_token_list(input, llist_link(TEMP_HEAD), Btl::Inserted);
}
/// The procedure `conv_toks` uses `str_toks` to insert the token list
/// for `convert` functions into the scanner; `\outer` control sequences
/// are allowed to follow `\string` and `\meaning`.
///
/// The extra temp string `u` is needed because `pdf_scan_ext_toks` incorporates
/// any pending string in its output. In order to save such a pending string,
/// we have to create a temporary string that is destroyed immediately after.
pub(crate) unsafe fn conv_toks(input: &mut input_state_t, chr: i32, cs: i32) {
    let mut fnt: usize = 0;
    let mut arg1: i32 = 0;
    let mut arg2: i32 = 0;
    let mut p = None;
    let mut cat = 0;
    let c = ConvertCode::n(chr as u8).unwrap();
    let mut oval = None;
    let mut o = None;
    match c {
        ConvertCode::Number | ConvertCode::RomanNumeral => oval = Some(scan_int(input)),
        ConvertCode::String | ConvertCode::Meaning => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            let (_, cmd, chr, cs) = get_token(input);
            o = Some((cmd, chr, cs));
            scanner_status = save_scanner_status;
        }
        ConvertCode::FontName => oval = Some(scan_font_ident(input)),
        ConvertCode::XetexUchar => oval = Some(scan_usv_num(input)),
        ConvertCode::XetexUcharcat => {
            let saved_chr = scan_usv_num(input);
            let val = scan_int(input);
            if val < Cmd::LeftBrace as i32
                || val > Cmd::OtherChar as i32
                || val == OUT_PARAM as i32
                || val == IGNORE as i32
            {
                t_eprint!(
                    "Invalid code ({}), should be in the ranges 1..4, 6..8, 10..12",
                    val
                );
                help!("I\'m going to use 12 instead of that illegal code value.");
                error();
                cat = 12;
            } else {
                cat = val as i16
            }
            oval = Some(saved_chr);
        }
        ConvertCode::PdfStrcmp => {
            let save_scanner_status = scanner_status;
            let save_warning_index = warning_index;
            let save_def_ref = def_ref;
            let u = if str_start[(str_ptr - TOO_BIG_CHAR) as usize] < pool_ptr {
                make_string()
            } else {
                0
            };
            oval = Some(compare_strings(input, cs));
            def_ref = save_def_ref;
            warning_index = save_warning_index;
            scanner_status = save_scanner_status;
            if u != 0 {
                str_ptr -= 1;
            }
        }
        ConvertCode::PdfMdfiveSum => {
            let save_scanner_status = scanner_status;
            let save_warning_index = warning_index;
            let save_def_ref = def_ref;
            let u = if str_start[(str_ptr - TOO_BIG_CHAR) as usize] < pool_ptr {
                make_string()
            } else {
                0
            };
            let boolvar = scan_keyword(input, "file");
            scan_pdf_ext_toks(input, cs);
            let s = format!("{}", TokenList(llist_link(def_ref)));
            delete_token_ref(def_ref);
            def_ref = save_def_ref;
            warning_index = save_warning_index;
            scanner_status = save_scanner_status;
            let md5 = getmd5sum(&s, boolvar);
            *LLIST_link(GARBAGE as usize) = Some(str_toks_cat_utf8(&md5, 0)).tex_int();
            begin_token_list(input, llist_link(TEMP_HEAD), Btl::Inserted);
            if u != 0 {
                str_ptr -= 1;
            }
            return;
        }
        ConvertCode::XetexVariationName => {
            let val = scan_font_ident(input);
            fnt = val as usize;
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(_)) => {
                    arg1 = scan_int(input);
                    arg2 = 0;
                }
                _ => not_aat_font_error(Cmd::Convert, c as i32, fnt),
            }
        }
        ConvertCode::XetexFeatureName => {
            let val = scan_font_ident(input);
            fnt = val as usize;
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(_)) => {
                    arg1 = scan_int(input);
                    arg2 = 0;
                }
                Font::Native(Otgr(e)) if e.using_graphite() => {
                    arg1 = scan_int(input);
                    arg2 = 0;
                }
                _ => not_aat_gr_font_error(Cmd::Convert, c as i32, fnt),
            }
        }
        ConvertCode::XetexSelectorName => {
            let val = scan_font_ident(input);
            fnt = val as usize;
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(_)) => {
                    arg1 = scan_int(input);
                    arg2 = scan_int(input);
                }
                Font::Native(Otgr(e)) if e.using_graphite() => {
                    arg1 = scan_int(input);
                    arg2 = scan_int(input);
                }
                _ => not_aat_gr_font_error(Cmd::Convert, c as i32, fnt),
            }
        }
        ConvertCode::XetexGlyphName => {
            let val = scan_font_ident(input);
            fnt = val as usize;
            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[fnt as usize] {
                arg1 = scan_int(input);
            } else {
                not_native_font_error(Cmd::Convert, c as i32, fnt);
            }
        }
        ConvertCode::LeftMarginKern | ConvertCode::RightMarginKern => {
            let val = scan_register_num(input);
            p = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt())
            };
            match p.map(TxtNode::from) {
                Some(TxtNode::List(b)) if b.is_horizontal() => {}
                _ => pdf_error("marginkern", "a non-empty hbox expected"),
            }
        }
        ConvertCode::JobName => {
            if job_name == 0 {
                open_log_file();
            }
        }
        ConvertCode::EtexRevision | ConvertCode::XetexRevision => {}
    }
    let s = match c {
        ConvertCode::Number => format!("{}", oval.unwrap()),
        ConvertCode::RomanNumeral => format!("{}", Roman(oval.unwrap())),
        ConvertCode::String => {
            let (_, chr, cs) = o.unwrap();
            if cs != 0 {
                format!("{:#}", Cs(cs))
            } else {
                std::char::from_u32(chr as u32).unwrap().to_string()
            }
        }
        ConvertCode::Meaning => {
            let (cmd, chr, _) = o.unwrap();
            format_meaning(cmd, chr)
        }
        ConvertCode::FontName => {
            let val = oval.unwrap();
            let font_name_str = PoolString::from(FONT_NAME[val as usize]);
            let mut s = match &FONT_LAYOUT_ENGINE[val as usize] {
                Font::Native(_) => {
                    let mut quote_char = '\"';
                    if font_name_str.as_slice().contains(&('\"' as u16)) {
                        quote_char = '\'';
                    }
                    format!("{0}{1}{0}", quote_char, font_name_str)
                }
                _ => format!("{}", font_name_str),
            };
            if FONT_SIZE[val as usize] != FONT_DSIZE[val as usize] {
                s += &format!(" at {}pt", FONT_SIZE[val as usize]);
            }
            s
        }
        ConvertCode::XetexUchar | ConvertCode::XetexUcharcat => {
            format!("{}", std::char::from_u32(oval.unwrap() as u32).unwrap())
        }
        ConvertCode::EtexRevision => ".6".to_string(),
        ConvertCode::PdfStrcmp => format!("{}", oval.unwrap()),
        ConvertCode::XetexRevision => ".99998".to_string(),
        ConvertCode::XetexVariationName => match &FONT_LAYOUT_ENGINE[fnt as usize] {
            #[cfg(target_os = "macos")]
            Font::Native(Aat(e)) => aat::aat_get_font_name(c as i32, e.attributes, arg1, arg2),
            _ => String::new(),
        },
        ConvertCode::XetexFeatureName | ConvertCode::XetexSelectorName => {
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(e)) => aat::aat_get_font_name(c as i32, e.attributes, arg1, arg2),
                Font::Native(Otgr(e)) if e.using_graphite() => {
                    gr_get_font_name(c as i32, e, arg1, arg2)
                }
                _ => String::new(),
            }
        }
        ConvertCode::XetexGlyphName => match &FONT_LAYOUT_ENGINE[fnt as usize] {
            #[cfg(target_os = "macos")]
            Font::Native(Aat(engine)) => engine.glyph_name(arg1 as u16),
            Font::Native(Otgr(engine)) => engine.glyph_name(arg1 as u16),
            _ => panic!("bad native font flag in `print_glyph_name`"),
        },
        ConvertCode::LeftMarginKern => {
            let mut popt = List::from(p.unwrap()).list_ptr().opt();
            while let Some(p) = popt {
                if match CharOrText::from(p) {
                    CharOrText::Char(_) => false,
                    CharOrText::Text(n) => match n {
                        TxtNode::Ins(_)
                        | TxtNode::Mark(_)
                        | TxtNode::Adjust(_)
                        | TxtNode::Penalty(_) => true,
                        TxtNode::Disc(n) if n.is_empty() => true,
                        TxtNode::Math(n) if n.is_empty() => true,
                        TxtNode::Kern(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.is_empty() => true,
                        TxtNode::List(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.param() == (GluePar::right_skip as u16) + 1 => true,
                        _ => false,
                    },
                } {
                    popt = llist_link(p);
                } else {
                    break;
                }
            }
            match popt.map(CharOrText::from) {
                Some(CharOrText::Text(TxtNode::MarginKern(m))) if MEM[m.ptr()].b16.s0 == 0 => {
                    format!("{}pt", Scaled(MEM[m.ptr() + 1].b32.s1))
                }
                _ => "0pt".to_string(),
            }
        }
        ConvertCode::RightMarginKern => {
            let q = List::from(p.unwrap()).list_ptr().opt();
            let mut popt = prev_rightmost(q, None);
            while let Some(p) = popt {
                if match CharOrText::from(p) {
                    CharOrText::Char(_) => false,
                    CharOrText::Text(n) => match n {
                        TxtNode::Ins(_)
                        | TxtNode::Mark(_)
                        | TxtNode::Adjust(_)
                        | TxtNode::Penalty(_) => true,
                        TxtNode::Disc(n) if n.is_empty() => true,
                        TxtNode::Math(n) if n.is_empty() => true,
                        TxtNode::Kern(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.is_empty() => true,
                        TxtNode::List(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.param() == (GluePar::right_skip as u16) + 1 => true,
                        _ => false,
                    },
                } {
                    popt = prev_rightmost(q, Some(p));
                } else {
                    break;
                }
            }
            match popt.map(CharOrText::from) {
                Some(CharOrText::Text(TxtNode::MarginKern(m))) if MEM[m.ptr()].b16.s0 == 1 => {
                    format!("{}pt", Scaled(MEM[m.ptr() + 1].b32.s1))
                }
                _ => "0pt".to_string(),
            }
        }
        ConvertCode::JobName => format!(
            "{:#}",
            FileName {
                name: job_name,
                area: 0,
                ext: 0
            }
        ),
        _ => String::new(),
    };
    *LLIST_link(GARBAGE) = str_toks_cat_utf8(&s, cat) as i32;
    begin_token_list(input, llist_link(TEMP_HEAD), Btl::Inserted);
}
/// Returns a pointer to the tail of a new token
/// list, and it also makes `def_ref` point to the reference count at the
/// head of that list.
///
/// There are two boolean parameters, `macro_def` and `xpand`. If `macro_def`
/// is true, the goal is to create the token list for a macro definition;
/// otherwise the goal is to create the token list for some other \TeX
/// primitive: `\mark`, `\output`, `\everypar`, `\lowercase`,
/// `\uppercase`, `\message`, `\errmessage`, `\write`, or
/// `\special`. In the latter cases a left brace must be scanned next; this
/// left brace will not be part of the token list, nor will the matching right
/// brace that comes at the end. If `xpand` is false, the token list will
/// simply be copied from the input using `get_token`. Otherwise all expandable
/// tokens will be expanded until unexpandable tokens are left, except that
/// the results of expanding `\the` are not expanded further.
/// If both `macro_def` and `xpand` are true, the expansion applies
/// only to the macro body (i.e., to the material following the first
/// `left_brace` character).
///
/// The value of `cs` when `scan_toks` begins should be the `EQTB`
/// address of the control sequence to display in "runaway" error
/// messages.
pub(crate) unsafe fn scan_toks(
    input: &mut input_state_t,
    cs: i32,
    macro_def: bool,
    xpand: bool,
) -> usize {
    scanner_status = if macro_def {
        ScannerStatus::Defining
    } else {
        ScannerStatus::Absorbing
    };
    warning_index = cs;
    def_ref = get_avail();
    *token_ref_count(def_ref) = None.tex_int();
    let mut p = def_ref; // tail of the token list being built
    let mut hash_brace = 0; // possible `#{` token
    let mut t = ZERO_TOKEN; // token representing the highest parameter number
    if macro_def {
        // Scan and build the parameter part of the macro definition
        let mut done1 = true;
        let cmd = loop
        /*493: */
        {
            let (mut otok, cmd, chr, _) = get_token(input);
            if otok < RIGHT_BRACE_LIMIT {
                break cmd;
            }
            if cmd == Cmd::MacParam {
                // If the next character is a parameter number, make `tok`
                // a `match` token; but if it is a left brace, store
                // `left_brace`, `end_match`, set `hash_brace`, and goto `'done`
                let s = MATCH_TOKEN + chr; // saved token
                let (tok, cmd, _, _) = get_token(input);
                otok = tok;
                if cmd == Cmd::LeftBrace {
                    hash_brace = tok;
                    store_new_token(&mut p, tok);
                    store_new_token(&mut p, END_MATCH_TOKEN);
                    done1 = false;
                    break cmd;
                } else if t == ZERO_TOKEN + 9 {
                    t_eprint!("You already have nine parameters");
                    help!("I\'m going to ignore the # sign you just used.");
                    error();
                } else {
                    t += 1;
                    if tok != t {
                        t_eprint!("Parameters must be numbered consecutively");
                        help!(
                            "I\'ve inserted the digit you should have used after the #.",
                            "Type `1\' to delete what you did use."
                        );
                        back_error(input, tok);
                    }
                    otok = s
                }
            }
            store_new_token(&mut p, otok);
        };

        if done1 {
            // done1:
            store_new_token(&mut p, END_MATCH_TOKEN);
            if cmd == Cmd::RightBrace {
                // Express shock at the missing left brace; goto `'found`
                t_eprint!("Missing {{ inserted");
                align_state += 1;
                help!(
                    "Where was the left brace? You said something like `\\def\\a}\',",
                    "which I\'m going to interpret as `\\def\\a{}\'."
                );
                error();
                return found(p, hash_brace);
            }
        }
    } else {
        scan_left_brace(input);
        //remove the compulsory left brace
    }

    // Scan and build the body of the token list; goto `'found` when finished
    let mut unbalance = 1; // number of unmatched left braces
    loop {
        let (mut tok, cmd) = if xpand {
            // Expand the next part of the input
            let mut ocmd;
            let mut ochr;
            let mut ocs;
            loop {
                /*497: */
                let (cmd, chr, cs) = get_next(input);
                ocmd = cmd;
                ochr = chr;
                ocs = cs;
                if ocmd >= Cmd::Call
                    && *LLIST_info(*LLIST_link(ochr as usize) as usize) == PROTECTED_TOKEN
                {
                    ocmd = Cmd::Relax;
                    ochr = NO_EXPAND_FLAG;
                }
                if ocmd > MAX_COMMAND {
                    if ocmd == Cmd::The {
                        let q = the_toks(input, ochr, ocs);
                        if let Some(m) = llist_link(TEMP_HEAD) {
                            *LLIST_link(p) = Some(m).tex_int();
                            p = q
                        }
                    } else {
                        expand(input, cmd, chr, cs);
                    }
                } else {
                    break;
                }
            }
            // done2:
            let tok = x_token(input, &mut ocmd, &mut ochr, &mut ocs);
            (tok, ocmd)
        } else {
            let (tok, cmd, ..) = get_token(input);
            (tok, cmd)
        };
        if tok < RIGHT_BRACE_LIMIT {
            if cmd < Cmd::RightBrace {
                unbalance += 1;
            } else {
                unbalance -= 1;
                if unbalance == 0 {
                    return found(p, hash_brace);
                }
            }
        } else if cmd == Cmd::MacParam && macro_def {
            // Look for parameter number or `##`
            let s = tok;
            let next = if xpand {
                get_x_token(input)
            } else {
                get_token(input)
            };
            tok = next.0;
            let cmd = next.1;
            let chr = next.2;
            if cmd != Cmd::MacParam {
                if tok <= ZERO_TOKEN || tok > t {
                    t_eprint!(
                        "Illegal parameter number in definition of {:#}",
                        Cs(warning_index)
                    );
                    help!(
                        "You meant to type ## instead of #, right?",
                        "Or maybe a } was forgotten somewhere earlier, and things",
                        "are all screwed up? I\'m going to assume that you meant ##."
                    );
                    back_error(input, tok);
                    tok = s
                } else {
                    tok = OUT_PARAM_TOKEN - ('0' as i32) + chr
                }
            }
        }
        store_new_token(&mut p, tok);
    }

    unsafe fn found(mut p: usize, hash_brace: i32) -> usize {
        scanner_status = ScannerStatus::Normal;
        if hash_brace != 0 {
            store_new_token(&mut p, hash_brace);
        }
        p
    }
}

/// The `read_toks` procedure constructs a token list like that for any
/// macro definition, and makes `cur_val` point to it. Parameter `r` points
/// to the control sequence that will receive this token list.
pub(crate) unsafe fn read_toks(input: &mut input_state_t, n: i32, r: i32, j: i32) -> i32 {
    scanner_status = ScannerStatus::Defining;
    warning_index = r;
    def_ref = get_avail();
    *token_ref_count(def_ref) = None.tex_int();
    let mut p = def_ref; // tail of the token list
    store_new_token(&mut p, END_MATCH_TOKEN);
    let m = if n < 0 || n > 15 { 16 } else { n as i16 }; // stream number
    let s = align_state; // saved value of `align_state`
    align_state = 1000000; // disable tab marks, etc.
    loop {
        // Input and store tokens from the next line of the file
        begin_file_reading(input);
        input.name = m as i32 + 1;
        assert!(
            read_open[m as usize] != OpenMode::Closed,
            /*503:*/
            "terminal input forbidden"
        );
        /*505:*/
        if read_open[m as usize] == OpenMode::JustOpen {
            // Input the first line of `read_file[m]`
            if read_file[m as usize].as_mut().unwrap().input_line() {
                read_open[m as usize] = OpenMode::Normal;
            } else {
                let _ = read_file[m as usize].take();
                read_open[m as usize] = OpenMode::Closed;
            }
        } else if !read_file[m as usize].as_mut().unwrap().input_line() {
            // Input the next line of |read_file[m]|
            let _ = read_file[m as usize].take();
            read_open[m as usize] = OpenMode::Closed;
            if align_state as i64 != 1000000 {
                runaway();
                t_eprint!("File ended within {}", Esc("read"));
                help!("This \\read has unbalanced braces.");
                align_state = 1000000;
                error();
            }
        }
        input.limit = last;
        if get_int_par(IntPar::end_line_char) < 0 || get_int_par(IntPar::end_line_char) > 255 {
            input.limit -= 1
        } else {
            BUFFER[input.limit] = get_int_par(IntPar::end_line_char)
        }
        first = input.limit + 1;
        input.loc = input.start;
        input.state = InputState::NewLine;
        // Handle `\readline` and goto `'done`
        let limit = input.limit;
        let loc = input.loc.as_mut().unwrap();
        if j == 1 {
            while *loc <= limit {
                // current line not yet finished
                let chr = BUFFER[*loc];
                *loc += 1;
                let tok = if chr == ' ' as i32 {
                    SPACE_TOKEN
                } else {
                    chr + OTHER_TOKEN
                };
                store_new_token(&mut p, tok);
            }
        } else {
            loop {
                let mut tok = get_token(input).0;
                if tok == 0 {
                    break;
                    // `cmd=chr=0` will occur at the end of the line
                }
                if (align_state as i64) < 1000000 {
                    // unmatched `}` aborts the line
                    loop {
                        tok = get_token(input).0;
                        if tok == 0 {
                            break;
                        }
                    }
                    align_state = 1000000;
                    break;
                } else {
                    store_new_token(&mut p, tok);
                }
            }
        }
        end_file_reading(input);
        if align_state as i64 == 1000000 {
            break;
        }
    }
    scanner_status = ScannerStatus::Normal;
    align_state = s;
    def_ref as i32
}
pub(crate) unsafe fn pass_text(input: &mut input_state_t) -> (Cmd, i32, i32) {
    let save_scanner_status = scanner_status;
    scanner_status = ScannerStatus::Skipping;
    let mut l = 0;
    skip_line = line;
    let mut cmd;
    let mut chr;
    let mut cs;
    loop {
        let next = get_next(input);
        cmd = next.0;
        chr = next.1;
        cs = next.2;
        if cmd == Cmd::FiOrElse {
            if l == 0 {
                break;
            }
            if chr == FiOrElseCode::Fi as i32 {
                l -= 1;
            }
        } else if cmd == Cmd::IfTest {
            l += 1;
        }
    }
    scanner_status = save_scanner_status;
    if get_int_par(IntPar::tracing_ifs) > 0 {
        show_cur_cmd_chr(cmd, chr);
    };
    (cmd, chr, cs)
}
pub(crate) unsafe fn change_if_limit(l: FiOrElseCode, p: Option<usize>) {
    if p == cond_ptr {
        if_limit = l;
    } else {
        let mut qopt = cond_ptr;
        loop {
            let q = qopt.confuse("if");
            if llist_link(q) == p {
                MEM[q].b16.s1 = l as u16;
                return;
            }
            qopt = llist_link(q);
        }
    };
}
pub(crate) unsafe fn conditional(input: &mut input_state_t, cmd: Cmd, chr: i32) {
    let mut b: bool = false;

    if get_int_par(IntPar::tracing_ifs) > 0 && get_int_par(IntPar::tracing_commands) <= 1 {
        show_cur_cmd_chr(cmd, chr);
    }

    let p = get_node(IF_NODE_SIZE);
    *LLIST_link(p) = cond_ptr.tex_int();
    MEM[p].b16.s1 = if_limit as u16;
    MEM[p].b16.s0 = cur_if as u16;
    MEM[p + 1].b32.s1 = if_line;
    cond_ptr = Some(p);
    cur_if = chr as i16;
    if_limit = FiOrElseCode::If;
    if_line = line;

    let save_cond_ptr = cond_ptr;
    let is_unless = chr >= UNLESS_CODE;
    let this_if = IfTestCode::n((chr % UNLESS_CODE) as u8).unwrap();

    match this_if {
        IfTestCode::IfChar | IfTestCode::IfCat => {
            // Test if two characters match
            unsafe fn get_x_token_or_active_char(input: &mut input_state_t) -> (Cmd, i32) {
                let (tok, cmd, chr, _) = get_x_token(input);
                if cmd == Cmd::Relax && chr == NO_EXPAND_FLAG {
                    return (Cmd::ActiveChar, tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32));
                }
                (cmd, chr)
            }
            let (cmd, chr) = get_x_token_or_active_char(input);

            let (m, n) = if cmd > Cmd::ActiveChar || chr > BIGGEST_USV as i32 {
                // not a character
                (Cmd::Relax, TOO_BIG_USV as i32)
            } else {
                (cmd, chr)
            };

            let (tok, mut cmd, mut chr, _) = get_x_token(input);

            if cmd == Cmd::Relax && chr == NO_EXPAND_FLAG {
                cmd = Cmd::ActiveChar;
                chr = tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32);
            }

            if cmd > Cmd::ActiveChar || chr > BIGGEST_USV as i32 {
                cmd = Cmd::Relax;
                chr = TOO_BIG_USV as i32;
            }

            if this_if == IfTestCode::IfChar {
                b = n == chr
            } else {
                b = m == cmd
            }
        }
        IfTestCode::IfInt | IfTestCode::IfDim => {
            // Test relation between integers or dimensions
            let n = if this_if == IfTestCode::IfInt {
                scan_int(input)
            } else {
                scan_dimen(input, false, false, None).0
            };

            let tok = loop {
                let (tok, cmd, ..) = get_x_token(input);
                if cmd != Cmd::Spacer {
                    break tok;
                }
            };

            let r = if tok >= OTHER_TOKEN + 60 && tok <= OTHER_TOKEN + 62 {
                (tok - OTHER_TOKEN) as u8
            } else {
                t_eprint!(
                    "Missing = inserted for {}",
                    CmdChr(Cmd::IfTest, this_if as i32)
                );
                help!("I was expecting to see `<\', `=\', or `>\'. Didn\'t.");
                back_error(input, tok);
                b'='
            };

            let val = if this_if == IfTestCode::IfInt {
                scan_int(input)
            } else {
                scan_dimen(input, false, false, None).0
            };

            match r {
                b'<' => {
                    /*"<"*/
                    b = n < val
                }
                b'=' => {
                    /*"="*/
                    b = n == val
                }
                b'>' => {
                    /*">"*/
                    b = n > val
                }
                _ => {}
            }
        }

        IfTestCode::IfOdd => {
            let val = scan_int(input);
            b = val & 1 != 0;
        }

        IfTestCode::IfVMode => {
            b = cur_list.mode.1 == ListMode::VMode;
        }

        IfTestCode::IfHMode => {
            b = cur_list.mode.1 == ListMode::HMode;
        }

        IfTestCode::IfMMode => {
            b = cur_list.mode.1 == ListMode::MMode;
        }

        IfTestCode::IfInner => {
            b = cur_list.mode.0;
        }

        IfTestCode::IfVoid | IfTestCode::IfHBox | IfTestCode::IfVBox => {
            let val = scan_register_num(input);
            let p = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|cp| MEM[cp + 1].b32.s1.opt())
            };
            b = match (this_if, p.map(TxtNode::from)) {
                (IfTestCode::IfVoid, None) => true,
                (IfTestCode::IfHBox, Some(TxtNode::List(l))) if l.is_horizontal() => true,
                (IfTestCode::IfVBox, Some(TxtNode::List(l))) if l.is_vertical() => true,
                _ => false,
            };
        }

        IfTestCode::Ifx => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            let (p, q, n) = get_next(input);
            let (cmd, chr, _) = get_next(input);

            b = if cmd != p {
                false
            } else if cmd < Cmd::Call {
                chr == q
            } else {
                let mut popt = LLIST_link(chr as usize).opt();
                let mut qopt = MEM[EQTB[n as usize].val as usize].b32.s1.opt();
                if popt == qopt {
                    true
                } else {
                    while let (Some(p), Some(q)) = (popt, qopt) {
                        if MEM[p].b32.s0 != MEM[q as usize].b32.s0 {
                            popt = None
                        } else {
                            popt = llist_link(p);
                            qopt = llist_link(q);
                        }
                    }
                    popt.is_none() && qopt.is_none()
                }
            };

            scanner_status = save_scanner_status;
        }

        IfTestCode::IfEof => {
            let val = scan_four_bit_int_or_18(input);
            b = if val == 18 {
                true
            } else {
                read_open[val as usize] == OpenMode::Closed
            };
        }

        IfTestCode::IfTrue => {
            b = true;
        }

        IfTestCode::IfFalse => {
            b = false;
        }

        IfTestCode::IfDef => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            let (cmd, _, _) = get_next(input);
            b = cmd != Cmd::UndefinedCS;
            scanner_status = save_scanner_status;
        }

        IfTestCode::IfCS => {
            let n = get_avail();
            let mut p = n; // head of the list of characters
            let e = is_in_csname;
            is_in_csname = true;

            let (tok, cmd) = loop {
                let (tok, cmd, _, cs) = get_x_token(input);
                if cs == 0 {
                    store_new_token(&mut p, tok);
                } else {
                    break (tok, cmd);
                }
            };

            if cmd != Cmd::EndCSName {
                // Complain about missing `\endcsname`
                t_eprint!("Missing {} inserted", Esc("endcsname"));
                help!(
                    "The control sequence marked <to be read again> should",
                    "not appear between \\csname and \\endcsname."
                );
                back_error(input, tok);
            }

            // Look up the characters of list `n` in the hash table, and set `cs`
            let mut m = first;
            let mut popt = llist_link(n);

            while let Some(p) = popt {
                if m >= max_buf_stack {
                    max_buf_stack = m + 1;
                    if max_buf_stack as usize == BUF_SIZE {
                        overflow("buffer size", BUF_SIZE);
                    }
                    // TeX capacity exceeded buffer size
                }
                BUFFER[m as usize] = MEM[p as usize].b32.s0 % MAX_CHAR_VAL;
                m += 1;
                popt = llist_link(p);
            }

            let cs = if m == first {
                NULL_CS as i32
            } else if m == first + 1 {
                SINGLE_BASE as i32 + BUFFER[first as usize]
            } else {
                id_lookup(first as usize, (m - first) as usize)
                // `no_new_control_sequence` is `true`
            };

            flush_list(Some(n));
            b = eq_type(cs as usize) != Cmd::UndefinedCS;
            is_in_csname = e;
        }

        IfTestCode::IfInCSName => {
            b = is_in_csname;
        }

        IfTestCode::IfFontChar => {
            let n = scan_font_ident(input) as usize;
            let val = scan_usv_num(input);
            b = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[n] {
                map_char_to_glyph(nf, std::char::from_u32(val as u32).unwrap()) > 0
            } else if FONT_BC[n] as i32 <= val && FONT_EC[n] as i32 >= val {
                FONT_CHARACTER_INFO(n, effective_char(true, n, val as u16) as usize).s3 > 0
            } else {
                false
            };
        }

        IfTestCode::IfCase => {
            let mut n = scan_int(input);

            if get_int_par(IntPar::tracing_commands) > 1 {
                diagnostic(false, || {
                    t_print!("{{case {}}}", n);
                });
            }

            loop {
                if n == 0 {
                    break;
                }

                let chr = pass_text(input).1;

                if cond_ptr == save_cond_ptr {
                    if chr == FiOrElseCode::Or as i32 {
                        n -= 1;
                    } else {
                        return common_ending(input, chr);
                    }
                } else if chr == FiOrElseCode::Fi as i32 {
                    /*515:*/
                    if IF_STACK[IN_OPEN] == cond_ptr {
                        INPUT_STACK[INPUT_PTR] = *input;
                        if_warning(input, &INPUT_STACK[..INPUT_PTR + 1]);
                    }
                    let p = cond_ptr.unwrap();
                    if_line = MEM[p + 1].b32.s1;
                    cur_if = MEM[p].b16.s0 as i16;
                    if_limit = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
                    cond_ptr = llist_link(p);
                    free_node(p, IF_NODE_SIZE);
                }
            }
            change_if_limit(FiOrElseCode::Or, save_cond_ptr);
            return;
        }
        IfTestCode::IfPrimitive => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            let (cmd, chr, cs) = get_next(input);
            scanner_status = save_scanner_status;
            let m = if cs < HASH_BASE as i32 {
                prim_lookup(cs - SINGLE_BASE as i32)
            } else {
                prim_lookup(yhash[cs as usize - hash_offset].s1)
            } as i32;
            b = cmd != Cmd::UndefinedCS
                && m != UNDEFINED_PRIMITIVE
                && cmd == Cmd::from(prim_eqtb[m as usize].cmd)
                && chr == prim_eqtb[m as usize].val;
        }
    }

    if is_unless {
        b = !b
    }

    if get_int_par(IntPar::tracing_commands) > 1 {
        /*521:*/
        diagnostic(false, || {
            if b {
                t_print!("{{true}}")
            } else {
                t_print!("{{false}}")
            }
        });
    }

    if b {
        change_if_limit(FiOrElseCode::Else, save_cond_ptr);
        return;
    }

    loop {
        let chr = pass_text(input).1;

        if cond_ptr == save_cond_ptr {
            if chr != FiOrElseCode::Or as i32 {
                return common_ending(input, chr);
            }

            t_eprint!("Extra {}", Esc("or"));
            help!("I\'m ignoring this; it doesn\'t match any \\if.");
            error();
        } else if chr == FiOrElseCode::Fi as i32 {
            /*515:*/
            if IF_STACK[IN_OPEN] == cond_ptr {
                INPUT_STACK[INPUT_PTR] = *input;
                if_warning(input, &INPUT_STACK[..INPUT_PTR + 1]);
            }
            let p = cond_ptr.unwrap();
            if_line = MEM[p + 1].b32.s1;
            cur_if = MEM[p].b16.s0 as i16;
            if_limit = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
            cond_ptr = llist_link(p);
            free_node(p, IF_NODE_SIZE);
        }
    }

    unsafe fn common_ending(input: &mut input_state_t, chr: i32) {
        if chr == FiOrElseCode::Fi as i32 {
            /*515:*/
            if IF_STACK[IN_OPEN] == cond_ptr {
                INPUT_STACK[INPUT_PTR] = *input;
                if_warning(input, &INPUT_STACK[..INPUT_PTR + 1]);
            }
            let p = cond_ptr.unwrap();
            if_line = MEM[p + 1].b32.s1;
            cur_if = MEM[p].b16.s0 as i16;
            if_limit = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
            cond_ptr = llist_link(p);
            free_node(p, IF_NODE_SIZE);
        } else {
            if_limit = FiOrElseCode::Fi;
        }
    }
}
pub(crate) unsafe fn more_name(
    c: UTF16_code,
    stop_at_space_: bool,
    area_delimiter: &mut usize,
    ext_delimiter: &mut usize,
    quoted_filename: &mut bool,
    file_name_quote_char: &mut Option<u8>,
) -> bool {
    if stop_at_space_ && file_name_quote_char.is_none() && c == b' ' as _ {
        return false;
    }
    if stop_at_space_ && file_name_quote_char.map(|qc| qc as _) == Some(c) {
        *file_name_quote_char = None;
        return true;
    }
    if stop_at_space_ && file_name_quote_char.is_none() && (c == b'\"' as _ || c == b'\'' as _) {
        *file_name_quote_char = Some(c as u8);
        *quoted_filename = true;
        return true;
    }
    PoolString::check_capacity(1);
    str_pool[pool_ptr] = c;
    pool_ptr += 1;
    if c == b'/' as u16 {
        // IS_DIR_SEP
        *area_delimiter = PoolString::current().len();
        *ext_delimiter = 0;
    } else if c == b'.' as u16 {
        *ext_delimiter = PoolString::current().len();
    }
    true
}

#[derive(Clone, Debug)]
pub(crate) struct FileName {
    pub name: str_number,
    pub area: str_number,
    pub ext: str_number,
}

pub(crate) unsafe fn make_name<F>(mut f: F) -> (FileName, bool, Option<u8>)
where
    F: FnMut(&mut usize, &mut usize, &mut bool, &mut Option<u8>),
{
    let mut area_delimiter = 0;
    let mut ext_delimiter = 0;
    let mut quoted_filename = false;
    let mut file_name_quote_char = None;

    f(
        &mut area_delimiter,
        &mut ext_delimiter,
        &mut quoted_filename,
        &mut file_name_quote_char,
    );

    if str_ptr + 3 > max_strings as i32 {
        overflow("number of strings", max_strings - init_str_ptr as usize);
    }
    /* area_delimiter is the length from the start of the filename to the
     * directory seperator "/", which we use to construct the stringpool
     * string `area`. If there was already a string in the stringpool for
     * the area, reuse it. */
    let mut name;
    let mut area;
    let ext;
    if area_delimiter == 0 {
        area = EMPTY_STRING as str_number
    } else {
        area = str_ptr;
        str_start[((str_ptr + 1) - TOO_BIG_CHAR) as usize] =
            str_start[(str_ptr - TOO_BIG_CHAR) as usize] + area_delimiter;
        str_ptr += 1;
        if let Some(temp_str) = search_string(area) {
            area = temp_str;
            str_ptr -= 1;
            for j in str_start[((str_ptr + 1) - TOO_BIG_CHAR) as usize]..=(pool_ptr - 1) {
                str_pool[(j - area_delimiter) as usize] = str_pool[j];
            }
            pool_ptr -= area_delimiter
        }
    }
    /* ext_delimiter is the length from the start of the filename to the
     * extension '.' delimiter, which we use to construct the stringpool
     * strings `ext` and `name`. */
    if ext_delimiter == 0 {
        ext = EMPTY_STRING as str_number;
        name = slow_make_string()
    } else {
        name = str_ptr;
        str_start[((str_ptr + 1) - TOO_BIG_CHAR) as usize] =
            str_start[(str_ptr - TOO_BIG_CHAR) as usize] + ext_delimiter - area_delimiter - 1;
        str_ptr += 1;
        let _ext = make_string();
        str_ptr -= 1;
        if let Some(temp_str) = search_string(name) {
            name = temp_str;
            str_ptr -= 1;
            for j in str_start[((str_ptr + 1) - TOO_BIG_CHAR) as usize]..=(pool_ptr - 1) {
                str_pool[(j - ext_delimiter + area_delimiter + 1) as usize] = str_pool[j];
            }
            pool_ptr = pool_ptr - ext_delimiter + area_delimiter + 1;
        }
        ext = slow_make_string()
    };
    (
        FileName { name, area, ext },
        quoted_filename,
        file_name_quote_char,
    )
}

pub(crate) unsafe fn make_name_string(name: &str) -> str_number {
    if pool_ptr as usize + name.len() > pool_size as usize
        || str_ptr == max_strings as i32
        || PoolString::current().len() > 0
    {
        return '?' as i32;
    }
    let name_of_file16: Vec<u16> = name.encode_utf16().collect();
    for &k in &name_of_file16 {
        str_pool[pool_ptr] = k;
        pool_ptr += 1;
    }
    let result = make_string();
    let save_name_in_progress = name_in_progress;
    name_in_progress = true;
    let res = make_name(|a, e, q, qc| {
        for &k in &name_of_file16 {
            if !more_name(k, false, a, e, q, qc) {
                break;
            }
        }
    });
    cur_name = res.0.name;
    name_in_progress = save_name_in_progress;
    result
}

pub(crate) unsafe fn scan_file_name(input: &mut input_state_t) -> (FileName, bool, Option<u8>) {
    name_in_progress = true;
    let res = make_name(|a, e, q, qc| {
        let (mut tok, mut cmd, mut chr, _) = loop {
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer) {
                break next;
            }
        };
        loop {
            if cmd > Cmd::OtherChar || chr >= TOO_BIG_CHAR {
                back_input(input, tok);
                break;
            } else {
                if !more_name(chr as UTF16_code, stop_at_space, a, e, q, qc) {
                    break;
                }
                let next = get_x_token(input);
                tok = next.0;
                cmd = next.1;
                chr = next.2;
            }
        }
    });
    name_in_progress = false;
    res
}
pub(crate) unsafe fn pack_job_name(ext: &str) -> String {
    maketexstring(ext); // NOTE: format file only
    gettexstring(job_name) + ext
}
pub(crate) unsafe fn open_log_file() {
    let old_setting = selector;
    if job_name == 0 {
        job_name = maketexstring("texput")
    }
    let log_name = pack_job_name(".log");
    log_file = OutputHandleWrapper::open(&log_name, 0);
    if log_file.is_none() {
        abort!("cannot open log file output \"{}\"", log_name);
    }
    texmf_log_name = make_name_string(&log_name);
    selector = Selector::LOG_ONLY;
    log_opened = true;
    INPUT_STACK[INPUT_PTR] = cur_input;
    /* Here we catch the log file up with anything that has already been
     * printed. The eqtb reference is end_line_char. */
    t_print_nl!("**");
    let mut l = INPUT_STACK[0].limit;
    if BUFFER[l as usize] == get_int_par(IntPar::end_line_char) {
        l -= 1
    }
    let mut k = 1;
    while k <= l {
        t_print!(
            "{}",
            std::char::from_u32(BUFFER[k as usize] as u32).unwrap()
        );
        k += 1;
    }
    print_ln();
    selector = match old_setting {
        Selector::NO_PRINT => Selector::LOG_ONLY,
        Selector::TERM_ONLY => Selector::TERM_AND_LOG,
        _ => unreachable!(),
    };
}

/// `\TeX` will `\input` something
///
/// Let's turn now to the procedure that is used to initiate file reading
/// when an `\input` command is being processed.
pub(crate) unsafe fn start_input(input: &mut input_state_t, primary_input_name: &str) {
    let mut format = TTInputFormat::TEX;
    let file = if !primary_input_name.is_empty() {
        /* If this is the case, we're opening the primary input file, and the
         * name that we should use to refer to it has been handed directly to
         * us. We emulate the hacks used below to fill in cur_name, etc., from
         * a UTF-8 C string. It looks like the `cur_{name,area,ext}` strings
         * are hardly used so it'd be nice to get rid of them someday. */
        format = TTInputFormat::TECTONIC_PRIMARY;
        name_in_progress = true;
        let res = make_name(|area_delimiter, ext_delimiter, _, _| {
            stop_at_space = false;
            PoolString::check_capacity(primary_input_name.len() * 2);
            for rval in primary_input_name.chars() {
                let mut b = [0; 2];
                for i in rval.encode_utf16(&mut b) {
                    if pool_ptr < pool_size {
                        str_pool[pool_ptr as usize] = *i;
                        pool_ptr += 1
                    }
                }
                if rval == '/' {
                    *area_delimiter = PoolString::current().len();
                    *ext_delimiter = 0;
                } else if rval == '.' {
                    *ext_delimiter = PoolString::current().len();
                }
            }
            stop_at_space = true;
        });
        name_in_progress = false;
        res.0
    } else {
        /* Scan in the file name from the current token stream. The file name to
         * input is saved as the stringpool strings `cur_{name,area,ext}` and the
         * UTF-8 string `filename`. */
        scan_file_name(input).0
    };
    let filename = file.to_string();
    begin_file_reading(input);
    let ufile = u_open_in(
        &filename,
        format,
        b"rb",
        UnicodeMode::from(get_int_par(IntPar::xetex_default_input_mode)),
        get_int_par(IntPar::xetex_default_input_encoding),
    );
    if ufile.is_none() {
        abort!("failed to open input file \"{}\"", filename);
    }
    INPUT_FILE[cur_input.index as usize] = ufile;
    /* Now re-encode `filename` into the UTF-16 variable `name_of_file16`,
     * and use that to recompute `cur_{name,area,ext}`. */
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
    /* Now generate a stringpool string corresponding to the full path of the
     * input file. This calls make_utf16_name() again and reruns through the
     * {begin,more,end}_name() trifecta to re-re-compute
     * `cur_{name,area,ext}`. */
    input.name = make_name_string(&filename);
    SOURCE_FILENAME_STACK[IN_OPEN] = input.name;
    /* *This* variant is a TeX string made out of `name_of_input_file`. */
    FULL_SOURCE_FILENAME_STACK[IN_OPEN] = maketexstring(&name_of_input_file);
    if input.name == str_ptr - 1 {
        // we can conserve string pool space now
        if let Some(temp_str) = search_string(input.name) {
            input.name = temp_str;
            PoolString::flush();
        }
    }
    /* Finally we start really doing stuff with the newly-opened file. */
    if job_name == 0 {
        job_name = cur_name; /* this is the "flush_string" macro which discards the most recent string */
        open_log_file(); /* "really a CFDictionaryRef or *mut XeTeXLayoutEngine" */
    } /* = first_math_fontdimen (=10) + lastMathConstant (= radicalDegreeBottomRaisePercent = 55) */
    let fname = PoolString::from(FULL_SOURCE_FILENAME_STACK[IN_OPEN]);
    if term_offset + (fname.len() as i32) > max_print_line - 2 {
        print_ln();
    } else if term_offset > 0 || file_offset > 0 {
        t_print!(" ");
    }
    open_parens += 1;
    t_print!("({}", fname);
    rust_stdout.as_mut().unwrap().flush().unwrap();
    input.state = InputState::NewLine;
    synctex_start_input(input);

    // Read the first line of the new file
    line = 1;
    INPUT_FILE[input.index as usize]
        .as_mut()
        .unwrap()
        .input_line();
    input.limit = last;
    if get_int_par(IntPar::end_line_char) < 0 || get_int_par(IntPar::end_line_char) > 255 {
        input.limit -= 1
    } else {
        BUFFER[input.limit] = get_int_par(IntPar::end_line_char)
    }
    first = input.limit + 1;
    input.loc = input.start;

    // Here we have to remember to tell the |input_ln| routine not to
    // start with a |get|. If the file is empty, it is considered to
    // contain a single blank line.
}
pub(crate) unsafe fn effective_char_info(f: internal_font_number, mut c: u16) -> b16x4 {
    if !xtx_ligature_present && !(FONT_MAPPING[f]).is_null() {
        c = apply_tfm_font_mapping(FONT_MAPPING[f], c as i32) as u16
    }
    xtx_ligature_present = false;
    FONT_CHARACTER_INFO(f, c as usize)
}
pub(crate) unsafe fn char_warning(f: internal_font_number, c: char) {
    if get_int_par(IntPar::tracing_lost_chars) > 0 {
        let old_setting = get_int_par(IntPar::tracing_online);
        if get_int_par(IntPar::tracing_lost_chars) > 1 {
            set_int_par(IntPar::tracing_online, 1);
        }
        diagnostic(false, || {
            t_print_nl!(
                "Missing character: There is no {} in font {}!",
                c,
                PoolString::from(FONT_NAME[f])
            );
        });
        set_int_par(IntPar::tracing_online, old_setting);
    }
    let fn_0 = gettexstring(FONT_NAME[f]);
    ttstub_issue_warning(&format!(
        "could not represent character \"{}\" (0x{:x}) in font \"{}\"",
        c, c as u32, fn_0
    ));
    if !gave_char_warning_help {
        ttstub_issue_warning(
            "  you may need to load the `fontspec` package and use (e.g.) \\setmainfont to",
        );
        ttstub_issue_warning(
            "  choose a different font that covers the unrepresentable character(s)",
        );
        gave_char_warning_help = true
    };
}
pub(crate) unsafe fn new_native_word_node(f: usize, n: i32) -> NativeWord {
    let l = (NATIVE_NODE_SIZE as u64).wrapping_add(
        ((n as u64) * (std::mem::size_of::<u16>() as u64)
            + (std::mem::size_of::<memory_word>() as u64)
            - 1)
            / (std::mem::size_of::<memory_word>() as u64),
    ) as i32;
    let mut q = NativeWord::from(get_node(l) as usize);
    set_NODE_type(q.ptr(), TextNode::WhatsIt);
    q.set_actual_text(get_int_par(IntPar::xetex_generate_actual_text) > 0);
    q.set_size(l as u16)
        .set_font(f as u16)
        .set_length(n as u16)
        .set_glyph_count(0)
        .set_glyph_info_ptr(ptr::null_mut());
    q
}
pub(crate) unsafe fn new_native_character(f: internal_font_number, c: char) -> NativeWord {
    let mut p;
    let nf = match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(nf) => nf,
        _ => panic!("Not native font"),
    };
    if !(FONT_MAPPING[f]).is_null() {
        let mut buf = [0; 2];
        let b = c.encode_utf16(&mut buf);
        PoolString::check_capacity(b.len());
        for c16 in b {
            str_pool[pool_ptr] = *c16;
            pool_ptr += 1
        }

        let mapped_text = apply_mapping(FONT_MAPPING[f], PoolString::current().as_slice());
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];

        for chr in std::char::decode_utf16(mapped_text.iter().cloned()) {
            let chr = chr.unwrap();
            if map_char_to_glyph(nf, chr) == 0 {
                char_warning(f, chr);
            }
        }

        p = new_native_word_node(f, mapped_text.len() as _);
        p.text_mut().copy_from_slice(&mapped_text[..]);
    } else {
        if get_int_par(IntPar::tracing_lost_chars) > 0 && map_char_to_glyph(nf, c) == 0 {
            char_warning(f, c);
        }
        p = NativeWord::from(get_node(NATIVE_NODE_SIZE + 1));
        set_NODE_type(p.ptr(), TextNode::WhatsIt);
        p.set_actual_text(false);
        p.set_size((NATIVE_NODE_SIZE + 1) as u16);
        p.set_glyph_count(0);
        p.set_glyph_info_ptr(ptr::null_mut());
        p.set_font(f as u16);

        let mut buf = [0; 2];
        let s16 = c.encode_utf16(&mut buf);
        p.set_length(s16.len() as _);
        let text = p.text_mut();
        for (i, c16) in s16.iter().enumerate() {
            text[i] = *c16;
        }
    }
    p.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
    p
}
pub(crate) unsafe fn font_feature_warning(feature_name: &[u8], setting_name: &[u8]) {
    diagnostic(false, || {
        t_print_nl!("Unknown ");
        if !setting_name.is_empty() {
            t_print!(
                "selector `{}\' for ",
                std::str::from_utf8(setting_name).unwrap()
            );
        }
        t_print!(
            "feature `{}\' in font `{}\'.",
            std::str::from_utf8(feature_name).unwrap(),
            name_of_font
        );
    });
}
pub(crate) unsafe fn font_mapping_warning(mapping_name: &str, warningType: i32) {
    diagnostic(false, || {
        if warningType == 0 {
            t_print_nl!(
                "Loaded mapping `{}\' for font `{}",
                mapping_name,
                name_of_font
            );
        } else {
            t_print_nl!(
                "Font mapping `{}\' for font `{}",
                mapping_name,
                name_of_font
            );
        }
        match warningType {
            1 => t_print!("\' not found."),
            2 => {
                t_print!("\' not usable;");
                t_print_nl!("bad mapping file or incorrect mapping type.");
            }
            _ => t_print!("\'."),
        }
    });
}
/*pub(crate) unsafe fn graphite_warning() {
    diagnostic(false, || {
        t_print_nl!(
            "Font `{}\' does not support Graphite. Trying OpenType layout instead.",
            name_of_font
        );
    });
}*/
pub(crate) unsafe fn do_locale_linebreaks(text: &[u16]) {
    if get_int_par(IntPar::xetex_linebreak_locale) == 0 || text.len() == 1 {
        let mut nwn = new_native_word_node(main_f, text.len() as _);
        *LLIST_link(cur_list.tail) = Some(nwn.ptr()).tex_int();
        cur_list.tail = nwn.ptr();
        let tail_text = nwn.text_mut();
        tail_text.copy_from_slice(text);
        nwn.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
    } else {
        let use_skip = get_glue_par(GluePar::xetex_linebreak_skip).ptr() != 0;
        let use_penalty = get_int_par(IntPar::xetex_linebreak_penalty) != 0 || !use_skip;
        linebreak_start(main_f, get_int_par(IntPar::xetex_linebreak_locale), text);
        let mut offs = 0;
        loop {
            let prevOffs = offs;
            offs = linebreak_next();
            if offs > 0 {
                if prevOffs != 0 {
                    if use_penalty {
                        let pen = new_penalty(get_int_par(IntPar::xetex_linebreak_penalty));
                        *LLIST_link(cur_list.tail) = Some(pen.ptr()).tex_int();
                        cur_list.tail = pen.ptr();
                    }
                    if use_skip {
                        let pg = new_param_glue(GluePar::xetex_linebreak_skip);
                        *LLIST_link(cur_list.tail) = Some(pg.ptr()).tex_int();
                        cur_list.tail = pg.ptr();
                    }
                }
                let mut nwn = new_native_word_node(main_f, offs - prevOffs);
                *LLIST_link(cur_list.tail) = Some(nwn.ptr()).tex_int();
                cur_list.tail = nwn.ptr();

                let tail_text = nwn.text_mut();
                tail_text.copy_from_slice(&text[prevOffs as usize..offs as usize]);

                nwn.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
            }
            if offs < 0 {
                break;
            }
        }
    };
}
pub(crate) unsafe fn bad_utf8_warning() {
    diagnostic(false, || {
        if cur_input.name == 0 {
            t_print_nl!("Invalid UTF-8 byte or sequence in terminal input replaced by U+FFFD.");
        } else {
            t_print_nl!(
                "Invalid UTF-8 byte or sequence at line {} replaced by U+FFFD.",
                line
            );
        }
    });
}
pub(crate) unsafe fn get_input_normalization_state() -> i32 {
    if EQTB.is_empty() {
        0
    } else {
        get_int_par(IntPar::xetex_input_normalization)
    }
}
pub(crate) unsafe fn get_tracing_fonts_state() -> i32 {
    get_int_par(IntPar::xetex_tracing_fonts)
}

pub(crate) unsafe fn new_character(f: internal_font_number, c: UTF16_code) -> Option<usize> {
    if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
        let chr = std::char::from_u32(c as u32).unwrap();
        return Some(new_native_character(f, chr).ptr());
    }
    let ec = effective_char(false, f, c) as u16;
    if FONT_BC[f] as i32 <= ec as i32
        && FONT_EC[f] as i32 >= ec as i32
        && FONT_CHARACTER_INFO(f, ec as usize).s3 > 0
    {
        let mut p = Char(get_avail());
        p.set_font(f as u16);
        p.set_character(c);
        return Some(p.ptr());
    }
    let chr = std::char::from_u32(c as u32).unwrap();
    char_warning(f, chr);
    None
}
pub(crate) unsafe fn scan_spec(input: &mut input_state_t, c: GroupCode, three_codes: bool) -> Cmd {
    let mut s: i32 = 0;
    if three_codes {
        s = SAVE_STACK[SAVE_PTR + 0].val
    }
    let spec_code;
    let mut sd = None;
    if scan_keyword(input, "to") {
        spec_code = PackMode::Exactly;
    } else if scan_keyword(input, "spread") {
        spec_code = PackMode::Additional;
    } else {
        spec_code = PackMode::Additional;
        sd = Some(Scaled::ZERO);
    }
    let val = sd.unwrap_or_else(|| scan_dimen(input, false, false, None));
    // found
    if three_codes {
        SAVE_STACK[SAVE_PTR + 0].val = s;
        SAVE_PTR += 1;
    }
    SAVE_STACK[SAVE_PTR + 0].val = spec_code as i32;
    SAVE_STACK[SAVE_PTR + 1].val = val.0;
    SAVE_PTR += 2;
    new_save_level(c);
    scan_left_brace(input).1
}
pub(crate) unsafe fn char_pw(p: Option<usize>, side: Side) -> Scaled {
    if side == Side::Left {
        last_leftmost_char = None.tex_int()
    } else {
        last_rightmost_char = None.tex_int()
    }
    if p.is_none() {
        return Scaled::ZERO;
    }
    let p = match CharOrText::from(p.unwrap()) {
        CharOrText::Char(p) => p,
        CharOrText::Text(TxtNode::Ligature(p)) => p.as_char(),
        CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw))) => {
            return if !(nw.glyph_info_ptr()).is_null() {
                let f = nw.font() as internal_font_number;
                round_xn_over_d(
                    Scaled(FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1),
                    nw.native_word_cp(side),
                    1000,
                )
            } else {
                Scaled::ZERO
            };
        }
        CharOrText::Text(TxtNode::WhatsIt(WhatsIt::Glyph(g))) => {
            let f = g.font() as internal_font_number;
            return round_xn_over_d(
                Scaled(FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1),
                get_cp_code(f, g.glyph() as u32, side),
                1000,
            );
        }
        _ => return Scaled::ZERO,
    };
    let f = p.font() as internal_font_number;
    let c = get_cp_code(f, p.character() as u32, side);
    match side {
        Side::Left => last_leftmost_char = Some(p.ptr()).tex_int(),
        Side::Right => last_rightmost_char = Some(p.ptr()).tex_int(),
    }
    if c == 0 {
        return Scaled::ZERO;
    }
    round_xn_over_d(
        Scaled(FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1),
        c,
        1000,
    )
}
pub(crate) unsafe fn new_margin_kern(w: Scaled, _p: i32, side: Side) -> usize {
    let k = get_node(MARGIN_KERN_NODE_SIZE);
    set_NODE_type(k, TextNode::MarginKern);
    MEM[k].b16.s0 = side as u16;
    MEM[k + 1].b32.s1 = w.0;
    k
}
pub(crate) unsafe fn hpack(mut popt: Option<usize>, mut w: Scaled, m: PackMode) -> List {
    last_badness = 0;
    let mut r = List::from(get_node(BOX_NODE_SIZE));
    r.set_horizontal();
    r.set_lr_mode(LRMode::Normal);
    r.set_shift_amount(Scaled::ZERO);
    let mut q = r.ptr() + 5;
    r.set_list_ptr(popt.tex_int());
    let mut h = Scaled::ZERO;
    let mut d = Scaled::ZERO;
    let mut x = Scaled::ZERO;
    total_stretch[NORMAL as usize] = Scaled::ZERO;
    total_shrink[NORMAL as usize] = Scaled::ZERO;
    total_stretch[FIL as usize] = Scaled::ZERO;
    total_shrink[FIL as usize] = Scaled::ZERO;
    total_stretch[FILL as usize] = Scaled::ZERO;
    total_shrink[FILL as usize] = Scaled::ZERO;
    total_stretch[FILLL as usize] = Scaled::ZERO;
    total_shrink[FILLL as usize] = Scaled::ZERO;
    if get_int_par(IntPar::texxet) > 0 {
        /*1497: */
        let mut tmp_ptr = Math(get_avail());
        tmp_ptr.set_subtype_i32(MathType::Before);
        *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
        LR_ptr = Some(tmp_ptr.ptr()).tex_int();
    }
    while let Some(mut p) = popt {
        /*674: */
        match CharOrText::from(p) {
            CharOrText::Char(p) => {
                /*677: */
                let f = p.font() as internal_font_number;
                let i = FONT_CHARACTER_INFO(f, effective_char(true, f, p.character()) as usize);
                x += *FONT_CHARINFO_WIDTH(f, i);
                let s = *FONT_CHARINFO_HEIGHT(f, i);
                h = h.max(s);
                let s = *FONT_CHARINFO_DEPTH(f, i);
                d = d.max(s);
            }
            CharOrText::Text(n) => match n {
                TxtNode::List(b) => {
                    x += b.width();
                    let s = b.shift_amount();
                    h = h.max(b.height() - s);
                    d = d.max(b.depth() + s);
                }
                TxtNode::Rule(r) => {
                    x += r.width();
                    h = h.max(r.height());
                    d = d.max(r.depth());
                }
                TxtNode::Unset(u) => {
                    x += u.width();
                    h = h.max(u.height());
                    d = d.max(u.depth());
                }
                TxtNode::Ins(_) | TxtNode::Mark(_) | TxtNode::Adjust(_) => {
                    if let (Some(at), Some(pat)) = (adjust_tail.as_mut(), pre_adjust_tail.as_mut())
                    {
                        /*680: */
                        while llist_link(q) != Some(p) {
                            q = *LLIST_link(q) as usize;
                        }
                        if let TxtNode::Adjust(a) = TxtNode::from(p) {
                            match a.subtype() {
                                AdjustType::Pre => {
                                    *LLIST_link(*pat) = a.adj_ptr();
                                    while let Some(next) = LLIST_link(*pat).opt() {
                                        *pat = next;
                                    }
                                }
                                AdjustType::Post => {
                                    *LLIST_link(*at) = a.adj_ptr();
                                    while let Some(next) = LLIST_link(*at).opt() {
                                        *at = next;
                                    }
                                }
                            }
                            p = *LLIST_link(p) as usize;
                            free_node(*LLIST_link(q) as usize, SMALL_NODE_SIZE);
                        } else {
                            *LLIST_link(*at) = Some(p).tex_int();
                            *at = p;
                            p = *LLIST_link(p) as usize;
                        }
                        *LLIST_link(q) = Some(p).tex_int();
                        p = q;
                    }
                }
                TxtNode::WhatsIt(w) => match w {
                    WhatsIt::NativeWord(mut p_nw) => {
                        let mut k = if q != r.ptr() + 5 {
                            match Node::from(q) {
                                Node::Text(TxtNode::Disc(d)) => d.replace_count() as i32,
                                _ => 0,
                            }
                        } else {
                            0
                        };
                        while llist_link(q) != Some(p) {
                            k -= 1;
                            q = *LLIST_link(q) as usize;
                            if let Node::Text(TxtNode::Disc(d)) = Node::from(q) {
                                k = d.replace_count() as i32;
                            }
                        }
                        let mut pp_opt = llist_link(p);
                        while let Some(pp) = pp_opt.filter(|&pp| k <= 0 && !is_char_node(Some(pp)))
                        {
                            match TxtNode::from(pp) {
                                TxtNode::WhatsIt(WhatsIt::NativeWord(pp))
                                    if pp.font() == p_nw.font() =>
                                {
                                    pp_opt = llist_link(pp.ptr());
                                }
                                TxtNode::Disc(_) => match llist_link(pp).map(CharOrText::from) {
                                    Some(CharOrText::Text(TxtNode::WhatsIt(
                                        WhatsIt::NativeWord(ppp),
                                    ))) if ppp.font() == p_nw.font() => {
                                        pp_opt = llist_link(ppp.ptr());
                                    }
                                    _ => break,
                                },
                                _ => break,
                            }
                        }
                        if pp_opt != llist_link(p) {
                            let mut total_chars = 0;
                            let mut z = llist_link(q);
                            let mut ppp = usize::MAX; // TODO: check
                            while z != pp_opt {
                                ppp = z.unwrap();
                                if let CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw))) =
                                    CharOrText::from(ppp)
                                {
                                    total_chars += nw.text().len() as i32;
                                }
                                z = llist_link(ppp);
                            }
                            p = llist_link(q).unwrap();
                            p_nw = NativeWord::from(p);
                            let mut pp = new_native_word_node(p_nw.font() as usize, total_chars);
                            pp.set_actual_text_from(&p_nw);
                            *LLIST_link(q) = Some(pp.ptr()).tex_int();
                            *LLIST_link(pp.ptr()) = *LLIST_link(ppp);
                            *LLIST_link(ppp) = None.tex_int();
                            total_chars = 0;
                            let mut ppp = p;
                            loop {
                                if let CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw))) =
                                    CharOrText::from(ppp)
                                {
                                    let ppp_text = nw.text();
                                    pp.text_mut()[total_chars as usize
                                        ..(total_chars as usize) + ppp_text.len()]
                                        .copy_from_slice(&ppp_text);
                                    total_chars += ppp_text.len() as i32;
                                }
                                if let Some(next) = llist_link(ppp) {
                                    ppp = next;
                                } else {
                                    break;
                                }
                            }
                            flush_node_list(Some(p));
                            p = *LLIST_link(q) as usize;
                            p_nw = NativeWord::from(p);
                            p_nw.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
                        }
                        h = h.max(p_nw.height());
                        d = d.max(p_nw.depth());
                        x += p_nw.width();
                    }
                    WhatsIt::Glyph(g) => {
                        h = h.max(g.height());
                        d = d.max(g.depth());
                        x += g.width();
                    }
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        h = h.max(p.height());
                        d = d.max(p.depth());
                        x += p.width();
                    }
                    _ => {}
                },
                TxtNode::Glue(p) => {
                    let g = GlueSpec(p.glue_ptr() as usize);
                    x += g.size();
                    let o = g.stretch_order() as usize;
                    total_stretch[o] += g.stretch();
                    let o = g.shrink_order() as usize;
                    total_shrink[o] += g.shrink();
                    if p.param() >= A_LEADERS {
                        let g = List::from(p.leader_ptr() as usize);
                        h = h.max(g.height());
                        d = d.max(g.depth());
                    }
                }
                TxtNode::Kern(k) => {
                    x += k.width();
                }
                TxtNode::MarginKern(k) => {
                    x += k.width();
                }
                TxtNode::Math(p) => {
                    x += p.width();
                    if get_int_par(IntPar::texxet) > 0 {
                        /*1498: */
                        let (be, mode) = p.subtype().equ();
                        if be == BE::End {
                            if Math(LR_ptr as usize).subtype_i32() == MathType::Eq(BE::End, mode) {
                                let tmp_ptr = LR_ptr as usize; /*689: */
                                LR_ptr = *LLIST_link(tmp_ptr);
                                *LLIST_link(tmp_ptr) = avail.tex_int();
                                avail = Some(tmp_ptr);
                            } else {
                                LR_problems += 1;
                                set_NODE_type(p.ptr(), TextNode::Kern);
                                Kern(p.ptr()).set_subtype(KernType::Explicit);
                            }
                        } else {
                            let mut tmp_ptr = Math(get_avail());
                            tmp_ptr.set_subtype_i32(MathType::Eq(BE::End, mode));
                            *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
                            LR_ptr = Some(tmp_ptr.ptr()).tex_int();
                        }
                    }
                }
                TxtNode::Ligature(l) => {
                    let mut g = Char(GARBAGE);
                    g.set_character(l.char());
                    g.set_font(l.font());
                    *LLIST_link(GARBAGE) = *LLIST_link(l.ptr());
                    popt = Some(GARBAGE);
                    xtx_ligature_present = true;
                    continue;
                }
                _ => {}
            },
        }
        popt = llist_link(p)
    }

    if let Some(a) = adjust_tail {
        *LLIST_link(a) = None.tex_int();
    }
    if let Some(a) = pre_adjust_tail {
        *LLIST_link(a) = None.tex_int();
    }
    r.set_height(h).set_depth(d);
    if m == PackMode::Additional {
        w = x + w;
    }
    r.set_width(w);
    x = w - x;
    match x.cmp(&Scaled::ZERO) {
        Ordering::Equal => {
            r.set_glue_sign(GlueSign::Normal)
                .set_glue_order(GlueOrder::Normal)
                .set_glue_set(0.);
            return exit(r, q);
        }
        Ordering::Greater => {
            /*683: */
            let o = if total_stretch[GlueOrder::Filll as usize] != Scaled::ZERO {
                GlueOrder::Filll
            } else if total_stretch[GlueOrder::Fill as usize] != Scaled::ZERO {
                GlueOrder::Fill
            } else if total_stretch[GlueOrder::Fil as usize] != Scaled::ZERO {
                GlueOrder::Fil
            } else {
                GlueOrder::Normal
            }; /*normal *//*:684 */
            r.set_glue_order(o).set_glue_sign(GlueSign::Stretching);
            if total_stretch[o as usize] != Scaled::ZERO {
                r.set_glue_set(x.0 as f64 / total_stretch[o as usize].0 as f64);
            } else {
                r.set_glue_sign(GlueSign::Normal).set_glue_set(0.);
            }
            if o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
                /*685: */
                last_badness = badness(x, total_stretch[NORMAL as usize]); /*normal *//*:690 */
                if last_badness > get_int_par(IntPar::hbadness) {
                    print_ln();
                    if last_badness > 100 {
                        t_print_nl!("Underfull \\hbox (badness {}", last_badness);
                    } else {
                        t_print_nl!("Loose \\hbox (badness {}", last_badness);
                    }
                    return common_ending(r, q);
                }
            }
            return exit(r, q);
        }
        Ordering::Less => {
            let o = if total_shrink[GlueOrder::Filll as usize] != Scaled::ZERO {
                GlueOrder::Filll
            } else if total_shrink[GlueOrder::Fill as usize] != Scaled::ZERO {
                GlueOrder::Fill
            } else if total_shrink[GlueOrder::Fil as usize] != Scaled::ZERO {
                GlueOrder::Fil
            } else {
                GlueOrder::Normal
            };
            r.set_glue_order(o).set_glue_sign(GlueSign::Shrinking);
            if total_shrink[o as usize] != Scaled::ZERO {
                r.set_glue_set(-x.0 as f64 / total_shrink[o as usize].0 as f64);
            } else {
                r.set_glue_sign(GlueSign::Normal).set_glue_set(0.);
            }
            if total_shrink[o as usize] < -x
                && o == GlueOrder::Normal
                && r.list_ptr().opt().is_some()
            {
                last_badness = 1000000;
                r.set_glue_set(1.);
                if -x - total_shrink[0] > get_dimen_par(DimenPar::hfuzz)
                    || get_int_par(IntPar::hbadness) < 100
                {
                    if get_dimen_par(DimenPar::overfull_rule) > Scaled::ZERO
                        && -x - total_shrink[0] > get_dimen_par(DimenPar::hfuzz)
                    {
                        while let Some(next) = LLIST_link(q as usize).opt() {
                            q = next;
                        }
                        *LLIST_link(q as usize) = Some(new_rule().ptr()).tex_int();
                        MEM[(*LLIST_link(q as usize) + 1) as usize].b32.s1 =
                            (get_dimen_par(DimenPar::overfull_rule)).0
                    }
                    print_ln();
                    t_print_nl!(
                        "Overfull \\hbox ({}pt too wide",
                        -x - total_shrink[NORMAL as usize]
                    );
                    return common_ending(r, q);
                }
            } else if o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
                /*692: */
                last_badness = badness(-x, total_shrink[NORMAL as usize]);
                if last_badness > get_int_par(IntPar::hbadness) {
                    print_ln();
                    t_print_nl!("Tight \\hbox (badness {}", last_badness);
                    return common_ending(r, q);
                }
            }

            return exit(r, q);
        }
    }

    unsafe fn common_ending(r: List, q: usize) -> List {
        if output_active {
            t_print!(") has occurred while \\output is active");
        } else if pack_begin_line != 0 {
            if pack_begin_line > 0 {
                t_print!(
                    ") in paragraph at lines {}--{}",
                    pack_begin_line.abs(),
                    line
                );
            } else {
                t_print!(
                    ") in alignment at lines {}--{}",
                    pack_begin_line.abs(),
                    line
                );
            }
        } else {
            t_print!(") detected at line {}", line);
        }
        print_ln();
        font_in_short_display = 0;
        short_display(r.list_ptr().opt());
        print_ln();
        diagnostic(true, || show_box(Some(r.ptr())));
        exit(r, q)
    }

    unsafe fn exit(r: List, mut q: usize) -> List {
        if get_int_par(IntPar::texxet) > 0 {
            /*1499: */
            if Math(LR_ptr as usize).subtype_i32() != MathType::Before {
                while let Some(next) = llist_link(q) {
                    q = next;
                } /*:673 */
                loop {
                    let tmp_ptr = q;
                    let m = new_math(Scaled::ZERO, Math(LR_ptr as usize).subtype_i32());
                    q = m.ptr();
                    *LLIST_link(tmp_ptr) = Some(m.ptr()).tex_int();
                    LR_problems += 10000;
                    let tmp_ptr = LR_ptr as usize;
                    LR_ptr = *LLIST_link(tmp_ptr);
                    *LLIST_link(tmp_ptr) = avail.tex_int();
                    avail = Some(tmp_ptr);
                    if Math(LR_ptr as usize).subtype_i32() == MathType::Before {
                        break;
                    }
                }
            }
            if LR_problems > 0 {
                print_ln();
                t_print_nl!(
                    "\\endL or \\endR problem ({} missing, {} extra",
                    LR_problems / 10000,
                    LR_problems % 10000
                );
                LR_problems = 0;
                return common_ending(r, q);
            } else {
                let tmp_ptr = LR_ptr as usize;
                LR_ptr = *LLIST_link(tmp_ptr);
                *LLIST_link(tmp_ptr) = avail.tex_int();
                avail = Some(tmp_ptr);
                if LR_ptr.opt().is_some() {
                    confusion("LR1");
                }
            }
        }
        r
    }
}
pub(crate) unsafe fn vpackage(
    mut popt: Option<usize>,
    mut h: Scaled,
    m: PackMode,
    l: Scaled,
) -> List {
    last_badness = 0;
    let mut r = List::from(get_node(BOX_NODE_SIZE) as usize);
    r.set_vertical();
    r.set_lr_mode(if get_int_par(IntPar::xetex_upwards) > 0 {
        LRMode::Reversed
    } else {
        LRMode::Normal
    });
    r.set_shift_amount(Scaled::ZERO);
    r.set_list_ptr(popt.tex_int());
    let mut w = Scaled::ZERO;
    let mut d = Scaled::ZERO;
    let mut x = Scaled::ZERO;
    total_stretch[NORMAL as usize] = Scaled::ZERO;
    total_shrink[NORMAL as usize] = Scaled::ZERO;
    total_stretch[FIL as usize] = Scaled::ZERO;
    total_shrink[FIL as usize] = Scaled::ZERO;
    total_stretch[FILL as usize] = Scaled::ZERO;
    total_shrink[FILL as usize] = Scaled::ZERO;
    total_stretch[FILLL as usize] = Scaled::ZERO;
    total_shrink[FILLL as usize] = Scaled::ZERO;
    while let Some(p) = popt {
        /*694: */
        match &TxtNode::from(p) {
            TxtNode::List(b) => {
                x += d + b.height();
                d = b.depth();
                w = w.max(b.width() + b.shift_amount());
            }
            TxtNode::Rule(r) => {
                x += d + r.height();
                d = r.depth();
                w = w.max(r.width());
            }
            TxtNode::Unset(u) => {
                x += d + u.height();
                d = u.depth();
                w = w.max(u.width());
            }
            TxtNode::WhatsIt(p) => match p {
                WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                    x += d + p.height();
                    d = p.depth();
                    w = w.max(p.width());
                }
                _ => {}
            },
            TxtNode::Glue(p) => {
                x += d;
                d = Scaled::ZERO;
                let g = GlueSpec(p.glue_ptr() as usize);
                x += g.size();
                let o = g.stretch_order() as usize;
                total_stretch[o] += g.stretch();
                let o = g.shrink_order() as usize;
                total_shrink[o] += g.shrink();
                if p.param() >= A_LEADERS {
                    let g = p.leader_ptr() as usize;
                    w = w.max(List::from(g).width());
                }
            }
            TxtNode::Kern(k) => {
                x += d + k.width();
                d = Scaled::ZERO;
            }
            _ => {}
        }
        popt = llist_link(p);
    }
    r.set_width(w);
    if d > l {
        x += d - l;
        r.set_depth(l);
    } else {
        r.set_depth(d);
    }
    if m == PackMode::Additional {
        h = x + h;
    }
    r.set_height(h);
    x = h - x;
    match x.cmp(&Scaled::ZERO) {
        Ordering::Equal => {
            r.set_glue_sign(GlueSign::Normal)
                .set_glue_order(GlueOrder::Normal)
                .set_glue_set(0.);
        }
        Ordering::Greater => {
            /*698: */
            let o = if total_stretch[FILLL as usize] != Scaled::ZERO {
                GlueOrder::Filll
            } else if total_stretch[FILL as usize] != Scaled::ZERO {
                GlueOrder::Fill
            } else if total_stretch[FIL as usize] != Scaled::ZERO {
                GlueOrder::Fil
            } else {
                GlueOrder::Normal
            }; /*normal *//*:684 */
            r.set_glue_order(o).set_glue_sign(GlueSign::Stretching);
            if total_stretch[o as usize] != Scaled::ZERO {
                r.set_glue_set(x.0 as f64 / total_stretch[o as usize].0 as f64);
            } else {
                r.set_glue_sign(GlueSign::Normal).set_glue_set(0.);
            }
            if o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
                /*699: */
                last_badness = badness(x, total_stretch[GlueOrder::Normal as usize]); /*normal *//*:690 */
                if last_badness > get_int_par(IntPar::vbadness) {
                    print_ln();
                    if last_badness > 100 {
                        t_print_nl!("Underfull \\vbox (badness {}", last_badness);
                    } else {
                        t_print_nl!("Loose \\vbox (badness {}", last_badness);
                    }
                    return common_ending(r);
                }
            }
        }
        Ordering::Less => {
            let o = if total_shrink[FILLL as usize] != Scaled::ZERO {
                GlueOrder::Filll
            } else if total_shrink[FILL as usize] != Scaled::ZERO {
                GlueOrder::Fill
            } else if total_shrink[FIL as usize] != Scaled::ZERO {
                GlueOrder::Fil
            } else {
                GlueOrder::Normal
            };
            r.set_glue_order(o).set_glue_sign(GlueSign::Shrinking);
            if total_shrink[o as usize] != Scaled::ZERO {
                r.set_glue_set(-x.0 as f64 / total_shrink[o as usize].0 as f64);
            } else {
                r.set_glue_sign(GlueSign::Normal).set_glue_set(0.);
            }
            if total_shrink[o as usize] < -x
                && o == GlueOrder::Normal
                && r.list_ptr().opt().is_some()
            {
                last_badness = 1000000;
                r.set_glue_set(1.);
                if -x - total_shrink[GlueOrder::Normal as usize] > get_dimen_par(DimenPar::vfuzz)
                    || get_int_par(IntPar::vbadness) < 100
                {
                    print_ln();
                    t_print_nl!(
                        "Overfull \\vbox ({}pt too high",
                        -x - total_shrink[NORMAL as usize]
                    );
                    return common_ending(r);
                }
            } else if o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
                /*703: */
                last_badness = badness(-x, total_shrink[NORMAL as usize]);
                if last_badness > get_int_par(IntPar::vbadness) {
                    print_ln();
                    t_print_nl!("Tight \\vbox (badness {}", last_badness);
                    return common_ending(r);
                }
            }
        }
    }
    return r;

    unsafe fn common_ending(r: List) -> List {
        if output_active {
            t_print!(") has occurred while \\output is active");
        } else {
            if pack_begin_line != 0 {
                t_print!(
                    ") in alignment at lines {}--{}",
                    pack_begin_line.abs(),
                    line
                );
            } else {
                t_print!(") detected at line {}", line);
            }
            print_ln();
        }
        diagnostic(true, || show_box(Some(r.ptr())));
        r
    }
}
pub(crate) unsafe fn append_to_vlist(b: List) {
    let upwards = get_int_par(IntPar::xetex_upwards) > 0;
    if cur_list.aux.b32.s1 > IGNORE_DEPTH {
        let d = if upwards {
            get_glue_par(GluePar::baseline_skip).size() - Scaled(cur_list.aux.b32.s1) - b.depth()
        } else {
            get_glue_par(GluePar::baseline_skip).size() - Scaled(cur_list.aux.b32.s1) - b.height()
        };
        let p = if d < get_dimen_par(DimenPar::line_skip_limit) {
            new_param_glue(GluePar::line_skip)
        } else {
            let (p, mut tmp_ptr) = new_skip_param(GluePar::baseline_skip);
            tmp_ptr.set_size(d);
            p
        };
        *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
        cur_list.tail = p.ptr();
    }
    *LLIST_link(cur_list.tail) = Some(b.ptr()).tex_int();
    cur_list.tail = b.ptr();
    cur_list.aux.b32.s1 = (if upwards { b.height() } else { b.depth() }).0;
}
pub(crate) unsafe fn new_noad() -> usize {
    let p = get_node(NOAD_SIZE);
    MEM[p].b16.s1 = MathNode::Ord as u16;
    MEM[p].b16.s0 = NORMAL;
    let mut p = BaseMath(p);
    p.nucleus_mut().empty();
    p.subscr_mut().empty();
    p.supscr_mut().empty();
    p.ptr()
}
pub(crate) unsafe fn new_style(s: i16) -> usize {
    let p = get_node(STYLE_NODE_SIZE);
    set_NODE_type(p, TextNode::Style);
    MEM[p].b16.s0 = s as u16;
    MEM[p + 1].b32.s1 = 0;
    MEM[p + 2].b32.s1 = 0;
    p
}
pub(crate) unsafe fn new_choice() -> usize {
    let mut p = Choice(get_node(STYLE_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Choice);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_display(None);
    p.set_text(None);
    p.set_script(None);
    p.set_scriptscript(None);
    p.ptr()
}
pub(crate) unsafe fn push_alignment() {
    let mut p = Alignment(get_node(ALIGN_STACK_NODE_SIZE));
    *LLIST_link(p.ptr()) = align_ptr.tex_int();
    MEM[p.ptr()].b32.s0 = cur_align.tex_int();
    MEM[p.ptr() + 1].b32.s0 = *LLIST_link(ALIGN_HEAD);
    p.set_span(cur_span.tex_int())
        .set_v_part(cur_loop.tex_int())
        .set_u_part(align_state)
        .set_head(cur_head.tex_int())
        .set_tail(cur_tail.tex_int())
        .set_extra_info(cur_pre_head.tex_int())
        .set_pre_tail(cur_pre_tail.tex_int());
    align_ptr = Some(p.ptr());
    cur_head = Some(get_avail());
    cur_pre_head = Some(get_avail());
}
pub(crate) unsafe fn pop_alignment() {
    MEM[cur_head.unwrap()].b32.s1 = avail.tex_int();
    avail = cur_head;
    MEM[cur_pre_head.unwrap()].b32.s1 = avail.tex_int();
    avail = cur_pre_head;
    let p = Alignment(align_ptr.unwrap());
    cur_tail = p.tail().opt();
    cur_head = p.head().opt();
    cur_pre_tail = p.pre_tail().opt();
    cur_pre_head = p.extra_info().opt();
    align_state = p.u_part();
    cur_loop = p.v_part().opt();
    cur_span = p.span().opt();
    *LLIST_link(ALIGN_HEAD) = MEM[p.ptr() + 1].b32.s0;
    cur_align = MEM[p.ptr()].b32.s0.opt();
    align_ptr = llist_link(p.ptr());
    p.free();
}
pub(crate) unsafe fn get_preamble_token(input: &mut input_state_t) -> (i32, Cmd) {
    let mut otok;
    let mut ocmd;
    loop {
        let (tok, cmd, chr, _) = get_token(input);
        otok = tok;
        ocmd = cmd;
        let mut ochr = chr;
        while ochr == SPAN_CODE && ocmd == Cmd::TabMark {
            let (tok, cmd, chr, cs) = get_token(input);
            otok = tok;
            ocmd = cmd;
            ochr = chr;
            if cmd > MAX_COMMAND {
                expand(input, cmd, chr, cs);
                let (tok, cmd, chr, _) = get_token(input);
                otok = tok;
                ocmd = cmd;
                ochr = chr;
            }
        }
        if ocmd == Cmd::EndV {
            fatal_error("(interwoven alignment preambles are not allowed)");
        }
        if !(ocmd == Cmd::AssignGlue && ochr == GLUE_BASE as i32 + GluePar::tab_skip as i32) {
            break;
        }
        scan_optional_equals(input);
        let val = scan_glue(input, ValLevel::Glue);
        if get_int_par(IntPar::global_defs) > 0 {
            geq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                Some(val.ptr()),
            );
        } else {
            eq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                Some(val.ptr()),
            );
        }
    }
    (otok, ocmd)
}
pub(crate) unsafe fn init_align(input: &mut input_state_t, wcs: i32) {
    push_alignment();
    align_state = -1000000;
    if cur_list.mode == (false, ListMode::MMode)
        && (cur_list.tail != cur_list.head || cur_list.aux.b32.s1.opt().is_some())
    {
        t_eprint!("Improper {} inside $$\'s", Esc("halign"));
        help!(
            "Displays can use special alignments (like \\eqalignno)",
            "only if nothing but the alignment itself is between $$\'s.",
            "So I\'ve deleted the formulas that preceded this alignment."
        );
        error();
        flush_math();
    }
    push_nest();
    if cur_list.mode == (false, ListMode::MMode) {
        cur_list.mode = (true, ListMode::VMode);
        cur_list.aux.b32.s1 = NEST[NEST_PTR - 2].aux.b32.s1
    } else if !cur_list.mode.0 {
        cur_list.mode = (!cur_list.mode.0, cur_list.mode.1);
        /*:804*/
    }
    let mut ocmd = scan_spec(input, GroupCode::Align, false);
    *LLIST_link(ALIGN_HEAD) = None.tex_int();
    let mut ca = ALIGN_HEAD;
    cur_align = Some(ca);
    cur_loop = None;
    scanner_status = ScannerStatus::Aligning;
    warning_index = wcs;
    align_state = -1000000;
    loop {
        let ca2 = new_param_glue(GluePar::tab_skip);
        *LLIST_link(ca) = Some(ca2.ptr()).tex_int();
        /*:808 */
        cur_align = Some(ca2.ptr()); /*:807*/
        if ocmd == Cmd::CarRet {
            break; /*:813*/
        } /*:806 */
        let mut p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            let (tok, cmd) = get_preamble_token(input);
            //ocmd = cmd;
            if cmd == Cmd::MacParam {
                break;
            }
            if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state as i64 == -1000000 {
                if p == HOLD_HEAD as i32 && cur_loop.is_none() && cmd == Cmd::TabMark {
                    cur_loop = Some(ca2.ptr());
                } else {
                    t_eprint!("Missing # inserted in alignment preamble");
                    help!(
                        "There should be exactly one # between &\'s, when an",
                        "\\halign or \\valign is being set up. In this case you had",
                        "none, so I\'ve put one in; maybe that will work."
                    );
                    back_error(input, tok);
                    break;
                }
            } else if cmd != Cmd::Spacer || p != HOLD_HEAD as i32 {
                *LLIST_link(p as usize) = Some(get_avail()).tex_int();
                p = *LLIST_link(p as usize);
                MEM[p as usize].b32.s0 = tok;
            }
        }
        ca = new_null_box(); // TODO: ????????
        *LLIST_link(ca2.ptr()) = Some(ca).tex_int();
        cur_align = Some(ca);
        MEM[ca].b32.s0 = END_SPAN as i32;
        MEM[ca + 1].b32.s1 = NULL_FLAG.0;
        MEM[ca + 3].b32.s1 = *LLIST_link(HOLD_HEAD);
        let mut p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            let (tok, cmd) = get_preamble_token(input);
            ocmd = cmd;
            if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state as i64 == -1000000 {
                break;
            }
            if cmd == Cmd::MacParam {
                t_eprint!("Only one # is allowed per tab");
                help!(
                    "There should be exactly one # between &\'s, when an",
                    "\\halign or \\valign is being set up. In this case you had",
                    "more than one, so I\'m ignoring all but the first."
                );
                error();
            } else {
                *LLIST_link(p as usize) = Some(get_avail()).tex_int();
                p = *LLIST_link(p as usize);
                MEM[p as usize].b32.s0 = tok;
            }
        }
        *LLIST_link(p as usize) = Some(get_avail()).tex_int();
        let p = *LLIST_link(p as usize);
        MEM[p as usize].b32.s0 = CS_TOKEN_FLAG + FROZEN_END_TEMPLATE as i32;
        MEM[ca + 2].b32.s1 = *LLIST_link(HOLD_HEAD)
    }
    scanner_status = ScannerStatus::Normal;
    new_save_level(GroupCode::Align);
    if let Some(l) = LOCAL(Local::every_cr).opt() {
        begin_token_list(input, Some(l), Btl::EveryCRText);
    }
    align_peek(input);
}
pub(crate) unsafe fn init_span(p: Option<usize>) {
    push_nest();
    if cur_list.mode == (true, ListMode::HMode) {
        cur_list.aux.b32.s0 = 1000;
    } else {
        cur_list.aux.b32.s1 = IGNORE_DEPTH;
        normal_paragraph();
    }
    cur_span = p;
}
pub(crate) unsafe fn init_row() {
    push_nest();
    cur_list.mode = match cur_list.mode {
        (true, ListMode::VMode) => (true, ListMode::HMode),
        (true, ListMode::HMode) => (true, ListMode::VMode),
        _ => panic!(),
    };
    if cur_list.mode == (true, ListMode::HMode) {
        cur_list.aux.b32.s0 = 0;
    } else {
        cur_list.aux.b32.s1 = 0;
    }
    let g = new_glue(&GlueSpec(
        MEM[(*LLIST_link(ALIGN_HEAD) + 1) as usize].b32.s0 as usize,
    ));
    *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
    cur_list.tail = g.ptr();
    MEM[cur_list.tail].b16.s0 = GluePar::tab_skip as u16 + 1;
    cur_align = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1.opt();
    cur_tail = cur_head;
    cur_pre_tail = cur_pre_head;
    init_span(cur_align);
}
pub(crate) unsafe fn init_col(input: &mut input_state_t, tok: i32, cmd: Cmd) {
    let mut ca = Alignment(cur_align.unwrap());
    ca.set_extra_info(cmd as i32);
    if cmd == Cmd::Omit {
        align_state = 0;
    } else {
        back_input(input, tok);
        begin_token_list(input, ca.u_part().opt(), Btl::UTemplate);
    };
}
pub(crate) unsafe fn fin_col(input: &mut input_state_t) -> bool {
    let mut ca = Alignment(cur_align.confuse("endv"));
    let q = llist_link(ca.ptr()).confuse("endv");
    if (align_state as i64) < 500000 {
        fatal_error("(interwoven alignment preambles are not allowed)");
    }
    let mut p = llist_link(q);
    if p.is_none() && ca.extra_info() < CR_CODE {
        if let Some(cl) = cur_loop {
            /*822: */
            let nb = new_null_box(); // TODO: ????????
            *LLIST_link(q) = Some(nb).tex_int(); /*:823 */
            p = Some(nb);
            MEM[nb].b32.s0 = END_SPAN as i32;
            MEM[nb + 1].b32.s1 = NULL_FLAG.0;
            let cl = *LLIST_link(cl) as usize;
            cur_loop = Some(cl);
            let mut q = HOLD_HEAD;
            let mut ropt = MEM[cl + 3].b32.s1.opt();
            while let Some(r) = ropt {
                let a = get_avail();
                *LLIST_link(q) = Some(a).tex_int();
                q = a;
                MEM[q].b32.s0 = MEM[r].b32.s0;
                ropt = llist_link(r);
            }
            *LLIST_link(q) = None.tex_int();
            MEM[nb + 3].b32.s1 = *LLIST_link(HOLD_HEAD);
            let mut q = HOLD_HEAD;
            let mut ropt = MEM[cl + 2].b32.s1.opt();
            while let Some(r) = ropt {
                let a = get_avail();
                *LLIST_link(q) = Some(a).tex_int();
                q = a;
                MEM[q].b32.s0 = MEM[r].b32.s0;
                ropt = llist_link(r);
            }
            *LLIST_link(q) = None.tex_int();
            MEM[nb + 2].b32.s1 = *LLIST_link(HOLD_HEAD);
            let cl = *LLIST_link(cl) as usize;
            cur_loop = Some(cl);
            let g = new_glue(&GlueSpec(MEM[cl + 1].b32.s0 as usize));
            *LLIST_link(nb) = Some(g.ptr()).tex_int();
        } else {
            t_eprint!("Extra alignment tab has been changed to {}", Esc("cr"));
            help!(
                "You have given more \\span or & marks than there were",
                "in the preamble to the \\halign or \\valign now in progress.",
                "So I\'ll assume that you meant to type \\cr instead."
            );
            ca.set_extra_info(CR_CODE);
            error();
        }
    }
    if ca.extra_info() != SPAN_CODE {
        unsave(input);
        new_save_level(GroupCode::Align);
        let u;
        let w;
        if cur_list.mode == (true, ListMode::HMode) {
            adjust_tail = cur_tail;
            pre_adjust_tail = cur_pre_tail;
            u = hpack(
                MEM[cur_list.head].b32.s1.opt(),
                Scaled::ZERO,
                PackMode::Additional,
            );
            w = u.width();
            cur_tail = adjust_tail;
            adjust_tail = None;
            cur_pre_tail = pre_adjust_tail;
            pre_adjust_tail = None;
        } else {
            u = vpackage(
                MEM[cur_list.head].b32.s1.opt(),
                Scaled::ZERO,
                PackMode::Additional,
                Scaled::ZERO,
            );
            w = u.height();
        }
        let mut n = 0;
        if cur_span != Some(ca.ptr()) {
            /*827: */
            let mut q = cur_span; /*normal *//*:684 */
            loop {
                n += 1; /*normal *//*:690 */
                q = MEM[MEM[q.unwrap()].b32.s1 as usize].b32.s1.opt(); /*tab_skip_code 1 *//*:824 */
                if q == Some(ca.ptr()) {
                    break;
                }
            }
            if n > u16::MAX as i32 {
                confusion("too many spans");
            }
            let mut q = Span(cur_span.unwrap());
            while q.next().number() < n {
                q = q.next();
            }
            if q.next().number() > n {
                let mut s = Span(get_node(SPAN_NODE_SIZE));
                s.set_next(&q.next()).set_number(n);
                q.set_next(&s);
                s.set_size(w);
            } else if q.next().size() < w {
                q.next().set_size(w);
            }
        } else if w > Scaled(ca.span()) {
            // TODO: fix
            ca.set_span(w.0);
        }
        let mut u = Unset::from(u.ptr());
        set_NODE_type(u.ptr(), TextNode::Unset);
        u.set_columns(n as u16);
        let o = if total_stretch[FILLL as usize] != Scaled::ZERO {
            GlueOrder::Filll
        } else if total_stretch[FILL as usize] != Scaled::ZERO {
            GlueOrder::Fill
        } else if total_stretch[FIL as usize] != Scaled::ZERO {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        };
        u.set_stretch_order(o);
        u.set_stretch(total_stretch[o as usize]);
        let o = if total_shrink[FILLL as usize] != Scaled::ZERO {
            GlueOrder::Filll
        } else if total_shrink[FILL as usize] != Scaled::ZERO {
            GlueOrder::Fill
        } else if total_shrink[FIL as usize] != Scaled::ZERO {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        };
        u.set_shrink_order(o);
        u.set_shrink(total_shrink[o as usize]);
        pop_nest();
        *LLIST_link(cur_list.tail) = Some(u.ptr()).tex_int();
        cur_list.tail = u.ptr();
        let mut g = new_glue(&GlueSpec(
            Glue(*LLIST_link(ca.ptr()) as usize).glue_ptr() as usize
        ));
        *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
        cur_list.tail = g.ptr();
        g.set_param(GluePar::tab_skip as u16 + 1);
        if ca.extra_info() >= CR_CODE {
            return true;
        }
        init_span(p);
    }
    align_state = 1000000;
    let (tok, cmd) = loop {
        let (tok, cmd, _) = get_x_or_protected(input);
        if cmd != Cmd::Spacer {
            break (tok, cmd);
        }
    };
    cur_align = p;
    init_col(input, tok, cmd);
    false
}
pub(crate) unsafe fn fin_row(input: &mut input_state_t) {
    let p;
    if cur_list.mode == (true, ListMode::HMode) {
        p = hpack(
            MEM[cur_list.head].b32.s1.opt(),
            Scaled::ZERO,
            PackMode::Additional,
        );
        pop_nest();
        if cur_pre_head != cur_pre_tail {
            *LLIST_link(cur_list.tail) = MEM[cur_pre_head.unwrap()].b32.s1;
            cur_list.tail = cur_pre_tail.unwrap();
        }
        append_to_vlist(p);
        if cur_head != cur_tail {
            *LLIST_link(cur_list.tail) = MEM[cur_head.unwrap()].b32.s1;
            cur_list.tail = cur_tail.unwrap();
        }
    } else {
        p = vpackage(
            MEM[cur_list.head].b32.s1.opt(),
            Scaled::ZERO,
            PackMode::Additional,
            Scaled::MAX_HALFWORD,
        );
        pop_nest();
        *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
        cur_list.tail = p.ptr();
        cur_list.aux.b32.s0 = 1000;
    }
    let mut p = Unset::from(p.ptr());
    set_NODE_type(p.ptr(), TextNode::Unset);
    p.set_stretch(Scaled::ZERO);
    if let Some(ecr) = LOCAL(Local::every_cr).opt() {
        begin_token_list(input, Some(ecr), Btl::EveryCRText);
    }
    align_peek(input);
}
pub(crate) unsafe fn fin_align(input: &mut input_state_t, group: GroupCode) {
    if group != GroupCode::Align {
        confusion("align1");
    }
    unsave(input);
    if group != GroupCode::Align {
        confusion("align0");
    }
    unsave(input);
    let o = if NEST[(NEST_PTR - 1) as usize].mode == (false, ListMode::MMode) {
        get_dimen_par(DimenPar::display_indent)
    } else {
        Scaled::ZERO
    };
    let mut q = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1 as usize;
    loop {
        flush_list(MEM[q + 3].b32.s1.opt());
        flush_list(MEM[q + 2].b32.s1.opt());
        let p = MEM[MEM[q].b32.s1 as usize].b32.s1.opt();
        if Scaled(MEM[q + 1].b32.s1) == NULL_FLAG {
            /*831: */
            MEM[q + 1].b32.s1 = 0;
            let r = MEM[q].b32.s1 as usize;
            let s = MEM[r + 1].b32.s0;
            if s != 0 {
                ZERO_GLUE.rc_inc();
                delete_glue_ref(s as usize);
                MEM[r + 1].b32.s0 = 0
            }
        }
        if MEM[q].b32.s0 != END_SPAN as i32 {
            /*832: */
            let t = MEM[q + 1].b32.s1
                + MEM[(MEM[(MEM[q].b32.s1 + 1) as usize].b32.s0 + 1) as usize]
                    .b32
                    .s1; /*:833 */
            let mut r = MEM[q].b32.s0 as usize;
            let mut s = END_SPAN;
            MEM[s].b32.s0 = p.tex_int();
            let mut n = 1;
            loop {
                MEM[r + 1].b32.s1 -= t;
                let u = MEM[r].b32.s0 as usize;
                while MEM[r].b32.s1 > n {
                    s = Span(s).next().ptr();
                    n = Span(s).next().number() + 1;
                }
                {
                    let mut r = Span(r);
                    if r.number() < n {
                        r.set_next(&Span(s).next());
                        Span(s).set_next(&r);
                        r.set_number(r.number() - 1);
                        s = r.ptr();
                    } else {
                        if r.size() > Span(s).next().size() {
                            Span(s).next().set_size(r.size());
                        }
                        r.free();
                    }
                }
                r = u;
                if r == END_SPAN {
                    break;
                }
            }
        }
        let mut q_unset = Unset::from(q);
        set_NODE_type(q_unset.ptr(), TextNode::Unset);
        q_unset.set_columns(0);
        q_unset.set_height(Scaled::ZERO).set_depth(Scaled::ZERO);
        q_unset
            .set_stretch_order(GlueOrder::Normal)
            .set_shrink_order(GlueOrder::Normal)
            .set_stretch(Scaled::ZERO)
            .set_shrink(Scaled::ZERO);
        if let Some(pp) = p {
            q = pp;
        } else {
            break;
        }
    }
    SAVE_PTR -= 2;
    pack_begin_line = -cur_list.mode_line;
    let p;
    if cur_list.mode == (true, ListMode::VMode) {
        let rule_save = get_dimen_par(DimenPar::overfull_rule);
        set_dimen_par(DimenPar::overfull_rule, Scaled::ZERO);
        p = hpack(
            llist_link(ALIGN_HEAD),
            Scaled(SAVE_STACK[SAVE_PTR + 1].val),
            PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
        );
        set_dimen_par(DimenPar::overfull_rule, rule_save);
    } else {
        let mut q = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1 as usize;
        loop {
            MEM[q + 3].b32.s1 = MEM[q + 1].b32.s1;
            MEM[q + 1].b32.s1 = 0;
            if let Some(next) = MEM[*LLIST_link(q) as usize].b32.s1.opt() {
                q = next;
            } else {
                break;
            }
        }
        p = vpackage(
            llist_link(ALIGN_HEAD),
            Scaled(SAVE_STACK[SAVE_PTR + 1].val),
            PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
            Scaled::MAX_HALFWORD,
        );
        let mut q = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1 as usize;
        loop {
            MEM[q + 1].b32.s1 = MEM[q + 3].b32.s1;
            MEM[q + 3].b32.s1 = 0;
            if let Some(next) = MEM[*LLIST_link(q) as usize].b32.s1.opt() {
                q = next;
            } else {
                break;
            }
        }
    }
    pack_begin_line = 0;
    let mut qopt = MEM[cur_list.head].b32.s1.opt();
    let mut s = cur_list.head;
    while let Some(mut q) = qopt {
        match CharOrText::from(q) {
            CharOrText::Text(TxtNode::Unset(_)) => {
                /*836: */
                let mut q = List::from(q);
                if cur_list.mode == (true, ListMode::VMode) {
                    q.set_horizontal();
                    q.set_width(p.width());
                    if NEST[NEST_PTR - 1].mode == (false, ListMode::MMode) {
                        q.set_lr_mode(LRMode::DList);
                    }
                } else {
                    q.set_vertical();
                    q.set_height(p.height());
                }
                q.set_glue_order(p.glue_order())
                    .set_glue_sign(p.glue_sign())
                    .set_glue_set(p.glue_set())
                    .set_shift_amount(o);
                let mut r = *LLIST_link(q.list_ptr() as usize) as usize;
                s = *LLIST_link(p.list_ptr() as usize) as usize;
                loop {
                    /*837: */
                    let mut n = MEM[r].b16.s0 as i32; /*840: */
                    let mut t = BaseBox(s).width(); // TODO: check
                    let w = t;
                    let mut u = HOLD_HEAD;
                    MEM[r].b16.s0 = 0;
                    while n > 0 {
                        n -= 1;
                        s = *LLIST_link(s) as usize;
                        let v = GlueSpec(Glue(s).glue_ptr() as usize);
                        let g = new_glue(&v);
                        *LLIST_link(u) = Some(g.ptr()).tex_int();
                        u = g.ptr();
                        MEM[u].b16.s0 = GluePar::tab_skip as u16 + 1;
                        t += v.size();
                        if p.glue_sign() == GlueSign::Stretching {
                            if v.stretch_order() == p.glue_order() {
                                t += tex_round(p.glue_set() * v.stretch().0 as f64);
                            }
                        } else if p.glue_sign() == GlueSign::Shrinking
                            && v.shrink_order() == p.glue_order()
                        {
                            t -= tex_round(p.glue_set() * v.shrink().0 as f64);
                        }
                        s = *LLIST_link(s) as usize;
                        let mut nb = List::from(new_null_box());
                        *LLIST_link(u) = Some(nb.ptr()).tex_int();
                        u = nb.ptr();
                        t += BaseBox(s).width();
                        if cur_list.mode == (true, ListMode::VMode) {
                            nb.set_width(BaseBox(s).width());
                        } else {
                            nb.set_vertical();
                            nb.set_height(BaseBox(s).width());
                        }
                    }
                    let mut r_box = List::from(r);
                    let r_unset = Unset::from(r);
                    if cur_list.mode == (true, ListMode::VMode) {
                        /*839: */
                        r_box.set_height(q.height()).set_depth(q.depth());
                        match t.cmp(&r_unset.width()) {
                            Ordering::Equal => {
                                r_box
                                    .set_glue_sign(GlueSign::Normal)
                                    .set_glue_order(GlueOrder::Normal)
                                    .set_glue_set(0.);
                            }
                            Ordering::Greater => {
                                r_box.set_glue_sign(GlueSign::Stretching).set_glue_set(
                                    if r_unset.stretch() == Scaled::ZERO {
                                        0.
                                    } else {
                                        (t - r_unset.width()).0 as f64 / r_unset.stretch().0 as f64
                                    },
                                );
                            }
                            Ordering::Less => {
                                r_box
                                    .set_glue_order(r_unset.shrink_order())
                                    .set_glue_sign(GlueSign::Shrinking);
                                r_box.set_glue_set(if r_unset.shrink() == Scaled::ZERO {
                                    0.
                                } else if r_box.glue_order() == GlueOrder::Normal
                                    && r_unset.width() - t > r_unset.shrink()
                                {
                                    1.
                                } else {
                                    (r_unset.width() - t).0 as f64 / r_unset.shrink().0 as f64
                                    // BOX_glue
                                });
                            }
                        }
                        r_box.set_width(w);
                        r_box.set_horizontal();
                    } else {
                        r_box.set_width(q.width());
                        match t.cmp(&r_unset.height()) {
                            Ordering::Equal => {
                                r_box
                                    .set_glue_sign(GlueSign::Normal)
                                    .set_glue_order(GlueOrder::Normal)
                                    .set_glue_set(0.);
                            }
                            Ordering::Greater => {
                                r_box.set_glue_sign(GlueSign::Stretching).set_glue_set(
                                    if r_unset.stretch() == Scaled::ZERO {
                                        0.
                                    } else {
                                        (t - r_unset.height()).0 as f64 / r_unset.stretch().0 as f64
                                    },
                                );
                            }
                            Ordering::Less => {
                                r_box
                                    .set_glue_order(r_unset.shrink_order())
                                    .set_glue_sign(GlueSign::Shrinking);
                                r_box.set_glue_set(if r_unset.shrink() == Scaled::ZERO {
                                    0.
                                } else if r_box.glue_order() == GlueOrder::Normal
                                    && r_unset.height() - t > r_unset.shrink()
                                {
                                    1.
                                } else {
                                    (r_unset.height() - t).0 as f64 / r_unset.shrink().0 as f64
                                });
                            }
                        }
                        r_box.set_height(w);
                        r_box.set_vertical();
                    }
                    r_box.set_shift_amount(Scaled::ZERO);
                    if u != HOLD_HEAD {
                        *LLIST_link(u) = *LLIST_link(r);
                        *LLIST_link(r) = *LLIST_link(HOLD_HEAD);
                        r = u;
                    }
                    let ropt = LLIST_link(*LLIST_link(r) as usize).opt();
                    s = *LLIST_link(*LLIST_link(s) as usize) as usize;
                    if let Some(r_) = ropt {
                        r = r_;
                    } else {
                        break;
                    }
                }
            }
            CharOrText::Text(TxtNode::Rule(mut q_rule)) => {
                /*835: */
                if q_rule.width() == NULL_FLAG {
                    q_rule.set_width(p.width());
                }
                if q_rule.height() == NULL_FLAG {
                    q_rule.set_height(p.height());
                }
                if q_rule.depth() == NULL_FLAG {
                    q_rule.set_depth(p.depth());
                }
                if o != Scaled::ZERO {
                    let r = *LLIST_link(q);
                    *LLIST_link(q) = None.tex_int();
                    let mut q_box = hpack(Some(q), Scaled::ZERO, PackMode::Additional);
                    q_box.set_shift_amount(o);
                    q = q_box.ptr();
                    *LLIST_link(q) = r;
                    *LLIST_link(s) = Some(q).tex_int();
                }
            }
            _ => {}
        }
        s = q;
        qopt = llist_link(q);
    }
    flush_node_list(Some(p.ptr()));
    pop_alignment();
    let aux_save = cur_list.aux;
    let p = MEM[cur_list.head].b32.s1.opt();
    let q = cur_list.tail;
    pop_nest();
    if cur_list.mode == (false, ListMode::MMode) {
        /*1241: */
        let (tok, cmd, _) = do_assignments(input); /*1232: */
        if cmd != Cmd::MathShift {
            /*1242: */
            t_eprint!("Missing $$ inserted");
            help!(
                "Displays can use special alignments (like \\eqalignno)",
                "only if nothing but the alignment itself is between $$\'s."
            );
            back_error(input, tok);
        } else {
            let (tok, cmd, ..) = get_x_token(input);
            if cmd != Cmd::MathShift {
                t_eprint!("Display math should end with $$");
                help!(
                    "The `$\' that I just saw supposedly matches a previous `$$\'.",
                    "So I shall assume that you typed `$$\' both times."
                );
                back_error(input, tok);
            }
        }
        flush_node_list(cur_list.eTeX_aux);
        pop_nest();
        let pen = new_penalty(get_int_par(IntPar::pre_display_penalty));
        *LLIST_link(cur_list.tail) = Some(pen.ptr()).tex_int();
        cur_list.tail = pen.ptr();
        let pg = new_param_glue(GluePar::above_display_skip);
        *LLIST_link(cur_list.tail) = Some(pg.ptr()).tex_int();
        cur_list.tail = pg.ptr();
        *LLIST_link(cur_list.tail) = p.tex_int();
        if p.is_some() {
            cur_list.tail = q;
        }
        let pen = new_penalty(get_int_par(IntPar::post_display_penalty));
        *LLIST_link(cur_list.tail) = Some(pen.ptr()).tex_int();
        cur_list.tail = pen.ptr();
        let pg = new_param_glue(GluePar::below_display_skip);
        *LLIST_link(cur_list.tail) = Some(pg.ptr()).tex_int();
        cur_list.tail = pg.ptr();
        cur_list.aux.b32.s1 = aux_save.b32.s1;
        resume_after_display(input);
    } else {
        cur_list.aux = aux_save;
        *LLIST_link(cur_list.tail) = p.tex_int();
        if p.is_some() {
            cur_list.tail = q;
        }
        if cur_list.mode == (false, ListMode::VMode) {
            build_page(input);
        }
    };
}
pub(crate) unsafe fn align_peek(input: &mut input_state_t) {
    loop {
        align_state = 1000000;
        let (tok, cmd, chr) = loop {
            let next = get_x_or_protected(input);
            if next.1 != Cmd::Spacer {
                break next;
            }
        };
        if cmd == Cmd::NoAlign {
            scan_left_brace(input);
            new_save_level(GroupCode::NoAlign);
            if cur_list.mode == (true, ListMode::VMode) {
                normal_paragraph();
            }
            break;
        } else if cmd == Cmd::RightBrace {
            fin_align(input, cur_group);
            break;
        } else {
            if cmd == Cmd::CarRet && chr == CR_CR_CODE {
                continue;
            }
            init_row();
            init_col(input, tok, cmd);
            break;
        }
    }
}
pub(crate) unsafe fn max_hyphenatable_length() -> usize {
    (get_int_par(IntPar::xetex_hyphenatable_length) as usize).min(HYPHENATABLE_LENGTH_LIMIT)
}
pub(crate) unsafe fn eTeX_enabled(b: bool, j: Cmd, k: i32) -> bool {
    if !b {
        t_eprint!("Improper {}", CmdChr(j, k));
        help!("Sorry, this optional e-TeX feature has been disabled.");
        error();
    }
    b
}
pub(crate) unsafe fn show_save_groups(group: GroupCode, level: u16) {
    pub(crate) static mut cur_level1: u16 = 0;
    pub(crate) static mut cur_group1: GroupCode = GroupCode::BottomLevel;
    pub(crate) static mut SAVE_PTR1: usize = 0;

    unsafe fn do_loop(mut p: usize, mut a: i8) -> (bool, usize, i8) {
        t_print_nl!("### ");
        print_group(cur_group1, cur_level1, SAVE_PTR1, true);
        if cur_group1 == GroupCode::BottomLevel {
            return (true, p, a);
        }
        let m = loop {
            let mut m = NEST[p].mode;
            if p > 0 {
                p -= 1
            } else {
                m = (false, ListMode::VMode);
            }
            if m != (false, ListMode::HMode) {
                break m;
            }
        };
        t_print!(" (");
        let s = match cur_group1 {
            GroupCode::BottomLevel => unreachable!(),
            GroupCode::Simple => {
                p += 1;
                return found2(p, a);
            }
            GroupCode::HBox | GroupCode::AdjustedHBox => "hbox",
            GroupCode::VBox => "vbox",
            GroupCode::VTop => "vtop",
            GroupCode::Align => {
                if a == 0 {
                    let s = if m == (true, ListMode::VMode) {
                        "halign"
                    } else {
                        "valign"
                    };
                    a = 1;
                    return found1(s, p, a);
                } else {
                    if a == 1 {
                        t_print!("align entry");
                    } else {
                        print_esc_cstr("cr");
                    }
                    if p as i32 >= a as i32 {
                        p = (p as i32 - a as i32) as usize
                    }
                    a = 0;
                    return found(p, a);
                }
            }
            GroupCode::NoAlign => {
                p += 1;
                a = -1;
                print_esc_cstr("noalign");
                return found2(p, a);
            }
            GroupCode::Output => {
                print_esc_cstr("output");
                return found(p, a);
            }
            GroupCode::Math => return found2(p, a),
            GroupCode::Disc | GroupCode::MathChoice => {
                t_print!(
                    "{}",
                    if cur_group1 == GroupCode::Disc {
                        Esc("discretionary")
                    } else {
                        Esc("mathchoice")
                    }
                );
                let mut i = 1;
                while i <= 3 {
                    if i <= SAVE_STACK[SAVE_PTR1 - 2].val {
                        t_print!("{{}}");
                    }
                    i += 1;
                }
                return found2(p, a);
            }
            GroupCode::Insert => {
                let n = SAVE_STACK[SAVE_PTR1 - 2].val;
                if n == 255 {
                    t_print!("{}", Esc("vadjust"));
                } else {
                    t_print!("{}{}", Esc("insert"), n);
                }
                return found2(p, a);
            }
            GroupCode::VCenter => {
                return found1("vcenter", p, a);
            }
            GroupCode::SemiSimple => {
                p += 1;
                print_esc_cstr("begingroup");
                return found(p, a);
            }
            GroupCode::MathShift => {
                if m == (false, ListMode::MMode) {
                    t_print!("{}", '$');
                } else if NEST[p].mode == (false, ListMode::MMode) {
                    t_print!("{}", CmdChr(Cmd::EqNo, SAVE_STACK[SAVE_PTR1 - 2].val));
                    return found(p, a);
                }
                print_chr('$');
                return found(p, a);
            }
            GroupCode::MathLeft => {
                if MEM[NEST[p + 1].eTeX_aux.unwrap()].b16.s1 == MathNode::Left as u16 {
                    print_esc_cstr("left");
                } else {
                    print_esc_cstr("middle");
                }
                return found(p, a);
            }
        };

        let mut i = SAVE_STACK[SAVE_PTR1 - 4].val;

        if i != 0 {
            if i < BOX_FLAG {
                let j = if NEST[p].mode.1 == ListMode::VMode {
                    Cmd::HMove
                } else {
                    Cmd::VMove
                };
                t_print!(
                    "{}{}pt",
                    if i > 0 { CmdChr(j, 0) } else { CmdChr(j, 1) },
                    Scaled(i.abs())
                );
            } else if i < SHIP_OUT_FLAG {
                if i >= GLOBAL_BOX_FLAG {
                    print_esc_cstr("global");
                    i -= GLOBAL_BOX_FLAG - BOX_FLAG;
                }
                t_print!("{}{}=", Esc("setbox"), i - BOX_FLAG);
            } else {
                t_print!(
                    "{}",
                    CmdChr(Cmd::LeaderShip, i - (LEADER_FLAG - (A_LEADERS as i32)))
                );
            }
        }
        found1(s, p, a)
    }

    unsafe fn found1(s: &str, p: usize, a: i8) -> (bool, usize, i8) {
        t_print!("{}", Esc(s));
        if SAVE_STACK[SAVE_PTR1 - 2].val != 0 {
            if SAVE_STACK[SAVE_PTR1 - 3].val == PackMode::Exactly as i32 {
                t_print!(" to{}pt", Scaled(SAVE_STACK[SAVE_PTR1 - 2].val));
            } else {
                t_print!(" spread{}pt", Scaled(SAVE_STACK[SAVE_PTR1 - 2].val));
            }
        }
        found2(p, a)
    }

    unsafe fn found2(p: usize, a: i8) -> (bool, usize, i8) {
        print_chr('{');
        found(p, a)
    }

    unsafe fn found(p: usize, a: i8) -> (bool, usize, i8) {
        print_chr(')');
        cur_level1 = cur_level1.wrapping_sub(1);
        cur_group1 = GroupCode::from(SAVE_STACK[SAVE_PTR1].lvl);
        SAVE_PTR1 = SAVE_STACK[SAVE_PTR1].val as usize;
        (false, p, a)
    }

    let mut p = NEST_PTR;
    NEST[p] = cur_list;
    SAVE_PTR1 = cur_boundary as usize;
    cur_group1 = group;
    cur_level1 = level - 1;
    let mut a = 1_i8;
    t_print_nl!("");
    print_ln();
    loop {
        let (is_done, new_p, new_a) = do_loop(p, a);
        if is_done {
            return;
        }
        p = new_p;
        a = new_a;
    }
}
pub(crate) unsafe fn vert_break(mut p: i32, h: Scaled, d: Scaled) -> i32 {
    let mut best_place = None;
    let mut prev_p = p;
    let mut least_cost = MAX_HALFWORD;
    active_width = DeltaSize::new();
    let mut prev_dp = Scaled::ZERO;
    loop {
        if let Some(p) = p.opt() {
            /*1008: */
            match &mut TxtNode::from(p) {
                TxtNode::List(b) => {
                    active_width.width += prev_dp + b.height();
                    prev_dp = b.depth();
                }
                TxtNode::Rule(r) => {
                    active_width.width += prev_dp + r.height();
                    prev_dp = r.depth();
                }
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        active_width.width += prev_dp + p.height();
                        prev_dp = p.depth();
                    }
                    _ => {}
                },
                TxtNode::Glue(g) => {
                    match TxtNode::from(prev_p as usize) {
                        TxtNode::List(_)
                        | TxtNode::Rule(_)
                        | TxtNode::Ins(_)
                        | TxtNode::Mark(_)
                        | TxtNode::Adjust(_)
                        | TxtNode::Ligature(_)
                        | TxtNode::Disc(_)
                        | TxtNode::WhatsIt(_) => {
                            if with_penalty(p, 0, h, &mut prev_dp, &mut best_place, &mut least_cost)
                            {
                                return best_place.tex_int();
                            }
                        }
                        _ => {}
                    }
                    let q = GlueSpec(g.glue_ptr() as usize); /*:1011 */
                    match q.stretch_order() {
                        GlueOrder::Normal => active_width.stretch0 += q.stretch(),
                        GlueOrder::Fil => active_width.stretch1 += q.stretch(),
                        GlueOrder::Fill => active_width.stretch2 += q.stretch(),
                        GlueOrder::Filll => active_width.stretch3 += q.stretch(),
                        _ => unreachable!(),
                    } /*:1014*/
                    active_width.shrink += q.shrink();
                    let width =
                        if q.shrink_order() != GlueOrder::Normal && q.shrink() != Scaled::ZERO {
                            t_eprint!("Infinite glue shrinkage found in box being split");
                            help!(
                                "The box you are \\vsplitting contains some infinitely",
                                "shrinkable glue, e.g., `\\vss\' or `\\vskip 0pt minus 1fil\'.",
                                "Such glue doesn\'t belong there; but you can safely proceed,",
                                "since the offensive shrinkability has been made finite."
                            );
                            error();
                            let mut r = new_spec(&q);
                            r.set_shrink_order(GlueOrder::Normal);
                            delete_glue_ref(q.ptr());
                            g.set_glue_ptr(Some(r.ptr()).tex_int());
                            r.size()
                        } else {
                            q.size()
                        };
                    active_width.width += prev_dp + width;
                    prev_dp = Scaled::ZERO;
                }
                TxtNode::Kern(k) => {
                    if let Some(Node::Text(TxtNode::Glue(_))) = llist_link(p).map(Node::from) {
                        if with_penalty(p, 0, h, &mut prev_dp, &mut best_place, &mut least_cost) {
                            return best_place.tex_int();
                        }
                    }
                    active_width.width += prev_dp + k.width();
                    prev_dp = Scaled::ZERO;
                }
                TxtNode::Penalty(pen) => {
                    if with_penalty(
                        p,
                        pen.penalty(),
                        h,
                        &mut prev_dp,
                        &mut best_place,
                        &mut least_cost,
                    ) {
                        return best_place.tex_int();
                    }
                }
                TxtNode::Mark(_) | TxtNode::Ins(_) => {}
                _ => confusion("vertbreak"),
            }
        } else {
            // TODO: remove unreachable
            let mut b = if active_width.width < h {
                if active_width.stretch1 != Scaled::ZERO
                    || active_width.stretch2 != Scaled::ZERO
                    || active_width.stretch3 != Scaled::ZERO
                {
                    0
                } else {
                    badness(h - active_width.width, active_width.stretch0)
                }
            } else if active_width.width - h > active_width.shrink {
                MAX_HALFWORD
            } else {
                badness(active_width.width - h, active_width.shrink)
            };
            if b < MAX_HALFWORD {
                b = EJECT_PENALTY;
            }
            if b <= least_cost {
                best_place = None;
                //least_cost = b;
                best_height_plus_depth = active_width.width + prev_dp;
            }
            return best_place.tex_int();
        }
        if prev_dp > d {
            active_width.width += prev_dp - d;
            prev_dp = d
        }
        prev_p = p;
        p = *LLIST_link(p as usize);
    }

    unsafe fn with_penalty(
        p: usize,
        pi: i32,
        h: Scaled,
        prev_dp: &mut Scaled,
        best_place: &mut Option<usize>,
        least_cost: &mut i32,
    ) -> bool {
        if pi < INF_PENALTY {
            let mut b = if active_width.width < h {
                if active_width.stretch1 != Scaled::ZERO
                    || active_width.stretch2 != Scaled::ZERO
                    || active_width.stretch3 != Scaled::ZERO
                {
                    0
                } else {
                    badness(h - active_width.width, active_width.stretch0)
                }
            } else if active_width.width - h > active_width.shrink {
                MAX_HALFWORD
            } else {
                badness(active_width.width - h, active_width.shrink)
            };
            if b < MAX_HALFWORD {
                if pi <= EJECT_PENALTY {
                    b = pi
                } else if b < INF_BAD {
                    b += pi
                } else {
                    b = 100000;
                }
            }
            if b <= *least_cost {
                *best_place = Some(p);
                *least_cost = b;
                best_height_plus_depth = active_width.width + *prev_dp;
            }
            if b == MAX_HALFWORD || pi <= EJECT_PENALTY {
                return true;
            }
        }
        false
    }
}
pub(crate) unsafe fn vsplit(n: i32, h: Scaled) -> Option<usize> {
    let val = n;
    let v = if val < 256 {
        get_box_reg(val as usize)
    } else {
        find_sa_element(ValLevel::Ident, val, false);
        cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
    };
    flush_node_list(disc_ptr[VSPLIT_CODE as usize].opt());
    disc_ptr[VSPLIT_CODE as usize] = None.tex_int();
    if let Some(m) = sa_root[ValLevel::Mark as usize] {
        if do_marks(MarkMode::VSplitInit, 0, m) {
            sa_root[ValLevel::Mark as usize] = None;
        }
    }
    if let (Some(fm), Some(bm)) = (
        cur_mark[SPLIT_FIRST_MARK_CODE].take(),
        cur_mark[SPLIT_BOT_MARK_CODE].take(),
    ) {
        delete_token_ref(fm);
        delete_token_ref(bm);
    }
    let v = v?;
    let mut v = List::from(v);
    if !v.is_vertical() {
        t_eprint!("{} needs a {}", Esc("vsplit"), Esc("vbox"));
        help!(
            "The box you are trying to split is an \\hbox.",
            "I can\'t split such a box, so I\'ll leave it alone."
        );
        error();
        return None;
    }
    let q = vert_break(v.list_ptr(), h, get_dimen_par(DimenPar::split_max_depth));
    let mut p = v.list_ptr();
    if p == q {
        v.set_list_ptr(None.tex_int());
    } else {
        loop {
            if let Node::Text(TxtNode::Mark(p)) = Node::from(p as usize) {
                if p.class() != 0 {
                    /*1615: */
                    find_sa_element(ValLevel::Mark, p.class(), true);
                    let c = cur_ptr.unwrap();
                    if MEM[c + 2].b32.s1.opt().is_none() {
                        MEM[c + 2].b32.s1 = p.mark_ptr();
                        MEM[p.mark_ptr() as usize].b32.s0 += 1
                    } else {
                        delete_token_ref(MEM[c + 3].b32.s0 as usize);
                    }
                    MEM[c + 3].b32.s0 = p.mark_ptr();
                    MarkClass(p.mark_ptr() as usize).rc_inc();
                } else if cur_mark[SPLIT_FIRST_MARK_CODE].is_none() {
                    let mut m = MarkClass(p.mark_ptr().opt().unwrap());
                    cur_mark[SPLIT_FIRST_MARK_CODE] = Some(m.ptr());
                    m.rc_inc();
                    cur_mark[SPLIT_BOT_MARK_CODE] = Some(m.ptr());
                    m.rc_inc();
                } else {
                    delete_token_ref(cur_mark[SPLIT_BOT_MARK_CODE].unwrap());
                    cur_mark[SPLIT_BOT_MARK_CODE] = p.mark_ptr().opt();
                    MarkClass(cur_mark[SPLIT_BOT_MARK_CODE].unwrap()).rc_inc();
                }
            }
            if *LLIST_link(p as usize) == q {
                *LLIST_link(p as usize) = None.tex_int();
                break;
            } else {
                p = *LLIST_link(p as usize);
            }
        }
    }
    let q = prune_page_top(q.opt(), get_int_par(IntPar::saving_vdiscards) > 0).opt();
    let p = v.list_ptr().opt();
    free_node(v.ptr(), BOX_NODE_SIZE);
    let q = if let Some(q) = q {
        Some(
            vpackage(
                Some(q),
                Scaled::ZERO,
                PackMode::Additional,
                Scaled::MAX_HALFWORD,
            )
            .ptr(),
        )
    } else {
        None
    };
    if val < 256 {
        set_box_reg(val as usize, q);
    } else {
        find_sa_element(ValLevel::Ident, val, false);
        if let Some(c) = cur_ptr {
            MEM[c + 1].b32.s1 = q.tex_int();
            MEM[c + 1].b32.s0 += 1;
            delete_sa_ref(c);
        }
    }
    Some(
        vpackage(
            p,
            h,
            PackMode::Exactly,
            get_dimen_par(DimenPar::split_max_depth),
        )
        .ptr(),
    )
}
pub(crate) unsafe fn print_totals() {
    t_print!("{}", page_so_far[1]);
    if page_so_far[2] != Scaled::ZERO {
        t_print!(" plus {}", page_so_far[2]);
    }
    if page_so_far[3] != Scaled::ZERO {
        t_print!(" plus {}fil", page_so_far[3]);
    }
    if page_so_far[4] != Scaled::ZERO {
        t_print!(" plus {}fill", page_so_far[4]);
    }
    if page_so_far[5] != Scaled::ZERO {
        t_print!(" plus {}filll", page_so_far[5]);
    }
    if page_so_far[6] != Scaled::ZERO {
        t_print!(" minus {}", page_so_far[6]);
    };
}
pub(crate) unsafe fn box_error(n: u8) {
    error();
    diagnostic(true, || {
        t_print_nl!("The following box has been deleted:");
        show_box(get_box_reg(n as usize));
    });
    flush_node_list(get_box_reg(n as usize));
    set_box_reg(n as usize, None);
}
pub(crate) unsafe fn app_space() {
    let q;
    if cur_list.aux.b32.s0 >= 2000 && get_glue_par(GluePar::xspace_skip).ptr() != 0 {
        q = new_param_glue(GluePar::xspace_skip)
    } else {
        let main_p = if get_glue_par(GluePar::space_skip).ptr() != 0 {
            get_glue_par(GluePar::space_skip)
        } else {
            /*1077: */
            FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize]
                .opt()
                .map(GlueSpec)
                .unwrap_or_else(|| {
                    /*:1079 */
                    let mut main_p = new_spec(&ZERO_GLUE);
                    main_k = PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize] + 2;
                    main_p
                        .set_size(Scaled(FONT_INFO[main_k as usize].b32.s1))
                        .set_stretch(Scaled(FONT_INFO[(main_k + 1) as usize].b32.s1))
                        .set_shrink(Scaled(FONT_INFO[(main_k + 2) as usize].b32.s1));
                    FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize] = Some(main_p.ptr()).tex_int();
                    main_p
                })
        };
        let mut main_p = new_spec(&main_p);
        if cur_list.aux.b32.s0 >= 2000 {
            main_p.set_size(
                main_p.size()
                    + Scaled(
                        FONT_INFO[(EXTRA_SPACE_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize])
                            as usize]
                            .b32
                            .s1,
                    ),
            );
        }
        main_p.set_stretch(xn_over_d(main_p.stretch(), Scaled(cur_list.aux.b32.s0), 1000).0);
        main_p.set_shrink(xn_over_d(main_p.shrink(), Scaled(1000), cur_list.aux.b32.s0).0);
        q = new_glue(&main_p);
        main_p.rc_set_none();
    }
    *LLIST_link(cur_list.tail) = Some(q.ptr()).tex_int();
    cur_list.tail = q.ptr();
}
pub(crate) unsafe fn insert_dollar_sign(input: &mut input_state_t, tok: i32) {
    back_input(input, tok);
    let tok = MATH_SHIFT_TOKEN + 36;
    t_eprint!("Missing $ inserted");
    help!(
        "I\'ve inserted a begin-math/end-math symbol since I think",
        "you left one out. Proceed, with fingers crossed."
    );
    ins_error(input, tok);
}
pub(crate) unsafe fn you_cant(cmd: Cmd, chr: i32) {
    t_eprint!("You can\'t use `{}", CmdChr(cmd, chr));
    print_in_mode(cur_list.mode);
}
pub(crate) unsafe fn report_illegal_case(cmd: Cmd, chr: i32) {
    you_cant(cmd, chr);
    help!(
        "Sorry, but I\'m not programmed to handle this case;",
        "I\'ll just pretend that you didn\'t ask for it.",
        "If you\'re in the wrong mode, you might be able to",
        "return to the right one by typing `I}\' or `I$\' or `I\\par\'."
    );
    error();
}
pub(crate) unsafe fn privileged(cmd: Cmd, chr: i32) -> bool {
    if !cur_list.mode.0 {
        true
    } else {
        report_illegal_case(cmd, chr);
        false
    }
}
pub(crate) unsafe fn its_all_over(input: &mut input_state_t, tok: i32, cmd: Cmd, chr: i32) -> bool {
    if privileged(cmd, chr) {
        if PAGE_HEAD == page_tail && cur_list.head == cur_list.tail && dead_cycles == 0 {
            return true;
        }
        back_input(input, tok);
        let mut nb = List::from(new_null_box());
        *LLIST_link(cur_list.tail) = Some(nb.ptr()).tex_int();
        cur_list.tail = nb.ptr();
        nb.set_width(get_dimen_par(DimenPar::hsize));
        let g = new_glue(&FILL_GLUE);
        *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
        cur_list.tail = g.ptr();
        let p = new_penalty(NULL_FLAG.0);
        *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
        cur_list.tail = p.ptr();
        build_page(input);
    }
    false
}
pub(crate) unsafe fn append_glue(input: &mut input_state_t, chr: i32) {
    let s = SkipCode::n(chr as u8).unwrap();
    let val = match s {
        SkipCode::Fil => FIL_GLUE,
        SkipCode::Fill => FILL_GLUE,
        SkipCode::Ss => SS_GLUE,
        SkipCode::FilNeg => FIL_NEG_GLUE,
        SkipCode::Skip => scan_glue(input, ValLevel::Glue),
        SkipCode::MSkip => scan_glue(input, ValLevel::Mu),
    };
    let g = new_glue(&val);
    *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
    cur_list.tail = g.ptr();
    if s == SkipCode::Skip || s == SkipCode::MSkip {
        MEM[val.ptr()].b32.s1 -= 1;
        if s == SkipCode::MSkip {
            MEM[g.ptr()].b16.s0 = MU_GLUE;
        }
    };
}
pub(crate) unsafe fn append_kern(input: &mut input_state_t, chr: i32) {
    let s = chr as u16;
    let val = scan_dimen(input, s == KernType::Math as u16, false, None);
    let k = new_kern(val);
    *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
    cur_list.tail = k.ptr();
    MEM[cur_list.tail].b16.s0 = s;
}
pub(crate) unsafe fn off_save(
    input: &mut input_state_t,
    group: GroupCode,
    tok: i32,
    cmd: Cmd,
    chr: i32,
) {
    if group == GroupCode::BottomLevel {
        /*1101:*/
        t_eprint!("Extra {}", CmdChr(cmd, chr));
        help!("Things are pretty mixed up, but I think the worst is over.");
        error();
    } else {
        back_input(input, tok);
        let mut p = get_avail();
        *LLIST_link(TEMP_HEAD) = Some(p).tex_int();
        t_eprint!("Missing ");
        match group {
            GroupCode::SemiSimple => {
                MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_END_GROUP as i32;
                print_esc_cstr("endgroup");
            }
            GroupCode::MathShift => {
                MEM[p].b32.s0 = MATH_SHIFT_TOKEN + '$' as i32;
                print_chr('$');
            }
            GroupCode::MathLeft => {
                MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_RIGHT as i32;
                *LLIST_link(p) = Some(get_avail()).tex_int();
                p = *LLIST_link(p) as usize;
                MEM[p].b32.s0 = OTHER_TOKEN + '.' as i32;
                print_esc_cstr("right.");
            }
            _ => {
                MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
                print_chr('}');
            }
        }
        t_print!(" inserted");
        begin_token_list(input, llist_link(TEMP_HEAD), Btl::Inserted);
        help!(
            "I\'ve inserted something that you may have forgotten.",
            "(See the <inserted text> above.)",
            "With luck, this will get me unwedged. But if you",
            "really didn\'t forget anything, try typing `2\' now; then",
            "my insertion and my current dilemma will both disappear."
        );
        error();
    };
}
pub(crate) unsafe fn extra_right_brace(group: GroupCode) {
    t_eprint!("Extra }}, or forgotten ");
    match group {
        GroupCode::SemiSimple => t_print!("{}", Esc("endgroup")),
        GroupCode::MathShift => t_print!("$"),
        GroupCode::MathLeft => t_print!("{}", Esc("right")),
        _ => {}
    }
    help!(
        "I\'ve deleted a group-closing symbol because it seems to be",
        "spurious, as in `$x}$\'. But perhaps the } is legitimate and",
        "you forgot something else, as in `\\hbox{$x}\'. In such cases",
        "the way to recover is to insert both the forgotten and the",
        "deleted material, e.g., by typing `I$}\'."
    );
    error();
    align_state += 1;
}
pub(crate) unsafe fn normal_paragraph() {
    if get_int_par(IntPar::looseness) != 0 {
        eq_word_define(INT_BASE as usize + (IntPar::looseness as usize), 0);
    }
    if get_dimen_par(DimenPar::hang_indent) != Scaled::ZERO {
        eq_word_define(DIMEN_BASE as usize + (DimenPar::hang_indent as usize), 0);
    }
    if get_int_par(IntPar::hang_after) != 1 {
        eq_word_define(INT_BASE as usize + (IntPar::hang_after as usize), 1);
    }
    if LOCAL(Local::par_shape).opt().is_some() {
        eq_define(
            LOCAL_BASE as usize + Local::par_shape as usize,
            Cmd::ShapeRef,
            None,
        );
    }
    if EQTB[INTER_LINE_PENALTIES_LOC].val.opt().is_some() {
        eq_define(INTER_LINE_PENALTIES_LOC, Cmd::ShapeRef, None);
    };
}
/*1110: "The box_end procedure does the right thing with cur_box, if
 * box_context represents the context as explained [as follows]." The
 * box_context is one of (1) a signed shift amount; (2) BOX_FLAG+N, signifying
 * a `\setbox<N>`; (3) GLOBAL_BOX_FLAG+N, signifying `\global\setbox<N>`; (4)
 * SHIP_OUT_FLAG, signifying `\shipout`; or (5) LEADER_FLAG+k, signifying (in
 * order) `\leaders`, `\cleaders`, or `\xleaders`. */
pub(crate) unsafe fn box_end(input: &mut input_state_t, box_context: i32) {
    if box_context < BOX_FLAG {
        /*1111:*/
        if let Some(mut cb) = cur_box {
            List::from(cb).set_shift_amount(Scaled(box_context));
            if cur_list.mode.1 == ListMode::VMode {
                if let Some(a) = pre_adjust_tail {
                    if PRE_ADJUST_HEAD != a {
                        *LLIST_link(cur_list.tail) = *LLIST_link(PRE_ADJUST_HEAD);
                        cur_list.tail = a;
                    }
                    pre_adjust_tail = None;
                }
                append_to_vlist(List::from(cb));
                if let Some(a) = adjust_tail {
                    if ADJUST_HEAD != a {
                        *LLIST_link(cur_list.tail) = *LLIST_link(ADJUST_HEAD);
                        cur_list.tail = a;
                    }
                    adjust_tail = None;
                }
                if !cur_list.mode.0 {
                    build_page(input);
                }
            } else {
                if cur_list.mode.1 == ListMode::HMode {
                    cur_list.aux.b32.s0 = 1000
                } else {
                    let p = new_noad();
                    MEM[p + 1].b32.s1 = MathCell::SubBox as _;
                    MEM[p + 1].b32.s0 = Some(cb).tex_int();
                    cb = p;
                    cur_box = Some(cb);
                }
                *LLIST_link(cur_list.tail) = Some(cb).tex_int();
                cur_list.tail = cb;
            }
        }
    } else if box_context < SHIP_OUT_FLAG {
        /*1112:*/
        let (val, a) = if box_context < GLOBAL_BOX_FLAG {
            (box_context - BOX_FLAG, 0)
        } else {
            (box_context - GLOBAL_BOX_FLAG, 4)
        };
        if val < 256 {
            if a >= 4 {
                geq_define(BOX_BASE + val as usize, Cmd::BoxRef, cur_box);
            } else {
                eq_define(BOX_BASE + val as usize, Cmd::BoxRef, cur_box);
            }
        } else {
            find_sa_element(ValLevel::Ident, val, true);
            if a >= 4 {
                gsa_def(cur_ptr.unwrap(), cur_box);
            } else {
                sa_def(cur_ptr.unwrap(), cur_box);
            }
        }
    } else if let Some(cb) = cur_box {
        if box_context > SHIP_OUT_FLAG {
            let (tok, cmd, chr, _) = loop
            /*1113:*/
            {
                let next = get_x_token(input);
                if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
                    break next;
                }
            };
            if cmd == Cmd::HSkip && cur_list.mode.1 != ListMode::VMode
                || cmd == Cmd::VSkip && cur_list.mode.1 == ListMode::VMode
            {
                append_glue(input, chr);
                MEM[cur_list.tail].b16.s0 =
                    (box_context - (LEADER_FLAG - (A_LEADERS as i32))) as u16;
                MEM[cur_list.tail + 1].b32.s1 = Some(cb).tex_int();
            } else {
                t_eprint!("Leaders not followed by proper glue");
                help!(
                    "You should say `\\leaders <box or rule><hskip or vskip>\'.",
                    "I found the <box or rule>, but there\'s no suitable",
                    "<hskip or vskip>, so I\'m ignoring these leaders."
                );
                back_error(input, tok);
                flush_node_list(Some(cb));
            }
        } else {
            ship_out(List::from(cb));
        }
    };
}
pub(crate) unsafe fn begin_box(input: &mut input_state_t, cmd: Cmd, chr: i32, box_context: i32) {
    match BoxCode::n(chr as u8).unwrap() {
        BoxCode::Box => {
            let val = scan_register_num(input);
            cur_box = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            if val < 256 {
                set_box_reg(val as usize, None)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                if let Some(c) = cur_ptr {
                    MEM[c + 1].b32.s1 = None.tex_int();
                    MEM[c + 1].b32.s0 += 1;
                    delete_sa_ref(c);
                }
            }
        }
        BoxCode::Copy => {
            let val = scan_register_num(input);
            let q = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            cur_box = copy_node_list(q).opt();
        }
        BoxCode::LastBox => {
            cur_box = None;
            if cur_list.mode.1 == ListMode::MMode {
                you_cant(cmd, chr);
                help!("Sorry; this \\lastbox will be void.");
                error();
            } else if cur_list.mode == (false, ListMode::VMode) && cur_list.head == cur_list.tail {
                you_cant(cmd, chr);
                help!(
                    "Sorry...I usually can\'t take things from the current page.",
                    "This \\lastbox will therefore be void."
                );
                error();
            } else {
                let mut tx = cur_list.tail as i32;
                match CharOrText::from(tx as usize) {
                    CharOrText::Text(TxtNode::Math(m))
                        if m.subtype() == MathType::Eq(BE::End, MathMode::Middle) =>
                    {
                        let mut r = cur_list.head as i32;
                        let mut q;
                        loop {
                            q = r;
                            r = *LLIST_link(q as usize);
                            if r == tx {
                                break;
                            }
                        }
                        tx = q
                    }
                    _ => {}
                }
                if let CharOrText::Text(TxtNode::List(b)) = &mut CharOrText::from(tx as usize) {
                    /*1116:*/
                    let mut q = cur_list.head as i32;
                    let mut p = None.tex_int();
                    loop {
                        let r = p;
                        p = q;
                        let mut fm = false;
                        match CharOrText::from(q as usize) {
                            CharOrText::Text(TxtNode::Disc(d)) => {
                                for _ in 0..d.replace_count() {
                                    p = *LLIST_link(p as usize);
                                }
                                if p == tx {
                                    break;
                                }
                            }
                            CharOrText::Text(TxtNode::Math(m))
                                if m.subtype() == MathType::Eq(BE::Begin, MathMode::Middle) =>
                            {
                                fm = true;
                            }
                            _ => {}
                        }
                        q = *LLIST_link(p as usize);
                        if q == tx {
                            q = *LLIST_link(tx as usize);
                            *LLIST_link(p as usize) = q;
                            *LLIST_link(tx as usize) = None.tex_int();
                            if q.opt().is_none() {
                                if fm {
                                    confusion("tail1");
                                } else {
                                    cur_list.tail = p as usize;
                                }
                            } else if fm {
                                cur_list.tail = r as usize;
                                *LLIST_link(r as usize) = None.tex_int();
                                flush_node_list(p.opt());
                            }
                            cur_box = Some(tx as usize);
                            b.set_shift_amount(Scaled::ZERO);
                            break;
                        }
                    }
                }
            }
        }
        BoxCode::VSplit => {
            let n = scan_register_num(input);
            if !scan_keyword(input, "to") {
                t_eprint!("Missing `to\' inserted");
                help!(
                    "I\'m working on `\\vsplit<box number> to <dimen>\';",
                    "will look for the <dimen> next."
                );
                error();
            }
            let val = scan_dimen(input, false, false, None);
            cur_box = vsplit(n, val);
        }
        _ => {
            let k = chr - 4;
            let mut k = (k < 0, ListMode::from(k.abs() as u8));
            SAVE_STACK[SAVE_PTR + 0].val = box_context;
            if k == (false, ListMode::HMode) {
                if box_context < BOX_FLAG && cur_list.mode.1 == ListMode::VMode {
                    scan_spec(input, GroupCode::AdjustedHBox, true);
                } else {
                    scan_spec(input, GroupCode::HBox, true);
                }
            } else {
                if k == (false, ListMode::VMode) {
                    scan_spec(input, GroupCode::VBox, true);
                } else {
                    scan_spec(input, GroupCode::VTop, true);
                    k = (false, ListMode::VMode);
                }
                normal_paragraph();
            }
            push_nest();
            cur_list.mode = (!k.0, k.1);
            if k == (false, ListMode::VMode) {
                cur_list.aux.b32.s1 = IGNORE_DEPTH;
                if let Some(ev) = LOCAL(Local::every_vbox).opt() {
                    begin_token_list(input, Some(ev), Btl::EveryVBoxText);
                }
            } else {
                cur_list.aux.b32.s0 = 1000;
                if let Some(eh) = LOCAL(Local::every_hbox).opt() {
                    begin_token_list(input, Some(eh), Btl::EveryHBoxText);
                }
            }
            return;
        }
    }
    box_end(input, box_context);
}
pub(crate) unsafe fn scan_box(input: &mut input_state_t, box_context: i32) {
    let (tok, cmd, chr, _) = loop {
        let next = get_x_token(input);
        if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
            break next;
        }
    };
    if cmd == Cmd::MakeBox {
        begin_box(input, cmd, chr, box_context);
    } else if box_context >= LEADER_FLAG && (cmd == Cmd::HRule || cmd == Cmd::VRule) {
        cur_box = Some(scan_rule_spec(input, cmd).ptr());
        box_end(input, box_context);
    } else {
        t_eprint!("A <box> was supposed to be here");
        help!(
            "I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
            "something like that. So you might find something missing in",
            "your output. But keep trying; you can fix this later."
        );
        back_error(input, tok);
    };
}
pub(crate) unsafe fn package(input: &mut input_state_t, c: i16) {
    let d = get_dimen_par(DimenPar::box_max_depth);
    let u = get_int_par(IntPar::xetex_upwards);
    unsave(input);
    SAVE_PTR -= 3;
    let v = get_int_par(IntPar::xetex_upwards);
    set_int_par(IntPar::xetex_upwards, u);
    if cur_list.mode == (true, ListMode::HMode) {
        cur_box = Some(
            hpack(
                MEM[cur_list.head].b32.s1.opt(),
                Scaled(SAVE_STACK[SAVE_PTR + 2].val),
                PackMode::from(SAVE_STACK[SAVE_PTR + 1].val),
            )
            .ptr(),
        );
    } else {
        let mut cb = vpackage(
            MEM[cur_list.head].b32.s1.opt(),
            Scaled(SAVE_STACK[SAVE_PTR + 2].val),
            PackMode::from(SAVE_STACK[SAVE_PTR + 1].val),
            d,
        );
        cur_box = Some(cb.ptr());
        if c == BoxCode::VTop as i16 {
            /*1122: */
            let mut h = Scaled::ZERO;
            if let Some(p) = cb.list_ptr().opt() {
                match Node::from(p) {
                    Node::Text(TxtNode::List(b)) => {
                        h = b.height();
                    }
                    Node::Text(TxtNode::Rule(r)) => {
                        h = r.height();
                    }
                    _ => {}
                }
            }
            let cbd = cb.depth();
            let cbh = cb.height();
            cb.set_depth(cbd - h + cbh);
            cb.set_height(h);
        }
    }
    set_int_par(IntPar::xetex_upwards, v);
    pop_nest();
    box_end(input, SAVE_STACK[SAVE_PTR + 0].val);
}
pub(crate) unsafe fn norm_min(h: i32) -> i16 {
    (if h <= 0 {
        1
    } else if h >= 63 {
        63
    } else {
        h
    }) as i16
}
pub(crate) unsafe fn new_graf(input: &mut input_state_t, indented: bool) {
    cur_list.prev_graf = 0;
    if cur_list.mode == (false, ListMode::VMode) || cur_list.head != cur_list.tail {
        let pg = new_param_glue(GluePar::par_skip);
        *LLIST_link(cur_list.tail) = Some(pg.ptr()).tex_int();
        cur_list.tail = pg.ptr();
    }
    push_nest();
    cur_list.mode = (false, ListMode::HMode);
    cur_list.aux.b32.s0 = 1000;
    let lang = get_int_par(IntPar::language);
    cur_lang = if lang <= 0 || lang > BIGGEST_LANG {
        0
    } else {
        lang as u8
    };
    cur_list.aux.b32.s1 = cur_lang as i32;
    cur_list.prev_graf = ((norm_min(get_int_par(IntPar::left_hyphen_min)) as i32 * 64i32
        + norm_min(get_int_par(IntPar::right_hyphen_min)) as i32) as i64
        * 65536
        + cur_lang as i64) as i32;
    if indented {
        let mut nb = List::from(new_null_box());
        cur_list.tail = nb.ptr();
        MEM[cur_list.head].b32.s1 = Some(nb.ptr()).tex_int();
        nb.set_width(get_dimen_par(DimenPar::par_indent));
        if insert_src_special_every_par {
            insert_src_special();
        }
    }
    if let Some(ep) = LOCAL(Local::every_par).opt() {
        begin_token_list(input, Some(ep), Btl::EveryParText);
    }
    if NEST_PTR == 1 {
        build_page(input);
    };
}
pub(crate) unsafe fn indent_in_hmode(chr: i32) {
    if chr > 0 {
        let mut p = List::from(new_null_box());
        p.set_width(get_dimen_par(DimenPar::par_indent));
        let p = if cur_list.mode.1 == ListMode::HMode {
            cur_list.aux.b32.s0 = 1000;
            p.ptr()
        } else {
            let q = new_noad();
            MEM[q + 1].b32.s1 = MathCell::SubBox as _;
            MEM[q + 1].b32.s0 = Some(p.ptr()).tex_int();
            q
        };
        *LLIST_link(cur_list.tail) = Some(p).tex_int();
        cur_list.tail = p;
    };
}
pub(crate) unsafe fn head_for_vmode(input: &mut input_state_t, tok: i32, cmd: Cmd, chr: i32) {
    if cur_list.mode.0 {
        if cmd != Cmd::HRule {
            off_save(input, cur_group, tok, cmd, chr);
        } else {
            t_eprint!(
                "You can\'t use `{}\' here except with leaders",
                Esc("hrule")
            );
            help!(
                "To put a horizontal rule in an hbox or an alignment,",
                "you should use \\leaders or \\hrulefill (see The TeXbook)."
            );
            error();
        }
    } else {
        back_input(input, tok);
        let tok = par_token;
        back_input(input, tok);
        input.index = Btl::Inserted;
    };
}
pub(crate) unsafe fn end_graf() {
    if cur_list.mode == (false, ListMode::HMode) {
        if cur_list.head == cur_list.tail {
            pop_nest();
        } else {
            line_break(false);
        }
        if cur_list.eTeX_aux.is_some() {
            flush_list(cur_list.eTeX_aux);
            cur_list.eTeX_aux = None;
        }
        normal_paragraph();
        error_count = 0;
    };
}
pub(crate) unsafe fn begin_insert_or_adjust(input: &mut input_state_t, cmd: Cmd) {
    let val = if cmd == Cmd::VAdjust {
        255
    } else {
        let val = scan_eight_bit_int(input);
        if val == 255 {
            t_eprint!("You can\'t {}{}", Esc("insert"), 255);
            help!("I\'m changing to \\insert0; box 255 is special.");
            error();
            0
        } else {
            val
        }
    };
    SAVE_STACK[SAVE_PTR + 0].val = val;
    if cmd == Cmd::VAdjust && scan_keyword(input, "pre") {
        SAVE_STACK[SAVE_PTR + 1].val = 1;
    } else {
        SAVE_STACK[SAVE_PTR + 1].val = 0;
    }
    SAVE_PTR += 2;
    new_save_level(GroupCode::Insert);
    scan_left_brace(input);
    normal_paragraph();
    push_nest();
    cur_list.mode = (true, ListMode::VMode);
    cur_list.aux.b32.s1 = IGNORE_DEPTH;
}
pub(crate) unsafe fn make_mark(input: &mut input_state_t, chr: i32, cs: i32) {
    let c = if chr == 0 {
        0
    } else {
        scan_register_num(input)
    };
    let _p = scan_toks(input, cs, false, true);
    let mut p = Mark(get_node(SMALL_NODE_SIZE));
    p.set_class(c);
    set_NODE_type(p.ptr() as usize, TextNode::Mark);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_mark_ptr(def_ref as i32);
    *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
    cur_list.tail = p.ptr();
}
pub(crate) unsafe fn append_penalty(input: &mut input_state_t) {
    let val = scan_int(input);
    let p = new_penalty(val);
    *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
    cur_list.tail = p.ptr();
    if cur_list.mode == (false, ListMode::VMode) {
        build_page(input);
    };
}
pub(crate) unsafe fn delete_last(cmd: Cmd, chr: i32) {
    if cur_list.mode == (false, ListMode::VMode) && cur_list.tail == cur_list.head {
        /*1141: */
        if chr != TextNode::Glue as i32 || last_glue != MAX_HALFWORD {
            you_cant(cmd, chr);
            let help0 = if chr == TextNode::Kern as i32 {
                &"Try `I\\kern-\\lastkern\' instead."[..]
            } else if chr != TextNode::Glue as i32 {
                &"Perhaps you can make the output routine do it."[..]
            } else {
                &"Try `I\\vskip-\\lastskip\' instead."[..]
            };
            help!(
                "Sorry...I usually can\'t take things from the current page.",
                help0
            );
            error();
        }
    } else {
        let mut tx = cur_list.tail;
        match Node::from(tx) {
            Node::Text(TxtNode::Math(m))
                if m.subtype() == MathType::Eq(BE::End, MathMode::Middle) =>
            {
                let mut r = cur_list.head as i32;
                let mut q;
                loop {
                    q = r;
                    r = *LLIST_link(q as usize);
                    if r == tx as i32 {
                        break;
                    }
                }
                tx = q as usize;
            }
            _ => {}
        }
        if !is_char_node(Some(tx)) && MEM[tx].b16.s1 as i32 == chr {
            let mut fm;
            let mut q = cur_list.head as i32;
            let mut p = None.tex_int();
            let mut r;
            loop {
                r = p;
                p = q;
                fm = false;
                match Node::from(q as usize) {
                    Node::Text(TxtNode::Disc(d)) => {
                        for _ in 0..d.replace_count() {
                            p = *LLIST_link(p as usize);
                        }
                        if p == tx as i32 {
                            return;
                        }
                    }
                    Node::Text(TxtNode::Math(m))
                        if m.subtype() == MathType::Eq(BE::Begin, MathMode::Middle) =>
                    {
                        fm = true
                    }
                    _ => {}
                }
                q = *LLIST_link(p as usize);
                if q == tx as i32 {
                    break;
                }
            }
            q = *LLIST_link(tx);
            *LLIST_link(p as usize) = q;
            *LLIST_link(tx) = None.tex_int();
            if q.opt().is_none() {
                if fm {
                    confusion("tail1");
                } else {
                    cur_list.tail = p as usize;
                }
            } else if fm {
                cur_list.tail = r as usize;
                *LLIST_link(r as usize) = None.tex_int();
                flush_node_list(p.opt());
            }
            flush_node_list(Some(tx));
        }
    };
}
pub(crate) unsafe fn unpackage(input: &mut input_state_t, chr: i32) {
    let c = BoxCode::n(chr as u8).unwrap();
    match c {
        BoxCode::Box | BoxCode::Copy => {
            let val = scan_register_num(input);
            let p = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            if let Some(p) = p {
                let p = List::from(p);
                let dir = p.list_dir();
                if cur_list.mode.1 == ListMode::MMode
                    || cur_list.mode.1 == ListMode::VMode && dir != ListDir::Vertical
                    || cur_list.mode.1 == ListMode::HMode && dir != ListDir::Horizontal
                {
                    t_eprint!("Incompatible list can\'t be unboxed");
                    help!(
                        "Sorry, Pandora. (You sneaky devil.)",
                        "I refuse to unbox an \\hbox in vertical mode or vice versa.",
                        "And I can\'t open any boxes in math mode."
                    );
                    error();
                    return;
                }
                if c == BoxCode::Copy {
                    *LLIST_link(cur_list.tail) = copy_node_list(p.list_ptr().opt())
                } else {
                    *LLIST_link(cur_list.tail) = p.list_ptr();
                    if val < 256 {
                        set_box_reg(val as usize, None);
                    } else {
                        find_sa_element(ValLevel::Ident, val, false);
                        if let Some(c) = cur_ptr {
                            MEM[c + 1].b32.s1 = None.tex_int();
                            MEM[c + 1].b32.s0 += 1;
                            delete_sa_ref(c);
                        }
                    }
                    p.free();
                }
            } else {
                return;
            }
        }
        _ => {
            /*1651: */
            *LLIST_link(cur_list.tail) = disc_ptr[chr as usize]; /*:1156 */
            disc_ptr[chr as usize] = None.tex_int();
        }
    }
    while let Some(r) = LLIST_link(cur_list.tail).opt() {
        if let CharOrText::Text(TxtNode::MarginKern(r)) = CharOrText::from(r) {
            *LLIST_link(cur_list.tail) = *LLIST_link(r.ptr());
            r.free();
        }
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    }
}
pub(crate) unsafe fn append_italic_correction() {
    if cur_list.tail != cur_list.head {
        let p = match CharOrText::from(cur_list.tail) {
            CharOrText::Char(c) => c,
            CharOrText::Text(TxtNode::Ligature(l)) => l.as_char(),
            CharOrText::Text(TxtNode::WhatsIt(p)) => {
                match p {
                    WhatsIt::NativeWord(nw) => {
                        let mut k = new_kern(nw.italic_correction());
                        *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
                        cur_list.tail = k.ptr();
                        k.set_subtype(KernType::Explicit);
                    }
                    WhatsIt::Glyph(g) => {
                        let mut k = new_kern(g.italic_correction());
                        *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
                        cur_list.tail = k.ptr();
                        k.set_subtype(KernType::Explicit);
                    }
                    _ => {}
                }
                return;
            }
            _ => return,
        };
        let f = p.font() as internal_font_number;
        let mut k = new_kern(*FONT_CHARINFO_ITALCORR(
            f,
            FONT_CHARACTER_INFO(f, effective_char(true, f, p.character()) as usize),
        ));
        *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
        cur_list.tail = k.ptr();
        k.set_subtype(KernType::Explicit);
    };
}
pub(crate) unsafe fn append_discretionary(input: &mut input_state_t, chr: i32) {
    let d = new_disc();
    *LLIST_link(cur_list.tail) = Some(d).tex_int();
    cur_list.tail = d;
    if chr == 1 {
        let c = HYPHEN_CHAR[EQTB[CUR_FONT_LOC].val as usize];
        if c >= 0 && c < TOO_BIG_CHAR {
            MEM[cur_list.tail + 1].b32.s0 =
                new_character(EQTB[CUR_FONT_LOC].val as usize, c as UTF16_code).tex_int();
        }
    } else {
        SAVE_PTR += 1;
        SAVE_STACK[SAVE_PTR - 1].val = 0;
        new_save_level(GroupCode::Disc);
        scan_left_brace(input);
        push_nest();
        cur_list.mode = (true, ListMode::HMode);
        cur_list.aux.b32.s0 = 1000;
    };
}
pub(crate) unsafe fn build_discretionary(input: &mut input_state_t) {
    unsave(input);
    let mut q = cur_list.head;
    let mut popt = llist_link(q);
    let mut n = 0;
    while let Some(p) = popt {
        match CharOrText::from(p) {
            CharOrText::Char(_) => {}
            CharOrText::Text(n) => match n {
                TxtNode::List(_)
                | TxtNode::Rule(_)
                | TxtNode::Kern(_)
                | TxtNode::Ligature(_)
                | TxtNode::WhatsIt(WhatsIt::NativeWord(_))
                | TxtNode::WhatsIt(WhatsIt::Glyph(_)) => {}
                _ => {
                    t_eprint!("Improper discretionary list");
                    help!("Discretionary lists must contain only boxes and kerns.");
                    error();
                    diagnostic(true, || {
                        t_print_nl!("The following discretionary sublist has been deleted:");
                        show_box(Some(p));
                    });
                    flush_node_list(Some(p));
                    *LLIST_link(q as usize) = None.tex_int();
                    break;
                }
            },
        }
        q = p;
        popt = llist_link(q);
        n += 1;
    }
    let p = MEM[cur_list.head].b32.s1;
    pop_nest();
    match SAVE_STACK[SAVE_PTR - 1].val {
        0 => MEM[cur_list.tail + 1].b32.s0 = p,
        1 => MEM[cur_list.tail + 1].b32.s1 = p,
        2 => {
            if n > 0 && cur_list.mode.1 == ListMode::MMode {
                t_eprint!("Illegal math {}", Esc("discretionary"));
                help!(
                    "Sorry: The third part of a discretionary break must be",
                    "empty, in math formulas. I had to delete your third part."
                );
                flush_node_list(p.opt());
                n = 0;
                error();
            } else {
                *LLIST_link(cur_list.tail) = p
            }
            if n <= u16::MAX as i32 {
                MEM[cur_list.tail].b16.s0 = n as u16
            } else {
                t_eprint!("Discretionary list is too long");
                help!(
                    "Wow---I never thought anybody would tweak me here.",
                    "You can\'t seriously need such a huge discretionary list?"
                );
                error();
            }
            if n > 0 {
                cur_list.tail = q as usize;
            }
            SAVE_PTR -= 1;
            return;
        }
        _ => {}
    }
    SAVE_STACK[SAVE_PTR - 1].val += 1;
    new_save_level(GroupCode::Disc);
    scan_left_brace(input);
    push_nest();
    cur_list.mode = (true, ListMode::HMode);
    cur_list.aux.b32.s0 = 1000;
}
pub(crate) unsafe fn make_accent(input: &mut input_state_t) {
    let val = scan_char_num(input);
    let mut f = EQTB[CUR_FONT_LOC].val as usize;
    if let Some(mut p) = new_character(f, val as UTF16_code) {
        let mut lsb: Scaled = Scaled::ZERO;
        let mut rsb: Scaled = Scaled::ZERO;
        let x = Scaled(FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1);
        let s = FONT_INFO[(SLANT_CODE + PARAM_BASE[f]) as usize].b32.s1 as f64 / 65536.;
        let a = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[f] {
            let a = NativeWord::from(p).width();
            if a == Scaled::ZERO {
                let val = std::char::from_u32(val as u32).unwrap();
                let (lsb_, rsb_) = get_native_char_sidebearings(nf, val);
                lsb = lsb_;
                rsb = rsb_;
            }
            a
        } else {
            *FONT_CHARACTER_WIDTH(f, effective_char(true, f, Char(p).character()) as usize)
        };
        let (tok, cmd, chr) = do_assignments(input);
        let mut q = None;
        f = EQTB[CUR_FONT_LOC].val as usize;
        let val = if cmd == Cmd::Letter || cmd == Cmd::OtherChar || cmd == Cmd::CharGiven {
            q = new_character(f, chr as u16);
            chr
        } else if cmd == Cmd::CharNum {
            let val = scan_char_num(input);
            q = new_character(f, val as u16);
            val
        } else {
            back_input(input, tok);
            val
        };
        if let Some(q) = q {
            /*1160: */
            let h;
            let w;
            let t = FONT_INFO[(SLANT_CODE + PARAM_BASE[f]) as usize].b32.s1 as f64 / 65536.;
            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
                w = NativeWord::from(q).width();
                let val = std::char::from_u32(val as u32).unwrap();
                h = get_native_char_height_depth(f, val).0;
            } else {
                let i =
                    FONT_CHARACTER_INFO(f, effective_char(true, f, Char(q).character()) as usize);
                w = *FONT_CHARINFO_WIDTH(f, i);
                h = *FONT_CHARINFO_HEIGHT(f, i);
            }
            if h != x {
                let mut p_box = hpack(Some(p), Scaled::ZERO, PackMode::Additional);
                p_box.set_shift_amount(x - h);
                p = p_box.ptr();
            }
            let delta = match &FONT_LAYOUT_ENGINE[f] {
                Font::Native(_) if a == Scaled::ZERO => {
                    tex_round((w - lsb + rsb).0 as f64 / 2. + h.0 as f64 * t - x.0 as f64 * s)
                }
                _ => tex_round((w - a).0 as f64 / 2. + h.0 as f64 * t - x.0 as f64 * s),
            };
            let mut r = new_kern(delta);
            r.set_subtype(KernType::AccKern);
            *LLIST_link(cur_list.tail) = Some(r.ptr()).tex_int();
            *LLIST_link(r.ptr()) = Some(p).tex_int();
            let mut k = new_kern(-a - delta);
            cur_list.tail = k.ptr();
            k.set_subtype(KernType::AccKern);
            *LLIST_link(p) = Some(k.ptr()).tex_int();
            p = q;
        }
        *LLIST_link(cur_list.tail) = p as i32;
        cur_list.tail = p;
        cur_list.aux.b32.s0 = 1000;
    };
}
pub(crate) unsafe fn align_error(input: &mut input_state_t, tok: i32, cmd: Cmd, chr: i32) {
    if align_state.abs() > 2 {
        /*1163: */
        t_eprint!("Misplaced {}", CmdChr(cmd, chr));
        if tok == TAB_TOKEN + 38 {
            help!(
                "I can\'t figure out why you would want to use a tab mark",
                "here. If you just want an ampersand, the remedy is",
                "simple: Just type `I\\&\' now. But if some right brace",
                "up above has ended a previous alignment prematurely,",
                "you\'re probably due for more error messages, and you",
                "might try typing `S\' now just to see what is salvageable."
            );
        } else {
            help!(
                "I can\'t figure out why you would want to use a tab mark",
                "or \\cr or \\span just now. If something like a right brace",
                "up above has ended a previous alignment prematurely,",
                "you\'re probably due for more error messages, and you",
                "might try typing `S\' now just to see what is salvageable."
            );
        }
        error();
    } else {
        back_input(input, tok);
        let tok = if align_state < 0 {
            t_eprint!("Missing {{ inserted");
            align_state += 1;
            LEFT_BRACE_TOKEN + 123
        } else {
            t_eprint!("Missing }} inserted");
            align_state -= 1;
            RIGHT_BRACE_TOKEN + 125
        };
        help!(
            "I\'ve put in what seems to be necessary to fix",
            "the current column of the current alignment.",
            "Try to go on, since this might almost work."
        );
        ins_error(input, tok);
    };
}
pub(crate) unsafe fn no_align_error() {
    t_eprint!("Misplaced {}", Esc("noalign"));
    help!(
        "I expect to see \\noalign only after the \\cr of",
        "an alignment. Proceed, and I\'ll ignore this case."
    );
    error();
}
pub(crate) unsafe fn omit_error() {
    t_eprint!("Misplaced {}", Esc("omit"));
    help!(
        "I expect to see \\omit only after tab marks or the \\cr of",
        "an alignment. Proceed, and I\'ll ignore this case."
    );
    error();
}
pub(crate) unsafe fn do_endv(
    input: &mut input_state_t,
    group: GroupCode,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    input_stack: &[input_state_t],
) {
    let mut base_ptr = input_stack.len() - 1;
    while INPUT_STACK[base_ptr].index != Btl::VTemplate
        && INPUT_STACK[base_ptr].loc.is_none()
        && INPUT_STACK[base_ptr].state == InputState::TokenList
    {
        base_ptr -= 1
    }
    if INPUT_STACK[base_ptr].index != Btl::VTemplate
        || INPUT_STACK[base_ptr].loc.is_some()
        || INPUT_STACK[base_ptr].state != InputState::TokenList
    {
        fatal_error("(interwoven alignment preambles are not allowed)");
    }
    if group == GroupCode::Align {
        end_graf();
        if fin_col(input) {
            fin_row(input);
        }
    } else {
        off_save(input, group, tok, cmd, chr);
    };
}
pub(crate) unsafe fn cs_error() {
    t_eprint!("Extra {}", Esc("endcsname"));
    help!("I\'m ignoring this, since I wasn\'t doing a \\csname.");
    error();
}
pub(crate) unsafe fn push_math(c: GroupCode) {
    push_nest();
    cur_list.mode = (true, ListMode::MMode);
    cur_list.aux.b32.s1 = None.tex_int();
    new_save_level(c);
}
pub(crate) unsafe fn just_copy(mut popt: Option<usize>, mut h: usize, t: i32) {
    while let Some(p) = popt {
        let mut r = 0;
        let mut words = 1;

        let mut copy = true;
        let mut found = true;
        match CharOrText::from(p) {
            CharOrText::Char(_) => {
                r = get_avail();
            }
            CharOrText::Text(p) => match p {
                TxtNode::List(p) => {
                    r = get_node(BOX_NODE_SIZE);
                    *SYNCTEX_tag(r, BOX_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE);
                    *SYNCTEX_line(r, BOX_NODE_SIZE) = *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE);
                    MEM[r + 6] = MEM[p.ptr() + 6];
                    MEM[r + 5] = MEM[p.ptr() + 5];
                    words = 5;
                    List::from(r).set_list_ptr(None.tex_int());
                }
                TxtNode::Rule(_) => {
                    r = get_node(RULE_NODE_SIZE);
                    words = RULE_NODE_SIZE;
                }
                TxtNode::Ligature(l) => {
                    r = get_avail();
                    MEM[r as usize] = MEM[(l.ptr() + 1) as usize];
                    copy = false;
                }
                TxtNode::Kern(_) | TxtNode::Math(_) => {
                    words = MEDIUM_NODE_SIZE;
                    r = get_node(words as i32);
                }
                TxtNode::Glue(p) => {
                    r = get_node(MEDIUM_NODE_SIZE);
                    let mut r_glue = Glue(r);
                    GlueSpec(p.glue_ptr() as usize).rc_inc();
                    *SYNCTEX_tag(r, MEDIUM_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), MEDIUM_NODE_SIZE);
                    *SYNCTEX_line(r, MEDIUM_NODE_SIZE) = *SYNCTEX_line(p.ptr(), MEDIUM_NODE_SIZE);
                    r_glue
                        .set_glue_ptr(p.glue_ptr())
                        .set_leader_ptr(None.tex_int());
                }
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::Open(_p) => {
                        r = get_node(OPEN_NODE_SIZE);
                        words = OPEN_NODE_SIZE;
                    }
                    WhatsIt::Write(p) => {
                        r = get_node(WRITE_NODE_SIZE);
                        *TOKEN_LIST_ref_count(p.tokens() as usize) += 1;
                        words = WRITE_NODE_SIZE;
                    }
                    WhatsIt::Special(p) => {
                        r = get_node(WRITE_NODE_SIZE);
                        *TOKEN_LIST_ref_count(p.tokens() as usize) += 1;
                        words = WRITE_NODE_SIZE;
                    }
                    WhatsIt::Close(_) | WhatsIt::Language(_) => {
                        r = get_node(SMALL_NODE_SIZE);
                        words = SMALL_NODE_SIZE;
                    }
                    WhatsIt::NativeWord(p) => {
                        words = p.size() as i32;
                        let mut r_nw = NativeWord::from(get_node(words as i32));
                        r = r_nw.ptr();

                        MEM[r..r + (words as usize)]
                            .copy_from_slice(&MEM[p.ptr()..p.ptr() + (words as usize)]);
                        words = 0;

                        r_nw.set_glyph_info_ptr(core::ptr::null_mut());
                        r_nw.set_glyph_count(0);
                        copy_native_glyph_info(&p, &mut r_nw);
                    }
                    WhatsIt::Glyph(_) => {
                        r = get_node(GLYPH_NODE_SIZE);
                        words = GLYPH_NODE_SIZE;
                    }
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        words = Picture::total_size(p.path_len()) as i32;
                        r = get_node(words as i32);
                    }
                    WhatsIt::PdfSavePos(_) => r = get_node(SMALL_NODE_SIZE),
                    //_ => confusion("ext2"),
                },
                _ => {
                    copy = false;
                    found = false;
                }
            },
        }
        if copy {
            MEM[r..r + (words as usize)].copy_from_slice(&MEM[p..p + (words as usize)]);
        }

        if found {
            *LLIST_link(h) = Some(r).tex_int();
            h = r;
        }

        // not_found:
        popt = llist_link(p);
    }
    *LLIST_link(h) = t;
}
pub(crate) unsafe fn just_reverse(p: usize) {
    let mut q;
    let mut m = MIN_HALFWORD;
    let mut n = MIN_HALFWORD;
    if let Some(th) = llist_link(TEMP_HEAD) {
        q = llist_link(p);
        *LLIST_link(p) = None.tex_int();
        flush_node_list(Some(th));
    } else {
        just_copy(llist_link(p), TEMP_HEAD, None.tex_int());
        q = llist_link(TEMP_HEAD);
    }
    let mut t = Edge(new_edge(cur_dir, Scaled::ZERO));
    let mut l = t.ptr();
    cur_dir = !cur_dir;
    while let Some(p) = q {
        match CharOrText::from(p) {
            CharOrText::Char(_) => loop {
                let p = q.unwrap();
                q = llist_link(p);
                *LLIST_link(p) = Some(l).tex_int();
                l = p;
                if !is_char_node(q) {
                    break;
                }
            },
            CharOrText::Text(nd) => {
                q = llist_link(p);
                if let TxtNode::Math(mut p) = nd {
                    /*1527: */
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
                                    MathType::Eq(BE::End, mode) => MathType::Eq(BE::Begin, mode),
                                    _ => unreachable!(),
                                });
                            } else if m > MIN_HALFWORD {
                                m -= 1;
                                set_NODE_type(p.ptr(), TextNode::Kern);
                            } else {
                                t.set_width(p.width());
                                *LLIST_link(t.ptr()) = q.tex_int();
                                p.free();
                                break;
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
                            m += 1
                        }
                    }
                }
                *LLIST_link(p) = Some(l).tex_int();
                l = p
            }
        }
    }
    *LLIST_link(TEMP_HEAD) = Some(l).tex_int();
}
pub(crate) unsafe fn get_r_token(input: &mut input_state_t) -> (i32, Cmd, i32, i32) {
    let mut tok;
    let mut cmd;
    let mut chr;
    let mut cs;
    loop {
        loop {
            let (tok_, cmd_, chr_, cs_) = get_token(input);
            tok = tok_;
            cmd = cmd_;
            chr = chr_;
            cs = cs_;
            if tok != SPACE_TOKEN {
                break;
            }
        }
        if !(cs == 0
            || cs > EQTB_TOP as i32
            || cs > FROZEN_CONTROL_SEQUENCE as i32 && cs <= EQTB_SIZE as i32)
        {
            break;
        }
        t_eprint!("Missing control sequence inserted");
        help!(
            "Please don\'t say `\\def cs{...}\', say `\\def\\cs{...}\'.",
            "I\'ve inserted an inaccessible control sequence so that your",
            "definition will be completed without mixing me up too badly.",
            "You can recover graciously from this error, if you\'re",
            "careful; see exercise 27.2 in The TeXbook."
        );
        if cs == 0 {
            back_input(input, tok);
        }
        tok = CS_TOKEN_FLAG + FROZEN_PROTECTION as i32;
        ins_error(input, tok);
    }
    (tok, cmd, chr, cs)
}
pub(crate) unsafe fn trap_zero_glue(val: GlueSpec) -> GlueSpec {
    if val.size() == Scaled::ZERO && val.stretch() == Scaled::ZERO && val.shrink() == Scaled::ZERO {
        let zero = ZERO_GLUE;
        zero.rc_inc();
        delete_glue_ref(val.ptr());
        zero
    } else {
        val
    }
}
pub(crate) unsafe fn do_register_command(
    input: &mut input_state_t,
    ocmd: Cmd,
    mut ochr: i32,
    a: i16,
) {
    let mut l: i32 = None.tex_int();
    let mut p = ValLevel::Int;
    let q = ocmd;
    let mut e = false;

    let mut flag = true;
    if q != Cmd::Register {
        let (_, cmd, chr, _) = get_x_token(input);
        ochr = chr;
        match cmd {
            Cmd::AssignInt | Cmd::AssignDimen | Cmd::AssignGlue | Cmd::AssignMuGlue => {
                l = chr;
                p = match cmd {
                    Cmd::AssignInt => ValLevel::Int,
                    Cmd::AssignDimen => ValLevel::Dimen,
                    Cmd::AssignGlue => ValLevel::Glue,
                    Cmd::AssignMuGlue => ValLevel::Mu,
                    _ => unreachable!(),
                };
                flag = false;
            }
            _ => {
                if cmd != Cmd::Register {
                    t_eprint!(
                        "You can\'t use `{}\' after {}",
                        CmdChr(cmd, chr),
                        CmdChr(q, 0)
                    );
                    help!("I\'m forgetting what you said and not changing anything.");
                    error();
                    return;
                }
            }
        }
    }

    if flag {
        if ochr < 0 || ochr > LO_MEM_STAT_MAX {
            l = ochr;
            p = ValLevel::from((MEM[l as usize].b16.s1 as i32 / 64) as u8);
            e = true
        } else {
            p = ValLevel::from(ochr as u8);
            let val = scan_register_num(input);
            if val > 255 {
                find_sa_element(p, val, true);
                l = cur_ptr.tex_int();
                e = true
            } else {
                match p {
                    ValLevel::Int => l = val + COUNT_BASE as i32,
                    ValLevel::Dimen => l = val + SCALED_BASE as i32,
                    ValLevel::Glue => l = val + SKIP_BASE as i32,
                    ValLevel::Mu => l = val + MU_SKIP_BASE as i32,
                    _ => {}
                }
            }
        }
    }

    let mut w = 0;
    let mut s = None.tex_int();
    if p < ValLevel::Glue {
        if e {
            w = MEM[(l + 2) as usize].b32.s1;
        } else {
            w = EQTB[l as usize].val;
        }
    } else if e {
        s = MEM[(l + 1) as usize].b32.s1
    } else {
        s = EQTB[l as usize].val
        /*:1272*/
    } /*1275:*/
    if q == Cmd::Register {
        scan_optional_equals(input);
    } else {
        scan_keyword(input, "by");
    }
    arith_error = false;
    let val = if q < Cmd::Multiply {
        /*1273:*/
        match p {
            ValLevel::Int | ValLevel::Dimen => {
                let val = if p == ValLevel::Int {
                    scan_int(input)
                } else {
                    scan_dimen(input, false, false, None).0
                };
                if q == Cmd::Advance {
                    val + w
                } else {
                    val
                }
            }
            _ => {
                let val = scan_glue(input, p);
                if q == Cmd::Advance {
                    /*1274:*/
                    let mut q = new_spec(&val);
                    let r = GlueSpec(s as usize);
                    delete_glue_ref(val.ptr());
                    q.set_size(q.size() + r.size());
                    if q.stretch() == Scaled::ZERO {
                        q.set_stretch_order(GlueOrder::Normal);
                    }
                    if q.stretch_order() == r.stretch_order() {
                        q.set_stretch(q.stretch() + r.stretch());
                    } else if q.stretch_order() < r.stretch_order() && r.stretch() != Scaled::ZERO {
                        q.set_stretch(r.stretch())
                            .set_stretch_order(r.stretch_order());
                    }
                    if q.shrink() == Scaled::ZERO {
                        q.set_shrink_order(GlueOrder::Normal);
                    }
                    if q.shrink_order() == r.shrink_order() {
                        q.set_shrink(q.shrink() + r.shrink());
                    } else if q.shrink_order() < r.shrink_order() && r.shrink() != Scaled::ZERO {
                        q.set_shrink(r.shrink()).set_shrink_order(r.shrink_order());
                    }
                    q.ptr() as i32
                } else {
                    val.ptr() as i32
                }
            }
        }
    } else {
        let val = scan_int(input);
        match p {
            ValLevel::Int | ValLevel::Dimen => {
                if q == Cmd::Multiply {
                    if p == ValLevel::Int {
                        mult_and_add(w, val, 0, TEX_INFINITY)
                    } else {
                        Scaled(w).mul_add(val, Scaled::ZERO).0
                    }
                } else {
                    (x_over_n(Scaled(w), val).0).0
                }
            }
            _ => {
                let s_spec = GlueSpec(s as usize);
                let mut r = new_spec(&s_spec);
                if q == Cmd::Multiply {
                    r.set_size(s_spec.size().mul_add(val, Scaled::ZERO))
                        .set_stretch(s_spec.stretch().mul_add(val, Scaled::ZERO))
                        .set_shrink(s_spec.shrink().mul_add(val, Scaled::ZERO));
                } else {
                    r.set_size(x_over_n(s_spec.size(), val).0)
                        .set_stretch(x_over_n(s_spec.stretch(), val).0)
                        .set_shrink(x_over_n(s_spec.shrink(), val).0);
                }
                r.ptr() as i32
            }
        }
    };
    if arith_error {
        t_eprint!("Arithmetic overflow");
        help!(
            "I can\'t carry out that multiplication or division,",
            "since the result is out of range."
        );
        if p >= ValLevel::Glue {
            delete_glue_ref(val as usize);
        }
        error();
        return;
    }
    if p < ValLevel::Glue {
        if e {
            if a >= 4 {
                gsa_w_def(l as usize, val);
            } else {
                sa_w_def(l as usize, val);
            }
        } else if a >= 4 {
            geq_word_define(l as usize, val);
        } else {
            eq_word_define(l as usize, val);
        }
    } else {
        let val = trap_zero_glue(GlueSpec(val as usize));
        if e {
            if a >= 4 {
                gsa_def(l as usize, Some(val.ptr()));
            } else {
                sa_def(l as usize, Some(val.ptr()));
            }
        } else if a >= 4 {
            geq_define(l as usize, Cmd::GlueRef, Some(val.ptr()));
        } else {
            eq_define(l as usize, Cmd::GlueRef, Some(val.ptr()));
        }
    };
}
pub(crate) unsafe fn alter_aux(input: &mut input_state_t, cmd: Cmd, chr: i32) {
    if chr != cur_list.mode.1 as i32 {
        report_illegal_case(cmd, chr);
    } else {
        let c = chr;
        scan_optional_equals(input);
        if c == ListMode::VMode as i32 {
            let val = scan_dimen(input, false, false, None);
            cur_list.aux.b32.s1 = val.0;
        } else {
            let val = scan_int(input);
            if val <= 0 || val > 32767 {
                t_eprint!("Bad space factor");
                help!("I allow only values in the range 1..32767 here.");
                int_error(val);
            } else {
                cur_list.aux.b32.s0 = val;
            }
        }
    };
}
pub(crate) unsafe fn alter_prev_graf(input: &mut input_state_t) {
    NEST[NEST_PTR] = cur_list;
    let mut p = NEST_PTR;
    while NEST[p as usize].mode.1 != ListMode::VMode {
        p -= 1;
    }
    scan_optional_equals(input);
    let val = scan_int(input);
    if val < 0 {
        t_eprint!("Bad {}", Esc("prevgraf"));
        help!("I allow only nonnegative values here.");
        int_error(val);
    } else {
        NEST[p as usize].prev_graf = val;
        cur_list = NEST[NEST_PTR]
    };
}
pub(crate) unsafe fn alter_page_so_far(input: &mut input_state_t, chr: i32) {
    let c = chr as u8 as usize;
    scan_optional_equals(input);
    let val = scan_dimen(input, false, false, None);
    page_so_far[c] = val;
}
pub(crate) unsafe fn alter_integer(input: &mut input_state_t, chr: i32) {
    let c = chr as i16;
    scan_optional_equals(input);
    let val = scan_int(input);
    match c {
        0 => dead_cycles = val,
        2 => {
            if InteractionMode::n(val as u8).is_none() {
                t_eprint!("Bad interaction mode");
                help!(
                    "Modes are 0=batch, 1=nonstop, 2=scroll, and",
                    "3=errorstop. Proceed, and I\'ll ignore this case."
                );
                int_error(val);
            } else {
                new_interaction(val);
            }
        }
        _ => insert_penalties = val,
    }
}
pub(crate) unsafe fn alter_box_dimen(input: &mut input_state_t, chr: i32) {
    let c = chr as usize;
    let val = scan_register_num(input);
    let b = if val < 256 {
        get_box_reg(val as usize)
    } else {
        find_sa_element(ValLevel::Ident, val, false);
        cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
    };
    scan_optional_equals(input);
    let val = scan_dimen(input, false, false, None);
    if let Some(b) = b {
        MEM[b + c].b32.s1 = val.0;
    };
}
pub(crate) unsafe fn new_font(input: &mut input_state_t, a: i16) {
    if job_name == 0 {
        open_log_file();
    }
    let u = get_r_token(input).3 as usize;
    let t = if u >= HASH_BASE {
        yhash[u - hash_offset].s1
    } else if u >= SINGLE_BASE {
        if u == NULL_CS {
            maketexstring("FONT")
        } else {
            (u - SINGLE_BASE) as i32
        }
    } else {
        let s = format!("FONT{}", PoolString::from((u - 1) as i32));
        PoolString::add_new_from_str(&s)
    };
    if a >= 4 {
        geq_define(u, Cmd::SetFont, Some(FONT_BASE));
    } else {
        eq_define(u, Cmd::SetFont, Some(FONT_BASE));
    }
    scan_optional_equals(input);
    let (file, quoted_filename, file_name_quote_char) = scan_file_name(input);
    name_in_progress = true;
    let mut s;
    if scan_keyword(input, "at") {
        /*1294: */
        s = scan_dimen(input, false, false, None); /*:1293 */
        /*:79 */
        if s <= Scaled::ZERO || s >= Scaled::from(2048) {
            t_eprint!("Improper `at\' size ({}pt), replaced by 10pt", s);
            help!(
                "I can only handle fonts at positive sizes that are",
                "less than 2048pt, so I\'ve changed what you said to 10pt."
            );
            error();
            s = Scaled::from(10);
        }
    } else if scan_keyword(input, "scaled") {
        let val = scan_int(input);
        s = Scaled(-val);
        if val <= 0 || val > 32768 {
            t_eprint!("Illegal magnification has been changed to 1000");
            help!("The magnification ratio must be between 1 and 32768.");
            int_error(val);
            s = Scaled(-1000);
        }
    } else {
        s = Scaled(-1000);
    }
    name_in_progress = false;

    for f in (FONT_BASE + 1)..FONT_PTR + 1 {
        // TODO: check
        let font_name = PoolString::from(FONT_NAME[f]);
        let font_area = PoolString::from(FONT_AREA[f]);
        let area = PoolString::from(file.area);
        let name = PoolString::from(file.name);
        if font_name == name
            && ((area.len() == 0 && matches!(&FONT_LAYOUT_ENGINE[f], Font::Native(_)))
                || font_area == area)
        {
            if s > Scaled::ZERO {
                if s == FONT_SIZE[f] {
                    return common_ending(a, u, f, t);
                }
            } else if FONT_SIZE[f] == xn_over_d(FONT_DSIZE[f], -s, 1000).0 {
                return common_ending(a, u, f, t);
            }
        }
        append_str(file.area);
        append_str(file.name);
        append_str(file.ext);
        if PoolString::from(FONT_NAME[f]) == PoolString::from(make_string()) {
            PoolString::flush();
            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
                if s > Scaled::ZERO {
                    if s == FONT_SIZE[f] {
                        return common_ending(a, u, f, t);
                    }
                } else if FONT_SIZE[f] == xn_over_d(FONT_DSIZE[f], -s, 1000).0 {
                    return common_ending(a, u, f, t);
                }
            }
        } else {
            PoolString::flush();
        }
    }

    unsafe fn common_ending(a: i16, u: usize, f: usize, t: i32) {
        if a >= 4 {
            geq_define(u, Cmd::SetFont, Some(f));
        } else {
            eq_define(u, Cmd::SetFont, Some(f));
        }
        EQTB[FROZEN_NULL_FONT + f] = EQTB[u];
        yhash[FROZEN_NULL_FONT + f - hash_offset].s1 = t;
    }

    let f = crate::tfm::read_font_info(u as i32, &file, s, quoted_filename, file_name_quote_char)
        .map(crate::tfm::good_tfm)
        .unwrap_or_else(|e| {
            crate::tfm::bad_tfm(e, u as i32, &file, s, file_name_quote_char);
            FONT_BASE
        });
    common_ending(a, u, f, t)
}
pub(crate) unsafe fn new_interaction(chr: i32) {
    print_ln();
    interaction = InteractionMode::n(chr as u8).unwrap();
    selector = match (interaction, log_opened) {
        (InteractionMode::Batch, false) => Selector::NO_PRINT,
        (InteractionMode::Batch, true) => Selector::LOG_ONLY,
        (_, false) => Selector::TERM_ONLY,
        (_, true) => Selector::TERM_AND_LOG,
    };
}
pub(crate) unsafe fn issue_message(input: &mut input_state_t, chr: i32, cs: i32) {
    let c = chr as u8;
    *LLIST_link(GARBAGE) = scan_toks(input, cs, false, true) as i32;
    let s = format!("{}", TokenNode(Some(def_ref)));
    flush_list(Some(def_ref));
    if c == 0 {
        /*1315: */
        if term_offset + (s.len() as i32) > max_print_line - 2 {
            print_ln();
        } else if term_offset > 0 || file_offset > 0 {
            t_print!(" ");
        }
        t_print!("{}", s);
        rust_stdout.as_mut().unwrap().flush().unwrap();
    } else {
        t_eprint!("{}", s);
        if LOCAL(Local::err_help).opt().is_some() {
            use_err_help = true;
        } else if long_help_seen {
            help!("(That was another \\errmessage.)");
        } else {
            if interaction != InteractionMode::ErrorStop {
                long_help_seen = true;
            }
            help!(
                "This error message was generated by an \\errmessage",
                "command, so I can\'t give any explicit help.",
                "Pretend that you\'re Hercule Poirot: Examine all clues,",
                "and deduce the truth by order and method."
            );
        }
        error();
        use_err_help = false
    }
}
pub(crate) unsafe fn shift_case(input: &mut input_state_t, chr: i32, cs: i32) {
    let b = chr;
    let _p = scan_toks(input, cs, false, false);
    let mut popt = llist_link(def_ref);
    while let Some(p) = popt {
        let t = MEM[p].b32.s0;
        if t < CS_TOKEN_FLAG + SINGLE_BASE as i32 {
            let c = t % MAX_CHAR_VAL;
            if EQTB[(b + c) as usize].val != 0 {
                MEM[p].b32.s0 = t - c + EQTB[(b + c) as usize].val
            }
        }
        popt = llist_link(p);
    }
    begin_token_list(input, llist_link(def_ref), Btl::BackedUp);
    *LLIST_link(def_ref) = avail.tex_int();
    avail = Some(def_ref);
}
pub(crate) unsafe fn show_whatever(input: &mut input_state_t, chr: i32, cs: i32) {
    match chr {
        SHOW_LISTS => {
            diagnostic(true, || show_activities());
        }
        SHOW_BOX_CODE => {
            let val = scan_register_num(input);
            let p = if val < 256 {
                get_box_reg(val as usize)
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            diagnostic(true, || {
                t_print_nl!("> \\box{}=", val);
                if p.is_none() {
                    t_print!("void");
                } else {
                    show_box(p);
                }
            });
        }
        SHOW_CODE => {
            let (_, cmd, chr, cs) = get_token(input);
            t_print_nl!("> ");
            if cs != 0 {
                t_print!("{:#}=", Cs(cs));
            }
            print_meaning(cmd, chr);
            return common_ending();
        }
        SHOW_GROUPS => {
            diagnostic(true, || show_save_groups(cur_group, cur_level));
        }
        SHOW_IFS => {
            diagnostic(true, || {
                t_print_nl!("");
                print_ln();
                if let Some(cp) = cond_ptr {
                    let mut p = cp;
                    let mut n = 0;
                    loop {
                        n += 1;
                        if let Some(next) = llist_link(p) {
                            p = next;
                        } else {
                            break;
                        }
                    }
                    let mut p = cp;
                    let mut t = cur_if;
                    let mut l = if_line;
                    let mut m = if_limit;
                    loop {
                        t_print_nl!("### level {}: {}", n, CmdChr(Cmd::IfTest, t as i32));
                        if m == FiOrElseCode::Fi {
                            t_print!("{}", Esc("else"));
                        }
                        if l != 0 {
                            t_print!(" entered on line {}", l);
                        }
                        n -= 1;
                        t = MEM[p].b16.s0 as i16;
                        l = MEM[p + 1].b32.s1;
                        m = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
                        if let Some(next) = llist_link(p) {
                            p = next;
                        } else {
                            break;
                        }
                    }
                } else {
                    t_print_nl!("### no active conditionals");
                }
            });
        }
        SHOW_THE_CODE | SHOW_TOKENS => {
            let _p = the_toks(input, chr, cs) as i32;
            t_print_nl!("> ");
            token_show(Some(TEMP_HEAD));
            flush_list(llist_link(TEMP_HEAD));
            return common_ending();
        }
        _ => unreachable!(),
    }

    t_eprint!("OK");
    if selector == Selector::TERM_AND_LOG && get_int_par(IntPar::tracing_online) <= 0 {
        selector = Selector::TERM_ONLY;
        t_print!(" (see the transcript file)");
        selector = Selector::TERM_AND_LOG;
    }

    unsafe fn common_ending() {
        if interaction != InteractionMode::ErrorStop {
            help!();
            error_count -= 1;
        } else if get_int_par(IntPar::tracing_online) > 0 {
            help!(
                "This isn\'t an error message; I\'m just \\showing something.",
                "Type `I\\show...\' to show more (e.g., \\show\\cs,",
                "\\showthe\\count10, \\showbox255, \\showlists)."
            );
        } else {
            help!(
                "This isn\'t an error message; I\'m just \\showing something.",
                "Type `I\\show...\' to show more (e.g., \\show\\cs,",
                "\\showthe\\count10, \\showbox255, \\showlists).",
                "And type `I\\tracingonline=1\\show...\' to show boxes and",
                "lists on your terminal as well as in the transcript file."
            );
        }
        error();
    }
    common_ending()
}
pub(crate) unsafe fn do_extension(
    input: &mut input_state_t,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    cs: i32,
) {
    let mut j: i32 = 0;
    match chr as u16 {
        0 => {
            // OpenFile
            let mut o = OpenFile::new_node();
            *LLIST_link(cur_list.tail) = Some(o.ptr()).tex_int();
            cur_list.tail = o.ptr();
            let val = scan_four_bit_int(input);
            o.set_id(val);
            scan_optional_equals(input);
            let (file, ..) = scan_file_name(input);
            o.set_name(file.name).set_area(file.area).set_ext(file.ext);
        }
        1 => {
            // WriteFile
            let k = cs;
            let mut w = WriteFile::new_node();
            *LLIST_link(cur_list.tail) = Some(w.ptr()).tex_int();
            cur_list.tail = w.ptr();
            let val = scan_int(input);
            let val = if val < 0 {
                17
            } else if val > 15 && val != 18 {
                16
            } else {
                val
            };
            w.set_id(val);
            scan_toks(input, k, false, false);
            w.set_tokens(def_ref as i32);
        }
        2 => {
            // CloseFile
            let mut c = CloseFile::new_node();
            *LLIST_link(cur_list.tail) = Some(c.ptr()).tex_int();
            cur_list.tail = c.ptr();
            let val = scan_int(input);
            let val = if val < 0 {
                17
            } else if val > 15 && val != 18 {
                16
            } else {
                val
            };
            c.set_id(val);
            MEM[cur_list.tail + 1].b32.s1 = None.tex_int()
        }
        3 => {
            let mut s = Special::new_node();
            *LLIST_link(cur_list.tail) = Some(s.ptr()).tex_int();
            cur_list.tail = s.ptr();
            MEM[cur_list.tail + 1].b32.s0 = None.tex_int();
            scan_toks(input, cs, false, true);
            s.set_tokens(def_ref as i32);
        }
        IMMEDIATE_CODE => {
            let (tok, cmd, chr, cs) = get_x_token(input);
            if cmd == Cmd::Extension && chr <= CloseFile::WHATS_IT as i32 {
                let p = cur_list.tail;
                do_extension(input, tok, cmd, chr, cs);
                out_what(input, &WhatsIt::from(cur_list.tail));
                flush_node_list(Some(cur_list.tail));
                cur_list.tail = p;
                *LLIST_link(p) = None.tex_int();
            } else {
                back_input(input, tok);
            }
        }
        SET_LANGUAGE_CODE => {
            if cur_list.mode.1 != ListMode::HMode {
                report_illegal_case(cmd, chr);
            } else {
                let mut l = Language::new_node();
                *LLIST_link(cur_list.tail) = Some(l.ptr()).tex_int();
                cur_list.tail = l.ptr();
                let val = scan_int(input);
                cur_list.aux.b32.s1 = if val <= 0 || val > 255 { 0 } else { val };
                l.set_lang(cur_list.aux.b32.s1)
                    .set_lhm(norm_min(get_int_par(IntPar::left_hyphen_min)) as u16)
                    .set_rhm(norm_min(get_int_par(IntPar::right_hyphen_min)) as u16);
            }
        }
        PIC_FILE_CODE => {
            if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case(cmd, chr);
            } else {
                load_picture(input, false);
            }
        }
        PDF_FILE_CODE => {
            if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case(cmd, chr);
            } else {
                load_picture(input, true);
            }
        }
        GLYPH_CODE => {
            if cur_list.mode.1 == ListMode::VMode {
                back_input(input, tok);
                new_graf(input, true);
            } else if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case(cmd, chr);
            } else if let Font::Native(_) = &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize] {
                let mut g = Glyph::new_node();
                *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
                cur_list.tail = g.ptr();
                let val = scan_int(input);
                let val = if val < 0 || val > 65535 {
                    t_eprint!("Bad glyph number");
                    help!(
                        "A glyph number must be between 0 and 65535.",
                        "I changed this one to zero."
                    );
                    int_error(val);
                    0
                } else {
                    val
                };
                g.set_font(EQTB[CUR_FONT_LOC].val as u16)
                    .set_glyph(val as u16);
                g.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
            } else {
                not_native_font_error(
                    Cmd::Extension,
                    GLYPH_CODE as i32,
                    EQTB[CUR_FONT_LOC].val as usize,
                );
            }
        }
        XETEX_INPUT_ENCODING_EXTENSION_CODE => {
            let name = scan_file_name(input).0.to_string();
            let i = get_encoding_mode_and_info(&name, &mut j);
            if i == UnicodeMode::Auto {
                t_eprint!("Encoding mode `auto\' is not valid for \\XeTeXinputencoding");
                help!(
                    "You can\'t use `auto\' encoding here, only for \\XeTeXdefaultencoding.",
                    "I\'ll ignore this and leave the current encoding unchanged."
                );
                error();
            } else {
                set_input_file_encoding(INPUT_FILE[IN_OPEN].as_mut().unwrap(), i, j);
            }
        }
        XETEX_DEFAULT_ENCODING_EXTENSION_CODE => {
            let name = scan_file_name(input).0.to_string();
            let i = get_encoding_mode_and_info(&name, &mut j);
            set_int_par(IntPar::xetex_default_input_mode, i as i32);
            set_int_par(IntPar::xetex_default_input_encoding, j);
        }
        XETEX_LINEBREAK_LOCALE_EXTENSION_CODE => {
            let (file, ..) = scan_file_name(input);
            if PoolString::from(file.name).len() == 0 {
                set_int_par(IntPar::xetex_linebreak_locale, 0);
            } else {
                set_int_par(IntPar::xetex_linebreak_locale, file.name);
            }
        }
        6 => {
            let p = PdfSavePos::new_node();
            *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
            cur_list.tail = p.ptr();
        }
        _ => confusion("ext1"),
    };
}
pub(crate) unsafe fn fix_language() {
    let lang = get_int_par(IntPar::language);
    let l = if lang <= 0 || lang > 255 { 0 } else { lang };
    if l != cur_list.aux.b32.s1 {
        let mut lang = Language::new_node();
        *LLIST_link(cur_list.tail) = Some(lang.ptr()).tex_int();
        cur_list.tail = lang.ptr();
        lang.set_lang(l);
        cur_list.aux.b32.s1 = l;
        lang.set_lhm(norm_min(get_int_par(IntPar::left_hyphen_min)) as u16)
            .set_rhm(norm_min(get_int_par(IntPar::right_hyphen_min)) as u16);
    };
}
pub(crate) unsafe fn insert_src_special() {
    if SOURCE_FILENAME_STACK[IN_OPEN] > 0 && is_new_source(SOURCE_FILENAME_STACK[IN_OPEN], line) {
        let toklist = get_avail();
        let p = toklist;
        MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_SPECIAL as i32;
        *LLIST_link(p) = Some(get_avail()).tex_int();
        let p = *LLIST_link(p) as usize;
        MEM[p].b32.s0 = LEFT_BRACE_TOKEN + '{' as i32;
        let q =
            str_toks_cat_utf8(&make_src_special(SOURCE_FILENAME_STACK[IN_OPEN], line), 0) as usize;
        *LLIST_link(p) = *LLIST_link(TEMP_HEAD);
        let p = q;
        *LLIST_link(p) = Some(get_avail()).tex_int();
        let p = *LLIST_link(p) as usize;
        MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
        begin_token_list(&mut cur_input, Some(toklist), Btl::Inserted);
        remember_source_info(SOURCE_FILENAME_STACK[IN_OPEN], line);
    };
}
pub(crate) unsafe fn append_src_special() {
    if SOURCE_FILENAME_STACK[IN_OPEN] > 0 && is_new_source(SOURCE_FILENAME_STACK[IN_OPEN], line) {
        let s = Special::new_node();
        *LLIST_link(cur_list.tail) = Some(s.ptr()).tex_int();
        cur_list.tail = s.ptr();
        MEM[cur_list.tail + 1].b32.s0 = 0;
        def_ref = get_avail();
        MEM[def_ref].b32.s0 = None.tex_int();
        str_toks_cat_utf8(&make_src_special(SOURCE_FILENAME_STACK[IN_OPEN], line), 0);
        *LLIST_link(def_ref) = *LLIST_link(TEMP_HEAD);
        MEM[cur_list.tail + 1].b32.s1 = def_ref as i32;
        remember_source_info(SOURCE_FILENAME_STACK[IN_OPEN], line);
    };
}
pub(crate) unsafe fn handle_right_brace(input: &mut input_state_t, group: GroupCode, tok: i32) {
    match group {
        GroupCode::Simple => unsave(input),
        GroupCode::BottomLevel => {
            t_eprint!("Too many }}\'s");
            help!(
                "You\'ve closed more groups than you opened.",
                "Such booboos are generally harmless, so keep going."
            );
            error();
        }
        GroupCode::SemiSimple | GroupCode::MathShift | GroupCode::MathLeft => {
            extra_right_brace(group)
        }
        GroupCode::HBox => package(input, 0),
        GroupCode::AdjustedHBox => {
            adjust_tail = Some(ADJUST_HEAD);
            pre_adjust_tail = Some(PRE_ADJUST_HEAD);
            package(input, 0);
        }
        GroupCode::VBox => {
            end_graf();
            package(input, 0);
        }
        GroupCode::VTop => {
            end_graf();
            package(input, BoxCode::VTop as i16);
        }
        GroupCode::Insert => {
            end_graf();
            let q = get_glue_par(GluePar::split_top_skip);
            q.rc_inc();
            let d = get_dimen_par(DimenPar::split_max_depth);
            let f = get_int_par(IntPar::floating_penalty);
            unsave(input);
            SAVE_PTR -= 2;
            let p = vpackage(
                MEM[cur_list.head].b32.s1.opt(),
                Scaled::ZERO,
                PackMode::Additional,
                Scaled::MAX_HALFWORD,
            );
            pop_nest();
            if SAVE_STACK[SAVE_PTR + 0].val < 255 {
                let mut i = Insertion::new_node();
                *LLIST_link(cur_list.tail) = Some(i.ptr()).tex_int();
                cur_list.tail = i.ptr();
                i.set_box_reg(SAVE_STACK[SAVE_PTR + 0].val as u16)
                    .set_height(p.height() + p.depth())
                    .set_ins_ptr(p.list_ptr())
                    .set_split_top_ptr(Some(q.ptr()).tex_int())
                    .set_depth(d)
                    .set_float_cost(f);
            } else {
                let a = Adjust::new_node();
                *LLIST_link(cur_list.tail) = Some(a.ptr()).tex_int();
                cur_list.tail = a.ptr();
                MEM[cur_list.tail].b16.s0 = SAVE_STACK[SAVE_PTR + 1].val as u16;
                MEM[cur_list.tail + 1].b32.s1 = p.list_ptr();
                delete_glue_ref(q.ptr());
            }
            free_node(p.ptr(), BOX_NODE_SIZE);
            if NEST_PTR == 0 {
                build_page(input);
            }
        }
        GroupCode::Output => {
            /*1062:*/
            if input.loc.is_some() || input.index != Btl::OutputText && input.index != Btl::BackedUp
            {
                t_eprint!("Unbalanced output routine");
                help!(
                    "Your sneaky output routine has problematic {\'s and/or }\'s.",
                    "I can\'t handle that very well; good luck."
                );
                error();
                loop {
                    let _ = get_token(input);
                    if input.loc.is_none() {
                        break;
                    }
                }
            }

            end_token_list(input);
            end_graf();
            unsave(input);
            output_active = false;
            insert_penalties = 0;

            if get_box_reg(255).is_some() {
                t_eprint!("Output routine didn\'t use all of {}{}", Esc("box"), 255);
                help!(
                    "Your \\output commands should empty \\box255,",
                    "e.g., by saying `\\shipout\\box255\'.",
                    "Proceed; I\'ll discard its present contents."
                );
                box_error(255);
            }
            if cur_list.tail != cur_list.head {
                *LLIST_link(page_tail) = *LLIST_link(cur_list.head);
                page_tail = cur_list.tail;
            }
            if let Some(ph) = llist_link(PAGE_HEAD) {
                if llist_link(CONTRIB_HEAD).is_none() {
                    NEST[0].tail = page_tail;
                }
                *LLIST_link(page_tail) = *LLIST_link(CONTRIB_HEAD);
                *LLIST_link(CONTRIB_HEAD) = Some(ph).tex_int();
                *LLIST_link(PAGE_HEAD) = None.tex_int();
                page_tail = PAGE_HEAD;
            }
            flush_node_list(disc_ptr[LAST_BOX_CODE as usize].opt());
            disc_ptr[LAST_BOX_CODE as usize] = None.tex_int();
            pop_nest();
            build_page(input);
        }
        GroupCode::Disc => build_discretionary(input),
        GroupCode::Align => {
            back_input(input, tok);
            let tok = CS_TOKEN_FLAG + FROZEN_CR as i32;
            t_eprint!("Missing {} inserted", Esc("cr"));
            help!("I\'m guessing that you meant to end an alignment here.");
            ins_error(input, tok);
        }
        GroupCode::NoAlign => {
            end_graf();
            unsave(input);
            align_peek(input);
        }
        GroupCode::VCenter => {
            end_graf();
            unsave(input);
            SAVE_PTR -= 2;
            let p = vpackage(
                MEM[cur_list.head].b32.s1.opt(),
                Scaled(SAVE_STACK[SAVE_PTR + 1].val),
                PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
                Scaled::MAX_HALFWORD,
            );
            pop_nest();
            let n = new_noad();
            *LLIST_link(cur_list.tail) = Some(n).tex_int();
            cur_list.tail = n;
            MEM[cur_list.tail].b16.s1 = MathNode::VCenter as u16;
            MEM[cur_list.tail + 1].b32.s1 = MathCell::SubBox as _;
            MEM[cur_list.tail + 1].b32.s0 = Some(p.ptr()).tex_int()
        }
        GroupCode::MathChoice => build_choices(input),
        GroupCode::Math => {
            unsave(input);
            SAVE_PTR -= 1;
            MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32.s1 = MathCell::SubMList as _;
            let p = fin_mlist(None);
            MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32.s0 = p;
            if let Some(p) = p.opt() {
                if llist_link(p).is_none() {
                    match Node::from(p) {
                        Node::Math(MathNode::Ord) => {
                            if MEM[p + 3].b32.s1 == MathCell::Empty as _
                                && MEM[p + 2].b32.s1 == MathCell::Empty as _
                            {
                                MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32 = MEM[p + 1].b32;
                                free_node(p, NOAD_SIZE);
                            }
                        }
                        Node::Math(MathNode::Accent) => {
                            if SAVE_STACK[SAVE_PTR + 0].val == cur_list.tail as i32 + 1 {
                                if let Node::Math(MathNode::Ord) = Node::from(cur_list.tail) {
                                    /*1222:*/
                                    let mut q = cur_list.head;
                                    while llist_link(q) != Some(cur_list.tail) {
                                        q = *LLIST_link(q) as usize;
                                    }
                                    *LLIST_link(q) = Some(p).tex_int();
                                    free_node(cur_list.tail, NOAD_SIZE);
                                    cur_list.tail = p;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        } //_ => confusion("rightbrace"),
    };
}
pub(crate) unsafe fn main_control(input: &mut input_state_t) {
    if let Some(ej) = LOCAL(Local::every_job).opt() {
        begin_token_list(input, Some(ej), Btl::EveryJobText);
    }
    let mut big_switch = true;
    'big_switch: loop {
        /* big_switch */
        if big_switch {
            let next = get_x_token(input);
            cur_tok = next.0;
            cur_cmd = next.1;
            cur_chr = next.2;
            cur_cs = next.3;
        }
        big_switch = true;

        // reswitch
        /*1066: */
        if get_int_par(IntPar::tracing_commands) > 0 {
            show_cur_cmd_chr(cur_cmd, cur_chr); /*:1490 */
        }
        use ListMode::*;
        match (cur_list.mode.1, cur_cmd) {
            (HMode, Cmd::Letter) | (HMode, Cmd::OtherChar) | (HMode, Cmd::CharGiven) => { // 115 | 116 | 172
            }
            (HMode, Cmd::CharNum) => {
                // 120
                cur_chr = scan_usv_num(input);
            }
            (HMode, Cmd::NoBoundary) => {
                // 169
                let next = get_x_token(input);
                cur_tok = next.0;
                cur_cmd = next.1;
                cur_chr = next.2;
                cur_cs = next.3;
                if matches!(
                    cur_cmd,
                    Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven | Cmd::CharNum
                ) {
                    cancel_boundary = true;
                }
                big_switch = false;
                continue 'big_switch;
            }
            _ => {
                if cur_list.mode.1 == ListMode::HMode
                    && get_int_par(IntPar::xetex_inter_char_tokens) > 0
                    && space_class != CHAR_CLASS_LIMIT
                    && prev_class != CHAR_CLASS_LIMIT - 1
                {
                    prev_class = CHAR_CLASS_LIMIT - 1;
                    find_sa_element(
                        ValLevel::InterChar,
                        space_class * CHAR_CLASS_LIMIT + (CHAR_CLASS_LIMIT - 1),
                        false,
                    );
                    if let Some(c) = cur_ptr {
                        let tok = if cur_cs == 0 {
                            if cur_cmd == Cmd::CharNum {
                                cur_cmd = Cmd::OtherChar;
                            }
                            cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
                        } else {
                            CS_TOKEN_FLAG + cur_cs
                        };
                        back_input(input, tok);
                        begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                        continue 'big_switch;
                    }
                }
                match (cur_list.mode.1, cur_cmd) {
                    (NoMode, _) => {}
                    (HMode, Cmd::Spacer) => {
                        // 114
                        if cur_list.aux.b32.s0 == 1000 {
                            append_normal_space(input, cur_cmd, cur_chr, cur_cs);
                        } else {
                            app_space();
                        }
                    }
                    (HMode, Cmd::ExSpace) | (MMode, Cmd::ExSpace) => {
                        // 168 | 271
                        append_normal_space(input, cur_cmd, cur_chr, cur_cs);
                    }
                    (_, Cmd::IgnoreSpaces) => {
                        // 40 | 143 | 246
                        if cur_chr == 0 {
                            loop {
                                let next = get_x_token(input);
                                cur_tok = next.0;
                                cur_cmd = next.1;
                                cur_chr = next.2;
                                cur_cs = next.3;
                                if !(cur_cmd == Cmd::Spacer) {
                                    break;
                                }
                            }
                            big_switch = false;
                        } else {
                            let t = scanner_status;
                            scanner_status = ScannerStatus::Normal;
                            let (_, _, cs) = get_next(input);
                            scanner_status = t;
                            cur_cs = if cs < HASH_BASE as i32 {
                                prim_lookup(cs - SINGLE_BASE as i32) as i32
                            } else {
                                prim_lookup(yhash[cs as usize - hash_offset].s1) as i32
                            };
                            if cur_cs != UNDEFINED_PRIMITIVE {
                                cur_cmd = Cmd::from(prim_eqtb[cur_cs as usize].cmd);
                                cur_chr = prim_eqtb[cur_cs as usize].val;
                                big_switch = false;
                            }
                        }
                    }
                    (VMode, Cmd::Comment) => {
                        // 15
                        if its_all_over(input, cur_tok, cur_cmd, cur_chr) {
                            return;
                        }
                    }
                    (VMode, Cmd::VMove)
                    | (HMode, Cmd::HMove)
                    | (MMode, Cmd::HMove)
                    | (_, Cmd::LastItem)
                    | (VMode, Cmd::VAdjust)
                    | (VMode, Cmd::ItalCorr)
                    | (VMode, Cmd::EqNo)
                    | (HMode, Cmd::EqNo)
                    | (_, Cmd::MacParam) => {
                        // 23 | 125 | 228 | 72 | 175 | 278 | 39 | 45 | 49 | 152 | 7 | 110 | 213
                        report_illegal_case(cur_cmd, cur_chr);
                    }
                    (VMode, Cmd::SupMark)
                    | (HMode, Cmd::SupMark)
                    | (VMode, Cmd::SubMark)
                    | (HMode, Cmd::SubMark)
                    | (VMode, Cmd::MathCharNum)
                    | (HMode, Cmd::MathCharNum)
                    | (VMode, Cmd::MathGiven)
                    | (HMode, Cmd::MathGiven)
                    | (VMode, Cmd::XetexMathGiven)
                    | (HMode, Cmd::XetexMathGiven)
                    | (VMode, Cmd::MathComp)
                    | (HMode, Cmd::MathComp)
                    | (VMode, Cmd::DelimNum)
                    | (HMode, Cmd::DelimNum)
                    | (VMode, Cmd::LeftRight)
                    | (HMode, Cmd::LeftRight)
                    | (VMode, Cmd::Above)
                    | (HMode, Cmd::Above)
                    | (VMode, Cmd::Radical)
                    | (HMode, Cmd::Radical)
                    | (VMode, Cmd::MathStyle)
                    | (HMode, Cmd::MathStyle)
                    | (VMode, Cmd::MathChoice)
                    | (HMode, Cmd::MathChoice)
                    | (VMode, Cmd::VCenter)
                    | (HMode, Cmd::VCenter)
                    | (VMode, Cmd::NonScript)
                    | (HMode, Cmd::NonScript)
                    | (VMode, Cmd::MKern)
                    | (HMode, Cmd::MKern)
                    | (VMode, Cmd::LimitSwitch)
                    | (HMode, Cmd::LimitSwitch)
                    | (VMode, Cmd::MSkip)
                    | (HMode, Cmd::MSkip)
                    | (VMode, Cmd::MathAccent)
                    | (HMode, Cmd::MathAccent)
                    | (MMode, Cmd::EndV)
                    | (MMode, Cmd::ActiveChar)
                    | (MMode, Cmd::Comment)
                    | (MMode, Cmd::VSkip)
                    | (MMode, Cmd::UnVBox)
                    | (MMode, Cmd::VAlign)
                    | (MMode, Cmd::HRule) => {
                        // 8 | 111 | 9 | 112 | 18 | 121 | 70 | 173 | 71 | 174 | 51 | 154 | 16
                        //| 119 | 50 | 153 | 53 | 156 | 67 | 170 | 54 | 157 | 55 | 158 | 57 | 160
                        //| 56 | 159 | 31 | 134 | 52 | 155 | 29 | 132 | 47 | 150 | 216 | 220
                        //| 221 | 234 | 231 | 240 | 243
                        insert_dollar_sign(input, cur_tok);
                    }
                    (VMode, Cmd::HRule) | (HMode, Cmd::VRule) | (MMode, Cmd::VRule) => {
                        // 37 | 139 | 242
                        let srs = scan_rule_spec(input, cur_cmd).ptr();
                        *LLIST_link(cur_list.tail) = Some(srs).tex_int();
                        cur_list.tail = srs;
                        if cur_list.mode.1 == ListMode::VMode {
                            cur_list.aux.b32.s1 = IGNORE_DEPTH;
                        } else if cur_list.mode.1 == ListMode::HMode {
                            cur_list.aux.b32.s0 = 1000;
                        }
                    }
                    (VMode, Cmd::VSkip)
                    | (HMode, Cmd::HSkip)
                    | (MMode, Cmd::HSkip)
                    | (MMode, Cmd::MSkip) => {
                        // 28 | 130 | 233 | 235
                        append_glue(input, cur_chr);
                    }
                    (_, Cmd::Kern) | (MMode, Cmd::MKern) => {
                        // 30 | 133 | 236 | 237
                        append_kern(input, cur_chr);
                    }
                    (VMode, Cmd::LeftBrace) | (HMode, Cmd::LeftBrace) => {
                        // 2 | 105
                        new_save_level(GroupCode::Simple);
                    }
                    (_, Cmd::BeginGroup) => {
                        // 62 | 165 | 268
                        new_save_level(GroupCode::SemiSimple);
                    }
                    (_, Cmd::EndGroup) => {
                        // 63 | 166 | 269
                        if cur_group == GroupCode::SemiSimple {
                            unsave(input);
                        } else {
                            off_save(input, cur_group, cur_tok, cur_cmd, cur_chr);
                        }
                    }
                    (_, Cmd::RightBrace) => {
                        // 3 | 106 | 209
                        handle_right_brace(input, cur_group, cur_tok);
                    }
                    (VMode, Cmd::HMove) | (HMode, Cmd::VMove) | (MMode, Cmd::VMove) => {
                        // 22 | 126 | 229
                        let t = cur_chr;
                        let val = scan_dimen(input, false, false, None);
                        if t == 0 {
                            scan_box(input, val.0);
                        } else {
                            scan_box(input, -val.0);
                        }
                    }
                    (_, Cmd::LeaderShip) => {
                        // 32 | 135 | 238
                        scan_box(input, LEADER_FLAG - (A_LEADERS as i32) + cur_chr);
                    }
                    (_, Cmd::MakeBox) => {
                        // 21 | 124 | 227
                        begin_box(input, cur_cmd, cur_chr, 0);
                    }
                    (VMode, Cmd::StartPar) => {
                        // 44
                        new_graf(input, cur_chr > 0);
                    }
                    (VMode, Cmd::Letter)
                    | (VMode, Cmd::OtherChar)
                    | (VMode, Cmd::CharNum)
                    | (VMode, Cmd::CharGiven)
                    | (VMode, Cmd::MathShift)
                    | (VMode, Cmd::UnHBox)
                    | (VMode, Cmd::VRule)
                    | (VMode, Cmd::Accent)
                    | (VMode, Cmd::Discretionary)
                    | (VMode, Cmd::HSkip)
                    | (VMode, Cmd::VAlign)
                    | (VMode, Cmd::ExSpace)
                    | (VMode, Cmd::NoBoundary) => {
                        // 12 | 13 | 17 | 69 | 4 | 24 | 36 | 46 | 48 | 27 | 34 | 65 | 66
                        back_input(input, cur_tok);
                        new_graf(input, true);
                    }
                    (HMode, Cmd::StartPar) | (MMode, Cmd::StartPar) => {
                        // 147 | 250
                        indent_in_hmode(cur_chr);
                    }
                    (VMode, Cmd::ActiveChar) => {
                        // 14
                        normal_paragraph();
                        if !cur_list.mode.0 {
                            build_page(input);
                        }
                    }
                    (HMode, Cmd::ActiveChar) => {
                        // 117
                        if align_state < 0 {
                            off_save(input, cur_group, cur_tok, cur_cmd, cur_chr);
                        }
                        end_graf();
                        if cur_list.mode == (false, ListMode::VMode) {
                            build_page(input);
                        }
                    }
                    (HMode, Cmd::Comment)
                    | (HMode, Cmd::VSkip)
                    | (HMode, Cmd::HRule)
                    | (HMode, Cmd::UnVBox)
                    | (HMode, Cmd::HAlign) => {
                        // 118 | 131 | 140 | 128 | 136
                        head_for_vmode(input, cur_tok, cur_cmd, cur_chr);
                    }
                    (_, Cmd::Insert) | (HMode, Cmd::VAdjust) | (MMode, Cmd::VAdjust) => {
                        // 38 | 141 | 244 | 142 | 245
                        begin_insert_or_adjust(input, cur_cmd);
                    }
                    (_, Cmd::Mark) => {
                        // 19 | 122 | 225
                        make_mark(input, cur_chr, cur_cs);
                    }
                    (_, Cmd::BreakPenalty) => {
                        // 43 | 146 | 249
                        append_penalty(input);
                    }
                    (_, Cmd::RemoveItem) => {
                        // 26 | 129 | 232
                        delete_last(cur_cmd, cur_chr);
                    }
                    (VMode, Cmd::UnVBox) | (HMode, Cmd::UnHBox) | (MMode, Cmd::UnHBox) => {
                        // 25 | 127 | 230
                        unpackage(input, cur_chr);
                    }
                    (HMode, Cmd::ItalCorr) => {
                        // 148
                        append_italic_correction();
                    }
                    (MMode, Cmd::ItalCorr) => {
                        // 251
                        let k = new_kern(Scaled::ZERO);
                        *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
                        cur_list.tail = k.ptr();
                    }
                    (HMode, Cmd::Discretionary) | (MMode, Cmd::Discretionary) => {
                        // 151 | 254
                        append_discretionary(input, cur_chr);
                    }
                    (HMode, Cmd::Accent) => {
                        // 149
                        make_accent(input);
                    }
                    (_, Cmd::CarRet) | (_, Cmd::TabMark) => {
                        // 6 | 109 | 212 | 5 | 108 | 211
                        align_error(input, cur_tok, cur_cmd, cur_chr);
                    }
                    (_, Cmd::NoAlign) => {
                        // 35 | 138 | 241
                        no_align_error();
                    }
                    (_, Cmd::Omit) => {
                        // 64 | 167 | 270
                        omit_error();
                    }
                    (VMode, Cmd::HAlign) => {
                        // 33
                        init_align(input, cur_cs);
                    }
                    (HMode, Cmd::VAlign) => {
                        // 137
                        if cur_chr > 0 {
                            if eTeX_enabled(get_int_par(IntPar::texxet) > 0, cur_cmd, cur_chr) {
                                let m = new_math(Scaled::ZERO, MathType::from(cur_chr as u16));
                                *LLIST_link(cur_list.tail) = Some(m.ptr()).tex_int();
                                cur_list.tail = m.ptr();
                            }
                        } else {
                            init_align(input, cur_cs);
                        }
                    }
                    (MMode, Cmd::HAlign) => {
                        // 239
                        if privileged(cur_cmd, cur_chr) {
                            if cur_group == GroupCode::MathShift {
                                init_align(input, cur_cs);
                            } else {
                                off_save(input, cur_group, cur_tok, cur_cmd, cur_chr);
                            }
                        }
                    }
                    (VMode, Cmd::EndV) | (HMode, Cmd::EndV) => {
                        // 10 | 113
                        INPUT_STACK[INPUT_PTR] = *input;
                        do_endv(
                            input,
                            cur_group,
                            cur_tok,
                            cur_cmd,
                            cur_chr,
                            &INPUT_STACK[..INPUT_PTR + 1],
                        );
                    }
                    (_, Cmd::EndCSName) => {
                        // 68 | 171 | 274
                        cs_error();
                    }
                    (HMode, Cmd::MathShift) => {
                        // 107
                        init_math(input);
                    }
                    (MMode, Cmd::EqNo) => {
                        // 255
                        if privileged(cur_cmd, cur_chr) {
                            if cur_group == GroupCode::MathShift {
                                start_eq_no(input, cur_chr);
                            } else {
                                off_save(input, cur_group, cur_tok, cur_cmd, cur_chr);
                            }
                        }
                    }
                    (MMode, Cmd::LeftBrace) => {
                        // 208
                        let n = new_noad();
                        *LLIST_link(cur_list.tail) = Some(n).tex_int();
                        cur_list.tail = n;
                        back_input(input, cur_tok);
                        let mut m = BaseMath(cur_list.tail);
                        scan_math(input, m.nucleus_mut(), cur_list.tail + 1);
                    }
                    (MMode, Cmd::Letter) | (MMode, Cmd::OtherChar) | (MMode, Cmd::CharGiven) => {
                        // 218 | 219 | 275
                        set_math_char(input, cur_chr, *MATH_CODE(cur_chr as usize));
                    }
                    (MMode, Cmd::CharNum) => {
                        // 223
                        cur_chr = scan_char_num(input);
                        set_math_char(input, cur_chr, *MATH_CODE(cur_chr as usize));
                    }
                    (MMode, Cmd::MathCharNum) => {
                        // 224
                        if cur_chr == 2 {
                            let val = scan_math_class_int(input);
                            let t = set_class(val);
                            let val = scan_math_fam_int(input);
                            let t = t + set_family(val);
                            let val = scan_usv_num(input);
                            let t = t + val;
                            set_math_char(input, cur_chr, t);
                        } else if cur_chr == 1 {
                            let val = scan_xetex_math_char_int(input);
                            set_math_char(input, cur_chr, val);
                        } else {
                            let val = scan_fifteen_bit_int(input);
                            set_math_char(
                                input,
                                cur_chr,
                                set_class(val / 4096)
                                    + set_family((val % 4096) / 256)
                                    + (val % 256),
                            );
                        }
                    }
                    (MMode, Cmd::MathGiven) => {
                        // 276
                        set_math_char(
                            input,
                            cur_chr,
                            set_class(cur_chr / 4096)
                                + set_family((cur_chr % 4096) / 256)
                                + (cur_chr % 256),
                        );
                    }
                    (MMode, Cmd::XetexMathGiven) => {
                        // 277
                        set_math_char(input, cur_chr, cur_chr);
                    }
                    (MMode, Cmd::DelimNum) => {
                        // 222
                        if cur_chr == 1 {
                            scan_math_class_int(input);
                            let val = scan_math_class_int(input);
                            let t = set_class(val);
                            let val = scan_math_fam_int(input);
                            let t = t + set_family(val);
                            let val = scan_usv_num(input);
                            let t = t + val;
                            set_math_char(input, cur_chr, t);
                        } else {
                            let val = scan_delimiter_int(input);
                            let val = val / 4096;
                            set_math_char(
                                input,
                                cur_chr,
                                set_class(val / 4096)
                                    + set_family((val % 4096) / 256)
                                    + (val % 256),
                            );
                        }
                    }
                    (MMode, Cmd::MathComp) => {
                        // 257
                        let n = new_noad();
                        *LLIST_link(cur_list.tail) = Some(n).tex_int();
                        cur_list.tail = n;
                        set_math_NODE_type(n, MathNode::n(cur_chr as u16).unwrap());
                        let mut m = BaseMath(cur_list.tail);
                        scan_math(input, m.nucleus_mut(), cur_list.tail + 1);
                    }
                    (MMode, Cmd::LimitSwitch) => {
                        // 258
                        math_limit_switch(cur_chr);
                    }
                    (MMode, Cmd::Radical) => {
                        // 273
                        math_radical(input, cur_tok, cur_chr);
                    }
                    (MMode, Cmd::Accent) | (MMode, Cmd::MathAccent) => {
                        // 252 | 253
                        math_ac(input, cur_cmd, cur_chr);
                    }
                    (MMode, Cmd::VCenter) => {
                        // 263
                        scan_spec(input, GroupCode::VCenter, false);
                        normal_paragraph();
                        push_nest();
                        cur_list.mode = (true, ListMode::VMode);
                        cur_list.aux.b32.s1 = IGNORE_DEPTH;
                        if insert_src_special_every_vbox {
                            insert_src_special();
                        }
                        if let Some(ev) = LOCAL(Local::every_vbox).opt() {
                            begin_token_list(input, Some(ev), Btl::EveryVBoxText);
                        }
                    }
                    (MMode, Cmd::MathStyle) => {
                        // 260
                        let s = new_style(cur_chr as i16);
                        *LLIST_link(cur_list.tail) = Some(s).tex_int();
                        cur_list.tail = s;
                    }
                    (MMode, Cmd::NonScript) => {
                        // 262
                        let g = new_glue(&ZERO_GLUE);
                        *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
                        cur_list.tail = g.ptr();
                        MEM[cur_list.tail].b16.s0 = COND_MATH_GLUE;
                    }
                    (MMode, Cmd::MathChoice) => {
                        // 261
                        append_choices(input);
                    }
                    (MMode, Cmd::SubMark) | (MMode, Cmd::SupMark) => {
                        // 215 | 214
                        sub_sup(input, cur_cmd);
                    }
                    (MMode, Cmd::Above) => {
                        // 259
                        math_fraction(input, cur_tok, cur_chr);
                    }
                    (MMode, Cmd::LeftRight) => {
                        // 256
                        math_left_right(input, cur_group, cur_tok, cur_cmd, cur_chr);
                    }
                    (MMode, Cmd::MathShift) => {
                        // 210
                        if cur_group == GroupCode::MathShift {
                            after_math(input);
                        } else {
                            off_save(input, cur_group, cur_tok, cur_cmd, cur_chr);
                        }
                    }
                    (_, Cmd::ToksRegister)
                    | (_, Cmd::AssignToks)
                    | (_, Cmd::AssignInt)
                    | (_, Cmd::AssignDimen)
                    | (_, Cmd::AssignGlue)
                    | (_, Cmd::AssignMuGlue)
                    | (_, Cmd::AssignFontDimen)
                    | (_, Cmd::AssignFontInt)
                    | (_, Cmd::SetAux)
                    | (_, Cmd::SetPrevGraf)
                    | (_, Cmd::SetPageDimen)
                    | (_, Cmd::SetPageInt)
                    | (_, Cmd::SetBoxDimen)
                    | (_, Cmd::SetShape)
                    | (_, Cmd::DefCode)
                    | (_, Cmd::XetexDefCode)
                    | (_, Cmd::DefFamily)
                    | (_, Cmd::SetFont)
                    | (_, Cmd::DefFont)
                    | (_, Cmd::Register)
                    | (_, Cmd::Advance)
                    | (_, Cmd::Multiply)
                    | (_, Cmd::Divide)
                    | (_, Cmd::Prefix)
                    | (_, Cmd::Let)
                    | (_, Cmd::ShorthandDef)
                    | (_, Cmd::ReadToCS)
                    | (_, Cmd::Def)
                    | (_, Cmd::SetBox)
                    | (_, Cmd::HyphData)
                    | (_, Cmd::SetInteraction) => {
                        // 73 | 176 | 279 | 74 | 177 | 280 | 75 | 178 | 281 | 76 | 179 | 282 | 77
                        //| 180 | 283 | 78 | 181 | 284 | 79 | 182 | 285 | 80 | 183 | 286 | 81
                        //| 184 | 287 | 82 | 185 | 288 | 83 | 186 | 289 | 84 | 187 | 290 | 85
                        //| 188 | 291 | 86 | 189 | 292 | 87 | 190 | 293 | 88 | 191 | 294 | 89
                        //| 192 | 295 | 90 | 193 | 296 | 91 | 194 | 297 | 92 | 195 | 298 | 93
                        //| 196 | 299 | 94 | 197 | 300 | 95 | 198 | 301 | 96 | 199 | 302 | 97
                        //| 200 | 303 | 98 | 201 | 304 | 99 | 202 | 305 | 100 | 203 | 306 | 101
                        //| 204 | 307 | 102 | 205 | 308 | 103 | 206 | 309
                        prefixed_command(input, cur_cmd, cur_chr, cur_cs);
                    }
                    (_, Cmd::AfterAssignment) => {
                        // 41 | 144 | 247
                        let (tok, ..) = get_token(input);
                        after_token = tok;
                    }
                    (_, Cmd::AfterGroup) => {
                        // 42 | 145 | 248
                        let (tok, ..) = get_token(input);
                        save_for_after(tok);
                    }
                    (_, Cmd::InStream) => {
                        // 61 | 164 | 267
                        open_or_close_in(input, cur_chr);
                    }
                    (_, Cmd::Message) => {
                        // 59 | 162 | 265
                        issue_message(input, cur_chr, cur_cs);
                    }
                    (_, Cmd::CaseShift) => {
                        // 58 | 161 | 264
                        shift_case(input, cur_chr, cur_cs);
                    }
                    (_, Cmd::XRay) => {
                        // 20 | 123 | 226
                        show_whatever(input, cur_chr, cur_cs);
                    }
                    (_, Cmd::Extension) => {
                        // 60 | 163 | 266
                        do_extension(input, cur_tok, cur_cmd, cur_chr, cur_cs);
                    }
                    //(_, Cmd::Relax)
                    //| (VMode, Cmd::Spacer)
                    //| (MMode, Cmd::Spacer)
                    //| (MMode, Cmd::NoBoundary) |
                    _ => {
                        // 1 | 104 | 207 | 11 | 217 | 272 | _
                    }
                }
                continue 'big_switch;
            }
        }

        /*main_loop *//*1069: */
        if cur_list.head == cur_list.tail && !cur_list.mode.0 && insert_src_special_auto {
            append_src_special();
        }
        prev_class = CHAR_CLASS_LIMIT - 1;
        if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize] {
            if !cur_list.mode.0 && get_int_par(IntPar::language) != cur_list.aux.b32.s1 {
                fix_language();
            }
            main_h = 0;
            main_f = EQTB[CUR_FONT_LOC].val as usize;
            let mut native_text = Vec::with_capacity(128);
            let lab72 = loop {
                // lab71: collect_native
                main_s = (*SF_CODE(cur_chr as usize) as i64 % 65536) as i32;
                if main_s == 1000 {
                    cur_list.aux.b32.s0 = 1000;
                } else if main_s < 1000 {
                    if main_s > 0 {
                        cur_list.aux.b32.s0 = main_s;
                    }
                } else if cur_list.aux.b32.s0 < 1000 {
                    cur_list.aux.b32.s0 = 1000;
                } else {
                    cur_list.aux.b32.s0 = main_s;
                }
                cur_ptr = None;
                space_class = (*SF_CODE(cur_chr as usize) as i64 / 65536) as i32;
                if get_int_par(IntPar::xetex_inter_char_tokens) > 0 && space_class != 4096 {
                    if prev_class == CHAR_CLASS_LIMIT - 1 {
                        if input.state != InputState::TokenList || input.index != Btl::BackedUpChar
                        {
                            find_sa_element(
                                ValLevel::InterChar,
                                (CHAR_CLASS_LIMIT - 1) * CHAR_CLASS_LIMIT + space_class,
                                false,
                            );
                            if let Some(c) = cur_ptr {
                                if cur_cmd != Cmd::Letter {
                                    cur_cmd = Cmd::OtherChar;
                                }
                                let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                back_input(input, tok);
                                input.index = Btl::BackedUpChar;
                                begin_token_list(
                                    input,
                                    MEM[c + 1].b32.s1.opt(),
                                    Btl::InterCharText,
                                );
                                continue 'big_switch;
                            }
                        }
                    } else {
                        find_sa_element(
                            ValLevel::InterChar,
                            prev_class * CHAR_CLASS_LIMIT + space_class,
                            false,
                        );
                        if let Some(c) = cur_ptr {
                            if cur_cmd != Cmd::Letter {
                                cur_cmd = Cmd::OtherChar;
                            }
                            let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                            back_input(input, tok);
                            input.index = Btl::BackedUpChar;
                            begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                            prev_class = CHAR_CLASS_LIMIT - 1;
                            break false;
                        }
                    }
                    prev_class = space_class
                }
                let mut buf = [0; 2];
                for c16 in std::char::from_u32(cur_chr as u32)
                    .unwrap()
                    .encode_utf16(&mut buf)
                {
                    native_text.push(*c16);
                }
                is_hyph = cur_chr == HYPHEN_CHAR[main_f as usize]
                    || get_int_par(IntPar::xetex_dash_break) > 0
                        && (cur_chr == '—' as i32 || cur_chr == '–' as i32);
                if main_h == 0 && is_hyph {
                    main_h = native_text.len() as _;
                }
                let (cmd, chr, cs) = get_next(input);
                cur_cmd = cmd;
                cur_chr = chr;
                cur_cs = cs;
                if cur_cmd == Cmd::Letter || cur_cmd == Cmd::OtherChar || cur_cmd == Cmd::CharGiven
                {
                    continue;
                }
                cur_tok = x_token(input, &mut cur_cmd, &mut cur_chr, &mut cur_cs);
                if cur_cmd == Cmd::Letter || cur_cmd == Cmd::OtherChar || cur_cmd == Cmd::CharGiven
                {
                    continue;
                }
                if cur_cmd == Cmd::CharNum {
                    cur_chr = scan_usv_num(input);
                } else if get_int_par(IntPar::xetex_inter_char_tokens) > 0
                    && space_class != CHAR_CLASS_LIMIT
                    && prev_class != CHAR_CLASS_LIMIT - 1
                {
                    break true;
                } else {
                    break false;
                }
            };
            if lab72 {
                prev_class = CHAR_CLASS_LIMIT - 1;
                find_sa_element(
                    ValLevel::InterChar,
                    space_class * CHAR_CLASS_LIMIT + (CHAR_CLASS_LIMIT - 1),
                    false,
                );
                if let Some(c) = cur_ptr {
                    let tok = if cur_cs == 0 {
                        if cur_cmd == Cmd::CharNum {
                            cur_cmd = Cmd::OtherChar;
                        }
                        cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
                    } else {
                        CS_TOKEN_FLAG + cur_cs
                    };
                    back_input(input, tok);
                    begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                }
            }
            /*collected */
            if !(FONT_MAPPING[main_f as usize]).is_null() {
                let mapped_text =
                    apply_mapping(FONT_MAPPING[main_f as usize], native_text.as_slice());
                main_k = mapped_text.len() as _;
                native_text.clear();
                main_h = 0;
                for &mt in &mapped_text {
                    native_text.push(mt);
                    if main_h == 0
                        && (mt as i32 == HYPHEN_CHAR[main_f as usize]
                            || get_int_par(IntPar::xetex_dash_break) > 0
                                && (mt == '—' as u16 || mt == '–' as u16))
                    {
                        main_h = native_text.len() as _;
                    }
                }
            }
            if get_int_par(IntPar::tracing_lost_chars) > 0 {
                for c in std::char::decode_utf16(native_text.iter().cloned()) {
                    let c = c.unwrap();
                    if map_char_to_glyph(nf, c) == 0 {
                        char_warning(main_f, c);
                    }
                }
            }
            main_k = native_text.len() as _;
            if cur_list.mode == (false, ListMode::HMode) {
                let mut main_pp = cur_list.tail;
                let mut main_ppp = cur_list.head;
                if main_ppp != main_pp {
                    while llist_link(main_ppp) != Some(main_pp) {
                        if let CharOrText::Text(TxtNode::Disc(d)) = CharOrText::from(main_ppp) {
                            for _ in 0..d.replace_count() {
                                main_ppp = llist_link(main_ppp).unwrap();
                            }
                        }
                        if main_ppp != main_pp {
                            main_ppp = llist_link(main_ppp).unwrap();
                        }
                    }
                }
                let mut tmp_ptr = 0;
                loop {
                    if main_h == 0 {
                        main_h = main_k
                    }
                    match CharOrText::from(main_pp) {
                        CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw)))
                            if nw.font() as usize == main_f
                                && main_ppp != main_pp
                                && match CharOrText::from(main_ppp) {
                                    CharOrText::Char(_) => false,
                                    CharOrText::Text(TxtNode::Disc(_)) => false,
                                    _ => true,
                                } =>
                        {
                            let native_pp = NativeWord::from(main_pp);
                            main_k = main_h + native_pp.length() as i32;
                            let save_native_len = native_text.len();
                            for c in native_pp.text() {
                                native_text.push(*c);
                            }
                            for main_p in 0..main_h {
                                native_text.push(native_text[tmp_ptr + (main_p as usize)]);
                            }
                            do_locale_linebreaks(
                                &native_text[save_native_len..save_native_len + (main_k as usize)],
                            );
                            native_text.truncate(save_native_len);
                            main_k = (native_text.len() as i32) - main_h - (tmp_ptr as i32);
                            tmp_ptr = main_h as usize;
                            main_h = 0;
                            while main_h < main_k
                                && native_text[tmp_ptr + (main_h as usize)] as i32
                                    != HYPHEN_CHAR[main_f as usize]
                                && (get_int_par(IntPar::xetex_dash_break) <= 0
                                    || native_text[tmp_ptr + (main_h as usize)] as i32 != 8212
                                        && native_text[tmp_ptr + (main_h as usize)] as i32 != 8211)
                            {
                                main_h += 1
                            }
                            if main_h < main_k {
                                main_h += 1
                            }
                            *LLIST_link(main_ppp) = *LLIST_link(main_pp);
                            *LLIST_link(main_pp) = None.tex_int();
                            flush_node_list(Some(main_pp));
                            main_pp = cur_list.tail;
                            while llist_link(main_ppp) != Some(main_pp) {
                                main_ppp = llist_link(main_ppp).unwrap();
                            }
                        }
                        _ => {
                            do_locale_linebreaks(
                                &native_text[tmp_ptr..tmp_ptr + (main_h as usize)],
                            );
                            tmp_ptr += main_h as usize;
                            main_k -= main_h;
                            main_h = 0;
                            while main_h < main_k
                                && native_text[tmp_ptr + (main_h as usize)] as i32
                                    != HYPHEN_CHAR[main_f as usize]
                                && (get_int_par(IntPar::xetex_dash_break) <= 0
                                    || native_text[tmp_ptr + (main_h as usize)] as i32 != 8212
                                        && native_text[tmp_ptr + (main_h as usize)] as i32 != 8211)
                            {
                                main_h += 1
                            }
                            if main_h < main_k {
                                main_h += 1
                            }
                        }
                    }
                    if main_k > 0 || is_hyph {
                        let d = new_disc();
                        *LLIST_link(cur_list.tail) = Some(d).tex_int();
                        cur_list.tail = d;
                        main_pp = cur_list.tail;
                    }
                    if main_k == 0 {
                        break;
                    }
                }
            } else {
                let main_pp = cur_list.tail;
                let mut main_ppp = cur_list.head;
                if main_ppp != main_pp {
                    while llist_link(main_ppp) != Some(main_pp) {
                        if let CharOrText::Text(TxtNode::Disc(d)) = CharOrText::from(main_ppp) {
                            for _ in 0..d.replace_count() {
                                main_ppp = *LLIST_link(main_ppp) as usize;
                            }
                        }
                        if main_ppp != main_pp {
                            main_ppp = *LLIST_link(main_ppp) as usize;
                        }
                    }
                }
                match CharOrText::from(main_pp) {
                    CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw)))
                        if nw.font() as usize == main_f
                            && main_ppp != main_pp
                            && match CharOrText::from(main_ppp) {
                                CharOrText::Char(_) => false,
                                CharOrText::Text(TxtNode::Disc(_)) => false,
                                _ => true,
                            } =>
                    {
                        let nw = NativeWord::from(main_pp);
                        let text = nw.text();
                        let mut nwn = new_native_word_node(main_f, main_k + text.len() as i32);
                        *LLIST_link(main_pp) = Some(nwn.ptr()).tex_int();
                        cur_list.tail = nwn.ptr();

                        let tail_text = nwn.text_mut();
                        tail_text[..text.len()].copy_from_slice(text);

                        tail_text[text.len()..].copy_from_slice(&native_text[..main_k as usize]);

                        nwn.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
                        let mut main_p = cur_list.head;
                        if main_p != main_pp {
                            while llist_link(main_p) != Some(main_pp) {
                                main_p = *LLIST_link(main_p) as usize;
                            }
                        }
                        *LLIST_link(main_p) = *LLIST_link(main_pp);
                        *LLIST_link(main_pp) = None.tex_int();
                        flush_node_list(Some(main_pp));
                    }
                    _ => {
                        let mut nwn = new_native_word_node(main_f, main_k);
                        *LLIST_link(main_pp) = Some(nwn.ptr()).tex_int();
                        cur_list.tail = nwn.ptr();

                        nwn.text_mut()
                            .copy_from_slice(&native_text[..main_k as usize]);

                        nwn.set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
                    }
                }
            }
            if get_int_par(IntPar::xetex_interword_space_shaping) > 0 {
                let mut main_p = cur_list.head;
                let mut main_pp = None;
                while main_p != cur_list.tail {
                    if let CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(_))) =
                        CharOrText::from(main_p)
                    {
                        main_pp = Some(main_p);
                    }
                    main_p = llist_link(main_p).unwrap();
                }
                if let Some(main_pp) = main_pp {
                    let native_pp = NativeWord::from(main_pp);
                    if native_pp.font() as usize == main_f {
                        let mut main_p = llist_link(main_pp).unwrap();
                        while match CharOrText::from(main_p) {
                            CharOrText::Text(n) => match n {
                                TxtNode::Penalty(_)
                                | TxtNode::Ins(_)
                                | TxtNode::Mark(_)
                                | TxtNode::Adjust(_) => true,
                                TxtNode::WhatsIt(n) => matches!(n, WhatsIt::Open(_)
                                    | WhatsIt::Write(_)
                                    | WhatsIt::Close(_)
                                    | WhatsIt::Special(_)
                                    | WhatsIt::Language(_)),
                                _ => false,
                            },
                            _ => false,
                        } {
                            main_p = llist_link(main_p).unwrap();
                        }
                        if let CharOrText::Text(TxtNode::Glue(_)) = CharOrText::from(main_p) {
                            let mut main_ppp = llist_link(main_p).unwrap();
                            while match CharOrText::from(main_ppp) {
                                CharOrText::Text(n) => match n {
                                    TxtNode::Penalty(_)
                                    | TxtNode::Ins(_)
                                    | TxtNode::Mark(_)
                                    | TxtNode::Adjust(_) => true,
                                    TxtNode::WhatsIt(n) => matches!(n,
                                        WhatsIt::Open(_)
                                        | WhatsIt::Write(_)
                                        | WhatsIt::Close(_)
                                        | WhatsIt::Special(_)
                                        | WhatsIt::Language(_)),
                                    _ => false,
                                },
                                _ => false,
                            } {
                                main_ppp = llist_link(main_ppp).unwrap();
                            }
                            if main_ppp == cur_list.tail {
                                let pp_text = native_pp.text();
                                let native_tail = NativeWord::from(cur_list.tail);
                                let tail_text = native_tail.text();
                                main_k = pp_text.len() as i32 + 1 + tail_text.len() as i32;
                                let mut tmp_ptr = new_native_word_node(main_f, main_k);
                                let temp_text = tmp_ptr.text_mut();
                                temp_text[..pp_text.len()].copy_from_slice(&pp_text);
                                temp_text[pp_text.len()] = ' ' as u16;
                                temp_text[pp_text.len() + 1..].copy_from_slice(&tail_text);

                                tmp_ptr
                                    .set_metrics(get_int_par(IntPar::xetex_use_glyph_metrics) > 0);
                                let t = tmp_ptr.width() - native_pp.width() - native_tail.width();
                                tmp_ptr.free();
                                let fg = GlueSpec(FONT_GLUE[main_f as usize] as usize);
                                if t != fg.size() {
                                    let mut tmp_ptr = new_kern(t - fg.size());
                                    tmp_ptr.set_subtype(KernType::SpaceAdjustment);
                                    *LLIST_link(tmp_ptr.ptr()) = *LLIST_link(main_p);
                                    *LLIST_link(main_p) = Some(tmp_ptr.ptr()).tex_int();
                                }
                            }
                        }
                    }
                }
            }
            if cur_ptr.is_none() {
                big_switch = false;
            }
            continue 'big_switch;
        }

        main_s = (*SF_CODE(cur_chr as usize) as i64 % 65536) as i32;
        if main_s == 1000 {
            cur_list.aux.b32.s0 = 1000
        } else if main_s < 1000 {
            if main_s > 0 {
                cur_list.aux.b32.s0 = main_s
            }
        } else if cur_list.aux.b32.s0 < 1000 {
            cur_list.aux.b32.s0 = 1000
        } else {
            cur_list.aux.b32.s0 = main_s
        }
        cur_ptr = None;
        space_class = (*SF_CODE(cur_chr as usize) as i64 / 65536) as i32;
        if get_int_par(IntPar::xetex_inter_char_tokens) > 0 && space_class != CHAR_CLASS_LIMIT {
            if prev_class == CHAR_CLASS_LIMIT - 1 {
                if input.state != InputState::TokenList || input.index != Btl::BackedUpChar {
                    find_sa_element(
                        ValLevel::InterChar,
                        (CHAR_CLASS_LIMIT - 1) * CHAR_CLASS_LIMIT + space_class,
                        false,
                    );
                    if let Some(c) = cur_ptr {
                        if cur_cmd != Cmd::Letter {
                            cur_cmd = Cmd::OtherChar;
                        }
                        let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                        back_input(input, tok);
                        input.index = Btl::BackedUpChar;
                        begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                        continue 'big_switch;
                    }
                }
            } else {
                find_sa_element(
                    ValLevel::InterChar,
                    prev_class * CHAR_CLASS_LIMIT + space_class,
                    false,
                );
                if let Some(c) = cur_ptr {
                    if cur_cmd != Cmd::Letter {
                        cur_cmd = Cmd::OtherChar;
                    }
                    let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                    back_input(input, tok);
                    input.index = Btl::BackedUpChar;
                    begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                    prev_class = CHAR_CLASS_LIMIT - 1;
                    continue 'big_switch;
                }
            }
            prev_class = space_class
        }
        main_f = EQTB[CUR_FONT_LOC].val as usize;
        bchar = FONT_BCHAR[main_f as usize];
        false_bchar = FONT_FALSE_BCHAR[main_f as usize];
        if !cur_list.mode.0 && get_int_par(IntPar::language) != cur_list.aux.b32.s1 {
            fix_language();
        }
        let mut ls = Char(fast_get_avail());
        lig_stack = Some(ls.ptr());
        ls.set_font(main_f as u16);
        cur_l = cur_chr;
        ls.set_character(cur_l as u16);
        cur_q = cur_list.tail;

        let mut current_block: u64;
        if cancel_boundary {
            cancel_boundary = false;
            main_k = NON_ADDRESS as _;
        } else {
            main_k = BCHAR_LABEL[main_f as usize]
        }
        if main_k == NON_ADDRESS as _ {
            current_block = 249799543778823886;
        } else {
            cur_r = cur_l;
            cur_l = TOO_BIG_CHAR;
            current_block = 13962460947151495567;
        }
        loop {
            match current_block {
                13962460947151495567 => {
                    // lab111
                    /*main_lig_loop 1 */
                    main_j = FONT_INFO[main_k as usize].b16;
                    current_block = 11331079115679122507;
                }
                _ => {
                    let ls = lig_stack.unwrap();
                    let c = std::char::from_u32(cur_chr as u32).unwrap();
                    if effective_char(false, main_f, c as u16) > FONT_EC[main_f as usize] as i32
                        || effective_char(false, main_f, c as u16) < FONT_BC[main_f as usize] as i32
                    {
                        char_warning(main_f, c);
                        *LLIST_link(ls) = avail.tex_int();
                        avail = lig_stack;
                        continue 'big_switch;
                    } else {
                        main_i = effective_char_info(main_f, cur_l as u16);
                        if main_i.s3 == 0 {
                            char_warning(main_f, c);
                            *LLIST_link(ls) = avail.tex_int();
                            avail = lig_stack;
                            continue 'big_switch;
                        } else {
                            *LLIST_link(cur_list.tail) = lig_stack.tex_int();
                            cur_list.tail = ls;
                        }
                    }
                    current_block = 18270385712206273994;
                }
            }
            'c_125244: loop {
                match current_block {
                    11331079115679122507 =>
                    // lab112
                    /*main_lig_loop 2 */
                    {
                        if main_j.s2 as i32 == cur_r {
                            if main_j.s3 <= 128 {
                                /*1075: */
                                if main_j.s1 >= 128 {
                                    if cur_l < TOO_BIG_CHAR {
                                        if LLIST_link(cur_q).opt().is_some()
                                            && MEM[cur_list.tail].b16.s0 as i32
                                                == HYPHEN_CHAR[main_f as usize]
                                        {
                                            ins_disc = true;
                                        }
                                        if ligature_present {
                                            let mut main_p = Ligature(new_ligature(
                                                main_f,
                                                cur_l as u16,
                                                llist_link(cur_q),
                                            ));
                                            main_p.set_hits(lft_hit, rt_hit && lig_stack.is_some());
                                            lft_hit = false;
                                            if rt_hit && lig_stack.is_some() {
                                                rt_hit = false;
                                            }
                                            *LLIST_link(cur_q) = Some(main_p.ptr()).tex_int();
                                            cur_list.tail = main_p.ptr();
                                            ligature_present = false
                                        }
                                        if ins_disc {
                                            ins_disc = false;
                                            if !cur_list.mode.0 {
                                                let d = new_disc();
                                                *LLIST_link(cur_list.tail) = Some(d).tex_int();
                                                cur_list.tail = d;
                                            }
                                        }
                                    }
                                    *LLIST_link(cur_list.tail) = new_kern(Scaled(
                                        FONT_INFO[(KERN_BASE[main_f as usize]
                                            + 256 * main_j.s1 as i32
                                            + main_j.s0 as i32)
                                            as usize]
                                            .b32
                                            .s1,
                                    ))
                                    .ptr()
                                        as i32;
                                    cur_list.tail = *LLIST_link(cur_list.tail) as usize;
                                    current_block = 2772858075894446251;
                                } else {
                                    if cur_l == TOO_BIG_CHAR {
                                        lft_hit = true
                                    } else if lig_stack.is_none() {
                                        rt_hit = true
                                    }
                                    match main_j.s1 {
                                        1 | 5 => {
                                            cur_l = main_j.s0 as i32;
                                            main_i = FONT_CHARACTER_INFO(
                                                main_f,
                                                effective_char(true, main_f, cur_l as u16) as usize,
                                            );
                                            ligature_present = true;
                                            current_block = 5062343687657450649;
                                        }
                                        2 | 6 => {
                                            cur_r = main_j.s0 as i32;
                                            if let Some(ls) = lig_stack {
                                                if is_char_node(Some(ls)) {
                                                    let main_p = ls;
                                                    let ls = new_lig_item(cur_r as u16);
                                                    lig_stack = Some(ls);
                                                    MEM[ls + 1].b32.s1 = Some(main_p).tex_int();
                                                } else {
                                                    MEM[ls].b16.s0 = cur_r as u16
                                                }
                                            } else {
                                                lig_stack = Some(new_lig_item(cur_r as u16));
                                                bchar = TOO_BIG_CHAR
                                            }
                                            current_block = 5062343687657450649;
                                        }
                                        3 => {
                                            cur_r = main_j.s0 as i32;
                                            let main_p = lig_stack;
                                            let ls = new_lig_item(cur_r as u16);
                                            lig_stack = Some(ls);
                                            *LLIST_link(ls) = main_p.tex_int();
                                            current_block = 5062343687657450649;
                                        }
                                        7 | 11 => {
                                            if cur_l < TOO_BIG_CHAR {
                                                if LLIST_link(cur_q).opt().is_some()
                                                    && MEM[cur_list.tail].b16.s0 as i32
                                                        == HYPHEN_CHAR[main_f as usize]
                                                {
                                                    ins_disc = true;
                                                }
                                                if ligature_present {
                                                    let main_p = new_ligature(
                                                        main_f,
                                                        cur_l as u16,
                                                        llist_link(cur_q),
                                                    );
                                                    if lft_hit {
                                                        MEM[main_p].b16.s0 = 2;
                                                        lft_hit = false
                                                    }
                                                    *LLIST_link(cur_q) = Some(main_p).tex_int();
                                                    cur_list.tail = main_p;
                                                    ligature_present = false
                                                }
                                                if ins_disc {
                                                    ins_disc = false;
                                                    if !cur_list.mode.0 {
                                                        let d = new_disc();
                                                        *LLIST_link(cur_list.tail) =
                                                            Some(d).tex_int();
                                                        cur_list.tail = d;
                                                    }
                                                }
                                            }
                                            cur_q = cur_list.tail;
                                            cur_l = main_j.s0 as i32;
                                            main_i = FONT_CHARACTER_INFO(
                                                main_f,
                                                effective_char(true, main_f, cur_l as u16) as usize,
                                            );
                                            ligature_present = true;
                                            current_block = 5062343687657450649;
                                        }
                                        _ => {
                                            cur_l = main_j.s0 as i32;
                                            ligature_present = true;
                                            if lig_stack.is_none() {
                                                current_block = 7236688557761431611;
                                            } else {
                                                current_block = 4014385708774270501;
                                            }
                                        }
                                    }
                                    match current_block {
                                        7236688557761431611 => {}
                                        4014385708774270501 => {}
                                        _ => {
                                            if main_j.s1 > 4 {
                                                if main_j.s1 != 7 {
                                                    current_block = 7236688557761431611;
                                                } else {
                                                    current_block = 17785146416239343017;
                                                }
                                            } else {
                                                current_block = 17785146416239343017;
                                            }
                                            match current_block {
                                                7236688557761431611 => {}
                                                _ => {
                                                    if cur_l < TOO_BIG_CHAR {
                                                        current_block = 4700797278417140031;
                                                    } else {
                                                        main_k = BCHAR_LABEL[main_f as usize];
                                                        current_block = 13962460947151495567;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                current_block = 17910696963991344696;
                            }
                        } else {
                            current_block = 17910696963991344696;
                        }
                        match current_block {
                            2772858075894446251 => {}
                            7236688557761431611 => {}
                            4014385708774270501 => {}
                            4700797278417140031 => {}
                            _ => {
                                if main_j.s3 == 0 {
                                    main_k += 1;
                                    current_block = 13962460947151495567;
                                    break;
                                } else if main_j.s3 < 128 {
                                    main_k = main_k + main_j.s3 as i32 + 1;
                                    current_block = 13962460947151495567;
                                    break;
                                }
                                current_block = 7236688557761431611;
                            }
                        }
                    }
                    _ => {
                        // lab100:
                        /*main_loop_lookahead *//*1073: */
                        let mut lab101 = true;
                        let (cmd, chr, cs) = get_next(input);
                        cur_cmd = cmd;
                        cur_chr = chr;
                        cur_cs = cs;
                        match cur_cmd {
                            Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {}
                            _ => {
                                cur_tok = x_token(input, &mut cur_cmd, &mut cur_chr, &mut cur_cs);
                                match cur_cmd {
                                    Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {}
                                    Cmd::CharNum => {
                                        cur_chr = scan_char_num(input);
                                    }
                                    _ => {
                                        if cur_cmd == Cmd::NoBoundary {
                                            bchar = TOO_BIG_CHAR;
                                        }
                                        cur_r = bchar;
                                        lig_stack = None;
                                        lab101 = false;
                                    }
                                }
                            }
                        }
                        if lab101 {
                            // lab101:
                            /*main_loop_lookahead 1 */
                            main_s = (*SF_CODE(cur_chr as usize) as i64 % 65536) as i32; /*:1073 */
                            if main_s == 1000 {
                                cur_list.aux.b32.s0 = 1000
                            } else if main_s < 1000 {
                                if main_s > 0 {
                                    cur_list.aux.b32.s0 = main_s;
                                }
                            } else if cur_list.aux.b32.s0 < 1000 {
                                cur_list.aux.b32.s0 = 1000;
                            } else {
                                cur_list.aux.b32.s0 = main_s;
                            }
                            cur_ptr = None;
                            space_class = (*SF_CODE(cur_chr as usize) as i64 / 65536) as i32;
                            if get_int_par(IntPar::xetex_inter_char_tokens) > 0
                                && space_class != CHAR_CLASS_LIMIT
                            {
                                if prev_class == CHAR_CLASS_LIMIT - 1 {
                                    if input.state != InputState::TokenList
                                        || input.index != Btl::BackedUpChar
                                    {
                                        find_sa_element(
                                            ValLevel::InterChar,
                                            (CHAR_CLASS_LIMIT - 1) * CHAR_CLASS_LIMIT + space_class,
                                            false,
                                        );
                                        if let Some(c) = cur_ptr {
                                            if cur_cmd != Cmd::Letter {
                                                cur_cmd = Cmd::OtherChar
                                            }
                                            let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                            back_input(input, tok);
                                            input.index = Btl::BackedUpChar;
                                            begin_token_list(
                                                input,
                                                MEM[c + 1].b32.s1.opt(),
                                                Btl::InterCharText,
                                            );
                                            continue 'big_switch;
                                        }
                                    }
                                } else {
                                    find_sa_element(
                                        ValLevel::InterChar,
                                        prev_class * CHAR_CLASS_LIMIT + space_class,
                                        false,
                                    );
                                    if let Some(c) = cur_ptr {
                                        if cur_cmd != Cmd::Letter {
                                            cur_cmd = Cmd::OtherChar;
                                        }
                                        let tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                        back_input(input, tok);
                                        input.index = Btl::BackedUpChar;
                                        begin_token_list(
                                            input,
                                            MEM[c + 1].b32.s1.opt(),
                                            Btl::InterCharText,
                                        );
                                        prev_class = CHAR_CLASS_LIMIT - 1;
                                        continue 'big_switch;
                                    }
                                }
                                prev_class = space_class
                            }
                            let mut ls = Char(fast_get_avail());
                            lig_stack = Some(ls.ptr());
                            ls.set_font(main_f as u16);
                            cur_r = cur_chr;
                            ls.set_character(cur_r as u16);
                            if cur_r == false_bchar {
                                cur_r = TOO_BIG_CHAR;
                            }
                        }
                        current_block = 4700797278417140031; // lab110
                    }
                }
                loop {
                    match current_block {
                        7236688557761431611 => {
                            // lab80:
                            /*main_loop_wrapup *//*1070: */
                            if cur_l < TOO_BIG_CHAR {
                                if LLIST_link(cur_q).opt().is_some()
                                    && MEM[cur_list.tail].b16.s0 as i32
                                        == HYPHEN_CHAR[main_f as usize]
                                {
                                    ins_disc = true;
                                }
                                if ligature_present {
                                    let main_p =
                                        new_ligature(main_f, cur_l as u16, llist_link(cur_q));
                                    if lft_hit {
                                        MEM[main_p].b16.s0 = 2;
                                        lft_hit = false
                                    }
                                    if rt_hit && lig_stack.is_none() {
                                        MEM[main_p].b16.s0 += 1;
                                        rt_hit = false;
                                    }
                                    *LLIST_link(cur_q) = Some(main_p).tex_int();
                                    cur_list.tail = main_p;
                                    ligature_present = false
                                }
                                if ins_disc {
                                    ins_disc = false;
                                    if !cur_list.mode.0 {
                                        let d = new_disc();
                                        *LLIST_link(cur_list.tail) = Some(d).tex_int();
                                        cur_list.tail = d;
                                    }
                                }
                            }
                            current_block = 2772858075894446251;
                        }
                        4700797278417140031 =>
                        /*main_lig_loop *//*1074: */
                        {
                            if main_i.s1 % 4 != LIG_TAG as u16 {
                                current_block = 7236688557761431611;
                                continue;
                            }
                            if cur_r == TOO_BIG_CHAR {
                                current_block = 7236688557761431611;
                            } else {
                                break;
                            }
                        }
                        2772858075894446251 => {
                            // lab90:
                            /*main_loop_move *//*1071: */
                            if lig_stack.is_none() {
                                big_switch = false;
                                continue 'big_switch;
                            }
                            cur_q = cur_list.tail;
                            cur_l = MEM[lig_stack.unwrap()].b16.s0 as i32;
                            current_block = 4014385708774270501;
                        }
                        _ => {
                            // lab91:
                            /*main_loop_move 1 */
                            if is_char_node(lig_stack) {
                                current_block = 249799543778823886; // lab92
                                break 'c_125244;
                            }
                            // lab95:
                            /*main_loop_move_lig *//*1072: */
                            let ls = lig_stack.unwrap();
                            let main_p = MEM[ls + 1].b32.s1.opt();
                            if let Some(main_p) = main_p {
                                *LLIST_link(cur_list.tail) = Some(main_p).tex_int();
                                cur_list.tail = *LLIST_link(cur_list.tail) as usize;
                            }
                            let tmp_ptr = ls;
                            lig_stack = llist_link(tmp_ptr);
                            free_node(tmp_ptr, SMALL_NODE_SIZE);
                            main_i = FONT_CHARACTER_INFO(
                                main_f,
                                effective_char(true, main_f, cur_l as u16) as usize,
                            );
                            ligature_present = true;
                            if let Some(ls) = lig_stack {
                                cur_r = MEM[ls].b16.s0 as i32;
                                current_block = 4700797278417140031;
                            } else {
                                if main_p.is_some() {
                                    current_block = 18270385712206273994; // lab100
                                    continue 'c_125244;
                                }
                                cur_r = bchar;
                                current_block = 4700797278417140031;
                            }
                        }
                    }
                }
                main_k = LIG_KERN_BASE[main_f as usize] + main_i.s0 as i32;
                main_j = FONT_INFO[main_k as usize].b16;
                if main_j.s3 as i32 <= 128 {
                    current_block = 11331079115679122507; // lab112
                    continue;
                }
                main_k = ((LIG_KERN_BASE[main_f as usize]
                    + 256 * main_j.s1 as i32
                    + main_j.s0 as i32) as i64
                    + 32768
                    - (256 * 128) as i64) as font_index;
                current_block = 13962460947151495567; // lab111
                break;
            }
        }
    }

    unsafe fn append_normal_space(input: &mut input_state_t, mut cmd: Cmd, chr: i32, cs: i32) {
        if get_int_par(IntPar::xetex_inter_char_tokens) > 0
            && space_class != CHAR_CLASS_LIMIT
            && prev_class != CHAR_CLASS_LIMIT - 1
        {
            prev_class = CHAR_CLASS_LIMIT - 1;
            find_sa_element(
                ValLevel::InterChar,
                space_class * CHAR_CLASS_LIMIT + (CHAR_CLASS_LIMIT - 1),
                false,
            );
            if let Some(c) = cur_ptr {
                let tok = if cs == 0 {
                    if cmd == Cmd::CharNum {
                        cmd = Cmd::OtherChar;
                    }
                    cmd as i32 * MAX_CHAR_VAL + chr
                } else {
                    CS_TOKEN_FLAG + cs
                };
                back_input(input, tok);
                begin_token_list(input, MEM[c + 1].b32.s1.opt(), Btl::InterCharText);
                return;
            }
        }
        let tmp = if get_glue_par(GluePar::space_skip).ptr() == 0 {
            let main_p = FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize]
                .opt()
                .map(GlueSpec)
                .unwrap_or_else(|| {
                    let mut main_p = new_spec(&ZERO_GLUE);
                    main_k = PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize] + 2;
                    main_p
                        .set_size(Scaled(FONT_INFO[main_k as usize].b32.s1))
                        .set_stretch(Scaled(FONT_INFO[(main_k + 1) as usize].b32.s1))
                        .set_shrink(Scaled(FONT_INFO[(main_k + 2) as usize].b32.s1));
                    FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize] = Some(main_p.ptr()).tex_int();
                    main_p
                });
            new_glue(&main_p)
        } else {
            new_param_glue(GluePar::space_skip)
        };
        *LLIST_link(cur_list.tail) = Some(tmp.ptr()).tex_int();
        cur_list.tail = tmp.ptr();
    }
}
pub(crate) unsafe fn give_err_help() {
    token_show(LOCAL(Local::err_help).opt());
}
pub(crate) unsafe fn close_files_and_terminate() {
    terminate_font_manager();
    for k in 0..=15 {
        if write_open[k] {
            ttstub_output_close(write_file[k].take().unwrap());
        }
    }
    finalize_dvi_file();
    synctex_terminate(log_opened);
    if log_opened {
        writeln!(log_file.as_mut().unwrap()).unwrap();
        ttstub_output_close(log_file.take().unwrap());
        log_file = None;
        match selector {
            Selector::LOG_ONLY => selector = Selector::NO_PRINT,
            Selector::TERM_AND_LOG => {
                selector = Selector::TERM_ONLY;
                t_print_nl!(
                    "Transcript written on {}.",
                    PoolString::from(texmf_log_name)
                );
            }
            _ => unreachable!(),
        }
    }
    print_ln();
}
pub(crate) unsafe fn scan_pdf_ext_toks(input: &mut input_state_t, cs: i32) {
    scan_toks(input, cs, false, true);
}
pub(crate) unsafe fn compare_strings(input: &mut input_state_t, cs: i32) -> i32 {
    scan_toks(input, cs, false, true);
    let s1 = format!("{}", TokenList(LLIST_link(def_ref).opt()));
    delete_token_ref(def_ref);
    scan_toks(input, cs, false, true);
    let s2 = format!("{}", TokenList(LLIST_link(def_ref).opt()));
    delete_token_ref(def_ref);
    match s1.cmp(&s2) {
        Ordering::Equal => 0,
        Ordering::Greater => 1,
        Ordering::Less => -1,
    }
}
pub(crate) unsafe fn prune_page_top(mut popt: Option<usize>, s: bool) -> i32 {
    let mut r: i32 = None.tex_int();
    let mut prev_p = TEMP_HEAD;
    *LLIST_link(TEMP_HEAD) = popt.tex_int();
    while let Some(p) = popt {
        match TxtNode::from(p) {
            TxtNode::List(b) => {
                let (q, mut tmp_ptr) = new_skip_param(GluePar::split_top_skip);
                *LLIST_link(prev_p) = Some(q.ptr()).tex_int();
                *LLIST_link(q.ptr()) = Some(b.ptr()).tex_int();
                if tmp_ptr.size() > b.height() {
                    tmp_ptr.set_size(tmp_ptr.size() - b.height());
                } else {
                    tmp_ptr.set_size(Scaled::ZERO);
                }
                popt = None;
            }
            TxtNode::Rule(r) => {
                let (q, mut tmp_ptr) = new_skip_param(GluePar::split_top_skip);
                *LLIST_link(prev_p) = Some(q.ptr()).tex_int();
                *LLIST_link(q.ptr()) = Some(r.ptr()).tex_int();
                if tmp_ptr.size() > r.height() {
                    tmp_ptr.set_size(tmp_ptr.size() - r.height());
                } else {
                    tmp_ptr.set_size(Scaled::ZERO);
                }
                popt = None;
            }
            TxtNode::WhatsIt(_) | TxtNode::Mark(_) | TxtNode::Ins(_) => {
                prev_p = p;
                popt = llist_link(prev_p);
            }
            TxtNode::Glue(_) | TxtNode::Kern(_) | TxtNode::Penalty(_) => {
                let q = p;
                popt = llist_link(q);
                *LLIST_link(q) = None.tex_int();
                *LLIST_link(prev_p) = popt.tex_int();
                if s {
                    if disc_ptr[VSPLIT_CODE as usize].opt().is_none() {
                        disc_ptr[VSPLIT_CODE as usize] = q as i32;
                    } else {
                        *LLIST_link(r as usize) = Some(q).tex_int();
                    }
                    r = q as i32;
                } else {
                    flush_node_list(Some(q));
                }
            }
            _ => confusion("pruning"),
        }
    }
    *LLIST_link(TEMP_HEAD)
}
pub(crate) unsafe fn do_marks(a: MarkMode, l: i16, q: usize) -> bool {
    if l < 4 {
        let mut qi = Index(q);
        for i in &mut qi.indexes_mut()[0..16] {
            cur_ptr = (*i).opt();
            if let Some(p) = cur_ptr {
                if do_marks(a, l + 1, p) {
                    *i = None.tex_int();
                    MEM[q].b16.s0 -= 1; //q.rc_dec();
                }
            }
        }
        if qi.rc() == 0 {
            qi.free();
            return true;
        }
    } else {
        let mut q = MarkClass(q);
        match a {
            MarkMode::VSplitInit => {
                /*1614: */
                let qi = q.indexes_mut();
                if let (Some(qi3), Some(qi4)) = (qi[3].opt(), qi[4].opt()) {
                    delete_token_ref(qi3);
                    qi[3] = None.tex_int();
                    delete_token_ref(qi4);
                    qi[4] = None.tex_int()
                }
            }
            MarkMode::FireUpInit => {
                let qi = q.indexes_mut();
                if let (Some(qi1), Some(qi2)) = (qi[1].opt(), qi[2].opt()) {
                    if let Some(qi0) = qi[0].opt() {
                        delete_token_ref(qi0);
                    }
                    delete_token_ref(qi1);
                    qi[1] = None.tex_int();
                    if MEM[qi2].b32.s1.opt().is_none() {
                        delete_token_ref(qi2);
                        qi[2] = None.tex_int()
                    } else {
                        MEM[qi2].b32.s0 += 1;
                    }
                    qi[0] = qi[2];
                }
            }
            MarkMode::FireUpDone => {
                let qi = q.indexes_mut();
                if let Some(qi0) = qi[0].opt() {
                    if qi[1].opt().is_none() {
                        qi[1] = Some(qi0).tex_int();
                        MEM[qi0].b32.s0 += 1;
                    }
                }
            }
            MarkMode::DestroyMarks => {
                for i in q.indexes_mut() {
                    cur_ptr = (*i).opt();
                    if let Some(c) = cur_ptr {
                        delete_token_ref(c);
                        *i = None.tex_int();
                    }
                }
            }
        }
        let qi = q.indexes();
        if qi[2].opt().is_none() && qi[4].opt().is_none() {
            free_node(q.ptr(), MARK_CLASS_NODE_SIZE);
            return true;
        }
    }
    false
}
pub(crate) unsafe fn do_assignments(input: &mut input_state_t) -> (i32, Cmd, i32) {
    loop {
        let mut next;
        loop {
            next = get_x_token(input);
            if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
                break;
            }
        }
        let tok = next.0;
        let cmd = next.1;
        let chr = next.2;
        let cs = next.3;
        if cmd <= MAX_NON_PREFIXED_COMMAND {
            return (tok, cmd, chr);
        }
        set_box_allowed = false;
        prefixed_command(input, cmd, chr, cs);
        set_box_allowed = true
    }
}

/// saves the procedure-call
/// overhead at the expense of extra programming. This routine is used in
/// the places that would otherwise account for the most calls of `get_avail`
unsafe fn fast_get_avail() -> usize {
    // avoid `get_avail` if possible, to save time
    if let Some(a) = avail {
        avail = llist_link(a);
        *LLIST_link(a) = None.tex_int();
        a
    } else {
        get_avail()
    }
}

unsafe fn store_new_token(p: &mut usize, val: i32) {
    let q = get_avail();
    *LLIST_link(*p) = Some(q).tex_int();
    *LLIST_info(q) = val;
    *p = q; // link(p) `null`
}

unsafe fn fast_store_new_token(p: &mut usize, val: i32) {
    let q = fast_get_avail();
    *LLIST_link(*p) = Some(q).tex_int();
    *LLIST_info(q) = val;
    *p = q; // link(p) `null`
}

unsafe fn eq_type(cs: usize) -> Cmd {
    Cmd::from(EQTB[cs].cmd)
}
