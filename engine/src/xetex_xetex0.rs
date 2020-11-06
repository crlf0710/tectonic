#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use std::ffi::CString;
use std::io::Write;
use std::ptr;

use super::xetex_ini::{input_state_t, EqtbWord, Selector};
use super::xetex_io::{bytesFromUTF8, name_of_input_file, offsetsFromUTF8, u_open_in};
use crate::cmd::*;
use crate::core_memory::{mfree, xmalloc_array, xrealloc};
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
    gr_print_font_name, linebreak_next, linebreak_start, map_char_to_glyph, map_glyph_to_index,
    ot_font_get, ot_font_get_1, ot_font_get_2, ot_font_get_3, print_glyph_name, print_utf8_str,
    Font, NativeFont, NativeFont::*,
};
use crate::xetex_ini::FONT_LETTER_SPACE;
use crate::xetex_ini::{
    _xeq_level_array, active_width, adjust_tail, after_token, align_ptr, align_state, arith_error,
    avail, bchar, best_height_plus_depth, breadth_max, cancel_boundary, cond_ptr, cur_align,
    cur_area, cur_boundary, cur_box, cur_chr, cur_cmd, cur_cs, cur_dir, cur_ext, cur_group,
    cur_head, cur_if, cur_input, cur_l, cur_lang, cur_level, cur_list, cur_loop, cur_mark,
    cur_name, cur_order, cur_pre_head, cur_pre_tail, cur_ptr, cur_q, cur_r, cur_span, cur_tail,
    cur_tok, dead_cycles, def_ref, deletions_allowed, depth_threshold, dig, disc_ptr, error_count,
    error_line, expand_depth, expand_depth_count, false_bchar, file_line_error_style_p,
    file_offset, first, first_count, fmem_ptr, font_in_short_display, force_eof,
    gave_char_warning_help, half_error_line, hash, hash_extra, hash_high, hash_used, hi_mem_min,
    history, if_limit, if_line, init_pool_ptr, init_str_ptr, ins_disc, insert_penalties,
    insert_src_special_auto, insert_src_special_every_par, insert_src_special_every_vbox,
    interaction, is_hyph, is_in_csname, job_name, last, last_badness, last_glue, last_kern,
    last_leftmost_char, last_node_type, last_penalty, last_rightmost_char, lft_hit, lig_stack,
    ligature_present, line, lo_mem_max, log_file, log_opened, long_help_seen, long_state, mag_set,
    main_f, main_h, main_i, main_j, main_k, main_s, mapped_text, max_buf_stack, max_print_line,
    max_reg_help_line, max_reg_num, max_strings, mem_end, name_in_progress, name_of_file,
    native_len, native_text, native_text_size, no_new_control_sequence, open_parens, output_active,
    pack_begin_line, page_contents, page_so_far, page_tail, par_loc, par_token, pdf_last_x_pos,
    pdf_last_y_pos, pool_ptr, pool_size, pre_adjust_tail, prev_class, prim, prim_eqtb, prim_used,
    pseudo_files, pstack, read_file, read_open, rover, rt_hit, rust_stdout, sa_chain, sa_level,
    sa_root, save_native_len, scanner_status, selector, set_box_allowed, shown_mode, skip_line,
    space_class, stop_at_space, str_pool, str_ptr, str_start, tally, term_offset, texmf_log_name,
    total_shrink, total_stretch, trick_buf, trick_count, use_err_help, used_tectonic_coda_tokens,
    warning_index, write_file, write_open, xtx_ligature_present, LR_problems, LR_ptr, BCHAR_LABEL,
    BUFFER, BUF_SIZE, CHAR_BASE, EOF_SEEN, EQTB, EQTB_TOP, FONT_AREA, FONT_BC, FONT_BCHAR,
    FONT_DSIZE, FONT_EC, FONT_FALSE_BCHAR, FONT_GLUE, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_MAPPING,
    FONT_MAX, FONT_MEM_SIZE, FONT_NAME, FONT_PARAMS, FONT_PTR, FONT_SIZE,
    FULL_SOURCE_FILENAME_STACK, GRP_STACK, HYPHEN_CHAR, IF_STACK, INPUT_FILE, INPUT_PTR,
    INPUT_STACK, IN_OPEN, KERN_BASE, LIG_KERN_BASE, LINE_STACK, MAX_IN_OPEN, MAX_IN_STACK,
    MAX_NEST_STACK, MAX_PARAM_STACK, MAX_SAVE_STACK, MEM, NEST, NEST_PTR, NEST_SIZE, PARAM_BASE,
    PARAM_PTR, PARAM_SIZE, PARAM_STACK, SAVE_PTR, SAVE_SIZE, SAVE_STACK, SKEW_CHAR,
    SOURCE_FILENAME_STACK, STACK_SIZE,
};
use crate::xetex_ini::{b16x4, b32x2, memory_word, prefixed_command};
use crate::xetex_io::{input_line, open_or_close_in, set_input_file_encoding};
use crate::xetex_layout_interface::*;
use crate::xetex_linebreak::line_break;
use crate::xetex_math::{
    after_math, append_choices, build_choices, fin_mlist, flush_math, init_math, math_ac,
    math_fraction, math_left_right, math_limit_switch, math_radical, resume_after_display,
    start_eq_no, sub_sup,
};
use crate::xetex_output::{
    print, print_char, print_chr, print_cs, print_cstr, print_current_string, print_esc,
    print_esc_cstr, print_file_line, print_file_name, print_hex, print_int, print_ln,
    print_native_word, print_nl, print_nl_cstr, print_raw_char, print_roman_int, print_sa_num,
    print_scaled, print_size, print_write_whatsit, sprint_cs,
};
use crate::xetex_pagebuilder::build_page;
use crate::xetex_pic::{count_pdf_file_pages, load_picture};
use crate::xetex_scaledmath::{
    mult_and_add, round_xn_over_d, tex_round, x_over_n, xn_over_d, Scaled,
};
use crate::xetex_shipout::{finalize_dvi_file, new_edge, out_what, ship_out};
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_stringpool::{
    append_str, length, make_string, search_string, slow_make_string, str_eq_buf, PoolString,
};
use crate::xetex_synctex::{synctex_start_input, synctex_terminate};
use crate::xetex_texmfmp::{
    getmd5sum, gettexstring, is_new_source, make_src_special, maketexstring, remember_source_info,
};
use crate::xetex_xetexd::*;
use bridge::{ttstub_issue_warning, ttstub_output_close, ttstub_output_open, ttstub_output_putc};

use bridge::{TTHistory, TTInputFormat};

use libc::{memcpy, strlen};

pub(crate) type CFDictionaryRef = *mut libc::c_void;

pub(crate) type UTF16_code = u16;
pub(crate) type UTF8_code = u8;
pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
pub(crate) type glue_ord = u8;
pub(crate) type group_code = u8;
pub(crate) type internal_font_number = usize;
pub(crate) type font_index = i32;
pub(crate) type nine_bits = i32;
pub(crate) type save_pointer = i32;

#[inline]
pub(crate) unsafe fn cur_length() -> pool_pointer {
    pool_ptr - str_start[(str_ptr - 65536) as usize]
}

fn IS_LC_HEX(c: i32) -> bool {
    (c >= ('0' as i32) && c <= ('9' as i32)) || (c >= ('a' as i32) && c <= ('f' as i32))
}

unsafe fn int_error(mut n: i32) {
    print_cstr(" (");
    print_int(n);
    print_chr(')');
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

/*:112*/
/*118:*/
pub(crate) unsafe fn show_token_list(mut popt: Option<usize>, q: Option<usize>, mut l: i32) {
    let mut n: UTF16_code = 0;
    let mut match_chr = '#' as i32;
    n = '0' as UTF16_code;
    tally = 0;
    while let Some(p) = popt {
        if !(tally < l) {
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
        if p < hi_mem_min as usize || p > mem_end as usize {
            print_esc_cstr("CLOBBERED.");
            return;
        }
        if MEM[p as usize].b32.s0 >= CS_TOKEN_FLAG {
            print_cs(MEM[p as usize].b32.s0 - CS_TOKEN_FLAG);
        } else {
            let m = Cmd::from((MEM[p as usize].b32.s0 / MAX_CHAR_VAL) as u16);
            let c = MEM[p as usize].b32.s0 % MAX_CHAR_VAL;
            if MEM[p as usize].b32.s0 < 0 {
                print_esc_cstr("BAD.");
            } else {
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
                    | Cmd::OtherChar => print_char(c),
                    Cmd::MacParam => {
                        print_char(c);
                        print_char(c);
                    }
                    OUT_PARAM => {
                        print_char(match_chr);
                        if c <= 0x9 {
                            print_char(c + '0' as i32);
                        } else {
                            print_chr('!');
                            return;
                        }
                    }
                    MATCH => {
                        match_chr = c;
                        print_char(c);
                        n += 1;
                        print_char(n as i32);
                        if n as i32 > '9' as i32 {
                            return;
                        }
                    }
                    END_MATCH => {
                        if c == 0 {
                            print_cstr("->");
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
pub(crate) unsafe fn runaway() {
    if scanner_status != ScannerStatus::Normal && scanner_status != ScannerStatus::Skipping {
        let p = match scanner_status {
            ScannerStatus::Defining => {
                print_nl_cstr("Runaway definition");
                def_ref
            }
            ScannerStatus::Matching => {
                print_nl_cstr("Runaway argument");
                TEMP_HEAD
            }
            ScannerStatus::Aligning => {
                print_nl_cstr("Runaway preamble");
                HOLD_HEAD
            }
            ScannerStatus::Absorbing => {
                print_nl_cstr("Runaway text");
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
pub(crate) unsafe fn get_node(mut s: i32) -> usize {
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
            if r == p {
                if MEM[(p + 1) as usize].b32.s1 != p {
                    /*133: */
                    rover = MEM[(p + 1) as usize].b32.s1;
                    let t = MEM[(p + 1) as usize].b32.s0;
                    MEM[(rover + 1) as usize].b32.s0 = t;
                    MEM[(t + 1) as usize].b32.s1 = rover;
                    return found(r as usize, s as usize);
                }
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
        if lo_mem_max + 2 < hi_mem_min {
            if lo_mem_max + 2 <= MAX_HALFWORD {
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
pub(crate) unsafe fn free_node(p: usize, mut s: i32) {
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
pub(crate) unsafe fn new_ligature(mut f: internal_font_number, mut c: u16, mut q: i32) -> usize {
    let mut p = Ligature(get_node(SMALL_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Ligature);
    p.set_font(f as u16)
        .set_char(c)
        .set_lig_ptr(q)
        .set_hits(false, false);
    p.ptr()
}
pub(crate) unsafe fn new_lig_item(mut c: u16) -> usize {
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
    let mut glyph_count: i32 = 0;
    if !src.glyph_info_ptr().is_null() {
        glyph_count = src.glyph_count() as i32;
        dest.set_glyph_info_ptr(xmalloc_array::<libc::c_char>(
            glyph_count as usize * NATIVE_GLYPH_INFO_SIZE as usize,
        ) as *mut _);
        memcpy(
            dest.glyph_info_ptr(),
            src.glyph_info_ptr(),
            (glyph_count * NATIVE_GLYPH_INFO_SIZE) as usize,
        );
        dest.set_glyph_count(glyph_count as u16);
    };
}
pub(crate) unsafe fn new_math(w: Scaled, s: MathType) -> usize {
    let mut p = Math(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Math);
    p.set_subtype(s).set_width(w);
    p.ptr()
}
pub(crate) unsafe fn new_spec(other: usize) -> usize {
    let other = GlueSpec(other);
    let mut q = GlueSpec(get_node(GLUE_SPEC_SIZE));
    MEM[q.ptr()] = MEM[other.ptr()];
    q.rc_none();
    q.set_size(other.size())
        .set_stretch(other.stretch())
        .set_shrink(other.shrink());
    q.ptr()
}
pub(crate) unsafe fn new_param_glue(n: GluePar) -> usize {
    let mut p = Glue(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Glue);
    p.set_param(n as u16 + 1).set_leader_ptr(None.tex_int());
    let q = EQTB[GLUE_BASE + n as usize].val as usize;
    p.set_glue_ptr(q as i32);
    GlueSpec(q).rc_inc();
    p.ptr()
}
pub(crate) unsafe fn new_glue(q: usize) -> usize {
    let mut p = Glue(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Glue);
    p.set_param(NORMAL as _)
        .set_leader_ptr(None.tex_int())
        .set_glue_ptr(q as i32);
    GlueSpec(q).rc_inc();
    p.ptr()
}
pub(crate) unsafe fn new_skip_param(n: GluePar) -> (usize, GlueSpec) {
    let mut tmp_ptr = GlueSpec(new_spec(
        EQTB[(GLUE_BASE as i32 + n as i32) as usize].val as usize,
    )); // 232
    let p = new_glue(tmp_ptr.ptr());
    tmp_ptr.rc_none();
    MEM[p].b16.s0 = n as u16 + 1;
    (p, tmp_ptr)
}
pub(crate) unsafe fn new_kern(w: Scaled) -> usize {
    let mut p = Kern(get_node(MEDIUM_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Kern);
    p.set_subtype(KernType::Normal).set_width(w);
    p.ptr()
}
pub(crate) unsafe fn new_penalty(mut m: i32) -> usize {
    let p = get_node(MEDIUM_NODE_SIZE);
    set_NODE_type(p, TextNode::Penalty);
    MEM[p].b16.s0 = 0;
    MEM[p + 1].b32.s1 = m;
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
                        print_chr('*');
                    } else {
                        /*279:*/
                        print_esc(
                            (*hash.offset((FONT_ID_BASE as i32 + p.font() as i32) as isize)).s1,
                        );
                    }
                    print_chr(' ');
                    font_in_short_display = p.font() as usize
                }
                print(p.character() as i32);
            }
        } else {
            /*183:*/
            match TxtNode::from(p) {
                TxtNode::List(_)
                | TxtNode::Ins(_)
                | TxtNode::Mark(_)
                | TxtNode::Adjust(_)
                | TxtNode::Unset(_) => print_cstr("[]"),
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::NativeWord(nw) => {
                        if nw.font() as usize != font_in_short_display {
                            print_esc(
                                (*hash.offset((FONT_ID_BASE as i32 + nw.font() as i32) as isize))
                                    .s1,
                            );
                            print_chr(' ');
                            font_in_short_display = nw.font() as usize
                        }
                        print_native_word(&nw);
                    }
                    _ => print_cstr("[]"),
                },
                TxtNode::Rule(_) => print_chr('|'),
                TxtNode::Glue(_) => {
                    if MEM[p + 1].b32.s0 != 0 {
                        // TODO: strange (special case?)
                        print_chr(' ');
                    }
                }
                TxtNode::Math(m) => match m.subtype() {
                    MathType::Eq(_, MathMode::Left) | MathType::Eq(_, MathMode::Right) => {
                        print_cstr("[]")
                    }
                    _ => print_chr('$'),
                },
                TxtNode::Ligature(l) => short_display(l.lig_ptr().opt()),
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
pub(crate) unsafe fn print_font_and_char(p: usize) {
    if p > mem_end as usize {
        print_esc_cstr("CLOBBERED.");
    } else {
        if MEM[p].b16.s1 as i32 > FONT_MAX as i32 {
            print_chr('*');
        } else {
            /*279: */
            print_esc((*hash.offset((FONT_ID_BASE as i32 + MEM[p].b16.s1 as i32) as isize)).s1);
        }
        print_chr(' ');
        print(MEM[p].b16.s0 as i32);
    };
}
pub(crate) unsafe fn print_mark(mut p: i32) {
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
        print_chr('*');
    } else {
        print_scaled(d);
    };
}
pub(crate) unsafe fn print_glue(d: Scaled, order: GlueOrder, s: &str) {
    print_scaled(d);
    match order {
        // TODO: optimize
        GlueOrder::Incorrect => print_cstr("foul"),
        GlueOrder::Fil => print_cstr("fil"),
        GlueOrder::Fill => print_cstr("fill"),
        GlueOrder::Filll => print_cstr("filll"),
        GlueOrder::Normal => print_cstr(s),
    }
}
pub(crate) unsafe fn print_spec(p: i32, unit: &str) {
    if p < 0 || p >= lo_mem_max {
        print_chr('*');
    } else {
        let p = GlueSpec(p as usize);
        print_scaled(p.size());
        if !unit.is_empty() {
            print_cstr(unit);
        }
        if p.stretch() != Scaled::ZERO {
            print_cstr(" plus ");
            print_glue(p.stretch(), p.stretch_order(), unit);
        }
        if p.shrink() != Scaled::ZERO {
            print_cstr(" minus ");
            print_glue(p.shrink(), p.shrink_order(), unit);
        }
    };
}
pub(crate) unsafe fn print_fam_and_char(p: usize) {
    let mut c: i32 = 0;
    print_esc_cstr("fam");
    print_int(MEM[p].b16.s1 as i32 % 256 % 256);
    print_chr(' ');
    c = (MEM[p].b16.s0 as i64 + (MEM[p].b16.s1 as i32 / 256) as i64 * 65536) as i32;
    if (c as i64) < 65536 {
        print(c);
    } else {
        print_char(c);
    };
}
pub(crate) unsafe fn print_delimiter(d: &Delimeter) {
    let mut a: i32 = 0;
    a = ((d.s3 as i32 % 256 * 256) as i64 + (d.s2 as i64 + (d.s3 as i32 / 256) as i64 * 65536))
        as i32;
    a = ((a * 4096 + d.s1 as i32 % 256 * 256) as i64
        + (d.s0 as i64 + (d.s1 as i32 / 256) as i64 * 65536)) as i32;
    if a < 0 {
        print_int(a);
    } else {
        print_hex(a);
    };
}
pub(crate) unsafe fn print_subsidiary_data(p: usize, mut c: UTF16_code) {
    if cur_length() >= depth_threshold {
        if MEM[p as usize].b32.s1 != 0 {
            print_cstr(" []");
        }
    } else {
        str_pool[pool_ptr as usize] = c;
        pool_ptr += 1;
        let tmp_ptr = p;
        match MathCell::n(MEM[p].b32.s1).unwrap() {
            MathCell::MathChar => {
                print_ln();
                print_current_string();
                print_fam_and_char(p);
            }
            MathCell::SubBox => show_info(tmp_ptr),
            MathCell::SubMList => {
                if MEM[p].b32.s0.opt().is_none() {
                    print_ln();
                    print_current_string();
                    print_cstr("{}");
                } else {
                    show_info(tmp_ptr);
                }
            }
            _ => {}
        }
        pool_ptr -= 1
    };
}
pub(crate) unsafe fn print_style(c: i32) {
    match MathStyle::from_cur(c as i16) {
        Some(MathStyle::Display) => print_esc_cstr("displaystyle"),
        Some(MathStyle::Text) => print_esc_cstr("textstyle"),
        Some(MathStyle::Script) => print_esc_cstr("scriptstyle"),
        Some(MathStyle::ScriptScript) => print_esc_cstr("scriptscriptstyle"),
        None => print_cstr("Unknown style!"),
    };
}
pub(crate) unsafe fn print_skip_param(n: GluePar) {
    use GluePar::*;
    match n {
        line_skip => print_esc_cstr("lineskip"),
        baseline_skip => print_esc_cstr("baselineskip"),
        par_skip => print_esc_cstr("parskip"),
        above_display_skip => print_esc_cstr("abovedisplayskip"),
        below_display_skip => print_esc_cstr("belowdisplayskip"),
        above_display_short_skip => print_esc_cstr("abovedisplayshortskip"),
        below_display_short_skip => print_esc_cstr("belowdisplayshortskip"),
        left_skip => print_esc_cstr("leftskip"),
        right_skip => print_esc_cstr("rightskip"),
        top_skip => print_esc_cstr("topskip"),
        split_top_skip => print_esc_cstr("splittopskip"),
        tab_skip => print_esc_cstr("tabskip"),
        space_skip => print_esc_cstr("spaceskip"),
        xspace_skip => print_esc_cstr("xspaceskip"),
        par_fill_skip => print_esc_cstr("parfillskip"),
        xetex_linebreak_skip => print_esc_cstr("XeTeXlinebreakskip"),
        thin_mu_skip => print_esc_cstr("thinmuskip"),
        med_mu_skip => print_esc_cstr("medmuskip"),
        thick_mu_skip => print_esc_cstr("thickmuskip"),
    };
}
pub(crate) unsafe fn show_node_list(mut popt: Option<usize>) {
    if cur_length() > depth_threshold {
        if popt.is_some() {
            print_cstr(" []");
        }
        return;
    }
    let mut n = 0;
    while let Some(p) = popt.filter(|&p| p != 0) {
        print_ln();
        print_current_string();
        if p > mem_end as usize {
            print_cstr("Bad link, display aborted.");
            return;
        }
        n += 1;
        if n > breadth_max {
            print_cstr("etc.");
            return;
        }
        let p = p as usize;
        match Node::from(p) {
            Node::Char(_) => print_font_and_char(p as _),
            Node::Text(n) => match n {
                TxtNode::List(p) => {
                    if p.is_horizontal() {
                        print_esc('h' as i32);
                    } else {
                        print_esc('v' as i32);
                    }
                    print_cstr("box(");
                    print_scaled(p.height());
                    print_chr('+');
                    print_scaled(p.depth());
                    print_cstr(")x");
                    print_scaled(p.width());
                    let g = p.glue_set();
                    if g != 0. && p.glue_sign() != GlueSign::Normal {
                        print_cstr(", glue set ");
                        if p.glue_sign() == GlueSign::Shrinking {
                            print_cstr("- ");
                        }
                        if g.abs() > 20000. {
                            if g > 0. {
                                print_chr('>');
                            } else {
                                print_cstr("< -");
                            }
                            print_glue(Scaled::from(20000), p.glue_order(), "");
                        } else {
                            print_glue(tex_round(65536_f64 * g), p.glue_order(), "");
                        }
                    }
                    if p.shift_amount() != Scaled::ZERO {
                        print_cstr(", shifted ");
                        print_scaled(p.shift_amount());
                    }
                    /*1491:*/
                    if p.is_horizontal() && p.lr_mode() == LRMode::DList {
                        print_cstr(", display");
                    }
                    str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(p.list_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Unset(p) => {
                    print_esc_cstr("unset");
                    print_cstr("box(");
                    print_scaled(p.height());
                    print_chr('+');
                    print_scaled(p.depth());
                    print_cstr(")x");
                    print_scaled(p.width());
                    /*193:*/
                    if p.columns() != 0 {
                        print_cstr(" (");
                        print_int(p.columns() as i32 + 1);
                        print_cstr(" columns)");
                    }
                    if p.stretch() != Scaled::ZERO {
                        print_cstr(", stretch ");
                        print_glue(p.stretch(), p.stretch_order(), "");
                    }
                    if p.shrink() != Scaled::ZERO {
                        print_cstr(", shrink ");
                        print_glue(p.shrink(), p.shrink_order(), "");
                    }
                    str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(p.list_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Rule(p) => {
                    print_esc_cstr("rule(");
                    print_rule_dimen(p.height());
                    print_chr('+');
                    print_rule_dimen(p.depth());
                    print_cstr(")x");
                    print_rule_dimen(p.width());
                }
                TxtNode::Ins(p_ins) => {
                    print_esc_cstr("insert");
                    print_int(p_ins.box_reg() as i32);
                    print_cstr(", natural size ");
                    print_scaled(p_ins.height());
                    print_cstr("; split(");
                    print_spec(p_ins.split_top_ptr(), "");
                    print_chr(',');
                    print_scaled(p_ins.depth());
                    print_cstr("); float cost ");
                    print_int(p_ins.float_cost());
                    str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(p_ins.ins_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::WhatsIt(p) => match p {
                    WhatsIt::Open(p) => {
                        print_write_whatsit("openout", p.ptr());
                        print_chr('=');
                        print_file_name(p.name(), p.area(), p.ext());
                    }
                    WhatsIt::Write(p) => {
                        print_write_whatsit("write", p.ptr());
                        print_mark(p.tokens());
                    }
                    WhatsIt::Close(p) => print_write_whatsit("closeout", p.ptr()),
                    WhatsIt::Special(s) => {
                        print_esc_cstr("special");
                        print_mark(s.tokens());
                    }
                    WhatsIt::Language(l) => {
                        print_esc_cstr("setlanguage");
                        print_int(l.lang());
                        print_cstr(" (hyphenmin ");
                        print_int(l.lhm() as i32);
                        print_chr(',');
                        print_int(l.rhm() as i32);
                        print_chr(')');
                    }
                    WhatsIt::NativeWord(nw) => {
                        print_esc(
                            (*hash.offset((FONT_ID_BASE as i32 + nw.font() as i32) as isize)).s1,
                        );
                        print_chr(' ');
                        print_native_word(&nw);
                    }
                    WhatsIt::Glyph(g) => {
                        print_esc(
                            (*hash.offset((FONT_ID_BASE as i32 + g.font() as i32) as isize)).s1,
                        );
                        print_cstr(" glyph#");
                        print_int(g.glyph() as i32);
                    }
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        if p.is_pdf() {
                            print_esc_cstr("XeTeXpdffile");
                        } else {
                            print_esc_cstr("XeTeXpicfile");
                        }
                        print_cstr("( ");
                        for i in p.path() {
                            print_raw_char(*i as UTF16_code, true);
                        }
                        print('\"' as i32);
                    }
                    WhatsIt::PdfSavePos(_) => print_esc_cstr("pdfsavepos"),
                    //_ => print_cstr("whatsit?"),
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
                        print_cstr("leaders ");
                        print_spec(g.glue_ptr(), "");
                        str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                        pool_ptr += 1;
                        show_node_list(g.leader_ptr().opt());
                        pool_ptr -= 1
                    } else {
                        print_esc_cstr("glue");
                        if g.param() != NORMAL {
                            print_chr('(');
                            if g.param() < COND_MATH_GLUE {
                                match GluePar::n(g.param() - 1) {
                                    Some(dimen) => print_skip_param(dimen),
                                    None => print_cstr("[unknown glue parameter!]"),
                                }
                            } else if g.param() == COND_MATH_GLUE {
                                print_esc_cstr("nonscript");
                            } else {
                                print_esc_cstr("mskip");
                            }
                            print_chr(')');
                        }
                        if g.param() != COND_MATH_GLUE {
                            print_chr(' ');
                            if g.param() < COND_MATH_GLUE {
                                print_spec(g.glue_ptr(), "");
                            } else {
                                print_spec(g.glue_ptr(), "mu");
                            }
                        }
                    }
                }
                TxtNode::Kern(k) => {
                    if k.subtype() != KernType::Math {
                        print_esc_cstr("kern");
                        if k.subtype() != KernType::Normal {
                            print_chr(' ');
                        }
                        print_scaled(k.width());
                        if k.subtype() == KernType::AccKern {
                            print_cstr(" (for accent)");
                        } else if k.subtype() == KernType::SpaceAdjustment {
                            print_cstr(" (space adjustment)");
                        }
                    } else {
                        print_esc_cstr("mkern");
                        print_scaled(k.width());
                        print_cstr("mu");
                    }
                }
                TxtNode::MarginKern(m) => {
                    print_esc_cstr("kern");
                    print_scaled(m.width());
                    if MEM[m.ptr()].b16.s0 == 0 {
                        print_cstr(" (left margin)");
                    } else {
                        print_cstr(" (right margin)");
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
                            MathType::Before => print_cstr("on"),
                            MathType::After => print_cstr("off"),
                            _ => unreachable!(),
                        }
                        if p.width() != Scaled::ZERO {
                            print_cstr(", surrounded ");
                            print_scaled(p.width());
                        }
                    }
                },
                TxtNode::Ligature(l) => {
                    print_font_and_char(l.ptr() + 1);
                    print_cstr(" (ligature ");
                    if l.left_hit() {
                        print_chr('|');
                    }
                    font_in_short_display = l.font() as usize;
                    short_display(l.lig_ptr().opt());
                    if l.right_hit() {
                        print_chr('|');
                    }
                    print_chr(')');
                }
                TxtNode::Penalty(p) => {
                    print_esc_cstr("penalty ");
                    print_int(p.penalty());
                }
                TxtNode::Disc(d) => {
                    print_esc_cstr("discretionary");
                    if d.replace_count() > 0 {
                        print_cstr(" replacing ");
                        print_int(d.replace_count() as i32);
                    }
                    str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(d.pre_break().opt());
                    pool_ptr -= 1;
                    str_pool[pool_ptr as usize] = '|' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(d.post_break().opt());
                    pool_ptr -= 1
                }
                TxtNode::Mark(m) => {
                    print_esc_cstr("mark");
                    if m.class() != 0 {
                        print_chr('s');
                        print_int(m.class());
                    }
                    print_mark(m.mark_ptr());
                }
                TxtNode::Adjust(a) => {
                    print_esc_cstr("vadjust");
                    if a.subtype() != AdjustType::Post {
                        print_cstr(" pre ");
                    }
                    str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(a.adj_ptr().opt());
                    pool_ptr -= 1
                }
                TxtNode::Style(_) => print_style(MEM[p].b16.s0 as i32),
                TxtNode::Choice(c) => {
                    print_esc_cstr("mathchoice");
                    str_pool[pool_ptr as usize] = 'D' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(c.display());
                    pool_ptr -= 1;
                    str_pool[pool_ptr as usize] = 'T' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(c.text());
                    pool_ptr -= 1;
                    str_pool[pool_ptr as usize] = 'S' as i32 as packed_UTF16_code;
                    pool_ptr += 1;
                    show_node_list(c.script());
                    pool_ptr -= 1;
                    str_pool[pool_ptr as usize] = 's' as i32 as packed_UTF16_code;
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
                            print_esc_cstr("accent");
                            print_fam_and_char(p + 4);
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
                    if n != MathNode::Left && n != MathNode::Right {
                        match Limit::from(MEM[p].b16.s0) {
                            Limit::Limits => print_esc_cstr("limits"),
                            Limit::NoLimits => print_esc_cstr("nolimits"),
                            Limit::Normal => {}
                        }
                        print_subsidiary_data(p + 1, '.' as i32 as UTF16_code);
                    }
                    print_subsidiary_data(p + 2, '^' as i32 as UTF16_code);
                    print_subsidiary_data(p + 3, '_' as i32 as UTF16_code);
                }
                MathNode::Fraction => {
                    let f = Fraction(p);
                    print_esc_cstr("fraction, thickness ");
                    if f.thickness() == DEFAULT_CODE {
                        print_cstr("= default");
                    } else {
                        print_scaled(f.thickness());
                    }
                    let ld = f.left_delimeter();
                    if ld.s3 as i32 % 256 != 0
                        || ld.s2 as i64 + (ld.s3 as i32 / 256) as i64 * 65536 != 0
                        || ld.s1 as i32 % 256 != 0
                        || ld.s0 as i64 + (ld.s1 as i32 / 256) as i64 * 65536 != 0
                    {
                        print_cstr(", left-delimiter ");
                        print_delimiter(ld);
                    }
                    let rd = f.right_delimeter();
                    if rd.s3 as i32 % 256 != 0
                        || rd.s2 as i64 + (rd.s3 as i32 / 256) as i64 * 65536 != 0
                        || rd.s1 as i32 % 256 != 0
                        || rd.s0 as i64 + (rd.s1 as i32 / 256) as i64 * 65536 != 0
                    {
                        print_cstr(", right-delimiter ");
                        print_delimiter(rd);
                    }
                    print_subsidiary_data(p + 2, '\\' as i32 as UTF16_code);
                    print_subsidiary_data(p + 3, '/' as i32 as UTF16_code);
                }
            },
            Node::Unknown(_) => print_cstr("Unknown node type!"),
        }
        popt = llist_link(p);
    }
}
pub(crate) unsafe fn show_box(p: Option<usize>) {
    depth_threshold = *INTPAR(IntPar::show_box_depth);
    breadth_max = *INTPAR(IntPar::show_box_breadth);
    if breadth_max <= 0 {
        breadth_max = 5;
    }
    if pool_ptr + depth_threshold >= pool_size {
        depth_threshold = pool_size - pool_ptr - 1;
    }
    show_node_list(p);
    print_ln();
}
pub(crate) unsafe fn short_display_n(p: Option<usize>, mut m: i32) {
    breadth_max = m;
    depth_threshold = pool_size - pool_ptr - 1;
    show_node_list(p);
}
pub(crate) unsafe fn delete_token_ref(p: usize) {
    if MEM[p].b32.s0.opt().is_none() {
        flush_list(Some(p));
    } else {
        MEM[p].b32.s0 -= 1;
    };
}
pub(crate) unsafe fn delete_glue_ref(p: usize) {
    if llist_link(p).is_none() {
        free_node(p, GLUE_SPEC_SIZE);
    } else {
        MEM[p].b32.s1 -= 1;
    };
}
pub(crate) unsafe fn flush_node_list(mut popt: Option<usize>) {
    let mut q: i32 = 0;
    while let Some(p) = popt {
        q = *LLIST_link(p);
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
                    let mut g = GlueSpec(p.glue_ptr() as usize);
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
                TxtNode::Kern(_) | TxtNode::Math(_) | TxtNode::Penalty(_) => {
                    free_node(p, MEDIUM_NODE_SIZE);
                }
                TxtNode::MarginKern(_) => {
                    free_node(p, MARGIN_KERN_NODE_SIZE);
                }
                TxtNode::Ligature(l) => {
                    flush_node_list(l.lig_ptr().opt());
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
                    flush_node_list(MEM[p + 2].b32.s0.opt());
                    flush_node_list(MEM[p + 3].b32.s0.opt());
                    free_node(p, FRACTION_NOAD_SIZE);
                }
            },
            Node::Unknown(_) => confusion("flushing"),
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
                    r_lig.set_lig_ptr(copy_node_list(p.lig_ptr().opt()));
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
pub(crate) unsafe fn print_mode(mut m: (bool, ListMode)) {
    match m {
        (_, ListMode::NoMode) => print_cstr("no mode"),
        (false, ListMode::VMode) => print_cstr("vertical mode"),
        (false, ListMode::HMode) => print_cstr("horizontal mode"),
        (false, ListMode::MMode) => print_cstr("display math mode"),
        (true, ListMode::VMode) => print_cstr("internal vertical mode"),
        (true, ListMode::HMode) => print_cstr("restricted horizontal mode"),
        (true, ListMode::MMode) => print_cstr("math mode"),
    }
}
pub(crate) unsafe fn print_in_mode(m: (bool, ListMode)) {
    match m {
        (_, ListMode::NoMode) => print_cstr("\' in no mode"),
        (false, ListMode::VMode) => print_cstr("\' in vertical mode"),
        (false, ListMode::HMode) => print_cstr("\' in horizontal mode"),
        (false, ListMode::MMode) => print_cstr("\' in display math mode"),
        (true, ListMode::VMode) => print_cstr("\' in internal vertical mode"),
        (true, ListMode::HMode) => print_cstr("\' in restricted horizontal mode"),
        (true, ListMode::MMode) => print_cstr("\' in math mode"),
    }
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
    print_nl_cstr("");
    print_ln();
    let mut p = NEST_PTR as usize;
    let for_end = 0;
    if p >= for_end {
        loop {
            let mut m = NEST[p].mode;
            let a = NEST[p].aux;
            print_nl_cstr("### ");
            print_mode(m);
            print_cstr(" entered at line ");
            print_int(NEST[p].mode_line.abs());
            if m == (false, ListMode::HMode) {
                if NEST[p].prev_graf != 0x830000 {
                    print_cstr(" (language");
                    print_int((NEST[p].prev_graf as i64 % 65536) as i32);
                    print_cstr(":hyphenmin");
                    print_int(NEST[p].prev_graf / 0x400000);
                    print_chr(',');
                    print_int((NEST[p].prev_graf as i64 / 65536 % 64) as i32);
                    print_chr(')');
                }
            }
            if NEST[p].mode_line < 0 {
                print_cstr(" (\\output routine)");
            }
            if p == 0 {
                if PAGE_HEAD != page_tail {
                    print_nl_cstr("### current page:");
                    if output_active {
                        print_cstr(" (held over for next output)");
                    }
                    show_box(llist_link(PAGE_HEAD));
                    if page_contents == PageContents::InsertsOnly
                        || page_contents == PageContents::BoxThere
                    {
                        print_nl_cstr("total height ");
                        print_totals();
                        print_nl_cstr(" goal height ");
                        print_scaled(page_so_far[0]);
                        let mut r = PageInsertion(*LLIST_link(PAGE_INS_HEAD) as usize);
                        while r.ptr() != PAGE_INS_HEAD {
                            print_ln();
                            print_esc_cstr("insert");
                            let t = r.box_reg() as i32;
                            print_int(t);
                            print_cstr(" adds ");
                            let t = if *COUNT_REG(t as usize) == 1000 {
                                r.height()
                            } else {
                                x_over_n(r.height(), 1000).0 * *COUNT_REG(t as usize)
                            };
                            print_scaled(t);
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
                                print_cstr(", #");
                                print_int(t);
                                print_cstr(" might split");
                            }
                            r = PageInsertion(*LLIST_link(r.ptr()) as usize);
                        }
                    }
                }
                if llist_link(CONTRIB_HEAD).is_some() {
                    print_nl_cstr("### recent contributions:");
                }
            }
            show_box(MEM[NEST[p].head as usize].b32.s1.opt());
            match m.1 {
                ListMode::VMode => {
                    print_nl_cstr("prevdepth ");
                    if a.b32.s1 <= IGNORE_DEPTH {
                        print_cstr("ignored");
                    } else {
                        print_scaled(Scaled(a.b32.s1));
                    }
                    if NEST[p].prev_graf != 0 {
                        print_cstr(", prevgraf ");
                        print_int(NEST[p].prev_graf);
                        if NEST[p].prev_graf != 1 {
                            print_cstr(" lines");
                        } else {
                            print_cstr(" line");
                        }
                    }
                }
                ListMode::HMode => {
                    print_nl_cstr("spacefactor ");
                    print_int(a.b32.s0);
                    if m.0 == false {
                        if a.b32.s1 > 0 {
                            print_cstr(", current language ");
                            print_int(a.b32.s1);
                        }
                    }
                }
                ListMode::MMode => {
                    if let Some(o) = a.b32.s1.opt() {
                        print_cstr("this will be denominator of:");
                        show_box(Some(o));
                    }
                }
                ListMode::NoMode => {}
            }
            if !(p > for_end) {
                break;
            }
            p -= 1;
        }
    };
}
pub(crate) unsafe fn print_param(n: IntPar) {
    use IntPar::*;
    match n {
        pretolerance => print_esc_cstr("pretolerance"),
        tolerance => print_esc_cstr("tolerance"),
        line_penalty => print_esc_cstr("linepenalty"),
        hyphen_penalty => print_esc_cstr("hyphenpenalty"),
        ex_hyphen_penalty => print_esc_cstr("exhyphenpenalty"),
        club_penalty => print_esc_cstr("clubpenalty"),
        widow_penalty => print_esc_cstr("widowpenalty"),
        display_widow_penalty => print_esc_cstr("displaywidowpenalty"),
        broken_penalty => print_esc_cstr("brokenpenalty"),
        bin_op_penalty => print_esc_cstr("binoppenalty"),
        rel_penalty => print_esc_cstr("relpenalty"),
        pre_display_penalty => print_esc_cstr("predisplaypenalty"),
        post_display_penalty => print_esc_cstr("postdisplaypenalty"),
        inter_line_penalty => print_esc_cstr("interlinepenalty"),
        double_hyphen_demerits => print_esc_cstr("doublehyphendemerits"),
        final_hyphen_demerits => print_esc_cstr("finalhyphendemerits"),
        adj_demerits => print_esc_cstr("adjdemerits"),
        mag => print_esc_cstr("mag"),
        delimiter_factor => print_esc_cstr("delimiterfactor"),
        looseness => print_esc_cstr("looseness"),
        time => print_esc_cstr("time"),
        day => print_esc_cstr("day"),
        month => print_esc_cstr("month"),
        year => print_esc_cstr("year"),
        show_box_breadth => print_esc_cstr("showboxbreadth"),
        show_box_depth => print_esc_cstr("showboxdepth"),
        hbadness => print_esc_cstr("hbadness"),
        vbadness => print_esc_cstr("vbadness"),
        pausing => print_esc_cstr("pausing"),
        tracing_online => print_esc_cstr("tracingonline"),
        tracing_macros => print_esc_cstr("tracingmacros"),
        tracing_stats => print_esc_cstr("tracingstats"),
        tracing_paragraphs => print_esc_cstr("tracingparagraphs"),
        tracing_pages => print_esc_cstr("tracingpages"),
        tracing_output => print_esc_cstr("tracingoutput"),
        tracing_lost_chars => print_esc_cstr("tracinglostchars"),
        tracing_commands => print_esc_cstr("tracingcommands"),
        tracing_restores => print_esc_cstr("tracingrestores"),
        uc_hyph => print_esc_cstr("uchyph"),
        output_penalty => print_esc_cstr("outputpenalty"),
        max_dead_cycles => print_esc_cstr("maxdeadcycles"),
        hang_after => print_esc_cstr("hangafter"),
        floating_penalty => print_esc_cstr("floatingpenalty"),
        global_defs => print_esc_cstr("globaldefs"),
        cur_fam => print_esc_cstr("fam"),
        escape_char => print_esc_cstr("escapechar"),
        default_hyphen_char => print_esc_cstr("defaulthyphenchar"),
        default_skew_char => print_esc_cstr("defaultskewchar"),
        end_line_char => print_esc_cstr("endlinechar"),
        new_line_char => print_esc_cstr("newlinechar"),
        language => print_esc_cstr("language"),
        left_hyphen_min => print_esc_cstr("lefthyphenmin"),
        right_hyphen_min => print_esc_cstr("righthyphenmin"),
        holding_inserts => print_esc_cstr("holdinginserts"),
        error_context_lines => print_esc_cstr("errorcontextlines"),
        char_sub_def_min => print_esc_cstr("charsubdefmin"),
        char_sub_def_max => print_esc_cstr("charsubdefmax"),
        tracing_char_sub_def => print_esc_cstr("tracingcharsubdef"),
        xetex_linebreak_penalty => print_esc_cstr("XeTeXlinebreakpenalty"),
        xetex_protrude_chars => print_esc_cstr("XeTeXprotrudechars"),
        synctex => print_esc_cstr("synctex"),
        tracing_assigns => print_esc_cstr("tracingassigns"),
        tracing_groups => print_esc_cstr("tracinggroups"),
        tracing_ifs => print_esc_cstr("tracingifs"),
        tracing_scan_tokens => print_esc_cstr("tracingscantokens"),
        tracing_nesting => print_esc_cstr("tracingnesting"),
        pre_display_correction => print_esc_cstr("predisplaydirection"),
        last_line_fit => print_esc_cstr("lastlinefit"),
        saving_vdiscards => print_esc_cstr("savingvdiscards"),
        saving_hyphs => print_esc_cstr("savinghyphcodes"),
        suppress_fontnotfound_error => print_esc_cstr("suppressfontnotfounderror"),
        texxet => print_esc_cstr("TeXXeTstate"),
        xetex_upwards => print_esc_cstr("XeTeXupwardsmode"),
        xetex_use_glyph_metrics => print_esc_cstr("XeTeXuseglyphmetrics"),
        xetex_inter_char_tokens => print_esc_cstr("XeTeXinterchartokenstate"),
        xetex_dash_break => print_esc_cstr("XeTeXdashbreakstate"),
        xetex_input_normalization => print_esc_cstr("XeTeXinputnormalization"),
        xetex_tracing_fonts => print_esc_cstr("XeTeXtracingfonts"),
        xetex_interword_space_shaping => print_esc_cstr("XeTeXinterwordspaceshaping"),
        xetex_generate_actual_text => print_esc_cstr("XeTeXgenerateactualtext"),
        xetex_hyphenatable_length => print_esc_cstr("XeTeXhyphenatablelength"),
        pdfoutput => print_esc_cstr("pdfoutput"),
        _ => print_cstr("[unknown i32 parameter!]"), // NOTE: several parameters not covered
    };
}

pub(crate) unsafe fn diagnostic<F>(blank_line: bool, f: F)
where
    F: Fn(),
{
    let oldsetting = selector;
    if *INTPAR(IntPar::tracing_online) <= 0 && selector == Selector::TERM_AND_LOG {
        selector = (u8::from(selector) - 1).into();
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
    f();
    print_nl_cstr("");
    if blank_line {
        print_ln();
    }
    selector = oldsetting;
}

pub(crate) unsafe fn print_length_param(mut n: DimenPar) {
    use DimenPar::*;
    match n {
        par_indent => print_esc_cstr("parindent"),
        math_surround => print_esc_cstr("mathsurround"),
        line_skip_limit => print_esc_cstr("lineskiplimit"),
        hsize => print_esc_cstr("hsize"),
        vsize => print_esc_cstr("vsize"),
        max_depth => print_esc_cstr("maxdepth"),
        split_max_depth => print_esc_cstr("splitmaxdepth"),
        box_max_depth => print_esc_cstr("boxmaxdepth"),
        hfuzz => print_esc_cstr("hfuzz"),
        vfuzz => print_esc_cstr("vfuzz"),
        delimiter_shortfall => print_esc_cstr("delimitershortfall"),
        null_delimiter_space => print_esc_cstr("nulldelimiterspace"),
        script_space => print_esc_cstr("scriptspace"),
        pre_display_size => print_esc_cstr("predisplaysize"),
        display_width => print_esc_cstr("displaywidth"),
        display_indent => print_esc_cstr("displayindent"),
        overfull_rule => print_esc_cstr("overfullrule"),
        hang_indent => print_esc_cstr("hangindent"),
        h_offset => print_esc_cstr("hoffset"),
        v_offset => print_esc_cstr("voffset"),
        emergency_stretch => print_esc_cstr("emergencystretch"),
        pdf_page_width => print_esc_cstr("pdfpagewidth"),
        pdf_page_height => print_esc_cstr("pdfpageheight"),
    }
}
pub(crate) unsafe fn print_cmd_chr(mut cmd: Cmd, mut chr_code: i32) {
    let mut font_name_str: str_number = 0;
    let mut quote_char: UTF16_code = 0;
    match cmd {
        Cmd::LeftBrace => {
            print_cstr("begin-group character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::RightBrace => {
            print_cstr("end-group character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::MathShift => {
            print_cstr("math shift character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::MacParam => {
            print_cstr("macro parameter character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::SupMark => {
            print_cstr("superscript character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::SubMark => {
            print_cstr("subscript character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::EndV => print_cstr("end of alignment template"),
        Cmd::Spacer => {
            print_cstr("blank space ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::Letter => {
            print_cstr("the letter ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::OtherChar => {
            print_cstr("the character ");
            if chr_code < 65536 {
                print(chr_code);
            } else {
                print_char(chr_code);
            }
        }
        Cmd::AssignGlue | Cmd::AssignMuGlue => {
            if chr_code < SKIP_BASE as i32 {
                match GluePar::n((chr_code - GLUE_BASE as i32) as u16) {
                    Some(dimen) => print_skip_param(dimen),
                    None => print_cstr("[unknown glue parameter!]"),
                }
            } else if chr_code < MU_SKIP_BASE as i32 {
                print_esc_cstr("skip");
                print_int(chr_code - SKIP_BASE as i32);
            } else {
                print_esc_cstr("muskip");
                print_int(chr_code - MU_SKIP_BASE as i32);
            }
        }
        Cmd::AssignToks => {
            if chr_code >= TOKS_BASE as i32 {
                print_esc_cstr("toks");
                print_int(chr_code - TOKS_BASE as i32);
            } else {
                print_esc_cstr(match Local::n(chr_code - LOCAL_BASE as i32) {
                    Some(Local::output_routine) => "output",
                    Some(Local::every_par) => "everypar",
                    Some(Local::every_math) => "everymath",
                    Some(Local::every_display) => "everydisplay",
                    Some(Local::every_hbox) => "everyhbox",
                    Some(Local::every_vbox) => "everyvbox",
                    Some(Local::every_job) => "everyjob",
                    Some(Local::every_cr) => "everycr",
                    Some(Local::every_eof) => "everyeof",
                    Some(Local::xetex_inter_char) => "XeTeXinterchartoks",
                    Some(Local::TectonicCodaTokens) => "TectonicCodaTokens",
                    _ => "errhelp",
                });
            }
        }
        Cmd::AssignInt => {
            if chr_code < COUNT_BASE as i32 {
                match IntPar::n(chr_code - INT_BASE as i32) {
                    Some(dimen) => print_param(dimen),
                    None => print_cstr("[unknown i32 parameter!]"),
                }
            } else {
                print_esc_cstr("count");
                print_int(chr_code - COUNT_BASE as i32);
            }
        }
        Cmd::AssignDimen => {
            if chr_code < SCALED_BASE as i32 {
                match DimenPar::n(chr_code - DIMEN_BASE as i32) {
                    Some(dimen) => print_length_param(dimen),
                    None => print_cstr("[unknown dimen parameter!]"),
                }
            } else {
                print_esc_cstr("dimen");
                print_int(chr_code - SCALED_BASE as i32);
            }
        }
        Cmd::Accent => print_esc_cstr("accent"),
        Cmd::Advance => print_esc_cstr("advance"),
        Cmd::AfterAssignment => print_esc_cstr("afterassignment"),
        Cmd::AfterGroup => print_esc_cstr("aftergroup"),
        Cmd::AssignFontDimen => print_esc_cstr("fontdimen"),
        Cmd::BeginGroup => print_esc_cstr("begingroup"),
        Cmd::BreakPenalty => print_esc_cstr("penalty"),
        Cmd::CharNum => print_esc_cstr("char"),
        Cmd::CSName => print_esc_cstr("csname"),
        Cmd::DefFont => print_esc_cstr("font"),
        Cmd::DelimNum => print_esc_cstr(if chr_code == 1 {
            "Udelimiter"
        } else {
            "delimiter"
        }),
        Cmd::Divide => print_esc_cstr("divide"),
        Cmd::EndCSName => print_esc_cstr("endcsname"),
        Cmd::EndGroup => print_esc_cstr("endgroup"),
        Cmd::ExSpace => print_esc(' ' as i32),
        Cmd::ExpandAfter => print_esc_cstr(if chr_code == 0 {
            "expandafter"
        } else {
            "unless"
        }),
        Cmd::HAlign => print_esc_cstr("halign"),
        Cmd::HRule => print_esc_cstr("hrule"),
        Cmd::IgnoreSpaces => print_esc_cstr(if chr_code == 0 {
            "ignorespaces"
        } else {
            "primitive"
        }),
        Cmd::Insert => print_esc_cstr("insert"),
        Cmd::ItalCorr => print_esc('/' as i32),
        Cmd::Mark => {
            print_esc_cstr("mark");
            if chr_code > 0 {
                print_chr('s');
            }
        }
        Cmd::MathAccent => print_esc_cstr(if chr_code == 1 {
            "Umathaccent"
        } else {
            "mathaccent"
        }),
        Cmd::MathCharNum => print_esc_cstr(match chr_code {
            2 => "Umathchar",
            1 => "Umathcharnum",
            _ => "mathchar",
        }),
        Cmd::MathChoice => print_esc_cstr("mathchoice"),
        Cmd::Multiply => print_esc_cstr("multiply"),
        Cmd::NoAlign => print_esc_cstr("noalign"),
        Cmd::NoBoundary => print_esc_cstr("noboundary"),
        Cmd::NoExpand => print_esc_cstr(if chr_code == 0 {
            "noexpand"
        } else {
            "primitive"
        }),
        Cmd::NonScript => print_esc_cstr("nonscript"),
        Cmd::Omit => print_esc_cstr("omit"),
        Cmd::Radical => print_esc_cstr(if chr_code == 1 { "Uradical" } else { "radical" }),
        Cmd::ReadToCS => print_esc_cstr(if chr_code == 0 { "read" } else { "readline" }),
        Cmd::Relax => print_esc_cstr("relax"),
        Cmd::SetBox => print_esc_cstr("setbox"),
        Cmd::SetPrevGraf => print_esc_cstr("prevgraf"),
        Cmd::SetShape => print_esc_cstr(match chr_code as usize {
            c if c == LOCAL_BASE as usize + Local::par_shape as usize => "parshape",
            INTER_LINE_PENALTIES_LOC => "interlinepenalties",
            CLUB_PENALTIES_LOC => "clubpenalties",
            WIDOW_PENALTIES_LOC => "widowpenalties",
            DISPLAY_WIDOW_PENALTIES_LOC => "displaywidowpenalties",
            _ => unreachable!(),
        }),
        Cmd::The => print_esc_cstr(match chr_code {
            0 => "the",
            1 => "unexpanded",
            _ => "detokenize",
        }),
        Cmd::ToksRegister => {
            print_esc_cstr("toks");
            if chr_code != 0 {
                print_sa_num(chr_code.opt().unwrap());
            }
        }
        Cmd::VAdjust => print_esc_cstr("vadjust"),
        Cmd::VAlign => print_esc_cstr(if chr_code == 0 {
            "valign"
        } else {
            match MathType::from(chr_code as u16) {
                MathType::Eq(BE::Begin, MathMode::Left) => "beginL",
                MathType::Eq(BE::End, MathMode::Left) => "endL",
                MathType::Eq(BE::Begin, MathMode::Right) => "beginR",
                _ => "endR",
            }
        }),
        Cmd::VCenter => print_esc_cstr("vcenter"),
        Cmd::VRule => print_esc_cstr("vrule"),
        PAR_END => print_esc_cstr("par"),
        Cmd::Input => print_esc_cstr(match chr_code {
            0 => "input",
            2 => "scantokens",
            _ => "endinput",
        }),
        Cmd::TopBotMark => {
            print_esc_cstr(match (chr_code % MARKS_CODE) as usize {
                FIRST_MARK_CODE => "firstmark",
                BOT_MARK_CODE => "botmark",
                SPLIT_FIRST_MARK_CODE => "splitfirstmark",
                SPLIT_BOT_MARK_CODE => "splitbotmark",
                _ => "topmark",
            });
            if chr_code >= MARKS_CODE {
                print_chr('s');
            }
        }
        Cmd::Register => {
            let cmd;
            if chr_code < 0 || chr_code > 19 {
                /*lo_mem_stat_max*/
                cmd = (MEM[chr_code as usize].b16.s1 as i32 / 64) as u16
            } else {
                cmd = chr_code as u16;
                chr_code = None.tex_int();
            }
            print_esc_cstr(if cmd == ValLevel::Int as u16 {
                "count"
            } else if cmd == ValLevel::Dimen as u16 {
                "dimen"
            } else if cmd == ValLevel::Glue as u16 {
                "skip"
            } else {
                "muskip"
            });
            if let Some(q) = chr_code.opt() {
                print_sa_num(q);
            }
        }
        Cmd::SetAux => print_esc_cstr(if chr_code == ListMode::VMode as i32 {
            "prevdepth"
        } else {
            "spacefactor"
        }),
        Cmd::SetPageInt => print_esc_cstr(match chr_code {
            0 => "deadcycles",
            2 => "interactionmode",
            _ => "insertpenalties",
        }),
        Cmd::SetBoxDimen => print_esc_cstr(match SetBoxDimen::n(chr_code as u8).unwrap() {
            SetBoxDimen::WidthOffset => "wd",
            SetBoxDimen::HeightOffset => "ht",
            SetBoxDimen::DepthOffset => "dp",
        }),
        Cmd::LastItem => print_esc_cstr({
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
        }),
        Cmd::Convert => print_esc_cstr({
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
        }),
        Cmd::IfTest => {
            if chr_code >= UNLESS_CODE {
                print_esc_cstr("unless");
            }
            print_esc_cstr(
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
            );
        }
        Cmd::FiOrElse => print_esc_cstr(match FiOrElseCode::n(chr_code as u8).unwrap() {
            FiOrElseCode::Fi => "fi",
            FiOrElseCode::Or => "or",
            FiOrElseCode::Else => "else",
            _ => unreachable!(),
        }),
        Cmd::TabMark => {
            if chr_code == SPAN_CODE {
                print_esc_cstr("span");
            } else {
                print_cstr("alignment tab character ");
                if (chr_code as i64) < 65536 {
                    print(chr_code);
                } else {
                    print_char(chr_code);
                }
            }
        }
        Cmd::CarRet => print_esc_cstr(if chr_code == CR_CODE { "cr" } else { "crcr" }),
        Cmd::SetPageDimen => print_esc_cstr(match chr_code {
            0 => "pagegoal",         // genuine literal in WEB
            1 => "pagetotal",        // genuine literal in WEB
            2 => "pagestretch",      // genuine literal in WEB
            3 => "pagefilstretch",   // genuine literal in WEB
            4 => "pagefillstretch",  // genuine literal in WEB
            5 => "pagefilllstretch", // genuine literal in WEB
            6 => "pageshrink",       // genuine literal in WEB
            _ => "pagedepth",
        }),
        STOP => print_esc_cstr(if chr_code == 1 { "dump" } else { "end" }),
        Cmd::HSkip => print_esc_cstr(match SkipCode::n(chr_code as u8).unwrap() {
            SkipCode::Skip => "hskip",
            SkipCode::Fil => "hfil",
            SkipCode::Fill => "hfill",
            SkipCode::Ss => "hss",
            SkipCode::FilNeg => "hfilneg",
            _ => unreachable!(),
        }),
        Cmd::VSkip => print_esc_cstr(match SkipCode::n(chr_code as u8).unwrap() {
            SkipCode::Skip => "vskip",
            SkipCode::Fil => "vfil",
            SkipCode::Fill => "vfill",
            SkipCode::Ss => "vss",
            SkipCode::FilNeg => "vfilneg",
            _ => unreachable!(),
        }),
        Cmd::MSkip => print_esc_cstr("mskip"),
        Cmd::Kern => print_esc_cstr("kern"),
        Cmd::MKern => print_esc_cstr("mkern"),
        Cmd::HMove => print_esc_cstr(if chr_code == 1 {
            "moveleft"
        } else {
            "moveright"
        }),
        Cmd::VMove => print_esc_cstr({
            if chr_code == 1 {
                "raise"
            } else {
                "lower"
            }
        }),
        Cmd::MakeBox => print_esc_cstr(match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Box => "box",
            BoxCode::Copy => "copy",
            BoxCode::LastBox => "lastbox",
            BoxCode::VSplit => "vsplit",
            BoxCode::VTop => "vtop",
            BoxCode::VBox => "vbox",
            BoxCode::HBox => "hbox",
        }),
        Cmd::LeaderShip => print_esc_cstr(match chr_code as u16 {
            A_LEADERS => "leaders",
            C_LEADERS => "cleaders",
            X_LEADERS => "xleaders",
            _ => "shipout",
        }),
        Cmd::StartPar => print_esc_cstr(if chr_code == 0 { "noindent" } else { "indent" }),
        Cmd::RemoveItem => print_esc_cstr(match chr_code {
            10 => "unskip", // Node::Glue
            11 => "unkern", // Node::Kern
            _ => "unpenalty",
        }),
        Cmd::UnHBox => print_esc_cstr(match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Copy => "unhcopy",
            _ => "unhbox",
        }),
        Cmd::UnVBox => print_esc_cstr(match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Copy => "unvcopy",
            BoxCode::LastBox => "pagediscards",
            BoxCode::VSplit => "splitdiscards",
            _ => "unvbox",
        }),
        Cmd::Discretionary => {
            if chr_code == 1 {
                print_esc('-' as i32);
            } else {
                print_esc_cstr("discretionary");
            }
        }
        Cmd::EqNo => print_esc_cstr(if chr_code == 1 { "leqno" } else { "eqno" }),
        Cmd::MathComp => print_esc_cstr(match MathNode::n(chr_code as u16).unwrap() {
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
        }),
        Cmd::LimitSwitch => match Limit::from(chr_code as u16) {
            Limit::Limits => print_esc_cstr("limits"),
            Limit::NoLimits => print_esc_cstr("nolimits"),
            Limit::Normal => print_esc_cstr("displaylimits"),
        },
        Cmd::MathStyle => print_style(chr_code),
        Cmd::Above => print_esc_cstr(match chr_code {
            OVER_CODE => "over",
            ATOP_CODE => "atop",
            // DELIMITED_CODE + ABOVE_CODE
            3 => "abovewithdelims",
            // DELIMITED_CODE + OVER_CODE
            4 => "overwithdelims",
            // DELIMITED_CODE + ATOP_CODE
            5 => "atopwithdelims",
            _ => "above",
        }),
        Cmd::LeftRight => print_esc_cstr(if chr_code as u16 == MathNode::Left as u16 {
            "left"
        } else if chr_code as u16 == MIDDLE_NOAD {
            "middle"
        } else {
            "right"
        }),
        Cmd::Prefix => print_esc_cstr(match chr_code {
            1 => "long",
            2 => "outer",
            8 => "protected",
            _ => "global",
        }),
        Cmd::Def => print_esc_cstr(match chr_code {
            0 => "def",
            1 => "gdef",
            2 => "edef",
            _ => "xdef",
        }),
        Cmd::Let => {
            print_esc_cstr(if chr_code as u16 != NORMAL {
                "futurelet"
            } else {
                "let"
            });
        }
        Cmd::ShorthandDef => print_esc_cstr(match ShorthandDefCode::n(chr_code as u8).unwrap() {
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
        }),
        Cmd::CharGiven => {
            print_esc_cstr("char");
            print_hex(chr_code);
        }
        Cmd::MathGiven => {
            print_esc_cstr("mathchar");
            print_hex(chr_code);
        }
        Cmd::XetexMathGiven => {
            print_esc_cstr("Umathchar");
            print_hex(math_class(chr_code) as i32);
            print_hex(math_fam(chr_code) as i32);
            print_hex(math_char(chr_code) as i32);
        }
        Cmd::DefCode => print_esc_cstr(if chr_code == CAT_CODE_BASE as i32 {
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
        }),
        Cmd::XetexDefCode => print_esc_cstr(if chr_code == SF_CODE_BASE as i32 {
            "XeTeXcharclass"
        } else if chr_code == MATH_CODE_BASE as i32 {
            "Umathcodenum"
        } else if chr_code == MATH_CODE_BASE as i32 + 1 {
            "Umathcode"
        } else if chr_code == DEL_CODE_BASE as i32 {
            "Udelcodenum"
        } else {
            "Udelcode"
        }),
        Cmd::DefFamily => print_size(chr_code - (MATH_FONT_BASE as i32)),
        Cmd::HyphData => print_esc_cstr(if chr_code == 1 {
            "patterns"
        } else {
            "hyphenation"
        }),
        Cmd::AssignFontInt => print_esc_cstr(match AssignFontInt::from(chr_code) {
            AssignFontInt::HyphenChar => "hyphenchar",
            AssignFontInt::SkewChar => "skewchar",
            AssignFontInt::LpCode => "lpcode",
            AssignFontInt::RpCode => "rpcode",
        }),
        Cmd::SetFont => {
            print_cstr("select font ");
            font_name_str = FONT_NAME[chr_code as usize];
            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[chr_code as usize] {
                quote_char = '\"' as i32 as UTF16_code;
                for n in 0..=length(font_name_str) {
                    if str_pool[(str_start[(font_name_str as i64 - 65536) as usize] + n) as usize]
                        as i32
                        == '\"' as i32
                    {
                        quote_char = '\'' as i32 as UTF16_code
                    }
                }
                print_char(quote_char as i32);
                print(font_name_str);
                print_char(quote_char as i32);
            } else {
                print(font_name_str);
            }
            if FONT_SIZE[chr_code as usize] != FONT_DSIZE[chr_code as usize] {
                print_cstr(" at ");
                print_scaled(FONT_SIZE[chr_code as usize]);
                print_cstr("pt");
            }
        }
        Cmd::SetInteraction => print_esc_cstr(match InteractionMode::n(chr_code as u8).unwrap() {
            InteractionMode::Batch => "batchmode",
            InteractionMode::NonStop => "nonstopmode",
            InteractionMode::Scroll => "scrollmode",
            InteractionMode::ErrorStop => "errorstopmode",
        }),
        Cmd::InStream => print_esc_cstr(if chr_code == 0 { "closein" } else { "openin" }),
        Cmd::Message => print_esc_cstr(if chr_code == 0 {
            "message"
        } else {
            "errmessage"
        }),
        Cmd::CaseShift => print_esc_cstr(if chr_code == LC_CODE_BASE as i32 {
            "lowercase"
        } else {
            "uppercase"
        }),
        Cmd::XRay => print_esc_cstr(match chr_code {
            SHOW_BOX_CODE => "showbox",
            SHOW_THE_CODE => "showthe",
            SHOW_LISTS => "showlists",
            SHOW_GROUPS => "showgroups",
            SHOW_TOKENS => "showtokens",
            SHOW_IFS => "showifs",
            _ => "show",
        }),
        Cmd::UndefinedCS => print_cstr("undefined"),
        Cmd::Call | Cmd::LongCall | Cmd::OuterCall | Cmd::LongOuterCall => {
            let mut n = cmd as u8 - Cmd::Call as u8;
            if MEM[*LLIST_link(chr_code as usize) as usize].b32.s0 == PROTECTED_TOKEN {
                n = n + 4
            }
            if n / 4 & 1 != 0 {
                print_esc_cstr("protected");
            }
            if n & 1 != 0 {
                print_esc_cstr("long");
            }
            if n / 2 & 1 != 0 {
                print_esc_cstr("outer");
            }
            if n > 0 {
                print_chr(' ');
            }
            print_cstr("macro");
        }
        Cmd::EndTemplate => print_esc_cstr("outer endtemplate"),
        Cmd::Extension => match chr_code as u16 {
            0 => print_esc_cstr("openout"),               // WhatsIt::Open
            1 => print_esc_cstr("write"),                 // WhatsIt::Write
            2 => print_esc_cstr("closeout"),              // WhatsIt::Close
            3 => print_esc_cstr("special"),               // WhatsIt::Special
            4 => print_esc_cstr("immediate"),             // IMMEDIATE_CODE
            5 => print_esc_cstr("setlanguage"),           // SET_LANGUAGE_CODE
            41 => print_esc_cstr("XeTeXpicfile"),         // PIC_FILE_CODE
            42 => print_esc_cstr("XeTeXpdffile"),         // PDF_FILE_CODE
            43 => print_esc_cstr("XeTeXglyph"),           // GLYPH_CODE
            46 => print_esc_cstr("XeTeXlinebreaklocale"), // XETEX_LINEBREAK_LOCALE_EXTENSION_CODE
            44 => print_esc_cstr("XeTeXinputencoding"),   // XETEX_INPUT_ENCODING_EXTENSION_CODE
            45 => print_esc_cstr("XeTeXdefaultencoding"), // XETEX_DEFAULT_ENCODING_EXTENSION_CODE
            6 => print_esc_cstr("pdfsavepos"),            // WhatsIt::PdfSavePos
            _ => print_cstr("[unknown extension!]"),
        },
        _ => print_cstr("[unknown command code!]"),
    };
}
pub(crate) unsafe fn not_aat_font_error(cmd: Cmd, mut c: i32, f: usize) {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Cannot use ");
    print_cmd_chr(cmd, c);
    print_cstr(" with ");
    print(FONT_NAME[f]);
    print_cstr("; not an AAT font");
    error();
}
pub(crate) unsafe fn not_aat_gr_font_error(cmd: Cmd, mut c: i32, f: usize) {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Cannot use ");
    print_cmd_chr(cmd, c);
    print_cstr(" with ");
    print(FONT_NAME[f]);
    print_cstr("; not an AAT or Graphite font");
    error();
}
pub(crate) unsafe fn not_ot_font_error(cmd: Cmd, mut c: i32, f: usize) {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Cannot use ");
    print_cmd_chr(cmd, c);
    print_cstr(" with ");
    print(FONT_NAME[f]);
    print_cstr("; not an OpenType Layout font");
    error();
}
pub(crate) unsafe fn not_native_font_error(cmd: Cmd, c: i32, f: usize) {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Cannot use ");
    print_cmd_chr(cmd, c);
    print_cstr(" with ");
    print(FONT_NAME[f]);
    print_cstr("; not a native platform font");
    error();
}
/*:1434*/
pub(crate) unsafe fn id_lookup(mut j: i32, mut l: i32) -> i32 {
    let mut ll: i32 = 0;
    let mut h = 0;
    for k in j..=j + l - 1 {
        h = h + h + BUFFER[k as usize];
        while h >= HASH_PRIME as i32 {
            h = h - 8501;
        }
    }
    let mut p = h + HASH_BASE as i32;
    ll = l;
    for d in 0..=l - 1 {
        if BUFFER[(j + d) as usize] as i64 >= 65536 {
            ll += 1
        }
    }
    loop {
        if (*hash.offset(p as isize)).s1 > 0 {
            if length((*hash.offset(p as isize)).s1) == ll {
                if str_eq_buf((*hash.offset(p as isize)).s1, j) {
                    break;
                }
            }
        }
        if (*hash.offset(p as isize)).s0 == 0 {
            if no_new_control_sequence {
                p = UNDEFINED_CONTROL_SEQUENCE as i32;
            } else {
                if (*hash.offset(p as isize)).s1 > 0 {
                    if hash_high < hash_extra {
                        hash_high += 1;
                        (*hash.offset(p as isize)).s0 = hash_high + EQTB_SIZE as i32;
                        p = hash_high + EQTB_SIZE as i32;
                    } else {
                        loop {
                            if hash_used == HASH_BASE as i32 {
                                overflow("hash size", HASH_SIZE + hash_extra as usize);
                            }
                            hash_used -= 1;
                            if !((*hash.offset(hash_used as isize)).s1 != 0) {
                                break;
                            }
                        }
                        (*hash.offset(p as isize)).s0 = hash_used;
                        p = hash_used
                    }
                }
                if pool_ptr + ll > pool_size {
                    overflow("pool size", (pool_size - init_pool_ptr) as usize);
                }
                let d = cur_length();
                while pool_ptr > str_start[(str_ptr - TOO_BIG_CHAR) as usize] {
                    pool_ptr -= 1;
                    str_pool[(pool_ptr + l) as usize] = str_pool[pool_ptr as usize]
                }
                for k in j..=j + l - 1 {
                    let mut b = [0; 2];
                    for c in std::char::from_u32(BUFFER[k as usize] as u32)
                        .unwrap()
                        .encode_utf16(&mut b)
                    {
                        str_pool[pool_ptr as usize] = *c;
                        pool_ptr += 1
                    }
                }
                (*hash.offset(p as isize)).s1 = make_string();
                pool_ptr += d
            }
            break;
        } else {
            p = (*hash.offset(p as isize)).s0
        }
    }
    p
}
pub(crate) unsafe fn prim_lookup(mut s: str_number) -> usize {
    let mut l: i32 = 0;
    let mut p = if s <= BIGGEST_CHAR {
        if s < 0 {
            return UNDEFINED_PRIMITIVE as usize;
        } else {
            ((s as usize) % PRIM_PRIME) + 1
        }
    } else {
        let j = str_start[(s as i64 - 65536) as usize];
        if s == str_ptr {
            l = cur_length()
        } else {
            l = length(s)
        }
        let mut h = str_pool[j as usize] as usize;
        for k in (j + 1)..(j + l) {
            h = h + h + str_pool[k as usize] as usize;
            while h >= PRIM_PRIME {
                h = h - 431;
            }
        }
        h + 1
    };
    loop {
        if prim[p].s1 as i64 > 65536 {
            if length(prim[p].s1) - 1 == l {
                if PoolString::from(prim[p].s1 - 1) == PoolString::from(s) {
                    return p;
                }
            }
        } else if prim[p].s1 == 1 + s {
            return p;
        }
        if prim[p].s0 == 0i32 {
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
                prim[p].s1 = s + 1i32
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
            print_cstr("bottom level");
            return;
        }
        GroupCode::Simple | GroupCode::SemiSimple => {
            if group == GroupCode::SemiSimple {
                print_cstr("semi ");
            }
            print_cstr("simple");
        }
        GroupCode::HBox | GroupCode::AdjustedHBox => {
            if group == GroupCode::AdjustedHBox {
                print_cstr("adjusted ");
            }
            print_cstr("hbox");
        }
        GroupCode::VBox => print_cstr("vbox"),
        GroupCode::VTop => print_cstr("vtop"),
        GroupCode::Align | GroupCode::NoAlign => {
            if group == GroupCode::NoAlign {
                print_cstr("no ");
            }
            print_cstr("align");
        }
        GroupCode::Output => print_cstr("output"),
        GroupCode::Disc => print_cstr("disc"),
        GroupCode::Insert => print_cstr("insert"),
        GroupCode::VCenter => print_cstr("vcenter"),
        GroupCode::Math | GroupCode::MathChoice | GroupCode::MathShift | GroupCode::MathLeft => {
            print_cstr("math");
            if group == GroupCode::MathChoice {
                print_cstr(" choice");
            } else if group == GroupCode::MathShift {
                print_cstr(" shift");
            } else if group == GroupCode::MathLeft {
                print_cstr(" left");
            }
        }
    }
    print_cstr(" group (level ");
    print_int(level as i32);
    print_chr(')');
    if SAVE_STACK[saveptr - 1].val != 0 {
        if e {
            print_cstr(" entered at line ");
        } else {
            print_cstr(" at line ");
        }
        print_int(SAVE_STACK[saveptr - 1].val);
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
            input.loc = first;
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
            last = last + 4;
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
    let mut p = MEM[pseudo_files as usize].b32.s1;
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
        if *INTPAR(IntPar::tracing_nesting) > 0 {
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
        print_nl_cstr("Warning: end of ");
        print_group(cur_group, cur_level, SAVE_PTR, true);
        print_cstr(" of a different file");
        print_ln();
        if *INTPAR(IntPar::tracing_nesting) > 1 {
            INPUT_STACK[INPUT_PTR] = *input;
            show_context(&INPUT_STACK[..INPUT_PTR + 1]);
        }
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn if_warning(input: &mut input_state_t, input_stack: &[input_state_t]) {
    let mut w: bool = false;
    let mut base_ptr = input_stack.len() - 1;
    let mut i = IN_OPEN;
    w = false;
    while IF_STACK[i] == cond_ptr {
        if *INTPAR(IntPar::tracing_nesting) > 0 {
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
        print_nl_cstr("Warning: end of ");
        print_cmd_chr(Cmd::IfTest, cur_if as i32);
        if if_line != 0 {
            print_cstr(" entered on line ");
            print_int(if_line);
        }
        print_cstr(" of a different file");
        print_ln();
        if *INTPAR(IntPar::tracing_nesting) > 1 {
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
        print_nl_cstr("Warning: end of file when ");
        print_group(group, level, saveptr, true);
        print_cstr(" is incomplete");
        group = GroupCode::from(SAVE_STACK[saveptr].lvl);
        saveptr = SAVE_STACK[saveptr].val as usize
    }

    let mut condptr = cond_ptr;
    let mut iflimit = if_limit;
    let mut curif = cur_if;
    let mut ifline = if_line;
    while IF_STACK[IN_OPEN] != condptr {
        print_nl_cstr("Warning: end of file when ");
        print_cmd_chr(Cmd::IfTest, curif as i32);
        if iflimit == FiOrElseCode::Fi {
            print_esc_cstr("else");
        }
        if ifline != 0 {
            print_cstr(" entered on line ");
            print_int(ifline);
        }
        print_cstr(" is incomplete");
        let cp = condptr.unwrap();
        ifline = MEM[cp + 1].b32.s1;
        curif = MEM[cp].b16.s0 as i16;
        iflimit = FiOrElseCode::n(MEM[cp].b16.s1 as u8).unwrap();
        condptr = llist_link(cp);
    }
    print_ln();
    if *INTPAR(IntPar::tracing_nesting) > 1 {
        INPUT_STACK[INPUT_PTR] = *input;
        show_context(&INPUT_STACK[..INPUT_PTR + 1]);
    }
    if history == TTHistory::SPOTLESS {
        history = TTHistory::WARNING_ISSUED
    };
}
pub(crate) unsafe fn delete_sa_ref(mut q: usize) {
    let mut s: i16 = 0;
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
    let mut q: usize;
    let mut i: u16 = 0;
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
    i = MEM[p].b16.s1;
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
pub(crate) unsafe fn sa_w_def(p: usize, mut w: i32) {
    MEM[p + 1].b32.s0 += 1;
    if !(MEM[p + 2].b32.s1 == w) {
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
    if Cmd::from(EQTB[p].cmd) == t && EQTB[p].val.opt() == e {
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
pub(crate) unsafe fn save_for_after(mut t: i32) {
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
                    let p = get_avail() as i32;
                    MEM[p as usize].b32.s0 = tok;
                    *LLIST_link(p as usize) = input.loc;
                    input.loc = p;
                    input.start = p;
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
    if mag_set > 0 && *INTPAR(IntPar::mag) != mag_set {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Incompatible magnification (");
        print_int(*INTPAR(IntPar::mag));
        print_cstr(");");
        print_nl_cstr(" the previous value will be retained");

        help!(
            "I can handle only one magnification ratio per job. So I\'ve",
            "reverted to the magnification you used earlier on this run."
        );
        int_error(mag_set);
        geq_word_define(INT_BASE as usize + IntPar::mag as usize, mag_set);
    }
    if *INTPAR(IntPar::mag) <= 0 || *INTPAR(IntPar::mag) as i64 > 32768 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Illegal magnification has been changed to 1000");

        help!("The magnification ratio must be between 1 and 32768.");
        int_error(*INTPAR(IntPar::mag));
        geq_word_define(INT_BASE as usize + IntPar::mag as usize, 1000);
    }
    mag_set = *INTPAR(IntPar::mag);
}
pub(crate) unsafe fn token_show(p: Option<usize>) {
    if let Some(p) = p {
        show_token_list(llist_link(p), None, 10000000);
    };
}
pub(crate) unsafe fn print_meaning(cmd: Cmd, chr: i32) {
    print_cmd_chr(cmd, chr);
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
pub(crate) unsafe fn show_cur_cmd_chr(cmd: Cmd, chr: i32) {
    diagnostic(false, || {
        print_nl('{' as i32);
        if cur_list.mode != shown_mode {
            print_mode(cur_list.mode);
            print_cstr(": ");
            shown_mode = cur_list.mode
        }
        print_cmd_chr(cmd, chr);
        if *INTPAR(IntPar::tracing_ifs) > 0 {
            if cmd >= Cmd::IfTest {
                if cmd <= Cmd::FiOrElse {
                    print_cstr(": ");
                    let mut n;
                    let l;
                    if cmd == Cmd::FiOrElse {
                        print_cmd_chr(Cmd::IfTest, cur_if as i32);
                        print_chr(' ');
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
                    print_cstr("(level ");
                    print_int(n);
                    print_chr(')');
                    if l != 0 {
                        print_cstr(" entered on line ");
                        print_int(l);
                    }
                }
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
        if input.state != InputState::TokenList {
            if input.name > 19 || base_ptr == 0 {
                bottom_line = true
            }
        }
        if base_ptr == last_ptr || bottom_line || nn < *INTPAR(IntPar::error_context_lines) {
            /*324: */
            if base_ptr == last_ptr
                || input.state != InputState::TokenList
                || input.index != Btl::BackedUp
                || input.loc.opt().is_some()
            {
                tally = 0i32;
                let old_setting = selector;
                let l;
                if input.state != InputState::TokenList {
                    if input.name <= 17 {
                        if input.name == 0 {
                            if base_ptr == 0 {
                                print_nl_cstr("<*>");
                            } else {
                                print_nl_cstr("<insert> ");
                            }
                        } else {
                            print_nl_cstr("<read ");
                            if input.name == 17 {
                                print_chr('*');
                            } else {
                                print_int(input.name - 1);
                            }
                            print_chr('>');
                        }
                    } else {
                        print_nl_cstr("l.");
                        if input.index as usize == IN_OPEN {
                            print_int(line);
                        } else {
                            print_int(LINE_STACK[(input.index as i32 + 1) as usize]);
                        }
                    }
                    print_chr(' ');
                    l = tally;
                    tally = 0;
                    selector = Selector::PSEUDO;
                    trick_count = 1000000;
                    let j = if BUFFER[input.limit as usize] == *INTPAR(IntPar::end_line_char) {
                        input.limit
                    } else {
                        input.limit + 1
                    };
                    if j > 0 {
                        for i in input.start..j {
                            if i == input.loc {
                                first_count = tally;
                                trick_count = tally + 1 + error_line - half_error_line;
                                if trick_count < error_line {
                                    trick_count = error_line
                                }
                            }
                            print_char(BUFFER[i as usize]);
                        }
                    }
                } else {
                    match input.index {
                        Btl::Parameter => print_nl_cstr("<argument> "),
                        Btl::UTemplate | Btl::VTemplate => print_nl_cstr("<template> "),
                        Btl::BackedUp | Btl::BackedUpChar => {
                            if input.loc.opt().is_none() {
                                print_nl_cstr("<recently read> ");
                            } else {
                                print_nl_cstr("<to be read again> ");
                            }
                        }
                        Btl::Inserted => print_nl_cstr("<inserted text> "),
                        Btl::Macro => {
                            print_ln();
                            print_cs(input.name);
                        }
                        Btl::OutputText => print_nl_cstr("<output> "),
                        Btl::EveryParText => print_nl_cstr("<everypar> "),
                        Btl::EveryMathText => print_nl_cstr("<everymath> "),
                        Btl::EveryDisplayText => print_nl_cstr("<everydisplay> "),
                        Btl::EveryHBoxText => print_nl_cstr("<everyhbox> "),
                        Btl::EveryVBoxText => print_nl_cstr("<everyvbox> "),
                        Btl::EveryJobText => print_nl_cstr("<everyjob> "),
                        Btl::EveryCRText => print_nl_cstr("<everycr> "),
                        Btl::MarkText => print_nl_cstr("<mark> "),
                        Btl::EveryEOFText => print_nl_cstr("<everyeof> "),
                        Btl::InterCharText => print_nl_cstr("<XeTeXinterchartoks> "),
                        Btl::WriteText => print_nl_cstr("<write> "),
                        Btl::TectonicCodaText => print_nl_cstr("<TectonicCodaTokens> "),
                        //_ => print_nl('?' as i32),
                    }
                    l = tally;
                    tally = 0i32;
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
                        show_token_list(input.start.opt(), input.loc.opt(), 100000);
                    } else {
                        show_token_list(
                            MEM[input.start as usize].b32.s1.opt(),
                            input.loc.opt(),
                            100_000,
                        );
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
                    print_cstr("...");
                    p = l + first_count - half_error_line + 3;
                    n = half_error_line
                }
                for q in p..first_count {
                    print_raw_char(trick_buf[(q % error_line) as usize], true);
                }
                print_ln();
                for _ in 0..n {
                    print_raw_char(' ' as i32 as UTF16_code, true);
                }
                let p = if m + n <= error_line {
                    first_count + m
                } else {
                    first_count + (error_line - n - 3)
                };
                for q in first_count..p {
                    print_raw_char(trick_buf[(q % error_line) as usize], true);
                }
                if m + n > error_line {
                    print_cstr("...");
                }
                nn += 1
            }
        } else if nn == *INTPAR(IntPar::error_context_lines) {
            print_nl_cstr("...");
            nn += 1
        }
        if bottom_line {
            break;
        }
        base_ptr -= 1
    }
}
pub(crate) unsafe fn begin_token_list(input: &mut input_state_t, p: usize, t: Btl) {
    if INPUT_PTR > MAX_IN_STACK {
        MAX_IN_STACK = INPUT_PTR;
        if INPUT_PTR == STACK_SIZE {
            overflow("input stack size", STACK_SIZE);
        }
    }
    INPUT_STACK[INPUT_PTR] = *input; // push
    INPUT_PTR += 1;
    input.state = InputState::TokenList;
    input.start = p as i32;
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
        MEM[p].b32.s0 += 1;
        if t == Btl::Macro {
            input.limit = PARAM_PTR as i32
        } else {
            input.loc = *LLIST_link(p);
            if *INTPAR(IntPar::tracing_macros) > 1i32 {
                diagnostic(false, || {
                    print_nl_cstr("");
                    match t {
                        Btl::MarkText => print_esc_cstr("mark"),
                        Btl::WriteText => print_esc_cstr("write"),
                        _ => {
                            print_cmd_chr(
                                Cmd::AssignToks,
                                (t as i32) + LOCAL_BASE as i32 + (Local::output_routine as i32)
                                    - (Btl::OutputText) as i32,
                            );
                        }
                    }
                    print_cstr("->");
                    token_show(Some(p));
                });
            }
        }
    } else {
        input.loc = p as i32;
    };
}
pub(crate) unsafe fn end_token_list(input: &mut input_state_t) {
    if ![Btl::Parameter, Btl::UTemplate, Btl::VTemplate].contains(&input.index) {
        if [Btl::BackedUp, Btl::BackedUpChar, Btl::Inserted].contains(&input.index) {
            flush_list(input.start.opt());
        } else {
            delete_token_ref(input.start as usize);
            if input.index == Btl::Macro {
                while PARAM_PTR as i32 > input.limit {
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
        && input.loc.opt().is_none()
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
    input.start = p as i32;
    input.index = Btl::BackedUp;
    input.loc = p as i32;
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
    input.start = first;
    input.state = InputState::MidLine;
    input.name = 0;
    input.synctex_tag = 0;
}
pub(crate) unsafe fn end_file_reading(input: &mut input_state_t) {
    first = input.start;
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
pub(crate) unsafe fn check_outer_validity(input: &mut input_state_t, cs: &mut i32) -> bool {
    let mut spacer = false;
    if scanner_status != ScannerStatus::Normal {
        deletions_allowed = false;
        if *cs != 0 {
            if input.state == InputState::TokenList || input.name < 1 || input.name > 17 {
                let p = get_avail();
                MEM[p].b32.s0 = CS_TOKEN_FLAG + *cs;
                begin_token_list(input, p, Btl::BackedUp);
            }
            spacer = true;
        }
        if scanner_status != ScannerStatus::Normal && scanner_status != ScannerStatus::Skipping {
            /*350:*/
            runaway();
            if *cs == 0 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("File ended");
            } else {
                *cs = 0;
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Forbidden control sequence found");
            }
            let mut p = get_avail();
            match scanner_status {
                ScannerStatus::Defining => {
                    print_cstr(" while scanning definition");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32
                }
                ScannerStatus::Matching => {
                    print_cstr(" while scanning use");
                    MEM[p].b32.s0 = par_token;
                    long_state = Cmd::OuterCall as u8
                }
                ScannerStatus::Aligning => {
                    print_cstr(" while scanning preamble");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
                    let q = p;
                    p = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_CR as i32;
                    align_state = -1_000_000;
                }
                ScannerStatus::Absorbing => {
                    print_cstr(" while scanning text");
                    MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32
                }
                _ => unreachable!(),
            }
            begin_token_list(input, p, Btl::Inserted);
            print_cstr(" of ");
            sprint_cs(warning_index);
            help!(
                "I suspect you have forgotten a `}\', causing me",
                "to read past where you wanted me to stop.",
                "I\'ll try to recover; but if the error is serious,",
                "you\'d better type `E\' or `X\' now and fix your file."
            );
            error();
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Incomplete ");
            print_cmd_chr(Cmd::IfTest, cur_if as i32);
            print_cstr("; all text was ignored after line ");
            print_int(skip_line);
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
/* These macros are kinda scary, but convenient */
pub(crate) unsafe fn get_next(input: &mut input_state_t) -> (Cmd, i32, i32) {
    let mut ochr = None;
    let mut ocmd = None;
    'c_63502: loop {
        let mut current_block: u64;
        let mut cs = 0;
        if input.state != InputState::TokenList {
            /*355:*/
            'c_63807: loop
            /*357:*/
            {
                if input.loc <= input.limit {
                    let mut chr = BUFFER[input.loc as usize];
                    input.loc += 1;
                    'c_65186: loop {
                        ochr = Some(chr);
                        let cmd = Cmd::from(*CAT_CODE(chr as usize) as u16);
                        ocmd = Some(cmd);
                        match (input.state, cmd) {
                            (InputState::MidLine, IGNORE)
                            | (InputState::SkipBlanks, IGNORE)
                            | (InputState::NewLine, IGNORE)
                            | (InputState::SkipBlanks, Cmd::Spacer)
                            | (InputState::NewLine, Cmd::Spacer) => break,
                            (InputState::MidLine, ESCAPE)
                            | (InputState::SkipBlanks, ESCAPE)
                            | (InputState::NewLine, ESCAPE) => {
                                if input.loc > input.limit {
                                    current_block = 17833034027772472439;
                                    break 'c_63807;
                                } else {
                                    current_block = 7720778817628725688;
                                    break 'c_63807;
                                }
                            }
                            (InputState::MidLine, Cmd::ActiveChar)
                            | (InputState::SkipBlanks, Cmd::ActiveChar)
                            | (InputState::NewLine, Cmd::ActiveChar) => {
                                cs = chr + 1;
                                let mut cmd = Cmd::from(EQTB[cs as usize].cmd);
                                let mut chr = EQTB[cs as usize].val;
                                input.state = InputState::MidLine;
                                if cmd >= Cmd::OuterCall {
                                    if check_outer_validity(input, &mut cs) {
                                        cmd = Cmd::Spacer;
                                        chr = ' ' as i32;
                                    }
                                }
                                ocmd = Some(cmd);
                                ochr = Some(chr);
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::SupMark)
                            | (InputState::SkipBlanks, Cmd::SupMark)
                            | (InputState::NewLine, Cmd::SupMark) => {
                                if !(chr == BUFFER[input.loc as usize]) {
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                }
                                if !(input.loc < input.limit) {
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                }
                                let mut sup_count = 2;
                                while sup_count < 6
                                    && input.loc + 2 * sup_count as i32 - 2 <= input.limit
                                    && chr == BUFFER[(input.loc + sup_count as i32 - 1) as usize]
                                {
                                    sup_count += 1
                                }
                                for d in 1..=sup_count as i32 {
                                    if !(BUFFER
                                        [(input.loc + sup_count as i32 - 2 + d as i32) as usize]
                                        >= '0' as i32
                                        && BUFFER[(input.loc + sup_count as i32 - 2 + d as i32)
                                            as usize]
                                            <= '9' as i32
                                        || BUFFER[(input.loc + sup_count as i32 - 2 + d as i32)
                                            as usize]
                                            >= 'a' as i32
                                            && BUFFER[(input.loc + sup_count as i32 - 2 + d as i32)
                                                as usize]
                                                <= 'f' as i32)
                                    {
                                        let c = BUFFER[(input.loc + 1) as usize];
                                        if !(c < 128) {
                                            ochr = Some(chr);
                                            current_block = 8567661057257693057;
                                            break 'c_63807;
                                        }
                                        input.loc = input.loc + 2;
                                        chr = if c < 64 { c + 64 } else { c - 64 };
                                        continue 'c_65186;
                                    }
                                }
                                chr = 0;
                                for d in 1..=sup_count as i32 {
                                    let c = BUFFER
                                        [(input.loc + sup_count as i32 - 2 + d as i32) as usize];
                                    if c <= '9' as i32 {
                                        chr = 16 * chr + c - '0' as i32
                                    } else {
                                        chr = 16 * chr + c - 'a' as i32 + 10
                                    }
                                }
                                if chr > BIGGEST_USV as i32 {
                                    ochr = Some(BUFFER[input.loc as usize]);
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                } else {
                                    input.loc += 2 * sup_count as i32 - 1;
                                }
                            }
                            (InputState::MidLine, INVALID_CHAR)
                            | (InputState::SkipBlanks, INVALID_CHAR)
                            | (InputState::NewLine, INVALID_CHAR) => {
                                if file_line_error_style_p != 0 {
                                    print_file_line();
                                } else {
                                    print_nl_cstr("! ");
                                }
                                print_cstr("Text line contains an invalid character");
                                help!(
                                    "A funny symbol that I can\'t read has just been input.",
                                    "Continue, and I\'ll forget that it ever happened."
                                );
                                deletions_allowed = false;
                                error();
                                deletions_allowed = true;
                                continue 'c_63502;
                            }
                            (InputState::MidLine, Cmd::Spacer) => {
                                input.state = InputState::SkipBlanks;
                                ochr = Some(' ' as i32);
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::CarRet) => {
                                input.loc = input.limit + 1;
                                ocmd = Some(Cmd::Spacer);
                                ochr = Some(' ' as i32);
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::Comment)
                            | (InputState::NewLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::CarRet) => {
                                input.loc = input.limit + 1;
                                break;
                            }
                            (InputState::NewLine, Cmd::CarRet) => {
                                input.loc = input.limit + 1;
                                cs = par_loc;
                                let mut cmd = Cmd::from(EQTB[cs as usize].cmd);
                                let mut chr = EQTB[cs as usize].val;
                                if cmd >= Cmd::OuterCall {
                                    if check_outer_validity(input, &mut cs) {
                                        cmd = Cmd::Spacer;
                                        chr = ' ' as i32;
                                    }
                                }
                                ocmd = Some(cmd);
                                ochr = Some(chr);
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::LeftBrace) => {
                                align_state += 1;
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::SkipBlanks, Cmd::LeftBrace)
                            | (InputState::NewLine, Cmd::LeftBrace) => {
                                input.state = InputState::MidLine;
                                align_state += 1;
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::RightBrace) => {
                                align_state -= 1;
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::SkipBlanks, Cmd::RightBrace)
                            | (InputState::NewLine, Cmd::RightBrace) => {
                                input.state = InputState::MidLine;
                                align_state -= 1;
                                current_block = 14956172121224201915;
                                break 'c_63807;
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
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            _ => {
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                        }
                    }
                } else {
                    input.state = InputState::NewLine;
                    if input.name > 17 {
                        /*374:*/
                        line += 1; /*367:*/
                        first = input.start;
                        if !force_eof {
                            if input.name <= 19 {
                                if pseudo_input(input) {
                                    input.limit = last
                                } else if let Some(l) = LOCAL(Local::every_eof)
                                    .opt()
                                    .filter(|_| !EOF_SEEN[input.index as usize])
                                {
                                    input.limit = first - 1;
                                    EOF_SEEN[input.index as usize] = true;
                                    begin_token_list(input, l, Btl::EveryEOFText);
                                    continue 'c_63502;
                                } else {
                                    force_eof = true
                                }
                            } else if input_line(INPUT_FILE[input.index as usize].as_mut().unwrap())
                            {
                                input.limit = last
                            } else if let Some(l) = LOCAL(Local::every_eof)
                                .opt()
                                .filter(|_| !EOF_SEEN[input.index as usize])
                            {
                                input.limit = first - 1;
                                EOF_SEEN[input.index as usize] = true;
                                begin_token_list(input, l, Btl::EveryEOFText);
                                continue 'c_63502;
                            } else {
                                force_eof = true
                            }
                        }
                        if force_eof {
                            if *INTPAR(IntPar::tracing_nesting) > 0 {
                                if GRP_STACK[IN_OPEN] != cur_boundary
                                    || IF_STACK[IN_OPEN] != cond_ptr
                                {
                                    file_warning(input);
                                }
                            }
                            if input.name >= 19 {
                                print_chr(')');
                                open_parens -= 1;
                                rust_stdout.as_mut().unwrap().flush().unwrap();
                            }
                            force_eof = false;
                            end_file_reading(input);
                            if check_outer_validity(input, &mut cs) {
                                ocmd = Some(Cmd::Spacer);
                                ochr = Some(' ' as i32);
                            }
                            continue 'c_63502;
                        } else {
                            if *INTPAR(IntPar::end_line_char) < 0
                                || *INTPAR(IntPar::end_line_char) > 255
                            {
                                input.limit -= 1
                            } else {
                                BUFFER[input.limit as usize] = *INTPAR(IntPar::end_line_char)
                            }
                            first = input.limit + 1;
                            input.loc = input.start
                        }
                    } else {
                        if input.name != 0 {
                            return (Cmd::Relax, 0, cs);
                        }
                        if INPUT_PTR > 0 {
                            current_block = 4001239642700071046;
                            break;
                        } else {
                            current_block = 15055213890147597004;
                            break;
                        }
                    }
                }
            }
            match current_block {
                14956172121224201915 => {}
                _ => {
                    match current_block {
                        8567661057257693057 => {
                            input.state = InputState::MidLine;
                            current_block = 14956172121224201915;
                        }
                        7720778817628725688 => {
                            let mut k;
                            let mut cat;
                            let mut chr;
                            'c_65963: loop {
                                k = input.loc;
                                chr = BUFFER[k as usize];
                                cat = Cmd::from(*CAT_CODE(chr as usize) as u16);
                                k += 1;
                                if cat == Cmd::Letter {
                                    input.state = InputState::SkipBlanks; // TODO: check
                                } else if cat == Cmd::Spacer {
                                    input.state = InputState::SkipBlanks;
                                } else {
                                    input.state = InputState::MidLine;
                                }
                                if cat == Cmd::Letter && k <= input.limit {
                                    loop
                                    /*368:*/
                                    {
                                        chr = BUFFER[k as usize];
                                        cat = Cmd::from(*CAT_CODE(chr as usize) as u16);
                                        k += 1;
                                        if !(cat == Cmd::Letter && k <= input.limit) {
                                            break;
                                        }
                                    }
                                    if !(cat == Cmd::SupMark
                                        && BUFFER[k as usize] == chr
                                        && k < input.limit)
                                    {
                                        current_block = 5873035170358615968;
                                        break;
                                    }
                                    /* Special characters: either ^^X, or up to six
                                     * ^'s followed by one hex character for each
                                     * ^. */
                                    let mut sup_count_save: i32 = 0;
                                    /* How many ^'s are there? */
                                    let mut sup_count = 2;
                                    while sup_count < 6
                                        && k + 2 * sup_count as i32 - 2 <= input.limit
                                        && BUFFER[(k + sup_count as i32 - 1) as usize] == chr
                                    {
                                        sup_count += 1;
                                    }
                                    /* If they are followed by a sufficient number of
                                     * hex characters, treat it as an extended ^^^
                                     * sequence. If not, treat it as original-style
                                     * ^^X. */
                                    sup_count_save = sup_count as i32;
                                    for d in 1..=sup_count_save {
                                        if !(BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize]
                                            >= '0' as i32
                                            && BUFFER
                                                [(k + sup_count as i32 - 2 + d as i32) as usize]
                                                <= '9' as i32
                                            || BUFFER
                                                [(k + sup_count as i32 - 2 + d as i32) as usize]
                                                >= 'a' as i32
                                                && BUFFER[(k + sup_count as i32 - 2 + d as i32)
                                                    as usize]
                                                    <= 'f' as i32)
                                        {
                                            /* Non-hex: do it old style */
                                            let c = BUFFER[(k + 1) as usize];
                                            if c < 128 {
                                                if c < 64 {
                                                    BUFFER[(k - 1) as usize] = c + 64
                                                } else {
                                                    BUFFER[(k - 1) as usize] = c - 64
                                                }
                                                let d = 2;
                                                input.limit = input.limit - d as i32;
                                                while k <= input.limit {
                                                    BUFFER[k as usize] =
                                                        BUFFER[(k + d as i32) as usize];
                                                    k += 1
                                                }
                                                continue 'c_65963;
                                            } else {
                                                sup_count = 0;
                                            }
                                        }
                                    }
                                    if !(sup_count as i32 > 0) {
                                        current_block = 5873035170358615968;
                                        break;
                                    }

                                    chr = 0;

                                    for d in 1..=sup_count as i32 {
                                        let c =
                                            BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize];
                                        chr = if c <= '9' as i32 {
                                            16 * chr + c - '0' as i32
                                        } else {
                                            16 * chr + c - 'a' as i32 + 10
                                        };
                                    }

                                    if chr > BIGGEST_USV as i32 {
                                        chr = BUFFER[k as usize];
                                        current_block = 5873035170358615968;
                                        break;
                                    } else {
                                        BUFFER[(k - 1) as usize] = chr;
                                        let d = (2 * sup_count as i32 - 1) as i16;
                                        input.limit = input.limit - d as i32;
                                        while k <= input.limit {
                                            BUFFER[k as usize] = BUFFER[(k + d as i32) as usize];
                                            k += 1
                                        }
                                    }
                                } else {
                                    if !(cat == Cmd::SupMark
                                        && BUFFER[k as usize] == chr
                                        && k < input.limit)
                                    {
                                        current_block = 1604201581803946138;
                                        break;
                                    }
                                    let mut sup_count_save_0: i32 = 0;
                                    let mut sup_count = 2;
                                    while sup_count < 6
                                        && k + 2 * sup_count as i32 - 2 <= input.limit
                                        && BUFFER[(k + sup_count as i32 - 1) as usize] == chr
                                    {
                                        sup_count += 1
                                    }
                                    sup_count_save_0 = sup_count as i32;
                                    for d in 1..=sup_count_save_0 {
                                        if !IS_LC_HEX(
                                            BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize],
                                        ) {
                                            let c = BUFFER[(k + 1) as usize];
                                            if c < 128 {
                                                if c < 64 {
                                                    BUFFER[(k - 1) as usize] = c + 64
                                                } else {
                                                    BUFFER[(k - 1) as usize] = c - 64
                                                }
                                                let d = 2;
                                                input.limit = input.limit - d as i32;
                                                while k <= input.limit {
                                                    BUFFER[k as usize] =
                                                        BUFFER[(k + d as i32) as usize];
                                                    k += 1
                                                }
                                                continue 'c_65963;
                                            } else {
                                                sup_count = 0;
                                            }
                                        }
                                    }
                                    if !(sup_count > 0) {
                                        current_block = 1604201581803946138;
                                        break;
                                    }
                                    chr = 0;
                                    for d in 1..=sup_count as i32 {
                                        let c =
                                            BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize];
                                        if c <= '9' as i32 {
                                            chr = 16 * chr + c - '0' as i32
                                        } else {
                                            chr = 16 * chr + c - 'a' as i32 + 10
                                        }
                                    }

                                    if chr > BIGGEST_USV as i32 {
                                        chr = BUFFER[k as usize];
                                        current_block = 1604201581803946138;
                                        break;
                                    } else {
                                        BUFFER[(k - 1) as usize] = chr;
                                        let d = (2 * sup_count as i32 - 1) as i16;
                                        input.limit = input.limit - d as i32;
                                        while k <= input.limit {
                                            BUFFER[k as usize] = BUFFER[(k + d as i32) as usize];
                                            k += 1
                                        }
                                    }
                                }
                            }
                            ochr = Some(chr);
                            match current_block {
                                5873035170358615968 => {
                                    if cat != Cmd::Letter {
                                        k -= 1
                                    }
                                    if k > input.loc + 1 {
                                        cs = id_lookup(input.loc, k - input.loc);
                                        input.loc = k;
                                        current_block = 10802200937357087535;
                                    } else {
                                        current_block = 1604201581803946138;
                                    }
                                }
                                _ => {}
                            }
                            match current_block {
                                10802200937357087535 => {}
                                _ => {
                                    if BUFFER[input.loc as usize] as i64 > 65535 {
                                        cs = id_lookup(input.loc, 1i32);
                                        input.loc += 1
                                    } else {
                                        cs = 1i32
                                            + (0x10ffffi32 + 1i32)
                                            + BUFFER[input.loc as usize];
                                        input.loc += 1
                                    }
                                    current_block = 10802200937357087535;
                                }
                            }
                        }
                        17833034027772472439 => {
                            cs = 1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32);
                            current_block = 10802200937357087535;
                        }
                        4001239642700071046 => {
                            end_file_reading(input);
                            continue;
                        }
                        _ =>
                        /* Tectonic extension: we add a \TectonicCodaTokens toklist
                         * that gets inserted at the very very end of processing if no
                         * \end or \dump has been seen. We just use a global state
                         * variable to make sure it only gets inserted once. */
                        {
                            match (
                                used_tectonic_coda_tokens,
                                LOCAL(Local::TectonicCodaTokens).opt(),
                            ) {
                                (false, Some(l)) => {
                                    used_tectonic_coda_tokens = true; /* token list but no tokens left */
                                    begin_token_list(input, l, Btl::TectonicCodaText);
                                    continue;
                                }
                                _ => {
                                    if u8::from(selector) < u8::from(Selector::LOG_ONLY) {
                                        open_log_file();
                                    }
                                    fatal_error("*** (job aborted, no legal \\end found)");
                                }
                            }
                        }
                    }
                    match current_block {
                        14956172121224201915 => {}
                        _ => {
                            // found:
                            let mut cmd = Cmd::from(EQTB[cs as usize].cmd);
                            let mut chr = EQTB[cs as usize].val;
                            if cmd >= Cmd::OuterCall {
                                if check_outer_validity(input, &mut cs) {
                                    cmd = Cmd::Spacer;
                                    chr = ' ' as i32;
                                }
                            }
                            ocmd = Some(cmd);
                            ochr = Some(chr);
                        }
                    }
                }
            }
        } else if let Some(loc) = input.loc.opt() {
            /* if we're inputting from a non-null token list: */
            let t = MEM[loc].b32.s0;
            input.loc = *LLIST_link(loc);
            if t >= CS_TOKEN_FLAG {
                cs = t - CS_TOKEN_FLAG;
                let mut cmd = Cmd::from(EQTB[cs as usize].cmd);
                let mut chr = EQTB[cs as usize].val;
                if cmd >= Cmd::OuterCall {
                    if cmd == Cmd::DontExpand {
                        /*370:*/
                        cs = MEM[input.loc.opt().unwrap()].b32.s0 - CS_TOKEN_FLAG;
                        input.loc = None.tex_int();
                        cmd = Cmd::from(EQTB[cs as usize].cmd);
                        chr = EQTB[cs as usize].val;
                        if cmd > MAX_COMMAND {
                            cmd = Cmd::Relax;
                            chr = NO_EXPAND_FLAG;
                        }
                    } else {
                        if check_outer_validity(input, &mut cs) {
                            cmd = Cmd::Spacer;
                            chr = ' ' as i32;
                        }
                    }
                }
                ocmd = Some(cmd);
                ochr = Some(chr);
            } else {
                let cmd = Cmd::from((t / MAX_CHAR_VAL) as u16);
                let chr = t % MAX_CHAR_VAL;
                ochr = Some(chr);
                ocmd = Some(cmd);
                match cmd {
                    Cmd::LeftBrace => {
                        align_state += 1;
                    }
                    Cmd::RightBrace => {
                        align_state -= 1;
                    }
                    OUT_PARAM => {
                        begin_token_list(
                            input,
                            PARAM_STACK[(input.limit + chr - 1) as usize] as usize,
                            Btl::Parameter,
                        );
                        continue;
                    }
                    _ => {}
                }
            }
        } else {
            end_token_list(input);
            continue;
        }
        let mut cmd = ocmd.unwrap();
        if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state == 0 {
            /*818:*/
            if scanner_status == ScannerStatus::Aligning {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
            if let Some(ca) = cur_align {
                cmd = Cmd::from(MEM[ca + 5].b32.s0 as u16);
                MEM[ca + 5].b32.s0 = ochr.unwrap();
                if cmd == Cmd::Omit {
                    begin_token_list(input, OMIT_TEMPLATE, Btl::VTemplate);
                } else {
                    begin_token_list(input, MEM[ca + 2].b32.s1 as usize, Btl::VTemplate);
                }
                align_state = 1_000_000;
            } else {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
        } else {
            return (cmd, ochr.unwrap(), cs);
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
pub(crate) unsafe fn macro_call(input: &mut input_state_t, chr: i32, cs: i32) {
    let mut p: i32 = None.tex_int();
    let mut rbrace_ptr: i32 = None.tex_int();
    let mut match_chr: UTF16_code = 0;
    let save_scanner_status = scanner_status;
    let save_warning_index = warning_index;
    warning_index = cs;
    let ref_count = chr as usize;
    let mut r = llist_link(ref_count).unwrap();
    let mut n = 0_i16;
    if *INTPAR(IntPar::tracing_macros) > 0 {
        /*419:*/
        diagnostic(false, || {
            print_ln();
            print_cs(warning_index);
            token_show(Some(ref_count));
        });
    }
    if MEM[r].b32.s0 == PROTECTED_TOKEN {
        r = llist_link(r).unwrap();
    }
    if MEM[r].b32.s0 != END_MATCH_TOKEN {
        /*409:*/
        scanner_status = ScannerStatus::Matching;
        let mut unbalance = 0;
        long_state = EQTB[cs as usize].cmd as u8;

        if long_state as u16 >= Cmd::OuterCall as u16 {
            long_state = (long_state as i32 - 2) as u8
        }

        let mut m = 0;
        let mut s = None;
        let mut cont = false;
        's_135: loop {
            if !cont {
                *LLIST_link(TEMP_HEAD) = None.tex_int();
                if MEM[r].b32.s0 >= END_MATCH_TOKEN || MEM[r].b32.s0 < MATCH_TOKEN {
                    s = None;
                } else {
                    match_chr = (MEM[r].b32.s0 - MATCH_TOKEN) as UTF16_code;
                    s = llist_link(r);
                    r = s.unwrap();
                    p = TEMP_HEAD as i32;
                    m = 0;
                }
            }
            cont = false;

            let mut tok = get_token(input).0;
            if tok == MEM[r].b32.s0 {
                /*412:*/
                r = llist_link(r).unwrap();
                if MEM[r].b32.s0 >= MATCH_TOKEN && MEM[r].b32.s0 <= END_MATCH_TOKEN {
                    if tok < LEFT_BRACE_LIMIT {
                        align_state -= 1
                    }
                } else {
                    cont = true;
                    continue;
                }
            } else {
                if s != Some(r) {
                    if let Some(s) = s {
                        let mut t = s;
                        loop {
                            let q = get_avail();
                            *LLIST_link(p as usize) = Some(q).tex_int();
                            MEM[q].b32.s0 = MEM[t].b32.s0;
                            p = q as i32;
                            m += 1;
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
                        /*416:*/
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Use of ");
                        sprint_cs(warning_index);
                        print_cstr(" doesn\'t match its definition");
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
                    if long_state as u16 != Cmd::LongCall as u16 {
                        /*414:*/
                        if long_state as u16 == Cmd::Call as u16 {
                            runaway(); /*411:*/
                            if file_line_error_style_p != 0 {
                                print_file_line(); /*413:*/
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("Paragraph ended before ");
                            sprint_cs(warning_index);
                            print_cstr(" was complete");
                            help!(
                                "I suspect you\'ve forgotten a `}\', causing me to apply this",
                                "control sequence to too much text. How can we recover?",
                                "My plan is to forget the whole thing and hope for the best."
                            );
                            back_error(input, tok);
                        }

                        pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                        align_state = align_state - unbalance;

                        for m in 0..=(n as usize) {
                            flush_list(pstack[m].opt());
                        }

                        return exit(save_scanner_status, save_warning_index);
                    }
                }

                if tok < RIGHT_BRACE_LIMIT {
                    if tok < LEFT_BRACE_LIMIT {
                        /*417:*/
                        unbalance = 1;

                        loop {
                            let q = if let Some(a) = avail {
                                avail = llist_link(a);
                                *LLIST_link(a) = None.tex_int();
                                a
                            } else {
                                get_avail()
                            };

                            *LLIST_link(p as usize) = Some(q).tex_int();
                            MEM[q].b32.s0 = tok;
                            p = q as i32;

                            tok = get_token(input).0;

                            if tok == par_token {
                                if long_state as u16 != Cmd::LongCall as u16 {
                                    /*414:*/
                                    if long_state as u16 == Cmd::Call as u16 {
                                        runaway();
                                        if file_line_error_style_p != 0 {
                                            print_file_line();
                                        } else {
                                            print_nl_cstr("! ");
                                        }
                                        print_cstr("Paragraph ended before ");
                                        sprint_cs(warning_index);
                                        print_cstr(" was complete");
                                        help!("I suspect you\'ve forgotten a `}\', causing me to apply this",
                                        "control sequence to too much text. How can we recover?",
                                        "My plan is to forget the whole thing and hope for the best.");
                                        back_error(input, tok);
                                    }

                                    pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                                    align_state = align_state - unbalance;

                                    for m in 0..=(n as usize) {
                                        flush_list(pstack[m].opt());
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
                        rbrace_ptr = p;

                        let q = get_avail();
                        *LLIST_link(p as usize) = Some(q).tex_int();
                        MEM[q].b32.s0 = tok;
                        p = q as i32;
                    } else {
                        /* 413 */
                        back_input(input, tok);

                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Argument of ");
                        sprint_cs(warning_index);
                        print_cstr(" has an extra }");
                        help!(
                            "I\'ve run across a `}\' that doesn\'t seem to match anything.",
                            "For example, `\\def\\a#1{...}\' and `\\a}\' would produce",
                            "this error. If you simply proceed now, the `\\par\' that",
                            "I\'ve just inserted will cause me to report a runaway",
                            "argument that might be the root of the problem. But if",
                            "your `}\' was spurious, just type `2\' and it will go away."
                        );
                        align_state += 1;
                        long_state = Cmd::Call as u8;
                        tok = par_token;
                        ins_error(input, tok);
                        cont = true;
                        continue;
                    }
                } else {
                    if tok == SPACE_TOKEN {
                        if MEM[r].b32.s0 <= END_MATCH_TOKEN {
                            if MEM[r].b32.s0 >= MATCH_TOKEN {
                                cont = true;
                                continue;
                            }
                        }
                    }

                    let q = get_avail();
                    *LLIST_link(p as usize) = Some(q).tex_int();
                    MEM[q].b32.s0 = tok;
                    p = q as i32;
                }

                m += 1;

                if MEM[r].b32.s0 > END_MATCH_TOKEN {
                    cont = true;
                    continue;
                }
                if MEM[r].b32.s0 < MATCH_TOKEN {
                    cont = true;
                    continue;
                }
            }

            // found:
            if s.is_some() {
                /*418:*/
                if m == 1 && MEM[p as usize].b32.s0 < RIGHT_BRACE_LIMIT && p != TEMP_HEAD as i32 {
                    *LLIST_link(rbrace_ptr as usize) = None.tex_int();
                    *LLIST_link(p as usize) = avail.tex_int();
                    avail = Some(p as usize);
                    p = *LLIST_link(TEMP_HEAD);
                    pstack[n as usize] = *LLIST_link(p as usize);
                    *LLIST_link(p as usize) = avail.tex_int();
                    avail = Some(p as usize);
                } else {
                    pstack[n as usize] = *LLIST_link(TEMP_HEAD)
                }
                n += 1;
                if *INTPAR(IntPar::tracing_macros) > 0 {
                    diagnostic(false, || {
                        print_nl(match_chr as str_number);
                        print_int(n as i32);
                        print_cstr("<-");
                        show_token_list(pstack[(n as i32 - 1) as usize].opt(), None, 1000);
                    });
                }
            }
            if !(MEM[r].b32.s0 != END_MATCH_TOKEN) {
                break;
            }
        }
    }

    while input.state == InputState::TokenList
        && input.loc.opt().is_none()
        && input.index != Btl::VTemplate
    {
        end_token_list(input);
    }

    begin_token_list(input, ref_count, Btl::Macro);
    input.name = warning_index;
    input.loc = *LLIST_link(r);

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
pub(crate) unsafe fn find_sa_element(t: ValLevel, mut n: i32, mut w: bool) {
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
                MEM[p + 1].b32.s1 = 0;
                GlueSpec(0).rc_inc();
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
    let mut b: bool = false;
    let mut j: i32 = 0;
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
            if *INTPAR(IntPar::tracing_commands) > 1 {
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
                        begin_token_list(input, p, Btl::MarkText);
                    }
                    break;
                }
                Cmd::ExpandAfter => {
                    /*385:*/
                    if ochr == 0 {
                        let (t, _, _, _) = get_token(input);
                        /*1553: "\unless" implementation */
                        let (tok, cmd, chr, cs) = get_token(input);
                        ocmd = cmd;
                        ochr = chr;
                        ocs = cs;
                        if ocmd > MAX_COMMAND {
                            expand(input, cmd, chr, cs);
                        } else {
                            back_input(input, tok);
                        }
                        back_input(input, t);
                        break;
                    } else {
                        let (tok, cmd, chr, cs) = get_token(input);
                        ocmd = cmd;
                        ochr = chr;
                        ocs = cs;
                        if ocmd == Cmd::IfTest && ochr != IfTestCode::IfCase as i32 {
                            ochr = ochr + UNLESS_CODE
                        } else {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("You can\'t use `");
                            print_esc_cstr("unless");
                            print_cstr("\' before `");
                            print_cmd_chr(ocmd, ochr);
                            print_chr('\'');
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
                            *LLIST_link(p) = input.loc;
                            input.start = p as i32;
                            input.loc = p as i32;
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
                            prim_lookup((*hash.offset(cs as isize)).s1) as i32
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
                            *LLIST_link(p) = input.loc;
                            input.loc = Some(p).tex_int();
                            input.start = Some(p).tex_int();
                            break;
                        }
                    }
                }
                Cmd::CSName => {
                    let r = get_avail();
                    let mut p = r;
                    b = is_in_csname;
                    is_in_csname = true;
                    let (tok, cmd) = loop {
                        let (tok, cmd, _, cs) = get_x_token(input);
                        if cs == 0 {
                            let q = get_avail();
                            *LLIST_link(p) = Some(q).tex_int();
                            MEM[q].b32.s0 = tok;
                            p = q;
                        }
                        if !(cs == 0) {
                            break (tok, cmd);
                        }
                    };
                    if cmd != Cmd::EndCSName {
                        /*391:*/
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Missing ");
                        print_esc_cstr("endcsname");
                        print_cstr(" inserted");
                        help!(
                            "The control sequence marked <to be read again> should",
                            "not appear between \\csname and \\endcsname."
                        );
                        back_error(input, tok);
                    }
                    is_in_csname = b;
                    j = first;
                    let mut popt = llist_link(r);
                    while let Some(p) = popt {
                        if j >= max_buf_stack {
                            max_buf_stack = j + 1;
                            if max_buf_stack as usize == BUF_SIZE {
                                overflow("buffer size", BUF_SIZE);
                            }
                        }
                        BUFFER[j as usize] = MEM[p].b32.s0 % MAX_CHAR_VAL;
                        j += 1;
                        popt = llist_link(p);
                    }
                    let cs;
                    if j > first + 1 || BUFFER[first as usize] as i64 > 65535 {
                        no_new_control_sequence = false;
                        cs = id_lookup(first, j - first);
                        no_new_control_sequence = true
                    } else if j == first {
                        cs = NULL_CS as i32;
                    } else {
                        cs = SINGLE_BASE as i32 + BUFFER[first as usize];
                        /*:392*/
                    }
                    flush_list(Some(r));
                    if Cmd::from(EQTB[cs as usize].cmd) == Cmd::UndefinedCS {
                        eq_define(cs as usize, Cmd::Relax, Some(TOO_BIG_USV));
                    }
                    let tok = cs + CS_TOKEN_FLAG;
                    back_input(input, tok);
                    break;
                }
                Cmd::Convert => {
                    conv_toks(input, ocmd, ochr, ocs);
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
                    if *INTPAR(IntPar::tracing_ifs) > 0 {
                        if *INTPAR(IntPar::tracing_commands) <= 1i32 {
                            show_cur_cmd_chr(ocmd, ochr);
                        }
                    }
                    if ochr > if_limit as i32 {
                        if if_limit == FiOrElseCode::If {
                            insert_relax(input, ocs);
                        } else {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("Extra ");
                            print_cmd_chr(Cmd::FiOrElse, ochr);
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
                        start_input(input, ptr::null()); /*393:*/
                    }
                    break;
                }
                _ => {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Undefined control sequence");
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
                expand(input, cmd, chr, cs)
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Missing { inserted");
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

pub(crate) unsafe fn scan_keyword(input: &mut input_state_t, s: &[u8]) -> bool {
    let mut p = BACKUP_HEAD;
    *LLIST_link(p) = None.tex_int();
    if s.len() == 1 {
        let mut c: i8 = s[0] as i8;
        loop {
            let (tok, cmd, chr, cs) = get_x_token(input);
            if cs == 0 && (chr == c as i32 || chr == c as i32 - 32) {
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = tok;
                p = q;
                flush_list(llist_link(BACKUP_HEAD));
                return true;
            } else {
                if cmd != Cmd::Spacer || p != BACKUP_HEAD {
                    back_input(input, tok);
                    if p != BACKUP_HEAD {
                        begin_token_list(input, *LLIST_link(BACKUP_HEAD) as usize, Btl::BackedUp);
                    }
                    return false;
                }
            }
        }
    }
    let slen = s.len();
    let mut i = 0;
    while i < slen {
        let (tok, cmd, chr, cs) = get_x_token(input);
        if cs == 0 && (chr == s[i] as i8 as i32 || chr == s[i] as i8 as i32 - 32) {
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = tok;
            p = q;
            i += 1;
        } else if cmd != Cmd::Spacer || p != BACKUP_HEAD {
            back_input(input, tok);
            if p != BACKUP_HEAD {
                begin_token_list(input, *LLIST_link(BACKUP_HEAD) as usize, Btl::BackedUp);
            }
            return false;
        }
    }
    flush_list(llist_link(BACKUP_HEAD));
    true
}

pub(crate) unsafe fn mu_error() {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Incompatible glue units");
    help!("I\'m going to assume that 1mu=1pt when they\'re mixed.");
    error();
}
pub(crate) unsafe fn scan_glyph_number(input: &mut input_state_t, f: &NativeFont) -> i32 {
    if scan_keyword(input, b"/") {
        scan_and_pack_name(input);
        let val = map_glyph_to_index(f);
        val
    } else if scan_keyword(input, b"u") {
        let val = scan_char_num(input);
        let val = map_char_to_glyph(f, val);
        val
    } else {
        scan_int(input)
    }
}
pub(crate) unsafe fn scan_char_class(input: &mut input_state_t) -> i32 {
    let val = scan_int(input);
    if val < 0 || val > CHAR_CLASS_LIMIT {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad character class");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad character class");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad register code");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad character code");
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
    if val < 0 || val > BIGGEST_CHAR {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad character code");
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Bad active XeTeX math code");

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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad XeTeX math character code");

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
    let mut c: i32 = 0;
    'c_118470: loop {
        let (mut tok, mut cmd, mut chr, mut cs) = loop {
            /*422:*/
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
                break next;
            }
        };
        loop {
            match cmd {
                Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {
                    c = *MATH_CODE(chr as usize);
                    if !(math_char(c) == ACTIVE_MATH_CHAR as u32) {
                        break 'c_118470;
                    }
                    cs = chr + 1;
                    cmd = Cmd::from(EQTB[cs as usize].cmd);
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
                    if chr == 2 {
                        let val = scan_math_class_int(input);
                        c = set_class(val);
                        let val = scan_math_fam_int(input);
                        c = c + set_family(val);
                        let val = scan_usv_num(input);
                        c = c + val;
                    } else if chr == 1 {
                        let val = scan_xetex_math_char_int(input);
                        c = val
                    } else {
                        let val = scan_fifteen_bit_int(input);
                        c = set_class(val / 4096) + set_family((val % 4096) / 256) + (val % 256);
                    }
                    break 'c_118470;
                }
                Cmd::MathGiven => {
                    c = set_class(chr / 4096) + set_family((chr % 4096) / 256) + (chr % 256);
                    break 'c_118470;
                }
                Cmd::XetexMathGiven => {
                    c = chr;
                    break 'c_118470;
                }
                Cmd::DelimNum => {
                    if chr == 1 {
                        let val = scan_math_class_int(input);
                        c = set_class(val);
                        let val = scan_math_fam_int(input);
                        c = c + set_family(val);
                        let val = scan_usv_num(input);
                        c = c + val;
                    } else {
                        let mut val = scan_delimiter_int(input);
                        c = val / 4096;
                        c = set_class(c / 4096) + set_family((c % 4096) / 256) + (c % 256);
                    }
                    break 'c_118470;
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
    }
    m.typ = MathCell::MathChar;
    m.val.chr.character = (c as i64 % 65536) as u16;
    let font = if (math_class(c) == 7)
        && (*INTPAR(IntPar::cur_fam) >= 0 && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32)
    {
        *INTPAR(IntPar::cur_fam) as u16
    } else {
        math_fam(c) as u16
    };
    m.val.chr.font = (font as i64 + ((math_char(c) as i64) / 65536) * 256) as u16;
}
pub(crate) unsafe fn set_math_char(input: &mut input_state_t, chr: i32, mut c: i32) {
    let mut ch: UnicodeScalar = 0;
    if math_char(c) == ACTIVE_MATH_CHAR as u32 {
        /*1187: */
        let mut cs = chr + 1; /* ... "between 0 and 15" */
        let mut cmd = Cmd::from(EQTB[cs as usize].cmd); /* ... "between 0 and 15" */
        let mut chr = EQTB[cs as usize].val;
        let tok = x_token(input, &mut cmd, &mut chr, &mut cs);
        back_input(input, tok);
    } else {
        let p = new_noad();
        MEM[p + 1].b32.s1 = MathCell::MathChar as _;
        ch = math_char(c) as UnicodeScalar;
        MEM[p + 1].b16.s0 = (ch as i64 % 65536) as u16;
        MEM[p + 1].b16.s1 = math_fam(c) as u16;
        if math_class(c) == 7 {
            if *INTPAR(IntPar::cur_fam) >= 0
                && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32
            {
                MEM[p + 1].b16.s1 = *INTPAR(IntPar::cur_fam) as u16
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad math class");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad math family");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad number");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad mathchar");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad delimiter code");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad register code");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad number");
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
        if cmd >= Cmd::Call && cmd < Cmd::EndTemplate {
            if MEM[*LLIST_link(chr as usize) as usize].b32.s0 == PROTECTED_TOKEN {
                return (tok, cmd, chr);
            }
        }
        expand(input, cmd, chr, cs);
    }
}
pub(crate) unsafe fn effective_char(
    mut _err_p: bool,
    mut f: internal_font_number,
    mut c: u16,
) -> i32 {
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing font identifier");
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
    let mut n: i32 = 0;
    n = scan_int(input);
    let val = scan_font_ident(input);
    let mut f = val as usize;
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Font ");
        print_esc((*hash.offset((FROZEN_NULL_FONT + f) as isize)).s1);
        print_cstr(" has only ");
        print_int(FONT_PARAMS[f]);
        print_cstr(" fontdimen parameters");
        help!(
            "To increase the number of font parameters, you must",
            "use \\fontdimen immediately after the \\font is loaded."
        );
        error();
    };
    val
}
pub(crate) unsafe fn scan_something_internal(
    input: &mut input_state_t,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    level: ValLevel,
    mut negative: bool,
) -> (i32, ValLevel) {
    let (mut val, mut val_level) = match cmd {
        Cmd::DefCode => {
            let m = chr;
            let val = scan_usv_num(input);
            let val = if m == MATH_CODE_BASE as i32 {
                let mut val1 = *MATH_CODE(val as usize);
                if math_char(val1) == ACTIVE_MATH_CHAR as u32 {
                    val1 = 0x8000;
                } else if math_class(val1) > 7 || math_fam(val1) > 15 || math_char(val1) > 255 {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Extended mathchar used as mathchar");
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Extended delcode used as delcode");
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
            (val, ValLevel::Int)
        }
        Cmd::XetexDefCode => {
            let m = chr;
            let val = scan_usv_num(input);
            let val = if m == SF_CODE_BASE as i32 {
                (*SF_CODE(val as usize) as i64 / 65536) as i32
            } else if m == MATH_CODE_BASE as i32 {
                *MATH_CODE(val as usize)
            } else if m == MATH_CODE_BASE as i32 + 1 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Can\'t use \\Umathcode as a number (try \\Umathcodenum)");
                help!(
                    "\\Umathcode is for setting a mathcode from separate values;",
                    "use \\Umathcodenum to access them as single values."
                );
                error();
                0
            } else if m == DEL_CODE_BASE as i32 {
                EQTB[DEL_CODE_BASE + val as usize].val
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Can\'t use \\Udelcode as a number (try \\Udelcodenum)");
                help!(
                    "\\Udelcode is for setting a delcode from separate values;",
                    "use \\Udelcodenum to access them as single values."
                );
                error();
                0
            };
            (val, ValLevel::Int)
        }
        Cmd::ToksRegister | Cmd::AssignToks | Cmd::DefFamily | Cmd::SetFont | Cmd::DefFont => {
            let m = chr;
            if level != ValLevel::Tok {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing number, treated as zero");
                help!(
                    "A number should have been here; I inserted `0\'.",
                    "(If you can\'t figure out why I needed to see a number,",
                    "look up `weird error\' in the index to The TeXbook.)"
                );
                back_error(input, tok);
                (0, ValLevel::Dimen)
            } else if cmd <= Cmd::AssignToks {
                let val = if cmd < Cmd::AssignToks {
                    if m == 0i32 {
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
                (val, ValLevel::Tok)
            } else {
                back_input(input, tok);
                let val = scan_font_ident(input);
                (FONT_ID_BASE as i32 + val, ValLevel::Ident)
            }
        }
        Cmd::AssignInt => {
            let m = chr;
            (EQTB[m as usize].val, ValLevel::Int)
        }
        Cmd::AssignDimen => {
            let m = chr;
            (EQTB[m as usize].val, ValLevel::Dimen)
        }
        Cmd::AssignGlue => {
            let m = chr;
            (EQTB[m as usize].val, ValLevel::Glue)
        }
        Cmd::AssignMuGlue => {
            let m = chr;
            (EQTB[m as usize].val, ValLevel::Mu)
        }
        Cmd::SetAux => {
            let m = chr;
            if cur_list.mode.1 as i32 != m {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Improper ");
                print_cmd_chr(Cmd::SetAux, m);
                help!(
                    "You can refer to \\spacefactor only in horizontal mode;",
                    "you can refer to \\prevdepth only in vertical mode; and",
                    "neither of these is meaningful inside \\write. So",
                    "I\'m forgetting what you said and using zero instead."
                );
                error();
                if level != ValLevel::Tok {
                    (0, ValLevel::Dimen)
                } else {
                    (0, ValLevel::Int)
                }
            } else if m == ListMode::VMode as i32 {
                (cur_list.aux.b32.s1, ValLevel::Dimen)
            } else {
                (cur_list.aux.b32.s0, ValLevel::Int)
            }
        }
        Cmd::SetPrevGraf => {
            if cur_list.mode.1 == ListMode::NoMode {
                (0, ValLevel::Int)
            } else {
                NEST[NEST_PTR] = cur_list;
                let mut p = NEST_PTR;
                while NEST[p].mode.1 != ListMode::VMode {
                    p -= 1
                }
                (NEST[p].prev_graf, ValLevel::Int)
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
            (val, ValLevel::Int)
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
            (val.0, ValLevel::Dimen)
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
            (val, ValLevel::Int)
        }
        Cmd::SetBoxDimen => {
            let m = chr;
            let val = scan_register_num(input);
            let q = if val < 256 {
                BOX_REG(val as usize).opt()
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
            (val, ValLevel::Dimen)
        }
        Cmd::CharGiven | Cmd::MathGiven => (chr, ValLevel::Int),
        Cmd::AssignFontDimen => {
            let val = find_font_dimen(input, false);
            FONT_INFO[fmem_ptr as usize].b32.s1 = 0;
            (FONT_INFO[val as usize].b32.s1, ValLevel::Dimen)
        }
        Cmd::AssignFontInt => {
            let m = AssignFontInt::from(chr);
            let val = scan_font_ident(input);
            match m {
                AssignFontInt::HyphenChar => (HYPHEN_CHAR[val as usize], ValLevel::Int),
                AssignFontInt::SkewChar => (SKEW_CHAR[val as usize], ValLevel::Int),
                _ => {
                    let n = val;
                    let k = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[n as usize] {
                        scan_glyph_number(input, nf)
                    } else {
                        scan_char_num(input)
                    };
                    match m {
                        AssignFontInt::LpCode => {
                            (get_cp_code(n as usize, k as u32, Side::Left), ValLevel::Int)
                        }
                        AssignFontInt::RpCode => (
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
            if m < 0 || m > 19 {
                // TODO: may be bug
                /* 19 = "lo_mem_stat_max" */
                let val_level = ValLevel::from((MEM[m as usize].b16.s1 as i32 / 64) as u8);
                let val = match val_level {
                    ValLevel::Int | ValLevel::Dimen => MEM[(m + 2) as usize].b32.s1,
                    _ => MEM[(m + 1) as usize].b32.s1,
                };
                (val, val_level)
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
                        ValLevel::Int => *COUNT_REG(val as usize),
                        ValLevel::Dimen => (*SCALED_REG(val as usize)).0,
                        ValLevel::Glue => *SKIP_REG(val as usize),
                        ValLevel::Mu => *MU_SKIP_REG(val as usize),
                        _ => val,
                    }
                };
                (val, val_level)
            }
        }
        Cmd::LastItem => {
            let m = LastItemCode::n(chr as u8).unwrap();
            if m >= LastItemCode::InputLineNo {
                if m >= LastItemCode::MuToGlue {
                    /*1568:*/
                    let (mut val, mut val_level) = match m {
                        LastItemCode::MuToGlue => {
                            let val = scan_mu_glue(input); // 1595:
                            (val, ValLevel::Glue)
                        }
                        LastItemCode::GlueToMu => {
                            let val = scan_normal_glue(input); // 1596:
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
                                let m = val as usize;
                                let mut spec = GlueSpec(new_spec(m));
                                val = spec.ptr() as i32;
                                delete_glue_ref(m);
                                spec.set_size(-spec.size());
                                spec.set_stretch(-spec.stretch());
                                spec.set_shrink(-spec.shrink());
                            }
                        }
                    }
                    return (val, val_level);
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
                                    if file_line_error_style_p != 0 {
                                        print_file_line();
                                    } else {
                                        print_nl_cstr("! ");
                                    }
                                    print_cstr(
                                        "\\\\XeTeXglyphbounds requires an edge index from 1 to 4;",
                                    );
                                    print_nl_cstr("I don\'t know anything about edge ");
                                    print_int(n);
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
                            let q = GlueSpec(scan_normal_glue(input) as usize);
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
                    (val, ValLevel::Dimen)
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
                                Font::Native(Aat(e)) => aat::aat_font_get(m.into(), *e),
                                Font::Native(Otgr(e)) => ot_font_get((m as i32) - 14, e),
                                _ => 0,
                            }
                        }
                        LastItemCode::XetexCountFeatures => {
                            let n = scan_font_ident(input);
                            match &FONT_LAYOUT_ENGINE[n as usize] {
                                #[cfg(target_os = "macos")]
                                Font::Native(Aat(e)) => aat::aat_font_get(m.into(), *e),
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
                                    aat::aat_font_get_1(m.into(), *e, k)
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
                                    aat::aat_font_get_2(m.into(), *e, k, val)
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
                                    scan_and_pack_name(input);
                                    aat::aat_font_get_named(m.into(), *e)
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
                                    scan_and_pack_name(input);
                                    aat::aat_font_get_named(m.into(), *e)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    scan_and_pack_name(input);
                                    gr_font_get_named((m as i32) - 14, e)
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
                                    scan_and_pack_name(input);
                                    aat::aat_font_get_named_1(m.into(), *e, k)
                                }
                                Font::Native(Otgr(e)) if e.using_graphite() => {
                                    let k = scan_int(input);
                                    scan_and_pack_name(input);
                                    gr_font_get_named_1((m as i32) - 14, e, k)
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
                                map_char_to_glyph(nf, n)
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
                                scan_and_pack_name(input);
                                map_glyph_to_index(nf)
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
                            scan_and_pack_name(input);
                            count_pdf_file_pages()
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
                            let q = GlueSpec(scan_normal_glue(input) as usize);
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
                    (val, ValLevel::Int)
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
                (val, val_level)
            }
        }
        _ => {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("You can\'t use `");
            print_cmd_chr(cmd, chr);
            print_cstr("\' after ");
            print_esc_cstr("the");
            help!("I\'m forgetting what you said and using zero instead.");
            error();
            (
                0,
                if level != ValLevel::Tok {
                    ValLevel::Dimen
                } else {
                    ValLevel::Int
                },
            )
        }
    };

    while val_level > level {
        /*447:*/
        if val_level == ValLevel::Glue {
            val = MEM[(val + 1) as usize].b32.s1
        } else if val_level == ValLevel::Mu {
            mu_error();
        }
        val_level.prev();
    }

    let val = if negative {
        match val_level {
            ValLevel::Int | ValLevel::Dimen => -val,
            _ => {
                let mut spec = GlueSpec(new_spec(val as usize));
                let val = spec.ptr() as i32;
                spec.set_size(-spec.size());
                spec.set_stretch(-spec.stretch());
                spec.set_shrink(-spec.shrink());
                val
            }
        }
    } else {
        if val_level == ValLevel::Glue || val_level == ValLevel::Mu {
            GlueSpec(val as usize).rc_inc();
        }
        val
    };
    (val, val_level)
}
pub(crate) unsafe fn scan_int(input: &mut input_state_t) -> i32 {
    scan_int_with_radix(input).0
}
pub(crate) unsafe fn scan_int_with_radix(input: &mut input_state_t) -> (i32, i16, i32) {
    let mut d: i16 = 0;
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Improper alphabetic constant");
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
            chr = next.2;
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
            chr = next.2;
        } else if tok == HEX_TOKEN {
            radix = 16;
            m = 0x8000000;
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
            chr = next.2;
        }
        let mut vacuous = true;
        ival = 0;
        loop {
            if tok < ZERO_TOKEN + radix as i32 && tok >= ZERO_TOKEN && tok <= ZERO_TOKEN + 9 {
                d = (tok - ZERO_TOKEN) as i16
            } else {
                if !(radix as i32 == 16) {
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Number too big");
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
            chr = next.2;
        }
        if vacuous {
            /*464:*/
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing number, treated as zero");
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
unsafe fn round_decimals(mut k: i16) -> Scaled {
    let mut a: i32 = 0;
    while k as i32 > 0 {
        k -= 1;
        a = (a + dig[k as usize] as i32 * 0x20000) / 10
    }
    Scaled((a + 1) / 2)
}
pub(crate) unsafe fn xetex_scan_dimen(
    input: &mut input_state_t,
    mut mu: bool,
    mut inf: bool,
    shortcut: Option<i32>,
    mut requires_units: bool,
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
            if !(tok == OTHER_TOKEN + '+' as i32) {
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
            if scan_keyword(input, b"fil") {
                cur_order = GlueOrder::Fil;
                while scan_keyword(input, b"l") {
                    if cur_order == GlueOrder::Filll {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Illegal unit of measure (");
                        print_cstr("replaced by filll)");
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
            if scan_keyword(input, b"em") {
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
            } else if scan_keyword(input, b"ex") {
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
            if scan_keyword(input, b"mu") {
                return attach_fraction(input, f, negative, val);
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Illegal unit of measure (");
                print_cstr("mu inserted)");
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

        if scan_keyword(input, b"true") {
            /*476:*/
            prepare_mag(); /* magic ratio consant */
            if *INTPAR(IntPar::mag) != 1000 {
                let (v, tex_remainder) = xn_over_d(Scaled(val), Scaled(1000), *INTPAR(IntPar::mag)); /* magic ratio consant */
                val = v.0;
                let f_ = (((1000 * f.0) as i64 + 65536 * tex_remainder as i64)
                    / *INTPAR(IntPar::mag) as i64) as i32;
                val = (val as i64 + f_ as i64 / 65536) as i32;
                f = Scaled((f_ as i64 % 65536) as i32);
            }
        }

        if scan_keyword(input, b"pt") {
            return attach_fraction(input, f, negative, val);
        }

        let num;
        let denom;
        if scan_keyword(input, b"in") {
            num = 7227;
            denom = 100;
        } else if scan_keyword(input, b"pc") {
            num = 12;
            denom = 1;
        } else if scan_keyword(input, b"cm") {
            num = 7227; // magic ratio consant
            denom = 254; // magic ratio consant
        } else if scan_keyword(input, b"mm") {
            num = 7227; // magic ratio consant
            denom = 2540; // magic ratio consant
        } else if scan_keyword(input, b"bp") {
            num = 7227; // magic ratio consant
            denom = 7200; // magic ratio consant
        } else if scan_keyword(input, b"dd") {
            num = 1238; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(input, b"cc") {
            num = 14856; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(input, b"sp") {
            return done(input, negative, Scaled(val));
        /*478:*/
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Illegal unit of measure (");
            print_cstr("pt inserted)");
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
    } else {
        if val >= 16384 {
            arith_error = true
        } else {
            val = (val as i64 * 65536 + f.0 as i64) as i32
        }
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Dimension too large");
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
    mut mu: bool,
    mut inf: bool,
    shortcut: Option<i32>,
) -> Scaled {
    xetex_scan_dimen(input, mu, inf, shortcut, true)
}
pub(crate) unsafe fn scan_decimal(input: &mut input_state_t) -> Scaled {
    xetex_scan_dimen(input, false, false, None, false)
}
pub(crate) unsafe fn scan_glue(input: &mut input_state_t, level: ValLevel) -> i32 {
    let mut negative: bool = false;
    let mut mu: bool = false;
    mu = level == ValLevel::Mu;
    negative = false;
    let (tok, cmd, chr) = loop {
        let (mut tok, cmd, chr, _) = loop {
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer) {
                break next;
            }
        };

        if tok == OTHER_TOKEN + 45 {
            /*"-"*/
            negative = !negative;
            tok = OTHER_TOKEN + 43
            /*"+"*/
        }
        if !(tok == OTHER_TOKEN + 43) {
            break (tok, cmd, chr);
        }
        /*"+"*/
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
            _ => {
                if val_level != level {
                    mu_error();
                }
                return val;
            }
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
    let mut q = GlueSpec(new_spec(0));
    q.set_size(val);
    if scan_keyword(input, b"plus") {
        let val = scan_dimen(input, mu, true, None);
        q.set_stretch(val).set_stretch_order(cur_order);
    }
    if scan_keyword(input, b"minus") {
        let val = scan_dimen(input, mu, true, None);
        q.set_shrink(val).set_shrink_order(cur_order);
    }
    q.ptr() as i32
    /*:481*/
}
pub(crate) unsafe fn add_or_sub(
    mut x: i32,
    mut y: i32,
    mut max_answer: i32,
    mut negative: bool,
) -> i32 {
    let mut a = 0;
    if negative {
        y = -y
    }
    if x >= 0 {
        if y <= max_answer - x {
            a = x + y;
        } else {
            arith_error = true;
            a = 0;
        }
    } else if y >= -max_answer - x {
        a = x + y;
    } else {
        arith_error = true;
        a = 0;
    }
    a
}
pub(crate) unsafe fn quotient(mut n: i32, mut d: i32) -> i32 {
    let mut negative: bool = false;
    let mut a: i32 = 0;
    if d == 0 {
        arith_error = true;
        a = 0
    } else {
        if d > 0 {
            negative = false
        } else {
            d = -d;
            negative = true
        }
        if n < 0 {
            n = -n;
            negative = !negative
        }
        a = n / d;
        n = n - a * d;
        d = n - d;
        if d + n >= 0 {
            a += 1;
        }
        if negative {
            a = -a;
        }
    }
    a
}
pub(crate) unsafe fn fract(mut x: i32, mut n: i32, mut d: i32, mut max_answer: i32) -> i32 {
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
    if x < 0 {
        x = -x;
        negative = !negative;
    } else if x == 0 {
        return a;
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
    n = n - t * d;
    if n == 0 {
        return found(a, negative);
    }
    let t = x / d;
    if t > (max_answer - a) / n {
        return too_big();
    }
    a = a + t * n;
    x = x - t * d;
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
            r = r + x;
            if r >= 0 {
                r = r - d;
                f += 1
            }
        }
        n = n / 2;
        if n == 0 {
            break;
        }
        if x < h {
            x = x + x
        } else {
            let t = x - d;
            x = t + x;
            f = f + n;
            if !(x < n) {
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
        a = a + f;
        found(a, negative)
    }
}
pub(crate) unsafe fn scan_expr(input: &mut input_state_t, val_level: &mut ValLevel) -> i32 {
    let mut e: i32 = 0;
    let mut t: i32 = 0;
    let mut n: i32 = 0;
    let mut l = *val_level;
    let mut a = arith_error;
    let mut b = false;
    let mut p = None.tex_int();
    'c_78022: loop {
        let mut o;
        let mut r = Expr::None;
        e = 0;
        let mut s = Expr::None;
        t = 0;
        n = 0;
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
                ValLevel::Glue => scan_normal_glue(input),
                _ => scan_mu_glue(input),
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
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Missing ) inserted for expression");
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
                    f = new_spec(0) as i32
                }
                match s {
                    Expr::None => {
                        /*1579: */
                        t = if l >= ValLevel::Glue && o != Expr::None {
                            let mut t = GlueSpec(new_spec(f as usize));
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Arithmetic overflow");
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
pub(crate) unsafe fn scan_normal_glue(input: &mut input_state_t) -> i32 {
    scan_glue(input, ValLevel::Glue)
}
pub(crate) unsafe fn scan_mu_glue(input: &mut input_state_t) -> i32 {
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
        if scan_keyword(input, b"width") {
            q.set_width(scan_dimen(input, false, false, None));
        } else if scan_keyword(input, b"height") {
            q.set_height(scan_dimen(input, false, false, None));
        } else if scan_keyword(input, b"depth") {
            q.set_depth(scan_dimen(input, false, false, None));
        } else {
            break;
        }
    }
    q
}
pub(crate) unsafe fn scan_general_text(input: &mut input_state_t, cs: i32) -> i32 {
    let mut unbalance: i32 = 0;
    let mut s = scanner_status;
    let mut w = warning_index;
    let mut d = def_ref;
    scanner_status = ScannerStatus::Absorbing;
    warning_index = cs;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let mut p = def_ref;
    scan_left_brace(input);
    unbalance = 1;
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
        let q = get_avail();
        *LLIST_link(p) = Some(q).tex_int();
        MEM[q].b32.s0 = tok;
        p = q;
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
    let mut w: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let _ = scan_general_text(input, cs);
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    token_show(Some(TEMP_HEAD));
    selector = old_setting;
    flush_list(llist_link(TEMP_HEAD));
    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    let mut s = make_string();
    str_pool[pool_ptr as usize] = ' ' as i32 as packed_UTF16_code;
    let mut l = str_start[(s as i64 - 65536) as usize];
    let mut nl = *INTPAR(IntPar::new_line_char);
    let mut p = get_avail();
    let mut q = p;
    while l < pool_ptr {
        let mut m = l;
        while l < pool_ptr && str_pool[l as usize] as i32 != nl {
            l += 1
        }
        let mut sz = (l - m + 7) / 4;
        if sz == 1 {
            sz = 2
        }
        let mut r = get_node(sz);
        *LLIST_link(q) = Some(r).tex_int();
        q = r;
        MEM[q].b32.s0 = sz;
        while sz > 2 {
            sz -= 1;
            r += 1;
            w.s3 = str_pool[m as usize];
            w.s2 = str_pool[(m + 1) as usize];
            w.s1 = str_pool[(m + 2) as usize];
            w.s0 = str_pool[(m + 3) as usize];
            MEM[r].b16 = w;
            m = m + 4
        }
        w.s3 = ' ' as i32 as u16;
        w.s2 = ' ' as i32 as u16;
        w.s1 = ' ' as i32 as u16;
        w.s0 = ' ' as i32 as u16;
        if l > m {
            w.s3 = str_pool[m as usize];
            if l > m + 1 {
                w.s2 = str_pool[(m + 1) as usize];
                if l > m + 2 {
                    w.s1 = str_pool[(m + 2) as usize];
                    if l > m + 3 {
                        w.s0 = str_pool[(m + 3) as usize]
                    }
                }
            }
        }
        MEM[r + 1].b16 = w;
        if str_pool[l as usize] as i32 == nl {
            l += 1
        }
    }
    MEM[p].b32.s0 = MEM[p].b32.s1;
    MEM[p].b32.s1 = pseudo_files;
    pseudo_files = p as i32;
    str_ptr -= 1;
    pool_ptr = str_start[(str_ptr - 65536) as usize];
    begin_file_reading(input);
    line = 0;
    input.limit = input.start;
    input.loc = input.limit + 1;
    if *INTPAR(IntPar::tracing_scan_tokens) > 0 {
        if term_offset > max_print_line - 3 {
            print_ln();
        } else if term_offset > 0 || file_offset > 0 {
            print_chr(' ');
        }
        input.name = 19;
        print_cstr("( ");
        open_parens += 1;
        rust_stdout.as_mut().unwrap().flush().unwrap();
    } else {
        input.name = 18;
        input.synctex_tag = 0;
    };
}
pub(crate) unsafe fn str_toks_cat(mut b: pool_pointer, mut cat: i16) -> usize {
    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    let mut p = TEMP_HEAD;
    *LLIST_link(p) = None.tex_int();
    let mut k = b;
    while k < pool_ptr {
        let mut t = str_pool[k as usize] as i32;
        if t == ' ' as i32 && cat == 0 {
            t = SPACE_TOKEN
        } else {
            if t >= 0xd800
                && t < 0xdc00
                && k + 1 < pool_ptr
                && str_pool[(k + 1) as usize] as i32 >= 0xdc00
                && (str_pool[(k + 1) as usize] as i32) < 0xe000
            {
                k += 1;
                t = (65536
                    + ((t - 0xd800) * 1024) as i64
                    + (str_pool[k as usize] as i32 - 0xdc00) as i64) as i32
            }
            if cat == 0 {
                t = OTHER_TOKEN + t
            } else {
                t = MAX_CHAR_VAL * cat as i32 + t
            }
        }
        let q = if let Some(q) = avail {
            avail = llist_link(q);
            *LLIST_link(q) = None.tex_int();
            q
        } else {
            get_avail()
        };
        *LLIST_link(p) = Some(q).tex_int();
        MEM[q].b32.s0 = t;
        p = q;
        k += 1;
    }
    pool_ptr = b;
    p
}
pub(crate) unsafe fn str_toks(mut b: pool_pointer) -> usize {
    str_toks_cat(b, 0)
}
pub(crate) unsafe fn the_toks(input: &mut input_state_t, chr: i32, cs: i32) -> usize {
    if chr & 1 != 0 {
        let c = chr as i16;
        let val = scan_general_text(input, cs);
        if c == 1 {
            assert!(val.opt().is_some());
            return val as usize; // TODO: check TEX_NULL
        } else {
            let old_setting = selector;
            selector = Selector::NEW_STRING;
            let b = pool_ptr;
            let p = get_avail();
            *LLIST_link(p) = *LLIST_link(TEMP_HEAD);
            token_show(Some(p));
            flush_list(Some(p));
            selector = old_setting;
            return str_toks(b);
        }
    }
    let (tok, cmd, chr, _) = get_x_token(input);
    let (val, val_level) = scan_something_internal(input, tok, cmd, chr, ValLevel::Tok, false);
    match val_level {
        ValLevel::Ident | ValLevel::Tok | ValLevel::InterChar | ValLevel::Mark => {
            /*485: */
            let mut p = TEMP_HEAD;
            *LLIST_link(p) = None.tex_int();
            if val_level == ValLevel::Ident {
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = CS_TOKEN_FLAG + val;
                p = q;
            } else if let Some(v) = val.opt() {
                let mut ropt = llist_link(v);
                while let Some(r) = ropt {
                    let q = if let Some(q) = avail {
                        avail = llist_link(q);
                        *LLIST_link(q) = None.tex_int();
                        q
                    } else {
                        get_avail()
                    };
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = MEM[r].b32.s0;
                    p = q;
                    ropt = llist_link(r);
                }
            }
            p
        }
        _ => {
            let old_setting = selector;
            selector = Selector::NEW_STRING;
            let b = pool_ptr;
            match val_level {
                ValLevel::Int => print_int(val),
                ValLevel::Dimen => {
                    print_scaled(Scaled(val));
                    print_cstr("pt");
                }
                ValLevel::Glue => {
                    print_spec(val, "pt");
                    delete_glue_ref(val as usize);
                }
                ValLevel::Mu => {
                    print_spec(val, "mu");
                    delete_glue_ref(val as usize);
                }
                _ => {}
            }
            selector = old_setting;
            str_toks(b)
        }
    }
}
pub(crate) unsafe fn ins_the_toks(input: &mut input_state_t, chr: i32, cs: i32) {
    *LLIST_link(GARBAGE as usize) = Some(the_toks(input, chr, cs)).tex_int();
    begin_token_list(input, *LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
}
pub(crate) unsafe fn conv_toks(
    input: &mut input_state_t,
    mut ocmd: Cmd,
    mut ochr: i32,
    mut ocs: i32,
) {
    let mut save_warning_index: i32 = 0;
    let mut boolvar: bool = false;
    let mut s: str_number = 0;
    let mut u: str_number = 0;
    let mut b: pool_pointer = 0;
    let mut fnt: usize = 0;
    let mut arg1: i32 = 0i32;
    let mut arg2: i32 = 0i32;
    let mut font_name_str: str_number = 0;
    let mut quote_char: UTF16_code = 0;
    let mut saved_chr: UnicodeScalar = 0;
    let mut p = None;
    let mut cat = 0i32 as i16;
    let c = ConvertCode::n(ochr as u8).unwrap();
    let mut oval = None;
    match c {
        ConvertCode::Number | ConvertCode::RomanNumeral => oval = Some(scan_int(input)),
        ConvertCode::String | ConvertCode::Meaning => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            let (_, cmd, chr, cs) = get_token(input);
            ocmd = cmd;
            ochr = chr;
            ocs = cs;
            scanner_status = save_scanner_status;
        }
        ConvertCode::FontName => oval = Some(scan_font_ident(input)),
        ConvertCode::XetexUchar => oval = Some(scan_usv_num(input)),
        ConvertCode::XetexUcharcat => {
            saved_chr = scan_usv_num(input);
            let val = scan_int(input);
            if val < Cmd::LeftBrace as i32
                || val > Cmd::OtherChar as i32
                || val == OUT_PARAM as i32
                || val == IGNORE as i32
            {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Invalid code (");
                print_int(val);
                print_cstr("), should be in the ranges 1..4, 6..8, 10..12");
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
            save_warning_index = warning_index;
            let save_def_ref = def_ref;
            if str_start[(str_ptr - TOO_BIG_CHAR) as usize] < pool_ptr {
                u = make_string()
            } else {
                u = 0;
            }
            oval = Some(compare_strings(input, ocs));
            def_ref = save_def_ref;
            warning_index = save_warning_index;
            scanner_status = save_scanner_status;
            if u != 0 {
                str_ptr -= 1;
            }
        }
        ConvertCode::PdfMdfiveSum => {
            let save_scanner_status = scanner_status;
            save_warning_index = warning_index;
            let save_def_ref = def_ref;
            if str_start[(str_ptr - TOO_BIG_CHAR) as usize] < pool_ptr {
                u = make_string()
            } else {
                u = 0;
            }
            boolvar = scan_keyword(input, b"file");
            scan_pdf_ext_toks(input, ocs);
            if selector == Selector::NEW_STRING {
                pdf_error(
                    "tokens",
                    "tokens_to_string() called while selector = new_string",
                );
            }
            let old_setting = selector;
            selector = Selector::NEW_STRING;
            show_token_list(llist_link(def_ref), None, pool_size - pool_ptr);
            selector = old_setting;
            s = make_string();
            delete_token_ref(def_ref);
            def_ref = save_def_ref;
            warning_index = save_warning_index;
            scanner_status = save_scanner_status;
            b = pool_ptr;
            getmd5sum(s, boolvar);
            *LLIST_link(GARBAGE as usize) = Some(str_toks(b)).tex_int();
            if s == str_ptr - 1 {
                str_ptr -= 1;
                pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
            }
            begin_token_list(input, *LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
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
                BOX_REG(val as usize).opt()
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
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    b = pool_ptr;
    match c {
        ConvertCode::Number => print_int(oval.unwrap()),
        ConvertCode::RomanNumeral => print_roman_int(oval.unwrap()),
        ConvertCode::String => {
            if ocs != 0 {
                sprint_cs(ocs);
            } else {
                print_char(ochr);
            }
        }
        ConvertCode::Meaning => print_meaning(ocmd, ochr),
        ConvertCode::FontName => {
            let val = oval.unwrap();
            font_name_str = FONT_NAME[val as usize];
            match &FONT_LAYOUT_ENGINE[val as usize] {
                Font::Native(_) => {
                    quote_char = '\"' as i32 as UTF16_code;
                    for i in 0..=(length(font_name_str) - 1) {
                        if str_pool[(str_start[(font_name_str as i64 - 65536) as usize] + i as i32)
                            as usize] as i32
                            == '\"' as i32
                        {
                            quote_char = '\'' as i32 as UTF16_code
                        }
                    }
                    print_char(quote_char as i32);
                    print(font_name_str);
                    print_char(quote_char as i32);
                }
                _ => print(font_name_str),
            }
            if FONT_SIZE[val as usize] != FONT_DSIZE[val as usize] {
                print_cstr(" at ");
                print_scaled(FONT_SIZE[val as usize]);
                print_cstr("pt");
            }
        }
        ConvertCode::XetexUchar | ConvertCode::XetexUcharcat => print_char(oval.unwrap()),
        ConvertCode::EtexRevision => print_cstr(".6"),
        ConvertCode::PdfStrcmp => print_int(oval.unwrap()),
        ConvertCode::XetexRevision => print_cstr(".99998"),
        ConvertCode::XetexVariationName => {
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(e)) => {
                    aat::aat_print_font_name(c as i32, *e, arg1, arg2);
                }
                _ => {
                    // do nothing
                }
            }
        }
        ConvertCode::XetexFeatureName | ConvertCode::XetexSelectorName => {
            match &FONT_LAYOUT_ENGINE[fnt as usize] {
                #[cfg(target_os = "macos")]
                Font::Native(Aat(e)) => {
                    aat::aat_print_font_name(c as i32, *e, arg1, arg2);
                }
                Font::Native(Otgr(e)) if e.using_graphite() => {
                    gr_print_font_name(c as i32, e, arg1, arg2);
                }
                _ => {}
            }
        }
        ConvertCode::XetexGlyphName => match &FONT_LAYOUT_ENGINE[fnt as usize] {
            Font::Native(_) => print_glyph_name(fnt, arg1),
            _ => {}
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
            match popt.map(|p| CharOrText::from(p)) {
                Some(CharOrText::Text(TxtNode::MarginKern(m))) if MEM[m.ptr()].b16.s0 == 0 => {
                    print_scaled(Scaled(MEM[m.ptr() + 1].b32.s1));
                }
                _ => print('0' as i32),
            }
            print_cstr("pt");
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
            match popt.map(|p| CharOrText::from(p)) {
                Some(CharOrText::Text(TxtNode::MarginKern(m))) if MEM[m.ptr()].b16.s0 == 1 => {
                    print_scaled(Scaled(MEM[m.ptr() + 1].b32.s1));
                }
                _ => print('0' as i32),
            }
            print_cstr("pt");
        }
        ConvertCode::JobName => print_file_name(job_name, 0, 0),
        _ => {}
    }
    selector = old_setting;
    *LLIST_link(GARBAGE) = str_toks_cat(b, cat) as i32;
    begin_token_list(input, *LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
}
pub(crate) unsafe fn scan_toks(
    input: &mut input_state_t,
    cs: i32,
    mut macro_def: bool,
    mut xpand: bool,
) -> usize {
    let mut s: i32 = 0;
    let mut unbalance: i32 = 0;
    scanner_status = if macro_def {
        ScannerStatus::Defining
    } else {
        ScannerStatus::Absorbing
    };
    warning_index = cs;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let mut p = def_ref;
    let mut hash_brace = 0;
    let mut t = ZERO_TOKEN;
    if macro_def {
        let mut done1 = true;
        let cmd = loop
        /*493: */
        {
            let (mut otok, cmd, chr, _) = get_token(input);
            if otok < RIGHT_BRACE_LIMIT {
                break cmd;
            }
            if cmd == Cmd::MacParam {
                /*495: */
                s = MATCH_TOKEN + chr;
                let (tok, cmd, _, _) = get_token(input);
                otok = tok;
                if cmd == Cmd::LeftBrace {
                    hash_brace = tok;
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = tok;
                    p = q;
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = END_MATCH_TOKEN;
                    p = q;
                    done1 = false;
                    break cmd;
                } else if t == ZERO_TOKEN + 9 {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("You already have nine parameters");
                    help!("I\'m going to ignore the # sign you just used.");
                    error();
                } else {
                    t += 1;
                    if tok != t {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Parameters must be numbered consecutively");
                        help!(
                            "I\'ve inserted the digit you should have used after the #.",
                            "Type `1\' to delete what you did use."
                        );
                        back_error(input, tok);
                    }
                    otok = s
                }
            }
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = otok;
            p = q;
        };

        if done1 {
            // done1:
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = END_MATCH_TOKEN;
            p = q;
            if cmd == Cmd::RightBrace {
                /*494: */
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing { inserted");
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
    }

    unbalance = 1;
    loop {
        let (mut tok, cmd) = if xpand {
            let mut ocmd;
            let mut ochr;
            let mut ocs;
            loop {
                /*497: */
                let (cmd, chr, cs) = get_next(input);
                ocmd = cmd;
                ochr = chr;
                ocs = cs;
                if ocmd >= Cmd::Call {
                    if MEM[*LLIST_link(ochr as usize) as usize].b32.s0 == PROTECTED_TOKEN {
                        ocmd = Cmd::Relax;
                        ochr = NO_EXPAND_FLAG;
                    }
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
        } else if cmd == Cmd::MacParam {
            if macro_def {
                /*498: */
                s = tok;
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
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Illegal parameter number in definition of ");
                        sprint_cs(warning_index);
                        help!(
                            "You meant to type ## instead of #, right?",
                            "Or maybe a } was forgotten somewhere earlier, and things",
                            "are all screwed up? I\'m going to assume that you meant ##."
                        );
                        back_error(input, tok);
                        tok = s
                    } else {
                        tok = OUT_PARAM_TOKEN - 48 + chr
                    }
                }
            }
        }
        let q = get_avail();
        *LLIST_link(p) = Some(q).tex_int();
        MEM[q].b32.s0 = tok;
        p = q;
    }

    unsafe fn found(p: usize, hash_brace: i32) -> usize {
        scanner_status = ScannerStatus::Normal;
        if hash_brace != 0 {
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = hash_brace;
            q
        } else {
            p
        }
    }
}
pub(crate) unsafe fn read_toks(
    input: &mut input_state_t,
    mut n: i32,
    mut r: i32,
    mut j: i32,
) -> i32 {
    let mut s: i32 = 0;
    let mut m: i16 = 0;
    scanner_status = ScannerStatus::Defining;
    warning_index = r;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let p = def_ref;
    let q = get_avail();
    *LLIST_link(p) = Some(q).tex_int();
    MEM[q].b32.s0 = END_MATCH_TOKEN;
    let mut p = q;
    if n < 0 || n > 15 {
        m = 16;
    } else {
        m = n as i16
    }
    s = align_state;
    align_state = 1000000;
    loop {
        /*502:*/
        begin_file_reading(input);
        input.name = m as i32 + 1;
        assert!(
            read_open[m as usize] != OpenMode::Closed,
            /*503:*/
            "terminal input forbidden"
        );
        /*505:*/
        if read_open[m as usize] == OpenMode::JustOpen {
            /*504:*/
            if input_line(read_file[m as usize].as_mut().unwrap()) {
                read_open[m as usize] = OpenMode::Normal;
            } else {
                let _ = read_file[m as usize].take();
                read_open[m as usize] = OpenMode::Closed;
            }
        } else if !input_line(read_file[m as usize].as_mut().unwrap()) {
            let _ = read_file[m as usize].take();
            read_open[m as usize] = OpenMode::Closed;
            if align_state as i64 != 1000000 {
                runaway();
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("File ended within ");
                print_esc_cstr("read");
                help!("This \\read has unbalanced braces.");
                align_state = 1000000;
                error();
            }
        }
        input.limit = last;
        if *INTPAR(IntPar::end_line_char) < 0 || *INTPAR(IntPar::end_line_char) > 255 {
            input.limit -= 1
        } else {
            BUFFER[input.limit as usize] = *INTPAR(IntPar::end_line_char)
        }
        first = input.limit + 1;
        input.loc = input.start;
        input.state = InputState::NewLine;
        if j == 1 {
            while input.loc <= input.limit {
                let chr = BUFFER[input.loc as usize];
                input.loc += 1;
                let tok = if chr == ' ' as i32 {
                    SPACE_TOKEN
                } else {
                    chr + OTHER_TOKEN
                };
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = tok;
                p = q;
            }
        } else {
            loop {
                let mut tok = get_token(input).0;
                if tok == 0 {
                    break;
                }
                if (align_state as i64) < 1000000 {
                    loop {
                        tok = get_token(input).0;
                        if tok == 0 {
                            break;
                        }
                    }
                    align_state = 1000000;
                    break;
                } else {
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = tok;
                    p = q;
                }
            }
        }
        end_file_reading(input);
        if !(align_state as i64 != 1000000) {
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
    if *INTPAR(IntPar::tracing_ifs) > 0 {
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
    let mut e: bool = false;
    let mut r: u8 = 0;

    if *INTPAR(IntPar::tracing_ifs) > 0 {
        if *INTPAR(IntPar::tracing_commands) <= 1 {
            show_cur_cmd_chr(cmd, chr);
        }
    }

    let mut p = get_node(IF_NODE_SIZE);
    *LLIST_link(p) = cond_ptr.tex_int();
    MEM[p].b16.s1 = if_limit as u16;
    MEM[p].b16.s0 = cur_if as u16;
    MEM[p + 1].b32.s1 = if_line;
    cond_ptr = Some(p);
    cur_if = chr as i16;
    if_limit = FiOrElseCode::If;
    if_line = line;

    let mut save_cond_ptr = cond_ptr;
    let mut is_unless = chr >= UNLESS_CODE;
    let mut this_if = IfTestCode::n((chr % UNLESS_CODE) as u8).unwrap();

    match this_if {
        IfTestCode::IfChar | IfTestCode::IfCat => {
            let (tok, mut cmd, mut chr, _) = get_x_token(input);
            if cmd == Cmd::Relax {
                if chr == NO_EXPAND_FLAG {
                    cmd = Cmd::ActiveChar;
                    chr = tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
                }
            }

            let (m, n) = if cmd > Cmd::ActiveChar || chr > BIGGEST_USV as i32 {
                (Cmd::Relax, TOO_BIG_USV as i32)
            } else {
                (cmd, chr)
            };

            let (tok, mut cmd, mut chr, _) = get_x_token(input);

            if cmd == Cmd::Relax {
                if chr == NO_EXPAND_FLAG {
                    cmd = Cmd::ActiveChar;
                    chr = tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
                }
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

            if tok >= OTHER_TOKEN + 60 && tok <= OTHER_TOKEN + 62 {
                r = (tok - OTHER_TOKEN) as u8
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing = inserted for ");
                print_cmd_chr(Cmd::IfTest, this_if as i32);
                help!("I was expecting to see `<\', `=\', or `>\'. Didn\'t.");
                back_error(input, tok);
                r = b'=';
            }

            let val = if this_if == IfTestCode::IfInt {
                scan_int(input)
            } else {
                scan_dimen(input, false, false, None).0
            };

            match r {
                60 => {
                    /*"<"*/
                    b = n < val
                }
                61 => {
                    /*"="*/
                    b = n == val
                }
                62 => {
                    /*">"*/
                    b = n > val
                }
                _ => {}
            }
        }

        IfTestCode::IfOdd => {
            let val = scan_int(input);
            b = val & 1i32 != 0;
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
            b = cur_list.mode.0 == true;
        }

        IfTestCode::IfVoid | IfTestCode::IfHBox | IfTestCode::IfVBox => {
            let val = scan_register_num(input);
            let p = if val < 256 {
                BOX_REG(val as usize).opt()
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
            let mut p = n;
            e = is_in_csname;
            is_in_csname = true;

            let (tok, cmd) = loop {
                let (tok, cmd, _, cs) = get_x_token(input);
                if cs == 0 {
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = tok;
                    p = q;
                }
                if !(cs == 0) {
                    break (tok, cmd);
                }
            };

            if cmd != Cmd::EndCSName {
                /*391:*/
                if file_line_error_style_p != 0 {
                    print_file_line(); /*:1556*/
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing ");
                print_esc_cstr("endcsname");
                print_cstr(" inserted");
                help!(
                    "The control sequence marked <to be read again> should",
                    "not appear between \\csname and \\endcsname."
                );
                back_error(input, tok);
            }

            let mut m = first;
            let mut popt = llist_link(n);

            while let Some(p) = popt {
                if m >= max_buf_stack {
                    max_buf_stack = m + 1i32;
                    if max_buf_stack as usize == BUF_SIZE {
                        overflow("buffer size", BUF_SIZE);
                    }
                }
                BUFFER[m as usize] = MEM[p as usize].b32.s0 % MAX_CHAR_VAL;
                m += 1;
                popt = llist_link(p);
            }

            let cs = if m == first {
                NULL_CS as i32
            } else if m == first + 1i32 {
                SINGLE_BASE as i32 + BUFFER[first as usize]
            } else {
                id_lookup(first, m - first)
            };

            flush_list(Some(n));
            b = Cmd::from(EQTB[cs as usize].cmd) != Cmd::UndefinedCS;
            is_in_csname = e;
        }

        IfTestCode::IfInCSName => {
            b = is_in_csname;
        }

        IfTestCode::IfFontChar => {
            let n = scan_font_ident(input) as usize;
            let val = scan_usv_num(input);
            b = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[n] {
                map_char_to_glyph(nf, val) > 0
            } else if FONT_BC[n] as i32 <= val && FONT_EC[n] as i32 >= val {
                FONT_CHARACTER_INFO(n, effective_char(true, n, val as u16) as usize).s3 > 0
            } else {
                false
            };
        }

        IfTestCode::IfCase => {
            let mut n = scan_int(input);

            if *INTPAR(IntPar::tracing_commands) > 1 {
                diagnostic(false, || {
                    print_cstr("{case ");
                    print_int(n);
                    print_chr('}');
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
                prim_lookup((*hash.offset(cs as isize)).s1)
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

    if *INTPAR(IntPar::tracing_commands) > 1 {
        /*521:*/
        diagnostic(false, || print_cstr(if b { "{true}" } else { "{false}" }));
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

            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Extra ");
            print_esc_cstr("or");
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
    area_delimiter: &mut pool_pointer,
    ext_delimiter: &mut pool_pointer,
    quoted_filename: &mut bool,
    file_name_quote_char: &mut Option<u16>,
) -> bool {
    if stop_at_space_ && file_name_quote_char.is_none() && c as i32 == ' ' as i32 {
        return false;
    }
    if stop_at_space_ && *file_name_quote_char == Some(c) {
        *file_name_quote_char = None;
        return true;
    }
    if stop_at_space_ && file_name_quote_char.is_none() && (c == '\"' as u16 || c == '\'' as u16) {
        *file_name_quote_char = Some(c);
        *quoted_filename = true;
        return true;
    }
    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    str_pool[pool_ptr as usize] = c;
    pool_ptr += 1;
    if c == '/' as u16 {
        // IS_DIR_SEP
        *area_delimiter = cur_length();
        *ext_delimiter = 0;
    } else if c == '.' as u16 {
        *ext_delimiter = cur_length()
    }
    true
}
pub(crate) unsafe fn make_name<F>(mut f: F) -> (bool, Option<u16>)
where
    F: FnMut(&mut pool_pointer, &mut pool_pointer, &mut bool, &mut Option<u16>),
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

    let mut j: pool_pointer = 0;
    if str_ptr + 3 > max_strings as i32 {
        overflow("number of strings", max_strings - init_str_ptr as usize);
    }
    /* area_delimiter is the length from the start of the filename to the
     * directory seperator "/", which we use to construct the stringpool
     * string `cur_area`. If there was already a string in the stringpool for
     * the area, reuse it. */
    if area_delimiter == 0 {
        cur_area = EMPTY_STRING as str_number
    } else {
        cur_area = str_ptr;
        str_start[((str_ptr + 1) as i64 - 65536) as usize] =
            str_start[(str_ptr - 65536) as usize] + area_delimiter;
        str_ptr += 1;
        if let Some(temp_str) = search_string(cur_area) {
            cur_area = temp_str;
            str_ptr -= 1;
            j = str_start[((str_ptr + 1) as i64 - 65536) as usize];
            while j <= pool_ptr - 1 {
                str_pool[(j - area_delimiter) as usize] = str_pool[j as usize];
                j += 1
            }
            pool_ptr = pool_ptr - area_delimiter
        }
    }
    /* ext_delimiter is the length from the start of the filename to the
     * extension '.' delimiter, which we use to construct the stringpool
     * strings `cur_ext` and `cur_name`. */
    if ext_delimiter == 0 {
        cur_ext = EMPTY_STRING as str_number;
        cur_name = slow_make_string()
    } else {
        cur_name = str_ptr;
        str_start[((str_ptr + 1) as i64 - 65536) as usize] =
            str_start[(str_ptr - 65536) as usize] + ext_delimiter - area_delimiter - 1;
        str_ptr += 1;
        cur_ext = make_string();
        str_ptr -= 1;
        if let Some(temp_str) = search_string(cur_name) {
            cur_name = temp_str;
            str_ptr -= 1;
            j = str_start[((str_ptr + 1) as i64 - 65536) as usize];
            while j <= pool_ptr - 1 {
                str_pool[(j - ext_delimiter + area_delimiter + 1) as usize] = str_pool[j as usize];
                j += 1
            }
            pool_ptr = pool_ptr - ext_delimiter + area_delimiter + 1i32
        }
        cur_ext = slow_make_string()
    };
    (quoted_filename, file_name_quote_char)
}
pub(crate) unsafe fn pack_file_name(name: str_number, path: str_number, ext: str_number) {
    name_of_file = gettexstring(path) + &gettexstring(name) + &gettexstring(ext);
}
pub(crate) unsafe fn make_name_string() -> str_number {
    if pool_ptr as usize + name_of_file.as_bytes().len() > pool_size as usize
        || str_ptr == max_strings as i32
        || cur_length() > 0
    {
        return '?' as i32;
    }
    let name_of_file16: Vec<u16> = name_of_file.encode_utf16().collect();
    for &k in &name_of_file16 {
        str_pool[pool_ptr as usize] = k;
        pool_ptr += 1;
    }
    let Result: str_number = make_string();
    let save_name_in_progress = name_in_progress;
    name_in_progress = true;
    make_name(|a, e, q, qc| {
        for &k in &name_of_file16 {
            if !more_name(k, false, a, e, q, qc) {
                break;
            }
        }
    });
    name_in_progress = save_name_in_progress;
    Result
}
pub(crate) unsafe fn scan_file_name(input: &mut input_state_t) -> (bool, Option<u16>) {
    name_in_progress = true;
    let res = make_name(|a, e, q, qc| {
        let (mut tok, mut cmd, mut chr, _) = loop {
            let next = get_x_token(input);
            if !(next.1 == Cmd::Spacer) {
                break next;
            }
        };
        loop {
            if cmd > Cmd::OtherChar || chr > BIGGEST_CHAR {
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
pub(crate) unsafe fn pack_job_name(s: &str) {
    cur_area = EMPTY_STRING as str_number;
    cur_ext = maketexstring(s);
    cur_name = job_name;
    pack_file_name(cur_name, cur_area, cur_ext);
}
pub(crate) unsafe fn open_log_file() {
    let old_setting = selector;
    if job_name == 0 {
        job_name = maketexstring("texput")
    }
    pack_job_name(".log");
    let log_name = CString::new(name_of_file.as_str()).unwrap();
    log_file = ttstub_output_open(log_name.as_ptr(), 0);
    if log_file.is_none() {
        abort!("cannot open log file output \"{}\"", name_of_file);
    }
    texmf_log_name = make_name_string();
    selector = Selector::LOG_ONLY;
    log_opened = true;
    INPUT_STACK[INPUT_PTR] = cur_input;
    /* Here we catch the log file up with anything that has already been
     * printed. The eqtb reference is end_line_char. */
    print_nl_cstr("**");
    let mut l = INPUT_STACK[0].limit;
    if BUFFER[l as usize] == *INTPAR(IntPar::end_line_char) {
        l -= 1
    }
    let mut k = 1;
    while k <= l {
        print(BUFFER[k as usize]);
        k += 1;
    }
    print_ln();
    selector = (u8::from(old_setting) + 2).into();
}
pub(crate) unsafe fn start_input(input: &mut input_state_t, mut primary_input_name: *const i8) {
    let mut format = TTInputFormat::TEX;
    if !primary_input_name.is_null() {
        /* If this is the case, we're opening the primary input file, and the
         * name that we should use to refer to it has been handed directly to
         * us. We emulate the hacks used below to fill in cur_name, etc., from
         * a UTF-8 C string. It looks like the `cur_{name,area,ext}` strings
         * are hardly used so it'd be nice to get rid of them someday. */
        format = TTInputFormat::TECTONIC_PRIMARY;
        name_in_progress = true;
        make_name(|area_delimiter, ext_delimiter, _, _| {
            stop_at_space = false;
            let mut cp: *const u8 = primary_input_name as *const u8;
            assert!(
                !((pool_ptr as usize).wrapping_add(strlen(primary_input_name).wrapping_mul(2))
                    >= pool_size as usize),
                "string pool overflow [{} bytes]",
                pool_size,
            );
            loop {
                let mut rval = *cp as u32;
                if !(rval != 0_u32) {
                    break;
                }
                cp = cp.offset(1);
                let mut extraBytes: u16 = bytesFromUTF8[rval as usize] as u16;
                if extraBytes < 6 {
                    for _ in 0..extraBytes {
                        rval <<= 6i32;
                        if *cp != 0 {
                            rval = (rval as u32).wrapping_add(*cp as u32) as u32 as u32;
                            cp = cp.offset(1);
                        }
                    }
                }
                rval =
                    (rval as u32).wrapping_sub(offsetsFromUTF8[extraBytes as usize]) as u32 as u32;
                if rval > 0xffff_u32 {
                    rval = (rval as u32).wrapping_sub(0x10000_u32) as u32 as u32;
                    let fresh45 = pool_ptr;
                    pool_ptr = pool_ptr + 1;
                    str_pool[fresh45 as usize] = (0xd800_u32)
                        .wrapping_add(rval.wrapping_div(0x400_u32))
                        as packed_UTF16_code;
                    let fresh46 = pool_ptr;
                    pool_ptr = pool_ptr + 1;
                    str_pool[fresh46 as usize] =
                        (0xdc00_u32).wrapping_add(rval.wrapping_rem(0x400_u32)) as packed_UTF16_code
                } else {
                    let fresh47 = pool_ptr;
                    pool_ptr = pool_ptr + 1;
                    str_pool[fresh47 as usize] = rval as packed_UTF16_code
                }
                if rval == '/' as i32 as u32 {
                    *area_delimiter = cur_length();
                    *ext_delimiter = 0;
                } else if rval == '.' as i32 as u32 {
                    *ext_delimiter = cur_length()
                }
            }
            stop_at_space = true;
        });
        name_in_progress = false
    } else {
        /* Scan in the file name from the current token stream. The file name to
         * input is saved as the stringpool strings `cur_{name,area,ext}` and the
         * UTF-8 string `name_of_file`. */
        scan_file_name(input);
    }
    pack_file_name(cur_name, cur_area, cur_ext);
    /* Open up the new file to be read. The name of the file to be read comes
     * from `name_of_file`. */
    begin_file_reading(input);
    let ufile = u_open_in(
        format,
        b"rb",
        UnicodeMode::from(*INTPAR(IntPar::xetex_default_input_mode)),
        *INTPAR(IntPar::xetex_default_input_encoding),
    );
    if ufile.is_none() {
        abort!("failed to open input file \"{}\"", name_of_file);
    }
    INPUT_FILE[cur_input.index as usize] = ufile;
    /* Now re-encode `name_of_file` into the UTF-16 variable `name_of_file16`,
     * and use that to recompute `cur_{name,area,ext}`. */
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
    /* Now generate a stringpool string corresponding to the full path of the
     * input file. This calls make_utf16_name() again and reruns through the
     * {begin,more,end}_name() trifecta to re-re-compute
     * `cur_{name,area,ext}`. */
    input.name = make_name_string();
    SOURCE_FILENAME_STACK[IN_OPEN] = input.name;
    /* *This* variant is a TeX string made out of `name_of_input_file`. */
    FULL_SOURCE_FILENAME_STACK[IN_OPEN] = maketexstring(&name_of_input_file);
    if input.name == str_ptr - 1 {
        if let Some(temp_str) = search_string(input.name) {
            input.name = temp_str;
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
        }
    }
    /* Finally we start really doing stuff with the newly-opened file. */
    if job_name == 0 {
        job_name = cur_name; /* this is the "flush_string" macro which discards the most recent string */
        open_log_file(); /* "really a CFDictionaryRef or *mut XeTeXLayoutEngine" */
    } /* = first_math_fontdimen (=10) + lastMathConstant (= radicalDegreeBottomRaisePercent = 55) */
    if term_offset + length(FULL_SOURCE_FILENAME_STACK[IN_OPEN]) > max_print_line - 2 {
        print_ln();
    } else if term_offset > 0 || file_offset > 0 {
        print_chr(' ');
    }
    print_chr('(');
    open_parens += 1;
    print(FULL_SOURCE_FILENAME_STACK[IN_OPEN]);
    rust_stdout.as_mut().unwrap().flush().unwrap();
    input.state = InputState::NewLine;
    synctex_start_input(input);
    line = 1;
    input_line(INPUT_FILE[input.index as usize].as_mut().unwrap());
    input.limit = last;
    if *INTPAR(IntPar::end_line_char) < 0 || *INTPAR(IntPar::end_line_char) > 255 {
        input.limit -= 1
    } else {
        BUFFER[input.limit as usize] = *INTPAR(IntPar::end_line_char)
    }
    first = input.limit + 1;
    input.loc = input.start;
}
pub(crate) unsafe fn effective_char_info(mut f: internal_font_number, mut c: u16) -> b16x4 {
    if !xtx_ligature_present && !(FONT_MAPPING[f]).is_null() {
        c = apply_tfm_font_mapping(FONT_MAPPING[f], c as i32) as u16
    }
    xtx_ligature_present = false;
    FONT_CHARACTER_INFO(f, c as usize)
}
pub(crate) unsafe fn char_warning(mut f: internal_font_number, mut c: i32) {
    if *INTPAR(IntPar::tracing_lost_chars) > 0 {
        let old_setting = *INTPAR(IntPar::tracing_online);
        if *INTPAR(IntPar::tracing_lost_chars) > 1 {
            *INTPAR(IntPar::tracing_online) = 1
        }
        diagnostic(false, || {
            print_nl_cstr("Missing character: There is no ");
            if (c as i64) < 65536 {
                print(c);
            } else {
                print_char(c);
            }
            print_cstr(" in font ");
            print(FONT_NAME[f]);
            print_chr('!');
        });
        *INTPAR(IntPar::tracing_online) = old_setting
    }
    let fn_0 = gettexstring(FONT_NAME[f]);
    let prev_selector = selector;
    let mut s: i32 = 0;
    selector = Selector::NEW_STRING;
    if c < 0x10000 {
        print(c);
    } else {
        print_char(c);
    }
    selector = prev_selector;
    s = make_string();
    let chr = gettexstring(s);
    str_ptr -= 1;
    pool_ptr = str_start[(str_ptr - 0x10000) as usize];
    ttstub_issue_warning(&format!(
        "could not represent character \"{}\" (0x{:x}) in font \"{}\"",
        chr, c as u32, fn_0
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
        ((n as u64) * (::std::mem::size_of::<UTF16_code>() as u64)
            + (::std::mem::size_of::<memory_word>() as u64)
            - 1)
            / (::std::mem::size_of::<memory_word>() as u64),
    ) as i32;
    let mut q = NativeWord::from(get_node(l) as usize);
    set_NODE_type(q.ptr(), TextNode::WhatsIt);
    q.set_actual_text(*INTPAR(IntPar::xetex_generate_actual_text) > 0);
    q.set_size(l as u16)
        .set_font(f as u16)
        .set_length(n as u16)
        .set_glyph_count(0)
        .set_glyph_info_ptr(ptr::null_mut());
    q
}
pub(crate) unsafe fn new_native_character(
    f: internal_font_number,
    mut c: UnicodeScalar,
) -> NativeWord {
    let mut p;
    let nf = FONT_LAYOUT_ENGINE[f].as_native();
    if !(FONT_MAPPING[f]).is_null() {
        if c as i64 > 65535 {
            if pool_ptr + 2 > pool_size {
                overflow("pool size", (pool_size - init_pool_ptr) as usize);
            }
            str_pool[pool_ptr as usize] =
                ((c as i64 - 65536) / 1024 as i64 + 0xd800) as packed_UTF16_code;
            pool_ptr += 1;
            str_pool[pool_ptr as usize] =
                ((c as i64 - 65536) % 1024 as i64 + 0xdc00) as packed_UTF16_code;
            pool_ptr += 1
        } else {
            if pool_ptr + 1 > pool_size {
                overflow("pool size", (pool_size - init_pool_ptr) as usize);
            }
            str_pool[pool_ptr as usize] = c as packed_UTF16_code;
            pool_ptr += 1
        }

        let len = apply_mapping(
            FONT_MAPPING[f],
            &mut str_pool[str_start[(str_ptr - TOO_BIG_CHAR) as usize] as usize],
            cur_length(),
        );
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];

        let mut i = 0;

        while i < len {
            if *mapped_text.offset(i as isize) as i32 >= 0xd800
                && (*mapped_text.offset(i as isize) as i32) < 0xdc00
            {
                c = (*mapped_text.offset(i as isize) as i32 - 0xd800) * 1024
                    + *mapped_text.offset((i + 1) as isize) as i32
                    + 9216;
                if map_char_to_glyph(nf, c) == 0 {
                    char_warning(f, c);
                }
                i += 2;
            } else {
                if map_char_to_glyph(nf, *mapped_text.offset(i as isize) as i32) == 0 {
                    char_warning(f, *mapped_text.offset(i as isize) as i32);
                }
                i += 1;
            }
        }

        p = new_native_word_node(f, len);
        let slice = std::slice::from_raw_parts(mapped_text, len as usize);
        p.text_mut().copy_from_slice(&slice[..len as usize]);
    } else {
        if *INTPAR(IntPar::tracing_lost_chars) > 0 {
            if map_char_to_glyph(nf, c) == 0 {
                char_warning(f, c);
            }
        }
        p = NativeWord::from(get_node(NATIVE_NODE_SIZE + 1));
        set_NODE_type(p.ptr(), TextNode::WhatsIt);
        p.set_actual_text(false);
        p.set_size((NATIVE_NODE_SIZE + 1) as u16);
        p.set_glyph_count(0);
        p.set_glyph_info_ptr(ptr::null_mut());
        p.set_font(f as u16);
        if c as i64 > 65535 {
            p.set_length(2);
            p.text_mut().copy_from_slice(&[
                ((c as i64 - 65536) / 1024 as i64 + 0xd800) as u16,
                ((c as i64 - 65536) % 1024 as i64 + 0xdc00) as u16,
            ]);
        } else {
            p.set_length(1);
            p.text_mut()[0] = c as u16;
        }
    }
    p.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
    p
}
pub(crate) unsafe fn font_feature_warning(feature_name: &[u8], setting_name: &[u8]) {
    diagnostic(false, || {
        print_nl_cstr("Unknown ");
        if !setting_name.is_empty() {
            print_cstr("selector `");
            print_utf8_str(setting_name);
            print_cstr("\' for ");
        }
        print_cstr("feature `");
        print_utf8_str(feature_name);
        print_cstr("\' in font `");
        for b in name_of_file.bytes() {
            print_raw_char(b as UTF16_code, true);
        }
        print_cstr("\'.");
    });
}
pub(crate) unsafe fn font_mapping_warning(mapping_name: &str, mut warningType: i32) {
    diagnostic(false, || {
        if warningType == 0i32 {
            print_nl_cstr("Loaded mapping `");
        } else {
            print_nl_cstr("Font mapping `");
        }
        print_utf8_str(mapping_name.as_bytes());
        print_cstr("\' for font `");
        for b in name_of_file.bytes() {
            print_raw_char(b as UTF16_code, true);
        }
        match warningType {
            1 => print_cstr("\' not found."),
            2 => {
                print_cstr("\' not usable;");
                print_nl_cstr("bad mapping file or incorrect mapping type.");
            }
            _ => print_cstr("\'."),
        }
    });
}
pub(crate) unsafe fn graphite_warning() {
    diagnostic(false, || {
        print_nl_cstr("Font `");
        for b in name_of_file.bytes() {
            print_raw_char(b as UTF16_code, true);
        }
        print_cstr("\' does not support Graphite. Trying OpenType layout instead.");
    });
}
pub(crate) unsafe fn do_locale_linebreaks(mut s: i32, mut len: i32) {
    let mut offs: i32 = 0;
    let mut prevOffs: i32 = 0;
    let mut use_penalty: bool = false;
    let mut use_skip: bool = false;
    if *INTPAR(IntPar::xetex_linebreak_locale) == 0 || len == 1 {
        let mut nwn = new_native_word_node(main_f, len);
        *LLIST_link(cur_list.tail) = Some(nwn.ptr()).tex_int();
        cur_list.tail = nwn.ptr();
        let tail_text = nwn.text_mut();
        let slice = std::slice::from_raw_parts(native_text.offset(s as isize), len as usize);
        tail_text.copy_from_slice(&slice[..len as usize]);

        nwn.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
    } else {
        use_skip = *GLUEPAR(GluePar::xetex_linebreak_skip) != 0;
        use_penalty = *INTPAR(IntPar::xetex_linebreak_penalty) != 0 || !use_skip;
        linebreak_start(
            main_f,
            *INTPAR(IntPar::xetex_linebreak_locale),
            native_text.offset(s as isize),
            len,
        );
        offs = 0;
        loop {
            prevOffs = offs;
            offs = linebreak_next();
            if offs > 0 {
                if prevOffs != 0 {
                    if use_penalty {
                        let pen = new_penalty(*INTPAR(IntPar::xetex_linebreak_penalty));
                        *LLIST_link(cur_list.tail) = Some(pen).tex_int();
                        cur_list.tail = pen as usize;
                    }
                    if use_skip {
                        let pg = new_param_glue(GluePar::xetex_linebreak_skip);
                        *LLIST_link(cur_list.tail) = Some(pg).tex_int();
                        cur_list.tail = pg;
                    }
                }
                let mut nwn = new_native_word_node(main_f, offs - prevOffs);
                *LLIST_link(cur_list.tail) = Some(nwn.ptr()).tex_int();
                cur_list.tail = nwn.ptr();

                let tail_text = nwn.text_mut();
                let slice =
                    std::slice::from_raw_parts(native_text.offset(s as isize), offs as usize);
                tail_text.copy_from_slice(&slice[prevOffs as usize..offs as usize]);

                nwn.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
            }
            if offs < 0 {
                break;
            }
        }
    };
}
pub(crate) unsafe fn bad_utf8_warning() {
    diagnostic(false, || {
        print_nl_cstr("Invalid UTF-8 byte or sequence");
        if cur_input.name == 0 {
            print_cstr(" in terminal input");
        } else {
            print_cstr(" at line ");
            print_int(line);
        }
        print_cstr(" replaced by U+FFFD.");
    });
}
pub(crate) unsafe fn get_input_normalization_state() -> i32 {
    if EQTB.is_empty() {
        0
    } else {
        *INTPAR(IntPar::xetex_input_normalization)
    }
}
pub(crate) unsafe fn get_tracing_fonts_state() -> i32 {
    *INTPAR(IntPar::xetex_tracing_fonts)
}

pub(crate) unsafe fn new_character(
    mut f: internal_font_number,
    mut c: UTF16_code,
) -> Option<usize> {
    let mut ec: u16 = 0;
    if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
        return Some(new_native_character(f, c as UnicodeScalar).ptr());
    }
    ec = effective_char(false, f, c) as u16;
    if FONT_BC[f] as i32 <= ec as i32 {
        if FONT_EC[f] as i32 >= ec as i32 {
            if FONT_INFO[(CHAR_BASE[f] + ec as i32) as usize].b16.s3 > 0 {
                let mut p = Char(get_avail());
                p.set_font(f as u16);
                p.set_character(c);
                return Some(p.ptr());
            }
        }
    }
    char_warning(f, c as i32);
    None
}
pub(crate) unsafe fn scan_spec(
    input: &mut input_state_t,
    c: GroupCode,
    mut three_codes: bool,
) -> Cmd {
    let mut s: i32 = 0;
    let mut spec_code: PackMode = PackMode::Exactly;
    if three_codes {
        s = SAVE_STACK[SAVE_PTR + 0].val
    }
    let mut sd = None;
    if scan_keyword(input, b"to") {
        spec_code = PackMode::Exactly;
    } else if scan_keyword(input, b"spread") {
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
    if *INTPAR(IntPar::texxet) > 0 {
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
                            match Node::from(q) {
                                Node::Text(TxtNode::Disc(d)) => k = d.replace_count() as i32,
                                _ => {}
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
                            p_nw.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
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
                    if *INTPAR(IntPar::texxet) > 0 {
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
    if x == Scaled::ZERO {
        r.set_glue_sign(GlueSign::Normal)
            .set_glue_order(GlueOrder::Normal)
            .set_glue_set(0.);
        return exit(r, q);
    } else if x > Scaled::ZERO {
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
        if o == GlueOrder::Normal {
            if r.list_ptr().opt().is_some() {
                /*685: */
                last_badness = badness(x, total_stretch[NORMAL as usize]); /*normal *//*:690 */
                if last_badness > *INTPAR(IntPar::hbadness) {
                    print_ln();
                    if last_badness > 100 {
                        print_nl_cstr("Underfull");
                    } else {
                        print_nl_cstr("Loose");
                    }
                    print_cstr(" \\hbox (badness ");
                    print_int(last_badness);
                    return common_ending(r, q);
                }
            }
        }
        return exit(r, q);
    } else {
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
        if total_shrink[o as usize] < -x && o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
            last_badness = 1000000;
            r.set_glue_set(1.);
            if -x - total_shrink[0] > *DIMENPAR(DimenPar::hfuzz) || *INTPAR(IntPar::hbadness) < 100
            {
                if *DIMENPAR(DimenPar::overfull_rule) > Scaled::ZERO
                    && -x - total_shrink[0] > *DIMENPAR(DimenPar::hfuzz)
                {
                    while let Some(next) = LLIST_link(q as usize).opt() {
                        q = next;
                    }
                    *LLIST_link(q as usize) = Some(new_rule().ptr()).tex_int();
                    MEM[(*LLIST_link(q as usize) + 1) as usize].b32.s1 =
                        (*DIMENPAR(DimenPar::overfull_rule)).0
                }
                print_ln();
                print_nl_cstr("Overfull \\hbox (");
                print_scaled(-x - total_shrink[NORMAL as usize]);
                print_cstr("pt too wide");
                return common_ending(r, q);
            }
        } else if o == GlueOrder::Normal {
            if r.list_ptr().opt().is_some() {
                /*692: */
                last_badness = badness(-x, total_shrink[NORMAL as usize]);
                if last_badness > *INTPAR(IntPar::hbadness) {
                    print_ln();
                    print_nl_cstr("Tight \\hbox (badness ");
                    print_int(last_badness);
                    return common_ending(r, q);
                }
            }
        }

        return exit(r, q);
    }

    unsafe fn common_ending(r: List, q: usize) -> List {
        if output_active {
            print_cstr(") has occurred while \\output is active");
        } else {
            if pack_begin_line != 0 {
                if pack_begin_line > 0 {
                    print_cstr(") in paragraph at lines ");
                } else {
                    print_cstr(") in alignment at lines ");
                }
                print_int(pack_begin_line.abs());
                print_cstr("--");
            } else {
                print_cstr(") detected at line ");
            }
            print_int(line);
        }
        print_ln();
        font_in_short_display = 0;
        short_display(r.list_ptr().opt());
        print_ln();
        diagnostic(true, || show_box(Some(r.ptr())));
        return exit(r, q);
    }

    unsafe fn exit(r: List, mut q: usize) -> List {
        if *INTPAR(IntPar::texxet) > 0 {
            /*1499: */
            if Math(LR_ptr as usize).subtype_i32() != MathType::Before {
                while let Some(next) = llist_link(q) {
                    q = next;
                } /*:673 */
                loop {
                    let tmp_ptr = q;
                    q = new_math(Scaled::ZERO, Math(LR_ptr as usize).subtype_i32());
                    *LLIST_link(tmp_ptr) = Some(q).tex_int();
                    LR_problems = LR_problems + 10000;
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
                print_nl_cstr("\\endL or \\endR problem (");
                print_int(LR_problems / 10000);
                print_cstr(" missing, ");
                print_int(LR_problems % 10000);
                print_cstr(" extra");
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
    mut m: PackMode,
    mut l: Scaled,
) -> List {
    last_badness = 0;
    let mut r = List::from(get_node(BOX_NODE_SIZE) as usize);
    r.set_vertical();
    r.set_lr_mode(if *INTPAR(IntPar::xetex_upwards) > 0 {
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
    if x == Scaled::ZERO {
        r.set_glue_sign(GlueSign::Normal)
            .set_glue_order(GlueOrder::Normal)
            .set_glue_set(0.);
    } else if x > Scaled::ZERO {
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
        if o == GlueOrder::Normal {
            if r.list_ptr().opt().is_some() {
                /*699: */
                last_badness = badness(x, total_stretch[GlueOrder::Normal as usize]); /*normal *//*:690 */
                if last_badness > *INTPAR(IntPar::vbadness) {
                    print_ln();
                    if last_badness > 100 {
                        print_nl_cstr("Underfull");
                    } else {
                        print_nl_cstr("Loose");
                    }
                    print_cstr(" \\vbox (badness ");
                    print_int(last_badness);
                    return common_ending(r);
                }
            }
        }
    } else {
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
        if total_shrink[o as usize] < -x && o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
            last_badness = 1000000;
            r.set_glue_set(1.);
            if -x - total_shrink[GlueOrder::Normal as usize] > *DIMENPAR(DimenPar::vfuzz)
                || *INTPAR(IntPar::vbadness) < 100
            {
                print_ln();
                print_nl_cstr("Overfull \\vbox (");
                print_scaled(-x - total_shrink[NORMAL as usize]);
                print_cstr("pt too high");
                return common_ending(r);
            }
        } else if o == GlueOrder::Normal {
            if r.list_ptr().opt().is_some() {
                /*703: */
                last_badness = badness(-x, total_shrink[NORMAL as usize]);
                if last_badness > *INTPAR(IntPar::vbadness) {
                    print_ln();
                    print_nl_cstr("Tight \\vbox (badness ");
                    print_int(last_badness);
                    return common_ending(r);
                }
            }
        }
    }
    return r;

    unsafe fn common_ending(r: List) -> List {
        if output_active {
            print_cstr(") has occurred while \\output is active");
        } else {
            if pack_begin_line != 0 {
                print_cstr(") in alignment at lines ");
                print_int(pack_begin_line.abs());
                print_cstr("--");
            } else {
                print_cstr(") detected at line ");
            }
            print_int(line);
            print_ln();
        }
        diagnostic(true, || show_box(Some(r.ptr())));
        return r;
    }
}
pub(crate) unsafe fn append_to_vlist(b: List) {
    let mut upwards: bool = false;
    upwards = *INTPAR(IntPar::xetex_upwards) > 0;
    if cur_list.aux.b32.s1 > IGNORE_DEPTH {
        let d = if upwards {
            Scaled(MEM[(*GLUEPAR(GluePar::baseline_skip) + 1) as usize].b32.s1)
                - Scaled(cur_list.aux.b32.s1)
                - b.depth()
        } else {
            Scaled(MEM[(*GLUEPAR(GluePar::baseline_skip) + 1) as usize].b32.s1)
                - Scaled(cur_list.aux.b32.s1)
                - b.height()
        };
        let p = if d < *DIMENPAR(DimenPar::line_skip_limit) {
            new_param_glue(GluePar::line_skip)
        } else {
            let (p, mut tmp_ptr) = new_skip_param(GluePar::baseline_skip);
            tmp_ptr.set_size(d);
            p
        };
        *LLIST_link(cur_list.tail) = Some(p).tex_int();
        cur_list.tail = p;
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
    p.first_mut().empty();
    p.third_mut().empty();
    p.second_mut().empty();
    p.ptr()
}
pub(crate) unsafe fn new_style(mut s: i16) -> usize {
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
pub(crate) unsafe fn show_info(tmp_ptr: usize) {
    show_node_list(MEM[tmp_ptr].b32.s0.opt());
}
pub(crate) unsafe fn push_alignment() {
    let p = get_node(ALIGN_STACK_NODE_SIZE);
    *LLIST_link(p) = align_ptr.tex_int();
    MEM[p].b32.s0 = cur_align.tex_int();
    MEM[p + 1].b32.s0 = *LLIST_link(ALIGN_HEAD);
    MEM[p + 1].b32.s1 = cur_span.tex_int();
    MEM[p + 2].b32.s1 = cur_loop.tex_int();
    MEM[p + 3].b32.s1 = align_state;
    MEM[p + 4].b32.s0 = cur_head.tex_int();
    MEM[p + 4].b32.s1 = cur_tail.tex_int();
    MEM[p + 5].b32.s0 = cur_pre_head.tex_int();
    MEM[p + 5].b32.s1 = cur_pre_tail.tex_int();
    align_ptr = Some(p);
    cur_head = Some(get_avail());
    cur_pre_head = Some(get_avail());
}
pub(crate) unsafe fn pop_alignment() {
    MEM[cur_head.unwrap()].b32.s1 = avail.tex_int();
    avail = cur_head;
    MEM[cur_pre_head.unwrap()].b32.s1 = avail.tex_int();
    avail = cur_pre_head;
    let p = align_ptr.unwrap();
    cur_tail = MEM[p + 4].b32.s1.opt();
    cur_head = MEM[p + 4].b32.s0.opt();
    cur_pre_tail = MEM[p + 5].b32.s1.opt();
    cur_pre_head = MEM[p + 5].b32.s0.opt();
    align_state = MEM[p + 3].b32.s1;
    cur_loop = MEM[p + 2].b32.s1.opt();
    cur_span = MEM[p + 1].b32.s1.opt();
    *LLIST_link(ALIGN_HEAD) = MEM[p + 1].b32.s0;
    cur_align = MEM[p].b32.s0.opt();
    align_ptr = llist_link(p);
    free_node(p, ALIGN_STACK_NODE_SIZE);
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
        if *INTPAR(IntPar::global_defs) > 0 {
            geq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                val.opt(),
            );
        } else {
            eq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                val.opt(),
            );
        }
    }
    (otok, ocmd)
}
pub(crate) unsafe fn init_align(input: &mut input_state_t, wcs: i32) {
    let mut p: i32 = 0;
    push_alignment();
    align_state = -1000000;
    if cur_list.mode == (false, ListMode::MMode)
        && (cur_list.tail != cur_list.head || cur_list.aux.b32.s1.opt().is_some())
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Improper ");
        print_esc_cstr("halign");
        print_cstr(" inside $$\'s");
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
    } else if cur_list.mode.0 == false {
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
        *LLIST_link(ca) = Some(ca2).tex_int();
        /*:808 */
        cur_align = Some(ca2); /*:807*/
        if ocmd == Cmd::CarRet {
            break; /*:813*/
        } /*:806 */
        p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            let (tok, cmd) = get_preamble_token(input);
            ocmd = cmd;
            if cmd == Cmd::MacParam {
                break;
            }
            if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state as i64 == -1000000 {
                if p == HOLD_HEAD as i32 && cur_loop.is_none() && cmd == Cmd::TabMark {
                    cur_loop = Some(ca2);
                } else {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Missing # inserted in alignment preamble");
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
        ca = new_null_box();
        *LLIST_link(ca2) = Some(ca).tex_int();
        cur_align = Some(ca);
        MEM[ca].b32.s0 = END_SPAN as i32;
        MEM[ca + 1].b32.s1 = NULL_FLAG.0;
        MEM[ca + 3].b32.s1 = *LLIST_link(HOLD_HEAD);
        p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            let (tok, cmd) = get_preamble_token(input);
            ocmd = cmd;
            if (cmd == Cmd::CarRet || cmd == Cmd::TabMark) && align_state as i64 == -1000000 {
                break;
            }
            if cmd == Cmd::MacParam {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Only one # is allowed per tab");
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
        p = *LLIST_link(p as usize);
        MEM[p as usize].b32.s0 = CS_TOKEN_FLAG + FROZEN_END_TEMPLATE as i32;
        MEM[ca + 2].b32.s1 = *LLIST_link(HOLD_HEAD)
    }
    scanner_status = ScannerStatus::Normal;
    new_save_level(GroupCode::Align);
    if let Some(l) = LOCAL(Local::every_cr).opt() {
        begin_token_list(input, l, Btl::EveryCRText);
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
    let g = new_glue(MEM[(*LLIST_link(ALIGN_HEAD) + 1) as usize].b32.s0 as usize);
    *LLIST_link(cur_list.tail) = Some(g).tex_int();
    cur_list.tail = g;
    MEM[cur_list.tail].b16.s0 = GluePar::tab_skip as u16 + 1;
    cur_align = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1.opt();
    cur_tail = cur_head;
    cur_pre_tail = cur_pre_head;
    init_span(cur_align);
}
pub(crate) unsafe fn init_col(input: &mut input_state_t, tok: i32, cmd: Cmd) {
    let ca = cur_align.unwrap();
    MEM[ca + 5].b32.s0 = cmd as i32;
    if cmd == Cmd::Omit {
        align_state = 0;
    } else {
        back_input(input, tok);
        begin_token_list(input, MEM[ca + 3].b32.s1 as usize, Btl::UTemplate);
    };
}
pub(crate) unsafe fn fin_col(input: &mut input_state_t) -> bool {
    let ca = cur_align.confuse("endv");
    let q = llist_link(ca).confuse("endv");
    if (align_state as i64) < 500000 {
        fatal_error("(interwoven alignment preambles are not allowed)");
    }
    let mut p = llist_link(q);
    if p.is_none() && MEM[ca + 5].b32.s0 < CR_CODE {
        if let Some(cl) = cur_loop {
            /*822: */
            let nb = new_null_box();
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
            *LLIST_link(nb) = new_glue(MEM[cl + 1].b32.s0 as usize) as i32;
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Extra alignment tab has been changed to ");
            print_esc_cstr("cr");
            help!(
                "You have given more \\span or & marks than there were",
                "in the preamble to the \\halign or \\valign now in progress.",
                "So I\'ll assume that you meant to type \\cr instead."
            );
            MEM[ca + 5].b32.s0 = CR_CODE;
            error();
        }
    }
    if MEM[ca + 5].b32.s0 != SPAN_CODE {
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
        if cur_span != Some(ca) {
            /*827: */
            let mut q = cur_span; /*normal *//*:684 */
            loop {
                n += 1; /*normal *//*:690 */
                q = MEM[MEM[q.unwrap()].b32.s1 as usize].b32.s1.opt(); /*tab_skip_code 1 *//*:824 */
                if q == Some(ca) {
                    break;
                }
            }
            if n > u16::MAX as i32 {
                confusion("too many spans");
            }
            let mut q = cur_span.unwrap();
            while MEM[MEM[q].b32.s0 as usize].b32.s1 < n {
                q = MEM[q].b32.s0 as usize;
            }
            if MEM[MEM[q].b32.s0 as usize].b32.s1 > n {
                let s = get_node(SPAN_NODE_SIZE);
                MEM[s].b32.s0 = MEM[q].b32.s0;
                MEM[s].b32.s1 = n;
                MEM[q].b32.s0 = Some(s).tex_int();
                MEM[s + 1].b32.s1 = w.0
            } else if Scaled(MEM[(MEM[q].b32.s0 + 1) as usize].b32.s1) < w {
                MEM[(MEM[q].b32.s0 + 1) as usize].b32.s1 = w.0;
            }
        } else if w > Scaled(MEM[ca + 1].b32.s1) {
            MEM[ca + 1].b32.s1 = w.0
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
        let g = new_glue(MEM[(*LLIST_link(ca) + 1) as usize].b32.s0 as usize);
        *LLIST_link(cur_list.tail) = Some(g).tex_int();
        cur_list.tail = g;
        MEM[cur_list.tail].b16.s0 = 12;
        if MEM[ca + 5].b32.s0 >= CR_CODE {
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
        begin_token_list(input, ecr, Btl::EveryCRText);
    }
    align_peek(input);
}
pub(crate) unsafe fn fin_align(input: &mut input_state_t, group: GroupCode) {
    let mut n: i32 = 0;
    let mut rule_save: Scaled = Scaled::ZERO;
    let mut aux_save: memory_word = memory_word {
        b32: b32x2 { s0: 0, s1: 0 },
    };
    if group != GroupCode::Align {
        confusion("align1");
    }
    unsave(input);
    if group != GroupCode::Align {
        confusion("align0");
    }
    unsave(input);
    let o = if NEST[(NEST_PTR - 1) as usize].mode == (false, ListMode::MMode) {
        *DIMENPAR(DimenPar::display_indent)
    } else {
        Scaled::ZERO
    };
    let mut q = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1 as usize;
    loop {
        flush_list(MEM[q + 3].b32.s1.opt());
        flush_list(MEM[q + 2].b32.s1.opt());
        let p = MEM[*LLIST_link(q) as usize].b32.s1.opt();
        if Scaled(MEM[q + 1].b32.s1) == NULL_FLAG {
            /*831: */
            MEM[q + 1].b32.s1 = 0;
            let r = *LLIST_link(q) as usize;
            let s = MEM[r + 1].b32.s0;
            if s != 0 {
                GlueSpec(0).rc_inc();
                delete_glue_ref(s as usize);
                MEM[r + 1].b32.s0 = 0
            }
        }
        if MEM[q].b32.s0 != END_SPAN as i32 {
            /*832: */
            let t = MEM[q + 1].b32.s1
                + MEM[(MEM[(*LLIST_link(q) + 1) as usize].b32.s0 + 1) as usize]
                    .b32
                    .s1; /*:833 */
            let mut r = MEM[q].b32.s0 as usize;
            let mut s = END_SPAN;
            MEM[s].b32.s0 = p.tex_int();
            n = 1;
            loop {
                MEM[r + 1].b32.s1 = MEM[r + 1].b32.s1 - t;
                let u = MEM[r].b32.s0 as usize;
                while MEM[r].b32.s1 > n {
                    s = MEM[s].b32.s0 as usize;
                    n = MEM[MEM[s].b32.s0 as usize].b32.s1 + 1;
                }
                if MEM[r].b32.s1 < n {
                    MEM[r].b32.s0 = MEM[s].b32.s0;
                    MEM[s].b32.s0 = r as i32;
                    MEM[r].b32.s1 -= 1;
                    s = r
                } else {
                    if MEM[r + 1].b32.s1 > MEM[(MEM[s].b32.s0 + 1) as usize].b32.s1 {
                        MEM[(MEM[s].b32.s0 + 1) as usize].b32.s1 = MEM[r + 1].b32.s1;
                    }
                    free_node(r, SPAN_NODE_SIZE);
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
        rule_save = *DIMENPAR(DimenPar::overfull_rule);
        *DIMENPAR(DimenPar::overfull_rule) = Scaled::ZERO;
        p = hpack(
            llist_link(ALIGN_HEAD),
            Scaled(SAVE_STACK[SAVE_PTR + 1].val),
            PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
        );
        *DIMENPAR(DimenPar::overfull_rule) = rule_save
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
                    n = MEM[r].b16.s0 as i32; /*840: */
                    let mut t = BaseBox(s).width(); // TODO: check
                    let w = t;
                    let mut u = HOLD_HEAD;
                    MEM[r].b16.s0 = 0;
                    while n > 0 {
                        n -= 1;
                        s = *LLIST_link(s) as usize;
                        let v = GlueSpec(Glue(s).glue_ptr() as usize);
                        let g = new_glue(v.ptr());
                        *LLIST_link(u) = Some(g).tex_int();
                        u = g;
                        MEM[u].b16.s0 = GluePar::tab_skip as u16 + 1;
                        t += v.size();
                        if p.glue_sign() == GlueSign::Stretching {
                            if v.stretch_order() == p.glue_order() {
                                t += tex_round(p.glue_set() * v.stretch().0 as f64);
                            }
                        } else if p.glue_sign() == GlueSign::Shrinking {
                            if v.shrink_order() == p.glue_order() {
                                t -= tex_round(p.glue_set() * v.shrink().0 as f64);
                            }
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
                        if t == r_unset.width() {
                            r_box
                                .set_glue_sign(GlueSign::Normal)
                                .set_glue_order(GlueOrder::Normal)
                                .set_glue_set(0.);
                        } else if t > r_unset.width() {
                            r_box.set_glue_sign(GlueSign::Stretching).set_glue_set(
                                if r_unset.stretch() == Scaled::ZERO {
                                    0.
                                } else {
                                    (t - r_unset.width()).0 as f64 / r_unset.stretch().0 as f64
                                },
                            );
                        } else {
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
                        r_box.set_width(w);
                        r_box.set_horizontal();
                    } else {
                        r_box.set_width(q.width());
                        if t == r_unset.height() {
                            r_box
                                .set_glue_sign(GlueSign::Normal)
                                .set_glue_order(GlueOrder::Normal)
                                .set_glue_set(0.);
                        } else if t > r_unset.height() {
                            r_box.set_glue_sign(GlueSign::Stretching).set_glue_set(
                                if r_unset.stretch() == Scaled::ZERO {
                                    0.
                                } else {
                                    (t - r_unset.height()).0 as f64 / r_unset.stretch().0 as f64
                                },
                            );
                        } else {
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
    aux_save = cur_list.aux;
    let p = MEM[cur_list.head].b32.s1.opt();
    let q = cur_list.tail;
    pop_nest();
    if cur_list.mode == (false, ListMode::MMode) {
        /*1241: */
        let (tok, cmd, _) = do_assignments(input); /*1232: */
        if cmd != Cmd::MathShift {
            /*1242: */
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing $$ inserted");
            help!(
                "Displays can use special alignments (like \\eqalignno)",
                "only if nothing but the alignment itself is between $$\'s."
            );
            back_error(input, tok);
        } else {
            let (tok, cmd, ..) = get_x_token(input);
            if cmd != Cmd::MathShift {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Display math should end with $$");
                help!(
                    "The `$\' that I just saw supposedly matches a previous `$$\'.",
                    "So I shall assume that you typed `$$\' both times."
                );
                back_error(input, tok);
            }
        }
        flush_node_list(cur_list.eTeX_aux);
        pop_nest();
        let pen = new_penalty(*INTPAR(IntPar::pre_display_penalty));
        *LLIST_link(cur_list.tail) = Some(pen).tex_int();
        cur_list.tail = pen;
        let pg = new_param_glue(GluePar::above_display_skip);
        *LLIST_link(cur_list.tail) = Some(pg).tex_int();
        cur_list.tail = pg;
        *LLIST_link(cur_list.tail) = p.tex_int();
        if p.is_some() {
            cur_list.tail = q;
        }
        let pen = new_penalty(*INTPAR(IntPar::post_display_penalty));
        *LLIST_link(cur_list.tail) = Some(pen).tex_int();
        cur_list.tail = pen;
        let pg = new_param_glue(GluePar::below_display_skip);
        *LLIST_link(cur_list.tail) = Some(pg).tex_int();
        cur_list.tail = pg;
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
    (*INTPAR(IntPar::xetex_hyphenatable_length) as usize).min(HYPHENATABLE_LENGTH_LIMIT)
}
pub(crate) unsafe fn eTeX_enabled(mut b: bool, j: Cmd, mut k: i32) -> bool {
    if !b {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Improper ");
        print_cmd_chr(j, k);
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
        print_nl_cstr("### ");
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
        print_cstr(" (");
        let mut s: &str = "";
        match cur_group1 {
            GroupCode::BottomLevel => unreachable!(),
            GroupCode::Simple => {
                p += 1;
                return found2(p, a);
            }
            GroupCode::HBox | GroupCode::AdjustedHBox => {
                s = "hbox";
            }
            GroupCode::VBox => {
                s = "vbox";
            }
            GroupCode::VTop => {
                s = "vtop";
            }
            GroupCode::Align => {
                if a == 0 {
                    if m == (true, ListMode::VMode) {
                        s = "halign"
                    } else {
                        s = "valign"
                    }
                    a = 1;
                    return found1(s, p, a);
                } else {
                    if a == 1 {
                        print_cstr("align entry");
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
                if cur_group1 == GroupCode::Disc {
                    print_esc_cstr("discretionary");
                } else {
                    print_esc_cstr("mathchoice");
                }
                let mut i = 1;
                while i <= 3 {
                    if i <= SAVE_STACK[SAVE_PTR1 - 2].val {
                        print_cstr("{}");
                    }
                    i += 1;
                }
                return found2(p, a);
            }
            GroupCode::Insert => {
                if SAVE_STACK[SAVE_PTR1 - 2].val == 255 {
                    print_esc_cstr("vadjust");
                } else {
                    print_esc_cstr("insert");
                    print_int(SAVE_STACK[SAVE_PTR1 - 2].val);
                }
                return found2(p, a);
            }
            GroupCode::VCenter => {
                s = "vcenter";
                return found1(s, p, a);
            }
            GroupCode::SemiSimple => {
                p += 1;
                print_esc_cstr("begingroup");
                return found(p, a);
            }
            GroupCode::MathShift => {
                if m == (false, ListMode::MMode) {
                    print_chr('$');
                } else if NEST[p].mode == (false, ListMode::MMode) {
                    print_cmd_chr(Cmd::EqNo, SAVE_STACK[SAVE_PTR1 - 2].val);
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
        }

        let mut i = SAVE_STACK[SAVE_PTR1 - 4].val;

        if i != 0 {
            if i < BOX_FLAG {
                let j = if NEST[p].mode.1 == ListMode::VMode {
                    Cmd::HMove
                } else {
                    Cmd::VMove
                };
                if i > 0 {
                    print_cmd_chr(j, 0);
                } else {
                    print_cmd_chr(j, 1);
                }
                print_scaled(Scaled(i.abs()));
                print_cstr("pt");
            } else if i < SHIP_OUT_FLAG {
                if i >= GLOBAL_BOX_FLAG {
                    print_esc_cstr("global");
                    i = i - (GLOBAL_BOX_FLAG - BOX_FLAG)
                }
                print_esc_cstr("setbox");
                print_int(i - BOX_FLAG);
                print_chr('=');
            } else {
                print_cmd_chr(Cmd::LeaderShip, i - (LEADER_FLAG - (A_LEADERS as i32)));
            }
        }
        found1(s, p, a)
    }

    unsafe fn found1(s: &str, p: usize, a: i8) -> (bool, usize, i8) {
        print_esc_cstr(s);
        if SAVE_STACK[SAVE_PTR1 - 2].val != 0 {
            print_chr(' ');
            if SAVE_STACK[SAVE_PTR1 - 3].val == PackMode::Exactly as i32 {
                print_cstr("to");
            } else {
                print_cstr("spread");
            }
            print_scaled(Scaled(SAVE_STACK[SAVE_PTR1 - 2].val));
            print_cstr("pt");
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
    print_nl_cstr("");
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
pub(crate) unsafe fn vert_break(mut p: i32, mut h: Scaled, mut d: Scaled) -> i32 {
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
                    let mut q = GlueSpec(g.glue_ptr() as usize); /*:1011 */
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
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("Infinite glue shrinkage found in box being split");
                            help!(
                                "The box you are \\vsplitting contains some infinitely",
                                "shrinkable glue, e.g., `\\vss\' or `\\vskip 0pt minus 1fil\'.",
                                "Such glue doesn\'t belong there; but you can safely proceed,",
                                "since the offensive shrinkability has been made finite."
                            );
                            error();
                            let mut r = GlueSpec(new_spec(q.ptr()));
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
                    match llist_link(p).map(|next| Node::from(next)) {
                        Some(Node::Text(TxtNode::Glue(_))) => {
                            if with_penalty(p, 0, h, &mut prev_dp, &mut best_place, &mut least_cost)
                            {
                                return best_place.tex_int();
                            }
                        }
                        _ => {}
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
                least_cost = b;
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
                    b = b + pi
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
pub(crate) unsafe fn vsplit(mut n: i32, mut h: Scaled) -> Option<usize> {
    let val = n;
    let v = if val < 256 {
        BOX_REG(val as usize).opt()
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("");
        print_esc_cstr("vsplit");
        print_cstr(" needs a ");
        print_esc_cstr("vbox");
        help!(
            "The box you are trying to split is an \\hbox.",
            "I can\'t split such a box, so I\'ll leave it alone."
        );
        error();
        return None;
    }
    let q = vert_break(v.list_ptr(), h, *DIMENPAR(DimenPar::split_max_depth));
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
    let q = prune_page_top(q.opt(), *INTPAR(IntPar::saving_vdiscards) > 0).opt();
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
        *BOX_REG(val as usize) = q.tex_int();
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
            *DIMENPAR(DimenPar::split_max_depth),
        )
        .ptr(),
    )
}
pub(crate) unsafe fn print_totals() {
    print_scaled(page_so_far[1]);
    if page_so_far[2] != Scaled::ZERO {
        print_cstr(" plus ");
        print_scaled(page_so_far[2]);
        print_cstr("");
    }
    if page_so_far[3] != Scaled::ZERO {
        print_cstr(" plus ");
        print_scaled(page_so_far[3]);
        print_cstr("fil");
    }
    if page_so_far[4] != Scaled::ZERO {
        print_cstr(" plus ");
        print_scaled(page_so_far[4]);
        print_cstr("fill");
    }
    if page_so_far[5] != Scaled::ZERO {
        print_cstr(" plus ");
        print_scaled(page_so_far[5]);
        print_cstr("filll");
    }
    if page_so_far[6] != Scaled::ZERO {
        print_cstr(" minus ");
        print_scaled(page_so_far[6]);
    };
}
pub(crate) unsafe fn box_error(mut n: u8) {
    error();
    diagnostic(true, || {
        print_nl_cstr("The following box has been deleted:");
        show_box(BOX_REG(n as usize).opt());
    });
    flush_node_list(BOX_REG(n as usize).opt());
    *BOX_REG(n as usize) = None.tex_int();
}
pub(crate) unsafe fn app_space() {
    let q;
    if cur_list.aux.b32.s0 >= 2000 && *GLUEPAR(GluePar::xspace_skip) != 0 {
        q = new_param_glue(GluePar::xspace_skip)
    } else {
        let mut main_p = if *GLUEPAR(GluePar::space_skip) != 0 {
            *GLUEPAR(GluePar::space_skip) as usize
        } else {
            /*1077: */
            FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize]
                .opt()
                .unwrap_or_else(|| {
                    /*:1079 */
                    let main_p = new_spec(0);
                    main_k = PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize] + 2;
                    GlueSpec(main_p)
                        .set_size(Scaled(FONT_INFO[main_k as usize].b32.s1))
                        .set_stretch(Scaled(FONT_INFO[(main_k + 1) as usize].b32.s1))
                        .set_shrink(Scaled(FONT_INFO[(main_k + 2) as usize].b32.s1));
                    FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize] = Some(main_p).tex_int();
                    main_p
                })
        };
        let mut main_p = GlueSpec(new_spec(main_p));
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
        q = new_glue(main_p.ptr());
        main_p.rc_none();
    }
    *LLIST_link(cur_list.tail) = Some(q).tex_int();
    cur_list.tail = q;
}
pub(crate) unsafe fn insert_dollar_sign(input: &mut input_state_t, tok: i32) {
    back_input(input, tok);
    let tok = MATH_SHIFT_TOKEN + 36;
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Missing $ inserted");
    help!(
        "I\'ve inserted a begin-math/end-math symbol since I think",
        "you left one out. Proceed, with fingers crossed."
    );
    ins_error(input, tok);
}
pub(crate) unsafe fn you_cant(cmd: Cmd, chr: i32) {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("You can\'t use `");
    print_cmd_chr(cmd, chr);
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
    if cur_list.mode.0 == false {
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
        let nb = new_null_box();
        *LLIST_link(cur_list.tail) = Some(nb).tex_int();
        cur_list.tail = nb;
        MEM[cur_list.tail + 1].b32.s1 = (*DIMENPAR(DimenPar::hsize)).0;
        let g = new_glue(8);
        *LLIST_link(cur_list.tail) = Some(g).tex_int();
        cur_list.tail = g;
        let p = new_penalty(NULL_FLAG.0);
        *LLIST_link(cur_list.tail) = Some(p).tex_int();
        cur_list.tail = p;
        build_page(input);
    }
    false
}
pub(crate) unsafe fn append_glue(input: &mut input_state_t, chr: i32) {
    let s = SkipCode::n(chr as u8).unwrap();
    let val = match s {
        SkipCode::Fil => 4,
        SkipCode::Fill => 8,
        SkipCode::Ss => 12,
        SkipCode::FilNeg => 16,
        SkipCode::Skip => scan_glue(input, ValLevel::Glue),
        SkipCode::MSkip => scan_glue(input, ValLevel::Mu),
    };
    let g = new_glue(val as usize);
    *LLIST_link(cur_list.tail) = Some(g).tex_int();
    cur_list.tail = g;
    if s == SkipCode::Skip || s == SkipCode::MSkip {
        MEM[val as usize].b32.s1 -= 1;
        if s == SkipCode::MSkip {
            MEM[g].b16.s0 = MU_GLUE;
        }
    };
}
pub(crate) unsafe fn append_kern(input: &mut input_state_t, chr: i32) {
    let s = chr as u16;
    let val = scan_dimen(input, s == KernType::Math as u16, false, None);
    let k = new_kern(val);
    *LLIST_link(cur_list.tail) = Some(k).tex_int();
    cur_list.tail = k;
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Extra ");
        print_cmd_chr(cmd, chr);
        help!("Things are pretty mixed up, but I think the worst is over.");
        error();
    } else {
        back_input(input, tok);
        let mut p = get_avail();
        *LLIST_link(TEMP_HEAD) = Some(p).tex_int();
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Missing ");
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
        print_cstr(" inserted");
        begin_token_list(input, *LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
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
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Extra }, or forgotten ");
    match group {
        GroupCode::SemiSimple => print_esc_cstr("endgroup"),
        GroupCode::MathShift => print_chr('$'),
        GroupCode::MathLeft => print_esc_cstr("right"),
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
    if *INTPAR(IntPar::looseness) != 0 {
        eq_word_define(INT_BASE as usize + (IntPar::looseness as usize), 0i32);
    }
    if *DIMENPAR(DimenPar::hang_indent) != Scaled::ZERO {
        eq_word_define(DIMEN_BASE as usize + (DimenPar::hang_indent as usize), 0i32);
    }
    if *INTPAR(IntPar::hang_after) != 1 {
        eq_word_define(INT_BASE as usize + (IntPar::hang_after as usize), 1i32);
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
pub(crate) unsafe fn box_end(input: &mut input_state_t, mut box_context: i32) {
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
                if cur_list.mode.0 == false {
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
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Leaders not followed by proper glue");
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
pub(crate) unsafe fn begin_box(
    input: &mut input_state_t,
    cmd: Cmd,
    chr: i32,
    mut box_context: i32,
) {
    match BoxCode::n(chr as u8).unwrap() {
        BoxCode::Box => {
            let val = scan_register_num(input);
            cur_box = if val < 256 {
                BOX_REG(val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            if val < 256 {
                *BOX_REG(val as usize) = None.tex_int()
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
                BOX_REG(val as usize).opt()
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
                        let mut q: i32 = 0;
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
                match &mut CharOrText::from(tx as usize) {
                    CharOrText::Text(TxtNode::List(b)) => {
                        /*1116:*/
                        let mut q = cur_list.head as i32;
                        let mut p = None.tex_int();
                        let mut r: i32 = 0;
                        let mut fm: bool = false;
                        loop {
                            r = p;
                            p = q;
                            fm = false;
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
                    _ => {}
                }
            }
        }
        BoxCode::VSplit => {
            let n = scan_register_num(input);
            if !scan_keyword(input, b"to") {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing `to\' inserted");
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
                    begin_token_list(input, ev, Btl::EveryVBoxText);
                }
            } else {
                cur_list.aux.b32.s0 = 1000;
                if let Some(eh) = LOCAL(Local::every_hbox).opt() {
                    begin_token_list(input, eh, Btl::EveryHBoxText);
                }
            }
            return;
        }
    }
    box_end(input, box_context);
}
pub(crate) unsafe fn scan_box(input: &mut input_state_t, mut box_context: i32) {
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("A <box> was supposed to be here");
        help!(
            "I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
            "something like that. So you might find something missing in",
            "your output. But keep trying; you can fix this later."
        );
        back_error(input, tok);
    };
}
pub(crate) unsafe fn package(input: &mut input_state_t, c: i16) {
    let d = *DIMENPAR(DimenPar::box_max_depth);
    let u = *INTPAR(IntPar::xetex_upwards);
    unsave(input);
    SAVE_PTR -= 3;
    let v = *INTPAR(IntPar::xetex_upwards);
    *INTPAR(IntPar::xetex_upwards) = u;
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
    *INTPAR(IntPar::xetex_upwards) = v;
    pop_nest();
    box_end(input, SAVE_STACK[SAVE_PTR + 0].val);
}
pub(crate) unsafe fn norm_min(mut h: i32) -> i16 {
    (if h <= 0 {
        1
    } else if h >= 63 {
        63
    } else {
        h
    }) as i16
}
pub(crate) unsafe fn new_graf(input: &mut input_state_t, mut indented: bool) {
    cur_list.prev_graf = 0;
    if cur_list.mode == (false, ListMode::VMode) || cur_list.head != cur_list.tail {
        let pg = new_param_glue(GluePar::par_skip);
        *LLIST_link(cur_list.tail) = Some(pg).tex_int();
        cur_list.tail = pg;
    }
    push_nest();
    cur_list.mode = (false, ListMode::HMode);
    cur_list.aux.b32.s0 = 1000;
    cur_lang = if *INTPAR(IntPar::language) <= 0 {
        0
    } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
        0
    } else {
        *INTPAR(IntPar::language) as u8
    };
    cur_list.aux.b32.s1 = cur_lang as i32;
    cur_list.prev_graf = ((norm_min(*INTPAR(IntPar::left_hyphen_min)) as i32 * 64i32
        + norm_min(*INTPAR(IntPar::right_hyphen_min)) as i32) as i64
        * 65536
        + cur_lang as i64) as i32;
    if indented {
        cur_list.tail = new_null_box();
        MEM[cur_list.head].b32.s1 = cur_list.tail as i32;
        MEM[cur_list.tail + 1].b32.s1 = EQTB[DIMEN_BASE].val;
        if insert_src_special_every_par {
            insert_src_special();
        }
    }
    if let Some(ep) = LOCAL(Local::every_par).opt() {
        begin_token_list(input, ep, Btl::EveryParText);
    }
    if NEST_PTR == 1 {
        build_page(input);
    };
}
pub(crate) unsafe fn indent_in_hmode(chr: i32) {
    if chr > 0 {
        let mut p = new_null_box();
        MEM[p + 1].b32.s1 = EQTB[DIMEN_BASE].val;
        if cur_list.mode.1 == ListMode::HMode {
            cur_list.aux.b32.s0 = 1000
        } else {
            let q = new_noad();
            MEM[q + 1].b32.s1 = MathCell::SubBox as _;
            MEM[q + 1].b32.s0 = p as i32;
            p = q;
        }
        *LLIST_link(cur_list.tail) = Some(p).tex_int();
        cur_list.tail = p;
    };
}
pub(crate) unsafe fn head_for_vmode(input: &mut input_state_t, tok: i32, cmd: Cmd, chr: i32) {
    if cur_list.mode.0 == true {
        if cmd != Cmd::HRule {
            off_save(input, cur_group, tok, cmd, chr);
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("You can\'t use `");
            print_esc_cstr("hrule");
            print_cstr("\' here except with leaders");
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
        if let Some(_) = cur_list.eTeX_aux {
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("You can\'t ");
            print_esc_cstr("insert");
            print_int(255);
            help!("I\'m changing to \\insert0; box 255 is special.");
            error();
            0
        } else {
            val
        }
    };
    SAVE_STACK[SAVE_PTR + 0].val = val;
    if cmd == Cmd::VAdjust && scan_keyword(input, b"pre") {
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
    *LLIST_link(cur_list.tail) = Some(p).tex_int();
    cur_list.tail = p;
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
        if !is_char_node(Some(tx)) {
            if MEM[tx].b16.s1 as i32 == chr {
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
        }
    };
}
pub(crate) unsafe fn unpackage(input: &mut input_state_t, chr: i32) {
    let c = BoxCode::n(chr as u8).unwrap();
    match c {
        BoxCode::Box | BoxCode::Copy => {
            let val = scan_register_num(input);
            let p = if val < 256 {
                BOX_REG(val as usize).opt()
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Incompatible list can\'t be unboxed");
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
                        *BOX_REG(val as usize) = None.tex_int()
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
    let mut f: internal_font_number = 0;
    if cur_list.tail != cur_list.head {
        let p = match CharOrText::from(cur_list.tail) {
            CharOrText::Char(c) => c,
            CharOrText::Text(TxtNode::Ligature(l)) => l.as_char(),
            CharOrText::Text(TxtNode::WhatsIt(p)) => {
                match p {
                    WhatsIt::NativeWord(nw) => {
                        let mut k = Kern(new_kern(nw.italic_correction()));
                        *LLIST_link(cur_list.tail) = Some(k.ptr()).tex_int();
                        cur_list.tail = k.ptr();
                        k.set_subtype(KernType::Explicit);
                    }
                    WhatsIt::Glyph(g) => {
                        let mut k = Kern(new_kern(g.italic_correction()));
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
        f = p.font() as internal_font_number;
        let mut k = Kern(new_kern(*FONT_CHARINFO_ITALCORR(
            f,
            FONT_CHARACTER_INFO(f, effective_char(true, f, p.character()) as usize),
        )));
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
        if c >= 0 {
            if c <= BIGGEST_CHAR {
                MEM[cur_list.tail + 1].b32.s0 =
                    new_character(EQTB[CUR_FONT_LOC].val as usize, c as UTF16_code).tex_int()
            }
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Improper discretionary list");
                    help!("Discretionary lists must contain only boxes and kerns.");
                    error();
                    diagnostic(true, || {
                        print_nl_cstr("The following discretionary sublist has been deleted:");
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
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Illegal math ");
                print_esc_cstr("discretionary");
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
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Discretionary list is too long");
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
            q = new_character(f, chr as UTF16_code);
            chr
        } else if cmd == Cmd::CharNum {
            let val = scan_char_num(input);
            q = new_character(f, val as UTF16_code);
            val
        } else {
            back_input(input, tok);
            val
        };
        if let Some(q) = q {
            /*1160: */
            let mut h;
            let mut w;
            let t = FONT_INFO[(SLANT_CODE + PARAM_BASE[f]) as usize].b32.s1 as f64 / 65536.;
            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
                w = NativeWord::from(q).width();
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
            let mut r = Kern(new_kern(delta));
            r.set_subtype(KernType::AccKern);
            *LLIST_link(cur_list.tail) = Some(r.ptr()).tex_int();
            *LLIST_link(r.ptr()) = Some(p).tex_int();
            let mut k = Kern(new_kern(-a - delta));
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Misplaced ");
        print_cmd_chr(cmd, chr);
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing { inserted");
            align_state += 1;
            LEFT_BRACE_TOKEN + 123
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing } inserted");
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
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Misplaced ");
    print_esc_cstr("noalign");
    help!(
        "I expect to see \\noalign only after the \\cr of",
        "an alignment. Proceed, and I\'ll ignore this case."
    );
    error();
}
pub(crate) unsafe fn omit_error() {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Misplaced ");
    print_esc_cstr("omit");
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
        && INPUT_STACK[base_ptr].loc.opt().is_none()
        && INPUT_STACK[base_ptr].state == InputState::TokenList
    {
        base_ptr -= 1
    }
    if INPUT_STACK[base_ptr].index != Btl::VTemplate
        || INPUT_STACK[base_ptr].loc.opt().is_some()
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
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Extra ");
    print_esc_cstr("endcsname");
    help!("I\'m ignoring this, since I wasn\'t doing a \\csname.");
    error();
}
pub(crate) unsafe fn push_math(c: GroupCode) {
    push_nest();
    cur_list.mode = (true, ListMode::MMode);
    cur_list.aux.b32.s1 = None.tex_int();
    new_save_level(c);
}
pub(crate) unsafe fn just_copy(mut popt: Option<usize>, mut h: usize, mut t: i32) {
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Missing control sequence inserted");
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
pub(crate) unsafe fn trap_zero_glue(val: i32) -> i32 {
    if MEM[(val + 1) as usize].b32.s1 == 0
        && MEM[(val + 2) as usize].b32.s1 == 0
        && MEM[(val + 3) as usize].b32.s1 == 0
    {
        GlueSpec(0).rc_inc(); // TODO: check
        delete_glue_ref(val as usize);
        0
    } else {
        val
    }
}
pub(crate) unsafe fn do_register_command(
    input: &mut input_state_t,
    ocmd: Cmd,
    mut ochr: i32,
    mut a: i16,
) {
    let mut l: i32 = None.tex_int();
    let mut p = ValLevel::Int;
    let mut q = ocmd;
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("You can\'t use `");
                    print_cmd_chr(cmd, chr);
                    print_cstr("\' after ");
                    print_cmd_chr(q, 0);
                    help!("I\'m forgetting what you said and not changing anything.");
                    error();
                    return;
                }
            }
        }
    }

    if flag {
        if ochr < 0 || ochr > 19 {
            /*lo_mem_stat_max*/
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
        scan_keyword(input, b"by");
    }
    arith_error = false;
    let mut val = if q < Cmd::Multiply {
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
                    let mut q = GlueSpec(new_spec(val as usize));
                    let r = GlueSpec(s as usize);
                    delete_glue_ref(val as usize);
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
                    val
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
                let mut r = GlueSpec(new_spec(s as usize));
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Arithmetic overflow");
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
        let val = trap_zero_glue(val);
        if e {
            if a >= 4 {
                gsa_def(l as usize, val.opt());
            } else {
                sa_def(l as usize, val.opt());
            }
        } else if a >= 4 {
            geq_define(l as usize, Cmd::GlueRef, val.opt());
        } else {
            eq_define(l as usize, Cmd::GlueRef, val.opt());
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
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Bad space factor");
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
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad ");
        print_esc_cstr("prevgraf");
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
    let mut c: i16 = 0;
    c = chr as i16;
    scan_optional_equals(input);
    let val = scan_int(input);
    if c == 0 {
        dead_cycles = val
    } else if c == 2 {
        if InteractionMode::n(val as u8).is_none() {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Bad interaction mode");
            help!(
                "Modes are 0=batch, 1=nonstop, 2=scroll, and",
                "3=errorstop. Proceed, and I\'ll ignore this case."
            );
            int_error(val);
        } else {
            new_interaction(val);
        }
    } else {
        insert_penalties = val
    };
}
pub(crate) unsafe fn alter_box_dimen(input: &mut input_state_t, chr: i32) {
    let c = chr as usize;
    let val = scan_register_num(input);
    let b = if val < 256 {
        BOX_REG(val as usize).opt()
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
pub(crate) unsafe fn new_font(input: &mut input_state_t, mut a: i16) {
    let mut s: Scaled = Scaled::ZERO;
    if job_name == 0 {
        open_log_file();
    }
    let u = get_r_token(input).3 as usize;
    let mut t = if u >= HASH_BASE {
        (*hash.offset(u as isize)).s1
    } else if u >= SINGLE_BASE {
        if u == NULL_CS {
            maketexstring("FONT")
        } else {
            (u - SINGLE_BASE) as i32
        }
    } else {
        let old_setting = selector;
        selector = Selector::NEW_STRING;
        print_cstr("FONT");
        print((u - 1) as i32);
        selector = old_setting;
        if pool_ptr + 1 > pool_size {
            overflow("pool size", (pool_size - init_pool_ptr) as usize);
        }
        make_string()
    };
    if a >= 4 {
        geq_define(u, Cmd::SetFont, Some(FONT_BASE));
    } else {
        eq_define(u, Cmd::SetFont, Some(FONT_BASE));
    }
    scan_optional_equals(input);
    let (quoted_filename, file_name_quote_char) = scan_file_name(input);
    name_in_progress = true;
    if scan_keyword(input, b"at") {
        /*1294: */
        s = scan_dimen(input, false, false, None); /*:1293 */
        /*:79 */
        if s <= Scaled::ZERO || s >= Scaled::from(2048) {
            if file_line_error_style_p != 0 {
                print_file_line(); /*1318: */
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Improper `at\' size (");
            print_scaled(s);
            print_cstr("pt), replaced by 10pt");
            help!(
                "I can only handle fonts at positive sizes that are",
                "less than 2048pt, so I\'ve changed what you said to 10pt."
            );
            error();
            s = Scaled::from(10);
        }
    } else if scan_keyword(input, b"scaled") {
        let val = scan_int(input);
        s = Scaled(-val);
        if val <= 0 || val > 32768 {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Illegal magnification has been changed to 1000");
            help!("The magnification ratio must be between 1 and 32768.");
            int_error(val);
            s = Scaled(-1000);
        }
    } else {
        s = Scaled(-1000);
    }
    name_in_progress = false;

    for f in (FONT_BASE + 1)..=FONT_PTR {
        // TODO: check
        if PoolString::from(FONT_NAME[f]) == PoolString::from(cur_name)
            && ((length(cur_area) == 0 && matches!(&FONT_LAYOUT_ENGINE[f], Font::Native(_)))
                || PoolString::from(FONT_AREA[f]) == PoolString::from(cur_area))
        {
            if s > Scaled::ZERO {
                if s == FONT_SIZE[f] {
                    return common_ending(a, u, f, t);
                }
            } else if FONT_SIZE[f] == xn_over_d(FONT_DSIZE[f], -s, 1000).0 {
                return common_ending(a, u, f, t);
            }
        }
        append_str(cur_area);
        append_str(cur_name);
        append_str(cur_ext);
        if PoolString::from(FONT_NAME[f]) == PoolString::from(make_string()) {
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
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
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
        }
    }

    unsafe fn common_ending(a: i16, u: usize, f: usize, t: i32) {
        if a >= 4 {
            geq_define(u, Cmd::SetFont, Some(f));
        } else {
            eq_define(u, Cmd::SetFont, Some(f));
        }
        EQTB[(FROZEN_NULL_FONT + f) as usize] = EQTB[u];
        (*hash.offset((FROZEN_NULL_FONT + f) as isize)).s1 = t;
    }

    let f = crate::tfm::read_font_info(
        u as i32,
        cur_name,
        cur_area,
        s,
        quoted_filename,
        file_name_quote_char,
    )
    .map(crate::tfm::good_tfm)
    .unwrap_or_else(|e| {
        crate::tfm::bad_tfm(e, u as i32, cur_name, cur_area, s, file_name_quote_char);
        FONT_BASE
    });
    common_ending(a, u, f, t)
}
pub(crate) unsafe fn new_interaction(chr: i32) {
    print_ln();
    interaction = InteractionMode::n(chr as u8).unwrap();
    selector = if interaction == InteractionMode::Batch {
        Selector::NO_PRINT
    } else {
        Selector::TERM_ONLY
    };
    if log_opened {
        selector = (u8::from(selector)).wrapping_add(2).into()
    };
}
pub(crate) unsafe fn issue_message(input: &mut input_state_t, chr: i32, cs: i32) {
    let mut c: u8 = 0;
    let mut s: str_number = 0;
    c = chr as u8;
    *LLIST_link(GARBAGE) = scan_toks(input, cs, false, true) as i32;
    let old_setting_0 = selector;
    selector = Selector::NEW_STRING;
    token_show(Some(def_ref));
    selector = old_setting_0;
    flush_list(Some(def_ref));
    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    s = make_string();
    if c == 0 {
        /*1315: */
        if term_offset + length(s) > max_print_line - 2 {
            print_ln();
        } else if term_offset > 0 || file_offset > 0 {
            print_chr(' ');
        }
        print(s);
        rust_stdout.as_mut().unwrap().flush().unwrap();
    } else {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("");
        print(s);
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
    str_ptr -= 1;
    pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
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
    begin_token_list(input, *LLIST_link(def_ref) as usize, Btl::BackedUp);
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
                BOX_REG(val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            diagnostic(true, || {
                print_nl_cstr("> \\box");
                print_int(val);
                print_chr('=');
                if p.is_none() {
                    print_cstr("void");
                } else {
                    show_box(p);
                }
            });
        }
        SHOW_CODE => {
            let (_, cmd, chr, cs) = get_token(input);
            print_nl_cstr("> ");
            if cs != 0 {
                sprint_cs(cs);
                print_chr('=');
            }
            print_meaning(cmd, chr);
            return common_ending();
        }
        SHOW_GROUPS => {
            diagnostic(true, || show_save_groups(cur_group, cur_level));
        }
        SHOW_IFS => {
            diagnostic(true, || {
                print_nl_cstr("");
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
                        print_nl_cstr("### level ");
                        print_int(n);
                        print_cstr(": ");
                        print_cmd_chr(Cmd::IfTest, t as i32);
                        if m == FiOrElseCode::Fi {
                            print_esc_cstr("else");
                        }
                        if l != 0i32 {
                            print_cstr(" entered on line ");
                            print_int(l);
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
                    print_nl_cstr("### ");
                    print_cstr("no active conditionals");
                }
            });
        }
        SHOW_THE_CODE | SHOW_TOKENS => {
            let _p = the_toks(input, chr, cs) as i32;
            print_nl_cstr("> ");
            token_show(Some(TEMP_HEAD));
            flush_list(llist_link(TEMP_HEAD));
            return common_ending();
        }
        _ => unreachable!(),
    }

    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("OK");
    if selector == Selector::TERM_AND_LOG {
        if *INTPAR(IntPar::tracing_online) <= 0i32 {
            selector = Selector::TERM_ONLY;
            print_cstr(" (see the transcript file)");
            selector = Selector::TERM_AND_LOG
        }
    }

    unsafe fn common_ending() {
        if interaction != InteractionMode::ErrorStop {
            help!();
            error_count -= 1;
        } else if *INTPAR(IntPar::tracing_online) > 0 {
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
pub(crate) unsafe fn scan_and_pack_name(input: &mut input_state_t) {
    scan_file_name(input);
    pack_file_name(cur_name, cur_area, cur_ext);
}
pub(crate) unsafe fn do_extension(
    input: &mut input_state_t,
    tok: i32,
    cmd: Cmd,
    chr: i32,
    cs: i32,
) {
    let mut j: i32 = 0;
    let mut p: usize = 0;
    match chr as u16 {
        0 => {
            // OpenFile
            let mut o = OpenFile::new_node();
            *LLIST_link(cur_list.tail) = Some(o.ptr()).tex_int();
            cur_list.tail = o.ptr();
            let val = scan_four_bit_int(input);
            o.set_id(val);
            scan_optional_equals(input);
            scan_file_name(input);
            o.set_name(cur_name).set_area(cur_area).set_ext(cur_ext);
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
            p = scan_toks(input, k, false, false);
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
            p = scan_toks(input, cs, false, true);
            s.set_tokens(def_ref as i32);
        }
        IMMEDIATE_CODE => {
            let (tok, cmd, chr, cs) = get_x_token(input);
            if cmd == Cmd::Extension && chr <= WhatsItNST::Close as i32 {
                p = cur_list.tail;
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
                cur_list.aux.b32.s1 = if val <= 0 {
                    0
                } else if val > 255 {
                    0
                } else {
                    val
                };
                l.set_lang(cur_list.aux.b32.s1)
                    .set_lhm(norm_min(*INTPAR(IntPar::left_hyphen_min)) as u16)
                    .set_rhm(norm_min(*INTPAR(IntPar::right_hyphen_min)) as u16);
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
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Bad glyph number");
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
                g.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
            } else {
                not_native_font_error(
                    Cmd::Extension,
                    GLYPH_CODE as i32,
                    EQTB[CUR_FONT_LOC].val as usize,
                );
            }
        }
        XETEX_INPUT_ENCODING_EXTENSION_CODE => {
            scan_and_pack_name(input);
            let i = get_encoding_mode_and_info(&mut j);
            if i == UnicodeMode::Auto {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Encoding mode `auto\' is not valid for \\XeTeXinputencoding");
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
            scan_and_pack_name(input);
            let i = get_encoding_mode_and_info(&mut j);
            *INTPAR(IntPar::xetex_default_input_mode) = i as i32;
            *INTPAR(IntPar::xetex_default_input_encoding) = j
        }
        XETEX_LINEBREAK_LOCALE_EXTENSION_CODE => {
            scan_file_name(input);
            if length(cur_name) == 0 {
                *INTPAR(IntPar::xetex_linebreak_locale) = 0;
            } else {
                *INTPAR(IntPar::xetex_linebreak_locale) = cur_name;
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
    let mut l: UTF16_code = if *INTPAR(IntPar::language) <= 0 {
        0
    } else if *INTPAR(IntPar::language) > 255 {
        0
    } else {
        *INTPAR(IntPar::language) as UTF16_code
    };
    if l as i32 != cur_list.aux.b32.s1 {
        let mut lang = Language::new_node();
        *LLIST_link(cur_list.tail) = Some(lang.ptr()).tex_int();
        cur_list.tail = lang.ptr();
        lang.set_lang(l as i32);
        cur_list.aux.b32.s1 = l as i32;
        lang.set_lhm(norm_min(*INTPAR(IntPar::left_hyphen_min)) as u16)
            .set_rhm(norm_min(*INTPAR(IntPar::right_hyphen_min)) as u16);
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
        let q = str_toks(make_src_special(SOURCE_FILENAME_STACK[IN_OPEN], line)) as usize;
        *LLIST_link(p) = *LLIST_link(TEMP_HEAD);
        let p = q;
        *LLIST_link(p) = Some(get_avail()).tex_int();
        let p = *LLIST_link(p) as usize;
        MEM[p].b32.s0 = RIGHT_BRACE_TOKEN + '}' as i32;
        begin_token_list(&mut cur_input, toklist, Btl::Inserted);
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
        str_toks(make_src_special(SOURCE_FILENAME_STACK[IN_OPEN], line));
        *LLIST_link(def_ref) = *LLIST_link(TEMP_HEAD);
        MEM[cur_list.tail + 1].b32.s1 = def_ref as i32;
        remember_source_info(SOURCE_FILENAME_STACK[IN_OPEN], line);
    };
}
pub(crate) unsafe fn handle_right_brace(input: &mut input_state_t, group: GroupCode, tok: i32) {
    match group {
        GroupCode::Simple => unsave(input),
        GroupCode::BottomLevel => {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Too many }\'s");
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
            let q = *GLUEPAR(GluePar::split_top_skip) as usize;
            GlueSpec(q).rc_inc();
            let d = *DIMENPAR(DimenPar::split_max_depth);
            let f = *INTPAR(IntPar::floating_penalty);
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
                    .set_split_top_ptr(Some(q).tex_int())
                    .set_depth(d)
                    .set_float_cost(f);
            } else {
                let a = Adjust::new_node();
                *LLIST_link(cur_list.tail) = Some(a.ptr()).tex_int();
                cur_list.tail = a.ptr();
                MEM[cur_list.tail].b16.s0 = SAVE_STACK[SAVE_PTR + 1].val as u16;
                MEM[cur_list.tail + 1].b32.s1 = p.list_ptr();
                delete_glue_ref(q);
            }
            free_node(p.ptr(), BOX_NODE_SIZE);
            if NEST_PTR == 0 {
                build_page(input);
            }
        }
        GroupCode::Output => {
            /*1062:*/
            if input.loc.opt().is_some()
                || input.index != Btl::OutputText && input.index != Btl::BackedUp
            {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Unbalanced output routine");
                help!(
                    "Your sneaky output routine has problematic {\'s and/or }\'s.",
                    "I can\'t handle that very well; good luck."
                );
                error();
                loop {
                    let _ = get_token(input);
                    if input.loc.opt().is_none() {
                        break;
                    }
                }
            }

            end_token_list(input);
            end_graf();
            unsave(input);
            output_active = false;
            insert_penalties = 0;

            if BOX_REG(255).opt().is_some() {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Output routine didn\'t use all of ");
                print_esc_cstr("box");
                print_int(255);
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
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing ");
            print_esc_cstr("cr");
            print_cstr(" inserted");
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
            let mut p = fin_mlist(None);
            MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32.s0 = p;
            if let Some(p) = p.opt() {
                if llist_link(p).is_none() {
                    match Node::from(p) {
                        Node::Math(MathNode::Ord) => {
                            if MEM[p + 3].b32.s1 == MathCell::Empty as _ {
                                if MEM[p + 2].b32.s1 == MathCell::Empty as _ {
                                    MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32 = MEM[p + 1].b32;
                                    free_node(p, NOAD_SIZE);
                                }
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
        begin_token_list(input, ej, Btl::EveryJobText);
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
        if *INTPAR(IntPar::tracing_commands) > 0i32 {
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
                if cur_list.mode.1 == ListMode::HMode {
                    if *INTPAR(IntPar::xetex_inter_char_tokens) > 0
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
                            begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                            continue 'big_switch;
                        }
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
                                prim_lookup((*hash.offset(cs as isize)).s1) as i32
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
                        if cur_list.mode.0 == false {
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
                        *LLIST_link(cur_list.tail) = Some(k).tex_int();
                        cur_list.tail = k;
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
                            if eTeX_enabled(*INTPAR(IntPar::texxet) > 0, cur_cmd, cur_chr) {
                                let m = new_math(Scaled::ZERO, MathType::from(cur_chr as u16));
                                *LLIST_link(cur_list.tail) = Some(m).tex_int();
                                cur_list.tail = m;
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
                        let m = BaseMath(cur_list.tail);
                        scan_math(input, m.first_mut(), cur_list.tail + 1);
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
                            let mut val = scan_delimiter_int(input);
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
                        let m = BaseMath(cur_list.tail);
                        scan_math(input, m.first_mut(), cur_list.tail + 1);
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
                            begin_token_list(input, ev, Btl::EveryVBoxText);
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
                        let g = new_glue(0);
                        *LLIST_link(cur_list.tail) = Some(g).tex_int();
                        cur_list.tail = g;
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
                    (_, Cmd::Relax)
                    | (VMode, Cmd::Spacer)
                    | (MMode, Cmd::Spacer)
                    | (MMode, Cmd::NoBoundary)
                    | _ => {
                        // 1 | 104 | 207 | 11 | 217 | 272 | _
                    }
                }
                continue 'big_switch;
            }
        }

        /*main_loop *//*1069: */
        if cur_list.head == cur_list.tail && cur_list.mode.0 == false {
            if insert_src_special_auto {
                append_src_special();
            }
        }
        prev_class = CHAR_CLASS_LIMIT - 1;
        if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[EQTB[CUR_FONT_LOC].val as usize] {
            if cur_list.mode.0 == false {
                if *INTPAR(IntPar::language) != cur_list.aux.b32.s1 {
                    fix_language();
                }
            }
            main_h = 0;
            main_f = EQTB[CUR_FONT_LOC].val as usize;
            native_len = 0;
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
                if *INTPAR(IntPar::xetex_inter_char_tokens) > 0 && space_class != 4096 {
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
                                    MEM[c + 1].b32.s1 as usize,
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
                            begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                            prev_class = CHAR_CLASS_LIMIT - 1;
                            break false;
                        }
                    }
                    prev_class = space_class
                }
                if cur_chr as i64 > 65535 {
                    while native_text_size <= native_len + 2 {
                        native_text_size = native_text_size + 128;
                        native_text = xrealloc(
                            native_text as *mut libc::c_void,
                            (native_text_size as u64)
                                .wrapping_mul(::std::mem::size_of::<UTF16_code>() as u64)
                                as _,
                        ) as *mut UTF16_code
                    }
                    *native_text.offset(native_len as isize) =
                        ((cur_chr as i64 - 65536) / 1024 + 0xd800) as UTF16_code;
                    native_len += 1;
                    *native_text.offset(native_len as isize) =
                        ((cur_chr as i64 - 65536) % 1024 + 0xdc00) as UTF16_code;
                    native_len += 1
                } else {
                    while native_text_size <= native_len + 1 {
                        native_text_size = native_text_size + 128;
                        native_text = xrealloc(
                            native_text as *mut libc::c_void,
                            (native_text_size as u64)
                                .wrapping_mul(::std::mem::size_of::<UTF16_code>() as u64)
                                as _,
                        ) as *mut UTF16_code
                    }
                    *native_text.offset(native_len as isize) = cur_chr as UTF16_code;
                    native_len += 1
                }
                is_hyph = cur_chr == HYPHEN_CHAR[main_f as usize]
                    || *INTPAR(IntPar::xetex_dash_break) > 0
                        && (cur_chr == 8212 || cur_chr == 8211);
                if main_h == 0 && is_hyph {
                    main_h = native_len
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
                } else if *INTPAR(IntPar::xetex_inter_char_tokens) > 0
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
                    begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                }
            }
            /*collected */
            if !(FONT_MAPPING[main_f as usize]).is_null() {
                main_k = apply_mapping(FONT_MAPPING[main_f as usize], native_text, native_len);
                native_len = 0;
                while native_text_size <= native_len + main_k {
                    native_text_size = native_text_size + 128;
                    native_text = xrealloc(
                        native_text as *mut libc::c_void,
                        (native_text_size as u64)
                            .wrapping_mul(::std::mem::size_of::<UTF16_code>() as u64)
                            as _,
                    ) as *mut UTF16_code
                }
                main_h = 0;
                for main_p in 0..main_k {
                    *native_text.offset(native_len as isize) = *mapped_text.offset(main_p as isize);
                    native_len += 1;
                    if main_h == 0
                        && (*mapped_text.offset(main_p as isize) as i32
                            == HYPHEN_CHAR[main_f as usize]
                            || *INTPAR(IntPar::xetex_dash_break) > 0
                                && (*mapped_text.offset(main_p as isize) == 8212
                                    || *mapped_text.offset(main_p as isize) == 8211))
                    {
                        main_h = native_len
                    }
                }
            }
            if *INTPAR(IntPar::tracing_lost_chars) > 0 {
                let mut tmp_ptr = 0;
                while tmp_ptr < native_len as usize {
                    main_k = *native_text.offset(tmp_ptr as isize) as font_index;
                    tmp_ptr += 1;
                    if main_k >= 0xd800 && main_k < 0xdc00 {
                        main_k = (65536 + ((main_k - 0xd800) * 1024) as i64) as font_index;
                        main_k = main_k + *native_text.offset(tmp_ptr as isize) as i32 - 0xdc00;
                        tmp_ptr += 1;
                    }
                    if map_char_to_glyph(nf, main_k) == 0 {
                        char_warning(main_f, main_k);
                    }
                }
            }
            main_k = native_len;
            let mut main_pp = cur_list.tail;
            if cur_list.mode == (false, ListMode::HMode) {
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
                            while native_text_size <= native_len + main_k {
                                native_text_size = native_text_size + 128;
                                native_text = xrealloc(
                                    native_text as *mut libc::c_void,
                                    (native_text_size as u64).wrapping_mul(::std::mem::size_of::<
                                        UTF16_code,
                                    >(
                                    )
                                        as u64) as _,
                                ) as *mut UTF16_code
                            }
                            save_native_len = native_len;
                            for c in native_pp.text() {
                                *native_text.offset(native_len as isize) = *c;
                                native_len += 1;
                            }
                            for main_p in 0..main_h {
                                *native_text.offset(native_len as isize) =
                                    *native_text.offset((tmp_ptr as isize) + (main_p as isize));
                                native_len += 1;
                            }
                            do_locale_linebreaks(save_native_len, main_k);
                            native_len = save_native_len;
                            main_k = native_len - main_h - (tmp_ptr as i32);
                            tmp_ptr = main_h as usize;
                            main_h = 0;
                            while main_h < main_k
                                && *native_text.offset((tmp_ptr as isize) + (main_h as isize))
                                    as i32
                                    != HYPHEN_CHAR[main_f as usize]
                                && (!(*INTPAR(IntPar::xetex_dash_break) > 0)
                                    || *native_text.offset((tmp_ptr as isize) + (main_h as isize))
                                        as i32
                                        != 8212
                                        && *native_text
                                            .offset((tmp_ptr as isize) + (main_h as isize))
                                            as i32
                                            != 8211)
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
                            do_locale_linebreaks(tmp_ptr as i32, main_h);
                            tmp_ptr = tmp_ptr + main_h as usize;
                            main_k = main_k - main_h;
                            main_h = 0;
                            while main_h < main_k
                                && *native_text.offset((tmp_ptr as isize) + (main_h as isize))
                                    as i32
                                    != HYPHEN_CHAR[main_f as usize]
                                && (!(*INTPAR(IntPar::xetex_dash_break) > 0)
                                    || *native_text.offset((tmp_ptr as isize) + (main_h as isize))
                                        as i32
                                        != 8212
                                        && *native_text
                                            .offset((tmp_ptr as isize) + (main_h as isize))
                                            as i32
                                            != 8211)
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

                        let slice = std::slice::from_raw_parts(native_text, main_k as usize);
                        tail_text[text.len()..].copy_from_slice(slice);

                        nwn.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
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

                        let slice = std::slice::from_raw_parts(native_text, main_k as usize);
                        nwn.text_mut().copy_from_slice(slice);

                        nwn.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                    }
                }
            }
            if *INTPAR(IntPar::xetex_interword_space_shaping) > 0 {
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
                                main_ppp = llist_link(main_ppp).unwrap();
                            }
                            if main_ppp == cur_list.tail {
                                let pp_text = native_pp.text();
                                let mut native_tail = NativeWord::from(cur_list.tail);
                                let tail_text = native_tail.text();
                                main_k = pp_text.len() as i32 + 1 + tail_text.len() as i32;
                                let mut tmp_ptr = new_native_word_node(main_f, main_k);
                                let mut temp_text = tmp_ptr.text_mut();
                                temp_text[..pp_text.len()].copy_from_slice(&pp_text);
                                temp_text[pp_text.len()] = ' ' as u16;
                                temp_text[pp_text.len() + 1..].copy_from_slice(&tail_text);

                                tmp_ptr.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                                let t = tmp_ptr.width() - native_pp.width() - native_tail.width();
                                tmp_ptr.free();
                                if t != Scaled(
                                    MEM[(FONT_GLUE[main_f as usize] + 1) as usize].b32.s1,
                                ) {
                                    let mut tmp_ptr = Kern(new_kern(
                                        t - Scaled(
                                            MEM[(FONT_GLUE[main_f as usize] + 1) as usize].b32.s1,
                                        ),
                                    ));
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
        if *INTPAR(IntPar::xetex_inter_char_tokens) > 0 && space_class != CHAR_CLASS_LIMIT {
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
                        begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                    begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                    prev_class = CHAR_CLASS_LIMIT - 1;
                    continue 'big_switch;
                }
            }
            prev_class = space_class
        }
        main_f = EQTB[CUR_FONT_LOC].val as usize;
        bchar = FONT_BCHAR[main_f as usize];
        false_bchar = FONT_FALSE_BCHAR[main_f as usize];
        if cur_list.mode.0 == false {
            if *INTPAR(IntPar::language) != cur_list.aux.b32.s1 {
                fix_language();
            }
        }
        lig_stack = avail;
        let ls = if let Some(ls) = lig_stack {
            avail = llist_link(ls);
            *LLIST_link(ls) = None.tex_int();
            ls
        } else {
            get_avail()
        };
        lig_stack = Some(ls);
        MEM[ls].b16.s1 = main_f as u16;
        cur_l = cur_chr;
        MEM[ls].b16.s0 = cur_l as u16;
        cur_q = cur_list.tail as i32;

        let mut current_block: u64;
        if cancel_boundary {
            cancel_boundary = false;
            main_k = NON_ADDRESS;
        } else {
            main_k = BCHAR_LABEL[main_f as usize]
        }
        if main_k == NON_ADDRESS {
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
                    if effective_char(false, main_f, cur_chr as u16)
                        > FONT_EC[main_f as usize] as i32
                        || effective_char(false, main_f, cur_chr as u16)
                            < FONT_BC[main_f as usize] as i32
                    {
                        char_warning(main_f, cur_chr);
                        *LLIST_link(ls) = avail.tex_int();
                        avail = lig_stack;
                        continue 'big_switch;
                    } else {
                        main_i = effective_char_info(main_f, cur_l as u16);
                        if !(main_i.s3 > 0) {
                            char_warning(main_f, cur_chr);
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
                                        if LLIST_link(cur_q as usize).opt().is_some() {
                                            if MEM[cur_list.tail].b16.s0 as i32
                                                == HYPHEN_CHAR[main_f as usize]
                                            {
                                                ins_disc = true
                                            }
                                        }
                                        if ligature_present {
                                            let mut main_p = Ligature(new_ligature(
                                                main_f,
                                                cur_l as u16,
                                                *LLIST_link(cur_q as usize),
                                            ));
                                            main_p.set_hits(lft_hit, rt_hit && lig_stack.is_some());
                                            lft_hit = false;
                                            if rt_hit && lig_stack.is_some() {
                                                rt_hit = false;
                                            }
                                            *LLIST_link(cur_q as usize) =
                                                Some(main_p.ptr()).tex_int();
                                            cur_list.tail = main_p.ptr();
                                            ligature_present = false
                                        }
                                        if ins_disc {
                                            ins_disc = false;
                                            if cur_list.mode.0 == false {
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
                                                if LLIST_link(cur_q as usize).opt().is_some() {
                                                    if MEM[cur_list.tail].b16.s0 as i32
                                                        == HYPHEN_CHAR[main_f as usize]
                                                    {
                                                        ins_disc = true
                                                    }
                                                }
                                                if ligature_present {
                                                    let main_p = new_ligature(
                                                        main_f,
                                                        cur_l as u16,
                                                        *LLIST_link(cur_q as usize),
                                                    );
                                                    if lft_hit {
                                                        MEM[main_p].b16.s0 = 2;
                                                        lft_hit = false
                                                    }
                                                    *LLIST_link(cur_q as usize) =
                                                        Some(main_p).tex_int();
                                                    cur_list.tail = main_p;
                                                    ligature_present = false
                                                }
                                                if ins_disc {
                                                    ins_disc = false;
                                                    if cur_list.mode.0 == false {
                                                        let d = new_disc();
                                                        *LLIST_link(cur_list.tail) =
                                                            Some(d).tex_int();
                                                        cur_list.tail = d;
                                                    }
                                                }
                                            }
                                            cur_q = cur_list.tail as i32;
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
                                } else if !(main_j.s3 >= 128) {
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
                            if *INTPAR(IntPar::xetex_inter_char_tokens) > 0
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
                                                MEM[c + 1].b32.s1 as usize,
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
                                            MEM[c + 1].b32.s1 as usize,
                                            Btl::InterCharText,
                                        );
                                        prev_class = CHAR_CLASS_LIMIT - 1;
                                        continue 'big_switch;
                                    }
                                }
                                prev_class = space_class
                            }
                            lig_stack = avail;
                            let ls = if let Some(ls) = lig_stack {
                                avail = llist_link(ls);
                                *LLIST_link(ls) = None.tex_int();
                                ls
                            } else {
                                get_avail()
                            };
                            lig_stack = Some(ls);
                            MEM[ls].b16.s1 = main_f as u16;
                            cur_r = cur_chr;
                            MEM[ls].b16.s0 = cur_r as u16;
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
                                if LLIST_link(cur_q as usize).opt().is_some() {
                                    if MEM[cur_list.tail].b16.s0 as i32
                                        == HYPHEN_CHAR[main_f as usize]
                                    {
                                        ins_disc = true
                                    }
                                }
                                if ligature_present {
                                    let main_p = new_ligature(
                                        main_f,
                                        cur_l as u16,
                                        *LLIST_link(cur_q as usize) as i32,
                                    );
                                    if lft_hit {
                                        MEM[main_p].b16.s0 = 2;
                                        lft_hit = false
                                    }
                                    if rt_hit {
                                        if lig_stack.is_none() {
                                            MEM[main_p].b16.s0 += 1;
                                            rt_hit = false
                                        }
                                    }
                                    *LLIST_link(cur_q as usize) = Some(main_p).tex_int();
                                    cur_list.tail = main_p;
                                    ligature_present = false
                                }
                                if ins_disc {
                                    ins_disc = false;
                                    if cur_list.mode.0 == false {
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
                            cur_q = cur_list.tail as i32;
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
        if *INTPAR(IntPar::xetex_inter_char_tokens) > 0
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
                begin_token_list(input, MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                return;
            }
        }
        let tmp_ptr = if *GLUEPAR(GluePar::space_skip) == 0 {
            let mut main_p = FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize]
                .opt()
                .unwrap_or_else(|| {
                    let main_p = new_spec(0);
                    main_k = PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize] + 2;
                    GlueSpec(main_p)
                        .set_size(Scaled(FONT_INFO[main_k as usize].b32.s1))
                        .set_stretch(Scaled(FONT_INFO[(main_k + 1) as usize].b32.s1))
                        .set_shrink(Scaled(FONT_INFO[(main_k + 2) as usize].b32.s1));
                    FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize] = Some(main_p).tex_int();
                    main_p
                });
            new_glue(main_p as usize)
        } else {
            new_param_glue(GluePar::space_skip)
        };
        *LLIST_link(cur_list.tail) = Some(tmp_ptr).tex_int();
        cur_list.tail = tmp_ptr;
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
        ttstub_output_putc(log_file.as_mut().unwrap(), '\n' as i32);
        ttstub_output_close(log_file.take().unwrap());
        log_file = None;
        selector = u8::from(selector).wrapping_sub(2).into();
        if selector == Selector::TERM_ONLY {
            print_nl_cstr("Transcript written on ");
            print(texmf_log_name);
            print_chr('.');
        }
    }
    print_ln();
}
pub(crate) unsafe fn flush_str(mut s: str_number) {
    if s == str_ptr - 1 {
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
    };
}
pub(crate) unsafe fn tokens_to_string(mut p: i32) -> str_number {
    if selector == Selector::NEW_STRING {
        pdf_error(
            "tokens",
            "tokens_to_string() called while selector = new_string",
        );
    }
    let old_setting = selector;
    selector = Selector::NEW_STRING;
    show_token_list(LLIST_link(p as usize).opt(), None, pool_size - pool_ptr);
    selector = old_setting;
    make_string()
}
pub(crate) unsafe fn scan_pdf_ext_toks(input: &mut input_state_t, cs: i32) {
    scan_toks(input, cs, false, true);
}
pub(crate) unsafe fn compare_strings(input: &mut input_state_t, cs: i32) -> i32 {
    unsafe fn done(s1: str_number, s2: str_number, val: i32) -> i32 {
        flush_str(s2);
        flush_str(s1);
        //cur_val_level = ValLevel::Int;
        val
    }
    scan_toks(input, cs, false, true);
    let s1 = tokens_to_string(def_ref as i32);
    delete_token_ref(def_ref);
    scan_toks(input, cs, false, true);
    let s2 = tokens_to_string(def_ref as i32);
    delete_token_ref(def_ref);
    let mut i1 = str_start[(s1 as i64 - 65536) as usize];
    let j1 = str_start[((s1 + 1i32) as i64 - 65536) as usize];
    let mut i2 = str_start[(s2 as i64 - 65536) as usize];
    let j2 = str_start[((s2 + 1i32) as i64 - 65536) as usize];
    loop {
        if !(i1 < j1 && i2 < j2) {
            break;
        }
        if (str_pool[i1 as usize] as i32) < str_pool[i2 as usize] as i32 {
            return done(s1, s2, -1);
        } else if str_pool[i1 as usize] as i32 > str_pool[i2 as usize] as i32 {
            return done(s1, s2, 1);
        } else {
            i1 += 1;
            i2 += 1
        }
    }
    let val = if i1 == j1 && i2 == j2 {
        0
    } else if i1 < j1 {
        1
    } else {
        -1
    };
    done(s1, s2, val)
}
pub(crate) unsafe fn prune_page_top(mut popt: Option<usize>, mut s: bool) -> i32 {
    let mut r: i32 = None.tex_int();
    let mut prev_p = TEMP_HEAD;
    *LLIST_link(TEMP_HEAD) = popt.tex_int();
    while let Some(p) = popt {
        match TxtNode::from(p) {
            TxtNode::List(b) => {
                let (q, mut tmp_ptr) = new_skip_param(GluePar::split_top_skip);
                *LLIST_link(prev_p) = Some(q).tex_int();
                *LLIST_link(q) = Some(b.ptr()).tex_int();
                if tmp_ptr.size() > b.height() {
                    tmp_ptr.set_size(tmp_ptr.size() - b.height());
                } else {
                    tmp_ptr.set_size(Scaled::ZERO);
                }
                popt = None;
            }
            TxtNode::Rule(r) => {
                let (q, mut tmp_ptr) = new_skip_param(GluePar::split_top_skip);
                *LLIST_link(prev_p) = Some(q).tex_int();
                *LLIST_link(q) = Some(r.ptr()).tex_int();
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
pub(crate) unsafe fn do_marks(a: MarkMode, mut l: i16, q: usize) -> bool {
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
