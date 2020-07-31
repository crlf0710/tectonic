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

use super::xetex_ini::{EqtbWord, Selector};
use super::xetex_io::{
    bytesFromUTF8, name_of_input_file, offsetsFromUTF8, tt_xetex_open_input, u_open_in,
};
use crate::core_memory::{mfree, xmalloc_array, xrealloc};
use crate::help;
#[cfg(target_os = "macos")]
use crate::xetex_aatfont as aat;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, fatal_error, overflow, pdf_error, Confuse};
use crate::xetex_ext::{
    apply_mapping, apply_tfm_font_mapping, check_for_tfm_font_mapping, find_native_font,
    get_encoding_mode_and_info, get_font_char_range, get_glyph_bounds,
    get_native_char_height_depth, get_native_char_sidebearings, getnativechardp, getnativecharht,
    getnativecharic, getnativecharwd, gr_font_get_named, gr_font_get_named_1, gr_print_font_name,
    linebreak_next, linebreak_start, load_tfm_font_mapping, map_char_to_glyph, map_glyph_to_index,
    ot_font_get, ot_font_get_1, ot_font_get_2, ot_font_get_3, ot_get_font_metrics,
    print_glyph_name, print_utf8_str, release_font_engine, AAT_FONT_FLAG, OTGR_FONT_FLAG,
};
use crate::xetex_ini::{
    _xeq_level_array, active_width, adjust_tail, after_token, align_ptr, align_state,
    area_delimiter, arith_error, avail, bchar, best_height_plus_depth, breadth_max,
    cancel_boundary, cond_ptr, cur_align, cur_area, cur_boundary, cur_box, cur_chr, cur_cmd,
    cur_cs, cur_dir, cur_ext, cur_group, cur_head, cur_if, cur_input, cur_l, cur_lang, cur_level,
    cur_list, cur_loop, cur_mark, cur_name, cur_order, cur_pre_head, cur_pre_tail, cur_ptr, cur_q,
    cur_r, cur_span, cur_tail, cur_tok, cur_val, cur_val1, cur_val_level, dead_cycles, def_ref,
    deletions_allowed, depth_threshold, dig, disc_ptr, error_count, error_line, expand_depth,
    expand_depth_count, ext_delimiter, false_bchar, file_line_error_style_p, file_name_quote_char,
    file_offset, first, first_count, fmem_ptr, font_in_short_display, font_used, force_eof,
    gave_char_warning_help, half_error_line, hash, hash_extra, hash_high, hash_used, hi_mem_min,
    history, if_limit, if_line, init_pool_ptr, init_str_ptr, ins_disc, insert_penalties,
    insert_src_special_auto, insert_src_special_every_par, insert_src_special_every_vbox,
    interaction, is_hyph, is_in_csname, job_name, last, last_badness, last_glue, last_kern,
    last_leftmost_char, last_node_type, last_penalty, last_rightmost_char, lft_hit, lig_stack,
    ligature_present, line, lo_mem_max, loaded_font_design_size, loaded_font_flags,
    loaded_font_letter_space, loaded_font_mapping, log_file, log_opened, long_help_seen,
    long_state, mag_set, main_f, main_h, main_i, main_j, main_k, main_s, mapped_text,
    max_buf_stack, max_print_line, max_reg_help_line, max_reg_num, max_strings, mem_end,
    name_in_progress, name_of_file, native_font_type_flag, native_len, native_text,
    native_text_size, no_new_control_sequence, old_setting, open_parens, output_active,
    pack_begin_line, page_contents, page_so_far, page_tail, par_loc, par_token, pdf_last_x_pos,
    pdf_last_y_pos, pool_ptr, pool_size, pre_adjust_tail, prev_class, prim, prim_eqtb, prim_used,
    pseudo_files, pstack, quoted_filename, radix, read_file, read_open, rover, rt_hit, rust_stdout,
    sa_chain, sa_level, sa_root, save_native_len, scanner_status, selector, set_box_allowed,
    shown_mode, skip_line, space_class, stop_at_space, str_pool, str_ptr, str_start, tally,
    term_offset, tex_remainder, texmf_log_name, total_shrink, total_stretch, trick_buf,
    trick_count, use_err_help, used_tectonic_coda_tokens, warning_index, write_file, write_open,
    xtx_ligature_present, LR_problems, LR_ptr, BASE_PTR, BCHAR_LABEL, BUFFER, BUF_SIZE, CHAR_BASE,
    DEPTH_BASE, EOF_SEEN, EQTB, EQTB_TOP, EXTEN_BASE, FONT_AREA, FONT_BC, FONT_BCHAR, FONT_CHECK,
    FONT_DSIZE, FONT_EC, FONT_FALSE_BCHAR, FONT_FLAGS, FONT_GLUE, FONT_INFO, FONT_LAYOUT_ENGINE,
    FONT_LETTER_SPACE, FONT_MAPPING, FONT_MAX, FONT_MEM_SIZE, FONT_NAME, FONT_PARAMS, FONT_PTR,
    FONT_SIZE, FULL_SOURCE_FILENAME_STACK, GRP_STACK, HEIGHT_BASE, HYPHEN_CHAR, IF_STACK,
    INPUT_FILE, INPUT_PTR, INPUT_STACK, IN_OPEN, ITALIC_BASE, KERN_BASE, LIG_KERN_BASE, LINE_STACK,
    MAX_IN_OPEN, MAX_IN_STACK, MAX_NEST_STACK, MAX_PARAM_STACK, MAX_SAVE_STACK, MEM, NEST,
    NEST_PTR, NEST_SIZE, PARAM_BASE, PARAM_PTR, PARAM_SIZE, PARAM_STACK, SAVE_PTR, SAVE_SIZE,
    SAVE_STACK, SKEW_CHAR, SOURCE_FILENAME_STACK, STACK_SIZE, WIDTH_BASE,
};
use crate::xetex_ini::{b16x4, b32x2, memory_word, prefixed_command};
use crate::xetex_io::{input_line, open_or_close_in, set_input_file_encoding, u_close};
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
use crate::xetex_scaledmath::{mult_and_add, round_xn_over_d, tex_round, x_over_n, xn_over_d};
use crate::xetex_shipout::{finalize_dvi_file, new_edge, out_what, ship_out};
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_stringpool::{
    append_str, length, make_string, search_string, slow_make_string, str_eq_buf, str_eq_str,
};
use crate::xetex_synctex::{synctex_start_input, synctex_terminate};
use crate::xetex_texmfmp::{
    getmd5sum, gettexstring, is_new_source, make_src_special, maketexstring, remember_source_info,
};
use crate::xetex_xetexd::*;
use bridge::{
    ttstub_input_close, ttstub_input_getc, ttstub_issue_warning_slice, ttstub_output_close,
    ttstub_output_open, ttstub_output_putc,
};

use bridge::{TTHistory, TTInputFormat};

use libc::{memcpy, strlen};

use bridge::InputHandleWrapper;
pub(crate) type scaled_t = i32;
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
pub(crate) unsafe fn badness(mut t: scaled_t, mut s: scaled_t) -> i32 {
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
    set_NODE_type(p.ptr(), TextNode::HList);
    p.set_lr_mode(LRMode::Normal)
        .set_width(0)
        .set_depth(0)
        .set_height(0);
    p.set_shift_amount(0)
        .set_list_ptr(None.tex_int())
        .set_glue_sign(GlueSign::Normal)
        .set_glue_order(GlueOrder::Normal)
        .set_glue_set(0.);
    p.ptr()
}
pub(crate) unsafe fn new_rule() -> usize {
    let mut p = Rule::from(get_node(RULE_NODE_SIZE));
    set_NODE_type(p.ptr(), TextNode::Rule);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_width(NULL_FLAG)
        .set_depth(NULL_FLAG)
        .set_height(NULL_FLAG);
    p.ptr()
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
pub(crate) unsafe fn new_math(w: i32, s: MathType) -> usize {
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
pub(crate) unsafe fn new_kern(w: i32) -> usize {
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
            match text_NODE_type(p).unwrap() {
                TextNode::HList
                | TextNode::VList
                | TextNode::Ins
                | TextNode::Mark
                | TextNode::Adjust
                | TextNode::Unset => print_cstr("[]"),
                TextNode::WhatsIt => match WhatsIt::from(p) {
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
                TextNode::Rule => print_chr('|'),
                TextNode::Glue => {
                    if MEM[p + 1].b32.s0 != 0 {
                        // TODO: strange (special case?)
                        print_chr(' ');
                    }
                }
                TextNode::Math => match Math(p).subtype() {
                    MathType::Eq(_, MathMode::Left) | MathType::Eq(_, MathMode::Right) => {
                        print_cstr("[]")
                    }
                    _ => print_chr('$'),
                },
                TextNode::Ligature => short_display(Ligature(p).lig_ptr().opt()),
                TextNode::Disc => {
                    let d = Discretionary(p);
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
pub(crate) unsafe fn print_rule_dimen(mut d: scaled_t) {
    if d == NULL_FLAG {
        print_chr('*');
    } else {
        print_scaled(d);
    };
}
pub(crate) unsafe fn print_glue(mut d: scaled_t, order: GlueOrder, s: &str) {
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
        if p.stretch() != 0 {
            print_cstr(" plus ");
            print_glue(p.stretch(), p.stretch_order(), unit);
        }
        if p.shrink() != 0 {
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
pub(crate) unsafe fn print_delimiter(p: usize) {
    let mut a: i32 = 0;
    a = ((MEM[p].b16.s3 as i32 % 256 * 256) as i64
        + (MEM[p].b16.s2 as i64 + (MEM[p].b16.s3 as i32 / 256) as i64 * 65536)) as i32;
    a = ((a * 4096 + MEM[p].b16.s1 as i32 % 256 * 256) as i64
        + (MEM[p].b16.s0 as i64 + (MEM[p].b16.s1 as i32 / 256) as i64 * 65536)) as i32;
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
    let mut n: i32 = 0;
    let mut g: f64 = 0.;
    if cur_length() > depth_threshold {
        if popt.is_some() {
            print_cstr(" []");
        }
        return;
    }
    n = 0;
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
        if is_char_node(Some(p)) {
            print_font_and_char(p);
        } else {
            let p = p as usize;
            match NODE_type(p) {
                ND::Text(n) => match n {
                    TextNode::HList | TextNode::VList => {
                        let p = List::from(p);
                        match n {
                            TextNode::HList => print_esc('h' as i32),
                            TextNode::VList => print_esc('v' as i32),
                            _ => unreachable!(),
                        }
                        print_cstr("box(");
                        print_scaled(p.height());
                        print_chr('+');
                        print_scaled(p.depth());
                        print_cstr(")x");
                        print_scaled(p.width());
                        g = p.glue_set();
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
                                print_glue((20000_i64 * 65536) as scaled_t, p.glue_order(), "");
                            } else {
                                print_glue(tex_round(65536_f64 * g), p.glue_order(), "");
                            }
                        }
                        if p.shift_amount() != 0 {
                            print_cstr(", shifted ");
                            print_scaled(p.shift_amount());
                        }
                        /*1491:*/
                        if text_NODE_type(p.ptr()) == TextNode::HList.into()
                            && p.lr_mode() == LRMode::DList
                        {
                            print_cstr(", display");
                        }
                        str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                        pool_ptr += 1;
                        show_node_list(p.list_ptr().opt());
                        pool_ptr -= 1
                    }
                    TextNode::Unset => {
                        let p = Unset::from(p);
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
                        if p.stretch() != 0 {
                            print_cstr(", stretch ");
                            print_glue(p.stretch(), p.stretch_order(), "");
                        }
                        if p.shrink() != 0 {
                            print_cstr(", shrink ");
                            print_glue(p.shrink(), p.shrink_order(), "");
                        }
                        str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                        pool_ptr += 1;
                        show_node_list(p.list_ptr().opt());
                        pool_ptr -= 1
                    }
                    TextNode::Rule => {
                        let p = Rule::from(p);
                        print_esc_cstr("rule(");
                        print_rule_dimen(p.height());
                        print_chr('+');
                        print_rule_dimen(p.depth());
                        print_cstr(")x");
                        print_rule_dimen(p.width());
                    }
                    TextNode::Ins => {
                        let p_ins = Insertion(p);
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
                    TextNode::WhatsIt => match WhatsIt::from(p) {
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
                                (*hash.offset((FONT_ID_BASE as i32 + nw.font() as i32) as isize))
                                    .s1,
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
                    TextNode::Glue => {
                        let g = Glue(p);
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
                    TextNode::Kern => {
                        let k = Kern(p);
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
                    TextNode::MarginKern => {
                        print_esc_cstr("kern");
                        print_scaled(MEM[p + 1].b32.s1);
                        if MEM[p].b16.s0 == 0 {
                            print_cstr(" (left margin)");
                        } else {
                            print_cstr(" (right margin)");
                        }
                    }
                    TextNode::Math => {
                        let p = Math(p);
                        match p.subtype() {
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
                                if p.width() != 0 {
                                    print_cstr(", surrounded ");
                                    print_scaled(p.width());
                                }
                            }
                        }
                    }
                    TextNode::Ligature => {
                        let l = Ligature(p);
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
                    TextNode::Penalty => {
                        let p = Penalty(p);
                        print_esc_cstr("penalty ");
                        print_int(p.penalty());
                    }
                    TextNode::Disc => {
                        let d = Discretionary(p);
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
                    TextNode::Mark => {
                        let m = Mark(p);
                        print_esc_cstr("mark");
                        if m.class() != 0 {
                            print_chr('s');
                            print_int(m.class());
                        }
                        print_mark(m.mark_ptr());
                    }
                    TextNode::Adjust => {
                        let a = Adjust(p);
                        print_esc_cstr("vadjust");
                        if a.subtype() != AdjustType::Post {
                            print_cstr(" pre ");
                        }
                        str_pool[pool_ptr as usize] = '.' as i32 as packed_UTF16_code;
                        pool_ptr += 1;
                        show_node_list(a.adj_ptr().opt());
                        pool_ptr -= 1
                    }
                    TextNode::Style => print_style(MEM[p].b16.s0 as i32),
                    TextNode::Choice => {
                        let c = Choice(p);
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
                ND::Math(n) => match n {
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
                                print_esc_cstr("radical");
                                print_delimiter(p + 4);
                            }
                            MathNode::Accent => {
                                print_esc_cstr("accent");
                                print_fam_and_char(p + 4);
                            }
                            MathNode::Left => {
                                print_esc_cstr("left");
                                print_delimiter(p + 1);
                            }
                            MathNode::Right => {
                                if MEM[p].b16.s0 == NORMAL {
                                    print_esc_cstr("right");
                                } else {
                                    print_esc_cstr("middle");
                                }
                                print_delimiter(p + 1);
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
                        print_esc_cstr("fraction, thickness ");
                        if MEM[p + 1].b32.s1 == DEFAULT_CODE {
                            print_cstr("= default");
                        } else {
                            print_scaled(MEM[p + 1].b32.s1);
                        }
                        if MEM[p + 4].b16.s3 as i32 % 256 != 0
                            || MEM[p + 4].b16.s2 as i64
                                + (MEM[p + 4].b16.s3 as i32 / 256) as i64 * 65536
                                != 0
                            || MEM[p + 4].b16.s1 as i32 % 256 != 0
                            || MEM[p + 4].b16.s0 as i64
                                + (MEM[p + 4].b16.s1 as i32 / 256) as i64 * 65536
                                != 0
                        {
                            print_cstr(", left-delimiter ");
                            print_delimiter(p + 4);
                        }
                        if MEM[p + 5].b16.s3 as i32 % 256 != 0
                            || MEM[p + 5].b16.s2 as i64
                                + (MEM[p + 5].b16.s3 as i32 / 256) as i64 * 65536
                                != 0
                            || MEM[p + 5].b16.s1 as i32 % 256 != 0
                            || MEM[p + 5].b16.s0 as i64
                                + (MEM[p + 5].b16.s1 as i32 / 256) as i64 * 65536
                                != 0
                        {
                            print_cstr(", right-delimiter ");
                            print_delimiter(p + 5);
                        }
                        print_subsidiary_data(p + 2, '\\' as i32 as UTF16_code);
                        print_subsidiary_data(p + 3, '/' as i32 as UTF16_code);
                    }
                },
                ND::Unknown(_) => print_cstr("Unknown node type!"),
            }
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
        if is_char_node(Some(p)) {
            *LLIST_link(p) = avail.tex_int();
            avail = Some(p);
        } else {
            match ND::from(MEM[p].b16.s1) {
                ND::Text(n) => match n {
                    TextNode::HList | TextNode::VList => {
                        let b = List::from(p);
                        flush_node_list(b.list_ptr().opt());
                        b.free();
                    }
                    TextNode::Unset => {
                        let u = Unset::from(p);
                        flush_node_list(u.list_ptr().opt());
                        u.free();
                    }
                    TextNode::Rule => {
                        let r = Rule::from(p);
                        r.free();
                    }
                    TextNode::Ins => {
                        let i = Insertion(p);
                        flush_node_list(i.ins_ptr().opt());
                        delete_glue_ref(i.split_top_ptr() as usize);
                        i.free();
                    }
                    TextNode::WhatsIt => {
                        match WhatsIt::from(p) {
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
                            //_ => confusion("ext3"),
                        }
                    }
                    TextNode::Glue => {
                        let mut p = Glue(p);
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
                    TextNode::Kern | TextNode::Math | TextNode::Penalty => {
                        free_node(p, MEDIUM_NODE_SIZE);
                    }
                    TextNode::MarginKern => {
                        free_node(p, MARGIN_KERN_NODE_SIZE);
                    }
                    TextNode::Ligature => {
                        let l = Ligature(p);
                        flush_node_list(l.lig_ptr().opt());
                        l.free();
                    }
                    TextNode::Mark => {
                        let m = Mark(p);
                        delete_token_ref(m.mark_ptr() as usize);
                        free_node(p, SMALL_NODE_SIZE);
                    }
                    TextNode::Disc => {
                        let d = Discretionary(p);
                        flush_node_list(d.pre_break().opt());
                        flush_node_list(d.post_break().opt());
                        d.free();
                    }
                    TextNode::Adjust => {
                        let a = Adjust(p);
                        flush_node_list(a.adj_ptr().opt());
                        a.free();
                    }
                    TextNode::Style => {
                        free_node(p, STYLE_NODE_SIZE);
                    }
                    TextNode::Choice => {
                        let c = Choice(p);
                        flush_node_list(c.display());
                        flush_node_list(c.text());
                        flush_node_list(c.script());
                        flush_node_list(c.scriptscript());
                        c.free();
                    }
                },
                ND::Math(n) => match n {
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
                ND::Unknown(_) => confusion("flushing"),
            }
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
            match text_NODE_type(p).confuse("copying") {
                TextNode::HList | TextNode::VList => {
                    let p = List::from(p);
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
                TextNode::Unset => {
                    let p = Unset::from(p);
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
                TextNode::Rule => {
                    r = get_node(RULE_NODE_SIZE);
                    words = (RULE_NODE_SIZE - 1) as u8
                }
                TextNode::Ins => {
                    r = get_node(INS_NODE_SIZE);
                    let p_ins = Insertion(p);
                    let mut r_ins = Insertion(r);
                    r_ins.set_split_top_ptr(p_ins.split_top_ptr());
                    GlueSpec(p_ins.split_top_ptr() as usize).rc_inc();
                    r_ins.set_ins_ptr(copy_node_list(p_ins.ins_ptr().opt()));
                    words = (INS_NODE_SIZE - 1) as u8
                }
                TextNode::WhatsIt => match WhatsIt::from(p) {
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
                TextNode::Glue => {
                    let p = Glue(p);
                    r = get_node(MEDIUM_NODE_SIZE);
                    let mut r_glue = Glue(r);
                    GlueSpec(p.glue_ptr() as usize).rc_inc();
                    *SYNCTEX_tag(r, MEDIUM_NODE_SIZE) = *SYNCTEX_tag(p.ptr(), MEDIUM_NODE_SIZE);
                    *SYNCTEX_line(r, MEDIUM_NODE_SIZE) = *SYNCTEX_line(p.ptr(), MEDIUM_NODE_SIZE);
                    r_glue.set_glue_ptr(p.glue_ptr());
                    r_glue.set_leader_ptr(copy_node_list(p.leader_ptr().opt()));
                }
                TextNode::Kern | TextNode::Math | TextNode::Penalty => {
                    r = get_node(MEDIUM_NODE_SIZE);
                    words = MEDIUM_NODE_SIZE as u8
                }
                TextNode::MarginKern => {
                    r = get_node(MARGIN_KERN_NODE_SIZE);
                    words = MARGIN_KERN_NODE_SIZE as u8
                }
                TextNode::Ligature => {
                    let p = Ligature(p);
                    r = get_node(SMALL_NODE_SIZE);
                    let mut r_lig = Ligature(r);
                    r_lig.set_char(p.char());
                    r_lig.set_font(p.font());
                    r_lig.set_lig_ptr(copy_node_list(p.lig_ptr().opt()));
                }
                TextNode::Disc => {
                    let p = Discretionary(p);
                    r = get_node(SMALL_NODE_SIZE);
                    let mut r_disc = Discretionary(r);
                    r_disc.set_pre_break(copy_node_list(p.pre_break().opt()));
                    r_disc.set_post_break(copy_node_list(p.post_break().opt()));
                }
                TextNode::Mark => {
                    let p = Mark(p);
                    r = get_node(SMALL_NODE_SIZE);
                    MarkClass(p.mark_ptr() as usize).rc_inc();
                    words = SMALL_NODE_SIZE as u8
                }
                TextNode::Adjust => {
                    let p = Adjust(p);
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
                            let mut t = r.box_reg() as i32;
                            print_int(t);
                            print_cstr(" adds ");
                            if *COUNT_REG(t as usize) == 1000 {
                                t = r.height();
                            } else {
                                t = x_over_n(r.height(), 1000) * *COUNT_REG(t as usize)
                            }
                            print_scaled(t);
                            if r.subtype() == PageInsType::SplitUp {
                                let mut q = PAGE_HEAD;
                                t = 0;
                                loop {
                                    q = *LLIST_link(q as usize) as usize;
                                    if NODE_type(q) == TextNode::Ins.into()
                                        && Insertion(q).box_reg() == r.box_reg()
                                    {
                                        t += 1
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
                        print_scaled(a.b32.s1);
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
pub(crate) unsafe fn begin_diagnostic() {
    old_setting = selector;
    if *INTPAR(IntPar::tracing_online) <= 0 && selector == Selector::TERM_AND_LOG {
        selector = (u8::from(selector) - 1).into();
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn end_diagnostic(mut blank_line: bool) {
    print_nl_cstr("");
    if blank_line {
        print_ln();
    }
    selector = old_setting;
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
                match Local::n(chr_code - LOCAL_BASE as i32) {
                    Some(Local::output_routine) => print_esc_cstr("output"),
                    Some(Local::every_par) => print_esc_cstr("everypar"),
                    Some(Local::every_math) => print_esc_cstr("everymath"),
                    Some(Local::every_display) => print_esc_cstr("everydisplay"),
                    Some(Local::every_hbox) => print_esc_cstr("everyhbox"),
                    Some(Local::every_vbox) => print_esc_cstr("everyvbox"),
                    Some(Local::every_job) => print_esc_cstr("everyjob"),
                    Some(Local::every_cr) => print_esc_cstr("everycr"),
                    Some(Local::every_eof) => print_esc_cstr("everyeof"),
                    Some(Local::xetex_inter_char) => print_esc_cstr("XeTeXinterchartoks"),
                    Some(Local::TectonicCodaTokens) => print_esc_cstr("TectonicCodaTokens"),
                    _ => print_esc_cstr("errhelp"),
                }
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
        Cmd::DelimNum => {
            if chr_code == 1 {
                print_esc_cstr("Udelimiter");
            } else {
                print_esc_cstr("delimiter");
            }
        }
        Cmd::Divide => print_esc_cstr("divide"),
        Cmd::EndCSName => print_esc_cstr("endcsname"),
        Cmd::EndGroup => print_esc_cstr("endgroup"),
        Cmd::ExSpace => print_esc(' ' as i32),
        Cmd::ExpandAfter => {
            if chr_code == 0 {
                print_esc_cstr("expandafter");
            } else {
                print_esc_cstr("unless");
            }
        }
        Cmd::HAlign => print_esc_cstr("halign"),
        Cmd::HRule => print_esc_cstr("hrule"),
        Cmd::IgnoreSpaces => {
            if chr_code == 0 {
                print_esc_cstr("ignorespaces");
            } else {
                print_esc_cstr("primitive");
            }
        }
        Cmd::Insert => print_esc_cstr("insert"),
        Cmd::ItalCorr => print_esc('/' as i32),
        Cmd::Mark => {
            print_esc_cstr("mark");
            if chr_code > 0 {
                print_chr('s');
            }
        }
        Cmd::MathAccent => {
            if chr_code == 1 {
                print_esc_cstr("Umathaccent");
            } else {
                print_esc_cstr("mathaccent");
            }
        }
        Cmd::MathCharNum => {
            if chr_code == 2 {
                print_esc_cstr("Umathchar");
            } else if chr_code == 1 {
                print_esc_cstr("Umathcharnum");
            } else {
                print_esc_cstr("mathchar");
            }
        }
        Cmd::MathChoice => print_esc_cstr("mathchoice"),
        Cmd::Multiply => print_esc_cstr("multiply"),
        Cmd::NoAlign => print_esc_cstr("noalign"),
        Cmd::NoBoundary => print_esc_cstr("noboundary"),
        Cmd::NoExpand => {
            if chr_code == 0 {
                print_esc_cstr("noexpand");
            } else {
                print_esc_cstr("primitive");
            }
        }
        Cmd::NonScript => print_esc_cstr("nonscript"),
        Cmd::Omit => print_esc_cstr("omit"),
        Cmd::Radical => {
            if chr_code == 1 {
                print_esc_cstr("Uradical");
            } else {
                print_esc_cstr("radical");
            }
        }
        Cmd::ReadToCS => {
            if chr_code == 0 {
                print_esc_cstr("read");
            } else {
                print_esc_cstr("readline");
            }
        }
        Cmd::Relax => print_esc_cstr("relax"),
        Cmd::SetBox => print_esc_cstr("setbox"),
        Cmd::SetPrevGraf => print_esc_cstr("prevgraf"),
        Cmd::SetShape => match chr_code as usize {
            c if c == LOCAL_BASE as usize + Local::par_shape as usize => print_esc_cstr("parshape"),
            INTER_LINE_PENALTIES_LOC => print_esc_cstr("interlinepenalties"),
            CLUB_PENALTIES_LOC => print_esc_cstr("clubpenalties"),
            WIDOW_PENALTIES_LOC => print_esc_cstr("widowpenalties"),
            DISPLAY_WIDOW_PENALTIES_LOC => print_esc_cstr("displaywidowpenalties"),
            _ => {}
        },
        Cmd::The => {
            if chr_code == 0 {
                print_esc_cstr("the");
            } else if chr_code == 1 {
                print_esc_cstr("unexpanded");
            } else {
                print_esc_cstr("detokenize");
            }
        }
        Cmd::ToksRegister => {
            print_esc_cstr("toks");
            if chr_code != 0 {
                print_sa_num(chr_code.opt().unwrap());
            }
        }
        Cmd::VAdjust => print_esc_cstr("vadjust"),
        Cmd::VAlign => {
            if chr_code == 0 {
                print_esc_cstr("valign");
            } else {
                match MathType::from(chr_code as u16) {
                    MathType::Eq(BE::Begin, MathMode::Left) => print_esc_cstr("beginL"),
                    MathType::Eq(BE::End, MathMode::Left) => print_esc_cstr("endL"),
                    MathType::Eq(BE::Begin, MathMode::Right) => print_esc_cstr("beginR"),
                    _ => print_esc_cstr("endR"),
                }
            }
        }
        Cmd::VCenter => print_esc_cstr("vcenter"),
        Cmd::VRule => print_esc_cstr("vrule"),
        PAR_END => print_esc_cstr("par"),
        Cmd::Input => {
            if chr_code == 0 {
                print_esc_cstr("input");
            } else if chr_code == 2i32 {
                print_esc_cstr("scantokens");
            } else {
                print_esc_cstr("endinput");
            }
        }
        Cmd::TopBotMark => {
            match (chr_code % MARKS_CODE) as usize {
                FIRST_MARK_CODE => print_esc_cstr("firstmark"),
                BOT_MARK_CODE => print_esc_cstr("botmark"),
                SPLIT_FIRST_MARK_CODE => print_esc_cstr("splitfirstmark"),
                SPLIT_BOT_MARK_CODE => print_esc_cstr("splitbotmark"),
                _ => print_esc_cstr("topmark"),
            }
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
            if cmd == ValLevel::Int as u16 {
                print_esc_cstr("count");
            } else if cmd == ValLevel::Dimen as u16 {
                print_esc_cstr("dimen");
            } else if cmd == ValLevel::Glue as u16 {
                print_esc_cstr("skip");
            } else {
                print_esc_cstr("muskip");
            }
            if let Some(q) = chr_code.opt() {
                print_sa_num(q);
            }
        }
        Cmd::SetAux => {
            if chr_code == ListMode::VMode as i32 {
                print_esc_cstr("prevdepth");
            } else {
                print_esc_cstr("spacefactor");
            }
        }
        Cmd::SetPageInt => {
            if chr_code == 0 {
                print_esc_cstr("deadcycles");
            } else if chr_code == 2 {
                print_esc_cstr("interactionmode");
            } else {
                print_esc_cstr("insertpenalties");
            }
        }
        Cmd::SetBoxDimen => match SetBoxDimen::n(chr_code as u8).unwrap() {
            SetBoxDimen::WidthOffset => print_esc_cstr("wd"),
            SetBoxDimen::HeightOffset => print_esc_cstr("ht"),
            SetBoxDimen::DepthOffset => print_esc_cstr("dp"),
        },
        Cmd::LastItem => {
            use LastItemCode::*;
            match LastItemCode::n(chr_code as u8).unwrap() {
                LastPenalty => print_esc_cstr("lastpenalty"),
                LastKern => print_esc_cstr("lastkern"),
                LastSkip => print_esc_cstr("lastskip"),
                InputLineNo => print_esc_cstr("inputlineno"),
                PdfShellEscape => print_esc_cstr("shellescape"),
                LastNodeType => print_esc_cstr("lastnodetype"),
                EtexVersion => print_esc_cstr("eTeXversion"),
                XetexVersion => print_esc_cstr("XeTeXversion"),
                XetexCountGlyphs => print_esc_cstr("XeTeXcountglyphs"),
                XetexCountVariations => print_esc_cstr("XeTeXcountvariations"),
                XetexVariation => print_esc_cstr("XeTeXvariation"),
                XetexFindVariationByName => print_esc_cstr("XeTeXfindvariationbyname"),
                XetexVariationMin => print_esc_cstr("XeTeXvariationmin"),
                XetexVariationMax => print_esc_cstr("XeTeXvariationmax"),
                XetexVariationDefault => print_esc_cstr("XeTeXvariationdefault"),
                XetexCountFeatures => print_esc_cstr("XeTeXcountfeatures"),
                XetexFeatureCode => print_esc_cstr("XeTeXfeaturecode"),
                XetexFindFeatureByName => print_esc_cstr("XeTeXfindfeaturebyname"),
                XetexIsExclusiveFeature => print_esc_cstr("XeTeXisexclusivefeature"),
                XetexCountSelectors => print_esc_cstr("XeTeXcountselectors"),
                XetexSelectorCode => print_esc_cstr("XeTeXselectorcode"),
                XetexFindSelectorByName => print_esc_cstr("XeTeXfindselectorbyname"),
                XetexIsDefaultSelector => print_esc_cstr("XeTeXisdefaultselector"),
                XetexOTCountScripts => print_esc_cstr("XeTeXOTcountscripts"),
                XetexOTCountLanguages => print_esc_cstr("XeTeXOTcountlanguages"),
                XetexOTCountFeatures => print_esc_cstr("XeTeXOTcountfeatures"),
                XetexOTScript => print_esc_cstr("XeTeXOTscripttag"),
                XetexOTLanguage => print_esc_cstr("XeTeXOTlanguagetag"),
                XetexOTFeature => print_esc_cstr("XeTeXOTfeaturetag"),
                XetexMapCharToGlyph => print_esc_cstr("XeTeXcharglyph"),
                XetexGlyphIndex => print_esc_cstr("XeTeXglyphindex"),
                XetexGlyphBounds => print_esc_cstr("XeTeXglyphbounds"),
                XetexFontType => print_esc_cstr("XeTeXfonttype"),
                XetexFirstChar => print_esc_cstr("XeTeXfirstfontchar"),
                XetexLastChar => print_esc_cstr("XeTeXlastfontchar"),
                PdfLastXPos => print_esc_cstr("pdflastxpos"),
                PdfLastYPos => print_esc_cstr("pdflastypos"),
                XetexPdfPageCount => print_esc_cstr("XeTeXpdfpagecount"),
                CurrentGroupLevel => print_esc_cstr("currentgrouplevel"),
                CurrentGroupType => print_esc_cstr("currentgrouptype"),
                CurrentIfLevel => print_esc_cstr("currentiflevel"),
                CurrentIfType => print_esc_cstr("currentiftype"),
                CurrentIfBranch => print_esc_cstr("currentifbranch"),
                FontCharWd => print_esc_cstr("fontcharwd"),
                FontCharHt => print_esc_cstr("fontcharht"),
                FontCharDp => print_esc_cstr("fontchardp"),
                FontCharIc => print_esc_cstr("fontcharic"),
                ParShapeLength => print_esc_cstr("parshapelength"),
                ParShapeIndent => print_esc_cstr("parshapeindent"),
                ParShapeDimen => print_esc_cstr("parshapedimen"),
                EtexExprInt => print_esc_cstr("numexpr"),
                EtexExprDimen => print_esc_cstr("dimexpr"),
                EtexExprGlue => print_esc_cstr("glueexpr"),
                EtexExprMu => print_esc_cstr("muexpr"),
                GlueStretchOrder => print_esc_cstr("gluestretchorder"),
                GlueShrinkOrder => print_esc_cstr("glueshrinkorder"),
                GlueStretch => print_esc_cstr("gluestretch"),
                GlueShrink => print_esc_cstr("glueshrink"),
                MuToGlue => print_esc_cstr("mutoglue"),
                GlueToMu => print_esc_cstr("gluetomu"),
                Badness => print_esc_cstr("badness"),
            }
        }
        Cmd::Convert => {
            use ConvertCode::*;
            match ConvertCode::n(chr_code as u8).unwrap() {
                Number => print_esc_cstr("number"),
                RomanNumeral => print_esc_cstr("romannumeral"),
                String => print_esc_cstr("string"),
                Meaning => print_esc_cstr("meaning"),
                FontName => print_esc_cstr("fontname"),
                PdfStrcmp => print_esc_cstr("strcmp"),
                PdfMdfiveSum => print_esc_cstr("mdfivesum"),
                LeftMarginKern => print_esc_cstr("leftmarginkern"),
                RightMarginKern => print_esc_cstr("rightmarginkern"),
                EtexRevision => print_esc_cstr("eTeXrevision"),
                XetexRevision => print_esc_cstr("XeTeXrevision"),
                XetexVariationName => print_esc_cstr("XeTeXvariationname"),
                XetexFeatureName => print_esc_cstr("XeTeXfeaturename"),
                XetexSelectorName => print_esc_cstr("XeTeXselectorname"),
                XetexGlyphName => print_esc_cstr("XeTeXglyphname"),
                XetexUchar => print_esc_cstr("Uchar"),
                XetexUcharcat => print_esc_cstr("Ucharcat"),
                JobName => print_esc_cstr("jobname"),
            }
        }
        Cmd::IfTest => {
            if chr_code >= UNLESS_CODE {
                print_esc_cstr("unless");
            }
            match IfTestCode::n((chr_code % UNLESS_CODE) as u8).unwrap() {
                IfTestCode::IfCat => print_esc_cstr("ifcat"),
                IfTestCode::IfInt => print_esc_cstr("ifnum"),
                IfTestCode::IfDim => print_esc_cstr("ifdim"),
                IfTestCode::IfOdd => print_esc_cstr("ifodd"),
                IfTestCode::IfVMode => print_esc_cstr("ifvmode"),
                IfTestCode::IfHMode => print_esc_cstr("ifhmode"),
                IfTestCode::IfMMode => print_esc_cstr("ifmmode"),
                IfTestCode::IfInner => print_esc_cstr("ifinner"),
                IfTestCode::IfVoid => print_esc_cstr("ifvoid"),
                IfTestCode::IfHBox => print_esc_cstr("ifhbox"),
                IfTestCode::IfVBox => print_esc_cstr("ifvbox"),
                IfTestCode::Ifx => print_esc_cstr("ifx"),
                IfTestCode::IfEof => print_esc_cstr("ifeof"),
                IfTestCode::IfTrue => print_esc_cstr("iftrue"),
                IfTestCode::IfFalse => print_esc_cstr("iffalse"),
                IfTestCode::IfCase => print_esc_cstr("ifcase"),
                IfTestCode::IfPrimitive => print_esc_cstr("ifprimitive"),
                IfTestCode::IfDef => print_esc_cstr("ifdefined"),
                IfTestCode::IfCS => print_esc_cstr("ifcsname"),
                IfTestCode::IfFontChar => print_esc_cstr("iffontchar"),
                IfTestCode::IfInCSName => print_esc_cstr("ifincsname"),
                IfTestCode::IfChar => print_esc_cstr("if"),
            }
        }
        Cmd::FiOrElse => match FiOrElseCode::n(chr_code as u8).unwrap() {
            FiOrElseCode::Fi => print_esc_cstr("fi"),
            FiOrElseCode::Or => print_esc_cstr("or"),
            FiOrElseCode::Else => print_esc_cstr("else"),
            _ => unreachable!(),
        },
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
        Cmd::CarRet => {
            if chr_code == CR_CODE {
                print_esc_cstr("cr");
            } else {
                print_esc_cstr("crcr");
            }
        }
        Cmd::SetPageDimen => {
            match chr_code {
                0 => print_esc_cstr("pagegoal"),         // genuine literal in WEB
                1 => print_esc_cstr("pagetotal"),        // genuine literal in WEB
                2 => print_esc_cstr("pagestretch"),      // genuine literal in WEB
                3 => print_esc_cstr("pagefilstretch"),   // genuine literal in WEB
                4 => print_esc_cstr("pagefillstretch"),  // genuine literal in WEB
                5 => print_esc_cstr("pagefilllstretch"), // genuine literal in WEB
                6 => print_esc_cstr("pageshrink"),       // genuine literal in WEB
                _ => print_esc_cstr("pagedepth"),
            }
        }
        STOP => {
            if chr_code == 1 {
                print_esc_cstr("dump");
            } else {
                print_esc_cstr("end");
            }
        }
        Cmd::HSkip => match SkipCode::n(chr_code as u8).unwrap() {
            SkipCode::Skip => print_esc_cstr("hskip"),
            SkipCode::Fil => print_esc_cstr("hfil"),
            SkipCode::Fill => print_esc_cstr("hfill"),
            SkipCode::Ss => print_esc_cstr("hss"),
            SkipCode::FilNeg => print_esc_cstr("hfilneg"),
            _ => unreachable!(),
        },
        Cmd::VSkip => match SkipCode::n(chr_code as u8).unwrap() {
            SkipCode::Skip => print_esc_cstr("vskip"),
            SkipCode::Fil => print_esc_cstr("vfil"),
            SkipCode::Fill => print_esc_cstr("vfill"),
            SkipCode::Ss => print_esc_cstr("vss"),
            SkipCode::FilNeg => print_esc_cstr("vfilneg"),
            _ => unreachable!(),
        },
        Cmd::MSkip => print_esc_cstr("mskip"),
        Cmd::Kern => print_esc_cstr("kern"),
        Cmd::MKern => print_esc_cstr("mkern"),
        Cmd::HMove => {
            if chr_code == 1i32 {
                print_esc_cstr("moveleft");
            } else {
                print_esc_cstr("moveright");
            }
        }
        Cmd::VMove => {
            if chr_code == 1 {
                print_esc_cstr("raise");
            } else {
                print_esc_cstr("lower");
            }
        }
        Cmd::MakeBox => match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Box => print_esc_cstr("box"),
            BoxCode::Copy => print_esc_cstr("copy"),
            BoxCode::LastBox => print_esc_cstr("lastbox"),
            BoxCode::VSplit => print_esc_cstr("vsplit"),
            BoxCode::VTop => print_esc_cstr("vtop"),
            BoxCode::VBox => print_esc_cstr("vbox"),
            BoxCode::HBox => print_esc_cstr("hbox"),
        },
        Cmd::LeaderShip => match chr_code as u16 {
            A_LEADERS => print_esc_cstr("leaders"),
            C_LEADERS => print_esc_cstr("cleaders"),
            X_LEADERS => print_esc_cstr("xleaders"),
            _ => print_esc_cstr("shipout"),
        },
        Cmd::StartPar => {
            if chr_code == 0 {
                print_esc_cstr("noindent");
            } else {
                print_esc_cstr("indent");
            }
        }
        Cmd::RemoveItem => match ND::from(chr_code as u16) {
            ND::Text(TextNode::Glue) => print_esc_cstr("unskip"),
            ND::Text(TextNode::Kern) => print_esc_cstr("unkern"),
            _ => print_esc_cstr("unpenalty"),
        },
        Cmd::UnHBox => match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Copy => print_esc_cstr("unhcopy"),
            _ => print_esc_cstr("unhbox"),
        },
        Cmd::UnVBox => match BoxCode::n(chr_code as u8).unwrap() {
            BoxCode::Copy => print_esc_cstr("unvcopy"),
            BoxCode::LastBox => print_esc_cstr("pagediscards"),
            BoxCode::VSplit => print_esc_cstr("splitdiscards"),
            _ => print_esc_cstr("unvbox"),
        },
        Cmd::Discretionary => {
            if chr_code == 1 {
                print_esc('-' as i32);
            } else {
                print_esc_cstr("discretionary");
            }
        }
        Cmd::EqNo => {
            if chr_code == 1 {
                print_esc_cstr("leqno");
            } else {
                print_esc_cstr("eqno");
            }
        }
        Cmd::MathComp => match MathNode::n(chr_code as u16).unwrap() {
            MathNode::Ord => print_esc_cstr("mathord"),
            MathNode::Op => print_esc_cstr("mathop"),
            MathNode::Bin => print_esc_cstr("mathbin"),
            MathNode::Rel => print_esc_cstr("mathrel"),
            MathNode::Open => print_esc_cstr("mathopen"),
            MathNode::Close => print_esc_cstr("mathclose"),
            MathNode::Punct => print_esc_cstr("mathpunct"),
            MathNode::Inner => print_esc_cstr("mathinner"),
            MathNode::Under => print_esc_cstr("underline"),
            _ => print_esc_cstr("overline"),
        },
        Cmd::LimitSwitch => match Limit::from(chr_code as u16) {
            Limit::Limits => print_esc_cstr("limits"),
            Limit::NoLimits => print_esc_cstr("nolimits"),
            Limit::Normal => print_esc_cstr("displaylimits"),
        },
        Cmd::MathStyle => print_style(chr_code),
        Cmd::Above => match chr_code {
            OVER_CODE => print_esc_cstr("over"),
            ATOP_CODE => print_esc_cstr("atop"),
            // DELIMITED_CODE + ABOVE_CODE
            3 => print_esc_cstr("abovewithdelims"),
            // DELIMITED_CODE + OVER_CODE
            4 => print_esc_cstr("overwithdelims"),
            // DELIMITED_CODE + ATOP_CODE
            5 => print_esc_cstr("atopwithdelims"),
            _ => print_esc_cstr("above"),
        },
        Cmd::LeftRight => {
            if chr_code as u16 == MathNode::Left as u16 {
                print_esc_cstr("left");
            } else if chr_code as u16 == MIDDLE_NOAD {
                print_esc_cstr("middle");
            } else {
                print_esc_cstr("right");
            }
        }
        Cmd::Prefix => {
            if chr_code == 1 {
                print_esc_cstr("long");
            } else if chr_code == 2 {
                print_esc_cstr("outer");
            } else if chr_code == 8 {
                print_esc_cstr("protected");
            } else {
                print_esc_cstr("global");
            }
        }
        Cmd::Def => {
            if chr_code == 0 {
                print_esc_cstr("def");
            } else if chr_code == 1 {
                print_esc_cstr("gdef");
            } else if chr_code == 2 {
                print_esc_cstr("edef");
            } else {
                print_esc_cstr("xdef");
            }
        }
        Cmd::Let => {
            if chr_code as u16 != NORMAL {
                print_esc_cstr("futurelet");
            } else {
                print_esc_cstr("let");
            }
        }
        Cmd::ShorthandDef => match ShorthandDefCode::n(chr_code as u8).unwrap() {
            ShorthandDefCode::Char => print_esc_cstr("chardef"),
            ShorthandDefCode::MathChar => print_esc_cstr("mathchardef"),
            ShorthandDefCode::XetexMathChar => print_esc_cstr("Umathchardef"),
            ShorthandDefCode::XetexMathCharNum => print_esc_cstr("Umathcharnumdef"),
            ShorthandDefCode::Count => print_esc_cstr("countdef"),
            ShorthandDefCode::Dimen => print_esc_cstr("dimendef"),
            ShorthandDefCode::Skip => print_esc_cstr("skipdef"),
            ShorthandDefCode::MuSkip => print_esc_cstr("muskipdef"),
            ShorthandDefCode::CharSub => print_esc_cstr("charsubdef"),
            ShorthandDefCode::Toks => print_esc_cstr("toksdef"),
        },
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
        Cmd::DefCode => {
            if chr_code == CAT_CODE_BASE as i32 {
                print_esc_cstr("catcode");
            } else if chr_code == MATH_CODE_BASE as i32 {
                print_esc_cstr("mathcode");
            } else if chr_code == LC_CODE_BASE as i32 {
                print_esc_cstr("lccode");
            } else if chr_code == UC_CODE_BASE as i32 {
                print_esc_cstr("uccode");
            } else if chr_code == SF_CODE_BASE as i32 {
                print_esc_cstr("sfcode");
            } else {
                print_esc_cstr("delcode");
            }
        }
        Cmd::XetexDefCode => {
            if chr_code == SF_CODE_BASE as i32 {
                print_esc_cstr("XeTeXcharclass");
            } else if chr_code == MATH_CODE_BASE as i32 {
                print_esc_cstr("Umathcodenum");
            } else if chr_code == MATH_CODE_BASE as i32 + 1 {
                print_esc_cstr("Umathcode");
            } else if chr_code == DEL_CODE_BASE as i32 {
                print_esc_cstr("Udelcodenum");
            } else {
                print_esc_cstr("Udelcode");
            }
        }
        Cmd::DefFamily => print_size(chr_code - (MATH_FONT_BASE as i32)),
        Cmd::HyphData => {
            if chr_code == 1 {
                print_esc_cstr("patterns");
            } else {
                print_esc_cstr("hyphenation");
            }
        }
        Cmd::AssignFontInt => match chr_code {
            0 => print_esc_cstr("hyphenchar"),
            1 => print_esc_cstr("skewchar"),
            LP_CODE_BASE => print_esc_cstr("lpcode"),
            RP_CODE_BASE => print_esc_cstr("rpcode"),
            _ => {}
        },
        Cmd::SetFont => {
            print_cstr("select font ");
            font_name_str = FONT_NAME[chr_code as usize];
            if FONT_AREA[chr_code as usize] as u32 == AAT_FONT_FLAG
                || FONT_AREA[chr_code as usize] as u32 == OTGR_FONT_FLAG
            {
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
        Cmd::SetInteraction => match InteractionMode::n(chr_code as u8).unwrap() {
            InteractionMode::Batch => print_esc_cstr("batchmode"),
            InteractionMode::NonStop => print_esc_cstr("nonstopmode"),
            InteractionMode::Scroll => print_esc_cstr("scrollmode"),
            InteractionMode::ErrorStop => print_esc_cstr("errorstopmode"),
        },
        Cmd::InStream => {
            if chr_code == 0 {
                print_esc_cstr("closein");
            } else {
                print_esc_cstr("openin");
            }
        }
        Cmd::Message => {
            if chr_code == 0 {
                print_esc_cstr("message");
            } else {
                print_esc_cstr("errmessage");
            }
        }
        Cmd::CaseShift => {
            if chr_code == LC_CODE_BASE as i32 {
                print_esc_cstr("lowercase");
            } else {
                print_esc_cstr("uppercase");
            }
        }
        Cmd::XRay => match chr_code {
            SHOW_BOX_CODE => print_esc_cstr("showbox"),
            SHOW_THE_CODE => print_esc_cstr("showthe"),
            SHOW_LISTS => print_esc_cstr("showlists"),
            SHOW_GROUPS => print_esc_cstr("showgroups"),
            SHOW_TOKENS => print_esc_cstr("showtokens"),
            SHOW_IFS => print_esc_cstr("showifs"),
            _ => print_esc_cstr("show"),
        },
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
                if str_eq_str(prim[p].s1 - 1, s) {
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
pub(crate) unsafe fn print_group(mut e: bool) {
    match cur_group {
        GroupCode::BottomLevel => {
            print_cstr("bottom level");
            return;
        }
        GroupCode::Simple | GroupCode::SemiSimple => {
            if cur_group == GroupCode::SemiSimple {
                print_cstr("semi ");
            }
            print_cstr("simple");
        }
        GroupCode::HBox | GroupCode::AdjustedHBox => {
            if cur_group == GroupCode::AdjustedHBox {
                print_cstr("adjusted ");
            }
            print_cstr("hbox");
        }
        GroupCode::VBox => print_cstr("vbox"),
        GroupCode::VTop => print_cstr("vtop"),
        GroupCode::Align | GroupCode::NoAlign => {
            if cur_group == GroupCode::NoAlign {
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
            if cur_group == GroupCode::MathChoice {
                print_cstr(" choice");
            } else if cur_group == GroupCode::MathShift {
                print_cstr(" shift");
            } else if cur_group == GroupCode::MathLeft {
                print_cstr(" left");
            }
        }
    }
    print_cstr(" group (level ");
    print_int(cur_level as i32);
    print_chr(')');
    if SAVE_STACK[SAVE_PTR - 1].val != 0 {
        if e {
            print_cstr(" entered at line ");
        } else {
            print_cstr(" at line ");
        }
        print_int(SAVE_STACK[SAVE_PTR - 1].val);
    };
}
/*:1448*/
/*1449: */
pub(crate) unsafe fn pseudo_input() -> bool {
    last = first;
    if let Some(p) = MEM[pseudo_files as usize].b32.s0.opt() {
        MEM[pseudo_files as usize].b32.s0 = MEM[p as usize].b32.s1;
        let sz = MEM[p].b32.s0 as usize;
        if 4 * sz - 3 >= BUF_SIZE - last as usize {
            /*35: */
            cur_input.loc = first;
            cur_input.limit = last - 1;
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
pub(crate) unsafe fn group_warning() {
    let mut w: bool = false;
    BASE_PTR = INPUT_PTR;
    INPUT_STACK[BASE_PTR] = cur_input;
    let mut i = IN_OPEN;
    w = false;
    while GRP_STACK[i] == cur_boundary && i > 0 {
        if *INTPAR(IntPar::tracing_nesting) > 0 {
            while INPUT_STACK[BASE_PTR].state == InputState::TokenList
                || INPUT_STACK[BASE_PTR].index as usize > i
            {
                BASE_PTR -= 1;
            }
            if INPUT_STACK[BASE_PTR].name > 17 {
                w = true
            }
        }
        GRP_STACK[i] = SAVE_STACK[SAVE_PTR].val;
        i -= 1;
    }
    if w {
        print_nl_cstr("Warning: end of ");
        print_group(true);
        print_cstr(" of a different file");
        print_ln();
        if *INTPAR(IntPar::tracing_nesting) > 1 {
            show_context();
        }
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn if_warning() {
    let mut w: bool = false;
    BASE_PTR = INPUT_PTR;
    INPUT_STACK[BASE_PTR] = cur_input;
    let mut i = IN_OPEN;
    w = false;
    while IF_STACK[i] == cond_ptr {
        if *INTPAR(IntPar::tracing_nesting) > 0 {
            while INPUT_STACK[BASE_PTR].state == InputState::TokenList
                || INPUT_STACK[BASE_PTR].index as usize > i
            {
                BASE_PTR -= 1
            }
            if INPUT_STACK[BASE_PTR].name > 17 {
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
            show_context();
        }
        if history == TTHistory::SPOTLESS {
            history = TTHistory::WARNING_ISSUED
        }
    };
}
pub(crate) unsafe fn file_warning() {
    let mut l: u16 = 0;
    let mut i: i32 = 0;
    let p = SAVE_PTR as i32;
    l = cur_level;
    let c = cur_group;
    SAVE_PTR = cur_boundary as usize;
    while GRP_STACK[IN_OPEN] != SAVE_PTR as i32 {
        cur_level -= 1;
        print_nl_cstr("Warning: end of file when ");
        print_group(true);
        print_cstr(" is incomplete");
        cur_group = GroupCode::from(SAVE_STACK[SAVE_PTR].lvl);
        SAVE_PTR = SAVE_STACK[SAVE_PTR].val as usize
    }
    SAVE_PTR = p as usize;
    cur_level = l;
    cur_group = c;
    let p = cond_ptr;
    l = if_limit as u16;
    let c = cur_if as u16;
    i = if_line;
    while IF_STACK[IN_OPEN] != cond_ptr {
        print_nl_cstr("Warning: end of file when ");
        print_cmd_chr(Cmd::IfTest, cur_if as i32);
        if if_limit == FiOrElseCode::Fi {
            print_esc_cstr("else");
        }
        if if_line != 0 {
            print_cstr(" entered on line ");
            print_int(if_line);
        }
        print_cstr(" is incomplete");
        let cp = cond_ptr.unwrap();
        if_line = MEM[cp + 1].b32.s1;
        cur_if = MEM[cp].b16.s0 as i16;
        if_limit = FiOrElseCode::n(MEM[cp].b16.s1 as u8).unwrap();
        cond_ptr = llist_link(cp);
    }
    cond_ptr = p;
    if_limit = FiOrElseCode::n(l as u8).unwrap();
    cur_if = c as i16;
    if_line = i;
    print_ln();
    if *INTPAR(IntPar::tracing_nesting) > 1 {
        show_context();
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
    SAVE_STACK[SAVE_PTR + 0].val = line;
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
pub(crate) unsafe fn unsave() {
    let mut p: i32 = 0;
    let mut l = 0_u16;
    let mut t: i32 = 0;
    let mut a: bool = false;
    a = false;
    if cur_level > LEVEL_ONE {
        cur_level -= 1;
        loop {
            SAVE_PTR -= 1;
            if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::LevelBoundary as u16 {
                break;
            }
            p = SAVE_STACK[SAVE_PTR].val;
            if SAVE_STACK[SAVE_PTR].cmd == SaveCmd::InsertToken as u16 {
                /*338: */
                t = cur_tok;
                cur_tok = p;
                if a {
                    p = get_avail() as i32;
                    MEM[p as usize].b32.s0 = cur_tok;
                    *LLIST_link(p as usize) = cur_input.loc;
                    cur_input.loc = p;
                    cur_input.start = p;
                    if cur_tok < RIGHT_BRACE_LIMIT {
                        if cur_tok < LEFT_BRACE_LIMIT {
                            align_state -= 1
                        } else {
                            align_state += 1
                        }
                    }
                } else {
                    back_input();
                    a = true
                }
                cur_tok = t
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
            group_warning();
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
pub(crate) unsafe fn print_meaning() {
    print_cmd_chr(cur_cmd, cur_chr);
    if cur_cmd >= Cmd::Call {
        print_chr(':');
        print_ln();
        token_show(cur_chr.opt());
    } else if cur_cmd == Cmd::TopBotMark && cur_chr < 5 {
        print_chr(':');
        print_ln();
        token_show(cur_mark[cur_chr as usize]);
    };
}
pub(crate) unsafe fn show_cur_cmd_chr() {
    begin_diagnostic();
    print_nl('{' as i32);
    if cur_list.mode != shown_mode {
        print_mode(cur_list.mode);
        print_cstr(": ");
        shown_mode = cur_list.mode
    }
    print_cmd_chr(cur_cmd, cur_chr);
    if *INTPAR(IntPar::tracing_ifs) > 0 {
        if cur_cmd >= Cmd::IfTest {
            if cur_cmd <= Cmd::FiOrElse {
                print_cstr(": ");
                let mut n;
                let l;
                if cur_cmd == Cmd::FiOrElse {
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
    end_diagnostic(false);
}
pub(crate) unsafe fn show_context() {
    BASE_PTR = INPUT_PTR;
    INPUT_STACK[BASE_PTR] = cur_input;
    let mut nn = -1;
    let mut bottom_line = false;
    loop {
        cur_input = INPUT_STACK[BASE_PTR];
        if cur_input.state != InputState::TokenList {
            if cur_input.name > 19 || BASE_PTR == 0 {
                bottom_line = true
            }
        }
        if BASE_PTR == INPUT_PTR || bottom_line || nn < *INTPAR(IntPar::error_context_lines) {
            /*324: */
            if BASE_PTR == INPUT_PTR
                || cur_input.state != InputState::TokenList
                || cur_input.index != Btl::BackedUp
                || cur_input.loc.opt().is_some()
            {
                tally = 0i32;
                let old_setting_0 = selector;
                let l;
                if cur_input.state != InputState::TokenList {
                    if cur_input.name <= 17 {
                        if cur_input.name == 0 {
                            if BASE_PTR == 0 {
                                print_nl_cstr("<*>");
                            } else {
                                print_nl_cstr("<insert> ");
                            }
                        } else {
                            print_nl_cstr("<read ");
                            if cur_input.name == 17 {
                                print_chr('*');
                            } else {
                                print_int(cur_input.name - 1);
                            }
                            print_chr('>');
                        }
                    } else {
                        print_nl_cstr("l.");
                        if cur_input.index as usize == IN_OPEN {
                            print_int(line);
                        } else {
                            print_int(LINE_STACK[(cur_input.index as i32 + 1) as usize]);
                        }
                    }
                    print_chr(' ');
                    l = tally;
                    tally = 0;
                    selector = Selector::PSEUDO;
                    trick_count = 1000000;
                    let j = if BUFFER[cur_input.limit as usize] == *INTPAR(IntPar::end_line_char) {
                        cur_input.limit
                    } else {
                        cur_input.limit + 1
                    };
                    if j > 0 {
                        for i in cur_input.start..j {
                            if i == cur_input.loc {
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
                    match cur_input.index {
                        Btl::Parameter => print_nl_cstr("<argument> "),
                        Btl::UTemplate | Btl::VTemplate => print_nl_cstr("<template> "),
                        Btl::BackedUp | Btl::BackedUpChar => {
                            if cur_input.loc.opt().is_none() {
                                print_nl_cstr("<recently read> ");
                            } else {
                                print_nl_cstr("<to be read again> ");
                            }
                        }
                        Btl::Inserted => print_nl_cstr("<inserted text> "),
                        Btl::Macro => {
                            print_ln();
                            print_cs(cur_input.name);
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
                    .contains(&cur_input.index)
                    {
                        show_token_list(cur_input.start.opt(), cur_input.loc.opt(), 100000);
                    } else {
                        show_token_list(
                            MEM[cur_input.start as usize].b32.s1.opt(),
                            cur_input.loc.opt(),
                            100_000,
                        );
                    }
                }
                selector = old_setting_0;
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
        BASE_PTR -= 1
    }
    cur_input = INPUT_STACK[INPUT_PTR];
}
pub(crate) unsafe fn begin_token_list(p: usize, t: Btl) {
    if INPUT_PTR > MAX_IN_STACK {
        MAX_IN_STACK = INPUT_PTR;
        if INPUT_PTR == STACK_SIZE {
            overflow("input stack size", STACK_SIZE);
        }
    }
    INPUT_STACK[INPUT_PTR] = cur_input;
    INPUT_PTR += 1;
    cur_input.state = InputState::TokenList;
    cur_input.start = p as i32;
    cur_input.index = t;
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
            cur_input.limit = PARAM_PTR as i32
        } else {
            cur_input.loc = *LLIST_link(p);
            if *INTPAR(IntPar::tracing_macros) > 1i32 {
                begin_diagnostic();
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
                end_diagnostic(false);
            }
        }
    } else {
        cur_input.loc = p as i32;
    };
}
pub(crate) unsafe fn end_token_list() {
    if ![Btl::Parameter, Btl::UTemplate, Btl::VTemplate].contains(&cur_input.index) {
        if [Btl::BackedUp, Btl::BackedUpChar, Btl::Inserted].contains(&cur_input.index) {
            flush_list(cur_input.start.opt());
        } else {
            delete_token_ref(cur_input.start as usize);
            if cur_input.index == Btl::Macro {
                while PARAM_PTR as i32 > cur_input.limit {
                    PARAM_PTR -= 1;
                    flush_list(PARAM_STACK[PARAM_PTR].opt());
                }
            }
        }
    } else if cur_input.index == Btl::UTemplate {
        if align_state > 500000 {
            align_state = 0
        } else {
            fatal_error("(interwoven alignment preambles are not allowed)");
        }
    }
    INPUT_PTR -= 1;
    cur_input = INPUT_STACK[INPUT_PTR];
}
pub(crate) unsafe fn back_input() {
    while cur_input.state == InputState::TokenList
        && cur_input.loc.opt().is_none()
        && cur_input.index != Btl::VTemplate
    {
        end_token_list();
    }
    let p = get_avail();
    MEM[p].b32.s0 = cur_tok;
    if cur_tok < RIGHT_BRACE_LIMIT {
        if cur_tok < LEFT_BRACE_LIMIT {
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
    INPUT_STACK[INPUT_PTR] = cur_input;
    INPUT_PTR += 1;
    cur_input.state = InputState::TokenList;
    cur_input.start = p as i32;
    cur_input.index = Btl::BackedUp;
    cur_input.loc = p as i32;
}
pub(crate) unsafe fn back_error() {
    back_input();
    error();
}
pub(crate) unsafe fn ins_error() {
    back_input();
    cur_input.index = Btl::Inserted;
    error();
}
pub(crate) unsafe fn begin_file_reading() {
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
    INPUT_STACK[INPUT_PTR] = cur_input;
    INPUT_PTR += 1;
    cur_input.index = Btl::from(IN_OPEN as u16);
    SOURCE_FILENAME_STACK[cur_input.index as usize] = 0;
    FULL_SOURCE_FILENAME_STACK[cur_input.index as usize] = 0;
    EOF_SEEN[cur_input.index as usize] = false;
    GRP_STACK[cur_input.index as usize] = cur_boundary;
    IF_STACK[cur_input.index as usize] = cond_ptr;
    LINE_STACK[cur_input.index as usize] = line;
    cur_input.start = first;
    cur_input.state = InputState::MidLine;
    cur_input.name = 0;
    cur_input.synctex_tag = 0;
}
pub(crate) unsafe fn end_file_reading() {
    first = cur_input.start;
    line = LINE_STACK[cur_input.index as usize];
    if cur_input.name == 18 || cur_input.name == 19 {
        pseudo_close();
    } else if cur_input.name > 17 {
        u_close(INPUT_FILE[cur_input.index as usize]);
    }
    INPUT_PTR -= 1;
    cur_input = INPUT_STACK[INPUT_PTR];
    IN_OPEN -= 1;
}
pub(crate) unsafe fn check_outer_validity() {
    if scanner_status != ScannerStatus::Normal {
        deletions_allowed = false;
        if cur_cs != 0 {
            if cur_input.state == InputState::TokenList || cur_input.name < 1 || cur_input.name > 17
            {
                let p = get_avail();
                MEM[p].b32.s0 = CS_TOKEN_FLAG + cur_cs;
                begin_token_list(p, Btl::BackedUp);
            }
            cur_cmd = Cmd::Spacer;
            cur_chr = ' ' as i32
        }
        if scanner_status != ScannerStatus::Normal && scanner_status != ScannerStatus::Skipping {
            /*350:*/
            runaway();
            if cur_cs == 0 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("File ended");
            } else {
                cur_cs = 0;
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
            begin_token_list(p, Btl::Inserted);
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
            let help2 = if cur_cs != 0 {
                cur_cs = 0;
                &"A forbidden control sequence occurred in skipped text."[..]
            } else {
                &"The file ended while I was skipping conditional text."[..]
            };
            help!(
                help2,
                "This kind of error happens when you say `\\if...\' and forget",
                "the matching `\\fi\'. I\'ve inserted a `\\fi\'; this might work."
            );
            cur_tok = CS_TOKEN_FLAG + FROZEN_FI as i32;
            ins_error();
        }
        deletions_allowed = true
    };
}
/* These macros are kinda scary, but convenient */
pub(crate) unsafe fn get_next() {
    let mut current_block: u64;
    'c_63502: loop {
        cur_cs = 0;
        if cur_input.state != InputState::TokenList {
            /*355:*/
            'c_63807: loop
            /*357:*/
            {
                if cur_input.loc <= cur_input.limit {
                    cur_chr = BUFFER[cur_input.loc as usize];
                    cur_input.loc += 1;
                    if cur_chr >= 0xd800
                        && cur_chr < 0xdc00
                        && cur_input.loc <= cur_input.limit
                        && BUFFER[cur_input.loc as usize] >= 0xdc00
                        && BUFFER[cur_input.loc as usize] < 0xe000
                    {
                        let lower = (BUFFER[cur_input.loc as usize] - 0xdc00) as UTF16_code;
                        cur_input.loc += 1;
                        cur_chr =
                            (0x1_0000 + ((cur_chr - 0xd800) * 1024) as i64 + lower as i64) as i32
                    }
                    'c_65186: loop {
                        cur_cmd = Cmd::from(*CAT_CODE(cur_chr as usize) as u16);
                        match (cur_input.state, cur_cmd) {
                            (InputState::MidLine, IGNORE)
                            | (InputState::SkipBlanks, IGNORE)
                            | (InputState::NewLine, IGNORE)
                            | (InputState::SkipBlanks, Cmd::Spacer)
                            | (InputState::NewLine, Cmd::Spacer) => break,
                            (InputState::MidLine, ESCAPE)
                            | (InputState::SkipBlanks, ESCAPE)
                            | (InputState::NewLine, ESCAPE) => {
                                if cur_input.loc > cur_input.limit {
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
                                cur_cs = cur_chr + 1;
                                cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                                cur_chr = EQTB[cur_cs as usize].val;
                                cur_input.state = InputState::MidLine;
                                if cur_cmd >= Cmd::OuterCall {
                                    check_outer_validity();
                                }
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::SupMark)
                            | (InputState::SkipBlanks, Cmd::SupMark)
                            | (InputState::NewLine, Cmd::SupMark) => {
                                if !(cur_chr == BUFFER[cur_input.loc as usize]) {
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                }
                                if !(cur_input.loc < cur_input.limit) {
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                }
                                let mut sup_count = 2;
                                while sup_count < 6
                                    && cur_input.loc + 2 * sup_count as i32 - 2 <= cur_input.limit
                                    && cur_chr
                                        == BUFFER[(cur_input.loc + sup_count as i32 - 1) as usize]
                                {
                                    sup_count += 1
                                }
                                for d in 1..=sup_count as i32 {
                                    if !(BUFFER[(cur_input.loc + sup_count as i32 - 2 + d as i32)
                                        as usize]
                                        >= '0' as i32
                                        && BUFFER[(cur_input.loc + sup_count as i32 - 2 + d as i32)
                                            as usize]
                                            <= '9' as i32
                                        || BUFFER[(cur_input.loc + sup_count as i32 - 2 + d as i32)
                                            as usize]
                                            >= 'a' as i32
                                            && BUFFER[(cur_input.loc + sup_count as i32 - 2
                                                + d as i32)
                                                as usize]
                                                <= 'f' as i32)
                                    {
                                        let c = BUFFER[(cur_input.loc + 1) as usize];
                                        if !(c < 128) {
                                            current_block = 8567661057257693057;
                                            break 'c_63807;
                                        }
                                        cur_input.loc = cur_input.loc + 2;
                                        cur_chr = if c < 64 { c + 64 } else { c - 64 };
                                        continue 'c_65186;
                                    }
                                }
                                cur_chr = 0i32;
                                for d in 1..=sup_count as i32 {
                                    let c = BUFFER[(cur_input.loc + sup_count as i32 - 2 + d as i32)
                                        as usize];
                                    if c <= '9' as i32 {
                                        cur_chr = 16 * cur_chr + c - '0' as i32
                                    } else {
                                        cur_chr = 16 * cur_chr + c - 'a' as i32 + 10
                                    }
                                }
                                if cur_chr > BIGGEST_USV as i32 {
                                    cur_chr = BUFFER[cur_input.loc as usize];
                                    current_block = 8567661057257693057;
                                    break 'c_63807;
                                } else {
                                    cur_input.loc = cur_input.loc + 2i32 * sup_count as i32 - 1i32
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
                                cur_input.state = InputState::SkipBlanks;
                                cur_chr = ' ' as i32;
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::CarRet) => {
                                cur_input.loc = cur_input.limit + 1;
                                cur_cmd = Cmd::Spacer;
                                cur_chr = ' ' as i32;
                                current_block = 14956172121224201915;
                                break 'c_63807;
                            }
                            (InputState::MidLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::Comment)
                            | (InputState::NewLine, Cmd::Comment)
                            | (InputState::SkipBlanks, Cmd::CarRet) => {
                                cur_input.loc = cur_input.limit + 1;
                                break;
                            }
                            (InputState::NewLine, Cmd::CarRet) => {
                                cur_input.loc = cur_input.limit + 1;
                                cur_cs = par_loc;
                                cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                                cur_chr = EQTB[cur_cs as usize].val;
                                if cur_cmd >= Cmd::OuterCall {
                                    check_outer_validity();
                                }
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
                                cur_input.state = InputState::MidLine;
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
                                cur_input.state = InputState::MidLine;
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
                                cur_input.state = InputState::MidLine;
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
                    cur_input.state = InputState::NewLine;
                    if cur_input.name > 17 {
                        /*374:*/
                        line += 1; /*367:*/
                        first = cur_input.start;
                        if !force_eof {
                            if cur_input.name <= 19 {
                                if pseudo_input() {
                                    cur_input.limit = last
                                } else if let Some(l) = LOCAL(Local::every_eof)
                                    .opt()
                                    .filter(|_| !EOF_SEEN[cur_input.index as usize])
                                {
                                    cur_input.limit = first - 1;
                                    EOF_SEEN[cur_input.index as usize] = true;
                                    begin_token_list(l, Btl::EveryEOFText);
                                    continue 'c_63502;
                                } else {
                                    force_eof = true
                                }
                            } else if input_line(INPUT_FILE[cur_input.index as usize]) {
                                cur_input.limit = last
                            } else if let Some(l) = LOCAL(Local::every_eof)
                                .opt()
                                .filter(|_| !EOF_SEEN[cur_input.index as usize])
                            {
                                cur_input.limit = first - 1;
                                EOF_SEEN[cur_input.index as usize] = true;
                                begin_token_list(l, Btl::EveryEOFText);
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
                                    file_warning();
                                }
                            }
                            if cur_input.name >= 19 {
                                print_chr(')');
                                open_parens -= 1;
                                rust_stdout.as_mut().unwrap().flush().unwrap();
                            }
                            force_eof = false;
                            end_file_reading();
                            check_outer_validity();
                            continue 'c_63502;
                        } else {
                            if *INTPAR(IntPar::end_line_char) < 0
                                || *INTPAR(IntPar::end_line_char) > 255
                            {
                                cur_input.limit -= 1
                            } else {
                                BUFFER[cur_input.limit as usize] = *INTPAR(IntPar::end_line_char)
                            }
                            first = cur_input.limit + 1;
                            cur_input.loc = cur_input.start
                        }
                    } else {
                        if cur_input.name != 0 {
                            cur_cmd = Cmd::Relax;
                            cur_chr = 0;
                            return;
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
                            cur_input.state = InputState::MidLine;
                            current_block = 14956172121224201915;
                        }
                        7720778817628725688 => {
                            let mut k;
                            let mut cat;
                            'c_65963: loop {
                                k = cur_input.loc;
                                cur_chr = BUFFER[k as usize];
                                cat = Cmd::from(*CAT_CODE(cur_chr as usize) as u16);
                                k += 1;
                                if cat == Cmd::Letter {
                                    cur_input.state = InputState::SkipBlanks; // TODO: check
                                } else if cat == Cmd::Spacer {
                                    cur_input.state = InputState::SkipBlanks;
                                } else {
                                    cur_input.state = InputState::MidLine;
                                }
                                if cat == Cmd::Letter && k <= cur_input.limit {
                                    loop
                                    /*368:*/
                                    {
                                        cur_chr = BUFFER[k as usize];
                                        cat = Cmd::from(*CAT_CODE(cur_chr as usize) as u16);
                                        k += 1;
                                        if !(cat == Cmd::Letter && k <= cur_input.limit) {
                                            break;
                                        }
                                    }
                                    if !(cat == Cmd::SupMark
                                        && BUFFER[k as usize] == cur_chr
                                        && k < cur_input.limit)
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
                                        && k + 2 * sup_count as i32 - 2 <= cur_input.limit
                                        && BUFFER[(k + sup_count as i32 - 1) as usize] == cur_chr
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
                                                cur_input.limit = cur_input.limit - d as i32;
                                                while k <= cur_input.limit {
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

                                    cur_chr = 0;

                                    for d in 1..=sup_count as i32 {
                                        let c =
                                            BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize];
                                        cur_chr = if c <= '9' as i32 {
                                            16 * cur_chr + c - '0' as i32
                                        } else {
                                            16 * cur_chr + c - 'a' as i32 + 10
                                        };
                                    }

                                    if cur_chr > BIGGEST_USV as i32 {
                                        cur_chr = BUFFER[k as usize];
                                        current_block = 5873035170358615968;
                                        break;
                                    } else {
                                        BUFFER[(k - 1) as usize] = cur_chr;
                                        let d = (2 * sup_count as i32 - 1) as i16;
                                        cur_input.limit = cur_input.limit - d as i32;
                                        while k <= cur_input.limit {
                                            BUFFER[k as usize] = BUFFER[(k + d as i32) as usize];
                                            k += 1
                                        }
                                    }
                                } else {
                                    if !(cat == Cmd::SupMark
                                        && BUFFER[k as usize] == cur_chr
                                        && k < cur_input.limit)
                                    {
                                        current_block = 1604201581803946138;
                                        break;
                                    }
                                    let mut sup_count_save_0: i32 = 0;
                                    let mut sup_count = 2;
                                    while sup_count < 6
                                        && k + 2 * sup_count as i32 - 2 <= cur_input.limit
                                        && BUFFER[(k + sup_count as i32 - 1) as usize] == cur_chr
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
                                                cur_input.limit = cur_input.limit - d as i32;
                                                while k <= cur_input.limit {
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
                                    cur_chr = 0;
                                    for d in 1..=sup_count as i32 {
                                        let c =
                                            BUFFER[(k + sup_count as i32 - 2 + d as i32) as usize];
                                        if c <= '9' as i32 {
                                            cur_chr = 16 * cur_chr + c - '0' as i32
                                        } else {
                                            cur_chr = 16 * cur_chr + c - 'a' as i32 + 10
                                        }
                                    }

                                    if cur_chr > BIGGEST_USV as i32 {
                                        cur_chr = BUFFER[k as usize];
                                        current_block = 1604201581803946138;
                                        break;
                                    } else {
                                        BUFFER[(k - 1) as usize] = cur_chr;
                                        let d = (2 * sup_count as i32 - 1) as i16;
                                        cur_input.limit = cur_input.limit - d as i32;
                                        while k <= cur_input.limit {
                                            BUFFER[k as usize] = BUFFER[(k + d as i32) as usize];
                                            k += 1
                                        }
                                    }
                                }
                            }
                            match current_block {
                                5873035170358615968 => {
                                    if cat != Cmd::Letter {
                                        k -= 1
                                    }
                                    if k > cur_input.loc + 1 {
                                        cur_cs = id_lookup(cur_input.loc, k - cur_input.loc);
                                        cur_input.loc = k;
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
                                    if BUFFER[cur_input.loc as usize] as i64 > 65535 {
                                        cur_cs = id_lookup(cur_input.loc, 1i32);
                                        cur_input.loc += 1
                                    } else {
                                        cur_cs = 1i32
                                            + (0x10ffffi32 + 1i32)
                                            + BUFFER[cur_input.loc as usize];
                                        cur_input.loc += 1
                                    }
                                    current_block = 10802200937357087535;
                                }
                            }
                        }
                        17833034027772472439 => {
                            cur_cs = 1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32);
                            current_block = 10802200937357087535;
                        }
                        4001239642700071046 => {
                            end_file_reading();
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
                                    begin_token_list(l, Btl::TectonicCodaText);
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
                            cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                            cur_chr = EQTB[cur_cs as usize].val;
                            if cur_cmd >= Cmd::OuterCall {
                                check_outer_validity();
                            }
                        }
                    }
                }
            }
        } else if let Some(loc) = cur_input.loc.opt() {
            /* if we're inputting from a non-null token list: */
            let t = MEM[loc].b32.s0;
            cur_input.loc = *LLIST_link(loc);
            if t >= CS_TOKEN_FLAG {
                cur_cs = t - CS_TOKEN_FLAG;
                cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                cur_chr = EQTB[cur_cs as usize].val;
                if cur_cmd >= Cmd::OuterCall {
                    if cur_cmd == Cmd::DontExpand {
                        /*370:*/
                        cur_cs = MEM[cur_input.loc.opt().unwrap()].b32.s0 - CS_TOKEN_FLAG;
                        cur_input.loc = None.tex_int();
                        cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                        cur_chr = EQTB[cur_cs as usize].val;
                        if cur_cmd > MAX_COMMAND {
                            cur_cmd = Cmd::Relax;
                            cur_chr = NO_EXPAND_FLAG;
                        }
                    } else {
                        check_outer_validity();
                    }
                }
            } else {
                cur_cmd = Cmd::from((t / MAX_CHAR_VAL) as u16);
                cur_chr = t % MAX_CHAR_VAL;
                match cur_cmd {
                    Cmd::LeftBrace => {
                        current_block = 17818108259648334471;
                        align_state += 1;
                    }
                    Cmd::RightBrace => {
                        current_block = 1336783539463924428;
                        align_state -= 1;
                    }
                    OUT_PARAM => {
                        current_block = 1132450443677887731;
                        begin_token_list(
                            PARAM_STACK[(cur_input.limit + cur_chr - 1) as usize] as usize,
                            Btl::Parameter,
                        );
                        continue;
                    }
                    _ => {}
                }
            }
        } else {
            end_token_list();
            continue;
        }
        if cur_cmd <= Cmd::CarRet && cur_cmd >= Cmd::TabMark && align_state == 0 {
            /*818:*/
            if scanner_status == ScannerStatus::Aligning {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
            if let Some(ca) = cur_align {
                cur_cmd = Cmd::from(MEM[ca + 5].b32.s0 as u16);
                MEM[ca + 5].b32.s0 = cur_chr;
                if cur_cmd == Cmd::Omit {
                    begin_token_list(OMIT_TEMPLATE, Btl::VTemplate);
                } else {
                    begin_token_list(MEM[ca + 2].b32.s1 as usize, Btl::VTemplate);
                }
                align_state = 1000000;
            } else {
                fatal_error("(interwoven alignment preambles are not allowed)");
            }
        } else {
            return;
        }
    }
}
pub(crate) unsafe fn get_token() {
    no_new_control_sequence = false;
    get_next();
    no_new_control_sequence = true;
    if cur_cs == 0 {
        cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
    } else {
        cur_tok = CS_TOKEN_FLAG + cur_cs
    };
}
pub(crate) unsafe fn macro_call() {
    let mut p: i32 = None.tex_int();
    let mut rbrace_ptr: i32 = None.tex_int();
    let mut match_chr: UTF16_code = 0;
    let save_scanner_status = scanner_status;
    let save_warning_index = warning_index;
    warning_index = cur_cs;
    let ref_count = cur_chr as usize;
    let mut r = llist_link(ref_count).unwrap();
    let mut n = 0_i16;
    if *INTPAR(IntPar::tracing_macros) > 0 {
        /*419:*/
        begin_diagnostic();
        print_ln();
        print_cs(warning_index);
        token_show(Some(ref_count));
        end_diagnostic(false);
    }
    if MEM[r].b32.s0 == PROTECTED_TOKEN {
        r = llist_link(r).unwrap();
    }
    if MEM[r].b32.s0 != END_MATCH_TOKEN {
        /*409:*/
        scanner_status = ScannerStatus::Matching;
        let mut unbalance = 0;
        long_state = EQTB[cur_cs as usize].cmd as u8;

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

            get_token();
            if cur_tok == MEM[r].b32.s0 {
                /*412:*/
                r = llist_link(r).unwrap();
                if MEM[r].b32.s0 >= MATCH_TOKEN && MEM[r].b32.s0 <= END_MATCH_TOKEN {
                    if cur_tok < LEFT_BRACE_LIMIT {
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
                                    if cur_tok != MEM[v].b32.s0 {
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

                if cur_tok == par_token {
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
                            back_error();
                        }

                        pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                        align_state = align_state - unbalance;

                        for m in 0..=(n as usize) {
                            flush_list(pstack[m].opt());
                        }

                        return exit(save_scanner_status, save_warning_index);
                    }
                }

                if cur_tok < RIGHT_BRACE_LIMIT {
                    if cur_tok < LEFT_BRACE_LIMIT {
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
                            MEM[q].b32.s0 = cur_tok;
                            p = q as i32;

                            get_token();

                            if cur_tok == par_token {
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
                                        back_error();
                                    }

                                    pstack[n as usize] = *LLIST_link(TEMP_HEAD);
                                    align_state = align_state - unbalance;

                                    for m in 0..=(n as usize) {
                                        flush_list(pstack[m].opt());
                                    }

                                    return exit(save_scanner_status, save_warning_index);
                                }
                            }

                            if cur_tok < RIGHT_BRACE_LIMIT {
                                if cur_tok < LEFT_BRACE_LIMIT {
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
                        MEM[q].b32.s0 = cur_tok;
                        p = q as i32;
                    } else {
                        /* 413 */
                        back_input();

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
                        cur_tok = par_token;
                        ins_error();
                        cont = true;
                        continue;
                    }
                } else {
                    if cur_tok == SPACE_TOKEN {
                        if MEM[r].b32.s0 <= END_MATCH_TOKEN {
                            if MEM[r].b32.s0 >= MATCH_TOKEN {
                                cont = true;
                                continue;
                            }
                        }
                    }

                    let q = get_avail();
                    *LLIST_link(p as usize) = Some(q).tex_int();
                    MEM[q].b32.s0 = cur_tok;
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
                    begin_diagnostic();
                    print_nl(match_chr as str_number);
                    print_int(n as i32);
                    print_cstr("<-");
                    show_token_list(pstack[(n as i32 - 1) as usize].opt(), None, 1000);
                    end_diagnostic(false);
                }
            }
            if !(MEM[r].b32.s0 != END_MATCH_TOKEN) {
                break;
            }
        }
    }

    while cur_input.state == InputState::TokenList
        && cur_input.loc.opt().is_none()
        && cur_input.index != Btl::VTemplate
    {
        end_token_list();
    }

    begin_token_list(ref_count, Btl::Macro);
    cur_input.name = warning_index;
    cur_input.loc = *LLIST_link(r);

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
pub(crate) unsafe fn insert_relax() {
    cur_tok = CS_TOKEN_FLAG + cur_cs;
    back_input();
    cur_tok = CS_TOKEN_FLAG + FROZEN_RELAX as i32;
    back_input();
    cur_input.index = Btl::Inserted;
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
pub(crate) unsafe fn expand() {
    let mut b: bool = false;
    let mut j: i32 = 0;
    expand_depth_count += 1;
    if expand_depth_count >= expand_depth {
        overflow("expansion depth", expand_depth as usize);
    }
    let cv_backup = cur_val;
    let cvl_backup = cur_val_level;
    let radix_backup = radix;
    let co_backup = cur_order;
    let backup_backup = *LLIST_link(BACKUP_HEAD);
    loop {
        if cur_cmd < Cmd::Call {
            /*384:*/
            if *INTPAR(IntPar::tracing_commands) > 1 {
                show_cur_cmd_chr(); /*1612:*/
            }
            match cur_cmd {
                Cmd::TopBotMark => {
                    let t = (cur_chr % 5) as usize;
                    if cur_chr >= 5 {
                        scan_register_num();
                    } else {
                        cur_val = 0;
                    }
                    cur_ptr = if cur_val == 0 {
                        cur_mark[t as usize]
                    } else {
                        find_sa_element(ValLevel::Mark, cur_val, false);
                        cur_ptr.and_then(|p| MarkClass(p).indexes()[t].opt())
                    };
                    if let Some(p) = cur_ptr {
                        begin_token_list(p, Btl::MarkText);
                    }
                    break;
                }
                Cmd::ExpandAfter => {
                    /*385:*/
                    if cur_chr == 0 {
                        get_token(); /*1553: "\unless" implementation */
                        let t = cur_tok;
                        get_token();
                        if cur_cmd > MAX_COMMAND {
                            expand();
                        } else {
                            back_input();
                        }
                        cur_tok = t;
                        back_input();
                        break;
                    } else {
                        get_token();
                        if cur_cmd == Cmd::IfTest && cur_chr != IfTestCode::IfCase as i32 {
                            cur_chr = cur_chr + UNLESS_CODE
                        } else {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("You can\'t use `");
                            print_esc_cstr("unless");
                            print_cstr("\' before `");
                            print_cmd_chr(cur_cmd, cur_chr);
                            print_chr('\'');
                            help!("Continue, and I\'ll forget that it ever happened.");
                            back_error();
                            break;
                        }
                    }
                }
                Cmd::NoExpand => {
                    /*386:*/
                    if cur_chr == 0 {
                        let save_scanner_status = scanner_status; /*387: \primitive implementation */
                        scanner_status = ScannerStatus::Normal;
                        get_token();
                        scanner_status = save_scanner_status;
                        let t = cur_tok;
                        back_input();
                        if t >= CS_TOKEN_FLAG {
                            let p = get_avail();
                            MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_DONT_EXPAND as i32;
                            *LLIST_link(p) = cur_input.loc;
                            cur_input.start = p as i32;
                            cur_input.loc = p as i32;
                        }
                        break;
                    } else {
                        let save_scanner_status = scanner_status;
                        scanner_status = ScannerStatus::Normal;
                        get_token();
                        scanner_status = save_scanner_status;
                        if cur_cs < HASH_BASE as i32 {
                            cur_cs = prim_lookup(cur_cs - SINGLE_BASE as i32) as i32
                        } else {
                            cur_cs = prim_lookup((*hash.offset(cur_cs as isize)).s1) as i32
                        }
                        if !(cur_cs != UNDEFINED_PRIMITIVE) {
                            break;
                        }
                        let t = prim_eqtb[cur_cs as usize].cmd as i32;
                        if t > MAX_COMMAND as i32 {
                            cur_cmd = Cmd::from(t as u16);
                            cur_chr = prim_eqtb[cur_cs as usize].val;
                            cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                            cur_cs = 0;
                        } else {
                            back_input();
                            let p = get_avail();
                            MEM[p].b32.s0 = CS_TOKEN_FLAG + FROZEN_PRIMITIVE as i32;
                            *LLIST_link(p) = cur_input.loc;
                            cur_input.loc = Some(p).tex_int();
                            cur_input.start = Some(p).tex_int();
                            break;
                        }
                    }
                }
                Cmd::CSName => {
                    let r = get_avail();
                    let mut p = r;
                    b = is_in_csname;
                    is_in_csname = true;
                    loop {
                        get_x_token();
                        if cur_cs == 0 {
                            let q = get_avail();
                            *LLIST_link(p) = Some(q).tex_int();
                            MEM[q].b32.s0 = cur_tok;
                            p = q;
                        }
                        if !(cur_cs == 0) {
                            break;
                        }
                    }
                    if cur_cmd != Cmd::EndCSName {
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
                        back_error();
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
                    if j > first + 1 || BUFFER[first as usize] as i64 > 65535 {
                        no_new_control_sequence = false;
                        cur_cs = id_lookup(first, j - first);
                        no_new_control_sequence = true
                    } else if j == first {
                        cur_cs = NULL_CS as i32;
                    } else {
                        cur_cs = SINGLE_BASE as i32 + BUFFER[first as usize];
                        /*:392*/
                    }
                    flush_list(Some(r));
                    if Cmd::from(EQTB[cur_cs as usize].cmd) == Cmd::UndefinedCS {
                        eq_define(cur_cs as usize, Cmd::Relax, Some(TOO_BIG_USV));
                    }
                    cur_tok = cur_cs + CS_TOKEN_FLAG;
                    back_input();
                    break;
                }
                Cmd::Convert => {
                    conv_toks();
                    break;
                }
                Cmd::The => {
                    ins_the_toks();
                    break;
                }
                Cmd::IfTest => {
                    conditional();
                    break;
                }
                Cmd::FiOrElse => {
                    if *INTPAR(IntPar::tracing_ifs) > 0 {
                        if *INTPAR(IntPar::tracing_commands) <= 1i32 {
                            show_cur_cmd_chr();
                        }
                    }
                    if cur_chr > if_limit as i32 {
                        if if_limit == FiOrElseCode::If {
                            insert_relax();
                        } else {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("Extra ");
                            print_cmd_chr(Cmd::FiOrElse, cur_chr);
                            help!("I\'m ignoring this; it doesn\'t match any \\if.");
                            error();
                        }
                    } else {
                        while cur_chr != FiOrElseCode::Fi as i32 {
                            pass_text();
                        }
                        if IF_STACK[IN_OPEN] == cond_ptr {
                            if_warning();
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
                    if cur_chr == 1 {
                        /* \endinput */
                        force_eof = true
                    } else if cur_chr == 2 {
                        /*1537:*/
                        /* \scantokens */
                        pseudo_start();
                    } else if name_in_progress {
                        insert_relax();
                    } else {
                        /* \input */
                        start_input(ptr::null()); /*393:*/
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
            if cur_cmd < Cmd::EndTemplate {
                macro_call();
            } else {
                cur_tok = CS_TOKEN_FLAG + FROZEN_ENDV as i32;
                back_input();
            }
            break;
        }
    }
    cur_val = cv_backup;
    cur_val_level = cvl_backup;
    radix = radix_backup;
    cur_order = co_backup;
    *LLIST_link(BACKUP_HEAD) = backup_backup;
    expand_depth_count -= 1;
}
pub(crate) unsafe fn get_x_token() {
    loop {
        get_next();
        if cur_cmd <= MAX_COMMAND {
            break;
        }
        if cur_cmd >= Cmd::Call {
            if cur_cmd < Cmd::EndTemplate {
                macro_call();
            } else {
                cur_cs = FROZEN_ENDV as i32;
                cur_cmd = Cmd::EndV;
                break;
            }
        } else {
            expand();
        }
    }
    cur_tok = if cur_cs == 0 {
        cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
    } else {
        CS_TOKEN_FLAG + cur_cs
    };
}
pub(crate) unsafe fn x_token() {
    while cur_cmd > MAX_COMMAND {
        expand();
        get_next();
    }
    cur_tok = if cur_cs == 0 {
        cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
    } else {
        CS_TOKEN_FLAG + cur_cs
    };
}
pub(crate) unsafe fn scan_left_brace() {
    loop {
        get_x_token();
        if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
            break;
        }
    }
    if cur_cmd != Cmd::LeftBrace {
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
        back_error();
        cur_tok = LEFT_BRACE_TOKEN + '{' as i32;
        cur_cmd = Cmd::LeftBrace;
        cur_chr = '{' as i32;
        align_state += 1
    };
}
pub(crate) unsafe fn scan_optional_equals() {
    loop {
        get_x_token();
        if !(cur_cmd == Cmd::Spacer) {
            break;
        }
    }
    if cur_tok != OTHER_TOKEN + 61 {
        /*"="*/
        back_input();
    };
}

pub(crate) unsafe fn scan_keyword(s: &[u8]) -> bool {
    let mut p = BACKUP_HEAD;
    *LLIST_link(p) = None.tex_int();
    if s.len() == 1 {
        let mut c: i8 = s[0] as i8;
        loop {
            get_x_token();
            if cur_cs == 0 && (cur_chr == c as i32 || cur_chr == c as i32 - 32) {
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = cur_tok;
                p = q;
                flush_list(llist_link(BACKUP_HEAD));
                return true;
            } else {
                if cur_cmd != Cmd::Spacer || p != BACKUP_HEAD {
                    back_input();
                    if p != BACKUP_HEAD {
                        begin_token_list(*LLIST_link(BACKUP_HEAD) as usize, Btl::BackedUp);
                    }
                    return false;
                }
            }
        }
    }
    let slen = s.len();
    let mut i = 0;
    while i < slen {
        get_x_token();
        if cur_cs == 0 && (cur_chr == s[i] as i8 as i32 || cur_chr == s[i] as i8 as i32 - 32) {
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = cur_tok;
            p = q;
            i += 1;
        } else if cur_cmd != Cmd::Spacer || p != BACKUP_HEAD {
            back_input();
            if p != BACKUP_HEAD {
                begin_token_list(*LLIST_link(BACKUP_HEAD) as usize, Btl::BackedUp);
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
pub(crate) unsafe fn scan_glyph_number(mut f: internal_font_number) {
    if scan_keyword(b"/") {
        scan_and_pack_name();
        cur_val = map_glyph_to_index(f);
        cur_val_level = ValLevel::Int;
    } else if scan_keyword(b"u") {
        scan_char_num();
        cur_val = map_char_to_glyph(f, cur_val);
        cur_val_level = ValLevel::Int;
    } else {
        scan_int();
    };
}
pub(crate) unsafe fn scan_char_class() {
    scan_int();
    if cur_val < 0 || cur_val > CHAR_CLASS_LIMIT {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_char_class_not_ignored() {
    scan_int();
    if cur_val < 0 || cur_val > CHAR_CLASS_LIMIT {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_eight_bit_int() {
    scan_int();
    if cur_val < 0 || cur_val > 255 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_usv_num() {
    scan_int();
    if cur_val < 0 || cur_val > BIGGEST_USV as i32 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_char_num() {
    scan_int();
    if cur_val < 0 || cur_val > BIGGEST_CHAR {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_xetex_math_char_int() {
    scan_int();
    if math_char(cur_val) == ACTIVE_MATH_CHAR as u32 {
        if cur_val != ACTIVE_MATH_CHAR {
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
            int_error(cur_val);
            cur_val = ACTIVE_MATH_CHAR;
        }
    } else if math_char(cur_val) as u32 > BIGGEST_USV as u32 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_math(m: &mut MCell, p: usize) {
    let mut c: i32 = 0;
    'c_118470: loop {
        loop
        /*422:*/
        {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
                break;
            }
        }
        loop {
            match cur_cmd {
                Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {
                    c = *MATH_CODE(cur_chr as usize);
                    if !(math_char(c) == ACTIVE_MATH_CHAR as u32) {
                        break 'c_118470;
                    }
                    cur_cs = cur_chr + 1;
                    cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd);
                    cur_chr = EQTB[cur_cs as usize].val;
                    x_token();
                    back_input();
                    break;
                }
                Cmd::CharNum => {
                    scan_char_num();
                    cur_chr = cur_val;
                    cur_cmd = Cmd::CharGiven;
                }
                Cmd::MathCharNum => {
                    if cur_chr == 2 {
                        scan_math_class_int();
                        c = set_class(cur_val);
                        scan_math_fam_int();
                        c = c + set_family(cur_val);
                        scan_usv_num();
                        c = c + cur_val;
                    } else if cur_chr == 1 {
                        scan_xetex_math_char_int();
                        c = cur_val
                    } else {
                        scan_fifteen_bit_int();
                        c = set_class(cur_val / 4096)
                            + set_family((cur_val % 4096) / 256)
                            + (cur_val % 256);
                    }
                    break 'c_118470;
                }
                Cmd::MathGiven => {
                    c = set_class(cur_chr / 4096)
                        + set_family((cur_chr % 4096) / 256)
                        + (cur_chr % 256);
                    break 'c_118470;
                }
                Cmd::XetexMathGiven => {
                    c = cur_chr;
                    break 'c_118470;
                }
                Cmd::DelimNum => {
                    if cur_chr == 1 {
                        scan_math_class_int();
                        c = set_class(cur_val);
                        scan_math_fam_int();
                        c = c + set_family(cur_val);
                        scan_usv_num();
                        c = c + cur_val;
                    } else {
                        scan_delimiter_int();
                        c = cur_val / 4096;
                        c = set_class(c / 4096) + set_family((c % 4096) / 256) + (c % 256);
                    }
                    break 'c_118470;
                }
                _ => {
                    back_input();
                    scan_left_brace();
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
pub(crate) unsafe fn set_math_char(mut c: i32) {
    let mut ch: UnicodeScalar = 0;
    if math_char(c) == ACTIVE_MATH_CHAR as u32 {
        /*1187: */
        cur_cs = cur_chr + 1; /* ... "between 0 and 15" */
        cur_cmd = Cmd::from(EQTB[cur_cs as usize].cmd); /* ... "between 0 and 15" */
        cur_chr = EQTB[cur_cs as usize].val;
        x_token();
        back_input();
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
pub(crate) unsafe fn scan_math_class_int() {
    scan_int();
    if cur_val < 0 || cur_val > 7 {
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
        int_error(cur_val);
        cur_val = 0
    };
}
pub(crate) unsafe fn scan_math_fam_int() {
    scan_int();
    if cur_val < 0 || cur_val > NUMBER_MATH_FAMILIES as i32 - 1 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_four_bit_int() {
    scan_int();
    if cur_val < 0 || cur_val > 15 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_fifteen_bit_int() {
    scan_int();
    if cur_val < 0 || cur_val > 32767 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_delimiter_int() {
    scan_int();
    if cur_val < 0 || cur_val > 0x7ffffff {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_register_num() {
    scan_int();
    if cur_val < 0 || cur_val > max_reg_num {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad register code");
        help!(max_reg_help_line, "I changed this one to zero.");
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn scan_four_bit_int_or_18() {
    scan_int();
    if cur_val < 0 || cur_val > 15 && cur_val != 18 {
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
        int_error(cur_val);
        cur_val = 0;
    };
}
pub(crate) unsafe fn get_x_or_protected() {
    loop {
        get_token();
        if cur_cmd <= MAX_COMMAND {
            return;
        }
        if cur_cmd >= Cmd::Call && cur_cmd < Cmd::EndTemplate {
            if MEM[*LLIST_link(cur_chr as usize) as usize].b32.s0 == PROTECTED_TOKEN {
                return;
            }
        }
        expand();
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
pub(crate) unsafe fn scan_font_ident() {
    let mut f: internal_font_number = 0;
    let mut m: i32 = 0;
    loop {
        get_x_token();
        if !(cur_cmd == Cmd::Spacer) {
            break;
        }
    }
    if cur_cmd == Cmd::DefFont {
        f = EQTB[CUR_FONT_LOC].val as usize;
    } else if cur_cmd == Cmd::SetFont {
        f = cur_chr as usize;
    } else if cur_cmd == Cmd::DefFamily {
        m = cur_chr;
        scan_math_fam_int();
        f = EQTB[(m + cur_val) as usize].val as usize
    } else {
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
        back_error();
        f = FONT_BASE;
    }
    cur_val = f as i32;
}
pub(crate) unsafe fn find_font_dimen(writing: bool) {
    let mut n: i32 = 0;
    scan_int();
    n = cur_val;
    scan_font_ident();
    let mut f = cur_val as usize;
    if n <= 0 {
        cur_val = fmem_ptr
    } else {
        if writing && n <= SPACE_SHRINK_CODE && n >= SPACE_CODE {
            if let Some(g) = FONT_GLUE[f].opt() {
                delete_glue_ref(g);
                FONT_GLUE[f] = None.tex_int()
            }
        }
        if n > FONT_PARAMS[f] {
            if f < FONT_PTR {
                cur_val = fmem_ptr
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
                cur_val = fmem_ptr - 1;
            }
        } else {
            cur_val = n + PARAM_BASE[f]
        }
    }
    if cur_val == fmem_ptr {
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
}
pub(crate) unsafe fn scan_something_internal(level: ValLevel, mut negative: bool) {
    let mut n: i32 = 0;
    let mut k: i32 = 0;
    let mut kk: i32 = 0;
    let mut r: i32 = 0;
    let mut i: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    match cur_cmd {
        Cmd::DefCode => {
            let m = cur_chr;
            scan_usv_num();
            if m == MATH_CODE_BASE as i32 {
                cur_val1 = *MATH_CODE(cur_val as usize);
                if math_char(cur_val1) == ACTIVE_MATH_CHAR as u32 {
                    cur_val1 = 0x8000;
                } else if math_class(cur_val1) > 7
                    || math_fam(cur_val1) > 15
                    || math_char(cur_val1) > 255
                {
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
                    int_error(cur_val1);
                    cur_val1 = 0;
                }
                cur_val1 = (math_class(cur_val1) * 0x1000
                    + math_fam(cur_val1) * 0x100
                    + math_char(cur_val1)) as i32;
                cur_val = cur_val1;
                cur_val_level = ValLevel::Int
            } else if m == DEL_CODE_BASE as i32 {
                cur_val1 = EQTB[DEL_CODE_BASE + cur_val as usize].val;
                if cur_val1 >= 0x40000000 {
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
                    cur_val = 0;
                    cur_val_level = ValLevel::Int;
                } else {
                    cur_val = cur_val1;
                    cur_val_level = ValLevel::Int;
                }
            } else if m < SF_CODE_BASE as i32 {
                cur_val = EQTB[(m + cur_val) as usize].val;
                cur_val_level = ValLevel::Int;
            } else if m < MATH_CODE_BASE as i32 {
                cur_val = (EQTB[(m + cur_val) as usize].val as i64 % 65536) as i32;
                cur_val_level = ValLevel::Int;
            } else {
                cur_val = EQTB[(m + cur_val) as usize].val;
                cur_val_level = ValLevel::Int;
            }
        }
        Cmd::XetexDefCode => {
            let m = cur_chr;
            scan_usv_num();
            if m == SF_CODE_BASE as i32 {
                cur_val = (*SF_CODE(cur_val as usize) as i64 / 65536) as i32;
                cur_val_level = ValLevel::Int
            } else if m == MATH_CODE_BASE as i32 {
                cur_val = *MATH_CODE(cur_val as usize);
                cur_val_level = ValLevel::Int
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
                cur_val = 0;
                cur_val_level = ValLevel::Int;
            } else if m == DEL_CODE_BASE as i32 {
                cur_val = EQTB[DEL_CODE_BASE + cur_val as usize].val;
                cur_val_level = ValLevel::Int;
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
                cur_val = 0;
                cur_val_level = ValLevel::Int;
            }
        }
        Cmd::ToksRegister | Cmd::AssignToks | Cmd::DefFamily | Cmd::SetFont | Cmd::DefFont => {
            let m = cur_chr;
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
                back_error();
                cur_val = 0;
                cur_val_level = ValLevel::Dimen;
            } else if cur_cmd <= Cmd::AssignToks {
                if cur_cmd < Cmd::AssignToks {
                    if m == 0i32 {
                        scan_register_num();
                        if cur_val < 256 {
                            cur_val = EQTB[TOKS_BASE + cur_val as usize].val
                        } else {
                            find_sa_element(ValLevel::Tok, cur_val, false);
                            cur_val = cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt()).tex_int();
                        }
                    } else {
                        cur_val = MEM[(m + 1) as usize].b32.s1
                    }
                } else if cur_chr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                    scan_char_class_not_ignored();
                    cur_ptr = cur_val.opt();
                    scan_char_class_not_ignored();
                    find_sa_element(
                        ValLevel::InterChar,
                        cur_ptr.tex_int() * CHAR_CLASS_LIMIT + cur_val,
                        false,
                    );
                    cur_val = if let Some(p) = cur_ptr {
                        MEM[p + 1].b32.s1
                    } else {
                        None.tex_int()
                    };
                } else {
                    cur_val = EQTB[m as usize].val
                }
                cur_val_level = ValLevel::Tok;
            } else {
                back_input();
                scan_font_ident();
                cur_val = FONT_ID_BASE as i32 + cur_val;
                cur_val_level = ValLevel::Ident;
            }
        }
        Cmd::AssignInt => {
            let m = cur_chr;
            cur_val = EQTB[m as usize].val;
            cur_val_level = ValLevel::Int;
        }
        Cmd::AssignDimen => {
            let m = cur_chr;
            cur_val = EQTB[m as usize].val;
            cur_val_level = ValLevel::Dimen;
        }
        Cmd::AssignGlue => {
            let m = cur_chr;
            cur_val = EQTB[m as usize].val;
            cur_val_level = ValLevel::Glue;
        }
        Cmd::AssignMuGlue => {
            let m = cur_chr;
            cur_val = EQTB[m as usize].val;
            cur_val_level = ValLevel::Mu;
        }
        Cmd::SetAux => {
            let m = cur_chr;
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
                    cur_val = 0;
                    cur_val_level = ValLevel::Dimen;
                } else {
                    cur_val = 0;
                    cur_val_level = ValLevel::Int;
                }
            } else if m == ListMode::VMode as i32 {
                cur_val = cur_list.aux.b32.s1;
                cur_val_level = ValLevel::Dimen;
            } else {
                cur_val = cur_list.aux.b32.s0;
                cur_val_level = ValLevel::Int;
            }
        }
        Cmd::SetPrevGraf => {
            if cur_list.mode.1 == ListMode::NoMode {
                cur_val = 0;
                cur_val_level = ValLevel::Int
            } else {
                NEST[NEST_PTR] = cur_list;
                let mut p = NEST_PTR;
                while NEST[p].mode.1 != ListMode::VMode {
                    p -= 1
                }
                cur_val = NEST[p].prev_graf;
                cur_val_level = ValLevel::Int
            }
        }
        Cmd::SetPageInt => {
            let m = cur_chr;
            if m == 0 {
                cur_val = dead_cycles
            } else if m == 2 {
                cur_val = interaction as i32
            } else {
                cur_val = insert_penalties
            }
            cur_val_level = ValLevel::Int
        }
        Cmd::SetPageDimen => {
            let m = cur_chr;
            if page_contents == PageContents::Empty && !output_active {
                if m == 0 {
                    cur_val = MAX_HALFWORD;
                } else {
                    cur_val = 0;
                }
            } else {
                cur_val = page_so_far[m as usize]
            }
            cur_val_level = ValLevel::Dimen
        }
        Cmd::SetShape => {
            let m = cur_chr;
            if m > LOCAL_BASE as i32 + Local::par_shape as i32 {
                /*1654:*/
                scan_int();
                if cur_val < 0 {
                    cur_val = 0;
                } else if let Some(v) = EQTB[m as usize].val.opt() {
                    if cur_val > MEM[v + 1].b32.s1 {
                        cur_val = MEM[v + 1].b32.s1
                    }
                    cur_val = MEM[v + cur_val as usize + 1].b32.s1;
                } else {
                    cur_val = 0;
                }
            } else if let Some(l) = LOCAL(Local::par_shape).opt() {
                cur_val = MEM[l].b32.s0;
            } else {
                cur_val = 0;
            }
            cur_val_level = ValLevel::Int;
        }
        Cmd::SetBoxDimen => {
            let m = cur_chr;
            scan_register_num();
            let q = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                if let Some(p) = cur_ptr {
                    MEM[p + 1].b32.s1.opt()
                } else {
                    None
                }
            };
            cur_val = if let Some(q) = q {
                MEM[q + (m as usize)].b32.s1
            } else {
                0
            };
            cur_val_level = ValLevel::Dimen
        }
        Cmd::CharGiven | Cmd::MathGiven => {
            cur_val = cur_chr;
            cur_val_level = ValLevel::Int
        }
        Cmd::AssignFontDimen => {
            find_font_dimen(false);
            FONT_INFO[fmem_ptr as usize].b32.s1 = 0;
            cur_val = FONT_INFO[cur_val as usize].b32.s1;
            cur_val_level = ValLevel::Dimen
        }
        Cmd::AssignFontInt => {
            let m = cur_chr;
            scan_font_ident();
            if m == 0 {
                cur_val = HYPHEN_CHAR[cur_val as usize];
                cur_val_level = ValLevel::Int
            } else if m == 1 {
                cur_val = SKEW_CHAR[cur_val as usize];
                cur_val_level = ValLevel::Int
            } else {
                n = cur_val;
                if FONT_AREA[n as usize] as u32 == AAT_FONT_FLAG
                    || FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                {
                    scan_glyph_number(n as usize);
                } else {
                    scan_char_num();
                }
                k = cur_val;
                match m {
                    LP_CODE_BASE => {
                        cur_val = get_cp_code(n as usize, k as u32, Side::Left);
                        cur_val_level = ValLevel::Int
                    }
                    RP_CODE_BASE => {
                        cur_val = get_cp_code(n as usize, k as u32, Side::Right);
                        cur_val_level = ValLevel::Int
                    }
                    _ => {}
                }
            }
        }
        Cmd::Register => {
            let m = cur_chr;
            if m < 0 || m > 19 {
                // TODO: may be bug
                /* 19 = "lo_mem_stat_max" */
                cur_val_level = ValLevel::from((MEM[m as usize].b16.s1 as i32 / 64) as u8);
                cur_val = match cur_val_level {
                    ValLevel::Int | ValLevel::Dimen => MEM[(m + 2) as usize].b32.s1,
                    _ => MEM[(m + 1) as usize].b32.s1,
                };
            } else {
                scan_register_num();
                cur_val_level = ValLevel::from(m as u8);
                if cur_val > 255 {
                    find_sa_element(cur_val_level, cur_val, false);
                    cur_val = if let Some(p) = cur_ptr {
                        match cur_val_level {
                            ValLevel::Int | ValLevel::Dimen => MEM[p + 2].b32.s1,
                            _ => MEM[p + 1].b32.s1,
                        }
                    } else {
                        0
                    };
                } else {
                    match cur_val_level {
                        ValLevel::Int => cur_val = *COUNT_REG(cur_val as usize),
                        ValLevel::Dimen => cur_val = *SCALED_REG(cur_val as usize),
                        ValLevel::Glue => cur_val = *SKIP_REG(cur_val as usize),
                        ValLevel::Mu => cur_val = *MU_SKIP_REG(cur_val as usize),
                        _ => {}
                    }
                }
            }
        }
        Cmd::LastItem => {
            let m = LastItemCode::n(cur_chr as u8).unwrap();
            if m >= LastItemCode::InputLineNo {
                if m >= LastItemCode::MuToGlue {
                    /*1568:*/
                    match m {
                        LastItemCode::MuToGlue => {
                            scan_mu_glue(); // 1595:
                            cur_val_level = ValLevel::Glue;
                        }
                        LastItemCode::GlueToMu => {
                            scan_normal_glue(); // 1596:
                            cur_val_level = ValLevel::Mu;
                        }
                        _ => {
                            cur_val_level = match m {
                                LastItemCode::EtexExprInt => ValLevel::Int,
                                LastItemCode::EtexExprDimen => ValLevel::Dimen,
                                LastItemCode::EtexExprGlue => ValLevel::Glue,
                                LastItemCode::EtexExprMu => ValLevel::Mu,
                                _ => unreachable!(),
                            };
                            scan_expr();
                        }
                    }
                    while cur_val_level > level {
                        if cur_val_level == ValLevel::Glue {
                            let m = cur_val as usize;
                            cur_val = MEM[m + 1].b32.s1;
                            delete_glue_ref(m);
                        } else if cur_val_level == ValLevel::Mu {
                            mu_error();
                        }
                        cur_val_level.prev();
                    }
                    if negative {
                        match cur_val_level {
                            ValLevel::Int | ValLevel::Dimen => cur_val = -cur_val,
                            _ => {
                                let m = cur_val as usize;
                                let mut cur_val_ = GlueSpec(new_spec(m));
                                cur_val = cur_val_.ptr() as i32;
                                delete_glue_ref(m);
                                cur_val_.set_size(-cur_val_.size());
                                cur_val_.set_stretch(-cur_val_.stretch());
                                cur_val_.set_shrink(-cur_val_.shrink());
                            }
                        }
                    }
                    return;
                }
                if m >= XETEX_DIM {
                    match m {
                        LastItemCode::XetexGlyphBounds => {
                            /*1435:*/
                            if FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32
                                    == OTGR_FONT_FLAG
                            {
                                scan_int(); /* shellenabledp */
                                n = cur_val;
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
                                    cur_val = 0;
                                } else {
                                    scan_int();
                                    cur_val = get_glyph_bounds(
                                        EQTB[CUR_FONT_LOC].val as usize,
                                        n,
                                        cur_val,
                                    )
                                }
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                cur_val = 0;
                            }
                        }
                        LastItemCode::FontCharWd
                        | LastItemCode::FontCharHt
                        | LastItemCode::FontCharDp
                        | LastItemCode::FontCharIc => {
                            scan_font_ident();
                            let q = cur_val as usize;
                            scan_usv_num();
                            if FONT_AREA[q] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[q] as u32 == OTGR_FONT_FLAG
                            {
                                match m {
                                    LastItemCode::FontCharWd => {
                                        cur_val = getnativecharwd(q, cur_val)
                                    }
                                    LastItemCode::FontCharHt => {
                                        cur_val = getnativecharht(q, cur_val)
                                    }
                                    LastItemCode::FontCharDp => {
                                        cur_val = getnativechardp(q, cur_val)
                                    }
                                    LastItemCode::FontCharIc => {
                                        cur_val = getnativecharic(q, cur_val)
                                    }
                                    _ => unreachable!(),
                                }
                            } else if FONT_BC[q] as i32 <= cur_val && FONT_EC[q] as i32 >= cur_val {
                                i = FONT_CHARACTER_INFO(
                                    q,
                                    effective_char(true, q, cur_val as u16) as usize,
                                );
                                match m {
                                    LastItemCode::FontCharWd => {
                                        cur_val = *FONT_CHARINFO_WIDTH(q, i)
                                    }
                                    LastItemCode::FontCharHt => {
                                        cur_val = *FONT_CHARINFO_HEIGHT(q, i)
                                    }
                                    LastItemCode::FontCharDp => {
                                        cur_val = *FONT_CHARINFO_DEPTH(q, i)
                                    }
                                    LastItemCode::FontCharIc => {
                                        cur_val = *FONT_CHARINFO_ITALCORR(q, i)
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                cur_val = 0;
                            }
                        }
                        LastItemCode::ParShapeLength
                        | LastItemCode::ParShapeIndent
                        | LastItemCode::ParShapeDimen => {
                            let mut q = cur_chr - (LastItemCode::ParShapeLength as i32);
                            scan_int();
                            if cur_val <= 0 {
                                cur_val = 0;
                            } else if let Some(l) = LOCAL(Local::par_shape).opt() {
                                if q == 2 {
                                    q = cur_val % 2;
                                    cur_val = (cur_val + q) / 2;
                                }
                                if cur_val > MEM[l].b32.s0 {
                                    cur_val = MEM[l].b32.s0
                                }
                                cur_val = MEM[l + 2 * (cur_val as usize) - (q as usize)].b32.s1;
                            } else {
                                cur_val = 0;
                            }
                            cur_val_level = ValLevel::Dimen
                        }
                        LastItemCode::GlueStretch | LastItemCode::GlueShrink => {
                            scan_normal_glue();
                            let q = cur_val as usize;
                            if m == LastItemCode::GlueStretch {
                                cur_val = MEM[q + 2].b32.s1
                            } else {
                                cur_val = MEM[q + 3].b32.s1
                            }
                            delete_glue_ref(q);
                        }
                        _ => {}
                    }
                    cur_val_level = ValLevel::Dimen
                } else {
                    match m {
                        LastItemCode::InputLineNo => cur_val = line,
                        LastItemCode::Badness => cur_val = last_badness,
                        LastItemCode::PdfShellEscape => cur_val = 0,
                        LastItemCode::EtexVersion => cur_val = ETEX_VERSION,
                        LastItemCode::XetexVersion => cur_val = XETEX_VERSION,
                        LastItemCode::XetexCountGlyphs => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    cur_val = aat::aat_font_get(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                    )
                                }
                                OTGR_FONT_FLAG => {
                                    cur_val =
                                        ot_font_get((m as i32) - 14, FONT_LAYOUT_ENGINE[n as usize])
                                }
                                _ => cur_val = 0,
                            }
                        }
                        LastItemCode::XetexCountFeatures => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    cur_val = aat::aat_font_get(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                    )
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => cur_val = -1,
                                OTGR_FONT_FLAG => {
                                    if usingGraphite(
                                        FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0
                                    {
                                        cur_val = ot_font_get(
                                            (m as i32) - 14,
                                            FONT_LAYOUT_ENGINE[n as usize],
                                        );
                                    } else {
                                        cur_val = 0;
                                    }
                                }
                                _ => cur_val = 0,
                            }
                        }
                        LastItemCode::XetexVariation
                        | LastItemCode::XetexVariationMin
                        | LastItemCode::XetexVariationMax
                        | LastItemCode::XetexVariationDefault
                        | LastItemCode::XetexCountVariations => {
                            scan_font_ident();
                            n = cur_val;
                            cur_val = 0;
                        }
                        LastItemCode::XetexFeatureCode
                        | LastItemCode::XetexIsExclusiveFeature
                        | LastItemCode::XetexCountSelectors => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    cur_val = aat::aat_font_get_1(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                        k,
                                    )
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    cur_val = -1;
                                }
                                OTGR_FONT_FLAG => {
                                    if usingGraphite(
                                        FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0
                                    {
                                        scan_int();
                                        k = cur_val;
                                        cur_val = ot_font_get_1(
                                            (m as i32) - 14,
                                            FONT_LAYOUT_ENGINE[n as usize],
                                            k,
                                        )
                                    } else {
                                        not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                        cur_val = -1;
                                    }
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    cur_val = -1;
                                }
                            }
                        }
                        LastItemCode::XetexSelectorCode | LastItemCode::XetexIsDefaultSelector => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    scan_int();
                                    cur_val = aat::aat_font_get_2(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                        k,
                                        cur_val,
                                    )
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    scan_int();
                                    cur_val = -1;
                                }
                                OTGR_FONT_FLAG => {
                                    if usingGraphite(
                                        FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0
                                    {
                                        scan_int();
                                        k = cur_val;
                                        scan_int();
                                        cur_val = ot_font_get_2(
                                            (m as i32) - 14,
                                            FONT_LAYOUT_ENGINE[n as usize],
                                            k,
                                            cur_val,
                                        )
                                    } else {
                                        not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                        cur_val = -1;
                                    }
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    cur_val = -1;
                                }
                            }
                        }
                        LastItemCode::XetexFindVariationByName => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    scan_and_pack_name();
                                    cur_val = aat::aat_font_get_named(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                    );
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => {
                                    scan_and_pack_name();
                                    cur_val = -1;
                                }
                                _ => {
                                    not_aat_font_error(Cmd::LastItem, m as i32, n as usize);
                                    cur_val = -1;
                                }
                            }
                        }
                        LastItemCode::XetexFindFeatureByName => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    scan_and_pack_name();
                                    cur_val = aat::aat_font_get_named(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                    );
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => {
                                    scan_and_pack_name();
                                    cur_val = -1;
                                }
                                OTGR_FONT_FLAG => {
                                    if usingGraphite(
                                        FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0
                                    {
                                        scan_and_pack_name();
                                        cur_val = gr_font_get_named(
                                            (m as i32) - 14,
                                            FONT_LAYOUT_ENGINE[n as usize],
                                        )
                                    } else {
                                        not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                        cur_val = -1;
                                    }
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    cur_val = -1;
                                }
                            }
                        }
                        LastItemCode::XetexFindSelectorByName => {
                            scan_font_ident();
                            n = cur_val;
                            match FONT_AREA[n as usize] as u32 {
                                #[cfg(target_os = "macos")]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    scan_and_pack_name();
                                    cur_val = aat::aat_font_get_named_1(
                                        m.into(),
                                        (FONT_LAYOUT_ENGINE[n as usize]) as _,
                                        k,
                                    );
                                }
                                #[cfg(not(target_os = "macos"))]
                                AAT_FONT_FLAG => {
                                    scan_int();
                                    k = cur_val;
                                    scan_and_pack_name();
                                    cur_val = -1;
                                }
                                OTGR_FONT_FLAG => {
                                    if usingGraphite(
                                        FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0
                                    {
                                        scan_int();
                                        k = cur_val;
                                        scan_and_pack_name();
                                        cur_val = gr_font_get_named_1(
                                            (m as i32) - 14,
                                            FONT_LAYOUT_ENGINE[n as usize],
                                            k,
                                        )
                                    } else {
                                        not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                        cur_val = -1;
                                    }
                                }
                                _ => {
                                    not_aat_gr_font_error(Cmd::LastItem, m as i32, n as usize);
                                    cur_val = -1;
                                }
                            }
                        }
                        LastItemCode::XetexOTCountScripts => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingOpenType(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                cur_val =
                                    ot_font_get((m as i32) - 14, FONT_LAYOUT_ENGINE[n as usize])
                            } else {
                                cur_val = 0;
                            }
                        }
                        LastItemCode::XetexOTCountLanguages | LastItemCode::XetexOTScript => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingOpenType(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                scan_int();
                                cur_val = ot_font_get_1(
                                    (m as i32) - 14,
                                    FONT_LAYOUT_ENGINE[n as usize],
                                    cur_val,
                                )
                            } else {
                                not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                cur_val = -1;
                            }
                        }
                        LastItemCode::XetexOTCountFeatures | LastItemCode::XetexOTLanguage => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingOpenType(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                scan_int();
                                k = cur_val;
                                scan_int();
                                cur_val = ot_font_get_2(
                                    (m as i32) - 14,
                                    FONT_LAYOUT_ENGINE[n as usize],
                                    k,
                                    cur_val,
                                )
                            } else {
                                not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                cur_val = -1;
                            }
                        }
                        LastItemCode::XetexOTFeature => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingOpenType(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                scan_int();
                                k = cur_val;
                                scan_int();
                                kk = cur_val;
                                scan_int();
                                cur_val = ot_font_get_3(
                                    (m as i32) - 14,
                                    FONT_LAYOUT_ENGINE[n as usize],
                                    k,
                                    kk,
                                    cur_val,
                                )
                            } else {
                                not_ot_font_error(Cmd::LastItem, m as i32, n as usize);
                                cur_val = -1;
                            }
                        }
                        LastItemCode::XetexMapCharToGlyph => {
                            if FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32
                                    == OTGR_FONT_FLAG
                            {
                                scan_int();
                                n = cur_val;
                                cur_val = map_char_to_glyph(EQTB[CUR_FONT_LOC].val as usize, n)
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                cur_val = 0;
                            }
                        }
                        LastItemCode::XetexGlyphIndex => {
                            if FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32
                                    == OTGR_FONT_FLAG
                            {
                                scan_and_pack_name();
                                cur_val = map_glyph_to_index(EQTB[CUR_FONT_LOC].val as usize)
                            } else {
                                not_native_font_error(
                                    Cmd::LastItem,
                                    m as i32,
                                    EQTB[CUR_FONT_LOC].val as usize,
                                );
                                cur_val = 0;
                            }
                        }
                        LastItemCode::XetexFontType => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == AAT_FONT_FLAG {
                                cur_val = 1;
                            } else if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingOpenType(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                cur_val = 2;
                            } else if FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                                && usingGraphite(
                                    FONT_LAYOUT_ENGINE[n as usize] as XeTeXLayoutEngine,
                                ) as i32
                                    != 0
                            {
                                cur_val = 3;
                            } else {
                                cur_val = 0;
                            }
                        }
                        LastItemCode::XetexFirstChar | LastItemCode::XetexLastChar => {
                            scan_font_ident();
                            n = cur_val;
                            if FONT_AREA[n as usize] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[n as usize] as u32 == OTGR_FONT_FLAG
                            {
                                cur_val = get_font_char_range(
                                    n as usize,
                                    (m == LastItemCode::XetexFirstChar) as i32,
                                )
                            } else if m == LastItemCode::XetexFirstChar {
                                cur_val = FONT_BC[n as usize] as i32
                            } else {
                                cur_val = FONT_EC[n as usize] as i32
                            }
                        }
                        LastItemCode::PdfLastXPos => cur_val = pdf_last_x_pos,
                        LastItemCode::PdfLastYPos => cur_val = pdf_last_y_pos,
                        LastItemCode::XetexPdfPageCount => {
                            scan_and_pack_name();
                            cur_val = count_pdf_file_pages()
                        }
                        LastItemCode::CurrentGroupLevel => cur_val = cur_level as i32 - 1,
                        LastItemCode::CurrentGroupType => cur_val = cur_group as i32,
                        LastItemCode::CurrentIfLevel => {
                            let mut qopt = cond_ptr;
                            cur_val = 0;
                            while let Some(q) = qopt {
                                cur_val += 1;
                                qopt = llist_link(q);
                            }
                        }
                        LastItemCode::CurrentIfType => {
                            cur_val = if cond_ptr.is_none() {
                                0
                            } else if (cur_if as i32) < UNLESS_CODE {
                                cur_if as i32 + 1
                            } else {
                                -(cur_if as i32 - 31)
                            };
                        }
                        LastItemCode::CurrentIfBranch => {
                            if if_limit == FiOrElseCode::Or || if_limit == FiOrElseCode::Else {
                                cur_val = 1;
                            } else if if_limit == FiOrElseCode::Fi {
                                cur_val = -1;
                            } else {
                                cur_val = 0;
                            }
                        }
                        LastItemCode::GlueStretchOrder | LastItemCode::GlueShrinkOrder => {
                            scan_normal_glue();
                            let q = cur_val as usize;
                            if m == LastItemCode::GlueStretchOrder {
                                cur_val = MEM[q].b16.s1 as i32
                            } else {
                                cur_val = MEM[q].b16.s0 as i32
                            }
                            delete_glue_ref(q);
                        }
                        _ => {}
                    }
                    cur_val_level = ValLevel::Int;
                }
            } else {
                cur_val = 0;
                let mut tx = cur_list.tail;
                if tx < hi_mem_min as usize {
                    if text_NODE_type(tx) == TextNode::Math.into()
                        && Math(tx).subtype() == MathType::Eq(BE::End, MathMode::Middle)
                    {
                        r = cur_list.head as i32;
                        let mut q;
                        loop {
                            q = r as usize;
                            r = *LLIST_link(q);
                            if !(r != tx as i32) {
                                break;
                            }
                        }
                        tx = q;
                    }
                }
                if m == LastItemCode::LastNodeType {
                    cur_val_level = ValLevel::Int;
                    if tx == cur_list.head || cur_list.mode.1 == ListMode::NoMode {
                        cur_val = -1;
                    }
                } else {
                    cur_val_level = ValLevel::from(m as u8);
                }
                if tx < hi_mem_min as usize && cur_list.mode.1 != ListMode::NoMode {
                    match m {
                        LastItemCode::LastPenalty => {
                            if text_NODE_type(tx) == TextNode::Penalty.into() {
                                cur_val = MEM[tx + 1].b32.s1;
                            }
                        }
                        LastItemCode::LastKern => {
                            if text_NODE_type(tx) == TextNode::Kern.into() {
                                cur_val = MEM[tx + 1].b32.s1;
                            }
                        }
                        LastItemCode::LastSkip => {
                            if text_NODE_type(tx) == TextNode::Glue.into() {
                                let g = Glue(tx);
                                cur_val = g.glue_ptr();
                                if g.param() == MU_GLUE {
                                    cur_val_level = ValLevel::Mu;
                                }
                            }
                        }
                        LastItemCode::LastNodeType => {
                            cur_val = if NODE_type(tx).u16() <= TextNode::Unset as u16 {
                                NODE_type(tx).u16() as i32 + 1
                            } else {
                                TextNode::Unset as i32 + 2
                            };
                        }
                        _ => {}
                    }
                } else if cur_list.mode == (false, ListMode::VMode) && tx == cur_list.head {
                    match m {
                        LastItemCode::LastPenalty => cur_val = last_penalty,
                        LastItemCode::LastKern => cur_val = last_kern,
                        LastItemCode::LastSkip => {
                            if last_glue != MAX_HALFWORD {
                                cur_val = last_glue
                            }
                        }
                        LastItemCode::LastNodeType => cur_val = last_node_type,
                        _ => {}
                    }
                }
            }
        }
        _ => {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("You can\'t use `");
            print_cmd_chr(cur_cmd, cur_chr);
            print_cstr("\' after ");
            print_esc_cstr("the");
            help!("I\'m forgetting what you said and using zero instead.");
            error();
            cur_val = 0;
            if level != ValLevel::Tok {
                cur_val_level = ValLevel::Dimen;
            } else {
                cur_val_level = ValLevel::Int;
            }
        }
    }
    while cur_val_level > level {
        /*447:*/
        if cur_val_level == ValLevel::Glue {
            cur_val = MEM[(cur_val + 1) as usize].b32.s1
        } else if cur_val_level == ValLevel::Mu {
            mu_error();
        }
        cur_val_level.prev();
    }
    if negative {
        match cur_val_level {
            ValLevel::Int | ValLevel::Dimen => cur_val = -cur_val,
            _ => {
                let mut cur_val_ = GlueSpec(new_spec(cur_val as usize));
                cur_val = cur_val_.ptr() as i32;
                cur_val_.set_size(-cur_val_.size());
                cur_val_.set_stretch(-cur_val_.stretch());
                cur_val_.set_shrink(-cur_val_.shrink());
            }
        }
    } else if cur_val_level == ValLevel::Glue || cur_val_level == ValLevel::Mu {
        GlueSpec(cur_val as usize).rc_inc();
    };
}
pub(crate) unsafe fn scan_int() {
    let mut d: i16 = 0;
    radix = 0;
    let mut OK_so_far = true;
    let mut negative = false;
    loop {
        loop
        /*424:*/
        {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer) {
                break;
            }
        }
        if cur_tok == OTHER_TOKEN + '-' as i32 {
            negative = !negative;
            cur_tok = OTHER_TOKEN + '+' as i32
        }
        if !(cur_tok == OTHER_TOKEN + '+' as i32) {
            break;
        }
    }
    if cur_tok == ALPHA_TOKEN as i32 {
        /*460:*/
        get_token(); /*461:*/
        if cur_tok < CS_TOKEN_FLAG {
            cur_val = cur_chr; /*462:*/
            if cur_cmd <= Cmd::RightBrace {
                if cur_cmd == Cmd::RightBrace {
                    align_state += 1;
                } else {
                    align_state -= 1;
                }
            }
        } else if cur_tok < CS_TOKEN_FLAG + SINGLE_BASE as i32 {
            cur_val = cur_tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
        } else {
            cur_val = cur_tok - (CS_TOKEN_FLAG + SINGLE_BASE as i32)
        } /*:463*/
        if cur_val > BIGGEST_USV as i32 {
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
            cur_val = '0' as i32;
            back_error();
        } else {
            get_x_token();
            if cur_cmd != Cmd::Spacer {
                back_input();
            }
        }
    } else if cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL {
        scan_something_internal(ValLevel::Int, false);
    } else {
        radix = 10;
        let mut m = 0xccccccc;
        if cur_tok == OCTAL_TOKEN {
            radix = 8;
            m = 0x10000000;
            get_x_token();
        } else if cur_tok == HEX_TOKEN {
            radix = 16;
            m = 0x8000000;
            get_x_token();
        }
        let mut vacuous = true;
        cur_val = 0;
        loop {
            if cur_tok < ZERO_TOKEN + radix as i32
                && cur_tok >= ZERO_TOKEN
                && cur_tok <= ZERO_TOKEN + 9
            {
                d = (cur_tok - ZERO_TOKEN) as i16
            } else {
                if !(radix as i32 == 16) {
                    break;
                }
                if cur_tok <= A_TOKEN + 5 && cur_tok >= A_TOKEN {
                    d = (cur_tok - A_TOKEN + 10) as i16
                } else {
                    if !(cur_tok <= OTHER_A_TOKEN + 5 && cur_tok >= OTHER_A_TOKEN) {
                        break;
                    }
                    d = (cur_tok - OTHER_A_TOKEN + 10) as i16
                }
            }
            vacuous = false;
            if cur_val >= m && (cur_val > m || d as i32 > 7 || radix as i32 != 10) {
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
                    cur_val = TEX_INFINITY;
                    OK_so_far = false
                }
            } else {
                cur_val = cur_val * radix as i32 + d as i32
            }
            get_x_token();
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
            back_error();
        } else if cur_cmd != Cmd::Spacer {
            back_input();
        }
    }
    if negative {
        cur_val = -cur_val;
    };
}
unsafe fn round_decimals(mut k: i16) -> scaled_t {
    let mut a: i32 = 0;
    while k as i32 > 0 {
        k -= 1;
        a = (a + dig[k as usize] as i32 * 0x20000) / 10
    }
    (a + 1) / 2
}
pub(crate) unsafe fn xetex_scan_dimen(
    mut mu: bool,
    mut inf: bool,
    mut shortcut: bool,
    mut requires_units: bool,
) {
    let mut f = 0;
    arith_error = false;
    cur_order = GlueOrder::Normal;
    let mut negative = false;
    if !shortcut {
        negative = false;
        loop {
            loop {
                get_x_token();
                if !(cur_cmd == Cmd::Spacer) {
                    break;
                }
            }
            if cur_tok == OTHER_TOKEN + '-' as i32 {
                negative = !negative;
                cur_tok = OTHER_TOKEN + '+' as i32
            }
            if !(cur_tok == OTHER_TOKEN + '+' as i32) {
                break;
            }
        }
        if cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL {
            /*468:*/
            if mu {
                scan_something_internal(ValLevel::Mu, false);
                match cur_val_level {
                    ValLevel::Int | ValLevel::Dimen => {}
                    _ => {
                        let v = MEM[(cur_val + 1) as usize].b32.s1;
                        delete_glue_ref(cur_val as usize);
                        cur_val = v;
                    }
                }
                if cur_val_level == ValLevel::Mu {
                    return attach_sign(negative);
                } else if cur_val_level != ValLevel::Int {
                    mu_error();
                }
            } else {
                scan_something_internal(ValLevel::Dimen, false);
                if cur_val_level == ValLevel::Dimen {
                    return attach_sign(negative);
                }
            }
        } else {
            back_input();
            if cur_tok == CONTINENTAL_POINT_TOKEN {
                cur_tok = POINT_TOKEN;
            }
            if cur_tok != POINT_TOKEN {
                scan_int();
            } else {
                radix = 10;
                cur_val = 0;
            }
            if cur_tok == CONTINENTAL_POINT_TOKEN {
                cur_tok = POINT_TOKEN;
            }
            if radix as i32 == 10 && cur_tok == POINT_TOKEN {
                /*471:*/
                let mut k = 0; /* if(requires_units) */
                let mut p = None.tex_int();
                get_token();
                loop {
                    get_x_token();
                    if cur_tok > ZERO_TOKEN + 9 || cur_tok < ZERO_TOKEN {
                        break;
                    }
                    if (k as i32) < 17 {
                        let q = get_avail();
                        *LLIST_link(q) = p;
                        MEM[q].b32.s0 = cur_tok - ZERO_TOKEN;
                        p = q as i32;
                        k += 1
                    }
                }

                // done1:
                for kk in (0..k as usize).rev() {
                    dig[kk] = MEM[p as usize].b32.s0 as u8;
                    let q = p as usize;
                    p = *LLIST_link(p as usize);
                    *LLIST_link(q) = avail.tex_int();
                    avail = Some(q);
                }
                f = round_decimals(k);
                if cur_cmd != Cmd::Spacer {
                    back_input();
                }
            }
        }
    }

    if cur_val < 0 {
        negative = !negative;
        cur_val = -cur_val
    }
    if requires_units {
        if inf {
            /*473:*/
            if scan_keyword(b"fil") {
                cur_order = GlueOrder::Fil;
                while scan_keyword(b"l") {
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
                return attach_fraction(f, negative);
            }
        }

        let save_cur_val = cur_val;

        loop {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer) {
                break;
            }
        }
        if cur_cmd < MIN_INTERNAL || cur_cmd > MAX_INTERNAL {
            back_input();
        } else {
            if mu {
                scan_something_internal(ValLevel::Mu, false);
                match cur_val_level {
                    ValLevel::Int | ValLevel::Dimen => {}
                    _ => {
                        let v = MEM[(cur_val + 1) as usize].b32.s1;
                        delete_glue_ref(cur_val as usize);
                        cur_val = v
                    }
                }
                if cur_val_level != ValLevel::Mu {
                    mu_error();
                }
            } else {
                scan_something_internal(ValLevel::Dimen, false);
            }
            let v = cur_val;
            return found(save_cur_val, v, f, negative);
        }

        if !mu {
            if scan_keyword(b"em") {
                let v = FONT_INFO
                    [(QUAD_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize]) as usize]
                    .b32
                    .s1;
                get_x_token();
                if cur_cmd != Cmd::Spacer {
                    back_input();
                }
                return found(save_cur_val, v, f, negative);
            } else if scan_keyword(b"ex") {
                let v = FONT_INFO
                    [(X_HEIGHT_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize]) as usize]
                    .b32
                    .s1;
                get_x_token();
                if cur_cmd != Cmd::Spacer {
                    back_input();
                }
                return found(save_cur_val, v, f, negative);
            }
        }

        // not_found:
        if mu {
            /*475:*/
            if scan_keyword(b"mu") {
                return attach_fraction(f, negative);
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
                return attach_fraction(f, negative);
            }
        }

        if scan_keyword(b"true") {
            /*476:*/
            prepare_mag(); /* magic ratio consant */
            if *INTPAR(IntPar::mag) != 1000 {
                cur_val = xn_over_d(cur_val, 1000, *INTPAR(IntPar::mag)); /* magic ratio consant */
                f = (((1000 * f) as i64 + 65536 * tex_remainder as i64)
                    / *INTPAR(IntPar::mag) as i64) as i32;
                cur_val = (cur_val as i64 + f as i64 / 65536) as i32;
                f = (f as i64 % 65536) as i32
            }
        }

        if scan_keyword(b"pt") {
            return attach_fraction(f, negative);
        }

        let num;
        let denom;
        if scan_keyword(b"in") {
            num = 7227;
            denom = 100;
        } else if scan_keyword(b"pc") {
            num = 12;
            denom = 1;
        } else if scan_keyword(b"cm") {
            num = 7227; // magic ratio consant
            denom = 254; // magic ratio consant
        } else if scan_keyword(b"mm") {
            num = 7227; // magic ratio consant
            denom = 2540; // magic ratio consant
        } else if scan_keyword(b"bp") {
            num = 7227; // magic ratio consant
            denom = 7200; // magic ratio consant
        } else if scan_keyword(b"dd") {
            num = 1238; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(b"cc") {
            num = 14856; // magic ratio consant
            denom = 1157; // magic ratio consant
        } else if scan_keyword(b"sp") {
            return done(negative);
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
            return attach_fraction(f, negative);
        }

        cur_val = xn_over_d(cur_val, num, denom);
        f = (((num * f) as i64 + 65536 * tex_remainder as i64) / denom as i64) as i32;
        cur_val = (cur_val as i64 + f as i64 / 65536) as i32;
        f = (f as i64 % 65536) as i32;
        return attach_fraction(f, negative);
    } else if cur_val >= 16384 {
        arith_error = true
    } else {
        cur_val = (cur_val as i64 * 65536 + f as i64) as i32
    }

    // done2:
    unsafe fn attach_fraction(f: i32, negative: bool) {
        if cur_val >= 16384 {
            arith_error = true
        } else {
            cur_val = (cur_val as i64 * 65536 + f as i64) as i32
        }
        done(negative)
    }

    unsafe fn done(negative: bool) {
        get_x_token();
        if cur_cmd != Cmd::Spacer {
            back_input();
        }
        attach_sign(negative)
    }

    unsafe fn found(save_cur_val: i32, v: i32, f: i32, negative: bool) {
        cur_val = mult_and_add(save_cur_val, v, xn_over_d(v, f, 65536 as i32), 0x3fffffff);
        attach_sign(negative)
    }

    unsafe fn attach_sign(negative: bool) {
        if arith_error || cur_val.wrapping_abs() >= 0x40000000 {
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
            cur_val = MAX_HALFWORD;
            arith_error = false
        }
        if negative {
            cur_val = -cur_val
        };
    }

    attach_sign(negative)
}
pub(crate) unsafe fn scan_dimen(mut mu: bool, mut inf: bool, mut shortcut: bool) {
    xetex_scan_dimen(mu, inf, shortcut, true);
}
pub(crate) unsafe fn scan_decimal() {
    xetex_scan_dimen(false, false, false, false);
}
pub(crate) unsafe fn scan_glue(level: ValLevel) {
    let mut negative: bool = false;
    let mut mu: bool = false;
    mu = level == ValLevel::Mu;
    negative = false;
    loop {
        loop {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer) {
                break;
            }
        }
        if cur_tok == OTHER_TOKEN + 45 {
            /*"-"*/
            negative = !negative;
            cur_tok = OTHER_TOKEN + 43
            /*"+"*/
        }
        if !(cur_tok == OTHER_TOKEN + 43) {
            break;
        }
        /*"+"*/
    }
    if cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL {
        scan_something_internal(level, negative);
        match cur_val_level {
            ValLevel::Int | ValLevel::Dimen => {}
            _ => {
                if cur_val_level != level {
                    mu_error();
                }
                return;
            }
        }
        if cur_val_level == ValLevel::Int {
            scan_dimen(mu, false, true);
        } else if level == ValLevel::Mu {
            mu_error();
        }
    } else {
        back_input();
        scan_dimen(mu, false, false);
        if negative {
            cur_val = -cur_val;
        }
    }
    let mut q = GlueSpec(new_spec(0));
    q.set_size(cur_val);
    if scan_keyword(b"plus") {
        scan_dimen(mu, true, false);
        q.set_stretch(cur_val).set_stretch_order(cur_order);
    }
    if scan_keyword(b"minus") {
        scan_dimen(mu, true, false);
        q.set_shrink(cur_val).set_shrink_order(cur_order);
    }
    cur_val = q.ptr() as i32;
    /*:481*/
}
pub(crate) unsafe fn add_or_sub(
    mut x: i32,
    mut y: i32,
    mut max_answer: i32,
    mut negative: bool,
) -> i32 {
    let mut a: i32 = 0;
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

    let mut a: i32 = 0;
    let mut f: i32 = 0;
    let mut h: i32 = 0;
    let mut r: i32 = 0;
    let mut t: i32 = 0;
    if d == 0 {
        return too_big();
    }
    a = 0;
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
    t = n / d;
    if t > max_answer / x {
        return too_big();
    }
    a = t * x;
    n = n - t * d;
    if n == 0 {
        return found(a, negative);
    }
    t = x / d;
    if t > (max_answer - a) / n {
        return too_big();
    }
    a = a + t * n;
    x = x - t * d;
    if x == 0 {
        return found(a, negative);
    }
    if x < n {
        t = x;
        x = n;
        n = t
    }
    f = 0;
    r = d / 2 - d;
    h = -r;
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
            t = x - d;
            x = t + x;
            f = f + n;
            if !(x < n) {
                continue;
            }
            if x == 0 {
                break;
            }
            t = x;
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
pub(crate) unsafe fn scan_expr() {
    let mut e: i32 = 0;
    let mut t: i32 = 0;
    let mut n: i32 = 0;
    let mut l = cur_val_level;
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
            loop {
                get_x_token();
                if !(cur_cmd == Cmd::Spacer) {
                    break;
                }
            }
            if cur_tok == OTHER_TOKEN + 40 {
                break;
            }
            back_input();
            match o {
                ValLevel::Int => scan_int(),
                ValLevel::Dimen => scan_dimen(false, false, false),
                ValLevel::Glue => scan_normal_glue(),
                _ => scan_mu_glue(),
            }
            let mut f = cur_val;
            loop {
                loop
                /*1572:*//*424:*/
                {
                    get_x_token();
                    if !(cur_cmd == Cmd::Spacer) {
                        break;
                    }
                }
                let mut o;
                if cur_tok == OTHER_TOKEN + 43 {
                    o = Expr::Add;
                } else if cur_tok == OTHER_TOKEN + 45 {
                    o = Expr::Sub;
                } else if cur_tok == OTHER_TOKEN + 42 {
                    o = Expr::Mult;
                } else if cur_tok == OTHER_TOKEN + 47 {
                    o = Expr::Div;
                } else {
                    o = Expr::None;
                    if p.opt().is_none() {
                        if cur_cmd != Cmd::Relax {
                            back_input();
                        }
                    } else if cur_tok != OTHER_TOKEN + 41 {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Missing ) inserted for expression");
                        help!("I was expecting to see `+\', `-\', `*\', `/\', or `)\'. Didn\'t.");
                        back_error();
                    }
                }
                arith_error = b;
                if l == ValLevel::Int || (s == Expr::Mult || s == Expr::Div || s == Expr::Scale) {
                    if f > TEX_INFINITY || f < -TEX_INFINITY {
                        arith_error = true;
                        f = 0;
                    }
                } else if l == ValLevel::Dimen {
                    if f.abs() > MAX_HALFWORD {
                        arith_error = true;
                        f = 0;
                    }
                } else if (MEM[(f + 1) as usize].b32.s1).abs() > MAX_HALFWORD
                    || (MEM[(f + 2) as usize].b32.s1).abs() > MAX_HALFWORD
                    || (MEM[(f + 3) as usize].b32.s1).abs() > MAX_HALFWORD
                {
                    arith_error = true;
                    delete_glue_ref(f as usize);
                    f = new_spec(0) as i32
                }
                match s as i32 {
                    0 => {
                        /*1579: */
                        t = if l >= ValLevel::Glue && o != Expr::None {
                            let mut t = GlueSpec(new_spec(f as usize));
                            delete_glue_ref(f as usize);
                            if t.stretch() == 0 {
                                t.set_stretch_order(GlueOrder::Normal);
                            }
                            if t.shrink() == 0 {
                                t.set_shrink_order(GlueOrder::Normal);
                            }
                            t.ptr() as i32
                        } else {
                            f
                        };
                    }
                    3 => {
                        if o == Expr::Div {
                            n = f;
                            o = Expr::Scale;
                        } else if l == ValLevel::Int {
                            t = mult_and_add(t, f, 0, TEX_INFINITY)
                        } else if l == ValLevel::Dimen {
                            t = mult_and_add(t, f, 0, MAX_HALFWORD)
                        } else {
                            MEM[(t + 1) as usize].b32.s1 =
                                mult_and_add(MEM[(t + 1) as usize].b32.s1, f, 0, MAX_HALFWORD);
                            MEM[(t + 2) as usize].b32.s1 =
                                mult_and_add(MEM[(t + 2) as usize].b32.s1, f, 0, MAX_HALFWORD);
                            MEM[(t + 3) as usize].b32.s1 =
                                mult_and_add(MEM[(t + 3) as usize].b32.s1, f, 0, MAX_HALFWORD)
                        }
                    }
                    4 => {
                        if l < ValLevel::Glue {
                            t = quotient(t, f)
                        } else {
                            MEM[(t + 1) as usize].b32.s1 =
                                quotient(MEM[(t + 1) as usize].b32.s1, f);
                            MEM[(t + 2) as usize].b32.s1 =
                                quotient(MEM[(t + 2) as usize].b32.s1, f);
                            MEM[(t + 3) as usize].b32.s1 = quotient(MEM[(t + 3) as usize].b32.s1, f)
                        }
                    }
                    5 => {
                        if l == ValLevel::Int {
                            t = fract(t, n, f, TEX_INFINITY)
                        } else if l == ValLevel::Dimen {
                            t = fract(t, n, f, MAX_HALFWORD)
                        } else {
                            let mut e = GlueSpec(e as usize);
                            let mut t = GlueSpec(t as usize);
                            t.set_size(fract(t.size(), n, f, MAX_HALFWORD));
                            t.set_stretch(fract(t.stretch(), n, f, MAX_HALFWORD));
                            e.set_shrink(fract(e.shrink(), n, f, MAX_HALFWORD));
                        }
                    }
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
                        e = add_or_sub(e, t, MAX_HALFWORD, r == Expr::Sub)
                    } else {
                        /*1582: */
                        let mut e = GlueSpec(e as usize);
                        let t = GlueSpec(t as usize);
                        e.set_size(add_or_sub(e.size(), t.size(), MAX_HALFWORD, r == Expr::Sub));
                        if e.stretch_order() == t.stretch_order() {
                            e.set_stretch(add_or_sub(
                                e.stretch(),
                                t.stretch(),
                                MAX_HALFWORD,
                                r == Expr::Sub,
                            ));
                        } else if e.stretch_order() < t.stretch_order() && t.stretch() != 0 {
                            e.set_stretch(t.stretch())
                                .set_stretch_order(t.stretch_order());
                        }
                        if e.shrink_order() == t.shrink_order() {
                            e.set_shrink(add_or_sub(
                                e.shrink(),
                                t.shrink(),
                                MAX_HALFWORD,
                                r == Expr::Sub,
                            ));
                        } else if e.shrink_order() < t.shrink_order() && t.shrink() != 0 {
                            e.set_shrink(t.shrink()).set_shrink_order(t.shrink_order());
                        }
                        delete_glue_ref(t.ptr());
                        if e.stretch() == 0 {
                            e.set_stretch_order(GlueOrder::Normal);
                        }
                        if e.shrink() == 0 {
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
    cur_val = e;
    cur_val_level = l;
}
pub(crate) unsafe fn scan_normal_glue() {
    scan_glue(ValLevel::Glue);
}
pub(crate) unsafe fn scan_mu_glue() {
    scan_glue(ValLevel::Mu);
}
pub(crate) unsafe fn scan_rule_spec() -> usize {
    let q = new_rule();
    if cur_cmd == Cmd::VRule {
        MEM[q + 1].b32.s1 = DEFAULT_RULE
    } else {
        MEM[q + 3].b32.s1 = DEFAULT_RULE;
        MEM[q + 2].b32.s1 = 0
    }
    loop {
        if scan_keyword(b"width") {
            scan_dimen(false, false, false);
            MEM[q + 1].b32.s1 = cur_val
        } else if scan_keyword(b"height") {
            scan_dimen(false, false, false);
            MEM[q + 3].b32.s1 = cur_val
        } else {
            if !scan_keyword(b"depth") {
                break;
            }
            scan_dimen(false, false, false);
            MEM[q + 2].b32.s1 = cur_val
        }
    }
    q
}
pub(crate) unsafe fn scan_general_text() {
    let mut unbalance: i32 = 0;
    let mut s = scanner_status;
    let mut w = warning_index;
    let mut d = def_ref;
    scanner_status = ScannerStatus::Absorbing;
    warning_index = cur_cs;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let mut p = def_ref;
    scan_left_brace();
    unbalance = 1;
    loop {
        get_token();
        if cur_tok < RIGHT_BRACE_LIMIT {
            if cur_cmd < Cmd::RightBrace {
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
        MEM[q].b32.s0 = cur_tok;
        p = q;
    }
    let q = llist_link(def_ref);
    *LLIST_link(def_ref) = avail.tex_int();
    avail = Some(def_ref);
    cur_val = if q.is_none() { TEMP_HEAD } else { p } as i32;
    *LLIST_link(TEMP_HEAD) = q.tex_int();
    scanner_status = s;
    warning_index = w;
    def_ref = d;
}
pub(crate) unsafe fn pseudo_start() {
    let mut w: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    scan_general_text();
    let old_setting_0 = selector;
    selector = Selector::NEW_STRING;
    token_show(Some(TEMP_HEAD));
    selector = old_setting_0;
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
    begin_file_reading();
    line = 0;
    cur_input.limit = cur_input.start;
    cur_input.loc = cur_input.limit + 1;
    if *INTPAR(IntPar::tracing_scan_tokens) > 0 {
        if term_offset > max_print_line - 3 {
            print_ln();
        } else if term_offset > 0 || file_offset > 0 {
            print_chr(' ');
        }
        cur_input.name = 19;
        print_cstr("( ");
        open_parens += 1;
        rust_stdout.as_mut().unwrap().flush().unwrap();
    } else {
        cur_input.name = 18;
        cur_input.synctex_tag = 0;
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
pub(crate) unsafe fn the_toks() -> usize {
    if cur_chr & 1 != 0 {
        let c = cur_chr as i16;
        scan_general_text();
        if c == 1 {
            assert!(cur_val.opt().is_some());
            return cur_val as usize; // TODO: check TEX_NULL
        } else {
            let old_setting_0 = selector;
            selector = Selector::NEW_STRING;
            let b = pool_ptr;
            let p = get_avail();
            *LLIST_link(p) = *LLIST_link(TEMP_HEAD);
            token_show(Some(p));
            flush_list(Some(p));
            selector = old_setting_0;
            return str_toks(b);
        }
    }
    get_x_token();
    scan_something_internal(ValLevel::Tok, false);
    match cur_val_level {
        ValLevel::Ident | ValLevel::Tok | ValLevel::InterChar | ValLevel::Mark => {
            /*485: */
            let mut p = TEMP_HEAD;
            *LLIST_link(p) = None.tex_int();
            if cur_val_level == ValLevel::Ident {
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = CS_TOKEN_FLAG + cur_val;
                p = q;
            } else if let Some(v) = cur_val.opt() {
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
            let old_setting_0 = selector;
            selector = Selector::NEW_STRING;
            let b = pool_ptr;
            match cur_val_level {
                ValLevel::Int => print_int(cur_val),
                ValLevel::Dimen => {
                    print_scaled(cur_val);
                    print_cstr("pt");
                }
                ValLevel::Glue => {
                    print_spec(cur_val, "pt");
                    delete_glue_ref(cur_val as usize);
                }
                ValLevel::Mu => {
                    print_spec(cur_val, "mu");
                    delete_glue_ref(cur_val as usize);
                }
                _ => {}
            }
            selector = old_setting_0;
            str_toks(b)
        }
    }
}
pub(crate) unsafe fn ins_the_toks() {
    *LLIST_link(GARBAGE as usize) = Some(the_toks()).tex_int();
    begin_token_list(*LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
}
pub(crate) unsafe fn conv_toks() {
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
    let c = ConvertCode::n(cur_chr as u8).unwrap();
    match c {
        ConvertCode::Number | ConvertCode::RomanNumeral => scan_int(),
        ConvertCode::String | ConvertCode::Meaning => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            get_token();
            scanner_status = save_scanner_status;
        }
        ConvertCode::FontName => scan_font_ident(),
        ConvertCode::XetexUchar => scan_usv_num(),
        ConvertCode::XetexUcharcat => {
            scan_usv_num();
            saved_chr = cur_val;
            scan_int();
            if cur_val < Cmd::LeftBrace as i32
                || cur_val > Cmd::OtherChar as i32
                || cur_val == OUT_PARAM as i32
                || cur_val == IGNORE as i32
            {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Invalid code (");
                print_int(cur_val);
                print_cstr("), should be in the ranges 1..4, 6..8, 10..12");
                help!("I\'m going to use 12 instead of that illegal code value.");
                error();
                cat = 12;
            } else {
                cat = cur_val as i16
            }
            cur_val = saved_chr
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
            compare_strings();
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
            boolvar = scan_keyword(b"file");
            scan_pdf_ext_toks();
            if selector == Selector::NEW_STRING {
                pdf_error(
                    "tokens",
                    "tokens_to_string() called while selector = new_string",
                );
            }
            let old_setting_0 = selector;
            selector = Selector::NEW_STRING;
            show_token_list(llist_link(def_ref), None, pool_size - pool_ptr);
            selector = old_setting_0;
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
            begin_token_list(*LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
            if u != 0 {
                str_ptr -= 1;
            }
            return;
        }
        ConvertCode::XetexVariationName => {
            scan_font_ident();
            fnt = cur_val as usize;
            if FONT_AREA[fnt as usize] as u32 == AAT_FONT_FLAG {
                scan_int();
                arg1 = cur_val;
                arg2 = 0;
            } else {
                not_aat_font_error(Cmd::Convert, c as i32, fnt);
            }
        }
        ConvertCode::XetexFeatureName => {
            scan_font_ident();
            fnt = cur_val as usize;
            if FONT_AREA[fnt as usize] as u32 == AAT_FONT_FLAG
                || FONT_AREA[fnt as usize] as u32 == OTGR_FONT_FLAG
                    && usingGraphite(FONT_LAYOUT_ENGINE[fnt as usize] as XeTeXLayoutEngine) as i32
                        != 0
            {
                scan_int();
                arg1 = cur_val;
                arg2 = 0;
            } else {
                not_aat_gr_font_error(Cmd::Convert, c as i32, fnt);
            }
        }
        ConvertCode::XetexSelectorName => {
            scan_font_ident();
            fnt = cur_val as usize;
            if FONT_AREA[fnt as usize] as u32 == AAT_FONT_FLAG
                || FONT_AREA[fnt as usize] as u32 == OTGR_FONT_FLAG
                    && usingGraphite(FONT_LAYOUT_ENGINE[fnt as usize] as XeTeXLayoutEngine) as i32
                        != 0
            {
                scan_int();
                arg1 = cur_val;
                scan_int();
                arg2 = cur_val
            } else {
                not_aat_gr_font_error(Cmd::Convert, c as i32, fnt);
            }
        }
        ConvertCode::XetexGlyphName => {
            scan_font_ident();
            fnt = cur_val as usize;
            if FONT_AREA[fnt as usize] as u32 == AAT_FONT_FLAG
                || FONT_AREA[fnt as usize] as u32 == OTGR_FONT_FLAG
            {
                scan_int();
                arg1 = cur_val
            } else {
                not_native_font_error(Cmd::Convert, c as i32, fnt);
            }
        }
        ConvertCode::LeftMarginKern | ConvertCode::RightMarginKern => {
            scan_register_num();
            p = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt())
            };
            if p.filter(|&p| text_NODE_type(p) == TextNode::HList.into())
                .is_none()
            {
                pdf_error("marginkern", "a non-empty hbox expected");
            }
        }
        ConvertCode::JobName => {
            if job_name == 0 {
                open_log_file();
            }
        }
        ConvertCode::EtexRevision | ConvertCode::XetexRevision => {}
    }
    let old_setting_0 = selector;
    selector = Selector::NEW_STRING;
    b = pool_ptr;
    match c {
        ConvertCode::Number => print_int(cur_val),
        ConvertCode::RomanNumeral => print_roman_int(cur_val),
        ConvertCode::String => {
            if cur_cs != 0 {
                sprint_cs(cur_cs);
            } else {
                print_char(cur_chr);
            }
        }
        ConvertCode::Meaning => print_meaning(),
        ConvertCode::FontName => {
            font_name_str = FONT_NAME[cur_val as usize];
            match FONT_AREA[cur_val as usize] as u32 {
                AAT_FONT_FLAG | OTGR_FONT_FLAG => {
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
            if FONT_SIZE[cur_val as usize] != FONT_DSIZE[cur_val as usize] {
                print_cstr(" at ");
                print_scaled(FONT_SIZE[cur_val as usize]);
                print_cstr("pt");
            }
        }
        ConvertCode::XetexUchar | ConvertCode::XetexUcharcat => print_char(cur_val),
        ConvertCode::EtexRevision => print_cstr(".6"),
        ConvertCode::PdfStrcmp => print_int(cur_val),
        ConvertCode::XetexRevision => print_cstr(".99998"),
        ConvertCode::XetexVariationName => {
            match FONT_AREA[fnt as usize] as u32 {
                #[cfg(target_os = "macos")]
                AAT_FONT_FLAG => {
                    aat::aat_print_font_name(
                        c as i32,
                        (FONT_LAYOUT_ENGINE[fnt as usize]) as _,
                        arg1,
                        arg2,
                    );
                }
                #[cfg(not(target_os = "macos"))]
                AAT_FONT_FLAG => {
                    // do nothing
                }
                _ => {
                    // do nothing
                }
            }
        }
        ConvertCode::XetexFeatureName | ConvertCode::XetexSelectorName => {
            match FONT_AREA[fnt as usize] as u32 {
                #[cfg(target_os = "macos")]
                AAT_FONT_FLAG => {
                    aat::aat_print_font_name(
                        c as i32,
                        (FONT_LAYOUT_ENGINE[fnt as usize]) as _,
                        arg1,
                        arg2,
                    );
                }
                #[cfg(not(target_os = "macos"))]
                AAT_FONT_FLAG => {
                    // do nothing
                }
                OTGR_FONT_FLAG => {
                    if usingGraphite(FONT_LAYOUT_ENGINE[fnt as usize] as XeTeXLayoutEngine) as i32
                        != 0
                    {
                        gr_print_font_name(c as i32, FONT_LAYOUT_ENGINE[fnt as usize], arg1, arg2);
                    }
                }
                _ => {}
            }
        }
        ConvertCode::XetexGlyphName => match FONT_AREA[fnt as usize] as u32 {
            AAT_FONT_FLAG | OTGR_FONT_FLAG => print_glyph_name(fnt, arg1),
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
                        TxtNode::HList(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.param() == (GluePar::right_skip as u16) + 1 => true,
                        _ => false,
                    },
                } {
                    popt = llist_link(p);
                } else {
                    break;
                }
            }
            if let Some(p) = popt.filter(|&p| {
                p < hi_mem_min as usize
                    && text_NODE_type(p) == TextNode::MarginKern.into()
                    && MEM[p].b16.s0 == 0
            }) {
                print_scaled(MEM[p + 1].b32.s1);
            } else {
                print('0' as i32);
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
                        TxtNode::HList(n) if n.is_empty() => true,
                        TxtNode::Glue(n) if n.param() == (GluePar::right_skip as u16) + 1 => true,
                        _ => false,
                    },
                } {
                    popt = prev_rightmost(q, Some(p));
                } else {
                    break;
                }
            }
            if let Some(p) = popt.filter(|&p| {
                p < hi_mem_min as usize
                    && text_NODE_type(p) == TextNode::MarginKern.into()
                    && MEM[p].b16.s0 == 1
            }) {
                print_scaled(MEM[p + 1].b32.s1);
            } else {
                print('0' as i32);
            }
            print_cstr("pt");
        }
        ConvertCode::JobName => print_file_name(job_name, 0, 0),
        _ => {}
    }
    selector = old_setting_0;
    *LLIST_link(GARBAGE) = str_toks_cat(b, cat) as i32;
    begin_token_list(*LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
}
pub(crate) unsafe fn scan_toks(mut macro_def: bool, mut xpand: bool) -> usize {
    let mut s: i32 = 0;
    let mut unbalance: i32 = 0;
    scanner_status = if macro_def {
        ScannerStatus::Defining
    } else {
        ScannerStatus::Absorbing
    };
    warning_index = cur_cs;
    def_ref = get_avail();
    MEM[def_ref].b32.s0 = None.tex_int();
    let mut p = def_ref;
    let mut hash_brace = 0;
    let mut t = ZERO_TOKEN;
    if macro_def {
        let mut done1 = true;
        loop
        /*493: */
        {
            get_token();
            if cur_tok < RIGHT_BRACE_LIMIT {
                break;
            }
            if cur_cmd == Cmd::MacParam {
                /*495: */
                s = MATCH_TOKEN + cur_chr;
                get_token();
                if cur_cmd == Cmd::LeftBrace {
                    hash_brace = cur_tok;
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = cur_tok;
                    p = q;
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = END_MATCH_TOKEN;
                    p = q;
                    done1 = false;
                    break;
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
                    if cur_tok != t {
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
                        back_error();
                    }
                    cur_tok = s
                }
            }
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = cur_tok;
            p = q;
        }

        if done1 {
            // done1:
            let q = get_avail();
            *LLIST_link(p) = Some(q).tex_int();
            MEM[q].b32.s0 = END_MATCH_TOKEN;
            p = q;
            if cur_cmd == Cmd::RightBrace {
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
        scan_left_brace();
    }

    unbalance = 1;
    loop {
        if xpand {
            loop
            /*497: */
            {
                get_next();
                if cur_cmd >= Cmd::Call {
                    if MEM[*LLIST_link(cur_chr as usize) as usize].b32.s0 == PROTECTED_TOKEN {
                        cur_cmd = Cmd::Relax;
                        cur_chr = NO_EXPAND_FLAG;
                    }
                }
                if cur_cmd <= MAX_COMMAND {
                    break;
                }
                if cur_cmd != Cmd::The {
                    expand();
                } else {
                    let q = the_toks();
                    if let Some(m) = llist_link(TEMP_HEAD) {
                        *LLIST_link(p) = Some(m).tex_int();
                        p = q
                    }
                }
            }
            // done2:
            x_token();
        } else {
            get_token();
        }
        if cur_tok < RIGHT_BRACE_LIMIT {
            if cur_cmd < Cmd::RightBrace {
                unbalance += 1;
            } else {
                unbalance -= 1;
                if unbalance == 0 {
                    return found(p, hash_brace);
                }
            }
        } else if cur_cmd == Cmd::MacParam {
            if macro_def {
                /*498: */
                s = cur_tok;
                if xpand {
                    get_x_token();
                } else {
                    get_token();
                }
                if cur_cmd != Cmd::MacParam {
                    if cur_tok <= ZERO_TOKEN || cur_tok > t {
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
                        back_error();
                        cur_tok = s
                    } else {
                        cur_tok = OUT_PARAM_TOKEN - 48 + cur_chr
                    }
                }
            }
        }
        let q = get_avail();
        *LLIST_link(p) = Some(q).tex_int();
        MEM[q].b32.s0 = cur_tok;
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
pub(crate) unsafe fn read_toks(mut n: i32, mut r: i32, mut j: i32) {
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
        begin_file_reading();
        cur_input.name = m as i32 + 1;
        assert!(
            read_open[m as usize] != OpenMode::Closed,
            /*503:*/
            "terminal input forbidden"
        );
        /*505:*/
        if read_open[m as usize] == OpenMode::JustOpen {
            /*504:*/
            if input_line(read_file[m as usize]) {
                read_open[m as usize] = OpenMode::Normal;
            } else {
                u_close(read_file[m as usize]);
                read_open[m as usize] = OpenMode::Closed;
            }
        } else if !input_line(read_file[m as usize]) {
            u_close(read_file[m as usize]);
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
        cur_input.limit = last;
        if *INTPAR(IntPar::end_line_char) < 0 || *INTPAR(IntPar::end_line_char) > 255 {
            cur_input.limit -= 1
        } else {
            BUFFER[cur_input.limit as usize] = *INTPAR(IntPar::end_line_char)
        }
        first = cur_input.limit + 1;
        cur_input.loc = cur_input.start;
        cur_input.state = InputState::NewLine;
        if j == 1 {
            while cur_input.loc <= cur_input.limit {
                cur_chr = BUFFER[cur_input.loc as usize];
                cur_input.loc += 1;
                if cur_chr == ' ' as i32 {
                    cur_tok = SPACE_TOKEN
                } else {
                    cur_tok = cur_chr + OTHER_TOKEN
                }
                let q = get_avail();
                *LLIST_link(p) = Some(q).tex_int();
                MEM[q].b32.s0 = cur_tok;
                p = q;
            }
        } else {
            loop {
                get_token();
                if cur_tok == 0 {
                    break;
                }
                if (align_state as i64) < 1000000 {
                    loop {
                        get_token();
                        if !(cur_tok != 0) {
                            break;
                        }
                    }
                    align_state = 1000000;
                    break;
                } else {
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = cur_tok;
                    p = q;
                }
            }
        }
        end_file_reading();
        if !(align_state as i64 != 1000000) {
            break;
        }
    }
    cur_val = def_ref as i32;
    scanner_status = ScannerStatus::Normal;
    align_state = s;
}
pub(crate) unsafe fn pass_text() {
    let save_scanner_status = scanner_status;
    scanner_status = ScannerStatus::Skipping;
    let mut l = 0;
    skip_line = line;
    loop {
        get_next();
        if cur_cmd == Cmd::FiOrElse {
            if l == 0 {
                break;
            }
            if cur_chr == FiOrElseCode::Fi as i32 {
                l -= 1;
            }
        } else if cur_cmd == Cmd::IfTest {
            l += 1;
        }
    }
    scanner_status = save_scanner_status;
    if *INTPAR(IntPar::tracing_ifs) > 0 {
        show_cur_cmd_chr();
    };
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
pub(crate) unsafe fn conditional() {
    let mut b: bool = false;
    let mut e: bool = false;
    let mut r: u8 = 0;

    if *INTPAR(IntPar::tracing_ifs) > 0 {
        if *INTPAR(IntPar::tracing_commands) <= 1 {
            show_cur_cmd_chr();
        }
    }

    let mut p = get_node(IF_NODE_SIZE);
    *LLIST_link(p) = cond_ptr.tex_int();
    MEM[p].b16.s1 = if_limit as u16;
    MEM[p].b16.s0 = cur_if as u16;
    MEM[p + 1].b32.s1 = if_line;
    cond_ptr = Some(p);
    cur_if = cur_chr as i16;
    if_limit = FiOrElseCode::If;
    if_line = line;

    let mut save_cond_ptr = cond_ptr;
    let mut is_unless = cur_chr >= UNLESS_CODE;
    let mut this_if = IfTestCode::n((cur_chr % UNLESS_CODE) as u8).unwrap();

    match this_if {
        IfTestCode::IfChar | IfTestCode::IfCat => {
            get_x_token();
            if cur_cmd == Cmd::Relax {
                if cur_chr == NO_EXPAND_FLAG {
                    cur_cmd = Cmd::ActiveChar;
                    cur_chr = cur_tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
                }
            }

            let (m, n) = if cur_cmd > Cmd::ActiveChar || cur_chr > BIGGEST_USV as i32 {
                (Cmd::Relax, TOO_BIG_USV as i32)
            } else {
                (cur_cmd, cur_chr)
            };

            get_x_token();

            if cur_cmd == Cmd::Relax {
                if cur_chr == NO_EXPAND_FLAG {
                    cur_cmd = Cmd::ActiveChar;
                    cur_chr = cur_tok - (CS_TOKEN_FLAG + ACTIVE_BASE as i32)
                }
            }

            if cur_cmd > Cmd::ActiveChar || cur_chr > BIGGEST_USV as i32 {
                cur_cmd = Cmd::Relax;
                cur_chr = TOO_BIG_USV as i32;
            }

            if this_if == IfTestCode::IfChar {
                b = n == cur_chr
            } else {
                b = m == cur_cmd
            }
        }
        IfTestCode::IfInt | IfTestCode::IfDim => {
            if this_if == IfTestCode::IfInt {
                scan_int();
            } else {
                scan_dimen(false, false, false);
            }

            let n = cur_val;

            loop {
                get_x_token();
                if !(cur_cmd == Cmd::Spacer) {
                    break;
                }
            }

            if cur_tok >= OTHER_TOKEN + 60 && cur_tok <= OTHER_TOKEN + 62 {
                r = (cur_tok - OTHER_TOKEN) as u8
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing = inserted for ");
                print_cmd_chr(Cmd::IfTest, this_if as i32);
                help!("I was expecting to see `<\', `=\', or `>\'. Didn\'t.");
                back_error();
                r = b'=';
            }

            if this_if == IfTestCode::IfInt {
                scan_int();
            } else {
                scan_dimen(false, false, false);
            }

            match r {
                60 => {
                    /*"<"*/
                    b = n < cur_val
                }
                61 => {
                    /*"="*/
                    b = n == cur_val
                }
                62 => {
                    /*">"*/
                    b = n > cur_val
                }
                _ => {}
            }
        }

        IfTestCode::IfOdd => {
            scan_int();
            b = cur_val & 1i32 != 0;
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
            scan_register_num();
            let p = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                cur_ptr.and_then(|cp| MEM[cp + 1].b32.s1.opt())
            };
            b = if this_if == IfTestCode::IfVoid {
                p.is_none()
            } else if let Some(p) = p {
                if this_if == IfTestCode::IfHBox {
                    text_NODE_type(p) == TextNode::HList.into()
                } else {
                    text_NODE_type(p) == TextNode::VList.into()
                }
            } else {
                false
            };
        }

        IfTestCode::Ifx => {
            let save_scanner_status = scanner_status;
            scanner_status = ScannerStatus::Normal;
            get_next();
            let n = cur_cs;
            let p = cur_cmd;
            let q = cur_chr;
            get_next();

            if cur_cmd != p {
                b = false
            } else if cur_cmd < Cmd::Call {
                b = cur_chr == q
            } else {
                let mut popt = LLIST_link(cur_chr as usize).opt();
                let mut qopt = MEM[EQTB[n as usize].val as usize].b32.s1.opt();
                if popt == qopt {
                    b = true
                } else {
                    while let (Some(p), Some(q)) = (popt, qopt) {
                        if MEM[p].b32.s0 != MEM[q as usize].b32.s0 {
                            popt = None
                        } else {
                            popt = llist_link(p);
                            qopt = llist_link(q);
                        }
                    }
                    b = popt.is_none() && qopt.is_none();
                }
            }

            scanner_status = save_scanner_status;
        }

        IfTestCode::IfEof => {
            scan_four_bit_int_or_18();
            b = if cur_val == 18 {
                true
            } else {
                read_open[cur_val as usize] == OpenMode::Closed
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
            get_next();
            b = cur_cmd != Cmd::UndefinedCS;
            scanner_status = save_scanner_status;
        }

        IfTestCode::IfCS => {
            let n = get_avail();
            let mut p = n;
            e = is_in_csname;
            is_in_csname = true;

            loop {
                get_x_token();
                if cur_cs == 0 {
                    let q = get_avail();
                    *LLIST_link(p) = Some(q).tex_int();
                    MEM[q].b32.s0 = cur_tok;
                    p = q;
                }
                if !(cur_cs == 0) {
                    break;
                }
            }

            if cur_cmd != Cmd::EndCSName {
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
                back_error();
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

            cur_cs = if m == first {
                NULL_CS as i32
            } else if m == first + 1i32 {
                SINGLE_BASE as i32 + BUFFER[first as usize]
            } else {
                id_lookup(first, m - first)
            };

            flush_list(Some(n));
            b = Cmd::from(EQTB[cur_cs as usize].cmd) != Cmd::UndefinedCS;
            is_in_csname = e;
        }

        IfTestCode::IfInCSName => {
            b = is_in_csname;
        }

        IfTestCode::IfFontChar => {
            scan_font_ident();
            let n = cur_val as usize;
            scan_usv_num();
            b = if FONT_AREA[n] as u32 == AAT_FONT_FLAG || FONT_AREA[n] as u32 == OTGR_FONT_FLAG {
                map_char_to_glyph(n, cur_val) > 0
            } else if FONT_BC[n] as i32 <= cur_val && FONT_EC[n] as i32 >= cur_val {
                FONT_CHARACTER_INFO(n, effective_char(true, n, cur_val as u16) as usize).s3 > 0
            } else {
                false
            };
        }

        IfTestCode::IfCase => {
            scan_int();
            let mut n = cur_val;

            if *INTPAR(IntPar::tracing_commands) > 1 {
                begin_diagnostic();
                print_cstr("{case ");
                print_int(n);
                print_chr('}');
                end_diagnostic(false);
            }

            loop {
                if n == 0 {
                    break;
                }

                pass_text();

                if cond_ptr == save_cond_ptr {
                    if cur_chr == FiOrElseCode::Or as i32 {
                        n -= 1;
                    } else {
                        return common_ending();
                    }
                } else if cur_chr == FiOrElseCode::Fi as i32 {
                    /*515:*/
                    if IF_STACK[IN_OPEN] == cond_ptr {
                        if_warning();
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
            get_next();
            scanner_status = save_scanner_status;
            let m = if cur_cs < HASH_BASE as i32 {
                prim_lookup(cur_cs - SINGLE_BASE as i32)
            } else {
                prim_lookup((*hash.offset(cur_cs as isize)).s1)
            } as i32;
            b = cur_cmd != Cmd::UndefinedCS
                && m != UNDEFINED_PRIMITIVE
                && cur_cmd == Cmd::from(prim_eqtb[m as usize].cmd)
                && cur_chr == prim_eqtb[m as usize].val;
        }
    }

    if is_unless {
        b = !b
    }

    if *INTPAR(IntPar::tracing_commands) > 1 {
        /*521:*/
        begin_diagnostic();
        if b {
            print_cstr("{true}");
        } else {
            print_cstr("{false}");
        }
        end_diagnostic(false);
    }

    if b {
        change_if_limit(FiOrElseCode::Else, save_cond_ptr);
        return;
    }

    loop {
        pass_text();

        if cond_ptr == save_cond_ptr {
            if cur_chr != FiOrElseCode::Or as i32 {
                return common_ending();
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
        } else if cur_chr == FiOrElseCode::Fi as i32 {
            /*515:*/
            if IF_STACK[IN_OPEN] == cond_ptr {
                if_warning();
            }
            let p = cond_ptr.unwrap();
            if_line = MEM[p + 1].b32.s1;
            cur_if = MEM[p].b16.s0 as i16;
            if_limit = FiOrElseCode::n(MEM[p].b16.s1 as u8).unwrap();
            cond_ptr = llist_link(p);
            free_node(p, IF_NODE_SIZE);
        }
    }

    unsafe fn common_ending() {
        if cur_chr == FiOrElseCode::Fi as i32 {
            /*515:*/
            if IF_STACK[IN_OPEN] == cond_ptr {
                if_warning();
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
pub(crate) unsafe fn begin_name() {
    area_delimiter = 0;
    ext_delimiter = 0;
    quoted_filename = false;
    file_name_quote_char = None;
}
pub(crate) unsafe fn more_name(c: UTF16_code, stop_at_space_: bool) -> bool {
    if stop_at_space_ && file_name_quote_char.is_none() && c as i32 == ' ' as i32 {
        return false;
    }
    if stop_at_space_ && file_name_quote_char == Some(c) {
        file_name_quote_char = None;
        return true;
    }
    if stop_at_space_ && file_name_quote_char.is_none() && (c == '\"' as u16 || c == '\'' as u16) {
        file_name_quote_char = Some(c);
        quoted_filename = true;
        return true;
    }
    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    str_pool[pool_ptr as usize] = c;
    pool_ptr += 1;
    if c == '/' as u16 {
        // IS_DIR_SEP
        area_delimiter = cur_length();
        ext_delimiter = 0;
    } else if c == '.' as u16 {
        ext_delimiter = cur_length()
    }
    true
}
pub(crate) unsafe fn end_name() {
    let mut temp_str: str_number = 0;
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
        temp_str = search_string(cur_area);
        if temp_str > 0 {
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
        cur_ext = (65536 + 1 as i64) as str_number;
        cur_name = slow_make_string()
    } else {
        cur_name = str_ptr;
        str_start[((str_ptr + 1) as i64 - 65536) as usize] =
            str_start[(str_ptr - 65536) as usize] + ext_delimiter - area_delimiter - 1;
        str_ptr += 1;
        cur_ext = make_string();
        str_ptr -= 1;
        temp_str = search_string(cur_name);
        if temp_str > 0 {
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
}
pub(crate) unsafe fn pack_file_name(mut n: str_number, mut a: str_number, mut e: str_number) {
    name_of_file = gettexstring(a) + &gettexstring(n) + &gettexstring(e);
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
    let save_area_delimiter = area_delimiter;
    let save_ext_delimiter = ext_delimiter;
    let save_name_in_progress = name_in_progress;
    name_in_progress = true;
    begin_name();
    for &k in &name_of_file16 {
        if !more_name(k, false) {
            break;
        }
    }
    end_name();
    name_in_progress = save_name_in_progress;
    area_delimiter = save_area_delimiter;
    ext_delimiter = save_ext_delimiter;
    Result
}
pub(crate) unsafe fn scan_file_name() {
    name_in_progress = true;
    begin_name();
    loop {
        get_x_token();
        if !(cur_cmd == Cmd::Spacer) {
            break;
        }
    }
    loop {
        if cur_cmd > Cmd::OtherChar || cur_chr > BIGGEST_CHAR {
            back_input();
            break;
        } else {
            if !more_name(cur_chr as UTF16_code, stop_at_space) {
                break;
            }
            get_x_token();
        }
    }
    end_name();
    name_in_progress = false;
}
pub(crate) unsafe fn pack_job_name(s: &str) {
    cur_area = EMPTY_STRING as str_number;
    cur_ext = maketexstring(s);
    cur_name = job_name;
    pack_file_name(cur_name, cur_area, cur_ext);
}
pub(crate) unsafe fn open_log_file() {
    let mut k: i32 = 0;
    let mut l: i32 = 0;
    let old_setting_0 = selector;
    if job_name == 0 {
        job_name = maketexstring("texput")
    }
    pack_job_name(".log");
    log_file = ttstub_output_open(CString::new(name_of_file.as_str()).unwrap().as_ptr(), 0);
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
    l = INPUT_STACK[0].limit;
    if BUFFER[l as usize] == *INTPAR(IntPar::end_line_char) {
        l -= 1
    }
    k = 1;
    while k <= l {
        print(BUFFER[k as usize]);
        k += 1;
    }
    print_ln();
    selector = (u8::from(old_setting_0) + 2).into();
}
pub(crate) unsafe fn start_input(mut primary_input_name: *const i8) {
    let mut format = TTInputFormat::TEX;
    let mut temp_str: str_number = 0;
    if !primary_input_name.is_null() {
        /* If this is the case, we're opening the primary input file, and the
         * name that we should use to refer to it has been handed directly to
         * us. We emulate the hacks used below to fill in cur_name, etc., from
         * a UTF-8 C string. It looks like the `cur_{name,area,ext}` strings
         * are hardly used so it'd be nice to get rid of them someday. */
        format = TTInputFormat::TECTONIC_PRIMARY;
        name_in_progress = true;
        begin_name();
        stop_at_space = false;
        let mut cp: *const u8 = primary_input_name as *const u8;
        assert!(
            !((pool_ptr as usize).wrapping_add(strlen(primary_input_name).wrapping_mul(2))
                >= pool_size as usize),
            "string pool overflow [{} bytes]",
            pool_size,
        );
        let mut rval: u32 = 0;
        loop {
            let fresh39 = cp;
            cp = cp.offset(1);
            rval = *fresh39 as u32;
            if !(rval != 0_u32) {
                break;
            }
            let mut extraBytes: u16 = bytesFromUTF8[rval as usize] as u16;
            let mut current_block_21: u64;
            match extraBytes as i32 {
                5 => {
                    /* note: code falls through cases! */
                    rval <<= 6i32;
                    if *cp != 0 {
                        let fresh40 = cp;
                        cp = cp.offset(1);
                        rval = (rval as u32).wrapping_add(*fresh40 as u32) as u32 as u32
                    }
                    current_block_21 = 7676382540965064243;
                }
                4 => current_block_21 = 7676382540965064243,
                3 => current_block_21 = 13258898395114305131,
                2 => current_block_21 = 10625751394499422232,
                1 => current_block_21 = 4051951890355284227,
                0 | _ => current_block_21 = 14818589718467733107,
            }
            match current_block_21 {
                7676382540965064243 => {
                    rval <<= 6i32;
                    if *cp != 0 {
                        let fresh41 = cp;
                        cp = cp.offset(1);
                        rval = (rval as u32).wrapping_add(*fresh41 as u32) as u32 as u32
                    }
                    current_block_21 = 13258898395114305131;
                }
                _ => {}
            }
            match current_block_21 {
                13258898395114305131 => {
                    rval <<= 6i32;
                    if *cp != 0 {
                        let fresh42 = cp;
                        cp = cp.offset(1);
                        rval = (rval as u32).wrapping_add(*fresh42 as u32) as u32 as u32
                    }
                    current_block_21 = 10625751394499422232;
                }
                _ => {}
            }
            match current_block_21 {
                10625751394499422232 => {
                    rval <<= 6i32;
                    if *cp != 0 {
                        let fresh43 = cp;
                        cp = cp.offset(1);
                        rval = (rval as u32).wrapping_add(*fresh43 as u32) as u32 as u32
                    }
                    current_block_21 = 4051951890355284227;
                }
                _ => {}
            }
            match current_block_21 {
                4051951890355284227 => {
                    rval <<= 6i32;
                    if *cp != 0 {
                        let fresh44 = cp;
                        cp = cp.offset(1);
                        rval = (rval as u32).wrapping_add(*fresh44 as u32) as u32 as u32
                    }
                }
                _ => {}
            }
            rval = (rval as u32).wrapping_sub(offsetsFromUTF8[extraBytes as usize]) as u32 as u32;
            if rval > 0xffff_u32 {
                rval = (rval as u32).wrapping_sub(0x10000_u32) as u32 as u32;
                let fresh45 = pool_ptr;
                pool_ptr = pool_ptr + 1;
                str_pool[fresh45 as usize] =
                    (0xd800_u32).wrapping_add(rval.wrapping_div(0x400_u32)) as packed_UTF16_code;
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
                area_delimiter = cur_length();
                ext_delimiter = 0;
            } else if rval == '.' as i32 as u32 {
                ext_delimiter = cur_length()
            }
        }
        stop_at_space = true;
        end_name();
        name_in_progress = false
    } else {
        /* Scan in the file name from the current token stream. The file name to
         * input is saved as the stringpool strings `cur_{name,area,ext}` and the
         * UTF-8 string `name_of_file`. */
        scan_file_name();
    }
    pack_file_name(cur_name, cur_area, cur_ext);
    /* Open up the new file to be read. The name of the file to be read comes
     * from `name_of_file`. */
    begin_file_reading();
    if u_open_in(
        &mut INPUT_FILE[cur_input.index as usize],
        format,
        b"rb",
        UnicodeMode::from(*INTPAR(IntPar::xetex_default_input_mode)),
        *INTPAR(IntPar::xetex_default_input_encoding),
    ) == 0
    {
        abort!("failed to open input file \"{}\"", name_of_file);
    }
    /* Now re-encode `name_of_file` into the UTF-16 variable `name_of_file16`,
     * and use that to recompute `cur_{name,area,ext}`. */
    name_in_progress = true;
    begin_name();
    for k in name_of_file.encode_utf16() {
        if !more_name(k, false) {
            break;
        }
    }
    stop_at_space = true;
    end_name();
    name_in_progress = false;
    /* Now generate a stringpool string corresponding to the full path of the
     * input file. This calls make_utf16_name() again and reruns through the
     * {begin,more,end}_name() trifecta to re-re-compute
     * `cur_{name,area,ext}`. */
    cur_input.name = make_name_string();
    SOURCE_FILENAME_STACK[IN_OPEN] = cur_input.name;
    /* *This* variant is a TeX string made out of `name_of_input_file`. */
    FULL_SOURCE_FILENAME_STACK[IN_OPEN] = maketexstring(&name_of_input_file);
    if cur_input.name == str_ptr - 1 {
        temp_str = search_string(cur_input.name);
        if temp_str > 0 {
            cur_input.name = temp_str;
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize]
        }
    }
    /* Finally we start really doing stuff with the newly-opened file. */
    if job_name == 0 {
        job_name = cur_name; /* this is the "flush_string" macro which discards the most recent string */
        open_log_file(); /* "really a CFDictionaryRef or XeTeXLayoutEngine" */
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
    cur_input.state = InputState::NewLine;
    synctex_start_input();
    line = 1;
    input_line(INPUT_FILE[cur_input.index as usize]);
    cur_input.limit = last;
    if *INTPAR(IntPar::end_line_char) < 0 || *INTPAR(IntPar::end_line_char) > 255 {
        cur_input.limit -= 1
    } else {
        BUFFER[cur_input.limit as usize] = *INTPAR(IntPar::end_line_char)
    }
    first = cur_input.limit + 1;
    cur_input.loc = cur_input.start;
}
pub(crate) unsafe fn effective_char_info(mut f: internal_font_number, mut c: u16) -> b16x4 {
    if !xtx_ligature_present && !(FONT_MAPPING[f]).is_null() {
        c = apply_tfm_font_mapping(FONT_MAPPING[f], c as i32) as u16
    }
    xtx_ligature_present = false;
    FONT_CHARACTER_INFO(f, c as usize)
}
pub(crate) unsafe fn char_warning(mut f: internal_font_number, mut c: i32) {
    let mut old_setting_0: i32 = 0;
    if *INTPAR(IntPar::tracing_lost_chars) > 0 {
        old_setting_0 = *INTPAR(IntPar::tracing_online);
        if *INTPAR(IntPar::tracing_lost_chars) > 1 {
            *INTPAR(IntPar::tracing_online) = 1
        }
        begin_diagnostic();
        print_nl_cstr("Missing character: There is no ");
        if (c as i64) < 65536 {
            print(c);
        } else {
            print_char(c);
        }
        print_cstr(" in font ");
        print(FONT_NAME[f]);
        print_chr('!');
        end_diagnostic(false);
        *INTPAR(IntPar::tracing_online) = old_setting_0
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
    ttstub_issue_warning_slice(
        format!(
            "could not represent character \"{}\" in font \"{}\"",
            chr, fn_0
        )
        .as_bytes(),
    );
    if !gave_char_warning_help {
        ttstub_issue_warning_slice(
            b"  you may need to load the `fontspec` package and use (e.g.) \\setmainfont to",
        );
        ttstub_issue_warning_slice(
            b"  choose a different font that covers the unrepresentable character(s)",
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
    mut f: internal_font_number,
    mut c: UnicodeScalar,
) -> NativeWord {
    let mut p;
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
                if map_char_to_glyph(f, c) == 0 {
                    char_warning(f, c);
                }
                i += 2;
            } else {
                if map_char_to_glyph(f, *mapped_text.offset(i as isize) as i32) == 0 {
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
            if map_char_to_glyph(f, c) == 0 {
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
    begin_diagnostic();
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
    end_diagnostic(false);
}
pub(crate) unsafe fn font_mapping_warning(mut mapping_name: &[u8], mut warningType: i32) {
    begin_diagnostic();
    if warningType == 0i32 {
        print_nl_cstr("Loaded mapping `");
    } else {
        print_nl_cstr("Font mapping `");
    }
    print_utf8_str(mapping_name);
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
    end_diagnostic(false);
}
pub(crate) unsafe fn graphite_warning() {
    begin_diagnostic();
    print_nl_cstr("Font `");
    for b in name_of_file.bytes() {
        print_raw_char(b as UTF16_code, true);
    }
    print_cstr("\' does not support Graphite. Trying OpenType layout instead.");
    end_diagnostic(false);
}
pub(crate) unsafe fn load_native_font(
    mut u: i32,
    mut nom: str_number,
    mut aire: str_number,
    mut s: scaled_t,
) -> usize {
    let mut num_font_dimens: i32 = 0;
    let mut actual_size: scaled_t = 0;
    let mut ascent: scaled_t = 0;
    let mut descent: scaled_t = 0;
    let mut font_slant: scaled_t = 0;
    let mut x_ht: scaled_t = 0;
    let mut cap_ht: scaled_t = 0;
    let mut font_engine =
        find_native_font(CString::new(name_of_file.as_str()).unwrap().as_ptr(), s);
    if font_engine.is_null() {
        return FONT_BASE;
    }
    if s >= 0 {
        actual_size = s
    } else if s != -1000 {
        actual_size = xn_over_d(loaded_font_design_size, -s, 1000)
    } else {
        actual_size = loaded_font_design_size
    }
    if (pool_ptr as usize) + name_of_file.as_bytes().len() > (pool_size as usize) {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    for b in name_of_file.bytes() {
        str_pool[pool_ptr as usize] = b as packed_UTF16_code;
        pool_ptr = pool_ptr + 1;
    }

    let full_name = make_string();

    for f in (0 + 1)..=FONT_PTR {
        if FONT_AREA[f] == native_font_type_flag
            && str_eq_str(FONT_NAME[f], full_name)
            && FONT_SIZE[f] == actual_size
        {
            release_font_engine(font_engine, native_font_type_flag);
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
            return f;
        }
    }

    num_font_dimens = if native_font_type_flag as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(font_engine as XeTeXLayoutEngine)
    {
        65 // = first_math_fontdimen (=10) + lastMathConstant (= radicalDegreeBottomRaisePercent = 55)
    } else {
        8
    };
    if FONT_PTR == FONT_MAX || fmem_ptr + num_font_dimens > FONT_MEM_SIZE as i32 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Font ");
        sprint_cs(u);
        print_chr('=');
        if let Some(qc) = file_name_quote_char {
            print_char(qc as i32);
        }
        print_file_name(nom, aire, cur_ext);
        if let Some(qc) = file_name_quote_char {
            print_char(qc as i32);
        }
        if s >= 0 {
            print_cstr(" at ");
            print_scaled(s);
            print_cstr("pt");
        } else if s != -1000 {
            print_cstr(" scaled ");
            print_int(-s);
        }
        print_cstr(" not loaded: Not enough room left");
        help!(
            "I\'m afraid I won\'t be able to make use of this font,",
            "because my memory for character-size data is too small.",
            "If you\'re really stuck, ask a wizard to enlarge me.",
            "Or maybe try `I\\font<same font id>=<name of loaded font>\'."
        );
        error();
        return FONT_BASE;
    }
    FONT_PTR += 1;
    FONT_AREA[FONT_PTR] = native_font_type_flag;
    FONT_NAME[FONT_PTR] = full_name;
    FONT_CHECK[FONT_PTR].s3 = 0;
    FONT_CHECK[FONT_PTR].s2 = 0;
    FONT_CHECK[FONT_PTR].s1 = 0;
    FONT_CHECK[FONT_PTR].s0 = 0;
    FONT_GLUE[FONT_PTR] = None.tex_int();
    FONT_DSIZE[FONT_PTR] = loaded_font_design_size;
    FONT_SIZE[FONT_PTR] = actual_size;
    match native_font_type_flag as u32 {
        #[cfg(target_os = "macos")]
        AAT_FONT_FLAG => {
            aat::aat_get_font_metrics(
                font_engine as _,
                &mut ascent,
                &mut descent,
                &mut x_ht,
                &mut cap_ht,
                &mut font_slant,
            );
        }
        #[cfg(not(target_os = "macos"))]
        AAT_FONT_FLAG => {
            // do nothing
        }
        _ => {
            ot_get_font_metrics(
                font_engine,
                &mut ascent,
                &mut descent,
                &mut x_ht,
                &mut cap_ht,
                &mut font_slant,
            );
        }
    }
    HEIGHT_BASE[FONT_PTR] = ascent;
    DEPTH_BASE[FONT_PTR] = -descent;
    FONT_PARAMS[FONT_PTR] = num_font_dimens;
    FONT_BC[FONT_PTR] = 0 as UTF16_code;
    FONT_EC[FONT_PTR] = 65535 as UTF16_code;
    font_used[FONT_PTR] = false;
    HYPHEN_CHAR[FONT_PTR] = *INTPAR(IntPar::default_hyphen_char);
    SKEW_CHAR[FONT_PTR] = *INTPAR(IntPar::default_skew_char);
    PARAM_BASE[FONT_PTR] = fmem_ptr - 1;
    FONT_LAYOUT_ENGINE[FONT_PTR] = font_engine;
    FONT_MAPPING[FONT_PTR] = 0 as *mut libc::c_void;
    FONT_LETTER_SPACE[FONT_PTR] = loaded_font_letter_space;
    /* "measure the width of the space character and set up font parameters" */
    let p = new_native_character(FONT_PTR, ' ' as i32);
    s = p.width() + loaded_font_letter_space;
    p.free();

    FONT_INFO[fmem_ptr as usize].b32.s1 = font_slant;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 2; // space_stretch
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 3; // space_shrink
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = x_ht;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = FONT_SIZE[FONT_PTR]; // quad
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 3; // extra_space
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = cap_ht;
    fmem_ptr += 1;
    if num_font_dimens == 65 {
        FONT_INFO[fmem_ptr as usize].b32.s1 = num_font_dimens;
        fmem_ptr += 1;
        for k in 0..=55 {
            /* 55 = lastMathConstant */
            /*:582*/
            FONT_INFO[fmem_ptr as usize].b32.s1 = get_ot_math_constant(FONT_PTR, k);
            fmem_ptr += 1;
        }
    }
    FONT_MAPPING[FONT_PTR] = loaded_font_mapping;
    FONT_FLAGS[FONT_PTR] = loaded_font_flags;
    FONT_PTR
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
    begin_diagnostic();
    print_nl_cstr("Invalid UTF-8 byte or sequence");
    if cur_input.name == 0 {
        print_cstr(" in terminal input");
    } else {
        print_cstr(" at line ");
        print_int(line);
    }
    print_cstr(" replaced by U+FFFD.");
    end_diagnostic(false);
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
pub(crate) unsafe fn read_font_info(
    mut u: i32,
    mut nom: str_number,
    mut aire: str_number,
    mut s: scaled_t,
) -> internal_font_number {
    let mut k: font_index = 0;
    let mut name_too_long: bool = false;
    let mut lf: i32 = 0;
    let mut lh: i32 = 0;
    let mut bc: i32 = 0;
    let mut ec: i32 = 0;
    let mut nw: i32 = 0;
    let mut nh: i32 = 0;
    let mut nd: i32 = 0;
    let mut ni: i32 = 0;
    let mut nl: i32 = 0;
    let mut nk: i32 = 0;
    let mut ne: i32 = 0;
    let mut np: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut a: i32 = 0;
    let mut b: i32 = 0;
    let mut c: i32 = 0;
    let mut d: i32 = 0;
    let mut qw: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut sw: scaled_t = 0;
    let mut bch_label: i32 = 0;
    let mut bchar_0: i16 = 0;
    let mut z: scaled_t = 0;
    let mut alpha: i32 = 0;
    let mut beta: u8 = 0;

    let mut g = FONT_BASE;

    pack_file_name(nom, aire, cur_ext);

    if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
        begin_diagnostic();
        print_nl_cstr("Requested font \"");
        print_c_str(&name_of_file);
        print('\"' as i32);
        if s < 0 {
            print_cstr(" scaled ");
            print_int(-s);
        } else {
            print_cstr(" at ");
            print_scaled(s);
            print_cstr("pt");
        }
        end_diagnostic(false);
    }

    if quoted_filename {
        g = load_native_font(u, nom, aire, s);
        if g != FONT_BASE {
            return done(None, g);
        }
    }

    name_too_long = length(nom) > 255 || length(aire) > 255;
    if name_too_long {
        return bad_tfm(None, g, u, nom, aire, s, name_too_long);
    }
    pack_file_name(nom, aire, EMPTY_STRING as str_number);
    check_for_tfm_font_mapping();

    let mut tfm_file_owner = tt_xetex_open_input(TTInputFormat::TFM);
    if tfm_file_owner.is_none() {
        if !quoted_filename {
            g = load_native_font(u, nom, aire, s);
            if g != FONT_BASE {
                return done(None, g);
            }
        }
        return bad_tfm(None, g, u, nom, aire, s, name_too_long);
    }

    let tfm_file = tfm_file_owner.as_mut().unwrap();

    /* We are a bit cavalier about EOF-checking since we can't very
     * conveniently implement feof() in the Rust layer, and it only ever is
     * used in this one place. */

    macro_rules! READFIFTEEN (
        ($x:expr) => {
            $x = ttstub_input_getc(tfm_file);
            if $x > 127 || $x == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            $x *= 256;
            $x += ttstub_input_getc(tfm_file);

        };
    );

    READFIFTEEN!(lf);
    READFIFTEEN!(lh);
    READFIFTEEN!(bc);
    READFIFTEEN!(ec);

    if bc > ec + 1 || ec > 255 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if bc > 255 {
        bc = 1;
        ec = 0
    }

    READFIFTEEN!(nw);
    READFIFTEEN!(nh);
    READFIFTEEN!(nd);
    READFIFTEEN!(ni);
    READFIFTEEN!(nl);
    READFIFTEEN!(nk);
    READFIFTEEN!(ne);
    READFIFTEEN!(np);

    if lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    } else if nw == 0 || nh == 0 || nd == 0 || ni == 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }

    lf = lf - 6 - lh;
    if np < 7 {
        lf = lf + 7 - np
    }
    assert!(
        !(FONT_PTR == FONT_MAX || fmem_ptr + lf > FONT_MEM_SIZE as i32),
        "not enough memory to load another font"
    );

    f = FONT_PTR + 1;
    CHAR_BASE[f] = fmem_ptr - bc;
    WIDTH_BASE[f] = CHAR_BASE[f] + ec + 1;
    HEIGHT_BASE[f] = WIDTH_BASE[f] + nw;
    DEPTH_BASE[f] = HEIGHT_BASE[f] + nh;
    ITALIC_BASE[f] = DEPTH_BASE[f] + nd;
    LIG_KERN_BASE[f] = ITALIC_BASE[f] + ni;
    KERN_BASE[f] = LIG_KERN_BASE[f] + nl - 256 * 128;
    EXTEN_BASE[f] = KERN_BASE[f] + 256 * 128 + nk;
    PARAM_BASE[f] = EXTEN_BASE[f] + ne;
    if lh < 2 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    a = ttstub_input_getc(tfm_file);
    qw.s3 = a as u16;
    b = ttstub_input_getc(tfm_file);
    qw.s2 = b as u16;
    c = ttstub_input_getc(tfm_file);
    qw.s1 = c as u16;
    d = ttstub_input_getc(tfm_file);
    qw.s0 = d as u16;
    if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    FONT_CHECK[f as usize] = qw;

    READFIFTEEN!(z);
    z = z * 256 + ttstub_input_getc(tfm_file);
    z = z * 16 + ttstub_input_getc(tfm_file) / 16;
    if z < 65536 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    while lh > 2 {
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        lh -= 1
    }
    FONT_DSIZE[f] = z;
    if s != -1000 {
        if s >= 0 {
            z = s
        } else {
            z = xn_over_d(z, -s, 1000)
        }
    }
    FONT_SIZE[f] = z;

    k = fmem_ptr;
    loop {
        if !(k <= WIDTH_BASE[f] - 1) {
            break;
        }
        a = ttstub_input_getc(tfm_file);
        qw.s3 = a as u16;
        b = ttstub_input_getc(tfm_file);
        qw.s2 = b as u16;
        c = ttstub_input_getc(tfm_file);
        qw.s1 = c as u16;
        d = ttstub_input_getc(tfm_file);
        qw.s0 = d as u16;
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        FONT_INFO[k as usize].b16 = qw;

        if a >= nw || b / 16 >= nh || b % 16 >= nd || c / 4 >= ni {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }

        match c % 4 {
            1 => {
                if d >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            3 => {
                if d >= ne {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            2 => {
                if d < bc || d > ec {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                loop {
                    if !(d < k + bc - fmem_ptr) {
                        break;
                    }
                    qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
                    if qw.s1 as i32 % 4 != LIST_TAG {
                        break;
                    }
                    d = qw.s0 as i32
                }
                if d == k + bc - fmem_ptr {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            _ => {}
        }
        k += 1;
    }

    alpha = 16;
    while z >= 0x800000 {
        z = z / 2;
        alpha = alpha + alpha
    }
    beta = (256 / alpha) as u8;
    alpha = alpha * z;

    for k in WIDTH_BASE[f]..=LIG_KERN_BASE[f] - 1 {
        a = ttstub_input_getc(tfm_file);
        b = ttstub_input_getc(tfm_file);
        c = ttstub_input_getc(tfm_file);
        d = ttstub_input_getc(tfm_file);
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        sw = ((d * z / 256 + c * z) / 256 + b * z) / beta as i32;

        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = sw - alpha
        } else {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    if FONT_INFO[WIDTH_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[HEIGHT_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[DEPTH_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[ITALIC_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }

    bch_label = 32767;
    bchar_0 = 256;
    if nl > 0 {
        for k in LIG_KERN_BASE[f]..=KERN_BASE[f] + 256 * 128 - 1 {
            a = ttstub_input_getc(tfm_file);
            qw.s3 = a as u16;
            b = ttstub_input_getc(tfm_file);
            qw.s2 = b as u16;
            c = ttstub_input_getc(tfm_file);
            qw.s1 = c as u16;
            d = ttstub_input_getc(tfm_file);
            qw.s0 = d as u16;
            if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            FONT_INFO[k as usize].b16 = qw;

            if a > 128 {
                if 256 * c + d >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                if a == 255 && k == LIG_KERN_BASE[f] {
                    bchar_0 = b as i16
                }
            } else {
                if b != bchar_0 as i32 {
                    if b < bc || b > ec {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }

                    qw = FONT_INFO[(CHAR_BASE[f] + b) as usize].b16;
                    if !(qw.s3 > 0) {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                }

                if c < 128 {
                    if d < bc || d > ec {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                    qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
                    if !(qw.s3 > 0) {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                } else if 256 * (c - 128) + d >= nk {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                if a < 128 && k - LIG_KERN_BASE[f] + a + 1i32 >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
        }
        if a == 255 {
            bch_label = 256 * c + d
        }
    }

    for k in KERN_BASE[f] + 256 * 128..=EXTEN_BASE[f] - 1 {
        a = ttstub_input_getc(tfm_file);
        b = ttstub_input_getc(tfm_file);
        c = ttstub_input_getc(tfm_file);
        d = ttstub_input_getc(tfm_file);
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        sw = ((d * z / 256i32 + c * z) / 256i32 + b * z) / beta as i32;
        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = sw - alpha
        } else {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    for k in EXTEN_BASE[f]..=PARAM_BASE[f] - 1 {
        a = ttstub_input_getc(tfm_file);
        qw.s3 = a as u16;
        b = ttstub_input_getc(tfm_file);
        qw.s2 = b as u16;
        c = ttstub_input_getc(tfm_file);
        qw.s1 = c as u16;
        d = ttstub_input_getc(tfm_file);
        qw.s0 = d as u16;
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        FONT_INFO[k as usize].b16 = qw;

        if a != 0 {
            if a < bc || a > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            qw = FONT_INFO[(CHAR_BASE[f] + a) as usize].b16;
            if !(qw.s3 as i32 > 0i32) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if b != 0 {
            if b < bc || b > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            qw = FONT_INFO[(CHAR_BASE[f] + b) as usize].b16;
            if !(qw.s3 > 0) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if c != 0 {
            if c < bc || c > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            qw = FONT_INFO[(CHAR_BASE[f] + c) as usize].b16;
            if !(qw.s3 > 0) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if d < bc || d > ec {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
        if !(qw.s3 > 0) {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    for k in 1..=np {
        if k == 1 {
            sw = ttstub_input_getc(tfm_file);
            if sw == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            if sw > 127 {
                sw = sw - 256
            }

            sw = sw * 256 + ttstub_input_getc(tfm_file);
            sw = sw * 256 + ttstub_input_getc(tfm_file);
            FONT_INFO[PARAM_BASE[f] as usize].b32.s1 = sw * 16 + ttstub_input_getc(tfm_file) / 16
        } else {
            a = ttstub_input_getc(tfm_file);
            b = ttstub_input_getc(tfm_file);
            c = ttstub_input_getc(tfm_file);
            d = ttstub_input_getc(tfm_file);
            if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            sw = ((d * z / 256 + c * z) / 256 + b * z) / beta as i32;
            if a == 0 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = sw
            } else if a == 255 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = sw - alpha
            } else {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }
    }

    for k in np + 1..=7 {
        FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = 0;
    }

    if np >= 7 {
        FONT_PARAMS[f] = np
    } else {
        FONT_PARAMS[f] = 7
    }

    HYPHEN_CHAR[f] = *INTPAR(IntPar::default_hyphen_char);
    SKEW_CHAR[f] = *INTPAR(IntPar::default_skew_char);
    if bch_label < nl {
        BCHAR_LABEL[f] = bch_label + LIG_KERN_BASE[f]
    } else {
        BCHAR_LABEL[f] = NON_ADDRESS;
    }
    FONT_BCHAR[f] = bchar_0 as _;
    FONT_FALSE_BCHAR[f] = bchar_0 as nine_bits;

    if bchar_0 as i32 <= ec {
        if bchar_0 as i32 >= bc {
            qw = FONT_INFO[(CHAR_BASE[f] + bchar_0 as i32) as usize].b16;
            if qw.s3 as i32 > 0 {
                FONT_FALSE_BCHAR[f] = TOO_BIG_CHAR;
            }
        }
    }

    FONT_NAME[f] = nom;
    FONT_AREA[f] = aire;
    FONT_BC[f] = bc as UTF16_code;
    FONT_EC[f] = ec as UTF16_code;
    FONT_GLUE[f] = None.tex_int();
    PARAM_BASE[f] -= 1;
    fmem_ptr = fmem_ptr + lf;
    FONT_PTR = f;
    g = f;
    FONT_MAPPING[f] = load_tfm_font_mapping();

    return done(tfm_file_owner, g);

    /// Called on error
    unsafe fn bad_tfm(
        tfm_file: Option<InputHandleWrapper>,
        g: usize,
        u: i32,
        nom: i32,
        aire: i32,
        s: i32,
        name_too_long: bool,
    ) -> usize {
        if *INTPAR(IntPar::suppress_fontnotfound_error) == 0 {
            /* NOTE: must preserve this path to keep passing the TRIP tests */
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Font ");
            sprint_cs(u);
            print_chr('=');
            if let Some(qc) = file_name_quote_char {
                print_char(qc as i32);
            }
            print_file_name(nom, aire, cur_ext);
            if let Some(qc) = file_name_quote_char {
                print_char(qc as i32);
            }
            if s >= 0 {
                print_cstr(" at ");
                print_scaled(s);
                print_cstr("pt");
            } else if s != -1000 {
                print_cstr(" scaled ");
                print_int(-s);
            }
            if tfm_file.is_some() {
                print_cstr(" not loadable: Bad metric (TFM) file");
            } else if name_too_long {
                print_cstr(" not loadable: Metric (TFM) file name too long");
            } else {
                print_cstr(" not loadable: Metric (TFM) file or installed font not found");
            }
            help!(
                "I wasn\'t able to read the size data for this font,",
                "so I will ignore the font specification.",
                "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
                "You might try inserting a different font spec;",
                "e.g., type `I\\font<same font id>=<substitute font name>\'."
            );
            error();
        }
        return done(tfm_file, g);
    }
    // unreachable
    // return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);

    unsafe fn done(tfm_file: Option<InputHandleWrapper>, g: usize) -> usize {
        let file_opened = tfm_file.is_some();
        if let Some(handle) = tfm_file {
            ttstub_input_close(handle);
        }

        if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
            if g == FONT_BASE {
                begin_diagnostic();
                print_nl_cstr(" -> font not found, using \"nullfont\"");
                end_diagnostic(false);
            } else if file_opened {
                begin_diagnostic();
                print_nl_cstr(" -> ");
                print_c_str(&name_of_file);
                end_diagnostic(false);
            }
        }
        g
    }
    // unreachable
    // return done(tfm_file_owner, g);
}
pub(crate) unsafe fn new_character(
    mut f: internal_font_number,
    mut c: UTF16_code,
) -> Option<usize> {
    let mut ec: u16 = 0;
    if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
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
pub(crate) unsafe fn scan_spec(c: GroupCode, mut three_codes: bool) {
    let mut s: i32 = 0;
    let mut spec_code: PackMode = PackMode::Exactly;
    if three_codes {
        s = SAVE_STACK[SAVE_PTR + 0].val
    }
    let mut sd = true;
    if scan_keyword(b"to") {
        spec_code = PackMode::Exactly;
    } else if scan_keyword(b"spread") {
        spec_code = PackMode::Additional;
    } else {
        spec_code = PackMode::Additional;
        cur_val = 0;
        sd = false;
    }
    if sd {
        scan_dimen(false, false, false);
    }
    // found
    if three_codes {
        SAVE_STACK[SAVE_PTR + 0].val = s;
        SAVE_PTR += 1;
    }
    SAVE_STACK[SAVE_PTR + 0].val = spec_code as i32;
    SAVE_STACK[SAVE_PTR + 1].val = cur_val;
    SAVE_PTR += 2;
    new_save_level(c);
    scan_left_brace();
}
pub(crate) unsafe fn char_pw(p: Option<usize>, side: Side) -> scaled_t {
    if side == Side::Left {
        last_leftmost_char = None.tex_int()
    } else {
        last_rightmost_char = None.tex_int()
    }
    if p.is_none() {
        return 0;
    }
    let p = match CharOrText::from(p.unwrap()) {
        CharOrText::Char(p) => p,
        CharOrText::Text(TxtNode::Ligature(p)) => p.as_char(),
        CharOrText::Text(TxtNode::WhatsIt(WhatsIt::NativeWord(nw))) => {
            return if !(nw.glyph_info_ptr()).is_null() {
                let f = nw.font() as internal_font_number;
                round_xn_over_d(
                    FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1,
                    nw.native_word_cp(side),
                    1000,
                )
            } else {
                0
            };
        }
        CharOrText::Text(TxtNode::WhatsIt(WhatsIt::Glyph(g))) => {
            let f = g.font() as internal_font_number;
            return round_xn_over_d(
                FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1,
                get_cp_code(f, g.glyph() as u32, side),
                1000,
            );
        }
        _ => return 0,
    };
    let f = p.font() as internal_font_number;
    let c = get_cp_code(f, p.character() as u32, side);
    match side {
        Side::Left => last_leftmost_char = Some(p.ptr()).tex_int(),
        Side::Right => last_rightmost_char = Some(p.ptr()).tex_int(),
    }
    if c == 0 {
        return 0;
    }
    round_xn_over_d(
        FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1,
        c,
        1000,
    )
}
pub(crate) unsafe fn new_margin_kern(w: scaled_t, _p: i32, side: Side) -> usize {
    let k = get_node(MARGIN_KERN_NODE_SIZE);
    set_NODE_type(k, TextNode::MarginKern);
    MEM[k].b16.s0 = side as u16;
    MEM[k + 1].b32.s1 = w;
    k
}
pub(crate) unsafe fn hpack(mut popt: Option<usize>, mut w: scaled_t, m: PackMode) -> List {
    last_badness = 0;
    let mut r = List::from(get_node(BOX_NODE_SIZE));
    set_NODE_type(r.ptr(), TextNode::HList);
    r.set_lr_mode(LRMode::Normal);
    r.set_shift_amount(0);
    let mut q = r.ptr() + 5;
    r.set_list_ptr(popt.tex_int());
    let mut h = 0;
    let mut d = 0;
    let mut x = 0;
    total_stretch[NORMAL as usize] = 0;
    total_shrink[NORMAL as usize] = 0;
    total_stretch[FIL as usize] = 0;
    total_shrink[FIL as usize] = 0;
    total_stretch[FILL as usize] = 0;
    total_shrink[FILL as usize] = 0;
    total_stretch[FILLL as usize] = 0;
    total_shrink[FILLL as usize] = 0;
    if *INTPAR(IntPar::texxet) > 0 {
        /*1497: */
        let mut tmp_ptr = Math(get_avail());
        tmp_ptr.set_subtype_i32(MathType::Before);
        *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
        LR_ptr = Some(tmp_ptr.ptr()).tex_int();
    }
    while popt.is_some() {
        /*674: */
        while is_char_node(popt) {
            let p = Char(popt.unwrap());
            /*677: */
            let f = p.font() as internal_font_number;
            let i = FONT_CHARACTER_INFO(f, effective_char(true, f, p.character()) as usize);
            x += *FONT_CHARINFO_WIDTH(f, i);
            let s = *FONT_CHARINFO_HEIGHT(f, i);
            h = h.max(s);
            let s = *FONT_CHARINFO_DEPTH(f, i);
            d = d.max(s);
            popt = LLIST_link(p.ptr()).opt();
        }
        if let Some(mut p) = popt {
            let n = text_NODE_type(p).unwrap();
            match n {
                TextNode::HList | TextNode::VList => {
                    let b = List::from(p);
                    x += b.width();
                    let s = b.shift_amount();
                    h = h.max(b.height() - s);
                    d = d.max(b.depth() + s);
                }
                TextNode::Rule => {
                    let r = Rule::from(p);
                    x += r.width();
                    h = h.max(r.height());
                    d = d.max(r.depth());
                }
                TextNode::Unset => {
                    let u = Unset::from(p);
                    x += u.width();
                    h = h.max(u.height());
                    d = d.max(u.depth());
                }
                TextNode::Ins | TextNode::Mark | TextNode::Adjust => {
                    if let (Some(at), Some(pat)) = (adjust_tail.as_mut(), pre_adjust_tail.as_mut())
                    {
                        /*680: */
                        while llist_link(q) != Some(p) {
                            q = *LLIST_link(q) as usize;
                        }
                        if text_NODE_type(p) == TextNode::Adjust.into() {
                            let a = Adjust(p);
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
                TextNode::WhatsIt => match WhatsIt::from(p) {
                    WhatsIt::NativeWord(mut p_nw) => {
                        let mut k = if q != r.ptr() + 5 && NODE_type(q) == TextNode::Disc.into() {
                            let q = Discretionary(q);
                            q.replace_count() as i32
                        } else {
                            0
                        };
                        while llist_link(q) != Some(p) {
                            k -= 1;
                            q = *LLIST_link(q) as usize;
                            if NODE_type(q) == TextNode::Disc.into() {
                                let q = Discretionary(q);
                                k = q.replace_count() as i32
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
                                if NODE_type(ppp) == TextNode::WhatsIt.into() {
                                    total_chars += NativeWord::from(ppp).text().len() as i32;
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
                                if NODE_type(ppp) == TextNode::WhatsIt.into() {
                                    let nw = NativeWord::from(ppp);
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
                TextNode::Glue => {
                    let p = Glue(p);
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
                TextNode::Kern => {
                    let k = Kern(p);
                    x += k.width();
                }
                TextNode::MarginKern => {
                    let k = MarginKern(p);
                    x += k.width();
                }
                TextNode::Math => {
                    let p = Math(p);
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
                TextNode::Ligature => {
                    let l = Ligature(p);
                    let mut g = Char(GARBAGE);
                    g.set_character(l.char());
                    g.set_font(l.font());
                    *LLIST_link(GARBAGE) = *LLIST_link(l.ptr());
                    popt = Some(GARBAGE);
                    xtx_ligature_present = true;
                    continue;
                }
                _ => {}
            }
            popt = llist_link(p);
        }
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
    if x == 0 {
        r.set_glue_sign(GlueSign::Normal)
            .set_glue_order(GlueOrder::Normal)
            .set_glue_set(0.);
        return exit(r, q);
    } else if x > 0 {
        /*683: */
        let o = if total_stretch[GlueOrder::Filll as usize] != 0 {
            GlueOrder::Filll
        } else if total_stretch[GlueOrder::Fill as usize] != 0 {
            GlueOrder::Fill
        } else if total_stretch[GlueOrder::Fil as usize] != 0 {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        }; /*normal *//*:684 */
        r.set_glue_order(o).set_glue_sign(GlueSign::Stretching);
        if total_stretch[o as usize] != 0 {
            r.set_glue_set(x as f64 / total_stretch[o as usize] as f64);
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
        let o = if total_shrink[GlueOrder::Filll as usize] != 0 {
            GlueOrder::Filll
        } else if total_shrink[GlueOrder::Fill as usize] != 0 {
            GlueOrder::Fill
        } else if total_shrink[GlueOrder::Fil as usize] != 0 {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        };
        r.set_glue_order(o).set_glue_sign(GlueSign::Shrinking);
        if total_shrink[o as usize] != 0 {
            r.set_glue_set(-x as f64 / total_shrink[o as usize] as f64);
        } else {
            r.set_glue_sign(GlueSign::Normal).set_glue_set(0.);
        }
        if total_shrink[o as usize] < -x && o == GlueOrder::Normal && r.list_ptr().opt().is_some() {
            last_badness = 1000000;
            r.set_glue_set(1.);
            if -x - total_shrink[0] > *DIMENPAR(DimenPar::hfuzz) || *INTPAR(IntPar::hbadness) < 100
            {
                if *DIMENPAR(DimenPar::overfull_rule) > 0
                    && -x - total_shrink[0] > *DIMENPAR(DimenPar::hfuzz)
                {
                    while let Some(next) = LLIST_link(q as usize).opt() {
                        q = next;
                    }
                    *LLIST_link(q as usize) = Some(new_rule()).tex_int();
                    MEM[(*LLIST_link(q as usize) + 1) as usize].b32.s1 =
                        *DIMENPAR(DimenPar::overfull_rule)
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
        begin_diagnostic();
        show_box(Some(r.ptr()));
        end_diagnostic(true);
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
                    q = new_math(0, Math(LR_ptr as usize).subtype_i32());
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
    mut h: scaled_t,
    mut m: PackMode,
    mut l: scaled_t,
) -> List {
    last_badness = 0;
    let mut r = List::from(get_node(BOX_NODE_SIZE) as usize);
    set_NODE_type(r.ptr(), TextNode::VList);
    r.set_lr_mode(if *INTPAR(IntPar::xetex_upwards) > 0 {
        LRMode::Reversed
    } else {
        LRMode::Normal
    });
    r.set_shift_amount(0);
    r.set_list_ptr(popt.tex_int());
    let mut w = 0;
    let mut d = 0;
    let mut x = 0;
    total_stretch[NORMAL as usize] = 0;
    total_shrink[NORMAL as usize] = 0;
    total_stretch[FIL as usize] = 0;
    total_shrink[FIL as usize] = 0;
    total_stretch[FILL as usize] = 0;
    total_shrink[FILL as usize] = 0;
    total_stretch[FILLL as usize] = 0;
    total_shrink[FILLL as usize] = 0;
    while let Some(p) = popt {
        /*694: */
        if is_char_node(Some(p)) {
            confusion("vpack"); /*701: */
        } else {
            match text_NODE_type(p).unwrap() {
                TextNode::HList | TextNode::VList => {
                    let b = List::from(p);
                    x += d + b.height();
                    d = b.depth();
                    w = w.max(b.width() + b.shift_amount());
                }
                TextNode::Rule => {
                    let r = Rule::from(p);
                    x += d + r.height();
                    d = r.depth();
                    w = w.max(r.width());
                }
                TextNode::Unset => {
                    let u = Unset::from(p);
                    x += d + u.height();
                    d = u.depth();
                    w = w.max(u.width());
                }
                TextNode::WhatsIt => match WhatsIt::from(p) {
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        x += d + p.height();
                        d = p.depth();
                        w = w.max(p.width());
                    }
                    _ => {}
                },
                TextNode::Glue => {
                    let p = Glue(p);
                    x += d;
                    d = 0;
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
                TextNode::Kern => {
                    let k = Kern(p);
                    x += d + k.width();
                    d = 0;
                }
                _ => {}
            }
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
    if x == 0 {
        r.set_glue_sign(GlueSign::Normal)
            .set_glue_order(GlueOrder::Normal)
            .set_glue_set(0.);
    } else if x > 0 {
        /*698: */
        let o = if total_stretch[FILLL as usize] != 0 {
            GlueOrder::Filll
        } else if total_stretch[FILL as usize] != 0 {
            GlueOrder::Fill
        } else if total_stretch[FIL as usize] != 0 {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        }; /*normal *//*:684 */
        r.set_glue_order(o).set_glue_sign(GlueSign::Stretching);
        if total_stretch[o as usize] != 0 {
            r.set_glue_set(x as f64 / total_stretch[o as usize] as f64);
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
        let o = if total_shrink[FILLL as usize] != 0 {
            GlueOrder::Filll
        } else if total_shrink[FILL as usize] != 0 {
            GlueOrder::Fill
        } else if total_shrink[FIL as usize] != 0 {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        };
        r.set_glue_order(o).set_glue_sign(GlueSign::Shrinking);
        if total_shrink[o as usize] != 0 {
            r.set_glue_set(-x as f64 / total_shrink[o as usize] as f64);
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
        begin_diagnostic();
        show_box(Some(r.ptr()));
        end_diagnostic(true);
        return r;
    }
}
pub(crate) unsafe fn append_to_vlist(b: List) {
    let mut upwards: bool = false;
    upwards = *INTPAR(IntPar::xetex_upwards) > 0;
    if cur_list.aux.b32.s1 > IGNORE_DEPTH {
        let d = if upwards {
            MEM[(*GLUEPAR(GluePar::baseline_skip) + 1) as usize].b32.s1
                - cur_list.aux.b32.s1
                - b.depth()
        } else {
            MEM[(*GLUEPAR(GluePar::baseline_skip) + 1) as usize].b32.s1
                - cur_list.aux.b32.s1
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
    cur_list.aux.b32.s1 = if upwards { b.height() } else { b.depth() };
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
pub(crate) unsafe fn get_preamble_token() {
    loop {
        get_token();
        while cur_chr == SPAN_CODE && cur_cmd == Cmd::TabMark {
            get_token();
            if cur_cmd > MAX_COMMAND {
                expand();
                get_token();
            }
        }
        if cur_cmd == Cmd::EndV {
            fatal_error("(interwoven alignment preambles are not allowed)");
        }
        if !(cur_cmd == Cmd::AssignGlue && cur_chr == GLUE_BASE as i32 + GluePar::tab_skip as i32) {
            break;
        }
        scan_optional_equals();
        scan_glue(ValLevel::Glue);
        if *INTPAR(IntPar::global_defs) > 0 {
            geq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                cur_val.opt(),
            );
        } else {
            eq_define(
                (GLUE_BASE as usize) + (GluePar::tab_skip as usize),
                Cmd::GlueRef,
                cur_val.opt(),
            );
        }
    }
}
pub(crate) unsafe fn init_align() {
    let mut save_cs_ptr: i32 = 0;
    let mut p: i32 = 0;
    save_cs_ptr = cur_cs;
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
    scan_spec(GroupCode::Align, false);
    *LLIST_link(ALIGN_HEAD) = None.tex_int();
    let mut ca = ALIGN_HEAD;
    cur_align = Some(ca);
    cur_loop = None;
    scanner_status = ScannerStatus::Aligning;
    warning_index = save_cs_ptr;
    align_state = -1000000;
    loop {
        let ca2 = new_param_glue(GluePar::tab_skip);
        *LLIST_link(ca) = Some(ca2).tex_int();
        /*:808 */
        cur_align = Some(ca2); /*:807*/
        if cur_cmd == Cmd::CarRet {
            break; /*:813*/
        } /*:806 */
        p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            get_preamble_token();
            if cur_cmd == Cmd::MacParam {
                break;
            }
            if cur_cmd <= Cmd::CarRet && cur_cmd >= Cmd::TabMark && align_state as i64 == -1000000 {
                if p == HOLD_HEAD as i32 && cur_loop.is_none() && cur_cmd == Cmd::TabMark {
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
                    back_error();
                    break;
                }
            } else if cur_cmd != Cmd::Spacer || p != HOLD_HEAD as i32 {
                *LLIST_link(p as usize) = Some(get_avail()).tex_int();
                p = *LLIST_link(p as usize);
                MEM[p as usize].b32.s0 = cur_tok;
            }
        }
        ca = new_null_box();
        *LLIST_link(ca2) = Some(ca).tex_int();
        cur_align = Some(ca);
        MEM[ca].b32.s0 = END_SPAN as i32;
        MEM[ca + 1].b32.s1 = NULL_FLAG;
        MEM[ca + 3].b32.s1 = *LLIST_link(HOLD_HEAD);
        p = HOLD_HEAD as i32;
        *LLIST_link(p as usize) = None.tex_int();
        loop {
            get_preamble_token();
            if cur_cmd <= Cmd::CarRet && cur_cmd >= Cmd::TabMark && align_state as i64 == -1000000 {
                break;
            }
            if cur_cmd == Cmd::MacParam {
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
                MEM[p as usize].b32.s0 = cur_tok;
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
        begin_token_list(l, Btl::EveryCRText);
    }
    align_peek();
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
pub(crate) unsafe fn init_col() {
    let ca = cur_align.unwrap();
    MEM[ca + 5].b32.s0 = cur_cmd as i32;
    if cur_cmd == Cmd::Omit {
        align_state = 0;
    } else {
        back_input();
        begin_token_list(MEM[ca + 3].b32.s1 as usize, Btl::UTemplate);
    };
}
pub(crate) unsafe fn fin_col() -> bool {
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
            MEM[nb + 1].b32.s1 = NULL_FLAG;
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
        unsave();
        new_save_level(GroupCode::Align);
        let u;
        let w;
        if cur_list.mode == (true, ListMode::HMode) {
            adjust_tail = cur_tail;
            pre_adjust_tail = cur_pre_tail;
            u = hpack(MEM[cur_list.head].b32.s1.opt(), 0, PackMode::Additional);
            w = u.width();
            cur_tail = adjust_tail;
            adjust_tail = None;
            cur_pre_tail = pre_adjust_tail;
            pre_adjust_tail = None;
        } else {
            u = vpackage(MEM[cur_list.head].b32.s1.opt(), 0, PackMode::Additional, 0);
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
                MEM[s + 1].b32.s1 = w
            } else if MEM[(MEM[q].b32.s0 + 1) as usize].b32.s1 < w {
                MEM[(MEM[q].b32.s0 + 1) as usize].b32.s1 = w
            }
        } else if w > MEM[ca + 1].b32.s1 {
            MEM[ca + 1].b32.s1 = w
        }
        let mut u = Unset::from(u.ptr());
        set_NODE_type(u.ptr(), TextNode::Unset);
        u.set_columns(n as u16);
        let o = if total_stretch[FILLL as usize] != 0 {
            GlueOrder::Filll
        } else if total_stretch[FILL as usize] != 0 {
            GlueOrder::Fill
        } else if total_stretch[FIL as usize] != 0 {
            GlueOrder::Fil
        } else {
            GlueOrder::Normal
        };
        u.set_stretch_order(o);
        u.set_stretch(total_stretch[o as usize]);
        let o = if total_shrink[FILLL as usize] != 0 {
            GlueOrder::Filll
        } else if total_shrink[FILL as usize] != 0 {
            GlueOrder::Fill
        } else if total_shrink[FIL as usize] != 0 {
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
    loop {
        get_x_or_protected();
        if cur_cmd != Cmd::Spacer {
            break;
        }
    }
    cur_align = p;
    init_col();
    false
}
pub(crate) unsafe fn fin_row() {
    let p;
    if cur_list.mode == (true, ListMode::HMode) {
        p = hpack(MEM[cur_list.head].b32.s1.opt(), 0, PackMode::Additional);
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
            0,
            PackMode::Additional,
            MAX_HALFWORD,
        );
        pop_nest();
        *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
        cur_list.tail = p.ptr();
        cur_list.aux.b32.s0 = 1000;
    }
    let mut p = Unset::from(p.ptr());
    set_NODE_type(p.ptr(), TextNode::Unset);
    p.set_stretch(0);
    if let Some(ecr) = LOCAL(Local::every_cr).opt() {
        begin_token_list(ecr, Btl::EveryCRText);
    }
    align_peek();
}
pub(crate) unsafe fn fin_align() {
    let mut t: scaled_t = 0;
    let mut w: scaled_t = 0;
    let mut n: i32 = 0;
    let mut rule_save: scaled_t = 0;
    let mut aux_save: memory_word = memory_word {
        b32: b32x2 { s0: 0, s1: 0 },
    };
    if cur_group != GroupCode::Align {
        confusion("align1");
    }
    unsave();
    if cur_group != GroupCode::Align {
        confusion("align0");
    }
    unsave();
    let o = if NEST[(NEST_PTR - 1) as usize].mode == (false, ListMode::MMode) {
        *DIMENPAR(DimenPar::display_indent)
    } else {
        0
    };
    let mut q = MEM[*LLIST_link(ALIGN_HEAD) as usize].b32.s1 as usize;
    loop {
        flush_list(MEM[q + 3].b32.s1.opt());
        flush_list(MEM[q + 2].b32.s1.opt());
        let p = MEM[*LLIST_link(q) as usize].b32.s1.opt();
        if MEM[q + 1].b32.s1 == NULL_FLAG {
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
            t = MEM[q + 1].b32.s1
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
        q_unset.set_height(0).set_depth(0);
        q_unset
            .set_stretch_order(GlueOrder::Normal)
            .set_shrink_order(GlueOrder::Normal)
            .set_stretch(0)
            .set_shrink(0);
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
        *DIMENPAR(DimenPar::overfull_rule) = 0;
        p = hpack(
            llist_link(ALIGN_HEAD),
            SAVE_STACK[SAVE_PTR + 1].val,
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
            SAVE_STACK[SAVE_PTR + 1].val,
            PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
            MAX_HALFWORD,
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
        if !is_char_node(Some(q)) {
            if NODE_type(q) == TextNode::Unset.into() {
                /*836: */
                let mut q = List::from(q);
                if cur_list.mode == (true, ListMode::VMode) {
                    set_NODE_type(q.ptr(), TextNode::HList);
                    q.set_width(p.width());
                    if NEST[NEST_PTR - 1].mode == (false, ListMode::MMode) {
                        q.set_lr_mode(LRMode::DList);
                    }
                } else {
                    set_NODE_type(q.ptr(), TextNode::VList);
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
                    t = BaseBox(s).width(); // TODO: check
                    w = t;
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
                                t += tex_round(p.glue_set() * v.stretch() as f64);
                            }
                        } else if p.glue_sign() == GlueSign::Shrinking {
                            if v.shrink_order() == p.glue_order() {
                                t -= tex_round(p.glue_set() * v.shrink() as f64);
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
                            set_NODE_type(nb.ptr(), TextNode::VList);
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
                                if r_unset.stretch() == 0 {
                                    0.
                                } else {
                                    (t - r_unset.width()) as f64 / r_unset.stretch() as f64
                                },
                            );
                        } else {
                            r_box
                                .set_glue_order(r_unset.shrink_order())
                                .set_glue_sign(GlueSign::Shrinking);
                            r_box.set_glue_set(if r_unset.shrink() == 0 {
                                0.
                            } else if r_box.glue_order() == GlueOrder::Normal
                                && r_unset.width() - t > r_unset.shrink()
                            {
                                1.
                            } else {
                                (r_unset.width() - t) as f64 / r_unset.shrink() as f64
                                // BOX_glue
                            });
                        }
                        r_box.set_width(w);
                        set_NODE_type(r, TextNode::HList);
                    } else {
                        r_box.set_width(q.width());
                        if t == r_unset.height() {
                            r_box
                                .set_glue_sign(GlueSign::Normal)
                                .set_glue_order(GlueOrder::Normal)
                                .set_glue_set(0.);
                        } else if t > r_unset.height() {
                            r_box.set_glue_sign(GlueSign::Stretching).set_glue_set(
                                if r_unset.stretch() == 0 {
                                    0.
                                } else {
                                    (t - r_unset.height()) as f64 / r_unset.stretch() as f64
                                },
                            );
                        } else {
                            r_box
                                .set_glue_order(r_unset.shrink_order())
                                .set_glue_sign(GlueSign::Shrinking);
                            r_box.set_glue_set(if r_unset.shrink() == 0 {
                                0.
                            } else if r_box.glue_order() == GlueOrder::Normal
                                && r_unset.height() - t > r_unset.shrink()
                            {
                                1.
                            } else {
                                (r_unset.height() - t) as f64 / r_unset.shrink() as f64
                            });
                        }
                        r_box.set_height(w);
                        set_NODE_type(r, TextNode::VList);
                    }
                    r_box.set_shift_amount(0);
                    if u != HOLD_HEAD {
                        *LLIST_link(u) = *LLIST_link(r);
                        *LLIST_link(r) = *LLIST_link(HOLD_HEAD);
                        r = u;
                    }
                    let ropt = MEM[*LLIST_link(r) as usize].b32.s1.opt();
                    s = MEM[*LLIST_link(s) as usize].b32.s1 as usize;
                    if let Some(r_) = ropt {
                        r = r_;
                    } else {
                        break;
                    }
                }
            } else if NODE_type(q) == TextNode::Rule.into() {
                /*835: */
                let mut q_rule = Rule::from(q);
                if q_rule.width() == NULL_FLAG {
                    q_rule.set_width(p.width());
                }
                if q_rule.height() == NULL_FLAG {
                    q_rule.set_height(p.height());
                }
                if q_rule.depth() == NULL_FLAG {
                    q_rule.set_depth(p.depth());
                }
                if o != 0 {
                    let r = *LLIST_link(q);
                    *LLIST_link(q) = None.tex_int();
                    let mut q_box = hpack(Some(q), 0, PackMode::Additional);
                    q_box.set_shift_amount(o);
                    q = q_box.ptr();
                    *LLIST_link(q) = r;
                    *LLIST_link(s) = Some(q).tex_int();
                }
            }
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
        do_assignments(); /*1232: */
        if cur_cmd != Cmd::MathShift {
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
            back_error();
        } else {
            get_x_token();
            if cur_cmd != Cmd::MathShift {
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
                back_error();
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
        resume_after_display();
    } else {
        cur_list.aux = aux_save;
        *LLIST_link(cur_list.tail) = p.tex_int();
        if p.is_some() {
            cur_list.tail = q;
        }
        if cur_list.mode == (false, ListMode::VMode) {
            build_page();
        }
    };
}
pub(crate) unsafe fn align_peek() {
    loop {
        align_state = 1000000;
        loop {
            get_x_or_protected();
            if cur_cmd != Cmd::Spacer {
                break;
            }
        }
        if cur_cmd == Cmd::NoAlign {
            scan_left_brace();
            new_save_level(GroupCode::NoAlign);
            if cur_list.mode == (true, ListMode::VMode) {
                normal_paragraph();
            }
            break;
        } else if cur_cmd == Cmd::RightBrace {
            fin_align();
            break;
        } else {
            if cur_cmd == Cmd::CarRet && cur_chr == CR_CR_CODE {
                continue;
            }
            init_row();
            init_col();
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
pub(crate) unsafe fn show_save_groups() {
    unsafe fn do_loop(mut p: usize, mut a: i8) -> (bool, usize, i8) {
        print_nl_cstr("### ");
        print_group(true);
        if cur_group == GroupCode::BottomLevel {
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
        match cur_group {
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
                if cur_group == GroupCode::Disc {
                    print_esc_cstr("discretionary");
                } else {
                    print_esc_cstr("mathchoice");
                }
                let mut i = 1;
                while i <= 3 {
                    if i <= SAVE_STACK[SAVE_PTR - 2].val {
                        print_cstr("{}");
                    }
                    i += 1;
                }
                return found2(p, a);
            }
            GroupCode::Insert => {
                if SAVE_STACK[SAVE_PTR - 2].val == 255 {
                    print_esc_cstr("vadjust");
                } else {
                    print_esc_cstr("insert");
                    print_int(SAVE_STACK[SAVE_PTR - 2].val);
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
                    print_cmd_chr(Cmd::EqNo, SAVE_STACK[SAVE_PTR - 2].val);
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

        let mut i = SAVE_STACK[SAVE_PTR - 4].val;

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
                print_scaled(i.abs());
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
        if SAVE_STACK[SAVE_PTR - 2].val != 0 {
            print_chr(' ');
            if SAVE_STACK[SAVE_PTR - 3].val == PackMode::Exactly as i32 {
                print_cstr("to");
            } else {
                print_cstr("spread");
            }
            print_scaled(SAVE_STACK[SAVE_PTR - 2].val);
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
        cur_level = cur_level.wrapping_sub(1);
        cur_group = GroupCode::from(SAVE_STACK[SAVE_PTR].lvl);
        SAVE_PTR = SAVE_STACK[SAVE_PTR].val as usize;
        (false, p, a)
    }

    unsafe fn done(v: i32, l: u16, c: GroupCode) {
        SAVE_PTR = v as usize;
        cur_level = l;
        cur_group = c;
    }

    let mut p = NEST_PTR;
    NEST[p] = cur_list;
    let v = SAVE_PTR as i32;
    let l = cur_level;
    let c = cur_group;
    SAVE_PTR = cur_boundary as usize;
    cur_level = cur_level.wrapping_sub(1);
    let mut a = 1_i8;
    print_nl_cstr("");
    print_ln();
    loop {
        let (is_done, new_p, new_a) = do_loop(p, a);
        p = new_p;
        a = new_a;
        if is_done {
            return done(v, l, c);
        }
    }
}
pub(crate) unsafe fn vert_break(mut p: i32, mut h: scaled_t, mut d: scaled_t) -> i32 {
    let mut current_block: u64;
    let mut pi: i32 = 0;
    let mut b: i32 = 0;
    let mut best_place: i32 = None.tex_int();
    let mut prev_p = p;
    let mut least_cost = MAX_HALFWORD;
    active_width[1..].copy_from_slice(&[0; 6]);
    let mut prev_dp = 0;
    loop {
        if let Some(p) = p.opt() {
            /*1008: */
            match text_NODE_type(p).unwrap() {
                TextNode::HList | TextNode::VList => {
                    let b = List::from(p);
                    active_width[1] += prev_dp + b.height();
                    prev_dp = b.depth();
                    current_block = 10249009913728301645;
                }
                TextNode::Rule => {
                    let r = Rule::from(p);
                    active_width[1] += prev_dp + r.height();
                    prev_dp = r.depth();
                    current_block = 10249009913728301645;
                }
                TextNode::WhatsIt => {
                    match WhatsIt::from(p as usize) {
                        WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                            active_width[1] += prev_dp + p.height();
                            prev_dp = p.depth();
                        }
                        _ => {}
                    }
                    current_block = 10249009913728301645;
                }
                TextNode::Glue => match TxtNode::from(prev_p as usize) {
                    TxtNode::HList(_)
                    | TxtNode::VList(_)
                    | TxtNode::Rule(_)
                    | TxtNode::Ins(_)
                    | TxtNode::Mark(_)
                    | TxtNode::Adjust(_)
                    | TxtNode::Ligature(_)
                    | TxtNode::Disc(_)
                    | TxtNode::WhatsIt(_) => {
                        pi = 0;
                        current_block = 9007357115414505193;
                    }
                    _ => current_block = 11492179201936201469,
                },
                TextNode::Kern => {
                    let t = if let Some(next) = llist_link(p) {
                        NODE_type(next)
                    } else {
                        TextNode::Penalty.into()
                    };
                    if t == TextNode::Glue.into() {
                        pi = 0;
                        current_block = 9007357115414505193;
                    } else {
                        current_block = 11492179201936201469;
                    }
                }
                TextNode::Penalty => {
                    let p = Penalty(p);
                    pi = p.penalty();
                    current_block = 9007357115414505193;
                }
                TextNode::Mark | TextNode::Ins => current_block = 10249009913728301645,
                _ => confusion("vertbreak"),
            }
        } else {
            pi = EJECT_PENALTY;
            current_block = 9007357115414505193;
        }
        match current_block {
            9007357115414505193 => {
                if pi < INF_PENALTY {
                    b = if active_width[1] < h {
                        if active_width[3] != 0 || active_width[4] != 0 || active_width[5] != 0 {
                            0
                        } else {
                            badness(h - active_width[1], active_width[2])
                        }
                    } else if active_width[1] - h > active_width[6] {
                        MAX_HALFWORD
                    } else {
                        badness(active_width[1] - h, active_width[6])
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
                    if b <= least_cost {
                        best_place = p;
                        least_cost = b;
                        best_height_plus_depth = active_width[1] + prev_dp
                    }
                    if b == MAX_HALFWORD || pi <= EJECT_PENALTY {
                        return best_place;
                    }
                }
                if NODE_type(p as usize).u16() < TextNode::Glue as u16
                    || NODE_type(p as usize).u16() > TextNode::Kern as u16
                {
                    current_block = 10249009913728301645;
                } else {
                    current_block = 11492179201936201469;
                }
            }
            _ => {}
        }
        match current_block {
            11492179201936201469 => {
                /*update_heights *//*1011: */
                let width = if NODE_type(p as usize) == TextNode::Kern.into() {
                    Kern(p as usize).width()
                } else {
                    let mut p = Glue(p as usize);
                    let mut q = GlueSpec(p.glue_ptr() as usize); /*:1011 */
                    active_width[2 + q.stretch_order() as usize] += q.stretch(); /*:1014*/
                    active_width[6] += q.shrink();
                    if q.shrink_order() != GlueOrder::Normal && q.shrink() != 0 {
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
                        p.set_glue_ptr(Some(r.ptr()).tex_int());
                        r.size()
                    } else {
                        q.size()
                    }
                };
                active_width[1] += prev_dp + width;
                prev_dp = 0;
            }
            _ => {}
        }
        if prev_dp > d {
            active_width[1] += prev_dp - d;
            prev_dp = d
        }
        prev_p = p;
        p = *LLIST_link(p as usize);
    }
}
pub(crate) unsafe fn vsplit(mut n: i32, mut h: scaled_t) -> Option<usize> {
    cur_val = n;
    let v = if cur_val < 256 {
        BOX_REG(cur_val as usize).opt()
    } else {
        find_sa_element(ValLevel::Ident, cur_val, false);
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
    if NODE_type(v as usize) != TextNode::VList.into() {
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
    let mut v = List::from(v);
    let q = vert_break(v.list_ptr(), h, *DIMENPAR(DimenPar::split_max_depth));
    let mut p = v.list_ptr();
    if p == q {
        v.set_list_ptr(None.tex_int());
    } else {
        loop {
            if NODE_type(p as usize) == TextNode::Mark.into() {
                let p = Mark(p as usize);
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
        Some(vpackage(Some(q), 0, PackMode::Additional, MAX_HALFWORD).ptr())
    } else {
        None
    };
    if cur_val < 256 {
        *BOX_REG(cur_val as usize) = q.tex_int();
    } else {
        find_sa_element(ValLevel::Ident, cur_val, false);
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
    if page_so_far[2] != 0 {
        print_cstr(" plus ");
        print_scaled(page_so_far[2]);
        print_cstr("");
    }
    if page_so_far[3] != 0 {
        print_cstr(" plus ");
        print_scaled(page_so_far[3]);
        print_cstr("fil");
    }
    if page_so_far[4] != 0 {
        print_cstr(" plus ");
        print_scaled(page_so_far[4]);
        print_cstr("fill");
    }
    if page_so_far[5] != 0 {
        print_cstr(" plus ");
        print_scaled(page_so_far[5]);
        print_cstr("filll");
    }
    if page_so_far[6] != 0 {
        print_cstr(" minus ");
        print_scaled(page_so_far[6]);
    };
}
pub(crate) unsafe fn box_error(mut n: u8) {
    error();
    begin_diagnostic();
    print_nl_cstr("The following box has been deleted:");
    show_box(BOX_REG(n as usize).opt());
    end_diagnostic(true);
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
                        .set_size(FONT_INFO[main_k as usize].b32.s1)
                        .set_stretch(FONT_INFO[(main_k + 1) as usize].b32.s1)
                        .set_shrink(FONT_INFO[(main_k + 2) as usize].b32.s1);
                    FONT_GLUE[EQTB[CUR_FONT_LOC].val as usize] = Some(main_p).tex_int();
                    main_p
                })
        };
        let mut main_p = GlueSpec(new_spec(main_p));
        if cur_list.aux.b32.s0 >= 2000 {
            main_p.set_size(
                main_p.size()
                    + FONT_INFO
                        [(EXTRA_SPACE_CODE + PARAM_BASE[EQTB[CUR_FONT_LOC].val as usize]) as usize]
                        .b32
                        .s1,
            );
        }
        main_p.set_stretch(xn_over_d(main_p.stretch(), cur_list.aux.b32.s0, 1000));
        main_p.set_shrink(xn_over_d(main_p.shrink(), 1000, cur_list.aux.b32.s0));
        q = new_glue(main_p.ptr());
        main_p.rc_none();
    }
    *LLIST_link(cur_list.tail) = Some(q).tex_int();
    cur_list.tail = q;
}
pub(crate) unsafe fn insert_dollar_sign() {
    back_input();
    cur_tok = MATH_SHIFT_TOKEN + 36;
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
    ins_error();
}
pub(crate) unsafe fn you_cant() {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("You can\'t use `");
    print_cmd_chr(cur_cmd, cur_chr);
    print_in_mode(cur_list.mode);
}
pub(crate) unsafe fn report_illegal_case() {
    you_cant();
    help!(
        "Sorry, but I\'m not programmed to handle this case;",
        "I\'ll just pretend that you didn\'t ask for it.",
        "If you\'re in the wrong mode, you might be able to",
        "return to the right one by typing `I}\' or `I$\' or `I\\par\'."
    );
    error();
}
pub(crate) unsafe fn privileged() -> bool {
    if cur_list.mode.0 == false {
        true
    } else {
        report_illegal_case();
        false
    }
}
pub(crate) unsafe fn its_all_over() -> bool {
    if privileged() {
        if PAGE_HEAD == page_tail && cur_list.head == cur_list.tail && dead_cycles == 0 {
            return true;
        }
        back_input();
        let nb = new_null_box();
        *LLIST_link(cur_list.tail) = Some(nb).tex_int();
        cur_list.tail = nb;
        MEM[cur_list.tail + 1].b32.s1 = *DIMENPAR(DimenPar::hsize);
        let g = new_glue(8);
        *LLIST_link(cur_list.tail) = Some(g).tex_int();
        cur_list.tail = g;
        let p = new_penalty(NULL_FLAG);
        *LLIST_link(cur_list.tail) = Some(p).tex_int();
        cur_list.tail = p;
        build_page();
    }
    false
}
pub(crate) unsafe fn append_glue() {
    let s = SkipCode::n(cur_chr as u8).unwrap();
    match s {
        SkipCode::Fil => cur_val = 4,
        SkipCode::Fill => cur_val = 8,
        SkipCode::Ss => cur_val = 12,
        SkipCode::FilNeg => cur_val = 16,
        SkipCode::Skip => scan_glue(ValLevel::Glue),
        SkipCode::MSkip => scan_glue(ValLevel::Mu),
    }
    let g = new_glue(cur_val as usize);
    *LLIST_link(cur_list.tail) = Some(g).tex_int();
    cur_list.tail = g;
    if s == SkipCode::Skip || s == SkipCode::MSkip {
        MEM[cur_val as usize].b32.s1 -= 1;
        if s == SkipCode::MSkip {
            MEM[g].b16.s0 = MU_GLUE;
        }
    };
}
pub(crate) unsafe fn append_kern() {
    let s = cur_chr as u16;
    scan_dimen(s == KernType::Math as u16, false, false);
    let k = new_kern(cur_val);
    *LLIST_link(cur_list.tail) = Some(k).tex_int();
    cur_list.tail = k;
    MEM[cur_list.tail].b16.s0 = s;
}
pub(crate) unsafe fn off_save() {
    if cur_group == GroupCode::BottomLevel {
        /*1101:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Extra ");
        print_cmd_chr(cur_cmd, cur_chr);
        help!("Things are pretty mixed up, but I think the worst is over.");
        error();
    } else {
        back_input();
        let mut p = get_avail();
        *LLIST_link(TEMP_HEAD) = Some(p).tex_int();
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Missing ");
        match cur_group {
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
        begin_token_list(*LLIST_link(TEMP_HEAD) as usize, Btl::Inserted);
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
pub(crate) unsafe fn extra_right_brace() {
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Extra }, or forgotten ");
    match cur_group {
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
    if *DIMENPAR(DimenPar::hang_indent) != 0 {
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
pub(crate) unsafe fn box_end(mut box_context: i32) {
    let mut a: i16 = 0;
    if box_context < BOX_FLAG {
        /*1111:*/
        if let Some(mut cb) = cur_box {
            List::from(cb).set_shift_amount(box_context);
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
                    build_page();
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
        if box_context < GLOBAL_BOX_FLAG {
            cur_val = box_context - BOX_FLAG;
            a = 0
        } else {
            cur_val = box_context - GLOBAL_BOX_FLAG;
            a = 4
        }
        if cur_val < 256 {
            if a >= 4 {
                geq_define(BOX_BASE + cur_val as usize, Cmd::BoxRef, cur_box);
            } else {
                eq_define(BOX_BASE + cur_val as usize, Cmd::BoxRef, cur_box);
            }
        } else {
            find_sa_element(ValLevel::Ident, cur_val, true);
            if a >= 4 {
                gsa_def(cur_ptr.unwrap(), cur_box);
            } else {
                sa_def(cur_ptr.unwrap(), cur_box);
            }
        }
    } else if let Some(cb) = cur_box {
        if box_context > SHIP_OUT_FLAG {
            loop
            /*1113:*/
            {
                get_x_token();
                if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
                    break;
                }
            }
            if cur_cmd == Cmd::HSkip && cur_list.mode.1 != ListMode::VMode
                || cur_cmd == Cmd::VSkip && cur_list.mode.1 == ListMode::VMode
            {
                append_glue();
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
                back_error();
                flush_node_list(Some(cb));
            }
        } else {
            ship_out(List::from(cb));
        }
    };
}
pub(crate) unsafe fn begin_box(mut box_context: i32) {
    match BoxCode::n(cur_chr as u8).unwrap() {
        BoxCode::Box => {
            scan_register_num();
            cur_box = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            if cur_val < 256 {
                *BOX_REG(cur_val as usize) = None.tex_int()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                if let Some(c) = cur_ptr {
                    MEM[c + 1].b32.s1 = None.tex_int();
                    MEM[c + 1].b32.s0 += 1;
                    delete_sa_ref(c);
                }
            }
        }
        BoxCode::Copy => {
            scan_register_num();
            let q = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            cur_box = copy_node_list(q).opt();
        }
        BoxCode::LastBox => {
            cur_box = None;
            if cur_list.mode.1 == ListMode::MMode {
                you_cant();
                help!("Sorry; this \\lastbox will be void.");
                error();
            } else if cur_list.mode == (false, ListMode::VMode) && cur_list.head == cur_list.tail {
                you_cant();
                help!(
                    "Sorry...I usually can\'t take things from the current page.",
                    "This \\lastbox will therefore be void."
                );
                error();
            } else {
                let mut current_block_79: u64;
                let mut tx = cur_list.tail as i32;
                if tx < hi_mem_min {
                    if NODE_type(tx as usize) == TextNode::Math.into()
                        && Math(tx as usize).subtype() == MathType::Eq(BE::End, MathMode::Middle)
                    {
                        let mut r = cur_list.head as i32;
                        let mut q: i32 = 0;
                        loop {
                            q = r;
                            r = *LLIST_link(q as usize);
                            if !(r != tx) {
                                break;
                            }
                        }
                        tx = q
                    }
                }
                if tx < hi_mem_min {
                    if NODE_type(tx as usize) == TextNode::HList.into()
                        || NODE_type(tx as usize) == TextNode::VList.into()
                    {
                        /*1116:*/
                        let mut q = cur_list.head as i32;
                        let mut p = None.tex_int();
                        let mut r: i32 = 0;
                        let mut fm: bool = false;
                        loop {
                            r = p;
                            p = q;
                            fm = false;
                            if q < hi_mem_min {
                                if NODE_type(q as usize) == TextNode::Disc.into() {
                                    for _ in 0..Discretionary(q as usize).replace_count() {
                                        p = *LLIST_link(p as usize);
                                    }
                                    if p == tx {
                                        current_block_79 = 1209030638129645089;
                                        break;
                                    }
                                } else if NODE_type(q as usize) == TextNode::Math.into()
                                    && Math(q as usize).subtype()
                                        == MathType::Eq(BE::Begin, MathMode::Middle)
                                {
                                    fm = true;
                                }
                            }
                            q = *LLIST_link(p as usize);
                            if !(q != tx) {
                                current_block_79 = 12961834331865314435;
                                break;
                            }
                        }
                        match current_block_79 {
                            1209030638129645089 => {}
                            _ => {
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
                                MEM[(tx + 4) as usize].b32.s1 = 0
                            }
                        }
                    }
                }
            }
        }
        BoxCode::VSplit => {
            scan_register_num();
            let n = cur_val;
            if !scan_keyword(b"to") {
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
            scan_dimen(false, false, false);
            cur_box = vsplit(n, cur_val);
        }
        _ => {
            let k = cur_chr - 4;
            let mut k = (k < 0, ListMode::from(k.abs() as u8));
            SAVE_STACK[SAVE_PTR + 0].val = box_context;
            if k == (false, ListMode::HMode) {
                if box_context < BOX_FLAG && cur_list.mode.1 == ListMode::VMode {
                    scan_spec(GroupCode::AdjustedHBox, true);
                } else {
                    scan_spec(GroupCode::HBox, true);
                }
            } else {
                if k == (false, ListMode::VMode) {
                    scan_spec(GroupCode::VBox, true);
                } else {
                    scan_spec(GroupCode::VTop, true);
                    k = (false, ListMode::VMode);
                }
                normal_paragraph();
            }
            push_nest();
            cur_list.mode = (!k.0, k.1);
            if k == (false, ListMode::VMode) {
                cur_list.aux.b32.s1 = IGNORE_DEPTH;
                if let Some(ev) = LOCAL(Local::every_vbox).opt() {
                    begin_token_list(ev, Btl::EveryVBoxText);
                }
            } else {
                cur_list.aux.b32.s0 = 1000;
                if let Some(eh) = LOCAL(Local::every_hbox).opt() {
                    begin_token_list(eh, Btl::EveryHBoxText);
                }
            }
            return;
        }
    }
    box_end(box_context);
}
pub(crate) unsafe fn scan_box(mut box_context: i32) {
    loop {
        get_x_token();
        if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
            break;
        }
    }
    if cur_cmd == Cmd::MakeBox {
        begin_box(box_context);
    } else if box_context >= LEADER_FLAG && (cur_cmd == Cmd::HRule || cur_cmd == Cmd::VRule) {
        cur_box = Some(scan_rule_spec());
        box_end(box_context);
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
        back_error();
    };
}
pub(crate) unsafe fn package(mut c: i16) {
    let d = *DIMENPAR(DimenPar::box_max_depth);
    let u = *INTPAR(IntPar::xetex_upwards);
    unsave();
    SAVE_PTR -= 3;
    let v = *INTPAR(IntPar::xetex_upwards);
    *INTPAR(IntPar::xetex_upwards) = u;
    if cur_list.mode == (true, ListMode::HMode) {
        cur_box = Some(
            hpack(
                MEM[cur_list.head].b32.s1.opt(),
                SAVE_STACK[SAVE_PTR + 2].val,
                PackMode::from(SAVE_STACK[SAVE_PTR + 1].val),
            )
            .ptr(),
        );
    } else {
        let mut cb = vpackage(
            MEM[cur_list.head].b32.s1.opt(),
            SAVE_STACK[SAVE_PTR + 2].val,
            PackMode::from(SAVE_STACK[SAVE_PTR + 1].val),
            d,
        );
        cur_box = Some(cb.ptr());
        if c == BoxCode::VTop as i16 {
            /*1122: */
            let mut h = 0;
            if let Some(p) = cb.list_ptr().opt() {
                if [
                    TextNode::HList.into(),
                    TextNode::VList.into(),
                    TextNode::Rule.into(),
                ]
                .contains(&NODE_type(p))
                {
                    h = MEM[p + 3].b32.s1
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
    box_end(SAVE_STACK[SAVE_PTR + 0].val);
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
pub(crate) unsafe fn new_graf(mut indented: bool) {
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
        begin_token_list(ep, Btl::EveryParText);
    }
    if NEST_PTR == 1 {
        build_page();
    };
}
pub(crate) unsafe fn indent_in_hmode() {
    if cur_chr > 0 {
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
pub(crate) unsafe fn head_for_vmode() {
    if cur_list.mode.0 == true {
        if cur_cmd != Cmd::HRule {
            off_save();
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
        back_input();
        cur_tok = par_token;
        back_input();
        cur_input.index = Btl::Inserted;
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
pub(crate) unsafe fn begin_insert_or_adjust() {
    if cur_cmd == Cmd::VAdjust {
        cur_val = 255;
    } else {
        scan_eight_bit_int();
        if cur_val == 255 {
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
            cur_val = 0;
        }
    }
    SAVE_STACK[SAVE_PTR + 0].val = cur_val;
    if cur_cmd == Cmd::VAdjust && scan_keyword(b"pre") {
        SAVE_STACK[SAVE_PTR + 1].val = 1;
    } else {
        SAVE_STACK[SAVE_PTR + 1].val = 0;
    }
    SAVE_PTR += 2;
    new_save_level(GroupCode::Insert);
    scan_left_brace();
    normal_paragraph();
    push_nest();
    cur_list.mode = (true, ListMode::VMode);
    cur_list.aux.b32.s1 = IGNORE_DEPTH;
}
pub(crate) unsafe fn make_mark() {
    let c = if cur_chr == 0 {
        0
    } else {
        scan_register_num();
        cur_val
    };
    let _p = scan_toks(false, true);
    let mut p = Mark(get_node(SMALL_NODE_SIZE));
    p.set_class(c);
    set_NODE_type(p.ptr() as usize, TextNode::Mark);
    MEM[p.ptr()].b16.s0 = 0;
    p.set_mark_ptr(def_ref as i32);
    *LLIST_link(cur_list.tail) = Some(p.ptr()).tex_int();
    cur_list.tail = p.ptr();
}
pub(crate) unsafe fn append_penalty() {
    scan_int();
    let p = new_penalty(cur_val);
    *LLIST_link(cur_list.tail) = Some(p).tex_int();
    cur_list.tail = p;
    if cur_list.mode == (false, ListMode::VMode) {
        build_page();
    };
}
pub(crate) unsafe fn delete_last() {
    if cur_list.mode == (false, ListMode::VMode) && cur_list.tail == cur_list.head {
        /*1141: */
        if cur_chr != TextNode::Glue as i32 || last_glue != MAX_HALFWORD {
            you_cant();
            let help0 = if cur_chr == TextNode::Kern as i32 {
                &"Try `I\\kern-\\lastkern\' instead."[..]
            } else if cur_chr != TextNode::Glue as i32 {
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
        if !is_char_node(Some(tx)) {
            if NODE_type(tx) == TextNode::Math.into()
                && Math(tx).subtype() == MathType::Eq(BE::End, MathMode::Middle)
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
        }
        if !is_char_node(Some(tx)) {
            if MEM[tx].b16.s1 as i32 == cur_chr {
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
pub(crate) unsafe fn unpackage() {
    let c = BoxCode::n(cur_chr as u8).unwrap();
    match c {
        BoxCode::Box | BoxCode::Copy => {
            scan_register_num();
            let p = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
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
                    if cur_val < 256 {
                        *BOX_REG(cur_val as usize) = None.tex_int()
                    } else {
                        find_sa_element(ValLevel::Ident, cur_val, false);
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
            *LLIST_link(cur_list.tail) = disc_ptr[cur_chr as usize]; /*:1156 */
            disc_ptr[cur_chr as usize] = None.tex_int();
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
pub(crate) unsafe fn append_discretionary() {
    let d = new_disc();
    *LLIST_link(cur_list.tail) = Some(d).tex_int();
    cur_list.tail = d;
    if cur_chr == 1 {
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
        scan_left_brace();
        push_nest();
        cur_list.mode = (true, ListMode::HMode);
        cur_list.aux.b32.s0 = 1000;
    };
}
pub(crate) unsafe fn build_discretionary() {
    unsave();
    let mut q = cur_list.head;
    let mut popt = llist_link(q);
    let mut n = 0;
    while let Some(p) = popt {
        match CharOrText::from(p) {
            CharOrText::Char(_) => {}
            CharOrText::Text(n) => match n {
                TxtNode::HList(_)
                | TxtNode::VList(_)
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
                    begin_diagnostic();
                    print_nl_cstr("The following discretionary sublist has been deleted:");
                    show_box(Some(p));
                    end_diagnostic(true);
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
    scan_left_brace();
    push_nest();
    cur_list.mode = (true, ListMode::HMode);
    cur_list.aux.b32.s0 = 1000;
}
pub(crate) unsafe fn make_accent() {
    scan_char_num();
    let mut f = EQTB[CUR_FONT_LOC].val as usize;
    if let Some(mut p) = new_character(f, cur_val as UTF16_code) {
        let mut lsb: scaled_t = 0;
        let mut rsb: scaled_t = 0;
        let x = FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1;
        let s = FONT_INFO[(SLANT_CODE + PARAM_BASE[f]) as usize].b32.s1 as f64 / 65536.;
        let a = if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
            let a = NativeWord::from(p).width();
            if a == 0 {
                get_native_char_sidebearings(f, cur_val, &mut lsb, &mut rsb);
            }
            a
        } else {
            *FONT_CHARACTER_WIDTH(f, effective_char(true, f, Char(p).character()) as usize)
        };
        do_assignments();
        let mut q = None;
        f = EQTB[CUR_FONT_LOC].val as usize;
        if cur_cmd == Cmd::Letter || cur_cmd == Cmd::OtherChar || cur_cmd == Cmd::CharGiven {
            q = new_character(f, cur_chr as UTF16_code);
            cur_val = cur_chr
        } else if cur_cmd == Cmd::CharNum {
            scan_char_num();
            q = new_character(f, cur_val as UTF16_code)
        } else {
            back_input();
        }
        if let Some(q) = q {
            /*1160: */
            let mut h;
            let mut w;
            let t = FONT_INFO[(SLANT_CODE + PARAM_BASE[f]) as usize].b32.s1 as f64 / 65536.;
            if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
                w = NativeWord::from(q).width();
                h = get_native_char_height_depth(f, cur_val).0;
            } else {
                let i =
                    FONT_CHARACTER_INFO(f, effective_char(true, f, Char(q).character()) as usize);
                w = *FONT_CHARINFO_WIDTH(f, i);
                h = *FONT_CHARINFO_HEIGHT(f, i);
            }
            if h != x {
                let mut p_box = hpack(Some(p), 0, PackMode::Additional);
                p_box.set_shift_amount(x - h);
                p = p_box.ptr();
            }
            let delta = if (FONT_AREA[f] as u32 == AAT_FONT_FLAG
                || FONT_AREA[f] as u32 == OTGR_FONT_FLAG)
                && a == 0
            {
                tex_round((w - lsb + rsb) as f64 / 2. + h as f64 * t - x as f64 * s)
            } else {
                tex_round((w - a) as f64 / 2. + h as f64 * t - x as f64 * s)
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
pub(crate) unsafe fn align_error() {
    if align_state.abs() > 2 {
        /*1163: */
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Misplaced ");
        print_cmd_chr(cur_cmd, cur_chr);
        if cur_tok == TAB_TOKEN + 38 {
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
        back_input();
        if align_state < 0 {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing { inserted");
            align_state += 1;
            cur_tok = LEFT_BRACE_TOKEN + 123;
        } else {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing } inserted");
            align_state -= 1;
            cur_tok = RIGHT_BRACE_TOKEN + 125;
        }
        help!(
            "I\'ve put in what seems to be necessary to fix",
            "the current column of the current alignment.",
            "Try to go on, since this might almost work."
        );
        ins_error();
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
pub(crate) unsafe fn do_endv() {
    BASE_PTR = INPUT_PTR;
    INPUT_STACK[BASE_PTR] = cur_input;
    while INPUT_STACK[BASE_PTR].index != Btl::VTemplate
        && INPUT_STACK[BASE_PTR].loc.opt().is_none()
        && INPUT_STACK[BASE_PTR].state == InputState::TokenList
    {
        BASE_PTR -= 1
    }
    if INPUT_STACK[BASE_PTR].index != Btl::VTemplate
        || INPUT_STACK[BASE_PTR].loc.opt().is_some()
        || INPUT_STACK[BASE_PTR].state != InputState::TokenList
    {
        fatal_error("(interwoven alignment preambles are not allowed)");
    }
    if cur_group == GroupCode::Align {
        end_graf();
        if fin_col() {
            fin_row();
        }
    } else {
        off_save();
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
                TxtNode::HList(p) | TxtNode::VList(p) => {
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
    let mut t = Edge(new_edge(cur_dir, 0));
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
pub(crate) unsafe fn get_r_token() {
    loop {
        loop {
            get_token();
            if !(cur_tok == SPACE_TOKEN) {
                break;
            }
        }
        if !(cur_cs == 0
            || cur_cs > EQTB_TOP as i32
            || cur_cs > FROZEN_CONTROL_SEQUENCE as i32 && cur_cs <= EQTB_SIZE as i32)
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
        if cur_cs == 0 {
            back_input();
        }
        cur_tok = CS_TOKEN_FLAG + FROZEN_PROTECTION as i32;
        ins_error();
    }
}
pub(crate) unsafe fn trap_zero_glue() {
    if MEM[(cur_val + 1) as usize].b32.s1 == 0
        && MEM[(cur_val + 2) as usize].b32.s1 == 0
        && MEM[(cur_val + 3) as usize].b32.s1 == 0
    {
        GlueSpec(0).rc_inc(); // TODO: check
        delete_glue_ref(cur_val as usize);
        cur_val = 0;
    };
}
pub(crate) unsafe fn do_register_command(mut a: i16) {
    let mut l: i32 = None.tex_int();
    let mut p = ValLevel::Int;
    let mut q = cur_cmd;
    let mut e = false;

    let mut flag = true;
    if q != Cmd::Register {
        get_x_token();
        match cur_cmd {
            Cmd::AssignInt | Cmd::AssignDimen | Cmd::AssignGlue | Cmd::AssignMuGlue => {
                l = cur_chr;
                p = match cur_cmd {
                    Cmd::AssignInt => ValLevel::Int,
                    Cmd::AssignDimen => ValLevel::Dimen,
                    Cmd::AssignGlue => ValLevel::Glue,
                    Cmd::AssignMuGlue => ValLevel::Mu,
                    _ => unreachable!(),
                };
                flag = false;
            }
            _ => {
                if cur_cmd != Cmd::Register {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("You can\'t use `");
                    print_cmd_chr(cur_cmd, cur_chr);
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
        if cur_chr < 0 || cur_chr > 19 {
            /*lo_mem_stat_max*/
            l = cur_chr;
            p = ValLevel::from((MEM[l as usize].b16.s1 as i32 / 64) as u8);
            e = true
        } else {
            p = ValLevel::from(cur_chr as u8);
            scan_register_num();
            if cur_val > 255 {
                find_sa_element(p, cur_val, true);
                l = cur_ptr.tex_int();
                e = true
            } else {
                match p {
                    ValLevel::Int => l = cur_val + COUNT_BASE as i32,
                    ValLevel::Dimen => l = cur_val + SCALED_BASE as i32,
                    ValLevel::Glue => l = cur_val + SKIP_BASE as i32,
                    ValLevel::Mu => l = cur_val + MU_SKIP_BASE as i32,
                    _ => {}
                }
            }
        }
    }

    let mut w = 0;
    let mut s = None.tex_int();
    if p < ValLevel::Glue {
        if e {
            w = MEM[(l + 2) as usize].b32.s1
        } else {
            w = EQTB[l as usize].val
        }
    } else if e {
        s = MEM[(l + 1) as usize].b32.s1
    } else {
        s = EQTB[l as usize].val
        /*:1272*/
    } /*1275:*/
    if q == Cmd::Register {
        scan_optional_equals();
    } else {
        scan_keyword(b"by");
    }
    arith_error = false;
    if q < Cmd::Multiply {
        /*1273:*/
        match p {
            ValLevel::Int | ValLevel::Dimen => {
                if p == ValLevel::Int {
                    scan_int();
                } else {
                    scan_dimen(false, false, false);
                }
                if q == Cmd::Advance {
                    cur_val = cur_val + w
                }
            }
            _ => {
                scan_glue(p);
                if q == Cmd::Advance {
                    /*1274:*/
                    let mut q = GlueSpec(new_spec(cur_val as usize));
                    let r = GlueSpec(s as usize);
                    delete_glue_ref(cur_val as usize);
                    q.set_size(q.size() + r.size());
                    if q.stretch() == 0 {
                        q.set_stretch_order(GlueOrder::Normal);
                    }
                    if q.stretch_order() == r.stretch_order() {
                        q.set_stretch(q.stretch() + r.stretch());
                    } else if q.stretch_order() < r.stretch_order() && r.stretch() != 0 {
                        q.set_stretch(r.stretch())
                            .set_stretch_order(r.stretch_order());
                    }
                    if q.shrink() == 0 {
                        q.set_shrink_order(GlueOrder::Normal);
                    }
                    if q.shrink_order() == r.shrink_order() {
                        q.set_shrink(q.shrink() + r.shrink());
                    } else if q.shrink_order() < r.shrink_order() && r.shrink() != 0 {
                        q.set_shrink(r.shrink()).set_shrink_order(r.shrink_order());
                    }
                    cur_val = q.ptr() as i32;
                }
            }
        }
    } else {
        scan_int();
        match p {
            ValLevel::Int | ValLevel::Dimen => {
                if q == Cmd::Multiply {
                    if p == ValLevel::Int {
                        cur_val = mult_and_add(w, cur_val, 0, TEX_INFINITY)
                    } else {
                        cur_val = mult_and_add(w, cur_val, 0, MAX_HALFWORD)
                    }
                } else {
                    cur_val = x_over_n(w, cur_val)
                }
            }
            _ => {
                let s_spec = GlueSpec(s as usize);
                let mut r = GlueSpec(new_spec(s as usize));
                if q == Cmd::Multiply {
                    r.set_size(mult_and_add(s_spec.size(), cur_val, 0, MAX_HALFWORD))
                        .set_stretch(mult_and_add(s_spec.stretch(), cur_val, 0, MAX_HALFWORD))
                        .set_shrink(mult_and_add(s_spec.shrink(), cur_val, 0, MAX_HALFWORD));
                } else {
                    r.set_size(x_over_n(s_spec.size(), cur_val))
                        .set_stretch(x_over_n(s_spec.stretch(), cur_val))
                        .set_shrink(x_over_n(s_spec.shrink(), cur_val));
                }
                cur_val = r.ptr() as i32;
            }
        }
    }
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
            delete_glue_ref(cur_val as usize);
        }
        error();
        return;
    }
    if p < ValLevel::Glue {
        if e {
            if a >= 4 {
                gsa_w_def(l as usize, cur_val);
            } else {
                sa_w_def(l as usize, cur_val);
            }
        } else if a >= 4 {
            geq_word_define(l as usize, cur_val);
        } else {
            eq_word_define(l as usize, cur_val);
        }
    } else {
        trap_zero_glue();
        if e {
            if a >= 4 {
                gsa_def(l as usize, cur_val.opt());
            } else {
                sa_def(l as usize, cur_val.opt());
            }
        } else if a >= 4 {
            geq_define(l as usize, Cmd::GlueRef, cur_val.opt());
        } else {
            eq_define(l as usize, Cmd::GlueRef, cur_val.opt());
        }
    };
}
pub(crate) unsafe fn alter_aux() {
    if cur_chr != cur_list.mode.1 as i32 {
        report_illegal_case();
    } else {
        let c = cur_chr;
        scan_optional_equals();
        if c == ListMode::VMode as i32 {
            scan_dimen(false, false, false);
            cur_list.aux.b32.s1 = cur_val
        } else {
            scan_int();
            if cur_val <= 0 || cur_val > 32767 {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Bad space factor");
                help!("I allow only values in the range 1..32767 here.");
                int_error(cur_val);
            } else {
                cur_list.aux.b32.s0 = cur_val;
            }
        }
    };
}
pub(crate) unsafe fn alter_prev_graf() {
    NEST[NEST_PTR] = cur_list;
    let mut p = NEST_PTR;
    while NEST[p as usize].mode.1 != ListMode::VMode {
        p -= 1;
    }
    scan_optional_equals();
    scan_int();
    if cur_val < 0 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Bad ");
        print_esc_cstr("prevgraf");
        help!("I allow only nonnegative values here.");
        int_error(cur_val);
    } else {
        NEST[p as usize].prev_graf = cur_val;
        cur_list = NEST[NEST_PTR]
    };
}
pub(crate) unsafe fn alter_page_so_far() {
    let c = cur_chr as u8 as usize;
    scan_optional_equals();
    scan_dimen(false, false, false);
    page_so_far[c] = cur_val;
}
pub(crate) unsafe fn alter_integer() {
    let mut c: i16 = 0;
    c = cur_chr as i16;
    scan_optional_equals();
    scan_int();
    if c == 0 {
        dead_cycles = cur_val
    } else if c == 2 {
        if InteractionMode::n(cur_val as u8).is_none() {
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
            int_error(cur_val);
        } else {
            cur_chr = cur_val;
            new_interaction();
        }
    } else {
        insert_penalties = cur_val
    };
}
pub(crate) unsafe fn alter_box_dimen() {
    let c = cur_chr as usize;
    scan_register_num();
    let b = if cur_val < 256 {
        BOX_REG(cur_val as usize).opt()
    } else {
        find_sa_element(ValLevel::Ident, cur_val, false);
        cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
    };
    scan_optional_equals();
    scan_dimen(false, false, false);
    if let Some(b) = b {
        MEM[b + c].b32.s1 = cur_val;
    };
}
pub(crate) unsafe fn new_font(mut a: i16) {
    let mut s: scaled_t = 0;
    if job_name == 0 {
        open_log_file();
    }
    get_r_token();
    let u = cur_cs as usize;
    let mut t = if u >= HASH_BASE {
        (*hash.offset(u as isize)).s1
    } else if u >= SINGLE_BASE {
        if u == NULL_CS {
            maketexstring("FONT")
        } else {
            (u - SINGLE_BASE) as i32
        }
    } else {
        let old_setting_0 = selector;
        selector = Selector::NEW_STRING;
        print_cstr("FONT");
        print((u - 1) as i32);
        selector = old_setting_0;
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
    scan_optional_equals();
    scan_file_name();
    name_in_progress = true;
    if scan_keyword(b"at") {
        /*1294: */
        scan_dimen(false, false, false); /*:1293 */
        s = cur_val; /*:79 */
        if s <= 0 || s >= 0x8000000 {
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
            s = (10_i64 * 65536) as scaled_t
        }
    } else if scan_keyword(b"scaled") {
        scan_int();
        s = -cur_val;
        if cur_val <= 0 || cur_val > 32768 {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Illegal magnification has been changed to 1000");
            help!("The magnification ratio must be between 1 and 32768.");
            int_error(cur_val);
            s = -1000;
        }
    } else {
        s = -1000;
    }
    name_in_progress = false;

    for f in (FONT_BASE + 1)..=FONT_PTR {
        // TODO: check
        if str_eq_str(FONT_NAME[f], cur_name)
            && (length(cur_area) == 0
                && (FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG)
                || str_eq_str(FONT_AREA[f], cur_area))
        {
            if s > 0 {
                if s == FONT_SIZE[f] {
                    return common_ending(a, u, f, t);
                }
            } else if FONT_SIZE[f] == xn_over_d(FONT_DSIZE[f], -s, 1000) {
                return common_ending(a, u, f, t);
            }
        }
        append_str(cur_area);
        append_str(cur_name);
        append_str(cur_ext);
        if str_eq_str(FONT_NAME[f], make_string()) {
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
            if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
                if s > 0 {
                    if s == FONT_SIZE[f] {
                        return common_ending(a, u, f, t);
                    }
                } else if FONT_SIZE[f] == xn_over_d(FONT_DSIZE[f], -s, 1000) {
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

    let f = read_font_info(u as i32, cur_name, cur_area, s);
    common_ending(a, u, f, t)
}
pub(crate) unsafe fn new_interaction() {
    print_ln();
    interaction = InteractionMode::n(cur_chr as u8).unwrap();
    selector = if interaction == InteractionMode::Batch {
        Selector::NO_PRINT
    } else {
        Selector::TERM_ONLY
    };
    if log_opened {
        selector = (u8::from(selector)).wrapping_add(2).into()
    };
}
pub(crate) unsafe fn issue_message() {
    let mut c: u8 = 0;
    let mut s: str_number = 0;
    c = cur_chr as u8;
    *LLIST_link(GARBAGE) = scan_toks(false, true) as i32;
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
pub(crate) unsafe fn shift_case() {
    let b = cur_chr;
    let _p = scan_toks(false, false);
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
    begin_token_list(*LLIST_link(def_ref) as usize, Btl::BackedUp);
    *LLIST_link(def_ref) = avail.tex_int();
    avail = Some(def_ref);
}
pub(crate) unsafe fn show_whatever() {
    match cur_chr {
        3 => {
            begin_diagnostic();
            show_activities();
        }
        1 => {
            scan_register_num();
            let p = if cur_val < 256 {
                BOX_REG(cur_val as usize).opt()
            } else {
                find_sa_element(ValLevel::Ident, cur_val, false);
                cur_ptr.and_then(|c| MEM[c + 1].b32.s1.opt())
            };
            begin_diagnostic();
            print_nl_cstr("> \\box");
            print_int(cur_val);
            print_chr('=');
            if p.is_none() {
                print_cstr("void");
            } else {
                show_box(p);
            }
        }
        0 => {
            get_token();
            print_nl_cstr("> ");
            if cur_cs != 0 {
                sprint_cs(cur_cs);
                print_chr('=');
            }
            print_meaning();
            return common_ending();
        }
        4 => {
            begin_diagnostic();
            show_save_groups();
        }
        6 => {
            begin_diagnostic();
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
        }
        _ => {
            let _p = the_toks() as i32;
            print_nl_cstr("> ");
            token_show(Some(TEMP_HEAD));
            flush_list(llist_link(TEMP_HEAD));
            return common_ending();
        }
    }

    end_diagnostic(true);
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
pub(crate) unsafe fn scan_and_pack_name() {
    scan_file_name();
    pack_file_name(cur_name, cur_area, cur_ext);
}
pub(crate) unsafe fn do_extension() {
    let mut j: i32 = 0;
    let mut p: usize = 0;
    match cur_chr as u16 {
        0 => {
            // OpenFile
            let mut o = OpenFile::new_node();
            *LLIST_link(cur_list.tail) = Some(o.ptr()).tex_int();
            cur_list.tail = o.ptr();
            scan_four_bit_int();
            o.set_id(cur_val);
            scan_optional_equals();
            scan_file_name();
            o.set_name(cur_name).set_area(cur_area).set_ext(cur_ext);
        }
        1 => {
            // WriteFile
            let k = cur_cs;
            let mut w = WriteFile::new_node();
            *LLIST_link(cur_list.tail) = Some(w.ptr()).tex_int();
            cur_list.tail = w.ptr();
            scan_int();
            if cur_val < 0 {
                cur_val = 17;
            } else if cur_val > 15 && cur_val != 18 {
                cur_val = 16;
            }
            w.set_id(cur_val);
            cur_cs = k;
            p = scan_toks(false, false);
            w.set_tokens(def_ref as i32);
        }
        2 => {
            // CloseFile
            let mut c = CloseFile::new_node();
            *LLIST_link(cur_list.tail) = Some(c.ptr()).tex_int();
            cur_list.tail = c.ptr();
            scan_int();
            if cur_val < 0 {
                cur_val = 17;
            } else if cur_val > 15 && cur_val != 18 {
                cur_val = 16;
            }
            c.set_id(cur_val);
            MEM[cur_list.tail + 1].b32.s1 = None.tex_int()
        }
        3 => {
            let mut s = Special::new_node();
            *LLIST_link(cur_list.tail) = Some(s.ptr()).tex_int();
            cur_list.tail = s.ptr();
            MEM[cur_list.tail + 1].b32.s0 = None.tex_int();
            p = scan_toks(false, true);
            s.set_tokens(def_ref as i32);
        }
        IMMEDIATE_CODE => {
            get_x_token();
            if cur_cmd == Cmd::Extension && cur_chr <= WhatsItNST::Close as i32 {
                p = cur_list.tail;
                do_extension();
                out_what(cur_list.tail);
                flush_node_list(Some(cur_list.tail));
                cur_list.tail = p;
                *LLIST_link(p) = None.tex_int();
            } else {
                back_input();
            }
        }
        SET_LANGUAGE_CODE => {
            if cur_list.mode.1 != ListMode::HMode {
                report_illegal_case();
            } else {
                let mut l = Language::new_node();
                *LLIST_link(cur_list.tail) = Some(l.ptr()).tex_int();
                cur_list.tail = l.ptr();
                scan_int();
                cur_list.aux.b32.s1 = if cur_val <= 0 {
                    0
                } else if cur_val > 255 {
                    0
                } else {
                    cur_val
                };
                l.set_lang(cur_list.aux.b32.s1)
                    .set_lhm(norm_min(*INTPAR(IntPar::left_hyphen_min)) as u16)
                    .set_rhm(norm_min(*INTPAR(IntPar::right_hyphen_min)) as u16);
            }
        }
        PIC_FILE_CODE => {
            if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case();
            } else {
                load_picture(false);
            }
        }
        PDF_FILE_CODE => {
            if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case();
            } else {
                load_picture(true);
            }
        }
        GLYPH_CODE => {
            if cur_list.mode.1 == ListMode::VMode {
                back_input();
                new_graf(true);
            } else if cur_list.mode.1 == ListMode::MMode {
                report_illegal_case();
            } else if FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == AAT_FONT_FLAG
                || FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == OTGR_FONT_FLAG
            {
                let mut g = Glyph::new_node();
                *LLIST_link(cur_list.tail) = Some(g.ptr()).tex_int();
                cur_list.tail = g.ptr();
                scan_int();
                if cur_val < 0 || cur_val > 65535 {
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
                    int_error(cur_val);
                    cur_val = 0;
                }
                g.set_font(EQTB[CUR_FONT_LOC].val as u16)
                    .set_glyph(cur_val as u16);
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
            scan_and_pack_name();
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
                set_input_file_encoding(INPUT_FILE[IN_OPEN], i, j);
            }
        }
        XETEX_DEFAULT_ENCODING_EXTENSION_CODE => {
            scan_and_pack_name();
            let i = get_encoding_mode_and_info(&mut j);
            *INTPAR(IntPar::xetex_default_input_mode) = i as i32;
            *INTPAR(IntPar::xetex_default_input_encoding) = j
        }
        XETEX_LINEBREAK_LOCALE_EXTENSION_CODE => {
            scan_file_name();
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
        begin_token_list(toklist, Btl::Inserted);
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
pub(crate) unsafe fn handle_right_brace() {
    match cur_group {
        GroupCode::Simple => unsave(),
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
        GroupCode::SemiSimple | GroupCode::MathShift | GroupCode::MathLeft => extra_right_brace(),
        GroupCode::HBox => package(0),
        GroupCode::AdjustedHBox => {
            adjust_tail = Some(ADJUST_HEAD);
            pre_adjust_tail = Some(PRE_ADJUST_HEAD);
            package(0);
        }
        GroupCode::VBox => {
            end_graf();
            package(0);
        }
        GroupCode::VTop => {
            end_graf();
            package(BoxCode::VTop as i16);
        }
        GroupCode::Insert => {
            end_graf();
            let q = *GLUEPAR(GluePar::split_top_skip) as usize;
            GlueSpec(q).rc_inc();
            let d = *DIMENPAR(DimenPar::split_max_depth);
            let f = *INTPAR(IntPar::floating_penalty);
            unsave();
            SAVE_PTR -= 2;
            let p = vpackage(
                MEM[cur_list.head].b32.s1.opt(),
                0,
                PackMode::Additional,
                MAX_HALFWORD,
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
                build_page();
            }
        }
        GroupCode::Output => {
            /*1062:*/
            if cur_input.loc.opt().is_some()
                || cur_input.index != Btl::OutputText && cur_input.index != Btl::BackedUp
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
                    get_token();
                    if cur_input.loc.opt().is_none() {
                        break;
                    }
                }
            }

            end_token_list();
            end_graf();
            unsave();
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
            build_page();
        }
        GroupCode::Disc => build_discretionary(),
        GroupCode::Align => {
            back_input();
            cur_tok = CS_TOKEN_FLAG + FROZEN_CR as i32;
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Missing ");
            print_esc_cstr("cr");
            print_cstr(" inserted");
            help!("I\'m guessing that you meant to end an alignment here.");
            ins_error();
        }
        GroupCode::NoAlign => {
            end_graf();
            unsave();
            align_peek();
        }
        GroupCode::VCenter => {
            end_graf();
            unsave();
            SAVE_PTR -= 2;
            let p = vpackage(
                MEM[cur_list.head].b32.s1.opt(),
                SAVE_STACK[SAVE_PTR + 1].val,
                PackMode::from(SAVE_STACK[SAVE_PTR + 0].val),
                MAX_HALFWORD,
            );
            pop_nest();
            let n = new_noad();
            *LLIST_link(cur_list.tail) = Some(n).tex_int();
            cur_list.tail = n;
            MEM[cur_list.tail].b16.s1 = MathNode::VCenter as u16;
            MEM[cur_list.tail + 1].b32.s1 = MathCell::SubBox as _;
            MEM[cur_list.tail + 1].b32.s0 = Some(p.ptr()).tex_int()
        }
        GroupCode::MathChoice => build_choices(),
        GroupCode::Math => {
            unsave();
            SAVE_PTR -= 1;
            MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32.s1 = MathCell::SubMList as _;
            let mut p = fin_mlist(None);
            MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32.s0 = p;
            if let Some(p) = p.opt() {
                if llist_link(p).is_none() {
                    if NODE_type(p) == MathNode::Ord.into() {
                        if MEM[p + 3].b32.s1 == MathCell::Empty as _ {
                            if MEM[p + 2].b32.s1 == MathCell::Empty as _ {
                                MEM[SAVE_STACK[SAVE_PTR + 0].val as usize].b32 = MEM[p + 1].b32;
                                free_node(p, NOAD_SIZE);
                            }
                        }
                    } else if NODE_type(p) == MathNode::Accent.into() {
                        if SAVE_STACK[SAVE_PTR + 0].val == cur_list.tail as i32 + 1 {
                            if NODE_type(cur_list.tail) == MathNode::Ord.into() {
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
                }
            }
        } //_ => confusion("rightbrace"),
    };
}
pub(crate) unsafe fn main_control() {
    if let Some(ej) = LOCAL(Local::every_job).opt() {
        begin_token_list(ej, Btl::EveryJobText);
    }
    let mut big_switch = true;
    'big_switch: loop {
        /* big_switch */
        if big_switch {
            get_x_token();
        }
        big_switch = true;

        // reswitch
        /*1066: */
        if *INTPAR(IntPar::tracing_commands) > 0i32 {
            show_cur_cmd_chr(); /*:1490 */
        }
        use ListMode::*;
        match (cur_list.mode.1, cur_cmd) {
            (HMode, Cmd::Letter) | (HMode, Cmd::OtherChar) | (HMode, Cmd::CharGiven) => { // 115 | 116 | 172
            }
            (HMode, Cmd::CharNum) => {
                // 120
                scan_usv_num();
                cur_chr = cur_val;
            }
            (HMode, Cmd::NoBoundary) => {
                // 169
                get_x_token();
                if cur_cmd == Cmd::Letter
                    || cur_cmd == Cmd::OtherChar
                    || cur_cmd == Cmd::CharGiven
                    || cur_cmd == Cmd::CharNum
                {
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
                            if cur_cs == 0 {
                                if cur_cmd == Cmd::CharNum {
                                    cur_cmd = Cmd::OtherChar;
                                }
                                cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
                            } else {
                                cur_tok = CS_TOKEN_FLAG + cur_cs
                            }
                            back_input();
                            begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
                            continue 'big_switch;
                        }
                    }
                }
                match (cur_list.mode.1, cur_cmd) {
                    (HMode, Cmd::Spacer) => {
                        // 114
                        if cur_list.aux.b32.s0 == 1000 {
                            append_normal_space();
                        } else {
                            app_space();
                        }
                    }
                    (HMode, Cmd::ExSpace) | (MMode, Cmd::ExSpace) => {
                        // 168 | 271
                        append_normal_space();
                    }
                    (VMode, Cmd::IgnoreSpaces)
                    | (HMode, Cmd::IgnoreSpaces)
                    | (MMode, Cmd::IgnoreSpaces) => {
                        // 40 | 143 | 246
                        if cur_chr == 0 {
                            loop {
                                get_x_token();
                                if !(cur_cmd == Cmd::Spacer) {
                                    break;
                                }
                            }
                            big_switch = false;
                        } else {
                            let t = scanner_status;
                            scanner_status = ScannerStatus::Normal;
                            get_next();
                            scanner_status = t;
                            if cur_cs < HASH_BASE as i32 {
                                cur_cs = prim_lookup(cur_cs - SINGLE_BASE as i32) as i32
                            } else {
                                cur_cs = prim_lookup((*hash.offset(cur_cs as isize)).s1) as i32
                            }
                            if cur_cs != UNDEFINED_PRIMITIVE {
                                cur_cmd = Cmd::from(prim_eqtb[cur_cs as usize].cmd);
                                cur_chr = prim_eqtb[cur_cs as usize].val;
                                big_switch = false;
                            }
                        }
                    }
                    (VMode, Cmd::Comment) => {
                        // 15
                        if its_all_over() {
                            return;
                        }
                    }
                    (VMode, Cmd::VMove)
                    | (HMode, Cmd::HMove)
                    | (MMode, Cmd::HMove)
                    | (VMode, Cmd::LastItem)
                    | (HMode, Cmd::LastItem)
                    | (MMode, Cmd::LastItem)
                    | (VMode, Cmd::VAdjust)
                    | (VMode, Cmd::ItalCorr)
                    | (VMode, Cmd::EqNo)
                    | (HMode, Cmd::EqNo)
                    | (VMode, Cmd::MacParam)
                    | (HMode, Cmd::MacParam)
                    | (MMode, Cmd::MacParam) => {
                        // 23 | 125 | 228 | 72 | 175 | 278 | 39 | 45 | 49 | 152 | 7 | 110 | 213
                        report_illegal_case();
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
                        insert_dollar_sign();
                    }
                    (VMode, Cmd::HRule) | (HMode, Cmd::VRule) | (MMode, Cmd::VRule) => {
                        // 37 | 139 | 242
                        let srs = scan_rule_spec();
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
                        append_glue();
                    }
                    (VMode, Cmd::Kern)
                    | (HMode, Cmd::Kern)
                    | (MMode, Cmd::Kern)
                    | (MMode, Cmd::MKern) => {
                        // 30 | 133 | 236 | 237
                        append_kern();
                    }
                    (VMode, Cmd::LeftBrace) | (HMode, Cmd::LeftBrace) => {
                        // 2 | 105
                        new_save_level(GroupCode::Simple);
                    }
                    (VMode, Cmd::BeginGroup)
                    | (HMode, Cmd::BeginGroup)
                    | (MMode, Cmd::BeginGroup) => {
                        // 62 | 165 | 268
                        new_save_level(GroupCode::SemiSimple);
                    }
                    (VMode, Cmd::EndGroup) | (HMode, Cmd::EndGroup) | (MMode, Cmd::EndGroup) => {
                        // 63 | 166 | 269
                        if cur_group == GroupCode::SemiSimple {
                            unsave();
                        } else {
                            off_save();
                        }
                    }
                    (VMode, Cmd::RightBrace)
                    | (HMode, Cmd::RightBrace)
                    | (MMode, Cmd::RightBrace) => {
                        // 3 | 106 | 209
                        handle_right_brace();
                    }
                    (VMode, Cmd::HMove) | (HMode, Cmd::VMove) | (MMode, Cmd::VMove) => {
                        // 22 | 126 | 229
                        let t = cur_chr;
                        scan_dimen(false, false, false);
                        if t == 0 {
                            scan_box(cur_val);
                        } else {
                            scan_box(-cur_val);
                        }
                    }
                    (VMode, Cmd::LeaderShip)
                    | (HMode, Cmd::LeaderShip)
                    | (MMode, Cmd::LeaderShip) => {
                        // 32 | 135 | 238
                        scan_box(LEADER_FLAG - (A_LEADERS as i32) + cur_chr);
                    }
                    (VMode, Cmd::MakeBox) | (HMode, Cmd::MakeBox) | (MMode, Cmd::MakeBox) => {
                        // 21 | 124 | 227
                        begin_box(0);
                    }
                    (VMode, Cmd::StartPar) => {
                        // 44
                        new_graf(cur_chr > 0);
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
                        back_input();
                        new_graf(true);
                    }
                    (HMode, Cmd::StartPar) | (MMode, Cmd::StartPar) => {
                        // 147 | 250
                        indent_in_hmode();
                    }
                    (VMode, Cmd::ActiveChar) => {
                        // 14
                        normal_paragraph();
                        if cur_list.mode.0 == false {
                            build_page();
                        }
                    }
                    (HMode, Cmd::ActiveChar) => {
                        // 117
                        if align_state < 0 {
                            off_save();
                        }
                        end_graf();
                        if cur_list.mode == (false, ListMode::VMode) {
                            build_page();
                        }
                    }
                    (HMode, Cmd::Comment)
                    | (HMode, Cmd::VSkip)
                    | (HMode, Cmd::HRule)
                    | (HMode, Cmd::UnVBox)
                    | (HMode, Cmd::HAlign) => {
                        // 118 | 131 | 140 | 128 | 136
                        head_for_vmode();
                    }
                    (VMode, Cmd::Insert)
                    | (HMode, Cmd::Insert)
                    | (MMode, Cmd::Insert)
                    | (HMode, Cmd::VAdjust)
                    | (MMode, Cmd::VAdjust) => {
                        // 38 | 141 | 244 | 142 | 245
                        begin_insert_or_adjust();
                    }
                    (VMode, Cmd::Mark) | (HMode, Cmd::Mark) | (MMode, Cmd::Mark) => {
                        // 19 | 122 | 225
                        make_mark();
                    }
                    (VMode, Cmd::BreakPenalty)
                    | (HMode, Cmd::BreakPenalty)
                    | (MMode, Cmd::BreakPenalty) => {
                        // 43 | 146 | 249
                        append_penalty();
                    }
                    (VMode, Cmd::RemoveItem)
                    | (HMode, Cmd::RemoveItem)
                    | (MMode, Cmd::RemoveItem) => {
                        // 26 | 129 | 232
                        delete_last();
                    }
                    (VMode, Cmd::UnVBox) | (HMode, Cmd::UnHBox) | (MMode, Cmd::UnHBox) => {
                        // 25 | 127 | 230
                        unpackage();
                    }
                    (HMode, Cmd::ItalCorr) => {
                        // 148
                        append_italic_correction();
                    }
                    (MMode, Cmd::ItalCorr) => {
                        // 251
                        let k = new_kern(0);
                        *LLIST_link(cur_list.tail) = Some(k).tex_int();
                        cur_list.tail = k;
                    }
                    (HMode, Cmd::Discretionary) | (MMode, Cmd::Discretionary) => {
                        // 151 | 254
                        append_discretionary();
                    }
                    (HMode, Cmd::Accent) => {
                        // 149
                        make_accent();
                    }
                    (VMode, Cmd::CarRet)
                    | (HMode, Cmd::CarRet)
                    | (MMode, Cmd::CarRet)
                    | (VMode, Cmd::TabMark)
                    | (HMode, Cmd::TabMark)
                    | (MMode, Cmd::TabMark) => {
                        // 6 | 109 | 212 | 5 | 108 | 211
                        align_error();
                    }
                    (VMode, Cmd::NoAlign) | (HMode, Cmd::NoAlign) | (MMode, Cmd::NoAlign) => {
                        // 35 | 138 | 241
                        no_align_error();
                    }
                    (VMode, Cmd::Omit) | (HMode, Cmd::Omit) | (MMode, Cmd::Omit) => {
                        // 64 | 167 | 270
                        omit_error();
                    }
                    (VMode, Cmd::HAlign) => {
                        // 33
                        init_align();
                    }
                    (HMode, Cmd::VAlign) => {
                        // 137
                        if cur_chr > 0 {
                            if eTeX_enabled(*INTPAR(IntPar::texxet) > 0, cur_cmd, cur_chr) {
                                let m = new_math(0, MathType::from(cur_chr as u16));
                                *LLIST_link(cur_list.tail) = Some(m).tex_int();
                                cur_list.tail = m;
                            }
                        } else {
                            init_align();
                        }
                    }
                    (MMode, Cmd::HAlign) => {
                        // 239
                        if privileged() {
                            if cur_group == GroupCode::MathShift {
                                init_align();
                            } else {
                                off_save();
                            }
                        }
                    }
                    (VMode, Cmd::EndV) | (HMode, Cmd::EndV) => {
                        // 10 | 113
                        do_endv();
                    }
                    (VMode, Cmd::EndCSName) | (HMode, Cmd::EndCSName) | (MMode, Cmd::EndCSName) => {
                        // 68 | 171 | 274
                        cs_error();
                    }
                    (HMode, Cmd::MathShift) => {
                        // 107
                        init_math();
                    }
                    (MMode, Cmd::EqNo) => {
                        // 255
                        if privileged() {
                            if cur_group == GroupCode::MathShift {
                                start_eq_no();
                            } else {
                                off_save();
                            }
                        }
                    }
                    (MMode, Cmd::LeftBrace) => {
                        // 208
                        let n = new_noad();
                        *LLIST_link(cur_list.tail) = Some(n).tex_int();
                        cur_list.tail = n;
                        back_input();
                        let m = BaseMath(cur_list.tail);
                        scan_math(m.first_mut(), cur_list.tail + 1);
                    }
                    (MMode, Cmd::Letter) | (MMode, Cmd::OtherChar) | (MMode, Cmd::CharGiven) => {
                        // 218 | 219 | 275
                        set_math_char(*MATH_CODE(cur_chr as usize));
                    }
                    (MMode, Cmd::CharNum) => {
                        // 223
                        scan_char_num();
                        cur_chr = cur_val;
                        set_math_char(*MATH_CODE(cur_chr as usize));
                    }
                    (MMode, Cmd::MathCharNum) => {
                        // 224
                        if cur_chr == 2 {
                            scan_math_class_int();
                            let t = set_class(cur_val);
                            scan_math_fam_int();
                            let t = t + set_family(cur_val);
                            scan_usv_num();
                            let t = t + cur_val;
                            set_math_char(t);
                        } else if cur_chr == 1 {
                            scan_xetex_math_char_int();
                            set_math_char(cur_val);
                        } else {
                            scan_fifteen_bit_int();
                            set_math_char(
                                set_class(cur_val / 4096)
                                    + set_family((cur_val % 4096) / 256)
                                    + (cur_val % 256),
                            );
                        }
                    }
                    (MMode, Cmd::MathGiven) => {
                        // 276
                        set_math_char(
                            set_class(cur_chr / 4096)
                                + set_family((cur_chr % 4096) / 256)
                                + (cur_chr % 256),
                        );
                    }
                    (MMode, Cmd::XetexMathGiven) => {
                        // 277
                        set_math_char(cur_chr);
                    }
                    (MMode, Cmd::DelimNum) => {
                        // 222
                        if cur_chr == 1i32 {
                            scan_math_class_int();
                            scan_math_class_int();
                            let t = set_class(cur_val);
                            scan_math_fam_int();
                            let t = t + set_family(cur_val);
                            scan_usv_num();
                            let t = t + cur_val;
                            set_math_char(t);
                        } else {
                            scan_delimiter_int();
                            cur_val = cur_val / 4096;
                            set_math_char(
                                set_class(cur_val / 4096)
                                    + set_family((cur_val % 4096) / 256)
                                    + (cur_val % 256),
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
                        scan_math(m.first_mut(), cur_list.tail + 1);
                    }
                    (MMode, Cmd::LimitSwitch) => {
                        // 258
                        math_limit_switch();
                    }
                    (MMode, Cmd::Radical) => {
                        // 273
                        math_radical();
                    }
                    (MMode, Cmd::Accent) | (MMode, Cmd::MathAccent) => {
                        // 252 | 253
                        math_ac();
                    }
                    (MMode, Cmd::VCenter) => {
                        // 263
                        scan_spec(GroupCode::VCenter, false);
                        normal_paragraph();
                        push_nest();
                        cur_list.mode = (true, ListMode::VMode);
                        cur_list.aux.b32.s1 = IGNORE_DEPTH;
                        if insert_src_special_every_vbox {
                            insert_src_special();
                        }
                        if let Some(ev) = LOCAL(Local::every_vbox).opt() {
                            begin_token_list(ev, Btl::EveryVBoxText);
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
                        append_choices();
                    }
                    (MMode, Cmd::SubMark) | (MMode, Cmd::SupMark) => {
                        // 215 | 214
                        sub_sup();
                    }
                    (MMode, Cmd::Above) => {
                        // 259
                        math_fraction();
                    }
                    (MMode, Cmd::LeftRight) => {
                        // 256
                        math_left_right();
                    }
                    (MMode, Cmd::MathShift) => {
                        // 210
                        if cur_group == GroupCode::MathShift {
                            after_math();
                        } else {
                            off_save();
                        }
                    }
                    (VMode, Cmd::ToksRegister)
                    | (HMode, Cmd::ToksRegister)
                    | (MMode, Cmd::ToksRegister)
                    | (VMode, Cmd::AssignToks)
                    | (HMode, Cmd::AssignToks)
                    | (MMode, Cmd::AssignToks)
                    | (VMode, Cmd::AssignInt)
                    | (HMode, Cmd::AssignInt)
                    | (MMode, Cmd::AssignInt)
                    | (VMode, Cmd::AssignDimen)
                    | (HMode, Cmd::AssignDimen)
                    | (MMode, Cmd::AssignDimen)
                    | (VMode, Cmd::AssignGlue)
                    | (HMode, Cmd::AssignGlue)
                    | (MMode, Cmd::AssignGlue)
                    | (VMode, Cmd::AssignMuGlue)
                    | (HMode, Cmd::AssignMuGlue)
                    | (MMode, Cmd::AssignMuGlue)
                    | (VMode, Cmd::AssignFontDimen)
                    | (HMode, Cmd::AssignFontDimen)
                    | (MMode, Cmd::AssignFontDimen)
                    | (VMode, Cmd::AssignFontInt)
                    | (HMode, Cmd::AssignFontInt)
                    | (MMode, Cmd::AssignFontInt)
                    | (VMode, Cmd::SetAux)
                    | (HMode, Cmd::SetAux)
                    | (MMode, Cmd::SetAux)
                    | (VMode, Cmd::SetPrevGraf)
                    | (HMode, Cmd::SetPrevGraf)
                    | (MMode, Cmd::SetPrevGraf)
                    | (VMode, Cmd::SetPageDimen)
                    | (HMode, Cmd::SetPageDimen)
                    | (MMode, Cmd::SetPageDimen)
                    | (VMode, Cmd::SetPageInt)
                    | (HMode, Cmd::SetPageInt)
                    | (MMode, Cmd::SetPageInt)
                    | (VMode, Cmd::SetBoxDimen)
                    | (HMode, Cmd::SetBoxDimen)
                    | (MMode, Cmd::SetBoxDimen)
                    | (VMode, Cmd::SetShape)
                    | (HMode, Cmd::SetShape)
                    | (MMode, Cmd::SetShape)
                    | (VMode, Cmd::DefCode)
                    | (HMode, Cmd::DefCode)
                    | (MMode, Cmd::DefCode)
                    | (VMode, Cmd::XetexDefCode)
                    | (HMode, Cmd::XetexDefCode)
                    | (MMode, Cmd::XetexDefCode)
                    | (VMode, Cmd::DefFamily)
                    | (HMode, Cmd::DefFamily)
                    | (MMode, Cmd::DefFamily)
                    | (VMode, Cmd::SetFont)
                    | (HMode, Cmd::SetFont)
                    | (MMode, Cmd::SetFont)
                    | (VMode, Cmd::DefFont)
                    | (HMode, Cmd::DefFont)
                    | (MMode, Cmd::DefFont)
                    | (VMode, Cmd::Register)
                    | (HMode, Cmd::Register)
                    | (MMode, Cmd::Register)
                    | (VMode, Cmd::Advance)
                    | (HMode, Cmd::Advance)
                    | (MMode, Cmd::Advance)
                    | (VMode, Cmd::Multiply)
                    | (HMode, Cmd::Multiply)
                    | (MMode, Cmd::Multiply)
                    | (VMode, Cmd::Divide)
                    | (HMode, Cmd::Divide)
                    | (MMode, Cmd::Divide)
                    | (VMode, Cmd::Prefix)
                    | (HMode, Cmd::Prefix)
                    | (MMode, Cmd::Prefix)
                    | (VMode, Cmd::Let)
                    | (HMode, Cmd::Let)
                    | (MMode, Cmd::Let)
                    | (VMode, Cmd::ShorthandDef)
                    | (HMode, Cmd::ShorthandDef)
                    | (MMode, Cmd::ShorthandDef)
                    | (VMode, Cmd::ReadToCS)
                    | (HMode, Cmd::ReadToCS)
                    | (MMode, Cmd::ReadToCS)
                    | (VMode, Cmd::Def)
                    | (HMode, Cmd::Def)
                    | (MMode, Cmd::Def)
                    | (VMode, Cmd::SetBox)
                    | (HMode, Cmd::SetBox)
                    | (MMode, Cmd::SetBox)
                    | (VMode, Cmd::HyphData)
                    | (HMode, Cmd::HyphData)
                    | (MMode, Cmd::HyphData)
                    | (VMode, Cmd::SetInteraction)
                    | (HMode, Cmd::SetInteraction)
                    | (MMode, Cmd::SetInteraction) => {
                        // 73 | 176 | 279 | 74 | 177 | 280 | 75 | 178 | 281 | 76 | 179 | 282 | 77
                        //| 180 | 283 | 78 | 181 | 284 | 79 | 182 | 285 | 80 | 183 | 286 | 81
                        //| 184 | 287 | 82 | 185 | 288 | 83 | 186 | 289 | 84 | 187 | 290 | 85
                        //| 188 | 291 | 86 | 189 | 292 | 87 | 190 | 293 | 88 | 191 | 294 | 89
                        //| 192 | 295 | 90 | 193 | 296 | 91 | 194 | 297 | 92 | 195 | 298 | 93
                        //| 196 | 299 | 94 | 197 | 300 | 95 | 198 | 301 | 96 | 199 | 302 | 97
                        //| 200 | 303 | 98 | 201 | 304 | 99 | 202 | 305 | 100 | 203 | 306 | 101
                        //| 204 | 307 | 102 | 205 | 308 | 103 | 206 | 309
                        prefixed_command();
                    }
                    (VMode, Cmd::AfterAssignment)
                    | (HMode, Cmd::AfterAssignment)
                    | (MMode, Cmd::AfterAssignment) => {
                        // 41 | 144 | 247
                        get_token();
                        after_token = cur_tok;
                    }
                    (VMode, Cmd::AfterGroup)
                    | (HMode, Cmd::AfterGroup)
                    | (MMode, Cmd::AfterGroup) => {
                        // 42 | 145 | 248
                        get_token();
                        save_for_after(cur_tok);
                    }
                    (VMode, Cmd::InStream) | (HMode, Cmd::InStream) | (MMode, Cmd::InStream) => {
                        // 61 | 164 | 267
                        open_or_close_in();
                    }
                    (VMode, Cmd::Message) | (HMode, Cmd::Message) | (MMode, Cmd::Message) => {
                        // 59 | 162 | 265
                        issue_message();
                    }
                    (VMode, Cmd::CaseShift) | (HMode, Cmd::CaseShift) | (MMode, Cmd::CaseShift) => {
                        // 58 | 161 | 264
                        shift_case();
                    }
                    (VMode, Cmd::XRay) | (HMode, Cmd::XRay) | (MMode, Cmd::XRay) => {
                        // 20 | 123 | 226
                        show_whatever();
                    }
                    (VMode, Cmd::Extension) | (HMode, Cmd::Extension) | (MMode, Cmd::Extension) => {
                        // 60 | 163 | 266
                        do_extension();
                    }
                    (VMode, Cmd::Relax)
                    | (HMode, Cmd::Relax)
                    | (MMode, Cmd::Relax)
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
        if FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == AAT_FONT_FLAG
            || FONT_AREA[EQTB[CUR_FONT_LOC].val as usize] as u32 == OTGR_FONT_FLAG
        {
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
                        if cur_input.state != InputState::TokenList
                            || cur_input.index != Btl::BackedUpChar
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
                                cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                back_input();
                                cur_input.index = Btl::BackedUpChar;
                                begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                            cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                            back_input();
                            cur_input.index = Btl::BackedUpChar;
                            begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                get_next();
                if cur_cmd == Cmd::Letter || cur_cmd == Cmd::OtherChar || cur_cmd == Cmd::CharGiven
                {
                    continue;
                }
                x_token();
                if cur_cmd == Cmd::Letter || cur_cmd == Cmd::OtherChar || cur_cmd == Cmd::CharGiven
                {
                    continue;
                }
                if cur_cmd == Cmd::CharNum {
                    scan_usv_num();
                    cur_chr = cur_val
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
                    cur_tok = if cur_cs == 0 {
                        if cur_cmd == Cmd::CharNum {
                            cur_cmd = Cmd::OtherChar;
                        }
                        cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
                    } else {
                        CS_TOKEN_FLAG + cur_cs
                    };
                    back_input();
                    begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                    if map_char_to_glyph(main_f, main_k) == 0 {
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
                                if t != MEM[(FONT_GLUE[main_f as usize] + 1) as usize].b32.s1 {
                                    let mut tmp_ptr = Kern(new_kern(
                                        t - MEM[(FONT_GLUE[main_f as usize] + 1) as usize].b32.s1,
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
                if cur_input.state != InputState::TokenList || cur_input.index != Btl::BackedUpChar
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
                        cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                        back_input();
                        cur_input.index = Btl::BackedUpChar;
                        begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                    cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                    back_input();
                    cur_input.index = Btl::BackedUpChar;
                    begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                                    *LLIST_link(cur_list.tail) = new_kern(
                                        FONT_INFO[(KERN_BASE[main_f as usize]
                                            + 256 * main_j.s1 as i32
                                            + main_j.s0 as i32)
                                            as usize]
                                            .b32
                                            .s1,
                                    )
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
                        get_next();
                        match cur_cmd {
                            Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {}
                            _ => {
                                x_token();
                                match cur_cmd {
                                    Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {}
                                    Cmd::CharNum => {
                                        scan_char_num();
                                        cur_chr = cur_val;
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
                                    if cur_input.state != InputState::TokenList
                                        || cur_input.index != Btl::BackedUpChar
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
                                            cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                            back_input();
                                            cur_input.index = Btl::BackedUpChar;
                                            begin_token_list(
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
                                        cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr;
                                        back_input();
                                        cur_input.index = Btl::BackedUpChar;
                                        begin_token_list(
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

    unsafe fn append_normal_space() {
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
                if cur_cs == 0 {
                    if cur_cmd == Cmd::CharNum {
                        cur_cmd = Cmd::OtherChar;
                    }
                    cur_tok = cur_cmd as i32 * MAX_CHAR_VAL + cur_chr
                } else {
                    cur_tok = CS_TOKEN_FLAG + cur_cs
                }
                back_input();
                begin_token_list(MEM[c + 1].b32.s1 as usize, Btl::InterCharText);
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
                        .set_size(FONT_INFO[main_k as usize].b32.s1)
                        .set_stretch(FONT_INFO[(main_k + 1) as usize].b32.s1)
                        .set_shrink(FONT_INFO[(main_k + 2) as usize].b32.s1);
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
    old_setting = selector;
    selector = Selector::NEW_STRING;
    show_token_list(LLIST_link(p as usize).opt(), None, pool_size - pool_ptr);
    selector = old_setting;
    make_string()
}
pub(crate) unsafe fn scan_pdf_ext_toks() {
    scan_toks(false, true);
}
pub(crate) unsafe fn compare_strings() {
    unsafe fn done(s1: str_number, s2: str_number) {
        flush_str(s2);
        flush_str(s1);
        cur_val_level = ValLevel::Int;
    }
    scan_toks(false, true);
    let s1 = tokens_to_string(def_ref as i32);
    delete_token_ref(def_ref);
    scan_toks(false, true);
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
            cur_val = -1i32;
            return done(s1, s2);
        } else if str_pool[i1 as usize] as i32 > str_pool[i2 as usize] as i32 {
            cur_val = 1i32;
            return done(s1, s2);
        } else {
            i1 += 1;
            i2 += 1
        }
    }
    if i1 == j1 && i2 == j2 {
        cur_val = 0i32
    } else if i1 < j1 {
        cur_val = 1i32
    } else {
        cur_val = -1i32
    }
    done(s1, s2);
}
pub(crate) unsafe fn prune_page_top(mut popt: Option<usize>, mut s: bool) -> i32 {
    let mut r: i32 = None.tex_int();
    let mut prev_p = TEMP_HEAD;
    *LLIST_link(TEMP_HEAD) = popt.tex_int();
    while let Some(p) = popt {
        match TxtNode::from(p) {
            TxtNode::HList(b) | TxtNode::VList(b) => {
                let (q, mut tmp_ptr) = new_skip_param(GluePar::split_top_skip);
                *LLIST_link(prev_p) = Some(q).tex_int();
                *LLIST_link(q) = Some(b.ptr()).tex_int();
                if tmp_ptr.size() > b.height() {
                    tmp_ptr.set_size(tmp_ptr.size() - b.height());
                } else {
                    tmp_ptr.set_size(0);
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
                    tmp_ptr.set_size(0);
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
pub(crate) unsafe fn do_assignments() {
    loop {
        loop {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
                break;
            }
        }
        if cur_cmd <= MAX_NON_PREFIXED_COMMAND {
            return;
        }
        set_box_allowed = false;
        prefixed_command();
        set_box_allowed = true
    }
}
