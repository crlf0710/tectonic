#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, pdf_error};
use crate::xetex_ext::measure_native_node;
use crate::xetex_ini::{
    active_width, adjust_tail, arith_error, avail, cur_l, cur_lang, cur_list, cur_q, cur_r,
    file_line_error_style_p, first_p, font_in_short_display, global_prev_p, hc, help_line,
    help_ptr, hf, hi_mem_min, hlist_stack, hlist_stack_level, hu, hyf, hyf_distance, hyf_next,
    hyf_num, hyph_index, hyph_start, hyphen_passed, init_lft, init_lig, init_list, init_trie,
    just_box, last_leftmost_char, last_rightmost_char, lft_hit, lig_stack, ligature_present,
    max_hyph_char, op_start, pack_begin_line, pre_adjust_tail, rt_hit, semantic_pagination_enabled,
    str_pool, str_start, temp_ptr, trie_not_ready, trie_trc, trie_trl, trie_tro,
    xtx_ligature_present, BCHAR_LABEL, CHAR_BASE, EQTB, FONT_BCHAR, FONT_INFO, HYPHEN_CHAR,
    HYPH_LINK, HYPH_LIST, HYPH_WORD, KERN_BASE, LIG_KERN_BASE, MEM, WIDTH_BASE,
};
use crate::xetex_ini::{b16x4, memory_word};
use crate::xetex_output::{print_cstr, print_file_line, print_nl_cstr};
use crate::xetex_stringpool::length;
use crate::xetex_xetex0::{
    append_to_vlist, badness, char_pw, delete_glue_ref, effective_char, flush_list,
    flush_node_list, fract, free_node, get_avail, get_node, hpack, max_hyphenatable_length,
    new_character, new_disc, new_kern, new_lig_item, new_ligature, new_margin_kern, new_math,
    new_native_character, new_native_word_node, new_param_glue, new_penalty, new_spec, pop_nest,
    prev_rightmost,
};
use crate::xetex_xetex0::{
    BOX_width, GLUE_NODE_glue_ptr, GLUE_NODE_leader_ptr, GLUE_SPEC_shrink, GLUE_SPEC_shrink_order,
    GLUE_SPEC_stretch, GLUE_SPEC_stretch_order, LLIST_info, LLIST_link, NODE_subtype, NODE_type,
    PENALTY_NODE_penalty,
};
use crate::xetex_xetexd::{is_char_node, is_non_discardable_node};

pub(crate) type scaled_t = i32;
pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
pub(crate) type small_number = i16;
pub(crate) type internal_font_number = i32;
pub(crate) type font_index = i32;
pub(crate) type nine_bits = i32;
pub(crate) type trie_pointer = i32;
pub(crate) type trie_opcode = u16;
pub(crate) type hyph_pointer = u16;

const AWFUL_BAD: i32 = 0x3FFFFFFF;
const VERY_LOOSE_FIT: usize = 0;
const LOOSE_FIT: usize = 1;
const DECENT_FIT: usize = 2;
const TIGHT_FIT: usize = 3;
const LAST_ACTIVE: i32 = ACTIVE_LIST;

static mut passive: i32 = 0;
static mut cur_active_width: [scaled_t; 7] = [0; 7];
static mut background: [scaled_t; 7] = [0; 7];
static mut break_width: [scaled_t; 7] = [0; 7];
static mut best_place: [i32; 4] = [0; 4];
static mut best_pl_line: [i32; 4] = [0; 4];
static mut disc_width: scaled_t = 0;
static mut no_shrink_error_yet: bool = false;
static mut cur_p: i32 = 0;
static mut second_pass: bool = false;
static mut final_pass: bool = false;
static mut threshold: i32 = 0;
static mut minimal_demerits: [i32; 4] = [0; 4];
static mut minimum_demerits: i32 = 0;
static mut easy_line: i32 = 0;
static mut last_special_line: i32 = 0;
static mut first_width: scaled_t = 0;
static mut second_width: scaled_t = 0;
static mut first_indent: scaled_t = 0;
static mut second_indent: scaled_t = 0;
static mut best_bet: i32 = 0;
static mut fewest_demerits: i32 = 0;
static mut best_line: i32 = 0;
static mut actual_looseness: i32 = 0;
static mut line_diff: i32 = 0;
static mut hn: small_number = 0;
static mut ha: i32 = 0;
static mut hb: i32 = 0;
static mut hyf_char: i32 = 0;
static mut init_cur_lang: u8 = 0;
static mut l_hyf: i32 = 0;
static mut r_hyf: i32 = 0;
static mut init_l_hyf: i32 = 0;
static mut init_r_hyf: i32 = 0;
static mut hyf_bchar: i32 = 0;
static mut last_line_fill: i32 = 0;
static mut do_last_line_fit: bool = false;
static mut active_node_size: small_number = 0;
static mut fill_width: [scaled_t; 3] = [0; 3];
static mut best_pl_short: [scaled_t; 4] = [0; 4];
static mut best_pl_glue: [scaled_t; 4] = [0; 4];
#[inline]
unsafe extern "C" fn get_native_usv(mut p: i32, mut i: i32) -> UnicodeScalar {
    let mut c: u16 =
        *(&mut MEM[(p + 6) as usize] as *mut memory_word as *mut u16).offset(i as isize);
    if c as i32 >= 0xd800i32 && (c as i32) < 0xdc00i32 {
        return 0x10000i32
            + (c as i32 - 0xd800i32) * 0x400i32
            + *(&mut MEM[(p + 6) as usize] as *mut memory_word as *mut u16)
                .offset((i + 1i32) as isize) as i32
            - 0xdc00i32;
    }
    c as UnicodeScalar
}
/* Break a paragraph into lines (XTTP:843).
 *
 * d: true if we are breaking a partial paragraph preceding display math mode
 *
 * Should only be called in horizontal mode. Will leave horizontal and place
 * the output in the enclosing vertical list.
 *
 * `cur_list.head` is the non-empty hlist to be broken. `prev_graf` tells the
 * starting line number (0 unless we're continuing after display math). After
 * completion, `just_box` will point to the final box created.
 */
#[no_mangle]
pub(crate) unsafe extern "C" fn line_break(mut d: bool) {
    let mut current_block: u64; /* "this is for over/underfull box messages" */
    let mut auto_breaking: bool = false;
    let mut prev_p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut prev_s: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut j: small_number = 0;
    let mut c: UnicodeScalar = 0;
    let mut l: i32 = 0;
    let mut i: i32 = 0;
    let mut for_end_1: i32 = 0;
    pack_begin_line = cur_list.mode_line; /* "this is for over/underfull box messages" */
    *LLIST_link(TEMP_HEAD as isize) = *LLIST_link(cur_list.head as isize);

    /* Remove trailing space or glue if present; add infinite penalty then par_fill_skip */

    if is_char_node(cur_list.tail) {
        /* is_char_node */
        *LLIST_link(cur_list.tail as isize) = new_penalty(INF_PENALTY);
        cur_list.tail = *LLIST_link(cur_list.tail as isize);
    } else if *NODE_type(cur_list.tail as isize) != GLUE_NODE {
        *LLIST_link(cur_list.tail as isize) = new_penalty(INF_PENALTY);
        cur_list.tail = *LLIST_link(cur_list.tail as isize);
    } else {
        *NODE_type(cur_list.tail as isize) = PENALTY_NODE;
        delete_glue_ref(*GLUE_NODE_glue_ptr(cur_list.tail as isize));
        flush_node_list(*GLUE_NODE_leader_ptr(cur_list.tail as isize));
        *PENALTY_NODE_penalty(cur_list.tail as isize) = INF_PENALTY;
    }

    *LLIST_link(cur_list.tail as isize) = new_param_glue(GLUE_PAR__par_fill_skip as _);
    last_line_fill = *LLIST_link(cur_list.tail as isize);

    /* Yet more initialization of various kinds */

    init_cur_lang = (cur_list.prev_graf % 65536) as _;
    init_l_hyf = cur_list.prev_graf / 0x400000;
    init_r_hyf = (cur_list.prev_graf / 65536) % 64;

    pop_nest();

    no_shrink_error_yet = true;

    if *GLUE_SPEC_shrink_order(*GLUEPAR(GLUE_PAR__left_skip) as isize) != NORMAL as _
        && *GLUE_SPEC_shrink(*GLUEPAR(GLUE_PAR__left_skip) as isize) != 0
    {
        *GLUEPAR(GLUE_PAR__left_skip) = finite_shrink(*GLUEPAR(GLUE_PAR__left_skip));
    }
    if *GLUE_SPEC_shrink_order(*GLUEPAR(GLUE_PAR__right_skip) as isize) != NORMAL as _
        && *GLUE_SPEC_shrink(*GLUEPAR(GLUE_PAR__right_skip) as isize) != 0
    {
        *GLUEPAR(GLUE_PAR__right_skip) = finite_shrink(*GLUEPAR(GLUE_PAR__right_skip));
    }

    q = *GLUEPAR(GLUE_PAR__left_skip);
    r = *GLUEPAR(GLUE_PAR__right_skip);
    background[1] = *BOX_width(q as isize) + *BOX_width(r as isize);
    background[2] = 0;
    background[3] = 0;
    background[4] = 0;
    background[5] = 0;
    background[2 + *GLUE_SPEC_stretch_order(q as isize) as usize] = *GLUE_SPEC_stretch(q as isize);
    background[2 + *GLUE_SPEC_stretch_order(r as isize) as usize] += *GLUE_SPEC_stretch(r as isize);
    background[6] = *GLUE_SPEC_shrink(q as isize) + *GLUE_SPEC_shrink(r as isize);

    /* 1631: "check for special treatment of last line of paragraph" (\lastlinefit > 0) */

    do_last_line_fit = false; /*863:*/
    active_node_size = ACTIVE_NODE_SIZE_NORMAL as _;
    if INTPAR(INT_PAR__last_line_fit) > 0 {
        q = *GLUE_NODE_glue_ptr(last_line_fill as isize);
        if *GLUE_SPEC_stretch(q as isize) > 0 && *GLUE_SPEC_stretch_order(q as isize) > NORMAL as _
        {
            if background[3] == 0 && background[4] == 0 && background[5] == 0 {
                do_last_line_fit = true;
                active_node_size = ACTIVE_NODE_SIZE_EXTENDED as _;
                fill_width[0] = 0;
                fill_width[1] = 0;
                fill_width[2] = 0;
                fill_width[*GLUE_SPEC_stretch_order(q as isize) as usize - 1] =
                    *GLUE_SPEC_stretch(q as isize);
            }
        }
    }
    minimum_demerits = AWFUL_BAD; /* 863: */
    minimal_demerits[TIGHT_FIT] = AWFUL_BAD;
    minimal_demerits[DECENT_FIT] = AWFUL_BAD;
    minimal_demerits[LOOSE_FIT] = AWFUL_BAD;
    minimal_demerits[VERY_LOOSE_FIT] = AWFUL_BAD;

    /* Prep relating to par_shape (877) */

    if LOCAL(LOCAL__par_shape) == TEX_NULL {
        if DIMENPAR(DIMEN_PAR__hang_indent) == 0 {
            last_special_line = 0;
            second_width = DIMENPAR(DIMEN_PAR__hsize);
            second_indent = 0;
        } else {
            /*878:*/
            last_special_line = INTPAR(INT_PAR__hang_after).abs();

            if INTPAR(INT_PAR__hang_after) < 0 {
                first_width = DIMENPAR(DIMEN_PAR__hsize) - DIMENPAR(DIMEN_PAR__hang_indent).abs();
                if DIMENPAR(DIMEN_PAR__hang_indent) >= 0 {
                    first_indent = DIMENPAR(DIMEN_PAR__hang_indent);
                } else {
                    first_indent = 0;
                }
                second_width = DIMENPAR(DIMEN_PAR__hsize);
                second_indent = 0;
            } else {
                first_width = DIMENPAR(DIMEN_PAR__hsize);
                first_indent = 0;
                second_width = DIMENPAR(DIMEN_PAR__hsize) - DIMENPAR(DIMEN_PAR__hang_indent).abs();
                if DIMENPAR(DIMEN_PAR__hang_indent) >= 0 {
                    second_indent = DIMENPAR(DIMEN_PAR__hang_indent);
                } else {
                    second_indent = 0;
                }
            }
        }
    } else {
        last_special_line = *LLIST_info(LOCAL(LOCAL__par_shape) as isize) - 1;
        /* These direct `mem` accesses are in the original WEB code */
        second_width = MEM[LOCAL(LOCAL__par_shape) as usize + 2 * (last_special_line as usize + 1)]
            .b32
            .s1;
        second_indent = MEM[LOCAL(LOCAL__par_shape) as usize + 2 * last_special_line as usize + 1]
            .b32
            .s1;
    }

    if INTPAR(INT_PAR__looseness) == 0 {
        easy_line = last_special_line
    } else {
        easy_line = MAX_HALFWORD; /*:877*/
    }

    /* Start finding optimal breakpoints (892) */

    threshold = INTPAR(INT_PAR__pretolerance);
    if threshold >= 0 {
        second_pass = false;
        final_pass = false
    } else {
        threshold = INTPAR(INT_PAR__tolerance);
        second_pass = true;
        final_pass = DIMENPAR(DIMEN_PAR__emergency_stretch) <= 0;
    }
    loop {
        if threshold > INF_BAD {
            threshold = INF_BAD;
        }
        if second_pass {
            /*920:*/
            if trie_not_ready {
                init_trie(); /*893:*/
            }
            cur_lang = init_cur_lang;
            l_hyf = init_l_hyf;
            r_hyf = init_r_hyf;
            if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32 != cur_lang as i32 {
                hyph_index = 0i32
            } else {
                hyph_index = *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
            }
        }
        q = get_node(active_node_size as i32);
        *NODE_type(q as isize) = UNHYPHENATED as _;
        MEM[q as usize].b16.s0 = DECENT_FIT as _;
        *LLIST_link(q as isize) = LAST_ACTIVE;
        MEM[(q + 1) as usize].b32.s1 = TEX_NULL;
        MEM[(q + 1) as usize].b32.s0 = cur_list.prev_graf + 1;
        MEM[(q + 2) as usize].b32.s1 = 0;
        *LLIST_link(ACTIVE_LIST as isize) = q;

        if do_last_line_fit {
            /*1633:*/
            MEM[(q + 3) as usize].b32.s1 = 0; /*:893*/
            MEM[(q + 4) as usize].b32.s1 = 0
        }
        active_width[1] = background[1];
        active_width[2] = background[2];
        active_width[3] = background[3];
        active_width[4] = background[4];
        active_width[5] = background[5];
        active_width[6] = background[6];
        passive = TEX_NULL;
        font_in_short_display = 0; /*:893*/
        cur_p = *LLIST_link(TEMP_HEAD as isize);
        auto_breaking = true;

        global_prev_p = cur_p;
        prev_p = global_prev_p;
        first_p = cur_p;

        while cur_p != TEX_NULL && *LLIST_link(ACTIVE_LIST as isize) != LAST_ACTIVE {
            /*895: "Call try_break if cur_p is a legal breakpoint; on the
             * second pass, also try to hyphenate the next word, if cur_p is a
             * glue node; then advance cur_p to the next node of the paragraph
             * that could possibly be a legal breakpoint." */
            if is_char_node(cur_p) {
                /*896:*/
                global_prev_p = cur_p;
                prev_p = global_prev_p;
                loop {
                    let mut eff_char: i32 = 0;
                    f = MEM[cur_p as usize].b16.s1 as internal_font_number;
                    eff_char = effective_char(1i32 != 0, f, MEM[cur_p as usize].b16.s0);
                    active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                        + FONT_INFO[(CHAR_BASE[f as usize] + eff_char) as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1;
                    cur_p = *LLIST_link(cur_p as isize);
                    if !is_char_node(cur_p) {
                        break;
                    }
                }
            }
            match *NODE_type(cur_p as isize) {
                HLIST_NODE | VLIST_NODE | RULE_NODE => {
                    active_width[1] += *BOX_width(cur_p as isize);
                }
                WHATSIT_NODE => {
                    if *NODE_subtype(cur_p as isize) == LANGUAGE_NODE as _ {
                        cur_lang = MEM[(cur_p + 1) as usize].b32.s1 as u8;
                        l_hyf = MEM[(cur_p + 1) as usize].b16.s1 as i32;
                        r_hyf = MEM[(cur_p + 1) as usize].b16.s0 as i32;
                        if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32
                            != cur_lang as i32
                        {
                            hyph_index = 0i32
                        } else {
                            hyph_index = *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
                        }
                    } else if *NODE_subtype(cur_p as isize) == NATIVE_WORD_NODE
                        || *NODE_subtype(cur_p as isize) == NATIVE_WORD_NODE_AT
                        || *NODE_subtype(cur_p as isize) == GLYPH_NODE
                        || *NODE_subtype(cur_p as isize) == PIC_NODE
                        || *NODE_subtype(cur_p as isize) == PDF_NODE
                    {
                        active_width[1] += *BOX_width(cur_p as isize);
                    }
                }
                GLUE_NODE => {
                    if auto_breaking {
                        if is_char_node(prev_p) {
                            try_break(0, UNHYPHENATED as _);
                        } else if is_non_discardable_node(prev_p) {
                            try_break(0, UNHYPHENATED as _);
                        } else if *NODE_type(prev_p as isize) == KERN_NODE
                            && *NODE_subtype(prev_p as isize) != EXPLICIT as _
                        {
                            try_break(0, UNHYPHENATED as _);
                        }
                    }
                    q = *GLUE_NODE_glue_ptr(cur_p as isize);
                    if MEM[q as usize].b16.s0 as i32 != 0 && MEM[(q + 3) as usize].b32.s1 != 0 {
                        let ref mut fresh3 = MEM[(cur_p + 1) as usize].b32.s0;
                        *fresh3 = finite_shrink(q);
                        q = *fresh3
                    }
                    active_width[1] += MEM[(q + 1) as usize].b32.s1;
                    active_width[(2i32 + MEM[q as usize].b16.s1 as i32) as usize] +=
                        MEM[(q + 2) as usize].b32.s1;
                    /*:895*/
                    active_width[6] += MEM[(q + 3) as usize].b32.s1; /*:897*/
                    if second_pass as i32 != 0 && auto_breaking as i32 != 0 {
                        /*924: "Try to hyphenate the following word." */
                        prev_s = cur_p;
                        s = MEM[prev_s as usize].b32.s1;
                        if s != -0xfffffffi32 {
                            's_786: loop
                            /*930: skip to node ha, or goto done1 if no hyphenation should be attempted */
                            {
                                if is_char_node(s) {
                                    c = MEM[s as usize].b16.s0 as UnicodeScalar; /*:930*/
                                    hf = MEM[s as usize].b16.s1 as internal_font_number;
                                    current_block = 11202235766349324107;
                                } else if MEM[s as usize].b16.s1 as i32 == 6 {
                                    if MEM[(s + 1) as usize].b32.s1 == -0xfffffff {
                                        current_block = 13855806088735179493;
                                    } else {
                                        q = MEM[(s + 1) as usize].b32.s1;
                                        c = MEM[q as usize].b16.s0 as UnicodeScalar;
                                        hf = MEM[q as usize].b16.s1 as internal_font_number;
                                        current_block = 11202235766349324107;
                                    }
                                } else if MEM[s as usize].b16.s1 as i32 == 11
                                    && MEM[s as usize].b16.s0 as i32 == 0
                                {
                                    current_block = 13855806088735179493;
                                } else if MEM[s as usize].b16.s1 as i32 == 9
                                    && MEM[s as usize].b16.s0 as i32 >= 4
                                {
                                    current_block = 13855806088735179493;
                                } else {
                                    if !(MEM[s as usize].b16.s1 as i32 == 8) {
                                        current_block = 8166967358843938227;
                                        break;
                                    }
                                    if MEM[s as usize].b16.s0 as i32 == 40
                                        || MEM[s as usize].b16.s0 as i32 == 41
                                    {
                                        l = 0i32;
                                        while l < MEM[(s + 4) as usize].b16.s1 as i32 {
                                            c = get_native_usv(s, l);
                                            if LC_CODE(c) != 0 {
                                                hf = MEM[(s + 4) as usize].b16.s2
                                                    as internal_font_number;
                                                prev_s = s;
                                                current_block = 16581706250867416845;
                                                break 's_786;
                                            } else {
                                                if c as i64 >= 65536 {
                                                    l += 1
                                                }
                                                l += 1
                                            }
                                        }
                                    }
                                    if MEM[s as usize].b16.s0 as i32 == 4 {
                                        cur_lang = MEM[(s + 1) as usize].b32.s1 as u8;
                                        l_hyf = MEM[(s + 1) as usize].b16.s1 as i32;
                                        r_hyf = MEM[(s + 1) as usize].b16.s0 as i32;
                                        if *trie_trc.offset((hyph_start + cur_lang as i32) as isize)
                                            as i32
                                            != cur_lang as i32
                                        {
                                            hyph_index = 0i32
                                        } else {
                                            hyph_index = *trie_trl
                                                .offset((hyph_start + cur_lang as i32) as isize)
                                        }
                                    }
                                    current_block = 13855806088735179493;
                                }
                                match current_block {
                                    11202235766349324107 => {
                                        if hyph_index == 0i32 || c > 255i32 {
                                            hc[0] = LC_CODE(c);
                                        } else if *trie_trc.offset((hyph_index + c) as isize) as i32
                                            != c
                                        {
                                            hc[0] = 0i32
                                        } else {
                                            hc[0] = *trie_tro.offset((hyph_index + c) as isize)
                                        }
                                        if hc[0] != 0i32 {
                                            if hc[0] == c || INTPAR(INT_PAR__uc_hyph) > 0 {
                                                current_block = 16581706250867416845;
                                                break;
                                            } else {
                                                current_block = 8166967358843938227;
                                                break;
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                                prev_s = s;
                                s = MEM[prev_s as usize].b32.s1
                            }
                            match current_block {
                                8166967358843938227 => {}
                                _ => {
                                    hyf_char = HYPHEN_CHAR[hf as usize];
                                    if !(hyf_char < 0i32) {
                                        if !(hyf_char > 0xffffi32) {
                                            ha = prev_s;
                                            if !(l_hyf + r_hyf > max_hyphenatable_length()) {
                                                if ha != -0xfffffffi32
                                                    && ha < hi_mem_min
                                                    && MEM[ha as usize].b16.s1 as i32 == 8
                                                    && (MEM[ha as usize].b16.s0 as i32 == 40
                                                        || MEM[ha as usize].b16.s0 as i32 == 41)
                                                {
                                                    /*926: check that nodes after native_word permit hyphenation; if not, goto done1 */
                                                    s = MEM[ha as usize].b32.s1;
                                                    loop {
                                                        if !is_char_node(s) {
                                                            match MEM[s as usize].b16.s1 as i32 {
                                                                6 => {}
                                                                11 => {
                                                                    if MEM[s as usize].b16.s0 as i32
                                                                        != 0i32
                                                                    {
                                                                        current_block =
                                                                            2606747282402567793;
                                                                        break;
                                                                    }
                                                                }
                                                                8 | 10 | 12 | 3 | 5 | 4 => {
                                                                    current_block =
                                                                        2606747282402567793;
                                                                    break;
                                                                }
                                                                _ => {
                                                                    current_block =
                                                                        8166967358843938227;
                                                                    break;
                                                                }
                                                            }
                                                        }
                                                        s = MEM[s as usize].b32.s1
                                                    }
                                                    match current_block {
                                                        8166967358843938227 => {}
                                                        _ => {
                                                            /*927: prepare a native_word_node for hyphenation.
                                                             * "Note that if there are chars with lccode = 0,
                                                             * we split them out into separate native_word
                                                             * nodes." */
                                                            hn = 0i32 as small_number;
                                                            'c_31290: loop {
                                                                /* 'ha' can change in the loop, so for safety: */
                                                                for_end_1 = MEM
                                                                    [(ha + 4i32) as usize]
                                                                    .b16
                                                                    .s1
                                                                    as i32;
                                                                l = 0i32;
                                                                loop {
                                                                    if !(l < for_end_1) {
                                                                        break 'c_31290;
                                                                    }
                                                                    c = get_native_usv(ha, l);
                                                                    if hyph_index == 0i32
                                                                        || c > 255i32
                                                                    {
                                                                        hc[0] = LC_CODE(c);
                                                                    } else if *trie_trc.offset(
                                                                        (hyph_index + c) as isize,
                                                                    )
                                                                        as i32
                                                                        != c
                                                                    {
                                                                        hc[0] = 0i32
                                                                    } else {
                                                                        hc[0] = *trie_tro.offset(
                                                                            (hyph_index + c)
                                                                                as isize,
                                                                        )
                                                                    }
                                                                    if hc[0] == 0i32 {
                                                                        if hn as i32 > 0i32 {
                                                                            q
                                                                                    =
                                                                                    new_native_word_node(hf,
                                                                                                         MEM[(ha
                                                                                                                           +
                                                                                                                           4i32)
                                                                                                                          as
                                                                                                                          usize].b16.s1
                                                                                                             as
                                                                                                             i32
                                                                                                             -
                                                                                                             l);
                                                                            MEM[q as usize]
                                                                                .b16
                                                                                .s0 = MEM
                                                                                [ha as usize]
                                                                                .b16
                                                                                .s0;
                                                                            i = l;
                                                                            while i < MEM[(ha
                                                                                + 4i32)
                                                                                as usize]
                                                                                .b16
                                                                                .s1
                                                                                as i32
                                                                            {
                                                                                *(&mut MEM[(q
                                                                                                            +
                                                                                                            6i32)
                                                                                                           as
                                                                                                           usize]
                                                                                          as
                                                                                          *mut memory_word
                                                                                          as
                                                                                          *mut u16).offset((i
                                                                                                                           -
                                                                                                                           l)
                                                                                                                          as
                                                                                                                          isize)
                                                                                        =
                                                                                        *(&mut MEM[(ha
                                                                                                                +
                                                                                                                6i32)
                                                                                                               as
                                                                                                               usize]
                                                                                              as
                                                                                              *mut memory_word
                                                                                              as
                                                                                              *mut u16).offset(i
                                                                                                                              as
                                                                                                                              isize);
                                                                                i += 1
                                                                            }
                                                                            measure_native_node(&mut MEM[q
                                                                                                                         as
                                                                                                                         usize]
                                                                                                        as
                                                                                                        *mut memory_word
                                                                                                        as
                                                                                                        *mut libc::c_void,
                                                                                                    (EQTB[(1i32
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        15000i32
                                                                                                                        +
                                                                                                                        12i32
                                                                                                                        +
                                                                                                                        9000i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        19i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        13i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        4i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        3i32
                                                                                                                            *
                                                                                                                            256i32
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        74i32)
                                                                                                                       as
                                                                                                                       usize].b32.s1
                                                                                                         >
                                                                                                         0i32)
                                                                                                        as
                                                                                                        i32);
                                                                            MEM[q as usize]
                                                                                .b32
                                                                                .s1 = MEM
                                                                                [ha as usize]
                                                                                .b32
                                                                                .s1;
                                                                            MEM[ha as usize]
                                                                                .b32
                                                                                .s1 = q;
                                                                            MEM[(ha + 4)
                                                                                as usize]
                                                                                .b16
                                                                                .s1 = l as u16;
                                                                            measure_native_node(&mut MEM[ha
                                                                                                                         as
                                                                                                                         usize]
                                                                                                        as
                                                                                                        *mut memory_word
                                                                                                        as
                                                                                                        *mut libc::c_void,
                                                                                                    (EQTB[(1i32
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        15000i32
                                                                                                                        +
                                                                                                                        12i32
                                                                                                                        +
                                                                                                                        9000i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        19i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        13i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        4i32
                                                                                                                        +
                                                                                                                        256i32
                                                                                                                        +
                                                                                                                        1i32
                                                                                                                        +
                                                                                                                        3i32
                                                                                                                            *
                                                                                                                            256i32
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        (0x10ffffi32
                                                                                                                             +
                                                                                                                             1i32)
                                                                                                                        +
                                                                                                                        74i32)
                                                                                                                       as
                                                                                                                       usize].b32.s1
                                                                                                         >
                                                                                                         0i32)
                                                                                                        as
                                                                                                        i32);
                                                                            break 'c_31290;
                                                                        }
                                                                    } else if hn as i32 == 0i32
                                                                        && l > 0i32
                                                                    {
                                                                        q = new_native_word_node(
                                                                            hf,
                                                                            MEM[(ha + 4) as usize]
                                                                                .b16
                                                                                .s1
                                                                                as i32
                                                                                - l,
                                                                        );
                                                                        MEM[q as usize].b16.s0 =
                                                                            MEM[ha as usize].b16.s0;
                                                                        i = l;
                                                                        while i < MEM
                                                                            [(ha + 4i32) as usize]
                                                                            .b16
                                                                            .s1
                                                                            as i32
                                                                        {
                                                                            *(&mut MEM
                                                                                [(q + 6) as usize]
                                                                                as *mut memory_word
                                                                                as *mut u16)
                                                                                .offset(
                                                                                    (i - l)
                                                                                        as isize,
                                                                                ) = *(&mut MEM[(ha
                                                                                + 6i32)
                                                                                as usize]
                                                                                as *mut memory_word
                                                                                as *mut u16)
                                                                                .offset(i as isize);
                                                                            i += 1
                                                                        }
                                                                        measure_native_node(&mut MEM[q
                                                                                                                     as
                                                                                                                     usize]
                                                                                                    as
                                                                                                    *mut memory_word
                                                                                                    as
                                                                                                    *mut libc::c_void,
                                                                                                (EQTB[(1i32
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    15000i32
                                                                                                                    +
                                                                                                                    12i32
                                                                                                                    +
                                                                                                                    9000i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    19i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    13i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    4i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    3i32
                                                                                                                        *
                                                                                                                        256i32
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    74i32)
                                                                                                                   as
                                                                                                                   usize].b32.s1
                                                                                                     >
                                                                                                     0i32)
                                                                                                    as
                                                                                                    i32);
                                                                        MEM[q as usize].b32.s1 =
                                                                            MEM[ha as usize].b32.s1;
                                                                        MEM[ha as usize].b32.s1 = q;
                                                                        MEM[(ha + 4) as usize]
                                                                            .b16
                                                                            .s1 = l as u16;
                                                                        measure_native_node(&mut MEM[ha
                                                                                                                     as
                                                                                                                     usize]
                                                                                                    as
                                                                                                    *mut memory_word
                                                                                                    as
                                                                                                    *mut libc::c_void,
                                                                                                (EQTB[(1i32
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    15000i32
                                                                                                                    +
                                                                                                                    12i32
                                                                                                                    +
                                                                                                                    9000i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    19i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    13i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    4i32
                                                                                                                    +
                                                                                                                    256i32
                                                                                                                    +
                                                                                                                    1i32
                                                                                                                    +
                                                                                                                    3i32
                                                                                                                        *
                                                                                                                        256i32
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    (0x10ffffi32
                                                                                                                         +
                                                                                                                         1i32)
                                                                                                                    +
                                                                                                                    74i32)
                                                                                                                   as
                                                                                                                   usize].b32.s1
                                                                                                     >
                                                                                                     0i32)
                                                                                                    as
                                                                                                    i32);
                                                                        ha =
                                                                            MEM[ha as usize].b32.s1;
                                                                        break;
                                                                    } else {
                                                                        if hn
                                                                                   as
                                                                                   i32
                                                                                   ==
                                                                                   max_hyphenatable_length()
                                                                               {
                                                                                break
                                                                                    'c_31290
                                                                                    ;
                                                                            }
                                                                        hn += 1;
                                                                        if (c as i64) < 65536 {
                                                                            hu[hn as usize] = c;
                                                                            hc[hn as usize] = hc[0]
                                                                        } else {
                                                                            hu[hn as usize] = ((c
                                                                                as i64
                                                                                - 65536)
                                                                                / 1024i32 as i64
                                                                                + 0xd800i32 as i64)
                                                                                as i32;
                                                                            hc[hn as usize] = ((hc
                                                                                [0]
                                                                                as i64
                                                                                - 65536)
                                                                                / 1024i32 as i64
                                                                                + 0xd800i32 as i64)
                                                                                as i32;
                                                                            hn += 1;
                                                                            hu[hn as usize] = c
                                                                                % 1024i32
                                                                                + 0xdc00i32;
                                                                            hc[hn as usize] = hc[0]
                                                                                % 1024i32
                                                                                + 0xdc00i32;
                                                                            l += 1
                                                                        }
                                                                        hyf_bchar = 65536i32
                                                                    }
                                                                    l += 1
                                                                }
                                                            }
                                                            current_block = 4362442400146949691;
                                                        }
                                                    }
                                                } else {
                                                    /*931: skip to node hb, putting letters into hu and hc */
                                                    hn = 0i32 as small_number;
                                                    's_1342: loop {
                                                        if is_char_node(s) {
                                                            if MEM[s as usize].b16.s1 as i32 != hf {
                                                                break;
                                                            }
                                                            hyf_bchar =
                                                                MEM[s as usize].b16.s0 as i32;
                                                            c = hyf_bchar;
                                                            if hyph_index == 0i32 || c > 255i32 {
                                                                hc[0] = LC_CODE(c);
                                                            } else if *trie_trc
                                                                .offset((hyph_index + c) as isize)
                                                                as i32
                                                                != c
                                                            {
                                                                hc[0] = 0i32
                                                            } else {
                                                                hc[0] = *trie_tro.offset(
                                                                    (hyph_index + c) as isize,
                                                                )
                                                            }
                                                            if hc[0] == 0i32 {
                                                                break;
                                                            }
                                                            if hc[0] > max_hyph_char {
                                                                break;
                                                            }
                                                            if hn as i32
                                                                == max_hyphenatable_length()
                                                            {
                                                                break;
                                                            }
                                                            hb = s;
                                                            hn += 1;
                                                            hu[hn as usize] = c;
                                                            hc[hn as usize] = hc[0];
                                                            hyf_bchar = 65536i32
                                                        } else if MEM[s as usize].b16.s1 as i32
                                                            == 6i32
                                                        {
                                                            /*932: move the characters of a ligature node to hu and hc; but goto done3
                                                             * if they are not all letters. */
                                                            if MEM[(s + 1) as usize].b16.s1 as i32
                                                                != hf
                                                            {
                                                                break;
                                                            }
                                                            j = hn;
                                                            q = MEM[(s + 1) as usize].b32.s1;
                                                            if q > TEX_NULL {
                                                                hyf_bchar =
                                                                    MEM[q as usize].b16.s0 as i32
                                                            }
                                                            while q > TEX_NULL {
                                                                c = MEM[q as usize].b16.s0
                                                                    as UnicodeScalar;
                                                                if hyph_index == 0i32 || c > 255i32
                                                                {
                                                                    hc[0] = LC_CODE(c);
                                                                } else if *trie_trc.offset(
                                                                    (hyph_index + c) as isize,
                                                                )
                                                                    as i32
                                                                    != c
                                                                {
                                                                    hc[0] = 0i32
                                                                } else {
                                                                    hc[0] = *trie_tro.offset(
                                                                        (hyph_index + c) as isize,
                                                                    )
                                                                }
                                                                if hc[0] == 0i32 {
                                                                    break 's_1342;
                                                                }
                                                                if hc[0] > max_hyph_char {
                                                                    break 's_1342;
                                                                }
                                                                if j as i32
                                                                    == max_hyphenatable_length()
                                                                {
                                                                    break 's_1342;
                                                                }
                                                                j += 1;
                                                                hu[j as usize] = c;
                                                                hc[j as usize] = hc[0];
                                                                q = MEM[q as usize].b32.s1
                                                            }
                                                            hb = s;
                                                            hn = j;
                                                            if MEM[s as usize].b16.s0 as i32 & 1
                                                                != 0
                                                            {
                                                                hyf_bchar = FONT_BCHAR[hf as usize]
                                                            } else {
                                                                hyf_bchar = 65536i32
                                                            }
                                                        /*:932*/
                                                        } else {
                                                            if !(MEM[s as usize].b16.s1 as i32
                                                                == 11i32
                                                                && MEM[s as usize].b16.s0 as i32
                                                                    == 0i32)
                                                            {
                                                                break;
                                                            }
                                                            hb = s;
                                                            hyf_bchar = FONT_BCHAR[hf as usize]
                                                        }
                                                        s = MEM[s as usize].b32.s1
                                                    }
                                                    current_block = 4362442400146949691;
                                                }
                                                match current_block {
                                                    8166967358843938227 => {}
                                                    _ =>
                                                    /*933: check that the nodes following hb permit
                                                     * hyphenation and that at least l_hyf + r_hyf letters
                                                     * have been found, otherwise goto done1 */
                                                    {
                                                        if !((hn as i32) < l_hyf + r_hyf) {
                                                            loop {
                                                                if !is_char_node(s) {
                                                                    match *NODE_type(s as isize) {
                                                                        LIGATURE_NODE => {}
                                                                        KERN_NODE => {
                                                                            current_block =
                                                                                5935670669791948619;
                                                                            if *NODE_subtype(
                                                                                s as isize,
                                                                            ) != NORMAL as _
                                                                            {
                                                                                current_block
                                                                                            =
                                                                                            16848571710846909653;
                                                                                break;
                                                                            }
                                                                        }
                                                                        8 | 10 | 12 | 3 | 5 | 4 => {
                                                                            current_block
                                                                                =
                                                                                16848571710846909653;
                                                                            break;
                                                                        }
                                                                        9 => {
                                                                            current_block =
                                                                                2529459302156174429;
                                                                            if MEM[s as usize]
                                                                                .b16
                                                                                .s0
                                                                                as i32
                                                                                >= 4i32
                                                                            {
                                                                                current_block
                                                                                            =
                                                                                            16848571710846909653;
                                                                                break;
                                                                            } else {
                                                                                current_block
                                                                                            =
                                                                                            8166967358843938227;
                                                                                break;
                                                                            }
                                                                        }
                                                                        _ => {
                                                                            current_block =
                                                                                8166967358843938227;
                                                                            break;
                                                                        }
                                                                    }
                                                                }
                                                                s = MEM[s as usize].b32.s1
                                                            }
                                                            match current_block {
                                                                8166967358843938227 => {}
                                                                _ => {
                                                                    /*:933*/
                                                                    hyphenate();
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                11 => {
                    /* ... resuming 895 ... */
                    if MEM[cur_p as usize].b16.s0 as i32 == 1 {
                        if (!is_char_node(MEM[cur_p as usize].b32.s1) as i32) < hi_mem_min
                            && auto_breaking as i32 != 0
                        {
                            if MEM[MEM[cur_p as usize].b32.s1 as usize].b16.s1 as i32 == 10 {
                                try_break(0i32, 0i32 as small_number);
                            }
                        }
                        active_width[1] += MEM[(cur_p + 1) as usize].b32.s1
                    } else {
                        active_width[1] += MEM[(cur_p + 1) as usize].b32.s1
                    }
                }
                6 => {
                    f = MEM[(cur_p + 1) as usize].b16.s1 as internal_font_number;
                    xtx_ligature_present = true;
                    active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                        + FONT_INFO[(CHAR_BASE[f as usize]
                            + effective_char(true, f, MEM[(cur_p + 1) as usize].b16.s0))
                            as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1
                }
                DISC_NODE => {
                    /*898: try to break after a discretionary fragment, then goto done5 */
                    s = MEM[(cur_p + 1) as usize].b32.s0;
                    disc_width = 0;

                    if s == TEX_NULL {
                        try_break(INTPAR(INT_PAR__ex_hyphen_penalty), HYPHENATED as _);
                    } else {
                        loop {
                            /*899:*/
                            if is_char_node(s) {
                                let mut eff_char_0: i32 = 0; /*:898 big DISC_NODE case */
                                f = MEM[s as usize].b16.s1 as internal_font_number;
                                eff_char_0 = effective_char(1i32 != 0, f, MEM[s as usize].b16.s0);
                                disc_width += FONT_INFO[(WIDTH_BASE[f as usize]
                                    + FONT_INFO[(CHAR_BASE[f as usize] + eff_char_0) as usize]
                                        .b16
                                        .s3 as i32)
                                    as usize]
                                    .b32
                                    .s1
                            } else {
                                match MEM[s as usize].b16.s1 as i32 {
                                    6 => {
                                        let mut eff_char_1: i32 = 0;
                                        f = MEM[(s + 1) as usize].b16.s1 as internal_font_number;
                                        xtx_ligature_present = true;
                                        eff_char_1 =
                                            effective_char(true, f, MEM[(s + 1) as usize].b16.s0);
                                        disc_width += FONT_INFO[(WIDTH_BASE[f as usize]
                                            + FONT_INFO
                                                [(CHAR_BASE[f as usize] + eff_char_1) as usize]
                                                .b16
                                                .s3
                                                as i32)
                                            as usize]
                                            .b32
                                            .s1
                                    }
                                    0 | 1 | 2 | 11 => disc_width += MEM[(s + 1) as usize].b32.s1,
                                    8 => {
                                        if MEM[s as usize].b16.s0 as i32 == 40
                                            || MEM[s as usize].b16.s0 as i32 == 41
                                            || MEM[s as usize].b16.s0 as i32 == 42
                                            || MEM[s as usize].b16.s0 as i32 == 43
                                            || MEM[s as usize].b16.s0 as i32 == 44
                                        {
                                            disc_width += MEM[(s + 1) as usize].b32.s1
                                        } else {
                                            confusion(b"disc3a");
                                        }
                                    }
                                    _ => {
                                        confusion(b"disc3");
                                    }
                                }
                            }
                            s = MEM[s as usize].b32.s1;
                            if !(s != -0xfffffffi32) {
                                break;
                            }
                        }
                        active_width[1] += disc_width;
                        try_break(INTPAR(INT_PAR__hyphen_penalty), HYPHENATED as _);
                        active_width[1] -= disc_width
                    }
                    r = MEM[cur_p as usize].b16.s0 as i32;
                    s = MEM[cur_p as usize].b32.s1;
                    while r > 0i32 {
                        if is_char_node(s) {
                            let mut eff_char_2: i32 = 0;
                            f = MEM[s as usize].b16.s1 as internal_font_number;
                            eff_char_2 = effective_char(1i32 != 0, f, MEM[s as usize].b16.s0);
                            active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                                + FONT_INFO[(CHAR_BASE[f as usize] + eff_char_2) as usize]
                                    .b16
                                    .s3 as i32)
                                as usize]
                                .b32
                                .s1
                        } else {
                            match MEM[s as usize].b16.s1 as i32 {
                                6 => {
                                    let mut eff_char_3: i32 = 0;
                                    f = MEM[(s + 1) as usize].b16.s1 as internal_font_number;
                                    xtx_ligature_present = true;
                                    eff_char_3 =
                                        effective_char(true, f, MEM[(s + 1) as usize].b16.s0);
                                    active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                                        + FONT_INFO[(CHAR_BASE[f as usize] + eff_char_3) as usize]
                                            .b16
                                            .s3 as i32)
                                        as usize]
                                        .b32
                                        .s1
                                }
                                0 | 1 | 2 | 11 => active_width[1] += MEM[(s + 1) as usize].b32.s1,
                                8 => {
                                    if MEM[s as usize].b16.s0 as i32 == 40
                                        || MEM[s as usize].b16.s0 as i32 == 41
                                        || MEM[s as usize].b16.s0 as i32 == 42
                                        || MEM[s as usize].b16.s0 as i32 == 43
                                        || MEM[s as usize].b16.s0 as i32 == 44
                                    {
                                        active_width[1] += MEM[(s + 1) as usize].b32.s1
                                    } else {
                                        confusion(b"disc4a");
                                    }
                                }
                                _ => {
                                    confusion(b"disc4");
                                }
                            }
                        }
                        r -= 1;
                        s = MEM[s as usize].b32.s1
                    }
                    global_prev_p = cur_p;
                    prev_p = global_prev_p;
                    cur_p = s;
                    continue;
                }
                9 => {
                    if (MEM[cur_p as usize].b16.s0 as i32) < 4 {
                        auto_breaking = MEM[cur_p as usize].b16.s0 as i32 & 1 != 0
                    }
                    if !is_char_node(MEM[cur_p as usize].b32.s1) && auto_breaking as i32 != 0 {
                        if MEM[MEM[cur_p as usize].b32.s1 as usize].b16.s1 as i32 == 10 {
                            try_break(0i32, 0i32 as small_number);
                        }
                    }
                    active_width[1] += MEM[(cur_p + 1) as usize].b32.s1
                }
                12 => {
                    try_break(MEM[(cur_p + 1) as usize].b32.s1, 0 as small_number);
                }
                4 | 3 | 5 => {}
                _ => {
                    confusion(b"paragraph");
                }
            }
            global_prev_p = cur_p;
            prev_p = global_prev_p;
            cur_p = MEM[cur_p as usize].b32.s1
        }
        if cur_p == TEX_NULL {
            /*902: "Try the final line break at the end of the paragraph, and
             * goto done if the desired breakpoints have been found." */
            try_break(EJECT_PENALTY, HYPHENATED as _);
            if *LLIST_link(ACTIVE_LIST as isize) != LAST_ACTIVE {
                /*903:*/
                r = *LLIST_link(ACTIVE_LIST as isize);
                fewest_demerits = MAX_HALFWORD;
                loop {
                    if *NODE_type(r as isize) != DELTA_NODE {
                        if MEM[(r + 2) as usize].b32.s1 < fewest_demerits {
                            fewest_demerits = MEM[(r + 2) as usize].b32.s1; /*:904*/
                            best_bet = r
                        }
                    }
                    r = *LLIST_link(r as isize);
                    if !(r != LAST_ACTIVE) {
                        break;
                    }
                }
                best_line = MEM[(best_bet + 1) as usize].b32.s0;
                if INTPAR(INT_PAR__looseness) == 0 {
                    break;
                }

                r = *LLIST_link(ACTIVE_LIST as isize); /*904:*/
                actual_looseness = 0;

                loop {
                    if MEM[r as usize].b16.s1 as i32 != 2 {
                        line_diff = MEM[(r + 1) as usize].b32.s0 - best_line;
                        if line_diff < actual_looseness && INTPAR(INT_PAR__looseness) <= line_diff
                            || line_diff > actual_looseness
                                && INTPAR(INT_PAR__looseness) >= line_diff
                        {
                            best_bet = r;
                            actual_looseness = line_diff;
                            fewest_demerits = MEM[(r + 2) as usize].b32.s1
                        } else if line_diff == actual_looseness
                            && MEM[(r + 2) as usize].b32.s1 < fewest_demerits
                        {
                            best_bet = r;
                            fewest_demerits = MEM[(r + 2) as usize].b32.s1
                        }
                    }
                    r = MEM[r as usize].b32.s1;
                    if !(r != 4999999i32 - 7i32) {
                        break;
                    }
                }
                best_line = MEM[(best_bet + 1) as usize].b32.s0;
                if actual_looseness == INTPAR(INT_PAR__looseness) || final_pass {
                    break;
                }
            }
        }
        /*894: clean up the memory by removing the break nodes */
        q = MEM[(4999999 - 7) as usize].b32.s1;
        while q != 4999999i32 - 7i32 {
            cur_p = MEM[q as usize].b32.s1;
            if MEM[q as usize].b16.s1 as i32 == 2 {
                free_node(q, 7i32);
            } else {
                free_node(q, active_node_size as i32);
            }
            q = cur_p
        }
        q = passive;
        while q != -0xfffffffi32 {
            cur_p = MEM[q as usize].b32.s1;
            free_node(q, 2i32);
            q = cur_p
        }
        /* ... resuming 892 ... */
        if !second_pass {
            threshold = INTPAR(INT_PAR__tolerance);
            second_pass = true;
            final_pass = DIMENPAR(DIMEN_PAR__emergency_stretch) <= 0;
        } else {
            background[2] = background[2] + DIMENPAR(DIMEN_PAR__emergency_stretch);
            final_pass = true;
        }
    }
    if do_last_line_fit {
        /*1641:*/
        if MEM[(best_bet + 3) as usize].b32.s1 == 0 {
            do_last_line_fit = false
        } else {
            q = new_spec(MEM[(last_line_fill + 1) as usize].b32.s0);
            delete_glue_ref(MEM[(last_line_fill + 1) as usize].b32.s0);
            let ref mut fresh4 = MEM[(q + 1) as usize].b32.s1;
            *fresh4 += MEM[(best_bet + 3) as usize].b32.s1 - MEM[(best_bet + 4) as usize].b32.s1;
            MEM[(q + 2) as usize].b32.s1 = 0;
            MEM[(last_line_fill + 1) as usize].b32.s0 = q
        }
    }
    post_line_break(d);
    /* Clean up by removing break nodes (894, again) */
    q = MEM[(4999999 - 7) as usize].b32.s1;
    while q != 4999999i32 - 7i32 {
        let mut next: i32 = MEM[q as usize].b32.s1;
        if MEM[q as usize].b16.s1 as i32 == 2 {
            free_node(q, 7i32);
        } else {
            free_node(q, active_node_size as i32);
        }
        q = next
    }
    q = passive;
    while q != -0xfffffffi32 {
        let mut next_0: i32 = MEM[q as usize].b32.s1;
        free_node(q, 2i32);
        q = next_0
    }
    /* All done */
    pack_begin_line = 0i32;
}
/* This was just separated out to prevent line_break() from becoming
 * proposterously long. */
unsafe extern "C" fn post_line_break(mut d: bool) {
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut p: i32 = 0;
    let mut k: i32 = 0;
    let mut w: scaled_t = 0;
    let mut glue_break: bool = false;
    let mut ptmp: i32 = 0;
    let mut disc_break: bool = false;
    let mut post_disc_break: bool = false;
    let mut cur_width: scaled_t = 0;
    let mut cur_indent: scaled_t = 0;
    let mut t: u16 = 0;
    let mut pen: i32 = 0;
    let mut cur_line: i32 = 0;
    let mut LR_ptr: i32 = 0;
    LR_ptr = cur_list.eTeX_aux;
    /* Reverse the list of break nodes (907) */
    q = MEM[(best_bet + 1) as usize].b32.s1; /*:907*/
    cur_p = -0xfffffffi32;
    loop {
        r = q;
        q = MEM[(q + 1) as usize].b32.s0;
        MEM[(r + 1) as usize].b32.s0 = cur_p;
        cur_p = r;
        if !(q != -0xfffffffi32) {
            break;
        }
    }
    cur_line = cur_list.prev_graf + 1i32;
    loop {
        /* 909: justify the line ending at breakpoint cur_p and append it to
         * the current vertical list, with associated penalties and
         * insertions. The current line starts a TEMP_HEAD.link and ends at
         * cur_p.cur_break.
         **/
        if INTPAR(INT_PAR__texxet) > 0 {
            /*1494:*/
            q = MEM[TEMP_HEAD as usize].b32.s1;
            if LR_ptr != TEX_NULL {
                temp_ptr = LR_ptr;
                r = q;
                loop {
                    s = new_math(0i32, (MEM[temp_ptr as usize].b32.s0 - 1i32) as small_number);
                    MEM[s as usize].b32.s1 = r;
                    r = s;
                    temp_ptr = MEM[temp_ptr as usize].b32.s1;
                    if !(temp_ptr != -0xfffffffi32) {
                        break;
                    }
                }
                MEM[(4999999 - 3) as usize].b32.s1 = r
            }
            while q != MEM[(cur_p + 1) as usize].b32.s1 {
                if q < hi_mem_min && MEM[q as usize].b16.s1 as i32 == 9 {
                    /*1495:*/
                    if MEM[q as usize].b16.s0 as i32 & 1 != 0 {
                        if LR_ptr != -0xfffffffi32
                            && MEM[LR_ptr as usize].b32.s0
                                == 4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32
                        {
                            temp_ptr = LR_ptr;
                            LR_ptr = MEM[temp_ptr as usize].b32.s1;
                            MEM[temp_ptr as usize].b32.s1 = avail;
                            avail = temp_ptr
                        }
                    } else {
                        temp_ptr = get_avail();
                        MEM[temp_ptr as usize].b32.s0 =
                            4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32;
                        MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                        LR_ptr = temp_ptr
                    }
                }
                q = MEM[q as usize].b32.s1
            }
        }
        /* 910: "Modify the end of the line to reflect the nature of the break
         * and to include \rightskip; also set the proper value of
         * disc_break" */
        q = MEM[(cur_p + 1) as usize].b32.s1;
        disc_break = false;
        post_disc_break = false;
        glue_break = false;

        if q == TEX_NULL {
            q = TEMP_HEAD;
            while *LLIST_link(q as isize) != TEX_NULL {
                q = *LLIST_link(q as isize);
            }
        } else if MEM[q as usize].b16.s1 as i32 == 10 {
            delete_glue_ref(MEM[(q + 1) as usize].b32.s0);
            *GLUE_NODE_glue_ptr(q as isize) = *GLUEPAR(GLUE_PAR__right_skip);
            *NODE_subtype(q as isize) = GLUE_PAR__right_skip as u16 + 1;
            let ref mut fresh5 = MEM[EQTB[(1
                + (0x10ffff + 1)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 8i32) as usize]
                .b32
                .s1 as usize]
                .b32
                .s1;
            *fresh5 += 1;
            glue_break = true;
        } else if MEM[q as usize].b16.s1 as i32 == 7 {
            /*911:*/
            t = MEM[q as usize].b16.s0;
            if t as i32 == 0i32 {
                r = MEM[q as usize].b32.s1
            } else {
                r = q;
                while t as i32 > 1i32 {
                    r = MEM[r as usize].b32.s1;
                    t = t.wrapping_sub(1)
                }
                s = MEM[r as usize].b32.s1;
                r = MEM[s as usize].b32.s1;
                MEM[s as usize].b32.s1 = -0xfffffff;
                flush_node_list(MEM[q as usize].b32.s1);
                MEM[q as usize].b16.s0 = 0_u16
            }
            if MEM[(q + 1) as usize].b32.s1 != -0xfffffff {
                /*913:*/
                s = MEM[(q + 1) as usize].b32.s1;
                while MEM[s as usize].b32.s1 != -0xfffffff {
                    s = MEM[s as usize].b32.s1
                }
                MEM[s as usize].b32.s1 = r;
                r = MEM[(q + 1) as usize].b32.s1;
                MEM[(q + 1) as usize].b32.s1 = -0xfffffff;
                post_disc_break = 1i32 != 0
            }
            if MEM[(q + 1) as usize].b32.s0 != -0xfffffff {
                /*914:*/
                s = MEM[(q + 1) as usize].b32.s0;
                MEM[q as usize].b32.s1 = s;
                while MEM[s as usize].b32.s1 != -0xfffffff {
                    s = MEM[s as usize].b32.s1
                }
                MEM[(q + 1) as usize].b32.s0 = -0xfffffff;
                q = s
            }
            MEM[q as usize].b32.s1 = r;
            disc_break = 1i32 != 0
        } else if MEM[q as usize].b16.s1 as i32 == 11 {
            MEM[(q + 1) as usize].b32.s1 = 0
        } else if MEM[q as usize].b16.s1 as i32 == 9 {
            MEM[(q + 1) as usize].b32.s1 = 0;
            if INTPAR(INT_PAR__texxet) > 0 {
                /*1495:*/
                if MEM[q as usize].b16.s0 as i32 & 1 != 0 {
                    if LR_ptr != TEX_NULL
                        && MEM[LR_ptr as usize].b32.s0
                            == 4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32
                    {
                        temp_ptr = LR_ptr;
                        LR_ptr = MEM[temp_ptr as usize].b32.s1;
                        MEM[temp_ptr as usize].b32.s1 = avail;
                        avail = temp_ptr
                    }
                } else {
                    temp_ptr = get_avail();
                    MEM[temp_ptr as usize].b32.s0 =
                        4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32;
                    MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                    LR_ptr = temp_ptr
                }
            }
        }
        /* "at this point q is the rightmost breakpoint; the only exception is
         * the case of a discretionary break with non-empty pre_break -- then
         * q has been changed to the last node of the pre-break list" */
        if INTPAR(INT_PAR__xetex_protrude_chars) > 0 {
            if disc_break as i32 != 0
                && (is_char_node(q) as i32 != 0 || MEM[q as usize].b16.s1 as i32 != 7i32)
            {
                p = q; /*:915*/
                ptmp = p
            } else {
                p = prev_rightmost(MEM[(4999999 - 3) as usize].b32.s1, q);
                ptmp = p;
                p = find_protchar_right(MEM[(4999999 - 3) as usize].b32.s1, p)
            }
            w = char_pw(p, 1i32 as small_number);
            if w != 0i32 {
                k = new_margin_kern(-w, last_rightmost_char, 1i32 as small_number);
                MEM[k as usize].b32.s1 = MEM[ptmp as usize].b32.s1;
                MEM[ptmp as usize].b32.s1 = k;
                if ptmp == q {
                    q = MEM[q as usize].b32.s1
                }
            }
        }
        if !glue_break {
            r = new_param_glue(8i32 as small_number);
            MEM[r as usize].b32.s1 = MEM[q as usize].b32.s1;
            MEM[q as usize].b32.s1 = r;
            q = r
        }
        if INTPAR(INT_PAR__texxet) > 0 {
            /*1496:*/
            if LR_ptr != TEX_NULL {
                s = TEMP_HEAD;
                r = MEM[s as usize].b32.s1;
                while r != q {
                    s = r;
                    r = MEM[s as usize].b32.s1
                }
                r = LR_ptr;
                while r != -0xfffffffi32 {
                    temp_ptr = new_math(0i32, MEM[r as usize].b32.s0 as small_number);
                    MEM[s as usize].b32.s1 = temp_ptr;
                    s = temp_ptr;
                    r = MEM[r as usize].b32.s1
                }
                MEM[s as usize].b32.s1 = q
            }
        }
        /* 916: Put \leftskip at the left and detach this line. */
        r = MEM[q as usize].b32.s1;
        MEM[q as usize].b32.s1 = -0xfffffff;
        q = MEM[(4999999 - 3) as usize].b32.s1;
        MEM[(4999999 - 3) as usize].b32.s1 = r;
        /* "at this point q is the leftmost node; all discardable nodes have been discarded */
        
        if INTPAR(INT_PAR__xetex_protrude_chars) > 0 {
            p = q;
            p = find_protchar_left(p, false);
            w = char_pw(p, 0i32 as small_number);
            if w != 0i32 {
                k = new_margin_kern(-w, last_leftmost_char, 0i32 as small_number);
                MEM[k as usize].b32.s1 = q;
                q = k
            }
        }
        if *GLUEPAR(GLUE_PAR__left_skip) != 0 {
            r = new_param_glue(7i32 as small_number);
            MEM[r as usize].b32.s1 = q;
            q = r
        }
        /* 918: q points to the hlist that represents the current line. Pack
         * it up at the right width. */
        if cur_line > last_special_line {
            cur_width = second_width;
            cur_indent = second_indent
        } else if LOCAL(LOCAL__par_shape) == TEX_NULL {
            cur_width = first_width;
            cur_indent = first_indent
        } else {
            /* These manual `mem` indices are in the original WEB code */
            cur_width = MEM[LOCAL(LOCAL__par_shape) as usize + 2 * cur_line as usize]
                .b32
                .s1;
            cur_indent = MEM[LOCAL(LOCAL__par_shape) as usize + 2 * cur_line as usize - 1]
                .b32
                .s1;
        }
        adjust_tail = ADJUST_HEAD;
        pre_adjust_tail = PRE_ADJUST_HEAD;
        /* Tectonic: in semantic pagination mode, set each "line" (really the
         * whole paragraph) at its natural width. */
        if semantic_pagination_enabled {
            just_box = hpack(q, 0i32, 1i32 as small_number)
        } else {
            just_box = hpack(q, cur_width, 0i32 as small_number)
        } /*:918*/
        MEM[(just_box + 4) as usize].b32.s1 = cur_indent;
        /* 917: append the new box to the current vertical list, followed
         * by any of its special nodes that were taken out */
        if 4999999i32 - 14i32 != pre_adjust_tail {
            MEM[cur_list.tail as usize].b32.s1 = MEM[(4999999 - 14) as usize].b32.s1; /*:917*/
            cur_list.tail = pre_adjust_tail
        }
        pre_adjust_tail = -0xfffffffi32;
        append_to_vlist(just_box);
        if 4999999i32 - 5i32 != adjust_tail {
            MEM[cur_list.tail as usize].b32.s1 = MEM[(4999999 - 5) as usize].b32.s1;
            cur_list.tail = adjust_tail
        }
        adjust_tail = -0xfffffffi32;
        /* 919: Set `pen` to all of the penalties relevant to this line. */
        if cur_line + 1i32 != best_line {
            q = EQTB[(ETEX_PEN_BASE + 0i32) as usize].b32.s1;
            if q != -0xfffffffi32 {
                r = cur_line;
                if r > MEM[(q + 1) as usize].b32.s1 {
                    r = MEM[(q + 1) as usize].b32.s1
                }
                pen = MEM[(q + r + 1) as usize].b32.s1
            } else {
                pen = EQTB[(INT_BASE + 13i32) as usize].b32.s1
            }
            q = EQTB[(ETEX_PEN_BASE + 1i32) as usize].b32.s1;
            if q != -0xfffffffi32 {
                r = cur_line - cur_list.prev_graf;
                if r > MEM[(q + 1) as usize].b32.s1 {
                    r = MEM[(q + 1) as usize].b32.s1
                }
                pen += MEM[(q + r + 1) as usize].b32.s1
            } else if cur_line == cur_list.prev_graf + 1i32 {
                pen += EQTB[(INT_BASE + 5i32) as usize].b32.s1
            }
            if d {
                q = EQTB[(ETEX_PEN_BASE + 3i32) as usize].b32.s1
            } else {
                q = EQTB[(ETEX_PEN_BASE + 2i32) as usize].b32.s1
            }
            if q != -0xfffffffi32 {
                r = best_line - cur_line - 1i32;
                if r > MEM[(q + 1) as usize].b32.s1 {
                    r = MEM[(q + 1) as usize].b32.s1
                }
                pen += MEM[(q + r + 1) as usize].b32.s1
            } else if cur_line + 2i32 == best_line {
                if d {
                    pen += EQTB[(INT_BASE + 7i32) as usize].b32.s1
                } else {
                    pen += EQTB[(INT_BASE + 6i32) as usize].b32.s1
                }
            }
            if disc_break {
                pen += EQTB[(INT_BASE + 8i32) as usize].b32.s1
            }
            if pen != 0i32 {
                r = new_penalty(pen);
                MEM[cur_list.tail as usize].b32.s1 = r;
                cur_list.tail = r
            }
        }
        /* Done justifying this line. */
        cur_line += 1;
        cur_p = MEM[(cur_p + 1) as usize].b32.s0;
        if cur_p != -0xfffffffi32 {
            if !post_disc_break {
                /* 908: "prune unwanted nodes at the beginning of the next
                 * line". Delete glues, penalties, kerns, and math nodes at
                 * the beginning of the line, unless the node in question is
                 * the chosen breakpoint. */
                r = 4999999i32 - 3i32;
                loop {
                    q = MEM[r as usize].b32.s1;
                    if q == MEM[(cur_p + 1) as usize].b32.s1 {
                        break;
                    }
                    if is_char_node(q) {
                        break;
                    }
                    if is_non_discardable_node(q) {
                        break;
                    }
                    if MEM[q as usize].b16.s1 as i32 == 11i32
                        && MEM[q as usize].b16.s0 as i32 != 1i32
                        && MEM[q as usize].b16.s0 as i32 != 3i32
                    {
                        break;
                    }
                    r = q;
                    if MEM[q as usize].b16.s1 as i32 == 9
                        && EQTB[(INT_BASE + 71i32) as usize].b32.s1 > 0i32
                    {
                        /*1495:*/
                        if MEM[q as usize].b16.s0 as i32 & 1i32 != 0 {
                            if LR_ptr != -0xfffffffi32
                                && MEM[LR_ptr as usize].b32.s0
                                    == 4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32
                            {
                                temp_ptr = LR_ptr;
                                LR_ptr = MEM[temp_ptr as usize].b32.s1;
                                MEM[temp_ptr as usize].b32.s1 = avail;
                                avail = temp_ptr
                            }
                        } else {
                            temp_ptr = get_avail();
                            MEM[temp_ptr as usize].b32.s0 =
                                4i32 * (MEM[q as usize].b16.s0 as i32 / 4i32) + 3i32;
                            MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                            LR_ptr = temp_ptr
                        }
                    }
                }
                if r != 4999999i32 - 3i32 {
                    MEM[r as usize].b32.s1 = -0xfffffff;
                    flush_node_list(MEM[(4999999 - 3) as usize].b32.s1);
                    MEM[(4999999 - 3) as usize].b32.s1 = q
                }
            }
        }
        if !(cur_p != -0xfffffffi32) {
            break;
        }
    }
    if cur_line != best_line || MEM[(4999999 - 3) as usize].b32.s1 != -0xfffffff {
        confusion(b"line breaking");
    }
    cur_list.prev_graf = best_line - 1i32;
    cur_list.eTeX_aux = LR_ptr;
}
/*858: "The heart of the line-breaking procedure is try_break, a subroutine
 * that tests if the current breakpoint cur_p is feasible, by running through
 * the active list to see what lines of text can be made from active nodes to
 * cur_p. If feasible breaks are possible, new break nodes are created. If
 * cur_p is too far from an active node, that node is deactivated. The
 * parameter pi to try_break is the penalty associated with a break at cur_p;
 * we have pi = eject_penalty if the break is forced, and pi = inf_penalty if
 * the break is illegal. The other parameter, break_type, is set to HYPHENATED
 * or UNHYPHENATED, depending on whether or not the current break is at a
 * disc_node. The end of a paragraph is also regarded as hyphenated; this case
 * is distinguishable by the condition cur_p = null." */
unsafe extern "C" fn try_break(mut pi: i32, mut break_type: small_number) {
    let mut current_block: u64;
    let mut r: i32 = 0;
    let mut prev_r: i32 = 0;
    let mut old_l: i32 = 0;
    let mut no_break_yet: bool = false;
    let mut prev_prev_r: i32 = -0xfffffffi32;
    let mut s: i32 = 0;
    let mut q: i32 = 0;
    let mut v: i32 = 0;
    let mut t: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut l: i32 = 0;
    let mut node_r_stays_active: bool = false;
    let mut line_width: scaled_t = 0i32;
    let mut fit_class: u8 = 0;
    let mut b: i32 = 0;
    let mut d: i32 = 0;
    let mut artificial_demerits: bool = false;
    let mut shortfall: scaled_t = 0;
    let mut g: scaled_t = 0i32;
    /* Tectonic: no-op except at the end of the paragraph. We know we're at
     * the very end of the paragraph when cur_p is TEX_NULL. */
    if semantic_pagination_enabled as i32 != 0 && cur_p != -0xfffffffi32 {
        return;
    }
    if pi.abs() >= 10000i32 {
        if pi > 0i32 {
            return;
        }
        pi = -10000i32
    }
    no_break_yet = 1i32 != 0;
    prev_r = 4999999i32 - 7i32;
    old_l = 0i32;
    cur_active_width[1] = active_width[1];
    cur_active_width[2] = active_width[2];
    cur_active_width[3] = active_width[3];
    cur_active_width[4] = active_width[4];
    cur_active_width[5] = active_width[5];
    cur_active_width[6] = active_width[6];
    loop {
        r = MEM[prev_r as usize].b32.s1;
        /*861: "If node r is of type delta_node, update cur_active_width, set
         * prev_r and prev_prev_r, then goto continue" */
        if MEM[r as usize].b16.s1 as i32 == 2 {
            cur_active_width[1] += MEM[(r + 1) as usize].b32.s1;
            cur_active_width[2] += MEM[(r + 2) as usize].b32.s1;
            cur_active_width[3] += MEM[(r + 3) as usize].b32.s1;
            cur_active_width[4] += MEM[(r + 4) as usize].b32.s1;
            cur_active_width[5] += MEM[(r + 5) as usize].b32.s1;
            cur_active_width[6] += MEM[(r + 6) as usize].b32.s1;
            prev_prev_r = prev_r;
            prev_r = r
        } else {
            /*864: "If a line number class has ended, create new active nodes for
             * the best feasible breaks in that class; then return if r =
             * last_active, otherwise compute the new line_width." */
            l = MEM[(r + 1) as usize].b32.s0;
            if l > old_l {
                /* "now we are no longer in the inner loop" */
                if minimum_demerits < 0x3fffffffi32
                    && (old_l != easy_line || r == 4999999i32 - 7i32)
                {
                    /*865: "Create new active nodes for the best feasible breaks
                     * just found." */
                    if no_break_yet {
                        /*866: "Compute the values of break_width". */
                        no_break_yet = false;
                        break_width[1] = background[1];
                        break_width[2] = background[2];
                        break_width[3] = background[3];
                        break_width[4] = background[4];
                        break_width[5] = background[5];
                        break_width[6] = background[6];
                        s = cur_p;
                        if break_type as i32 > 0i32 {
                            /*869: "Compute the discretionary break_width values" */
                            if cur_p != -0xfffffffi32 {
                                t = MEM[cur_p as usize].b16.s0 as i32;
                                v = cur_p;
                                s = MEM[(cur_p + 1) as usize].b32.s1;
                                while t > 0i32 {
                                    t -= 1;
                                    v = MEM[v as usize].b32.s1;
                                    /*870: "subtract the width of node v from break_width" */
                                    if is_char_node(v) {
                                        let mut eff_char: i32 = 0;
                                        f = MEM[v as usize].b16.s1 as internal_font_number;
                                        eff_char =
                                            effective_char(1i32 != 0, f, MEM[v as usize].b16.s0);
                                        break_width[1] -= FONT_INFO[(WIDTH_BASE[f as usize]
                                            + FONT_INFO[(CHAR_BASE[f as usize] + eff_char) as usize]
                                                .b16
                                                .s3
                                                as i32)
                                            as usize]
                                            .b32
                                            .s1
                                    } else {
                                        match MEM[v as usize].b16.s1 as i32 {
                                            6 => {
                                                let mut eff_char_0: i32 = 0;
                                                f = MEM[(v + 1) as usize].b16.s1
                                                    as internal_font_number;
                                                xtx_ligature_present = 1i32 != 0;
                                                eff_char_0 = effective_char(
                                                    1i32 != 0,
                                                    f,
                                                    MEM[(v + 1) as usize].b16.s0,
                                                );
                                                break_width[1] -= FONT_INFO[(WIDTH_BASE[f as usize]
                                                    + FONT_INFO[(CHAR_BASE[f as usize] + eff_char_0)
                                                        as usize]
                                                        .b16
                                                        .s3
                                                        as i32)
                                                    as usize]
                                                    .b32
                                                    .s1
                                            }
                                            0 | 1 | 2 | 11 => {
                                                break_width[1] -= MEM[(v + 1) as usize].b32.s1
                                            }
                                            8 => {
                                                if MEM[v as usize].b16.s0 as i32 == 40
                                                    || MEM[v as usize].b16.s0 as i32 == 41
                                                    || MEM[v as usize].b16.s0 as i32 == 42
                                                    || MEM[v as usize].b16.s0 as i32 == 43
                                                    || MEM[v as usize].b16.s0 as i32 == 44
                                                {
                                                    break_width[1] -= MEM[(v + 1) as usize].b32.s1
                                                } else {
                                                    confusion(b"disc1a");
                                                }
                                            }
                                            _ => {
                                                confusion(b"disc1");
                                            }
                                        }
                                    }
                                }
                                /*871: "add the width of node s to break_width" */
                                while s != -0xfffffffi32 {
                                    if is_char_node(s) {
                                        let mut eff_char_1: i32 = 0;
                                        f = MEM[s as usize].b16.s1 as internal_font_number;
                                        eff_char_1 =
                                            effective_char(1i32 != 0, f, MEM[s as usize].b16.s0);
                                        break_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                                            + FONT_INFO
                                                [(CHAR_BASE[f as usize] + eff_char_1) as usize]
                                                .b16
                                                .s3
                                                as i32)
                                            as usize]
                                            .b32
                                            .s1
                                    } else {
                                        match MEM[s as usize].b16.s1 as i32 {
                                            6 => {
                                                let mut eff_char_2: i32 = 0;
                                                f = MEM[(s + 1) as usize].b16.s1
                                                    as internal_font_number;
                                                xtx_ligature_present = 1i32 != 0;
                                                eff_char_2 = effective_char(
                                                    1i32 != 0,
                                                    f,
                                                    MEM[(s + 1) as usize].b16.s0,
                                                );
                                                break_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                                                    + FONT_INFO[(CHAR_BASE[f as usize] + eff_char_2)
                                                        as usize]
                                                        .b16
                                                        .s3
                                                        as i32)
                                                    as usize]
                                                    .b32
                                                    .s1
                                            }
                                            0 | 1 | 2 | 11 => {
                                                break_width[1] += MEM[(s + 1) as usize].b32.s1
                                            }
                                            8 => {
                                                if MEM[s as usize].b16.s0 as i32 == 40
                                                    || MEM[s as usize].b16.s0 as i32 == 41
                                                    || MEM[s as usize].b16.s0 as i32 == 42
                                                    || MEM[s as usize].b16.s0 as i32 == 43
                                                    || MEM[s as usize].b16.s0 as i32 == 44
                                                {
                                                    break_width[1] += MEM[(s + 1) as usize].b32.s1
                                                } else {
                                                    confusion(b"disc2a");
                                                }
                                            }
                                            _ => {
                                                confusion(b"disc2");
                                            }
                                        }
                                    }
                                    s = MEM[s as usize].b32.s1
                                }
                                break_width[1] += disc_width;
                                if MEM[(cur_p + 1) as usize].b32.s1 == -0xfffffff {
                                    s = MEM[v as usize].b32.s1
                                }
                            }
                        }
                        while s != -0xfffffffi32 {
                            if is_char_node(s) {
                                break;
                            }
                            match MEM[s as usize].b16.s1 as i32 {
                                10 => {
                                    v = MEM[(s + 1) as usize].b32.s0;
                                    break_width[1] -= MEM[(v + 1) as usize].b32.s1;
                                    break_width[(2i32 + MEM[v as usize].b16.s1 as i32) as usize] -=
                                        MEM[(v + 2) as usize].b32.s1;
                                    break_width[6] -= MEM[(v + 3) as usize].b32.s1
                                }
                                12 => {}
                                9 => break_width[1] -= MEM[(s + 1) as usize].b32.s1,
                                11 => {
                                    if MEM[s as usize].b16.s0 as i32 != 1 {
                                        break;
                                    }
                                    break_width[1] -= MEM[(s + 1) as usize].b32.s1
                                }
                                _ => {
                                    break;
                                }
                            }
                            s = MEM[s as usize].b32.s1
                        }
                    }
                    /*872: "Insert a delta node to prepare for breaks at cur_p" */
                    if MEM[prev_r as usize].b16.s1 as i32 == 2 {
                        let ref mut fresh6 = MEM[(prev_r + 1) as usize].b32.s1; /* this is unused */
                        *fresh6 += -cur_active_width[1] + break_width[1];
                        let ref mut fresh7 = MEM[(prev_r + 2) as usize].b32.s1;
                        *fresh7 += -cur_active_width[2] + break_width[2];
                        let ref mut fresh8 = MEM[(prev_r + 3) as usize].b32.s1;
                        *fresh8 += -cur_active_width[3] + break_width[3];
                        let ref mut fresh9 = MEM[(prev_r + 4) as usize].b32.s1;
                        *fresh9 += -cur_active_width[4] + break_width[4];
                        let ref mut fresh10 = MEM[(prev_r + 5) as usize].b32.s1;
                        *fresh10 += -cur_active_width[5] + break_width[5];
                        let ref mut fresh11 = MEM[(prev_r + 6) as usize].b32.s1;
                        *fresh11 += -cur_active_width[6] + break_width[6]
                    } else if prev_r == 4999999i32 - 7i32 {
                        active_width[1] = break_width[1];
                        active_width[2] = break_width[2];
                        active_width[3] = break_width[3];
                        active_width[4] = break_width[4];
                        active_width[5] = break_width[5];
                        active_width[6] = break_width[6]
                    } else {
                        q = get_node(7i32);
                        MEM[q as usize].b32.s1 = r;
                        MEM[q as usize].b16.s1 = 2_u16;
                        MEM[q as usize].b16.s0 = 0_u16;
                        MEM[(q + 1) as usize].b32.s1 = break_width[1] - cur_active_width[1];
                        MEM[(q + 2) as usize].b32.s1 = break_width[2] - cur_active_width[2];
                        MEM[(q + 3) as usize].b32.s1 = break_width[3] - cur_active_width[3];
                        MEM[(q + 4) as usize].b32.s1 = break_width[4] - cur_active_width[4];
                        MEM[(q + 5) as usize].b32.s1 = break_width[5] - cur_active_width[5];
                        MEM[(q + 6) as usize].b32.s1 = break_width[6] - cur_active_width[6];
                        MEM[prev_r as usize].b32.s1 = q;
                        prev_prev_r = prev_r;
                        prev_r = q
                    }
                    /* ... resuming 865 ... */
                    if EQTB[(INT_BASE + 16i32) as usize].b32.s1.abs()
                        >= 0x3fffffffi32 - minimum_demerits
                    {
                        minimum_demerits = 0x3fffffffi32 - 1i32
                    } else {
                        minimum_demerits =
                            minimum_demerits + EQTB[(INT_BASE + 16i32) as usize].b32.s1.abs()
                    }
                    fit_class = 0_u8;
                    while fit_class as i32 <= 3i32 {
                        if minimal_demerits[fit_class as usize] <= minimum_demerits {
                            /*874: "Insert a new active node from best_place[fit_class] to cur_p" */
                            q = get_node(2i32);
                            MEM[q as usize].b32.s1 = passive;
                            passive = q;
                            MEM[(q + 1) as usize].b32.s1 = cur_p;
                            MEM[(q + 1) as usize].b32.s0 = best_place[fit_class as usize];
                            q = get_node(active_node_size as i32);
                            MEM[(q + 1) as usize].b32.s1 = passive;
                            MEM[(q + 1) as usize].b32.s0 = best_pl_line[fit_class as usize] + 1;
                            MEM[q as usize].b16.s0 = fit_class as u16;
                            MEM[q as usize].b16.s1 = break_type as u16;
                            MEM[(q + 2) as usize].b32.s1 = minimal_demerits[fit_class as usize];
                            if do_last_line_fit {
                                /*1639: */
                                MEM[(q + 3) as usize].b32.s1 = best_pl_short[fit_class as usize];
                                MEM[(q + 4) as usize].b32.s1 = best_pl_glue[fit_class as usize]
                            }
                            MEM[q as usize].b32.s1 = r;
                            MEM[prev_r as usize].b32.s1 = q;
                            prev_r = q
                        }
                        minimal_demerits[fit_class as usize] = 0x3fffffffi32;
                        fit_class = fit_class.wrapping_add(1)
                    }
                    minimum_demerits = 0x3fffffffi32;
                    /*873: "Insert a delta node to prepare for the next active node" */
                    if r != 4999999i32 - 7i32 {
                        q = get_node(7i32); /* subtype is not used */
                        MEM[q as usize].b32.s1 = r;
                        MEM[q as usize].b16.s1 = 2_u16;
                        MEM[q as usize].b16.s0 = 0_u16;
                        MEM[(q + 1) as usize].b32.s1 = cur_active_width[1] - break_width[1];
                        MEM[(q + 2) as usize].b32.s1 = cur_active_width[2] - break_width[2];
                        MEM[(q + 3) as usize].b32.s1 = cur_active_width[3] - break_width[3];
                        MEM[(q + 4) as usize].b32.s1 = cur_active_width[4] - break_width[4];
                        MEM[(q + 5) as usize].b32.s1 = cur_active_width[5] - break_width[5];
                        MEM[(q + 6) as usize].b32.s1 = cur_active_width[6] - break_width[6];
                        MEM[prev_r as usize].b32.s1 = q;
                        prev_prev_r = prev_r;
                        prev_r = q
                    }
                }
                /* ... resuming 864 ... */
                if r == 4999999i32 - 7i32 {
                    return;
                }
                /*879: "Compute the new line width" */
                if l > easy_line {
                    line_width = second_width;
                    old_l = 0x3fffffffi32 - 1i32
                } else {
                    old_l = l;
                    if l > last_special_line {
                        line_width = second_width
                    } else if EQTB[(LOCAL_BASE + 0i32) as usize].b32.s1 == -0xfffffffi32 {
                        line_width = first_width
                    } else {
                        line_width = MEM
                            [(EQTB[(LOCAL_BASE + 0i32) as usize].b32.s1 + 2i32 * l) as usize]
                            .b32
                            .s1
                    }
                    /* this mem access is in the WEB */
                }
            }
            /*880: "Consider the demerits for a line from r to cur_p; deactivate
             * node r if it should no longer be active; then goto continue if a
             * line from r to cur_p is infeasible; otherwise record a new feasible
             * break" */
            /* Tectonic: if we got here, we must be "considering" a linebreak
             * at the very end of the paragraph. How amazing, it's a perfect fit!
             */
            if semantic_pagination_enabled {
                line_width = cur_active_width[1];
                artificial_demerits = 1i32 != 0;
                shortfall = 0i32
            } else {
                artificial_demerits = false;
                shortfall = line_width - cur_active_width[1];
                if EQTB[(INT_BASE + 70i32) as usize].b32.s1 > 1i32 {
                    shortfall = shortfall + total_pw(r, cur_p)
                }
            }
            if shortfall > 0i32 {
                /*881: "Set the value of b to the badness for stretching the line,
                 * and compute the corresponding fit_class" */
                if cur_active_width[3] != 0i32
                    || cur_active_width[4] != 0i32
                    || cur_active_width[5] != 0i32
                {
                    if do_last_line_fit {
                        if cur_p == -0xfffffffi32 {
                            /*1634: "Perform computations for the last line and goto found" */
                            if MEM[(r + 3) as usize].b32.s1 == 0
                                || MEM[(r + 4) as usize].b32.s1 <= 0
                            {
                                current_block = 5565703735569783978;
                            } else if cur_active_width[3] != fill_width[0]
                                || cur_active_width[4] != fill_width[1]
                                || cur_active_width[5] != fill_width[2]
                            {
                                current_block = 5565703735569783978;
                            } else {
                                if MEM[(r + 3) as usize].b32.s1 > 0 {
                                    g = cur_active_width[2]
                                } else {
                                    g = cur_active_width[6]
                                }
                                if g <= 0i32 {
                                    current_block = 5565703735569783978;
                                } else {
                                    arith_error = false;
                                    g = fract(
                                        g,
                                        MEM[(r + 3) as usize].b32.s1,
                                        MEM[(r + 4) as usize].b32.s1,
                                        0x3fffffffi32,
                                    );
                                    if EQTB[(INT_BASE + 64i32) as usize].b32.s1 < 1000i32 {
                                        g = fract(
                                            g,
                                            EQTB[(INT_BASE + 64i32) as usize].b32.s1,
                                            1000i32,
                                            0x3fffffffi32,
                                        )
                                    }
                                    if arith_error {
                                        if MEM[(r + 3) as usize].b32.s1 > 0 {
                                            g = 0x3fffffffi32
                                        } else {
                                            g = -0x3fffffffi32
                                        }
                                    }
                                    if g > 0i32 {
                                        /*1635: "Set the value of b to the badness of the
                                         * last line for stretching, compute the
                                         * corresponding fit_class, and goto found" */
                                        if g > shortfall {
                                            g = shortfall
                                        }
                                        if g as i64 > 7230584 {
                                            /* XXX: magic number in original WEB code */
                                            if (cur_active_width[2] as i64) < 1663497 {
                                                /* XXX: magic number in original WEB code */
                                                b = 10000i32;
                                                fit_class = 0_u8;
                                                current_block = 11849408527845460430;
                                            } else {
                                                current_block = 16221891950104054966;
                                            }
                                        } else {
                                            current_block = 16221891950104054966;
                                        }
                                        match current_block {
                                            11849408527845460430 => {}
                                            _ => {
                                                b = badness(g, cur_active_width[2]);
                                                if b > 12i32 {
                                                    if b > 99i32 {
                                                        fit_class = 0_u8
                                                    } else {
                                                        fit_class = 1_u8
                                                    }
                                                } else {
                                                    fit_class = 2_u8
                                                }
                                                current_block = 11849408527845460430;
                                            }
                                        }
                                    } else if g < 0i32 {
                                        /*1636: "Set the value of b to the badness of the
                                         * last line for shrinking, compute the
                                         * corresponding fit_class, and goto found" */
                                        if -g > cur_active_width[6] {
                                            g = -cur_active_width[6]
                                        }
                                        b = badness(-g, cur_active_width[6]);
                                        if b > 12i32 {
                                            /* XXX hardcoded in WEB */
                                            fit_class = 3_u8
                                        } else {
                                            fit_class = 2_u8
                                        }
                                        current_block = 11849408527845460430;
                                    } else {
                                        current_block = 5565703735569783978;
                                    }
                                }
                            }
                        } else {
                            current_block = 5565703735569783978;
                        }
                        match current_block {
                            11849408527845460430 => {}
                            _ => {
                                shortfall = 0i32;
                                current_block = 16988252441985098516;
                            }
                        }
                    } else {
                        current_block = 16988252441985098516;
                    }
                    match current_block {
                        11849408527845460430 => {}
                        _ => {
                            b = 0i32;
                            fit_class = 2_u8;
                            current_block = 8633396468472091231;
                        }
                    }
                } else {
                    let mut current_block_230: u64;
                    if shortfall as i64 > 7230584 {
                        /* XXX: magic number in original WEB code */
                        if (cur_active_width[2] as i64) < 1663497 {
                            /* XXX: magic number in original WEB code */
                            b = 10000i32;
                            fit_class = 0_u8;
                            current_block_230 = 4001239642700071046;
                        } else {
                            current_block_230 = 15455430299222214173;
                        }
                    } else {
                        current_block_230 = 15455430299222214173;
                    }
                    match current_block_230 {
                        15455430299222214173 => {
                            b = badness(shortfall, cur_active_width[2]);
                            if b > 12i32 {
                                if b > 99i32 {
                                    fit_class = 0_u8
                                } else {
                                    fit_class = 1_u8
                                }
                            } else {
                                fit_class = 2_u8
                            }
                        }
                        _ => {}
                    }
                    current_block = 8633396468472091231;
                }
            } else {
                /*882: "Set the value of b to the badness for shrinking the line,
                 * and compute the corresponding fit_class" */
                if -shortfall > cur_active_width[6] {
                    b = 10000i32 + 1i32
                } else {
                    b = badness(-shortfall, cur_active_width[6])
                }
                if b > 12i32 {
                    fit_class = 3_u8
                } else {
                    fit_class = 2_u8
                }
                current_block = 8633396468472091231;
            }
            match current_block {
                8633396468472091231 => {
                    if do_last_line_fit {
                        /*1637: "Adjust the additional data for last line" */
                        if cur_p == -0xfffffffi32 {
                            shortfall = 0i32
                        }
                        if shortfall > 0i32 {
                            g = cur_active_width[2]
                        } else if shortfall < 0i32 {
                            g = cur_active_width[6]
                        } else {
                            g = 0i32
                        }
                    }
                }
                _ => {}
            }
            if b > 10000i32 || pi == -10000i32 {
                /*883: "Prepare to deactivate node r, and goto deactivate unless
                 * there is a reason to consider lines of text from r to cur_p" */
                if final_pass as i32 != 0
                    && minimum_demerits == 0x3fffffffi32
                    && MEM[r as usize].b32.s1 == 4999999 - 7
                    && prev_r == 4999999i32 - 7i32
                {
                    artificial_demerits = 1i32 != 0;
                    current_block = 8298116646536739282;
                } else if b > threshold {
                    current_block = 4955522990288899513;
                } else {
                    current_block = 8298116646536739282;
                }
                match current_block {
                    4955522990288899513 => {}
                    _ => {
                        node_r_stays_active = false;
                        current_block = 14114736409816581360;
                    }
                }
            } else {
                prev_r = r;
                if b > threshold {
                    continue;
                }
                node_r_stays_active = 1i32 != 0;
                current_block = 14114736409816581360;
            }
            match current_block {
                14114736409816581360 => {
                    if artificial_demerits {
                        d = 0i32
                    } else {
                        /*888: "Compute the demerits, d, from r to cur_p" */
                        d = INTPAR(INT_PAR__line_penalty) + b;
                        if d.abs() >= 10000 {
                            d = 100000000;
                        /* algorithmic constant */
                        } else {
                            d = d * d
                        }
                        if pi != 0 {
                            if pi > 0 {
                                d = d + pi * pi
                            } else if pi > EJECT_PENALTY {
                                d = d - pi * pi
                            }
                        }
                        if break_type as i32 == 1i32 && MEM[r as usize].b16.s1 as i32 == 1 {
                            if cur_p != TEX_NULL {
                                d = d + INTPAR(INT_PAR__double_hyphen_demerits);
                            } else {
                                d = d + INTPAR(INT_PAR__final_hyphen_demerits);
                            }
                        }
                        if (fit_class as i32 - MEM[r as usize].b16.s0 as i32).abs() > 1 {
                            d = d + INTPAR(INT_PAR__adj_demerits);
                        }
                    }
                    /* resuming 884: */
                    d = d + MEM[(r + 2) as usize].b32.s1;
                    if d <= minimal_demerits[fit_class as usize] {
                        minimal_demerits[fit_class as usize] = d;
                        best_place[fit_class as usize] = MEM[(r + 1) as usize].b32.s1;
                        best_pl_line[fit_class as usize] = l;
                        if do_last_line_fit {
                            /*1638:*/
                            best_pl_short[fit_class as usize] = shortfall;
                            best_pl_glue[fit_class as usize] = g
                        }
                        if d < minimum_demerits {
                            minimum_demerits = d
                        }
                    }
                    if node_r_stays_active {
                        continue;
                    }
                }
                _ => {}
            }
            /*889: "Deactivate node r" */
            MEM[prev_r as usize].b32.s1 = MEM[r as usize].b32.s1;
            free_node(r, active_node_size as i32);
            if prev_r == 4999999i32 - 7i32 {
                /*890: "Update the active widths, since the first active node has been deleted" */
                r = MEM[(4999999 - 7) as usize].b32.s1; /*:966 */
                if MEM[r as usize].b16.s1 as i32 == 2 {
                    active_width[1] += MEM[(r + 1) as usize].b32.s1;
                    active_width[2] += MEM[(r + 2) as usize].b32.s1;
                    active_width[3] += MEM[(r + 3) as usize].b32.s1;
                    active_width[4] += MEM[(r + 4) as usize].b32.s1;
                    active_width[5] += MEM[(r + 5) as usize].b32.s1;
                    active_width[6] += MEM[(r + 6) as usize].b32.s1;
                    cur_active_width[1] = active_width[1];
                    cur_active_width[2] = active_width[2];
                    cur_active_width[3] = active_width[3];
                    cur_active_width[4] = active_width[4];
                    cur_active_width[5] = active_width[5];
                    cur_active_width[6] = active_width[6];
                    MEM[(4999999 - 7) as usize].b32.s1 = MEM[r as usize].b32.s1;
                    free_node(r, 7i32);
                }
            } else if MEM[prev_r as usize].b16.s1 as i32 == 2 {
                r = MEM[prev_r as usize].b32.s1;
                if r == 4999999i32 - 7i32 {
                    cur_active_width[1] -= MEM[(prev_r + 1) as usize].b32.s1;
                    cur_active_width[2] -= MEM[(prev_r + 2) as usize].b32.s1;
                    cur_active_width[3] -= MEM[(prev_r + 3) as usize].b32.s1;
                    cur_active_width[4] -= MEM[(prev_r + 4) as usize].b32.s1;
                    cur_active_width[5] -= MEM[(prev_r + 5) as usize].b32.s1;
                    cur_active_width[6] -= MEM[(prev_r + 6) as usize].b32.s1;
                    MEM[prev_prev_r as usize].b32.s1 = 4999999 - 7;
                    free_node(prev_r, 7i32);
                    prev_r = prev_prev_r
                } else if MEM[r as usize].b16.s1 as i32 == 2 {
                    cur_active_width[1] += MEM[(r + 1) as usize].b32.s1;
                    cur_active_width[2] += MEM[(r + 2) as usize].b32.s1;
                    cur_active_width[3] += MEM[(r + 3) as usize].b32.s1;
                    cur_active_width[4] += MEM[(r + 4) as usize].b32.s1;
                    cur_active_width[5] += MEM[(r + 5) as usize].b32.s1;
                    cur_active_width[6] += MEM[(r + 6) as usize].b32.s1;
                    let ref mut fresh12 = MEM[(prev_r + 1) as usize].b32.s1;
                    *fresh12 += MEM[(r + 1) as usize].b32.s1;
                    let ref mut fresh13 = MEM[(prev_r + 2) as usize].b32.s1;
                    *fresh13 += MEM[(r + 2) as usize].b32.s1;
                    let ref mut fresh14 = MEM[(prev_r + 4) as usize].b32.s1;
                    *fresh14 += MEM[(r + 3) as usize].b32.s1;
                    let ref mut fresh15 = MEM[(prev_r + 4) as usize].b32.s1;
                    *fresh15 += MEM[(r + 4) as usize].b32.s1;
                    let ref mut fresh16 = MEM[(prev_r + 5) as usize].b32.s1;
                    *fresh16 += MEM[(r + 5) as usize].b32.s1;
                    let ref mut fresh17 = MEM[(prev_r + 6) as usize].b32.s1;
                    *fresh17 += MEM[(r + 6) as usize].b32.s1;
                    MEM[prev_r as usize].b32.s1 = MEM[r as usize].b32.s1;
                    free_node(r, 7i32);
                }
            }
        }
    }
}
unsafe extern "C" fn hyphenate() {
    let mut current_block: u64;
    let mut i: i16 = 0;
    let mut j: i16 = 0;
    let mut l: i16 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut bchar: i32 = 0;
    let mut major_tail: i32 = 0;
    let mut minor_tail: i32 = 0;
    let mut c: UnicodeScalar = 0i32;
    let mut c_loc: i16 = 0;
    let mut r_count: i32 = 0;
    let mut hyf_node: i32 = 0;
    let mut z: trie_pointer = 0;
    let mut v: i32 = 0;
    let mut h: hyph_pointer = 0;
    let mut k: str_number = 0;
    let mut u: pool_pointer = 0;
    let mut for_end: i32 = 0;
    j = 0_i16;
    for_end = hn as i32;
    if j as i32 <= for_end {
        loop {
            hyf[j as usize] = 0_u8;
            let fresh18 = j;
            j = j + 1;
            if !((fresh18 as i32) < for_end) {
                break;
            }
        }
    }
    h = hc[1] as hyph_pointer;
    hn += 1;
    hc[hn as usize] = cur_lang as i32;
    let mut for_end_0: i32 = 0;
    j = 2_i16;
    for_end_0 = hn as i32;
    if j as i32 <= for_end_0 {
        loop {
            h = ((h as i32 + h as i32 + hc[j as usize]) % 607i32) as hyph_pointer;
            let fresh19 = j;
            j = j + 1;
            if !((fresh19 as i32) < for_end_0) {
                break;
            }
        }
    }
    loop {
        k = HYPH_WORD[h as usize];
        if k == 0i32 {
            current_block = 10027897684796195291;
            break;
        }
        if length(k) == hn as i32 {
            j = 1_i16;
            u = *str_start.offset((k as i64 - 65536) as isize);
            loop {
                if *str_pool.offset(u as isize) as i32 != hc[j as usize] {
                    current_block = 1763490972649755258;
                    break;
                }
                j += 1;
                u += 1;
                if j as i32 > hn as i32 {
                    current_block = 3275366147856559585;
                    break;
                }
            }
            match current_block {
                1763490972649755258 => {}
                _ => {
                    s = HYPH_LIST[h as usize];
                    while s != -0xfffffffi32 {
                        hyf[MEM[s as usize].b32.s0 as usize] = 1_u8;
                        s = MEM[s as usize].b32.s1
                    }
                    hn -= 1;
                    current_block = 15736053877802236303;
                    break;
                }
            }
        }
        h = HYPH_LINK[h as usize];
        if h as i32 == 0i32 {
            current_block = 10027897684796195291;
            break;
        }
        h = h.wrapping_sub(1)
    }
    match current_block {
        10027897684796195291 => {
            hn -= 1;
            if *trie_trc.offset((cur_lang as i32 + 1i32) as isize) as i32 != cur_lang as i32 {
                return;
            }
            hc[0] = 0i32;
            hc[(hn as i32 + 1i32) as usize] = 0i32;
            hc[(hn as i32 + 2i32) as usize] = max_hyph_char;
            let mut for_end_1: i32 = 0;
            j = 0_i16;
            for_end_1 = hn as i32 - r_hyf + 1i32;
            if j as i32 <= for_end_1 {
                loop {
                    z = *trie_trl.offset((cur_lang as i32 + 1i32) as isize) + hc[j as usize];
                    l = j;
                    while hc[l as usize] == *trie_trc.offset(z as isize) as i32 {
                        if *trie_tro.offset(z as isize) != 0i32 {
                            /*959: */
                            v = *trie_tro.offset(z as isize); /*:958 */
                            loop {
                                v = v + op_start[cur_lang as usize];
                                i = (l as i32 - hyf_distance[v as usize] as i32) as i16;
                                if hyf_num[v as usize] as i32 > hyf[i as usize] as i32 {
                                    hyf[i as usize] = hyf_num[v as usize] as u8
                                }
                                v = hyf_next[v as usize] as i32;
                                if v == 0i32 {
                                    break;
                                }
                            }
                        }
                        l += 1;
                        z = *trie_trl.offset(z as isize) + hc[l as usize]
                    }
                    let fresh20 = j;
                    j = j + 1;
                    if !((fresh20 as i32) < for_end_1) {
                        break;
                    }
                }
            }
        }
        _ => {}
    }
    let mut for_end_2: i32 = 0;
    j = 0_i16;
    for_end_2 = l_hyf - 1i32;
    if j as i32 <= for_end_2 {
        loop {
            hyf[j as usize] = 0_u8;
            let fresh21 = j;
            j = j + 1;
            if !((fresh21 as i32) < for_end_2) {
                break;
            }
        }
    }
    let mut for_end_3: i32 = 0;
    j = 0_i16;
    for_end_3 = r_hyf - 1i32;
    if j as i32 <= for_end_3 {
        loop {
            hyf[(hn as i32 - j as i32) as usize] = 0_u8;
            let fresh22 = j;
            j = j + 1;
            if !((fresh22 as i32) < for_end_3) {
                break;
            }
        }
    }
    let mut for_end_4: i32 = 0;
    j = l_hyf as i16;
    for_end_4 = hn as i32 - r_hyf;
    if j as i32 <= for_end_4 {
        current_block = 5207889489643863322;
    } else {
        current_block = 8102658916883067714;
    }
    loop {
        match current_block {
            8102658916883067714 => return,
            _ => {
                if hyf[j as usize] as i32 & 1i32 != 0 {
                    break;
                }
                let fresh23 = j;
                j = j + 1;
                if (fresh23 as i32) < for_end_4 {
                    current_block = 5207889489643863322;
                } else {
                    current_block = 8102658916883067714;
                }
            }
        }
    }
    if ha != -0xfffffffi32
        && !is_char_node(ha)
        && MEM[ha as usize].b16.s1 as i32 == 8
        && (MEM[ha as usize].b16.s0 as i32 == 40 || MEM[ha as usize].b16.s0 as i32 == 41)
    {
        s = cur_p;
        while MEM[s as usize].b32.s1 != ha {
            s = MEM[s as usize].b32.s1
        }
        hyphen_passed = 0i32 as small_number;
        let mut for_end_5: i32 = 0;
        j = l_hyf as i16;
        for_end_5 = hn as i32 - r_hyf;
        if j as i32 <= for_end_5 {
            loop {
                if hyf[j as usize] as i32 & 1i32 != 0 {
                    q = new_native_word_node(hf, j as i32 - hyphen_passed as i32);
                    MEM[q as usize].b16.s0 = MEM[ha as usize].b16.s0;
                    let mut for_end_6: i32 = 0;
                    i = 0_i16;
                    for_end_6 = j as i32 - hyphen_passed as i32 - 1i32;
                    if i as i32 <= for_end_6 {
                        loop {
                            *(&mut MEM[(q + 6) as usize] as *mut memory_word as *mut u16)
                                .offset(i as isize) =
                                *(&mut MEM[(ha + 6) as usize] as *mut memory_word as *mut u16)
                                    .offset((i as i32 + hyphen_passed as i32) as isize);
                            let fresh24 = i;
                            i = i + 1;
                            if !((fresh24 as i32) < for_end_6) {
                                break;
                            }
                        }
                    }
                    measure_native_node(
                        &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
                        (EQTB[(INT_BASE + 74i32) as usize].b32.s1 > 0i32) as i32,
                    );
                    MEM[s as usize].b32.s1 = q;
                    s = q;
                    q = new_disc();
                    MEM[(q + 1) as usize].b32.s0 = new_native_character(hf, hyf_char);
                    MEM[s as usize].b32.s1 = q;
                    s = q;
                    hyphen_passed = j
                }
                let fresh25 = j;
                j = j + 1;
                if !((fresh25 as i32) < for_end_5) {
                    break;
                }
            }
        }
        hn = MEM[(ha + 4) as usize].b16.s1 as small_number;
        q = new_native_word_node(hf, hn as i32 - hyphen_passed as i32);
        MEM[q as usize].b16.s0 = MEM[ha as usize].b16.s0;
        let mut for_end_7: i32 = 0;
        i = 0_i16;
        for_end_7 = hn as i32 - hyphen_passed as i32 - 1i32;
        if i as i32 <= for_end_7 {
            loop {
                *(&mut MEM[(q + 6) as usize] as *mut memory_word as *mut u16).offset(i as isize) =
                    *(&mut MEM[(ha + 6) as usize] as *mut memory_word as *mut u16)
                        .offset((i as i32 + hyphen_passed as i32) as isize);
                let fresh26 = i;
                i = i + 1;
                if !((fresh26 as i32) < for_end_7) {
                    break;
                }
            }
        }
        measure_native_node(
            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
            (EQTB[(INT_BASE + 74i32) as usize].b32.s1 > 0i32) as i32,
        );
        MEM[s as usize].b32.s1 = q;
        s = q;
        q = MEM[ha as usize].b32.s1;
        MEM[s as usize].b32.s1 = q;
        MEM[ha as usize].b32.s1 = -0xfffffff;
        flush_node_list(ha);
    } else {
        q = MEM[hb as usize].b32.s1;
        MEM[hb as usize].b32.s1 = -0xfffffff;
        r = MEM[ha as usize].b32.s1;
        MEM[ha as usize].b32.s1 = -0xfffffff;
        bchar = hyf_bchar;
        if is_char_node(ha) {
            if MEM[ha as usize].b16.s1 as i32 != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = ha;
                init_lig = false;
                hu[0] = MEM[ha as usize].b16.s0 as i32;
                current_block = 6662862405959679103;
            }
        } else if MEM[ha as usize].b16.s1 as i32 == 6 {
            if MEM[(ha + 1) as usize].b16.s1 as i32 != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = MEM[(ha + 1) as usize].b32.s1;
                init_lig = 1i32 != 0;
                init_lft = MEM[ha as usize].b16.s0 as i32 > 1;
                hu[0] = MEM[(ha + 1) as usize].b16.s0 as i32;
                if init_list == -0xfffffffi32 {
                    if init_lft {
                        hu[0] = max_hyph_char;
                        init_lig = false
                    }
                }
                free_node(ha, 2i32);
                current_block = 6662862405959679103;
            }
        } else {
            if !is_char_node(r) {
                if MEM[r as usize].b16.s1 as i32 == 6 {
                    if MEM[r as usize].b16.s0 as i32 > 1 {
                        current_block = 6826215413708131726;
                    } else {
                        current_block = 2415422468722899689;
                    }
                } else {
                    current_block = 2415422468722899689;
                }
            } else {
                current_block = 2415422468722899689;
            }
            match current_block {
                6826215413708131726 => {}
                _ => {
                    j = 1_i16;
                    s = ha;
                    init_list = -0xfffffffi32;
                    current_block = 5209103994167801282;
                }
            }
        }
        match current_block {
            6662862405959679103 => {
                s = cur_p;
                while MEM[s as usize].b32.s1 != ha {
                    s = MEM[s as usize].b32.s1
                }
                j = 0_i16
            }
            6826215413708131726 => {
                s = ha;
                j = 0_i16;
                hu[0] = max_hyph_char;
                init_lig = false;
                init_list = -0xfffffffi32
            }
            _ => {}
        }
        flush_node_list(r);
        loop {
            l = j;
            j = (reconstitute(j, hn, bchar, hyf_char) as i32 + 1i32) as i16;
            if hyphen_passed as i32 == 0i32 {
                MEM[s as usize].b32.s1 = MEM[(4999999 - 4) as usize].b32.s1;
                while MEM[s as usize].b32.s1 > -0xfffffff {
                    s = MEM[s as usize].b32.s1
                }
                if hyf[(j as i32 - 1i32) as usize] as i32 & 1i32 != 0 {
                    l = j;
                    hyphen_passed = (j as i32 - 1i32) as small_number;
                    MEM[(4999999 - 4) as usize].b32.s1 = -0xfffffff
                }
            }
            if hyphen_passed as i32 > 0i32 {
                loop
                /*949: */
                {
                    r = get_node(2i32);
                    MEM[r as usize].b32.s1 = MEM[(4999999 - 4) as usize].b32.s1;
                    MEM[r as usize].b16.s1 = 7_u16;
                    major_tail = r;
                    r_count = 0i32;
                    while MEM[major_tail as usize].b32.s1 > -0xfffffff {
                        major_tail = MEM[major_tail as usize].b32.s1;
                        r_count += 1
                    }
                    i = hyphen_passed;
                    hyf[i as usize] = 0_u8;
                    minor_tail = -0xfffffffi32;
                    MEM[(r + 1) as usize].b32.s0 = -0xfffffff;
                    hyf_node = new_character(hf, hyf_char as UTF16_code);
                    if hyf_node != -0xfffffffi32 {
                        i += 1;
                        c = hu[i as usize];
                        hu[i as usize] = hyf_char;
                        MEM[hyf_node as usize].b32.s1 = avail;
                        avail = hyf_node
                    }
                    while l as i32 <= i as i32 {
                        l = (reconstitute(l, i, FONT_BCHAR[hf as usize], 65536) as i32 + 1i32)
                            as i16;
                        if MEM[(4999999 - 4) as usize].b32.s1 > -0xfffffff {
                            if minor_tail == -0xfffffffi32 {
                                MEM[(r + 1) as usize].b32.s0 = MEM[(4999999 - 4) as usize].b32.s1
                            } else {
                                MEM[minor_tail as usize].b32.s1 = MEM[(4999999 - 4) as usize].b32.s1
                            }
                            minor_tail = MEM[(4999999 - 4) as usize].b32.s1;
                            while MEM[minor_tail as usize].b32.s1 > -0xfffffff {
                                minor_tail = MEM[minor_tail as usize].b32.s1
                            }
                        }
                    }
                    if hyf_node != -0xfffffffi32 {
                        hu[i as usize] = c;
                        l = i;
                        i -= 1
                    }
                    minor_tail = -0xfffffffi32;
                    MEM[(r + 1) as usize].b32.s1 = -0xfffffff;
                    c_loc = 0_i16;
                    if BCHAR_LABEL[hf as usize] != 0 {
                        l -= 1;
                        c = hu[l as usize];
                        c_loc = l;
                        hu[l as usize] = max_hyph_char
                    }
                    while (l as i32) < j as i32 {
                        loop {
                            l = (reconstitute(l, hn, bchar, 65536i32) as i32 + 1i32) as i16;
                            if c_loc as i32 > 0i32 {
                                hu[c_loc as usize] = c;
                                c_loc = 0_i16
                            }
                            if MEM[(4999999 - 4) as usize].b32.s1 > -0xfffffff {
                                if minor_tail == -0xfffffffi32 {
                                    MEM[(r + 1) as usize].b32.s1 =
                                        MEM[(4999999 - 4) as usize].b32.s1
                                } else {
                                    MEM[minor_tail as usize].b32.s1 =
                                        MEM[(4999999 - 4) as usize].b32.s1
                                }
                                minor_tail = MEM[(4999999 - 4) as usize].b32.s1;
                                while MEM[minor_tail as usize].b32.s1 > -0xfffffff {
                                    minor_tail = MEM[minor_tail as usize].b32.s1
                                }
                            }
                            if l as i32 >= j as i32 {
                                break;
                            }
                        }
                        while l as i32 > j as i32 {
                            /*952: */
                            j = (reconstitute(j, hn, bchar, 65536i32) as i32 + 1i32) as i16; /*:944*/
                            MEM[major_tail as usize].b32.s1 = MEM[(4999999 - 4) as usize].b32.s1;
                            while MEM[major_tail as usize].b32.s1 > -0xfffffff {
                                major_tail = MEM[major_tail as usize].b32.s1;
                                r_count += 1
                            }
                        }
                    }
                    if r_count > 127i32 {
                        MEM[s as usize].b32.s1 = MEM[r as usize].b32.s1;
                        MEM[r as usize].b32.s1 = -0xfffffff;
                        flush_node_list(r);
                    } else {
                        MEM[s as usize].b32.s1 = r;
                        MEM[r as usize].b16.s0 = r_count as u16
                    }
                    s = major_tail;
                    hyphen_passed = (j as i32 - 1i32) as small_number;
                    MEM[(4999999 - 4) as usize].b32.s1 = -0xfffffff;
                    if !(hyf[(j as i32 - 1i32) as usize] as i32 & 1i32 != 0) {
                        break;
                    }
                }
            }
            if j as i32 > hn as i32 {
                break;
            }
        }
        MEM[s as usize].b32.s1 = q;
        flush_list(init_list);
    };
}
unsafe extern "C" fn finite_shrink(mut p: i32) -> i32 {
    let mut q: i32 = 0;
    if no_shrink_error_yet {
        no_shrink_error_yet = false;
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Infinite glue shrinkage found in a paragraph");
        help_ptr = 5_u8;
        help_line[4] = b"The paragraph just ended includes some glue that has";
        help_line[3] = b"infinite shrinkability, e.g., `\\hskip 0pt minus 1fil\'.";
        help_line[2] = b"Such glue doesn\'t belong there---it allows a paragraph";
        help_line[1] = b"of any length to fit on one line. But it\'s safe to proceed,";
        help_line[0] = b"since the offensive shrinkability has been made finite.";
        error();
    }
    q = new_spec(p);
    MEM[q as usize].b16.s0 = 0_u16;
    delete_glue_ref(p);
    q
}
unsafe extern "C" fn reconstitute(
    mut j: small_number,
    mut n: small_number,
    mut bchar: i32,
    mut hchar: i32,
) -> small_number {
    let mut current_block: u64;
    let mut p: i32 = 0;
    let mut t: i32 = 0;
    let mut q: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut cur_rh: i32 = 0;
    let mut test_char: i32 = 0;
    let mut w: scaled_t = 0;
    let mut k: font_index = 0;
    hyphen_passed = 0i32 as small_number;
    t = 4999999i32 - 4i32;
    w = 0i32;
    MEM[(4999999 - 4) as usize].b32.s1 = -0xfffffff;
    cur_l = hu[j as usize];
    cur_q = t;
    if j as i32 == 0i32 {
        ligature_present = init_lig;
        p = init_list;
        if ligature_present {
            lft_hit = init_lft
        }
        while p > -0xfffffffi32 {
            MEM[t as usize].b32.s1 = get_avail();
            t = MEM[t as usize].b32.s1;
            MEM[t as usize].b16.s1 = hf as u16;
            MEM[t as usize].b16.s0 = MEM[p as usize].b16.s0;
            p = MEM[p as usize].b32.s1
        }
    } else if cur_l < 65536i32 {
        MEM[t as usize].b32.s1 = get_avail();
        t = MEM[t as usize].b32.s1;
        MEM[t as usize].b16.s1 = hf as u16;
        MEM[t as usize].b16.s0 = cur_l as u16
    }
    lig_stack = -0xfffffffi32;
    if (j as i32) < n as i32 {
        cur_r = hu[(j as i32 + 1i32) as usize]
    } else {
        cur_r = bchar
    }
    if hyf[j as usize] as i32 & 1i32 != 0 {
        cur_rh = hchar
    } else {
        cur_rh = 65536i32
    }
    'c_27176: loop {
        if cur_l == 65536i32 {
            k = BCHAR_LABEL[hf as usize];
            if k == 0i32 {
                current_block = 4939169394500275451;
            } else {
                q = FONT_INFO[k as usize].b16;
                current_block = 1434579379687443766;
            }
        } else {
            q = FONT_INFO
                [(CHAR_BASE[hf as usize] + effective_char(1i32 != 0, hf, cur_l as u16)) as usize]
                .b16;
            if q.s1 as i32 % 4i32 != 1i32 {
                current_block = 4939169394500275451;
            } else {
                k = LIG_KERN_BASE[hf as usize] + q.s0 as i32;
                q = FONT_INFO[k as usize].b16;
                if q.s3 as i32 > 128i32 {
                    k = ((LIG_KERN_BASE[hf as usize] + 256i32 * q.s1 as i32 + q.s0 as i32) as i64
                        + 32768
                        - (256i32 * 128i32) as i64) as font_index;
                    q = FONT_INFO[k as usize].b16
                }
                current_block = 1434579379687443766;
            }
        }
        match current_block {
            1434579379687443766 => {
                if cur_rh < 65536i32 {
                    test_char = cur_rh
                } else {
                    test_char = cur_r
                }
                loop {
                    if q.s2 as i32 == test_char {
                        if q.s3 as i32 <= 128i32 {
                            if cur_rh < 65536i32 {
                                hyphen_passed = j;
                                hchar = 65536i32;
                                cur_rh = 65536i32;
                                continue 'c_27176;
                            } else {
                                if hchar < 65536i32 {
                                    if hyf[j as usize] as i32 & 1i32 != 0 {
                                        hyphen_passed = j;
                                        hchar = 65536i32
                                    }
                                }
                                if (q.s1 as i32) < 128i32 {
                                    /*946: */
                                    if cur_l == 65536i32 {
                                        lft_hit = 1i32 != 0
                                    }
                                    if j as i32 == n as i32 {
                                        if lig_stack == -0xfffffffi32 {
                                            rt_hit = 1i32 != 0
                                        }
                                    }
                                    match q.s1 as i32 {
                                        1 | 5 => {
                                            cur_l = q.s0 as i32;
                                            ligature_present = 1i32 != 0
                                        }
                                        2 | 6 => {
                                            cur_r = q.s0 as i32;
                                            if lig_stack > -0xfffffffi32 {
                                                MEM[lig_stack as usize].b16.s0 = cur_r as u16
                                            } else {
                                                lig_stack = new_lig_item(cur_r as u16);
                                                if j as i32 == n as i32 {
                                                    bchar = 65536i32
                                                } else {
                                                    p = get_avail();
                                                    MEM[(lig_stack + 1) as usize].b32.s1 = p;
                                                    MEM[p as usize].b16.s0 =
                                                        hu[(j as i32 + 1i32) as usize] as u16;
                                                    MEM[p as usize].b16.s1 = hf as u16
                                                }
                                            }
                                        }
                                        3 => {
                                            cur_r = q.s0 as i32;
                                            p = lig_stack;
                                            lig_stack = new_lig_item(cur_r as u16);
                                            MEM[lig_stack as usize].b32.s1 = p
                                        }
                                        7 | 11 => {
                                            if ligature_present {
                                                p = new_ligature(
                                                    hf,
                                                    cur_l as u16,
                                                    MEM[cur_q as usize].b32.s1,
                                                );
                                                if lft_hit {
                                                    MEM[p as usize].b16.s0 = 2_u16;
                                                    lft_hit = false
                                                }
                                                MEM[cur_q as usize].b32.s1 = p;
                                                t = p;
                                                ligature_present = false
                                            }
                                            cur_q = t;
                                            cur_l = q.s0 as i32;
                                            ligature_present = 1i32 != 0
                                        }
                                        _ => {
                                            cur_l = q.s0 as i32;
                                            ligature_present = 1i32 != 0;
                                            if lig_stack > -0xfffffffi32 {
                                                if MEM[(lig_stack + 1) as usize].b32.s1
                                                    > -0xfffffffi32
                                                {
                                                    MEM[t as usize].b32.s1 =
                                                        MEM[(lig_stack + 1) as usize].b32.s1;
                                                    t = MEM[t as usize].b32.s1;
                                                    j += 1
                                                }
                                                p = lig_stack;
                                                lig_stack = MEM[p as usize].b32.s1;
                                                free_node(p, 2i32);
                                                if lig_stack == -0xfffffffi32 {
                                                    if (j as i32) < n as i32 {
                                                        cur_r = hu[(j as i32 + 1i32) as usize]
                                                    } else {
                                                        cur_r = bchar
                                                    }
                                                    if hyf[j as usize] as i32 & 1i32 != 0 {
                                                        cur_rh = hchar
                                                    } else {
                                                        cur_rh = 65536i32
                                                    }
                                                } else {
                                                    cur_r = MEM[lig_stack as usize].b16.s0 as i32
                                                }
                                            } else {
                                                if j as i32 == n as i32 {
                                                    break;
                                                }
                                                MEM[t as usize].b32.s1 = get_avail();
                                                t = MEM[t as usize].b32.s1;
                                                MEM[t as usize].b16.s1 = hf as u16;
                                                MEM[t as usize].b16.s0 = cur_r as u16;
                                                j += 1;
                                                if (j as i32) < n as i32 {
                                                    cur_r = hu[(j as i32 + 1i32) as usize]
                                                } else {
                                                    cur_r = bchar
                                                }
                                                if hyf[j as usize] as i32 & 1i32 != 0 {
                                                    cur_rh = hchar
                                                } else {
                                                    cur_rh = 65536i32
                                                }
                                            }
                                        }
                                    }
                                    if !(q.s1 as i32 > 4i32) {
                                        continue 'c_27176;
                                    }
                                    if q.s1 as i32 != 7i32 {
                                        break;
                                    } else {
                                        continue 'c_27176;
                                    }
                                } else {
                                    w = FONT_INFO[(KERN_BASE[hf as usize]
                                        + 256i32 * q.s1 as i32
                                        + q.s0 as i32)
                                        as usize]
                                        .b32
                                        .s1;
                                    break;
                                }
                            }
                        }
                    }
                    if q.s3 as i32 >= 128i32 {
                        if cur_rh == 65536i32 {
                            break;
                        }
                        cur_rh = 65536i32;
                        continue 'c_27176;
                    } else {
                        k = k + q.s3 as i32 + 1i32;
                        q = FONT_INFO[k as usize].b16
                    }
                }
            }
            _ => {}
        }
        if ligature_present {
            p = new_ligature(hf, cur_l as u16, MEM[cur_q as usize].b32.s1);
            if lft_hit {
                MEM[p as usize].b16.s0 = 2_u16;
                lft_hit = false
            }
            if rt_hit {
                if lig_stack == -0xfffffffi32 {
                    let ref mut fresh28 = MEM[p as usize].b16.s0;
                    *fresh28 = (*fresh28).wrapping_add(1);
                    rt_hit = false
                }
            }
            MEM[cur_q as usize].b32.s1 = p;
            t = p;
            ligature_present = false
        }
        if w != 0i32 {
            MEM[t as usize].b32.s1 = new_kern(w);
            t = MEM[t as usize].b32.s1;
            w = 0i32;
            MEM[(t + 2) as usize].b32.s0 = 0
        }
        if !(lig_stack > -0xfffffffi32) {
            break;
        }
        cur_q = t;
        cur_l = MEM[lig_stack as usize].b16.s0 as i32;
        ligature_present = 1i32 != 0;
        if MEM[(lig_stack + 1) as usize].b32.s1 > -0xfffffff {
            MEM[t as usize].b32.s1 = MEM[(lig_stack + 1) as usize].b32.s1;
            t = MEM[t as usize].b32.s1;
            j += 1
        }
        p = lig_stack;
        lig_stack = MEM[p as usize].b32.s1;
        free_node(p, 2i32);
        if lig_stack == -0xfffffffi32 {
            if (j as i32) < n as i32 {
                cur_r = hu[(j as i32 + 1i32) as usize]
            } else {
                cur_r = bchar
            }
            if hyf[j as usize] as i32 & 1i32 != 0 {
                cur_rh = hchar
            } else {
                cur_rh = 65536i32
            }
        } else {
            cur_r = MEM[lig_stack as usize].b16.s0 as i32
        }
    }
    j
}
unsafe extern "C" fn total_pw(mut q: i32, mut p: i32) -> scaled_t {
    let mut l: i32 = 0;
    let mut r: i32 = 0;
    let mut n: i32 = 0;
    if MEM[(q + 1) as usize].b32.s1 == -0xfffffff {
        l = first_p
    } else {
        l = MEM[(MEM[(q + 1) as usize].b32.s1 + 1) as usize].b32.s1
    }
    r = prev_rightmost(global_prev_p, p);
    if p != -0xfffffffi32
        && MEM[p as usize].b16.s1 as i32 == 7
        && MEM[(p + 1) as usize].b32.s0 != -0xfffffff
    {
        r = MEM[(p + 1) as usize].b32.s0;
        while MEM[r as usize].b32.s1 != -0xfffffff {
            r = MEM[r as usize].b32.s1
        }
    } else {
        r = find_protchar_right(l, r)
    }
    if l != -0xfffffffi32 && MEM[l as usize].b16.s1 as i32 == 7 {
        if MEM[(l + 1) as usize].b32.s1 != -0xfffffff {
            l = MEM[(l + 1) as usize].b32.s1;
            return char_pw(l, 0) + char_pw(r, 1);
        } else {
            n = MEM[l as usize].b16.s0 as i32;
            l = MEM[l as usize].b32.s1;
            while n > 0i32 {
                if MEM[l as usize].b32.s1 != -0xfffffff {
                    l = MEM[l as usize].b32.s1
                }
                n -= 1
            }
        }
    }
    l = find_protchar_left(l, true);
    char_pw(l, 0) + char_pw(r, 1)
}
unsafe extern "C" fn find_protchar_left(mut l: i32, mut d: bool) -> i32 {
    let mut t: i32 = 0;
    let mut run: bool = false;
    if MEM[l as usize].b32.s1 != -0xfffffff
        && MEM[l as usize].b16.s1 as i32 == 0
        && MEM[(l + 1) as usize].b32.s1 == 0
        && MEM[(l + 3) as usize].b32.s1 == 0
        && MEM[(l + 2) as usize].b32.s1 == 0
        && MEM[(l + 5) as usize].b32.s1 == -0xfffffff
    {
        l = MEM[l as usize].b32.s1
    } else if d {
        while MEM[l as usize].b32.s1 != -0xfffffff
            && !(is_char_node(l) as i32 != 0 || is_non_discardable_node(l) as i32 != 0)
        {
            l = MEM[l as usize].b32.s1
        }
    }
    hlist_stack_level = 0_i16;
    run = 1i32 != 0;
    loop {
        t = l;
        while run as i32 != 0
            && MEM[l as usize].b16.s1 as i32 == 0
            && MEM[(l + 5) as usize].b32.s1 != -0xfffffff
        {
            push_node(l);
            l = MEM[(l + 5) as usize].b32.s1
        }
        while run as i32 != 0
            && (!is_char_node(l)
                && (MEM[l as usize].b16.s1 as i32 == 3
                    || MEM[l as usize].b16.s1 as i32 == 4
                    || MEM[l as usize].b16.s1 as i32 == 5
                    || MEM[l as usize].b16.s1 as i32 == 12
                    || MEM[l as usize].b16.s1 as i32 == 7
                        && MEM[(l + 1) as usize].b32.s0 == -0xfffffff
                        && MEM[(l + 1) as usize].b32.s1 == -0xfffffff
                        && MEM[l as usize].b16.s0 as i32 == 0
                    || MEM[l as usize].b16.s1 as i32 == 9 && MEM[(l + 1) as usize].b32.s1 == 0
                    || MEM[l as usize].b16.s1 as i32 == 11
                        && (MEM[(l + 1) as usize].b32.s1 == 0
                            || MEM[l as usize].b16.s0 as i32 == 0)
                    || MEM[l as usize].b16.s1 as i32 == 10 && MEM[(l + 1) as usize].b32.s0 == 0
                    || MEM[l as usize].b16.s1 as i32 == 0
                        && MEM[(l + 1) as usize].b32.s1 == 0
                        && MEM[(l + 3) as usize].b32.s1 == 0
                        && MEM[(l + 2) as usize].b32.s1 == 0
                        && MEM[(l + 5) as usize].b32.s1 == -0xfffffff))
        {
            while MEM[l as usize].b32.s1 == -0xfffffff && hlist_stack_level as i32 > 0 {
                l = pop_node()
            }
            if MEM[l as usize].b32.s1 != -0xfffffff {
                l = MEM[l as usize].b32.s1
            } else if hlist_stack_level as i32 == 0i32 {
                run = false
            }
        }
        if t == l {
            break;
        }
    }
    l
}
unsafe extern "C" fn find_protchar_right(mut l: i32, mut r: i32) -> i32 {
    let mut t: i32 = 0;
    let mut run: bool = false;
    if r == -0xfffffffi32 {
        return -0xfffffffi32;
    }
    hlist_stack_level = 0_i16;
    run = 1i32 != 0;
    loop {
        t = r;
        while run as i32 != 0
            && MEM[r as usize].b16.s1 as i32 == 0
            && MEM[(r + 5) as usize].b32.s1 != -0xfffffff
        {
            push_node(l);
            push_node(r);
            l = MEM[(r + 5) as usize].b32.s1;
            r = l;
            while MEM[r as usize].b32.s1 != -0xfffffff {
                r = MEM[r as usize].b32.s1
            }
        }
        while run as i32 != 0
            && (!is_char_node(r)
                && (MEM[r as usize].b16.s1 as i32 == 3
                    || MEM[r as usize].b16.s1 as i32 == 4
                    || MEM[r as usize].b16.s1 as i32 == 5
                    || MEM[r as usize].b16.s1 as i32 == 12
                    || MEM[r as usize].b16.s1 as i32 == 7
                        && MEM[(r + 1) as usize].b32.s0 == -0xfffffff
                        && MEM[(r + 1) as usize].b32.s1 == -0xfffffff
                        && MEM[r as usize].b16.s0 as i32 == 0
                    || MEM[r as usize].b16.s1 as i32 == 9 && MEM[(r + 1) as usize].b32.s1 == 0
                    || MEM[r as usize].b16.s1 as i32 == 11
                        && (MEM[(r + 1) as usize].b32.s1 == 0
                            || MEM[r as usize].b16.s0 as i32 == 0)
                    || MEM[r as usize].b16.s1 as i32 == 10 && MEM[(r + 1) as usize].b32.s0 == 0
                    || MEM[r as usize].b16.s1 as i32 == 0
                        && MEM[(r + 1) as usize].b32.s1 == 0
                        && MEM[(r + 3) as usize].b32.s1 == 0
                        && MEM[(r + 2) as usize].b32.s1 == 0
                        && MEM[(r + 5) as usize].b32.s1 == -0xfffffff))
        {
            while r == l && hlist_stack_level as i32 > 0i32 {
                r = pop_node();
                l = pop_node()
            }
            if r != l && r != -0xfffffffi32 {
                r = prev_rightmost(l, r)
            } else if r == l && hlist_stack_level as i32 == 0i32 {
                run = false
            }
        }
        if t == r {
            break;
        }
    }
    r
}
unsafe extern "C" fn push_node(mut p: i32) {
    if hlist_stack_level as i32 > 512i32 {
        pdf_error(b"push_node", b"stack overflow");
    }
    hlist_stack[hlist_stack_level as usize] = p;
    hlist_stack_level = (hlist_stack_level as i32 + 1i32) as i16;
}
unsafe extern "C" fn pop_node() -> i32 {
    hlist_stack_level = (hlist_stack_level as i32 - 1i32) as i16;
    if (hlist_stack_level as i32) < 0i32 {
        pdf_error(b"pop_node", b"stack underflow (internal error)");
    }
    hlist_stack[hlist_stack_level as usize]
}
