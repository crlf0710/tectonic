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
use crate::xetex_xetexd::{
    clear_NODE_subtype, is_char_node, is_non_discardable_node, kern_NODE_subtype, set_NODE_subtype,
    set_NODE_type, ACTIVE_NODE_break_node, ACTIVE_NODE_fitness, ACTIVE_NODE_glue,
    ACTIVE_NODE_line_number, ACTIVE_NODE_shortfall, ACTIVE_NODE_total_demerits, BOX_shift_amount,
    BOX_width, CHAR_NODE_character, CHAR_NODE_font, DELTA_NODE_dshrink, DELTA_NODE_dstretch0,
    DELTA_NODE_dstretch1, DELTA_NODE_dstretch2, DELTA_NODE_dstretch3, DELTA_NODE_dwidth,
    DISCRETIONARY_NODE_post_break, DISCRETIONARY_NODE_pre_break, DISCRETIONARY_NODE_replace_count,
    GLUE_NODE_glue_ptr, GLUE_NODE_leader_ptr, GLUE_SPEC_ref_count, GLUE_SPEC_shrink,
    GLUE_SPEC_shrink_order, GLUE_SPEC_stretch, GLUE_SPEC_stretch_order, LANGUAGE_NODE_what_lang,
    LANGUAGE_NODE_what_lhm, LANGUAGE_NODE_what_rhm, LIGATURE_NODE_lig_char, LIGATURE_NODE_lig_font,
    LIGATURE_NODE_lig_ptr, LLIST_info, LLIST_link, NATIVE_NODE_font, NATIVE_NODE_length,
    NODE_subtype, NODE_type, PASSIVE_NODE_cur_break, PASSIVE_NODE_next_break,
    PASSIVE_NODE_prev_break, PENALTY_NODE_penalty, TeXInt, TeXOpt, FONT_CHARACTER_INFO,
    FONT_CHARACTER_WIDTH, MIN_TRIE_OP,
};

pub(crate) type scaled_t = i32;
pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
pub(crate) type small_number = i16;
pub(crate) type font_index = i32;
pub(crate) type nine_bits = i32;
pub(crate) type trie_pointer = i32;
pub(crate) type trie_opcode = u16;
pub(crate) type hyph_pointer = u16;

const AWFUL_BAD: i32 = 0x3FFFFFFF;
const VERY_LOOSE_FIT: u8 = 0;
const LOOSE_FIT: u8 = 1;
const DECENT_FIT: u8 = 2;
const TIGHT_FIT: u8 = 3;
const LAST_ACTIVE: usize = ACTIVE_LIST;

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
unsafe fn get_native_usv(p: usize, i: usize) -> UnicodeScalar {
    let mut c: u16 =
        *(&mut MEM[(p + 6) as usize] as *mut memory_word as *mut u16).offset(i as isize);
    if c as i32 >= 0xd800 && (c as i32) < 0xdc00 {
        return 0x10000
            + (c as i32 - 0xd800) * 0x400
            + *(&mut MEM[(p + 6) as usize] as *mut memory_word as *mut u16).offset((i + 1) as isize)
                as i32
            - 0xdc00;
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
pub(crate) unsafe fn line_break(mut d: bool) {
    let mut current_block: u64;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut prev_s: i32 = 0;
    let mut f: usize = 0;
    let mut j: small_number = 0;
    let mut c: UnicodeScalar = 0;
    let mut l: i32 = 0;
    let mut i: i32 = 0;
    let mut for_end_1: i32 = 0;
    pack_begin_line = cur_list.mode_line; /* "this is for over/underfull box messages" */
    *LLIST_link(TEMP_HEAD as usize) = *LLIST_link(cur_list.head);

    /* Remove trailing space or glue if present; add infinite penalty then par_fill_skip */

    if is_char_node(cur_list.tail as i32) {
        /* is_char_node */
        *LLIST_link(cur_list.tail) = new_penalty(INF_PENALTY) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    } else if NODE_type(cur_list.tail) != GLUE_NODE {
        *LLIST_link(cur_list.tail) = new_penalty(INF_PENALTY) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    } else {
        set_NODE_type(cur_list.tail, PENALTY_NODE);
        delete_glue_ref(*GLUE_NODE_glue_ptr(cur_list.tail) as usize);
        flush_node_list(GLUE_NODE_leader_ptr(cur_list.tail).opt());
        *PENALTY_NODE_penalty(cur_list.tail) = INF_PENALTY;
    }

    *LLIST_link(cur_list.tail) = new_param_glue(GluePar::par_fill_skip as _) as i32;
    last_line_fill = *LLIST_link(cur_list.tail);

    /* Yet more initialization of various kinds */

    init_cur_lang = (cur_list.prev_graf % 65536) as _;
    init_l_hyf = cur_list.prev_graf / 0x400000;
    init_r_hyf = (cur_list.prev_graf / 65536) % 64;

    pop_nest();

    no_shrink_error_yet = true;

    if *GLUE_SPEC_shrink_order(*GLUEPAR(GluePar::left_skip) as usize) != NORMAL as _
        && *GLUE_SPEC_shrink(*GLUEPAR(GluePar::left_skip) as usize) != 0
    {
        *GLUEPAR(GluePar::left_skip) = finite_shrink(*GLUEPAR(GluePar::left_skip) as usize) as i32;
    }
    if *GLUE_SPEC_shrink_order(*GLUEPAR(GluePar::right_skip) as usize) != NORMAL as _
        && *GLUE_SPEC_shrink(*GLUEPAR(GluePar::right_skip) as usize) != 0
    {
        *GLUEPAR(GluePar::right_skip) =
            finite_shrink(*GLUEPAR(GluePar::right_skip) as usize) as i32;
    }

    q = *GLUEPAR(GluePar::left_skip);
    r = *GLUEPAR(GluePar::right_skip);
    background[1] = *BOX_width(q as usize) + *BOX_width(r as usize);
    background[2] = 0;
    background[3] = 0;
    background[4] = 0;
    background[5] = 0;
    background[2 + *GLUE_SPEC_stretch_order(q as usize) as usize] = *GLUE_SPEC_stretch(q as usize);
    background[2 + *GLUE_SPEC_stretch_order(r as usize) as usize] += *GLUE_SPEC_stretch(r as usize);
    background[6] = *GLUE_SPEC_shrink(q as usize) + *GLUE_SPEC_shrink(r as usize);

    /* 1631: "check for special treatment of last line of paragraph" (\lastlinefit > 0) */

    do_last_line_fit = false; /*863:*/
    active_node_size = ACTIVE_NODE_SIZE_NORMAL as _;
    if *INTPAR(IntPar::last_line_fit) > 0 {
        q = *GLUE_NODE_glue_ptr(last_line_fill as usize);
        if *GLUE_SPEC_stretch(q as usize) > 0 && *GLUE_SPEC_stretch_order(q as usize) > NORMAL as _
        {
            if background[3] == 0 && background[4] == 0 && background[5] == 0 {
                do_last_line_fit = true;
                active_node_size = ACTIVE_NODE_SIZE_EXTENDED as _;
                fill_width[0] = 0;
                fill_width[1] = 0;
                fill_width[2] = 0;
                fill_width[*GLUE_SPEC_stretch_order(q as usize) as usize - 1] =
                    *GLUE_SPEC_stretch(q as usize);
            }
        }
    }
    minimum_demerits = AWFUL_BAD; /* 863: */
    minimal_demerits[TIGHT_FIT as usize] = AWFUL_BAD;
    minimal_demerits[DECENT_FIT as usize] = AWFUL_BAD;
    minimal_demerits[LOOSE_FIT as usize] = AWFUL_BAD;
    minimal_demerits[VERY_LOOSE_FIT as usize] = AWFUL_BAD;

    /* Prep relating to par_shape (877) */

    if LOCAL(Local::par_shape).is_texnull() {
        if *DIMENPAR(DimenPar::hang_indent) == 0 {
            last_special_line = 0;
            second_width = *DIMENPAR(DimenPar::hsize);
            second_indent = 0;
        } else {
            /*878:*/
            last_special_line = (*INTPAR(IntPar::hang_after)).abs();

            if *INTPAR(IntPar::hang_after) < 0 {
                first_width = *DIMENPAR(DimenPar::hsize) - (*DIMENPAR(DimenPar::hang_indent)).abs();
                if *DIMENPAR(DimenPar::hang_indent) >= 0 {
                    first_indent = *DIMENPAR(DimenPar::hang_indent);
                } else {
                    first_indent = 0;
                }
                second_width = *DIMENPAR(DimenPar::hsize);
                second_indent = 0;
            } else {
                first_width = *DIMENPAR(DimenPar::hsize);
                first_indent = 0;
                second_width =
                    *DIMENPAR(DimenPar::hsize) - (*DIMENPAR(DimenPar::hang_indent)).abs();
                if *DIMENPAR(DimenPar::hang_indent) >= 0 {
                    second_indent = *DIMENPAR(DimenPar::hang_indent);
                } else {
                    second_indent = 0;
                }
            }
        }
    } else {
        last_special_line = *LLIST_info(*LOCAL(Local::par_shape) as usize) - 1;
        /* These direct `mem` accesses are in the original WEB code */
        second_width = MEM
            [*LOCAL(Local::par_shape) as usize + 2 * (last_special_line as usize + 1)]
            .b32
            .s1;
        second_indent = MEM[*LOCAL(Local::par_shape) as usize + 2 * last_special_line as usize + 1]
            .b32
            .s1;
    }

    if *INTPAR(IntPar::looseness) == 0 {
        easy_line = last_special_line
    } else {
        easy_line = MAX_HALFWORD; /*:877*/
    }

    /* Start finding optimal breakpoints (892) */

    threshold = *INTPAR(IntPar::pretolerance);
    if threshold >= 0 {
        second_pass = false;
        final_pass = false
    } else {
        threshold = *INTPAR(IntPar::tolerance);
        second_pass = true;
        final_pass = *DIMENPAR(DimenPar::emergency_stretch) <= 0;
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
        q = get_node(active_node_size as i32) as i32;
        MEM[q as usize].b16.s1 = UNHYPHENATED as _; //set_NODE_type(q as usize, UNHYPHENATED as _);
        MEM[q as usize].b16.s0 = DECENT_FIT as _;
        *LLIST_link(q as usize) = LAST_ACTIVE as i32;
        MEM[(q + 1) as usize].b32.s1 = TEX_NULL;
        MEM[(q + 1) as usize].b32.s0 = cur_list.prev_graf + 1;
        MEM[(q + 2) as usize].b32.s1 = 0;
        *LLIST_link(ACTIVE_LIST as usize) = q;

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
        cur_p = *LLIST_link(TEMP_HEAD as usize);
        let mut auto_breaking = true;

        global_prev_p = cur_p;
        let mut prev_p = global_prev_p;
        first_p = cur_p;

        while !cur_p.is_texnull() && *LLIST_link(ACTIVE_LIST as usize) != LAST_ACTIVE as i32 {
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
                    f = MEM[cur_p as usize].b16.s1 as usize;
                    eff_char = effective_char(true, f, MEM[cur_p as usize].b16.s0);
                    active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                        + FONT_INFO[(CHAR_BASE[f as usize] + eff_char) as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1;
                    cur_p = *LLIST_link(cur_p as usize);
                    if !is_char_node(cur_p) {
                        break;
                    }
                }
            }
            match NODE_type(cur_p as usize) {
                HLIST_NODE | VLIST_NODE | RULE_NODE => {
                    active_width[1] += *BOX_width(cur_p as usize)
                }
                WHATSIT_NODE => {
                    if NODE_subtype(cur_p as usize) == LANGUAGE_NODE as _ {
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
                    } else if NODE_subtype(cur_p as usize) == NATIVE_WORD_NODE
                        || NODE_subtype(cur_p as usize) == NATIVE_WORD_NODE_AT
                        || NODE_subtype(cur_p as usize) == GLYPH_NODE
                        || NODE_subtype(cur_p as usize) == PIC_NODE
                        || NODE_subtype(cur_p as usize) == PDF_NODE
                    {
                        active_width[1] += *BOX_width(cur_p as usize);
                    }
                }
                GLUE_NODE => {
                    if auto_breaking {
                        if is_char_node(prev_p) {
                            try_break(0, UNHYPHENATED as _);
                        } else if is_non_discardable_node(prev_p) {
                            try_break(0, UNHYPHENATED as _);
                        } else if NODE_type(prev_p as usize) == KERN_NODE
                            && kern_NODE_subtype(prev_p as usize) != KernNodeSubType::Explicit
                        {
                            try_break(0, UNHYPHENATED as _);
                        }
                    }
                    q = *GLUE_NODE_glue_ptr(cur_p as usize);
                    if *GLUE_SPEC_shrink_order(q as usize) != NORMAL
                        && *GLUE_SPEC_shrink(q as usize) != 0
                    {
                        let fresh3 = GLUE_NODE_glue_ptr(cur_p as usize);
                        *fresh3 = finite_shrink(q as usize) as i32;
                        q = *fresh3
                    }
                    active_width[1] += *BOX_width(q as usize);
                    active_width[(2 + *GLUE_SPEC_stretch_order(q as usize)) as usize] +=
                        *GLUE_SPEC_stretch(q as usize);
                    /*:895*/
                    active_width[6] += *GLUE_SPEC_shrink(q as usize); /*:897*/

                    if second_pass && auto_breaking {
                        /*924: "Try to hyphenate the following word." */
                        prev_s = cur_p;
                        s = *LLIST_link(prev_s as usize);

                        if s != TEX_NULL {
                            's_786: loop
                            /*930: skip to node ha, or goto done1 if no hyphenation should be attempted */
                            {
                                if is_char_node(s) {
                                    c = *CHAR_NODE_character(s as usize) as UnicodeScalar; /*:930*/
                                    hf = *CHAR_NODE_font(s as usize) as usize;
                                    current_block = 11202235766349324107;
                                } else if NODE_type(s as usize) == LIGATURE_NODE {
                                    if *LIGATURE_NODE_lig_ptr(s as usize) == TEX_NULL {
                                        current_block = 13855806088735179493;
                                    } else {
                                        q = *LIGATURE_NODE_lig_ptr(s as usize);
                                        c = *CHAR_NODE_character(q as usize) as UnicodeScalar;
                                        hf = *CHAR_NODE_font(q as usize) as usize;
                                        current_block = 11202235766349324107;
                                    }
                                } else if NODE_type(s as usize) == KERN_NODE
                                    && kern_NODE_subtype(s as usize) == KernNodeSubType::Normal
                                {
                                    current_block = 13855806088735179493;
                                } else if NODE_type(s as usize) == MATH_NODE
                                    && MEM[s as usize].b16.s0 >= L_CODE
                                // NODE_subtype(s as usize)
                                {
                                    current_block = 13855806088735179493;
                                } else {
                                    if !(NODE_type(s as usize) == WHATSIT_NODE) {
                                        current_block = 8166967358843938227;
                                        break;
                                    }
                                    if NODE_subtype(s as usize) == NATIVE_WORD_NODE
                                        || NODE_subtype(s as usize) == NATIVE_WORD_NODE_AT
                                    {
                                        l = 0;
                                        while l < *NATIVE_NODE_length(s as usize) as i32 {
                                            c = get_native_usv(s as usize, l as usize);
                                            if *LC_CODE(c as usize) != 0 {
                                                hf = *NATIVE_NODE_font(s as usize) as usize;
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

                                    if NODE_subtype(s as usize) == LANGUAGE_NODE {
                                        cur_lang = *LANGUAGE_NODE_what_lang(s as usize) as u8;
                                        l_hyf = *LANGUAGE_NODE_what_lhm(s as usize) as i32;
                                        r_hyf = *LANGUAGE_NODE_what_rhm(s as usize) as i32;

                                        if *trie_trc.offset((hyph_start + cur_lang as i32) as isize)
                                            as i32
                                            != cur_lang as i32
                                        {
                                            hyph_index = 0
                                        } else {
                                            hyph_index = *trie_trl
                                                .offset((hyph_start + cur_lang as i32) as isize)
                                        }
                                    }
                                    current_block = 13855806088735179493;
                                }
                                match current_block {
                                    11202235766349324107 => {
                                        if hyph_index == 0 || c > 255 {
                                            hc[0] = *LC_CODE(c as usize);
                                        } else if *trie_trc.offset((hyph_index + c) as isize) as i32
                                            != c
                                        {
                                            hc[0] = 0;
                                        } else {
                                            hc[0] = *trie_tro.offset((hyph_index + c) as isize)
                                        }
                                        if hc[0] != 0 {
                                            if hc[0] == c || *INTPAR(IntPar::uc_hyph) > 0 {
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
                                s = *LLIST_link(prev_s as usize);
                            }
                            match current_block {
                                8166967358843938227 => {}
                                _ => {
                                    hyf_char = HYPHEN_CHAR[hf as usize];
                                    if !(hyf_char < 0) {
                                        if !(hyf_char > BIGGEST_CHAR) {
                                            ha = prev_s;

                                            if !(l_hyf + r_hyf > max_hyphenatable_length()) {
                                                if ha != TEX_NULL
                                                    && ha < hi_mem_min
                                                    && NODE_type(ha as usize) == WHATSIT_NODE
                                                    && (NODE_subtype(ha as usize)
                                                        == NATIVE_WORD_NODE
                                                        || NODE_subtype(ha as usize)
                                                            == NATIVE_WORD_NODE_AT)
                                                {
                                                    /*926: check that nodes after native_word permit hyphenation; if not, goto done1 */
                                                    s = *LLIST_link(ha as usize);

                                                    loop {
                                                        if !is_char_node(s) {
                                                            match NODE_type(s as usize) {
                                                                LIGATURE_NODE => {}
                                                                KERN_NODE => {
                                                                    if kern_NODE_subtype(s as usize)
                                                                        != KernNodeSubType::Normal
                                                                    {
                                                                        current_block =
                                                                            2606747282402567793;
                                                                        break;
                                                                    }
                                                                }
                                                                WHATSIT_NODE | GLUE_NODE
                                                                | PENALTY_NODE | INS_NODE
                                                                | ADJUST_NODE | MARK_NODE => {
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
                                                        s = *LLIST_link(s as usize);
                                                    }
                                                    match current_block {
                                                        8166967358843938227 => {}
                                                        _ => {
                                                            /*927: prepare a native_word_node for hyphenation.
                                                             * "Note that if there are chars with lccode = 0,
                                                             * we split them out into separate native_word
                                                             * nodes." */
                                                            hn = 0 as small_number;
                                                            'c_31290: loop {
                                                                /* 'ha' can change in the loop, so for safety: */
                                                                for_end_1 = *NATIVE_NODE_length(
                                                                    ha as usize,
                                                                )
                                                                    as i32;

                                                                l = 0;
                                                                loop {
                                                                    if !(l < for_end_1) {
                                                                        break 'c_31290;
                                                                    }
                                                                    c = get_native_usv(
                                                                        ha as usize,
                                                                        l as usize,
                                                                    );
                                                                    if hyph_index == 0 || c > 255 {
                                                                        hc[0] =
                                                                            *LC_CODE(c as usize);
                                                                    } else if *trie_trc.offset(
                                                                        (hyph_index + c) as isize,
                                                                    )
                                                                        as i32
                                                                        != c
                                                                    {
                                                                        hc[0] = 0
                                                                    } else {
                                                                        hc[0] = *trie_tro.offset(
                                                                            (hyph_index + c)
                                                                                as isize,
                                                                        )
                                                                    }
                                                                    if hc[0] == 0 {
                                                                        if hn > 0 {
                                                                            q = new_native_word_node(
                                                                                hf,
                                                                                *NATIVE_NODE_length(
                                                                                    ha as usize,
                                                                                )
                                                                                    as i32
                                                                                    - l,
                                                                            )
                                                                                as i32;
                                                                            set_NODE_subtype(
                                                                                q as usize,
                                                                                NODE_subtype(
                                                                                    ha as usize,
                                                                                ),
                                                                            );
                                                                            i = l;
                                                                            while i < *NATIVE_NODE_length(ha as usize)
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
                                                                                                    (*INTPAR(IntPar::xetex_use_glyph_metrics)
                                                                                                         >
                                                                                                         0)
                                                                                                        as
                                                                                                        i32);
                                                                            *LLIST_link(
                                                                                q as usize,
                                                                            ) = *LLIST_link(
                                                                                ha as usize,
                                                                            );
                                                                            *LLIST_link(
                                                                                ha as usize,
                                                                            ) = q;
                                                                            *NATIVE_NODE_length(
                                                                                ha as usize,
                                                                            ) = l as u16;
                                                                            measure_native_node(&mut MEM[ha
                                                                                                                         as
                                                                                                                         usize]
                                                                                                        as
                                                                                                        *mut memory_word
                                                                                                        as
                                                                                                        *mut libc::c_void,
                                                                                                    (*INTPAR(IntPar::xetex_use_glyph_metrics)
                                                                                                         >
                                                                                                         0)
                                                                                                        as
                                                                                                        i32);
                                                                            break 'c_31290;
                                                                        }
                                                                    } else if hn == 0 && l > 0 {
                                                                        q = new_native_word_node(
                                                                            hf,
                                                                            *NATIVE_NODE_length(
                                                                                ha as usize,
                                                                            )
                                                                                as i32
                                                                                - l,
                                                                        )
                                                                            as i32;
                                                                        set_NODE_subtype(
                                                                            q as usize,
                                                                            NODE_subtype(
                                                                                ha as usize,
                                                                            ),
                                                                        );
                                                                        i = l;
                                                                        while i
                                                                            < *NATIVE_NODE_length(
                                                                                ha as usize,
                                                                            )
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
                                                                                                (*INTPAR(IntPar::xetex_use_glyph_metrics)
                                                                                                     >
                                                                                                     0)
                                                                                                    as
                                                                                                    i32);
                                                                        *LLIST_link(q as usize) =
                                                                            *LLIST_link(
                                                                                ha as usize,
                                                                            );
                                                                        *LLIST_link(ha as usize) =
                                                                            q;
                                                                        *NATIVE_NODE_length(
                                                                            ha as usize,
                                                                        ) = l as u16;
                                                                        measure_native_node(&mut MEM[ha
                                                                                                                     as
                                                                                                                     usize]
                                                                                                    as
                                                                                                    *mut memory_word
                                                                                                    as
                                                                                                    *mut libc::c_void,
                                                                                                (*INTPAR(IntPar::xetex_use_glyph_metrics)
                                                                                                     >
                                                                                                     0)
                                                                                                    as
                                                                                                    i32);
                                                                        ha = *LLIST_link(
                                                                            ha as usize,
                                                                        );
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
                                                                            hu[hn as usize] =
                                                                                ((c as i64 - 65536)
                                                                                    / 1024
                                                                                    + 0xd800)
                                                                                    as i32;
                                                                            hc[hn as usize] =
                                                                                ((hc[0] as i64
                                                                                    - 65536)
                                                                                    / 1024
                                                                                    + 0xd800)
                                                                                    as i32;
                                                                            hn += 1;
                                                                            hu[hn as usize] =
                                                                                c % 1024 + 0xdc00;
                                                                            hc[hn as usize] = hc[0]
                                                                                % 1024
                                                                                + 0xdc00;
                                                                            l += 1;
                                                                        }
                                                                        hyf_bchar = TOO_BIG_CHAR;
                                                                    }
                                                                    l += 1;
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
                                                            if *CHAR_NODE_font(s as usize) as usize
                                                                != hf
                                                            {
                                                                break;
                                                            }
                                                            hyf_bchar =
                                                                *CHAR_NODE_character(s as usize)
                                                                    as i32;
                                                            c = hyf_bchar;
                                                            if hyph_index == 0 || c > 255 {
                                                                hc[0] = *LC_CODE(c as usize);
                                                            } else if *trie_trc
                                                                .offset((hyph_index + c) as isize)
                                                                as i32
                                                                != c
                                                            {
                                                                hc[0] = 0
                                                            } else {
                                                                hc[0] = *trie_tro.offset(
                                                                    (hyph_index + c) as isize,
                                                                )
                                                            }
                                                            if hc[0] == 0 {
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
                                                            hyf_bchar = TOO_BIG_CHAR;
                                                        } else if NODE_type(s as usize)
                                                            == LIGATURE_NODE
                                                        {
                                                            /*932: move the characters of a ligature node to hu and hc; but goto done3
                                                             * if they are not all letters. */
                                                            if *LIGATURE_NODE_lig_font(s as usize)
                                                                as usize
                                                                != hf
                                                            {
                                                                break;
                                                            }
                                                            j = hn;
                                                            q = *LIGATURE_NODE_lig_ptr(s as usize);
                                                            if q > TEX_NULL {
                                                                hyf_bchar =
                                                                    *CHAR_NODE_character(q as usize)
                                                                        as i32
                                                            }
                                                            while q > TEX_NULL {
                                                                c = *CHAR_NODE_character(q as usize)
                                                                    as UnicodeScalar;
                                                                if hyph_index == 0 || c > 255 {
                                                                    hc[0] = *LC_CODE(c as usize);
                                                                } else if *trie_trc.offset(
                                                                    (hyph_index + c) as isize,
                                                                )
                                                                    as i32
                                                                    != c
                                                                {
                                                                    hc[0] = 0;
                                                                } else {
                                                                    hc[0] = *trie_tro.offset(
                                                                        (hyph_index + c) as isize,
                                                                    )
                                                                }
                                                                if hc[0] == 0 {
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
                                                                q = *LLIST_link(q as usize);
                                                            }
                                                            hb = s;
                                                            hn = j;
                                                            if MEM[s as usize].b16.s0 as i32 & 1
                                                                != 0
                                                            {
                                                                hyf_bchar = FONT_BCHAR[hf as usize]
                                                            } else {
                                                                hyf_bchar = TOO_BIG_CHAR;
                                                            }
                                                        /*:932*/
                                                        } else {
                                                            if !(NODE_type(s as usize) == KERN_NODE
                                                                && kern_NODE_subtype(s as usize)
                                                                    == KernNodeSubType::Normal)
                                                            {
                                                                break;
                                                            }
                                                            hb = s;
                                                            hyf_bchar = FONT_BCHAR[hf as usize]
                                                        }
                                                        s = *LLIST_link(s as usize);
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
                                                                    match NODE_type(s as usize) {
                                                                        LIGATURE_NODE => {}
                                                                        KERN_NODE => {
                                                                            current_block =
                                                                                5935670669791948619;
                                                                            if kern_NODE_subtype(
                                                                                s as usize,
                                                                            ) != KernNodeSubType::Normal
                                                                            {
                                                                                current_block
                                                                                            =
                                                                                            16848571710846909653;
                                                                                break;
                                                                            }
                                                                        }
                                                                        WHATSIT_NODE
                                                                        | GLUE_NODE
                                                                        | PENALTY_NODE
                                                                        | INS_NODE
                                                                        | ADJUST_NODE
                                                                        | MARK_NODE => {
                                                                            current_block
                                                                                =
                                                                                16848571710846909653;
                                                                            break;
                                                                        }
                                                                        MATH_NODE => {
                                                                            current_block =
                                                                                2529459302156174429;
                                                                            if MEM[s as usize]
                                                                                .b16
                                                                                .s0
                                                                                >= L_CODE
                                                                            {
                                                                                // NODE_subtype(s as usize)
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
                                                                s = *LLIST_link(s as usize);
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
                KERN_NODE => {
                    /* ... resuming 895 ... */
                    if kern_NODE_subtype(cur_p as usize) == KernNodeSubType::Explicit {
                        if (!is_char_node(*LLIST_link(cur_p as usize)) as i32) < hi_mem_min
                            && auto_breaking as i32 != 0
                        {
                            if NODE_type(*LLIST_link(cur_p as usize) as usize) == GLUE_NODE {
                                try_break(0, UNHYPHENATED);
                            }
                        }
                        active_width[1] += *BOX_width(cur_p as usize);
                    } else {
                        active_width[1] += *BOX_width(cur_p as usize);
                    }
                }
                LIGATURE_NODE => {
                    f = *LIGATURE_NODE_lig_font(cur_p as usize) as usize;
                    xtx_ligature_present = true;
                    active_width[1] += *FONT_CHARACTER_WIDTH(
                        f,
                        effective_char(true, f, *LIGATURE_NODE_lig_char(cur_p as usize)) as usize,
                    );
                }
                DISC_NODE => {
                    /*898: try to break after a discretionary fragment, then goto done5 */
                    s = *DISCRETIONARY_NODE_pre_break(cur_p as usize);
                    disc_width = 0;

                    if s.is_texnull() {
                        try_break(*INTPAR(IntPar::ex_hyphen_penalty), HYPHENATED as _);
                    } else {
                        loop {
                            /*899:*/
                            if is_char_node(s) {
                                let mut eff_char_0: i32 = 0; /*:898 big DISC_NODE case */
                                f = MEM[s as usize].b16.s1 as usize;
                                eff_char_0 = effective_char(true, f, MEM[s as usize].b16.s0);
                                disc_width += *FONT_CHARACTER_WIDTH(f, eff_char_0 as usize);
                            } else {
                                match NODE_type(s as usize) {
                                    LIGATURE_NODE => {
                                        let mut eff_char_1: i32 = 0;
                                        f = *LIGATURE_NODE_lig_font(s as usize) as usize;
                                        xtx_ligature_present = true;
                                        eff_char_1 = effective_char(
                                            true,
                                            f,
                                            *LIGATURE_NODE_lig_char(s as usize),
                                        );
                                        disc_width += *FONT_CHARACTER_WIDTH(f, eff_char_1 as usize);
                                    }
                                    HLIST_NODE | VLIST_NODE | RULE_NODE | KERN_NODE => {
                                        disc_width += *BOX_width(s as usize)
                                    }
                                    WHATSIT_NODE => {
                                        if NODE_subtype(s as usize) == NATIVE_WORD_NODE
                                            || NODE_subtype(s as usize) == NATIVE_WORD_NODE_AT
                                            || NODE_subtype(s as usize) == GLYPH_NODE
                                            || NODE_subtype(s as usize) == PIC_NODE
                                            || NODE_subtype(s as usize) == PDF_NODE
                                        {
                                            disc_width += *BOX_width(s as usize);
                                        } else {
                                            confusion(b"disc3a");
                                        }
                                    }
                                    _ => confusion(b"disc3"),
                                }
                            }
                            s = *LLIST_link(s as usize);
                            if !(s != TEX_NULL) {
                                break;
                            }
                        }
                        active_width[1] += disc_width;
                        try_break(*INTPAR(IntPar::hyphen_penalty), HYPHENATED as _);
                        active_width[1] -= disc_width;
                    }
                    r = *DISCRETIONARY_NODE_replace_count(cur_p as usize) as i32;
                    s = *LLIST_link(cur_p as usize);
                    while r > 0 {
                        if is_char_node(s) {
                            let mut eff_char_2: i32 = 0;
                            f = MEM[s as usize].b16.s1 as usize;
                            eff_char_2 = effective_char(true, f, *CHAR_NODE_character(s as usize));
                            active_width[1] += *FONT_CHARACTER_WIDTH(f, eff_char_2 as usize);
                        } else {
                            match NODE_type(s as usize) {
                                LIGATURE_NODE => {
                                    let mut eff_char_3: i32 = 0;
                                    f = *LIGATURE_NODE_lig_font(s as usize) as usize;
                                    xtx_ligature_present = true;
                                    eff_char_3 =
                                        effective_char(true, f, MEM[(s + 1) as usize].b16.s0);
                                    active_width[1] +=
                                        *FONT_CHARACTER_WIDTH(f, eff_char_3 as usize);
                                }
                                HLIST_NODE | VLIST_NODE | RULE_NODE | KERN_NODE => {
                                    active_width[1] += *BOX_width(s as usize)
                                }
                                WHATSIT_NODE => {
                                    if NODE_subtype(s as usize) == NATIVE_WORD_NODE
                                        || NODE_subtype(s as usize) == NATIVE_WORD_NODE_AT
                                        || NODE_subtype(s as usize) == GLYPH_NODE
                                        || NODE_subtype(s as usize) == PIC_NODE
                                        || NODE_subtype(s as usize) == PDF_NODE
                                    {
                                        active_width[1] += *BOX_width(s as usize);
                                    } else {
                                        confusion(b"disc4a");
                                    }
                                }
                                _ => confusion(b"disc4"),
                            }
                        }
                        r -= 1;
                        s = *LLIST_link(s as usize);
                    }
                    global_prev_p = cur_p;
                    prev_p = global_prev_p;
                    cur_p = s;
                    continue;
                }
                MATH_NODE => {
                    if MEM[cur_p as usize].b16.s0 < L_CODE {
                        // NODE_subtype(cur_p as usize)
                        auto_breaking = NODE_subtype(cur_p as usize) as i32 & 1 != 0
                    }
                    if !is_char_node(*LLIST_link(cur_p as usize)) && auto_breaking {
                        if NODE_type(*LLIST_link(cur_p as usize) as usize) == GLUE_NODE {
                            try_break(0, UNHYPHENATED);
                        }
                    }
                    active_width[1] += *BOX_width(cur_p as usize);
                }
                PENALTY_NODE => try_break(*PENALTY_NODE_penalty(cur_p as usize), UNHYPHENATED),
                MARK_NODE | INS_NODE | ADJUST_NODE => {}
                _ => confusion(b"paragraph"),
            }
            global_prev_p = cur_p;
            prev_p = global_prev_p;
            cur_p = *LLIST_link(cur_p as usize);
        }
        if cur_p.is_texnull() {
            /*902: "Try the final line break at the end of the paragraph, and
             * goto done if the desired breakpoints have been found." */
            try_break(EJECT_PENALTY, HYPHENATED);
            if *LLIST_link(ACTIVE_LIST as usize) != LAST_ACTIVE as i32 {
                /*903:*/
                r = *LLIST_link(ACTIVE_LIST as usize);
                fewest_demerits = MAX_HALFWORD;

                loop {
                    if NODE_type(r as usize) != DELTA_NODE {
                        if *ACTIVE_NODE_total_demerits(r as usize) < fewest_demerits {
                            fewest_demerits = *ACTIVE_NODE_total_demerits(r as usize); /*:904*/
                            best_bet = r;
                        }
                    }
                    r = *LLIST_link(r as usize);
                    if !(r != LAST_ACTIVE as i32) {
                        break;
                    }
                }
                best_line = *ACTIVE_NODE_line_number(best_bet as usize);
                if *INTPAR(IntPar::looseness) == 0 {
                    break;
                }

                r = *LLIST_link(ACTIVE_LIST as usize); /*904:*/
                actual_looseness = 0;

                loop {
                    if NODE_type(r as usize) != DELTA_NODE {
                        line_diff = *ACTIVE_NODE_line_number(r as usize) - best_line;

                        if line_diff < actual_looseness && *INTPAR(IntPar::looseness) <= line_diff
                            || line_diff > actual_looseness
                                && *INTPAR(IntPar::looseness) >= line_diff
                        {
                            best_bet = r;
                            actual_looseness = line_diff;
                            fewest_demerits = *ACTIVE_NODE_total_demerits(r as usize);
                        } else if line_diff == actual_looseness
                            && *ACTIVE_NODE_total_demerits(r as usize) < fewest_demerits
                        {
                            best_bet = r;
                            fewest_demerits = *ACTIVE_NODE_total_demerits(r as usize);
                        }
                    }
                    r = *LLIST_link(r as usize);
                    if !(r != LAST_ACTIVE as i32) {
                        break;
                    }
                }
                best_line = *ACTIVE_NODE_line_number(best_bet as usize);
                if actual_looseness == *INTPAR(IntPar::looseness) || final_pass {
                    break;
                }
            }
        }
        /*894: clean up the memory by removing the break nodes */
        q = *LLIST_link(ACTIVE_LIST);
        while q != LAST_ACTIVE as i32 {
            cur_p = *LLIST_link(q as usize);
            if NODE_type(q as usize) == DELTA_NODE {
                free_node(q as usize, DELTA_NODE_SIZE);
            } else {
                free_node(q as usize, active_node_size as i32);
            }
            q = cur_p
        }

        q = passive;

        while q != TEX_NULL {
            cur_p = *LLIST_link(q as usize);
            free_node(q as usize, PASSIVE_NODE_SIZE);
            q = cur_p;
        }
        /* ... resuming 892 ... */
        if !second_pass {
            threshold = *INTPAR(IntPar::tolerance);
            second_pass = true;
            final_pass = *DIMENPAR(DimenPar::emergency_stretch) <= 0;
        } else {
            background[2] = background[2] + *DIMENPAR(DimenPar::emergency_stretch);
            final_pass = true;
        }
    }
    if do_last_line_fit {
        /*1641:*/
        if *ACTIVE_NODE_shortfall(best_bet as usize) == 0 {
            do_last_line_fit = false
        } else {
            let q = new_spec(*GLUE_NODE_glue_ptr(last_line_fill as usize) as usize);
            delete_glue_ref(*GLUE_NODE_glue_ptr(last_line_fill as usize) as usize);
            MEM[q + 1].b32.s1 +=
                *ACTIVE_NODE_shortfall(best_bet as usize) - *ACTIVE_NODE_glue(best_bet as usize);
            *GLUE_SPEC_stretch(q) = 0;
            *GLUE_NODE_glue_ptr(last_line_fill as usize) = q as i32;
        }
    }

    post_line_break(d);

    /* Clean up by removing break nodes (894, again) */
    let mut q = *LLIST_link(ACTIVE_LIST);
    while q != ACTIVE_LIST as i32 {
        let mut next: i32 = *LLIST_link(q as usize);

        if NODE_type(q as usize) == DELTA_NODE {
            free_node(q as usize, DELTA_NODE_SIZE);
        } else {
            free_node(q as usize, active_node_size as i32);
        }
        q = next;
    }

    let mut q = passive;

    while q != TEX_NULL {
        let mut next_0: i32 = *LLIST_link(q as usize);
        free_node(q as usize, PASSIVE_NODE_SIZE);
        q = next_0;
    }
    /* All done */
    pack_begin_line = 0;
}
/* This was just separated out to prevent line_break() from becoming
 * proposterously long. */
unsafe fn post_line_break(mut d: bool) {
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut p: i32 = 0;
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
    LR_ptr = cur_list.eTeX_aux.tex_int();
    /* Reverse the list of break nodes (907) */
    q = *ACTIVE_NODE_break_node(best_bet as usize); /*:907*/
    cur_p = TEX_NULL;
    loop {
        r = q;
        q = *PASSIVE_NODE_prev_break(q as usize);
        *PASSIVE_NODE_next_break(r as usize) = cur_p;
        cur_p = r;
        if !(q != TEX_NULL) {
            break;
        }
    }
    cur_line = cur_list.prev_graf + 1;
    loop {
        /* 909: justify the line ending at breakpoint cur_p and append it to
         * the current vertical list, with associated penalties and
         * insertions. The current line starts a TEMP_HEAD.link and ends at
         * cur_p.cur_break.
         **/
        if *INTPAR(IntPar::texxet) > 0 {
            /*1494:*/
            q = MEM[TEMP_HEAD].b32.s1;
            if !LR_ptr.is_texnull() {
                temp_ptr = LR_ptr;
                r = q;
                loop {
                    s = new_math(0, (MEM[temp_ptr as usize].b32.s0 - 1) as small_number) as i32;
                    MEM[s as usize].b32.s1 = r;
                    r = s;
                    temp_ptr = *LLIST_link(temp_ptr as usize);
                    if !(temp_ptr != TEX_NULL) {
                        break;
                    }
                }
                MEM[TEMP_HEAD].b32.s1 = r;
            }
            while q != MEM[(cur_p + 1) as usize].b32.s1 {
                if q < hi_mem_min && NODE_type(q as usize) == MATH_NODE {
                    /*1495:*/
                    if MEM[q as usize].b16.s0 as i32 & 1 != 0 {
                        if LR_ptr != TEX_NULL
                            && MEM[LR_ptr as usize].b32.s0
                                == (L_CODE as i32)
                                    * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32))
                                    + 3
                        {
                            temp_ptr = LR_ptr;
                            LR_ptr = MEM[temp_ptr as usize].b32.s1;
                            MEM[temp_ptr as usize].b32.s1 = avail;
                            avail = temp_ptr
                        }
                    } else {
                        temp_ptr = get_avail() as i32;
                        MEM[temp_ptr as usize].b32.s0 =
                            (L_CODE as i32) * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32)) + 3;
                        MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                        LR_ptr = temp_ptr;
                    }
                }
                q = *LLIST_link(q as usize);
            }
        }
        /* 910: "Modify the end of the line to reflect the nature of the break
         * and to include \rightskip; also set the proper value of
         * disc_break" */
        q = *PASSIVE_NODE_cur_break(cur_p as usize);
        disc_break = false;
        post_disc_break = false;
        glue_break = false;

        if q.is_texnull() {
            q = TEMP_HEAD as i32;
            while !LLIST_link(q as usize).is_texnull() {
                q = *LLIST_link(q as usize);
            }
        } else if NODE_type(q as usize) == GLUE_NODE {
            delete_glue_ref(*GLUE_NODE_glue_ptr(q as usize) as usize);
            *GLUE_NODE_glue_ptr(q as usize) = *GLUEPAR(GluePar::right_skip);
            MEM[q as usize].b16.s0 = GluePar::right_skip as u16 + 1; // NODE_subtype(q as usize)
            *GLUE_SPEC_ref_count(*GLUEPAR(GluePar::right_skip) as usize) += 1;
            glue_break = true;
        } else if NODE_type(q as usize) == DISC_NODE {
            /*911:*/
            t = MEM[q as usize].b16.s0;
            if t == 0 {
                r = *LLIST_link(q as usize);
            } else {
                r = q;
                while t > 1 {
                    r = *LLIST_link(r as usize);
                    t -= 1;
                }
                s = *LLIST_link(r as usize);
                r = *LLIST_link(s as usize);
                *LLIST_link(s as usize) = TEX_NULL;
                flush_node_list(LLIST_link(q as usize).opt());
                *DISCRETIONARY_NODE_replace_count(q as usize) = 0;
            }
            if *DISCRETIONARY_NODE_post_break(q as usize) != TEX_NULL {
                /*913:*/
                s = *DISCRETIONARY_NODE_post_break(q as usize);

                while *LLIST_link(s as usize) != TEX_NULL {
                    s = *LLIST_link(s as usize);
                }

                *LLIST_link(s as usize) = r;

                r = *DISCRETIONARY_NODE_post_break(q as usize);
                *DISCRETIONARY_NODE_post_break(q as usize) = TEX_NULL;
                post_disc_break = true;
            }
            if *DISCRETIONARY_NODE_pre_break(q as usize) != TEX_NULL {
                /*914:*/
                s = *DISCRETIONARY_NODE_pre_break(q as usize);
                *LLIST_link(q as usize) = s;

                while *LLIST_link(s as usize) != TEX_NULL {
                    s = *LLIST_link(s as usize);
                }
                *DISCRETIONARY_NODE_pre_break(q as usize) = TEX_NULL;
                q = s;
            }
            *LLIST_link(q as usize) = r;
            disc_break = true;
        } else if NODE_type(q as usize) == KERN_NODE {
            *BOX_width(q as usize) = 0
        } else if NODE_type(q as usize) == MATH_NODE {
            *BOX_width(q as usize) = 0;
            if *INTPAR(IntPar::texxet) > 0 {
                /*1495:*/
                if *INTPAR(IntPar::texxet) as i32 & 1 != 0 {
                    if !LR_ptr.is_texnull()
                        && MEM[LR_ptr as usize].b32.s0
                            == (L_CODE as i32) * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32))
                                + 3i32
                    {
                        temp_ptr = LR_ptr;
                        LR_ptr = MEM[temp_ptr as usize].b32.s1;
                        MEM[temp_ptr as usize].b32.s1 = avail;
                        avail = temp_ptr;
                    }
                } else {
                    temp_ptr = get_avail() as i32;
                    MEM[temp_ptr as usize].b32.s0 =
                        (L_CODE as i32) * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32)) + 3;
                    MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                    LR_ptr = temp_ptr;
                }
            }
        }
        /* "at this point q is the rightmost breakpoint; the only exception is
         * the case of a discretionary break with non-empty pre_break -- then
         * q has been changed to the last node of the pre-break list" */
        if *INTPAR(IntPar::xetex_protrude_chars) > 0 {
            if disc_break as i32 != 0 && (is_char_node(q) || NODE_type(q as usize) != DISC_NODE) {
                p = q; /*:915*/
                ptmp = p
            } else {
                p = prev_rightmost(MEM[TEMP_HEAD].b32.s1, q);
                ptmp = p;
                p = find_protchar_right(MEM[TEMP_HEAD].b32.s1, p)
            }
            w = char_pw(p, 1i32 as small_number);
            if w != 0i32 {
                let k = new_margin_kern(-w, last_rightmost_char, 1);
                MEM[k].b32.s1 = MEM[ptmp as usize].b32.s1;
                MEM[ptmp as usize].b32.s1 = k as i32;
                if ptmp == q {
                    q = *LLIST_link(q as usize);
                }
            }
        }
        if !glue_break {
            r = new_param_glue(GluePar::right_skip as small_number) as i32;
            MEM[r as usize].b32.s1 = *LLIST_link(q as usize);
            MEM[q as usize].b32.s1 = r;
            q = r
        }
        if *INTPAR(IntPar::texxet) > 0 {
            /*1496:*/
            if !LR_ptr.is_texnull() {
                s = TEMP_HEAD as i32;
                r = MEM[s as usize].b32.s1;

                while r != q {
                    s = r;
                    r = MEM[s as usize].b32.s1
                }

                r = LR_ptr;

                while r != TEX_NULL {
                    temp_ptr = new_math(0, MEM[r as usize].b32.s0 as small_number) as i32;
                    MEM[s as usize].b32.s1 = temp_ptr;
                    s = temp_ptr;
                    r = *LLIST_link(r as usize);
                }

                MEM[s as usize].b32.s1 = q;
            }
        }
        /* 916: Put \leftskip at the left and detach this line. */
        r = *LLIST_link(q as usize);
        *LLIST_link(q as usize) = TEX_NULL;
        q = *LLIST_link(TEMP_HEAD as usize);
        *LLIST_link(TEMP_HEAD as usize) = r;
        /* "at this point q is the leftmost node; all discardable nodes have been discarded */
        
        if *INTPAR(IntPar::xetex_protrude_chars) > 0 {
            p = q;
            p = find_protchar_left(p, false);
            w = char_pw(p, 0);
            if w != 0 {
                let k = new_margin_kern(-w, last_leftmost_char, 0);
                MEM[k].b32.s1 = q;
                q = k as i32;
            }
        }
        if *GLUEPAR(GluePar::left_skip) != 0 {
            r = new_param_glue(GluePar::left_skip as small_number) as i32;
            *LLIST_link(r as usize) = q;
            q = r
        }
        /* 918: q points to the hlist that represents the current line. Pack
         * it up at the right width. */
        if cur_line > last_special_line {
            cur_width = second_width;
            cur_indent = second_indent
        } else if LOCAL(Local::par_shape).is_texnull() {
            cur_width = first_width;
            cur_indent = first_indent
        } else {
            /* These manual `mem` indices are in the original WEB code */
            cur_width = MEM[*LOCAL(Local::par_shape) as usize + 2 * cur_line as usize]
                .b32
                .s1;
            cur_indent = MEM[*LOCAL(Local::par_shape) as usize + 2 * cur_line as usize - 1]
                .b32
                .s1;
        }
        adjust_tail = ADJUST_HEAD as i32;
        pre_adjust_tail = PRE_ADJUST_HEAD as i32;
        /* Tectonic: in semantic pagination mode, set each "line" (really the
         * whole paragraph) at its natural width. */
        if semantic_pagination_enabled {
            just_box = hpack(q, 0, ADDITIONAL as small_number)
        } else {
            just_box = hpack(q, cur_width, EXACTLY as small_number)
        } /*:918*/
        *BOX_shift_amount(just_box as usize) = cur_indent;
        /* 917: append the new box to the current vertical list, followed
         * by any of its special nodes that were taken out */

        if PRE_ADJUST_HEAD as i32 != pre_adjust_tail {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(PRE_ADJUST_HEAD as usize); /*:917*/
            cur_list.tail = pre_adjust_tail as usize;
        }

        pre_adjust_tail = TEX_NULL;
        append_to_vlist(just_box as usize);

        if ADJUST_HEAD as i32 != adjust_tail {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(ADJUST_HEAD as usize);
            cur_list.tail = adjust_tail as usize;
        }

        adjust_tail = TEX_NULL;

        /* 919: Set `pen` to all of the penalties relevant to this line. */
        if cur_line + 1 != best_line {
            q = EQTB[INTER_LINE_PENALTIES_LOC].b32.s1;
            if q != TEX_NULL {
                r = cur_line;
                if r > *PENALTY_NODE_penalty(q as usize) {
                    r = *PENALTY_NODE_penalty(q as usize)
                }
                pen = *PENALTY_NODE_penalty((q + r) as usize)
            } else {
                pen = *INTPAR(IntPar::inter_line_penalty)
            }
            q = EQTB[CLUB_PENALTIES_LOC].b32.s1;
            if q != TEX_NULL {
                r = cur_line - cur_list.prev_graf;
                if r > *PENALTY_NODE_penalty(q as usize) {
                    r = *PENALTY_NODE_penalty(q as usize)
                }
                pen += *PENALTY_NODE_penalty((q + r) as usize)
            } else if cur_line == cur_list.prev_graf + 1 {
                pen += *INTPAR(IntPar::club_penalty)
            }
            if d {
                q = EQTB[DISPLAY_WIDOW_PENALTIES_LOC].b32.s1
            } else {
                q = EQTB[WIDOW_PENALTIES_LOC].b32.s1
            }
            if q != TEX_NULL {
                r = best_line - cur_line - 1i32;
                if r > *PENALTY_NODE_penalty(q as usize) {
                    r = *PENALTY_NODE_penalty(q as usize)
                }
                pen += *PENALTY_NODE_penalty((q + r) as usize);
            } else if cur_line + 2i32 == best_line {
                if d {
                    pen += *INTPAR(IntPar::display_widow_penalty)
                } else {
                    pen += *INTPAR(IntPar::widow_penalty)
                }
            }
            if disc_break {
                pen += *INTPAR(IntPar::broken_penalty)
            }
            if pen != 0 {
                r = new_penalty(pen) as i32;
                *LLIST_link(cur_list.tail) = r;
                cur_list.tail = r as usize;
            }
        }
        /* Done justifying this line. */
        cur_line += 1;
        cur_p = *PASSIVE_NODE_next_break(cur_p as usize);
        if cur_p != TEX_NULL {
            if !post_disc_break {
                /* 908: "prune unwanted nodes at the beginning of the next
                 * line". Delete glues, penalties, kerns, and math nodes at
                 * the beginning of the line, unless the node in question is
                 * the chosen breakpoint. */
                let mut r = TEMP_HEAD;
                loop {
                    q = *LLIST_link(r as usize);
                    if q == *PASSIVE_NODE_cur_break(cur_p as usize) {
                        break;
                    }
                    if is_char_node(q) {
                        break;
                    }
                    if is_non_discardable_node(q) {
                        break;
                    }
                    if NODE_type(q as usize) == KERN_NODE
                        && kern_NODE_subtype(q as usize) != KernNodeSubType::Explicit
                        && kern_NODE_subtype(q as usize) != KernNodeSubType::SpaceAdjustment
                    {
                        break;
                    }
                    r = q as usize;
                    if NODE_type(q as usize) == MATH_NODE && *INTPAR(IntPar::texxet) > 0 {
                        /*1495:*/
                        if MEM[q as usize].b16.s0 as i32 & 1i32 != 0 {
                            if LR_ptr != TEX_NULL
                                && MEM[LR_ptr as usize].b32.s0
                                    == (L_CODE as i32)
                                        * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32))
                                        + 3
                            {
                                temp_ptr = LR_ptr;
                                LR_ptr = MEM[temp_ptr as usize].b32.s1;
                                MEM[temp_ptr as usize].b32.s1 = avail;
                                avail = temp_ptr
                            }
                        } else {
                            temp_ptr = get_avail() as i32;
                            MEM[temp_ptr as usize].b32.s0 = (L_CODE as i32)
                                * (MEM[q as usize].b16.s0 as i32 / (L_CODE as i32))
                                + 3;
                            MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                            LR_ptr = temp_ptr
                        }
                    }
                }
                if r != TEMP_HEAD {
                    *LLIST_link(r as usize) = TEX_NULL;
                    flush_node_list(LLIST_link(TEMP_HEAD).opt());
                    *LLIST_link(TEMP_HEAD) = q;
                }
            }
        }
        if !(cur_p != TEX_NULL) {
            break;
        }
    }
    if cur_line != best_line || *LLIST_link(TEMP_HEAD) != TEX_NULL {
        confusion(b"line breaking");
    }
    cur_list.prev_graf = best_line - 1;
    cur_list.eTeX_aux = LR_ptr.opt();
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
unsafe fn try_break(mut pi: i32, mut break_type: small_number) {
    let mut current_block: u64;
    let mut r: i32 = 0;
    let mut prev_r: i32 = 0;
    let mut old_l: i32 = 0;
    let mut no_break_yet: bool = false;
    let mut prev_prev_r: i32 = TEX_NULL;
    let mut s: i32 = 0;
    let mut v: i32 = 0;
    let mut t: i32 = 0;
    let mut f: usize = 0;
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
    if semantic_pagination_enabled as i32 != 0 && cur_p != TEX_NULL {
        return;
    }
    if pi.abs() >= INF_PENALTY {
        if pi > 0 {
            return;
        }
        pi = EJECT_PENALTY
    }
    no_break_yet = true;
    prev_r = ACTIVE_LIST as i32;
    old_l = 0;
    cur_active_width[1] = active_width[1];
    cur_active_width[2] = active_width[2];
    cur_active_width[3] = active_width[3];
    cur_active_width[4] = active_width[4];
    cur_active_width[5] = active_width[5];
    cur_active_width[6] = active_width[6];
    loop {
        r = *LLIST_link(prev_r as usize);
        /*861: "If node r is of type delta_node, update cur_active_width, set
         * prev_r and prev_prev_r, then goto continue" */
        if NODE_type(r as usize) == DELTA_NODE {
            cur_active_width[1] += *DELTA_NODE_dwidth(r as usize);
            cur_active_width[2] += *DELTA_NODE_dstretch0(r as usize);
            cur_active_width[3] += *DELTA_NODE_dstretch1(r as usize);
            cur_active_width[4] += *DELTA_NODE_dstretch2(r as usize);
            cur_active_width[5] += *DELTA_NODE_dstretch3(r as usize);
            cur_active_width[6] += *DELTA_NODE_dshrink(r as usize);
            prev_prev_r = prev_r;
            prev_r = r
        } else {
            /*864: "If a line number class has ended, create new active nodes for
             * the best feasible breaks in that class; then return if r =
             * last_active, otherwise compute the new line_width." */
            l = *ACTIVE_NODE_line_number(r as usize);

            if l > old_l {
                /* "now we are no longer in the inner loop" */
                if minimum_demerits < AWFUL_BAD && (old_l != easy_line || r == LAST_ACTIVE as i32) {
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
                        if break_type > UNHYPHENATED {
                            /*869: "Compute the discretionary break_width values" */
                            if cur_p != TEX_NULL {
                                t = *DISCRETIONARY_NODE_replace_count(cur_p as usize) as i32;
                                v = cur_p;
                                s = *DISCRETIONARY_NODE_post_break(cur_p as usize);
                                while t > 0 {
                                    t -= 1;
                                    v = *LLIST_link(v as usize);
                                    /*870: "subtract the width of node v from break_width" */
                                    if is_char_node(v) {
                                        let mut eff_char: i32 = 0;

                                        f = *CHAR_NODE_font(v as usize) as usize;
                                        eff_char = effective_char(
                                            true,
                                            f,
                                            *CHAR_NODE_character(v as usize),
                                        );
                                        break_width[1] -=
                                            *FONT_CHARACTER_WIDTH(f, eff_char as usize);
                                    } else {
                                        match NODE_type(v as usize) {
                                            LIGATURE_NODE => {
                                                let mut eff_char_0: i32 = 0;
                                                f = *LIGATURE_NODE_lig_font(v as usize) as usize;
                                                xtx_ligature_present = true;
                                                eff_char_0 = effective_char(
                                                    true,
                                                    f,
                                                    *LIGATURE_NODE_lig_char(v as usize),
                                                );
                                                break_width[1] -=
                                                    *FONT_CHARACTER_WIDTH(f, eff_char_0 as usize);
                                            }
                                            HLIST_NODE | VLIST_NODE | RULE_NODE | KERN_NODE => {
                                                break_width[1] -= *BOX_width(v as usize);
                                            }
                                            WHATSIT_NODE => {
                                                if NODE_subtype(v as usize) == NATIVE_WORD_NODE
                                                    || NODE_subtype(v as usize)
                                                        == NATIVE_WORD_NODE_AT
                                                    || NODE_subtype(v as usize) == GLYPH_NODE
                                                    || NODE_subtype(v as usize) == PIC_NODE
                                                    || NODE_subtype(v as usize) == PDF_NODE
                                                {
                                                    break_width[1] -= *BOX_width(v as usize);
                                                } else {
                                                    confusion(b"disc1a");
                                                }
                                            }
                                            _ => confusion(b"disc1"),
                                        }
                                    }
                                }
                                /*871: "add the width of node s to break_width" */
                                while s != TEX_NULL {
                                    if is_char_node(s) {
                                        let mut eff_char_1: i32 = 0;
                                        f = *CHAR_NODE_font(s as usize) as usize;
                                        eff_char_1 =
                                            effective_char(true, f, MEM[s as usize].b16.s0);
                                        break_width[1] +=
                                            *FONT_CHARACTER_WIDTH(f, eff_char_1 as usize);
                                    } else {
                                        match ND::from(MEM[s as usize].b16.s1) {
                                            LIGATURE_NODE => {
                                                let mut eff_char_2: i32 = 0;
                                                f = *LIGATURE_NODE_lig_font(s as usize) as usize;
                                                xtx_ligature_present = true;
                                                eff_char_2 = effective_char(
                                                    true,
                                                    f,
                                                    *LIGATURE_NODE_lig_char(s as usize),
                                                );
                                                break_width[1] +=
                                                    *FONT_CHARACTER_WIDTH(f, eff_char_2 as usize);
                                            }
                                            HLIST_NODE | VLIST_NODE | RULE_NODE | KERN_NODE => {
                                                break_width[1] += *BOX_width(s as usize);
                                            }
                                            WHATSIT_NODE => {
                                                if NODE_subtype(s as usize) == NATIVE_WORD_NODE
                                                    || NODE_subtype(s as usize)
                                                        == NATIVE_WORD_NODE_AT
                                                    || NODE_subtype(s as usize) == GLYPH_NODE
                                                    || NODE_subtype(s as usize) == PIC_NODE
                                                    || NODE_subtype(s as usize) == PDF_NODE
                                                {
                                                    break_width[1] += *BOX_width(s as usize);
                                                } else {
                                                    confusion(b"disc2a");
                                                }
                                            }
                                            _ => confusion(b"disc2"),
                                        }
                                    }
                                    s = *LLIST_link(s as usize);
                                }
                                break_width[1] += disc_width;
                                if *DISCRETIONARY_NODE_post_break(cur_p as usize) == TEX_NULL {
                                    s = *LLIST_link(v as usize);
                                }
                            }
                        }
                        while s != TEX_NULL {
                            if is_char_node(s) {
                                break;
                            }
                            match NODE_type(s as usize) {
                                GLUE_NODE => {
                                    v = *GLUE_NODE_glue_ptr(s as usize);
                                    break_width[1] -= *BOX_width(v as usize);
                                    break_width
                                        [2 + *GLUE_SPEC_stretch_order(v as usize) as usize] -=
                                        *GLUE_SPEC_stretch(v as usize);
                                    break_width[6] -= *GLUE_SPEC_shrink(v as usize);
                                }
                                PENALTY_NODE => {}
                                MATH_NODE => break_width[1] -= *BOX_width(s as usize),
                                KERN_NODE => {
                                    if kern_NODE_subtype(s as usize) != KernNodeSubType::Explicit {
                                        break;
                                    }
                                    break_width[1] -= *BOX_width(s as usize)
                                }
                                _ => break,
                            }
                            s = *LLIST_link(s as usize);
                        }
                    }
                    /*872: "Insert a delta node to prepare for breaks at cur_p" */
                    if MEM[prev_r as usize].b16.s1 as i32 == 2 {
                        /* this is unused */
                        *DELTA_NODE_dwidth(prev_r as usize) +=
                            -cur_active_width[1] + break_width[1];
                        *DELTA_NODE_dstretch0(prev_r as usize) +=
                            -cur_active_width[2] + break_width[2];
                        *DELTA_NODE_dstretch1(prev_r as usize) +=
                            -cur_active_width[3] + break_width[3];
                        *DELTA_NODE_dstretch2(prev_r as usize) +=
                            -cur_active_width[4] + break_width[4];
                        *DELTA_NODE_dstretch3(prev_r as usize) +=
                            -cur_active_width[5] + break_width[5];
                        *DELTA_NODE_dshrink(prev_r as usize) +=
                            -cur_active_width[6] + break_width[6]
                    } else if prev_r == ACTIVE_LIST as i32 {
                        active_width[1] = break_width[1];
                        active_width[2] = break_width[2];
                        active_width[3] = break_width[3];
                        active_width[4] = break_width[4];
                        active_width[5] = break_width[5];
                        active_width[6] = break_width[6]
                    } else {
                        let q = get_node(DELTA_NODE_SIZE);
                        *LLIST_link(q) = r;
                        set_NODE_type(q, DELTA_NODE);
                        clear_NODE_subtype(q);
                        *DELTA_NODE_dwidth(q) = break_width[1] - cur_active_width[1];
                        *DELTA_NODE_dstretch0(q) = break_width[2] - cur_active_width[2];
                        *DELTA_NODE_dstretch1(q) = break_width[3] - cur_active_width[3];
                        *DELTA_NODE_dstretch2(q) = break_width[4] - cur_active_width[4];
                        *DELTA_NODE_dstretch3(q) = break_width[5] - cur_active_width[5];
                        *DELTA_NODE_dshrink(q) = break_width[6] - cur_active_width[6];
                        *LLIST_link(prev_r as usize) = q as i32;
                        prev_prev_r = prev_r;
                        prev_r = q as i32;
                    }
                    /* ... resuming 865 ... */
                    if (*INTPAR(IntPar::adj_demerits)).abs() >= MAX_HALFWORD - minimum_demerits {
                        minimum_demerits = AWFUL_BAD - 1;
                    } else {
                        minimum_demerits = minimum_demerits + (*INTPAR(IntPar::adj_demerits)).abs()
                    }
                    fit_class = VERY_LOOSE_FIT;
                    while fit_class <= TIGHT_FIT {
                        if minimal_demerits[fit_class as usize] <= minimum_demerits {
                            /*874: "Insert a new active node from best_place[fit_class] to cur_p" */
                            let q = get_node(PASSIVE_NODE_SIZE);
                            *LLIST_link(q) = passive;
                            passive = q as i32;
                            *PASSIVE_NODE_cur_break(q) = cur_p;
                            *PASSIVE_NODE_prev_break(q) = best_place[fit_class as usize];

                            let q = get_node(active_node_size as i32);
                            *ACTIVE_NODE_break_node(q) = passive;
                            *ACTIVE_NODE_line_number(q) = best_pl_line[fit_class as usize] + 1;
                            *ACTIVE_NODE_fitness(q) = fit_class as u16;
                            MEM[q as usize].b16.s1 = break_type as u16; //set_NODE_type(q, break_type as u16);
                            *ACTIVE_NODE_total_demerits(q) = minimal_demerits[fit_class as usize];

                            if do_last_line_fit {
                                /*1639: */
                                *ACTIVE_NODE_shortfall(q) = best_pl_short[fit_class as usize];
                                *ACTIVE_NODE_glue(q) = best_pl_glue[fit_class as usize];
                            }
                            *LLIST_link(q) = r;
                            *LLIST_link(prev_r as usize) = q as i32;
                            prev_r = q as i32;
                        }
                        minimal_demerits[fit_class as usize] = MAX_HALFWORD;
                        fit_class = fit_class.wrapping_add(1)
                    }
                    minimum_demerits = MAX_HALFWORD;
                    /*873: "Insert a delta node to prepare for the next active node" */
                    if r != LAST_ACTIVE as i32 {
                        let q = get_node(DELTA_NODE_SIZE);
                        *LLIST_link(q) = r;
                        set_NODE_type(q, DELTA_NODE);
                        clear_NODE_subtype(q); /* subtype is not used */
                        *DELTA_NODE_dwidth(q) = cur_active_width[1] - break_width[1];
                        *DELTA_NODE_dstretch0(q) = cur_active_width[2] - break_width[2];
                        *DELTA_NODE_dstretch1(q) = cur_active_width[3] - break_width[3];
                        *DELTA_NODE_dstretch2(q) = cur_active_width[4] - break_width[4];
                        *DELTA_NODE_dstretch3(q) = cur_active_width[5] - break_width[5];
                        *DELTA_NODE_dshrink(q) = cur_active_width[6] - break_width[6];
                        *LLIST_link(prev_r as usize) = q as i32;
                        prev_prev_r = prev_r;
                        prev_r = q as i32;
                    }
                }
                /* ... resuming 864 ... */
                if r == LAST_ACTIVE as i32 {
                    return;
                }
                /*879: "Compute the new line width" */
                if l > easy_line {
                    line_width = second_width;
                    old_l = MAX_HALFWORD - 1;
                } else {
                    old_l = l;
                    if l > last_special_line {
                        line_width = second_width
                    } else if *LOCAL(Local::par_shape) == TEX_NULL {
                        line_width = first_width
                    } else {
                        line_width = MEM[(*LOCAL(Local::par_shape) + 2 * l) as usize].b32.s1
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
                artificial_demerits = true;
                shortfall = 0;
            } else {
                artificial_demerits = false;
                shortfall = line_width - cur_active_width[1];
                if *INTPAR(IntPar::xetex_protrude_chars) > 1 {
                    shortfall = shortfall + total_pw(r, cur_p)
                }
            }
            if shortfall > 0 {
                /*881: "Set the value of b to the badness for stretching the line,
                 * and compute the corresponding fit_class" */
                if cur_active_width[3] != 0 || cur_active_width[4] != 0 || cur_active_width[5] != 0
                {
                    if do_last_line_fit {
                        if cur_p == TEX_NULL {
                            /*1634: "Perform computations for the last line and goto found" */
                            if *ACTIVE_NODE_shortfall(r as usize) == 0
                                || *ACTIVE_NODE_glue(r as usize) <= 0
                            {
                                current_block = 5565703735569783978;
                            } else if cur_active_width[3] != fill_width[0]
                                || cur_active_width[4] != fill_width[1]
                                || cur_active_width[5] != fill_width[2]
                            {
                                current_block = 5565703735569783978;
                            } else {
                                if *ACTIVE_NODE_shortfall(r as usize) > 0 {
                                    g = cur_active_width[2]
                                } else {
                                    g = cur_active_width[6]
                                }
                                if g <= 0 {
                                    current_block = 5565703735569783978;
                                } else {
                                    arith_error = false;
                                    g = fract(
                                        g,
                                        *ACTIVE_NODE_shortfall(r as usize),
                                        *ACTIVE_NODE_glue(r as usize),
                                        MAX_HALFWORD,
                                    );
                                    if *INTPAR(IntPar::last_line_fit) < 1000 {
                                        g = fract(
                                            g,
                                            *INTPAR(IntPar::last_line_fit),
                                            1000,
                                            MAX_HALFWORD,
                                        )
                                    }
                                    if arith_error {
                                        if *ACTIVE_NODE_shortfall(r as usize) > 0 {
                                            g = MAX_HALFWORD
                                        } else {
                                            g = -MAX_HALFWORD
                                        }
                                    }
                                    if g > 0 {
                                        /*1635: "Set the value of b to the badness of the
                                         * last line for stretching, compute the
                                         * corresponding fit_class, and goto found" */
                                        if g > shortfall {
                                            g = shortfall
                                        }
                                        if g > 7230584 {
                                            /* XXX: magic number in original WEB code */
                                            if (cur_active_width[2] as i64) < 1663497 {
                                                /* XXX: magic number in original WEB code */
                                                b = INF_BAD;
                                                fit_class = VERY_LOOSE_FIT;
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
                                                if b > 12 {
                                                    if b > 99 {
                                                        fit_class = VERY_LOOSE_FIT
                                                    } else {
                                                        fit_class = LOOSE_FIT
                                                    }
                                                } else {
                                                    fit_class = DECENT_FIT
                                                }
                                                current_block = 11849408527845460430;
                                            }
                                        }
                                    } else if g < 0 {
                                        /*1636: "Set the value of b to the badness of the
                                         * last line for shrinking, compute the
                                         * corresponding fit_class, and goto found" */
                                        if -g > cur_active_width[6] {
                                            g = -cur_active_width[6]
                                        }
                                        b = badness(-g, cur_active_width[6]);
                                        if b > 12 {
                                            /* XXX hardcoded in WEB */
                                            fit_class = TIGHT_FIT
                                        } else {
                                            fit_class = DECENT_FIT
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
                        if cur_p == TEX_NULL {
                            shortfall = 0
                        }
                        if shortfall > 0 {
                            g = cur_active_width[2]
                        } else if shortfall < 0 {
                            g = cur_active_width[6]
                        } else {
                            g = 0
                        }
                    }
                }
                _ => {}
            }
            if b > INF_BAD || pi == EJECT_PENALTY {
                /*883: "Prepare to deactivate node r, and goto deactivate unless
                 * there is a reason to consider lines of text from r to cur_p" */
                if final_pass as i32 != 0
                    && minimum_demerits == AWFUL_BAD
                    && *LLIST_link(r as usize) == LAST_ACTIVE as i32
                    && prev_r == ACTIVE_LIST as i32
                {
                    artificial_demerits = true;
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
                node_r_stays_active = true;
                current_block = 14114736409816581360;
            }
            match current_block {
                14114736409816581360 => {
                    if artificial_demerits {
                        d = 0
                    } else {
                        /*888: "Compute the demerits, d, from r to cur_p" */
                        d = *INTPAR(IntPar::line_penalty) + b;
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
                        if break_type == HYPHENATED
                            && NODE_type(r as usize).u16() == HYPHENATED as u16
                        {
                            if !cur_p.is_texnull() {
                                d = d + *INTPAR(IntPar::double_hyphen_demerits);
                            } else {
                                d = d + *INTPAR(IntPar::final_hyphen_demerits);
                            }
                        }
                        if (fit_class as i32 - *ACTIVE_NODE_fitness(r as usize) as i32).abs() > 1 {
                            d = d + *INTPAR(IntPar::adj_demerits);
                        }
                    }
                    /* resuming 884: */
                    d = d + *ACTIVE_NODE_total_demerits(r as usize);
                    if d <= minimal_demerits[fit_class as usize] {
                        minimal_demerits[fit_class as usize] = d;
                        best_place[fit_class as usize] = *ACTIVE_NODE_break_node(r as usize);
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
            *LLIST_link(prev_r as usize) = *LLIST_link(r as usize);
            free_node(r as usize, active_node_size as i32);
            if prev_r == ACTIVE_LIST as i32 {
                /*890: "Update the active widths, since the first active node has been deleted" */
                r = *LLIST_link(ACTIVE_LIST); /*:966 */
                if NODE_type(r as usize) == DELTA_NODE {
                    active_width[1] += *DELTA_NODE_dwidth(r as usize);
                    active_width[2] += *DELTA_NODE_dstretch0(r as usize);
                    active_width[3] += *DELTA_NODE_dstretch1(r as usize);
                    active_width[4] += *DELTA_NODE_dstretch2(r as usize);
                    active_width[5] += *DELTA_NODE_dstretch3(r as usize);
                    active_width[6] += *DELTA_NODE_dshrink(r as usize);
                    cur_active_width[1] = active_width[1];
                    cur_active_width[2] = active_width[2];
                    cur_active_width[3] = active_width[3];
                    cur_active_width[4] = active_width[4];
                    cur_active_width[5] = active_width[5];
                    cur_active_width[6] = active_width[6];
                    *LLIST_link(ACTIVE_LIST) = *LLIST_link(r as usize);
                    free_node(r as usize, DELTA_NODE_SIZE);
                }
            } else if NODE_type(prev_r as usize) == DELTA_NODE {
                r = *LLIST_link(prev_r as usize);

                let r = r as usize;
                if r == LAST_ACTIVE {
                    cur_active_width[1] -= *DELTA_NODE_dwidth(prev_r as usize);
                    cur_active_width[2] -= *DELTA_NODE_dstretch0(prev_r as usize);
                    cur_active_width[3] -= *DELTA_NODE_dstretch1(prev_r as usize);
                    cur_active_width[4] -= *DELTA_NODE_dstretch2(prev_r as usize);
                    cur_active_width[5] -= *DELTA_NODE_dstretch3(prev_r as usize);
                    cur_active_width[6] -= *DELTA_NODE_dshrink(prev_r as usize);
                    *LLIST_link(prev_prev_r as usize) = LAST_ACTIVE as i32;
                    free_node(prev_r as usize, DELTA_NODE_SIZE);
                    prev_r = prev_prev_r
                } else if NODE_type(r) == DELTA_NODE {
                    cur_active_width[1] += *DELTA_NODE_dwidth(r);
                    cur_active_width[2] += *DELTA_NODE_dstretch0(r);
                    cur_active_width[3] += *DELTA_NODE_dstretch1(r);
                    cur_active_width[4] += *DELTA_NODE_dstretch2(r);
                    cur_active_width[5] += *DELTA_NODE_dstretch3(r);
                    cur_active_width[6] += *DELTA_NODE_dshrink(r);
                    *DELTA_NODE_dwidth(prev_r as usize) += *DELTA_NODE_dwidth(r);
                    *DELTA_NODE_dstretch0(prev_r as usize) += *DELTA_NODE_dstretch0(r);
                    *DELTA_NODE_dstretch2(prev_r as usize) += *DELTA_NODE_dstretch1(r);
                    *DELTA_NODE_dstretch2(prev_r as usize) += *DELTA_NODE_dstretch2(r);
                    *DELTA_NODE_dstretch3(prev_r as usize) += *DELTA_NODE_dstretch3(r);
                    *DELTA_NODE_dshrink(prev_r as usize) += *DELTA_NODE_dshrink(r);
                    *LLIST_link(prev_r as usize) = *LLIST_link(r);
                    free_node(r, DELTA_NODE_SIZE);
                }
            }
        }
    }
}
unsafe fn hyphenate() {
    let mut current_block: u64;
    let mut i: i16 = 0;
    let mut l: i16 = 0;
    let mut s: i32 = 0;
    let mut bchar: i32 = 0;
    let mut major_tail: i32 = 0;
    let mut minor_tail: i32 = 0;
    let mut c: UnicodeScalar = 0i32;
    let mut c_loc: i16 = 0;
    let mut r_count: i32 = 0;
    let mut z: trie_pointer = 0;
    let mut v: i32 = 0;
    let mut u: pool_pointer = 0;

    let mut j = 0_i16;
    let mut for_end = hn as i32;
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
    let mut h = hc[1] as hyph_pointer;
    hn += 1;
    hc[hn as usize] = cur_lang as i32;
    let mut for_end_0: i32 = 0;
    j = 2_i16;
    for_end_0 = hn as i32;
    if j as i32 <= for_end_0 {
        loop {
            h = ((h as i32 + h as i32 + hc[j as usize]) % HYPH_PRIME) as hyph_pointer;
            let fresh19 = j;
            j = j + 1;
            if !((fresh19 as i32) < for_end_0) {
                break;
            }
        }
    }
    loop {
        let k = HYPH_WORD[h as usize];
        if k == 0 {
            current_block = 10027897684796195291;
            break;
        }
        if length(k) == hn as i32 {
            j = 1_i16;
            u = str_start[(k as i64 - 65536) as usize];
            loop {
                if str_pool[u as usize] as i32 != hc[j as usize] {
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
                    while s != TEX_NULL {
                        hyf[MEM[s as usize].b32.s0 as usize] = 1_u8;
                        s = *LLIST_link(s as usize);
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
            hc[0] = 0;
            hc[(hn as i32 + 1i32) as usize] = 0i32;
            hc[(hn as i32 + 2i32) as usize] = max_hyph_char;
            let mut for_end_1: i32 = 0;
            j = 0_i16;
            for_end_1 = hn as i32 - r_hyf + 1;
            if j as i32 <= for_end_1 {
                loop {
                    z = *trie_trl.offset((cur_lang as i32 + 1) as isize) + hc[j as usize];
                    l = j;
                    while hc[l as usize] == *trie_trc.offset(z as isize) as i32 {
                        if *trie_tro.offset(z as isize) != MIN_TRIE_OP {
                            /*959: */
                            v = *trie_tro.offset(z as isize); /*:958 */
                            loop {
                                v = v + op_start[cur_lang as usize];
                                i = (l as i32 - hyf_distance[v as usize] as i32) as i16;
                                if hyf_num[v as usize] as i32 > hyf[i as usize] as i32 {
                                    hyf[i as usize] = hyf_num[v as usize] as u8
                                }
                                v = hyf_next[v as usize] as i32;
                                if v == MIN_TRIE_OP {
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
    for_end_2 = l_hyf - 1;
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
    for_end_3 = r_hyf - 1;
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
    if ha != TEX_NULL
        && !is_char_node(ha)
        && NODE_type(ha as usize) == WHATSIT_NODE
        && (NODE_subtype(ha as usize) == NATIVE_WORD_NODE
            || NODE_subtype(ha as usize) == NATIVE_WORD_NODE_AT)
    {
        s = cur_p;
        while MEM[s as usize].b32.s1 != ha {
            s = MEM[s as usize].b32.s1
        }
        hyphen_passed = 0;
        let mut for_end_5: i32 = 0;
        j = l_hyf as i16;
        for_end_5 = hn as i32 - r_hyf;
        if j as i32 <= for_end_5 {
            loop {
                if hyf[j as usize] as i32 & 1i32 != 0 {
                    let q = new_native_word_node(hf, j as i32 - hyphen_passed as i32);
                    MEM[q].b16.s0 = MEM[ha as usize].b16.s0;
                    let mut for_end_6: i32 = 0;
                    i = 0_i16;
                    for_end_6 = j as i32 - hyphen_passed as i32 - 1;
                    if i as i32 <= for_end_6 {
                        loop {
                            *(&mut MEM[q + 6] as *mut memory_word as *mut u16).offset(i as isize) =
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
                        &mut MEM[q] as *mut memory_word as *mut libc::c_void,
                        (*INTPAR(IntPar::xetex_use_glyph_metrics) > 0) as i32,
                    );
                    MEM[s as usize].b32.s1 = q as i32;
                    s = q as i32;
                    let q = new_disc();
                    MEM[q + 1].b32.s0 = new_native_character(hf, hyf_char) as i32;
                    MEM[s as usize].b32.s1 = q as i32;
                    s = q as i32;
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
        let q = new_native_word_node(hf, hn as i32 - hyphen_passed as i32);
        MEM[q].b16.s0 = MEM[ha as usize].b16.s0;
        let mut for_end_7: i32 = 0;
        i = 0_i16;
        for_end_7 = hn as i32 - hyphen_passed as i32 - 1;
        if i as i32 <= for_end_7 {
            loop {
                *(&mut MEM[q + 6] as *mut memory_word as *mut u16).offset(i as isize) =
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
            &mut MEM[q] as *mut memory_word as *mut libc::c_void,
            (*INTPAR(IntPar::xetex_use_glyph_metrics) > 0i32) as i32,
        );
        MEM[s as usize].b32.s1 = q as i32;
        s = q as i32;
        let q = MEM[ha as usize].b32.s1;
        MEM[s as usize].b32.s1 = q;
        MEM[ha as usize].b32.s1 = TEX_NULL;
        flush_node_list(ha.opt());
    } else {
        let q = MEM[hb as usize].b32.s1;
        MEM[hb as usize].b32.s1 = TEX_NULL;
        let r = MEM[ha as usize].b32.s1;
        MEM[ha as usize].b32.s1 = TEX_NULL;
        bchar = hyf_bchar;
        if is_char_node(ha) {
            if MEM[ha as usize].b16.s1 as usize != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = ha;
                init_lig = false;
                hu[0] = MEM[ha as usize].b16.s0 as i32;
                current_block = 6662862405959679103;
            }
        } else if NODE_type(ha as usize) == LIGATURE_NODE {
            if MEM[(ha + 1) as usize].b16.s1 as usize != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = MEM[(ha + 1) as usize].b32.s1;
                init_lig = true;
                init_lft = MEM[ha as usize].b16.s0 as i32 > 1;
                hu[0] = MEM[(ha + 1) as usize].b16.s0 as i32;
                if init_list == TEX_NULL {
                    if init_lft {
                        hu[0] = max_hyph_char;
                        init_lig = false
                    }
                }
                free_node(ha as usize, SMALL_NODE_SIZE);
                current_block = 6662862405959679103;
            }
        } else {
            if !is_char_node(r) {
                if NODE_type(r as usize) == LIGATURE_NODE {
                    if MEM[r as usize].b16.s0 > 1 {
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
                    init_list = TEX_NULL;
                    current_block = 5209103994167801282;
                }
            }
        }
        match current_block {
            6662862405959679103 => {
                s = cur_p;
                while MEM[s as usize].b32.s1 != ha {
                    s = *LLIST_link(s as usize);
                }
                j = 0_i16
            }
            6826215413708131726 => {
                s = ha;
                j = 0_i16;
                hu[0] = max_hyph_char;
                init_lig = false;
                init_list = TEX_NULL
            }
            _ => {}
        }
        flush_node_list(r.opt());
        loop {
            l = j;
            j = (reconstitute(j, hn, bchar, hyf_char) as i32 + 1) as i16;
            if hyphen_passed == 0 {
                MEM[s as usize].b32.s1 = MEM[HOLD_HEAD].b32.s1;
                while MEM[s as usize].b32.s1 > TEX_NULL {
                    s = MEM[s as usize].b32.s1
                }
                if hyf[(j as i32 - 1i32) as usize] as i32 & 1i32 != 0 {
                    l = j;
                    hyphen_passed = (j as i32 - 1i32) as small_number;
                    MEM[HOLD_HEAD].b32.s1 = TEX_NULL
                }
            }
            if hyphen_passed as i32 > 0 {
                loop
                /*949: */
                {
                    let r = get_node(SMALL_NODE_SIZE);
                    MEM[r].b32.s1 = MEM[HOLD_HEAD].b32.s1;
                    set_NODE_type(r, DISC_NODE);
                    major_tail = r as i32;
                    r_count = 0;
                    while MEM[major_tail as usize].b32.s1 > TEX_NULL {
                        major_tail = *LLIST_link(major_tail as usize);
                        r_count += 1;
                    }
                    i = hyphen_passed;
                    hyf[i as usize] = 0;
                    minor_tail = TEX_NULL;
                    MEM[r + 1].b32.s0 = TEX_NULL;
                    let hyf_node = new_character(hf, hyf_char as UTF16_code);
                    if let Some(hyf_node) = hyf_node {
                        i += 1;
                        c = hu[i as usize];
                        hu[i as usize] = hyf_char;
                        MEM[hyf_node].b32.s1 = avail;
                        avail = hyf_node as i32;
                    }
                    while l as i32 <= i as i32 {
                        l = (reconstitute(l, i, FONT_BCHAR[hf as usize], TOO_BIG_CHAR) as i32 + 1)
                            as i16;
                        if MEM[HOLD_HEAD].b32.s1 > TEX_NULL {
                            if minor_tail == TEX_NULL {
                                MEM[r + 1].b32.s0 = MEM[HOLD_HEAD].b32.s1
                            } else {
                                MEM[minor_tail as usize].b32.s1 = MEM[HOLD_HEAD].b32.s1
                            }
                            minor_tail = MEM[HOLD_HEAD].b32.s1;
                            while MEM[minor_tail as usize].b32.s1 > TEX_NULL {
                                minor_tail = *LLIST_link(minor_tail as usize);
                            }
                        }
                    }
                    if hyf_node.is_some() {
                        hu[i as usize] = c;
                        l = i;
                        i -= 1
                    }
                    minor_tail = TEX_NULL;
                    MEM[r + 1].b32.s1 = TEX_NULL;
                    c_loc = 0_i16;
                    if BCHAR_LABEL[hf as usize] != NON_ADDRESS {
                        l -= 1;
                        c = hu[l as usize];
                        c_loc = l;
                        hu[l as usize] = max_hyph_char
                    }
                    while (l as i32) < j as i32 {
                        loop {
                            l = (reconstitute(l, hn, bchar, TOO_BIG_CHAR) as i32 + 1) as i16;
                            if c_loc > 0 {
                                hu[c_loc as usize] = c;
                                c_loc = 0_i16
                            }
                            if MEM[HOLD_HEAD].b32.s1 > TEX_NULL {
                                if minor_tail == TEX_NULL {
                                    MEM[r + 1].b32.s1 = MEM[HOLD_HEAD].b32.s1
                                } else {
                                    MEM[minor_tail as usize].b32.s1 = MEM[HOLD_HEAD].b32.s1
                                }
                                minor_tail = MEM[HOLD_HEAD].b32.s1;
                                while MEM[minor_tail as usize].b32.s1 > TEX_NULL {
                                    minor_tail = *LLIST_link(minor_tail as usize);
                                }
                            }
                            if l as i32 >= j as i32 {
                                break;
                            }
                        }
                        while l as i32 > j as i32 {
                            /*952: */
                            j = (reconstitute(j, hn, bchar, TOO_BIG_CHAR) as i32 + 1i32) as i16; /*:944*/
                            MEM[major_tail as usize].b32.s1 = MEM[HOLD_HEAD].b32.s1;
                            while MEM[major_tail as usize].b32.s1 > TEX_NULL {
                                major_tail = *LLIST_link(major_tail as usize);
                                r_count += 1;
                            }
                        }
                    }
                    if r_count > 127 {
                        MEM[s as usize].b32.s1 = MEM[r].b32.s1;
                        MEM[r].b32.s1 = TEX_NULL;
                        flush_node_list(Some(r));
                    } else {
                        MEM[s as usize].b32.s1 = r as i32;
                        MEM[r].b16.s0 = r_count as u16;
                    }
                    s = major_tail;
                    hyphen_passed = (j - 1) as small_number;
                    MEM[HOLD_HEAD].b32.s1 = TEX_NULL;
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
        flush_list(init_list.opt());
    };
}
unsafe fn finite_shrink(p: usize) -> usize {
    if no_shrink_error_yet {
        no_shrink_error_yet = false;
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Infinite glue shrinkage found in a paragraph");
        help_ptr = 5;
        help_line[4] = b"The paragraph just ended includes some glue that has";
        help_line[3] = b"infinite shrinkability, e.g., `\\hskip 0pt minus 1fil\'.";
        help_line[2] = b"Such glue doesn\'t belong there---it allows a paragraph";
        help_line[1] = b"of any length to fit on one line. But it\'s safe to proceed,";
        help_line[0] = b"since the offensive shrinkability has been made finite.";
        error();
    }
    let q = new_spec(p);
    MEM[q].b16.s0 = NORMAL;
    delete_glue_ref(p);
    q
}
unsafe fn reconstitute(
    mut j: small_number,
    mut n: small_number,
    mut bchar: i32,
    mut hchar: i32,
) -> small_number {
    let mut current_block: u64;
    let mut p: i32 = 0;
    let mut q: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut cur_rh: i32 = 0;
    let mut test_char: i32 = 0;
    let mut k: font_index = 0;

    hyphen_passed = 0;
    let mut t = HOLD_HEAD as i32;
    let mut w = 0;
    MEM[HOLD_HEAD].b32.s1 = TEX_NULL;
    cur_l = hu[j as usize];
    cur_q = t;
    if j == 0 {
        ligature_present = init_lig;
        p = init_list;
        if ligature_present {
            lft_hit = init_lft
        }
        while p > TEX_NULL {
            MEM[t as usize].b32.s1 = get_avail() as i32;
            t = MEM[t as usize].b32.s1;
            MEM[t as usize].b16.s1 = hf as u16;
            MEM[t as usize].b16.s0 = MEM[p as usize].b16.s0;
            p = *LLIST_link(p as usize)
        }
    } else if cur_l < TOO_BIG_CHAR {
        MEM[t as usize].b32.s1 = get_avail() as i32;
        t = *LLIST_link(t as usize);
        MEM[t as usize].b16.s1 = hf as u16;
        MEM[t as usize].b16.s0 = cur_l as u16
    }
    lig_stack = TEX_NULL;
    if (j as i32) < n as i32 {
        cur_r = hu[(j + 1) as usize]
    } else {
        cur_r = bchar
    }
    if hyf[j as usize] as i32 & 1i32 != 0 {
        cur_rh = hchar
    } else {
        cur_rh = TOO_BIG_CHAR
    }
    'c_27176: loop {
        if cur_l == TOO_BIG_CHAR {
            k = BCHAR_LABEL[hf as usize];
            if k == NON_ADDRESS {
                current_block = 4939169394500275451;
            } else {
                q = FONT_INFO[k as usize].b16;
                current_block = 1434579379687443766;
            }
        } else {
            q = FONT_CHARACTER_INFO(hf, effective_char(true, hf, cur_l as u16) as usize);
            if q.s1 as i32 % 4i32 != LIG_TAG {
                current_block = 4939169394500275451;
            } else {
                k = LIG_KERN_BASE[hf as usize] + q.s0 as i32;
                q = FONT_INFO[k as usize].b16;
                if q.s3 as i32 > 128 {
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
                if cur_rh < TOO_BIG_CHAR {
                    test_char = cur_rh
                } else {
                    test_char = cur_r
                }
                loop {
                    if q.s2 as i32 == test_char {
                        if q.s3 <= 128 {
                            if cur_rh < TOO_BIG_CHAR {
                                hyphen_passed = j;
                                hchar = TOO_BIG_CHAR;
                                cur_rh = TOO_BIG_CHAR;
                                continue 'c_27176;
                            } else {
                                if hchar < TOO_BIG_CHAR {
                                    if hyf[j as usize] as i32 & 1i32 != 0 {
                                        hyphen_passed = j;
                                        hchar = TOO_BIG_CHAR
                                    }
                                }
                                if (q.s1 as i32) < 128 {
                                    /*946: */
                                    if cur_l == TOO_BIG_CHAR {
                                        lft_hit = true
                                    }
                                    if j as i32 == n as i32 {
                                        if lig_stack == TEX_NULL {
                                            rt_hit = true
                                        }
                                    }
                                    match q.s1 as i32 {
                                        1 | 5 => {
                                            cur_l = q.s0 as i32;
                                            ligature_present = true
                                        }
                                        2 | 6 => {
                                            cur_r = q.s0 as i32;
                                            if lig_stack > TEX_NULL {
                                                MEM[lig_stack as usize].b16.s0 = cur_r as u16
                                            } else {
                                                lig_stack = new_lig_item(cur_r as u16) as i32;
                                                if j as i32 == n as i32 {
                                                    bchar = TOO_BIG_CHAR
                                                } else {
                                                    p = get_avail() as i32;
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
                                            lig_stack = new_lig_item(cur_r as u16) as i32;
                                            MEM[lig_stack as usize].b32.s1 = p
                                        }
                                        7 | 11 => {
                                            if ligature_present {
                                                p = new_ligature(
                                                    hf,
                                                    cur_l as u16,
                                                    MEM[cur_q as usize].b32.s1,
                                                )
                                                    as i32;
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
                                            ligature_present = true
                                        }
                                        _ => {
                                            cur_l = q.s0 as i32;
                                            ligature_present = true;
                                            if lig_stack > TEX_NULL {
                                                if MEM[(lig_stack + 1) as usize].b32.s1 > TEX_NULL {
                                                    MEM[t as usize].b32.s1 =
                                                        MEM[(lig_stack + 1) as usize].b32.s1;
                                                    t = *LLIST_link(t as usize);
                                                    j += 1
                                                }
                                                p = lig_stack;
                                                lig_stack = MEM[p as usize].b32.s1;
                                                free_node(p as usize, SMALL_NODE_SIZE);
                                                if lig_stack == TEX_NULL {
                                                    if (j as i32) < n as i32 {
                                                        cur_r = hu[(j as i32 + 1i32) as usize]
                                                    } else {
                                                        cur_r = bchar
                                                    }
                                                    if hyf[j as usize] as i32 & 1i32 != 0 {
                                                        cur_rh = hchar
                                                    } else {
                                                        cur_rh = TOO_BIG_CHAR;
                                                    }
                                                } else {
                                                    cur_r = MEM[lig_stack as usize].b16.s0 as i32
                                                }
                                            } else {
                                                if j as i32 == n as i32 {
                                                    break;
                                                }
                                                MEM[t as usize].b32.s1 = get_avail() as i32;
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
                                                    cur_rh = TOO_BIG_CHAR
                                                }
                                            }
                                        }
                                    }
                                    if !(q.s1 as i32 > 4) {
                                        continue 'c_27176;
                                    }
                                    if q.s1 as i32 != 7 {
                                        break;
                                    } else {
                                        continue 'c_27176;
                                    }
                                } else {
                                    w = FONT_INFO[(KERN_BASE[hf as usize]
                                        + 256 * q.s1 as i32
                                        + q.s0 as i32)
                                        as usize]
                                        .b32
                                        .s1;
                                    break;
                                }
                            }
                        }
                    }
                    if q.s3 >= 128 {
                        if cur_rh == TOO_BIG_CHAR {
                            break;
                        }
                        cur_rh = TOO_BIG_CHAR;
                        continue 'c_27176;
                    } else {
                        k = k + q.s3 as i32 + 1;
                        q = FONT_INFO[k as usize].b16
                    }
                }
            }
            _ => {}
        }
        if ligature_present {
            p = new_ligature(hf, cur_l as u16, MEM[cur_q as usize].b32.s1) as i32;
            if lft_hit {
                MEM[p as usize].b16.s0 = 2;
                lft_hit = false
            }
            if rt_hit {
                if lig_stack == TEX_NULL {
                    MEM[p as usize].b16.s0 += 1;
                    rt_hit = false
                }
            }
            MEM[cur_q as usize].b32.s1 = p;
            t = p;
            ligature_present = false
        }
        if w != 0 {
            MEM[t as usize].b32.s1 = new_kern(w) as i32;
            t = *LLIST_link(t as usize);
            w = 0;
            MEM[(t + 2) as usize].b32.s0 = 0
        }
        if !(lig_stack > TEX_NULL) {
            break;
        }
        cur_q = t;
        cur_l = MEM[lig_stack as usize].b16.s0 as i32;
        ligature_present = true;
        if MEM[(lig_stack + 1) as usize].b32.s1 > TEX_NULL {
            MEM[t as usize].b32.s1 = MEM[(lig_stack + 1) as usize].b32.s1;
            t = *LLIST_link(t as usize);
            j += 1;
        }
        p = lig_stack;
        lig_stack = MEM[p as usize].b32.s1;
        free_node(p as usize, SMALL_NODE_SIZE);
        if lig_stack == TEX_NULL {
            if (j as i32) < n as i32 {
                cur_r = hu[(j as i32 + 1i32) as usize]
            } else {
                cur_r = bchar
            }
            if hyf[j as usize] as i32 & 1i32 != 0 {
                cur_rh = hchar
            } else {
                cur_rh = TOO_BIG_CHAR;
            }
        } else {
            cur_r = MEM[lig_stack as usize].b16.s0 as i32
        }
    }
    j
}
unsafe fn total_pw(mut q: i32, mut p: i32) -> scaled_t {
    let mut l: i32 = 0;
    let mut r: i32 = 0;
    let mut n: i32 = 0;
    if MEM[(q + 1) as usize].b32.s1 == TEX_NULL {
        l = first_p
    } else {
        l = MEM[(MEM[(q + 1) as usize].b32.s1 + 1) as usize].b32.s1
    }
    r = prev_rightmost(global_prev_p, p);
    if p != TEX_NULL
        && NODE_type(p as usize) == DISC_NODE
        && MEM[(p + 1) as usize].b32.s0 != TEX_NULL
    {
        r = MEM[(p + 1) as usize].b32.s0;
        while MEM[r as usize].b32.s1 != TEX_NULL {
            r = *LLIST_link(r as usize);
        }
    } else {
        r = find_protchar_right(l, r);
    }
    if l != TEX_NULL && NODE_type(l as usize) == DISC_NODE {
        if MEM[(l + 1) as usize].b32.s1 != TEX_NULL {
            l = MEM[(l + 1) as usize].b32.s1;
            return char_pw(l, 0) + char_pw(r, 1);
        } else {
            n = MEM[l as usize].b16.s0 as i32;
            l = *LLIST_link(l as usize);
            while n > 0 {
                if MEM[l as usize].b32.s1 != TEX_NULL {
                    l = *LLIST_link(l as usize);
                }
                n -= 1;
            }
        }
    }
    l = find_protchar_left(l, true);
    char_pw(l, 0) + char_pw(r, 1)
}
unsafe fn find_protchar_left(mut l: i32, mut d: bool) -> i32 {
    let mut t: i32 = 0;
    let mut run: bool = false;
    if MEM[l as usize].b32.s1 != TEX_NULL
        && NODE_type(l as usize) == HLIST_NODE
        && MEM[(l + 1) as usize].b32.s1 == 0
        && MEM[(l + 3) as usize].b32.s1 == 0
        && MEM[(l + 2) as usize].b32.s1 == 0
        && MEM[(l + 5) as usize].b32.s1 == TEX_NULL
    {
        l = *LLIST_link(l as usize);
    } else if d {
        while MEM[l as usize].b32.s1 != TEX_NULL
            && !(is_char_node(l) as i32 != 0 || is_non_discardable_node(l) as i32 != 0)
        {
            l = *LLIST_link(l as usize);
        }
    }
    hlist_stack_level = 0;
    run = true;
    loop {
        t = l;
        while run as i32 != 0
            && NODE_type(l as usize) == HLIST_NODE
            && MEM[(l + 5) as usize].b32.s1 != TEX_NULL
        {
            push_node(l);
            l = MEM[(l + 5) as usize].b32.s1
        }
        while run as i32 != 0
            && (!is_char_node(l)
                && (NODE_type(l as usize) == INS_NODE
                    || NODE_type(l as usize) == MARK_NODE
                    || NODE_type(l as usize) == ADJUST_NODE
                    || NODE_type(l as usize) == PENALTY_NODE
                    || NODE_type(l as usize) == DISC_NODE
                        && MEM[(l + 1) as usize].b32.s0 == TEX_NULL
                        && MEM[(l + 1) as usize].b32.s1 == TEX_NULL
                        && MEM[l as usize].b16.s0 == 0
                    || NODE_type(l as usize) == MATH_NODE && MEM[(l + 1) as usize].b32.s1 == 0
                    || NODE_type(l as usize) == KERN_NODE
                        && (MEM[(l + 1) as usize].b32.s1 == 0 || MEM[l as usize].b16.s0 == NORMAL)
                    || NODE_type(l as usize) == GLUE_NODE && MEM[(l + 1) as usize].b32.s0 == 0
                    || NODE_type(l as usize) == HLIST_NODE
                        && MEM[(l + 1) as usize].b32.s1 == 0
                        && MEM[(l + 3) as usize].b32.s1 == 0
                        && MEM[(l + 2) as usize].b32.s1 == 0
                        && MEM[(l + 5) as usize].b32.s1 == TEX_NULL))
        {
            while MEM[l as usize].b32.s1 == TEX_NULL && hlist_stack_level > 0 {
                l = pop_node()
            }
            if MEM[l as usize].b32.s1 != TEX_NULL {
                l = *LLIST_link(l as usize);
            } else if hlist_stack_level == 0 {
                run = false
            }
        }
        if t == l {
            break;
        }
    }
    l
}
unsafe fn find_protchar_right(mut l: i32, mut r: i32) -> i32 {
    let mut t: i32 = 0;
    let mut run: bool = false;
    if r == TEX_NULL {
        return TEX_NULL;
    }
    hlist_stack_level = 0;
    run = true;
    loop {
        t = r;
        while run as i32 != 0
            && NODE_type(r as usize) == HLIST_NODE
            && MEM[(r + 5) as usize].b32.s1 != TEX_NULL
        {
            push_node(l);
            push_node(r);
            l = MEM[(r + 5) as usize].b32.s1;
            r = l;
            while MEM[r as usize].b32.s1 != TEX_NULL {
                r = *LLIST_link(r as usize)
            }
        }
        while run as i32 != 0
            && (!is_char_node(r)
                && (NODE_type(r as usize) == INS_NODE
                    || NODE_type(r as usize) == MARK_NODE
                    || NODE_type(r as usize) == ADJUST_NODE
                    || NODE_type(r as usize) == PENALTY_NODE
                    || NODE_type(r as usize) == DISC_NODE
                        && MEM[(r + 1) as usize].b32.s0 == TEX_NULL
                        && MEM[(r + 1) as usize].b32.s1 == TEX_NULL
                        && MEM[r as usize].b16.s0 == 0
                    || NODE_type(r as usize) == MATH_NODE && MEM[(r + 1) as usize].b32.s1 == 0
                    || NODE_type(r as usize) == KERN_NODE
                        && (MEM[(r + 1) as usize].b32.s1 == 0 || MEM[r as usize].b16.s0 == NORMAL)
                    || NODE_type(r as usize) == GLUE_NODE && MEM[(r + 1) as usize].b32.s0 == 0
                    || NODE_type(r as usize) == HLIST_NODE
                        && MEM[(r + 1) as usize].b32.s1 == 0
                        && MEM[(r + 3) as usize].b32.s1 == 0
                        && MEM[(r + 2) as usize].b32.s1 == 0
                        && MEM[(r + 5) as usize].b32.s1 == TEX_NULL))
        {
            while r == l && hlist_stack_level > 0 {
                r = pop_node();
                l = pop_node()
            }
            if r != l && r != TEX_NULL {
                r = prev_rightmost(l, r)
            } else if r == l && hlist_stack_level == 0 {
                run = false
            }
        }
        if t == r {
            break;
        }
    }
    r
}
unsafe fn push_node(mut p: i32) {
    if hlist_stack_level as i32 > MAX_HLIST_STACK {
        pdf_error(b"push_node", b"stack overflow");
    }
    hlist_stack[hlist_stack_level as usize] = p;
    hlist_stack_level = (hlist_stack_level as i32 + 1) as i16;
}
unsafe fn pop_node() -> i32 {
    hlist_stack_level = (hlist_stack_level as i32 - 1) as i16;
    if (hlist_stack_level as i32) < 0 {
        pdf_error(b"pop_node", b"stack underflow (internal error)");
    }
    hlist_stack[hlist_stack_level as usize]
}
