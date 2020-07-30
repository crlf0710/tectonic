#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::help;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, Confuse};
use crate::xetex_ini::{
    active_width, adjust_tail, arith_error, avail, cur_l, cur_lang, cur_list, cur_q, cur_r,
    file_line_error_style_p, first_p, font_in_short_display, global_prev_p, hc, hf, hi_mem_min, hu,
    hyf, hyf_distance, hyf_next, hyf_num, hyph_index, hyph_start, hyphen_passed, init_lft,
    init_lig, init_list, init_trie, just_box, last_leftmost_char, last_rightmost_char, lft_hit,
    lig_stack, ligature_present, max_hyph_char, op_start, pack_begin_line, pre_adjust_tail, rt_hit,
    semantic_pagination_enabled, str_pool, str_start, trie_not_ready, trie_trc, trie_trl, trie_tro,
    xtx_ligature_present, BCHAR_LABEL, CHAR_BASE, EQTB, FONT_BCHAR, FONT_INFO, HYPHEN_CHAR,
    HYPH_LINK, HYPH_LIST, HYPH_WORD, KERN_BASE, LIG_KERN_BASE, MEM, WIDTH_BASE,
};
use crate::xetex_ini::{b16x4, memory_word, MIN_TRIE_OP};
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
    clear_NODE_subtype, is_char_node, is_non_discardable_node,
    kern_NODE_subtype, /*set_NODE_subtype,*/
    llist_link, set_NODE_type, set_whatsit_NODE_subtype, text_NODE_type, whatsit_NODE_subtype,
    ACTIVE_NODE_break_node, ACTIVE_NODE_fitness, ACTIVE_NODE_glue, ACTIVE_NODE_line_number,
    ACTIVE_NODE_shortfall, ACTIVE_NODE_total_demerits, BOX_depth, BOX_height, BOX_width,
    CHAR_NODE_character, CHAR_NODE_font, DISCRETIONARY_NODE_post_break,
    DISCRETIONARY_NODE_pre_break, DISCRETIONARY_NODE_replace_count, GLUE_NODE_glue_ptr,
    GLUE_NODE_leader_ptr, LLIST_info, LLIST_link, NODE_type, PASSIVE_NODE_cur_break,
    PASSIVE_NODE_next_break, PASSIVE_NODE_prev_break, PENALTY_NODE_penalty, TeXInt, TeXOpt,
    FONT_CHARACTER_INFO, FONT_CHARACTER_WIDTH,
};

pub(crate) type scaled_t = i32;
pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
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
static mut cur_p: Option<usize> = Some(0);
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
static mut hn: i16 = 0;
static mut ha: usize = 0;
static mut hb: usize = 0;
static mut hyf_char: i32 = 0;
static mut init_cur_lang: u8 = 0;
static mut l_hyf: i32 = 0;
static mut r_hyf: i32 = 0;
static mut init_l_hyf: i32 = 0;
static mut init_r_hyf: i32 = 0;
static mut hyf_bchar: i32 = 0;
static mut last_line_fill: i32 = 0;
static mut do_last_line_fit: bool = false;
static mut active_node_size: i16 = 0;
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
    let mut c: UnicodeScalar = 0;
    pack_begin_line = cur_list.mode_line; /* "this is for over/underfull box messages" */
    *LLIST_link(TEMP_HEAD as usize) = *LLIST_link(cur_list.head);

    /* Remove trailing space or glue if present; add infinite penalty then par_fill_skip */

    if is_char_node(Some(cur_list.tail)) {
        /* is_char_node */
        *LLIST_link(cur_list.tail) = new_penalty(INF_PENALTY) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    } else if NODE_type(cur_list.tail) != TextNode::Glue.into() {
        *LLIST_link(cur_list.tail) = new_penalty(INF_PENALTY) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    } else {
        set_NODE_type(cur_list.tail, TextNode::Penalty);
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

    if GlueSpec(*GLUEPAR(GluePar::left_skip) as usize).shrink_order() != GlueOrder::Normal
        && GlueSpec(*GLUEPAR(GluePar::left_skip) as usize).shrink() != 0
    {
        *GLUEPAR(GluePar::left_skip) = finite_shrink(*GLUEPAR(GluePar::left_skip) as usize) as i32;
    }
    if GlueSpec(*GLUEPAR(GluePar::right_skip) as usize).shrink_order() != GlueOrder::Normal
        && GlueSpec(*GLUEPAR(GluePar::right_skip) as usize).shrink() != 0
    {
        *GLUEPAR(GluePar::right_skip) =
            finite_shrink(*GLUEPAR(GluePar::right_skip) as usize) as i32;
    }

    let q = GlueSpec(*GLUEPAR(GluePar::left_skip) as usize);
    let r = GlueSpec(*GLUEPAR(GluePar::right_skip) as usize);
    background[1] = q.size() + r.size();
    background[2] = 0;
    background[3] = 0;
    background[4] = 0;
    background[5] = 0;
    background[2 + q.stretch_order() as usize] = q.stretch();
    background[2 + r.stretch_order() as usize] += r.stretch();
    background[6] = q.shrink() + r.shrink();

    /* 1631: "check for special treatment of last line of paragraph" (\lastlinefit > 0) */

    do_last_line_fit = false; /*863:*/
    active_node_size = ACTIVE_NODE_SIZE_NORMAL as _;
    if *INTPAR(IntPar::last_line_fit) > 0 {
        let q = GlueSpec(*GLUE_NODE_glue_ptr(last_line_fill as usize) as usize);
        if q.stretch() > 0 && q.stretch_order() > GlueOrder::Normal {
            if background[3] == 0 && background[4] == 0 && background[5] == 0 {
                do_last_line_fit = true;
                active_node_size = ACTIVE_NODE_SIZE_EXTENDED as _;
                fill_width[0] = 0;
                fill_width[1] = 0;
                fill_width[2] = 0;
                fill_width[q.stretch_order() as usize - 1] = q.stretch();
            }
        }
    }
    minimum_demerits = AWFUL_BAD; /* 863: */
    minimal_demerits[TIGHT_FIT as usize] = AWFUL_BAD;
    minimal_demerits[DECENT_FIT as usize] = AWFUL_BAD;
    minimal_demerits[LOOSE_FIT as usize] = AWFUL_BAD;
    minimal_demerits[VERY_LOOSE_FIT as usize] = AWFUL_BAD;

    /* Prep relating to par_shape (877) */

    if let Some(ps) = LOCAL(Local::par_shape).opt() {
        last_special_line = *LLIST_info(ps) - 1;
        /* These direct `mem` accesses are in the original WEB code */
        second_width = MEM[ps + 2 * (last_special_line as usize + 1)].b32.s1;
        second_indent = MEM[ps + 2 * last_special_line as usize + 1].b32.s1;
    } else {
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
    }

    if *INTPAR(IntPar::looseness) == 0 {
        easy_line = last_special_line
    } else {
        easy_line = MAX_HALFWORD; /*:877*/
    }

    /* Start finding optimal breakpoints (892) */

    threshold = *INTPAR(IntPar::pretolerance);
    let mut second_pass;
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
            hyph_index = if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32
                != cur_lang as i32
            {
                0
            } else {
                *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
            };
        }
        let q = get_node(active_node_size as i32);
        MEM[q].b16.s1 = BreakType::Unhyphenated as _; //set_NODE_type(q as usize, UNHYPHENATED as _);
        MEM[q].b16.s0 = DECENT_FIT as _;
        *LLIST_link(q) = LAST_ACTIVE as i32;
        MEM[q + 1].b32.s1 = None.tex_int();
        MEM[q + 1].b32.s0 = cur_list.prev_graf + 1;
        MEM[q + 2].b32.s1 = 0;
        *LLIST_link(ACTIVE_LIST as usize) = q as i32;

        if do_last_line_fit {
            /*1633:*/
            MEM[q + 3].b32.s1 = 0; /*:893*/
            MEM[q + 4].b32.s1 = 0
        }
        active_width[1..].copy_from_slice(&background[1..]);
        passive = None.tex_int();
        font_in_short_display = 0; /*:893*/
        cur_p = LLIST_link(TEMP_HEAD as usize).opt();
        let mut auto_breaking = true;

        global_prev_p = cur_p.tex_int();
        let mut prev_p = global_prev_p;
        first_p = cur_p.tex_int();

        while let Some(mut cp) = cur_p {
            if llist_link(ACTIVE_LIST) == Some(LAST_ACTIVE) {
                break;
            }
            /*895: "Call try_break if cur_p is a legal breakpoint; on the
             * second pass, also try to hyphenate the next word, if cur_p is a
             * glue node; then advance cur_p to the next node of the paragraph
             * that could possibly be a legal breakpoint." */
            if is_char_node(Some(cp)) {
                /*896:*/
                global_prev_p = cp as i32;
                prev_p = global_prev_p;
                loop {
                    let f = MEM[cp].b16.s1 as usize;
                    let eff_char = effective_char(true, f, MEM[cp].b16.s0);
                    active_width[1] += FONT_INFO[(WIDTH_BASE[f as usize]
                        + FONT_INFO[(CHAR_BASE[f as usize] + eff_char) as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1;
                    cur_p = llist_link(cp);
                    cp = cur_p.unwrap();
                    if !is_char_node(Some(cp)) {
                        break;
                    }
                }
            }
            match text_NODE_type(cp).unwrap() {
                TextNode::HList | TextNode::VList | TextNode::Rule => {
                    active_width[1] += *BOX_width(cp)
                }
                TextNode::WhatsIt => match whatsit_NODE_subtype(cp) {
                    WhatsItNST::Language => {
                        let l = Language(cp);
                        cur_lang = l.lang() as u8;
                        l_hyf = l.lhm() as i32;
                        r_hyf = l.rhm() as i32;
                        if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32
                            != cur_lang as i32
                        {
                            hyph_index = 0
                        } else {
                            hyph_index = *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
                        }
                    }
                    WhatsItNST::NativeWord
                    | WhatsItNST::NativeWordAt
                    | WhatsItNST::Glyph
                    | WhatsItNST::Pic
                    | WhatsItNST::Pdf => {
                        active_width[1] += *BOX_width(cp);
                    }
                    _ => {}
                },
                TextNode::Glue => {
                    if auto_breaking {
                        if is_char_node(prev_p.opt()) {
                            try_break(0, BreakType::Unhyphenated);
                        } else if is_non_discardable_node(prev_p as usize) {
                            try_break(0, BreakType::Unhyphenated);
                        } else if NODE_type(prev_p as usize) == TextNode::Kern.into()
                            && kern_NODE_subtype(prev_p as usize) != KernNST::Explicit
                        {
                            try_break(0, BreakType::Unhyphenated);
                        }
                    }

                    c = process_glue(cp, c, second_pass, auto_breaking);
                }
                TextNode::Kern => {
                    /* ... resuming 895 ... */
                    if kern_NODE_subtype(cp) == KernNST::Explicit {
                        if (!is_char_node(llist_link(cp)) as i32) < hi_mem_min && auto_breaking {
                            if NODE_type(*LLIST_link(cp) as usize) == TextNode::Glue.into() {
                                try_break(0, BreakType::Unhyphenated);
                            }
                        }
                        active_width[1] += *BOX_width(cp);
                    } else {
                        active_width[1] += *BOX_width(cp);
                    }
                }
                TextNode::Ligature => {
                    let l = Ligature(cp);
                    let f = l.font() as usize;
                    xtx_ligature_present = true;
                    active_width[1] +=
                        *FONT_CHARACTER_WIDTH(f, effective_char(true, f, l.char()) as usize);
                }
                TextNode::Disc => {
                    /*898: try to break after a discretionary fragment, then goto done5 */
                    disc_width = 0;

                    if let Some(mut s) = DISCRETIONARY_NODE_pre_break(cp).opt() {
                        loop {
                            /*899:*/
                            if is_char_node(Some(s)) {
                                let mut eff_char_0: i32 = 0; /*:898 big DISC_NODE case */
                                let f = MEM[s].b16.s1 as usize;
                                eff_char_0 = effective_char(true, f, MEM[s].b16.s0);
                                disc_width += *FONT_CHARACTER_WIDTH(f, eff_char_0 as usize);
                            } else {
                                match text_NODE_type(s).unwrap() {
                                    TextNode::Ligature => {
                                        let l = Ligature(s);
                                        let f = l.font() as usize;
                                        xtx_ligature_present = true;
                                        let eff_char_1 = effective_char(true, f, l.char());
                                        disc_width += *FONT_CHARACTER_WIDTH(f, eff_char_1 as usize);
                                    }
                                    TextNode::HList
                                    | TextNode::VList
                                    | TextNode::Rule
                                    | TextNode::Kern => disc_width += *BOX_width(s),
                                    TextNode::WhatsIt => match whatsit_NODE_subtype(s) {
                                        WhatsItNST::NativeWord
                                        | WhatsItNST::NativeWordAt
                                        | WhatsItNST::Glyph
                                        | WhatsItNST::Pic
                                        | WhatsItNST::Pdf => {
                                            disc_width += *BOX_width(s);
                                        }
                                        _ => confusion(b"disc3a"),
                                    },
                                    _ => confusion(b"disc3"),
                                }
                            }
                            if let Some(next) = llist_link(s) {
                                s = next;
                            } else {
                                break;
                            }
                        }
                        active_width[1] += disc_width;
                        try_break(*INTPAR(IntPar::hyphen_penalty), BreakType::Hyphenated);
                        active_width[1] -= disc_width;
                    } else {
                        try_break(*INTPAR(IntPar::ex_hyphen_penalty), BreakType::Hyphenated);
                    }
                    let mut r = *DISCRETIONARY_NODE_replace_count(cp) as i32;
                    let mut sopt = llist_link(cp);
                    while r > 0 {
                        let s = sopt.unwrap();
                        if is_char_node(Some(s)) {
                            let f = *CHAR_NODE_font(s) as usize;
                            let eff_char_2 = effective_char(true, f, *CHAR_NODE_character(s));
                            active_width[1] += *FONT_CHARACTER_WIDTH(f, eff_char_2 as usize);
                        } else {
                            match text_NODE_type(s).confuse(b"disc4") {
                                TextNode::Ligature => {
                                    let l = Ligature(s);
                                    let f = l.font() as usize;
                                    xtx_ligature_present = true;
                                    let eff_char_3 = effective_char(true, f, l.char());
                                    active_width[1] +=
                                        *FONT_CHARACTER_WIDTH(f, eff_char_3 as usize);
                                }
                                TextNode::HList
                                | TextNode::VList
                                | TextNode::Rule
                                | TextNode::Kern => active_width[1] += *BOX_width(s),
                                TextNode::WhatsIt => match whatsit_NODE_subtype(s) {
                                    WhatsItNST::NativeWord
                                    | WhatsItNST::NativeWordAt
                                    | WhatsItNST::Glyph
                                    | WhatsItNST::Pic
                                    | WhatsItNST::Pdf => {
                                        active_width[1] += *BOX_width(s);
                                    }
                                    _ => confusion(b"disc4a"),
                                },
                                _ => confusion(b"disc4"),
                            }
                        }
                        r -= 1;
                        sopt = llist_link(s);
                    }
                    global_prev_p = cp as i32;
                    prev_p = global_prev_p;
                    cur_p = sopt;
                    continue;
                }
                TextNode::Math => {
                    match MathNST::from(MEM[cp].b16.s0) {
                        MathNST::Before | MathNST::Eq(BE::Begin, MathMode::Middle) => {
                            auto_breaking = false
                        }
                        MathNST::After | MathNST::Eq(BE::End, MathMode::Middle) => {
                            auto_breaking = true
                        }
                        _ => {}
                    }
                    if !is_char_node(llist_link(cp)) && auto_breaking {
                        if NODE_type(*LLIST_link(cp) as usize) == TextNode::Glue.into() {
                            try_break(0, BreakType::Unhyphenated);
                        }
                    }
                    active_width[1] += *BOX_width(cp);
                }
                TextNode::Penalty => try_break(*PENALTY_NODE_penalty(cp), BreakType::Unhyphenated),
                TextNode::Mark | TextNode::Ins | TextNode::Adjust => {}
                _ => confusion(b"paragraph"),
            }
            global_prev_p = cp as i32;
            prev_p = global_prev_p;
            cur_p = llist_link(cp);
        }
        if cur_p.is_none() {
            /*902: "Try the final line break at the end of the paragraph, and
             * goto done if the desired breakpoints have been found." */
            try_break(EJECT_PENALTY, BreakType::Hyphenated);
            if *LLIST_link(ACTIVE_LIST as usize) != LAST_ACTIVE as i32 {
                /*903:*/
                let mut r = *LLIST_link(ACTIVE_LIST as usize);
                fewest_demerits = MAX_HALFWORD;

                loop {
                    if NODE_type(r as usize) != DELTA_NODE.into() {
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

                let mut r = *LLIST_link(ACTIVE_LIST as usize); /*904:*/
                actual_looseness = 0;

                loop {
                    if NODE_type(r as usize) != DELTA_NODE.into() {
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
        let mut q = llist_link(ACTIVE_LIST).unwrap();
        while q != LAST_ACTIVE {
            cur_p = llist_link(q);
            if NODE_type(q) == DELTA_NODE.into() {
                free_node(q, DELTA_NODE_SIZE);
            } else {
                free_node(q, active_node_size as i32);
            }
            q = cur_p.unwrap();
        }

        let mut qopt = passive.opt();

        while let Some(q) = qopt {
            cur_p = llist_link(q);
            free_node(q, PASSIVE_NODE_SIZE);
            qopt = cur_p;
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
            let mut q = GlueSpec(new_spec(
                *GLUE_NODE_glue_ptr(last_line_fill as usize) as usize
            ));
            delete_glue_ref(*GLUE_NODE_glue_ptr(last_line_fill as usize) as usize);
            q.set_size(
                q.size() + *ACTIVE_NODE_shortfall(best_bet as usize)
                    - *ACTIVE_NODE_glue(best_bet as usize),
            )
            .set_stretch(0);
            *GLUE_NODE_glue_ptr(last_line_fill as usize) = q.ptr() as i32;
        }
    }

    post_line_break(d);

    /* Clean up by removing break nodes (894, again) */
    let mut q = llist_link(ACTIVE_LIST).unwrap();
    while q != ACTIVE_LIST {
        let mut next = llist_link(q);

        if NODE_type(q) == DELTA_NODE.into() {
            free_node(q, DELTA_NODE_SIZE);
        } else {
            free_node(q, active_node_size as i32);
        }
        q = next.unwrap();
    }

    let mut qopt = passive.opt();

    while let Some(q) = qopt {
        let next_0 = llist_link(q);
        free_node(q, PASSIVE_NODE_SIZE);
        qopt = next_0;
    }
    /* All done */
    pack_begin_line = 0;

    unsafe fn process_glue(
        cp: usize,
        mut c: UnicodeScalar,
        second_pass: bool,
        auto_breaking: bool,
    ) -> UnicodeScalar {
        let mut q = GlueSpec(*GLUE_NODE_glue_ptr(cp) as usize);
        if q.shrink_order() != GlueOrder::Normal && q.shrink() != 0 {
            let fresh3 = GLUE_NODE_glue_ptr(cp);
            *fresh3 = finite_shrink(q.ptr()) as i32;
            q = GlueSpec(*fresh3 as usize);
        }
        active_width[1] += q.size();
        active_width[2 + (q.stretch_order() as usize)] += q.stretch();
        /*:895*/
        active_width[6] += q.shrink(); /*:897*/

        if second_pass && auto_breaking {
            /*924: "Try to hyphenate the following word." */
            let mut prev_s = cp;

            if let Some(mut s) = llist_link(prev_s) {
                's_786: loop
                /*930: skip to node ha, or goto done1 if no hyphenation should be attempted */
                {
                    let mut flag = true;
                    if is_char_node(Some(s)) {
                        c = *CHAR_NODE_character(s) as UnicodeScalar; /*:930*/
                        hf = *CHAR_NODE_font(s) as usize;
                    } else if NODE_type(s) == TextNode::Ligature.into() {
                        let l = Ligature(s);
                        if let Some(q) = l.lig_ptr().opt() {
                            c = *CHAR_NODE_character(q) as UnicodeScalar;
                            hf = *CHAR_NODE_font(q) as usize;
                        } else {
                            flag = false;
                        }
                    } else if NODE_type(s) == TextNode::Kern.into()
                        && kern_NODE_subtype(s) == KernNST::Normal
                    {
                        flag = false;
                    } else if NODE_type(s) == TextNode::Math.into()
                        && (match MathNST::from(MEM[s].b16.s0) {
                            MathNST::Eq(_, MathMode::Left) | MathNST::Eq(_, MathMode::Right) => {
                                true
                            }
                            _ => false,
                        })
                    // NODE_subtype(s as usize)
                    {
                        flag = false;
                    } else {
                        if !(NODE_type(s) == TextNode::WhatsIt.into()) {
                            return c;
                        }
                        match whatsit_NODE_subtype(s) {
                            WhatsItNST::NativeWord | WhatsItNST::NativeWordAt => {
                                let s = NativeWord::from(s);
                                let mut l = 0;
                                while l < s.text().len() as i32 {
                                    c = get_native_usv(s.ptr(), l as usize);
                                    if *LC_CODE(c as usize) != 0 {
                                        hf = s.font() as usize;
                                        prev_s = s.ptr();
                                        break 's_786;
                                    } else {
                                        if c as i64 >= 65536 {
                                            l += 1
                                        }
                                        l += 1
                                    }
                                }
                            }
                            WhatsItNST::Language => {
                                let l = Language(s);
                                cur_lang = l.lang() as u8;
                                l_hyf = l.lhm() as i32;
                                r_hyf = l.rhm() as i32;

                                hyph_index = if *trie_trc
                                    .offset((hyph_start + cur_lang as i32) as isize)
                                    as i32
                                    != cur_lang as i32
                                {
                                    0
                                } else {
                                    *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
                                };
                            }
                            _ => {}
                        }
                        flag = false;
                    }
                    if flag {
                        hc[0] = if hyph_index == 0 || c > 255 {
                            *LC_CODE(c as usize)
                        } else if *trie_trc.offset((hyph_index + c) as isize) as i32 != c {
                            0
                        } else {
                            *trie_tro.offset((hyph_index + c) as isize)
                        };
                        if hc[0] != 0 {
                            if hc[0] == c || *INTPAR(IntPar::uc_hyph) > 0 {
                                break;
                            } else {
                                return c;
                            }
                        }
                    }

                    // continue:
                    prev_s = s;
                    s = llist_link(prev_s).unwrap();
                }

                // done2:
                hyf_char = HYPHEN_CHAR[hf as usize];
                if hyf_char < 0 {
                    return c;
                }
                if hyf_char > BIGGEST_CHAR {
                    return c;
                }

                ha = prev_s; // :930

                if (l_hyf + r_hyf) as usize > max_hyphenatable_length() {
                    return c;
                }

                if ha < hi_mem_min as usize
                    && NODE_type(ha) == TextNode::WhatsIt.into()
                    && (whatsit_NODE_subtype(ha) == WhatsItNST::NativeWord
                        || whatsit_NODE_subtype(ha) == WhatsItNST::NativeWordAt)
                {
                    let mut ha_nw = NativeWord::from(ha);
                    /*926: check that nodes after native_word permit hyphenation; if not, goto done1 */
                    s = llist_link(ha).unwrap();

                    loop {
                        if !is_char_node(Some(s)) {
                            match text_NODE_type(s).unwrap() {
                                TextNode::Ligature => {}
                                TextNode::Kern => {
                                    if kern_NODE_subtype(s) != KernNST::Normal {
                                        break;
                                    }
                                }
                                TextNode::WhatsIt
                                | TextNode::Glue
                                | TextNode::Penalty
                                | TextNode::Ins
                                | TextNode::Adjust
                                | TextNode::Mark => {
                                    break;
                                }
                                _ => {
                                    return c;
                                }
                            }
                        }
                        s = llist_link(s).unwrap();
                    }

                    // done6:
                    /*927: prepare a native_word_node for hyphenation.
                     * "Note that if there are chars with lccode = 0,
                     * we split them out into separate native_word
                     * nodes." */
                    hn = 0 as i16;

                    'restart: loop {
                        // 'ha' can change in the loop, so for safety:
                        let for_end_1 = ha_nw.text().len() as i32;

                        let mut l = 0;
                        loop {
                            if !(l < for_end_1) {
                                break 'restart;
                            }
                            c = get_native_usv(ha, l as usize);
                            hc[0] = if hyph_index == 0 || c > 255 {
                                *LC_CODE(c as usize)
                            } else if *trie_trc.offset((hyph_index + c) as isize) as i32 != c {
                                0
                            } else {
                                *trie_tro.offset((hyph_index + c) as isize)
                            };
                            if hc[0] == 0 {
                                if hn > 0 {
                                    let ha_text = ha_nw.text();
                                    let mut q = new_native_word_node(hf, ha_text.len() as i32 - l);
                                    set_whatsit_NODE_subtype(q.ptr(), whatsit_NODE_subtype(ha));
                                    q.text_mut().copy_from_slice(&ha_text[l as usize..]);

                                    q.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                                    *LLIST_link(q.ptr()) = *LLIST_link(ha);
                                    *LLIST_link(ha) = Some(q.ptr()).tex_int();
                                    ha_nw.set_length(l as u16);
                                    ha_nw.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                                    break 'restart;
                                }
                            } else if hn == 0 && l > 0 {
                                let ha_text = ha_nw.text();
                                let mut q = new_native_word_node(hf, ha_text.len() as i32 - l);
                                set_whatsit_NODE_subtype(q.ptr(), whatsit_NODE_subtype(ha));
                                q.text_mut().copy_from_slice(&ha_text[l as usize..]);

                                q.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                                *LLIST_link(q.ptr()) = *LLIST_link(ha);
                                *LLIST_link(ha) = Some(q.ptr()).tex_int();
                                ha_nw.set_length(l as u16);
                                ha_nw.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                                ha = *LLIST_link(ha) as usize;
                                ha_nw = NativeWord::from(ha);
                                break;
                            } else {
                                if hn as usize == max_hyphenatable_length() {
                                    break 'restart;
                                }
                                hn += 1;
                                if (c as i64) < 65536 {
                                    hu[hn as usize] = c;
                                    hc[hn as usize] = hc[0]
                                } else {
                                    hu[hn as usize] = ((c as i64 - 65536) / 1024 + 0xd800) as i32;
                                    hc[hn as usize] =
                                        ((hc[0] as i64 - 65536) / 1024 + 0xd800) as i32;
                                    hn += 1;
                                    hu[hn as usize] = c % 1024 + 0xdc00;
                                    hc[hn as usize] = hc[0] % 1024 + 0xdc00;
                                    l += 1;
                                }
                                hyf_bchar = TOO_BIG_CHAR;
                            }
                            l += 1;
                        }
                    }
                } else {
                    /*931: skip to node hb, putting letters into hu and hc */
                    hn = 0;

                    's_1342: loop {
                        if is_char_node(Some(s)) {
                            if *CHAR_NODE_font(s) as usize != hf {
                                break;
                            }
                            hyf_bchar = *CHAR_NODE_character(s) as i32;
                            c = hyf_bchar;
                            if hyph_index == 0 || c > 255 {
                                hc[0] = *LC_CODE(c as usize);
                            } else if *trie_trc.offset((hyph_index + c) as isize) as i32 != c {
                                hc[0] = 0
                            } else {
                                hc[0] = *trie_tro.offset((hyph_index + c) as isize)
                            }
                            if hc[0] == 0 {
                                break;
                            }
                            if hc[0] > max_hyph_char {
                                break;
                            }
                            if hn as usize == max_hyphenatable_length() {
                                break;
                            }
                            hb = s;
                            hn += 1;
                            hu[hn as usize] = c;
                            hc[hn as usize] = hc[0];
                            hyf_bchar = TOO_BIG_CHAR;
                        } else if NODE_type(s) == TextNode::Ligature.into() {
                            let l = Ligature(s);
                            /*932: move the characters of a ligature node to hu and hc; but goto done3
                             * if they are not all letters. */
                            if l.font() as usize != hf {
                                break;
                            }
                            let mut j = hn;
                            let mut qopt = l.lig_ptr().opt();
                            if let Some(q) = qopt {
                                hyf_bchar = *CHAR_NODE_character(q as usize) as i32
                            }
                            while let Some(q) = qopt {
                                c = *CHAR_NODE_character(q) as UnicodeScalar;
                                hc[0] = if hyph_index == 0 || c > 255 {
                                    *LC_CODE(c as usize)
                                } else if *trie_trc.offset((hyph_index + c) as isize) as i32 != c {
                                    0
                                } else {
                                    *trie_tro.offset((hyph_index + c) as isize)
                                };
                                if hc[0] == 0 {
                                    break 's_1342;
                                }
                                if hc[0] > max_hyph_char {
                                    break 's_1342;
                                }
                                if j as usize == max_hyphenatable_length() {
                                    break 's_1342;
                                }
                                j += 1;
                                hu[j as usize] = c;
                                hc[j as usize] = hc[0];
                                qopt = llist_link(q);
                            }
                            hb = l.ptr();
                            hn = j;
                            if l.right_hit() {
                                hyf_bchar = FONT_BCHAR[hf as usize]
                            } else {
                                hyf_bchar = TOO_BIG_CHAR;
                            }
                        /*:932*/
                        } else {
                            if !(NODE_type(s) == TextNode::Kern.into()
                                && kern_NODE_subtype(s) == KernNST::Normal)
                            {
                                break;
                            }
                            hb = s;
                            hyf_bchar = FONT_BCHAR[hf as usize]
                        }
                        s = llist_link(s).unwrap();
                    }
                }

                /*933: check that the nodes following hb permit
                 * hyphenation and that at least l_hyf + r_hyf letters
                 * have been found, otherwise goto done1 */
                if (hn as i32) < l_hyf + r_hyf {
                    return c;
                }

                loop {
                    if !is_char_node(Some(s)) {
                        match text_NODE_type(s).unwrap() {
                            TextNode::Ligature => {}
                            TextNode::Kern => {
                                if kern_NODE_subtype(s) != KernNST::Normal {
                                    break;
                                }
                            }
                            TextNode::WhatsIt
                            | TextNode::Glue
                            | TextNode::Penalty
                            | TextNode::Ins
                            | TextNode::Adjust
                            | TextNode::Mark => {
                                break;
                            }
                            TextNode::Math => {
                                match MathNST::from(MEM[s].b16.s0) {
                                    MathNST::Eq(_, MathMode::Left)
                                    | MathNST::Eq(_, MathMode::Right) => break,
                                    _ => return c,
                                };
                            }
                            _ => {
                                return c;
                            }
                        }
                    }
                    s = llist_link(s).unwrap();
                }
                // done4: 933
                hyphenate();
            }
        }
        c
    }
}
/* This was just separated out to prevent line_break() from becoming
 * proposterously long. */
unsafe fn post_line_break(mut d: bool) {
    let mut LR_ptr = cur_list.eTeX_aux;
    /* Reverse the list of break nodes (907) */
    let mut q = *ACTIVE_NODE_break_node(best_bet as usize); /*:907*/
    cur_p = None;
    loop {
        let mut r = q;
        q = *PASSIVE_NODE_prev_break(q as usize);
        *PASSIVE_NODE_next_break(r as usize) = cur_p.tex_int();
        cur_p = Some(r as usize);
        if q.opt().is_none() {
            break;
        }
    }
    let mut cur_line = cur_list.prev_graf + 1;
    loop {
        /* 909: justify the line ending at breakpoint cur_p and append it to
         * the current vertical list, with associated penalties and
         * insertions. The current line starts a TEMP_HEAD.link and ends at
         * cur_p.cur_break.
         **/
        let cp = cur_p.unwrap();
        if *INTPAR(IntPar::texxet) > 0 {
            /*1494:*/
            let mut q = *LLIST_link(TEMP_HEAD);
            if let Some(lr) = LR_ptr {
                let mut tmp_ptr = lr;
                let mut r = q;
                loop {
                    let s = new_math(0, MathNST::from((MEM[tmp_ptr].b32.s0 - 1) as u16)) as usize;
                    *LLIST_link(s) = r;
                    r = s as i32;
                    if let Some(next) = llist_link(tmp_ptr) {
                        tmp_ptr = next;
                    } else {
                        break;
                    }
                }
                *LLIST_link(TEMP_HEAD) = r;
            }
            while q != MEM[cp + 1].b32.s1 {
                if q < hi_mem_min && NODE_type(q as usize) == TextNode::Math.into() {
                    /*1495:*/
                    let (be, mode) = MathNST::from(MEM[q as usize].b16.s0).equ();
                    if be == BE::End {
                        if let Some(lr) = LR_ptr {
                            if MathNST::from(MEM[lr].b32.s0 as u16) == MathNST::Eq(BE::End, mode) {
                                let tmp_ptr = lr;
                                LR_ptr = llist_link(tmp_ptr);
                                *LLIST_link(tmp_ptr) = avail.tex_int();
                                avail = Some(tmp_ptr);
                            }
                        }
                    } else {
                        let tmp_ptr = get_avail();
                        MEM[tmp_ptr].b32.s0 = u16::from(MathNST::Eq(BE::End, mode)) as i32;
                        *LLIST_link(tmp_ptr) = LR_ptr.tex_int();
                        LR_ptr = Some(tmp_ptr);
                    }
                }
                q = *LLIST_link(q as usize);
            }
        }
        /* 910: "Modify the end of the line to reflect the nature of the break
         * and to include \rightskip; also set the proper value of
         * disc_break" */
        let mut disc_break = false;
        let mut post_disc_break = false;
        let mut glue_break = false;

        let mut q = if let Some(mut q) = PASSIVE_NODE_cur_break(cp).opt() {
            match text_NODE_type(q).unwrap() {
                TextNode::Glue => {
                    delete_glue_ref(*GLUE_NODE_glue_ptr(q) as usize);
                    *GLUE_NODE_glue_ptr(q) = *GLUEPAR(GluePar::right_skip);
                    MEM[q].b16.s0 = GluePar::right_skip as u16 + 1; // NODE_subtype(q)
                    GlueSpec(*GLUEPAR(GluePar::right_skip) as usize).rc_inc();
                    glue_break = true;
                }
                TextNode::Disc => {
                    /*911:*/
                    let mut t = *DISCRETIONARY_NODE_replace_count(q);
                    let mut r;
                    if t == 0 {
                        r = *LLIST_link(q);
                    } else {
                        r = Some(q).tex_int();
                        while t > 1 {
                            r = *LLIST_link(r as usize);
                            t -= 1;
                        }
                        let s = *LLIST_link(r as usize) as usize;
                        r = *LLIST_link(s);
                        *LLIST_link(s) = None.tex_int();
                        flush_node_list(llist_link(q));
                        *DISCRETIONARY_NODE_replace_count(q) = 0;
                    }
                    if let Some(mut s) = DISCRETIONARY_NODE_post_break(q).opt() {
                        /*913:*/
                        while let Some(next) = llist_link(s) {
                            s = next;
                        }

                        *LLIST_link(s) = r;

                        r = *DISCRETIONARY_NODE_post_break(q);
                        *DISCRETIONARY_NODE_post_break(q) = None.tex_int();
                        post_disc_break = true;
                    }
                    if let Some(mut s) = DISCRETIONARY_NODE_pre_break(q).opt() {
                        /*914:*/
                        *LLIST_link(q) = Some(s).tex_int();

                        while let Some(next) = llist_link(s) {
                            s = next;
                        }
                        *DISCRETIONARY_NODE_pre_break(q) = None.tex_int();
                        q = s;
                    }
                    *LLIST_link(q) = r;
                    disc_break = true;
                }
                TextNode::Kern => {
                    *BOX_width(q) = 0
                }
                TextNode::Math => {
                    *BOX_width(q) = 0;
                    if *INTPAR(IntPar::texxet) > 0 {
                        /*1495:*/
                        let (be, _) = MathNST::from(*INTPAR(IntPar::texxet) as u16).equ();
                        let (_, mode) = MathNST::from(MEM[q].b16.s0 as u16).equ();
                        if be == BE::End {
                            if let Some(lr) = LR_ptr {
                                if MathNST::from(MEM[lr].b32.s0 as u16) == MathNST::Eq(BE::End, mode) {
                                    let tmp_ptr = lr;
                                    LR_ptr = llist_link(tmp_ptr);
                                    *LLIST_link(tmp_ptr) = avail.tex_int();
                                    avail = Some(tmp_ptr);
                                }
                            }
                        } else {
                            let tmp_ptr = get_avail();
                            MEM[tmp_ptr].b32.s0 = u16::from(MathNST::Eq(BE::End, mode)) as i32;
                            *LLIST_link(tmp_ptr) = LR_ptr.tex_int();
                            LR_ptr = Some(tmp_ptr);
                        }
                    }
                }
                _ => {}
            }
            q
        } else {
            let mut q = TEMP_HEAD;
            while let Some(next) = llist_link(q) {
                q = next
            }
            q
        };
        /* "at this point q is the rightmost breakpoint; the only exception is
         * the case of a discretionary break with non-empty pre_break -- then
         * q has been changed to the last node of the pre-break list" */
        if *INTPAR(IntPar::xetex_protrude_chars) > 0 {
            let ptmp;
            let p = if disc_break && (is_char_node(Some(q)) || NODE_type(q) != TextNode::Disc.into()) {
                /*:915*/
                ptmp = Some(q);
                Some(q)
            } else {
                let p = prev_rightmost(llist_link(TEMP_HEAD), Some(q));
                ptmp = p;
                find_protchar_right(llist_link(TEMP_HEAD), p)
            };
            let w = char_pw(p, Side::Right);
            if w != 0 {
                let ptmp = ptmp.unwrap();
                let k = new_margin_kern(-w, last_rightmost_char, Side::Right);
                *LLIST_link(k) = *LLIST_link(ptmp);
                *LLIST_link(ptmp) = Some(k).tex_int();
                if ptmp == q {
                    q = llist_link(q).unwrap();
                }
            }
        }
        if !glue_break {
            let r = new_param_glue(GluePar::right_skip);
            *LLIST_link(r) = *LLIST_link(q);
            *LLIST_link(q) = Some(r).tex_int();
            q = r;
        }
        if *INTPAR(IntPar::texxet) > 0 {
            /*1496:*/
            if let Some(lr) = LR_ptr {
                let mut s = TEMP_HEAD;
                let mut r = *LLIST_link(s);

                while r.opt() != Some(q) {
                    s = r as usize;
                    r = *LLIST_link(s);
                }

                let mut ropt = Some(lr);

                while let Some(r) = ropt {
                    let tmp_ptr = new_math(0, MathNST::from(MEM[r].b32.s0 as u16));
                    *LLIST_link(s) = Some(tmp_ptr).tex_int();
                    s = tmp_ptr;
                    ropt = llist_link(r);
                }

                *LLIST_link(s) = Some(q).tex_int();
            }
        }
        /* 916: Put \leftskip at the left and detach this line. */
        let r = *LLIST_link(q);
        *LLIST_link(q) = None.tex_int();
        let mut q = *LLIST_link(TEMP_HEAD);
        *LLIST_link(TEMP_HEAD) = r;
        /* "at this point q is the leftmost node; all discardable nodes have been discarded */
        
        if *INTPAR(IntPar::xetex_protrude_chars) > 0 {
            let p = find_protchar_left(q as usize, false);
            let w = char_pw(Some(p), Side::Left);
            if w != 0 {
                let k = new_margin_kern(-w, last_leftmost_char, Side::Left);
                *LLIST_link(k) = q;
                q = k as i32;
            }
        }
        if *GLUEPAR(GluePar::left_skip) != 0 {
            let r = new_param_glue(GluePar::left_skip);
            *LLIST_link(r) = q;
            q = r as i32;
        }
        /* 918: q points to the hlist that represents the current line. Pack
         * it up at the right width. */
        let (cur_width, cur_indent) = if cur_line > last_special_line {
            (second_width, second_indent)
        } else if let Some(l) = LOCAL(Local::par_shape).opt() {
            /* These manual `mem` indices are in the original WEB code */
            (MEM[l + 2 * cur_line as usize].b32.s1, MEM[l + 2 * cur_line as usize - 1].b32.s1)
        } else {
            (first_width, first_indent)
        };
        adjust_tail = Some(ADJUST_HEAD);
        pre_adjust_tail = Some(PRE_ADJUST_HEAD);
        /* Tectonic: in semantic pagination mode, set each "line" (really the
         * whole paragraph) at its natural width. */
        let mut jb = if semantic_pagination_enabled {
            hpack(q.opt(), 0, PackMode::Additional)
        } else {
            hpack(q.opt(), cur_width, PackMode::Exactly)
        }; /*:918*/
        jb.set_shift_amount(cur_indent);
        just_box = jb.ptr();
        /* 917: append the new box to the current vertical list, followed
         * by any of its special nodes that were taken out */

        if Some(PRE_ADJUST_HEAD) != pre_adjust_tail {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(PRE_ADJUST_HEAD); /*:917*/
            cur_list.tail = pre_adjust_tail.unwrap();
        }

        pre_adjust_tail = None;
        append_to_vlist(just_box);

        if Some(ADJUST_HEAD) != adjust_tail {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(ADJUST_HEAD);
            cur_list.tail = adjust_tail.unwrap();
        }

        adjust_tail = None;

        /* 919: Set `pen` to all of the penalties relevant to this line. */
        if cur_line + 1 != best_line {
            let mut pen = if let Some(q) = EQTB[INTER_LINE_PENALTIES_LOC].val.opt() {
                let mut r = cur_line;
                if r > *PENALTY_NODE_penalty(q) {
                    r = *PENALTY_NODE_penalty(q)
                }
                *PENALTY_NODE_penalty(q + (r as usize))
            } else {
                *INTPAR(IntPar::inter_line_penalty)
            };
            if let Some(q) = EQTB[CLUB_PENALTIES_LOC].val.opt() {
                let mut r = cur_line - cur_list.prev_graf;
                if r > *PENALTY_NODE_penalty(q) {
                    r = *PENALTY_NODE_penalty(q)
                }
                pen += *PENALTY_NODE_penalty(q + (r as usize))
            } else if cur_line == cur_list.prev_graf + 1 {
                pen += *INTPAR(IntPar::club_penalty)
            }
            let q = if d {
                EQTB[DISPLAY_WIDOW_PENALTIES_LOC].val.opt()
            } else {
                EQTB[WIDOW_PENALTIES_LOC].val.opt()
            };
            if let Some(q) = q {
                let mut r = best_line - cur_line - 1;
                if r > *PENALTY_NODE_penalty(q) {
                    r = *PENALTY_NODE_penalty(q)
                }
                pen += *PENALTY_NODE_penalty(q + (r as usize));
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
                let r = new_penalty(pen);
                *LLIST_link(cur_list.tail) = Some(r).tex_int();
                cur_list.tail = r;
            }
        }
        /* Done justifying this line. */
        cur_line += 1;
        cur_p = PASSIVE_NODE_next_break(cp).opt();
        if let Some(cp) = cur_p {
            if !post_disc_break {
                /* 908: "prune unwanted nodes at the beginning of the next
                 * line". Delete glues, penalties, kerns, and math nodes at
                 * the beginning of the line, unless the node in question is
                 * the chosen breakpoint. */
                let mut r = TEMP_HEAD;
                let mut q;
                loop {
                    q = *LLIST_link(r as usize);
                    if q == *PASSIVE_NODE_cur_break(cp) {
                        break;
                    }
                    if is_char_node(q.opt()) {
                        break;
                    }
                    if is_non_discardable_node(q as usize) {
                        break;
                    }
                    if NODE_type(q as usize) == TextNode::Kern.into() && kern_NODE_subtype(q as usize) != KernNST::Explicit && kern_NODE_subtype(q as usize) != KernNST::SpaceAdjustment {
                        break;
                    }
                    r = q as usize;
                    if NODE_type(q as usize) == TextNode::Math.into() && *INTPAR(IntPar::texxet) > 0 {
                        /*1495:*/
                        let (be, mode) = MathNST::from(MEM[q as usize].b16.s0).equ();
                        if be == BE::End {
                            if let Some(lr) = LR_ptr {
                                if MathNST::from(MEM[lr].b32.s0 as u16) == MathNST::Eq(BE::End, mode) {
                                    let tmp_ptr = lr;
                                    LR_ptr = llist_link(tmp_ptr);
                                    *LLIST_link(tmp_ptr) = avail.tex_int();
                                    avail = Some(tmp_ptr);
                                }
                            }
                        } else {
                            let tmp_ptr = get_avail();
                            MEM[tmp_ptr].b32.s0 = u16::from(MathNST::Eq(BE::End, mode)) as i32;
                            *LLIST_link(tmp_ptr) = LR_ptr.tex_int();
                            LR_ptr = Some(tmp_ptr);
                        }
                    }
                }
                if r != TEMP_HEAD {
                    *LLIST_link(r as usize) = None.tex_int();
                    flush_node_list(llist_link(TEMP_HEAD));
                    *LLIST_link(TEMP_HEAD) = q;
                }
            }
        }
        if cur_p.is_none() {
            break;
        }
    }
    if cur_line != best_line || llist_link(TEMP_HEAD).is_some() {
        confusion(b"line breaking");
    }
    cur_list.prev_graf = best_line - 1;
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
unsafe fn try_break(mut pi: i32, mut break_type: BreakType) {
    let mut prev_prev_r = None;
    let mut node_r_stays_active: bool = false;
    let mut line_width: scaled_t = 0i32;
    let mut fit_class: u8 = 0;
    let mut b: i32 = 0;
    let mut artificial_demerits: bool = false;
    let mut shortfall: scaled_t = 0;
    let mut g: scaled_t = 0i32;
    /* Tectonic: no-op except at the end of the paragraph. We know we're at
     * the very end of the paragraph when cur_p is TEX_NULL. */
    if semantic_pagination_enabled && cur_p.is_some() {
        return;
    }
    if pi.abs() >= INF_PENALTY {
        if pi > 0 {
            return;
        }
        pi = EJECT_PENALTY
    }
    let mut no_break_yet = true;
    let mut prev_r = ACTIVE_LIST;
    let mut old_l = 0;
    cur_active_width[1..].copy_from_slice(&active_width[1..]);
    loop {
        let r = llist_link(prev_r).unwrap();
        /*861: "If node r is of type delta_node, update cur_active_width, set
         * prev_r and prev_prev_r, then goto continue" */
        if NODE_type(r) == DELTA_NODE.into() {
            let r = Delta(r);
            cur_active_width[1] += r.dwidth();
            cur_active_width[2] += r.dstretch0();
            cur_active_width[3] += r.dstretch1();
            cur_active_width[4] += r.dstretch2();
            cur_active_width[5] += r.dstretch3();
            cur_active_width[6] += r.dshrink();
            prev_prev_r = Some(prev_r);
            prev_r = r.ptr();
        } else {
            /*864: "If a line number class has ended, create new active nodes for
             * the best feasible breaks in that class; then return if r =
             * last_active, otherwise compute the new line_width." */
            let l = *ACTIVE_NODE_line_number(r);

            if l > old_l {
                /* "now we are no longer in the inner loop" */
                if minimum_demerits < AWFUL_BAD && (old_l != easy_line || r == LAST_ACTIVE) {
                    /*865: "Create new active nodes for the best feasible breaks
                     * just found." */
                    if no_break_yet {
                        /*866: "Compute the values of break_width". */
                        no_break_yet = false;
                        break_width[1..].copy_from_slice(&background[1..]);
                        let mut sopt = cur_p;
                        if break_type == BreakType::Hyphenated {
                            /*869: "Compute the discretionary break_width values" */
                            if let Some(cp) = cur_p {
                                let mut t = *DISCRETIONARY_NODE_replace_count(cp) as i32;
                                let mut v = cp;
                                sopt = DISCRETIONARY_NODE_post_break(cp).opt();
                                while t > 0 {
                                    t -= 1;
                                    v = *LLIST_link(v) as usize;
                                    /*870: "subtract the width of node v from break_width" */
                                    if is_char_node(Some(v)) {
                                        let mut eff_char: i32 = 0;

                                        let f = *CHAR_NODE_font(v) as usize;
                                        eff_char = effective_char(true, f, *CHAR_NODE_character(v));
                                        break_width[1] -=
                                            *FONT_CHARACTER_WIDTH(f, eff_char as usize);
                                    } else {
                                        match text_NODE_type(v).confuse(b"disc1") {
                                            TextNode::Ligature => {
                                                let l = Ligature(v);
                                                let f = l.font() as usize;
                                                xtx_ligature_present = true;
                                                let eff_char_0 = effective_char(true, f, l.char());
                                                break_width[1] -=
                                                    *FONT_CHARACTER_WIDTH(f, eff_char_0 as usize);
                                            }
                                            TextNode::HList
                                            | TextNode::VList
                                            | TextNode::Rule
                                            | TextNode::Kern => {
                                                break_width[1] -= *BOX_width(v);
                                            }
                                            TextNode::WhatsIt => match whatsit_NODE_subtype(v) {
                                                WhatsItNST::NativeWord
                                                | WhatsItNST::NativeWordAt
                                                | WhatsItNST::Glyph
                                                | WhatsItNST::Pic
                                                | WhatsItNST::Pdf => {
                                                    break_width[1] -= *BOX_width(v);
                                                }
                                                _ => confusion(b"disc1a"),
                                            },
                                            _ => confusion(b"disc1"),
                                        }
                                    }
                                }
                                /*871: "add the width of node s to break_width" */
                                while let Some(s) = sopt {
                                    if is_char_node(Some(s)) {
                                        let mut eff_char_1: i32 = 0;
                                        let f = *CHAR_NODE_font(s) as usize;
                                        eff_char_1 = effective_char(true, f, MEM[s].b16.s0);
                                        break_width[1] +=
                                            *FONT_CHARACTER_WIDTH(f, eff_char_1 as usize);
                                    } else {
                                        match text_NODE_type(s).confuse(b"disc2") {
                                            TextNode::Ligature => {
                                                let l = Ligature(s);
                                                let f = l.font() as usize;
                                                xtx_ligature_present = true;
                                                let eff_char_2 = effective_char(true, f, l.char());
                                                break_width[1] +=
                                                    *FONT_CHARACTER_WIDTH(f, eff_char_2 as usize);
                                            }
                                            TextNode::HList
                                            | TextNode::VList
                                            | TextNode::Rule
                                            | TextNode::Kern => {
                                                break_width[1] += *BOX_width(s);
                                            }
                                            TextNode::WhatsIt => match whatsit_NODE_subtype(s) {
                                                WhatsItNST::NativeWord
                                                | WhatsItNST::NativeWordAt
                                                | WhatsItNST::Glyph
                                                | WhatsItNST::Pic
                                                | WhatsItNST::Pdf => {
                                                    break_width[1] += *BOX_width(s);
                                                }
                                                _ => confusion(b"disc2a"),
                                            },
                                            _ => confusion(b"disc2"),
                                        }
                                    }
                                    sopt = llist_link(s);
                                }
                                break_width[1] += disc_width;
                                if DISCRETIONARY_NODE_post_break(cp).opt().is_none() {
                                    sopt = llist_link(v);
                                }
                            }
                        }
                        while let Some(s) = sopt {
                            if is_char_node(Some(s)) {
                                break;
                            }
                            match text_NODE_type(s).unwrap() {
                                TextNode::Glue => {
                                    let v = GlueSpec(*GLUE_NODE_glue_ptr(s) as usize);
                                    break_width[1] -= v.size();
                                    break_width[2 + v.stretch_order() as usize] -= v.stretch();
                                    break_width[6] -= v.shrink();
                                }
                                TextNode::Penalty => {}
                                TextNode::Math => break_width[1] -= *BOX_width(s),
                                TextNode::Kern => {
                                    if kern_NODE_subtype(s) != KernNST::Explicit {
                                        break;
                                    }
                                    break_width[1] -= *BOX_width(s)
                                }
                                _ => break,
                            }
                            sopt = llist_link(s);
                        }
                    }
                    /*872: "Insert a delta node to prepare for breaks at cur_p" */
                    if NODE_type(prev_r) == DELTA_NODE.into() {
                        /* this is unused */
                        let mut prev_r = Delta(prev_r);
                        prev_r.set_dwidth(prev_r.dwidth() - cur_active_width[1] + break_width[1]);
                        prev_r.set_dstretch0(
                            prev_r.dstretch0() - cur_active_width[2] + break_width[2],
                        );
                        prev_r.set_dstretch1(
                            prev_r.dstretch1() - cur_active_width[3] + break_width[3],
                        );
                        prev_r.set_dstretch2(
                            prev_r.dstretch2() - cur_active_width[4] + break_width[4],
                        );
                        prev_r.set_dstretch3(
                            prev_r.dstretch3() - cur_active_width[5] + break_width[5],
                        );
                        prev_r.set_dshrink(prev_r.dshrink() - cur_active_width[6] + break_width[6]);
                    } else if prev_r == ACTIVE_LIST {
                        active_width[1..].copy_from_slice(&break_width[1..]);
                    } else {
                        let q = get_node(DELTA_NODE_SIZE);
                        *LLIST_link(q) = Some(r).tex_int();
                        set_NODE_type(q, DELTA_NODE);
                        clear_NODE_subtype(q);
                        Delta(q)
                            .set_dwidth(break_width[1] - cur_active_width[1])
                            .set_dstretch0(break_width[2] - cur_active_width[2])
                            .set_dstretch1(break_width[3] - cur_active_width[3])
                            .set_dstretch2(break_width[4] - cur_active_width[4])
                            .set_dstretch3(break_width[5] - cur_active_width[5])
                            .set_dshrink(break_width[6] - cur_active_width[6]);
                        *LLIST_link(prev_r) = Some(q).tex_int();
                        prev_prev_r = Some(prev_r);
                        prev_r = q;
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
                            *PASSIVE_NODE_cur_break(q) = cur_p.tex_int();
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
                            *LLIST_link(q) = Some(r).tex_int();
                            *LLIST_link(prev_r) = Some(q).tex_int();
                            prev_r = q;
                        }
                        minimal_demerits[fit_class as usize] = MAX_HALFWORD;
                        fit_class = fit_class.wrapping_add(1)
                    }
                    minimum_demerits = MAX_HALFWORD;
                    /*873: "Insert a delta node to prepare for the next active node" */
                    if r != LAST_ACTIVE {
                        let q = get_node(DELTA_NODE_SIZE);
                        *LLIST_link(q) = Some(r).tex_int();
                        set_NODE_type(q, DELTA_NODE);
                        clear_NODE_subtype(q); /* subtype is not used */
                        Delta(q)
                            .set_dwidth(cur_active_width[1] - break_width[1])
                            .set_dstretch0(cur_active_width[2] - break_width[2])
                            .set_dstretch1(cur_active_width[3] - break_width[3])
                            .set_dstretch2(cur_active_width[4] - break_width[4])
                            .set_dstretch3(cur_active_width[5] - break_width[5])
                            .set_dshrink(cur_active_width[6] - break_width[6]);
                        *LLIST_link(prev_r) = Some(q).tex_int();
                        prev_prev_r = Some(prev_r);
                        prev_r = q;
                    }
                }
                /* ... resuming 864 ... */
                if r == LAST_ACTIVE {
                    return;
                }
                /*879: "Compute the new line width" */
                if l > easy_line {
                    line_width = second_width;
                    old_l = MAX_HALFWORD - 1;
                } else {
                    old_l = l;
                    line_width = if l > last_special_line {
                        second_width
                    } else if let Some(ps) = LOCAL(Local::par_shape).opt() {
                        MEM[ps + 2 * (l as usize)].b32.s1
                    } else {
                        first_width
                    };
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

            let mut current_block: u64;
            if shortfall > 0 {
                /*881: "Set the value of b to the badness for stretching the line,
                 * and compute the corresponding fit_class" */
                if cur_active_width[3] != 0 || cur_active_width[4] != 0 || cur_active_width[5] != 0
                {
                    if do_last_line_fit {
                        if cur_p.is_none() {
                            /*1634: "Perform computations for the last line and goto found" */
                            if *ACTIVE_NODE_shortfall(r) == 0 || *ACTIVE_NODE_glue(r) <= 0 {
                                current_block = 5565703735569783978;
                            } else if cur_active_width[3] != fill_width[0]
                                || cur_active_width[4] != fill_width[1]
                                || cur_active_width[5] != fill_width[2]
                            {
                                current_block = 5565703735569783978;
                            } else {
                                g = if *ACTIVE_NODE_shortfall(r) > 0 {
                                    cur_active_width[2]
                                } else {
                                    cur_active_width[6]
                                };
                                if g <= 0 {
                                    current_block = 5565703735569783978;
                                } else {
                                    arith_error = false;
                                    g = fract(
                                        g,
                                        *ACTIVE_NODE_shortfall(r),
                                        *ACTIVE_NODE_glue(r),
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
                                        g = if *ACTIVE_NODE_shortfall(r) > 0 {
                                            MAX_HALFWORD
                                        } else {
                                            -MAX_HALFWORD
                                        };
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
                                                fit_class = if b > 12 {
                                                    if b > 99 {
                                                        VERY_LOOSE_FIT
                                                    } else {
                                                        LOOSE_FIT
                                                    }
                                                } else {
                                                    DECENT_FIT
                                                };
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
                                        fit_class = if b > 12 {
                                            /* XXX hardcoded in WEB */
                                            TIGHT_FIT
                                        } else {
                                            DECENT_FIT
                                        };
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
                            fit_class = DECENT_FIT;
                            current_block = 8633396468472091231;
                        }
                    }
                } else {
                    let mut current_block_230: u64;
                    if shortfall as i64 > 7230584 {
                        /* XXX: magic number in original WEB code */
                        if (cur_active_width[2] as i64) < 1663497 {
                            /* XXX: magic number in original WEB code */
                            b = INF_BAD;
                            fit_class = VERY_LOOSE_FIT;
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
                            fit_class = if b > 12 {
                                if b > 99 {
                                    VERY_LOOSE_FIT
                                } else {
                                    LOOSE_FIT
                                }
                            } else {
                                DECENT_FIT
                            };
                        }
                        _ => {}
                    }
                    current_block = 8633396468472091231;
                }
            } else {
                /*882: "Set the value of b to the badness for shrinking the line,
                 * and compute the corresponding fit_class" */
                if -shortfall > cur_active_width[6] {
                    b = INF_BAD + 1
                } else {
                    b = badness(-shortfall, cur_active_width[6])
                }
                fit_class = if b > 12 { TIGHT_FIT } else { DECENT_FIT };
                current_block = 8633396468472091231;
            }
            match current_block {
                8633396468472091231 => {
                    if do_last_line_fit {
                        /*1637: "Adjust the additional data for last line" */
                        if cur_p.is_none() {
                            shortfall = 0
                        }
                        g = if shortfall > 0 {
                            cur_active_width[2]
                        } else if shortfall < 0 {
                            cur_active_width[6]
                        } else {
                            0
                        };
                    }
                }
                _ => {}
            }
            if b > INF_BAD || pi == EJECT_PENALTY {
                /*883: "Prepare to deactivate node r, and goto deactivate unless
                 * there is a reason to consider lines of text from r to cur_p" */
                if final_pass
                    && minimum_demerits == AWFUL_BAD
                    && llist_link(r) == Some(LAST_ACTIVE)
                    && prev_r == ACTIVE_LIST
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
                    let mut d;
                    if artificial_demerits {
                        d = 0
                    } else {
                        /*888: "Compute the demerits, d, from r to cur_p" */
                        d = *INTPAR(IntPar::line_penalty) + b;
                        d = if d.abs() >= 10_000 {
                            100_000_000
                        /* algorithmic constant */
                        } else {
                            d * d
                        };
                        if pi != 0 {
                            if pi > 0 {
                                d = d + pi * pi
                            } else if pi > EJECT_PENALTY {
                                d = d - pi * pi
                            }
                        }
                        if break_type == BreakType::Hyphenated
                            && MEM[r].b16.s1 == BreakType::Hyphenated as u16
                        // NODE_type(r) == HYPHENATED
                        {
                            d += if cur_p.is_some() {
                                *INTPAR(IntPar::double_hyphen_demerits)
                            } else {
                                *INTPAR(IntPar::final_hyphen_demerits)
                            };
                        }
                        if (fit_class as i32 - *ACTIVE_NODE_fitness(r) as i32).abs() > 1 {
                            d += *INTPAR(IntPar::adj_demerits);
                        }
                    }
                    /* resuming 884: */
                    d += *ACTIVE_NODE_total_demerits(r as usize);
                    if d <= minimal_demerits[fit_class as usize] {
                        minimal_demerits[fit_class as usize] = d;
                        best_place[fit_class as usize] = *ACTIVE_NODE_break_node(r);
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
            *LLIST_link(prev_r) = *LLIST_link(r);
            free_node(r, active_node_size as i32);
            if prev_r == ACTIVE_LIST {
                /*890: "Update the active widths, since the first active node has been deleted" */
                let r = llist_link(ACTIVE_LIST).unwrap(); /*:966 */
                if NODE_type(r) == DELTA_NODE.into() {
                    let r = Delta(r);
                    active_width[1] += r.dwidth();
                    active_width[2] += r.dstretch0();
                    active_width[3] += r.dstretch1();
                    active_width[4] += r.dstretch2();
                    active_width[5] += r.dstretch3();
                    active_width[6] += r.dshrink();
                    cur_active_width[1..].copy_from_slice(&active_width[1..]);
                    *LLIST_link(ACTIVE_LIST) = *LLIST_link(r.ptr());
                    free_node(r.ptr(), DELTA_NODE_SIZE);
                }
            } else if NODE_type(prev_r) == DELTA_NODE.into() {
                let r = llist_link(prev_r).unwrap();
                if r == LAST_ACTIVE {
                    let prev_r_delta = Delta(prev_r);
                    cur_active_width[1] -= prev_r_delta.dwidth();
                    cur_active_width[2] -= prev_r_delta.dstretch0();
                    cur_active_width[3] -= prev_r_delta.dstretch1();
                    cur_active_width[4] -= prev_r_delta.dstretch2();
                    cur_active_width[5] -= prev_r_delta.dstretch3();
                    cur_active_width[6] -= prev_r_delta.dshrink();
                    *LLIST_link(prev_prev_r.unwrap()) = Some(LAST_ACTIVE).tex_int();
                    free_node(prev_r, DELTA_NODE_SIZE);
                    prev_r = prev_prev_r.unwrap();
                } else if NODE_type(r) == DELTA_NODE.into() {
                    let r = Delta(r);
                    let mut prev_r_delta = Delta(prev_r);
                    cur_active_width[1] += r.dwidth();
                    cur_active_width[2] += r.dstretch0();
                    cur_active_width[3] += r.dstretch1();
                    cur_active_width[4] += r.dstretch2();
                    cur_active_width[5] += r.dstretch3();
                    cur_active_width[6] += r.dshrink();
                    prev_r_delta.set_dwidth(prev_r_delta.dwidth() + r.dwidth());
                    prev_r_delta.set_dstretch0(prev_r_delta.dstretch0() + r.dstretch0());
                    prev_r_delta.set_dstretch2(prev_r_delta.dstretch2() + r.dstretch1()); // TODO: looks like typo
                    prev_r_delta.set_dstretch2(prev_r_delta.dstretch2() + r.dstretch2());
                    prev_r_delta.set_dstretch3(prev_r_delta.dstretch3() + r.dstretch3());
                    prev_r_delta.set_dshrink(prev_r_delta.dshrink() + r.dshrink());
                    *LLIST_link(prev_r) = *LLIST_link(r.ptr());
                    free_node(r.ptr(), DELTA_NODE_SIZE);
                }
            }
        }
    }
}
unsafe fn hyphenate() {
    let mut current_block: u64;
    let mut l: i16 = 0;
    let mut bchar: i32 = 0;
    let mut c: UnicodeScalar = 0i32;
    let mut c_loc: i16 = 0;
    let mut r_count: i32 = 0;
    let mut z: trie_pointer = 0;
    let mut v: i32 = 0;
    let mut u: pool_pointer = 0;

    for j in 0..=hn {
        hyf[j as usize] = 0_u8;
    }
    let mut h = hc[1] as hyph_pointer;
    hn += 1;
    hc[hn as usize] = cur_lang as i32;
    for j in 2..=hn {
        h = ((h as i32 + h as i32 + hc[j as usize]) % HYPH_PRIME) as hyph_pointer;
    }
    loop {
        let k = HYPH_WORD[h as usize];
        if k == 0 {
            current_block = 10027897684796195291;
            break;
        }
        if length(k) == hn as i32 {
            let mut j = 1_i16;
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
                    let mut sopt = HYPH_LIST[h as usize];
                    while let Some(s) = sopt {
                        hyf[MEM[s].b32.s0 as usize] = 1_u8;
                        sopt = llist_link(s);
                    }
                    hn -= 1;
                    current_block = 15736053877802236303;
                    break;
                }
            }
        }
        h = HYPH_LINK[h as usize];
        if h == 0 {
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
            for j in 0..=(hn as i32 - r_hyf + 1) {
                z = *trie_trl.offset((cur_lang as i32 + 1) as isize) + hc[j as usize];
                l = j as i16;
                while hc[l as usize] == *trie_trc.offset(z as isize) as i32 {
                    if *trie_tro.offset(z as isize) != MIN_TRIE_OP as i32 {
                        /*959: */
                        v = *trie_tro.offset(z as isize); /*:958 */
                        loop {
                            v = v + op_start[cur_lang as usize];
                            let i = (l as i32 - hyf_distance[v as usize] as i32) as i16;
                            if hyf_num[v as usize] as i32 > hyf[i as usize] as i32 {
                                hyf[i as usize] = hyf_num[v as usize] as u8
                            }
                            v = hyf_next[v as usize] as i32;
                            if v == MIN_TRIE_OP as i32 {
                                break;
                            }
                        }
                    }
                    l += 1;
                    z = *trie_trl.offset(z as isize) + hc[l as usize]
                }
            }
        }
        _ => {}
    }
    for j in 0..l_hyf {
        hyf[j as usize] = 0_u8;
    }
    for j in 0..r_hyf {
        hyf[(hn as i32 - j as i32) as usize] = 0_u8;
    }
    let mut j = l_hyf as i16;
    let mut for_end_4 = hn as i32 - r_hyf;
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
    if !is_char_node(Some(ha))
        && NODE_type(ha) == TextNode::WhatsIt.into()
        && (whatsit_NODE_subtype(ha) == WhatsItNST::NativeWord
            || whatsit_NODE_subtype(ha) == WhatsItNST::NativeWordAt)
    {
        let ha_nw = NativeWord::from(ha);
        let mut s = cur_p.unwrap();
        while llist_link(s) != Some(ha) {
            s = *LLIST_link(s) as usize;
        }
        hyphen_passed = 0;
        for j in l_hyf..=(hn as i32 - r_hyf) {
            if hyf[j as usize] as i32 & 1i32 != 0 {
                let mut q = new_native_word_node(hf, j as i32 - hyphen_passed as i32);
                set_whatsit_NODE_subtype(q.ptr(), whatsit_NODE_subtype(ha));

                let ha_text = ha_nw.text();
                q.text_mut()
                    .copy_from_slice(&ha_text[hyphen_passed as usize..j as usize]);

                q.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
                *LLIST_link(s) = Some(q.ptr()).tex_int();
                s = q.ptr();
                let q = new_disc();
                *DISCRETIONARY_NODE_pre_break(q) = new_native_character(hf, hyf_char).ptr() as i32;
                *LLIST_link(s) = Some(q).tex_int();
                s = q;
                hyphen_passed = j as i16;
            }
        }
        let ha_text = ha_nw.text();
        hn = ha_text.len() as i16;
        let mut q = new_native_word_node(hf, hn as i32 - hyphen_passed as i32);
        set_whatsit_NODE_subtype(q.ptr(), whatsit_NODE_subtype(ha));
        q.text_mut()
            .copy_from_slice(&ha_text[(hyphen_passed as usize)..]);

        q.set_metrics(*INTPAR(IntPar::xetex_use_glyph_metrics) > 0);
        *LLIST_link(s) = Some(q.ptr()).tex_int();
        s = q.ptr();
        let q = *LLIST_link(ha);
        *LLIST_link(s) = q;
        *LLIST_link(ha) = None.tex_int();
        flush_node_list(Some(ha));
    } else {
        let mut s = 0; // TODO: check
        let q = *LLIST_link(hb);
        *LLIST_link(hb) = None.tex_int();
        let r = *LLIST_link(ha);
        *LLIST_link(ha) = None.tex_int();
        bchar = hyf_bchar;
        if is_char_node(Some(ha)) {
            if MEM[ha].b16.s1 as usize != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = Some(ha).tex_int();
                init_lig = false;
                hu[0] = MEM[ha].b16.s0 as i32;
                current_block = 6662862405959679103;
            }
        } else if NODE_type(ha) == TextNode::Ligature.into() {
            let l = Ligature(ha);
            if l.font() as usize != hf {
                current_block = 6826215413708131726;
            } else {
                init_list = l.lig_ptr();
                init_lig = true;
                init_lft = l.left_hit();
                hu[0] = l.char() as i32;
                if init_list.opt().is_none() {
                    if init_lft {
                        hu[0] = max_hyph_char;
                        init_lig = false
                    }
                }
                l.free();
                current_block = 6662862405959679103;
            }
        } else {
            if !is_char_node(r.opt()) {
                if NODE_type(r as usize) == TextNode::Ligature.into() {
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
                    s = ha as i32;
                    init_list = None.tex_int();
                    current_block = 5209103994167801282;
                }
            }
        }
        match current_block {
            6662862405959679103 => {
                s = cur_p.tex_int();
                while LLIST_link(s as usize).opt() != Some(ha) {
                    s = *LLIST_link(s as usize);
                }
                j = 0_i16
            }
            6826215413708131726 => {
                s = ha as i32;
                j = 0_i16;
                hu[0] = max_hyph_char;
                init_lig = false;
                init_list = None.tex_int()
            }
            _ => {}
        }
        flush_node_list(r.opt());
        loop {
            l = j;
            j = (reconstitute(j, hn, bchar, hyf_char) as i32 + 1) as i16;
            if hyphen_passed == 0 {
                *LLIST_link(s as usize) = *LLIST_link(HOLD_HEAD);
                while let Some(next) = LLIST_link(s as usize).opt() {
                    s = next as i32
                }
                if hyf[(j as i32 - 1i32) as usize] as i32 & 1i32 != 0 {
                    l = j;
                    hyphen_passed = (j as i32 - 1i32) as i16;
                    *LLIST_link(HOLD_HEAD) = None.tex_int()
                }
            }
            if hyphen_passed as i32 > 0 {
                loop
                /*949: */
                {
                    let r = get_node(SMALL_NODE_SIZE);
                    *LLIST_link(r) = *LLIST_link(HOLD_HEAD);
                    set_NODE_type(r, TextNode::Disc);
                    let mut major_tail = r;
                    r_count = 0;
                    while let Some(next) = LLIST_link(major_tail as usize).opt() {
                        major_tail = next;
                        r_count += 1;
                    }
                    let mut i = hyphen_passed;
                    hyf[i as usize] = 0;
                    let mut minor_tail: Option<usize> = None;
                    *DISCRETIONARY_NODE_pre_break(r) = None.tex_int();
                    let hyf_node = new_character(hf, hyf_char as UTF16_code);
                    if let Some(hyf_node) = hyf_node {
                        i += 1;
                        c = hu[i as usize];
                        hu[i as usize] = hyf_char;
                        *LLIST_link(hyf_node) = avail.tex_int();
                        avail = Some(hyf_node);
                    }
                    while l as i32 <= i as i32 {
                        l = (reconstitute(l, i, FONT_BCHAR[hf as usize], TOO_BIG_CHAR) as i32 + 1)
                            as i16;
                        if let Some(hh) = llist_link(HOLD_HEAD) {
                            if let Some(mt) = minor_tail {
                                *LLIST_link(mt) = Some(hh).tex_int();
                            } else {
                                *DISCRETIONARY_NODE_pre_break(r) = Some(hh).tex_int();
                            }
                            let mut mt = hh;
                            minor_tail = Some(mt);
                            while let Some(next) = llist_link(mt) {
                                mt = next;
                                minor_tail = Some(next);
                            }
                        }
                    }
                    if hyf_node.is_some() {
                        hu[i as usize] = c;
                        l = i;
                        i -= 1
                    }
                    let mut minor_tail: Option<usize> = None;
                    MEM[r + 1].b32.s1 = None.tex_int();
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
                            if let Some(hh) = llist_link(HOLD_HEAD) {
                                if let Some(mt) = minor_tail {
                                    *LLIST_link(mt) = Some(hh).tex_int();
                                } else {
                                    *DISCRETIONARY_NODE_post_break(r) = Some(hh).tex_int();
                                }
                                let mut mt = hh;
                                minor_tail = Some(mt);
                                while let Some(next) = llist_link(mt) {
                                    mt = next;
                                    minor_tail = Some(next);
                                }
                            }
                            if l as i32 >= j as i32 {
                                break;
                            }
                        }
                        while l as i32 > j as i32 {
                            /*952: */
                            j = (reconstitute(j, hn, bchar, TOO_BIG_CHAR) as i32 + 1i32) as i16; /*:944*/
                            *LLIST_link(major_tail) = *LLIST_link(HOLD_HEAD);
                            while let Some(next) = llist_link(major_tail) {
                                major_tail = next;
                                r_count += 1;
                            }
                        }
                    }
                    if r_count > 127 {
                        *LLIST_link(s as usize) = *LLIST_link(r);
                        *LLIST_link(r) = None.tex_int();
                        flush_node_list(Some(r));
                    } else {
                        *LLIST_link(s as usize) = Some(r).tex_int();
                        *DISCRETIONARY_NODE_replace_count(r) = r_count as u16;
                    }
                    s = major_tail as i32;
                    hyphen_passed = (j - 1) as i16;
                    *LLIST_link(HOLD_HEAD) = None.tex_int();
                    if !(hyf[(j as i32 - 1i32) as usize] as i32 & 1i32 != 0) {
                        break;
                    }
                }
            }
            if j as i32 > hn as i32 {
                break;
            }
        }
        *LLIST_link(s as usize) = q;
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
        help!(
            b"The paragraph just ended includes some glue that has",
            b"infinite shrinkability, e.g., `\\hskip 0pt minus 1fil\'.",
            b"Such glue doesn\'t belong there---it allows a paragraph",
            b"of any length to fit on one line. But it\'s safe to proceed,",
            b"since the offensive shrinkability has been made finite."
        );
        error();
    }
    let mut q = GlueSpec(new_spec(p));
    q.set_shrink_order(GlueOrder::Normal);
    delete_glue_ref(p);
    q.ptr()
}
unsafe fn reconstitute(mut j: i16, mut n: i16, mut bchar: i32, mut hchar: i32) -> i16 {
    let mut current_block: u64;
    let mut q: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut k: font_index = 0;

    hyphen_passed = 0;
    let mut t = HOLD_HEAD as i32;
    let mut w = 0;
    *LLIST_link(HOLD_HEAD) = None.tex_int();
    cur_l = hu[j as usize];
    cur_q = t;
    if j == 0 {
        ligature_present = init_lig;
        let mut popt = init_list.opt();
        if ligature_present {
            lft_hit = init_lft
        }
        while let Some(p) = popt {
            *LLIST_link(t as usize) = Some(get_avail()).tex_int();
            t = *LLIST_link(t as usize);
            MEM[t as usize].b16.s1 = hf as u16;
            MEM[t as usize].b16.s0 = MEM[p].b16.s0;
            popt = llist_link(p)
        }
    } else if cur_l < TOO_BIG_CHAR {
        *LLIST_link(t as usize) = Some(get_avail()).tex_int();
        t = *LLIST_link(t as usize);
        MEM[t as usize].b16.s1 = hf as u16;
        MEM[t as usize].b16.s0 = cur_l as u16
    }
    lig_stack = None;
    cur_r = if (j as i32) < n as i32 {
        hu[(j + 1) as usize]
    } else {
        bchar
    };
    let mut cur_rh = if hyf[j as usize] as i32 & 1i32 != 0 {
        hchar
    } else {
        TOO_BIG_CHAR
    };
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
                let test_char = if cur_rh < TOO_BIG_CHAR { cur_rh } else { cur_r };
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
                                        if lig_stack.is_none() {
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
                                            if let Some(ls) = lig_stack {
                                                MEM[ls].b16.s0 = cur_r as u16
                                            } else {
                                                let ls = new_lig_item(cur_r as u16);
                                                lig_stack = Some(ls);
                                                if j as i32 == n as i32 {
                                                    bchar = TOO_BIG_CHAR
                                                } else {
                                                    let p = get_avail();
                                                    MEM[ls + 1].b32.s1 = Some(p).tex_int();
                                                    MEM[p].b16.s0 =
                                                        hu[(j as i32 + 1i32) as usize] as u16;
                                                    MEM[p].b16.s1 = hf as u16
                                                }
                                            }
                                        }
                                        3 => {
                                            cur_r = q.s0 as i32;
                                            let p = lig_stack;
                                            let ls = new_lig_item(cur_r as u16);
                                            lig_stack = Some(ls);
                                            *LLIST_link(ls) = p.tex_int()
                                        }
                                        7 | 11 => {
                                            if ligature_present {
                                                let p = new_ligature(
                                                    hf,
                                                    cur_l as u16,
                                                    *LLIST_link(cur_q as usize),
                                                );
                                                if lft_hit {
                                                    MEM[p as usize].b16.s0 = 2_u16;
                                                    lft_hit = false
                                                }
                                                *LLIST_link(cur_q as usize) = Some(p).tex_int();
                                                t = p as i32;
                                                ligature_present = false
                                            }
                                            cur_q = t;
                                            cur_l = q.s0 as i32;
                                            ligature_present = true
                                        }
                                        _ => {
                                            cur_l = q.s0 as i32;
                                            ligature_present = true;
                                            if let Some(ls) = lig_stack {
                                                if let Some(l) = MEM[(ls + 1) as usize].b32.s1.opt()
                                                {
                                                    *LLIST_link(t as usize) = Some(l).tex_int();
                                                    t = *LLIST_link(t as usize);
                                                    j += 1
                                                }
                                                lig_stack = llist_link(ls);
                                                free_node(ls, SMALL_NODE_SIZE);
                                                if let Some(ls) = lig_stack {
                                                    cur_r = MEM[ls].b16.s0 as i32
                                                } else {
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
                                                }
                                            } else {
                                                if j as i32 == n as i32 {
                                                    break;
                                                }
                                                *LLIST_link(t as usize) = get_avail() as i32;
                                                t = *LLIST_link(t as usize);
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
            let p = new_ligature(hf, cur_l as u16, *LLIST_link(cur_q as usize));
            if lft_hit {
                MEM[p].b16.s0 = 2;
                lft_hit = false
            }
            if rt_hit {
                if lig_stack.is_none() {
                    MEM[p as usize].b16.s0 += 1;
                    rt_hit = false
                }
            }
            *LLIST_link(cur_q as usize) = Some(p).tex_int();
            t = p as i32;
            ligature_present = false
        }
        if w != 0 {
            *LLIST_link(t as usize) = new_kern(w) as i32;
            t = *LLIST_link(t as usize);
            w = 0;
            MEM[(t + 2) as usize].b32.s0 = 0
        }
        if let Some(ls) = lig_stack {
            cur_q = t;
            cur_l = MEM[ls].b16.s0 as i32;
            ligature_present = true;
            if let Some(l) = MEM[ls + 1].b32.s1.opt() {
                *LLIST_link(t as usize) = Some(l).tex_int();
                t = *LLIST_link(t as usize);
                j += 1;
            }
            let p = ls;
            lig_stack = llist_link(p);
            free_node(p, SMALL_NODE_SIZE);
            if let Some(ls) = lig_stack {
                cur_r = MEM[ls].b16.s0 as i32
            } else {
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
            }
        } else {
            break;
        }
    }
    j
}
unsafe fn total_pw(q: usize, p: Option<usize>) -> scaled_t {
    let mut lopt = if let Some(r) = MEM[(q + 1) as usize].b32.s1.opt() {
        MEM[r + 1].b32.s1.opt()
    } else {
        first_p.opt()
    };
    let mut r = prev_rightmost(global_prev_p.opt(), p);
    match p {
        Some(p)
            if NODE_type(p) == TextNode::Disc.into()
                && DISCRETIONARY_NODE_pre_break(p).opt().is_some() =>
        {
            if let Some(mut m) = DISCRETIONARY_NODE_pre_break(p).opt() {
                while let Some(next) = llist_link(m) {
                    m = next;
                }
                r = Some(m);
            }
        }
        _ => r = find_protchar_right(lopt, r),
    }
    let l = match lopt {
        Some(mut l) if NODE_type(l) == TextNode::Disc.into() => {
            if let Some(l1) = DISCRETIONARY_NODE_post_break(l).opt() {
                l = l1;
                return char_pw(Some(l), Side::Left) + char_pw(r, Side::Right);
            } else {
                let mut n = *DISCRETIONARY_NODE_replace_count(l);
                l = llist_link(l).unwrap();
                while n > 0 {
                    if let Some(next) = llist_link(l) {
                        l = next;
                    }
                    n -= 1;
                }
            }
            l
        }
        Some(l) => l,
        None => panic!(),
    };
    let l = find_protchar_left(l, true);
    char_pw(Some(l), Side::Left) + char_pw(r, Side::Right)
}
unsafe fn find_protchar_left(mut l: usize, mut d: bool) -> usize {
    let mut run: bool = false;
    match llist_link(l) {
        Some(next)
            if NODE_type(l) == TextNode::HList.into()
                && *BOX_width(l) == 0
                && *BOX_height(l) == 0
                && *BOX_depth(l) == 0
                && Box::from(l).list_ptr().opt().is_none() =>
        {
            l = next
        }
        _ => {
            if d {
                while let Some(next) = llist_link(l) {
                    if is_char_node(Some(l)) || is_non_discardable_node(l) {
                        break;
                    }
                    l = next;
                }
            }
        }
    }
    let mut hlist_stack: Vec<usize> = Vec::new();
    run = true;
    loop {
        let t = l;
        while run && NODE_type(l) == TextNode::HList.into() {
            if let Some(next) = Box::from(l).list_ptr().opt() {
                hlist_stack.push(l);
                l = next;
            } else {
                break;
            }
        }
        while run
            && (!is_char_node(Some(l))
                && (NODE_type(l) == TextNode::Ins.into()
                    || NODE_type(l) == TextNode::Mark.into()
                    || NODE_type(l) == TextNode::Adjust.into()
                    || NODE_type(l) == TextNode::Penalty.into()
                    || NODE_type(l) == TextNode::Disc.into()
                        && DISCRETIONARY_NODE_pre_break(l).opt().is_none()
                        && DISCRETIONARY_NODE_post_break(l).opt().is_none()
                        && *DISCRETIONARY_NODE_replace_count(l) == 0
                    || NODE_type(l) == TextNode::Math.into() && MEM[l + 1].b32.s1 == 0
                    || NODE_type(l) == TextNode::Kern.into()
                        && (MEM[l + 1].b32.s1 == 0 || MEM[l].b16.s0 == NORMAL)
                    || NODE_type(l) == TextNode::Glue.into() && MEM[l + 1].b32.s0 == 0
                    || NODE_type(l) == TextNode::HList.into()
                        && *BOX_width(l) == 0
                        && *BOX_height(l) == 0
                        && *BOX_depth(l) == 0
                        && Box::from(l).list_ptr().opt().is_none()))
        {
            while LLIST_link(l as usize).opt().is_none() {
                if let Some(last) = hlist_stack.pop() {
                    l = last;
                } else {
                    break;
                }
            }
            if let Some(next) = llist_link(l) {
                l = next;
            } else if hlist_stack.is_empty() {
                run = false
            }
        }
        if t == l {
            break;
        }
    }
    l
}
unsafe fn find_protchar_right(mut l: Option<usize>, mut r: Option<usize>) -> Option<usize> {
    let mut run: bool = false;
    if r.is_none() {
        return None;
    }
    let mut r = r.unwrap();
    let mut hlist_stack: Vec<(Option<usize>, usize)> = Vec::new();
    run = true;
    loop {
        let t = r;
        while run && NODE_type(r) == TextNode::HList.into() {
            if let Some(hnext) = Box::from(r).list_ptr().opt() {
                hlist_stack.push((l, r));
                l = Some(hnext);
                r = hnext;
                while let Some(next) = llist_link(r) {
                    r = next;
                }
            } else {
                break;
            }
        }
        while run
            && (!is_char_node(Some(r))
                && (NODE_type(r) == TextNode::Ins.into()
                    || NODE_type(r) == TextNode::Mark.into()
                    || NODE_type(r) == TextNode::Adjust.into()
                    || NODE_type(r) == TextNode::Penalty.into()
                    || NODE_type(r) == TextNode::Disc.into()
                        && DISCRETIONARY_NODE_pre_break(r).opt().is_none()
                        && DISCRETIONARY_NODE_post_break(r).opt().is_none()
                        && *DISCRETIONARY_NODE_replace_count(r) == 0
                    || NODE_type(r) == TextNode::Math.into() && MEM[r + 1].b32.s1 == 0
                    || NODE_type(r) == TextNode::Kern.into()
                        && (MEM[r + 1].b32.s1 == 0 || kern_NODE_subtype(r) == KernNST::Normal)
                    || NODE_type(r) == TextNode::Glue.into() && MEM[r + 1].b32.s0 == 0
                    || NODE_type(r) == TextNode::HList.into()
                        && *BOX_width(r) == 0
                        && *BOX_height(r) == 0
                        && *BOX_depth(r) == 0
                        && Box::from(r).list_ptr().opt().is_none()))
        {
            while Some(r) == l {
                if let Some(last) = hlist_stack.pop() {
                    l = last.0;
                    r = last.1;
                } else {
                    break;
                }
            }
            if Some(r) != l
            /* && r.is_some()*/
            {
                r = prev_rightmost(l, Some(r)).unwrap();
            } else if Some(r) == l && hlist_stack.is_empty() {
                run = false
            }
        }
        if t == r {
            break;
        }
    }
    Some(r)
}
