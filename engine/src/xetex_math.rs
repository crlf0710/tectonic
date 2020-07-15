#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use core::ptr;

use crate::help;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, Confuse};
use crate::xetex_ext::{
    map_char_to_glyph, measure_native_glyph, real_get_native_glyph, AAT_FONT_FLAG, OTGR_FONT_FLAG,
};
use crate::xetex_ini::{
    adjust_tail, avail, cur_c, cur_chr, cur_cmd, cur_dir, cur_f, cur_group, cur_i, cur_lang,
    cur_list, cur_val, cur_val1, empty, file_line_error_style_p, insert_src_special_every_math,
    just_box, pre_adjust_tail, tex_remainder, total_shrink, xtx_ligature_present, LR_problems,
    LR_ptr, CHAR_BASE, DEPTH_BASE, EQTB, EXTEN_BASE, FONT_AREA, FONT_BC, FONT_EC, FONT_INFO,
    FONT_LAYOUT_ENGINE, FONT_PARAMS, HEIGHT_BASE, ITALIC_BASE, KERN_BASE, LIG_KERN_BASE, MEM,
    NEST_PTR, NULL_CHARACTER, PARAM_BASE, SAVE_PTR, SAVE_STACK, SKEW_CHAR, WIDTH_BASE,
};
use crate::xetex_ini::{b16x4, b16x4_le_t, memory_word};
use crate::xetex_layout_interface::*;
use crate::xetex_linebreak::line_break;
use crate::xetex_output::{
    print, print_char, print_cstr, print_esc_cstr, print_file_line, print_int, print_nl_cstr,
    print_size,
};
use crate::xetex_pagebuilder::build_page;
use crate::xetex_scaledmath::{half, mult_and_add, tex_round, x_over_n, xn_over_d};
use crate::xetex_xetex0::{
    append_to_vlist, back_error, back_input, begin_token_list, char_warning, copy_node_list,
    delete_glue_ref, effective_char, eq_word_define, flush_node_list, free_node, get_avail,
    get_node, get_token, get_x_token, hpack, insert_src_special, internal_font_number, just_copy,
    just_reverse, new_character, new_choice, new_glue, new_kern, new_math, new_native_character,
    new_noad, new_null_box, new_param_glue, new_penalty, new_rule, new_skip_param, new_spec,
    norm_min, off_save, pop_nest, push_math, push_nest, scan_delimiter_int, scan_dimen,
    scan_fifteen_bit_int, scan_keyword, scan_left_brace, scan_math, scan_math_class_int,
    scan_math_fam_int, scan_usv_num, unsave, vpackage,
};
use crate::xetex_xetexd::{
    is_char_node, kern_NODE_width, math_char, math_class, math_fam, set_BOX_lr_mode, set_NODE_type,
    set_class, set_family, set_kern_NODE_subtype, set_whatsit_NODE_subtype, text_NODE_type,
    whatsit_NODE_subtype, BOX_depth, BOX_glue_order, BOX_glue_set, BOX_glue_sign, BOX_height,
    BOX_list_ptr, BOX_shift_amount, BOX_width, CHAR_NODE_character, CHAR_NODE_font,
    CHOICE_NODE_display, CHOICE_NODE_script, CHOICE_NODE_scriptscript, CHOICE_NODE_text,
    GLUE_NODE_glue_ptr, GLUE_SPEC_shrink, GLUE_SPEC_shrink_order, GLUE_SPEC_size,
    GLUE_SPEC_stretch, GLUE_SPEC_stretch_order, LIGATURE_NODE_lig_char, LIGATURE_NODE_lig_font,
    LLIST_link, NATIVE_NODE_font, NATIVE_NODE_glyph, NATIVE_NODE_size, NODE_type, TeXInt, TeXOpt,
};

pub(crate) type scaled_t = i32;
/* ***************************************************************************\
 Part of the XeTeX typesetting system
 Copyright (c) 1994-2008 by SIL International
 Copyright (c) 2009 by Jonathan Kew

 SIL Author(s): Jonathan Kew

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the copyright holders
shall not be used in advertising or otherwise to promote the sale,
use or other dealings in this Software without prior written
authorization from the copyright holders.
\****************************************************************************/
pub(crate) type UTF16_code = u16;
pub(crate) type UnicodeScalar = i32;
pub(crate) type str_number = i32;
static mut null_delimiter: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
static mut cur_mlist: i32 = 0;
static mut cur_style: (MathStyle, u8) = (MathStyle::Display, 0);
static mut cur_size: usize = 0;
static mut cur_mu: scaled_t = 0;
static mut mlist_penalties: bool = false;
pub(crate) unsafe fn initialize_math_variables() {
    null_delimiter.s3 = 0_u16;
    null_delimiter.s2 = 0_u16;
    null_delimiter.s1 = 0_u16;
    null_delimiter.s0 = 0_u16;
}
pub(crate) unsafe fn init_math() {
    let mut x: i32 = 0;
    let mut v: scaled_t = 0;

    get_token();

    if cur_cmd == Cmd::MathShift && cur_list.mode.0 == false {
        // 1180:
        let mut j = None;
        let mut w = -MAX_HALFWORD;
        if cur_list.head == cur_list.tail {
            // 1520:
            pop_nest();
            x = if let Some(aux) = cur_list.eTeX_aux {
                if MathNST::from(MEM[aux].b32.s0 as u16).dir() == LR::RightToLeft {
                    -1
                } else {
                    1 // :1519
                }
            } else {
                0
            };
        } else {
            line_break(true);
            // 1528:
            j = Some(if *GLUEPAR(GluePar::right_skip) == 0 {
                new_kern(0)
            } else {
                new_param_glue(GluePar::right_skip)
            });

            let p = if *GLUEPAR(GluePar::left_skip) == 0 {
                new_kern(0)
            } else {
                new_param_glue(GluePar::left_skip)
            };

            *LLIST_link(p) = j.tex_int();

            let j_ = new_null_box();
            j = Some(j_);
            *BOX_width(j_) = *BOX_width(just_box);
            *BOX_shift_amount(j_) = *BOX_shift_amount(just_box);
            *BOX_list_ptr(j_) = Some(p).tex_int();
            *BOX_glue_order(j_) = *BOX_glue_order(just_box);
            *BOX_glue_sign(j_) = *BOX_glue_sign(just_box);
            *BOX_glue_set(j_) = *BOX_glue_set(just_box);

            v = *BOX_shift_amount(just_box);
            x = if let Some(aux) = cur_list.eTeX_aux {
                if MathNST::from(MEM[aux].b32.s0 as u16).dir() == LR::RightToLeft {
                    -1
                } else {
                    1 // :1519
                }
            } else {
                0
            };
            let mut popt = if x >= 0 {
                let p = BOX_list_ptr(just_box).opt();
                *LLIST_link(TEMP_HEAD) = None.tex_int();
                p
            } else {
                v = -v - *BOX_width(just_box);
                let p = new_math(0, BEGIN_L_CODE);
                *LLIST_link(TEMP_HEAD) = p as i32;
                just_copy(
                    BOX_list_ptr(just_box).opt(),
                    p,
                    new_math(0, END_L_CODE) as i32,
                );
                cur_dir = LR::RightToLeft;
                Some(p)
            };
            v = v + 2i32
                * FONT_INFO
                    [(QUAD_CODE + PARAM_BASE[EQTB[(CUR_FONT_LOC) as usize].val as usize]) as usize]
                    .b32
                    .s1;
            if *INTPAR(IntPar::texxet) > 0 {
                // 1497:
                let tmp_ptr = get_avail(); /*1523:*/
                MEM[tmp_ptr].b32.s0 = u16::from(MathNST::Before) as i32; /*:1398 */
                *LLIST_link(tmp_ptr) = LR_ptr;
                LR_ptr = Some(tmp_ptr).tex_int();
            }
            while let Some(mut p) = popt {
                let found;
                let d;
                if is_char_node(Some(p)) {
                    let f = *CHAR_NODE_font(p) as internal_font_number;
                    d = FONT_INFO[(WIDTH_BASE[f]
                        + FONT_INFO
                            [(CHAR_BASE[f] + effective_char(true, f, MEM[p].b16.s0)) as usize]
                            .b16
                            .s3 as i32) as usize]
                        .b32
                        .s1;
                    found = true;
                } else {
                    match text_NODE_type(p).unwrap() {
                        TextNode::HList | TextNode::VList | TextNode::Rule => {
                            d = *BOX_width(p);
                            found = true;
                        }
                        TextNode::Ligature => {
                            *CHAR_NODE_character(GARBAGE) = *LIGATURE_NODE_lig_char(p);
                            *CHAR_NODE_font(GARBAGE) = *LIGATURE_NODE_lig_font(p);
                            *LLIST_link(GARBAGE) = *LLIST_link(p);
                            popt = Some(GARBAGE);
                            xtx_ligature_present = true;
                            continue;
                        }
                        TextNode::Kern => {
                            d = *kern_NODE_width(p);
                            found = false;
                        }
                        TextNode::MarginKern => {
                            d = *kern_NODE_width(p);
                            found = false;
                        }
                        TextNode::Math => {
                            d = MEM[p + 1].b32.s1;
                            if *INTPAR(IntPar::texxet) > 0 {
                                /*1525: */
                                if MEM[p].b16.s0 as i32 & 1 != 0 {
                                    if MEM[LR_ptr as usize].b32.s0
                                        == 4i32 * (MEM[p].b16.s0 as i32 / 4) + 3
                                    {
                                        let tmp_ptr = LR_ptr as usize;
                                        LR_ptr = *LLIST_link(tmp_ptr);
                                        *LLIST_link(tmp_ptr) = avail.tex_int();
                                        avail = Some(tmp_ptr);
                                    } else if MEM[p].b16.s0 as i32 > 4 {
                                        w = MAX_HALFWORD;
                                        break;
                                    }
                                } else {
                                    let tmp_ptr = get_avail();
                                    MEM[tmp_ptr].b32.s0 = 4i32 * (MEM[p].b16.s0 as i32 / 4) + 3;
                                    *LLIST_link(tmp_ptr) = LR_ptr;
                                    LR_ptr = Some(tmp_ptr).tex_int();
                                    if MEM[p].b16.s0 as i32 / 8 != cur_dir as i32 {
                                        just_reverse(p);
                                        p = TEMP_HEAD;
                                    }
                                }
                                found = false;
                            } else {
                                if MEM[p].b16.s0 as i32 >= 4 {
                                    w = MAX_HALFWORD;
                                    break;
                                } else {
                                    found = false;
                                }
                            }
                        }
                        TextNode::Style => {
                            d = MEM[(p + 1) as usize].b32.s1;
                            cur_dir = LR::n(MEM[p as usize].b16.s0).unwrap();
                            found = false;
                        }
                        TextNode::Glue => {
                            let q = *GLUE_NODE_glue_ptr(p) as usize;
                            d = *GLUE_SPEC_size(q);
                            if *BOX_glue_sign(just_box) == GlueSign::Stretching as u16 {
                                if *BOX_glue_order(just_box) == *GLUE_SPEC_stretch_order(q)
                                    && *GLUE_SPEC_stretch(q) != 0
                                {
                                    v = MAX_HALFWORD
                                }
                            } else if *BOX_glue_sign(just_box) == GlueSign::Shrinking as u16 {
                                if *BOX_glue_order(just_box) == *GLUE_SPEC_shrink_order(q)
                                    && *GLUE_SPEC_shrink(q) != 0
                                {
                                    v = MAX_HALFWORD
                                }
                            }
                            if MEM[p].b16.s0 >= A_LEADERS {
                                found = true;
                            } else {
                                found = false;
                            }
                        }
                        TextNode::WhatsIt => match whatsit_NODE_subtype(p) {
                            WhatsItNST::NativeWord
                            | WhatsItNST::NativeWordAt
                            | WhatsItNST::Glyph
                            | WhatsItNST::Pic
                            | WhatsItNST::Pdf => {
                                d = *BOX_width(p);
                                found = true;
                            }
                            _ => {
                                d = 0;
                                found = false;
                            }
                        },
                        _ => {
                            d = 0;
                            found = false;
                        }
                    }
                }
                if found {
                    if v < MAX_HALFWORD {
                        v = v + d;
                        w = v
                    } else {
                        w = MAX_HALFWORD;
                        break;
                    }
                } else {
                    if v < MAX_HALFWORD {
                        v = v + d
                    }
                }
                popt = LLIST_link(p).opt();
            }
            if *INTPAR(IntPar::texxet) > 0 {
                while let Some(l) = LR_ptr.opt() {
                    let tmp_ptr = l;
                    LR_ptr = *LLIST_link(tmp_ptr);
                    *LLIST_link(tmp_ptr) = avail.tex_int();
                    avail = Some(tmp_ptr);
                }
                if LR_problems != 0 {
                    w = MAX_HALFWORD;
                    LR_problems = 0
                }
            }
            cur_dir = LR::LeftToRight;
            flush_node_list(LLIST_link(TEMP_HEAD).opt());
        }
        let s;
        let l;
        if let Some(ps) = LOCAL(Local::par_shape).opt() {
            let n = MEM[ps].b32.s0;
            let p = if cur_list.prev_graf + 2 >= n {
                ps + 2 * (n as usize)
            } else {
                ps + 2 * (cur_list.prev_graf as usize + 2)
            };
            s = MEM[p - 1].b32.s1;
            l = MEM[p].b32.s1;
        } else {
            if *DIMENPAR(DimenPar::hang_indent) != 0
                && (*INTPAR(IntPar::hang_after) >= 0
                    && cur_list.prev_graf + 2 > *INTPAR(IntPar::hang_after)
                    || cur_list.prev_graf + 1 < -(*INTPAR(IntPar::hang_after) as i32))
            {
                l = *DIMENPAR(DimenPar::hsize) - (*DIMENPAR(DimenPar::hang_indent)).abs();
                if *DIMENPAR(DimenPar::hang_indent) > 0 {
                    s = *DIMENPAR(DimenPar::hang_indent)
                } else {
                    s = 0
                }
            } else {
                l = *DIMENPAR(DimenPar::hsize);
                s = 0
            }
        }
        push_math(GroupCode::MathShift);
        cur_list.mode = (false, ListMode::MMode);
        eq_word_define(INT_BASE as usize + IntPar::cur_fam as usize, -1i32);
        eq_word_define(DIMEN_BASE as usize + DimenPar::pre_display_size as usize, w);
        cur_list.eTeX_aux = j;
        eq_word_define(
            INT_BASE as usize + IntPar::pre_display_correction as usize,
            x,
        );
        eq_word_define(DIMEN_BASE as usize + DimenPar::display_width as usize, l);
        eq_word_define(DIMEN_BASE as usize + DimenPar::display_indent as usize, s);
        if let Some(ed) = LOCAL(Local::every_display).opt() {
            begin_token_list(ed, Btl::EveryDisplayText);
        }
        if NEST_PTR == 1 {
            build_page();
        }
    } else {
        back_input();
        push_math(GroupCode::MathShift);
        eq_word_define(INT_BASE as usize + IntPar::cur_fam as usize, -1);
        if insert_src_special_every_math {
            insert_src_special();
        }
        if let Some(em) = LOCAL(Local::every_math).opt() {
            begin_token_list(em, Btl::EveryMathText);
        }
    };
}
pub(crate) unsafe fn start_eq_no() {
    SAVE_STACK[SAVE_PTR + 0].val = cur_chr;
    SAVE_PTR += 1;
    push_math(GroupCode::MathShift);
    eq_word_define(INT_BASE as usize + (IntPar::cur_fam as usize), -1);
    if insert_src_special_every_math {
        insert_src_special();
    }
    if let Some(em) = LOCAL(Local::every_math).opt() {
        begin_token_list(em, Btl::EveryMathText);
    };
}
pub(crate) unsafe fn math_limit_switch() {
    if cur_list.head != cur_list.tail {
        if MEM[cur_list.tail].b16.s1 == MathNode::Op as u16 {
            MEM[cur_list.tail].b16.s0 = cur_chr as u16;
            return;
        }
    }
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr(b"! ");
    }
    print_cstr(b"Limit controls must follow a math operator");
    help!(b"I\'m ignoring this misplaced \\limits or \\nolimits command.");
    error();
}
unsafe fn scan_delimiter(p: usize, mut r: bool) {
    if r {
        if cur_chr == 1 {
            cur_val1 = 0x40000000;
            scan_math_fam_int();
            cur_val1 += cur_val * 0x200000;
            scan_usv_num();
            cur_val += cur_val1
        } else {
            scan_delimiter_int();
        }
    } else {
        loop {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
                break;
            }
        }
        match cur_cmd {
            Cmd::Letter | Cmd::OtherChar => {
                cur_val = EQTB[(DEL_CODE_BASE as i32 + cur_chr) as usize].val
            }
            Cmd::DelimNum => {
                if cur_chr == 1 {
                    cur_val1 = 0x40000000;
                    scan_math_class_int();
                    scan_math_fam_int();
                    cur_val1 += cur_val * 0x20000;
                    scan_usv_num();
                    cur_val += cur_val1
                } else {
                    scan_delimiter_int();
                }
            }
            _ => cur_val = -1,
        }
    }
    if cur_val < 0 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Missing delimiter (. inserted)");
        help!(
            b"I was expecting to see something like `(\' or `\\{\' or",
            b"`\\}\' here. If you typed, e.g., `{\' instead of `\\{\', you",
            b"should probably delete the `{\' by typing `1\' now, so that",
            b"braces don\'t get unbalanced. Otherwise just proceed.",
            b"Acceptable delimiters are characters whose \\delcode is",
            b"nonnegative, or you can use `\\delimiter <delimiter code>\'."
        );
        back_error();
        cur_val = 0i32
    }
    if cur_val >= 0x40000000i32 {
        MEM[p].b16.s3 =
            (cur_val % 0x200000 / 0x10000 * 0x100 + cur_val / 0x200000i32 % 0x100i32) as u16;
        MEM[p].b16.s2 = (cur_val % 0x10000) as u16;
        MEM[p].b16.s1 = 0_u16;
        MEM[p].b16.s0 = 0_u16
    } else {
        MEM[p].b16.s3 = (cur_val / 0x100000 % 16) as u16;
        MEM[p].b16.s2 = (cur_val / 0x1000 % 0x100) as u16;
        MEM[p].b16.s1 = (cur_val / 0x100 % 16) as u16;
        MEM[p].b16.s0 = (cur_val % 0x100) as u16
    };
}
pub(crate) unsafe fn math_radical() {
    let rn = get_node(RADICAL_NOAD_SIZE);
    *LLIST_link(cur_list.tail) = Some(rn).tex_int();
    cur_list.tail = rn;
    MEM[cur_list.tail].b16.s1 = MathNode::Radical as u16;
    MEM[cur_list.tail].b16.s0 = NORMAL as u16;
    MEM[cur_list.tail + 1].b32 = empty;
    MEM[cur_list.tail + 3].b32 = empty;
    MEM[cur_list.tail + 2].b32 = empty;
    scan_delimiter(cur_list.tail + 4, true);
    scan_math(cur_list.tail + 1);
}
pub(crate) unsafe fn math_ac() {
    if cur_cmd == Cmd::Accent {
        /*1201: */
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Please use ");
        print_esc_cstr(b"mathaccent");
        print_cstr(b" for accents in math mode");
        help!(
            b"I\'m changing \\accent to \\mathaccent here; wish me luck.",
            b"(Accents are not the same in formulas as they are in text.)"
        );
        error();
    }
    let an = get_node(ACCENT_NOAD_SIZE);
    *LLIST_link(cur_list.tail) = Some(an).tex_int();
    cur_list.tail = an;
    MEM[cur_list.tail].b16.s1 = MathNode::Accent as u16;
    MEM[cur_list.tail].b16.s0 = AccentType::Normal as _;
    MEM[cur_list.tail + 1].b32 = empty;
    MEM[cur_list.tail + 3].b32 = empty;
    MEM[cur_list.tail + 2].b32 = empty;
    MEM[cur_list.tail + 4].b32.s1 = MathCell::MathChar as _;
    if cur_chr == 1i32 {
        MEM[cur_list.tail].b16.s0 = if scan_keyword(b"fixed") {
            AccentType::Fixed as _
        } else if scan_keyword(b"bottom") {
            if scan_keyword(b"fixed") {
                AccentType::BottomFixed as _
            } else {
                AccentType::Bottom as _
            }
        } else {
            AccentType::Normal as _
        };
        scan_math_class_int();
        let mut c = set_class(cur_val);
        scan_math_fam_int();
        c += set_family(cur_val);
        scan_usv_num();
        cur_val = cur_val + c
    } else {
        scan_fifteen_bit_int();
        cur_val = set_class(cur_val / 4096) + set_family(cur_val % 4096 / 256) + (cur_val % 256);
    }
    MEM[cur_list.tail + 4].b16.s0 = (cur_val as i64 % 65536) as u16;
    if math_class(cur_val) == 7
        && (*INTPAR(IntPar::cur_fam) >= 0 && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32)
    {
        MEM[cur_list.tail + 4].b16.s1 = *INTPAR(IntPar::cur_fam) as u16
    } else {
        MEM[cur_list.tail + 4].b16.s1 = math_fam(cur_val) as u16
    }
    MEM[cur_list.tail + 4].b16.s1 =
        (MEM[cur_list.tail + 4].b16.s1 as i64 + math_char(cur_val) as i64 / 65536 * 256) as u16;
    scan_math((cur_list.tail + 1) as usize);
}
pub(crate) unsafe fn append_choices() {
    let c = new_choice();
    *LLIST_link(cur_list.tail) = Some(c).tex_int();
    cur_list.tail = c;
    SAVE_PTR += 1;
    SAVE_STACK[SAVE_PTR - 1].val = 0;
    push_math(GroupCode::MathChoice);
    scan_left_brace();
}
pub(crate) unsafe fn fin_mlist(p: Option<usize>) -> i32 {
    let q = if let Some(a) = cur_list.aux.b32.s1.opt() {
        /*1220: */
        MEM[a + 3].b32.s1 = MathCell::SubMList as _;
        MEM[a + 3].b32.s0 = *LLIST_link(cur_list.head);
        let q;
        if let Some(p) = p {
            q = MEM[a + 2].b32.s0 as usize;
            if MEM[q].b16.s1 != 30 || cur_list.eTeX_aux.is_none() {
                confusion(b"right");
            }
            if let Some(aux) = cur_list.eTeX_aux {
                MEM[a + 2].b32.s0 = *LLIST_link(aux);
                *LLIST_link(aux) = Some(a).tex_int();
                *LLIST_link(a) = Some(p).tex_int();
            }
        } else {
            q = a;
        }
        q as i32
    } else {
        *LLIST_link(cur_list.tail) = p.tex_int();
        *LLIST_link(cur_list.head)
    };
    pop_nest();
    q
}
pub(crate) unsafe fn build_choices() {
    unsave();
    let mut p = fin_mlist(None);
    match MathStyle::n(SAVE_STACK[SAVE_PTR - 1].val as i16).unwrap() {
        MathStyle::Display => *CHOICE_NODE_display(cur_list.tail) = p,
        MathStyle::Text => *CHOICE_NODE_text(cur_list.tail) = p,
        MathStyle::Script => *CHOICE_NODE_script(cur_list.tail) = p,
        MathStyle::ScriptScript => {
            *CHOICE_NODE_scriptscript(cur_list.tail) = p;
            SAVE_PTR -= 1;
            return;
        }
    }
    SAVE_STACK[SAVE_PTR - 1].val += 1;
    push_math(GroupCode::MathChoice);
    scan_left_brace();
}
pub(crate) unsafe fn sub_sup() {
    let mut t = MathCell::Empty;
    let mut p = None;
    if cur_list.tail != cur_list.head {
        if MEM[cur_list.tail].b16.s1 >= MathNode::Ord as u16
            && MEM[cur_list.tail].b16.s1 < MathNode::Left as u16
        {
            let g = cur_list.tail + 2 + cur_cmd as usize - 7;
            t = MathCell::n(MEM[g as usize].b32.s1).unwrap();
            p = Some(g);
        }
    }
    let p = match (p, t) {
        (Some(p), MathCell::Empty) => p,
        _ => {
            /*1212: */
            MEM[cur_list.tail].b32.s1 = new_noad() as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
            let p = cur_list.tail + 2 + cur_cmd as usize - 7;
            if t != MathCell::Empty {
                if cur_cmd == Cmd::SupMark {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Double superscript");
                    help!(b"I treat `x^1^2\' essentially like `x^1{}^2\'.");
                } else {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Double subscript");
                    help!(b"I treat `x_1_2\' essentially like `x_1{}_2\'.");
                }
                error();
            }
            p
        }
    };
    scan_math(p);
}
pub(crate) unsafe fn math_fraction() {
    let mut c: i16 = 0;
    c = cur_chr as i16;
    if cur_list.aux.b32.s1.opt().is_some() {
        /*1218:*/
        if c as i32 >= DELIMITED_CODE {
            scan_delimiter(GARBAGE, false);
            scan_delimiter(GARBAGE, false);
        }
        if c as i32 % DELIMITED_CODE == ABOVE_CODE {
            scan_dimen(false, false, false);
        }
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Ambiguous; you need another { and }");
        help!(
            b"I\'m ignoring this fraction specification, since I don\'t",
            b"know whether a construction like `x \\over y \\over z\'",
            b"means `{x \\over y} \\over z\' or `x \\over {y \\over z}\'."
        );
        error();
    } else {
        let a = get_node(FRACTION_NOAD_SIZE);
        cur_list.aux.b32.s1 = Some(a).tex_int();
        MEM[a].b16.s1 = MathNode::Fraction as u16;
        MEM[a].b16.s0 = NORMAL as u16;
        MEM[a + 2].b32.s1 = MathCell::SubMList as _;
        MEM[a + 2].b32.s0 = MEM[cur_list.head].b32.s1;
        MEM[a + 3].b32 = empty;
        MEM[a + 4].b16 = null_delimiter;
        MEM[a + 5].b16 = null_delimiter;
        MEM[cur_list.head].b32.s1 = None.tex_int();
        cur_list.tail = cur_list.head;
        if c as i32 >= DELIMITED_CODE {
            scan_delimiter(a + 4, false);
            scan_delimiter(a + 5, false);
        }
        match c as i32 % DELIMITED_CODE {
            ABOVE_CODE => {
                scan_dimen(false, false, false);
                MEM[a + 1].b32.s1 = cur_val
            }
            OVER_CODE => MEM[a + 1].b32.s1 = DEFAULT_CODE,
            ATOP_CODE => MEM[a + 1].b32.s1 = 0,
            _ => {}
        }
    };
}
pub(crate) unsafe fn math_left_right() {
    let mut q: i32 = 0;
    let mut t = cur_chr as i16;
    if t != MathNode::Left as i16 && cur_group != GroupCode::MathLeft {
        /*1227: */
        if cur_group == GroupCode::MathShift {
            scan_delimiter(GARBAGE, false); /*:1530 */
            if file_line_error_style_p != 0 {
                print_file_line(); /*:1530 */
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Extra ");
            if t as u16 == 1 {
                print_esc_cstr(b"middle");
                help!(b"I\'m ignoring a \\middle that had no matching \\left.");
            } else {
                print_esc_cstr(b"right");
                help!(b"I\'m ignoring a \\right that had no matching \\left.");
            }
            error();
        } else {
            off_save();
        }
    } else {
        let p = new_noad() as usize;
        MEM[p].b16.s1 = t as u16;
        scan_delimiter(p + 1, false);
        if t as i32 == 1i32 {
            MEM[p].b16.s1 = MathNode::Right as u16;
            MEM[p].b16.s0 = 1;
        }
        if t == MathNode::Left as i16 {
            q = p as i32;
        } else {
            q = fin_mlist(Some(p));
            unsave();
        }
        if t != MathNode::Right as i16 {
            push_math(GroupCode::MathLeft);
            MEM[cur_list.head].b32.s1 = q;
            cur_list.tail = p;
            cur_list.eTeX_aux = Some(p);
        } else {
            MEM[cur_list.tail].b32.s1 = new_noad() as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
            MEM[cur_list.tail].b16.s1 = MathNode::Inner as u16;
            MEM[cur_list.tail + 1].b32.s1 = MathCell::SubMList as _;
            MEM[cur_list.tail + 1].b32.s0 = q
        }
    };
}
unsafe fn app_display(j: Option<usize>, mut b: usize, mut d: scaled_t) {
    let mut z: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut e: scaled_t = 0;
    let mut x: i32 = 0;
    s = *DIMENPAR(DimenPar::display_indent);
    x = *INTPAR(IntPar::pre_display_correction);
    if x == 0 {
        *BOX_shift_amount(b) = s + d
    } else {
        let mut q;
        z = *DIMENPAR(DimenPar::display_width);
        let mut p = b;
        if x > 0 {
            e = z - d - MEM[p + 1].b32.s1
        } else {
            e = d;
            d = z - e - MEM[p + 1].b32.s1
        }
        if let Some(j) = j {
            b = copy_node_list(Some(j)) as usize;
            *BOX_height(b) = *BOX_height(p);
            *BOX_depth(b) = *BOX_depth(p);
            s = s - *BOX_shift_amount(b);
            d = d + s;
            e = e + *BOX_width(b) - z - s
        }
        if MEM[p].b16.s0 == LRMode::DList as u16 {
            q = p;
        } else {
            let r = BOX_list_ptr(p).opt();
            free_node(p, BOX_NODE_SIZE);
            let mut r = r.confuse(b"LR4");
            if x > 0 {
                p = r;
                loop {
                    q = r;
                    if let Some(t) = LLIST_link(r as usize).opt() {
                        r = t;
                    } else {
                        break;
                    }
                }
            } else {
                let mut popt = None;
                q = r;
                loop {
                    let t = LLIST_link(r as usize).opt();
                    *LLIST_link(r) = popt.tex_int();
                    p = r;
                    popt = Some(p);
                    if let Some(t) = t {
                        r = t;
                    } else {
                        break;
                    }
                }
            }
        }
        let r;
        let t;
        if j.is_none() {
            r = new_kern(0);
            t = new_kern(0)
        } else {
            r = *BOX_list_ptr(b) as usize;
            t = *LLIST_link(r) as usize;
        }
        let u = new_math(0, END_M_CODE);
        let j = if NODE_type(t) == TextNode::Glue.into() {
            let (j, tmp_ptr) = new_skip_param(GluePar::right_skip);
            *LLIST_link(q as usize) = Some(j).tex_int();
            *LLIST_link(j) = Some(u).tex_int();
            let j = *GLUE_NODE_glue_ptr(t) as usize;
            *GLUE_SPEC_stretch_order(tmp_ptr) = *GLUE_SPEC_stretch_order(j);
            *GLUE_SPEC_shrink_order(tmp_ptr) = *GLUE_SPEC_shrink_order(j);
            *GLUE_SPEC_size(tmp_ptr) = e - *GLUE_SPEC_size(j);
            *GLUE_SPEC_stretch(tmp_ptr) = -(*GLUE_SPEC_stretch(j));
            *GLUE_SPEC_shrink(tmp_ptr) = -(*GLUE_SPEC_shrink(j));
            *LLIST_link(u) = Some(t).tex_int();
            Some(j)
        } else {
            MEM[t + 1].b32.s1 = e;
            *LLIST_link(t) = Some(u).tex_int();
            *LLIST_link(q as usize) = Some(t).tex_int();
            j
        };
        let u = new_math(0, BEGIN_M_CODE);
        if NODE_type(r) == TextNode::Glue.into() {
            let (j, tmp_ptr) = new_skip_param(GluePar::left_skip);
            *LLIST_link(u) = Some(j).tex_int();
            *LLIST_link(j) = Some(p).tex_int();
            let j = *GLUE_NODE_glue_ptr(r) as usize;
            *GLUE_SPEC_stretch_order(tmp_ptr) = *GLUE_SPEC_stretch_order(j);
            *GLUE_SPEC_shrink_order(tmp_ptr) = *GLUE_SPEC_shrink_order(j);
            *GLUE_SPEC_size(tmp_ptr) = d - *GLUE_SPEC_size(j);
            *GLUE_SPEC_stretch(tmp_ptr) = -(*GLUE_SPEC_stretch(j));
            *GLUE_SPEC_shrink(tmp_ptr) = -(*GLUE_SPEC_shrink(j));
            *LLIST_link(r) = Some(u).tex_int();
        } else {
            MEM[r + 1].b32.s1 = d;
            *LLIST_link(r) = Some(p).tex_int();
            *LLIST_link(u) = Some(r).tex_int();
            if j.is_none() {
                b = hpack(Some(u), 0, PackMode::Additional);
                *BOX_shift_amount(b) = s;
            } else {
                *BOX_list_ptr(b) = Some(u).tex_int();
            }
        }
    }
    append_to_vlist(b);
}
pub(crate) unsafe fn after_math() {
    let mut l: bool = false;
    let mut danger: bool = false;
    let mut p: i32 = 0;
    let mut w: scaled_t = 0;
    let mut z: scaled_t = 0;
    let mut e: scaled_t = 0;
    let mut q: scaled_t = 0;
    let mut d: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut j = None;

    danger = false;

    if cur_list.mode == (false, ListMode::MMode) {
        j = cur_list.eTeX_aux; // :1530
    }
    if FONT_PARAMS[MATH_FONT(2)] < TOTAL_MATHSY_PARAMS
        && !(FONT_AREA[MATH_FONT(2)] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[MATH_FONT(2)] as XeTeXLayoutEngine) as i32
                != 0)
        || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
            && !(FONT_AREA[MATH_FONT(2 + SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SIZE)] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
        || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
            && !(FONT_AREA[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Math formula deleted: Insufficient symbol fonts");

        help!(
            b"Sorry, but I can\'t typeset math unless \\textfont 2",
            b"and \\scriptfont 2 and \\scriptscriptfont 2 have all",
            b"the \\fontdimen values needed in math symbol fonts."
        );
        error();
        flush_math();
        danger = true
    } else if FONT_PARAMS[MATH_FONT(3 + TEXT_SIZE)] < TOTAL_MATHEX_PARAMS
        && !(FONT_AREA[MATH_FONT(3 + TEXT_SIZE)] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[MATH_FONT(3 + TEXT_SIZE)] as XeTeXLayoutEngine)
                as i32
                != 0)
        || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !(FONT_AREA[MATH_FONT(3 + SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SIZE)] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
        || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !(FONT_AREA[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Math formula deleted: Insufficient extension fonts");

        help!(
            b"Sorry, but I can\'t typeset math unless \\textfont 3",
            b"and \\scriptfont 3 and \\scriptscriptfont 3 have all",
            b"the \\fontdimen values needed in math extension fonts."
        );
        error();
        flush_math();
        danger = true
    }
    let mut m = cur_list.mode;
    l = false;
    p = fin_mlist(None);
    let a = if cur_list.mode == (!m.0, m.1) {
        get_x_token();
        if cur_cmd != Cmd::MathShift {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Display math should end with $$");

            help!(
                b"The `$\' that I just saw supposedly matches a previous `$$\'.",
                b"So I shall assume that you typed `$$\' both times."
            );
            back_error();
        }
        cur_mlist = p;
        cur_style = (MathStyle::Text, 0);
        mlist_penalties = false;
        mlist_to_hlist();
        let a = hpack(LLIST_link(TEMP_HEAD).opt(), 0, PackMode::Additional);
        MEM[a].b16.s0 = LRMode::DList as u16;
        unsave();
        SAVE_PTR -= 1;
        if SAVE_STACK[SAVE_PTR + 0].val == 1 {
            l = true
        }
        danger = false;
        if cur_list.mode == (false, ListMode::MMode) {
            j = cur_list.eTeX_aux
        }
        if FONT_PARAMS[MATH_FONT(2)] < TOTAL_MATHSY_PARAMS
            && !(FONT_AREA[MATH_FONT(2)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[MATH_FONT(2)] as XeTeXLayoutEngine) as i32
                    != 0)
            || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
                && !(FONT_AREA[MATH_FONT(2 + SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SIZE)] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
            || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
                && !(FONT_AREA[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
        {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Math formula deleted: Insufficient symbol fonts");

            help!(
                b"Sorry, but I can\'t typeset math unless \\textfont 2",
                b"and \\scriptfont 2 and \\scriptscriptfont 2 have all",
                b"the \\fontdimen values needed in math symbol fonts."
            );
            error();
            flush_math();
            danger = true
        } else if FONT_PARAMS[MATH_FONT(3 + TEXT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !(FONT_AREA[MATH_FONT(3 + TEXT_SIZE)] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[MATH_FONT(3 + TEXT_SIZE)] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
            || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
                && !(FONT_AREA[MATH_FONT(3 + SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SIZE)] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
            || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
                && !(FONT_AREA[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
        {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Math formula deleted: Insufficient extension fonts");

            help!(
                b"Sorry, but I can\'t typeset math unless \\textfont 3",
                b"and \\scriptfont 3 and \\scriptscriptfont 3 have all",
                b"the \\fontdimen values needed in math extension fonts."
            );
            error();
            flush_math();
            danger = true
        }
        m = cur_list.mode;
        p = fin_mlist(None);
        Some(a)
    } else {
        None
    };
    if m.0 == true {
        // 1231:
        MEM[cur_list.tail].b32.s1 =
            new_math(*DIMENPAR(DimenPar::math_surround), MathNST::Before) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        cur_mlist = p;
        cur_style = (MathStyle::Text, 0);
        mlist_penalties = cur_list.mode.0 == false;
        mlist_to_hlist();
        MEM[cur_list.tail].b32.s1 = *LLIST_link(TEMP_HEAD);
        while let Some(next) = LLIST_link(cur_list.tail).opt() {
            cur_list.tail = next;
        }
        MEM[cur_list.tail].b32.s1 =
            new_math(*DIMENPAR(DimenPar::math_surround), MathNST::After) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        cur_list.aux.b32.s0 = 1000;
        unsave();
    } else {
        if a.is_none() {
            // 1232:
            get_x_token();
            if cur_cmd != Cmd::MathShift {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Display math should end with $$");

                help!(
                    b"The `$\' that I just saw supposedly matches a previous `$$\'.",
                    b"So I shall assume that you typed `$$\' both times."
                );
                back_error();
            }
        }
        cur_mlist = p;
        cur_style = (MathStyle::Display, 0);
        mlist_penalties = false;
        mlist_to_hlist();
        let p = LLIST_link(TEMP_HEAD).opt();
        adjust_tail = Some(ADJUST_HEAD);
        pre_adjust_tail = Some(PRE_ADJUST_HEAD);
        let mut b = hpack(p, 0, PackMode::Additional);
        let p = BOX_list_ptr(b).opt();
        let t = adjust_tail.unwrap();
        adjust_tail = None;
        let pre_t = pre_adjust_tail.unwrap();
        pre_adjust_tail = None;
        w = *BOX_width(b);
        z = *DIMENPAR(DimenPar::display_width);
        s = *DIMENPAR(DimenPar::display_indent);
        if *INTPAR(IntPar::pre_display_correction) < 0i32 {
            s = -s - z
        }
        if danger {
            e = 0;
            q = 0;
        } else if let Some(a) = a {
            e = MEM[a + 1].b32.s1;
            q = e + math_quad(TEXT_SIZE);
        } else {
            e = 0;
            q = 0;
        }
        if w + q > z {
            // 1236:
            if e != 0
                && (w - total_shrink[NORMAL as usize] + q <= z
                    || total_shrink[FIL as usize] != 0
                    || total_shrink[FILL as usize] != 0
                    || total_shrink[FILLL as usize] != 0)
            {
                free_node(b, BOX_NODE_SIZE);
                b = hpack(p, z - q, PackMode::Exactly);
            } else {
                e = 0;
                if w > z {
                    free_node(b, BOX_NODE_SIZE);
                    b = hpack(p, z, PackMode::Exactly);
                }
            }
            w = *BOX_width(b);
        }
        set_BOX_lr_mode(b, LRMode::DList);
        d = half(z - w);
        if e > 0 && d < 2 * e {
            d = half(z - w - e);
            if let Some(p) = p {
                if !is_char_node(Some(p)) {
                    if NODE_type(p) == TextNode::Glue.into() {
                        d = 0
                    }
                }
            }
        }
        let mut g1;
        let mut g2;
        MEM[cur_list.tail].b32.s1 = new_penalty(*INTPAR(IntPar::pre_display_penalty)) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        if d + s <= *DIMENPAR(DimenPar::pre_display_size) || l {
            g1 = GluePar::above_display_skip;
            g2 = Some(GluePar::below_display_skip);
        } else {
            g1 = GluePar::above_display_short_skip;
            g2 = Some(GluePar::below_display_short_skip);
        }
        if l && e == 0 {
            app_display(j, a.unwrap(), 0);
            let pen = new_penalty(INF_PENALTY);
            *LLIST_link(cur_list.tail) = Some(pen).tex_int();
            cur_list.tail = pen;
        } else {
            let pg = new_param_glue(g1);
            *LLIST_link(cur_list.tail) = Some(pg).tex_int();
            cur_list.tail = pg;
        }
        if e != 0i32 {
            let a = a.unwrap();
            let r = new_kern(z - w - e - d);
            if l {
                *LLIST_link(a) = Some(r).tex_int();
                *LLIST_link(r) = Some(b).tex_int();
                b = a;
                d = 0;
            } else {
                *LLIST_link(b) = Some(r).tex_int();
                *LLIST_link(r) = Some(a).tex_int();
            }
            b = hpack(Some(b), 0, PackMode::Additional);
        }
        app_display(j, b, d);
        if let Some(a) = a {
            if e == 0 && !l {
                MEM[cur_list.tail].b32.s1 = new_penalty(INF_PENALTY) as i32;
                cur_list.tail = *LLIST_link(cur_list.tail) as usize;
                app_display(j, a, z - MEM[a + 1].b32.s1);
                g2 = None;
            }
        }
        if t != ADJUST_HEAD {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(ADJUST_HEAD as usize);
            cur_list.tail = t;
        }
        if pre_t != PRE_ADJUST_HEAD {
            MEM[cur_list.tail].b32.s1 = *LLIST_link(PRE_ADJUST_HEAD as usize);
            cur_list.tail = pre_t;
        }
        MEM[cur_list.tail].b32.s1 = new_penalty(*INTPAR(IntPar::post_display_penalty)) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        if let Some(g2) = g2 {
            MEM[cur_list.tail].b32.s1 = new_param_glue(g2) as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        }
        flush_node_list(j);
        resume_after_display();
    };
}
pub(crate) unsafe fn resume_after_display() {
    if cur_group != GroupCode::MathShift {
        confusion(b"display");
    }
    unsave();
    cur_list.prev_graf = cur_list.prev_graf + 3;
    push_nest();
    cur_list.mode = (false, ListMode::HMode);
    cur_list.aux.b32.s0 = 1000;
    if *INTPAR(IntPar::language) <= 0 {
        cur_lang = 0;
    } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
        cur_lang = 0;
    } else {
        cur_lang = *INTPAR(IntPar::language) as u8;
    }
    cur_list.aux.b32.s1 = cur_lang as i32;
    cur_list.prev_graf = ((norm_min(*INTPAR(IntPar::left_hyphen_min)) as i32 * 64
        + norm_min(*INTPAR(IntPar::right_hyphen_min)) as i32) as i64
        * 65536
        + cur_lang as i64) as i32;
    get_x_token();
    if cur_cmd != Cmd::Spacer {
        back_input();
    }
    if NEST_PTR == 1 {
        build_page();
    };
}
/* Copyright 2016-2018 The Tectonic Project
 * Licensed under the MIT License.
 */
unsafe fn math_x_height(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 5)
    } else {
        FONT_INFO[(5 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn math_quad(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 6i32)
    } else {
        FONT_INFO[(6 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn num1(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 8)
    } else {
        FONT_INFO[(8 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn num2(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 9i32)
    } else {
        FONT_INFO[(9 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn num3(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 10)
    } else {
        FONT_INFO[(10 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn denom1(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 11)
    } else {
        FONT_INFO[(11 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn denom2(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 12)
    } else {
        FONT_INFO[(12 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sup1(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 13)
    } else {
        FONT_INFO[(13 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sup2(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 14)
    } else {
        FONT_INFO[(14 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sup3(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 15)
    } else {
        FONT_INFO[(15 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sub1(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 16)
    } else {
        FONT_INFO[(16 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sub2(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 17)
    } else {
        FONT_INFO[(17 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sup_drop(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 18)
    } else {
        FONT_INFO[(18 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn sub_drop(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code) as usize;
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 19)
    } else {
        FONT_INFO[(19 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn delim1(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 20)
    } else {
        FONT_INFO[(20 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn delim2(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 21)
    } else {
        FONT_INFO[(21 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn axis_height(size_code: usize) -> scaled_t {
    let f = MATH_FONT(2 + size_code);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathsy_param(f, 22)
    } else {
        FONT_INFO[(22 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn default_rule_thickness() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 8)
    } else {
        FONT_INFO[(8 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn big_op_spacing1() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 9)
    } else {
        FONT_INFO[(9 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn big_op_spacing2() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 10)
    } else {
        FONT_INFO[(10 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn big_op_spacing3() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 11)
    } else {
        FONT_INFO[(11 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn big_op_spacing4() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 12)
    } else {
        FONT_INFO[(12 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn big_op_spacing5() -> scaled_t {
    let f = MATH_FONT(3 + cur_size);
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_native_mathex_param(f, 13)
    } else {
        FONT_INFO[(13 + PARAM_BASE[f]) as usize].b32.s1
    }
}
unsafe fn fraction_rule(mut t: scaled_t) -> usize {
    let p = new_rule();
    MEM[p + 3].b32.s1 = t;
    MEM[p + 2].b32.s1 = 0;
    p
}
unsafe fn overbar(b: i32, k: scaled_t, mut t: scaled_t) -> usize {
    let p = new_kern(k);
    *LLIST_link(p) = b;
    let q = fraction_rule(t);
    *LLIST_link(q) = Some(p).tex_int();
    let p = new_kern(t);
    *LLIST_link(p) = Some(q).tex_int();
    vpackage(Some(p), 0, PackMode::Additional, MAX_HALFWORD)
}
unsafe fn math_glue(g: usize, mut m: scaled_t) -> usize {
    let mut n = x_over_n(m, 65536);
    let mut f = tex_remainder;
    if f < 0 {
        n -= 1;
        f = (f as i64 + 65536) as scaled_t
    }
    let p = get_node(GLUE_SPEC_SIZE);
    MEM[p + 1].b32.s1 = mult_and_add(
        n,
        MEM[g + 1].b32.s1,
        xn_over_d(MEM[g + 1].b32.s1, f, 65536),
        MAX_HALFWORD,
    );
    MEM[p].b16.s1 = MEM[g].b16.s1;
    if MEM[p].b16.s1 == NORMAL {
        MEM[p + 2].b32.s1 = mult_and_add(
            n,
            MEM[g + 2].b32.s1,
            xn_over_d(MEM[g + 2].b32.s1, f, 65536),
            MAX_HALFWORD,
        )
    } else {
        MEM[p + 2].b32.s1 = MEM[g + 2].b32.s1
    }
    MEM[p].b16.s0 = MEM[g].b16.s0;
    if *GLUE_SPEC_shrink_order(p) == NORMAL {
        MEM[p + 3].b32.s1 = mult_and_add(
            n,
            MEM[g + 3].b32.s1,
            xn_over_d(MEM[g + 3].b32.s1, f, 65536),
            MAX_HALFWORD,
        )
    } else {
        MEM[p + 3].b32.s1 = MEM[g + 3].b32.s1
    }
    p
}
unsafe fn math_kern(p: usize, mut m: scaled_t) {
    let mut n: i32 = 0;
    let mut f: scaled_t = 0;
    if MEM[p].b16.s0 == MU_GLUE {
        n = x_over_n(m, 65536);
        f = tex_remainder;
        if f < 0 {
            n -= 1;
            f = (f as i64 + 65536) as scaled_t
        }
        MEM[p + 1].b32.s1 = mult_and_add(
            n,
            MEM[p + 1].b32.s1,
            xn_over_d(MEM[p + 1].b32.s1, f, 65536),
            MAX_HALFWORD,
        );
        set_kern_NODE_subtype(p, KernNST::Explicit);
    };
}
pub(crate) unsafe fn flush_math() {
    flush_node_list(MEM[cur_list.head].b32.s1.opt());
    flush_node_list(cur_list.aux.b32.s1.opt());
    *LLIST_link(cur_list.head) = None.tex_int();
    cur_list.tail = cur_list.head;
    cur_list.aux.b32.s1 = None.tex_int();
}
unsafe fn clean_box(p: usize, s: (MathStyle, u8)) -> usize {
    match MEM[p].b32.s1 {
        1 => {
            cur_mlist = new_noad() as i32;
            MEM[(cur_mlist + 1) as usize] = MEM[p];
        }
        2 => {
            return found(MEM[p].b32.s0);
        }
        3 => {
            cur_mlist = MEM[p].b32.s0;
        }
        _ => {
            return found(new_null_box() as i32);
        }
    }

    let save_style = cur_style;
    cur_style = s;
    mlist_penalties = false;
    mlist_to_hlist();
    let q = *LLIST_link(TEMP_HEAD);
    cur_style = save_style;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18);

    unsafe fn found(q: i32) -> usize {
        let x = if is_char_node(q.opt()) || q.opt().is_none() {
            hpack(q.opt(), 0, PackMode::Additional)
        } else if LLIST_link(q as usize).opt().is_none()
            && [TextNode::HList.into(), TextNode::VList.into()].contains(&NODE_type(q as usize))
            && *BOX_shift_amount(q as usize) == 0
        {
            q as usize
        } else {
            hpack(q.opt(), 0, PackMode::Additional)
        };
        let q = *BOX_list_ptr(x);
        if is_char_node(q.opt()) {
            if let Some(r) = LLIST_link(q as usize).opt() {
                if LLIST_link(r).opt().is_none() {
                    if !is_char_node(Some(r)) {
                        if NODE_type(r) == TextNode::Kern.into() {
                            free_node(r, MEDIUM_NODE_SIZE);
                            *LLIST_link(q as usize) = None.tex_int()
                        }
                    }
                }
            }
        }
        x
    }
    found(q)
}
unsafe fn fetch(a: usize) {
    cur_c = MEM[a].b16.s0 as i32;
    cur_f = MATH_FONT(MEM[a].b16.s1 as usize % 256 + cur_size);
    cur_c = (cur_c as i64 + (MEM[a].b16.s1 as i32 / 256) as i64 * 65536) as i32;
    if cur_f == FONT_BASE {
        // 749:
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"");
        print_size(cur_size as i32);
        print_char(' ' as i32);
        print_int(MEM[a].b16.s1 as i32 % 256);
        print_cstr(b" is undefined (character ");
        print(cur_c);
        print_char(')' as i32);

        help!(
            b"Somewhere in the math formula just ended, you used the",
            b"stated character from an undefined font family. For example,",
            b"plain TeX doesn\'t allow \\it or \\sl in subscripts. Proceed,",
            b"and I\'ll try to forget that I needed that character."
        );
        error();
        cur_i = NULL_CHARACTER;
        MEM[a].b32.s1 = 0
    } else if FONT_AREA[cur_f as usize] as u32 == AAT_FONT_FLAG
        || FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
    {
        cur_i = NULL_CHARACTER
    } else {
        if cur_c >= FONT_BC[cur_f as usize] as i32 && cur_c <= FONT_EC[cur_f as usize] as i32 {
            cur_i = FONT_INFO[(CHAR_BASE[cur_f as usize] + cur_c) as usize].b16
        } else {
            cur_i = NULL_CHARACTER
        }
        if !(cur_i.s3 as i32 > 0) {
            char_warning(cur_f, cur_c);
            MEM[a].b32.s1 = MathCell::Empty as _;
        }
    };
}
unsafe fn make_over(q: usize) {
    MEM[q + 1].b32.s0 = overbar(
        clean_box(q + 1, (cur_style.0, 1)) as i32,
        3 * default_rule_thickness(),
        default_rule_thickness(),
    ) as i32;
    MEM[(q + 1) as usize].b32.s1 = MathCell::SubBox as _;
}
unsafe fn make_under(q: usize) {
    let x = clean_box(q + 1, cur_style);
    let p = new_kern(3 * default_rule_thickness());
    *LLIST_link(x) = Some(p).tex_int();
    *LLIST_link(p) = Some(fraction_rule(default_rule_thickness())).tex_int();
    let y = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD);
    let delta = *BOX_height(y) + *BOX_depth(y) + default_rule_thickness();
    *BOX_height(y) = *BOX_height(x);
    *BOX_depth(y) = delta - *BOX_height(y);
    MEM[q + 1].b32.s0 = y as i32;
    MEM[q + 1].b32.s1 = MathCell::SubBox as _;
}
unsafe fn make_vcenter(q: usize) {
    let v = MEM[q + 1].b32.s0 as usize;
    if NODE_type(v) != TextNode::VList.into() {
        confusion(b"vcenter");
    }
    let delta = *BOX_height(v) + *BOX_depth(v);
    *BOX_height(v) = axis_height(cur_size) + half(delta);
    *BOX_depth(v) = delta - *BOX_height(v);
}
unsafe fn make_radical(q: usize) {
    let f = MATH_FONT(MEM[q + 4].b16.s3 as usize % 256 + cur_size);
    let rule_thickness = if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        get_ot_math_constant(f, RADICALRULETHICKNESS)
    } else {
        default_rule_thickness()
    };
    let x = clean_box(q + 1, (cur_style.0, 1));
    let mut clr = if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        if cur_style.0 == MathStyle::Display {
            get_ot_math_constant(f, RADICALDISPLAYSTYLEVERTICALGAP)
        } else {
            get_ot_math_constant(f, RADICALVERTICALGAP)
        }
    } else if cur_style.0 == MathStyle::Display {
        rule_thickness + math_x_height(cur_size).abs() / 4
    } else {
        let clr = rule_thickness;
        clr + clr.abs() / 4
    };
    let y = var_delimiter(
        q + 4,
        cur_size,
        *BOX_height(x) + *BOX_depth(x) + clr + rule_thickness,
    );
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        *BOX_depth(y) = *BOX_height(y) + *BOX_depth(y) - rule_thickness;
        *BOX_height(y) = rule_thickness
    }
    let delta = *BOX_depth(y) - (*BOX_height(x) + *BOX_depth(x) + clr);
    if delta > 0 {
        clr += half(delta);
    }
    *BOX_shift_amount(y) = -((*BOX_height(x) + clr) as i32);
    *LLIST_link(y) = overbar(x as i32, clr, *BOX_height(y)) as i32;
    MEM[q + 1].b32.s0 = hpack(Some(y), 0, PackMode::Additional) as i32;
    MEM[q + 1].b32.s1 = MathCell::SubBox as _;
}
unsafe fn compute_ot_math_accent_pos(p: usize) -> scaled_t {
    if MEM[p + 1].b32.s1 == MathCell::MathChar as _ {
        fetch(p + 1);
        let q = new_native_character(cur_f, cur_c);
        let g = real_get_native_glyph(
            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
            0_u32,
        ) as scaled_t;
        get_ot_math_accent_pos(cur_f, g)
    } else if MEM[p + 1].b32.s1 == MathCell::SubMList as _ {
        match MEM[p + 1].b32.s0.opt() {
            Some(r) if MEM[r].b16.s1 == MathNode::Accent as u16 => compute_ot_math_accent_pos(r),
            _ => TEX_INFINITY,
        }
    } else {
        TEX_INFINITY
    }
}
unsafe fn make_math_accent(q: usize) {
    let mut a: i32 = 0;
    let mut c: i32 = 0;
    let mut g: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut s: scaled_t = 0;
    let mut h: scaled_t = 0;
    let mut w: scaled_t = 0;
    let mut w2: scaled_t = 0;
    let mut ot_assembly_ptr: *mut libc::c_void = 0 as *mut libc::c_void;
    fetch(q + 4);
    ot_assembly_ptr = 0 as *mut libc::c_void;
    let x = if FONT_AREA[cur_f as usize] as u32 == AAT_FONT_FLAG
        || FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
    {
        c = cur_c;
        f = cur_f;
        s = if !(MEM[q].b16.s0 == AccentType::Bottom as _
            || MEM[q as usize].b16.s0 == AccentType::BottomFixed as _)
        {
            compute_ot_math_accent_pos(q)
        } else {
            0
        };
        let x = clean_box(q + 1, (cur_style.0, 1));
        w = *BOX_width(x);
        h = *BOX_height(x);
        Some(x)
    } else if cur_i.s3 as i32 > 0 {
        let mut i = cur_i;
        c = cur_c;
        f = cur_f;
        s = 0;
        if MEM[q + 1].b32.s1 == MathCell::MathChar as _ {
            fetch(q + 1);
            if cur_i.s1 as i32 % 4 == LIG_TAG {
                a = LIG_KERN_BASE[cur_f as usize] + cur_i.s0 as i32;
                cur_i = FONT_INFO[a as usize].b16;
                if cur_i.s3 as i32 > 128 {
                    a = ((LIG_KERN_BASE[cur_f as usize] + 256 * cur_i.s1 as i32 + cur_i.s0 as i32)
                        as i64
                        + 32768
                        - (256 * 128) as i64) as i32;
                    cur_i = FONT_INFO[a as usize].b16
                }
                loop {
                    if cur_i.s2 as i32 == SKEW_CHAR[cur_f as usize] {
                        if cur_i.s1 as i32 >= 128 {
                            if cur_i.s3 as i32 <= 128 {
                                s = FONT_INFO[(KERN_BASE[cur_f as usize]
                                    + 256 * cur_i.s1 as i32
                                    + cur_i.s0 as i32)
                                    as usize]
                                    .b32
                                    .s1
                            }
                        }
                        break;
                    } else {
                        if cur_i.s3 as i32 >= 128 {
                            break;
                        }
                        a += cur_i.s3 as i32 + 1;
                        cur_i = FONT_INFO[a as usize].b16
                    }
                }
            }
        }
        let x = clean_box(q + 1, (cur_style.0, 1));
        w = *BOX_width(x);
        h = *BOX_height(x);
        while !(i.s1 as i32 % 4 != LIST_TAG) {
            let y = i.s0 as i32;
            i = FONT_INFO[(CHAR_BASE[f] + y) as usize].b16;
            if !(i.s3 as i32 > 0) {
                break;
            }
            if FONT_INFO[(WIDTH_BASE[f] + i.s3 as i32) as usize].b32.s1 > w {
                break;
            }
            c = y
        }
        Some(x)
    } else {
        None
    };
    // :767
    if let Some(mut x) = x {
        let mut delta = if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
        {
            if MEM[q].b16.s0 == AccentType::Bottom as _
                || MEM[q].b16.s0 == AccentType::BottomFixed as _
            {
                0
            } else if h < get_ot_math_constant(f, ACCENTBASEHEIGHT) {
                h
            } else {
                get_ot_math_constant(f, ACCENTBASEHEIGHT)
            }
        } else if h < FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1 {
            h
        } else {
            FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1
        };
        if MEM[q + 2].b32.s1 != MathCell::Empty as _
            || MEM[(q + 3) as usize].b32.s1 != MathCell::Empty as _
        {
            if MEM[q + 1].b32.s1 == MathCell::MathChar as _ {
                // 769:
                flush_node_list(Some(x));
                let xn = new_noad();
                MEM[xn + 1] = MEM[q + 1];
                MEM[xn + 2] = MEM[q + 2];
                MEM[xn + 3] = MEM[q + 3];
                MEM[q + 2].b32 = empty;
                MEM[q + 3].b32 = empty;
                MEM[q + 1].b32.s1 = MathCell::SubMList as _;
                MEM[q + 1].b32.s0 = Some(xn).tex_int();
                x = clean_box(q + 1, cur_style);
                delta += *BOX_height(x) - h;
                h = *BOX_height(x);
            }
        }
        let mut y = char_box(f, c);
        if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
            let mut p = get_node(GLYPH_NODE_SIZE);
            set_NODE_type(p, TextNode::WhatsIt);
            set_whatsit_NODE_subtype(p, WhatsItNST::Glyph);
            *NATIVE_NODE_font(p) = f as u16;
            *NATIVE_NODE_glyph(p) = real_get_native_glyph(
                &mut MEM[*BOX_list_ptr(y) as usize] as *mut memory_word as *mut libc::c_void,
                0,
            );
            measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1i32);
            free_node(
                *BOX_list_ptr(y) as usize,
                *NATIVE_NODE_size(*BOX_list_ptr(y) as usize) as i32,
            );
            *BOX_list_ptr(y) = Some(p).tex_int();
            if MEM[q].b16.s0 as i32 & 1 != 0 {
                measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1);
            } else {
                c = *NATIVE_NODE_glyph(p) as i32;
                a = 0;
                loop {
                    g = get_ot_math_variant(f, c, a, &mut w2, 1);
                    if w2 > 0 && w2 <= w {
                        *NATIVE_NODE_glyph(p) = g as u16;
                        measure_native_glyph(
                            &mut MEM[p] as *mut memory_word as *mut libc::c_void,
                            1,
                        );
                        a += 1
                    }
                    if w2 < 0 || w2 >= w {
                        break;
                    }
                }
                if w2 < 0 {
                    ot_assembly_ptr = get_ot_assembly_ptr(f, c, 1);
                    if !ot_assembly_ptr.is_null() {
                        free_node(p, GLYPH_NODE_SIZE);
                        p = build_opentype_assembly(f, ot_assembly_ptr, w, true);
                        *BOX_list_ptr(y) = Some(p).tex_int();
                    }
                } else {
                    measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1);
                }
            }
            *BOX_width(y) = *BOX_width(p);
            *BOX_height(y) = *BOX_height(p);
            *BOX_depth(y) = *BOX_depth(p);
            if MEM[q].b16.s0 == AccentType::Bottom as _
                || MEM[q].b16.s0 == AccentType::BottomFixed as _
            {
                if *BOX_height(y) < 0 {
                    *BOX_height(y) = 0;
                }
            } else if *BOX_depth(y) < 0 {
                *BOX_depth(y) = 0;
            }
            let mut sa;
            if !is_char_node(Some(p))
                && NODE_type(p) == TextNode::WhatsIt.into()
                && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
            {
                sa = get_ot_math_accent_pos(f, *NATIVE_NODE_glyph(p) as i32);
                if sa == TEX_INFINITY {
                    sa = half(*BOX_width(y))
                }
            } else {
                sa = half(*BOX_width(y))
            }
            if MEM[q].b16.s0 == AccentType::Bottom as _
                || MEM[q].b16.s0 == AccentType::BottomFixed as _
                || s == TEX_INFINITY
            {
                s = half(w)
            }
            *BOX_shift_amount(y) = s - sa;
        } else {
            *BOX_shift_amount(y) = s + half(w - *BOX_width(y));
        }
        *BOX_width(y) = 0;
        if MEM[q].b16.s0 == AccentType::Bottom as _ || MEM[q].b16.s0 == AccentType::BottomFixed as _
        {
            *LLIST_link(x) = y as i32;
            y = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD);
            *BOX_shift_amount(y) = -((h - *BOX_height(y)) as i32)
        } else {
            let p = new_kern(-(delta as i32));
            *LLIST_link(p) = Some(x).tex_int();
            *LLIST_link(y) = p as i32;
            y = vpackage(Some(y), 0, PackMode::Additional, MAX_HALFWORD);
            if *BOX_height(y) < h {
                // 765:
                let p = new_kern(h - *BOX_height(y)); /*773:*/
                *LLIST_link(p) = *BOX_list_ptr(y);
                *BOX_list_ptr(y) = p as i32;
                *BOX_height(y) = h;
            }
        }
        *BOX_width(y) = *BOX_width(x);
        MEM[q + 1].b32.s0 = y as i32;
        MEM[q + 1].b32.s1 = MathCell::SubBox as _;
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
}
unsafe fn make_fraction(q: usize) {
    if MEM[q + 1].b32.s1 == DEFAULT_CODE {
        MEM[q + 1].b32.s1 = default_rule_thickness()
    }
    let mut x = clean_box(
        q + 2,
        (
            match cur_style.0 {
                MathStyle::Display => MathStyle::Text,
                MathStyle::Text => MathStyle::Script,
                MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
            },
            cur_style.1,
        ),
    );
    let mut z = clean_box(
        q + 3,
        (
            match cur_style.0 {
                MathStyle::Display => MathStyle::Text,
                MathStyle::Text => MathStyle::Script,
                MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
            },
            1,
        ),
    );
    if *BOX_width(x) < *BOX_width(z) {
        x = rebox(x, *BOX_width(z))
    } else {
        z = rebox(z, *BOX_width(x))
    }
    let mut shift_up;
    let mut shift_down;
    if cur_style.0 == MathStyle::Display {
        shift_up = num1(cur_size);
        shift_down = denom1(cur_size)
    } else {
        shift_down = denom2(cur_size);
        shift_up = if MEM[q + 1].b32.s1 != 0 {
            num2(cur_size)
        } else {
            num3(cur_size)
        };
    }
    let delta;
    if MEM[q + 1].b32.s1 == 0 {
        /*772:*/
        let clr = if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            if cur_style.0 == MathStyle::Display {
                get_ot_math_constant(cur_f, STACKDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, STACKGAPMIN)
            }
        } else if cur_style.0 == MathStyle::Display {
            7 * default_rule_thickness()
        } else {
            3 * default_rule_thickness()
        };
        delta = half(clr - (shift_up - *BOX_depth(x) - (*BOX_height(z) - shift_down)));
        if delta > 0 {
            shift_up = shift_up + delta;
            shift_down = shift_down + delta
        }
    } else {
        let delta1;
        let delta2;
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            delta = half(MEM[q + 1].b32.s1);
            let clr = if cur_style.0 == MathStyle::Display {
                get_ot_math_constant(cur_f, FRACTIONNUMDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, FRACTIONNUMERATORGAPMIN)
            };
            delta1 = clr - (shift_up - *BOX_depth(x) - (axis_height(cur_size) + delta));
            let clr = if cur_style.0 == MathStyle::Display {
                get_ot_math_constant(cur_f, FRACTIONDENOMDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, FRACTIONDENOMINATORGAPMIN)
            };
            delta2 = clr - (axis_height(cur_size) - delta - (*BOX_height(z) - shift_down))
        } else {
            let clr = if cur_style.0 == MathStyle::Display {
                3 * MEM[q + 1].b32.s1
            } else {
                MEM[q + 1].b32.s1
            };
            delta = half(MEM[q + 1].b32.s1);
            delta1 = clr - (shift_up - *BOX_depth(x) - (axis_height(cur_size) + delta));
            delta2 = clr - (axis_height(cur_size) - delta - (*BOX_height(z) - shift_down))
        }
        if delta1 > 0 {
            shift_up += delta1;
        }
        if delta2 > 0 {
            shift_down += delta2;
        }
    }
    let v = new_null_box();
    set_NODE_type(v, TextNode::VList);
    *BOX_height(v) = shift_up + *BOX_height(x);
    *BOX_depth(v) = *BOX_depth(z) + shift_down;
    *BOX_width(v) = *BOX_width(x);
    let mut p;
    if MEM[q + 1].b32.s1 == 0 {
        p = new_kern(shift_up - *BOX_depth(x) - (*BOX_height(z) - shift_down));
        *LLIST_link(p) = Some(z).tex_int();
    } else {
        let y = fraction_rule(MEM[q + 1].b32.s1);
        p = new_kern(axis_height(cur_size) - delta - (*BOX_height(z) - shift_down));
        *LLIST_link(y) = Some(p).tex_int();
        *LLIST_link(p) = Some(z).tex_int();
        p = new_kern(shift_up - *BOX_depth(x) - (axis_height(cur_size) + delta));
        *LLIST_link(p) = Some(y).tex_int();
    }
    *LLIST_link(x) = Some(p).tex_int();
    *BOX_list_ptr(v) = Some(x).tex_int();
    // :774
    let delta = if cur_style.0 == MathStyle::Display {
        delim1(cur_size)
    } else {
        delim2(cur_size)
    };
    x = var_delimiter(q + 4, cur_size, delta);
    *LLIST_link(x) = Some(v).tex_int();
    z = var_delimiter(q + 5, cur_size, delta);
    *LLIST_link(v) = Some(z).tex_int();
    MEM[q + 1].b32.s1 = Some(hpack(Some(x), 0, PackMode::Additional)).tex_int();
    // :775
}
unsafe fn make_op(q: usize) -> scaled_t {
    if MEM[q].b16.s0 == Limit::Normal as u16 && cur_style.0 == MathStyle::Display {
        MEM[q].b16.s0 = Limit::Limits as u16;
    }
    let mut delta = 0;
    let mut ot_assembly_ptr = ptr::null_mut();
    if MEM[q + 1].b32.s1 == MathCell::MathChar as _ {
        let mut c = 0;
        fetch(q + 1);
        if !(FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && usingOpenType(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine))
        {
            if cur_style.0 == MathStyle::Display && cur_i.s1 as i32 % 4 == LIST_TAG {
                c = cur_i.s0;
                let i = FONT_INFO[(CHAR_BASE[cur_f as usize] + c as i32) as usize].b16;
                if i.s3 as i32 > 0 {
                    cur_c = c as i32;
                    cur_i = i;
                    MEM[q + 1].b16.s0 = c
                }
            }
            delta = FONT_INFO[(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4i32) as usize]
                .b32
                .s1
        }
        let x = clean_box(q + 1, cur_style);
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            if let Some(mut p) = BOX_list_ptr(x).opt() {
                if !is_char_node(Some(p))
                    && NODE_type(p) == TextNode::WhatsIt.into()
                    && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                {
                    let mut ital_corr = true;
                    if cur_style.0 == MathStyle::Display {
                        let mut h1 = 0;
                        h1 = get_ot_math_constant(cur_f, DISPLAYOPERATORMINHEIGHT);
                        if (h1 as f64) < ((*BOX_height(p) + *BOX_depth(p)) * 5) as f64 / 4_f64 {
                            h1 = (((*BOX_height(p) + *BOX_depth(p)) * 5) as f64 / 4_f64) as scaled_t
                        }
                        c = *NATIVE_NODE_glyph(p);
                        let mut n = 0;
                        let mut h2 = 0;
                        loop {
                            let g = get_ot_math_variant(cur_f, c as i32, n, &mut h2, 0);
                            if h2 > 0 {
                                *NATIVE_NODE_glyph(p) = g as u16;
                                measure_native_glyph(
                                    &mut MEM[p] as *mut memory_word as *mut libc::c_void,
                                    1,
                                );
                            }
                            n += 1;
                            if h2 < 0 || h2 >= h1 {
                                break;
                            }
                        }
                        if h2 < 0 {
                            ot_assembly_ptr = get_ot_assembly_ptr(cur_f, c as i32, 0);
                            if !ot_assembly_ptr.is_null() {
                                free_node(p, GLYPH_NODE_SIZE);
                                p = build_opentype_assembly(cur_f, ot_assembly_ptr, h1, false);
                                *BOX_list_ptr(x) = Some(p).tex_int();
                                delta = 0;
                                ital_corr = false;
                            }
                        } else {
                            measure_native_glyph(
                                &mut MEM[p] as *mut memory_word as *mut libc::c_void,
                                1i32,
                            );
                        }
                    }
                    if ital_corr {
                        delta = get_ot_math_ital_corr(cur_f, *NATIVE_NODE_glyph(p) as i32)
                    }
                    *BOX_width(x) = *BOX_width(p);
                    *BOX_height(x) = *BOX_height(p);
                    *BOX_depth(x) = *BOX_depth(p);
                }
            }
        }
        if MEM[q + 3].b32.s1 != 0 && MEM[q as usize].b16.s0 != Limit::Limits as u16 {
            *BOX_width(x) -= delta;
        }
        *BOX_shift_amount(x) = half(*BOX_height(x) - *BOX_depth(x)) - axis_height(cur_size);
        MEM[q + 1].b32.s1 = MathCell::SubBox as _;
        MEM[q + 1].b32.s0 = Some(x).tex_int();
    }
    let save_f = cur_f;
    if MEM[q].b16.s0 == Limit::Limits as u16 {
        // 777:
        let x = clean_box(
            q + 2,
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                cur_style.1,
            ),
        );
        let y = clean_box(q + 1, cur_style);
        let z = clean_box(
            q + 3,
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                1,
            ),
        );
        let v = new_null_box();
        set_NODE_type(v, TextNode::VList);
        *BOX_width(v) = (*BOX_width(y)).max(*BOX_width(x)).max(*BOX_width(z));
        let x = rebox(x, *BOX_width(v));
        let y = rebox(y, *BOX_width(v));
        let z = rebox(z, *BOX_width(v));
        *BOX_shift_amount(x) = half(delta);
        *BOX_shift_amount(z) = -(*BOX_shift_amount(x));
        *BOX_height(v) = *BOX_height(y);
        *BOX_depth(v) = *BOX_depth(y);
        cur_f = save_f;
        if MEM[q + 2].b32.s1 == MathCell::Empty as _ {
            free_node(x, BOX_NODE_SIZE);
            *BOX_list_ptr(v) = y as i32;
        } else {
            let mut shift_up = big_op_spacing3() - *BOX_depth(x);
            if shift_up < big_op_spacing1() {
                shift_up = big_op_spacing1()
            }
            let p = new_kern(shift_up);
            *LLIST_link(p) = y as i32;
            *LLIST_link(x) = p as i32;
            let p = new_kern(big_op_spacing5());
            *LLIST_link(p) = x as i32;
            *BOX_list_ptr(v) = p as i32;
            *BOX_height(v) =
                *BOX_height(v) + big_op_spacing5() + *BOX_height(x) + *BOX_depth(x) + shift_up
        }
        if MEM[q + 3].b32.s1 == MathCell::Empty as _ {
            free_node(z, BOX_NODE_SIZE);
        } else {
            let mut shift_down = big_op_spacing4() - *BOX_height(z);
            if shift_down < big_op_spacing2() {
                shift_down = big_op_spacing2()
            }
            let p = new_kern(shift_down);
            *LLIST_link(y as usize) = Some(p).tex_int();
            *LLIST_link(p) = Some(z).tex_int();
            let p = new_kern(big_op_spacing5());
            *LLIST_link(z) = Some(p).tex_int();
            *BOX_depth(v) =
                *BOX_depth(v) + big_op_spacing5() + *BOX_height(z) + *BOX_depth(z) + shift_down
        }
        MEM[q + 1].b32.s1 = v as i32
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    delta
}
unsafe fn make_ord(q: usize) {
    while MEM[q + 3].b32.s1 == MathCell::Empty as _ {
        if !(MEM[q + 2].b32.s1 == MathCell::Empty as _) {
            break;
        }
        if !(MEM[q + 1].b32.s1 == MathCell::MathChar as _) {
            break;
        }
        let p = LLIST_link(q).opt();
        if p.is_none() {
            break;
        }
        let mut p = p.unwrap();
        if !(MEM[p].b16.s1 >= MathNode::Ord as u16 && MEM[p].b16.s1 <= MathNode::Punct as u16) {
            break;
        }
        if !(MEM[p + 1].b32.s1 == MathCell::MathChar as _) {
            break;
        }
        if !(MEM[p + 1].b16.s1 as i32 % 256 == MEM[q + 1].b16.s1 as i32 % 256) {
            break;
        }
        MEM[q + 1].b32.s1 = MathCell::MathTextChar as _;
        fetch(q + 1);
        if !(cur_i.s1 as i32 % 4 == LIG_TAG) {
            break;
        }
        let mut a = LIG_KERN_BASE[cur_f as usize] + cur_i.s0 as i32;
        cur_c = MEM[p + 1].b16.s0 as i32;
        cur_i = FONT_INFO[a as usize].b16;
        if cur_i.s3 as i32 > 128 {
            a = ((LIG_KERN_BASE[cur_f as usize] + 256 * cur_i.s1 as i32 + cur_i.s0 as i32) as i64
                + 32768
                - (256 * 128) as i64) as i32;
            cur_i = FONT_INFO[a as usize].b16
        }
        loop {
            if cur_i.s2 as i32 == cur_c {
                if cur_i.s3 as i32 <= 128 {
                    if cur_i.s1 as i32 >= 128 {
                        p = new_kern(
                            FONT_INFO[(KERN_BASE[cur_f as usize]
                                + 256 * cur_i.s1 as i32
                                + cur_i.s0 as i32) as usize]
                                .b32
                                .s1,
                        );
                        *LLIST_link(p) = *LLIST_link(q);
                        *LLIST_link(q) = Some(p).tex_int();
                        return;
                    } else {
                        match cur_i.s1 as i32 {
                            1 | 5 => MEM[q + 1].b16.s0 = cur_i.s0,
                            2 | 6 => MEM[p + 1].b16.s0 = cur_i.s0,
                            3 | 7 | 11 => {
                                let r = new_noad();
                                MEM[r + 1].b16.s0 = cur_i.s0;
                                MEM[r + 1].b16.s1 = (MEM[q + 1].b16.s1 as i32 % 256) as u16;
                                *LLIST_link(q) = Some(r).tex_int();
                                *LLIST_link(r) = Some(p).tex_int();
                                if (cur_i.s1 as i32) < 11 {
                                    MEM[r + 1].b32.s1 = MathCell::MathChar as _;
                                } else {
                                    MEM[r + 1].b32.s1 = MathCell::MathTextChar as _;
                                }
                            }
                            _ => {
                                *LLIST_link(q) = *LLIST_link(p);
                                MEM[q + 1].b16.s0 = cur_i.s0;
                                MEM[q + 3] = MEM[p + 3];
                                MEM[q + 2] = MEM[p + 2];
                                free_node(p, NOAD_SIZE);
                            }
                        }
                        if cur_i.s1 as i32 > 3 {
                            return;
                        }
                        MEM[q + 1].b32.s1 = MathCell::MathChar as _;
                        break;
                    }
                }
            }
            if cur_i.s3 as i32 >= 128 {
                return;
            }
            a = a + cur_i.s3 as i32 + 1;
            cur_i = FONT_INFO[a as usize].b16
        }
    }
}
unsafe fn attach_hkern_to_new_hlist(q: usize, mut delta: scaled_t) -> usize {
    let mut z = Some(new_kern(delta)).tex_int();
    if let Some(mut y) = MEM[q + 1].b32.s1.opt() {
        while let Some(next) = LLIST_link(y).opt() {
            y = next;
        }
        *LLIST_link(y) = z;
    } else {
        MEM[q + 1].b32.s1 = z;
    }
    MEM[q + 1].b32.s1 as usize
}
unsafe fn make_scripts(q: usize, mut delta: scaled_t) {
    let mut shift_up: scaled_t = 0;
    let mut shift_down: scaled_t = 0;
    let mut clr: scaled_t = 0;
    let mut p = MEM[q + 1].b32.s1;
    let mut script_g = 0_u16;
    let mut script_f = 0;
    let mut sup_kern = 0i32;
    let mut sub_kern = 0i32;
    if is_char_node(p.opt())
        || p.opt().is_some()
            && !is_char_node(p.opt())
            && NODE_type(p as usize) == TextNode::WhatsIt.into()
            && whatsit_NODE_subtype(p as usize) == WhatsItNST::Glyph
    {
        shift_up = 0;
        shift_down = 0;
    } else {
        let z = hpack(p.opt(), 0, PackMode::Additional);
        let t = match cur_style.0 {
            MathStyle::Display | MathStyle::Text => SCRIPT_SIZE,
            MathStyle::Script | MathStyle::ScriptScript => SCRIPT_SCRIPT_SIZE,
        };
        shift_up = *BOX_height(z) - sup_drop(t);
        shift_down = *BOX_depth(z) + sub_drop(t);
        free_node(z, BOX_NODE_SIZE);
    }
    let mut x: usize;
    if MEM[q + 2].b32.s1 == MathCell::Empty as _ {
        // 784:
        let save_f = cur_f;
        x = clean_box(
            q + 3,
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                1,
            ),
        );
        cur_f = save_f;
        *BOX_width(x) += *DIMENPAR(DimenPar::script_space);
        shift_down = shift_down.max(sub1(cur_size));
        clr = if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            *BOX_height(x) - get_ot_math_constant(cur_f, SUBSCRIPTTOPMAX)
        } else {
            *BOX_height(x) - (math_x_height(cur_size) * 4).abs() / 5
        };
        if shift_down < clr {
            shift_down = clr;
        }
        *BOX_shift_amount(x) = shift_down;
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            /*787: */
            if MEM[q + 3].b32.s1 == MathCell::MathChar as _ {
                let save_f = cur_f;
                fetch(q + 3);
                if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    let script_c = new_native_character(cur_f, cur_c);
                    script_g = real_get_native_glyph(
                        &mut MEM[script_c] as *mut memory_word as *mut libc::c_void,
                        0,
                    );
                    script_f = cur_f;
                } else {
                    script_g = 0;
                    script_f = 0;
                }
                cur_f = save_f
            }
            if let Some(p) = p.opt() {
                if !is_char_node(Some(p))
                    && NODE_type(p) == TextNode::WhatsIt.into()
                    && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                {
                    sub_kern = get_ot_math_kern(
                        *NATIVE_NODE_font(p) as usize,
                        *NATIVE_NODE_glyph(p) as i32,
                        script_f,
                        script_g as i32,
                        SUB_CMD,
                        shift_down,
                    )
                }
            }
            if sub_kern != 0 {
                p = attach_hkern_to_new_hlist(q, sub_kern) as i32;
            }
        }
    } else {
        let save_f = cur_f;
        x = clean_box(
            q + 2,
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                cur_style.1,
            ),
        );
        cur_f = save_f;
        *BOX_width(x) += *DIMENPAR(DimenPar::script_space);
        clr = if cur_style.1 != 0 {
            sup3(cur_size)
        } else if cur_style.0 == MathStyle::Display {
            sup1(cur_size)
        } else {
            sup2(cur_size)
        };
        if shift_up < clr {
            shift_up = clr
        }
        clr = if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            *BOX_depth(x) + get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMIN)
        } else {
            *BOX_depth(x) + math_x_height(cur_size).abs() / 4
        };
        if shift_up < clr {
            shift_up = clr
        }
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            // 788:
            if MEM[q + 2].b32.s1 == MathCell::MathChar as _ {
                let save_f = cur_f;
                fetch(q + 2);
                if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    let script_c = new_native_character(cur_f, cur_c);
                    script_g = real_get_native_glyph(
                        &mut MEM[script_c] as *mut memory_word as *mut libc::c_void,
                        0,
                    );
                    script_f = cur_f
                } else {
                    script_g = 0;
                    script_f = 0;
                }
                cur_f = save_f
            }
            if let Some(p) = p.opt() {
                if !is_char_node(Some(p))
                    && NODE_type(p) == TextNode::WhatsIt.into()
                    && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                {
                    sup_kern = get_ot_math_kern(
                        *NATIVE_NODE_font(p) as usize,
                        *NATIVE_NODE_glyph(p) as i32,
                        script_f,
                        script_g as i32,
                        SUP_CMD,
                        shift_up,
                    )
                }
            }
            if sup_kern != 0i32 && MEM[q + 3].b32.s1 == MathCell::Empty as _ {
                p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
            }
        }
        if MEM[q + 3].b32.s1 == MathCell::Empty as _ {
            *BOX_shift_amount(x) = -(shift_up as i32)
        } else {
            // 786:
            let save_f = cur_f;
            let y = clean_box(
                q + 3,
                (
                    match cur_style.0 {
                        MathStyle::Display | MathStyle::Text => MathStyle::Script,
                        MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                    },
                    1,
                ),
            );
            cur_f = save_f;
            *BOX_width(y) += *DIMENPAR(DimenPar::script_space);
            if shift_down < sub2(cur_size) {
                shift_down = sub2(cur_size)
            }
            if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                    as i32
                    != 0
            {
                clr = get_ot_math_constant(cur_f, SUBSUPERSCRIPTGAPMIN)
                    - (shift_up - *BOX_depth(x) - (*BOX_height(y) - shift_down))
            } else {
                clr = 4 * default_rule_thickness()
                    - (shift_up - *BOX_depth(x) - (*BOX_height(y) - shift_down))
            }
            if clr > 0 {
                shift_down = shift_down + clr;
                if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    clr = get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT)
                        - (shift_up - *BOX_depth(x))
                } else {
                    clr = (math_x_height(cur_size) * 4).abs() / 5 - (shift_up - *BOX_depth(x))
                }
                if clr > 0i32 {
                    shift_up = shift_up + clr;
                    shift_down = shift_down - clr
                }
            }
            if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                    as i32
                    != 0
            {
                if MEM[q + 3].b32.s1 == MathCell::MathChar as _ {
                    let save_f = cur_f;
                    fetch(q + 3);
                    if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                        && isOpenTypeMathFont(
                            FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                        ) as i32
                            != 0
                    {
                        let script_c = new_native_character(cur_f, cur_c);
                        script_g = real_get_native_glyph(
                            &mut MEM[script_c] as *mut memory_word as *mut libc::c_void,
                            0,
                        );
                        script_f = cur_f
                    } else {
                        script_g = 0;
                        script_f = 0;
                    }
                    cur_f = save_f
                }
                if let Some(p) = p.opt() {
                    if !is_char_node(Some(p))
                        && NODE_type(p) == TextNode::WhatsIt.into()
                        && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                    {
                        sub_kern = get_ot_math_kern(
                            *NATIVE_NODE_font(p) as usize,
                            *NATIVE_NODE_glyph(p) as i32,
                            script_f,
                            script_g as i32,
                            SUB_CMD,
                            shift_down,
                        )
                    }
                }
                if sub_kern != MathCell::MathChar as _ {
                    p = attach_hkern_to_new_hlist(q, sub_kern) as i32;
                }
                if MEM[q + 2].b32.s1 == 1 {
                    let save_f = cur_f;
                    fetch(q + 2);
                    if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                        && isOpenTypeMathFont(
                            FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                        ) as i32
                            != 0
                    {
                        let script_c = new_native_character(cur_f, cur_c);
                        script_g = real_get_native_glyph(
                            &mut MEM[script_c] as *mut memory_word as *mut libc::c_void,
                            0,
                        );
                        script_f = cur_f
                    } else {
                        script_g = 0;
                        script_f = 0;
                    }
                    cur_f = save_f
                }
                if let Some(p) = p.opt() {
                    if !is_char_node(Some(p))
                        && NODE_type(p) == TextNode::WhatsIt.into()
                        && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                    {
                        sup_kern = get_ot_math_kern(
                            *NATIVE_NODE_font(p) as usize,
                            *NATIVE_NODE_glyph(p) as i32,
                            script_f,
                            script_g as i32,
                            SUP_CMD,
                            shift_up,
                        )
                    }
                }
                if sup_kern != 0 && MEM[(q + 3) as usize].b32.s1 == MathCell::Empty as _ {
                    p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
                }
            }
            *BOX_shift_amount(x) = sup_kern + delta - sub_kern;
            let p = new_kern(shift_up - *BOX_depth(x) - (*BOX_height(y) - shift_down));
            *LLIST_link(x) = Some(p).tex_int();
            *LLIST_link(p) = y as i32;
            x = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD) as usize;
            *BOX_shift_amount(x) = shift_down;
        }
    }
    if let Some(mut p) = MEM[q + 1].b32.s1.opt() {
        while let Some(next) = LLIST_link(p).opt() {
            p = next;
        }
        *LLIST_link(p) = Some(x).tex_int();
    } else {
        MEM[q + 1].b32.s1 = Some(x).tex_int();
    }
}
unsafe fn make_left_right(
    mut q: usize,
    mut style: (MathStyle, u8),
    mut max_d: scaled_t,
    mut max_h: scaled_t,
) -> i16 {
    cur_style = style;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18);
    let delta2 = max_d + axis_height(cur_size);
    let mut delta1 = max_h + max_d - delta2;
    if delta2 > delta1 {
        delta1 = delta2
    }
    let mut delta = delta1 / 500 * *INTPAR(IntPar::delimiter_factor);
    let delta2 = delta1 + delta1 - *DIMENPAR(DimenPar::delimiter_shortfall);
    if delta < delta2 {
        delta = delta2
    }
    MEM[q + 1].b32.s1 = var_delimiter(q + 1, cur_size, delta) as i32;
    (MEM[q].b16.s1 as i32 - (MathNode::Left as i32 - 20)) as i16
}
unsafe fn mlist_to_hlist() {
    let mlist = cur_mlist;
    let penalties = mlist_penalties;
    let style = cur_style;
    let mut qopt = mlist.opt();
    let mut r = None.tex_int();
    let mut r_type = MathNode::Op as i16;
    let mut max_h = 0;
    let mut max_d = 0;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18);
    while let Some(q) = qopt {
        // 753:
        match ND::from(MEM[q].b16.s1) {
            ND::Math(n) => {
                let mut delta = 0; /*:755 */
                let mut flag = true;
                match n {
                    MathNode::Bin => match ND::from(r_type as u16) {
                        ND::Math(rt) => match rt {
                            MathNode::Bin
                            | MathNode::Op
                            | MathNode::Rel
                            | MathNode::Open
                            | MathNode::Punct
                            | MathNode::Left => {
                                MEM[q].b16.s1 = MathNode::Ord as u16;
                                continue;
                            }
                            _ => {}
                        },
                        _ => {
                            unreachable!();
                        }
                    },
                    MathNode::Rel | MathNode::Close | MathNode::Punct | MathNode::Right => {
                        if r_type as u16 == MathNode::Bin as u16 {
                            MEM[r as usize].b16.s1 = MathNode::Ord as u16;
                        }
                        if MEM[q].b16.s1 == MathNode::Right as u16 {
                            flag = false;
                        }
                    }
                    MathNode::Left => {
                        flag = false;
                    }
                    MathNode::Fraction => {
                        make_fraction(q);
                        flag = false;
                        /*check_dimensions */
                        let z = hpack(MEM[q + 1].b32.s1.opt(), 0, PackMode::Additional);
                        max_h = max_h.max(*BOX_height(z));
                        max_d = max_d.max(*BOX_depth(z));
                        free_node(z, BOX_NODE_SIZE);
                    }
                    MathNode::Op => {
                        delta = make_op(q);
                        if MEM[q].b16.s0 == Limit::Limits as u16 {
                            flag = false;
                            /*check_dimensions */
                            let z = hpack(MEM[q + 1].b32.s1.opt(), 0, PackMode::Additional);
                            max_h = max_h.max(*BOX_height(z));
                            max_d = max_d.max(*BOX_depth(z));
                            free_node(z, BOX_NODE_SIZE);
                        } else {
                        }
                    }
                    MathNode::Ord => {
                        make_ord(q as usize);
                    }
                    MathNode::Open | MathNode::Inner => {}
                    MathNode::Radical => {
                        make_radical(q);
                    }
                    MathNode::Over => {
                        make_over(q);
                    }
                    MathNode::Under => {
                        make_under(q);
                    }
                    MathNode::Accent => {
                        make_math_accent(q);
                    }
                    MathNode::VCenter => {
                        make_vcenter(q);
                    }
                }
                if flag {
                    let p = match MEM[q + 1].b32.s1 {
                        1 | 4 => {
                            fetch(q + 1);
                            if FONT_AREA[cur_f as usize] as u32 == AAT_FONT_FLAG
                                || FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                            {
                                let z = new_native_character(cur_f, cur_c);
                                let p = get_node(GLYPH_NODE_SIZE);
                                set_NODE_type(p, TextNode::WhatsIt);
                                set_whatsit_NODE_subtype(p, WhatsItNST::Glyph);
                                *NATIVE_NODE_font(p) = cur_f as u16;
                                *NATIVE_NODE_glyph(p) = real_get_native_glyph(
                                    &mut MEM[z] as *mut memory_word as *mut libc::c_void,
                                    0,
                                );
                                measure_native_glyph(
                                    &mut MEM[p] as *mut memory_word as *mut libc::c_void,
                                    1,
                                );
                                free_node(z, *NATIVE_NODE_size(z) as i32);
                                delta = get_ot_math_ital_corr(cur_f, *NATIVE_NODE_glyph(p) as i32);
                                if MEM[q + 1].b32.s1 == MathCell::MathTextChar as _
                                    && !(FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                                        && isOpenTypeMathFont(
                                            FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                                        ) as i32
                                            != 0) as i32
                                        != 0
                                {
                                    delta = 0;
                                }
                                if MEM[q + 3].b32.s1 == MathCell::Empty as _ && delta != 0 {
                                    *LLIST_link(p) = Some(new_kern(delta)).tex_int();
                                    delta = 0;
                                }
                                Some(p)
                            } else if cur_i.s3 as i32 > 0 {
                                delta = FONT_INFO[(ITALIC_BASE[cur_f as usize]
                                    + cur_i.s1 as i32 / 4i32)
                                    as usize]
                                    .b32
                                    .s1;
                                let p = new_character(cur_f, cur_c as UTF16_code);
                                if MEM[q + 1].b32.s1 == MathCell::MathTextChar as _
                                    && FONT_INFO[(2 + PARAM_BASE[cur_f as usize]) as usize].b32.s1
                                        != 0
                                {
                                    delta = 0;
                                }
                                if MEM[q + 3].b32.s1 == MathCell::Empty as _ && delta != 0 {
                                    *LLIST_link(p.unwrap()) = Some(new_kern(delta)).tex_int();
                                    delta = 0i32
                                }
                                p
                            } else {
                                None
                            }
                        }
                        0 => None,
                        2 => MEM[q + 1].b32.s0.opt(),
                        3 => {
                            cur_mlist = MEM[q + 1].b32.s0;
                            let save_style = cur_style;
                            mlist_penalties = false;
                            mlist_to_hlist();
                            cur_style = save_style;
                            cur_size = cur_style.0.size();
                            cur_mu = x_over_n(math_quad(cur_size), 18);
                            Some(hpack(LLIST_link(TEMP_HEAD).opt(), 0, PackMode::Additional))
                        }
                        _ => confusion(b"mlist2"),
                    };
                    MEM[q + 1].b32.s1 = p.tex_int();
                    if !(MEM[q + 3].b32.s1 == MathCell::Empty as _
                        && MEM[q + 2].b32.s1 == MathCell::Empty as _)
                    {
                        make_scripts(q, delta);
                    }
                    /*check_dimensions */
                    let z = hpack(MEM[q + 1].b32.s1.opt(), 0, PackMode::Additional);
                    max_h = max_h.max(*BOX_height(z));
                    max_d = max_d.max(*BOX_depth(z));
                    free_node(z, BOX_NODE_SIZE);
                }
                /*done_with_noad */
                r = q as i32;
                r_type = MEM[r as usize].b16.s1 as i16;
                if r_type == MathNode::Right as i16 {
                    r_type = MathNode::Left as i16;
                    cur_style = style;
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18)
                }
            }
            ND::Text(n) => match n {
                TextNode::Style => {
                    let m = MEM[q].b16.s0;
                    cur_style = (MathStyle::n((m / 2) as i16).unwrap(), (m % 2) as u8);
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18);
                }
                TextNode::Choice => {
                    let mut p = None;
                    match cur_style.0 {
                        MathStyle::Display => {
                            p = CHOICE_NODE_display(q).opt();
                            *CHOICE_NODE_display(q) = None.tex_int();
                        }
                        MathStyle::Text => {
                            p = CHOICE_NODE_text(q).opt();
                            *CHOICE_NODE_text(q) = None.tex_int();
                        }
                        MathStyle::Script => {
                            p = CHOICE_NODE_script(q).opt();
                            *CHOICE_NODE_script(q) = None.tex_int();
                        }
                        MathStyle::ScriptScript => {
                            p = CHOICE_NODE_scriptscript(q).opt();
                            *CHOICE_NODE_scriptscript(q) = None.tex_int();
                        }
                    }
                    flush_node_list(CHOICE_NODE_display(q).opt());
                    flush_node_list(CHOICE_NODE_text(q).opt());
                    flush_node_list(CHOICE_NODE_script(q).opt());
                    flush_node_list(CHOICE_NODE_scriptscript(q).opt());
                    set_NODE_type(q, TextNode::Style);
                    MEM[q].b16.s0 = (cur_style.0 as u16) * 2 + cur_style.1 as u16;
                    MEM[q + 1].b32.s1 = 0;
                    MEM[q + 2].b32.s1 = 0;
                    if let Some(mut p) = p {
                        let z = *LLIST_link(q);
                        *LLIST_link(q) = Some(p).tex_int();
                        while let Some(next) = LLIST_link(p as usize).opt() {
                            p = next;
                        }
                        *LLIST_link(p) = z;
                    }
                }
                TextNode::Ins
                | TextNode::Mark
                | TextNode::Adjust
                | TextNode::WhatsIt
                | TextNode::Penalty
                | TextNode::Disc => {}
                TextNode::Rule => {
                    max_h = max_h.max(*BOX_height(q));
                    max_d = max_d.max(*BOX_depth(q));
                }
                TextNode::Glue => {
                    if MEM[q].b16.s0 == MU_GLUE {
                        let x = *GLUE_NODE_glue_ptr(q) as usize;
                        let y = math_glue(x, cur_mu);
                        delete_glue_ref(x);
                        *GLUE_NODE_glue_ptr(q) = Some(y).tex_int();
                        MEM[q].b16.s0 = NORMAL as u16
                    } else if cur_size != TEXT_SIZE && MEM[q].b16.s0 == COND_MATH_GLUE {
                        if let Some(p) = LLIST_link(q).opt() {
                            if NODE_type(p) == TextNode::Glue.into()
                                || NODE_type(p) == TextNode::Kern.into()
                            {
                                *LLIST_link(q) = *LLIST_link(p);
                                *LLIST_link(p) = None.tex_int();
                                flush_node_list(Some(p));
                            }
                        }
                    }
                }
                TextNode::Kern => {
                    math_kern(q as usize, cur_mu);
                }
                _ => confusion(b"mlist1"),
            },
            _ => confusion(b"mlist1"),
        }

        /*done_with_node */
        qopt = LLIST_link(q).opt();
    } /*ord_noad *//*:755 */

    if r_type == MathNode::Bin as i16 {
        MEM[r as usize].b16.s1 = 16;
    }
    let mut p = TEMP_HEAD;
    *LLIST_link(p) = None.tex_int();
    let mut qopt = mlist.opt();
    let mut r_type = 0 as i16;
    cur_style = style;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18);
    while let Some(q) = qopt {
        let mut t = MathNode::Ord as i16;
        let mut s = NOAD_SIZE as i16;
        let mut pen = INF_PENALTY;
        match NODE_type(q) {
            ND::Math(n) => match n {
                MathNode::Op
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct
                | MathNode::Inner => {
                    t = MEM[q].b16.s1 as i16;
                }
                MathNode::Bin => {
                    t = MathNode::Bin as i16;
                    pen = *INTPAR(IntPar::bin_op_penalty);
                }
                MathNode::Rel => {
                    t = MathNode::Rel as i16;
                    pen = *INTPAR(IntPar::rel_penalty);
                }
                MathNode::Ord | MathNode::VCenter | MathNode::Over | MathNode::Under => {}
                MathNode::Radical => {
                    s = RADICAL_NOAD_SIZE as i16;
                }
                MathNode::Accent => {
                    s = ACCENT_NOAD_SIZE as i16;
                }
                MathNode::Fraction => {
                    t = MathNode::Inner as i16;
                    s = FRACTION_NOAD_SIZE as i16;
                }
                MathNode::Left | MathNode::Right => {
                    t = make_left_right(q, style, max_d, max_h);
                }
            },
            ND::Text(n) => match n {
                TextNode::Style => {
                    let m = MEM[q].b16.s0;
                    cur_style = (MathStyle::n((m / 2) as i16).unwrap(), (m % 2) as u8);
                    s = STYLE_NODE_SIZE as i16;
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18);
                    /*delete_q */
                    qopt = LLIST_link(q).opt();
                    free_node(q, s as i32);
                    continue;
                }
                TextNode::WhatsIt
                | TextNode::Penalty
                | TextNode::Rule
                | TextNode::Disc
                | TextNode::Adjust
                | TextNode::Ins
                | TextNode::Mark
                | TextNode::Glue
                | TextNode::Kern => {
                    *LLIST_link(p) = Some(q).tex_int();
                    p = q;
                    qopt = LLIST_link(q).opt();
                    *LLIST_link(p) = None.tex_int();
                    continue;
                }
                _ => confusion(b"mlist3"),
            },
            _ => confusion(b"mlist3"),
        }
        if r_type > 0 {
            const OFFSET_TABLE: [&[u8]; 8] = [
                b"02340001",
                b"22*40001",
                b"33**3**3",
                b"44*04004",
                b"00*00000",
                b"02340001",
                b"11*11111",
                b"12341011",
            ];
            // The inter-element spacing in math formulas depends on a 8x8 table.
            // The table indices range from MathNode::Ord to MathNode::Inner.
            // The chars of this table have the following significance:
            let x = match OFFSET_TABLE[r_type as usize - MathNode::Ord as usize]
                [t as usize - MathNode::Ord as usize]
            {
                b'0' => {
                    // no space
                    0
                }
                b'1' => {
                    // a conditional thin space
                    match cur_style.0 {
                        MathStyle::Display | MathStyle::Text => GluePar::thin_mu_skip as i32,
                        _ => 0,
                    }
                }
                b'2' => {
                    // a thin space
                    GluePar::thin_mu_skip as i32
                }
                b'3' => {
                    // a conditional medium space
                    match cur_style.0 {
                        MathStyle::Display | MathStyle::Text => GluePar::med_mu_skip as i32,
                        _ => 0,
                    }
                }
                b'4' => {
                    // a conditional thick space
                    match cur_style.0 {
                        MathStyle::Display | MathStyle::Text => GluePar::thick_mu_skip as i32,
                        _ => 0,
                    }
                }
                _ => {
                    // impossible
                    confusion(b"mlist4");
                }
            };
            if x != 0 {
                let y = math_glue(EQTB[GLUE_BASE + x as usize].val as usize, cur_mu);
                let z = new_glue(y);
                *LLIST_link(y) = None.tex_int();
                *LLIST_link(p) = Some(z).tex_int();
                p = z;
                MEM[z].b16.s0 = (x + 1) as u16
            }
        }
        if let Some(mut lst) = MEM[q + 1].b32.s1.opt() {
            *LLIST_link(p) = Some(lst).tex_int();
            loop {
                p = lst;
                if let Some(next) = LLIST_link(lst).opt() {
                    lst = next;
                } else {
                    break;
                }
            }
        }
        if penalties {
            if let Some(m) = LLIST_link(q).opt() {
                if pen < INF_PENALTY {
                    r_type = MEM[m].b16.s1 as i16;
                    if ND::from(r_type as u16) != TextNode::Penalty.into() {
                        if r_type != MathNode::Rel as i16 {
                            let z = new_penalty(pen);
                            *LLIST_link(p) = Some(z).tex_int();
                            p = z
                        }
                    }
                }
            }
        }
        if MEM[q].b16.s1 == MathNode::Right as u16 {
            t = MathNode::Open as i16;
        }
        r_type = t;
        /*delete_q */
        qopt = LLIST_link(q).opt();
        free_node(q, s as i32);
    }
}
unsafe fn var_delimiter(d: usize, mut s: usize, mut v: scaled_t) -> usize {
    let mut c: u16 = 0;
    let mut y: u16 = 0;
    let mut n: i32 = 0;
    let mut u: scaled_t = 0;
    let mut q: b16x4 = b16x4_le_t {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };

    let mut f = FONT_BASE;
    let mut w = 0;
    let mut large_attempt = false;
    let mut z = MEM[d].b16.s3 as i32 % 256;
    let mut x = (MEM[d].b16.s2 as i64 + (MEM[d].b16.s3 as i32 / 256) as i64 * 65536) as u16;
    let mut ot_assembly_ptr = ptr::null_mut();
    's_62: loop {
        if z != 0 || x != 0 {
            z = z + s as i32 + 256;
            loop {
                z = z - 256;
                let g = MATH_FONT(z as usize);
                if g != FONT_BASE {
                    /*734: */
                    if FONT_AREA[g as usize] as u32 == OTGR_FONT_FLAG
                        && usingOpenType(FONT_LAYOUT_ENGINE[g as usize] as XeTeXLayoutEngine) as i32
                            != 0
                    {
                        x = map_char_to_glyph(g, x as i32) as u16;
                        f = g;
                        c = x;
                        w = 0;
                        n = 0;
                        loop {
                            y = get_ot_math_variant(g, x as i32, n, &mut u, 0) as u16;
                            if u > w {
                                c = y;
                                w = u;
                                if u >= v {
                                    break 's_62;
                                }
                            }
                            n = n + 1;
                            if u < 0 {
                                break;
                            }
                        }
                        ot_assembly_ptr = get_ot_assembly_ptr(g, x as i32, 0);
                        if !ot_assembly_ptr.is_null() {
                            break 's_62;
                        }
                    } else {
                        y = x;
                        if y as i32 >= FONT_BC[g as usize] as i32
                            && y as i32 <= FONT_EC[g as usize] as i32
                        {
                            loop {
                                q = FONT_INFO[(CHAR_BASE[g as usize] + y as i32) as usize].b16;
                                if !(q.s3 as i32 > 0) {
                                    break;
                                }
                                if q.s1 as i32 % 4 == EXT_TAG {
                                    f = g;
                                    c = y;
                                    break 's_62;
                                } else {
                                    u = FONT_INFO
                                        [(HEIGHT_BASE[g as usize] + q.s2 as i32 / 16) as usize]
                                        .b32
                                        .s1
                                        + FONT_INFO
                                            [(DEPTH_BASE[g as usize] + q.s2 as i32 % 16) as usize]
                                            .b32
                                            .s1;
                                    if u > w {
                                        f = g;
                                        c = y;
                                        w = u;
                                        if u >= v {
                                            break 's_62;
                                        }
                                    }
                                    if !(q.s1 as i32 % 4 == LIST_TAG) {
                                        break;
                                    }
                                    y = q.s0
                                }
                            }
                        }
                    }
                }
                if z < 256 {
                    break;
                }
            }
        }
        if large_attempt {
            break;
        }
        large_attempt = true;
        z = MEM[d].b16.s1 as i32 % 256;
        x = (MEM[d].b16.s0 as i64 + (MEM[d].b16.s1 as i32 / 256) as i64 * 65536) as u16
    }
    let b;
    if f != FONT_BASE {
        if !(FONT_AREA[f] as u32 == OTGR_FONT_FLAG
            && usingOpenType(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine))
        {
            /*736: */
            if q.s1 as i32 % 4 == EXT_TAG {
                /*739: */
                b = new_null_box();
                set_NODE_type(b, TextNode::VList);
                let r = FONT_INFO[(EXTEN_BASE[f] + q.s0 as i32) as usize].b16;
                c = r.s0;
                u = height_plus_depth(f, c);
                w = 0;
                q = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize].b16;
                *BOX_width(b) = FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1
                    + FONT_INFO[(ITALIC_BASE[f] + q.s1 as i32 / 4) as usize]
                        .b32
                        .s1;
                c = r.s1;
                if c != 0 {
                    w = w + height_plus_depth(f, c)
                }
                c = r.s2;
                if c != 0 {
                    w = w + height_plus_depth(f, c)
                }
                c = r.s3;
                if c != 0 {
                    w = w + height_plus_depth(f, c)
                }
                n = 0;
                if u > 0 {
                    while w < v {
                        w = w + u;
                        n += 1;
                        if r.s2 != 0 {
                            w = w + u
                        }
                    }
                }
                c = r.s1;
                if c != 0 {
                    stack_into_box(b, f, c);
                }
                c = r.s0;
                for _ in 0..n {
                    stack_into_box(b, f, c);
                }
                c = r.s2;
                if c != 0 {
                    stack_into_box(b, f, c);
                    c = r.s0;
                    for _ in 0..n {
                        stack_into_box(b, f, c);
                    }
                }
                c = r.s3;
                if c != 0 {
                    stack_into_box(b, f, c);
                }
                *BOX_depth(b) = w - *BOX_height(b);
            } else {
                b = char_box(f, c as i32)
            }
        /*:736 */
        } else if !ot_assembly_ptr.is_null() {
            b = build_opentype_assembly(f, ot_assembly_ptr, v, false) as usize
        } else {
            b = new_null_box();
            set_NODE_type(b, TextNode::VList);
            let g = get_node(GLYPH_NODE_SIZE);
            *BOX_list_ptr(b) = g as i32;
            set_NODE_type(g, TextNode::WhatsIt);
            set_whatsit_NODE_subtype(g, WhatsItNST::Glyph);
            *NATIVE_NODE_font(g) = f as u16;
            *NATIVE_NODE_glyph(g) = c;
            measure_native_glyph(&mut MEM[g] as *mut memory_word as *mut libc::c_void, 1);
            *BOX_width(b) = *BOX_width(g);
            *BOX_height(b) = *BOX_height(g);
            *BOX_depth(b) = *BOX_depth(g);
        }
    } else {
        b = new_null_box();
        *BOX_width(b) = *DIMENPAR(DimenPar::null_delimiter_space)
    }
    *BOX_shift_amount(b) = half(*BOX_height(b) - *BOX_depth(b)) - axis_height(s);
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    b
}
unsafe fn char_box(mut f: internal_font_number, mut c: i32) -> usize {
    let b;
    let p;
    if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
        b = new_null_box();
        p = new_native_character(f, c);
        *BOX_list_ptr(b) = Some(p).tex_int();
        *BOX_height(b) = *BOX_height(p);
        *BOX_width(b) = *BOX_width(p);
        *BOX_depth(b) = (*BOX_depth(p)).max(0);
    } else {
        let q = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c as u16)) as usize].b16;
        b = new_null_box();
        *BOX_width(b) = FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1
            + FONT_INFO[(ITALIC_BASE[f] + q.s1 as i32 / 4) as usize]
                .b32
                .s1;
        *BOX_height(b) = FONT_INFO[(HEIGHT_BASE[f] + q.s2 as i32 / 16) as usize]
            .b32
            .s1;
        *BOX_depth(b) = FONT_INFO[(DEPTH_BASE[f] + q.s2 as i32 % 16) as usize]
            .b32
            .s1;
        p = get_avail();
        MEM[p].b16.s0 = c as u16;
        MEM[p].b16.s1 = f as u16
    }
    *BOX_list_ptr(b) = Some(p).tex_int();
    b
}
unsafe fn stack_into_box(b: usize, mut f: internal_font_number, mut c: u16) {
    let p = char_box(f, c as i32);
    *LLIST_link(p) = *BOX_list_ptr(b);
    *BOX_list_ptr(b) = Some(p).tex_int();
    *BOX_height(b) = *BOX_height(p);
}
unsafe fn height_plus_depth(mut f: internal_font_number, mut c: u16) -> scaled_t {
    let mut q: b16x4 = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize].b16;
    FONT_INFO[(HEIGHT_BASE[f] + q.s2 as i32 / 16) as usize]
        .b32
        .s1
        + FONT_INFO[(DEPTH_BASE[f] + q.s2 as i32 % 16) as usize]
            .b32
            .s1
}
unsafe fn stack_glyph_into_box(b: usize, mut f: internal_font_number, mut g: i32) {
    let p = get_node(GLYPH_NODE_SIZE);
    set_NODE_type(p, TextNode::WhatsIt);
    set_whatsit_NODE_subtype(p, WhatsItNST::Glyph);
    MEM[p + 4].b16.s2 = f as u16;
    MEM[p + 4].b16.s1 = g as u16;
    measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1);
    if NODE_type(b) == TextNode::HList.into() {
        if let Some(mut q) = BOX_list_ptr(b).opt() {
            while let Some(next) = LLIST_link(q).opt() {
                q = next;
            }
            *LLIST_link(q) = Some(p).tex_int();
            *BOX_height(b) = (*BOX_height(b)).max(*BOX_height(p));
            *BOX_depth(b) = (*BOX_depth(b)).max(*BOX_depth(p));
        } else {
            *BOX_list_ptr(b) = Some(p).tex_int();
        }
    } else {
        *LLIST_link(p) = *BOX_list_ptr(b);
        *BOX_list_ptr(b) = Some(p).tex_int();
        *BOX_height(b) = *BOX_height(p);
        *BOX_width(b) = (*BOX_width(b)).max(*BOX_width(p));
    };
}
unsafe fn stack_glue_into_box(b: usize, mut min: scaled_t, mut max: scaled_t) {
    let q = new_spec(0);
    *GLUE_SPEC_size(q) = min;
    *GLUE_SPEC_stretch(q) = max - min;
    let p = new_glue(q);
    if NODE_type(b) == TextNode::HList.into() {
        if let Some(mut q) = BOX_list_ptr(b).opt() {
            while let Some(next) = LLIST_link(q).opt() {
                q = next;
            }
            *LLIST_link(q) = Some(p).tex_int();
        } else {
            *BOX_list_ptr(b) = Some(p).tex_int();
        }
    } else {
        *LLIST_link(p) = *BOX_list_ptr(b);
        *BOX_list_ptr(b) = Some(p).tex_int();
        *BOX_height(b) = MEM[p + 3].b32.s1; // TODO: strange, maybe BUG
        *BOX_width(b) = MEM[p + 1].b32.s1;
    };
}
unsafe fn build_opentype_assembly(
    mut f: internal_font_number,
    mut a: *mut libc::c_void,
    mut s: scaled_t,
    mut horiz: bool,
) -> usize {
    let mut b = new_null_box();
    set_NODE_type(
        b,
        if horiz {
            TextNode::HList
        } else {
            TextNode::VList
        },
    );
    let mut n = -1;
    let mut no_extenders = true;
    let mut min_o = ot_min_connector_overlap(f);
    loop {
        n = n + 1;
        let mut s_max = 0;
        let mut prev_o = 0;
        for i in 0..ot_part_count(a as *const GlyphAssembly) {
            if ot_part_is_extender(a as *const GlyphAssembly, i) {
                no_extenders = false;
                for _ in 0..n {
                    let o = ot_part_start_connector(f, a as *const GlyphAssembly, i)
                        .min(min_o)
                        .min(prev_o);

                    s_max = s_max - o + ot_part_full_advance(f, a as *const GlyphAssembly, i);
                    prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
                }
            } else {
                let o = ot_part_start_connector(f, a as *const GlyphAssembly, i)
                    .min(min_o)
                    .min(prev_o);

                s_max = s_max - o + ot_part_full_advance(f, a as *const GlyphAssembly, i);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
            }
        }
        if s_max >= s || no_extenders {
            break;
        }
    }
    let mut prev_o = 0;
    for i in 0..ot_part_count(a as *const GlyphAssembly) {
        if ot_part_is_extender(a as *const GlyphAssembly, i) {
            for _ in 0..n {
                let o = ot_part_start_connector(f, a as *const GlyphAssembly, i).min(prev_o);

                let oo = o;

                let o = o.min(min_o);

                if oo > 0 {
                    stack_glue_into_box(b, -oo, -o);
                }
                let g = ot_part_glyph(a as *const GlyphAssembly, i);
                stack_glyph_into_box(b, f, g);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
            }
        } else {
            let o = ot_part_start_connector(f, a as *const GlyphAssembly, i).min(prev_o);

            let oo = o;

            let o = o.min(min_o);

            if oo > 0 {
                stack_glue_into_box(b, -oo, -o);
            }
            let g = ot_part_glyph(a as *const GlyphAssembly, i);
            stack_glyph_into_box(b, f, g);
            prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
        }
    }
    let mut popt = BOX_list_ptr(b).opt();
    let mut nat = 0;
    let mut str = 0;
    while let Some(p) = popt {
        if NODE_type(p) == TextNode::WhatsIt.into() {
            nat += if horiz {
                *BOX_width(p)
            } else {
                *BOX_height(p) + *BOX_depth(p)
            };
        } else if NODE_type(p as usize) == TextNode::Glue.into() {
            nat += *GLUE_SPEC_size(*GLUE_NODE_glue_ptr(p) as usize);
            str += *GLUE_SPEC_stretch(*GLUE_NODE_glue_ptr(p) as usize);
        }
        popt = LLIST_link(p).opt();
    }

    if s > nat && str > 0 {
        let o = (s - nat).min(str);

        *BOX_glue_order(b) = GlueOrder::Normal as u16;
        *BOX_glue_sign(b) = GlueSign::Stretching as u16;
        *BOX_glue_set(b) = o as f64 / str as f64;
        if horiz {
            *BOX_width(b) = nat + tex_round(str as f64 * *BOX_glue_set(b));
        } else {
            *BOX_height(b) = nat + tex_round(str as f64 * *BOX_glue_set(b));
        }
    } else if horiz {
        *BOX_width(b) = nat;
    } else {
        *BOX_height(b) = nat;
    }
    b
}
unsafe fn rebox(mut b: usize, mut w: scaled_t) -> usize {
    let mut f: internal_font_number = 0;
    let mut v: scaled_t = 0;
    if *BOX_width(b) != w && BOX_list_ptr(b).opt().is_some() {
        if NODE_type(b) == TextNode::VList.into() {
            b = hpack(Some(b), 0, PackMode::Additional) as usize
        }
        let mut p = *BOX_list_ptr(b) as usize;
        if is_char_node(Some(p)) && LLIST_link(p).opt().is_none() {
            f = *CHAR_NODE_font(p) as internal_font_number;
            v = FONT_INFO[(WIDTH_BASE[f]
                + FONT_INFO
                    [(CHAR_BASE[f] + effective_char(true, f, *CHAR_NODE_character(p))) as usize]
                    .b16
                    .s3 as i32) as usize]
                .b32
                .s1;
            if v != *BOX_width(b) {
                *LLIST_link(p) = new_kern(*BOX_width(b) - v) as i32;
            }
        }
        free_node(b, BOX_NODE_SIZE);
        let b = new_glue(12);
        *LLIST_link(b) = Some(p).tex_int();
        while let Some(next) = LLIST_link(p).opt() {
            p = next;
        }
        *LLIST_link(p) = new_glue(12) as i32;
        hpack(Some(b), w, PackMode::Exactly) as usize
    } else {
        *BOX_width(b) = w;
        b
    }
}
