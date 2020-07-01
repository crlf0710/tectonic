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

use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, Confuse};
use crate::xetex_ext::{
    map_char_to_glyph, measure_native_glyph, real_get_native_glyph, AAT_FONT_FLAG, OTGR_FONT_FLAG,
};
use crate::xetex_ini::{
    adjust_tail, avail, cur_c, cur_chr, cur_cmd, cur_dir, cur_f, cur_group, cur_i, cur_lang,
    cur_list, cur_val, cur_val1, empty, file_line_error_style_p, help_line, help_ptr,
    insert_src_special_every_math, just_box, pre_adjust_tail, temp_ptr, tex_remainder,
    total_shrink, xtx_ligature_present, LR_problems, LR_ptr, CHAR_BASE, DEPTH_BASE, EQTB,
    EXTEN_BASE, FONT_AREA, FONT_BC, FONT_EC, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_PARAMS,
    HEIGHT_BASE, ITALIC_BASE, KERN_BASE, LIG_KERN_BASE, MEM, NEST_PTR, NULL_CHARACTER, PARAM_BASE,
    SAVE_PTR, SAVE_STACK, SKEW_CHAR, WIDTH_BASE,
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
    is_char_node, set_NODE_type, set_kern_NODE_subtype, set_whatsit_NODE_subtype,
    whatsit_NODE_subtype, BOX_glue_order, BOX_glue_set, BOX_glue_sign, BOX_list_ptr,
    BOX_shift_amount, BOX_width, CHAR_NODE_font, GLUE_NODE_glue_ptr, GLUE_SPEC_shrink,
    GLUE_SPEC_shrink_order, GLUE_SPEC_stretch, GLUE_SPEC_stretch_order, GLUE_SPEC_width,
    LLIST_link, NODE_type, TeXInt, TeXOpt,
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
static mut cur_style: i16 = 0;
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
    let mut current_block: u64;
    let mut x: i32 = 0;
    let mut l: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut f: internal_font_number = 0;
    let mut n: i32 = 0;
    let mut v: scaled_t = 0;
    let mut d: scaled_t = 0;

    get_token();

    if cur_cmd == Cmd::MathShift && cur_list.mode.0 == false {
        // 1180:
        let mut j = None;
        let mut w = -MAX_HALFWORD;
        if cur_list.head == cur_list.tail {
            // 1520:
            pop_nest();
            x = if let Some(aux) = cur_list.eTeX_aux {
                if MEM[aux].b32.s0 >= R_CODE as i32 {
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

            MEM[p].b32.s1 = j.tex_int();

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
                if MEM[aux].b32.s0 >= R_CODE as i32 {
                    -1
                } else {
                    1 // :1519
                }
            } else {
                0
            };
            let mut popt = if x >= 0 {
                let p = BOX_list_ptr(just_box).opt();
                MEM[TEMP_HEAD].b32.s1 = None.tex_int();
                p
            } else {
                v = -v - *BOX_width(just_box);
                let p = new_math(0, BEGIN_L_CODE as i16);
                MEM[TEMP_HEAD].b32.s1 = p as i32;
                just_copy(
                    BOX_list_ptr(just_box).opt(),
                    p,
                    new_math(0, END_L_CODE as i16) as i32,
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
                temp_ptr = get_avail(); /*1523:*/
                MEM[temp_ptr].b32.s0 = BEFORE as i32; /*:1398 */
                MEM[temp_ptr].b32.s1 = LR_ptr;
                LR_ptr = Some(temp_ptr).tex_int();
            }
            while let Some(mut p) = popt {
                loop {
                    if is_char_node(Some(p)) {
                        f = *CHAR_NODE_font(p) as internal_font_number;
                        d = FONT_INFO[(WIDTH_BASE[f]
                            + FONT_INFO
                                [(CHAR_BASE[f] + effective_char(true, f, MEM[p].b16.s0)) as usize]
                                .b16
                                .s3 as i32) as usize]
                            .b32
                            .s1;
                        current_block = 9427725525305667067;
                        break;
                    } else {
                        match TextNode::n(MEM[p].b16.s1).unwrap() {
                            TextNode::HList | TextNode::VList | TextNode::Rule => {
                                d = MEM[p + 1].b32.s1;
                                current_block = 9427725525305667067;
                                break;
                            }
                            TextNode::Ligature => {
                                MEM[GARBAGE] = MEM[p + 1];
                                MEM[GARBAGE].b32.s1 = MEM[p].b32.s1;
                                p = GARBAGE;
                                xtx_ligature_present = true
                            }
                            TextNode::Kern => {
                                d = MEM[p + 1].b32.s1;
                                current_block = 1677945370889843322;
                                break;
                            }
                            TextNode::MarginKern => {
                                d = MEM[p + 1].b32.s1;
                                current_block = 1677945370889843322;
                                break;
                            }
                            TextNode::Math => {
                                d = MEM[p + 1].b32.s1;
                                if *INTPAR(IntPar::texxet) > 0i32 {
                                    current_block = 13660591889533726445;
                                    break;
                                } else {
                                    current_block = 2631791190359682872;
                                    break;
                                }
                            }
                            TextNode::Style => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                cur_dir = LR::n(MEM[p as usize].b16.s0).unwrap();
                                current_block = 1677945370889843322;
                                break;
                            }
                            TextNode::Glue => {
                                let q = *GLUE_NODE_glue_ptr(p) as usize;
                                d = *GLUE_SPEC_width(q);
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
                                    current_block = 9427725525305667067;
                                    break;
                                } else {
                                    current_block = 1677945370889843322;
                                    break;
                                }
                            }
                            TextNode::WhatsIt => match whatsit_NODE_subtype(p) {
                                WhatsItNST::NativeWord
                                | WhatsItNST::NativeWordAt
                                | WhatsItNST::Glyph
                                | WhatsItNST::Pic
                                | WhatsItNST::Pdf => {
                                    current_block = 11064061988481400464;
                                    break;
                                }
                                _ => {
                                    current_block = 5846959088466685742;
                                    break;
                                }
                            },
                            _ => {
                                d = 0i32;
                                current_block = 1677945370889843322;
                                break;
                            }
                        }
                    }
                }
                match current_block {
                    2631791190359682872 => {
                        if MEM[p].b16.s0 as i32 >= 4 {
                            w = MAX_HALFWORD;
                            break;
                        } else {
                            current_block = 1677945370889843322;
                        }
                    }
                    13660591889533726445 =>
                    /*1525: */
                    {
                        if MEM[p].b16.s0 as i32 & 1 != 0 {
                            if MEM[LR_ptr as usize].b32.s0 == 4i32 * (MEM[p].b16.s0 as i32 / 4) + 3
                            {
                                temp_ptr = LR_ptr as usize;
                                LR_ptr = MEM[temp_ptr].b32.s1;
                                MEM[temp_ptr].b32.s1 = avail.tex_int();
                                avail = Some(temp_ptr);
                            } else if MEM[p].b16.s0 as i32 > 4 {
                                w = MAX_HALFWORD;
                                break;
                            }
                        } else {
                            temp_ptr = get_avail();
                            MEM[temp_ptr].b32.s0 = 4i32 * (MEM[p].b16.s0 as i32 / 4) + 3;
                            MEM[temp_ptr].b32.s1 = LR_ptr;
                            LR_ptr = Some(temp_ptr).tex_int();
                            if MEM[p].b16.s0 as i32 / 8 != cur_dir as i32 {
                                just_reverse(p);
                                p = TEMP_HEAD;
                            }
                        }
                        current_block = 1677945370889843322;
                    }
                    5846959088466685742 => {
                        d = 0i32;
                        current_block = 1677945370889843322;
                    }
                    11064061988481400464 => {
                        d = MEM[(p + 1) as usize].b32.s1;
                        current_block = 9427725525305667067;
                    }
                    _ => {}
                }
                match current_block {
                    1677945370889843322 => {
                        if v < MAX_HALFWORD {
                            v = v + d
                        }
                    }
                    _ => {
                        if v < MAX_HALFWORD {
                            v = v + d;
                            w = v
                        } else {
                            w = MAX_HALFWORD;
                            break;
                        }
                    }
                }
                popt = LLIST_link(p).opt();
            }
            if *INTPAR(IntPar::texxet) > 0 {
                while let Some(l) = LR_ptr.opt() {
                    temp_ptr = l;
                    LR_ptr = MEM[temp_ptr].b32.s1;
                    MEM[temp_ptr].b32.s1 = avail.tex_int();
                    avail = Some(temp_ptr);
                }
                if LR_problems != 0 {
                    w = MAX_HALFWORD;
                    LR_problems = 0
                }
            }
            cur_dir = LR::LeftToRight;
            flush_node_list(MEM[TEMP_HEAD].b32.s1.opt());
        }
        if let Some(ps) = LOCAL(Local::par_shape).opt() {
            n = MEM[ps].b32.s0;
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
    help_ptr = 1_u8;
    help_line[0] = b"I\'m ignoring this misplaced \\limits or \\nolimits command.";
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
        help_ptr = 6;
        help_line[5] = b"I was expecting to see something like `(\' or `\\{\' or";
        help_line[4] = b"`\\}\' here. If you typed, e.g., `{\' instead of `\\{\', you";
        help_line[3] = b"should probably delete the `{\' by typing `1\' now, so that";
        help_line[2] = b"braces don\'t get unbalanced. Otherwise just proceed.";
        help_line[1] = b"Acceptable delimiters are characters whose \\delcode is";
        help_line[0] = b"nonnegative, or you can use `\\delimiter <delimiter code>\'.";
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
    MEM[cur_list.tail].b32.s1 = get_node(RADICAL_NOAD_SIZE) as i32;
    cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    MEM[cur_list.tail].b16.s1 = MathNode::Radical as u16;
    MEM[cur_list.tail].b16.s0 = NORMAL as u16;
    MEM[cur_list.tail + 1].b32 = empty;
    MEM[cur_list.tail + 3].b32 = empty;
    MEM[cur_list.tail + 2].b32 = empty;
    scan_delimiter(cur_list.tail + 4, true);
    scan_math(cur_list.tail + 1);
}
pub(crate) unsafe fn math_ac() {
    let mut c: i32 = 0;
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
        help_ptr = 2_u8;
        help_line[1] = b"I\'m changing \\accent to \\mathaccent here; wish me luck.";
        help_line[0] = b"(Accents are not the same in formulas as they are in text.)";
        error();
    }
    MEM[cur_list.tail].b32.s1 = get_node(ACCENT_NOAD_SIZE) as i32;
    cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    MEM[cur_list.tail].b16.s1 = MathNode::Accent as u16;
    MEM[cur_list.tail].b16.s0 = NORMAL as u16;
    MEM[cur_list.tail + 1].b32 = empty;
    MEM[cur_list.tail + 3].b32 = empty;
    MEM[cur_list.tail + 2].b32 = empty;
    MEM[cur_list.tail + 4].b32.s1 = MATH_CHAR;
    if cur_chr == 1i32 {
        if scan_keyword(b"fixed") {
            MEM[cur_list.tail].b16.s0 = FIXED_ACC as u16;
        } else if scan_keyword(b"bottom") {
            if scan_keyword(b"fixed") {
                MEM[cur_list.tail].b16.s0 = (BOTTOM_ACC + 1) as u16;
            } else {
                MEM[cur_list.tail].b16.s0 = BOTTOM_ACC as u16;
            }
        }
        scan_math_class_int();
        c = ((cur_val as u32 & 0x7_u32) << 21i32) as i32;
        scan_math_fam_int();
        c = (c as u32).wrapping_add((cur_val as u32 & 0xff_u32) << 24i32) as i32;
        scan_usv_num();
        cur_val = cur_val + c
    } else {
        scan_fifteen_bit_int();
        cur_val = (((cur_val / 4096i32) as u32 & 0x7_u32) << 21i32)
            .wrapping_add(((cur_val % 4096i32 / 256i32) as u32 & 0xff_u32) << 24i32)
            .wrapping_add((cur_val % 256i32) as u32) as i32
    }
    MEM[cur_list.tail + 4].b16.s0 = (cur_val as i64 % 65536) as u16;
    if cur_val as u32 >> 21i32 & 0x7_u32 == 7_u32
        && (*INTPAR(IntPar::cur_fam) >= 0 && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32)
    {
        MEM[cur_list.tail + 4].b16.s1 = *INTPAR(IntPar::cur_fam) as u16
    } else {
        MEM[cur_list.tail + 4].b16.s1 = (cur_val as u32 >> 24 & 0xff_u32) as u16
    }
    MEM[cur_list.tail + 4].b16.s1 = (MEM[cur_list.tail + 4].b16.s1 as i64
        + (cur_val as u32 & 0x1fffff_u32) as i64 / 65536 * 256i32 as i64)
        as u16;
    scan_math((cur_list.tail + 1) as usize);
}
pub(crate) unsafe fn append_choices() {
    MEM[cur_list.tail].b32.s1 = new_choice() as i32;
    cur_list.tail = *LLIST_link(cur_list.tail) as usize;
    SAVE_PTR += 1;
    SAVE_STACK[SAVE_PTR - 1].val = 0;
    push_math(GroupCode::MathChoice);
    scan_left_brace();
}
pub(crate) unsafe fn fin_mlist(p: Option<usize>) -> i32 {
    let q = if let Some(a) = cur_list.aux.b32.s1.opt() {
        /*1220: */
        MEM[a + 3].b32.s1 = SUB_MLIST;
        MEM[a + 3].b32.s0 = MEM[cur_list.head].b32.s1;
        let q;
        if let Some(p) = p {
            q = MEM[a + 2].b32.s0 as usize;
            if MEM[q].b16.s1 != 30 || cur_list.eTeX_aux.is_none() {
                confusion(b"right");
            }
            if let Some(aux) = cur_list.eTeX_aux {
                MEM[a + 2].b32.s0 = MEM[aux].b32.s1;
                MEM[aux].b32.s1 = Some(a).tex_int();
                MEM[a].b32.s1 = Some(p).tex_int();
            }
        } else {
            q = a;
        }
        q as i32
    } else {
        MEM[cur_list.tail].b32.s1 = p.tex_int();
        MEM[cur_list.head].b32.s1
    };
    pop_nest();
    q
}
pub(crate) unsafe fn build_choices() {
    let mut p: i32 = 0;
    unsave();
    p = fin_mlist(None);
    match SAVE_STACK[SAVE_PTR - 1].val {
        0 => MEM[cur_list.tail + 1].b32.s0 = p,
        1 => MEM[cur_list.tail + 1].b32.s1 = p,
        2 => MEM[cur_list.tail + 2].b32.s0 = p,
        3 => {
            MEM[cur_list.tail + 2].b32.s1 = p;
            SAVE_PTR -= 1;
            return;
        }
        _ => {}
    }
    SAVE_STACK[SAVE_PTR - 1].val += 1;
    push_math(GroupCode::MathChoice);
    scan_left_brace();
}
pub(crate) unsafe fn sub_sup() {
    let mut t = EMPTY as i16;
    let mut p = None;
    if cur_list.tail != cur_list.head {
        if MEM[cur_list.tail].b16.s1 >= MathNode::Ord as u16
            && MEM[cur_list.tail].b16.s1 < MathNode::Left as u16
        {
            let g = cur_list.tail + 2 + cur_cmd as usize - 7;
            t = MEM[g as usize].b32.s1 as i16;
            p = Some(g);
        }
    }
    let p = match (p, t as i32) {
        (Some(p), EMPTY) => p,
        _ => {
            /*1212: */
            MEM[cur_list.tail].b32.s1 = new_noad() as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
            let p = cur_list.tail + 2 + cur_cmd as usize - 7;
            if t as i32 != EMPTY {
                if cur_cmd == Cmd::SupMark {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Double superscript");
                    help_ptr = 1;
                    help_line[0] = b"I treat `x^1^2\' essentially like `x^1{}^2\'."
                } else {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Double subscript");
                    help_ptr = 1_u8;
                    help_line[0] = b"I treat `x_1_2\' essentially like `x_1{}_2\'."
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
        help_ptr = 3_u8;
        help_line[2] = b"I\'m ignoring this fraction specification, since I don\'t";
        help_line[1] = b"know whether a construction like `x \\over y \\over z\'";
        help_line[0] = b"means `{x \\over y} \\over z\' or `x \\over {y \\over z}\'.";
        error();
    } else {
        let a = get_node(FRACTION_NOAD_SIZE);
        cur_list.aux.b32.s1 = Some(a).tex_int();
        MEM[a].b16.s1 = MathNode::Fraction as u16;
        MEM[a].b16.s0 = NORMAL as u16;
        MEM[a + 2].b32.s1 = SUB_MLIST;
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
                help_ptr = 1_u8;
                help_line[0] = b"I\'m ignoring a \\middle that had no matching \\left."
            } else {
                print_esc_cstr(b"right");
                help_ptr = 1_u8;
                help_line[0] = b"I\'m ignoring a \\right that had no matching \\left."
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
            MEM[cur_list.tail + 1].b32.s1 = SUB_MLIST;
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
    if x == 0i32 {
        MEM[b + 4].b32.s1 = s + d
    } else {
        let mut q;
        z = *DIMENPAR(DimenPar::display_width);
        let mut p = b;
        if x > 0i32 {
            e = z - d - MEM[p + 1].b32.s1
        } else {
            e = d;
            d = z - e - MEM[p + 1].b32.s1
        }
        if let Some(j) = j {
            b = copy_node_list(Some(j)) as usize;
            MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1;
            MEM[b + 2].b32.s1 = MEM[p + 2].b32.s1;
            s = s - MEM[b + 4].b32.s1;
            d = d + s;
            e = e + MEM[b + 1].b32.s1 - z - s
        }
        if MEM[p].b16.s0 == LRMode::DList as u16 {
            q = p;
        } else {
            let r = MEM[p + 5].b32.s1.opt();
            free_node(p, BOX_NODE_SIZE);
            let mut r = r.confuse(b"LR4");
            if x > 0i32 {
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
                    let t = MEM[r as usize].b32.s1.opt();
                    MEM[r].b32.s1 = popt.tex_int();
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
            r = MEM[(b + 5) as usize].b32.s1 as usize;
            t = MEM[r].b32.s1 as usize;
        }
        let u = new_math(0i32, END_M_CODE as i16);
        let j = if NODE_type(t) == TextNode::Glue.into() {
            let j = new_skip_param(GluePar::right_skip);
            MEM[q as usize].b32.s1 = Some(j).tex_int();
            MEM[j].b32.s1 = Some(u).tex_int();
            let j = *GLUE_NODE_glue_ptr(t) as usize;
            *GLUE_SPEC_stretch_order(temp_ptr) = *GLUE_SPEC_stretch_order(j);
            *GLUE_SPEC_shrink_order(temp_ptr) = *GLUE_SPEC_shrink_order(j);
            *GLUE_SPEC_width(temp_ptr) = e - *GLUE_SPEC_width(j);
            *GLUE_SPEC_stretch(temp_ptr) = -(*GLUE_SPEC_stretch(j));
            *GLUE_SPEC_shrink(temp_ptr) = -(*GLUE_SPEC_shrink(j));
            MEM[u].b32.s1 = t as i32;
            Some(j)
        } else {
            MEM[t + 1].b32.s1 = e;
            MEM[t].b32.s1 = Some(u).tex_int();
            MEM[q as usize].b32.s1 = t as i32;
            j
        };
        let u = new_math(0, BEGIN_M_CODE as i16);
        if NODE_type(r) == TextNode::Glue.into() {
            let j = new_skip_param(GluePar::left_skip);
            MEM[u].b32.s1 = Some(j).tex_int();
            MEM[j].b32.s1 = Some(p).tex_int();
            let j = *GLUE_NODE_glue_ptr(r) as usize;
            *GLUE_SPEC_stretch_order(temp_ptr) = *GLUE_SPEC_stretch_order(j);
            *GLUE_SPEC_shrink_order(temp_ptr) = *GLUE_SPEC_shrink_order(j);
            *GLUE_SPEC_width(temp_ptr) = d - *GLUE_SPEC_width(j);
            *GLUE_SPEC_stretch(temp_ptr) = -(*GLUE_SPEC_stretch(j));
            *GLUE_SPEC_shrink(temp_ptr) = -(*GLUE_SPEC_shrink(j));
            MEM[r].b32.s1 = Some(u).tex_int();
        } else {
            MEM[r + 1].b32.s1 = d;
            MEM[r].b32.s1 = Some(p).tex_int();
            MEM[u].b32.s1 = Some(r).tex_int();
            if j.is_none() {
                b = hpack(u as i32, 0, PackMode::Additional);
                MEM[b + 4].b32.s1 = s;
            } else {
                MEM[b + 5].b32.s1 = u as i32;
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

        help_ptr = 3_u8;
        help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 2";
        help_line[1] = b"and \\scriptfont 2 and \\scriptscriptfont 2 have all";
        help_line[0] = b"the \\fontdimen values needed in math symbol fonts.";
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

        help_ptr = 3;
        help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 3";
        help_line[1] = b"and \\scriptfont 3 and \\scriptscriptfont 3 have all";
        help_line[0] = b"the \\fontdimen values needed in math extension fonts.";
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

            help_ptr = 2;
            help_line[1] = b"The `$\' that I just saw supposedly matches a previous `$$\'.";
            help_line[0] = b"So I shall assume that you typed `$$\' both times.";
            back_error();
        }
        cur_mlist = p;
        cur_style = TEXT_STYLE as i16;
        mlist_penalties = false;
        mlist_to_hlist();
        let a = hpack(MEM[TEMP_HEAD].b32.s1, 0, PackMode::Additional);
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

            help_ptr = 3;
            help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 2";
            help_line[1] = b"and \\scriptfont 2 and \\scriptscriptfont 2 have all";
            help_line[0] = b"the \\fontdimen values needed in math symbol fonts.";
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

            help_ptr = 3;
            help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 3";
            help_line[1] = b"and \\scriptfont 3 and \\scriptscriptfont 3 have all";
            help_line[0] = b"the \\fontdimen values needed in math extension fonts.";
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
            new_math(*DIMENPAR(DimenPar::math_surround), BEFORE as i16) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        cur_mlist = p;
        cur_style = TEXT_STYLE as i16;
        mlist_penalties = cur_list.mode.0 == false;
        mlist_to_hlist();
        MEM[cur_list.tail].b32.s1 = MEM[TEMP_HEAD].b32.s1;
        while let Some(next) = LLIST_link(cur_list.tail).opt() {
            cur_list.tail = next;
        }
        MEM[cur_list.tail].b32.s1 =
            new_math(*DIMENPAR(DimenPar::math_surround), AFTER as i16) as i32;
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

                help_ptr = 2_u8;
                help_line[1] = b"The `$\' that I just saw supposedly matches a previous `$$\'.";
                help_line[0] = b"So I shall assume that you typed `$$\' both times.";
                back_error();
            }
        }
        cur_mlist = p;
        cur_style = DISPLAY_STYLE as i16;
        mlist_penalties = false;
        mlist_to_hlist();
        p = MEM[TEMP_HEAD].b32.s1;
        adjust_tail = Some(ADJUST_HEAD);
        pre_adjust_tail = Some(PRE_ADJUST_HEAD);
        let mut b = hpack(p, 0, PackMode::Additional);
        p = MEM[b + 5].b32.s1;
        let t = adjust_tail.unwrap();
        adjust_tail = None;
        let pre_t = pre_adjust_tail.unwrap();
        pre_adjust_tail = None;
        w = MEM[b + 1].b32.s1;
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
            w = MEM[(b + 1) as usize].b32.s1
        }
        MEM[b].b16.s0 = LRMode::DList as u16;
        d = half(z - w);
        if e > 0 && d < 2 * e {
            d = half(z - w - e);
            if let Some(p) = p.opt() {
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
            MEM[cur_list.tail].b32.s1 = new_penalty(INF_PENALTY) as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        } else {
            MEM[cur_list.tail].b32.s1 = new_param_glue(g1) as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        }
        if e != 0i32 {
            let a = a.unwrap();
            let r = new_kern(z - w - e - d);
            if l {
                MEM[a].b32.s1 = r as i32;
                MEM[r].b32.s1 = b as i32;
                b = a;
                d = 0;
            } else {
                MEM[b].b32.s1 = r as i32;
                MEM[r].b32.s1 = Some(a).tex_int();
            }
            b = hpack(b as i32, 0, PackMode::Additional);
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
            MEM[cur_list.tail].b32.s1 = MEM[ADJUST_HEAD as usize].b32.s1;
            cur_list.tail = t;
        }
        if pre_t != PRE_ADJUST_HEAD {
            MEM[cur_list.tail].b32.s1 = MEM[PRE_ADJUST_HEAD as usize].b32.s1;
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
unsafe fn overbar(mut b: i32, mut k: scaled_t, mut t: scaled_t) -> usize {
    let p = new_kern(k);
    MEM[p].b32.s1 = b;
    let q = fraction_rule(t);
    MEM[q].b32.s1 = p as i32;
    let p = new_kern(t);
    MEM[p].b32.s1 = q as i32;
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
    MEM[cur_list.head].b32.s1 = None.tex_int();
    cur_list.tail = cur_list.head;
    cur_list.aux.b32.s1 = None.tex_int();
}
unsafe fn clean_box(p: usize, mut s: i16) -> usize {
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
    let q = MEM[TEMP_HEAD].b32.s1;
    cur_style = save_style;
    if (cur_style as i32) < SCRIPT_STYLE {
        cur_size = TEXT_SIZE;
    } else {
        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18);

    unsafe fn found(q: i32) -> usize {
        let x = if is_char_node(q.opt()) || q.is_texnull() {
            hpack(q, 0, PackMode::Additional)
        } else if MEM[q as usize].b32.s1.is_texnull()
            && [TextNode::HList.into(), TextNode::VList.into()].contains(&NODE_type(q as usize))
            && MEM[(q + 4) as usize].b32.s1 == 0
        {
            q as usize
        } else {
            hpack(q, 0, PackMode::Additional)
        };
        let q = MEM[x + 5].b32.s1;
        if is_char_node(q.opt()) {
            if let Some(r) = MEM[q as usize].b32.s1.opt() {
                if MEM[r].b32.s1.opt().is_none() {
                    if !is_char_node(Some(r)) {
                        if NODE_type(r) == TextNode::Kern.into() {
                            free_node(r, MEDIUM_NODE_SIZE);
                            MEM[q as usize].b32.s1 = None.tex_int()
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

        help_ptr = 4_u8;
        help_line[3] = b"Somewhere in the math formula just ended, you used the";
        help_line[2] = b"stated character from an undefined font family. For example,";
        help_line[1] = b"plain TeX doesn\'t allow \\it or \\sl in subscripts. Proceed,";
        help_line[0] = b"and I\'ll try to forget that I needed that character.";
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
            MEM[a].b32.s1 = EMPTY;
        }
    };
}
unsafe fn make_over(q: usize) {
    MEM[q + 1].b32.s0 = overbar(
        clean_box(q + 1, (2 * (cur_style as i32 / 2) + 1) as i16) as i32,
        3 * default_rule_thickness(),
        default_rule_thickness(),
    ) as i32;
    MEM[(q + 1) as usize].b32.s1 = SUB_BOX;
}
unsafe fn make_under(q: usize) {
    let x = clean_box(q + 1, cur_style);
    let p = new_kern(3 * default_rule_thickness());
    MEM[x].b32.s1 = p as i32;
    MEM[p].b32.s1 = fraction_rule(default_rule_thickness()) as i32;
    let y = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD);
    let delta = MEM[y + 3].b32.s1 + MEM[y + 2].b32.s1 + default_rule_thickness();
    MEM[y + 3].b32.s1 = MEM[x + 3].b32.s1;
    MEM[y + 2].b32.s1 = delta - MEM[y + 3].b32.s1;
    MEM[q + 1].b32.s0 = y as i32;
    MEM[q + 1].b32.s1 = SUB_BOX;
}
unsafe fn make_vcenter(q: usize) {
    let v = MEM[q + 1].b32.s0;
    if NODE_type(v as usize) != TextNode::VList.into() {
        confusion(b"vcenter");
    }
    let delta = MEM[(v + 3) as usize].b32.s1 + MEM[(v + 2) as usize].b32.s1;
    MEM[(v + 3) as usize].b32.s1 = axis_height(cur_size) + half(delta);
    MEM[(v + 2) as usize].b32.s1 = delta - MEM[(v + 3) as usize].b32.s1;
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
    let x = clean_box(q + 1, (2 * (cur_style as i32 / 2) + 1) as i16);
    let mut clr = if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        if (cur_style as i32) < TEXT_STYLE {
            get_ot_math_constant(f, RADICALDISPLAYSTYLEVERTICALGAP)
        } else {
            get_ot_math_constant(f, RADICALVERTICALGAP)
        }
    } else if (cur_style as i32) < TEXT_STYLE {
        rule_thickness + math_x_height(cur_size).abs() / 4
    } else {
        let clr = rule_thickness;
        clr + clr.abs() / 4
    };
    let y = var_delimiter(
        q + 4,
        cur_size,
        MEM[x + 3].b32.s1 + MEM[x + 2].b32.s1 + clr + rule_thickness,
    );
    if FONT_AREA[f] as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f] as XeTeXLayoutEngine)
    {
        MEM[y + 2].b32.s1 = MEM[y + 3].b32.s1 + MEM[y + 2].b32.s1 - rule_thickness;
        MEM[y + 3].b32.s1 = rule_thickness
    }
    let delta = MEM[y + 2].b32.s1 - (MEM[x + 3].b32.s1 + MEM[x + 2].b32.s1 + clr);
    if delta > 0 {
        clr = clr + half(delta)
    }
    MEM[y + 4].b32.s1 = -((MEM[x + 3].b32.s1 + clr) as i32);
    MEM[y].b32.s1 = overbar(x as i32, clr, MEM[y + 3].b32.s1) as i32;
    MEM[q + 1].b32.s0 = hpack(y as i32, 0, PackMode::Additional) as i32;
    MEM[q + 1].b32.s1 = SUB_BOX;
}
unsafe fn compute_ot_math_accent_pos(p: usize) -> scaled_t {
    if MEM[p + 1].b32.s1 == MATH_CHAR {
        fetch(p + 1);
        let q = new_native_character(cur_f, cur_c);
        let g = real_get_native_glyph(
            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
            0_u32,
        ) as scaled_t;
        get_ot_math_accent_pos(cur_f, g)
    } else if MEM[p + 1].b32.s1 == SUB_MLIST {
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
        s = if !(MEM[q].b16.s0 == BOTTOM_ACC || MEM[q as usize].b16.s0 == BOTTOM_ACC + 1) {
            compute_ot_math_accent_pos(q)
        } else {
            0
        };
        let x = clean_box(q + 1, (2 * (cur_style as i32 / 2) + 1) as i16);
        w = MEM[x + 1].b32.s1;
        h = MEM[x + 3].b32.s1;
        Some(x)
    } else if cur_i.s3 as i32 > 0 {
        let mut i = cur_i;
        c = cur_c;
        f = cur_f;
        s = 0;
        if MEM[q + 1].b32.s1 == MATH_CHAR {
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
        let x = clean_box(q + 1, (2 * (cur_style as i32 / 2) + 1) as i16);
        w = MEM[x + 1].b32.s1;
        h = MEM[x + 3].b32.s1;
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
            if MEM[q].b16.s0 == BOTTOM_ACC || MEM[q].b16.s0 == BOTTOM_ACC + 1 {
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
        if MEM[q + 2].b32.s1 != EMPTY || MEM[(q + 3) as usize].b32.s1 != EMPTY {
            if MEM[q + 1].b32.s1 == MATH_CHAR {
                // 769:
                flush_node_list(Some(x));
                x = new_noad();
                MEM[x + 1] = MEM[q + 1];
                MEM[x + 2] = MEM[q + 2];
                MEM[x + 3] = MEM[q + 3];
                MEM[q + 2].b32 = empty;
                MEM[q + 3].b32 = empty;
                MEM[q + 1].b32.s1 = SUB_MLIST;
                MEM[q + 1].b32.s0 = Some(x).tex_int();
                x = clean_box(q + 1, cur_style);
                delta = delta + MEM[x + 3].b32.s1 - h;
                h = MEM[x + 3].b32.s1
            }
        }
        let mut y = char_box(f, c);
        if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
            let mut p = get_node(GLYPH_NODE_SIZE);
            set_NODE_type(p, TextNode::WhatsIt);
            set_whatsit_NODE_subtype(p, WhatsItNST::Glyph);
            MEM[p + 4].b16.s2 = f as u16;
            MEM[p + 4].b16.s1 = real_get_native_glyph(
                &mut MEM[MEM[y + 5].b32.s1 as usize] as *mut memory_word as *mut libc::c_void,
                0,
            );
            measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1i32);
            free_node(
                MEM[y + 5].b32.s1 as usize,
                MEM[(MEM[y + 5].b32.s1 + 4) as usize].b16.s3 as i32,
            );
            MEM[y + 5].b32.s1 = Some(p).tex_int();
            if MEM[q].b16.s0 as i32 & 1 != 0 {
                measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1);
            } else {
                c = MEM[p + 4].b16.s1 as i32;
                a = 0i32;
                loop {
                    g = get_ot_math_variant(f, c, a, &mut w2, 1);
                    if w2 > 0i32 && w2 <= w {
                        MEM[p + 4].b16.s1 = g as u16;
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
                        MEM[y + 5].b32.s1 = Some(p).tex_int();
                    }
                } else {
                    measure_native_glyph(&mut MEM[p] as *mut memory_word as *mut libc::c_void, 1);
                }
            }
            MEM[y + 1].b32.s1 = MEM[p + 1].b32.s1;
            MEM[y + 3].b32.s1 = MEM[p + 3].b32.s1;
            MEM[y + 2].b32.s1 = MEM[p + 2].b32.s1;
            if MEM[q].b16.s0 == BOTTOM_ACC || MEM[q].b16.s0 == BOTTOM_ACC + 1 {
                if MEM[y + 3].b32.s1 < 0 {
                    MEM[y + 3].b32.s1 = 0
                }
            } else if MEM[y + 2].b32.s1 < 0 {
                MEM[y + 2].b32.s1 = 0
            }
            let mut sa;
            if !is_char_node(Some(p))
                && NODE_type(p) == TextNode::WhatsIt.into()
                && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
            {
                sa = get_ot_math_accent_pos(f, MEM[p + 4].b16.s1 as i32);
                if sa == TEX_INFINITY {
                    sa = half(MEM[y + 1].b32.s1)
                }
            } else {
                sa = half(MEM[y + 1].b32.s1)
            }
            if MEM[q].b16.s0 == BOTTOM_ACC || MEM[q].b16.s0 == BOTTOM_ACC + 1 || s == TEX_INFINITY {
                s = half(w)
            }
            MEM[y + 4].b32.s1 = s - sa
        } else {
            MEM[y + 4].b32.s1 = s + half(w - MEM[y + 1].b32.s1)
        }
        MEM[y + 1].b32.s1 = 0;
        if MEM[q].b16.s0 == BOTTOM_ACC || MEM[q].b16.s0 == BOTTOM_ACC + 1 {
            MEM[x].b32.s1 = y as i32;
            y = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD);
            MEM[y + 4].b32.s1 = -((h - MEM[y + 3].b32.s1) as i32)
        } else {
            let p = new_kern(-(delta as i32));
            MEM[p].b32.s1 = Some(x).tex_int();
            MEM[y].b32.s1 = p as i32;
            y = vpackage(Some(y), 0, PackMode::Additional, MAX_HALFWORD);
            if MEM[y + 3].b32.s1 < h {
                // 765:
                let p = new_kern(h - MEM[y + 3].b32.s1); /*773:*/
                MEM[p].b32.s1 = MEM[y + 5].b32.s1;
                MEM[y + 5].b32.s1 = p as i32;
                MEM[y + 3].b32.s1 = h
            }
        }
        MEM[y + 1].b32.s1 = MEM[x + 1].b32.s1;
        MEM[q + 1].b32.s0 = y as i32;
        MEM[q + 1].b32.s1 = SUB_BOX;
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
}
unsafe fn make_fraction(q: usize) {
    let mut delta: scaled_t = 0;
    let mut delta1: scaled_t = 0;
    let mut delta2: scaled_t = 0;
    let mut shift_up: scaled_t = 0;
    let mut shift_down: scaled_t = 0;
    let mut clr: scaled_t = 0;
    if MEM[q + 1].b32.s1 == DEFAULT_CODE {
        MEM[q + 1].b32.s1 = default_rule_thickness()
    }
    let mut x = clean_box(
        q + 2,
        (cur_style as i32 + 2 - 2 * (cur_style as i32 / 6)) as i16,
    );
    let mut z = clean_box(
        q + 3,
        (2 * (cur_style as i32 / 2) + 3 - 2 * (cur_style as i32 / 6)) as i16,
    );
    if MEM[x + 1].b32.s1 < MEM[z + 1].b32.s1 {
        x = rebox(x, MEM[z + 1].b32.s1)
    } else {
        z = rebox(z, MEM[x + 1].b32.s1)
    }
    if (cur_style as i32) < TEXT_STYLE {
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
    if MEM[q + 1].b32.s1 == 0 {
        /*772:*/
        clr = if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            if (cur_style as i32) < TEXT_STYLE {
                get_ot_math_constant(cur_f, STACKDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, STACKGAPMIN)
            }
        } else if (cur_style as i32) < TEXT_STYLE {
            7 * default_rule_thickness()
        } else {
            3 * default_rule_thickness()
        };
        delta = half(clr - (shift_up - MEM[x + 2].b32.s1 - (MEM[z + 3].b32.s1 - shift_down)));
        if delta > 0i32 {
            shift_up = shift_up + delta;
            shift_down = shift_down + delta
        }
    } else {
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            delta = half(MEM[q + 1].b32.s1);
            clr = if (cur_style as i32) < TEXT_STYLE {
                get_ot_math_constant(cur_f, FRACTIONNUMDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, FRACTIONNUMERATORGAPMIN)
            };
            delta1 =
                clr - (shift_up - MEM[(x + 2) as usize].b32.s1 - (axis_height(cur_size) + delta));
            clr = if (cur_style as i32) < TEXT_STYLE {
                get_ot_math_constant(cur_f, FRACTIONDENOMDISPLAYSTYLEGAPMIN)
            } else {
                get_ot_math_constant(cur_f, FRACTIONDENOMINATORGAPMIN)
            };
            delta2 =
                clr - (axis_height(cur_size) - delta - (MEM[(z + 3) as usize].b32.s1 - shift_down))
        } else {
            if (cur_style as i32) < TEXT_STYLE {
                clr = 3 * MEM[q + 1].b32.s1
            } else {
                clr = MEM[q + 1].b32.s1
            }
            delta = half(MEM[q + 1].b32.s1);
            delta1 =
                clr - (shift_up - MEM[(x + 2) as usize].b32.s1 - (axis_height(cur_size) + delta));
            delta2 =
                clr - (axis_height(cur_size) - delta - (MEM[(z + 3) as usize].b32.s1 - shift_down))
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
    MEM[v + 3].b32.s1 = shift_up + MEM[x + 3].b32.s1;
    MEM[v + 2].b32.s1 = MEM[z + 2].b32.s1 + shift_down;
    MEM[v + 1].b32.s1 = MEM[x + 1].b32.s1;
    let mut p;
    if MEM[q + 1].b32.s1 == 0 {
        p = new_kern(shift_up - MEM[x + 2].b32.s1 - (MEM[z + 3].b32.s1 - shift_down));
        MEM[p].b32.s1 = Some(z).tex_int();
    } else {
        let y = fraction_rule(MEM[q + 1].b32.s1);
        p = new_kern(axis_height(cur_size) - delta - (MEM[z + 3].b32.s1 - shift_down));
        MEM[y].b32.s1 = Some(p).tex_int();
        MEM[p].b32.s1 = Some(z).tex_int();
        p = new_kern(shift_up - MEM[x + 2].b32.s1 - (axis_height(cur_size) + delta));
        MEM[p].b32.s1 = Some(y).tex_int();
    }
    MEM[x].b32.s1 = p as i32;
    MEM[v + 5].b32.s1 = x as i32;
    // :774
    if (cur_style as i32) < TEXT_STYLE {
        delta = delim1(cur_size)
    } else {
        delta = delim2(cur_size)
    }
    x = var_delimiter(q + 4, cur_size, delta);
    MEM[x].b32.s1 = v as i32;
    z = var_delimiter(q + 5, cur_size, delta);
    MEM[v].b32.s1 = z as i32;
    MEM[q + 1].b32.s1 = hpack(x as i32, 0, PackMode::Additional) as i32;
    // :775
}
unsafe fn make_op(q: usize) -> scaled_t {
    if MEM[q].b16.s0 == Limit::Normal as u16 && (cur_style as i32) < TEXT_STYLE {
        MEM[q].b16.s0 = Limit::Limits as u16;
    }
    let mut delta = 0;
    let mut ot_assembly_ptr = ptr::null_mut();
    if MEM[q + 1].b32.s1 == MATH_CHAR {
        let mut c = 0;
        fetch(q + 1);
        if !(FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && usingOpenType(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine))
        {
            if (cur_style as i32) < TEXT_STYLE && cur_i.s1 as i32 % 4 == LIST_TAG {
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
            if let Some(mut p) = MEM[x + 5].b32.s1.opt() {
                if !is_char_node(Some(p))
                    && NODE_type(p) == TextNode::WhatsIt.into()
                    && whatsit_NODE_subtype(p) == WhatsItNST::Glyph
                {
                    let mut current_block_41: u64;
                    if (cur_style as i32) < TEXT_STYLE {
                        let mut h1 = 0;
                        h1 = get_ot_math_constant(cur_f, DISPLAYOPERATORMINHEIGHT);
                        if (h1 as f64)
                            < ((MEM[p + 3].b32.s1 + MEM[p + 2].b32.s1) * 5) as f64 / 4_f64
                        {
                            h1 = (((MEM[p + 3].b32.s1 + MEM[p + 2].b32.s1) * 5) as f64 / 4_f64)
                                as scaled_t
                        }
                        c = MEM[p + 4].b16.s1;
                        let mut n = 0;
                        let mut h2 = 0;
                        loop {
                            let g = get_ot_math_variant(cur_f, c as i32, n, &mut h2, 0);
                            if h2 > 0 {
                                MEM[p + 4].b16.s1 = g as u16;
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
                                MEM[x + 5].b32.s1 = Some(p).tex_int();
                                delta = 0;
                                current_block_41 = 18116816373875863516;
                            } else {
                                current_block_41 = 6717214610478484138;
                            }
                        } else {
                            measure_native_glyph(
                                &mut MEM[p] as *mut memory_word as *mut libc::c_void,
                                1i32,
                            );
                            current_block_41 = 6717214610478484138;
                        }
                    } else {
                        current_block_41 = 6717214610478484138;
                    }
                    match current_block_41 {
                        6717214610478484138 => {
                            delta = get_ot_math_ital_corr(cur_f, MEM[p + 4].b16.s1 as i32)
                        }
                        _ => {}
                    }
                    MEM[x + 1].b32.s1 = MEM[p + 1].b32.s1;
                    MEM[x + 3].b32.s1 = MEM[p + 3].b32.s1;
                    MEM[x + 2].b32.s1 = MEM[p + 2].b32.s1;
                }
            }
        }
        if MEM[q + 3].b32.s1 != 0 && MEM[q as usize].b16.s0 != Limit::Limits as u16 {
            MEM[x + 1].b32.s1 = MEM[x + 1].b32.s1 - delta
        }
        MEM[x + 4].b32.s1 = half(MEM[x + 3].b32.s1 - MEM[x + 2].b32.s1) - axis_height(cur_size);
        MEM[q + 1].b32.s1 = SUB_BOX;
        MEM[q + 1].b32.s0 = Some(x).tex_int();
    }
    let save_f = cur_f;
    if MEM[q].b16.s0 == Limit::Limits as u16 {
        // 777:
        let x = clean_box(
            q + 2,
            (2 * (cur_style as i32 / 4) + 4 + cur_style as i32 % 2) as i16,
        );
        let y = clean_box(q + 1, cur_style);
        let z = clean_box(q + 3, (2 * (cur_style as i32 / 4) + 5) as i16);
        let v = new_null_box();
        set_NODE_type(v, TextNode::VList);
        MEM[v + 1].b32.s1 = MEM[y + 1].b32.s1;
        if MEM[x + 1].b32.s1 > MEM[v + 1].b32.s1 {
            MEM[v + 1].b32.s1 = MEM[x + 1].b32.s1
        }
        if MEM[z + 1].b32.s1 > MEM[v + 1].b32.s1 {
            MEM[v + 1].b32.s1 = MEM[z + 1].b32.s1
        }
        let x = rebox(x, MEM[v + 1].b32.s1);
        let y = rebox(y, MEM[v + 1].b32.s1);
        let z = rebox(z, MEM[v + 1].b32.s1);
        MEM[x + 4].b32.s1 = half(delta);
        MEM[z + 4].b32.s1 = -(MEM[x + 4].b32.s1 as i32);
        MEM[v + 3].b32.s1 = MEM[y + 3].b32.s1;
        MEM[v + 2].b32.s1 = MEM[y + 2].b32.s1;
        cur_f = save_f;
        if MEM[q + 2].b32.s1 == EMPTY {
            free_node(x, BOX_NODE_SIZE);
            MEM[v + 5].b32.s1 = y as i32;
        } else {
            let mut shift_up = big_op_spacing3() - MEM[x + 2].b32.s1;
            if shift_up < big_op_spacing1() {
                shift_up = big_op_spacing1()
            }
            let p = new_kern(shift_up);
            MEM[p].b32.s1 = y as i32;
            MEM[x].b32.s1 = p as i32;
            let p = new_kern(big_op_spacing5());
            MEM[p].b32.s1 = x as i32;
            MEM[v + 5].b32.s1 = p as i32;
            MEM[v + 3].b32.s1 = MEM[v + 3].b32.s1
                + big_op_spacing5()
                + MEM[x + 3].b32.s1
                + MEM[x + 2].b32.s1
                + shift_up
        }
        if MEM[q + 3].b32.s1 == EMPTY {
            free_node(z, BOX_NODE_SIZE);
        } else {
            let mut shift_down = big_op_spacing4() - MEM[z + 3].b32.s1;
            if shift_down < big_op_spacing2() {
                shift_down = big_op_spacing2()
            }
            let p = new_kern(shift_down);
            MEM[y as usize].b32.s1 = p as i32;
            MEM[p].b32.s1 = z as i32;
            let p = new_kern(big_op_spacing5());
            MEM[z].b32.s1 = p as i32;
            MEM[v + 2].b32.s1 = MEM[v + 2].b32.s1
                + big_op_spacing5()
                + MEM[z + 3].b32.s1
                + MEM[z + 2].b32.s1
                + shift_down
        }
        MEM[q + 1].b32.s1 = v as i32
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    delta
}
unsafe fn make_ord(q: usize) {
    while MEM[q + 3].b32.s1 == EMPTY {
        if !(MEM[q + 2].b32.s1 == EMPTY) {
            break;
        }
        if !(MEM[q + 1].b32.s1 == MATH_CHAR) {
            break;
        }
        let p = MEM[q].b32.s1.opt();
        if p.is_none() {
            break;
        }
        let mut p = p.unwrap();
        if !(MEM[p].b16.s1 >= MathNode::Ord as u16 && MEM[p].b16.s1 <= MathNode::Punct as u16) {
            break;
        }
        if !(MEM[p + 1].b32.s1 == MATH_CHAR) {
            break;
        }
        if !(MEM[p + 1].b16.s1 as i32 % 256 == MEM[q + 1].b16.s1 as i32 % 256) {
            break;
        }
        MEM[q + 1].b32.s1 = MATH_TEXT_CHAR;
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
                        MEM[p].b32.s1 = MEM[q].b32.s1;
                        MEM[q].b32.s1 = Some(p).tex_int();
                        return;
                    } else {
                        match cur_i.s1 as i32 {
                            1 | 5 => MEM[q + 1].b16.s0 = cur_i.s0,
                            2 | 6 => MEM[p + 1].b16.s0 = cur_i.s0,
                            3 | 7 | 11 => {
                                let r = new_noad();
                                MEM[r + 1].b16.s0 = cur_i.s0;
                                MEM[r + 1].b16.s1 = (MEM[q + 1].b16.s1 as i32 % 256) as u16;
                                MEM[q].b32.s1 = r as i32;
                                MEM[r].b32.s1 = Some(p).tex_int();
                                if (cur_i.s1 as i32) < 11 {
                                    MEM[r + 1].b32.s1 = MATH_CHAR;
                                } else {
                                    MEM[r + 1].b32.s1 = MATH_TEXT_CHAR;
                                }
                            }
                            _ => {
                                MEM[q].b32.s1 = MEM[p].b32.s1;
                                MEM[q + 1].b16.s0 = cur_i.s0;
                                MEM[q + 3] = MEM[p + 3];
                                MEM[q + 2] = MEM[p + 2];
                                free_node(p, NOAD_SIZE);
                            }
                        }
                        if cur_i.s1 as i32 > 3 {
                            return;
                        }
                        MEM[q + 1].b32.s1 = MATH_CHAR;
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
        while let Some(next) = MEM[y].b32.s1.opt() {
            y = next;
        }
        MEM[y].b32.s1 = z;
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
        || !p.is_texnull()
            && !is_char_node(p.opt())
            && NODE_type(p as usize) == TextNode::WhatsIt.into()
            && whatsit_NODE_subtype(p as usize) == WhatsItNST::Glyph
    {
        shift_up = 0;
        shift_down = 0;
    } else {
        let z = hpack(p, 0, PackMode::Additional);
        let t = if (cur_style as i32) < SCRIPT_STYLE {
            SCRIPT_SIZE
        } else {
            SCRIPT_SCRIPT_SIZE
        };
        shift_up = MEM[z + 3].b32.s1 - sup_drop(t);
        shift_down = MEM[z + 2].b32.s1 + sub_drop(t);
        free_node(z, BOX_NODE_SIZE);
    }
    let mut x: usize;
    if MEM[q + 2].b32.s1 == EMPTY {
        // 784:
        let save_f = cur_f;
        x = clean_box(q + 3, (2 * (cur_style as i32 / 4) + 5) as i16);
        cur_f = save_f;
        MEM[x + 1].b32.s1 = MEM[x + 1].b32.s1 + *DIMENPAR(DimenPar::script_space);
        if shift_down < sub1(cur_size) {
            shift_down = sub1(cur_size)
        }
        clr = if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            MEM[x + 3].b32.s1 - get_ot_math_constant(cur_f, SUBSCRIPTTOPMAX)
        } else {
            MEM[x + 3].b32.s1 - (math_x_height(cur_size) * 4).abs() / 5
        };
        if shift_down < clr {
            shift_down = clr;
        }
        MEM[x + 4].b32.s1 = shift_down;
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            /*787: */
            if MEM[q + 3].b32.s1 == MATH_CHAR {
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
                        MEM[p + 4].b16.s2 as usize,
                        MEM[p + 4].b16.s1 as i32,
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
            (2 * (cur_style as i32 / 4) + 4 + cur_style as i32 % 2) as i16,
        );
        cur_f = save_f;
        MEM[x + 1].b32.s1 = MEM[x + 1].b32.s1 + *DIMENPAR(DimenPar::script_space);
        clr = if cur_style as i32 & 1i32 != 0 {
            sup3(cur_size)
        } else if (cur_style as i32) < TEXT_STYLE {
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
            MEM[x + 2].b32.s1 + get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMIN)
        } else {
            MEM[x + 2].b32.s1 + math_x_height(cur_size).abs() / 4
        };
        if shift_up < clr {
            shift_up = clr
        }
        if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            // 788:
            if MEM[q + 2].b32.s1 == MATH_CHAR {
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
                        MEM[p + 4].b16.s2 as usize,
                        MEM[p + 4].b16.s1 as i32,
                        script_f,
                        script_g as i32,
                        SUP_CMD,
                        shift_up,
                    )
                }
            }
            if sup_kern != 0i32 && MEM[q + 3].b32.s1 == EMPTY {
                p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
            }
        }
        if MEM[q + 3].b32.s1 == EMPTY {
            MEM[x + 4].b32.s1 = -(shift_up as i32)
        } else {
            // 786:
            let save_f = cur_f;
            let y = clean_box(q + 3, (2 * (cur_style as i32 / 4) + 5) as i16);
            cur_f = save_f;
            MEM[y + 1].b32.s1 = MEM[y + 1].b32.s1 + *DIMENPAR(DimenPar::script_space);
            if shift_down < sub2(cur_size) {
                shift_down = sub2(cur_size)
            }
            if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                    as i32
                    != 0
            {
                clr = get_ot_math_constant(cur_f, SUBSUPERSCRIPTGAPMIN)
                    - (shift_up - MEM[x + 2].b32.s1 - (MEM[y + 3].b32.s1 - shift_down))
            } else {
                clr = 4 * default_rule_thickness()
                    - (shift_up - MEM[x + 2].b32.s1 - (MEM[y + 3].b32.s1 - shift_down))
            }
            if clr > 0 {
                shift_down = shift_down + clr;
                if FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    clr = get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT)
                        - (shift_up - MEM[x + 2].b32.s1)
                } else {
                    clr = (math_x_height(cur_size) * 4).abs() / 5 - (shift_up - MEM[x + 2].b32.s1)
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
                if MEM[q + 3].b32.s1 == MATH_CHAR {
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
                            MEM[p + 4].b16.s2 as usize,
                            MEM[p + 4].b16.s1 as i32,
                            script_f,
                            script_g as i32,
                            SUB_CMD,
                            shift_down,
                        )
                    }
                }
                if sub_kern != MATH_CHAR {
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
                            MEM[p + 4].b16.s2 as usize,
                            MEM[p + 4].b16.s1 as i32,
                            script_f,
                            script_g as i32,
                            SUP_CMD,
                            shift_up,
                        )
                    }
                }
                if sup_kern != 0 && MEM[(q + 3) as usize].b32.s1 == EMPTY {
                    p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
                }
            }
            MEM[x + 4].b32.s1 = sup_kern + delta - sub_kern;
            let p = new_kern(shift_up - MEM[x + 2].b32.s1 - (MEM[y + 3].b32.s1 - shift_down));
            MEM[x].b32.s1 = Some(p).tex_int();
            MEM[p].b32.s1 = y as i32;
            x = vpackage(Some(x), 0, PackMode::Additional, MAX_HALFWORD) as usize;
            MEM[x + 4].b32.s1 = shift_down
        }
    }
    if let Some(mut p) = MEM[q + 1].b32.s1.opt() {
        while let Some(next) = MEM[p].b32.s1.opt() {
            p = next;
        }
        MEM[p].b32.s1 = Some(x).tex_int();
    } else {
        MEM[q + 1].b32.s1 = Some(x).tex_int();
    }
}
unsafe fn make_left_right(
    mut q: usize,
    mut style: i16,
    mut max_d: scaled_t,
    mut max_h: scaled_t,
) -> i16 {
    cur_style = style;
    if (cur_style as i32) < SCRIPT_STYLE {
        cur_size = TEXT_SIZE;
    } else {
        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
    }
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
    let mut current_block: u64;
    let mut mlist: i32 = 0;
    let mut penalties: bool = false;
    let mut style: i16 = 0;
    let mut save_style: i16 = 0;
    let mut r: i32 = 0;
    let mut r_type: i16 = 0;
    let mut t: i16 = 0;
    let mut x: i32 = 0;
    let mut pen: i32 = 0;
    let mut s: i16 = 0;
    let mut max_h: scaled_t = 0;
    let mut max_d: scaled_t = 0;
    let mut delta: scaled_t = 0;
    mlist = cur_mlist;
    penalties = mlist_penalties;
    style = cur_style;
    let mut qopt = mlist.opt();
    r = None.tex_int();
    r_type = MathNode::Op as i16;
    max_h = 0;
    max_d = 0;
    if (cur_style as i32) < SCRIPT_STYLE {
        cur_size = TEXT_SIZE;
    } else {
        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18);
    while let Some(q) = qopt {
        loop
        // 753:
        {
            delta = 0; /*:755 */
            match ND::from(MEM[q].b16.s1) {
                ND::Math(n) => match n {
                    MathNode::Bin => {
                        match ND::from(r_type as u16) {
                            ND::Math(rt) => match rt {
                                MathNode::Bin
                                | MathNode::Op
                                | MathNode::Rel
                                | MathNode::Open
                                | MathNode::Punct
                                | MathNode::Left => {}
                                _ => {
                                    current_block = 1677945370889843322;
                                    break;
                                }
                            },
                            _ => {
                                // TODO: check
                                current_block = 1677945370889843322;
                                break;
                            }
                        }
                        MEM[q as usize].b16.s1 = MathNode::Ord as u16;
                    }
                    MathNode::Rel | MathNode::Close | MathNode::Punct | MathNode::Right => {
                        if r_type as u16 == MathNode::Bin as u16 {
                            MEM[r as usize].b16.s1 = MathNode::Ord as u16;
                        }
                        if MEM[q].b16.s1 == MathNode::Right as u16 {
                            current_block = 2476306051584715158;
                            break;
                        } else {
                            current_block = 1677945370889843322;
                            break;
                        }
                    }
                    MathNode::Left => {
                        current_block = 2476306051584715158;
                        break;
                    }
                    MathNode::Fraction => {
                        make_fraction(q);
                        current_block = 454865348394072936;
                        break;
                    }
                    MathNode::Op => {
                        delta = make_op(q);
                        if MEM[q].b16.s0 == Limit::Limits as u16 {
                            current_block = 454865348394072936;
                            break;
                        } else {
                            current_block = 1677945370889843322;
                            break;
                        }
                    }
                    MathNode::Ord => {
                        make_ord(q as usize);
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::Open | MathNode::Inner => {
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::Radical => {
                        make_radical(q);
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::Over => {
                        make_over(q);
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::Under => {
                        make_under(q);
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::Accent => {
                        make_math_accent(q);
                        current_block = 1677945370889843322;
                        break;
                    }
                    MathNode::VCenter => {
                        make_vcenter(q);
                        current_block = 1677945370889843322;
                        break;
                    }
                },
                ND::Text(n) => match n {
                    TextNode::Style => {
                        cur_style = MEM[q].b16.s0 as i16;
                        if (cur_style as i32) < SCRIPT_STYLE {
                            cur_size = TEXT_SIZE;
                        } else {
                            cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
                        }
                        cur_mu = x_over_n(math_quad(cur_size), 18);
                        current_block = 12027452349022962373;
                        break;
                    }
                    TextNode::Choice => {
                        let mut p = None;
                        match cur_style as i32 / 2 {
                            0 => {
                                p = MEM[q + 1].b32.s0.opt();
                                MEM[q + 1].b32.s0 = None.tex_int();
                            }
                            1 => {
                                p = MEM[q + 1].b32.s1.opt();
                                MEM[q + 1].b32.s1 = None.tex_int();
                            }
                            2 => {
                                p = MEM[q + 2].b32.s0.opt();
                                MEM[q + 2].b32.s0 = None.tex_int();
                            }
                            3 => {
                                p = MEM[q + 2].b32.s1.opt();
                                MEM[q + 2].b32.s1 = None.tex_int();
                            }
                            _ => {}
                        }
                        flush_node_list(MEM[q + 1].b32.s0.opt());
                        flush_node_list(MEM[q + 1].b32.s1.opt());
                        flush_node_list(MEM[q + 2].b32.s0.opt());
                        flush_node_list(MEM[q + 2].b32.s1.opt());
                        set_NODE_type(q, TextNode::Style);
                        MEM[q].b16.s0 = cur_style as u16;
                        MEM[q + 1].b32.s1 = 0;
                        MEM[q + 2].b32.s1 = 0;
                        if let Some(mut p) = p {
                            let z = MEM[q].b32.s1;
                            MEM[q].b32.s1 = Some(p).tex_int();
                            while let Some(next) = LLIST_link(p as usize).opt() {
                                p = next;
                            }
                            MEM[p].b32.s1 = z;
                        }
                        current_block = 12027452349022962373;
                        break;
                    }
                    TextNode::Ins
                    | TextNode::Mark
                    | TextNode::Adjust
                    | TextNode::WhatsIt
                    | TextNode::Penalty
                    | TextNode::Disc => {
                        current_block = 12027452349022962373;
                        break;
                    }
                    TextNode::Rule => {
                        if MEM[q + 3].b32.s1 > max_h {
                            max_h = MEM[q + 3].b32.s1
                        }
                        if MEM[q + 2].b32.s1 > max_d {
                            max_d = MEM[q + 2].b32.s1
                        }
                        current_block = 12027452349022962373;
                        break;
                    }
                    TextNode::Glue => {
                        if MEM[q].b16.s0 == MU_GLUE {
                            x = *GLUE_NODE_glue_ptr(q);
                            let y = math_glue(x as usize, cur_mu);
                            delete_glue_ref(x as usize);
                            *GLUE_NODE_glue_ptr(q) = Some(y).tex_int();
                            MEM[q].b16.s0 = NORMAL as u16
                        } else if cur_size != TEXT_SIZE && MEM[q].b16.s0 == COND_MATH_GLUE {
                            if let Some(p) = MEM[q].b32.s1.opt() {
                                if NODE_type(p) == TextNode::Glue.into()
                                    || NODE_type(p) == TextNode::Kern.into()
                                {
                                    MEM[q].b32.s1 = MEM[p].b32.s1;
                                    MEM[p].b32.s1 = None.tex_int();
                                    flush_node_list(Some(p));
                                }
                            }
                        }
                        current_block = 12027452349022962373;
                        break;
                    }
                    TextNode::Kern => {
                        math_kern(q as usize, cur_mu);
                        current_block = 12027452349022962373;
                        break;
                    }
                    _ => confusion(b"mlist1"),
                },
                _ => confusion(b"mlist1"),
            }
        }
        match current_block {
            1677945370889843322 => {
                let mut p;
                match MEM[q + 1].b32.s1 {
                    1 | 4 => {
                        fetch(q + 1);
                        if FONT_AREA[cur_f as usize] as u32 == AAT_FONT_FLAG
                            || FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                        {
                            let z = new_native_character(cur_f, cur_c);
                            p = get_node(GLYPH_NODE_SIZE) as i32;
                            set_NODE_type(p as usize, TextNode::WhatsIt);
                            set_whatsit_NODE_subtype(p as usize, WhatsItNST::Glyph);
                            MEM[(p + 4) as usize].b16.s2 = cur_f as u16;
                            MEM[(p + 4) as usize].b16.s1 = real_get_native_glyph(
                                &mut MEM[z] as *mut memory_word as *mut libc::c_void,
                                0,
                            );
                            measure_native_glyph(
                                &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                                1,
                            );
                            free_node(z, MEM[z + 4].b16.s3 as i32);
                            delta =
                                get_ot_math_ital_corr(cur_f, MEM[(p + 4) as usize].b16.s1 as i32);
                            if MEM[q + 1].b32.s1 == MATH_TEXT_CHAR
                                && !(FONT_AREA[cur_f as usize] as u32 == OTGR_FONT_FLAG
                                    && isOpenTypeMathFont(
                                        FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0) as i32
                                    != 0
                            {
                                delta = 0;
                            }
                            if MEM[q + 3].b32.s1 == EMPTY && delta != 0 {
                                MEM[p as usize].b32.s1 = new_kern(delta) as i32;
                                delta = 0;
                            }
                        } else if cur_i.s3 as i32 > 0 {
                            delta = FONT_INFO
                                [(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4i32) as usize]
                                .b32
                                .s1;
                            p = new_character(cur_f, cur_c as UTF16_code).tex_int();
                            if MEM[q + 1].b32.s1 == MATH_TEXT_CHAR
                                && FONT_INFO[(2 + PARAM_BASE[cur_f as usize]) as usize].b32.s1 != 0
                            {
                                delta = 0;
                            }
                            if MEM[q + 3].b32.s1 == EMPTY && delta != 0 {
                                MEM[p as usize].b32.s1 = new_kern(delta) as i32;
                                delta = 0i32
                            }
                        } else {
                            p = None.tex_int()
                        }
                    }
                    0 => p = None.tex_int(),
                    2 => p = MEM[q + 1].b32.s0,
                    3 => {
                        cur_mlist = MEM[q + 1].b32.s0;
                        save_style = cur_style;
                        mlist_penalties = false;
                        mlist_to_hlist();
                        cur_style = save_style;
                        if (cur_style as i32) < SCRIPT_STYLE {
                            cur_size = TEXT_SIZE
                        } else {
                            cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
                        }
                        cur_mu = x_over_n(math_quad(cur_size), 18i32);
                        p = hpack(MEM[TEMP_HEAD].b32.s1, 0i32, PackMode::Additional) as i32;
                    }
                    _ => confusion(b"mlist2"),
                }
                MEM[q + 1].b32.s1 = p;
                if MEM[q + 3].b32.s1 == EMPTY && MEM[q + 2].b32.s1 == EMPTY {
                    current_block = 454865348394072936;
                } else {
                    make_scripts(q, delta);
                    current_block = 454865348394072936;
                }
            }
            _ => {}
        }
        match current_block {
            454865348394072936 => {
                /*check_dimensions */
                let z = hpack(MEM[q + 1].b32.s1, 0, PackMode::Additional);
                if MEM[(z + 3) as usize].b32.s1 > max_h {
                    max_h = MEM[(z + 3) as usize].b32.s1
                }
                if MEM[(z + 2) as usize].b32.s1 > max_d {
                    max_d = MEM[(z + 2) as usize].b32.s1
                }
                free_node(z as usize, BOX_NODE_SIZE);
                current_block = 2476306051584715158;
            }
            _ => {}
        }
        match current_block {
            2476306051584715158 => {
                /*done_with_noad */
                r = q as i32;
                r_type = MEM[r as usize].b16.s1 as i16;
                if r_type == MathNode::Right as i16 {
                    r_type = MathNode::Left as i16;
                    cur_style = style;
                    if (cur_style as i32) < SCRIPT_STYLE {
                        cur_size = TEXT_SIZE;
                    } else {
                        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
                    }
                    cur_mu = x_over_n(math_quad(cur_size), 18)
                }
            }
            _ => {}
        }
        /*done_with_node */
        qopt = LLIST_link(q).opt();
    } /*ord_noad *//*:755 */
    if r_type == MathNode::Bin as i16 {
        MEM[r as usize].b16.s1 = 16;
    }
    let mut p = TEMP_HEAD;
    MEM[p].b32.s1 = None.tex_int();
    let mut qopt = mlist.opt();
    r_type = 0 as i16;
    cur_style = style;
    if (cur_style as i32) < SCRIPT_STYLE {
        cur_size = TEXT_SIZE
    } else {
        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18);
    while let Some(q) = qopt {
        let mut current_block_236: u64;
        t = MathNode::Ord as i16;
        s = NOAD_SIZE as i16;
        pen = INF_PENALTY;
        match NODE_type(q) {
            ND::Math(n) => match n {
                MathNode::Op
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct
                | MathNode::Inner => {
                    t = MEM[q].b16.s1 as i16;
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Bin => {
                    t = MathNode::Bin as i16;
                    pen = *INTPAR(IntPar::bin_op_penalty);
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Rel => {
                    t = MathNode::Rel as i16;
                    pen = *INTPAR(IntPar::rel_penalty);
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Ord | MathNode::VCenter | MathNode::Over | MathNode::Under => {
                    current_block_236 = 15067367080042895309
                }
                MathNode::Radical => {
                    s = RADICAL_NOAD_SIZE as i16;
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Accent => {
                    s = ACCENT_NOAD_SIZE as i16;
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Fraction => {
                    t = MathNode::Inner as i16;
                    s = FRACTION_NOAD_SIZE as i16;
                    current_block_236 = 15067367080042895309;
                }
                MathNode::Left | MathNode::Right => {
                    t = make_left_right(q, style, max_d, max_h);
                    current_block_236 = 15067367080042895309;
                }
            },
            ND::Text(n) => match n {
                TextNode::Style => {
                    cur_style = MEM[q].b16.s0 as i16;
                    s = STYLE_NODE_SIZE as i16;
                    if (cur_style as i32) < SCRIPT_STYLE {
                        cur_size = TEXT_SIZE
                    } else {
                        cur_size = SCRIPT_SIZE * ((cur_style as usize - 2) / 2)
                    }
                    cur_mu = x_over_n(math_quad(cur_size), 18);
                    current_block_236 = 11920828421623439930;
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
                    MEM[p].b32.s1 = q as i32;
                    p = q;
                    qopt = LLIST_link(q).opt();
                    MEM[p].b32.s1 = None.tex_int();
                    current_block_236 = 7344615536999694015;
                }
                _ => confusion(b"mlist3"),
            },
            _ => confusion(b"mlist3"),
        }
        match current_block_236 {
            15067367080042895309 => {
                if r_type as i32 > 0i32 {
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
                    match OFFSET_TABLE[r_type as usize - MathNode::Ord as usize]
                        [t as usize - MathNode::Ord as usize]
                    {
                        b'0' => {
                            // no space
                            x = 0i32
                        }
                        b'1' => {
                            // a conditional thin space
                            if (cur_style as i32) < SCRIPT_STYLE {
                                x = GluePar::thin_mu_skip as i32;
                            } else {
                                x = 0;
                            }
                        }
                        b'2' => {
                            // a thin space
                            x = GluePar::thin_mu_skip as i32;
                        }
                        b'3' => {
                            // a conditional medium space
                            if (cur_style as i32) < SCRIPT_STYLE {
                                x = GluePar::med_mu_skip as i32;
                            } else {
                                x = 0;
                            }
                        }
                        b'4' => {
                            // a conditional thick space
                            if (cur_style as i32) < SCRIPT_STYLE {
                                x = GluePar::thick_mu_skip as i32;
                            } else {
                                x = 0;
                            }
                        }
                        _ => {
                            // impossible
                            confusion(b"mlist4");
                        }
                    }
                    if x != 0 {
                        let y = math_glue(EQTB[GLUE_BASE + x as usize].val as usize, cur_mu);
                        let z = new_glue(y);
                        MEM[y].b32.s1 = None.tex_int();
                        MEM[p].b32.s1 = Some(z).tex_int();
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
                    if let Some(m) = MEM[q].b32.s1.opt() {
                        if pen < INF_PENALTY {
                            r_type = MEM[m].b16.s1 as i16;
                            if ND::from(r_type as u16) != TextNode::Penalty.into() {
                                if r_type != MathNode::Rel as i16 {
                                    let z = new_penalty(pen);
                                    MEM[p].b32.s1 = Some(z).tex_int();
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
                current_block_236 = 11920828421623439930;
            }
            _ => {}
        }
        match current_block_236 {
            11920828421623439930 => {
                /*delete_q */
                r = q as i32;
                qopt = LLIST_link(q).opt();
                free_node(r as usize, s as i32);
            }
            _ => {}
        }
    }
}
unsafe fn var_delimiter(d: usize, mut s: usize, mut v: scaled_t) -> usize {
    let mut b: usize = 0;
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
                MEM[b + 1].b32.s1 = FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1
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
                MEM[(b + 2) as usize].b32.s1 = w - MEM[(b + 3) as usize].b32.s1
            } else {
                b = char_box(f, c as i32)
            }
        /*:736 */
        } else if !ot_assembly_ptr.is_null() {
            b = build_opentype_assembly(f, ot_assembly_ptr, v, false) as usize
        } else {
            b = new_null_box();
            set_NODE_type(b, TextNode::VList);
            MEM[b + 5].b32.s1 = get_node(GLYPH_NODE_SIZE) as i32;
            set_NODE_type(MEM[b + 5].b32.s1 as usize, TextNode::WhatsIt);
            set_whatsit_NODE_subtype(MEM[b + 5].b32.s1 as usize, WhatsItNST::Glyph);
            MEM[(MEM[b + 5].b32.s1 + 4) as usize].b16.s2 = f as u16;
            MEM[(MEM[b + 5].b32.s1 + 4) as usize].b16.s1 = c;
            measure_native_glyph(
                &mut MEM[MEM[b + 5].b32.s1 as usize] as *mut memory_word as *mut libc::c_void,
                1,
            );
            MEM[b + 1].b32.s1 = MEM[(MEM[b + 5].b32.s1 + 1) as usize].b32.s1;
            MEM[b + 3].b32.s1 = MEM[(MEM[b + 5].b32.s1 + 3) as usize].b32.s1;
            MEM[b + 2].b32.s1 = MEM[(MEM[b + 5].b32.s1 + 2) as usize].b32.s1
        }
    } else {
        b = new_null_box();
        MEM[b + 1].b32.s1 = *DIMENPAR(DimenPar::null_delimiter_space)
    }
    MEM[b + 4].b32.s1 = half(MEM[b + 3].b32.s1 - MEM[b + 2].b32.s1) - axis_height(s);
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    b
}
unsafe fn char_box(mut f: internal_font_number, mut c: i32) -> usize {
    let b;
    let p;
    if FONT_AREA[f] as u32 == AAT_FONT_FLAG || FONT_AREA[f] as u32 == OTGR_FONT_FLAG {
        b = new_null_box();
        p = new_native_character(f, c);
        MEM[b + 5].b32.s1 = Some(p).tex_int();
        MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1;
        MEM[b + 1].b32.s1 = MEM[p + 1].b32.s1;
        if MEM[p + 2].b32.s1 < 0 {
            MEM[b + 2].b32.s1 = 0
        } else {
            MEM[b + 2].b32.s1 = MEM[p + 2].b32.s1
        }
    } else {
        let q = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c as u16)) as usize].b16;
        b = new_null_box();
        MEM[b + 1].b32.s1 = FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1
            + FONT_INFO[(ITALIC_BASE[f] + q.s1 as i32 / 4) as usize]
                .b32
                .s1;
        MEM[b + 3].b32.s1 = FONT_INFO[(HEIGHT_BASE[f] + q.s2 as i32 / 16) as usize]
            .b32
            .s1;
        MEM[b + 2].b32.s1 = FONT_INFO[(DEPTH_BASE[f] + q.s2 as i32 % 16) as usize]
            .b32
            .s1;
        p = get_avail();
        MEM[p].b16.s0 = c as u16;
        MEM[p].b16.s1 = f as u16
    }
    MEM[b + 5].b32.s1 = p as i32;
    b
}
unsafe fn stack_into_box(b: usize, mut f: internal_font_number, mut c: u16) {
    let p = char_box(f, c as i32);
    MEM[p].b32.s1 = MEM[b + 5].b32.s1;
    MEM[b + 5].b32.s1 = p as i32;
    MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1;
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
        if let Some(mut q) = MEM[b + 5].b32.s1.opt() {
            while let Some(next) = LLIST_link(q).opt() {
                q = next;
            }
            MEM[q].b32.s1 = Some(p).tex_int();
            if MEM[b + 3].b32.s1 < MEM[p + 3].b32.s1 {
                MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1
            }
            if MEM[b + 2].b32.s1 < MEM[p + 2].b32.s1 {
                MEM[b + 2].b32.s1 = MEM[p + 2].b32.s1
            }
        } else {
            MEM[b + 5].b32.s1 = Some(p).tex_int();
        }
    } else {
        MEM[p].b32.s1 = MEM[b + 5].b32.s1;
        MEM[b + 5].b32.s1 = Some(p).tex_int();
        MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1;
        if MEM[b + 1].b32.s1 < MEM[p + 1].b32.s1 {
            MEM[b + 1].b32.s1 = MEM[p + 1].b32.s1
        }
    };
}
unsafe fn stack_glue_into_box(b: usize, mut min: scaled_t, mut max: scaled_t) {
    let q = new_spec(0);
    *GLUE_SPEC_width(q) = min;
    *GLUE_SPEC_stretch(q) = max - min;
    let p = new_glue(q);
    if NODE_type(b) == TextNode::HList.into() {
        if let Some(mut q) = MEM[b + 5].b32.s1.opt() {
            while let Some(next) = MEM[q].b32.s1.opt() {
                q = next;
            }
            MEM[q].b32.s1 = p as i32;
        } else {
            MEM[b + 5].b32.s1 = p as i32;
        }
    } else {
        MEM[p].b32.s1 = MEM[b + 5].b32.s1;
        MEM[b + 5].b32.s1 = p as i32;
        MEM[b + 3].b32.s1 = MEM[p + 3].b32.s1;
        MEM[b + 1].b32.s1 = MEM[p + 1].b32.s1;
    };
}
unsafe fn build_opentype_assembly(
    mut f: internal_font_number,
    mut a: *mut libc::c_void,
    mut s: scaled_t,
    mut horiz: bool,
) -> usize {
    let mut g: i32 = 0;
    let mut s_max: scaled_t = 0;
    let mut o: scaled_t = 0;
    let mut oo: scaled_t = 0;
    let mut prev_o: scaled_t = 0;
    let mut nat: scaled_t = 0;
    let mut str: scaled_t = 0;
    let mut b = new_null_box() as usize;
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
        s_max = 0;
        prev_o = 0;
        for i in 0..ot_part_count(a as *const GlyphAssembly) {
            if ot_part_is_extender(a as *const GlyphAssembly, i) {
                no_extenders = false;
                for _ in 0..n {
                    o = ot_part_start_connector(f, a as *const GlyphAssembly, i);
                    if min_o < o {
                        o = min_o
                    }
                    if prev_o < o {
                        o = prev_o
                    }
                    s_max = s_max - o + ot_part_full_advance(f, a as *const GlyphAssembly, i);
                    prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
                }
            } else {
                o = ot_part_start_connector(f, a as *const GlyphAssembly, i);
                if min_o < o {
                    o = min_o
                }
                if prev_o < o {
                    o = prev_o
                }
                s_max = s_max - o + ot_part_full_advance(f, a as *const GlyphAssembly, i);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
            }
        }
        if s_max >= s || no_extenders {
            break;
        }
    }
    prev_o = 0;
    for i in 0..ot_part_count(a as *const GlyphAssembly) {
        if ot_part_is_extender(a as *const GlyphAssembly, i) {
            for _ in 0..n {
                o = ot_part_start_connector(f, a as *const GlyphAssembly, i);
                if prev_o < o {
                    o = prev_o
                }
                oo = o;
                if min_o < o {
                    o = min_o
                }
                if oo > 0i32 {
                    stack_glue_into_box(b, -oo, -o);
                }
                g = ot_part_glyph(a as *const GlyphAssembly, i);
                stack_glyph_into_box(b, f, g);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
            }
        } else {
            o = ot_part_start_connector(f, a as *const GlyphAssembly, i);
            if prev_o < o {
                o = prev_o
            }
            oo = o;
            if min_o < o {
                o = min_o
            }
            if oo > 0 {
                stack_glue_into_box(b, -oo, -o);
            }
            g = ot_part_glyph(a as *const GlyphAssembly, i);
            stack_glyph_into_box(b, f, g);
            prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
        }
    }
    let mut popt = MEM[b + 5].b32.s1.opt();
    nat = 0i32;
    str = 0i32;
    while let Some(p) = popt {
        if NODE_type(p) == TextNode::WhatsIt.into() {
            if horiz {
                nat = nat + MEM[p + 1].b32.s1
            } else {
                nat = nat + MEM[p + 3].b32.s1 + MEM[p + 2].b32.s1
            }
        } else if NODE_type(p as usize) == TextNode::Glue.into() {
            nat = nat + MEM[(MEM[p + 1].b32.s0 + 1) as usize].b32.s1;
            str = str + MEM[(MEM[p + 1].b32.s0 + 2) as usize].b32.s1
        }
        popt = LLIST_link(p).opt();
    }
    o = 0;
    if s > nat && str > 0 {
        o = s - nat;
        if o > str {
            o = str
        }
        MEM[b + 5].b16.s0 = NORMAL as u16;
        MEM[b + 5].b16.s1 = GlueSign::Stretching as u16;
        MEM[b + 6].gr = o as f64 / str as f64;
        if horiz {
            MEM[b + 1].b32.s1 = nat + tex_round(str as f64 * *BOX_glue_set(b))
        } else {
            MEM[b + 3].b32.s1 = nat + tex_round(str as f64 * *BOX_glue_set(b))
        }
    } else if horiz {
        MEM[b + 1].b32.s1 = nat
    } else {
        MEM[b + 3].b32.s1 = nat
    }
    b
}
unsafe fn rebox(mut b: usize, mut w: scaled_t) -> usize {
    let mut f: internal_font_number = 0;
    let mut v: scaled_t = 0;
    if MEM[b + 1].b32.s1 != w && !MEM[b + 5].b32.s1.is_texnull() {
        if NODE_type(b) == TextNode::VList.into() {
            b = hpack(b as i32, 0, PackMode::Additional) as usize
        }
        let mut p = MEM[b + 5].b32.s1 as usize;
        if is_char_node(Some(p)) && MEM[p].b32.s1.is_texnull() {
            f = MEM[p as usize].b16.s1 as internal_font_number;
            v = FONT_INFO[(WIDTH_BASE[f]
                + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, MEM[p].b16.s0)) as usize]
                    .b16
                    .s3 as i32) as usize]
                .b32
                .s1;
            if v != MEM[b + 1].b32.s1 {
                MEM[p].b32.s1 = new_kern(MEM[b + 1].b32.s1 - v) as i32;
            }
        }
        free_node(b, BOX_NODE_SIZE);
        b = new_glue(12);
        MEM[b].b32.s1 = p as i32;
        while let Some(next) = LLIST_link(p).opt() {
            p = next;
        }
        MEM[p].b32.s1 = new_glue(12) as i32;
        hpack(b as i32, w, PackMode::Exactly) as usize
    } else {
        MEM[b + 1].b32.s1 = w;
        b
    }
}
