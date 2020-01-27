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
use crate::xetex_errors::{confusion, error};
use crate::xetex_ext::{
    map_char_to_glyph, measure_native_glyph, real_get_native_glyph, OTGR_FONT_FLAG,
};
use crate::xetex_ini::{
    adjust_tail, avail, cur_c, cur_chr, cur_cmd, cur_dir, cur_f, cur_group, cur_i, cur_lang,
    cur_list, cur_val, cur_val1, empty, file_line_error_style_p, help_line, help_ptr,
    insert_src_special_every_math, just_box, nest_ptr, null_character, pre_adjust_tail, temp_ptr,
    tex_remainder, total_shrink, xtx_ligature_present, LR_problems, LR_ptr, CHAR_BASE, DEPTH_BASE,
    EQTB, EXTEN_BASE, FONT_AREA, FONT_BC, FONT_EC, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_PARAMS,
    HEIGHT_BASE, ITALIC_BASE, KERN_BASE, LIG_KERN_BASE, MEM, PARAM_BASE, SAVE_PTR, SAVE_STACK,
    SKEW_CHAR, WIDTH_BASE,
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
    get_node, get_token, get_x_token, group_code, hpack, insert_src_special, internal_font_number,
    just_copy, just_reverse, new_character, new_choice, new_glue, new_kern, new_math,
    new_native_character, new_noad, new_null_box, new_param_glue, new_penalty, new_rule,
    new_skip_param, new_spec, norm_min, off_save, pop_nest, push_math, push_nest,
    scan_delimiter_int, scan_dimen, scan_fifteen_bit_int, scan_keyword, scan_left_brace, scan_math,
    scan_math_class_int, scan_math_fam_int, scan_usv_num, unsave, vpackage,
};
use crate::xetex_xetexd::{is_char_node, BOX_glue_set, CHAR_NODE_font, LLIST_link};

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
pub(crate) type eight_bits = u8;
pub(crate) type str_number = i32;
pub(crate) type small_number = i16;
static mut null_delimiter: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
static mut cur_mlist: i32 = 0;
static mut cur_style: small_number = 0;
static mut cur_size: i32 = 0;
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
    let mut w: scaled_t = 0;
    let mut j: i32 = 0;
    let mut x: i32 = 0;
    let mut l: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut n: i32 = 0;
    let mut v: scaled_t = 0;
    let mut d: scaled_t = 0;

    get_token();

    if cur_cmd as u16 == MATH_SHIFT && cur_list.mode as i32 > 0 {
        // 1180:
        j = TEX_NULL;
        w = -MAX_HALFWORD;
        if cur_list.head == cur_list.tail {
            // 1520:
            pop_nest();
            if cur_list.eTeX_aux == TEX_NULL {
                x = 0
            } else if MEM[cur_list.eTeX_aux as usize].b32.s0 >= R_CODE {
                x = -1
            } else {
                x = 1; // :1519
            }
        } else {
            line_break(true);
            // 1528:
            if *GLUEPAR(GluePar::right_skip) == 0 {
                j = new_kern(0)
            } else {
                j = new_param_glue(GluePar::right_skip as small_number)
            }

            if *GLUEPAR(GluePar::left_skip) == 0 {
                p = new_kern(0)
            } else {
                p = new_param_glue(GluePar::left_skip as small_number)
            }

            MEM[p as usize].b32.s1 = j;

            j = new_null_box();
            MEM[(j + 1) as usize].b32.s1 = MEM[(just_box + 1) as usize].b32.s1;
            MEM[(j + 4) as usize].b32.s1 = MEM[(just_box + 4) as usize].b32.s1;
            MEM[(j + 5) as usize].b32.s1 = p;
            MEM[(j + 5) as usize].b16.s0 = MEM[(just_box + 5) as usize].b16.s0;
            MEM[(j + 5) as usize].b16.s1 = MEM[(just_box + 5) as usize].b16.s1;
            *BOX_glue_set(j) = *BOX_glue_set(just_box);

            v = MEM[(just_box + 4) as usize].b32.s1;
            if cur_list.eTeX_aux == TEX_NULL {
                x = 0;
            } else if MEM[cur_list.eTeX_aux as usize].b32.s0 >= R_CODE {
                x = -1;
            } else {
                x = 1; // :1519
            }
            if x >= 0i32 {
                p = MEM[(just_box + 5) as usize].b32.s1;
                MEM[TEMP_HEAD as usize].b32.s1 = TEX_NULL
            } else {
                v = -v - MEM[(just_box + 1) as usize].b32.s1;
                p = new_math(0i32, BEGIN_L_CODE as small_number);
                MEM[TEMP_HEAD as usize].b32.s1 = p;
                just_copy(
                    MEM[(just_box + 5) as usize].b32.s1,
                    p,
                    new_math(0i32, END_L_CODE as small_number),
                );
                cur_dir = RIGHT_TO_LEFT as small_number
            }
            v = v + 2i32
                * FONT_INFO[(QUAD_CODE + PARAM_BASE[EQTB[(CUR_FONT_LOC) as usize].b32.s1 as usize])
                    as usize]
                    .b32
                    .s1;
            if *INTPAR(IntPar::texxet) > 0i32 {
                // 1497:
                temp_ptr = get_avail(); /*1523:*/
                MEM[temp_ptr as usize].b32.s0 = BEFORE; /*:1398 */
                MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                LR_ptr = temp_ptr
            }
            while p != TEX_NULL {
                loop {
                    if is_char_node(p) {
                        f = *CHAR_NODE_font(p) as internal_font_number;
                        d = FONT_INFO[(WIDTH_BASE[f as usize]
                            + FONT_INFO[(CHAR_BASE[f as usize]
                                + effective_char(true, f, MEM[p as usize].b16.s0))
                                as usize]
                                .b16
                                .s3 as i32) as usize]
                            .b32
                            .s1;
                        current_block = 9427725525305667067;
                        break;
                    } else {
                        match MEM[p as usize].b16.s1 as i32 {
                            0 | 1 | 2 => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                current_block = 9427725525305667067;
                                break;
                            }
                            6 => {
                                MEM[GARBAGE as usize] = MEM[(p + 1) as usize];
                                MEM[GARBAGE as usize].b32.s1 = MEM[p as usize].b32.s1;
                                p = GARBAGE;
                                xtx_ligature_present = true
                            }
                            11 => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                current_block = 1677945370889843322;
                                break;
                            }
                            40 => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                current_block = 1677945370889843322;
                                break;
                            }
                            9 => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                if *INTPAR(IntPar::texxet) > 0i32 {
                                    current_block = 13660591889533726445;
                                    break;
                                } else {
                                    current_block = 2631791190359682872;
                                    break;
                                }
                            }
                            14 => {
                                d = MEM[(p + 1) as usize].b32.s1;
                                cur_dir = MEM[p as usize].b16.s0 as small_number;
                                current_block = 1677945370889843322;
                                break;
                            }
                            10 => {
                                q = MEM[(p + 1) as usize].b32.s0;
                                d = MEM[(q + 1) as usize].b32.s1;
                                if MEM[(just_box + 5) as usize].b16.s1 as i32 == STRETCHING {
                                    if MEM[(just_box + 5) as usize].b16.s0 as i32
                                        == MEM[q as usize].b16.s1 as i32
                                        && MEM[(q + 2) as usize].b32.s1 != 0
                                    {
                                        v = 0x3fffffffi32
                                    }
                                } else if MEM[(just_box + 5) as usize].b16.s1 as i32 == SHRINKING {
                                    if MEM[(just_box + 5) as usize].b16.s0 as i32
                                        == MEM[q as usize].b16.s0 as i32
                                        && MEM[(q + 3) as usize].b32.s1 != 0
                                    {
                                        v = 0x3fffffffi32
                                    }
                                }
                                if MEM[p as usize].b16.s0 as i32 >= 100 {
                                    current_block = 9427725525305667067;
                                    break;
                                } else {
                                    current_block = 1677945370889843322;
                                    break;
                                }
                            }
                            8 => {
                                if MEM[p as usize].b16.s0 == NATIVE_WORD_NODE
                                    || MEM[p as usize].b16.s0 == NATIVE_WORD_NODE_AT
                                    || MEM[p as usize].b16.s0 == GLYPH_NODE
                                    || MEM[p as usize].b16.s0 == PIC_NODE
                                    || MEM[p as usize].b16.s0 == PDF_NODE
                                {
                                    current_block = 11064061988481400464;
                                    break;
                                } else {
                                    current_block = 5846959088466685742;
                                    break;
                                }
                            }
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
                        if MEM[p as usize].b16.s0 as i32 >= 4 {
                            w = 0x3fffffffi32;
                            break;
                        } else {
                            current_block = 1677945370889843322;
                        }
                    }
                    13660591889533726445 =>
                    /*1525: */
                    {
                        if MEM[p as usize].b16.s0 as i32 & 1 != 0 {
                            if MEM[LR_ptr as usize].b32.s0
                                == 4i32 * (MEM[p as usize].b16.s0 as i32 / 4) + 3
                            {
                                temp_ptr = LR_ptr;
                                LR_ptr = MEM[temp_ptr as usize].b32.s1;
                                MEM[temp_ptr as usize].b32.s1 = avail;
                                avail = temp_ptr
                            } else if MEM[p as usize].b16.s0 as i32 > 4 {
                                w = 0x3fffffffi32;
                                break;
                            }
                        } else {
                            temp_ptr = get_avail();
                            MEM[temp_ptr as usize].b32.s0 =
                                4i32 * (MEM[p as usize].b16.s0 as i32 / 4) + 3;
                            MEM[temp_ptr as usize].b32.s1 = LR_ptr;
                            LR_ptr = temp_ptr;
                            if MEM[p as usize].b16.s0 as i32 / 8 != cur_dir as i32 {
                                just_reverse(p);
                                p = 4999999i32 - 3i32
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
                p = *LLIST_link(p as isize);
            }
            if *INTPAR(IntPar::texxet) > 0 {
                while LR_ptr != TEX_NULL {
                    temp_ptr = LR_ptr;
                    LR_ptr = MEM[temp_ptr as usize].b32.s1;
                    MEM[temp_ptr as usize].b32.s1 = avail;
                    avail = temp_ptr
                }
                if LR_problems != 0 {
                    w = MAX_HALFWORD;
                    LR_problems = 0
                }
            }
            cur_dir = LEFT_TO_RIGHT as small_number;
            flush_node_list(MEM[TEMP_HEAD as usize].b32.s1);
        }
        if *LOCAL(Local::par_shape) == TEX_NULL {
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
        } else {
            n = MEM[*LOCAL(Local::par_shape) as usize].b32.s0;
            if cur_list.prev_graf + 2 >= n {
                p = *LOCAL(Local::par_shape) + 2 * n
            } else {
                p = *LOCAL(Local::par_shape) + 2 * (cur_list.prev_graf + 2i32)
            }
            s = MEM[(p - 1) as usize].b32.s1;
            l = MEM[p as usize].b32.s1
        }
        push_math(MATH_SHIFT_GROUP as group_code);
        cur_list.mode = MMODE as i16;
        eq_word_define(INT_BASE + IntPar::cur_fam as i32, -1i32);
        eq_word_define(DIMEN_BASE + DimenPar::pre_display_size as i32, w);
        cur_list.eTeX_aux = j;
        eq_word_define(INT_BASE + IntPar::pre_display_correction as i32, x);
        eq_word_define(DIMEN_BASE + DimenPar::display_width as i32, l);
        eq_word_define(DIMEN_BASE + DimenPar::display_indent as i32, s);
        if *LOCAL(Local::every_display) != TEX_NULL {
            begin_token_list(*LOCAL(Local::every_display), EVERY_DISPLAY_TEXT);
        }
        if nest_ptr == 1 {
            build_page();
        }
    } else {
        back_input();
        push_math(MATH_SHIFT_GROUP as group_code);
        eq_word_define(INT_BASE + IntPar::cur_fam as i32, -1);
        if insert_src_special_every_math {
            insert_src_special();
        }
        if *LOCAL(Local::every_math) != TEX_NULL {
            begin_token_list(*LOCAL(Local::every_math), EVERY_MATH_TEXT);
        }
    };
}
pub(crate) unsafe fn start_eq_no() {
    SAVE_STACK[SAVE_PTR + 0].b32.s1 = cur_chr;
    SAVE_PTR += 1;
    push_math(MATH_SHIFT_GROUP as group_code);
    eq_word_define(INT_BASE + IntPar::cur_fam as i32, -1);
    if insert_src_special_every_math {
        insert_src_special();
    }
    if *LOCAL(Local::every_math) != TEX_NULL {
        begin_token_list(*LOCAL(Local::every_math), EVERY_MATH_TEXT);
    };
}
pub(crate) unsafe fn math_limit_switch() {
    if cur_list.head != cur_list.tail {
        if MEM[cur_list.tail as usize].b16.s1 as i32 == OP_NOAD {
            MEM[cur_list.tail as usize].b16.s0 = cur_chr as u16;
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
unsafe extern "C" fn scan_delimiter(mut p: i32, mut r: bool) {
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
            if !(cur_cmd as u16 == SPACER || cur_cmd as u16 == RELAX) {
                break;
            }
        }
        match cur_cmd as u16 {
            LETTER | OTHER_CHAR => cur_val = EQTB[(DEL_CODE_BASE + cur_chr) as usize].b32.s1,
            DELIM_NUM => {
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
        MEM[p as usize].b16.s3 =
            (cur_val % 0x200000 / 0x10000 * 0x100 + cur_val / 0x200000i32 % 0x100i32) as u16;
        MEM[p as usize].b16.s2 = (cur_val % 0x10000) as u16;
        MEM[p as usize].b16.s1 = 0_u16;
        MEM[p as usize].b16.s0 = 0_u16
    } else {
        MEM[p as usize].b16.s3 = (cur_val / 0x100000 % 16) as u16;
        MEM[p as usize].b16.s2 = (cur_val / 0x1000 % 0x100) as u16;
        MEM[p as usize].b16.s1 = (cur_val / 0x100 % 16) as u16;
        MEM[p as usize].b16.s0 = (cur_val % 0x100) as u16
    };
}
pub(crate) unsafe fn math_radical() {
    MEM[cur_list.tail as usize].b32.s1 = get_node(RADICAL_NOAD_SIZE);
    cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
    MEM[cur_list.tail as usize].b16.s1 = RADICAL_NOAD as u16;
    MEM[cur_list.tail as usize].b16.s0 = NORMAL as u16;
    MEM[(cur_list.tail + 1) as usize].b32 = empty;
    MEM[(cur_list.tail + 3) as usize].b32 = empty;
    MEM[(cur_list.tail + 2) as usize].b32 = empty;
    scan_delimiter(cur_list.tail + 4i32, true);
    scan_math(cur_list.tail + 1i32);
}
pub(crate) unsafe fn math_ac() {
    let mut c: i32 = 0;
    if cur_cmd == ACCENT as u8 {
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
    MEM[cur_list.tail as usize].b32.s1 = get_node(ACCENT_NOAD_SIZE);
    cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
    MEM[cur_list.tail as usize].b16.s1 = ACCENT_NOAD as u16;
    MEM[cur_list.tail as usize].b16.s0 = NORMAL as u16;
    MEM[(cur_list.tail + 1) as usize].b32 = empty;
    MEM[(cur_list.tail + 3) as usize].b32 = empty;
    MEM[(cur_list.tail + 2) as usize].b32 = empty;
    MEM[(cur_list.tail + 4) as usize].b32.s1 = MATH_CHAR;
    if cur_chr == 1i32 {
        if scan_keyword(b"fixed") {
            MEM[cur_list.tail as usize].b16.s0 = FIXED_ACC as u16;
        } else if scan_keyword(b"bottom") {
            if scan_keyword(b"fixed") {
                MEM[cur_list.tail as usize].b16.s0 = (BOTTOM_ACC + 1) as u16;
            } else {
                MEM[cur_list.tail as usize].b16.s0 = BOTTOM_ACC as u16;
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
    MEM[(cur_list.tail + 4) as usize].b16.s0 = (cur_val as i64 % 65536) as u16;
    if cur_val as u32 >> 21i32 & 0x7_u32 == 7_u32
        && (*INTPAR(IntPar::cur_fam) >= 0i32 && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES)
    {
        MEM[(cur_list.tail + 4) as usize].b16.s1 = *INTPAR(IntPar::cur_fam) as u16
    } else {
        MEM[(cur_list.tail + 4) as usize].b16.s1 = (cur_val as u32 >> 24 & 0xff_u32) as u16
    }
    MEM[(cur_list.tail + 4) as usize].b16.s1 = (MEM[(cur_list.tail + 4) as usize].b16.s1 as i64
        + (cur_val as u32 & 0x1fffff_u32) as i64 / 65536 * 256i32 as i64)
        as u16;
    scan_math(cur_list.tail + 1i32);
}
pub(crate) unsafe fn append_choices() {
    MEM[cur_list.tail as usize].b32.s1 = new_choice();
    cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
    SAVE_PTR += 1;
    SAVE_STACK[SAVE_PTR - 1].b32.s1 = 0;
    push_math(MATH_CHOICE_GROUP as group_code);
    scan_left_brace();
}
pub(crate) unsafe fn fin_mlist(mut p: i32) -> i32 {
    let mut q: i32 = 0;
    if cur_list.aux.b32.s1 != TEX_NULL {
        /*1220: */
        MEM[(cur_list.aux.b32.s1 + 3) as usize].b32.s1 = SUB_MLIST;
        MEM[(cur_list.aux.b32.s1 + 3) as usize].b32.s0 = MEM[cur_list.head as usize].b32.s1;
        if p == TEX_NULL {
            q = cur_list.aux.b32.s1
        } else {
            q = MEM[(cur_list.aux.b32.s1 + 2) as usize].b32.s0;
            if MEM[q as usize].b16.s1 as i32 != 30 || cur_list.eTeX_aux == TEX_NULL {
                confusion(b"right");
            }
            MEM[(cur_list.aux.b32.s1 + 2) as usize].b32.s0 = MEM[cur_list.eTeX_aux as usize].b32.s1;
            MEM[cur_list.eTeX_aux as usize].b32.s1 = cur_list.aux.b32.s1;
            MEM[cur_list.aux.b32.s1 as usize].b32.s1 = p
        }
    } else {
        MEM[cur_list.tail as usize].b32.s1 = p;
        q = MEM[cur_list.head as usize].b32.s1
    }
    pop_nest();
    q
}
pub(crate) unsafe fn build_choices() {
    let mut p: i32 = 0;
    unsave();
    p = fin_mlist(TEX_NULL);
    match SAVE_STACK[SAVE_PTR - 1].b32.s1 {
        0 => MEM[(cur_list.tail + 1) as usize].b32.s0 = p,
        1 => MEM[(cur_list.tail + 1) as usize].b32.s1 = p,
        2 => MEM[(cur_list.tail + 2) as usize].b32.s0 = p,
        3 => {
            MEM[(cur_list.tail + 2) as usize].b32.s1 = p;
            SAVE_PTR -= 1;
            return;
        }
        _ => {}
    }
    SAVE_STACK[SAVE_PTR - 1].b32.s1 += 1;
    push_math(MATH_CHOICE_GROUP as group_code);
    scan_left_brace();
}
pub(crate) unsafe fn sub_sup() {
    let mut t: small_number = 0;
    let mut p: i32 = 0;
    t = EMPTY as small_number;
    p = TEX_NULL;
    if cur_list.tail != cur_list.head {
        if MEM[cur_list.tail as usize].b16.s1 as i32 >= ORD_NOAD
            && (MEM[cur_list.tail as usize].b16.s1 as i32) < LEFT_NOAD
        {
            p = cur_list.tail + 2i32 + cur_cmd as i32 - 7i32;
            t = MEM[p as usize].b32.s1 as small_number
        }
    }
    if p == TEX_NULL || t as i32 != EMPTY {
        /*1212: */
        MEM[cur_list.tail as usize].b32.s1 = new_noad();
        cur_list.tail = *LLIST_link(cur_list.tail as isize);
        p = cur_list.tail + 2 + cur_cmd as i32 - 7;
        if t as i32 != EMPTY {
            if cur_cmd as u16 == SUP_MARK {
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
    }
    scan_math(p);
}
pub(crate) unsafe fn math_fraction() {
    let mut c: small_number = 0;
    c = cur_chr as small_number;
    if cur_list.aux.b32.s1 != TEX_NULL {
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
        cur_list.aux.b32.s1 = get_node(FRACTION_NOAD_SIZE);
        MEM[cur_list.aux.b32.s1 as usize].b16.s1 = FRACTION_NOAD as u16;
        MEM[cur_list.aux.b32.s1 as usize].b16.s0 = NORMAL as u16;
        MEM[(cur_list.aux.b32.s1 + 2) as usize].b32.s1 = SUB_MLIST;
        MEM[(cur_list.aux.b32.s1 + 2) as usize].b32.s0 = MEM[cur_list.head as usize].b32.s1;
        MEM[(cur_list.aux.b32.s1 + 3) as usize].b32 = empty;
        MEM[(cur_list.aux.b32.s1 + 4) as usize].b16 = null_delimiter;
        MEM[(cur_list.aux.b32.s1 + 5) as usize].b16 = null_delimiter;
        MEM[cur_list.head as usize].b32.s1 = TEX_NULL;
        cur_list.tail = cur_list.head;
        if c as i32 >= DELIMITED_CODE {
            scan_delimiter(cur_list.aux.b32.s1 + 4i32, false);
            scan_delimiter(cur_list.aux.b32.s1 + 5i32, false);
        }
        match c as i32 % DELIMITED_CODE {
            ABOVE_CODE => {
                scan_dimen(false, false, false);
                MEM[(cur_list.aux.b32.s1 + 1) as usize].b32.s1 = cur_val
            }
            OVER_CODE => MEM[(cur_list.aux.b32.s1 + 1) as usize].b32.s1 = DEFAULT_CODE,
            ATOP_CODE => MEM[(cur_list.aux.b32.s1 + 1) as usize].b32.s1 = 0,
            _ => {}
        }
    };
}
pub(crate) unsafe fn math_left_right() {
    let mut t: small_number = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    t = cur_chr as small_number;
    if t as i32 != LEFT_NOAD && cur_group as i32 != MATH_LEFT_GROUP {
        /*1227: */
        if cur_group as i32 == MATH_SHIFT_GROUP {
            scan_delimiter(GARBAGE, false); /*:1530 */
            if file_line_error_style_p != 0 {
                print_file_line(); /*:1530 */
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Extra ");
            if t as i32 == 1i32 {
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
        p = new_noad();
        MEM[p as usize].b16.s1 = t as u16;
        scan_delimiter(p + 1i32, false);
        if t as i32 == 1i32 {
            MEM[p as usize].b16.s1 = RIGHT_NOAD as u16;
            MEM[p as usize].b16.s0 = 1;
        }
        if t as i32 == LEFT_NOAD {
            q = p
        } else {
            q = fin_mlist(p);
            unsave();
        }
        if t as i32 != RIGHT_NOAD {
            push_math(MATH_LEFT_GROUP as group_code);
            MEM[cur_list.head as usize].b32.s1 = q;
            cur_list.tail = p;
            cur_list.eTeX_aux = p
        } else {
            MEM[cur_list.tail as usize].b32.s1 = new_noad();
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
            MEM[cur_list.tail as usize].b16.s1 = INNER_NOAD as u16;
            MEM[(cur_list.tail + 1) as usize].b32.s1 = SUB_MLIST;
            MEM[(cur_list.tail + 1) as usize].b32.s0 = q
        }
    };
}
unsafe extern "C" fn app_display(mut j: i32, mut b: i32, mut d: scaled_t) {
    let mut z: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut e: scaled_t = 0;
    let mut x: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut t: i32 = 0;
    let mut u: i32 = 0;
    s = *DIMENPAR(DimenPar::display_indent);
    x = *INTPAR(IntPar::pre_display_correction);
    if x == 0i32 {
        MEM[(b + 4) as usize].b32.s1 = s + d
    } else {
        z = *DIMENPAR(DimenPar::display_width);
        p = b;
        if x > 0i32 {
            e = z - d - MEM[(p + 1) as usize].b32.s1
        } else {
            e = d;
            d = z - e - MEM[(p + 1) as usize].b32.s1
        }
        if j != TEX_NULL {
            b = copy_node_list(j);
            MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
            MEM[(b + 2) as usize].b32.s1 = MEM[(p + 2) as usize].b32.s1;
            s = s - MEM[(b + 4) as usize].b32.s1;
            d = d + s;
            e = e + MEM[(b + 1) as usize].b32.s1 - z - s
        }
        if MEM[p as usize].b16.s0 as i32 == 2 {
            q = p
        } else {
            r = MEM[(p + 5) as usize].b32.s1;
            free_node(p, BOX_NODE_SIZE);
            if r == TEX_NULL {
                confusion(b"LR4");
            }
            if x > 0i32 {
                p = r;
                loop {
                    q = r;
                    r = *LLIST_link(r as isize);
                    if r == TEX_NULL {
                        break;
                    }
                }
            } else {
                p = TEX_NULL;
                q = r;
                loop {
                    t = MEM[r as usize].b32.s1;
                    MEM[r as usize].b32.s1 = p;
                    p = r;
                    r = t;
                    if r == TEX_NULL {
                        break;
                    }
                }
            }
        }
        if j == TEX_NULL {
            r = new_kern(0);
            t = new_kern(0)
        } else {
            r = MEM[(b + 5) as usize].b32.s1;
            t = MEM[r as usize].b32.s1
        }
        u = new_math(0i32, END_M_CODE as small_number);
        if MEM[t as usize].b16.s1 == GLUE_NODE {
            j = new_skip_param(GluePar::right_skip as small_number);
            MEM[q as usize].b32.s1 = j;
            MEM[j as usize].b32.s1 = u;
            j = MEM[(t + 1) as usize].b32.s0;
            MEM[temp_ptr as usize].b16.s1 = MEM[j as usize].b16.s1;
            MEM[temp_ptr as usize].b16.s0 = MEM[j as usize].b16.s0;
            MEM[(temp_ptr + 1) as usize].b32.s1 = e - MEM[(j + 1) as usize].b32.s1;
            MEM[(temp_ptr + 2) as usize].b32.s1 = -MEM[(j + 2) as usize].b32.s1;
            MEM[(temp_ptr + 3) as usize].b32.s1 = -MEM[(j + 3) as usize].b32.s1;
            MEM[u as usize].b32.s1 = t
        } else {
            MEM[(t + 1) as usize].b32.s1 = e;
            MEM[t as usize].b32.s1 = u;
            MEM[q as usize].b32.s1 = t
        }
        u = new_math(0, BEGIN_M_CODE as small_number);
        if MEM[r as usize].b16.s1 == GLUE_NODE {
            j = new_skip_param(GluePar::left_skip as small_number);
            MEM[u as usize].b32.s1 = j;
            MEM[j as usize].b32.s1 = p;
            j = MEM[(r + 1) as usize].b32.s0;
            MEM[temp_ptr as usize].b16.s1 = MEM[j as usize].b16.s1;
            MEM[temp_ptr as usize].b16.s0 = MEM[j as usize].b16.s0;
            MEM[(temp_ptr + 1) as usize].b32.s1 = d - MEM[(j + 1) as usize].b32.s1;
            MEM[(temp_ptr + 2) as usize].b32.s1 = -MEM[(j + 2) as usize].b32.s1;
            MEM[(temp_ptr + 3) as usize].b32.s1 = -MEM[(j + 3) as usize].b32.s1;
            MEM[r as usize].b32.s1 = u
        } else {
            MEM[(r + 1) as usize].b32.s1 = d;
            MEM[r as usize].b32.s1 = p;
            MEM[u as usize].b32.s1 = r;
            if j == TEX_NULL {
                b = hpack(u, 0i32, ADDITIONAL as small_number);
                MEM[(b + 4) as usize].b32.s1 = s
            } else {
                MEM[(b + 5) as usize].b32.s1 = u
            }
        }
    }
    append_to_vlist(b);
}
pub(crate) unsafe fn after_math() {
    let mut l: bool = false;
    let mut danger: bool = false;
    let mut m: i32 = 0;
    let mut p: i32 = 0;
    let mut a: i32 = 0;
    let mut b: i32 = 0;
    let mut w: scaled_t = 0;
    let mut z: scaled_t = 0;
    let mut e: scaled_t = 0;
    let mut q: scaled_t = 0;
    let mut d: scaled_t = 0;
    let mut s: scaled_t = 0;
    let mut g1: small_number = 0;
    let mut g2: small_number = 0;
    let mut r: i32 = 0;
    let mut t: i32 = 0;
    let mut pre_t: i32 = 0;
    let mut j: i32 = TEX_NULL;

    danger = false;

    if cur_list.mode as i32 == MMODE {
        j = cur_list.eTeX_aux; // :1530
    }
    if FONT_PARAMS[*MATH_FONT(2) as usize] < TOTAL_MATHSY_PARAMS
        && !(FONT_AREA[*MATH_FONT(2) as usize] as u32 == OTGR_FONT_FLAG
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[*MATH_FONT(2) as usize] as XeTeXLayoutEngine)
                as i32
                != 0)
        || FONT_PARAMS[*MATH_FONT(2 + SCRIPT_SIZE) as usize] < TOTAL_MATHSY_PARAMS
            && !(FONT_AREA[*MATH_FONT(2 + SCRIPT_SIZE) as usize] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(2 + SCRIPT_SIZE) as usize] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
        || FONT_PARAMS[*MATH_FONT(2 + SCRIPT_SCRIPT_SIZE) as usize] < TOTAL_MATHSY_PARAMS
            && !(FONT_AREA[*MATH_FONT(2 + SCRIPT_SCRIPT_SIZE) as usize] as u32 == OTGR_FONT_FLAG
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(2 + SCRIPT_SCRIPT_SIZE) as usize]
                        as XeTeXLayoutEngine,
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
    } else if FONT_PARAMS[*MATH_FONT(3i32 + 0i32) as usize] < TOTAL_MATHEX_PARAMS
        && !(FONT_AREA[*MATH_FONT(3i32 + 0i32) as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(
                FONT_LAYOUT_ENGINE[*MATH_FONT(3i32 + 0i32) as usize] as XeTeXLayoutEngine,
            ) as i32
                != 0)
        || FONT_PARAMS[*MATH_FONT(3i32 + 256i32) as usize] < TOTAL_MATHEX_PARAMS
            && !(FONT_AREA[*MATH_FONT(3i32 + 256i32) as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(3i32 + 256i32) as usize] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
        || FONT_PARAMS[*MATH_FONT(3i32 + 2i32 * 256i32) as usize] < TOTAL_MATHEX_PARAMS
            && !(FONT_AREA[*MATH_FONT(3i32 + 2i32 * 256i32) as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(3i32 + 2i32 * 256i32) as usize]
                        as XeTeXLayoutEngine,
                ) as i32
                    != 0)
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Math formula deleted: Insufficient extension fonts");
        help_ptr = 3_u8;
        help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 3";
        help_line[1] = b"and \\scriptfont 3 and \\scriptscriptfont 3 have all";
        help_line[0] = b"the \\fontdimen values needed in math extension fonts.";
        error();
        flush_math();
        danger = true
    }
    m = cur_list.mode as i32;
    l = false;
    p = fin_mlist(TEX_NULL);
    if cur_list.mode as i32 == -m {
        get_x_token();
        if cur_cmd as i32 != 3i32 {
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
        cur_mlist = p;
        cur_style = 2i32 as small_number;
        mlist_penalties = false;
        mlist_to_hlist();
        a = hpack(
            MEM[(4999999 - 3) as usize].b32.s1,
            0i32,
            1i32 as small_number,
        );
        MEM[a as usize].b16.s0 = 2_u16;
        unsave();
        SAVE_PTR -= 1;
        if SAVE_STACK[SAVE_PTR + 0].b32.s1 == 1 {
            l = true
        }
        danger = false;
        if cur_list.mode as i32 == 207i32 {
            j = cur_list.eTeX_aux
        }
        if FONT_PARAMS[*MATH_FONT(2) as usize] < 22i32
            && !(FONT_AREA[*MATH_FONT(2) as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(2) as usize] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
            || FONT_PARAMS[*MATH_FONT(2i32 + 256i32) as usize] < 22i32
                && !(FONT_AREA[*MATH_FONT(2i32 + 256i32) as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[*MATH_FONT(2i32 + 256i32) as usize] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
            || FONT_PARAMS[*MATH_FONT(2i32 + 2i32 * 256i32) as usize] < 22i32
                && !(FONT_AREA[*MATH_FONT(2i32 + 2i32 * 256i32) as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[*MATH_FONT(2i32 + 2i32 * 256i32) as usize]
                            as XeTeXLayoutEngine,
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
        } else if FONT_PARAMS[*MATH_FONT(3i32 + 0i32) as usize] < 13i32
            && !(FONT_AREA[*MATH_FONT(3i32 + 0i32) as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(
                    FONT_LAYOUT_ENGINE[*MATH_FONT(3i32 + 0i32) as usize] as XeTeXLayoutEngine,
                ) as i32
                    != 0)
            || FONT_PARAMS[*MATH_FONT(3i32 + 256i32) as usize] < 13i32
                && !(FONT_AREA[*MATH_FONT(3i32 + 256i32) as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[EQTB[(1i32
                            + (0x10ffffi32 + 1i32)
                            + (0x10ffffi32 + 1i32)
                            + 1i32
                            + 15000i32
                            + 12i32
                            + 9000i32
                            + 1i32
                            + 1i32
                            + 19i32
                            + 256i32
                            + 256i32
                            + 13i32
                            + 256i32
                            + 4i32
                            + 256i32
                            + 1i32
                            + (3i32 + 256i32))
                            as usize]
                            .b32
                            .s1 as usize] as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
            || FONT_PARAMS[*MATH_FONT(3i32 + 2i32 * 256i32) as usize] < 13i32
                && !(FONT_AREA[*MATH_FONT(3i32 + 2i32 * 256i32) as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(
                        FONT_LAYOUT_ENGINE[*MATH_FONT(3i32 + 2i32 * 256i32) as usize]
                            as XeTeXLayoutEngine,
                    ) as i32
                        != 0)
        {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Math formula deleted: Insufficient extension fonts");
            help_ptr = 3_u8;
            help_line[2] = b"Sorry, but I can\'t typeset math unless \\textfont 3";
            help_line[1] = b"and \\scriptfont 3 and \\scriptscriptfont 3 have all";
            help_line[0] = b"the \\fontdimen values needed in math extension fonts.";
            error();
            flush_math();
            danger = true
        }
        m = cur_list.mode as i32;
        p = fin_mlist(TEX_NULL)
    } else {
        a = TEX_NULL
    }
    if m < 0i32 {
        /*1231: */
        MEM[cur_list.tail as usize].b32.s1 =
            new_math(*DIMENPAR(DimenPar::math_surround), 0i32 as small_number);
        cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
        cur_mlist = p;
        cur_style = 2i32 as small_number;
        mlist_penalties = cur_list.mode as i32 > 0i32;
        mlist_to_hlist();
        MEM[cur_list.tail as usize].b32.s1 = MEM[(4999999 - 3) as usize].b32.s1;
        while MEM[cur_list.tail as usize].b32.s1 != -0xfffffff {
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1
        }
        MEM[cur_list.tail as usize].b32.s1 =
            new_math(*DIMENPAR(DimenPar::math_surround), 1i32 as small_number);
        cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
        cur_list.aux.b32.s0 = 1000i32;
        unsave();
    } else {
        if a == TEX_NULL {
            /*1232: */
            get_x_token();
            if cur_cmd as i32 != 3i32 {
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
        cur_style = 0i32 as small_number;
        mlist_penalties = false;
        mlist_to_hlist();
        p = MEM[(4999999 - 3) as usize].b32.s1;
        adjust_tail = 4999999i32 - 5i32;
        pre_adjust_tail = 4999999i32 - 14i32;
        b = hpack(p, 0i32, 1i32 as small_number);
        p = MEM[(b + 5) as usize].b32.s1;
        t = adjust_tail;
        adjust_tail = TEX_NULL;
        pre_t = pre_adjust_tail;
        pre_adjust_tail = TEX_NULL;
        w = MEM[(b + 1) as usize].b32.s1;
        z = *DIMENPAR(DimenPar::display_width);
        s = *DIMENPAR(DimenPar::display_indent);
        if *INTPAR(IntPar::pre_display_correction) < 0i32 {
            s = -s - z
        }
        if a == TEX_NULL || danger as i32 != 0 {
            e = 0i32;
            q = 0i32
        } else {
            e = MEM[(a + 1) as usize].b32.s1;
            q = e + math_quad(0i32)
        }
        if w + q > z {
            /*1236: */
            if e != 0i32
                && (w - total_shrink[0] + q <= z
                    || total_shrink[1] != 0i32
                    || total_shrink[2] != 0i32
                    || total_shrink[3] != 0i32)
            {
                free_node(b, 8i32);
                b = hpack(p, z - q, 0i32 as small_number)
            } else {
                e = 0i32;
                if w > z {
                    free_node(b, 8i32);
                    b = hpack(p, z, 0i32 as small_number)
                }
            }
            w = MEM[(b + 1) as usize].b32.s1
        }
        MEM[b as usize].b16.s0 = 2_u16;
        d = half(z - w);
        if e > 0i32 && d < 2i32 * e {
            d = half(z - w - e);
            if p != TEX_NULL {
                if !is_char_node(p) {
                    if MEM[p as usize].b16.s1 as i32 == 10 {
                        d = 0i32
                    }
                }
            }
        }
        MEM[cur_list.tail as usize].b32.s1 = new_penalty(*INTPAR(IntPar::pre_display_penalty));
        cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
        if d + s <= *DIMENPAR(DimenPar::pre_display_size) || l as i32 != 0 {
            g1 = 3i32 as small_number;
            g2 = 4i32 as small_number
        } else {
            g1 = 5i32 as small_number;
            g2 = 6i32 as small_number
        }
        if l as i32 != 0 && e == 0i32 {
            app_display(j, a, 0i32);
            MEM[cur_list.tail as usize].b32.s1 = new_penalty(10000);
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1
        } else {
            MEM[cur_list.tail as usize].b32.s1 = new_param_glue(g1);
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1
        }
        if e != 0i32 {
            r = new_kern(z - w - e - d);
            if l {
                MEM[a as usize].b32.s1 = r;
                MEM[r as usize].b32.s1 = b;
                b = a;
                d = 0i32
            } else {
                MEM[b as usize].b32.s1 = r;
                MEM[r as usize].b32.s1 = a
            }
            b = hpack(b, 0i32, 1i32 as small_number)
        }
        app_display(j, b, d);
        if a != TEX_NULL && e == 0i32 && !l {
            MEM[cur_list.tail as usize].b32.s1 = new_penalty(10000);
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
            app_display(j, a, z - MEM[(a + 1) as usize].b32.s1);
            g2 = 0i32 as small_number
        }
        if t != 4999999i32 - 5i32 {
            MEM[cur_list.tail as usize].b32.s1 = MEM[(4999999 - 5) as usize].b32.s1;
            cur_list.tail = t
        }
        if pre_t != 4999999i32 - 14i32 {
            MEM[cur_list.tail as usize].b32.s1 = MEM[(4999999 - 14) as usize].b32.s1;
            cur_list.tail = pre_t
        }
        MEM[cur_list.tail as usize].b32.s1 = new_penalty(*INTPAR(IntPar::post_display_penalty));
        cur_list.tail = MEM[cur_list.tail as usize].b32.s1;
        if g2 as i32 > 0i32 {
            MEM[cur_list.tail as usize].b32.s1 = new_param_glue(g2);
            cur_list.tail = MEM[cur_list.tail as usize].b32.s1
        }
        flush_node_list(j);
        resume_after_display();
    };
}
pub(crate) unsafe fn resume_after_display() {
    if cur_group as i32 != 15i32 {
        confusion(b"display");
    }
    unsave();
    cur_list.prev_graf = cur_list.prev_graf + 3i32;
    push_nest();
    cur_list.mode = 104_i16;
    cur_list.aux.b32.s0 = 1000i32;
    if *INTPAR(IntPar::language) <= 0i32 {
        cur_lang = 0_u8
    } else if *INTPAR(IntPar::language) > 255i32 {
        cur_lang = 0_u8
    } else {
        cur_lang = *INTPAR(IntPar::language) as u8
    }
    cur_list.aux.b32.s1 = cur_lang as i32;
    cur_list.prev_graf = ((norm_min(*INTPAR(IntPar::left_hyphen_min)) as i32 * 64i32
        + norm_min(*INTPAR(IntPar::right_hyphen_min)) as i32) as i64
        * 65536
        + cur_lang as i64) as i32;
    get_x_token();
    if cur_cmd as i32 != 10i32 {
        back_input();
    }
    if nest_ptr == 1i32 {
        build_page();
    };
}
/* Copyright 2016-2018 The Tectonic Project
 * Licensed under the MIT License.
 */
unsafe extern "C" fn math_x_height(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 5i32)
    } else {
        rval = FONT_INFO[(5 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn math_quad(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 6i32)
    } else {
        rval = FONT_INFO[(6 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn num1(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 8i32)
    } else {
        rval = FONT_INFO[(8 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn num2(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 9i32)
    } else {
        rval = FONT_INFO[(9 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn num3(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 10i32)
    } else {
        rval = FONT_INFO[(10 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn denom1(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 11i32)
    } else {
        rval = FONT_INFO[(11 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn denom2(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 12i32)
    } else {
        rval = FONT_INFO[(12 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sup1(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 13i32)
    } else {
        rval = FONT_INFO[(13 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sup2(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 14i32)
    } else {
        rval = FONT_INFO[(14 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sup3(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 15i32)
    } else {
        rval = FONT_INFO[(15 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sub1(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 16i32)
    } else {
        rval = FONT_INFO[(16 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sub2(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 17i32)
    } else {
        rval = FONT_INFO[(17 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sup_drop(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 18i32)
    } else {
        rval = FONT_INFO[(18 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn sub_drop(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 19i32)
    } else {
        rval = FONT_INFO[(19 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn delim1(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 20i32)
    } else {
        rval = FONT_INFO[(20 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn delim2(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 21i32)
    } else {
        rval = FONT_INFO[(21 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn axis_height(mut size_code: i32) -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(2 + size_code);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathsy_param(f, 22i32)
    } else {
        rval = FONT_INFO[(22 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn default_rule_thickness() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 8i32)
    } else {
        rval = FONT_INFO[(8 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn big_op_spacing1() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 9i32)
    } else {
        rval = FONT_INFO[(9 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn big_op_spacing2() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 10i32)
    } else {
        rval = FONT_INFO[(10 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn big_op_spacing3() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 11i32)
    } else {
        rval = FONT_INFO[(11 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn big_op_spacing4() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 12i32)
    } else {
        rval = FONT_INFO[(12 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn big_op_spacing5() -> scaled_t {
    let mut f: i32 = 0;
    let mut rval: scaled_t = 0;
    f = *MATH_FONT(3 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rval = get_native_mathex_param(f, 13i32)
    } else {
        rval = FONT_INFO[(13 + PARAM_BASE[f as usize]) as usize].b32.s1
    }
    rval
}
unsafe extern "C" fn fraction_rule(mut t: scaled_t) -> i32 {
    let mut p: i32 = 0;
    p = new_rule();
    MEM[(p + 3) as usize].b32.s1 = t;
    MEM[(p + 2) as usize].b32.s1 = 0;
    p
}
unsafe extern "C" fn overbar(mut b: i32, mut k: scaled_t, mut t: scaled_t) -> i32 {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    p = new_kern(k);
    MEM[p as usize].b32.s1 = b;
    q = fraction_rule(t);
    MEM[q as usize].b32.s1 = p;
    p = new_kern(t);
    MEM[p as usize].b32.s1 = q;
    vpackage(p, 0i32, 1i32 as small_number, 0x3fffffffi32)
}
unsafe extern "C" fn math_glue(mut g: i32, mut m: scaled_t) -> i32 {
    let mut p: i32 = 0;
    let mut n: i32 = 0;
    let mut f: scaled_t = 0;
    n = x_over_n(m, 65536 as i32);
    f = tex_remainder;
    if f < 0i32 {
        n -= 1;
        f = (f as i64 + 65536) as scaled_t
    }
    p = get_node(4i32);
    MEM[(p + 1) as usize].b32.s1 = mult_and_add(
        n,
        MEM[(g + 1) as usize].b32.s1,
        xn_over_d(MEM[(g + 1) as usize].b32.s1, f, 65536 as i32),
        0x3fffffffi32,
    );
    MEM[p as usize].b16.s1 = MEM[g as usize].b16.s1;
    if MEM[p as usize].b16.s1 as i32 == 0 {
        MEM[(p + 2) as usize].b32.s1 = mult_and_add(
            n,
            MEM[(g + 2) as usize].b32.s1,
            xn_over_d(MEM[(g + 2) as usize].b32.s1, f, 65536 as i32),
            0x3fffffffi32,
        )
    } else {
        MEM[(p + 2) as usize].b32.s1 = MEM[(g + 2) as usize].b32.s1
    }
    MEM[p as usize].b16.s0 = MEM[g as usize].b16.s0;
    if MEM[p as usize].b16.s0 as i32 == 0 {
        MEM[(p + 3) as usize].b32.s1 = mult_and_add(
            n,
            MEM[(g + 3) as usize].b32.s1,
            xn_over_d(MEM[(g + 3) as usize].b32.s1, f, 65536 as i32),
            0x3fffffffi32,
        )
    } else {
        MEM[(p + 3) as usize].b32.s1 = MEM[(g + 3) as usize].b32.s1
    }
    p
}
unsafe extern "C" fn math_kern(mut p: i32, mut m: scaled_t) {
    let mut n: i32 = 0;
    let mut f: scaled_t = 0;
    if MEM[p as usize].b16.s0 as i32 == 99 {
        n = x_over_n(m, 65536 as i32);
        f = tex_remainder;
        if f < 0i32 {
            n -= 1;
            f = (f as i64 + 65536) as scaled_t
        }
        MEM[(p + 1) as usize].b32.s1 = mult_and_add(
            n,
            MEM[(p + 1) as usize].b32.s1,
            xn_over_d(MEM[(p + 1) as usize].b32.s1, f, 65536 as i32),
            0x3fffffffi32,
        );
        MEM[p as usize].b16.s0 = 1_u16
    };
}
pub(crate) unsafe fn flush_math() {
    flush_node_list(MEM[cur_list.head as usize].b32.s1);
    flush_node_list(cur_list.aux.b32.s1);
    MEM[cur_list.head as usize].b32.s1 = -0xfffffff;
    cur_list.tail = cur_list.head;
    cur_list.aux.b32.s1 = TEX_NULL;
}
unsafe extern "C" fn clean_box(mut p: i32, mut s: small_number) -> i32 {
    let mut current_block: u64;
    let mut q: i32 = 0;
    let mut save_style: small_number = 0;
    let mut x: i32 = 0;
    let mut r: i32 = 0;
    match MEM[p as usize].b32.s1 {
        1 => {
            cur_mlist = new_noad();
            MEM[(cur_mlist + 1) as usize] = MEM[p as usize];
            current_block = 12209867499936983673;
        }
        2 => {
            q = MEM[p as usize].b32.s0;
            current_block = 3089856285952541829;
        }
        3 => {
            cur_mlist = MEM[p as usize].b32.s0;
            current_block = 12209867499936983673;
        }
        _ => {
            q = new_null_box();
            current_block = 3089856285952541829;
        }
    }
    match current_block {
        12209867499936983673 => {
            save_style = cur_style;
            cur_style = s;
            mlist_penalties = false;
            mlist_to_hlist();
            q = MEM[(4999999 - 3) as usize].b32.s1;
            cur_style = save_style;
            if (cur_style as i32) < 4i32 {
                cur_size = 0i32
            } else {
                cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
            }
            cur_mu = x_over_n(math_quad(cur_size), 18i32)
        }
        _ => {}
    }
    if is_char_node(q) as i32 != 0 || q == TEX_NULL {
        x = hpack(q, 0i32, 1i32 as small_number)
    } else if MEM[q as usize].b32.s1 == -0xfffffff
        && MEM[q as usize].b16.s1 as i32 <= 1
        && MEM[(q + 4) as usize].b32.s1 == 0
    {
        x = q
    } else {
        x = hpack(q, 0i32, 1i32 as small_number)
    }
    q = MEM[(x + 5) as usize].b32.s1;
    if is_char_node(q) {
        r = MEM[q as usize].b32.s1;
        if r != TEX_NULL {
            if MEM[r as usize].b32.s1 == -0xfffffff {
                if !is_char_node(r) {
                    if MEM[r as usize].b16.s1 as i32 == 11 {
                        free_node(r, 3i32);
                        MEM[q as usize].b32.s1 = -0xfffffff
                    }
                }
            }
        }
    }
    x
}
unsafe extern "C" fn fetch(mut a: i32) {
    cur_c = MEM[a as usize].b16.s0 as i32;
    cur_f = *MATH_FONT(MEM[a as usize].b16.s1 as i32 % 256 + cur_size);
    cur_c = (cur_c as i64 + (MEM[a as usize].b16.s1 as i32 / 256) as i64 * 65536) as i32;
    if cur_f == 0i32 {
        /*749: */
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"");
        print_size(cur_size);
        print_char(' ' as i32);
        print_int(MEM[a as usize].b16.s1 as i32 % 256);
        print_cstr(b" is undefined (character ");
        print(cur_c);
        print_char(')' as i32);
        help_ptr = 4_u8;
        help_line[3] = b"Somewhere in the math formula just ended, you used the";
        help_line[2] = b"stated character from an undefined font family. For example,";
        help_line[1] = b"plain TeX doesn\'t allow \\it or \\sl in subscripts. Proceed,";
        help_line[0] = b"and I\'ll try to forget that I needed that character.";
        error();
        cur_i = null_character;
        MEM[a as usize].b32.s1 = 0
    } else if FONT_AREA[cur_f as usize] as u32 == 0xffffu32
        || FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
    {
        cur_i = null_character
    } else {
        if cur_c >= FONT_BC[cur_f as usize] as i32 && cur_c <= FONT_EC[cur_f as usize] as i32 {
            cur_i = FONT_INFO[(CHAR_BASE[cur_f as usize] + cur_c) as usize].b16
        } else {
            cur_i = null_character
        }
        if !(cur_i.s3 as i32 > 0i32) {
            char_warning(cur_f, cur_c);
            MEM[a as usize].b32.s1 = 0
        }
    };
}
unsafe extern "C" fn make_over(mut q: i32) {
    MEM[(q + 1) as usize].b32.s0 = overbar(
        clean_box(
            q + 1i32,
            (2i32 * (cur_style as i32 / 2i32) + 1i32) as small_number,
        ),
        3i32 * default_rule_thickness(),
        default_rule_thickness(),
    );
    MEM[(q + 1) as usize].b32.s1 = 2;
}
unsafe extern "C" fn make_under(mut q: i32) {
    let mut p: i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut delta: scaled_t = 0;
    x = clean_box(q + 1i32, cur_style);
    p = new_kern(3i32 * default_rule_thickness());
    MEM[x as usize].b32.s1 = p;
    MEM[p as usize].b32.s1 = fraction_rule(default_rule_thickness());
    y = vpackage(x, 0i32, 1i32 as small_number, 0x3fffffffi32);
    delta = MEM[(y + 3) as usize].b32.s1 + MEM[(y + 2) as usize].b32.s1 + default_rule_thickness();
    MEM[(y + 3) as usize].b32.s1 = MEM[(x + 3) as usize].b32.s1;
    MEM[(y + 2) as usize].b32.s1 = delta - MEM[(y + 3) as usize].b32.s1;
    MEM[(q + 1) as usize].b32.s0 = y;
    MEM[(q + 1) as usize].b32.s1 = 2;
}
unsafe extern "C" fn make_vcenter(mut q: i32) {
    let mut v: i32 = 0;
    let mut delta: scaled_t = 0;
    v = MEM[(q + 1) as usize].b32.s0;
    if MEM[v as usize].b16.s1 as i32 != 1 {
        confusion(b"vcenter");
    }
    delta = MEM[(v + 3) as usize].b32.s1 + MEM[(v + 2) as usize].b32.s1;
    MEM[(v + 3) as usize].b32.s1 = axis_height(cur_size) + half(delta);
    MEM[(v + 2) as usize].b32.s1 = delta - MEM[(v + 3) as usize].b32.s1;
}
unsafe extern "C" fn make_radical(mut q: i32) {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut rule_thickness: scaled_t = 0;
    let mut delta: scaled_t = 0;
    let mut clr: scaled_t = 0;
    f = *MATH_FONT(MEM[(q + 4) as usize].b16.s3 as i32 % 256 + cur_size);
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        rule_thickness = get_ot_math_constant(f, 51i32)
    } else {
        rule_thickness = default_rule_thickness()
    }
    x = clean_box(
        q + 1i32,
        (2i32 * (cur_style as i32 / 2i32) + 1i32) as small_number,
    );
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        if (cur_style as i32) < 2i32 {
            clr = get_ot_math_constant(f, 50i32)
        } else {
            clr = get_ot_math_constant(f, 49i32)
        }
    } else if (cur_style as i32) < 2i32 {
        clr = rule_thickness + math_x_height(cur_size).abs() / 4i32
    } else {
        clr = rule_thickness;
        clr = clr + clr.abs() / 4i32
    }
    y = var_delimiter(
        q + 4i32,
        cur_size,
        MEM[(x + 3) as usize].b32.s1 + MEM[(x + 2) as usize].b32.s1 + clr + rule_thickness,
    );
    if FONT_AREA[f as usize] as u32 == 0xfffeu32
        && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
    {
        MEM[(y + 2) as usize].b32.s1 =
            MEM[(y + 3) as usize].b32.s1 + MEM[(y + 2) as usize].b32.s1 - rule_thickness;
        MEM[(y + 3) as usize].b32.s1 = rule_thickness
    }
    delta = MEM[(y + 2) as usize].b32.s1
        - (MEM[(x + 3) as usize].b32.s1 + MEM[(x + 2) as usize].b32.s1 + clr);
    if delta > 0i32 {
        clr = clr + half(delta)
    }
    MEM[(y + 4) as usize].b32.s1 = -(MEM[(x + 3) as usize].b32.s1 + clr);
    MEM[y as usize].b32.s1 = overbar(x, clr, MEM[(y + 3) as usize].b32.s1);
    MEM[(q + 1) as usize].b32.s0 = hpack(y, 0, 1 as small_number);
    MEM[(q + 1) as usize].b32.s1 = 2;
}
unsafe extern "C" fn compute_ot_math_accent_pos(mut p: i32) -> scaled_t {
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: scaled_t = 0;
    let mut g: scaled_t = 0;
    if MEM[(p + 1) as usize].b32.s1 == 1 {
        fetch(p + 1i32);
        q = new_native_character(cur_f, cur_c);
        g = real_get_native_glyph(
            &mut MEM[q as usize] as *mut memory_word as *mut libc::c_void,
            0_u32,
        ) as scaled_t;
        s = get_ot_math_accent_pos(cur_f, g)
    } else if MEM[(p + 1) as usize].b32.s1 == 3 {
        r = MEM[(p + 1) as usize].b32.s0;
        if r != TEX_NULL && MEM[r as usize].b16.s1 as i32 == 28 {
            s = compute_ot_math_accent_pos(r)
        } else {
            s = 0x7fffffffi32
        }
    } else {
        s = 0x7fffffffi32
    }
    s
}
unsafe extern "C" fn make_math_accent(mut q: i32) {
    let mut p: i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut a: i32 = 0;
    let mut c: i32 = 0;
    let mut g: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut i: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut s: scaled_t = 0;
    let mut sa: scaled_t = 0;
    let mut h: scaled_t = 0;
    let mut delta: scaled_t = 0;
    let mut w: scaled_t = 0;
    let mut w2: scaled_t = 0;
    let mut ot_assembly_ptr: *mut libc::c_void = 0 as *mut libc::c_void;
    fetch(q + 4i32);
    x = TEX_NULL;
    ot_assembly_ptr = 0 as *mut libc::c_void;
    if FONT_AREA[cur_f as usize] as u32 == 0xffffu32
        || FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
    {
        c = cur_c;
        f = cur_f;
        if !(MEM[q as usize].b16.s0 as i32 == 2 || MEM[q as usize].b16.s0 as i32 == 2 + 1) {
            s = compute_ot_math_accent_pos(q)
        } else {
            s = 0i32
        }
        x = clean_box(
            q + 1i32,
            (2i32 * (cur_style as i32 / 2i32) + 1i32) as small_number,
        );
        w = MEM[(x + 1) as usize].b32.s1;
        h = MEM[(x + 3) as usize].b32.s1
    } else if cur_i.s3 as i32 > 0i32 {
        i = cur_i;
        c = cur_c;
        f = cur_f;
        s = 0i32;
        if MEM[(q + 1) as usize].b32.s1 == 1 {
            fetch(q + 1i32);
            if cur_i.s1 as i32 % 4i32 == 1i32 {
                a = LIG_KERN_BASE[cur_f as usize] + cur_i.s0 as i32;
                cur_i = FONT_INFO[a as usize].b16;
                if cur_i.s3 as i32 > 128i32 {
                    a = ((LIG_KERN_BASE[cur_f as usize]
                        + 256i32 * cur_i.s1 as i32
                        + cur_i.s0 as i32) as i64
                        + 32768
                        - (256i32 * 128i32) as i64) as i32;
                    cur_i = FONT_INFO[a as usize].b16
                }
                loop {
                    if cur_i.s2 as i32 == SKEW_CHAR[cur_f as usize] {
                        if cur_i.s1 as i32 >= 128i32 {
                            if cur_i.s3 as i32 <= 128i32 {
                                s = FONT_INFO[(KERN_BASE[cur_f as usize]
                                    + 256i32 * cur_i.s1 as i32
                                    + cur_i.s0 as i32)
                                    as usize]
                                    .b32
                                    .s1
                            }
                        }
                        break;
                    } else {
                        if cur_i.s3 as i32 >= 128i32 {
                            break;
                        }
                        a = a + cur_i.s3 as i32 + 1i32;
                        cur_i = FONT_INFO[a as usize].b16
                    }
                }
            }
        }
        x = clean_box(
            q + 1i32,
            (2i32 * (cur_style as i32 / 2i32) + 1i32) as small_number,
        );
        w = MEM[(x + 1) as usize].b32.s1;
        h = MEM[(x + 3) as usize].b32.s1;
        while !(i.s1 as i32 % 4i32 != 2i32) {
            y = i.s0 as i32;
            i = FONT_INFO[(CHAR_BASE[f as usize] + y) as usize].b16;
            if !(i.s3 as i32 > 0i32) {
                break;
            }
            if FONT_INFO[(WIDTH_BASE[f as usize] + i.s3 as i32) as usize]
                .b32
                .s1
                > w
            {
                break;
            }
            c = y
        }
    }
    /*:767*/
    if x != TEX_NULL {
        if FONT_AREA[f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0
        {
            if MEM[q as usize].b16.s0 as i32 == 2 || MEM[q as usize].b16.s0 as i32 == 2 + 1 {
                delta = 0i32
            } else if h < get_ot_math_constant(f, 6i32) {
                delta = h
            } else {
                delta = get_ot_math_constant(f, 6i32)
            }
        } else if h < FONT_INFO[(5 + PARAM_BASE[f as usize]) as usize].b32.s1 {
            delta = h
        } else {
            delta = FONT_INFO[(5 + PARAM_BASE[f as usize]) as usize].b32.s1
        }
        if MEM[(q + 2) as usize].b32.s1 != 0 || MEM[(q + 3) as usize].b32.s1 != 0 {
            if MEM[(q + 1) as usize].b32.s1 == 1 {
                /*769: */
                flush_node_list(x);
                x = new_noad();
                MEM[(x + 1) as usize] = MEM[(q + 1) as usize];
                MEM[(x + 2) as usize] = MEM[(q + 2) as usize];
                MEM[(x + 3) as usize] = MEM[(q + 3) as usize];
                MEM[(q + 2) as usize].b32 = empty;
                MEM[(q + 3) as usize].b32 = empty;
                MEM[(q + 1) as usize].b32.s1 = 3;
                MEM[(q + 1) as usize].b32.s0 = x;
                x = clean_box(q + 1i32, cur_style);
                delta = delta + MEM[(x + 3) as usize].b32.s1 - h;
                h = MEM[(x + 3) as usize].b32.s1
            }
        }
        y = char_box(f, c);
        if FONT_AREA[f as usize] as u32 == 0xffffu32 || FONT_AREA[f as usize] as u32 == 0xfffeu32 {
            p = get_node(5i32);
            MEM[p as usize].b16.s1 = 8_u16;
            MEM[p as usize].b16.s0 = 42_u16;
            MEM[(p + 4) as usize].b16.s2 = f as u16;
            MEM[(p + 4) as usize].b16.s1 = real_get_native_glyph(
                &mut MEM[MEM[(y + 5) as usize].b32.s1 as usize] as *mut memory_word
                    as *mut libc::c_void,
                0_u32,
            );
            measure_native_glyph(
                &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                1i32,
            );
            free_node(
                MEM[(y + 5) as usize].b32.s1,
                MEM[(MEM[(y + 5) as usize].b32.s1 + 4) as usize].b16.s3 as i32,
            );
            MEM[(y + 5) as usize].b32.s1 = p;
            if MEM[q as usize].b16.s0 as i32 & 1 != 0 {
                measure_native_glyph(
                    &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                    1i32,
                );
            } else {
                c = MEM[(p + 4) as usize].b16.s1 as i32;
                a = 0i32;
                loop {
                    g = get_ot_math_variant(f, c, a, &mut w2, 1i32);
                    if w2 > 0i32 && w2 <= w {
                        MEM[(p + 4) as usize].b16.s1 = g as u16;
                        measure_native_glyph(
                            &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                            1i32,
                        );
                        a += 1
                    }
                    if w2 < 0i32 || w2 >= w {
                        break;
                    }
                }
                if w2 < 0i32 {
                    ot_assembly_ptr = get_ot_assembly_ptr(f, c, 1i32);
                    if !ot_assembly_ptr.is_null() {
                        free_node(p, 5i32);
                        p = build_opentype_assembly(f, ot_assembly_ptr, w, true);
                        MEM[(y + 5) as usize].b32.s1 = p
                    }
                } else {
                    measure_native_glyph(
                        &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                        1i32,
                    );
                }
            }
            MEM[(y + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1;
            MEM[(y + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
            MEM[(y + 2) as usize].b32.s1 = MEM[(p + 2) as usize].b32.s1;
            if MEM[q as usize].b16.s0 as i32 == 2 || MEM[q as usize].b16.s0 as i32 == 2 + 1 {
                if MEM[(y + 3) as usize].b32.s1 < 0 {
                    MEM[(y + 3) as usize].b32.s1 = 0
                }
            } else if MEM[(y + 2) as usize].b32.s1 < 0 {
                MEM[(y + 2) as usize].b32.s1 = 0
            }
            if p != TEX_NULL
                && !is_char_node(p)
                && MEM[p as usize].b16.s1 as i32 == 8
                && MEM[p as usize].b16.s0 as i32 == 42
            {
                sa = get_ot_math_accent_pos(f, MEM[(p + 4) as usize].b16.s1 as i32);
                if sa == 0x7fffffffi32 {
                    sa = half(MEM[(y + 1) as usize].b32.s1)
                }
            } else {
                sa = half(MEM[(y + 1) as usize].b32.s1)
            }
            if MEM[q as usize].b16.s0 as i32 == 2
                || MEM[q as usize].b16.s0 as i32 == 2 + 1
                || s == 0x7fffffffi32
            {
                s = half(w)
            }
            MEM[(y + 4) as usize].b32.s1 = s - sa
        } else {
            MEM[(y + 4) as usize].b32.s1 = s + half(w - MEM[(y + 1) as usize].b32.s1)
        }
        MEM[(y + 1) as usize].b32.s1 = 0;
        if MEM[q as usize].b16.s0 as i32 == 2 || MEM[q as usize].b16.s0 as i32 == 2 + 1 {
            MEM[x as usize].b32.s1 = y;
            y = vpackage(x, 0i32, 1i32 as small_number, 0x3fffffffi32);
            MEM[(y + 4) as usize].b32.s1 = -(h - MEM[(y + 3) as usize].b32.s1)
        } else {
            p = new_kern(-delta);
            MEM[p as usize].b32.s1 = x;
            MEM[y as usize].b32.s1 = p;
            y = vpackage(y, 0i32, 1i32 as small_number, 0x3fffffffi32);
            if MEM[(y + 3) as usize].b32.s1 < h {
                /*765: */
                p = new_kern(h - MEM[(y + 3) as usize].b32.s1); /*773:*/
                MEM[p as usize].b32.s1 = MEM[(y + 5) as usize].b32.s1;
                MEM[(y + 5) as usize].b32.s1 = p;
                MEM[(y + 3) as usize].b32.s1 = h
            }
        }
        MEM[(y + 1) as usize].b32.s1 = MEM[(x + 1) as usize].b32.s1;
        MEM[(q + 1) as usize].b32.s0 = y;
        MEM[(q + 1) as usize].b32.s1 = 2
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
}
unsafe extern "C" fn make_fraction(mut q: i32) {
    let mut p: i32 = 0;
    let mut v: i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut z: i32 = 0;
    let mut delta: scaled_t = 0;
    let mut delta1: scaled_t = 0;
    let mut delta2: scaled_t = 0;
    let mut shift_up: scaled_t = 0;
    let mut shift_down: scaled_t = 0;
    let mut clr: scaled_t = 0;
    if MEM[(q + 1) as usize].b32.s1 == 0x40000000 {
        MEM[(q + 1) as usize].b32.s1 = default_rule_thickness()
    }
    x = clean_box(
        q + 2i32,
        (cur_style as i32 + 2i32 - 2i32 * (cur_style as i32 / 6i32)) as small_number,
    );
    z = clean_box(
        q + 3i32,
        (2i32 * (cur_style as i32 / 2i32) + 3i32 - 2i32 * (cur_style as i32 / 6i32))
            as small_number,
    );
    if MEM[(x + 1) as usize].b32.s1 < MEM[(z + 1) as usize].b32.s1 {
        x = rebox(x, MEM[(z + 1) as usize].b32.s1)
    } else {
        z = rebox(z, MEM[(x + 1) as usize].b32.s1)
    }
    if (cur_style as i32) < 2i32 {
        shift_up = num1(cur_size);
        shift_down = denom1(cur_size)
    } else {
        shift_down = denom2(cur_size);
        if MEM[(q + 1) as usize].b32.s1 != 0 {
            shift_up = num2(cur_size)
        } else {
            shift_up = num3(cur_size)
        }
    }
    if MEM[(q + 1) as usize].b32.s1 == 0 {
        /*772:*/
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            if (cur_style as i32) < 2i32 {
                clr = get_ot_math_constant(cur_f, 27i32)
            } else {
                clr = get_ot_math_constant(cur_f, 26i32)
            }
        } else if (cur_style as i32) < 2i32 {
            clr = 7i32 * default_rule_thickness()
        } else {
            clr = 3i32 * default_rule_thickness()
        } /*:774*/
        delta = half(
            clr - (shift_up
                - MEM[(x + 2) as usize].b32.s1
                - (MEM[(z + 3) as usize].b32.s1 - shift_down)),
        );
        if delta > 0i32 {
            shift_up = shift_up + delta;
            shift_down = shift_down + delta
        }
    } else {
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            delta = half(MEM[(q + 1) as usize].b32.s1);
            if (cur_style as i32) < 2i32 {
                clr = get_ot_math_constant(cur_f, 37i32)
            } else {
                clr = get_ot_math_constant(cur_f, 36i32)
            }
            delta1 =
                clr - (shift_up - MEM[(x + 2) as usize].b32.s1 - (axis_height(cur_size) + delta));
            if (cur_style as i32) < 2i32 {
                clr = get_ot_math_constant(cur_f, 40i32)
            } else {
                clr = get_ot_math_constant(cur_f, 39i32)
            }
            delta2 =
                clr - (axis_height(cur_size) - delta - (MEM[(z + 3) as usize].b32.s1 - shift_down))
        } else {
            if (cur_style as i32) < 2i32 {
                clr = 3i32 * MEM[(q + 1) as usize].b32.s1
            } else {
                clr = MEM[(q + 1) as usize].b32.s1
            }
            delta = half(MEM[(q + 1) as usize].b32.s1);
            delta1 =
                clr - (shift_up - MEM[(x + 2) as usize].b32.s1 - (axis_height(cur_size) + delta));
            delta2 =
                clr - (axis_height(cur_size) - delta - (MEM[(z + 3) as usize].b32.s1 - shift_down))
        }
        if delta1 > 0i32 {
            shift_up = shift_up + delta1
        }
        if delta2 > 0i32 {
            shift_down = shift_down + delta2
        }
    }
    v = new_null_box();
    MEM[v as usize].b16.s1 = 1_u16;
    MEM[(v + 3) as usize].b32.s1 = shift_up + MEM[(x + 3) as usize].b32.s1;
    MEM[(v + 2) as usize].b32.s1 = MEM[(z + 2) as usize].b32.s1 + shift_down;
    MEM[(v + 1) as usize].b32.s1 = MEM[(x + 1) as usize].b32.s1;
    if MEM[(q + 1) as usize].b32.s1 == 0 {
        p = new_kern(
            shift_up - MEM[(x + 2) as usize].b32.s1 - (MEM[(z + 3) as usize].b32.s1 - shift_down),
        );
        MEM[p as usize].b32.s1 = z
    } else {
        y = fraction_rule(MEM[(q + 1) as usize].b32.s1);
        p = new_kern(axis_height(cur_size) - delta - (MEM[(z + 3) as usize].b32.s1 - shift_down));
        MEM[y as usize].b32.s1 = p;
        MEM[p as usize].b32.s1 = z;
        p = new_kern(shift_up - MEM[(x + 2) as usize].b32.s1 - (axis_height(cur_size) + delta));
        MEM[p as usize].b32.s1 = y
    }
    MEM[x as usize].b32.s1 = p;
    MEM[(v + 5) as usize].b32.s1 = x;
    if (cur_style as i32) < 2i32 {
        delta = delim1(cur_size)
    } else {
        delta = delim2(cur_size)
    }
    x = var_delimiter(q + 4i32, cur_size, delta);
    MEM[x as usize].b32.s1 = v;
    z = var_delimiter(q + 5i32, cur_size, delta);
    MEM[v as usize].b32.s1 = z;
    MEM[(q + 1) as usize].b32.s1 = hpack(x, 0, 1 as small_number);
    /*:775*/
}
unsafe extern "C" fn make_op(mut q: i32) -> scaled_t {
    let mut delta: scaled_t = 0;
    let mut p: i32 = 0;
    let mut v: i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut z: i32 = 0;
    let mut c: u16 = 0;
    let mut i: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut shift_up: scaled_t = 0;
    let mut shift_down: scaled_t = 0;
    let mut h1: scaled_t = 0;
    let mut h2: scaled_t = 0;
    let mut n: i32 = 0;
    let mut g: i32 = 0;
    let mut ot_assembly_ptr: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut save_f: internal_font_number = 0;
    if MEM[q as usize].b16.s0 as i32 == 0 && (cur_style as i32) < 2 {
        MEM[q as usize].b16.s0 = 1_u16
    }
    delta = 0i32;
    ot_assembly_ptr = 0 as *mut libc::c_void;
    if MEM[(q + 1) as usize].b32.s1 == 1 {
        fetch(q + 1i32);
        if !(FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && usingOpenType(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32 != 0)
        {
            if (cur_style as i32) < 2i32 && cur_i.s1 as i32 % 4i32 == 2i32 {
                c = cur_i.s0;
                i = FONT_INFO[(CHAR_BASE[cur_f as usize] + c as i32) as usize].b16;
                if i.s3 as i32 > 0i32 {
                    cur_c = c as i32;
                    cur_i = i;
                    MEM[(q + 1) as usize].b16.s0 = c
                }
            }
            delta = FONT_INFO[(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4i32) as usize]
                .b32
                .s1
        }
        x = clean_box(q + 1i32, cur_style);
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            p = MEM[(x + 5) as usize].b32.s1;
            if p != TEX_NULL
                && !is_char_node(p)
                && MEM[p as usize].b16.s1 as i32 == 8
                && MEM[p as usize].b16.s0 as i32 == 42
            {
                let mut current_block_41: u64;
                if (cur_style as i32) < 2i32 {
                    h1 = get_ot_math_constant(cur_f, 3i32);
                    if (h1 as f64)
                        < ((MEM[(p + 3) as usize].b32.s1 + MEM[(p + 2) as usize].b32.s1) * 5) as f64
                            / 4_f64
                    {
                        h1 = (((MEM[(p + 3) as usize].b32.s1 + MEM[(p + 2) as usize].b32.s1) * 5)
                            as f64
                            / 4_f64) as scaled_t
                    }
                    c = MEM[(p + 4) as usize].b16.s1;
                    n = 0i32;
                    loop {
                        g = get_ot_math_variant(cur_f, c as i32, n, &mut h2, 0i32);
                        if h2 > 0i32 {
                            MEM[(p + 4) as usize].b16.s1 = g as u16;
                            measure_native_glyph(
                                &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                                1i32,
                            );
                        }
                        n += 1;
                        if h2 < 0i32 || h2 >= h1 {
                            break;
                        }
                    }
                    if h2 < 0i32 {
                        ot_assembly_ptr = get_ot_assembly_ptr(cur_f, c as i32, 0i32);
                        if !ot_assembly_ptr.is_null() {
                            free_node(p, 5i32);
                            p = build_opentype_assembly(cur_f, ot_assembly_ptr, h1, false);
                            MEM[(x + 5) as usize].b32.s1 = p;
                            delta = 0i32;
                            current_block_41 = 18116816373875863516;
                        } else {
                            current_block_41 = 6717214610478484138;
                        }
                    } else {
                        measure_native_glyph(
                            &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                            1i32,
                        );
                        current_block_41 = 6717214610478484138;
                    }
                } else {
                    current_block_41 = 6717214610478484138;
                }
                match current_block_41 {
                    6717214610478484138 => {
                        delta = get_ot_math_ital_corr(cur_f, MEM[(p + 4) as usize].b16.s1 as i32)
                    }
                    _ => {}
                }
                MEM[(x + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1;
                MEM[(x + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
                MEM[(x + 2) as usize].b32.s1 = MEM[(p + 2) as usize].b32.s1
            }
        }
        if MEM[(q + 3) as usize].b32.s1 != 0 && MEM[q as usize].b16.s0 as i32 != 1 {
            MEM[(x + 1) as usize].b32.s1 = MEM[(x + 1) as usize].b32.s1 - delta
        }
        MEM[(x + 4) as usize].b32.s1 =
            half(MEM[(x + 3) as usize].b32.s1 - MEM[(x + 2) as usize].b32.s1)
                - axis_height(cur_size);
        MEM[(q + 1) as usize].b32.s1 = 2;
        MEM[(q + 1) as usize].b32.s0 = x
    }
    save_f = cur_f;
    if MEM[q as usize].b16.s0 as i32 == 1 {
        /*777: */
        x = clean_box(
            q + 2i32,
            (2i32 * (cur_style as i32 / 4i32) + 4i32 + cur_style as i32 % 2i32) as small_number,
        );
        y = clean_box(q + 1i32, cur_style);
        z = clean_box(
            q + 3i32,
            (2i32 * (cur_style as i32 / 4i32) + 5i32) as small_number,
        );
        v = new_null_box();
        MEM[v as usize].b16.s1 = 1_u16;
        MEM[(v + 1) as usize].b32.s1 = MEM[(y + 1) as usize].b32.s1;
        if MEM[(x + 1) as usize].b32.s1 > MEM[(v + 1) as usize].b32.s1 {
            MEM[(v + 1) as usize].b32.s1 = MEM[(x + 1) as usize].b32.s1
        }
        if MEM[(z + 1) as usize].b32.s1 > MEM[(v + 1) as usize].b32.s1 {
            MEM[(v + 1) as usize].b32.s1 = MEM[(z + 1) as usize].b32.s1
        }
        x = rebox(x, MEM[(v + 1) as usize].b32.s1);
        y = rebox(y, MEM[(v + 1) as usize].b32.s1);
        z = rebox(z, MEM[(v + 1) as usize].b32.s1);
        MEM[(x + 4) as usize].b32.s1 = half(delta);
        MEM[(z + 4) as usize].b32.s1 = -MEM[(x + 4) as usize].b32.s1;
        MEM[(v + 3) as usize].b32.s1 = MEM[(y + 3) as usize].b32.s1;
        MEM[(v + 2) as usize].b32.s1 = MEM[(y + 2) as usize].b32.s1;
        cur_f = save_f;
        if MEM[(q + 2) as usize].b32.s1 == 0 {
            free_node(x, 8i32);
            MEM[(v + 5) as usize].b32.s1 = y
        } else {
            shift_up = big_op_spacing3() - MEM[(x + 2) as usize].b32.s1;
            if shift_up < big_op_spacing1() {
                shift_up = big_op_spacing1()
            }
            p = new_kern(shift_up);
            MEM[p as usize].b32.s1 = y;
            MEM[x as usize].b32.s1 = p;
            p = new_kern(big_op_spacing5());
            MEM[p as usize].b32.s1 = x;
            MEM[(v + 5) as usize].b32.s1 = p;
            MEM[(v + 3) as usize].b32.s1 = MEM[(v + 3) as usize].b32.s1
                + big_op_spacing5()
                + MEM[(x + 3) as usize].b32.s1
                + MEM[(x + 2) as usize].b32.s1
                + shift_up
        }
        if MEM[(q + 3) as usize].b32.s1 == 0 {
            free_node(z, 8i32);
        } else {
            shift_down = big_op_spacing4() - MEM[(z + 3) as usize].b32.s1;
            if shift_down < big_op_spacing2() {
                shift_down = big_op_spacing2()
            }
            p = new_kern(shift_down);
            MEM[y as usize].b32.s1 = p;
            MEM[p as usize].b32.s1 = z;
            p = new_kern(big_op_spacing5());
            MEM[z as usize].b32.s1 = p;
            MEM[(v + 2) as usize].b32.s1 = MEM[(v + 2) as usize].b32.s1
                + big_op_spacing5()
                + MEM[(z + 3) as usize].b32.s1
                + MEM[(z + 2) as usize].b32.s1
                + shift_down
        }
        MEM[(q + 1) as usize].b32.s1 = v
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    delta
}
unsafe extern "C" fn make_ord(mut q: i32) {
    let mut a: i32 = 0;
    let mut p: i32 = 0;
    let mut r: i32 = 0;
    while MEM[(q + 3) as usize].b32.s1 == 0 {
        if !(MEM[(q + 2) as usize].b32.s1 == 0) {
            break;
        }
        if !(MEM[(q + 1) as usize].b32.s1 == 1) {
            break;
        }
        p = MEM[q as usize].b32.s1;
        if !(p != TEX_NULL) {
            break;
        }
        if !(MEM[p as usize].b16.s1 as i32 >= 16 && MEM[p as usize].b16.s1 as i32 <= 22) {
            break;
        }
        if !(MEM[(p + 1) as usize].b32.s1 == 1) {
            break;
        }
        if !(MEM[(p + 1) as usize].b16.s1 as i32 % 256 == MEM[(q + 1) as usize].b16.s1 as i32 % 256)
        {
            break;
        }
        MEM[(q + 1) as usize].b32.s1 = 4;
        fetch(q + 1i32);
        if !(cur_i.s1 as i32 % 4i32 == 1i32) {
            break;
        }
        a = LIG_KERN_BASE[cur_f as usize] + cur_i.s0 as i32;
        cur_c = MEM[(p + 1) as usize].b16.s0 as i32;
        cur_i = FONT_INFO[a as usize].b16;
        if cur_i.s3 as i32 > 128i32 {
            a = ((LIG_KERN_BASE[cur_f as usize] + 256i32 * cur_i.s1 as i32 + cur_i.s0 as i32)
                as i64
                + 32768
                - (256i32 * 128i32) as i64) as i32;
            cur_i = FONT_INFO[a as usize].b16
        }
        loop {
            if cur_i.s2 as i32 == cur_c {
                if cur_i.s3 as i32 <= 128i32 {
                    if cur_i.s1 as i32 >= 128i32 {
                        p = new_kern(
                            FONT_INFO[(KERN_BASE[cur_f as usize]
                                + 256i32 * cur_i.s1 as i32
                                + cur_i.s0 as i32) as usize]
                                .b32
                                .s1,
                        );
                        MEM[p as usize].b32.s1 = MEM[q as usize].b32.s1;
                        MEM[q as usize].b32.s1 = p;
                        return;
                    } else {
                        match cur_i.s1 as i32 {
                            1 | 5 => MEM[(q + 1) as usize].b16.s0 = cur_i.s0,
                            2 | 6 => MEM[(p + 1) as usize].b16.s0 = cur_i.s0,
                            3 | 7 | 11 => {
                                r = new_noad();
                                MEM[(r + 1) as usize].b16.s0 = cur_i.s0;
                                MEM[(r + 1) as usize].b16.s1 =
                                    (MEM[(q + 1) as usize].b16.s1 as i32 % 256) as u16;
                                MEM[q as usize].b32.s1 = r;
                                MEM[r as usize].b32.s1 = p;
                                if (cur_i.s1 as i32) < 11i32 {
                                    MEM[(r + 1) as usize].b32.s1 = 1
                                } else {
                                    MEM[(r + 1) as usize].b32.s1 = 4
                                }
                            }
                            _ => {
                                MEM[q as usize].b32.s1 = MEM[p as usize].b32.s1;
                                MEM[(q + 1) as usize].b16.s0 = cur_i.s0;
                                MEM[(q + 3) as usize] = MEM[(p + 3) as usize];
                                MEM[(q + 2) as usize] = MEM[(p + 2) as usize];
                                free_node(p, 4i32);
                            }
                        }
                        if cur_i.s1 as i32 > 3i32 {
                            return;
                        }
                        MEM[(q + 1) as usize].b32.s1 = 1;
                        break;
                    }
                }
            }
            if cur_i.s3 as i32 >= 128i32 {
                return;
            }
            a = a + cur_i.s3 as i32 + 1i32;
            cur_i = FONT_INFO[a as usize].b16
        }
    }
}
unsafe extern "C" fn attach_hkern_to_new_hlist(mut q: i32, mut delta: scaled_t) -> i32 {
    let mut y: i32 = 0;
    let mut z: i32 = 0;
    z = new_kern(delta);
    if MEM[(q + 1) as usize].b32.s1 == -0xfffffff {
        MEM[(q + 1) as usize].b32.s1 = z
    } else {
        y = MEM[(q + 1) as usize].b32.s1;
        while MEM[y as usize].b32.s1 != -0xfffffff {
            y = MEM[y as usize].b32.s1
        }
        MEM[y as usize].b32.s1 = z
    }
    MEM[(q + 1) as usize].b32.s1
}
unsafe extern "C" fn make_scripts(mut q: i32, mut delta: scaled_t) {
    let mut p: i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut z: i32 = 0;
    let mut shift_up: scaled_t = 0;
    let mut shift_down: scaled_t = 0;
    let mut clr: scaled_t = 0;
    let mut sub_kern: scaled_t = 0;
    let mut sup_kern: scaled_t = 0;
    let mut script_c: i32 = 0;
    let mut script_g: u16 = 0;
    let mut script_f: internal_font_number = 0;
    let mut t: i32 = 0;
    let mut save_f: internal_font_number = 0;
    p = MEM[(q + 1) as usize].b32.s1;
    script_c = TEX_NULL;
    script_g = 0_u16;
    script_f = 0i32;
    sup_kern = 0i32;
    sub_kern = 0i32;
    if is_char_node(p) as i32 != 0
        || p != TEX_NULL
            && !is_char_node(p)
            && MEM[p as usize].b16.s1 as i32 == 8
            && MEM[p as usize].b16.s0 as i32 == 42
    {
        shift_up = 0i32;
        shift_down = 0i32
    } else {
        z = hpack(p, 0i32, 1i32 as small_number);
        if (cur_style as i32) < 4i32 {
            t = 256i32
        } else {
            t = 2i32 * 256i32
        }
        shift_up = MEM[(z + 3) as usize].b32.s1 - sup_drop(t);
        shift_down = MEM[(z + 2) as usize].b32.s1 + sub_drop(t);
        free_node(z, 8i32);
    }
    if MEM[(q + 2) as usize].b32.s1 == 0 {
        /*784: */
        save_f = cur_f;
        x = clean_box(
            q + 3i32,
            (2i32 * (cur_style as i32 / 4i32) + 5i32) as small_number,
        );
        cur_f = save_f;
        MEM[(x + 1) as usize].b32.s1 =
            MEM[(x + 1) as usize].b32.s1 + *DIMENPAR(DimenPar::script_space);
        if shift_down < sub1(cur_size) {
            shift_down = sub1(cur_size)
        }
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            clr = MEM[(x + 3) as usize].b32.s1 - get_ot_math_constant(cur_f, 9)
        } else {
            clr = MEM[(x + 3) as usize].b32.s1 - (math_x_height(cur_size) * 4).abs() / 5
        }
        if shift_down < clr {
            shift_down = clr
        }
        MEM[(x + 4) as usize].b32.s1 = shift_down;
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            /*787: */
            if MEM[(q + 3) as usize].b32.s1 == 1 {
                save_f = cur_f;
                fetch(q + 3i32);
                if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    script_c = new_native_character(cur_f, cur_c);
                    script_g = real_get_native_glyph(
                        &mut MEM[script_c as usize] as *mut memory_word as *mut libc::c_void,
                        0_u32,
                    );
                    script_f = cur_f
                } else {
                    script_g = 0_u16;
                    script_f = 0i32
                }
                cur_f = save_f
            }
            if p != TEX_NULL
                && !is_char_node(p)
                && MEM[p as usize].b16.s1 as i32 == 8
                && MEM[p as usize].b16.s0 as i32 == 42
            {
                sub_kern = get_ot_math_kern(
                    MEM[(p + 4) as usize].b16.s2 as i32,
                    MEM[(p + 4) as usize].b16.s1 as i32,
                    script_f,
                    script_g as i32,
                    1i32,
                    shift_down,
                )
            }
            if sub_kern != 0i32 {
                p = attach_hkern_to_new_hlist(q, sub_kern)
            }
        }
    } else {
        save_f = cur_f;
        x = clean_box(
            q + 2i32,
            (2i32 * (cur_style as i32 / 4i32) + 4i32 + cur_style as i32 % 2i32) as small_number,
        );
        cur_f = save_f;
        MEM[(x + 1) as usize].b32.s1 =
            MEM[(x + 1) as usize].b32.s1 + *DIMENPAR(DimenPar::script_space);
        if cur_style as i32 & 1i32 != 0 {
            clr = sup3(cur_size)
        } else if (cur_style as i32) < 2i32 {
            clr = sup1(cur_size)
        } else {
            clr = sup2(cur_size)
        }
        if shift_up < clr {
            shift_up = clr
        }
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            clr = MEM[(x + 2) as usize].b32.s1 + get_ot_math_constant(cur_f, 13)
        } else {
            clr = MEM[(x + 2) as usize].b32.s1 + math_x_height(cur_size).abs() / 4
        }
        if shift_up < clr {
            shift_up = clr
        }
        if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
            && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine) as i32
                != 0
        {
            /*788: */
            if MEM[(q + 2) as usize].b32.s1 == 1 {
                save_f = cur_f;
                fetch(q + 2i32);
                if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    script_c = new_native_character(cur_f, cur_c);
                    script_g = real_get_native_glyph(
                        &mut MEM[script_c as usize] as *mut memory_word as *mut libc::c_void,
                        0_u32,
                    );
                    script_f = cur_f
                } else {
                    script_g = 0_u16;
                    script_f = 0i32
                }
                cur_f = save_f
            }
            if p != TEX_NULL
                && !is_char_node(p)
                && MEM[p as usize].b16.s1 as i32 == 8
                && MEM[p as usize].b16.s0 as i32 == 42
            {
                sup_kern = get_ot_math_kern(
                    MEM[(p + 4) as usize].b16.s2 as i32,
                    MEM[(p + 4) as usize].b16.s1 as i32,
                    script_f,
                    script_g as i32,
                    0i32,
                    shift_up,
                )
            }
            if sup_kern != 0i32 && MEM[(q + 3) as usize].b32.s1 == 0 {
                p = attach_hkern_to_new_hlist(q, sup_kern)
            }
        }
        if MEM[(q + 3) as usize].b32.s1 == 0 {
            MEM[(x + 4) as usize].b32.s1 = -shift_up
        } else {
            /*786: */
            save_f = cur_f;
            y = clean_box(
                q + 3i32,
                (2i32 * (cur_style as i32 / 4i32) + 5i32) as small_number,
            );
            cur_f = save_f;
            MEM[(y + 1) as usize].b32.s1 =
                MEM[(y + 1) as usize].b32.s1 + *DIMENPAR(DimenPar::script_space);
            if shift_down < sub2(cur_size) {
                shift_down = sub2(cur_size)
            }
            if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                    as i32
                    != 0
            {
                clr = get_ot_math_constant(cur_f, 15i32)
                    - (shift_up
                        - MEM[(x + 2) as usize].b32.s1
                        - (MEM[(y + 3) as usize].b32.s1 - shift_down))
            } else {
                clr = 4i32 * default_rule_thickness()
                    - (shift_up
                        - MEM[(x + 2) as usize].b32.s1
                        - (MEM[(y + 3) as usize].b32.s1 - shift_down))
            }
            if clr > 0i32 {
                shift_down = shift_down + clr;
                if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                    && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                        as i32
                        != 0
                {
                    clr = get_ot_math_constant(cur_f, 16i32)
                        - (shift_up - MEM[(x + 2) as usize].b32.s1)
                } else {
                    clr = (math_x_height(cur_size) * 4i32).abs() / 5i32
                        - (shift_up - MEM[(x + 2) as usize].b32.s1)
                }
                if clr > 0i32 {
                    shift_up = shift_up + clr;
                    shift_down = shift_down - clr
                }
            }
            if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                && isOpenTypeMathFont(FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine)
                    as i32
                    != 0
            {
                if MEM[(q + 3) as usize].b32.s1 == 1 {
                    save_f = cur_f;
                    fetch(q + 3i32);
                    if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                        && isOpenTypeMathFont(
                            FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                        ) as i32
                            != 0
                    {
                        script_c = new_native_character(cur_f, cur_c);
                        script_g = real_get_native_glyph(
                            &mut MEM[script_c as usize] as *mut memory_word as *mut libc::c_void,
                            0_u32,
                        );
                        script_f = cur_f
                    } else {
                        script_g = 0_u16;
                        script_f = 0i32
                    }
                    cur_f = save_f
                }
                if p != TEX_NULL
                    && !is_char_node(p)
                    && MEM[p as usize].b16.s1 as i32 == 8
                    && MEM[p as usize].b16.s0 as i32 == 42
                {
                    sub_kern = get_ot_math_kern(
                        MEM[(p + 4) as usize].b16.s2 as i32,
                        MEM[(p + 4) as usize].b16.s1 as i32,
                        script_f,
                        script_g as i32,
                        1i32,
                        shift_down,
                    )
                }
                if sub_kern != 0i32 {
                    p = attach_hkern_to_new_hlist(q, sub_kern)
                }
                if MEM[(q + 2) as usize].b32.s1 == 1 {
                    save_f = cur_f;
                    fetch(q + 2i32);
                    if FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                        && isOpenTypeMathFont(
                            FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                        ) as i32
                            != 0
                    {
                        script_c = new_native_character(cur_f, cur_c);
                        script_g = real_get_native_glyph(
                            &mut MEM[script_c as usize] as *mut memory_word as *mut libc::c_void,
                            0_u32,
                        );
                        script_f = cur_f
                    } else {
                        script_g = 0_u16;
                        script_f = 0i32
                    }
                    cur_f = save_f
                }
                if p != TEX_NULL
                    && !is_char_node(p)
                    && MEM[p as usize].b16.s1 as i32 == 8
                    && MEM[p as usize].b16.s0 as i32 == 42
                {
                    sup_kern = get_ot_math_kern(
                        MEM[(p + 4) as usize].b16.s2 as i32,
                        MEM[(p + 4) as usize].b16.s1 as i32,
                        script_f,
                        script_g as i32,
                        0i32,
                        shift_up,
                    )
                }
                if sup_kern != 0i32 && MEM[(q + 3) as usize].b32.s1 == 0 {
                    p = attach_hkern_to_new_hlist(q, sup_kern)
                }
            }
            MEM[(x + 4) as usize].b32.s1 = sup_kern + delta - sub_kern;
            p = new_kern(
                shift_up
                    - MEM[(x + 2) as usize].b32.s1
                    - (MEM[(y + 3) as usize].b32.s1 - shift_down),
            );
            MEM[x as usize].b32.s1 = p;
            MEM[p as usize].b32.s1 = y;
            x = vpackage(x, 0i32, 1i32 as small_number, 0x3fffffffi32);
            MEM[(x + 4) as usize].b32.s1 = shift_down
        }
    }
    if MEM[(q + 1) as usize].b32.s1 == -0xfffffff {
        MEM[(q + 1) as usize].b32.s1 = x
    } else {
        p = MEM[(q + 1) as usize].b32.s1;
        while MEM[p as usize].b32.s1 != -0xfffffff {
            p = MEM[p as usize].b32.s1
        }
        MEM[p as usize].b32.s1 = x
    };
}
unsafe extern "C" fn make_left_right(
    mut q: i32,
    mut style: small_number,
    mut max_d: scaled_t,
    mut max_h: scaled_t,
) -> small_number {
    let mut delta: scaled_t = 0;
    let mut delta1: scaled_t = 0;
    let mut delta2: scaled_t = 0;
    cur_style = style;
    if (cur_style as i32) < 4i32 {
        cur_size = 0i32
    } else {
        cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18i32);
    delta2 = max_d + axis_height(cur_size);
    delta1 = max_h + max_d - delta2;
    if delta2 > delta1 {
        delta1 = delta2
    }
    delta = delta1 / 500i32 * *INTPAR(IntPar::delimiter_factor);
    delta2 = delta1 + delta1 - *DIMENPAR(DimenPar::delimiter_shortfall);
    if delta < delta2 {
        delta = delta2
    }
    MEM[(q + 1) as usize].b32.s1 = var_delimiter(q + 1, cur_size, delta);
    (MEM[q as usize].b16.s1 as i32 - (30 - 20)) as small_number
}
unsafe extern "C" fn mlist_to_hlist() {
    let mut current_block: u64;
    let mut mlist: i32 = 0;
    let mut penalties: bool = false;
    let mut style: small_number = 0;
    let mut save_style: small_number = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut r_type: small_number = 0;
    let mut t: small_number = 0;
    let mut p: i32 = TEX_NULL;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut z: i32 = 0;
    let mut pen: i32 = 0;
    let mut s: small_number = 0;
    let mut max_h: scaled_t = 0;
    let mut max_d: scaled_t = 0;
    let mut delta: scaled_t = 0;
    mlist = cur_mlist;
    penalties = mlist_penalties;
    style = cur_style;
    q = mlist;
    r = TEX_NULL;
    r_type = 17i32 as small_number;
    max_h = 0i32;
    max_d = 0i32;
    if (cur_style as i32) < 4i32 {
        cur_size = 0i32
    } else {
        cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18i32);
    while q != TEX_NULL {
        loop
        /*753: */
        {
            delta = 0i32; /*:755 */
            match MEM[q as usize].b16.s1 as i32 {
                18 => {
                    match r_type as i32 {
                        18 | 17 | 19 | 20 | 22 | 30 => {}
                        _ => {
                            current_block = 1677945370889843322;
                            break;
                        }
                    }
                    MEM[q as usize].b16.s1 = 16_u16
                }
                19 | 21 | 22 | 31 => {
                    if r_type as i32 == 18i32 {
                        MEM[r as usize].b16.s1 = 16_u16
                    }
                    if MEM[q as usize].b16.s1 as i32 == 31 {
                        current_block = 2476306051584715158;
                        break;
                    } else {
                        current_block = 1677945370889843322;
                        break;
                    }
                }
                30 => {
                    current_block = 2476306051584715158;
                    break;
                }
                25 => {
                    make_fraction(q);
                    current_block = 454865348394072936;
                    break;
                }
                17 => {
                    delta = make_op(q);
                    if MEM[q as usize].b16.s0 as i32 == 1 {
                        current_block = 454865348394072936;
                        break;
                    } else {
                        current_block = 1677945370889843322;
                        break;
                    }
                }
                16 => {
                    make_ord(q);
                    current_block = 1677945370889843322;
                    break;
                }
                20 | 23 => {
                    current_block = 1677945370889843322;
                    break;
                }
                24 => {
                    make_radical(q);
                    current_block = 1677945370889843322;
                    break;
                }
                27 => {
                    make_over(q);
                    current_block = 1677945370889843322;
                    break;
                }
                26 => {
                    make_under(q);
                    current_block = 1677945370889843322;
                    break;
                }
                28 => {
                    make_math_accent(q);
                    current_block = 1677945370889843322;
                    break;
                }
                29 => {
                    make_vcenter(q);
                    current_block = 1677945370889843322;
                    break;
                }
                14 => {
                    cur_style = MEM[q as usize].b16.s0 as small_number;
                    if (cur_style as i32) < 4i32 {
                        cur_size = 0i32
                    } else {
                        cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
                    }
                    cur_mu = x_over_n(math_quad(cur_size), 18i32);
                    current_block = 12027452349022962373;
                    break;
                }
                15 => {
                    match cur_style as i32 / 2i32 {
                        0 => {
                            p = MEM[(q + 1) as usize].b32.s0;
                            MEM[(q + 1) as usize].b32.s0 = -0xfffffff
                        }
                        1 => {
                            p = MEM[(q + 1) as usize].b32.s1;
                            MEM[(q + 1) as usize].b32.s1 = -0xfffffff
                        }
                        2 => {
                            p = MEM[(q + 2) as usize].b32.s0;
                            MEM[(q + 2) as usize].b32.s0 = -0xfffffff
                        }
                        3 => {
                            p = MEM[(q + 2) as usize].b32.s1;
                            MEM[(q + 2) as usize].b32.s1 = -0xfffffff
                        }
                        _ => {}
                    }
                    flush_node_list(MEM[(q + 1) as usize].b32.s0);
                    flush_node_list(MEM[(q + 1) as usize].b32.s1);
                    flush_node_list(MEM[(q + 2) as usize].b32.s0);
                    flush_node_list(MEM[(q + 2) as usize].b32.s1);
                    MEM[q as usize].b16.s1 = 14_u16;
                    MEM[q as usize].b16.s0 = cur_style as u16;
                    MEM[(q + 1) as usize].b32.s1 = 0;
                    MEM[(q + 2) as usize].b32.s1 = 0;
                    if p != TEX_NULL {
                        z = MEM[q as usize].b32.s1;
                        MEM[q as usize].b32.s1 = p;
                        while MEM[p as usize].b32.s1 != -0xfffffff {
                            p = MEM[p as usize].b32.s1
                        }
                        MEM[p as usize].b32.s1 = z
                    }
                    current_block = 12027452349022962373;
                    break;
                }
                3 | 4 | 5 | 8 | 12 | 7 => {
                    current_block = 12027452349022962373;
                    break;
                }
                2 => {
                    if MEM[(q + 3) as usize].b32.s1 > max_h {
                        max_h = MEM[(q + 3) as usize].b32.s1
                    }
                    if MEM[(q + 2) as usize].b32.s1 > max_d {
                        max_d = MEM[(q + 2) as usize].b32.s1
                    }
                    current_block = 12027452349022962373;
                    break;
                }
                10 => {
                    if MEM[q as usize].b16.s0 as i32 == 99 {
                        x = MEM[(q + 1) as usize].b32.s0;
                        y = math_glue(x, cur_mu);
                        delete_glue_ref(x);
                        MEM[(q + 1) as usize].b32.s0 = y;
                        MEM[q as usize].b16.s0 = 0_u16
                    } else if cur_size != 0i32 && MEM[q as usize].b16.s0 as i32 == 98 {
                        p = MEM[q as usize].b32.s1;
                        if p != TEX_NULL {
                            if MEM[p as usize].b16.s1 as i32 == 10
                                || MEM[p as usize].b16.s1 as i32 == 11
                            {
                                MEM[q as usize].b32.s1 = MEM[p as usize].b32.s1;
                                MEM[p as usize].b32.s1 = -0xfffffff;
                                flush_node_list(p);
                            }
                        }
                    }
                    current_block = 12027452349022962373;
                    break;
                }
                11 => {
                    math_kern(q, cur_mu);
                    current_block = 12027452349022962373;
                    break;
                }
                _ => confusion(b"mlist1"),
            }
        }
        match current_block {
            1677945370889843322 => {
                match MEM[(q + 1) as usize].b32.s1 {
                    1 | 4 => {
                        fetch(q + 1i32);
                        if FONT_AREA[cur_f as usize] as u32 == 0xffffu32
                            || FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                        {
                            z = new_native_character(cur_f, cur_c);
                            p = get_node(5i32);
                            MEM[p as usize].b16.s1 = 8_u16;
                            MEM[p as usize].b16.s0 = 42_u16;
                            MEM[(p + 4) as usize].b16.s2 = cur_f as u16;
                            MEM[(p + 4) as usize].b16.s1 = real_get_native_glyph(
                                &mut MEM[z as usize] as *mut memory_word as *mut libc::c_void,
                                0_u32,
                            );
                            measure_native_glyph(
                                &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
                                1i32,
                            );
                            free_node(z, MEM[(z + 4) as usize].b16.s3 as i32);
                            delta =
                                get_ot_math_ital_corr(cur_f, MEM[(p + 4) as usize].b16.s1 as i32);
                            if MEM[(q + 1) as usize].b32.s1 == 4
                                && !(FONT_AREA[cur_f as usize] as u32 == 0xfffeu32
                                    && isOpenTypeMathFont(
                                        FONT_LAYOUT_ENGINE[cur_f as usize] as XeTeXLayoutEngine,
                                    ) as i32
                                        != 0) as i32
                                    != 0i32
                            {
                                delta = 0i32
                            }
                            if MEM[(q + 3) as usize].b32.s1 == 0 && delta != 0 {
                                MEM[p as usize].b32.s1 = new_kern(delta);
                                delta = 0i32
                            }
                        } else if cur_i.s3 as i32 > 0i32 {
                            delta = FONT_INFO
                                [(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4i32) as usize]
                                .b32
                                .s1;
                            p = new_character(cur_f, cur_c as UTF16_code);
                            if MEM[(q + 1) as usize].b32.s1 == 4
                                && FONT_INFO[(2 + PARAM_BASE[cur_f as usize]) as usize].b32.s1
                                    != 0i32
                            {
                                delta = 0i32
                            }
                            if MEM[(q + 3) as usize].b32.s1 == 0 && delta != 0 {
                                MEM[p as usize].b32.s1 = new_kern(delta);
                                delta = 0i32
                            }
                        } else {
                            p = TEX_NULL
                        }
                    }
                    0 => p = TEX_NULL,
                    2 => p = MEM[(q + 1) as usize].b32.s0,
                    3 => {
                        cur_mlist = MEM[(q + 1) as usize].b32.s0;
                        save_style = cur_style;
                        mlist_penalties = false;
                        mlist_to_hlist();
                        cur_style = save_style;
                        if (cur_style as i32) < 4i32 {
                            cur_size = 0i32
                        } else {
                            cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
                        }
                        cur_mu = x_over_n(math_quad(cur_size), 18i32);
                        p = hpack(
                            MEM[(4999999 - 3) as usize].b32.s1,
                            0i32,
                            1i32 as small_number,
                        )
                    }
                    _ => confusion(b"mlist2"),
                }
                MEM[(q + 1) as usize].b32.s1 = p;
                if MEM[(q + 3) as usize].b32.s1 == 0 && MEM[(q + 2) as usize].b32.s1 == 0 {
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
                z = hpack(MEM[(q + 1) as usize].b32.s1, 0, 1 as small_number);
                if MEM[(z + 3) as usize].b32.s1 > max_h {
                    max_h = MEM[(z + 3) as usize].b32.s1
                }
                if MEM[(z + 2) as usize].b32.s1 > max_d {
                    max_d = MEM[(z + 2) as usize].b32.s1
                }
                free_node(z, 8i32);
                current_block = 2476306051584715158;
            }
            _ => {}
        }
        match current_block {
            2476306051584715158 => {
                /*done_with_noad */
                r = q;
                r_type = MEM[r as usize].b16.s1 as small_number;
                if r_type as i32 == 31i32 {
                    r_type = 30i32 as small_number;
                    cur_style = style;
                    if (cur_style as i32) < 4i32 {
                        cur_size = 0i32
                    } else {
                        cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
                    }
                    cur_mu = x_over_n(math_quad(cur_size), 18i32)
                }
            }
            _ => {}
        }
        /*done_with_node */
        q = MEM[q as usize].b32.s1
    } /*ord_noad *//*:755 */
    if r_type as i32 == 18i32 {
        MEM[r as usize].b16.s1 = 16_u16
    }
    p = 4999999i32 - 3i32;
    MEM[p as usize].b32.s1 = -0xfffffff;
    q = mlist;
    r_type = 0i32 as small_number;
    cur_style = style;
    if (cur_style as i32) < 4i32 {
        cur_size = 0i32
    } else {
        cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
    }
    cur_mu = x_over_n(math_quad(cur_size), 18i32);
    while q != TEX_NULL {
        let mut current_block_236: u64;
        t = 16i32 as small_number;
        s = 4i32 as small_number;
        pen = 10000i32;
        match MEM[q as usize].b16.s1 as i32 {
            17 | 20 | 21 | 22 | 23 => {
                t = MEM[q as usize].b16.s1 as small_number;
                current_block_236 = 15067367080042895309;
            }
            18 => {
                t = 18i32 as small_number;
                pen = *INTPAR(IntPar::bin_op_penalty);
                current_block_236 = 15067367080042895309;
            }
            19 => {
                t = 19i32 as small_number;
                pen = *INTPAR(IntPar::rel_penalty);
                current_block_236 = 15067367080042895309;
            }
            16 | 29 | 27 | 26 => current_block_236 = 15067367080042895309,
            24 => {
                s = 5i32 as small_number;
                current_block_236 = 15067367080042895309;
            }
            28 => {
                s = 5i32 as small_number;
                current_block_236 = 15067367080042895309;
            }
            25 => {
                t = 23i32 as small_number;
                s = 6i32 as small_number;
                current_block_236 = 15067367080042895309;
            }
            30 | 31 => {
                t = make_left_right(q, style, max_d, max_h);
                current_block_236 = 15067367080042895309;
            }
            14 => {
                cur_style = MEM[q as usize].b16.s0 as small_number;
                s = 3i32 as small_number;
                if (cur_style as i32) < 4i32 {
                    cur_size = 0i32
                } else {
                    cur_size = 256i32 * ((cur_style as i32 - 2i32) / 2i32)
                }
                cur_mu = x_over_n(math_quad(cur_size), 18i32);
                current_block_236 = 11920828421623439930;
            }
            8 | 12 | 2 | 7 | 5 | 3 | 4 | 10 | 11 => {
                MEM[p as usize].b32.s1 = q;
                p = q;
                q = MEM[q as usize].b32.s1;
                MEM[p as usize].b32.s1 = -0xfffffff;
                current_block_236 = 7344615536999694015;
            }
            _ => confusion(b"mlist3"),
        }
        match current_block_236 {
            15067367080042895309 => {
                if r_type as i32 > 0i32 {
                    let mut offset_table: [*const i8; 8] = [
                        b"02340001\x00" as *const u8 as *const i8,
                        b"22*40001\x00" as *const u8 as *const i8,
                        b"33**3**3\x00" as *const u8 as *const i8,
                        b"44*04004\x00" as *const u8 as *const i8,
                        b"00*00000\x00" as *const u8 as *const i8,
                        b"02340001\x00" as *const u8 as *const i8,
                        b"11*11111\x00" as *const u8 as *const i8,
                        b"12341011\x00" as *const u8 as *const i8,
                    ];
                    // The inter-element spacing in math formulas depends on a 8x8 table.
                    // The table indices range from ORD_NOAD to INNER_NOAD.
                    // The chars of this table have the following significance:
                    match *offset_table[(r_type as i32 - 16i32) as usize]
                        .offset((t as i32 - 16i32) as isize) as i32
                    {
                        48 => {
                            // no space
                            x = 0i32
                        }
                        49 => {
                            // a conditional thin space
                            if (cur_style as i32) < 4i32 {
                                x = 16i32
                            } else {
                                x = 0i32
                            }
                        }
                        50 => {
                            // a thin space
                            x = 16i32
                        }
                        51 => {
                            // a conditional medium space
                            if (cur_style as i32) < 4i32 {
                                x = 17i32
                            } else {
                                x = 0i32
                            }
                        }
                        52 => {
                            // a conditional thick space
                            if (cur_style as i32) < 4i32 {
                                x = 18i32
                            } else {
                                x = 0i32
                            }
                        }
                        _ => {
                            // impossible
                            confusion(b"mlist4");
                        }
                    }
                    if x != 0i32 {
                        y = math_glue(EQTB[(GLUE_BASE + x) as usize].b32.s1, cur_mu);
                        z = new_glue(y);
                        MEM[y as usize].b32.s1 = -0xfffffff;
                        MEM[p as usize].b32.s1 = z;
                        p = z;
                        MEM[z as usize].b16.s0 = (x + 1) as u16
                    }
                }
                if MEM[(q + 1) as usize].b32.s1 != -0xfffffff {
                    MEM[p as usize].b32.s1 = MEM[(q + 1) as usize].b32.s1;
                    loop {
                        p = MEM[p as usize].b32.s1;
                        if MEM[p as usize].b32.s1 == -0xfffffff {
                            break;
                        }
                    }
                }
                if penalties {
                    if MEM[q as usize].b32.s1 != -0xfffffff {
                        if pen < 10000i32 {
                            r_type = MEM[MEM[q as usize].b32.s1 as usize].b16.s1 as small_number;
                            if r_type as i32 != 12i32 {
                                if r_type as i32 != 19i32 {
                                    z = new_penalty(pen);
                                    MEM[p as usize].b32.s1 = z;
                                    p = z
                                }
                            }
                        }
                    }
                }
                if MEM[q as usize].b16.s1 as i32 == 31 {
                    t = 20i32 as small_number
                }
                r_type = t;
                current_block_236 = 11920828421623439930;
            }
            _ => {}
        }
        match current_block_236 {
            11920828421623439930 => {
                /*delete_q */
                r = q;
                q = MEM[q as usize].b32.s1;
                free_node(r, s as i32);
            }
            _ => {}
        }
    }
}
unsafe extern "C" fn var_delimiter(mut d: i32, mut s: i32, mut v: scaled_t) -> i32 {
    let mut b: i32 = 0;
    let mut ot_assembly_ptr: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut f: internal_font_number = 0;
    let mut g: internal_font_number = 0;
    let mut c: u16 = 0;
    let mut x: u16 = 0;
    let mut y: u16 = 0;
    let mut m: i32 = 0;
    let mut n: i32 = 0;
    let mut u: scaled_t = 0;
    let mut w: scaled_t = 0;
    let mut q: b16x4 = b16x4_le_t {
        s0: 0_u16,
        s1: 0_u16,
        s2: 0_u16,
        s3: 0_u16,
    };
    let mut r: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut z: i32 = 0;
    let mut large_attempt: bool = false;
    f = 0i32;
    w = 0i32;
    large_attempt = false;
    z = MEM[d as usize].b16.s3 as i32 % 256;
    x = (MEM[d as usize].b16.s2 as i64 + (MEM[d as usize].b16.s3 as i32 / 256) as i64 * 65536)
        as u16;
    ot_assembly_ptr = 0 as *mut libc::c_void;
    's_62: loop {
        if z != 0i32 || x as i32 != 0i32 {
            z = z + s + 256i32;
            loop {
                z = z - 256i32;
                g = *MATH_FONT(z);
                if g != 0i32 {
                    /*734: */
                    if FONT_AREA[g as usize] as u32 == 0xfffeu32
                        && usingOpenType(FONT_LAYOUT_ENGINE[g as usize] as XeTeXLayoutEngine) as i32
                            != 0
                    {
                        x = map_char_to_glyph(g, x as i32) as u16;
                        f = g;
                        c = x;
                        w = 0i32;
                        n = 0i32;
                        loop {
                            y = get_ot_math_variant(g, x as i32, n, &mut u, 0i32) as u16;
                            if u > w {
                                c = y;
                                w = u;
                                if u >= v {
                                    break 's_62;
                                }
                            }
                            n = n + 1i32;
                            if u < 0i32 {
                                break;
                            }
                        }
                        ot_assembly_ptr = get_ot_assembly_ptr(g, x as i32, 0i32);
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
                                if !(q.s3 as i32 > 0i32) {
                                    break;
                                }
                                if q.s1 as i32 % 4i32 == 3i32 {
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
                                    if !(q.s1 as i32 % 4i32 == 2i32) {
                                        break;
                                    }
                                    y = q.s0
                                }
                            }
                        }
                    }
                }
                if z < 256i32 {
                    break;
                }
            }
        }
        if large_attempt {
            break;
        }
        large_attempt = true;
        z = MEM[d as usize].b16.s1 as i32 % 256;
        x = (MEM[d as usize].b16.s0 as i64 + (MEM[d as usize].b16.s1 as i32 / 256) as i64 * 65536)
            as u16
    }
    if f != 0i32 {
        if !(FONT_AREA[f as usize] as u32 == 0xfffeu32
            && usingOpenType(FONT_LAYOUT_ENGINE[f as usize] as XeTeXLayoutEngine) as i32 != 0)
        {
            /*736: */
            if q.s1 as i32 % 4i32 == 3i32 {
                /*739: */
                b = new_null_box();
                MEM[b as usize].b16.s1 = 1_u16;
                r = FONT_INFO[(EXTEN_BASE[f as usize] + q.s0 as i32) as usize].b16;
                c = r.s0;
                u = height_plus_depth(f, c);
                w = 0i32;
                q = FONT_INFO[(CHAR_BASE[f as usize] + effective_char(true, f, c)) as usize].b16;
                MEM[(b + 1) as usize].b32.s1 = FONT_INFO
                    [(WIDTH_BASE[f as usize] + q.s3 as i32) as usize]
                    .b32
                    .s1
                    + FONT_INFO[(ITALIC_BASE[f as usize] + q.s1 as i32 / 4) as usize]
                        .b32
                        .s1;
                c = r.s1;
                if c as i32 != 0i32 {
                    w = w + height_plus_depth(f, c)
                }
                c = r.s2;
                if c as i32 != 0i32 {
                    w = w + height_plus_depth(f, c)
                }
                c = r.s3;
                if c as i32 != 0i32 {
                    w = w + height_plus_depth(f, c)
                }
                n = 0i32;
                if u > 0i32 {
                    while w < v {
                        w = w + u;
                        n += 1;
                        if r.s2 as i32 != 0i32 {
                            w = w + u
                        }
                    }
                }
                c = r.s1;
                if c as i32 != 0i32 {
                    stack_into_box(b, f, c);
                }
                c = r.s0;
                let mut for_end: i32 = 0;
                m = 1i32;
                for_end = n;
                if m <= for_end {
                    loop {
                        stack_into_box(b, f, c);
                        let fresh1 = m;
                        m = m + 1;
                        if !(fresh1 < for_end) {
                            break;
                        }
                    }
                }
                c = r.s2;
                if c as i32 != 0i32 {
                    stack_into_box(b, f, c);
                    c = r.s0;
                    let mut for_end_0: i32 = 0;
                    m = 1i32;
                    for_end_0 = n;
                    if m <= for_end_0 {
                        loop {
                            stack_into_box(b, f, c);
                            let fresh2 = m;
                            m = m + 1;
                            if !(fresh2 < for_end_0) {
                                break;
                            }
                        }
                    }
                }
                c = r.s3;
                if c as i32 != 0i32 {
                    stack_into_box(b, f, c);
                }
                MEM[(b + 2) as usize].b32.s1 = w - MEM[(b + 3) as usize].b32.s1
            } else {
                b = char_box(f, c as i32)
            }
        /*:736 */
        } else if !ot_assembly_ptr.is_null() {
            b = build_opentype_assembly(f, ot_assembly_ptr, v, false)
        } else {
            b = new_null_box();
            MEM[b as usize].b16.s1 = 1_u16;
            MEM[(b + 5) as usize].b32.s1 = get_node(5);
            MEM[MEM[(b + 5) as usize].b32.s1 as usize].b16.s1 = 8_u16;
            MEM[MEM[(b + 5) as usize].b32.s1 as usize].b16.s0 = 42_u16;
            MEM[(MEM[(b + 5) as usize].b32.s1 + 4) as usize].b16.s2 = f as u16;
            MEM[(MEM[(b + 5) as usize].b32.s1 + 4) as usize].b16.s1 = c;
            measure_native_glyph(
                &mut MEM[MEM[(b + 5) as usize].b32.s1 as usize] as *mut memory_word
                    as *mut libc::c_void,
                1i32,
            );
            MEM[(b + 1) as usize].b32.s1 = MEM[(MEM[(b + 5) as usize].b32.s1 + 1) as usize].b32.s1;
            MEM[(b + 3) as usize].b32.s1 = MEM[(MEM[(b + 5) as usize].b32.s1 + 3) as usize].b32.s1;
            MEM[(b + 2) as usize].b32.s1 = MEM[(MEM[(b + 5) as usize].b32.s1 + 2) as usize].b32.s1
        }
    } else {
        b = new_null_box();
        MEM[(b + 1) as usize].b32.s1 = *DIMENPAR(DimenPar::null_delimiter_space)
    }
    MEM[(b + 4) as usize].b32.s1 =
        half(MEM[(b + 3) as usize].b32.s1 - MEM[(b + 2) as usize].b32.s1) - axis_height(s);
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    b
}
unsafe extern "C" fn char_box(mut f: internal_font_number, mut c: i32) -> i32 {
    let mut q: b16x4 = b16x4 {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };
    let mut b: i32 = 0;
    let mut p: i32 = 0;
    if FONT_AREA[f as usize] as u32 == 0xffffu32 || FONT_AREA[f as usize] as u32 == 0xfffeu32 {
        b = new_null_box();
        p = new_native_character(f, c);
        MEM[(b + 5) as usize].b32.s1 = p;
        MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
        MEM[(b + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1;
        if MEM[(p + 2) as usize].b32.s1 < 0 {
            MEM[(b + 2) as usize].b32.s1 = 0
        } else {
            MEM[(b + 2) as usize].b32.s1 = MEM[(p + 2) as usize].b32.s1
        }
    } else {
        q = FONT_INFO[(CHAR_BASE[f as usize] + effective_char(true, f, c as u16)) as usize].b16;
        b = new_null_box();
        MEM[(b + 1) as usize].b32.s1 = FONT_INFO[(WIDTH_BASE[f as usize] + q.s3 as i32) as usize]
            .b32
            .s1
            + FONT_INFO[(ITALIC_BASE[f as usize] + q.s1 as i32 / 4) as usize]
                .b32
                .s1;
        MEM[(b + 3) as usize].b32.s1 = FONT_INFO
            [(HEIGHT_BASE[f as usize] + q.s2 as i32 / 16) as usize]
            .b32
            .s1;
        MEM[(b + 2) as usize].b32.s1 = FONT_INFO
            [(DEPTH_BASE[f as usize] + q.s2 as i32 % 16) as usize]
            .b32
            .s1;
        p = get_avail();
        MEM[p as usize].b16.s0 = c as u16;
        MEM[p as usize].b16.s1 = f as u16
    }
    MEM[(b + 5) as usize].b32.s1 = p;
    b
}
unsafe extern "C" fn stack_into_box(mut b: i32, mut f: internal_font_number, mut c: u16) {
    let mut p: i32 = 0;
    p = char_box(f, c as i32);
    MEM[p as usize].b32.s1 = MEM[(b + 5) as usize].b32.s1;
    MEM[(b + 5) as usize].b32.s1 = p;
    MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
}
unsafe extern "C" fn height_plus_depth(mut f: internal_font_number, mut c: u16) -> scaled_t {
    let mut q: b16x4 =
        FONT_INFO[(CHAR_BASE[f as usize] + effective_char(1i32 != 0, f, c)) as usize].b16;
    FONT_INFO[(HEIGHT_BASE[f as usize] + q.s2 as i32 / 16) as usize]
        .b32
        .s1
        + FONT_INFO[(DEPTH_BASE[f as usize] + q.s2 as i32 % 16) as usize]
            .b32
            .s1
}
unsafe extern "C" fn stack_glyph_into_box(mut b: i32, mut f: internal_font_number, mut g: i32) {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    p = get_node(5i32);
    MEM[p as usize].b16.s1 = 8_u16;
    MEM[p as usize].b16.s0 = 42_u16;
    MEM[(p + 4) as usize].b16.s2 = f as u16;
    MEM[(p + 4) as usize].b16.s1 = g as u16;
    measure_native_glyph(
        &mut MEM[p as usize] as *mut memory_word as *mut libc::c_void,
        1i32,
    );
    if MEM[b as usize].b16.s1 as i32 == 0 {
        q = MEM[(b + 5) as usize].b32.s1;
        if q == TEX_NULL {
            MEM[(b + 5) as usize].b32.s1 = p
        } else {
            while MEM[q as usize].b32.s1 != -0xfffffff {
                q = MEM[q as usize].b32.s1
            }
            MEM[q as usize].b32.s1 = p;
            if MEM[(b + 3) as usize].b32.s1 < MEM[(p + 3) as usize].b32.s1 {
                MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1
            }
            if MEM[(b + 2) as usize].b32.s1 < MEM[(p + 2) as usize].b32.s1 {
                MEM[(b + 2) as usize].b32.s1 = MEM[(p + 2) as usize].b32.s1
            }
        }
    } else {
        MEM[p as usize].b32.s1 = MEM[(b + 5) as usize].b32.s1;
        MEM[(b + 5) as usize].b32.s1 = p;
        MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
        if MEM[(b + 1) as usize].b32.s1 < MEM[(p + 1) as usize].b32.s1 {
            MEM[(b + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1
        }
    };
}
unsafe extern "C" fn stack_glue_into_box(mut b: i32, mut min: scaled_t, mut max: scaled_t) {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    q = new_spec(0i32);
    MEM[(q + 1) as usize].b32.s1 = min;
    MEM[(q + 2) as usize].b32.s1 = max - min;
    p = new_glue(q);
    if MEM[b as usize].b16.s1 as i32 == 0 {
        q = MEM[(b + 5) as usize].b32.s1;
        if q == TEX_NULL {
            MEM[(b + 5) as usize].b32.s1 = p
        } else {
            while MEM[q as usize].b32.s1 != -0xfffffff {
                q = MEM[q as usize].b32.s1
            }
            MEM[q as usize].b32.s1 = p
        }
    } else {
        MEM[p as usize].b32.s1 = MEM[(b + 5) as usize].b32.s1;
        MEM[(b + 5) as usize].b32.s1 = p;
        MEM[(b + 3) as usize].b32.s1 = MEM[(p + 3) as usize].b32.s1;
        MEM[(b + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1
    };
}
unsafe extern "C" fn build_opentype_assembly(
    mut f: internal_font_number,
    mut a: *mut libc::c_void,
    mut s: scaled_t,
    mut horiz: bool,
) -> i32 {
    let mut b: i32 = 0;
    let mut n: i32 = 0;
    let mut i: i32 = 0;
    let mut j: i32 = 0;
    let mut g: i32 = 0;
    let mut p: i32 = 0;
    let mut s_max: scaled_t = 0;
    let mut o: scaled_t = 0;
    let mut oo: scaled_t = 0;
    let mut prev_o: scaled_t = 0;
    let mut min_o: scaled_t = 0;
    let mut no_extenders: bool = false;
    let mut nat: scaled_t = 0;
    let mut str: scaled_t = 0;
    b = new_null_box();
    if horiz {
        MEM[b as usize].b16.s1 = 0_u16
    } else {
        MEM[b as usize].b16.s1 = 1_u16
    }
    n = -1i32;
    no_extenders = true;
    min_o = ot_min_connector_overlap(f);
    loop {
        n = n + 1i32;
        s_max = 0i32;
        prev_o = 0i32;
        let mut for_end: i32 = 0;
        i = 0i32;
        for_end = ot_part_count(a as *const GlyphAssembly) - 1i32;
        if i <= for_end {
            loop {
                if ot_part_is_extender(a as *const GlyphAssembly, i) {
                    no_extenders = false;
                    let mut for_end_0: i32 = 0;
                    j = 1i32;
                    for_end_0 = n;
                    if j <= for_end_0 {
                        loop {
                            o = ot_part_start_connector(f, a as *const GlyphAssembly, i);
                            if min_o < o {
                                o = min_o
                            }
                            if prev_o < o {
                                o = prev_o
                            }
                            s_max =
                                s_max - o + ot_part_full_advance(f, a as *const GlyphAssembly, i);
                            prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
                            let fresh3 = j;
                            j = j + 1;
                            if !(fresh3 < for_end_0) {
                                break;
                            }
                        }
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
                let fresh4 = i;
                i = i + 1;
                if !(fresh4 < for_end) {
                    break;
                }
            }
        }
        if s_max >= s || no_extenders as i32 != 0 {
            break;
        }
    }
    prev_o = 0i32;
    let mut for_end_1: i32 = 0;
    i = 0i32;
    for_end_1 = ot_part_count(a as *const GlyphAssembly) - 1i32;
    if i <= for_end_1 {
        loop {
            if ot_part_is_extender(a as *const GlyphAssembly, i) {
                let mut for_end_2: i32 = 0;
                j = 1i32;
                for_end_2 = n;
                if j <= for_end_2 {
                    loop {
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
                        let fresh5 = j;
                        j = j + 1;
                        if !(fresh5 < for_end_2) {
                            break;
                        }
                    }
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
                if oo > 0i32 {
                    stack_glue_into_box(b, -oo, -o);
                }
                g = ot_part_glyph(a as *const GlyphAssembly, i);
                stack_glyph_into_box(b, f, g);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
            }
            let fresh6 = i;
            i = i + 1;
            if !(fresh6 < for_end_1) {
                break;
            }
        }
    }
    p = MEM[(b + 5) as usize].b32.s1;
    nat = 0i32;
    str = 0i32;
    while p != TEX_NULL {
        if MEM[p as usize].b16.s1 as i32 == 8 {
            if horiz {
                nat = nat + MEM[(p + 1) as usize].b32.s1
            } else {
                nat = nat + MEM[(p + 3) as usize].b32.s1 + MEM[(p + 2) as usize].b32.s1
            }
        } else if MEM[p as usize].b16.s1 as i32 == 10 {
            nat = nat + MEM[(MEM[(p + 1) as usize].b32.s0 + 1) as usize].b32.s1;
            str = str + MEM[(MEM[(p + 1) as usize].b32.s0 + 2) as usize].b32.s1
        }
        p = MEM[p as usize].b32.s1
    }
    o = 0i32;
    if s > nat && str > 0i32 {
        o = s - nat;
        if o > str {
            o = str
        }
        MEM[(b + 5) as usize].b16.s0 = 0_u16;
        MEM[(b + 5) as usize].b16.s1 = 1_u16;
        MEM[(b + 6) as usize].gr = o as f64 / str as f64;
        if horiz {
            MEM[(b + 1) as usize].b32.s1 = nat + tex_round(str as f64 * MEM[(b + 6) as usize].gr)
        } else {
            MEM[(b + 3) as usize].b32.s1 = nat + tex_round(str as f64 * MEM[(b + 6) as usize].gr)
        }
    } else if horiz {
        MEM[(b + 1) as usize].b32.s1 = nat
    } else {
        MEM[(b + 3) as usize].b32.s1 = nat
    }
    b
}
unsafe extern "C" fn rebox(mut b: i32, mut w: scaled_t) -> i32 {
    let mut p: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut v: scaled_t = 0;
    if MEM[(b + 1) as usize].b32.s1 != w && MEM[(b + 5) as usize].b32.s1 != -0xfffffff {
        if MEM[b as usize].b16.s1 as i32 == 1 {
            b = hpack(b, 0i32, 1i32 as small_number)
        }
        p = MEM[(b + 5) as usize].b32.s1;
        if is_char_node(p) as i32 != 0 && MEM[p as usize].b32.s1 == -0xfffffff {
            f = MEM[p as usize].b16.s1 as internal_font_number;
            v = FONT_INFO[(WIDTH_BASE[f as usize]
                + FONT_INFO[(CHAR_BASE[f as usize]
                    + effective_char(1i32 != 0, f, MEM[p as usize].b16.s0))
                    as usize]
                    .b16
                    .s3 as i32) as usize]
                .b32
                .s1;
            if v != MEM[(b + 1) as usize].b32.s1 {
                MEM[p as usize].b32.s1 = new_kern(MEM[(b + 1) as usize].b32.s1 - v)
            }
        }
        free_node(b, 8i32);
        b = new_glue(12i32);
        MEM[b as usize].b32.s1 = p;
        while MEM[p as usize].b32.s1 != -0xfffffff {
            p = MEM[p as usize].b32.s1
        }
        MEM[p as usize].b32.s1 = new_glue(12);
        hpack(b, w, 0i32 as small_number)
    } else {
        MEM[(b + 1) as usize].b32.s1 = w;
        b
    }
}
