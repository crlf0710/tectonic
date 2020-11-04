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

use crate::cmd::*;
use crate::help;
use crate::node::*;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, Confuse};
use crate::xetex_ext::{Font, NativeFont::*};
use crate::xetex_ini::{
    adjust_tail, avail, cur_c, cur_dir, cur_f, cur_group, cur_i, cur_lang, cur_list,
    file_line_error_style_p, input_state_t, insert_src_special_every_math, just_box, memory_word,
    pre_adjust_tail, total_shrink, xtx_ligature_present, LR_problems, LR_ptr, CHAR_BASE,
    DEPTH_BASE, EQTB, EXTEN_BASE, FONT_BC, FONT_EC, FONT_INFO, FONT_LAYOUT_ENGINE, FONT_PARAMS,
    HEIGHT_BASE, ITALIC_BASE, KERN_BASE, LIG_KERN_BASE, MEM, NEST_PTR, NULL_CHARACTER, PARAM_BASE,
    SAVE_PTR, SAVE_STACK, SKEW_CHAR, WIDTH_BASE,
};
use crate::xetex_ini::{b16x4, b16x4_le_t};
use crate::xetex_layout_interface::*;
use crate::xetex_linebreak::line_break;
use crate::xetex_output::{
    print, print_chr, print_cstr, print_esc_cstr, print_file_line, print_int, print_nl_cstr,
    print_size,
};
use crate::xetex_pagebuilder::build_page;
use crate::xetex_scaledmath::{tex_round, x_over_n, xn_over_d, Scaled};
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
    is_char_node, llist_link, math_NODE_type, math_char, math_class, math_fam, set_NODE_type,
    set_class, set_family, set_math_NODE_type, LLIST_link, TeXInt, TeXOpt,
};

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
use crate::node::Delimeter;
pub(crate) const NULL_DELIMITER: Delimeter = Delimeter {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
static mut cur_mlist: i32 = 0;
static mut cur_style: (MathStyle, u8) = (MathStyle::Display, 0);
static mut cur_size: usize = 0;
static mut cur_mu: Scaled = Scaled::ZERO;
static mut mlist_penalties: bool = false;

pub(crate) unsafe fn init_math(input: &mut input_state_t) {
    let mut x: i32 = 0;
    let mut v: Scaled = Scaled::ZERO;

    let (tok, cmd, ..) = get_token(input);

    if cmd == Cmd::MathShift && cur_list.mode.0 == false {
        // 1180:
        let mut j = None;
        let mut w = -Scaled::MAX_HALFWORD;
        if cur_list.head == cur_list.tail {
            // 1520:
            pop_nest();
            x = if let Some(aux) = cur_list.eTeX_aux {
                if Math(aux).subtype_i32().dir() == LR::RightToLeft {
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
                new_kern(Scaled::ZERO)
            } else {
                new_param_glue(GluePar::right_skip)
            });

            let p = if *GLUEPAR(GluePar::left_skip) == 0 {
                new_kern(Scaled::ZERO)
            } else {
                new_param_glue(GluePar::left_skip)
            };

            *LLIST_link(p) = j.tex_int();

            let jb = List::from(just_box);
            let mut j_ = List::from(new_null_box());
            j = Some(j_.ptr());
            j_.set_width(jb.width());
            j_.set_shift_amount(jb.shift_amount());
            j_.set_list_ptr(Some(p).tex_int());
            j_.set_glue_order(jb.glue_order());
            j_.set_glue_sign(jb.glue_sign());
            j_.set_glue_set(jb.glue_set());

            v = jb.shift_amount();
            x = if let Some(aux) = cur_list.eTeX_aux {
                if Math(aux).subtype_i32().dir() == LR::RightToLeft {
                    -1
                } else {
                    1 // :1519
                }
            } else {
                0
            };
            let mut popt = if x >= 0 {
                let p = jb.list_ptr().opt();
                *LLIST_link(TEMP_HEAD) = None.tex_int();
                p
            } else {
                v = -v - jb.width();
                let p = new_math(Scaled::ZERO, MathType::Eq(BE::Begin, MathMode::Left));
                *LLIST_link(TEMP_HEAD) = p as i32;
                just_copy(
                    jb.list_ptr().opt(),
                    p,
                    new_math(Scaled::ZERO, MathType::Eq(BE::End, MathMode::Left)) as i32,
                );
                cur_dir = LR::RightToLeft;
                Some(p)
            };
            v = v + Scaled(
                FONT_INFO
                    [(QUAD_CODE + PARAM_BASE[EQTB[(CUR_FONT_LOC) as usize].val as usize]) as usize]
                    .b32
                    .s1,
            ) * 2;
            if *INTPAR(IntPar::texxet) > 0 {
                // 1497:
                let mut tmp_ptr = Math(get_avail()); /*1523:*/
                tmp_ptr.set_subtype_i32(MathType::Before); /*:1398 */
                *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
                LR_ptr = Some(tmp_ptr.ptr()).tex_int();
            }
            while let Some(mut p) = popt {
                let found;
                let d;
                match CharOrText::from(p) {
                    CharOrText::Char(c) => {
                        let f = c.font() as internal_font_number;
                        d = Scaled(
                            FONT_INFO[(WIDTH_BASE[f]
                                + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c.character()))
                                    as usize]
                                    .b16
                                    .s3 as i32) as usize]
                                .b32
                                .s1,
                        );
                        found = true;
                    }
                    CharOrText::Text(n) => match n {
                        TxtNode::List(b) => {
                            d = b.width();
                            found = true;
                        }
                        TxtNode::Rule(r) => {
                            d = r.width();
                            found = true;
                        }
                        TxtNode::Ligature(l) => {
                            let mut c = Char(GARBAGE);
                            c.set_character(l.char());
                            c.set_font(l.font());
                            *LLIST_link(GARBAGE) = *LLIST_link(l.ptr());
                            popt = Some(GARBAGE);
                            xtx_ligature_present = true;
                            continue;
                        }
                        TxtNode::Kern(k) => {
                            d = k.width();
                            found = false;
                        }
                        TxtNode::MarginKern(k) => {
                            d = k.width();
                            found = false;
                        }
                        TxtNode::Math(m) => {
                            d = m.width();
                            if *INTPAR(IntPar::texxet) > 0 {
                                /*1525: */
                                let (be, mode) = m.subtype().equ();
                                if be == BE::End {
                                    if Math(LR_ptr as usize).subtype_i32()
                                        == MathType::Eq(BE::End, mode)
                                    {
                                        let tmp_ptr = LR_ptr as usize;
                                        LR_ptr = *LLIST_link(tmp_ptr);
                                        *LLIST_link(tmp_ptr) = avail.tex_int();
                                        avail = Some(tmp_ptr);
                                    } else {
                                        match m.subtype() {
                                            MathType::Eq(_, MathMode::Left)
                                            | MathType::Eq(_, MathMode::Right) => {
                                                w = Scaled::MAX_HALFWORD;
                                                break;
                                            }
                                            _ => {}
                                        }
                                    }
                                } else {
                                    let mut tmp_ptr = Math(get_avail());
                                    tmp_ptr.set_subtype_i32(MathType::Eq(BE::End, mode));
                                    *LLIST_link(tmp_ptr.ptr()) = LR_ptr;
                                    LR_ptr = Some(tmp_ptr.ptr()).tex_int();
                                    if m.dir() != cur_dir {
                                        just_reverse(p);
                                        p = TEMP_HEAD;
                                    }
                                }
                                found = false;
                            } else {
                                match m.subtype() {
                                    MathType::Eq(_, MathMode::Left)
                                    | MathType::Eq(_, MathMode::Right) => {
                                        w = Scaled::MAX_HALFWORD;
                                        break;
                                    }
                                    _ => {
                                        found = false;
                                    }
                                }
                            }
                        }
                        TxtNode::Style(e) => {
                            d = e.width();
                            cur_dir = e.lr();
                            found = false;
                        }
                        TxtNode::Glue(p) => {
                            let q = GlueSpec(p.glue_ptr() as usize);
                            d = q.size();
                            let jb = List::from(just_box);
                            if jb.glue_sign() == GlueSign::Stretching {
                                if jb.glue_order() == q.stretch_order()
                                    && q.stretch() != Scaled::ZERO
                                {
                                    v = Scaled::MAX_HALFWORD
                                }
                            } else if jb.glue_sign() == GlueSign::Shrinking {
                                if jb.glue_order() == q.shrink_order() && q.shrink() != Scaled::ZERO
                                {
                                    v = Scaled::MAX_HALFWORD
                                }
                            }
                            if p.param() >= A_LEADERS {
                                found = true;
                            } else {
                                found = false;
                            }
                        }
                        TxtNode::WhatsIt(p) => match p {
                            WhatsIt::NativeWord(nw) => {
                                d = nw.width();
                                found = true;
                            }
                            WhatsIt::Glyph(g) => {
                                d = g.width();
                                found = true;
                            }
                            WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                                d = p.width();
                                found = true;
                            }
                            _ => {
                                d = Scaled::ZERO;
                                found = false;
                            }
                        },
                        _ => {
                            d = Scaled::ZERO;
                            found = false;
                        }
                    },
                }
                if found {
                    if v < Scaled::MAX_HALFWORD {
                        v = v + d;
                        w = v
                    } else {
                        w = Scaled::MAX_HALFWORD;
                        break;
                    }
                } else {
                    if v < Scaled::MAX_HALFWORD {
                        v = v + d
                    }
                }
                popt = llist_link(p);
            }
            if *INTPAR(IntPar::texxet) > 0 {
                while let Some(l) = LR_ptr.opt() {
                    let tmp_ptr = l;
                    LR_ptr = *LLIST_link(tmp_ptr);
                    *LLIST_link(tmp_ptr) = avail.tex_int();
                    avail = Some(tmp_ptr);
                }
                if LR_problems != 0 {
                    w = Scaled::MAX_HALFWORD;
                    LR_problems = 0
                }
            }
            cur_dir = LR::LeftToRight;
            flush_node_list(llist_link(TEMP_HEAD));
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
            s = Scaled(MEM[p - 1].b32.s1);
            l = Scaled(MEM[p].b32.s1);
        } else {
            if *DIMENPAR(DimenPar::hang_indent) != Scaled::ZERO
                && (*INTPAR(IntPar::hang_after) >= 0
                    && cur_list.prev_graf + 2 > *INTPAR(IntPar::hang_after)
                    || cur_list.prev_graf + 1 < -(*INTPAR(IntPar::hang_after) as i32))
            {
                l = *DIMENPAR(DimenPar::hsize) - (*DIMENPAR(DimenPar::hang_indent)).abs();
                if *DIMENPAR(DimenPar::hang_indent) > Scaled::ZERO {
                    s = *DIMENPAR(DimenPar::hang_indent)
                } else {
                    s = Scaled::ZERO
                }
            } else {
                l = *DIMENPAR(DimenPar::hsize);
                s = Scaled::ZERO
            }
        }
        push_math(GroupCode::MathShift);
        cur_list.mode = (false, ListMode::MMode);
        eq_word_define(INT_BASE as usize + IntPar::cur_fam as usize, -1i32);
        eq_word_define(
            DIMEN_BASE as usize + DimenPar::pre_display_size as usize,
            w.0,
        );
        cur_list.eTeX_aux = j;
        eq_word_define(
            INT_BASE as usize + IntPar::pre_display_correction as usize,
            x,
        );
        eq_word_define(DIMEN_BASE as usize + DimenPar::display_width as usize, l.0);
        eq_word_define(DIMEN_BASE as usize + DimenPar::display_indent as usize, s.0);
        if let Some(ed) = LOCAL(Local::every_display).opt() {
            begin_token_list(input, ed, Btl::EveryDisplayText);
        }
        if NEST_PTR == 1 {
            build_page(input);
        }
    } else {
        back_input(input, tok);
        push_math(GroupCode::MathShift);
        eq_word_define(INT_BASE as usize + IntPar::cur_fam as usize, -1);
        if insert_src_special_every_math {
            insert_src_special();
        }
        if let Some(em) = LOCAL(Local::every_math).opt() {
            begin_token_list(input, em, Btl::EveryMathText);
        }
    };
}
pub(crate) unsafe fn start_eq_no(input: &mut input_state_t, chr: i32) {
    SAVE_STACK[SAVE_PTR].val = chr; // push
    SAVE_PTR += 1;
    push_math(GroupCode::MathShift);
    eq_word_define(INT_BASE as usize + (IntPar::cur_fam as usize), -1);
    if insert_src_special_every_math {
        insert_src_special();
    }
    if let Some(em) = LOCAL(Local::every_math).opt() {
        begin_token_list(input, em, Btl::EveryMathText);
    };
}
pub(crate) unsafe fn math_limit_switch(chr: i32) {
    if cur_list.head != cur_list.tail {
        if MEM[cur_list.tail].b16.s1 == MathNode::Op as u16 {
            MEM[cur_list.tail].b16.s0 = chr as u16;
            return;
        }
    }
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr("! ");
    }
    print_cstr("Limit controls must follow a math operator");
    help!("I\'m ignoring this misplaced \\limits or \\nolimits command.");
    error();
}
unsafe fn scan_delimiter(
    input: &mut input_state_t,
    mut tok: i32,
    chr: i32,
    d: &mut Delimeter,
    r: bool,
) {
    let mut val = if r {
        if chr == 1 {
            let mut val1 = 0x40000000;
            let val = scan_math_fam_int(input);
            val1 += val * 0x200000;
            let val = scan_usv_num(input);
            val + val1
        } else {
            scan_delimiter_int(input)
        }
    } else {
        let mut cmd;
        let mut chr;
        loop {
            let next = get_x_token(input);
            tok = next.0;
            cmd = next.1;
            chr = next.2;
            if !(cmd == Cmd::Spacer || cmd == Cmd::Relax) {
                break;
            }
        }
        match cmd {
            Cmd::Letter | Cmd::OtherChar => EQTB[(DEL_CODE_BASE as i32 + chr) as usize].val,
            Cmd::DelimNum => {
                if chr == 1 {
                    let mut val1 = 0x40000000;
                    scan_math_class_int(input);
                    let val = scan_math_fam_int(input);
                    val1 += val * 0x20000;
                    let val = scan_usv_num(input);
                    val + val1
                } else {
                    scan_delimiter_int(input)
                }
            }
            _ => -1,
        }
    };
    if val < 0 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Missing delimiter (. inserted)");
        help!(
            "I was expecting to see something like `(\' or `\\{\' or",
            "`\\}\' here. If you typed, e.g., `{\' instead of `\\{\', you",
            "should probably delete the `{\' by typing `1\' now, so that",
            "braces don\'t get unbalanced. Otherwise just proceed.",
            "Acceptable delimiters are characters whose \\delcode is",
            "nonnegative, or you can use `\\delimiter <delimiter code>\'."
        );
        back_error(input, tok);
        val = 0;
    }
    if val >= 0x40000000i32 {
        d.s3 = (val % 0x200000 / 0x10000 * 0x100 + val / 0x200000i32 % 0x100i32) as u16;
        d.s2 = (val % 0x10000) as u16;
        d.s1 = 0_u16;
        d.s0 = 0_u16
    } else {
        d.s3 = (val / 0x100000 % 16) as u16;
        d.s2 = (val / 0x1000 % 0x100) as u16;
        d.s1 = (val / 0x100 % 16) as u16;
        d.s0 = (val % 0x100) as u16
    };
}
pub(crate) unsafe fn math_radical(input: &mut input_state_t, tok: i32, chr: i32) {
    let rn = Radical::from(get_node(RADICAL_NOAD_SIZE));
    *LLIST_link(cur_list.tail) = Some(rn.ptr()).tex_int();
    cur_list.tail = rn.ptr();
    set_math_NODE_type(rn.ptr(), MathNode::Radical);
    MEM[rn.ptr()].b16.s0 = NORMAL as u16;
    rn.first_mut().empty();
    rn.third_mut().empty();
    rn.second_mut().empty();
    scan_delimiter(input, tok, chr, rn.delimeter_mut(), true);
    scan_math(input, rn.first_mut(), rn.ptr() + 1);
}
pub(crate) unsafe fn math_ac(input: &mut input_state_t, cmd: Cmd, chr: i32) {
    if cmd == Cmd::Accent {
        /*1201: */
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Please use ");
        print_esc_cstr("mathaccent");
        print_cstr(" for accents in math mode");
        help!(
            "I\'m changing \\accent to \\mathaccent here; wish me luck.",
            "(Accents are not the same in formulas as they are in text.)"
        );
        error();
    }
    let mut acc = Accent::from(get_node(ACCENT_NOAD_SIZE));
    *LLIST_link(cur_list.tail) = Some(acc.ptr()).tex_int();
    cur_list.tail = acc.ptr();
    set_math_NODE_type(acc.ptr(), MathNode::Accent);
    MEM[acc.ptr()].b16.s0 = AccentType::Normal as _;
    acc.first_mut().empty();
    acc.third_mut().empty();
    acc.second_mut().empty();
    acc.fourth_mut().typ = MathCell::MathChar as _;
    let val = if chr == 1 {
        acc.set_accent_type(if scan_keyword(input, b"fixed") {
            AccentType::Fixed
        } else if scan_keyword(input, b"bottom") {
            if scan_keyword(input, b"fixed") {
                AccentType::BottomFixed
            } else {
                AccentType::Bottom
            }
        } else {
            AccentType::Normal
        });
        let val = scan_math_class_int(input);
        let mut c = set_class(val);
        let val = scan_math_fam_int(input);
        c += set_family(val);
        let val = scan_usv_num(input);
        val + c
    } else {
        let val = scan_fifteen_bit_int(input);
        set_class(val / 4096) + set_family(val % 4096 / 256) + (val % 256)
    };
    acc.fourth_mut().val.chr.character = (val as i64 % 65536) as u16;
    let font = if math_class(val) == 7
        && (*INTPAR(IntPar::cur_fam) >= 0 && *INTPAR(IntPar::cur_fam) < NUMBER_MATH_FAMILIES as i32)
    {
        *INTPAR(IntPar::cur_fam) as u16
    } else {
        math_fam(val) as u16
    };
    acc.fourth_mut().val.chr.font = (font as i64 + math_char(val) as i64 / 65536 * 256) as u16;
    scan_math(input, acc.first_mut(), acc.ptr() + 1);
}
pub(crate) unsafe fn append_choices(input: &mut input_state_t) {
    let c = new_choice();
    *LLIST_link(cur_list.tail) = Some(c).tex_int();
    cur_list.tail = c;
    SAVE_STACK[SAVE_PTR].val = 0; // push
    SAVE_PTR += 1;
    push_math(GroupCode::MathChoice);
    scan_left_brace(input);
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
                confusion("right");
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
pub(crate) unsafe fn build_choices(input: &mut input_state_t) {
    unsave(input);
    let mut p = fin_mlist(None).opt();
    let mut tail_choice = Choice(cur_list.tail);
    match MathStyle::n(SAVE_STACK[SAVE_PTR - 1].val as i16).unwrap() {
        MathStyle::Display => tail_choice.set_display(p),
        MathStyle::Text => tail_choice.set_text(p),
        MathStyle::Script => tail_choice.set_script(p),
        MathStyle::ScriptScript => {
            tail_choice.set_scriptscript(p);
            SAVE_PTR -= 1;
            return;
        }
    }
    SAVE_STACK[SAVE_PTR - 1].val += 1;
    push_math(GroupCode::MathChoice);
    scan_left_brace(input);
}
pub(crate) unsafe fn sub_sup(input: &mut input_state_t, cmd: Cmd) {
    let mut t = MathCell::Empty;
    let mut p = None;
    let cell = match cmd {
        Cmd::SupMark => 2,
        Cmd::SubMark => 3,
        _ => unreachable!(),
    };
    if cur_list.tail != cur_list.head {
        if MEM[cur_list.tail].b16.s1 >= MathNode::Ord as u16
            && MEM[cur_list.tail].b16.s1 < MathNode::Left as u16
        {
            let m = BaseMath(cur_list.tail);
            t = if cell == 2 {
                m.second().typ
            } else {
                m.third().typ
            };
            p = Some(cur_list.tail);
        }
    }
    let p = match (p, t) {
        (Some(p), MathCell::Empty) => p,
        _ => {
            /*1212: */
            *LLIST_link(cur_list.tail) = new_noad() as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
            if t != MathCell::Empty {
                if cmd == Cmd::SupMark {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Double superscript");
                    help!("I treat `x^1^2\' essentially like `x^1{}^2\'.");
                } else {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Double subscript");
                    help!("I treat `x_1_2\' essentially like `x_1{}_2\'.");
                }
                error();
            }
            cur_list.tail
        }
    };
    let p = p + cell;
    let m = BaseMath(cur_list.tail);
    if cell == 2 {
        scan_math(input, m.second_mut(), p);
    } else {
        scan_math(input, m.third_mut(), p);
    }
}
pub(crate) unsafe fn math_fraction(input: &mut input_state_t, tok: i32, chr: i32) {
    let mut c: i16 = 0;
    c = chr as i16;
    if cur_list.aux.b32.s1.opt().is_some() {
        /*1218:*/
        if c as i32 >= DELIMITED_CODE {
            scan_delimiter(
                input,
                tok,
                chr,
                &mut *(&mut MEM[GARBAGE] as *mut memory_word as *mut Delimeter),
                false,
            );
            scan_delimiter(
                input,
                tok,
                chr,
                &mut *(&mut MEM[GARBAGE] as *mut memory_word as *mut Delimeter),
                false,
            );
        }
        if c as i32 % DELIMITED_CODE == ABOVE_CODE {
            let _ = scan_dimen(input, false, false, None);
        }
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Ambiguous; you need another { and }");
        help!(
            "I\'m ignoring this fraction specification, since I don\'t",
            "know whether a construction like `x \\over y \\over z\'",
            "means `{x \\over y} \\over z\' or `x \\over {y \\over z}\'."
        );
        error();
    } else {
        let mut a = Fraction(get_node(FRACTION_NOAD_SIZE));
        cur_list.aux.b32.s1 = Some(a.ptr()).tex_int();
        set_math_NODE_type(a.ptr(), MathNode::Fraction);
        MEM[a.ptr()].b16.s0 = NORMAL as u16;
        a.second_mut().set_submlist(MEM[cur_list.head].b32.s1);
        a.third_mut().empty();
        *a.left_delimeter_mut() = NULL_DELIMITER;
        *a.right_delimeter_mut() = NULL_DELIMITER;
        MEM[cur_list.head].b32.s1 = None.tex_int();
        cur_list.tail = cur_list.head;
        if c as i32 >= DELIMITED_CODE {
            scan_delimiter(input, tok, chr, a.left_delimeter_mut(), false);
            scan_delimiter(input, tok, chr, a.right_delimeter_mut(), false);
        }
        match c as i32 % DELIMITED_CODE {
            ABOVE_CODE => {
                let val = scan_dimen(input, false, false, None);
                a.set_thickness(val);
            }
            OVER_CODE => {
                a.set_thickness(DEFAULT_CODE);
            }
            ATOP_CODE => {
                a.set_thickness(Scaled::ZERO);
            }
            _ => {}
        }
    };
}
pub(crate) unsafe fn math_left_right(
    input: &mut input_state_t,
    group: GroupCode,
    tok: i32,
    cmd: Cmd,
    chr: i32,
) {
    let mut q: i32 = 0;
    let mut t = chr as i16;
    if t != MathNode::Left as i16 && group != GroupCode::MathLeft {
        /*1227: */
        if group == GroupCode::MathShift {
            scan_delimiter(
                input,
                tok,
                chr,
                &mut *(&mut MEM[GARBAGE] as *mut memory_word as *mut Delimeter),
                false,
            ); /*:1530 */
            if file_line_error_style_p != 0 {
                print_file_line(); /*:1530 */
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Extra ");
            if t as u16 == 1 {
                print_esc_cstr("middle");
                help!("I\'m ignoring a \\middle that had no matching \\left.");
            } else {
                print_esc_cstr("right");
                help!("I\'m ignoring a \\right that had no matching \\left.");
            }
            error();
        } else {
            off_save(input, group, tok, cmd, chr);
        }
    } else {
        let p = new_noad() as usize;
        MEM[p].b16.s1 = t as u16;
        scan_delimiter(input, tok, chr, LeftRight(p).delimeter_mut(), false);
        if t == 1 {
            MEM[p].b16.s1 = MathNode::Right as u16;
            MEM[p].b16.s0 = 1;
        }
        if t == MathNode::Left as i16 {
            q = p as i32;
        } else {
            q = fin_mlist(Some(p));
            unsave(input);
        }
        if t != MathNode::Right as i16 {
            push_math(GroupCode::MathLeft);
            MEM[cur_list.head].b32.s1 = q;
            cur_list.tail = p;
            cur_list.eTeX_aux = Some(p);
        } else {
            *LLIST_link(cur_list.tail) = new_noad() as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
            MEM[cur_list.tail].b16.s1 = MathNode::Inner as u16;
            MEM[cur_list.tail + 1].b32.s1 = MathCell::SubMList as _;
            MEM[cur_list.tail + 1].b32.s0 = q
        }
    };
}
unsafe fn app_display(j: Option<usize>, mut b: List, mut d: Scaled) {
    let mut z: Scaled = Scaled::ZERO;
    let mut s: Scaled = Scaled::ZERO;
    let mut e: Scaled = Scaled::ZERO;
    let mut x: i32 = 0;
    s = *DIMENPAR(DimenPar::display_indent);
    x = *INTPAR(IntPar::pre_display_correction);
    if x == 0 {
        b.set_shift_amount(s + d);
    } else {
        let mut q;
        z = *DIMENPAR(DimenPar::display_width);
        let mut p = b;
        if x > 0 {
            e = z - d - p.width();
        } else {
            e = d;
            d = z - e - p.width();
        }
        if let Some(j) = j {
            b = List::from(copy_node_list(Some(j)) as usize);
            b.set_height(p.height()).set_depth(p.depth());
            s = s - b.shift_amount();
            d = d + s;
            e = e + b.width() - z - s
        }
        let p = if p.lr_mode() == LRMode::DList {
            q = p.ptr();
            p.ptr()
        } else {
            let r = p.list_ptr().opt();
            free_node(p.ptr(), BOX_NODE_SIZE);
            let mut r = r.confuse("LR4");
            let mut p;
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
            p
        };
        let r;
        let t;
        if j.is_none() {
            r = new_kern(Scaled::ZERO);
            t = new_kern(Scaled::ZERO);
        } else {
            r = b.list_ptr() as usize;
            t = *LLIST_link(r) as usize;
        }
        let u = new_math(Scaled::ZERO, MathType::Eq(BE::End, MathMode::Middle));
        let j = if let TxtNode::Glue(t) = TxtNode::from(r) {
            let (j, mut tmp_ptr) = new_skip_param(GluePar::right_skip);
            *LLIST_link(q as usize) = Some(j).tex_int();
            *LLIST_link(j) = Some(u).tex_int();
            let j = GlueSpec(t.glue_ptr() as usize);
            tmp_ptr
                .set_stretch_order(j.stretch_order())
                .set_shrink_order(j.shrink_order())
                .set_size(e - j.size())
                .set_stretch(-j.stretch())
                .set_shrink(-j.shrink());
            *LLIST_link(u) = Some(t.ptr()).tex_int();
            Some(j.ptr())
        } else {
            MEM[t + 1].b32.s1 = e.0;
            *LLIST_link(t) = Some(u).tex_int();
            *LLIST_link(q as usize) = Some(t).tex_int();
            j
        };
        let u = new_math(Scaled::ZERO, MathType::Eq(BE::Begin, MathMode::Middle));
        if let TxtNode::Glue(r) = TxtNode::from(r) {
            let (j, mut tmp_ptr) = new_skip_param(GluePar::left_skip);
            *LLIST_link(u) = Some(j).tex_int();
            *LLIST_link(j) = Some(p).tex_int();
            let j = GlueSpec(r.glue_ptr() as usize);
            tmp_ptr
                .set_stretch_order(j.stretch_order())
                .set_shrink_order(j.shrink_order())
                .set_size(d - j.size())
                .set_stretch(-j.stretch())
                .set_shrink(-j.shrink());
            *LLIST_link(r.ptr()) = Some(u).tex_int();
        } else {
            MEM[r + 1].b32.s1 = d.0;
            *LLIST_link(r) = Some(p).tex_int();
            *LLIST_link(u) = Some(r).tex_int();
            if j.is_none() {
                b = hpack(Some(u), Scaled::ZERO, PackMode::Additional);
                b.set_shift_amount(s);
            } else {
                b.set_list_ptr(Some(u).tex_int());
            }
        }
    }
    append_to_vlist(b);
}
pub(crate) unsafe fn after_math(input: &mut input_state_t) {
    let mut l: bool = false;
    let mut danger: bool = false;
    let mut p: i32 = 0;
    let mut j = None;

    danger = false;

    if cur_list.mode == (false, ListMode::MMode) {
        j = cur_list.eTeX_aux; // :1530
    }
    if FONT_PARAMS[MATH_FONT(2 + TEXT_SIZE)] < TOTAL_MATHSY_PARAMS
        && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + TEXT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Math formula deleted: Insufficient symbol fonts");

        help!(
            "Sorry, but I can\'t typeset math unless \\textfont 2",
            "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
            "the \\fontdimen values needed in math symbol fonts."
        );
        error();
        flush_math();
        danger = true
    } else if FONT_PARAMS[MATH_FONT(3 + TEXT_SIZE)] < TOTAL_MATHEX_PARAMS
        && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + TEXT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
    {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Math formula deleted: Insufficient extension fonts");

        help!(
            "Sorry, but I can\'t typeset math unless \\textfont 3",
            "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
            "the \\fontdimen values needed in math extension fonts."
        );
        error();
        flush_math();
        danger = true
    }
    let mut m = cur_list.mode;
    l = false;
    p = fin_mlist(None);
    let a = if cur_list.mode == (!m.0, m.1) {
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
        cur_mlist = p;
        cur_style = (MathStyle::Text, 0);
        mlist_penalties = false;
        mlist_to_hlist();
        let mut a = hpack(llist_link(TEMP_HEAD), Scaled::ZERO, PackMode::Additional);
        a.set_lr_mode(LRMode::DList);
        unsave(input);
        SAVE_PTR -= 1;
        if SAVE_STACK[SAVE_PTR].val == 1 {
            l = true
        }
        danger = false;
        if cur_list.mode == (false, ListMode::MMode) {
            j = cur_list.eTeX_aux
        }
        if FONT_PARAMS[MATH_FONT(2 + TEXT_SIZE)] < TOTAL_MATHSY_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + TEXT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
            || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
                && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
            || FONT_PARAMS[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS
                && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(2 + SCRIPT_SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Math formula deleted: Insufficient symbol fonts");

            help!(
                "Sorry, but I can\'t typeset math unless \\textfont 2",
                "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
                "the \\fontdimen values needed in math symbol fonts."
            );
            error();
            flush_math();
            danger = true
        } else if FONT_PARAMS[MATH_FONT(3 + TEXT_SIZE)] < TOTAL_MATHEX_PARAMS
            && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + TEXT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
            || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
                && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
            || FONT_PARAMS[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS
                && !matches!(&FONT_LAYOUT_ENGINE[MATH_FONT(3 + SCRIPT_SCRIPT_SIZE)], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Math formula deleted: Insufficient extension fonts");

            help!(
                "Sorry, but I can\'t typeset math unless \\textfont 3",
                "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
                "the \\fontdimen values needed in math extension fonts."
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
        let m = new_math(*DIMENPAR(DimenPar::math_surround), MathType::Before);
        *LLIST_link(cur_list.tail) = Some(m).tex_int();
        cur_list.tail = m;
        cur_mlist = p;
        cur_style = (MathStyle::Text, 0);
        mlist_penalties = cur_list.mode.0 == false;
        mlist_to_hlist();
        *LLIST_link(cur_list.tail) = *LLIST_link(TEMP_HEAD);
        while let Some(next) = LLIST_link(cur_list.tail).opt() {
            cur_list.tail = next;
        }
        *LLIST_link(cur_list.tail) =
            new_math(*DIMENPAR(DimenPar::math_surround), MathType::After) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        cur_list.aux.b32.s0 = 1000;
        unsave(input);
    } else {
        if a.is_none() {
            // 1232:
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
        cur_mlist = p;
        cur_style = (MathStyle::Display, 0);
        mlist_penalties = false;
        mlist_to_hlist();
        let p = llist_link(TEMP_HEAD);
        adjust_tail = Some(ADJUST_HEAD);
        pre_adjust_tail = Some(PRE_ADJUST_HEAD);
        let mut b = hpack(p, Scaled::ZERO, PackMode::Additional);
        let p = b.list_ptr().opt();
        let t = adjust_tail.unwrap();
        adjust_tail = None;
        let pre_t = pre_adjust_tail.unwrap();
        pre_adjust_tail = None;
        let mut w = b.width();
        let z = *DIMENPAR(DimenPar::display_width);
        let mut s = *DIMENPAR(DimenPar::display_indent);
        if *INTPAR(IntPar::pre_display_correction) < 0i32 {
            s = -s - z
        }
        let (mut e, q) = if danger {
            (Scaled::ZERO, Scaled::ZERO)
        } else if let Some(a) = a {
            let e = a.width();
            (e, e + math_quad(TEXT_SIZE))
        } else {
            (Scaled::ZERO, Scaled::ZERO)
        };
        if w + q > z {
            // 1236:
            if e != Scaled::ZERO
                && (w - total_shrink[NORMAL as usize] + q <= z
                    || total_shrink[FIL as usize] != Scaled::ZERO
                    || total_shrink[FILL as usize] != Scaled::ZERO
                    || total_shrink[FILLL as usize] != Scaled::ZERO)
            {
                free_node(b.ptr(), BOX_NODE_SIZE);
                b = hpack(p, z - q, PackMode::Exactly);
            } else {
                e = Scaled::ZERO;
                if w > z {
                    free_node(b.ptr(), BOX_NODE_SIZE);
                    b = hpack(p, z, PackMode::Exactly);
                }
            }
            w = b.width();
        }
        b.set_lr_mode(LRMode::DList);
        let mut d = (z - w).half();
        if e > Scaled::ZERO && d < e * 2 {
            d = (z - w - e).half();
            if let Some(CharOrText::Text(TxtNode::Glue(_))) = p.map(CharOrText::from) {
                d = Scaled::ZERO;
            }
        }
        let mut g1;
        let mut g2;
        *LLIST_link(cur_list.tail) = new_penalty(*INTPAR(IntPar::pre_display_penalty)) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        if d + s <= *DIMENPAR(DimenPar::pre_display_size) || l {
            g1 = GluePar::above_display_skip;
            g2 = Some(GluePar::below_display_skip);
        } else {
            g1 = GluePar::above_display_short_skip;
            g2 = Some(GluePar::below_display_short_skip);
        }
        if l && e == Scaled::ZERO {
            app_display(j, a.unwrap(), Scaled::ZERO);
            let pen = new_penalty(INF_PENALTY);
            *LLIST_link(cur_list.tail) = Some(pen).tex_int();
            cur_list.tail = pen;
        } else {
            let pg = new_param_glue(g1);
            *LLIST_link(cur_list.tail) = Some(pg).tex_int();
            cur_list.tail = pg;
        }
        if e != Scaled::ZERO {
            let a = a.unwrap();
            let r = new_kern(z - w - e - d);
            if l {
                *LLIST_link(a.ptr()) = Some(r).tex_int();
                *LLIST_link(r) = Some(b.ptr()).tex_int();
                b = a;
                d = Scaled::ZERO;
            } else {
                *LLIST_link(b.ptr()) = Some(r).tex_int();
                *LLIST_link(r) = Some(a.ptr()).tex_int();
            }
            b = hpack(Some(b.ptr()), Scaled::ZERO, PackMode::Additional);
        }
        app_display(j, b, d);
        if let Some(a) = a {
            if e == Scaled::ZERO && !l {
                *LLIST_link(cur_list.tail) = new_penalty(INF_PENALTY) as i32;
                cur_list.tail = *LLIST_link(cur_list.tail) as usize;
                let w = a.width();
                app_display(j, a, z - w);
                g2 = None;
            }
        }
        if t != ADJUST_HEAD {
            *LLIST_link(cur_list.tail) = *LLIST_link(ADJUST_HEAD as usize);
            cur_list.tail = t;
        }
        if pre_t != PRE_ADJUST_HEAD {
            *LLIST_link(cur_list.tail) = *LLIST_link(PRE_ADJUST_HEAD as usize);
            cur_list.tail = pre_t;
        }
        *LLIST_link(cur_list.tail) = new_penalty(*INTPAR(IntPar::post_display_penalty)) as i32;
        cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        if let Some(g2) = g2 {
            *LLIST_link(cur_list.tail) = new_param_glue(g2) as i32;
            cur_list.tail = *LLIST_link(cur_list.tail) as usize;
        }
        flush_node_list(j);
        resume_after_display(input);
    };
}
pub(crate) unsafe fn resume_after_display(input: &mut input_state_t) {
    if cur_group != GroupCode::MathShift {
        confusion("display");
    }
    unsave(input);
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
    let (tok, cmd, ..) = get_x_token(input);
    if cmd != Cmd::Spacer {
        back_input(input, tok);
    }
    if NEST_PTR == 1 {
        build_page(input);
    };
}
/* Copyright 2016-2018 The Tectonic Project
 * Licensed under the MIT License.
 */
unsafe fn math_x_height(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
            get_native_mathsy_param(f, X_HEIGHT_CODE)
        }
        _ => Scaled(FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn math_quad(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
            get_native_mathsy_param(f, QUAD_CODE)
        }
        _ => Scaled(FONT_INFO[(QUAD_CODE + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn num1(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 8),
        _ => Scaled(FONT_INFO[(8 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn num2(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 9i32),
        _ => Scaled(FONT_INFO[(9 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn num3(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 10),
        _ => Scaled(FONT_INFO[(10 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn denom1(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 11),
        _ => Scaled(FONT_INFO[(11 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn denom2(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 12),
        _ => Scaled(FONT_INFO[(12 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sup1(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 13),
        _ => Scaled(FONT_INFO[(13 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sup2(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 14),
        _ => Scaled(FONT_INFO[(14 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sup3(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 15),
        _ => Scaled(FONT_INFO[(15 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sub1(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 16),
        _ => Scaled(FONT_INFO[(16 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sub2(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 17),
        _ => Scaled(FONT_INFO[(17 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sup_drop(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 18),
        _ => Scaled(FONT_INFO[(18 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn sub_drop(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code) as usize;
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 19),
        _ => Scaled(FONT_INFO[(19 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn delim1(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 20),
        _ => Scaled(FONT_INFO[(20 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn delim2(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 21),
        _ => Scaled(FONT_INFO[(21 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn axis_height(size_code: usize) -> Scaled {
    let f = MATH_FONT(2 + size_code);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathsy_param(f, 22),
        _ => Scaled(FONT_INFO[(22 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn default_rule_thickness() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 8),
        _ => Scaled(FONT_INFO[(8 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn big_op_spacing1() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 9),
        _ => Scaled(FONT_INFO[(9 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn big_op_spacing2() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 10),
        _ => Scaled(FONT_INFO[(10 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn big_op_spacing3() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 11),
        _ => Scaled(FONT_INFO[(11 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn big_op_spacing4() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 12),
        _ => Scaled(FONT_INFO[(12 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn big_op_spacing5() -> Scaled {
    let f = MATH_FONT(3 + cur_size);
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => get_native_mathex_param(f, 13),
        _ => Scaled(FONT_INFO[(13 + PARAM_BASE[f]) as usize].b32.s1),
    }
}
unsafe fn fraction_rule(t: Scaled) -> Rule {
    let mut p = new_rule();
    p.set_height(t).set_depth(Scaled::ZERO);
    p
}
unsafe fn overbar(b: List, k: Scaled, t: Scaled) -> List {
    let p = new_kern(k);
    *LLIST_link(p) = Some(b.ptr()).tex_int();
    let q = fraction_rule(t).ptr();
    *LLIST_link(q) = Some(p).tex_int();
    let p = new_kern(t);
    *LLIST_link(p) = Some(q).tex_int();
    vpackage(
        Some(p),
        Scaled::ZERO,
        PackMode::Additional,
        Scaled::MAX_HALFWORD,
    )
}
unsafe fn math_glue(g: &GlueSpec, m: Scaled) -> usize {
    let (n, mut f) = x_over_n(m, 65536);
    let mut n = n.0;
    if f < Scaled::ZERO {
        n = n - 1;
        f = f + Scaled(65536);
    }
    let mut p = GlueSpec(get_node(GLUE_SPEC_SIZE));
    p.set_size(g.size().mul_add(n, xn_over_d(g.size(), f, 65536).0));
    p.set_stretch_order(g.stretch_order());
    if p.stretch_order() == GlueOrder::Normal {
        p.set_stretch(g.stretch().mul_add(n, xn_over_d(g.stretch(), f, 65536).0));
    } else {
        p.set_stretch(g.stretch());
    }
    p.set_shrink_order(g.shrink_order());
    if p.shrink_order() == GlueOrder::Normal {
        p.set_shrink(g.shrink().mul_add(n, xn_over_d(g.shrink(), f, 65536).0));
    } else {
        p.set_shrink(g.shrink());
    }
    p.ptr()
}
unsafe fn math_kern(p: usize, m: Scaled) {
    let mut k = Kern(p);
    if k.subtype() == KernType::Math {
        let (n, mut f) = x_over_n(m, 65536);
        let mut n = n.0;
        if f < Scaled::ZERO {
            n = n - 1;
            f = f + Scaled(65536);
        }
        k.set_width(k.width().mul_add(n, xn_over_d(k.width(), f, 65536).0));
        k.set_subtype(KernType::Explicit);
    };
}
pub(crate) unsafe fn flush_math() {
    flush_node_list(MEM[cur_list.head].b32.s1.opt());
    flush_node_list(cur_list.aux.b32.s1.opt());
    *LLIST_link(cur_list.head) = None.tex_int();
    cur_list.tail = cur_list.head;
    cur_list.aux.b32.s1 = None.tex_int();
}
unsafe fn clean_box(p: &MCell, s: (MathStyle, u8)) -> List {
    match p.typ {
        MathCell::MathChar => {
            cur_mlist = new_noad() as i32;
            BaseMath(cur_mlist as usize).first_mut().set(p);
        }
        MathCell::SubBox => {
            return found(p.val.ptr);
        }
        MathCell::SubMList => {
            cur_mlist = p.val.ptr;
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
    cur_mu = x_over_n(math_quad(cur_size), 18).0;

    unsafe fn found(q: i32) -> List {
        let x = match q.opt() {
            None => hpack(None, Scaled::ZERO, PackMode::Additional),
            Some(q) => match Node::from(q) {
                Node::Text(TxtNode::List(b))
                    if b.shift_amount() == Scaled::ZERO
                        && LLIST_link(q as usize).opt().is_none() =>
                {
                    b
                }
                _ => hpack(Some(q), Scaled::ZERO, PackMode::Additional),
            },
        };
        let q = x.list_ptr();
        if is_char_node(q.opt()) {
            if let Some(r) = LLIST_link(q as usize).opt() {
                if llist_link(r).is_none() {
                    if let CharOrText::Text(TxtNode::Kern(k)) = CharOrText::from(r) {
                        k.free();
                        *LLIST_link(q as usize) = None.tex_int();
                    }
                }
            }
        }
        x
    }
    found(q)
}
pub(crate) unsafe fn fetch(a: &mut MCell) {
    cur_c = a.val.chr.character as i32;
    cur_f = MATH_FONT(a.val.chr.font as usize % 256 + cur_size);
    cur_c = (cur_c as i64 + (a.val.chr.font as i32 / 256) as i64 * 65536) as i32;
    if cur_f == FONT_BASE {
        // 749:
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("");
        print_size(cur_size as i32);
        print_chr(' ');
        print_int(a.val.chr.font as i32 % 256);
        print_cstr(" is undefined (character ");
        print(cur_c);
        print_chr(')');

        help!(
            "Somewhere in the math formula just ended, you used the",
            "stated character from an undefined font family. For example,",
            "plain TeX doesn\'t allow \\it or \\sl in subscripts. Proceed,",
            "and I\'ll try to forget that I needed that character."
        );
        error();
        cur_i = NULL_CHARACTER;
        a.typ = MathCell::Empty;
    } else if let Font::Native(_) = &FONT_LAYOUT_ENGINE[cur_f as usize] {
        cur_i = NULL_CHARACTER
    } else {
        if cur_c >= FONT_BC[cur_f as usize] as i32 && cur_c <= FONT_EC[cur_f as usize] as i32 {
            cur_i = FONT_INFO[(CHAR_BASE[cur_f as usize] + cur_c) as usize].b16
        } else {
            cur_i = NULL_CHARACTER
        }
        if !(cur_i.s3 as i32 > 0) {
            char_warning(cur_f, cur_c);
            a.typ = MathCell::Empty;
        }
    };
}
unsafe fn make_over(q: &mut Over) {
    q.first_mut().set_subbox(overbar(
        clean_box(q.first(), (cur_style.0, 1)),
        default_rule_thickness() * 3,
        default_rule_thickness(),
    ));
}
unsafe fn make_under(q: &mut Under) {
    let x = clean_box(q.first(), cur_style);
    let p = new_kern(default_rule_thickness() * 3);
    *LLIST_link(x.ptr()) = Some(p).tex_int();
    *LLIST_link(p) = Some(fraction_rule(default_rule_thickness()).ptr()).tex_int();
    let mut y = vpackage(
        Some(x.ptr()),
        Scaled::ZERO,
        PackMode::Additional,
        Scaled::MAX_HALFWORD,
    );
    let delta = y.height() + y.depth() + default_rule_thickness();
    y.set_height(x.height());
    let h = y.height();
    y.set_depth(delta - h);
    q.first_mut().set_subbox(y);
}
unsafe fn make_vcenter(q: usize) {
    match &mut Node::from(MEM[q + 1].b32.s0 as usize) {
        Node::Text(TxtNode::List(v)) if v.is_vertical() => {
            let delta = v.height() + v.depth();
            v.set_height(axis_height(cur_size) + delta.half());
            let d = v.height();
            v.set_depth(delta - d);
        }
        _ => confusion("vcenter"),
    }
}
unsafe fn make_radical(q: &mut Radical) {
    let f = MATH_FONT(q.delimeter().s3 as usize % 256 + cur_size);
    let rule_thickness = match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
            get_ot_math_constant(f, RADICALRULETHICKNESS)
        }
        _ => default_rule_thickness(),
    };
    let x = clean_box(q.first(), (cur_style.0, 1));
    let mut clr = match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
            if cur_style.0 == MathStyle::Display {
                get_ot_math_constant(f, RADICALDISPLAYSTYLEVERTICALGAP)
            } else {
                get_ot_math_constant(f, RADICALVERTICALGAP)
            }
        }
        _ => {
            if cur_style.0 == MathStyle::Display {
                rule_thickness + math_x_height(cur_size).abs() / 4
            } else {
                let clr = rule_thickness;
                clr + clr.abs() / 4
            }
        }
    };
    let mut y = List::from(var_delimiter(
        q.delimeter(),
        cur_size,
        x.height() + x.depth() + clr + rule_thickness,
    ));
    match &FONT_LAYOUT_ENGINE[f] {
        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
            let h = y.height();
            let d = y.depth();
            y.set_depth(h + d - rule_thickness);
            y.set_height(rule_thickness);
        }
        _ => {}
    }
    let delta = y.depth() - (x.height() + x.depth() + clr);
    if delta > Scaled::ZERO {
        clr += delta.half();
    }
    y.set_shift_amount(-(x.height() + clr));
    *LLIST_link(y.ptr()) = Some(overbar(x, clr, y.height()).ptr()).tex_int();
    q.first_mut()
        .set_subbox(hpack(Some(y.ptr()), Scaled::ZERO, PackMode::Additional));
}
unsafe fn compute_ot_math_accent_pos(p: &mut Accent) -> Scaled {
    match p.first().typ {
        MathCell::MathChar => {
            p.first_mut().fetch();
            let q = new_native_character(cur_f, cur_c);
            let g = q.native_glyph(0) as i32;
            get_ot_math_accent_pos(cur_f, g)
        }
        MathCell::SubMList => match p.first().val.ptr.opt() {
            Some(r) if math_NODE_type(r).unwrap() == MathNode::Accent => {
                compute_ot_math_accent_pos(&mut Accent::from(r))
            }
            _ => Scaled::INFINITY,
        },
        _ => Scaled::INFINITY,
    }
}
unsafe fn make_math_accent(q: &mut Accent) {
    let mut a: i32 = 0;
    let mut c: i32 = 0;
    let mut f: internal_font_number = 0;
    let mut s = Scaled::ZERO;
    let mut h: Scaled = Scaled::ZERO;
    let mut w: Scaled = Scaled::ZERO;
    let mut w2: Scaled = Scaled::ZERO;
    let mut ot_assembly_ptr: *mut libc::c_void = 0 as *mut libc::c_void;
    q.fourth_mut().fetch();
    ot_assembly_ptr = 0 as *mut libc::c_void;
    let x = if let Font::Native(_) = &FONT_LAYOUT_ENGINE[cur_f as usize] {
        c = cur_c;
        f = cur_f;
        s = match q.accent_type() {
            AccentType::Bottom | AccentType::BottomFixed => Scaled::ZERO,
            _ => compute_ot_math_accent_pos(q),
        };
        let x = clean_box(q.first(), (cur_style.0, 1));
        w = x.width();
        h = x.height();
        Some(x)
    } else if cur_i.s3 as i32 > 0 {
        let mut i = cur_i;
        c = cur_c;
        f = cur_f;
        s = Scaled::ZERO;
        if q.first().typ == MathCell::MathChar {
            q.first_mut().fetch();
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
                                s = Scaled(
                                    FONT_INFO[(KERN_BASE[cur_f as usize]
                                        + 256 * cur_i.s1 as i32
                                        + cur_i.s0 as i32)
                                        as usize]
                                        .b32
                                        .s1,
                                );
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
        let x = clean_box(q.first(), (cur_style.0, 1));
        w = x.width();
        h = x.height();
        while !(i.s1 as i32 % 4 != LIST_TAG) {
            let y = i.s0 as i32;
            i = FONT_INFO[(CHAR_BASE[f] + y) as usize].b16;
            if !(i.s3 as i32 > 0) {
                break;
            }
            if Scaled(FONT_INFO[(WIDTH_BASE[f] + i.s3 as i32) as usize].b32.s1) > w {
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
        let mut delta = match &FONT_LAYOUT_ENGINE[f] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => match q.accent_type() {
                AccentType::Bottom | AccentType::BottomFixed => Scaled::ZERO,
                _ => h.min(get_ot_math_constant(f, ACCENTBASEHEIGHT)),
            },
            _ => h.min(Scaled(
                FONT_INFO[(X_HEIGHT_CODE + PARAM_BASE[f]) as usize].b32.s1,
            )),
        };
        if q.second().typ != MathCell::Empty || q.third().typ != MathCell::Empty {
            if q.first().typ == MathCell::MathChar {
                // 769:
                flush_node_list(Some(x.ptr()));
                let xn = new_noad();
                MEM[xn + 1] = MEM[q.ptr() + 1];
                MEM[xn + 2] = MEM[q.ptr() + 2];
                MEM[xn + 3] = MEM[q.ptr() + 3];
                q.second_mut().empty();
                q.third_mut().empty();
                q.first_mut().set_submlist(Some(xn).tex_int());
                x = clean_box(q.first(), cur_style);
                delta += x.height() - h;
                h = x.height();
            }
        }
        let mut y = char_box(f, c);
        if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
            let mut p = Glyph::new_node();
            p.set_font(f as u16);
            let nw = NativeWord::from(y.list_ptr() as usize);
            p.set_glyph(nw.native_glyph(0));
            p.set_metrics(true);
            nw.free();
            y.set_list_ptr(Some(p.ptr()).tex_int());
            let (p, whd) = match q.accent_type() {
                AccentType::Fixed | AccentType::BottomFixed => {
                    p.set_metrics(true);
                    (p.ptr(), (p.width(), p.height(), p.depth()))
                }
                _ => {
                    c = p.glyph() as i32;
                    a = 0;
                    loop {
                        let g = get_ot_math_variant(f, c, a, &mut w2, 1);
                        if w2 > Scaled::ZERO && w2 <= w {
                            p.set_glyph(g as u16);
                            p.set_metrics(true);
                            a += 1
                        }
                        if w2 < Scaled::ZERO || w2 >= w {
                            break;
                        }
                    }
                    if w2 < Scaled::ZERO {
                        ot_assembly_ptr = get_ot_assembly_ptr(f, c, 1);
                        if !ot_assembly_ptr.is_null() {
                            free_node(p.ptr(), GLYPH_NODE_SIZE);
                            let b =
                                build_opentype_assembly(f, ot_assembly_ptr, w, ListDir::Horizontal);
                            y.set_list_ptr(Some(b.ptr()).tex_int());
                            (b.ptr(), (b.width(), b.height(), b.depth()))
                        } else {
                            (p.ptr(), (p.width(), p.height(), p.depth()))
                        }
                    } else {
                        p.set_metrics(true);
                        (p.ptr(), (p.width(), p.height(), p.depth()))
                    }
                }
            };
            y.set_width(whd.0).set_height(whd.1).set_depth(whd.2);
            if q.accent_type() == AccentType::Bottom || q.accent_type() == AccentType::BottomFixed {
                if y.height() < Scaled::ZERO {
                    y.set_height(Scaled::ZERO);
                }
            } else if y.depth() < Scaled::ZERO {
                y.set_depth(Scaled::ZERO);
            }
            let mut sa;
            if let CharOrText::Text(TxtNode::WhatsIt(WhatsIt::Glyph(p))) = CharOrText::from(p) {
                sa = get_ot_math_accent_pos(f, p.glyph() as i32);
                if sa == Scaled::INFINITY {
                    sa = y.width().half();
                }
            } else {
                sa = y.width().half();
            }
            if q.accent_type() == AccentType::Bottom
                || q.accent_type() == AccentType::BottomFixed
                || s == Scaled::INFINITY
            {
                s = w.half();
            }
            y.set_shift_amount(s - sa);
        } else {
            y.set_shift_amount(s + (w - y.width()).half());
        }
        y.set_width(Scaled::ZERO);
        match q.accent_type() {
            AccentType::Bottom | AccentType::BottomFixed => {
                *LLIST_link(x.ptr()) = Some(y.ptr()).tex_int();
                y = vpackage(
                    Some(x.ptr()),
                    Scaled::ZERO,
                    PackMode::Additional,
                    Scaled::MAX_HALFWORD,
                );
                y.set_shift_amount(-(h - y.height()));
            }
            _ => {
                let p = new_kern(-delta);
                *LLIST_link(p) = Some(x.ptr()).tex_int();
                *LLIST_link(y.ptr()) = p as i32;
                y = vpackage(
                    Some(y.ptr()),
                    Scaled::ZERO,
                    PackMode::Additional,
                    Scaled::MAX_HALFWORD,
                );
                if y.height() < h {
                    // 765:
                    let p = new_kern(h - y.height()); /*773:*/
                    *LLIST_link(p) = y.list_ptr();
                    y.set_list_ptr(Some(p).tex_int());
                    y.set_height(h);
                }
            }
        }
        y.set_width(x.width());
        q.first_mut().set_subbox(y);
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
}
unsafe fn make_fraction(q: &mut Fraction) {
    if q.thickness() == DEFAULT_CODE {
        q.set_thickness(default_rule_thickness());
    }
    let mut x = clean_box(
        q.second(),
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
        q.third(),
        (
            match cur_style.0 {
                MathStyle::Display => MathStyle::Text,
                MathStyle::Text => MathStyle::Script,
                MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
            },
            1,
        ),
    );
    if x.width() < z.width() {
        x = rebox(x, z.width())
    } else {
        z = rebox(z, x.width())
    }
    let mut shift_up;
    let mut shift_down;
    if cur_style.0 == MathStyle::Display {
        shift_up = num1(cur_size);
        shift_down = denom1(cur_size)
    } else {
        shift_down = denom2(cur_size);
        shift_up = if q.thickness() != Scaled::ZERO {
            num2(cur_size)
        } else {
            num3(cur_size)
        };
    }
    let delta;
    if q.thickness() == Scaled::ZERO {
        /*772:*/
        let clr = match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                if cur_style.0 == MathStyle::Display {
                    get_ot_math_constant(cur_f, STACKDISPLAYSTYLEGAPMIN)
                } else {
                    get_ot_math_constant(cur_f, STACKGAPMIN)
                }
            }
            _ => {
                if cur_style.0 == MathStyle::Display {
                    default_rule_thickness() * 7
                } else {
                    default_rule_thickness() * 3
                }
            }
        };
        delta = (clr - (shift_up - x.depth() - (z.height() - shift_down))).half();
        if delta > Scaled::ZERO {
            shift_up = shift_up + delta;
            shift_down = shift_down + delta
        }
    } else {
        let delta1;
        let delta2;
        match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                delta = q.thickness().half();
                let clr = if cur_style.0 == MathStyle::Display {
                    get_ot_math_constant(cur_f, FRACTIONNUMDISPLAYSTYLEGAPMIN)
                } else {
                    get_ot_math_constant(cur_f, FRACTIONNUMERATORGAPMIN)
                };
                delta1 = clr - (shift_up - x.depth() - (axis_height(cur_size) + delta));
                let clr = if cur_style.0 == MathStyle::Display {
                    get_ot_math_constant(cur_f, FRACTIONDENOMDISPLAYSTYLEGAPMIN)
                } else {
                    get_ot_math_constant(cur_f, FRACTIONDENOMINATORGAPMIN)
                };
                delta2 = clr - (axis_height(cur_size) - delta - (z.height() - shift_down))
            }
            _ => {
                let clr = if cur_style.0 == MathStyle::Display {
                    q.thickness() * 3
                } else {
                    q.thickness()
                };
                delta = q.thickness().half();
                delta1 = clr - (shift_up - x.depth() - (axis_height(cur_size) + delta));
                delta2 = clr - (axis_height(cur_size) - delta - (z.height() - shift_down))
            }
        }
        if delta1 > Scaled::ZERO {
            shift_up += delta1;
        }
        if delta2 > Scaled::ZERO {
            shift_down += delta2;
        }
    }
    let mut v = List::from(new_null_box());
    v.set_vertical();
    v.set_height(shift_up + x.height())
        .set_depth(z.depth() + shift_down)
        .set_width(x.width());
    let mut p;
    if q.thickness() == Scaled::ZERO {
        p = new_kern(shift_up - x.depth() - (z.height() - shift_down));
        *LLIST_link(p) = Some(z.ptr()).tex_int();
    } else {
        let y = fraction_rule(q.thickness()).ptr();
        p = new_kern(axis_height(cur_size) - delta - (z.height() - shift_down));
        *LLIST_link(y) = Some(p).tex_int();
        *LLIST_link(p) = Some(z.ptr()).tex_int();
        p = new_kern(shift_up - x.depth() - (axis_height(cur_size) + delta));
        *LLIST_link(p) = Some(y).tex_int();
    }
    *LLIST_link(x.ptr()) = Some(p).tex_int();
    v.set_list_ptr(Some(x.ptr()).tex_int());
    // :774
    let delta = if cur_style.0 == MathStyle::Display {
        delim1(cur_size)
    } else {
        delim2(cur_size)
    };
    let x = var_delimiter(q.left_delimeter(), cur_size, delta);
    *LLIST_link(x) = Some(v.ptr()).tex_int();
    let z = var_delimiter(q.right_delimeter(), cur_size, delta);
    *LLIST_link(v.ptr()) = Some(z).tex_int();
    MEM[q.ptr() + 1].b32.s1 =
        Some(hpack(Some(x), Scaled::ZERO, PackMode::Additional).ptr()).tex_int();
    // :775
}
unsafe fn make_op(q: &mut Operator) -> Scaled {
    if q.limits() == Limit::Normal && cur_style.0 == MathStyle::Display {
        q.set_limits(Limit::Limits);
    }
    let mut delta = Scaled::ZERO;
    let mut ot_assembly_ptr = ptr::null_mut();
    if q.first().typ == MathCell::MathChar {
        let mut c = 0;
        q.first_mut().fetch();
        match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.using_open_type() => {}
            _ => {
                if cur_style.0 == MathStyle::Display && cur_i.s1 as i32 % 4 == LIST_TAG {
                    c = cur_i.s0;
                    let i = FONT_INFO[(CHAR_BASE[cur_f as usize] + c as i32) as usize].b16;
                    if i.s3 as i32 > 0 {
                        cur_c = c as i32;
                        cur_i = i;
                        q.first_mut().val.chr.character = c
                    }
                }
                delta = Scaled(
                    FONT_INFO[(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4i32) as usize]
                        .b32
                        .s1,
                )
            }
        }
        let mut x = clean_box(q.first(), cur_style);
        match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                if let Some(mut p) = x.list_ptr().opt() {
                    if let CharOrText::Text(TxtNode::WhatsIt(WhatsIt::Glyph(mut p))) =
                        CharOrText::from(p)
                    {
                        let mut ital_corr = true;
                        let mut width = p.width();
                        let mut depth = p.depth();
                        let mut height = p.height();
                        if cur_style.0 == MathStyle::Display {
                            let mut h1 = get_ot_math_constant(cur_f, DISPLAYOPERATORMINHEIGHT);
                            if (h1.0 as f64) < ((p.height() + p.depth()) * 5).0 as f64 / 4_f64 {
                                h1 =
                                    Scaled((((p.height() + p.depth()) * 5).0 as f64 / 4_f64) as i32)
                            }
                            c = p.glyph();
                            let mut n = 0;
                            let mut h2 = Scaled::ZERO;
                            loop {
                                let g = get_ot_math_variant(cur_f, c as i32, n, &mut h2, 0);
                                if h2 > Scaled::ZERO {
                                    p.set_glyph(g as u16);
                                    p.set_metrics(true);
                                }
                                n += 1;
                                if h2 < Scaled::ZERO || h2 >= h1 {
                                    break;
                                }
                            }
                            if h2 < Scaled::ZERO {
                                ot_assembly_ptr = get_ot_assembly_ptr(cur_f, c as i32, 0);
                                if !ot_assembly_ptr.is_null() {
                                    free_node(p.ptr(), GLYPH_NODE_SIZE);
                                    let b = build_opentype_assembly(
                                        cur_f,
                                        ot_assembly_ptr,
                                        h1,
                                        ListDir::Vertical,
                                    );
                                    x.set_list_ptr(Some(b.ptr()).tex_int());
                                    delta = Scaled::ZERO;
                                    width = b.width();
                                    depth = b.depth();
                                    height = b.height();
                                    ital_corr = false;
                                }
                            } else {
                                p.set_metrics(true);
                            }
                        }
                        if ital_corr {
                            delta = get_ot_math_ital_corr(cur_f, p.glyph() as i32);
                            width = p.width();
                            depth = p.depth();
                            height = p.height();
                        }
                        x.set_width(width).set_height(depth).set_depth(height);
                    }
                }
            }
            _ => {}
        }
        if q.third().typ != MathCell::Empty && q.limits() != Limit::Limits {
            let w = x.width();
            x.set_width(w - delta);
        }
        x.set_shift_amount((x.height() - x.depth()).half() - axis_height(cur_size));
        q.first_mut().set_subbox(x);
    }
    let save_f = cur_f;
    if q.limits() == Limit::Limits {
        // 777:
        let x = clean_box(
            q.second(),
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                cur_style.1,
            ),
        );
        let y = clean_box(q.first(), cur_style);
        let z = clean_box(
            q.third(),
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                1,
            ),
        );
        let mut v = List::from(new_null_box());
        v.set_vertical();
        v.set_width((y.width()).max(x.width()).max(z.width()));
        let mut x = rebox(x, v.width());
        let y = rebox(y, v.width());
        let mut z = rebox(z, v.width());
        x.set_shift_amount(delta.half());
        z.set_shift_amount(-x.shift_amount());
        v.set_height(y.height());
        v.set_depth(y.depth());
        cur_f = save_f;
        if q.second().typ == MathCell::Empty {
            x.free();
            v.set_list_ptr(y.ptr() as i32);
        } else {
            let mut shift_up = big_op_spacing3() - x.depth();
            if shift_up < big_op_spacing1() {
                shift_up = big_op_spacing1()
            }
            let p = new_kern(shift_up);
            *LLIST_link(p) = Some(y.ptr()).tex_int();
            *LLIST_link(x.ptr()) = p as i32;
            let p = new_kern(big_op_spacing5());
            *LLIST_link(p) = Some(x.ptr()).tex_int();
            v.set_list_ptr(p as i32);
            let h = v.height();
            v.set_height(h + big_op_spacing5() + x.height() + x.depth() + shift_up);
        }
        if q.third().typ == MathCell::Empty {
            z.free();
        } else {
            let mut shift_down = big_op_spacing4() - z.height();
            if shift_down < big_op_spacing2() {
                shift_down = big_op_spacing2()
            }
            let p = new_kern(shift_down);
            *LLIST_link(y.ptr()) = Some(p).tex_int();
            *LLIST_link(p) = Some(z.ptr()).tex_int();
            let p = new_kern(big_op_spacing5());
            *LLIST_link(z.ptr()) = Some(p).tex_int();
            let d = v.depth();
            v.set_depth(d + big_op_spacing5() + z.height() + z.depth() + shift_down);
        }
        MEM[q.ptr() + 1].b32.s1 = Some(v.ptr()).tex_int() // TODO: strange
    }
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    delta
}
unsafe fn make_ord(q: &mut Ord) {
    while q.third().typ == MathCell::Empty {
        if !(q.second().typ == MathCell::Empty) {
            break;
        }
        if !(q.first().typ == MathCell::MathChar) {
            break;
        }
        let p = llist_link(q.ptr());
        if p.is_none() {
            break;
        }
        let mut p = p.unwrap();
        match Node::from(p) {
            Node::Math(m) => match m {
                MathNode::Ord
                | MathNode::Op
                | MathNode::Bin
                | MathNode::Rel
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct => {}
                _ => break,
            },
            _ => break,
        }
        let p = BaseMath(p);
        if !(p.first().typ == MathCell::MathChar) {
            break;
        }
        if !(p.first().val.chr.font % 256 == q.first().val.chr.font % 256) {
            break;
        }
        q.first_mut().typ = MathCell::MathTextChar;
        q.first_mut().fetch();
        if !(cur_i.s1 as i32 % 4 == LIG_TAG) {
            break;
        }
        let mut a = LIG_KERN_BASE[cur_f as usize] + cur_i.s0 as i32;
        cur_c = p.first().val.chr.character as i32;
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
                        let p = new_kern(Scaled(
                            FONT_INFO[(KERN_BASE[cur_f as usize]
                                + 256 * cur_i.s1 as i32
                                + cur_i.s0 as i32) as usize]
                                .b32
                                .s1,
                        ));
                        *LLIST_link(p) = *LLIST_link(q.ptr());
                        *LLIST_link(q.ptr()) = Some(p).tex_int();
                        return;
                    } else {
                        match cur_i.s1 as i32 {
                            1 | 5 => q.first_mut().val.chr.character = cur_i.s0,
                            2 | 6 => p.first_mut().val.chr.character = cur_i.s0,
                            3 | 7 | 11 => {
                                let mut r = BaseMath(new_noad());
                                r.first_mut().val.chr.character = cur_i.s0;
                                r.first_mut().val.chr.font =
                                    (q.first().val.chr.font as i32 % 256) as u16;
                                *LLIST_link(q.ptr()) = Some(r.ptr()).tex_int();
                                *LLIST_link(r.ptr()) = Some(p.ptr()).tex_int();
                                r.first_mut().typ = if (cur_i.s1 as i32) < 11 {
                                    MathCell::MathChar
                                } else {
                                    MathCell::MathTextChar
                                };
                            }
                            _ => {
                                *LLIST_link(q.ptr()) = *LLIST_link(p.ptr());
                                q.first_mut().val.chr.character = cur_i.s0;
                                q.third_mut().set(p.third());
                                q.second_mut().set(p.second());
                                free_node(p.ptr(), NOAD_SIZE);
                            }
                        }
                        if cur_i.s1 as i32 > 3 {
                            return;
                        }
                        q.first_mut().typ = MathCell::MathChar;
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
unsafe fn attach_hkern_to_new_hlist(q: &mut BaseMath, mut delta: Scaled) -> usize {
    let mut z = new_kern(delta);
    if let Some(mut y) = MEM[q.ptr() + 1].b32.s1.opt() {
        while let Some(next) = llist_link(y) {
            y = next;
        }
        *LLIST_link(y) = Some(z).tex_int();
    } else {
        MEM[q.ptr() + 1].b32.s1 = Some(z).tex_int();
    }
    MEM[q.ptr() + 1].b32.s1 as usize
}
unsafe fn make_scripts(q: &mut BaseMath, mut delta: Scaled) {
    let mut shift_up: Scaled = Scaled::ZERO;
    let mut shift_down: Scaled = Scaled::ZERO;
    let mut clr: Scaled = Scaled::ZERO;
    let mut p = MEM[q.ptr() + 1].b32.s1;
    let mut script_g = 0_u16;
    let mut script_f = 0;
    let mut sup_kern = Scaled::ZERO;
    let mut sub_kern = Scaled::ZERO;
    match p.opt().map(Node::from) {
        Some(Node::Char(_)) | Some(Node::Text(TxtNode::WhatsIt(WhatsIt::Glyph(_)))) => {
            shift_up = Scaled::ZERO;
            shift_down = Scaled::ZERO;
        }
        _ => {
            let z = hpack(p.opt(), Scaled::ZERO, PackMode::Additional);
            let t = match cur_style.0 {
                MathStyle::Display | MathStyle::Text => SCRIPT_SIZE,
                MathStyle::Script | MathStyle::ScriptScript => SCRIPT_SCRIPT_SIZE,
            };
            shift_up = z.height() - sup_drop(t);
            shift_down = z.depth() + sub_drop(t);
            z.free();
        }
    }
    let mut x;
    if q.second().typ == MathCell::Empty {
        // 784:
        let save_f = cur_f;
        x = clean_box(
            q.third(),
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                1,
            ),
        );
        cur_f = save_f;
        let w = x.width();
        x.set_width(w + *DIMENPAR(DimenPar::script_space));
        shift_down = shift_down.max(sub1(cur_size));
        clr = match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                x.height() - get_ot_math_constant(cur_f, SUBSCRIPTTOPMAX)
            }
            _ => x.height() - (math_x_height(cur_size) * 4).abs() / 5,
        };
        if shift_down < clr {
            shift_down = clr;
        }
        x.set_shift_amount(shift_down);
        if matches!(&FONT_LAYOUT_ENGINE[cur_f as usize], Font::Native(Otgr(eng)) if eng.is_open_type_math_font())
        {
            /*787: */
            if q.third().typ == MathCell::MathChar {
                let save_f = cur_f;
                q.third_mut().fetch();
                match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                    Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                        let script_c = new_native_character(cur_f, cur_c);
                        script_g = script_c.native_glyph(0);
                        script_f = cur_f;
                    }
                    _ => {
                        script_g = 0;
                        script_f = 0;
                    }
                }
                cur_f = save_f
            }
            if let Some(Node::Text(TxtNode::WhatsIt(WhatsIt::Glyph(p)))) = p.opt().map(Node::from) {
                sub_kern = get_ot_math_kern(
                    p.font() as usize,
                    p.glyph() as i32,
                    script_f,
                    script_g as i32,
                    SUB_CMD,
                    shift_down,
                )
            }
            if sub_kern != Scaled::ZERO {
                p = attach_hkern_to_new_hlist(q, sub_kern) as i32;
            }
        }
    } else {
        let save_f = cur_f;
        x = clean_box(
            q.second(),
            (
                match cur_style.0 {
                    MathStyle::Display | MathStyle::Text => MathStyle::Script,
                    MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                },
                cur_style.1,
            ),
        );
        cur_f = save_f;
        let w = x.width();
        x.set_width(w + *DIMENPAR(DimenPar::script_space));
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
        clr = match &FONT_LAYOUT_ENGINE[cur_f as usize] {
            Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                x.depth() + get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMIN)
            }
            _ => x.depth() + math_x_height(cur_size).abs() / 4,
        };
        if shift_up < clr {
            shift_up = clr
        }
        if matches!(&FONT_LAYOUT_ENGINE[cur_f as usize], Font::Native(Otgr(e)) if e.is_open_type_math_font())
        {
            // 788:
            if q.second().typ == MathCell::MathChar {
                let save_f = cur_f;
                q.second_mut().fetch();
                match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                    Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                        let script_c = new_native_character(cur_f, cur_c);
                        script_g = script_c.native_glyph(0);
                        script_f = cur_f
                    }
                    _ => {
                        script_g = 0;
                        script_f = 0;
                    }
                }
                cur_f = save_f
            }
            if let Some(Node::Text(TxtNode::WhatsIt(WhatsIt::Glyph(p)))) = p.opt().map(Node::from) {
                sup_kern = get_ot_math_kern(
                    p.font() as usize,
                    p.glyph() as i32,
                    script_f,
                    script_g as i32,
                    SUP_CMD,
                    shift_up,
                )
            }
            if sup_kern != Scaled::ZERO && q.third().typ == MathCell::Empty {
                p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
            }
        }
        if q.third().typ == MathCell::Empty {
            x.set_shift_amount(-shift_up);
        } else {
            // 786:
            let save_f = cur_f;
            let mut y = clean_box(
                q.third(),
                (
                    match cur_style.0 {
                        MathStyle::Display | MathStyle::Text => MathStyle::Script,
                        MathStyle::Script | MathStyle::ScriptScript => MathStyle::ScriptScript,
                    },
                    1,
                ),
            );
            cur_f = save_f;
            let w = y.width();
            y.set_width(w + *DIMENPAR(DimenPar::script_space));
            if shift_down < sub2(cur_size) {
                shift_down = sub2(cur_size)
            }
            match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                    clr = get_ot_math_constant(cur_f, SUBSUPERSCRIPTGAPMIN)
                        - (shift_up - x.depth() - (y.height() - shift_down))
                }
                _ => {
                    clr = default_rule_thickness() * 4
                        - (shift_up - x.depth() - (y.height() - shift_down))
                }
            }
            if clr > Scaled::ZERO {
                shift_down = shift_down + clr;
                match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                    Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                        clr = get_ot_math_constant(cur_f, SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT)
                            - (shift_up - x.depth())
                    }
                    _ => clr = (math_x_height(cur_size) * 4).abs() / 5 - (shift_up - x.depth()),
                }
                if clr > Scaled::ZERO {
                    shift_up = shift_up + clr;
                    shift_down = shift_down - clr
                }
            }
            if matches!(&FONT_LAYOUT_ENGINE[cur_f as usize], Font::Native(Otgr(eng)) if eng.is_open_type_math_font())
            {
                if q.third().typ == MathCell::MathChar {
                    let save_f = cur_f;
                    q.third_mut().fetch();
                    match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                            let script_c = new_native_character(cur_f, cur_c);
                            script_g = script_c.native_glyph(0);
                            script_f = cur_f
                        }
                        _ => {
                            script_g = 0;
                            script_f = 0;
                        }
                    }
                    cur_f = save_f
                }
                if let Some(Node::Text(TxtNode::WhatsIt(WhatsIt::Glyph(p)))) =
                    p.opt().map(Node::from)
                {
                    sub_kern = get_ot_math_kern(
                        p.font() as usize,
                        p.glyph() as i32,
                        script_f,
                        script_g as i32,
                        SUB_CMD,
                        shift_down,
                    )
                }
                if sub_kern != Scaled::ZERO {
                    p = attach_hkern_to_new_hlist(q, sub_kern) as i32;
                }
                if q.second().typ == MathCell::MathChar {
                    let save_f = cur_f;
                    q.second_mut().fetch();
                    match &FONT_LAYOUT_ENGINE[cur_f as usize] {
                        Font::Native(Otgr(e)) if e.is_open_type_math_font() => {
                            let script_c = new_native_character(cur_f, cur_c);
                            script_g = script_c.native_glyph(0);
                            script_f = cur_f
                        }
                        _ => {
                            script_g = 0;
                            script_f = 0;
                        }
                    }
                    cur_f = save_f
                }
                if let Some(Node::Text(TxtNode::WhatsIt(WhatsIt::Glyph(p)))) =
                    p.opt().map(Node::from)
                {
                    sup_kern = get_ot_math_kern(
                        p.font() as usize,
                        p.glyph() as i32,
                        script_f,
                        script_g as i32,
                        SUP_CMD,
                        shift_up,
                    )
                }
                if sup_kern != Scaled::ZERO && q.third().typ == MathCell::Empty {
                    p = attach_hkern_to_new_hlist(q, sup_kern) as i32;
                }
            }
            x.set_shift_amount(sup_kern + delta - sub_kern);
            let p = new_kern(shift_up - x.depth() - (y.height() - shift_down));
            *LLIST_link(x.ptr()) = Some(p).tex_int();
            *LLIST_link(p) = Some(y.ptr()).tex_int();
            x = vpackage(
                Some(x.ptr()),
                Scaled::ZERO,
                PackMode::Additional,
                Scaled::MAX_HALFWORD,
            );
            x.set_shift_amount(shift_down);
        }
    }
    if let Some(mut p) = MEM[q.ptr() + 1].b32.s1.opt() {
        while let Some(next) = llist_link(p) {
            p = next;
        }
        *LLIST_link(p) = Some(x.ptr()).tex_int();
    } else {
        MEM[q.ptr() + 1].b32.s1 = Some(x.ptr()).tex_int();
    }
}
unsafe fn make_left_right(
    mut q: &mut LeftRight,
    mut style: (MathStyle, u8),
    mut max_d: Scaled,
    mut max_h: Scaled,
) -> MathNode {
    cur_style = style;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18).0;
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
    MEM[q.ptr() + 1].b32.s1 = var_delimiter(q.delimeter(), cur_size, delta) as i32;
    match math_NODE_type(q.ptr()).unwrap() {
        MathNode::Left => MathNode::Open,
        MathNode::Right => MathNode::Close,
        _ => unreachable!(),
    }
}
unsafe fn mlist_to_hlist() {
    let mlist = cur_mlist;
    let penalties = mlist_penalties;
    let style = cur_style;
    let mut qopt = mlist.opt();
    let mut r = None;
    let mut r_type = MathNode::Op;
    let mut max_h = Scaled::ZERO;
    let mut max_d = Scaled::ZERO;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18).0;
    while let Some(q) = qopt {
        // 753:
        match &mut Node::from(q) {
            Node::Math(n) => {
                let mut delta = Scaled::ZERO; /*:755 */
                let mut flag = true;
                match n {
                    MathNode::Bin => match r_type {
                        MathNode::Bin
                        | MathNode::Op
                        | MathNode::Rel
                        | MathNode::Open
                        | MathNode::Punct
                        | MathNode::Left => {
                            set_math_NODE_type(q, MathNode::Ord);
                            continue;
                        }
                        _ => {}
                    },
                    MathNode::Rel | MathNode::Close | MathNode::Punct => {
                        if let (Some(r), MathNode::Bin) = (r, r_type) {
                            set_math_NODE_type(r, MathNode::Ord);
                        }
                    }
                    MathNode::Right => {
                        if let (Some(r), MathNode::Bin) = (r, r_type) {
                            set_math_NODE_type(r, MathNode::Ord);
                        }
                        flag = false;
                    }
                    MathNode::Left => {
                        flag = false;
                    }
                    MathNode::Fraction => {
                        make_fraction(&mut Fraction(q));
                        flag = false;
                        /*check_dimensions */
                        let z = hpack(MEM[q + 1].b32.s1.opt(), Scaled::ZERO, PackMode::Additional);
                        max_h = max_h.max(z.height());
                        max_d = max_d.max(z.depth());
                        z.free();
                    }
                    MathNode::Op => {
                        delta = make_op(&mut Operator::from(q));
                        if MEM[q].b16.s0 == Limit::Limits as u16 {
                            flag = false;
                            /*check_dimensions */
                            let z =
                                hpack(MEM[q + 1].b32.s1.opt(), Scaled::ZERO, PackMode::Additional);
                            max_h = max_h.max(z.height());
                            max_d = max_d.max(z.depth());
                            z.free();
                        }
                    }
                    MathNode::Ord => {
                        make_ord(&mut Ord::from(q));
                    }
                    MathNode::Open | MathNode::Inner => {}
                    MathNode::Radical => {
                        make_radical(&mut Radical::from(q));
                    }
                    MathNode::Over => {
                        make_over(&mut Over::from(q));
                    }
                    MathNode::Under => {
                        make_under(&mut Under::from(q));
                    }
                    MathNode::Accent => {
                        make_math_accent(&mut Accent::from(q));
                    }
                    MathNode::VCenter => {
                        make_vcenter(q);
                    }
                }
                if flag {
                    let mut q = BaseMath(q);
                    let p = match q.first().typ {
                        MathCell::MathChar | MathCell::MathTextChar => {
                            q.first_mut().fetch();
                            if let Font::Native(_) = &FONT_LAYOUT_ENGINE[cur_f as usize] {
                                let z = new_native_character(cur_f, cur_c);
                                let mut p = Glyph::new_node();
                                p.set_font(cur_f as u16);
                                p.set_glyph(z.native_glyph(0));
                                p.set_metrics(true);
                                z.free();
                                delta = get_ot_math_ital_corr(cur_f, p.glyph() as i32);
                                if q.first().typ == MathCell::MathTextChar
                                    && !matches!(&FONT_LAYOUT_ENGINE[cur_f as usize], Font::Native(Otgr(e)) if e.is_open_type_math_font())
                                {
                                    delta = Scaled::ZERO;
                                }
                                if q.third().typ == MathCell::Empty && delta != Scaled::ZERO {
                                    *LLIST_link(p.ptr()) = Some(new_kern(delta)).tex_int();
                                    delta = Scaled::ZERO;
                                }
                                Some(p.ptr())
                            } else if cur_i.s3 as i32 > 0 {
                                delta = Scaled(
                                    FONT_INFO[(ITALIC_BASE[cur_f as usize] + cur_i.s1 as i32 / 4)
                                        as usize]
                                        .b32
                                        .s1,
                                );
                                let p = new_character(cur_f, cur_c as UTF16_code);
                                if q.first().typ == MathCell::MathTextChar
                                    && FONT_INFO[(2 + PARAM_BASE[cur_f as usize]) as usize].b32.s1
                                        != 0
                                {
                                    delta = Scaled::ZERO;
                                }
                                if q.third().typ == MathCell::Empty && delta != Scaled::ZERO {
                                    *LLIST_link(p.unwrap()) = Some(new_kern(delta)).tex_int();
                                    delta = Scaled::ZERO;
                                }
                                p
                            } else {
                                None
                            }
                        }
                        MathCell::Empty => None,
                        MathCell::SubBox => q.first().val.ptr.opt(),
                        MathCell::SubMList => {
                            cur_mlist = q.first().val.ptr;
                            let save_style = cur_style;
                            mlist_penalties = false;
                            mlist_to_hlist();
                            cur_style = save_style;
                            cur_size = cur_style.0.size();
                            cur_mu = x_over_n(math_quad(cur_size), 18).0;
                            Some(
                                hpack(llist_link(TEMP_HEAD), Scaled::ZERO, PackMode::Additional)
                                    .ptr(),
                            )
                        }
                    };
                    MEM[q.ptr() + 1].b32.s1 = p.tex_int();
                    if !(q.third().typ == MathCell::Empty && q.second().typ == MathCell::Empty) {
                        make_scripts(&mut q, delta);
                    }
                    /*check_dimensions */
                    let z = hpack(
                        MEM[q.ptr() + 1].b32.s1.opt(),
                        Scaled::ZERO,
                        PackMode::Additional,
                    );
                    max_h = max_h.max(z.height());
                    max_d = max_d.max(z.depth());
                    z.free();
                }
                /*done_with_noad */
                r = Some(q);
                r_type = math_NODE_type(q).unwrap();
                if r_type == MathNode::Right {
                    r_type = MathNode::Left;
                    cur_style = style;
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18).0;
                }
            }
            Node::Text(n) => match n {
                TxtNode::Style(_) => {
                    let m = MEM[q].b16.s0;
                    cur_style = (MathStyle::n((m / 2) as i16).unwrap(), (m % 2) as u8);
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18).0;
                }
                TxtNode::Choice(q_choice) => {
                    let mut p = None;
                    match cur_style.0 {
                        MathStyle::Display => {
                            p = q_choice.display();
                            q_choice.set_display(None);
                        }
                        MathStyle::Text => {
                            p = q_choice.text();
                            q_choice.set_text(None);
                        }
                        MathStyle::Script => {
                            p = q_choice.script();
                            q_choice.set_script(None);
                        }
                        MathStyle::ScriptScript => {
                            p = q_choice.scriptscript();
                            q_choice.set_scriptscript(None);
                        }
                    }
                    flush_node_list(q_choice.display());
                    flush_node_list(q_choice.text());
                    flush_node_list(q_choice.script());
                    flush_node_list(q_choice.scriptscript());
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
                TxtNode::Ins(_)
                | TxtNode::Mark(_)
                | TxtNode::Adjust(_)
                | TxtNode::WhatsIt(_)
                | TxtNode::Penalty(_)
                | TxtNode::Disc(_) => {}
                TxtNode::Rule(r) => {
                    max_h = max_h.max(r.height());
                    max_d = max_d.max(r.depth());
                }
                TxtNode::Glue(q) => {
                    if q.param() == MU_GLUE {
                        let x = q.glue_ptr() as usize;
                        let y = math_glue(&GlueSpec(x), cur_mu);
                        delete_glue_ref(x);
                        q.set_glue_ptr(Some(y).tex_int());
                        q.set_param(NORMAL as u16);
                    } else if cur_size != TEXT_SIZE && q.param() == COND_MATH_GLUE {
                        if let Some(p) = llist_link(q.ptr()) {
                            match TxtNode::from(p) {
                                TxtNode::Glue(_) | TxtNode::Kern(_) => {
                                    *LLIST_link(q.ptr()) = *LLIST_link(p);
                                    *LLIST_link(p) = None.tex_int();
                                    flush_node_list(Some(p));
                                }
                                _ => {}
                            }
                        }
                    }
                }
                TxtNode::Kern(_) => {
                    math_kern(q as usize, cur_mu);
                }
                _ => confusion("mlist1"),
            },
            _ => confusion("mlist1"),
        }

        /*done_with_node */
        qopt = llist_link(q);
    } /*ord_noad *//*:755 */

    if let (Some(r), MathNode::Bin) = (r, r_type) {
        set_math_NODE_type(r, MathNode::Ord);
    }
    let mut p = TEMP_HEAD;
    *LLIST_link(p) = None.tex_int();
    let mut qopt = mlist.opt();
    let mut r_type = None;
    cur_style = style;
    cur_size = cur_style.0.size();
    cur_mu = x_over_n(math_quad(cur_size), 18).0;
    while let Some(q) = qopt {
        let mut t = MathNode::Ord;
        let mut s = NOAD_SIZE as i16;
        let mut pen = INF_PENALTY;
        match Node::from(q) {
            Node::Math(n) => match n {
                MathNode::Op
                | MathNode::Open
                | MathNode::Close
                | MathNode::Punct
                | MathNode::Inner => {
                    t = math_NODE_type(q).unwrap();
                }
                MathNode::Bin => {
                    t = MathNode::Bin;
                    pen = *INTPAR(IntPar::bin_op_penalty);
                }
                MathNode::Rel => {
                    t = MathNode::Rel;
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
                    t = MathNode::Inner;
                    s = FRACTION_NOAD_SIZE as i16;
                }
                MathNode::Left | MathNode::Right => {
                    t = make_left_right(&mut LeftRight(q), style, max_d, max_h);
                }
            },
            Node::Text(n) => match n {
                TxtNode::Style(_) => {
                    let m = MEM[q].b16.s0;
                    cur_style = (MathStyle::n((m / 2) as i16).unwrap(), (m % 2) as u8);
                    s = STYLE_NODE_SIZE as i16;
                    cur_size = cur_style.0.size();
                    cur_mu = x_over_n(math_quad(cur_size), 18).0;
                    /*delete_q */
                    qopt = llist_link(q);
                    free_node(q, s as i32);
                    continue;
                }
                TxtNode::WhatsIt(_)
                | TxtNode::Penalty(_)
                | TxtNode::Rule(_)
                | TxtNode::Disc(_)
                | TxtNode::Adjust(_)
                | TxtNode::Ins(_)
                | TxtNode::Mark(_)
                | TxtNode::Glue(_)
                | TxtNode::Kern(_) => {
                    *LLIST_link(p) = Some(q).tex_int();
                    p = q;
                    qopt = llist_link(q);
                    *LLIST_link(p) = None.tex_int();
                    continue;
                }
                _ => confusion("mlist3"),
            },
            _ => confusion("mlist3"),
        }
        if let Some(r_type) = r_type {
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
                    confusion("mlist4");
                }
            };
            if x != 0 {
                let y = math_glue(&GlueSpec(EQTB[GLUE_BASE + x as usize].val as usize), cur_mu);
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
                if let Some(next) = llist_link(lst) {
                    lst = next;
                } else {
                    break;
                }
            }
        }
        if penalties {
            if let Some(m) = llist_link(q) {
                if pen < INF_PENALTY {
                    match Node::from(m) {
                        Node::Text(TxtNode::Penalty(_)) | Node::Math(MathNode::Rel) => {}
                        _ => {
                            let z = new_penalty(pen);
                            *LLIST_link(p) = Some(z).tex_int();
                            p = z
                        }
                    }
                }
            }
        }
        if MEM[q].b16.s1 == MathNode::Right as u16 {
            t = MathNode::Open;
        }
        r_type = Some(t);
        /*delete_q */
        qopt = llist_link(q);
        free_node(q, s as i32);
    }
}
unsafe fn var_delimiter(d: &Delimeter, mut s: usize, mut v: Scaled) -> usize {
    let mut c: u16 = 0;
    let mut y: u16 = 0;
    let mut n: i32 = 0;
    let mut u: Scaled = Scaled::ZERO;
    let mut q: b16x4 = b16x4_le_t {
        s0: 0,
        s1: 0,
        s2: 0,
        s3: 0,
    };

    let mut f = FONT_BASE;
    let mut w = Scaled::ZERO;
    let mut large_attempt = false;
    let mut z = d.s3 as i32 % 256;
    let mut x = (d.s2 as i64 + (d.s3 as i32 / 256) as i64 * 65536) as u16;
    let mut ot_assembly_ptr = ptr::null_mut();
    's_62: loop {
        if z != 0 || x != 0 {
            z = z + s as i32 + 256;
            loop {
                z = z - 256;
                let g = MATH_FONT(z as usize);
                if g != FONT_BASE {
                    /*734: */
                    match &FONT_LAYOUT_ENGINE[f] {
                        Font::Native(Otgr(e)) if e.using_open_type() => {
                            x = if x >= 0xd800 && x <= 0xdfff {
                                0
                            } else {
                                e.map_char_to_glyph(x as u32) as u16
                            };
                            f = g;
                            c = x;
                            w = Scaled::ZERO;
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
                                if u < Scaled::ZERO {
                                    break;
                                }
                            }
                            ot_assembly_ptr = get_ot_assembly_ptr(g, x as i32, 0);
                            if !ot_assembly_ptr.is_null() {
                                break 's_62;
                            }
                        }
                        _ => {
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
                                        u = Scaled(
                                            FONT_INFO[(HEIGHT_BASE[g as usize] + q.s2 as i32 / 16)
                                                as usize]
                                                .b32
                                                .s1,
                                        ) + Scaled(
                                            FONT_INFO[(DEPTH_BASE[g as usize] + q.s2 as i32 % 16)
                                                as usize]
                                                .b32
                                                .s1,
                                        );
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
        z = d.s1 as i32 % 256;
        x = (d.s0 as i64 + (d.s1 as i32 / 256) as i64 * 65536) as u16
    }
    let mut b = if f != FONT_BASE {
        match &FONT_LAYOUT_ENGINE[f] {
            Font::Native(Otgr(e)) if e.using_open_type() => {
                if !ot_assembly_ptr.is_null() {
                    build_opentype_assembly(f, ot_assembly_ptr, v, ListDir::Vertical)
                } else {
                    let mut b = List::from(new_null_box());
                    b.set_vertical();
                    let mut g = Glyph::new_node();
                    b.set_list_ptr(g.ptr() as i32);
                    g.set_font(f as u16).set_glyph(c);
                    g.set_metrics(true);
                    b.set_width(g.width())
                        .set_height(g.height())
                        .set_depth(g.depth());
                    b
                }
            }
            _ => {
                /*736: */
                if q.s1 as i32 % 4 == EXT_TAG {
                    /*739: */
                    let mut b = List::from(new_null_box());
                    b.set_vertical();
                    let r = FONT_INFO[(EXTEN_BASE[f] + q.s0 as i32) as usize].b16;
                    c = r.s0;
                    u = height_plus_depth(f, c);
                    w = Scaled::ZERO;
                    q = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize].b16;
                    b.set_width(
                        Scaled(FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1)
                            + Scaled(
                                FONT_INFO[(ITALIC_BASE[f] + q.s1 as i32 / 4) as usize]
                                    .b32
                                    .s1,
                            ),
                    );
                    c = r.s1;
                    if c != 0 {
                        w += height_plus_depth(f, c)
                    }
                    c = r.s2;
                    if c != 0 {
                        w += height_plus_depth(f, c)
                    }
                    c = r.s3;
                    if c != 0 {
                        w += height_plus_depth(f, c)
                    }
                    n = 0;
                    if u > Scaled::ZERO {
                        while w < v {
                            w += u;
                            n += 1;
                            if r.s2 != 0 {
                                w += u;
                            }
                        }
                    }
                    c = r.s1;
                    if c != 0 {
                        stack_into_box(&mut b, f, c);
                    }
                    c = r.s0;
                    for _ in 0..n {
                        stack_into_box(&mut b, f, c);
                    }
                    c = r.s2;
                    if c != 0 {
                        stack_into_box(&mut b, f, c);
                        c = r.s0;
                        for _ in 0..n {
                            stack_into_box(&mut b, f, c);
                        }
                    }
                    c = r.s3;
                    if c != 0 {
                        stack_into_box(&mut b, f, c);
                    }
                    let h = b.height();
                    b.set_depth(w - h);
                    b
                } else {
                    char_box(f, c as i32)
                }
                /*:736 */
            }
        }
    } else {
        let mut b = List::from(new_null_box());
        b.set_width(*DIMENPAR(DimenPar::null_delimiter_space));
        b
    };
    b.set_shift_amount((b.height() - b.depth()).half() - axis_height(s));
    free_ot_assembly(ot_assembly_ptr as *mut GlyphAssembly);
    b.ptr()
}
unsafe fn char_box(mut f: usize, mut c: i32) -> List {
    let mut b;
    let p = if let Font::Native(_) = &FONT_LAYOUT_ENGINE[f] {
        b = List::from(new_null_box());
        let p = new_native_character(f, c);
        b.set_list_ptr(Some(p.ptr()).tex_int());
        b.set_height(p.height())
            .set_width(p.width())
            .set_depth((p.depth()).max(Scaled::ZERO));
        p.ptr()
    } else {
        let q = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c as u16)) as usize].b16;
        b = List::from(new_null_box());
        b.set_width(
            Scaled(FONT_INFO[(WIDTH_BASE[f] + q.s3 as i32) as usize].b32.s1)
                + Scaled(
                    FONT_INFO[(ITALIC_BASE[f] + q.s1 as i32 / 4) as usize]
                        .b32
                        .s1,
                ),
        )
        .set_height(Scaled(
            FONT_INFO[(HEIGHT_BASE[f] + q.s2 as i32 / 16) as usize]
                .b32
                .s1,
        ))
        .set_depth(Scaled(
            FONT_INFO[(DEPTH_BASE[f] + q.s2 as i32 % 16) as usize]
                .b32
                .s1,
        ));
        let p = get_avail();
        MEM[p].b16.s0 = c as u16;
        MEM[p].b16.s1 = f as u16;
        p
    };
    b.set_list_ptr(Some(p).tex_int());
    b
}
unsafe fn stack_into_box(b: &mut List, f: usize, c: u16) {
    let p = char_box(f, c as i32);
    *LLIST_link(p.ptr()) = b.list_ptr();
    b.set_list_ptr(Some(p.ptr()).tex_int());
    b.set_height(p.height());
}
unsafe fn height_plus_depth(mut f: internal_font_number, mut c: u16) -> Scaled {
    let mut q: b16x4 = FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, c)) as usize].b16;
    Scaled(
        FONT_INFO[(HEIGHT_BASE[f] + q.s2 as i32 / 16) as usize]
            .b32
            .s1
            + FONT_INFO[(DEPTH_BASE[f] + q.s2 as i32 % 16) as usize]
                .b32
                .s1,
    )
}
unsafe fn stack_glyph_into_box(b: &mut List, mut f: internal_font_number, mut g: i32) {
    let mut p = Glyph::new_node();
    p.set_font(f as u16).set_glyph(g as u16);
    p.set_metrics(true);
    if b.is_horizontal() {
        if let Some(mut q) = b.list_ptr().opt() {
            while let Some(next) = llist_link(q) {
                q = next;
            }
            *LLIST_link(q) = Some(p.ptr()).tex_int();
            let h = b.height();
            b.set_height(h.max(p.height()));
            let d = b.depth();
            b.set_depth(d.max(p.depth()));
        } else {
            b.set_list_ptr(Some(p.ptr()).tex_int());
        }
    } else {
        *LLIST_link(p.ptr()) = b.list_ptr();
        b.set_list_ptr(Some(p.ptr()).tex_int());
        b.set_height(p.height());
        let w = b.width();
        b.set_width(w.max(p.width()));
    };
}
unsafe fn stack_glue_into_box(b: &mut List, mut min: Scaled, mut max: Scaled) {
    let q = new_spec(0);
    GlueSpec(q).set_size(min).set_stretch(max - min);
    let p = new_glue(q);
    if b.is_horizontal() {
        if let Some(mut q) = b.list_ptr().opt() {
            while let Some(next) = llist_link(q) {
                q = next;
            }
            *LLIST_link(q) = Some(p).tex_int();
        } else {
            b.set_list_ptr(Some(p).tex_int());
        }
    } else {
        *LLIST_link(p) = b.list_ptr();
        b.set_list_ptr(Some(p).tex_int());
        b.set_height(Scaled(MEM[p + 3].b32.s1)); // TODO: strange, maybe BUG
        b.set_width(Scaled(MEM[p + 1].b32.s1));
    };
}
unsafe fn build_opentype_assembly(
    mut f: internal_font_number,
    mut a: *mut libc::c_void,
    mut s: Scaled,
    dir: ListDir,
) -> List {
    let mut b = List::from(new_null_box());
    b.set_list_dir(dir);
    let mut n = -1;
    let mut no_extenders = true;
    let mut min_o = ot_min_connector_overlap(f);
    loop {
        n = n + 1;
        let mut s_max = Scaled::ZERO;
        let mut prev_o = Scaled::ZERO;
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
    let mut prev_o = Scaled::ZERO;
    for i in 0..ot_part_count(a as *const GlyphAssembly) {
        if ot_part_is_extender(a as *const GlyphAssembly, i) {
            for _ in 0..n {
                let o = ot_part_start_connector(f, a as *const GlyphAssembly, i).min(prev_o);

                let oo = o;

                let o = o.min(min_o);

                if oo > Scaled::ZERO {
                    stack_glue_into_box(&mut b, -oo, -o);
                }
                let g = ot_part_glyph(a as *const GlyphAssembly, i);
                stack_glyph_into_box(&mut b, f, g);
                prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i);
            }
        } else {
            let o = ot_part_start_connector(f, a as *const GlyphAssembly, i).min(prev_o);

            let oo = o;

            let o = o.min(min_o);

            if oo > Scaled::ZERO {
                stack_glue_into_box(&mut b, -oo, -o);
            }
            let g = ot_part_glyph(a as *const GlyphAssembly, i);
            stack_glyph_into_box(&mut b, f, g);
            prev_o = ot_part_end_connector(f, a as *const GlyphAssembly, i)
        }
    }
    let mut popt = b.list_ptr().opt();
    let mut nat = Scaled::ZERO;
    let mut str = Scaled::ZERO;
    while let Some(p) = popt {
        match TxtNode::from(p) {
            TxtNode::WhatsIt(w) => match w {
                // TODO: check unreachable
                WhatsIt::NativeWord(nw) => {
                    nat += if dir == ListDir::Horizontal {
                        nw.width()
                    } else {
                        nw.height() + nw.depth()
                    };
                }
                WhatsIt::Glyph(g) => {
                    nat += if dir == ListDir::Horizontal {
                        g.width()
                    } else {
                        g.height() + g.depth()
                    };
                }
                WhatsIt::Pic(g) | WhatsIt::Pdf(g) => {
                    nat += if dir == ListDir::Horizontal {
                        g.width()
                    } else {
                        g.height() + g.depth()
                    };
                }
                _ => {}
            },
            TxtNode::Glue(g) => {
                let spec = GlueSpec(g.glue_ptr() as usize);
                nat += spec.size();
                str += spec.stretch();
            }
            _ => {}
        }
        popt = llist_link(p);
    }

    if s > nat && str > Scaled::ZERO {
        let o = (s - nat).min(str);

        b.set_glue_order(GlueOrder::Normal)
            .set_glue_sign(GlueSign::Stretching)
            .set_glue_set(o.0 as f64 / str.0 as f64);
        let glue = b.glue_set();
        if dir == ListDir::Horizontal {
            b.set_width(nat + tex_round(str.0 as f64 * glue));
        } else {
            b.set_height(nat + tex_round(str.0 as f64 * glue));
        }
    } else if dir == ListDir::Horizontal {
        b.set_width(nat);
    } else {
        b.set_height(nat);
    }
    b
}
unsafe fn rebox(mut b: List, mut w: Scaled) -> List {
    if b.width() != w && b.list_ptr().opt().is_some() {
        if b.is_vertical() {
            b = hpack(Some(b.ptr()), Scaled::ZERO, PackMode::Additional);
        }
        let mut p = b.list_ptr() as usize;
        if is_char_node(Some(p)) && llist_link(p).is_none() {
            let p = Char(p);
            let f = p.font() as usize;
            let v = Scaled(
                FONT_INFO[(WIDTH_BASE[f]
                    + FONT_INFO[(CHAR_BASE[f] + effective_char(true, f, p.character())) as usize]
                        .b16
                        .s3 as i32) as usize]
                    .b32
                    .s1,
            );
            if v != b.width() {
                *LLIST_link(p.ptr()) = new_kern(b.width() - v) as i32;
            }
        }
        free_node(b.ptr(), BOX_NODE_SIZE);
        let g = new_glue(12);
        *LLIST_link(g) = Some(p).tex_int();
        while let Some(next) = llist_link(p) {
            p = next;
        }
        *LLIST_link(p) = new_glue(12) as i32;
        hpack(Some(g), w, PackMode::Exactly)
    } else {
        b.set_width(w);
        b
    }
}
