use crate::xetex_ini::MEM;
use crate::{xetex_ini, xetex_output};

/* Symbolic accessors for various TeX data structures. I would loooove to turn these
 * into actual structs, but the path to doing that is not currently clear. Making
 * field references symbolic seems like a decent start. Sadly I don't see how to do
 * this conversion besides painstakingly annotating things.
 */

pub(crate) unsafe fn LLIST_link(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize].b32.s1
}
pub(crate) unsafe fn LLIST_info(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize].b32.s0
}

/// half of LLIST_info(p)
pub(crate) unsafe fn NODE_type(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s1
}
/// the other half of LLIST_info(p)
pub(crate) unsafe fn NODE_subtype(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s0
}

/// subtype; records L/R direction mode
pub(crate) unsafe fn BOX_lr_mode(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s0
}
/// a scaled; 1 <=> WEB const `width_offset`
pub(crate) unsafe fn BOX_width(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize + 1].b32.s1
}
/// a scaled; 2 <=> WEB const `depth_offset`
pub(crate) unsafe fn BOX_depth(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize + 2].b32.s1
}
/// a scaled; 3 <=> WEB const `height_offset`
pub(crate) unsafe fn BOX_height(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize + 3].b32.s1
}
/// a scaled
pub(crate) unsafe fn BOX_shift_amount(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize + 4].b32.s1
}
/// aka `link` of p+5
pub(crate) unsafe fn BOX_list_ptr(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize + 5].b32.s1
}
/// aka `type` of p+5
pub(crate) unsafe fn BOX_glue_sign(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize + 5].b16.s1
}
/// aka `subtype` of p+5
pub(crate) unsafe fn BOX_glue_order(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize + 5].b16.s0
}
/// the glue ratio
pub(crate) unsafe fn BOX_glue_set(p: i32) -> &'static mut f64 {
    &mut MEM[p as usize + 6].gr
}

/// aka "subtype" of a node
pub(crate) unsafe fn ACTIVE_NODE_fitness(p: i32) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s0
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn ACTIVE_NODE_break_node(p: i32) -> &'static mut i32 {
    &mut MEM[p as usize + 1].b32.s1
}
/// aka "llink" in doubly-linked list
pub(crate) unsafe fn ACTIVE_NODE_line_number(p: i32) -> &'static mut i32 {
    &mut MEM[p as usize + 1].b32.s0
}
/// was originally the `mem[x+2].int` field
pub(crate) unsafe fn ACTIVE_NODE_total_demerits(p: i32) -> &'static mut i32 {
    &mut MEM[p as usize + 2].b32.s1
}
/// a scaled; "active_short" in the WEB
pub(crate) unsafe fn ACTIVE_NODE_shortfall(p: i32) -> &'static mut i32 {
    &mut MEM[p as usize + 3].b32.s1
}
/// a scaled
pub(crate) unsafe fn ACTIVE_NODE_glue(p: i32) -> &'static mut i32 {
    &mut MEM[p as usize + 4].b32.s1
}

/// aka "type" of a node
pub(crate) unsafe fn CHAR_NODE_font(p: i32) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s1
}
/// aka "subtype" of a node
pub(crate) unsafe fn CHAR_NODE_character(p: i32) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s0
}
/*

#define DELTA_NODE_dwidth(p) mem[(p) + 1].b32.s1 /* the "natural width" difference */
#define DELTA_NODE_dstretch0(p) mem[(p) + 2].b32.s1 /* the stretch difference in points */
#define DELTA_NODE_dstretch1(p) mem[(p) + 3].b32.s1 /* the stretch difference in fil */
#define DELTA_NODE_dstretch2(p) mem[(p) + 4].b32.s1 /* the stretch difference in fill */
#define DELTA_NODE_dstretch3(p) mem[(p) + 5].b32.s1 /* the stretch difference in fill */
#define DELTA_NODE_dshrink(p) mem[(p) + 6].b32.s1 /* the shrink difference */

#define DISCRETIONARY_NODE_replace_count(p) mem[p].b16.s0 /* aka "subtype" of a node */
#define DISCRETIONARY_NODE_pre_break(p) mem[(p) + 1].b32.s0 /* aka "llink" in doubly-linked list */
#define DISCRETIONARY_NODE_post_break(p) mem[(p) + 1].b32.s1 /* aka "rlink" in double-linked list */

*/

/// "new left_edge position relative to cur_h"
pub(crate) unsafe fn EDGE_NODE_edge_dist(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 2) as usize].b32.s1
}

/// aka "llink" in doubly-linked list
pub(crate) unsafe fn GLUE_NODE_glue_ptr(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 1) as usize].b32.s0
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn GLUE_NODE_leader_ptr(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 1) as usize].b32.s1
}

/*
#define INSERTION_NODE_float_cost(p) mem[(p) + 1].b32.s1 /* "the floating_penalty to be used" */
#define INSERTION_NODE_split_top_ptr(p) mem[(p) + 4].b32.s1 /* a glue pointer */
#define INSERTION_NODE_ins_ptr(p) mem[(p) + 4].b32.s0 /* a pointer to a vlist */

#define LANGUAGE_NODE_what_lang(p) mem[(p) + 1].b32.s1 /* language number, 0..255 */
#define LANGUAGE_NODE_what_lhm(p) mem[(p) + 1].b16.s1 /* "minimum left fragment, range 1..63" */
#define LANGUAGE_NODE_what_rhm(p) mem[(p) + 1].b16.s0 /* "minimum right fragment, range 1..63" */

#define LIGATURE_NODE_lig_font(p) mem[(p) + 1].b16.s1 /* WEB: font(lig_char(p)) */
#define LIGATURE_NODE_lig_char(p) mem[(p) + 1].b16.s0 /* WEB: character(lig_char(p)) */
#define LIGATURE_NODE_lig_ptr(p) mem[(p) + 1].b32.s1 /* WEB: link(lig_char(p)) */

#define MARK_NODE_ptr(p) mem[(p) + 1].b32.s1 /* "head of the token list for the mark" */
#define MARK_NODE_class(p) mem[(p) + 1].b32.s0 /* "the mark class" */

/* To check: do these really only apply to MATH_NODEs? */
#define MATH_NODE_lr_dir(p) (NODE_subtype(p) / R_CODE)
#define MATH_NODE_end_lr_type(p) (L_CODE * (NODE_subtype(p) / L_CODE) + END_M_CODE)

#define NATIVE_NODE_size(p) mem[(p) + 4].b16.s3
#define NATIVE_NODE_font(p) mem[(p) + 4].b16.s2
#define NATIVE_NODE_length(p) mem[(p) + 4].b16.s1 /* number of UTF16 items in the text */
#define NATIVE_NODE_glyph(p) mem[(p) + 4].b16.s1 /* ... or the glyph number, if subtype==GLYPH_NODE */
#define NATIVE_NODE_glyph_count(p) mem[(p) + 4].b16.s0
#define NATIVE_NODE_glyph_info_ptr(p) mem[(p) + 5].ptr
#define NATIVE_NODE_text(p) ((unsigned short *) &mem[(p) + NATIVE_NODE_SIZE])

#define PAGE_INS_NODE_broken_ptr(p) mem[(p) + 1].b32.s1 /* "an insertion for this class will break here if anywhere" */
#define PAGE_INS_NODE_broken_ins(p) mem[(p) + 1].b32.s0 /* "this insertion might break at broken_ptr" */
#define PAGE_INS_NODE_last_ins_ptr(p) mem[(p) + 2].b32.s1 /* "the most recent insertion for this subtype" */
#define PAGE_INS_NODE_best_ins_ptr(p) mem[(p) + 2].b32.s0 /* "the optimum most recent insertion" */

#define PASSIVE_NODE_prev_break(p) mem[(p) + 1].b32.s0 /* aka "llink" in doubly-linked list */
#define PASSIVE_NODE_next_break(p) PASSIVE_NODE_prev_break(p) /* siggggghhhhh */
#define PASSIVE_NODE_cur_break(p) mem[(p) + 1].b32.s1 /* aka "rlink" in double-linked list */
#define PASSIVE_NODE_serial(p) mem[p].b32.s0 /* aka "info" */
*/

/// was originally the `mem[x+1].int` field
pub(crate) unsafe fn PENALTY_NODE_penalty(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 1) as usize].b32.s1
}

/*
#define PIC_NODE_path_len(p) mem[(p) + 4].b16.s1 /* number of bytes in the path item */
#define PIC_NODE_path(p) ((unsigned char *) &mem[(p) + PIC_NODE_SIZE])
#define PIC_NODE_total_size(p) (PIC_NODE_SIZE + (PIC_NODE_path_len(p) + sizeof(memory_word) - 1) / sizeof(memory_word))

#define WRITE_NODE_tokens(p) mem[(p) + 1].b32.s1 /* "reference count of token list to write" */

/* Synctex hacks various nodes to add an extra word at the end to store its
 * information, hence the need to know the node size to get the synctex
 * info. */
#define SYNCTEX_tag(p, nodesize) mem[(p) + nodesize - SYNCTEX_FIELD_SIZE].b32.s0
#define SYNCTEX_line(p, nodesize) mem[(p) + nodesize - SYNCTEX_FIELD_SIZE].b32.s1
*/

/// aka "link" of a link-list node
pub(crate) unsafe fn GLUE_SPEC_ref_count(p: isize) -> &'static mut i32 {
    &mut MEM[p as usize].b32.s1
}
/// aka "type" of a node
pub(crate) unsafe fn GLUE_SPEC_stretch_order(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s1
}
/// aka "subtype" of a node
pub(crate) unsafe fn GLUE_SPEC_shrink_order(p: isize) -> &'static mut u16 {
    &mut MEM[p as usize].b16.s0
}
/// a scaled
pub(crate) unsafe fn GLUE_SPEC_stretch(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 2) as usize].b32.s1
}
/// a scaled
pub(crate) unsafe fn GLUE_SPEC_shrink(p: isize) -> &'static mut i32 {
    &mut MEM[(p + 3) as usize].b32.s1
}

/*
#define FONT_CHARACTER_INFO(f, c) font_info[char_base[f] + (c)].b16
#define FONT_CHARINFO_WIDTH(f, info) font_info[width_base[f] + (info).s3].b32.s1
#define FONT_CHARINFO_HEIGHT(f, info) font_info[height_base[f] + (info).s2 / 16].b32.s1
#define FONT_CHARINFO_DEPTH(f, info) font_info[depth_base[f] + (info).s2 % 16].b32.s1
#define FONT_CHARINFO_ITALCORR(f, info) font_info[italic_base[f] + (info).s1 / 4].b32.s1
#define FONT_CHARACTER_WIDTH(f, c) FONT_CHARINFO_WIDTH(f, FONT_CHARACTER_INFO(f, c))

#define TOKEN_LIST_ref_count(p) mem[p].b32.s0

/* e-TeX extended marks stuff ... not sure where to put these */
#define ETEX_MARK_sa_top_mark(p) mem[(p) + 1].b32.s0 /* \topmarks<n> */
#define ETEX_MARK_sa_first_mark(p) mem[(p) + 1].b32.s1 /* \firstmarks<n> */
#define ETEX_MARK_sa_bot_mark(p) mem[(p) + 2].b32.s0 /* \botmarks<n> */
#define ETEX_MARK_sa_split_first_mark(p) mem[(p) + 2].b32.s1 /* \splitfirstmarks<n> */
#define ETEX_MARK_sa_split_bot_mark(p) mem[(p) + 3].b32.s0 /* \splitbotmarks<n> */
*/

#[inline]
pub(crate) unsafe fn is_non_discardable_node(p: i32) -> bool {
    (MEM[p as usize].b16.s1 as i32) < 9
}

#[inline]
pub(crate) unsafe fn is_char_node(p: i32) -> bool {
    p >= xetex_ini::hi_mem_min
}

#[inline]
pub(crate) unsafe fn print_c_string(mut str: *const i8) {
    while *str != 0 {
        let fresh0 = str;
        str = str.offset(1);
        xetex_output::print_char(*fresh0 as i32);
    }
}
