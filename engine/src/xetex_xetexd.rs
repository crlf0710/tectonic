use crate::xetex_consts::{
    KernNST, LRMode, TextNode, WhatsItNST, NATIVE_NODE_SIZE, ND, PIC_NODE_SIZE, SYNCTEX_FIELD_SIZE,
};
use crate::xetex_ini::MEM;
use crate::{xetex_ini, xetex_output};

/* Symbolic accessors for various TeX data structures. I would loooove to turn these
 * into actual structs, but the path to doing that is not currently clear. Making
 * field references symbolic seems like a decent start. Sadly I don't see how to do
 * this conversion besides painstakingly annotating things.
 */

pub(crate) trait TeXInt {
    fn tex_int(self) -> i32;
}
impl TeXInt for Option<usize> {
    fn tex_int(self) -> i32 {
        match self {
            Some(u) => u as i32,
            None => crate::xetex_consts::TEX_NULL,
        }
    }
}

pub(crate) trait TeXOpt {
    fn opt(self) -> Option<usize>;
}

impl TeXOpt for i32 {
    fn opt(self) -> Option<usize> {
        match self {
            crate::xetex_consts::TEX_NULL => None,
            _ => Some(self as usize),
        }
    }
}

pub(crate) unsafe fn LLIST_link<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s1
}
pub(crate) unsafe fn LLIST_info<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}

/// half of LLIST_info(p)
pub(crate) unsafe fn NODE_type(p: usize) -> ND {
    ND::from(MEM[p].b16.s1)
}
pub(crate) unsafe fn set_NODE_type(p: usize, n: TextNode) {
    MEM[p].b16.s1 = n as u16;
}
pub(crate) unsafe fn text_NODE_type(p: usize) -> Option<TextNode> {
    TextNode::n(MEM[p].b16.s1)
}

/// the other half of LLIST_info(p)
pub(crate) unsafe fn whatsit_NODE_subtype(p: usize) -> WhatsItNST {
    WhatsItNST::from(MEM[p].b16.s0)
}
pub(crate) unsafe fn set_whatsit_NODE_subtype(p: usize, n: WhatsItNST) {
    MEM[p].b16.s0 = n as u16;
}
pub(crate) unsafe fn kern_NODE_subtype(p: usize) -> KernNST {
    KernNST::from(MEM[p].b16.s0)
}
pub(crate) unsafe fn kern_NODE_width<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
pub(crate) unsafe fn set_kern_NODE_subtype(p: usize, n: KernNST) {
    MEM[p].b16.s0 = n as u16;
}
pub(crate) unsafe fn clear_NODE_subtype(p: usize) {
    MEM[p].b16.s0 = 0;
}

/// subtype; records L/R direction mode
pub(crate) unsafe fn BOX_lr_mode(p: usize) -> LRMode {
    LRMode::from(MEM[p].b16.s0)
}
pub(crate) unsafe fn set_BOX_lr_mode(p: usize, mode: LRMode) {
    MEM[p].b16.s0 = mode as u16
}
/// a scaled; 1 <=> WEB const `width_offset`
pub(crate) unsafe fn BOX_width<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// a scaled; 2 <=> WEB const `depth_offset`
pub(crate) unsafe fn BOX_depth<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}
/// a scaled; 3 <=> WEB const `height_offset`
pub(crate) unsafe fn BOX_height<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 3].b32.s1
}
/// a scaled
pub(crate) unsafe fn BOX_shift_amount<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s1
}
/// aka `link` of p+5
pub(crate) unsafe fn BOX_list_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 5].b32.s1
}
/// aka `type` of p+5
pub(crate) unsafe fn BOX_glue_sign<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 5].b16.s1
}
/// aka `subtype` of p+5
pub(crate) unsafe fn BOX_glue_order<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 5].b16.s0
}

/// the glue ratio
pub(crate) unsafe fn BOX_glue_set<'a>(p: usize) -> &'a mut f64 {
    &mut MEM[p + 6].gr
}

pub(crate) unsafe fn UNSET_NODE_columns<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
pub(crate) unsafe fn UNSET_NODE_shrink<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s1
}
pub(crate) unsafe fn UNSET_NODE_stretch<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 6].b32.s1
}
pub(crate) unsafe fn UNSET_NODE_shrink_order<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 5].b16.s1
}
pub(crate) unsafe fn UNSET_NODE_stretch_order<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 5].b16.s0
}

/// aka "subtype" of a node
pub(crate) unsafe fn ACTIVE_NODE_fitness<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn ACTIVE_NODE_break_node<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// aka "llink" in doubly-linked list
pub(crate) unsafe fn ACTIVE_NODE_line_number<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
/// was originally the `mem[x+2].int` field
pub(crate) unsafe fn ACTIVE_NODE_total_demerits<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}
/// a scaled; "active_short" in the WEB
pub(crate) unsafe fn ACTIVE_NODE_shortfall<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 3].b32.s1
}
/// a scaled
pub(crate) unsafe fn ACTIVE_NODE_glue<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s1
}

/// aka "type" of a node
pub(crate) unsafe fn CHAR_NODE_font<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s1
}
/// aka "subtype" of a node
pub(crate) unsafe fn CHAR_NODE_character<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}

/// the "natural width" difference
pub(crate) unsafe fn DELTA_NODE_dwidth<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// the stretch difference in points
pub(crate) unsafe fn DELTA_NODE_dstretch0<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}
/// the stretch difference in fil
pub(crate) unsafe fn DELTA_NODE_dstretch1<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 3].b32.s1
}
/// the stretch difference in fill
pub(crate) unsafe fn DELTA_NODE_dstretch2<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s1
}
/// the stretch difference in filll
pub(crate) unsafe fn DELTA_NODE_dstretch3<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 5].b32.s1
}
/// the shrink difference
pub(crate) unsafe fn DELTA_NODE_dshrink<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 6].b32.s1
}

/// aka "subtype" of a node
pub(crate) unsafe fn DISCRETIONARY_NODE_replace_count<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
/// aka "llink" in doubly-linked list
pub(crate) unsafe fn DISCRETIONARY_NODE_pre_break<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn DISCRETIONARY_NODE_post_break<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

/// "new left_edge position relative to cur_h"
pub(crate) unsafe fn EDGE_NODE_edge_dist<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}

/// aka "llink" in doubly-linked list
pub(crate) unsafe fn GLUE_NODE_glue_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn GLUE_NODE_leader_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

pub(crate) unsafe fn INSERTION_NODE_box_reg<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
/// "the floating_penalty to be used"
pub(crate) unsafe fn INSERTION_NODE_float_cost<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
pub(crate) unsafe fn INSERTION_NODE_depth<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}
pub(crate) unsafe fn INSERTION_NODE_height<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 3].b32.s1
}
/// a glue pointer
pub(crate) unsafe fn INSERTION_NODE_split_top_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s1
}
/// a pointer to a vlist
pub(crate) unsafe fn INSERTION_NODE_ins_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 4].b32.s0
}

/// language number, 0..255
pub(crate) unsafe fn LANGUAGE_NODE_what_lang<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// "minimum left fragment, range 1..63"
pub(crate) unsafe fn LANGUAGE_NODE_what_lhm<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 1].b16.s1
}
/// "minimum right fragment, range 1..63"
pub(crate) unsafe fn LANGUAGE_NODE_what_rhm<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 1].b16.s0
}

/// WEB: font(lig_char(p))
pub(crate) unsafe fn LIGATURE_NODE_lig_font<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 1].b16.s1
}
///  WEB: character(lig_char(p))
pub(crate) unsafe fn LIGATURE_NODE_lig_char<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 1].b16.s0
}
/// WEB: link(lig_char(p))
pub(crate) unsafe fn LIGATURE_NODE_lig_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

/// "head of the token list for the mark"
pub(crate) unsafe fn MARK_NODE_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// "the mark class"
pub(crate) unsafe fn MARK_NODE_class<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}

pub(crate) unsafe fn ADJUST_NODE_type<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
pub(crate) unsafe fn ADJUST_NODE_ptr<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

/*
/* To check: do these really only apply to MATH_NODEs? */
#define MATH_NODE_lr_dir(p) (NODE_subtype(p) / R_CODE)
#define MATH_NODE_end_lr_type(p) (L_CODE * (NODE_subtype(p) / L_CODE) + END_M_CODE)
*/
pub(crate) unsafe fn NATIVE_NODE_size<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s3
}
pub(crate) unsafe fn NATIVE_NODE_font<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s2
}
/// number of UTF16 items in the text
pub(crate) unsafe fn NATIVE_NODE_length<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s1
}
/// ... or the glyph number, if subtype==WhatsItNST::Glyph
pub(crate) unsafe fn NATIVE_NODE_glyph<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s1
}

pub(crate) unsafe fn NATIVE_NODE_glyph_count<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s0
}
pub(crate) unsafe fn NATIVE_NODE_glyph_info_ptr<'a>(p: usize) -> &'a mut *mut core::ffi::c_void {
    &mut MEM[p + 5].ptr
}
pub(crate) unsafe fn NATIVE_NODE_text<'a>(p: usize) -> &'a mut [u16] {
    let len = *NATIVE_NODE_length(p) as usize;
    let pp = &mut MEM[p + NATIVE_NODE_SIZE as usize].b16.s0 as *mut u16;
    std::slice::from_raw_parts_mut(pp, len)
}
/*
#define PAGE_INS_NODE_broken_ptr(p) mem[(p) + 1].b32.s1 /* "an insertion for this class will break here if anywhere" */
#define PAGE_INS_NODE_broken_ins(p) mem[(p) + 1].b32.s0 /* "this insertion might break at broken_ptr" */
#define PAGE_INS_NODE_last_ins_ptr(p) mem[(p) + 2].b32.s1 /* "the most recent insertion for this subtype" */
#define PAGE_INS_NODE_best_ins_ptr(p) mem[(p) + 2].b32.s0 /* "the optimum most recent insertion" */
*/
pub(crate) unsafe fn FILE_NODE_id<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
pub(crate) unsafe fn OPEN_NODE_name<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
pub(crate) unsafe fn OPEN_NODE_area<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s0
}
pub(crate) unsafe fn OPEN_NODE_ext<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}

/// aka "llink" in doubly-linked list
pub(crate) unsafe fn PASSIVE_NODE_prev_break<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
/// siggggghhhhh
pub(crate) unsafe fn PASSIVE_NODE_next_break<'a>(p: usize) -> &'a mut i32 {
    PASSIVE_NODE_prev_break(p)
}
/// aka "rlink" in double-linked list
pub(crate) unsafe fn PASSIVE_NODE_cur_break<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/*
#define PASSIVE_NODE_serial(p) mem[p].b32.s0 /* aka "info" */
*/

/// was originally the `mem[x+1].int` field
pub(crate) unsafe fn PENALTY_NODE_penalty<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

pub(crate) unsafe fn PIC_NODE_page<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s0
}
/// number of bytes in the path item
pub(crate) unsafe fn PIC_NODE_path_len<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 4].b16.s1
}
pub(crate) unsafe fn PIC_NODE_transform_matrix(p: usize) -> (i32, i32, i32, i32, i32, i32) {
    (
        MEM[p + 5].b32.s0,
        MEM[p + 5].b32.s1,
        MEM[p + 6].b32.s0,
        MEM[p + 6].b32.s1,
        MEM[p + 7].b32.s0,
        MEM[p + 7].b32.s1,
    )
}
pub(crate) unsafe fn set_PIC_NODE_transform_matrix(p: usize, m: (i32, i32, i32, i32, i32, i32)) {
    MEM[p + 5].b32.s0 = m.0;
    MEM[p + 5].b32.s1 = m.1;
    MEM[p + 6].b32.s0 = m.2;
    MEM[p + 6].b32.s1 = m.3;
    MEM[p + 7].b32.s0 = m.4;
    MEM[p + 7].b32.s1 = m.5;
}
pub(crate) unsafe fn PIC_NODE_pagebox<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p + 8].b16.s1
}

pub(crate) unsafe fn PIC_NODE_path<'a>(p: usize) -> &'a mut [u8] {
    let len = *PIC_NODE_path_len(p) as usize;
    let pp = &mut MEM[p + PIC_NODE_SIZE as usize] as *mut crate::xetex_ini::memory_word as *mut u8;
    std::slice::from_raw_parts_mut(pp, len)
}
/*
#define PIC_NODE_total_size(p) (PIC_NODE_SIZE + (PIC_NODE_path_len(p) + sizeof(memory_word) - 1) / sizeof(memory_word))

*/

pub(crate) unsafe fn MARK_CLASS_indexes<'a>(p: usize) -> &'a mut [i32] {
    let pp = &mut MEM[p + 1].b32.s0;
    std::slice::from_raw_parts_mut(pp, 5) // TODO: check size
}
pub(crate) unsafe fn INDEX_NODE_indexes<'a>(p: usize) -> &'a mut [i32] {
    let pp = &mut MEM[p + 1].b32.s0;
    std::slice::from_raw_parts_mut(pp, 64)
}

/// "reference count of token list to write"
pub(crate) unsafe fn WRITE_NODE_tokens<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

/* Synctex hacks various nodes to add an extra word at the end to store its
 * information, hence the need to know the node size to get the synctex
 * info. */

pub(crate) unsafe fn SYNCTEX_tag<'a>(p: usize, nodesize: i32) -> &'a mut i32 {
    &mut MEM[p + (nodesize as usize) - (SYNCTEX_FIELD_SIZE as usize)]
        .b32
        .s0
}
pub(crate) unsafe fn SYNCTEX_line<'a>(p: usize, nodesize: i32) -> &'a mut i32 {
    &mut MEM[p + (nodesize as usize) - (SYNCTEX_FIELD_SIZE as usize)]
        .b32
        .s1
}

/// aka "link" of a link-list node
pub(crate) unsafe fn GLUE_SPEC_ref_count<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s1
}
/// aka "type" of a node
pub(crate) unsafe fn GLUE_SPEC_stretch_order<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s1
}
/// aka "subtype" of a node
pub(crate) unsafe fn GLUE_SPEC_shrink_order<'a>(p: usize) -> &'a mut u16 {
    &mut MEM[p].b16.s0
}
pub(crate) unsafe fn GLUE_SPEC_size<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
/// a scaled
pub(crate) unsafe fn GLUE_SPEC_stretch<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}
/// a scaled
pub(crate) unsafe fn GLUE_SPEC_shrink<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 3].b32.s1
}

pub(crate) unsafe fn CHOICE_NODE_display<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
pub(crate) unsafe fn CHOICE_NODE_text<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}
pub(crate) unsafe fn CHOICE_NODE_script<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s0
}
pub(crate) unsafe fn CHOICE_NODE_scriptscript<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 2].b32.s1
}

use super::xetex_ini::{
    b16x4, CHAR_BASE, DEPTH_BASE, FONT_INFO, HEIGHT_BASE, ITALIC_BASE, WIDTH_BASE,
};
pub(crate) unsafe fn FONT_CHARACTER_INFO(f: usize, c: usize) -> b16x4 {
    FONT_INFO[CHAR_BASE[f] as usize + c].b16
}

pub(crate) unsafe fn FONT_CHARINFO_WIDTH<'a>(f: usize, info: b16x4) -> &'a mut i32 {
    &mut FONT_INFO[(WIDTH_BASE[f] + (info.s3 as i32)) as usize]
        .b32
        .s1
}
pub(crate) unsafe fn FONT_CHARINFO_HEIGHT<'a>(f: usize, info: b16x4) -> &'a mut i32 {
    &mut FONT_INFO[(HEIGHT_BASE[f] + ((info.s2 / 16) as i32)) as usize]
        .b32
        .s1
}
pub(crate) unsafe fn FONT_CHARINFO_DEPTH<'a>(f: usize, info: b16x4) -> &'a mut i32 {
    &mut FONT_INFO[(DEPTH_BASE[f] + ((info.s2 % 16) as i32)) as usize]
        .b32
        .s1
}
pub(crate) unsafe fn FONT_CHARINFO_ITALCORR<'a>(f: usize, info: b16x4) -> &'a mut i32 {
    &mut FONT_INFO[(ITALIC_BASE[f] + ((info.s1 / 4) as i32)) as usize]
        .b32
        .s1
}
pub(crate) unsafe fn FONT_CHARACTER_WIDTH<'a>(f: usize, c: usize) -> &'a mut i32 {
    FONT_CHARINFO_WIDTH(f, FONT_CHARACTER_INFO(f, c))
}

pub(crate) unsafe fn TOKEN_LIST_ref_count<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}
/*
/* e-TeX extended marks stuff ... not sure where to put these */
#define ETEX_MARK_sa_top_mark(p) mem[(p) + 1].b32.s0 /* \topmarks<n> */
#define ETEX_MARK_sa_first_mark(p) mem[(p) + 1].b32.s1 /* \firstmarks<n> */
#define ETEX_MARK_sa_bot_mark(p) mem[(p) + 2].b32.s0 /* \botmarks<n> */
#define ETEX_MARK_sa_split_first_mark(p) mem[(p) + 2].b32.s1 /* \splitfirstmarks<n> */
#define ETEX_MARK_sa_split_bot_mark(p) mem[(p) + 3].b32.s0 /* \splitbotmarks<n> */
*/

#[inline]
pub(crate) unsafe fn is_non_discardable_node(p: usize) -> bool {
    (MEM[p].b16.s1 as i32) < 9
}

#[inline]
pub(crate) unsafe fn is_char_node(p: Option<usize>) -> bool {
    match p {
        Some(p) => p >= xetex_ini::hi_mem_min as usize,
        None => false,
    }
}

#[inline]
pub(crate) unsafe fn print_c_string(mut str: *const i8) {
    while *str != 0 {
        let fresh0 = str;
        str = str.offset(1);
        xetex_output::print_char(*fresh0 as i32);
    }
}

/*
static inline pool_pointer
cur_length(void) {
    /*41: The length of the current string in the pool */
    return pool_ptr - str_start[str_ptr - TOO_BIG_CHAR];
}


/* Tectonic related functions */
tt_history_t tt_run_engine(char *dump_name, char *input_file_name, time_t build_date);


/* formerly xetex.h: */
/* additional declarations we want to slip in for xetex */

/* p is native_word node; g is XeTeX_use_glyph_metrics flag */
#define set_native_metrics(p,g)               measure_native_node(&(mem[p]), g)
#define set_native_glyph_metrics(p,g)         measure_native_glyph(&(mem[p]), g)
#define set_justified_native_glyphs(p)        store_justified_native_glyphs(&(mem[p]))
#define get_native_italic_correction(p)       real_get_native_italic_correction(&(mem[p]))
#define get_native_glyph_italic_correction(p) real_get_native_glyph_italic_correction(&(mem[p]))
#define get_native_glyph(p,i)                 real_get_native_glyph(&(mem[p]), i)
#define make_xdv_glyph_array_data(p)          makeXDVGlyphArrayData(&(mem[p]))
#define get_native_word_cp(p,s)               real_get_native_word_cp(&(mem[p]), s)
*/

/* easier to do the bit-twiddling here than in Pascal */
/* read fields from a 32-bit math code */
pub(crate) fn math_fam(x: i32) -> u32 {
    x as u32 >> 24 & 0xff
}
pub(crate) fn math_class(x: i32) -> u32 {
    x as u32 >> 21 & 0x7
}
pub(crate) fn math_char(x: i32) -> u32 {
    x as u32 & 0x1fffff
}
/* calculate pieces to assign to a math code */
pub(crate) fn set_family(x: i32) -> i32 {
    ((x as u32 & 0xff) << 24) as i32
}
pub(crate) fn set_class(x: i32) -> i32 {
    ((x as u32 & 0x7) << 21) as i32
}
