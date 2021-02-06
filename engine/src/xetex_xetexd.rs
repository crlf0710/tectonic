use crate::node::{MathNode, TextNode, SYNCTEX_FIELD_SIZE};
use crate::xetex_ini;
use crate::xetex_ini::MEM;

use crate::xetex_scaledmath::Scaled;

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
pub(crate) unsafe fn llist_link(p: usize) -> Option<usize> {
    MEM[p].b32.s1.opt()
}
pub(crate) unsafe fn LLIST_info<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}
/// the size field in empty variable-size nodes
pub(crate) unsafe fn node_size<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}
/// reference count preceding a token list
pub(crate) unsafe fn token_ref_count<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}
/// left link in doubly-linked list of empty nodes
pub(crate) unsafe fn DLIST_llink<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}
/// right link in doubly-linked list of empty nodes
pub(crate) unsafe fn DLIST_rlink<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}

/// half of LLIST_info(p)
pub(crate) unsafe fn set_NODE_type(p: usize, n: TextNode) {
    MEM[p].b16.s1 = n as u16;
}
pub(crate) unsafe fn math_NODE_type(p: usize) -> Option<MathNode> {
    MathNode::n(MEM[p].b16.s1)
}
pub(crate) unsafe fn set_math_NODE_type(p: usize, n: MathNode) {
    MEM[p].b16.s1 = n as u16;
}

pub(crate) unsafe fn clear_NODE_subtype(p: usize) {
    MEM[p].b16.s0 = 0;
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

use super::xetex_ini::{
    b16x4, CHAR_BASE, DEPTH_BASE, FONT_INFO, HEIGHT_BASE, ITALIC_BASE, WIDTH_BASE,
};
pub(crate) unsafe fn FONT_CHARACTER_INFO(f: usize, c: usize) -> b16x4 {
    FONT_INFO[CHAR_BASE[f] as usize + c].b16
}

pub(crate) unsafe fn FONT_CHARINFO_WIDTH<'a>(f: usize, info: b16x4) -> &'a mut Scaled {
    &mut *(&mut FONT_INFO[(WIDTH_BASE[f] + (info.s3 as i32)) as usize]
        .b32
        .s1 as *mut i32 as *mut Scaled)
}
pub(crate) unsafe fn FONT_CHARINFO_HEIGHT<'a>(f: usize, info: b16x4) -> &'a mut Scaled {
    &mut *(&mut FONT_INFO[(HEIGHT_BASE[f] + ((info.s2 / 16) as i32)) as usize]
        .b32
        .s1 as *mut i32 as *mut Scaled)
}
pub(crate) unsafe fn FONT_CHARINFO_DEPTH<'a>(f: usize, info: b16x4) -> &'a mut Scaled {
    &mut *(&mut FONT_INFO[(DEPTH_BASE[f] + ((info.s2 % 16) as i32)) as usize]
        .b32
        .s1 as *mut i32 as *mut Scaled)
}
pub(crate) unsafe fn FONT_CHARINFO_ITALCORR<'a>(f: usize, info: b16x4) -> &'a mut Scaled {
    &mut *(&mut FONT_INFO[(ITALIC_BASE[f] + ((info.s1 / 4) as i32)) as usize]
        .b32
        .s1 as *mut i32 as *mut Scaled)
}
pub(crate) unsafe fn FONT_CHARACTER_WIDTH<'a>(f: usize, c: usize) -> &'a mut Scaled {
    FONT_CHARINFO_WIDTH(f, FONT_CHARACTER_INFO(f, c))
}

pub(crate) unsafe fn TOKEN_LIST_ref_count<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p].b32.s0
}

#[inline]
pub(crate) unsafe fn is_char_node(p: Option<usize>) -> bool {
    match p {
        Some(p) => p >= xetex_ini::hi_mem_min as usize,
        None => false,
    }
}

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

/* e-TeX sparse arrays for large-numebered registers, etc. */
/*pub(crate) unsafe fn ETEX_SA_ref<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s0
}*/
pub(crate) unsafe fn ETEX_SA_ptr<'a>(p: usize) -> Option<usize> {
    MEM[p + 1].b32.s1.opt()
}
/*pub(crate) unsafe fn ETEX_SA_num<'a>(p: usize) -> &'a mut i32 {
    &mut MEM[p + 1].b32.s1
}*/
