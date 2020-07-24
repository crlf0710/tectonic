use crate::xetex_xetex0::free_node;
use std::ops::{Deref, DerefMut};

use crate::xetex_consts::{BreakType, GlueOrder, GlueSign};
use crate::xetex_ini::MEM;
use crate::xetex_xetexd::{TeXInt, TeXOpt};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ND {
    Text(TextNode),
    Math(MathNode),
    Unknown(u16),
}

impl From<u16> for ND {
    fn from(n: u16) -> Self {
        match n {
            0..=15 | 40 => Self::Text(TextNode::from(n)),
            16..=31 => Self::Math(MathNode::from(n)),
            _ => Self::Unknown(n),
        }
    }
}
impl From<TextNode> for ND {
    fn from(n: TextNode) -> Self {
        Self::Text(n)
    }
}
impl From<MathNode> for ND {
    fn from(n: MathNode) -> Self {
        Self::Math(n)
    }
}

impl ND {
    pub fn u16(self) -> u16 {
        match self {
            Self::Text(n) => n as u16,
            Self::Math(n) => n as u16,
            Self::Unknown(n) => n,
        }
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
pub(crate) enum TextNode {
    HList = 0,
    VList = 1,
    Rule = 2,
    Ins = 3,
    Mark = 4,
    Adjust = 5,
    Ligature = 6,
    Disc = 7,
    WhatsIt = 8,
    Math = 9,
    Glue = 10,
    Kern = 11,
    Penalty = 12,
    Unset = 13,
    Style = 14,
    Choice = 15,
    MarginKern = 40,
}

pub(crate) const EDGE_NODE: TextNode = TextNode::Style;

impl From<u16> for TextNode {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect TextNode = {}", n))
    }
}

pub(crate) use whatsit::*;
pub(crate) mod whatsit {
    use super::{free_node, BaseBox, Deref, DerefMut, NodeSize, MEM};

    #[repr(u16)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
    pub(crate) enum WhatsItNST {
        Open = 0,
        Write = 1,
        Close = 2,
        Special = 3,
        Language = 4,
        PdfSavePos = 6,
        NativeWord = 40,
        NativeWordAt = 41,
        Glyph = 42,
        Pic = 43,
        Pdf = 44,
    }

    impl From<u16> for WhatsItNST {
        fn from(n: u16) -> Self {
            Self::n(n).expect(&format!("Incorrect WhatsItNST = {}", n))
        }
    }

    pub(crate) struct OpenFile(pub usize);
    impl OpenFile {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        pub(crate) unsafe fn id(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s0
        }
        pub(crate) unsafe fn set_id(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s0 = v;
            self
        }
        pub(crate) unsafe fn name(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s1
        }
        pub(crate) unsafe fn set_name(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s1 = v;
            self
        }
        pub(crate) unsafe fn area(&self) -> i32 {
            MEM[self.ptr() + 2].b32.s0
        }
        pub(crate) unsafe fn set_area(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 2].b32.s0 = v;
            self
        }
        pub(crate) unsafe fn ext(&self) -> i32 {
            MEM[self.ptr() + 2].b32.s1
        }
        pub(crate) unsafe fn set_ext(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 2].b32.s1 = v;
            self
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), super::OPEN_NODE_SIZE);
        }
    }

    pub(crate) struct WriteFile(pub usize);
    impl WriteFile {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        pub(crate) unsafe fn id(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s0
        }
        pub(crate) unsafe fn set_id(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s0 = v;
            self
        }
        /// "reference count of token list to write"
        pub(crate) unsafe fn tokens(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s1
        }
        pub(crate) unsafe fn set_tokens(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s1 = v;
            self
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), super::WRITE_NODE_SIZE);
        }
    }

    pub(crate) struct CloseFile(pub usize);
    impl CloseFile {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        pub(crate) unsafe fn id(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s0
        }
        pub(crate) unsafe fn set_id(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s0 = v;
            self
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), super::SMALL_NODE_SIZE);
        }
    }

    pub(crate) struct Language(pub usize);
    impl Language {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        /// language number, 0..255
        pub(crate) unsafe fn lang(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s1
        }
        pub(crate) unsafe fn set_lang(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s1 = v;
            self
        }
        /// "minimum left fragment, range 1..63"
        pub(crate) unsafe fn lhm(&self) -> u16 {
            MEM[self.ptr() + 1].b16.s1
        }
        pub(crate) unsafe fn set_lhm(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 1].b16.s1 = v;
            self
        }
        /// "minimum right fragment, range 1..63"
        pub(crate) unsafe fn rhm(&self) -> u16 {
            MEM[self.ptr() + 1].b16.s0
        }
        pub(crate) unsafe fn set_rhm(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 1].b16.s0 = v;
            self
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), super::SMALL_NODE_SIZE);
        }
    }

    pub(crate) struct Special(pub usize);
    impl Special {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        pub(crate) unsafe fn tokens(&self) -> i32 {
            MEM[self.ptr() + 1].b32.s1
        }
        pub(crate) unsafe fn set_tokens(&mut self, v: i32) -> &mut Self {
            MEM[self.ptr() + 1].b32.s1 = v;
            self
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), super::WRITE_NODE_SIZE);
        }
    }

    pub(crate) struct NativeWord(BaseBox);
    impl Deref for NativeWord {
        type Target = BaseBox;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    impl DerefMut for NativeWord {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }
    impl NativeWord {
        pub(crate) const fn from(p: usize) -> Self {
            Self(BaseBox(p))
        }
        pub(crate) unsafe fn size(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s3
        }
        pub(crate) unsafe fn set_size(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s3 = v;
            self
        }
        pub(crate) unsafe fn font(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s2
        }
        pub(crate) unsafe fn set_font(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s2 = v;
            self
        }
        /// number of UTF16 items in the text
        pub(crate) unsafe fn length(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s1
        }
        pub(crate) unsafe fn set_length(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s1 = v;
            self
        }
        pub(crate) unsafe fn glyph_count(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s0
        }
        pub(crate) unsafe fn set_glyph_count(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s0 = v;
            self
        }
        pub(crate) unsafe fn glyph_info_ptr(&self) -> *mut core::ffi::c_void {
            MEM[self.ptr() + 5].ptr
        }
        pub(crate) unsafe fn set_glyph_info_ptr(&mut self, p: *mut core::ffi::c_void) {
            MEM[self.ptr() + 5].ptr = p;
        }
        pub(crate) unsafe fn text(&self) -> &[u16] {
            let len = self.length() as usize;
            let pp = &MEM[self.ptr() + super::NATIVE_NODE_SIZE as usize].b16.s0 as *const u16;
            std::slice::from_raw_parts(pp, len)
        }
        pub(crate) unsafe fn text_mut(&mut self) -> &mut [u16] {
            let len = self.length() as usize;
            let pp = &mut MEM[self.ptr() + super::NATIVE_NODE_SIZE as usize].b16.s0 as *mut u16;
            std::slice::from_raw_parts_mut(pp, len)
        }
        pub(crate) unsafe fn set_metrics(&mut self, use_glyph_metrics: bool) {
            crate::xetex_ext::measure_native_node(self, use_glyph_metrics)
        }
        pub(crate) unsafe fn set_justified_native_glyphs(&mut self) {
            crate::xetex_ext::store_justified_native_glyphs(self)
        }
        pub(crate) unsafe fn italic_correction(&self) -> i32 {
            crate::xetex_ext::real_get_native_italic_correction(self)
        }
        pub(crate) unsafe fn make_xdv_glyph_array_data(&self) -> i32 {
            crate::xetex_ext::makeXDVGlyphArrayData(self)
        }
        pub(crate) unsafe fn native_glyph(&self, index: u32) -> u16 {
            crate::xetex_ext::real_get_native_glyph(self, index)
        }
        pub(crate) unsafe fn native_word_cp(&self, side: crate::xetex_consts::Side) -> i32 {
            crate::xetex_ext::real_get_native_word_cp(self, side)
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), self.size() as i32);
        }
    }

    pub(crate) struct Glyph(BaseBox);
    impl Deref for Glyph {
        type Target = BaseBox;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    impl DerefMut for Glyph {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }
    impl Glyph {
        pub(crate) const fn from(p: usize) -> Self {
            Self(BaseBox(p))
        }
        pub(crate) unsafe fn font(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s2
        }
        pub(crate) unsafe fn set_font(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s2 = v;
            self
        }
        pub(crate) unsafe fn glyph(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s1
        }
        pub(crate) unsafe fn set_glyph(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s1 = v;
            self
        }
        pub(crate) unsafe fn set_metrics(&mut self, use_glyph_metrics: bool) {
            crate::xetex_ext::measure_native_glyph(self, use_glyph_metrics)
        }
        pub(crate) unsafe fn italic_correction(&self) -> i32 {
            crate::xetex_ext::real_get_native_glyph_italic_correction(self)
        }
    }

    pub(crate) struct Picture(BaseBox);
    impl Deref for Picture {
        type Target = BaseBox;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
    impl DerefMut for Picture {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }
    impl Picture {
        pub(crate) const fn from(p: usize) -> Self {
            Self(BaseBox(p))
        }
        pub(crate) unsafe fn page(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s0
        }
        pub(crate) unsafe fn set_page(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s0 = v;
            self
        }
        /// number of bytes in the path item
        pub(crate) unsafe fn path_len(&self) -> u16 {
            MEM[self.ptr() + 4].b16.s1
        }
        pub(crate) unsafe fn set_path_len(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 4].b16.s1 = v;
            self
        }
        pub(crate) unsafe fn transform_matrix(&self) -> [i32; 6] {
            [
                MEM[self.ptr() + 5].b32.s0,
                MEM[self.ptr() + 5].b32.s1,
                MEM[self.ptr() + 6].b32.s0,
                MEM[self.ptr() + 6].b32.s1,
                MEM[self.ptr() + 7].b32.s0,
                MEM[self.ptr() + 7].b32.s1,
            ]
        }
        pub(crate) unsafe fn set_transform_matrix(&self, m: [i32; 6]) {
            std::slice::from_raw_parts_mut(&mut MEM[self.ptr() + 5].b32.s0, 6).copy_from_slice(&m);
        }
        pub(crate) unsafe fn pagebox(&self) -> u16 {
            MEM[self.ptr() + 8].b16.s1
        }
        pub(crate) unsafe fn set_pagebox(&mut self, v: u16) -> &mut Self {
            MEM[self.ptr() + 8].b16.s1 = v;
            self
        }
        pub(crate) unsafe fn path(&self) -> &[u8] {
            let len = self.path_len() as usize;
            let pp = &MEM[self.ptr() + super::PIC_NODE_SIZE as usize]
                as *const crate::xetex_ini::memory_word as *const u8;
            std::slice::from_raw_parts(pp, len)
        }
        pub(crate) unsafe fn path_mut(&mut self) -> &mut [u8] {
            let len = self.path_len() as usize;
            let pp = &mut MEM[self.ptr() + super::PIC_NODE_SIZE as usize]
                as *mut crate::xetex_ini::memory_word as *mut u8;
            std::slice::from_raw_parts_mut(pp, len)
        }
        pub(crate) unsafe fn total_size(&self) -> usize {
            let mw_size = std::mem::size_of::<crate::xetex_ini::memory_word>();
            (super::PIC_NODE_SIZE as usize) + ((self.path_len() as usize) + mw_size - 1) / mw_size
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), self.total_size() as i32);
        }
    }

    pub(crate) struct PdfSavePos(pub usize);
    impl NodeSize for PdfSavePos {
        const SIZE: i32 = super::SMALL_NODE_SIZE;
    }
    impl PdfSavePos {
        pub(crate) const fn ptr(&self) -> usize {
            self.0
        }
        pub(crate) unsafe fn free(self) {
            free_node(self.ptr(), Self::SIZE);
        }
    }
}

pub(crate) struct Kern(pub usize);
impl NodeSize for Kern {
    const SIZE: i32 = MEDIUM_NODE_SIZE;
}
impl Kern {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn subtype(&self) -> KernType {
        let n = MEM[self.ptr()].b16.s0;
        KernType::n(n).expect(&format!("Incorrect Kern type {}", n))
    }
    pub(crate) unsafe fn set_subtype(&mut self, v: KernType) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v as u16;
        self
    }
    pub(crate) unsafe fn width(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_width(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct MarginKern(pub usize);
impl NodeSize for MarginKern {
    const SIZE: i32 = MEDIUM_NODE_SIZE;
}
impl MarginKern {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn width(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_width(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Insertion(pub usize);
impl NodeSize for Insertion {
    const SIZE: i32 = INS_NODE_SIZE;
}
impl Insertion {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn box_reg(&self) -> u16 {
        MEM[self.ptr()].b16.s0
    }
    pub(crate) unsafe fn set_box_reg(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v;
        self
    }
    /// "the floating_penalty to be used"
    pub(crate) unsafe fn float_cost(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_float_cost(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn depth(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_depth(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn height(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_height(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
    /// a pointer to a vlist
    pub(crate) unsafe fn ins_ptr(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s0
    }
    pub(crate) unsafe fn set_ins_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s0 = v;
        self
    }
    /// a glue pointer
    pub(crate) unsafe fn split_top_ptr(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s1
    }
    pub(crate) unsafe fn set_split_top_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct PageInsertion(pub usize);
impl NodeSize for PageInsertion {
    const SIZE: i32 = PAGE_INS_NODE_SIZE;
}
impl PageInsertion {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn subtype(&self) -> PageInsType {
        let n = MEM[self.ptr()].b16.s1;
        PageInsType::n(n).expect(&format!("Incorrect Page Insertion type {}", n))
    }
    pub(crate) unsafe fn set_subtype(&mut self, v: PageInsType) -> &mut Self {
        MEM[self.ptr()].b16.s1 = v as u16;
        self
    }
    pub(crate) unsafe fn box_reg(&self) -> u16 {
        MEM[self.ptr()].b16.s0
    }
    pub(crate) unsafe fn set_box_reg(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v;
        self
    }
    /// an insertion for this class will break here if anywhere
    pub(crate) unsafe fn broken_ptr(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_broken_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    /// this insertion might break at broken_ptr
    pub(crate) unsafe fn broken_ins(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s0
    }
    pub(crate) unsafe fn set_broken_ins(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s0 = v;
        self
    }
    /// the most recent insertion for this subtype
    pub(crate) unsafe fn last_ins_ptr(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_last_ins_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    /// the optimum most recent insertion
    pub(crate) unsafe fn best_ins_ptr(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s0
    }
    pub(crate) unsafe fn set_best_ins_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s0 = v;
        self
    }
    pub(crate) unsafe fn height(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_height(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Penalty(pub usize);
impl NodeSize for Penalty {
    const SIZE: i32 = MEDIUM_NODE_SIZE;
}
impl Penalty {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn penalty(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_penalty(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
pub(crate) enum PageInsType {
    Inserting = 0,
    SplitUp = 1,
}

pub(crate) struct Choice(pub usize);
impl NodeSize for Choice {
    const SIZE: i32 = STYLE_NODE_SIZE;
}
impl Choice {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn display(&self) -> Option<usize> {
        MEM[self.ptr() + 1].b32.s0.opt()
    }
    pub(crate) unsafe fn set_display(&mut self, v: Option<usize>) {
        MEM[self.ptr() + 1].b32.s0 = v.tex_int();
    }
    pub(crate) unsafe fn text(&self) -> Option<usize> {
        MEM[self.ptr() + 1].b32.s1.opt()
    }
    pub(crate) unsafe fn set_text(&mut self, v: Option<usize>) {
        MEM[self.ptr() + 1].b32.s1 = v.tex_int();
    }
    pub(crate) unsafe fn script(&self) -> Option<usize> {
        MEM[self.ptr() + 2].b32.s0.opt()
    }
    pub(crate) unsafe fn set_script(&mut self, v: Option<usize>) {
        MEM[self.ptr() + 2].b32.s0 = v.tex_int();
    }
    pub(crate) unsafe fn scriptscript(&self) -> Option<usize> {
        MEM[self.ptr() + 2].b32.s1.opt()
    }
    pub(crate) unsafe fn set_scriptscript(&mut self, v: Option<usize>) {
        MEM[self.ptr() + 2].b32.s1 = v.tex_int();
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

#[derive(Clone, Copy)] // TODO: remove this
pub(crate) struct BaseBox(pub usize);
impl BaseBox {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    /// a scaled; 1 <=> WEB const `width_offset`
    pub(crate) unsafe fn width(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_width(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    /// a scaled; 2 <=> WEB const `depth_offset`
    pub(crate) unsafe fn depth(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_depth(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    /// a scaled; 3 <=> WEB const `height_offset`
    pub(crate) unsafe fn height(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_height(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
}

#[derive(Clone, Copy)] // TODO: remove this
pub(crate) struct Box(BaseBox);
impl Deref for Box {
    type Target = BaseBox;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Box {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl NodeSize for Box {
    const SIZE: i32 = BOX_NODE_SIZE;
}
impl Box {
    pub(crate) const fn from(p: usize) -> Self {
        Self(BaseBox(p))
    }
    /// subtype; records L/R direction mode
    pub(crate) unsafe fn lr_mode(&self) -> LRMode {
        LRMode::from(MEM[self.ptr()].b16.s0)
    }
    pub(crate) unsafe fn set_lr_mode(&mut self, mode: LRMode) -> &mut Self {
        MEM[self.ptr()].b16.s0 = mode as u16;
        self
    }
    pub(crate) unsafe fn shift_amount(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s1
    }
    pub(crate) unsafe fn set_shift_amount(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn list_ptr(&self) -> i32 {
        MEM[self.ptr() + 5].b32.s1
    }
    pub(crate) unsafe fn set_list_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 5].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn glue_sign(&self) -> GlueSign {
        GlueSign::from(MEM[self.ptr() + 5].b16.s1)
    }
    pub(crate) unsafe fn set_glue_sign(&mut self, v: GlueSign) -> &mut Self {
        MEM[self.ptr() + 5].b16.s1 = v as _;
        self
    }
    pub(crate) unsafe fn glue_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.ptr() + 5].b16.s0)
    }
    pub(crate) unsafe fn set_glue_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.ptr() + 5].b16.s0 = v as _;
        self
    }
    /// the glue ratio
    pub(crate) unsafe fn glue_set(&self) -> f64 {
        MEM[self.ptr() + 6].gr
    }
    pub(crate) unsafe fn set_glue_set(&mut self, v: f64) -> &mut Self {
        MEM[self.ptr() + 6].gr = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Unset(BaseBox);
impl Deref for Unset {
    type Target = BaseBox;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Unset {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl NodeSize for Unset {
    const SIZE: i32 = BOX_NODE_SIZE;
}
impl Unset {
    pub(crate) const fn from(p: usize) -> Self {
        Self(BaseBox(p))
    }
    pub(crate) unsafe fn columns(&self) -> u16 {
        MEM[self.ptr()].b16.s0
    }
    pub(crate) unsafe fn set_columns(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v;
        self
    }
    pub(crate) unsafe fn shrink(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s1
    }
    pub(crate) unsafe fn set_shrink(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn stretch(&self) -> i32 {
        MEM[self.ptr() + 6].b32.s1
    }
    pub(crate) unsafe fn set_stretch(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 6].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn stretch_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.ptr() + 5].b16.s0)
    }
    pub(crate) unsafe fn set_stretch_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.ptr() + 5].b16.s0 = v as _;
        self
    }
    pub(crate) unsafe fn shrink_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.ptr() + 5].b16.s1)
    }
    pub(crate) unsafe fn set_shrink_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.ptr() + 5].b16.s1 = v as _;
        self
    }
    pub(crate) unsafe fn list_ptr(&self) -> i32 {
        // TODO: check
        MEM[self.ptr() + 5].b32.s1
    }
    pub(crate) unsafe fn set_list_ptr(&mut self, v: i32) -> &mut Self {
        // TODO: check
        MEM[self.ptr() + 5].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Rule(BaseBox);
impl Deref for Rule {
    type Target = BaseBox;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Rule {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl NodeSize for Rule {
    const SIZE: i32 = RULE_NODE_SIZE;
}
impl Rule {
    pub(crate) const fn from(p: usize) -> Self {
        Self(BaseBox(p))
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) enum ActiveNode {
    Active(Active),
    Delta(Delta),
}

impl From<usize> for ActiveNode {
    fn from(p: usize) -> Self {
        const DELTA: u16 = 2;
        let n = unsafe { MEM[p].b16.s1 };
        match n {
            0 | 1 => Self::Active(Active(p)),
            DELTA => Self::Delta(Delta(p)),
            _ => panic!(format!("Incorrect node {}", n)),
        }
    }
}

pub(crate) struct Delta(pub usize);
impl NodeSize for Delta {
    const SIZE: i32 = DELTA_NODE_SIZE;
}
impl Delta {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    /// the "natural width" difference
    pub(crate) unsafe fn dwidth(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_dwidth(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    /// the stretch difference in points
    pub(crate) unsafe fn dstretch0(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_dstretch0(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    /// the stretch difference in fil
    pub(crate) unsafe fn dstretch1(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_dstretch1(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
    /// the stretch difference in fill
    pub(crate) unsafe fn dstretch2(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s1
    }
    pub(crate) unsafe fn set_dstretch2(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s1 = v;
        self
    }
    /// the stretch difference in filll
    pub(crate) unsafe fn dstretch3(&self) -> i32 {
        MEM[self.ptr() + 5].b32.s1
    }
    pub(crate) unsafe fn set_dstretch3(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 5].b32.s1 = v;
        self
    }
    /// the shrink difference
    pub(crate) unsafe fn dshrink(&self) -> i32 {
        MEM[self.ptr() + 6].b32.s1
    }
    pub(crate) unsafe fn set_dshrink(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 6].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

#[derive(Clone, Copy)] // TODO: remove this
pub(crate) struct Active(pub usize);
impl Active {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn fitness(&self) -> u16 {
        MEM[self.ptr()].b16.s0
    }
    pub(crate) unsafe fn set_fitness(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v;
        self
    }
    pub(crate) unsafe fn break_type(&self) -> BreakType {
        BreakType::from(MEM[self.ptr()].b16.s1)
    }
    pub(crate) unsafe fn set_break_type(&mut self, v: BreakType) -> &mut Self {
        MEM[self.ptr()].b16.s1 = v as u16;
        self
    }
    pub(crate) unsafe fn break_node(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_break_node(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn line_number(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s0
    }
    pub(crate) unsafe fn set_line_number(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s0 = v;
        self
    }
    pub(crate) unsafe fn total_demerits(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_total_demerits(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    /// a scaled; "active_short" in the WEB
    pub(crate) unsafe fn shortfall(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_shortfall(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn glue(&self) -> i32 {
        MEM[self.ptr() + 4].b32.s1
    }
    pub(crate) unsafe fn set_glue(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 4].b32.s1 = v;
        self
    }
}

#[derive(Clone, Copy)] // TODO: remove this
pub(crate) struct Passive(pub usize);
impl NodeSize for Passive {
    const SIZE: i32 = PASSIVE_NODE_SIZE;
}
impl Passive {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn serial(&self) -> i32 {
        MEM[self.ptr()].b32.s0
    }
    pub(crate) unsafe fn set_serial(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr()].b32.s0 = v;
        self
    }
    /// aka "llink" in doubly-linked list
    pub(crate) unsafe fn prev_break(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s0
    }
    pub(crate) unsafe fn set_prev_break(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s0 = v;
        self
    }
    pub(crate) unsafe fn next_break(&self) -> i32 {
        self.prev_break()
    }
    pub(crate) unsafe fn set_next_break(&mut self, v: i32) -> &mut Self {
        self.set_prev_break(v)
    }
    /// aka "rlink" in double-linked list
    pub(crate) unsafe fn cur_break(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_cur_break(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Adjust(pub usize);
impl NodeSize for Adjust {
    const SIZE: i32 = SMALL_NODE_SIZE;
}
impl Adjust {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn subtype(&self) -> AdjustType {
        let n = MEM[self.ptr()].b16.s0;
        AdjustType::n(n).expect(&format!("Incorrect Adjust type {}", n))
    }
    pub(crate) unsafe fn set_subtype(&mut self, v: AdjustType) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v as u16;
        self
    }
    pub(crate) unsafe fn adj_ptr(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_adj_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Discretionary(pub usize);
impl NodeSize for Discretionary {
    const SIZE: i32 = SMALL_NODE_SIZE;
}
impl Discretionary {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn replace_count(&self) -> u16 {
        MEM[self.ptr()].b16.s0
    }
    pub(crate) unsafe fn set_replace_count(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v;
        self
    }
    /// aka "llink" in doubly-linked list
    pub(crate) unsafe fn pre_break(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s0
    }
    pub(crate) unsafe fn set_pre_break(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s0 = v;
        self
    }
    /// aka "rlink" in double-linked list
    pub(crate) unsafe fn post_break(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_post_break(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }

    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct Ligature(pub usize);
impl NodeSize for Ligature {
    const SIZE: i32 = SMALL_NODE_SIZE;
}
impl Ligature {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn left_hit(&self) -> bool {
        MEM[self.ptr()].b16.s0 > 1
    }
    pub(crate) unsafe fn right_hit(&self) -> bool {
        MEM[self.ptr()].b16.s0 & 1 != 0
    }
    pub(crate) unsafe fn set_hits(&mut self, v: (bool, bool)) -> &mut Self {
        MEM[self.ptr()].b16.s0 = (v.0 as u16) * 2 + (v.1 as u16);
        self
    }
    /// WEB: font(char(p))
    pub(crate) unsafe fn font(&self) -> u16 {
        MEM[self.ptr() + 1].b16.s1
    }
    pub(crate) unsafe fn set_font(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr() + 1].b16.s1 = v;
        self
    }
    ///  WEB: character(char(p))
    pub(crate) unsafe fn char(&self) -> u16 {
        MEM[self.ptr() + 1].b16.s0
    }
    pub(crate) unsafe fn set_char(&mut self, v: u16) -> &mut Self {
        MEM[self.ptr() + 1].b16.s0 = v;
        self
    }
    /// WEB: link(char(p))
    pub(crate) unsafe fn lig_ptr(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_lig_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn free(self) {
        free_node(self.ptr(), Self::SIZE);
    }
}

pub(crate) struct GlueSpec(pub usize);
impl NodeSize for GlueSpec {
    const SIZE: i32 = GLUE_SPEC_SIZE;
}
impl GlueSpec {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn shrink_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.ptr()].b16.s0)
    }
    pub(crate) unsafe fn set_shrink_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.ptr()].b16.s0 = v as _;
        self
    }
    pub(crate) unsafe fn stretch_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.ptr()].b16.s1)
    }
    pub(crate) unsafe fn set_stretch_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.ptr()].b16.s1 = v as _;
        self
    }
    pub(crate) unsafe fn rc(&self) -> i32 {
        MEM[self.ptr()].b32.s1
    }
    pub(crate) unsafe fn rc_zero(&mut self) {
        MEM[self.ptr()].b32.s1 = 0;
    }
    pub(crate) unsafe fn rc_none(&mut self) {
        MEM[self.ptr()].b32.s1 = None.tex_int();
    }
    pub(crate) unsafe fn rc_inc(&mut self) {
        MEM[self.ptr()].b32.s1 += 1;
    }
    pub(crate) unsafe fn rc_dec(&mut self) {
        MEM[self.ptr()].b32.s1 -= 1;
    }
    pub(crate) unsafe fn size(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_size(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn stretch(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_stretch(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn shrink(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s1
    }
    pub(crate) unsafe fn set_shrink(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 3].b32.s1 = v;
        self
    }
}

/// e-TeX extended marks stuff
pub(crate) struct EtexMark(pub usize);
impl EtexMark {
    pub(crate) const fn ptr(&self) -> usize {
        self.0
    }
    /// \topmarks<n>
    pub(crate) unsafe fn sa_top_mark(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s0
    }
    pub(crate) unsafe fn set_sa_top_mark(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s0 = v;
        self
    }
    /// \firstmarks<n>
    pub(crate) unsafe fn sa_first_mark(&self) -> i32 {
        MEM[self.ptr() + 1].b32.s1
    }
    pub(crate) unsafe fn set_sa_first_mark(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 1].b32.s1 = v;
        self
    }
    /// \botmarks<n>
    pub(crate) unsafe fn sa_bot_mark(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s0
    }
    pub(crate) unsafe fn set_sa_bot_mark(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s0 = v;
        self
    }
    /// \splitfirstmarks<n>
    pub(crate) unsafe fn sa_split_first_mark(&self) -> i32 {
        MEM[self.ptr() + 2].b32.s1
    }
    pub(crate) unsafe fn set_sa_split_first_mark(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 2].b32.s1 = v;
        self
    }
    /// \splitbotmarks<n>
    pub(crate) unsafe fn sa_split_bot_mark(&self) -> i32 {
        MEM[self.ptr() + 3].b32.s0
    }
    pub(crate) unsafe fn set_sa_split_bot_mark(&mut self, v: i32) -> &mut Self {
        MEM[self.ptr() + 23].b32.s0 = v;
        self
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum LR {
    LeftToRight = 0,
    RightToLeft = 1,
}

impl core::ops::Not for LR {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::LeftToRight => Self::RightToLeft,
            Self::RightToLeft => Self::LeftToRight,
        }
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum LRMode {
    Normal = 0, // TODO: check name
    Reversed = 1,
    DList = 2,
}

impl From<u16> for LRMode {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect LRMode = {}", n))
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum AdjustType {
    Post = 0,
    Pre = 1,
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
pub(crate) enum InsNST {
    NS100 = 100, // Unknown
    NS200 = 200,
    NS253 = 253,
}

impl From<u16> for InsNST {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect InsNST = {}", n))
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
pub(crate) enum KernType {
    Normal = 0,
    Explicit = 1,
    AccKern = 2,
    SpaceAdjustment = 3,
    Math = 99,
}

/* Cmd::MathComp and others */
#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, enumn::N)]
pub(crate) enum MathNode {
    Ord = 16,
    Op = 17,
    Bin = 18,
    Rel = 19,
    Open = 20,
    Close = 21,
    Punct = 22,
    Inner = 23,
    Radical = 24,
    Fraction = 25,
    Under = 26,
    Over = 27,
    Accent = 28,
    VCenter = 29,
    Left = 30,
    Right = 31,
}

impl From<u16> for MathNode {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect MathNode = {}", n))
    }
}

pub(crate) const IF_NODE_SIZE: i32 = 2;
pub(crate) const PASSIVE_NODE_SIZE: i32 = 2;
pub(crate) const POINTER_NODE_SIZE: i32 = 2;
pub(crate) const SMALL_NODE_SIZE: i32 = 2;
pub(crate) const SPAN_NODE_SIZE: i32 = 2;
pub(crate) const WRITE_NODE_SIZE: i32 = 2;
pub(crate) const ACTIVE_NODE_SIZE_NORMAL: i32 = 3;
pub(crate) const EDGE_NODE_SIZE: i32 = 3;
pub(crate) const MARGIN_KERN_NODE_SIZE: i32 = 3;
pub(crate) const MEDIUM_NODE_SIZE: i32 = 3;
pub(crate) const MOVEMENT_NODE_SIZE: i32 = 3;
pub(crate) const OPEN_NODE_SIZE: i32 = 3;
pub(crate) const STYLE_NODE_SIZE: i32 = 3;
pub(crate) const WORD_NODE_SIZE: i32 = 3;
pub(crate) const EXPR_NODE_SIZE: i32 = 4;
pub(crate) const GLUE_SPEC_SIZE: i32 = 4;
pub(crate) const MARK_CLASS_NODE_SIZE: i32 = 4;
pub(crate) const PAGE_INS_NODE_SIZE: i32 = 4;
pub(crate) const ACTIVE_NODE_SIZE_EXTENDED: i32 = 5;
pub(crate) const GLYPH_NODE_SIZE: i32 = 5;
pub(crate) const INS_NODE_SIZE: i32 = 5;
pub(crate) const RULE_NODE_SIZE: i32 = 5;
pub(crate) const ALIGN_STACK_NODE_SIZE: i32 = 6;
pub(crate) const NATIVE_NODE_SIZE: i32 = 6;
pub(crate) const DELTA_NODE_SIZE: i32 = 7;
pub(crate) const BOX_NODE_SIZE: i32 = 8;
pub(crate) const PIC_NODE_SIZE: i32 = 9;
pub(crate) const INDEX_NODE_SIZE: i32 = 33;

pub(crate) const NOAD_SIZE: i32 = 4;
pub(crate) const ACCENT_NOAD_SIZE: i32 = 5;
pub(crate) const RADICAL_NOAD_SIZE: i32 = 5;
pub(crate) const FRACTION_NOAD_SIZE: i32 = 6;

/* How many memory words are needed for storing synctex information on various
 * kinds of nodes. This extra size is already included in the *_NODE_SIZE
 * definitions below.
 */
pub(crate) const SYNCTEX_FIELD_SIZE: i32 = 1;

pub(crate) trait NodeSize {
    const SIZE: i32;
}
