use std::ops::{Deref, DerefMut};

use crate::xetex_consts::{GlueOrder, GlueSign};
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

pub(crate) const INSERTING: TextNode = TextNode::HList;
pub(crate) const SPLIT_UP: TextNode = TextNode::VList;
pub(crate) const DELTA_NODE: TextNode = TextNode::Rule;
pub(crate) const EDGE_NODE: TextNode = TextNode::Style;

impl From<u16> for TextNode {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect TextNode = {}", n))
    }
}

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

pub(crate) struct Insertion(pub usize);
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
}

pub(crate) struct Choice(pub usize);
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
impl Rule {
    pub(crate) const fn from(p: usize) -> Self {
        Self(BaseBox(p))
    }
}

pub(crate) struct Delta(pub usize);
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
}

pub(crate) struct GlueSpec(pub usize);
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
pub(crate) enum KernNST {
    Normal = 0,
    Explicit = 1,
    AccKern = 2,
    SpaceAdjustment = 3,
}

impl From<u16> for KernNST {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("Incorrect KernNST = {}", n))
    }
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
