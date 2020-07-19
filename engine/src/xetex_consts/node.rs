use crate::xetex_consts::GlueOrder;
use crate::xetex_ini::MEM;
use crate::xetex_xetexd::TeXInt;

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
    pub(crate) fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn box_reg(&self) -> u16 {
        MEM[self.0].b16.s0
    }
    pub(crate) unsafe fn set_box_reg(&mut self, v: u16) -> &mut Self {
        MEM[self.0].b16.s0 = v;
        self
    }
    /// "the floating_penalty to be used"
    pub(crate) unsafe fn float_cost(&self) -> i32 {
        MEM[self.0 + 1].b32.s1
    }
    pub(crate) unsafe fn set_float_cost(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn depth(&self) -> i32 {
        MEM[self.0 + 2].b32.s1
    }
    pub(crate) unsafe fn set_depth(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 2].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn height(&self) -> i32 {
        MEM[self.0 + 3].b32.s1
    }
    pub(crate) unsafe fn set_height(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 3].b32.s1 = v;
        self
    }
    /// a pointer to a vlist
    pub(crate) unsafe fn ins_ptr(&self) -> i32 {
        MEM[self.0 + 4].b32.s0
    }
    pub(crate) unsafe fn set_ins_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 4].b32.s0 = v;
        self
    }
    /// a glue pointer
    pub(crate) unsafe fn split_top_ptr(&self) -> i32 {
        MEM[self.0 + 4].b32.s1
    }
    pub(crate) unsafe fn set_split_top_ptr(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 4].b32.s1 = v;
        self
    }
}

pub(crate) struct GlueSpec(pub usize);
impl GlueSpec {
    pub(crate) fn ptr(&self) -> usize {
        self.0
    }
    pub(crate) unsafe fn shrink_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.0].b16.s0)
    }
    pub(crate) unsafe fn set_shrink_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.0].b16.s0 = v as _;
        self
    }
    pub(crate) unsafe fn stretch_order(&self) -> GlueOrder {
        GlueOrder::from(MEM[self.0].b16.s1)
    }
    pub(crate) unsafe fn set_stretch_order(&mut self, v: GlueOrder) -> &mut Self {
        MEM[self.0].b16.s1 = v as _;
        self
    }
    pub(crate) unsafe fn rc(&self) -> i32 {
        MEM[self.0].b32.s1
    }
    pub(crate) unsafe fn rc_zero(&mut self) {
        MEM[self.0].b32.s1 = 0;
    }
    pub(crate) unsafe fn rc_none(&mut self) {
        MEM[self.0].b32.s1 = None.tex_int();
    }
    pub(crate) unsafe fn rc_inc(&mut self) {
        MEM[self.0].b32.s1 += 1;
    }
    pub(crate) unsafe fn rc_dec(&mut self) {
        MEM[self.0].b32.s1 -= 1;
    }
    pub(crate) unsafe fn size(&self) -> i32 {
        MEM[self.0 + 1].b32.s1
    }
    pub(crate) unsafe fn set_size(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 1].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn stretch(&self) -> i32 {
        MEM[self.0 + 2].b32.s1
    }
    pub(crate) unsafe fn set_stretch(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 2].b32.s1 = v;
        self
    }
    pub(crate) unsafe fn shrink(&self) -> i32 {
        MEM[self.0 + 3].b32.s1
    }
    pub(crate) unsafe fn set_shrink(&mut self, v: i32) -> &mut Self {
        MEM[self.0 + 3].b32.s1 = v;
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
