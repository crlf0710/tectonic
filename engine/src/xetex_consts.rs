pub mod cmd;
pub(crate) use cmd::*;
pub mod node;
pub(crate) use node::*;

use crate::xetex_ini::EQTB;

pub(crate) type placeholdertype = i32;
pub(crate) const MIN_HALFWORD: placeholdertype = -0x0FFFFFFF;
pub(crate) const MAX_HALFWORD: placeholdertype = 0x3FFFFFFF;

/// a null "pointer"
pub(crate) const TEX_NULL: placeholdertype = MIN_HALFWORD;
/// "the largest positive value that TeX knows"
pub(crate) const TEX_INFINITY: placeholdertype = 0x7FFFFFFF;
/// "signifies a missing item" in rule nodes */
pub(crate) const NULL_FLAG: placeholdertype = -0x40000000;
/// "denotes default_rule_thickness"
pub(crate) const DEFAULT_CODE: placeholdertype = 0x40000000;

/* characters
 *
 * TeX thinks there are only 256 character but we know better. We use UTF16
 * codepoints. Actual Unicode character codes can exceed this, up to
 * BIGGEST_USV. "USV" here means Unicode Scalar Value. */

/// must be <= max_quarterword
pub(crate) const BIGGEST_CHAR: placeholdertype = u16::MAX as i32;
pub(crate) const BIGGEST_USV: usize = 0x10FFFF;
pub(crate) const NUMBER_USVS: usize = BIGGEST_USV + 1;
pub(crate) const TOO_BIG_USV: usize = BIGGEST_USV + 1;

/* Various buffer sizes */

/// max number of control sequences
pub(crate) const HASH_SIZE: usize = 15000;
/// "a prime number equal to about 85% of hash_size
pub(crate) const HASH_PRIME: usize = 8501;

pub(crate) const MAX_FONT_MAX: usize = 9000;

pub(crate) const NUMBER_MATH_FAMILIES: usize = 256;
pub(crate) const TEXT_SIZE: usize = 0;
pub(crate) const SCRIPT_SIZE: usize = NUMBER_MATH_FAMILIES;
pub(crate) const SCRIPT_SCRIPT_SIZE: usize = 2 * NUMBER_MATH_FAMILIES;
pub(crate) const NUMBER_MATH_FONTS: usize = 3 * NUMBER_MATH_FAMILIES;

pub(crate) const NUMBER_REGS: usize = 256;

/// the size of our main "mem" array, minus 1; classically this is
/// configurable, but we hardcode it.
pub(crate) const MEM_TOP: usize = 4999999;

/* fixed locations in the "mem" array */
pub(crate) const PAGE_INS_HEAD: usize = MEM_TOP;
pub(crate) const CONTRIB_HEAD: usize = MEM_TOP - 1;
pub(crate) const PAGE_HEAD: usize = MEM_TOP - 2;
pub(crate) const TEMP_HEAD: usize = MEM_TOP - 3;
pub(crate) const HOLD_HEAD: usize = MEM_TOP - 4;
pub(crate) const ADJUST_HEAD: usize = MEM_TOP - 5;
/// note: two words
pub(crate) const ACTIVE_LIST: usize = MEM_TOP - 7;
pub(crate) const ALIGN_HEAD: usize = MEM_TOP - 8;
pub(crate) const END_SPAN: usize = MEM_TOP - 9;
pub(crate) const OMIT_TEMPLATE: usize = MEM_TOP - 10;
pub(crate) const NULL_LIST: usize = MEM_TOP - 11;
pub(crate) const LIG_TRICK: usize = MEM_TOP - 12;
/// note: same as LIG_TRICK
pub(crate) const GARBAGE: usize = MEM_TOP - 12;
pub(crate) const BACKUP_HEAD: usize = MEM_TOP - 13;
pub(crate) const PRE_ADJUST_HEAD: usize = MEM_TOP - 14;

/* equivalents table offsets */

/// "region 1": active character equivalents
pub(crate) const ACTIVE_BASE: usize = 1;
pub(crate) const SINGLE_BASE: usize = ACTIVE_BASE + NUMBER_USVS;
pub(crate) const NULL_CS: usize = SINGLE_BASE + NUMBER_USVS;
/// "region 2": hash table
pub(crate) const HASH_BASE: usize = (NULL_CS + 1) as usize;
pub(crate) const FROZEN_CONTROL_SEQUENCE: usize = HASH_BASE + HASH_SIZE;
pub(crate) const FROZEN_PROTECTION: usize = FROZEN_CONTROL_SEQUENCE + 0;
pub(crate) const FROZEN_CR: usize = FROZEN_CONTROL_SEQUENCE + 1;
pub(crate) const FROZEN_END_GROUP: usize = FROZEN_CONTROL_SEQUENCE + 2;
pub(crate) const FROZEN_RIGHT: usize = FROZEN_CONTROL_SEQUENCE + 3;
pub(crate) const FROZEN_FI: usize = FROZEN_CONTROL_SEQUENCE + 4;
pub(crate) const FROZEN_END_TEMPLATE: usize = FROZEN_CONTROL_SEQUENCE + 5;
pub(crate) const FROZEN_ENDV: usize = FROZEN_CONTROL_SEQUENCE + 6;
pub(crate) const FROZEN_RELAX: usize = FROZEN_CONTROL_SEQUENCE + 7;
pub(crate) const END_WRITE: usize = FROZEN_CONTROL_SEQUENCE + 8;
pub(crate) const FROZEN_DONT_EXPAND: usize = FROZEN_CONTROL_SEQUENCE + 9;
pub(crate) const FROZEN_SPECIAL: usize = FROZEN_CONTROL_SEQUENCE + 10;
pub(crate) const FROZEN_PRIMITIVE: usize = FROZEN_CONTROL_SEQUENCE + 11;
pub(crate) const FROZEN_NULL_FONT: usize = FROZEN_CONTROL_SEQUENCE + 12;
/// nominally minus FONT_BASE, but that's 0
pub(crate) const FONT_ID_BASE: usize = FROZEN_NULL_FONT;
pub(crate) const UNDEFINED_CONTROL_SEQUENCE: usize = FROZEN_NULL_FONT + MAX_FONT_MAX + 1;

/// "region 3": glue values
pub(crate) const GLUE_BASE: usize = UNDEFINED_CONTROL_SEQUENCE + 1;

#[repr(u16)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum GluePar {
    line_skip = 0,
    baseline_skip = 1,
    par_skip = 2,
    above_display_skip = 3,
    below_display_skip = 4,
    above_display_short_skip = 5,
    below_display_short_skip = 6,
    left_skip = 7,
    right_skip = 8,
    top_skip = 9,
    split_top_skip = 10,
    tab_skip = 11,
    space_skip = 12,
    xspace_skip = 13,
    par_fill_skip = 14,
    xetex_linebreak_skip = 15,
    thin_mu_skip = 16,
    med_mu_skip = 17,
    thick_mu_skip = 18,
}

pub(crate) const GLUE_PARS: usize = 19;

pub(crate) unsafe fn GLUEPAR(s: GluePar) -> &'static mut i32 {
    &mut EQTB[GLUE_BASE + s as usize].val
}

pub(crate) const SKIP_BASE: usize = GLUE_BASE + GLUE_PARS;
pub(crate) unsafe fn SKIP_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[SKIP_BASE + n].val
}

pub(crate) const MU_SKIP_BASE: usize = SKIP_BASE + NUMBER_REGS;
pub(crate) unsafe fn MU_SKIP_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[MU_SKIP_BASE + n].val
}

/* "region 4": local halfword values like baselineskip. Some of these are
 * used as arguments to ASSIGN_TOKS, SET_SHAPE, etc. */

pub(crate) const LOCAL_BASE: usize = MU_SKIP_BASE + NUMBER_REGS;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum Local {
    par_shape = 0,
    output_routine = 1,
    every_par = 2,
    every_math = 3,
    every_display = 4,
    every_hbox = 5,
    every_vbox = 6,
    every_job = 7,
    every_cr = 8,
    err_help = 9,
    every_eof = 10,
    xetex_inter_char = 11,
    TectonicCodaTokens = 12,
}

pub(crate) const NUM_LOCALS: usize = 13;

pub(crate) fn LOCAL(n: Local) -> &'static mut i32 {
    unsafe { &mut EQTB[LOCAL_BASE + n as usize].val }
}

pub(crate) const TOKS_BASE: usize = LOCAL_BASE + NUM_LOCALS;
pub(crate) unsafe fn TOKS_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[TOKS_BASE + n].val
}

pub(crate) const ETEX_PEN_BASE: usize = TOKS_BASE + NUMBER_REGS;
pub(crate) const INTER_LINE_PENALTIES_LOC: usize = ETEX_PEN_BASE + 0;
pub(crate) const CLUB_PENALTIES_LOC: usize = ETEX_PEN_BASE + 1;
pub(crate) const WIDOW_PENALTIES_LOC: usize = ETEX_PEN_BASE + 2;
pub(crate) const DISPLAY_WIDOW_PENALTIES_LOC: usize = ETEX_PEN_BASE + 3;
pub(crate) const ETEX_PENS: usize = ETEX_PEN_BASE + 4;

pub(crate) const BOX_BASE: usize = ETEX_PENS;
pub(crate) unsafe fn BOX_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[BOX_BASE + n].val
}

pub(crate) const CUR_FONT_LOC: usize = BOX_BASE + NUMBER_REGS;
pub(crate) const MATH_FONT_BASE: usize = CUR_FONT_LOC + 1;

pub(crate) unsafe fn MATH_FONT(n: usize) -> usize {
    EQTB[MATH_FONT_BASE + n].val as usize
}

pub(crate) const CAT_CODE_BASE: usize = MATH_FONT_BASE + NUMBER_MATH_FONTS;
pub(crate) unsafe fn CAT_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[CAT_CODE_BASE + n].val
}

pub(crate) const LC_CODE_BASE: usize = CAT_CODE_BASE + NUMBER_USVS;
pub(crate) unsafe fn LC_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[LC_CODE_BASE + n].val
}

pub(crate) const UC_CODE_BASE: usize = LC_CODE_BASE + NUMBER_USVS;
pub(crate) unsafe fn UC_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[UC_CODE_BASE + n].val
}

pub(crate) const SF_CODE_BASE: usize = UC_CODE_BASE + NUMBER_USVS;
pub(crate) unsafe fn SF_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[SF_CODE_BASE + n].val
}

pub(crate) const MATH_CODE_BASE: usize = SF_CODE_BASE + NUMBER_USVS;
pub(crate) unsafe fn MATH_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[MATH_CODE_BASE + n].val
}

pub(crate) const CHAR_SUB_CODE_BASE: usize = MATH_CODE_BASE + NUMBER_USVS;
pub(crate) unsafe fn CHAR_SUB_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[CHAR_SUB_CODE_BASE + n].val
}

/* "region 5": current fullword integers like hyphenation penalty */

pub(crate) const INT_BASE: usize = CHAR_SUB_CODE_BASE + NUMBER_USVS;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum IntPar {
    pretolerance = 0,
    tolerance = 1,
    line_penalty = 2,
    hyphen_penalty = 3,
    ex_hyphen_penalty = 4,
    club_penalty = 5,
    widow_penalty = 6,
    display_widow_penalty = 7,
    broken_penalty = 8,
    bin_op_penalty = 9,
    rel_penalty = 10,
    pre_display_penalty = 11,
    post_display_penalty = 12,
    inter_line_penalty = 13,
    double_hyphen_demerits = 14,
    final_hyphen_demerits = 15,
    adj_demerits = 16,
    mag = 17,
    delimiter_factor = 18,
    looseness = 19,
    time = 20,
    day = 21,
    month = 22,
    year = 23,
    show_box_breadth = 24,
    show_box_depth = 25,
    hbadness = 26,
    vbadness = 27,
    pausing = 28,
    tracing_online = 29,
    tracing_macros = 30,
    tracing_stats = 31,
    tracing_paragraphs = 32,
    tracing_pages = 33,
    tracing_output = 34,
    tracing_lost_chars = 35,
    tracing_commands = 36,
    tracing_restores = 37,
    uc_hyph = 38,
    output_penalty = 39,
    max_dead_cycles = 40,
    hang_after = 41,
    floating_penalty = 42,
    global_defs = 43,
    cur_fam = 44,
    escape_char = 45,
    default_hyphen_char = 46,
    default_skew_char = 47,
    end_line_char = 48,
    new_line_char = 49,
    language = 50,
    left_hyphen_min = 51,
    right_hyphen_min = 52,
    holding_inserts = 53,
    error_context_lines = 54,
    /// TEX_INT_PARS = WEB2C_INT_BASE
    char_sub_def_min = 55,
    char_sub_def_max = 56,
    tracing_char_sub_def = 57,
    /// = WEB2C_INT_PARS = ETEX_INT_BASE
    tracing_assigns = 58,
    tracing_groups = 59,
    tracing_ifs = 60,
    tracing_scan_tokens = 61,
    tracing_nesting = 62,
    pre_display_correction = 63,
    last_line_fit = 64,
    saving_vdiscards = 65,
    saving_hyphs = 66,
    suppress_fontnotfound_error = 67,
    xetex_linebreak_locale = 68,
    xetex_linebreak_penalty = 69,
    xetex_protrude_chars = 70,
    texxet = 71,
    xetex_dash_break = 72,
    xetex_upwards = 73,
    xetex_use_glyph_metrics = 74,
    xetex_inter_char_tokens = 75,
    xetex_input_normalization = 76,
    xetex_default_input_mode = 77,
    xetex_default_input_encoding = 78,
    xetex_tracing_fonts = 79,
    xetex_interword_space_shaping = 80,
    xetex_generate_actual_text = 81,
    xetex_hyphenatable_length = 82,
    synctex = 83,
    pdfoutput = 84,
}

pub(crate) const INT_PARS: usize = 85;

pub(crate) unsafe fn INTPAR(x: IntPar) -> &'static mut i32 {
    &mut EQTB[INT_BASE + x as usize].val
}

pub(crate) const COUNT_BASE: usize = INT_BASE + INT_PARS;
pub(crate) unsafe fn COUNT_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[COUNT_BASE + n].val
}

pub(crate) const DEL_CODE_BASE: usize = COUNT_BASE + NUMBER_REGS;
pub(crate) unsafe fn DEL_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[DEL_CODE_BASE + n].val
}

/* "region 6": current fullword dimensions like hsize */

pub(crate) const DIMEN_BASE: usize = DEL_CODE_BASE + NUMBER_USVS;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum DimenPar {
    par_indent = 0,
    math_surround = 1,
    line_skip_limit = 2,
    hsize = 3,
    vsize = 4,
    max_depth = 5,
    split_max_depth = 6,
    box_max_depth = 7,
    hfuzz = 8,
    vfuzz = 9,
    delimiter_shortfall = 10,
    null_delimiter_space = 11,
    script_space = 12,
    pre_display_size = 13,
    display_width = 14,
    display_indent = 15,
    overfull_rule = 16,
    hang_indent = 17,
    h_offset = 18,
    v_offset = 19,
    emergency_stretch = 20,
    pdf_page_width = 21,
    pdf_page_height = 22,
}

pub(crate) const DIMEN_PARS: usize = 23;

pub(crate) fn DIMENPAR(x: DimenPar) -> &'static mut i32 {
    unsafe { &mut EQTB[DIMEN_BASE + x as usize].val }
}

pub(crate) const SCALED_BASE: usize = DIMEN_BASE + DIMEN_PARS;
pub(crate) unsafe fn SCALED_REG(n: usize) -> &'static mut i32 {
    &mut EQTB[SCALED_BASE + n].val
}

pub(crate) const EQTB_SIZE: usize = SCALED_BASE + NUMBER_REGS - 1;

/// "really" MIN_QUARTERWORD
pub(crate) const LEVEL_ZERO: u16 = 0;
pub(crate) const LEVEL_ONE: u16 = 1;

/* args to Cmd::TopBotMark */
pub(crate) const TOP_MARK_CODE: usize = TopBotMarkCode::Top as usize;
pub(crate) const FIRST_MARK_CODE: usize = TopBotMarkCode::First as usize;
pub(crate) const BOT_MARK_CODE: usize = TopBotMarkCode::Bot as usize;
pub(crate) const SPLIT_FIRST_MARK_CODE: usize = TopBotMarkCode::SplitFirst as usize;
pub(crate) const SPLIT_BOT_MARK_CODE: usize = TopBotMarkCode::SplitBot as usize;

#[repr(i16)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum Expr {
    None = 0,
    Add = 1,
    Sub = 2,
    Mult = 3,
    Div = 4,
    Scale = 5,
}
impl From<i32> for Expr {
    fn from(n: i32) -> Self {
        Self::n(n as i16).expect(&format!("incorrect expression = {}", n))
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum GroupCode {
    BottomLevel = 0,
    Simple = 1,
    HBox = 2,
    AdjustedHBox = 3,
    VBox = 4,
    VTop = 5,
    Align = 6,
    NoAlign = 7,
    Output = 8,
    Math = 9,
    Disc = 10,
    Insert = 11,
    VCenter = 12,
    MathChoice = 13,
    SemiSimple = 14,
    MathShift = 15,
    MathLeft = 16,
}
impl From<u16> for GroupCode {
    fn from(n: u16) -> Self {
        Self::n(n as u8).unwrap()
    }
}

pub(crate) const SUP_CMD: placeholdertype = 0;
pub(crate) const SUB_CMD: placeholdertype = 1;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum GlueOrder {
    Normal = 0,
    Fil = 1,
    Fill = 2,
    Filll = 3,
    Incorrect = 4,
}

impl From<u16> for GlueOrder {
    fn from(n: u16) -> Self {
        Self::n(n as u8).unwrap_or(GlueOrder::Incorrect)
    }
}

pub(crate) const FIL: GlueOrder = GlueOrder::Fil;
pub(crate) const FILL: GlueOrder = GlueOrder::Fill;
pub(crate) const FILLL: GlueOrder = GlueOrder::Filll;

pub(crate) const LIG_TAG: placeholdertype = 1;
pub(crate) const LIST_TAG: placeholdertype = 2;
pub(crate) const EXT_TAG: placeholdertype = 3;

pub(crate) const NORMAL: u16 = 0;

/* scanner_status values: */
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum ScannerStatus {
    Normal = 0,
    Skipping = 1,
    Defining = 2,
    Matching = 3,
    Aligning = 4,
    Absorbing = 5,
}

/* special args for Cmd::TabMark, Cmd::CarRet */
pub(crate) const SPAN_CODE: placeholdertype = BIGGEST_USV as i32 + 2;
pub(crate) const CR_CODE: placeholdertype = BIGGEST_USV as i32 + 3;
pub(crate) const CR_CR_CODE: placeholdertype = BIGGEST_USV as i32 + 4;

/* Cmd::MakeBox, Cmd::UnHBox, Cmd::UnVBox */
pub(crate) const COPY_CODE: usize = BoxCode::Copy as usize;
pub(crate) const LAST_BOX_CODE: usize = BoxCode::LastBox as usize;
pub(crate) const VSPLIT_CODE: usize = BoxCode::VSplit as usize;

/* Cmd::LeaderShip */
// NORMAL
// GluePar
pub(crate) const COND_MATH_GLUE: u16 = 98;
pub(crate) const MU_GLUE: u16 = 99;
pub(crate) const A_LEADERS: u16 = 100;
pub(crate) const C_LEADERS: u16 = 101;
pub(crate) const X_LEADERS: u16 = 102;

/* Cmd::LimitSwitch */
#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum Limit {
    Normal = 0,
    Limits = 1,
    NoLimits = 2,
}

impl From<u16> for Limit {
    fn from(n: u16) -> Self {
        Self::n(n).unwrap()
    }
}

/* Cmd::MathStyle */
#[repr(i16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum MathStyle {
    Display = 0,
    Text = 1,
    Script = 2,
    ScriptScript = 3,
}

impl MathStyle {
    pub(crate) fn from_cur(cs: i16) -> Option<Self> {
        Self::n(cs / 2)
    }
    pub(crate) fn size(self) -> usize {
        match self {
            MathStyle::Display | MathStyle::Text => TEXT_SIZE,
            MathStyle::Script => SCRIPT_SIZE,
            MathStyle::ScriptScript => SCRIPT_SCRIPT_SIZE,
        }
    }
}

/* begin_token_list() types */

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum Btl {
    Parameter = 0,
    UTemplate = 1,
    VTemplate = 2,
    BackedUp = 3,
    BackedUpChar = 4,
    Inserted = 5,
    Macro = 6,
    OutputText = 7,
    EveryParText = 8,
    EveryMathText = 9,
    EveryDisplayText = 10,
    EveryHBoxText = 11,
    EveryVBoxText = 12,
    EveryJobText = 13,
    EveryCRText = 14,
    MarkText = 15,
    EveryEOFText = 16,
    InterCharText = 17,
    WriteText = 18,
    TectonicCodaText = 19,
}

impl Default for Btl {
    fn default() -> Self {
        Btl::Parameter
    }
}

impl From<u16> for Btl {
    fn from(n: u16) -> Self {
        Self::n(n).unwrap()
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum GlueSign {
    Normal = 0,
    Stretching = 1,
    Shrinking = 2,
}

impl From<u16> for GlueSign {
    fn from(n: u16) -> Self {
        Self::n(n).unwrap()
    }
}

/* input state */
#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum InputState {
    TokenList = 0,
    MidLine = 1,
    SkipBlanks = 17,
    NewLine = 33,
}

impl Default for InputState {
    fn default() -> Self {
        Self::TokenList
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum ListMode {
    NoMode = 0,
    VMode = 1,
    HMode = 104,
    MMode = 207,
}

impl Default for ListMode {
    fn default() -> Self {
        Self::NoMode
    }
}

impl From<u8> for ListMode {
    fn from(n: u8) -> Self {
        Self::n(n).unwrap()
    }
}

/* DVI format codes */
pub(crate) const XDV_ID_BYTE: u8 = 7;
pub(crate) const SPX_ID_BYTE: u8 = 100;

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum SaveCmd {
    RestoreOldValue = 0,
    RestoreZero = 1,
    InsertToken = 2,
    LevelBoundary = 3,
    RestoreSA = 4,
}
impl From<u16> for SaveCmd {
    fn from(n: u16) -> Self {
        Self::n(n as u8).expect(&format!("incorrect save command = {}", n))
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum BreakType {
    Unhyphenated = 0,
    Hyphenated = 1,
}
impl From<u16> for BreakType {
    fn from(n: u16) -> Self {
        Self::n(n).expect(&format!("incorrect break type = {}", n))
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum ValLevel {
    Int = 0,
    Dimen = 1,
    Glue = 2,
    Mu = 3,
    Ident = 4,
    Tok = 5,
    InterChar = 6,
    Mark = 7,
}

impl From<u8> for ValLevel {
    fn from(n: u8) -> Self {
        Self::n(n).expect(&format!("incorrect value level = {}", n))
    }
}

impl ValLevel {
    pub(crate) fn prev(&mut self) {
        // TODO: remove this
        use ValLevel::*;
        *self = match self {
            Int => panic!("Minimal value level"),
            Dimen => Self::Int,
            Glue => Self::Dimen,
            Mu => Self::Glue,
            Ident => Self::Mu,
            Tok => Self::Ident,
            InterChar => Self::Tok,
            Mark => Self::InterChar,
        };
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PageContents {
    Empty = 0,
    InsertsOnly = 1,
    BoxThere = 2,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum OpenMode {
    Normal = 0,
    JustOpen = 1,
    Closed = 2,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum PackMode {
    Exactly = 0,
    Additional = 1,
}

impl From<i32> for PackMode {
    fn from(n: i32) -> Self {
        Self::n(n as u8).expect(&format!("incorrect PackMode = {}", n))
    }
}

pub(crate) const DISPLAYOPERATORMINHEIGHT: placeholdertype = 3;
pub(crate) const ACCENTBASEHEIGHT: placeholdertype = 6;
pub(crate) const SUBSCRIPTTOPMAX: placeholdertype = 9;
pub(crate) const SUPERSCRIPTBOTTOMMIN: placeholdertype = 13;
pub(crate) const SUBSUPERSCRIPTGAPMIN: placeholdertype = 15;
pub(crate) const SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT: placeholdertype = 16;
pub(crate) const STACKGAPMIN: placeholdertype = 26;
pub(crate) const STACKDISPLAYSTYLEGAPMIN: placeholdertype = 27;
pub(crate) const FRACTIONNUMERATORGAPMIN: placeholdertype = 36;
pub(crate) const FRACTIONNUMDISPLAYSTYLEGAPMIN: placeholdertype = 37;
pub(crate) const FRACTIONDENOMINATORGAPMIN: placeholdertype = 39;
pub(crate) const FRACTIONDENOMDISPLAYSTYLEGAPMIN: placeholdertype = 40;
pub(crate) const RADICALVERTICALGAP: placeholdertype = 49;
pub(crate) const RADICALDISPLAYSTYLEVERTICALGAP: placeholdertype = 50;
pub(crate) const RADICALRULETHICKNESS: placeholdertype = 51;

pub(crate) const SET1: u8 = 128;
pub(crate) const SET_RULE: u8 = 132;
pub(crate) const PUT_RULE: u8 = 137;
pub(crate) const BOP: u8 = 139;
pub(crate) const EOP: u8 = 140;
pub(crate) const PUSH: u8 = 141;
pub(crate) const POP: u8 = 142;
pub(crate) const RIGHT1: u8 = 143;
pub(crate) const DOWN1: u8 = 157;
pub(crate) const FNT1: u8 = 235;
pub(crate) const XXX1: u8 = 239;
pub(crate) const XXX4: u8 = 242;
pub(crate) const FNT_DEF1: u8 = 243;
pub(crate) const PRE: u8 = 247;
pub(crate) const POST: u8 = 248;
pub(crate) const POST_POST: u8 = 249;
pub(crate) const DEFINE_NATIVE_FONT: u8 = 252;
pub(crate) const SET_GLYPHS: u8 = 253;
pub(crate) const SET_TEXT_AND_GLYPHS: u8 = 254;

pub(crate) const XETEX_VERSION: placeholdertype = 0;
pub(crate) const FONT_BASE: usize = 0;
pub(crate) const NON_ADDRESS: placeholdertype = 0;
pub(crate) const UNDEFINED_PRIMITIVE: placeholdertype = 0;
pub(crate) const PRIM_BASE: usize = 1;
pub(crate) const SLANT_CODE: placeholdertype = 1;
pub(crate) const ETEX_VERSION: placeholdertype = 2;
pub(crate) const SPACE_CODE: placeholdertype = 2;
pub(crate) const SPACE_SHRINK_CODE: placeholdertype = 4;
pub(crate) const X_HEIGHT_CODE: placeholdertype = 5;
pub(crate) const QUAD_CODE: placeholdertype = 6;
pub(crate) const EXTRA_SPACE_CODE: placeholdertype = 7;
pub(crate) const VAR_FAM_CLASS: placeholdertype = 7;
pub(crate) const NATIVE_GLYPH_INFO_SIZE: placeholdertype = 10;
pub(crate) const CARRIAGE_RETURN: placeholdertype = 13;
pub(crate) const TOTAL_MATHEX_PARAMS: placeholdertype = 13;
pub(crate) const HI_MEM_STAT_USAGE: placeholdertype = 15;
pub(crate) const MAX_CHAR_CODE: placeholdertype = 15;
pub(crate) const TOTAL_MATHSY_PARAMS: placeholdertype = 22;

pub(crate) const DIMEN_VAL_LIMIT: u16 = 128;
pub(crate) const BIGGEST_LANG: placeholdertype = 255;
pub(crate) const MU_VAL_LIMIT: u16 = 256;
pub(crate) const TOO_BIG_LANG: placeholdertype = 256;
pub(crate) const BOX_VAL_LIMIT: u16 = 320;
pub(crate) const TOK_VAL_LIMIT: u16 = 384;
pub(crate) const PRIM_PRIME: usize = 431;
pub(crate) const PRIM_SIZE: usize = 500;
pub(crate) const HYPH_PRIME: placeholdertype = 607;
pub(crate) const HYPHENATABLE_LENGTH_LIMIT: usize = 4095;
pub(crate) const CHAR_CLASS_LIMIT: placeholdertype = 4096;
pub(crate) const EJECT_PENALTY: placeholdertype = -10000;
pub(crate) const INF_BAD: placeholdertype = 10000;
pub(crate) const INF_PENALTY: placeholdertype = 10000;
pub(crate) const DEFAULT_RULE: placeholdertype = 26214;
pub(crate) const TOO_BIG_CHAR: placeholdertype = 65536;
pub(crate) const NO_EXPAND_FLAG: placeholdertype = BIGGEST_USV as i32 + 2;

pub(crate) const ACTIVE_MATH_CHAR: placeholdertype = 0x1FFFFF;

/* Token codes */

/// 1 << 21
pub(crate) const MAX_CHAR_VAL: placeholdertype = 0x200000;
pub(crate) const CS_TOKEN_FLAG: placeholdertype = 0x1FFFFFF;
/// LEFT_BRACE << 21
pub(crate) const LEFT_BRACE_TOKEN: placeholdertype = 0x200000;
/// (LEFT_BRACE + 1) << 21
pub(crate) const LEFT_BRACE_LIMIT: placeholdertype = 0x400000;
/// RIGHT_BRACE << 21
pub(crate) const RIGHT_BRACE_TOKEN: placeholdertype = 0x400000;
/// (RIGHT_BRACE + 1) << 21
pub(crate) const RIGHT_BRACE_LIMIT: placeholdertype = 0x600000;
/// MATH_SHIFT << 21
pub(crate) const MATH_SHIFT_TOKEN: placeholdertype = 0x600000;
/// TAB_MARK << 21
pub(crate) const TAB_TOKEN: placeholdertype = 0x800000;
/// OUT_PARAM << 21
pub(crate) const OUT_PARAM_TOKEN: placeholdertype = 0xA00000;
/// SPACER << 21 + ord(' ')
pub(crate) const SPACE_TOKEN: placeholdertype = 0x1400020;
/// LETTER << 21
pub(crate) const LETTER_TOKEN: placeholdertype = 0x1600000;
/// OTHER_CHAR << 21
pub(crate) const OTHER_TOKEN: placeholdertype = 0x1800000;
/// MATCH << 21
pub(crate) const MATCH_TOKEN: placeholdertype = 0x1A00000;
/// END_MATCH << 21
pub(crate) const END_MATCH_TOKEN: placeholdertype = 0x1C00000;
pub(crate) const PROTECTED_TOKEN: placeholdertype = END_MATCH_TOKEN + 1;

pub(crate) const A_TOKEN: placeholdertype = LETTER_TOKEN + 'A' as placeholdertype;
pub(crate) const OTHER_A_TOKEN: placeholdertype = OTHER_TOKEN + 'A' as placeholdertype;
pub(crate) const HEX_TOKEN: placeholdertype = OTHER_TOKEN + '"' as placeholdertype;
pub(crate) const OCTAL_TOKEN: placeholdertype = OTHER_TOKEN + '\'' as placeholdertype;
pub(crate) const CONTINENTAL_POINT_TOKEN: placeholdertype = OTHER_TOKEN + ',' as placeholdertype;
pub(crate) const POINT_TOKEN: placeholdertype = OTHER_TOKEN + '.' as placeholdertype;
pub(crate) const ZERO_TOKEN: placeholdertype = OTHER_TOKEN + '0' as placeholdertype;
pub(crate) const ALPHA_TOKEN: placeholdertype = OTHER_TOKEN + '`' as placeholdertype;

pub(crate) const BOX_FLAG: placeholdertype = 0x40000000;
pub(crate) const GLOBAL_BOX_FLAG: placeholdertype = 0x40008000;
pub(crate) const SHIP_OUT_FLAG: placeholdertype = 0x40010000;
pub(crate) const LEADER_FLAG: placeholdertype = 0x40010001;

pub(crate) const LP_CODE_BASE: placeholdertype = 2;
pub(crate) const RP_CODE_BASE: placeholdertype = 3;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Side {
    Left = 0,
    Right = 1,
}

/* modes to do_marks() */
#[repr(i16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum MarkMode {
    VSplitInit = 0,
    FireUpInit = 1,
    FireUpDone = 2,
    DestroyMarks = 3,
}

pub(crate) const MARKS_CODE: placeholdertype = 5;

pub(crate) const IGNORE_DEPTH: placeholdertype = -65536000;

pub(crate) const MIDDLE_NOAD: u16 = 1;

/* movement() */

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum MoveSeen {
    None = 0,
    Y = 6,
    Z = 12,
}
#[repr(i32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum MoveDir {
    YHere = 1,
    ZHere = 2,
    YZOk = 3,
    YOk = 4,
    ZOk = 5,
    DFixed = 6,
}
impl From<i32> for MoveDir {
    fn from(n: i32) -> Self {
        Self::n(n).expect(&format!("incorrect move direction = {}", n))
    }
}

/* Increase this whenever the engine internals change such that the contents
 * of the "format" files must be regenerated -- this includes changes to the
 * string pool. KEEP SYNCHRONIZED WITH src/lib.rs!!! */

pub(crate) const FORMAT_SERIAL: placeholdertype = 28;

/// Unicode file reading modes
#[repr(i32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum UnicodeMode {
    /// default: will become one of 1..3 at file open time, after sniffing
    Auto = 0,
    Utf8 = 1,
    Utf16be = 2,
    Utf16le = 3,
    Raw = 4,
    ICUMapping = 5,
}
impl From<i32> for UnicodeMode {
    fn from(n: i32) -> Self {
        Self::n(n).expect(&format!("incorrect unicode encoding mode = {}", n))
    }
}
