use crate::cmd::{BoxCode, TopBotMarkCode};
use crate::xetex_ini::EQTB;

use crate::xetex_scaledmath::Scaled;
use crate::xetex_xetexd::{TeXInt, TeXOpt};

use crate::node::GlueSpec;
/// specification for `0pt plus 0pt minus 0pt`
pub(crate) const ZERO_GLUE: GlueSpec = GlueSpec(0);
/// `0pt plus 1fil minus 0pt`
pub(crate) const FIL_GLUE: GlueSpec = GlueSpec(4);
/// `0pt plus 1fill minus 0pt`
pub(crate) const FILL_GLUE: GlueSpec = GlueSpec(8);
/// `0pt plus 1fil minus 1fil`
pub(crate) const SS_GLUE: GlueSpec = GlueSpec(12);
/// `0pt plus -1fil minus 0pt`
pub(crate) const FIL_NEG_GLUE: GlueSpec = GlueSpec(16);
/// largest statically allocated word in the variable-size `MEM`
pub(crate) const LO_MEM_STAT_MAX: i32 = 19;

pub(crate) type placeholdertype = i32;
pub(crate) const MIN_HALFWORD: placeholdertype = -0x0FFFFFFF;
pub(crate) const MAX_HALFWORD: placeholdertype = 0x3FFFFFFF;

/// a null "pointer"
pub(crate) const TEX_NULL: placeholdertype = MIN_HALFWORD;
/// "the largest positive value that TeX knows"
pub(crate) const TEX_INFINITY: placeholdertype = 0x7FFFFFFF;
/// "signifies a missing item" in rule nodes */
pub(crate) const NULL_FLAG: Scaled = Scaled(-0x4000_0000);
/// "denotes default_rule_thickness"
pub(crate) const DEFAULT_CODE: Scaled = Scaled(0x4000_0000);

/* characters
 *
 * TeX thinks there are only 256 character but we know better. We use UTF16
 * codepoints. Actual Unicode character codes can exceed this, up to
 * BIGGEST_USV. "USV" here means Unicode Scalar Value. */

/// must be <= max_quarterword
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

// /// smallest index in the `MEM`
// pub(crate) const MEM_BOT: usize = 0;
/// the size of our main "mem" array, minus 1; classically this is
/// configurable, but we hardcode it.
pub(crate) const MEM_TOP: usize = 4999999;

/* fixed locations in the "mem" array */
/// list of insertion data for current page
pub(crate) const PAGE_INS_HEAD: usize = MEM_TOP;
/// vlist of items not yet on current page
pub(crate) const CONTRIB_HEAD: usize = MEM_TOP - 1;
/// vlist for current page
pub(crate) const PAGE_HEAD: usize = MEM_TOP - 2;
/// head of a temporary list of some kind
pub(crate) const TEMP_HEAD: usize = MEM_TOP - 3;
/// head of a temporary list of another kind
pub(crate) const HOLD_HEAD: usize = MEM_TOP - 4;
/// head of adjustment list returned by `hpack`
pub(crate) const ADJUST_HEAD: usize = MEM_TOP - 5;
/// head of active list in `line_break`, needs two words
pub(crate) const ACTIVE_LIST: usize = MEM_TOP - 7;
///head of preamble list for alignments
pub(crate) const ALIGN_HEAD: usize = MEM_TOP - 8;
/// tail of spanned-width lists
pub(crate) const END_SPAN: usize = MEM_TOP - 9;
/// a constant token list
pub(crate) const OMIT_TEMPLATE: usize = MEM_TOP - 10;
/// permanently empty list
pub(crate) const NULL_LIST: usize = MEM_TOP - 11;
/// a ligature masquerading as a `char_node`
pub(crate) const LIG_TRICK: usize = MEM_TOP - 12;
/// note: same as LIG_TRICK
pub(crate) const GARBAGE: usize = MEM_TOP - 12;
/// head of token list built by `scan_keyword`
pub(crate) const BACKUP_HEAD: usize = MEM_TOP - 13;
/// head of pre-adjustment list returned by `hpack`
pub(crate) const PRE_ADJUST_HEAD: usize = MEM_TOP - 14;

/// smallest statically allocated word in the one-word `MEM`
pub(crate) const HI_MEM_STAT_MIN: usize = PRE_ADJUST_HEAD;

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

pub(crate) unsafe fn set_glue_par(s: GluePar, g: GlueSpec) {
    EQTB[GLUE_BASE + s as usize].val = g.ptr() as i32;
}

pub(crate) unsafe fn get_glue_par(s: GluePar) -> GlueSpec {
    GlueSpec(EQTB[GLUE_BASE + s as usize].val as usize)
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
pub(crate) unsafe fn get_box_reg(n: usize) -> Option<usize> {
    EQTB[BOX_BASE + n].val.opt()
}
pub(crate) unsafe fn set_box_reg(n: usize, v: Option<usize>) {
    EQTB[BOX_BASE + n].val = v.tex_int();
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
/*pub(crate) unsafe fn CHAR_SUB_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[CHAR_SUB_CODE_BASE + n].val
}*/

/// Region 5 of `EQTB` contains the integer parameters and registers defined
/// here, as well as the `del_code` table. The latter table differs from the
/// |cat_code..math_code| tables that precede it, since delimiter codes are
/// fullword integers while the other kinds of codes occupy at most a
/// halfword. This is what makes region~5 different from region~4. We will
/// store the `eq_level` information in an auxiliary array of quarterwords
/// that will be defined later.
pub(crate) const INT_BASE: usize = CHAR_SUB_CODE_BASE + NUMBER_USVS;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum IntPar {
    /// badness tolerance before hyphenation
    pretolerance = 0,
    /// badness tolerance after hyphenation
    tolerance = 1,
    /// added to the badness of every line
    line_penalty = 2,
    /// penalty for break after discretionary hyphen
    hyphen_penalty = 3,
    /// penalty for break after explicit hyphen
    ex_hyphen_penalty = 4,
    /// penalty for creating a club line
    club_penalty = 5,
    /// penalty for creating a widow line
    widow_penalty = 6,
    /// ditto, just before a display
    display_widow_penalty = 7,
    /// penalty for breaking a page at a broken line
    broken_penalty = 8,
    /// penalty for breaking after a binary operation
    bin_op_penalty = 9,
    /// penalty for breaking after a relation
    rel_penalty = 10,
    /// penalty for breaking just before a displayed formula
    pre_display_penalty = 11,
    /// penalty for breaking just after a displayed formula
    post_display_penalty = 12,
    /// additional penalty between lines
    inter_line_penalty = 13,
    /// demerits for double hyphen break
    double_hyphen_demerits = 14,
    /// demerits for final hyphen break
    final_hyphen_demerits = 15,
    /// demerits for adjacent incompatible lines
    adj_demerits = 16,
    /// magnification ratio
    mag = 17,
    /// ratio for variable-size delimiters
    delimiter_factor = 18,
    /// change in number of lines for a paragraph
    looseness = 19,
    /// current time of day
    time = 20,
    /// current day of the month
    day = 21,
    /// current month of the year
    month = 22,
    /// current year of our Lord
    year = 23,
    /// nodes per level in `show_box`
    show_box_breadth = 24,
    /// maximum level in `show_box`
    show_box_depth = 25,
    /// hboxes exceeding this badness will be shown by `hpack`
    hbadness = 26,
    /// vboxes exceeding this badness will be shown by `vpack`
    vbadness = 27,
    /// pause after each line is read from a file
    pausing = 28,
    /// show diagnostic output on terminal
    tracing_online = 29,
    /// show macros as they are being expanded
    tracing_macros = 30,
    /// show memory usage if \TeX knows it
    tracing_stats = 31,
    /// show line-break calculations
    tracing_paragraphs = 32,
    /// show page-break calculations
    tracing_pages = 33,
    /// show boxes when they are shipped out
    tracing_output = 34,
    /// show characters that aren't in the font
    tracing_lost_chars = 35,
    /// show command codes at `big_switch`
    tracing_commands = 36,
    /// show equivalents when they are restored
    tracing_restores = 37,
    /// hyphenate words beginning with a capital letter
    uc_hyph = 38,
    /// penalty found at current page break
    output_penalty = 39,
    /// bound on consecutive dead cycles of output
    max_dead_cycles = 40,
    /// hanging indentation changes after this many lines
    hang_after = 41,
    /// penalty for insertions heldover after a split
    floating_penalty = 42,
    /// override `\global` specifications
    global_defs = 43,
    /// current family
    cur_fam = 44,
    /// escape character for token output
    escape_char = 45,
    /// value of `\hyphenchar` when a font is loaded
    default_hyphen_char = 46,
    /// value of `\skewchar` when a font is loaded
    default_skew_char = 47,
    /// character placed at the right end of the buffer
    end_line_char = 48,
    /// character that prints as `print_ln`
    new_line_char = 49,
    /// current hyphenation table
    language = 50,
    /// minimum left hyphenation fragment size
    left_hyphen_min = 51,
    /// minimum right hyphenation fragment size
    right_hyphen_min = 52,
    /// do not remove insertion nodes from `\box255`
    holding_inserts = 53,
    /// maximum intermediate line pairs shown
    error_context_lines = 54,
    // total number of \TeX's integer parameters
    // TEX_INT_PARS = WEB2C_INT_BASE
    char_sub_def_min = 55,
    char_sub_def_max = 56,
    tracing_char_sub_def = 57,
    // = WEB2C_INT_PARS = ETEX_INT_BASE
    /// show assignments
    tracing_assigns = 58,
    /// show save/restore groups
    tracing_groups = 59,
    /// show conditionals}
    tracing_ifs = 60,
    /// show pseudo file open and close
    tracing_scan_tokens = 61,
    /// how incomplete groups and ifs within files
    tracing_nesting = 62,
    /// ext direction preceding a display
    pre_display_correction = 63,
    /// adjustment for last line of paragraph
    last_line_fit = 64,
    ///save items discarded from vlists
    saving_vdiscards = 65,
    /// save hyphenation codes for languages
    saving_hyphs = 66,
    /// suppress errors for missing fonts
    suppress_fontnotfound_error = 67,
    /// string number of locale to use for linebreak locations
    xetex_linebreak_locale = 68,
    /// penalty to use at locale-dependent linebreak locations
    xetex_linebreak_penalty = 69,
    /// protrude chars at left/right edge of paragraphs
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

pub(crate) unsafe fn get_int_par(x: IntPar) -> i32 {
    EQTB[INT_BASE + x as usize].val
}
pub(crate) unsafe fn set_int_par(x: IntPar, v: i32) {
    EQTB[INT_BASE + x as usize].val = v;
}

pub(crate) const COUNT_BASE: usize = INT_BASE + INT_PARS;
pub(crate) unsafe fn get_count_reg(n: usize) -> i32 {
    EQTB[COUNT_BASE + n].val
}
/*pub(crate) unsafe fn set_count_reg(n: usize, v: i32) {
    EQTB[COUNT_BASE + n].val = v;
}*/

pub(crate) const DEL_CODE_BASE: usize = COUNT_BASE + NUMBER_REGS;
pub(crate) unsafe fn DEL_CODE(n: usize) -> &'static mut i32 {
    &mut EQTB[DEL_CODE_BASE + n].val
}

/* "region 6": current fullword dimensions like hsize */

/// The final region of `EQTB` contains the dimension parameters defined
/// here, and the `number_regs` `\dimen` registers.
pub(crate) const DIMEN_BASE: usize = DEL_CODE_BASE + NUMBER_USVS;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum DimenPar {
    /// indentation of paragraphs
    par_indent = 0,
    /// space around math in text
    math_surround = 1,
    /// threshold for `line_skip` instead of |baseline_skip|
    line_skip_limit = 2,
    /// line width in horizontal mode
    hsize = 3,
    /// page height in vertical mode
    vsize = 4,
    /// maximum depth of boxes on main pages
    max_depth = 5,
    /// maximum depth of boxes on split pages
    split_max_depth = 6,
    /// maximum depth of explicit vboxes
    box_max_depth = 7,
    /// tolerance for overfull hbox messages
    hfuzz = 8,
    /// tolerance for overfull vbox messages
    vfuzz = 9,
    /// maximum amount uncovered by variable delimiters
    delimiter_shortfall = 10,
    /// blank space in null delimiters
    null_delimiter_space = 11,
    /// extra space after subscript or superscript
    script_space = 12,
    /// length of text preceding a display
    pre_display_size = 13,
    /// length of line for displayed equation
    display_width = 14,
    /// indentation of line for displayed equation
    display_indent = 15,
    /// width of rule that identifies overfull hboxes
    overfull_rule = 16,
    /// amount of hanging indentation
    hang_indent = 17,
    /// amount of horizontal offset when shipping pages out
    h_offset = 18,
    /// amount of vertical offset when shipping pages out
    v_offset = 19,
    /// reduces badnesses on final pass of line-breaking
    emergency_stretch = 20,
    /// page width of the PDF output
    pdf_page_width = 21,
    /// page height of the PDF output
    pdf_page_height = 22,
}

/// total number of dimension parameters
pub(crate) const DIMEN_PARS: usize = 23;

pub(crate) fn get_dimen_par(x: DimenPar) -> Scaled {
    unsafe { Scaled(EQTB[DIMEN_BASE + x as usize].val) }
}
pub(crate) fn set_dimen_par(x: DimenPar, v: Scaled) {
    unsafe { EQTB[DIMEN_BASE + x as usize].val = v.0 }
}

/// table of `number_regs` user-defined `\dimen` registers
pub(crate) const SCALED_BASE: usize = DIMEN_BASE + DIMEN_PARS;
pub(crate) unsafe fn get_scaled_reg(n: usize) -> Scaled {
    Scaled(EQTB[SCALED_BASE + n].val)
}
/*pub(crate) unsafe fn set_scaled_reg(n: usize, v: Scaled) {
    EQTB[SCALED_BASE + n].val = v.0
}*/

/// largest subscript of `EQTB`
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
        Self::n(n as i16).unwrap_or_else(|| panic!("incorrect expression = {}", n))
    }
}

/// `save_level` for a level boundary
///
/// Here are the group codes that are used to discriminate between different
/// kinds of groups. They allow \TeX\ to decide what special actions, if any,
/// should be performed when a group ends.
/// `\def\grp{\char'173...\char'175}`
///
/// Some groups are not supposed to be ended by right braces. For example,
/// the `$` that begins a math formula causes a `math_shift_group` to
/// be started, and this should be terminated by a matching `$`. Similarly,
/// a group that starts with `\left` should end with `\right`, and
/// one that starts with `\begingroup` should end with `\endgroup`.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, enumn::N)]
pub(crate) enum GroupCode {
    /// group code for the outside world
    BottomLevel = 0,
    /// group code for local structure only
    Simple = 1,
    /// code for `\hbox\grp`
    HBox = 2,
    /// code for `\hbox\grp` in vertical mode
    AdjustedHBox = 3,
    /// code for `\vbox\grp`
    VBox = 4,
    /// code for `\vtop\grp`
    VTop = 5,
    /// code for `\halign\grp`, `\valign\grp`
    Align = 6,
    /// code for `\noalign\grp`
    NoAlign = 7,
    /// code for output routine
    Output = 8,
    /// code for, e.g., `\char'136\grp`
    Math = 9,
    /// code for `\discretionary\grp\grp\grp`
    Disc = 10,
    /// code for `\insert\grp`, `\vadjust\grp`
    Insert = 11,
    /// code for `\vcenter\grp`
    VCenter = 12,
    /// code for `\mathchoice\grp\grp\grp\grp`
    MathChoice = 13,
    /// code for `\begingroup...\endgroup`
    SemiSimple = 14,
    /// code for `$...$`
    MathShift = 15,
    /// code for `\left...\right'
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

/// A variable called `scanner_status` tells `\TeX` whether or not to complain
/// when a subfile ends
///
/// If the `scanner_status` is not `Normal`, the variable `warning_index` points
/// to the `EQTB` location for the relevant control sequence name to print
/// in an error message.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum ScannerStatus {
    /// subfile can safely end here without incident
    Normal = 0,
    /// subfile can safely end here, but not a file,
    /// because we're reading past some conditional text that was not selected
    Skipping = 1,
    /// subfile shouldn't end now because a macro is being defined
    Defining = 2,
    /// subfile shouldn't end now because a macro is being used and
    /// we are searching for the end of its arguments
    Matching = 3,
    /// subfile shouldn't end now because we are
    /// not finished with the preamble of an `\halign` or `\valign`
    Aligning = 4,
    /// subfile shouldn't end now because we are
    /// reading a balanced token list for `\message`, `\write`, etc.
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

/// The `token_type` can take several values, depending on
/// where the current token list came from:
#[repr(u16)]
#[derive(Clone, Copy, Debug, PartialEq, enumn::N)]
pub(crate) enum Btl {
    /// if a parameter is being scanned
    Parameter = 0,
    /// if the `<u_j>` part of an alignment template is being scanned
    UTemplate = 1,
    /// if the `<v_j>` part of an alignment template is being scanned
    VTemplate = 2,
    /// if the token list being scanned has been inserted as "to be read again"
    BackedUp = 3,
    /// special code for backed-up char from `\XeTeXinterchartoks` hook
    BackedUpChar = 4,
    /// if the token list being scanned has been inserted as
    /// the text expansion of a `\count` or similar variable
    Inserted = 5,
    /// if a user-defined control sequence is being scanned
    Macro = 6,
    /// if an `\output` routine is being scanned
    OutputText = 7,
    /// if the text of `\everypar` is being scanned
    EveryParText = 8,
    /// if the text of `\everymath` is being scanned
    EveryMathText = 9,
    /// if the text of `\everydisplay` is being scanned;
    EveryDisplayText = 10,
    /// if the text of `\everyhbox` is being scanned
    EveryHBoxText = 11,
    /// if the text of `\everyvbox` is being scanned
    EveryVBoxText = 12,
    /// if the text of `\everyjob` is being scanned
    EveryJobText = 13,
    /// if the text of `\everycr` is being scanned
    EveryCRText = 14,
    /// if the text of a `\mark` is being scanned
    MarkText = 15,
    EveryEOFText = 16,
    InterCharText = 17,
    /// if the text of a `\write` is being scanned
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
        Self::n(n as u8).unwrap_or_else(|| panic!("incorrect save command = {}", n))
    }
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum BreakType {
    /// the `type` of a normal active break node
    Unhyphenated = 0,
    /// the `type` of an active node that breaks at a `disc_node`
    Hyphenated = 1,
}
impl From<u16> for BreakType {
    fn from(n: u16) -> Self {
        Self::n(n).unwrap_or_else(|| panic!("incorrect break type = {}", n))
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
        Self::n(n).unwrap_or_else(|| panic!("incorrect value level = {}", n))
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
        Self::n(n as u8).unwrap_or_else(|| panic!("incorrect PackMode = {}", n))
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
pub(crate) const X_HEIGHT_CODE: i32 = 5;
pub(crate) const QUAD_CODE: i32 = 6;
pub(crate) const EXTRA_SPACE_CODE: placeholdertype = 7;
pub(crate) const VAR_FAM_CLASS: placeholdertype = 7;
pub(crate) const NATIVE_GLYPH_INFO_SIZE: placeholdertype = 10;
pub(crate) const TOTAL_MATHEX_PARAMS: placeholdertype = 13;
pub(crate) const HI_MEM_STAT_USAGE: placeholdertype = 15;
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
pub(crate) const DEFAULT_RULE: Scaled = Scaled(26214); // 0.4
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

pub(crate) const BOX_FLAG: placeholdertype = 0x4000_0000;
pub(crate) const GLOBAL_BOX_FLAG: placeholdertype = 0x4000_8000;
pub(crate) const SHIP_OUT_FLAG: placeholdertype = 0x4001_0000;
pub(crate) const LEADER_FLAG: placeholdertype = 0x4001_0001;

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
        Self::n(n).unwrap_or_else(|| panic!("incorrect move direction = {}", n))
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
        Self::n(n).unwrap_or_else(|| panic!("incorrect unicode encoding mode = {}", n))
    }
}
