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
pub(crate) const BIGGEST_CHAR: placeholdertype = 0xFFFF;
pub(crate) const BIGGEST_USV: placeholdertype = 0x10FFFF;
pub(crate) const NUMBER_USVS: placeholdertype = (BIGGEST_USV + 1);
pub(crate) const TOO_BIG_USV: placeholdertype = (BIGGEST_USV + 1);

/* Various buffer sizes */

/// max number of control sequences
pub(crate) const HASH_SIZE: placeholdertype = 15000;
/// "a prime number equal to about 85% of hash_size
pub(crate) const HASH_PRIME: placeholdertype = 8501;

pub(crate) const MAX_FONT_MAX: placeholdertype = 9000;

pub(crate) const NUMBER_MATH_FAMILIES: placeholdertype = 256;
pub(crate) const TEXT_SIZE: placeholdertype = 0;
pub(crate) const SCRIPT_SIZE: placeholdertype = NUMBER_MATH_FAMILIES;
pub(crate) const SCRIPT_SCRIPT_SIZE: placeholdertype = (2 * NUMBER_MATH_FAMILIES);
pub(crate) const NUMBER_MATH_FONTS: placeholdertype = (3 * NUMBER_MATH_FAMILIES);

pub(crate) const NUMBER_REGS: placeholdertype = 256;

/// the size of our main "mem" array, minus 1; classically this is
/// configurable, but we hardcode it.
pub(crate) const MEM_TOP: placeholdertype = 4999999;

/* fixed locations in the "mem" array */
pub(crate) const PAGE_INS_HEAD: placeholdertype = MEM_TOP;
pub(crate) const CONTRIB_HEAD: placeholdertype = (MEM_TOP - 1);
pub(crate) const PAGE_HEAD: placeholdertype = (MEM_TOP - 2);
pub(crate) const TEMP_HEAD: placeholdertype = (MEM_TOP - 3);
pub(crate) const HOLD_HEAD: placeholdertype = (MEM_TOP - 4);
pub(crate) const ADJUST_HEAD: placeholdertype = (MEM_TOP - 5);
/// note: two words
pub(crate) const ACTIVE_LIST: placeholdertype = (MEM_TOP - 7);
pub(crate) const ALIGN_HEAD: placeholdertype = (MEM_TOP - 8);
pub(crate) const END_SPAN: placeholdertype = (MEM_TOP - 9);
pub(crate) const OMIT_TEMPLATE: placeholdertype = (MEM_TOP - 10);
pub(crate) const NULL_LIST: placeholdertype = (MEM_TOP - 11);
pub(crate) const LIG_TRICK: placeholdertype = (MEM_TOP - 12);
/// note: same as LIG_TRICK
pub(crate) const GARBAGE: placeholdertype = (MEM_TOP - 12);
pub(crate) const BACKUP_HEAD: placeholdertype = (MEM_TOP - 13);
pub(crate) const PRE_ADJUST_HEAD: placeholdertype = (MEM_TOP - 14);

/* equivalents table offsets */

/// "region 1": active character equivalents
pub(crate) const ACTIVE_BASE: placeholdertype = 1;
pub(crate) const SINGLE_BASE: placeholdertype = (ACTIVE_BASE + NUMBER_USVS);
pub(crate) const NULL_CS: placeholdertype = (SINGLE_BASE + NUMBER_USVS);
/// "region 2": hash table
pub(crate) const HASH_BASE: placeholdertype = (NULL_CS + 1);
pub(crate) const FROZEN_CONTROL_SEQUENCE: usize = (HASH_BASE + HASH_SIZE) as usize;
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
pub(crate) const FONT_ID_BASE: placeholdertype = FROZEN_NULL_FONT as i32;
pub(crate) const UNDEFINED_CONTROL_SEQUENCE: placeholdertype =
    (FROZEN_NULL_FONT as i32 + MAX_FONT_MAX + 1);

/// "region 3": glue values
pub(crate) const GLUE_BASE: placeholdertype = (UNDEFINED_CONTROL_SEQUENCE + 1);

#[repr(i32)]
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
pub(crate) const GLUE_PARS: placeholdertype = 19;

pub(crate) unsafe fn GLUEPAR(s: GluePar) -> &'static mut i32 {
    &mut EQTB[(GLUE_BASE + s as i32) as usize].b32.s1
}

pub(crate) const SKIP_BASE: placeholdertype = (GLUE_BASE + GLUE_PARS);
pub(crate) unsafe fn SKIP_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(SKIP_BASE + n) as usize].b32.s1
}

pub(crate) const MU_SKIP_BASE: placeholdertype = (SKIP_BASE + NUMBER_REGS);
pub(crate) unsafe fn MU_SKIP_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(MU_SKIP_BASE + (n)) as usize].b32.s1
}

/* "region 4": local halfword values like baselineskip. Some of these are
 * used as arguments to ASSIGN_TOKS, SET_SHAPE, etc. */

pub(crate) const LOCAL_BASE: placeholdertype = (MU_SKIP_BASE + NUMBER_REGS);

#[repr(i32)]
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
pub(crate) const NUM_LOCALS: placeholdertype = 13;

pub(crate) fn LOCAL(n: Local) -> &'static mut i32 {
    unsafe { &mut EQTB[(LOCAL_BASE + n as i32) as usize].b32.s1 }
}

pub(crate) const TOKS_BASE: placeholdertype = (LOCAL_BASE + NUM_LOCALS);
pub(crate) unsafe fn TOKS_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(TOKS_BASE + n) as usize].b32.s1
}

pub(crate) const ETEX_PEN_BASE: placeholdertype = (TOKS_BASE + NUMBER_REGS);
pub(crate) const INTER_LINE_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 0);
pub(crate) const CLUB_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 1);
pub(crate) const WIDOW_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 2);
pub(crate) const DISPLAY_WIDOW_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 3);
pub(crate) const ETEX_PENS: placeholdertype = (ETEX_PEN_BASE + 4);

pub(crate) const BOX_BASE: placeholdertype = ETEX_PENS;
pub(crate) unsafe fn BOX_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(BOX_BASE + n) as usize].b32.s1
}

pub(crate) const CUR_FONT_LOC: placeholdertype = (BOX_BASE + NUMBER_REGS);
pub(crate) const MATH_FONT_BASE: placeholdertype = (CUR_FONT_LOC + 1);

pub(crate) unsafe fn MATH_FONT(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(MATH_FONT_BASE + n) as usize].b32.s1
}

pub(crate) const CAT_CODE_BASE: placeholdertype = (MATH_FONT_BASE + NUMBER_MATH_FONTS);
pub(crate) unsafe fn CAT_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(CAT_CODE_BASE + n) as usize].b32.s1
}

pub(crate) const LC_CODE_BASE: placeholdertype = (CAT_CODE_BASE + NUMBER_USVS);
pub(crate) unsafe fn LC_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(LC_CODE_BASE + n) as usize].b32.s1
}

pub(crate) const UC_CODE_BASE: placeholdertype = (LC_CODE_BASE + NUMBER_USVS);
pub(crate) unsafe fn UC_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(UC_CODE_BASE + n) as usize].b32.s1
}

pub(crate) const SF_CODE_BASE: placeholdertype = (UC_CODE_BASE + NUMBER_USVS);
pub(crate) unsafe fn SF_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(SF_CODE_BASE + n) as usize].b32.s1
}

pub(crate) const MATH_CODE_BASE: placeholdertype = (SF_CODE_BASE + NUMBER_USVS);
pub(crate) unsafe fn MATH_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(MATH_CODE_BASE + n) as usize].b32.s1
}

pub(crate) const CHAR_SUB_CODE_BASE: placeholdertype = (MATH_CODE_BASE + NUMBER_USVS);
pub(crate) unsafe fn CHAR_SUB_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(CHAR_SUB_CODE_BASE + n) as usize].b32.s1
}

/* "region 5": current fullword integers like hyphenation penalty */

pub(crate) const INT_BASE: placeholdertype = (CHAR_SUB_CODE_BASE + NUMBER_USVS);

#[repr(i32)]
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
pub(crate) const INT_PARS: placeholdertype = 85;

pub(crate) unsafe fn INTPAR(x: IntPar) -> &'static mut i32 {
    &mut EQTB[(INT_BASE + x as i32) as usize].b32.s1
}

pub(crate) const COUNT_BASE: placeholdertype = (INT_BASE + INT_PARS);
pub(crate) unsafe fn COUNT_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(COUNT_BASE + n) as usize].b32.s1
}

pub(crate) const DEL_CODE_BASE: placeholdertype = (COUNT_BASE + NUMBER_REGS);
pub(crate) unsafe fn DEL_CODE(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(DEL_CODE_BASE + n) as usize].b32.s1
}

/* "region 6": current fullword dimensions like hsize */

pub(crate) const DIMEN_BASE: placeholdertype = (DEL_CODE_BASE + NUMBER_USVS);

#[repr(i32)]
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
pub(crate) const DIMEN_PARS: placeholdertype = 23;

pub(crate) fn DIMENPAR(x: DimenPar) -> &'static mut i32 {
    unsafe { &mut EQTB[(DIMEN_BASE + x as i32) as usize].b32.s1 }
}

pub(crate) const SCALED_BASE: placeholdertype = (DIMEN_BASE + DIMEN_PARS);
pub(crate) unsafe fn SCALED_REG(n: placeholdertype) -> &'static mut i32 {
    &mut EQTB[(SCALED_BASE + n) as usize].b32.s1
}

pub(crate) const EQTB_SIZE: placeholdertype = (SCALED_BASE + NUMBER_REGS - 1);

/// "really" MIN_QUARTERWORD
pub(crate) const LEVEL_ZERO: placeholdertype = 0;
pub(crate) const LEVEL_ONE: placeholdertype = 1;

/* SET_INTERACTION */
pub(crate) const BATCH_MODE: placeholdertype = 0;
pub(crate) const NONSTOP_MODE: placeholdertype = 1;
pub(crate) const SCROLL_MODE: placeholdertype = 2;
pub(crate) const ERROR_STOP_MODE: placeholdertype = 3;
pub(crate) const UNSPECIFIED_MODE: placeholdertype = 4;

pub(crate) const LEFT_TO_RIGHT: placeholdertype = 0;
pub(crate) const RIGHT_TO_LEFT: placeholdertype = 1;

/* How many memory words are needed for storing synctex information on various
 * kinds of nodes. This extra size is already included in the *_NODE_SIZE
 * definitions below.
 */
pub(crate) const SYNCTEX_FIELD_SIZE: placeholdertype = 1;

pub(crate) const HLIST_NODE: u16 = 0;
pub(crate) const VLIST_NODE: u16 = 1;
pub(crate) const DELTA_NODE: u16 = 2;
pub(crate) const RULE_NODE: u16 = 2;
pub(crate) const INS_NODE: u16 = 3;
pub(crate) const MARK_NODE: u16 = 4;
pub(crate) const ADJUST_NODE: u16 = 5;
pub(crate) const LIGATURE_NODE: u16 = 6;
pub(crate) const DISC_NODE: u16 = 7;
pub(crate) const WHATSIT_NODE: u16 = 8;
pub(crate) const MATH_NODE: u16 = 9;
pub(crate) const GLUE_NODE: u16 = 10;
pub(crate) const KERN_NODE: u16 = 11;
pub(crate) const PENALTY_NODE: u16 = 12;
pub(crate) const UNSET_NODE: u16 = 13;
pub(crate) const EDGE_NODE: u16 = 14;
pub(crate) const STYLE_NODE: u16 = 14;
pub(crate) const CHOICE_NODE: u16 = 15;
pub(crate) const MARGIN_KERN_NODE: u16 = 40;
pub(crate) const NATIVE_WORD_NODE: u16 = 40;
pub(crate) const NATIVE_WORD_NODE_AT: u16 = 41;
/// not to be confused with GLYPH_CODE = 43!
pub(crate) const GLYPH_NODE: u16 = 42;
/// not to be confused with PIC_FILE_CODE = 41!
pub(crate) const PIC_NODE: u16 = 43;
/// not to be confused with PDF_FILE_CODE = 42!
pub(crate) const PDF_NODE: u16 = 44;

pub(crate) const IF_NODE_SIZE: placeholdertype = 2;
pub(crate) const PASSIVE_NODE_SIZE: placeholdertype = 2;
pub(crate) const POINTER_NODE_SIZE: placeholdertype = 2;
pub(crate) const SMALL_NODE_SIZE: placeholdertype = 2;
pub(crate) const SPAN_NODE_SIZE: placeholdertype = 2;
pub(crate) const WRITE_NODE_SIZE: placeholdertype = 2;
pub(crate) const ACTIVE_NODE_SIZE_NORMAL: placeholdertype = 3;
pub(crate) const EDGE_NODE_SIZE: placeholdertype = 3;
pub(crate) const MARGIN_KERN_NODE_SIZE: placeholdertype = 3;
pub(crate) const MEDIUM_NODE_SIZE: placeholdertype = 3;
pub(crate) const MOVEMENT_NODE_SIZE: placeholdertype = 3;
pub(crate) const OPEN_NODE_SIZE: placeholdertype = 3;
pub(crate) const STYLE_NODE_SIZE: placeholdertype = 3;
pub(crate) const WORD_NODE_SIZE: placeholdertype = 3;
pub(crate) const EXPR_NODE_SIZE: placeholdertype = 4;
pub(crate) const GLUE_SPEC_SIZE: placeholdertype = 4;
pub(crate) const MARK_CLASS_NODE_SIZE: placeholdertype = 4;
pub(crate) const PAGE_INS_NODE_SIZE: placeholdertype = 4;
pub(crate) const ACTIVE_NODE_SIZE_EXTENDED: placeholdertype = 5;
pub(crate) const GLYPH_NODE_SIZE: placeholdertype = 5;
pub(crate) const INS_NODE_SIZE: placeholdertype = 5;
pub(crate) const RULE_NODE_SIZE: placeholdertype = 5;
pub(crate) const ALIGN_STACK_NODE_SIZE: placeholdertype = 6;
pub(crate) const NATIVE_NODE_SIZE: placeholdertype = 6;
pub(crate) const DELTA_NODE_SIZE: placeholdertype = 7;
pub(crate) const BOX_NODE_SIZE: placeholdertype = 8;
pub(crate) const PIC_NODE_SIZE: placeholdertype = 9;
pub(crate) const INDEX_NODE_SIZE: placeholdertype = 33;

pub(crate) const NOAD_SIZE: placeholdertype = 4;
pub(crate) const ACCENT_NOAD_SIZE: placeholdertype = 5;
pub(crate) const RADICAL_NOAD_SIZE: placeholdertype = 5;
pub(crate) const FRACTION_NOAD_SIZE: placeholdertype = 6;

/* MATH_COMP and others */
pub(crate) const ORD_NOAD: u16 = 16;
pub(crate) const OP_NOAD: u16 = 17;
pub(crate) const BIN_NOAD: u16 = 18;
pub(crate) const REL_NOAD: u16 = 19;
pub(crate) const OPEN_NOAD: u16 = 20;
pub(crate) const CLOSE_NOAD: u16 = 21;
pub(crate) const PUNCT_NOAD: u16 = 22;
pub(crate) const INNER_NOAD: u16 = 23;
pub(crate) const RADICAL_NOAD: u16 = 24;
pub(crate) const FRACTION_NOAD: u16 = 25;
pub(crate) const UNDER_NOAD: u16 = 26;
pub(crate) const OVER_NOAD: u16 = 27;
pub(crate) const ACCENT_NOAD: u16 = 28;
pub(crate) const VCENTER_NOAD: u16 = 29;
pub(crate) const LEFT_NOAD: u16 = 30;
pub(crate) const RIGHT_NOAD: u16 = 31;

/* args to TOP_BOT_MARK */
pub(crate) const TOP_MARK_CODE: placeholdertype = 0;
pub(crate) const FIRST_MARK_CODE: placeholdertype = 1;
pub(crate) const BOT_MARK_CODE: placeholdertype = 2;
pub(crate) const SPLIT_FIRST_MARK_CODE: placeholdertype = 3;
pub(crate) const SPLIT_BOT_MARK_CODE: placeholdertype = 4;

/* MATH_NODE stuff with L/R typesetting extras */
pub(crate) const BEFORE: placeholdertype = 0;
pub(crate) const AFTER: placeholdertype = 1;
pub(crate) const BEGIN_M_CODE: placeholdertype = 2;
pub(crate) const END_M_CODE: placeholdertype = 3;
pub(crate) const L_CODE: placeholdertype = 4;
pub(crate) const R_CODE: placeholdertype = 8;

pub(crate) const EXPR_NONE: placeholdertype = 0;
pub(crate) const EXPR_ADD: placeholdertype = 1;
pub(crate) const EXPR_SUB: placeholdertype = 2;
pub(crate) const EXPR_MULT: placeholdertype = 3;
pub(crate) const EXPR_DIV: placeholdertype = 4;
pub(crate) const EXPR_SCALE: placeholdertype = 5;

pub(crate) const BOTTOM_LEVEL: placeholdertype = 0;
pub(crate) const SIMPLE_GROUP: placeholdertype = 1;
pub(crate) const HBOX_GROUP: placeholdertype = 2;
pub(crate) const ADJUSTED_HBOX_GROUP: placeholdertype = 3;
pub(crate) const VBOX_GROUP: placeholdertype = 4;
pub(crate) const VTOP_GROUP: placeholdertype = 5;
pub(crate) const ALIGN_GROUP: placeholdertype = 6;
pub(crate) const NO_ALIGN_GROUP: placeholdertype = 7;
pub(crate) const OUTPUT_GROUP: placeholdertype = 8;
pub(crate) const MATH_GROUP: placeholdertype = 9;
pub(crate) const DISC_GROUP: placeholdertype = 10;
pub(crate) const INSERT_GROUP: placeholdertype = 11;
pub(crate) const VCENTER_GROUP: placeholdertype = 12;
pub(crate) const MATH_CHOICE_GROUP: placeholdertype = 13;
pub(crate) const SEMI_SIMPLE_GROUP: placeholdertype = 14;
pub(crate) const MATH_SHIFT_GROUP: placeholdertype = 15;
pub(crate) const MATH_LEFT_GROUP: placeholdertype = 16;

pub(crate) const SUP_CMD: placeholdertype = 0;
pub(crate) const SUB_CMD: placeholdertype = 1;

pub(crate) const FIL: placeholdertype = 1;
pub(crate) const FILL: placeholdertype = 2;
pub(crate) const FILLL: placeholdertype = 3;

pub(crate) const LIG_TAG: placeholdertype = 1;
pub(crate) const LIST_TAG: placeholdertype = 2;
pub(crate) const EXT_TAG: placeholdertype = 3;

/* scanner_status values: */
pub(crate) const NORMAL: placeholdertype = 0;
pub(crate) const SKIPPING: placeholdertype = 1;
pub(crate) const DEFINING: placeholdertype = 2;
pub(crate) const MATCHING: placeholdertype = 3;
pub(crate) const ALIGNING: placeholdertype = 4;
pub(crate) const ABSORBING: placeholdertype = 5;

/* commands */

pub(crate) const ESCAPE: u16 = 0;
/// = ESCAPE
pub(crate) const RELAX: u16 = 0;
pub(crate) const LEFT_BRACE: u16 = 1;
pub(crate) const RIGHT_BRACE: u16 = 2;
pub(crate) const MATH_SHIFT: u16 = 3;
pub(crate) const TAB_MARK: u16 = 4;
pub(crate) const CAR_RET: u16 = 5;
/// = CAR_RET
pub(crate) const OUT_PARAM: u16 = 5;
pub(crate) const MAC_PARAM: u16 = 6;
pub(crate) const SUP_MARK: u16 = 7;
pub(crate) const SUB_MARK: u16 = 8;
pub(crate) const IGNORE: u16 = 9;
/// = IGNORE
pub(crate) const ENDV: u16 = 9;
pub(crate) const SPACER: u16 = 10;
pub(crate) const LETTER: u16 = 11;
pub(crate) const OTHER_CHAR: u16 = 12;
pub(crate) const ACTIVE_CHAR: u16 = 13;
/// = ACTIVE_CHAR
pub(crate) const PAR_END: u16 = 13;
/// = ACTIVE_CHAR
pub(crate) const MATCH: u16 = 13;
pub(crate) const COMMENT: u16 = 14;
/// = COMMENT
pub(crate) const END_MATCH: u16 = 14;
/// = COMMENT
pub(crate) const STOP: u16 = 14;
pub(crate) const INVALID_CHAR: u16 = 15;
/// = INVALID_CHAR
pub(crate) const DELIM_NUM: u16 = 15;
pub(crate) const CHAR_NUM: u16 = 16;
pub(crate) const MATH_CHAR_NUM: u16 = 17;
pub(crate) const MARK: u16 = 18;
pub(crate) const XRAY: u16 = 19;
pub(crate) const MAKE_BOX: u16 = 20;
pub(crate) const HMOVE: u16 = 21;
pub(crate) const VMOVE: u16 = 22;
pub(crate) const UN_HBOX: u16 = 23;
pub(crate) const UN_VBOX: u16 = 24;
pub(crate) const REMOVE_ITEM: u16 = 25;
pub(crate) const HSKIP: u16 = 26;
pub(crate) const VSKIP: u16 = 27;
pub(crate) const MSKIP: u16 = 28;
pub(crate) const KERN: u16 = 29;
pub(crate) const MKERN: u16 = 30;
pub(crate) const LEADER_SHIP: u16 = 31;
pub(crate) const HALIGN: u16 = 32;
pub(crate) const VALIGN: u16 = 33;
pub(crate) const NO_ALIGN: u16 = 34;
pub(crate) const VRULE: u16 = 35;
pub(crate) const HRULE: u16 = 36;
pub(crate) const INSERT: u16 = 37;
pub(crate) const VADJUST: u16 = 38;
pub(crate) const IGNORE_SPACES: u16 = 39;
pub(crate) const AFTER_ASSIGNMENT: u16 = 40;
pub(crate) const AFTER_GROUP: u16 = 41;
pub(crate) const BREAK_PENALTY: u16 = 42;
pub(crate) const START_PAR: u16 = 43;
pub(crate) const ITAL_CORR: u16 = 44;
pub(crate) const ACCENT: u16 = 45;
pub(crate) const MATH_ACCENT: u16 = 46;
pub(crate) const DISCRETIONARY: u16 = 47;
pub(crate) const EQ_NO: u16 = 48;
pub(crate) const LEFT_RIGHT: u16 = 49;
pub(crate) const MATH_COMP: u16 = 50;
pub(crate) const LIMIT_SWITCH: u16 = 51;
pub(crate) const ABOVE: u16 = 52;
pub(crate) const MATH_STYLE: u16 = 53;
pub(crate) const MATH_CHOICE: u16 = 54;
pub(crate) const NON_SCRIPT: u16 = 55;
pub(crate) const VCENTER: u16 = 56;
pub(crate) const CASE_SHIFT: u16 = 57;
pub(crate) const MESSAGE: u16 = 58;
pub(crate) const EXTENSION: u16 = 59;
pub(crate) const IN_STREAM: u16 = 60;
pub(crate) const BEGIN_GROUP: u16 = 61;
pub(crate) const END_GROUP: u16 = 62;
pub(crate) const OMIT: u16 = 63;
pub(crate) const EX_SPACE: u16 = 64;
pub(crate) const NO_BOUNDARY: u16 = 65;
pub(crate) const RADICAL: u16 = 66;
pub(crate) const END_CS_NAME: u16 = 67;
pub(crate) const CHAR_GIVEN: u16 = 68;
pub(crate) const MIN_INTERNAL: u16 = 68;
pub(crate) const MATH_GIVEN: u16 = 69;
pub(crate) const XETEX_MATH_GIVEN: u16 = 70;
pub(crate) const LAST_ITEM: u16 = 71;
pub(crate) const MAX_NON_PREFIXED_COMMAND: u16 = 71;
pub(crate) const TOKS_REGISTER: u16 = 72;
pub(crate) const ASSIGN_TOKS: u16 = 73;
pub(crate) const ASSIGN_INT: u16 = 74;
pub(crate) const ASSIGN_DIMEN: u16 = 75;
pub(crate) const ASSIGN_GLUE: u16 = 76;
pub(crate) const ASSIGN_MU_GLUE: u16 = 77;
pub(crate) const ASSIGN_FONT_DIMEN: u16 = 78;
pub(crate) const ASSIGN_FONT_INT: u16 = 79;
pub(crate) const SET_AUX: u16 = 80;
pub(crate) const SET_PREV_GRAF: u16 = 81;
pub(crate) const SET_PAGE_DIMEN: u16 = 82;
pub(crate) const SET_PAGE_INT: u16 = 83;
pub(crate) const SET_BOX_DIMEN: u16 = 84;
pub(crate) const SET_SHAPE: u16 = 85;
pub(crate) const DEF_CODE: u16 = 86;
pub(crate) const XETEX_DEF_CODE: u16 = 87;
pub(crate) const DEF_FAMILY: u16 = 88;
pub(crate) const SET_FONT: u16 = 89;
pub(crate) const DEF_FONT: u16 = 90;
pub(crate) const MAX_INTERNAL: u16 = 91;
pub(crate) const REGISTER: u16 = 91;
pub(crate) const ADVANCE: u16 = 92;
pub(crate) const MULTIPLY: u16 = 93;
pub(crate) const DIVIDE: u16 = 94;
pub(crate) const PREFIX: u16 = 95;
pub(crate) const LET: u16 = 96;
pub(crate) const SHORTHAND_DEF: u16 = 97;
pub(crate) const READ_TO_CS: u16 = 98;
pub(crate) const DEF: u16 = 99;
pub(crate) const SET_BOX: u16 = 100;
pub(crate) const HYPH_DATA: u16 = 101;
pub(crate) const SET_INTERACTION: u16 = 102;
pub(crate) const EXPAND_AFTER: u16 = 104;
pub(crate) const NO_EXPAND: u16 = 105;
pub(crate) const INPUT: u16 = 106;
pub(crate) const IF_TEST: u16 = 107;
pub(crate) const FI_OR_ELSE: u16 = 108;
pub(crate) const CS_NAME: u16 = 109;
pub(crate) const CONVERT: u16 = 110;
pub(crate) const THE: u16 = 111;
pub(crate) const TOP_BOT_MARK: u16 = 112;

/* args to SET_BOX_DIMEN */
pub(crate) const WIDTH_OFFSET: placeholdertype = 1;
pub(crate) const DEPTH_OFFSET: placeholdertype = 2;
pub(crate) const HEIGHT_OFFSET: placeholdertype = 3;

/* args to LAST_ITEM -- heavily overloaded by (X)eTeX for extensions */
pub(crate) const INT_VAL: placeholdertype = 0;
pub(crate) const DIMEN_VAL: placeholdertype = 1;
pub(crate) const GLUE_VAL: placeholdertype = 2;
pub(crate) const MU_VAL: placeholdertype = 3;
pub(crate) const LAST_NODE_TYPE_CODE: placeholdertype = 3;
pub(crate) const INPUT_LINE_NO_CODE: placeholdertype = 4;
pub(crate) const BADNESS_CODE: placeholdertype = 5;
pub(crate) const ETEX_VERSION_CODE: placeholdertype = 6;
pub(crate) const CURRENT_GROUP_LEVEL_CODE: placeholdertype = 7;
pub(crate) const CURRENT_GROUP_TYPE_CODE: placeholdertype = 8;
pub(crate) const CURRENT_IF_LEVEL_CODE: placeholdertype = 9;
pub(crate) const CURRENT_IF_TYPE_CODE: placeholdertype = 10;
pub(crate) const CURRENT_IF_BRANCH_CODE: placeholdertype = 11;
pub(crate) const GLUE_STRETCH_ORDER_CODE: placeholdertype = 12;
pub(crate) const GLUE_SHRINK_ORDER_CODE: placeholdertype = 13;
pub(crate) const XETEX_VERSION_CODE: placeholdertype = 14;
pub(crate) const XETEX_COUNT_GLYPHS_CODE: placeholdertype = 15;
pub(crate) const XETEX_COUNT_VARIATIONS_CODE: placeholdertype = 16;
pub(crate) const XETEX_VARIATION_CODE: placeholdertype = 17;
pub(crate) const XETEX_FIND_VARIATION_BY_NAME_CODE: placeholdertype = 18;
pub(crate) const XETEX_VARIATION_MIN_CODE: placeholdertype = 19;
pub(crate) const XETEX_VARIATION_MAX_CODE: placeholdertype = 20;
pub(crate) const XETEX_VARIATION_DEFAULT_CODE: placeholdertype = 21;
pub(crate) const XETEX_COUNT_FEATURES_CODE: placeholdertype = 22;
pub(crate) const XETEX_FEATURE_CODE_CODE: placeholdertype = 23;
pub(crate) const XETEX_FIND_FEATURE_BY_NAME_CODE: placeholdertype = 24;
pub(crate) const XETEX_IS_EXCLUSIVE_FEATURE_CODE: placeholdertype = 25;
pub(crate) const XETEX_COUNT_SELECTORS_CODE: placeholdertype = 26;
pub(crate) const XETEX_SELECTOR_CODE_CODE: placeholdertype = 27;
pub(crate) const XETEX_FIND_SELECTOR_BY_NAME_CODE: placeholdertype = 28;
pub(crate) const XETEX_IS_DEFAULT_SELECTOR_CODE: placeholdertype = 29;
pub(crate) const XETEX_OT_COUNT_SCRIPTS_CODE: placeholdertype = 30;
pub(crate) const XETEX_OT_COUNT_LANGUAGES_CODE: placeholdertype = 31;
pub(crate) const XETEX_OT_COUNT_FEATURES_CODE: placeholdertype = 32;
pub(crate) const XETEX_OT_SCRIPT_CODE: placeholdertype = 33;
pub(crate) const XETEX_OT_LANGUAGE_CODE: placeholdertype = 34;
pub(crate) const XETEX_OT_FEATURE_CODE: placeholdertype = 35;
pub(crate) const XETEX_MAP_CHAR_TO_GLYPH_CODE: placeholdertype = 36;
pub(crate) const XETEX_GLYPH_INDEX_CODE: placeholdertype = 37;
pub(crate) const XETEX_FONT_TYPE_CODE: placeholdertype = 38;
pub(crate) const XETEX_FIRST_CHAR_CODE: placeholdertype = 39;
pub(crate) const XETEX_LAST_CHAR_CODE: placeholdertype = 40;
pub(crate) const PDF_LAST_X_POS_CODE: placeholdertype = 41;
pub(crate) const PDF_LAST_Y_POS_CODE: placeholdertype = 42;
pub(crate) const PDF_SHELL_ESCAPE_CODE: placeholdertype = 45;
pub(crate) const XETEX_PDF_PAGE_COUNT_CODE: placeholdertype = 46;
pub(crate) const XETEX_GLYPH_BOUNDS_CODE: placeholdertype = 47;
pub(crate) const FONT_CHAR_WD_CODE: placeholdertype = 48;
pub(crate) const FONT_CHAR_HT_CODE: placeholdertype = 49;
pub(crate) const FONT_CHAR_DP_CODE: placeholdertype = 50;
pub(crate) const FONT_CHAR_IC_CODE: placeholdertype = 51;
pub(crate) const PAR_SHAPE_LENGTH_CODE: placeholdertype = 52;
pub(crate) const PAR_SHAPE_INDENT_CODE: placeholdertype = 53;
pub(crate) const PAR_SHAPE_DIMEN_CODE: placeholdertype = 54;
pub(crate) const GLUE_STRETCH_CODE: placeholdertype = 55;
pub(crate) const GLUE_SHRINK_CODE: placeholdertype = 56;
pub(crate) const MU_TO_GLUE_CODE: placeholdertype = 57;
pub(crate) const GLUE_TO_MU_CODE: placeholdertype = 58;
pub(crate) const ETEX_EXPR: placeholdertype = 59;

/* args to CONVERT -- also heavily overloaded */
pub(crate) const NUMBER_CODE: placeholdertype = 0;
pub(crate) const ROMAN_NUMERAL_CODE: placeholdertype = 1;
pub(crate) const STRING_CODE: placeholdertype = 2;
pub(crate) const MEANING_CODE: placeholdertype = 3;
pub(crate) const FONT_NAME_CODE: placeholdertype = 4;
pub(crate) const ETEX_REVISION_CODE: placeholdertype = 5;
pub(crate) const XETEX_REVISION_CODE: placeholdertype = 6;
pub(crate) const XETEX_VARIATION_NAME_CODE: placeholdertype = 7;
pub(crate) const XETEX_FEATURE_NAME_CODE: placeholdertype = 8;
pub(crate) const XETEX_SELECTOR_NAME_CODE: placeholdertype = 9;
pub(crate) const XETEX_GLYPH_NAME_CODE: placeholdertype = 10;
pub(crate) const LEFT_MARGIN_KERN_CODE: placeholdertype = 11;
pub(crate) const RIGHT_MARGIN_KERN_CODE: placeholdertype = 12;
pub(crate) const XETEX_UCHAR_CODE: placeholdertype = 13;
pub(crate) const XETEX_UCHARCAT_CODE: placeholdertype = 14;
pub(crate) const JOB_NAME_CODE: placeholdertype = 15;
pub(crate) const PDF_STRCMP_CODE: placeholdertype = 43;
pub(crate) const PDF_MDFIVE_SUM_CODE: placeholdertype = 44;

/* args to IF_TEST */
pub(crate) const IF_CHAR_CODE: placeholdertype = 0;
pub(crate) const IF_CODE: placeholdertype = 1;
pub(crate) const IF_CAT_CODE: placeholdertype = 1;
pub(crate) const IF_INT_CODE: placeholdertype = 2;
pub(crate) const IF_DIM_CODE: placeholdertype = 3;
pub(crate) const IF_ODD_CODE: placeholdertype = 4;
pub(crate) const IF_VMODE_CODE: placeholdertype = 5;
pub(crate) const IF_HMODE_CODE: placeholdertype = 6;
pub(crate) const IF_MMODE_CODE: placeholdertype = 7;
pub(crate) const IF_INNER_CODE: placeholdertype = 8;
pub(crate) const IF_VOID_CODE: placeholdertype = 9;
pub(crate) const IF_HBOX_CODE: placeholdertype = 10;
pub(crate) const IF_VBOX_CODE: placeholdertype = 11;
pub(crate) const IFX_CODE: placeholdertype = 12;
pub(crate) const IF_EOF_CODE: placeholdertype = 13;
pub(crate) const IF_TRUE_CODE: placeholdertype = 14;
pub(crate) const IF_FALSE_CODE: placeholdertype = 15;
pub(crate) const IF_CASE_CODE: placeholdertype = 16;
pub(crate) const IF_DEF_CODE: placeholdertype = 17;
pub(crate) const IF_CS_CODE: placeholdertype = 18;
pub(crate) const IF_FONT_CHAR_CODE: placeholdertype = 19;
pub(crate) const IF_IN_CSNAME_CODE: placeholdertype = 20;
pub(crate) const IF_PRIMITIVE_CODE: placeholdertype = 21;

/* args to FI_OR_ELSE */
pub(crate) const FI_CODE: placeholdertype = 2;
pub(crate) const ELSE_CODE: placeholdertype = 3;
pub(crate) const OR_CODE: placeholdertype = 4;

/* special args for TAB_MARK, CAR_RET */
pub(crate) const SPAN_CODE: placeholdertype = (BIGGEST_USV + 2);
pub(crate) const CR_CODE: placeholdertype = (BIGGEST_USV + 3);
pub(crate) const CR_CR_CODE: placeholdertype = (BIGGEST_USV + 4);

/* HSKIP, VSKIP, MSKIP */
pub(crate) const FIL_CODE: placeholdertype = 0;
pub(crate) const FILL_CODE: placeholdertype = 1;
pub(crate) const SS_CODE: placeholdertype = 2;
pub(crate) const FIL_NEG_CODE: placeholdertype = 3;
pub(crate) const SKIP_CODE: placeholdertype = 4;
pub(crate) const MSKIP_CODE: placeholdertype = 5;

/* MAKE_BOX, UN_HBOX, UN_VBOX */
pub(crate) const BOX_CODE: placeholdertype = 0;
pub(crate) const COPY_CODE: placeholdertype = 1;
pub(crate) const LAST_BOX_CODE: placeholdertype = 2;
pub(crate) const VSPLIT_CODE: placeholdertype = 3;
pub(crate) const VTOP_CODE: placeholdertype = 4;

/* LEADER_SHIP */
pub(crate) const A_LEADERS: placeholdertype = 100;
pub(crate) const C_LEADERS: placeholdertype = 101;
pub(crate) const X_LEADERS: placeholdertype = 102;

/* LIMIT_SWITCH */
/* also NORMAL = 0 */
pub(crate) const LIMITS: placeholdertype = 1;
pub(crate) const NO_LIMITS: placeholdertype = 2;

/* MATH_STYLE */
pub(crate) const DISPLAY_STYLE: placeholdertype = 0;
pub(crate) const TEXT_STYLE: placeholdertype = 2;
pub(crate) const SCRIPT_STYLE: placeholdertype = 4;
pub(crate) const SCRIPT_SCRIPT_STYLE: placeholdertype = 6;

/* ABOVE */
pub(crate) const ABOVE_CODE: placeholdertype = 0;
pub(crate) const OVER_CODE: placeholdertype = 1;
pub(crate) const ATOP_CODE: placeholdertype = 2;
pub(crate) const DELIMITED_CODE: placeholdertype = 3;

/* SHORTHAND_DEF */
pub(crate) const CHAR_DEF_CODE: placeholdertype = 0;
pub(crate) const MATH_CHAR_DEF_CODE: placeholdertype = 1;
pub(crate) const COUNT_DEF_CODE: placeholdertype = 2;
pub(crate) const DIMEN_DEF_CODE: placeholdertype = 3;
pub(crate) const SKIP_DEF_CODE: placeholdertype = 4;
pub(crate) const MU_SKIP_DEF_CODE: placeholdertype = 5;
pub(crate) const TOKS_DEF_CODE: placeholdertype = 6;
pub(crate) const CHAR_SUB_DEF_CODE: placeholdertype = 7;
pub(crate) const XETEX_MATH_CHAR_NUM_DEF_CODE: placeholdertype = 8;
pub(crate) const XETEX_MATH_CHAR_DEF_CODE: placeholdertype = 9;

/* XRAY */
pub(crate) const SHOW_CODE: placeholdertype = 0;
pub(crate) const SHOW_BOX_CODE: placeholdertype = 1;
pub(crate) const SHOW_THE_CODE: placeholdertype = 2;
pub(crate) const SHOW_LISTS: placeholdertype = 3;
pub(crate) const SHOW_GROUPS: placeholdertype = 4;
pub(crate) const SHOW_TOKENS: placeholdertype = 5;
pub(crate) const SHOW_IFS: placeholdertype = 6;

/* EXTENSION */
pub(crate) const OPEN_NODE: placeholdertype = 0;
pub(crate) const WRITE_NODE: placeholdertype = 1;
pub(crate) const CLOSE_NODE: placeholdertype = 2;
pub(crate) const SPECIAL_NODE: placeholdertype = 3;
pub(crate) const LANGUAGE_NODE: placeholdertype = 4;
pub(crate) const IMMEDIATE_CODE: placeholdertype = 4;
pub(crate) const SET_LANGUAGE_CODE: placeholdertype = 5;
pub(crate) const PDFTEX_FIRST_EXTENSION_CODE: placeholdertype = 6;
pub(crate) const PDF_SAVE_POS_NODE: placeholdertype = 6;
/// not to be confused with PIC_NODE = 43!
pub(crate) const PIC_FILE_CODE: placeholdertype = 41;
/// not to be confused with PDF_NODE = 44!
pub(crate) const PDF_FILE_CODE: placeholdertype = 42;
/// not to be confused with GLYPH_NODE = 42!
pub(crate) const GLYPH_CODE: placeholdertype = 43;
pub(crate) const XETEX_INPUT_ENCODING_EXTENSION_CODE: placeholdertype = 44;
pub(crate) const XETEX_DEFAULT_ENCODING_EXTENSION_CODE: placeholdertype = 45;
pub(crate) const XETEX_LINEBREAK_LOCALE_EXTENSION_CODE: placeholdertype = 46;

/* VALIGN overloads */
pub(crate) const BEGIN_L_CODE: placeholdertype = 6;
pub(crate) const END_L_CODE: placeholdertype = 7;
pub(crate) const BEGIN_R_CODE: placeholdertype = 10;
pub(crate) const END_R_CODE: placeholdertype = 11;

/* begin_token_list() types */
pub(crate) const PARAMETER: u16 = 0;
pub(crate) const U_TEMPLATE: u16 = 1;
pub(crate) const V_TEMPLATE: u16 = 2;
pub(crate) const BACKED_UP: u16 = 3;
pub(crate) const BACKED_UP_CHAR: u16 = 4;
pub(crate) const INSERTED: u16 = 5;
pub(crate) const MACRO: u16 = 6;
pub(crate) const OUTPUT_TEXT: u16 = 7;
pub(crate) const EVERY_PAR_TEXT: u16 = 8;
pub(crate) const EVERY_MATH_TEXT: u16 = 9;
pub(crate) const EVERY_DISPLAY_TEXT: u16 = 10;
pub(crate) const EVERY_HBOX_TEXT: u16 = 11;
pub(crate) const EVERY_VBOX_TEXT: u16 = 12;
pub(crate) const EVERY_JOB_TEXT: u16 = 13;
pub(crate) const EVERY_CR_TEXT: u16 = 14;
pub(crate) const MARK_TEXT: u16 = 15;
pub(crate) const EVERY_EOF_TEXT: u16 = 16;
pub(crate) const INTER_CHAR_TEXT: u16 = 17;
pub(crate) const WRITE_TEXT: u16 = 18;
pub(crate) const TECTONIC_CODA_TEXT: u16 = 19;

/* input state */
pub(crate) const MID_LINE: placeholdertype = 1;
pub(crate) const SKIP_BLANKS: placeholdertype = 17;
pub(crate) const NEW_LINE: placeholdertype = 33;

/* DVI format codes */
pub(crate) const XDV_ID_BYTE: placeholdertype = 7;
pub(crate) const SPX_ID_BYTE: placeholdertype = 100;

/* page_contents possibilities (EMPTY is overloaded) */
pub(crate) const EMPTY: placeholdertype = 0;
pub(crate) const INSERTS_ONLY: placeholdertype = 1;
pub(crate) const BOX_THERE: placeholdertype = 2;

pub(crate) const SET1: placeholdertype = 128;
pub(crate) const SET_RULE: placeholdertype = 132;
pub(crate) const PUT_RULE: placeholdertype = 137;
pub(crate) const BOP: placeholdertype = 139;
pub(crate) const EOP: placeholdertype = 140;
pub(crate) const PUSH: placeholdertype = 141;
pub(crate) const POP: placeholdertype = 142;
pub(crate) const RIGHT1: placeholdertype = 143;
pub(crate) const DOWN1: placeholdertype = 157;
pub(crate) const FNT1: placeholdertype = 235;
pub(crate) const XXX1: placeholdertype = 239;
pub(crate) const XXX4: placeholdertype = 242;
pub(crate) const FNT_DEF1: placeholdertype = 243;
pub(crate) const PRE: placeholdertype = 247;
pub(crate) const POST: placeholdertype = 248;
pub(crate) const POST_POST: placeholdertype = 249;
pub(crate) const DEFINE_NATIVE_FONT: placeholdertype = 252;
pub(crate) const SET_GLYPHS: placeholdertype = 253;
pub(crate) const SET_TEXT_AND_GLYPHS: placeholdertype = 254;

pub(crate) const XETEX_INPUT_MODE_AUTO: placeholdertype = 0;
pub(crate) const XETEX_VERSION: placeholdertype = 0;
pub(crate) const EXACTLY: placeholdertype = 0;
pub(crate) const FONT_BASE: placeholdertype = 0;
pub(crate) const INSERTING: placeholdertype = 0;
pub(crate) const NON_ADDRESS: placeholdertype = 0;
pub(crate) const RESTORE_OLD_VALUE: placeholdertype = 0;
pub(crate) const TOKEN_LIST: placeholdertype = 0;
pub(crate) const UNDEFINED_PRIMITIVE: placeholdertype = 0;
pub(crate) const UNHYPHENATED: placeholdertype = 0;
pub(crate) const ADDITIONAL: placeholdertype = 1;
pub(crate) const EXPLICIT: placeholdertype = 1;
pub(crate) const FIXED_ACC: placeholdertype = 1;
pub(crate) const HYPHENATED: placeholdertype = 1;
pub(crate) const JUST_OPEN: placeholdertype = 1;
pub(crate) const MATH_CHAR: placeholdertype = 1;
pub(crate) const PRIM_BASE: placeholdertype = 1;
pub(crate) const RESTORE_ZERO: placeholdertype = 1;
pub(crate) const REVERSED: placeholdertype = 1;
pub(crate) const SLANT_CODE: placeholdertype = 1;
pub(crate) const SPLIT_UP: placeholdertype = 1;
pub(crate) const STRETCHING: placeholdertype = 1;
pub(crate) const VMODE: placeholdertype = 1;
pub(crate) const ACC_KERN: placeholdertype = 2;
pub(crate) const BOTTOM_ACC: placeholdertype = 2;
pub(crate) const CLOSED: placeholdertype = 2;
pub(crate) const DLIST: placeholdertype = 2;
pub(crate) const ETEX_VERSION: placeholdertype = 2;
pub(crate) const INSERT_TOKEN: placeholdertype = 2;
pub(crate) const SHRINKING: placeholdertype = 2;
pub(crate) const SPACE_CODE: placeholdertype = 2;
pub(crate) const SUB_BOX: placeholdertype = 2;
pub(crate) const DISPLAYOPERATORMINHEIGHT: placeholdertype = 3;
pub(crate) const LEVEL_BOUNDARY: placeholdertype = 3;
// pub(crate) const MATH_SHIFT: placeholdertype = 3;
pub(crate) const SPACE_ADJUSTMENT: placeholdertype = 3;
pub(crate) const SUB_MLIST: placeholdertype = 3;
pub(crate) const IDENT_VAL: placeholdertype = 4;
pub(crate) const MATH_TEXT_CHAR: placeholdertype = 4;
pub(crate) const RESTORE_SA: placeholdertype = 4;
pub(crate) const SPACE_SHRINK_CODE: placeholdertype = 4;
// pub(crate) const OUT_PARAM: placeholdertype = 5;
pub(crate) const TOK_VAL: placeholdertype = 5;
pub(crate) const X_HEIGHT_CODE: placeholdertype = 5;
pub(crate) const ACCENTBASEHEIGHT: placeholdertype = 6;
pub(crate) const INTER_CHAR_VAL: placeholdertype = 6;
// pub(crate) const MAC_PARAM: placeholdertype = 6;
pub(crate) const QUAD_CODE: placeholdertype = 6;
pub(crate) const EXTRA_SPACE_CODE: placeholdertype = 7;
pub(crate) const MARK_VAL: placeholdertype = 7;
// pub(crate) const SUP_MARK: placeholdertype = 7;
pub(crate) const VAR_FAM_CLASS: placeholdertype = 7;
// pub(crate) const IGNORE: placeholdertype = 9;
pub(crate) const SUBSCRIPTTOPMAX: placeholdertype = 9;
pub(crate) const NATIVE_GLYPH_INFO_SIZE: placeholdertype = 10;
// pub(crate) const ACTIVE_CHAR: placeholdertype = 13;
pub(crate) const CARRIAGE_RETURN: placeholdertype = 13;
pub(crate) const SUPERSCRIPTBOTTOMMIN: placeholdertype = 13;
pub(crate) const TOTAL_MATHEX_PARAMS: placeholdertype = 13;
// pub(crate) const COMMENT: placeholdertype = 14;
pub(crate) const HI_MEM_STAT_USAGE: placeholdertype = 15;
// pub(crate) const INVALID_CHAR: placeholdertype = 15;
pub(crate) const MAX_CHAR_CODE: placeholdertype = 15;
pub(crate) const SUBSUPERSCRIPTGAPMIN: placeholdertype = 15;
pub(crate) const SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT: placeholdertype = 16;
pub(crate) const TOTAL_MATHSY_PARAMS: placeholdertype = 22;
pub(crate) const STACKGAPMIN: placeholdertype = 26;
pub(crate) const STACKDISPLAYSTYLEGAPMIN: placeholdertype = 27;
pub(crate) const UNLESS_CODE: placeholdertype = 32;
// pub(crate) const VRULE: placeholdertype = 35;
pub(crate) const FRACTIONNUMERATORGAPMIN: placeholdertype = 36;
pub(crate) const FRACTIONNUMDISPLAYSTYLEGAPMIN: placeholdertype = 37;
// pub(crate) const XETEX_FIRST_CHAR_CODE: placeholdertype = 39;
pub(crate) const FRACTIONDENOMINATORGAPMIN: placeholdertype = 39;
pub(crate) const FRACTIONDENOMDISPLAYSTYLEGAPMIN: placeholdertype = 40;
pub(crate) const XETEX_DIM: placeholdertype = 47;
pub(crate) const RADICALVERTICALGAP: placeholdertype = 49;
pub(crate) const RADICALDISPLAYSTYLEVERTICALGAP: placeholdertype = 50;
pub(crate) const RADICALRULETHICKNESS: placeholdertype = 51;
pub(crate) const ETEX_GLUE: placeholdertype = 57;
pub(crate) const ETEX_MU: placeholdertype = 58;
pub(crate) const COND_MATH_GLUE: placeholdertype = 98;
pub(crate) const MU_GLUE: placeholdertype = 99;
pub(crate) const MAX_COMMAND: placeholdertype = 102;
pub(crate) const UNDEFINED_CS: placeholdertype = 103;
pub(crate) const HMODE: placeholdertype = 104;
pub(crate) const CALL: placeholdertype = 113;
pub(crate) const LONG_CALL: placeholdertype = 114;
pub(crate) const OUTER_CALL: placeholdertype = 115;
pub(crate) const LONG_OUTER_CALL: placeholdertype = 116;
pub(crate) const END_TEMPLATE: placeholdertype = 117;
pub(crate) const DONT_EXPAND: placeholdertype = 118;
pub(crate) const GLUE_REF: placeholdertype = 119;
pub(crate) const SHAPE_REF: placeholdertype = 120;
pub(crate) const BOX_REF: placeholdertype = 121;
pub(crate) const DATA: placeholdertype = 122;
pub(crate) const DIMEN_VAL_LIMIT: placeholdertype = 128;
pub(crate) const MMODE: placeholdertype = 207;
pub(crate) const BIGGEST_LANG: placeholdertype = 255;
pub(crate) const MU_VAL_LIMIT: placeholdertype = 256;
pub(crate) const TOO_BIG_LANG: placeholdertype = 256;
pub(crate) const BOX_VAL_LIMIT: placeholdertype = 320;
pub(crate) const TOK_VAL_LIMIT: placeholdertype = 384;
pub(crate) const PRIM_PRIME: placeholdertype = 431;
pub(crate) const PRIM_SIZE: placeholdertype = 500;
pub(crate) const MAX_HLIST_STACK: placeholdertype = 512;
pub(crate) const HYPH_PRIME: placeholdertype = 607;
pub(crate) const HYPHENATABLE_LENGTH_LIMIT: placeholdertype = 4095;
pub(crate) const CHAR_CLASS_LIMIT: placeholdertype = 4096;
pub(crate) const EJECT_PENALTY: placeholdertype = -10000;
pub(crate) const INF_BAD: placeholdertype = 10000;
pub(crate) const INF_PENALTY: placeholdertype = 10000;
pub(crate) const DEFAULT_RULE: placeholdertype = 26214;
pub(crate) const TOO_BIG_CHAR: placeholdertype = 65536;
pub(crate) const NO_EXPAND_FLAG: placeholdertype = (BIGGEST_USV + 2);

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
pub(crate) const PROTECTED_TOKEN: placeholdertype = (END_MATCH_TOKEN + 1);

pub(crate) const A_TOKEN: placeholdertype = (LETTER_TOKEN + 'A' as placeholdertype);
pub(crate) const OTHER_A_TOKEN: placeholdertype = (OTHER_TOKEN + 'A' as placeholdertype);
pub(crate) const HEX_TOKEN: placeholdertype = (OTHER_TOKEN + '"' as placeholdertype);
pub(crate) const OCTAL_TOKEN: placeholdertype = (OTHER_TOKEN + '\'' as placeholdertype);
pub(crate) const CONTINENTAL_POINT_TOKEN: placeholdertype = (OTHER_TOKEN + ',' as placeholdertype);
pub(crate) const POINT_TOKEN: placeholdertype = (OTHER_TOKEN + '.' as placeholdertype);
pub(crate) const ZERO_TOKEN: placeholdertype = (OTHER_TOKEN + '0' as placeholdertype);
pub(crate) const ALPHA_TOKEN: placeholdertype = (OTHER_TOKEN + '`' as placeholdertype);

pub(crate) const BOX_FLAG: placeholdertype = 0x40000000;
pub(crate) const GLOBAL_BOX_FLAG: placeholdertype = 0x40008000;
pub(crate) const SHIP_OUT_FLAG: placeholdertype = 0x40010000;
pub(crate) const LEADER_FLAG: placeholdertype = 0x40010001;

pub(crate) const LP_CODE_BASE: placeholdertype = 2;
pub(crate) const RP_CODE_BASE: placeholdertype = 3;

pub(crate) const LEFT_SIDE: placeholdertype = 0;
pub(crate) const RIGHT_SIDE: placeholdertype = 1;

/* modes to do_marks() */
pub(crate) const VSPLIT_INIT: placeholdertype = 0;
pub(crate) const FIRE_UP_INIT: placeholdertype = 1;
pub(crate) const FIRE_UP_DONE: placeholdertype = 2;
pub(crate) const DESTROY_MARKS: placeholdertype = 3;

pub(crate) const MARKS_CODE: placeholdertype = 5;

pub(crate) const IGNORE_DEPTH: placeholdertype = -65536000;

pub(crate) const MIDDLE_NOAD: placeholdertype = 1;

/* movement() */
pub(crate) const MOV_NONE_SEEN: placeholdertype = 0;
pub(crate) const MOV_Y_HERE: placeholdertype = 1;
pub(crate) const MOV_Z_HERE: placeholdertype = 2;
pub(crate) const MOV_YZ_OK: placeholdertype = 3;
pub(crate) const MOV_Y_OK: placeholdertype = 4;
pub(crate) const MOV_Z_OK: placeholdertype = 5;
pub(crate) const MOV_Y_SEEN: placeholdertype = 6;
pub(crate) const MOV_D_FIXED: placeholdertype = 6;
pub(crate) const MOV_Z_SEEN: placeholdertype = 12;

/* Increase this whenever the engine internals change such that the contents
 * of the "format" files must be regenerated -- this includes changes to the
 * string pool. KEEP SYNCHRONIZED WITH src/lib.rs!!! */

pub(crate) const FORMAT_SERIAL: placeholdertype = 28;
