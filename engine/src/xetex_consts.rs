use crate::xetex_ini::eqtb;

pub type placeholdertype = i32;
pub const MIN_HALFWORD: placeholdertype = -0x0FFFFFFF;
pub const MAX_HALFWORD: placeholdertype = 0x3FFFFFFF;

/// a null "pointer"
pub const TEX_NULL: placeholdertype = MIN_HALFWORD;
/// "the largest positive value that TeX knows"
pub const TEX_INFINITY: placeholdertype = 0x7FFFFFFF;
/// "signifies a missing item" in rule nodes */
pub const NULL_FLAG: placeholdertype = -0x40000000;
/// "denotes default_rule_thickness"
pub const DEFAULT_CODE: placeholdertype = 0x40000000;

/* characters
 *
 * TeX thinks there are only 256 character but we know better. We use UTF16
 * codepoints. Actual Unicode character codes can exceed this, up to
 * BIGGEST_USV. "USV" here means Unicode Scalar Value. */

/// must be <= max_quarterword
pub const BIGGEST_CHAR: placeholdertype = 0xFFFF;
pub const BIGGEST_USV: placeholdertype = 0x10FFFF;
pub const NUMBER_USVS: placeholdertype = (BIGGEST_USV + 1);
pub const TOO_BIG_USV: placeholdertype = (BIGGEST_USV + 1);

/* Various buffer sizes */

/// max number of control sequences
pub const HASH_SIZE: placeholdertype = 15000;
/// "a prime number equal to about 85% of hash_size
pub const HASH_PRIME: placeholdertype = 8501;

pub const MAX_FONT_MAX: placeholdertype = 9000;

pub const NUMBER_MATH_FAMILIES: placeholdertype = 256;
pub const TEXT_SIZE: placeholdertype = 0;
pub const SCRIPT_SIZE: placeholdertype = NUMBER_MATH_FAMILIES;
pub const SCRIPT_SCRIPT_SIZE: placeholdertype = (2 * NUMBER_MATH_FAMILIES);
pub const NUMBER_MATH_FONTS: placeholdertype = (3 * NUMBER_MATH_FAMILIES);

pub const NUMBER_REGS: placeholdertype = 256;

/// the size of our main "mem" array, minus 1; classically this is
/// configurable, but we hardcode it.
pub const MEM_TOP: placeholdertype = 4999999;

/* fixed locations in the "mem" array */
pub const PAGE_INS_HEAD: placeholdertype = MEM_TOP;
pub const CONTRIB_HEAD: placeholdertype = (MEM_TOP - 1);
pub const PAGE_HEAD: placeholdertype = (MEM_TOP - 2);
pub const TEMP_HEAD: placeholdertype = (MEM_TOP - 3);
pub const HOLD_HEAD: placeholdertype = (MEM_TOP - 4);
pub const ADJUST_HEAD: placeholdertype = (MEM_TOP - 5);
/// note: two words
pub const ACTIVE_LIST: placeholdertype = (MEM_TOP - 7);
pub const ALIGN_HEAD: placeholdertype = (MEM_TOP - 8);
pub const END_SPAN: placeholdertype = (MEM_TOP - 9);
pub const OMIT_TEMPLATE: placeholdertype = (MEM_TOP - 10);
pub const NULL_LIST: placeholdertype = (MEM_TOP - 11);
pub const LIG_TRICK: placeholdertype = (MEM_TOP - 12);
/// note: same as LIG_TRICK
pub const GARBAGE: placeholdertype = (MEM_TOP - 12);
pub const BACKUP_HEAD: placeholdertype = (MEM_TOP - 13);
pub const PRE_ADJUST_HEAD: placeholdertype = (MEM_TOP - 14);

/* equivalents table offsets */

/// "region 1": active character equivalents
pub const ACTIVE_BASE: placeholdertype = 1;
pub const SINGLE_BASE: placeholdertype = (ACTIVE_BASE + NUMBER_USVS);
pub const NULL_CS: placeholdertype = (SINGLE_BASE + NUMBER_USVS);
/// "region 2": hash table
pub const HASH_BASE: placeholdertype = (NULL_CS + 1);
pub const FROZEN_CONTROL_SEQUENCE: placeholdertype = (HASH_BASE + HASH_SIZE);
pub const FROZEN_PROTECTION: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 0);
pub const FROZEN_CR: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 1);
pub const FROZEN_END_GROUP: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 2);
pub const FROZEN_RIGHT: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 3);
pub const FROZEN_FI: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 4);
pub const FROZEN_END_TEMPLATE: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 5);
pub const FROZEN_ENDV: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 6);
pub const FROZEN_RELAX: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 7);
pub const END_WRITE: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 8);
pub const FROZEN_DONT_EXPAND: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 9);
pub const FROZEN_SPECIAL: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 10);
pub const FROZEN_PRIMITIVE: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 11);
pub const FROZEN_NULL_FONT: placeholdertype = (FROZEN_CONTROL_SEQUENCE + 12);
/// nominally minus FONT_BASE, but that's 0
pub const FONT_ID_BASE: placeholdertype = FROZEN_NULL_FONT;
pub const UNDEFINED_CONTROL_SEQUENCE: placeholdertype = (FROZEN_NULL_FONT + MAX_FONT_MAX + 1);

/// "region 3": glue values
pub const GLUE_BASE: placeholdertype = (UNDEFINED_CONTROL_SEQUENCE + 1);

pub const GLUE_PAR__line_skip: placeholdertype = 0;
pub const GLUE_PAR__baseline_skip: placeholdertype = 1;
pub const GLUE_PAR__par_skip: placeholdertype = 2;
pub const GLUE_PAR__above_display_skip: placeholdertype = 3;
pub const GLUE_PAR__below_display_skip: placeholdertype = 4;
pub const GLUE_PAR__above_display_short_skip: placeholdertype = 5;
pub const GLUE_PAR__below_display_short_skip: placeholdertype = 6;
pub const GLUE_PAR__left_skip: placeholdertype = 7;
pub const GLUE_PAR__right_skip: placeholdertype = 8;
pub const GLUE_PAR__top_skip: placeholdertype = 9;
pub const GLUE_PAR__split_top_skip: placeholdertype = 10;
pub const GLUE_PAR__tab_skip: placeholdertype = 11;
pub const GLUE_PAR__space_skip: placeholdertype = 12;
pub const GLUE_PAR__xspace_skip: placeholdertype = 13;
pub const GLUE_PAR__par_fill_skip: placeholdertype = 14;
pub const GLUE_PAR__xetex_linebreak_skip: placeholdertype = 15;
pub const GLUE_PAR__thin_mu_skip: placeholdertype = 16;
pub const GLUE_PAR__med_mu_skip: placeholdertype = 17;
pub const GLUE_PAR__thick_mu_skip: placeholdertype = 18;
pub const GLUE_PARS: placeholdertype = 19;

pub unsafe fn GLUEPAR(s: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((GLUE_BASE + s) as isize))).b32.s1
}

pub const SKIP_BASE: placeholdertype = (GLUE_BASE + GLUE_PARS);
pub unsafe fn SKIP_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((SKIP_BASE + n) as isize))).b32.s1
}

pub const MU_SKIP_BASE: placeholdertype = (SKIP_BASE + NUMBER_REGS);
pub unsafe fn MU_SKIP_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((MU_SKIP_BASE + (n)) as isize))).b32.s1
}

/* "region 4": local halfword values like baselineskip. Some of these are
 * used as arguments to ASSIGN_TOKS, SET_SHAPE, etc. */

pub const LOCAL_BASE: placeholdertype = (MU_SKIP_BASE + NUMBER_REGS);

pub const LOCAL__par_shape: placeholdertype = 0;
pub const LOCAL__output_routine: placeholdertype = 1;
pub const LOCAL__every_par: placeholdertype = 2;
pub const LOCAL__every_math: placeholdertype = 3;
pub const LOCAL__every_display: placeholdertype = 4;
pub const LOCAL__every_hbox: placeholdertype = 5;
pub const LOCAL__every_vbox: placeholdertype = 6;
pub const LOCAL__every_job: placeholdertype = 7;
pub const LOCAL__every_cr: placeholdertype = 8;
pub const LOCAL__err_help: placeholdertype = 9;
pub const LOCAL__every_eof: placeholdertype = 10;
pub const LOCAL__xetex_inter_char: placeholdertype = 11;
pub const LOCAL__TectonicCodaTokens: placeholdertype = 12;
pub const NUM_LOCALS: placeholdertype = 13;

pub unsafe fn LOCAL(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((LOCAL_BASE + n) as isize))).b32.s1
}

pub const TOKS_BASE: placeholdertype = (LOCAL_BASE + NUM_LOCALS);
pub unsafe fn TOKS_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((TOKS_BASE + n) as isize))).b32.s1
}

pub const ETEX_PEN_BASE: placeholdertype = (TOKS_BASE + NUMBER_REGS);
pub const INTER_LINE_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 0);
pub const CLUB_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 1);
pub const WIDOW_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 2);
pub const DISPLAY_WIDOW_PENALTIES_LOC: placeholdertype = (ETEX_PEN_BASE + 3);
pub const ETEX_PENS: placeholdertype = (ETEX_PEN_BASE + 4);

pub const BOX_BASE: placeholdertype = ETEX_PENS;
pub unsafe fn BOX_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((BOX_BASE + n) as isize))).b32.s1
}

pub const CUR_FONT_LOC: placeholdertype = (BOX_BASE + NUMBER_REGS);
pub const MATH_FONT_BASE: placeholdertype = (CUR_FONT_LOC + 1);

pub unsafe fn MATH_FONT(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((MATH_FONT_BASE + n) as isize))).b32.s1
}

pub const CAT_CODE_BASE: placeholdertype = (MATH_FONT_BASE + NUMBER_MATH_FONTS);
pub unsafe fn CAT_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((CAT_CODE_BASE + n) as isize))).b32.s1
}

pub const LC_CODE_BASE: placeholdertype = (CAT_CODE_BASE + NUMBER_USVS);
pub unsafe fn LC_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((LC_CODE_BASE + n) as isize))).b32.s1
}

pub const UC_CODE_BASE: placeholdertype = (LC_CODE_BASE + NUMBER_USVS);
pub unsafe fn UC_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((UC_CODE_BASE + n) as isize))).b32.s1
}

pub const SF_CODE_BASE: placeholdertype = (UC_CODE_BASE + NUMBER_USVS);
pub unsafe fn SF_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((SF_CODE_BASE + n) as isize))).b32.s1
}

pub const MATH_CODE_BASE: placeholdertype = (SF_CODE_BASE + NUMBER_USVS);
pub unsafe fn MATH_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((MATH_CODE_BASE + n) as isize))).b32.s1
}

pub const CHAR_SUB_CODE_BASE: placeholdertype = (MATH_CODE_BASE + NUMBER_USVS);
pub unsafe fn CHAR_SUB_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((CHAR_SUB_CODE_BASE + n) as isize))).b32.s1
}

/* "region 5": current fullword integers like hyphenation penalty */

pub const INT_BASE: placeholdertype = (CHAR_SUB_CODE_BASE + NUMBER_USVS);
pub const INT_PAR__pretolerance: placeholdertype = 0;
pub const INT_PAR__tolerance: placeholdertype = 1;
pub const INT_PAR__line_penalty: placeholdertype = 2;
pub const INT_PAR__hyphen_penalty: placeholdertype = 3;
pub const INT_PAR__ex_hyphen_penalty: placeholdertype = 4;
pub const INT_PAR__club_penalty: placeholdertype = 5;
pub const INT_PAR__widow_penalty: placeholdertype = 6;
pub const INT_PAR__display_widow_penalty: placeholdertype = 7;
pub const INT_PAR__broken_penalty: placeholdertype = 8;
pub const INT_PAR__bin_op_penalty: placeholdertype = 9;
pub const INT_PAR__rel_penalty: placeholdertype = 10;
pub const INT_PAR__pre_display_penalty: placeholdertype = 11;
pub const INT_PAR__post_display_penalty: placeholdertype = 12;
pub const INT_PAR__inter_line_penalty: placeholdertype = 13;
pub const INT_PAR__double_hyphen_demerits: placeholdertype = 14;
pub const INT_PAR__final_hyphen_demerits: placeholdertype = 15;
pub const INT_PAR__adj_demerits: placeholdertype = 16;
pub const INT_PAR__mag: placeholdertype = 17;
pub const INT_PAR__delimiter_factor: placeholdertype = 18;
pub const INT_PAR__looseness: placeholdertype = 19;
pub const INT_PAR__time: placeholdertype = 20;
pub const INT_PAR__day: placeholdertype = 21;
pub const INT_PAR__month: placeholdertype = 22;
pub const INT_PAR__year: placeholdertype = 23;
pub const INT_PAR__show_box_breadth: placeholdertype = 24;
pub const INT_PAR__show_box_depth: placeholdertype = 25;
pub const INT_PAR__hbadness: placeholdertype = 26;
pub const INT_PAR__vbadness: placeholdertype = 27;
pub const INT_PAR__pausing: placeholdertype = 28;
pub const INT_PAR__tracing_online: placeholdertype = 29;
pub const INT_PAR__tracing_macros: placeholdertype = 30;
pub const INT_PAR__tracing_stats: placeholdertype = 31;
pub const INT_PAR__tracing_paragraphs: placeholdertype = 32;
pub const INT_PAR__tracing_pages: placeholdertype = 33;
pub const INT_PAR__tracing_output: placeholdertype = 34;
pub const INT_PAR__tracing_lost_chars: placeholdertype = 35;
pub const INT_PAR__tracing_commands: placeholdertype = 36;
pub const INT_PAR__tracing_restores: placeholdertype = 37;
pub const INT_PAR__uc_hyph: placeholdertype = 38;
pub const INT_PAR__output_penalty: placeholdertype = 39;
pub const INT_PAR__max_dead_cycles: placeholdertype = 40;
pub const INT_PAR__hang_after: placeholdertype = 41;
pub const INT_PAR__floating_penalty: placeholdertype = 42;
pub const INT_PAR__global_defs: placeholdertype = 43;
pub const INT_PAR__cur_fam: placeholdertype = 44;
pub const INT_PAR__escape_char: placeholdertype = 45;
pub const INT_PAR__default_hyphen_char: placeholdertype = 46;
pub const INT_PAR__default_skew_char: placeholdertype = 47;
pub const INT_PAR__end_line_char: placeholdertype = 48;
pub const INT_PAR__new_line_char: placeholdertype = 49;
pub const INT_PAR__language: placeholdertype = 50;
pub const INT_PAR__left_hyphen_min: placeholdertype = 51;
pub const INT_PAR__right_hyphen_min: placeholdertype = 52;
pub const INT_PAR__holding_inserts: placeholdertype = 53;
pub const INT_PAR__error_context_lines: placeholdertype = 54;
/// TEX_INT_PARS = WEB2C_INT_BASE
pub const INT_PAR__char_sub_def_min: placeholdertype = 55;
pub const INT_PAR__char_sub_def_max: placeholdertype = 56;
pub const INT_PAR__tracing_char_sub_def: placeholdertype = 57;
/// = WEB2C_INT_PARS = ETEX_INT_BASE
pub const INT_PAR__tracing_assigns: placeholdertype = 58;
pub const INT_PAR__tracing_groups: placeholdertype = 59;
pub const INT_PAR__tracing_ifs: placeholdertype = 60;
pub const INT_PAR__tracing_scan_tokens: placeholdertype = 61;
pub const INT_PAR__tracing_nesting: placeholdertype = 62;
pub const INT_PAR__pre_display_correction: placeholdertype = 63;
pub const INT_PAR__last_line_fit: placeholdertype = 64;
pub const INT_PAR__saving_vdiscards: placeholdertype = 65;
pub const INT_PAR__saving_hyphs: placeholdertype = 66;
pub const INT_PAR__suppress_fontnotfound_error: placeholdertype = 67;
pub const INT_PAR__xetex_linebreak_locale: placeholdertype = 68;
pub const INT_PAR__xetex_linebreak_penalty: placeholdertype = 69;
pub const INT_PAR__xetex_protrude_chars: placeholdertype = 70;
pub const INT_PAR__texxet: placeholdertype = 71;
pub const INT_PAR__xetex_dash_break: placeholdertype = 72;
pub const INT_PAR__xetex_upwards: placeholdertype = 73;
pub const INT_PAR__xetex_use_glyph_metrics: placeholdertype = 74;
pub const INT_PAR__xetex_inter_char_tokens: placeholdertype = 75;
pub const INT_PAR__xetex_input_normalization: placeholdertype = 76;
pub const INT_PAR__xetex_default_input_mode: placeholdertype = 77;
pub const INT_PAR__xetex_default_input_encoding: placeholdertype = 78;
pub const INT_PAR__xetex_tracing_fonts: placeholdertype = 79;
pub const INT_PAR__xetex_interword_space_shaping: placeholdertype = 80;
pub const INT_PAR__xetex_generate_actual_text: placeholdertype = 81;
pub const INT_PAR__xetex_hyphenatable_length: placeholdertype = 82;
pub const INT_PAR__synctex: placeholdertype = 83;
pub const INT_PAR__pdfoutput: placeholdertype = 84;
pub const INT_PARS: placeholdertype = 85;

pub unsafe fn INTPAR(x: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((INT_BASE + x) as isize))).b32.s1
}

pub const COUNT_BASE: placeholdertype = (INT_BASE + INT_PARS);
pub unsafe fn COUNT_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((COUNT_BASE + n) as isize))).b32.s1
}

pub const DEL_CODE_BASE: placeholdertype = (COUNT_BASE + NUMBER_REGS);
pub unsafe fn DEL_CODE(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((DEL_CODE_BASE + n) as isize))).b32.s1
}

/* "region 6": current fullword dimensions like hsize */

pub const DIMEN_BASE: placeholdertype = (DEL_CODE_BASE + NUMBER_USVS);
pub const DIMEN_PAR__par_indent: placeholdertype = 0;
pub const DIMEN_PAR__math_surround: placeholdertype = 1;
pub const DIMEN_PAR__line_skip_limit: placeholdertype = 2;
pub const DIMEN_PAR__hsize: placeholdertype = 3;
pub const DIMEN_PAR__vsize: placeholdertype = 4;
pub const DIMEN_PAR__max_depth: placeholdertype = 5;
pub const DIMEN_PAR__split_max_depth: placeholdertype = 6;
pub const DIMEN_PAR__box_max_depth: placeholdertype = 7;
pub const DIMEN_PAR__hfuzz: placeholdertype = 8;
pub const DIMEN_PAR__vfuzz: placeholdertype = 9;
pub const DIMEN_PAR__delimiter_shortfall: placeholdertype = 10;
pub const DIMEN_PAR__null_delimiter_space: placeholdertype = 11;
pub const DIMEN_PAR__script_space: placeholdertype = 12;
pub const DIMEN_PAR__pre_display_size: placeholdertype = 13;
pub const DIMEN_PAR__display_width: placeholdertype = 14;
pub const DIMEN_PAR__display_indent: placeholdertype = 15;
pub const DIMEN_PAR__overfull_rule: placeholdertype = 16;
pub const DIMEN_PAR__hang_indent: placeholdertype = 17;
pub const DIMEN_PAR__h_offset: placeholdertype = 18;
pub const DIMEN_PAR__v_offset: placeholdertype = 19;
pub const DIMEN_PAR__emergency_stretch: placeholdertype = 20;
pub const DIMEN_PAR__pdf_page_width: placeholdertype = 21;
pub const DIMEN_PAR__pdf_page_height: placeholdertype = 22;
pub const DIMEN_PARS: placeholdertype = 23;

pub unsafe fn DIMENPAR(x: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((DIMEN_BASE + x as placeholdertype) as isize)))
        .b32
        .s1
}

pub const SCALED_BASE: placeholdertype = (DIMEN_BASE + DIMEN_PARS);
pub unsafe fn SCALED_REG(n: placeholdertype) -> placeholdertype {
    (*(eqtb.offset((SCALED_BASE + n) as isize))).b32.s1
}

pub const EQTB_SIZE: placeholdertype = (SCALED_BASE + NUMBER_REGS - 1);

/// "really" MIN_QUARTERWORD
pub const LEVEL_ZERO: placeholdertype = 0;
pub const LEVEL_ONE: placeholdertype = 1;

/* SET_INTERACTION */
pub const BATCH_MODE: placeholdertype = 0;
pub const NONSTOP_MODE: placeholdertype = 1;
pub const SCROLL_MODE: placeholdertype = 2;
pub const ERROR_STOP_MODE: placeholdertype = 3;
pub const UNSPECIFIED_MODE: placeholdertype = 4;

pub const LEFT_TO_RIGHT: placeholdertype = 0;
pub const RIGHT_TO_LEFT: placeholdertype = 1;

/* How many memory words are needed for storing synctex information on various
 * kinds of nodes. This extra size is already included in the *_NODE_SIZE
 * definitions below.
 */
pub const SYNCTEX_FIELD_SIZE: placeholdertype = 1;

pub const HLIST_NODE: u16 = 0;
pub const VLIST_NODE: u16 = 1;
pub const DELTA_NODE: u16 = 2;
pub const RULE_NODE: u16 = 2;
pub const INS_NODE: u16 = 3;
pub const MARK_NODE: u16 = 4;
pub const ADJUST_NODE: u16 = 5;
pub const LIGATURE_NODE: u16 = 6;
pub const DISC_NODE: u16 = 7;
pub const WHATSIT_NODE: u16 = 8;
pub const MATH_NODE: u16 = 9;
pub const GLUE_NODE: u16 = 10;
pub const KERN_NODE: u16 = 11;
pub const PENALTY_NODE: u16 = 12;
pub const UNSET_NODE: u16 = 13;
pub const EDGE_NODE: u16 = 14;
pub const STYLE_NODE: u16 = 14;
pub const CHOICE_NODE: u16 = 15;
pub const MARGIN_KERN_NODE: u16 = 40;
pub const NATIVE_WORD_NODE: u16 = 40;
pub const NATIVE_WORD_NODE_AT: u16 = 41;
/// not to be confused with GLYPH_CODE = 43!
pub const GLYPH_NODE: u16 = 42;
/// not to be confused with PIC_FILE_CODE = 41!
pub const PIC_NODE: u16 = 43;
/// not to be confused with PDF_FILE_CODE = 42!
pub const PDF_NODE: u16 = 44;

pub const IF_NODE_SIZE: placeholdertype = 2;
pub const PASSIVE_NODE_SIZE: placeholdertype = 2;
pub const POINTER_NODE_SIZE: placeholdertype = 2;
pub const SMALL_NODE_SIZE: placeholdertype = 2;
pub const SPAN_NODE_SIZE: placeholdertype = 2;
pub const WRITE_NODE_SIZE: placeholdertype = 2;
pub const ACTIVE_NODE_SIZE_NORMAL: placeholdertype = 3;
pub const EDGE_NODE_SIZE: placeholdertype = 3;
pub const MARGIN_KERN_NODE_SIZE: placeholdertype = 3;
pub const MEDIUM_NODE_SIZE: placeholdertype = 3;
pub const MOVEMENT_NODE_SIZE: placeholdertype = 3;
pub const OPEN_NODE_SIZE: placeholdertype = 3;
pub const STYLE_NODE_SIZE: placeholdertype = 3;
pub const WORD_NODE_SIZE: placeholdertype = 3;
pub const EXPR_NODE_SIZE: placeholdertype = 4;
pub const GLUE_SPEC_SIZE: placeholdertype = 4;
pub const MARK_CLASS_NODE_SIZE: placeholdertype = 4;
pub const PAGE_INS_NODE_SIZE: placeholdertype = 4;
pub const ACTIVE_NODE_SIZE_EXTENDED: placeholdertype = 5;
pub const GLYPH_NODE_SIZE: placeholdertype = 5;
pub const INS_NODE_SIZE: placeholdertype = 5;
pub const RULE_NODE_SIZE: placeholdertype = 5;
pub const ALIGN_STACK_NODE_SIZE: placeholdertype = 6;
pub const NATIVE_NODE_SIZE: placeholdertype = 6;
pub const DELTA_NODE_SIZE: placeholdertype = 7;
pub const BOX_NODE_SIZE: placeholdertype = 8;
pub const PIC_NODE_SIZE: placeholdertype = 9;
pub const INDEX_NODE_SIZE: placeholdertype = 33;

pub const NOAD_SIZE: placeholdertype = 4;
pub const ACCENT_NOAD_SIZE: placeholdertype = 5;
pub const RADICAL_NOAD_SIZE: placeholdertype = 5;
pub const FRACTION_NOAD_SIZE: placeholdertype = 6;

/* MATH_COMP and others */
pub const ORD_NOAD: placeholdertype = 16;
pub const OP_NOAD: placeholdertype = 17;
pub const BIN_NOAD: placeholdertype = 18;
pub const REL_NOAD: placeholdertype = 19;
pub const OPEN_NOAD: placeholdertype = 20;
pub const CLOSE_NOAD: placeholdertype = 21;
pub const PUNCT_NOAD: placeholdertype = 22;
pub const INNER_NOAD: placeholdertype = 23;
pub const RADICAL_NOAD: placeholdertype = 24;
pub const FRACTION_NOAD: placeholdertype = 25;
pub const UNDER_NOAD: placeholdertype = 26;
pub const OVER_NOAD: placeholdertype = 27;
pub const ACCENT_NOAD: placeholdertype = 28;
pub const VCENTER_NOAD: placeholdertype = 29;
pub const LEFT_NOAD: placeholdertype = 30;
pub const RIGHT_NOAD: placeholdertype = 31;

/* args to TOP_BOT_MARK */
pub const TOP_MARK_CODE: placeholdertype = 0;
pub const FIRST_MARK_CODE: placeholdertype = 1;
pub const BOT_MARK_CODE: placeholdertype = 2;
pub const SPLIT_FIRST_MARK_CODE: placeholdertype = 3;
pub const SPLIT_BOT_MARK_CODE: placeholdertype = 4;

/* MATH_NODE stuff with L/R typesetting extras */
pub const BEFORE: placeholdertype = 0;
pub const AFTER: placeholdertype = 1;
pub const BEGIN_M_CODE: placeholdertype = 2;
pub const END_M_CODE: placeholdertype = 3;
pub const L_CODE: placeholdertype = 4;
pub const R_CODE: placeholdertype = 8;

pub const EXPR_NONE: placeholdertype = 0;
pub const EXPR_ADD: placeholdertype = 1;
pub const EXPR_SUB: placeholdertype = 2;
pub const EXPR_MULT: placeholdertype = 3;
pub const EXPR_DIV: placeholdertype = 4;
pub const EXPR_SCALE: placeholdertype = 5;

pub const BOTTOM_LEVEL: placeholdertype = 0;
pub const SIMPLE_GROUP: placeholdertype = 1;
pub const HBOX_GROUP: placeholdertype = 2;
pub const ADJUSTED_HBOX_GROUP: placeholdertype = 3;
pub const VBOX_GROUP: placeholdertype = 4;
pub const VTOP_GROUP: placeholdertype = 5;
pub const ALIGN_GROUP: placeholdertype = 6;
pub const NO_ALIGN_GROUP: placeholdertype = 7;
pub const OUTPUT_GROUP: placeholdertype = 8;
pub const MATH_GROUP: placeholdertype = 9;
pub const DISC_GROUP: placeholdertype = 10;
pub const INSERT_GROUP: placeholdertype = 11;
pub const VCENTER_GROUP: placeholdertype = 12;
pub const MATH_CHOICE_GROUP: placeholdertype = 13;
pub const SEMI_SIMPLE_GROUP: placeholdertype = 14;
pub const MATH_SHIFT_GROUP: placeholdertype = 15;
pub const MATH_LEFT_GROUP: placeholdertype = 16;

pub const SUP_CMD: placeholdertype = 0;
pub const SUB_CMD: placeholdertype = 1;

pub const FIL: placeholdertype = 1;
pub const FILL: placeholdertype = 2;
pub const FILLL: placeholdertype = 3;

pub const LIG_TAG: placeholdertype = 1;
pub const LIST_TAG: placeholdertype = 2;
pub const EXT_TAG: placeholdertype = 3;

/* scanner_status values: */
pub const NORMAL: placeholdertype = 0;
pub const SKIPPING: placeholdertype = 1;
pub const DEFINING: placeholdertype = 2;
pub const MATCHING: placeholdertype = 3;
pub const ALIGNING: placeholdertype = 4;
pub const ABSORBING: placeholdertype = 5;

/* commands */

pub const ESCAPE: placeholdertype = 0;
/// = ESCAPE
pub const RELAX: placeholdertype = 0;
pub const LEFT_BRACE: placeholdertype = 1;
pub const RIGHT_BRACE: placeholdertype = 2;
pub const MATH_SHIFT: placeholdertype = 3;
pub const TAB_MARK: placeholdertype = 4;
pub const CAR_RET: placeholdertype = 5;
/// = CAR_RET
pub const OUT_PARAM: placeholdertype = 5;
pub const MAC_PARAM: placeholdertype = 6;
pub const SUP_MARK: placeholdertype = 7;
pub const SUB_MARK: placeholdertype = 8;
pub const IGNORE: placeholdertype = 9;
/// = IGNORE
pub const ENDV: placeholdertype = 9;
pub const SPACER: placeholdertype = 10;
pub const LETTER: placeholdertype = 11;
pub const OTHER_CHAR: placeholdertype = 12;
pub const ACTIVE_CHAR: placeholdertype = 13;
/// = ACTIVE_CHAR
pub const PAR_END: placeholdertype = 13;
/// = ACTIVE_CHAR
pub const MATCH: placeholdertype = 13;
pub const COMMENT: placeholdertype = 14;
/// = COMMENT
pub const END_MATCH: placeholdertype = 14;
/// = COMMENT
pub const STOP: placeholdertype = 14;
pub const INVALID_CHAR: placeholdertype = 15;
/// = INVALID_CHAR
pub const DELIM_NUM: placeholdertype = 15;
pub const CHAR_NUM: placeholdertype = 16;
pub const MATH_CHAR_NUM: placeholdertype = 17;
pub const MARK: placeholdertype = 18;
pub const XRAY: placeholdertype = 19;
pub const MAKE_BOX: placeholdertype = 20;
pub const HMOVE: placeholdertype = 21;
pub const VMOVE: placeholdertype = 22;
pub const UN_HBOX: placeholdertype = 23;
pub const UN_VBOX: placeholdertype = 24;
pub const REMOVE_ITEM: placeholdertype = 25;
pub const HSKIP: placeholdertype = 26;
pub const VSKIP: placeholdertype = 27;
pub const MSKIP: placeholdertype = 28;
pub const KERN: placeholdertype = 29;
pub const MKERN: placeholdertype = 30;
pub const LEADER_SHIP: placeholdertype = 31;
pub const HALIGN: placeholdertype = 32;
pub const VALIGN: placeholdertype = 33;
pub const NO_ALIGN: placeholdertype = 34;
pub const VRULE: placeholdertype = 35;
pub const HRULE: placeholdertype = 36;
pub const INSERT: placeholdertype = 37;
pub const VADJUST: placeholdertype = 38;
pub const IGNORE_SPACES: placeholdertype = 39;
pub const AFTER_ASSIGNMENT: placeholdertype = 40;
pub const AFTER_GROUP: placeholdertype = 41;
pub const BREAK_PENALTY: placeholdertype = 42;
pub const START_PAR: placeholdertype = 43;
pub const ITAL_CORR: placeholdertype = 44;
pub const ACCENT: placeholdertype = 45;
pub const MATH_ACCENT: placeholdertype = 46;
pub const DISCRETIONARY: placeholdertype = 47;
pub const EQ_NO: placeholdertype = 48;
pub const LEFT_RIGHT: placeholdertype = 49;
pub const MATH_COMP: placeholdertype = 50;
pub const LIMIT_SWITCH: placeholdertype = 51;
pub const ABOVE: placeholdertype = 52;
pub const MATH_STYLE: placeholdertype = 53;
pub const MATH_CHOICE: placeholdertype = 54;
pub const NON_SCRIPT: placeholdertype = 55;
pub const VCENTER: placeholdertype = 56;
pub const CASE_SHIFT: placeholdertype = 57;
pub const MESSAGE: placeholdertype = 58;
pub const EXTENSION: placeholdertype = 59;
pub const IN_STREAM: placeholdertype = 60;
pub const BEGIN_GROUP: placeholdertype = 61;
pub const END_GROUP: placeholdertype = 62;
pub const OMIT: placeholdertype = 63;
pub const EX_SPACE: placeholdertype = 64;
pub const NO_BOUNDARY: placeholdertype = 65;
pub const RADICAL: placeholdertype = 66;
pub const END_CS_NAME: placeholdertype = 67;
pub const CHAR_GIVEN: placeholdertype = 68;
pub const MIN_INTERNAL: placeholdertype = 68;
pub const MATH_GIVEN: placeholdertype = 69;
pub const XETEX_MATH_GIVEN: placeholdertype = 70;
pub const LAST_ITEM: placeholdertype = 71;
pub const MAX_NON_PREFIXED_COMMAND: placeholdertype = 71;
pub const TOKS_REGISTER: placeholdertype = 72;
pub const ASSIGN_TOKS: placeholdertype = 73;
pub const ASSIGN_INT: placeholdertype = 74;
pub const ASSIGN_DIMEN: placeholdertype = 75;
pub const ASSIGN_GLUE: placeholdertype = 76;
pub const ASSIGN_MU_GLUE: placeholdertype = 77;
pub const ASSIGN_FONT_DIMEN: placeholdertype = 78;
pub const ASSIGN_FONT_INT: placeholdertype = 79;
pub const SET_AUX: placeholdertype = 80;
pub const SET_PREV_GRAF: placeholdertype = 81;
pub const SET_PAGE_DIMEN: placeholdertype = 82;
pub const SET_PAGE_INT: placeholdertype = 83;
pub const SET_BOX_DIMEN: placeholdertype = 84;
pub const SET_SHAPE: placeholdertype = 85;
pub const DEF_CODE: placeholdertype = 86;
pub const XETEX_DEF_CODE: placeholdertype = 87;
pub const DEF_FAMILY: placeholdertype = 88;
pub const SET_FONT: placeholdertype = 89;
pub const DEF_FONT: placeholdertype = 90;
pub const MAX_INTERNAL: placeholdertype = 91;
pub const REGISTER: placeholdertype = 91;
pub const ADVANCE: placeholdertype = 92;
pub const MULTIPLY: placeholdertype = 93;
pub const DIVIDE: placeholdertype = 94;
pub const PREFIX: placeholdertype = 95;
pub const LET: placeholdertype = 96;
pub const SHORTHAND_DEF: placeholdertype = 97;
pub const READ_TO_CS: placeholdertype = 98;
pub const DEF: placeholdertype = 99;
pub const SET_BOX: placeholdertype = 100;
pub const HYPH_DATA: placeholdertype = 101;
pub const SET_INTERACTION: placeholdertype = 102;
pub const EXPAND_AFTER: placeholdertype = 104;
pub const NO_EXPAND: placeholdertype = 105;
pub const INPUT: placeholdertype = 106;
pub const IF_TEST: placeholdertype = 107;
pub const FI_OR_ELSE: placeholdertype = 108;
pub const CS_NAME: placeholdertype = 109;
pub const CONVERT: placeholdertype = 110;
pub const THE: placeholdertype = 111;
pub const TOP_BOT_MARK: placeholdertype = 112;

/* args to SET_BOX_DIMEN */
pub const WIDTH_OFFSET: placeholdertype = 1;
pub const DEPTH_OFFSET: placeholdertype = 2;
pub const HEIGHT_OFFSET: placeholdertype = 3;

/* args to LAST_ITEM -- heavily overloaded by (X)eTeX for extensions */
pub const INT_VAL: placeholdertype = 0;
pub const DIMEN_VAL: placeholdertype = 1;
pub const GLUE_VAL: placeholdertype = 2;
pub const MU_VAL: placeholdertype = 3;
pub const LAST_NODE_TYPE_CODE: placeholdertype = 3;
pub const INPUT_LINE_NO_CODE: placeholdertype = 4;
pub const BADNESS_CODE: placeholdertype = 5;
pub const ETEX_VERSION_CODE: placeholdertype = 6;
pub const CURRENT_GROUP_LEVEL_CODE: placeholdertype = 7;
pub const CURRENT_GROUP_TYPE_CODE: placeholdertype = 8;
pub const CURRENT_IF_LEVEL_CODE: placeholdertype = 9;
pub const CURRENT_IF_TYPE_CODE: placeholdertype = 10;
pub const CURRENT_IF_BRANCH_CODE: placeholdertype = 11;
pub const GLUE_STRETCH_ORDER_CODE: placeholdertype = 12;
pub const GLUE_SHRINK_ORDER_CODE: placeholdertype = 13;
pub const XETEX_VERSION_CODE: placeholdertype = 14;
pub const XETEX_COUNT_GLYPHS_CODE: placeholdertype = 15;
pub const XETEX_COUNT_VARIATIONS_CODE: placeholdertype = 16;
pub const XETEX_VARIATION_CODE: placeholdertype = 17;
pub const XETEX_FIND_VARIATION_BY_NAME_CODE: placeholdertype = 18;
pub const XETEX_VARIATION_MIN_CODE: placeholdertype = 19;
pub const XETEX_VARIATION_MAX_CODE: placeholdertype = 20;
pub const XETEX_VARIATION_DEFAULT_CODE: placeholdertype = 21;
pub const XETEX_COUNT_FEATURES_CODE: placeholdertype = 22;
pub const XETEX_FEATURE_CODE_CODE: placeholdertype = 23;
pub const XETEX_FIND_FEATURE_BY_NAME_CODE: placeholdertype = 24;
pub const XETEX_IS_EXCLUSIVE_FEATURE_CODE: placeholdertype = 25;
pub const XETEX_COUNT_SELECTORS_CODE: placeholdertype = 26;
pub const XETEX_SELECTOR_CODE_CODE: placeholdertype = 27;
pub const XETEX_FIND_SELECTOR_BY_NAME_CODE: placeholdertype = 28;
pub const XETEX_IS_DEFAULT_SELECTOR_CODE: placeholdertype = 29;
pub const XETEX_OT_COUNT_SCRIPTS_CODE: placeholdertype = 30;
pub const XETEX_OT_COUNT_LANGUAGES_CODE: placeholdertype = 31;
pub const XETEX_OT_COUNT_FEATURES_CODE: placeholdertype = 32;
pub const XETEX_OT_SCRIPT_CODE: placeholdertype = 33;
pub const XETEX_OT_LANGUAGE_CODE: placeholdertype = 34;
pub const XETEX_OT_FEATURE_CODE: placeholdertype = 35;
pub const XETEX_MAP_CHAR_TO_GLYPH_CODE: placeholdertype = 36;
pub const XETEX_GLYPH_INDEX_CODE: placeholdertype = 37;
pub const XETEX_FONT_TYPE_CODE: placeholdertype = 38;
pub const XETEX_FIRST_CHAR_CODE: placeholdertype = 39;
pub const XETEX_LAST_CHAR_CODE: placeholdertype = 40;
pub const PDF_LAST_X_POS_CODE: placeholdertype = 41;
pub const PDF_LAST_Y_POS_CODE: placeholdertype = 42;
pub const PDF_SHELL_ESCAPE_CODE: placeholdertype = 45;
pub const XETEX_PDF_PAGE_COUNT_CODE: placeholdertype = 46;
pub const XETEX_GLYPH_BOUNDS_CODE: placeholdertype = 47;
pub const FONT_CHAR_WD_CODE: placeholdertype = 48;
pub const FONT_CHAR_HT_CODE: placeholdertype = 49;
pub const FONT_CHAR_DP_CODE: placeholdertype = 50;
pub const FONT_CHAR_IC_CODE: placeholdertype = 51;
pub const PAR_SHAPE_LENGTH_CODE: placeholdertype = 52;
pub const PAR_SHAPE_INDENT_CODE: placeholdertype = 53;
pub const PAR_SHAPE_DIMEN_CODE: placeholdertype = 54;
pub const GLUE_STRETCH_CODE: placeholdertype = 55;
pub const GLUE_SHRINK_CODE: placeholdertype = 56;
pub const MU_TO_GLUE_CODE: placeholdertype = 57;
pub const GLUE_TO_MU_CODE: placeholdertype = 58;
pub const ETEX_EXPR: placeholdertype = 59;

/* args to CONVERT -- also heavily overloaded */
pub const NUMBER_CODE: placeholdertype = 0;
pub const ROMAN_NUMERAL_CODE: placeholdertype = 1;
pub const STRING_CODE: placeholdertype = 2;
pub const MEANING_CODE: placeholdertype = 3;
pub const FONT_NAME_CODE: placeholdertype = 4;
pub const ETEX_REVISION_CODE: placeholdertype = 5;
pub const XETEX_REVISION_CODE: placeholdertype = 6;
pub const XETEX_VARIATION_NAME_CODE: placeholdertype = 7;
pub const XETEX_FEATURE_NAME_CODE: placeholdertype = 8;
pub const XETEX_SELECTOR_NAME_CODE: placeholdertype = 9;
pub const XETEX_GLYPH_NAME_CODE: placeholdertype = 10;
pub const LEFT_MARGIN_KERN_CODE: placeholdertype = 11;
pub const RIGHT_MARGIN_KERN_CODE: placeholdertype = 12;
pub const XETEX_UCHAR_CODE: placeholdertype = 13;
pub const XETEX_UCHARCAT_CODE: placeholdertype = 14;
pub const JOB_NAME_CODE: placeholdertype = 15;
pub const PDF_STRCMP_CODE: placeholdertype = 43;
pub const PDF_MDFIVE_SUM_CODE: placeholdertype = 44;

/* args to IF_TEST */
pub const IF_CHAR_CODE: placeholdertype = 0;
pub const IF_CODE: placeholdertype = 1;
pub const IF_CAT_CODE: placeholdertype = 1;
pub const IF_INT_CODE: placeholdertype = 2;
pub const IF_DIM_CODE: placeholdertype = 3;
pub const IF_ODD_CODE: placeholdertype = 4;
pub const IF_VMODE_CODE: placeholdertype = 5;
pub const IF_HMODE_CODE: placeholdertype = 6;
pub const IF_MMODE_CODE: placeholdertype = 7;
pub const IF_INNER_CODE: placeholdertype = 8;
pub const IF_VOID_CODE: placeholdertype = 9;
pub const IF_HBOX_CODE: placeholdertype = 10;
pub const IF_VBOX_CODE: placeholdertype = 11;
pub const IFX_CODE: placeholdertype = 12;
pub const IF_EOF_CODE: placeholdertype = 13;
pub const IF_TRUE_CODE: placeholdertype = 14;
pub const IF_FALSE_CODE: placeholdertype = 15;
pub const IF_CASE_CODE: placeholdertype = 16;
pub const IF_DEF_CODE: placeholdertype = 17;
pub const IF_CS_CODE: placeholdertype = 18;
pub const IF_FONT_CHAR_CODE: placeholdertype = 19;
pub const IF_IN_CSNAME_CODE: placeholdertype = 20;
pub const IF_PRIMITIVE_CODE: placeholdertype = 21;

/* args to FI_OR_ELSE */
pub const FI_CODE: placeholdertype = 2;
pub const ELSE_CODE: placeholdertype = 3;
pub const OR_CODE: placeholdertype = 4;

/* special args for TAB_MARK, CAR_RET */
pub const SPAN_CODE: placeholdertype = (BIGGEST_USV + 2);
pub const CR_CODE: placeholdertype = (BIGGEST_USV + 3);
pub const CR_CR_CODE: placeholdertype = (BIGGEST_USV + 4);

/* HSKIP, VSKIP, MSKIP */
pub const FIL_CODE: placeholdertype = 0;
pub const FILL_CODE: placeholdertype = 1;
pub const SS_CODE: placeholdertype = 2;
pub const FIL_NEG_CODE: placeholdertype = 3;
pub const SKIP_CODE: placeholdertype = 4;
pub const MSKIP_CODE: placeholdertype = 5;

/* MAKE_BOX, UN_HBOX, UN_VBOX */
pub const BOX_CODE: placeholdertype = 0;
pub const COPY_CODE: placeholdertype = 1;
pub const LAST_BOX_CODE: placeholdertype = 2;
pub const VSPLIT_CODE: placeholdertype = 3;
pub const VTOP_CODE: placeholdertype = 4;

/* LEADER_SHIP */
pub const A_LEADERS: placeholdertype = 100;
pub const C_LEADERS: placeholdertype = 101;
pub const X_LEADERS: placeholdertype = 102;

/* LIMIT_SWITCH */
/* also NORMAL = 0 */
pub const LIMITS: placeholdertype = 1;
pub const NO_LIMITS: placeholdertype = 2;

/* MATH_STYLE */
pub const DISPLAY_STYLE: placeholdertype = 0;
pub const TEXT_STYLE: placeholdertype = 2;
pub const SCRIPT_STYLE: placeholdertype = 4;
pub const SCRIPT_SCRIPT_STYLE: placeholdertype = 6;

/* ABOVE */
pub const ABOVE_CODE: placeholdertype = 0;
pub const OVER_CODE: placeholdertype = 1;
pub const ATOP_CODE: placeholdertype = 2;
pub const DELIMITED_CODE: placeholdertype = 3;

/* SHORTHAND_DEF */
pub const CHAR_DEF_CODE: placeholdertype = 0;
pub const MATH_CHAR_DEF_CODE: placeholdertype = 1;
pub const COUNT_DEF_CODE: placeholdertype = 2;
pub const DIMEN_DEF_CODE: placeholdertype = 3;
pub const SKIP_DEF_CODE: placeholdertype = 4;
pub const MU_SKIP_DEF_CODE: placeholdertype = 5;
pub const TOKS_DEF_CODE: placeholdertype = 6;
pub const CHAR_SUB_DEF_CODE: placeholdertype = 7;
pub const XETEX_MATH_CHAR_NUM_DEF_CODE: placeholdertype = 8;
pub const XETEX_MATH_CHAR_DEF_CODE: placeholdertype = 9;

/* XRAY */
pub const SHOW_CODE: placeholdertype = 0;
pub const SHOW_BOX_CODE: placeholdertype = 1;
pub const SHOW_THE_CODE: placeholdertype = 2;
pub const SHOW_LISTS: placeholdertype = 3;
pub const SHOW_GROUPS: placeholdertype = 4;
pub const SHOW_TOKENS: placeholdertype = 5;
pub const SHOW_IFS: placeholdertype = 6;

/* EXTENSION */
pub const OPEN_NODE: placeholdertype = 0;
pub const WRITE_NODE: placeholdertype = 1;
pub const CLOSE_NODE: placeholdertype = 2;
pub const SPECIAL_NODE: placeholdertype = 3;
pub const LANGUAGE_NODE: placeholdertype = 4;
pub const IMMEDIATE_CODE: placeholdertype = 4;
pub const SET_LANGUAGE_CODE: placeholdertype = 5;
pub const PDFTEX_FIRST_EXTENSION_CODE: placeholdertype = 6;
pub const PDF_SAVE_POS_NODE: placeholdertype = 6;
/// not to be confused with PIC_NODE = 43!
pub const PIC_FILE_CODE: placeholdertype = 41;
/// not to be confused with PDF_NODE = 44!
pub const PDF_FILE_CODE: placeholdertype = 42;
/// not to be confused with GLYPH_NODE = 42!
pub const GLYPH_CODE: placeholdertype = 43;
pub const XETEX_INPUT_ENCODING_EXTENSION_CODE: placeholdertype = 44;
pub const XETEX_DEFAULT_ENCODING_EXTENSION_CODE: placeholdertype = 45;
pub const XETEX_LINEBREAK_LOCALE_EXTENSION_CODE: placeholdertype = 46;

/* VALIGN overloads */
pub const BEGIN_L_CODE: placeholdertype = 6;
pub const END_L_CODE: placeholdertype = 7;
pub const BEGIN_R_CODE: placeholdertype = 10;
pub const END_R_CODE: placeholdertype = 11;

/* begin_token_list() types */
pub const PARAMETER: u16 = 0;
pub const U_TEMPLATE: u16 = 1;
pub const V_TEMPLATE: u16 = 2;
pub const BACKED_UP: u16 = 3;
pub const BACKED_UP_CHAR: u16 = 4;
pub const INSERTED: u16 = 5;
pub const MACRO: u16 = 6;
pub const OUTPUT_TEXT: u16 = 7;
pub const EVERY_PAR_TEXT: u16 = 8;
pub const EVERY_MATH_TEXT: u16 = 9;
pub const EVERY_DISPLAY_TEXT: u16 = 10;
pub const EVERY_HBOX_TEXT: u16 = 11;
pub const EVERY_VBOX_TEXT: u16 = 12;
pub const EVERY_JOB_TEXT: u16 = 13;
pub const EVERY_CR_TEXT: u16 = 14;
pub const MARK_TEXT: u16 = 15;
pub const EVERY_EOF_TEXT: u16 = 16;
pub const INTER_CHAR_TEXT: u16 = 17;
pub const WRITE_TEXT: u16 = 18;
pub const TECTONIC_CODA_TEXT: u16 = 19;

/* input state */
pub const MID_LINE: placeholdertype = 1;
pub const SKIP_BLANKS: placeholdertype = 17;
pub const NEW_LINE: placeholdertype = 33;

/* DVI format codes */
pub const XDV_ID_BYTE: placeholdertype = 7;
pub const SPX_ID_BYTE: placeholdertype = 100;

/* page_contents possibilities (EMPTY is overloaded) */
pub const EMPTY: placeholdertype = 0;
pub const INSERTS_ONLY: placeholdertype = 1;
pub const BOX_THERE: placeholdertype = 2;

pub const SET1: placeholdertype = 128;
pub const SET_RULE: placeholdertype = 132;
pub const PUT_RULE: placeholdertype = 137;
pub const BOP: placeholdertype = 139;
pub const EOP: placeholdertype = 140;
pub const PUSH: placeholdertype = 141;
pub const POP: placeholdertype = 142;
pub const RIGHT1: placeholdertype = 143;
pub const DOWN1: placeholdertype = 157;
pub const FNT1: placeholdertype = 235;
pub const XXX1: placeholdertype = 239;
pub const XXX4: placeholdertype = 242;
pub const FNT_DEF1: placeholdertype = 243;
pub const PRE: placeholdertype = 247;
pub const POST: placeholdertype = 248;
pub const POST_POST: placeholdertype = 249;
pub const DEFINE_NATIVE_FONT: placeholdertype = 252;
pub const SET_GLYPHS: placeholdertype = 253;
pub const SET_TEXT_AND_GLYPHS: placeholdertype = 254;

pub const XETEX_INPUT_MODE_AUTO: placeholdertype = 0;
pub const XETEX_VERSION: placeholdertype = 0;
pub const EXACTLY: placeholdertype = 0;
pub const FONT_BASE: placeholdertype = 0;
pub const INSERTING: placeholdertype = 0;
pub const NON_ADDRESS: placeholdertype = 0;
pub const RESTORE_OLD_VALUE: placeholdertype = 0;
pub const TOKEN_LIST: placeholdertype = 0;
pub const UNDEFINED_PRIMITIVE: placeholdertype = 0;
pub const UNHYPHENATED: placeholdertype = 0;
pub const ADDITIONAL: placeholdertype = 1;
pub const EXPLICIT: placeholdertype = 1;
pub const FIXED_ACC: placeholdertype = 1;
pub const HYPHENATED: placeholdertype = 1;
pub const JUST_OPEN: placeholdertype = 1;
pub const MATH_CHAR: placeholdertype = 1;
pub const PRIM_BASE: placeholdertype = 1;
pub const RESTORE_ZERO: placeholdertype = 1;
pub const REVERSED: placeholdertype = 1;
pub const SLANT_CODE: placeholdertype = 1;
pub const SPLIT_UP: placeholdertype = 1;
pub const STRETCHING: placeholdertype = 1;
pub const VMODE: placeholdertype = 1;
pub const ACC_KERN: placeholdertype = 2;
pub const BOTTOM_ACC: placeholdertype = 2;
pub const CLOSED: placeholdertype = 2;
pub const DLIST: placeholdertype = 2;
pub const ETEX_VERSION: placeholdertype = 2;
pub const INSERT_TOKEN: placeholdertype = 2;
pub const SHRINKING: placeholdertype = 2;
pub const SPACE_CODE: placeholdertype = 2;
pub const SUB_BOX: placeholdertype = 2;
pub const DISPLAYOPERATORMINHEIGHT: placeholdertype = 3;
pub const LEVEL_BOUNDARY: placeholdertype = 3;
// pub const MATH_SHIFT: placeholdertype = 3;
pub const SPACE_ADJUSTMENT: placeholdertype = 3;
pub const SUB_MLIST: placeholdertype = 3;
pub const IDENT_VAL: placeholdertype = 4;
pub const MATH_TEXT_CHAR: placeholdertype = 4;
pub const RESTORE_SA: placeholdertype = 4;
pub const SPACE_SHRINK_CODE: placeholdertype = 4;
// pub const OUT_PARAM: placeholdertype = 5;
pub const TOK_VAL: placeholdertype = 5;
pub const X_HEIGHT_CODE: placeholdertype = 5;
pub const ACCENTBASEHEIGHT: placeholdertype = 6;
pub const INTER_CHAR_VAL: placeholdertype = 6;
// pub const MAC_PARAM: placeholdertype = 6;
pub const QUAD_CODE: placeholdertype = 6;
pub const EXTRA_SPACE_CODE: placeholdertype = 7;
pub const MARK_VAL: placeholdertype = 7;
// pub const SUP_MARK: placeholdertype = 7;
pub const VAR_FAM_CLASS: placeholdertype = 7;
// pub const IGNORE: placeholdertype = 9;
pub const SUBSCRIPTTOPMAX: placeholdertype = 9;
pub const NATIVE_GLYPH_INFO_SIZE: placeholdertype = 10;
// pub const ACTIVE_CHAR: placeholdertype = 13;
pub const CARRIAGE_RETURN: placeholdertype = 13;
pub const SUPERSCRIPTBOTTOMMIN: placeholdertype = 13;
pub const TOTAL_MATHEX_PARAMS: placeholdertype = 13;
// pub const COMMENT: placeholdertype = 14;
pub const HI_MEM_STAT_USAGE: placeholdertype = 15;
// pub const INVALID_CHAR: placeholdertype = 15;
pub const MAX_CHAR_CODE: placeholdertype = 15;
pub const SUBSUPERSCRIPTGAPMIN: placeholdertype = 15;
pub const SUPERSCRIPTBOTTOMMAXWITHSUBSCRIPT: placeholdertype = 16;
pub const TOTAL_MATHSY_PARAMS: placeholdertype = 22;
pub const STACKGAPMIN: placeholdertype = 26;
pub const STACKDISPLAYSTYLEGAPMIN: placeholdertype = 27;
pub const UNLESS_CODE: placeholdertype = 32;
// pub const VRULE: placeholdertype = 35;
pub const FRACTIONNUMERATORGAPMIN: placeholdertype = 36;
pub const FRACTIONNUMDISPLAYSTYLEGAPMIN: placeholdertype = 37;
// pub const XETEX_FIRST_CHAR_CODE: placeholdertype = 39;
pub const FRACTIONDENOMINATORGAPMIN: placeholdertype = 39;
pub const FRACTIONDENOMDISPLAYSTYLEGAPMIN: placeholdertype = 40;
pub const XETEX_DIM: placeholdertype = 47;
pub const RADICALVERTICALGAP: placeholdertype = 49;
pub const RADICALDISPLAYSTYLEVERTICALGAP: placeholdertype = 50;
pub const RADICALRULETHICKNESS: placeholdertype = 51;
pub const ETEX_GLUE: placeholdertype = 57;
pub const ETEX_MU: placeholdertype = 58;
pub const COND_MATH_GLUE: placeholdertype = 98;
pub const MU_GLUE: placeholdertype = 99;
pub const MAX_COMMAND: placeholdertype = 102;
pub const UNDEFINED_CS: placeholdertype = 103;
pub const HMODE: placeholdertype = 104;
pub const CALL: placeholdertype = 113;
pub const LONG_CALL: placeholdertype = 114;
pub const OUTER_CALL: placeholdertype = 115;
pub const LONG_OUTER_CALL: placeholdertype = 116;
pub const END_TEMPLATE: placeholdertype = 117;
pub const DONT_EXPAND: placeholdertype = 118;
pub const GLUE_REF: placeholdertype = 119;
pub const SHAPE_REF: placeholdertype = 120;
pub const BOX_REF: placeholdertype = 121;
pub const DATA: placeholdertype = 122;
pub const DIMEN_VAL_LIMIT: placeholdertype = 128;
pub const MMODE: placeholdertype = 207;
pub const BIGGEST_LANG: placeholdertype = 255;
pub const MU_VAL_LIMIT: placeholdertype = 256;
pub const TOO_BIG_LANG: placeholdertype = 256;
pub const BOX_VAL_LIMIT: placeholdertype = 320;
pub const TOK_VAL_LIMIT: placeholdertype = 384;
pub const PRIM_PRIME: placeholdertype = 431;
pub const PRIM_SIZE: placeholdertype = 500;
pub const MAX_HLIST_STACK: placeholdertype = 512;
pub const HYPH_PRIME: placeholdertype = 607;
pub const HYPHENATABLE_LENGTH_LIMIT: placeholdertype = 4095;
pub const CHAR_CLASS_LIMIT: placeholdertype = 4096;
pub const EJECT_PENALTY: placeholdertype = -10000;
pub const INF_BAD: placeholdertype = 10000;
pub const INF_PENALTY: placeholdertype = 10000;
pub const DEFAULT_RULE: placeholdertype = 26214;
pub const TOO_BIG_CHAR: placeholdertype = 65536;
pub const NO_EXPAND_FLAG: placeholdertype = (BIGGEST_USV + 2);

pub const ACTIVE_MATH_CHAR: placeholdertype = 0x1FFFFF;

/* Token codes */

/// 1 << 21
pub const MAX_CHAR_VAL: placeholdertype = 0x200000;
pub const CS_TOKEN_FLAG: placeholdertype = 0x1FFFFFF;
/// LEFT_BRACE << 21
pub const LEFT_BRACE_TOKEN: placeholdertype = 0x200000;
/// (LEFT_BRACE + 1) << 21
pub const LEFT_BRACE_LIMIT: placeholdertype = 0x400000;
/// RIGHT_BRACE << 21
pub const RIGHT_BRACE_TOKEN: placeholdertype = 0x400000;
/// (RIGHT_BRACE + 1) << 21
pub const RIGHT_BRACE_LIMIT: placeholdertype = 0x600000;
/// MATH_SHIFT << 21
pub const MATH_SHIFT_TOKEN: placeholdertype = 0x600000;
/// TAB_MARK << 21
pub const TAB_TOKEN: placeholdertype = 0x800000;
/// OUT_PARAM << 21
pub const OUT_PARAM_TOKEN: placeholdertype = 0xA00000;
/// SPACER << 21 + ord(' ')
pub const SPACE_TOKEN: placeholdertype = 0x1400020;
/// LETTER << 21
pub const LETTER_TOKEN: placeholdertype = 0x1600000;
/// OTHER_CHAR << 21
pub const OTHER_TOKEN: placeholdertype = 0x1800000;
/// MATCH << 21
pub const MATCH_TOKEN: placeholdertype = 0x1A00000;
/// END_MATCH << 21
pub const END_MATCH_TOKEN: placeholdertype = 0x1C00000;
pub const PROTECTED_TOKEN: placeholdertype = (END_MATCH_TOKEN + 1);

pub const A_TOKEN: placeholdertype = (LETTER_TOKEN + 'A' as placeholdertype);
pub const OTHER_A_TOKEN: placeholdertype = (OTHER_TOKEN + 'A' as placeholdertype);
pub const HEX_TOKEN: placeholdertype = (OTHER_TOKEN + '"' as placeholdertype);
pub const OCTAL_TOKEN: placeholdertype = (OTHER_TOKEN + '\'' as placeholdertype);
pub const CONTINENTAL_POINT_TOKEN: placeholdertype = (OTHER_TOKEN + ',' as placeholdertype);
pub const POINT_TOKEN: placeholdertype = (OTHER_TOKEN + '.' as placeholdertype);
pub const ZERO_TOKEN: placeholdertype = (OTHER_TOKEN + '0' as placeholdertype);
pub const ALPHA_TOKEN: placeholdertype = (OTHER_TOKEN + '`' as placeholdertype);

pub const BOX_FLAG: placeholdertype = 0x40000000;
pub const GLOBAL_BOX_FLAG: placeholdertype = 0x40008000;
pub const SHIP_OUT_FLAG: placeholdertype = 0x40010000;
pub const LEADER_FLAG: placeholdertype = 0x40010001;

pub const LP_CODE_BASE: placeholdertype = 2;
pub const RP_CODE_BASE: placeholdertype = 3;

pub const LEFT_SIDE: placeholdertype = 0;
pub const RIGHT_SIDE: placeholdertype = 1;

/* modes to do_marks() */
pub const VSPLIT_INIT: placeholdertype = 0;
pub const FIRE_UP_INIT: placeholdertype = 1;
pub const FIRE_UP_DONE: placeholdertype = 2;
pub const DESTROY_MARKS: placeholdertype = 3;

pub const MARKS_CODE: placeholdertype = 5;

pub const IGNORE_DEPTH: placeholdertype = -65536000;

pub const MIDDLE_NOAD: placeholdertype = 1;

/* movement() */
pub const MOV_NONE_SEEN: placeholdertype = 0;
pub const MOV_Y_HERE: placeholdertype = 1;
pub const MOV_Z_HERE: placeholdertype = 2;
pub const MOV_YZ_OK: placeholdertype = 3;
pub const MOV_Y_OK: placeholdertype = 4;
pub const MOV_Z_OK: placeholdertype = 5;
pub const MOV_Y_SEEN: placeholdertype = 6;
pub const MOV_D_FIXED: placeholdertype = 6;
pub const MOV_Z_SEEN: placeholdertype = 12;

/* Increase this whenever the engine internals change such that the contents
 * of the "format" files must be regenerated -- this includes changes to the
 * string pool. KEEP SYNCHRONIZED WITH src/lib.rs!!! */

pub const FORMAT_SERIAL: placeholdertype = 28;
