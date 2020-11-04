#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use std::ptr;

use super::xetex_texmfmp::{get_date_and_time, to_rust_string};
use crate::cmd::*;
use crate::core_memory::{xmalloc, xmalloc_array};
use crate::fmt_file::{load_fmt_file, store_fmt_file};
use crate::help;
use crate::node::*;
use crate::trie::*;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, overflow};
use crate::xetex_layout_interface::{destroy_font_manager, set_cp_code};
use crate::xetex_output::{
    print, print_chr, print_cstr, print_esc_cstr, print_file_line, print_int, print_nl,
    print_nl_cstr,
};
use crate::xetex_pagebuilder::initialize_pagebuilder_variables;
use crate::xetex_shipout::{deinitialize_shipout_variables, initialize_shipout_variables};
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_stringpool::{length, load_pool_strings, make_string};
use crate::xetex_synctex::synctex_init_command;
use crate::xetex_texmfmp::maketexstring;
use crate::xetex_xetex0::{
    alter_aux, alter_box_dimen, alter_integer, alter_page_so_far, alter_prev_graf, back_error,
    back_input, close_files_and_terminate, delete_glue_ref, delete_token_ref, diagnostic, do_marks,
    do_register_command, end_file_reading, end_token_list, eq_define, eq_word_define,
    find_font_dimen, find_sa_element, flush_list, flush_node_list, free_node, geq_define,
    geq_word_define, get_avail, get_node, get_r_token, get_token, get_x_token, gsa_def, id_lookup,
    main_control, max_hyphenatable_length, new_font, new_interaction, open_log_file, prim_lookup,
    print_cmd_chr, read_toks, sa_def, scan_box, scan_char_class, scan_char_class_not_ignored,
    scan_char_num, scan_dimen, scan_fifteen_bit_int, scan_font_ident, scan_glue, scan_glyph_number,
    scan_int, scan_keyword, scan_left_brace, scan_math_class_int, scan_math_fam_int,
    scan_optional_equals, scan_register_num, scan_toks, scan_usv_num, scan_xetex_math_char_int,
    show_cur_cmd_chr, show_save_groups, start_input, trap_zero_glue,
};
use crate::xetex_xetexd::{llist_link, set_class, set_family, LLIST_link, TeXInt, TeXOpt};
use bridge::ttstub_output_open_stdout;
use dpx::{pdf_files_close, pdf_files_init};
use libc::free;

pub(crate) type uintptr_t = u64;
pub(crate) type size_t = usize;
pub(crate) type ssize_t = isize;
/* tectonic/core-bridge.h: declarations of C/C++ => Rust bridge API
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

use bridge::TTHistory;

use bridge::OutputHandleWrapper;
/* quasi-hack to get the primary input */
/* tectonic/xetex-core.h: core XeTeX types and #includes.
   Copyright 2016 the Tectonic Project
   Licensed under the MIT License.
*/
// defines U_IS_BIG_ENDIAN for us
/* fontconfig */
/* freetype */
/* harfbuzz */
/* Endianness foo */
/* our typedefs */

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Selector {
    FILE_0,
    FILE_15,
    NO_PRINT,
    TERM_ONLY,
    LOG_ONLY,
    TERM_AND_LOG,
    PSEUDO,
    NEW_STRING,
    // Looks like bug in `write_out`, should be deleted after oxidize
    Other(u8),
}

impl From<Selector> for u8 {
    fn from(u: Selector) -> Self {
        use Selector::*;
        match u {
            FILE_0 => 0,
            FILE_15 => 15,
            NO_PRINT => 16,
            TERM_ONLY => 17,
            LOG_ONLY => 18,
            TERM_AND_LOG => 19,
            PSEUDO => 20,
            NEW_STRING => 21,
            Other(u) => u,
        }
    }
}

impl From<u8> for Selector {
    fn from(u: u8) -> Self {
        use Selector::*;
        match u {
            0 => FILE_0,
            15 => FILE_15,
            16 => NO_PRINT,
            17 => TERM_ONLY,
            18 => LOG_ONLY,
            19 => TERM_AND_LOG,
            20 => PSEUDO,
            21 => NEW_STRING,
            n => Other(n),
        }
    }
}

/*18: */
pub(crate) type UTF16_code = u16;
pub(crate) type UTF8_code = u8;
pub(crate) type UnicodeScalar = i32;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct b32x2_le_t {
    pub(crate) s0: i32,
    pub(crate) s1: i32,
}
/* The annoying `memory_word` type. We have to make sure the byte-swapping
 * that the (un)dumping routines do suffices to put things in the right place
 * in memory.
 *
 * This set of data used to be a huge mess (see comment after the
 * definitions). It is now (IMO) a lot more reasonable, but there will no
 * doubt be carryover weird terminology around the code.
 *
 * ## ENDIANNESS (cheat sheet because I'm lame)
 *
 * Intel is little-endian. Say that we have a 32-bit integer stored in memory
 * with `p` being a `uint8` pointer to its location. In little-endian land,
 * `p[0]` is least significant byte and `p[3]` is its most significant byte.
 *
 * Conversely, in big-endian land, `p[0]` is its most significant byte and
 * `p[3]` is its least significant byte.
 *
 * ## MEMORY_WORD LAYOUT
 *
 * Little endian:
 *
 *   bytes: --0-- --1-- --2-- --3-- --4-- --5-- --6-- --7--
 *   b32:   [lsb......s0.......msb] [lsb......s1.......msb]
 *   b16:   [l..s0...m] [l..s1...m] [l..s2...m] [l..s3...m]
 *
 * Big endian:
 *
 *   bytes: --0-- --1-- --2-- --3-- --4-- --5-- --6-- --7--
 *   b32:   [msb......s1.......lsb] [msb......s0.......lsb]
 *   b16:   [m..s3...l] [m..s2...l] [m..s1...l] [m...s0..l]
 *
 */
pub(crate) type b32x2 = b32x2_le_t;
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct b16x4_le_t {
    pub(crate) s0: u16,
    pub(crate) s1: u16,
    pub(crate) s2: u16,
    pub(crate) s3: u16,
}
pub(crate) type b16x4 = b16x4_le_t;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) union memory_word {
    pub(crate) b32: b32x2,
    pub(crate) b16: b16x4,
    pub(crate) gr: f64,
    pub(crate) ptr: *mut libc::c_void,
}

impl Default for memory_word {
    fn default() -> Self {
        unsafe {
            Self {
                ptr: 0 as *mut libc::c_void,
            }
        }
    }
}

#[derive(Copy, Clone, Default, PartialEq, Eq, derive_more::Constructor)]
#[repr(C)]
pub(crate) struct EqtbWord {
    pub(crate) lvl: u16, // b16.s0
    pub(crate) cmd: u16, // b16.s1
    pub(crate) val: i32, // b32.s1
}

/* ## THE ORIGINAL SITUATION (archived for posterity)
 *
 * In XeTeX, a "quarterword" is 16 bits. Who knows why. A "halfword" is,
 * sensibly, 32 bits. A "memory word" is a full word: either four quarters or
 * two halves: i.e., 64 bits. The memory word union also has options for
 * doubles (called `gr`), `integer` which is an i32 (called `cint`), and a
 * pointer (`ptr`).
 *
 * Original struct definition, LITTLE ENDIAN (condensed):
 *
 *   typedef union {
 *       struct { i32 LH, RH; } v;
 *       struct { short B1, B0; } u;
 *   } two_halves;
 *
 *   typedef struct {
 *       struct { u16 B3, B2, B1, B0; } u;
 *   } four_quarters;
 *
 *   typedef union {
 *       two_halves hh;
 *
 *       struct {
 *           i32 junk;
 *           i32 CINT;
 *       } u;
 *
 *       struct {
 *           four_quarters QQQQ;
 *       } v;
 *   } memory_word;
 *
 *   #  define cint u.CINT
 *   #  define qqqq v.QQQQ
 *
 * Original memory layout, LITTLE ENDIAN:
 *
 *   bytes:    --0-- --1-- --2-- --3-- --4-- --5-- --6-- --7--
 *   cint:                             [lsb...............msb]
 *   hh.u:     [l..B1...m] [l..B0...m]
 *   hh.v:     [lsb......LH.......msb] [lsb......RH.......msb]
 *   quarters: [l..B3...m] [l..B2...m] [l..B1...m] [l..B0...m]
 *
 * Original struct definition, BIG ENDIAN (condensed):
 *
 *   typedef union {
 *       struct { i32 RH, LH; } v;
 *       struct {
 *           i32 junk;
 *           short B0, B1;
 *       } u;
 *   } two_halves;
 *
 *   typedef struct {
 *       struct { u16 B0, B1, B2, B3; } u;
 *   } four_quarters;
 *
 *   typedef union {
 *       two_halves hh;
 *       four_quarters qqqq;
 *   } memory_word;
 *
 * Original memory layout, BIG ENDIAN:
 *
 *   bytes:    --0-- --1-- --2-- --3-- --4-- --5-- --6-- --7--
 *   cint:     [msb...............lsb]
 *   hh.u:                             [m..B0...l] [m..B1...l]
 *   hh.v:     [msb......RH.......lsb] [msb......LH.......lsb]
 *   quarters: [m..B0...l] [m..B1...l] [m..B2...l] [m...B3..l]
 *
 * Several things to note that apply to both endiannesses:
 *
 *   1. The different B0 and B1 instances do not line up.
 *   2. `cint` is isomorphic to `hh.v.RH`
 *   3. `hh.u.B0` is isomorphic to `qqqq.u.B2`
 *   4. `hh.u.B1` is isomorphic to `qqqq.u.B3`.
 *   5. The `four_quarters` field `u` serves no discernable purpose.
 *
 * CONVERTING TO THE NEW SYSTEM
 *
 * - `w.cint` => `w.b32.s1`
 * - `w.qqqq.u.B<n>` => `w.b16.s{{3 - <n>}}` !!!!!!!!!!!
 * - similar for `<quarterword_variable>.u.B<n>` => `<quarterword_variable>.s{{3 - <n>}}` !!!
 * - `w.hh.u.B0` => `w.b16.s1`
 * - `w.hh.u.B1` => `w.b16.s0`
 * - `w.hh.v.RH` => `w.b32.s1`
 * - `w.hh.v.LH` => `w.b32.s0`
 * - `four_quarters` => `b16x4`
 * - `two_halves` => `b32x2`
 *
 */
/* Symbolic accessors for various TeX data structures. I would loooove to turn these
 * into actual structs, but the path to doing that is not currently clear. Making
 * field references symbolic seems like a decent start. Sadly I don't see how to do
 * this conversion besides painstakingly annotating things.
 */
/* half of LLIST_info(p) */
/* the other half of LLIST_info(p) */
/* subtype; records L/R direction mode */
/* a scaled; 1 <=> WEB const `width_offset` */
/* a scaled; 2 <=> WEB const `depth_offset` */
/* a scaled; 3 <=> WEB const `height_offset` */
/* a scaled */
/* aka `link` of p+5 */
/* aka `type` of p+5 */
/* aka `subtype` of p+5 */
/* the glue ratio */
/* aka "subtype" of a node */
/* aka "rlink" in double-linked list */
/* aka "llink" in doubly-linked list */
/* was originally the `mem[x+2].int` field */
/* a scaled; "active_short" in the WEB */
/* a scaled */
/* aka "type" of a node */
/* aka "subtype" of a node */
/* the "natural width" difference */
/* the stretch difference in points */
/* the stretch difference in fil */
/* the stretch difference in fill */
/* the stretch difference in fill */
/* the shrink difference */
/* aka "subtype" of a node */
/* aka "llink" in doubly-linked list */
/* aka "rlink" in double-linked list */
/* "new left_edge position relative to cur_h" */
/* aka "llink" in doubly-linked list */
/* aka "rlink" in double-linked list */
/* "the floating_penalty to be used" */
/* a glue pointer */
/* a pointer to a vlist */
/* language number, 0..255 */
/* "minimum left fragment, range 1..63" */
/* "minimum right fragment, range 1..63" */
/* WEB: font(char(p)) */
/* WEB: character(char(p)) */
/* WEB: link(char(p)) */
/* "head of the token list for the mark" */
/* "the mark class" */
/* To check: do these really only apply to MATH_NODEs? */
/* number of UTF16 items in the text */
/* ... or the glyph number, if subtype==WhatsItNST::Glyph */
/* "an insertion for this class will break here if anywhere" */
/* "this insertion might break at broken_ptr" */
/* "the most recent insertion for this subtype" */
/* "the optimum most recent insertion" */
/* aka "llink" in doubly-linked list */
/* siggggghhhhh */
/* aka "rlink" in double-linked list */
/* aka "info" */
/* was originally the `mem[x+1].int` field */
/* number of bytes in the path item */
/* "reference count of token list to write" */
/* Synctex hacks various nodes to add an extra word at the end to store its
 * information, hence the need to know the node size to get the synctex
 * info. */
/* aka "link" of a link-list node */
/* aka "type" of a node */
/* aka "subtype" of a node */
/* a scaled */
/* a scaled */
/* e-TeX extended marks stuff ... not sure where to put these */
/* \topmarks<n> */
/* \firstmarks<n> */
/* \botmarks<n> */
/* \splitfirstmarks<n> */
/* \splitbotmarks<n> */
pub(crate) type glue_ord = u8;
/* enum: normal .. filll */
pub(crate) type internal_font_number = usize;
pub(crate) type font_index = i32;
pub(crate) type nine_bits = i32;
/* range: 0 .. 0x1FF */
pub(crate) type hyph_pointer = u16;
pub(crate) type save_pointer = i32;
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct list_state_record {
    pub(crate) mode: (bool, ListMode),
    pub(crate) head: usize,
    pub(crate) tail: usize,
    pub(crate) eTeX_aux: Option<usize>,
    pub(crate) prev_graf: i32,
    pub(crate) mode_line: i32,
    pub(crate) aux: memory_word,
}
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct input_state_t {
    pub(crate) state: InputState,
    pub(crate) index: Btl,
    pub(crate) start: i32,
    pub(crate) loc: i32,
    pub(crate) limit: i32,
    pub(crate) name: i32,
    pub(crate) synctex_tag: i32,
}
/* tectonic/xetex-io.h: XeTeX-specific low-level I/O routines
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

pub(crate) use super::xetex_io::UFILE;
/* xetex-ini.c: WEB initialization code translated to C
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* All the following variables are declared in xetex-xetexd.h */
pub(crate) static mut shell_escape_enabled: bool = false;

pub(crate) static mut EQTB: Vec<EqtbWord> = Vec::new();
#[no_mangle]
pub(crate) static mut bad: i32 = 0;

pub(crate) static mut name_of_file: String = String::new();

#[no_mangle]
pub(crate) static mut BUFFER: Vec<UnicodeScalar> = Vec::new();
#[no_mangle]
pub(crate) static mut first: i32 = 0;
#[no_mangle]
pub(crate) static mut last: i32 = 0;
#[no_mangle]
pub(crate) static mut max_buf_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut in_initex_mode: bool = false;
#[no_mangle]
pub(crate) static mut error_line: i32 = 0;
#[no_mangle]
pub(crate) static mut half_error_line: i32 = 0;
#[no_mangle]
pub(crate) static mut max_print_line: i32 = 0;
#[no_mangle]
pub(crate) static mut max_strings: usize = 0;
#[no_mangle]
pub(crate) static mut strings_free: i32 = 0;
#[no_mangle]
pub(crate) static mut string_vacancies: i32 = 0;
#[no_mangle]
pub(crate) static mut pool_size: i32 = 0;
#[no_mangle]
pub(crate) static mut pool_free: i32 = 0;
#[no_mangle]
pub(crate) static mut FONT_MEM_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut FONT_MAX: usize = 0;
#[no_mangle]
pub(crate) static mut HYPH_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut BUF_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut STACK_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut MAX_IN_OPEN: usize = 0;
#[no_mangle]
pub(crate) static mut PARAM_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut NEST_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut SAVE_SIZE: usize = 0;
#[no_mangle]
pub(crate) static mut expand_depth: i32 = 0;
#[no_mangle]
pub(crate) static mut file_line_error_style_p: i32 = 0;
#[no_mangle]
pub(crate) static mut halt_on_error_p: i32 = 0;
#[no_mangle]
pub(crate) static mut insert_src_special_auto: bool = false;
#[no_mangle]
pub(crate) static mut insert_src_special_every_par: bool = false;
#[no_mangle]
pub(crate) static mut insert_src_special_every_math: bool = false;
#[no_mangle]
pub(crate) static mut insert_src_special_every_vbox: bool = false;
#[no_mangle]
pub(crate) static mut str_pool: Vec<packed_UTF16_code> = Vec::new();
#[no_mangle]
pub(crate) static mut str_start: Vec<pool_pointer> = Vec::new();
#[no_mangle]
pub(crate) static mut pool_ptr: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut str_ptr: str_number = 0;
#[no_mangle]
pub(crate) static mut init_pool_ptr: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut init_str_ptr: str_number = 0;
#[no_mangle]
pub(crate) static mut rust_stdout: Option<OutputHandleWrapper> = None;
#[no_mangle]
pub(crate) static mut log_file: Option<OutputHandleWrapper> = None;
#[no_mangle]
pub(crate) static mut selector: Selector = Selector::FILE_0;
#[no_mangle]
pub(crate) static mut dig: [u8; 23] = [0; 23];
#[no_mangle]
pub(crate) static mut tally: i32 = 0;
#[no_mangle]
pub(crate) static mut term_offset: i32 = 0;
#[no_mangle]
pub(crate) static mut file_offset: i32 = 0;
#[no_mangle]
pub(crate) static mut trick_buf: [UTF16_code; 256] = [0; 256];
#[no_mangle]
pub(crate) static mut trick_count: i32 = 0;
#[no_mangle]
pub(crate) static mut first_count: i32 = 0;
#[no_mangle]
pub(crate) static mut doing_special: bool = false;
#[no_mangle]
pub(crate) static mut native_text: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut native_text_size: i32 = 0;
#[no_mangle]
pub(crate) static mut native_len: i32 = 0;
#[no_mangle]
pub(crate) static mut save_native_len: i32 = 0;
#[no_mangle]
pub(crate) static mut interaction: InteractionMode = InteractionMode::Batch;
#[no_mangle]
pub(crate) static mut deletions_allowed: bool = false;
#[no_mangle]
pub(crate) static mut set_box_allowed: bool = false;
#[no_mangle]
pub(crate) static mut history: TTHistory = TTHistory::SPOTLESS;
#[no_mangle]
pub(crate) static mut error_count: i8 = 0;
#[no_mangle]
pub(crate) static mut help_line: [&'static str; 6] = [""; 6];
#[no_mangle]
pub(crate) static mut help_ptr: u8 = 0;
#[no_mangle]
pub(crate) static mut use_err_help: bool = false;
#[no_mangle]
pub(crate) static mut arith_error: bool = false;
#[no_mangle]
pub(crate) static mut MEM: Vec<memory_word> = Vec::new();
#[no_mangle]
pub(crate) static mut lo_mem_max: i32 = 0;
#[no_mangle]
pub(crate) static mut hi_mem_min: i32 = 0;
#[no_mangle]
pub(crate) static mut var_used: i32 = 0;
#[no_mangle]
pub(crate) static mut dyn_used: i32 = 0;
#[no_mangle]
pub(crate) static mut avail: Option<usize> = Some(0);
#[no_mangle]
pub(crate) static mut mem_end: i32 = 0;
#[no_mangle]
pub(crate) static mut rover: i32 = 0;
#[no_mangle]
pub(crate) static mut last_leftmost_char: i32 = 0;
#[no_mangle]
pub(crate) static mut last_rightmost_char: i32 = 0;
#[no_mangle]
pub(crate) static mut first_p: i32 = 0;
#[no_mangle]
pub(crate) static mut global_prev_p: i32 = 0;
#[no_mangle]
pub(crate) static mut font_in_short_display: usize = 0;
#[no_mangle]
pub(crate) static mut depth_threshold: i32 = 0;
#[no_mangle]
pub(crate) static mut breadth_max: i32 = 0;
#[no_mangle]
pub(crate) static mut NEST: Vec<list_state_record> = Vec::new();
#[no_mangle]
pub(crate) static mut NEST_PTR: usize = 0;
#[no_mangle]
pub(crate) static mut MAX_NEST_STACK: usize = 0;
#[no_mangle]
pub(crate) static mut cur_list: list_state_record = list_state_record {
    mode: (false, ListMode::NoMode),
    head: 0,
    tail: 0,
    eTeX_aux: None,
    prev_graf: 0,
    mode_line: 0,
    aux: memory_word {
        b32: b32x2 { s0: 0, s1: 0 },
    },
};
#[no_mangle]
pub(crate) static mut shown_mode: (bool, ListMode) = (false, ListMode::NoMode);
#[no_mangle]
pub(crate) static mut hash: *mut b32x2 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hash_used: i32 = 0;
#[no_mangle]
pub(crate) static mut hash_extra: i32 = 0;
#[no_mangle]
pub(crate) static mut hash_top: i32 = 0;
#[no_mangle]
pub(crate) static mut EQTB_TOP: usize = 0;
#[no_mangle]
pub(crate) static mut hash_high: i32 = 0;
#[no_mangle]
pub(crate) static mut no_new_control_sequence: bool = false;
#[no_mangle]
pub(crate) static mut cs_count: i32 = 0;
#[no_mangle]
pub(crate) static mut prim: [b32x2; 501] = [b32x2 { s0: 0, s1: 0 }; 501];
#[no_mangle]
pub(crate) static mut prim_used: usize = 0;
#[no_mangle]
pub(crate) static mut prim_eqtb: [EqtbWord; 501] = [EqtbWord {
    lvl: 0,
    cmd: 0,
    val: 0,
}; 501];
#[no_mangle]
pub(crate) static mut SAVE_STACK: Vec<EqtbWord> = Vec::new();
#[no_mangle]
pub(crate) static mut SAVE_PTR: usize = 0;
#[no_mangle]
pub(crate) static mut MAX_SAVE_STACK: usize = 0;
#[no_mangle]
pub(crate) static mut cur_level: u16 = 0;
#[no_mangle]
pub(crate) static mut cur_group: GroupCode = GroupCode::BottomLevel;
#[no_mangle]
pub(crate) static mut cur_boundary: i32 = 0;
#[no_mangle]
pub(crate) static mut mag_set: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_cmd: Cmd = Cmd::Relax;
#[no_mangle]
pub(crate) static mut cur_chr: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_cs: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_tok: i32 = 0;
#[no_mangle]
pub(crate) static mut INPUT_STACK: Vec<input_state_t> = Vec::new();
#[no_mangle]
pub(crate) static mut INPUT_PTR: usize = 0;
#[no_mangle]
pub(crate) static mut MAX_IN_STACK: usize = 0;
#[no_mangle]
pub(crate) static mut cur_input: input_state_t = input_state_t {
    state: InputState::TokenList,
    index: Btl::Parameter,
    start: 0,
    loc: 0,
    limit: 0,
    name: 0,
    synctex_tag: 0,
};
#[no_mangle]
pub(crate) static mut IN_OPEN: usize = 0;
#[no_mangle]
pub(crate) static mut open_parens: i32 = 0;
#[no_mangle]
pub(crate) static mut INPUT_FILE: Vec<Option<Box<UFILE>>> = Vec::new();
#[no_mangle]
pub(crate) static mut line: i32 = 0;
#[no_mangle]
pub(crate) static mut LINE_STACK: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut SOURCE_FILENAME_STACK: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut FULL_SOURCE_FILENAME_STACK: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut scanner_status: ScannerStatus = ScannerStatus::Normal;
#[no_mangle]
pub(crate) static mut warning_index: i32 = 0;
#[no_mangle]
pub(crate) static mut def_ref: usize = 0;
#[no_mangle]
pub(crate) static mut PARAM_STACK: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut PARAM_PTR: usize = 0;
#[no_mangle]
pub(crate) static mut MAX_PARAM_STACK: usize = 0;
#[no_mangle]
pub(crate) static mut align_state: i32 = 0;
#[no_mangle]
pub(crate) static mut par_loc: i32 = 0;
#[no_mangle]
pub(crate) static mut par_token: i32 = 0;
#[no_mangle]
pub(crate) static mut force_eof: bool = false;
#[no_mangle]
pub(crate) static mut expand_depth_count: i32 = 0;
#[no_mangle]
pub(crate) static mut is_in_csname: bool = false;
#[no_mangle]
pub(crate) static mut cur_mark: [Option<usize>; 5] = [Some(0); 5];
#[no_mangle]
pub(crate) static mut long_state: u8 = 0;
#[no_mangle]
pub(crate) static mut pstack: [i32; 9] = [0; 9];
#[no_mangle]
pub(crate) static mut cur_order: GlueOrder = GlueOrder::Normal;

const NONE_UFILE: Option<Box<UFILE>> = None;
pub(crate) static mut read_file: [Option<Box<UFILE>>; 17] = [NONE_UFILE; 17];
#[no_mangle]
pub(crate) static mut read_open: [OpenMode; 17] = [OpenMode::Closed; 17];
#[no_mangle]
pub(crate) static mut cond_ptr: Option<usize> = None;
#[no_mangle]
pub(crate) static mut if_limit: FiOrElseCode = FiOrElseCode::Normal;
#[no_mangle]
pub(crate) static mut cur_if: i16 = 0;
#[no_mangle]
pub(crate) static mut if_line: i32 = 0;
#[no_mangle]
pub(crate) static mut skip_line: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_name: str_number = 0;
#[no_mangle]
pub(crate) static mut cur_area: str_number = 0;
#[no_mangle]
pub(crate) static mut cur_ext: str_number = 0;
#[no_mangle]
pub(crate) static mut TEX_format_default: String = String::new();
#[no_mangle]
pub(crate) static mut name_in_progress: bool = false;
#[no_mangle]
pub(crate) static mut job_name: str_number = 0;
#[no_mangle]
pub(crate) static mut log_opened: bool = false;
#[no_mangle]
pub(crate) static mut output_file_extension: String = String::new();
#[no_mangle]
pub(crate) static mut texmf_log_name: str_number = 0;
#[no_mangle]
pub(crate) static mut FONT_INFO: Vec<memory_word> = Vec::new();
#[no_mangle]
pub(crate) static mut fmem_ptr: font_index = 0;
#[no_mangle]
pub(crate) static mut FONT_PTR: usize = 0;
#[no_mangle]
pub(crate) static mut FONT_CHECK: Vec<b16x4> = Vec::new();

use crate::xetex_scaledmath::Scaled;
#[no_mangle]
pub(crate) static mut FONT_SIZE: Vec<Scaled> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_DSIZE: Vec<Scaled> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_PARAMS: Vec<font_index> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_NAME: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_AREA: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_BC: Vec<UTF16_code> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_EC: Vec<UTF16_code> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_GLUE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut font_used: Vec<bool> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPHEN_CHAR: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut SKEW_CHAR: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut BCHAR_LABEL: Vec<font_index> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_BCHAR: Vec<nine_bits> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_FALSE_BCHAR: Vec<nine_bits> = Vec::new();

use crate::xetex_ext::Font;

pub(crate) static mut FONT_LAYOUT_ENGINE: Vec<Font> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_MAPPING: Vec<*mut libc::c_void> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_FLAGS: Vec<i8> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_LETTER_SPACE: Vec<Scaled> = Vec::new();
#[no_mangle]
pub(crate) static mut loaded_font_mapping: *mut libc::c_void = ptr::null_mut();
#[no_mangle]
pub(crate) static mut loaded_font_flags: i8 = 0;
#[no_mangle]
pub(crate) static mut loaded_font_letter_space: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut loaded_font_design_size: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut mapped_text: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut CHAR_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut WIDTH_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut HEIGHT_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut DEPTH_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut ITALIC_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut LIG_KERN_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut KERN_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut EXTEN_BASE: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut PARAM_BASE: Vec<i32> = Vec::new();

pub(crate) const NULL_CHARACTER: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
#[no_mangle]
pub(crate) static mut TOTAL_PAGES: usize = 0;
#[no_mangle]
pub(crate) static mut max_v: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut max_h: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut max_push: i32 = 0;
#[no_mangle]
pub(crate) static mut last_bop: i32 = 0;
#[no_mangle]
pub(crate) static mut dead_cycles: i32 = 0;
#[no_mangle]
pub(crate) static mut doing_leaders: bool = false;
#[no_mangle]
pub(crate) static mut rule_ht: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut rule_dp: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut rule_wd: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut cur_h: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut cur_v: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut total_stretch: [Scaled; 4] = [Scaled::ZERO; 4];
#[no_mangle]
pub(crate) static mut total_shrink: [Scaled; 4] = [Scaled::ZERO; 4];
#[no_mangle]
pub(crate) static mut last_badness: i32 = 0;
#[no_mangle]
pub(crate) static mut adjust_tail: Option<usize> = Some(0);
#[no_mangle]
pub(crate) static mut pre_adjust_tail: Option<usize> = Some(0);
#[no_mangle]
pub(crate) static mut pack_begin_line: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_f: internal_font_number = 0;
#[no_mangle]
pub(crate) static mut cur_c: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_i: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
#[no_mangle]
pub(crate) static mut cur_align: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_span: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_loop: Option<usize> = None;
#[no_mangle]
pub(crate) static mut align_ptr: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_head: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_tail: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_pre_head: Option<usize> = None;
#[no_mangle]
pub(crate) static mut cur_pre_tail: Option<usize> = None;
#[no_mangle]
pub(crate) static mut just_box: usize = 0;

use crate::node::DeltaSize;
pub(crate) static mut active_width: DeltaSize = DeltaSize::new();
#[no_mangle]
pub(crate) static mut hc: [i32; 4099] = [0; 4099];
#[no_mangle]
pub(crate) static mut hf: internal_font_number = 0;
#[no_mangle]
pub(crate) static mut hu: [i32; 4097] = [0; 4097];
#[no_mangle]
pub(crate) static mut cur_lang: u8 = 0;
#[no_mangle]
pub(crate) static mut hyf: [u8; 4097] = [0; 4097];
#[no_mangle]
pub(crate) static mut init_list: i32 = 0;
#[no_mangle]
pub(crate) static mut init_lig: bool = false;
#[no_mangle]
pub(crate) static mut init_lft: bool = false;
#[no_mangle]
pub(crate) static mut hyphen_passed: i16 = 0;
#[no_mangle]
pub(crate) static mut cur_l: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_r: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_q: i32 = 0;
#[no_mangle]
pub(crate) static mut lig_stack: Option<usize> = Some(0);
#[no_mangle]
pub(crate) static mut ligature_present: bool = false;
#[no_mangle]
pub(crate) static mut lft_hit: bool = false;
#[no_mangle]
pub(crate) static mut rt_hit: bool = false;
#[no_mangle]
pub(crate) static mut HYPH_WORD: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_LIST: Vec<Option<usize>> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_LINK: Vec<hyph_pointer> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_COUNT: usize = 0;
#[no_mangle]
pub(crate) static mut HYPH_NEXT: usize = 0;
#[no_mangle]
pub(crate) static mut best_height_plus_depth: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut main_f: internal_font_number = 0;
#[no_mangle]
pub(crate) static mut main_i: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
#[no_mangle]
pub(crate) static mut main_j: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
#[no_mangle]
pub(crate) static mut main_k: font_index = 0;
#[no_mangle]
pub(crate) static mut main_h: i32 = 0;
#[no_mangle]
pub(crate) static mut is_hyph: bool = false;
#[no_mangle]
pub(crate) static mut space_class: i32 = 0;
#[no_mangle]
pub(crate) static mut prev_class: i32 = 0;
#[no_mangle]
pub(crate) static mut main_s: i32 = 0;
#[no_mangle]
pub(crate) static mut bchar: i32 = 0;
#[no_mangle]
pub(crate) static mut false_bchar: i32 = 0;
#[no_mangle]
pub(crate) static mut cancel_boundary: bool = false;
#[no_mangle]
pub(crate) static mut ins_disc: bool = false;
#[no_mangle]
pub(crate) static mut cur_box: Option<usize> = Some(0);
#[no_mangle]
pub(crate) static mut after_token: i32 = 0;
#[no_mangle]
pub(crate) static mut long_help_seen: bool = false;
#[no_mangle]
pub(crate) static mut format_ident: str_number = 0;
#[no_mangle]
pub(crate) static mut write_file: [Option<OutputHandleWrapper>; 16] = [
    None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None,
];
#[no_mangle]
pub(crate) static mut write_open: [bool; 18] = [false; 18];
#[no_mangle]
pub(crate) static mut write_loc: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_page_width: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut cur_page_height: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut cur_h_offset: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut cur_v_offset: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut pdf_last_x_pos: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut pdf_last_y_pos: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut EOF_SEEN: Vec<bool> = Vec::new();
#[no_mangle]
pub(crate) static mut LR_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut LR_problems: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_dir: LR = LR::LeftToRight;
#[no_mangle]
pub(crate) static mut pseudo_files: i32 = 0;
#[no_mangle]
pub(crate) static mut GRP_STACK: Vec<save_pointer> = Vec::new();
#[no_mangle]
pub(crate) static mut IF_STACK: Vec<Option<usize>> = Vec::new();
#[no_mangle]
pub(crate) static mut max_reg_num: i32 = 0;
#[no_mangle]
pub(crate) static mut max_reg_help_line: &str = "";
#[no_mangle]
pub(crate) static mut sa_root: [Option<usize>; 8] = [Some(0); 8];
#[no_mangle]
pub(crate) static mut cur_ptr: Option<usize> = Some(0);

#[no_mangle]
pub(crate) static mut sa_chain: i32 = 0;
#[no_mangle]
pub(crate) static mut sa_level: u16 = 0;
#[no_mangle]
pub(crate) static mut hyph_index: trie_pointer = 0;
#[no_mangle]
pub(crate) static mut disc_ptr: [i32; 4] = [0; 4];
#[no_mangle]
pub(crate) static mut edit_name_start: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut stop_at_space: bool = false;
#[no_mangle]
pub(crate) static mut xtx_ligature_present: bool = false;
#[no_mangle]
pub(crate) static mut delta: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut synctex_enabled: i32 = 0;
#[no_mangle]
pub(crate) static mut used_tectonic_coda_tokens: bool = false;
#[no_mangle]
pub(crate) static mut semantic_pagination_enabled: bool = false;
#[no_mangle]
pub(crate) static mut gave_char_warning_help: bool = false;
/* These ought to live in xetex-pagebuilder.c but are shared a lot: */
#[no_mangle]
pub(crate) static mut page_tail: usize = 0;
#[no_mangle]
pub(crate) static mut page_contents: PageContents = PageContents::Empty;
#[no_mangle]
pub(crate) static mut page_so_far: [Scaled; 8] = [Scaled::ZERO; 8];
#[no_mangle]
pub(crate) static mut last_glue: i32 = 0;
#[no_mangle]
pub(crate) static mut last_penalty: i32 = 0;
#[no_mangle]
pub(crate) static mut last_kern: Scaled = Scaled::ZERO;
#[no_mangle]
pub(crate) static mut last_node_type: i32 = 0;
#[no_mangle]
pub(crate) static mut insert_penalties: i32 = 0;
#[no_mangle]
pub(crate) static mut output_active: bool = false;
#[no_mangle]
pub(crate) static mut _xeq_level_array: [u16; 1114732] = [0; 1114732];
pub(crate) static mut yhash: *mut b32x2 = ptr::null_mut();

pub(crate) const hash_offset: i32 = 514;

unsafe fn primitive<I>(ident: &str, c: Cmd, o: I) -> i32
where
    I: std::convert::TryInto<i32>,
    <I as std::convert::TryInto<i32>>::Error: std::fmt::Debug,
{
    let o = o.try_into().unwrap();
    let mut prim_val = 0;
    let b_ident = ident.as_bytes();
    let len = b_ident.len() as i32;
    let mut val;
    if len > 1 {
        let mut s: str_number = maketexstring(ident);
        if first + len > BUF_SIZE as i32 + 1 {
            overflow("buffer size", BUF_SIZE);
        }
        for i in 0..len {
            BUFFER[(first + i) as usize] = b_ident[i as usize] as UnicodeScalar;
        }
        val = id_lookup(first, len);
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        (*hash.offset(val as isize)).s1 = s;
        prim_val = prim_lookup(s)
    } else {
        val = b_ident[0] as i32 + SINGLE_BASE as i32;
        prim_val = prim_lookup(b_ident[0] as str_number)
    }
    EQTB[val as usize].lvl = LEVEL_ONE;
    EQTB[val as usize].cmd = c as u16;
    EQTB[val as usize].val = o;
    prim_eqtb[prim_val as usize].lvl = LEVEL_ONE;
    prim_eqtb[prim_val as usize].cmd = c as u16;
    prim_eqtb[prim_val as usize].val = o;
    val
}

unsafe fn new_patterns(input: &mut input_state_t, cs: i32) {
    if trie_not_ready {
        if *INTPAR(IntPar::language) <= 0 {
            cur_lang = 0
        } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
            cur_lang = 0
        } else {
            cur_lang = *INTPAR(IntPar::language) as _;
        }
        scan_left_brace(input);
        let mut k = 0;
        hyf[0] = 0;
        let mut digit_sensed = false;
        loop {
            let (_, cmd, mut chr, _) = get_x_token(input);
            match cmd {
                Cmd::Letter | Cmd::OtherChar => {
                    if digit_sensed || chr < '0' as i32 || chr > '9' as i32 {
                        if chr == '.' as i32 {
                            chr = 0;
                        } else {
                            chr = *LC_CODE(chr as usize);
                            if chr == 0 {
                                if file_line_error_style_p != 0 {
                                    print_file_line();
                                } else {
                                    print_nl_cstr("! ");
                                }
                                print_cstr("Nonletter");
                                help!("(See Appendix H.)");
                                error();
                            }
                        }
                        if chr > max_hyph_char {
                            max_hyph_char = chr;
                        }
                        if (k as usize) < max_hyphenatable_length() {
                            k += 1;
                            hc[k as usize] = chr;
                            hyf[k as usize] = 0;
                            digit_sensed = false;
                        }
                    } else if (k as usize) < max_hyphenatable_length() {
                        hyf[k as usize] = (chr - 48) as u8;
                        digit_sensed = true;
                    }
                }
                Cmd::Spacer | Cmd::RightBrace => {
                    if k as i32 > 0 {
                        /*998:*/
                        if hc[1] == 0 {
                            hyf[0] = 0;
                        }
                        if hc[k as usize] == 0 {
                            hyf[k as usize] = 0;
                        }
                        let mut l = k;
                        let mut v = MIN_TRIE_OP;
                        loop {
                            if hyf[l as usize] != 0 {
                                v = new_trie_op(
                                    (k as i32 - l as i32) as i16,
                                    hyf[l as usize] as i16,
                                    v,
                                )
                            }
                            if !(l as i32 > 0) {
                                break;
                            }
                            l -= 1
                        }
                        let mut q = 0;
                        hc[0] = cur_lang as i32;
                        while l <= k {
                            let c = hc[l as usize] as UTF16_code;
                            l += 1;
                            let mut p = trie_l[q as usize];
                            let mut first_child = true;
                            while p > 0 && c > trie_c[p as usize] {
                                q = p;
                                p = trie_r[q as usize];
                                first_child = false
                            }
                            if p == 0 || c < trie_c[p as usize] {
                                /*999:*/
                                if trie_ptr == trie_size {
                                    overflow("pattern memory", trie_size as usize);
                                }
                                trie_ptr += 1;
                                trie_r[trie_ptr as usize] = p;
                                p = trie_ptr;
                                trie_l[p as usize] = 0;
                                if first_child {
                                    trie_l[q as usize] = p;
                                } else {
                                    trie_r[q as usize] = p;
                                }
                                trie_c[p as usize] = c;
                                trie_o[p as usize] = MIN_TRIE_OP;
                            }
                            q = p;
                        }
                        if trie_o[q as usize] != MIN_TRIE_OP {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr("! ");
                            }
                            print_cstr("Duplicate pattern");
                            help!("(See Appendix H.)");
                            error();
                        }
                        trie_o[q as usize] = v;
                    }
                    if cmd == Cmd::RightBrace {
                        break;
                    }
                    k = 0;
                    hyf[0] = 0;
                    digit_sensed = false
                }
                _ => {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr("! ");
                    }
                    print_cstr("Bad ");
                    print_esc_cstr("patterns");
                    help!("(See Appendix H.)");
                    error();
                }
            }
        }
        /*:996*/
        if *INTPAR(IntPar::saving_hyphs) > 0 {
            /*1643:*/
            let c = cur_lang as UTF16_code;
            let first_child = false;
            let mut p = 0;
            let mut q;
            loop {
                q = p;
                p = trie_r[q as usize];
                if p == 0 || c as i32 <= trie_c[p as usize] as i32 {
                    break;
                }
            }
            if p == 0 || (c as i32) < trie_c[p as usize] as i32 {
                /*:1644*/
                /*999:*/
                if trie_ptr == trie_size {
                    overflow("pattern memory", trie_size as usize);
                }
                trie_ptr += 1;
                trie_r[trie_ptr as usize] = p;
                p = trie_ptr;
                trie_l[p as usize] = 0;
                if first_child {
                    trie_l[q as usize] = p;
                } else {
                    trie_r[q as usize] = p;
                }
                trie_c[p as usize] = c;
                trie_o[p as usize] = MIN_TRIE_OP;
            }
            let mut q = p;
            let mut p = trie_l[q as usize];
            let mut first_child = true;
            let mut c = 0 as UTF16_code;
            while c as i32 <= 255 {
                if *LC_CODE(c as _) > 0 || c == 255 && first_child {
                    if p == 0 {
                        /*999:*/
                        if trie_ptr == trie_size {
                            overflow("pattern memory", trie_size as usize);
                            /*:987 */
                        }
                        trie_ptr += 1;
                        trie_r[trie_ptr as usize] = p;
                        p = trie_ptr;
                        trie_l[p as usize] = 0;
                        if first_child {
                            trie_l[q as usize] = p;
                        } else {
                            trie_r[q as usize] = p;
                        }
                        trie_c[p as usize] = c;
                        trie_o[p as usize] = MIN_TRIE_OP;
                    } else {
                        trie_c[p as usize] = c;
                    }
                    trie_o[p as usize] = *LC_CODE(c as _) as _;
                    q = p;
                    p = trie_r[q as usize];
                    first_child = false
                }
                c = c.wrapping_add(1)
            }
            if first_child {
                trie_l[q as usize] = 0;
            } else {
                trie_r[q as usize] = 0;
            }
        }
    } else {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Too late for ");
        print_esc_cstr("patterns");
        help!("All patterns must be given before typesetting begins.");
        error();
        *LLIST_link(GARBAGE) = scan_toks(input, cs, false, false) as i32;
        flush_list(Some(def_ref));
    };
}
/*:1001*/
unsafe fn new_hyph_exceptions(input: &mut input_state_t) {
    scan_left_brace(input);

    cur_lang = if *INTPAR(IntPar::language) <= 0 {
        0
    } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
        0
    } else {
        *INTPAR(IntPar::language) as _
    };

    hyph_index = if trie_not_ready {
        0
    } else if trie_trc[(hyph_start + cur_lang as i32) as usize] as i32 != cur_lang as i32 {
        0
    } else {
        trie_trl[(hyph_start + cur_lang as i32) as usize]
    };

    /*970: not_found:*/
    let mut n = 0_i16;
    let mut p = None;

    let mut reswitch = None;
    loop {
        let (cmd, chr) = reswitch.take().unwrap_or_else(|| {
            let next = get_x_token(input);
            (next.1, next.2)
        });

        match cmd {
            Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {
                if chr == '-' as i32 {
                    /*973:*/
                    if (n as usize) < max_hyphenatable_length() {
                        let q = get_avail();
                        *LLIST_link(q) = p.tex_int();
                        MEM[q].b32.s0 = n as i32;
                        p = Some(q);
                    }
                } else {
                    hc[0] = if hyph_index == 0 || chr > 255 {
                        *LC_CODE(chr as usize) as _
                    } else if trie_trc[(hyph_index + chr) as usize] as i32 != chr {
                        0
                    } else {
                        trie_tro[(hyph_index + chr) as usize]
                    };
                    if hc[0] == 0 {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Not a letter");
                        help!(
                            "Letters in \\hyphenation words must have \\lccode>0.",
                            "Proceed; I\'ll ignore the character I just read."
                        );
                        error();
                    } else if (n as usize) < max_hyphenatable_length() {
                        n += 1;
                        if (hc[0] as i64) < 0x1_0000 {
                            hc[n as usize] = hc[0]
                        } else {
                            hc[n as usize] =
                                ((hc[0] as i64 - 0x1_0000) / 1024 as i64 + 0xd800) as i32;
                            n += 1;
                            hc[n as usize] = ((hc[0] % 1024) as i64 + 0xdc00) as i32
                        }
                    }
                }
            }
            Cmd::CharNum => {
                reswitch = Some((Cmd::CharGiven, scan_char_num(input)));
                continue;
            }
            Cmd::Spacer | Cmd::RightBrace => {
                if n > 1 {
                    /*974:*/
                    n += 1;
                    hc[n as usize] = cur_lang as i32;
                    if pool_ptr + n as i32 > pool_size {
                        overflow("pool size", (pool_size - init_pool_ptr) as usize);
                    }
                    let mut h = 0;

                    for j in 1..=(n as usize) {
                        h = ((h as i32 + h as i32 + hc[j]) % HYPH_PRIME) as hyph_pointer;
                        str_pool[pool_ptr as usize] = hc[j] as packed_UTF16_code;
                        pool_ptr += 1;
                    }

                    let mut s = make_string();

                    if HYPH_NEXT <= HYPH_PRIME as usize {
                        while HYPH_NEXT > 0 && HYPH_WORD[HYPH_NEXT - 1] > 0 {
                            HYPH_NEXT -= 1;
                        }
                    }

                    if HYPH_COUNT == HYPH_SIZE || HYPH_NEXT == 0 {
                        overflow("exception dictionary", HYPH_SIZE);
                    }

                    HYPH_COUNT += 1;

                    while HYPH_WORD[h as usize] != 0 {
                        let mut k = HYPH_WORD[h as usize];
                        let mut not_found = false;
                        if length(k) == length(s) {
                            let mut u = str_start[(k as i64 - 65536) as usize];
                            let mut v = str_start[(s as i64 - 65536) as usize];
                            loop {
                                if str_pool[u as usize] as i32 != str_pool[v as usize] as i32 {
                                    not_found = true;
                                    break;
                                }
                                u += 1;
                                v += 1;
                                if u == str_start[((k + 1i32) as i64 - 65536) as usize] {
                                    break;
                                }
                            }
                            if !not_found {
                                str_ptr -= 1;
                                pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                                s = HYPH_WORD[h as usize];
                                HYPH_COUNT -= 1;
                                break;
                            }
                        }
                        /*:975*/
                        /*:976*/
                        // not_found
                        if HYPH_LINK[h as usize] == 0 {
                            HYPH_LINK[h as usize] = HYPH_NEXT as hyph_pointer;
                            if HYPH_NEXT >= HYPH_SIZE {
                                HYPH_NEXT = HYPH_PRIME as usize;
                            }
                            if HYPH_NEXT > HYPH_PRIME as usize {
                                HYPH_NEXT += 1;
                            }
                        }
                        h = (HYPH_LINK[h as usize] as i32 - 1) as hyph_pointer;
                    }

                    // found
                    HYPH_WORD[h as usize] = s;
                    HYPH_LIST[h as usize] = p;
                }

                if cmd == Cmd::RightBrace {
                    return;
                }

                n = 0;
                p = None;
            }
            _ => {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Improper ");
                print_esc_cstr("hyphenation");
                print_cstr(" will be flushed");
                help!(
                    "Hyphenation exceptions must contain only letters",
                    "and hyphens. But continue; I\'ll forgive and forget."
                );
                error();
            }
        }
    }
}
pub(crate) unsafe fn prefixed_command(
    input: &mut input_state_t,
    mut ocmd: Cmd,
    mut ochr: i32,
    mut ocs: i32,
) {
    let mut f: internal_font_number = 0;
    let mut j: i32 = 0;
    let mut k: font_index = 0;
    let mut e: bool = false;

    let mut a = 0 as i16;
    while ocmd == Cmd::Prefix {
        if a as i32 / ochr & 1i32 == 0 {
            a = (a as i32 + ochr) as i16
        }
        let mut next;
        loop {
            next = get_x_token(input);
            if !(next.1 == Cmd::Spacer || next.1 == Cmd::Relax) {
                break;
            }
        }
        let tok = next.0;
        ocmd = next.1;
        ochr = next.2;
        ocs = next.3;
        if ocmd <= MAX_NON_PREFIXED_COMMAND {
            /*1247:*/
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("You can\'t use a prefix with `");
            print_cmd_chr(ocmd, ochr);
            print_chr('\'');
            help!("I\'ll pretend you didn\'t say \\long or \\outer or \\global or \\protected.");
            back_error(input, tok);
            return;
        }
        if *INTPAR(IntPar::tracing_commands) > 2 {
            show_cur_cmd_chr(ocmd, ochr);
        }
    }
    if a >= 8 {
        j = PROTECTED_TOKEN;
        a -= 8;
    } else {
        j = 0;
    }

    if ocmd != Cmd::Def && (a % 4 != 0 || j != 0) {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("You can\'t use `");
        print_esc_cstr("long");
        print_cstr("\' or `");
        print_esc_cstr("outer");
        help!("I\'ll pretend you didn\'t say \\long or \\outer or \\protected here.");
        print_cstr("\' or `");
        print_esc_cstr("protected");
        print_cstr("\' with `");
        print_cmd_chr(ocmd, ochr);
        print_chr('\'');
        error();
    }

    if *INTPAR(IntPar::global_defs) != 0 {
        if *INTPAR(IntPar::global_defs) < 0 {
            if a >= 4 {
                a -= 4;
            }
        } else if a < 4 {
            a += 4;
        }
    }
    match ocmd {
        Cmd::SetFont => {
            /*1252:*/
            if a >= 4 {
                geq_define(CUR_FONT_LOC, Cmd::Data, ochr.opt());
            } else {
                eq_define(CUR_FONT_LOC, Cmd::Data, ochr.opt());
            }
        }
        Cmd::Def => {
            if ochr & 1i32 != 0 && (a as i32) < 4i32 && *INTPAR(IntPar::global_defs) >= 0 {
                a = (a as i32 + 4i32) as i16
            }
            e = ochr >= 2;
            let p = get_r_token(input).3;
            let _q = scan_toks(input, p, true, e) as i32;
            if j != 0 {
                let q = get_avail();
                MEM[q].b32.s0 = j;
                *LLIST_link(q) =
                    *LLIST_link(def_ref);
                *LLIST_link(def_ref) = Some(q).tex_int();
            }
            if a >= 4 {
                geq_define(p as usize, Cmd::from(Cmd::Call as u16 + (a % 4) as u16),
                           Some(def_ref));
            } else {
                eq_define(p as usize, Cmd::from(Cmd::Call as u16 + (a % 4) as u16),
                          Some(def_ref));
            }
        }
        Cmd::Let => {
            let n = ochr;
            let (_, mut cmd, mut chr, p) = get_r_token(input);
            if n == NORMAL as i32 {
                let mut next;
                loop  {
                    next = get_token(input);
                    if next.1 != Cmd::Spacer { break ; }
                }
                cmd = next.1;
                chr = next.2;
                if next.0 == OTHER_TOKEN + '=' as i32 {
                    let next = get_token(input);
                    cmd = next.1;
                    chr = next.2;
                    if cmd == Cmd::Spacer {
                        let next = get_token(input);
                        cmd = next.1;
                        chr = next.2;
                    }
                }
            } else {
                let q = get_token(input).0;
                let next = get_token(input);
                cmd = next.1;
                chr = next.2;
                back_input(input, next.0);
                back_input(input, q);
            }
            if cmd >= Cmd::Call {
                MEM[chr as usize].b32.s0 += 1
            } else if cmd == Cmd::Register ||
                          cmd == Cmd::ToksRegister {
                if chr < 0 || chr > 19 {
                    /* 19 = lo_mem_stat_max, I think */
                    MEM[(chr + 1) as usize].b32.s0 += 1;
                }
            }
            if a >= 4 {
                geq_define(p as usize, cmd, chr.opt());
            } else { eq_define(p as usize, cmd, chr.opt()); }
        }
        Cmd::ShorthandDef => {
            let mut n = ShorthandDefCode::n(ochr as u8).unwrap();
            if n == ShorthandDefCode::CharSub {
                let val = scan_char_num(input);
                let p = CHAR_SUB_CODE_BASE as i32 + val;
                scan_optional_equals(input);
                let mut n = scan_char_num(input);
                let val = scan_char_num(input);
                if *INTPAR(IntPar::tracing_char_sub_def) > 0 {
                    diagnostic(false, || {
                        print_nl_cstr("New character substitution: ");
                        print(p - CHAR_SUB_CODE_BASE as i32);
                        print_cstr(" = ");
                        print(n);
                        print_chr(' ');
                        print(val);
                    });
                }
                n = n * 256 + val;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, Some(n as usize));
                } else { eq_define(p as usize, Cmd::Data, Some(n as usize)); }
                if (p - CHAR_SUB_CODE_BASE as i32) < *INTPAR(IntPar::char_sub_def_min) {
                    if a >= 4 {
                        geq_word_define(INT_BASE as usize + IntPar::char_sub_def_min as usize, p - CHAR_SUB_CODE_BASE as i32);
                    } else {
                        eq_word_define(INT_BASE as usize + IntPar::char_sub_def_min as usize, p - CHAR_SUB_CODE_BASE as i32);
                    }
                }
                if (p - CHAR_SUB_CODE_BASE as i32) < *INTPAR(IntPar::char_sub_def_max) {
                    if a >= 4 {
                        geq_word_define(INT_BASE as usize + IntPar::char_sub_def_max as usize, p - CHAR_SUB_CODE_BASE as i32);
                    } else {
                        eq_word_define(INT_BASE as usize + IntPar::char_sub_def_max as usize, p - CHAR_SUB_CODE_BASE as i32);
                    }
                }
            } else {
                let p = get_r_token(input).3;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Relax, Some(TOO_BIG_USV));
                } else {
                    eq_define(p as usize, Cmd::Relax, Some(TOO_BIG_USV));
                }
                scan_optional_equals(input);
                match n {
                    ShorthandDefCode::Char => {
                        let val = scan_usv_num(input);
                        if a >= 4 {
                            geq_define(p as usize, Cmd::CharGiven, val.opt());
                        } else { eq_define(p as usize, Cmd::CharGiven, val.opt()); }
                    }
                    ShorthandDefCode::MathChar => {
                        let val = scan_fifteen_bit_int(input);
                        if a >= 4 {
                            geq_define(p as usize, Cmd::MathGiven, val.opt());
                        } else { eq_define(p as usize, Cmd::MathGiven, val.opt()); }
                    }
                    ShorthandDefCode::XetexMathCharNum => {
                        let val = scan_xetex_math_char_int(input);
                        if a >= 4 {
                            geq_define(p as usize, Cmd::XetexMathGiven, val.opt());
                        } else { eq_define(p as usize, Cmd::XetexMathGiven, val.opt()); }
                    }
                    ShorthandDefCode::XetexMathChar => {
                        let val = scan_math_class_int(input);
                        let mut n = set_class(val);
                        let val = scan_math_fam_int(input);
                        n += set_family(val);
                        let val = scan_usv_num(input);
                        n += val;
                        if a >= 4 {
                            geq_define(p as usize, Cmd::XetexMathGiven, n.opt());
                        } else { eq_define(p as usize, Cmd::XetexMathGiven, n.opt()); }
                    }
                    _ => {
                        let val = scan_register_num(input);
                        if val > 255 {
                            j = (n as i32) - 2; // TODO
                            if j > ValLevel::Mu as i32 { j = ValLevel::Tok as i32 }

                            find_sa_element(ValLevel::from(j as u8), val,
                                            true);
                            let c = cur_ptr.unwrap();
                            MEM[c + 1].b32.s0 += 1;

                            let j = if j == ValLevel::Tok as i32 { Cmd::ToksRegister } else { Cmd::Register };
                            if a >= 4 {
                                geq_define(p as usize, j, Some(c));
                            } else { eq_define(p as usize, j, Some(c)); }
                        } else {
                            match n {
                                ShorthandDefCode::Count => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignInt, Some(COUNT_BASE + val as usize));
                                    } else {
                                        eq_define(p as usize, Cmd::AssignInt, Some(COUNT_BASE + val as usize));
                                    }
                                }
                                ShorthandDefCode::Dimen => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignDimen, Some(SCALED_BASE + val as usize));
                                    } else {
                                        eq_define(p as usize, Cmd::AssignDimen, Some(SCALED_BASE + val as usize));
                                    }
                                }
                                ShorthandDefCode::Skip => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignGlue, Some(SKIP_BASE + val as usize));
                                    } else {
                                        eq_define(p as usize, Cmd::AssignGlue, Some(SKIP_BASE + val as usize));
                                    }
                                }
                                ShorthandDefCode::MuSkip => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignMuGlue, Some(MU_SKIP_BASE + val as usize));
                                    } else {
                                        eq_define(p as usize, Cmd::AssignMuGlue, Some(MU_SKIP_BASE + val as usize));
                                    }
                                }
                                ShorthandDefCode::Toks => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignToks, Some(TOKS_BASE + val as usize));
                                    } else {
                                        eq_define(p as usize, Cmd::AssignToks, Some(TOKS_BASE + val as usize));
                                    }
                                }
                                _ => { }
                            }
                        }
                    }
                }
            }
        }
        Cmd::ReadToCS => {
            j = ochr;
            let n = scan_int(input);
            if !scan_keyword(input, b"to") {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Missing `to\' inserted");
                help!("You should have said `\\read<number> to \\cs\'.", "I\'m going to look for the \\cs now.");
                error();
            }
            let p = get_r_token(input).3;
            let val = read_toks(input, n, p, j);
            if a >= 4 {
                geq_define(p as usize, Cmd::Call, val.opt());
            } else { eq_define(p as usize, Cmd::Call, val.opt()); }
        }
        Cmd::ToksRegister | Cmd::AssignToks => {
            let mut q = ocs;
            e = false;
            if ocmd == Cmd::ToksRegister {
                if ochr == 0 {
                    let val = scan_register_num(input);
                    if val > 255 {
                        find_sa_element(ValLevel::Tok, val,
                                        true);
                        ochr = cur_ptr.tex_int();
                        e = true
                    } else {
                        ochr = TOKS_BASE as i32 + val;
                    }
                } else { e = true }
            } else if ochr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                cur_ptr = scan_char_class_not_ignored(input).opt();
                let val = scan_char_class_not_ignored(input);
                find_sa_element(ValLevel::InterChar,
                                cur_ptr.tex_int() * CHAR_CLASS_LIMIT + val, true);
                ochr = cur_ptr.tex_int();
                e = true
            }
            let p = ochr;
            scan_optional_equals(input);
            let mut next;
            loop  {
                next = get_x_token(input);
                if !(next.1 == Cmd::Spacer ||
                         next.1 == Cmd::Relax) {
                    break ;
                }
            }
            let (tok, cmd, chr, _) = next;
            if cmd != Cmd::LeftBrace {
                /*1262:*/
                if cmd == Cmd::ToksRegister ||
                       cmd == Cmd::AssignToks {
                    let q = if cmd == Cmd::ToksRegister {
                        if chr == 0 {
                            let val = scan_register_num(input); /* "extended delimiter code flag" */
                            (if val < 256 {
                                TOKS_REG(val as usize).opt()
                            } else {
                                find_sa_element(ValLevel::Tok, val,
                                                false); /* "extended delimiter code family */
                                cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt())
                            }).tex_int()
                        } else {
                            MEM[(chr + 1) as
                                                 usize].b32.s1
                        }
                    } else if chr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                        cur_ptr = scan_char_class_not_ignored(input).opt(); /*:1268 */
                        let val = scan_char_class_not_ignored(input);
                        find_sa_element(ValLevel::InterChar,
                                        cur_ptr.tex_int() * CHAR_CLASS_LIMIT + val,
                                        false);
                        cur_ptr.and_then(|p| MEM[p + 1].b32.s1.opt()).tex_int()
                    } else { EQTB[chr as usize].val };

                    if let Some(q) = q.opt() {
                        MEM[q].b32.s0 += 1;
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, Some(q));
                            } else { sa_def(p as usize, Some(q)); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::Call, Some(q));
                        } else { eq_define(p as usize, Cmd::Call, Some(q)); }
                    } else {
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, None);
                            } else { sa_def(p as usize, None); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::UndefinedCS, None);
                        } else {
                            eq_define(p as usize, Cmd::UndefinedCS, None);
                        }
                    }
                    return done(input);
                }
            }

            back_input(input, tok);
            let q = scan_toks(input, q, false, false);

            if llist_link(def_ref).is_none() {
                if e {
                    if a >= 4 {
                        gsa_def(p as usize, None);
                    } else { sa_def(p as usize, None); }
                } else if a >= 4 {
                    geq_define(p as usize, Cmd::UndefinedCS, None);
                } else {
                    eq_define(p as usize, Cmd::UndefinedCS, None);
                }
                *LLIST_link(def_ref) = avail.tex_int();
                avail = Some(def_ref);
            } else {
                if p == LOCAL_BASE as i32 + Local::output_routine as i32 && !e {
                    let v = get_avail();
                    *LLIST_link(q as usize) = Some(v).tex_int();
                    MEM[v].b32.s0 =
                        RIGHT_BRACE_TOKEN + 125;
                    let q = get_avail();
                    MEM[q].b32.s0 =
                        LEFT_BRACE_TOKEN + 123;
                    *LLIST_link(q) =
                        *LLIST_link(def_ref);
                    *LLIST_link(def_ref) = Some(q).tex_int();
                }
                if e {
                    if a >= 4 {
                        gsa_def(p as usize, Some(def_ref));
                    } else { sa_def(p as usize, Some(def_ref)); }
                } else if a >= 4 {
                    geq_define(p as usize, Cmd::Call, Some(def_ref));
                } else { eq_define(p as usize, Cmd::Call, Some(def_ref)); }
            }
        }
        Cmd::AssignInt => {
            let p = ochr as usize;
            scan_optional_equals(input);
            let val = scan_int(input);
            if a >= 4 {
                geq_word_define(p, val);
            } else { eq_word_define(p, val); }
        }
        Cmd::AssignDimen => {
            let p = ochr as usize;
            scan_optional_equals(input);
            let val = scan_dimen(input, false, false, None);
            if a >= 4 {
                geq_word_define(p, val.0);
            } else { eq_word_define(p, val.0); }
        }
        Cmd::AssignGlue | Cmd::AssignMuGlue => {
            let p = ochr as usize;
            let n = ocmd;
            scan_optional_equals(input);
            let val = if n == Cmd::AssignMuGlue {
                scan_glue(input, ValLevel::Mu)
            } else {
                scan_glue(input, ValLevel::Glue)
            };
            let val = trap_zero_glue(val);
            if a >= 4 {
                geq_define(p, Cmd::GlueRef, val.opt());
            } else { eq_define(p, Cmd::GlueRef, val.opt()); }
        }
        Cmd::XetexDefCode => {
            if ochr == SF_CODE_BASE as i32 {
                let p = ochr;
                let val = scan_usv_num(input);
                let p = p + val;
                let n = *SF_CODE(val as usize) % 65536;
                scan_optional_equals(input);
                let val = scan_char_class(input);
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data,
                               ((val as i64 * 65536 +
                                    n as i64) as i32).opt());
                } else {
                    eq_define(p as usize, Cmd::Data,
                              ((val as i64 * 65536 +
                                   n as i64) as i32).opt());
                }
            } else if ochr == MATH_CODE_BASE as i32 {
                let p = ochr;
                let val = scan_usv_num(input);
                let p = p + val;
                scan_optional_equals(input);
                let val = scan_xetex_math_char_int(input);
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, val.opt());
                } else { eq_define(p as usize, Cmd::Data, val.opt()); }
            } else if ochr == MATH_CODE_BASE as i32 + 1 {
                let p = ochr - 1;
                let val = scan_usv_num(input);
                let p = p + val;
                scan_optional_equals(input);
                let val = scan_math_class_int(input);
                let mut n = set_class(val);
                let val = scan_math_fam_int(input);
                n = n + set_family(val);
                let val = scan_usv_num(input);
                n = n + val;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, n.opt());
                } else { eq_define(p as usize, Cmd::Data, n.opt()); }
            } else if ochr == DEL_CODE_BASE as i32 {
                let p = ochr;
                let val = scan_usv_num(input);
                let p = p + val;
                scan_optional_equals(input);
                let val = scan_int(input);
                if a >= 4 {
                    geq_word_define(p as usize, val);
                } else { eq_word_define(p as usize, val); }
            } else {
                let p = ochr - 1;
                let val = scan_usv_num(input);
                let p = p + val;
                scan_optional_equals(input);
                let mut n = 0x40000000;
                let val = scan_math_fam_int(input);
                n = n + val * 0x200000;
                let val = scan_usv_num(input);
                n = n + val;
                if a >= 4 {
                    geq_word_define(p as usize, n);
                } else { eq_word_define(p as usize, n); }
            }
        }
        Cmd::DefCode => {
            let n = if ochr == CAT_CODE_BASE as i32 {
                MAX_CHAR_CODE
            } else if ochr == MATH_CODE_BASE as i32 {
                0x8000
            } else if ochr == SF_CODE_BASE as i32 {
                0x7fff
            } else if ochr == DEL_CODE_BASE as i32 {
                0xffffff
            } else { BIGGEST_USV as i32 }; // :1268

            let p = ochr;
            let val = scan_usv_num(input);
            let p = p + val;
            scan_optional_equals(input);
            let val = scan_int(input);

            let val = if (val < 0 && p < DEL_CODE_BASE as i32) || val > n {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Invalid code (");
                print_int(val);
                if p < MATH_CODE_BASE as i32 {
                    print_cstr("), should be in the range 0..");
                } else {
                    print_cstr("), should be at most ");
                }
                print_int(n);
                help!("I\'m going to use 0 instead of that illegal code value.");
                error();
                0
            } else {
                val
            };

            if p < MATH_CODE_BASE as i32 {
                if p >= SF_CODE_BASE as i32 {
                    let n =
                        (EQTB[p as usize].val as i64 /
                             65536) as i32;
                    if a >= 4 {
                        geq_define(p as usize, Cmd::Data,
                                   ((n as i64 * 65536 +
                                        val as i64) as i32).opt());
                    } else {
                        eq_define(p as usize, Cmd::Data,
                                  ((n as i64 * 65536 +
                                       val as i64) as i32).opt());
                    }
                } else if a >= 4 {
                    geq_define(p as usize, Cmd::Data, val.opt());
                } else { eq_define(p as usize, Cmd::Data, val.opt()); }

            } else if p < DEL_CODE_BASE as i32 {
                let val = if val as i64 == 32768 {
                    ACTIVE_MATH_CHAR
                } else {
                    set_class(val / 4096) + set_family((val % 4096) / 256) + (val % 256)
                };
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, val.opt());
                } else { eq_define(p as usize, Cmd::Data, val.opt()); }
            } else if a >= 4 {
                geq_word_define(p as usize, val);
            } else { eq_word_define(p as usize, val); }
        }
        Cmd::DefFamily => {
            let p = ochr;
            let val = scan_math_fam_int(input);
            let p = p + val;
            scan_optional_equals(input);
            let val = scan_font_ident(input);
            if a >= 4 {
                geq_define(p as usize, Cmd::Data, val.opt());
            } else { eq_define(p as usize, Cmd::Data, val.opt()); }
        }
        Cmd::Register | Cmd::Advance | Cmd::Multiply | Cmd::Divide => { do_register_command(input, ocmd, ochr, a); }
        Cmd::SetBox => {
            let val = scan_register_num(input);
            let n = if a >= 4 {
                GLOBAL_BOX_FLAG + val
            } else { BOX_FLAG + val };
            scan_optional_equals(input);
            if set_box_allowed {
                scan_box(input, n);
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Improper ");
                print_esc_cstr("setbox");
                help!("Sorry, \\setbox is not allowed after \\halign in a display,", "or between \\accent and an accented character.");
                error();
            }
        }
        Cmd::SetAux => { alter_aux(input, ocmd, ochr); }
        Cmd::SetPrevGraf => { alter_prev_graf(input); }
        Cmd::SetPageDimen => { alter_page_so_far(input, ochr); }
        Cmd::SetPageInt => { alter_integer(input, ochr); }
        Cmd::SetBoxDimen => { alter_box_dimen(input, ochr); }
        Cmd::SetShape => {
            let q = ochr;
            scan_optional_equals(input);
            let val = scan_int(input);
            let mut n = val;
            let p = if n <= 0 {
                None
            } else if q > LOCAL_BASE as i32 + Local::par_shape as i32 {
                n = val / 2 + 1;
                let p = get_node(2 * n + 1);
                MEM[p].b32.s0 = n;
                n = val;
                MEM[p + 1].b32.s1 = n;

                for j in (p + 2)..=(p + (n as usize) + 1) {
                    let val = scan_int(input);
                    MEM[j].b32.s1 = val;
                }

                if n & 1 == 0 {
                    MEM[p + (n as usize) + 2].b32.s1 = 0
                }
                Some(p)
            } else {
                let p = get_node(2 * n + 1);
                MEM[p].b32.s0 = n;
    
                for j in 1..=(n as usize) {
                    let val = scan_dimen(input, false, false, None);
                    MEM[p + 2 * j - 1].b32.s1 = val.0;
                    let val = scan_dimen(input, false, false, None);
                    MEM[p + 2 * j].b32.s1 = val.0;
                }
                Some(p)
            };
            if a >= 4 {
                geq_define(q as usize, Cmd::ShapeRef, p);
            } else { eq_define(q as usize, Cmd::ShapeRef, p); }
        }
        Cmd::HyphData => {
            if ochr == 1 {
                if in_initex_mode {
                    new_patterns(input, ocs);
                    return done(input);
                }

                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Patterns can be loaded only by INITEX");
                help!();
                error();
                loop  {
                    let cmd = get_token(input).1;
                    if cmd == Cmd::RightBrace { break ; }
                }
                return;
            } else { new_hyph_exceptions(input); }
        }
        Cmd::AssignFontDimen => {
            k = find_font_dimen(input, true);
            scan_optional_equals(input);
            let val = scan_dimen(input, false, false, None);
            FONT_INFO[k as usize].b32.s1 = val.0;
        }
        Cmd::AssignFontInt => {
            let n = AssignFontInt::from(ochr);
            f = scan_font_ident(input) as usize;
            match n {
                AssignFontInt::HyphenChar | AssignFontInt::SkewChar => {
                    scan_optional_equals(input);
                    let val = scan_int(input);
                    if n == AssignFontInt::HyphenChar {
                        HYPHEN_CHAR[f] = val
                    } else { SKEW_CHAR[f] = val }
                }
                _ => {
                    let p = if let Font::Native(nf) = &FONT_LAYOUT_ENGINE[f] {
                        scan_glyph_number(input, nf)
                    } else { scan_char_num(input) };
                    scan_optional_equals(input);
                    let val = scan_int(input);
                    match n {
                        AssignFontInt::LpCode => { set_cp_code(f, p as u32, Side::Left, val); }
                        AssignFontInt::RpCode => { set_cp_code(f, p as u32, Side::Right, val); }
                        _ => unreachable!(),
                    }
                }
            }
        }
        Cmd::DefFont => { new_font(input, a); }
        Cmd::SetInteraction => { new_interaction(ochr); }
        _ => { confusion("prefix"); }
    }

    /*1304:*/
    unsafe fn done(input: &mut input_state_t) {
        if after_token != 0 {
            let tok = after_token;
            back_input(input, tok);
            after_token = 0;
        }
    }

    done(input)
}

unsafe fn final_cleanup(input: &mut input_state_t) {
    let mut c = cur_chr as usize;
    if job_name == 0 {
        open_log_file();
    }
    while INPUT_PTR > 0 {
        if input.state == InputState::TokenList {
            end_token_list(input);
        } else {
            end_file_reading(input);
        }
    }
    while open_parens > 0 {
        print_cstr(" )");
        open_parens -= 1;
    }
    if cur_level > LEVEL_ONE {
        print_nl('(' as i32);
        print_esc_cstr("end occurred ");
        print_cstr("inside a group at level ");
        print_int(cur_level as i32 - 1);
        print_chr(')');
        show_save_groups(cur_group, cur_level);
    }
    while let Some(cp) = cond_ptr {
        print_nl('(' as i32);
        print_esc_cstr("end occurred ");
        print_cstr("when ");
        print_cmd_chr(Cmd::IfTest, cur_if as i32);
        if if_line != 0 {
            print_cstr(" on line ");
            print_int(if_line);
        }
        print_cstr(" was incomplete)");
        if_line = MEM[cp + 1].b32.s1;
        cur_if = MEM[cp].b16.s0 as i16;
        let tmp_ptr = cp;
        cond_ptr = llist_link(cp);
        free_node(tmp_ptr, IF_NODE_SIZE);
    }
    if history != TTHistory::SPOTLESS {
        if history == TTHistory::WARNING_ISSUED || interaction != InteractionMode::ErrorStop {
            if selector == Selector::TERM_AND_LOG {
                selector = Selector::TERM_ONLY;
                print_nl_cstr("(see the transcript file for additional information)");
                selector = Selector::TERM_AND_LOG
            }
        }
    }
    if c == 1 {
        if in_initex_mode {
            for c in TOP_MARK_CODE..=SPLIT_BOT_MARK_CODE {
                if let Some(m) = cur_mark[c] {
                    delete_token_ref(m);
                }
            }
            if let Some(m) = sa_root[ValLevel::Mark as usize] {
                if do_marks(MarkMode::DestroyMarks, 0, m) {
                    sa_root[ValLevel::Mark as usize] = None;
                }
            }
            for c in LAST_BOX_CODE..=VSPLIT_CODE {
                flush_node_list(disc_ptr[c as usize].opt());
            }
            if last_glue != MAX_HALFWORD {
                delete_glue_ref(last_glue as usize);
            }
            store_fmt_file();
            return;
        }
        print_nl_cstr("(\\dump is performed only by INITEX)");
        return;
    };
}
/* Engine initialization */
unsafe fn init_io(input: &mut input_state_t) {
    /* This is largely vestigial at this point */
    INPUT_FILE[0] = Some(Box::new(UFILE {
        handle: None,
        savedChar: -1,
        skipNextLF: 0,
        encodingMode: UnicodeMode::Utf8,
        conversionData: 0 as *mut libc::c_void,
    }));

    BUFFER[first as usize] = 0;
    last = first;
    input.loc = first;
    input.limit = last;
    first = last + 1;
}
unsafe fn initialize_more_variables() {
    doing_special = false;
    native_text_size = 128;
    native_text = xmalloc(
        (native_text_size as u64).wrapping_mul(::std::mem::size_of::<UTF16_code>() as _) as _,
    ) as *mut UTF16_code;

    interaction = InteractionMode::ErrorStop;

    deletions_allowed = true;
    set_box_allowed = true;
    error_count = 0_i8;
    help_ptr = 0_u8;
    use_err_help = false;
    NEST_PTR = 0;
    MAX_NEST_STACK = 0;
    cur_list.mode = (false, ListMode::VMode);
    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    cur_list.eTeX_aux = None;
    cur_list.aux.b32.s1 = IGNORE_DEPTH;
    cur_list.mode_line = 0;
    cur_list.prev_graf = 0;
    shown_mode = (false, ListMode::NoMode);
    page_contents = PageContents::Empty;
    page_tail = PAGE_HEAD;
    last_glue = MAX_HALFWORD;
    last_penalty = 0;
    last_kern = Scaled::ZERO;
    page_so_far[7] = Scaled::ZERO;

    for k in INT_BASE..=EQTB_SIZE {
        _xeq_level_array[k - INT_BASE] = 1_u16;
    }

    no_new_control_sequence = true;
    prim[0].s0 = 0;
    prim[0].s1 = 0;

    for k in 1..=PRIM_SIZE {
        prim[k] = prim[0];
    }

    prim_eqtb[0].lvl = LEVEL_ZERO;
    prim_eqtb[0].cmd = Cmd::UndefinedCS as u16;
    prim_eqtb[0].val = None.tex_int();

    for k in 1..=PRIM_SIZE {
        prim_eqtb[k] = prim_eqtb[0];
    }

    SAVE_PTR = 0;
    cur_level = LEVEL_ONE;
    cur_group = GroupCode::BottomLevel;
    cur_boundary = 0;
    MAX_SAVE_STACK = 0;
    mag_set = 0;
    expand_depth_count = 0;
    is_in_csname = false;
    cur_mark[TOP_MARK_CODE] = None;
    cur_mark[FIRST_MARK_CODE] = None;
    cur_mark[BOT_MARK_CODE] = None;
    cur_mark[SPLIT_FIRST_MARK_CODE] = None;
    cur_mark[SPLIT_BOT_MARK_CODE] = None;
    cur_order = GlueOrder::Normal;

    read_file = [NONE_UFILE; 17];
    read_open = [OpenMode::Closed; 17];

    cond_ptr = None;
    if_limit = FiOrElseCode::Normal;
    cur_if = 0;
    if_line = 0;
    TOTAL_PAGES = 0;
    max_v = Scaled::ZERO;
    max_h = Scaled::ZERO;
    max_push = 0;
    last_bop = -1;
    doing_leaders = false;
    dead_cycles = 0;
    adjust_tail = None;
    last_badness = 0;
    pre_adjust_tail = None;
    pack_begin_line = 0;
    align_ptr = None;
    cur_align = None;
    cur_span = None;
    cur_loop = None;
    cur_head = None;
    cur_tail = None;
    cur_pre_head = None;
    cur_pre_tail = None;
    cur_f = 0;
    max_hyph_char = TOO_BIG_LANG;

    for z in 0..=HYPH_SIZE {
        HYPH_WORD[z as usize] = 0;
        HYPH_LIST[z as usize] = None;
        HYPH_LINK[z as usize] = 0;
    }

    HYPH_COUNT = 0;
    HYPH_NEXT = HYPH_PRIME as usize + 1;
    if HYPH_NEXT > HYPH_SIZE {
        HYPH_NEXT = HYPH_PRIME as usize;
    }

    output_active = false;
    insert_penalties = 0;
    ligature_present = false;
    cancel_boundary = false;
    lft_hit = false;
    rt_hit = false;
    ins_disc = false;
    after_token = 0;
    long_help_seen = false;
    format_ident = 0;

    for k in 0..=17 {
        write_open[k] = false;
    }

    LR_ptr = None.tex_int();
    LR_problems = 0i32;
    cur_dir = LR::LeftToRight;
    pseudo_files = None.tex_int();
    sa_root[ValLevel::Mark as usize] = None;
    sa_chain = None.tex_int();
    sa_level = LEVEL_ZERO;
    disc_ptr[2] = None.tex_int();
    disc_ptr[3] = None.tex_int();
    edit_name_start = 0;
    stop_at_space = true;
}
unsafe fn initialize_more_initex_variables() {
    for k in 1..=19 {
        MEM[k].b32.s1 = 0;
    }

    for k in (0..=19).step_by(4) {
        MEM[k].b32.s1 = None.tex_int() + 1;
        MEM[k].b16.s1 = NORMAL;
        MEM[k].b16.s0 = NORMAL;
    }

    MEM[6].b32.s1 = 65536;
    MEM[4].b16.s1 = FIL as u16;
    MEM[10].b32.s1 = 65536;
    MEM[8].b16.s1 = FILL as u16;
    MEM[14].b32.s1 = 65536;
    MEM[12].b16.s1 = FIL as u16;
    MEM[15].b32.s1 = 65536;
    MEM[12].b16.s0 = FIL as u16;
    MEM[18].b32.s1 = -65536;
    MEM[16].b16.s1 = FIL as u16;
    rover = 20;
    MEM[rover as usize].b32.s1 = MAX_HALFWORD;
    MEM[rover as usize].b32.s0 = 1000;
    MEM[(rover + 1) as usize].b32.s0 = rover;
    MEM[(rover + 1) as usize].b32.s1 = rover;
    lo_mem_max = rover + 1000;
    MEM[lo_mem_max as usize].b32.s1 = None.tex_int();
    MEM[lo_mem_max as usize].b32.s0 = None.tex_int();

    for k in PRE_ADJUST_HEAD..=MEM_TOP {
        MEM[k] = MEM[lo_mem_max as usize];
    }

    MEM[OMIT_TEMPLATE as usize].b32.s0 = CS_TOKEN_FLAG + FROZEN_END_TEMPLATE as i32;
    MEM[END_SPAN].b32.s1 = std::u16::MAX as i32 + 1;
    MEM[END_SPAN].b32.s0 = None.tex_int();
    MEM[ACTIVE_LIST].b16.s1 = BreakType::Hyphenated as _;
    MEM[ACTIVE_LIST + 1].b32.s0 = MAX_HALFWORD;
    MEM[ACTIVE_LIST].b16.s0 = 0;
    PageInsertion(PAGE_INS_HEAD)
        .set_box_reg(255)
        .set_subtype(PageInsType::SplitUp);
    *LLIST_link(PAGE_INS_HEAD) = Some(PAGE_INS_HEAD).tex_int();
    MEM[PAGE_HEAD].b16.s1 = TextNode::Glue as u16;
    MEM[PAGE_HEAD].b16.s0 = NORMAL;
    avail = None;
    mem_end = MEM_TOP as i32;
    hi_mem_min = PRE_ADJUST_HEAD as i32;
    var_used = 20;
    dyn_used = HI_MEM_STAT_USAGE;
    EQTB[UNDEFINED_CONTROL_SEQUENCE].cmd = Cmd::UndefinedCS as u16;
    EQTB[UNDEFINED_CONTROL_SEQUENCE].val = None.tex_int();
    EQTB[UNDEFINED_CONTROL_SEQUENCE].lvl = LEVEL_ZERO;

    for k in ACTIVE_BASE..=EQTB_TOP {
        EQTB[k] = EQTB[UNDEFINED_CONTROL_SEQUENCE];
    }

    EQTB[GLUE_BASE].val = 0;
    EQTB[GLUE_BASE].lvl = LEVEL_ONE;
    EQTB[GLUE_BASE].cmd = Cmd::GlueRef as u16;

    for k in GLUE_BASE..=LOCAL_BASE {
        EQTB[k] = EQTB[GLUE_BASE];
    }

    MEM[0].b32.s1 += 531;
    *LOCAL(Local::par_shape) = None.tex_int();
    EQTB[LOCAL_BASE + Local::par_shape as usize].cmd = Cmd::ShapeRef as _;
    EQTB[LOCAL_BASE + Local::par_shape as usize].lvl = LEVEL_ONE as _;

    for k in ETEX_PEN_BASE..=(ETEX_PENS - 1) {
        EQTB[k] = EQTB[LOCAL_BASE + Local::par_shape as usize];
    }

    for k in (LOCAL_BASE + Local::output_routine as usize)..=(TOKS_BASE + NUMBER_REGS - 1) {
        EQTB[k] = EQTB[UNDEFINED_CONTROL_SEQUENCE];
    }

    EQTB[BOX_BASE].val = None.tex_int();
    EQTB[BOX_BASE].cmd = Cmd::BoxRef as u16;
    EQTB[BOX_BASE].lvl = LEVEL_ONE;

    for k in (BOX_BASE + 1)..=(BOX_BASE + NUMBER_REGS - 1) {
        EQTB[k] = EQTB[BOX_BASE];
    }

    EQTB[CUR_FONT_LOC].val = FONT_BASE as i32;
    EQTB[CUR_FONT_LOC].cmd = Cmd::Data as u16;
    EQTB[CUR_FONT_LOC].lvl = LEVEL_ONE;

    for k in MATH_FONT_BASE..=(MATH_FONT_BASE + NUMBER_MATH_FONTS - 1) {
        EQTB[k] = EQTB[CUR_FONT_LOC];
    }

    EQTB[CAT_CODE_BASE].val = 0;
    EQTB[CAT_CODE_BASE].cmd = Cmd::Data as u16;
    EQTB[CAT_CODE_BASE].lvl = LEVEL_ONE;

    for k in (CAT_CODE_BASE + 1)..=(INT_BASE as usize - 1) {
        EQTB[k] = EQTB[CAT_CODE_BASE];
    }

    for k in 0..=(NUMBER_USVS as i32 - 1) {
        *CAT_CODE(k as usize) = Cmd::OtherChar as _;
        *MATH_CODE(k as usize) = k;
        *SF_CODE(k as usize) = 1000;
    }

    *CAT_CODE('\r' as usize) = Cmd::CarRet as _;
    *CAT_CODE(' ' as usize) = Cmd::Spacer as _;
    *CAT_CODE('\\' as usize) = ESCAPE as _;
    *CAT_CODE('%' as usize) = Cmd::Comment as _;
    *CAT_CODE(0x7f) = INVALID_CHAR as _;

    EQTB[CAT_CODE_BASE].val = IGNORE as _;
    for k in ('0' as i32)..=('9' as i32) {
        *MATH_CODE(k as usize) = k + set_class(VAR_FAM_CLASS);
    }

    for k in ('A' as i32)..=('Z' as i32) {
        *CAT_CODE(k as usize) = Cmd::Letter as _;
        *CAT_CODE(k as usize + 32) = Cmd::Letter as _;
        *MATH_CODE(k as usize) = k + set_family(1) + set_class(VAR_FAM_CLASS);
        *MATH_CODE(k as usize + 32) = k + 32 + set_family(1) + set_class(VAR_FAM_CLASS);
        *LC_CODE(k as usize) = k + 32;
        *LC_CODE(k as usize + 32) = k + 32;
        *UC_CODE(k as usize) = k;
        *UC_CODE(k as usize + 32) = k;
        *SF_CODE(k as usize) = 999;
    }

    for k in INT_BASE..=(DEL_CODE_BASE - 1) {
        EQTB[k].val = 0;
    }

    *INTPAR(IntPar::char_sub_def_min) = 256;
    *INTPAR(IntPar::char_sub_def_max) = -1;
    *INTPAR(IntPar::mag) = 1000;
    *INTPAR(IntPar::tolerance) = 10_000;
    *INTPAR(IntPar::hang_after) = 1;
    *INTPAR(IntPar::max_dead_cycles) = 25;
    *INTPAR(IntPar::escape_char) = '\\' as i32;
    *INTPAR(IntPar::end_line_char) = '\r' as i32;

    for k in 0..=(NUMBER_USVS - 1) {
        *DEL_CODE(k) = -1;
    }

    *DEL_CODE(46) = 0;

    for k in DIMEN_BASE..=EQTB_SIZE {
        EQTB[k].val = 0;
    }

    prim_used = PRIM_SIZE;
    hash_used = FROZEN_CONTROL_SEQUENCE as i32;
    hash_high = 0;
    cs_count = 0;
    EQTB[FROZEN_DONT_EXPAND as usize].cmd = Cmd::DontExpand as _;
    (*hash.offset(FROZEN_DONT_EXPAND as isize)).s1 = maketexstring("notexpanded:");
    EQTB[FROZEN_PRIMITIVE as usize].cmd = Cmd::IgnoreSpaces as u16;
    EQTB[FROZEN_PRIMITIVE as usize].val = 1;
    EQTB[FROZEN_PRIMITIVE as usize].lvl = LEVEL_ONE;
    (*hash.offset(FROZEN_PRIMITIVE as isize)).s1 = maketexstring("primitive");

    for k in (-TRIE_OP_SIZE)..=TRIE_OP_SIZE {
        _trie_op_hash_array[(k as i64 - -35111) as usize] = 0;
    }

    for k in 0..=BIGGEST_LANG {
        trie_used[k as usize] = MIN_TRIE_OP;
    }

    max_op_used = MIN_TRIE_OP;
    trie_op_ptr = 0;
    trie_not_ready = true;
    (*hash.offset(FROZEN_PROTECTION as isize)).s1 = maketexstring("inaccessible");

    format_ident = maketexstring(" (INITEX)");

    (*hash.offset(END_WRITE as isize)).s1 = maketexstring("endwrite");
    EQTB[END_WRITE as usize].lvl = LEVEL_ONE;
    EQTB[END_WRITE as usize].cmd = Cmd::OuterCall as u16;
    EQTB[END_WRITE as usize].val = None.tex_int();

    max_reg_num = 32767;
    max_reg_help_line = "A register number must be between 0 and 32767.";

    for i in (ValLevel::Int as usize)..=(ValLevel::InterChar as usize) {
        sa_root[i] = None;
    }

    *INTPAR(IntPar::xetex_hyphenatable_length) = 63;
}
/*:1370*/
/*1371: */
unsafe fn initialize_primitives() {
    no_new_control_sequence = false;
    first = 0i32;
    primitive(
        "lineskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::line_skip as usize,
    );
    primitive(
        "baselineskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::baseline_skip as usize,
    );
    primitive(
        "parskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::par_skip as usize,
    );
    primitive(
        "abovedisplayskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::above_display_skip as usize,
    );
    primitive(
        "belowdisplayskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::below_display_skip as usize,
    );
    primitive(
        "abovedisplayshortskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::above_display_short_skip as usize,
    );
    primitive(
        "belowdisplayshortskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::below_display_short_skip as usize,
    );
    primitive(
        "leftskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::left_skip as usize,
    );
    primitive(
        "rightskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::right_skip as usize,
    );
    primitive(
        "topskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::top_skip as usize,
    );
    primitive(
        "splittopskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::split_top_skip as usize,
    );
    primitive(
        "tabskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::tab_skip as usize,
    );
    primitive(
        "spaceskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::space_skip as usize,
    );
    primitive(
        "xspaceskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::xspace_skip as usize,
    );
    primitive(
        "parfillskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::par_fill_skip as usize,
    );
    primitive(
        "XeTeXlinebreakskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::xetex_linebreak_skip as usize,
    );

    primitive(
        "thinmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::thin_mu_skip as usize,
    );
    primitive(
        "medmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::med_mu_skip as usize,
    );
    primitive(
        "thickmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::thick_mu_skip as usize,
    );

    primitive(
        "output",
        Cmd::AssignToks,
        LOCAL_BASE + Local::output_routine as usize,
    );
    primitive(
        "everypar",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_par as usize,
    );
    primitive(
        "everymath",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_math as usize,
    );
    primitive(
        "everydisplay",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_display as usize,
    );
    primitive(
        "everyhbox",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_hbox as usize,
    );
    primitive(
        "everyvbox",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_vbox as usize,
    );
    primitive(
        "everyjob",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_job as usize,
    );
    primitive(
        "everycr",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_cr as usize,
    );
    primitive(
        "errhelp",
        Cmd::AssignToks,
        LOCAL_BASE + Local::err_help as usize,
    );
    primitive(
        "everyeof",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_eof as usize,
    );
    primitive(
        "XeTeXinterchartoks",
        Cmd::AssignToks,
        LOCAL_BASE + Local::xetex_inter_char as usize,
    );
    primitive(
        "TectonicCodaTokens",
        Cmd::AssignToks,
        LOCAL_BASE + Local::TectonicCodaTokens as usize,
    );

    primitive(
        "pretolerance",
        Cmd::AssignInt,
        INT_BASE + IntPar::pretolerance as usize,
    );
    primitive(
        "tolerance",
        Cmd::AssignInt,
        INT_BASE + IntPar::tolerance as usize,
    );
    primitive(
        "linepenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::line_penalty as usize,
    );
    primitive(
        "hyphenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::hyphen_penalty as usize,
    );
    primitive(
        "exhyphenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::ex_hyphen_penalty as usize,
    );
    primitive(
        "clubpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::club_penalty as usize,
    );
    primitive(
        "widowpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::widow_penalty as usize,
    );
    primitive(
        "displaywidowpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::display_widow_penalty as usize,
    );
    primitive(
        "brokenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::broken_penalty as usize,
    );
    primitive(
        "binoppenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::bin_op_penalty as usize,
    );
    primitive(
        "relpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::rel_penalty as usize,
    );
    primitive(
        "predisplaypenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::pre_display_penalty as usize,
    );
    primitive(
        "postdisplaypenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::post_display_penalty as usize,
    );
    primitive(
        "interlinepenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::inter_line_penalty as usize,
    );
    primitive(
        "doublehyphendemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::double_hyphen_demerits as usize,
    );
    primitive(
        "finalhyphendemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::final_hyphen_demerits as usize,
    );
    primitive(
        "adjdemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::adj_demerits as usize,
    );
    primitive("mag", Cmd::AssignInt, INT_BASE + IntPar::mag as usize);
    primitive(
        "delimiterfactor",
        Cmd::AssignInt,
        INT_BASE + IntPar::delimiter_factor as usize,
    );
    primitive(
        "looseness",
        Cmd::AssignInt,
        INT_BASE + IntPar::looseness as usize,
    );
    primitive("time", Cmd::AssignInt, INT_BASE + IntPar::time as usize);
    primitive("day", Cmd::AssignInt, INT_BASE + IntPar::day as usize);
    primitive("month", Cmd::AssignInt, INT_BASE + IntPar::month as usize);
    primitive("year", Cmd::AssignInt, INT_BASE + IntPar::year as usize);
    primitive(
        "showboxbreadth",
        Cmd::AssignInt,
        INT_BASE + IntPar::show_box_breadth as usize,
    );
    primitive(
        "showboxdepth",
        Cmd::AssignInt,
        INT_BASE + IntPar::show_box_depth as usize,
    );
    primitive(
        "hbadness",
        Cmd::AssignInt,
        INT_BASE + IntPar::hbadness as usize,
    );
    primitive(
        "vbadness",
        Cmd::AssignInt,
        INT_BASE + IntPar::vbadness as usize,
    );
    primitive(
        "pausing",
        Cmd::AssignInt,
        INT_BASE + IntPar::pausing as usize,
    );
    primitive(
        "tracingonline",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_online as usize,
    );
    primitive(
        "tracingmacros",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_macros as usize,
    );
    primitive(
        "tracingstats",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_stats as usize,
    );
    primitive(
        "tracingparagraphs",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_paragraphs as usize,
    );
    primitive(
        "tracingpages",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_pages as usize,
    );
    primitive(
        "tracingoutput",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_output as usize,
    );
    primitive(
        "tracinglostchars",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_lost_chars as usize,
    );
    primitive(
        "tracingcommands",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_commands as usize,
    );
    primitive(
        "tracingrestores",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_restores as usize,
    );
    primitive(
        "uchyph",
        Cmd::AssignInt,
        INT_BASE + IntPar::uc_hyph as usize,
    );
    primitive(
        "outputpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::output_penalty as usize,
    );
    primitive(
        "maxdeadcycles",
        Cmd::AssignInt,
        INT_BASE + IntPar::max_dead_cycles as usize,
    );
    primitive(
        "hangafter",
        Cmd::AssignInt,
        INT_BASE + IntPar::hang_after as usize,
    );
    primitive(
        "floatingpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::floating_penalty as usize,
    );
    primitive(
        "globaldefs",
        Cmd::AssignInt,
        INT_BASE + IntPar::global_defs as usize,
    );
    primitive("fam", Cmd::AssignInt, INT_BASE + IntPar::cur_fam as usize);
    primitive(
        "escapechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::escape_char as usize,
    );
    primitive(
        "defaulthyphenchar",
        Cmd::AssignInt,
        INT_BASE + IntPar::default_hyphen_char as usize,
    );
    primitive(
        "defaultskewchar",
        Cmd::AssignInt,
        INT_BASE + IntPar::default_skew_char as usize,
    );
    primitive(
        "endlinechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::end_line_char as usize,
    );
    primitive(
        "newlinechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::new_line_char as usize,
    );
    primitive(
        "language",
        Cmd::AssignInt,
        INT_BASE + IntPar::language as usize,
    );
    primitive(
        "lefthyphenmin",
        Cmd::AssignInt,
        INT_BASE + IntPar::left_hyphen_min as usize,
    );
    primitive(
        "righthyphenmin",
        Cmd::AssignInt,
        INT_BASE + IntPar::right_hyphen_min as usize,
    );
    primitive(
        "holdinginserts",
        Cmd::AssignInt,
        INT_BASE + IntPar::holding_inserts as usize,
    );
    primitive(
        "errorcontextlines",
        Cmd::AssignInt,
        INT_BASE + IntPar::error_context_lines as usize,
    );

    primitive(
        "XeTeXlinebreakpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::xetex_linebreak_penalty as usize,
    );
    primitive(
        "XeTeXprotrudechars",
        Cmd::AssignInt,
        INT_BASE + IntPar::xetex_protrude_chars as usize,
    );

    primitive(
        "parindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::par_indent as usize,
    );
    primitive(
        "mathsurround",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::math_surround as usize,
    );
    primitive(
        "lineskiplimit",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::line_skip_limit as usize,
    );
    primitive(
        "hsize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hsize as usize,
    );
    primitive(
        "vsize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::vsize as usize,
    );
    primitive(
        "maxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::max_depth as usize,
    );
    primitive(
        "splitmaxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::split_max_depth as usize,
    );
    primitive(
        "boxmaxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::box_max_depth as usize,
    );
    primitive(
        "hfuzz",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hfuzz as usize,
    );
    primitive(
        "vfuzz",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::vfuzz as usize,
    );
    primitive(
        "delimitershortfall",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::delimiter_shortfall as usize,
    );
    primitive(
        "nulldelimiterspace",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::null_delimiter_space as usize,
    );
    primitive(
        "scriptspace",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::script_space as usize,
    );
    primitive(
        "predisplaysize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pre_display_size as usize,
    );
    primitive(
        "displaywidth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::display_width as usize,
    );
    primitive(
        "displayindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::display_indent as usize,
    );
    primitive(
        "overfullrule",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::overfull_rule as usize,
    );
    primitive(
        "hangindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hang_indent as usize,
    );
    primitive(
        "hoffset",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::h_offset as usize,
    );
    primitive(
        "voffset",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::v_offset as usize,
    );
    primitive(
        "emergencystretch",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::emergency_stretch as usize,
    );
    primitive(
        "pdfpagewidth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pdf_page_width as usize,
    );
    primitive(
        "pdfpageheight",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pdf_page_height as usize,
    );

    primitive(" ", Cmd::ExSpace, 0);
    primitive("/", Cmd::ItalCorr, 0);
    primitive("accent", Cmd::Accent, 0);
    primitive("advance", Cmd::Advance, 0);
    primitive("afterassignment", Cmd::AfterAssignment, 0);
    primitive("aftergroup", Cmd::AfterGroup, 0);
    primitive("begingroup", Cmd::BeginGroup, 0);
    primitive("char", Cmd::CharNum, 0);
    primitive("csname", Cmd::CSName, 0);
    primitive("delimiter", Cmd::DelimNum, 0);
    primitive("XeTeXdelimiter", Cmd::DelimNum, 1);
    primitive("Udelimiter", Cmd::DelimNum, 1);
    primitive("divide", Cmd::Divide, 0);
    primitive("endcsname", Cmd::EndCSName, 0);
    let val = primitive("endgroup", Cmd::EndGroup, 0);
    (*hash.offset(FROZEN_END_GROUP as isize)).s1 = maketexstring("endgroup");
    EQTB[FROZEN_END_GROUP] = EQTB[val as usize];
    primitive("expandafter", Cmd::ExpandAfter, 0);
    primitive("font", Cmd::DefFont, 0);
    primitive("fontdimen", Cmd::AssignFontDimen, 0);
    primitive("halign", Cmd::HAlign, 0);
    primitive("hrule", Cmd::HRule, 0);
    primitive("ignorespaces", Cmd::IgnoreSpaces, 0);
    primitive("insert", Cmd::Insert, 0);
    primitive("mark", Cmd::Mark, 0);
    primitive("mathaccent", Cmd::MathAccent, 0);
    primitive("XeTeXmathaccent", Cmd::MathAccent, 1);
    primitive("Umathaccent", Cmd::MathAccent, 1);
    primitive("mathchar", Cmd::MathCharNum, 0);
    primitive("XeTeXmathcharnum", Cmd::MathCharNum, 1);
    primitive("Umathcharnum", Cmd::MathCharNum, 1);
    primitive("XeTeXmathchar", Cmd::MathCharNum, 2);
    primitive("Umathchar", Cmd::MathCharNum, 2);
    primitive("mathchoice", Cmd::MathChoice, 0);
    primitive("multiply", Cmd::Multiply, 0);
    primitive("noalign", Cmd::NoAlign, 0);
    primitive("noboundary", Cmd::NoBoundary, 0);
    primitive("noexpand", Cmd::NoExpand, 0);
    primitive("primitive", Cmd::NoExpand, 1);
    primitive("nonscript", Cmd::NonScript, 0);
    primitive("omit", Cmd::Omit, 0);
    primitive(
        "parshape",
        Cmd::SetShape,
        LOCAL_BASE as i32 + Local::par_shape as i32,
    );
    primitive("penalty", Cmd::BreakPenalty, 0);
    primitive("prevgraf", Cmd::SetPrevGraf, 0);
    primitive("radical", Cmd::Radical, 0);
    primitive("XeTeXradical", Cmd::Radical, 1);
    primitive("Uradical", Cmd::Radical, 1);
    primitive("read", Cmd::ReadToCS, 0);
    let val = primitive("relax", Cmd::Relax, TOO_BIG_USV as i32);
    (*hash.offset(FROZEN_RELAX as isize)).s1 = maketexstring("relax");
    EQTB[FROZEN_RELAX] = EQTB[val as usize];
    primitive("setbox", Cmd::SetBox, 0);
    primitive("the", Cmd::The, 0);
    primitive("toks", Cmd::ToksRegister, 0);
    primitive("vadjust", Cmd::VAdjust, 0);
    primitive("valign", Cmd::VAlign, 0);
    primitive("vcenter", Cmd::VCenter, 0);
    primitive("vrule", Cmd::VRule, 0);
    let val = primitive("par", PAR_END, TOO_BIG_USV as i32);
    par_loc = val;
    par_token = CS_TOKEN_FLAG + par_loc;

    primitive("input", Cmd::Input, 0);
    primitive("endinput", Cmd::Input, 1);

    primitive("topmark", Cmd::TopBotMark, TopBotMarkCode::Top);
    primitive("firstmark", Cmd::TopBotMark, TopBotMarkCode::First);
    primitive("botmark", Cmd::TopBotMark, TopBotMarkCode::Bot);
    primitive(
        "splitfirstmark",
        Cmd::TopBotMark,
        TopBotMarkCode::SplitFirst,
    );
    primitive("splitbotmark", Cmd::TopBotMark, TopBotMarkCode::SplitBot);

    primitive("count", Cmd::Register, 0);
    primitive("dimen", Cmd::Register, 1);
    primitive("skip", Cmd::Register, 2);
    primitive("muskip", Cmd::Register, 3);

    primitive("spacefactor", Cmd::SetAux, ListMode::HMode as i32);
    primitive("prevdepth", Cmd::SetAux, ListMode::VMode as i32);

    primitive("deadcycles", Cmd::SetPageInt, 0);
    primitive("insertpenalties", Cmd::SetPageInt, 1);

    primitive("wd", Cmd::SetBoxDimen, SetBoxDimen::WidthOffset);
    primitive("ht", Cmd::SetBoxDimen, SetBoxDimen::HeightOffset);
    primitive("dp", Cmd::SetBoxDimen, SetBoxDimen::DepthOffset);

    primitive("lastpenalty", Cmd::LastItem, LastItemCode::LastPenalty);
    primitive("lastkern", Cmd::LastItem, LastItemCode::LastKern);
    primitive("lastskip", Cmd::LastItem, LastItemCode::LastSkip);
    primitive("inputlineno", Cmd::LastItem, LastItemCode::InputLineNo);
    primitive("badness", Cmd::LastItem, LastItemCode::Badness);

    primitive("number", Cmd::Convert, ConvertCode::Number);
    primitive("romannumeral", Cmd::Convert, ConvertCode::RomanNumeral);
    primitive("string", Cmd::Convert, ConvertCode::String);
    primitive("meaning", Cmd::Convert, ConvertCode::Meaning);
    primitive("fontname", Cmd::Convert, ConvertCode::FontName);
    primitive("jobname", Cmd::Convert, ConvertCode::JobName);
    primitive("leftmarginkern", Cmd::Convert, ConvertCode::LeftMarginKern);
    primitive(
        "rightmarginkern",
        Cmd::Convert,
        ConvertCode::RightMarginKern,
    );
    primitive("Uchar", Cmd::Convert, ConvertCode::XetexUchar);
    primitive("Ucharcat", Cmd::Convert, ConvertCode::XetexUcharcat);

    primitive("if", Cmd::IfTest, IfTestCode::IfChar);
    primitive("ifcat", Cmd::IfTest, IfTestCode::IfCat);
    primitive("ifnum", Cmd::IfTest, IfTestCode::IfInt);
    primitive("ifdim", Cmd::IfTest, IfTestCode::IfDim);
    primitive("ifodd", Cmd::IfTest, IfTestCode::IfOdd);
    primitive("ifvmode", Cmd::IfTest, IfTestCode::IfVMode);
    primitive("ifhmode", Cmd::IfTest, IfTestCode::IfHMode);
    primitive("ifmmode", Cmd::IfTest, IfTestCode::IfMMode);
    primitive("ifinner", Cmd::IfTest, IfTestCode::IfInner);
    primitive("ifvoid", Cmd::IfTest, IfTestCode::IfVoid);
    primitive("ifhbox", Cmd::IfTest, IfTestCode::IfHBox);
    primitive("ifvbox", Cmd::IfTest, IfTestCode::IfVBox);
    primitive("ifx", Cmd::IfTest, IfTestCode::Ifx);
    primitive("ifeof", Cmd::IfTest, IfTestCode::IfEof);
    primitive("iftrue", Cmd::IfTest, IfTestCode::IfTrue);
    primitive("iffalse", Cmd::IfTest, IfTestCode::IfFalse);
    primitive("ifcase", Cmd::IfTest, IfTestCode::IfCase);
    primitive("ifprimitive", Cmd::IfTest, IfTestCode::IfPrimitive);

    let val = primitive("fi", Cmd::FiOrElse, FiOrElseCode::Fi);
    (*hash.offset(FROZEN_FI as isize)).s1 = maketexstring("fi");
    EQTB[FROZEN_FI] = EQTB[val as usize];
    primitive("or", Cmd::FiOrElse, FiOrElseCode::Or);
    primitive("else", Cmd::FiOrElse, FiOrElseCode::Else);

    let val = primitive("nullfont", Cmd::SetFont, FONT_BASE);
    (*hash.offset(FROZEN_NULL_FONT as isize)).s1 = maketexstring("nullfont");
    EQTB[FROZEN_NULL_FONT] = EQTB[val as usize];

    primitive("span", Cmd::TabMark, SPAN_CODE);
    let val = primitive("cr", Cmd::CarRet, CR_CODE);
    (*hash.offset(FROZEN_CR as isize)).s1 = maketexstring("cr");
    EQTB[FROZEN_CR] = EQTB[val as usize];
    primitive("crcr", Cmd::CarRet, CR_CR_CODE);

    (*hash.offset(FROZEN_END_TEMPLATE as isize)).s1 = maketexstring("endtemplate");
    (*hash.offset(FROZEN_ENDV as isize)).s1 = maketexstring("endtemplate");
    EQTB[FROZEN_ENDV].cmd = Cmd::EndV as u16;
    EQTB[FROZEN_ENDV].val = NULL_LIST as i32;
    EQTB[FROZEN_ENDV].lvl = LEVEL_ONE;
    EQTB[FROZEN_END_TEMPLATE] = EQTB[FROZEN_ENDV];
    EQTB[FROZEN_END_TEMPLATE].cmd = Cmd::EndTemplate as u16;

    primitive("pagegoal", Cmd::SetPageDimen, 0);
    primitive("pagetotal", Cmd::SetPageDimen, 1);
    primitive("pagestretch", Cmd::SetPageDimen, 2);
    primitive("pagefilstretch", Cmd::SetPageDimen, 3);
    primitive("pagefillstretch", Cmd::SetPageDimen, 4);
    primitive("pagefilllstretch", Cmd::SetPageDimen, 5);
    primitive("pageshrink", Cmd::SetPageDimen, 6);
    primitive("pagedepth", Cmd::SetPageDimen, 7);

    primitive("end", STOP, 0);
    primitive("dump", STOP, 1);

    primitive("hskip", Cmd::HSkip, SkipCode::Skip);
    primitive("hfil", Cmd::HSkip, SkipCode::Fil);
    primitive("hfill", Cmd::HSkip, SkipCode::Fill);
    primitive("hss", Cmd::HSkip, SkipCode::Ss);
    primitive("hfilneg", Cmd::HSkip, SkipCode::FilNeg);
    primitive("vskip", Cmd::VSkip, SkipCode::Skip);
    primitive("vfil", Cmd::VSkip, SkipCode::Fil);
    primitive("vfill", Cmd::VSkip, SkipCode::Fill);
    primitive("vss", Cmd::VSkip, SkipCode::Ss);
    primitive("vfilneg", Cmd::VSkip, SkipCode::FilNeg);
    primitive("mskip", Cmd::MSkip, SkipCode::MSkip);

    primitive("kern", Cmd::Kern, KernType::Explicit as i32);
    primitive("mkern", Cmd::MKern, KernType::Math as i32);
    primitive("moveleft", Cmd::HMove, 1);
    primitive("moveright", Cmd::HMove, 0);
    primitive("raise", Cmd::VMove, 1);
    primitive("lower", Cmd::VMove, 0);

    primitive("box", Cmd::MakeBox, BoxCode::Box);
    primitive("copy", Cmd::MakeBox, BoxCode::Copy);
    primitive("lastbox", Cmd::MakeBox, BoxCode::LastBox);
    primitive("vsplit", Cmd::MakeBox, BoxCode::VSplit);
    primitive("vtop", Cmd::MakeBox, BoxCode::VTop);
    primitive("vbox", Cmd::MakeBox, BoxCode::VBox);
    primitive("hbox", Cmd::MakeBox, BoxCode::HBox);

    primitive("shipout", Cmd::LeaderShip, A_LEADERS as i32 - 1);
    primitive("leaders", Cmd::LeaderShip, A_LEADERS as i32);
    primitive("cleaders", Cmd::LeaderShip, C_LEADERS as i32);
    primitive("xleaders", Cmd::LeaderShip, X_LEADERS as i32);

    primitive("indent", Cmd::StartPar, 1);
    primitive("noindent", Cmd::StartPar, 0);
    primitive("unpenalty", Cmd::RemoveItem, TextNode::Penalty as i32);
    primitive("unkern", Cmd::RemoveItem, TextNode::Kern as i32);
    primitive("unskip", Cmd::RemoveItem, TextNode::Glue as i32);
    primitive("unhbox", Cmd::UnHBox, BoxCode::Box);
    primitive("unhcopy", Cmd::UnHBox, BoxCode::Copy);
    primitive("unvbox", Cmd::UnVBox, BoxCode::Box);
    primitive("unvcopy", Cmd::UnVBox, BoxCode::Copy);

    primitive("-", Cmd::Discretionary, 1);
    primitive("discretionary", Cmd::Discretionary, 0);

    primitive("eqno", Cmd::EqNo, 0);
    primitive("leqno", Cmd::EqNo, 1);

    primitive("mathord", Cmd::MathComp, MathNode::Ord as i32);
    primitive("mathop", Cmd::MathComp, MathNode::Op as i32);
    primitive("mathbin", Cmd::MathComp, MathNode::Bin as i32);
    primitive("mathrel", Cmd::MathComp, MathNode::Rel as i32);
    primitive("mathopen", Cmd::MathComp, MathNode::Open as i32);
    primitive("mathclose", Cmd::MathComp, MathNode::Close as i32);
    primitive("mathpunct", Cmd::MathComp, MathNode::Punct as i32);
    primitive("mathinner", Cmd::MathComp, MathNode::Inner as i32);
    primitive("underline", Cmd::MathComp, MathNode::Under as i32);
    primitive("overline", Cmd::MathComp, MathNode::Over as i32);

    primitive("displaylimits", Cmd::LimitSwitch, Limit::Normal as i32);
    primitive("limits", Cmd::LimitSwitch, Limit::Limits as i32);
    primitive("nolimits", Cmd::LimitSwitch, Limit::NoLimits as i32);

    primitive(
        "displaystyle",
        Cmd::MathStyle,
        (MathStyle::Display as i32) * 2,
    );
    primitive("textstyle", Cmd::MathStyle, (MathStyle::Text as i32) * 2);
    primitive(
        "scriptstyle",
        Cmd::MathStyle,
        (MathStyle::Script as i32) * 2,
    );
    primitive(
        "scriptscriptstyle",
        Cmd::MathStyle,
        (MathStyle::ScriptScript as i32) * 2,
    );

    primitive("above", Cmd::Above, ABOVE_CODE);
    primitive("over", Cmd::Above, OVER_CODE);
    primitive("atop", Cmd::Above, ATOP_CODE);
    primitive("abovewithdelims", Cmd::Above, DELIMITED_CODE + 0);
    primitive("overwithdelims", Cmd::Above, DELIMITED_CODE + 1);
    primitive("atopwithdelims", Cmd::Above, DELIMITED_CODE + 2);

    primitive("left", Cmd::LeftRight, MathNode::Left as i32);
    let val = primitive("right", Cmd::LeftRight, MathNode::Right as i32);
    (*hash.offset(FROZEN_RIGHT as isize)).s1 = maketexstring("right");
    EQTB[FROZEN_RIGHT] = EQTB[val as usize];

    primitive("long", Cmd::Prefix, 1);
    primitive("outer", Cmd::Prefix, 2);
    primitive("global", Cmd::Prefix, 4);
    primitive("def", Cmd::Def, 0);
    primitive("gdef", Cmd::Def, 1);
    primitive("edef", Cmd::Def, 2);
    primitive("xdef", Cmd::Def, 3);
    primitive("let", Cmd::Let, NORMAL as i32);
    primitive("futurelet", Cmd::Let, NORMAL as i32 + 1);

    primitive("chardef", Cmd::ShorthandDef, ShorthandDefCode::Char);
    primitive("mathchardef", Cmd::ShorthandDef, ShorthandDefCode::MathChar);
    primitive(
        "XeTeXmathcharnumdef",
        Cmd::ShorthandDef,
        ShorthandDefCode::XetexMathCharNum,
    );
    primitive(
        "Umathcharnumdef",
        Cmd::ShorthandDef,
        ShorthandDefCode::XetexMathCharNum,
    );
    primitive(
        "XeTeXmathchardef",
        Cmd::ShorthandDef,
        ShorthandDefCode::XetexMathChar,
    );
    primitive(
        "Umathchardef",
        Cmd::ShorthandDef,
        ShorthandDefCode::XetexMathChar,
    );
    primitive("countdef", Cmd::ShorthandDef, ShorthandDefCode::Count);
    primitive("dimendef", Cmd::ShorthandDef, ShorthandDefCode::Dimen);
    primitive("skipdef", Cmd::ShorthandDef, ShorthandDefCode::Skip);
    primitive("muskipdef", Cmd::ShorthandDef, ShorthandDefCode::MuSkip);
    primitive("toksdef", Cmd::ShorthandDef, ShorthandDefCode::Toks);

    primitive("catcode", Cmd::DefCode, CAT_CODE_BASE as i32);
    primitive("mathcode", Cmd::DefCode, MATH_CODE_BASE as i32);
    primitive("XeTeXmathcodenum", Cmd::XetexDefCode, MATH_CODE_BASE as i32);
    primitive("Umathcodenum", Cmd::XetexDefCode, MATH_CODE_BASE as i32);
    primitive(
        "XeTeXmathcode",
        Cmd::XetexDefCode,
        MATH_CODE_BASE as i32 + 1,
    );
    primitive("Umathcode", Cmd::XetexDefCode, MATH_CODE_BASE as i32 + 1);
    primitive("lccode", Cmd::DefCode, LC_CODE_BASE as i32);
    primitive("uccode", Cmd::DefCode, UC_CODE_BASE as i32);
    primitive("sfcode", Cmd::DefCode, SF_CODE_BASE as i32);
    primitive("XeTeXcharclass", Cmd::XetexDefCode, SF_CODE_BASE as i32);
    primitive("delcode", Cmd::DefCode, DEL_CODE_BASE);
    primitive("XeTeXdelcodenum", Cmd::XetexDefCode, DEL_CODE_BASE);
    primitive("Udelcodenum", Cmd::XetexDefCode, DEL_CODE_BASE);
    primitive("XeTeXdelcode", Cmd::XetexDefCode, DEL_CODE_BASE + 1);
    primitive("Udelcode", Cmd::XetexDefCode, DEL_CODE_BASE + 1);

    primitive(
        "textfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + TEXT_SIZE as i32,
    );
    primitive(
        "scriptfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + SCRIPT_SIZE as i32,
    );
    primitive(
        "scriptscriptfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + SCRIPT_SCRIPT_SIZE as i32,
    );

    primitive("hyphenation", Cmd::HyphData, 0);
    primitive("patterns", Cmd::HyphData, 1);

    primitive(
        "hyphenchar",
        Cmd::AssignFontInt,
        AssignFontInt::HyphenChar as i32,
    );
    primitive(
        "skewchar",
        Cmd::AssignFontInt,
        AssignFontInt::SkewChar as i32,
    );
    primitive("lpcode", Cmd::AssignFontInt, AssignFontInt::LpCode as i32);
    primitive("rpcode", Cmd::AssignFontInt, AssignFontInt::RpCode as i32);

    primitive("batchmode", Cmd::SetInteraction, InteractionMode::Batch);
    primitive("nonstopmode", Cmd::SetInteraction, InteractionMode::NonStop);
    primitive("scrollmode", Cmd::SetInteraction, InteractionMode::Scroll);
    primitive(
        "errorstopmode",
        Cmd::SetInteraction,
        InteractionMode::ErrorStop,
    );

    primitive("openin", Cmd::InStream, 1);
    primitive("closein", Cmd::InStream, 0);
    primitive("message", Cmd::Message, 0);
    primitive("errmessage", Cmd::Message, 1);
    primitive("lowercase", Cmd::CaseShift, LC_CODE_BASE as i32);
    primitive("uppercase", Cmd::CaseShift, UC_CODE_BASE as i32);

    primitive("show", Cmd::XRay, SHOW_CODE);
    primitive("showbox", Cmd::XRay, SHOW_BOX_CODE);
    primitive("showthe", Cmd::XRay, SHOW_THE_CODE);
    primitive("showlists", Cmd::XRay, SHOW_LISTS);

    primitive("openout", Cmd::Extension, WhatsItNST::Open as i32);
    let val = primitive("write", Cmd::Extension, WhatsItNST::Write as i32);
    write_loc = val;
    primitive("closeout", Cmd::Extension, WhatsItNST::Close as i32);
    let val = primitive("special", Cmd::Extension, WhatsItNST::Special as i32);
    (*hash.offset(FROZEN_SPECIAL as isize)).s1 = maketexstring("special");
    EQTB[FROZEN_SPECIAL] = EQTB[val as usize];
    primitive("immediate", Cmd::Extension, IMMEDIATE_CODE as i32);
    primitive("setlanguage", Cmd::Extension, SET_LANGUAGE_CODE as i32);

    primitive(
        "synctex",
        Cmd::AssignInt,
        INT_BASE + IntPar::synctex as usize,
    );
    no_new_control_sequence = true;
}
unsafe fn get_strings_started() {
    pool_ptr = 0;
    str_ptr = 0;
    str_start[0] = 0;
    str_ptr = TOO_BIG_CHAR;
    if load_pool_strings(pool_size - string_vacancies) == 0 {
        panic!("must increase pool_size");
    };
}
/* xetex-errors */
/* xetex-math */
/* xetex-output */
/* xetex-pagebuilder */
/* xetex-scaledmath */
/* xetex-shipout */
/* Inlines */
/* Strings printed this way will end up in the .log as well
 * as the terminal output. */
/*41: The length of the current string in the pool */
/* Tectonic related functions */
/*:1001*/
pub(crate) unsafe fn tt_cleanup() {
    /*
        Cleanup of all intermediate buffers.
        Conceptually, final_cleanup() and close_files_and_terminate() also
        belong here, but that requires a more thorough refactor as presently
        it would result in a segfault.
    */
    pdf_files_close();
    TEX_format_default = String::new();
    font_used = Vec::new();
    deinitialize_shipout_variables();

    destroy_font_manager();

    FONT_LAYOUT_ENGINE = Vec::new();

    // Free the big allocated arrays
    BUFFER = Vec::new();
    NEST = Vec::new();
    SAVE_STACK = Vec::new();
    INPUT_STACK = Vec::new();
    INPUT_FILE = Vec::new();
    LINE_STACK = Vec::new();
    EOF_SEEN = Vec::new();
    GRP_STACK = Vec::new();
    IF_STACK = Vec::new();
    SOURCE_FILENAME_STACK = Vec::new();
    FULL_SOURCE_FILENAME_STACK = Vec::new();
    PARAM_STACK = Vec::new();
    HYPH_WORD = Vec::new();
    HYPH_LIST = Vec::new();
    HYPH_LINK = Vec::new();

    // initialize_more_variables @ 3277
    free(native_text as *mut libc::c_void);

    // Free arrays allocated in load_fmt_file
    free(yhash as *mut libc::c_void);
    EQTB = Vec::new();
    MEM = Vec::new();
    str_start = Vec::new();
    str_pool = Vec::new();
    FONT_INFO = Vec::new();

    FONT_MAPPING = Vec::new();
    FONT_LAYOUT_ENGINE = Vec::new();
    FONT_FLAGS = Vec::new();
    FONT_LETTER_SPACE = Vec::new();
    FONT_CHECK = Vec::new();
    FONT_SIZE = Vec::new();
    FONT_DSIZE = Vec::new();
    FONT_PARAMS = Vec::new();
    FONT_NAME = Vec::new();
    FONT_AREA = Vec::new();
    FONT_BC = Vec::new();
    FONT_EC = Vec::new();
    FONT_GLUE = Vec::new();
    HYPHEN_CHAR = Vec::new();
    SKEW_CHAR = Vec::new();
    BCHAR_LABEL = Vec::new();
    FONT_BCHAR = Vec::new();
    FONT_FALSE_BCHAR = Vec::new();
    CHAR_BASE = Vec::new();
    WIDTH_BASE = Vec::new();
    HEIGHT_BASE = Vec::new();
    DEPTH_BASE = Vec::new();
    ITALIC_BASE = Vec::new();
    LIG_KERN_BASE = Vec::new();
    KERN_BASE = Vec::new();
    EXTEN_BASE = Vec::new();
    PARAM_BASE = Vec::new();

    trie_trl = Vec::new();
    trie_tro = Vec::new();
    trie_trc = Vec::new();

    read_file = [NONE_UFILE; 17];
    read_open = [OpenMode::Closed; 17];
}

pub(crate) unsafe fn tt_run_engine(
    mut dump_name: *const i8,
    mut input_file_name: *const i8,
) -> TTHistory {
    /* Miscellaneous initializations that were mostly originally done in the
     * main() driver routines. */
    /* Get our stdout handle */
    rust_stdout = ttstub_output_open_stdout();
    TEX_format_default = to_rust_string(dump_name);
    /* Not sure why these get custom initializations. */
    if file_line_error_style_p < 0 {
        file_line_error_style_p = 0
    }
    /* These various parameters were configurable in web2c TeX. We don't
     * bother to allow that. */
    pool_size = 6250000;
    string_vacancies = 90000;
    pool_free = 47500;
    max_strings = 565536;
    strings_free = 100;
    FONT_MEM_SIZE = 8000000;
    FONT_MAX = 9000;
    trie_size = 1000000;
    HYPH_SIZE = 8191;
    BUF_SIZE = 200000;
    NEST_SIZE = 500;
    MAX_IN_OPEN = 15;
    PARAM_SIZE = 10000;
    SAVE_SIZE = 80000;
    STACK_SIZE = 5000;
    error_line = 79;
    half_error_line = 50;
    max_print_line = 79;
    hash_extra = 600000;
    expand_depth = 10000;
    /* Allocate many of our big arrays. */
    BUFFER = vec![0; BUF_SIZE + 1];
    NEST = vec![list_state_record::default(); NEST_SIZE + 1];
    SAVE_STACK = vec![EqtbWord::default(); SAVE_SIZE + 1];
    INPUT_STACK = vec![input_state_t::default(); STACK_SIZE + 1];
    INPUT_FILE = Vec::with_capacity(MAX_IN_OPEN + 1);
    for _ in 0..MAX_IN_OPEN + 1 {
        INPUT_FILE.push(None);
    }
    LINE_STACK = vec![0; MAX_IN_OPEN + 1];
    EOF_SEEN = vec![false; MAX_IN_OPEN + 1];
    GRP_STACK = vec![0; MAX_IN_OPEN + 1];
    IF_STACK = vec![Some(0); MAX_IN_OPEN + 1];
    SOURCE_FILENAME_STACK = vec![0; MAX_IN_OPEN + 1];
    FULL_SOURCE_FILENAME_STACK = vec![0; MAX_IN_OPEN + 1];
    PARAM_STACK = vec![0; PARAM_SIZE + 1];
    HYPH_WORD = vec![0; HYPH_SIZE + 1];
    HYPH_LIST = vec![Some(0); HYPH_SIZE + 1];
    HYPH_LINK = vec![0; HYPH_SIZE + 1];

    /* First bit of initex handling: more allocations. */

    if in_initex_mode {
        MEM = vec![memory_word::default(); MEM_TOP as usize + 2];
        EQTB_TOP = EQTB_SIZE + hash_extra as usize;
        if hash_extra == 0 {
            hash_top = UNDEFINED_CONTROL_SEQUENCE as i32;
        } else {
            hash_top = EQTB_TOP as i32;
        }
        yhash = xmalloc_array((1 + hash_top - hash_offset) as usize);
        hash = yhash.offset(-514);
        (*hash.offset((HASH_BASE) as isize)).s0 = 0;
        (*hash.offset((HASH_BASE) as isize)).s1 = 0;
        hash_used = HASH_BASE as i32 + 1;
        while hash_used <= hash_top {
            *hash.offset(hash_used as isize) = *hash.offset(HASH_BASE as isize);
            hash_used += 1
        }
        EQTB = vec![EqtbWord::default(); EQTB_TOP + 1];
        str_start = vec![pool_pointer::default(); max_strings + 1];
        str_pool = vec![0; pool_size as usize + 1];
        FONT_INFO = vec![memory_word::default(); FONT_MEM_SIZE + 1];
    }
    /* Sanity-check various invariants. */
    history = TTHistory::FATAL_ERROR;
    bad = 0i32;
    if half_error_line < 30 || half_error_line > error_line - 15 {
        bad = 1
    }
    if max_print_line < 60 {
        bad = 2
    }
    if 1100 > MEM_TOP {
        bad = 4
    }
    if HASH_PRIME > HASH_SIZE {
        bad = 5
    }
    if MAX_IN_OPEN >= 128 {
        bad = 6
    }
    if MEM_TOP < 267 {
        bad = 7
    }
    if MIN_HALFWORD > 0 {
        bad = 12
    }
    if (MAX_FONT_MAX as i32) < MIN_HALFWORD || (MAX_FONT_MAX as i32) > MAX_HALFWORD {
        bad = 15
    }
    if FONT_MAX > FONT_BASE + 9000 {
        bad = 16
    }
    if SAVE_SIZE as i32 > MAX_HALFWORD || max_strings as i32 > MAX_HALFWORD {
        bad = 17
    }
    if BUF_SIZE > MAX_HALFWORD as usize {
        bad = 18
    }
    if CS_TOKEN_FLAG as i32 + EQTB_SIZE as i32 + hash_extra > MAX_HALFWORD {
        bad = 21
    }
    if 514 < 0 || 514 > HASH_BASE {
        bad = 42
    }
    if TEX_format_default.as_bytes().len() > std::i32::MAX as usize {
        bad = 31
    }
    if 2 * MAX_HALFWORD < MEM_TOP as i32 {
        bad = 41
    }
    if bad > 0 {
        panic!("failed internal consistency check #{}", bad,);
    }

    /* OK, ready to keep on initializing. */

    initialize_more_variables();

    if in_initex_mode {
        get_strings_started();
        initialize_more_initex_variables();
        initialize_primitives();
        init_str_ptr = str_ptr;
        init_pool_ptr = pool_ptr
    }

    /*55:*/
    initialize_pagebuilder_variables();
    initialize_shipout_variables();

    selector = Selector::TERM_ONLY;
    tally = 0i32;
    term_offset = 0i32;
    file_offset = 0i32;
    job_name = 0i32;
    name_in_progress = false;
    log_opened = false;

    if semantic_pagination_enabled {
        output_file_extension = ".spx".to_string();
    } else {
        output_file_extension = ".xdv".to_string();
    }

    INPUT_PTR = 0;
    MAX_IN_STACK = 0;
    SOURCE_FILENAME_STACK[0] = 0;
    FULL_SOURCE_FILENAME_STACK[0] = 0;
    IN_OPEN = 0;
    open_parens = 0i32;
    max_buf_stack = 0i32;
    GRP_STACK[0] = 0;
    IF_STACK[0] = None;
    PARAM_PTR = 0;
    MAX_PARAM_STACK = 0;
    used_tectonic_coda_tokens = false;
    gave_char_warning_help = false;

    /*memset(
        buffer as *mut libc::c_void,
        0i32,
        (BUF_SIZE as usize).wrapping_mul(::std::mem::size_of::<UnicodeScalar>()),
    );*/
    first = 0;

    scanner_status = ScannerStatus::Normal;
    warning_index = None.tex_int();
    first = 1;
    cur_input.state = InputState::NewLine;
    cur_input.start = 1;
    cur_input.index = Btl::Parameter;
    line = 0;
    cur_input.name = 0;
    force_eof = false;
    align_state = 1000000;

    init_io(&mut cur_input);

    if in_initex_mode {
        no_new_control_sequence = false;

        primitive("XeTeXpicfile", Cmd::Extension, PIC_FILE_CODE as i32);
        primitive("XeTeXpdffile", Cmd::Extension, PDF_FILE_CODE as i32);
        primitive("XeTeXglyph", Cmd::Extension, GLYPH_CODE as i32);
        primitive(
            "XeTeXlinebreaklocale",
            Cmd::Extension,
            XETEX_LINEBREAK_LOCALE_EXTENSION_CODE as i32,
        );
        primitive(
            "pdfsavepos",
            Cmd::Extension,
            PDFTEX_FIRST_EXTENSION_CODE as i32 + 0,
        );

        primitive("lastnodetype", Cmd::LastItem, LastItemCode::LastNodeType);
        primitive("eTeXversion", Cmd::LastItem, LastItemCode::EtexVersion);

        primitive("eTeXrevision", Cmd::Convert, ConvertCode::EtexRevision);

        primitive("XeTeXversion", Cmd::LastItem, LastItemCode::XetexVersion);

        primitive("XeTeXrevision", Cmd::Convert, ConvertCode::XetexRevision);

        primitive(
            "XeTeXcountglyphs",
            Cmd::LastItem,
            LastItemCode::XetexCountGlyphs,
        );
        primitive(
            "XeTeXcountvariations",
            Cmd::LastItem,
            LastItemCode::XetexCountVariations,
        );
        primitive(
            "XeTeXvariation",
            Cmd::LastItem,
            LastItemCode::XetexVariation,
        );
        primitive(
            "XeTeXfindvariationbyname",
            Cmd::LastItem,
            LastItemCode::XetexFindVariationByName,
        );
        primitive(
            "XeTeXvariationmin",
            Cmd::LastItem,
            LastItemCode::XetexVariationMin,
        );
        primitive(
            "XeTeXvariationmax",
            Cmd::LastItem,
            LastItemCode::XetexVariationMax,
        );
        primitive(
            "XeTeXvariationdefault",
            Cmd::LastItem,
            LastItemCode::XetexVariationDefault,
        );
        primitive(
            "XeTeXcountfeatures",
            Cmd::LastItem,
            LastItemCode::XetexCountFeatures,
        );
        primitive(
            "XeTeXfeaturecode",
            Cmd::LastItem,
            LastItemCode::XetexFeatureCode,
        );
        primitive(
            "XeTeXfindfeaturebyname",
            Cmd::LastItem,
            LastItemCode::XetexFindFeatureByName,
        );
        primitive(
            "XeTeXisexclusivefeature",
            Cmd::LastItem,
            LastItemCode::XetexIsExclusiveFeature,
        );
        primitive(
            "XeTeXcountselectors",
            Cmd::LastItem,
            LastItemCode::XetexCountSelectors,
        );
        primitive(
            "XeTeXselectorcode",
            Cmd::LastItem,
            LastItemCode::XetexSelectorCode,
        );
        primitive(
            "XeTeXfindselectorbyname",
            Cmd::LastItem,
            LastItemCode::XetexFindSelectorByName,
        );
        primitive(
            "XeTeXisdefaultselector",
            Cmd::LastItem,
            LastItemCode::XetexIsDefaultSelector,
        );

        primitive(
            "XeTeXvariationname",
            Cmd::Convert,
            ConvertCode::XetexVariationName,
        );
        primitive(
            "XeTeXfeaturename",
            Cmd::Convert,
            ConvertCode::XetexFeatureName,
        );
        primitive(
            "XeTeXselectorname",
            Cmd::Convert,
            ConvertCode::XetexSelectorName,
        );

        primitive(
            "XeTeXOTcountscripts",
            Cmd::LastItem,
            LastItemCode::XetexOTCountScripts,
        );
        primitive(
            "XeTeXOTcountlanguages",
            Cmd::LastItem,
            LastItemCode::XetexOTCountLanguages,
        );
        primitive(
            "XeTeXOTcountfeatures",
            Cmd::LastItem,
            LastItemCode::XetexOTCountFeatures,
        );
        primitive(
            "XeTeXOTscripttag",
            Cmd::LastItem,
            LastItemCode::XetexOTScript,
        );
        primitive(
            "XeTeXOTlanguagetag",
            Cmd::LastItem,
            LastItemCode::XetexOTLanguage,
        );
        primitive(
            "XeTeXOTfeaturetag",
            Cmd::LastItem,
            LastItemCode::XetexOTFeature,
        );
        primitive(
            "XeTeXcharglyph",
            Cmd::LastItem,
            LastItemCode::XetexMapCharToGlyph,
        );
        primitive(
            "XeTeXglyphindex",
            Cmd::LastItem,
            LastItemCode::XetexGlyphIndex,
        );
        primitive(
            "XeTeXglyphbounds",
            Cmd::LastItem,
            LastItemCode::XetexGlyphBounds,
        );

        primitive("XeTeXglyphname", Cmd::Convert, ConvertCode::XetexGlyphName);

        primitive("XeTeXfonttype", Cmd::LastItem, LastItemCode::XetexFontType);
        primitive(
            "XeTeXfirstfontchar",
            Cmd::LastItem,
            LastItemCode::XetexFirstChar,
        );
        primitive(
            "XeTeXlastfontchar",
            Cmd::LastItem,
            LastItemCode::XetexLastChar,
        );
        primitive("pdflastxpos", Cmd::LastItem, LastItemCode::PdfLastXPos);
        primitive("pdflastypos", Cmd::LastItem, LastItemCode::PdfLastYPos);

        primitive("strcmp", Cmd::Convert, ConvertCode::PdfStrcmp);
        primitive("mdfivesum", Cmd::Convert, ConvertCode::PdfMdfiveSum);
        primitive("pdfmdfivesum", Cmd::Convert, ConvertCode::PdfMdfiveSum);

        primitive("shellescape", Cmd::LastItem, LastItemCode::PdfShellEscape);
        primitive(
            "XeTeXpdfpagecount",
            Cmd::LastItem,
            LastItemCode::XetexPdfPageCount,
        );

        primitive(
            "tracingassigns",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_assigns as usize,
        );
        primitive(
            "tracinggroups",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_groups as usize,
        );
        primitive(
            "tracingifs",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_ifs as usize,
        );
        primitive(
            "tracingscantokens",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_scan_tokens as usize,
        );
        primitive(
            "tracingnesting",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_nesting as usize,
        );
        primitive(
            "predisplaydirection",
            Cmd::AssignInt,
            INT_BASE + IntPar::pre_display_correction as usize,
        );
        primitive(
            "lastlinefit",
            Cmd::AssignInt,
            INT_BASE + IntPar::last_line_fit as usize,
        );
        primitive(
            "savingvdiscards",
            Cmd::AssignInt,
            INT_BASE + IntPar::saving_vdiscards as usize,
        );
        primitive(
            "savinghyphcodes",
            Cmd::AssignInt,
            INT_BASE + IntPar::saving_hyphs as usize,
        );

        primitive(
            "currentgrouplevel",
            Cmd::LastItem,
            LastItemCode::CurrentGroupLevel,
        );
        primitive(
            "currentgrouptype",
            Cmd::LastItem,
            LastItemCode::CurrentGroupType,
        );
        primitive(
            "currentiflevel",
            Cmd::LastItem,
            LastItemCode::CurrentIfLevel,
        );
        primitive("currentiftype", Cmd::LastItem, LastItemCode::CurrentIfType);
        primitive(
            "currentifbranch",
            Cmd::LastItem,
            LastItemCode::CurrentIfBranch,
        );
        primitive("fontcharwd", Cmd::LastItem, LastItemCode::FontCharWd);
        primitive("fontcharht", Cmd::LastItem, LastItemCode::FontCharHt);
        primitive("fontchardp", Cmd::LastItem, LastItemCode::FontCharDp);
        primitive("fontcharic", Cmd::LastItem, LastItemCode::FontCharIc);
        primitive(
            "parshapelength",
            Cmd::LastItem,
            LastItemCode::ParShapeLength,
        );
        primitive(
            "parshapeindent",
            Cmd::LastItem,
            LastItemCode::ParShapeIndent,
        );
        primitive("parshapedimen", Cmd::LastItem, LastItemCode::ParShapeDimen);

        primitive("showgroups", Cmd::XRay, SHOW_GROUPS);
        primitive("showtokens", Cmd::XRay, SHOW_TOKENS);

        primitive("unexpanded", Cmd::The, 1);
        primitive("detokenize", Cmd::The, SHOW_TOKENS);

        primitive("showifs", Cmd::XRay, SHOW_IFS);

        primitive("interactionmode", Cmd::SetPageInt, 2);

        primitive("middle", Cmd::LeftRight, 1);

        primitive(
            "suppressfontnotfounderror",
            Cmd::AssignInt,
            INT_BASE + IntPar::suppress_fontnotfound_error as usize,
        );

        primitive(
            "TeXXeTstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::texxet as usize,
        );
        primitive(
            "XeTeXupwardsmode",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_upwards as usize,
        );
        primitive(
            "XeTeXuseglyphmetrics",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_use_glyph_metrics as usize,
        );
        primitive(
            "XeTeXinterchartokenstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_inter_char_tokens as usize,
        );
        primitive(
            "XeTeXdashbreakstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_dash_break as usize,
        );
        primitive(
            "XeTeXinputnormalization",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_input_normalization as usize,
        );
        primitive(
            "XeTeXtracingfonts",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_tracing_fonts as usize,
        );
        primitive(
            "XeTeXinterwordspaceshaping",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_interword_space_shaping as usize,
        );
        primitive(
            "XeTeXgenerateactualtext",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_generate_actual_text as usize,
        );
        primitive(
            "XeTeXhyphenatablelength",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_hyphenatable_length as usize,
        );
        primitive(
            "pdfoutput",
            Cmd::AssignInt,
            INT_BASE + IntPar::pdfoutput as usize,
        );

        primitive(
            "XeTeXinputencoding",
            Cmd::Extension,
            XETEX_INPUT_ENCODING_EXTENSION_CODE as usize,
        );
        primitive(
            "XeTeXdefaultencoding",
            Cmd::Extension,
            XETEX_DEFAULT_ENCODING_EXTENSION_CODE as usize,
        );

        primitive(
            "beginL",
            Cmd::VAlign,
            u16::from(MathType::Eq(BE::Begin, MathMode::Left)),
        );
        primitive(
            "endL",
            Cmd::VAlign,
            u16::from(MathType::Eq(BE::End, MathMode::Left)),
        );
        primitive(
            "beginR",
            Cmd::VAlign,
            u16::from(MathType::Eq(BE::Begin, MathMode::Right)),
        );
        primitive(
            "endR",
            Cmd::VAlign,
            u16::from(MathType::Eq(BE::End, MathMode::Right)),
        );

        primitive("scantokens", Cmd::Input, 2);
        primitive("readline", Cmd::ReadToCS, 1);
        primitive("unless", Cmd::ExpandAfter, 1);

        primitive("ifdefined", Cmd::IfTest, IfTestCode::IfDef);
        primitive("ifcsname", Cmd::IfTest, IfTestCode::IfCS);
        primitive("iffontchar", Cmd::IfTest, IfTestCode::IfFontChar);
        primitive("ifincsname", Cmd::IfTest, IfTestCode::IfInCSName);

        primitive("protected", Cmd::Prefix, 8);

        primitive("numexpr", Cmd::LastItem, LastItemCode::EtexExprInt);
        primitive("dimexpr", Cmd::LastItem, LastItemCode::EtexExprDimen);
        primitive("glueexpr", Cmd::LastItem, LastItemCode::EtexExprGlue);
        primitive("muexpr", Cmd::LastItem, LastItemCode::EtexExprMu);
        primitive(
            "gluestretchorder",
            Cmd::LastItem,
            LastItemCode::GlueStretchOrder,
        );
        primitive(
            "glueshrinkorder",
            Cmd::LastItem,
            LastItemCode::GlueShrinkOrder,
        );
        primitive("gluestretch", Cmd::LastItem, LastItemCode::GlueStretch);
        primitive("glueshrink", Cmd::LastItem, LastItemCode::GlueShrink);
        primitive("mutoglue", Cmd::LastItem, LastItemCode::MuToGlue);
        primitive("gluetomu", Cmd::LastItem, LastItemCode::GlueToMu);

        primitive("marks", Cmd::Mark, 5);
        primitive("topmarks", Cmd::TopBotMark, TOP_MARK_CODE + 5);
        primitive("firstmarks", Cmd::TopBotMark, FIRST_MARK_CODE + 5);
        primitive("botmarks", Cmd::TopBotMark, BOT_MARK_CODE + 5);
        primitive(
            "splitfirstmarks",
            Cmd::TopBotMark,
            SPLIT_FIRST_MARK_CODE + 5,
        );
        primitive("splitbotmarks", Cmd::TopBotMark, SPLIT_BOT_MARK_CODE + 5);

        primitive("pagediscards", Cmd::UnVBox, BoxCode::LastBox);
        primitive("splitdiscards", Cmd::UnVBox, BoxCode::VSplit);

        primitive(
            "interlinepenalties",
            Cmd::SetShape,
            INTER_LINE_PENALTIES_LOC as i32,
        );
        primitive("clubpenalties", Cmd::SetShape, CLUB_PENALTIES_LOC as i32);
        primitive("widowpenalties", Cmd::SetShape, WIDOW_PENALTIES_LOC as i32);
        primitive(
            "displaywidowpenalties",
            Cmd::SetShape,
            DISPLAY_WIDOW_PENALTIES_LOC as i32,
        );
        max_reg_num = 32767;
        max_reg_help_line = "A register number must be between 0 and 32767.";
    }
    no_new_control_sequence = true;

    if !in_initex_mode {
        if !load_fmt_file() {
            return history;
        }
    }

    if *INTPAR(IntPar::end_line_char) < 0 || *INTPAR(IntPar::end_line_char) < BIGGEST_CHAR {
        cur_input.limit -= 1
    } else {
        BUFFER[cur_input.limit as usize] = *INTPAR(IntPar::end_line_char);
    }

    if in_initex_mode {
        /* TeX initializes with the real date and time, but for format file
         * reproducibility we do this: */
        *INTPAR(IntPar::time) = 0;
        *INTPAR(IntPar::day) = 0;
        *INTPAR(IntPar::month) = 0;
        *INTPAR(IntPar::year) = 0;
    } else {
        let (minutes, day, month, year) = get_date_and_time();
        *INTPAR(IntPar::time) = minutes;
        *INTPAR(IntPar::day) = day;
        *INTPAR(IntPar::month) = month;
        *INTPAR(IntPar::year) = year;
    }
    if trie_not_ready {
        trie_trl = vec![0; trie_size as usize + 1];
        trie_tro = vec![0; trie_size as usize + 1];
        trie_trc = vec![0; trie_size as usize + 1];
        trie_c = vec![0; trie_size as usize + 1];
        trie_o = vec![0; trie_size as usize + 1];
        trie_l = vec![0; trie_size as usize + 1];
        trie_r = vec![0; trie_size as usize + 1];
        trie_hash = vec![0; trie_size as usize + 1];
        trie_taken = vec![false; trie_size as usize + 1];
        trie_l[0] = 0;
        trie_c[0] = 0;
        trie_ptr = 0;
        trie_r[0] = 0;
        hyph_start = 0;
        FONT_MAPPING = vec![0 as *mut libc::c_void; FONT_MAX + 1];
        FONT_LAYOUT_ENGINE.clear();
        for _ in 0..FONT_MAX + 1 {
            FONT_LAYOUT_ENGINE.push(Font::None);
        }
        FONT_FLAGS = vec![0; FONT_MAX + 1];
        FONT_LETTER_SPACE = vec![Scaled::ZERO; FONT_MAX + 1];
        FONT_CHECK = vec![b16x4_le_t::default(); FONT_MAX + 1];
        FONT_SIZE = vec![Scaled::ZERO; FONT_MAX + 1];
        FONT_DSIZE = vec![Scaled::ZERO; FONT_MAX + 1];
        FONT_PARAMS = vec![0; FONT_MAX + 1];
        FONT_NAME = vec![0; FONT_MAX + 1];
        FONT_AREA = vec![0; FONT_MAX + 1];
        FONT_BC = vec![0; FONT_MAX + 1];
        FONT_EC = vec![0; FONT_MAX + 1];
        FONT_GLUE = vec![0; FONT_MAX + 1];
        HYPHEN_CHAR = vec![0; FONT_MAX + 1];
        SKEW_CHAR = vec![0; FONT_MAX + 1];
        BCHAR_LABEL = vec![0; FONT_MAX + 1];
        FONT_BCHAR = vec![0; FONT_MAX + 1];
        FONT_FALSE_BCHAR = vec![0; FONT_MAX + 1];
        CHAR_BASE = vec![0; FONT_MAX + 1];
        WIDTH_BASE = vec![0; FONT_MAX + 1];
        HEIGHT_BASE = vec![0; FONT_MAX + 1];
        DEPTH_BASE = vec![0; FONT_MAX + 1];
        ITALIC_BASE = vec![0; FONT_MAX + 1];
        LIG_KERN_BASE = vec![0; FONT_MAX + 1];
        KERN_BASE = vec![0; FONT_MAX + 1];
        EXTEN_BASE = vec![0; FONT_MAX + 1];
        PARAM_BASE = vec![0; FONT_MAX + 1];
        FONT_PTR = 0;
        fmem_ptr = 7;
        FONT_NAME[0] = maketexstring("nullfont");
        FONT_AREA[0] = EMPTY_STRING;
        HYPHEN_CHAR[0] = '-' as i32;
        SKEW_CHAR[0] = -1;
        BCHAR_LABEL[0] = NON_ADDRESS;
        FONT_BCHAR[0] = TOO_BIG_CHAR;
        FONT_FALSE_BCHAR[0] = TOO_BIG_CHAR;
        FONT_BC[0] = 1;
        FONT_EC[0] = 0;
        FONT_SIZE[0] = Scaled::ZERO;
        FONT_DSIZE[0] = Scaled::ZERO;
        CHAR_BASE[0] = 0;
        WIDTH_BASE[0] = 0;
        HEIGHT_BASE[0] = 0;
        DEPTH_BASE[0] = 0;
        ITALIC_BASE[0] = 0;
        LIG_KERN_BASE[0] = 0;
        KERN_BASE[0] = 0;
        EXTEN_BASE[0] = 0;
        FONT_GLUE[0] = None.tex_int();
        FONT_PARAMS[0] = 7;
        FONT_MAPPING[0] = 0 as *mut libc::c_void;
        PARAM_BASE[0] = -1;

        for font_k in 0..7 {
            FONT_INFO[font_k].b32.s1 = 0;
        }
    }

    font_used = vec![false; FONT_MAX + 1];

    selector = if interaction == InteractionMode::Batch {
        Selector::NO_PRINT
    } else {
        Selector::TERM_ONLY
    };
    if semantic_pagination_enabled {
        *INTPAR(IntPar::xetex_generate_actual_text) = 1;
    }
    pdf_files_init();
    synctex_init_command();
    start_input(&mut cur_input, input_file_name);
    history = TTHistory::SPOTLESS;
    main_control(&mut cur_input);
    final_cleanup(&mut cur_input);
    close_files_and_terminate();
    tt_cleanup();

    history
}
