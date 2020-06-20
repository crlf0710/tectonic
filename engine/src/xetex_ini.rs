#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use bridge::DisplayExt;
use std::ffi::CStr;
use std::io::{Read, Write};
use std::ptr;

use super::xetex_texmfmp::get_date_and_time;
use crate::core_memory::{mfree, xmalloc, xmalloc_array};
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, overflow};
use crate::xetex_ext::release_font_engine;
use crate::xetex_ext::{AAT_FONT_FLAG, OTGR_FONT_FLAG};
use crate::xetex_layout_interface::{destroy_font_manager, set_cp_code};
use crate::xetex_math::initialize_math_variables;
use crate::xetex_output::{
    print, print_char, print_cstr, print_esc, print_esc_cstr, print_file_line, print_file_name,
    print_int, print_ln, print_nl, print_nl_cstr, print_scaled,
};
use crate::xetex_pagebuilder::initialize_pagebuilder_variables;
use crate::xetex_shipout::{deinitialize_shipout_variables, initialize_shipout_variables};
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_stringpool::{length, load_pool_strings, make_string};
use crate::xetex_synctex::synctex_init_command;
use crate::xetex_texmfmp::maketexstring;
use crate::xetex_xetex0::{
    alter_aux, alter_box_dimen, alter_integer, alter_page_so_far, alter_prev_graf, back_error,
    back_input, begin_diagnostic, close_files_and_terminate, delete_glue_ref, delete_token_ref,
    do_marks, do_register_command, end_diagnostic, end_file_reading, end_token_list, eq_define,
    eq_word_define, find_font_dimen, find_sa_element, flush_list, flush_node_list, free_node,
    geq_define, geq_word_define, get_avail, get_node, get_r_token, get_token, get_x_token, gsa_def,
    id_lookup, main_control, make_name_string, max_hyphenatable_length, new_font, new_interaction,
    open_log_file, pack_job_name, prim_lookup, print_cmd_chr, pseudo_close, read_toks, sa_def,
    scan_box, scan_char_class, scan_char_class_not_ignored, scan_char_num, scan_dimen,
    scan_fifteen_bit_int, scan_font_ident, scan_glue, scan_glyph_number, scan_int, scan_keyword,
    scan_left_brace, scan_math_class_int, scan_math_fam_int, scan_optional_equals,
    scan_register_num, scan_toks, scan_usv_num, scan_xetex_math_char_int, show_cur_cmd_chr,
    show_save_groups, start_input, trap_zero_glue,
};
use crate::xetex_xetexd::{set_class, set_family, LLIST_link, TeXOpt, UTF8};
use bridge::{
    ttstub_input_close, ttstub_input_open, ttstub_output_close, ttstub_output_open,
    ttstub_output_open_stdout,
};
use dpx::{pdf_files_close, pdf_files_init};
use libc::{free, strcpy, strlen};

pub(crate) type uintptr_t = u64;
pub(crate) type size_t = usize;
pub(crate) type ssize_t = isize;
/* tectonic/core-bridge.h: declarations of C/C++ => Rust bridge API
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/

use bridge::{TTHistory, TTInputFormat};

use bridge::InputHandleWrapper;
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
pub(crate) type scaled_t = i32;

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

/// Magic constant, origin unclear
const sup_max_strings: i32 = 2097151;
const sup_hash_extra: i32 = sup_max_strings;
/// "TTNC" in ASCII
const FORMAT_HEADER_MAGIC: i32 = 0x54544E43;
const FORMAT_FOOTER_MAGIC: i32 = 0x0000029A;
const sup_pool_size: i32 = 40000000;
/// magic constant, origin unclear
const sup_font_mem_size: i32 = 147483647;

pub(crate) const MIN_TRIE_OP: u16 = 0;
pub(crate) const MAX_TRIE_OP: u16 = 65535;
const TRIE_OP_SIZE: i32 = 35111;
const NEG_TRIE_OP_SIZE: i32 = -35111;
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
/* WEB: font(lig_char(p)) */
/* WEB: character(lig_char(p)) */
/* WEB: link(lig_char(p)) */
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
pub(crate) type trie_pointer = i32;
pub(crate) type trie_opcode = u16;
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
#[no_mangle]
pub(crate) static mut EQTB: Vec<EqtbWord> = Vec::new();
#[no_mangle]
pub(crate) static mut bad: i32 = 0;
#[no_mangle]
pub(crate) static mut name_of_file: *mut i8 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut name_of_file16: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut name_length: i32 = 0;
#[no_mangle]
pub(crate) static mut name_length16: i32 = 0;
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
pub(crate) static mut trie_size: i32 = 0;
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
pub(crate) static mut quoted_filename: bool = false;
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
pub(crate) static mut interaction: u8 = 0;
#[no_mangle]
pub(crate) static mut deletions_allowed: bool = false;
#[no_mangle]
pub(crate) static mut set_box_allowed: bool = false;
#[no_mangle]
pub(crate) static mut history: TTHistory = TTHistory::SPOTLESS;
#[no_mangle]
pub(crate) static mut error_count: i8 = 0;
#[no_mangle]
pub(crate) static mut help_line: [&'static [u8]; 6] = [&[]; 6];
#[no_mangle]
pub(crate) static mut help_ptr: u8 = 0;
#[no_mangle]
pub(crate) static mut use_err_help: bool = false;
#[no_mangle]
pub(crate) static mut arith_error: bool = false;
#[no_mangle]
pub(crate) static mut tex_remainder: scaled_t = 0;
#[no_mangle]
pub(crate) static mut temp_ptr: i32 = 0;
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
pub(crate) static mut avail: i32 = 0;
#[no_mangle]
pub(crate) static mut mem_end: i32 = 0;
#[no_mangle]
pub(crate) static mut rover: i32 = 0;
#[no_mangle]
pub(crate) static mut last_leftmost_char: i32 = 0;
#[no_mangle]
pub(crate) static mut last_rightmost_char: i32 = 0;
#[no_mangle]
pub(crate) static mut hlist_stack: [i32; 513] = [0; 513];
#[no_mangle]
pub(crate) static mut hlist_stack_level: i16 = 0;
#[no_mangle]
pub(crate) static mut first_p: i32 = 0;
#[no_mangle]
pub(crate) static mut global_prev_p: i32 = 0;
#[no_mangle]
pub(crate) static mut font_in_short_display: i32 = 0;
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
pub(crate) static mut old_setting: Selector = Selector::FILE_0;
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
pub(crate) static mut INPUT_FILE: Vec<*mut UFILE> = Vec::new();
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
pub(crate) static mut BASE_PTR: usize = 0;
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
pub(crate) static mut cur_mark: [i32; 5] = [0; 5];
#[no_mangle]
pub(crate) static mut long_state: u8 = 0;
#[no_mangle]
pub(crate) static mut pstack: [i32; 9] = [0; 9];
#[no_mangle]
pub(crate) static mut cur_val: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_val1: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_val_level: u8 = 0;
#[no_mangle]
pub(crate) static mut radix: i16 = 0;
#[no_mangle]
pub(crate) static mut cur_order: glue_ord = 0;
#[no_mangle]
pub(crate) static mut read_file: [*mut UFILE; 16] = [ptr::null_mut(); 16];
#[no_mangle]
pub(crate) static mut read_open: [u8; 17] = [0; 17];
#[no_mangle]
pub(crate) static mut cond_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut if_limit: u8 = 0;
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
pub(crate) static mut area_delimiter: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut ext_delimiter: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut file_name_quote_char: UTF16_code = 0;
#[no_mangle]
pub(crate) static mut format_default_length: i32 = 0;
#[no_mangle]
pub(crate) static mut TEX_format_default: *mut i8 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut name_in_progress: bool = false;
#[no_mangle]
pub(crate) static mut job_name: str_number = 0;
#[no_mangle]
pub(crate) static mut log_opened: bool = false;
#[no_mangle]
pub(crate) static mut output_file_extension: *const i8 = ptr::null();
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
#[no_mangle]
pub(crate) static mut FONT_SIZE: Vec<scaled_t> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_DSIZE: Vec<scaled_t> = Vec::new();
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
#[no_mangle]
pub(crate) static mut FONT_LAYOUT_ENGINE: Vec<*mut libc::c_void> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_MAPPING: Vec<*mut libc::c_void> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_FLAGS: Vec<i8> = Vec::new();
#[no_mangle]
pub(crate) static mut FONT_LETTER_SPACE: Vec<scaled_t> = Vec::new();
#[no_mangle]
pub(crate) static mut loaded_font_mapping: *mut libc::c_void = ptr::null_mut();
#[no_mangle]
pub(crate) static mut loaded_font_flags: i8 = 0;
#[no_mangle]
pub(crate) static mut loaded_font_letter_space: scaled_t = 0;
#[no_mangle]
pub(crate) static mut loaded_font_design_size: scaled_t = 0;
#[no_mangle]
pub(crate) static mut mapped_text: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut xdv_buffer: *mut i8 = ptr::null_mut();
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
pub(crate) static mut max_v: scaled_t = 0;
#[no_mangle]
pub(crate) static mut max_h: scaled_t = 0;
#[no_mangle]
pub(crate) static mut max_push: i32 = 0;
#[no_mangle]
pub(crate) static mut last_bop: i32 = 0;
#[no_mangle]
pub(crate) static mut dead_cycles: i32 = 0;
#[no_mangle]
pub(crate) static mut doing_leaders: bool = false;
#[no_mangle]
pub(crate) static mut rule_ht: scaled_t = 0;
#[no_mangle]
pub(crate) static mut rule_dp: scaled_t = 0;
#[no_mangle]
pub(crate) static mut rule_wd: scaled_t = 0;
#[no_mangle]
pub(crate) static mut cur_h: scaled_t = 0;
#[no_mangle]
pub(crate) static mut cur_v: scaled_t = 0;
#[no_mangle]
pub(crate) static mut total_stretch: [scaled_t; 4] = [0; 4];
#[no_mangle]
pub(crate) static mut total_shrink: [scaled_t; 4] = [0; 4];
#[no_mangle]
pub(crate) static mut last_badness: i32 = 0;
#[no_mangle]
pub(crate) static mut adjust_tail: i32 = 0;
#[no_mangle]
pub(crate) static mut pre_adjust_tail: i32 = 0;
#[no_mangle]
pub(crate) static mut pack_begin_line: i32 = 0;
#[no_mangle]
pub(crate) static mut empty: b32x2 = b32x2 { s0: 0, s1: 0 };
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
pub(crate) static mut cur_align: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_span: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_loop: i32 = 0;
#[no_mangle]
pub(crate) static mut align_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_head: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_tail: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_pre_head: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_pre_tail: i32 = 0;
#[no_mangle]
pub(crate) static mut just_box: i32 = 0;
#[no_mangle]
pub(crate) static mut active_width: [scaled_t; 7] = [0; 7];
#[no_mangle]
pub(crate) static mut hc: [i32; 4099] = [0; 4099];
#[no_mangle]
pub(crate) static mut hf: internal_font_number = 0;
#[no_mangle]
pub(crate) static mut hu: [i32; 4097] = [0; 4097];
#[no_mangle]
pub(crate) static mut cur_lang: u8 = 0;
#[no_mangle]
pub(crate) static mut max_hyph_char: i32 = 0;
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
pub(crate) static mut lig_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut ligature_present: bool = false;
#[no_mangle]
pub(crate) static mut lft_hit: bool = false;
#[no_mangle]
pub(crate) static mut rt_hit: bool = false;
#[no_mangle]
pub(crate) static mut trie_trl: *mut trie_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_tro: *mut trie_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_trc: *mut u16 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hyf_distance: [i16; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut hyf_num: [i16; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut hyf_next: [trie_opcode; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut op_start: [i32; 256] = [0; 256];
#[no_mangle]
pub(crate) static mut HYPH_WORD: Vec<str_number> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_LIST: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_LINK: Vec<hyph_pointer> = Vec::new();
#[no_mangle]
pub(crate) static mut HYPH_COUNT: usize = 0;
#[no_mangle]
pub(crate) static mut HYPH_NEXT: usize = 0;
#[no_mangle]
pub(crate) static mut trie_used: [trie_opcode; 256] = [0; 256];
#[no_mangle]
pub(crate) static mut trie_op_lang: [u8; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut trie_op_val: [trie_opcode; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut trie_op_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut max_op_used: trie_opcode = 0;
#[no_mangle]
pub(crate) static mut trie_c: *mut packed_UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_o: *mut trie_opcode = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_l: *mut trie_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_r: *mut trie_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_ptr: trie_pointer = 0;
#[no_mangle]
pub(crate) static mut trie_hash: *mut trie_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_taken: *mut bool = ptr::null_mut();
#[no_mangle]
pub(crate) static mut trie_min: [trie_pointer; 65536] = [0; 65536];
#[no_mangle]
pub(crate) static mut trie_max: trie_pointer = 0;
#[no_mangle]
pub(crate) static mut trie_not_ready: bool = false;
#[no_mangle]
pub(crate) static mut best_height_plus_depth: scaled_t = 0;
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
pub(crate) static mut main_p: i32 = 0;
#[no_mangle]
pub(crate) static mut main_pp: i32 = 0;
#[no_mangle]
pub(crate) static mut main_ppp: i32 = 0;
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
pub(crate) static mut cur_box: i32 = 0;
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
pub(crate) static mut cur_page_width: scaled_t = 0;
#[no_mangle]
pub(crate) static mut cur_page_height: scaled_t = 0;
#[no_mangle]
pub(crate) static mut cur_h_offset: scaled_t = 0;
#[no_mangle]
pub(crate) static mut cur_v_offset: scaled_t = 0;
#[no_mangle]
pub(crate) static mut pdf_last_x_pos: i32 = 0;
#[no_mangle]
pub(crate) static mut pdf_last_y_pos: i32 = 0;
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
pub(crate) static mut IF_STACK: Vec<i32> = Vec::new();
#[no_mangle]
pub(crate) static mut max_reg_num: i32 = 0;
#[no_mangle]
pub(crate) static mut max_reg_help_line: &[u8] = &[];
#[no_mangle]
pub(crate) static mut sa_root: [i32; 8] = [0; 8];
#[no_mangle]
pub(crate) static mut cur_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut sa_null: memory_word = memory_word {
    b32: b32x2 { s0: 0, s1: 0 },
};
#[no_mangle]
pub(crate) static mut sa_chain: i32 = 0;
#[no_mangle]
pub(crate) static mut sa_level: u16 = 0;
#[no_mangle]
pub(crate) static mut hyph_start: trie_pointer = 0;
#[no_mangle]
pub(crate) static mut hyph_index: trie_pointer = 0;
#[no_mangle]
pub(crate) static mut disc_ptr: [i32; 4] = [0; 4];
#[no_mangle]
pub(crate) static mut edit_name_start: pool_pointer = 0;
#[no_mangle]
pub(crate) static mut stop_at_space: bool = false;
#[no_mangle]
pub(crate) static mut native_font_type_flag: i32 = 0;
#[no_mangle]
pub(crate) static mut xtx_ligature_present: bool = false;
#[no_mangle]
pub(crate) static mut delta: scaled_t = 0;
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
pub(crate) static mut page_tail: i32 = 0;
#[no_mangle]
pub(crate) static mut page_contents: PageContents = PageContents::Empty;
#[no_mangle]
pub(crate) static mut page_so_far: [scaled_t; 8] = [0; 8];
#[no_mangle]
pub(crate) static mut last_glue: i32 = 0;
#[no_mangle]
pub(crate) static mut last_penalty: i32 = 0;
#[no_mangle]
pub(crate) static mut last_kern: scaled_t = 0;
#[no_mangle]
pub(crate) static mut last_node_type: i32 = 0;
#[no_mangle]
pub(crate) static mut insert_penalties: i32 = 0;
#[no_mangle]
pub(crate) static mut output_active: bool = false;
#[no_mangle]
pub(crate) static mut _xeq_level_array: [u16; 1114732] = [0; 1114732];
static mut _trie_op_hash_array: [i32; 70223] = [0; 70223];
static mut yhash: *mut b32x2 = ptr::null_mut();

const hash_offset: i32 = 514;

/*:134*/
/*135: */
unsafe fn sort_avail() {
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut old_rover: i32 = 0;
    let _p = get_node(0x40000000) as i32;
    let mut p = MEM[(rover + 1) as usize].b32.s1;
    MEM[(rover + 1) as usize].b32.s1 = MAX_HALFWORD;
    old_rover = rover;
    /*136: */
    while p != old_rover {
        if p < rover {
            q = p;
            p = MEM[(q + 1) as usize].b32.s1;
            MEM[(q + 1) as usize].b32.s1 = rover;
            rover = q
        } else {
            q = rover;
            while MEM[(q + 1) as usize].b32.s1 < p {
                q = MEM[(q + 1) as usize].b32.s1
            }
            r = MEM[(p + 1) as usize].b32.s1;
            MEM[(p + 1) as usize].b32.s1 = MEM[(q + 1) as usize].b32.s1;
            MEM[(q + 1) as usize].b32.s1 = p;
            p = r
        }
    }
    let mut p = rover as usize;
    while MEM[p + 1].b32.s1 != MAX_HALFWORD {
        MEM[(MEM[p + 1].b32.s1 + 1) as usize].b32.s0 = p as i32;
        p = MEM[(p + 1) as usize].b32.s1 as usize
    }
    MEM[p + 1].b32.s1 = rover;
    MEM[(rover + 1) as usize].b32.s0 = p as i32;
}
/*:271*/
/*276: */
unsafe fn primitive<I>(ident: &[u8], c: Cmd, o: I)
where
    I: std::convert::TryInto<i32>,
    <I as std::convert::TryInto<i32>>::Error: std::fmt::Debug,
{
    let o = o.try_into().unwrap();
    let mut prim_val = 0;
    let mut len = ident.len() as i32;
    if len > 1 {
        let mut s: str_number = maketexstring(ident);
        if first + len > BUF_SIZE as i32 + 1 {
            overflow(b"buffer size", BUF_SIZE);
        }
        for i in 0..len {
            BUFFER[(first + i) as usize] = ident[i as usize] as UnicodeScalar;
        }
        cur_val = id_lookup(first, len);
        str_ptr -= 1;
        pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
        (*hash.offset(cur_val as isize)).s1 = s;
        prim_val = prim_lookup(s)
    } else {
        cur_val = ident[0] as i32 + SINGLE_BASE as i32;
        prim_val = prim_lookup(ident[0] as str_number)
    }
    EQTB[cur_val as usize].lvl = LEVEL_ONE;
    EQTB[cur_val as usize].cmd = c as u16;
    EQTB[cur_val as usize].val = o;
    prim_eqtb[prim_val as usize].lvl = LEVEL_ONE;
    prim_eqtb[prim_val as usize].cmd = c as u16;
    prim_eqtb[prim_val as usize].val = o;
}
/*:925*/
/*977: */
pub(crate) unsafe fn new_trie_op(mut d: i16, mut n: i16, mut v: trie_opcode) -> trie_opcode {
    let mut h: i32 = 0;
    let mut u: trie_opcode = 0;
    let mut l: i32 = 0;
    h = ((n as i32 + 313 * d as i32 + 361 * v as i32 + 1009 * cur_lang as i32).abs() as i64
        % (TRIE_OP_SIZE as i64 - NEG_TRIE_OP_SIZE as i64)
        + NEG_TRIE_OP_SIZE as i64) as i32;
    loop {
        l = _trie_op_hash_array[(h as i64 - -35111) as usize];
        if l == 0i32 {
            if trie_op_ptr as i64 == 35111 {
                overflow(b"pattern memory ops", 35111);
            }
            u = trie_used[cur_lang as usize];
            if u == MAX_TRIE_OP {
                overflow(b"pattern memory ops per language", 65535 - 0);
            }
            trie_op_ptr += 1;
            u += 1;
            trie_used[cur_lang as usize] = u;
            if u as i32 > max_op_used as i32 {
                max_op_used = u
            }
            hyf_distance[trie_op_ptr as usize] = d;
            hyf_num[trie_op_ptr as usize] = n;
            hyf_next[trie_op_ptr as usize] = v;
            trie_op_lang[trie_op_ptr as usize] = cur_lang;
            _trie_op_hash_array[(h as i64 - -35111) as usize] = trie_op_ptr;
            trie_op_val[trie_op_ptr as usize] = u;
            return u;
        }
        if hyf_distance[l as usize] as i32 == d as i32
            && hyf_num[l as usize] as i32 == n as i32
            && hyf_next[l as usize] as i32 == v as i32
            && trie_op_lang[l as usize] as i32 == cur_lang as i32
        {
            return trie_op_val[l as usize];
        }
        if h > -(TRIE_OP_SIZE as i32) {
            h -= 1
        } else {
            h = TRIE_OP_SIZE as i32
        }
    }
}
pub(crate) unsafe fn trie_node(mut p: trie_pointer) -> trie_pointer {
    let mut h: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    h = ((*trie_c.offset(p as isize) as u32
        + 1009 * *trie_o.offset(p as isize) as u32
        + 2718 * *trie_l.offset(p as isize) as u32
        + 3142 * *trie_r.offset(p as isize) as u32)
        % trie_size as u32) as i32;
    loop {
        q = *trie_hash.offset(h as isize);
        if q == 0 {
            *trie_hash.offset(h as isize) = p;
            return p;
        }
        if *trie_c.offset(q as isize) as i32 == *trie_c.offset(p as isize) as i32
            && *trie_o.offset(q as isize) as i32 == *trie_o.offset(p as isize) as i32
            && *trie_l.offset(q as isize) == *trie_l.offset(p as isize)
            && *trie_r.offset(q as isize) == *trie_r.offset(p as isize)
        {
            return q;
        }
        if h > 0 {
            h -= 1;
        } else {
            h = trie_size
        }
    }
}
pub(crate) unsafe fn compress_trie(mut p: trie_pointer) -> trie_pointer {
    if p == 0 {
        0
    } else {
        *trie_l.offset(p as isize) = compress_trie(*trie_l.offset(p as isize));
        *trie_r.offset(p as isize) = compress_trie(*trie_r.offset(p as isize));
        trie_node(p)
    }
}
pub(crate) unsafe fn first_fit(mut p: trie_pointer) {
    let mut h: trie_pointer = 0;
    let mut z: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    let mut c: UTF16_code = 0;
    let mut l: trie_pointer = 0;
    let mut r: trie_pointer = 0;
    let mut ll: i32 = 0;
    c = *trie_c.offset(p as isize);
    z = trie_min[c as usize];
    's_31: loop {
        h = z - c as i32;
        if trie_max < h + max_hyph_char {
            if trie_size <= h + max_hyph_char {
                overflow(b"pattern memory", trie_size as usize);
            }
            loop {
                trie_max += 1;
                *trie_taken.offset(trie_max as isize) = false;
                *trie_trl.offset(trie_max as isize) = trie_max + 1;
                *trie_tro.offset(trie_max as isize) = trie_max - 1;
                if trie_max == h + max_hyph_char {
                    break;
                }
            }
        }
        if !*trie_taken.offset(h as isize) {
            q = *trie_r.offset(p as isize);
            loop {
                if !(q > 0) {
                    break 's_31;
                }
                if *trie_trl.offset((h + *trie_c.offset(q as isize) as i32) as isize) == 0i32 {
                    break;
                }
                q = *trie_r.offset(q as isize)
            }
        }
        /*not_found */
        z = *trie_trl.offset(z as isize)
    }
    /*found *//*991: */
    *trie_taken.offset(h as isize) = true;
    *trie_hash.offset(p as isize) = h;
    q = p;
    loop {
        z = h + *trie_c.offset(q as isize) as i32;
        l = *trie_tro.offset(z as isize);
        r = *trie_trl.offset(z as isize);
        *trie_tro.offset(r as isize) = l;
        *trie_trl.offset(l as isize) = r;
        *trie_trl.offset(z as isize) = 0i32;
        if l < max_hyph_char {
            if z < max_hyph_char {
                ll = z
            } else {
                ll = max_hyph_char
            }
            loop {
                trie_min[l as usize] = r;
                l += 1;
                if l == ll {
                    break;
                }
            }
        }
        q = *trie_r.offset(q as isize);
        if q == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_pack(mut p: trie_pointer) {
    let mut q: trie_pointer = 0;
    loop {
        q = *trie_l.offset(p as isize);
        if q > 0 && *trie_hash.offset(q as isize) == 0 {
            first_fit(q);
            trie_pack(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_fix(mut p: trie_pointer) {
    let mut q: trie_pointer = 0;
    let mut c: UTF16_code = 0;
    let mut z: trie_pointer = 0;
    z = *trie_hash.offset(p as isize);
    loop {
        q = *trie_l.offset(p as isize);
        c = *trie_c.offset(p as isize);
        *trie_trl.offset((z + c as i32) as isize) = *trie_hash.offset(q as isize);
        *trie_trc.offset((z + c as i32) as isize) = c;
        *trie_tro.offset((z + c as i32) as isize) = *trie_o.offset(p as isize) as trie_pointer;
        if q > 0 {
            trie_fix(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0 {
            break;
        }
    }
}
unsafe fn new_patterns() {
    let mut k: i16 = 0;
    let mut l: i16 = 0;
    let mut digit_sensed: bool = false;
    let mut v: trie_opcode = 0;
    let mut p: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    let mut first_child: bool = false;
    let mut c: UTF16_code = 0;
    if trie_not_ready {
        if *INTPAR(IntPar::language) <= 0 {
            cur_lang = 0
        } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
            cur_lang = 0
        } else {
            cur_lang = *INTPAR(IntPar::language) as _;
        }
        scan_left_brace();
        k = 0;
        hyf[0] = 0;
        digit_sensed = false;
        loop {
            get_x_token();
            match cur_cmd {
                Cmd::Letter | Cmd::OtherChar => {
                    if digit_sensed || cur_chr < '0' as i32 || cur_chr > '9' as i32 {
                        if cur_chr == '.' as i32 {
                            cur_chr = 0
                        } else {
                            cur_chr = *LC_CODE(cur_chr as usize);
                            if cur_chr == 0 {
                                if file_line_error_style_p != 0 {
                                    print_file_line();
                                } else {
                                    print_nl_cstr(b"! ");
                                }
                                print_cstr(b"Nonletter");
                                help_ptr = 1;
                                help_line[0] = b"(See Appendix H.)";
                                error();
                            }
                        }
                        if cur_chr > max_hyph_char {
                            max_hyph_char = cur_chr
                        }
                        if (k as usize) < max_hyphenatable_length() {
                            k += 1;
                            hc[k as usize] = cur_chr;
                            hyf[k as usize] = 0;
                            digit_sensed = false
                        }
                    } else if (k as usize) < max_hyphenatable_length() {
                        hyf[k as usize] = (cur_chr - 48) as u8;
                        digit_sensed = true
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
                        l = k;
                        v = MIN_TRIE_OP;
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
                        q = 0;
                        hc[0] = cur_lang as i32;
                        while l <= k {
                            c = hc[l as usize] as UTF16_code;
                            l += 1;
                            p = *trie_l.offset(q as isize);
                            first_child = true;
                            while p > 0 && c > *trie_c.offset(p as isize) {
                                q = p;
                                p = *trie_r.offset(q as isize);
                                first_child = false
                            }
                            if p == 0 || c < *trie_c.offset(p as isize) {
                                /*999:*/
                                if trie_ptr == trie_size {
                                    overflow(b"pattern memory", trie_size as usize);
                                }
                                trie_ptr += 1;
                                *trie_r.offset(trie_ptr as isize) = p;
                                p = trie_ptr;
                                *trie_l.offset(p as isize) = 0;
                                if first_child {
                                    *trie_l.offset(q as isize) = p
                                } else {
                                    *trie_r.offset(q as isize) = p
                                }
                                *trie_c.offset(p as isize) = c;
                                *trie_o.offset(p as isize) = MIN_TRIE_OP;
                            }
                            q = p
                        }
                        if *trie_o.offset(q as isize) != MIN_TRIE_OP {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr(b"! ");
                            }
                            print_cstr(b"Duplicate pattern");
                            help_ptr = 1;
                            help_line[0] = b"(See Appendix H.)";
                            error();
                        }
                        *trie_o.offset(q as isize) = v
                    }
                    if cur_cmd == Cmd::RightBrace {
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
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Bad ");
                    print_esc_cstr(b"patterns");
                    help_ptr = 1;
                    help_line[0] = b"(See Appendix H.)";
                    error();
                }
            }
        }
        /*:996*/
        if *INTPAR(IntPar::saving_hyphs) > 0 {
            /*1643:*/
            c = cur_lang as UTF16_code;
            first_child = false;
            p = 0;
            loop {
                q = p;
                p = *trie_r.offset(q as isize);
                if p == 0 || c as i32 <= *trie_c.offset(p as isize) as i32 {
                    break;
                }
            }
            if p == 0 || (c as i32) < *trie_c.offset(p as isize) as i32 {
                /*:1644*/
                /*999:*/
                if trie_ptr == trie_size {
                    overflow(b"pattern memory", trie_size as usize);
                }
                trie_ptr += 1;
                *trie_r.offset(trie_ptr as isize) = p;
                p = trie_ptr;
                *trie_l.offset(p as isize) = 0;
                if first_child {
                    *trie_l.offset(q as isize) = p
                } else {
                    *trie_r.offset(q as isize) = p
                }
                *trie_c.offset(p as isize) = c;
                *trie_o.offset(p as isize) = MIN_TRIE_OP;
            }
            q = p;
            p = *trie_l.offset(q as isize);
            first_child = true;
            c = 0i32 as UTF16_code;
            while c as i32 <= 255 {
                if *LC_CODE(c as _) > 0 || c == 255 && first_child {
                    if p == 0i32 {
                        /*999:*/
                        if trie_ptr == trie_size {
                            overflow(b"pattern memory", trie_size as usize);
                            /*:987 */
                        }
                        trie_ptr += 1;
                        *trie_r.offset(trie_ptr as isize) = p;
                        p = trie_ptr;
                        *trie_l.offset(p as isize) = 0;
                        if first_child {
                            *trie_l.offset(q as isize) = p
                        } else {
                            *trie_r.offset(q as isize) = p
                        }
                        *trie_c.offset(p as isize) = c;
                        *trie_o.offset(p as isize) = MIN_TRIE_OP
                    } else {
                        *trie_c.offset(p as isize) = c
                    }
                    *trie_o.offset(p as isize) = *LC_CODE(c as _) as _;
                    q = p;
                    p = *trie_r.offset(q as isize);
                    first_child = false
                }
                c = c.wrapping_add(1)
            }
            if first_child {
                *trie_l.offset(q as isize) = 0i32
            } else {
                *trie_r.offset(q as isize) = 0i32
            }
        }
    } else {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"Too late for ");
        print_esc_cstr(b"patterns");
        help_ptr = 1_u8;
        help_line[0] = b"All patterns must be given before typesetting begins.";
        error();
        MEM[GARBAGE].b32.s1 = scan_toks(false, false) as i32;
        flush_list(Some(def_ref));
    };
}
pub(crate) unsafe fn init_trie() {
    let mut p: trie_pointer = 0;
    let mut j: i32 = 0;
    let mut k: i32 = 0;
    let mut t: i32 = 0;
    let mut r: trie_pointer = 0;
    let mut s: trie_pointer = 0;
    max_hyph_char += 1;
    op_start[0] = -(MIN_TRIE_OP as i32);
    let mut for_end: i32 = 0;
    j = 1;
    for_end = BIGGEST_LANG;
    if j <= for_end {
        loop {
            op_start[j as usize] = op_start[(j - 1) as usize] + trie_used[(j - 1) as usize] as i32;
            let fresh4 = j;
            j = j + 1;
            if !(fresh4 < for_end) {
                break;
            }
        }
    }
    let mut for_end_0: i32 = 0;
    j = 1;
    for_end_0 = trie_op_ptr;
    if j <= for_end_0 {
        loop {
            _trie_op_hash_array[(j as i64 - -35111) as usize] =
                op_start[trie_op_lang[j as usize] as usize] + trie_op_val[j as usize] as i32;
            let fresh5 = j;
            j = j + 1;
            if !(fresh5 < for_end_0) {
                break;
            }
        }
    }
    let mut for_end_1: i32 = 0;
    j = 1;
    for_end_1 = trie_op_ptr;
    if j <= for_end_1 {
        loop {
            while _trie_op_hash_array[(j as i64 - -35111) as usize] > j {
                k = _trie_op_hash_array[(j as i64 - -35111) as usize];
                t = hyf_distance[k as usize] as i32;
                hyf_distance[k as usize] = hyf_distance[j as usize];
                hyf_distance[j as usize] = t as i16;
                t = hyf_num[k as usize] as i32;
                hyf_num[k as usize] = hyf_num[j as usize];
                hyf_num[j as usize] = t as i16;
                t = hyf_next[k as usize] as i32;
                hyf_next[k as usize] = hyf_next[j as usize];
                hyf_next[j as usize] = t as trie_opcode;
                _trie_op_hash_array[(j as i64 - -35111) as usize] =
                    _trie_op_hash_array[(k as i64 - -35111) as usize];
                _trie_op_hash_array[(k as i64 - -35111) as usize] = k
            }
            let fresh6 = j;
            j = j + 1;
            if !(fresh6 < for_end_1) {
                break;
            }
        }
    }
    let mut for_end_2: i32 = 0;
    p = 0;
    for_end_2 = trie_size;
    if p <= for_end_2 {
        loop {
            *trie_hash.offset(p as isize) = 0;
            let fresh7 = p;
            p = p + 1;
            if !(fresh7 < for_end_2) {
                break;
            }
        }
    }
    *trie_r.offset(0) = compress_trie(*trie_r.offset(0));
    *trie_l.offset(0) = compress_trie(*trie_l.offset(0));
    let mut for_end_3: i32 = 0;
    p = 0;
    for_end_3 = trie_ptr;
    if p <= for_end_3 {
        loop {
            *trie_hash.offset(p as isize) = 0i32;
            let fresh8 = p;
            p = p + 1;
            if !(fresh8 < for_end_3) {
                break;
            }
        }
    }
    let mut for_end_4: i32 = 0;
    p = 0i32;
    for_end_4 = 0xffffi32;
    if p <= for_end_4 {
        loop {
            trie_min[p as usize] = p + 1i32;
            let fresh9 = p;
            p = p + 1;
            if !(fresh9 < for_end_4) {
                break;
            }
        }
    }
    *trie_trl.offset(0) = 1i32;
    trie_max = 0i32;
    if *trie_l.offset(0) != 0i32 {
        first_fit(*trie_l.offset(0));
        trie_pack(*trie_l.offset(0));
    }
    if *trie_r.offset(0) != 0i32 {
        /*1645: */
        if *trie_l.offset(0) == 0i32 {
            let mut for_end_5: i32 = 0;
            p = 0i32;
            for_end_5 = 255i32;
            if p <= for_end_5 {
                loop {
                    trie_min[p as usize] = p + 2i32;
                    let fresh10 = p;
                    p = p + 1;
                    if !(fresh10 < for_end_5) {
                        break;
                    }
                }
            }
        }
        first_fit(*trie_r.offset(0));
        trie_pack(*trie_r.offset(0));
        hyph_start = *trie_hash.offset(*trie_r.offset(0) as isize)
    }
    if trie_max == 0i32 {
        let mut for_end_6: i32 = 0;
        r = 0i32;
        for_end_6 = max_hyph_char;
        if r <= for_end_6 {
            loop {
                *trie_trl.offset(r as isize) = 0i32;
                *trie_tro.offset(r as isize) = 0i32;
                *trie_trc.offset(r as isize) = 0_u16;
                let fresh11 = r;
                r = r + 1;
                if !(fresh11 < for_end_6) {
                    break;
                }
            }
        }
        trie_max = max_hyph_char
    } else {
        if *trie_r.offset(0) > 0i32 {
            trie_fix(*trie_r.offset(0));
        }
        if *trie_l.offset(0) > 0i32 {
            trie_fix(*trie_l.offset(0));
        }
        r = 0i32;
        loop {
            s = *trie_trl.offset(r as isize);
            *trie_trl.offset(r as isize) = 0i32;
            *trie_tro.offset(r as isize) = 0i32;
            *trie_trc.offset(r as isize) = 0_u16;
            r = s;
            if r > trie_max {
                break;
            }
        }
    }
    *trie_trc.offset(0) = '?' as i32 as u16;
    trie_not_ready = false;
}
/*:1001*/
unsafe fn new_hyph_exceptions() {
    let mut current_block: u64;
    let mut n: i16 = 0;
    let mut j: i16 = 0;
    let mut h: hyph_pointer = 0;
    let mut k: str_number = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut s: str_number = 0;
    let mut u: pool_pointer = 0;
    let mut v: pool_pointer = 0;

    scan_left_brace();

    if *INTPAR(IntPar::language) <= 0 {
        cur_lang = 0_u8
    } else if *INTPAR(IntPar::language) > BIGGEST_LANG {
        cur_lang = 0_u8
    } else {
        cur_lang = *INTPAR(IntPar::language) as _;
    }

    if trie_not_ready {
        hyph_index = 0;
    } else if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32 != cur_lang as i32 {
        hyph_index = 0;
    } else {
        hyph_index = *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
    }

    /*970: not_found:*/
    n = 0_i16;
    p = TEX_NULL;

    's_91: loop {
        get_x_token();
        loop {
            match cur_cmd {
                Cmd::Letter | Cmd::OtherChar | Cmd::CharGiven => {
                    if cur_chr == '-' as i32 {
                        /*973:*/
                        if (n as usize) < max_hyphenatable_length() {
                            q = get_avail() as i32;
                            MEM[q as usize].b32.s1 = p;
                            MEM[q as usize].b32.s0 = n as i32;
                            p = q
                        }
                    } else {
                        if hyph_index == 0 || cur_chr > 255 {
                            hc[0] = *LC_CODE(cur_chr as usize) as _;
                        } else if *trie_trc.offset((hyph_index + cur_chr) as isize) as i32
                            != cur_chr
                        {
                            hc[0] = 0;
                        } else {
                            hc[0] = *trie_tro.offset((hyph_index + cur_chr) as isize)
                        }
                        if hc[0] == 0 {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr(b"! ");
                            }
                            print_cstr(b"Not a letter");
                            help_ptr = 2_u8;
                            help_line[1] = b"Letters in \\hyphenation words must have \\lccode>0.";
                            help_line[0] = b"Proceed; I\'ll ignore the character I just read.";
                            error();
                        } else if (n as usize) < max_hyphenatable_length() {
                            n += 1;
                            if (hc[0] as i64) < 65536 {
                                hc[n as usize] = hc[0]
                            } else {
                                hc[n as usize] =
                                    ((hc[0] as i64 - 65536) / 1024 as i64 + 55296) as i32;
                                n += 1;
                                hc[n as usize] = ((hc[0] % 1024) as i64 + 56320) as i32
                            }
                        }
                    }
                    continue 's_91;
                }
                Cmd::CharNum => {
                    scan_char_num();
                    cur_chr = cur_val;
                    cur_cmd = Cmd::CharGiven;
                }
                Cmd::Spacer | Cmd::RightBrace => {
                    if n > 1 {
                        current_block = 10753070352654377903;
                        break;
                    } else {
                        current_block = 9500030526577190060;
                        break;
                    }
                }
                _ => {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Improper ");
                    print_esc_cstr(b"hyphenation");
                    print_cstr(b" will be flushed");
                    help_ptr = 2_u8;
                    help_line[1] = b"Hyphenation exceptions must contain only letters";
                    help_line[0] = b"and hyphens. But continue; I\'ll forgive and forget.";
                    error();
                    continue 's_91;
                }
            }
        }
        match current_block {
            10753070352654377903 => {
                /*974:*/
                n += 1;
                hc[n as usize] = cur_lang as i32;
                if pool_ptr + n as i32 > pool_size {
                    overflow(b"pool size", (pool_size - init_pool_ptr) as usize);
                }
                h = 0;

                j = 1;
                while j as i32 <= n as i32 {
                    h = ((h as i32 + h as i32 + hc[j as usize]) % HYPH_PRIME) as hyph_pointer;
                    str_pool[pool_ptr as usize] = hc[j as usize] as packed_UTF16_code;
                    pool_ptr += 1;
                    j += 1
                }

                s = make_string();

                if HYPH_NEXT <= HYPH_PRIME as usize {
                    while HYPH_NEXT > 0 && HYPH_WORD[HYPH_NEXT - 1] > 0 {
                        HYPH_NEXT -= 1;
                    }
                }

                if HYPH_COUNT == HYPH_SIZE || HYPH_NEXT == 0 {
                    overflow(b"exception dictionary", HYPH_SIZE);
                }

                HYPH_COUNT += 1;

                while HYPH_WORD[h as usize] != 0 {
                    k = HYPH_WORD[h as usize];
                    if length(k) == length(s) {
                        u = str_start[(k as i64 - 65536) as usize];
                        v = str_start[(s as i64 - 65536) as usize];
                        loop {
                            if str_pool[u as usize] as i32 != str_pool[v as usize] as i32 {
                                current_block = 876886731760051519;
                                break;
                            }
                            u += 1;
                            v += 1;
                            if u == str_start[((k + 1i32) as i64 - 65536) as usize] {
                                current_block = 8732226822098929438;
                                break;
                            }
                        }
                        match current_block {
                            876886731760051519 => {}
                            _ => {
                                str_ptr -= 1;
                                pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
                                s = HYPH_WORD[h as usize];
                                HYPH_COUNT -= 1;
                                break;
                            }
                        }
                    }
                    /*:975*/
                    /*:976*/
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
                HYPH_WORD[h as usize] = s;
                HYPH_LIST[h as usize] = p
            }
            _ => {}
        }

        if cur_cmd == Cmd::RightBrace {
            return;
        }

        n = 0;
        p = TEX_NULL;
    }
}
pub(crate) unsafe fn prefixed_command() {
    let mut current_block: u64;

    let mut f: internal_font_number = 0;
    let mut j: i32 = 0;
    let mut k: font_index = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut e: bool = false;

    let mut a = 0 as i16;

    while cur_cmd == Cmd::Prefix {
        if a as i32 / cur_chr & 1i32 == 0 {
            a = (a as i32 + cur_chr) as i16
        }
        loop {
            get_x_token();
            if !(cur_cmd == Cmd::Spacer || cur_cmd == Cmd::Relax) {
                break;
            }
        }
        if cur_cmd <= MAX_NON_PREFIXED_COMMAND {
            /*1247:*/
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"You can\'t use a prefix with `");
            print_cmd_chr(cur_cmd, cur_chr);
            print_char('\'' as i32);
            help_ptr = 1_u8;
            help_line[0] =
                b"I\'ll pretend you didn\'t say \\long or \\outer or \\global or \\protected.";
            back_error();
            return;
        }
        if *INTPAR(IntPar::tracing_commands) > 2 {
            show_cur_cmd_chr();
        }
    }
    if a >= 8 {
        j = PROTECTED_TOKEN;
        a -= 8;
    } else {
        j = 0;
    }
    if cur_cmd != Cmd::Def && (a % 4 != 0 || j != 0) {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"You can\'t use `");
        print_esc_cstr(b"long");
        print_cstr(b"\' or `");
        print_esc_cstr(b"outer");
        help_ptr = 1_u8;
        help_line[0] = b"I\'ll pretend you didn\'t say \\long or \\outer or \\protected here.";
        print_cstr(b"\' or `");
        print_esc_cstr(b"protected");
        print_cstr(b"\' with `");
        print_cmd_chr(cur_cmd, cur_chr);
        print_char('\'' as i32);
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
    match cur_cmd as _ {
        Cmd::SetFont => {
            /*1252:*/
            if a >= 4 {
                geq_define(CUR_FONT_LOC, Cmd::Data, cur_chr);
            } else {
                eq_define(CUR_FONT_LOC, Cmd::Data, cur_chr);
            }
        }
        Cmd::Def => {
            if cur_chr & 1i32 != 0 && (a as i32) < 4i32 && *INTPAR(IntPar::global_defs) >= 0 {
                a = (a as i32 + 4i32) as i16
            }
            e = cur_chr >= 2;
            get_r_token();
            p = cur_cs;
            q = scan_toks(true, e) as i32;
            if j != 0 {
                q = get_avail() as i32;
                MEM[q as usize].b32.s0 = j;
                MEM[q as usize].b32.s1 =
                    MEM[def_ref].b32.s1;
                MEM[def_ref].b32.s1 = q
            }
            if a >= 4 {
                geq_define(p as usize, Cmd::from(Cmd::Call as u16 + (a % 4) as u16),
                           def_ref as i32);
            } else {
                eq_define(p as usize, Cmd::from(Cmd::Call as u16 + (a % 4) as u16),
                          def_ref as i32);
            }
        }
        Cmd::Let => {
            let n = cur_chr;
            get_r_token();
            p = cur_cs;
            if n == NORMAL as i32 {
                loop  {
                    get_token();
                    if !(cur_cmd == Cmd::Spacer) { break ; }
                }
                if cur_tok == OTHER_TOKEN + '=' as i32 {
                    get_token();
                    if cur_cmd == Cmd::Spacer { get_token(); }
                }
            } else {
                get_token();
                q = cur_tok;
                get_token();
                back_input();
                cur_tok = q;
                back_input();
            }
            if cur_cmd >= Cmd::Call {
                MEM[cur_chr as usize].b32.s0 += 1
            } else if cur_cmd == Cmd::Register ||
                          cur_cmd == Cmd::ToksRegister {
                if cur_chr < 0 || cur_chr > 19 {
                    /* 19 = lo_mem_stat_max, I think */
                    MEM[(cur_chr + 1) as usize].b32.s0 += 1;
                }
            }
            if a >= 4 {
                geq_define(p as usize, cur_cmd, cur_chr);
            } else { eq_define(p as usize, cur_cmd, cur_chr); }
        }
        Cmd::ShorthandDef => {
            if cur_chr == CHAR_SUB_DEF_CODE {
                scan_char_num();
                p = CHAR_SUB_CODE_BASE as i32 + cur_val;
                scan_optional_equals();
                scan_char_num();
                let mut n = cur_val;
                scan_char_num();
                if *INTPAR(IntPar::tracing_char_sub_def) > 0 {
                    begin_diagnostic();
                    print_nl_cstr(b"New character substitution: ");
                    print(p - CHAR_SUB_CODE_BASE as i32);
                    print_cstr(b" = ");
                    print(n);
                    print_char(' ' as i32);
                    print(cur_val);
                    end_diagnostic(false);
                }
                n = n * 256 + cur_val;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, n);
                } else { eq_define(p as usize, Cmd::Data, n); }
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
                let mut n = cur_chr;
                get_r_token();
                p = cur_cs;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Relax, TOO_BIG_USV);
                } else {
                    eq_define(p as usize, Cmd::Relax, TOO_BIG_USV);
                }
                scan_optional_equals();
                match n {
                    CHAR_DEF_CODE => {
                        scan_usv_num();
                        if a >= 4 {
                            geq_define(p as usize, Cmd::CharGiven, cur_val);
                        } else { eq_define(p as usize, Cmd::CharGiven, cur_val); }
                    }
                    MATH_CHAR_DEF_CODE => {
                        scan_fifteen_bit_int();
                        if a >= 4 {
                            geq_define(p as usize, Cmd::MathGiven, cur_val);
                        } else { eq_define(p as usize, Cmd::MathGiven, cur_val); }
                    }
                    XETEX_MATH_CHAR_NUM_DEF_CODE => {
                        scan_xetex_math_char_int();
                        if a >= 4 {
                            geq_define(p as usize, Cmd::XetexMathGiven, cur_val);
                        } else { eq_define(p as usize, Cmd::XetexMathGiven, cur_val); }
                    }
                    XETEX_MATH_CHAR_DEF_CODE => {
                        scan_math_class_int();
                        n = set_class(cur_val);
                        scan_math_fam_int();
                        n = n + set_family(cur_val);
                        scan_usv_num();
                        n = n + cur_val;
                        if a >= 4 {
                            geq_define(p as usize, Cmd::XetexMathGiven, n);
                        } else { eq_define(p as usize, Cmd::XetexMathGiven, n); }
                    }
                    _ => {
                        scan_register_num();
                        if cur_val > 255 {
                            j = n - 2;
                            if j > MU_VAL as i32 { j = TOK_VAL as i32 }

                            find_sa_element(j as i16, cur_val,
                                            true);
                            MEM[(cur_ptr + 1) as usize].b32.s0 += 1;

                            let j = if j == TOK_VAL as i32 { Cmd::ToksRegister } else { Cmd::Register };
                            if a >= 4 {
                                geq_define(p as usize, j, cur_ptr);
                            } else { eq_define(p as usize, j, cur_ptr); }
                        } else {
                            match n {
                                COUNT_DEF_CODE => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignInt, COUNT_BASE as i32 + cur_val);
                                    } else {
                                        eq_define(p as usize, Cmd::AssignInt, COUNT_BASE as i32 + cur_val);
                                    }
                                }
                                DIMEN_DEF_CODE => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignDimen, SCALED_BASE as i32 + cur_val);
                                    } else {
                                        eq_define(p as usize, Cmd::AssignDimen, SCALED_BASE as i32 + cur_val);
                                    }
                                }
                                SKIP_DEF_CODE => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignGlue, SKIP_BASE as i32 + cur_val);
                                    } else {
                                        eq_define(p as usize, Cmd::AssignGlue, SKIP_BASE as i32 + cur_val);
                                    }
                                }
                                MU_SKIP_DEF_CODE => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignMuGlue, MU_SKIP_BASE as i32 + cur_val);
                                    } else {
                                        eq_define(p as usize, Cmd::AssignMuGlue, MU_SKIP_BASE as i32 + cur_val);
                                    }
                                }
                                TOKS_DEF_CODE => {
                                    if a >= 4 {
                                        geq_define(p as usize, Cmd::AssignToks, TOKS_BASE as i32 + cur_val);
                                    } else {
                                        eq_define(p as usize, Cmd::AssignToks, TOKS_BASE as i32 + cur_val);
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
            j = cur_chr;
            scan_int();
            let n = cur_val;
            if !scan_keyword(b"to") {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Missing `to\' inserted");
                help_ptr = 2_u8;
                help_line[1] =
                    b"You should have said `\\read<number> to \\cs\'.";
                help_line[0] =
                    b"I\'m going to look for the \\cs now.";
                error();
            }
            get_r_token();
            p = cur_cs;
            read_toks(n, p, j);
            if a >= 4 {
                geq_define(p as usize, Cmd::Call, cur_val);
            } else { eq_define(p as usize, Cmd::Call, cur_val); }
        }
        Cmd::ToksRegister | Cmd::AssignToks => {
            q = cur_cs;
            e = false;
            if cur_cmd == Cmd::ToksRegister {
                if cur_chr == 0 {
                    scan_register_num();
                    if cur_val > 255 {
                        find_sa_element(TOK_VAL as i16, cur_val,
                                        true);
                        cur_chr = cur_ptr;
                        e = true
                    } else {
                        cur_chr = TOKS_BASE as i32 + cur_val;
                    }
                } else { e = true }
            } else if cur_chr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                scan_char_class_not_ignored();
                cur_ptr = cur_val;
                scan_char_class_not_ignored();
                find_sa_element(INTER_CHAR_VAL as i16,
                                cur_ptr * CHAR_CLASS_LIMIT + cur_val, true);
                cur_chr = cur_ptr;
                e = true
            }
            p = cur_chr;
            scan_optional_equals();
            loop  {
                get_x_token();
                if !(cur_cmd == Cmd::Spacer ||
                         cur_cmd == Cmd::Relax) {
                    break ;
                }
            }
            if cur_cmd != Cmd::LeftBrace {
                /*1262:*/
                if cur_cmd == Cmd::ToksRegister ||
                       cur_cmd == Cmd::AssignToks {
                    if cur_cmd == Cmd::ToksRegister {
                        if cur_chr == 0 {
                            scan_register_num(); /* "extended delimiter code flag" */
                            if cur_val < 256 {
                                q = *TOKS_REG(cur_val as usize);
                            } else {
                                find_sa_element(TOK_VAL as i16, cur_val,
                                                false); /* "extended delimiter code family */
                                if cur_ptr.is_texnull() {
                                    q = TEX_NULL
                                } else {
                                    q =
                                        MEM[(cur_ptr + 1) as
                                                         usize].b32.s1
                                }
                            }
                        } else {
                            q =
                                MEM[(cur_chr + 1) as
                                                 usize].b32.s1
                        }
                    } else if cur_chr == LOCAL_BASE as i32 + Local::xetex_inter_char as i32 {
                        scan_char_class_not_ignored(); /*:1268 */
                        cur_ptr = cur_val;
                        scan_char_class_not_ignored();
                        find_sa_element(INTER_CHAR_VAL as i16,
                                        cur_ptr * CHAR_CLASS_LIMIT + cur_val,
                                        false);
                        if cur_ptr.is_texnull() {
                            q = TEX_NULL
                        } else {
                            q =
                                MEM[(cur_ptr + 1) as
                                                 usize].b32.s1
                        }
                    } else { q = EQTB[cur_chr as usize].val }
                    if q.is_texnull() {
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, TEX_NULL);
                            } else { sa_def(p as usize, TEX_NULL); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::UndefinedCS, TEX_NULL);
                        } else {
                            eq_define(p as usize, Cmd::UndefinedCS, TEX_NULL);
                        }
                    } else {
                        MEM[q as usize].b32.s0 += 1;
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, q);
                            } else { sa_def(p as usize, q); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::Call, q);
                        } else { eq_define(p as usize, Cmd::Call, q); }
                    }
                    current_block = 1862445865460439639;
                } else { current_block = 15174492983169363256; }
            } else { current_block = 15174492983169363256; }
            match current_block {
                1862445865460439639 => { }
                _ => {
                    back_input();
                    cur_cs = q;
                    q = scan_toks(false, false) as i32;
                    if MEM[def_ref].b32.s1.is_texnull() {
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, TEX_NULL);
                            } else { sa_def(p as usize, TEX_NULL); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::UndefinedCS, TEX_NULL);
                        } else {
                            eq_define(p as usize, Cmd::UndefinedCS, TEX_NULL);
                        }
                        MEM[def_ref].b32.s1 = avail;
                        avail = def_ref as i32;
                    } else {
                        if p == LOCAL_BASE as i32 + Local::output_routine as i32 && !e {
                            MEM[q as usize].b32.s1 = get_avail() as i32;
                            q = *LLIST_link(q as usize);
                            MEM[q as usize].b32.s0 =
                                RIGHT_BRACE_TOKEN + 125;
                            q = get_avail() as i32;
                            MEM[q as usize].b32.s0 =
                                LEFT_BRACE_TOKEN + 123;
                            MEM[q as usize].b32.s1 =
                                MEM[def_ref].b32.s1;
                            MEM[def_ref].b32.s1 = q
                        }
                        if e {
                            if a >= 4 {
                                gsa_def(p as usize, def_ref as i32);
                            } else { sa_def(p as usize, def_ref as i32); }
                        } else if a >= 4 {
                            geq_define(p as usize, Cmd::Call, def_ref as i32);
                        } else { eq_define(p as usize, Cmd::Call, def_ref as i32); }
                    }
                }
            }
        }
        Cmd::AssignInt => {
            p = cur_chr;
            scan_optional_equals();
            scan_int();
            if a >= 4 {
                geq_word_define(p as usize, cur_val);
            } else { eq_word_define(p as usize, cur_val); }
        }
        Cmd::AssignDimen => {
            p = cur_chr;
            scan_optional_equals();
            scan_dimen(false, false, false);
            if a >= 4 {
                geq_word_define(p as usize, cur_val);
            } else { eq_word_define(p as usize, cur_val); }
        }
        Cmd::AssignGlue | Cmd::AssignMuGlue => {
            p = cur_chr;
            let n = cur_cmd;
            scan_optional_equals();
            if n == Cmd::AssignMuGlue {
                scan_glue(MU_VAL as i16);
            } else { scan_glue(GLUE_VAL as i16); }
            trap_zero_glue();
            if a >= 4 {
                geq_define(p as usize, Cmd::GlueRef, cur_val);
            } else { eq_define(p as usize, Cmd::GlueRef, cur_val); }
        }
        Cmd::XetexDefCode => {
            if cur_chr == SF_CODE_BASE as i32 {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                let n = *SF_CODE(cur_val as usize) % 65536;
                scan_optional_equals();
                scan_char_class();
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data,
                               (cur_val as i64 * 65536 +
                                    n as i64) as i32);
                } else {
                    eq_define(p as usize, Cmd::Data,
                              (cur_val as i64 * 65536 +
                                   n as i64) as i32);
                }
            } else if cur_chr == MATH_CODE_BASE as i32 {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_xetex_math_char_int();
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, cur_val);
                } else { eq_define(p as usize, Cmd::Data, cur_val); }
            } else if cur_chr == MATH_CODE_BASE as i32 + 1 {
                p = cur_chr - 1;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_math_class_int();
                let mut n = set_class(cur_val);
                scan_math_fam_int();
                n = n + set_family(cur_val);
                scan_usv_num();
                n = n + cur_val;
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, n);
                } else { eq_define(p as usize, Cmd::Data, n); }
            } else if cur_chr == DEL_CODE_BASE as i32 {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_int();
                if a >= 4 {
                    geq_word_define(p as usize, cur_val);
                } else { eq_word_define(p as usize, cur_val); }
            } else {
                p = cur_chr - 1;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                let mut n = 0x40000000;
                scan_math_fam_int();
                n = n + cur_val * 0x200000;
                scan_usv_num();
                n = n + cur_val;
                if a >= 4 {
                    geq_word_define(p as usize, n);
                } else { eq_word_define(p as usize, n); }
            }
        }
    Cmd::DefCode => {
            let n = if cur_chr == CAT_CODE_BASE as i32 {
                MAX_CHAR_CODE
            } else if cur_chr == MATH_CODE_BASE as i32 {
                0x8000
            } else if cur_chr == SF_CODE_BASE as i32 {
                0x7fff
            } else if cur_chr == DEL_CODE_BASE as i32 {
                0xffffff
            } else { BIGGEST_USV as i32 }; // :1268

            p = cur_chr;
            scan_usv_num();
            p = p + cur_val;
            scan_optional_equals();
            scan_int();

            if (cur_val < 0 && p < DEL_CODE_BASE as i32) || cur_val > n {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Invalid code (");
                print_int(cur_val);
                if p < MATH_CODE_BASE as i32 {
                    print_cstr(b"), should be in the range 0..");
                } else {
                    print_cstr(b"), should be at most ");
                }
                print_int(n);
                help_ptr = 1_u8;
                help_line[0] =
                    b"I\'m going to use 0 instead of that illegal code value.";
                error();
                cur_val = 0
            }

            if p < MATH_CODE_BASE as i32 {
                if p >= SF_CODE_BASE as i32 {
                    let n =
                        (EQTB[p as usize].val as i64 /
                             65536) as i32;
                    if a >= 4 {
                        geq_define(p as usize, Cmd::Data,
                                   (n as i64 * 65536 +
                                        cur_val as i64) as i32);
                    } else {
                        eq_define(p as usize, Cmd::Data,
                                  (n as i64 * 65536 +
                                       cur_val as i64) as i32);
                    }
                } else if a >= 4 {
                    geq_define(p as usize, Cmd::Data, cur_val);
                } else { eq_define(p as usize, Cmd::Data, cur_val); }

            } else if p < DEL_CODE_BASE as i32 {
                if cur_val as i64 == 32768 {
                    cur_val = ACTIVE_MATH_CHAR
                } else {
                    cur_val = set_class(cur_val / 4096) + set_family((cur_val % 4096) / 256) + (cur_val % 256);
                }
                if a >= 4 {
                    geq_define(p as usize, Cmd::Data, cur_val);
                } else { eq_define(p as usize, Cmd::Data, cur_val); }
            } else if a >= 4 {
                geq_word_define(p as usize, cur_val);
            } else { eq_word_define(p as usize, cur_val); }
        }
        Cmd::DefFamily => {
            p = cur_chr;
            scan_math_fam_int();
            p = p + cur_val;
            scan_optional_equals();
            scan_font_ident();
            if a >= 4 {
                geq_define(p as usize, Cmd::Data, cur_val);
            } else { eq_define(p as usize, Cmd::Data, cur_val); }
        }
        Cmd::Register | Cmd::Advance | Cmd::Multiply | Cmd::Divide => { do_register_command(a); }
        Cmd::SetBox => {
            scan_register_num();
            let n = if a >= 4 {
                GLOBAL_BOX_FLAG + cur_val
            } else { BOX_FLAG + cur_val };
            scan_optional_equals();
            if set_box_allowed {
                scan_box(n);
            } else {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Improper ");
                print_esc_cstr(b"setbox");
                help_ptr = 2_u8;
                help_line[1] =
                    b"Sorry, \\setbox is not allowed after \\halign in a display,";
                help_line[0] =
                    b"or between \\accent and an accented character.";
                error();
            }
        }
        Cmd::SetAux => { alter_aux(); }
        Cmd::SetPrevGraf => { alter_prev_graf(); }
        Cmd::SetPageDimen => { alter_page_so_far(); }
        Cmd::SetPageInt => { alter_integer(); }
        Cmd::SetBoxDimen => { alter_box_dimen(); }
        Cmd::SetShape => {
            q = cur_chr;
            scan_optional_equals();
            scan_int();
            let mut n = cur_val;
            if n <= 0 {
                p = TEX_NULL
            } else if q > LOCAL_BASE as i32 + Local::par_shape as i32 {
                n = cur_val / 2 + 1;
                p = get_node(2 * n + 1) as i32;
                MEM[p as usize].b32.s0 = n;
                n = cur_val;
                MEM[(p + 1) as usize].b32.s1 = n;

                j = p + 2;
                while j <= p + n + 1 {
                    scan_int();
                    MEM[j as usize].b32.s1 = cur_val;
                    j += 1;
                }

                if n & 1 == 0 {
                    MEM[(p + n + 2) as usize].b32.s1 = 0
                }
            } else {
                p = get_node(2 * n + 1) as i32;
                MEM[p as usize].b32.s0 = n;
    
                j = 1i32;
                while j <= n {
                    scan_dimen(false, false, false);
                    MEM[(p + 2 * j - 1) as usize].b32.s1 =
                        cur_val;
                    scan_dimen(false, false, false);
                    MEM[(p + 2 * j) as usize].b32.s1 = cur_val;
                    j += 1
                }
            }
            if a >= 4 {
                geq_define(q as usize, Cmd::ShapeRef, p);
            } else { eq_define(q as usize, Cmd::ShapeRef, p); }
        }
        Cmd::HyphData => {
            if cur_chr == 1 {
                if in_initex_mode {
                    new_patterns();
                } else {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Patterns can be loaded only by INITEX");
                    help_ptr = 0_u8;
                    error();
                    loop  {
                        get_token();
                        if cur_cmd == Cmd::RightBrace { break ; }
                    }
                    return
                }
            } else { new_hyph_exceptions(); }
        }
        Cmd::AssignFontDimen => {
            find_font_dimen(true);
            k = cur_val;
            scan_optional_equals();
            scan_dimen(false, false, false);
            FONT_INFO[k as usize].b32.s1 = cur_val
        }
        Cmd::AssignFontInt => {
            let n = cur_chr;
            scan_font_ident();
            f = cur_val as usize;
            if n < 2 {
                scan_optional_equals();
                scan_int();
                if n == 0 {
                    HYPHEN_CHAR[f] = cur_val
                } else { SKEW_CHAR[f] = cur_val }
            } else {
                if FONT_AREA[f] as u32 == AAT_FONT_FLAG
                       ||
                       FONT_AREA[f] as u32 ==
                           OTGR_FONT_FLAG {
                    scan_glyph_number(f);
                } else { scan_char_num(); }
                p = cur_val;
                scan_optional_equals();
                scan_int();
                match n {
                    LP_CODE_BASE => { set_cp_code(f, p as u32, LEFT_SIDE, cur_val); }
                    RP_CODE_BASE => { set_cp_code(f, p as u32, RIGHT_SIDE, cur_val); }
                    _ => { }
                }
            }
        }
        Cmd::DefFont => { new_font(a); }
        Cmd::SetInteraction => { new_interaction(); }
        _ => { confusion(b"prefix"); }
    }
    /*1304:*/
    if after_token != 0 {
        cur_tok = after_token;
        back_input();
        after_token = 0;
    };
}
/*:1328*/
/*1337:*/
unsafe fn store_fmt_file() {
    let mut current_block: u64;
    let mut j: i32 = 0;
    let mut l: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut x: i32 = 0;
    if SAVE_PTR != 0 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"You can\'t dump inside a group");
        help_ptr = 1_u8;
        help_line[0] = b"`{...\\dump}\' is a no-no.";

        if interaction == ERROR_STOP_MODE {
            interaction = SCROLL_MODE;
        }
        if log_opened {
            error();
        }

        history = TTHistory::FATAL_ERROR;
        close_files_and_terminate();
        rust_stdout.as_mut().unwrap().flush().unwrap();
        panic!("\\dump inside a group");
    }

    selector = Selector::NEW_STRING;
    print_cstr(b" (preloaded format=");
    print(job_name);
    print_char(' ' as i32);
    print_int(*INTPAR(IntPar::year));
    print_char('.' as i32);
    print_int(*INTPAR(IntPar::month));
    print_char('.' as i32);
    print_int(*INTPAR(IntPar::day));
    print_char(')' as i32);

    if interaction == BATCH_MODE {
        selector = Selector::LOG_ONLY
    } else {
        selector = Selector::TERM_AND_LOG
    }

    if pool_ptr + 1 > pool_size {
        overflow(b"pool size", (pool_size - init_pool_ptr) as usize);
    }

    format_ident = make_string();
    pack_job_name(b".fmt");

    let fmt_out = ttstub_output_open(name_of_file, 0);
    if fmt_out.is_none() {
        abort!(
            "cannot open format output file \"{}\"",
            CStr::from_ptr(name_of_file).display()
        );
    }

    let mut fmt_out_owner = fmt_out.unwrap();
    let fmt_out = &mut fmt_out_owner;
    print_nl_cstr(b"Beginning to dump on file ");
    print(make_name_string());

    str_ptr -= 1;
    pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];

    print_nl_cstr(b"");
    print(format_ident);

    /* Header */
    /* TODO: can we move this farther up in this function? */
    fmt_out.dump_one(FORMAT_HEADER_MAGIC);
    fmt_out.dump_one(FORMAT_SERIAL);
    fmt_out.dump_one(hash_high);

    while !pseudo_files.is_texnull() {
        pseudo_close();
    }

    fmt_out.dump_one(MEM_TOP as i32);
    fmt_out.dump_one(EQTB_SIZE as i32);
    fmt_out.dump_one(HASH_PRIME as i32);
    fmt_out.dump_one(HYPH_PRIME);

    /* string pool */

    fmt_out.dump_one(pool_ptr);
    fmt_out.dump_one(str_ptr);
    fmt_out.dump(&str_start[..(str_ptr - TOO_BIG_CHAR + 1) as usize]);
    fmt_out.dump(&str_pool[..(pool_ptr as usize)]);

    print_ln();
    print_int(str_ptr);
    print_cstr(b" strings of total length ");
    print_int(pool_ptr);

    /* "memory locations" */

    sort_avail();
    var_used = 0;
    fmt_out.dump_one(lo_mem_max);
    fmt_out.dump_one(rover);

    for k in (INT_VAL as usize)..=(INTER_CHAR_VAL as usize) {
        fmt_out.dump_one(sa_root[k as usize]);
    }

    p = 0;
    q = rover;
    x = 0;
    loop {
        fmt_out.dump(&MEM[p as usize..(q + 2) as usize]);
        x = x + q + 2 - p;
        var_used = var_used + q - p;
        p = q + MEM[q as usize].b32.s0;
        q = MEM[(q + 1) as usize].b32.s1;
        if !(q != rover) {
            break;
        }
    }

    var_used = var_used + lo_mem_max - p;
    dyn_used = mem_end + 1 - hi_mem_min;
    fmt_out.dump(&MEM[p as usize..(lo_mem_max + 1) as usize]);

    x = x + lo_mem_max + 1 - p;
    fmt_out.dump_one(hi_mem_min as i32);
    fmt_out.dump_one(avail as i32);
    fmt_out.dump(&MEM[hi_mem_min as usize..(mem_end + 1) as usize]);

    x = x + mem_end + 1 - hi_mem_min;
    p = avail;
    while !p.is_texnull() {
        dyn_used -= 1;
        p = *LLIST_link(p as usize)
    }

    fmt_out.dump_one(var_used as i32);
    fmt_out.dump_one(dyn_used as i32);

    print_ln();
    print_int(x);
    print_cstr(b" memory locations dumped; current usage is ");
    print_int(var_used);
    print_char('&' as i32);
    print_int(dyn_used);

    /* equivalents table / primitive */

    let mut k = ACTIVE_BASE as i32; /*:1350*/
    loop {
        j = k;
        loop {
            if !(j < (INT_BASE as i32) - 1) {
                current_block = 7923086311623215889;
                break;
            }
            if EQTB[j as usize] == EQTB[(j + 1) as usize] {
                current_block = 8379985486002839332;
                break;
            }
            j += 1
        }
        match current_block {
            7923086311623215889 => l = INT_BASE as i32,
            _ => {
                j += 1;
                l = j;
                while j < (INT_BASE as i32) - 1 {
                    if EQTB[j as usize] != EQTB[(j + 1) as usize] {
                        break;
                    }
                    j += 1;
                }
            }
        }
        fmt_out.dump_one((l - k) as i32);
        fmt_out.dump(&EQTB[k as usize..l as usize]);
        k = j + 1;
        fmt_out.dump_one((k - l) as i32);
        if k == INT_BASE as i32 {
            break;
        }
    }
    loop {
        j = k;
        loop {
            if !(j < EQTB_SIZE as i32) {
                current_block = 10505255564575309249;
                break;
            }
            if EQTB[j as usize].val == EQTB[(j + 1) as usize].val {
                current_block = 18329769178042496632;
                break;
            }
            j += 1
        }
        match current_block {
            10505255564575309249 => l = (EQTB_SIZE as i32) + 1,
            _ => {
                j += 1;
                l = j;
                while j < EQTB_SIZE as i32 {
                    if EQTB[j as usize].val != EQTB[(j + 1) as usize].val {
                        break;
                    }
                    j += 1
                }
            }
        }

        // done2:
        fmt_out.dump_one((l - k) as i32);
        fmt_out.dump(&EQTB[k as usize..l as usize]);
        k = j + 1i32;
        fmt_out.dump_one((k - l) as i32);
        if !(k <= EQTB_SIZE as i32) {
            break;
        }
    }

    if hash_high > 0 {
        fmt_out.dump(&EQTB[EQTB_SIZE as usize + 1..EQTB_SIZE as usize + 1 + hash_high as usize]);
    }

    fmt_out.dump_one(par_loc as i32);
    fmt_out.dump_one(write_loc as i32);

    for p in 0..=PRIM_SIZE {
        fmt_out.dump_one(prim[p]);
    }

    for p in 0..=PRIM_SIZE {
        fmt_out.dump_one(prim_eqtb[p]);
    }

    /* control sequences */
    fmt_out.dump_one(hash_used as i32);
    cs_count = (FROZEN_CONTROL_SEQUENCE as i32 - 1) - hash_used + hash_high;

    for p in (HASH_BASE as i32)..=hash_used {
        if (*hash.offset(p as isize)).s1 != 0 {
            fmt_out.dump_one(p as i32);
            fmt_out.dump_one(*hash.offset(p as isize));
            cs_count += 1;
        }
    }

    let dump_slice = std::slice::from_raw_parts(
        hash.offset((hash_used + 1i32) as isize),
        ((UNDEFINED_CONTROL_SEQUENCE as i32 - 1) - hash_used) as _,
    );
    fmt_out.dump(dump_slice);

    if hash_high > 0 {
        let dump_slice =
            std::slice::from_raw_parts(hash.offset(EQTB_SIZE as isize + 1), hash_high as usize);
        fmt_out.dump(dump_slice);
    }

    fmt_out.dump_one(cs_count);

    print_ln();
    print_int(cs_count);
    print_cstr(b" multiletter control sequences");

    /* fonts */

    fmt_out.dump_one(fmem_ptr as i32);
    fmt_out.dump(&FONT_INFO[..fmem_ptr as usize]);
    fmt_out.dump_one(FONT_PTR as i32);
    fmt_out.dump(&FONT_CHECK[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_SIZE[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_DSIZE[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_PARAMS[..FONT_PTR + 1]);
    fmt_out.dump(&HYPHEN_CHAR[..FONT_PTR + 1]);
    fmt_out.dump(&SKEW_CHAR[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_NAME[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_AREA[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_BC[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_EC[..FONT_PTR + 1]);
    fmt_out.dump(&CHAR_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&WIDTH_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&HEIGHT_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&DEPTH_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&ITALIC_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&LIG_KERN_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&KERN_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&EXTEN_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&PARAM_BASE[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_GLUE[..FONT_PTR + 1]);
    fmt_out.dump(&BCHAR_LABEL[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_BCHAR[..FONT_PTR + 1]);
    fmt_out.dump(&FONT_FALSE_BCHAR[..FONT_PTR + 1]);

    for k in FONT_BASE..=FONT_PTR {
        print_nl_cstr(b"\\font");
        print_esc((*hash.offset(FONT_ID_BASE as isize + k as isize)).s1);
        print_char('=' as i32);

        if FONT_AREA[k] as u32 == AAT_FONT_FLAG
            || FONT_AREA[k] as u32 == OTGR_FONT_FLAG
            || !(FONT_MAPPING[k]).is_null()
        {
            print_file_name(FONT_NAME[k], EMPTY_STRING, EMPTY_STRING);

            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Can\'t \\dump a format with native fonts or font-mappings");

            help_ptr = 3_u8;
            help_line[2] = b"You really, really don\'t want to do this.";
            help_line[1] = b"It won\'t work, and only confuses me.";
            help_line[0] = b"(Load them at runtime, not as part of the format file.)";
            error();
        } else {
            print_file_name(FONT_NAME[k], FONT_AREA[k], EMPTY_STRING);
        }

        if FONT_SIZE[k] != FONT_DSIZE[k] {
            print_cstr(b" at ");
            print_scaled(FONT_SIZE[k]);
            print_cstr(b"pt");
        }
    }

    print_ln();
    print_int(fmem_ptr - 7);
    print_cstr(b" words of font info for ");
    print_int(FONT_PTR as i32 - 0);
    if FONT_PTR != FONT_BASE + 1 {
        print_cstr(b" preloaded fonts");
    } else {
        print_cstr(b" preloaded font");
    }

    /* hyphenation info */

    fmt_out.dump_one(HYPH_COUNT as i32);
    if HYPH_NEXT <= HYPH_PRIME as usize {
        HYPH_NEXT = HYPH_SIZE
    }
    fmt_out.dump_one(HYPH_NEXT as i32);

    for k in 0..=HYPH_SIZE {
        if HYPH_WORD[k] != 0 {
            fmt_out.dump_one((k as i64 + 65536 * HYPH_LINK[k] as i64) as i32);
            fmt_out.dump_one(HYPH_WORD[k]);
            fmt_out.dump_one(HYPH_LIST[k]);
        }
    }

    print_ln();
    print_int(HYPH_COUNT as i32);
    if HYPH_COUNT != 1 {
        print_cstr(b" hyphenation exceptions");
    } else {
        print_cstr(b" hyphenation exception");
    }
    if trie_not_ready {
        init_trie();
    }

    fmt_out.dump_one(trie_max);
    fmt_out.dump_one(hyph_start);
    let dump_slice = std::slice::from_raw_parts(trie_trl, (trie_max + 1) as usize);
    fmt_out.dump(dump_slice);
    let dump_slice = std::slice::from_raw_parts(trie_tro, (trie_max + 1) as usize);
    fmt_out.dump(dump_slice);
    let dump_slice = std::slice::from_raw_parts(trie_trc, (trie_max + 1) as usize);
    fmt_out.dump(dump_slice);
    fmt_out.dump_one(max_hyph_char);
    fmt_out.dump_one(trie_op_ptr as i32);
    fmt_out.dump(&hyf_distance[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_num[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_next[1..trie_op_ptr as usize + 1]);

    print_nl_cstr(b"Hyphenation trie of length ");
    print_int(trie_max);
    print_cstr(b" has ");
    print_int(trie_op_ptr);
    if trie_op_ptr != 1i32 {
        print_cstr(b" ops");
    } else {
        print_cstr(b" op");
    }
    print_cstr(b" out of ");
    print_int(TRIE_OP_SIZE as i32);

    for k in (0..=BIGGEST_LANG).rev() {
        if trie_used[k as usize] as i32 > 0i32 {
            print_nl_cstr(b"  ");
            print_int(trie_used[k as usize] as i32);
            print_cstr(b" for language ");
            print_int(k);
            fmt_out.dump_one(k as i32);
            fmt_out.dump_one(trie_used[k as usize] as i32);
        }
    }

    /* footer */

    fmt_out.dump_one(FORMAT_FOOTER_MAGIC);

    *INTPAR(IntPar::tracing_stats) = 0; /*:1361*/
    ttstub_output_close(fmt_out_owner);
}
unsafe fn pack_buffered_name(mut _n: i16, mut _a: i32, mut _b: i32) {
    free(name_of_file as *mut libc::c_void);
    name_of_file = xmalloc_array(format_default_length as usize + 1);

    strcpy(name_of_file, TEX_format_default);
    name_length = strlen(name_of_file) as i32;
}
unsafe fn load_fmt_file() -> bool {
    let mut _current_block: u64;
    let mut j: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut x: i32 = 0;

    j = cur_input.loc;

    /* This is where a first line starting with "&" used to
     * trigger code that would change the format file. */

    pack_buffered_name((format_default_length - 4) as i16, 1, 0);

    let fmt_in_owner = ttstub_input_open(name_of_file, TTInputFormat::FORMAT, 0);
    if fmt_in_owner.is_none() {
        abort!(
            "cannot open the format file \"{}\"",
            CStr::from_ptr(name_of_file).display()
        );
    }
    let mut fmt_in_owner = fmt_in_owner.unwrap();
    let fmt_in = &mut fmt_in_owner;

    cur_input.loc = j;

    if in_initex_mode {
        FONT_INFO = Vec::new();
        str_pool = Vec::new();
        str_start = Vec::new();
        free(yhash as *mut libc::c_void);
        EQTB = Vec::new();
        MEM = Vec::new();
    }

    fn bad_fmt() -> ! {
        panic!("fatal format file error");
    };

    /* start reading the header */

    fmt_in.undump_one(&mut x);
    if x != FORMAT_HEADER_MAGIC {
        bad_fmt();
    }

    fmt_in.undump_one(&mut x);
    if x != FORMAT_SERIAL {
        abort!(
            "format file \"{}\" is of the wrong version: expected {}, found {}",
            CStr::from_ptr(name_of_file).display(),
            FORMAT_SERIAL,
            x
        );
    }

    /* hash table parameters */

    fmt_in.undump_one(&mut hash_high);
    if hash_high < 0 || hash_high > sup_hash_extra {
        bad_fmt();
    }
    if hash_extra < hash_high {
        hash_extra = hash_high
    }

    EQTB_TOP = EQTB_SIZE + hash_extra as usize;
    if hash_extra == 0 {
        hash_top = UNDEFINED_CONTROL_SEQUENCE as i32;
    } else {
        hash_top = EQTB_TOP as i32;
    }

    yhash = xmalloc_array::<b32x2>((1 + hash_top - hash_offset) as usize);
    hash = yhash.offset(-514);
    (*hash.offset(HASH_BASE as isize)).s0 = 0;
    (*hash.offset(HASH_BASE as isize)).s1 = 0;

    x = (HASH_BASE + 1) as i32;
    while x <= hash_top {
        *hash.offset(x as isize) = *hash.offset(HASH_BASE as isize);
        x += 1;
    }

    EQTB = vec![EqtbWord::default(); EQTB_TOP + 2];
    EQTB[UNDEFINED_CONTROL_SEQUENCE as usize].cmd = Cmd::UndefinedCS as _;
    EQTB[UNDEFINED_CONTROL_SEQUENCE as usize].val = TEX_NULL as _;
    EQTB[UNDEFINED_CONTROL_SEQUENCE as usize].lvl = LEVEL_ZERO as _;

    x = EQTB_SIZE as i32 + 1;
    while x <= EQTB_TOP as i32 {
        EQTB[x as usize] = EQTB[UNDEFINED_CONTROL_SEQUENCE as usize];
        x += 1;
    }

    max_reg_num = 32767;
    max_reg_help_line = b"A register number must be between 0 and 32767.";

    /* "memory locations" */

    fmt_in.undump_one(&mut x);
    if x != MEM_TOP as i32 {
        bad_fmt();
    }

    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    page_tail = PAGE_HEAD as i32;
    MEM = vec![memory_word::default(); MEM_TOP as usize + 2];

    fmt_in.undump_one(&mut x);
    if x != EQTB_SIZE as i32 {
        bad_fmt();
    }

    fmt_in.undump_one(&mut x);
    if x != HASH_PRIME as i32 {
        bad_fmt();
    }

    fmt_in.undump_one(&mut x);
    if x != HYPH_PRIME {
        bad_fmt();
    }

    /* string pool */

    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x as i64 > sup_pool_size as i64 - pool_free as i64 {
        panic!("must increase string_pool_size");
    }
    pool_ptr = x;
    if pool_size < pool_ptr + pool_free {
        pool_size = pool_ptr + pool_free
    }
    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x as i64 > sup_max_strings as i64 - strings_free as i64 {
        panic!("must increase sup_strings");
    }
    str_ptr = x;

    if (max_strings as i32) < str_ptr + strings_free {
        max_strings = (str_ptr + strings_free) as usize
    }

    str_start = vec![pool_pointer::default(); max_strings + 1];
    let mut i: i32 = 0;
    fmt_in.undump(&mut str_start[..(str_ptr - 65536 + 1) as usize]);
    i = 0;
    while i < str_ptr - 65536 + 1 {
        if str_start[i as usize] < 0 || str_start[i as usize] > pool_ptr {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                str_start[i as usize] as uintptr_t,
                &mut str_start[0] as *mut pool_pointer as uintptr_t,
                0 as uintptr_t,
                pool_ptr as uintptr_t
            );
        }
        i += 1
    }
    str_pool = vec![0; pool_size as usize + 1];
    fmt_in.undump(&mut str_pool[..pool_ptr as usize]);
    init_str_ptr = str_ptr;
    init_pool_ptr = pool_ptr;
    /* "By sorting the list of available spaces in the variable-size portion
     * of |mem|, we are usually able to get by without having to dump very
     * much of the dynamic memory." */
    fmt_in.undump_one(&mut x);
    if x < 1019 || x > MEM_TOP as i32 - HI_MEM_STAT_USAGE {
        bad_fmt();
    } else {
        lo_mem_max = x;
    }
    fmt_in.undump_one(&mut x);
    if x < 20 || x > lo_mem_max {
        bad_fmt();
    } else {
        rover = x;
    }
    for k in INT_VAL..=INTER_CHAR_VAL {
        fmt_in.undump_one(&mut x);
        if x < MIN_HALFWORD || x > lo_mem_max {
            bad_fmt();
        } else {
            sa_root[k as usize] = x;
        }
    }

    p = 0;
    q = rover;

    loop {
        fmt_in.undump(&mut MEM[p as usize..(q + 2) as usize]);
        p = q + MEM[q as usize].b32.s0;
        if p > lo_mem_max
            || q >= MEM[(q + 1) as usize].b32.s1 && MEM[(q + 1) as usize].b32.s1 != rover
        {
            bad_fmt();
        }
        q = MEM[(q + 1) as usize].b32.s1;
        if !(q != rover) {
            break;
        }
    }

    fmt_in.undump(&mut MEM[p as usize..(lo_mem_max + 1) as usize]);

    fmt_in.undump_one(&mut x);
    if x < lo_mem_max + 1 || x > PRE_ADJUST_HEAD as i32 {
        bad_fmt();
    } else {
        hi_mem_min = x;
    }

    fmt_in.undump_one(&mut x);
    if x < MIN_HALFWORD || x > MEM_TOP as i32 {
        bad_fmt();
    } else {
        avail = x;
    }

    mem_end = MEM_TOP as i32;

    fmt_in.undump(&mut MEM[hi_mem_min as usize..(mem_end + 1) as usize]);
    fmt_in.undump_one(&mut var_used);
    fmt_in.undump_one(&mut dyn_used);

    /* equivalents table / primitives
     *
     * "The table of equivalents usually contains repeated information, so we
     * dump it in compressed form: The sequence of $n + 2$ values
     * $(n, x_1, \ldots, x_n, m)$ in the format file represents $n + m$ consecutive
     * entries of |eqtb|, with |m| extra copies of $x_n$, namely
     * $(x_1, \ldots, x_n, x_n, \ldots, x_n)$"
     */

    let mut k = ACTIVE_BASE as i32;

    loop {
        fmt_in.undump_one(&mut x);
        if x < 1 || k + x > (EQTB_SIZE as i32) + 1 {
            bad_fmt();
        }

        fmt_in.undump(&mut EQTB[k as usize..(k + x) as usize]);
        k = k + x;

        fmt_in.undump_one(&mut x);
        if x < 0 || k + x > (EQTB_SIZE as i32) + 1 {
            bad_fmt();
        }

        j = k;
        while j <= k + x - 1 {
            EQTB[j as usize] = EQTB[(k - 1) as usize];
            j += 1
        }
        k = k + x;
        if !(k <= EQTB_SIZE as i32) {
            break;
        }
    }

    if hash_high > 0 {
        fmt_in
            .undump(&mut EQTB[EQTB_SIZE as usize + 1..EQTB_SIZE as usize + 1 + hash_high as usize]);
    }

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > hash_top {
        bad_fmt();
    } else {
        par_loc = x;
    }

    par_token = CS_TOKEN_FLAG + par_loc;

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > hash_top {
        bad_fmt();
    } else {
        write_loc = x;
    }

    /* control sequence names
     *
     * "A different scheme is used to compress the hash table, since its lower
     * region is usually sparse. When |text(p) != 0| for |p <= hash_used|, we
     * output two words, |p| and |hash[p]|. The hash table is, of course,
     * densely packed for |p >= hash_used|, so the remaining entries are
     * output in a block."
     */

    p = 0i32;
    while p <= 500i32 {
        fmt_in.undump_one(&mut prim[p as usize]);
        p += 1
    }

    p = 0i32;
    while p <= 500i32 {
        fmt_in.undump_one(&mut prim_eqtb[p as usize]);
        p += 1
    }

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > FROZEN_CONTROL_SEQUENCE as i32 {
        bad_fmt();
    } else {
        hash_used = x;
    }

    p = HASH_BASE as i32 - 1;

    loop {
        fmt_in.undump_one(&mut x);
        if x < p + 1 || x > hash_used {
            bad_fmt();
        } else {
            p = x;
        }
        fmt_in.undump_one(&mut *hash.offset(p as isize));
        if !(p != hash_used) {
            break;
        }
    }
    let undump_slice = std::slice::from_raw_parts_mut(
        hash.offset((hash_used + 1) as isize),
        (UNDEFINED_CONTROL_SEQUENCE - 1) - (hash_used as usize),
    );

    fmt_in.undump(undump_slice);
    if hash_high > 0 {
        let undump_slice =
            std::slice::from_raw_parts_mut(hash.offset(EQTB_SIZE as isize + 1), hash_high as usize);
        fmt_in.undump(undump_slice);
    }

    fmt_in.undump_one(&mut cs_count);

    /* font info */

    fmt_in.undump_one(&mut x);
    if x < 7 {
        bad_fmt();
    }
    if x > sup_font_mem_size {
        panic!("must increase font_mem_size");
    }

    fmem_ptr = x;
    if fmem_ptr > FONT_MEM_SIZE as i32 {
        FONT_MEM_SIZE = fmem_ptr as usize
    }
    FONT_INFO = vec![memory_word::default(); FONT_MEM_SIZE + 1];
    fmt_in.undump(&mut FONT_INFO[..fmem_ptr as usize]);
    fmt_in.undump_one(&mut x);
    if x < FONT_BASE as i32 {
        bad_fmt();
    }
    if x > (FONT_BASE + MAX_FONT_MAX) as i32 {
        panic!("must increase FONT_MAX");
    }

    FONT_PTR = x as usize;

    FONT_MAPPING = vec![0 as *mut libc::c_void; FONT_MAX + 1];
    FONT_LAYOUT_ENGINE = vec![0 as *mut libc::c_void; FONT_MAX + 1];
    FONT_FLAGS = vec![0; FONT_MAX + 1];
    FONT_LETTER_SPACE = vec![0; FONT_MAX + 1];
    FONT_CHECK = vec![b16x4_le_t::default(); FONT_MAX + 1];
    FONT_SIZE = vec![0; FONT_MAX + 1];
    FONT_DSIZE = vec![0; FONT_MAX + 1];
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

    for k in 0..=FONT_PTR {
        FONT_MAPPING[k as usize] = 0 as *mut libc::c_void;
    }

    fmt_in.undump(&mut FONT_CHECK[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_SIZE[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_DSIZE[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_PARAMS[..FONT_PTR + 1]);
    for i_0 in 0..FONT_PTR + 1 {
        if FONT_PARAMS[i_0] < MIN_HALFWORD || FONT_PARAMS[i_0] > 0x3fffffff {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_0,
                FONT_PARAMS[i_0],
                FONT_PARAMS.as_ptr() as uintptr_t,
                MIN_HALFWORD,
                0x3fffffff
            );
        }
    }
    fmt_in.undump(&mut HYPHEN_CHAR[..FONT_PTR + 1]);
    fmt_in.undump(&mut SKEW_CHAR[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_NAME[..FONT_PTR + 1]);
    for i_1 in 0..FONT_PTR + 1 {
        if FONT_NAME[i_1] > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_1,
                FONT_NAME[i_1],
                FONT_NAME.as_ptr() as uintptr_t,
                str_ptr
            );
        }
    }
    fmt_in.undump(&mut FONT_AREA[..FONT_PTR + 1]);
    for i_2 in 0..FONT_PTR + 1 {
        if FONT_AREA[i_2] > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_2,
                FONT_AREA[i_2],
                FONT_AREA.as_ptr() as uintptr_t,
                str_ptr
            );
        }
    }
    fmt_in.undump(&mut FONT_BC[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_EC[..FONT_PTR + 1]);
    fmt_in.undump(&mut CHAR_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut WIDTH_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut HEIGHT_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut DEPTH_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut ITALIC_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut LIG_KERN_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut KERN_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut EXTEN_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut PARAM_BASE[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_GLUE[..FONT_PTR + 1]);
    for i_3 in 0..FONT_PTR + 1 {
        if FONT_GLUE[i_3] < MIN_HALFWORD || FONT_GLUE[i_3] > lo_mem_max {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_3,
                FONT_GLUE[i_3],
                FONT_GLUE.as_ptr() as uintptr_t,
                MIN_HALFWORD,
                lo_mem_max
            );
        }
    }
    fmt_in.undump(&mut BCHAR_LABEL[..FONT_PTR + 1]);
    for i_4 in 0..FONT_PTR + 1 {
        if BCHAR_LABEL[i_4] < 0 || BCHAR_LABEL[i_4] > fmem_ptr - 1 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_4,
                BCHAR_LABEL[i_4],
                BCHAR_LABEL.as_ptr() as uintptr_t,
                0,
                fmem_ptr - 1
            );
        }
    }
    fmt_in.undump(&mut FONT_BCHAR[..FONT_PTR + 1]);
    for i_5 in 0..FONT_PTR + 1 {
        if FONT_BCHAR[i_5] < 0 || FONT_BCHAR[i_5] > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_5,
                FONT_BCHAR[i_5],
                FONT_BCHAR.as_ptr() as uintptr_t,
                0,
                65536
            );
        }
    }
    fmt_in.undump(&mut FONT_FALSE_BCHAR[..FONT_PTR + 1]);
    for i_6 in 0..FONT_PTR + 1 {
        if FONT_FALSE_BCHAR[i_6] < 0 || FONT_FALSE_BCHAR[i_6] > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_6,
                FONT_FALSE_BCHAR[i_6],
                FONT_FALSE_BCHAR.as_ptr() as uintptr_t,
                0,
                65536
            );
        }
    }

    /* hyphenations */

    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }

    if x > HYPH_SIZE as i32 {
        panic!("must increase HYPH_SIZE");
    }
    HYPH_COUNT = x as usize;

    fmt_in.undump_one(&mut x);
    if x < HYPH_PRIME {
        bad_fmt();
    }
    if x > HYPH_SIZE as i32 {
        panic!("must increase HYPH_SIZE");
    }
    HYPH_NEXT = x as usize;

    j = 0;

    for _k in 1..=HYPH_COUNT {
        fmt_in.undump_one(&mut j);
        if j < 0i32 {
            bad_fmt();
        }
        if j > 65535 {
            HYPH_NEXT = (j as i64 / 65536) as usize;
            j = (j as i64 - HYPH_NEXT as i64 * 65536) as i32
        } else {
            HYPH_NEXT = 0
        }
        if j >= HYPH_SIZE as i32 || HYPH_NEXT > HYPH_SIZE {
            bad_fmt();
        }
        HYPH_LINK[j as usize] = HYPH_NEXT as hyph_pointer;
        fmt_in.undump_one(&mut x);
        if x < 0 || x > str_ptr {
            bad_fmt();
        } else {
            HYPH_WORD[j as usize] = x;
        }
        fmt_in.undump_one(&mut x);
        if x < MIN_HALFWORD || x > MAX_HALFWORD {
            bad_fmt();
        } else {
            HYPH_LIST[j as usize] = x;
        }
    }
    j += 1;
    if j < HYPH_PRIME {
        j = HYPH_PRIME
    }

    HYPH_NEXT = j as usize;
    if HYPH_NEXT >= HYPH_SIZE {
        HYPH_NEXT = HYPH_PRIME as usize
    } else if HYPH_NEXT >= HYPH_PRIME as usize {
        HYPH_NEXT += 1
    }
    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x > trie_size {
        panic!("must increase trie_size");
    }

    j = x;
    trie_max = j;

    fmt_in.undump_one(&mut x);
    if x < 0 || x > j {
        bad_fmt();
    } else {
        hyph_start = x;
    }

    if trie_trl.is_null() {
        trie_trl = xmalloc_array(j as usize + 1);
    }
    let undump_slice = std::slice::from_raw_parts_mut(trie_trl, (j + 1) as usize);
    fmt_in.undump(undump_slice);
    if trie_tro.is_null() {
        trie_tro = xmalloc_array(j as usize + 1);
    }
    let undump_slice = std::slice::from_raw_parts_mut(trie_tro, (j + 1) as usize);
    fmt_in.undump(undump_slice);
    if trie_trc.is_null() {
        trie_trc = xmalloc_array(j as usize + 1);
    }
    let undump_slice = std::slice::from_raw_parts_mut(trie_trc, (j + 1) as usize);
    fmt_in.undump(undump_slice);
    fmt_in.undump_one(&mut max_hyph_char);
    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x > TRIE_OP_SIZE {
        panic!("must increase TRIE_OP_SIZE");
    }

    j = x;
    trie_op_ptr = j;

    fmt_in.undump(&mut hyf_distance[1..(j + 1) as usize]);
    fmt_in.undump(&mut hyf_num[1..(j + 1) as usize]);
    fmt_in.undump(&mut hyf_next[1..(j + 1) as usize]);
    let mut i_7 = 0;
    while i_7 < j as usize {
        if hyf_next[1 + i_7] as i64 > 65535 {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_7,
                hyf_next[1 + i_7] as uintptr_t,
                &mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode as uintptr_t,
                65535 as uintptr_t
            );
        }
        i_7 += 1
    }
    for k in 0..=BIGGEST_LANG {
        trie_used[k as usize] = 0;
    }
    let mut k = BIGGEST_LANG + 1;
    loop {
        if !(j > 0) {
            break;
        }
        fmt_in.undump_one(&mut x);
        if x < 0i32 || x > k - 1i32 {
            bad_fmt();
        } else {
            k = x;
        }
        fmt_in.undump_one(&mut x);
        if x < 1 || x > j {
            bad_fmt();
        }
        trie_used[k as usize] = x as trie_opcode;
        j = j - x;
        op_start[k as usize] = j
    }
    trie_not_ready = false;

    /* trailer */

    fmt_in.undump_one(&mut x);
    if x != FORMAT_FOOTER_MAGIC {
        bad_fmt();
    }

    ttstub_input_close(fmt_in_owner);
    return true;
}

unsafe fn final_cleanup() {
    let mut c = cur_chr as i16;
    if job_name == 0 {
        open_log_file();
    }
    while INPUT_PTR > 0 {
        if cur_input.state == InputState::TokenList {
            end_token_list();
        } else {
            end_file_reading();
        }
    }
    while open_parens > 0 {
        print_cstr(b" )");
        open_parens -= 1;
    }
    if cur_level > LEVEL_ONE {
        print_nl('(' as i32);
        print_esc_cstr(b"end occurred ");
        print_cstr(b"inside a group at level ");
        print_int(cur_level as i32 - 1);
        print_char(')' as i32);
        show_save_groups();
    }
    while !cond_ptr.is_texnull() {
        print_nl('(' as i32);
        print_esc_cstr(b"end occurred ");
        print_cstr(b"when ");
        print_cmd_chr(Cmd::IfTest, cur_if as i32);
        if if_line != 0 {
            print_cstr(b" on line ");
            print_int(if_line);
        }
        print_cstr(b" was incomplete)");
        if_line = MEM[(cond_ptr + 1) as usize].b32.s1;
        cur_if = MEM[cond_ptr as usize].b16.s0 as i16;
        temp_ptr = cond_ptr;
        cond_ptr = MEM[cond_ptr as usize].b32.s1;
        free_node(temp_ptr as usize, IF_NODE_SIZE);
    }
    if history != TTHistory::SPOTLESS {
        if history == TTHistory::WARNING_ISSUED || interaction < ERROR_STOP_MODE {
            if selector == Selector::TERM_AND_LOG {
                selector = Selector::TERM_ONLY;
                print_nl_cstr(b"(see the transcript file for additional information)");
                selector = Selector::TERM_AND_LOG
            }
        }
    }
    if c == 1 {
        if in_initex_mode {
            c = TOP_MARK_CODE as i16;
            let mut for_end = SPLIT_BOT_MARK_CODE as i32;
            if c as i32 <= for_end {
                loop {
                    if !cur_mark[c as usize].is_texnull() {
                        delete_token_ref(cur_mark[c as usize] as usize);
                    }
                    let fresh17 = c;
                    c = c + 1;
                    if !((fresh17 as i32) < for_end) {
                        break;
                    }
                }
            }
            if !sa_root[MARK_VAL as usize].is_texnull() {
                if do_marks(3, 0, sa_root[MARK_VAL as usize]) {
                    sa_root[MARK_VAL as usize] = TEX_NULL;
                }
            }
            c = LAST_BOX_CODE as i16;
            let mut for_end_0 = VSPLIT_CODE;
            if c as i32 <= for_end_0 {
                loop {
                    flush_node_list(disc_ptr[c as usize].opt());
                    let fresh18 = c;
                    c = c + 1;
                    if !((fresh18 as i32) < for_end_0) {
                        break;
                    }
                }
            }
            if last_glue != MAX_HALFWORD {
                delete_glue_ref(last_glue as usize);
            }
            store_fmt_file();
            return;
        }
        print_nl_cstr(b"(\\dump is performed only by INITEX)");
        return;
    };
}
/* Engine initialization */
static mut stdin_ufile: UFILE = UFILE {
    handle: None,
    savedChar: 0,
    skipNextLF: 0,
    encodingMode: 0,
    conversionData: ptr::null_mut(),
};
unsafe fn init_io() {
    /* This is largely vestigial at this point */
    stdin_ufile.handle = None;
    stdin_ufile.savedChar = -1;
    stdin_ufile.skipNextLF = 0;
    stdin_ufile.encodingMode = UTF8;
    stdin_ufile.conversionData = 0 as *mut libc::c_void;
    INPUT_FILE[0] = &mut stdin_ufile;

    BUFFER[first as usize] = 0;
    last = first;
    cur_input.loc = first;
    cur_input.limit = last;
    first = last + 1;
}
unsafe fn initialize_more_variables() {
    doing_special = false;
    native_text_size = 128;
    native_text = xmalloc(
        (native_text_size as u64).wrapping_mul(::std::mem::size_of::<UTF16_code>() as _) as _,
    ) as *mut UTF16_code;

    interaction = ERROR_STOP_MODE;

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
    page_tail = PAGE_HEAD as i32;
    last_glue = MAX_HALFWORD;
    last_penalty = 0;
    last_kern = 0;
    page_so_far[7] = 0;

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
    prim_eqtb[0].val = TEX_NULL;

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
    cur_mark[TOP_MARK_CODE] = TEX_NULL;
    cur_mark[FIRST_MARK_CODE] = TEX_NULL;
    cur_mark[BOT_MARK_CODE] = TEX_NULL;
    cur_mark[SPLIT_FIRST_MARK_CODE] = TEX_NULL;
    cur_mark[SPLIT_BOT_MARK_CODE] = TEX_NULL;
    cur_val = 0;
    cur_val_level = INT_VAL;
    radix = 0;
    cur_order = GlueOrder::Normal as u8;

    for k in 0..=16 {
        read_open[k] = CLOSED;
    }

    cond_ptr = TEX_NULL;
    if_limit = NORMAL as u8;
    cur_if = 0;
    if_line = 0;
    TOTAL_PAGES = 0;
    max_v = 0;
    max_h = 0;
    max_push = 0;
    last_bop = -1;
    doing_leaders = false;
    dead_cycles = 0;
    adjust_tail = TEX_NULL;
    last_badness = 0;
    pre_adjust_tail = TEX_NULL;
    pack_begin_line = 0;
    empty.s1 = EMPTY;
    empty.s0 = TEX_NULL;
    align_ptr = TEX_NULL;
    cur_align = TEX_NULL;
    cur_span = TEX_NULL;
    cur_loop = TEX_NULL;
    cur_head = TEX_NULL;
    cur_tail = TEX_NULL;
    cur_pre_head = TEX_NULL;
    cur_pre_tail = TEX_NULL;
    cur_f = 0;
    max_hyph_char = TOO_BIG_LANG;

    for z in 0..=HYPH_SIZE {
        HYPH_WORD[z as usize] = 0;
        HYPH_LIST[z as usize] = TEX_NULL;
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

    LR_ptr = TEX_NULL;
    LR_problems = 0i32;
    cur_dir = LR::LeftToRight;
    pseudo_files = TEX_NULL;
    sa_root[7] = TEX_NULL;
    sa_null.b32.s0 = TEX_NULL;
    sa_null.b32.s1 = TEX_NULL;
    sa_chain = TEX_NULL;
    sa_level = LEVEL_ZERO;
    disc_ptr[2] = TEX_NULL;
    disc_ptr[3] = TEX_NULL;
    edit_name_start = 0;
    stop_at_space = true;
}
unsafe fn initialize_more_initex_variables() {
    for k in 1..=19 {
        MEM[k].b32.s1 = 0;
    }

    for k in (0..=19).step_by(4) {
        MEM[k].b32.s1 = TEX_NULL + 1;
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
    MEM[lo_mem_max as usize].b32.s1 = TEX_NULL;
    MEM[lo_mem_max as usize].b32.s0 = TEX_NULL;

    for k in PRE_ADJUST_HEAD..=MEM_TOP {
        MEM[k] = MEM[lo_mem_max as usize];
    }

    MEM[OMIT_TEMPLATE as usize].b32.s0 = CS_TOKEN_FLAG + FROZEN_END_TEMPLATE as i32;
    MEM[END_SPAN].b32.s1 = std::u16::MAX as i32 + 1;
    MEM[END_SPAN].b32.s0 = TEX_NULL;
    MEM[ACTIVE_LIST].b16.s1 = BreakType::Hyphenated as _;
    MEM[ACTIVE_LIST + 1].b32.s0 = MAX_HALFWORD;
    MEM[ACTIVE_LIST].b16.s0 = 0;
    MEM[PAGE_INS_HEAD].b16.s0 = 255;
    MEM[PAGE_INS_HEAD].b16.s1 = SPLIT_UP as u16;
    MEM[PAGE_INS_HEAD].b32.s1 = PAGE_INS_HEAD as i32;
    MEM[PAGE_HEAD].b16.s1 = TextNode::Glue as u16;
    MEM[PAGE_HEAD].b16.s0 = NORMAL;
    avail = TEX_NULL;
    mem_end = MEM_TOP as i32;
    hi_mem_min = PRE_ADJUST_HEAD as i32;
    var_used = 20;
    dyn_used = HI_MEM_STAT_USAGE;
    EQTB[UNDEFINED_CONTROL_SEQUENCE].cmd = Cmd::UndefinedCS as u16;
    EQTB[UNDEFINED_CONTROL_SEQUENCE].val = TEX_NULL;
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
    *LOCAL(Local::par_shape) = TEX_NULL;
    EQTB[LOCAL_BASE + Local::par_shape as usize].cmd = Cmd::ShapeRef as _;
    EQTB[LOCAL_BASE + Local::par_shape as usize].lvl = LEVEL_ONE as _;

    for k in ETEX_PEN_BASE..=(ETEX_PENS - 1) {
        EQTB[k] = EQTB[LOCAL_BASE + Local::par_shape as usize];
    }

    for k in (LOCAL_BASE + Local::output_routine as usize)..=(TOKS_BASE + NUMBER_REGS - 1) {
        EQTB[k] = EQTB[UNDEFINED_CONTROL_SEQUENCE];
    }

    EQTB[BOX_BASE].val = TEX_NULL;
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

    *CAT_CODE(13) = Cmd::CarRet as _;
    *CAT_CODE(32) = Cmd::Spacer as _;
    *CAT_CODE(92) = ESCAPE as _;
    *CAT_CODE(37) = Cmd::Comment as _;
    *CAT_CODE(127) = INVALID_CHAR as _;

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
    *INTPAR(IntPar::end_line_char) = CARRIAGE_RETURN;

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
    (*hash.offset(FROZEN_DONT_EXPAND as isize)).s1 = maketexstring(b"notexpanded:");
    EQTB[FROZEN_PRIMITIVE as usize].cmd = Cmd::IgnoreSpaces as u16;
    EQTB[FROZEN_PRIMITIVE as usize].val = 1;
    EQTB[FROZEN_PRIMITIVE as usize].lvl = LEVEL_ONE;
    (*hash.offset(FROZEN_PRIMITIVE as isize)).s1 = maketexstring(b"primitive");

    for k in (-TRIE_OP_SIZE)..=TRIE_OP_SIZE {
        _trie_op_hash_array[(k as i64 - -35111) as usize] = 0i32;
    }

    for k in 0..=BIGGEST_LANG {
        trie_used[k as usize] = MIN_TRIE_OP;
    }

    max_op_used = MIN_TRIE_OP;
    trie_op_ptr = 0;
    trie_not_ready = true;
    (*hash.offset(FROZEN_PROTECTION as isize)).s1 = maketexstring(b"inaccessible");

    format_ident = maketexstring(b" (INITEX)");

    (*hash.offset(END_WRITE as isize)).s1 = maketexstring(b"endwrite");
    EQTB[END_WRITE as usize].lvl = LEVEL_ONE;
    EQTB[END_WRITE as usize].cmd = Cmd::OuterCall as u16;
    EQTB[END_WRITE as usize].val = TEX_NULL;

    max_reg_num = 32767;
    max_reg_help_line = b"A register number must be between 0 and 32767.";

    for i in (INT_VAL as usize)..=(INTER_CHAR_VAL as usize) {
        sa_root[i] = TEX_NULL;
    }

    *INTPAR(IntPar::xetex_hyphenatable_length) = 63;
}
/*:1370*/
/*1371: */
unsafe fn initialize_primitives() {
    no_new_control_sequence = false;
    first = 0i32;
    primitive(
        b"lineskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::line_skip as usize,
    );
    primitive(
        b"baselineskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::baseline_skip as usize,
    );
    primitive(
        b"parskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::par_skip as usize,
    );
    primitive(
        b"abovedisplayskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::above_display_skip as usize,
    );
    primitive(
        b"belowdisplayskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::below_display_skip as usize,
    );
    primitive(
        b"abovedisplayshortskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::above_display_short_skip as usize,
    );
    primitive(
        b"belowdisplayshortskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::below_display_short_skip as usize,
    );
    primitive(
        b"leftskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::left_skip as usize,
    );
    primitive(
        b"rightskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::right_skip as usize,
    );
    primitive(
        b"topskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::top_skip as usize,
    );
    primitive(
        b"splittopskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::split_top_skip as usize,
    );
    primitive(
        b"tabskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::tab_skip as usize,
    );
    primitive(
        b"spaceskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::space_skip as usize,
    );
    primitive(
        b"xspaceskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::xspace_skip as usize,
    );
    primitive(
        b"parfillskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::par_fill_skip as usize,
    );
    primitive(
        b"XeTeXlinebreakskip",
        Cmd::AssignGlue,
        GLUE_BASE + GluePar::xetex_linebreak_skip as usize,
    );

    primitive(
        b"thinmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::thin_mu_skip as usize,
    );
    primitive(
        b"medmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::med_mu_skip as usize,
    );
    primitive(
        b"thickmuskip",
        Cmd::AssignMuGlue,
        GLUE_BASE + GluePar::thick_mu_skip as usize,
    );

    primitive(
        b"output",
        Cmd::AssignToks,
        LOCAL_BASE + Local::output_routine as usize,
    );
    primitive(
        b"everypar",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_par as usize,
    );
    primitive(
        b"everymath",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_math as usize,
    );
    primitive(
        b"everydisplay",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_display as usize,
    );
    primitive(
        b"everyhbox",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_hbox as usize,
    );
    primitive(
        b"everyvbox",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_vbox as usize,
    );
    primitive(
        b"everyjob",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_job as usize,
    );
    primitive(
        b"everycr",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_cr as usize,
    );
    primitive(
        b"errhelp",
        Cmd::AssignToks,
        LOCAL_BASE + Local::err_help as usize,
    );
    primitive(
        b"everyeof",
        Cmd::AssignToks,
        LOCAL_BASE + Local::every_eof as usize,
    );
    primitive(
        b"XeTeXinterchartoks",
        Cmd::AssignToks,
        LOCAL_BASE + Local::xetex_inter_char as usize,
    );
    primitive(
        b"TectonicCodaTokens",
        Cmd::AssignToks,
        LOCAL_BASE + Local::TectonicCodaTokens as usize,
    );

    primitive(
        b"pretolerance",
        Cmd::AssignInt,
        INT_BASE + IntPar::pretolerance as usize,
    );
    primitive(
        b"tolerance",
        Cmd::AssignInt,
        INT_BASE + IntPar::tolerance as usize,
    );
    primitive(
        b"linepenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::line_penalty as usize,
    );
    primitive(
        b"hyphenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::hyphen_penalty as usize,
    );
    primitive(
        b"exhyphenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::ex_hyphen_penalty as usize,
    );
    primitive(
        b"clubpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::club_penalty as usize,
    );
    primitive(
        b"widowpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::widow_penalty as usize,
    );
    primitive(
        b"displaywidowpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::display_widow_penalty as usize,
    );
    primitive(
        b"brokenpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::broken_penalty as usize,
    );
    primitive(
        b"binoppenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::bin_op_penalty as usize,
    );
    primitive(
        b"relpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::rel_penalty as usize,
    );
    primitive(
        b"predisplaypenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::pre_display_penalty as usize,
    );
    primitive(
        b"postdisplaypenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::post_display_penalty as usize,
    );
    primitive(
        b"interlinepenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::inter_line_penalty as usize,
    );
    primitive(
        b"doublehyphendemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::double_hyphen_demerits as usize,
    );
    primitive(
        b"finalhyphendemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::final_hyphen_demerits as usize,
    );
    primitive(
        b"adjdemerits",
        Cmd::AssignInt,
        INT_BASE + IntPar::adj_demerits as usize,
    );
    primitive(b"mag", Cmd::AssignInt, INT_BASE + IntPar::mag as usize);
    primitive(
        b"delimiterfactor",
        Cmd::AssignInt,
        INT_BASE + IntPar::delimiter_factor as usize,
    );
    primitive(
        b"looseness",
        Cmd::AssignInt,
        INT_BASE + IntPar::looseness as usize,
    );
    primitive(b"time", Cmd::AssignInt, INT_BASE + IntPar::time as usize);
    primitive(b"day", Cmd::AssignInt, INT_BASE + IntPar::day as usize);
    primitive(b"month", Cmd::AssignInt, INT_BASE + IntPar::month as usize);
    primitive(b"year", Cmd::AssignInt, INT_BASE + IntPar::year as usize);
    primitive(
        b"showboxbreadth",
        Cmd::AssignInt,
        INT_BASE + IntPar::show_box_breadth as usize,
    );
    primitive(
        b"showboxdepth",
        Cmd::AssignInt,
        INT_BASE + IntPar::show_box_depth as usize,
    );
    primitive(
        b"hbadness",
        Cmd::AssignInt,
        INT_BASE + IntPar::hbadness as usize,
    );
    primitive(
        b"vbadness",
        Cmd::AssignInt,
        INT_BASE + IntPar::vbadness as usize,
    );
    primitive(
        b"pausing",
        Cmd::AssignInt,
        INT_BASE + IntPar::pausing as usize,
    );
    primitive(
        b"tracingonline",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_online as usize,
    );
    primitive(
        b"tracingmacros",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_macros as usize,
    );
    primitive(
        b"tracingstats",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_stats as usize,
    );
    primitive(
        b"tracingparagraphs",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_paragraphs as usize,
    );
    primitive(
        b"tracingpages",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_pages as usize,
    );
    primitive(
        b"tracingoutput",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_output as usize,
    );
    primitive(
        b"tracinglostchars",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_lost_chars as usize,
    );
    primitive(
        b"tracingcommands",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_commands as usize,
    );
    primitive(
        b"tracingrestores",
        Cmd::AssignInt,
        INT_BASE + IntPar::tracing_restores as usize,
    );
    primitive(
        b"uchyph",
        Cmd::AssignInt,
        INT_BASE + IntPar::uc_hyph as usize,
    );
    primitive(
        b"outputpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::output_penalty as usize,
    );
    primitive(
        b"maxdeadcycles",
        Cmd::AssignInt,
        INT_BASE + IntPar::max_dead_cycles as usize,
    );
    primitive(
        b"hangafter",
        Cmd::AssignInt,
        INT_BASE + IntPar::hang_after as usize,
    );
    primitive(
        b"floatingpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::floating_penalty as usize,
    );
    primitive(
        b"globaldefs",
        Cmd::AssignInt,
        INT_BASE + IntPar::global_defs as usize,
    );
    primitive(b"fam", Cmd::AssignInt, INT_BASE + IntPar::cur_fam as usize);
    primitive(
        b"escapechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::escape_char as usize,
    );
    primitive(
        b"defaulthyphenchar",
        Cmd::AssignInt,
        INT_BASE + IntPar::default_hyphen_char as usize,
    );
    primitive(
        b"defaultskewchar",
        Cmd::AssignInt,
        INT_BASE + IntPar::default_skew_char as usize,
    );
    primitive(
        b"endlinechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::end_line_char as usize,
    );
    primitive(
        b"newlinechar",
        Cmd::AssignInt,
        INT_BASE + IntPar::new_line_char as usize,
    );
    primitive(
        b"language",
        Cmd::AssignInt,
        INT_BASE + IntPar::language as usize,
    );
    primitive(
        b"lefthyphenmin",
        Cmd::AssignInt,
        INT_BASE + IntPar::left_hyphen_min as usize,
    );
    primitive(
        b"righthyphenmin",
        Cmd::AssignInt,
        INT_BASE + IntPar::right_hyphen_min as usize,
    );
    primitive(
        b"holdinginserts",
        Cmd::AssignInt,
        INT_BASE + IntPar::holding_inserts as usize,
    );
    primitive(
        b"errorcontextlines",
        Cmd::AssignInt,
        INT_BASE + IntPar::error_context_lines as usize,
    );

    primitive(
        b"XeTeXlinebreakpenalty",
        Cmd::AssignInt,
        INT_BASE + IntPar::xetex_linebreak_penalty as usize,
    );
    primitive(
        b"XeTeXprotrudechars",
        Cmd::AssignInt,
        INT_BASE + IntPar::xetex_protrude_chars as usize,
    );

    primitive(
        b"parindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::par_indent as usize,
    );
    primitive(
        b"mathsurround",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::math_surround as usize,
    );
    primitive(
        b"lineskiplimit",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::line_skip_limit as usize,
    );
    primitive(
        b"hsize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hsize as usize,
    );
    primitive(
        b"vsize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::vsize as usize,
    );
    primitive(
        b"maxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::max_depth as usize,
    );
    primitive(
        b"splitmaxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::split_max_depth as usize,
    );
    primitive(
        b"boxmaxdepth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::box_max_depth as usize,
    );
    primitive(
        b"hfuzz",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hfuzz as usize,
    );
    primitive(
        b"vfuzz",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::vfuzz as usize,
    );
    primitive(
        b"delimitershortfall",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::delimiter_shortfall as usize,
    );
    primitive(
        b"nulldelimiterspace",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::null_delimiter_space as usize,
    );
    primitive(
        b"scriptspace",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::script_space as usize,
    );
    primitive(
        b"predisplaysize",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pre_display_size as usize,
    );
    primitive(
        b"displaywidth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::display_width as usize,
    );
    primitive(
        b"displayindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::display_indent as usize,
    );
    primitive(
        b"overfullrule",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::overfull_rule as usize,
    );
    primitive(
        b"hangindent",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::hang_indent as usize,
    );
    primitive(
        b"hoffset",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::h_offset as usize,
    );
    primitive(
        b"voffset",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::v_offset as usize,
    );
    primitive(
        b"emergencystretch",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::emergency_stretch as usize,
    );
    primitive(
        b"pdfpagewidth",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pdf_page_width as usize,
    );
    primitive(
        b"pdfpageheight",
        Cmd::AssignDimen,
        DIMEN_BASE + DimenPar::pdf_page_height as usize,
    );

    primitive(b" ", Cmd::ExSpace, 0);
    primitive(b"/", Cmd::ItalCorr, 0);
    primitive(b"accent", Cmd::Accent, 0);
    primitive(b"advance", Cmd::Advance, 0);
    primitive(b"afterassignment", Cmd::AfterAssignment, 0);
    primitive(b"aftergroup", Cmd::AfterGroup, 0);
    primitive(b"begingroup", Cmd::BeginGroup, 0);
    primitive(b"char", Cmd::CharNum, 0);
    primitive(b"csname", Cmd::CSName, 0);
    primitive(b"delimiter", Cmd::DelimNum, 0);
    primitive(b"XeTeXdelimiter", Cmd::DelimNum, 1);
    primitive(b"Udelimiter", Cmd::DelimNum, 1);
    primitive(b"divide", Cmd::Divide, 0);
    primitive(b"endcsname", Cmd::EndCSName, 0);
    primitive(b"endgroup", Cmd::EndGroup, 0);
    (*hash.offset(FROZEN_END_GROUP as isize)).s1 = maketexstring(b"endgroup");
    EQTB[FROZEN_END_GROUP] = EQTB[cur_val as usize];
    primitive(b"expandafter", Cmd::ExpandAfter, 0);
    primitive(b"font", Cmd::DefFont, 0);
    primitive(b"fontdimen", Cmd::AssignFontDimen, 0);
    primitive(b"halign", Cmd::HAlign, 0);
    primitive(b"hrule", Cmd::HRule, 0);
    primitive(b"ignorespaces", Cmd::IgnoreSpaces, 0);
    primitive(b"insert", Cmd::Insert, 0);
    primitive(b"mark", Cmd::Mark, 0);
    primitive(b"mathaccent", Cmd::MathAccent, 0);
    primitive(b"XeTeXmathaccent", Cmd::MathAccent, 1);
    primitive(b"Umathaccent", Cmd::MathAccent, 1);
    primitive(b"mathchar", Cmd::MathCharNum, 0);
    primitive(b"XeTeXmathcharnum", Cmd::MathCharNum, 1);
    primitive(b"Umathcharnum", Cmd::MathCharNum, 1);
    primitive(b"XeTeXmathchar", Cmd::MathCharNum, 2);
    primitive(b"Umathchar", Cmd::MathCharNum, 2);
    primitive(b"mathchoice", Cmd::MathChoice, 0);
    primitive(b"multiply", Cmd::Multiply, 0);
    primitive(b"noalign", Cmd::NoAlign, 0);
    primitive(b"noboundary", Cmd::NoBoundary, 0);
    primitive(b"noexpand", Cmd::NoExpand, 0);
    primitive(b"primitive", Cmd::NoExpand, 1);
    primitive(b"nonscript", Cmd::NonScript, 0);
    primitive(b"omit", Cmd::Omit, 0);
    primitive(
        b"parshape",
        Cmd::SetShape,
        LOCAL_BASE as i32 + Local::par_shape as i32,
    );
    primitive(b"penalty", Cmd::BreakPenalty, 0);
    primitive(b"prevgraf", Cmd::SetPrevGraf, 0);
    primitive(b"radical", Cmd::Radical, 0);
    primitive(b"XeTeXradical", Cmd::Radical, 1);
    primitive(b"Uradical", Cmd::Radical, 1);
    primitive(b"read", Cmd::ReadToCS, 0);
    primitive(b"relax", Cmd::Relax, TOO_BIG_USV);
    (*hash.offset(FROZEN_RELAX as isize)).s1 = maketexstring(b"relax");
    EQTB[FROZEN_RELAX] = EQTB[cur_val as usize];
    primitive(b"setbox", Cmd::SetBox, 0);
    primitive(b"the", Cmd::The, 0);
    primitive(b"toks", Cmd::ToksRegister, 0);
    primitive(b"vadjust", Cmd::VAdjust, 0);
    primitive(b"valign", Cmd::VAlign, 0);
    primitive(b"vcenter", Cmd::VCenter, 0);
    primitive(b"vrule", Cmd::VRule, 0);
    primitive(b"par", PAR_END, TOO_BIG_USV);
    par_loc = cur_val;
    par_token = 0x1ffffffi32 + par_loc;

    primitive(b"input", Cmd::Input, 0);
    primitive(b"endinput", Cmd::Input, 1);

    primitive(b"topmark", Cmd::TopBotMark, TOP_MARK_CODE);
    primitive(b"firstmark", Cmd::TopBotMark, FIRST_MARK_CODE);
    primitive(b"botmark", Cmd::TopBotMark, BOT_MARK_CODE);
    primitive(b"splitfirstmark", Cmd::TopBotMark, SPLIT_FIRST_MARK_CODE);
    primitive(b"splitbotmark", Cmd::TopBotMark, SPLIT_BOT_MARK_CODE);

    primitive(b"count", Cmd::Register, 0);
    primitive(b"dimen", Cmd::Register, 1);
    primitive(b"skip", Cmd::Register, 2);
    primitive(b"muskip", Cmd::Register, 3);

    primitive(b"spacefactor", Cmd::SetAux, ListMode::HMode as i32);
    primitive(b"prevdepth", Cmd::SetAux, ListMode::VMode as i32);

    primitive(b"deadcycles", Cmd::SetPageInt, 0);
    primitive(b"insertpenalties", Cmd::SetPageInt, 1);

    primitive(b"wd", Cmd::SetBoxDimen, WIDTH_OFFSET);
    primitive(b"ht", Cmd::SetBoxDimen, HEIGHT_OFFSET);
    primitive(b"dp", Cmd::SetBoxDimen, DEPTH_OFFSET);

    primitive(b"lastpenalty", Cmd::LastItem, INT_VAL as i32);
    primitive(b"lastkern", Cmd::LastItem, DIMEN_VAL as i32);
    primitive(b"lastskip", Cmd::LastItem, GLUE_VAL as i32);
    primitive(b"inputlineno", Cmd::LastItem, INPUT_LINE_NO_CODE);
    primitive(b"badness", Cmd::LastItem, BADNESS_CODE);

    primitive(b"number", Cmd::Convert, NUMBER_CODE);
    primitive(b"romannumeral", Cmd::Convert, ROMAN_NUMERAL_CODE);
    primitive(b"string", Cmd::Convert, STRING_CODE);
    primitive(b"meaning", Cmd::Convert, MEANING_CODE);
    primitive(b"fontname", Cmd::Convert, FONT_NAME_CODE);
    primitive(b"jobname", Cmd::Convert, JOB_NAME_CODE);
    primitive(b"leftmarginkern", Cmd::Convert, LEFT_MARGIN_KERN_CODE);
    primitive(b"rightmarginkern", Cmd::Convert, RIGHT_MARGIN_KERN_CODE);
    primitive(b"Uchar", Cmd::Convert, XETEX_UCHAR_CODE);
    primitive(b"Ucharcat", Cmd::Convert, XETEX_UCHARCAT_CODE);

    primitive(b"if", Cmd::IfTest, IF_CHAR_CODE as i32);
    primitive(b"ifcat", Cmd::IfTest, IF_CAT_CODE as i32);
    primitive(b"ifnum", Cmd::IfTest, IF_INT_CODE as i32);
    primitive(b"ifdim", Cmd::IfTest, IF_DIM_CODE as i32);
    primitive(b"ifodd", Cmd::IfTest, IF_ODD_CODE as i32);
    primitive(b"ifvmode", Cmd::IfTest, IF_VMODE_CODE as i32);
    primitive(b"ifhmode", Cmd::IfTest, IF_HMODE_CODE as i32);
    primitive(b"ifmmode", Cmd::IfTest, IF_MMODE_CODE as i32);
    primitive(b"ifinner", Cmd::IfTest, IF_INNER_CODE as i32);
    primitive(b"ifvoid", Cmd::IfTest, IF_VOID_CODE as i32);
    primitive(b"ifhbox", Cmd::IfTest, IF_HBOX_CODE as i32);
    primitive(b"ifvbox", Cmd::IfTest, IF_VBOX_CODE as i32);
    primitive(b"ifx", Cmd::IfTest, IFX_CODE as i32);
    primitive(b"ifeof", Cmd::IfTest, IF_EOF_CODE as i32);
    primitive(b"iftrue", Cmd::IfTest, IF_TRUE_CODE as i32);
    primitive(b"iffalse", Cmd::IfTest, IF_FALSE_CODE as i32);
    primitive(b"ifcase", Cmd::IfTest, IF_CASE_CODE as i32);
    primitive(b"ifprimitive", Cmd::IfTest, IF_PRIMITIVE_CODE as i32);

    primitive(b"fi", Cmd::FiOrElse, FI_CODE as i32);
    (*hash.offset(FROZEN_FI as isize)).s1 = maketexstring(b"fi");
    EQTB[FROZEN_FI] = EQTB[cur_val as usize];
    primitive(b"or", Cmd::FiOrElse, OR_CODE as i32);
    primitive(b"else", Cmd::FiOrElse, ELSE_CODE as i32);

    primitive(b"nullfont", Cmd::SetFont, FONT_BASE);
    (*hash.offset(FROZEN_NULL_FONT as isize)).s1 = maketexstring(b"nullfont");
    EQTB[FROZEN_NULL_FONT] = EQTB[cur_val as usize];

    primitive(b"span", Cmd::TabMark, SPAN_CODE);
    primitive(b"cr", Cmd::CarRet, CR_CODE);
    (*hash.offset(FROZEN_CR as isize)).s1 = maketexstring(b"cr");
    EQTB[FROZEN_CR] = EQTB[cur_val as usize];
    primitive(b"crcr", Cmd::CarRet, CR_CR_CODE);

    (*hash.offset(FROZEN_END_TEMPLATE as isize)).s1 = maketexstring(b"endtemplate");
    (*hash.offset(FROZEN_ENDV as isize)).s1 = maketexstring(b"endtemplate");
    EQTB[FROZEN_ENDV].cmd = Cmd::EndV as u16;
    EQTB[FROZEN_ENDV].val = NULL_LIST as i32;
    EQTB[FROZEN_ENDV].lvl = LEVEL_ONE;
    EQTB[FROZEN_END_TEMPLATE] = EQTB[FROZEN_ENDV];
    EQTB[FROZEN_END_TEMPLATE].cmd = Cmd::EndTemplate as u16;

    primitive(b"pagegoal", Cmd::SetPageDimen, 0);
    primitive(b"pagetotal", Cmd::SetPageDimen, 1);
    primitive(b"pagestretch", Cmd::SetPageDimen, 2);
    primitive(b"pagefilstretch", Cmd::SetPageDimen, 3);
    primitive(b"pagefillstretch", Cmd::SetPageDimen, 4);
    primitive(b"pagefilllstretch", Cmd::SetPageDimen, 5);
    primitive(b"pageshrink", Cmd::SetPageDimen, 6);
    primitive(b"pagedepth", Cmd::SetPageDimen, 7);

    primitive(b"end", STOP, 0);
    primitive(b"dump", STOP, 1);

    primitive(b"hskip", Cmd::HSkip, SKIP_CODE);
    primitive(b"hfil", Cmd::HSkip, FIL_CODE);
    primitive(b"hfill", Cmd::HSkip, FILL_CODE);
    primitive(b"hss", Cmd::HSkip, SS_CODE);
    primitive(b"hfilneg", Cmd::HSkip, FIL_NEG_CODE);
    primitive(b"vskip", Cmd::VSkip, SKIP_CODE);
    primitive(b"vfil", Cmd::VSkip, FIL_CODE);
    primitive(b"vfill", Cmd::VSkip, FILL_CODE);
    primitive(b"vss", Cmd::VSkip, SS_CODE);
    primitive(b"vfilneg", Cmd::VSkip, FIL_NEG_CODE);
    primitive(b"mskip", Cmd::MSkip, MSKIP_CODE);

    primitive(b"kern", Cmd::Kern, KernNST::Explicit as i32);
    primitive(b"mkern", Cmd::MKern, MU_GLUE as i32);
    primitive(b"moveleft", Cmd::HMove, 1);
    primitive(b"moveright", Cmd::HMove, 0);
    primitive(b"raise", Cmd::VMove, 1);
    primitive(b"lower", Cmd::VMove, 0);

    primitive(b"box", Cmd::MakeBox, BOX_CODE);
    primitive(b"copy", Cmd::MakeBox, COPY_CODE);
    primitive(b"lastbox", Cmd::MakeBox, LAST_BOX_CODE);
    primitive(b"vsplit", Cmd::MakeBox, VSPLIT_CODE);
    primitive(b"vtop", Cmd::MakeBox, VTOP_CODE);
    primitive(b"vbox", Cmd::MakeBox, VTOP_CODE + 1);
    primitive(b"hbox", Cmd::MakeBox, VTOP_CODE + 104);

    primitive(b"shipout", Cmd::LeaderShip, A_LEADERS as i32 - 1);
    primitive(b"leaders", Cmd::LeaderShip, A_LEADERS as i32);
    primitive(b"cleaders", Cmd::LeaderShip, C_LEADERS as i32);
    primitive(b"xleaders", Cmd::LeaderShip, X_LEADERS as i32);

    primitive(b"indent", Cmd::StartPar, 1);
    primitive(b"noindent", Cmd::StartPar, 0);
    primitive(b"unpenalty", Cmd::RemoveItem, TextNode::Penalty as i32);
    primitive(b"unkern", Cmd::RemoveItem, TextNode::Kern as i32);
    primitive(b"unskip", Cmd::RemoveItem, TextNode::Glue as i32);
    primitive(b"unhbox", Cmd::UnHBox, BOX_CODE);
    primitive(b"unhcopy", Cmd::UnHBox, COPY_CODE);
    primitive(b"unvbox", Cmd::UnVBox, BOX_CODE);
    primitive(b"unvcopy", Cmd::UnVBox, COPY_CODE);

    primitive(b"-", Cmd::Discretionary, 1);
    primitive(b"discretionary", Cmd::Discretionary, 0);

    primitive(b"eqno", Cmd::EqNo, 0);
    primitive(b"leqno", Cmd::EqNo, 1);

    primitive(b"mathord", Cmd::MathComp, MathNode::Ord as i32);
    primitive(b"mathop", Cmd::MathComp, MathNode::Op as i32);
    primitive(b"mathbin", Cmd::MathComp, MathNode::Bin as i32);
    primitive(b"mathrel", Cmd::MathComp, MathNode::Rel as i32);
    primitive(b"mathopen", Cmd::MathComp, MathNode::Open as i32);
    primitive(b"mathclose", Cmd::MathComp, MathNode::Close as i32);
    primitive(b"mathpunct", Cmd::MathComp, MathNode::Punct as i32);
    primitive(b"mathinner", Cmd::MathComp, MathNode::Inner as i32);
    primitive(b"underline", Cmd::MathComp, MathNode::Under as i32);
    primitive(b"overline", Cmd::MathComp, MathNode::Over as i32);

    primitive(b"displaylimits", Cmd::LimitSwitch, Limit::Normal as i32);
    primitive(b"limits", Cmd::LimitSwitch, Limit::Limits as i32);
    primitive(b"nolimits", Cmd::LimitSwitch, Limit::NoLimits as i32);

    primitive(b"displaystyle", Cmd::MathStyle, DISPLAY_STYLE);
    primitive(b"textstyle", Cmd::MathStyle, TEXT_STYLE);
    primitive(b"scriptstyle", Cmd::MathStyle, SCRIPT_STYLE);
    primitive(b"scriptscriptstyle", Cmd::MathStyle, SCRIPT_SCRIPT_STYLE);

    primitive(b"above", Cmd::Above, ABOVE_CODE);
    primitive(b"over", Cmd::Above, OVER_CODE);
    primitive(b"atop", Cmd::Above, ATOP_CODE);
    primitive(b"abovewithdelims", Cmd::Above, DELIMITED_CODE + 0);
    primitive(b"overwithdelims", Cmd::Above, DELIMITED_CODE + 1);
    primitive(b"atopwithdelims", Cmd::Above, DELIMITED_CODE + 2);

    primitive(b"left", Cmd::LeftRight, MathNode::Left as i32);
    primitive(b"right", Cmd::LeftRight, MathNode::Right as i32);
    (*hash.offset(FROZEN_RIGHT as isize)).s1 = maketexstring(b"right");
    EQTB[FROZEN_RIGHT] = EQTB[cur_val as usize];

    primitive(b"long", Cmd::Prefix, 1);
    primitive(b"outer", Cmd::Prefix, 2);
    primitive(b"global", Cmd::Prefix, 4);
    primitive(b"def", Cmd::Def, 0);
    primitive(b"gdef", Cmd::Def, 1);
    primitive(b"edef", Cmd::Def, 2);
    primitive(b"xdef", Cmd::Def, 3);
    primitive(b"let", Cmd::Let, NORMAL as i32);
    primitive(b"futurelet", Cmd::Let, NORMAL as i32 + 1);

    primitive(b"chardef", Cmd::ShorthandDef, CHAR_DEF_CODE);
    primitive(b"mathchardef", Cmd::ShorthandDef, MATH_CHAR_DEF_CODE);
    primitive(
        b"XeTeXmathcharnumdef",
        Cmd::ShorthandDef,
        XETEX_MATH_CHAR_NUM_DEF_CODE,
    );
    primitive(
        b"Umathcharnumdef",
        Cmd::ShorthandDef,
        XETEX_MATH_CHAR_NUM_DEF_CODE,
    );
    primitive(
        b"XeTeXmathchardef",
        Cmd::ShorthandDef,
        XETEX_MATH_CHAR_DEF_CODE,
    );
    primitive(b"Umathchardef", Cmd::ShorthandDef, XETEX_MATH_CHAR_DEF_CODE);
    primitive(b"countdef", Cmd::ShorthandDef, COUNT_DEF_CODE);
    primitive(b"dimendef", Cmd::ShorthandDef, DIMEN_DEF_CODE);
    primitive(b"skipdef", Cmd::ShorthandDef, SKIP_DEF_CODE);
    primitive(b"muskipdef", Cmd::ShorthandDef, MU_SKIP_DEF_CODE);
    primitive(b"toksdef", Cmd::ShorthandDef, TOKS_DEF_CODE);

    primitive(b"catcode", Cmd::DefCode, CAT_CODE_BASE as i32);
    primitive(b"mathcode", Cmd::DefCode, MATH_CODE_BASE as i32);
    primitive(
        b"XeTeXmathcodenum",
        Cmd::XetexDefCode,
        MATH_CODE_BASE as i32,
    );
    primitive(b"Umathcodenum", Cmd::XetexDefCode, MATH_CODE_BASE as i32);
    primitive(
        b"XeTeXmathcode",
        Cmd::XetexDefCode,
        MATH_CODE_BASE as i32 + 1,
    );
    primitive(b"Umathcode", Cmd::XetexDefCode, MATH_CODE_BASE as i32 + 1);
    primitive(b"lccode", Cmd::DefCode, LC_CODE_BASE as i32);
    primitive(b"uccode", Cmd::DefCode, UC_CODE_BASE as i32);
    primitive(b"sfcode", Cmd::DefCode, SF_CODE_BASE as i32);
    primitive(b"XeTeXcharclass", Cmd::XetexDefCode, SF_CODE_BASE as i32);
    primitive(b"delcode", Cmd::DefCode, DEL_CODE_BASE);
    primitive(b"XeTeXdelcodenum", Cmd::XetexDefCode, DEL_CODE_BASE);
    primitive(b"Udelcodenum", Cmd::XetexDefCode, DEL_CODE_BASE);
    primitive(b"XeTeXdelcode", Cmd::XetexDefCode, DEL_CODE_BASE + 1);
    primitive(b"Udelcode", Cmd::XetexDefCode, DEL_CODE_BASE + 1);

    primitive(
        b"textfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + TEXT_SIZE as i32,
    );
    primitive(
        b"scriptfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + SCRIPT_SIZE as i32,
    );
    primitive(
        b"scriptscriptfont",
        Cmd::DefFamily,
        MATH_FONT_BASE as i32 + SCRIPT_SCRIPT_SIZE as i32,
    );

    primitive(b"hyphenation", Cmd::HyphData, 0);
    primitive(b"patterns", Cmd::HyphData, 1);

    primitive(b"hyphenchar", Cmd::AssignFontInt, 0);
    primitive(b"skewchar", Cmd::AssignFontInt, 1);
    primitive(b"lpcode", Cmd::AssignFontInt, 2);
    primitive(b"rpcode", Cmd::AssignFontInt, 3);

    primitive(b"batchmode", Cmd::SetInteraction, BATCH_MODE);
    primitive(b"nonstopmode", Cmd::SetInteraction, NONSTOP_MODE);
    primitive(b"scrollmode", Cmd::SetInteraction, SCROLL_MODE);
    primitive(
        b"errorstopmode",
        Cmd::SetInteraction,
        ERROR_STOP_MODE as i32,
    );

    primitive(b"openin", Cmd::InStream, 1);
    primitive(b"closein", Cmd::InStream, 0);
    primitive(b"message", Cmd::Message, 0);
    primitive(b"errmessage", Cmd::Message, 1);
    primitive(b"lowercase", Cmd::CaseShift, LC_CODE_BASE as i32);
    primitive(b"uppercase", Cmd::CaseShift, UC_CODE_BASE as i32);

    primitive(b"show", Cmd::XRay, SHOW_CODE);
    primitive(b"showbox", Cmd::XRay, SHOW_BOX_CODE);
    primitive(b"showthe", Cmd::XRay, SHOW_THE_CODE);
    primitive(b"showlists", Cmd::XRay, SHOW_LISTS);

    primitive(b"openout", Cmd::Extension, WhatsItNST::Open as i32);
    primitive(b"write", Cmd::Extension, WhatsItNST::Write as i32);
    write_loc = cur_val;
    primitive(b"closeout", Cmd::Extension, WhatsItNST::Close as i32);
    primitive(b"special", Cmd::Extension, WhatsItNST::Special as i32);
    (*hash.offset(FROZEN_SPECIAL as isize)).s1 = maketexstring(b"special");
    EQTB[FROZEN_SPECIAL] = EQTB[cur_val as usize];
    primitive(b"immediate", Cmd::Extension, IMMEDIATE_CODE as i32);
    primitive(b"setlanguage", Cmd::Extension, SET_LANGUAGE_CODE as i32);

    primitive(
        b"synctex",
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
pub(crate) unsafe fn tt_run_engine(
    mut dump_name: *const i8,
    mut input_file_name: *const i8,
) -> TTHistory {
    /* Miscellaneous initializations that were mostly originally done in the
     * main() driver routines. */
    /* Get our stdout handle */
    rust_stdout = ttstub_output_open_stdout();
    let len = strlen(dump_name);
    TEX_format_default = xmalloc(len.wrapping_add(1) as _) as *mut i8;
    strcpy(TEX_format_default, dump_name);
    format_default_length = len as i32;
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
    INPUT_FILE = vec![0 as *mut UFILE; MAX_IN_OPEN + 1];
    LINE_STACK = vec![0; MAX_IN_OPEN + 1];
    EOF_SEEN = vec![false; MAX_IN_OPEN + 1];
    GRP_STACK = vec![0; MAX_IN_OPEN + 1];
    IF_STACK = vec![0; MAX_IN_OPEN + 1];
    SOURCE_FILENAME_STACK = vec![0; MAX_IN_OPEN + 1];
    FULL_SOURCE_FILENAME_STACK = vec![0; MAX_IN_OPEN + 1];
    PARAM_STACK = vec![0; PARAM_SIZE + 1];
    HYPH_WORD = vec![0; HYPH_SIZE + 1];
    HYPH_LIST = vec![0; HYPH_SIZE + 1];
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
    if format_default_length > std::i32::MAX {
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
    initialize_math_variables();
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
        output_file_extension = b".spx\x00" as *const u8 as *const i8
    } else {
        output_file_extension = b".xdv\x00" as *const u8 as *const i8
    }

    INPUT_PTR = 0;
    MAX_IN_STACK = 0;
    SOURCE_FILENAME_STACK[0] = 0;
    FULL_SOURCE_FILENAME_STACK[0] = 0;
    IN_OPEN = 0;
    open_parens = 0i32;
    max_buf_stack = 0i32;
    GRP_STACK[0] = 0;
    IF_STACK[0] = TEX_NULL;
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
    warning_index = TEX_NULL;
    first = 1;
    cur_input.state = InputState::NewLine;
    cur_input.start = 1;
    cur_input.index = Btl::Parameter;
    line = 0;
    cur_input.name = 0;
    force_eof = false;
    align_state = 1000000;

    init_io();

    if in_initex_mode {
        no_new_control_sequence = false;

        primitive(b"XeTeXpicfile", Cmd::Extension, PIC_FILE_CODE as i32);
        primitive(b"XeTeXpdffile", Cmd::Extension, PDF_FILE_CODE as i32);
        primitive(b"XeTeXglyph", Cmd::Extension, GLYPH_CODE as i32);
        primitive(
            b"XeTeXlinebreaklocale",
            Cmd::Extension,
            XETEX_LINEBREAK_LOCALE_EXTENSION_CODE as i32,
        );
        primitive(
            b"pdfsavepos",
            Cmd::Extension,
            PDFTEX_FIRST_EXTENSION_CODE as i32 + 0,
        );

        primitive(b"lastnodetype", Cmd::LastItem, LAST_NODE_TYPE_CODE as i32);
        primitive(b"eTeXversion", Cmd::LastItem, ETEX_VERSION_CODE);

        primitive(b"eTeXrevision", Cmd::Convert, ETEX_REVISION_CODE);

        primitive(b"XeTeXversion", Cmd::LastItem, XETEX_VERSION_CODE);

        primitive(b"XeTeXrevision", Cmd::Convert, XETEX_REVISION_CODE);

        primitive(b"XeTeXcountglyphs", Cmd::LastItem, XETEX_COUNT_GLYPHS_CODE);
        primitive(
            b"XeTeXcountvariations",
            Cmd::LastItem,
            XETEX_COUNT_VARIATIONS_CODE,
        );
        primitive(b"XeTeXvariation", Cmd::LastItem, XETEX_VARIATION_CODE);
        primitive(
            b"XeTeXfindvariationbyname",
            Cmd::LastItem,
            XETEX_FIND_VARIATION_BY_NAME_CODE,
        );
        primitive(
            b"XeTeXvariationmin",
            Cmd::LastItem,
            XETEX_VARIATION_MIN_CODE,
        );
        primitive(
            b"XeTeXvariationmax",
            Cmd::LastItem,
            XETEX_VARIATION_MAX_CODE,
        );
        primitive(
            b"XeTeXvariationdefault",
            Cmd::LastItem,
            XETEX_VARIATION_DEFAULT_CODE,
        );
        primitive(
            b"XeTeXcountfeatures",
            Cmd::LastItem,
            XETEX_COUNT_FEATURES_CODE,
        );
        primitive(b"XeTeXfeaturecode", Cmd::LastItem, XETEX_FEATURE_CODE_CODE);
        primitive(
            b"XeTeXfindfeaturebyname",
            Cmd::LastItem,
            XETEX_FIND_FEATURE_BY_NAME_CODE,
        );
        primitive(
            b"XeTeXisexclusivefeature",
            Cmd::LastItem,
            XETEX_IS_EXCLUSIVE_FEATURE_CODE,
        );
        primitive(
            b"XeTeXcountselectors",
            Cmd::LastItem,
            XETEX_COUNT_SELECTORS_CODE,
        );
        primitive(
            b"XeTeXselectorcode",
            Cmd::LastItem,
            XETEX_SELECTOR_CODE_CODE,
        );
        primitive(
            b"XeTeXfindselectorbyname",
            Cmd::LastItem,
            XETEX_FIND_SELECTOR_BY_NAME_CODE,
        );
        primitive(
            b"XeTeXisdefaultselector",
            Cmd::LastItem,
            XETEX_IS_DEFAULT_SELECTOR_CODE,
        );

        primitive(
            b"XeTeXvariationname",
            Cmd::Convert,
            XETEX_VARIATION_NAME_CODE,
        );
        primitive(b"XeTeXfeaturename", Cmd::Convert, XETEX_FEATURE_NAME_CODE);
        primitive(b"XeTeXselectorname", Cmd::Convert, XETEX_SELECTOR_NAME_CODE);

        primitive(
            b"XeTeXOTcountscripts",
            Cmd::LastItem,
            XETEX_OT_COUNT_SCRIPTS_CODE,
        );
        primitive(
            b"XeTeXOTcountlanguages",
            Cmd::LastItem,
            XETEX_OT_COUNT_LANGUAGES_CODE,
        );
        primitive(
            b"XeTeXOTcountfeatures",
            Cmd::LastItem,
            XETEX_OT_COUNT_FEATURES_CODE,
        );
        primitive(b"XeTeXOTscripttag", Cmd::LastItem, XETEX_OT_SCRIPT_CODE);
        primitive(b"XeTeXOTlanguagetag", Cmd::LastItem, XETEX_OT_LANGUAGE_CODE);
        primitive(b"XeTeXOTfeaturetag", Cmd::LastItem, XETEX_OT_FEATURE_CODE);
        primitive(
            b"XeTeXcharglyph",
            Cmd::LastItem,
            XETEX_MAP_CHAR_TO_GLYPH_CODE,
        );
        primitive(b"XeTeXglyphindex", Cmd::LastItem, XETEX_GLYPH_INDEX_CODE);
        primitive(b"XeTeXglyphbounds", Cmd::LastItem, XETEX_GLYPH_BOUNDS_CODE);

        primitive(b"XeTeXglyphname", Cmd::Convert, XETEX_GLYPH_NAME_CODE);

        primitive(b"XeTeXfonttype", Cmd::LastItem, XETEX_FONT_TYPE_CODE);
        primitive(b"XeTeXfirstfontchar", Cmd::LastItem, XETEX_FIRST_CHAR_CODE);
        primitive(b"XeTeXlastfontchar", Cmd::LastItem, XETEX_LAST_CHAR_CODE);
        primitive(b"pdflastxpos", Cmd::LastItem, PDF_LAST_X_POS_CODE);
        primitive(b"pdflastypos", Cmd::LastItem, PDF_LAST_Y_POS_CODE);

        primitive(b"strcmp", Cmd::Convert, PDF_STRCMP_CODE);
        primitive(b"mdfivesum", Cmd::Convert, PDF_MDFIVE_SUM_CODE);
        primitive(b"pdfmdfivesum", Cmd::Convert, PDF_MDFIVE_SUM_CODE);

        primitive(b"shellescape", Cmd::LastItem, PDF_SHELL_ESCAPE_CODE);
        primitive(
            b"XeTeXpdfpagecount",
            Cmd::LastItem,
            XETEX_PDF_PAGE_COUNT_CODE,
        );

        primitive(
            b"tracingassigns",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_assigns as usize,
        );
        primitive(
            b"tracinggroups",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_groups as usize,
        );
        primitive(
            b"tracingifs",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_ifs as usize,
        );
        primitive(
            b"tracingscantokens",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_scan_tokens as usize,
        );
        primitive(
            b"tracingnesting",
            Cmd::AssignInt,
            INT_BASE + IntPar::tracing_nesting as usize,
        );
        primitive(
            b"predisplaydirection",
            Cmd::AssignInt,
            INT_BASE + IntPar::pre_display_correction as usize,
        );
        primitive(
            b"lastlinefit",
            Cmd::AssignInt,
            INT_BASE + IntPar::last_line_fit as usize,
        );
        primitive(
            b"savingvdiscards",
            Cmd::AssignInt,
            INT_BASE + IntPar::saving_vdiscards as usize,
        );
        primitive(
            b"savinghyphcodes",
            Cmd::AssignInt,
            INT_BASE + IntPar::saving_hyphs as usize,
        );

        primitive(
            b"currentgrouplevel",
            Cmd::LastItem,
            CURRENT_GROUP_LEVEL_CODE,
        );
        primitive(b"currentgrouptype", Cmd::LastItem, CURRENT_GROUP_TYPE_CODE);
        primitive(b"currentiflevel", Cmd::LastItem, CURRENT_IF_LEVEL_CODE);
        primitive(b"currentiftype", Cmd::LastItem, CURRENT_IF_TYPE_CODE);
        primitive(b"currentifbranch", Cmd::LastItem, CURRENT_IF_BRANCH_CODE);
        primitive(b"fontcharwd", Cmd::LastItem, FONT_CHAR_WD_CODE);
        primitive(b"fontcharht", Cmd::LastItem, FONT_CHAR_HT_CODE);
        primitive(b"fontchardp", Cmd::LastItem, FONT_CHAR_DP_CODE);
        primitive(b"fontcharic", Cmd::LastItem, FONT_CHAR_IC_CODE);
        primitive(b"parshapelength", Cmd::LastItem, PAR_SHAPE_LENGTH_CODE);
        primitive(b"parshapeindent", Cmd::LastItem, PAR_SHAPE_INDENT_CODE);
        primitive(b"parshapedimen", Cmd::LastItem, PAR_SHAPE_DIMEN_CODE);

        primitive(b"showgroups", Cmd::XRay, SHOW_GROUPS);
        primitive(b"showtokens", Cmd::XRay, SHOW_TOKENS);

        primitive(b"unexpanded", Cmd::The, 1);
        primitive(b"detokenize", Cmd::The, SHOW_TOKENS);

        primitive(b"showifs", Cmd::XRay, SHOW_IFS);

        primitive(b"interactionmode", Cmd::SetPageInt, 2);

        primitive(b"middle", Cmd::LeftRight, 1);

        primitive(
            b"suppressfontnotfounderror",
            Cmd::AssignInt,
            INT_BASE + IntPar::suppress_fontnotfound_error as usize,
        );

        primitive(
            b"TeXXeTstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::texxet as usize,
        );
        primitive(
            b"XeTeXupwardsmode",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_upwards as usize,
        );
        primitive(
            b"XeTeXuseglyphmetrics",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_use_glyph_metrics as usize,
        );
        primitive(
            b"XeTeXinterchartokenstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_inter_char_tokens as usize,
        );
        primitive(
            b"XeTeXdashbreakstate",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_dash_break as usize,
        );
        primitive(
            b"XeTeXinputnormalization",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_input_normalization as usize,
        );
        primitive(
            b"XeTeXtracingfonts",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_tracing_fonts as usize,
        );
        primitive(
            b"XeTeXinterwordspaceshaping",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_interword_space_shaping as usize,
        );
        primitive(
            b"XeTeXgenerateactualtext",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_generate_actual_text as usize,
        );
        primitive(
            b"XeTeXhyphenatablelength",
            Cmd::AssignInt,
            INT_BASE + IntPar::xetex_hyphenatable_length as usize,
        );
        primitive(
            b"pdfoutput",
            Cmd::AssignInt,
            INT_BASE + IntPar::pdfoutput as usize,
        );

        primitive(
            b"XeTeXinputencoding",
            Cmd::Extension,
            XETEX_INPUT_ENCODING_EXTENSION_CODE as usize,
        );
        primitive(
            b"XeTeXdefaultencoding",
            Cmd::Extension,
            XETEX_DEFAULT_ENCODING_EXTENSION_CODE as usize,
        );

        primitive(b"beginL", Cmd::VAlign, BEGIN_L_CODE);
        primitive(b"endL", Cmd::VAlign, END_L_CODE);
        primitive(b"beginR", Cmd::VAlign, BEGIN_R_CODE);
        primitive(b"endR", Cmd::VAlign, END_R_CODE);

        primitive(b"scantokens", Cmd::Input, 2);
        primitive(b"readline", Cmd::ReadToCS, 1);
        primitive(b"unless", Cmd::ExpandAfter, 1);

        primitive(b"ifdefined", Cmd::IfTest, IF_DEF_CODE as i32);
        primitive(b"ifcsname", Cmd::IfTest, IF_CS_CODE as i32);
        primitive(b"iffontchar", Cmd::IfTest, IF_FONT_CHAR_CODE as i32);
        primitive(b"ifincsname", Cmd::IfTest, IF_IN_CSNAME_CODE as i32);

        primitive(b"protected", Cmd::Prefix, 8);

        primitive(b"numexpr", Cmd::LastItem, ETEX_EXPR + 0);
        primitive(b"dimexpr", Cmd::LastItem, ETEX_EXPR + 1);
        primitive(b"glueexpr", Cmd::LastItem, ETEX_EXPR + 2);
        primitive(b"muexpr", Cmd::LastItem, ETEX_EXPR + 3);
        primitive(b"gluestretchorder", Cmd::LastItem, GLUE_STRETCH_ORDER_CODE);
        primitive(b"glueshrinkorder", Cmd::LastItem, GLUE_SHRINK_ORDER_CODE);
        primitive(b"gluestretch", Cmd::LastItem, GLUE_STRETCH_CODE);
        primitive(b"glueshrink", Cmd::LastItem, GLUE_SHRINK_CODE);
        primitive(b"mutoglue", Cmd::LastItem, MU_TO_GLUE_CODE);
        primitive(b"gluetomu", Cmd::LastItem, GLUE_TO_MU_CODE);

        primitive(b"marks", Cmd::Mark, 5);
        primitive(b"topmarks", Cmd::TopBotMark, TOP_MARK_CODE + 5);
        primitive(b"firstmarks", Cmd::TopBotMark, FIRST_MARK_CODE + 5);
        primitive(b"botmarks", Cmd::TopBotMark, BOT_MARK_CODE + 5);
        primitive(
            b"splitfirstmarks",
            Cmd::TopBotMark,
            SPLIT_FIRST_MARK_CODE + 5,
        );
        primitive(b"splitbotmarks", Cmd::TopBotMark, SPLIT_BOT_MARK_CODE + 5);

        primitive(b"pagediscards", Cmd::UnVBox, LAST_BOX_CODE);
        primitive(b"splitdiscards", Cmd::UnVBox, VSPLIT_CODE);

        primitive(
            b"interlinepenalties",
            Cmd::SetShape,
            INTER_LINE_PENALTIES_LOC as i32,
        );
        primitive(b"clubpenalties", Cmd::SetShape, CLUB_PENALTIES_LOC as i32);
        primitive(b"widowpenalties", Cmd::SetShape, WIDOW_PENALTIES_LOC as i32);
        primitive(
            b"displaywidowpenalties",
            Cmd::SetShape,
            DISPLAY_WIDOW_PENALTIES_LOC as i32,
        );
        max_reg_num = 32767;
        max_reg_help_line = b"A register number must be between 0 and 32767.";
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
        trie_trl = xmalloc_array(trie_size as usize);
        trie_tro = xmalloc_array(trie_size as usize);
        trie_trc = xmalloc_array(trie_size as usize);
        trie_c = xmalloc_array(trie_size as usize);
        trie_o = xmalloc_array(trie_size as usize);
        trie_l = xmalloc_array(trie_size as usize);
        trie_r = xmalloc_array(trie_size as usize);
        trie_hash = xmalloc_array(trie_size as usize);
        trie_taken = xmalloc_array(trie_size as usize);
        *trie_l.offset(0) = 0;
        *trie_c.offset(0) = 0;
        trie_ptr = 0;
        *trie_r.offset(0) = 0;
        hyph_start = 0;
        FONT_MAPPING = vec![0 as *mut libc::c_void; FONT_MAX + 1];
        FONT_LAYOUT_ENGINE = vec![0 as *mut libc::c_void; FONT_MAX + 1];
        FONT_FLAGS = vec![0; FONT_MAX + 1];
        FONT_LETTER_SPACE = vec![0; FONT_MAX + 1];
        FONT_CHECK = vec![b16x4_le_t::default(); FONT_MAX + 1];
        FONT_SIZE = vec![0; FONT_MAX + 1];
        FONT_DSIZE = vec![0; FONT_MAX + 1];
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
        FONT_NAME[0] = maketexstring(b"nullfont");
        FONT_AREA[0] = EMPTY_STRING;
        HYPHEN_CHAR[0] = '-' as i32;
        SKEW_CHAR[0] = -1;
        BCHAR_LABEL[0] = NON_ADDRESS;
        FONT_BCHAR[0] = TOO_BIG_CHAR;
        FONT_FALSE_BCHAR[0] = TOO_BIG_CHAR;
        FONT_BC[0] = 1;
        FONT_EC[0] = 0;
        FONT_SIZE[0] = 0;
        FONT_DSIZE[0] = 0;
        CHAR_BASE[0] = 0;
        WIDTH_BASE[0] = 0;
        HEIGHT_BASE[0] = 0;
        DEPTH_BASE[0] = 0;
        ITALIC_BASE[0] = 0;
        LIG_KERN_BASE[0] = 0;
        KERN_BASE[0] = 0;
        EXTEN_BASE[0] = 0;
        FONT_GLUE[0] = TEX_NULL;
        FONT_PARAMS[0] = 7;
        FONT_MAPPING[0] = 0 as *mut libc::c_void;
        PARAM_BASE[0] = -1;

        for font_k in 0..=6 {
            FONT_INFO[font_k].b32.s1 = 0;
        }
    }

    font_used = vec![false; FONT_MAX + 1];

    if interaction == BATCH_MODE {
        selector = Selector::NO_PRINT
    } else {
        selector = Selector::TERM_ONLY
    }
    if semantic_pagination_enabled {
        *INTPAR(IntPar::xetex_generate_actual_text) = 1;
    }
    pdf_files_init();
    synctex_init_command();
    start_input(input_file_name);
    history = TTHistory::SPOTLESS;
    main_control();
    final_cleanup();
    close_files_and_terminate();
    pdf_files_close();
    free(TEX_format_default as *mut libc::c_void);
    font_used = Vec::new();
    deinitialize_shipout_variables();

    destroy_font_manager();

    for font_k in 0..FONT_MAX {
        if !(FONT_LAYOUT_ENGINE[font_k]).is_null() {
            release_font_engine(FONT_LAYOUT_ENGINE[font_k], FONT_AREA[font_k]);
            FONT_LAYOUT_ENGINE[font_k] = 0 as *mut libc::c_void
        }
    }
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

    trie_trl = mfree(trie_trl as *mut libc::c_void) as *mut trie_pointer;
    trie_tro = mfree(trie_tro as *mut libc::c_void) as *mut trie_pointer;
    trie_trc = mfree(trie_trc as *mut libc::c_void) as *mut u16;
    history
}

trait AsU8Slice {
    unsafe fn as_u8_slice(&self, num: usize) -> &[u8];
    unsafe fn as_u8_slice_mut(&mut self, num: usize) -> &mut [u8];
}
trait ToU8Slice {
    unsafe fn to_u8_slice(&self) -> &[u8];
    unsafe fn to_u8_slice_mut(&mut self) -> &mut [u8];
}
macro_rules! slice {
    ( $( $x:ty ),* ) => {
        $(
            impl AsU8Slice for $x {
                unsafe fn as_u8_slice(&self, num: usize) -> &[u8] {
                    let p = self as *const Self as *const u8;
                    let item_size = std::mem::size_of::<Self>();
                    unsafe { std::slice::from_raw_parts(p, item_size * num) }
                }
                unsafe fn as_u8_slice_mut(&mut self, num: usize) -> &mut [u8] {
                    let p = self as *mut Self as *mut u8;
                    let item_size = std::mem::size_of::<Self>();
                    unsafe { std::slice::from_raw_parts_mut(p, item_size * num) }
                }
            }
            impl ToU8Slice for [$x] {
                unsafe fn to_u8_slice(&self) -> &[u8] {
                    let p = self.as_ptr() as *const u8;
                    let item_size = std::mem::size_of::<$x>();
                    unsafe { std::slice::from_raw_parts(p, item_size * self.len()) }
                }
                unsafe fn to_u8_slice_mut(&mut self) -> &mut [u8] {
                    let p = self.as_mut_ptr() as *mut u8;
                    let item_size = std::mem::size_of::<$x>();
                    unsafe { std::slice::from_raw_parts_mut(p, item_size * self.len()) }
                }
            }
        )*
    };
}

slice!(i32, memory_word, b32x2, b16x4, UTF16_code, i16, EqtbWord);

/* Read and write dump files.  As distributed, these files are
architecture dependent; specifically, BigEndian and LittleEndian
architectures produce different files.  These routines always output
BigEndian files.  This still does not guarantee them to be
architecture-independent, because it is possible to make a format
that dumps a glue ratio, i.e., a floating-point number.  Fortunately,
none of the standard formats do that.  */

// TODO: optimize
trait Dump {
    fn dump<T>(&mut self, p: &[T])
    where
        [T]: ToU8Slice;
    fn dump_one<T>(&mut self, p: T)
    where
        T: Copy,
        [T]: ToU8Slice,
    {
        let slice = unsafe { std::slice::from_raw_parts(&p, 1) };
        self.dump(slice);
    }
}

impl Dump for OutputHandleWrapper {
    fn dump<T>(&mut self, p: &[T])
    where
        [T]: ToU8Slice,
    {
        let nitems = p.len();
        let p = unsafe { p.to_u8_slice() };
        let item_size = std::mem::size_of::<T>();
        let mut v = Vec::with_capacity(item_size * nitems);
        for i in p.chunks(item_size) {
            v.extend(i.iter().rev());
        }

        self.write(&v).expect(&format!(
            "could not write {} {}-byte item(s) to {}",
            nitems,
            item_size,
            unsafe { CStr::from_ptr(name_of_file).to_string_lossy() },
        ));
    }
}

trait UnDump {
    fn undump<T>(&mut self, p: &mut [T])
    where
        [T]: ToU8Slice;
    fn undump_one<T>(&mut self, p: &mut T)
    where
        [T]: ToU8Slice,
    {
        let slice = unsafe { std::slice::from_raw_parts_mut(p, 1) };
        self.undump(slice);
    }
}
impl UnDump for InputHandleWrapper {
    fn undump<T>(&mut self, p: &mut [T])
    where
        [T]: ToU8Slice,
    {
        let nitems = p.len();
        let item_size = std::mem::size_of::<T>();
        let mut v = vec![0; item_size * nitems];
        if self.read_exact(v.as_mut_slice()).is_err() {
            unsafe {
                abort!(
                    "could not undump {} {}-byte item(s) from {}",
                    nitems,
                    item_size,
                    CStr::from_ptr(name_of_file).display()
                );
            }
        }
        for i in v.chunks_mut(item_size) {
            i.reverse();
        }
        let p = unsafe { p.to_u8_slice_mut() };
        p.copy_from_slice(v.as_slice());
    }
}
