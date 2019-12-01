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
use std::io::Write;
use std::ptr;

use super::xetex_texmfmp::get_date_and_time;
use crate::core_memory::{mfree, xcalloc, xcalloc_array, xmalloc, xmalloc_array};
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error, overflow};
use crate::xetex_ext::release_font_engine;
use crate::xetex_layout_interface::{destroy_font_manager, set_cp_code};
use crate::xetex_math::initialize_math_variables;
use crate::xetex_output::{
    print, print_char, print_cstr, print_esc, print_esc_cstr, print_file_line, print_file_name,
    print_int, print_ln, print_nl, print_nl_cstr, print_scaled,
};
use crate::xetex_pagebuilder::initialize_pagebuilder_variables;
use crate::xetex_shipout::{deinitialize_shipout_variables, initialize_shipout_variables};
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
use bridge::{
    ttstub_input_close, ttstub_input_open, ttstub_input_read, ttstub_output_close,
    ttstub_output_open, ttstub_output_open_stdout,
};
use dpx::{pdf_files_close, pdf_files_init};
use libc::{free, memset, strcpy, strlen};

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
const TRIE_OP_SIZE: i32 = 35111;
/*18: */
pub(crate) type UTF16_code = u16;
pub(crate) type UTF8_code = u8;
pub(crate) type UnicodeScalar = i32;
pub(crate) type eight_bits = u8;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
pub(crate) type small_number = i16;
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
#[derive(Copy, Clone)]
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
/* ... or the glyph number, if subtype==GLYPH_NODE */
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
pub(crate) type group_code = u8;
pub(crate) type internal_font_number = i32;
pub(crate) type font_index = i32;
pub(crate) type nine_bits = i32;
/* range: 0 .. 0x1FF */
pub(crate) type trie_pointer = i32;
pub(crate) type trie_opcode = u16;
pub(crate) type hyph_pointer = u16;
pub(crate) type save_pointer = i32;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct list_state_record {
    pub(crate) mode: i16,
    pub(crate) head: i32,
    pub(crate) tail: i32,
    pub(crate) eTeX_aux: i32,
    pub(crate) prev_graf: i32,
    pub(crate) mode_line: i32,
    pub(crate) aux: memory_word,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct input_state_t {
    pub(crate) state: u16,
    pub(crate) index: u16,
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
pub(crate) static mut eqtb: *mut memory_word = ptr::null_mut();
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
pub(crate) static mut buffer: *mut UnicodeScalar = ptr::null_mut();
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
pub(crate) static mut max_strings: i32 = 0;
#[no_mangle]
pub(crate) static mut strings_free: i32 = 0;
#[no_mangle]
pub(crate) static mut string_vacancies: i32 = 0;
#[no_mangle]
pub(crate) static mut pool_size: i32 = 0;
#[no_mangle]
pub(crate) static mut pool_free: i32 = 0;
#[no_mangle]
pub(crate) static mut font_mem_size: i32 = 0;
#[no_mangle]
pub(crate) static mut font_max: i32 = 0;
#[no_mangle]
pub(crate) static mut hyph_size: i32 = 0;
#[no_mangle]
pub(crate) static mut trie_size: i32 = 0;
#[no_mangle]
pub(crate) static mut buf_size: i32 = 0;
#[no_mangle]
pub(crate) static mut stack_size: i32 = 0;
#[no_mangle]
pub(crate) static mut max_in_open: i32 = 0;
#[no_mangle]
pub(crate) static mut param_size: i32 = 0;
#[no_mangle]
pub(crate) static mut nest_size: i32 = 0;
#[no_mangle]
pub(crate) static mut save_size: i32 = 0;
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
pub(crate) static mut str_pool: *mut packed_UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut str_start: *mut pool_pointer = ptr::null_mut();
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
pub(crate) static mut mem: *mut memory_word = ptr::null_mut();
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
pub(crate) static mut nest: *mut list_state_record = ptr::null_mut();
#[no_mangle]
pub(crate) static mut nest_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut max_nest_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_list: list_state_record = list_state_record {
    mode: 0,
    head: 0,
    tail: 0,
    eTeX_aux: 0,
    prev_graf: 0,
    mode_line: 0,
    aux: memory_word {
        b32: b32x2 { s0: 0, s1: 0 },
    },
};
#[no_mangle]
pub(crate) static mut shown_mode: i16 = 0;
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
pub(crate) static mut eqtb_top: i32 = 0;
#[no_mangle]
pub(crate) static mut hash_high: i32 = 0;
#[no_mangle]
pub(crate) static mut no_new_control_sequence: bool = false;
#[no_mangle]
pub(crate) static mut cs_count: i32 = 0;
#[no_mangle]
pub(crate) static mut prim: [b32x2; 501] = [b32x2 { s0: 0, s1: 0 }; 501];
#[no_mangle]
pub(crate) static mut prim_used: i32 = 0;
#[no_mangle]
pub(crate) static mut prim_eqtb: [memory_word; 501] = [memory_word {
    b32: b32x2 { s0: 0, s1: 0 },
}; 501];
#[no_mangle]
pub(crate) static mut save_stack: *mut memory_word = ptr::null_mut();
#[no_mangle]
pub(crate) static mut save_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut max_save_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_level: u16 = 0;
#[no_mangle]
pub(crate) static mut cur_group: group_code = 0;
#[no_mangle]
pub(crate) static mut cur_boundary: i32 = 0;
#[no_mangle]
pub(crate) static mut mag_set: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_cmd: eight_bits = 0;
#[no_mangle]
pub(crate) static mut cur_chr: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_cs: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_tok: i32 = 0;
#[no_mangle]
pub(crate) static mut input_stack: *mut input_state_t = ptr::null_mut();
#[no_mangle]
pub(crate) static mut input_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut max_in_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_input: input_state_t = input_state_t {
    state: 0,
    index: 0,
    start: 0,
    loc: 0,
    limit: 0,
    name: 0,
    synctex_tag: 0,
};
#[no_mangle]
pub(crate) static mut in_open: i32 = 0;
#[no_mangle]
pub(crate) static mut open_parens: i32 = 0;
#[no_mangle]
pub(crate) static mut input_file: *mut *mut UFILE = 0 as *const *mut UFILE as *mut *mut UFILE;
#[no_mangle]
pub(crate) static mut line: i32 = 0;
#[no_mangle]
pub(crate) static mut line_stack: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut source_filename_stack: *mut str_number = ptr::null_mut();
#[no_mangle]
pub(crate) static mut full_source_filename_stack: *mut str_number = ptr::null_mut();
#[no_mangle]
pub(crate) static mut scanner_status: u8 = 0;
#[no_mangle]
pub(crate) static mut warning_index: i32 = 0;
#[no_mangle]
pub(crate) static mut def_ref: i32 = 0;
#[no_mangle]
pub(crate) static mut param_stack: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut param_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut max_param_stack: i32 = 0;
#[no_mangle]
pub(crate) static mut align_state: i32 = 0;
#[no_mangle]
pub(crate) static mut base_ptr: i32 = 0;
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
pub(crate) static mut radix: small_number = 0;
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
pub(crate) static mut cur_if: small_number = 0;
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
pub(crate) static mut font_info: *mut memory_word = ptr::null_mut();
#[no_mangle]
pub(crate) static mut fmem_ptr: font_index = 0;
#[no_mangle]
pub(crate) static mut font_ptr: internal_font_number = 0;
#[no_mangle]
pub(crate) static mut font_check: *mut b16x4 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_size: *mut scaled_t = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_dsize: *mut scaled_t = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_params: *mut font_index = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_name: *mut str_number = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_area: *mut str_number = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_bc: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_ec: *mut UTF16_code = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_glue: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_used: *mut bool = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hyphen_char: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut skew_char: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut bchar_label: *mut font_index = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_bchar: *mut nine_bits = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_false_bchar: *mut nine_bits = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_layout_engine: *mut *mut libc::c_void =
    0 as *const *mut libc::c_void as *mut *mut libc::c_void;
#[no_mangle]
pub(crate) static mut font_mapping: *mut *mut libc::c_void =
    0 as *const *mut libc::c_void as *mut *mut libc::c_void;
#[no_mangle]
pub(crate) static mut font_flags: *mut i8 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut font_letter_space: *mut scaled_t = ptr::null_mut();
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
pub(crate) static mut char_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut width_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut height_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut depth_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut italic_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut lig_kern_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut kern_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut exten_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut param_base: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut null_character: b16x4 = b16x4 {
    s0: 0,
    s1: 0,
    s2: 0,
    s3: 0,
};
#[no_mangle]
pub(crate) static mut total_pages: i32 = 0;
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
pub(crate) static mut hyphen_passed: small_number = 0;
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
pub(crate) static mut hyf_distance: [small_number; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut hyf_num: [small_number; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut hyf_next: [trie_opcode; 35112] = [0; 35112];
#[no_mangle]
pub(crate) static mut op_start: [i32; 256] = [0; 256];
#[no_mangle]
pub(crate) static mut hyph_word: *mut str_number = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hyph_list: *mut i32 = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hyph_link: *mut hyph_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut hyph_count: i32 = 0;
#[no_mangle]
pub(crate) static mut hyph_next: i32 = 0;
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
pub(crate) static mut eof_seen: *mut bool = ptr::null_mut();
#[no_mangle]
pub(crate) static mut LR_ptr: i32 = 0;
#[no_mangle]
pub(crate) static mut LR_problems: i32 = 0;
#[no_mangle]
pub(crate) static mut cur_dir: small_number = 0;
#[no_mangle]
pub(crate) static mut pseudo_files: i32 = 0;
#[no_mangle]
pub(crate) static mut grp_stack: *mut save_pointer = ptr::null_mut();
#[no_mangle]
pub(crate) static mut if_stack: *mut i32 = ptr::null_mut();
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
pub(crate) static mut page_contents: u8 = 0;
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
/* Read and write dump files.  As distributed, these files are
architecture dependent; specifically, BigEndian and LittleEndian
architectures produce different files.  These routines always output
BigEndian files.  This still does not guarantee them to be
architecture-independent, because it is possible to make a format
that dumps a glue ratio, i.e., a floating-point number.  Fortunately,
none of the standard formats do that.  */
/* This macro is always invoked as a statement.  It assumes a variable
`temp'.  */
/* Make the NITEMS items pointed at by P, each of size SIZE, be the
opposite-endianness of whatever they are now.  */
unsafe extern "C" fn swap_items(mut p: *mut i8, mut nitems: size_t, mut size: size_t) {
    let mut temp: i8 = 0;
    match size {
        16 => loop {
            let fresh0 = nitems;
            nitems = nitems.wrapping_sub(1);
            if !(fresh0 != 0) {
                break;
            }
            temp = *p.offset(0);
            *p.offset(0) = *p.offset(15);
            *p.offset(15) = temp;
            temp = *p.offset(1);
            *p.offset(1) = *p.offset(14);
            *p.offset(14) = temp;
            temp = *p.offset(2);
            *p.offset(2) = *p.offset(13);
            *p.offset(13) = temp;
            temp = *p.offset(3);
            *p.offset(3) = *p.offset(12);
            *p.offset(12) = temp;
            temp = *p.offset(4);
            *p.offset(4) = *p.offset(11);
            *p.offset(11) = temp;
            temp = *p.offset(5);
            *p.offset(5) = *p.offset(10);
            *p.offset(10) = temp;
            temp = *p.offset(6);
            *p.offset(6) = *p.offset(9);
            *p.offset(9) = temp;
            temp = *p.offset(7);
            *p.offset(7) = *p.offset(8);
            *p.offset(8) = temp;
            p = p.offset(size as isize)
        },
        8 => loop {
            let fresh1 = nitems;
            nitems = nitems.wrapping_sub(1);
            if !(fresh1 != 0) {
                break;
            }
            temp = *p.offset(0);
            *p.offset(0) = *p.offset(7);
            *p.offset(7) = temp;
            temp = *p.offset(1);
            *p.offset(1) = *p.offset(6);
            *p.offset(6) = temp;
            temp = *p.offset(2);
            *p.offset(2) = *p.offset(5);
            *p.offset(5) = temp;
            temp = *p.offset(3);
            *p.offset(3) = *p.offset(4);
            *p.offset(4) = temp;
            p = p.offset(size as isize)
        },
        4 => loop {
            let fresh2 = nitems;
            nitems = nitems.wrapping_sub(1);
            if !(fresh2 != 0) {
                break;
            }
            temp = *p.offset(0);
            *p.offset(0) = *p.offset(3);
            *p.offset(3) = temp;
            temp = *p.offset(1);
            *p.offset(1) = *p.offset(2);
            *p.offset(2) = temp;
            p = p.offset(size as isize)
        },
        2 => loop {
            let fresh3 = nitems;
            nitems = nitems.wrapping_sub(1);
            if !(fresh3 != 0) {
                break;
            }
            temp = *p.offset(0);
            *p.offset(0) = *p.offset(1);
            *p.offset(1) = temp;
            p = p.offset(size as isize)
        },
        1 => {}
        _ => {
            abort!("can\'t swap a {}-byte item for (un)dumping", size);
        }
    };
}
/* not WORDS_BIGENDIAN */
/* Here we write NITEMS items, each item being ITEM_SIZE bytes long.
The pointer to the stuff to write is P, and we write to the file
OUT_FILE.  */
unsafe extern "C" fn do_dump(
    mut p: *mut i8,
    mut item_size: size_t,
    mut nitems: size_t,
    mut out_file: &mut OutputHandleWrapper,
) {
    swap_items(p, nitems, item_size);
    let mut v = Vec::new();
    for i in 0..(item_size * nitems) as isize {
        v.push(*p.offset(i) as u8);
    }
    out_file.write(&v).expect(&format!(
        "could not write {} {}-byte item(s) to {}",
        nitems,
        item_size,
        CStr::from_ptr(name_of_file).to_string_lossy(),
    ));
    /* Have to restore the old contents of memory, since some of it might
    get used again.  */
    swap_items(p, nitems, item_size);
}
/* Here is the dual of the writing routine.  */
unsafe extern "C" fn do_undump(
    mut p: *mut i8,
    mut item_size: size_t,
    mut nitems: size_t,
    in_file: &mut InputHandleWrapper,
) {
    let mut r: ssize_t = ttstub_input_read(in_file.as_ptr(), p, item_size.wrapping_mul(nitems));
    if r < 0 || r as size_t != item_size.wrapping_mul(nitems) {
        abort!(
            "could not undump {} {}-byte item(s) from {}",
            nitems,
            item_size,
            CStr::from_ptr(name_of_file).display()
        );
    }
    swap_items(p, nitems, item_size);
}

const hash_offset: i32 = 514;

/*:134*/
/*135: */
unsafe extern "C" fn sort_avail() {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut old_rover: i32 = 0;
    p = get_node(0x40000000i32);
    p = (*mem.offset((rover + 1i32) as isize)).b32.s1;
    (*mem.offset((rover + 1i32) as isize)).b32.s1 = 0x3fffffffi32;
    old_rover = rover;
    /*136: */
    while p != old_rover {
        if p < rover {
            q = p;
            p = (*mem.offset((q + 1i32) as isize)).b32.s1;
            (*mem.offset((q + 1i32) as isize)).b32.s1 = rover;
            rover = q
        } else {
            q = rover;
            while (*mem.offset((q + 1i32) as isize)).b32.s1 < p {
                q = (*mem.offset((q + 1i32) as isize)).b32.s1
            }
            r = (*mem.offset((p + 1i32) as isize)).b32.s1;
            (*mem.offset((p + 1i32) as isize)).b32.s1 = (*mem.offset((q + 1i32) as isize)).b32.s1;
            (*mem.offset((q + 1i32) as isize)).b32.s1 = p;
            p = r
        }
    }
    p = rover;
    while (*mem.offset((p + 1i32) as isize)).b32.s1 != 0x3fffffffi32 {
        (*mem.offset(((*mem.offset((p + 1i32) as isize)).b32.s1 + 1i32) as isize))
            .b32
            .s0 = p;
        p = (*mem.offset((p + 1i32) as isize)).b32.s1
    }
    (*mem.offset((p + 1i32) as isize)).b32.s1 = rover;
    (*mem.offset((rover + 1i32) as isize)).b32.s0 = p;
}
/*:271*/
/*276: */
unsafe extern "C" fn primitive(ident: &[u8], mut c: u16, mut o: i32) {
    let mut prim_val: i32 = 0;
    let mut len = ident.len() as i32;
    if len > 1i32 {
        let mut s: str_number = maketexstring(ident);
        if first + len > buf_size + 1i32 {
            overflow(b"buffer size", buf_size);
        }
        for i in 0..len {
            *buffer.offset((first + i) as isize) = ident[i as usize] as UnicodeScalar;
        }
        cur_val = id_lookup(first, len);
        str_ptr -= 1;
        pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
        (*hash.offset(cur_val as isize)).s1 = s;
        prim_val = prim_lookup(s)
    } else {
        cur_val = ident[0] as i32 + (1i32 + (0x10ffffi32 + 1i32));
        prim_val = prim_lookup(ident[0] as str_number)
    }
    (*eqtb.offset(cur_val as isize)).b16.s0 = 1_u16;
    (*eqtb.offset(cur_val as isize)).b16.s1 = c;
    (*eqtb.offset(cur_val as isize)).b32.s1 = o;
    prim_eqtb[prim_val as usize].b16.s0 = 1_u16;
    prim_eqtb[prim_val as usize].b16.s1 = c;
    prim_eqtb[prim_val as usize].b32.s1 = o;
}
/*:925*/
/*977: */
#[no_mangle]
pub(crate) unsafe extern "C" fn new_trie_op(
    mut d: small_number,
    mut n: small_number,
    mut v: trie_opcode,
) -> trie_opcode {
    let mut h: i32 = 0;
    let mut u: trie_opcode = 0;
    let mut l: i32 = 0;
    h = ((n as i32 + 313i32 * d as i32 + 361i32 * v as i32 + 1009i32 * cur_lang as i32).abs()
        as i64
        % (35111 - -35111)
        + -35111) as i32;
    loop {
        l = _trie_op_hash_array[(h as i64 - -35111) as usize];
        if l == 0i32 {
            if trie_op_ptr as i64 == 35111 {
                overflow(b"pattern memory ops", 35111 as i32);
            }
            u = trie_used[cur_lang as usize];
            if u as i64 == 65535 {
                overflow(b"pattern memory ops per language", (65535 - 0) as i32);
            }
            trie_op_ptr += 1;
            u = u.wrapping_add(1);
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
        if h > -(35111 as i32) {
            h -= 1
        } else {
            h = 35111 as i32
        }
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn trie_node(mut p: trie_pointer) -> trie_pointer {
    let mut h: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    h = ((*trie_c.offset(p as isize) as u32
        + 1009 * *trie_o.offset(p as isize) as u32
        + 2718 * *trie_l.offset(p as isize) as u32
        + 3142 * *trie_r.offset(p as isize) as u32)
        % trie_size as u32) as i32;
    loop {
        q = *trie_hash.offset(h as isize);
        if q == 0i32 {
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
        if h > 0i32 {
            h -= 1
        } else {
            h = trie_size
        }
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn compress_trie(mut p: trie_pointer) -> trie_pointer {
    if p == 0i32 {
        0i32
    } else {
        *trie_l.offset(p as isize) = compress_trie(*trie_l.offset(p as isize));
        *trie_r.offset(p as isize) = compress_trie(*trie_r.offset(p as isize));
        trie_node(p)
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn first_fit(mut p: trie_pointer) {
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
                overflow(b"pattern memory", trie_size);
            }
            loop {
                trie_max += 1;
                *trie_taken.offset(trie_max as isize) = false;
                *trie_trl.offset(trie_max as isize) = trie_max + 1i32;
                *trie_tro.offset(trie_max as isize) = trie_max - 1i32;
                if trie_max == h + max_hyph_char {
                    break;
                }
            }
        }
        if !*trie_taken.offset(h as isize) {
            q = *trie_r.offset(p as isize);
            loop {
                if !(q > 0i32) {
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
        if q == 0i32 {
            break;
        }
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn trie_pack(mut p: trie_pointer) {
    let mut q: trie_pointer = 0;
    loop {
        q = *trie_l.offset(p as isize);
        if q > 0i32 && *trie_hash.offset(q as isize) == 0i32 {
            first_fit(q);
            trie_pack(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0i32 {
            break;
        }
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn trie_fix(mut p: trie_pointer) {
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
        if q > 0i32 {
            trie_fix(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0i32 {
            break;
        }
    }
}
unsafe extern "C" fn new_patterns() {
    let mut k: i16 = 0;
    let mut l: i16 = 0;
    let mut digit_sensed: bool = false;
    let mut v: trie_opcode = 0;
    let mut p: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    let mut first_child: bool = false;
    let mut c: UTF16_code = 0;
    if trie_not_ready {
        if INTPAR(INT_PAR__language) <= 0 {
            cur_lang = 0_u8
        } else if INTPAR(INT_PAR__language) > BIGGEST_LANG {
            cur_lang = 0_u8
        } else {
            cur_lang = INTPAR(INT_PAR__language) as _;
        }
        scan_left_brace();
        k = 0_i16;
        hyf[0] = 0_u8;
        digit_sensed = false;
        loop {
            get_x_token();
            match cur_cmd as i32 {
                11 | 12 => {
                    if digit_sensed as i32 != 0 || cur_chr < '0' as i32 || cur_chr > '9' as i32 {
                        if cur_chr == '.' as i32 {
                            cur_chr = 0i32
                        } else {
                            cur_chr = LC_CODE(cur_chr);
                            if cur_chr == 0i32 {
                                if file_line_error_style_p != 0 {
                                    print_file_line();
                                } else {
                                    print_nl_cstr(b"! ");
                                }
                                print_cstr(b"Nonletter");
                                help_ptr = 1_u8;
                                help_line[0] = b"(See Appendix H.)";
                                error();
                            }
                        }
                        if cur_chr > max_hyph_char {
                            max_hyph_char = cur_chr
                        }
                        if (k as i32) < max_hyphenatable_length() {
                            k += 1;
                            hc[k as usize] = cur_chr;
                            hyf[k as usize] = 0_u8;
                            digit_sensed = false
                        }
                    } else if (k as i32) < max_hyphenatable_length() {
                        hyf[k as usize] = (cur_chr - 48i32) as u8;
                        digit_sensed = true
                    }
                }
                10 | 2 => {
                    if k as i32 > 0i32 {
                        /*998:*/
                        if hc[1] == 0i32 {
                            hyf[0] = 0_u8
                        }
                        if hc[k as usize] == 0i32 {
                            hyf[k as usize] = 0_u8
                        }
                        l = k;
                        v = 0i32 as trie_opcode;
                        loop {
                            if hyf[l as usize] as i32 != 0i32 {
                                v = new_trie_op(
                                    (k as i32 - l as i32) as small_number,
                                    hyf[l as usize] as small_number,
                                    v,
                                )
                            }
                            if !(l as i32 > 0i32) {
                                break;
                            }
                            l -= 1
                        }
                        q = 0i32;
                        hc[0] = cur_lang as i32;
                        while l as i32 <= k as i32 {
                            c = hc[l as usize] as UTF16_code;
                            l += 1;
                            p = *trie_l.offset(q as isize);
                            first_child = true;
                            while p > 0i32 && c as i32 > *trie_c.offset(p as isize) as i32 {
                                q = p;
                                p = *trie_r.offset(q as isize);
                                first_child = false
                            }
                            if p == 0i32 || (c as i32) < *trie_c.offset(p as isize) as i32 {
                                /*999:*/
                                if trie_ptr == trie_size {
                                    overflow(b"pattern memory", trie_size);
                                }
                                trie_ptr += 1;
                                *trie_r.offset(trie_ptr as isize) = p;
                                p = trie_ptr;
                                *trie_l.offset(p as isize) = 0i32;
                                if first_child {
                                    *trie_l.offset(q as isize) = p
                                } else {
                                    *trie_r.offset(q as isize) = p
                                }
                                *trie_c.offset(p as isize) = c;
                                *trie_o.offset(p as isize) = 0i32 as trie_opcode
                            }
                            q = p
                        }
                        if *trie_o.offset(q as isize) as i32 != 0i32 {
                            if file_line_error_style_p != 0 {
                                print_file_line();
                            } else {
                                print_nl_cstr(b"! ");
                            }
                            print_cstr(b"Duplicate pattern");
                            help_ptr = 1_u8;
                            help_line[0] = b"(See Appendix H.)";
                            error();
                        }
                        *trie_o.offset(q as isize) = v
                    }
                    if cur_cmd as i32 == 2i32 {
                        break;
                    }
                    k = 0_i16;
                    hyf[0] = 0_u8;
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
                    help_ptr = 1_u8;
                    help_line[0] = b"(See Appendix H.)";
                    error();
                }
            }
        }
        /*:996*/
        if INTPAR(INT_PAR__saving_hyphs) > 0 {
            /*1643:*/
            c = cur_lang as UTF16_code;
            first_child = false;
            p = 0i32;
            loop {
                q = p;
                p = *trie_r.offset(q as isize);
                if p == 0i32 || c as i32 <= *trie_c.offset(p as isize) as i32 {
                    break;
                }
            }
            if p == 0i32 || (c as i32) < *trie_c.offset(p as isize) as i32 {
                /*:1644*/
                /*999:*/
                if trie_ptr == trie_size {
                    overflow(b"pattern memory", trie_size);
                }
                trie_ptr += 1;
                *trie_r.offset(trie_ptr as isize) = p;
                p = trie_ptr;
                *trie_l.offset(p as isize) = 0i32;
                if first_child {
                    *trie_l.offset(q as isize) = p
                } else {
                    *trie_r.offset(q as isize) = p
                }
                *trie_c.offset(p as isize) = c;
                *trie_o.offset(p as isize) = 0i32 as trie_opcode
            }
            q = p;
            p = *trie_l.offset(q as isize);
            first_child = true;
            c = 0i32 as UTF16_code;
            while c as i32 <= 255i32 {
                if LC_CODE(c as _) > 0 || c as i32 == 255i32 && first_child as i32 != 0 {
                    if p == 0i32 {
                        /*999:*/
                        if trie_ptr == trie_size {
                            overflow(b"pattern memory", trie_size);
                            /*:987 */
                        }
                        trie_ptr += 1;
                        *trie_r.offset(trie_ptr as isize) = p;
                        p = trie_ptr;
                        *trie_l.offset(p as isize) = 0i32;
                        if first_child {
                            *trie_l.offset(q as isize) = p
                        } else {
                            *trie_r.offset(q as isize) = p
                        }
                        *trie_c.offset(p as isize) = c;
                        *trie_o.offset(p as isize) = 0i32 as trie_opcode
                    } else {
                        *trie_c.offset(p as isize) = c
                    }
                    *trie_o.offset(p as isize) = LC_CODE(c as _) as _;
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
        (*mem.offset((4999999i32 - 12i32) as isize)).b32.s1 = scan_toks(false, false);
        flush_list(def_ref);
    };
}
#[no_mangle]
pub(crate) unsafe extern "C" fn init_trie() {
    let mut p: trie_pointer = 0;
    let mut j: i32 = 0;
    let mut k: i32 = 0;
    let mut t: i32 = 0;
    let mut r: trie_pointer = 0;
    let mut s: trie_pointer = 0;
    max_hyph_char += 1;
    op_start[0] = -0i32;
    let mut for_end: i32 = 0;
    j = 1i32;
    for_end = 255i32;
    if j <= for_end {
        loop {
            op_start[j as usize] =
                op_start[(j - 1i32) as usize] + trie_used[(j - 1i32) as usize] as i32;
            let fresh4 = j;
            j = j + 1;
            if !(fresh4 < for_end) {
                break;
            }
        }
    }
    let mut for_end_0: i32 = 0;
    j = 1i32;
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
    j = 1i32;
    for_end_1 = trie_op_ptr;
    if j <= for_end_1 {
        loop {
            while _trie_op_hash_array[(j as i64 - -35111) as usize] > j {
                k = _trie_op_hash_array[(j as i64 - -35111) as usize];
                t = hyf_distance[k as usize] as i32;
                hyf_distance[k as usize] = hyf_distance[j as usize];
                hyf_distance[j as usize] = t as small_number;
                t = hyf_num[k as usize] as i32;
                hyf_num[k as usize] = hyf_num[j as usize];
                hyf_num[j as usize] = t as small_number;
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
    p = 0i32;
    for_end_2 = trie_size;
    if p <= for_end_2 {
        loop {
            *trie_hash.offset(p as isize) = 0i32;
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
    p = 0i32;
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
unsafe extern "C" fn new_hyph_exceptions() {
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
    if INTPAR(INT_PAR__language) <= 0 {
        cur_lang = 0_u8
    } else if INTPAR(INT_PAR__language) > BIGGEST_LANG {
        cur_lang = 0_u8
    } else {
        cur_lang = INTPAR(INT_PAR__language) as _;
    }
    if trie_not_ready {
        hyph_index = 0i32
    } else if *trie_trc.offset((hyph_start + cur_lang as i32) as isize) as i32 != cur_lang as i32 {
        hyph_index = 0i32
    } else {
        hyph_index = *trie_trl.offset((hyph_start + cur_lang as i32) as isize)
    }
    /*970:*/
    n = 0_i16;
    p = TEX_NULL;
    's_91: loop {
        get_x_token();
        loop {
            match cur_cmd as i32 {
                11 | 12 | 68 => {
                    if cur_chr == '-' as i32 {
                        /*973:*/
                        if (n as i32) < max_hyphenatable_length() {
                            q = get_avail();
                            (*mem.offset(q as isize)).b32.s1 = p;
                            (*mem.offset(q as isize)).b32.s0 = n as i32;
                            p = q
                        }
                    } else {
                        if hyph_index == 0i32 || cur_chr > 255i32 {
                            hc[0] = LC_CODE(cur_chr) as _;
                        } else if *trie_trc.offset((hyph_index + cur_chr) as isize) as i32
                            != cur_chr
                        {
                            hc[0] = 0i32
                        } else {
                            hc[0] = *trie_tro.offset((hyph_index + cur_chr) as isize)
                        }
                        if hc[0] == 0i32 {
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
                        } else if (n as i32) < max_hyphenatable_length() {
                            n += 1;
                            if (hc[0] as i64) < 65536 {
                                hc[n as usize] = hc[0]
                            } else {
                                hc[n as usize] =
                                    ((hc[0] as i64 - 65536) / 1024i32 as i64 + 55296) as i32;
                                n += 1;
                                hc[n as usize] = ((hc[0] % 1024i32) as i64 + 56320) as i32
                            }
                        }
                    }
                    continue 's_91;
                }
                16 => {
                    scan_char_num();
                    cur_chr = cur_val;
                    cur_cmd = 68i32 as eight_bits
                }
                10 | 2 => {
                    if n as i32 > 1i32 {
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
                    overflow(b"pool size", pool_size - init_pool_ptr);
                }
                h = 0i32 as hyph_pointer;
                j = 1_i16;
                while j as i32 <= n as i32 {
                    h = ((h as i32 + h as i32 + hc[j as usize]) % 607i32) as hyph_pointer;
                    *str_pool.offset(pool_ptr as isize) = hc[j as usize] as packed_UTF16_code;
                    pool_ptr += 1;
                    j += 1
                }
                s = make_string();
                if hyph_next <= 607i32 {
                    while hyph_next > 0i32 && *hyph_word.offset((hyph_next - 1i32) as isize) > 0i32
                    {
                        hyph_next -= 1
                    }
                }
                if hyph_count == hyph_size || hyph_next == 0i32 {
                    overflow(b"exception dictionary", hyph_size);
                }
                hyph_count += 1;
                while *hyph_word.offset(h as isize) != 0i32 {
                    k = *hyph_word.offset(h as isize);
                    if !(length(k) != length(s)) {
                        u = *str_start.offset((k as i64 - 65536) as isize);
                        v = *str_start.offset((s as i64 - 65536) as isize);
                        loop {
                            if *str_pool.offset(u as isize) as i32
                                != *str_pool.offset(v as isize) as i32
                            {
                                current_block = 876886731760051519;
                                break;
                            }
                            u += 1;
                            v += 1;
                            if !(u != *str_start.offset(((k + 1i32) as i64 - 65536) as isize)) {
                                current_block = 8732226822098929438;
                                break;
                            }
                        }
                        match current_block {
                            876886731760051519 => {}
                            _ => {
                                str_ptr -= 1;
                                pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
                                s = *hyph_word.offset(h as isize);
                                hyph_count -= 1;
                                break;
                            }
                        }
                    }
                    /*:975*/
                    /*:976*/
                    if *hyph_link.offset(h as isize) as i32 == 0i32 {
                        *hyph_link.offset(h as isize) = hyph_next as hyph_pointer;
                        if hyph_next >= hyph_size {
                            hyph_next = 607i32
                        }
                        if hyph_next > 607i32 {
                            hyph_next += 1
                        }
                    }
                    h = (*hyph_link.offset(h as isize) as i32 - 1i32) as hyph_pointer
                }
                *hyph_word.offset(h as isize) = s;
                *hyph_list.offset(h as isize) = p
            }
            _ => {}
        }
        if cur_cmd as i32 == 2i32 {
            return;
        }
        n = 0_i16;
        p = TEX_NULL
    }
}
#[no_mangle]
pub(crate) unsafe extern "C" fn prefixed_command() {
    let mut current_block: u64;
    let mut a: small_number = 0;
    let mut f: internal_font_number = 0;
    let mut j: i32 = 0;
    let mut k: font_index = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut n: i32 = 0;
    let mut e: bool = false;
    a = 0i32 as small_number;
    while cur_cmd as i32 == 95i32 {
        if a as i32 / cur_chr & 1i32 == 0 {
            a = (a as i32 + cur_chr) as small_number
        }
        loop {
            get_x_token();
            if !(cur_cmd as i32 == 10i32 || cur_cmd as i32 == 0i32) {
                break;
            }
        }
        if cur_cmd as i32 <= 71i32 {
            /*1247:*/
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"You can\'t use a prefix with `");
            print_cmd_chr(cur_cmd as u16, cur_chr);
            print_char('\'' as i32);
            help_ptr = 1_u8;
            help_line[0] =
                b"I\'ll pretend you didn\'t say \\long or \\outer or \\global or \\protected.";
            back_error();
            return;
        }
        if INTPAR(INT_PAR__tracing_commands) > 2 {
            show_cur_cmd_chr();
        }
    }
    if a as i32 >= 8i32 {
        j = PROTECTED_TOKEN;
        a = (a as i32 - 8i32) as small_number
    } else {
        j = 0i32
    }
    if cur_cmd as i32 != 99i32 && (a as i32 % 4i32 != 0i32 || j != 0i32) {
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
        print_cmd_chr(cur_cmd as u16, cur_chr);
        print_char('\'' as i32);
        error();
    }
    if INTPAR(INT_PAR__global_defs) != 0 {
        if INTPAR(INT_PAR__global_defs) < 0 {
            if a as i32 >= 4i32 {
                a = (a as i32 - 4i32) as small_number
            }
        } else if (a as i32) < 4i32 {
            a = (a as i32 + 4i32) as small_number
        }
    }
    match cur_cmd as _ {
        SET_FONT => {
            /*1252:*/
            if a as i32 >= 4i32 {
                geq_define(CUR_FONT_LOC, DATA as _, cur_chr);
            } else {
                eq_define(CUR_FONT_LOC, DATA as _, cur_chr);
            }
        }
        DEF => {
            if cur_chr & 1i32 != 0 && (a as i32) < 4i32 && INTPAR(INT_PAR__global_defs) >= 0 {
                a = (a as i32 + 4i32) as small_number
            }
            e = cur_chr >= 2i32;
            get_r_token();
            p = cur_cs;
            q = scan_toks(1i32 != 0, e);
            if j != 0i32 {
                q = get_avail();
                (*mem.offset(q as isize)).b32.s0 = j;
                (*mem.offset(q as isize)).b32.s1 =
                    (*mem.offset(def_ref as isize)).b32.s1;
                (*mem.offset(def_ref as isize)).b32.s1 = q
            }
            if a as i32 >= 4i32 {
                geq_define(p, (113i32 + a as i32 % 4i32) as u16,
                           def_ref);
            } else {
                eq_define(p, (113i32 + a as i32 % 4i32) as u16,
                          def_ref);
            }
        }
        LET => {
            n = cur_chr;
            get_r_token();
            p = cur_cs;
            if n == 0i32 {
                loop  {
                    get_token();
                    if !(cur_cmd as i32 == 10i32) { break ; }
                }
                if cur_tok == 0x1800000i32 + '=' as i32 {
                    get_token();
                    if cur_cmd as i32 == 10i32 { get_token(); }
                }
            } else {
                get_token();
                q = cur_tok;
                get_token();
                back_input();
                cur_tok = q;
                back_input();
            }
            if cur_cmd as i32 >= 113i32 {
                let ref mut fresh12 = (*mem.offset(cur_chr as isize)).b32.s0;
                *fresh12 += 1
            } else if cur_cmd as i32 == 91i32 ||
                          cur_cmd as i32 == 72i32 {
                if cur_chr < 0i32 || cur_chr > 19i32 {
                    /* 19 = lo_mem_stat_max, I think */
                    let ref mut fresh13 =
                        (*mem.offset((cur_chr + 1i32) as isize)).b32.s0;
                    *fresh13 += 1
                }
            }
            if a as i32 >= 4i32 {
                geq_define(p, cur_cmd as u16, cur_chr);
            } else { eq_define(p, cur_cmd as u16, cur_chr); }
        }
        SHORTHAND_DEF => {
            if cur_chr == 7i32 {
                scan_char_num();
                p = CHAR_SUB_CODE_BASE + cur_val;
                scan_optional_equals();
                scan_char_num();
                n = cur_val;
                scan_char_num();
                if INTPAR(INT_PAR__tracing_char_sub_def) > 0 {
                    begin_diagnostic();
                    print_nl_cstr(b"New character substitution: ");
                    print(p - CHAR_SUB_CODE_BASE);
                    print_cstr(b" = ");
                    print(n);
                    print_char(' ' as i32);
                    print(cur_val);
                    end_diagnostic(0i32 != 0);
                }
                n = n * 256i32 + cur_val;
                if a as i32 >= 4i32 {
                    geq_define(p, 122_u16, n);
                } else { eq_define(p, 122_u16, n); }
                if p - CHAR_SUB_CODE_BASE < INTPAR(INT_PAR__char_sub_def_min) {
                    if a as i32 >= 4i32 {
                        geq_word_define(INT_BASE + INT_PAR__char_sub_def_min, p - CHAR_SUB_CODE_BASE);
                    } else {
                        eq_word_define(INT_BASE + INT_PAR__char_sub_def_min, p - CHAR_SUB_CODE_BASE);
                    }
                }
                if p - CHAR_SUB_CODE_BASE < INTPAR(INT_PAR__char_sub_def_max) {
                    if a as i32 >= 4i32 {
                        geq_word_define(INT_BASE + INT_PAR__char_sub_def_max, p - CHAR_SUB_CODE_BASE);
                    } else {
                        eq_word_define(INT_BASE + INT_PAR__char_sub_def_max, p - CHAR_SUB_CODE_BASE);
                    }
                }
            } else {
                n = cur_chr;
                get_r_token();
                p = cur_cs;
                if a as i32 >= 4i32 {
                    geq_define(p, RELAX as _, TOO_BIG_USV);
                } else {
                    eq_define(p, RELAX as _, TOO_BIG_USV);
                }
                scan_optional_equals();
                match n {
                    CHAR_DEF_CODE => {
                        scan_usv_num();
                        if a as i32 >= 4i32 {
                            geq_define(p, 68_u16, cur_val);
                        } else { eq_define(p, 68_u16, cur_val); }
                    }
                    MATH_CHAR_DEF_CODE => {
                        scan_fifteen_bit_int();
                        if a as i32 >= 4i32 {
                            geq_define(p, 69_u16, cur_val);
                        } else { eq_define(p, 69_u16, cur_val); }
                    }
                    XETEX_MATH_CHAR_NUM_DEF_CODE => {
                        scan_xetex_math_char_int();
                        if a as i32 >= 4i32 {
                            geq_define(p, 70_u16, cur_val);
                        } else { eq_define(p, 70_u16, cur_val); }
                    }
                    XETEX_MATH_CHAR_DEF_CODE => {
                        scan_math_class_int();
                        n =
                            ((cur_val as u32 &
                                  0x7_u32) << 21i32) as
                                i32;
                        scan_math_fam_int();
                        n =
                            (n as
                                 u32).wrapping_add((cur_val as
                                                                 u32
                                                                 &
                                                                 0xffi32 as
                                                                     u32)
                                                                << 24i32) as
                                i32;
                        scan_usv_num();
                        n = n + cur_val;
                        if a as i32 >= 4i32 {
                            geq_define(p, 70_u16, n);
                        } else { eq_define(p, 70_u16, n); }
                    }
                    _ => {
                        scan_register_num();
                        if cur_val > 255 {
                            j = n - 2;
                            if j > MU_VAL { j = TOK_VAL }
                            find_sa_element(j as small_number, cur_val,
                                            true);
                            let ref mut fresh14 =
                                (*mem.offset((cur_ptr + 1i32) as
                                                 isize)).b32.s0;
                            *fresh14 += 1;
                            if j == 5i32 { j = 72i32 } else { j = 91i32 }
                            if a as i32 >= 4i32 {
                                geq_define(p, j as u16, cur_ptr);
                            } else { eq_define(p, j as u16, cur_ptr); }
                        } else {
                            match n {
                                COUNT_DEF_CODE => {
                                    if a as i32 >= 4i32 {
                                        geq_define(p, ASSIGN_INT, COUNT_BASE + cur_val);
                                    } else {
                                        eq_define(p, ASSIGN_INT, COUNT_BASE + cur_val);
                                    }
                                }
                                DIMEN_DEF_CODE => {
                                    if a as i32 >= 4i32 {
                                        geq_define(p, ASSIGN_DIMEN, SCALED_BASE + cur_val);
                                    } else {
                                        eq_define(p, ASSIGN_DIMEN, SCALED_BASE + cur_val);
                                    }
                                }
                                SKIP_DEF_CODE => {
                                    if a as i32 >= 4i32 {
                                        geq_define(p, ASSIGN_GLUE, SKIP_BASE + cur_val);
                                    } else {
                                        eq_define(p, ASSIGN_GLUE, SKIP_BASE + cur_val);
                                    }
                                }
                                MU_SKIP_DEF_CODE => {
                                    if a as i32 >= 4i32 {
                                        geq_define(p, ASSIGN_MU_GLUE, MU_SKIP_BASE + cur_val);
                                    } else {
                                        eq_define(p, ASSIGN_MU_GLUE, MU_SKIP_BASE + cur_val);
                                    }
                                }
                                TOKS_DEF_CODE => {
                                    if a as i32 >= 4i32 {
                                        geq_define(p, ASSIGN_TOKS, TOKS_BASE + cur_val);
                                    } else {
                                        eq_define(p, ASSIGN_TOKS, TOKS_BASE + cur_val);
                                    }
                                }
                                _ => { }
                            }
                        }
                    }
                }
            }
        }
        READ_TO_CS => {
            j = cur_chr;
            scan_int();
            n = cur_val;
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
            if a as i32 >= 4i32 {
                geq_define(p, 113_u16, cur_val);
            } else { eq_define(p, 113_u16, cur_val); }
        }
        TOKS_REGISTER | ASSIGN_TOKS => {
            q = cur_cs;
            e = false;
            if cur_cmd as i32 == 72i32 {
                if cur_chr == 0i32 {
                    scan_register_num();
                    if cur_val > 255i32 {
                        find_sa_element(5i32 as small_number, cur_val,
                                        true);
                        cur_chr = cur_ptr;
                        e = true
                    } else {
                        cur_chr = TOKS_BASE + cur_val;
                    }
                } else { e = true }
            } else if cur_chr == LOCAL_BASE + LOCAL__xetex_inter_char {
                scan_char_class_not_ignored();
                cur_ptr = cur_val;
                scan_char_class_not_ignored();
                find_sa_element(6i32 as small_number,
                                cur_ptr * 4096i32 + cur_val, true);
                cur_chr = cur_ptr;
                e = true
            }
            p = cur_chr;
            scan_optional_equals();
            loop  {
                get_x_token();
                if !(cur_cmd as i32 == 10i32 ||
                         cur_cmd as i32 == 0i32) {
                    break ;
                }
            }
            if cur_cmd as i32 != 1i32 {
                /*1262:*/
                if cur_cmd as i32 == 72i32 ||
                       cur_cmd as i32 == 73i32 {
                    if cur_cmd as i32 == 72i32 {
                        if cur_chr == 0i32 {
                            scan_register_num(); /* "extended delimiter code flag" */
                            if cur_val < 256i32 {
                                q = TOKS_REG(cur_val);
                            } else {
                                find_sa_element(5i32 as small_number, cur_val,
                                                0i32 !=
                                                    0); /* "extended delimiter code family */
                                if cur_ptr == TEX_NULL {
                                    q = TEX_NULL
                                } else {
                                    q =
                                        (*mem.offset((cur_ptr + 1i32) as
                                                         isize)).b32.s1
                                }
                            }
                        } else {
                            q =
                                (*mem.offset((cur_chr + 1i32) as
                                                 isize)).b32.s1
                        }
                    } else if cur_chr == LOCAL_BASE + LOCAL__xetex_inter_char {
                        scan_char_class_not_ignored(); /*:1268 */
                        cur_ptr = cur_val;
                        scan_char_class_not_ignored();
                        find_sa_element(6i32 as small_number,
                                        cur_ptr * 4096i32 + cur_val,
                                        false);
                        if cur_ptr == TEX_NULL {
                            q = TEX_NULL
                        } else {
                            q =
                                (*mem.offset((cur_ptr + 1i32) as
                                                 isize)).b32.s1
                        }
                    } else { q = (*eqtb.offset(cur_chr as isize)).b32.s1 }
                    if q == TEX_NULL {
                        if e {
                            if a as i32 >= 4i32 {
                                gsa_def(p, TEX_NULL);
                            } else { sa_def(p, TEX_NULL); }
                        } else if a as i32 >= 4i32 {
                            geq_define(p, 103_u16, TEX_NULL);
                        } else {
                            eq_define(p, 103_u16, TEX_NULL);
                        }
                    } else {
                        let ref mut fresh15 =
                            (*mem.offset(q as isize)).b32.s0;
                        *fresh15 += 1;
                        if e {
                            if a as i32 >= 4i32 {
                                gsa_def(p, q);
                            } else { sa_def(p, q); }
                        } else if a as i32 >= 4i32 {
                            geq_define(p, 113_u16, q);
                        } else { eq_define(p, 113_u16, q); }
                    }
                    current_block = 1862445865460439639;
                } else { current_block = 15174492983169363256; }
            } else { current_block = 15174492983169363256; }
            match current_block {
                1862445865460439639 => { }
                _ => {
                    back_input();
                    cur_cs = q;
                    q = scan_toks(false, false);
                    if (*mem.offset(def_ref as isize)).b32.s1 == TEX_NULL {
                        if e {
                            if a as i32 >= 4i32 {
                                gsa_def(p, TEX_NULL);
                            } else { sa_def(p, TEX_NULL); }
                        } else if a as i32 >= 4i32 {
                            geq_define(p, 103_u16, TEX_NULL);
                        } else {
                            eq_define(p, 103_u16, TEX_NULL);
                        }
                        (*mem.offset(def_ref as isize)).b32.s1 = avail;
                        avail = def_ref
                    } else {
                        if p == LOCAL_BASE + LOCAL__output_routine && !e {
                            (*mem.offset(q as isize)).b32.s1 = get_avail();
                            q = (*mem.offset(q as isize)).b32.s1;
                            (*mem.offset(q as isize)).b32.s0 =
                                0x400000i32 + 125i32;
                            q = get_avail();
                            (*mem.offset(q as isize)).b32.s0 =
                                0x200000i32 + 123i32;
                            (*mem.offset(q as isize)).b32.s1 =
                                (*mem.offset(def_ref as isize)).b32.s1;
                            (*mem.offset(def_ref as isize)).b32.s1 = q
                        }
                        if e {
                            if a as i32 >= 4i32 {
                                gsa_def(p, def_ref);
                            } else { sa_def(p, def_ref); }
                        } else if a as i32 >= 4i32 {
                            geq_define(p, 113_u16, def_ref);
                        } else { eq_define(p, 113_u16, def_ref); }
                    }
                }
            }
        }
        ASSIGN_INT => {
            p = cur_chr;
            scan_optional_equals();
            scan_int();
            if a as i32 >= 4i32 {
                geq_word_define(p, cur_val);
            } else { eq_word_define(p, cur_val); }
        }
        ASSIGN_DIMEN => {
            p = cur_chr;
            scan_optional_equals();
            scan_dimen(false, false, false);
            if a as i32 >= 4i32 {
                geq_word_define(p, cur_val);
            } else { eq_word_define(p, cur_val); }
        }
        ASSIGN_GLUE | ASSIGN_MU_GLUE => {
            p = cur_chr;
            n = cur_cmd as i32;
            scan_optional_equals();
            if n == 77i32 {
                scan_glue(3i32 as small_number);
            } else { scan_glue(2i32 as small_number); }
            trap_zero_glue();
            if a as i32 >= 4i32 {
                geq_define(p, 119_u16, cur_val);
            } else { eq_define(p, 119_u16, cur_val); }
        }
        XETEX_DEF_CODE => {
            if cur_chr == SF_CODE_BASE {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                n = SF_CODE(cur_val) % 65536;
                scan_optional_equals();
                scan_char_class();
                if a as i32 >= 4i32 {
                    geq_define(p, 122_u16,
                               (cur_val as i64 * 65536 +
                                    n as i64) as i32);
                } else {
                    eq_define(p, 122_u16,
                              (cur_val as i64 * 65536 +
                                   n as i64) as i32);
                }
            } else if cur_chr == MATH_CODE_BASE {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_xetex_math_char_int();
                if a as i32 >= 4i32 {
                    geq_define(p, 122_u16, cur_val);
                } else { eq_define(p, 122_u16, cur_val); }
            } else if cur_chr == MATH_CODE_BASE + 1 {
                p = cur_chr - 1i32;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_math_class_int();
                n =
                    ((cur_val as u32 & 0x7_u32) <<
                         21i32) as i32;
                scan_math_fam_int();
                n =
                    (n as
                         u32).wrapping_add((cur_val as u32 &
                                                         0xffi32 as
                                                             u32) <<
                                                        24i32) as i32;
                scan_usv_num();
                n = n + cur_val;
                if a as i32 >= 4i32 {
                    geq_define(p, 122_u16, n);
                } else { eq_define(p, 122_u16, n); }
            } else if cur_chr == DEL_CODE_BASE {
                p = cur_chr;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                scan_int();
                if a as i32 >= 4i32 {
                    geq_word_define(p, cur_val);
                } else { eq_word_define(p, cur_val); }
            } else {
                p = cur_chr - 1i32;
                scan_usv_num();
                p = p + cur_val;
                scan_optional_equals();
                n = 0x40000000i32;
                scan_math_fam_int();
                n = n + cur_val * 0x200000i32;
                scan_usv_num();
                n = n + cur_val;
                if a as i32 >= 4i32 {
                    geq_word_define(p, n);
                } else { eq_word_define(p, n); }
            }
        }
    DEF_CODE => {
            if cur_chr == CAT_CODE_BASE {
                n = 15i32
            } else if cur_chr == MATH_CODE_BASE {
                n = 0x8000i32
            } else if cur_chr == SF_CODE_BASE {
                n = 0x7fffi32
            } else if cur_chr == DEL_CODE_BASE {
                n = 0xffffffi32
            } else { n = BIGGEST_USV }
            p = cur_chr;
            scan_usv_num();
            p = p + cur_val;
            scan_optional_equals();
            scan_int();
            if (cur_val < 0i32 && p < DEL_CODE_BASE) || cur_val > n {
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr(b"! ");
                }
                print_cstr(b"Invalid code (");
                print_int(cur_val);
                if p < MATH_CODE_BASE {
                    print_cstr(b"), should be in the range 0..");
                } else {
                    print_cstr(b"), should be at most ");
                }
                print_int(n);
                help_ptr = 1_u8;
                help_line[0] =
                    b"I\'m going to use 0 instead of that illegal code value.";
                error();
                cur_val = 0i32
            }
            if p < MATH_CODE_BASE {
                if p >= SF_CODE_BASE {
                    n =
                        ((*eqtb.offset(p as isize)).b32.s1 as i64 /
                             65536) as i32;
                    if a as i32 >= 4i32 {
                        geq_define(p, 122_u16,
                                   (n as i64 * 65536 +
                                        cur_val as i64) as i32);
                    } else {
                        eq_define(p, 122_u16,
                                  (n as i64 * 65536 +
                                       cur_val as i64) as i32);
                    }
                } else if a as i32 >= 4i32 {
                    geq_define(p, 122_u16, cur_val);
                } else { eq_define(p, 122_u16, cur_val); }
            } else if p < DEL_CODE_BASE {
                if cur_val as i64 == 32768 {
                    cur_val = ACTIVE_MATH_CHAR
                } else {
                    cur_val =
                        (((cur_val / 4096i32) as u32 &
                              0x7_u32) <<
                             21i32).wrapping_add(((cur_val % 4096i32 / 256i32)
                                                      as u32 &
                                                      0xff_u32)
                                                     <<
                                                     24i32).wrapping_add((cur_val
                                                                              %
                                                                              256i32)
                                                                             as
                                                                             u32)
                            as i32
                }
                if a as i32 >= 4i32 {
                    geq_define(p, 122_u16, cur_val);
                } else { eq_define(p, 122_u16, cur_val); }
            } else if a as i32 >= 4i32 {
                geq_word_define(p, cur_val);
            } else { eq_word_define(p, cur_val); }
        }
        DEF_FAMILY => {
            p = cur_chr;
            scan_math_fam_int();
            p = p + cur_val;
            scan_optional_equals();
            scan_font_ident();
            if a as i32 >= 4i32 {
                geq_define(p, 122_u16, cur_val);
            } else { eq_define(p, 122_u16, cur_val); }
        }
        REGISTER | ADVANCE | MULTIPLY | DIVIDE => { do_register_command(a); }
        SET_BOX => {
            scan_register_num();
            if a as i32 >= 4i32 {
                n = GLOBAL_BOX_FLAG + cur_val
            } else { n = BOX_FLAG + cur_val }
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
        SET_AUX => { alter_aux(); }
        SET_PREV_GRAF => { alter_prev_graf(); }
        SET_PAGE_DIMEN => { alter_page_so_far(); }
        SET_PAGE_INT => { alter_integer(); }
        SET_BOX_DIMEN => { alter_box_dimen(); }
        SET_SHAPE => {
            q = cur_chr;
            scan_optional_equals();
            scan_int();
            n = cur_val;
            if n <= 0i32 {
                p = TEX_NULL
            } else if q > LOCAL_BASE + LOCAL__par_shape {
                n = cur_val / 2i32 + 1i32;
                p = get_node(2i32 * n + 1i32);
                (*mem.offset(p as isize)).b32.s0 = n;
                n = cur_val;
                (*mem.offset((p + 1i32) as isize)).b32.s1 = n;
                j = p + 2i32;
                while j <= p + n + 1i32 {
                    scan_int();
                    (*mem.offset(j as isize)).b32.s1 = cur_val;
                    j += 1
                }
                if n & 1i32 == 0 {
                    (*mem.offset((p + n + 2i32) as isize)).b32.s1 = 0i32
                }
            } else {
                p = get_node(2i32 * n + 1i32);
                (*mem.offset(p as isize)).b32.s0 = n;
                j = 1i32;
                while j <= n {
                    scan_dimen(false, false, false);
                    (*mem.offset((p + 2i32 * j - 1i32) as isize)).b32.s1 =
                        cur_val;
                    scan_dimen(false, false, false);
                    (*mem.offset((p + 2i32 * j) as isize)).b32.s1 = cur_val;
                    j += 1
                }
            }
            if a as i32 >= 4i32 {
                geq_define(q, 120_u16, p);
            } else { eq_define(q, 120_u16, p); }
        }
        HYPH_DATA => {
            if cur_chr == 1i32 {
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
                        if !(cur_cmd != RIGHT_BRACE as _) { break ; }
                    }
                    return
                }
            } else { new_hyph_exceptions(); }
        }
        ASSIGN_FONT_DIMEN => {
            find_font_dimen(true);
            k = cur_val;
            scan_optional_equals();
            scan_dimen(false, false, false);
            (*font_info.offset(k as isize)).b32.s1 = cur_val
        }
        ASSIGN_FONT_INT => {
            n = cur_chr;
            scan_font_ident();
            f = cur_val;
            if n < 2i32 {
                scan_optional_equals();
                scan_int();
                if n == 0i32 {
                    *hyphen_char.offset(f as isize) = cur_val
                } else { *skew_char.offset(f as isize) = cur_val }
            } else {
                if *font_area.offset(f as isize) as u32 == 0xffffu32
                       ||
                       *font_area.offset(f as isize) as u32 ==
                           0xfffeu32 {
                    scan_glyph_number(f);
                } else { scan_char_num(); }
                p = cur_val;
                scan_optional_equals();
                scan_int();
                match n {
                    LP_CODE_BASE => { set_cp_code(f, p as u32, 0i32, cur_val); }
                    RP_CODE_BASE => { set_cp_code(f, p as u32, 1i32, cur_val); }
                    _ => { }
                }
            }
        }
        DEF_FONT => { new_font(a); }
        SET_INTERACTION => { new_interaction(); }
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
unsafe extern "C" fn store_fmt_file() {
    let mut current_block: u64;
    let mut j: i32 = 0;
    let mut k: i32 = 0;
    let mut l: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut x: i32 = 0;
    if save_ptr != 0i32 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"You can\'t dump inside a group");
        help_ptr = 1_u8;
        help_line[0] = b"`{...\\dump}\' is a no-no.";
        if interaction as i32 == 3i32 {
            interaction = 2_u8
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
    print_int(INTPAR(INT_PAR__year));
    print_char('.' as i32);
    print_int(INTPAR(INT_PAR__month));
    print_char('.' as i32);
    print_int(INTPAR(INT_PAR__day));
    print_char(')' as i32);
    if interaction as i32 == BATCH_MODE {
        selector = Selector::LOG_ONLY
    } else {
        selector = Selector::TERM_AND_LOG
    }
    if pool_ptr + 1 > pool_size {
        overflow(b"pool size", pool_size - init_pool_ptr);
    }
    format_ident = make_string();
    pack_job_name(b".fmt");
    let fmt_out = ttstub_output_open(name_of_file, 0i32);
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
    pool_ptr = *str_start.offset((str_ptr - 65536i32) as isize);
    print_nl_cstr(b"");
    print(format_ident);
    /* Header */
    let mut x_val: i32 = 0x54544e43i32; /* TODO: can we move this farther up in this function? */
    do_dump(
        &mut x_val as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_0: i32 = FORMAT_SERIAL;
    do_dump(
        &mut x_val_0 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_1: i32 = hash_high;
    do_dump(
        &mut x_val_1 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    while pseudo_files != TEX_NULL {
        pseudo_close();
    }
    let mut x_val_2: i32 = MEM_TOP;
    do_dump(
        &mut x_val_2 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_3: i32 = EQTB_SIZE;
    do_dump(
        &mut x_val_3 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_4: i32 = HASH_PRIME;
    do_dump(
        &mut x_val_4 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_5: i32 = HYPH_PRIME;
    do_dump(
        &mut x_val_5 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    /* string pool */
    let mut x_val_6: i32 = pool_ptr;
    do_dump(
        &mut x_val_6 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_7: i32 = str_ptr;
    do_dump(
        &mut x_val_7 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *str_start.offset(0) as *mut pool_pointer as *mut i8,
        ::std::mem::size_of::<pool_pointer>() as _,
        (str_ptr - 65536i32 + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *str_pool.offset(0) as *mut packed_UTF16_code as *mut i8,
        ::std::mem::size_of::<packed_UTF16_code>() as _,
        pool_ptr as size_t,
        fmt_out,
    );
    print_ln();
    print_int(str_ptr);
    print_cstr(b" strings of total length ");
    print_int(pool_ptr);
    /* "memory locations" */
    sort_avail();
    var_used = 0i32;
    let mut x_val_8: i32 = lo_mem_max;
    do_dump(
        &mut x_val_8 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_9: i32 = rover;
    do_dump(
        &mut x_val_9 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    k = 0i32;
    while k <= 6i32 {
        let mut x_val_10: i32 = sa_root[k as usize];
        do_dump(
            &mut x_val_10 as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_out,
        );
        k += 1
    }
    p = 0i32;
    q = rover;
    x = 0i32;
    loop {
        do_dump(
            &mut *mem.offset(p as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            (q + 2i32 - p) as size_t,
            fmt_out,
        );
        x = x + q + 2i32 - p;
        var_used = var_used + q - p;
        p = q + (*mem.offset(q as isize)).b32.s0;
        q = (*mem.offset((q + 1i32) as isize)).b32.s1;
        if !(q != rover) {
            break;
        }
    }
    var_used = var_used + lo_mem_max - p;
    dyn_used = mem_end + 1i32 - hi_mem_min;
    do_dump(
        &mut *mem.offset(p as isize) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        (lo_mem_max + 1i32 - p) as size_t,
        fmt_out,
    );
    x = x + lo_mem_max + 1i32 - p;
    let mut x_val_11: i32 = hi_mem_min;
    do_dump(
        &mut x_val_11 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_12: i32 = avail;
    do_dump(
        &mut x_val_12 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *mem.offset(hi_mem_min as isize) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        (mem_end + 1i32 - hi_mem_min) as size_t,
        fmt_out,
    );
    x = x + mem_end + 1i32 - hi_mem_min;
    p = avail;
    while p != TEX_NULL {
        dyn_used -= 1;
        p = (*mem.offset(p as isize)).b32.s1
    }
    let mut x_val_13: i32 = var_used;
    do_dump(
        &mut x_val_13 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_14: i32 = dyn_used;
    do_dump(
        &mut x_val_14 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    print_ln();
    print_int(x);
    print_cstr(b" memory locations dumped; current usage is ");
    print_int(var_used);
    print_char('&' as i32);
    print_int(dyn_used);
    /* equivalents table / primitive */
    k = ACTIVE_BASE; /*:1350*/
    loop {
        j = k;
        loop {
            if !(j < INT_BASE - 1) {
                current_block = 7923086311623215889;
                break;
            }
            if (*eqtb.offset(j as isize)).b32.s1 == (*eqtb.offset((j + 1i32) as isize)).b32.s1
                && (*eqtb.offset(j as isize)).b16.s1 as i32
                    == (*eqtb.offset((j + 1i32) as isize)).b16.s1 as i32
                && (*eqtb.offset(j as isize)).b16.s0 as i32
                    == (*eqtb.offset((j + 1i32) as isize)).b16.s0 as i32
            {
                current_block = 8379985486002839332;
                break;
            }
            j += 1
        }
        match current_block {
            7923086311623215889 => {
                l = INT_BASE;
            }
            _ => {
                j += 1;
                l = j;
                while j < INT_BASE - 1 {
                    if (*eqtb.offset(j as isize)).b32.s1
                        != (*eqtb.offset((j + 1i32) as isize)).b32.s1
                        || (*eqtb.offset(j as isize)).b16.s1 as i32
                            != (*eqtb.offset((j + 1i32) as isize)).b16.s1 as i32
                        || (*eqtb.offset(j as isize)).b16.s0 as i32
                            != (*eqtb.offset((j + 1i32) as isize)).b16.s0 as i32
                    {
                        break;
                    }
                    j += 1
                }
            }
        }
        let mut x_val_15: i32 = l - k;
        do_dump(
            &mut x_val_15 as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_out,
        );
        do_dump(
            &mut *eqtb.offset(k as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            (l - k) as size_t,
            fmt_out,
        );
        k = j + 1i32;
        let mut x_val_16: i32 = k - l;
        do_dump(
            &mut x_val_16 as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_out,
        );
        if !(k != INT_BASE) {
            break;
        }
    }
    loop {
        j = k;
        loop {
            if !(j < EQTB_SIZE) {
                current_block = 10505255564575309249;
                break;
            }
            if (*eqtb.offset(j as isize)).b32.s1 == (*eqtb.offset((j + 1i32) as isize)).b32.s1 {
                current_block = 18329769178042496632;
                break;
            }
            j += 1
        }
        match current_block {
            10505255564575309249 => {
                l = EQTB_SIZE + 1;
            }
            _ => {
                j += 1;
                l = j;
                while j < EQTB_SIZE {
                    if (*eqtb.offset(j as isize)).b32.s1
                        != (*eqtb.offset((j + 1i32) as isize)).b32.s1
                    {
                        break;
                    }
                    j += 1
                }
            }
        }
        let mut x_val_17: i32 = l - k;
        do_dump(
            &mut x_val_17 as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_out,
        );
        do_dump(
            &mut *eqtb.offset(k as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            (l - k) as size_t,
            fmt_out,
        );
        k = j + 1i32;
        let mut x_val_18: i32 = k - l;
        do_dump(
            &mut x_val_18 as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_out,
        );
        if !(k <= EQTB_SIZE) {
            break;
        }
    }
    if hash_high > 0i32 {
        do_dump(
            &mut *eqtb.offset(EQTB_SIZE as isize + 1) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            hash_high as size_t,
            fmt_out,
        );
    }
    let mut x_val_19: i32 = par_loc;
    do_dump(
        &mut x_val_19 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_20: i32 = write_loc;
    do_dump(
        &mut x_val_20 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    p = 0i32;
    while p <= PRIM_SIZE {
        do_dump(
            &mut *prim.as_mut_ptr().offset(p as isize) as *mut b32x2 as *mut i8,
            ::std::mem::size_of::<b32x2>() as _,
            1i32 as size_t,
            fmt_out,
        );
        p += 1
    }
    p = 0i32;
    while p <= PRIM_SIZE {
        do_dump(
            &mut *prim_eqtb.as_mut_ptr().offset(p as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            1i32 as size_t,
            fmt_out,
        );
        p += 1
    }
    /* control sequences */
    let mut x_val_21: i32 = hash_used;
    do_dump(
        &mut x_val_21 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    cs_count = (FROZEN_CONTROL_SEQUENCE - 1) - hash_used + hash_high;
    p = HASH_BASE;
    while p <= hash_used {
        if (*hash.offset(p as isize)).s1 != 0i32 {
            let mut x_val_22: i32 = p;
            do_dump(
                &mut x_val_22 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
            do_dump(
                &mut *hash.offset(p as isize) as *mut b32x2 as *mut i8,
                ::std::mem::size_of::<b32x2>() as _,
                1i32 as size_t,
                fmt_out,
            );
            cs_count += 1
        }
        p += 1
    }
    do_dump(
        &mut *hash.offset((hash_used + 1i32) as isize) as *mut b32x2 as *mut i8,
        ::std::mem::size_of::<b32x2>() as _,
        ((UNDEFINED_CONTROL_SEQUENCE - 1) - hash_used) as _,
        fmt_out,
    );
    if hash_high > 0i32 {
        do_dump(
            &mut *hash.offset(EQTB_SIZE as isize + 1) as *mut b32x2 as *mut i8,
            ::std::mem::size_of::<b32x2>() as _,
            hash_high as size_t,
            fmt_out,
        );
    }
    let mut x_val_23: i32 = cs_count;
    do_dump(
        &mut x_val_23 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    print_ln();
    print_int(cs_count);
    print_cstr(b" multiletter control sequences");
    /* fonts */
    let mut x_val_24: i32 = fmem_ptr;
    do_dump(
        &mut x_val_24 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_info.offset(0) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        fmem_ptr as size_t,
        fmt_out,
    );
    let mut x_val_25: i32 = font_ptr;
    do_dump(
        &mut x_val_25 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_check.offset(0) as *mut b16x4 as *mut i8,
        ::std::mem::size_of::<b16x4>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_size.offset(0) as *mut scaled_t as *mut i8,
        ::std::mem::size_of::<scaled_t>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_dsize.offset(0) as *mut scaled_t as *mut i8,
        ::std::mem::size_of::<scaled_t>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_params.offset(0) as *mut font_index as *mut i8,
        ::std::mem::size_of::<font_index>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *hyphen_char.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *skew_char.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_name.offset(0) as *mut str_number as *mut i8,
        ::std::mem::size_of::<str_number>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_area.offset(0) as *mut str_number as *mut i8,
        ::std::mem::size_of::<str_number>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_bc.offset(0) as *mut UTF16_code as *mut i8,
        ::std::mem::size_of::<UTF16_code>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_ec.offset(0) as *mut UTF16_code as *mut i8,
        ::std::mem::size_of::<UTF16_code>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *char_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *width_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *height_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *depth_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *italic_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *lig_kern_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *kern_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *exten_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *param_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_glue.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *bchar_label.offset(0) as *mut font_index as *mut i8,
        ::std::mem::size_of::<font_index>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_bchar.offset(0) as *mut nine_bits as *mut i8,
        ::std::mem::size_of::<nine_bits>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *font_false_bchar.offset(0) as *mut nine_bits as *mut i8,
        ::std::mem::size_of::<nine_bits>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_out,
    );
    k = FONT_BASE;
    while k <= font_ptr {
        print_nl_cstr(b"\\font");
        print_esc((*hash.offset(FONT_ID_BASE as isize + k as isize)).s1);
        print_char('=' as i32);
        if *font_area.offset(k as isize) as u32 == 0xffffu32
            || *font_area.offset(k as isize) as u32 == 0xfffeu32
            || !(*font_mapping.offset(k as isize)).is_null()
        {
            print_file_name(
                *font_name.offset(k as isize),
                (65536 + 1i32 as i64) as i32,
                (65536 + 1i32 as i64) as i32,
            );
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
            print_file_name(
                *font_name.offset(k as isize),
                *font_area.offset(k as isize),
                (65536 + 1i32 as i64) as i32,
            );
        }
        if *font_size.offset(k as isize) != *font_dsize.offset(k as isize) {
            print_cstr(b" at ");
            print_scaled(*font_size.offset(k as isize));
            print_cstr(b"pt");
        }
        k += 1
    }
    print_ln();
    print_int(fmem_ptr - 7i32);
    print_cstr(b" words of font info for ");
    print_int(font_ptr - 0i32);
    if font_ptr != 0i32 + 1i32 {
        print_cstr(b" preloaded fonts");
    } else {
        print_cstr(b" preloaded font");
    }
    /* hyphenation info */
    let mut x_val_26: i32 = hyph_count;
    do_dump(
        &mut x_val_26 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    if hyph_next <= 607i32 {
        hyph_next = hyph_size
    }
    let mut x_val_27: i32 = hyph_next;
    do_dump(
        &mut x_val_27 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    k = 0i32;
    while k <= hyph_size {
        if *hyph_word.offset(k as isize) != 0i32 {
            let mut x_val_28: i32 =
                (k as i64 + 65536 * *hyph_link.offset(k as isize) as i64) as i32;
            do_dump(
                &mut x_val_28 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
            let mut x_val_29: i32 = *hyph_word.offset(k as isize);
            do_dump(
                &mut x_val_29 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
            let mut x_val_30: i32 = *hyph_list.offset(k as isize);
            do_dump(
                &mut x_val_30 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
        }
        k += 1
    }
    print_ln();
    print_int(hyph_count);
    if hyph_count != 1i32 {
        print_cstr(b" hyphenation exceptions");
    } else {
        print_cstr(b" hyphenation exception");
    }
    if trie_not_ready {
        init_trie();
    }
    let mut x_val_31: i32 = trie_max;
    do_dump(
        &mut x_val_31 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_32: i32 = hyph_start;
    do_dump(
        &mut x_val_32 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *trie_trl.offset(0) as *mut trie_pointer as *mut i8,
        ::std::mem::size_of::<trie_pointer>() as _,
        (trie_max + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *trie_tro.offset(0) as *mut trie_pointer as *mut i8,
        ::std::mem::size_of::<trie_pointer>() as _,
        (trie_max + 1i32) as size_t,
        fmt_out,
    );
    do_dump(
        &mut *trie_trc.offset(0) as *mut u16 as *mut i8,
        ::std::mem::size_of::<u16>() as _,
        (trie_max + 1i32) as size_t,
        fmt_out,
    );
    let mut x_val_33: i32 = max_hyph_char;
    do_dump(
        &mut x_val_33 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    let mut x_val_34: i32 = trie_op_ptr;
    do_dump(
        &mut x_val_34 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    do_dump(
        &mut *hyf_distance.as_mut_ptr().offset(1) as *mut small_number as *mut i8,
        ::std::mem::size_of::<small_number>() as _,
        trie_op_ptr as size_t,
        fmt_out,
    );
    do_dump(
        &mut *hyf_num.as_mut_ptr().offset(1) as *mut small_number as *mut i8,
        ::std::mem::size_of::<small_number>() as _,
        trie_op_ptr as size_t,
        fmt_out,
    );
    do_dump(
        &mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode as *mut i8,
        ::std::mem::size_of::<trie_opcode>() as _,
        trie_op_ptr as size_t,
        fmt_out,
    );
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
    print_int(35111 as i32);
    k = BIGGEST_LANG;
    while k >= 0i32 {
        if trie_used[k as usize] as i32 > 0i32 {
            print_nl_cstr(b"  ");
            print_int(trie_used[k as usize] as i32);
            print_cstr(b" for language ");
            print_int(k);
            let mut x_val_35: i32 = k;
            do_dump(
                &mut x_val_35 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
            let mut x_val_36: i32 = trie_used[k as usize] as i32;
            do_dump(
                &mut x_val_36 as *mut i32 as *mut i8,
                ::std::mem::size_of::<i32>() as _,
                1i32 as size_t,
                fmt_out,
            );
        }
        k -= 1
    }
    /* footer */
    let mut x_val_37: i32 = 0x29ai32; /*:1361*/
    do_dump(
        &mut x_val_37 as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_out,
    );
    INTPAR_set(INT_PAR__tracing_stats, 0);
    ttstub_output_close(fmt_out_owner);
}
unsafe extern "C" fn pack_buffered_name(mut _n: small_number, mut _a: i32, mut _b: i32) {
    free(name_of_file as *mut libc::c_void);
    name_of_file = xmalloc_array(format_default_length as usize + 1);
    strcpy(name_of_file, TEX_format_default);
    name_length = strlen(name_of_file) as i32;
}
unsafe extern "C" fn load_fmt_file() -> bool {
    let mut _current_block: u64;
    let mut j: i32 = 0;
    let mut k: i32 = 0;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut x: i32 = 0;

    j = cur_input.loc;

    /* This is where a first line starting with "&" used to
     * trigger code that would change the format file. */

    pack_buffered_name((format_default_length - 4) as small_number, 1, 0);
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
        free(font_info as *mut libc::c_void);
        free(str_pool as *mut libc::c_void);
        free(str_start as *mut libc::c_void);
        free(yhash as *mut libc::c_void);
        free(eqtb as *mut libc::c_void);
        free(mem as *mut libc::c_void);
        mem = 0 as *mut memory_word
    }
    fn bad_fmt() -> ! {
        panic!("fatal format file error");
    };
    /* start reading the header */
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != FORMAT_HEADER_MAGIC {
        bad_fmt();
    }
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != FORMAT_SERIAL {
        abort!(
            "format file \"{}\" is of the wrong version: expected {}, found {}",
            CStr::from_ptr(name_of_file).display(),
            FORMAT_SERIAL,
            x
        );
    }
    /* hash table parameters */
    do_undump(
        &mut hash_high as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if hash_high < 0 || hash_high > sup_hash_extra {
        bad_fmt();
    }
    if hash_extra < hash_high {
        hash_extra = hash_high
    }
    eqtb_top = EQTB_SIZE + hash_extra;
    if hash_extra == 0i32 {
        hash_top = UNDEFINED_CONTROL_SEQUENCE;
    } else {
        hash_top = eqtb_top
    }
    yhash = xmalloc_array::<b32x2>((1 + hash_top - hash_offset) as usize);
    hash = yhash.offset(-514);
    (*hash.offset((1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32) as isize)).s0 = 0i32;
    (*hash.offset((1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32) as isize)).s1 = 0i32;

    x = 1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 1i32;
    while x <= hash_top {
        *hash.offset(x as isize) = *hash.offset(HASH_BASE as isize);
        x += 1
    }

    eqtb = xmalloc_array::<memory_word>(eqtb_top as usize + 1);
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b16.s1 = UNDEFINED_CS as _;
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b32.s1 = TEX_NULL as _;
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b16.s0 = LEVEL_ZERO as _;
    x = EQTB_SIZE + 1;
    while x <= eqtb_top {
        *eqtb.offset(x as isize) = *eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize);
        x += 1
    }
    max_reg_num = 32767i32;
    max_reg_help_line = b"A register number must be between 0 and 32767.";
    /* "memory locations" */
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != MEM_TOP {
        bad_fmt();
    }
    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    page_tail = PAGE_HEAD;
    mem = xmalloc_array::<memory_word>(MEM_TOP as usize + 1);

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != EQTB_SIZE {
        bad_fmt();
    }

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != HASH_PRIME {
        bad_fmt();
    }

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != HYPH_PRIME {
        bad_fmt();
    }

    /* string pool */

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
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
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 0 {
        bad_fmt();
    }
    if x as i64 > sup_max_strings as i64 - strings_free as i64 {
        panic!("must increase sup_strings");
    }
    str_ptr = x;

    if max_strings < str_ptr + strings_free {
        max_strings = str_ptr + strings_free
    }

    str_start = xmalloc_array::<pool_pointer>(max_strings as usize);
    let mut i: i32 = 0;
    do_undump(
        &mut *str_start.offset(0) as *mut pool_pointer as *mut i8,
        ::std::mem::size_of::<pool_pointer>() as _,
        (str_ptr - 65536i32 + 1i32) as size_t,
        fmt_in,
    );
    i = 0i32;
    while i < str_ptr - 65536i32 + 1i32 {
        if *(&mut *str_start.offset(0) as *mut pool_pointer).offset(i as isize) < 0i32
            || *(&mut *str_start.offset(0) as *mut pool_pointer).offset(i as isize) > pool_ptr
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                *(&mut *str_start.offset(0) as *mut pool_pointer).offset(i as isize) as uintptr_t,
                &mut *str_start.offset(0) as *mut pool_pointer as uintptr_t,
                0i32 as uintptr_t,
                pool_ptr as uintptr_t
            );
        }
        i += 1
    }
    str_pool = xmalloc_array::<packed_UTF16_code>(pool_size as usize);
    do_undump(
        &mut *str_pool.offset(0) as *mut packed_UTF16_code as *mut i8,
        ::std::mem::size_of::<packed_UTF16_code>() as _,
        pool_ptr as size_t,
        fmt_in,
    );
    init_str_ptr = str_ptr;
    init_pool_ptr = pool_ptr;
    /* "By sorting the list of available spaces in the variable-size portion
     * of |mem|, we are usually able to get by without having to dump very
     * much of the dynamic memory." */
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 1019 || x > MEM_TOP - HI_MEM_STAT_USAGE {
        bad_fmt();
    } else {
        lo_mem_max = x;
    }
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 20 || x > lo_mem_max {
        bad_fmt();
    } else {
        rover = x;
    }
    k = INT_VAL;
    loop {
        if !(k <= INTER_CHAR_VAL) {
            break;
        }
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < TEX_NULL || x > lo_mem_max {
            bad_fmt();
        } else {
            sa_root[k as usize] = x;
        }
        k += 1
    }
    p = 0i32;
    q = rover;
    loop {
        do_undump(
            &mut *mem.offset(p as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            (q + 2i32 - p) as size_t,
            fmt_in,
        );
        p = q + (*mem.offset(q as isize)).b32.s0;
        if p > lo_mem_max
            || q >= (*mem.offset((q + 1i32) as isize)).b32.s1
                && (*mem.offset((q + 1i32) as isize)).b32.s1 != rover
        {
            bad_fmt();
        }
        q = (*mem.offset((q + 1i32) as isize)).b32.s1;
        if !(q != rover) {
            break;
        }
    }
    do_undump(
        &mut *mem.offset(p as isize) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        (lo_mem_max + 1i32 - p) as size_t,
        fmt_in,
    );
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < lo_mem_max + 1 || x > PRE_ADJUST_HEAD {
        bad_fmt();
    } else {
        hi_mem_min = x;
    }
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < MIN_HALFWORD || x > MEM_TOP {
        bad_fmt();
    } else {
        avail = x;
    }

    mem_end = MEM_TOP;

    do_undump(
        &mut *mem.offset(hi_mem_min as isize) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        (mem_end + 1i32 - hi_mem_min) as size_t,
        fmt_in,
    );
    do_undump(
        &mut var_used as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    do_undump(
        &mut dyn_used as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    /* equivalents table / primitives
     *
     * "The table of equivalents usually contains repeated information, so we
     * dump it in compressed form: The sequence of $n + 2$ values
     * $(n, x_1, \ldots, x_n, m)$ in the format file represents $n + m$ consecutive
     * entries of |eqtb|, with |m| extra copies of $x_n$, namely
     * $(x_1, \ldots, x_n, x_n, \ldots, x_n)$"
     */
    k = ACTIVE_BASE;
    loop {
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < 1 || k + x > EQTB_SIZE + 1 {
            bad_fmt();
        }

        do_undump(
            &mut *eqtb.offset(k as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            x as size_t,
            fmt_in,
        );
        k = k + x;

        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < 0i32 || k + x > EQTB_SIZE + 1 {
            bad_fmt();
        }

        j = k;
        while j <= k + x - 1 {
            *eqtb.offset(j as isize) = *eqtb.offset((k - 1) as isize);
            j += 1
        }
        k = k + x;
        if !(k <= EQTB_SIZE) {
            break;
        }
    }
    if hash_high > 0i32 {
        do_undump(
            &mut *eqtb.offset(EQTB_SIZE as isize + 1) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            hash_high as size_t,
            fmt_in,
        );
    }
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < HASH_BASE || x > hash_top {
        bad_fmt();
    } else {
        par_loc = x;
    }
    par_token = CS_TOKEN_FLAG + par_loc;
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < HASH_BASE || x > hash_top {
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
        do_undump(
            &mut *prim.as_mut_ptr().offset(p as isize) as *mut b32x2 as *mut i8,
            ::std::mem::size_of::<b32x2>() as _,
            1i32 as size_t,
            fmt_in,
        );
        p += 1
    }
    p = 0i32;
    while p <= 500i32 {
        do_undump(
            &mut *prim_eqtb.as_mut_ptr().offset(p as isize) as *mut memory_word as *mut i8,
            ::std::mem::size_of::<memory_word>() as _,
            1i32 as size_t,
            fmt_in,
        );
        p += 1
    }

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < HASH_BASE || x > FROZEN_CONTROL_SEQUENCE {
        bad_fmt();
    } else {
        hash_used = x;
    }
    p = HASH_BASE - 1;
    loop {
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < p + 1 || x > hash_used {
            bad_fmt();
        } else {
            p = x;
        }
        do_undump(
            &mut *hash.offset(p as isize) as *mut b32x2 as *mut i8,
            ::std::mem::size_of::<b32x2>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if !(p != hash_used) {
            break;
        }
    }
    do_undump(
        &mut *hash.offset((hash_used + 1i32) as isize) as *mut b32x2 as *mut i8,
        ::std::mem::size_of::<b32x2>() as _,
        (1i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            - 1i32
            - hash_used) as size_t,
        fmt_in,
    );
    if hash_high > 0i32 {
        do_undump(
            &mut *hash.offset(EQTB_SIZE as isize + 1) as *mut b32x2 as *mut i8,
            ::std::mem::size_of::<b32x2>() as _,
            hash_high as size_t,
            fmt_in,
        );
    }
    do_undump(
        &mut cs_count as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );

    /* font info */

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 7 {
        bad_fmt();
    }
    if x > sup_font_mem_size {
        panic!("must increase font_mem_size");
    }

    fmem_ptr = x;
    if fmem_ptr > font_mem_size {
        font_mem_size = fmem_ptr
    }
    font_info = xmalloc_array::<memory_word>(font_mem_size as usize);
    do_undump(
        &mut *font_info.offset(0) as *mut memory_word as *mut i8,
        ::std::mem::size_of::<memory_word>() as _,
        fmem_ptr as size_t,
        fmt_in,
    );
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < FONT_BASE {
        bad_fmt();
    }
    if x > FONT_BASE + MAX_FONT_MAX {
        panic!("must increase font_max");
    }

    font_ptr = x;
    font_mapping = xmalloc_array::<*mut libc::c_void>(font_max as usize);
    font_layout_engine = xcalloc_array::<*mut libc::c_void>(font_max as usize);
    font_flags = xmalloc_array(font_max as usize);
    font_letter_space = xmalloc_array(font_max as usize);
    font_check = xmalloc_array(font_max as usize);
    font_size = xmalloc_array(font_max as usize);
    font_dsize = xmalloc_array(font_max as usize);
    font_params = xmalloc_array(font_max as usize);
    font_name = xmalloc_array(font_max as usize);
    font_area = xmalloc_array(font_max as usize);
    font_bc = xmalloc_array(font_max as usize);
    font_ec = xmalloc_array(font_max as usize);
    font_glue = xmalloc_array(font_max as usize);
    hyphen_char = xmalloc_array(font_max as usize);
    skew_char = xmalloc_array(font_max as usize);
    bchar_label = xmalloc_array(font_max as usize);
    font_bchar = xmalloc_array(font_max as usize);
    font_false_bchar = xmalloc_array::<nine_bits>(font_max as usize);
    char_base = xmalloc_array(font_max as usize);
    width_base = xmalloc_array(font_max as usize);
    height_base = xmalloc_array(font_max as usize);
    depth_base = xmalloc_array(font_max as usize);
    italic_base = xmalloc_array(font_max as usize);
    lig_kern_base = xmalloc_array(font_max as usize);
    kern_base = xmalloc_array(font_max as usize);
    exten_base = xmalloc_array(font_max as usize);
    param_base = xmalloc_array(font_max as usize);

    k = 0i32;
    while k <= font_ptr {
        let ref mut fresh16 = *font_mapping.offset(k as isize);
        *fresh16 = 0 as *mut libc::c_void;
        k += 1
    }
    do_undump(
        &mut *font_check.offset(0) as *mut b16x4 as *mut i8,
        ::std::mem::size_of::<b16x4>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *font_size.offset(0) as *mut scaled_t as *mut i8,
        ::std::mem::size_of::<scaled_t>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *font_dsize.offset(0) as *mut scaled_t as *mut i8,
        ::std::mem::size_of::<scaled_t>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    let mut i_0: i32 = 0;
    do_undump(
        &mut *font_params.offset(0) as *mut font_index as *mut i8,
        ::std::mem::size_of::<font_index>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_0 = 0i32;
    while i_0 < font_ptr + 1i32 {
        if *(&mut *font_params.offset(0) as *mut font_index).offset(i_0 as isize) < TEX_NULL
            || *(&mut *font_params.offset(0) as *mut font_index).offset(i_0 as isize)
                > 0x3fffffffi32
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_0,
                *(&mut *font_params.offset(0) as *mut font_index).offset(i_0 as isize) as uintptr_t,
                &mut *font_params.offset(0) as *mut font_index as uintptr_t,
                TEX_NULL as uintptr_t,
                0x3fffffffi32 as uintptr_t
            );
        }
        i_0 += 1
    }
    do_undump(
        &mut *hyphen_char.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *skew_char.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    let mut i_1: i32 = 0;
    do_undump(
        &mut *font_name.offset(0) as *mut str_number as *mut i8,
        ::std::mem::size_of::<str_number>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_1 = 0i32;
    while i_1 < font_ptr + 1i32 {
        if *(&mut *font_name.offset(0) as *mut str_number).offset(i_1 as isize) > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_1,
                *(&mut *font_name.offset(0) as *mut str_number).offset(i_1 as isize) as uintptr_t,
                &mut *font_name.offset(0) as *mut str_number as uintptr_t,
                str_ptr as uintptr_t
            );
        }
        i_1 += 1
    }
    let mut i_2: i32 = 0;
    do_undump(
        &mut *font_area.offset(0) as *mut str_number as *mut i8,
        ::std::mem::size_of::<str_number>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_2 = 0i32;
    while i_2 < font_ptr + 1i32 {
        if *(&mut *font_area.offset(0) as *mut str_number).offset(i_2 as isize) > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_2,
                *(&mut *font_area.offset(0) as *mut str_number).offset(i_2 as isize) as uintptr_t,
                &mut *font_area.offset(0) as *mut str_number as uintptr_t,
                str_ptr as uintptr_t
            );
        }
        i_2 += 1
    }
    do_undump(
        &mut *font_bc.offset(0) as *mut UTF16_code as *mut i8,
        ::std::mem::size_of::<UTF16_code>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *font_ec.offset(0) as *mut UTF16_code as *mut i8,
        ::std::mem::size_of::<UTF16_code>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *char_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *width_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *height_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *depth_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *italic_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *lig_kern_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *kern_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *exten_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut *param_base.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    let mut i_3: i32 = 0;
    do_undump(
        &mut *font_glue.offset(0) as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_3 = 0i32;
    while i_3 < font_ptr + 1i32 {
        if *(&mut *font_glue.offset(0) as *mut i32).offset(i_3 as isize) < TEX_NULL
            || *(&mut *font_glue.offset(0) as *mut i32).offset(i_3 as isize) > lo_mem_max
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_3,
                *(&mut *font_glue.offset(0) as *mut i32).offset(i_3 as isize) as uintptr_t,
                &mut *font_glue.offset(0) as *mut i32 as uintptr_t,
                TEX_NULL as uintptr_t,
                lo_mem_max as uintptr_t
            );
        }
        i_3 += 1
    }
    let mut i_4: i32 = 0;
    do_undump(
        &mut *bchar_label.offset(0) as *mut font_index as *mut i8,
        ::std::mem::size_of::<font_index>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_4 = 0i32;
    while i_4 < font_ptr + 1i32 {
        if *(&mut *bchar_label.offset(0) as *mut font_index).offset(i_4 as isize) < 0i32
            || *(&mut *bchar_label.offset(0) as *mut font_index).offset(i_4 as isize)
                > fmem_ptr - 1i32
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_4,
                *(&mut *bchar_label.offset(0) as *mut font_index).offset(i_4 as isize) as uintptr_t,
                &mut *bchar_label.offset(0) as *mut font_index as uintptr_t,
                0i32 as uintptr_t,
                (fmem_ptr as uintptr_t).wrapping_sub(1i32 as u64)
            );
        }
        i_4 += 1
    }
    let mut i_5: i32 = 0;
    do_undump(
        &mut *font_bchar.offset(0) as *mut nine_bits as *mut i8,
        ::std::mem::size_of::<nine_bits>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_5 = 0i32;
    while i_5 < font_ptr + 1i32 {
        if *(&mut *font_bchar.offset(0) as *mut nine_bits).offset(i_5 as isize) < 0i32
            || *(&mut *font_bchar.offset(0) as *mut nine_bits).offset(i_5 as isize) > 65536i32
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_5,
                *(&mut *font_bchar.offset(0) as *mut nine_bits).offset(i_5 as isize) as uintptr_t,
                &mut *font_bchar.offset(0) as *mut nine_bits as uintptr_t,
                0i32 as uintptr_t,
                65536i32 as uintptr_t
            );
        }
        i_5 += 1
    }
    let mut i_6: i32 = 0;
    do_undump(
        &mut *font_false_bchar.offset(0) as *mut nine_bits as *mut i8,
        ::std::mem::size_of::<nine_bits>() as _,
        (font_ptr + 1i32) as size_t,
        fmt_in,
    );
    i_6 = 0i32;
    while i_6 < font_ptr + 1i32 {
        if *(&mut *font_false_bchar.offset(0) as *mut nine_bits).offset(i_6 as isize) < 0i32
            || *(&mut *font_false_bchar.offset(0) as *mut nine_bits).offset(i_6 as isize) > 65536i32
        {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_6,
                *(&mut *font_false_bchar.offset(0) as *mut nine_bits).offset(i_6 as isize)
                    as uintptr_t,
                &mut *font_false_bchar.offset(0) as *mut nine_bits as uintptr_t,
                0i32 as uintptr_t,
                65536i32 as uintptr_t
            );
        }
        i_6 += 1
    }

    /* hyphenations */

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 0 {
        bad_fmt();
    }

    if x > hyph_size {
        panic!("must increase hyph_size");
    }
    hyph_count = x;

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < HYPH_PRIME {
        bad_fmt();
    }
    if x > hyph_size {
        panic!("must increase hyph_size");
    }
    hyph_next = x;

    j = 0;

    for _k in 1..=hyph_count {
        do_undump(
            &mut j as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if j < 0i32 {
            bad_fmt();
        }
        if j > 65535 {
            hyph_next = (j as i64 / 65536) as i32;
            j = (j as i64 - hyph_next as i64 * 65536) as i32
        } else {
            hyph_next = 0
        }
        if j >= hyph_size || hyph_next > hyph_size {
            bad_fmt();
        }
        *hyph_link.offset(j as isize) = hyph_next as hyph_pointer;
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < 0 || x > str_ptr {
            bad_fmt();
        } else {
            *hyph_word.offset(j as isize) = x;
        }
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < MIN_HALFWORD || x > MAX_HALFWORD {
            bad_fmt();
        } else {
            *hyph_list.offset(j as isize) = x;
        }
    }
    j += 1;
    if j < HYPH_PRIME {
        j = HYPH_PRIME
    }

    hyph_next = j;
    if hyph_next >= hyph_size {
        hyph_next = HYPH_PRIME
    } else if hyph_next >= HYPH_PRIME {
        hyph_next += 1
    }
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 0 {
        bad_fmt();
    }
    if x > trie_size {
        panic!("must increase trie_size");
    }

    j = x;
    trie_max = j;

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 0 || x > j {
        bad_fmt();
    } else {
        hyph_start = x;
    }

    if trie_trl.is_null() {
        trie_trl = xmalloc_array(j as usize + 1);
    }
    do_undump(
        &mut *trie_trl.offset(0) as *mut trie_pointer as *mut i8,
        ::std::mem::size_of::<trie_pointer>() as _,
        (j + 1i32) as size_t,
        fmt_in,
    );
    if trie_tro.is_null() {
        trie_tro = xmalloc_array(j as usize + 1);
    }
    do_undump(
        &mut *trie_tro.offset(0) as *mut trie_pointer as *mut i8,
        ::std::mem::size_of::<trie_pointer>() as _,
        (j + 1i32) as size_t,
        fmt_in,
    );
    if trie_trc.is_null() {
        trie_trc = xmalloc_array(j as usize + 1);
    }
    do_undump(
        &mut *trie_trc.offset(0) as *mut u16 as *mut i8,
        ::std::mem::size_of::<u16>() as _,
        (j + 1i32) as size_t,
        fmt_in,
    );
    do_undump(
        &mut max_hyph_char as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x < 0 {
        bad_fmt();
    }
    if x > TRIE_OP_SIZE {
        panic!("must increase TRIE_OP_SIZE");
    }

    j = x;
    trie_op_ptr = j;

    do_undump(
        &mut *hyf_distance.as_mut_ptr().offset(1) as *mut small_number as *mut i8,
        ::std::mem::size_of::<small_number>() as _,
        j as size_t,
        fmt_in,
    );
    do_undump(
        &mut *hyf_num.as_mut_ptr().offset(1) as *mut small_number as *mut i8,
        ::std::mem::size_of::<small_number>() as _,
        j as size_t,
        fmt_in,
    );
    let mut i_7: i32 = 0;
    do_undump(
        &mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode as *mut i8,
        ::std::mem::size_of::<trie_opcode>() as _,
        j as size_t,
        fmt_in,
    );
    i_7 = 0i32;
    while i_7 < j {
        if *(&mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode).offset(i_7 as isize) as i64
            > 65535
        {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_7,
                *(&mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode).offset(i_7 as isize)
                    as uintptr_t,
                &mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode as uintptr_t,
                65535 as uintptr_t
            );
        }
        i_7 += 1
    }
    for k in 0..=BIGGEST_LANG {
        trie_used[k as usize] = 0;
    }
    k = BIGGEST_LANG + 1;
    loop {
        if !(j > 0) {
            break;
        }
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < 0i32 || x > k - 1i32 {
            bad_fmt();
        } else {
            k = x;
        }
        do_undump(
            &mut x as *mut i32 as *mut i8,
            ::std::mem::size_of::<i32>() as _,
            1i32 as size_t,
            fmt_in,
        );
        if x < 1 || x > j {
            bad_fmt();
        }
        trie_used[k as usize] = x as trie_opcode;
        j = j - x;
        op_start[k as usize] = j
    }
    trie_not_ready = false;

    /* trailer */

    do_undump(
        &mut x as *mut i32 as *mut i8,
        ::std::mem::size_of::<i32>() as _,
        1i32 as size_t,
        fmt_in,
    );
    if x != FORMAT_FOOTER_MAGIC {
        bad_fmt();
    }

    ttstub_input_close(fmt_in_owner);
    return true;
}

unsafe extern "C" fn final_cleanup() {
    let mut c: small_number = 0;
    c = cur_chr as small_number;
    if job_name == 0i32 {
        open_log_file();
    }
    while input_ptr > 0i32 {
        if cur_input.state as i32 == 0i32 {
            end_token_list();
        } else {
            end_file_reading();
        }
    }
    while open_parens > 0i32 {
        print_cstr(b" )");
        open_parens -= 1
    }
    if cur_level as i32 > 1i32 {
        print_nl('(' as i32);
        print_esc_cstr(b"end occurred ");
        print_cstr(b"inside a group at level ");
        print_int(cur_level as i32 - 1i32);
        print_char(')' as i32);
        show_save_groups();
    }
    while cond_ptr != TEX_NULL {
        print_nl('(' as i32);
        print_esc_cstr(b"end occurred ");
        print_cstr(b"when ");
        print_cmd_chr(107_u16, cur_if as i32);
        if if_line != 0i32 {
            print_cstr(b" on line ");
            print_int(if_line);
        }
        print_cstr(b" was incomplete)");
        if_line = (*mem.offset((cond_ptr + 1i32) as isize)).b32.s1;
        cur_if = (*mem.offset(cond_ptr as isize)).b16.s0 as small_number;
        temp_ptr = cond_ptr;
        cond_ptr = (*mem.offset(cond_ptr as isize)).b32.s1;
        free_node(temp_ptr, 2i32);
    }
    if history != TTHistory::SPOTLESS {
        if history == TTHistory::WARNING_ISSUED || (interaction as i32) < 3i32 {
            if selector == Selector::TERM_AND_LOG {
                selector = Selector::TERM_ONLY;
                print_nl_cstr(b"(see the transcript file for additional information)");
                selector = Selector::TERM_AND_LOG
            }
        }
    }
    if c as i32 == 1i32 {
        if in_initex_mode {
            let mut for_end: i32 = 0;
            c = 0i32 as small_number;
            for_end = 4i32;
            if c as i32 <= for_end {
                loop {
                    if cur_mark[c as usize] != TEX_NULL {
                        delete_token_ref(cur_mark[c as usize]);
                    }
                    let fresh17 = c;
                    c = c + 1;
                    if !((fresh17 as i32) < for_end) {
                        break;
                    }
                }
            }
            if sa_root[7] != TEX_NULL {
                if do_marks(3i32 as small_number, 0i32 as small_number, sa_root[7]) {
                    sa_root[7] = TEX_NULL
                }
            }
            let mut for_end_0: i32 = 0;
            c = 2i32 as small_number;
            for_end_0 = 3i32;
            if c as i32 <= for_end_0 {
                loop {
                    flush_node_list(disc_ptr[c as usize]);
                    let fresh18 = c;
                    c = c + 1;
                    if !((fresh18 as i32) < for_end_0) {
                        break;
                    }
                }
            }
            if last_glue != 0x3fffffffi32 {
                delete_glue_ref(last_glue);
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
unsafe extern "C" fn init_io() {
    /* This is largely vestigial at this point */
    stdin_ufile.handle = None;
    stdin_ufile.savedChar = -1i32 as i64;
    stdin_ufile.skipNextLF = 0_i16;
    stdin_ufile.encodingMode = 1_i16;
    stdin_ufile.conversionData = 0 as *mut libc::c_void;
    let ref mut fresh19 = *input_file.offset(0);
    *fresh19 = &mut stdin_ufile;
    *buffer.offset(first as isize) = 0i32;
    last = first;
    cur_input.loc = first;
    cur_input.limit = last;
    first = last + 1i32;
}
unsafe extern "C" fn initialize_more_variables() {
    let mut k: i32 = 0;
    let mut z: hyph_pointer = 0;
    doing_special = false;
    native_text_size = 128i32;
    native_text = xmalloc(
        (native_text_size as u64).wrapping_mul(::std::mem::size_of::<UTF16_code>() as _) as _,
    ) as *mut UTF16_code;
    interaction = 3_u8;
    deletions_allowed = true;
    set_box_allowed = true;
    error_count = 0_i8;
    help_ptr = 0_u8;
    use_err_help = false;
    nest_ptr = 0i32;
    max_nest_stack = 0i32;
    cur_list.mode = VMODE as _;
    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    cur_list.eTeX_aux = TEX_NULL;
    cur_list.aux.b32.s1 = IGNORE_DEPTH;
    cur_list.mode_line = 0i32;
    cur_list.prev_graf = 0i32;
    shown_mode = 0_i16;
    page_contents = EMPTY as _;
    page_tail = PAGE_HEAD;
    last_glue = MAX_HALFWORD;
    last_penalty = 0i32;
    last_kern = 0i32;
    page_so_far[7] = 0i32;
    k = INT_BASE;
    while k <= EQTB_SIZE {
        _xeq_level_array[(k
            - (1i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + 1i32
                + 15000i32
                + 12i32
                + 9000i32
                + 1i32
                + 1i32
                + 19i32
                + 256i32
                + 256i32
                + 13i32
                + 256i32
                + 4i32
                + 256i32
                + 1i32
                + 3i32 * 256i32
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32)
                + (0x10ffffi32 + 1i32))) as usize] = 1_u16;
        k += 1
    }
    no_new_control_sequence = true;
    prim[0].s0 = 0i32;
    prim[0].s1 = 0i32;
    k = 1i32;
    while k <= 500i32 {
        prim[k as usize] = prim[0];
        k += 1
    }
    prim_eqtb[0].b16.s0 = 0_u16;
    prim_eqtb[0].b16.s1 = 103_u16;
    prim_eqtb[0].b32.s1 = TEX_NULL;
    k = 1i32;
    while k <= 500i32 {
        prim_eqtb[k as usize] = prim_eqtb[0];
        k += 1
    }
    save_ptr = 0i32;
    cur_level = 1_u16;
    cur_group = 0i32 as group_code;
    cur_boundary = 0i32;
    max_save_stack = 0i32;
    mag_set = 0i32;
    expand_depth_count = 0i32;
    is_in_csname = false;
    cur_mark[0] = TEX_NULL;
    cur_mark[1] = TEX_NULL;
    cur_mark[2] = TEX_NULL;
    cur_mark[3] = TEX_NULL;
    cur_mark[4] = TEX_NULL;
    cur_val = 0i32;
    cur_val_level = 0_u8;
    radix = 0i32 as small_number;
    cur_order = 0i32 as glue_ord;
    k = 0i32;
    while k <= 16i32 {
        read_open[k as usize] = 2_u8;
        k += 1
    }
    cond_ptr = TEX_NULL;
    if_limit = 0_u8;
    cur_if = 0i32 as small_number;
    if_line = 0i32;
    null_character.s3 = 0_u16;
    null_character.s2 = 0_u16;
    null_character.s1 = 0_u16;
    null_character.s0 = 0_u16;
    total_pages = 0i32;
    max_v = 0i32;
    max_h = 0i32;
    max_push = 0i32;
    last_bop = -1i32;
    doing_leaders = false;
    dead_cycles = 0i32;
    adjust_tail = TEX_NULL;
    last_badness = 0i32;
    pre_adjust_tail = TEX_NULL;
    pack_begin_line = 0i32;
    empty.s1 = 0i32;
    empty.s0 = TEX_NULL;
    align_ptr = TEX_NULL;
    cur_align = TEX_NULL;
    cur_span = TEX_NULL;
    cur_loop = TEX_NULL;
    cur_head = TEX_NULL;
    cur_tail = TEX_NULL;
    cur_pre_head = TEX_NULL;
    cur_pre_tail = TEX_NULL;
    cur_f = 0i32;
    max_hyph_char = 256i32;
    z = 0i32 as hyph_pointer;
    while z as i32 <= hyph_size {
        *hyph_word.offset(z as isize) = 0i32;
        *hyph_list.offset(z as isize) = TEX_NULL;
        *hyph_link.offset(z as isize) = 0i32 as hyph_pointer;
        z = z.wrapping_add(1)
    }
    hyph_count = 0i32;
    hyph_next = 607i32 + 1i32;
    if hyph_next > hyph_size {
        hyph_next = 607i32
    }
    output_active = false;
    insert_penalties = 0i32;
    ligature_present = false;
    cancel_boundary = false;
    lft_hit = false;
    rt_hit = false;
    ins_disc = false;
    after_token = 0i32;
    long_help_seen = false;
    format_ident = 0i32;
    k = 0i32;
    while k <= 17i32 {
        write_open[k as usize] = false;
        k += 1
    }
    LR_ptr = TEX_NULL;
    LR_problems = 0i32;
    cur_dir = 0i32 as small_number;
    pseudo_files = TEX_NULL;
    sa_root[7] = TEX_NULL;
    sa_null.b32.s0 = TEX_NULL;
    sa_null.b32.s1 = TEX_NULL;
    sa_chain = TEX_NULL;
    sa_level = 0_u16;
    disc_ptr[2] = TEX_NULL;
    disc_ptr[3] = TEX_NULL;
    edit_name_start = 0i32;
    stop_at_space = true;
}
unsafe extern "C" fn initialize_more_initex_variables() {
    let mut i: i32 = 0;
    let mut k: i32 = 0;
    k = 1i32;
    while k <= 19i32 {
        (*mem.offset(k as isize)).b32.s1 = 0i32;
        k += 1
    }
    k = 0i32;
    while k <= 19i32 {
        (*mem.offset(k as isize)).b32.s1 = TEX_NULL + 1i32;
        (*mem.offset(k as isize)).b16.s1 = 0_u16;
        (*mem.offset(k as isize)).b16.s0 = 0_u16;
        k += 4i32
    }
    (*mem.offset(6)).b32.s1 = 65536 as i32;
    (*mem.offset(4)).b16.s1 = 1_u16;
    (*mem.offset(10)).b32.s1 = 65536 as i32;
    (*mem.offset(8)).b16.s1 = 2_u16;
    (*mem.offset(14)).b32.s1 = 65536 as i32;
    (*mem.offset(12)).b16.s1 = 1_u16;
    (*mem.offset(15)).b32.s1 = 65536 as i32;
    (*mem.offset(12)).b16.s0 = 1_u16;
    (*mem.offset(18)).b32.s1 = -65536 as i32;
    (*mem.offset(16)).b16.s1 = 1_u16;
    rover = 20i32;
    (*mem.offset(rover as isize)).b32.s1 = 0x3fffffffi32;
    (*mem.offset(rover as isize)).b32.s0 = 1000i32;
    (*mem.offset((rover + 1i32) as isize)).b32.s0 = rover;
    (*mem.offset((rover + 1i32) as isize)).b32.s1 = rover;
    lo_mem_max = rover + 1000i32;
    (*mem.offset(lo_mem_max as isize)).b32.s1 = TEX_NULL;
    (*mem.offset(lo_mem_max as isize)).b32.s0 = TEX_NULL;
    k = PRE_ADJUST_HEAD;
    while k <= MEM_TOP {
        *mem.offset(k as isize) = *mem.offset(lo_mem_max as isize);
        k += 1
    }
    (*mem.offset((OMIT_TEMPLATE) as isize)).b32.s0 = CS_TOKEN_FLAG + FROZEN_END_TEMPLATE;
    (*mem.offset(END_SPAN as isize)).b32.s1 = std::u16::MAX as i32 + 1;
    (*mem.offset(END_SPAN as isize)).b32.s0 = TEX_NULL;
    (*mem.offset(ACTIVE_LIST as isize)).b16.s1 = HYPHENATED as _;
    (*mem.offset((ACTIVE_LIST + 1) as isize)).b32.s0 = MAX_HALFWORD;
    (*mem.offset(ACTIVE_LIST as isize)).b16.s0 = 0_u16;
    (*mem.offset(PAGE_INS_HEAD as isize)).b16.s0 = 255_u16;
    (*mem.offset(PAGE_INS_HEAD as isize)).b16.s1 = SPLIT_UP as _;
    (*mem.offset(PAGE_INS_HEAD as isize)).b32.s1 = PAGE_INS_HEAD;
    (*mem.offset((4999999i32 - 2i32) as isize)).b16.s1 = 10_u16;
    (*mem.offset((4999999i32 - 2i32) as isize)).b16.s0 = 0_u16;
    avail = TEX_NULL;
    mem_end = MEM_TOP;
    hi_mem_min = PRE_ADJUST_HEAD;
    var_used = 20;
    dyn_used = HI_MEM_STAT_USAGE;
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b16.s1 = UNDEFINED_CS as _;
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b32.s1 = TEX_NULL;
    (*eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize)).b16.s0 = LEVEL_ZERO as _;
    k = ACTIVE_BASE;
    while k <= eqtb_top {
        *eqtb.offset(k as isize) = *eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize);
        k += 1
    }
    (*eqtb.offset(GLUE_BASE as isize)).b32.s1 = 0;
    (*eqtb.offset(GLUE_BASE as isize)).b16.s0 = LEVEL_ONE as _;
    (*eqtb.offset(GLUE_BASE as isize)).b16.s1 = GLUE_REF as _;
    k = GLUE_BASE;
    while k <= LOCAL_BASE {
        *eqtb.offset(k as isize) = *eqtb.offset(GLUE_BASE as isize);
        k += 1
    }
    let ref mut fresh20 = (*mem.offset(0)).b32.s1;
    *fresh20 += 531i32;
    LOCAL_set(LOCAL__par_shape, TEX_NULL);
    (*eqtb.offset(LOCAL_BASE as isize + LOCAL__par_shape as isize))
        .b16
        .s1 = SHAPE_REF as _;
    (*eqtb.offset(LOCAL_BASE as isize + LOCAL__par_shape as isize))
        .b16
        .s0 = LEVEL_ONE as _;
    k = ETEX_PEN_BASE;
    while k <= ETEX_PENS - 1 {
        *eqtb.offset(k as isize) = *eqtb.offset(LOCAL_BASE as isize + LOCAL__par_shape as isize);
        k += 1
    }
    k = LOCAL_BASE + LOCAL__output_routine;
    while k <= TOKS_BASE + NUMBER_REGS - 1 {
        *eqtb.offset(k as isize) = *eqtb.offset(UNDEFINED_CONTROL_SEQUENCE as isize);
        k += 1
    }
    (*eqtb.offset(BOX_BASE as isize)).b32.s1 = TEX_NULL;
    (*eqtb.offset(BOX_BASE as isize)).b16.s1 = BOX_REF as _;
    (*eqtb.offset(BOX_BASE as isize)).b16.s0 = LEVEL_ONE as _;
    k = BOX_BASE + 1;
    while k <= BOX_BASE + NUMBER_REGS - 1 {
        *eqtb.offset(k as isize) = *eqtb.offset(BOX_BASE as _);
        k += 1
    }
    (*eqtb.offset(CUR_FONT_LOC as isize)).b32.s1 = FONT_BASE;
    (*eqtb.offset(CUR_FONT_LOC as isize)).b16.s1 = DATA as _;
    (*eqtb.offset(CUR_FONT_LOC as isize)).b16.s0 = LEVEL_ONE as _;
    k = MATH_FONT_BASE;
    while k <= MATH_FONT_BASE + NUMBER_MATH_FONTS - 1 {
        *eqtb.offset(k as isize) = *eqtb.offset(CUR_FONT_LOC as isize);
        k += 1
    }
    (*eqtb.offset(CAT_CODE_BASE as isize)).b32.s1 = 0;
    (*eqtb.offset(CAT_CODE_BASE as isize)).b16.s1 = DATA as _;
    (*eqtb.offset(CAT_CODE_BASE as isize)).b16.s0 = LEVEL_ONE as _;
    k = CAT_CODE_BASE + 1;
    while k <= INT_BASE - 1 {
        *eqtb.offset(k as isize) = *eqtb.offset(CAT_CODE_BASE as isize);
        k += 1
    }
    k = 0;
    while k <= NUMBER_USVS - 1 {
        CAT_CODE_set(k, OTHER_CHAR as _);
        MATH_CODE_set(k, k);
        SF_CODE_set(k, 1000);
        k += 1
    }
    CAT_CODE_set(13, CAR_RET as _);
    CAT_CODE_set(32, SPACER as _);
    CAT_CODE_set(92, ESCAPE as _);
    CAT_CODE_set(37, COMMENT as _);
    CAT_CODE_set(127, INVALID_CHAR as _);
    (*eqtb.offset(CAT_CODE_BASE as isize)).b32.s1 = IGNORE as _;
    k = '0' as i32;
    while k <= '9' as i32 {
        MATH_CODE_set(
            k,
            (k as u32).wrapping_add((7_u32 & 0x7_u32) << 21i32) as i32,
        );
        k += 1
    }
    k = 'A' as i32;
    while k <= 'Z' as i32 {
        CAT_CODE_set(k, LETTER as _);
        CAT_CODE_set(k + 32, LETTER as _);
        MATH_CODE_set(
            k,
            (k as u32)
                .wrapping_add((1_u32 & 0xff_u32) << 24i32)
                .wrapping_add((7_u32 & 0x7_u32) << 21i32) as i32,
        );
        MATH_CODE_set(
            k + 32,
            ((k + 32i32) as u32)
                .wrapping_add((1_u32 & 0xff_u32) << 24i32)
                .wrapping_add((7_u32 & 0x7_u32) << 21i32) as i32,
        );
        LC_CODE_set(k, k + 32);
        LC_CODE_set(k + 32, k + 32);
        UC_CODE_set(k, k);
        UC_CODE_set(k + 32, k);
        SF_CODE_set(k, 999);
        k += 1
    }
    k = INT_BASE;
    while k <= DEL_CODE_BASE - 1 {
        (*eqtb.offset(k as isize)).b32.s1 = 0;
        k += 1
    }
    INTPAR_set(INT_PAR__char_sub_def_min, 256);
    INTPAR_set(INT_PAR__char_sub_def_max, -1);
    INTPAR_set(INT_PAR__mag, 1000);
    INTPAR_set(INT_PAR__tolerance, 10_000);
    INTPAR_set(INT_PAR__hang_after, 1);
    INTPAR_set(INT_PAR__max_dead_cycles, 25);
    INTPAR_set(INT_PAR__escape_char, '\\' as i32);
    INTPAR_set(INT_PAR__end_line_char, CARRIAGE_RETURN);
    k = 0;
    while k <= NUMBER_USVS - 1i32 {
        DEL_CODE_set(k, -1);
        k += 1
    }
    DEL_CODE_set(46, 0);
    k = DIMEN_BASE;
    while k <= EQTB_SIZE {
        (*eqtb.offset(k as isize)).b32.s1 = 0i32;
        k += 1
    }
    prim_used = PRIM_SIZE;
    hash_used = FROZEN_CONTROL_SEQUENCE;
    hash_high = 0;
    cs_count = 0;
    (*eqtb.offset(FROZEN_DONT_EXPAND as isize)).b16.s1 = DONT_EXPAND as _;
    (*hash.offset(FROZEN_DONT_EXPAND as isize)).s1 = maketexstring(b"notexpanded:");
    (*eqtb.offset(FROZEN_PRIMITIVE as isize)).b16.s1 = IGNORE_SPACES;
    (*eqtb.offset(FROZEN_PRIMITIVE as isize)).b32.s1 = 1;
    (*eqtb.offset(FROZEN_PRIMITIVE as isize)).b16.s0 = LEVEL_ONE as _;
    (*hash.offset(FROZEN_PRIMITIVE as isize)).s1 = maketexstring(b"primitive");
    k = -(35111 as i32);
    while k as i64 <= 35111 {
        _trie_op_hash_array[(k as i64 - -35111) as usize] = 0i32;
        k += 1
    }
    k = 0i32;
    while k <= 255i32 {
        trie_used[k as usize] = 0i32 as trie_opcode;
        k += 1
    }
    max_op_used = 0i32 as trie_opcode;
    trie_op_ptr = 0i32;
    trie_not_ready = true;
    (*hash.offset(FROZEN_PROTECTION as isize)).s1 = maketexstring(b"inaccessible");
    format_ident = maketexstring(b" (INITEX)");
    (*hash.offset(END_WRITE as isize)).s1 = maketexstring(b"endwrite");
    (*eqtb.offset(END_WRITE as isize)).b16.s0 = LEVEL_ONE as _;
    (*eqtb.offset(END_WRITE as isize)).b16.s1 = OUTER_CALL as _;
    (*eqtb.offset(END_WRITE as isize)).b32.s1 = TEX_NULL;
    max_reg_num = 32767;
    max_reg_help_line = b"A register number must be between 0 and 32767.";
    i = INT_VAL;
    while i <= INTER_CHAR_VAL {
        sa_root[i as usize] = TEX_NULL;
        i += 1
    }
    INTPAR_set(INT_PAR__xetex_hyphenatable_length, 63);
}
/*:1370*/
/*1371: */
unsafe extern "C" fn initialize_primitives() {
    no_new_control_sequence = false;
    first = 0i32;
    primitive(b"lineskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__line_skip);
    primitive(
        b"baselineskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__baseline_skip,
    );
    primitive(b"parskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__par_skip);
    primitive(
        b"abovedisplayskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__above_display_skip,
    );
    primitive(
        b"belowdisplayskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__below_display_skip,
    );
    primitive(
        b"abovedisplayshortskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__above_display_short_skip,
    );
    primitive(
        b"belowdisplayshortskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__below_display_short_skip,
    );
    primitive(b"leftskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__left_skip);
    primitive(b"rightskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__right_skip);
    primitive(b"topskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__top_skip);
    primitive(
        b"splittopskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__split_top_skip,
    );
    primitive(b"tabskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__tab_skip);
    primitive(b"spaceskip", ASSIGN_GLUE, GLUE_BASE + GLUE_PAR__space_skip);
    primitive(
        b"xspaceskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__xspace_skip,
    );
    primitive(
        b"parfillskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__par_fill_skip,
    );
    primitive(
        b"XeTeXlinebreakskip",
        ASSIGN_GLUE,
        GLUE_BASE + GLUE_PAR__xetex_linebreak_skip,
    );

    primitive(
        b"thinmuskip",
        ASSIGN_MU_GLUE,
        GLUE_BASE + GLUE_PAR__thin_mu_skip,
    );
    primitive(
        b"medmuskip",
        ASSIGN_MU_GLUE,
        GLUE_BASE + GLUE_PAR__med_mu_skip,
    );
    primitive(
        b"thickmuskip",
        ASSIGN_MU_GLUE,
        GLUE_BASE + GLUE_PAR__thick_mu_skip,
    );

    primitive(
        b"output",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 1i32,
    );
    primitive(
        b"everypar",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 2i32,
    );
    primitive(
        b"everymath",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 3i32,
    );
    primitive(
        b"everydisplay",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 4i32,
    );
    primitive(
        b"everyhbox",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 5i32,
    );
    primitive(
        b"everyvbox",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 6i32,
    );
    primitive(
        b"everyjob",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 7i32,
    );
    primitive(
        b"everycr",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 8i32,
    );
    primitive(
        b"errhelp",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 9i32,
    );
    primitive(
        b"everyeof",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 10i32,
    );
    primitive(
        b"XeTeXinterchartoks",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 11i32,
    );
    primitive(
        b"TectonicCodaTokens",
        73_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 12i32,
    );
    primitive(
        b"pretolerance",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 0i32,
    );
    primitive(
        b"tolerance",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32,
    );
    primitive(
        b"linepenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 2i32,
    );
    primitive(
        b"hyphenpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 3i32,
    );
    primitive(
        b"exhyphenpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 4i32,
    );
    primitive(
        b"clubpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 5i32,
    );
    primitive(
        b"widowpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 6i32,
    );
    primitive(
        b"displaywidowpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 7i32,
    );
    primitive(
        b"brokenpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 8i32,
    );
    primitive(
        b"binoppenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 9i32,
    );
    primitive(
        b"relpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 10i32,
    );
    primitive(
        b"predisplaypenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 11i32,
    );
    primitive(
        b"postdisplaypenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 12i32,
    );
    primitive(
        b"interlinepenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 13i32,
    );
    primitive(
        b"doublehyphendemerits",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 14i32,
    );
    primitive(
        b"finalhyphendemerits",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 15i32,
    );
    primitive(
        b"adjdemerits",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 16i32,
    );
    primitive(
        b"mag",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 17i32,
    );
    primitive(
        b"delimiterfactor",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 18i32,
    );
    primitive(
        b"looseness",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 19i32,
    );
    primitive(
        b"time",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 20i32,
    );
    primitive(
        b"day",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 21i32,
    );
    primitive(
        b"month",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 22i32,
    );
    primitive(
        b"year",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 23i32,
    );
    primitive(
        b"showboxbreadth",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 24i32,
    );
    primitive(
        b"showboxdepth",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 25i32,
    );
    primitive(
        b"hbadness",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 26i32,
    );
    primitive(
        b"vbadness",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 27i32,
    );
    primitive(
        b"pausing",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 28i32,
    );
    primitive(
        b"tracingonline",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 29i32,
    );
    primitive(
        b"tracingmacros",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 30i32,
    );
    primitive(
        b"tracingstats",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 31i32,
    );
    primitive(
        b"tracingparagraphs",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 32i32,
    );
    primitive(
        b"tracingpages",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 33i32,
    );
    primitive(
        b"tracingoutput",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 34i32,
    );
    primitive(
        b"tracinglostchars",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 35i32,
    );
    primitive(
        b"tracingcommands",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 36i32,
    );
    primitive(
        b"tracingrestores",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 37i32,
    );
    primitive(
        b"uchyph",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 38i32,
    );
    primitive(
        b"outputpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 39i32,
    );
    primitive(
        b"maxdeadcycles",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 40i32,
    );
    primitive(
        b"hangafter",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 41i32,
    );
    primitive(
        b"floatingpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 42i32,
    );
    primitive(
        b"globaldefs",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 43i32,
    );
    primitive(
        b"fam",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 44i32,
    );
    primitive(
        b"escapechar",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 45i32,
    );
    primitive(
        b"defaulthyphenchar",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 46i32,
    );
    primitive(
        b"defaultskewchar",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 47i32,
    );
    primitive(
        b"endlinechar",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 48i32,
    );
    primitive(
        b"newlinechar",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 49i32,
    );
    primitive(
        b"language",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 50i32,
    );
    primitive(
        b"lefthyphenmin",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 51i32,
    );
    primitive(
        b"righthyphenmin",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 52i32,
    );
    primitive(
        b"holdinginserts",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 53i32,
    );
    primitive(
        b"errorcontextlines",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 54i32,
    );
    primitive(
        b"XeTeXlinebreakpenalty",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 69i32,
    );
    primitive(
        b"XeTeXprotrudechars",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 70i32,
    );
    primitive(
        b"parindent",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 0i32,
    );
    primitive(
        b"mathsurround",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 1i32,
    );
    primitive(
        b"lineskiplimit",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 2i32,
    );
    primitive(
        b"hsize",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 3i32,
    );
    primitive(
        b"vsize",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 4i32,
    );
    primitive(
        b"maxdepth",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 5i32,
    );
    primitive(
        b"splitmaxdepth",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 6i32,
    );
    primitive(
        b"boxmaxdepth",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 7i32,
    );
    primitive(
        b"hfuzz",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 8i32,
    );
    primitive(
        b"vfuzz",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 9i32,
    );
    primitive(
        b"delimitershortfall",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 10i32,
    );
    primitive(
        b"nulldelimiterspace",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 11i32,
    );
    primitive(
        b"scriptspace",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 12i32,
    );
    primitive(
        b"predisplaysize",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 13i32,
    );
    primitive(
        b"displaywidth",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 14i32,
    );
    primitive(
        b"displayindent",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 15i32,
    );
    primitive(
        b"overfullrule",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 16i32,
    );
    primitive(
        b"hangindent",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 17i32,
    );
    primitive(
        b"hoffset",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 18i32,
    );
    primitive(
        b"voffset",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 19i32,
    );
    primitive(
        b"emergencystretch",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 20i32,
    );
    primitive(
        b"pdfpagewidth",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 21i32,
    );
    primitive(
        b"pdfpageheight",
        75_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + (0x10ffffi32 + 1i32)
            + 22i32,
    );
    primitive(b" ", 64_u16, 0i32);
    primitive(b"/", 44_u16, 0i32);
    primitive(b"accent", 45_u16, 0i32);
    primitive(b"advance", 92_u16, 0i32);
    primitive(b"afterassignment", 40_u16, 0i32);
    primitive(b"aftergroup", 41_u16, 0i32);
    primitive(b"begingroup", 61_u16, 0i32);
    primitive(b"char", 16_u16, 0i32);
    primitive(b"csname", 109_u16, 0i32);
    primitive(b"delimiter", 15_u16, 0i32);
    primitive(b"XeTeXdelimiter", 15_u16, 1i32);
    primitive(b"Udelimiter", 15_u16, 1i32);
    primitive(b"divide", 94_u16, 0i32);
    primitive(b"endcsname", 67_u16, 0i32);
    primitive(b"endgroup", 62_u16, 0i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 2i32) as isize,
    ))
    .s1 = maketexstring(b"endgroup");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 2i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"expandafter", 104_u16, 0i32);
    primitive(b"font", 90_u16, 0i32);
    primitive(b"fontdimen", 78_u16, 0i32);
    primitive(b"halign", 32_u16, 0i32);
    primitive(b"hrule", 36_u16, 0i32);
    primitive(b"ignorespaces", 39_u16, 0i32);
    primitive(b"insert", 37_u16, 0i32);
    primitive(b"mark", 18_u16, 0i32);
    primitive(b"mathaccent", 46_u16, 0i32);
    primitive(b"XeTeXmathaccent", 46_u16, 1i32);
    primitive(b"Umathaccent", 46_u16, 1i32);
    primitive(b"mathchar", 17_u16, 0i32);
    primitive(b"XeTeXmathcharnum", 17_u16, 1i32);
    primitive(b"Umathcharnum", 17_u16, 1i32);
    primitive(b"XeTeXmathchar", 17_u16, 2i32);
    primitive(b"Umathchar", 17_u16, 2i32);
    primitive(b"mathchoice", 54_u16, 0i32);
    primitive(b"multiply", 93_u16, 0i32);
    primitive(b"noalign", 34_u16, 0i32);
    primitive(b"noboundary", 65_u16, 0i32);
    primitive(b"noexpand", 105_u16, 0i32);
    primitive(b"primitive", 105_u16, 1i32);
    primitive(b"nonscript", 55_u16, 0i32);
    primitive(b"omit", 63_u16, 0i32);
    primitive(
        b"parshape",
        85_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 0i32,
    );
    primitive(b"penalty", 42_u16, 0i32);
    primitive(b"prevgraf", 81_u16, 0i32);
    primitive(b"radical", 66_u16, 0i32);
    primitive(b"XeTeXradical", 66_u16, 1i32);
    primitive(b"Uradical", 66_u16, 1i32);
    primitive(b"read", 98_u16, 0i32);
    primitive(b"relax", 0_u16, 0x10ffffi32 + 1i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 7i32) as isize,
    ))
    .s1 = maketexstring(b"relax");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 7i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"setbox", 100_u16, 0i32);
    primitive(b"the", 111_u16, 0i32);
    primitive(b"toks", 72_u16, 0i32);
    primitive(b"vadjust", 38_u16, 0i32);
    primitive(b"valign", 33_u16, 0i32);
    primitive(b"vcenter", 56_u16, 0i32);
    primitive(b"vrule", 35_u16, 0i32);
    primitive(b"par", 13_u16, 0x10ffffi32 + 1i32);
    par_loc = cur_val;
    par_token = 0x1ffffffi32 + par_loc;
    primitive(b"input", 106_u16, 0i32);
    primitive(b"endinput", 106_u16, 1i32);
    primitive(b"topmark", 112_u16, 0i32);
    primitive(b"firstmark", 112_u16, 1i32);
    primitive(b"botmark", 112_u16, 2i32);
    primitive(b"splitfirstmark", 112_u16, 3i32);
    primitive(b"splitbotmark", 112_u16, 4i32);
    primitive(b"count", 91_u16, 0i32);
    primitive(b"dimen", 91_u16, 1i32);
    primitive(b"skip", 91_u16, 2i32);
    primitive(b"muskip", 91_u16, 3i32);
    primitive(b"spacefactor", 80_u16, 104i32);
    primitive(b"prevdepth", 80_u16, 1i32);
    primitive(b"deadcycles", 83_u16, 0i32);
    primitive(b"insertpenalties", 83_u16, 1i32);
    primitive(b"wd", 84_u16, 1i32);
    primitive(b"ht", 84_u16, 3i32);
    primitive(b"dp", 84_u16, 2i32);
    primitive(b"lastpenalty", 71_u16, 0i32);
    primitive(b"lastkern", 71_u16, 1i32);
    primitive(b"lastskip", 71_u16, 2i32);
    primitive(b"inputlineno", 71_u16, 4i32);
    primitive(b"badness", 71_u16, 5i32);
    primitive(b"number", 110_u16, 0i32);
    primitive(b"romannumeral", 110_u16, 1i32);
    primitive(b"string", 110_u16, 2i32);
    primitive(b"meaning", 110_u16, 3i32);
    primitive(b"fontname", 110_u16, 4i32);
    primitive(b"jobname", 110_u16, 15i32);
    primitive(b"leftmarginkern", 110_u16, 11i32);
    primitive(b"rightmarginkern", 110_u16, 12i32);
    primitive(b"Uchar", 110_u16, 13i32);
    primitive(b"Ucharcat", 110_u16, 14i32);
    primitive(b"if", 107_u16, 0i32);
    primitive(b"ifcat", 107_u16, 1i32);
    primitive(b"ifnum", 107_u16, 2i32);
    primitive(b"ifdim", 107_u16, 3i32);
    primitive(b"ifodd", 107_u16, 4i32);
    primitive(b"ifvmode", 107_u16, 5i32);
    primitive(b"ifhmode", 107_u16, 6i32);
    primitive(b"ifmmode", 107_u16, 7i32);
    primitive(b"ifinner", 107_u16, 8i32);
    primitive(b"ifvoid", 107_u16, 9i32);
    primitive(b"ifhbox", 107_u16, 10i32);
    primitive(b"ifvbox", 107_u16, 11i32);
    primitive(b"ifx", 107_u16, 12i32);
    primitive(b"ifeof", 107_u16, 13i32);
    primitive(b"iftrue", 107_u16, 14i32);
    primitive(b"iffalse", 107_u16, 15i32);
    primitive(b"ifcase", 107_u16, 16i32);
    primitive(b"ifprimitive", 107_u16, 21i32);
    primitive(b"fi", 108_u16, 2i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 4i32) as isize,
    ))
    .s1 = maketexstring(b"fi");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 4i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"or", 108_u16, 4i32);
    primitive(b"else", 108_u16, 3i32);
    primitive(b"nullfont", 89_u16, 0i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 12i32) as isize,
    ))
    .s1 = maketexstring(b"nullfont");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 12i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"span", 4_u16, 0x10ffffi32 + 2i32);
    primitive(b"cr", 5_u16, 0x10ffffi32 + 3i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 1i32) as isize,
    ))
    .s1 = maketexstring(b"cr");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 1i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"crcr", 5_u16, 0x10ffffi32 + 4i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 5i32) as isize,
    ))
    .s1 = maketexstring(b"endtemplate");
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 6i32) as isize,
    ))
    .s1 = maketexstring(b"endtemplate");
    (*eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 6i32) as isize,
    ))
    .b16
    .s1 = 9_u16;
    (*eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 6i32) as isize,
    ))
    .b32
    .s1 = 4999999i32 - 11i32;
    (*eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 6i32) as isize,
    ))
    .b16
    .s0 = 1_u16;
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 5i32) as isize,
    ) = *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 6i32) as isize,
    );
    (*eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 5i32) as isize,
    ))
    .b16
    .s1 = 117_u16;
    primitive(b"pagegoal", 82_u16, 0i32);
    primitive(b"pagetotal", 82_u16, 1i32);
    primitive(b"pagestretch", 82_u16, 2i32);
    primitive(b"pagefilstretch", 82_u16, 3i32);
    primitive(b"pagefillstretch", 82_u16, 4i32);
    primitive(b"pagefilllstretch", 82_u16, 5i32);
    primitive(b"pageshrink", 82_u16, 6i32);
    primitive(b"pagedepth", 82_u16, 7i32);
    primitive(b"end", 14_u16, 0i32);
    primitive(b"dump", 14_u16, 1i32);
    primitive(b"hskip", 26_u16, 4i32);
    primitive(b"hfil", 26_u16, 0i32);
    primitive(b"hfill", 26_u16, 1i32);
    primitive(b"hss", 26_u16, 2i32);
    primitive(b"hfilneg", 26_u16, 3i32);
    primitive(b"vskip", 27_u16, 4i32);
    primitive(b"vfil", 27_u16, 0i32);
    primitive(b"vfill", 27_u16, 1i32);
    primitive(b"vss", 27_u16, 2i32);
    primitive(b"vfilneg", 27_u16, 3i32);
    primitive(b"mskip", 28_u16, 5i32);
    primitive(b"kern", 29_u16, 1i32);
    primitive(b"mkern", 30_u16, 99i32);
    primitive(b"moveleft", 21_u16, 1i32);
    primitive(b"moveright", 21_u16, 0i32);
    primitive(b"raise", 22_u16, 1i32);
    primitive(b"lower", 22_u16, 0i32);
    primitive(b"box", 20_u16, 0i32);
    primitive(b"copy", 20_u16, 1i32);
    primitive(b"lastbox", 20_u16, 2i32);
    primitive(b"vsplit", 20_u16, 3i32);
    primitive(b"vtop", 20_u16, 4i32);
    primitive(b"vbox", 20_u16, 4i32 + 1i32);
    primitive(b"hbox", 20_u16, 4i32 + 104i32);
    primitive(b"shipout", 31_u16, 100i32 - 1i32);
    primitive(b"leaders", 31_u16, 100i32);
    primitive(b"cleaders", 31_u16, 101i32);
    primitive(b"xleaders", 31_u16, 102i32);
    primitive(b"indent", 43_u16, 1i32);
    primitive(b"noindent", 43_u16, 0i32);
    primitive(b"unpenalty", 25_u16, 12i32);
    primitive(b"unkern", 25_u16, 11i32);
    primitive(b"unskip", 25_u16, 10i32);
    primitive(b"unhbox", 23_u16, 0i32);
    primitive(b"unhcopy", 23_u16, 1i32);
    primitive(b"unvbox", 24_u16, 0i32);
    primitive(b"unvcopy", 24_u16, 1i32);
    primitive(b"-", 47_u16, 1i32);
    primitive(b"discretionary", 47_u16, 0i32);
    primitive(b"eqno", 48_u16, 0i32);
    primitive(b"leqno", 48_u16, 1i32);
    primitive(b"mathord", 50_u16, 16i32);
    primitive(b"mathop", 50_u16, 17i32);
    primitive(b"mathbin", 50_u16, 18i32);
    primitive(b"mathrel", 50_u16, 19i32);
    primitive(b"mathopen", 50_u16, 20i32);
    primitive(b"mathclose", 50_u16, 21i32);
    primitive(b"mathpunct", 50_u16, 22i32);
    primitive(b"mathinner", 50_u16, 23i32);
    primitive(b"underline", 50_u16, 26i32);
    primitive(b"overline", 50_u16, 27i32);
    primitive(b"displaylimits", 51_u16, 0i32);
    primitive(b"limits", 51_u16, 1i32);
    primitive(b"nolimits", 51_u16, 2i32);
    primitive(b"displaystyle", 53_u16, 0i32);
    primitive(b"textstyle", 53_u16, 2i32);
    primitive(b"scriptstyle", 53_u16, 4i32);
    primitive(b"scriptscriptstyle", 53_u16, 6i32);
    primitive(b"above", 52_u16, 0i32);
    primitive(b"over", 52_u16, 1i32);
    primitive(b"atop", 52_u16, 2i32);
    primitive(b"abovewithdelims", 52_u16, 3i32 + 0i32);
    primitive(b"overwithdelims", 52_u16, 3i32 + 1i32);
    primitive(b"atopwithdelims", 52_u16, 3i32 + 2i32);
    primitive(b"left", 49_u16, 30i32);
    primitive(b"right", 49_u16, 31i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 3i32) as isize,
    ))
    .s1 = maketexstring(b"right");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 3i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"long", 95_u16, 1i32);
    primitive(b"outer", 95_u16, 2i32);
    primitive(b"global", 95_u16, 4i32);
    primitive(b"def", 99_u16, 0i32);
    primitive(b"gdef", 99_u16, 1i32);
    primitive(b"edef", 99_u16, 2i32);
    primitive(b"xdef", 99_u16, 3i32);
    primitive(b"let", 96_u16, 0i32);
    primitive(b"futurelet", 96_u16, 0i32 + 1i32);
    primitive(b"chardef", 97_u16, 0i32);
    primitive(b"mathchardef", 97_u16, 1i32);
    primitive(b"XeTeXmathcharnumdef", 97_u16, 8i32);
    primitive(b"Umathcharnumdef", 97_u16, 8i32);
    primitive(b"XeTeXmathchardef", 97_u16, 9i32);
    primitive(b"Umathchardef", 97_u16, 9i32);
    primitive(b"countdef", 97_u16, 2i32);
    primitive(b"dimendef", 97_u16, 3i32);
    primitive(b"skipdef", 97_u16, 4i32);
    primitive(b"muskipdef", 97_u16, 5i32);
    primitive(b"toksdef", 97_u16, 6i32);
    primitive(
        b"catcode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32,
    );
    primitive(
        b"mathcode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"XeTeXmathcodenum",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"Umathcodenum",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"XeTeXmathcode",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32,
    );
    primitive(
        b"Umathcode",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32,
    );
    primitive(
        b"lccode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"uccode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"sfcode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"XeTeXcharclass",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"delcode",
        86_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32,
    );
    primitive(
        b"XeTeXdelcodenum",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32,
    );
    primitive(
        b"Udelcodenum",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32,
    );
    primitive(
        b"XeTeXdelcode",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + 1i32,
    );
    primitive(
        b"Udelcode",
        87_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 85i32
            + 256i32
            + 1i32,
    );
    primitive(
        b"textfont",
        88_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 0i32,
    );
    primitive(
        b"scriptfont",
        88_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 256i32,
    );
    primitive(
        b"scriptscriptfont",
        88_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 2i32 * 256i32,
    );
    primitive(b"hyphenation", 101_u16, 0i32);
    primitive(b"patterns", 101_u16, 1i32);
    primitive(b"hyphenchar", 79_u16, 0i32);
    primitive(b"skewchar", 79_u16, 1i32);
    primitive(b"lpcode", 79_u16, 2i32);
    primitive(b"rpcode", 79_u16, 3i32);
    primitive(b"batchmode", 102_u16, 0i32);
    primitive(b"nonstopmode", 102_u16, 1i32);
    primitive(b"scrollmode", 102_u16, 2i32);
    primitive(b"errorstopmode", 102_u16, 3i32);
    primitive(b"openin", 60_u16, 1i32);
    primitive(b"closein", 60_u16, 0i32);
    primitive(b"message", 58_u16, 0i32);
    primitive(b"errmessage", 58_u16, 1i32);
    primitive(
        b"lowercase",
        57_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32),
    );
    primitive(
        b"uppercase",
        57_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32),
    );
    primitive(b"show", 19_u16, 0i32);
    primitive(b"showbox", 19_u16, 1i32);
    primitive(b"showthe", 19_u16, 2i32);
    primitive(b"showlists", 19_u16, 3i32);
    primitive(b"openout", 59_u16, 0i32);
    primitive(b"write", 59_u16, 1i32);
    write_loc = cur_val;
    primitive(b"closeout", 59_u16, 2i32);
    primitive(b"special", 59_u16, 3i32);
    (*hash.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 10i32) as isize,
    ))
    .s1 = maketexstring(b"special");
    *eqtb.offset(
        (1i32 + (0x10ffffi32 + 1i32) + (0x10ffffi32 + 1i32) + 1i32 + 15000i32 + 10i32) as isize,
    ) = *eqtb.offset(cur_val as isize);
    primitive(b"immediate", 59_u16, 4i32);
    primitive(b"setlanguage", 59_u16, 5i32);
    primitive(
        b"synctex",
        74_u16,
        1i32 + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 1i32
            + 15000i32
            + 12i32
            + 9000i32
            + 1i32
            + 1i32
            + 19i32
            + 256i32
            + 256i32
            + 13i32
            + 256i32
            + 4i32
            + 256i32
            + 1i32
            + 3i32 * 256i32
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + (0x10ffffi32 + 1i32)
            + 83i32,
    );
    no_new_control_sequence = true;
}
unsafe extern "C" fn get_strings_started() {
    pool_ptr = 0i32;
    str_ptr = 0i32;
    *str_start.offset(0) = 0i32;
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
#[no_mangle]
pub(crate) unsafe extern "C" fn tt_run_engine(
    mut dump_name: *const i8,
    mut input_file_name: *const i8,
) -> TTHistory {
    let mut font_k: i32 = 0;
    /* Miscellaneous initializations that were mostly originally done in the
     * main() driver routines. */
    /* Get our stdout handle */
    rust_stdout = ttstub_output_open_stdout();
    let len = strlen(dump_name);
    TEX_format_default = xmalloc(len.wrapping_add(1) as _) as *mut i8;
    strcpy(TEX_format_default, dump_name);
    format_default_length = len as i32;
    /* Not sure why these get custom initializations. */
    if file_line_error_style_p < 0i32 {
        file_line_error_style_p = 0i32
    }
    /* These various parameters were configurable in web2c TeX. We don't
     * bother to allow that. */
    pool_size = 6250000i64 as i32;
    string_vacancies = 90000i64 as i32;
    pool_free = 47500i64 as i32;
    max_strings = 565536i64 as i32;
    strings_free = 100i32;
    font_mem_size = 8000000i64 as i32;
    font_max = 9000i32;
    trie_size = 1000000i64 as i32;
    hyph_size = 8191i32;
    buf_size = 200000i64 as i32;
    nest_size = 500i32;
    max_in_open = 15i32;
    param_size = 10000i32;
    save_size = 80000i64 as i32;
    stack_size = 5000i32;
    error_line = 79i32;
    half_error_line = 50i32;
    max_print_line = 79i32;
    hash_extra = 600000i64 as i32;
    expand_depth = 10000i32;
    /* Allocate many of our big arrays. */
    buffer = xmalloc_array(buf_size as usize);
    nest = xmalloc_array(nest_size as usize);
    save_stack = xmalloc_array(save_size as usize);
    input_stack = xmalloc_array(stack_size as usize);
    input_file = xmalloc_array(max_in_open as usize);
    line_stack = xmalloc_array(max_in_open as usize);
    eof_seen = xmalloc_array(max_in_open as usize);
    grp_stack = xmalloc_array(max_in_open as usize);
    if_stack = xmalloc_array(max_in_open as usize);
    source_filename_stack = xmalloc_array(max_in_open as usize);
    full_source_filename_stack = xmalloc_array(max_in_open as usize);
    param_stack = xmalloc_array(param_size as usize);
    hyph_word = xmalloc_array(hyph_size as usize);
    hyph_list = xmalloc_array(hyph_size as usize);
    hyph_link = xmalloc_array(hyph_size as usize);

    /* First bit of initex handling: more allocations. */

    if in_initex_mode {
        mem = xmalloc_array(MEM_TOP as usize + 1);
        eqtb_top = EQTB_SIZE + hash_extra;
        if hash_extra == 0 {
            hash_top = UNDEFINED_CONTROL_SEQUENCE;
        } else {
            hash_top = eqtb_top
        }
        yhash = xmalloc_array((1 + hash_top - hash_offset) as usize);
        hash = yhash.offset(-514);
        (*hash.offset((HASH_BASE) as isize)).s0 = 0;
        (*hash.offset((HASH_BASE) as isize)).s1 = 0;
        hash_used = HASH_BASE + 1;
        while hash_used <= hash_top {
            *hash.offset(hash_used as isize) = *hash.offset(HASH_BASE as isize);
            hash_used += 1
        }
        eqtb = xcalloc_array(eqtb_top as usize);
        str_start = xmalloc_array(max_strings as usize);
        str_pool = xmalloc_array(pool_size as usize);
        font_info = xmalloc_array(font_mem_size as usize);
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
    if max_in_open >= 128 {
        bad = 6
    }
    if MEM_TOP < 267 {
        bad = 7
    }
    if MIN_HALFWORD > 0 {
        bad = 12
    }
    if MAX_FONT_MAX < MIN_HALFWORD || MAX_FONT_MAX > MAX_HALFWORD {
        bad = 15
    }
    if font_max > FONT_BASE + 9000 {
        bad = 16
    }
    if save_size > MAX_HALFWORD || max_strings > MAX_HALFWORD {
        bad = 17
    }
    if buf_size > MAX_HALFWORD {
        bad = 18
    }
    if CS_TOKEN_FLAG + EQTB_SIZE + hash_extra > MAX_HALFWORD {
        bad = 21
    }
    if 514i32 < 0 || 514i32 > HASH_BASE {
        bad = 42
    }
    if format_default_length > std::i32::MAX {
        bad = 31
    }
    if 2 * MAX_HALFWORD < MEM_TOP {
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
    input_ptr = 0i32;
    max_in_stack = 0i32;
    *source_filename_stack.offset(0) = 0i32;
    *full_source_filename_stack.offset(0) = 0i32;
    in_open = 0i32;
    open_parens = 0i32;
    max_buf_stack = 0i32;
    *grp_stack.offset(0) = 0i32;
    *if_stack.offset(0) = TEX_NULL;
    param_ptr = 0i32;
    max_param_stack = 0i32;
    used_tectonic_coda_tokens = false;
    gave_char_warning_help = false;
    memset(
        buffer as *mut libc::c_void,
        0i32,
        (buf_size as usize).wrapping_mul(::std::mem::size_of::<UnicodeScalar>()),
    );
    first = 0i32;
    scanner_status = 0_u8;
    warning_index = TEX_NULL;
    first = 1i32;
    cur_input.state = 33_u16;
    cur_input.start = 1i32;
    cur_input.index = 0_u16;
    line = 0i32;
    cur_input.name = 0i32;
    force_eof = false;
    align_state = 1000000i64 as i32;
    init_io();
    if in_initex_mode {
        no_new_control_sequence = false;
        primitive(b"XeTeXpicfile", 59_u16, 41i32);
        primitive(b"XeTeXpdffile", 59_u16, 42i32);
        primitive(b"XeTeXglyph", 59_u16, 43i32);
        primitive(b"XeTeXlinebreaklocale", 59_u16, 46i32);
        primitive(b"pdfsavepos", 59_u16, 6i32 + 0i32);
        primitive(b"lastnodetype", 71_u16, 3i32);
        primitive(b"eTeXversion", 71_u16, 6i32);
        primitive(b"eTeXrevision", 110_u16, 5i32);
        primitive(b"XeTeXversion", 71_u16, 14i32);
        primitive(b"XeTeXrevision", 110_u16, 6i32);
        primitive(b"XeTeXcountglyphs", 71_u16, 15i32);
        primitive(b"XeTeXcountvariations", 71_u16, 16i32);
        primitive(b"XeTeXvariation", 71_u16, 17i32);
        primitive(b"XeTeXfindvariationbyname", 71_u16, 18i32);
        primitive(b"XeTeXvariationmin", 71_u16, 19i32);
        primitive(b"XeTeXvariationmax", 71_u16, 20i32);
        primitive(b"XeTeXvariationdefault", 71_u16, 21i32);
        primitive(b"XeTeXcountfeatures", 71_u16, 22i32);
        primitive(b"XeTeXfeaturecode", 71_u16, 23i32);
        primitive(b"XeTeXfindfeaturebyname", 71_u16, 24i32);
        primitive(b"XeTeXisexclusivefeature", 71_u16, 25i32);
        primitive(b"XeTeXcountselectors", 71_u16, 26i32);
        primitive(b"XeTeXselectorcode", 71_u16, 27i32);
        primitive(b"XeTeXfindselectorbyname", 71_u16, 28i32);
        primitive(b"XeTeXisdefaultselector", 71_u16, 29i32);
        primitive(b"XeTeXvariationname", 110_u16, 7i32);
        primitive(b"XeTeXfeaturename", 110_u16, 8i32);
        primitive(b"XeTeXselectorname", 110_u16, 9i32);
        primitive(b"XeTeXOTcountscripts", 71_u16, 30i32);
        primitive(b"XeTeXOTcountlanguages", 71_u16, 31i32);
        primitive(b"XeTeXOTcountfeatures", 71_u16, 32i32);
        primitive(b"XeTeXOTscripttag", 71_u16, 33i32);
        primitive(b"XeTeXOTlanguagetag", 71_u16, 34i32);
        primitive(b"XeTeXOTfeaturetag", 71_u16, 35i32);
        primitive(b"XeTeXcharglyph", 71_u16, 36i32);
        primitive(b"XeTeXglyphindex", 71_u16, 37i32);
        primitive(b"XeTeXglyphbounds", 71_u16, 47i32);
        primitive(b"XeTeXglyphname", 110_u16, 10i32);
        primitive(b"XeTeXfonttype", 71_u16, 38i32);
        primitive(b"XeTeXfirstfontchar", 71_u16, 39i32);
        primitive(b"XeTeXlastfontchar", 71_u16, 40i32);
        primitive(b"pdflastxpos", 71_u16, 41i32);
        primitive(b"pdflastypos", 71_u16, 42i32);
        primitive(b"strcmp", 110_u16, 43i32);
        primitive(b"mdfivesum", 110_u16, 44i32);
        primitive(b"pdfmdfivesum", 110_u16, 44i32);
        primitive(b"shellescape", 71_u16, 45i32);
        primitive(b"XeTeXpdfpagecount", 71_u16, 46i32);

        primitive(
            b"tracingassigns",
            ASSIGN_INT,
            INT_BASE + INT_PAR__tracing_assigns,
        );
        primitive(
            b"tracinggroups",
            ASSIGN_INT,
            INT_BASE + INT_PAR__tracing_groups,
        );
        primitive(b"tracingifs", ASSIGN_INT, INT_BASE + INT_PAR__tracing_ifs);
        primitive(
            b"tracingscantokens",
            ASSIGN_INT,
            INT_BASE + INT_PAR__tracing_scan_tokens,
        );
        primitive(
            b"tracingnesting",
            ASSIGN_INT,
            INT_BASE + INT_PAR__tracing_nesting,
        );
        primitive(
            b"predisplaydirection",
            ASSIGN_INT,
            INT_BASE + INT_PAR__pre_display_correction,
        );
        primitive(
            b"lastlinefit",
            ASSIGN_INT,
            INT_BASE + INT_PAR__last_line_fit,
        );
        primitive(
            b"savingvdiscards",
            ASSIGN_INT,
            INT_BASE + INT_PAR__saving_vdiscards,
        );
        primitive(
            b"savinghyphcodes",
            ASSIGN_INT,
            INT_BASE + INT_PAR__saving_hyphs,
        );

        primitive(b"currentgrouplevel", 71_u16, 7i32);
        primitive(b"currentgrouptype", 71_u16, 8i32);
        primitive(b"currentiflevel", 71_u16, 9i32);
        primitive(b"currentiftype", 71_u16, 10i32);
        primitive(b"currentifbranch", 71_u16, 11i32);
        primitive(b"fontcharwd", 71_u16, 48i32);
        primitive(b"fontcharht", 71_u16, 49i32);
        primitive(b"fontchardp", 71_u16, 50i32);
        primitive(b"fontcharic", 71_u16, 51i32);
        primitive(b"parshapelength", 71_u16, 52i32);
        primitive(b"parshapeindent", 71_u16, 53i32);
        primitive(b"parshapedimen", 71_u16, 54i32);
        primitive(b"showgroups", 19_u16, 4i32);
        primitive(b"showtokens", 19_u16, 5i32);
        primitive(b"unexpanded", 111_u16, 1i32);
        primitive(b"detokenize", 111_u16, 5i32);
        primitive(b"showifs", 19_u16, 6i32);
        primitive(b"interactionmode", 83_u16, 2i32);
        primitive(b"middle", 49_u16, 1i32);
        primitive(
            b"suppressfontnotfounderror",
            ASSIGN_INT,
            INT_BASE + INT_PAR__suppress_fontnotfound_error,
        );

        primitive(b"TeXXeTstate", ASSIGN_INT, INT_BASE + INT_PAR__texxet);
        primitive(
            b"XeTeXupwardsmode",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_upwards,
        );
        primitive(
            b"XeTeXuseglyphmetrics",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_use_glyph_metrics,
        );
        primitive(
            b"XeTeXinterchartokenstate",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_inter_char_tokens,
        );
        primitive(
            b"XeTeXdashbreakstate",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_dash_break,
        );
        primitive(
            b"XeTeXinputnormalization",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_input_normalization,
        );
        primitive(
            b"XeTeXtracingfonts",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_tracing_fonts,
        );
        primitive(
            b"XeTeXinterwordspaceshaping",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_interword_space_shaping,
        );
        primitive(
            b"XeTeXgenerateactualtext",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_generate_actual_text,
        );
        primitive(
            b"XeTeXhyphenatablelength",
            ASSIGN_INT,
            INT_BASE + INT_PAR__xetex_hyphenatable_length,
        );
        primitive(b"pdfoutput", ASSIGN_INT, INT_BASE + INT_PAR__pdfoutput);

        primitive(
            b"XeTeXinputencoding",
            EXTENSION,
            XETEX_INPUT_ENCODING_EXTENSION_CODE,
        );
        primitive(
            b"XeTeXdefaultencoding",
            EXTENSION,
            XETEX_DEFAULT_ENCODING_EXTENSION_CODE,
        );

        primitive(b"beginL", 33_u16, 6i32);
        primitive(b"endL", 33_u16, 7i32);
        primitive(b"beginR", 33_u16, 10i32);
        primitive(b"endR", 33_u16, 11i32);
        primitive(b"scantokens", 106_u16, 2i32);
        primitive(b"readline", 98_u16, 1i32);
        primitive(b"unless", 104_u16, 1i32);
        primitive(b"ifdefined", 107_u16, 17i32);
        primitive(b"ifcsname", 107_u16, 18i32);
        primitive(b"iffontchar", 107_u16, 19i32);
        primitive(b"ifincsname", 107_u16, 20i32);
        primitive(b"protected", 95_u16, 8i32);
        primitive(b"numexpr", 71_u16, 59i32 + 0i32);
        primitive(b"dimexpr", 71_u16, 59i32 + 1i32);
        primitive(b"glueexpr", 71_u16, 59i32 + 2i32);
        primitive(b"muexpr", 71_u16, 59i32 + 3i32);
        primitive(b"gluestretchorder", 71_u16, 12i32);
        primitive(b"glueshrinkorder", 71_u16, 13i32);
        primitive(b"gluestretch", 71_u16, 55i32);
        primitive(b"glueshrink", 71_u16, 56i32);
        primitive(b"mutoglue", 71_u16, 57i32);
        primitive(b"gluetomu", 71_u16, 58i32);
        primitive(b"marks", 18_u16, 5i32);
        primitive(b"topmarks", 112_u16, 0i32 + 5i32);
        primitive(b"firstmarks", 112_u16, 1i32 + 5i32);
        primitive(b"botmarks", 112_u16, 2i32 + 5i32);
        primitive(b"splitfirstmarks", 112_u16, 3i32 + 5i32);
        primitive(b"splitbotmarks", 112_u16, 4i32 + 5i32);
        primitive(b"pagediscards", 24_u16, 2i32);
        primitive(b"splitdiscards", 24_u16, 3i32);

        primitive(b"interlinepenalties", SET_SHAPE, INTER_LINE_PENALTIES_LOC);
        primitive(b"clubpenalties", SET_SHAPE, CLUB_PENALTIES_LOC);
        primitive(b"widowpenalties", SET_SHAPE, WIDOW_PENALTIES_LOC);
        primitive(
            b"displaywidowpenalties",
            SET_SHAPE,
            DISPLAY_WIDOW_PENALTIES_LOC,
        );
        max_reg_num = 32767i32;
        max_reg_help_line = b"A register number must be between 0 and 32767.";
    }
    no_new_control_sequence = true;
    if !in_initex_mode {
        if !load_fmt_file() {
            return history;
        }
    }
    if INTPAR(INT_PAR__end_line_char) < 0 || INTPAR(INT_PAR__end_line_char) < BIGGEST_CHAR {
        cur_input.limit -= 1
    } else {
        *buffer.offset(cur_input.limit as isize) = INTPAR(INT_PAR__end_line_char);
    }
    if in_initex_mode {
        /* TeX initializes with the real date and time, but for format file
         * reproducibility we do this: */
        INTPAR_set(INT_PAR__time, 0);
        INTPAR_set(INT_PAR__day, 0);
        INTPAR_set(INT_PAR__month, 0);
        INTPAR_set(INT_PAR__year, 0);
    } else {
        let (minutes, day, month, year) = get_date_and_time();
        INTPAR_set(INT_PAR__time, minutes);
        INTPAR_set(INT_PAR__day, day);
        INTPAR_set(INT_PAR__month, month);
        INTPAR_set(INT_PAR__year, year);
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
        *trie_l.offset(0) = 0i32;
        *trie_c.offset(0) = 0i32 as packed_UTF16_code;
        trie_ptr = 0i32;
        *trie_r.offset(0) = 0i32;
        hyph_start = 0i32;
        font_mapping = xcalloc_array::<*mut libc::c_void>(font_max as usize);
        font_layout_engine = xcalloc_array::<*mut libc::c_void>(font_max as usize);
        font_flags = xcalloc_array(font_max as usize);
        font_letter_space = xcalloc_array(font_max as usize);
        font_check = xcalloc_array(font_max as usize);
        font_size = xcalloc_array(font_max as usize);
        font_dsize = xcalloc_array(font_max as usize);
        font_params = xcalloc_array(font_max as usize);
        font_name = xcalloc_array(font_max as usize);
        font_area = xcalloc_array(font_max as usize);
        font_bc = xcalloc_array(font_max as usize);
        font_ec = xcalloc_array(font_max as usize);
        font_glue = xcalloc_array(font_max as usize);
        hyphen_char = xcalloc_array(font_max as usize);
        skew_char = xcalloc_array(font_max as usize);
        bchar_label = xcalloc_array(font_max as usize);
        font_bchar = xcalloc_array(font_max as usize);
        font_false_bchar = xcalloc_array(font_max as usize);
        char_base = xcalloc_array(font_max as usize);
        width_base = xcalloc_array(font_max as usize);
        height_base = xcalloc_array(font_max as usize);
        depth_base = xcalloc_array(font_max as usize);
        italic_base = xcalloc_array(font_max as usize);
        lig_kern_base = xcalloc_array(font_max as usize);
        kern_base = xcalloc_array(font_max as usize);
        exten_base = xcalloc_array(font_max as usize);
        param_base = xcalloc_array(font_max as usize);
        font_ptr = 0i32;
        fmem_ptr = 7i32;
        *font_name.offset(0) = maketexstring(b"nullfont");
        *font_area.offset(0) = (65536 + 1i32 as i64) as str_number;
        *hyphen_char.offset(0) = '-' as i32;
        *skew_char.offset(0) = -1i32;
        *bchar_label.offset(0) = 0i32;
        *font_bchar.offset(0) = 65536i32;
        *font_false_bchar.offset(0) = 65536i32;
        *font_bc.offset(0) = 1i32 as UTF16_code;
        *font_ec.offset(0) = 0i32 as UTF16_code;
        *font_size.offset(0) = 0i32;
        *font_dsize.offset(0) = 0i32;
        *char_base.offset(0) = 0i32;
        *width_base.offset(0) = 0i32;
        *height_base.offset(0) = 0i32;
        *depth_base.offset(0) = 0i32;
        *italic_base.offset(0) = 0i32;
        *lig_kern_base.offset(0) = 0i32;
        *kern_base.offset(0) = 0i32;
        *exten_base.offset(0) = 0i32;
        *font_glue.offset(0) = TEX_NULL;
        *font_params.offset(0) = 7i32;
        let ref mut fresh21 = *font_mapping.offset(0);
        *fresh21 = 0 as *mut libc::c_void;
        *param_base.offset(0) = -1i32;
        font_k = 0i32;
        while font_k <= 6i32 {
            (*font_info.offset(font_k as isize)).b32.s1 = 0i32;
            font_k += 1
        }
    }
    font_used = xmalloc_array(font_max as usize);
    font_k = 0i32;
    while font_k <= font_max {
        *font_used.offset(font_k as isize) = false;
        font_k += 1
    }
    if interaction as i32 == 0i32 {
        selector = Selector::NO_PRINT
    } else {
        selector = Selector::TERM_ONLY
    }
    if semantic_pagination_enabled {
        INTPAR_set(INT_PAR__xetex_generate_actual_text, 1);
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
    free(font_used as *mut libc::c_void);
    deinitialize_shipout_variables();
    destroy_font_manager();
    font_k = 0i32;
    while font_k < font_max {
        if !(*font_layout_engine.offset(font_k as isize)).is_null() {
            release_font_engine(
                *font_layout_engine.offset(font_k as isize),
                *font_area.offset(font_k as isize),
            );
            let ref mut fresh22 = *font_layout_engine.offset(font_k as isize);
            *fresh22 = 0 as *mut libc::c_void
        }
        font_k += 1
    }
    // Free the big allocated arrays
    free(buffer as *mut libc::c_void);
    free(nest as *mut libc::c_void);
    free(save_stack as *mut libc::c_void);
    free(input_stack as *mut libc::c_void);
    free(input_file as *mut libc::c_void);
    free(line_stack as *mut libc::c_void);
    free(eof_seen as *mut libc::c_void);
    free(grp_stack as *mut libc::c_void);
    free(if_stack as *mut libc::c_void);
    free(source_filename_stack as *mut libc::c_void);
    free(full_source_filename_stack as *mut libc::c_void);
    free(param_stack as *mut libc::c_void);
    free(hyph_word as *mut libc::c_void);
    free(hyph_list as *mut libc::c_void);
    free(hyph_link as *mut libc::c_void);
    // initialize_more_variables @ 3277
    free(native_text as *mut libc::c_void);
    // Free arrays allocated in load_fmt_file
    free(yhash as *mut libc::c_void);
    free(eqtb as *mut libc::c_void);
    free(mem as *mut libc::c_void);
    free(str_start as *mut libc::c_void);
    free(str_pool as *mut libc::c_void);
    free(font_info as *mut libc::c_void);
    free(font_mapping as *mut libc::c_void);
    free(font_layout_engine as *mut libc::c_void);
    free(font_flags as *mut libc::c_void);
    free(font_letter_space as *mut libc::c_void);
    free(font_check as *mut libc::c_void);
    free(font_size as *mut libc::c_void);
    free(font_dsize as *mut libc::c_void);
    free(font_params as *mut libc::c_void);
    free(font_name as *mut libc::c_void);
    free(font_area as *mut libc::c_void);
    free(font_bc as *mut libc::c_void);
    free(font_ec as *mut libc::c_void);
    free(font_glue as *mut libc::c_void);
    free(hyphen_char as *mut libc::c_void);
    free(skew_char as *mut libc::c_void);
    free(bchar_label as *mut libc::c_void);
    free(font_bchar as *mut libc::c_void);
    free(font_false_bchar as *mut libc::c_void);
    free(char_base as *mut libc::c_void);
    free(width_base as *mut libc::c_void);
    free(height_base as *mut libc::c_void);
    free(depth_base as *mut libc::c_void);
    free(italic_base as *mut libc::c_void);
    free(lig_kern_base as *mut libc::c_void);
    free(kern_base as *mut libc::c_void);
    free(exten_base as *mut libc::c_void);
    free(param_base as *mut libc::c_void);
    trie_trl = mfree(trie_trl as *mut libc::c_void) as *mut trie_pointer;
    trie_tro = mfree(trie_tro as *mut libc::c_void) as *mut trie_pointer;
    trie_trc = mfree(trie_trc as *mut libc::c_void) as *mut u16;
    history
}
