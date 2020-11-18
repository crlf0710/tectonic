use crate::cmd::{Cmd, InteractionMode};
use crate::help;
use crate::xetex_consts::IntPar;
use crate::xetex_ini::{b16x4, b32x2, memory_word, name_of_file, EqtbWord, Selector, FONT_PTR};
use bridge::{ttstub_output_close, ttstub_output_open, InFile, TTHistory, TTInputFormat};
use std::ffi::CString;

use std::io::{Read, Write};

use crate::trie::{
    hyf_distance, hyf_next, hyf_num, hyph_start, init_trie, max_hyph_char, op_start, trie_max,
    trie_not_ready, trie_op_ptr, trie_opcode, trie_size, trie_trc, trie_trl, trie_tro, trie_used,
    TRIE_OP_SIZE,
};
use crate::xetex_ini::*;
use crate::xetex_scaledmath::Scaled;
/*use crate::xetex_ini::MEM;
use crate::xetex_ini::{ITALIC_BASE, LIG_KERN_BASE, DEPTH_BASE, HEIGHT_BASE, WIDTH_BASE, CHAR_BASE};
use crate::xetex_ini::fmem_ptr;
use crate::xetex_ini::FONT_INFO;
use crate::xetex_ini::FONT_MEM_SIZE;
use crate::xetex_ini::HYPH_NEXT;
use crate::xetex_ini::HYPH_SIZE;*/
use crate::xetex_consts::ACTIVE_BASE;
use crate::xetex_consts::BIGGEST_LANG;
use crate::xetex_consts::CONTRIB_HEAD;
use crate::xetex_consts::CS_TOKEN_FLAG;
use crate::xetex_consts::EQTB_SIZE;
use crate::xetex_consts::FONT_BASE;
use crate::xetex_consts::FONT_ID_BASE;
use crate::xetex_consts::FORMAT_SERIAL;
use crate::xetex_consts::FROZEN_CONTROL_SEQUENCE;
use crate::xetex_consts::HASH_BASE;
use crate::xetex_consts::HASH_PRIME;
use crate::xetex_consts::HI_MEM_STAT_USAGE;
use crate::xetex_consts::INT_BASE;
use crate::xetex_consts::LEVEL_ZERO;
use crate::xetex_consts::MAX_HALFWORD;
use crate::xetex_consts::MEM_TOP;
use crate::xetex_consts::PAGE_HEAD;
use crate::xetex_consts::PRE_ADJUST_HEAD;
use crate::xetex_consts::PRIM_SIZE;
use crate::xetex_consts::{get_int_par, set_int_par};
use crate::xetex_consts::{
    ValLevel, HYPH_PRIME, MAX_FONT_MAX, MIN_HALFWORD, UNDEFINED_CONTROL_SEQUENCE,
};
use crate::xetex_stringpool::{make_string, PoolString, EMPTY_STRING, TOO_BIG_CHAR};

use crate::xetex_errors::error;
use crate::xetex_errors::overflow;
use crate::xetex_output::print_file_name;
use crate::xetex_output::{
    print, print_chr, print_cstr, print_esc, print_file_line, print_int, print_ln, print_nl_cstr,
    print_scaled,
};
use crate::xetex_xetexd::llist_link;
use crate::xetex_xetexd::{TeXInt, TeXOpt};

use crate::core_memory::xmalloc_array;

use crate::xetex_xetex0::get_node;
use crate::xetex_xetex0::{
    close_files_and_terminate, make_name_string, pack_job_name, pseudo_close,
};

/// Magic constant, origin unclear
const sup_max_strings: i32 = 2097151;
const sup_hash_extra: i32 = sup_max_strings;
/// "TTNC" in ASCII
const FORMAT_HEADER_MAGIC: i32 = 0x54544E43;
const FORMAT_FOOTER_MAGIC: i32 = 0x0000029A;
const sup_pool_size: usize = 40000000;
/// magic constant, origin unclear
const sup_font_mem_size: i32 = 147483647;

use libc::free;

unsafe fn pack_buffered_name(mut _n: i16, mut _a: i32, mut _b: i32) {
    name_of_file = TEX_format_default.clone();
}

unsafe fn sort_avail() {
    let _p = get_node(0x40000000) as i32;
    let mut p = MEM[(rover + 1) as usize].b32.s1;
    MEM[(rover + 1) as usize].b32.s1 = MAX_HALFWORD;
    let old_rover = rover;
    /*136: */
    while p != old_rover {
        if p < rover {
            let q = p;
            p = MEM[(q + 1) as usize].b32.s1;
            MEM[(q + 1) as usize].b32.s1 = rover;
            rover = q
        } else {
            let mut q = rover;
            while MEM[(q + 1) as usize].b32.s1 < p {
                q = MEM[(q + 1) as usize].b32.s1
            }
            let r = MEM[(p + 1) as usize].b32.s1;
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

/*:1328*/
/*1337:*/
pub(crate) unsafe fn store_fmt_file() {
    if SAVE_PTR != 0 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("You can\'t dump inside a group");
        help!("`{...\\dump}\' is a no-no.");

        if interaction == InteractionMode::ErrorStop {
            interaction = InteractionMode::Scroll;
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
    print_cstr(" (preloaded format=");
    print(job_name);
    print_chr(' ');
    print_int(get_int_par(IntPar::year));
    print_chr('.');
    print_int(get_int_par(IntPar::month));
    print_chr('.');
    print_int(get_int_par(IntPar::day));
    print_chr(')');

    selector = if interaction == InteractionMode::Batch {
        Selector::LOG_ONLY
    } else {
        Selector::TERM_AND_LOG
    };

    if pool_ptr + 1 > pool_size {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }

    format_ident = make_string();
    pack_job_name(".fmt");

    let out_name = CString::new(name_of_file.as_str()).unwrap();
    let fmt_out = ttstub_output_open(out_name.as_ptr(), 0);
    if fmt_out.is_none() {
        abort!("cannot open format output file \"{}\"", name_of_file);
    }

    let mut fmt_out_owner = fmt_out.unwrap();
    let fmt_out = &mut fmt_out_owner;
    print_nl_cstr("Beginning to dump on file ");
    print(make_name_string());

    PoolString::flush();

    print_nl_cstr("");
    print(format_ident);

    /* Header */
    /* TODO: can we move this farther up in this function? */
    fmt_out.dump(FORMAT_HEADER_MAGIC);
    fmt_out.dump(FORMAT_SERIAL);
    fmt_out.dump(hash_high);

    while pseudo_files.opt().is_some() {
        pseudo_close();
    }

    fmt_out.dump(MEM_TOP as i32);
    fmt_out.dump(EQTB_SIZE as i32);
    fmt_out.dump(HASH_PRIME as i32);
    fmt_out.dump(HYPH_PRIME);

    /* string pool */

    fmt_out.dump(pool_ptr as i32);
    fmt_out.dump(str_ptr);
    fmt_out.dump(
        str_start[..(str_ptr - TOO_BIG_CHAR + 1) as usize]
            .iter()
            .map(|p| *p as i32)
            .collect::<Vec<_>>()
            .as_slice(),
    );
    fmt_out.dump(&str_pool[..pool_ptr]);

    print_ln();
    print_int(str_ptr);
    print_cstr(" strings of total length ");
    print_int(pool_ptr as i32);

    /* "memory locations" */

    sort_avail();
    var_used = 0;
    fmt_out.dump(lo_mem_max);
    fmt_out.dump(rover);

    for k in (ValLevel::Int as usize)..=(ValLevel::InterChar as usize) {
        fmt_out.dump(sa_root[k].tex_int());
    }

    let mut p = 0;
    let mut q = rover;
    let mut x = 0;
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
    fmt_out.dump(hi_mem_min as i32);
    fmt_out.dump(avail.tex_int());
    fmt_out.dump(&MEM[hi_mem_min as usize..(mem_end + 1) as usize]);

    x = x + mem_end + 1 - hi_mem_min;
    let mut popt = avail;
    while let Some(p) = popt {
        dyn_used -= 1;
        popt = llist_link(p)
    }

    fmt_out.dump(var_used as i32);
    fmt_out.dump(dyn_used as i32);

    print_ln();
    print_int(x);
    print_cstr(" memory locations dumped; current usage is ");
    print_int(var_used);
    print_chr('&');
    print_int(dyn_used);

    /* equivalents table / primitive */

    let mut k = ACTIVE_BASE; /*:1350*/
    let mut j;
    loop {
        j = k;
        let l;
        loop {
            if j >= INT_BASE - 1 {
                l = INT_BASE;
                break;
            }
            if EQTB[j] == EQTB[j + 1] {
                j += 1;
                l = j;
                while j < INT_BASE - 1 {
                    if EQTB[j] != EQTB[j + 1] {
                        break;
                    }
                    j += 1;
                }
                break;
            }
            j += 1;
        }
        fmt_out.dump((l as i32) - (k as i32));
        fmt_out.dump(&EQTB[k..l]);
        k = j + 1;
        fmt_out.dump((k as i32) - (l as i32));
        if k == INT_BASE {
            break;
        }
    }

    let mut j;
    loop {
        j = k;
        let l;
        loop {
            if j >= EQTB_SIZE {
                l = EQTB_SIZE + 1;
                break;
            }
            if EQTB[j].val == EQTB[j + 1].val {
                j += 1;
                l = j;
                while j < EQTB_SIZE {
                    if EQTB[j].val != EQTB[j + 1].val {
                        break;
                    }
                    j += 1;
                }
                break;
            }
            j += 1;
        }

        // done2:
        fmt_out.dump((l as i32) - (k as i32));
        fmt_out.dump(&EQTB[k..l]);
        k = j + 1;
        fmt_out.dump((k as i32) - (l as i32));
        if !(k <= EQTB_SIZE) {
            break;
        }
    }

    if hash_high > 0 {
        fmt_out.dump(&EQTB[EQTB_SIZE + 1..EQTB_SIZE + 1 + hash_high as usize]);
    }

    fmt_out.dump(par_loc as i32);
    fmt_out.dump(write_loc as i32);

    for p in 0..=PRIM_SIZE {
        fmt_out.dump(prim[p]);
    }

    for p in 0..=PRIM_SIZE {
        fmt_out.dump(prim_eqtb[p]);
    }

    /* control sequences */
    fmt_out.dump(hash_used as i32);
    cs_count = (FROZEN_CONTROL_SEQUENCE as i32 - 1) - hash_used + hash_high;

    for p in (HASH_BASE as i32)..=hash_used {
        if (*hash.offset(p as isize)).s1 != 0 {
            fmt_out.dump(p as i32);
            fmt_out.dump(*hash.offset(p as isize));
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

    fmt_out.dump(cs_count);

    print_ln();
    print_int(cs_count);
    print_cstr(" multiletter control sequences");

    /* fonts */

    fmt_out.dump(fmem_ptr as i32);
    fmt_out.dump(&FONT_INFO[..fmem_ptr as usize]);
    fmt_out.dump(FONT_PTR as i32);
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
        print_nl_cstr("\\font");
        print_esc((*hash.offset(FONT_ID_BASE as isize + k as isize)).s1);
        print_chr('=');

        if matches!(&FONT_LAYOUT_ENGINE[k], crate::xetex_ext::Font::Native(_))
            || !(FONT_MAPPING[k]).is_null()
        {
            print_file_name(FONT_NAME[k], EMPTY_STRING, EMPTY_STRING);

            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Can\'t \\dump a format with native fonts or font-mappings");

            help!(
                "You really, really don\'t want to do this.",
                "It won\'t work, and only confuses me.",
                "(Load them at runtime, not as part of the format file.)"
            );
            error();
        } else {
            print_file_name(FONT_NAME[k], FONT_AREA[k], EMPTY_STRING);
        }

        if FONT_SIZE[k] != FONT_DSIZE[k] {
            print_cstr(" at ");
            print_scaled(FONT_SIZE[k]);
            print_cstr("pt");
        }
    }

    print_ln();
    print_int(fmem_ptr - 7);
    print_cstr(" words of font info for ");
    print_int(FONT_PTR as i32 - 0);
    if FONT_PTR != FONT_BASE + 1 {
        print_cstr(" preloaded fonts");
    } else {
        print_cstr(" preloaded font");
    }

    /* hyphenation info */

    fmt_out.dump(HYPH_COUNT as i32);
    if HYPH_NEXT <= HYPH_PRIME as usize {
        HYPH_NEXT = HYPH_SIZE
    }
    fmt_out.dump(HYPH_NEXT as i32);

    for k in 0..=HYPH_SIZE {
        if HYPH_WORD[k] != 0 {
            fmt_out.dump((k as i64 + 65536 * HYPH_LINK[k] as i64) as i32);
            fmt_out.dump(HYPH_WORD[k]);
            fmt_out.dump(HYPH_LIST[k].tex_int());
        }
    }

    print_ln();
    print_int(HYPH_COUNT as i32);
    if HYPH_COUNT != 1 {
        print_cstr(" hyphenation exceptions");
    } else {
        print_cstr(" hyphenation exception");
    }
    if trie_not_ready {
        init_trie();
    }

    fmt_out.dump(trie_max);
    fmt_out.dump(hyph_start);
    fmt_out.dump(&trie_trl[..(trie_max + 1) as usize]);
    fmt_out.dump(&trie_tro[..(trie_max + 1) as usize]);
    fmt_out.dump(&trie_trc[..(trie_max + 1) as usize]);
    fmt_out.dump(max_hyph_char);
    fmt_out.dump(trie_op_ptr as i32);
    fmt_out.dump(&hyf_distance[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_num[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_next[1..trie_op_ptr as usize + 1]);

    print_nl_cstr("Hyphenation trie of length ");
    print_int(trie_max);
    print_cstr(" has ");
    print_int(trie_op_ptr);
    if trie_op_ptr != 1i32 {
        print_cstr(" ops");
    } else {
        print_cstr(" op");
    }
    print_cstr(" out of ");
    print_int(TRIE_OP_SIZE as i32);

    for k in (0..=BIGGEST_LANG).rev() {
        if trie_used[k as usize] as i32 > 0i32 {
            print_nl_cstr("  ");
            print_int(trie_used[k as usize] as i32);
            print_cstr(" for language ");
            print_int(k);
            fmt_out.dump(k as i32);
            fmt_out.dump(trie_used[k as usize] as i32);
        }
    }

    /* footer */

    fmt_out.dump(FORMAT_FOOTER_MAGIC);

    set_int_par(IntPar::tracing_stats, 0); /*:1361*/
    ttstub_output_close(fmt_out_owner);
}

pub(crate) unsafe fn load_fmt_file() -> bool {
    let j = cur_input.loc;

    /* This is where a first line starting with "&" used to
     * trigger code that would change the format file. */

    pack_buffered_name((TEX_format_default.as_bytes().len() - 4) as i16, 1, 0);

    let mut fmt_in = InFile::open(&name_of_file, TTInputFormat::FORMAT, 0)
        .unwrap_or_else(|| abort!("cannot open the format file \"{}\"", name_of_file));

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

    let x: i32 = fmt_in.undump();
    if x != FORMAT_HEADER_MAGIC {
        bad_fmt();
    }

    let x: i32 = fmt_in.undump();
    if x != FORMAT_SERIAL {
        abort!(
            "format file \"{}\" is of the wrong version: expected {}, found {}",
            name_of_file,
            FORMAT_SERIAL,
            x
        );
    }

    /* hash table parameters */

    hash_high = fmt_in.undump();
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

    for x in HASH_BASE..=(hash_top as usize) {
        *hash.add(x) = b32x2_le_t { s0: 0, s1: 0 };
    }

    EQTB = Vec::with_capacity(EQTB_TOP + 2);
    EQTB.set_len(EQTB_TOP + 2);
    for x in EQTB_SIZE..=EQTB_TOP {
        EQTB[x] = EqtbWord {
            cmd: Cmd::UndefinedCS as _,
            val: None.tex_int() as _,
            lvl: LEVEL_ZERO as _,
        };
    }

    max_reg_num = 32767;
    max_reg_help_line = "A register number must be between 0 and 32767.";

    /* "memory locations" */

    let x: i32 = fmt_in.undump();
    if x != MEM_TOP as i32 {
        bad_fmt();
    }

    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    page_tail = PAGE_HEAD;

    MEM = Vec::with_capacity(MEM_TOP + 2);
    MEM.set_len(MEM_TOP + 2);

    let x: i32 = fmt_in.undump();
    if x != EQTB_SIZE as i32 {
        bad_fmt();
    }

    let x: i32 = fmt_in.undump();
    if x != HASH_PRIME as i32 {
        bad_fmt();
    }

    let x: i32 = fmt_in.undump();
    if x != HYPH_PRIME {
        bad_fmt();
    }

    /* string pool */

    let x: i32 = fmt_in.undump();
    if x < 0 {
        bad_fmt();
    }
    if x as usize > sup_pool_size - pool_free {
        panic!("must increase string_pool_size");
    }
    pool_ptr = x as usize;
    if pool_size < pool_ptr + pool_free {
        pool_size = pool_ptr + pool_free
    }
    let x: i32 = fmt_in.undump();
    if x < 0 {
        bad_fmt();
    }
    if x as i64 > sup_max_strings as i64 - strings_free as i64 {
        panic!("must increase sup_strings");
    }
    str_ptr = x;

    max_strings = max_strings.max(str_ptr as usize + strings_free);

    str_start = vec![0; max_strings + 1];
    let v: Vec<i32> = fmt_in.undump_many((str_ptr - TOO_BIG_CHAR + 1) as usize);
    for (ind, val) in v.into_iter().enumerate() {
        str_start[ind] = val as usize;
    }
    for i in 0..str_ptr - TOO_BIG_CHAR + 1 {
        if str_start[i as usize] > pool_ptr {
            panic!(
                "item {} (={}) of .fmt array at {:p} <{} or >{}",
                i, str_start[i as usize] as u64, &str_start[0], 0, pool_ptr as u64
            );
        }
    }
    str_pool = fmt_in.undump_many(pool_ptr);
    str_pool.resize(pool_size + 1, 0);
    init_str_ptr = str_ptr;
    init_pool_ptr = pool_ptr;
    /* "By sorting the list of available spaces in the variable-size portion
     * of |mem|, we are usually able to get by without having to dump very
     * much of the dynamic memory." */
    let x: i32 = fmt_in.undump();
    if x < 1019 || x > MEM_TOP as i32 - HI_MEM_STAT_USAGE {
        bad_fmt();
    } else {
        lo_mem_max = x;
    }
    let x: i32 = fmt_in.undump();
    if x < 20 || x > lo_mem_max {
        bad_fmt();
    } else {
        rover = x;
    }
    for k in (ValLevel::Int as usize)..=(ValLevel::InterChar as usize) {
        let x: i32 = fmt_in.undump();
        if x < MIN_HALFWORD || x > lo_mem_max {
            bad_fmt();
        } else {
            sa_root[k] = x.opt();
        }
    }

    let mut p = 0;
    let mut q = rover;

    loop {
        fmt_in.undump_slice(&mut MEM[p as usize..(q + 2) as usize]);
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

    fmt_in.undump_slice(&mut MEM[p as usize..(lo_mem_max + 1) as usize]);

    let x: i32 = fmt_in.undump();
    if x < lo_mem_max + 1 || x > PRE_ADJUST_HEAD as i32 {
        bad_fmt();
    } else {
        hi_mem_min = x;
    }

    let x: i32 = fmt_in.undump();
    if x < MIN_HALFWORD || x > MEM_TOP as i32 {
        bad_fmt();
    } else {
        avail = x.opt();
    }

    mem_end = MEM_TOP as i32;

    fmt_in.undump_slice(&mut MEM[hi_mem_min as usize..(mem_end + 1) as usize]);
    var_used = fmt_in.undump();
    dyn_used = fmt_in.undump();

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
        let x: i32 = fmt_in.undump();
        if x < 1 || k + x > (EQTB_SIZE as i32) + 1 {
            bad_fmt();
        }

        fmt_in.undump_slice(&mut EQTB[k as usize..(k + x) as usize]);
        k = k + x;

        let x: i32 = fmt_in.undump();
        if x < 0 || k + x > (EQTB_SIZE as i32) + 1 {
            bad_fmt();
        }

        let mut j = k;
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
        fmt_in.undump_slice(
            &mut EQTB[EQTB_SIZE as usize + 1..EQTB_SIZE as usize + 1 + hash_high as usize],
        );
    }

    let x: i32 = fmt_in.undump();
    if x < HASH_BASE as i32 || x > hash_top {
        bad_fmt();
    } else {
        par_loc = x;
    }

    par_token = CS_TOKEN_FLAG + par_loc;

    let x: i32 = fmt_in.undump();
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
        prim[p as usize] = fmt_in.undump();
        p += 1
    }

    p = 0i32;
    while p <= 500i32 {
        prim_eqtb[p as usize] = fmt_in.undump();
        p += 1
    }

    let x: i32 = fmt_in.undump();
    if x < HASH_BASE as i32 || x > FROZEN_CONTROL_SEQUENCE as i32 {
        bad_fmt();
    } else {
        hash_used = x;
    }

    p = HASH_BASE as i32 - 1;

    loop {
        let x: i32 = fmt_in.undump();
        if x < p + 1 || x > hash_used {
            bad_fmt();
        } else {
            p = x;
        }
        *hash.offset(p as isize) = fmt_in.undump();
        if !(p != hash_used) {
            break;
        }
    }
    let slice = std::slice::from_raw_parts_mut(
        hash.offset((hash_used + 1) as isize),
        (UNDEFINED_CONTROL_SEQUENCE - 1) - (hash_used as usize),
    );

    fmt_in.undump_slice(slice);
    if hash_high > 0 {
        let slice =
            std::slice::from_raw_parts_mut(hash.offset(EQTB_SIZE as isize + 1), hash_high as usize);
        fmt_in.undump_slice(slice);
    }

    cs_count = fmt_in.undump();

    /* font info */

    let x: i32 = fmt_in.undump();
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
    FONT_INFO = fmt_in.undump_many(fmem_ptr as usize);
    FONT_INFO.resize_with(FONT_MEM_SIZE + 1, Default::default);
    let x: i32 = fmt_in.undump();
    if x < FONT_BASE as i32 {
        bad_fmt();
    }
    if x > (FONT_BASE + MAX_FONT_MAX) as i32 {
        panic!("must increase FONT_MAX");
    }

    FONT_PTR = x as usize;

    FONT_MAPPING = vec![0 as *mut libc::c_void; FONT_MAX + 1];
    for _ in 0..FONT_MAX + 1 {
        FONT_LAYOUT_ENGINE.push(crate::xetex_ext::Font::None);
    }
    FONT_FLAGS = vec![0; FONT_MAX + 1];
    FONT_LETTER_SPACE = vec![Scaled::ZERO; FONT_MAX + 1];

    for k in 0..=FONT_PTR {
        FONT_MAPPING[k as usize] = 0 as *mut libc::c_void;
    }

    FONT_CHECK = fmt_in.undump_many(FONT_PTR + 1);
    FONT_CHECK.resize_with(FONT_MAX + 1, Default::default);
    FONT_SIZE = fmt_in.undump_many(FONT_PTR + 1);
    FONT_SIZE.resize(FONT_MAX + 1, Scaled::ZERO);
    FONT_DSIZE = fmt_in.undump_many(FONT_PTR + 1);
    FONT_DSIZE.resize(FONT_MAX + 1, Scaled::ZERO);
    FONT_PARAMS = fmt_in.undump_many(FONT_PTR + 1);
    FONT_PARAMS.resize(FONT_MAX + 1, 0);
    for i_0 in 0..FONT_PTR + 1 {
        if FONT_PARAMS[i_0] < MIN_HALFWORD || FONT_PARAMS[i_0] > MAX_HALFWORD {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_0,
                FONT_PARAMS[i_0],
                FONT_PARAMS.as_ptr() as u64,
                MIN_HALFWORD,
                MAX_HALFWORD
            );
        }
    }
    HYPHEN_CHAR = fmt_in.undump_many(FONT_PTR + 1);
    HYPHEN_CHAR.resize(FONT_MAX + 1, 0);
    SKEW_CHAR = fmt_in.undump_many(FONT_PTR + 1);
    SKEW_CHAR.resize(FONT_MAX + 1, 0);
    FONT_NAME = fmt_in.undump_many(FONT_PTR + 1);
    FONT_NAME.resize(FONT_MAX + 1, 0);
    for i_1 in 0..FONT_PTR + 1 {
        if FONT_NAME[i_1] > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_1,
                FONT_NAME[i_1],
                FONT_NAME.as_ptr() as u64,
                str_ptr
            );
        }
    }
    FONT_AREA = fmt_in.undump_many(FONT_PTR + 1);
    FONT_AREA.resize(FONT_MAX + 1, 0);
    for i_2 in 0..FONT_PTR + 1 {
        if FONT_AREA[i_2] > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_2,
                FONT_AREA[i_2],
                FONT_AREA.as_ptr() as u64,
                str_ptr
            );
        }
    }
    FONT_BC = fmt_in.undump_many(FONT_PTR + 1);
    FONT_BC.resize(FONT_MAX + 1, 0);
    FONT_EC = fmt_in.undump_many(FONT_PTR + 1);
    FONT_EC.resize(FONT_MAX + 1, 0);
    CHAR_BASE = fmt_in.undump_many(FONT_PTR + 1);
    CHAR_BASE.resize(FONT_MAX + 1, 0);
    WIDTH_BASE = fmt_in.undump_many(FONT_PTR + 1);
    WIDTH_BASE.resize(FONT_MAX + 1, 0);
    HEIGHT_BASE = fmt_in.undump_many(FONT_PTR + 1);
    HEIGHT_BASE.resize(FONT_MAX + 1, 0);
    DEPTH_BASE = fmt_in.undump_many(FONT_PTR + 1);
    DEPTH_BASE.resize(FONT_MAX + 1, 0);
    ITALIC_BASE = fmt_in.undump_many(FONT_PTR + 1);
    ITALIC_BASE.resize(FONT_MAX + 1, 0);
    LIG_KERN_BASE = fmt_in.undump_many(FONT_PTR + 1);
    LIG_KERN_BASE.resize(FONT_MAX + 1, 0);
    KERN_BASE = fmt_in.undump_many(FONT_PTR + 1);
    KERN_BASE.resize(FONT_MAX + 1, 0);
    EXTEN_BASE = fmt_in.undump_many(FONT_PTR + 1);
    EXTEN_BASE.resize(FONT_MAX + 1, 0);
    PARAM_BASE = fmt_in.undump_many(FONT_PTR + 1);
    PARAM_BASE.resize(FONT_MAX + 1, 0);
    FONT_GLUE = fmt_in.undump_many(FONT_PTR + 1);
    FONT_GLUE.resize(FONT_MAX + 1, 0);
    for i_3 in 0..FONT_PTR + 1 {
        if FONT_GLUE[i_3] < MIN_HALFWORD || FONT_GLUE[i_3] > lo_mem_max {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_3,
                FONT_GLUE[i_3],
                FONT_GLUE.as_ptr() as u64,
                MIN_HALFWORD,
                lo_mem_max
            );
        }
    }
    BCHAR_LABEL = fmt_in.undump_many(FONT_PTR + 1);
    BCHAR_LABEL.resize(FONT_MAX + 1, 0);
    for i_4 in 0..FONT_PTR + 1 {
        if BCHAR_LABEL[i_4] < 0 || BCHAR_LABEL[i_4] > fmem_ptr - 1 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_4,
                BCHAR_LABEL[i_4],
                BCHAR_LABEL.as_ptr() as u64,
                0,
                fmem_ptr - 1
            );
        }
    }
    FONT_BCHAR = fmt_in.undump_many(FONT_PTR + 1);
    FONT_BCHAR.resize(FONT_MAX + 1, 0);
    for i_5 in 0..FONT_PTR + 1 {
        if FONT_BCHAR[i_5] < 0 || FONT_BCHAR[i_5] > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_5,
                FONT_BCHAR[i_5],
                FONT_BCHAR.as_ptr() as u64,
                0,
                65536
            );
        }
    }
    FONT_FALSE_BCHAR = fmt_in.undump_many(FONT_PTR + 1);
    FONT_FALSE_BCHAR.resize(FONT_MAX + 1, 0);
    for i_6 in 0..FONT_PTR + 1 {
        if FONT_FALSE_BCHAR[i_6] < 0 || FONT_FALSE_BCHAR[i_6] > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i_6,
                FONT_FALSE_BCHAR[i_6],
                FONT_FALSE_BCHAR.as_ptr() as u64,
                0,
                65536
            );
        }
    }

    /* hyphenations */

    let x: i32 = fmt_in.undump();
    if x < 0 {
        bad_fmt();
    }

    if x > HYPH_SIZE as i32 {
        panic!("must increase HYPH_SIZE");
    }
    HYPH_COUNT = x as usize;

    let x: i32 = fmt_in.undump();
    if x < HYPH_PRIME {
        bad_fmt();
    }
    if x > HYPH_SIZE as i32 {
        panic!("must increase HYPH_SIZE");
    }
    HYPH_NEXT = x as usize;

    let mut j = 0;

    for _k in 1..=HYPH_COUNT {
        let mut j: i32 = fmt_in.undump();
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
        let x: i32 = fmt_in.undump();
        if x < 0 || x > str_ptr {
            bad_fmt();
        } else {
            HYPH_WORD[j as usize] = x;
        }
        let x: i32 = fmt_in.undump();
        if x < MIN_HALFWORD || x > MAX_HALFWORD {
            bad_fmt();
        } else {
            HYPH_LIST[j as usize] = x.opt();
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
    let x: i32 = fmt_in.undump();
    if x < 0 {
        bad_fmt();
    }
    if x > trie_size {
        panic!("must increase trie_size");
    }

    let j = x;
    trie_max = j;

    let x: i32 = fmt_in.undump();
    if x < 0 || x > j {
        bad_fmt();
    } else {
        hyph_start = x;
    }

    if trie_trl.is_empty() {
        trie_trl = vec![0; j as usize + 2];
    }
    fmt_in.undump_slice(&mut trie_trl[..(j + 1) as usize]);
    if trie_tro.is_empty() {
        trie_tro = vec![0; j as usize + 2];
    }
    fmt_in.undump_slice(&mut trie_tro[..(j + 1) as usize]);
    if trie_trc.is_empty() {
        trie_trc = vec![0; j as usize + 2];
    }
    fmt_in.undump_slice(&mut trie_trc[..(j + 1) as usize]);
    max_hyph_char = fmt_in.undump();
    let x: i32 = fmt_in.undump();
    if x < 0 {
        bad_fmt();
    }
    if x > TRIE_OP_SIZE {
        panic!("must increase TRIE_OP_SIZE");
    }

    let mut j = x;
    trie_op_ptr = j;

    fmt_in.undump_slice(&mut hyf_distance[1..(j + 1) as usize]);
    fmt_in.undump_slice(&mut hyf_num[1..(j + 1) as usize]);
    fmt_in.undump_slice(&mut hyf_next[1..(j + 1) as usize]);
    let mut i_7 = 0;
    while i_7 < j as usize {
        if hyf_next[1 + i_7] as i64 > 65535 {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i_7,
                hyf_next[1 + i_7] as u64,
                &mut *hyf_next.as_mut_ptr().offset(1) as *mut trie_opcode as u64,
                65535
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
        let x: i32 = fmt_in.undump();
        if x < 0i32 || x > k - 1i32 {
            bad_fmt();
        } else {
            k = x;
        }
        let x: i32 = fmt_in.undump();
        if x < 1 || x > j {
            bad_fmt();
        }
        trie_used[k as usize] = x as trie_opcode;
        j = j - x;
        op_start[k as usize] = j
    }
    trie_not_ready = false;

    /* trailer */

    let x: i32 = fmt_in.undump();
    if x != FORMAT_FOOTER_MAGIC {
        bad_fmt();
    }
    return true;
}

/* Read and write dump files.  As distributed, these files are
architecture dependent; specifically, BigEndian and LittleEndian
architectures produce different files.  These routines always output
BigEndian files.  This still does not guarantee them to be
architecture-independent, because it is possible to make a format
that dumps a glue ratio, i.e., a floating-point number.  Fortunately,
none of the standard formats do that.  */

trait Dump<T>: Write {
    fn try_dump(&mut self, v: T) -> Result<usize, std::io::Error>;
    fn dump(&mut self, v: T) {
        self.try_dump(v).unwrap_or_else(|_| {
            let item_size = std::mem::size_of::<T>();
            abort!(
                "could not write 1 {}-byte item(s) to {}",
                item_size,
                unsafe { &name_of_file },
            )
        });
    }
}

impl<T, W> Dump<&[T]> for W
where
    Self: Write + Dump<T>,
    T: Copy,
{
    fn try_dump(&mut self, slice: &[T]) -> Result<usize, std::io::Error> {
        for &v in slice {
            self.try_dump(v)?;
        }
        Ok(std::mem::size_of::<T>() * slice.len())
    }
    fn dump(&mut self, slice: &[T]) {
        Dump::<&[T]>::try_dump(self, slice).unwrap_or_else(|_| {
            let nitems = slice.len();
            let item_size = std::mem::size_of::<T>();
            abort!(
                "could not write {} {}-byte item(s) to {}",
                nitems,
                item_size,
                unsafe { &name_of_file },
            )
        });
    }
}

impl<W> Dump<u16> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: u16) -> Result<usize, std::io::Error> {
        self.write(&v.to_be_bytes())
    }
}
impl<W> Dump<i16> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: i16) -> Result<usize, std::io::Error> {
        self.write(&v.to_be_bytes())
    }
}
impl<W> Dump<i32> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: i32) -> Result<usize, std::io::Error> {
        self.write(&v.to_be_bytes())
    }
}
impl<W> Dump<Scaled> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: Scaled) -> Result<usize, std::io::Error> {
        self.write(&v.0.to_be_bytes())
    }
}
impl<W> Dump<b32x2> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: b32x2) -> Result<usize, std::io::Error> {
        let [b0, b1, b2, b3] = v.s1.to_be_bytes();
        let [b4, b5, b6, b7] = v.s0.to_be_bytes();
        self.write(&[b0, b1, b2, b3, b4, b5, b6, b7])
    }
}
impl<W> Dump<b16x4> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: b16x4) -> Result<usize, std::io::Error> {
        let [b0, b1] = v.s3.to_be_bytes();
        let [b2, b3] = v.s2.to_be_bytes();
        let [b4, b5] = v.s1.to_be_bytes();
        let [b6, b7] = v.s0.to_be_bytes();
        self.write(&[b0, b1, b2, b3, b4, b5, b6, b7])
    }
}
impl<W> Dump<memory_word> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: memory_word) -> Result<usize, std::io::Error> {
        self.write(&unsafe { v.gr }.to_be_bytes())
    }
}
impl<W> Dump<EqtbWord> for W
where
    Self: Write,
{
    fn try_dump(&mut self, v: EqtbWord) -> Result<usize, std::io::Error> {
        self.try_dump(v.val)?;
        self.try_dump(v.cmd)?;
        self.try_dump(v.lvl)?;
        Ok(8)
    }
}

trait UnDump<T>: Read
where
    T: Copy,
{
    fn try_undump(&mut self) -> Result<T, std::io::Error>;
    fn undump(&mut self) -> T {
        self.try_undump().unwrap_or_else(|_| {
            let item_size = std::mem::size_of::<T>();
            abort!(
                "could not undump 1 {}-byte item(s) from {}",
                item_size,
                unsafe { &name_of_file },
            );
        })
    }
    fn try_undump_many(&mut self, n: usize) -> Result<Vec<T>, std::io::Error> {
        let mut v = Vec::with_capacity(n);
        for _ in 0..n {
            v.push(self.try_undump()?);
        }
        Ok(v)
    }
    fn undump_many(&mut self, n: usize) -> Vec<T> {
        self.try_undump_many(n).unwrap_or_else(|_| {
            let item_size = std::mem::size_of::<T>();
            abort!(
                "could not undump {} {}-byte item(s) from {}",
                n,
                item_size,
                unsafe { &name_of_file },
            );
        })
    }
    fn undump_slice(&mut self, slice: &mut [T]) {
        let v = self.undump_many(slice.len());
        slice.copy_from_slice(&v);
    }
}

impl<R> UnDump<u16> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<u16, std::io::Error> {
        let mut buf = [0; 2];
        self.read_exact(&mut buf[..])?;
        Ok(u16::from_be_bytes(buf))
    }
}

impl<R> UnDump<i16> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<i16, std::io::Error> {
        let mut buf = [0; 2];
        self.read_exact(&mut buf[..])?;
        Ok(i16::from_be_bytes(buf))
    }
}

impl<R> UnDump<i32> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<i32, std::io::Error> {
        let mut buf = [0; 4];
        self.read_exact(&mut buf[..])?;
        Ok(i32::from_be_bytes(buf))
    }
}

impl<R> UnDump<Scaled> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<Scaled, std::io::Error> {
        <Self as UnDump<i32>>::try_undump(self).map(Scaled)
    }
}

impl<R> UnDump<b32x2> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<b32x2, std::io::Error> {
        let s1 = self.try_undump()?;
        let s0 = self.try_undump()?;
        Ok(b32x2 { s0, s1 })
    }
}

impl<R> UnDump<b16x4> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<b16x4, std::io::Error> {
        let s3 = self.try_undump()?;
        let s2 = self.try_undump()?;
        let s1 = self.try_undump()?;
        let s0 = self.try_undump()?;
        Ok(b16x4 { s0, s1, s2, s3 })
    }
}

impl<R> UnDump<memory_word> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<memory_word, std::io::Error> {
        let mut buf = [0; 8];
        self.read_exact(&mut buf[..])?;
        Ok(memory_word {
            gr: f64::from_be_bytes(buf),
        })
    }
}

impl<R> UnDump<EqtbWord> for R
where
    Self: Read,
{
    fn try_undump(&mut self) -> Result<EqtbWord, std::io::Error> {
        let val = self.try_undump()?;
        let cmd = self.try_undump()?;
        let lvl = self.try_undump()?;
        Ok(EqtbWord { lvl, cmd, val })
    }
}
