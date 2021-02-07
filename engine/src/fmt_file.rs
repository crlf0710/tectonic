use std::ptr;

use crate::cmd::{Cmd, InteractionMode};
use crate::help;
use crate::xetex_consts::IntPar;
use crate::xetex_ini::{b16x4, b32x2, memory_word, EqtbWord, Selector, FONT_PTR};
use crate::xetex_output::Esc;
use crate::xetex_stringpool::{
    init_pool_ptr, init_str_ptr, max_strings, pool_free, pool_ptr, pool_size, str_pool, str_ptr,
    str_start, strings_free,
};
use bridge::{ttstub_output_close, InFile, OutputHandleWrapper, TTHistory, TTInputFormat};

use crate::{t_eprint, t_print, t_print_nl};
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
use crate::xetex_stringpool::{PoolString, EMPTY_STRING, TOO_BIG_CHAR};

use crate::xetex_errors::error;
use crate::xetex_output::print_ln;
use crate::xetex_xetexd::llist_link;
use crate::xetex_xetexd::{TeXInt, TeXOpt};

use crate::xetex_xetex0::get_node;
use crate::xetex_xetex0::FileName;
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

unsafe fn pack_buffered_name(mut _n: i16, mut _a: i32, mut _b: i32) -> String {
    TEX_format_default.clone()
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
        t_eprint!("You can\'t dump inside a group");
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

    selector = if interaction == InteractionMode::Batch {
        Selector::LOG_ONLY
    } else {
        Selector::TERM_AND_LOG
    };

    let s = format!(
        " (preloaded format={} {}.{}.{})",
        PoolString::from(job_name),
        get_int_par(IntPar::year),
        get_int_par(IntPar::month),
        get_int_par(IntPar::day),
    );

    format_ident = PoolString::add_new_from_str(&s);
    let out_name = pack_job_name(".fmt");

    let fmt_out = OutputHandleWrapper::open(&out_name, 0);
    if fmt_out.is_none() {
        abort!("cannot open format output file \"{}\"", out_name);
    }

    let mut fmt_out_owner = fmt_out.unwrap();
    let fmt_out = &mut fmt_out_owner;
    t_print_nl!(
        "Beginning to dump on file {}",
        PoolString::from(make_name_string(&out_name))
    );

    PoolString::flush();

    t_print_nl!("{}", PoolString::from(format_ident));

    /* Header */
    /* TODO: can we move this farther up in this function? */
    fmt_out.dump_one(FORMAT_HEADER_MAGIC);
    fmt_out.dump_one(FORMAT_SERIAL);
    fmt_out.dump_one(hash_high as i32);

    while pseudo_files.opt().is_some() {
        pseudo_close();
    }

    fmt_out.dump_one(MEM_TOP as i32);
    fmt_out.dump_one(EQTB_SIZE as i32);
    fmt_out.dump_one(HASH_PRIME as i32);
    fmt_out.dump_one(HYPH_PRIME);

    /* string pool */

    fmt_out.dump_one(pool_ptr as i32);
    fmt_out.dump_one(str_ptr);
    fmt_out.dump(
        &str_start[..(str_ptr - TOO_BIG_CHAR + 1) as usize]
            .iter()
            .map(|p| *p as i32)
            .collect::<Vec<_>>(),
    );
    fmt_out.dump(&str_pool[..pool_ptr]);

    print_ln();
    t_print!("{} strings of total length {}", str_ptr, pool_ptr as i32);

    /* "memory locations" */

    sort_avail();
    var_used = 0;
    fmt_out.dump_one(lo_mem_max);
    fmt_out.dump_one(rover);

    for k in (ValLevel::Int as usize)..=(ValLevel::InterChar as usize) {
        fmt_out.dump_one(sa_root[k].tex_int());
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
        if q == rover {
            break;
        }
    }

    var_used = var_used + lo_mem_max - p;
    dyn_used = mem_end + 1 - hi_mem_min;
    fmt_out.dump(&MEM[p as usize..(lo_mem_max + 1) as usize]);

    x = x + lo_mem_max + 1 - p;
    fmt_out.dump_one(hi_mem_min as i32);
    fmt_out.dump_one(avail.tex_int());
    fmt_out.dump(&MEM[hi_mem_min as usize..(mem_end + 1) as usize]);

    x = x + mem_end + 1 - hi_mem_min;
    let mut popt = avail;
    while let Some(p) = popt {
        dyn_used -= 1;
        popt = llist_link(p)
    }

    fmt_out.dump_one(var_used as i32);
    fmt_out.dump_one(dyn_used as i32);

    print_ln();
    t_print!(
        "{} memory locations dumped; current usage is {}&{}",
        x,
        var_used,
        dyn_used
    );

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
        fmt_out.dump_one((l as i32) - (k as i32));
        fmt_out.dump(&EQTB[k..l]);
        k = j + 1;
        fmt_out.dump_one((k as i32) - (l as i32));
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
        fmt_out.dump_one((l as i32) - (k as i32));
        fmt_out.dump(&EQTB[k..l]);
        k = j + 1;
        fmt_out.dump_one((k as i32) - (l as i32));
        if k > EQTB_SIZE {
            break;
        }
    }

    if hash_high > 0 {
        fmt_out.dump(&EQTB[EQTB_SIZE + 1..EQTB_SIZE + 1 + hash_high as usize]);
    }

    fmt_out.dump_one(par_loc as i32);
    fmt_out.dump_one(write_loc as i32);

    for &p in prim.iter().take(PRIM_SIZE + 1) {
        fmt_out.dump_one(p);
    }

    for &p in prim_eqtb.iter().take(PRIM_SIZE + 1) {
        fmt_out.dump_one(p);
    }

    /* control sequences */
    fmt_out.dump_one(hash_used as i32);
    cs_count = (hash_high + FROZEN_CONTROL_SEQUENCE - 1 - hash_used) as i32;

    for p in HASH_BASE..=hash_used {
        if yhash[p - hash_offset].s1 != 0 {
            fmt_out.dump_one(p as i32);
            fmt_out.dump_one(yhash[p - hash_offset]);
            cs_count += 1;
        }
    }

    fmt_out.dump(
        &yhash[(hash_used + 1) as usize - hash_offset..UNDEFINED_CONTROL_SEQUENCE - hash_offset],
    );

    if hash_high > 0 {
        fmt_out.dump(
            &yhash[EQTB_SIZE + 1 - hash_offset..EQTB_SIZE + 1 + hash_high as usize - hash_offset],
        );
    }

    fmt_out.dump_one(cs_count);

    print_ln();
    t_print!("{} multiletter control sequences", cs_count);

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

    for k in FONT_BASE..FONT_PTR + 1 {
        t_print_nl!(
            "\\font{}=",
            Esc(&PoolString::from(yhash[FONT_ID_BASE + k - hash_offset].s1).to_string())
        );

        if matches!(&FONT_LAYOUT_ENGINE[k], Some(_)) || !(FONT_MAPPING[k]).is_null() {
            t_print!(
                "{:#}",
                FileName {
                    name: FONT_NAME[k],
                    area: EMPTY_STRING,
                    ext: EMPTY_STRING
                }
            );

            t_eprint!("Can\'t \\dump a format with native fonts or font-mappings");

            help!(
                "You really, really don\'t want to do this.",
                "It won\'t work, and only confuses me.",
                "(Load them at runtime, not as part of the format file.)"
            );
            error();
        } else {
            t_print!(
                "{:#}",
                FileName {
                    name: FONT_NAME[k],
                    area: FONT_AREA[k],
                    ext: EMPTY_STRING
                }
            );
        }

        if FONT_SIZE[k] != FONT_DSIZE[k] {
            t_print!(" at {}pt", FONT_SIZE[k]);
        }
    }

    print_ln();
    t_print!("{} words of font info for ", fmem_ptr - 7);
    if FONT_PTR != FONT_BASE + 1 {
        t_print!("{} preloaded fonts", FONT_PTR - FONT_BASE);
    } else {
        t_print!("1 preloaded font");
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
            fmt_out.dump_one(HYPH_LIST[k].tex_int());
        }
    }

    print_ln();
    if HYPH_COUNT != 1 {
        t_print!("{} hyphenation exceptions", HYPH_COUNT as i32);
    } else {
        t_print!("1 hyphenation exception");
    }
    if trie_not_ready {
        init_trie();
    }

    fmt_out.dump_one(trie_max);
    fmt_out.dump_one(hyph_start);
    fmt_out.dump(&trie_trl[..(trie_max + 1) as usize]);
    fmt_out.dump(&trie_tro[..(trie_max + 1) as usize]);
    fmt_out.dump(&trie_trc[..(trie_max + 1) as usize]);
    fmt_out.dump_one(max_hyph_char);
    fmt_out.dump_one(trie_op_ptr as i32);
    fmt_out.dump(&hyf_distance[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_num[1..trie_op_ptr as usize + 1]);
    fmt_out.dump(&hyf_next[1..trie_op_ptr as usize + 1]);

    t_print_nl!(
        "Hyphenation trie of length {} has {}",
        trie_max,
        trie_op_ptr
    );
    if trie_op_ptr != 1 {
        t_print!(" ops");
    } else {
        t_print!(" op");
    }
    t_print!(" out of {}", TRIE_OP_SIZE as i32);

    for k in (0..=BIGGEST_LANG).rev() {
        if trie_used[k as usize] as i32 > 0 {
            t_print_nl!("  {} for language {}", trie_used[k as usize] as i32, k);
            fmt_out.dump_one(k as i32);
            fmt_out.dump_one(trie_used[k as usize] as i32);
        }
    }

    /* footer */

    fmt_out.dump_one(FORMAT_FOOTER_MAGIC);

    set_int_par(IntPar::tracing_stats, 0); /*:1361*/
    ttstub_output_close(fmt_out_owner);
}

static mut name_of_fmt_file: String = String::new();

pub(crate) unsafe fn load_fmt_file() -> bool {
    let mut x: i32 = 0;

    let j = cur_input.loc;

    /* This is where a first line starting with "&" used to
     * trigger code that would change the format file. */

    let filename = pack_buffered_name((TEX_format_default.as_bytes().len() - 4) as i16, 1, 0);
    name_of_fmt_file = filename.clone();

    let mut fmt_in = InFile::open(&filename, TTInputFormat::FORMAT, 0)
        .unwrap_or_else(|| abort!("cannot open the format file \"{}\"", filename));

    cur_input.loc = j;

    if in_initex_mode {
        FONT_INFO = Vec::new();
        str_pool = Vec::new();
        str_start = Vec::new();
        yhash = Vec::new();
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
            filename,
            FORMAT_SERIAL,
            x
        );
    }

    /* hash table parameters */

    fmt_in.undump_one(&mut x);
    if x < 0 || x > sup_hash_extra {
        bad_fmt();
    } else {
        hash_high = x as usize;
    }
    if hash_extra < hash_high {
        hash_extra = hash_high
    }

    EQTB_TOP = EQTB_SIZE + hash_extra as usize;
    hash_top = if hash_extra == 0 {
        UNDEFINED_CONTROL_SEQUENCE
    } else {
        EQTB_TOP
    };

    yhash = vec![b32x2::default(); 1 + hash_top - hash_offset + 1];

    for x in HASH_BASE..=hash_top {
        yhash[x - hash_offset] = b32x2_le_t { s0: 0, s1: 0 };
    }

    EQTB = Vec::with_capacity(EQTB_TOP + 2);
    EQTB.set_len(EQTB_TOP + 2);
    let ucs = EqtbWord {
        cmd: Cmd::UndefinedCS as _,
        val: None.tex_int() as _,
        lvl: LEVEL_ZERO as _,
    };
    for eqtb in EQTB.iter_mut().take(EQTB_TOP + 1).skip(EQTB_SIZE) {
        *eqtb = ucs;
    }

    max_reg_num = 32767;
    max_reg_help_line = "A register number must be between 0 and 32767.";

    /* "memory locations" */

    fmt_in.undump_one(&mut x);
    if x != MEM_TOP as i32 {
        bad_fmt();
    }

    cur_list.head = CONTRIB_HEAD;
    cur_list.tail = CONTRIB_HEAD;
    page_tail = PAGE_HEAD;

    MEM = Vec::with_capacity(MEM_TOP + 2);
    MEM.set_len(MEM_TOP + 2);

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
    if x as usize > sup_pool_size - pool_free {
        panic!("must increase string_pool_size");
    }
    pool_ptr = x as usize;
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

    max_strings = max_strings.max(str_ptr as usize + strings_free);

    str_start = vec![0; max_strings + 1];
    let mut v = vec![0_i32; (str_ptr - TOO_BIG_CHAR + 1) as usize];
    fmt_in.undump(&mut v);
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
    str_pool = vec![0; pool_size + 1];
    fmt_in.undump(&mut str_pool[..pool_ptr]);
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
    for k in (ValLevel::Int as usize)..=(ValLevel::InterChar as usize) {
        fmt_in.undump_one(&mut x);
        if x < MIN_HALFWORD || x > lo_mem_max {
            bad_fmt();
        } else {
            sa_root[k] = x.opt();
        }
    }

    let mut p = 0;
    let mut q = rover;

    loop {
        fmt_in.undump(&mut MEM[p as usize..(q + 2) as usize]);
        p = q + MEM[q as usize].b32.s0;
        if p > lo_mem_max
            || q >= MEM[(q + 1) as usize].b32.s1 && MEM[(q + 1) as usize].b32.s1 != rover
        {
            bad_fmt();
        }
        q = MEM[(q + 1) as usize].b32.s1;
        if q == rover {
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
        avail = x.opt();
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
        k += x;

        fmt_in.undump_one(&mut x);
        if x < 0 || k + x > (EQTB_SIZE as i32) + 1 {
            bad_fmt();
        }

        let mut j = k;
        while j < k + x {
            EQTB[j as usize] = EQTB[(k - 1) as usize];
            j += 1
        }
        k += x;
        if k > EQTB_SIZE as i32 {
            break;
        }
    }

    if hash_high > 0 {
        fmt_in
            .undump(&mut EQTB[EQTB_SIZE as usize + 1..EQTB_SIZE as usize + 1 + hash_high as usize]);
    }

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > hash_top as i32 {
        bad_fmt();
    } else {
        par_loc = x;
    }

    par_token = CS_TOKEN_FLAG + par_loc;

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > hash_top as i32 {
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

    for p in 0..=PRIM_SIZE {
        fmt_in.undump_one(&mut prim[p]);
    }

    for p in 0..=PRIM_SIZE {
        fmt_in.undump_one(&mut prim_eqtb[p]);
    }

    fmt_in.undump_one(&mut x);
    if x < HASH_BASE as i32 || x > FROZEN_CONTROL_SEQUENCE as i32 {
        bad_fmt();
    } else {
        hash_used = x as usize;
    }

    let mut p = HASH_BASE - 1;

    loop {
        fmt_in.undump_one(&mut x);
        if x < (p as i32) + 1 || x > hash_used as i32 {
            bad_fmt();
        } else {
            p = x as usize;
        }
        fmt_in.undump_one(&mut yhash[p - hash_offset]);
        if p == hash_used {
            break;
        }
    }
    fmt_in.undump(
        &mut yhash[(hash_used + 1) - hash_offset..UNDEFINED_CONTROL_SEQUENCE - hash_offset],
    );
    if hash_high > 0 {
        fmt_in.undump(
            &mut yhash
                [EQTB_SIZE + 1 - hash_offset..EQTB_SIZE + 1 + hash_high as usize - hash_offset],
        );
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

    FONT_MAPPING = vec![ptr::null_mut(); FONT_MAX + 1];
    for _ in 0..FONT_MAX + 1 {
        FONT_LAYOUT_ENGINE.push(None);
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

    for k in 0..FONT_PTR + 1 {
        FONT_MAPPING[k as usize] = ptr::null_mut();
    }

    fmt_in.undump(&mut FONT_CHECK[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_SIZE[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_DSIZE[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_PARAMS[..FONT_PTR + 1]);
    for (i, &param) in FONT_PARAMS.iter().enumerate().take(FONT_PTR + 1) {
        if param < MIN_HALFWORD || param > MAX_HALFWORD {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                param,
                FONT_PARAMS.as_ptr() as u64,
                MIN_HALFWORD,
                MAX_HALFWORD
            );
        }
    }
    fmt_in.undump(&mut HYPHEN_CHAR[..FONT_PTR + 1]);
    fmt_in.undump(&mut SKEW_CHAR[..FONT_PTR + 1]);
    fmt_in.undump(&mut FONT_NAME[..FONT_PTR + 1]);
    for (i, &name) in FONT_NAME.iter().enumerate().take(FONT_PTR + 1) {
        if name > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i,
                name,
                FONT_NAME.as_ptr() as u64,
                str_ptr
            );
        }
    }
    fmt_in.undump(&mut FONT_AREA[..FONT_PTR + 1]);
    for (i, &area) in FONT_AREA.iter().enumerate().take(FONT_PTR + 1) {
        if area > str_ptr {
            panic!(
                "Item {} (={}) of .fmt array at {:x} >{}",
                i,
                area,
                FONT_AREA.as_ptr() as u64,
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
    for (i, &glue) in FONT_GLUE.iter().enumerate().take(FONT_PTR + 1) {
        if glue < MIN_HALFWORD || glue > lo_mem_max {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                glue,
                FONT_GLUE.as_ptr() as u64,
                MIN_HALFWORD,
                lo_mem_max
            );
        }
    }
    fmt_in.undump(&mut BCHAR_LABEL[..FONT_PTR + 1]);
    for (i, &label) in BCHAR_LABEL.iter().enumerate().take(FONT_PTR + 1) {
        if label < 0 || label > fmem_ptr - 1 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                label,
                BCHAR_LABEL.as_ptr() as u64,
                0,
                fmem_ptr - 1
            );
        }
    }
    fmt_in.undump(&mut FONT_BCHAR[..FONT_PTR + 1]);
    for (i, &b_char) in FONT_BCHAR.iter().enumerate().take(FONT_PTR + 1) {
        if b_char < 0 || b_char > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                b_char,
                FONT_BCHAR.as_ptr() as u64,
                0,
                65536
            );
        }
    }
    fmt_in.undump(&mut FONT_FALSE_BCHAR[..FONT_PTR + 1]);
    for (i, &b_char) in FONT_FALSE_BCHAR.iter().enumerate().take(FONT_PTR + 1) {
        if b_char < 0 || b_char > 65536 {
            panic!(
                "item {} (={}) of .fmt array at {:x} <{} or >{}",
                i,
                b_char,
                FONT_FALSE_BCHAR.as_ptr() as u64,
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

    let mut j = 0;

    for _k in 1..=HYPH_COUNT {
        fmt_in.undump_one(&mut j);
        if j < 0 {
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
    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x > trie_size {
        panic!("must increase trie_size");
    }

    let j = x;
    trie_max = j;

    fmt_in.undump_one(&mut x);
    if x < 0 || x > j {
        bad_fmt();
    } else {
        hyph_start = x;
    }

    if trie_trl.is_empty() {
        trie_trl = vec![0; j as usize + 2];
    }
    fmt_in.undump(&mut trie_trl[..(j + 1) as usize]);
    if trie_tro.is_empty() {
        trie_tro = vec![0; j as usize + 2];
    }
    fmt_in.undump(&mut trie_tro[..(j + 1) as usize]);
    if trie_trc.is_empty() {
        trie_trc = vec![0; j as usize + 2];
    }
    fmt_in.undump(&mut trie_trc[..(j + 1) as usize]);
    fmt_in.undump_one(&mut max_hyph_char);
    fmt_in.undump_one(&mut x);
    if x < 0 {
        bad_fmt();
    }
    if x > TRIE_OP_SIZE {
        panic!("must increase TRIE_OP_SIZE");
    }

    let mut j = x;
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
    while j > 0 {
        fmt_in.undump_one(&mut x);
        if x < 0 || x > k - 1 {
            bad_fmt();
        } else {
            k = x;
        }
        fmt_in.undump_one(&mut x);
        if x < 1 || x > j {
            bad_fmt();
        }
        trie_used[k as usize] = x as trie_opcode;
        j -= x;
        op_start[k as usize] = j
    }
    trie_not_ready = false;

    /* trailer */

    fmt_in.undump_one(&mut x);
    if x != FORMAT_FOOTER_MAGIC {
        bad_fmt();
    }
    true
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
                    std::slice::from_raw_parts(p, item_size * num)
                }
                unsafe fn as_u8_slice_mut(&mut self, num: usize) -> &mut [u8] {
                    let p = self as *mut Self as *mut u8;
                    let item_size = std::mem::size_of::<Self>();
                    std::slice::from_raw_parts_mut(p, item_size * num)
                }
            }
            impl ToU8Slice for [$x] {
                unsafe fn to_u8_slice(&self) -> &[u8] {
                    let p = self.as_ptr() as *const u8;
                    let item_size = std::mem::size_of::<$x>();
                    std::slice::from_raw_parts(p, item_size * self.len())
                }
                unsafe fn to_u8_slice_mut(&mut self) -> &mut [u8] {
                    let p = self.as_mut_ptr() as *mut u8;
                    let item_size = std::mem::size_of::<$x>();
                    std::slice::from_raw_parts_mut(p, item_size * self.len())
                }
            }
        )*
    };
}

slice!(i32, memory_word, b32x2, b16x4, u16, i16, EqtbWord, Scaled);

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

impl<W> Dump for W
where
    Self: Write,
{
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

        self.write_all(&v).unwrap_or_else(|_| {
            panic!(
                "could not write {} {}-byte item(s) to {}",
                nitems,
                item_size,
                unsafe { &name_of_fmt_file },
            )
        });
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

impl<R> UnDump for R
where
    Self: Read,
{
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
                    name_of_fmt_file
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
