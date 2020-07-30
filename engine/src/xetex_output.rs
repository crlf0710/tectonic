#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use super::xetex_consts::{
    Cmd, IntPar, NativeWord, ACTIVE_BASE, BIGGEST_USV, CAT_CODE, DIMEN_VAL_LIMIT, EQTB_SIZE,
    HASH_BASE, INTPAR, NULL_CS, SCRIPT_SIZE, SINGLE_BASE, TEXT_SIZE, UNDEFINED_CONTROL_SEQUENCE,
};

use super::xetex_ini::Selector;
use super::xetex_ini::{
    dig, doing_special, error_line, file_offset, hash, line, log_file, max_print_line, pool_ptr,
    pool_size, rust_stdout, selector, str_pool, str_ptr, str_start, tally, term_offset, trick_buf,
    trick_count, write_file, EQTB_TOP, FULL_SOURCE_FILENAME_STACK, IN_OPEN, LINE_STACK, MEM,
};
use bridge::ttstub_output_putc;

pub(crate) type scaled_t = i32;

/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* Extra stuff used in various change files for various reasons.  */
/* Array allocations. Add 1 to size to account for Pascal indexing convention. */
/*11:*/
/*18: */
pub(crate) type UTF16_code = u16;
pub(crate) type pool_pointer = i32;
pub(crate) type str_number = i32;
pub(crate) type packed_UTF16_code = u16;
/* xetex-output */
/* tectonic/output.c -- functions related to outputting messages
 * Copyright 2016 the Tectonic Project
 * Licensed under the MIT License.
*/
pub(crate) unsafe fn print_ln() {
    match selector {
        Selector::TERM_AND_LOG => {
            ttstub_output_putc(rust_stdout.as_mut().unwrap(), '\n' as i32);
            ttstub_output_putc(log_file.as_mut().unwrap(), '\n' as i32);
            term_offset = 0i32;
            file_offset = 0i32
        }
        Selector::LOG_ONLY => {
            ttstub_output_putc(log_file.as_mut().unwrap(), '\n' as i32);
            file_offset = 0i32
        }
        Selector::TERM_ONLY => {
            ttstub_output_putc(rust_stdout.as_mut().unwrap(), '\n' as i32);
            term_offset = 0i32
        }
        Selector::NO_PRINT | Selector::PSEUDO | Selector::NEW_STRING => {}
        _ => {
            ttstub_output_putc(
                write_file[u8::from(selector) as usize].as_mut().unwrap(),
                '\n' as i32,
            );
        }
    };
}
pub(crate) unsafe fn print_raw_char(mut s: UTF16_code, mut incr_offset: bool) {
    match selector {
        Selector::TERM_AND_LOG => {
            let stdout = rust_stdout.as_mut().unwrap();
            let lg = log_file.as_mut().unwrap();
            ttstub_output_putc(stdout, s as i32);
            ttstub_output_putc(lg, s as i32);
            if incr_offset {
                term_offset += 1;
                file_offset += 1
            }
            if term_offset == max_print_line {
                ttstub_output_putc(stdout, '\n' as i32);
                term_offset = 0i32
            }
            if file_offset == max_print_line {
                ttstub_output_putc(lg, '\n' as i32);
                file_offset = 0i32
            }
        }
        Selector::LOG_ONLY => {
            ttstub_output_putc(log_file.as_mut().unwrap(), s as i32);
            if incr_offset {
                file_offset += 1
            }
            if file_offset == max_print_line {
                print_ln();
            }
        }
        Selector::TERM_ONLY => {
            ttstub_output_putc(rust_stdout.as_mut().unwrap(), s as i32);
            if incr_offset {
                term_offset += 1
            }
            if term_offset == max_print_line {
                print_ln();
            }
        }
        Selector::NO_PRINT => {}
        Selector::PSEUDO => {
            if tally < trick_count {
                trick_buf[(tally % error_line) as usize] = s
            }
        }
        Selector::NEW_STRING => {
            if pool_ptr < pool_size {
                str_pool[pool_ptr as usize] = s;
                pool_ptr += 1
            }
        }
        _ => {
            ttstub_output_putc(
                write_file[u8::from(selector) as usize].as_mut().unwrap(),
                s as i32,
            );
        }
    }
    tally += 1;
}
pub(crate) unsafe fn print_char(s: i32) {
    let mut l: i16 = 0;
    if (u8::from(selector) > u8::from(Selector::PSEUDO)) && !doing_special {
        if s >= 0x10000 {
            print_raw_char((0xd800 + (s - 0x10000) / 1024) as UTF16_code, true);
            print_raw_char((0xdc00 + (s - 0x10000) % 1024) as UTF16_code, true);
        } else {
            print_raw_char(s as UTF16_code, true);
        }
        return;
    }
    if s == *INTPAR(IntPar::new_line_char) {
        /*:252 */
        if u8::from(selector) < u8::from(Selector::PSEUDO) {
            print_ln();
            return;
        }
    }
    if s < 32 && !doing_special {
        print_raw_char('^' as i32 as UTF16_code, true);
        print_raw_char('^' as i32 as UTF16_code, true);
        print_raw_char((s + 64) as UTF16_code, true);
    } else if s < 127 {
        print_raw_char(s as UTF16_code, true);
    } else if s == 127 {
        if !doing_special {
            print_raw_char('^' as i32 as UTF16_code, true);
            print_raw_char('^' as i32 as UTF16_code, true);
            print_raw_char('?' as i32 as UTF16_code, true);
        } else {
            print_raw_char(s as UTF16_code, true);
        }
    } else if s < 160 && !doing_special {
        print_raw_char('^' as i32 as UTF16_code, true);
        print_raw_char('^' as i32 as UTF16_code, true);
        l = (s % 256 / 16) as i16;
        if l < 10 {
            print_raw_char(('0' as i32 + l as i32) as UTF16_code, true);
        } else {
            print_raw_char(('a' as i32 + l as i32 - 10) as UTF16_code, true);
        }
        l = (s % 16) as i16;
        if l < 10 {
            print_raw_char(('0' as i32 + l as i32) as UTF16_code, true);
        } else {
            print_raw_char(('a' as i32 + l as i32 - 10) as UTF16_code, true);
        }
    } else if s < 2048 {
        print_raw_char((192 + s / 64) as UTF16_code, false);
        print_raw_char((128 + s % 64) as UTF16_code, true);
    } else if s < 0x10000 {
        print_raw_char((224 + s / 4096) as UTF16_code, false);
        print_raw_char((128 + s % 4096 / 64) as UTF16_code, false);
        print_raw_char((128 + s % 64) as UTF16_code, true);
    } else {
        print_raw_char((240 + s / 0x40000) as UTF16_code, false);
        print_raw_char((128 + s % 0x40000 / 4096) as UTF16_code, false);
        print_raw_char((128 + s % 4096 / 64) as UTF16_code, false);
        print_raw_char((128 + s % 64) as UTF16_code, true);
    };
}
pub(crate) unsafe fn print(mut s: i32) {
    let mut nl: i32 = 0;
    if s >= str_ptr {
        return print_cstr(b"???");
    } else {
        if s < 0xffffi32 {
            if s < 0i32 {
                return print_cstr(b"???");
            } else {
                if u8::from(selector) > u8::from(Selector::PSEUDO) {
                    print_char(s);
                    return;
                }
                if s == *INTPAR(IntPar::new_line_char) {
                    /*:252 */
                    if u8::from(selector) < u8::from(Selector::PSEUDO) {
                        print_ln();
                        return;
                    }
                }
                nl = *INTPAR(IntPar::new_line_char);
                *INTPAR(IntPar::new_line_char) = -1i32;
                print_char(s);
                *INTPAR(IntPar::new_line_char) = nl;
                return;
            }
        }
    }
    let mut pool_idx: i32 = s - 0x10000i32;

    // TODO: fix this bug workaround
    let mut i = if pool_idx as usize > crate::xetex_ini::max_strings {
        0
    } else {
        str_start[pool_idx as usize]
    };

    while i < str_start[(pool_idx + 1) as usize] {
        if str_pool[i as usize] as i32 >= 0xd800i32
            && (str_pool[i as usize] as i32) < 0xdc00i32
            && i + 1 < str_start[(pool_idx + 1) as usize]
            && str_pool[(i + 1i32) as usize] as i32 >= 0xdc00i32
            && (str_pool[(i + 1i32) as usize] as i32) < 0xe000i32
        {
            print_char(
                0x10000i32
                    + (str_pool[i as usize] as i32 - 0xd800i32) * 1024i32
                    + str_pool[(i + 1i32) as usize] as i32
                    - 0xdc00i32,
            );
            i += 1
        } else {
            print_char(str_pool[i as usize] as i32);
        }
        i += 1
    }
}
pub(crate) unsafe fn print_cstr(slice: &[u8]) {
    for &s in slice {
        print_char(s as i32);
    }
}
pub(crate) unsafe fn print_nl(mut s: str_number) {
    if term_offset > 0i32 && u8::from(selector) & 1 != 0
        || file_offset > 0i32 && (u8::from(selector) >= u8::from(Selector::LOG_ONLY))
    {
        print_ln();
    }
    print(s);
}
pub(crate) unsafe fn print_nl_cstr(slice: &[u8]) {
    if term_offset > 0i32 && u8::from(selector) & 1 != 0
        || file_offset > 0i32 && (u8::from(selector) >= u8::from(Selector::LOG_ONLY))
    {
        print_ln();
    }
    print_cstr(slice);
}
pub(crate) unsafe fn print_esc(mut s: str_number) {
    let mut c = *INTPAR(IntPar::escape_char);
    if c >= 0i32 && c <= BIGGEST_USV as i32 {
        print_char(c);
    }
    print(s);
}
pub(crate) unsafe fn print_esc_cstr(s: &[u8]) {
    let mut c = *INTPAR(IntPar::escape_char);
    if c >= 0i32 && c <= BIGGEST_USV as i32 {
        print_char(c);
    }
    print_cstr(s);
}
unsafe fn print_the_digs(mut k: u8) {
    while k as i32 > 0i32 {
        k = k.wrapping_sub(1);
        if (dig[k as usize] as i32) < 10i32 {
            print_char('0' as i32 + dig[k as usize] as i32);
        } else {
            print_char(55i32 + dig[k as usize] as i32);
        }
    }
}
pub(crate) unsafe fn print_int(mut n: i32) {
    let mut k: u8 = 0_u8;
    let mut m: i32 = 0;
    if n < 0i32 {
        print_char('-' as i32);
        if n as i64 > -100000000 {
            n = -n
        } else {
            m = -1i32 - n;
            n = m / 10i32;
            m = m % 10i32 + 1i32;
            k = 1_u8;
            if m < 10i32 {
                dig[0] = m as u8
            } else {
                dig[0] = 0_u8;
                n += 1
            }
        }
    }
    loop {
        dig[k as usize] = (n % 10i32) as u8;
        n = n / 10i32;
        k = k.wrapping_add(1);
        if n == 0i32 {
            break;
        }
    }
    print_the_digs(k);
}
pub(crate) unsafe fn print_cs(mut p: i32) {
    if p < HASH_BASE as i32 {
        if p >= SINGLE_BASE as i32 {
            if p == NULL_CS as i32 {
                print_esc_cstr(b"csname");
                print_esc_cstr(b"endcsname");
                print_char(' ' as i32);
            } else {
                print_esc(p - SINGLE_BASE as i32);
                if *CAT_CODE(p as usize - SINGLE_BASE) == Cmd::Letter as _ {
                    print_char(' ' as i32);
                }
            }
        } else if p < ACTIVE_BASE as i32 {
            print_esc_cstr(b"IMPOSSIBLE.");
        } else {
            print_char(p - 1i32);
        }
    } else if p >= UNDEFINED_CONTROL_SEQUENCE as i32 && p <= EQTB_SIZE as i32 || p > EQTB_TOP as i32
    {
        print_esc_cstr(b"IMPOSSIBLE.");
    } else if (*hash.offset(p as isize)).s1 >= str_ptr {
        print_esc_cstr(b"NONEXISTENT.");
    } else {
        print_esc((*hash.offset(p as isize)).s1);
        print_char(' ' as i32);
    };
}
pub(crate) unsafe fn sprint_cs(mut p: i32) {
    if p < HASH_BASE as i32 {
        if p < SINGLE_BASE as i32 {
            print_char(p - 1i32);
        } else if p < NULL_CS as i32 {
            print_esc(p - SINGLE_BASE as i32);
        } else {
            print_esc_cstr(b"csname");
            print_esc_cstr(b"endcsname");
        }
    } else {
        print_esc((*hash.offset(p as isize)).s1);
    };
}
pub(crate) unsafe fn print_file_name(mut n: i32, mut a: i32, mut e: i32) {
    let mut must_quote: bool = false;
    let mut quote_char: i32 = 0i32;
    let mut j: pool_pointer = 0;
    if a != 0i32 {
        j = str_start[(a - 0x10000i32) as usize];
        while (!must_quote || quote_char == 0i32) && j < str_start[(a + 1i32 - 0x10000i32) as usize]
        {
            if str_pool[j as usize] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j as usize] as i32 == '\"' as i32
                || str_pool[j as usize] as i32 == '\'' as i32
            {
                must_quote = true;
                quote_char = 73i32 - str_pool[j as usize] as i32
            }
            j += 1
        }
    }
    if n != 0i32 {
        j = str_start[(n - 0x10000i32) as usize];
        while (!must_quote || quote_char == 0i32) && j < str_start[(n + 1i32 - 0x10000i32) as usize]
        {
            if str_pool[j as usize] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j as usize] as i32 == '\"' as i32
                || str_pool[j as usize] as i32 == '\'' as i32
            {
                must_quote = true;
                quote_char = 73i32 - str_pool[j as usize] as i32
            }
            j += 1
        }
    }
    if e != 0i32 {
        j = str_start[(e - 0x10000i32) as usize];
        while (!must_quote || quote_char == 0i32) && j < str_start[(e + 1i32 - 0x10000i32) as usize]
        {
            if str_pool[j as usize] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j as usize] as i32 == '\"' as i32
                || str_pool[j as usize] as i32 == '\'' as i32
            {
                must_quote = true;
                quote_char = 73i32 - str_pool[j as usize] as i32
            }
            j += 1
        }
    }
    if must_quote {
        if quote_char == 0i32 {
            quote_char = '\"' as i32
        }
        print_char(quote_char);
    }
    if a != 0i32 {
        for j in str_start[(a - 0x10000i32) as usize]..str_start[(a + 1i32 - 0x10000i32) as usize] {
            if str_pool[j as usize] as i32 == quote_char {
                print(quote_char);
                quote_char = 73i32 - quote_char;
                print(quote_char);
            }
            print(str_pool[j as usize] as i32);
        }
    }
    if n != 0i32 {
        for j in str_start[(n - 0x10000i32) as usize]..str_start[(n + 1i32 - 0x10000i32) as usize] {
            if str_pool[j as usize] as i32 == quote_char {
                print(quote_char);
                quote_char = 73i32 - quote_char;
                print(quote_char);
            }
            print(str_pool[j as usize] as i32);
        }
    }
    if e != 0i32 {
        for j in str_start[(e - 0x10000i32) as usize]..str_start[(e + 1i32 - 0x10000i32) as usize] {
            if str_pool[j as usize] as i32 == quote_char {
                print(quote_char);
                quote_char = 73i32 - quote_char;
                print(quote_char);
            }
            print(str_pool[j as usize] as i32);
        }
    }
    if quote_char != 0i32 {
        print_char(quote_char);
    };
}
pub(crate) unsafe fn print_size(mut s: i32) {
    if s == TEXT_SIZE as i32 {
        print_esc_cstr(b"textfont");
    } else if s == SCRIPT_SIZE as i32 {
        print_esc_cstr(b"scriptfont");
    } else {
        print_esc_cstr(b"scriptscriptfont");
    };
}
pub(crate) unsafe fn print_write_whatsit(s: &[u8], p: usize) {
    print_esc_cstr(s);
    if MEM[p + 1].b32.s0 < 16 {
        print_int(MEM[p + 1].b32.s0);
    } else if MEM[p + 1].b32.s0 == 16 {
        print_char('*' as i32);
    } else {
        print_char('-' as i32);
    };
}
pub(crate) unsafe fn print_native_word(p: &NativeWord) {
    let mut c: i32 = 0;
    let text = p.text();
    let mut i = 0;
    while i < text.len() {
        c = text[i] as i32;
        if c >= 0xd800 && c < 0xdc00 {
            if i < text.len() - 1 {
                let cc = text[i + 1] as i32;
                if cc >= 0xdc00 && cc < 0xe000 {
                    c = 0x10000 + (c - 0xd800) * 1024 + (cc - 0xdc00);
                    print_char(c);
                    i += 1
                } else {
                    print('.' as i32);
                }
            } else {
                print('.' as i32);
            }
        } else {
            print_char(c);
        }
        i += 1;
    }
}
pub(crate) unsafe fn print_sa_num(mut q: usize) {
    let mut n: i32 = 0;
    if MEM[q].b16.s1 < DIMEN_VAL_LIMIT {
        n = MEM[q + 1].b32.s1
    } else {
        n = MEM[q].b16.s1 as i32 % 64;
        q = MEM[q].b32.s1 as usize;
        n = n + 64 * MEM[q].b16.s1 as i32;
        q = MEM[q].b32.s1 as usize;
        n = n + 64 * 64 * (MEM[q].b16.s1 as i32 + 64 * MEM[MEM[q].b32.s1 as usize].b16.s1 as i32)
    }
    print_int(n);
}
pub(crate) unsafe fn print_file_line() {
    let mut level = IN_OPEN;
    while level > 0 && FULL_SOURCE_FILENAME_STACK[level] == 0 {
        level -= 1
    }
    if level == 0 {
        print_nl_cstr(b"! ");
    } else {
        print_nl_cstr(b"");
        print(FULL_SOURCE_FILENAME_STACK[level]);
        print(':' as i32);
        if level == IN_OPEN {
            print_int(line);
        } else {
            print_int(LINE_STACK[level + 1]);
        }
        print_cstr(b": ");
    };
}
/*:251 */
/*:251 */
/*:1660*/
pub(crate) unsafe fn print_two(mut n: i32) {
    n = n.abs() % 100i32;
    print_char('0' as i32 + n / 10i32);
    print_char('0' as i32 + n % 10i32);
}
pub(crate) unsafe fn print_hex(mut n: i32) {
    let mut k: u8 = 0_u8;
    print_char('\"' as i32);
    loop {
        dig[k as usize] = (n % 16i32) as u8;
        n = n / 16i32;
        k = k.wrapping_add(1);
        if !(n != 0i32) {
            break;
        }
    }
    print_the_digs(k);
}
pub(crate) unsafe fn print_roman_int(mut n: i32) {
    let mut u: i32 = 0;
    let mut v: i32 = 0;
    let mut roman_data: *const i8 = b"m2d5c2l5x2v5i\x00" as *const u8 as *const i8;
    let mut j: u8 = 0_u8;
    let mut k: u8 = 0_u8;
    v = 1000i32;
    loop {
        while n >= v {
            print_char(*roman_data.offset(j as isize) as i32);
            n = n - v
        }
        if n <= 0i32 {
            return;
        }
        k = (j as i32 + 2i32) as u8;
        u = v / (*roman_data.offset((k as i32 - 1i32) as isize) as i32 - '0' as i32);
        if *roman_data.offset((k as i32 - 1i32) as isize) as i32 == '2' as i32 {
            k = (k as i32 + 2i32) as u8;
            u = u / (*roman_data.offset((k as i32 - 1i32) as isize) as i32 - '0' as i32)
        }
        if n + u >= v {
            print_char(*roman_data.offset(k as isize) as i32);
            n = n + u
        } else {
            j = (j as i32 + 2i32) as u8;
            v = v / (*roman_data.offset((j as i32 - 1i32) as isize) as i32 - '0' as i32)
        }
    }
}
pub(crate) unsafe fn print_current_string() {
    let mut j: pool_pointer = str_start[(str_ptr - 0x10000) as usize];
    while j < pool_ptr {
        print_char(str_pool[j as usize] as i32);
        j += 1
    }
}
pub(crate) unsafe fn print_scaled(mut s: scaled_t) {
    let mut delta: scaled_t = 0;
    if s < 0i32 {
        print_char('-' as i32);
        s = s.wrapping_neg(); // TODO: check
    }
    print_int(s / 0x10000i32);
    print_char('.' as i32);
    s = 10i32 * (s % 0x10000i32) + 5i32;
    delta = 10i32;
    loop {
        if delta > 0x10000i32 {
            s = s + 0x8000i32 - 50000i32
        }
        print_char('0' as i32 + s / 0x10000i32);
        s = 10i32 * (s % 0x10000i32);
        delta = delta * 10i32;
        if !(s > delta) {
            break;
        }
    }
}
